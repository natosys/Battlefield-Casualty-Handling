#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_censored_interval_estimation.R                        ##
## Regression check — an interval still open when the window closes is      ##
## carried as the lower bound it is, not dropped                            ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_censored_interval_estimation.R
#   Rscript tests/checks/check_censored_interval_estimation.R --days 30 --seed 42
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. compute_treatment_interval_kpis() differenced a start
# attribute against an end attribute and kept the casualties carrying both, so
# a casualty still at the echelon when the run ended was dropped.
# That is right-censoring, and the dropped are not a random subset: they are
# the ones still present, which is both the late arrivals and the long stayers.
#
# At seed 42 over 30 days the dropped share was 21.6% of R2B stays and 18.0%
# of R2E stays, and the R2B stays that were dropped had already run a mean of
# 15.9 days against a completed-case mean of 0.71 days. Both dwell means are
# also screened Morris responses, so a parameter that lengthens dwell pushed
# casualties out of the very response meant to measure its effect.
#
# What it asserts:
#
#   1. The estimator degenerates: with nothing censored, the restricted mean
#      equals the arithmetic mean exactly. This is what makes it safe to apply
#      to an interval that rarely censors, such as the R2B to R2E transit, which
#      is held to that identity whenever no move is open at the close.
#   2. It recovers a known answer. Real stays from the run are re-censored at
#      an earlier calendar window, where their true durations are known, and
#      the estimate is required to come nearer the truth than the completed-
#      case mean it replaces. This is the assertion that would fail if the
#      product-limit arithmetic were wrong, rather than merely different.
#   3. Every casualty who entered an interval is represented in its `n`. None
#      is dropped for want of an end attribute.
#   4. The censored count and share are reported, and are non-zero for the two
#      dwells at the shipped configuration, so the check is not vacuous.
#   5. A quantile the curve does not reach is reported as NA rather than as
#      the same quantile of the casualties who finished, who are a different
#      and shorter-staying population, and one it does reach is located.
#   6. One definition of the observation window serves every reconstruction
#      that needs one.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
  library(ggplot2)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")

args <- commandArgs(trailingOnly = TRUE)

#' Read a flag's value from the command line
#'
#' @param flag Flag name, including its leading dashes.
#' @param default Value returned when the flag is absent or has no argument.
#' @return The argument following `flag`, or `default`.
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

#' Run length in days for the live run
#'
#' @details Long enough that both dwell intervals carry censored stays as well
#'   as closed ones, which is what makes assertions 2 and 4 non-vacuous; the
#'   check confirms rather than assumes it.
CHECK_DAYS <- as.integer(arg_value("--days", 30L))

#' Seed for the live run
CHECK_SEED <- as.integer(arg_value("--seed", 42L))

#' Fractions of the window the recovery test re-censors the observed stays at
#'
#' @details Each gives a cohort whose true durations are known from the full
#'   run, censored at a window shorter than the one that produced them.
RECENSOR_FRACTIONS <- c(0.4, 0.6, 0.8)

#' Censored share above which the recovery test demands a correction
#'
#' @details The estimator's claim is that it removes the bias censoring
#'   introduces, so it is held to that only where there is bias to remove.
#'   Below this share both estimators are effectively unbiased and which lands
#'   nearer the truth on one cohort is sampling noise; the check asserts they
#'   agree there instead, which is the same claim read the other way.
MATERIAL_CENSORING <- 0.05

#' Tolerance on the two estimators agreeing under negligible censoring
#'
#' @details As a share of the true restricted mean.
AGREEMENT_TOLERANCE <- 0.05

state <- new.env(parent = emptyenv())
state$failures <- character(0)

#' Record a failure, deferring the non-zero exit to the end of the run
#'
#' @param ... sprintf() format string and its arguments.
#' @return Invisibly, the accumulated failure vector.
fail <- function(...) state$failures <- c(state$failures, sprintf(...))

#' Print one PASS or FAIL line, recording a failure
#'
#' @param ok TRUE when the assertion held.
#' @param fmt sprintf() format string describing the assertion.
#' @param ... Arguments to `fmt`.
#' @return Invisible NULL.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

cat(sprintf("Censored interval estimation check: %d-day run, seed %d\n\n",
            CHECK_DAYS, CHECK_SEED))

# ── 1. The estimator degenerates to the arithmetic mean ─────────────────────

cat("-- degeneracy with nothing censored --\n")

set.seed(CHECK_SEED)
uncensored <- rexp(500, rate = 1 / 100)
report(isTRUE(all.equal(km_restricted_mean(uncensored, rep(1L, 500), max(uncensored)),
                        mean(uncensored))),
       "with nothing censored the restricted mean equals the arithmetic mean exactly")

# ── Live run ────────────────────────────────────────────────────────────────

# Assigned at top level, so these land in the global environment the model
# reads them from without needing <<-.
env_data <- load_scenario("env_data.json", "default")
day_min  <- DAY_MIN
counts   <- sapply(env_data$elms, length)

set.seed(CHECK_SEED)
invisible(capture.output(wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)))
mon <- list(
  arrivals   = get_mon_arrivals(list(wrapped), ongoing = TRUE),
  attributes = get_mon_attributes(list(wrapped)),
  resources  = get_mon_resources(list(wrapped))
)
scratch <- tempfile("censoring-check-")
dir.create(scratch, recursive = TRUE)
frames   <- prepare_run_frames(mon, 0, scratch)
combined <- frames$combined
window   <- observation_window_min(combined)

#' The two dwell intervals, by the attribute pair that bounds each
INTERVALS <- list(
  R2B = c("r2b_treatment_start_time", "r2b_departure_time"),
  R2E = c("r2e_arrival_time",         "r2e_departure_time")
)

#' Arithmetic mean over the casualties whose interval closed
#'
#' @param start_min Interval start, in minutes.
#' @param end_min Interval end, in minutes, NA where it had not closed.
#' @param tau Restriction horizon, in minutes.
#' @return The mean of the closed durations, each capped at `tau`.
#' @details The estimator this check exists to replace, retained so each
#'   assertion can state what the correction is worth.
completed_case_mean <- function(start_min, end_min, tau) {
  d <- as.numeric(end_min) - as.numeric(start_min)
  d <- d[is.finite(d) & d >= 0]
  if (length(d) == 0) NA_real_ else mean(pmin(d, tau))
}

# ── 2. It recovers a known answer on re-censored real stays ─────────────────

cat("\n-- recovery of a known restricted mean under injected censoring --\n")

for (nm in names(INTERVALS)) {
  cols   <- INTERVALS[[nm]]
  starts <- as.numeric(combined[[cols[1]]])
  ends   <- as.numeric(combined[[cols[2]]])
  closed <- !is.na(starts) & !is.na(ends) & ends >= starts
  for (frac in RECENSOR_FRACTIONS) {
    cut_at <- window * frac
    tau    <- INTERVAL_RESTRICTION_MIN
    cohort <- closed & starts < cut_at
    st     <- starts[cohort]
    en     <- ends[cohort]
    truth  <- mean(pmin(en - st, tau))
    # Re-censor at the earlier window: a stay closing after it is observed
    # only up to it, exactly as the run's own window censors the real ones.
    seen   <- ifelse(en <= cut_at, en, NA_real_)
    est    <- censored_interval_stats(st, seen, cut_at, tau)$mean_min
    naive  <- completed_case_mean(st, seen, tau)
    n_cens <- sum(is.na(seen))
    if (n_cens == 0) next
    share <- n_cens / length(st)
    # Material where the completed-case mean is measurably biased, which a
    # few censored stays can cause on their own when the stays are long (a
    # held R2B stay runs up to the evacuation policy), so the share of stays
    # censored is not what decides it.
    biased <- abs(naive - truth) > (AGREEMENT_TOLERANCE + 1 / length(st)) * truth
    if (share >= MATERIAL_CENSORING || biased) {
      report(abs(est - truth) <= abs(naive - truth),
             paste("%s re-censored at %.0f%% of the window (%d stays, %.1f%% censored):",
                   "restricted mean %.0f is nearer the true %.0f than the completed-case",
                   "%.0f"),
             nm, 100 * frac, length(st), 100 * share, est, truth, naive)
    } else {
      # One censored stay in a cohort of n can move a mean by about 1/n of its
      # scale however immaterial its share, so the bound carries that weight
      # alongside the tolerance rather than assuming a small share cannot move
      # a small cohort.
      report(abs(est - naive) <=
               (AGREEMENT_TOLERANCE + 1 / length(st)) * truth,
             paste("%s re-censored at %.0f%% of the window (%d stays, %.1f%% censored):",
                   "with no bias to remove the restricted mean %.0f and the completed-case",
                   "%.0f agree to within %.0f%% of the true %.0f"),
             nm, 100 * frac, length(st), 100 * share, est, naive,
             100 * AGREEMENT_TOLERANCE, truth)
    }
  }
}

# ── 3 and 4. Nobody is dropped, and the censoring is disclosed ──────────────

cat("\n-- cohort coverage and disclosure at the shipped configuration --\n")

kpis <- compute_treatment_interval_kpis(combined, window)
kpi_of <- list(R2B = kpis$r2b_dwell_time, R2E = kpis$r2e_dwell_time)

for (nm in names(INTERVALS)) {
  cols    <- INTERVALS[[nm]]
  entered <- sum(!is.na(as.numeric(combined[[cols[1]]])))
  k       <- kpi_of[[nm]]
  report(k$n == entered,
         "%s dwell counts all %d casualties who entered it, not the %d who also left",
         nm, entered, entered - k$n_censored)
  report(k$n_censored > 0 &&
           isTRUE(all.equal(k$censored_share, k$n_censored / k$n)),
         "%s dwell reports %d censored stays, %.1f%% of its cohort",
         nm, k$n_censored, 100 * k$censored_share)
  # Capped at the restriction horizon, not at the window. The censoring-aware
  # mean is restricted to INTERVAL_RESTRICTION_MIN, so a completed-case mean
  # capped at the 30-day window is a different quantity: comparing them puts
  # the restriction's effect on long stays into what is meant to measure the
  # censoring correction alone, and the ordering can then fail on a campaign
  # whose stays run past the horizon even though nothing is wrong.
  naive <- completed_case_mean(combined[[cols[1]]], combined[[cols[2]]],
                               INTERVAL_RESTRICTION_MIN)
  report(k$mean_min >= naive,
         "%s dwell mean %.0f min is at least the completed-case %.0f it replaces",
         nm, k$mean_min, naive)
}

# Whether a casualty is in transit when the window closes depends on the
# realisation, so the real interval is held to whichever case it presents: with
# nothing censored, a share of zero and the arithmetic mean; otherwise a share
# that counts its censored moves and a mean no shorter than the completed-case one.
transit <- kpis$r2b_r2e_transit_time
transit_naive <- completed_case_mean(combined$r2b_departure_time, combined$r2e_arrival_time,
                                     INTERVAL_RESTRICTION_MIN)
if (isTRUE(transit$n_censored == 0)) {
  report(!is.na(transit$censored_share) && transit$censored_share == 0,
         "the transit interval reports a censored share of zero rather than reporting none")
  report(isTRUE(all.equal(transit$mean_min, transit_naive)),
         paste("the uncensored transit mean is unchanged at %.5f min, the estimator",
               "degenerating on real data"),
         transit$mean_min)
} else {
  report(isTRUE(all.equal(transit$censored_share, transit$n_censored / transit$n)),
         "the transit interval reports its %d censored moves as a share of its cohort",
         transit$n_censored)
  report(transit$mean_min >= transit_naive,
         "the transit mean %.2f min is at least the completed-case %.2f it replaces",
         transit$mean_min, transit_naive)
}

#' Largest share of a dwell mean the flat tail beyond the last completion may carry
#'
#' @details The restriction horizon is fixed at seven days, so a campaign whose
#'   longest observed stay falls short of it leaves the survival curve flat over
#'   the remainder and that flat tail carries part of the mean. This is a
#'   property of the horizon meeting the campaign rather than an error, and
#'   requiring it to be exactly zero asserts that no campaign ever ends its last
#'   stay early. The share is bounded and reported instead, so a tail that grows
#'   into a material part of the mean still fails.
TAIL_SHARE_BOUND <- 0.10

cat("\n-- the restriction horizon rests on observed completions --\n")

for (nm in names(INTERVALS)) {
  k <- kpi_of[[nm]]
  report(!is.na(k$tail_share) && k$tail_share <= TAIL_SHARE_BOUND,
         paste("at most %.0f%% of the %s dwell mean lies beyond the last observed",
               "completion, so the flat tail carries little of it (tail share %.1f%%)"),
         100 * TAIL_SHARE_BOUND, nm, 100 * k$tail_share)
}

# ── 5. An unreached quantile is NA, not the completed-case quantile ─────────

cat("\n-- a quantile the run cannot locate --\n")

r2b_starts <- as.numeric(combined[[INTERVALS$R2B[1]]])
r2b_ends   <- as.numeric(combined[[INTERVALS$R2B[2]]])
at_risk    <- !is.na(r2b_starts)
r2b_closed <- !is.na(r2b_ends[at_risk])
r2b_time   <- ifelse(r2b_closed,
                     r2b_ends[at_risk] - r2b_starts[at_risk],
                     window - r2b_starts[at_risk])
lowest <- min(km_survival_curve(r2b_time, as.integer(r2b_closed))$s)
report(is.na(kpis$r2b_dwell_time$p90_min) == (lowest > 0.10),
       "the R2B dwell p90 is NA exactly when its curve stays above 0.10 (it falls to %.3f)",
       lowest)

# Whether the run's own R2B curve reaches its p90 depends on the realisation, so
# the unreached case is also asserted on a cohort constructed to leave it
# unreached: 80 stays closing within 80 minutes and 20 still open at the window,
# each having run far longer than any that closed.
built_start <- rep(0, 100)
built_end   <- c(seq_len(80), rep(NA, 20))
built       <- censored_interval_stats(built_start, built_end, window_min = 1000)
report(is.na(built$p90_min),
       paste("a cohort whose curve stops at 0.20 reports its p90 as NA rather than as",
             "the %.0f min p90 of the stays that closed"),
       quantile(built_end[1:80], 0.90))

r2e_starts <- as.numeric(combined[[INTERVALS$R2E[1]]])
r2e_ends   <- as.numeric(combined[[INTERVALS$R2E[2]]])
r2e_risk   <- !is.na(r2e_starts)
r2e_closed <- !is.na(r2e_ends[r2e_risk])
r2e_time   <- ifelse(r2e_closed, r2e_ends[r2e_risk] - r2e_starts[r2e_risk],
                     window - r2e_starts[r2e_risk])
r2e_lowest <- min(km_survival_curve(r2e_time, as.integer(r2e_closed))$s)
report(is.na(kpis$r2e_dwell_time$p90_min) == (r2e_lowest > 0.10),
       "the R2E dwell p90 is NA exactly when its curve stays above 0.10 (it falls to %.3f)",
       r2e_lowest)

# The reached case on a constructed cohort, so a quantile the curve does reach
# is located whatever the run: 95 stays closing within 95 minutes and 5 still
# open, the curve falling to 0.05.
reached <- censored_interval_stats(rep(0, 100), c(seq_len(95), rep(NA, 5)),
                                   window_min = 1000)
report(isTRUE(all.equal(reached$p90_min, 90)),
       paste("a cohort whose curve falls past 0.10 reports its p90 at %s min, so an NA",
             "marks a quantile the run cannot locate rather than one it does not look for"),
       format(reached$p90_min))

# ── 6. One definition of the observation window ────────────────────────────

cat("\n-- one window definition --\n")

report(window == ceiling(max(combined$start_time, na.rm = TRUE) / DAY_MIN) * DAY_MIN,
       "observation_window_min() is the window the holding reconstruction uses (%.0f min)",
       window)

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All censored interval estimation checks passed.\n")
quit(status = 0)
