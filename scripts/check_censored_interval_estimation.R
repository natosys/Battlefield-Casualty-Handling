#!/usr/bin/env Rscript
##############################################################################
## scripts/check_censored_interval_estimation.R                             ##
## Regression check — an interval still open when the window closes is      ##
## carried as the lower bound it is, not dropped                            ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_censored_interval_estimation.R
#   Rscript scripts/check_censored_interval_estimation.R --days 30 --seed 42
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. compute_treatment_interval_kpis() differenced a start
# attribute against an end attribute and kept the casualties carrying both, so
# a casualty still at the echelon when the run ended was dropped (Issue #331).
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
#      to an interval that does not censor at the shipped configuration.
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
#      and shorter-staying population.
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
    if (share >= MATERIAL_CENSORING) {
      report(abs(est - truth) <= abs(naive - truth),
             paste("%s re-censored at %.0f%% of the window (%d stays, %.1f%% censored):",
                   "restricted mean %.0f is nearer the true %.0f than the completed-case",
                   "%.0f"),
             nm, 100 * frac, length(st), 100 * share, est, truth, naive)
    } else {
      report(abs(est - naive) <= AGREEMENT_TOLERANCE * truth,
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
  naive <- completed_case_mean(combined[[cols[1]]], combined[[cols[2]]], window)
  report(k$mean_min > naive,
         "%s dwell mean %.0f min exceeds the completed-case %.0f it replaces",
         nm, k$mean_min, naive)
}

report(kpis$r2b_r2e_transit_time$n_censored == 0 &&
         !is.na(kpis$r2b_r2e_transit_time$censored_share),
       "the transit interval reports a censored share of zero rather than reporting none")

report(isTRUE(all.equal(kpis$r2b_r2e_transit_time$mean_min,
                        completed_case_mean(combined$r2b_departure_time,
                                            combined$r2e_arrival_time,
                                            INTERVAL_RESTRICTION_MIN))),
       paste("the uncensored transit mean is unchanged at %.5f min, the estimator",
             "degenerating on real data"),
       kpis$r2b_r2e_transit_time$mean_min)

cat("\n-- the restriction horizon rests on observed completions --\n")

for (nm in names(INTERVALS)) {
  k <- kpi_of[[nm]]
  report(!is.na(k$tail_share) && k$tail_share == 0,
         paste("no part of the %s dwell mean lies beyond the last observed completion,",
               "so none of it is the flat tail (tail share %.1f%%)"),
         nm, 100 * k$tail_share)
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
report(lowest > 0.10 && is.na(kpis$r2b_dwell_time$p90_min),
       paste("the R2B dwell curve falls only to %.3f, so its p90 is reported as NA",
             "rather than as the %.0f min p90 of the stays that closed"),
       lowest, quantile(r2b_time[r2b_closed], 0.90))

report(!is.na(kpis$r2e_dwell_time$p90_min),
       paste("the R2E dwell curve does reach its p90, reported at %.0f min, so an NA",
             "marks a quantile the run cannot locate rather than one it does not look for"),
       kpis$r2e_dwell_time$p90_min)

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
