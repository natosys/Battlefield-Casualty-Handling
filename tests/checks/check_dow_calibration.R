#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_dow_calibration.R                                     ##
## Regression check — died-of-wounds rate against its campaign's anchor     ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_dow_calibration.R                     # all profiles, 3 x 50 reps
#   Rscript tests/checks/check_dow_calibration.R --quick             # 2 x 10 reps, 10 days
#                                                                     (a smoke test only)
#   Rscript tests/checks/check_dow_calibration.R --scenario default  # one profile
#   Rscript tests/checks/check_dow_calibration.R --measurements 5 --reps 50
#   Rscript tests/checks/check_dow_calibration.R --days 360 --reps 10 --write-csv out.csv
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step. A full run executes 450 replications and takes a
# few hours on four cores; --quick finishes in about a minute but is a wiring
# test, not a calibration test, and says so in its output.
#
# Why this check exists: the died-of-wounds base rates are fitted to a
# historical anchor, and for the default and its `moderate_intensity` alias,
# which are the same configuration, that anchor is the three deaths among the
# "over 650" casualties who passed through the Ajax Bay field hospital and its
# forward stations at Teal Inlet and Fitzroy, a rate of approximately 0.46%
# (README — Calibration). Nothing in an ordinary run compares the model against
# it, so a change to routing or resources can move the fitted configuration off
# its anchor without anything failing.
#
# Three properties of the anchors govern what this check asserts.
#
#   1. The denominator is the treated cohort — casualties who reached a
#      surgical facility alive — not all wounded. The model's matching cohort
#      is the set of casualties reaching an R2B or R2E facility, identified by
#      the r2b_treated/r2e_treated attributes, which are set on entry to each
#      facility's trajectory and therefore before that facility's own DOW
#      check, so a casualty who dies there is inside their own denominator.
#
#   2. The default is fitted to Ajax Bay, so the pooled interval must contain
#      0.46%. The figure is strictly an upper bound, "over 650" being inexact,
#      and is used as the campaign's value because it is the one treated-cohort
#      figure from the campaign and era of care the default models.
#
#   3. high_intensity shares the default's mortality model and is not fitted:
#      its rate is a model output, bounded from above by Okinawa's 3.4%, a 1945
#      rate that 1982-era care is taken not to exceed. Only an overshoot fails,
#      and it is measured over Okinawa's 82 days rather than the default's 30.
#
# Under the accrual mechanism the same runs also measure the share of the
# treated cohort's expected deaths that the post-operative hazard causes, the
# quantity the hazard rates are fitted to (DOW_LATE_SHARE_TARGET,
# R/dow_calibration.R), and the check fails where it falls outside
# DOW_LATE_SHARE_TOLERANCE of the target. The share is an expected count rather
# than a count of deaths, so it carries no Bernoulli noise and needs no
# interval.
#
# The check pools independent measurements rather than taking one. That
# departure from this project's usual practice is deliberate, and is why the
# overshoot was reported. Died of wounds averages about one death per
# replication, so a single 50-replication measurement does not resolve it:
# three measurements of the base configuration shipped before the
# moderate-intensity calibration became the default returned 0.524%,
# 0.359% and 0.368%, a spread of 0.17 percentage points, about as wide as the
# whole interval any one of them reports.
# The replication is the unit of analysis, which it is entitled to be because
# run_replications() makes replications independent. It did not while they
# were antithetically paired, and this check folded partners into pair means
# to work around that; the pairing has since been withdrawn, so
# the fold is gone and every replication counts once.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/dow_calibration.R")

args  <- commandArgs(trailingOnly = TRUE)
quick <- "--quick" %in% args

#' Read one flagged command line argument
#'
#' @param flag Flag to look for, including its leading dashes.
#' @param default Value returned when the flag is absent or carries no value.
#' @return The argument following the flag, or `default`.
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

# Each shipped configuration is checked against the historical anchor of the
# campaign it models, which R/dow_calibration.R holds beside the fitter that
# searches for base rates reproducing it. "point" is fitted, so the interval
# must contain the anchor; "bound" is one-sided, so only an overshoot fails.
#
#   Ajax Bay: 3 deaths among "over 650" casualties reaching forward surgical
#     care (Westphalen, 2018). Fitted; the default and its alias, 30 days.
#   Okinawa: 3.4% of casualties reaching a hospital alive (Marble, 2025). An
#     upper bound on high_intensity, 82 days.

SCENARIOS    <- if ("--scenario" %in% args) arg_value("--scenario", "default") else
                  names(DOW_TARGETS)
N_MEASURE    <- as.integer(arg_value("--measurements", if (quick) 2L else 3L))
N_REPS       <- as.integer(arg_value("--reps",         if (quick) 10L else 50L))
#' Horizon given on the command line, or NULL to measure each anchor at its own
DAYS_ARG     <- if ("--days" %in% args) as.integer(arg_value("--days", NULL)) else
                  if (quick) 10L else NULL
#' Horizon the post-operative multiplier is fitted at, the one the share is gated at
#'
#' @details Over a longer campaign queues lengthen and the risk banked before
#'   surgery grows, so the share falls; at any other horizon it is reported
#'   rather than asserted, the target describing a campaign of this length.
FIT_DAYS     <- 30L
#' Where to write each scenario's pooled measurement as CSV, or NULL to write none
#'
#' @details The sustained-horizon measurement Results.md reports is tracked this
#'   way; the check itself stays at the historical campaign's length, which is
#'   the horizon its anchors describe.
WRITE_CSV    <- if ("--write-csv" %in% args) arg_value("--write-csv", NULL) else NULL
# Fixed control seeds so a run is reproducible and two runs of this check on
# unchanged code agree exactly. Each seeds one independent measurement.
CONTROL_SEEDS <- DOW_CONTROL_SEEDS

if (N_MEASURE > length(CONTROL_SEEDS)) {
  stop(sprintf("--measurements above %d needs more control seeds", length(CONTROL_SEEDS)))
}
failures <- character(0)
json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Record a failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated failures, invisibly; called for its side effect.
fail <- function(...) failures <<- c(failures, sprintf(...))

#' Print one PASS or FAIL line
#'
#' @param ok Logical: whether the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Values interpolated into `fmt`.
#' @return The printed line, invisibly; called for its side effect.
report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}

# ── Measurement ─────────────────────────────────────────────────────────────

#' One independent measurement of a scenario at a given control seed
#'
#' @param scenario Name of the scenario profile.
#' @param seed Control seed the replication set is drawn from.
#' @return A numeric vector of one treated-cohort DOW rate per replication, with
#'   replications in which no casualty reached a facility removed.
run_measurement <- function(scenario, seed) {
  measure_dow_response(json_data, scenario, n_reps = N_REPS, n_days = check_days(scenario),
                       seeds = seed)
}

#' Campaign length a scenario is measured over
#'
#' @param scenario Name of the scenario profile.
#' @return The --days given, or else the length of the campaign its anchor describes.
check_days <- function(scenario) {
  if (!is.null(DAYS_ARG)) DAYS_ARG else DOW_TARGETS[[scenario]]$days
}

# ── Checks ──────────────────────────────────────────────────────────────────

cat(sprintf("DOW calibration check: %d measurement(s) x %d replications per scenario\n",
            N_MEASURE, N_REPS))
for (sc in SCENARIOS) {
  tgt <- DOW_TARGETS[[sc]]
  if (is.null(tgt)) stop(sprintf("no died-of-wounds target defined for scenario '%s'", sc))
  cat(sprintf("Target (%s, %d days): treated-cohort DOW rate %s %.2f%% (%s)\n", sc,
              check_days(sc), if (tgt$kind == "bound") "at or below" else "fitted to",
              100 * tgt$rate, tgt$label))
}
cat("\n")
if (quick) {
  cat("QUICK MODE — too few replications to judge calibration. Wiring test only.\n\n")
}

records <- list()

for (scenario in SCENARIOS) {
  singles <- numeric(0)
  pooled  <- numeric(0)
  early   <- 0
  late    <- 0

  for (k in seq_len(N_MEASURE)) {
    m_k   <- run_measurement(scenario, CONTROL_SEEDS[k])
    rates <- m_k$rates
    early <- early + m_k$early
    late  <- late + m_k$late
    if (!length(rates)) {
      fail("%s: no casualty reached a facility in measurement %d", scenario, k)
      next
    }
    singles <- c(singles, mean(rates))
    pooled  <- c(pooled, rates)
  }

  if (!length(pooled)) next

  n  <- length(pooled)
  m  <- mean(pooled)
  hw <- qt(0.975, df = n - 1) * sd(pooled) / sqrt(n)
  # Clamped at zero on the same basis as clamp_ci() (R/analysis.R): a
  # mortality rate cannot be negative. Clamping cannot mask an overshoot,
  # since it only ever moves the lower bound further below the bound.
  lo <- max(m - hw, 0)
  hi <- m + hw

  anchor <- DOW_TARGETS[[scenario]]
  share  <- if (early + late > 0) late / (early + late) else NA_real_
  records[[scenario]] <- data.frame(scenario = scenario, days = check_days(scenario),
                                    measurements = length(singles), replications = n,
                                    rate = m, ci_lower = lo, ci_upper = hi,
                                    target = anchor$rate, anchor = anchor$label,
                                    kind = anchor$kind, hazard_share = share,
                                    stringsAsFactors = FALSE)

  cat(sprintf("\n%s — %d replications\n", scenario, n))
  cat(sprintf("  individual measurements: %s\n",
              paste(sprintf("%.3f%%", 100 * singles), collapse = ", ")))
  cat(sprintf("  pooled: %.3f%%  95%% CI [%.3f%%, %.3f%%]\n", 100 * m, 100 * lo, 100 * hi))

  tgt <- DOW_TARGETS[[scenario]]
  if (tgt$kind == "bound") {
    # The bound is one-sided. A model below it agrees with the record; only a
    # model whose whole interval clears it is overshooting.
    ok <- lo <= tgt$rate
    if (!ok) {
      fail(paste0("%s: treated-cohort DOW rate %.3f%% (95%% CI [%.3f%%, %.3f%%]) overshoots ",
                  "the %.2f%% bound — the entire interval sits above it"),
           scenario, 100 * m, 100 * lo, 100 * hi, 100 * tgt$rate)
    }
    report(ok, "%s does not overshoot the %.2f%% treated-cohort bound (%s)",
           scenario, 100 * tgt$rate, tgt$label)
  } else if (check_days(scenario) != tgt$days) {
    cat(sprintf("  [note] %s: the %.2f%% %s anchor describes %d days and is not asserted at %d\n",
                scenario, 100 * tgt$rate, tgt$label, tgt$days, check_days(scenario)))
  } else {
    # The configuration is fitted to this anchor, so a measurement whose
    # interval excludes it means the fit no longer holds.
    ok <- lo <= tgt$rate && hi >= tgt$rate
    if (!ok) {
      fail(paste0("%s: treated-cohort DOW rate %.3f%% (95%% CI [%.3f%%, %.3f%%]) excludes ",
                  "the %.2f%% %s anchor it is fitted to"),
           scenario, 100 * m, 100 * lo, 100 * hi, 100 * tgt$rate, tgt$label)
    }
    report(ok, "%s: the interval [%.3f%%, %.3f%%] contains the %.2f%% %s anchor",
           scenario, 100 * lo, 100 * hi, 100 * tgt$rate, tgt$label)
  }

  # The hazard's share is checked where the configuration has a hazard to
  # measure; under the legacy mechanism, or with every rate at zero, no check
  # records expected deaths and the share is undefined.
  if (!is.na(share) && late > 0 && (check_days(scenario) != FIT_DAYS || tgt$kind != "point")) {
    cat(sprintf("  [note] %s: post-operative care carries %.3f of expected deaths at %d days;",
                scenario, share, check_days(scenario)),
        sprintf("the %.2f target is asserted at %d days alone\n", DOW_LATE_SHARE_TARGET, FIT_DAYS))
  } else if (!is.na(share) && late > 0) {
    ok <- abs(share - DOW_LATE_SHARE_TARGET) <= DOW_LATE_SHARE_TOLERANCE
    if (!ok) {
      fail("%s: the post-operative hazard causes %.3f of expected deaths, outside %.2f of %.2f",
           scenario, share, DOW_LATE_SHARE_TOLERANCE, DOW_LATE_SHARE_TARGET)
    }
    report(ok, "%s: the post-operative hazard causes %.3f of expected deaths (target %.2f +/- %.2f)",
           scenario, share, DOW_LATE_SHARE_TARGET, DOW_LATE_SHARE_TOLERANCE)
  }

  # Not a failure, but the reason this check pools: if the individual
  # measurements disagree by more than the pooled interval spans, one
  # measurement on its own would not have been evidence either way.
  if (length(singles) > 1) {
    spread <- max(singles) - min(singles)
    cat(sprintf("  [note] single-measurement spread %.3f pp against a pooled half-width of %.3f pp%s\n",
                100 * spread, 100 * hw,
                if (spread > 2 * hw) " — a single measurement would not have settled this" else ""))
  }
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (!is.null(WRITE_CSV) && length(records)) {
  dir.create(dirname(WRITE_CSV), recursive = TRUE, showWarnings = FALSE)
  write.csv(do.call(rbind, records), WRITE_CSV, row.names = FALSE)
  cat(sprintf("Pooled measurements written to %s\n", WRITE_CSV))
}

if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All died-of-wounds calibration checks passed.\n")
quit(status = 0)
