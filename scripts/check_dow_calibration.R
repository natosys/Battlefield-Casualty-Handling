#!/usr/bin/env Rscript
##############################################################################
## scripts/check_dow_calibration.R                                          ##
## Regression check — died-of-wounds rate against its campaign's anchor     ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_dow_calibration.R                     # all profiles, 3 x 50 reps each
#   Rscript scripts/check_dow_calibration.R --quick             # 2 x 10 reps, 10 days — smoke test only
#   Rscript scripts/check_dow_calibration.R --scenario default  # one profile
#   Rscript scripts/check_dow_calibration.R --measurements 5 --reps 50
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step. A full run executes 450 replications and takes a
# few hours on four cores; --quick finishes in about a minute but is a wiring
# test, not a calibration test, and says so in its output.
#
# Why this check exists: each configuration's mortality ceilings are
# calibrated against a historical anchor, and for the two Falklands-calibrated
# configurations that anchor is the Ajax Bay Advanced Surgical Centre's three
# deaths among the "over 650" casualties who reached forward surgical care, a
# rate of approximately 0.46% (README — Parameter Calibration). Nothing in an
# ordinary run compares the model against it. The comparison was made by hand
# at each recalibration, which is how the model came to be reported as
# overshooting the bound by roughly a third when it was not.
#
# Two properties of the target govern what this check asserts.
#
#   1. The denominator is the treated cohort — casualties who reached a
#      surgical facility alive — not all wounded. The model's matching cohort
#      is the set of casualties reaching an R2B or R2E facility, identified by
#      the r2b_treated/r2e_treated attributes, which are set on entry to each
#      facility's trajectory and therefore before that facility's own DOW
#      check, so a casualty who dies there is inside their own denominator.
#
#   2. The rate is an upper bound, not a point estimate, because "over 650"
#      is inexact. The failure condition is therefore one-sided: the model
#      overshooting the bound. A configuration sitting comfortably below it
#      is consistent with the historical record and passes, which is why
#      moderate_intensity passes at roughly 0.27%.
#
# high_intensity is checked against a different anchor, because it models a
# different campaign under a different standard of care: the US Army on
# Okinawa reported 3.4% of casualties who reached a hospital alive dying
# there (Marble, 2025). That figure is a reported rate against a stated
# denominator rather than a bound derived from an inexact one, so the test
# is two-sided — the profile is calibrated to reproduce it, not merely to
# stay under it — at the same plus or minus 2 percentage point tolerance
# the profile was calibrated to.
#
# The check pools independent measurements rather than taking one. That
# departure from this project's usual practice is deliberate, and is why the
# overshoot was reported. Died of wounds averages about one death per
# replication, so a single 50-replication measurement does not resolve it:
# three measurements of the shipped base configuration returned 0.524%,
# 0.359% and 0.368%, a spread of 0.17 percentage points, about as wide as the
# whole interval any one of them reports.
# The replication is the unit of analysis, which it is entitled to be because
# run_replications() makes replications independent. It did not while they
# were antithetically paired, and this check folded partners into pair means
# to work around that; the pairing has since been withdrawn (Issue #189), so
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

args  <- commandArgs(trailingOnly = TRUE)
quick <- "--quick" %in% args

arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

# Each shipped configuration is checked against the historical anchor of the
# campaign it models. "bound" is one-sided (only an overshoot fails); "point"
# is two-sided, against TOLERANCE.
#
#   Ajax Bay: 3 deaths among "over 650" casualties reaching forward surgical
#     care (Westphalen, 2018), an upper bound because the denominator is
#     inexact. Applies to the two Falklands-calibrated configurations.
#   Okinawa: 3.4% of casualties who reached a hospital alive died there
#     (Marble, 2025), a reported rate against a stated denominator.
DOW_TARGETS <- list(
  default            = list(rate = 0.0046, kind = "bound", label = "Ajax Bay"),
  moderate_intensity = list(rate = 0.0046, kind = "bound", label = "Ajax Bay"),
  high_intensity     = list(rate = 0.0340, kind = "point", label = "Okinawa")
)
TOLERANCE <- 0.02

SCENARIOS    <- if ("--scenario" %in% args) arg_value("--scenario", "default") else
                  names(DOW_TARGETS)
N_MEASURE    <- as.integer(arg_value("--measurements", if (quick) 2L else 3L))
N_REPS       <- as.integer(arg_value("--reps",         if (quick) 10L else 50L))
CHECK_DAYS   <- as.integer(arg_value("--days",         if (quick) 10L else 30L))
# Fixed control seeds so a run is reproducible and two runs of this check on
# unchanged code agree exactly. Each seeds one independent measurement.
CONTROL_SEEDS <- c(42L, 777L, 20260808L, 13L, 20261L)

if (N_MEASURE > length(CONTROL_SEEDS)) {
  stop(sprintf("--measurements above %d needs more control seeds", length(CONTROL_SEEDS)))
}
failures <- character(0)

fail <- function(...) failures <<- c(failures, sprintf(...))

report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}

# ── Measurement ─────────────────────────────────────────────────────────────

#' Treated-cohort DOW rate for one replication set
#'
#' @param mon Monitoring list from run_replications()
#' @return Numeric vector of per-replication rates, in replication order
treated_cohort_rates <- function(mon) {
  treated <- mon$attributes %>%
    filter(key %in% c("r2b_treated", "r2e_treated")) %>%
    distinct(replication, name)
  died <- mon$attributes %>%
    filter(key == "dow", value == 1) %>%
    distinct(replication, name)

  vapply(sort(unique(mon$arrivals$replication)), function(r) {
    cohort <- treated$name[treated$replication == r]
    if (!length(cohort)) return(NA_real_)
    sum(died$name[died$replication == r] %in% cohort) / length(cohort)
  }, numeric(1))
}

#' One independent measurement of a scenario at a given control seed
run_measurement <- function(scenario, seed) {
  json     <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
  env_data <<- build_environment(resolve_scenario(json, scenario))
  day_min  <<- 1440L
  counts   <<- sapply(env_data$elms, length)

  set.seed(seed)
  treated_cohort_rates(run_replications(N_REPS, CHECK_DAYS))
}

# ── Checks ──────────────────────────────────────────────────────────────────

cat(sprintf("DOW calibration check: %d measurement(s) x %d replications x %d days per scenario\n",
            N_MEASURE, N_REPS, CHECK_DAYS))
for (sc in SCENARIOS) {
  tgt <- DOW_TARGETS[[sc]]
  if (is.null(tgt)) stop(sprintf("no died-of-wounds target defined for scenario '%s'", sc))
  cat(sprintf("Target (%s): treated-cohort DOW rate %s %.2f%% (%s)\n", sc,
              if (tgt$kind == "bound") "at or below" else
                sprintf("within %.0f pp of", 100 * TOLERANCE),
              100 * tgt$rate, tgt$label))
}
cat("\n")
if (quick) {
  cat("QUICK MODE — too few replications to judge calibration. Wiring test only.\n\n")
}

for (scenario in SCENARIOS) {
  singles <- numeric(0)
  pooled  <- numeric(0)

  for (k in seq_len(N_MEASURE)) {
    rates <- run_measurement(scenario, CONTROL_SEEDS[k])
    if (all(is.na(rates))) {
      fail("%s: no casualty reached a facility in measurement %d", scenario, k)
      next
    }
    singles <- c(singles, mean(rates, na.rm = TRUE))
    pooled  <- c(pooled, rates[!is.na(rates)])
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
  } else {
    # A reported rate is a point estimate, so the model has to reach it as
    # well as stay under it. The tolerance is the one the profile was
    # calibrated to; the interval spanning the target is reported alongside
    # but is not the failure condition, since a wide interval would then
    # pass more easily than a narrow one.
    ok <- abs(m - tgt$rate) <= TOLERANCE
    if (!ok) {
      fail(paste0("%s: treated-cohort DOW rate %.3f%% (95%% CI [%.3f%%, %.3f%%]) misses ",
                  "the %.2f%% %s target by more than %.0f pp"),
           scenario, 100 * m, 100 * lo, 100 * hi, 100 * tgt$rate, tgt$label, 100 * TOLERANCE)
    }
    report(ok, "%s reproduces the %.2f%% %s target within %.0f pp (interval %s it)",
           scenario, 100 * tgt$rate, tgt$label, 100 * TOLERANCE,
           if (lo <= tgt$rate && hi >= tgt$rate) "spans" else "does not span")
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
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All died-of-wounds calibration checks passed.\n")
quit(status = 0)
