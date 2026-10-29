#!/usr/bin/env Rscript
##############################################################################
## scripts/calibrate_dow.R                                                  ##
## CLI entry point — fit the died-of-wounds base rates and post-operative ##
## multiplier to their targets                                              ##
##############################################################################
#
# Usage:
#   Rscript scripts/calibrate_dow.R --scenario default
#   Rscript scripts/calibrate_dow.R --scenario default --hold-legacy-rate
#   Rscript scripts/calibrate_dow.R --config my_config.json --target 0.012 --days 30
#   Rscript scripts/calibrate_dow.R --quick          # wiring test only
#
# Why this exists. The care-state model has two groups of fitted values: the
# base rates (`dow.rates`), a death rate per day of untreated injury for each
# priority, and the post-operative multiplier (`dow.multipliers.post_op`). No
# source gives either directly, so each is searched for until the model meets
# a historical target. This is that procedure, written down so the shipped
# values and a recalibration to a new scenario can both be reproduced. It
# measures the pooled treated-cohort died-of-wounds rate (R/dow_calibration.R,
# the same measurement tests/checks/check_dow_calibration.R asserts) and the
# share of the cohort's expected deaths that accrue after surgery.
#
# Two searches alternate for --rounds rounds, each starting from the other's
# last fit, since each target moves with both values. The post-operative
# multiplier is searched until the post-operative share meets --late-share
# (DOW_LATE_SHARE_TARGET by default). The Priority 1 base rate is searched until
# the treated-cohort rate meets the target, the Priority 2 and 3 rates scaled
# with it so their ratios hold, so the three are one parameter.
# --hold-legacy-rate sets the rate target to the one the configuration produces
# under the legacy mechanism at the same seeds and horizon, which is how the
# shipped rates were fitted when the care-state model replaced it: held where
# the earlier model held mortality, beneath the Ajax Bay bound, rather than
# raised to it.
#
# What the target must be. The treated cohort, the casualties who reached a
# surgical facility alive, not the whole wounded population: a rate over all
# wounded understates mortality because casualties who died before reaching
# care are outside a historical treated-cohort denominator by construction.
# Everything else the model needs (force size, establishment, casualty
# generation, triage split, the sourced multipliers) is set from sources
# beforehand, since the fitted values are entangled with them and a changed
# input invalidates a fit (README, Calibrating to a New Scenario).
#
# It reports the fitted values with the interval of the rate they achieved and
# writes nothing to env_data.json: a fitted value enters a configuration by a
# reviewed edit, and the shipped values change only through a discussed issue
# (CLAUDE.md). With --write-csv it also records every evaluation. Confirm a fit
# with the regression check, which measures at the check's own sizing:
#   Rscript tests/checks/check_dow_calibration.R --scenario <name>
#
# Each evaluation is --measurements x --reps replications over --days, at
# control seeds fixed across evaluations. A search takes about five
# evaluations, so the default sizing (3 x 50 replications over 30 days) is
# roughly the cost of five runs of tests/checks/check_dow_calibration.R for one
# profile. --quick is a wiring test and says so.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
  library(optparse)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/dow_calibration.R")
source("R/cli.R")
source("R/run_manifest.R")

option_list <- list(
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile whose died-of-wounds model is fitted [default: %default]"),
  make_option("--config", type = "character", default = NULL,
              help = paste("Configuration file to fit, the shipped env_data.json or one",
                           "saved from the Shiny console [default: env_data.json]")),
  make_option("--target", type = "double", default = NULL,
              help = paste("Treated-cohort rate to reach, as a proportion",
                           "[default: the scenario's historical anchor]")),
  make_option("--hold-legacy-rate", action = "store_true", default = FALSE,
              help = paste("Take the rate target from the configuration run under the legacy",
                           "mechanism at the same seeds and horizon")),
  make_option("--late-share", type = "double", default = DOW_LATE_SHARE_TARGET,
              help = paste("Share of the treated cohort's expected deaths that should accrue",
                           "after surgery [default: %default]")),
  make_option("--rounds", type = "integer", default = 2L,
              help = "Alternations of the multiplier and base rate searches [default: %default]"),
  make_option("--tolerance", type = "double", default = 5e-4,
              help = "Stop when the measured rate is this close to the target [default: %default]"),
  make_option("--days", type = "integer", default = 30L,
              help = "Campaign length of each replication, in days [default: %default]"),
  make_option("--reps", type = "integer", default = 50L,
              help = "Replications per measurement [default: %default]"),
  make_option("--measurements", type = "integer", default = 3L,
              help = "Independent measurements pooled per evaluation [default: %default]"),
  make_option("--lower", type = "double", default = NULL,
              help = "Lower end of the Priority 1 base rate bracket [default: a quarter of the configured]"),
  make_option("--upper", type = "double", default = NULL,
              help = "Upper end of the Priority 1 base rate bracket [default: twice the configured]"),
  make_option("--max-iterations", type = "integer", default = 12L,
              help = "Cap on evaluations after the two bracket ends [default: %default]"),
  make_option("--write-csv", type = "character", default = NULL,
              help = "Write every evaluation of the search to this CSV file"),
  make_option("--quick", action = "store_true", default = FALSE,
              help = "Wiring test: 1 x 8 replications over 5 days, two iterations")
)

opt <- parse_args(OptionParser(option_list = option_list))
config_path <- resolve_config_path(opt$config)
json_data <- read_config(config_path)

if (isTRUE(opt$quick)) {
  opt$measurements <- 1L
  opt$reps <- 8L
  opt$days <- 5L
  opt$`max-iterations` <- 2L
}
if (opt$measurements < 1L || opt$measurements > length(DOW_CONTROL_SEEDS)) {
  stop(sprintf("--measurements must be between 1 and %d, found %d",
               length(DOW_CONTROL_SEEDS), opt$measurements), call. = FALSE)
}
if (opt$reps < 2L) stop("--reps must be at least 2, found ", opt$reps, call. = FALSE)
if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)

if (opt$rounds < 1L) stop("--rounds must be at least 1, found ", opt$rounds, call. = FALSE)
if (opt$`late-share` <= 0 || opt$`late-share` >= 1) {
  stop("--late-share must lie in (0, 1), found ", opt$`late-share`, call. = FALSE)
}

resolved <- build_environment(resolve_scenario(json_data, opt$scenario))$vars$dow
shipped <- resolved$rates
lower <- if (!is.null(opt$lower)) opt$lower else 0.25 * shipped$p1_rate
upper <- if (!is.null(opt$upper)) opt$upper else 2 * shipped$p1_rate
if (!(lower > 0 && upper > lower)) {
  stop(sprintf("the bracket must satisfy 0 < lower < upper, found [%g, %g]", lower, upper),
       call. = FALSE)
}
fit_late <- isTRUE(resolved$care_state$enabled == 1) && resolved$multipliers$post_op > 0

seeds <- DOW_CONTROL_SEEDS[seq_len(opt$measurements)]

#' Measure the configuration at one base rate and post-operative multiplier
#'
#' @param p1 Priority 1 base rate per day to measure at.
#' @param q Post-operative multiplier to measure at.
#' @param care_state 1 or 0 to override the model, or NULL for the configured one.
#' @return Named numeric of `rate`, `ci_lower`, `ci_upper` and `share`.
evaluate <- function(p1, q, care_state = NULL) {
  m <- measure_dow_response(json_data, opt$scenario, p1_rate = p1, post_op = q,
                            care_state = care_state,
                            n_reps = opt$reps, n_days = opt$days, seeds = seeds)
  s <- pooled_rate_summary(m$rates)
  share <- if (m$early + m$late > 0) m$late / (m$early + m$late) else 0
  cat(sprintf("  p1_rate = %.5f, post_op = %.5f: %.3f%%  95%% CI [%.3f%%, %.3f%%]; post-op share %.3f\n",
              p1, q, 100 * s$mean, 100 * s$ci_lower, 100 * s$ci_upper, share))
  c(rate = s$mean, ci_lower = s$ci_lower, ci_upper = s$ci_upper, share = share)
}

anchor <- DOW_TARGETS[[opt$scenario]]
cat(sprintf("Fitting the died-of-wounds model of '%s' (%s)\n", opt$scenario, config_path))
cat(sprintf("  each evaluation: %d x %d replications over %d days\n",
            opt$measurements, opt$reps, opt$days))
if (isTRUE(opt$quick)) cat("QUICK MODE: too few replications to calibrate. Wiring test only.\n")

target <- if (!is.null(opt$target)) {
  opt$target
} else if (isTRUE(opt$`hold-legacy-rate`)) {
  cat("\nMeasuring the legacy mechanism's rate\n")
  evaluate(shipped$p1_rate, resolved$multipliers$post_op, care_state = 0)[["rate"]]
} else {
  anchor$rate
}
if (is.null(target)) {
  stop(sprintf("no historical anchor is defined for scenario '%s'; give --target", opt$scenario),
       call. = FALSE)
}
if (target <= 0 || target >= 1) stop("the rate target must lie in (0, 1), found ", target,
                                     call. = FALSE)
cat(sprintf("  rate target: treated-cohort rate %.3f%%%s\n", 100 * target,
            if (isTRUE(opt$`hold-legacy-rate`) && is.null(opt$target)) {
              " (the legacy mechanism's)"
            } else if (!is.null(anchor) && is.null(opt$target)) {
              sprintf(" (%s%s)", anchor$label,
                      if (anchor$kind == "bound") ", an upper bound" else "")
            } else ""))
cat(sprintf("  bracket on p1_rate: [%.5f, %.5f]; Priority 2 and 3 rates held at their ratios to it\n",
            lower, upper))
if (fit_late) cat(sprintf("  post-operative share target: %.3f\n", opt$`late-share`))

p1 <- shipped$p1_rate
q <- resolved$multipliers$post_op
traces <- list()
converged <- TRUE
for (round in seq_len(if (fit_late) opt$rounds else 1L)) {
  if (fit_late) {
    cat(sprintf("\nRound %d: post-operative multiplier at p1_rate = %.5f\n", round, p1))
    qfit <- fit_post_op(function(x) evaluate(p1, x)[["share"]], opt$`late-share`,
                        lower = 0.25 * q, upper = 8 * q, max_iter = opt$`max-iterations`)
    cat(sprintf("  stopped: %s; post_op %.5f\n", qfit$reason, qfit$post_op))
    converged <- converged && isTRUE(qfit$converged)
    q <- qfit$post_op
    traces[[length(traces) + 1L]] <- cbind(round = round, search = "post_op",
                                           setNames(qfit$trace, c("iteration", "value", "rate",
                                                                  "ci_lower", "ci_upper")))
  }
  cat(sprintf("\nRound %d: base rate at post_op = %.5f\n", round, q))
  fit <- fit_dow_rate(function(p) evaluate(p, q)[c("rate", "ci_lower", "ci_upper")],
                      target, lower, upper, tol_rate = opt$tolerance,
                      max_iter = opt$`max-iterations`)
  cat(sprintf("  stopped: %s; p1_rate %.5f\n", fit$reason, fit$p1_rate))
  converged <- converged && isTRUE(fit$converged)
  p1 <- fit$p1_rate
  traces[[length(traces) + 1L]] <- cbind(round = round, search = "p1_rate",
                                         setNames(fit$trace, c("iteration", "value", "rate",
                                                               "ci_lower", "ci_upper")))
}

scale <- p1 / shipped$p1_rate
cat("\n")
cat(sprintf("Fitted rates per day: p1_rate = %.5f, p2_rate = %.5f, p3_rate = %.5f\n",
            p1, scale * shipped$p2_rate, scale * shipped$p3_rate))
if (fit_late) cat(sprintf("Fitted post-operative multiplier: post_op = %.5f\n", q))
cat(sprintf("  achieved treated-cohort rate %.3f%% (95%% CI [%.3f%%, %.3f%%]) against %.3f%%\n",
            100 * fit$rate, 100 * fit$ci_lower, 100 * fit$ci_upper, 100 * target))
cat(sprintf("  shipped for '%s': p1_rate = %.5f, post_op = %.5f\n", opt$scenario,
            shipped$p1_rate, resolved$multipliers$post_op))
cat(sprintf("Confirm with: Rscript tests/checks/check_dow_calibration.R --scenario %s\n",
            opt$scenario))
fit$trace <- do.call(rbind, traces)
fit$converged <- converged

if (!is.null(opt$`write-csv`)) {
  dir.create(dirname(opt$`write-csv`), recursive = TRUE, showWarnings = FALSE)
  write.csv(fit$trace, opt$`write-csv`, row.names = FALSE)
  cat(sprintf("Search trace written to %s\n", opt$`write-csv`))
}

quit(status = if (isTRUE(fit$converged)) 0L else 1L)
