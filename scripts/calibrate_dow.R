#!/usr/bin/env Rscript
##############################################################################
## scripts/calibrate_dow.R                                                  ##
## CLI entry point — fit the died-of-wounds ceilings to a target rate       ##
##############################################################################
#
# Usage:
#   Rscript scripts/calibrate_dow.R --scenario default
#   Rscript scripts/calibrate_dow.R --scenario high_intensity
#   Rscript scripts/calibrate_dow.R --config my_config.json --target 0.012 --days 30
#   Rscript scripts/calibrate_dow.R --quick          # wiring test only
#
# Why this exists. The shipped ceilings (`p1_p_max`, `p2_p_max`) were reached by
# an iterative Monte Carlo procedure that was never written down as a command,
# so neither the shipped values nor a recalibration to a new scenario could be
# reproduced. This is that procedure. It measures the pooled treated-cohort
# died-of-wounds rate (R/dow_calibration.R, the same measurement
# scripts/check_dow_calibration.R asserts) at a trial Priority 1 ceiling, and
# searches for the ceiling at which the rate meets the target by bracketing
# root search. The Priority 2 ceiling follows at a fixed ratio, the shipped
# pair's own by default, so the two are fitted jointly as one parameter.
#
# What the target must be. The treated cohort, the casualties who reached a
# surgical facility alive, not the whole wounded population: a rate over all
# wounded understates mortality because casualties who died before reaching
# care are outside a historical treated-cohort denominator by construction.
# Everything else the model needs (force size, establishment, casualty
# generation, triage split, treatment efficacy factors) is set from sources
# beforehand, since the ceilings are entangled with the efficacy factors and
# a changed factor invalidates a fitted ceiling (README, Calibrating to a New
# Scenario).
#
# It reports the fitted pair with the interval of the rate it achieved and
# writes nothing to env_data.json: a fitted value enters a configuration by a
# reviewed edit, and the shipped values change only through a discussed issue
# (CLAUDE.md). With --write-csv it also records every evaluation. Confirm a fit
# with the regression check, which measures at the check's own sizing:
#   Rscript scripts/check_dow_calibration.R --scenario <name>
#
# Each evaluation is --measurements x --reps replications over --days, at
# control seeds fixed across evaluations. A search takes about five
# evaluations, so the default sizing (3 x 50 replications over 30 days) is
# roughly the cost of five runs of scripts/check_dow_calibration.R for one
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
              help = "Scenario profile whose ceilings are fitted [default: %default]"),
  make_option("--config", type = "character", default = NULL,
              help = paste("Configuration file to fit, the shipped env_data.json or one",
                           "saved from the Shiny console [default: env_data.json]")),
  make_option("--target", type = "double", default = NULL,
              help = paste("Treated-cohort rate to reach, as a proportion",
                           "[default: the scenario's historical anchor]")),
  make_option("--tolerance", type = "double", default = 5e-4,
              help = "Stop when the measured rate is this close to the target [default: %default]"),
  make_option("--days", type = "integer", default = 30L,
              help = "Campaign length of each replication, in days [default: %default]"),
  make_option("--reps", type = "integer", default = 50L,
              help = "Replications per measurement [default: %default]"),
  make_option("--measurements", type = "integer", default = 3L,
              help = "Independent measurements pooled per evaluation [default: %default]"),
  make_option("--lower", type = "double", default = NULL,
              help = "Lower end of the Priority 1 ceiling bracket [default: half the configured]"),
  make_option("--upper", type = "double", default = NULL,
              help = "Upper end of the Priority 1 ceiling bracket [default: twice the configured]"),
  make_option("--ratio", type = "double", default = NULL,
              help = "Priority 1 ceiling over Priority 2 [default: the configuration's own]"),
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

anchor <- DOW_TARGETS[[opt$scenario]]
target <- if (!is.null(opt$target)) opt$target else anchor$rate
if (is.null(target)) {
  stop(sprintf("no historical anchor is defined for scenario '%s'; give --target", opt$scenario),
       call. = FALSE)
}
if (target <= 0 || target >= 1) stop("--target must lie in (0, 1), found ", target, call. = FALSE)

shipped <- build_environment(resolve_scenario(json_data, opt$scenario))$vars$dow$params
ratio <- if (!is.null(opt$ratio)) opt$ratio else shipped$p1_p_max / shipped$p2_p_max
lower <- if (!is.null(opt$lower)) opt$lower else 0.5 * shipped$p1_p_max
upper <- if (!is.null(opt$upper)) opt$upper else 2 * shipped$p1_p_max
if (!(lower > 0 && upper > lower)) {
  stop(sprintf("the bracket must satisfy 0 < lower < upper, found [%g, %g]", lower, upper),
       call. = FALSE)
}

seeds <- DOW_CONTROL_SEEDS[seq_len(opt$measurements)]

#' Pooled treated-cohort rate and interval at one Priority 1 ceiling
#'
#' @param p1 Priority 1 ceiling to measure at.
#' @return Named numeric of `rate`, `ci_lower` and `ci_upper`.
evaluate_ceiling <- function(p1) {
  rates <- measure_treated_cohort(json_data, opt$scenario, p1_p_max = p1, p1_over_p2 = ratio,
                                  n_reps = opt$reps, n_days = opt$days, seeds = seeds)
  s <- pooled_rate_summary(rates)
  cat(sprintf("  p1_p_max = %.5f (p2_p_max = %.5f): %.3f%%  95%% CI [%.3f%%, %.3f%%]\n",
              p1, p1 / ratio, 100 * s$mean, 100 * s$ci_lower, 100 * s$ci_upper))
  c(rate = s$mean, ci_lower = s$ci_lower, ci_upper = s$ci_upper)
}

cat(sprintf("Fitting the died-of-wounds ceilings of '%s' (%s)\n", opt$scenario, config_path))
cat(sprintf("  target: treated-cohort rate %.3f%%%s\n", 100 * target,
            if (!is.null(anchor) && is.null(opt$target)) {
              sprintf(" (%s%s)", anchor$label,
                      if (anchor$kind == "bound") ", an upper bound" else "")
            } else ""))
cat(sprintf("  bracket on p1_p_max: [%.5f, %.5f]; p1 over p2 held at %.4f\n", lower, upper, ratio))
cat(sprintf("  each evaluation: %d x %d replications over %d days\n",
            opt$measurements, opt$reps, opt$days))
if (isTRUE(opt$quick)) cat("QUICK MODE: too few replications to calibrate. Wiring test only.\n")
cat("\n")

fit <- fit_dow_ceiling(evaluate_ceiling, target, lower, upper, tol_rate = opt$tolerance,
                       max_iter = opt$`max-iterations`)

cat("\n")
cat(sprintf("Stopped: %s\n", fit$reason))
cat(sprintf("Fitted p1_p_max = %.4f, p2_p_max = %.4f\n", fit$p1_p_max, fit$p1_p_max / ratio))
cat(sprintf("  achieved treated-cohort rate %.3f%% (95%% CI [%.3f%%, %.3f%%]) against %.3f%%\n",
            100 * fit$rate, 100 * fit$ci_lower, 100 * fit$ci_upper, 100 * target))
cat(sprintf("  shipped for '%s': p1_p_max = %.4f, p2_p_max = %.4f\n", opt$scenario,
            shipped$p1_p_max, shipped$p2_p_max))
cat(sprintf("Confirm with: Rscript scripts/check_dow_calibration.R --scenario %s\n", opt$scenario))

if (!is.null(opt$`write-csv`)) {
  dir.create(dirname(opt$`write-csv`), recursive = TRUE, showWarnings = FALSE)
  write.csv(fit$trace, opt$`write-csv`, row.names = FALSE)
  cat(sprintf("Search trace written to %s\n", opt$`write-csv`))
}

quit(status = if (isTRUE(fit$converged)) 0L else 1L)
