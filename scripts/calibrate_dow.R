#!/usr/bin/env Rscript
##############################################################################
## scripts/calibrate_dow.R                                                  ##
## CLI entry point — fit the died-of-wounds ceilings to a target rate       ##
##############################################################################
#
# Usage:
#   Rscript scripts/calibrate_dow.R --scenario default
#   Rscript scripts/calibrate_dow.R --scenario default --hold-legacy-rate
#   Rscript scripts/calibrate_dow.R --config my_config.json --target 0.012 --days 30
#   Rscript scripts/calibrate_dow.R --quick          # wiring test only
#
# Why this exists. The shipped ceilings (`p1_p_max`, `p2_p_max`) were reached by
# an iterative Monte Carlo procedure that was never written down as a command,
# so neither the shipped values nor a recalibration to a new scenario could be
# reproduced. This is that procedure. It measures the pooled treated-cohort
# died-of-wounds rate (R/dow_calibration.R, the same measurement
# tests/checks/check_dow_calibration.R asserts) at a trial Priority 1 ceiling, and
# searches for the ceiling at which the rate meets the target by bracketing
# root search. The Priority 2 ceiling follows at a fixed ratio, the shipped
# pair's own by default, so the two are fitted jointly as one parameter.
#
# Under the accrual mechanism (`dow.accrual.enabled` = 1) with a post-operative
# hazard configured, the hazard rates are fitted too: a common scale on the
# three, holding their ratios, searched until the hazard causes the target
# share of the treated cohort's expected deaths (DOW_LATE_SHARE_TARGET,
# R/dow_calibration.R). The share moves with the ceilings and the rate with the
# hazard, so the two searches alternate for --rounds rounds, each starting from
# the other's last fit. --hold-legacy-rate sets the rate target to the one the
# configuration produces under the legacy mechanism at the same seeds and
# horizon, which is how the shipped pair was refitted when the accrual
# mechanism replaced it: held where the earlier model held it, beneath the
# Ajax Bay bound, rather than raised to it.
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
              help = "Scenario profile whose ceilings are fitted [default: %default]"),
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
              help = paste("Share of the treated cohort's expected deaths the post-operative",
                           "hazard should cause [default: %default]")),
  make_option("--rounds", type = "integer", default = 2L,
              help = "Alternations of the hazard and ceiling searches [default: %default]"),
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

if (opt$rounds < 1L) stop("--rounds must be at least 1, found ", opt$rounds, call. = FALSE)
if (opt$`late-share` <= 0 || opt$`late-share` >= 1) {
  stop("--late-share must lie in (0, 1), found ", opt$`late-share`, call. = FALSE)
}

resolved <- build_environment(resolve_scenario(json_data, opt$scenario))$vars$dow
shipped <- resolved$params
ratio <- if (!is.null(opt$ratio)) opt$ratio else shipped$p1_p_max / shipped$p2_p_max
lower <- if (!is.null(opt$lower)) opt$lower else 0.5 * shipped$p1_p_max
upper <- if (!is.null(opt$upper)) opt$upper else 2 * shipped$p1_p_max
if (!(lower > 0 && upper > lower)) {
  stop(sprintf("the bracket must satisfy 0 < lower < upper, found [%g, %g]", lower, upper),
       call. = FALSE)
}
hazard <- resolved$post_op_hazard
fit_hazard <- isTRUE(resolved$accrual$enabled == 1) &&
  any(c(hazard$p1_rate, hazard$p2_rate, hazard$p3_rate) > 0)

seeds <- DOW_CONTROL_SEEDS[seq_len(opt$measurements)]

#' Measure the configuration at one ceiling and hazard scale
#'
#' @param p1 Priority 1 ceiling to measure at.
#' @param scale Common scale on the post-operative hazard rates.
#' @param accrual 1 or 0 to override the mechanism, or NULL for the configured one.
#' @return Named numeric of `rate`, `ci_lower`, `ci_upper` and `share`.
evaluate <- function(p1, scale, accrual = NULL) {
  m <- measure_dow_response(json_data, opt$scenario, p1_p_max = p1, p1_over_p2 = ratio,
                            hazard_scale = scale, accrual = accrual,
                            n_reps = opt$reps, n_days = opt$days, seeds = seeds)
  s <- pooled_rate_summary(m$rates)
  share <- if (m$early + m$late > 0) m$late / (m$early + m$late) else 0
  cat(sprintf("  p1_p_max = %.5f, hazard scale %.3f: %.3f%%  95%% CI [%.3f%%, %.3f%%]; hazard share %.3f\n",
              p1, scale, 100 * s$mean, 100 * s$ci_lower, 100 * s$ci_upper, share))
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
  cat("\nMeasuring the legacy mechanism's rate at the configured ceilings\n")
  evaluate(shipped$p1_p_max, 1, accrual = 0)[["rate"]]
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
cat(sprintf("  bracket on p1_p_max: [%.5f, %.5f]; p1 over p2 held at %.4f\n", lower, upper, ratio))
if (fit_hazard) cat(sprintf("  hazard share target: %.3f\n", opt$`late-share`))

p1 <- shipped$p1_p_max
scale <- 1
traces <- list()
converged <- TRUE
for (round in seq_len(if (fit_hazard) opt$rounds else 1L)) {
  if (fit_hazard) {
    cat(sprintf("\nRound %d: hazard scale at p1_p_max = %.5f\n", round, p1))
    hfit <- fit_hazard_scale(function(s) evaluate(p1, s)[["share"]], opt$`late-share`,
                             lower = 0.25 * scale, upper = 4 * scale,
                             max_iter = opt$`max-iterations`)
    cat(sprintf("  stopped: %s; scale %.3f\n", hfit$reason, hfit$scale))
    converged <- converged && isTRUE(hfit$converged)
    scale <- hfit$scale
    traces[[length(traces) + 1L]] <- cbind(round = round, search = "hazard",
                                           setNames(hfit$trace, c("iteration", "value", "rate",
                                                                  "ci_lower", "ci_upper")))
  }
  cat(sprintf("\nRound %d: ceiling at hazard scale %.3f\n", round, scale))
  fit <- fit_dow_ceiling(function(p) evaluate(p, scale)[c("rate", "ci_lower", "ci_upper")],
                         target, lower, upper, tol_rate = opt$tolerance,
                         max_iter = opt$`max-iterations`)
  cat(sprintf("  stopped: %s; p1_p_max %.5f\n", fit$reason, fit$p1_p_max))
  converged <- converged && isTRUE(fit$converged)
  p1 <- fit$p1_p_max
  traces[[length(traces) + 1L]] <- cbind(round = round, search = "ceiling",
                                         setNames(fit$trace, c("iteration", "value", "rate",
                                                               "ci_lower", "ci_upper")))
}

cat("\n")
cat(sprintf("Fitted p1_p_max = %.4f, p2_p_max = %.4f\n", p1, p1 / ratio))
cat(sprintf("  achieved treated-cohort rate %.3f%% (95%% CI [%.3f%%, %.3f%%]) against %.3f%%\n",
            100 * fit$rate, 100 * fit$ci_lower, 100 * fit$ci_upper, 100 * target))
if (fit_hazard) {
  cat(sprintf("Fitted post-operative hazard per day: p1_rate = %.6f, p2_rate = %.6f, p3_rate = %.6f\n",
              scale * hazard$p1_rate, scale * hazard$p2_rate, scale * hazard$p3_rate))
}
cat(sprintf("  shipped for '%s': p1_p_max = %.4f, p2_p_max = %.4f\n", opt$scenario,
            shipped$p1_p_max, shipped$p2_p_max))
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
