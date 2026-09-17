#!/usr/bin/env Rscript
##############################################################################
## scripts/run_saturation_sweep.R                                           ##
## The forward surgical saturation release sweep, at the sustained horizon   ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_saturation_sweep.R --refresh-baseline
#   Rscript scripts/run_saturation_sweep.R --iterations 4 --days 90 --window 30
#   Rscript scripts/run_saturation_sweep.R --thresholds 0,2,5
#
# Why this exists. `r2eheavy.second_surgery.saturation_queue_threshold` decides
# when a damage control casualty is flown out with the definitive repair still
# outstanding, and it shipped at zero because the work that casualty carried
# rearward was not yet reported. It is now, so the threshold can be set from
# evidence rather than disabled, and it has none: no doctrinal source names a
# queue length at which a deployed hospital stops holding cases for a second
# operation. This script measures the trade the threshold consists of, so that
# the shipped value is chosen from a curve rather than assumed.
#
# The trade has a forward side and a rearward one. Releasing a casualty frees a
# theatre slot and a post-operative intensive care bed forward, and costs an
# operation at the national support base and a casualty who arrives there
# unrepaired. A threshold above the queue a campaign actually reaches changes
# nothing, which is a measurement rather than an assumption: the range runs past
# it deliberately so that the inert end is visible.
#
# The arms are paired, as the policy sweep's are. One control seed is set per
# arm, so replication k of every arm runs the same parent stream and the
# difference against the disabled arm is measured within replication.
#
# Each arm is checkpointed as it completes and resumed rather than re-run, so an
# interruption costs the arm in flight rather than all 210 replication-years.
#
# --refresh-baseline is the only way to write the tracked data/policy/ copy.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/policy_sweep.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--thresholds", type = "character",
              default = paste(POLICY_SATURATION_THRESHOLDS, collapse = ","),
              help = "Comma-separated saturation thresholds [default: %default]"),
  make_option("--iterations", type = "integer", default = POLICY_REPLICATIONS,
              help = "Replications per threshold [default: %default]"),
  make_option("--days", type = "integer", default = POLICY_DAYS_HORIZON,
              help = "Campaign length in days [default: %default]"),
  make_option("--window", type = "integer", default = POLICY_WINDOW_DAYS,
              help = "Closing window the stability responses use [default: %default]"),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = 42L,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/policy/ copy")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (opt$iterations < 1L) {
  stop("--iterations must be at least 1, found ", opt$iterations, call. = FALSE)
}
if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)
if (opt$window < 1L || opt$window > opt$days) {
  stop(sprintf("--window must lie between 1 and --days (%d), found %d", opt$days,
               opt$window), call. = FALSE)
}

thresholds <- as.integer(trimws(strsplit(opt$thresholds, ",")[[1]]))
if (any(is.na(thresholds)) || any(thresholds < 0L)) {
  stop("--thresholds must be whole non-negative queue lengths, found '",
       opt$thresholds, "'", call. = FALSE)
}

#' Directory the measurement is written to
OUTPUT_DIR <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "policy")
} else {
  file.path("outputs", "data", "policy")
}

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' The evacuation policy every arm runs under, in days
#'
#' @details The shipped policy, read from the file rather than restated, so this
#'   sweep varies one parameter and a later change to the policy cannot leave
#'   the threshold chosen under a configuration the model no longer ships.
SHIPPED_POLICY_DAYS <- local({
  described <- build_environment(resolve_scenario(json_data, opt$scenario))
  described$vars$r2eheavy$recovery$evacuation_policy_days
})

#' Measure one saturation threshold and return its per-replication responses
#'
#' @param threshold Saturation threshold to run at, in queued casualties.
#' @return The arm's response rows, carrying the threshold.
#'
#' @details The configuration globals are restored on exit, on the error path as
#'   well as the success path, so an arm that fails part-way leaves the session
#'   as it found it. The control seed is set once per arm, which is what pairs
#'   the arms.
measure_arm <- function(threshold) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  apply_policy_setting(json_data, opt$scenario, saturation_threshold = threshold)

  message(sprintf("Saturation threshold %d: %d replications x %d days",
                  threshold, opt$iterations, opt$days))
  set.seed(opt$seed)
  rows <- run_policy_measurement(SHIPPED_POLICY_DAYS, n_iterations = opt$iterations,
                                 n_days = opt$days, window_days = opt$window,
                                 max_cores = opt$`max-cores`)
  rows$saturation_threshold <- threshold
  rows$policy_days <- NULL
  rows
}

#' Path one arm's checkpointed responses are written to and resumed from
#'
#' @param threshold Saturation threshold the arm ran at.
#' @return The file path for that arm.
arm_path <- function(threshold) {
  file.path(OUTPUT_DIR, sprintf("saturation_sweep_arm_%dq.csv", threshold))
}

#' Measure one arm, or read it back where it has already been measured
#'
#' @param threshold Saturation threshold to run at, in queued casualties.
#' @return The arm's response rows.
#'
#' @details Each arm is written as it completes and read back rather than re-run
#'   on a later invocation, so an environment that reclaims its filesystem or a
#'   host that stops the process costs the arm in flight rather than the whole
#'   measurement, on the arrangement scripts/run_policy_sweep.R uses. Delete the
#'   arm files to force a fresh measurement.
measure_or_resume <- function(threshold) {
  path <- arm_path(threshold)
  label <- sprintf("Saturation threshold %d", threshold)
  if (file.exists(path)) {
    rows <- read.csv(path, stringsAsFactors = FALSE)
    if (nrow(rows) == opt$iterations) {
      message(sprintf("%s: resumed %d replications from %s", label, nrow(rows), path))
      return(rows)
    }
    message(sprintf("%s: discarding %d of %d checkpointed", label, nrow(rows),
                    opt$iterations))
  }
  rows <- measure_arm(threshold)
  write.csv(rows, path, row.names = FALSE)
  rows
}

per_replication <- do.call(rbind, lapply(thresholds, measure_or_resume))

summary_rows <- do.call(rbind, lapply(thresholds, function(threshold) {
  arm <- per_replication[per_replication$saturation_threshold == threshold, ]
  cbind(data.frame(saturation_threshold = threshold),
        summarise_policy(arm, arm_column = "saturation_threshold"))
}))

#' Responses the paired comparison against the disabled arm is reported for
#'
#' @details The forward side of the trade, the rearward side, and the two health
#'   outcomes a planner would refuse to trade either for.
PAIRED_RESPONSES <- c("theatre_mean_queue", "icu_occupancy",
                      "post_definitive_icu_share", "released_unrepaired",
                      "role4_operations", "role4_peak", "total_dow", "total_rtd")

#' Half-width each paired response is sized against, in its own units
#'
#' @details The smallest difference that would change a planning decision:
#'   one casualty on each count, five beds at the national support base, and one
#'   percentage point on each of the two shares.
PAIRED_HALF_WIDTHS <- c(theatre_mean_queue = 1, icu_occupancy = 0.01,
                        post_definitive_icu_share = 0.01,
                        released_unrepaired = 1, role4_operations = 1,
                        role4_peak = 5, total_dow = 1, total_rtd = 10)

#' Threshold the paired differences are measured against
#'
#' @details Zero, which disables the release, so every difference is what the
#'   lever buys and costs against the model without it.
BASELINE_THRESHOLD <- 0L

paired <- NULL
if (BASELINE_THRESHOLD %in% thresholds && length(thresholds) > 1) {
  others <- setdiff(thresholds, BASELINE_THRESHOLD)
  paired <- do.call(rbind, lapply(others, function(threshold) {
    do.call(rbind, lapply(PAIRED_RESPONSES, function(response) {
      row <- policy_paired_difference(per_replication, response,
                                      BASELINE_THRESHOLD, threshold,
                                      arm_column = "saturation_threshold")
      row$reps_needed <- policy_replications_for(per_replication, response,
                                                 BASELINE_THRESHOLD, threshold,
                                                 PAIRED_HALF_WIDTHS[[response]],
                                                 arm_column = "saturation_threshold")
      row
    }))
  }))
}

write.csv(per_replication,
          file.path(OUTPUT_DIR, "saturation_sweep_replications.csv"),
          row.names = FALSE)
write.csv(summary_rows, file.path(OUTPUT_DIR, "saturation_sweep.csv"),
          row.names = FALSE)
if (!is.null(paired)) {
  write.csv(paired, file.path(OUTPUT_DIR, "saturation_sweep_paired.csv"),
            row.names = FALSE)
}
message(sprintf("Saturation sweep responses, summary and paired differences written to %s",
                OUTPUT_DIR))

#' Print one response's swept means as a markdown table row
#'
#' @param response Name of the response to print.
#' @param label Column label for the response.
#' @param scale Multiplier applied before printing, 100 for a share.
#' @param digits Decimal places to print.
#' @return Invisible NULL.
print_row <- function(response, label, scale = 1, digits = 2) {
  cells <- vapply(thresholds, function(threshold) {
    r <- summary_rows[summary_rows$saturation_threshold == threshold &
                        summary_rows$response == response, ]
    if (nrow(r) != 1 || is.na(r$mean)) return("n/a")
    sprintf("%.*f [%.*f, %.*f]", digits, scale * r$mean, digits,
            scale * r$ci_lower, digits, scale * r$ci_upper)
  }, character(1))
  cat(sprintf("| %s | %s |\n", label, paste(cells, collapse = " | ")))
  invisible(NULL)
}

cat("\n| Response |", paste(sprintf("%d queued", thresholds), collapse = " | "), "|\n")
cat("|---", strrep("|---", length(thresholds)), "|\n", sep = "")
print_row("theatre_mean_queue", "R2E theatre mean queue", 1, 2)
print_row("icu_occupancy", "R2E ICU occupancy (%)", 100, 1)
print_row("icu_mean_queue", "R2E ICU mean queue", 1, 2)
print_row("hold_occupancy", "R2E hold occupancy (%)", 100, 1)
print_row("post_definitive_icu_share", "Post-definitive ICU access (%)", 100, 1)
print_row("released_unrepaired", "Released unrepaired", 1, 1)
print_row("role4_operations", "Role 4 operations", 1, 1)
print_row("role4_theatre_minutes", "Role 4 theatre minutes", 1, 0)
print_row("role4_peak", "Role 4 peak beds", 1, 1)
print_row("total_rtd", "Returns to duty", 1, 1)
print_row("total_dow", "Died of wounds", 1, 2)

if (!is.null(paired)) {
  cat("\nPaired differences against the disabled release:\n\n")
  cat("| Response | At | Difference | 95% CI | p | Reps for half-width |\n")
  cat("|---|---|---|---|---|---|\n")
  for (i in seq_len(nrow(paired))) {
    r <- paired[i, ]
    cat(sprintf("| %s | %d | %+.2f | [%+.2f, %+.2f] | %.3f | %s |\n",
                r$response, r$to, r$difference, r$ci_lower, r$ci_upper, r$p_value,
                if (is.na(r$reps_needed)) "n/a" else format(r$reps_needed)))
  }
}
