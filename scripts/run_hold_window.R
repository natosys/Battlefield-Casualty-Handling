#!/usr/bin/env Rscript
##############################################################################
## scripts/run_hold_window.R                                                ##
## The R2B pre-open hold window, at its two arms                            ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_hold_window.R --refresh-baseline
#   Rscript scripts/run_hold_window.R --iterations 4 --days 10
#
# Why this exists. Option 3 of docs/Multi_Run_Analysis.md prints eight paired
# differences comparing the shipped 60-minute R2B pre-open window against a
# window of zero, and until this script existed the comparison had no driver:
# it was invoked as run.R under a parameter override, so there was no command
# to give a --refresh-baseline flag to and no tracked evidence set behind the
# printed figures (Issue #387). This is that driver, on the arrangement
# scripts/run_policy_sweep.R establishes for a paired two-arm comparison.
#
# Both arms run under one control seed, so each draws the same per-replication
# seeds from R's PRNG, though not the same casualty stream: R/hold_window.R's
# header and docs/Multi_Run_Supplement.md's "The R2B Pre-Open Hold Window"
# record why introducing the hold shifts the stream from the first hold
# onward, and why the paired difference is reported anyway as the more precise
# of the two comparisons available.
#
# --refresh-baseline is the only way to write the tracked data/hold_window/,
# and it runs the documented protocol (50 replications x 30 days x 2 arms at
# seed 42) rather than whatever arguments accompany it, so the tracked set and
# the design docs/Multi_Run_Supplement.md documents cannot diverge through a
# mistyped argument. Without it the run writes under outputs/ alone.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/hold_window.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--iterations", type = "integer", default = HOLD_WINDOW_REPLICATIONS,
              help = "Replications per arm [default: %default]"),
  make_option("--days", type = "integer", default = HOLD_WINDOW_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = HOLD_WINDOW_SEED,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/hold_window/ copy")
)

opt <- parse_args(OptionParser(option_list = option_list))

# A baseline refresh runs the documented protocol rather than whatever the
# caller passed, on the convention scripts/run_scenarios.R establishes.
if (isTRUE(opt$`refresh-baseline`)) {
  opt$iterations <- HOLD_WINDOW_REPLICATIONS
  opt$days       <- HOLD_WINDOW_DAYS
  opt$seed       <- HOLD_WINDOW_SEED
  message("Baseline refresh: running the documented protocol, ",
          sprintf("%d replications x %d days x %d arms at seed %d",
                  HOLD_WINDOW_REPLICATIONS, HOLD_WINDOW_DAYS,
                  length(HOLD_WINDOW_ARMS), HOLD_WINDOW_SEED))
}

if (opt$iterations < 1L) {
  stop("--iterations must be at least 1, found ", opt$iterations, call. = FALSE)
}
if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)

#' Directory the measurement is written to
OUTPUT_DIR <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "hold_window")
} else {
  file.path("outputs", "data", "hold_window")
}
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Measure one arm and return its per-replication responses
#'
#' @param window_min Pre-open window to run at, in minutes.
#' @return The arm's response rows.
#'
#' @details The configuration globals are restored on exit, on the error path
#'   as well as the success path, so an arm that fails part-way leaves the
#'   session as it found it. The control seed is set once per arm, from the
#'   caller rather than inside R/hold_window.R, which is what pairs the arms.
measure_arm <- function(window_min) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  apply_hold_window_setting(json_data, opt$scenario, window_min)

  message(sprintf("Pre-open window %d min: %d replications x %d days",
                  window_min, opt$iterations, opt$days))
  set.seed(opt$seed)
  rows <- run_hold_window_measurement(window_min, n_iterations = opt$iterations,
                                      n_days = opt$days, max_cores = opt$`max-cores`)
  rows
}

per_replication <- do.call(rbind, lapply(HOLD_WINDOW_ARMS, measure_arm))

summary_rows <- do.call(rbind, lapply(HOLD_WINDOW_ARMS, function(window_min) {
  arm <- per_replication[per_replication$window_min == window_min, ]
  cbind(data.frame(window_min = window_min), summarise_hold_window(arm))
}))

#' Half-width each paired response is sized against, in its own units
#'
#' @details Chosen as roughly two casualties or two operations, the scale the
#'   companion paper's replication-count statement is made at.
PAIRED_HALF_WIDTHS <- c(held_r2b = 1, r2b_surgeries = 2, diverted_offshift = 2,
                        diverted_busy = 2, r2e_first_surgeries = 2,
                        r2e_theatre_deferred = 1, total_dow = 0.5, total_casualties = 5)

paired <- do.call(rbind, lapply(HOLD_WINDOW_RESPONSES, function(response) {
  row <- hold_window_paired_difference(per_replication, response,
                                       HOLD_WINDOW_ARMS[1], HOLD_WINDOW_ARMS[2])
  half_width <- PAIRED_HALF_WIDTHS[[response]]
  row$reps_needed <- hold_window_replications_for(per_replication, response,
                                                  HOLD_WINDOW_ARMS[1], HOLD_WINDOW_ARMS[2],
                                                  half_width)
  row
}))

write.csv(per_replication, file.path(OUTPUT_DIR, "hold_window_replications.csv"),
          row.names = FALSE)
write.csv(summary_rows, file.path(OUTPUT_DIR, "hold_window_summary.csv"),
          row.names = FALSE)
write.csv(paired, file.path(OUTPUT_DIR, "hold_window_paired.csv"), row.names = FALSE)
message(sprintf("Hold window responses, summary and paired differences written to %s",
                OUTPUT_DIR))

#' Print one response's arm means and paired difference as a markdown table row
#'
#' @param response Name of the response to print.
#' @param label Column label for the response.
#' @return Invisible NULL.
print_row <- function(response, label) {
  cells <- vapply(HOLD_WINDOW_ARMS, function(window_min) {
    r <- summary_rows[summary_rows$window_min == window_min &
                        summary_rows$response == response, ]
    if (nrow(r) != 1 || is.na(r$mean)) return("n/a")
    sprintf("%.2f", r$mean)
  }, character(1))
  diff <- paired[paired$response == response, ]
  diff_cell <- if (nrow(diff) != 1 || is.na(diff$difference)) {
    "n/a"
  } else {
    sprintf("%+.2f [%+.2f, %+.2f]", diff$difference, diff$ci_lower, diff$ci_upper)
  }
  cat(sprintf("| %s | %s | %s |\n", label, paste(cells, collapse = " | "), diff_cell))
  invisible(NULL)
}

cat("\n| Measure | Window 0 | Window 60 min | Difference |\n")
cat("| --- | --- | --- | --- |\n")
print_row("held_r2b", "Casualties held at R2B")
print_row("r2b_surgeries", "R2B surgeries")
print_row("diverted_offshift", "Diverted, team off shift")
print_row("diverted_busy", "Diverted, theatre busy")
print_row("r2e_first_surgeries", "R2E first surgeries")
print_row("r2e_theatre_deferred", "R2E theatre entry deferred")
print_row("total_dow", "Died of wounds per run")
print_row("total_casualties", "Total casualties")
