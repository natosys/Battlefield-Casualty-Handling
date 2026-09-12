#!/usr/bin/env Rscript
##############################################################################
## scripts/run_long_horizon.R                                               ##
## Sustained-operations horizon: replicated run and per-block stability     ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_long_horizon.R --refresh-baseline
#   Rscript scripts/run_long_horizon.R --scenarios moderate_intensity
#   Rscript scripts/run_long_horizon.R --days 180 --iterations 10
#
# Why this exists. Every other experiment in this project runs for 30 days, a
# window inherited from the campaign the baseline models rather than chosen by
# measurement, and a window that short cannot tell a system in equilibrium from
# one thirty days into a divergence. This entry point runs the protocol
# documented in docs/Multi_Run_Supplement.md and writes two things: the daily
# series each replication was reduced to, and the per-block means the protocol's
# stability statement is made from.
#
# One long run answers the question for every shorter horizon as well. The model
# is causal, so the mean over a block cannot depend on what happens after it, and
# block one of a 360-day run covers exactly the window every 30-day experiment
# here measures. Running several separate durations would cost several times as
# much and say the same thing.
#
# --refresh-baseline is the only way to write the tracked data/long_horizon/
# copies, matching the contract every other measurement script carries.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/long_horizon.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--scenarios", type = "character",
              default = "moderate_intensity,high_intensity",
              help = "Comma-separated scenario profiles to run [default: %default]"),
  make_option("--days", type = "integer", default = LONG_HORIZON_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--iterations", type = "integer", default = LONG_HORIZON_REPLICATIONS,
              help = "Replications per profile [default: %default]"),
  make_option("--seed", type = "integer", default = 42L,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/long_horizon/ copies")
)

opt <- parse_args(OptionParser(option_list = option_list))

#' Directory the protocol's output is written to
#'
#' @details The tracked copy is reachable only under --refresh-baseline, so an
#'   exploratory run at a shorter horizon cannot displace the published series.
OUTPUT_DIR <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "long_horizon")
} else {
  file.path("outputs", "data", "long_horizon")
}

if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)
if (opt$iterations < 1L) {
  stop("--iterations must be at least 1, found ", opt$iterations, call. = FALSE)
}

scenario_names <- trimws(strsplit(opt$scenarios, ",")[[1]])

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

#' Run the protocol against one scenario profile and return its series
#'
#' @param scenario Name of the profile in env_data.json's scenarios block.
#' @return The profile's long-horizon series, carrying a scenario column.
#'
#' @details Sets the four configuration globals the execution model requires and
#'   restores them on exit, on the error path as well as the success path, so a
#'   failure part-way through a multi-profile run leaves the session as it found
#'   it rather than on whichever profile failed.
run_profile <- function(scenario) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
  apply_config_globals(resolve_scenario(json_data, scenario))

  message(sprintf("Scenario '%s': %d replications x %d days", scenario,
                  opt$iterations, opt$days))
  set.seed(opt$seed)
  series <- run_long_horizon(n_iterations = opt$iterations, n_days = opt$days,
                             max_cores = opt$`max-cores`)
  series$scenario <- scenario
  series
}

series <- do.call(rbind, lapply(scenario_names, run_profile))

blocks <- do.call(rbind, lapply(scenario_names, function(scenario) {
  b <- block_means(series[series$scenario == scenario, ])
  b$scenario <- scenario
  b
}))

write.csv(series, file.path(OUTPUT_DIR, "long_horizon_series.csv"), row.names = FALSE)
write.csv(blocks, file.path(OUTPUT_DIR, "long_horizon_blocks.csv"), row.names = FALSE)

message(sprintf("Series and block means written to %s", OUTPUT_DIR))

#' Print one response's block means as a single line per scenario
#'
#' @param series_name Name of the series to print.
#' @param subject_name Subject within that series to print.
#' @return Invisible NULL.
print_blocks <- function(series_name, subject_name) {
  for (scenario in scenario_names) {
    rows <- blocks[blocks$scenario == scenario & blocks$series == series_name &
                     blocks$subject == subject_name, ]
    if (nrow(rows) == 0) next
    cat(sprintf("%-19s %-22s %-24s %s\n", scenario, series_name, subject_name,
                paste(sprintf("%6.1f", rows$mean), collapse = " ")))
  }
  invisible(NULL)
}

cat("\nBlock means (one column per 30-day block):\n")
for (pool in names(LONG_HORIZON_POOLS)) print_blocks("mean_queue", pool)
for (pool in names(LONG_HORIZON_POOLS)) print_blocks("occupancy", pool)
for (s in c("arrivals", "dow", "evac_backlog")) print_blocks(s, "system")
