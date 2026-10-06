#!/usr/bin/env Rscript
##############################################################################
## scripts/run_role4_demand.R                                               ##
## National support base bed demand, replicated at both intensities         ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_role4_demand.R --refresh-baseline
#   Rscript scripts/run_role4_demand.R --iterations 4 --days 60
#
# Why this exists. docs/Results.md carried one Role 4 figure, the peak of the
# daily census, as a row of three other sections. A peak cannot separate a
# short spike from a plateau of the same height, and says nothing of what the
# census is made of or whether it has settled. This reduces each replication to
# the daily census by ward phase and origin and to the theatre demand owed with
# it (R/role4_demand.R), at each casualty intensity, and writes the series, the
# per-replication responses, their summary, the cross-replication daily mean
# and the stationarity classification. It reports demand only: the model gives
# the national support base no capacity, queue or shortfall.
#
# --refresh-baseline is the only way to write the tracked data/role4_demand/,
# and it runs the documented protocol (30 replications x 360 days x 2
# intensities at seed 42) rather than whatever arguments accompany it. Without
# it the run writes under outputs/ alone.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/airlift.R")
source("R/long_horizon.R")
source("R/role4_demand.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--arms", type = "character", default = "intensity,reliability",
              help = "Comma-separated arms to run [default: %default]"),
  make_option("--iterations", type = "integer", default = ROLE4_DEMAND_REPLICATIONS,
              help = "Replications per intensity [default: %default]"),
  make_option("--days", type = "integer", default = ROLE4_DEMAND_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--seed", type = "integer", default = ROLE4_DEMAND_SEED,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/role4_demand/ copy")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (isTRUE(opt$`refresh-baseline`)) {
  opt$iterations <- ROLE4_DEMAND_REPLICATIONS
  opt$days       <- ROLE4_DEMAND_DAYS
  opt$seed       <- ROLE4_DEMAND_SEED
  message(sprintf(paste("Baseline refresh: running the documented protocol,",
                        "%d replications x %d days x %d intensities at seed %d"),
                  ROLE4_DEMAND_REPLICATIONS, ROLE4_DEMAND_DAYS,
                  length(ROLE4_DEMAND_SCENARIOS), ROLE4_DEMAND_SEED))
}
arms <- trimws(strsplit(opt$arms, ",")[[1]])
known <- c("intensity", "reliability")
if (length(setdiff(arms, known)) > 0) {
  stop(sprintf("--arms: unknown arm(s) %s; known arms are %s",
               paste(setdiff(arms, known), collapse = ", "), paste(known, collapse = ", ")),
       call. = FALSE)
}
if (opt$iterations < 1L) {
  stop("--iterations must be at least 1, found ", opt$iterations, call. = FALSE)
}
if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)

#' Directory the measurement is written to
OUTPUT_DIR <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "role4_demand")
} else {
  file.path("outputs", "data", "role4_demand")
}
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Measure one casualty intensity and return its daily series
#'
#' @param scenario Scenario profile to run under.
#' @return The intensity's daily series, labelled with the scenario.
#'
#' @details The control seed is set once per intensity, from the caller, so the
#'   moderate arm draws the seeds the strategic airlift baseline drew and its
#'   census is the one that table's Role 4 peak was taken from.
measure_scenario <- function(scenario) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)
  apply_config_globals(resolve_scenario(json_data, scenario))

  message(sprintf("Scenario '%s': %d replications x %d days", scenario,
                  opt$iterations, opt$days))
  set.seed(opt$seed)
  rows <- run_role4_demand(opt$iterations, opt$days, opt$`max-cores`)
  rows$scenario <- scenario
  rows
}

if ("intensity" %in% arms) {
  series <- do.call(rbind, lapply(ROLE4_DEMAND_SCENARIOS, measure_scenario))
  responses <- role4_demand_responses(series, opt$days, ROLE4_DEMAND_WINDOW_DAYS)
  summary_rows <- summarise_role4_demand(responses)
  daily <- role4_census_daily(series)
  operations_weekly <- role4_operations_weekly(series)

  # Stationarity on the sustained-operations convention (R/long_horizon.R): block
  # means of the daily census, classified by the late slope. The census is a
  # function of cumulative evacuations, so whether it has settled is a finding
  # rather than an assumption.
  stability <- do.call(rbind, lapply(ROLE4_DEMAND_SCENARIOS, function(scenario) {
    d <- series[series$scenario == scenario & series$series == "census", ]
    cbind(scenario = scenario, classify_stability(block_means(d)), row.names = NULL)
  }))

  gz <- gzfile(file.path(OUTPUT_DIR, "role4_demand_series.csv.gz"), "w")
  write.csv(series, gz, row.names = FALSE)
  close(gz)
  write.csv(responses, file.path(OUTPUT_DIR, "role4_demand_replications.csv"), row.names = FALSE)
  write.csv(summary_rows, file.path(OUTPUT_DIR, "role4_demand_summary.csv"), row.names = FALSE)
  write.csv(daily, file.path(OUTPUT_DIR, "role4_demand_daily.csv"), row.names = FALSE)
  write.csv(operations_weekly, file.path(OUTPUT_DIR, "role4_demand_operations_weekly.csv"),
            row.names = FALSE)
  write.csv(stability, file.path(OUTPUT_DIR, "role4_demand_stability.csv"), row.names = FALSE)
}

#' Measure the census at one sortie cancellation probability
#'
#' @param probability Probability a scheduled sortie is cancelled.
#' @return The per-replication responses at that probability, labelled with it.
#'
#' @details Moderate intensity, on the arms the strategic airlift sweep ran, so
#'   each probability draws the seeds that sweep's point drew. Only the reduced
#'   responses are kept: the daily series of six probabilities would add six
#'   copies of a file whose purpose the intensity arm already serves.
measure_reliability <- function(probability) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)
  apply_airlift_setting(json_data, "moderate_intensity", "failure_probability", probability)

  message(sprintf("Sortie cancellation %.2f: %d replications x %d days", probability,
                  opt$iterations, opt$days))
  set.seed(opt$seed)
  rows <- run_role4_demand(opt$iterations, opt$days, opt$`max-cores`)
  cbind(failure_probability = probability,
        role4_demand_responses(rows, opt$days, ROLE4_DEMAND_WINDOW_DAYS), row.names = NULL)
}

if ("reliability" %in% arms) {
  reliability <- do.call(rbind, lapply(AIRLIFT_FAILURE_PROBABILITIES, measure_reliability))
  reliability_summary <- do.call(rbind, lapply(AIRLIFT_FAILURE_PROBABILITIES, function(p) {
    cbind(failure_probability = p,
          summarise_role4_demand(reliability[reliability$failure_probability == p, ]),
          row.names = NULL)
  }))
  write.csv(reliability, file.path(OUTPUT_DIR, "role4_demand_reliability_replications.csv"),
            row.names = FALSE)
  write.csv(reliability_summary, file.path(OUTPUT_DIR, "role4_demand_reliability_summary.csv"),
            row.names = FALSE)
}
message("Role 4 demand evidence set written to ", OUTPUT_DIR)

if ("intensity" %in% arms) {
  cat("\nCensus, mean [lower, upper] across replications:\n")
  for (scenario in ROLE4_DEMAND_SCENARIOS) {
    for (r in c("mean", "peak", "closing_mean")) {
      x <- summary_rows[summary_rows$scenario == scenario & summary_rows$series == "census" &
                          summary_rows$subject == ROLE4_TOTAL & summary_rows$response == r, ]
      cat(sprintf("%-20s %-13s %.1f [%.1f, %.1f]\n", scenario, r, x$mean, x$ci_lower, x$ci_upper))
    }
  }
}
