#!/usr/bin/env Rscript
##############################################################################
## scripts/run_airlift_sweep.R                                              ##
## Strategic evacuation and Role 4 demand, replicated and swept             ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_airlift_sweep.R --refresh-baseline
#   Rscript scripts/run_airlift_sweep.R --iterations 10 --arms baseline
#   Rscript scripts/run_airlift_sweep.R --arms reliability,interval
#
# Why this exists. Every published finding about strategic aeromedical
# evacuation and the national support base came from one campaign, and the
# claim it rests on, that evacuation is bound by how many sorties depart rather
# than by how many places each carries, is the one most likely to be an artifact
# of the two cancellation draws that campaign happened to make.
# docs/Multi_Run_Supplement.md recorded the sweep behind it as the one
# experiment a reader could not re-execute from a tracked command, because it
# was run from a driver script. This is that command.
#
# Four arms are measured. The two baselines report the response set at the
# shipped configuration under each casualty intensity. The two sweeps vary the
# settings the single-campaign evidence pointed at: sortie reliability, and the
# interval between scheduled sorties. At the shipped reliability no backlog
# forms at all, so the sweep rather than the baseline is where the finding
# lives.
#
# --refresh-baseline is the only way to write the tracked data/airlift/ copies.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/airlift.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--arms", type = "character",
              default = "baseline,high,reliability,interval",
              help = "Comma-separated arms to run [default: %default]"),
  make_option("--iterations", type = "integer", default = AIRLIFT_REPLICATIONS,
              help = "Replications per arm [default: %default]"),
  make_option("--days", type = "integer", default = AIRLIFT_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--seed", type = "integer", default = 42L,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/airlift/ copies")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (opt$iterations < 1L) {
  stop("--iterations must be at least 1, found ", opt$iterations, call. = FALSE)
}
if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)

#' Directory the measurement is written to
OUTPUT_DIR <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "airlift")
} else {
  file.path("outputs", "data", "airlift")
}

arms <- trimws(strsplit(opt$arms, ",")[[1]])
known <- c("baseline", "high", "reliability", "interval")
unknown <- setdiff(arms, known)
if (length(unknown) > 0) {
  stop(sprintf("--arms: unknown arm(s) %s; known arms are %s",
               paste(unknown, collapse = ", "), paste(known, collapse = ", ")),
       call. = FALSE)
}

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

#' Measure one arm at the shipped configuration of a scenario profile
#'
#' @param scenario Scenario profile to run under.
#' @return The arm's per-replication responses, labelled as its own sweep of
#'   one value, so every arm has the same shape.
measure_baseline <- function(scenario) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
  apply_config_globals(resolve_scenario(json_data, scenario))
  shipped <- env_data$vars$role4$ame$failure_probability

  message(sprintf("Baseline '%s': %d replications x %d days", scenario,
                  opt$iterations, opt$days))
  set.seed(opt$seed)
  rows <- run_airlift_measurement(opt$iterations, opt$days, opt$`max-cores`)
  rows$setting <- "shipped"
  rows$value   <- shipped
  rows
}

results <- list()

if ("baseline" %in% arms) {
  results$baseline <- transform(measure_baseline("moderate_intensity"),
                                arm = "baseline", scenario = "moderate_intensity")
}
if ("high" %in% arms) {
  results$high <- transform(measure_baseline("high_intensity"),
                            arm = "high", scenario = "high_intensity")
}
if ("reliability" %in% arms) {
  rows <- run_airlift_sweep("failure_probability", AIRLIFT_FAILURE_PROBABILITIES,
                            scenario = "moderate_intensity",
                            n_iterations = opt$iterations, n_days = opt$days,
                            seed = opt$seed, max_cores = opt$`max-cores`)
  results$reliability <- transform(rows, arm = "reliability",
                                   scenario = "moderate_intensity")
}
if ("interval" %in% arms) {
  rows <- run_airlift_sweep("schedule_interval_days", AIRLIFT_SORTIE_INTERVALS,
                            scenario = "moderate_intensity",
                            n_iterations = opt$iterations, n_days = opt$days,
                            seed = opt$seed, max_cores = opt$`max-cores`)
  results$interval <- transform(rows, arm = "interval",
                                scenario = "moderate_intensity")
}

per_replication <- do.call(rbind, results)

#' Summarise every arm and value of the measurement
#'
#' @param rows Per-replication responses across every arm.
#' @return Data frame of arm, scenario, setting, value and the per-response
#'   mean and interval.
summarise_arms <- function(rows) {
  keys <- unique(rows[, c("arm", "scenario", "setting", "value")])
  do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
    k   <- keys[i, ]
    sub <- rows[rows$arm == k$arm & rows$setting == k$setting & rows$value == k$value, ]
    cbind(k, summarise_airlift(sub[, setdiff(names(sub),
                                             c("arm", "scenario", "setting", "value"))]),
          row.names = NULL)
  }))
}

summary_rows <- summarise_arms(per_replication)

write.csv(per_replication, file.path(OUTPUT_DIR, "airlift_replications.csv"),
          row.names = FALSE)
write.csv(summary_rows, file.path(OUTPUT_DIR, "airlift_summary.csv"), row.names = FALSE)
message(sprintf("Per-replication responses and summary written to %s", OUTPUT_DIR))

#' Print one response across every value of one arm
#'
#' @param arm Arm to print.
#' @param response Response to print.
#' @return Invisible NULL.
print_arm <- function(arm, response) {
  rows <- summary_rows[summary_rows$arm == arm & summary_rows$response == response, ]
  if (nrow(rows) == 0) return(invisible(NULL))
  rows <- rows[order(rows$value), ]
  cat(sprintf("%-12s %-22s %s\n", arm, response,
              paste(sprintf("%s=%.2f [%.2f, %.2f]", format(rows$value), rows$mean,
                            rows$ci_lower, rows$ci_upper), collapse = "  ")))
  invisible(NULL)
}

cat("\nResponses by arm:\n")
for (arm in unique(summary_rows$arm)) {
  for (response in c("boarded", "queued_at_end", "mean_wait_days", "p90_wait_days",
                     "sorties_flown", "cancellation_rate", "role4_peak",
                     "role4_peak_after_end", "hold_evac_bed_days", "hold_evac_share",
                     "ventilated_hold_hours")) {
    print_arm(arm, response)
  }
  cat("\n")
}
