#!/usr/bin/env Rscript
##############################################################################
## scripts/run_airlift_places_sweep.R                                       ##
## Critical-care places per sortie, crossed with the sortie interval        ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_airlift_places_sweep.R --refresh-baseline
#   Rscript scripts/run_airlift_places_sweep.R --scenario high_intensity \
#     --refresh-baseline
#   Rscript scripts/run_airlift_places_sweep.R --iterations 2 --days 30
#
# Why this exists. The critical-care places are the binding strategic airlift
# constraint at both intensities, and every airlift sweep held the split of each
# sortie at the shipped 36 critical and 54 standard places. This script moves
# places between the two pools with the total held at the airframe's 90,
# crossed with a weekly and a three-day sortie interval (R/airlift_places_sweep.R,
# AIRLIFT_PLACES_ARMS), and reports each route's backlog, wait and empty places,
# whether each campaign's routes collapse, the R2E intensive care and holding
# load a waiting critical casualty holds, deaths of wounds among casualties
# awaiting airlift and in total, and the Role 4 peak.
#
# The arms are paired on one control seed each, and each finished replication
# is saved and each arm checkpointed as it completes, so an interruption costs
# the replication in flight and not the whole measurement.
#
# --scenario writes every file, checkpoints included, with `_<scenario>`
# appended to its stem (scenario_output_path() in R/scenario.R), so a
# high-intensity run cannot overwrite the default set.
#
# --refresh-baseline is the only way to write the tracked
# data/airlift_places/ copy, and it runs the protocol R/airlift_places_sweep.R
# holds rather than whatever arguments accompany it, so the tracked set and the
# design docs/Methods.md documents cannot diverge through a mistyped argument.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/policy_sweep.R")
source("R/hold_window.R")
source("R/long_horizon.R")
source("R/airlift.R")
source("R/airlift_places_sweep.R")
source("R/cli.R")
source("R/run_manifest.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--iterations", type = "integer", default = AIRLIFT_PLACES_REPLICATIONS,
              help = "Replications per arm [default: %default]"),
  make_option("--days", type = "integer", default = AIRLIFT_PLACES_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--window", type = "integer", default = AIRLIFT_PLACES_WINDOW_DAYS,
              help = "Closing window the queue is measured over [default: %default]"),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = AIRLIFT_PLACES_SEED,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--config", type = "character", default = NULL,
              help = paste("Configuration file to run against, the shipped env_data.json",
                           "or one saved from the Shiny console. Rejected under",
                           "--refresh-baseline unless it is the shipped file",
                           "[default: env_data.json]")),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/airlift_places/ copy")
)

opt <- parse_args(do.call(OptionParser, c(list(option_list = option_list),
                                          cli_help("scripts/run_airlift_places_sweep.R"))))
refresh_requested <- isTRUE(opt$`refresh-baseline`)
config_path <- resolve_config_path(opt$config, refresh_baseline = refresh_requested)

# A baseline refresh runs the documented protocol rather than whatever the
# caller passed, on the convention scripts/run_scenarios.R establishes.
if (isTRUE(opt$`refresh-baseline`)) {
  opt$iterations <- AIRLIFT_PLACES_REPLICATIONS
  opt$days       <- AIRLIFT_PLACES_DAYS
  opt$window     <- AIRLIFT_PLACES_WINDOW_DAYS
  opt$seed       <- AIRLIFT_PLACES_SEED
  message("Baseline refresh: running the documented protocol, ",
          sprintf("%d replications x %d days x %d arms at seed %d",
                  AIRLIFT_PLACES_REPLICATIONS, AIRLIFT_PLACES_DAYS,
                  nrow(AIRLIFT_PLACES_ARMS), AIRLIFT_PLACES_SEED))
}

if (opt$iterations < 1L) {
  stop("--iterations must be at least 1, found ", opt$iterations, call. = FALSE)
}
if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)
if (opt$window < 1L || opt$window > opt$days) {
  stop(sprintf("--window must lie between 1 and --days (%d), found %d", opt$days,
               opt$window), call. = FALSE)
}

#' Directory the measurement is written to
OUTPUT_DIR <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "airlift_places")
} else {
  file.path("outputs", "data", "airlift_places")
}

# A run outside the tracked evidence set records what it ran (R/run_manifest.R).
if (!refresh_requested) write_run_manifest(OUTPUT_DIR, config_path, opt)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

#' Directory each arm's checkpoint, and each replication within it, is resumed from
CHECKPOINT_DIR <- checkpoint_dir("airlift_places", refresh_requested, config_path)

json_data <- read_config(config_path)

#' File stem of one arm's checkpoint
#'
#' @param i Row of AIRLIFT_PLACES_ARMS.
#' @return The stem, naming the critical places and the interval.
arm_stem <- function(i) {
  sprintf("airlift_places_arm_c%d_i%d", AIRLIFT_PLACES_ARMS$critical[i],
          AIRLIFT_PLACES_ARMS$interval[i])
}

#' Measure one arm and return its responses and daily series
#'
#' @param i Row of AIRLIFT_PLACES_ARMS.
#' @return A list of the arm's `rows` and `series`, each carrying the arm label.
#'
#' @details The configuration globals are restored on exit, on the error path as
#'   well as the success path. The control seed is set once per arm, from the
#'   caller, which is what pairs the arms.
measure_arm <- function(i) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  arm <- AIRLIFT_PLACES_ARMS[i, ]
  apply_airlift_places_setting(json_data, opt$scenario, arm$critical, arm$standard,
                               arm$interval)

  message(sprintf("%s: %d replications x %d days", arm$arm, opt$iterations, opt$days))
  set.seed(opt$seed)
  ckpt <- sub("\\.csv$", "_replications",
              scenario_output_path(CHECKPOINT_DIR, arm_stem(i), opt$scenario))
  dir.create(ckpt, recursive = TRUE, showWarnings = FALSE)
  out <- run_airlift_places_measurement(n_iterations = opt$iterations, n_days = opt$days,
                                        window_days = opt$window,
                                        max_cores = opt$`max-cores`, checkpoint_dir = ckpt)
  out$rows$arm <- arm$arm
  out$series$arm <- arm$arm
  out
}

#' Measure one arm, or read it back where it has already been measured
#'
#' @param i Row of AIRLIFT_PLACES_ARMS.
#' @return A list of the arm's `rows` and `series`.
#'
#' @details Each arm is written as it completes and read back rather than re-run
#'   on a later invocation. Delete the arm's files under CHECKPOINT_DIR to force a
#'   fresh measurement.
measure_or_resume <- function(i) {
  rows_path <- scenario_output_path(CHECKPOINT_DIR, arm_stem(i), opt$scenario)
  series_path <- sub("\\.csv$", "_series.csv.gz", rows_path)
  label <- AIRLIFT_PLACES_ARMS$arm[i]
  if (file.exists(rows_path) && file.exists(series_path)) {
    rows <- read.csv(rows_path, stringsAsFactors = FALSE)
    if (nrow(rows) == opt$iterations) {
      message(sprintf("%s: resumed %d replications from %s", label, nrow(rows), rows_path))
      return(list(rows = rows, series = read.csv(series_path, stringsAsFactors = FALSE)))
    }
    message(sprintf("%s: discarding %d of %d checkpointed", label, nrow(rows),
                    opt$iterations))
  }
  out <- measure_arm(i)
  write.csv(out$rows, rows_path, row.names = FALSE)
  gz <- gzfile(series_path, "w")
  write.csv(out$series, gz, row.names = FALSE)
  close(gz)
  out
}

measured <- lapply(seq_len(nrow(AIRLIFT_PLACES_ARMS)), measure_or_resume)

# Each arm's daily series is part of the evidence set, the queue classification
# being recomputed from it by the protocol check; the responses are not written
# per arm, the replications file carrying every arm's.
for (i in seq_along(measured)) {
  gz <- gzfile(sub("\\.csv$", "_series.csv.gz",
                   scenario_output_path(OUTPUT_DIR, arm_stem(i), opt$scenario)), "w")
  write.csv(measured[[i]]$series, gz, row.names = FALSE)
  close(gz)
}
per_replication <- do.call(rbind, lapply(measured, `[[`, "rows"))
series <- do.call(rbind, lapply(measured, `[[`, "series"))

summary_rows <- do.call(rbind, lapply(AIRLIFT_PLACES_ARMS$arm, function(a) {
  cbind(data.frame(arm = a),
        summarise_policy(per_replication[per_replication$arm == a, ], arm_column = "arm"))
}))

others <- setdiff(AIRLIFT_PLACES_ARMS$arm, AIRLIFT_PLACES_BASELINE_ARM)
paired <- do.call(rbind, lapply(others, function(a) {
  do.call(rbind, lapply(AIRLIFT_PLACES_PAIRED_RESPONSES, function(response) {
    row <- policy_paired_difference(per_replication, response, AIRLIFT_PLACES_BASELINE_ARM, a,
                                    arm_column = "arm")
    row$reps_needed <- policy_replications_for(
      per_replication, response, AIRLIFT_PLACES_BASELINE_ARM, a,
      AIRLIFT_PLACES_HALF_WIDTHS[[response]], arm_column = "arm"
    )
    row
  }))
}))

#' Fewest 30-day blocks the late-half fit of the collapse rule can classify
COLLAPSE_MIN_BLOCKS <- 6L

classifiable <- opt$days >= COLLAPSE_MIN_BLOCKS * LONG_HORIZON_BLOCK_DAYS
if (!classifiable) {
  message(sprintf("A campaign of %d days is too short to classify; no collapse written",
                  opt$days))
}
collapse <- if (classifiable) airlift_places_collapse(series, opt$days) else NULL
collapse_summary <- if (classifiable) {
  do.call(rbind, lapply(AIRLIFT_PLACES_ARMS$arm, function(a) {
    do.call(rbind, lapply(names(AIRLIFT_COLLAPSE_ROUTES), function(route) {
      cbind(data.frame(arm = a, route = route, stringsAsFactors = FALSE),
            summarise_collapse(collapse[collapse$arm == a & collapse$route == route, ]))
    }))
  }))
}

write.csv(per_replication,
          scenario_output_path(OUTPUT_DIR, "airlift_places_replications", opt$scenario),
          row.names = FALSE)
write.csv(summary_rows,
          scenario_output_path(OUTPUT_DIR, "airlift_places_summary", opt$scenario),
          row.names = FALSE)
write.csv(holm_adjust(paired),
          scenario_output_path(OUTPUT_DIR, "airlift_places_paired", opt$scenario),
          row.names = FALSE)
if (classifiable) {
  write.csv(collapse,
            scenario_output_path(OUTPUT_DIR, "airlift_places_collapse_replications",
                                 opt$scenario),
            row.names = FALSE)
  write.csv(collapse_summary,
            scenario_output_path(OUTPUT_DIR, "airlift_places_collapse", opt$scenario),
            row.names = FALSE)
}
message(sprintf(paste("Airlift places sweep responses, summary, paired differences and",
                      "collapse classification written to %s"), OUTPUT_DIR))
