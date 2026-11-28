#!/usr/bin/env Rscript
##############################################################################
## scripts/run_okinawa_flow.R                                               ##
## High-intensity died of wounds under Okinawa's flow and capacity          ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_okinawa_flow.R --refresh-baseline
#   Rscript scripts/run_okinawa_flow.R --iterations 2 --days 30 \
#     --anchor-reps 2 --anchor-days 10
#
# Why this exists. The high-intensity profile is checked against Okinawa's 3.4%
# hospital died-of-wounds rate only as an upper bound at 82 days, but Okinawa
# evacuated continuously and fielded about four times the model's beds per daily
# battle casualty, so the comparison mixes load, evacuation tempo and capacity.
# This script measures the profile's treated-cohort rate as each is brought
# towards Okinawa's in turn (R/okinawa_flow.R holds the arms): daily sorties,
# beds at Okinawa's ratio, surgery doubled, single-stage surgery, surgery at the
# estimated Okinawa ratio, and every establishment scaled to the
# congestion-free floor.
#
# Every arm is measured twice: under the sustained protocol (30 replications
# over 360 days at one control seed, paired against the shipped arm, with the
# R2E pools classified converged or drifting) and under the anchor protocol
# (the calibration check's pooled rate over three control seeds of 50
# replications at Okinawa's 82 days). Each replication is checkpointed as it
# completes and resumed rather than re-run, so an interruption costs the
# replications in flight and not the measurement.
#
# --refresh-baseline is the only way to write the tracked data/okinawa_flow/
# copy, and it runs the protocol R/okinawa_flow.R holds rather than whatever
# arguments accompany it, so the tracked set and the design docs/Methods.md
# documents cannot diverge through a mistyped argument.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/policy_sweep.R")
source("R/hold_window.R")
source("R/long_horizon.R")
source("R/airlift.R")
source("R/surgical_team_sweep.R")
source("R/icu_bed_sweep.R")
source("R/triage_sweep.R")
source("R/dow_calibration.R")
source("R/okinawa_flow.R")
source("R/cli.R")
source("R/run_manifest.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--iterations", type = "integer", default = OKINAWA_FLOW_REPLICATIONS,
              help = "Replications per arm, sustained protocol [default: %default]"),
  make_option("--days", type = "integer", default = OKINAWA_FLOW_DAYS,
              help = "Campaign length in days, sustained protocol [default: %default]"),
  make_option("--window", type = "integer", default = OKINAWA_FLOW_WINDOW_DAYS,
              help = "Closing window the queue is measured over [default: %default]"),
  make_option("--seed", type = "integer", default = OKINAWA_FLOW_SEED,
              help = "Control seed, sustained protocol [default: %default]"),
  make_option("--anchor-reps", type = "integer", default = OKINAWA_FLOW_ANCHOR_REPLICATIONS,
              help = "Replications per control seed, anchor protocol [default: %default]"),
  make_option("--anchor-days", type = "integer", default = OKINAWA_FLOW_ANCHOR_DAYS,
              help = "Campaign length in days, anchor protocol [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--config", type = "character", default = NULL,
              help = paste("Configuration file to run against, the shipped env_data.json",
                           "or one saved from the Shiny console. Rejected under",
                           "--refresh-baseline unless it is the shipped file",
                           "[default: env_data.json]")),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/okinawa_flow/ copy")
)

opt <- parse_args(do.call(OptionParser, c(list(option_list = option_list),
                                          cli_help("scripts/run_okinawa_flow.R"))))
refresh_requested <- isTRUE(opt$`refresh-baseline`)
config_path <- resolve_config_path(opt$config, refresh_baseline = refresh_requested)

# A baseline refresh runs the documented protocol rather than whatever the
# caller passed, on the convention scripts/run_scenarios.R establishes.
if (refresh_requested) {
  opt$iterations    <- OKINAWA_FLOW_REPLICATIONS
  opt$days          <- OKINAWA_FLOW_DAYS
  opt$window        <- OKINAWA_FLOW_WINDOW_DAYS
  opt$seed          <- OKINAWA_FLOW_SEED
  opt$`anchor-reps` <- OKINAWA_FLOW_ANCHOR_REPLICATIONS
  opt$`anchor-days` <- OKINAWA_FLOW_ANCHOR_DAYS
  message("Baseline refresh: running the documented protocol, ",
          sprintf("%d arms x (%d replications x %d days at seed %d, and %d seeds x %d x %d days)",
                  nrow(OKINAWA_FLOW_ARMS), OKINAWA_FLOW_REPLICATIONS, OKINAWA_FLOW_DAYS,
                  OKINAWA_FLOW_SEED, length(OKINAWA_FLOW_ANCHOR_SEEDS),
                  OKINAWA_FLOW_ANCHOR_REPLICATIONS, OKINAWA_FLOW_ANCHOR_DAYS))
}

for (field in c("iterations", "days", "anchor-reps", "anchor-days")) {
  if (opt[[field]] < 1L) {
    stop(sprintf("--%s must be at least 1, found %d", field, opt[[field]]), call. = FALSE)
  }
}
if (opt$window < 1L || opt$window > opt$days) {
  stop(sprintf("--window must lie between 1 and --days (%d), found %d", opt$days,
               opt$window), call. = FALSE)
}

#' Directory the measurement is written to
OUTPUT_DIR <- if (refresh_requested) {
  file.path("data", "okinawa_flow")
} else {
  file.path("outputs", "data", "okinawa_flow")
}

# A run outside the tracked evidence set records what it ran (R/run_manifest.R).
if (!refresh_requested) write_run_manifest(OUTPUT_DIR, config_path, opt)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

#' Directory each arm's checkpoint, and each replication within it, is resumed from
CHECKPOINT_DIR <- checkpoint_dir("okinawa_flow", refresh_requested, config_path)

json_data <- read_config(config_path)

#' Run a measurement with an arm applied and the configuration globals restored
#'
#' @param i Row of OKINAWA_FLOW_ARMS.
#' @param measure Zero-argument function taking the measurement.
#' @return Whatever `measure` returns.
#'
#' @details The globals are restored on exit, on the error path as well as the
#'   success path, so one arm's establishment cannot leak into the next.
with_arm <- function(i, measure) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)
  apply_okinawa_setting(json_data, OKINAWA_FLOW_ARMS[i, ])
  measure()
}

#' Measure one arm under the sustained protocol, or read it back
#'
#' @param i Row of OKINAWA_FLOW_ARMS.
#' @return A list of the arm's `rows` and `series`, each carrying the arm label.
#'
#' @details A completed arm is written as it finishes and read back rather than
#'   re-run on a later invocation. The control seed is set once per arm, which is
#'   what pairs the arms. Delete the arm's files under CHECKPOINT_DIR to force a
#'   fresh measurement.
sustained_arm <- function(i) {
  arm <- OKINAWA_FLOW_ARMS[i, ]
  rows_path <- file.path(CHECKPOINT_DIR, sprintf("okinawa_flow_arm_%s.csv", arm$stem))
  series_path <- sub("\\.csv$", "_series.csv.gz", rows_path)
  if (file.exists(rows_path) && file.exists(series_path)) {
    rows <- read.csv(rows_path, stringsAsFactors = FALSE)
    if (nrow(rows) == opt$iterations) {
      message(sprintf("%s: resumed %d replications", arm$arm, nrow(rows)))
      return(list(rows = rows, series = read.csv(series_path, stringsAsFactors = FALSE)))
    }
  }
  message(sprintf("%s: %d replications x %d days", arm$arm, opt$iterations, opt$days))
  ckpt <- file.path(CHECKPOINT_DIR, sprintf("okinawa_flow_arm_%s_replications", arm$stem))
  dir.create(ckpt, recursive = TRUE, showWarnings = FALSE)
  out <- with_arm(i, function() {
    set.seed(opt$seed)
    run_okinawa_measurement(n_iterations = opt$iterations, n_days = opt$days,
                            window_days = opt$window, max_cores = opt$`max-cores`,
                            checkpoint_dir = ckpt)
  })
  out$rows$arm <- arm$arm
  out$series$arm <- arm$arm
  write.csv(out$rows, rows_path, row.names = FALSE)
  gz <- gzfile(series_path, "w")
  write.csv(out$series, gz, row.names = FALSE)
  close(gz)
  out
}

#' Measure one arm under the anchor protocol, or read it back
#'
#' @param i Row of OKINAWA_FLOW_ARMS.
#' @return The arm's anchor rows, every control seed's, carrying the arm label.
anchor_arm <- function(i) {
  arm <- OKINAWA_FLOW_ARMS[i, ]
  rows_path <- file.path(CHECKPOINT_DIR, sprintf("okinawa_flow_anchor_%s.csv", arm$stem))
  expected <- opt$`anchor-reps` * length(OKINAWA_FLOW_ANCHOR_SEEDS)
  if (file.exists(rows_path)) {
    rows <- read.csv(rows_path, stringsAsFactors = FALSE)
    if (nrow(rows) == expected) {
      message(sprintf("%s: resumed %d anchor replications", arm$arm, nrow(rows)))
      return(rows)
    }
  }
  rows <- with_arm(i, function() {
    do.call(rbind, lapply(OKINAWA_FLOW_ANCHOR_SEEDS, function(seed) {
      message(sprintf("%s: anchor seed %d, %d replications x %d days", arm$arm, seed,
                      opt$`anchor-reps`, opt$`anchor-days`))
      ckpt <- file.path(CHECKPOINT_DIR,
                        sprintf("okinawa_flow_anchor_%s_seed%d", arm$stem, seed))
      dir.create(ckpt, recursive = TRUE, showWarnings = FALSE)
      run_okinawa_anchor(seed, n_reps = opt$`anchor-reps`, n_days = opt$`anchor-days`,
                         max_cores = opt$`max-cores`, checkpoint_dir = ckpt)
    }))
  })
  rows$arm <- arm$arm
  write.csv(rows, rows_path, row.names = FALSE)
  rows
}

arm_index <- seq_len(nrow(OKINAWA_FLOW_ARMS))
anchor_rows <- do.call(rbind, lapply(arm_index, anchor_arm))
measured <- lapply(arm_index, sustained_arm)

for (i in arm_index) {
  gz <- gzfile(file.path(OUTPUT_DIR, sprintf("okinawa_flow_arm_%s_series.csv.gz",
                                             OKINAWA_FLOW_ARMS$stem[i])), "w")
  write.csv(measured[[i]]$series, gz, row.names = FALSE)
  close(gz)
}
per_replication <- do.call(rbind, lapply(measured, `[[`, "rows"))
series <- do.call(rbind, lapply(measured, `[[`, "series"))

summary_rows <- do.call(rbind, lapply(OKINAWA_FLOW_ARMS$arm, function(a) {
  cbind(data.frame(arm = a),
        summarise_policy(per_replication[per_replication$arm == a, ], arm_column = "arm"))
}))

others <- setdiff(OKINAWA_FLOW_ARMS$arm, OKINAWA_FLOW_BASELINE_ARM)
paired <- do.call(rbind, lapply(others, function(a) {
  do.call(rbind, lapply(OKINAWA_FLOW_PAIRED_RESPONSES, function(response) {
    row <- policy_paired_difference(per_replication, response, OKINAWA_FLOW_BASELINE_ARM, a,
                                    arm_column = "arm")
    row$reps_needed <- policy_replications_for(
      per_replication, response, OKINAWA_FLOW_BASELINE_ARM, a,
      OKINAWA_FLOW_HALF_WIDTHS[[response]], arm_column = "arm"
    )
    row
  }))
}))

stability <- if (opt$days >= SURGICAL_SWEEP_MIN_BLOCKS * LONG_HORIZON_BLOCK_DAYS) {
  okinawa_stability(series)
} else {
  message(sprintf("A campaign of %d days is too short to classify; no stability written",
                  opt$days))
  NULL
}

write.csv(per_replication, file.path(OUTPUT_DIR, "okinawa_flow_replications.csv"),
          row.names = FALSE)
write.csv(summary_rows, file.path(OUTPUT_DIR, "okinawa_flow_summary.csv"), row.names = FALSE)
write.csv(holm_adjust(paired), file.path(OUTPUT_DIR, "okinawa_flow_paired.csv"),
          row.names = FALSE)
if (!is.null(stability)) {
  write.csv(stability, file.path(OUTPUT_DIR, "okinawa_flow_stability.csv"), row.names = FALSE)
}
write.csv(anchor_rows, file.path(OUTPUT_DIR, "okinawa_flow_anchor_replications.csv"),
          row.names = FALSE)
write.csv(summarise_okinawa_anchor(anchor_rows),
          file.path(OUTPUT_DIR, "okinawa_flow_anchor_summary.csv"), row.names = FALSE)
message(sprintf("Okinawa flow responses, summaries, paired differences and stability written to %s",
                OUTPUT_DIR))
