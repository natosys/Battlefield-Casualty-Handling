#!/usr/bin/env Rscript
##############################################################################
## scripts/run_post_definitive_sweep.R                                      ##
## The post-definitive intensive care share, at the sustained horizon       ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_post_definitive_sweep.R --refresh-baseline
#   Rscript scripts/run_post_definitive_sweep.R --scenario high_intensity \
#     --refresh-baseline
#   Rscript scripts/run_post_definitive_sweep.R --iterations 2 --days 30
#
# Why this exists. Every operated casualty at R2E is owed intensive care after
# the definitive repair, because the shipped share is 1, an upper bound no
# open-access source confirms (README Further Development L46). This script
# sweeps the share over 0.25, 0.5, 0.75 and 1 and reports what a lower share
# moves: intensive care and holding occupancy and queues and whether they
# converge, the overflow behind them, the ward recoveries and the holding
# bed-days they take, deaths of wounds and returns to duty.
#
# The arms are paired on one control seed each, and each replication is
# checkpointed as it completes and resumed rather than re-run, so an
# interruption costs the replications in flight and not the measurement. A
# completed arm is checkpointed as its per-replication responses and its daily
# series, from both of which the summary and the stability classification are
# rebuilt.
#
# --scenario writes every file, checkpoints included, with `_<scenario>`
# appended to its stem (scenario_output_path() in R/scenario.R), so a
# high-intensity run cannot overwrite the default set.
#
# --refresh-baseline is the only way to write the tracked data/post_definitive/
# copy, and it runs the protocol R/post_definitive_sweep.R holds rather than
# whatever arguments accompany it, so the tracked set and the design
# docs/Methods.md documents cannot diverge through a mistyped argument.

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
source("R/post_definitive_sweep.R")
source("R/cli.R")
source("R/run_manifest.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--iterations", type = "integer", default = POST_DEFINITIVE_SWEEP_REPLICATIONS,
              help = "Replications per arm [default: %default]"),
  make_option("--days", type = "integer", default = POST_DEFINITIVE_SWEEP_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--window", type = "integer", default = POST_DEFINITIVE_SWEEP_WINDOW_DAYS,
              help = "Closing window the queue is measured over [default: %default]"),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = POST_DEFINITIVE_SWEEP_SEED,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--config", type = "character", default = NULL,
              help = paste("Configuration file to run against, the shipped env_data.json",
                           "or one saved from the Shiny console. Rejected under",
                           "--refresh-baseline unless it is the shipped file",
                           "[default: env_data.json]")),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/post_definitive/ copy")
)

opt <- parse_args(OptionParser(option_list = option_list))
refresh_requested <- isTRUE(opt$`refresh-baseline`)
config_path <- resolve_config_path(opt$config, refresh_baseline = refresh_requested)

# A baseline refresh runs the documented protocol rather than whatever the
# caller passed, on the convention scripts/run_scenarios.R establishes.
if (isTRUE(opt$`refresh-baseline`)) {
  opt$iterations <- POST_DEFINITIVE_SWEEP_REPLICATIONS
  opt$days       <- POST_DEFINITIVE_SWEEP_DAYS
  opt$window     <- POST_DEFINITIVE_SWEEP_WINDOW_DAYS
  opt$seed       <- POST_DEFINITIVE_SWEEP_SEED
  message("Baseline refresh: running the documented protocol, ",
          sprintf("%d replications x %d days x %d arms at seed %d",
                  POST_DEFINITIVE_SWEEP_REPLICATIONS, POST_DEFINITIVE_SWEEP_DAYS,
                  nrow(POST_DEFINITIVE_SWEEP_ARMS), POST_DEFINITIVE_SWEEP_SEED))
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
  file.path("data", "post_definitive")
} else {
  file.path("outputs", "data", "post_definitive")
}

# A run outside the tracked evidence set records what it ran (R/run_manifest.R).
if (!refresh_requested) write_run_manifest(OUTPUT_DIR, config_path, opt)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

#' Directory each arm's checkpoint, and each replication within it, is resumed from
CHECKPOINT_DIR <- checkpoint_dir("post_definitive", refresh_requested)

json_data <- read_config(config_path)

#' File stem of one arm's checkpoint
#'
#' @param i Row of POST_DEFINITIVE_SWEEP_ARMS.
#' @return The stem, naming the share in percent.
arm_stem <- function(i) {
  sprintf("post_definitive_sweep_arm_share_%03d",
          as.integer(round(100 * POST_DEFINITIVE_SWEEP_ARMS$share[i])))
}

#' Measure one arm and return its responses and daily series
#'
#' @param i Row of POST_DEFINITIVE_SWEEP_ARMS.
#' @return A list of the arm's `rows` and `series`, each carrying the arm label.
#'
#' @details The configuration globals are restored on exit, on the error path as
#'   well as the success path. The control seed is set once per arm, from the
#'   caller, which is what pairs the arms.
measure_arm <- function(i) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  arm <- POST_DEFINITIVE_SWEEP_ARMS[i, ]
  apply_post_definitive_setting(json_data, opt$scenario, arm$share)

  message(sprintf("%s: %d replications x %d days", arm$arm, opt$iterations, opt$days))
  set.seed(opt$seed)
  ckpt <- sub("\\.csv$", "_replications",
              scenario_output_path(CHECKPOINT_DIR, arm_stem(i), opt$scenario))
  dir.create(ckpt, recursive = TRUE, showWarnings = FALSE)
  out <- run_post_definitive_measurement(n_iterations = opt$iterations, n_days = opt$days,
                                         window_days = opt$window,
                                         max_cores = opt$`max-cores`,
                                         checkpoint_dir = ckpt)
  out$rows$arm <- arm$arm
  out$series$arm <- arm$arm
  out
}

#' Measure one arm, or read it back where it has already been measured
#'
#' @param i Row of POST_DEFINITIVE_SWEEP_ARMS.
#' @return A list of the arm's `rows` and `series`.
#'
#' @details Each arm is written as it completes and read back rather than re-run
#'   on a later invocation. Delete the arm's files under CHECKPOINT_DIR to force a
#'   fresh measurement.
measure_or_resume <- function(i) {
  rows_path <- scenario_output_path(CHECKPOINT_DIR, arm_stem(i), opt$scenario)
  series_path <- sub("\\.csv$", "_series.csv.gz", rows_path)
  label <- POST_DEFINITIVE_SWEEP_ARMS$arm[i]
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

measured <- lapply(seq_len(nrow(POST_DEFINITIVE_SWEEP_ARMS)), measure_or_resume)

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

summary_rows <- do.call(rbind, lapply(POST_DEFINITIVE_SWEEP_ARMS$arm, function(a) {
  cbind(data.frame(arm = a),
        summarise_policy(per_replication[per_replication$arm == a, ], arm_column = "arm"))
}))

baseline_arm <- POST_DEFINITIVE_SWEEP_BASELINE_ARM
others <- setdiff(POST_DEFINITIVE_SWEEP_ARMS$arm, baseline_arm)
paired <- do.call(rbind, lapply(others, function(a) {
  do.call(rbind, lapply(POST_DEFINITIVE_SWEEP_PAIRED_RESPONSES, function(response) {
    row <- policy_paired_difference(per_replication, response, baseline_arm, a,
                                    arm_column = "arm")
    row$reps_needed <- policy_replications_for(
      per_replication, response, baseline_arm, a,
      POST_DEFINITIVE_SWEEP_HALF_WIDTHS[[response]], arm_column = "arm"
    )
    row
  }))
}))

stability <- if (opt$days >= SURGICAL_SWEEP_MIN_BLOCKS * LONG_HORIZON_BLOCK_DAYS) {
  post_definitive_stability(series)
} else {
  message(sprintf("A campaign of %d days is too short to classify; no stability written",
                  opt$days))
  NULL
}

write.csv(per_replication,
          scenario_output_path(OUTPUT_DIR, "post_definitive_sweep_replications", opt$scenario),
          row.names = FALSE)
write.csv(summary_rows,
          scenario_output_path(OUTPUT_DIR, "post_definitive_sweep_summary", opt$scenario),
          row.names = FALSE)
write.csv(holm_adjust(paired),
          scenario_output_path(OUTPUT_DIR, "post_definitive_sweep_paired", opt$scenario),
          row.names = FALSE)
if (!is.null(stability)) {
  write.csv(stability,
            scenario_output_path(OUTPUT_DIR, "post_definitive_sweep_stability", opt$scenario),
            row.names = FALSE)
}
message(sprintf(paste("Post-definitive share sweep responses, summary, paired differences",
                      "and stability written to %s"), OUTPUT_DIR))
