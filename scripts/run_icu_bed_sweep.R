#!/usr/bin/env Rscript
##############################################################################
## scripts/run_icu_bed_sweep.R                                              ##
## The R2E intensive care bed establishment, at the sustained horizon       ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_icu_bed_sweep.R --refresh-baseline
#   Rscript scripts/run_icu_bed_sweep.R --scenario high_intensity \
#     --refresh-baseline
#   Rscript scripts/run_icu_bed_sweep.R --iterations 2 --days 30
#
# Why this exists. R2E intensive care is the pool whose shortfall the results
# paper measures most severely, and no experiment varied its establishment.
# This script sweeps the R2E intensive care bed count over the shipped four, six,
# eight, twelve and sixteen beds and reports what each buys in the intensive care
# and holding queues and whether they converge, in the post-operative and
# post-definitive care that reaches an intensive care bed, in theatre entry
# deferrals, in deaths of wounds and returns to duty, in the strategic
# evacuation wait and in the Role 4 census.
#
# The arms are paired on one control seed each, and each is checkpointed as it
# completes and resumed rather than re-run, so an interruption costs the arm in
# flight and not the whole measurement. A checkpoint is the arm's
# per-replication responses and its daily theatre series, both of which the
# summary and the stability classification are rebuilt from.
#
# --scenario writes every file, checkpoints included, with `_<scenario>`
# appended to its stem (scenario_output_path() in R/scenario.R), so a
# high-intensity run cannot overwrite the default set.
#
# --refresh-baseline is the only way to write the tracked data/icu_beds/
# copy, and it runs the protocol R/icu_bed_sweep.R holds rather than
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
source("R/cli.R")
source("R/run_manifest.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--iterations", type = "integer", default = ICU_BED_SWEEP_REPLICATIONS,
              help = "Replications per arm [default: %default]"),
  make_option("--days", type = "integer", default = ICU_BED_SWEEP_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--window", type = "integer", default = ICU_BED_SWEEP_WINDOW_DAYS,
              help = "Closing window the queue is measured over [default: %default]"),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = ICU_BED_SWEEP_SEED,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--config", type = "character", default = NULL,
              help = paste("Configuration file to run against, the shipped env_data.json",
                           "or one saved from the Shiny console. Rejected under",
                           "--refresh-baseline unless it is the shipped file",
                           "[default: env_data.json]")),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/icu_beds/ copy")
)

opt <- parse_args(OptionParser(option_list = option_list))
refresh_requested <- isTRUE(opt$`refresh-baseline`)
config_path <- resolve_config_path(opt$config, refresh_baseline = refresh_requested)

# A baseline refresh runs the documented protocol rather than whatever the
# caller passed, on the convention scripts/run_scenarios.R establishes.
if (isTRUE(opt$`refresh-baseline`)) {
  opt$iterations <- ICU_BED_SWEEP_REPLICATIONS
  opt$days       <- ICU_BED_SWEEP_DAYS
  opt$window     <- ICU_BED_SWEEP_WINDOW_DAYS
  opt$seed       <- ICU_BED_SWEEP_SEED
  message("Baseline refresh: running the documented protocol, ",
          sprintf("%d replications x %d days x %d arms at seed %d",
                  ICU_BED_SWEEP_REPLICATIONS, ICU_BED_SWEEP_DAYS,
                  nrow(ICU_BED_SWEEP_ARMS), ICU_BED_SWEEP_SEED))
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
  file.path("data", "icu_beds")
} else {
  file.path("outputs", "data", "icu_beds")
}

# A run outside the tracked evidence set records what it ran (R/run_manifest.R).
if (!refresh_requested) write_run_manifest(OUTPUT_DIR, config_path, opt)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

json_data <- read_config(config_path)

#' File stem of one arm's checkpoint
#'
#' @param i Row of ICU_BED_SWEEP_ARMS.
#' @return The stem, naming the bed count.
arm_stem <- function(i) {
  sprintf("icu_bed_sweep_arm_%02d", ICU_BED_SWEEP_ARMS$icu_beds[i])
}

#' Measure one arm and return its responses and daily series
#'
#' @param i Row of ICU_BED_SWEEP_ARMS.
#' @return A list of the arm's `rows` and `series`, each carrying the arm label.
#'
#' @details The configuration globals are restored on exit, on the error path as
#'   well as the success path. The control seed is set once per arm, from the
#'   caller, which is what pairs the arms.
measure_arm <- function(i) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  arm <- ICU_BED_SWEEP_ARMS[i, ]
  apply_icu_bed_setting(json_data, opt$scenario, arm$icu_beds)

  message(sprintf("%s: %d replications x %d days", arm$arm, opt$iterations, opt$days))
  set.seed(opt$seed)
  ckpt <- file.path("outputs", "checkpoints",
                    basename(scenario_output_path(".", arm_stem(i), opt$scenario)))
  dir.create(ckpt, recursive = TRUE, showWarnings = FALSE)
  out <- run_icu_bed_measurement(n_iterations = opt$iterations, n_days = opt$days,
                                 window_days = opt$window, max_cores = opt$`max-cores`,
                                 checkpoint_dir = ckpt)
  out$rows$arm <- arm$arm
  out$series$arm <- arm$arm
  out
}

#' Measure one arm, or read it back where it has already been measured
#'
#' @param i Row of ICU_BED_SWEEP_ARMS.
#' @return A list of the arm's `rows` and `series`.
#'
#' @details Each arm is written as it completes and read back rather than re-run
#'   on a later invocation. Delete the arm's files to force a fresh measurement.
measure_or_resume <- function(i) {
  rows_path <- scenario_output_path(OUTPUT_DIR, arm_stem(i), opt$scenario)
  series_path <- sub("\\.csv$", "_series.csv.gz", rows_path)
  label <- ICU_BED_SWEEP_ARMS$arm[i]
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

measured <- lapply(seq_len(nrow(ICU_BED_SWEEP_ARMS)), measure_or_resume)
per_replication <- do.call(rbind, lapply(measured, `[[`, "rows"))
series <- do.call(rbind, lapply(measured, `[[`, "series"))

summary_rows <- do.call(rbind, lapply(ICU_BED_SWEEP_ARMS$arm, function(a) {
  cbind(data.frame(arm = a),
        summarise_policy(per_replication[per_replication$arm == a, ], arm_column = "arm"))
}))

others <- setdiff(ICU_BED_SWEEP_ARMS$arm, ICU_BED_SWEEP_BASELINE_ARM)
paired <- do.call(rbind, lapply(others, function(a) {
  do.call(rbind, lapply(ICU_BED_SWEEP_PAIRED_RESPONSES, function(response) {
    row <- policy_paired_difference(per_replication, response, ICU_BED_SWEEP_BASELINE_ARM, a,
                                    arm_column = "arm")
    row$reps_needed <- policy_replications_for(
      per_replication, response, ICU_BED_SWEEP_BASELINE_ARM, a,
      ICU_BED_SWEEP_HALF_WIDTHS[[response]], arm_column = "arm"
    )
    row
  }))
}))

stability <- if (opt$days >= SURGICAL_SWEEP_MIN_BLOCKS * LONG_HORIZON_BLOCK_DAYS) {
  icu_bed_stability(series)
} else {
  message(sprintf("A campaign of %d days is too short to classify; no stability written",
                  opt$days))
  NULL
}

write.csv(per_replication,
          scenario_output_path(OUTPUT_DIR, "icu_bed_sweep_replications", opt$scenario),
          row.names = FALSE)
write.csv(summary_rows,
          scenario_output_path(OUTPUT_DIR, "icu_bed_sweep_summary", opt$scenario),
          row.names = FALSE)
write.csv(paired,
          scenario_output_path(OUTPUT_DIR, "icu_bed_sweep_paired", opt$scenario),
          row.names = FALSE)
if (!is.null(stability)) {
  write.csv(stability,
            scenario_output_path(OUTPUT_DIR, "icu_bed_sweep_stability", opt$scenario),
            row.names = FALSE)
}
message(sprintf(paste("Intensive care bed sweep responses, summary, paired differences and",
                      "stability written to %s"), OUTPUT_DIR))
