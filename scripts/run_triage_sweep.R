#!/usr/bin/env Rscript
##############################################################################
## scripts/run_triage_sweep.R                                               ##
## The Priority 1 triage share, at the sustained horizon                    ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_triage_sweep.R --refresh-baseline
#   Rscript scripts/run_triage_sweep.R --scenario high_intensity \
#     --refresh-baseline
#   Rscript scripts/run_triage_sweep.R --iterations 2 --days 30
#
# Why this exists. The background triage split is an informed estimate and the
# model's only proxy for severity, and the comparators the README cites put
# urgent surgical need well below the shipped Priority 1 share of 0.65. This
# script sweeps that share over 0.15, 0.30, 0.45 and 0.65, the Priority 2 to
# Priority 3 ratio held at 20:15, and reports whether the capacity findings hold:
# the R2E theatre, intensive care and holding queues and whether they converge,
# R2B diversions, time to first incision and surgical delivery, deaths of
# wounds, returns to duty, strategic evacuations, the critical airlift backlog
# and the Role 4 intensive care peak, with each arm's treated-cohort died-of-
# wounds rate as a diagnostic of the calibration fitted at the shipped mix.
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
# --refresh-baseline is the only way to write the tracked data/triage_split/
# copy, and it runs the protocol R/triage_sweep.R holds rather than whatever
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
source("R/cli.R")
source("R/run_manifest.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--iterations", type = "integer", default = TRIAGE_SWEEP_REPLICATIONS,
              help = "Replications per arm [default: %default]"),
  make_option("--days", type = "integer", default = TRIAGE_SWEEP_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--window", type = "integer", default = TRIAGE_SWEEP_WINDOW_DAYS,
              help = "Closing window the queue is measured over [default: %default]"),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = TRIAGE_SWEEP_SEED,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--config", type = "character", default = NULL,
              help = paste("Configuration file to run against, the shipped env_data.json",
                           "or one saved from the Shiny console. Rejected under",
                           "--refresh-baseline unless it is the shipped file",
                           "[default: env_data.json]")),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/triage_split/ copy")
)

opt <- parse_args(do.call(OptionParser, c(list(option_list = option_list),
                                          cli_help("scripts/run_triage_sweep.R"))))
refresh_requested <- isTRUE(opt$`refresh-baseline`)
config_path <- resolve_config_path(opt$config, refresh_baseline = refresh_requested)

# A baseline refresh runs the documented protocol rather than whatever the
# caller passed, on the convention scripts/run_scenarios.R establishes.
if (isTRUE(opt$`refresh-baseline`)) {
  opt$iterations <- TRIAGE_SWEEP_REPLICATIONS
  opt$days       <- TRIAGE_SWEEP_DAYS
  opt$window     <- TRIAGE_SWEEP_WINDOW_DAYS
  opt$seed       <- TRIAGE_SWEEP_SEED
  message("Baseline refresh: running the documented protocol, ",
          sprintf("%d replications x %d days x %d arms at seed %d",
                  TRIAGE_SWEEP_REPLICATIONS, TRIAGE_SWEEP_DAYS,
                  nrow(TRIAGE_SWEEP_ARMS), TRIAGE_SWEEP_SEED))
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
  file.path("data", "triage_split")
} else {
  file.path("outputs", "data", "triage_split")
}

# A run outside the tracked evidence set records what it ran (R/run_manifest.R).
if (!refresh_requested) write_run_manifest(OUTPUT_DIR, config_path, opt)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

#' Directory each arm's checkpoint, and each replication within it, is resumed from
CHECKPOINT_DIR <- checkpoint_dir("triage_split", refresh_requested, config_path)

json_data <- read_config(config_path)

#' File stem of one arm's checkpoint
#'
#' @param i Row of TRIAGE_SWEEP_ARMS.
#' @return The stem, naming the Priority 1 share in percent.
arm_stem <- function(i) {
  sprintf("triage_sweep_arm_p1_%02d", as.integer(round(100 * TRIAGE_SWEEP_ARMS$p1[i])))
}

#' Measure one arm and return its responses and daily series
#'
#' @param i Row of TRIAGE_SWEEP_ARMS.
#' @return A list of the arm's `rows` and `series`, each carrying the arm label.
#'
#' @details The configuration globals are restored on exit, on the error path as
#'   well as the success path. The control seed is set once per arm, from the
#'   caller, which is what pairs the arms.
measure_arm <- function(i) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  arm <- TRIAGE_SWEEP_ARMS[i, ]
  apply_triage_setting(json_data, opt$scenario, arm$p1)

  message(sprintf("%s: %d replications x %d days", arm$arm, opt$iterations, opt$days))
  set.seed(opt$seed)
  ckpt <- sub("\\.csv$", "_replications",
              scenario_output_path(CHECKPOINT_DIR, arm_stem(i), opt$scenario))
  dir.create(ckpt, recursive = TRUE, showWarnings = FALSE)
  out <- run_triage_measurement(n_iterations = opt$iterations, n_days = opt$days,
                                window_days = opt$window, max_cores = opt$`max-cores`,
                                checkpoint_dir = ckpt)
  out$rows$arm <- arm$arm
  out$series$arm <- arm$arm
  out
}

#' Measure one arm, or read it back where it has already been measured
#'
#' @param i Row of TRIAGE_SWEEP_ARMS.
#' @return A list of the arm's `rows` and `series`.
#'
#' @details Each arm is written as it completes and read back rather than re-run
#'   on a later invocation. Delete the arm's files under CHECKPOINT_DIR to force a
#'   fresh measurement.
measure_or_resume <- function(i) {
  rows_path <- scenario_output_path(CHECKPOINT_DIR, arm_stem(i), opt$scenario)
  series_path <- sub("\\.csv$", "_series.csv.gz", rows_path)
  label <- TRIAGE_SWEEP_ARMS$arm[i]
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

measured <- lapply(seq_len(nrow(TRIAGE_SWEEP_ARMS)), measure_or_resume)

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

summary_rows <- do.call(rbind, lapply(TRIAGE_SWEEP_ARMS$arm, function(a) {
  cbind(data.frame(arm = a),
        summarise_policy(per_replication[per_replication$arm == a, ], arm_column = "arm"))
}))

others <- setdiff(TRIAGE_SWEEP_ARMS$arm, TRIAGE_SWEEP_BASELINE_ARM)
paired <- do.call(rbind, lapply(others, function(a) {
  do.call(rbind, lapply(TRIAGE_SWEEP_PAIRED_RESPONSES, function(response) {
    row <- policy_paired_difference(per_replication, response, TRIAGE_SWEEP_BASELINE_ARM, a,
                                    arm_column = "arm")
    row$reps_needed <- policy_replications_for(
      per_replication, response, TRIAGE_SWEEP_BASELINE_ARM, a,
      TRIAGE_SWEEP_HALF_WIDTHS[[response]], arm_column = "arm"
    )
    row
  }))
}))

stability <- if (opt$days >= SURGICAL_SWEEP_MIN_BLOCKS * LONG_HORIZON_BLOCK_DAYS) {
  triage_stability(series)
} else {
  message(sprintf("A campaign of %d days is too short to classify; no stability written",
                  opt$days))
  NULL
}

write.csv(per_replication,
          scenario_output_path(OUTPUT_DIR, "triage_sweep_replications", opt$scenario),
          row.names = FALSE)
write.csv(summary_rows,
          scenario_output_path(OUTPUT_DIR, "triage_sweep_summary", opt$scenario),
          row.names = FALSE)
write.csv(holm_adjust(paired),
          scenario_output_path(OUTPUT_DIR, "triage_sweep_paired", opt$scenario),
          row.names = FALSE)
if (!is.null(stability)) {
  write.csv(stability,
            scenario_output_path(OUTPUT_DIR, "triage_sweep_stability", opt$scenario),
            row.names = FALSE)
}
write.csv(triage_treated_rate(per_replication),
          scenario_output_path(OUTPUT_DIR, "triage_sweep_treated_cohort", opt$scenario),
          row.names = FALSE)
message(sprintf(paste("Triage sweep responses, summary, paired differences, stability and",
                      "treated cohort written to %s"), OUTPUT_DIR))
