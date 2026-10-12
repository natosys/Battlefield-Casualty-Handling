#!/usr/bin/env Rscript
##############################################################################
## scripts/run_surgical_team_sweep.R                                        ##
## The surgical team establishment at R2B and R2E, at the sustained horizon  ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_surgical_team_sweep.R --refresh-baseline
#   Rscript scripts/run_surgical_team_sweep.R --scenario high_intensity \
#     --refresh-baseline
#   Rscript scripts/run_surgical_team_sweep.R --iterations 2 --days 30
#
# Why this exists. The planning paper's first recommendation is to extend
# surgical cover towards 24 hours at R2B and R2E, and until this sweep its
# remedy was argued from the model's structure and not measured. This script
# varies the number of surgical sections each echelon fields: three, four and
# six at R2E, and one or two per unit at R2B, the second taking the shift the
# first does not, with the two levers together as the last arm. It reports what
# each buys in the R2E theatre queue and whether that queue converges, in the
# diversions from R2B, in operations performed, in deaths of wounds and in
# returns to duty.
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
# --refresh-baseline is the only way to write the tracked data/surgical_teams/
# copy, and it runs the protocol R/surgical_team_sweep.R holds rather than
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
source("R/surgical_team_sweep.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--iterations", type = "integer", default = SURGICAL_SWEEP_REPLICATIONS,
              help = "Replications per arm [default: %default]"),
  make_option("--days", type = "integer", default = SURGICAL_SWEEP_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--window", type = "integer", default = SURGICAL_SWEEP_WINDOW_DAYS,
              help = "Closing window the queue is measured over [default: %default]"),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = SURGICAL_SWEEP_SEED,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/surgical_teams/ copy")
)

opt <- parse_args(OptionParser(option_list = option_list))

# A baseline refresh runs the documented protocol rather than whatever the
# caller passed, on the convention scripts/run_scenarios.R establishes.
if (isTRUE(opt$`refresh-baseline`)) {
  opt$iterations <- SURGICAL_SWEEP_REPLICATIONS
  opt$days       <- SURGICAL_SWEEP_DAYS
  opt$window     <- SURGICAL_SWEEP_WINDOW_DAYS
  opt$seed       <- SURGICAL_SWEEP_SEED
  message("Baseline refresh: running the documented protocol, ",
          sprintf("%d replications x %d days x %d arms at seed %d",
                  SURGICAL_SWEEP_REPLICATIONS, SURGICAL_SWEEP_DAYS,
                  nrow(SURGICAL_SWEEP_ARMS), SURGICAL_SWEEP_SEED))
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
  file.path("data", "surgical_teams")
} else {
  file.path("outputs", "data", "surgical_teams")
}
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' File stem of one arm's checkpoint
#'
#' @param i Row of SURGICAL_SWEEP_ARMS.
#' @return The stem, naming both section counts.
arm_stem <- function(i) {
  sprintf("surgical_team_sweep_arm_r2e%d_r2b%d", SURGICAL_SWEEP_ARMS$r2e_sections[i],
          SURGICAL_SWEEP_ARMS$r2b_sections[i])
}

#' Measure one arm and return its responses and daily series
#'
#' @param i Row of SURGICAL_SWEEP_ARMS.
#' @return A list of the arm's `rows` and `series`, each carrying the arm label.
#'
#' @details The configuration globals are restored on exit, on the error path as
#'   well as the success path. The control seed is set once per arm, from the
#'   caller, which is what pairs the arms.
measure_arm <- function(i) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  arm <- SURGICAL_SWEEP_ARMS[i, ]
  apply_surgical_setting(json_data, opt$scenario, arm$r2e_sections, arm$r2b_sections)

  message(sprintf("%s: %d replications x %d days", arm$arm, opt$iterations, opt$days))
  set.seed(opt$seed)
  ckpt <- file.path("outputs", "checkpoints",
                    basename(scenario_output_path(".", arm_stem(i), opt$scenario)))
  dir.create(ckpt, recursive = TRUE, showWarnings = FALSE)
  out <- run_surgical_measurement(n_iterations = opt$iterations, n_days = opt$days,
                                  window_days = opt$window, max_cores = opt$`max-cores`,
                                  checkpoint_dir = ckpt)
  out$rows$arm <- arm$arm
  out$series$arm <- arm$arm
  out
}

#' Measure one arm, or read it back where it has already been measured
#'
#' @param i Row of SURGICAL_SWEEP_ARMS.
#' @return A list of the arm's `rows` and `series`.
#'
#' @details Each arm is written as it completes and read back rather than re-run
#'   on a later invocation. Delete the arm's files to force a fresh measurement.
measure_or_resume <- function(i) {
  rows_path <- scenario_output_path(OUTPUT_DIR, arm_stem(i), opt$scenario)
  series_path <- sub("\\.csv$", "_series.csv.gz", rows_path)
  label <- SURGICAL_SWEEP_ARMS$arm[i]
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

measured <- lapply(seq_len(nrow(SURGICAL_SWEEP_ARMS)), measure_or_resume)
per_replication <- do.call(rbind, lapply(measured, `[[`, "rows"))
series <- do.call(rbind, lapply(measured, `[[`, "series"))

summary_rows <- do.call(rbind, lapply(SURGICAL_SWEEP_ARMS$arm, function(a) {
  cbind(data.frame(arm = a),
        summarise_policy(per_replication[per_replication$arm == a, ], arm_column = "arm"))
}))

others <- setdiff(SURGICAL_SWEEP_ARMS$arm, SURGICAL_SWEEP_BASELINE)
paired <- do.call(rbind, lapply(others, function(a) {
  do.call(rbind, lapply(SURGICAL_SWEEP_PAIRED_RESPONSES, function(response) {
    row <- policy_paired_difference(per_replication, response, SURGICAL_SWEEP_BASELINE, a,
                                    arm_column = "arm")
    row$reps_needed <- policy_replications_for(
      per_replication, response, SURGICAL_SWEEP_BASELINE, a,
      SURGICAL_SWEEP_HALF_WIDTHS[[response]], arm_column = "arm"
    )
    row
  }))
}))

stability <- if (opt$days >= SURGICAL_SWEEP_MIN_BLOCKS * LONG_HORIZON_BLOCK_DAYS) {
  surgical_stability(series)
} else {
  message(sprintf("A campaign of %d days is too short to classify; no stability written",
                  opt$days))
  NULL
}

write.csv(per_replication,
          scenario_output_path(OUTPUT_DIR, "surgical_team_sweep_replications", opt$scenario),
          row.names = FALSE)
write.csv(summary_rows,
          scenario_output_path(OUTPUT_DIR, "surgical_team_sweep_summary", opt$scenario),
          row.names = FALSE)
write.csv(paired,
          scenario_output_path(OUTPUT_DIR, "surgical_team_sweep_paired", opt$scenario),
          row.names = FALSE)
if (!is.null(stability)) {
  write.csv(stability,
            scenario_output_path(OUTPUT_DIR, "surgical_team_sweep_stability", opt$scenario),
            row.names = FALSE)
}
message(sprintf(paste("Surgical team sweep responses, summary, paired differences and",
                      "stability written to %s"), OUTPUT_DIR))
