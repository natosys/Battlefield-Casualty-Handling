#!/usr/bin/env Rscript
##############################################################################
## scripts/run_casualty_surge_size_sweep.R                                   ##
## The casualty surge event size sweep                                       ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_casualty_surge_size_sweep.R --refresh-baseline
#   Rscript scripts/run_casualty_surge_size_sweep.R --iterations 4 --days 30
#
# Why this exists. The 20 to 60 casualty event range is an informed estimate,
# so the stress test alone cannot say how large an event must be before care
# degrades. This sweeps a fixed event size at the stress test's injection
# rate and reports the died-of-wounds rate among event casualties and the peak
# pool queues against size, beside a no-event arm.
#
# --refresh-baseline is the only way to write the tracked
# data/casualty_surge/ size sweep files, and it runs the documented protocol
# (30 replications x 360 days per size at seed 42) rather than the caller's
# arguments. Each size is checkpointed as it completes and resumed rather than
# re-run, so an interruption costs the size in flight.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/casualty_surge.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--iterations", type = "integer", default = CASUALTY_SURGE_SIZE_REPLICATIONS,
              help = "Replications per size [default: %default]"),
  make_option("--days", type = "integer", default = CASUALTY_SURGE_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = CASUALTY_SURGE_SEED,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/casualty_surge/ size sweep files")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (isTRUE(opt$`refresh-baseline`)) {
  opt$iterations <- CASUALTY_SURGE_SIZE_REPLICATIONS
  opt$days       <- CASUALTY_SURGE_DAYS
  opt$seed       <- CASUALTY_SURGE_SEED
}
if (opt$iterations < 1L) {
  stop("--iterations must be at least 1, found ", opt$iterations, call. = FALSE)
}
if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)

#' Directory the measurement is written to
OUTPUT_DIR <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "casualty_surge")
} else {
  file.path("outputs", "data", "casualty_surge")
}
dir.create(file.path(OUTPUT_DIR, "size_checkpoints"), recursive = TRUE, showWarnings = FALSE)

#' Suffix every file of this run carries, so a named scenario is kept beside the default
OUTPUT_SUFFIX <- scenario_output_suffix(opt$scenario)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Measure one size, resuming its checkpoint when present
#'
#' @param size Casualties per event; 0 is the no-event arm.
#' @return The size's per-replication responses.
measure_size <- function(size) {
  checkpoint <- file.path(OUTPUT_DIR, "size_checkpoints",
                          sprintf("size_%03d%s.csv", size, OUTPUT_SUFFIX))
  if (file.exists(checkpoint)) {
    message(sprintf("Size %d: resuming %s", size, checkpoint))
    return(read.csv(checkpoint))
  }
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  rate <- if (size == 0) 0 else CASUALTY_SURGE_SIZE_RATE
  apply_casualty_surge_size_setting(json_data, opt$scenario,
                                    if (size == 0) 40L else size, rate)
  message(sprintf("Size %d: %d replications x %d days", size, opt$iterations, opt$days))
  set.seed(opt$seed)
  rows <- run_casualty_surge_size_measurement(size, opt$iterations, opt$days, opt$`max-cores`)
  write.csv(rows, checkpoint, row.names = FALSE)
  rows
}

per_replication <- do.call(rbind, lapply(c(0L, CASUALTY_SURGE_SIZES), measure_size))
summary_rows <- summarise_casualty_surge_size(per_replication)

write.csv(per_replication, file.path(OUTPUT_DIR,
                    sprintf("casualty_surge_size_replications%s.csv", OUTPUT_SUFFIX)),
          row.names = FALSE)
write.csv(summary_rows, file.path(OUTPUT_DIR,
                    sprintf("casualty_surge_size_summary%s.csv", OUTPUT_SUFFIX)),
          row.names = FALSE)
message(sprintf("Size sweep written to %s", OUTPUT_DIR))
