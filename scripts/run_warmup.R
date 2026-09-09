#!/usr/bin/env Rscript
##############################################
## scripts/run_warmup.R                     ##
## Welch warm-up analysis entry point       ##
##############################################
#
# Run from repo root:
#   Rscript scripts/run_warmup.R              # default: 10 reps, 90 days, seed 42
#   Rscript scripts/run_warmup.R --reps 5 --days 60
#   Rscript scripts/run_warmup.R --seed 7 --output-dir /tmp/welch
#   Rscript scripts/run_warmup.R --refresh-baseline
#
# Artifact policy. Every run writes under --output-dir and --images-dir, both
# of which default inside outputs/, so an exploratory run cannot overwrite a
# tracked file. images/welch_plot_icu_queue.png is tracked, and
# --refresh-baseline is the only way to write it, matching run.R's policy
# (Issue #154).
#
# The measurement is a function of --seed alone: run_replications() draws each
# replication's seed from the parent stream, so seeding here is what makes a
# published warm-up figure reproducible from the command line (Issue #208).

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--reps", type = "integer", default = 10L,
              help = "Number of replications [default: %default]"),
  make_option("--days", type = "integer", default = 90L,
              help = "Simulation duration in days [default: %default]"),
  make_option("--seed", type = "integer", default = 42L,
              help = "Random seed for the measurement [default: %default]"),
  make_option("--output-dir", type = "character", default = "outputs",
              help = "Directory for welch_cma.csv [default: %default]"),
  make_option("--images-dir", type = "character", default = NULL,
              help = paste("Directory for the Welch plot. Ignored under",
                           "--refresh-baseline [default: <output-dir>/images]")),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = paste("Write the tracked images/welch_plot_icu_queue.png.",
                           "Without it, no tracked file is written",
                           "[default: %default]"))
)

opt <- parse_args(OptionParser(option_list = option_list))

source("R/cli.R")

require_number_in_range(opt$reps, "--reps", 1)
require_number_in_range(opt$days, "--days", 1)
require_number_in_range(opt$seed, "--seed", -.Machine$integer.max,
                        .Machine$integer.max)
require_directory(opt[["output-dir"]], "--output-dir")
require_directory(opt[["images-dir"]], "--images-dir")

images_dir <- if (opt[["refresh-baseline"]]) {
  "images"
} else if (is.null(opt[["images-dir"]])) {
  file.path(opt[["output-dir"]], "images")
} else {
  opt[["images-dir"]]
}

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/warmup.R")

env_data <<- load_elms("env_data.json")
day_min  <<- DAY_MIN
counts   <<- sapply(env_data$elms, length)

message(sprintf("Welch configuration: reps=%d, days=%d, seed=%d",
                opt$reps, opt$days, opt$seed))
if (opt[["refresh-baseline"]]) {
  message("Baseline refresh: writing tracked images/welch_plot_icu_queue.png")
}

set.seed(opt$seed)
run_welch_analysis(n_reps = opt$reps, n_days = opt$days,
                   output_dir = opt[["output-dir"]], images_dir = images_dir)
