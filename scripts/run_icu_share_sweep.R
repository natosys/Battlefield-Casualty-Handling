#!/usr/bin/env Rscript
##################################################
## scripts/run_icu_share_sweep.R                ##
## Forward ICU share decision-frontier sweep    ##
##################################################
#
# Terminal / Claude Code cloud:
#   Rscript scripts/run_icu_share_sweep.R                        # default: shares 0-1 by 0.25, 10 reps x 30 days
#   Rscript scripts/run_icu_share_sweep.R --shares "seq(0, 1, by = 0.1)"
#   Rscript scripts/run_icu_share_sweep.R --iterations 30 --days 30
#   Rscript scripts/run_icu_share_sweep.R --quick                # smoke test (2 reps, 3 days, 3 points)
#
# RStudio Console (interactive):
#   source("R/environment.R"); source("R/trajectories.R"); source("R/replication.R")
#   source("R/analysis.R"); source("R/scenario_runner.R")
#   sweep <- plot_r2b_icu_share_frontier(seq(0, 1, by = 0.25), n_rep = 10, n_days = 30)

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/scenario_runner.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--shares",     type = "character", default = "seq(0, 1, by = 0.25)",
              help = "Forward ICU shares to sweep, as an R range/vector expression [default: %default]"),
  make_option("--iterations", type = "integer", default = 10L,
              help = "Replications per share point [default: %default]"),
  make_option("--days",       type = "integer", default = 30L,
              help = "Simulation duration in days [default: %default]"),
  make_option("--seed",       type = "integer", default = 42L,
              help = "Random seed [default: %default]"),
  make_option("--quick",      action = "store_true", default = FALSE,
              help = "Smoke test: 2 iterations, 3 days, 3 share points"),
  make_option("--path",       type = "character", default = "env_data.json",
              help = "Path to env_data.json [default: %default]"),
  make_option("--output-dir", type = "character", default = "outputs",
              help = "Directory for CSV output [default: %default]"),
  make_option("--images-dir", type = "character", default = "images",
              help = "Directory for the saved plot [default: %default]")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (opt$quick) {
  opt$iterations <- 2L
  opt$days       <- 3L
  opt$shares     <- "c(0, 0.5, 1)"
  message("Quick mode: iterations=2, days=3, shares=c(0, 0.5, 1)")
}

shares <- eval(parse(text = opt$shares))

if (any(shares < 0 | shares > 1)) {
  stop("--shares must lie within [0, 1]: the forward ICU share is a fraction of one requirement.",
       call. = FALSE)
}

message(sprintf(
  "Forward ICU share sweep config: shares=%s, iterations=%d, days=%d, seed=%d",
  opt$shares, opt$iterations, opt$days, opt$seed
))

# plot_r2b_icu_share_frontier() saves/restores the global env_data/day_min/
# counts around its sweep (mirrors run_morris()'s env_data_base pattern,
# R/sensitivity.R), so they must already be set — same convention as
# scripts/run_transport_sweep.R.
env_data <<- load_elms(opt$path)
day_min  <<- DAY_MIN
counts   <<- sapply(env_data$elms, length)

set.seed(opt$seed)
sweep <- plot_r2b_icu_share_frontier(
  shares      = shares,
  n_days      = opt$days,
  n_rep       = opt$iterations,
  path        = opt$path,
  output_dir  = opt[["output-dir"]],
  images_dir  = opt[["images-dir"]]
)

message("\nForward ICU share sweep complete.")
print(sweep$data)
