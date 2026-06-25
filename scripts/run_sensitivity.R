#!/usr/bin/env Rscript
##############################################
## scripts/run_sensitivity.R               ##
## Morris EE sensitivity analysis runner   ##
##############################################
#
# Terminal / Claude Code cloud:
#   Rscript scripts/run_sensitivity.R                    # full Morris (r=20, reps=5)
#   Rscript scripts/run_sensitivity.R --quick            # smoke test (r=3, reps=3, days=5)
#   Rscript scripts/run_sensitivity.R --sobol            # Morris then Sobol on top 5
#
# RStudio Console (interactive):
#   source("R/sensitivity.R")                            # loads helpers only
#   mr <- run_morris(r=3, n_rep=3, n_days=5)            # smoke test
#   run_sobol(mr$ranking$parameter[1:5])                 # Sobol on top 5

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/sensitivity.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--days",       type = "integer", default = 30L,
              help = "Simulation days per evaluation [default: %default]"),
  make_option("--reps",       type = "integer", default = 5L,
              help = "Replications per Morris evaluation point [default: %default]"),
  make_option("--r",          type = "integer", default = 20L,
              help = "Number of Morris trajectories [default: %default]"),
  make_option("--levels",     type = "integer", default = 4L,
              help = "Morris grid levels [default: %default]"),
  make_option("--quick",      action = "store_true", default = FALSE,
              help = "Smoke test: r=3, reps=3, days=5"),
  make_option("--sobol",      action = "store_true", default = FALSE,
              help = "Run Sobol decomposition on top 5 parameters after Morris"),
  make_option("--n-sobol",    type = "integer", default = 200L,
              help = "Sobol sample size N [default: %default]"),
  make_option("--seed",       type = "integer", default = 42L,
              help = "Random seed for reproducibility [default: %default]"),
  make_option("--output-dir", type = "character", default = "outputs",
              help = "Directory for CSV and PNG outputs [default: %default]")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (opt$quick) {
  opt$r    <- 3L
  opt$reps <- 3L
  opt$days <- 5L
  message("Quick mode: r=3, reps=3, days=5")
}

message(sprintf(
  "Sensitivity config: r=%d, reps=%d, days=%d, levels=%d, seed=%d",
  opt$r, opt$reps, opt$days, opt$levels, opt$seed
))

set.seed(opt$seed)
env_data <<- load_elms("env_data.json")
day_min  <<- 1440L
counts   <<- sapply(env_data$elms, length)

morris_result <- run_morris(
  n_days     = opt$days,
  n_rep      = opt$reps,
  r          = opt$r,
  levels     = opt$levels,
  output_dir = opt[["output-dir"]]
)

if (opt$sobol) {
  top5 <- head(morris_result$ranking$parameter, 5)
  message(sprintf("\nRunning Sobol on top 5 parameters: %s", paste(top5, collapse = ", ")))
  run_sobol(
    top_params  = top5,
    n_days      = opt$days,
    n_rep       = opt$reps,
    n_sobol     = opt[["n-sobol"]],
    output_dir  = opt[["output-dir"]]
  )
}

message("\nSensitivity analysis complete.")
