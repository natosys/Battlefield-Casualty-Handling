#!/usr/bin/env Rscript
##############################################
## scripts/run_warmup.R                     ##
## Welch warm-up analysis entry point       ##
##############################################
#
# Run from repo root:
#   Rscript scripts/run_warmup.R              # default: 10 reps, 90 days
#   Rscript scripts/run_warmup.R --reps 5 --days 60

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--reps", type = "integer", default = 10L,
              help = "Number of replications [default: %default]"),
  make_option("--days", type = "integer", default = 90L,
              help = "Simulation duration in days [default: %default]")
)

opt <- parse_args(OptionParser(option_list = option_list))

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/warmup.R")

env_data <<- load_elms("env_data.json")
day_min  <<- 1440L
counts   <<- sapply(env_data$elms, length)

run_welch_analysis(n_reps = opt$reps, n_days = opt$days)
