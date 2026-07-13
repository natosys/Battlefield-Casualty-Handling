#!/usr/bin/env Rscript
##############################################
## scripts/run_transport_sweep.R            ##
## Transport fleet-size capacity margin sweep ##
##############################################
#
# Terminal / Claude Code cloud:
#   Rscript scripts/run_transport_sweep.R                          # default: PMVAmb 1-5, HX240M 1-4, 10 reps x 30 days
#   Rscript scripts/run_transport_sweep.R --pmvamb 1:5 --hx240m 1:4
#   Rscript scripts/run_transport_sweep.R --iterations 30 --days 30
#   Rscript scripts/run_transport_sweep.R --quick                  # smoke test (2 reps, 3 days)
#
# RStudio Console (interactive):
#   source("R/environment.R"); source("R/trajectories.R"); source("R/replication.R")
#   source("R/analysis.R"); source("R/scenario_runner.R")
#   sweep <- plot_transport_capacity_margin_by_fleet_size(list(PMVAmb = 1:5, HX240M = 1:4), n_rep = 10, n_days = 30)

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/scenario_runner.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--pmvamb",     type = "character", default = "1:5",
              help = "PMV Ambulance fleet sizes to sweep, as an R range/vector expression [default: %default]"),
  make_option("--hx240m",     type = "character", default = "1:4",
              help = "HX240M fleet sizes to sweep, as an R range/vector expression [default: %default]"),
  make_option("--iterations", type = "integer", default = 10L,
              help = "Replications per fleet-size point [default: %default]"),
  make_option("--days",       type = "integer", default = 30L,
              help = "Simulation duration in days [default: %default]"),
  make_option("--seed",       type = "integer", default = 42L,
              help = "Random seed [default: %default]"),
  make_option("--quick",      action = "store_true", default = FALSE,
              help = "Smoke test: 2 iterations, 3 days"),
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
  message("Quick mode: iterations=2, days=3")
}

fleet_sizes <- list(
  PMVAmb = eval(parse(text = opt$pmvamb)),
  HX240M = eval(parse(text = opt$hx240m))
)

message(sprintf(
  "Transport fleet-size sweep config: PMVAmb=%s, HX240M=%s, iterations=%d, days=%d, seed=%d",
  opt$pmvamb, opt$hx240m, opt$iterations, opt$days, opt$seed
))

# plot_transport_capacity_margin_by_fleet_size() saves/restores the global
# env_data/day_min/counts around its sweep (mirrors run_morris()'s
# env_data_base pattern, R/sensitivity.R), so they must already be set —
# same convention as scripts/run_sensitivity.R.
env_data <<- load_elms(opt$path)
day_min  <<- 1440L
counts   <<- sapply(env_data$elms, length)

set.seed(opt$seed)
sweep <- plot_transport_capacity_margin_by_fleet_size(
  fleet_sizes = fleet_sizes,
  n_days      = opt$days,
  n_rep       = opt$iterations,
  path        = opt$path,
  output_dir  = opt[["output-dir"]],
  images_dir  = opt[["images-dir"]]
)

message("\nTransport fleet-size sweep complete.")
print(sweep$data)
