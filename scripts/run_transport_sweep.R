#!/usr/bin/env Rscript
##############################################
## scripts/run_transport_sweep.R            ##
## Transport fleet-size capacity margin sweep ##
##############################################
#
# Terminal / Claude Code cloud:
#   Rscript scripts/run_transport_sweep.R --refresh-baseline       # write the tracked data/sweeps/
#   Rscript scripts/run_transport_sweep.R                          # default: PMVAmb 1-5, HX240M 1-4, 10 reps x 30 days
#   Rscript scripts/run_transport_sweep.R --pmvamb 1:5 --hx240m 1:4
#   Rscript scripts/run_transport_sweep.R --iterations 30 --days 360
#   Rscript scripts/run_transport_sweep.R --quick                  # smoke test (2 reps, 3 days)
#   Rscript scripts/run_transport_sweep.R --scenario high_intensity --refresh-baseline
#
# --refresh-baseline is the only way to write the tracked data/sweeps/ and the
# tracked images/transport_capacity_margin_by_fleet_size.png. Without it every
# invocation writes under outputs/ alone, so an exploratory run cannot move the
# evidence set docs/Multi_Run_Analysis.md's fleet-size table is checked
# against. The flag fixes the swept range, the replication count (30, the
# sustained-operations protocol, migrated from 10 under Issue #405), the
# horizon (360 days, migrated from 30) and the seed rather than accepting
# whichever the caller passed, an evidence set measured at some other design
# not being the experiment docs/Multi_Run_Supplement.md documents; --scenario
# still applies. It writes its own transport_capacity_by_fleet_size.csv (or
# that name with `_<scenario>` appended for a non-default --scenario) and
# leaves the forward ICU share sweep's files in the same directory untouched.
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
  make_option("--scenario",   type = "character", default = "default",
              help = "Scenario profile to run the sweep under [default: %default]"),
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
  make_option("--images-dir", type = "character", default = NULL,
              help = paste("Directory for the saved plot [default: outputs/images,",
                           "or images under --refresh-baseline]")),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/sweeps/ and images/ copies")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (opt$quick && isTRUE(opt$`refresh-baseline`)) {
  stop("--quick and --refresh-baseline are incompatible: a smoke test's two ",
       "replications over three days are not the experiment the tracked ",
       "evidence set records.", call. = FALSE)
}

if (opt$quick) {
  opt$iterations <- 2L
  opt$days       <- 3L
  message("Quick mode: iterations=2, days=3")
}

# A baseline refresh runs the protocol R/analysis.R holds rather than whatever
# the caller passed, so the tracked set and the design the supplement documents
# cannot diverge through a mistyped argument.
if (isTRUE(opt$`refresh-baseline`)) {
  opt$pmvamb     <- deparse(TRANSPORT_SWEEP_PMVAMB)
  opt$hx240m     <- deparse(TRANSPORT_SWEEP_HX240M)
  opt$iterations <- TRANSPORT_SWEEP_REPLICATIONS
  opt$days       <- TRANSPORT_SWEEP_DAYS
  opt$seed       <- CAPACITY_SWEEP_SEED
  message(sprintf(paste("Baseline refresh: running the documented protocol,",
                        "%d replications x %d days per point at seed %d"),
                  TRANSPORT_SWEEP_REPLICATIONS, TRANSPORT_SWEEP_DAYS,
                  CAPACITY_SWEEP_SEED))
}

output_dir <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "sweeps")
} else {
  opt[["output-dir"]]
}

images_dir <- if (!is.null(opt[["images-dir"]])) {
  opt[["images-dir"]]
} else if (isTRUE(opt$`refresh-baseline`)) {
  "images"
} else {
  file.path("outputs", "images")
}

fleet_sizes <- list(
  PMVAmb = eval(parse(text = opt$pmvamb)),
  HX240M = eval(parse(text = opt$hx240m))
)

message(sprintf(
  "Transport fleet-size sweep config: PMVAmb=%s, HX240M=%s, iterations=%d, days=%d, seed=%d, scenario=%s",
  opt$pmvamb, opt$hx240m, opt$iterations, opt$days, opt$seed, opt$scenario
))

# plot_transport_capacity_margin_by_fleet_size() saves/restores the global
# env_data/day_min/counts around its sweep (mirrors run_morris()'s
# env_data_base pattern, R/sensitivity.R), so they must already be set —
# same convention as scripts/run_sensitivity.R.
env_data <<- load_elms(opt$path)
day_min  <<- DAY_MIN
counts   <<- sapply(env_data$elms, length)

set.seed(opt$seed)
sweep <- plot_transport_capacity_margin_by_fleet_size(
  fleet_sizes = fleet_sizes,
  scenario    = opt$scenario,
  n_days      = opt$days,
  n_rep       = opt$iterations,
  path        = opt$path,
  output_dir  = output_dir,
  images_dir  = images_dir
)

message("\nTransport fleet-size sweep complete.")
print(sweep$data)
