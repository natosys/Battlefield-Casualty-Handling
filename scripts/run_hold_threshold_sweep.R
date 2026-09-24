#!/usr/bin/env Rscript
##################################################
## scripts/run_hold_threshold_sweep.R           ##
## R2B holding capacity x evacuation threshold  ##
##################################################
#
# Terminal / Claude Code cloud:
#   Rscript scripts/run_hold_threshold_sweep.R --refresh-baseline   # write the tracked data/sweeps/
#   Rscript scripts/run_hold_threshold_sweep.R                      # default: 5/7/10 beds x 0/1/3/5/7 day thresholds, 10 reps x 30 days
#   Rscript scripts/run_hold_threshold_sweep.R --beds "c(5, 10)" --thresholds-days "c(0, 2, 5)"
#   Rscript scripts/run_hold_threshold_sweep.R --iterations 20 --days 30
#   Rscript scripts/run_hold_threshold_sweep.R --quick               # smoke test (2 reps, 3 days, 2x2 grid)
#
# Why this exists. docs/Multi_Run_Analysis.md's Option 2 names the R2B
# holding shortfall (README Further Development L4, about 15.5 beds expected
# against ten fielded) and names two untested remedies that are substitutes
# for each other: adding holding beds, and an evacuation threshold that moves
# a casualty whose expected convalescence exceeds a set duration rearward
# rather than occupying a forward bed. This script sweeps the two jointly,
# following the shape scripts/run_transport_sweep.R and
# scripts/run_icu_share_sweep.R already use for a single-axis capacity sweep,
# and reports both the forward pool the levers act on and the two pools
# (R2E holding, R2E intensive care) the transferred load lands on.
#
# --refresh-baseline is the only way to write the tracked data/sweeps/ and the
# tracked images/r2b_hold_threshold_sweep.png. Without it every invocation
# writes under outputs/ alone, so an exploratory run cannot move the evidence
# set docs/Multi_Run_Analysis.md's table is checked against. The flag fixes
# the swept grid, the replication count, the horizon and the seed rather than
# accepting whichever the caller passed. It writes its own
# r2b_hold_threshold_sweep.csv and leaves the other sweeps' files in the same
# directory untouched.
#
# RStudio Console (interactive):
#   source("R/environment.R"); source("R/trajectories.R"); source("R/replication.R")
#   source("R/analysis.R"); source("R/scenario_runner.R")
#   sweep <- plot_r2b_hold_threshold_sweep(hold_beds = c(5, 7, 10),
#              evac_threshold_min = c(0, 1440, 4320, 7200, 10080), n_rep = 10, n_days = 30)

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/scenario_runner.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--beds",            type = "character",
              default = deparse(HOLD_THRESHOLD_SWEEP_BEDS),
              help = "R2B holding beds per unit to sweep, as an R vector expression [default: %default]"),
  make_option("--thresholds-days", type = "character",
              default = deparse(HOLD_THRESHOLD_SWEEP_MINUTES / DAY_MIN),
              help = paste("Evacuation thresholds to sweep, in days, as an R vector",
                           "expression; 0 disables the threshold [default: %default]")),
  make_option("--iterations",      type = "integer", default = HOLD_THRESHOLD_SWEEP_REPLICATIONS,
              help = "Replications per grid point [default: %default]"),
  make_option("--days",            type = "integer", default = CAPACITY_SWEEP_DAYS,
              help = "Simulation duration in days [default: %default]"),
  make_option("--seed",            type = "integer", default = CAPACITY_SWEEP_SEED,
              help = "Random seed [default: %default]"),
  make_option("--quick",           action = "store_true", default = FALSE,
              help = "Smoke test: 2 iterations, 3 days, a 2x2 grid"),
  make_option("--path",            type = "character", default = "env_data.json",
              help = "Path to env_data.json [default: %default]"),
  make_option("--output-dir",      type = "character", default = "outputs",
              help = "Directory for CSV output [default: %default]"),
  make_option("--images-dir",      type = "character", default = NULL,
              help = paste("Directory for the saved plot [default: outputs/images,",
                           "or images under --refresh-baseline]")),
  make_option("--max-cores",       type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
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
  opt$iterations      <- 2L
  opt$days            <- 3L
  opt$beds            <- "c(5, 10)"
  opt$`thresholds-days` <- "c(0, 5)"
  message("Quick mode: iterations=2, days=3, beds=c(5, 10), thresholds-days=c(0, 5)")
}

# A baseline refresh runs the protocol R/analysis.R holds rather than whatever
# the caller passed, so the tracked set and the design the supplement documents
# cannot diverge through a mistyped argument.
if (isTRUE(opt$`refresh-baseline`)) {
  opt$beds               <- deparse(HOLD_THRESHOLD_SWEEP_BEDS)
  opt$`thresholds-days`  <- deparse(HOLD_THRESHOLD_SWEEP_MINUTES / DAY_MIN)
  opt$iterations         <- HOLD_THRESHOLD_SWEEP_REPLICATIONS
  opt$days               <- CAPACITY_SWEEP_DAYS
  opt$seed               <- CAPACITY_SWEEP_SEED
  message(sprintf(paste("Baseline refresh: running the documented protocol,",
                        "%d replications x %d days per grid point at seed %d"),
                  HOLD_THRESHOLD_SWEEP_REPLICATIONS, CAPACITY_SWEEP_DAYS,
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

hold_beds           <- eval(parse(text = opt$beds))
thresholds_days      <- eval(parse(text = opt$`thresholds-days`))
evac_threshold_min  <- thresholds_days * DAY_MIN

if (any(thresholds_days < 0)) {
  stop("--thresholds-days must be non-negative: 0 disables the evacuation threshold.",
       call. = FALSE)
}

message(sprintf(
  "R2B holding threshold sweep config: beds=%s, thresholds(days)=%s, iterations=%d, days=%d, seed=%d",
  opt$beds, opt$`thresholds-days`, opt$iterations, opt$days, opt$seed
))

# plot_r2b_hold_threshold_sweep() saves/restores the global env_data/day_min/
# counts around its sweep (mirrors run_morris()'s env_data_base pattern,
# R/sensitivity.R), so they must already be set — same convention as
# scripts/run_transport_sweep.R and scripts/run_icu_share_sweep.R.
env_data <<- load_elms(opt$path)
day_min  <<- DAY_MIN
counts   <<- sapply(env_data$elms, length)

set.seed(opt$seed)
sweep <- plot_r2b_hold_threshold_sweep(
  hold_beds           = hold_beds,
  evac_threshold_min  = evac_threshold_min,
  n_days              = opt$days,
  n_rep               = opt$iterations,
  path                = opt$path,
  output_dir          = output_dir,
  images_dir          = images_dir,
  max_cores           = opt$`max-cores`
)

message("\nR2B holding threshold sweep complete.")
print(sweep$data)
