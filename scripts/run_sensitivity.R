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
#   Rscript scripts/run_sensitivity.R --r 5              # reduced-r full-coverage run (see below)
#
# RStudio Console (interactive):
#   source("R/sensitivity.R")                            # loads helpers only
#   mr <- run_morris(r=3, n_rep=3, n_days=5)            # smoke test
#   run_sobol(mr$ranking$parameter[1:5])                 # Sobol on top 5
#
# morris_params (R/sensitivity.R) covers 55 parameters as of Issue #112 (up
# from 11); r=20 at this parameter count is r*(p+1) = 1,120 design points x
# 5 reps = 5,600 simulation runs, impractical outside a long-lived compute
# session. --r 5 was used for the Issue #112 re-run documented in the
# README (280 design points x 5 reps = 1,400 runs, ~20s/run on 4 cores);
# scale --r up when a longer session is available.

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
              help = "Directory for CSV and PNG outputs [default: %default]"),
  make_option("--max-cores",  type = "integer", default = NULL,
              help = paste(
                "Cap mclapply's mc.cores per design-point evaluation. A random",
                "OAT trajectory start can land on several elevated",
                "casualty-generation-rate parameters simultaneously (Issue #112),",
                "producing a 30-day run with far more casualties/events than the",
                "baseline; at full core parallelism this can exceed the",
                "container's memory and get a worker OOM-killed (see",
                "run_replications()'s own comment on this, R/replication.R) —",
                "a partial-replication warning if some workers survive, or a",
                "hard error (caught by run_morris()'s tryCatch as an NA design",
                "point) if all do. NULL (default) preserves prior behaviour",
                "(uses all detected cores)."
              ))
)

opt <- parse_args(OptionParser(option_list = option_list))

if (opt$quick) {
  opt$r    <- 3L
  opt$reps <- 3L
  opt$days <- 5L
  message("Quick mode: r=3, reps=3, days=5")
}

message(sprintf(
  "Sensitivity config: r=%d, reps=%d, days=%d, levels=%d, seed=%d, max_cores=%s",
  opt$r, opt$reps, opt$days, opt$levels, opt$seed,
  if (is.null(opt[["max-cores"]])) "all" else opt[["max-cores"]]
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
  output_dir = opt[["output-dir"]],
  max_cores  = opt[["max-cores"]]
)

n_na <- sum(!complete.cases(morris_result$Y))
if (n_na > 0) {
  warning(sprintf(
    paste0(
      "%d of %d design points have at least one NA/failed KPI (see the ",
      "'X of Y replications did not complete' warning from run_replications(), ",
      "R/replication.R, likely an OOM-killed mclapply worker at this ",
      "parameter count — try a lower --max-cores). Elementary effects for ",
      "any parameter whose trajectory passes through an affected design ",
      "point will be NA, and sigma_ee may come out NA for some or all ",
      "parameters as a result — inspect outputs/morris_ranking.csv before ",
      "treating this run as authoritative."
    ),
    n_na, nrow(morris_result$Y)
  ), call. = FALSE)
}

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
