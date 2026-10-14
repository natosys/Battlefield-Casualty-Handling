#!/usr/bin/env Rscript
##############################################################################
## scripts/run_sobol_360.R                                                  ##
## Sobol decomposition of the 360-day Morris leaders                        ##
##############################################################################
#
# Usage:
#   BCH_POINT_WORKERS=4 Rscript scripts/run_sobol_360.R [--n-sobol 96] [--reps 4]
#     [--output-dir outputs/sobol_360] [--cache-dir outputs/cache/sobol_360]
#
# Decomposes the leading cluster of the 360-day Morris screen
# (data/sensitivity/morris_r20_d360) over the 360-day sustained horizon. The
# selection rule: the parameters ranked 1 to 7 on the R2E OT queue (mu* of 13.0
# and above, the cluster the screen resolves from the tail), plus the leading
# parameter on the surgical delivery response, which the queue ranking places
# tenth; a composition group enters whole through the Dirichlet option. Sized
# below the earlier 30-day N = 800 because a 360-day point costs about two
# orders of magnitude more compute, so N is the largest the budget allowed.
# The screens write to outputs/ and cannot disturb the tracked evidence set.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/sensitivity.R")
suppressPackageStartupMessages(library(optparse))

opt <- parse_args(OptionParser(option_list = list(
  make_option("--n-sobol", type = "integer", default = 96L),
  make_option("--reps", type = "integer", default = 4L),
  make_option("--days", type = "integer", default = 360L),
  make_option("--seed", type = "integer", default = 42L),
  make_option("--nboot", type = "integer", default = 1000L),
  make_option("--output-dir", type = "character", default = "outputs/sobol_360"),
  make_option("--cache-dir", type = "character", default = "outputs/cache/sobol_360")
)))

set.seed(opt$seed)
env_data <<- load_elms("env_data.json")
day_min  <<- DAY_MIN
counts   <<- sapply(env_data$elms, length)

TOP_PARAMS <- c(
  "casualty_surge_rate", "pri1_dcs_rate", "pri1_surg_prob", "wia_spt_mean",
  "r2e_dcs2_factor", "wia_cbt_mean", "triage_p1_balance", "pri1_evac_prob"
)

message(sprintf("Sobol 360-day run: N=%d, %d reps, %d days, params: %s",
                opt[["n-sobol"]], opt$reps, opt$days, paste(TOP_PARAMS, collapse = ", ")))

run_sobol(
  top_params = TOP_PARAMS,
  n_days     = opt$days,
  n_rep      = opt$reps,
  n_sobol    = opt[["n-sobol"]],
  output_dir = opt[["output-dir"]],
  dirichlet  = TRUE,
  cache_dir  = opt[["cache-dir"]],
  nboot      = opt$nboot,
  crn_seed   = NULL
)

message("SOBOL_360_COMPLETE")
