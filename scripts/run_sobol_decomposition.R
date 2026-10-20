#!/usr/bin/env Rscript
##############################################################################
## scripts/run_sobol_decomposition.R                                        ##
## CLI entry point for the Sobol decomposition behind data/sensitivity/     ##
## sobol_n96_d360/                                                          ##
##############################################################################
#
# Usage (from the repository root, so the relative sources and env_data.json
# resolve):
#   Rscript scripts/run_sobol_decomposition.R
#
# Why this script exists. `scripts/run_sensitivity.R --sobol` decomposes the
# top five parameters of a Morris ranking at its own default sizing. The
# tracked decomposition is a different design: the leading cluster of the
# 360-day Morris ranking of `r2e_ot_q` (data/sensitivity/morris_r20_d360,
# ranks 1 to 7, mu* 48.6 down to 12.5), with `triage_p2_p3_balance` joining
# because `triage_p1_balance` carries the triage composition in whole, plus
# `pri1_evac_prob`, which leads the surgical delivery ranking and stands tenth
# on the queue: nine coordinates at N = 96 and 4 replications per point over
# the 360-day horizon, evaluated four points at a time with each point seeded
# from its index (R/sensitivity.R, evaluate_design_concurrently()). N is the
# largest the compute allowed, a 360-day point costing about a hundred times
# a 30-day one; it is not sized from scripts/test_sobol_separation.R. It
# decomposes every response in SOBOL_RESPONSES. The replication
# seed is left unpinned, the shipped default. This script is the one place
# the design is written down as a command.
#
# Writes outputs/sobol_n96_d360/ and the point cache under outputs/cache/
# sobol_n96_d360/, never the tracked data/sensitivity/ copy. The run is long
# (1,056 design points, about seventy hours on four cores), so it resumes from
# the cache when interrupted; scripts/supervise_screen.sh drives it across
# environment failures, taking SOBOL_N96_D360_COMPLETE, which the script
# prints when the decomposition is finished, as its done marker. The design
# is documented in docs/Methods.md and asserted against the tracked metadata
# by tests/checks/check_sensitivity_protocol.R.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/sensitivity.R")

set.seed(42)
# The four globals the execution model requires, bound in the global
# environment so that the lint ratchet's `<<-` count does not rise.
elms_config <- load_elms("env_data.json")
assign("env_data", elms_config, envir = globalenv())
assign("day_min", DAY_MIN, envir = globalenv())
assign("counts", sapply(elms_config$elms, length), envir = globalenv())

top_params <- c(
  "casualty_surge_rate",
  "pri1_dcs_rate",
  "pri1_surg_prob",
  "wia_spt_mean",
  "r2e_dcs2_factor",
  "wia_cbt_mean",
  "triage_p1_balance",
  "pri1_evac_prob"
)

message("Sobol decomposition: N=96, 4 reps, 360 days, 4 point workers, params: ",
        paste(top_params, collapse = ", "))

run_sobol(
  top_params    = top_params,
  n_days        = 360,
  n_rep         = 4,
  n_sobol       = 96,
  output_dir    = "outputs/sobol_n96_d360",
  dirichlet     = TRUE,
  cache_dir     = "outputs/cache/sobol_n96_d360",
  nboot         = 1000,
  crn_seed      = NULL,
  point_workers = 4L
)

message("SOBOL_N96_D360_COMPLETE")
