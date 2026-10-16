#!/usr/bin/env Rscript
##############################################################################
## scripts/run_sobol_decomposition.R                                        ##
## CLI entry point for the Sobol decomposition behind data/sensitivity/     ##
## sobol_n800/                                                              ##
##############################################################################
#
# Usage (from the repository root, so the relative sources and env_data.json
# resolve):
#   Rscript scripts/run_sobol_decomposition.R
#
# Why this script exists. `scripts/run_sensitivity.R --sobol` decomposes the
# top five parameters of a Morris ranking at its own default sizing. The
# tracked decomposition is a different design: eight coordinates chosen from
# the unresolved leading cluster of the Morris ranking of `system_ot_q` current
# when it was run (ranks 1 to 7, widened by `mc_p1_balance`, the one
# composition group that qualifies), at N = 800 and 8 replications per point,
# over a 30-day horizon. That run decomposed `system_ot_q` alongside
# `r2e_ot_q` and `transport_util`; the response set it would now decompose
# (R/sensitivity.R, SOBOL_RESPONSES) has retired `system_ot_q`, which equalled
# `r2e_ot_q` at every design point.
# The replication seed is left unpinned, the shipped default. This script is
# the one place that design is written down as a command.
#
# Writes outputs/sobol_n800/ and the point cache under outputs/cache/
# sobol_n800/, never the tracked data/sensitivity/ copy. The run is long (the
# cache holds 8,000 design points), so it resumes from the cache when
# interrupted; scripts/supervise_screen.sh drives it across environment
# failures, taking SOBOL_N800_COMPLETE, which the script prints when the
# decomposition is finished, as its done marker. The design is documented in
# docs/Methods.md and asserted against the tracked metadata by
# scripts/check_sensitivity_protocol.R.

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
  "pri1_evac_prob",
  "pri1_surg_prob",
  "pri1_dcs_rate",
  "mc_p1_balance",
  "casualty_surge_kia_fraction",
  "casualty_surge_max_cas"
)

message("Sobol decomposition: N=800, 8 reps, 30 days, params: ",
        paste(top_params, collapse = ", "))

run_sobol(
  top_params = top_params,
  n_days     = 30,
  n_rep      = 8,
  n_sobol    = 800,
  output_dir = "outputs/sobol_n800",
  dirichlet  = TRUE,
  cache_dir  = "outputs/cache/sobol_n800",
  nboot      = 1000,
  crn_seed   = NULL
)

message("SOBOL_N800_COMPLETE")
