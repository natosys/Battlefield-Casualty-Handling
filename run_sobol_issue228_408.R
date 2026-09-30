#!/usr/bin/env Rscript
## Ad hoc driver for the joint Issue #408 / #228 Sobol decomposition.
## Run from the repo root so the relative sources and env_data.json resolve.
##
## Parameter selection (Issue #408): the unresolved leading cluster on the
## current (post-#410) Morris ranking of system_ot_q (ranks 1-7, mu* 13.04
## down to 3.42), widened to include mc_p1_balance, the one composition
## group that qualifies under the current ranking.
##
## Sizing (Issue #228): N = 800, 8 replications per point, dropping
## transport_q and r2b_ot_q from the response set (R/sensitivity.R,
## SOBOL_RESPONSES), keeping transport_util. crn-seed left unset.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/sensitivity.R")

set.seed(42)
env_data <<- load_elms("env_data.json")
day_min  <<- DAY_MIN
counts   <<- sapply(env_data$elms, length)

top_params <- c(
  "mass_casualty_rate",
  "pri1_evac_prob",
  "pri1_surg_prob",
  "pri1_dcs_rate",
  "mc_p1_balance",
  "mass_casualty_kia_fraction",
  "mass_casualty_max_cas"
)

message("Issue #408/#228 joint Sobol run: N=800, 8 reps, 30 days, params: ",
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

message("SOBOL_N800_ISSUE228_408_COMPLETE")
