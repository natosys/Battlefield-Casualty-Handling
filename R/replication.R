##############################################
## R/replication.R                          ##
## Simulation execution wrapper             ##
##############################################

# NOTE: This is a single-run stub. Issue #1 will replace this with an
# mclapply replication wrapper using wrap() for monitoring aggregation.

library(simmer)

#' Runs a single simulation replication and returns monitoring data
#'
#' @param env A fully configured simmer environment (resources and generators added)
#' @param n_days Number of simulation days to run
#' @return Named list with elements: arrivals, attributes, resources
#'
#' @details Executes one simmer run and captures arrival, attribute, and
#'   resource monitoring data. Returned objects are data frames suitable
#'   for passing directly to analyse_run().
run_single <- function(env, n_days) {
  day_min <- 1440L

  env %>% run(until = n_days * day_min)

  list(
    arrivals   = get_mon_arrivals(env, ongoing = TRUE),
    attributes = get_mon_attributes(env),
    resources  = get_mon_resources(env)
  )
}
