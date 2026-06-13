##############################################
## R/replication.R                          ##
## Simulation execution and replication     ##
##############################################

library(simmer)
library(parallel)

# ── Single simulation build + run ─────────────────────────────────────────────

#' Build and run one complete simulation replication
#'
#' @param n_days Simulation duration in days
#' @param seed   Random seed (NULL = random, for independent replications)
#' @param write_files Write arrival data to data/ directory (TRUE for single-run
#'   diagnostics; FALSE for parallel replication workers)
#' @return A wrapped simmer environment (use get_mon_*() on a list of these)
#'
#' @details Sets env globally (<<-) so trajectory closures can resolve it.
#'   In forked mclapply workers, <<- modifies only the fork's global state.
run_once <- function(n_days, seed = NULL, write_files = FALSE) {
  if (!is.null(seed)) set.seed(seed)

  env <<- simmer("Battlefield Casualty Handling")
  env <<- build_env(env, env_data)
  casualty <- build_casualty_trajectory()

  env <<- env %>%
    add_generator("wia_cbt",  casualty,
                  at(generate_ln_arrivals("wia_cbt",
                    env_data$vars$generators$wia_cbt$mean_daily,
                    env_data$vars$generators$wia_cbt$sd_daily,
                    env_data$pops$combat, n_days, write_file = write_files)), mon = 2) %>%
    add_generator("kia_cbt",  casualty,
                  at(generate_ln_arrivals("kia_cbt",
                    env_data$vars$generators$kia_cbt$mean_daily,
                    env_data$vars$generators$kia_cbt$sd_daily,
                    env_data$pops$combat, n_days, write_file = write_files)), mon = 2) %>%
    add_generator("dnbi_cbt", casualty,
                  at(generate_ln_arrivals("dnbi_cbt",
                    env_data$vars$generators$dnbi_cbt$mean_daily,
                    env_data$vars$generators$dnbi_cbt$sd_daily,
                    env_data$pops$combat, n_days, write_file = write_files)), mon = 2) %>%
    add_generator("wia_spt",  casualty,
                  at(generate_ln_arrivals("wia_spt",
                    env_data$vars$generators$wia_spt$mean_daily,
                    env_data$vars$generators$wia_spt$sd_daily,
                    env_data$pops$support, n_days, write_file = write_files)), mon = 2) %>%
    add_generator("kia_spt",  casualty,
                  at(generate_ln_arrivals("kia_spt",
                    env_data$vars$generators$kia_spt$mean_daily,
                    env_data$vars$generators$kia_spt$sd_daily,
                    env_data$pops$support, n_days, write_file = write_files)), mon = 2) %>%
    add_generator("dnbi_spt", casualty,
                  at(generate_ln_arrivals("dnbi_spt",
                    env_data$vars$generators$dnbi_spt$mean_daily,
                    env_data$vars$generators$dnbi_spt$sd_daily,
                    env_data$pops$support, n_days, write_file = write_files)), mon = 2) %>%
    add_global("evac_wait_count", 0)

  env %>% run(until = n_days * day_min)
  wrap(env)
}

# ── Multi-replication framework ───────────────────────────────────────────────

#' Run n_iterations independent replications using mclapply
#'
#' @param n_iterations Number of replications
#' @param n_days       Simulation duration in days
#' @return Named list with elements: arrivals, attributes, resources.
#'   Each data frame includes a 'replication' column (1..n_iterations).
#'
#' @details Uses mclapply on POSIX systems (Linux/macOS) and falls back to
#'   lapply on Windows. Each worker calls run_once() with seed = NULL so
#'   replications are statistically independent.
run_replications <- function(n_iterations, n_days) {
  message(sprintf("Running %d replications (%d days each)...", n_iterations, n_days))

  worker <- function(i) run_once(n_days, seed = NULL, write_files = FALSE)

  use_parallel <- .Platform$OS.type != "windows" && n_iterations > 1
  if (use_parallel) {
    envs <- mclapply(seq_len(n_iterations), worker, mc.set.seed = FALSE)
  } else {
    envs <- lapply(seq_len(n_iterations), worker)
  }

  list(
    arrivals   = get_mon_arrivals(envs, ongoing = TRUE),
    attributes = get_mon_attributes(envs),
    resources  = get_mon_resources(envs)
  )
}

# ── KPI summary ───────────────────────────────────────────────────────────────

#' Compute replication-level KPI summary for resource queues
#'
#' @param mon Named list with 'resources' element as returned by run_replications()
#' @return Data frame with one row per resource: mean, p10, p90, max queue,
#'   and 95% CI bounds, sorted descending by mean queue length
#'
#' @details Uses time-weighted mean queue per replication as the unit of
#'   analysis, then summarises across replications. 95% CI uses the t-distribution.
summarise_replications <- function(mon) {
  rep_means <- mon$resources %>%
    group_by(replication, resource) %>%
    arrange(time) %>%
    mutate(dt = lead(time, default = max(time)) - time) %>%
    summarise(
      rep_mean_q = weighted.mean(queue, w = pmax(dt, 0), na.rm = TRUE),
      rep_max_q  = max(queue, na.rm = TRUE),
      .groups    = "drop"
    )

  rep_means %>%
    group_by(resource) %>%
    summarise(
      n_reps   = n(),
      mean_q   = mean(rep_mean_q),
      sd_q     = sd(rep_mean_q),
      p10_q    = quantile(rep_mean_q, 0.10, na.rm = TRUE),
      p90_q    = quantile(rep_mean_q, 0.90, na.rm = TRUE),
      max_q    = max(rep_max_q),
      ci_lower = mean_q - qt(0.975, df = n() - 1) * sd_q / sqrt(n()),
      ci_upper = mean_q + qt(0.975, df = n() - 1) * sd_q / sqrt(n()),
      .groups  = "drop"
    ) %>%
    arrange(desc(mean_q))
}

# ── Backwards-compat shim ─────────────────────────────────────────────────────

#' Single-run shim retained for backwards compatibility
#'
#' @param env A fully configured simmer environment (already has generators)
#' @param n_days Number of simulation days to run
#' @return Named list: arrivals, attributes, resources
run_single <- function(env, n_days) {
  env %>% run(until = n_days * day_min)
  list(
    arrivals   = get_mon_arrivals(env, ongoing = TRUE),
    attributes = get_mon_attributes(env),
    resources  = get_mon_resources(env)
  )
}
