##############################################
## R/replication.R                          ##
## Simulation execution and replication     ##
##############################################

library(simmer)
library(parallel)

# ── Single simulation build + run ─────────────────────────────────────────────

#' Build and run one complete simulation replication
#'
#' @param n_days      Simulation duration in days
#' @param seed        Random seed (NULL = random, for independent replications)
#' @param write_files Write arrival data to data/ directory (TRUE for single-run
#'   diagnostics; FALSE for parallel replication workers)
#' @param ot_hours    Hours per day the first OT shift is active (default 12).
#'   Passed to build_env(); used by sensitivity screening to vary OT availability.
#' @param antithetic  Logical; when TRUE antithetic arrival variates are used
#'   (U' = 1 - U in both rate draws and within-minute jitter). Set TRUE for
#'   even-indexed replications in run_replications() antithetic pairing.
#' @return A wrapped simmer environment (use get_mon_*() on a list of these)
#'
#' @details Sets env globally (<<-) so trajectory closures can resolve it.
#'   In forked mclapply workers, <<- modifies only the fork's global state.
run_once <- function(n_days, seed = NULL, write_files = FALSE, ot_hours = 12,
                     antithetic = FALSE) {
  if (!is.null(seed)) set.seed(seed)

  env <<- simmer("Battlefield Casualty Handling")
  env <<- build_env(env, env_data, ot_hours = ot_hours)
  casualty <- build_casualty_trajectory()

  # Force regeneration feedback loop (Issue #18): the six background
  # casualty streams are live, force-size-reactive generator closures
  # (R/environment.R) rather than pre-computed at() vectors, reading the
  # effective_force_combat/effective_force_support globals initialised
  # below — those globals are debited at each casualty's injury_time and
  # credited at each RTD event (R/trajectories.R), plus a periodic
  # reinforcement trajectory, closing the loop the old fixed-population
  # generator could not represent.
  #
  # Mass casualty injection (Issue #9) stays exogenous/pre-computed (a
  # compound Poisson process, or in "scheduled" mode a planner-specified
  # day/probability list) — it is not population-scaled. wrap_with_mass_casualty()
  # interleaves it into the wia_cbt closure in true chronological order and
  # builds wia_cbt_mass_casualty_event_id incrementally (0 = background) as
  # entities are actually emitted, which build_casualty_trajectory() reads
  # via the entity's generator-assigned index to set the
  # mass_casualty_event/mass_casualty_event_id attributes and to look up
  # that event's own priority split (scheduled mode only; NA pri_one for
  # poisson-mode events falls back to the shared
  # env_data$vars$mass_casualty$priority split — see mass_casualty_event_priority_table).
  # Global assignment (<<-) mirrors env_data/day_min/counts (run.R); in
  # forked mclapply workers this modifies only the fork's local state.
  mass_casualty <- generate_mass_casualty_events(n_days,
                      env_data$vars$mass_casualty, write_file = write_files,
                      antithetic = antithetic)
  wia_cbt_mass_casualty_event_id <<- integer(0)
  mass_casualty_event_priority_table <<- mass_casualty$events

  wia_cbt_gen <- wrap_with_mass_casualty(
    generate_casualty_arrivals(env_data$vars$generators$wia_cbt,
                               "effective_force_combat", n_days, antithetic = antithetic),
    mass_casualty$arrival_times, mass_casualty$casualty_event_id)

  env <<- env %>%
    add_global("effective_force_combat", env_data$pops$combat) %>%
    add_global("effective_force_support", env_data$pops$support) %>%
    add_generator("wia_cbt",  casualty, wia_cbt_gen, mon = 2) %>%
    add_generator("kia_cbt",  casualty,
                  generate_casualty_arrivals(env_data$vars$generators$kia_cbt,
                    "effective_force_combat", n_days, antithetic = antithetic), mon = 2) %>%
    add_generator("dnbi_cbt", casualty,
                  generate_casualty_arrivals(env_data$vars$generators$dnbi_cbt,
                    "effective_force_combat", n_days, antithetic = antithetic), mon = 2) %>%
    add_generator("wia_spt",  casualty,
                  generate_casualty_arrivals(env_data$vars$generators$wia_spt,
                    "effective_force_support", n_days, antithetic = antithetic), mon = 2) %>%
    add_generator("kia_spt",  casualty,
                  generate_casualty_arrivals(env_data$vars$generators$kia_spt,
                    "effective_force_support", n_days, antithetic = antithetic), mon = 2) %>%
    add_generator("dnbi_spt", casualty,
                  generate_casualty_arrivals(env_data$vars$generators$dnbi_spt,
                    "effective_force_support", n_days, antithetic = antithetic), mon = 2) %>%
    add_global("evac_wait_count", 0)

  # Reinforcement demand cycle (Issue #18 follow-up): only scheduled when
  # demand_interval_days > 0, so the shipped disabled default consumes no
  # RNG draws and adds no generator at all, matching the mass-casualty
  # rate_per_day = 0 disable-path convention elsewhere in this file. First
  # demand fires at day `demand_interval_days`, not day 0 — a pool starts
  # at full strength, so an immediate submission would have zero demand.
  demand_interval <- env_data$vars$force_regeneration$reinforcement$demand_interval_days
  if (!is.null(demand_interval) && demand_interval > 0) {
    env <<- env %>%
      add_generator("force_reinforcement", build_reinforcement_trajectory(),
                    at(seq(demand_interval * day_min, n_days * day_min, by = demand_interval * day_min)),
                    mon = 0)
  }

  # Strategic AME resources (Issue #23 follow-up): two theatre-wide
  # resources seized only from the R2E Heavy Strategic Evac disposition
  # (R/trajectories.R) — "ame" (standard, Casualty Staging Unit-equivalent
  # Hold-bed evacuees) and "ame_critical" (CCATT/CCAST-supported, ICU-bed
  # evacuees), both fed by the same sortie schedule (a CCATT/CCAST team
  # augments the standard crew rather than flying separately — see
  # build_ame_sortie_trajectory(), R/trajectories.R). Both start at zero
  # capacity — they always exist (any casualty reaching Strategic Evac
  # unconditionally tries to seize one of them, so neither can be
  # conditionally absent the way mass casualty injection or reinforcement
  # are), but capacity is only ever added to by the periodic AME sortie
  # generator below. No generator is added at all — so AME never opens and
  # every strategic evacuee queues indefinitely — when
  # schedule_interval_days is non-positive (matching reinforcement's
  # disable convention) or exceeds n_days (no scheduled sortie opportunity
  # falls within the run at all; seq()'s from > to would otherwise error
  # rather than silently produce zero opportunities).
  env <<- env %>%
    add_resource("ame", capacity = 0) %>%
    add_resource("ame_critical", capacity = 0)

  ame_params <- env_data$vars$role4$ame
  if (!is.null(ame_params$schedule_interval_days) &&
      ame_params$schedule_interval_days > 0 &&
      ame_params$schedule_interval_days <= n_days) {
    env <<- env %>%
      add_generator("ame_sortie", build_ame_sortie_trajectory(),
                    at(seq(ame_params$schedule_interval_days * day_min, n_days * day_min,
                           by = ame_params$schedule_interval_days * day_min)),
                    mon = 0)
  }

  env %>% run(until = n_days * day_min)

  if (write_files) write_arrival_diagnostics(env)

  wrap(env)
}

# ── Multi-replication framework ───────────────────────────────────────────────

#' Run n_iterations independent replications using mclapply
#'
#' @param n_iterations Number of replications
#' @param n_days       Simulation duration in days
#' @param ot_hours     Hours per day the first OT shift is active (default 12).
#'   Threaded to run_once() → build_env(); used by sensitivity screening.
#' @param progress_dir Optional directory path; when supplied, an empty
#'   marker file ("rep_<i>.done") is written to it as each replication
#'   completes, letting a caller on another process (e.g. the Shiny app's
#'   main session, polling this directory via invalidateLater) observe real
#'   per-replication progress from mclapply's forked workers. NULL (default)
#'   disables this and preserves prior behaviour exactly for existing callers
#'   (run_welch_analysis(), run_scenarios(), eval_params()).
#' @param max_cores Optional integer cap on mclapply's mc.cores. NULL
#'   (default) preserves prior behaviour — getOption("mc.cores",
#'   parallel::detectCores(logical = FALSE)) — for scripted/CLI callers on
#'   a dedicated machine. This respects the mc.cores option the project's
#'   Dockerfile sets in Rprofile.site (physical core count), falling back to
#'   detectCores(logical = FALSE) outside that container. app.R's interactive
#'   (Shiny-triggered) callers pass a small explicit cap instead: even the
#'   physical core count can exceed the memory actually available to a local
#'   dev container, and each mclapply fork carries a full duplicate R
#'   session (simmer + the tidyverse-adjacent packages this codebase loads);
#'   forking one such session per core on an unconstrained core count
#'   has been observed to exhaust a local dev container's memory and
#'   crash it even at a modest replication count (Issue #15 follow-up).
#' @return Named list with elements: arrivals, attributes, resources.
#'   Each data frame includes a 'replication' column (1..n_iterations).
#'
#' @details Antithetic pairing: replications (2k-1, 2k) share a seed so that
#'   run_once() draws uniforms U for the primary and 1-U for the antithetic,
#'   inducing negative arrival-count correlation within each pair. Independence
#'   across pairs is ensured by distinct pair_seeds. RNGkind("L'Ecuyer-CMRG")
#'   is set before mclapply for the global stream; individual pair seeds are
#'   set inside each worker via run_once(seed = ...), overriding the substream
#'   but preserving pair-level independence. Falls back to lapply on Windows.
run_replications <- function(n_iterations, n_days, ot_hours = 12, progress_dir = NULL,
                             max_cores = NULL) {
  message(sprintf("Running %d replications (%d days each)...", n_iterations, n_days))

  # Each pair (2k-1, 2k) shares a seed: primary draws U, antithetic draws 1-U
  # from the same starting RNG state, giving Cor(primary, antithetic) < 0.
  n_pairs    <- ceiling(n_iterations / 2)
  pair_seeds <- sample.int(.Machine$integer.max, n_pairs)

  worker <- function(i) {
    res <- run_once(n_days,
             seed        = pair_seeds[ceiling(i / 2)],
             write_files = FALSE,
             ot_hours    = ot_hours,
             antithetic  = (i %% 2 == 0))
    if (!is.null(progress_dir)) {
      file.create(file.path(progress_dir, sprintf("rep_%d.done", i)))
    }
    res
  }

  use_parallel <- .Platform$OS.type != "windows" && n_iterations > 1
  if (use_parallel) {
    RNGkind("L'Ecuyer-CMRG")
    cores <- getOption("mc.cores", parallel::detectCores(logical = FALSE))
    if (!is.null(max_cores)) cores <- max(1L, min(cores, max_cores))
    envs <- mclapply(seq_len(n_iterations), worker,
                     mc.cores       = cores,
                     mc.set.seed    = TRUE,
                     # Only forced off for max_cores-capped (interactive
                     # Shiny) callers, preserving CLI/script behaviour
                     # exactly otherwise. mc.preschedule = TRUE (mclapply's
                     # default) pre-divides the n_iterations jobs into
                     # `cores` batches, one fork per batch; if a fork is
                     # OOM-killed mid-run (observed on a memory-constrained
                     # local dev container even at cores = 4 — Issue #15
                     # follow-up), mclapply's own warning is explicit that
                     # *every* job pre-assigned to that fork is lost, not
                     # just one. mc.preschedule = FALSE forks one process
                     # per job instead (still capped at `cores` concurrent),
                     # so a single killed fork costs exactly one
                     # replication's result, not an unpredictable batch of
                     # them — smaller, more diagnosable blast radius at the
                     # cost of more fork() calls.
                     mc.preschedule = is.null(max_cores))
  } else {
    envs <- lapply(seq_len(n_iterations), worker)
  }

  # A forked mclapply worker that is killed outright (e.g. OOM-killed by the
  # host/container) — as opposed to one whose R code merely throws a normal
  # error, which mclapply already converts into a "try-error" per job — can
  # leave `envs` containing NULL or malformed entries for every job it had
  # been assigned. Passing those straight to get_mon_arrivals()/
  # get_mon_attributes()/get_mon_resources() produces a confusing, unrelated
  # error deep in simmer's/dplyr's internals (observed: "argument \"x\" is
  # missing, with no default") rather than a clear diagnosis. Filter them
  # out here and fail (or warn) with an explicit, actionable message instead.
  is_valid_env <- function(e) !is.null(e) && !inherits(e, "try-error") && is.environment(e)
  valid <- vapply(envs, is_valid_env, logical(1))
  n_failed <- sum(!valid)
  if (n_failed > 0) {
    msg <- sprintf(
      paste0("%d of %d replications did not complete (their worker process was likely ",
             "killed by the host/container running out of memory). Reduce the replication ",
             "count, reduce simulation duration, or reduce available parallelism (max_cores) ",
             "and try again."),
      n_failed, n_iterations
    )
    if (all(!valid)) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
    envs <- envs[valid]
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
#' @param warm_up_days Days to exclude from the start of each replication
#'   (Welch warm-up period; default 0 = no exclusion)
#' @return Data frame with one row per resource: mean, p10, p90, max queue,
#'   and 95% CI bounds, sorted descending by mean queue length
#'
#' @details Uses time-weighted mean queue per replication as the unit of
#'   analysis, then summarises across replications. 95% CI uses the t-distribution.
#'   Under antithetic pairing, replications are correlated within pairs but the
#'   summary statistics remain valid estimators of the population mean and variance.
summarise_replications <- function(mon, warm_up_days = 0) {
  warm_up_min <- as.integer(warm_up_days) * 1440L

  rep_means <- mon$resources %>%
    filter(time >= warm_up_min) %>%
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
