##############################################################################
## R/mass_casualty.R                                                        ##
## The mass casualty event stress test, at its two arms                    ##
##############################################################################
#
# mass_casualty.event.rate_per_day lets a compound-Poisson surge of casualties
# be injected on top of the ordinary arrival streams. This module measures
# what that surge costs: how it moves total casualty volume, and whether
# casualties it injects, and casualties from the background streams running
# alongside it, die of wounds at a different rate from a campaign with no
# injection at all.
#
# The two arms are not the same realisation, on the same grounds as
# R/hold_window.R's: introducing an event shifts simmer's single global random
# stream from the first draw onward, so pairing on a common control seed
# removes none of the between-run variance that follows. Each replication is
# reduced to one row of the response set inside the forked worker that
# produced it, on the arrangement R/hold_window.R and R/policy_sweep.R both
# use.
#
# Every casualty carries a mass_casualty_event attribute regardless of whether
# injection is active (R/trajectories.R), 1 where it originated from an event
# and 0 otherwise, so the same reduction applies to both arms: the
# background-only arm simply has no casualty carrying a 1.
#
# Depends on R/analysis.R for build_attributes_wide(); source that before this
# file.

#' Replications per arm
MASS_CASUALTY_REPLICATIONS <- 62L

#' Campaign length in days each replication runs for
MASS_CASUALTY_DAYS <- 30L

#' Control seed the per-replication seeds are drawn from, one arm at a time
MASS_CASUALTY_SEED <- 42L

#' Injection rates compared, in events per day
#'
#' @details Zero is the shipped default (no injection); 0.2 is a mean of one
#'   event every five days, the override `docs/Multi_Run_Supplement.md`
#'   documents for this experiment.
MASS_CASUALTY_ARMS <- c(0, 0.2)

#' Reduce one replication to the mass casualty stress test's response row
#'
#' @param env Wrapped simmer environment for one replication.
#' @param rate_per_day Injection rate the replication ran under.
#' @return One-row data frame of the response set.
#'
#' @details `n_ordinary`/`dow_ordinary` count casualties whose
#'   `mass_casualty_event` attribute reads 0 (background origin, present in
#'   both arms); `n_event`/`dow_event` count those reading 1 (event origin,
#'   necessarily empty in the background-only arm). `n_events` reconstructs
#'   events from the event-origin casualties' arrival times by the same
#'   gap-based grouping `summarise_mass_casualty_events()` (`R/analysis.R`)
#'   applies to the illustrative single run, so the two counts cannot drift
#'   apart under one definition of what separates two events.
reduce_mass_casualty_replication <- function(env, rate_per_day) {
  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  attributes <- simmer::get_mon_attributes(env)

  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"),
                      suffix = c("", "_arrival"))

  ordinary <- !is.na(wide$mass_casualty_event) & wide$mass_casualty_event == 0
  event    <- !is.na(wide$mass_casualty_event) & wide$mass_casualty_event == 1
  died     <- !is.na(wide$dow) & wide$dow == 1

  event_starts <- sort(wide$start_time[event])
  window_max <- env_data$vars$mass_casualty$event$window_max
  n_events <- if (length(event_starts) == 0) {
    0L
  } else {
    gap <- c(Inf, diff(event_starts))
    sum(gap > window_max)
  }

  data.frame(
    rate_per_day = rate_per_day,
    total_casualties = nrow(arrivals),
    n_events          = n_events,
    n_ordinary        = sum(ordinary),
    dow_ordinary      = sum(ordinary & died),
    n_event           = sum(event),
    dow_event         = sum(event & died)
  )
}

#' Set the mass casualty injection rate on a resolved configuration
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param rate_per_day Injection rate to set, in events per day.
#' @return Invisibly, the described configuration that was applied.
#'
#' @details The rate is a variable rather than a bed count, so it is set after
#'   `build_environment()` on the described form, on the convention
#'   `R/hold_window.R`'s `apply_hold_window_setting()` establishes.
apply_mass_casualty_setting <- function(json_data, scenario, rate_per_day) {
  resolved <- resolve_scenario(json_data, scenario)
  described <- build_environment(resolved)
  described$vars$mass_casualty$event$rate_per_day <- rate_per_day
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Measure the mass casualty response set across replications at one arm
#'
#' @param rate_per_day Injection rate in force, in events per day.
#' @param n_iterations Replications to run (default MASS_CASUALTY_REPLICATIONS).
#' @param n_days Campaign length in days (default MASS_CASUALTY_DAYS).
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @return Data frame with one row per replication, carrying the replication
#'   index and the responses reduce_mass_casualty_replication() reports.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed under the caller's generator kind, and the caller's
#'   stream is restored on exit, on `R/hold_window.R`'s arrangement.
run_mass_casualty_measurement <- function(rate_per_day, n_iterations = MASS_CASUALTY_REPLICATIONS,
                                          n_days = MASS_CASUALTY_DAYS, max_cores = NULL) {
  rng_state <- capture_rng_state()
  on.exit(restore_rng_state(rng_state), add = TRUE)

  rep_seeds <- sample.int(.Machine$integer.max, n_iterations)
  RNGkind("L'Ecuyer-CMRG")

  #' Run one replication and return its reduced response row alone
  #'
  #' @param i Index of the replication, into `rep_seeds`.
  #' @return The replication's one-row response frame, carrying its index.
  worker <- function(i) {
    env <- run_once(n_days, seed = rep_seeds[i], write_files = FALSE)
    row <- reduce_mass_casualty_replication(env, rate_per_day)
    row$replication <- i
    row
  }

  dispatched <- dispatch_replications(worker, n_iterations, max_cores)
  usable     <- vapply(dispatched, is.data.frame, logical(1))
  if (!all(usable)) {
    stop(sprintf("%d of %d replications did not complete", sum(!usable), n_iterations),
         call. = FALSE)
  }
  do.call(rbind, dispatched)
}

#' Mean and 95% confidence interval of the count responses across replications
#'
#' @param rows Per-replication responses as returned by
#'   run_mass_casualty_measurement().
#' @return Data frame of response, n_reps, mean, ci_lower and ci_upper, one
#'   row per count-valued response (total_casualties, n_events).
summarise_mass_casualty_counts <- function(rows) {
  responses <- c("total_casualties", "n_events")
  do.call(rbind, lapply(responses, function(r) {
    x <- rows[[r]]
    n <- length(x)
    m <- mean(x)
    e <- if (n > 1) qt(0.975, df = n - 1) * sd(x) / sqrt(n) else 0
    data.frame(response = r, n_reps = n, mean = m,
               ci_lower = m - e, ci_upper = m + e)
  }))
}

#' Pooled died-of-wounds rate and its exact binomial interval
#'
#' @param n_col Name of the column carrying each replication's count at risk.
#' @param dow_col Name of the column carrying each replication's death count.
#' @param rows Per-replication responses carrying both columns.
#' @return One-row data frame of n, dow, rate, ci_lower and ci_upper.
#'
#' @details The rate is pooled across replications rather than averaged
#'   per-replication, on the convention `scripts/check_dow_calibration.R` and
#'   `scripts/check_airlift_collapse_protocol.R` both use for a count too rare
#'   to summarise one replication at a time: a died-of-wounds count of a
#'   handful per 62-replication arm leaves most individual replications with
#'   zero deaths, so a mean and standard deviation taken across replications
#'   would describe the noise in which replications drew a death rather than
#'   the rate itself. `binom.test()`'s exact Clopper-Pearson interval is used
#'   rather than a normal approximation, which would be unstable at these
#'   counts and could extend below zero.
mass_casualty_dow_rate <- function(rows, n_col, dow_col) {
  n   <- sum(rows[[n_col]])
  dow <- sum(rows[[dow_col]])
  if (n == 0) {
    return(data.frame(n = 0L, dow = 0L, rate = NA_real_,
                       ci_lower = NA_real_, ci_upper = NA_real_))
  }
  test <- binom.test(dow, n)
  data.frame(n = n, dow = dow, rate = dow / n,
             ci_lower = test$conf.int[1], ci_upper = test$conf.int[2])
}

#' Replications per arm a pooled rate difference would need for a given
#' half-width
#'
#' @param s1 Per-replication standard deviation of the response in arm 1.
#' @param s2 Per-replication standard deviation of the response in arm 2.
#' @param half_width Half-width wanted on the difference, in the response's
#'   own units.
#' @return The replication count required per arm, rounded up, or NA where
#'   `half_width` is non-positive.
#'
#' @details Uses the normal approximation for the difference of two
#'   independent means at equal replication counts,
#'   $n = (1.96 \sqrt{s_1^2 + s_2^2} / h)^2$, the two-sample form of the
#'   $n = (1.96 s / h)^2$ approximation `R/policy_sweep.R`'s
#'   `policy_replications_for()` and `docs/Multi_Run_Supplement.md`'s
#'   replication-count derivation both use for a paired one. The two arms here
#'   are not paired (this module's header records why), so the two variances
#'   add rather than being taken on one differenced sample.
mass_casualty_replications_for <- function(s1, s2, half_width) {
  if (half_width <= 0) return(NA_real_)
  ceiling((qnorm(0.975) * sqrt(s1^2 + s2^2) / half_width)^2)
}
