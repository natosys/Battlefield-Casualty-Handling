##############################################################################
## R/casualty_surge.R                                                        ##
## The casualty surge event stress test, at its two arms                    ##
##############################################################################
#
# casualty_surge.event.rate_per_day lets a compound-Poisson surge of casualties
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
# Every casualty carries a casualty_surge_event attribute regardless of whether
# injection is active (R/trajectories.R), 1 where it originated from an event
# and 0 otherwise, so the same reduction applies to both arms: the
# background-only arm simply has no casualty carrying a 1.
#
# Depends on R/analysis.R for build_attributes_wide(); source that before this
# file.

#' Replications per arm
CASUALTY_SURGE_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
CASUALTY_SURGE_DAYS <- 360L

#' Control seed the per-replication seeds are drawn from, one arm at a time
CASUALTY_SURGE_SEED <- 42L

#' Injection rates compared, in events per day
#'
#' @details Zero is the shipped default (no injection); 0.2 is a mean of one
#'   event every five days, the override `docs/Methods.md`
#'   documents for this experiment.
CASUALTY_SURGE_ARMS <- c(0, 0.2)

#' Reduce one replication to the casualty surge stress test's response row
#'
#' @param env Wrapped simmer environment for one replication.
#' @param rate_per_day Injection rate the replication ran under.
#' @return One-row data frame of the response set.
#'
#' @details `n_ordinary`/`dow_ordinary` count casualties whose
#'   `casualty_surge_event` attribute reads 0 (background origin, present in
#'   both arms); `n_event`/`dow_event` count those reading 1 (event origin,
#'   necessarily empty in the background-only arm). `n_events` reconstructs
#'   events as the distinct event ids the generator assigned to event-origin
#'   casualties, the grouping `reconstruct_surge_events()` (`R/analysis.R`)
#'   applies to the illustrative single run, so the two counts cannot drift
#'   apart under one definition of what separates two events.
reduce_casualty_surge_replication <- function(env, rate_per_day) {
  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  attributes <- simmer::get_mon_attributes(env)

  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"),
                      suffix = c("", "_arrival"))

  ordinary <- !is.na(wide$casualty_surge_event) & wide$casualty_surge_event == 0
  event    <- !is.na(wide$casualty_surge_event) & wide$casualty_surge_event == 1
  died     <- !is.na(wide$dow) & wide$dow == 1

  n_events <- length(unique(wide$casualty_surge_event_id[event]))

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

#' Set the casualty surge injection rate on a resolved configuration
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param rate_per_day Injection rate to set, in events per day.
#' @return Invisibly, the described configuration that was applied.
#'
#' @details The rate is a variable rather than a bed count, so it is set after
#'   `build_environment()` on the described form, on the convention
#'   `R/hold_window.R`'s `apply_hold_window_setting()` establishes.
apply_casualty_surge_setting <- function(json_data, scenario, rate_per_day) {
  resolved <- resolve_scenario(json_data, scenario)
  described <- build_environment(resolved)
  described$vars$casualty_surge$event$rate_per_day <- rate_per_day
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Run replications and reduce each to one row inside its own worker
#'
#' @param reducer Function of one wrapped environment returning a one-row
#'   data frame of that replication's responses.
#' @param n_iterations Replications to run.
#' @param n_days Campaign length in days.
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @return Data frame with one row per replication, carrying the replication
#'   index and the responses `reducer` reports.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed under the caller's generator kind, and the caller's
#'   stream is restored on exit, on `R/hold_window.R`'s arrangement.
measure_casualty_surge_replications <- function(reducer, n_iterations, n_days, max_cores = NULL) {
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
    row <- reducer(env)
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

#' Measure the casualty surge response set across replications at one arm
#'
#' @param rate_per_day Injection rate in force, in events per day.
#' @param n_iterations Replications to run (default CASUALTY_SURGE_REPLICATIONS).
#' @param n_days Campaign length in days (default CASUALTY_SURGE_DAYS).
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @return Data frame with one row per replication, carrying the replication
#'   index and the responses reduce_casualty_surge_replication() reports.
run_casualty_surge_measurement <- function(rate_per_day, n_iterations = CASUALTY_SURGE_REPLICATIONS,
                                           n_days = CASUALTY_SURGE_DAYS, max_cores = NULL) {
  measure_casualty_surge_replications(
    function(env) reduce_casualty_surge_replication(env, rate_per_day),
    n_iterations, n_days, max_cores
  )
}

#' Mean and 95% confidence interval of the count responses across replications
#'
#' @param rows Per-replication responses as returned by
#'   run_casualty_surge_measurement().
#' @return Data frame of response, n_reps, mean, ci_lower and ci_upper, one
#'   row per count-valued response (total_casualties, n_events).
summarise_casualty_surge_counts <- function(rows) {
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
casualty_surge_dow_rate <- function(rows, n_col, dow_col) {
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
#'   `policy_replications_for()` and `docs/Methods.md`'s
#'   replication-count derivation both use for a paired one. The two arms here
#'   are not paired (this module's header records why), so the two variances
#'   add rather than being taken on one differenced sample.
casualty_surge_replications_for <- function(s1, s2, half_width) {
  if (half_width <= 0) return(NA_real_)
  ceiling((qnorm(0.975) * sqrt(s1^2 + s2^2) / half_width)^2)
}

# ── Event size sweep ────────────────────────────────────────────────────────

#' Fixed event sizes swept, in casualties per event
#'
#' @details The shipped generator draws each event uniformly between
#'   `min_cas` and `max_cas`; the sweep fixes both at one value so that the
#'   response is a function of size alone. The range runs from a single
#'   background-scale burst (10) to well past anything the configured 20 to 60
#'   range reaches, so the size at which care degrades is bracketed by
#'   measurement rather than assumed.
CASUALTY_SURGE_SIZES <- c(10L, 20L, 40L, 60L, 90L, 120L, 180L)

#' Injection rate the size sweep runs at, in events per day
CASUALTY_SURGE_SIZE_RATE <- 0.2

#' Replications per size in the size sweep
CASUALTY_SURGE_SIZE_REPLICATIONS <- 30L

#' Set a fixed event size and injection rate on a resolved configuration
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param size Casualties per event; sets `min_cas` and `max_cas` together.
#' @param rate_per_day Injection rate, in events per day; zero disables injection.
#' @return Invisibly, the described configuration that was applied.
apply_casualty_surge_size_setting <- function(json_data, scenario, size, rate_per_day) {
  described <- apply_casualty_surge_setting(json_data, scenario, rate_per_day)
  described$vars$casualty_surge$event$min_cas <- size
  described$vars$casualty_surge$event$max_cas <- size
  assign("env_data", described, envir = globalenv())
  invisible(described)
}

#' Reduce one replication to the size sweep's response row
#'
#' @param env Wrapped simmer environment for one replication.
#' @param size Event size the replication ran under (0 for the no-event arm).
#' @return One-row data frame: the counts reduce_casualty_surge_replication()
#'   reports, the largest realised event, and the peak four-hour mean queue of
#'   each pool in `TIME_SERIES_POOLS` as `peak_queue_<n>`.
#'
#' @details Peaks are taken over the whole campaign, so a response that moves
#'   with size reflects the events and not a closing-window average that an
#'   event falling outside it would miss.
reduce_casualty_surge_size_replication <- function(env, size) {
  row <- reduce_casualty_surge_replication(env, if (size == 0) 0 else CASUALTY_SURGE_SIZE_RATE)
  row$size <- size

  attributes <- simmer::get_mon_attributes(env)
  ids <- attributes$value[attributes$key == "casualty_surge_event_id" & attributes$value > 0]
  row$max_event_size <- if (length(ids) == 0) 0L else max(table(ids))

  resources <- simmer::get_mon_resources(env)
  series <- pool_queue_series(resources, horizon_min = max(resources$time))
  for (i in seq_along(TIME_SERIES_POOLS)) {
    q <- series$queue[series$pool == names(TIME_SERIES_POOLS)[i]]
    row[[paste0("peak_queue_", i)]] <- if (length(q) == 0) NA_real_ else max(q)
  }
  row
}

#' Measure the size sweep's response set across replications at one size
#'
#' @param size Casualties per event (0 for the no-event arm).
#' @param n_iterations Replications to run.
#' @param n_days Campaign length in days.
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @return Data frame with one row per replication.
run_casualty_surge_size_measurement <- function(size,
                                                n_iterations = CASUALTY_SURGE_SIZE_REPLICATIONS,
                                                n_days = CASUALTY_SURGE_DAYS, max_cores = NULL) {
  measure_casualty_surge_replications(
    function(env) reduce_casualty_surge_size_replication(env, size),
    n_iterations, n_days, max_cores
  )
}

#' Summarise the size sweep, one row per size
#'
#' @param rows Per-replication responses from
#'   run_casualty_surge_size_measurement(), bound across sizes.
#' @return Data frame of size, n_reps, mean events, largest realised event,
#'   pooled event and ordinary died-of-wounds rates with exact intervals, and
#'   the mean and 95% interval of each `peak_queue_<n>` response.
summarise_casualty_surge_size <- function(rows) {
  peaks <- grep("^peak_queue_", names(rows), value = TRUE)
  do.call(rbind, lapply(sort(unique(rows$size)), function(sz) {
    arm <- rows[rows$size == sz, ]
    ev  <- casualty_surge_dow_rate(arm, "n_event", "dow_event")
    ord <- casualty_surge_dow_rate(arm, "n_ordinary", "dow_ordinary")
    out <- data.frame(
      size = sz, n_reps = nrow(arm), mean_events = mean(arm$n_events),
      max_event_size = max(arm$max_event_size),
      dow_event_rate = ev$rate, dow_event_lower = ev$ci_lower, dow_event_upper = ev$ci_upper,
      dow_ordinary_rate = ord$rate, dow_ordinary_lower = ord$ci_lower,
      dow_ordinary_upper = ord$ci_upper
    )
    for (p in peaks) {
      x <- arm[[p]]
      e <- if (length(x) > 1) qt(0.975, df = length(x) - 1) * sd(x) / sqrt(length(x)) else 0
      out[[p]] <- mean(x)
      out[[paste0(p, "_ci")]] <- e
    }
    out
  }))
}
