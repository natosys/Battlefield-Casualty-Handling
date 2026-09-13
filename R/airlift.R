##############################################
## R/airlift.R                              ##
## Strategic evacuation and Role 4 demand   ##
##############################################
#
# Every published finding about strategic aeromedical evacuation and the
# national support base it feeds came from one campaign, which cannot separate
# the mechanism from one set of sortie draws. This module measures the same
# responses across replications, and sweeps the two settings the single-campaign
# evidence pointed at, sortie reliability and the interval between sorties.
#
# Each replication is reduced to one row inside the forked worker that produced
# it, as R/long_horizon.R reduces to a daily series, so a sweep of several
# settings at fifty replications holds the responses rather than the monitoring
# data. Depends on R/replication.R, R/environment.R and R/analysis.R
# (compute_role4_census, compute_ame_sorties); source those before this file.

source("R/constants.R")

#' Replications the Role 4 and strategic evacuation measurement runs at
#'
#' @details Fifty, the count `docs/Multi_Run_Supplement.md` derives for a
#'   time-weighted occupancy or wait response and the count every other
#'   replicated experiment in the companion paper uses, so the figures here sit
#'   beside those rather than beneath a different design.
AIRLIFT_REPLICATIONS <- 50L

#' Campaign length the measurement runs over, in days
AIRLIFT_DAYS <- 30L

#' Sortie cancellation probabilities the reliability sweep covers
#'
#' @details Spans the range from a schedule that never fails to one that loses
#'   two sorties in five. The shipped default is the first of these, so the
#'   sweep's first point is the configuration the rest of the paper measures and
#'   the remainder say what departing from it costs.
AIRLIFT_FAILURE_PROBABILITIES <- c(0, 0.05, 0.10, 0.15, 0.25, 0.40)

#' Intervals between scheduled sorties the schedule sweep covers, in days
#'
#' @details The shipped seven days, and the intervals either side of it that a
#'   planner could realistically choose. A shorter interval buys resilience by
#'   flying more often rather than by flying larger, which is the comparison the
#'   single-campaign evidence proposed and could not make.
AIRLIFT_SORTIE_INTERVALS <- c(3L, 5L, 7L, 10L, 14L)

#' Resource-name pattern matching the R2E holding beds
#'
#' @details The pool that carries both in-theatre recovery and the strategic
#'   evacuation wait, which is the coupling this module separates.
R2E_HOLD_PATTERN <- "^b_r2eheavy_hold_[0-9]+_t[0-9]+$"

#' Route code of a casualty waiting on the critical airlift pool
#'
#' @details A critical-route casualty holds an intensive care bed through the
#'   wait rather than a holding bed, so the two routes consume different pools
#'   and only the standard route appears in the holding split.
AIRLIFT_ROUTE_CRITICAL <- 1

#' Route code of a casualty waiting on the standard airlift pool
AIRLIFT_ROUTE_STANDARD <- 2

#' Split R2E holding bed occupancy into recovery and evacuation wait
#'
#' @param resources Resource-monitor rows for one replication.
#' @param wide Per-casualty attributes for one replication, one row per
#'   casualty, carrying ame_route, r2e_departure_time and ame_departure_time.
#' @param horizon_min End of the campaign window, in minutes.
#' @return A list of `total_bed_days`, `evacuation_bed_days`,
#'   `recovery_bed_days` and `evacuation_share`.
#'
#' @details The two components are computed rather than estimated, because the
#'   model makes the attribution exact: a casualty awaiting the standard airlift
#'   pool seizes a holding bed on reaching the evacuation decision and releases
#'   it on boarding, so that casualty's evacuation wait is the whole of its
#'   holding occupancy over that period. A casualty awaiting the critical pool
#'   holds an intensive care bed instead and contributes nothing here. Recovery
#'   is then the remainder of the pool's measured occupancy rather than a second
#'   reconstruction, which is what makes the two sum to the total by
#'   construction; `scripts/check_holding_occupancy_split.R` asserts that they
#'   do against the resource monitor.
#'
#'   A casualty still waiting when the window closes is charged to the window's
#'   end rather than dropped, its wait being right-censored rather than absent.
holding_occupancy_split <- function(resources, wide, horizon_min) {
  hold <- resources[grepl(R2E_HOLD_PATTERN, resources$resource), ]
  total_bed_days <- 0
  if (nrow(hold) > 0) {
    steps <- pool_queue_steps(hold$resource, hold$time, hold$server)
    total_bed_days <- step_bin_means(steps, c(0, horizon_min)) * horizon_min / DAY_MIN
  }

  waiting <- wide[!is.na(wide$ame_route) & wide$ame_route == AIRLIFT_ROUTE_STANDARD &
                    !is.na(wide$r2e_departure_time), ]
  departed <- ifelse(is.na(waiting$ame_departure_time), horizon_min,
                     waiting$ame_departure_time)
  wait_min <- pmax(pmin(departed, horizon_min) - waiting$r2e_departure_time, 0)
  evacuation_bed_days <- sum(wait_min) / DAY_MIN

  list(
    total_bed_days      = total_bed_days,
    evacuation_bed_days = evacuation_bed_days,
    recovery_bed_days   = total_bed_days - evacuation_bed_days,
    evacuation_share    = if (total_bed_days > 0) evacuation_bed_days / total_bed_days else NA_real_
  )
}

#' Reduce one finished replication to the strategic evacuation response set
#'
#' @param env Wrapped simmer environment as returned by run_once().
#' @param n_days Campaign length in days.
#' @param seed The replication's own seed, which the Role 4 census draw is
#'   taken under.
#' @return A one-row data frame of the responses the experiment reports.
#'
#' @details Every response is a per-replication scalar, which is the unit of
#'   analysis every interval in this project is taken over. A wait is summarised
#'   over the casualties of that replication before the replication enters the
#'   interval, so the interval describes the spread of campaigns rather than of
#'   casualties.
#'
#'   The Role 4 census is drawn under the replication's own seed rather than
#'   under whatever stream position the caller happens to be at. Each evacuated
#'   casualty's length of stay at the national support base is drawn by the
#'   analysis rather than by the simulation (README Further Development L30), so
#'   the census and the peak taken from it otherwise depend on how many draws
#'   preceded them in the session: the same finished campaign gives a peak of
#'   117, 119 or 121 from three different stream positions. Seeding it here
#'   makes every response in this row a function of the replication's seed
#'   alone, which is the property every other measurement in this project has
#'   and which an interval across replications requires. The caller's stream is
#'   restored, so the measurement stays stream-neutral.
reduce_airlift_replication <- function(env, n_days, seed) {
  horizon_min <- n_days * DAY_MIN

  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  attributes <- simmer::get_mon_attributes(env)
  resources  <- simmer::get_mon_resources(env)

  # The same join analyse_run() builds, which is what compute_role4_census()
  # and compute_ame_sorties() read: one row per casualty carrying both its
  # arrival record and every attribute it set.
  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"),
                      suffix = c("", "_arrival"))

  decided <- wide[!is.na(wide$r2e_evac) & wide$r2e_evac == 1, ]
  boarded <- decided[!is.na(decided$ame_departure_time), ]
  queued  <- nrow(decided) - nrow(boarded)

  #' Mean or 90th percentile wait over one airlift route, in days
  #'
  #' @param route Route code to select, or NA for both.
  #' @param fn Summary to apply to the boarded casualties' waits.
  #' @return The summary in days, or NA where nobody boarded on that route.
  wait_stat <- function(route, fn) {
    rows <- if (is.na(route)) boarded else boarded[!is.na(boarded$ame_route) &
                                                     boarded$ame_route == route, ]
    if (nrow(rows) == 0) return(NA_real_)
    fn(rows$ame_wait_minutes) / DAY_MIN
  }
  #' The 90th percentile of a numeric vector
  #'
  #' @param x Waits in minutes, possibly carrying NA.
  #' @return The 90th percentile, ignoring NA.
  p90 <- function(x) as.numeric(quantile(x, 0.90, na.rm = TRUE))

  # compute_ame_sorties() returns one row per sortie and pool, so a sortie's
  # outcome is read once per departure rather than once per row; a sortie is
  # counted as flown only where every pool it carries reports "Flown", since a
  # partial reconstruction reads as "Unknown" rather than as a departure.
  #
  # A sortie scheduled at exactly the horizon is dropped from both counts. The
  # run ends at that instant, so its capacity bump never resolves and the
  # reconstruction reads it as cancelled whether or not it would have flown:
  # its outcome is censored by the window rather than observed. Counting it
  # would report a cancellation rate of 0.10 at a configured probability of
  # zero, purely because 30 divides by the sortie interval.
  sorties <- compute_ame_sorties(resources, env_data$vars$role4, n_days)
  sorties <- sorties[sorties$sortie_day < n_days, ]
  scheduled <- length(unique(sorties$sortie_day))
  flown <- if (scheduled == 0) {
    0L
  } else {
    sum(tapply(sorties$outcome, sorties$sortie_day, function(o) all(o == "Flown")))
  }

  census <- with_preserved_rng({
    set.seed(seed)
    compute_role4_census(wide, env_data$vars$role4)
  })
  peak_day <- NA_integer_
  peak_occupancy <- 0
  if (nrow(census) > 0) {
    by_day <- tapply(census$occupancy, census$day, sum)
    peak_occupancy <- max(by_day)
    peak_day <- as.integer(names(by_day)[which.max(by_day)])
  }

  split <- holding_occupancy_split(resources, wide, horizon_min)

  ventilated <- wide[!is.na(wide$ame_icu_hold) & wide$ame_icu_hold == 1 &
                       !is.na(wide$ame_icu_hold_minutes), ]

  data.frame(
    decisions            = nrow(decided),
    boarded              = nrow(boarded),
    queued_at_end        = queued,
    mean_wait_days       = wait_stat(NA, mean),
    p90_wait_days        = wait_stat(NA, p90),
    mean_wait_critical   = wait_stat(AIRLIFT_ROUTE_CRITICAL, mean),
    mean_wait_standard   = wait_stat(AIRLIFT_ROUTE_STANDARD, mean),
    sorties_scheduled    = scheduled,
    sorties_flown        = flown,
    cancellation_rate    = if (scheduled > 0) (scheduled - flown) / scheduled else NA_real_,
    role4_peak           = peak_occupancy,
    role4_peak_day       = peak_day,
    role4_peak_after_end = if (is.na(peak_day)) NA else peak_day - n_days,
    hold_total_bed_days  = split$total_bed_days,
    hold_evac_bed_days   = split$evacuation_bed_days,
    hold_recovery_bed_days = split$recovery_bed_days,
    hold_evac_share      = split$evacuation_share,
    ventilated_holds     = nrow(ventilated),
    ventilated_hold_hours = if (nrow(ventilated) > 0) {
      mean(ventilated$ame_icu_hold_minutes) / 60
    } else {
      NA_real_
    }
  )
}

#' Enter a scenario's configuration with one role4.ame field overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param field Name of the field inside role4.ame to override, or NULL for the
#'   shipped configuration.
#' @param value Value to set that field to.
#' @return Invisibly, the environment description that was bound.
#'
#' @details The override is applied after build_environment() rather than
#'   before it: resolve_scenario() returns the parsed JSON, in which vars is a
#'   list of name/value pairs, and build_environment() is what turns those into
#'   the named structure the model reads, so setting the field on the resolved
#'   form would write a value nothing looks at. Assignment is to globalenv()
#'   explicitly, matching apply_config_globals() and restore_config_globals().
apply_airlift_setting <- function(json_data, scenario, field = NULL, value = NULL) {
  described <- build_environment(resolve_scenario(json_data, scenario))
  if (!is.null(field)) described$vars$role4$ame[[field]] <- value
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Measure the strategic evacuation response set across replications
#'
#' @param n_iterations Replications to run (default AIRLIFT_REPLICATIONS).
#' @param n_days Campaign length in days (default AIRLIFT_DAYS).
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @return Data frame with one row per replication, carrying the replication
#'   index and the responses reduce_airlift_replication() reports.
#'
#' @details Each replication is reduced inside the forked worker that produced
#'   it, so a sweep over several settings holds the response rows rather than
#'   the monitoring data of every run behind them. The seeds are drawn as
#'   `run_replications()` draws them, from the caller's control seed under the
#'   caller's generator kind, and the caller's stream is restored on exit, so a
#'   measurement at a control seed is reproducible on the same terms as every
#'   other in this project.
run_airlift_measurement <- function(n_iterations = AIRLIFT_REPLICATIONS,
                                    n_days = AIRLIFT_DAYS, max_cores = NULL) {
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
    row <- reduce_airlift_replication(env, n_days, seed = rep_seeds[i])
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

#' Mean and 95% confidence interval of every response across replications
#'
#' @param rows Per-replication responses as returned by
#'   run_airlift_measurement().
#' @return Data frame of response, n_reps, mean, ci_lower and ci_upper, one row
#'   per response.
#'
#' @details A response no replication produced, such as a wait on a route
#'   nobody took, is reported with an `n_reps` of zero rather than dropped, so a
#'   reader can tell an absent measurement from a measured zero.
summarise_airlift <- function(rows) {
  responses <- setdiff(names(rows), "replication")
  do.call(rbind, lapply(responses, function(r) {
    x <- rows[[r]]
    x <- x[!is.na(x)]
    n <- length(x)
    m <- if (n > 0) mean(x) else NA_real_
    e <- if (n > 1) qt(0.975, df = n - 1) * sd(x) / sqrt(n) else 0
    data.frame(response = r, n_reps = n, mean = m,
               ci_lower = if (n > 1) m - e else m,
               ci_upper = if (n > 1) m + e else m)
  }))
}

#' Sweep one strategic airlift setting and measure the response set at each value
#'
#' @param field Name of the field inside `env_data$vars$role4$ame` to vary.
#' @param values Values to set it to, in the order they are reported.
#' @param scenario Scenario profile the sweep runs under.
#' @param n_iterations Replications per value (default AIRLIFT_REPLICATIONS).
#' @param n_days Campaign length in days (default AIRLIFT_DAYS).
#' @param seed Control seed, set once before each value so that the arms share
#'   their per-replication seeds.
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @return Data frame of the per-replication responses at every value, carrying
#'   `setting` (the field) and `value`.
#'
#' @details The configuration globals are restored on exit, on the error path as
#'   well as the success path, so a sweep that fails part-way leaves the session
#'   as it found it rather than on whichever value failed. The field is checked
#'   against the resolved configuration before the first run, so a typo in a
#'   field name fails immediately rather than after an arm has been paid for.
run_airlift_sweep <- function(field, values, scenario = "moderate_intensity",
                              n_iterations = AIRLIFT_REPLICATIONS,
                              n_days = AIRLIFT_DAYS, seed = 42L, max_cores = NULL) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
  probe <- build_environment(resolve_scenario(json_data, scenario))
  if (is.null(probe$vars$role4$ame[[field]])) {
    stop(sprintf("run_airlift_sweep: role4.ame.%s is not set in the configuration",
                 field), call. = FALSE)
  }

  do.call(rbind, lapply(values, function(value) {
    apply_airlift_setting(json_data, scenario, field, value)

    message(sprintf("role4.ame.%s = %s: %d replications x %d days",
                    field, format(value), n_iterations, n_days))
    set.seed(seed)
    rows <- run_airlift_measurement(n_iterations, n_days, max_cores)
    rows$setting <- field
    rows$value   <- value
    rows
  }))
}

# ── The airlift collapse experiment (Issue #312) ─────────────────────────────
# The sweep above measures what the airlift schedule costs within a 30-day
# campaign. This one asks a different question at a different horizon: whether a
# campaign collapses altogether, which is a property of a whole year rather than
# of any month inside it. It is the experiment docs/Multi_Run_Supplement.md
# recorded as the one a reader could not re-execute from a tracked command,
# because it needs the long-horizon runner and the cancellation sweep together
# and neither alone computes its response.

#' Sortie cancellation probabilities the collapse experiment covers
#'
#' @details The six the published table reports. They differ from
#'   AIRLIFT_FAILURE_PROBABILITIES because the two experiments are asking
#'   different questions: that sweep spans the range to establish a gradient,
#'   this one concentrates on the band the cliff sits in.
AIRLIFT_COLLAPSE_PROBABILITIES <- c(0, 0.05, 0.10, 0.15, 0.20, 0.25)

#' Replications the collapse experiment runs at each probability
AIRLIFT_COLLAPSE_REPLICATIONS <- 30L

#' Campaign length the collapse experiment runs over, in days
AIRLIFT_COLLAPSE_DAYS <- 360L

#' Length of the closing window the collapse response is measured over, in days
#'
#' @details Ninety days. Collapse is a state a campaign reaches and does not
#'   leave, so the response is where the campaign ended up rather than anything
#'   about its path, and a window at the end measures that while a mean over the
#'   whole year would mix it with the months before.
AIRLIFT_COLLAPSE_WINDOW_DAYS <- 90L

#' Mean R2E holding queue at or above which a campaign counts as collapsed
#'
#' @details Twenty casualties. A threshold classifier is used rather than a mean
#'   because the per-replication values are bimodal rather than spread: the
#'   published measurement puts the highest clear run at 17.9 and the lowest
#'   collapsed run at 84, so any threshold inside that gap returns the same
#'   count and this value is not a tuning choice. Reporting a mean over a
#'   bimodal population would describe no campaign in it.
AIRLIFT_COLLAPSE_THRESHOLD <- 20

#' Subject name the long-horizon series carries the R2E holding queue under
AIRLIFT_COLLAPSE_SUBJECT <- "R2E holding beds"

#' Reduce a long-horizon series to one collapse response per replication
#'
#' @param series Long-horizon series as returned by run_long_horizon().
#' @param n_days Campaign length in days.
#' @param window_days Length of the closing window (default
#'   AIRLIFT_COLLAPSE_WINDOW_DAYS).
#' @param threshold Mean queue at or above which a campaign is collapsed
#'   (default AIRLIFT_COLLAPSE_THRESHOLD).
#' @return Data frame of replication, closing_queue and collapsed.
#'
#' @details The window is the last `window_days` days of the campaign
#'   inclusive, so a 90-day window of a 360-day run covers days 271 to 360.
collapse_response <- function(series, n_days, window_days = AIRLIFT_COLLAPSE_WINDOW_DAYS,
                              threshold = AIRLIFT_COLLAPSE_THRESHOLD) {
  rows <- series[series$series == "mean_queue" &
                   series$subject == AIRLIFT_COLLAPSE_SUBJECT &
                   series$day > n_days - window_days, ]
  if (nrow(rows) == 0) {
    return(data.frame(replication = integer(0), closing_queue = numeric(0),
                      collapsed = logical(0)))
  }
  closing <- tapply(rows$value, rows$replication, mean)
  data.frame(
    replication    = as.integer(names(closing)),
    closing_queue  = as.numeric(closing),
    collapsed      = as.numeric(closing) >= threshold
  )
}

#' Summarise one arm's collapse responses as the published table reports them
#'
#' @param response Collapse responses as returned by collapse_response().
#' @return A one-row data frame of n_reps, n_collapsed, rate, ci_lower,
#'   ci_upper, median_queue and worst_queue.
#'
#' @details The interval is the Clopper-Pearson exact binomial interval rather
#'   than a normal approximation, the response being a proportion measured on
#'   thirty campaigns of which several arms have zero successes; a normal
#'   interval on a zero count has zero width, which would report a rate of zero
#'   as exactly known.
summarise_collapse <- function(response) {
  n <- nrow(response)
  k <- sum(response$collapsed)
  ci <- if (n == 0) c(NA_real_, NA_real_) else {
    c(if (k == 0) 0 else qbeta(0.025, k, n - k + 1),
      if (k == n) 1 else qbeta(0.975, k + 1, n - k))
  }
  data.frame(
    n_reps       = n,
    n_collapsed  = k,
    rate         = if (n > 0) k / n else NA_real_,
    ci_lower     = ci[1],
    ci_upper     = ci[2],
    median_queue = if (n > 0) median(response$closing_queue) else NA_real_,
    worst_queue  = if (n > 0) max(response$closing_queue) else NA_real_
  )
}
