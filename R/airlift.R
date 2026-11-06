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
source("R/long_horizon.R")

#' Replications the Role 4 and strategic evacuation measurement runs at
#'
#' @details Thirty, the sustained-operations protocol's count, so the figures
#'   here sit beside those of the other replicated experiments rather than
#'   beneath a different design.
AIRLIFT_REPLICATIONS <- 30L

#' Campaign length the measurement runs over, in days
AIRLIFT_DAYS <- 360L

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

#' Split R2E holding bed occupancy into the four stays that consume it
#'
#' @param resources Resource-monitor rows for one replication.
#' @param wide Per-casualty attributes for one replication, one row per
#'   casualty, carrying the staging and release fields the evacuation wait
#'   needs and the start and duration attributes each clinical stay records.
#' @param horizon_min End of the campaign window, in minutes.
#' @return A list of `total_bed_days`, the four components
#'   `evacuation_bed_days`, `recovery_bed_days`, `post_definitive_bed_days` and
#'   `post_op_hold_bed_days`, their sum as `accounted_bed_days`, the difference
#'   from the measured total as `unexplained_bed_days`, and
#'   `evacuation_share`.
#'
#' @details One pool of holding beds serves four unrelated demands, and each is
#'   reconstructed from what the model recorded rather than inferred as the
#'   remainder of the others. A casualty awaiting a sortie on either airlift
#'   pool is staged in a holding bed when one is free and releases it on
#'   boarding, recording `ame_hold_start` only when it holds one; a ventilated
#'   casualty whose pre-flight care is placed in a holding bed takes it from the
#'   start of that care. The three clinical stays (a stabilisation or
#'   post-definitive stay placed in a holding bed, and retained convalescence)
#'   each record the instant the bed was seized and the duration to be served,
#'   so each is bounded without reference to the others.
#'
#'   Each stay is bounded by the attribute its own exit route sets. The staging
#'   stay ends at `ame_hold_end`, which a casualty records both on boarding a
#'   sortie and on dying while waiting for one, rather than at the departure
#'   time, which the second of those never reaches.
#'
#'   `unexplained_bed_days` is what makes the split checkable. Reporting
#'   recovery as the remainder of the measured total would make the components
#'   sum to it by construction, so an error in any one of them would be
#'   invisible, absorbed into whichever component was the residual. Here every
#'   component is computed independently and the difference from the resource
#'   monitor is reported, so it is a measurement of how completely the four
#'   stays account for the pool rather than an artifact of the arithmetic.
#'
#'   A stay still running when the window closes is charged to the window's end
#'   rather than dropped, its duration being right-censored rather than absent.
holding_occupancy_split <- function(resources, wide, horizon_min) {
  hold <- resources[grepl(R2E_HOLD_PATTERN, resources$resource), ]
  total_bed_days <- 0
  if (nrow(hold) > 0) {
    steps <- pool_queue_steps(hold$resource, hold$time, hold$server)
    total_bed_days <- step_bin_means(steps, c(0, horizon_min)) * horizon_min / DAY_MIN
  }

  #' Bed-days of one clinical stay, censored at the window's close
  #'
  #' @param start_col Attribute naming the instant the bed was seized.
  #' @param minutes Vector of the durations to be served, aligned to `wide`.
  #' @return Bed-days over the window, zero where no casualty took the stay.
  #'
  #' @details A stay is counted from its start to whichever of its own end and
  #'   the window's close comes first, so a stay that began inside the window
  #'   and had not finished at its close contributes the part that fell inside
  #'   it. A stay starting at or after the close contributes nothing.
  stay_bed_days <- function(start_col, minutes) {
    if (!start_col %in% names(wide)) return(0)
    start <- wide[[start_col]]
    took <- !is.na(start) & !is.na(minutes) & start < horizon_min
    if (!any(took)) return(0)
    served <- pmin(start[took] + minutes[took], horizon_min) - start[took]
    sum(pmax(served, 0)) / DAY_MIN
  }

  # Either airlift route can stage in a holding bed, so the component is taken
  # from the instant the staging bed was seized rather than from the route; a
  # casualty staged in overflow holds no bed and records no start.
  #' One duration column of the attribute monitor, in minutes
  #'
  #' @param col Attribute naming the duration to be served.
  #' @param scale Multiplier carrying the attribute's own units to minutes.
  #' @return The column in minutes, or NULL where no casualty set it.
  #'
  #' @details An attribute no casualty set is an absent column rather than an
  #'   empty one on a monitor this narrow, so the column is guarded rather than
  #'   assumed.
  duration_of <- function(col, scale = 1) {
    if (!col %in% names(wide)) return(NULL)
    wide[[col]] * scale
  }

  # The stay ends where the bed was released, which the casualty records on
  # whichever route took it out of the pool: boarding a sortie, or dying while
  # waiting for one. Deriving the end from the departure time alone charged a
  # casualty who died awaiting a sortie to the window's close, that casualty
  # never departing, which claimed the whole of a wait they did not serve.
  staging_minutes <- if ("ame_hold_start" %in% names(wide)) {
    released <- if ("ame_hold_end" %in% names(wide)) wide$ame_hold_end else NA_real_
    released <- ifelse(is.na(released), horizon_min, released)
    pmax(released - wide$ame_hold_start, 0)
  } else {
    NULL
  }
  evacuation_bed_days <- stay_bed_days("ame_hold_start", staging_minutes)

  # A retained casualty holds the bed for what is left of its recovery once it
  # reaches one, the time it queued having counted toward it; battle fatigue
  # restored here records its bed time the same way.
  recovery_bed_days <- stay_bed_days("r2e_recovery_hold_start",
                                     duration_of("r2e_recovery_hold_min"))
  post_definitive_bed_days <- stay_bed_days("post_definitive_hold_start",
                                            duration_of("post_definitive_min"))
  post_op_hold_bed_days <- stay_bed_days("r2e_post_op_hold_start",
                                         duration_of("r2e_post_op_min"))

  accounted <- evacuation_bed_days + recovery_bed_days +
    post_definitive_bed_days + post_op_hold_bed_days

  list(
    total_bed_days           = total_bed_days,
    evacuation_bed_days      = evacuation_bed_days,
    recovery_bed_days        = recovery_bed_days,
    post_definitive_bed_days = post_definitive_bed_days,
    post_op_hold_bed_days    = post_op_hold_bed_days,
    accounted_bed_days       = accounted,
    unexplained_bed_days     = total_bed_days - accounted,
    evacuation_share         = if (total_bed_days > 0) {
      evacuation_bed_days / total_bed_days
    } else {
      NA_real_
    }
  )
}

#' Reduce one finished replication to the strategic evacuation response set
#'
#' @param env Wrapped simmer environment as returned by run_once().
#' @param n_days Campaign length in days.
#' @param seed The replication's own seed, which the Role 4 census draw is
#'   taken under.
#' @return A one-row data frame of the responses the experiment reports, ending
#'   in the overflow census of each point of care over the campaign
#'   (overflow_window_census(), R/queue_series.R).
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

  # simmer reports a casualty still inside a timeout at the instant the run ends
  # twice when ongoing arrivals are requested, which the join below would carry
  # into every count and into each stay that casualty is serving, charging it
  # to the pool twice. One row per casualty is what the reduction assumes.
  arrivals <- arrivals[!duplicated(arrivals[, c("name", "replication")]), ]

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
  #' Mean places a flown sortie carried away empty on one pool
  #'
  #' @param pool_prefix Leading word of the pool label, "Critical" or "Standard".
  #' @return The mean over flown sorties, or NA where none flew.
  places_empty <- function(pool_prefix) {
    rows <- sorties[sorties$outcome == "Flown" & startsWith(as.character(sorties$pool),
                                                            pool_prefix), ]
    if (nrow(rows) == 0) return(NA_real_)
    mean(rows$places_empty)
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
    places_empty_critical = places_empty("Critical"),
    places_empty_standard = places_empty("Standard"),
    role4_peak           = peak_occupancy,
    role4_peak_day       = peak_day,
    role4_peak_after_end = if (is.na(peak_day)) NA else peak_day - n_days,
    hold_total_bed_days  = split$total_bed_days,
    hold_evac_bed_days   = split$evacuation_bed_days,
    hold_recovery_bed_days = split$recovery_bed_days,
    hold_post_definitive_bed_days = split$post_definitive_bed_days,
    hold_post_op_bed_days = split$post_op_hold_bed_days,
    hold_unexplained_bed_days = split$unexplained_bed_days,
    hold_evac_share      = split$evacuation_share,
    ventilated_holds     = nrow(ventilated),
    ventilated_hold_hours = if (nrow(ventilated) > 0) {
      mean(ventilated$ame_icu_hold_minutes) / 60
    } else {
      NA_real_
    }
  ) %>%
    cbind(overflow_window_census(resources, 0, horizon_min))
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

# ── The airlift collapse experiment ─────────────────────────────
# The sweep above measures what the airlift schedule costs within a 30-day
# campaign. This one asks a different question at a different horizon: whether a
# campaign collapses altogether, which is a property of a whole year rather than
# of any month inside it. It is the experiment docs/Methods.md
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
#' @details Ninety days. The closing backlog describes where each campaign
#'   ended up, which the classification by trend does not report; a window at
#'   the end measures that, while a mean over the whole year would mix it with
#'   the months before.
AIRLIFT_COLLAPSE_WINDOW_DAYS <- 90L

#' Airlift routes the collapse experiment classifies, by the subject label
#' each carries in the series
#'
#' @details The two seat pools fill independently, a critical casualty being
#'   unable to take a standard place, so a route whose places run out
#'   collapses while the other keeps up. Classifying them together would let
#'   the larger and slacker standard route dilute the critical one.
AIRLIFT_COLLAPSE_ROUTES <- c("Critical airlift" = AIRLIFT_ROUTE_CRITICAL,
                             "Standard airlift" = AIRLIFT_ROUTE_STANDARD)

#' Leading word of each route's pool label in compute_ame_sorties()
AIRLIFT_COLLAPSE_POOL_PREFIX <- c("Critical airlift" = "Critical",
                                  "Standard airlift" = "Standard")

#' Reduce one finished replication to each airlift route's daily series
#'
#' @param env Wrapped simmer environment as returned by run_once().
#' @param n_days Campaign length in days.
#' @return Data frame of day, series, subject and value in long form: for each
#'   route (subject), `backlog`, the casualties awaiting a place on it at the
#'   end of the day; `decisions`, the casualties assigned to it that day; and
#'   `places_opened`, the places its flown sorties opened that day.
#'
#' @details A casualty joins a route's backlog when the evacuation decision is
#'   made and leaves it on boarding or on dying, whether while queued for a
#'   place or before being staged, so the backlog counts the living who are
#'   still owed a seat. It is a level at the end of the day, as the system
#'   backlog of reduce_flow_series() is. A sortie scheduled at the horizon is
#'   dropped, its outcome being censored by the window (see
#'   reduce_airlift_replication()).
reduce_route_backlog <- function(env, n_days) {
  attributes <- simmer::get_mon_attributes(env)
  resources  <- simmer::get_mon_resources(env)
  ends <- seq_len(n_days) * DAY_MIN

  #' One attribute's value and setting time per casualty
  #'
  #' @param key Attribute key.
  #' @param use "value" or "time".
  #' @return Named numeric vector keyed by casualty, the last setting kept.
  attr_of <- function(key, use) {
    rows <- attributes[attributes$key == key, ]
    if (key == "dow") rows <- rows[rows$value == 1, ]
    rows <- rows[!duplicated(rows$name, fromLast = TRUE), ]
    stats::setNames(rows[[use]], rows$name)
  }
  decided <- attr_of("r2e_evac", "time")
  route   <- attr_of("ame_route", "value")
  left    <- attr_of("ame_hold_end", "value")
  died    <- attr_of("dow", "time")
  who     <- intersect(names(decided), names(route))
  exit_at <- pmin(ifelse(is.na(left[who]), Inf, left[who]),
                  ifelse(is.na(died[who]), Inf, died[who]))

  sorties <- compute_ame_sorties(resources, env_data$vars$role4, n_days)
  sorties <- sorties[sorties$sortie_day < n_days & sorties$outcome == "Flown", ]

  do.call(rbind, lapply(names(AIRLIFT_COLLAPSE_ROUTES), function(label) {
    on_route <- who[route[who] == AIRLIFT_COLLAPSE_ROUTES[[label]]]
    t_in  <- decided[on_route]
    t_out <- exit_at[match(on_route, who)]
    backlog <- vapply(ends, function(e) sum(t_in <= e & t_out > e), numeric(1))
    days_in <- floor(t_in / DAY_MIN) + 1
    decisions <- tabulate(days_in[days_in <= n_days], nbins = n_days)
    pool <- sorties[startsWith(as.character(sorties$pool),
                               AIRLIFT_COLLAPSE_POOL_PREFIX[[label]]), ]
    places <- numeric(n_days)
    if (nrow(pool) > 0) {
      by_day <- tapply(pool$capacity_added, pool$sortie_day, sum)
      places[as.integer(names(by_day))] <- by_day
    }
    data.frame(day = rep(seq_len(n_days), 3),
               series = rep(c("backlog", "decisions", "places_opened"), each = n_days),
               subject = label,
               value = c(backlog, decisions, places))
  }))
}

#' Reduce each replication's route series to one collapse classification
#'
#' @param series Route series as reduce_route_backlog() returns them, carrying
#'   `replication`.
#' @param n_days Campaign length in days.
#' @param window_days Length of the closing window the descriptive closing
#'   backlog is averaged over (default AIRLIFT_COLLAPSE_WINDOW_DAYS).
#' @param block_days Block length the trend is fitted over (default
#'   LONG_HORIZON_BLOCK_DAYS).
#' @return Data frame of replication, route, closing_backlog, slope_per_day,
#'   stability, utilisation and collapsed, one row per replication and route.
#'
#' @details A route is collapsed in a campaign when its backlog is still rising
#'   over the second half of the campaign: classify_stability()
#'   (R/long_horizon.R), the rule the sustained-operations protocol applies to
#'   every pool, is applied to the replication's own block means of that
#'   route's backlog, and a drifting backlog with a positive slope is
#'   collapsed. A level threshold cannot make this distinction, because
#'   casualties wait between weekly sorties whether or not the airlift keeps
#'   up. `utilisation` is the route's demand over the campaign divided by the
#'   places its flown sorties opened, the quantity whose reaching one makes a
#'   queue unable to settle; it is reported beside the classification rather
#'   than replacing it, being an average that a run of cancellations late in
#'   a campaign can leave below one while the backlog is still climbing.
collapse_response <- function(series, n_days, window_days = AIRLIFT_COLLAPSE_WINDOW_DAYS,
                              block_days = LONG_HORIZON_BLOCK_DAYS) {
  empty <- data.frame(replication = integer(0), route = character(0),
                      closing_backlog = numeric(0), slope_per_day = numeric(0),
                      stability = character(0), utilisation = numeric(0),
                      collapsed = logical(0))
  if (nrow(series) == 0) return(empty)
  parts <- split(series, list(series$replication, series$subject), drop = TRUE)
  out <- do.call(rbind, lapply(parts, function(d) {
    backlog <- d[d$series == "backlog", ]
    if (nrow(backlog) == 0) return(NULL)
    st <- classify_stability(block_means(backlog, block_days))
    places <- sum(d$value[d$series == "places_opened"])
    data.frame(
      replication     = d$replication[1],
      route           = d$subject[1],
      closing_backlog = mean(backlog$value[backlog$day > n_days - window_days]),
      slope_per_day   = st$slope / block_days,
      stability       = st$stability,
      utilisation     = if (places > 0) sum(d$value[d$series == "decisions"]) / places else NA_real_,
      collapsed       = st$stability == "drifting" && st$slope > 0
    )
  }))
  if (is.null(out)) return(empty)
  out <- out[order(out$route, out$replication), ]
  rownames(out) <- NULL
  out
}

#' Summarise one arm and route's collapse responses as the published table
#' reports them
#'
#' @param response Collapse responses of one route, as returned by
#'   collapse_response().
#' @return A one-row data frame of n_reps, n_collapsed, rate, ci_lower,
#'   ci_upper, median_backlog, worst_backlog, mean_utilisation and
#'   n_over_capacity.
#'
#' @details The interval is the Clopper-Pearson exact binomial interval rather
#'   than a normal approximation, the response being a proportion measured on
#'   thirty campaigns of which several arms have zero successes; a normal
#'   interval on a zero count has zero width, which would report a rate of zero
#'   as exactly known. `n_over_capacity` counts the campaigns whose demand met
#'   or exceeded the places flown.
summarise_collapse <- function(response) {
  n <- nrow(response)
  k <- sum(response$collapsed)
  ci <- if (n == 0) c(NA_real_, NA_real_) else {
    c(if (k == 0) 0 else qbeta(0.025, k, n - k + 1),
      if (k == n) 1 else qbeta(0.975, k + 1, n - k))
  }
  data.frame(
    n_reps           = n,
    n_collapsed      = k,
    rate             = if (n > 0) k / n else NA_real_,
    ci_lower         = ci[1],
    ci_upper         = ci[2],
    median_backlog   = if (n > 0) median(response$closing_backlog) else NA_real_,
    worst_backlog    = if (n > 0) max(response$closing_backlog) else NA_real_,
    mean_utilisation = if (n > 0) mean(response$utilisation) else NA_real_,
    n_over_capacity  = sum(response$utilisation >= 1, na.rm = TRUE)
  )
}
