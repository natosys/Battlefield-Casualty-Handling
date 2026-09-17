##############################################################################
## R/policy_sweep.R                                                         ##
## The evacuation policy sweep, at the sustained-operations horizon          ##
##############################################################################
#
# The shipped evacuation policy sets how long a casualty may be expected to
# convalesce in theatre before being evacuated instead, and it is the parameter
# that decides how much of the R2E holding pool is spent on convalescence
# rather than on the trauma pathway. A casualty retained under the policy holds
# a holding bed for its whole recovery, so the policy and the establishment are
# substitutes: a longer policy and a larger pool buy the same forward capacity
# at different costs. Neither is a tuning parameter (evacuation policy is a
# command decision and establishment is force structure), so what this module
# measures is the shape of the trade rather than an optimum.
#
# Each replication is reduced to one row of the response set inside the forked
# worker that produced it, on the arrangement R/airlift.R uses, so a sweep over
# several policies holds the responses rather than the monitoring data of every
# 360-day run behind them.
#
# This module reduces its own responses rather than extending
# R/long_horizon.R's daily series, because the quantities the trade consists of
# are campaign totals and closing-window states rather than series: returns to
# duty, died of wounds, the national support base census, the in-theatre share
# and the forward pools' closing occupancy.
#
# Depends on R/queue_series.R for the pool-queue step-function estimators and
# on R/analysis.R for compute_role4_census() and build_attributes_wide();
# source those before this file.

#' Evacuation policy values swept, in days
#'
#' @details The doctrinal source names 30 days as a worked example and is
#'   explicit that the threshold is a command decision spanning at least 15 to
#'   60 days, so the sweep covers that stated range rather than a window around
#'   the shipped value. 21 is the shipped default and 30 the previous one, so
#'   both published configurations appear in the sweep.
POLICY_DAYS <- c(15L, 21L, 30L, 45L, 60L)

#' R2E holding bed establishments swept, as bed counts
#'
#' @details Absolute counts rather than multiples of today's pool, because an
#'   establishment is a force-structure number a planner states directly and the
#'   transport fleet sweep already varies its quantities the same way. 30 is the
#'   shipped establishment.
POLICY_HOLD_BEDS <- c(30L, 45L, 60L, 90L)

#' Forward surgical saturation thresholds swept, in queued casualties
#'
#' @details The threshold is the number waiting to be operated on at R2E at or
#'   above which a damage control casualty is released to strategic evacuation
#'   with the definitive repair outstanding. Zero disables the release and is
#'   the arm every other is measured against. R2E fields two operating theatre
#'   beds and three surgical sections, so the reachable queue is bounded by how
#'   far the backlog runs ahead of them rather than by the establishment itself;
#'   the range runs from a threshold that fires on the first case waiting to one
#'   deliberately beyond the queue a campaign reaches, so a value that is inert
#'   is identified as inert by measurement rather than assumed.
POLICY_SATURATION_THRESHOLDS <- c(0L, 1L, 2L, 3L, 5L, 8L, 12L, 16L, 24L)

#' Replications per swept value
#'
#' @details Matches the sustained-operations protocol's count, the responses
#'   here being measured over the same horizon and reduced from the same runs'
#'   monitors.
POLICY_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
POLICY_DAYS_HORIZON <- 360L

#' Closing window the forward stability responses are measured over, in days
#'
#' @details A campaign's closing state rather than its average, because the
#'   question is whether the system is stable at length; an average over the
#'   whole horizon mixes a settled system with one still filling.
POLICY_WINDOW_DAYS <- 90L

#' Resource-name patterns of the two pools the policy acts on
#'
#' @details The holding pool is where a retained casualty convalesces, and the
#'   intensive care pool is what a saturated holding pool blocks, step-down
#'   having nowhere to go.
POLICY_POOLS <- c(
  "R2E holding beds"   = "^b_r2eheavy_hold_[0-9]+_t[0-9]+$",
  "R2E intensive care" = "^b_r2eheavy_icu_[0-9]+_t[0-9]+$"
)

#' Resource-name patterns of the forward theatre the saturation release acts on
#'
#' @details Theatre entry is a two-stage seizure, an operating theatre bed and
#'   then a surgical section, so a casualty waiting to be operated on is queued
#'   on one or the other and counting either alone counts half the backlog.
#'   These are the monitor's names for the two stages `r2e_theatre_queue()`
#'   reads live inside the model (R/trajectories.R), so the swept response and
#'   the decision the sweep varies measure the same quantity.
POLICY_THEATRE_POOLS <- c(
  "^b_r2eheavy_ot_[0-9]+_t[0-9]+$",
  "^c_r2eheavy_surg_[0-9]+_.*_t[0-9]+$"
)

#' Attribute value recording the post-definitive intensive care pathway
#'
#' @details `post_definitive_pathway` is a simmer attribute and therefore
#'   numeric: 1 is the intensive care bed and 2 the degraded holding-bed
#'   fallback, as `R/trajectories.R` sets them.
POLICY_PATHWAY_ICU <- 1

#' Closing occupancy and mean queue of one pool over the campaign's last days
#'
#' @param resources Resource-monitor rows for one replication.
#' @param pattern Resource-name pattern selecting the pool's beds.
#' @param n_days Campaign length in days.
#' @param window_days Closing window to measure over.
#' @return Named numeric vector of occupancy and mean_queue, both NA where the
#'   monitor carries no row for the pool.
#'
#' @details The monitor records each bed separately, so the pool total is in
#'   none of its rows and is recovered by differencing each bed's series into
#'   changes and accumulating them in time order, which is what
#'   pool_queue_steps() does. Occupancy is the time-weighted mean of the pool's
#'   served count over the window, divided by the establishment.
policy_pool_state <- function(resources, pattern, n_days, window_days) {
  rows <- resources[grepl(pattern, resources$resource), ]
  if (nrow(rows) == 0) return(c(occupancy = NA_real_, mean_queue = NA_real_))

  edges <- c((n_days - window_days) * DAY_MIN, n_days * DAY_MIN)
  queue  <- pool_queue_steps(rows$resource, rows$time, rows$queue)
  server <- pool_queue_steps(rows$resource, rows$time, rows$server)
  capacity <- sum(tapply(rows$capacity, rows$resource, max))

  c(occupancy  = step_bin_means(server, edges) / capacity,
    mean_queue = step_bin_means(queue, edges))
}

#' Mean forward theatre queue over the campaign's closing days
#'
#' @param resources Resource-monitor rows for one replication.
#' @param n_days Campaign length in days.
#' @param window_days Closing window to measure over.
#' @return Time-weighted mean number of casualties waiting to be operated on at
#'   R2E, or NA where the monitor carries no theatre row.
#'
#' @details The two stages are summed rather than reported separately, a
#'   casualty being blocked on exactly one resource at a time, so the sum counts
#'   each waiting casualty once. Each stage's pool total is recovered by
#'   differencing its members' series and accumulating them in time order, as
#'   policy_pool_state() does, the monitor holding no row for a pool total.
policy_theatre_queue <- function(resources, n_days, window_days) {
  edges <- c((n_days - window_days) * DAY_MIN, n_days * DAY_MIN)
  stages <- vapply(POLICY_THEATRE_POOLS, function(pattern) {
    rows <- resources[grepl(pattern, resources$resource), ]
    if (nrow(rows) == 0) return(NA_real_)
    step_bin_means(pool_queue_steps(rows$resource, rows$time, rows$queue), edges)
  }, numeric(1))
  if (all(is.na(stages))) return(NA_real_)
  sum(stages, na.rm = TRUE)
}

#' Reduce one replication to the policy trade's response row
#'
#' @param env Wrapped simmer environment for one replication.
#' @param n_days Campaign length in days.
#' @param policy_days Evacuation policy the replication ran under, in days.
#' @param seed The replication's own seed.
#' @param window_days Closing window the stability responses are measured over.
#' @return One-row data frame of the response set.
#'
#' @details The national support base census is drawn under the replication's
#'   own seed and the caller's stream is restored, that length of stay being
#'   drawn by the analysis rather than by the simulation (README Further
#'   Development L30); without it the peak is a function of how many draws
#'   preceded it in the session rather than of the campaign.
#'
#'   The in-theatre share is computed as `analyse_run()` computes it, at the
#'   disposition decision rather than on completed returns to duty, a retained
#'   casualty's convalescence routinely outlasting the run. It is compared
#'   against the historical envelope rather than against another arm, so the
#'   two definitions must agree.
reduce_policy_replication <- function(env, n_days, policy_days, seed,
                                      window_days = POLICY_WINDOW_DAYS) {
  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  attributes <- simmer::get_mon_attributes(env)
  resources  <- simmer::get_mon_resources(env)

  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"),
                      suffix = c("", "_arrival"))

  hold <- policy_pool_state(resources, POLICY_POOLS[["R2E holding beds"]],
                            n_days, window_days)
  icu  <- policy_pool_state(resources, POLICY_POOLS[["R2E intensive care"]],
                            n_days, window_days)
  theatre_queue <- policy_theatre_queue(resources, n_days, window_days)

  decided <- wide[!is.na(wide$r2e_evac) & wide$r2e_evac == 1, ]
  boarded <- decided[!is.na(decided$ame_departure_time), ]

  dispositions <- wide[!is.na(wide$recovery_to_duty_days), ]
  in_theatre <- if (nrow(dispositions) == 0) {
    NA_real_
  } else {
    mean(dispositions$recovery_to_duty_days <= policy_days)
  }

  # post_definitive_pathway is a simmer attribute and so is numeric, not a
  # label: 1 is the intensive care bed and 2 the degraded holding-bed fallback
  # (R/trajectories.R). Comparing it against a string silently matches nothing
  # and reports a share of zero at every policy.
  post_definitive <- wide[!is.na(wide$post_definitive_pathway), ]
  icu_access <- if (nrow(post_definitive) == 0) {
    NA_real_
  } else {
    mean(post_definitive$post_definitive_pathway == POLICY_PATHWAY_ICU)
  }

  census <- with_preserved_rng({
    set.seed(seed)
    compute_role4_census(wide, env_data$vars$role4)
  })

  # Seeded identically to the census rather than continuing its stream, both
  # resting on the same length-of-stay draw: the day an operation is owed on has
  # to be the day the casualty is in a bed on.
  demand <- with_preserved_rng({
    set.seed(seed)
    compute_role4_surgical_demand(wide, env_data$vars$role4)
  })
  role4_peak <- 0
  role4_sustained <- NA_real_
  if (nrow(census) > 0) {
    by_day <- tapply(census$occupancy, census$day, sum)
    role4_peak <- max(by_day)
    closing <- by_day[as.integer(names(by_day)) > n_days - window_days]
    role4_sustained <- if (length(closing) > 0) mean(closing) else NA_real_
  }

  data.frame(
    policy_days        = policy_days,
    hold_occupancy     = hold[["occupancy"]],
    hold_mean_queue    = hold[["mean_queue"]],
    icu_occupancy      = icu[["occupancy"]],
    icu_mean_queue     = icu[["mean_queue"]],
    total_rtd          = sum(!is.na(wide$return_day)),
    total_dow          = sum(!is.na(wide$dow) & wide$dow == 1),
    dispositions       = nrow(dispositions),
    in_theatre_share   = in_theatre,
    post_definitive_icu_share = icu_access,
    evac_decisions     = nrow(decided),
    never_evacuated    = nrow(decided) - nrow(boarded),
    mean_evac_wait_days = if (nrow(boarded) == 0) {
      NA_real_
    } else {
      mean(boarded$ame_wait_minutes) / DAY_MIN
    },
    role4_sustained    = role4_sustained,
    role4_peak         = role4_peak,
    theatre_mean_queue = theatre_queue,
    released_unrepaired = sum(!is.na(wide$definitive_repair_outstanding) &
                                wide$definitive_repair_outstanding == 1),
    role4_operations   = if (nrow(demand) == 0) 0 else sum(demand$operations),
    role4_theatre_minutes = if (nrow(demand) == 0) 0 else sum(demand$theatre_minutes)
  )
}

#' Measure the policy response set across replications at one policy
#'
#' @param policy_days Evacuation policy in force, in days.
#' @param n_iterations Replications to run (default POLICY_REPLICATIONS).
#' @param n_days Campaign length in days (default POLICY_DAYS_HORIZON).
#' @param window_days Closing window the stability responses are measured over.
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @return Data frame with one row per replication, carrying the replication
#'   index and the responses reduce_policy_replication() reports.
#'
#' @details The seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed under the caller's generator kind, and the caller's
#'   stream is restored on exit. A caller who sets one control seed before each
#'   arm therefore gives replication $k$ of every arm the same parent stream, so
#'   the arms are paired under common random numbers and the difference between
#'   two policies is measured on matched campaigns rather than on independent
#'   ones.
#'
#'   The policy in force is passed to the reduction rather than read from
#'   `env_data` inside it, so that the in-theatre share is computed against the
#'   policy the replication actually ran under even if the global is restored
#'   between arms.
run_policy_measurement <- function(policy_days, n_iterations = POLICY_REPLICATIONS,
                                   n_days = POLICY_DAYS_HORIZON,
                                   window_days = POLICY_WINDOW_DAYS,
                                   max_cores = NULL) {
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
    row <- reduce_policy_replication(env, n_days, policy_days,
                                     seed = rep_seeds[i], window_days = window_days)
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

#' Enter a scenario's configuration with the policy and establishment overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param policy_days Evacuation policy to set, in days, or NULL to leave the
#'   profile's own value in place.
#' @param hold_beds R2E holding bed establishment to set, or NULL to leave the
#'   profile's own value in place.
#' @param saturation_threshold Forward surgical saturation threshold to set, in
#'   queued casualties, or NULL to leave the profile's own value in place.
#' @return Invisibly, the described configuration that was applied.
#'
#' @details The overrides enter at different points, which is why this
#'   function exists rather than a field setter. The policy and the saturation
#'   threshold are variables, and a variable has to be set after
#'   `build_environment()`: `resolve_scenario()`
#'   returns the raw parsed form, in which `vars` is a list of name and value
#'   pairs, and only `build_environment()` names them, so setting it on the
#'   resolved form writes a value nothing reads. The establishment is a bed
#'   count in `elms`, from which `build_environment()` constructs the resources
#'   themselves, so it has to be set before that call rather than after.
apply_policy_setting <- function(json_data, scenario, policy_days = NULL,
                                 hold_beds = NULL, saturation_threshold = NULL) {
  resolved <- resolve_scenario(json_data, scenario)
  if (!is.null(hold_beds)) resolved <- set_hold_establishment(resolved, hold_beds)

  described <- build_environment(resolved)
  if (!is.null(policy_days)) {
    described$vars$r2eheavy$recovery$evacuation_policy_days <- policy_days
  }
  # A variable, like the policy above it, so it is set on the described form
  # after build_environment() has named the value pairs rather than on the
  # resolved one, where nothing would read it.
  if (!is.null(saturation_threshold)) {
    described$vars$r2eheavy$second_surgery$saturation_queue_threshold <-
      saturation_threshold
  }
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Set the R2E holding bed establishment on a resolved configuration
#'
#' @param resolved Configuration as `resolve_scenario()` returns it.
#' @param hold_beds Number of R2E holding beds to establish.
#' @return The configuration with that establishment set.
#'
#' @details Fails rather than returning the configuration unchanged where the
#'   R2E element or its holding pool cannot be found, so a sweep cannot silently
#'   measure the shipped establishment at every point and report it as a
#'   frontier. The element is located by its `elm` name rather than by position,
#'   the order of `elms` being a property of the file rather than of the model.
set_hold_establishment <- function(resolved, hold_beds) {
  if (!is.numeric(hold_beds) || length(hold_beds) != 1 || is.na(hold_beds) ||
        hold_beds < 1) {
    stop("hold_beds must be a single bed count of at least 1, found '",
         paste(format(hold_beds), collapse = ", "), "'", call. = FALSE)
  }
  found <- FALSE
  for (i in seq_along(resolved$elms)) {
    if (!identical(resolved$elms[[i]]$elm, "r2eheavy")) next
    for (j in seq_along(resolved$elms[[i]]$beds)) {
      if (!identical(resolved$elms[[i]]$beds[[j]]$name, "hold")) next
      resolved$elms[[i]]$beds[[j]]$qty <- as.integer(hold_beds)
      found <- TRUE
    }
  }
  if (!found) {
    stop("no R2E holding bed pool found in elms, so the establishment ",
         "cannot be swept", call. = FALSE)
  }
  resolved
}

#' Mean and 95% confidence interval of every response across replications
#'
#' @param rows Per-replication responses as returned by
#'   run_policy_measurement().
#' @param arm_column Name of the column identifying which arm a row belongs to,
#'   which is summarised over rather than as a response.
#' @return Data frame of response, n_reps, mean, ci_lower and ci_upper, one row
#'   per response.
#'
#' @details A response no replication produced is reported with an `n_reps` of
#'   zero rather than dropped, so a reader can tell an absent measurement from a
#'   measured zero.
#'
#'   The arm column is named rather than fixed so that a sweep over another
#'   parameter of the same response set, such as the forward saturation
#'   threshold, is summarised by this function rather than by a second copy of
#'   it that could drift from it.
summarise_policy <- function(rows, arm_column = "policy_days") {
  responses <- setdiff(names(rows), c("replication", arm_column))
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

#' Paired difference in one response between two policies
#'
#' @param rows Per-replication responses carrying both policies.
#' @param response Name of the response to difference.
#' @param from Policy to measure the change from, in days.
#' @param to Policy to measure the change to, in days.
#' @param arm_column Name of the column `from` and `to` are values of.
#' @return One-row data frame of response, from, to, n_pairs, mean difference,
#'   its 95% interval and the paired t-test's p-value.
#'
#' @details The difference is taken within replication rather than between arm
#'   means, the arms sharing a seed vector, which removes the campaign-to-
#'   campaign variance that would otherwise swamp it. A response absent in
#'   either arm of a pair drops that pair rather than the whole comparison.
#'
#'   `n_pairs` is reported so that a difference resting on few pairs is visible,
#'   and the p-value so that a difference the count cannot establish is reported
#'   as unresolved rather than as absent.
policy_paired_difference <- function(rows, response, from, to,
                                     arm_column = "policy_days") {
  a <- rows[rows[[arm_column]] == from, c("replication", response)]
  b <- rows[rows[[arm_column]] == to, c("replication", response)]
  pairs <- merge(a, b, by = "replication", suffixes = c("_from", "_to"))
  d <- pairs[[paste0(response, "_to")]] - pairs[[paste0(response, "_from")]]
  d <- d[!is.na(d)]
  n <- length(d)
  m <- if (n > 0) mean(d) else NA_real_
  e <- if (n > 1) qt(0.975, df = n - 1) * sd(d) / sqrt(n) else NA_real_
  data.frame(
    response = response, from = from, to = to, n_pairs = n, difference = m,
    ci_lower = if (n > 1) m - e else NA_real_,
    ci_upper = if (n > 1) m + e else NA_real_,
    p_value  = if (n > 1 && sd(d) > 0) t.test(d)$p.value else NA_real_
  )
}

#' Replications a paired difference would need for a given half-width
#'
#' @param rows Per-replication responses carrying both policies.
#' @param response Name of the response to size.
#' @param from Policy to measure the change from, in days.
#' @param to Policy to measure the change to, in days.
#' @param half_width Half-width wanted on the difference, in the response's own
#'   units.
#' @param arm_column Name of the column `from` and `to` are values of.
#' @return The replication count required, rounded up, or NA where the measured
#'   pairs give no usable standard deviation.
#'
#' @details Uses the measured paired standard deviation and the normal
#'   approximation $n = (1.96 s / h)^2$, which is what the supplement's
#'   replication-count derivation uses. Reported so that an unresolved
#'   difference is stated with the count that would resolve it rather than left
#'   as a failure to measure.
policy_replications_for <- function(rows, response, from, to, half_width,
                                    arm_column = "policy_days") {
  a <- rows[rows[[arm_column]] == from, c("replication", response)]
  b <- rows[rows[[arm_column]] == to, c("replication", response)]
  pairs <- merge(a, b, by = "replication", suffixes = c("_from", "_to"))
  d <- pairs[[paste0(response, "_to")]] - pairs[[paste0(response, "_from")]]
  d <- d[!is.na(d)]
  if (length(d) < 2 || sd(d) == 0 || half_width <= 0) return(NA_real_)
  ceiling((qnorm(0.975) * sd(d) / half_width)^2)
}
