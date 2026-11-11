##############################################################################
## R/airlift_places_sweep.R                                                 ##
## Critical-care places per sortie, crossed with the sortie interval        ##
##############################################################################
#
# The critical-care places are the binding strategic airlift constraint at both
# intensities, and the planning paper recommends configuring more of each
# sortie for critical care; the airlift sweeps vary the interval and the
# reliability of sorties but always with the shipped split of the C-17A's 90
# places, 36 critical and 54 standard. This module moves places between the two
# pools with the total held at 90 (36/54, 45/45, 54/36 and 72/18), crossed with
# the shipped weekly interval and a three-day interval, so a planner can read
# whether reconfiguring the aircraft or flying more often clears the critical
# backlog, and what each costs the standard route.
#
# The split is set on the configured airframe's capacity block after
# build_environment(), as apply_airlift_setting() sets a role4.ame field; the
# seat pools take their sortie capacity from that block when a run is set up
# (add_strategic_evac(), R/replication.R), so no trajectory changes. Each
# replication is reduced inside the forked worker that produced it to the
# strategic airlift sweep's responses (reduce_airlift_replication()), the deaths
# of wounds of casualties awaiting airlift and in total, R2E intensive care and
# holding occupancy over the closing window, and each route's daily backlog
# series (reduce_route_backlog()), from which every campaign is classified
# collapsed or not by the rule the airlift collapse experiment applies.
#
# Arms are paired on one control seed each. The shipped split at the weekly and
# at the three-day interval are configurations the strategic airlift sweep also
# ran under the same seed, so their replications reproduce that sweep's rows.
#
# Depends on R/airlift.R, R/policy_sweep.R and R/replication.R; source those
# first.

#' Replications per arm
AIRLIFT_PLACES_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
AIRLIFT_PLACES_DAYS <- 360L

#' Control seed every arm is set to before it runs
AIRLIFT_PLACES_SEED <- 42L

#' Closing window the occupancy responses are measured over, in days
AIRLIFT_PLACES_WINDOW_DAYS <- 90L

#' Critical-care places per sortie swept, the rest of the airframe's going to
#' the standard pool
#'
#' @details 36 is the shipped split of the C-17A's 90 places; the range runs to
#'   twice it, 72, at which the standard pool keeps 18 places and may collapse
#'   in its turn. How far a real sortie can be reconfigured is not stated by any
#'   open-access source found (README Further Development L44), so 72 is an
#'   informed bound rather than a sourced configuration.
AIRLIFT_PLACES_CRITICAL <- c(36L, 45L, 54L, 72L)

#' Sortie intervals swept, in days: the shipped week and three days
AIRLIFT_PLACES_INTERVALS <- c(7L, 3L)

#' Arm label of a critical-place split and interval, as the output files name it
#'
#' @param critical Critical-care places per sortie.
#' @param standard Standard places per sortie.
#' @param interval Days between sorties.
#' @return The label, such as "45/45, 7 days".
airlift_places_label <- function(critical, standard, interval) {
  sprintf("%d/%d, %d days", critical, standard, interval)
}

#' The arms swept, as critical and standard places and a sortie interval
#'
#' @details The standard places are the shipped airframe's total less the
#'   critical places, so every arm flies the same 90 places; the total is read
#'   from the shipped configuration by the protocol check rather than here.
AIRLIFT_PLACES_ARMS <- local({
  grid <- expand.grid(critical = AIRLIFT_PLACES_CRITICAL, interval = AIRLIFT_PLACES_INTERVALS)
  grid$standard <- 90L - grid$critical
  data.frame(arm = airlift_places_label(grid$critical, grid$standard, grid$interval),
             critical = as.integer(grid$critical), standard = as.integer(grid$standard),
             interval = as.integer(grid$interval), stringsAsFactors = FALSE)
})

#' Arm every other arm is compared against: the shipped split, weekly
AIRLIFT_PLACES_BASELINE_ARM <- airlift_places_label(36L, 54L, 7L)

#' Responses the paired comparisons report
AIRLIFT_PLACES_PAIRED_RESPONSES <- c(
  "closing_backlog_critical", "closing_backlog_standard", "mean_wait_critical",
  "mean_wait_standard", "places_empty_critical", "places_empty_standard", "icu_occupancy",
  "hold_occupancy", "dow_awaiting_airlift", "total_dow", "role4_peak"
)

#' Half-width each paired response is sized against, in its own units
#'
#' @details The smallest difference that would change a planning decision, as
#'   the other experiments choose it: ten casualties of backlog, a day of wait,
#'   five places on a sortie, a percentage point of occupancy, one death and
#'   five patients of Role 4 census.
AIRLIFT_PLACES_HALF_WIDTHS <- c(
  closing_backlog_critical = 10, closing_backlog_standard = 10, mean_wait_critical = 1,
  mean_wait_standard = 1, places_empty_critical = 5, places_empty_standard = 5,
  icu_occupancy = 0.01, hold_occupancy = 0.01, dow_awaiting_airlift = 1, total_dow = 1,
  role4_peak = 5
)

#' Code of the died-of-wounds echelon for a casualty awaiting strategic airlift
#'
#' @details `dow_echelon` 5 (summarise_outcomes_by_echelon(),
#'   R/analysis_run_flow.R): an operated casualty dying at the death time it
#'   carries while queued for a seat.
DOW_ECHELON_AWAITING_AIRLIFT <- 5

#' Set the configured airframe's critical and standard places
#'
#' @param described Configuration as `build_environment()` returns it.
#' @param critical Critical-care places per sortie.
#' @param standard Standard places per sortie.
#' @return The configuration with those places set.
#'
#' @details Fails rather than returning the configuration unchanged where the
#'   configured airframe has no capacity block, and where the two counts do not
#'   sum to the airframe's places, so a sweep cannot measure the shipped split
#'   at every arm or fly a different aircraft than it reports.
set_airframe_places <- function(described, critical, standard) {
  #' Whether a value is a single whole count of at least zero
  #'
  #' @param x The value.
  #' @return TRUE or FALSE.
  ok <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0 && x == round(x)
  if (!ok(critical) || !ok(standard)) {
    stop("critical and standard places must each be a single whole count of at least 0, ",
         "found '", paste(format(critical), collapse = ", "), "' and '",
         paste(format(standard), collapse = ", "), "'", call. = FALSE)
  }
  role4 <- described$vars$role4
  id <- role4$ame$airframe
  key <- paste0("airframe_", id)
  if (is.null(id) || is.null(role4[[key]])) {
    stop("no capacity block found for the configured airframe '", format(id),
         "', so its places cannot be swept", call. = FALSE)
  }
  total <- role4[[key]]$critical_capacity + role4[[key]]$standard_capacity
  if (critical + standard != total) {
    stop(sprintf("places must sum to the airframe's %d, found %d critical and %d standard",
                 as.integer(total), as.integer(critical), as.integer(standard)), call. = FALSE)
  }
  described$vars$role4[[key]]$critical_capacity <- critical
  described$vars$role4[[key]]$standard_capacity <- standard
  described
}

#' Enter a scenario's configuration with the airframe split and interval overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param critical Critical-care places per sortie.
#' @param standard Standard places per sortie.
#' @param interval Days between sorties.
#' @return Invisibly, the described configuration that was applied.
#'
#' @details Both settings live in `vars`, which build_environment() builds, so
#'   they are set on the described form after it, as apply_airlift_setting()
#'   sets a role4.ame field.
apply_airlift_places_setting <- function(json_data, scenario, critical, standard, interval) {
  described <- apply_airlift_setting(json_data, scenario, "schedule_interval_days", interval)
  described <- set_airframe_places(described, critical, standard)
  assign("env_data", described, envir = globalenv())
  invisible(described)
}

#' Reduce one replication to the places sweep's responses and route series
#'
#' @param env Wrapped simmer environment for one replication.
#' @param n_days Campaign length in days.
#' @param seed The replication's own seed, for the Role 4 census.
#' @param window_days Closing window the occupancy and backlog are measured over.
#' @return A list of `row`, a one-row data frame of the scalar responses, and
#'   `series`, each route's daily backlog, decisions and places opened in the
#'   long form collapse_response() takes.
#'
#' @details The airlift responses are reduce_airlift_replication()'s, so a wait
#'   or an empty place is the quantity the strategic airlift sweep reports. Each
#'   route's closing backlog is its mean over the closing window, read from the
#'   series. R2E intensive care and holding occupancy are read by
#'   policy_pool_state() over the same window, a critical casualty awaiting its
#'   seat holding an intensive care bed or, when none is free, a holding bed.
reduce_airlift_places_replication <- function(env, n_days, seed,
                                              window_days = AIRLIFT_PLACES_WINDOW_DAYS) {
  base <- reduce_airlift_replication(env, n_days, seed)
  series <- reduce_route_backlog(env, n_days)
  resources <- simmer::get_mon_resources(env)
  attributes <- simmer::get_mon_attributes(env)

  #' Last value of one attribute per casualty
  #'
  #' @param key Attribute key.
  #' @return Named numeric vector keyed by casualty.
  last_value <- function(key) {
    rows <- attributes[attributes$key == key, ]
    rows <- rows[!duplicated(rows$name, fromLast = TRUE), ]
    stats::setNames(rows$value, rows$name)
  }
  dow <- last_value("dow")
  echelon <- last_value("dow_echelon")
  died <- names(dow)[dow == 1]
  #' A pool's occupancy over the closing window
  #'
  #' @param pattern Resource-name pattern of the pool's beds.
  #' @return The occupancy, a share of the establishment.
  occupancy <- function(pattern) {
    policy_pool_state(resources, pattern, n_days, window_days,
                      pool_establishment(env_data$elms, pattern))[["occupancy"]]
  }
  #' A route's mean backlog over the closing window
  #'
  #' @param label Route label, a name of AIRLIFT_COLLAPSE_ROUTES.
  #' @return The mean backlog.
  closing <- function(label) {
    rows <- series[series$series == "backlog" & series$subject == label &
                     series$day > n_days - window_days, ]
    mean(rows$value)
  }

  row <- cbind(base, data.frame(
    closing_backlog_critical = closing("Critical airlift"),
    closing_backlog_standard = closing("Standard airlift"),
    dow_awaiting_airlift = sum(!is.na(echelon[died]) &
                                 echelon[died] == DOW_ECHELON_AWAITING_AIRLIFT),
    total_dow = length(died),
    icu_occupancy = occupancy(POLICY_POOLS[["R2E intensive care"]]),
    hold_occupancy = occupancy(POLICY_POOLS[["R2E holding beds"]])
  ))
  list(row = row, series = series)
}

#' Measure the places sweep's responses across replications at one arm
#'
#' @param n_iterations Replications to run (default AIRLIFT_PLACES_REPLICATIONS).
#' @param n_days Campaign length in days (default AIRLIFT_PLACES_DAYS).
#' @param window_days Closing window for the occupancy and backlog responses.
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @param checkpoint_dir Directory each finished replication is saved to and
#'   read back from (checkpoint_replication()), or NULL for none.
#' @return A list of `rows`, one row per replication, and `series`, every
#'   replication's route series bound together with a `replication` column.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed under the caller's generator kind, and the caller's
#'   stream is restored on exit. The caller applies the arm's configuration
#'   first (apply_airlift_places_setting()); this function measures whatever is
#'   in force.
run_airlift_places_measurement <- function(n_iterations = AIRLIFT_PLACES_REPLICATIONS,
                                           n_days = AIRLIFT_PLACES_DAYS,
                                           window_days = AIRLIFT_PLACES_WINDOW_DAYS,
                                           max_cores = NULL, checkpoint_dir = NULL) {
  rng_state <- capture_rng_state()
  on.exit(restore_rng_state(rng_state), add = TRUE)

  rep_seeds <- sample.int(.Machine$integer.max, n_iterations)
  RNGkind("L'Ecuyer-CMRG")

  #' Run one replication and return its reduced responses alone
  #'
  #' @param i Index of the replication, into `rep_seeds`.
  #' @return The replication's row and series, each carrying its index.
  worker <- function(i) {
    checkpoint_replication(function() {
      env <- run_once(n_days, seed = rep_seeds[i], write_files = FALSE)
      out <- reduce_airlift_places_replication(env, n_days, rep_seeds[i], window_days)
      out$row$replication <- i
      out$series$replication <- i
      out
    }, checkpoint_dir, i, rep_seeds[i], n_days)
  }

  dispatched <- dispatch_replications(worker, n_iterations, max_cores)
  usable     <- vapply(dispatched, function(x) is.list(x) && !is.null(x$row), logical(1))
  if (!all(usable)) {
    stop(sprintf("%d of %d replications did not complete", sum(!usable), n_iterations),
         call. = FALSE)
  }
  list(rows = do.call(rbind, lapply(dispatched, `[[`, "row")),
       series = do.call(rbind, lapply(dispatched, `[[`, "series")))
}

#' Classify every campaign of every arm collapsed or not, on each route
#'
#' @param series Route series of every arm, carrying `arm` and `replication`.
#' @param n_days Campaign length in days.
#' @return One row per arm, replication and route, collapse_response()'s
#'   columns with the arm named.
#'
#' @details Each arm is classified against the places its own sorties open on
#'   each route, read from AIRLIFT_PLACES_ARMS, so the materiality condition of
#'   collapse_response() moves with the split rather than staying at the
#'   shipped 36 and 54.
airlift_places_collapse <- function(series, n_days) {
  do.call(rbind, lapply(unique(series$arm), function(a) {
    arm <- AIRLIFT_PLACES_ARMS[AIRLIFT_PLACES_ARMS$arm == a, ]
    if (nrow(arm) != 1L) {
      stop(sprintf("airlift_places_collapse: '%s' is not a swept arm", a), call. = FALSE)
    }
    places <- stats::setNames(c(arm$critical, arm$standard), names(AIRLIFT_COLLAPSE_ROUTES))
    out <- collapse_response(series[series$arm == a, ], n_days, places)
    cbind(data.frame(arm = rep(a, nrow(out)), stringsAsFactors = FALSE), out)
  }))
}
