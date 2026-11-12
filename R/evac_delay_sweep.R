##############################################################################
## R/evac_delay_sweep.R                                                     ##
## The forward evacuation delay, across replications                        ##
##############################################################################
#
# Forward casualty evacuation in the shipped model is a road move of minutes,
# taken as soon as a vehicle is free. Reports from Ukraine describe casualties
# held forward for hours to days, and in one account twelve days, because the
# evacuation axis is closed by drone surveillance (README Further Development
# L47). This module measures what holding casualties forward does to the trauma
# system: no delay (the shipped configuration) against mean delays of six hours,
# one day, three, seven, fourteen and thirty days, each casualty waiting under
# prolonged casualty care without a vehicle (r1_forward_wait()).
#
# Each arm's delay is triangular from half to one and a half times its nominal
# value, with the nominal value as the mode and so as the mean, so that the arms
# differ in scale and not in shape. The prolonged-care multiplier is held at its
# shipped value, so the sweep measures the delay alone.
#
# Each replication is reduced to one row of scalar responses and a daily series
# of the R2B and R2E pools inside the forked worker that produced it, on the
# arrangement R/triage_sweep.R uses: the responses are
# reduce_triage_replication()'s, with the casualties held forward, those who
# died while held, the mean delay served and the PMV ambulance fleet's
# utilisation. The shipped arm is the triage sweep's shipped arm under the same
# seed and reproduces its replications.
#
# Arms are paired on one control seed each, though not on one casualty stream:
# with the delay on, each casualty takes a draw for its delay and the wait moves
# every later event, so the pairing removes only the variance the shared parent
# seed carries.
#
# Depends on R/analysis.R, R/policy_sweep.R, R/hold_window.R, R/long_horizon.R,
# R/airlift.R, R/surgical_team_sweep.R, R/icu_bed_sweep.R and R/triage_sweep.R;
# source those first.

#' Replications per arm
EVAC_DELAY_SWEEP_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
EVAC_DELAY_SWEEP_DAYS <- 360L

#' Control seed every arm is set to before it runs
EVAC_DELAY_SWEEP_SEED <- 42L

#' Closing window the queue and occupancy responses are measured over, in days
EVAC_DELAY_SWEEP_WINDOW_DAYS <- 90L

#' Nominal forward evacuation delays swept, in hours
#'
#' @details Zero is the shipped configuration, the delay disabled. Six hours to
#'   fourteen days span the reported range; thirty days is an informed upper
#'   bound beyond the longest open-access account found.
EVAC_DELAY_SWEEP_HOURS <- c(0, 6, 24, 72, 168, 336, 720)

#' Lower and upper bound of each arm's triangular delay, as fractions of its mode
EVAC_DELAY_SPREAD <- c(0.5, 1.5)

#' Arm label of a nominal delay, as the summary and paired files name it
#'
#' @param hours Nominal delay in hours.
#' @return The label, such as "No delay", "6 hours" or "7 days".
evac_delay_arm_label <- function(hours) {
  ifelse(hours == 0, "No delay",
         ifelse(hours < 24, sprintf("%d hours", as.integer(hours)),
                ifelse(hours == 24, "1 day", sprintf("%d days", as.integer(hours / 24)))))
}

#' The arms swept, one per nominal delay
EVAC_DELAY_SWEEP_ARMS <- data.frame(
  arm = evac_delay_arm_label(EVAC_DELAY_SWEEP_HOURS),
  hours = EVAC_DELAY_SWEEP_HOURS,
  stringsAsFactors = FALSE
)

#' Arm the paired differences are measured against
EVAC_DELAY_SWEEP_BASELINE_ARM <- evac_delay_arm_label(0)

#' Pools whose daily queue, or overflow census, is classified
#'
#' @details The triage sweep's R2E pools and overflow points, with the R2B
#'   theatres, holding beds and reception overflow that casualties released
#'   forward in numbers reach first. Written out rather than built from
#'   TRIAGE_SWEEP_POOLS, since Shiny sources every file under `R/` in
#'   alphabetical order and this file precedes `R/triage_sweep.R`;
#'   `tests/checks/check_evac_delay_sweep.R` holds the two lists equal.
EVAC_DELAY_SWEEP_POOLS <- c("R2B operating theatres", "R2B holding beds",
                            "R2B reception overflow", "R2E operating theatres",
                            "R2E intensive care", "R2E holding beds",
                            "R2E post-operative overflow", "R2E staging overflow")

#' Responses the paired comparison against the shipped configuration reports
EVAC_DELAY_SWEEP_PAIRED_RESPONSES <- c(
  "died_held_forward", "total_dow", "treated_dow_rate", "first_incision_hours",
  "surgical_delivery_share", "theatre_mean_queue", "overflow_r2e_post_op",
  "hold_mean_queue", "icu_occupancy", "diverted_offshift", "diverted_busy",
  "total_rtd", "pmv_utilisation"
)

#' Half-width each paired response is sized against, in its own units
#'
#' @details The smallest difference that would change a planning decision, as
#'   the triage sweep chooses it: one death, a tenth of a percentage point on a
#'   mortality rate, an hour to first incision, a percentage point on a share or
#'   utilisation, one casualty in a queue or census, ten casualties in a count
#'   and ten returns to duty.
EVAC_DELAY_SWEEP_HALF_WIDTHS <- c(
  died_held_forward = 1, total_dow = 1, treated_dow_rate = 0.001,
  first_incision_hours = 1, surgical_delivery_share = 0.01, theatre_mean_queue = 1,
  overflow_r2e_post_op = 1, hold_mean_queue = 1, icu_occupancy = 0.01,
  diverted_offshift = 10, diverted_busy = 10, total_rtd = 10, pmv_utilisation = 0.01
)

#' Set the forward evacuation delay on a described configuration
#'
#' @param described Configuration as `build_environment()` returns it.
#' @param hours Nominal delay in hours; zero disables the delay.
#' @return The configuration with the delay set: disabled at zero, otherwise
#'   enabled and triangular over EVAC_DELAY_SPREAD of the nominal delay, with
#'   the nominal delay as its mode.
#'
#' @details Fails rather than returning the configuration unchanged where the
#'   `r1.evacuation_delay` block cannot be found, so a sweep cannot silently
#'   measure the shipped configuration at every arm, and fails on a delay that
#'   is not a single number of at least zero. The delay is a variable rather
#'   than a count in `elms`, so it is set on the described form, as
#'   set_priority_split() sets the triage split.
set_evacuation_delay <- function(described, hours) {
  if (!(is.numeric(hours) && length(hours) == 1L && is.finite(hours) && hours >= 0)) {
    stop("hours must be a single number of at least 0, found '",
         paste(format(hours), collapse = ", "), "'", call. = FALSE)
  }
  if (is.null(described$vars$r1$evacuation_delay$enabled)) {
    stop("no forward evacuation delay found at vars.r1.evacuation_delay, so the ",
         "delay cannot be swept", call. = FALSE)
  }
  if (hours == 0) {
    described$vars$r1$evacuation_delay$enabled <- 0
    return(described)
  }
  mode <- hours * 60
  described$vars$r1$evacuation_delay$enabled <- 1
  described$vars$r1$evacuation_delay$min <- mode * EVAC_DELAY_SPREAD[1]
  described$vars$r1$evacuation_delay$mode <- mode
  described$vars$r1$evacuation_delay$max <- mode * EVAC_DELAY_SPREAD[2]
  described
}

#' Enter a scenario's configuration with the forward evacuation delay overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param hours Nominal delay in hours.
#' @return Invisibly, the described configuration that was applied.
apply_evac_delay_setting <- function(json_data, scenario, hours) {
  described <- build_environment(resolve_scenario(json_data, scenario))
  described <- set_evacuation_delay(described, hours)
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Mean utilisation of the PMV ambulance fleet over a campaign
#'
#' @param resources Resource-monitor rows of one replication.
#' @param horizon_min Campaign length in minutes.
#' @return The time-weighted mean of vehicles in use over the campaign divided
#'   by the fleet, zero where the fleet saw no use.
pmv_utilisation <- function(resources, horizon_min) {
  amb <- resources[grepl("^t_PMVAmb_", resources$resource), ]
  fleet <- length(unique(amb$resource))
  if (fleet == 0L) return(0)
  steps <- pool_queue_steps(amb$resource, amb$time, amb$server)
  step_bin_means(steps, c(0, horizon_min)) / fleet
}

#' Reduce one replication to the forward evacuation delay's responses
#'
#' @param env Wrapped simmer environment for one replication.
#' @param n_days Campaign length in days.
#' @param seed The replication's own seed, for the Role 4 census.
#' @param window_days Closing window the queue and occupancy are measured over.
#' @return A list of `row`, a one-row data frame of the scalar responses, and
#'   `series`, the daily queue and occupancy of every pool EVAC_DELAY_SWEEP_POOLS
#'   names, in the long form block_means() takes.
#'
#' @details The responses are reduce_triage_replication()'s, and:
#'   - `held_forward`, casualties held forward before their evacuation;
#'   - `died_held_forward`, those who died while held;
#'   - `mean_delay_days`, the mean delay drawn, in days;
#'   - `pmv_utilisation`, the PMV ambulance fleet's utilisation over the
#'     campaign, which the wait must leave unchanged since it holds no vehicle.
#'   `first_incision_hours` is measured from arrival, so it carries the delay.
reduce_evac_delay_replication <- function(env, n_days, seed,
                                          window_days = EVAC_DELAY_SWEEP_WINDOW_DAYS) {
  base <- reduce_triage_replication(env, n_days, seed, window_days)$row

  resources  <- simmer::get_mon_resources(env)
  attributes <- simmer::get_mon_attributes(env)
  delays <- attributes$value[attributes$key == "evac_delay_min"]

  row <- cbind(
    base,
    data.frame(
      held_forward = length(delays),
      died_held_forward = sum(attributes$key == "evac_delay_died"),
      mean_delay_days = if (length(delays) == 0) 0 else mean(delays) / DAY_MIN,
      pmv_utilisation = pmv_utilisation(resources, n_days * DAY_MIN)
    )
  )

  pools <- reduce_pool_series(resources, n_days, env_data$elms)
  pools <- pools[pools$pool %in% EVAC_DELAY_SWEEP_POOLS, ]
  series <- do.call(rbind, lapply(EVAC_DELAY_SWEEP_POOLS, function(p) {
    rows <- pools[pools$pool == p, ]
    rbind(
      data.frame(day = rows$day, series = "mean_queue", subject = p, value = rows$mean_queue),
      data.frame(day = rows$day, series = "occupancy", subject = p, value = rows$occupancy)
    )
  }))
  # An overflow point has no establishment and so no occupancy row.
  list(row = row, series = series[!is.na(series$value), ])
}

#' Measure the forward evacuation delay's responses across replications at one arm
#'
#' @param n_iterations Replications to run (default EVAC_DELAY_SWEEP_REPLICATIONS).
#' @param n_days Campaign length in days (default EVAC_DELAY_SWEEP_DAYS).
#' @param window_days Closing window for the queue and occupancy responses.
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @param checkpoint_dir Directory each finished replication is saved to and
#'   read back from, or NULL for none (checkpoint_replication()).
#' @return A list of `rows`, one row per replication, and `series`, the daily
#'   series of every replication bound together with a `replication` column.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed under the caller's generator kind, and the caller's
#'   stream is restored on exit. The caller applies the arm's configuration first
#'   (apply_evac_delay_setting()); this function measures whatever is in force.
run_evac_delay_measurement <- function(n_iterations = EVAC_DELAY_SWEEP_REPLICATIONS,
                                       n_days = EVAC_DELAY_SWEEP_DAYS,
                                       window_days = EVAC_DELAY_SWEEP_WINDOW_DAYS,
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
    #' Run and reduce the replication
    #'
    #' @return The replication's row and series, each carrying its index.
    compute <- function() {
      env <- run_once(n_days, seed = rep_seeds[i], write_files = FALSE)
      out <- reduce_evac_delay_replication(env, n_days, rep_seeds[i], window_days)
      out$row$replication <- i
      out$series$replication <- i
      out
    }
    checkpoint_replication(compute, checkpoint_dir, i, rep_seeds[i], n_days)
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

#' Classify each pool's queue at every arm and read its shortfall per day
#'
#' @param series Daily series of every arm in long form, carrying `arm`,
#'   `replication`, `day`, `series`, `subject` and `value`.
#' @param block_days Block length in days (default LONG_HORIZON_BLOCK_DAYS).
#' @return One row per arm and pool, surgical_stability()'s columns with the
#'   pool named.
evac_delay_stability <- function(series, block_days = LONG_HORIZON_BLOCK_DAYS) {
  pools <- EVAC_DELAY_SWEEP_POOLS[EVAC_DELAY_SWEEP_POOLS %in% series$subject]
  do.call(rbind, lapply(pools, function(p) {
    st <- surgical_stability(series, block_days, pool = p)
    st$pool <- p
    st
  }))
}
