##############################################################################
## R/icu_bed_sweep.R                                                        ##
## The R2E intensive care bed establishment, across replications             ##
##############################################################################
#
# R2E intensive care is the pool whose shortfall the results paper measures
# most severely, and no experiment had varied its establishment. This module
# measures what adding beds buys: the shipped four R2E intensive care beds
# (elms.r2eheavy.beds.icu.qty) against six, eight, twelve and sixteen.
#
# An intensive care bed seizure at R2E takes the bed alone. The R2E intensive
# care sections (an intensivist and four nurses each) are built as resources
# but are not seized by the stabilisation, post-operative or pre-flight stays
# (R/trajectories.R builds `icu_team` and no stay uses it), so staff do not
# constrain the pool and sweeping beds alone measures exactly what the model
# represents. The nurse-to-bed ratio each arm implies is reported with the
# sweep, as a cost the model does not charge.
#
# Each replication is reduced to one row of scalar responses and a daily series
# of the R2E intensive care and holding queues inside the forked worker that
# produced it, on the arrangement R/surgical_team_sweep.R uses. The scalar
# responses are those of reduce_policy_replication(), so the intensive care
# and holding occupancy and queue, returns to duty, deaths of wounds, the
# post-definitive intensive care access, the strategic evacuation wait and the
# Role 4 census are the quantities the policy sweep reports. The daily series
# lets each queue be classified converged or drifting by classify_stability()
# on the block means the sustained-operations section uses.
#
# Arms are paired on one control seed each, as the other establishment sweeps
# are, so a difference against the shipped establishment is taken within
# replication; changing the establishment alters the draws a replication takes,
# so the pairing removes only the variance the shared parent seed carries.
#
# Depends on R/analysis.R, R/policy_sweep.R, R/hold_window.R, R/long_horizon.R
# and R/surgical_team_sweep.R (surgical_stability()); source those first.

#' Replications per arm
ICU_BED_SWEEP_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
ICU_BED_SWEEP_DAYS <- 360L

#' Control seed every arm is set to before it runs
ICU_BED_SWEEP_SEED <- 42L

#' Closing window the queue and occupancy responses are measured over, in days
ICU_BED_SWEEP_WINDOW_DAYS <- 90L

#' R2E intensive care bed establishments swept
#'
#' @details Four is the shipped establishment; the range runs to four times it
#'   so that the smallest establishment at which the pool converges is found or
#'   reported as beyond the swept range.
ICU_BED_SWEEP_BEDS <- c(4L, 6L, 8L, 12L, 16L)

#' Establishment the paired differences are measured against
ICU_BED_SWEEP_BASELINE <- 4L

#' Arm label of an establishment, as the summary and paired files name it
#'
#' @param icu_beds R2E intensive care bed count.
#' @return The label, such as "8 beds".
icu_bed_arm_label <- function(icu_beds) sprintf("%d beds", icu_beds)

#' The arms swept, one per establishment
ICU_BED_SWEEP_ARMS <- data.frame(
  arm = icu_bed_arm_label(ICU_BED_SWEEP_BEDS),
  icu_beds = ICU_BED_SWEEP_BEDS,
  stringsAsFactors = FALSE
)

#' Arm the paired differences are measured against
ICU_BED_SWEEP_BASELINE_ARM <- icu_bed_arm_label(ICU_BED_SWEEP_BASELINE)

#' Pools whose daily queue, or overflow census, is classified
#'
#' @details The two bed pools and the two overflow points their saturation
#'   spills into, the post-operative stays and the evacuees staged for flight
#'   (`OVERFLOW_POOLS`, R/queue_series.R). For an overflow point the classified
#'   series is its census.
ICU_BED_SWEEP_POOLS <- c("R2E intensive care", "R2E holding beds",
                         "R2E post-operative overflow", "R2E staging overflow")

#' Nurses an R2E intensive care section fields, for the staffing ratio reported
#'
#' @details From the section in `env_data.json`: three sections of one
#'   intensivist and four nurses. The ratio is reported, not simulated, since no
#'   stay seizes the section.
ICU_BED_SWEEP_NURSES_PER_SECTION <- 4L

#' Responses the paired comparison against the shipped establishment reports
ICU_BED_SWEEP_PAIRED_RESPONSES <- c(
  "icu_mean_queue", "hold_mean_queue", "icu_occupancy", "post_definitive_icu_share",
  "postop_degraded_share", "r2e_theatre_deferred", "total_dow", "total_rtd",
  "mean_evac_wait_critical", "role4_peak", "role4_sustained"
)

#' Half-width each paired response is sized against, in its own units
#'
#' @details The smallest difference that would change a planning decision, as
#'   the other experiments choose it: one casualty in a queue, a percentage
#'   point on a share or occupancy, ten casualties in a count, one death, ten
#'   returns to duty, one day of wait and five patients of Role 4 census.
ICU_BED_SWEEP_HALF_WIDTHS <- c(
  icu_mean_queue = 1, hold_mean_queue = 1, icu_occupancy = 0.01,
  post_definitive_icu_share = 0.01, postop_degraded_share = 0.01,
  r2e_theatre_deferred = 10, total_dow = 1, total_rtd = 10,
  mean_evac_wait_critical = 1, role4_peak = 5, role4_sustained = 5
)

#' Set the R2E intensive care bed establishment on a resolved configuration
#'
#' @param resolved Configuration as `resolve_scenario()` returns it.
#' @param icu_beds Number of R2E intensive care beds to establish.
#' @return The configuration with that establishment set.
#'
#' @details Fails rather than returning the configuration unchanged where the
#'   R2E element or its intensive care pool cannot be found, so a sweep cannot
#'   silently measure the shipped establishment at every arm. The element is
#'   located by name rather than by position.
set_icu_establishment <- function(resolved, icu_beds) {
  if (!is.numeric(icu_beds) || length(icu_beds) != 1 || is.na(icu_beds) ||
        icu_beds < 1 || icu_beds != round(icu_beds)) {
    stop("icu_beds must be a single whole bed count of at least 1, found '",
         paste(format(icu_beds), collapse = ", "), "'", call. = FALSE)
  }
  found <- FALSE
  for (i in seq_along(resolved$elms)) {
    if (!identical(resolved$elms[[i]]$elm, "r2eheavy")) next
    for (j in seq_along(resolved$elms[[i]]$beds)) {
      if (!identical(resolved$elms[[i]]$beds[[j]]$name, "icu")) next
      resolved$elms[[i]]$beds[[j]]$qty <- as.integer(icu_beds)
      found <- TRUE
    }
  }
  if (!found) {
    stop("no R2E intensive care bed pool found in elms, so the establishment ",
         "cannot be swept", call. = FALSE)
  }
  resolved
}

#' Enter a scenario's configuration with the R2E intensive care beds overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param icu_beds R2E intensive care bed establishment to set.
#' @return Invisibly, the described configuration that was applied.
#'
#' @details The establishment is a bed count in `elms`, from which
#'   `build_environment()` constructs the resources, so it is set on the
#'   resolved form before that call, as `apply_policy_setting()` sets holding
#'   beds.
apply_icu_bed_setting <- function(json_data, scenario, icu_beds) {
  resolved <- resolve_scenario(json_data, scenario)
  resolved <- set_icu_establishment(resolved, icu_beds)
  described <- build_environment(resolved)
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Nurses per intensive care bed an establishment implies
#'
#' @param icu_beds R2E intensive care beds.
#' @param sections R2E intensive care sections fielded.
#' @return The staffed nurse-to-bed ratio, which falls as beds are added to a
#'   fixed number of sections.
icu_nurses_per_bed <- function(icu_beds, sections) {
  sections * ICU_BED_SWEEP_NURSES_PER_SECTION / icu_beds
}

#' Reduce one replication to the intensive care establishment's responses
#'
#' @param env Wrapped simmer environment for one replication.
#' @param n_days Campaign length in days.
#' @param seed The replication's own seed, for the Role 4 census.
#' @param window_days Closing window the queue and occupancy are measured over.
#' @return A list of `row`, a one-row data frame of the scalar responses, and
#'   `series`, the daily R2E intensive care and holding queue and occupancy in
#'   the long form block_means() takes.
#'
#' @details The scalar responses are reduce_policy_replication()'s, run under
#'   the evacuation policy in force. The post-operative degraded share is the
#'   share of casualties whose post-operative stay was taken in a holding bed or
#'   overflow (`post_op_pathway` 2 or 3); the critical wait is the strategic evacuation wait
#'   of the critical route; the theatre deferrals are `surgery_deferred`, as
#'   reduce_hold_window_replication() reads them.
reduce_icu_bed_replication <- function(env, n_days, seed,
                                       window_days = ICU_BED_SWEEP_WINDOW_DAYS) {
  policy_days <- env_data$vars$r2eheavy$recovery$evacuation_policy_days
  base <- reduce_policy_replication(env, n_days, policy_days, seed, window_days)
  base$policy_days <- NULL

  resources  <- simmer::get_mon_resources(env)
  attributes <- simmer::get_mon_attributes(env)
  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"),
                      suffix = c("", "_arrival"))

  post_op <- wide[!is.na(wide$post_op_pathway), ]
  degraded <- if (nrow(post_op) == 0) NA_real_ else mean(post_op$post_op_pathway >= 2)

  decided <- wide[!is.na(wide$r2e_evac) & wide$r2e_evac == 1, ]
  critical <- decided[!is.na(decided$ame_departure_time) & !is.na(decided$ame_route) &
                        decided$ame_route == AIRLIFT_ROUTE_CRITICAL, ]
  critical_wait <- if (nrow(critical) == 0) {
    NA_real_
  } else {
    mean(critical$ame_wait_minutes) / DAY_MIN
  }

  row <- cbind(
    base,
    data.frame(
      postop_degraded_share = degraded,
      r2e_theatre_deferred = sum(!is.na(wide$surgery_deferred) &
                                   wide$surgery_deferred == 1),
      mean_evac_wait_critical = critical_wait
    )
  )

  pools <- reduce_pool_series(resources, n_days, env_data$elms)
  pools <- pools[pools$pool %in% ICU_BED_SWEEP_POOLS, ]
  series <- do.call(rbind, lapply(ICU_BED_SWEEP_POOLS, function(p) {
    rows <- pools[pools$pool == p, ]
    rbind(
      data.frame(day = rows$day, series = "mean_queue", subject = p,
                 value = rows$mean_queue),
      data.frame(day = rows$day, series = "occupancy", subject = p,
                 value = rows$occupancy)
    )
  }))
  # An overflow point has no establishment and so no occupancy row.
  list(row = row, series = series[!is.na(series$value), ])
}

#' Measure the intensive care establishment's responses across replications
#'
#' @param n_iterations Replications to run (default ICU_BED_SWEEP_REPLICATIONS).
#' @param n_days Campaign length in days (default ICU_BED_SWEEP_DAYS).
#' @param window_days Closing window for the queue and occupancy responses.
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @param checkpoint_dir Directory each finished replication is saved to and
#'   read back from, or NULL for none. A replication is named by its index and
#'   seed, so a changed control seed never reads another seed's file.
#' @return A list of `rows`, one row per replication, and `series`, the daily
#'   series of every replication bound together with a `replication` column.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed under the caller's generator kind, and the caller's
#'   stream is restored on exit. The caller applies the arm's configuration first
#'   (apply_icu_bed_setting()); this function measures whatever is in force.
run_icu_bed_measurement <- function(n_iterations = ICU_BED_SWEEP_REPLICATIONS,
                                    n_days = ICU_BED_SWEEP_DAYS,
                                    window_days = ICU_BED_SWEEP_WINDOW_DAYS,
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
    saved <- if (is.null(checkpoint_dir)) NULL else
      file.path(checkpoint_dir,
                sprintf("replication_%03d_%d_d%d.rds", i, rep_seeds[i], n_days))
    if (!is.null(saved) && file.exists(saved)) return(readRDS(saved))
    env <- run_once(n_days, seed = rep_seeds[i], write_files = FALSE)
    out <- reduce_icu_bed_replication(env, n_days, rep_seeds[i], window_days)
    out$row$replication <- i
    out$series$replication <- i
    if (!is.null(saved)) {
      saveRDS(out, paste0(saved, ".tmp"))
      file.rename(paste0(saved, ".tmp"), saved)
    }
    out
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
#'
#' @details The classification is surgical_stability()'s, so the verdict is the
#'   one the sustained-operations section reaches and the shortfall per day is
#'   read from the same late-half slope. A pool the series does not carry is
#'   not classified, so a set measured before the overflow points existed is
#'   classified on the pools it has.
icu_bed_stability <- function(series, block_days = LONG_HORIZON_BLOCK_DAYS) {
  pools <- ICU_BED_SWEEP_POOLS[ICU_BED_SWEEP_POOLS %in% series$subject]
  do.call(rbind, lapply(pools, function(p) {
    st <- surgical_stability(series, block_days, pool = p)
    st$pool <- p
    st
  }))
}
