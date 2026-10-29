##############################################################################
## R/surgical_team_sweep.R                                                  ##
## The surgical team establishment at R2B and R2E, across replications       ##
##############################################################################
#
# The number of surgical teams is a force-structure input a planner states
# directly, and the planning paper's first recommendation, extending surgical
# cover towards 24 hours, rests on it. This module measures what adding teams
# buys: the R2E surgical sections (elms.r2eheavy.sub_elms.surg.qty), each of
# which the roster puts on one of two alternating shifts, and the R2B surgical
# sections per unit, a second of which takes the opposite shift to the first so
# that a unit's single operating theatre is staffed around the clock.
#
# Each replication is reduced to one row of scalar responses and a daily series
# of the R2E theatre queue and occupancy inside the forked worker that produced
# it, on the arrangement R/policy_sweep.R and R/hold_window.R both use. The
# scalar responses reuse reduce_hold_window_replication() for the counts that
# module defines, so the diversions, first surgeries and deferrals reported
# here are the quantities its table reports. The daily series is what lets the
# R2E theatre queue be classified converged or drifting by classify_stability()
# on the same block means the sustained-operations section uses, and its
# shortfall per day read from the same fit.
#
# Arms are paired on one control seed each, as the establishment sweeps are, so
# a difference against the shipped establishment is taken within replication.
# Changing an establishment alters the draws a replication takes, so the arms
# drift into different casualty streams from the first case onward and the
# pairing removes only the variance the shared parent seed carries (the
# argument docs/Methods.md makes for the hold window).
#
# Depends on R/analysis.R (build_attributes_wide(), pool_queue_steps(),
# step_bin_means()), R/policy_sweep.R, R/hold_window.R and R/long_horizon.R;
# source those before this file.

#' Replications per arm
SURGICAL_SWEEP_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
SURGICAL_SWEEP_DAYS <- 360L

#' Control seed every arm is set to before it runs
SURGICAL_SWEEP_SEED <- 42L

#' Closing window the queue and occupancy responses are measured over, in days
SURGICAL_SWEEP_WINDOW_DAYS <- 90L

#' Arm the paired differences are measured against
#'
#' @details The shipped establishment, three R2E surgical sections and one per
#'   R2B unit, so every difference reads as what departing from the force a
#'   planner holds today buys.
SURGICAL_SWEEP_BASELINE <- "R2E 3, R2B 1"

#' The arms swept, as a surgical section count per echelon
#'
#' @details R2E fields two operating theatres in every arm, so an arm of four or
#'   six sections adds staff and not theatres: four sections puts two on each
#'   shift, staffing both theatres around the clock, and six adds a third pair
#'   that has no theatre to operate in. The R2B arms add a second section per
#'   unit on the opposite shift, covering the unit's one theatre around the
#'   clock. The last arm applies both levers together.
SURGICAL_SWEEP_ARMS <- data.frame(
  arm          = c("R2E 3, R2B 1", "R2E 4, R2B 1", "R2E 6, R2B 1", "R2E 3, R2B 2",
                   "R2E 4, R2B 2"),
  r2e_sections = c(3L, 4L, 6L, 3L, 4L),
  r2b_sections = c(1L, 1L, 1L, 2L, 2L),
  stringsAsFactors = FALSE
)

#' Fewest 30-day blocks a campaign needs for the late-half fit to have a residual
#'
#' @details classify_stability() fits the trend over the second half of the
#'   blocks, and a line fitted to two points has no residual degrees of freedom
#'   to put an interval on. Six blocks leave four in the late half.
SURGICAL_SWEEP_MIN_BLOCKS <- 6L

#' Pool and series the daily classification is made on
SURGICAL_SWEEP_POOL <- "R2E operating theatres"

#' Responses the paired comparison against the shipped establishment reports
SURGICAL_SWEEP_PAIRED_RESPONSES <- c(
  "theatre_mean_queue", "diverted_offshift", "diverted_busy", "r2b_surgeries",
  "r2e_first_surgeries", "r2e_theatre_deferred", "released_unrepaired",
  "total_dow", "total_rtd", "post_definitive_icu_share"
)

#' Half-width each paired response is sized against, in its own units
#'
#' @details Chosen, as the other experiments' are, as the smallest difference
#'   that would change a planning decision: one casualty in the queue, ten
#'   casualties in a count of operations or diversions, one death, ten returns
#'   to duty and one percentage point on the share.
SURGICAL_SWEEP_HALF_WIDTHS <- c(
  theatre_mean_queue = 1, diverted_offshift = 10, diverted_busy = 10,
  r2b_surgeries = 10, r2e_first_surgeries = 10, r2e_theatre_deferred = 10,
  released_unrepaired = 10, total_dow = 1, total_rtd = 10,
  post_definitive_icu_share = 0.01
)

#' Set the surgical section count of one echelon on a resolved configuration
#'
#' @param resolved Configuration as `resolve_scenario()` returns it.
#' @param echelon Element name, `"r2eheavy"` or `"r2b"`.
#' @param sections Number of surgical sections the echelon fields per unit.
#' @return The configuration with that section count set.
#'
#' @details Fails rather than returning the configuration unchanged where the
#'   element or its surgical sub-element cannot be found, so a sweep cannot
#'   silently measure the shipped establishment at every arm. The element is
#'   located by name rather than by position, the order of `elms` being a
#'   property of the file rather than of the model.
set_surgical_sections <- function(resolved, echelon, sections) {
  if (!is.numeric(sections) || length(sections) != 1 || is.na(sections) ||
        sections < 1 || sections != round(sections)) {
    stop("surgical sections must be a single whole count of at least 1, found '",
         paste(format(sections), collapse = ", "), "'", call. = FALSE)
  }
  found <- FALSE
  for (i in seq_along(resolved$elms)) {
    if (!identical(resolved$elms[[i]]$elm, echelon)) next
    for (j in seq_along(resolved$elms[[i]]$sub_elms)) {
      if (!identical(resolved$elms[[i]]$sub_elms[[j]]$sub_elm, "surg")) next
      resolved$elms[[i]]$sub_elms[[j]]$qty <- as.integer(sections)
      found <- TRUE
    }
  }
  if (!found) {
    stop("no surgical sub-element found for '", echelon, "', so its sections ",
         "cannot be swept", call. = FALSE)
  }
  resolved
}

#' Enter a scenario's configuration with the surgical establishment overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param r2e_sections Surgical sections at the R2E unit.
#' @param r2b_sections Surgical sections at each R2B unit.
#' @return Invisibly, the described configuration that was applied.
#'
#' @details The section counts are structure, not variables, so they are set on
#'   the resolved form before `build_environment()` constructs the resources
#'   from them, as the holding establishment is in `apply_policy_setting()`.
apply_surgical_setting <- function(json_data, scenario, r2e_sections, r2b_sections) {
  resolved <- resolve_scenario(json_data, scenario)
  resolved <- set_surgical_sections(resolved, "r2eheavy", r2e_sections)
  resolved <- set_surgical_sections(resolved, "r2b", r2b_sections)
  described <- build_environment(resolved)
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Reduce one replication to the surgical establishment's responses
#'
#' @param env Wrapped simmer environment for one replication.
#' @param n_days Campaign length in days.
#' @param window_days Closing window the queue and occupancy are measured over.
#' @return A list of `row`, a one-row data frame of the scalar responses ending
#'   in the overflow census of each point of care over the closing window, and
#'   `series`, the daily R2E theatre queue and occupancy in the long form
#'   block_means() takes.
#'
#' @details The counts are those of reduce_hold_window_replication(); the
#'   window column that function carries is dropped, there being no window here.
#'   The theatre queue is the two-stage backlog policy_theatre_queue() reads,
#'   a bed and then a section, so a casualty waiting on either is counted once.
#'   The degraded care share is the post-definitive pathway's, as the policy
#'   sweep reports it.
reduce_surgical_replication <- function(env, n_days,
                                        window_days = SURGICAL_SWEEP_WINDOW_DAYS) {
  resources <- simmer::get_mon_resources(env)
  base <- reduce_hold_window_replication(env, window_min = NA_integer_)
  base$window_min <- NULL

  attributes <- simmer::get_mon_attributes(env)
  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  wide <- build_attributes_wide(attributes, arrivals)

  post_definitive <- wide[!is.na(wide$post_definitive_pathway), ]
  icu_access <- if (nrow(post_definitive) == 0) {
    NA_real_
  } else {
    mean(post_definitive$post_definitive_pathway == POLICY_PATHWAY_ICU)
  }

  row <- cbind(
    base,
    data.frame(
      theatre_mean_queue = policy_theatre_queue(resources, n_days, window_days),
      released_unrepaired = sum(!is.na(wide$definitive_repair_outstanding) &
                                  wide$definitive_repair_outstanding == 1),
      total_rtd = sum(!is.na(wide$return_day)),
      post_definitive_icu_share = icu_access
    ),
    overflow_window_census(resources, (n_days - window_days) * DAY_MIN, n_days * DAY_MIN)
  )

  pools <- reduce_pool_series(resources, n_days, env_data$elms)
  pools <- pools[pools$pool == SURGICAL_SWEEP_POOL, ]
  series <- rbind(
    data.frame(day = pools$day, series = "mean_queue", subject = SURGICAL_SWEEP_POOL,
               value = pools$mean_queue),
    data.frame(day = pools$day, series = "occupancy", subject = SURGICAL_SWEEP_POOL,
               value = pools$occupancy)
  )
  list(row = row, series = series)
}

#' Measure the surgical establishment's responses across replications at one arm
#'
#' @param n_iterations Replications to run (default SURGICAL_SWEEP_REPLICATIONS).
#' @param n_days Campaign length in days (default SURGICAL_SWEEP_DAYS).
#' @param window_days Closing window for the queue and occupancy responses.
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @param checkpoint_dir Directory each finished replication is saved to and read
#'   back from, or NULL for none. A replication is named by its index and seed, so
#'   a changed control seed never reads another seed's file.
#' @return A list of `rows`, one row per replication, and `series`, the daily
#'   series of every replication bound together with a `replication` column.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the caller's
#'   control seed under the caller's generator kind, and the caller's stream is
#'   restored on exit. The caller applies the arm's configuration first
#'   (apply_surgical_setting()); this function measures whatever is in force.
run_surgical_measurement <- function(n_iterations = SURGICAL_SWEEP_REPLICATIONS,
                                     n_days = SURGICAL_SWEEP_DAYS,
                                     window_days = SURGICAL_SWEEP_WINDOW_DAYS,
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
      file.path(checkpoint_dir, sprintf("replication_%03d_%d.rds", i, rep_seeds[i]))
    if (!is.null(saved) && file.exists(saved)) return(readRDS(saved))
    env <- run_once(n_days, seed = rep_seeds[i], write_files = FALSE)
    out <- reduce_surgical_replication(env, n_days, window_days)
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

#' Classify the R2E theatre queue of each arm and read its shortfall per day
#'
#' @param series Daily series of every arm in long form, carrying `arm`,
#'   `replication`, `day`, `series`, `subject` and `value`.
#' @param block_days Block length in days (default LONG_HORIZON_BLOCK_DAYS).
#' @param pool Pool whose daily mean queue is classified (default
#'   SURGICAL_SWEEP_POOL, the R2E operating theatres).
#' @return One row per arm of the queue's classification: its first, last and
#'   late-mean block levels, the late slope with its interval, the shortfall per
#'   day, the drift per block and `converged`, `drifting` or `degenerate`.
#'
#' @details The classification is classify_stability()'s, on the block means
#'   block_means() takes of the arm's own replications, so the verdict is the
#'   one the sustained-operations section reaches for the shipped establishment.
#'   The shortfall per day is the late slope divided by the block length, with
#'   the slope's interval, as `docs/Methods.md` defines it.
surgical_stability <- function(series, block_days = LONG_HORIZON_BLOCK_DAYS,
                               pool = SURGICAL_SWEEP_POOL) {
  queue <- series[series$series == "mean_queue" & series$subject == pool, ]
  n_blocks <- floor(max(queue$day) / block_days)
  if (n_blocks < SURGICAL_SWEEP_MIN_BLOCKS) {
    stop(sprintf(paste("a campaign of %d days holds %d blocks of %d days; the late-half fit",
                       "needs at least %d"), max(queue$day), n_blocks, block_days,
                 SURGICAL_SWEEP_MIN_BLOCKS), call. = FALSE)
  }
  do.call(rbind, lapply(unique(queue$arm), function(a) {
    blocks <- block_means(queue[queue$arm == a, ], block_days)
    st <- classify_stability(blocks)
    st$arm <- a
    st$shortfall_per_day <- st$slope / block_days
    st$shortfall_lower <- st$ci_lower / block_days
    st$shortfall_upper <- st$ci_upper / block_days
    st[, c("arm", "first", "last", "late_mean", "slope", "ci_lower", "ci_upper",
           "shortfall_per_day", "shortfall_lower", "shortfall_upper",
           "drift_per_block", "stability")]
  }))
}
