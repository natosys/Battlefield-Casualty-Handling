##############################################################################
## R/ot_bed_sweep.R                                                         ##
## The R2E operating theatre establishment, crossed with surgical sections  ##
##############################################################################
#
# The R2E theatre count is held at two in every arm of the surgical team sweep,
# so whether a planner should add theatres, surgical sections or both cannot be
# read from it. This module measures it: the shipped two theatres
# (elms.r2eheavy.beds.ot.qty) against three and four, each crossed with the
# R2E surgical section count (elms.r2eheavy.sub_elms.surg.qty).
#
# What limits the theatre is the number of sections on shift at once against
# the number of theatres, not the section count itself. build_env() puts odd
# sections on shift 1 and even sections on shift 2, so three sections put two on
# the first shift, six put three on each and eight four. The arms are chosen on
# that ratio: the shipped establishment (two on shift against two theatres), six
# sections against two theatres (limited by theatres), six against three (the
# third theatre isolated), eight against four (balanced growth) and three
# sections against three theatres, a null control at which the theatres cannot
# limit.
#
# A theatre is a room: its staff come from the surgical section, which
# r2e_surgery_block() seizes before it takes a theatre from the unit's theatre
# pool, whose capacity build_env() sets from the theatre count. Setting the
# count on the resolved configuration therefore reaches the model with no
# trajectory change, as R/icu_bed_sweep.R sets beds.
#
# Each replication is reduced inside the forked worker that produced it to the
# surgical team sweep's scalar responses (reduce_surgical_replication()), the
# R2E theatre, intensive care and holding occupancy and queue over the closing
# window, and a daily series of the theatres, intensive care, holding and the
# two R2E overflow points, so each pool can be classified converged or drifting
# by surgical_stability(). Faster surgery moves load onto intensive care and
# holding, which is why those pools are measured here too.
#
# Arms are paired on one control seed each. The shipped arm and the six-section
# arm at two theatres are configurations the surgical team sweep also ran under
# the same seed, so their replications reproduce that sweep's rows.
#
# Depends on R/analysis.R, R/policy_sweep.R, R/hold_window.R, R/long_horizon.R
# and R/surgical_team_sweep.R; source those first.

#' Replications per arm
OT_SWEEP_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
OT_SWEEP_DAYS <- 360L

#' Control seed every arm is set to before it runs
OT_SWEEP_SEED <- 42L

#' Closing window the queue and occupancy responses are measured over, in days
OT_SWEEP_WINDOW_DAYS <- 90L

#' Arm label of a theatre and section establishment, as the output files name it
#'
#' @param theatres R2E operating theatres.
#' @param sections R2E surgical sections.
#' @return The label, such as "3 theatres, 6 sections".
ot_arm_label <- function(theatres, sections) {
  sprintf("%d theatres, %d sections", theatres, sections)
}

#' The arms swept, as an R2E theatre and surgical section count
#'
#' @details Ordered as the results table reads them: the shipped
#'   establishment, the null control, then the three arms at six or more
#'   sections. Sections on shift are the odd-numbered sections on shift 1, so
#'   the column `on_shift` is the larger shift's count, the one that meets the
#'   theatres.
OT_SWEEP_ARMS <- local({
  theatres <- c(2L, 3L, 2L, 3L, 4L)
  sections <- c(3L, 3L, 6L, 6L, 8L)
  data.frame(arm = ot_arm_label(theatres, sections), theatres = theatres,
             sections = sections, on_shift = as.integer(ceiling(sections / 2)),
             stringsAsFactors = FALSE)
})

#' Arm every arm but one is compared against: the shipped establishment
OT_SWEEP_BASELINE_ARM <- ot_arm_label(2L, 3L)

#' The paired comparisons reported, as a from and to arm
#'
#' @details Every arm against the shipped establishment, and the third theatre
#'   at six sections against two theatres at six sections, the comparison that
#'   isolates a theatre where the theatres limit. Holm's adjustment is taken
#'   over every row of the file.
OT_SWEEP_COMPARISONS <- rbind(
  data.frame(from = OT_SWEEP_BASELINE_ARM,
             to = setdiff(OT_SWEEP_ARMS$arm, OT_SWEEP_BASELINE_ARM),
             stringsAsFactors = FALSE),
  data.frame(from = ot_arm_label(2L, 6L), to = ot_arm_label(3L, 6L),
             stringsAsFactors = FALSE)
)

#' Pools whose daily queue, or overflow census, is classified
#'
#' @details The theatres, whose backlog is the two-stage queue on the section
#'   and theatre pools (POOL_QUEUE_PATTERNS, R/queue_series.R), the two R2E bed
#'   pools faster surgery loads, and the two overflow points their saturation
#'   spills into, classified on their census.
OT_SWEEP_POOLS <- c("R2E operating theatres", "R2E intensive care", "R2E holding beds",
                    "R2E post-operative overflow", "R2E staging overflow")

#' Responses the paired comparisons report
OT_SWEEP_PAIRED_RESPONSES <- c(
  "theatre_mean_queue", "theatre_occupancy", "r2e_first_surgeries", "r2e_theatre_deferred",
  "released_unrepaired", "diverted_busy", "icu_occupancy", "overflow_r2e_post_op",
  "hold_mean_queue", "post_definitive_icu_share", "total_dow", "total_rtd"
)

#' Half-width each paired response is sized against, in its own units
#'
#' @details The smallest difference that would change a planning decision, as
#'   the other experiments choose it: one casualty in a queue or overflow
#'   census, a percentage point on a share or occupancy, ten casualties in a
#'   count of operations or diversions, one death and ten returns to duty.
OT_SWEEP_HALF_WIDTHS <- c(
  theatre_mean_queue = 1, theatre_occupancy = 0.01, r2e_first_surgeries = 10,
  r2e_theatre_deferred = 10, released_unrepaired = 10, diverted_busy = 10,
  icu_occupancy = 0.01, overflow_r2e_post_op = 1, hold_mean_queue = 1,
  post_definitive_icu_share = 0.01, total_dow = 1, total_rtd = 10
)

#' Set the R2E operating theatre count on a resolved configuration
#'
#' @param resolved Configuration as `resolve_scenario()` returns it.
#' @param theatres Number of R2E operating theatres to establish.
#' @return The configuration with that count set.
#'
#' @details Fails rather than returning the configuration unchanged where the
#'   R2E element or its theatre pool cannot be found, so a sweep cannot
#'   silently measure the shipped count at every arm. The element is located by
#'   name rather than by position.
set_ot_establishment <- function(resolved, theatres) {
  if (!is.numeric(theatres) || length(theatres) != 1 || is.na(theatres) ||
        theatres < 1 || theatres != round(theatres)) {
    stop("theatres must be a single whole count of at least 1, found '",
         paste(format(theatres), collapse = ", "), "'", call. = FALSE)
  }
  found <- FALSE
  for (i in seq_along(resolved$elms)) {
    if (!identical(resolved$elms[[i]]$elm, "r2eheavy")) next
    for (j in seq_along(resolved$elms[[i]]$beds)) {
      if (!identical(resolved$elms[[i]]$beds[[j]]$name, "ot")) next
      resolved$elms[[i]]$beds[[j]]$qty <- as.integer(theatres)
      found <- TRUE
    }
  }
  if (!found) {
    stop("no R2E operating theatre pool found in elms, so the theatre count ",
         "cannot be swept", call. = FALSE)
  }
  resolved
}

#' Enter a scenario's configuration with the R2E theatres and sections overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param theatres R2E operating theatres to establish.
#' @param sections R2E surgical sections to establish.
#' @return Invisibly, the described configuration that was applied.
#'
#' @details Both counts are structure in `elms`, from which
#'   `build_environment()` constructs the resources, so they are set on the
#'   resolved form before that call. R2B keeps its shipped section count.
apply_ot_setting <- function(json_data, scenario, theatres, sections) {
  resolved <- resolve_scenario(json_data, scenario)
  resolved <- set_ot_establishment(resolved, theatres)
  resolved <- set_surgical_sections(resolved, "r2eheavy", sections)
  described <- build_environment(resolved)
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Reduce one replication to the theatre establishment's responses
#'
#' @param env Wrapped simmer environment for one replication.
#' @param n_days Campaign length in days.
#' @param window_days Closing window the queue and occupancy are measured over.
#' @return A list of `row`, a one-row data frame of the scalar responses, and
#'   `series`, the daily queue (or overflow census) and occupancy of every pool
#'   in OT_SWEEP_POOLS in the long form block_means() takes.
#'
#' @details The responses are reduce_surgical_replication()'s, so the theatre
#'   queue is the two-stage backlog and the counts are the surgical team sweep's,
#'   followed by the theatre, intensive care and holding occupancy and the
#'   intensive care and holding queue over the closing window, each read by
#'   policy_pool_state() as the policy sweep reads them. Theatre occupancy is
#'   the theatres' share in use around the clock, the rooms being open 24 hours.
reduce_ot_bed_replication <- function(env, n_days, window_days = OT_SWEEP_WINDOW_DAYS) {
  base <- reduce_surgical_replication(env, n_days, window_days)
  resources <- simmer::get_mon_resources(env)

  #' One pool's occupancy and queue over the closing window
  #'
  #' @param pool Pool label, a name of LONG_HORIZON_POOLS.
  #' @return policy_pool_state()'s named occupancy and mean queue.
  state <- function(pool) {
    pattern <- LONG_HORIZON_POOLS[[pool]]
    policy_pool_state(resources, pattern, n_days, window_days,
                      pool_establishment(env_data$elms, pattern))
  }
  theatre <- state("R2E operating theatres")
  icu     <- state("R2E intensive care")
  hold    <- state("R2E holding beds")

  row <- cbind(base$row, data.frame(
    theatre_occupancy = theatre[["occupancy"]],
    icu_occupancy     = icu[["occupancy"]],
    icu_mean_queue    = icu[["mean_queue"]],
    hold_occupancy    = hold[["occupancy"]],
    hold_mean_queue   = hold[["mean_queue"]]
  ))

  pools <- reduce_pool_series(resources, n_days, env_data$elms)
  pools <- pools[pools$pool %in% OT_SWEEP_POOLS, ]
  series <- do.call(rbind, lapply(OT_SWEEP_POOLS, function(p) {
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

#' Measure the theatre establishment's responses across replications at one arm
#'
#' @param n_iterations Replications to run (default OT_SWEEP_REPLICATIONS).
#' @param n_days Campaign length in days (default OT_SWEEP_DAYS).
#' @param window_days Closing window for the queue and occupancy responses.
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @param checkpoint_dir Directory each finished replication is saved to and
#'   read back from, or NULL for none. A replication is named by its index, seed
#'   and horizon, so a changed control seed never reads another seed's file.
#' @return A list of `rows`, one row per replication, and `series`, the daily
#'   series of every replication bound together with a `replication` column.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed under the caller's generator kind, and the caller's
#'   stream is restored on exit. The caller applies the arm's configuration
#'   first (apply_ot_setting()); this function measures whatever is in force.
run_ot_bed_measurement <- function(n_iterations = OT_SWEEP_REPLICATIONS,
                                   n_days = OT_SWEEP_DAYS,
                                   window_days = OT_SWEEP_WINDOW_DAYS,
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
    out <- reduce_ot_bed_replication(env, n_days, window_days)
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
#'   read from the same late-half slope.
ot_bed_stability <- function(series, block_days = LONG_HORIZON_BLOCK_DAYS) {
  pools <- OT_SWEEP_POOLS[OT_SWEEP_POOLS %in% series$subject]
  do.call(rbind, lapply(pools, function(p) {
    st <- surgical_stability(series, block_days, pool = p)
    st$pool <- p
    st
  }))
}
