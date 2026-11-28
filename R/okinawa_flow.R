##############################################################################
## R/okinawa_flow.R                                                         ##
## High-intensity died of wounds under Okinawa's flow and capacity          ##
##############################################################################
#
# The high-intensity profile is checked against Okinawa's 3.4% hospital
# died-of-wounds rate only as an upper bound at that campaign's 82 days. Okinawa
# evacuated casualties continuously and fielded about four times the model's
# beds per daily battle casualty, while the shipped profile flies one sortie a
# week and fields the shipped establishment, so the comparison mixes casualty
# load, evacuation tempo and capacity. This module measures the profile's
# treated-cohort rate as each is brought towards Okinawa's in turn: daily
# sorties; intensive care and holding beds at Okinawa's ratio; surgery doubled;
# single-stage surgery, the practice of 1945; surgery at the estimated Okinawa
# ratio; and every establishment scaled until nothing queues, the
# congestion-free floor.
#
# Two protocols are run on every arm. The sustained protocol is the
# establishment sweeps': 30 replications over 360 days under one control seed,
# each replication reduced to reduce_triage_replication()'s responses and daily
# pool series inside the forked worker that produced it, with paired differences
# against the shipped arm. The anchor protocol is the calibration check's: the
# treated-cohort rate pooled over three control seeds of 50 replications at
# Okinawa's 82 days, with the replication seeds drawn exactly as
# run_replications() draws them, so the shipped arm reproduces
# data/calibration/dow_calibration_anchor.csv. The anchor reduction keeps only
# the cohort counts, so the arms whose monitoring data run to gigabytes stay
# within memory.
#
# The arms are not one casualty stream: a changed establishment, tempo or
# pathway changes the draws each replication takes. The sustained pairing
# removes only the variance the shared parent seed carries.
#
# Depends on R/analysis.R, R/policy_sweep.R, R/hold_window.R, R/long_horizon.R,
# R/airlift.R, R/surgical_team_sweep.R, R/icu_bed_sweep.R, R/triage_sweep.R and
# R/dow_calibration.R; source those first.

#' Scenario profile every arm runs under
OKINAWA_FLOW_SCENARIO <- "high_intensity"

#' Replications per arm under the sustained protocol
OKINAWA_FLOW_REPLICATIONS <- 30L

#' Campaign length in days under the sustained protocol
OKINAWA_FLOW_DAYS <- 360L

#' Control seed every arm is set to under the sustained protocol
OKINAWA_FLOW_SEED <- 42L

#' Closing window the sustained queue and occupancy responses are measured over
OKINAWA_FLOW_WINDOW_DAYS <- 90L

#' Campaign length in days under the anchor protocol, Okinawa's 82 days
OKINAWA_FLOW_ANCHOR_DAYS <- 82L

#' Replications per control seed under the anchor protocol
OKINAWA_FLOW_ANCHOR_REPLICATIONS <- 50L

#' Control seeds of the anchor protocol, the calibration check's first three
OKINAWA_FLOW_ANCHOR_SEEDS <- DOW_CONTROL_SEEDS[1:3]

#' Bed pools scaled by an arm's bed factor
#'
#' @details The intensive care and holding beds of R2B and R2E, the pools a
#'   casualty occupies after surgery or while awaiting evacuation. Operating
#'   theatres are scaled with surgery instead, and resuscitation bays are left
#'   at their shipped counts.
OKINAWA_FLOW_BED_POOLS <- c("icu", "hold")

#' The arms measured
#'
#' @details One row per arm:
#'   - `interval_days`, the strategic airlift sortie interval;
#'   - `bed_scale`, the factor on OKINAWA_FLOW_BED_POOLS at R2B and R2E, 4 being
#'     Okinawa's 3.3 to 4.3 times the model's beds per daily battle casualty;
#'   - `surgery_scale`, the factor on R2B and R2E theatres and surgical sections;
#'   - `single_stage`, whether every priority's damage control rate is set to
#'     zero, damage control surgery postdating 1945;
#'   - `r2e_sections`, the R2E surgical section count where set, 2 giving 0.8
#'     times the shipped sections, the upper end of the informed estimate of
#'     Okinawa's surgical teams per daily casualty;
#'   - `all_scale`, the factor on every section, bed pool and vehicle, the
#'     congestion-free floor at 8.
OKINAWA_FLOW_ARMS <- data.frame(
  arm = c("Shipped", "Daily sorties", "Daily, beds x4", "Daily, beds x4, surgery x2",
          "Daily, beds x4, single-stage", "Daily, beds x4, single-stage, R2E sections 2",
          "Daily, every establishment x8"),
  stem = c("shipped", "daily", "daily_beds4", "daily_beds4_surg2", "daily_beds4_single",
           "daily_beds4_single_r2e2", "daily_all8"),
  interval_days = c(7, 1, 1, 1, 1, 1, 1),
  bed_scale = c(1L, 1L, 4L, 4L, 4L, 4L, 1L),
  surgery_scale = c(1L, 1L, 1L, 2L, 1L, 1L, 1L),
  single_stage = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
  r2e_sections = c(NA, NA, NA, NA, NA, 2L, NA),
  all_scale = c(1L, 1L, 1L, 1L, 1L, 1L, 8L),
  stringsAsFactors = FALSE
)

#' Arm the paired differences are measured against
OKINAWA_FLOW_BASELINE_ARM <- "Shipped"

#' Responses the paired comparison against the shipped arm reports
OKINAWA_FLOW_PAIRED_RESPONSES <- c(
  "treated_dow_rate", "total_dow", "dow_awaiting_airlift", "theatre_mean_queue",
  "overflow_r2e_post_op", "hold_mean_queue", "first_incision_hours", "critical_backlog",
  "total_rtd"
)

#' Half-width each paired response is sized against, in its own units
#'
#' @details As the triage sweep chooses them: a tenth of a percentage point on a
#'   mortality rate, one death, one casualty in a queue or census, an hour to
#'   first incision, ten casualties in a backlog and ten returns to duty.
OKINAWA_FLOW_HALF_WIDTHS <- c(
  treated_dow_rate = 0.001, total_dow = 1, dow_awaiting_airlift = 1,
  theatre_mean_queue = 1, overflow_r2e_post_op = 1, hold_mean_queue = 1,
  first_incision_hours = 1, critical_backlog = 10, total_rtd = 10
)

#' Multiply the count of named sub-elements or bed pools on a resolved configuration
#'
#' @param resolved Configuration as `resolve_scenario()` returns it.
#' @param field `"sub_elms"` or `"beds"`.
#' @param names Sub-element or bed pool names to scale, or NULL for every one.
#' @param factor Whole-number factor of at least 1.
#' @param echelons Element names to scale within, or NULL for every element.
#' @return The configuration with those counts multiplied.
#'
#' @details Fails rather than returning the configuration unchanged where no
#'   matching entry is found, so an arm cannot silently measure the shipped
#'   establishment. Elements and pools are located by name rather than position.
scale_establishment <- function(resolved, field, names, factor, echelons = NULL) {
  if (!is.numeric(factor) || length(factor) != 1L || is.na(factor) || factor < 1 ||
        factor != round(factor)) {
    stop("factor must be a single whole number of at least 1, found '",
         paste(format(factor), collapse = ", "), "'", call. = FALSE)
  }
  key <- if (field == "sub_elms") "sub_elm" else "name"
  found <- 0L
  for (i in seq_along(resolved$elms)) {
    if (!is.null(echelons) && !resolved$elms[[i]]$elm %in% echelons) next
    for (j in seq_along(resolved$elms[[i]][[field]])) {
      entry <- resolved$elms[[i]][[field]][[j]]
      if (!is.null(names) && !as.character(entry[[key]]) %in% names) next
      resolved$elms[[i]][[field]][[j]]$qty <- as.integer(factor * entry$qty)
      found <- found + 1L
    }
  }
  if (found == 0L) {
    stop("no ", field, " entry named ", paste(names, collapse = "/"), " found in elms",
         call. = FALSE)
  }
  resolved
}

#' Set an arm's establishment on a resolved configuration
#'
#' @param resolved Configuration as `resolve_scenario()` returns it.
#' @param arm One row of OKINAWA_FLOW_ARMS.
#' @return The configuration with the arm's beds, theatres, sections and
#'   vehicles set.
#'
#' @details The counts are structure in `elms` and `transports`, from which
#'   `build_environment()` constructs the resources, so they are set on the
#'   resolved form before that call, as the establishment sweeps set theirs.
set_okinawa_establishment <- function(resolved, arm) {
  r2 <- c("r2b", "r2eheavy")
  if (arm$all_scale > 1L) {
    resolved <- scale_establishment(resolved, "sub_elms", NULL, arm$all_scale)
    resolved <- scale_establishment(resolved, "beds", NULL, arm$all_scale)
    for (i in seq_along(resolved$transports)) {
      resolved$transports[[i]]$qty <- as.integer(arm$all_scale * resolved$transports[[i]]$qty)
    }
  }
  if (arm$bed_scale > 1L) {
    resolved <- scale_establishment(resolved, "beds", OKINAWA_FLOW_BED_POOLS, arm$bed_scale, r2)
  }
  if (arm$surgery_scale > 1L) {
    resolved <- scale_establishment(resolved, "beds", "ot", arm$surgery_scale, r2)
    resolved <- scale_establishment(resolved, "sub_elms", "surg", arm$surgery_scale, r2)
  }
  if (!is.na(arm$r2e_sections)) {
    resolved <- set_surgical_sections(resolved, "r2eheavy", arm$r2e_sections)
  }
  resolved
}

#' Set an arm's sortie interval and surgical pathway on a described configuration
#'
#' @param described Configuration as `build_environment()` returns it.
#' @param arm One row of OKINAWA_FLOW_ARMS.
#' @return The configuration with the interval set and, for a single-stage arm,
#'   every priority's damage control rate set to zero.
#'
#' @details Fails where either variable cannot be found, so an arm cannot
#'   silently keep the shipped value. A damage control rate of zero consumes no
#'   random draw, so a single-stage arm differs from its staged counterpart in
#'   the operations it performs and not in the draws that decide them.
set_okinawa_variables <- function(described, arm) {
  if (is.null(described$vars$role4$ame$schedule_interval_days)) {
    stop("no sortie interval found at vars.role4.ame.schedule_interval_days",
         call. = FALSE)
  }
  described$vars$role4$ame$schedule_interval_days <- arm$interval_days
  if (isTRUE(arm$single_stage)) {
    for (k in sprintf("pri%d_dcs_rate", 1:3)) {
      if (is.null(described$vars$r1$other[[k]])) {
        stop("no damage control rate found at vars.r1.other.", k, call. = FALSE)
      }
      described$vars$r1$other[[k]] <- 0
    }
  }
  described
}

#' Enter the high-intensity configuration with an arm's settings applied
#'
#' @param json_data Parsed env_data.json.
#' @param arm One row of OKINAWA_FLOW_ARMS.
#' @return Invisibly, the described configuration that was applied.
apply_okinawa_setting <- function(json_data, arm) {
  resolved <- set_okinawa_establishment(resolve_scenario(json_data, OKINAWA_FLOW_SCENARIO), arm)
  described <- set_okinawa_variables(build_environment(resolved), arm)
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Treated casualties who died awaiting strategic airlift in one replication
#'
#' @param attributes Attribute-monitor rows of one replication.
#' @return The count of treated casualties whose last recorded death echelon is
#'   the airlift wait (`dow_echelon` 5) and who died of wounds.
dow_awaiting_airlift <- function(attributes) {
  treated <- unique(attributes$name[attributes$key %in% c("r2b_treated", "r2e_treated")])
  died <- unique(attributes$name[attributes$key == "dow" & attributes$value == 1])
  ech <- attributes[attributes$key == "dow_echelon", ]
  ech <- ech[order(ech$time), ]
  last_ech <- tapply(ech$value, ech$name, function(v) v[length(v)])
  at_airlift <- names(last_ech)[last_ech == 5]
  sum(died %in% treated & died %in% at_airlift)
}

#' Reduce one replication to the sustained protocol's responses
#'
#' @param env Wrapped simmer environment for one replication.
#' @param n_days Campaign length in days.
#' @param seed The replication's own seed, for the Role 4 census.
#' @param window_days Closing window the queue and occupancy are measured over.
#' @return reduce_triage_replication()'s `row`, with `dow_awaiting_airlift`
#'   appended, and its `series`.
reduce_okinawa_replication <- function(env, n_days, seed,
                                       window_days = OKINAWA_FLOW_WINDOW_DAYS) {
  out <- reduce_triage_replication(env, n_days, seed, window_days)
  out$row$dow_awaiting_airlift <- dow_awaiting_airlift(simmer::get_mon_attributes(env))
  out
}

#' Measure the sustained protocol's responses across replications at one arm
#'
#' @param n_iterations Replications to run (default OKINAWA_FLOW_REPLICATIONS).
#' @param n_days Campaign length in days (default OKINAWA_FLOW_DAYS).
#' @param window_days Closing window for the queue and occupancy responses.
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @param checkpoint_dir Directory each finished replication is saved to and
#'   read back from, or NULL for none (checkpoint_replication()).
#' @return A list of `rows`, one row per replication, and `series`, the daily
#'   series of every replication bound together with a `replication` column.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed under the caller's generator kind, and the caller's
#'   stream is restored on exit. The caller applies the arm first
#'   (apply_okinawa_setting()); this function measures whatever is in force.
run_okinawa_measurement <- function(n_iterations = OKINAWA_FLOW_REPLICATIONS,
                                    n_days = OKINAWA_FLOW_DAYS,
                                    window_days = OKINAWA_FLOW_WINDOW_DAYS,
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
      out <- reduce_okinawa_replication(env, n_days, rep_seeds[i], window_days)
      out$row$replication <- i
      out$series$replication <- i
      out
    }
    checkpoint_replication(compute, checkpoint_dir, i, rep_seeds[i], n_days)
  }

  dispatched <- dispatch_replications(worker, n_iterations, max_cores)
  usable <- vapply(dispatched, function(x) is.list(x) && !is.null(x$row), logical(1))
  if (!all(usable)) {
    stop(sprintf("%d of %d replications did not complete", sum(!usable), n_iterations),
         call. = FALSE)
  }
  list(rows = do.call(rbind, lapply(dispatched, `[[`, "row")),
       series = do.call(rbind, lapply(dispatched, `[[`, "series")))
}

#' Measure the anchor protocol's treated-cohort rate at one arm and control seed
#'
#' @param seed Control seed.
#' @param n_reps Replications (default OKINAWA_FLOW_ANCHOR_REPLICATIONS).
#' @param n_days Campaign length in days (default OKINAWA_FLOW_ANCHOR_DAYS).
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @param checkpoint_dir Directory each finished replication is saved to and
#'   read back from, or NULL for none.
#' @return One row per replication: `seed`, `replication`, `treated_n`,
#'   `treated_dow`, `dow_awaiting_airlift` and `treated_dow_rate`.
#'
#' @details The replication seeds are drawn from `seed` exactly as
#'   `run_replications()` draws them, and each replication's cohort is the one
#'   treated_cohort_rates() defines (triage_treated_cohort()), so the shipped
#'   arm reproduces the calibration check's measurement. Each replication is
#'   reduced inside its worker to its cohort counts, so no monitoring data
#'   return to the parent. The caller's stream is restored on exit.
run_okinawa_anchor <- function(seed, n_reps = OKINAWA_FLOW_ANCHOR_REPLICATIONS,
                               n_days = OKINAWA_FLOW_ANCHOR_DAYS, max_cores = NULL,
                               checkpoint_dir = NULL) {
  rng_state <- capture_rng_state()
  on.exit(restore_rng_state(rng_state), add = TRUE)

  set.seed(seed)
  rep_seeds <- sample.int(.Machine$integer.max, n_reps)
  RNGkind("L'Ecuyer-CMRG")

  #' Run one replication and return its cohort counts
  #'
  #' @param i Index of the replication, into `rep_seeds`.
  #' @return A one-row data frame of the replication's cohort counts.
  worker <- function(i) {
    #' Run and reduce the replication
    #'
    #' @return A one-row data frame of the replication's cohort counts.
    compute <- function() {
      env <- run_once(n_days, seed = rep_seeds[i], write_files = FALSE)
      attributes <- simmer::get_mon_attributes(env)
      cohort <- triage_treated_cohort(attributes)
      data.frame(seed = seed, replication = i, treated_n = cohort$treated_n,
                 treated_dow = cohort$treated_dow,
                 dow_awaiting_airlift = dow_awaiting_airlift(attributes),
                 treated_dow_rate = cohort$treated_dow_rate)
    }
    checkpoint_replication(compute, checkpoint_dir, i, rep_seeds[i], n_days)
  }

  dispatched <- dispatch_replications(worker, n_reps, max_cores)
  usable <- vapply(dispatched, is.data.frame, logical(1))
  if (!all(usable)) {
    stop(sprintf("%d of %d anchor replications did not complete", sum(!usable), n_reps),
         call. = FALSE)
  }
  do.call(rbind, dispatched)
}

#' Summarise the anchor protocol's rates of every arm
#'
#' @param rows Anchor rows of every arm, carrying `arm`, `treated_dow_rate`,
#'   `treated_dow`, `treated_n` and `dow_awaiting_airlift`.
#' @return One row per arm, in OKINAWA_FLOW_ARMS order: the pooled rate, its
#'   95% interval (pooled_rate_summary()), the replications pooled, and the mean
#'   treated deaths and deaths awaiting airlift per replication.
summarise_okinawa_anchor <- function(rows) {
  arms <- OKINAWA_FLOW_ARMS$arm[OKINAWA_FLOW_ARMS$arm %in% rows$arm]
  do.call(rbind, lapply(arms, function(a) {
    r <- rows[rows$arm == a, ]
    s <- pooled_rate_summary(r$treated_dow_rate[!is.na(r$treated_dow_rate)])
    data.frame(arm = a, n = s$n, rate = s$mean, ci_lower = s$ci_lower, ci_upper = s$ci_upper,
               treated_dow = mean(r$treated_dow),
               dow_awaiting_airlift = mean(r$dow_awaiting_airlift),
               stringsAsFactors = FALSE)
  }))
}

#' Classify each pool's queue at every arm and read its shortfall per day
#'
#' @param series Daily series of every arm in long form, carrying `arm`,
#'   `replication`, `day`, `series`, `subject` and `value`.
#' @param block_days Block length in days (default LONG_HORIZON_BLOCK_DAYS).
#' @return One row per arm and pool, surgical_stability()'s columns with the
#'   pool named.
okinawa_stability <- function(series, block_days = LONG_HORIZON_BLOCK_DAYS) {
  pools <- TRIAGE_SWEEP_POOLS[TRIAGE_SWEEP_POOLS %in% series$subject]
  do.call(rbind, lapply(pools, function(p) {
    st <- surgical_stability(series, block_days, pool = p)
    st$pool <- p
    st
  }))
}
