##############################################################################
## R/post_definitive_sweep.R                                                ##
## The post-definitive intensive care share, across replications            ##
##############################################################################
#
# Every operated casualty at R2E is owed an intensive care episode after the
# definitive repair, because the shipped share is 1
# (`vars.r2eheavy.post_definitive_icu.share`). That value is an upper bound no
# open-access source confirms (README Further Development L46), and the
# intensive care findings rest on it: the bed sweep, the post-operative gate and
# the evacuation policy grid all read intensive care access. A share below 1
# sends part of the operated cohort to ward recovery in a holding bed or
# overflow instead (r2e_post_definitive_ward()), which frees intensive care beds
# and loads the holding pool. This module measures that trade: the shipped 1
# against 0.75, 0.5 and 0.25.
#
# Each replication is reduced to one row of scalar responses and a daily series
# of the R2E intensive care and holding queues and the overflow points behind
# them inside the forked worker that produced it, on the arrangement
# R/icu_bed_sweep.R uses, and each pool is classified converged or drifting by
# surgical_stability(). The scalar responses are reduce_icu_bed_replication()'s,
# so the shipped arm is the intensive care bed sweep's four-bed arm under the
# same seed and reproduces its replications, with the ward recoveries and the
# holding occupancy split (holding_occupancy_split()) added.
#
# Arms are paired on one control seed each, as the establishment sweeps are,
# though not on one casualty stream: below the shipped share each casualty owed
# the episode takes a draw deciding whether it needs intensive care, which
# shifts the draws the replication takes after the first, so the pairing removes
# only the variance the shared parent seed carries.
#
# Depends on R/analysis.R, R/policy_sweep.R, R/hold_window.R, R/long_horizon.R,
# R/airlift.R, R/surgical_team_sweep.R and R/icu_bed_sweep.R; source those
# first.

#' Replications per arm
POST_DEFINITIVE_SWEEP_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
POST_DEFINITIVE_SWEEP_DAYS <- 360L

#' Control seed every arm is set to before it runs
POST_DEFINITIVE_SWEEP_SEED <- 42L

#' Closing window the queue and occupancy responses are measured over, in days
POST_DEFINITIVE_SWEEP_WINDOW_DAYS <- 90L

#' Post-definitive intensive care shares swept
#'
#' @details 1 is the shipped share and the upper bound; the arms below it
#'   bracket the one open-access comparator the README cites, the 54% of
#'   children with war injuries admitted to intensive care at a Role 3 facility,
#'   rather than estimating it.
POST_DEFINITIVE_SWEEP_SHARES <- c(0.25, 0.5, 0.75, 1)

#' Share the paired differences are measured against
POST_DEFINITIVE_SWEEP_BASELINE <- 1

#' Arm label of a share, as the summary and paired files name it
#'
#' @param share Post-definitive intensive care share.
#' @return The label, such as "ICU share 50%".
post_definitive_arm_label <- function(share) {
  sprintf("ICU share %d%%", as.integer(round(100 * share)))
}

#' The arms swept, one per share
POST_DEFINITIVE_SWEEP_ARMS <- data.frame(
  arm = post_definitive_arm_label(POST_DEFINITIVE_SWEEP_SHARES),
  share = POST_DEFINITIVE_SWEEP_SHARES,
  stringsAsFactors = FALSE
)

#' Arm the paired differences are measured against
POST_DEFINITIVE_SWEEP_BASELINE_ARM <- post_definitive_arm_label(POST_DEFINITIVE_SWEEP_BASELINE)

#' Responses the paired comparison against the shipped share reports
POST_DEFINITIVE_SWEEP_PAIRED_RESPONSES <- c(
  "icu_occupancy", "icu_mean_queue", "hold_occupancy", "hold_mean_queue",
  "overflow_r2e_post_op", "overflow_r2e_staging", "post_definitive_icu_share",
  "postop_degraded_share", "r2e_theatre_deferred", "ward_recoveries",
  "ward_bed_days", "post_definitive_bed_days", "total_dow", "total_rtd",
  "mean_evac_wait_critical", "role4_peak"
)

#' Half-width each paired response is sized against, in its own units
#'
#' @details The smallest difference that would change a planning decision, as
#'   the intensive care bed sweep chooses it: a percentage point on an occupancy
#'   or share, one casualty in a queue or overflow census, ten casualties in a
#'   count, a hundred bed-days over the campaign, one death, ten returns to
#'   duty, one day of wait and five patients of Role 4 census.
POST_DEFINITIVE_SWEEP_HALF_WIDTHS <- c(
  icu_occupancy = 0.01, icu_mean_queue = 1, hold_occupancy = 0.01, hold_mean_queue = 1,
  overflow_r2e_post_op = 1, overflow_r2e_staging = 1, post_definitive_icu_share = 0.01,
  postop_degraded_share = 0.01, r2e_theatre_deferred = 10, ward_recoveries = 10,
  ward_bed_days = 100, post_definitive_bed_days = 100, total_dow = 1, total_rtd = 10,
  mean_evac_wait_critical = 1, role4_peak = 5
)

#' Set the post-definitive intensive care share on a described configuration
#'
#' @param described Configuration as `build_environment()` returns it.
#' @param share Share of the operated cohort owed intensive care after the
#'   definitive repair, from 0 to 1.
#' @return The configuration with that share set.
#'
#' @details Fails rather than returning the configuration unchanged where the
#'   share cannot be found at `vars.r2eheavy.post_definitive_icu.share`, so a
#'   sweep cannot silently measure the shipped share at every arm, and fails on
#'   a value that is not a single number from 0 to 1, the rule
#'   post_definitive_share_faults() applies to a configuration file. The share
#'   is a variable rather than a count in `elms`, so it is set on the described
#'   form, as set_priority_split() sets the triage split.
set_post_definitive_share <- function(described, share) {
  if (!(is.numeric(share) && length(share) == 1L && is.finite(share) &&
          share >= 0 && share <= 1)) {
    stop("share must be a single number from 0 to 1, found '",
         paste(format(share), collapse = ", "), "'", call. = FALSE)
  }
  if (is.null(described$vars$r2eheavy$post_definitive_icu$share)) {
    stop("no share found at vars.r2eheavy.post_definitive_icu.share, so the ",
         "post-definitive intensive care share cannot be swept", call. = FALSE)
  }
  described$vars$r2eheavy$post_definitive_icu$share <- share
  described
}

#' Enter a scenario's configuration with the post-definitive share overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param share Post-definitive intensive care share to set.
#' @return Invisibly, the described configuration that was applied.
apply_post_definitive_setting <- function(json_data, scenario, share) {
  described <- build_environment(resolve_scenario(json_data, scenario))
  described <- set_post_definitive_share(described, share)
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Holding bed-days of the post-definitive ward recoveries over a campaign
#'
#' @param wide One replication's attributes joined to its arrivals.
#' @param horizon_min Campaign length in minutes.
#' @return Bed-days, zero where no casualty recovered on the ward in a bed.
#'
#' @details Counted as holding_occupancy_split() counts every stay: from the
#'   instant the bed was seized to whichever of the stay's end and the
#'   campaign's close comes first. A ward recovery in overflow holds no bed and
#'   records no start, so it contributes nothing.
ward_bed_days <- function(wide, horizon_min) {
  if (!all(c("post_definitive_ward_hold_start", "post_definitive_ward_min") %in%
             names(wide))) {
    return(0)
  }
  start <- wide$post_definitive_ward_hold_start
  minutes <- wide$post_definitive_ward_min
  took <- !is.na(start) & !is.na(minutes) & start < horizon_min
  if (!any(took)) return(0)
  sum(pmax(pmin(start[took] + minutes[took], horizon_min) - start[took], 0)) / DAY_MIN
}

#' Reduce one replication to the post-definitive share's responses
#'
#' @param env Wrapped simmer environment for one replication.
#' @param n_days Campaign length in days.
#' @param seed The replication's own seed, for the Role 4 census.
#' @param window_days Closing window the queue and occupancy are measured over.
#' @return A list of `row`, a one-row data frame of the scalar responses, and
#'   `series`, the daily queue and occupancy of every pool ICU_BED_SWEEP_POOLS
#'   names, in the long form block_means() takes.
#'
#' @details The responses and series are reduce_icu_bed_replication()'s, and:
#'   - `ward_recoveries`, operated casualties drawn as not needing intensive care
#'     after the definitive repair, and `ward_overflow`, those of them who
#'     recovered in overflow for want of a holding bed;
#'   - `post_definitive_icu_n`, casualties placed for the intensive care
#'     episode, in whichever bed was free;
#'   - `ward_bed_days`, the holding bed-days the ward recoveries took;
#'   - the holding occupancy split over the whole campaign
#'     (holding_occupancy_split()): total, evacuation, recovery,
#'     post-definitive (ward recoveries included) and post-operative bed-days.
reduce_post_definitive_replication <- function(env, n_days, seed,
                                               window_days = POST_DEFINITIVE_SWEEP_WINDOW_DAYS) {
  out <- reduce_icu_bed_replication(env, n_days, seed, window_days)

  resources  <- simmer::get_mon_resources(env)
  attributes <- simmer::get_mon_attributes(env)
  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  arrivals   <- arrivals[!duplicated(arrivals[, c("name", "replication")]), ]
  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"),
                      suffix = c("", "_arrival"))

  horizon_min <- n_days * DAY_MIN
  split <- holding_occupancy_split(resources, wide, horizon_min)
  ward <- wide$post_definitive_ward
  out$row <- cbind(
    out$row,
    data.frame(
      ward_recoveries = sum(!is.na(ward)),
      ward_overflow = sum(!is.na(ward) & ward == 3),
      post_definitive_icu_n = sum(!is.na(wide$post_definitive_pathway)),
      ward_bed_days = ward_bed_days(wide, horizon_min),
      hold_total_bed_days = split$total_bed_days,
      evacuation_bed_days = split$evacuation_bed_days,
      recovery_bed_days = split$recovery_bed_days,
      post_definitive_bed_days = split$post_definitive_bed_days,
      post_op_hold_bed_days = split$post_op_hold_bed_days
    )
  )
  out
}

#' Measure the post-definitive share's responses across replications at one arm
#'
#' @param n_iterations Replications to run (default POST_DEFINITIVE_SWEEP_REPLICATIONS).
#' @param n_days Campaign length in days (default POST_DEFINITIVE_SWEEP_DAYS).
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
#'   (apply_post_definitive_setting()); this function measures whatever is in
#'   force.
run_post_definitive_measurement <- function(n_iterations = POST_DEFINITIVE_SWEEP_REPLICATIONS,
                                            n_days = POST_DEFINITIVE_SWEEP_DAYS,
                                            window_days = POST_DEFINITIVE_SWEEP_WINDOW_DAYS,
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
      out <- reduce_post_definitive_replication(env, n_days, rep_seeds[i], window_days)
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
post_definitive_stability <- function(series, block_days = LONG_HORIZON_BLOCK_DAYS) {
  icu_bed_stability(series, block_days)
}
