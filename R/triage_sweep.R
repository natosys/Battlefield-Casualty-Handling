##############################################################################
## R/triage_sweep.R                                                         ##
## The Priority 1 triage share, across replications                         ##
##############################################################################
#
# The background triage split (`vars.r1.priority`, Priority 1 65%, Priority 2
# 20%, Priority 3 15%) is an informed estimate, and it is the model's only
# proxy for severity: it drives surgical need, the damage control rate,
# intensive care gating, the critical airlift pool and Role 4 intensive care
# demand together. The comparators the README cites put urgent surgical need
# well below the shipped share. This module measures whether the published
# findings hold at a lower Priority 1 share: the shipped 0.65 against 0.45,
# 0.30 and 0.15, the Priority 2 to Priority 3 ratio held at its shipped 20:15
# so that the arms vary acuity without also reshaping the remainder.
#
# The split applies to wounded and non-battle injured casualties; disease and
# battle fatigue take their own priority rule (R/trajectories_casualty.R), and
# the casualty surge split is separate and ships disabled, so neither is
# swept. The died-of-wounds base rates and care multipliers, the surgical
# need rates and the damage control rates by priority are held fixed, so the
# sweep measures the triage mix alone. The base rates were fitted at the
# shipped mix, so each arm reports its treated-cohort died-of-wounds rate and
# the share of its expected deaths accrued after surgery as a diagnostic of how
# far the calibration moves; no arm is refitted.
#
# Each replication is reduced to one row of scalar responses and a daily series
# of the R2E theatre, intensive care and holding queues and the overflow points
# behind them inside the forked worker that produced it, on the arrangement
# R/icu_bed_sweep.R uses, and each pool is classified converged or drifting by
# surgical_stability(). Arms are paired on one control seed each, as the
# establishment sweeps are, though not on one casualty stream: a changed
# priority changes downstream branching and therefore the draws a replication
# takes, so the pairing removes only the variance the shared parent seed
# carries.
#
# Depends on R/analysis.R, R/policy_sweep.R, R/hold_window.R, R/long_horizon.R,
# R/airlift.R, R/surgical_team_sweep.R and R/icu_bed_sweep.R; source those
# first.

#' Replications per arm
TRIAGE_SWEEP_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
TRIAGE_SWEEP_DAYS <- 360L

#' Control seed every arm is set to before it runs
TRIAGE_SWEEP_SEED <- 42L

#' Closing window the queue and occupancy responses are measured over, in days
TRIAGE_SWEEP_WINDOW_DAYS <- 90L

#' Priority 1 shares swept
#'
#' @details 0.65 is the shipped share; 0.15 lies inside both comparator ranges
#'   the README cites for urgent surgical need and the critical share of
#'   survivors, and 0.30 and 0.45 space the arms evenly between.
TRIAGE_SWEEP_P1_SHARES <- c(0.15, 0.30, 0.45, 0.65)

#' Priority 1 share the paired differences are measured against
TRIAGE_SWEEP_BASELINE <- 0.65

#' Shipped Priority 2 and Priority 3 shares, whose ratio every arm keeps
TRIAGE_SWEEP_P2_P3 <- c(two = 0.20, three = 0.15)

#' Arm label of a Priority 1 share, as the summary and paired files name it
#'
#' @param p1 Priority 1 share.
#' @return The label, such as "P1 15%".
triage_arm_label <- function(p1) sprintf("P1 %d%%", as.integer(round(100 * p1)))

#' The triage split at one Priority 1 share
#'
#' @param p1 Priority 1 share, strictly between 0 and 1.
#' @return Named numeric vector `one`, `two`, `three` summing to one, the
#'   remainder divided between Priority 2 and Priority 3 in the shipped ratio.
#'
#' @details Rounded to twelve places, so the shipped share returns the shipped
#'   split exactly rather than a value one unit in the last place from it, and
#'   the shipped arm reproduces the shipped configuration draw for draw.
triage_split <- function(p1) {
  if (!is.numeric(p1) || length(p1) != 1 || is.na(p1) || p1 <= 0 || p1 >= 1) {
    stop("p1 must be a single share strictly between 0 and 1, found '",
         paste(format(p1), collapse = ", "), "'", call. = FALSE)
  }
  rest <- (1 - p1) / sum(TRIAGE_SWEEP_P2_P3)
  round(c(one = p1, two = rest * TRIAGE_SWEEP_P2_P3[["two"]],
          three = rest * TRIAGE_SWEEP_P2_P3[["three"]]), 12)
}

#' The arms swept, one per Priority 1 share
TRIAGE_SWEEP_ARMS <- data.frame(
  arm = triage_arm_label(TRIAGE_SWEEP_P1_SHARES),
  p1 = TRIAGE_SWEEP_P1_SHARES,
  p2 = vapply(TRIAGE_SWEEP_P1_SHARES, function(p) triage_split(p)[["two"]], numeric(1)),
  p3 = vapply(TRIAGE_SWEEP_P1_SHARES, function(p) triage_split(p)[["three"]], numeric(1)),
  stringsAsFactors = FALSE
)

#' Arm the paired differences are measured against
TRIAGE_SWEEP_BASELINE_ARM <- triage_arm_label(TRIAGE_SWEEP_BASELINE)

#' Pools whose daily queue, or overflow census, is classified
#'
#' @details The R2E theatres, the two R2E bed pools and the overflow points
#'   their saturation spills into, the capacity findings the sweep tests.
TRIAGE_SWEEP_POOLS <- c("R2E operating theatres", ICU_BED_SWEEP_POOLS)

#' Responses the paired comparison against the shipped split reports
TRIAGE_SWEEP_PAIRED_RESPONSES <- c(
  "theatre_mean_queue", "overflow_r2e_post_op", "overflow_r2e_staging", "hold_mean_queue",
  "icu_occupancy", "diverted_offshift", "diverted_busy", "first_incision_hours",
  "surgical_delivery_share", "total_dow", "dow_per_wounded", "treated_dow_rate",
  "total_rtd", "evac_decisions", "critical_backlog", "role4_peak_icu"
)

#' Half-width each paired response is sized against, in its own units
#'
#' @details The smallest difference that would change a planning decision, as
#'   the establishment sweeps choose it: one casualty in a queue, overflow
#'   census or backlog, a percentage point on an occupancy or share, ten
#'   casualties in a count, an hour to first incision, one death, a tenth of a
#'   percentage point on a mortality rate, ten returns to duty and five Role 4
#'   intensive care beds.
TRIAGE_SWEEP_HALF_WIDTHS <- c(
  theatre_mean_queue = 1, overflow_r2e_post_op = 1, overflow_r2e_staging = 1,
  hold_mean_queue = 1, icu_occupancy = 0.01, diverted_offshift = 10, diverted_busy = 10,
  first_incision_hours = 1, surgical_delivery_share = 0.01, total_dow = 1,
  dow_per_wounded = 0.001, treated_dow_rate = 0.001, total_rtd = 10, evac_decisions = 10,
  critical_backlog = 1, role4_peak_icu = 5
)

#' Set the triage split on a described configuration
#'
#' @param described Configuration as `build_environment()` returns it.
#' @param split Named numeric vector `one`, `two`, `three`, as triage_split()
#'   returns it.
#' @return The configuration with that split set.
#'
#' @details Fails rather than returning the configuration unchanged where the
#'   background split cannot be found, so a sweep cannot silently measure the
#'   shipped split at every arm, and fails on a split that is not a set of
#'   shares summing to one. The split is a variable rather than a count in
#'   `elms`, so it is set on the described form, as
#'   apply_hold_window_setting() sets the pre-open window.
set_priority_split <- function(described, split) {
  keys <- c("one", "two", "three")
  if (!is.numeric(split) || !all(keys %in% names(split)) ||
        any(is.na(split[keys])) || any(split[keys] < 0 | split[keys] > 1) ||
        abs(sum(split[keys]) - 1) > 1e-9) {
    stop("split must give shares one, two and three in [0, 1] summing to 1, found '",
         paste(names(split), format(split), sep = "=", collapse = ", "), "'", call. = FALSE)
  }
  current <- described$vars$r1$priority
  if (is.null(current) || !all(keys %in% names(current))) {
    stop("no background triage split found at vars.r1.priority, so the split ",
         "cannot be swept", call. = FALSE)
  }
  for (k in keys) described$vars$r1$priority[[k]] <- unname(split[[k]])
  described
}

#' Enter a scenario's configuration with the triage split overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param p1 Priority 1 share to set.
#' @return Invisibly, the described configuration that was applied.
apply_triage_setting <- function(json_data, scenario, p1) {
  described <- build_environment(resolve_scenario(json_data, scenario))
  described <- set_priority_split(described, triage_split(p1))
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Peak Role 4 intensive care census of one replication
#'
#' @param wide One replication's attributes joined to its arrivals.
#' @param seed The replication's own seed.
#' @return The largest daily intensive care ward census, zero where nobody
#'   reached Role 4.
#'
#' @details Drawn under the replication's own seed, as
#'   reduce_policy_replication() draws the total census, so the two rest on one
#'   length-of-stay draw and the peak is a function of the campaign rather than
#'   of the caller's stream position.
triage_role4_peak_icu <- function(wide, seed) {
  census <- with_preserved_rng({
    set.seed(seed)
    compute_role4_census(wide, env_data$vars$role4)
  })
  icu <- census[census$ward == "icu", ]
  if (nrow(icu) == 0) return(0)
  max(tapply(icu$occupancy, icu$day, sum))
}

#' The treated cohort's deaths of wounds and expected deaths
#'
#' @param attributes One replication's attribute monitor.
#' @return One-row data frame of `treated_n`, `treated_dow`, `treated_dow_rate`,
#'   `expected_early` and `expected_late`.
#'
#' @details The cohort is every casualty treated at R2B or R2E, as
#'   treated_cohort_rates() (R/dow_calibration.R) defines it; the counts are
#'   carried as well as the rate so the rate can be pooled across replications.
#'   The expected deaths are the risk the care-state model accrued before and
#'   after surgery, read from each casualty's last recorded value.
triage_treated_cohort <- function(attributes) {
  treated <- unique(attributes$name[attributes$key %in% c("r2b_treated", "r2e_treated")])
  died <- unique(attributes$name[attributes$key == "dow" & attributes$value == 1])
  #' Sum over the cohort of one expected-death attribute's last value
  #'
  #' @param k Attribute key.
  #' @return The sum, zero where none was recorded.
  expected <- function(k) {
    x <- attributes[attributes$key == k & attributes$name %in% treated, ]
    if (nrow(x) == 0) return(0)
    sum(tapply(x$value, x$name, function(v) v[length(v)]))
  }
  n <- length(treated)
  d <- sum(died %in% treated)
  data.frame(treated_n = n, treated_dow = d,
             treated_dow_rate = if (n == 0) NA_real_ else d / n,
             expected_early = expected("dow_expected_early"),
             expected_late = expected("dow_expected_late"))
}

#' Reduce one replication to the triage sweep's responses
#'
#' @param env Wrapped simmer environment for one replication.
#' @param n_days Campaign length in days.
#' @param seed The replication's own seed, for the Role 4 census.
#' @param window_days Closing window the queue and occupancy are measured over.
#' @return A list of `row`, a one-row data frame of the scalar responses, and
#'   `series`, the daily queue and occupancy of every pool TRIAGE_SWEEP_POOLS
#'   names, in the long form block_means() takes.
#'
#' @details The responses are reduce_icu_bed_replication()'s (the policy
#'   sweep's set and the overflow census) and the R2B counts
#'   reduce_hold_window_replication() reads, and:
#'   - `dow_per_wounded`, deaths of wounds over wounded-in-action arrivals;
#'   - `first_incision_hours`, the completed-case mean time from arrival to the
#'     first operation at R2B or R2E, KIA excluded, as the single-run pathway
#'     reports it;
#'   - `surgical_delivery_share`, of the casualties drawn as requiring surgery
#'     the share operated on, over one row per casualty;
#'   - `critical_backlog`, evacuees decided onto the critical route and not
#'     boarded when the run closed;
#'   - `role4_peak_icu`, the peak Role 4 intensive care census;
#'   - the treated cohort's counts and expected deaths (triage_treated_cohort()).
reduce_triage_replication <- function(env, n_days, seed,
                                      window_days = TRIAGE_SWEEP_WINDOW_DAYS) {
  base <- reduce_icu_bed_replication(env, n_days, seed, window_days)$row
  r2b <- reduce_hold_window_replication(env, window_min = NA_integer_)
  r2b <- r2b[, setdiff(names(r2b), c("window_min", names(base))), drop = FALSE]

  resources  <- simmer::get_mon_resources(env)
  attributes <- simmer::get_mon_attributes(env)
  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  arrivals   <- arrivals[!duplicated(arrivals[, c("name", "replication")]), ]
  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"),
                      suffix = c("", "_arrival"))

  wounded <- sum(startsWith(wide$name, "wia_"))
  first_op <- suppressWarnings(pmin(as.numeric(wide$r2b_surgery_start),
                                    as.numeric(wide$r2e_surgery_1_start), na.rm = TRUE))
  incision <- (first_op - wide$start_time)[!startsWith(wide$name, "kia_")]
  incision <- incision[is.finite(incision) & incision >= 0]
  needs <- !is.na(wide$surgery) & wide$surgery == 1
  operated <- !is.na(wide$r2b_surgery_start) | !is.na(wide$r2e_surgery_1_start)
  critical_backlog <- sum(!is.na(wide$r2e_evac) & wide$r2e_evac == 1 &
                            !is.na(wide$ame_route) & wide$ame_route == AIRLIFT_ROUTE_CRITICAL &
                            is.na(wide$ame_departure_time))

  row <- cbind(
    base, r2b,
    data.frame(
      wounded = wounded,
      dow_per_wounded = if (wounded == 0) NA_real_ else base$total_dow / wounded,
      first_incision_hours = if (length(incision) == 0) NA_real_ else mean(incision) / 60,
      surgical_delivery_share = if (!any(needs)) NA_real_ else sum(needs & operated) / sum(needs),
      critical_backlog = critical_backlog,
      role4_peak_icu = triage_role4_peak_icu(wide, seed)
    ),
    triage_treated_cohort(attributes)
  )

  pools <- reduce_pool_series(resources, n_days, env_data$elms)
  pools <- pools[pools$pool %in% TRIAGE_SWEEP_POOLS, ]
  series <- do.call(rbind, lapply(TRIAGE_SWEEP_POOLS, function(p) {
    rows <- pools[pools$pool == p, ]
    rbind(
      data.frame(day = rows$day, series = "mean_queue", subject = p, value = rows$mean_queue),
      data.frame(day = rows$day, series = "occupancy", subject = p, value = rows$occupancy)
    )
  }))
  # An overflow point has no establishment and so no occupancy row.
  list(row = row, series = series[!is.na(series$value), ])
}

#' Measure the triage sweep's responses across replications at one arm
#'
#' @param n_iterations Replications to run (default TRIAGE_SWEEP_REPLICATIONS).
#' @param n_days Campaign length in days (default TRIAGE_SWEEP_DAYS).
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
#'   (apply_triage_setting()); this function measures whatever is in force.
run_triage_measurement <- function(n_iterations = TRIAGE_SWEEP_REPLICATIONS,
                                   n_days = TRIAGE_SWEEP_DAYS,
                                   window_days = TRIAGE_SWEEP_WINDOW_DAYS,
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
    out <- reduce_triage_replication(env, n_days, rep_seeds[i], window_days)
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
triage_stability <- function(series, block_days = LONG_HORIZON_BLOCK_DAYS) {
  pools <- TRIAGE_SWEEP_POOLS[TRIAGE_SWEEP_POOLS %in% series$subject]
  do.call(rbind, lapply(pools, function(p) {
    st <- surgical_stability(series, block_days, pool = p)
    st$pool <- p
    st
  }))
}

#' Treated-cohort died-of-wounds rate of each arm across its replications
#'
#' @param rows Per-replication responses of every arm, carrying `arm`,
#'   `treated_n`, `treated_dow`, `treated_dow_rate`, `expected_early` and
#'   `expected_late`.
#' @return One row per arm: the cohort and death totals, the mean of the
#'   per-replication rates with its Student $t$ interval, and the share of
#'   expected deaths accrued after surgery.
#'
#' @details The interval is taken across replications, the project's unit of
#'   analysis, as the calibration measures the rate, rather than across
#'   casualties, which are not independent trials within one campaign; the lower
#'   bound is clamped at zero. The post-operative share is the ratio of the
#'   arm's summed expected deaths, a property of the arm rather than an
#'   estimate needing an interval.
triage_treated_pooled <- function(rows) {
  do.call(rbind, lapply(unique(rows$arm), function(a) {
    x <- rows[rows$arm == a, ]
    rates <- x$treated_dow_rate[!is.na(x$treated_dow_rate)]
    k <- length(rates)
    m <- if (k > 0) mean(rates) else NA_real_
    e <- if (k > 1) qt(0.975, df = k - 1) * sd(rates) / sqrt(k) else 0
    expected <- sum(x$expected_early) + sum(x$expected_late)
    data.frame(arm = a, treated_n = sum(x$treated_n), treated_dow = sum(x$treated_dow),
               rate = m, ci_lower = max(0, m - e), ci_upper = m + e,
               post_op_share = if (expected > 0) sum(x$expected_late) / expected else NA_real_,
               stringsAsFactors = FALSE)
  }))
}
