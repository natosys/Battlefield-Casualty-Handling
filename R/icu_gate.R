##############################################################################
## R/icu_gate.R                                                             ##
## The post-operative intensive care gate, at its two arms                  ##
##############################################################################
#
# r2b.icu_gating.enabled and r2eheavy.icu_gating.enabled ration a scarce
# post-operative resource rather than letting every casualty needing
# stabilisation queue indefinitely for one: with the gate in force a
# Priority 1 casualty is operated on regardless and recovers in a degraded
# holding bed when no intensive care bed is free, and a lower-priority
# casualty's theatre entry is deferred until one is. This module measures
# what that rationing buys and costs against the model as it stood before the
# gate existed, reconstructed as a configuration by setting both fields to
# zero rather than as a historical code state, which is what makes the
# comparison repeatable after a later model change
# (`scripts/check_icu_gate_switch.R` asserts the mechanism).
#
# Both arms run under one control seed, so replication $i$ of one is paired
# with replication $i$ of the other, on the arrangement R/hold_window.R uses
# for a paired two-arm comparison; the two modules stay separate rather than
# sharing a runner (docs/Multi_Run_Supplement.md, "The Post-Operative
# Intensive Care Gate" records the decision). Each replication is reduced to
# one row of the response set inside the forked worker that produced it.
#
# Depends on R/queue_series.R for the pool-queue step-function estimators and
# on R/analysis.R for build_attributes_wide(); source those before this file.

#' Replications per arm
ICU_GATE_REPLICATIONS <- 50L

#' Campaign length in days each replication runs for
ICU_GATE_DAYS <- 30L

#' Control seed the per-replication seeds are drawn from, one arm at a time
ICU_GATE_SEED <- 42L

#' Gate states compared: disabled (the pre-gate model) and enabled (shipped)
ICU_GATE_ARMS <- c(0L, 1L)

#' Response columns the published table reports, in its printed order
ICU_GATE_RESPONSES <- c(
  "icu_occupancy", "total_dow", "total_casualties",
  "icu_pathway_n", "icu_pathway_dow", "hold_pathway_n", "hold_pathway_dow"
)

#' Resource-name pattern of the R2E intensive care pool
#'
#' @details The same pattern R/policy_sweep.R's `POLICY_POOLS` uses for the
#'   same pool, kept as a separate constant here rather than a shared import
#'   so that this module does not depend on R/policy_sweep.R for a single
#'   string.
ICU_GATE_ICU_POOL <- "^b_r2eheavy_icu_[0-9]+_t[0-9]+$"

#' Attribute value recording the intensive care pathway
#'
#' @details `post_op_pathway` is a simmer attribute and therefore numeric: 1
#'   is the intensive care bed and 2 the degraded holding-bed fallback, as
#'   `R/trajectories.R` sets them at the stabilisation stage between a damage
#'   control casualty's two operations.
ICU_GATE_PATHWAY_ICU  <- 1

#' Attribute value recording the degraded holding-bed pathway
#'
#' @details The counterpart to `ICU_GATE_PATHWAY_ICU` above, on the same
#'   `post_op_pathway` attribute.
ICU_GATE_PATHWAY_HOLD <- 2

#' `dow_echelon` value recording a post-operative death
#'
#' @details Kept distinct from the Phase 1 R2E arrival checkpoint
#'   (`dow_echelon == 3`) so that the two pathways' realised post-operative
#'   mortality, read off `post_op_pathway`, can be compared to each other
#'   rather than to arrival mortality (`R/analysis.R`,
#'   `summarise_post_operative_pathways()`).
ICU_GATE_DOW_ECHELON_POSTOP <- 4

#' Mean occupancy of the R2E intensive care pool over the whole campaign
#'
#' @param resources Resource-monitor rows for one replication.
#' @param n_days Campaign length in days.
#' @return Time-weighted mean served fraction of the pool's capacity over the
#'   full campaign, or NA where the monitor carries no row for the pool.
#'
#' @details The monitor records each bed separately, so the pool total is in
#'   none of its rows and is recovered by differencing each bed's series into
#'   changes and accumulating them in time order, on the convention
#'   R/policy_sweep.R's `policy_pool_state()` establishes for the same pool
#'   over a closing window rather than the whole campaign.
icu_gate_occupancy <- function(resources, n_days) {
  rows <- resources[grepl(ICU_GATE_ICU_POOL, resources$resource), ]
  if (nrow(rows) == 0) return(NA_real_)

  edges    <- c(0, n_days * DAY_MIN)
  server   <- pool_queue_steps(rows$resource, rows$time, rows$server)
  capacity <- sum(tapply(rows$capacity, rows$resource, max))

  step_bin_means(server, edges) / capacity
}

#' Reduce one replication to the intensive care gate's response row
#'
#' @param env Wrapped simmer environment for one replication.
#' @param n_days Campaign length in days.
#' @param gate_enabled Gate state the replication ran under (0 or 1).
#' @return One-row data frame of the response set.
#'
#' @details The pathway counts are carried as raw numerators and
#'   denominators rather than as a per-replication rate, because most
#'   replications carry a handful of deaths on each pathway; a rate pooled
#'   after summing across replications is the quantity the published
#'   comparison reads, not the mean of many noisy per-replication rates.
reduce_icu_gate_replication <- function(env, n_days, gate_enabled) {
  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  attributes <- simmer::get_mon_attributes(env)
  resources  <- simmer::get_mon_resources(env)

  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"),
                      suffix = c("", "_arrival"))

  occupancy <- icu_gate_occupancy(resources, n_days)

  pathway <- wide[!is.na(wide$post_op_pathway), ]
  postop_dow <- !is.na(pathway$dow_echelon) &
    pathway$dow_echelon == ICU_GATE_DOW_ECHELON_POSTOP & pathway$dow == 1
  icu_rows  <- pathway$post_op_pathway == ICU_GATE_PATHWAY_ICU
  hold_rows <- pathway$post_op_pathway == ICU_GATE_PATHWAY_HOLD

  data.frame(
    gate_enabled     = gate_enabled,
    icu_occupancy    = occupancy,
    total_dow        = sum(!is.na(wide$dow) & wide$dow == 1),
    total_casualties = nrow(arrivals),
    icu_pathway_n    = sum(icu_rows),
    icu_pathway_dow  = sum(postop_dow[icu_rows]),
    hold_pathway_n   = sum(hold_rows),
    hold_pathway_dow = sum(postop_dow[hold_rows])
  )
}

#' Set the intensive care gate on a resolved configuration
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param gate_enabled Gate state to set at both echelons (0 or 1).
#' @return Invisibly, the described configuration that was applied.
#'
#' @details `icu_gating.enabled` is a variable rather than a bed count, so it
#'   is set after `build_environment()` on the described form, on the
#'   convention `R/hold_window.R`'s `apply_hold_window_setting()`
#'   establishes: `vars` is a list of name and value pairs only once
#'   `build_environment()` has named them.
apply_icu_gate_setting <- function(json_data, scenario, gate_enabled) {
  resolved <- resolve_scenario(json_data, scenario)
  described <- build_environment(resolved)
  described$vars$r2b$icu_gating$enabled      <- gate_enabled
  described$vars$r2eheavy$icu_gating$enabled <- gate_enabled
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Measure the intensive care gate response set across replications at one arm
#'
#' @param gate_enabled Gate state in force (0 or 1).
#' @param n_iterations Replications to run (default ICU_GATE_REPLICATIONS).
#' @param n_days Campaign length in days (default ICU_GATE_DAYS).
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @return Data frame with one row per replication, carrying the replication
#'   index and the responses reduce_icu_gate_replication() reports.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed under the caller's generator kind, and the
#'   caller's stream is restored on exit. A caller who sets the same control
#'   seed before each arm gives replication $k$ of every arm the same parent
#'   seed, pairing the two arms.
run_icu_gate_measurement <- function(gate_enabled, n_iterations = ICU_GATE_REPLICATIONS,
                                     n_days = ICU_GATE_DAYS, max_cores = NULL) {
  rng_state <- capture_rng_state()
  on.exit(restore_rng_state(rng_state), add = TRUE)

  rep_seeds <- sample.int(.Machine$integer.max, n_iterations)
  RNGkind("L'Ecuyer-CMRG")

  #' Run one replication and return its reduced response row alone
  #'
  #' @param i Index of the replication, into `rep_seeds`.
  #' @return The replication's one-row response frame, carrying its index.
  #'
  #' @details `run_once()` is wrapped in `capture.output()`, on the
  #'   convention `scripts/check_icu_gate_switch.R` establishes, so the
  #'   per-casualty trace `assign_injury_attributes()` logs
  #'   (`R/trajectories.R`) does not serialise a hundred replications' worth
  #'   of console output through the fork's stdout pipe.
  worker <- function(i) {
    env <- NULL
    invisible(capture.output(env <- run_once(n_days, seed = rep_seeds[i], write_files = FALSE)))
    row <- reduce_icu_gate_replication(env, n_days, gate_enabled)
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
#'   run_icu_gate_measurement().
#' @param arm_column Name of the column identifying which arm a row belongs
#'   to, which is summarised over rather than as a response.
#' @return Data frame of response, n_reps, mean, ci_lower and ci_upper, one
#'   row per response.
#'
#' @details A response no replication produced is reported with an `n_reps`
#'   of zero rather than dropped, so a reader can tell an absent measurement
#'   from a measured zero.
summarise_icu_gate <- function(rows, arm_column = "gate_enabled") {
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

#' Paired difference in one response between two arms
#'
#' @param rows Per-replication responses carrying both arms.
#' @param response Name of the response to difference.
#' @param from Arm to measure the change from (0 or 1).
#' @param to Arm to measure the change to (0 or 1).
#' @param arm_column Name of the column `from` and `to` are values of.
#' @return One-row data frame of response, from, to, n_pairs, mean
#'   difference, its 95% interval and the paired t-test's p-value.
#'
#' @details The difference is taken within replication rather than between
#'   arm means, the arms sharing a seed vector, on the convention
#'   `R/hold_window.R`'s `hold_window_paired_difference()` and
#'   `R/policy_sweep.R`'s `policy_paired_difference()` both establish.
#'   `n_pairs` is reported so that a difference resting on few pairs is
#'   visible, and the p-value so that a difference the count cannot
#'   establish is reported as unresolved rather than as absent.
icu_gate_paired_difference <- function(rows, response, from, to,
                                       arm_column = "gate_enabled") {
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
#' @param rows Per-replication responses carrying both arms.
#' @param response Name of the response to size.
#' @param from Arm to measure the change from (0 or 1).
#' @param to Arm to measure the change to (0 or 1).
#' @param half_width Half-width wanted on the difference, in the response's
#'   own units.
#' @param arm_column Name of the column `from` and `to` are values of.
#' @return The replication count required, rounded up, or NA where the
#'   measured pairs give no usable standard deviation.
#'
#' @details Uses the measured paired standard deviation and the normal
#'   approximation $n = (1.96 s / h)^2$, on the convention
#'   `R/policy_sweep.R`'s `policy_replications_for()` establishes and
#'   `docs/Multi_Run_Supplement.md`'s replication-count derivation uses.
icu_gate_replications_for <- function(rows, response, from, to, half_width,
                                      arm_column = "gate_enabled") {
  a <- rows[rows[[arm_column]] == from, c("replication", response)]
  b <- rows[rows[[arm_column]] == to, c("replication", response)]
  pairs <- merge(a, b, by = "replication", suffixes = c("_from", "_to"))
  d <- pairs[[paste0(response, "_to")]] - pairs[[paste0(response, "_from")]]
  d <- d[!is.na(d)]
  if (length(d) < 2 || sd(d) == 0 || half_width <= 0) return(NA_real_)
  ceiling((qnorm(0.975) * sd(d) / half_width)^2)
}
