##############################################################################
## R/dow_calibration.R                                                      ##
## The treated-cohort died-of-wounds rate, and the search for the ceilings  ##
## that reproduce a target of it                                            ##
##############################################################################
#
# The died-of-wounds ceilings (`p1_p_max`, `p2_p_max`) are calibrated rather
# than sourced: no dataset gives a per-minute survival curve, so each is
# searched for until the model's treated-cohort rate matches a historical
# target (README, Parameter Calibration). The post-operative hazard rates
# (`dow.post_op_hazard`) are calibrated alongside them, to the share of the
# treated cohort's expected deaths the hazard causes. This module holds the two halves of
# that procedure so one measurement serves the regression check that asks
# whether a configuration still agrees with its anchor
# (tests/checks/check_dow_calibration.R) and the fitter that asks what ceilings
# would make it agree (scripts/calibrate_dow.R).
#
#   1. The measurement. The treated cohort is the casualties who reached a
#      surgical facility alive, identified by the r2b_treated and r2e_treated
#      attributes, which are set on entry to each facility's trajectory and so
#      before that facility's own died-of-wounds check: a casualty who dies
#      there is inside their own denominator. The rate is pooled across
#      independent measurements, each at its own fixed control seed, because
#      one 50-replication measurement does not resolve it (docs/Methods.md,
#      Replication Count and Resolution).
#
#      The same runs give the hazard's share: each check splits its death
#      probability between the early, logistic risk and the post-operative
#      hazard (dow_settle_check(), R/trajectories_common.R), and the running
#      totals summed over the cohort are its expected deaths from each. An
#      expected count rather than a count of deaths, so the share carries no
#      Bernoulli noise.
#
#   2. The search. `fit_dow_ceiling()` is a bracketing root search, Illinois
#      false position, on the pooled rate as a function of the Priority 1
#      ceiling, the Priority 2 ceiling following at a fixed ratio. It takes the
#      measurement as a function argument, so it is base R and a regression
#      check can exercise it on a response whose answer is known, without
#      simmer or a run. `fit_hazard_scale()` runs the same search on the
#      hazard's share as a function of a common scale on the three rates.
#
# The measurement needs simmer, dplyr and the model modules sourced first
# (R/environment.R, R/trajectories.R, R/replication.R, R/scenario.R); the
# search needs nothing.

#' Historical anchor each calibrated configuration is checked against
#'
#' @details A list keyed by scenario name. `kind` is "bound" where the anchor is
#'   an upper bound on the treated-cohort rate (Ajax Bay, whose denominator is
#'   given as "over 650"), so only an overshoot fails, and "point" where it is a
#'   reported rate against a stated denominator, so the model must reach it.
#'   `rate` is the treated-cohort proportion. `high_intensity` carries no
#'   anchor: it shares the default's mortality model and differs from it in
#'   casualty generation alone, so its mortality is a model output rather than
#'   a calibration target.
DOW_TARGETS <- list(
  default            = list(rate = 0.0046, kind = "bound", label = "Ajax Bay"),
  moderate_intensity = list(rate = 0.0046, kind = "bound", label = "Ajax Bay")
)

#' Share of the treated cohort's expected deaths the post-operative hazard causes
#'
#' @details In a combat cohort 88 of 108 deaths within thirty days (81%) fell
#'   in the first 24 hours (Shackelford et al., 2017), so about one in five came
#'   later, from the complications post-operative care exists to manage. The
#'   model's early, logistic risk is all but spent within hours of injury, so
#'   the late fifth is the hazard's share. It is taken by mechanism rather than
#'   by the time of the check that records a death, a check settling risk that
#'   accrued hours before it (README Died of Wounds, Post-Operative Hazard).
DOW_LATE_SHARE_TARGET <- 0.20

#' Tolerance of the hazard's share, as an absolute difference
DOW_LATE_SHARE_TOLERANCE <- 0.03

#' Tolerance of a point anchor, as an absolute difference in the rate
DOW_TOLERANCE <- 0.02

#' Control seeds the pooled measurements are drawn under
#'
#' @details Each seeds one independent measurement, and each is fixed so that
#'   two runs on unchanged code agree exactly. The fitter reuses the same seeds
#'   at every ceiling it tries, so the pooled rate varies with the ceiling
#'   rather than with the draw.
DOW_CONTROL_SEEDS <- c(42L, 777L, 20260808L, 13L, 20261L)

#' Treated-cohort died-of-wounds rate of every replication in a monitoring list
#'
#' @param mon Monitoring list from `run_replications()`.
#' @return Numeric vector of one rate per replication, in replication order,
#'   `NA` where no casualty of that replication reached a facility.
treated_cohort_rates <- function(mon) {
  treated <- mon$attributes %>%
    dplyr::filter(key %in% c("r2b_treated", "r2e_treated")) %>%
    dplyr::distinct(replication, name)
  died <- mon$attributes %>%
    dplyr::filter(key == "dow", value == 1) %>%
    dplyr::distinct(replication, name)

  vapply(sort(unique(mon$arrivals$replication)), function(r) {
    cohort <- treated$name[treated$replication == r]
    if (!length(cohort)) return(NA_real_)
    sum(died$name[died$replication == r] %in% cohort) / length(cohort)
  }, numeric(1))
}

#' Expected deaths of the treated cohort by mechanism, per replication
#'
#' @param mon Monitoring list from `run_replications()`.
#' @return Data frame of `replication`, `early` and `late`: the cohort's
#'   expected deaths from the logistic risk and from the post-operative hazard,
#'   zero under the legacy mechanism, which records neither.
treated_cohort_expected <- function(mon) {
  treated <- mon$attributes %>%
    dplyr::filter(key %in% c("r2b_treated", "r2e_treated")) %>%
    dplyr::distinct(replication, name)
  totals <- mon$attributes %>%
    dplyr::filter(key %in% c("dow_expected_early", "dow_expected_late")) %>%
    dplyr::semi_join(treated, by = c("replication", "name")) %>%
    dplyr::group_by(replication, name, key) %>%
    dplyr::summarise(value = dplyr::last(value), .groups = "drop") %>%
    dplyr::group_by(replication, key) %>%
    dplyr::summarise(value = sum(value), .groups = "drop")
  reps <- sort(unique(mon$arrivals$replication))
  pick <- function(k) {
    v <- totals$value[totals$key == k][match(reps, totals$replication[totals$key == k])]
    ifelse(is.na(v), 0, v)
  }
  data.frame(replication = reps, early = pick("dow_expected_early"),
             late = pick("dow_expected_late"))
}

#' Mean and 95% interval of a set of per-replication rates
#'
#' @param rates Numeric vector of per-replication rates, without `NA`.
#' @return A list of `n`, `mean`, `half_width`, `ci_lower` and `ci_upper`.
#' @details The interval is the Student t one about the mean, with its lower
#'   bound clamped at zero on the basis `clamp_ci()` (R/analysis.R) uses: a
#'   mortality rate cannot be negative. Clamping cannot hide an overshoot, since
#'   it only moves the lower bound further below a bound.
pooled_rate_summary <- function(rates) {
  n  <- length(rates)
  m  <- mean(rates)
  hw <- if (n > 1) stats::qt(0.975, df = n - 1) * stats::sd(rates) / sqrt(n) else NA_real_
  list(n = n, mean = m, half_width = hw,
       ci_lower = max(m - hw, 0), ci_upper = m + hw)
}

#' Pooled treated-cohort rate of a configuration at given ceilings
#'
#' @param json_data The parsed configuration, as `read_config()` returns it.
#' @param scenario Scenario profile name, or "default" for the base configuration.
#' @param p1_p_max Priority 1 ceiling to run at, or NULL for the configuration's own.
#' @param p1_over_p2 Ratio of the Priority 1 ceiling to the Priority 2 ceiling.
#' @param n_reps Replications per measurement.
#' @param n_days Campaign length in days.
#' @param seeds Control seeds, one independent measurement each.
#' @return Numeric vector of every replication's treated-cohort rate across all
#'   measurements, `NA` removed.
#'
#' @details The rates of measure_dow_response(), for the callers that need
#'   nothing else.
measure_treated_cohort <- function(json_data, scenario, p1_p_max = NULL, p1_over_p2 = NULL,
                                   n_reps = 50L, n_days = 30L,
                                   seeds = DOW_CONTROL_SEEDS[1:3]) {
  measure_dow_response(json_data, scenario, p1_p_max = p1_p_max, p1_over_p2 = p1_over_p2,
                       n_reps = n_reps, n_days = n_days, seeds = seeds)$rates
}

#' Treated-cohort rate and the hazard's share of a configuration
#'
#' @param json_data The parsed configuration, as `read_config()` returns it.
#' @param scenario Scenario profile name, or "default" for the base configuration.
#' @param p1_p_max Priority 1 ceiling to run at, or NULL for the configuration's own.
#' @param p1_over_p2 Ratio of the Priority 1 ceiling to the Priority 2 ceiling
#'   held while `p1_p_max` is varied. Ignored where `p1_p_max` is NULL.
#' @param hazard_scale Common multiplier on the three post-operative hazard
#'   rates, or NULL for the configuration's own.
#' @param accrual 1 or 0 to override `dow.accrual.enabled`, or NULL for the
#'   configuration's own; 0 measures the legacy mechanism.
#' @param n_reps Replications per measurement.
#' @param n_days Campaign length in days.
#' @param seeds Control seeds, one independent measurement each.
#' @return A list of `rates`, every replication's treated-cohort rate across all
#'   measurements with `NA` removed, and `early` and `late`, the cohort's
#'   expected deaths by mechanism summed over every replication.
#'
#' @details The ceilings are set after `build_environment()` rather than on the
#'   parsed JSON, whose `vars` is a list of name and value pairs that
#'   `build_environment()` turns into the structure the model reads. The
#'   configuration globals are bound for the run and restored on exit, on the
#'   error path as well as the success path.
measure_dow_response <- function(json_data, scenario, p1_p_max = NULL, p1_over_p2 = NULL,
                                 hazard_scale = NULL, accrual = NULL,
                                 n_reps = 50L, n_days = 30L,
                                 seeds = DOW_CONTROL_SEEDS[1:3]) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  described <- build_environment(resolve_scenario(json_data, scenario))
  if (!is.null(p1_p_max)) {
    described$vars$dow$params$p1_p_max <- p1_p_max
    described$vars$dow$params$p2_p_max <- p1_p_max / p1_over_p2
  }
  if (!is.null(hazard_scale)) {
    for (k in c("p1_rate", "p2_rate", "p3_rate")) {
      described$vars$dow$post_op_hazard[[k]] <- described$vars$dow$post_op_hazard[[k]] *
        hazard_scale
    }
  }
  if (!is.null(accrual)) described$vars$dow$accrual$enabled <- accrual
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())

  pooled <- numeric(0)
  early <- 0
  late <- 0
  for (seed in seeds) {
    set.seed(seed)
    mon <- run_replications(n_reps, n_days)
    rates <- treated_cohort_rates(mon)
    pooled <- c(pooled, rates[!is.na(rates)])
    expected <- treated_cohort_expected(mon)
    early <- early + sum(expected$early)
    late <- late + sum(expected$late)
  }
  list(rates = pooled, early = early, late = late)
}

#' Search for the Priority 1 ceiling at which a measured rate meets a target
#'
#' @param evaluate Function of one argument, the Priority 1 ceiling, returning a
#'   named numeric of `rate`, `ci_lower` and `ci_upper`: the pooled
#'   treated-cohort rate at that ceiling and its 95% interval.
#' @param target The rate to reach.
#' @param lower,upper Bracket the ceiling is searched within.
#' @param tol_rate Absolute difference between the measured rate and the target
#'   at which the search stops.
#' @param tol_p Width of the bracket at which the search stops.
#' @param max_iter Cap on evaluations after the two bracket ends.
#' @return A list of `p1_p_max` (the evaluated ceiling closest to the target),
#'   `rate`, `ci_lower`, `ci_upper`, `converged`, `reason` and `trace`, a data
#'   frame of every evaluation in the order made.
#'
#' @details Illinois false position: the next ceiling is where the straight line
#'   between the bracket's two measurements crosses the target, and the weight on
#'   a bracket end that is retained twice running is halved, which keeps the
#'   bracket closing from both sides. The rate is taken to rise with the ceiling,
#'   which a larger ceiling cannot fail to do in expectation. It is measured, not
#'   exact, and a pooled rate carries sampling noise, so the search reports the
#'   evaluated ceiling nearest the target together with the interval of the rate
#'   it achieved, and stops on a tolerance rather than expecting an exact root.
#'   A bracket that does not contain the target is reported rather than searched,
#'   `converged` being FALSE and `reason` naming which end missed.
fit_dow_ceiling <- function(evaluate, target, lower, upper,
                            tol_rate = 5e-4, tol_p = 1e-5, max_iter = 12L) {
  state <- new.env(parent = emptyenv())
  state$rows <- list()
  #' Evaluate one ceiling and record it in the trace
  #'
  #' @param p Priority 1 ceiling to measure at.
  #' @return The named measurement `evaluate()` returned.
  measure <- function(p) {
    m <- evaluate(p)
    row <- data.frame(iteration = length(state$rows), p1_p_max = p, rate = m[["rate"]],
                      ci_lower = m[["ci_lower"]], ci_upper = m[["ci_upper"]])
    state$rows[[length(state$rows) + 1L]] <- row
    m
  }
  #' Assemble the result from the trace
  #'
  #' @param converged Whether the search met a tolerance.
  #' @param reason One line saying why the search stopped.
  #' @return The result list `fit_dow_ceiling()` returns.
  finish <- function(converged, reason) {
    trace <- do.call(rbind, state$rows)
    best  <- trace[which.min(abs(trace$rate - target)), ]
    list(p1_p_max = best$p1_p_max, rate = best$rate, ci_lower = best$ci_lower,
         ci_upper = best$ci_upper, converged = converged, reason = reason, trace = trace)
  }

  a <- lower
  b <- upper
  ga <- measure(a)[["rate"]] - target
  if (abs(ga) <= tol_rate) return(finish(TRUE, "the lower end of the bracket meets the target"))
  gb <- measure(b)[["rate"]] - target
  if (abs(gb) <= tol_rate) return(finish(TRUE, "the upper end of the bracket meets the target"))
  if (ga > 0) return(finish(FALSE, "the rate at the lower end already exceeds the target"))
  if (gb < 0) return(finish(FALSE, "the rate at the upper end is still below the target"))

  retained <- 0L
  for (i in seq_len(max_iter)) {
    if (b - a <= tol_p) return(finish(TRUE, "the bracket is narrower than the tolerance"))
    c_new <- (a * gb - b * ga) / (gb - ga)
    gc <- measure(c_new)[["rate"]] - target
    if (abs(gc) <= tol_rate) return(finish(TRUE, "the measured rate meets the target"))
    if (gc < 0) {
      a <- c_new
      ga <- gc
      if (retained == -1L) gb <- gb / 2
      retained <- -1L
    } else {
      b <- c_new
      gb <- gc
      if (retained == 1L) ga <- ga / 2
      retained <- 1L
    }
  }
  finish(FALSE, sprintf("the iteration cap of %d was reached", max_iter))
}

#' Search for the hazard scale at which the hazard's share meets a target
#'
#' @param evaluate Function of one argument, the common scale on the three
#'   post-operative hazard rates, returning the share of the treated cohort's
#'   expected deaths the hazard causes at that scale.
#' @param target The share to reach.
#' @param lower,upper Bracket the scale is searched within.
#' @param tol Absolute difference between the share and the target at which
#'   the search stops.
#' @param max_iter Cap on evaluations after the two bracket ends.
#' @return The list `fit_dow_ceiling()` returns, with the fitted scale under
#'   `scale` in place of `p1_p_max`.
#'
#' @details The share rises with the scale, so the ceiling search applies
#'   unchanged. The share is an expected count rather than a count of deaths,
#'   so the interval `fit_dow_ceiling()` expects collapses onto the estimate.
fit_hazard_scale <- function(evaluate, target, lower, upper, tol = 0.005, max_iter = 12L) {
  #' The share at one scale, in the form the ceiling search expects
  #'
  #' @param s Scale to measure at.
  #' @return Named numeric of `rate`, `ci_lower` and `ci_upper`, all the share.
  as_rate <- function(s) {
    share <- evaluate(s)
    c(rate = share, ci_lower = share, ci_upper = share)
  }
  fit <- fit_dow_ceiling(as_rate, target, lower, upper, tol_rate = tol, tol_p = 1e-3,
                         max_iter = max_iter)
  fit$scale <- fit$p1_p_max
  fit$p1_p_max <- NULL
  names(fit$trace)[names(fit$trace) == "p1_p_max"] <- "scale"
  fit
}
