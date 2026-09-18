#!/usr/bin/env Rscript
##############################################################################
## scripts/check_holding_occupancy_split.R                                  ##
## Regression check — R2E holding occupancy splits into recovery and        ##
## evacuation wait, and the two account for the pool within a stated bound  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_holding_occupancy_split.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. One pool of R2E holding beds carries two unrelated
# demands: casualties recovering to duty in theatre, and casualties waiting for
# a strategic evacuation sortie. Every intensive care and theatre finding in the
# companion paper has to be read alongside that pool's occupancy, so the split
# between the two is a reported quantity rather than an internal one, and a
# split that does not account for the pool would misattribute clinical capacity
# to logistics or the reverse.
#
# The attribution is computed rather than estimated, and this check is what
# holds it so. A casualty awaiting a sortie on either airlift pool stages in a
# holding bed and releases it on boarding, so its staging wait is holding
# occupancy; the three clinical stays that share the pool each record the
# instant their bed was seized and the duration to be served. Every component
# is computed independently rather than one of them being taken as the
# remainder, so the sum can disagree with the monitor, and what this check
# measures is whether it does.
#
# What this asserts:
#
#   1. The four components account for the pool's measured occupancy exactly,
#      at the shipped configuration, under a cancellation rate that forms a
#      backlog, and under one that leaves casualties staged at the close.
#   2. The evacuation component is bounded by the pool: never negative, never
#      more than the total.
#   3. It counts both airlift routes, asserted against a run carrying both.
#   4. The evacuation component is recomputed independently, casualty by
#      casualty from the attribute monitor, and agrees.
#   5. A casualty still waiting when the window closes is charged to the
#      window's end rather than dropped.
#   6. A casualty who dies while awaiting a sortie is charged to its death
#      rather than to the window's close, and the superseded estimator that
#      charged it to the close is shown still to over-count.
#
# Assertion 4 is what keeps the rest from being circular: assertions 1 and 2
# would hold for any evacuation component the code happened to produce.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/airlift.R")

state <- new.env(parent = emptyenv())
state$failures <- character(0)

#' Record a failure, deferring the non-zero exit to the end of the run
#'
#' @param ... sprintf() format string and its arguments.
#' @return Invisibly, the accumulated failure vector.
fail <- function(...) state$failures <- c(state$failures, sprintf(...))

#' Print one PASS or FAIL line, recording a failure
#'
#' @param ok TRUE when the assertion held.
#' @param fmt sprintf() format string describing the assertion.
#' @param ... Arguments to `fmt`.
#' @return Invisible NULL.
#'
#' @details Anything other than TRUE is a failure, NA included. A quantity this
#'   check compares can become NA when the code under test is wrong rather than
#'   when the assertion is inapplicable, and testing `if (ok)` on an NA raises
#'   an error that stops the run at the first such assertion instead of
#'   reporting it and continuing. That is how a fault injected into the
#'   evacuation component presented before this was written: the check detected
#'   it and died rather than failing.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' Scenario profile the check runs under
CHECK_SCENARIO <- "moderate_intensity"

#' Campaign length the check runs over, in days
CHECK_DAYS <- 30L

#' Control seed every run here is made under
CHECK_SEED <- 42L

#' Cancellation probability the backlog arm is run at
#'
#' @details High enough that sorties are lost and a backlog forms, so the
#'   evacuation component is a substantial share of the pool rather than the
#'   near-zero it is at the shipped reliability, and the assertions below are
#'   not made against an arm where the quantity barely exists.
CHECK_BACKLOG_FAILURE <- 0.40

#' Cancellation probability the censoring arm is run at
#'
#' @details Higher than the backlog arm because at 0.40 every casualty that
#'   reaches a staging bed still boards before the window closes, which would
#'   leave the censoring assertion vacuous. At this rate the sorties are lost
#'   often enough that casualties are still holding a staging bed at the close,
#'   which is the case that assertion exists to cover.
CHECK_CENSORING_FAILURE <- 0.75

#' Tolerance on a comparison of two computed reals, in bed-days
TOL <- 1e-8

#' Tracked airlift responses the population closure is measured on
POPULATION_PATH <- file.path("data", "airlift", "airlift_replications.csv")

#' Tolerance a replication counts as closing exactly within, in bed-days
POPULATION_TOL <- 1e-6

#' Largest over-count any replication may leave, as a share of its own pool
#'
#' @details Zero. The components over-counted a minority of campaigns until the
#'   staging stay was bounded by the instant its bed was released rather than by
#'   the instant its casualty departed: a casualty who died while awaiting a
#'   sortie never departs, so that casualty was charged with the whole of a wait
#'   it did not serve. With the stay bounded by `ame_hold_end`, which both
#'   routes out of the staging bed set, no campaign in the tracked set
#'   over-counts. The bound is a millionth of the pool rather than zero because
#'   the total is integrated from the monitor's step function while the
#'   components are summed from recorded durations, so the two reach the same
#'   quantity by different arithmetic and agree to floating-point rather than
#'   to the bit.
POPULATION_OVERCOUNT_SHARE <- 1e-6

#' Share of replications required to close exactly
#'
#' @details Every one. Closure is exact for each of the 650 tracked
#'   replications, so anything less is a regression rather than ordinary
#'   variation between evidence sets.
POPULATION_EXACT_SHARE <- 1

#' Tolerance the four components are required to close the pool within, in
#' bed-days
#'
#' @details Looser than TOL because the pool total is integrated from the
#'   resource monitor's step function while the components are summed from
#'   recorded durations, so the two reach the same quantity by different
#'   arithmetic. A thousandth of a bed-day is under two minutes of one bed.
CLOSE_TOL <- 1e-3

#' Run one campaign and return its monitors and its split
#'
#' @param failure_probability Sortie cancellation probability to run at.
#' @return A list of `wide` (per-casualty attributes joined to arrivals),
#'   `resources`, `split` and `horizon_min`.
measure <- function(failure_probability) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
  apply_airlift_setting(json_data, CHECK_SCENARIO, "failure_probability",
                        failure_probability)

  set.seed(CHECK_SEED)
  env <- run_once(CHECK_DAYS, seed = CHECK_SEED)

  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  attributes <- simmer::get_mon_attributes(env)
  resources  <- simmer::get_mon_resources(env)
  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"), suffix = c("", "_arrival"))

  horizon_min <- CHECK_DAYS * DAY_MIN
  list(wide = wide, resources = resources, horizon_min = horizon_min,
       split = holding_occupancy_split(resources, wide, horizon_min))
}

shipped   <- measure(0)
backlog   <- measure(CHECK_BACKLOG_FAILURE)
censoring <- measure(CHECK_CENSORING_FAILURE)

# ── 1. The components account for the pool ───────────────────────────────────

cat("\n-- the four components account for the pool in the campaigns run here --\n")

for (arm in list(list("shipped reliability", shipped),
                 list("a cancellation rate of 0.40", backlog),
                 list("a cancellation rate of 0.75", censoring))) {
  label <- arm[[1]]
  s     <- arm[[2]]$split
  report(abs(s$unexplained_bed_days) <= CLOSE_TOL,
         "%s: the four stays leave %.6f bed-days of %.3f unexplained",
         label, s$unexplained_bed_days, s$total_bed_days)
  report(abs(s$accounted_bed_days - (s$evacuation_bed_days + s$recovery_bed_days +
                                       s$post_definitive_bed_days +
                                       s$post_op_hold_bed_days)) < TOL,
         "%s: the reported sum is the four components (%.3f)", label, s$accounted_bed_days)
  for (part in c("recovery_bed_days", "post_definitive_bed_days", "post_op_hold_bed_days")) {
    report(s[[part]] > 0,
           "%s: %s is positive (%.3f), so no component is vacuous",
           label, part, s[[part]])
  }
}

# ── 1b. Closure across the tracked population, not one campaign ──────────────

cat("\n-- closure across the tracked replications --\n")

# The three campaigns above close exactly, and that is not evidence that every
# campaign does: closure is a property of the population, and asserting it on one
# seed is how a split that fails on a minority of campaigns passes a check. That
# is how the defect this bound once documented survived, the campaigns run here
# happening to carry no casualty who died in a staging bed. The tracked evidence
# set carries the residual for 650 replications, so the property is asserted
# where it can be measured rather than where it happens to hold.
if (!file.exists(POPULATION_PATH)) {
  report(FALSE, "the tracked airlift responses %s exist", POPULATION_PATH)
} else {
  population <- read.csv(POPULATION_PATH, stringsAsFactors = FALSE)
  if (!"hold_unexplained_bed_days" %in% names(population)) {
    report(FALSE, "the tracked responses carry the unexplained component")
  } else {
    residual <- population$hold_unexplained_bed_days
    total <- population$hold_total_bed_days
    report(length(residual) > 100,
           "the population is large enough to measure closure on (%d replications)",
           length(residual))

    # The components may leave part of the pool unexplained; they must never
    # claim more of it than the monitor measured. An under-count means a stay
    # the split does not know about, which is the error that matters, and it is
    # the direction the superseded standard-route-only estimator failed in.
    report(max(residual) <= POPULATION_TOL,
           "no replication under-counts the pool (largest residual %+.4f bed-days)",
           max(residual))

    worst <- min(residual / total)
    report(abs(worst) <= POPULATION_OVERCOUNT_SHARE,
           "the worst over-count is %.3f%% of its pool, within the stated %.1f%%",
           100 * abs(worst), 100 * POPULATION_OVERCOUNT_SHARE)

    exact <- sum(abs(residual) <= POPULATION_TOL)
    report(exact / length(residual) >= POPULATION_EXACT_SHARE,
           "%d of %d replications close exactly (%.1f%%), at or above the stated %.0f%%",
           exact, length(residual), 100 * exact / length(residual),
           100 * POPULATION_EXACT_SHARE)

    # Stated so that a reader of the check knows the residual is measured rather
    # than assumed away, and so that a change which worsens it is visible as a
    # number rather than only as a threshold breach.
    cat(sprintf("       mean residual %+.4f bed-days of %.1f (%.3f%% of the pool)\n",
                mean(residual), mean(total), 100 * mean(residual) / mean(total)))
  }
}

# ── 2. The evacuation component is bounded by the pool ───────────────────────

cat("\n-- the evacuation component is bounded by the pool --\n")

for (arm in list(list("shipped reliability", shipped),
                 list("a cancellation rate of 0.40", backlog))) {
  label <- arm[[1]]
  s     <- arm[[2]]$split
  report(s$evacuation_bed_days >= -TOL,
         "%s: the evacuation component is not negative (%.3f)", label,
         s$evacuation_bed_days)
  report(s$evacuation_bed_days <= s$total_bed_days + TOL,
         "%s: the evacuation component does not exceed the pool (%.3f of %.3f)",
         label, s$evacuation_bed_days, s$total_bed_days)
  report(s$recovery_bed_days >= -TOL,
         "%s: the recovery component is not negative (%.3f)", label,
         s$recovery_bed_days)
}

report(backlog$split$evacuation_bed_days > shipped$split$evacuation_bed_days,
       "losing sorties raises the evacuation component (%.3f against %.3f bed-days)",
       backlog$split$evacuation_bed_days, shipped$split$evacuation_bed_days)

# ── 3-4. It counts both routes, and agrees with an independent recount ───────

cat("\n-- both airlift routes stage in a holding bed --\n")

routes <- backlog$wide$ame_route[!is.na(backlog$wide$ame_route)]
report(any(routes == AIRLIFT_ROUTE_CRITICAL) && any(routes == AIRLIFT_ROUTE_STANDARD),
       "the run carries both routes, so the distinction is not vacuous (%d critical, %d standard)",
       sum(routes == AIRLIFT_ROUTE_CRITICAL), sum(routes == AIRLIFT_ROUTE_STANDARD))

#' The evacuation component of a set of casualties, recomputed independently
#'
#' @param wide Per-casualty attributes.
#' @param route Route code to include, or NA for every route.
#' @param horizon_min End of the campaign window, in minutes.
#' @return Bed-days those casualties spent in a staging holding bed.
#'
#' @details Written from the attribute monitor casualty by casualty rather than
#'   by calling the function under test, so that agreeing with it is evidence
#'   rather than a restatement. The stay runs from the instant the staging bed
#'   was seized, which is what `ame_hold_start` records, rather than from the
#'   evacuation decision: a ventilated casualty on the critical route holds an
#'   intensive care bed for its pre-flight period first and reaches a holding
#'   bed only on step-down. It ends where the bed was released, which
#'   `ame_hold_end` records on both routes out of the staging bed, rather than
#'   where the casualty departed, which a casualty who died waiting never does.
recompute <- function(wide, route, horizon_min) {
  rows <- wide[!is.na(wide$ame_hold_start), ]
  if (!is.na(route)) rows <- rows[!is.na(rows$ame_route) & rows$ame_route == route, ]
  total <- 0
  for (i in seq_len(nrow(rows))) {
    start <- rows$ame_hold_start[i]
    end   <- if (is.na(rows$ame_hold_end[i])) horizon_min else rows$ame_hold_end[i]
    if (start >= horizon_min) next
    total <- total + max(min(end, horizon_min) - start, 0)
  }
  total / DAY_MIN
}

# The critical route is counted, which is the property this section exists to
# hold: the published claim it replaces said the critical route contributed
# nothing, and counting the standard route alone left 14.7% of the pool
# unexplained at the shipped configuration.
critical_only <- recompute(backlog$wide, AIRLIFT_ROUTE_CRITICAL, backlog$horizon_min)
standard_only <- recompute(backlog$wide, AIRLIFT_ROUTE_STANDARD, backlog$horizon_min)
report(critical_only > 0,
       "the critical route holds staging beds and is counted (%.3f bed-days)", critical_only)
report(abs(backlog$split$evacuation_bed_days - (critical_only + standard_only)) < TOL,
       "the component is both routes together (%.3f = %.3f critical + %.3f standard)",
       backlog$split$evacuation_bed_days, critical_only, standard_only)

# A ventilated critical casualty's holding stay is shorter than its whole wait,
# the pre-flight period being served in an intensive care bed. This is what
# makes the component smaller than the wait rather than equal to it.
vent <- backlog$wide[!is.na(backlog$wide$ame_icu_hold) & backlog$wide$ame_icu_hold == 1 &
                       !is.na(backlog$wide$ame_hold_start) &
                       !is.na(backlog$wide$r2e_departure_time), ]
report(nrow(vent) > 0,
       "the run carries ventilated pre-flight holds, so this is not vacuous (%d)", nrow(vent))
if (nrow(vent) > 0) {
  report(all(vent$ame_hold_start > vent$r2e_departure_time),
         "every ventilated casualty reaches its staging bed after the decision, not at it")
}

cat("\n-- the evacuation component agrees with a casualty-by-casualty recount --\n")

for (arm in list(list("shipped reliability", shipped),
                 list("a cancellation rate of 0.40", backlog))) {
  label <- arm[[1]]
  a     <- arm[[2]]
  again <- recompute(a$wide, NA, a$horizon_min)
  report(abs(a$split$evacuation_bed_days - again) < TOL,
         "%s: the recount agrees (%.6f against %.6f bed-days)",
         label, a$split$evacuation_bed_days, again)
}

# ── 5. A casualty still waiting at the window's close is carried ─────────────

cat("\n-- a wait still running when the window closes is carried, not dropped --\n")

still_waiting <- censoring$wide[is.na(censoring$wide$ame_hold_end) &
                                  !is.na(censoring$wide$ame_hold_start) &
                                  censoring$wide$ame_hold_start < censoring$horizon_min, ]
report(nrow(still_waiting) > 0,
       "the backlog arm leaves casualties waiting at the close, so this is not vacuous (%d)",
       nrow(still_waiting))

if (nrow(still_waiting) > 0) {
  censored <- sum(pmax(censoring$horizon_min - still_waiting$ame_hold_start, 0)) / DAY_MIN
  boarded_rows <- censoring$wide[!is.na(censoring$wide$ame_hold_end), ]
  departed <- recompute(boarded_rows, NA, censoring$horizon_min)
  report(abs(censoring$split$evacuation_bed_days - (censored + departed)) < TOL,
         "the component is the boarded stays plus the open ones charged to the close (%.3f + %.3f)",
         departed, censored)
  report(censored > 0, "the open stays contribute a positive amount (%.3f bed-days)", censored)

  # A casualty that reached the evacuation decision but never obtained a
  # staging bed occupies none of the pool, so it contributes nothing however
  # long it waits. The censoring arm saturates the pool, which is what makes
  # this distinguishable from the case above.
  never_held <- censoring$wide[!is.na(censoring$wide$r2e_evac) &
                                 censoring$wide$r2e_evac == 1 &
                                 is.na(censoring$wide$ame_hold_start), ]
  report(nrow(never_held) > 0,
         "the censoring arm leaves casualties queued for a staging bed (%d)",
         nrow(never_held))
  report(abs(censoring$split$unexplained_bed_days) <= CLOSE_TOL,
         "and the pool still closes exactly (%.6f bed-days unexplained)",
         censoring$split$unexplained_bed_days)
}

# ── 6. A casualty who dies awaiting a sortie is charged to its death ───────

cat("\n-- a casualty who dies awaiting a sortie is charged to its death --\n")

#' The evacuation component under the superseded departure-bounded estimator
#'
#' @param wide Per-casualty attributes.
#' @param horizon_min End of the campaign window, in minutes.
#' @return Bed-days the staging stays would contribute if each were bounded by
#'   its casualty's departure rather than by the release of its bed.
#'
#' @details Kept so that the correction is asserted against the behaviour it
#'   replaced rather than only against the monitor. A casualty who died while
#'   waiting has no departure time, so this estimator charges it to the
#'   window's close and over-counts the pool by the wait it did not serve.
superseded <- function(wide, horizon_min) {
  rows <- wide[!is.na(wide$ame_hold_start) & wide$ame_hold_start < horizon_min, ]
  end  <- ifelse(is.na(rows$ame_departure_time), horizon_min, rows$ame_departure_time)
  sum(pmax(pmin(end, horizon_min) - rows$ame_hold_start, 0)) / DAY_MIN
}

for (arm in list(list("shipped reliability", shipped),
                 list("a cancellation rate of 0.40", backlog))) {
  label <- arm[[1]]
  a     <- arm[[2]]
  died  <- a$wide[!is.na(a$wide$dow_echelon) & a$wide$dow_echelon == 5 &
                    !is.na(a$wide$ame_hold_start), ]
  report(nrow(died) > 0,
         "%s: the run carries a casualty who died in a staging bed, so this is not vacuous (%d)",
         label, nrow(died))
  if (nrow(died) == 0) next

  report(all(!is.na(died$ame_hold_end)),
         "%s: every such casualty records the release of its staging bed", label)
  report(all(is.na(died$ame_departure_time)),
         "%s: and none of them departs, which is why the release is needed", label)
  report(all(died$ame_hold_end < a$horizon_min - TOL),
         "%s: each release falls inside the window rather than at its close", label)

  # The gap between the two estimators is exactly the unserved remainder of the
  # window, which is what the superseded one claimed. Asserting the size rather
  # than only the direction is what would catch a correction that moved the
  # boundary to some other instant.
  unserved <- sum(a$horizon_min - died$ame_hold_end) / DAY_MIN
  report(abs((superseded(a$wide, a$horizon_min) - a$split$evacuation_bed_days) -
               unserved) < TOL,
         "%s: the superseded estimator over-counts by the %.3f bed-days they did not serve",
         label, unserved)
  report(unserved > CLOSE_TOL,
         "%s: and that over-count is large enough to have mattered (%.3f bed-days)",
         label, unserved)
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All holding occupancy split checks passed.\n")
quit(status = 0)
