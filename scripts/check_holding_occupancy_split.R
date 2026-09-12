#!/usr/bin/env Rscript
##############################################################################
## scripts/check_holding_occupancy_split.R                                  ##
## Regression check — R2E holding occupancy splits into recovery and        ##
## evacuation wait, and the two account for the pool exactly                ##
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
# The attribution is exact rather than estimated, and this check is what holds
# it exact. A casualty awaiting the standard airlift pool seizes a holding bed
# on reaching the evacuation decision and releases it on boarding, so its whole
# wait is holding occupancy. A casualty awaiting the critical pool holds an
# intensive care bed instead and must contribute nothing. Recovery is then the
# remainder of the measured pool occupancy, which makes the two sum to the total
# by construction; what needs checking is that the evacuation component is right,
# since an error there moves the same quantity out of recovery and the sum would
# still hold.
#
# What this asserts:
#
#   1. The two components sum to the pool's measured occupancy, at the shipped
#      configuration and under a cancellation rate that forms a backlog.
#   2. The evacuation component is bounded by the pool: never negative, never
#      more than the total.
#   3. It counts the standard route and not the critical one, asserted against
#      a run carrying both.
#   4. The evacuation component is recomputed independently, casualty by
#      casualty from the attribute monitor, and agrees.
#   5. A casualty still waiting when the window closes is charged to the
#      window's end rather than dropped.
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

#' Tolerance on a comparison of two computed reals, in bed-days
TOL <- 1e-8

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

shipped <- measure(0)
backlog <- measure(CHECK_BACKLOG_FAILURE)

# ── 1. The components account for the pool ───────────────────────────────────

cat("\n-- the two components account for the pool --\n")

for (arm in list(list("shipped reliability", shipped),
                 list("a cancellation rate of 0.40", backlog))) {
  label <- arm[[1]]
  s     <- arm[[2]]$split
  report(abs((s$evacuation_bed_days + s$recovery_bed_days) - s$total_bed_days) < TOL,
         "%s: evacuation (%.3f) plus recovery (%.3f) equals the pool total (%.3f)",
         label, s$evacuation_bed_days, s$recovery_bed_days, s$total_bed_days)
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

# ── 3. It counts the standard route and not the critical one ─────────────────

cat("\n-- the critical route holds intensive care, not a holding bed --\n")

routes <- backlog$wide$ame_route[!is.na(backlog$wide$ame_route)]
report(any(routes == AIRLIFT_ROUTE_CRITICAL) && any(routes == AIRLIFT_ROUTE_STANDARD),
       "the run carries both routes, so the distinction is not vacuous (%d critical, %d standard)",
       sum(routes == AIRLIFT_ROUTE_CRITICAL), sum(routes == AIRLIFT_ROUTE_STANDARD))

#' The evacuation component of a set of casualties, recomputed independently
#'
#' @param wide Per-casualty attributes.
#' @param route Route code to include, or NA for every route.
#' @param horizon_min End of the campaign window, in minutes.
#' @return Bed-days those casualties spent awaiting a sortie.
#'
#' @details Written from the attribute monitor casualty by casualty rather than
#'   by calling the function under test, so that agreeing with it is evidence
#'   rather than a restatement.
recompute <- function(wide, route, horizon_min) {
  rows <- wide[!is.na(wide$r2e_evac) & wide$r2e_evac == 1 &
                 !is.na(wide$r2e_departure_time), ]
  if (!is.na(route)) rows <- rows[!is.na(rows$ame_route) & rows$ame_route == route, ]
  total <- 0
  for (i in seq_len(nrow(rows))) {
    start <- rows$r2e_departure_time[i]
    end   <- if (is.na(rows$ame_departure_time[i])) horizon_min else rows$ame_departure_time[i]
    total <- total + max(min(end, horizon_min) - start, 0)
  }
  total / DAY_MIN
}

critical_only <- recompute(backlog$wide, AIRLIFT_ROUTE_CRITICAL, backlog$horizon_min)
report(critical_only > 0,
       "critical-route casualties do wait (%.3f bed-days if they were counted)",
       critical_only)
report(abs(backlog$split$evacuation_bed_days -
             recompute(backlog$wide, AIRLIFT_ROUTE_STANDARD, backlog$horizon_min)) < TOL,
       "the split counts the standard route alone, not both (%.3f)",
       backlog$split$evacuation_bed_days)

# ── 4. The evacuation component agrees with an independent recomputation ─────

cat("\n-- the evacuation component agrees with a casualty-by-casualty recount --\n")

for (arm in list(list("shipped reliability", shipped),
                 list("a cancellation rate of 0.40", backlog))) {
  label <- arm[[1]]
  a     <- arm[[2]]
  again <- recompute(a$wide, AIRLIFT_ROUTE_STANDARD, a$horizon_min)
  report(abs(a$split$evacuation_bed_days - again) < TOL,
         "%s: the recount agrees (%.6f against %.6f bed-days)",
         label, a$split$evacuation_bed_days, again)
}

# ── 5. A casualty still waiting at the window's close is carried ─────────────

cat("\n-- a wait still running when the window closes is carried, not dropped --\n")

still_waiting <- backlog$wide[!is.na(backlog$wide$r2e_evac) & backlog$wide$r2e_evac == 1 &
                                is.na(backlog$wide$ame_departure_time) &
                                !is.na(backlog$wide$ame_route) &
                                backlog$wide$ame_route == AIRLIFT_ROUTE_STANDARD, ]
report(nrow(still_waiting) > 0,
       "the backlog arm leaves casualties waiting at the close, so this is not vacuous (%d)",
       nrow(still_waiting))

if (nrow(still_waiting) > 0) {
  censored <- sum(pmax(backlog$horizon_min - still_waiting$r2e_departure_time, 0)) / DAY_MIN
  boarded_rows <- backlog$wide[!is.na(backlog$wide$ame_departure_time), ]
  departed <- recompute(boarded_rows, AIRLIFT_ROUTE_STANDARD, backlog$horizon_min)
  report(abs(backlog$split$evacuation_bed_days - (censored + departed)) < TOL,
         "the component is the boarded waits plus the open ones charged to the close (%.3f + %.3f)",
         departed, censored)
  report(censored > 0, "the open waits contribute a positive amount (%.3f bed-days)", censored)
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
