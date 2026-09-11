#!/usr/bin/env Rscript
##############################################################################
## scripts/check_hold_episode_reconstruction.R                              ##
## Regression check — an R2B holding episode is bounded by the attribute    ##
## its own exit route sets                                                  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_hold_episode_reconstruction.R
#   Rscript scripts/check_hold_episode_reconstruction.R --days 30
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. summarise_r2b_hold_occupancy() reconstructed each
# holding episode as r2b_hold_start to return_day, which is the right pair for
# only one of the three ways a casualty leaves an R2B holding bed (Issue #327).
#
# A casualty sent on under the evacuation threshold releases the bed at once
# and sets r2b_hold_served, the bed time actually served; their return_day is
# set later and at another echelon, so charging the bed to it holds them at R2B
# for the whole of a stay they spent at R2E. A casualty still holding a bed when
# the run ends sets neither attribute and was dropped entirely, which undercounts
# the days nearest the run's end, where the unfinished episodes are.
#
# Both errors reach a tracked figure and the single-run paper's reading of it.
# At seed 42 over 30 days the old reconstruction understated total holding
# bed-days by 13% at the shipped default, and overstated them by 76% under the
# 4320-minute evacuation threshold the papers recommend as a planning lever,
# reporting 303.2 bed-days against a true 172.9. A planner testing that lever
# would have been shown it barely working.
#
# What it asserts:
#
#   1. Every casualty who entered holding is represented. None is dropped for
#      want of an end attribute.
#   2. An episode ended by return to duty from holding ends at return_day.
#   3. An episode ended by onward evacuation ends at the served bed time, not
#      at the return_day its casualty sets later at another echelon.
#   4. An episode with neither attribute runs to the end of the window.
#   5. The reconstructed bed-days agree with the resource monitor, which counts
#      bed occupancy directly and owes nothing to the attributes. This is the
#      assertion that would catch a fourth exit route added later, since it
#      compares against the beds themselves rather than against a list of
#      routes this check knows about.
#   6. Assertion 5 holds with the evacuation threshold set as well as at the
#      shipped default, the two configurations exercising different exits.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
  library(ggplot2)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")

args <- commandArgs(trailingOnly = TRUE)

#' Read a flag's value from the command line
#'
#' @param flag Flag name, including its leading dashes.
#' @param default Value returned when the flag is absent or has no argument.
#' @return The argument following `flag`, or `default`.
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

#' Run length in days for the live runs
#'
#' @details Long enough that holding episodes both complete and are still
#'   running when the window closes, which is what makes the open-episode arm
#'   non-vacuous; the check confirms rather than assumes it.
CHECK_DAYS <- as.integer(arg_value("--days", 30L))

#' Seed for the live runs
CHECK_SEED <- as.integer(arg_value("--seed", 42L))

#' The evacuation threshold, in minutes, that exercises the onward exit
#'
#' @details 4320 minutes is the three-day setting the analysis documents
#'   discuss as a planning lever, and the value scripts/check_lever_realisation.R
#'   uses. The threshold is configured in minutes, not days.
HOLD_THRESHOLD_MIN <- 4320

#' Tolerance, as a share of the monitor's own total, on the bed-day comparison
#'
#' @details The two quantities are measured differently: the monitor integrates
#'   server occupancy over time, and the reconstruction sums per-casualty
#'   episode durations. They agree to under 2% at both configurations tested,
#'   and the residual is not accounted for here, so the tolerance is set to
#'   catch a reconstruction error rather than to certify exact agreement.
BED_DAY_TOLERANCE <- 0.05

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
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

#' Run the model and return what both measurements need
#'
#' @param threshold Evacuation threshold in minutes, or NULL for the shipped
#'   configuration, which sets none.
#' @return Named list of `episodes` (the reconstruction), `attributes_wide`
#'   and `monitor_bed_days`.
measure_holding <- function(threshold) {
  ed <- load_scenario("env_data.json", "default")
  if (!is.null(threshold)) ed$vars$r2b$holding$evac_threshold <- threshold
  env_data <<- ed
  day_min  <<- DAY_MIN
  counts   <<- sapply(env_data$elms, length)

  window <- CHECK_DAYS * DAY_MIN
  set.seed(CHECK_SEED)
  invisible(capture.output(wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)))
  arrivals   <- get_mon_arrivals(list(wrapped), ongoing = TRUE)
  wide       <- build_attributes_wide(get_mon_attributes(list(wrapped)), arrivals)
  episodes   <- r2b_hold_episodes(wide, window)

  beds <- as.data.frame(get_mon_resources(list(wrapped))) %>%
    filter(grepl("^b_r2b_hold_", resource)) %>%
    arrange(resource, time) %>%
    group_by(resource) %>%
    mutate(dt = pmin(dplyr::lead(time, default = window), window) - time) %>%
    ungroup() %>%
    filter(time < window)

  list(
    episodes         = episodes,
    attributes_wide  = wide,
    window           = window,
    monitor_bed_days = sum(beds$server * beds$dt) / DAY_MIN
  )
}

#' Total bed-days a set of reconstructed episodes accounts for
#'
#' @param episodes The frame r2b_hold_episodes() returns.
#' @param window End of the observation window, in minutes.
#' @return Bed-days, the episode durations clipped to the window.
episode_bed_days <- function(episodes, window) {
  sum(pmin(episodes$hold_end_min, window) - pmin(episodes$hold_start_min, window)) / DAY_MIN
}

cat(sprintf("Holding episode reconstruction check: %d-day runs, seed %d\n\n",
            CHECK_DAYS, CHECK_SEED))

# ── 1 to 4. Each exit route bounds its own episode ──────────────────────────

cat("-- each exit route bounds its own episode --\n")

shipped <- measure_holding(NULL)
entered <- sum(!is.na(shipped$attributes_wide$r2b_hold_start))
report(nrow(shipped$episodes) == entered && entered > 0,
       "all %d casualties who entered holding are represented, none dropped", entered)

rtd <- shipped$episodes %>% filter(is.na(r2b_hold_served), !is.na(return_day))
report(nrow(rtd) > 0 && isTRUE(all.equal(rtd$hold_end_min, as.numeric(rtd$return_day))),
       "the %d episodes ended by return to duty end at return_day", nrow(rtd))

open <- shipped$episodes %>% filter(is.na(r2b_hold_served), is.na(return_day))
report(nrow(open) > 0 && all(open$hold_end_min == shipped$window),
       "the %d episodes still running at the window's close run to it, rather than being dropped",
       nrow(open))

# ── 5 and 6. The reconstruction agrees with the beds themselves ─────────────

cat("\n-- the reconstruction agrees with the resource monitor --\n")

for (arm in list(list(threshold = NULL, label = "shipped, no threshold"),
                 list(threshold = HOLD_THRESHOLD_MIN,
                      label = sprintf("evac_threshold %d min", HOLD_THRESHOLD_MIN)))) {
  m <- if (is.null(arm$threshold)) shipped else measure_holding(arm$threshold)
  recon <- episode_bed_days(m$episodes, m$window)
  gap   <- abs(recon - m$monitor_bed_days)
  within <- m$monitor_bed_days > 0 && gap <= BED_DAY_TOLERANCE * m$monitor_bed_days
  report(within,
         "%s: %.1f reconstructed bed-days against %.1f counted at the beds, %.1f%% apart",
         arm$label, recon, m$monitor_bed_days, 100 * gap / max(m$monitor_bed_days, 1))

  if (!is.null(arm$threshold)) {
    evac <- m$episodes %>% filter(!is.na(r2b_hold_served))
    served_end <- as.numeric(evac$r2b_hold_start) + as.numeric(evac$r2b_hold_served)
    report(nrow(evac) > 0 && isTRUE(all.equal(evac$hold_end_min, served_end)),
           "the %d episodes ended by onward evacuation end at the bed time served", nrow(evac))
    carried <- evac %>% filter(!is.na(return_day))
    report(nrow(carried) > 0 &&
             all(as.numeric(carried$return_day) > carried$hold_end_min),
           "the %d of those returning to duty elsewhere are not charged to that later day",
           nrow(carried))
  }
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All holding episode reconstruction checks passed.\n")
quit(status = 0)
