#!/usr/bin/env Rscript
##############################################################################
## scripts/check_ame_backlog_exit.R                                         ##
## Regression check — the strategic evacuation backlog is bounded by the    ##
## attribute its own exit route sets                                        ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_ame_backlog_exit.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. compute_ame_backlog() reconstructs how many casualties
# are simultaneously awaiting a strategic evacuation sortie, the resource
# monitor's own queue column being structurally zero for the two airlift pools
# (see that function's roxygen for why). It added a casualty at the evacuation
# decision and removed it at ame_departure_time, which is the right pair for
# only one of the two ways a casualty stops waiting.
#
# A casualty who dies during r2e_ame_dow_poll() leaves the queue without ever
# boarding and so sets no departure time. Removing casualties on the departure
# alone left every such casualty in the backlog for the whole remainder of the
# campaign, which is the same defect, in the same direction, that
# scripts/check_holding_occupancy_split.R records for the holding pool: a
# recorded interval outrunning the thing it describes, never the reverse. The
# backlog is a published figure and a screened sensitivity response, so an
# error in it reads as a deeper queue than the model produced.
#
# What it asserts:
#
#   1. On a constructed monitor whose answer is computable by hand, the
#      reconstruction is exactly right for all three exits: a casualty that
#      boards, one that dies waiting, and one still waiting when the window
#      closes.
#   2. On that same monitor, the superseded departure-bounded reconstruction
#      disagrees, and disagrees by leaving the casualty that died in the queue.
#      Without this the first assertion would pass for a reconstruction that
#      never had the defect and for one that still does, since both agree on
#      the two exits that set a departure time.
#   3. A casualty still queued for the staging bed it would wait in, which sets
#      neither attribute, is carried as waiting rather than dropped.
#   3b. The level carried to the window's close is the one in force after every
#      event of the campaign's last instant, not after the first of them. A
#      sortie that clears the queue it has just received leaves an arrival and
#      a departure at one instant, and carrying the intermediate level reads
#      one casualty high for the rest of the series.
#   4. On a real campaign the case is reachable: a run at a cancellation rate
#      that loses sorties carries a casualty who died awaiting one, and its
#      backlog returns to the level the superseded reconstruction could not
#      reach, by exactly the count of such casualties.
#
# Assertion 4 is what keeps the rest from resting on a monitor this check wrote
# itself.

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
#' @details Anything other than TRUE is a failure, NA included, so a quantity
#'   that becomes NA because the code under test is wrong is reported rather
#'   than raising an error that stops the run at the first such assertion.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' Campaign length the constructed monitor and the real run both cover, in days
CHECK_DAYS <- 30L

#' Control seed the real run is made under
CHECK_SEED <- 42L

#' Scenario profile the real run uses
CHECK_SCENARIO <- "moderate_intensity"

#' Sortie cancellation probability the real run is made at
#'
#' @details High enough that sorties are lost and casualties wait long enough
#'   to be polled for died-of-wounds risk, which is the only way the exit this
#'   check exists for is reached at all.
CHECK_FAILURE <- 0.75

#' Label compute_ame_backlog() gives the standard airlift pool
STANDARD_POOL <- "Standard (Hold, CSU)"

# ── 1-3. A constructed monitor whose answer is computable by hand ────────────

cat("\n-- every exit from the wait is counted, on a constructed monitor --\n")

#' One casualty's rows of a long-format attribute monitor
#'
#' @param name Casualty name.
#' @param decision Instant the evacuation decision was taken, in minutes.
#' @param hold_end Instant the wait ended, or NA where it had not.
#' @param departure Instant the casualty boarded, or NA where it never did.
#' @return A data frame of monitor rows for that casualty.
#'
#' @details Written in the monitor's own shape rather than by running the
#'   model, so the backlog this check expects is arithmetic rather than a
#'   second measurement of the thing under test.
casualty_rows <- function(name, decision, hold_end, departure) {
  keys <- c("r2e_departure_time", "ame_route", "ame_hold_end", "ame_departure_time")
  vals <- c(decision, 2, hold_end, departure)
  keep <- !is.na(vals)
  data.frame(name = name, replication = 1L, key = keys[keep], value = vals[keep],
             time = vals[keep], stringsAsFactors = FALSE)
}

# Three casualties, one per exit. All decide at t = 0 so the backlog opens at
# three, and each leaves by a different route:
#   boarder  boards at day 5, setting both attributes
#   dier     dies at day 10, setting the release and no departure
#   waiter   is still waiting at the close, setting neither
# The backlog is therefore 3 from day 0, 2 from day 5, 1 from day 10, and 1 at
# the close.
monitor <- rbind(
  casualty_rows("boarder", 0, 5 * DAY_MIN, 5 * DAY_MIN),
  casualty_rows("dier",    0, 10 * DAY_MIN, NA),
  casualty_rows("waiter",  0, NA, NA)
)

backlog <- compute_ame_backlog(monitor, CHECK_DAYS)
standard <- backlog[as.character(backlog$pool) == STANDARD_POOL, ]
standard <- standard[order(standard$time), ]

#' The backlog the reconstruction reports at one instant
#'
#' @param at Instant to read, in minutes.
#' @return The backlog in force at that instant, or NA where the series does
#'   not reach it.
#'
#' @details Read as a step function, taking the last event at or before the
#'   instant, which is how geom_step() and the sensitivity module's
#'   time-weighted mean both read it.
level_at <- function(at) {
  rows <- standard[standard$time <= at + 1e-9, ]
  if (nrow(rows) == 0) return(NA_integer_)
  rows$backlog[nrow(rows)]
}

expected <- c(3, 3, 2, 2, 1, 1)
at       <- c(0, 4, 5, 9, 10, CHECK_DAYS) * DAY_MIN
for (i in seq_along(at)) {
  report(isTRUE(level_at(at[i]) == expected[i]),
         "at day %g the backlog is %d, as the three exits require (found %s)",
         at[i] / DAY_MIN, expected[i], format(level_at(at[i])))
}

report(isTRUE(level_at(CHECK_DAYS * DAY_MIN) == 1),
       "the casualty still waiting at the close is carried, not dropped")

# The campaign's last two events fall at one instant: a casualty decides at day
# 20 and a sortie clears the pool at day 20, both it and the one still waiting
# boarding then. The level in force from that instant is zero, and a
# reconstruction reading the first of the two events instead reports one.
coincident <- rbind(
  casualty_rows("early",  0, 20 * DAY_MIN, 20 * DAY_MIN),
  casualty_rows("late",   20 * DAY_MIN, 20 * DAY_MIN, 20 * DAY_MIN)
)
tail_backlog <- compute_ame_backlog(coincident, CHECK_DAYS)
tail_standard <- tail_backlog[as.character(tail_backlog$pool) == STANDARD_POOL, ]
tail_standard <- tail_standard[order(tail_standard$time), ]
report(isTRUE(tail_standard$backlog[nrow(tail_standard)] == 0),
       "a pool cleared at the campaign's last instant reaches the close at 0, not 1 (found %s)",
       format(tail_standard$backlog[nrow(tail_standard)]))
report(isTRUE(tail_standard$time[nrow(tail_standard)] == CHECK_DAYS * DAY_MIN),
       "and the series is extended to the window's close rather than stopping at its last event")

#' The backlog under the superseded departure-bounded reconstruction
#'
#' @param at Instant to read, in minutes.
#' @return The backlog that reconstruction reports at that instant.
#'
#' @details Written here rather than called, so that the assertions above are
#'   held against the behaviour they replaced rather than only against the
#'   monitor. It removes a casualty on its departure alone, so the one that
#'   died stays queued for the rest of the campaign.
superseded <- function(at) {
  decided  <- 3
  departed <- sum(c(5 * DAY_MIN) <= at + 1e-9)
  decided - departed
}
report(superseded(CHECK_DAYS * DAY_MIN) == 2,
       "the superseded reconstruction ends the campaign at 2, one casualty too many")
report(superseded(10 * DAY_MIN) - level_at(10 * DAY_MIN) == 1,
       "and the difference is exactly the casualty that died, from the instant it did")

# ── 4. The exit is reachable in a real campaign ──────────────────────────────

cat("\n-- and the exit is reachable in a campaign the model produced --\n")

config_snapshot <- capture_config_globals()
on.exit(restore_config_globals(config_snapshot), add = TRUE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
apply_airlift_setting(json_data, CHECK_SCENARIO, "failure_probability", CHECK_FAILURE)

cat(sprintf("Running seed %d for %d days at a cancellation rate of %.2f\n",
            CHECK_SEED, CHECK_DAYS, CHECK_FAILURE))
set.seed(CHECK_SEED)
env <- run_once(CHECK_DAYS, seed = CHECK_SEED)

attributes <- simmer::get_mon_attributes(env)
arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
wide <- build_attributes_wide(attributes, arrivals) %>%
  dplyr::right_join(arrivals, by = c("name", "replication"), suffix = c("", "_arrival"))

died <- wide[!is.na(wide$dow_echelon) & wide$dow_echelon == 5 &
               !is.na(wide$ame_hold_start), ]
report(nrow(died) > 0,
       "the run carries a casualty who died awaiting a sortie, so this is not vacuous (%d)",
       nrow(died))
report(all(is.na(died$ame_departure_time)),
       "none of them boards, which is why the departure alone cannot end their wait")

run_backlog <- compute_ame_backlog(attributes, CHECK_DAYS)
report(nrow(run_backlog) > 0, "the run produced a backlog series (%d events)",
       nrow(run_backlog))

# The two reconstructions differ by the casualties that died, and by nothing
# else: every other casualty releases its bed at the instant it boards, so the
# release and the departure are the same event for them.
close <- CHECK_DAYS * DAY_MIN
corrected <- run_backlog %>%
  group_by(pool) %>%
  filter(time <= close) %>%
  filter(time == max(time)) %>%
  slice(1) %>%
  ungroup()
decided <- wide[!is.na(wide$r2e_evac) & wide$r2e_evac == 1 & !is.na(wide$ame_route), ]
boarded <- decided[!is.na(decided$ame_departure_time), ]
old_total <- nrow(decided) - nrow(boarded)
report(abs(sum(corrected$backlog) - (old_total - nrow(died))) < 1e-9,
       "the backlog at the close is %d, the superseded %d, differing by the %d who died",
       sum(corrected$backlog), old_total, nrow(died))
report(all(run_backlog$backlog >= 0),
       "the backlog is never negative, so no casualty leaves the queue twice")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All strategic evacuation backlog exit checks passed.\n")
quit(status = 0)
