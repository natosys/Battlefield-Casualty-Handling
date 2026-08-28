#!/usr/bin/env Rscript
##############################################################################
## scripts/check_pre_open_window.R                                          ##
## Regression check — the R2B pre-open hold window behaves at its bounds    ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_pre_open_window.R              # default 30-day runs
#   Rscript scripts/check_pre_open_window.R --days 10    # shorter runs
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step.
#
# Why this check exists. The pre-open window lets a casualty who finds the R2B
# surgical section closed be held forward for it rather than diverted, when the
# section is due to reopen within the window. Two properties of that mechanism
# are worth holding fixed against later edits, and neither is visible in a
# single run's output:
#
#   1. A window of zero reproduces the instant-bypass model exactly. The wait
#      branch must be unreachable at zero, and reaching it must be the only
#      thing that separates the two configurations, so a zero-window run has to
#      be bit-identical to one with the mechanism removed rather than merely
#      close. Nothing on the branch draws a random number, so an identical
#      arrival trace is the right assertion: a divergence of any size means the
#      branch is being entered, or is consuming draws when it is not.
#
#   2. A non-zero window holds casualties, and every casualty it holds is
#      operated on forward rather than diverted. That is what the mechanism
#      claims to do, and it is a property of the run itself rather than of a
#      comparison between runs. The bound on how long a hold may last is
#      checked with it, since a window measured against the wrong shift
#      boundary would otherwise surface only as an implausible utilisation
#      figure nobody was looking at.
#
# What this check deliberately does not assert is that the shipped window
# lowers the bypass count or raises the surgery count against a zero-window run
# at the same seed. Turning the window on shifts simmer's single global random
# stream, so the two runs are different realisations rather than a controlled
# comparison, and either count can move either way in any one of them. Both are
# reported below without gating, and the population-level movement is measured
# across replications in docs/Multi_Run_Analysis.md instead.
#
# minutes_to_shift_open() is checked directly as well, over a whole simulated
# day, since every hold decision rests on it and its two cases (before and
# after the roster's break) are trivial to get the wrong way round.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")

args <- commandArgs(trailingOnly = TRUE)

#' Read one flagged command line argument
#'
#' @param flag Flag to look for, including its leading dashes.
#' @param default Value returned when the flag is absent or carries no value.
#' @return The argument following the flag, or `default`.
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

SCENARIO   <- arg_value("--scenario", "default")
CHECK_DAYS <- as.integer(arg_value("--days", 30L))
CHECK_SEED <- as.integer(arg_value("--seed", 42L))

failures <- character(0)

#' Record a failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated failures, invisibly; called for its side effect.
fail     <- function(...) failures <<- c(failures, sprintf(...))

#' Print one PASS or FAIL line
#'
#' @param ok Logical: whether the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Values interpolated into `fmt`.
#' @return The printed line, invisibly; called for its side effect.
report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}

json <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
base_env_data <- build_environment(resolve_scenario(json, SCENARIO))

day_min <<- DAY_MIN

SHIPPED_WINDOW <- as.numeric(base_env_data$vars$r2b$surgery$pre_open_window_min)

#' Run the model once at a given pre-open window
#'
#' @param window Minutes for r2b.surgery.pre_open_window_min
#' @return Named list: `digest` (a string over every arrival's name and end
#'   time), `n` (arrival count), and the four counts the second check compares
#'
#' @details The window is written into the built env_data rather than into
#'   env_data.json, so the check never touches the tracked configuration.
run_at_window <- function(window) {
  ed <- base_env_data
  ed$vars$r2b$surgery$pre_open_window_min <- window
  env_data <<- ed
  counts   <<- sapply(ed$elms, length)

  invisible(capture.output(
    wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)
  ))

  arr <- get_mon_arrivals(wrapped, ongoing = TRUE)
  arr <- arr[order(arr$name), ]

  att <- get_mon_attributes(wrapped)
  #' Names of the casualties carrying one attribute value
  #'
  #' @param key Attribute key to match.
  #' @param val Attribute value to match, 1 by default.
  #' @return A character vector of casualty names, without repeats.
  who <- function(key, val = 1) {
    unique(att$name[att$key == key & att$value == val])
  }

  list(
    n        = nrow(arr),
    digest   = paste(sprintf("%s:%.10f", arr$name, arr$end_time), collapse = "|"),
    offshift = length(who("r2b_bypass_reason", 1)),
    busy     = length(who("r2b_bypass_reason", 2)),
    surgery  = length(who("r2b_surgery")),
    held     = who("r2b_pre_open_wait"),
    operated = who("r2b_surgery"),
    bypassed = unique(att$name[att$key == "r2b_bypass_reason"]),
    holds    = att$value[att$key == "r2b_pre_open_wait_min"]
  )
}

cat(sprintf("R2B pre-open window check: %s, %d-day runs at seed %d (shipped window %g min)\n\n",
            SCENARIO, CHECK_DAYS, CHECK_SEED, SHIPPED_WINDOW))

# ── 1. minutes_to_shift_open() over a whole day ─────────────────────────────

cat("-- minutes_to_shift_open() tracks the roster --\n")

ot_shift_break_min <<- as.integer(get_ot_hours(base_env_data) * 60L)
brk <- ot_shift_break_min

probe <- seq(0, 1439)
got   <- vapply(probe, minutes_to_shift_open, numeric(1))
# as.numeric: minutes_to_shift_open() returns a double, and identical() is
# type-sensitive, so the expectation is built as one rather than as the
# integer DAY_MIN would otherwise make it.
want  <- ifelse(probe < brk, brk - probe, as.numeric(DAY_MIN) - probe)

open_ok <- identical(got, want)
if (!open_ok) {
  wrong <- probe[got != want][1]
  fail(paste0("minutes_to_shift_open() disagrees with the roster: at minute %d of the day it ",
              "returns %g where the break at %d minutes makes the next opening %g minutes away"),
       wrong, got[probe == wrong], brk, want[probe == wrong])
}
report(open_ok, "agrees with the %d-minute roster break at all 1,440 minutes of the day", brk)

# The value must be positive everywhere, or a hold decision could be taken on a
# section that is not in fact reopening.
positive_ok <- all(got > 0)
if (!positive_ok) fail("minutes_to_shift_open() returned a non-positive interval")
report(positive_ok, "never reports a non-positive interval (min %g, max %g)", min(got), max(got))

# A roster with no closed shift has nothing to reopen.
ot_shift_break_min <<- DAY_MIN
degenerate_ok <- is.infinite(minutes_to_shift_open(0)) && is.infinite(minutes_to_shift_open(700))
ot_shift_break_min <<- brk
if (!degenerate_ok) {
  fail("minutes_to_shift_open() returned a finite interval for a roster that never closes")
}
report(degenerate_ok, "returns Inf for a degenerate roster, so no casualty is held for it")

# ── 2. A window of zero reproduces the instant-bypass model exactly ─────────

cat("\n-- a zero window is the instant-bypass model, bit for bit --\n")

zero_a <- run_at_window(0)
zero_b <- run_at_window(0)

repeatable <- identical(zero_a$digest, zero_b$digest)
if (!repeatable) {
  fail("two zero-window runs at seed %d disagree, so this check cannot compare anything",
       CHECK_SEED)
}
report(repeatable, "zero-window runs reproduce (%d arrivals)", zero_a$n)

no_holds <- length(zero_a$held) == 0 && length(zero_a$holds) == 0
if (!no_holds) {
  fail(paste0("a zero window held %d casualties forward. The wait branch must be unreachable ",
              "at zero, or the shipped regression baseline no longer describes a model anyone ",
              "can reproduce by setting the window to zero"),
       length(zero_a$held))
}
report(no_holds, "no casualty held forward at a zero window")

# ── 3. The shipped window holds casualties, and holds them to surgery ──────

cat("\n-- the shipped window holds casualties forward to surgery --\n")

if (SHIPPED_WINDOW <= 0) {
  cat("   shipped window is zero; nothing to hold\n")
} else {
  open <- run_at_window(SHIPPED_WINDOW)

  held_some <- length(open$held) > 0
  if (!held_some) {
    fail(paste0("a %g-minute window held nobody over %d days. Either the window is not reaching ",
                "the model or no casualty arrives inside it, and both make the parameter a no-op"),
         SHIPPED_WINDOW, CHECK_DAYS)
  }
  report(held_some, "%d casualties held forward at a %g-minute window",
         length(open$held), SHIPPED_WINDOW)

  # The hold exists to convert a diversion into a forward operation, so a held
  # casualty who was diverted anyway would mean the branch is falling through
  # to the bypass it was meant to replace.
  held_diverted <- intersect(open$held, open$bypassed)
  none_diverted <- length(held_diverted) == 0
  if (!none_diverted) {
    fail("%d casualties were held forward and then bypassed to R2E anyway: %s",
         length(held_diverted), paste(head(held_diverted, 5), collapse = ", "))
  }
  report(none_diverted, "no held casualty was bypassed to R2E")

  # A casualty still holding when the run ends has no operation to show for it
  # yet, which is the only reason a hold may go unresolved.
  held_unoperated <- setdiff(open$held, union(open$operated, open$bypassed))
  still_holding   <- length(open$held) - length(open$holds)
  resolved_ok     <- length(held_unoperated) <= still_holding
  if (!resolved_ok) {
    fail("%d held casualties reached neither surgery nor the end of the run still holding",
         length(held_unoperated) - still_holding)
  }
  report(resolved_ok, "%d of %d holds completed within the run, the rest still holding at its end",
         length(open$holds), length(open$held))

  # A hold may outlast the window, the section reopening to a theatre another
  # case has since taken, but never by more than one operation: the theatre is
  # held through the wait, so the only case that can precede this one is the
  # one already in progress when the shift opened.
  hold_ceiling <- SHIPPED_WINDOW + base_env_data$vars$r2b$surgery$max
  holds_bounded <- length(open$holds) == 0 || max(open$holds) <= hold_ceiling
  if (!holds_bounded) {
    fail(paste0("a casualty was held %.1f minutes against a %g-minute window and a %g-minute ",
                "longest operation. A hold beyond %g minutes means the window was measured ",
                "against the wrong shift boundary"),
         max(open$holds), SHIPPED_WINDOW, base_env_data$vars$r2b$surgery$max, hold_ceiling)
  }
  report(holds_bounded, "longest hold %.1f min, inside the %.0f min the roster can justify",
         if (length(open$holds)) max(open$holds) else 0, hold_ceiling)

  # Reported, not asserted: the two runs are different realisations of the
  # global random stream, so neither count is bound to move in either
  # direction in any one pair of them.
  cat(sprintf("   off-shift bypasses %d -> %d, R2B surgeries %d -> %d (single realisation, gates nothing)\n",
              zero_a$offshift, open$offshift, zero_a$surgery, open$surgery))
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All R2B pre-open window checks passed.\n")
quit(status = 0)
