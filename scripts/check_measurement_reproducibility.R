#!/usr/bin/env Rscript
##############################################################################
## scripts/check_measurement_reproducibility.R                              ##
## Regression check — a measurement is a function of its control seed       ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_measurement_reproducibility.R            # 4 checks
#   Rscript scripts/check_measurement_reproducibility.R --days 5   # shorter
#   Rscript scripts/check_measurement_reproducibility.R --reps 4
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step. At the defaults it runs four short measurements
# and finishes in a few minutes.
#
# Why this check exists: every multi-replication figure this project publishes
# is stated with the control seed it was measured at, which is only useful if
# that seed is the whole of what determines the measurement. It was not.
# run_replications() drew its per-replication seeds before setting
# RNGkind("L'Ecuyer-CMRG"), and RNGkind() persists for the rest of the session,
# so the first call in a session drew its seeds under Mersenne-Twister and
# every later call drew them under L'Ecuyer-CMRG. set.seed(777) gives a
# different stream under each, so a measurement was a function of its position
# in the session as well as of its seed (Issue #208). Measuring
# moderate_intensity on its own returned a treated-cohort died-of-wounds rate
# of 0.392%; measuring the same configuration at the same control seed with
# three default measurements ahead of it returned 0.248%, a gap wider than the
# interval either measurement reported.
#
# What it asserts:
#
#   1. Two measurements at one control seed, taken back to back, are identical.
#      This is the property the defect broke directly.
#
#   2. A measurement does not depend on what preceded it. A at seed 1, then B
#      at seed 2, then A again: the two A measurements must agree. This is what
#      makes `--scenario X` on its own agree with scenario X measured third in
#      a comparison, and it needs the stream position restored, not only the
#      generator kind.
#
#   3. run_replications() leaves the caller's generator kind and stream
#      position where it found them. This is the mechanism behind 1 and 2, and
#      checking it directly says which of the two failed when one does.
#
#   4. A replication's output is a function of its seed under the generator
#      run_replications() dispatches under. This is why the kind is now set on
#      the serial path as well as the parallel one: a single-replication
#      measurement, a Windows run and an mclapply batch would otherwise
#      disagree on the same seed.
#
# The complement to this check is scripts/check_replication_independence.R,
# which asserts that the replications within one measurement are independent
# of each other. That is a different property: replications can be perfectly
# independent within a measurement that is not reproducible, which is exactly
# the state this check was written to close.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")

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
CHECK_DAYS <- as.integer(arg_value("--days", 10L))
N_REPS     <- as.integer(arg_value("--reps", 6L))

CONTROL_SEEDS <- c(42L, 777L)

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

json     <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
env_data <<- build_environment(resolve_scenario(json, SCENARIO))
day_min  <<- DAY_MIN
counts   <<- sapply(env_data$elms, length)

#' A measurement's observable output, reduced to something comparable
#'
#' @param mon Monitoring list from run_replications()
#' @return A single string over every arrival's replication, name and end time
#'
#' @details Arrival identities and end times together cover both the arrival
#'   process and everything downstream of it that sets how long a casualty
#'   stays in the model, so two measurements agreeing on this agree on the
#'   whole of what either would report. Ongoing arrivals carry NA end times
#'   and are formatted as such rather than dropped, since which casualties are
#'   still in the system at the horizon is itself part of the output.
digest_measurement <- function(mon) {
  arr <- mon$arrivals[order(mon$arrivals$replication, mon$arrivals$name), ]
  paste(sprintf("%d:%s:%.10f", arr$replication, arr$name, arr$end_time),
        collapse = "|")
}

#' One measurement at a control seed, with its per-replication seeds
#'
#' @param seed Control seed set immediately before run_replications()
#' @return Named list: `seeds` (per-replication seed vector) and `digest`
measure <- function(seed) {
  set.seed(seed)
  invisible(capture.output(mon <- run_replications(N_REPS, CHECK_DAYS)))
  list(seeds = mon$seeds, digest = digest_measurement(mon))
}

cat(sprintf("Measurement reproducibility check: %s, %d replications x %d days\n\n",
            SCENARIO, N_REPS, CHECK_DAYS))

# ── 1. A measurement repeats at its control seed ────────────────────────────

cat("-- a measurement repeats at its control seed --\n")

a1 <- measure(CONTROL_SEEDS[1])
a2 <- measure(CONTROL_SEEDS[1])

repeats <- identical(a1$digest, a2$digest)
if (!repeats) {
  fail(paste0("two measurements at control seed %d gave different output. The seeds drawn ",
              "were %s and then %s. A published figure cannot be reproduced from the seed ",
              "it is stated at"),
       CONTROL_SEEDS[1],
       paste(head(a1$seeds, 3), collapse = ", "),
       paste(head(a2$seeds, 3), collapse = ", "))
}
report(repeats, "seed %d measured twice in a row gives identical output", CONTROL_SEEDS[1])

# ── 2. A measurement does not depend on what preceded it ────────────────────
#
# The intervening measurement at a second seed is the point of this: it is the
# stand-in for the two default measurements that precede moderate_intensity in
# the standard check_dow_calibration.R invocation, and for every earlier
# scenario in a comparison.

cat("\n-- a measurement does not depend on what preceded it --\n")

b1 <- measure(CONTROL_SEEDS[2])
a3 <- measure(CONTROL_SEEDS[1])

position_free <- identical(a1$digest, a3$digest)
if (!position_free) {
  fail(paste0("control seed %d gave different output when measured after an intervening ",
              "measurement at seed %d. A scenario measured on its own will not agree with ",
              "the same scenario measured later in a multi-scenario run"),
       CONTROL_SEEDS[1], CONTROL_SEEDS[2])
}
report(position_free,
       "seed %d reproduces exactly across an intervening measurement at seed %d",
       CONTROL_SEEDS[1], CONTROL_SEEDS[2])

seeds_reach <- !identical(a1$digest, b1$digest)
if (!seeds_reach) {
  fail("control seeds %d and %d gave identical measurements, so the control seed is not reaching the model",
       CONTROL_SEEDS[1], CONTROL_SEEDS[2])
}
report(seeds_reach, "control seeds %d and %d give different measurements",
       CONTROL_SEEDS[1], CONTROL_SEEDS[2])

# ── 3. The caller's RNG state is left where it was found ────────────────────

cat("\n-- run_replications() leaves the caller's RNG state alone --\n")

set.seed(CONTROL_SEEDS[1])
kind_before <- RNGkind()
seed_before <- get(".Random.seed", envir = globalenv())
draw_before <- {
  set.seed(CONTROL_SEEDS[1])
  sample.int(.Machine$integer.max, 1)
}

set.seed(CONTROL_SEEDS[1])
invisible(capture.output(run_replications(N_REPS, CHECK_DAYS)))
kind_after <- RNGkind()
seed_after <- get(".Random.seed", envir = globalenv())
draw_after <- sample.int(.Machine$integer.max, 1)

kind_kept <- identical(kind_before, kind_after)
if (!kind_kept) {
  fail(paste0("run_replications() left the generator kind as %s, having found it %s. The ",
              "next measurement in the session would draw its seeds from a different ",
              "generator and would not be reproducible from its control seed"),
       kind_after[1], kind_before[1])
}
report(kind_kept, "generator kind is still %s after the call", kind_before[1])

state_kept <- identical(seed_before, seed_after) && identical(draw_before, draw_after)
if (!state_kept) {
  fail(paste0("run_replications() advanced the caller's stream: the next draw gave %d where ",
              "%d was due. A measurement's result then depends on how many measurements ",
              "preceded it in the session"),
       draw_after, draw_before)
}
report(state_kept, "stream position is unchanged, so the next draw is still %d", draw_before)

# ── 4. A replication's output is a function of its seed ─────────────────────
#
# run_replications() dispatches under L'Ecuyer-CMRG on both paths, so a
# replication reproduces outside it only under that kind. Reproducing it here
# is what says a serial dispatch (one replication, or Windows) and an mclapply
# batch agree on the same seed.

cat("\n-- a replication reproduces from its seed alone --\n")

#' Digest of one replication's arrival stream
#'
#' @param mon Wrapped monitoring list from the replication framework.
#' @param r Replication index to digest.
#' @return A single string of every arrival's name and end time, ordered by
#'   name so two runs compare independent of dispatch order.
replication_digest <- function(mon, r) {
  arr <- mon$arrivals[mon$arrivals$replication == r, ]
  arr <- arr[order(arr$name), ]
  paste(sprintf("%s:%.10f", arr$name, arr$end_time), collapse = "|")
}

set.seed(CONTROL_SEEDS[1])
invisible(capture.output(batch <- run_replications(N_REPS, CHECK_DAYS)))

saved_kind <- RNGkind()
RNGkind("L'Ecuyer-CMRG")
invisible(capture.output(solo <- run_once(n_days = CHECK_DAYS, seed = batch$seeds[1])))
do.call(RNGkind, as.list(saved_kind))

solo_arr <- get_mon_arrivals(solo, ongoing = TRUE)
solo_arr <- solo_arr[order(solo_arr$name), ]
solo_digest <- paste(sprintf("%s:%.10f", solo_arr$name, solo_arr$end_time), collapse = "|")

path_free <- identical(replication_digest(batch, 1L), solo_digest)
if (!path_free) {
  fail(paste0("replication 1 of a batch and a standalone run at its seed (%d) gave different ",
              "output. A replication's result then depends on the dispatch path, so a ",
              "single-replication or Windows measurement disagrees with a parallel one"),
       batch$seeds[1])
}
report(path_free, "batch replication 1 and a standalone run at seed %d agree", batch$seeds[1])

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All measurement reproducibility checks passed.\n")
quit(status = 0)
