#!/usr/bin/env Rscript
##############################################################################
## scripts/check_definitive_repair_release.R                                ##
## Regression check — release to strategic evacuation with the definitive   ##
## repair outstanding                                                       ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_definitive_repair_release.R
#   Rscript scripts/check_definitive_repair_release.R --days 10 --seed 42
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. A damage control casualty operated on at Role 2E
# returns to that theatre for the definitive repair, and until now returned to
# it unconditionally: the model had no way to represent the casualty who is
# flown out with the abbreviated operation still the only one they have had.
# That is a real disposition, and the one the forward surgical establishment
# reaches when it saturates; representing it is what stops a policy sweep over
# forward capacity from being self-limiting.
#
# The release is a lever, not a behaviour change, and it ships disabled, so the
# check has three separable things to establish.
#
#   1. Disabled, the model is the one that was published. The decision reads a
#      queue and draws nothing, so a disabled threshold must leave the random
#      number stream exactly where the model without this branch left it; that
#      is asserted against a configuration with the block absent altogether,
#      which is the state the field was in before it existed.
#
#   2. In force, the release fires and is reachable. An assertion that nothing
#      is released when the lever is off means nothing unless something is
#      released when it is on.
#
#   3. A released casualty is the casualty the branch claims to release, and
#      leaves the echelon consistently. They are on the damage control pathway,
#      they were operated on at Role 2E, they never had the second procedure,
#      they served no post-definitive episode, and they were evacuated rather
#      than retained in theatre, a casualty whose definitive repair has not been
#      performed being one this echelon cannot return to duty. They also wait
#      for their flight in an intensive care bed on the critical airlift route,
#      whatever their triage priority: a casualty whose repair is outstanding is
#      not one a holding bed is for, and staging them in one would understate
#      intensive care demand and overstate holding availability at the exact
#      moment theatre is saturated.
#
#   4. The trigger measures the whole theatre queue. Theatre entry seizes a room
#      and then a section, so a casualty waiting for theatre is queued on one or
#      the other; counting sections alone would see only those already holding a
#      room, which the establishment caps at two, making any larger threshold
#      unreachable. The count is asserted against a monitor-independent recount
#      taken directly from the resources it sums.
#
# What this check deliberately does not assert is that the release improves or
# worsens any outcome. Enabling it changes which activities consume random
# draws, so the two arms are different realisations rather than a controlled
# comparison; the population effect belongs to replicated measurement.

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
#' @details Long enough that the surgical sections queue and the release is
#'   exercised, which assertion 2 confirms rather than assumes.
CHECK_DAYS <- as.integer(arg_value("--days", 30L))

#' Seed for the live runs
CHECK_SEED <- as.integer(arg_value("--seed", 42L))

#' Threshold that puts the release in force
#'
#' @details One queued casualty across the team's surgical sections. The
#'   sections are rostered in alternating shifts, so a queue forms readily and
#'   the arm is reached without contriving an establishment.
CHECK_THRESHOLD <- 1

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

#' Install a configuration into the globals the model reads
#'
#' @param ed The configuration list.
#' @return Invisible NULL.
#' @details Assigned explicitly into the global environment rather than with
#'   `<<-` from inside a function, which reads the same but states the target
#'   less clearly in a script.
install_config <- function(ed) {
  assign("env_data", ed, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(ed$elms, length), envir = globalenv())
  invisible(NULL)
}

#' Run the model at a given saturation threshold
#'
#' @param threshold The threshold to configure, or NULL to remove the
#'   `second_surgery` block altogether.
#' @return The wrapped environment `run_once()` returns.
run_at <- function(threshold) {
  ed <- load_scenario("env_data.json", "default")
  if (is.null(threshold)) {
    ed$vars$r2eheavy$second_surgery <- NULL
  } else {
    ed$vars$r2eheavy$second_surgery$saturation_queue_threshold <- threshold
  }
  install_config(ed)
  set.seed(CHECK_SEED)
  invisible(capture.output(wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)))
  wrapped
}

#' Pivot a run's attributes to one row per casualty
#'
#' @param wrapped The wrapped environment `run_at()` returned.
#' @return One row per casualty, attributes pivoted to columns.
wide_of <- function(wrapped) {
  build_attributes_wide(get_mon_attributes(list(wrapped)),
                        get_mon_arrivals(list(wrapped), ongoing = TRUE))
}

#' Count casualties carrying an outstanding definitive repair
#'
#' @param wide One row per casualty, attributes pivoted to columns.
#' @return The count.
released_count <- function(wide) {
  if (!"definitive_repair_outstanding" %in% names(wide)) return(0L)
  sum(as.numeric(wide$definitive_repair_outstanding) == 1, na.rm = TRUE)
}

cat(sprintf("Definitive repair release check: %d-day runs, seed %d\n\n",
            CHECK_DAYS, CHECK_SEED))

# ── 1. The shipped configuration disables the release ──────────────────────

cat("-- the shipped configuration --\n")

shipped <- load_scenario("env_data.json", "default")
shipped_threshold <- shipped$vars$r2eheavy$second_surgery$saturation_queue_threshold
report(!is.null(shipped_threshold) && length(shipped_threshold) == 1L &&
         !is.na(shipped_threshold) && shipped_threshold == 0,
       "r2eheavy.second_surgery.saturation_queue_threshold ships at 0, disabling the release")

install_config(shipped)
report(is.na(r2e_second_surgery_threshold()),
       "the accessor reads the shipped threshold as disabled")

# ── 2. A malformed threshold is rejected, naming the field ─────────────────

cat("\n-- malformed configuration --\n")

for (bad in list(-1, "many", c(1, 2), NA)) {
  ed <- load_scenario("env_data.json", "default")
  ed$vars$r2eheavy$second_surgery$saturation_queue_threshold <- bad
  install_config(ed)
  err <- tryCatch({
    r2e_second_surgery_threshold()
    NULL
  }, error = function(e) conditionMessage(e))
  report(!is.null(err) && grepl("saturation_queue_threshold", err, fixed = TRUE),
         "threshold %s is rejected with a message naming the field",
         paste(format(bad), collapse = "/"))
}

# ── 3. Disabled, the model is the one published ────────────────────────────

cat("\n-- disabled: the model without this branch --\n")

off     <- run_at(0)
absent  <- run_at(NULL)
#' A run's arrival monitor in a canonical row order
#'
#' @param wrapped The wrapped environment `run_at()` returned.
#' @return The arrival monitor, ordered by casualty name.
#' @details The monitor's own row order is the order arrivals completed, which
#'   two identical campaigns need not share; ordering by name compares the
#'   campaigns rather than the completion sequence.
arrivals_of <- function(wrapped) {
  arr <- get_mon_arrivals(list(wrapped), ongoing = TRUE)
  arr[order(arr$name), , drop = FALSE]
}

off_arr <- arrivals_of(off)
abs_arr <- arrivals_of(absent)

report(isTRUE(all.equal(off_arr, abs_arr, check.attributes = FALSE)),
       "a threshold of zero and no threshold at all produce the same campaign")

off_wide <- wide_of(off)
second_ops <- if ("r2e_surgery_2_start" %in% names(off_wide)) {
  sum(!is.na(as.numeric(off_wide$r2e_surgery_2_start)))
} else {
  0L
}
report(released_count(off_wide) == 0,
       "no casualty is released with the definitive repair outstanding while disabled")
report(second_ops > 0,
       paste("the second operation is reached while disabled (%d casualties),",
             "so the arm is not empty for want of casualties"),
       second_ops)

# ── 4. In force, the release fires ─────────────────────────────────────────

cat("\n-- in force: the release is reachable --\n")

on      <- run_at(CHECK_THRESHOLD)
on_wide <- wide_of(on)
n_released <- released_count(on_wide)

report(n_released > 0,
       paste("casualties are released with the definitive repair outstanding",
             "at a threshold of %s (%d released)"),
       format(CHECK_THRESHOLD), n_released)

# ── 5. A released casualty is the one the branch claims ────────────────────

cat("\n-- who is released, and how they leave --\n")

#' Read a wide-pivot column as numeric, tolerating its absence
#'
#' @param x The column, or NULL where the run set no such attribute.
#' @return The column as numeric, or a column of NA of the right length.
num <- function(x) if (is.null(x)) rep(NA_real_, nrow(on_wide)) else as.numeric(x)
released <- on_wide[which(num(on_wide$definitive_repair_outstanding) == 1), , drop = FALSE]

report(nrow(released) == n_released && all(num(released$dcs_pathway) == 1, na.rm = FALSE),
       "every released casualty is on the damage control pathway")
report(all(num(released$r2e_surgery) == 1),
       "every released casualty had their abbreviated operation at Role 2E")
report(all(is.na(num(released$r2e_surgery_2_start))),
       "no released casualty had the second procedure")
report(all(is.na(num(released$post_definitive_min))),
       paste("no released casualty served a post-definitive episode,",
             "there being no repair for one to follow"))
report(all(num(released$r2e_evac) == 1),
       "every released casualty was evacuated rather than retained in theatre")
report(all(num(released$ame_route) == 1),
       "every released casualty waits on the critical airlift route, whatever their priority")
report(all(num(released$ame_icu_hold) == 1),
       "every released casualty holds an intensive care bed pending flight, not a holding bed")
report(all(is.na(num(released$return_echelon)) | num(released$return_echelon) != 3),
       "no released casualty returned to duty from Role 2E")

# ── 6. The trigger measures the whole theatre queue ────────────────────────

cat("\n-- what the trigger counts --\n")

queue_src <- paste(readLines("R/trajectories.R", warn = FALSE), collapse = "\n")
report(grepl("waiting_for_room", queue_src, fixed = TRUE) &&
         grepl("waiting_for_staff", queue_src, fixed = TRUE),
       "the theatre queue sums the casualties waiting for a room and those waiting for a section")

# The count is exercised against a live environment rather than read off the
# source, so a helper that named both halves and summed only one would fail.
ed <- load_scenario("env_data.json", "default")
ed$vars$r2eheavy$second_surgery$saturation_queue_threshold <- CHECK_THRESHOLD
install_config(ed)
set.seed(CHECK_SEED)
invisible(capture.output(probe <- run_once(n_days = 2L, seed = CHECK_SEED)))
assign("env", probe, envir = globalenv())
ot_probe  <- env_data$elms$r2eheavy[[1]][["ot_bed"]]
sec_probe <- env_data$elms$r2eheavy[[1]][["surg"]]
recount <- sum(sapply(ot_probe, function(b) get_queue_count(probe, b))) +
  sum(sapply(sec_probe, function(m) sum(sapply(m, function(r) get_queue_count(probe, r)))))
report(identical(as.numeric(r2e_theatre_queue(1)), as.numeric(recount)),
       "the theatre queue agrees with an independent recount over the same resources")
report(length(sec_probe) > length(ot_probe),
       paste("the establishment fields more sections (%d) than theatre beds (%d),",
             "so a section-only count would cap the measurable queue"),
       length(sec_probe), length(ot_probe))

# ── 7. The attribute is registered ─────────────────────────────────────────

cat("\n-- the attribute registry --\n")

report("definitive_repair_outstanding" %in% MODEL_ATTRIBUTE_KEYS,
       "definitive_repair_outstanding is listed in MODEL_ATTRIBUTE_KEYS")
report("definitive_repair_outstanding" %in% names(off_wide),
       "the wide pivot carries the column even in a run that released nobody")

# ── Result ─────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (msg in state$failures) cat(sprintf("  - %s\n", msg))
  quit(status = 1)
}
cat("All checks passed.\n")
quit(status = 0)
