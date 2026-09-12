#!/usr/bin/env Rscript
##############################################################################
## scripts/check_icu_gate_switch.R                                          ##
## Regression check — the intensive care gate disable switch reproduces the ##
## pre-gate model                                                           ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_icu_gate_switch.R
#   Rscript scripts/check_icu_gate_switch.R --days 10 --seed 42
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The published before-and-after comparison of the
# post-operative intensive care gate compares the current model against a
# configuration that no longer exists in the codebase, so it could not be
# re-run and its intervals have been carried forward with a caveat instead
# (Issue #296). `icu_gating.enabled` reconstructs the earlier arm as a
# supported configuration rather than as a historical code state, and this
# check is what keeps it faithful to what it claims to reproduce.
#
# Faithful means two separable things, and only the first is a property of one
# run.
#
#   1. With the gate disabled, nothing is diverted or deferred by intensive
#      care being full. No casualty sets `surgery_deferred`, and no operated
#      casualty takes the degraded holding-bed recovery that a Priority 1
#      casualty takes when a bed is not free. Every operated casualty reaches
#      an intensive care bed, queueing for one rather than going without.
#
#   2. With the gate enabled, which is what ships, both of those pathways are
#      reachable. An assertion that they are empty when disabled means nothing
#      unless they are non-empty when enabled, so the check would otherwise
#      pass against a model that had lost the gate entirely.
#
# The shipped value is asserted to be the enabled one, so the comparison's
# baseline arm is the model as published rather than whatever the field was
# last left at.
#
# What this check deliberately does not assert is that disabling the gate moves
# any outcome in a particular direction. Turning it off changes which
# activities consume random draws, so the two runs are different realisations
# rather than a controlled comparison; the population-level effect is measured
# across independent replications and reported in docs/Multi_Run_Analysis.md.

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
#' @details Long enough that intensive care saturates and the gated pathways
#'   are exercised, which assertion 2 confirms rather than assumes.
CHECK_DAYS <- as.integer(arg_value("--days", 30L))

#' Seed for the live runs
CHECK_SEED <- as.integer(arg_value("--seed", 42L))

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

#' Forward intensive care share that makes the R2B gate a real constraint
#'
#' @details At the shipped share of zero the two intensive care beds per R2B
#'   team serve only the evacuation-wait fallback, so that gate almost never
#'   fires and a run at the shipped configuration cannot tell whether its
#'   switch is honoured. A non-zero share sends every casualty operated on
#'   forward into one of those beds, which is the regime the gate exists for.
R2B_GATE_SHARE <- 0.8

#' Run the model with the gate in a given state and return the wide attributes
#'
#' @param enabled 1 to leave the gate in force, 0 to disable it at both
#'   echelons.
#' @param r2b_share Forward intensive care share, or NULL to leave it shipped.
#' @return One row per casualty, attributes pivoted to columns.
measure_gate <- function(enabled, r2b_share = NULL) {
  ed <- load_scenario("env_data.json", "default")
  ed$vars$r2b$icu_gating$enabled       <- enabled
  ed$vars$r2eheavy$icu_gating$enabled  <- enabled
  if (!is.null(r2b_share)) ed$vars$r2b$post_op_icu$share <- r2b_share
  # Assigned explicitly into the global environment the model reads them from,
  # rather than with <<- from inside this function, which reads the same but
  # states the target less clearly in a script.
  assign("env_data", ed, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(ed$elms, length), envir = globalenv())

  set.seed(CHECK_SEED)
  invisible(capture.output(wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)))
  build_attributes_wide(get_mon_attributes(list(wrapped)),
                        get_mon_arrivals(list(wrapped), ongoing = TRUE))
}

#' Count casualties whose recovery took the degraded holding-bed pathway
#'
#' @param wide One row per casualty, attributes pivoted to columns.
#' @return The count.
#' @details `post_op_pathway` records which recovery an operated casualty
#'   took, 1 for the intensive care bed and 2 for the degraded holding bed a
#'   Priority 1 casualty is diverted to when no intensive care bed is free.
hold_diversions <- function(wide) {
  if (!"post_op_pathway" %in% names(wide)) return(NA_integer_)
  sum(as.numeric(wide$post_op_pathway) == 2, na.rm = TRUE)
}

#' Count casualties whose theatre entry was deferred
#'
#' @param wide One row per casualty, attributes pivoted to columns.
#' @return The count.
deferrals <- function(wide) {
  if (!"surgery_deferred" %in% names(wide)) return(0L)
  sum(!is.na(wide$surgery_deferred))
}

cat(sprintf("Intensive care gate switch check: %d-day runs, seed %d\n\n",
            CHECK_DAYS, CHECK_SEED))

# ── 1. The shipped configuration leaves the gate in force ──────────────────

cat("-- the shipped configuration --\n")

shipped <- load_scenario("env_data.json", "default")
for (echelon in c("r2b", "r2eheavy")) {
  value <- shipped$vars[[echelon]]$icu_gating$enabled
  report(!is.null(value) && !is.na(value) && value == 1,
         "%s ships with the gate enabled, so the comparison's baseline arm is the published model",
         echelon)
}

# ── 2. Disabled, nothing is diverted or deferred ───────────────────────────

cat("\n-- gate disabled: the pre-gate model --\n")

off <- measure_gate(0)
operated_off <- sum(!is.na(as.numeric(off$r2e_surgery_1_start)))

report(deferrals(off) == 0,
       "no casualty is deferred with the gate disabled (%d operated at R2E)", operated_off)
report(hold_diversions(off) == 0,
       "no operated casualty takes the degraded holding-bed recovery with the gate disabled")

# ── 3. Enabled, both pathways are reachable ────────────────────────────────

cat("\n-- gate enabled: the pathways assertion 2 asserts empty are reachable --\n")

on <- measure_gate(1)
operated_on <- sum(!is.na(as.numeric(on$r2e_surgery_1_start)))

report(deferrals(on) > 0,
       "the gate defers %d casualties when in force, so the deferral assertion is not vacuous",
       deferrals(on))
report(!is.na(hold_diversions(on)) && !is.na(hold_diversions(off)),
       "post_op_pathway is recorded in both runs, so the diversion assertion reads a real column")
report(isTRUE(hold_diversions(on) > 0),
       "the gate diverts %s operated casualties to the degraded holding-bed recovery when in force",
       format(hold_diversions(on)))
report(operated_on > 0 && operated_off > 0,
       "both configurations operate on casualties (%d enabled, %d disabled)",
       operated_on, operated_off)

# ── 4. The R2B gate's own switch is honoured ───────────────────────────────

# Asserted at a non-zero forward intensive care share rather than at the
# shipped one. At the shipped share of zero the R2B gate is close to inert, so
# a run there passes whether or not its switch is read, and an earlier version
# of this check did exactly that.

cat("\n-- the R2B gate, at a share that makes it bite --\n")

r2b_on  <- measure_gate(1, R2B_GATE_SHARE)
r2b_off <- measure_gate(0, R2B_GATE_SHARE)

report(deferrals(r2b_on) > 0,
       "at a forward share of %.1f the gates defer %d casualties, so the arm is not vacuous",
       R2B_GATE_SHARE, deferrals(r2b_on))
report(deferrals(r2b_off) == 0,
       "no casualty is deferred at that share with the gates disabled")

# ── 5. The switch is validated at the boundary ─────────────────────────────

cat("\n-- a malformed value is rejected by name --\n")

env_data <- shipped
for (bad in list(NULL, NA, 2, "yes")) {
  env_data$vars$r2eheavy$icu_gating$enabled <- bad
  assign("env_data", env_data, envir = globalenv())
  err <- tryCatch(
    {
      invisible(icu_gate_enabled("r2eheavy"))
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  report(!is.null(err) && grepl("icu_gating.enabled", err, fixed = TRUE),
         "a value of %s is rejected with a message naming the field",
         paste(format(bad), collapse = ", "))
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All intensive care gate switch checks passed.\n")
quit(status = 0)
