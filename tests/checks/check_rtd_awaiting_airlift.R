#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_rtd_awaiting_airlift.R                                ##
## Regression check — a convalescing evacuee returns to duty when their     ##
## recovery ends before a sortie boards them, and carries no risk waiting   ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_rtd_awaiting_airlift.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. A casualty evacuated because their recovery exceeds
# the theatre evacuation policy (evacuation_reason 1) has had their surgery and
# is convalescing, the state in which a casualty retained in theatre carries no
# died-of-wounds risk and returns to duty when fit. Before
# `r2eheavy.recovery.return_while_awaiting_airlift` such an evacuee waited for a
# seat however long the wait ran, at the post-operative intensive care death
# rate, long after their recovery had ended; at high intensity most of the
# critical-care queue was casualties who were fit for duty. Every part of the
# change is silent in an ordinary run, which still reports a plausible backlog
# and death count, so each property is asserted here:
#
#   1. With the switch on, casualties return to duty while waiting, and every
#      one of them was evacuated for recovery beyond the policy, never boarded,
#      did not die, released their staging placement at the moment they
#      returned, was recorded as an R2E return to duty, and returned when their
#      convalescence ended, counted from the disposition decision.
#   2. No such evacuee boarded a sortie after their recovery had ended.
#   3. No evacuee for recovery beyond the policy carries a death time for the
#      wait, while evacuees for reconstruction or with the repair outstanding
#      (reasons 2 and 3) do, and neither of those returns to duty.
#   4. With the switch off, no casualty returns to duty while waiting, and
#      operated wounded evacuees for recovery beyond the policy carry a death
#      time, the model as it stood before the switch.
#   5. The configuration validator rejects a switch that is neither 0 nor 1.
#
# The campaign is the high-intensity profile, where critical-care waits outrun
# a Priority 1 casualty's recovery within weeks, so the return is reached at
# any seed rather than by its luck.

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
#' @details Anything other than TRUE is a failure, NA included.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' Campaign length of each run, in days
CHECK_DAYS <- 150L

#' Control seed both runs are made under
CHECK_SEED <- 42L

#' Scenario profile both runs use
CHECK_SCENARIO <- "high_intensity"

#' Tolerance on a return-to-duty moment, in minutes
#'
#' @details The moment is computed from attributes the monitor stores as
#'   doubles, so it is compared to within a second rather than exactly.
CHECK_TOLERANCE_MIN <- 1 / 60

#' Run one campaign under the check's profile with the switch set
#'
#' @param on 1 or 0, the value of `return_while_awaiting_airlift`.
#' @return The wide attribute frame of the run, one row per casualty.
run_with_switch <- function(on) {
  json_data <- read_config("env_data.json")
  described <- build_environment(resolve_scenario(json_data, CHECK_SCENARIO))
  described$vars$r2eheavy$recovery$return_while_awaiting_airlift <- on
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  wrapped <- run_once(CHECK_DAYS, seed = CHECK_SEED)
  attributes <- simmer::get_mon_attributes(wrapped)
  arrivals <- simmer::get_mon_arrivals(wrapped)
  attributes$replication <- 1L
  arrivals$replication <- 1L
  as.data.frame(build_attributes_wide(attributes, arrivals))
}

#' Whether a column of the wide frame equals a value, NA reading as FALSE
#'
#' @param x Column.
#' @param v Value.
#' @return Logical vector.
is_val <- function(x, v) !is.na(x) & x == v

# ── 1-3. The switch on ───────────────────────────────────────────────────────

cat("-- with the switch on --\n")
on <- run_with_switch(1)
evacuated <- on[is_val(on$r2e_evac, 1), ]
returned <- evacuated[is_val(evacuated$ame_rtd, 1), ]

report(nrow(returned) > 0,
       "casualties return to duty while awaiting airlift (%d of %d evacuated)",
       nrow(returned), nrow(evacuated))
report(all(is_val(returned$evacuation_reason, 1)),
       "every one was evacuated for recovery beyond the policy")
report(all(is.na(returned$ame_departure_time)), "none of them boarded a sortie")
report(!any(is_val(returned$dow, 1)), "none of them died of wounds")
report(all(is_val(returned$return_echelon, 3)) &&
         isTRUE(all.equal(returned$return_day, returned$ame_hold_end)),
       "each released their staging placement at the return, recorded as an R2E return")

served <- ifelse(is.na(returned$r2b_hold_served), 0, returned$r2b_hold_served)
waited <- ifelse(is.na(returned$r2b_hold_waited), 0, returned$r2b_hold_waited)
due <- returned$r2e_departure_time +
  pmax(0, returned$recovery_to_duty_days * DAY_MIN - served - waited)
# A convalescence that ended during pre-flight care is acted on at staging, the
# first moment the casualty is in the seat queue's branch.
at_due <- abs(returned$return_day - due) <= CHECK_TOLERANCE_MIN
at_staging <- returned$return_day > due &
  abs(returned$return_day - returned$ame_stage_start) <= CHECK_TOLERANCE_MIN
report(all(at_due | at_staging),
       "each returned when their convalescence ended, or on staging if it ended before (%d, %d)",
       sum(at_due), sum(at_staging & !at_due))

reason1 <- evacuated[is_val(evacuated$evacuation_reason, 1), ]
boarded1 <- reason1[!is.na(reason1$ame_departure_time), ]
b_served <- ifelse(is.na(boarded1$r2b_hold_served), 0, boarded1$r2b_hold_served)
b_waited <- ifelse(is.na(boarded1$r2b_hold_waited), 0, boarded1$r2b_hold_waited)
b_due <- boarded1$r2e_departure_time +
  pmax(0, boarded1$recovery_to_duty_days * DAY_MIN - b_served - b_waited)
report(all(boarded1$ame_departure_time <= b_due + CHECK_TOLERANCE_MIN),
       "no evacuee for recovery beyond the policy boarded after their recovery ended (%d boarded)",
       nrow(boarded1))

report(all(is.na(reason1$ame_death_time)) && !any(is_val(reason1$dow_echelon, 5)),
       "no evacuee for recovery beyond the policy carries a death time for the wait")
surgical <- evacuated[is_val(evacuated$evacuation_reason, 2) |
                        is_val(evacuated$evacuation_reason, 3), ]
report(nrow(surgical) > 0 && any(!is.na(surgical$ame_death_time)),
       "evacuees for reconstruction or with the repair outstanding carry one (%d of %d)",
       sum(!is.na(surgical$ame_death_time)), nrow(surgical))
report(!any(is_val(surgical$ame_rtd, 1)), "and none of them returns to duty while waiting")

# ── 4. The switch off ────────────────────────────────────────────────────────

cat("\n-- with the switch off --\n")
off <- run_with_switch(0)
off_evacuated <- off[is_val(off$r2e_evac, 1), ]
report(!any(is_val(off_evacuated$ame_rtd, 1)), "no casualty returns to duty while waiting")
off_reason1 <- off_evacuated[is_val(off_evacuated$evacuation_reason, 1), ]
# Wounded only: a disease casualty carries no died-of-wounds risk at any check,
# so draws no death time with the switch either way.
operated <- (is_val(off_reason1$r2e_surgery, 1) | is_val(off_reason1$r2b_surgery, 1)) &
  startsWith(off_reason1$name, "wia_")
report(any(operated) && all(!is.na(off_reason1$ame_death_time[operated])),
       "operated wounded evacuees for recovery beyond the policy carry a death time (%d)",
       sum(operated))

# ── 5. Validation ────────────────────────────────────────────────────────────

cat("\n-- the configuration validator --\n")
bad <- read_config("env_data.json")
for (i in seq_along(bad$vars)) {
  if (!identical(bad$vars[[i]]$elm, "r2eheavy")) next
  for (j in seq_along(bad$vars[[i]]$actys)) {
    if (!identical(bad$vars[[i]]$actys[[j]]$acty, "recovery")) next
    for (k in seq_along(bad$vars[[i]]$actys[[j]]$vals)) {
      if (identical(bad$vars[[i]]$actys[[j]]$vals[[k]]$var, "return_while_awaiting_airlift")) {
        bad$vars[[i]]$actys[[j]]$vals[[k]]$val <- 2
      }
    }
  }
}
rejected <- tryCatch({
  validate_env_data_values(bad)
  FALSE
}, error = function(e) grepl("return_while_awaiting_airlift", conditionMessage(e)))
report(rejected, "a switch of 2 is rejected, naming the field")

if (length(state$failures) > 0) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat(" -", f, "\n")
  quit(status = 1)
}
cat("\nAll checks passed.\n")
quit(status = 0)
