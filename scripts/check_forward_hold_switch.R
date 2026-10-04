#!/usr/bin/env Rscript
##############################################################################
## scripts/check_forward_hold_switch.R                                      ##
## Regression check — disabled forward holding is the model as it stood     ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_forward_hold_switch.R
#   Rscript scripts/check_forward_hold_switch.R --days 10
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists: forward post-operative intensive care at R2B is now a
# per-casualty decision (`r2b.post_op_icu`: a stability window per surgical
# pathway and a capacity trigger), replacing a fraction applied to every
# damage control casualty. The rule ships disabled, and the published baseline
# is that disabled model. A disabled rule must therefore cost the model
# nothing: no forward stay, no extra random draw (a single-stage casualty draws
# their post-definitive requirement at R2B only where the rule applies) and so
# an event log identical to a configuration that never carried the fields. A
# stray draw would move every later casualty's randomness without any visible
# change in the run's output, which is why this is asserted on the log itself.

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
#' Run length of each run, in days
CHECK_DAYS <- if ("--days" %in% args) as.integer(args[which(args == "--days") + 1]) else 30L

#' Control seed shared by every run
CHECK_SEED <- 42L

failures <- character(0)

#' Print one PASS or FAIL line, recording a failure when the assertion fails
#'
#' @param ok Logical: whether the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Values interpolated into `fmt`.
#' @return Invisible NULL; called for its side effect.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) failures <<- c(failures, msg)
  invisible(NULL)
}

#' Run the model under a configuration edit and return the attribute log
#'
#' @param edit Function taking the parsed `r2b.post_op_icu` list and returning
#'   the list to run under.
#' @return The attribute monitor of one run, as a data frame.
run_with <- function(edit) {
  ed <- load_elms("env_data.json")
  ed$vars$r2b$post_op_icu <- edit(ed$vars$r2b$post_op_icu)
  assign("env_data", ed, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(ed$elms, length), envir = globalenv())
  invisible(capture.output(suppressWarnings(
    wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)
  )))
  get_mon_attributes(wrapped)
}

cat(sprintf("Forward holding switch check: %d-day runs, seed %d\n\n", CHECK_DAYS, CHECK_SEED))

shipped <- run_with(identity)

# A configuration that never carried the rule's fields.
legacy <- run_with(function(rule) rule[setdiff(names(rule), c(
  "stability_window_dcs", "stability_window_single_stage",
  "capacity_trigger", "capacity_poll_interval"))])
report(isTRUE(all.equal(shipped, legacy)),
       "a configuration without the rule's fields reproduces the shipped run (%d attribute rows)",
       nrow(shipped))

# Explicit zeros and a different poll interval: the interval is read only
# while the capacity trigger is in force.
explicit <- run_with(function(rule) {
  rule$stability_window_dcs <- 0
  rule$stability_window_single_stage <- 0
  rule$capacity_trigger <- 0
  rule$capacity_poll_interval <- 5
  rule
})
report(isTRUE(all.equal(shipped, explicit)),
       "explicit zeros and another poll interval reproduce the shipped run")

# Non-vacuity: the same comparison must detect an enabled rule.
enabled <- run_with(function(rule) {
  rule$stability_window_dcs <- 240
  rule$stability_window_single_stage <- 240
  rule
})
report(!isTRUE(all.equal(shipped, enabled)),
       "an enabled stability window changes the run, so the comparison is not vacuous")

# Nobody is held forward and no single-stage casualty draws a requirement at
# R2B in the shipped run.
held <- shipped %>% filter(key == "r2b_post_op_min", value > 0)
drawn <- shipped %>% filter(key == "post_definitive_total")
report(nrow(held) == 0, "no casualty is held forward in the shipped run")
report(nrow(drawn) == 0, "no post-definitive requirement is drawn at R2B in the shipped run")

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}
cat("All forward holding switch checks passed.\n")
quit(status = 0)
