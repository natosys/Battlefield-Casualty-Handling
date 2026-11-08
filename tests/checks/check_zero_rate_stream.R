#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_zero_rate_stream.R                                    ##
## Regression check — a casualty stream at a rate of zero runs empty        ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_zero_rate_stream.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Switching a casualty stream off, no killed in action
# among the support population for instance, is an input the console's
# Configure panel offers, its rate fields taking a minimum of zero. A lognormal
# stream derived its parameters from the log of its mean, so a mean of zero
# made every replication fail inside the arrival process with an error naming
# neither the stream nor the value, while an exponential stream at zero
# still drew a random number for every day of the campaign. Either stream now
# answers a zero mean before deriving anything, and this check holds that.
#
# What this asserts:
#
#   1. A lognormal and an exponential generator at a mean of zero end at once
#      and consume no random draw.
#   2. A short campaign with one stream at zero completes, generates nothing
#      from that stream, and still generates the others.

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

#' Configuration file the check edits
JSON_PATH <- "env_data.json"

#' Campaign length of the short run, in days
CHECK_DAYS <- 10L

#' Control seed of the short run
CHECK_SEED <- 586L

#' Stream switched off
ZERO_STREAM <- "kia_spt"

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

#' Whether a generator closure ends at once without drawing
#'
#' @param make Function of no arguments returning the generator closure.
#' @return TRUE where the first call returns -1 and the random number stream is
#'   where it was before the closure was built.
ends_without_drawing <- function(make) {
  set.seed(CHECK_SEED)
  before <- .Random.seed
  gen <- make()
  first <- gen()
  identical(first, -1) && identical(.Random.seed, before)
}

# ── 1. The generators at a mean of zero ─────────────────────────────────────

cat("-- a generator at a mean of zero --\n")

env <- simmer("zero rate check") %>% add_global("effective_force_check", 1000)
day_min <- DAY_MIN
report(ends_without_drawing(function() {
  make_ln_arrival_generator(0, 3.56, "effective_force_check", 1000, CHECK_DAYS)
}), "a lognormal stream at a mean of zero ends at once and draws nothing")
report(ends_without_drawing(function() {
  make_exp_arrival_generator(0, "effective_force_check", 1000, CHECK_DAYS)
}), "an exponential stream at a mean of zero ends at once and draws nothing")

# ── 2. A campaign with one stream switched off ──────────────────────────────

cat("\n-- a campaign with one stream switched off --\n")

json <- read_config(JSON_PATH)
described <- build_environment(resolve_scenario(json, "default"))
described$vars$generators[[ZERO_STREAM]]$mean_daily <- 0
env_data <- described
counts <- sapply(described$elms, length)

run <- tryCatch(run_once(CHECK_DAYS, seed = CHECK_SEED),
                error = function(e) conditionMessage(e))
completed <- !is.character(run)
report(completed, "a %d-day campaign with %s at zero completes%s", CHECK_DAYS, ZERO_STREAM,
       if (completed) "" else paste0(" (failed: ", run, ")"))
if (completed) {
  names_seen <- unique(simmer::get_mon_arrivals(run, ongoing = TRUE)$name)
  stream_of <- sub("[0-9]+$", "", names_seen)
  report(!any(stream_of == ZERO_STREAM), "no casualty comes from %s", ZERO_STREAM)
  report(all(c("wia_cbt", "dnbi_cbt", "wia_spt") %in% stream_of),
         "the other streams still generate casualties")
}

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nA casualty stream at a rate of zero runs empty.\n")
quit(status = 0L)
