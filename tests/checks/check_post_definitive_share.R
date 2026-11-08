#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_post_definitive_share.R                               ##
## Regression check — the post-definitive intensive care share divides the  ##
## operated cohort between intensive care and ward recovery                 ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_post_definitive_share.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Every operated casualty is given post-definitive
# intensive care at the shipped share of 1, an upper bound no open-access source
# confirms (README Post-Definitive Care), so `post_definitive_icu.share` lets a
# planner test how far the intensive care findings rest on it. The shipped value
# must cost the model nothing: no draw, no ward arm, and a run identical to a
# configuration saved before the field existed, or every tracked evidence set
# would move. Below it, a casualty who does not need intensive care recovers on
# the ward and must stay out of every measure of intensive care access, which
# reads `post_definitive_pathway`, while still being charged to the holding
# pool it occupies.
#
# What this asserts:
#
#   1. A configuration without the share runs identically to the shipped one.
#   2. At a share of 0, no post-definitive stay is placed, and every casualty
#      owed the episode recovers on the ward instead.
#   3. At a share of 0.5, both arms occur, no casualty takes both, and a ward
#      casualty carries no `post_definitive_pathway`.
#   4. At a share of 0.5, the holding pool's occupancy is still accounted for
#      in full by its components.

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
source("R/analysis.R")
source("R/airlift.R")

#' Configuration file the check edits
JSON_PATH <- "env_data.json"

#' Campaign length of each run, in days
CHECK_DAYS <- 30L

#' Control seed shared by every run
CHECK_SEED <- 587L

#' Tolerance on the holding pool's unexplained bed-days
TOL <- 1e-6

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

#' Run one campaign with the share set, or removed
#'
#' @param share The share to run under, or NULL to remove the field.
#' @return List of the attribute monitor, the wide attribute frame and the
#'   holding occupancy split.
run_at <- function(share) {
  described <- build_environment(resolve_scenario(read_config(JSON_PATH), "default"))
  described$vars$r2eheavy$post_definitive_icu$share <- share
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(capture.output(suppressWarnings(
    env <- run_once(CHECK_DAYS, seed = CHECK_SEED, write_files = FALSE)
  )))
  attributes <- simmer::get_mon_attributes(env)
  arrivals <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  arrivals <- arrivals[!duplicated(arrivals[, c("name", "replication")]), ]
  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"), suffix = c("", "_arrival"))
  list(attributes = attributes, wide = wide,
       split = holding_occupancy_split(simmer::get_mon_resources(env), wide,
                                       CHECK_DAYS * DAY_MIN))
}

#' Casualties carrying a value of an attribute
#'
#' @param wide Wide attribute frame.
#' @param key  Attribute name.
#' @return Logical vector, TRUE where the attribute is set.
has <- function(wide, key) !is.na(wide[[key]])

# ── 1. The shipped share is the model without it ────────────────────────────

cat("-- the shipped share costs the model nothing --\n")

shipped <- run_at(1)
absent <- run_at(NULL)
report(identical(shipped$attributes, absent$attributes),
       "a configuration without the share runs identically to the shipped one")
report(!any(has(shipped$wide, "post_definitive_ward")),
       "at the shipped share no casualty recovers on the ward")

# ── 2. A share of zero ──────────────────────────────────────────────────────

cat("\n-- a share of zero --\n")

none <- run_at(0)
report(!any(has(none$wide, "post_definitive_pathway")),
       "no post-definitive stay is placed in intensive care, a holding bed or overflow")
ward_none <- sum(has(none$wide, "post_definitive_ward"))
report(ward_none > 0 && ward_none == sum(has(none$wide, "post_definitive_ward_min")),
       "every casualty owed the episode recovers on the ward (%d)", ward_none)

# ── 3 and 4. A share of one half ────────────────────────────────────────────

cat("\n-- a share of one half --\n")

half <- run_at(0.5)
in_icu <- has(half$wide, "post_definitive_pathway")
on_ward <- has(half$wide, "post_definitive_ward")
report(sum(in_icu) > 0 && sum(on_ward) > 0,
       "both arms occur (%d placed stays, %d ward recoveries)", sum(in_icu), sum(on_ward))
report(!any(in_icu & on_ward), "no casualty takes both arms")
report(all(half$wide$post_definitive_ward[on_ward] %in% c(2, 3)),
       "a ward recovery is recorded in a holding bed (2) or overflow (3)")
report(abs(half$split$unexplained_bed_days) < TOL,
       "the holding pool's occupancy is accounted for in full (%.2e bed-days unexplained)",
       half$split$unexplained_bed_days)

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nThe post-definitive intensive care share divides the operated cohort.\n")
quit(status = 0L)
