#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_surgical_roster.R                                     ##
## Regression check — surgical sections share one shift and alternate        ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_surgical_roster.R
#
# Exits 0 when every assertion passes and 1 otherwise, so it can gate a pull
# request.
#
# Why this check exists. build_env() assigns each surgical section its day or
# night shift from a counter, and the surgical team sweep varies how many
# sections each echelon fields. A counter incremented once per member resource
# rather than once per section would split one section across both shifts, so
# that no section was ever fully staffed, and a sweep over the number of
# sections would then measure that fault rather than the establishment. The
# counter is correct today (it advances once per section, the member names
# being added as one vector); this check asserts the property directly, so a
# later change to the loop cannot lose it unnoticed. It asserts three things:
# every member of a section holds the same capacity at the same time, whatever
# the time of day; sections alternate between the day and night shift in the
# order the counter assigns, so a fourth R2E section makes two and two and six
# make three and three; and a second R2B section per unit takes the shift the
# first does not, covering the unit's theatre around the clock. A short
# campaign then confirms the behavioural half: with two sections per R2B unit
# both are used and no case is diverted for a closed section, while at the
# shipped single section some are, so the assertion is not vacuous.

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
source("R/policy_sweep.R")
source("R/hold_window.R")
source("R/long_horizon.R")
source("R/surgical_team_sweep.R")

#' Campaign length of the behavioural half, in days
CHECK_DAYS <- 30L

#' Seed of the behavioural half
CHECK_SEED <- 42L

state <- new.env()
state$failures <- character(0)

#' Record a failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated failures, invisibly; called for its side effect.
fail <- function(...) state$failures <- c(state$failures, sprintf(...))

#' Print one PASS or FAIL line and record a failure
#'
#' @param ok Logical: whether the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Values interpolated into `fmt`.
#' @return The printed line, invisibly; called for its side effect.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
}

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Capacity of every member of every surgical section at one time of day
#'
#' @param echelon `"r2b"` or `"r2eheavy"`.
#' @param at_min Minute of the day to read the roster at.
#' @return A list over teams of lists over sections of the members' capacities,
#'   read from a built environment run to `at_min`.
section_capacities <- function(echelon, at_min) {
  e <- simmer::simmer("roster check")
  e <- build_env(e, env_data)
  e <- simmer::run(e, until = at_min)
  lapply(env_data$elms[[echelon]], function(team) {
    lapply(team[["surg"]], function(members) {
      vapply(members, function(res) simmer::get_capacity(e, res), numeric(1))
    })
  })
}

#' Which shift each section of each team is on, read from its roster
#'
#' @param echelon `"r2b"` or `"r2eheavy"`.
#' @return A list over teams of integer vectors, 1 for a section on shift at
#'   minute 60 and 2 for one on shift at minute 60 plus the break, or NA where
#'   the members disagree.
section_shifts <- function(echelon) {
  brk <- get_ot_hours(env_data) * 60
  day <- section_capacities(echelon, 60)
  night <- section_capacities(echelon, brk + 60)
  lapply(seq_along(day), function(ti) {
    vapply(seq_along(day[[ti]]), function(si) {
      d <- day[[ti]][[si]]
      n <- night[[ti]][[si]]
      if (length(unique(d)) != 1L || length(unique(n)) != 1L) return(NA_integer_)
      if (d[1] > 0 && n[1] == 0) return(1L)
      if (d[1] == 0 && n[1] > 0) return(2L)
      NA_integer_
    }, integer(1))
  })
}

#' Enter a configuration with the given surgical establishment
#'
#' @param r2e_sections Surgical sections at the R2E unit.
#' @param r2b_sections Surgical sections at each R2B unit.
#' @return Invisibly, the configuration applied to the session globals.
set_config <- function(r2e_sections, r2b_sections) {
  apply_surgical_setting(json_data, "default", r2e_sections, r2b_sections)
}

# ── 1. The shipped roster ───────────────────────────────────────────────────

cat("-- the shipped roster: one shift per section, alternating --\n")

set_config(3L, 1L)
r2e <- section_shifts("r2eheavy")[[1]]
report(!anyNA(r2e), "every member of every R2E section shares one shift (%s)",
       paste(r2e, collapse = ", "))
report(identical(r2e, c(1L, 2L, 1L)),
       "three R2E sections take the day, night and day shifts (found %s)",
       paste(r2e, collapse = ", "))
r2b <- unlist(section_shifts("r2b"))
report(!anyNA(r2b) && identical(r2b, c(1L, 2L)),
       "the two R2B units, one section each, are on opposite shifts (found %s)",
       paste(r2b, collapse = ", "))

# ── 2. Other establishments ─────────────────────────────────────────────────

cat("\n-- other establishments: the split follows the counter --\n")

for (n in c(4L, 6L)) {
  set_config(n, 1L)
  shifts <- section_shifts("r2eheavy")[[1]]
  report(!anyNA(shifts) && sum(shifts == 1L) == n / 2 && sum(shifts == 2L) == n / 2,
         "%d R2E sections split evenly between the shifts (found %s)", n,
         paste(shifts, collapse = ", "))
}

set_config(3L, 2L)
r2b_shifts <- section_shifts("r2b")
report(all(vapply(r2b_shifts, function(s) !anyNA(s) && setequal(s, c(1L, 2L)),
                  logical(1))),
       "two R2B sections per unit give every unit a section on each shift (found %s)",
       paste(vapply(r2b_shifts, paste, character(1), collapse = "/"), collapse = " and "))

# ── 3. The setter changes only what it is told to ───────────────────────────

cat("\n-- the setter is exact and rejects what it cannot apply --\n")

set_config(4L, 2L)
report(length(env_data$elms$r2eheavy[[1]][["surg"]]) == 4L &&
         all(vapply(env_data$elms$r2b, function(t) length(t[["surg"]]) == 2L, logical(1))),
       "the section counts reach the built configuration")
report(length(env_data$elms$r2eheavy[[1]][["emerg"]]) == 3L &&
         length(env_data$elms$r2eheavy[[1]][["icu"]]) == 3L,
       "the other R2E sub-elements are left at their shipped counts")
resolved <- resolve_scenario(json_data, "default")
report(inherits(try(set_surgical_sections(resolved, "r2eheavy", 0L), silent = TRUE),
                "try-error"),
       "a section count of zero is rejected")
report(inherits(try(set_surgical_sections(resolved, "r2eheavy", 2.5), silent = TRUE),
                "try-error"),
       "a fractional section count is rejected")
report(inherits(try(set_surgical_sections(resolved, "nonexistent", 2L), silent = TRUE),
                "try-error"),
       "an echelon the configuration does not field is rejected rather than ignored")

# ── 4. Behaviour: a second R2B section is used and closes the roster gap ────

cat("\n-- a short campaign: the second R2B section is used --\n")

#' Run a short campaign and read the R2B diversion and section use off it
#'
#' @param r2b_sections Surgical sections at each R2B unit.
#' @return A list of the count of cases diverted for a closed section and the
#'   number of R2B surgical sections that served a case.
short_campaign <- function(r2b_sections) {
  set_config(3L, r2b_sections)
  set.seed(CHECK_SEED)
  e <- run_once(CHECK_DAYS, seed = CHECK_SEED, write_files = FALSE)
  attrs <- simmer::get_mon_attributes(e)
  arrivals <- simmer::get_mon_arrivals(e, ongoing = TRUE)
  wide <- build_attributes_wide(attrs, arrivals)
  resources <- simmer::get_mon_resources(e)
  members <- unlist(lapply(env_data$elms$r2b, function(t) t[["surg"]]), recursive = FALSE)
  used <- vapply(members, function(m) {
    any(resources$resource %in% m & resources$server > 0)
  }, logical(1))
  list(offshift = sum(!is.na(wide$r2b_bypass_reason) & wide$r2b_bypass_reason == 1),
       used = sum(used), sections = length(members))
}

shipped <- short_campaign(1L)
doubled <- short_campaign(2L)
report(shipped$offshift > 0,
       "at the shipped single section some cases are diverted for a closed section (%d)",
       shipped$offshift)
report(doubled$offshift == 0L,
       "with two sections per unit no case is diverted for a closed section (%d)",
       doubled$offshift)
report(doubled$used == doubled$sections,
       "every R2B surgical section serves a case (%d of %d)", doubled$used,
       doubled$sections)

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nEvery surgical section shares one shift and the roster alternates as designed.\n")
quit(status = 0L)
