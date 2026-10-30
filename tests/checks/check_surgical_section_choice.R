#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_surgical_section_choice.R                             ##
## Regression check — a casualty takes the first surgical section free and  ##
## then a theatre, and R2E emergency and evacuation sections are chosen per ##
## casualty                                                                 ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_surgical_section_choice.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. An R2E operation needs a theatre and a surgical
# section, and the sections work a two-shift roster. The model this replaced
# chose the section when the casualty joined the theatre queue and then took a
# theatre and waited for that section, so a theatre stood held for a section
# that had gone off shift or was operating elsewhere while another section sat
# idle, and the R2E theatre backlog the results report rested on that rather
# than on the establishment. The emergency and evacuation sections were fixed
# per route when the trajectory was built, so every casualty on a route queued
# for one section while the others were free. Each casualty now queues on its
# team's pool for the first section free (seize_first_free_section(),
# R/trajectories_common.R) and only then takes a theatre. Either half can
# regress silently, a backlog of binding reading like a shortage of surgeons.
#
# What it asserts, on a high-intensity campaign, where theatres are busy:
#
#   1. No theatre is held without a section operating in it: at every moment
#      the R2E theatres in use do not outnumber the sections in use.
#   2. No casualty waits for a section while a section is on shift and free
#      and a theatre is free, beyond the millionth of a minute after each shift
#      change at which the pool is reset, and none waits for a theatre while
#      one is free.
#   3. Every R2E emergency section resuscitates casualties from every route to
#      R2E (directly from R1, and through each R2B unit).
#   4. Every R2E evacuation section processes casualties who died.
#   5. A single-section R2B unit takes its section directly, as it always has,
#      its pool never queueing; check_pre_open_window.R asserts the hold.
#   6. The cases are reached: casualties wait for a section, so assertion 2 is
#      not vacuous.

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

#' Campaign length of the run, in days
CHECK_DAYS <- 60L

#' Control seed the run is made under
CHECK_SEED <- 42L

#' Scenario profile the run uses
CHECK_SCENARIO <- "high_intensity"

#' Total minutes a condition that should never hold may be measured to hold
#'
#' @details Events at one instant are recorded in sequence, so a condition can
#'   hold for zero simulated time between two rows; anything measurable is a
#'   failure.
TOL <- 1e-6

config_snapshot <- capture_config_globals()
on.exit(restore_config_globals(config_snapshot), add = TRUE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
apply_airlift_setting(json_data, CHECK_SCENARIO)

cat(sprintf("Running seed %d for %d days under %s\n", CHECK_SEED, CHECK_DAYS, CHECK_SCENARIO))
set.seed(CHECK_SEED)
env <- run_once(CHECK_DAYS, seed = CHECK_SEED)
res <- simmer::get_mon_resources(env)
attributes <- simmer::get_mon_attributes(env)
attributes$replication <- 1L
arrivals <- simmer::get_mon_arrivals(env, ongoing = TRUE)
arrivals$replication <- 1L
arrivals <- arrivals[!duplicated(arrivals$name), ]
wide <- build_attributes_wide(attributes, arrivals)
horizon <- CHECK_DAYS * DAY_MIN

#' One quantity over a set of resources as a step function of time
#'
#' @param names Resource names.
#' @param column "server", "queue" or "capacity".
#' @return A list of `time` and `total`, zero throughout where none has a row.
steps_of <- function(names, column) {
  rows <- res[res$resource %in% names & res$time <= horizon, ]
  if (nrow(rows) == 0) return(list(time = 0, total = 0))
  pool_queue_steps(rows$resource, rows$time, rows[[column]])
}

#' The value of a step function at each of a set of times
#'
#' @param steps A list of `time` and `total`.
#' @param t Times in minutes.
#' @return The value in force at each time, zero before the first step.
value_at <- function(steps, t) {
  i <- findInterval(t, steps$time)
  ifelse(i < 1, 0, steps$total[pmax(i, 1)])
}

team <- env_data$elms$r2eheavy[[1]]
sections <- team[["surg"]]
anchors <- vapply(sections, function(m) m[1], character(1))
ot_beds <- team[["ot_bed"]]
surg_pool <- section_pool("r2eheavy", "surg", 1)
ot_pool <- section_pool("r2eheavy", "ot", 1)

beds_busy <- steps_of(ot_beds, "server")
section_busy <- lapply(anchors, steps_of, column = "server")
section_open <- lapply(anchors, steps_of, column = "capacity")
pool_waiting <- steps_of(surg_pool, "queue")
theatre_waiting <- steps_of(ot_pool, "queue")

t <- sort(unique(c(0, beds_busy$time, pool_waiting$time, theatre_waiting$time,
                   unlist(lapply(section_busy, `[[`, "time")),
                   unlist(lapply(section_open, `[[`, "time")))))
t <- t[t < horizon]
dt <- diff(c(t, horizon))
in_use <- Reduce(`+`, lapply(section_busy, value_at, t = t))
free_on_shift <- Reduce(`+`, lapply(seq_along(anchors), function(i) {
  as.numeric(value_at(section_open[[i]], t) > 0 & value_at(section_busy[[i]], t) == 0)
}))
free_theatres <- length(ot_beds) - value_at(beds_busy, t)

# ── 1. No theatre held without a section ─────────────────────────────────────

cat("\n-- no theatre held without a section operating in it --\n")
held_empty <- sum(dt[value_at(beds_busy, t) > in_use])
report(held_empty < TOL,
       "the theatres in use never outnumber the sections in use (%.6f minutes otherwise)",
       held_empty)

# ── 2. No wait while a section and a theatre are free ───────────────────────

cat("\n-- no casualty waits while it could be operated on --\n")
waited <- sum(dt[value_at(pool_waiting, t) >= 1])
idle_wait <- sum(dt[value_at(pool_waiting, t) >= 1 & free_on_shift >= 1 & free_theatres >= 1])
# A section coming on shift is admitted to by its pool a millionth of a minute
# later (ROSTER_SYNC_DELAY_MIN, add_roster_sync()), so that much per shift change
# is the mechanism's own and not a casualty kept waiting.
sync_allowance <- 2 * CHECK_DAYS * ROSTER_SYNC_DELAY_MIN + TOL
report(idle_wait <= sync_allowance,
       paste("no casualty waits for a section while one is on shift and free and a",
             "theatre is free (%.6f of %.0f waiting minutes, within the %.6f the",
             "roster sync allows)"), idle_wait, waited, sync_allowance)
theatre_idle <- sum(dt[value_at(theatre_waiting, t) >= 1 & free_theatres >= 1])
report(theatre_idle < TOL,
       "no casualty waits for a theatre while one is free (%.6f minutes)", theatre_idle)

# ── 3-4. Emergency and evacuation sections chosen per casualty ──────────────

cat("\n-- emergency and evacuation sections are taken per casualty --\n")
resus <- wide[!is.na(wide$r2e_emerg_section) & !is.na(wide$r2e_treated) &
                wide$r2e_treated == 1, ]
route <- ifelse(is.na(resus$r2b_treated), "direct from R1",
                paste("through R2B unit", resus$r2b_treated))
used <- table(route, factor(resus$r2e_emerg_section, levels = seq_along(team[["emerg"]])))
report(all(used > 0),
       "every R2E emergency section resuscitates casualties from every route (%s)",
       paste(sprintf("%s: %s", rownames(used),
                     apply(used, 1, paste, collapse = "/")), collapse = "; "))
evac_used <- vapply(team[["evac"]], function(m) {
  rows <- res[res$resource == m[1], ]
  nrow(rows) > 0 && max(rows$server) > 0
}, logical(1))
report(all(evac_used), "every R2E evacuation section processes casualties who died (%d of %d)",
       sum(evac_used), length(evac_used))

# ── 5. A single-section R2B unit is unchanged ───────────────────────────────

cat("\n-- a single-section R2B unit takes its section directly --\n")
single <- which(vapply(env_data$elms$r2b, function(u) length(u[["surg"]]) == 1L, logical(1)))
r2b_pools <- vapply(single, function(u) section_pool("r2b", "surg", u), character(1))
pool_rows <- res[res$resource %in% r2b_pools, ]
report(length(single) > 0 && (nrow(pool_rows) == 0 || max(pool_rows$queue) == 0),
       "no casualty queues on the pool of a single-section R2B unit (%d units)", length(single))

# ── 6. The cases are reached ─────────────────────────────────────────────────

cat("\n-- the cases are reached --\n")
report(waited > 0, "casualties wait for a section, so assertion 2 is not vacuous (%.0f minutes)",
       waited)

if (length(state$failures) > 0) {
  cat(sprintf("\n%d check(s) failed.\n", length(state$failures)))
  quit(status = 1)
}
cat("\nAll checks passed.\n")
quit(status = 0)
