#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_r2b_evac_wait.R                                       ##
## Regression check — an R2B casualty waiting for the evacuation vehicle is ##
## placed rather than queued for a bed, and the R2B gate applies only when  ##
## forward holding can use R2B intensive care                               ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_r2b_evac_wait.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. A casualty at R2B whose evacuation crew is out waits
# for it in the best placement free at that instant: an intensive care bed,
# otherwise a holding bed, otherwise overflow (r2b_wait_for_evac(),
# R/trajectories_r2b.R). The model this replaced queued the casualty for an
# intensive care bed and then the section before it could board, so departure
# was gated by bed turnover rather than by the vehicle, and waiters who took
# both beds deferred every Priority 2 and 3 operation at the unit through the
# pre-theatre gate. That gate rationed beds an R2B operation never uses while
# nobody is held forward after surgery, so it now applies only where the
# forward holding rule can place the casualty in R2B intensive care
# (forward_hold_enabled()). Either half can regress silently: a waiter queueing
# again for a bed reads as forward congestion, and a gate firing at the shipped
# rule reads as a theatre constraint.
#
# What it asserts:
#
#   1. Under a forced shortage of the evacuation crew, no casualty queues for an
#      R2B intensive care bed, and no waiter seizes the intensive care section,
#      which staffs both of the unit's beds rather than one casualty's.
#   2. Every waiter boards as soon as the crew is free: across each wait the
#      unit's crew is committed throughout, so the vehicle alone sets departure.
#   3. Each placement arm is reached: intensive care, holding bed and overflow.
#   4. At the shipped forward holding rule no R2B operation is deferred, the
#      R2E gate being switched off so that any deferral counted is R2B's.
#   5. With a forward stability window in force the R2B gate still defers
#      Priority 2 and 3 casualties while R2B intensive care is full, so
#      assertion 4 is not vacuous.

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

#' Campaign length of each run, in days
CHECK_DAYS <- 60L

#' Control seed every run is made under
CHECK_SEED <- 42L

#' Scenario profile of the crew shortage run
#'
#' @details High intensity, so that enough casualties reach the evacuation
#'   decision while the crew is out for every placement arm to be reached.
CHECK_SCENARIO <- "high_intensity"

#' R2B to R2E road move forced on the crew shortage run, in minutes
#'
#' @details Four to eight hours each way against the shipped 15 to 45 minutes,
#'   so each unit's crew is out for most of the campaign and casualties wait.
CHECK_TRANSPORT <- list(min = 240, mode = 360, max = 480)

#' Forward stability window of the gate run, in minutes
CHECK_WINDOW <- 1440

#' Tolerance on a comparison of simulation times, in minutes
TOL <- 1e-6

config_snapshot <- capture_config_globals()
on.exit(restore_config_globals(config_snapshot), add = TRUE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Run one campaign and return its monitors
#'
#' @param scenario Scenario profile to run under.
#' @param transport R2B road move to set, or NULL for the shipped one.
#' @param window Forward stability window to set on both pathways, or NULL for
#'   the shipped rule.
#' @param r2e_gate R2E gate state to set (0 or 1).
#' @return A list of `resources` and `wide`, one row per casualty.
run_campaign <- function(scenario, transport = NULL, window = NULL, r2e_gate = 1) {
  apply_airlift_setting(json_data, scenario)
  if (!is.null(transport)) env_data$vars$r2b$wia_transport <<- transport
  if (!is.null(window)) {
    env_data$vars$r2b$post_op_icu$stability_window_dcs <<- window
    env_data$vars$r2b$post_op_icu$stability_window_single_stage <<- window
  }
  env_data$vars$r2eheavy$icu_gating$enabled <<- r2e_gate
  set.seed(CHECK_SEED)
  env <- run_once(CHECK_DAYS, seed = CHECK_SEED)
  attributes <- simmer::get_mon_attributes(env)
  attributes$replication <- 1L
  arrivals <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  arrivals$replication <- 1L
  arrivals <- arrivals[!duplicated(arrivals$name), ]
  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"), suffix = c("", "_arrival"))
  list(resources = simmer::get_mon_resources(env), wide = wide)
}

# ── 1-3. A forced crew shortage ──────────────────────────────────────────────

cat(sprintf("\n-- crew shortage: %s, R2B road move %d to %d minutes --\n",
            CHECK_SCENARIO, CHECK_TRANSPORT$min, CHECK_TRANSPORT$max))
short <- run_campaign(CHECK_SCENARIO, transport = CHECK_TRANSPORT)
res <- short$resources
w <- short$wide

report(max(res$queue[grepl("^b_r2b_icu_", res$resource)]) == 0,
       "nobody queues for an R2B intensive care bed")
section <- res[grepl("^c_r2b_icu_", res$resource), ]
report(nrow(section) == 0 || max(section$server) == 0,
       "no waiter seizes the R2B intensive care section, which staffs both beds")

waiters <- w[!is.na(w$r2b_evac_wait_start) & !is.na(w$r2b_departure_time), ]
violations <- 0L
for (i in seq_len(nrow(waiters))) {
  team <- waiters$r2b_treated[i]
  crew <- env_data$elms$r2b[[team]][["evac"]][[1]]
  rows <- res[res$resource %in% crew, ]
  steps <- pool_queue_steps(rows$resource, rows$time, rows$server)
  from <- waiters$r2b_evac_wait_start[i]
  to   <- waiters$r2b_departure_time[i]
  at_start <- steps$total[max(c(0L, which(steps$time <= from + TOL)))]
  inside <- steps$total[steps$time > from + TOL & steps$time < to - TOL]
  if (length(at_start) == 0 || any(c(at_start, inside) < length(crew))) {
    violations <- violations + 1L
  }
}
report(nrow(waiters) > 0 && violations == 0L,
       paste("every waiter boards as soon as its crew is free: the crew is committed",
             "across every wait (%d waits, %d violations)"), nrow(waiters), violations)

arms <- vapply(1:3, function(v) sum(w$r2b_evac_wait_bed == v, na.rm = TRUE), numeric(1))
report(all(arms > 0), "every placement arm is reached (intensive care %d, holding %d, overflow %d)",
       arms[1], arms[2], arms[3])

# ── 4-5. The R2B gate applies only when forward holding can use its beds ────

cat("\n-- the R2B gate, with the R2E gate switched off --\n")
shipped <- run_campaign(CHECK_SCENARIO, r2e_gate = 0)
deferred_shipped <- sum(shipped$wide$surgery_deferred == 1, na.rm = TRUE)
report(deferred_shipped == 0,
       "at the shipped forward holding rule no R2B operation is deferred (%d)", deferred_shipped)

windowed <- run_campaign(CHECK_SCENARIO, window = CHECK_WINDOW, r2e_gate = 0)
deferred <- windowed$wide[!is.na(windowed$wide$surgery_deferred) &
                            windowed$wide$surgery_deferred == 1, ]
report(nrow(deferred) > 0 && all(deferred$priority >= 2),
       paste("with a %d-minute forward window the R2B gate defers Priority 2 and 3",
             "casualties (%d deferred, none Priority 1)"), CHECK_WINDOW, nrow(deferred))

if (length(state$failures) > 0) {
  cat(sprintf("\n%d check(s) failed.\n", length(state$failures)))
  quit(status = 1)
}
cat("\nAll checks passed.\n")
quit(status = 0)
