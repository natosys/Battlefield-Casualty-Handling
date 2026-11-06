#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_reception_order.R                                     ##
## Regression check — casualties waiting at R2B reception take a freed      ##
## resuscitation bed by triage priority, then in order of arrival           ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_reception_order.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. A casualty arriving at R2B while every resuscitation
# bed is in use waits for the first to free (r2b_reception(),
# R/trajectories_r2b.R). Which waiting casualty takes it was once decided by a
# simmer signal that woke every waiter, and simmer orders a signal's
# subscribers by memory address, so the same seed gave different campaigns in
# different processes and some replications of the tracked evidence sets did
# not reproduce. The waiters now queue on a pool served by triage priority and,
# within a priority, in order of arrival. Two regressions would bring the fault
# back silently: a wake by signal reintroduced anywhere in the trajectories,
# or the pool's order changed. Neither shows in a campaign's figures, which
# differ between runs only by noise.
#
# What it asserts:
#
#   1. No trajectory source file uses trap(), untrap(), send() or wait(), the
#      activities whose subscribers simmer orders by address.
#   2. On a constructed reception with one resuscitation bed, five casualties
#      arriving while it is occupied take it Priority 1 first, then Priority 2,
#      then Priority 3, each priority in order of arrival.
#   3. The first of them waits in the unit's holding bed and the rest in
#      overflow, and the one finding the bed free takes it at once.
#   4. Two runs of the construction give the same order, bed for bed.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
})

source("R/trajectories.R")

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

#' Minutes each casualty holds the resuscitation bed in the construction
HOLD_MIN <- 100

#' The constructed arrivals: name, arrival time in minutes and triage priority
#'
#' @details The first finds the bed free; the other five arrive while it is
#'   occupied, in an order that differs from both priority order and arrival
#'   order, so neither rule alone gives the expected sequence.
ARRIVALS <- data.frame(
  name     = c("a", "b", "c", "d", "e", "f"),
  time     = c(0, 10, 20, 30, 40, 50),
  priority = c(3, 3, 1, 2, 1, 3),
  stringsAsFactors = FALSE
)

#' The order the construction must serve, by name
EXPECTED_ORDER <- c("a", "c", "e", "d", "b", "f")

# ── 1. No wake by signal in the trajectories ────────────────────────────────

cat("-- no trajectory wakes casualties by signal --\n")

files <- Sys.glob("R/trajectories*.R")
offending <- character(0)
for (f in files) {
  code <- sub("#.*$", "", readLines(f, warn = FALSE))
  hits <- grep("(^|[^A-Za-z0-9_.])(trap|untrap|send|wait)\\(", code)
  if (length(hits) > 0) offending <- c(offending, sprintf("%s:%s", f, hits))
}
report(length(files) > 0 && length(offending) == 0,
       "no trap(), untrap(), send() or wait() in %d trajectory files%s", length(files),
       if (length(offending) > 0) paste0(" (", paste(offending, collapse = ", "), ")") else "")

# ── 2 to 4. The constructed reception ───────────────────────────────────────

#' Build the constructed reception
#'
#' @return A simmer environment, not yet run.
#'
#' @details One resuscitation bed, one holding bed and overflow, with the unit's
#'   pool of one token, all named for this construction alone. The caller
#'   assigns it to the `env` global, which the reception's placement helpers
#'   read, before running it.
build_construction <- function() {
  bed <- "check_resus_bed"
  pool <- "check_resus_pool"
  trj <- trajectory("Constructed Reception") %>%
    set_attribute("priority", function() get_attribute(env, "triage")) %>%
    set_attribute("r2b_treatment_start_time", function() now(env)) %>%
    join(r2b_reception(bed, "check_hold_bed", "check_overflow", pool)) %>%
    set_attribute("bed_taken", function() now(env)) %>%
    timeout(HOLD_MIN) %>%
    release_selected(id = 2) %>%
    release(pool, 1)

  simmer("reception order") %>%
    add_resource(bed, 1) %>%
    add_resource("check_hold_bed", 1) %>%
    add_resource("check_overflow", Inf) %>%
    add_resource(pool, 1) %>%
    # A column named priority would set simmer's own prioritization, so the
    # triage priority travels as "triage" and is copied in by the trajectory.
    add_dataframe("cas_", trj, data.frame(time = ARRIVALS$time, triage = ARRIVALS$priority),
                  mon = 2, time = "absolute", col_attributes = "triage")
}

#' The order in which a finished construction's casualties took the bed
#'
#' @param sim The construction after its run.
#' @return Data frame of name, placement and the minute each casualty took the
#'   resuscitation bed, in the order they took it.
bed_order <- function(sim) {
  att <- get_mon_attributes(sim)
  #' Last value of one attribute for every casualty that set it
  #'
  #' @param key Attribute key.
  #' @return Named vector of values, by arrival name.
  last_value <- function(key) {
    x <- att[att$key == key, ]
    tapply(x$value, x$name, function(v) v[length(v)])
  }
  taken <- last_value("bed_taken")
  placed <- last_value("r2b_reception_bed")
  ids <- paste0("cas_", seq_len(nrow(ARRIVALS)) - 1L)
  out <- data.frame(name = ARRIVALS$name, placement = as.numeric(placed[ids]),
                    taken = as.numeric(taken[ids]), stringsAsFactors = FALSE)
  out[order(out$taken), ]
}

cat("\n-- a freed bed goes by triage priority, then arrival --\n")

env <- build_construction()
env %>% run()
first <- bed_order(env)
report(identical(first$name, EXPECTED_ORDER),
       "the bed is taken in the order %s (found %s)",
       paste(EXPECTED_ORDER, collapse = ", "), paste(first$name, collapse = ", "))
report(isTRUE(all.equal(first$taken, HOLD_MIN * (seq_len(nrow(ARRIVALS)) - 1L))),
       "each casualty takes the bed the minute the one before releases it")

placement <- setNames(first$placement, first$name)
report(placement[["a"]] == 1 && placement[["b"]] == 2 &&
         all(placement[c("c", "d", "e", "f")] == 3),
       "the first finds the bed free, the next waits in holding, the rest in overflow")

cat("\n-- the order is the same on a second run --\n")

env <- build_construction()
env %>% run()
second <- bed_order(env)
report(identical(first, second), "a second run gives the same order and times")

if (length(state$failures) > 0) {
  cat(sprintf("\n%d check(s) failed.\n", length(state$failures)))
  quit(status = 1)
}
cat("\nAll checks passed.\n")
quit(status = 0)
