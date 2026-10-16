#!/usr/bin/env Rscript
##############################################################################
## scripts/check_surgery_delivery_response.R                                ##
## Regression check — the screens measure surgical delivery, and no        ##
## duplicate theatre response remains                                       ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_surgery_delivery_response.R
#   Rscript scripts/check_surgery_delivery_response.R --days 5 --reps 2 --seed 42
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The sensitivity screens ranked parameters on queue
# lengths, which cannot register a casualty who died awaiting theatre, was
# deferred while intensive care was full, or was diverted past it: each leaves
# the queue and never reaches theatre. A delivery response, the share of
# casualties requiring surgery who were operated on, was added to measure that
# directly (R/sensitivity.R, `surgery_delivery_rate`). `system_ot_q`, the sum
# of the R2B and R2E theatre queues, was retired alongside it: the R2B theatre
# queue is zero at every design point, so it equalled `r2e_ot_q` and gave a
# second, identical ranking.
#
# What it asserts:
#
#   1. The response set carries `surgery_delivery_rate` in the surgical
#      throughput domain, carries no `system_ot_q`, and every response has a
#      label, domain, criteria and reduction (no blank cell from a vector
#      that drifted out of step with the names).
#   2. The Sobol response set carries no `system_ot_q`.
#   3. compute_surgery_delivery() is correct on inputs whose answers are
#      computable by hand: a casualty who never reached theatre is in the
#      denominator, an operation at either echelon counts, a casualty not
#      requiring surgery is in neither, and no casualty requiring surgery is
#      NA rather than zero.
#   4. Fault injection: the share over all casualties, and the share over
#      those operated, each disagree with the hand answer, so the assertion in
#      3 is not satisfied by a wrong denominator.
#   5. On a real short run, extract_kpis() returns its responses in the order
#      morris_kpis names them, the delivery rate lies in [0, 1], and it equals
#      a recount taken from the raw attribute log independently of the
#      function under test.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/sensitivity.R")

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

#' Run length in days for the end-to-end arm
CHECK_DAYS <- as.integer(arg_value("--days", 5L))

#' Replications for the end-to-end arm
CHECK_REPS <- as.integer(arg_value("--reps", 2L))

#' Seed for the end-to-end arm
CHECK_SEED <- as.integer(arg_value("--seed", 42L))

#' Tolerance when comparing two shares computed by different routes
TOLERANCE <- 1e-12

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
#' @return Invisibly, NULL.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

cat("Surgical delivery response check\n\n")

# ── 1 and 2. The response sets ───────────────────────────────────────────────

cat("-- the response sets --\n")

report("surgery_delivery_rate" %in% morris_kpis$name,
       "the Morris response set carries surgery_delivery_rate")
report(identical(morris_kpis$domain[morris_kpis$name == "surgery_delivery_rate"],
                 "3 — Surgical throughput"),
       "surgery_delivery_rate sits in the surgical throughput domain")
report(!("system_ot_q" %in% morris_kpis$name),
       "the Morris response set carries no system_ot_q")
report(!any(duplicated(morris_kpis$name)), "the response names are unique (%d responses)",
       nrow(morris_kpis))
blank <- vapply(morris_kpis[c("label", "domain", "criteria", "reduction")],
                function(col) sum(is.na(col) | !nzchar(col)), integer(1))
report(all(blank == 0), "every response has a label, domain, criteria and reduction (blanks: %s)",
       paste(names(blank), blank, sep = "=", collapse = ", "))
report(!("system_ot_q" %in% SOBOL_RESPONSES),
       "the Sobol response set carries no system_ot_q (%s)", paste(SOBOL_RESPONSES, collapse = ", "))

# ── 3 and 4. The delivery share, on inputs with a hand answer ───────────────

cat("\n-- compute_surgery_delivery() on hand-computed inputs --\n")

# Seven casualties: five require surgery. One is operated at R2B, one at R2E,
# one died awaiting theatre (no start), one was diverted past it and never
# operated, one is operated at both echelons (counted once). Two do not
# require surgery, one of whom has an operation recorded (cannot happen in the
# model, and must not enter either term if it did).
surgery   <- c(1, 1, 1, 1, 1, 0, 0)
r2b_start <- c(10, NA, NA, NA, 30, NA, 50)
r2e_start <- c(NA, 20, NA, NA, 40, NA, NA)
expected  <- 3 / 5

got <- compute_surgery_delivery(surgery, r2b_start, r2e_start)
report(abs(got - expected) < TOLERANCE,
       "delivery is %.4f on the hand case (expected %.4f)", got, expected)
report(is.na(compute_surgery_delivery(c(0, 0, NA), c(NA, 5, NA), c(NA, NA, NA))),
       "no casualty requiring surgery gives NA rather than zero")
report(abs(compute_surgery_delivery(c(1, 1), c(NA, NA), c(NA, NA)) - 0) < TOLERANCE,
       "casualties requiring surgery and none operated gives zero")
report(abs(compute_surgery_delivery(c(1, NA), c(5, NA), c(NA, NA)) - 1) < TOLERANCE,
       "a casualty whose surgery attribute was never set is not in the denominator")

over_all      <- sum(!is.na(r2b_start) | !is.na(r2e_start)) / length(surgery)
over_operated <- sum(surgery == 1 & (!is.na(r2b_start) | !is.na(r2e_start))) /
  sum(!is.na(r2b_start) | !is.na(r2e_start))
report(abs(over_all - expected) > 1e-6,
       "fault injection: the share over all casualties (%.4f) disagrees with the hand answer", over_all)
report(abs(over_operated - expected) > 1e-6,
       "fault injection: the share over those operated (%.4f) disagrees with the hand answer",
       over_operated)

# ── 5. A real run ────────────────────────────────────────────────────────────

cat(sprintf("\n-- a real %d-day, %d-replication run at seed %d --\n",
            CHECK_DAYS, CHECK_REPS, CHECK_SEED))

json     <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
env_data <- build_environment(resolve_scenario(json, "default"))
day_min  <- DAY_MIN
counts   <- sapply(env_data$elms, length)

set.seed(CHECK_SEED)
invisible(capture.output(mon <- run_replications(CHECK_REPS, CHECK_DAYS)))
kpis <- extract_kpis(mon)

report(identical(names(kpis), morris_kpis$name),
       "extract_kpis() returns the responses in morris_kpis order (%d)", length(kpis))
rate <- kpis[["surgery_delivery_rate"]]
report(is.finite(rate) && rate >= 0 && rate <= 1,
       "the delivery rate lies in [0, 1] (%.4f)", rate)

# Independent recount from the raw attribute log, one row per replication and
# casualty, without going through build_attributes_wide() or extract_kpis().
attrs <- mon$attributes
#' Casualties that have one attribute recorded, one row each
#'
#' @param key Attribute key to look for in the raw log.
#' @return Data frame of the distinct replication and casualty name pairs.
key_rows <- function(key) unique(attrs[attrs$key == key, c("replication", "name")])
needs <- unique(attrs[attrs$key == "surgery" & attrs$value == 1, c("replication", "name")])
ops   <- unique(rbind(key_rows("r2b_surgery_start"), key_rows("r2e_surgery_1_start")))
#' Identify each casualty by replication and name
#'
#' @param d Data frame with `replication` and `name` columns.
#' @return Character vector, one identifier per row.
tag <- function(d) paste(d$replication, d$name)
recount <- if (nrow(needs) == 0) NA_real_ else mean(tag(needs) %in% tag(ops))
report(isTRUE(all.equal(rate, recount, tolerance = 1e-9)) || (is.na(rate) && is.na(recount)),
       "the delivery rate (%.4f) equals a recount from the raw attribute log (%.4f, %d requiring surgery)",
       rate, recount, nrow(needs))

if (length(state$failures)) {
  cat(sprintf("\n%d check(s) failed:\n", length(state$failures)))
  for (m in state$failures) cat(sprintf(" - %s\n", m))
  quit(status = 1)
}
cat("\nAll surgical delivery response checks passed.\n")
quit(status = 0)
