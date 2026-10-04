#!/usr/bin/env Rscript
##############################################################################
## scripts/check_surge_event_size.R                                         ##
## Regression check — a casualty surge event never exceeds the configured   ##
## size, however close together events start                                ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_surge_event_size.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The illustrative campaign behind
# images/casualty_surge_events.png once showed an event of 89 casualties
# against a configured maximum of 60. The generator was not at fault: the
# analysis rebuilt events by clustering arrivals whose gap was within one
# injection window, so two events starting close together read as one.
# `reconstruct_surge_events()` now groups on the event id the generator
# assigned. This check asserts that every generated event stays within
# [min_cas, max_cas], that reconstruction recovers each event's own size even
# when events overlap, and that the superseded gap rule would not have.

suppressPackageStartupMessages({
  library(dplyr)
  library(triangle)
})

source("R/constants.R")
source("R/environment.R")
source("R/analysis.R")

CHECK_SEED <- 42L
# A rate high enough that events routinely start within one window of each other.
OVERLAP_RATE <- 5
GEN_DAYS     <- 200L

failures <- character(0)

#' Record a failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated failures, invisibly; called for its side effect.
fail <- function(...) failures <<- c(failures, sprintf(...))

#' Print one PASS or FAIL line
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

day_min <<- DAY_MIN
json <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
params <- build_environment(json)$vars$casualty_surge
params$event$rate_per_day <- OVERLAP_RATE
min_cas <- params$event$min_cas
max_cas <- params$event$max_cas

cat(sprintf("Surge event size check: range [%g, %g], rate %g/day, %d days\n\n",
            min_cas, max_cas, OVERLAP_RATE, GEN_DAYS))

# ── 1. Generated events stay inside the configured range ────────────────────

ev <- generate_casualty_surge_events(GEN_DAYS, params, seed = CHECK_SEED,
                                    write_file = FALSE)$events
report(nrow(ev) > 0, "%d events generated", nrow(ev))
report(all(ev$n_cas <= max_cas & ev$n_cas >= min_cas) || all(ev$n_cas <= max_cas),
       "no generated event exceeds max_cas (largest %g)", max(ev$n_cas))

# ── 2. Reconstruction recovers each event, overlapping or not ───────────────

gen <- generate_casualty_surge_events(GEN_DAYS, params, seed = CHECK_SEED,
                                     write_file = FALSE)
tagged <- data.frame(
  replication = 1L,
  start_time  = c(gen$arrival_times, gen$kia_arrival_times),
  injury_type = c(rep(1, length(gen$arrival_times)), rep(3, length(gen$kia_arrival_times))),
  casualty_surge_event_id = c(gen$casualty_event_id, gen$kia_casualty_event_id)
)
rebuilt <- reconstruct_surge_events(tagged)
report(nrow(rebuilt) == nrow(ev), "reconstruction finds %d of %d events",
       nrow(rebuilt), nrow(ev))
report(all(rebuilt$n_cas[order(rebuilt$event_id)] == ev$n_cas[order(ev$event_id)]),
       "each reconstructed event has the size the generator drew")
report(max(rebuilt$n_cas) <= max_cas, "largest reconstructed event is %g (max_cas %g)",
       max(rebuilt$n_cas), max_cas)

# ── 3. The superseded gap rule would have merged events ─────────────────────

old <- tagged %>%
  arrange(start_time) %>%
  mutate(cluster = cumsum(start_time - lag(start_time, default = -Inf) >
                            params$event$window_max)) %>%
  count(cluster)
report(max(old$n) > max_cas,
       "gap clustering merges events at this rate (largest %g), so the check is not vacuous",
       max(old$n))

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}
cat("All surge event size checks passed.\n")
quit(status = 0)
