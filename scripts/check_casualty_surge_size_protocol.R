#!/usr/bin/env Rscript
##############################################################################
## scripts/check_casualty_surge_size_protocol.R                              ##
## Regression check — the casualty surge event size sweep's parameters, its  ##
## evidence set and its summary agree                                        ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_casualty_surge_size_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The size sweep answers how large an event must be
# before care degrades, and its table in docs/Results.md is generated from the
# tracked summary. Nothing else asserts that the code's protocol constants are
# the ones docs/Methods.md documents, that the tracked evidence set is the
# documented sweep, or that the summary is the reduction of the per-replication
# responses beside it.
#
# What this asserts:
#
#   1. Every size sweep constant in R/casualty_surge.R equals the value
#      docs/Methods.md documents in a marker comment.
#   2. The tracked per-replication responses carry the no-event arm and every
#      documented size at the documented replication count, and no realised
#      event exceeds the size its arm was configured with.
#   3. The tracked summary is the reduction of those responses.
#   4. summarise_casualty_surge_size() is correct on an input whose answer is
#      computable by hand, so assertion 3 is not two copies of one error.

suppressPackageStartupMessages({
  library(dplyr)
})

source("R/constants.R")
source("R/casualty_surge.R")

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
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' The methods paper, which documents the sweep's design
METHODS_PATH <- file.path("docs", "Methods.md")

#' Tracked per-replication responses, every size
REPLICATIONS_PATH <- file.path("data", "casualty_surge", "casualty_surge_size_replications.csv")

#' Tracked per-size summary
SUMMARY_PATH <- file.path("data", "casualty_surge", "casualty_surge_size_summary.csv")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

# ── 1. The code's parameters are the ones the methods paper documents ───────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one size sweep parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "CASUALTY_SURGE_SIZE ".
#' @return The marker's value as a character string, or NA where absent.
size_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- CASUALTY_SURGE_SIZE %s=[^ ]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- CASUALTY_SURGE_SIZE [^=]+=(.*) -->$", "\\1", m)
}

held <- list(replications = CASUALTY_SURGE_SIZE_REPLICATIONS, days = CASUALTY_SURGE_DAYS,
             seed = CASUALTY_SURGE_SEED, rate = CASUALTY_SURGE_SIZE_RATE)
for (param in names(held)) {
  stated <- suppressWarnings(as.numeric(size_marker(param)))
  report(!is.na(stated) && stated == held[[param]],
         "the methods paper states %s = %s and the code holds %s",
         param, format(stated), format(held[[param]]))
}
stated_sizes <- as.numeric(trimws(strsplit(size_marker("sizes"), ",")[[1]]))
report(isTRUE(all.equal(stated_sizes, as.numeric(CASUALTY_SURGE_SIZES))),
       "the methods paper states the sizes %s and the code holds %s",
       paste(stated_sizes, collapse = ","), paste(CASUALTY_SURGE_SIZES, collapse = ","))

# ── 2. The tracked responses are the documented sweep ───────────────────────

cat("\n-- the tracked evidence set is that sweep --\n")

rows <- if (file.exists(REPLICATIONS_PATH)) read.csv(REPLICATIONS_PATH) else NULL
report(!is.null(rows), "the tracked per-replication responses %s exist", REPLICATIONS_PATH)
if (!is.null(rows)) {
  expected <- c(0L, CASUALTY_SURGE_SIZES)
  report(setequal(unique(rows$size), expected),
         "the responses carry the no-event arm and every documented size")
  per_size <- table(rows$size)
  report(all(per_size == CASUALTY_SURGE_SIZE_REPLICATIONS),
         "every size has %d replications", CASUALTY_SURGE_SIZE_REPLICATIONS)
  report(all(rows$max_event_size <= pmax(rows$size, 0)),
         "no realised event exceeds the size its arm was configured with")
  report(all(rows$n_events[rows$size == 0] == 0), "the no-event arm injected no event")

  # ── 3. The summary is the reduction of the responses ──────────────────────

  cat("\n-- the tracked summary is their reduction --\n")
  tracked <- read.csv(SUMMARY_PATH)
  fresh <- summarise_casualty_surge_size(rows)
  report(isTRUE(all.equal(tracked, fresh, tolerance = TOL, check.attributes = FALSE)),
         "the tracked summary equals summarise_casualty_surge_size() of the responses")
}

# ── 4. The summary function is right on a hand-computed input ───────────────

cat("\n-- the summary function is correct on a hand-computed input --\n")

toy <- data.frame(
  size = c(40L, 40L), n_events = c(2L, 4L), max_event_size = c(40L, 38L),
  n_event = c(80L, 160L), dow_event = c(1L, 3L),
  n_ordinary = c(1000L, 1000L), dow_ordinary = c(2L, 4L),
  peak_queue_1 = c(2, 4)
)
out <- summarise_casualty_surge_size(toy)
report(out$mean_events == 3 && out$max_event_size == 40,
       "mean events is 3 and the largest event is 40")
report(abs(out$dow_event_rate - 4 / 240) < TOL, "event died-of-wounds rate pools 4 of 240")
report(abs(out$dow_ordinary_rate - 6 / 2000) < TOL, "ordinary rate pools 6 of 2000")
report(abs(out$peak_queue_1 - 3) < TOL &&
         abs(out$peak_queue_1_ci - qt(0.975, 1) * sd(c(2, 4)) / sqrt(2)) < TOL,
       "peak queue is the mean 3 with its Student t half-width")

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}
cat("All casualty surge size sweep checks passed.\n")
quit(status = 0)
