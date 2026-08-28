#!/usr/bin/env Rscript
##############################################################################
## scripts/check_screen_cache.R                                             ##
## Regression check — a screen's point cache resumes what it recorded       ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_screen_cache.R
#
# Exits 0 when every check passes, 1 otherwise. It writes and reads temporary
# CSV files only, runs no simulation, and finishes in under a second.
#
# Why this check exists. A production screen is a multi-hour process whose
# design points are evaluated once and cached, so an interrupted run resumes
# instead of restarting. The cache is therefore the only record of work that
# cannot be repeated cheaply, and a fault in it is silent in both directions:
# a point wrongly read as cached is never evaluated and its stale responses
# enter the indices, while a point wrongly read as absent is evaluated again
# and the run never terminates on a design it has already covered. Neither
# shows up in the results.
#
# The cache has already failed once in exactly this way. Its write path
# guarded on a point having no missing response at all, so a design point at
# which two of thirty-six responses were legitimately undefined was evaluated,
# discarded and evaluated again on every resume. The screen made no progress
# across restarts and nothing in its output said so. The guard now treats only
# an all-missing row as absent, and that distinction is what the checks below
# hold in place.
#
# What it asserts:
#
#   1. A complete row round-trips. Values written are the values read back,
#      in the order the caller asked for, at the point index it asked for.
#
#   2. A partially-missing row round-trips with its missing entries intact.
#      This is the failure above: such a row is a real result and must read as
#      present, with the missing responses still missing rather than filled.
#
#   3. An all-missing row reads as absent. That is what an evaluation which
#      failed outright leaves behind, and it must be retried, not adopted.
#
#   4. An uncached point reads as absent, as does a point whose requested
#      responses the cache does not carry. The second is what a cache from a
#      different screen looks like, and adopting it would silently mix designs.
#
#   5. Extra columns do not disturb a lookup. The Sobol cache carries a
#      per-response standard deviation alongside each response, so a reader
#      asking for the responses alone must still get exactly those.

source("R/sensitivity.R")

failures <- character(0)

#' Record a failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated failures, invisibly; called for its side effect.
fail     <- function(...) failures <<- c(failures, sprintf(...))

#' Print one PASS or FAIL line, recording a failure
#'
#' @param ok Logical: whether the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Values interpolated into `fmt`.
#' @return Invisible NULL; called for its side effects.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
}

tmp <- tempfile(fileext = ".csv")
on.exit(unlink(tmp), add = TRUE)

RESP <- c("a", "b", "c")

# ── 1. A complete row round-trips ───────────────────────────────────────────

full <- c(a = 1.5, b = -2.25, c = 300)
cache_append(tmp, 1L, full)
got <- cache_lookup(tmp, 1L, RESP)
report(!is.null(got) && identical(names(got), RESP) &&
         isTRUE(all.equal(as.numeric(got), as.numeric(full))),
       "a complete row round-trips unchanged")

# ── 2. A partially-missing row round-trips with its gaps intact ─────────────

partial <- c(a = 4, b = NA_real_, c = 6)
cache_append(tmp, 2L, partial)
got <- cache_lookup(tmp, 2L, RESP)
report(!is.null(got) && is.na(got[["b"]]) &&
         isTRUE(all.equal(got[["a"]], 4)) && isTRUE(all.equal(got[["c"]], 6)),
       "a partially-missing row reads as present with its gaps preserved")

# ── 3. An all-missing row reads as absent ───────────────────────────────────

cache_append(tmp, 3L, c(a = NA_real_, b = NA_real_, c = NA_real_))
report(is.null(cache_lookup(tmp, 3L, RESP)),
       "an all-missing row reads as absent and is retried")

# ── 4. Absent points and foreign caches read as absent ──────────────────────

report(is.null(cache_lookup(tmp, 99L, RESP)),
       "a point never written reads as absent")
report(is.null(cache_lookup(tmp, 1L, c("a", "b", "not_a_response"))),
       "a cache missing a requested response reads as absent")
report(is.null(suppressWarnings(cache_lookup(tempfile(fileext = ".csv"), 1L, RESP))),
       "a cache file that does not exist reads as absent")

# ── 5. Extra columns do not disturb a lookup ────────────────────────────────

tmp2 <- tempfile(fileext = ".csv")
on.exit(unlink(tmp2), add = TRUE)
wide <- c(a = 7, b = 8, c = 9, sd_a = 0.1, sd_b = 0.2, sd_c = 0.3)
cache_append(tmp2, 1L, wide)
got <- cache_lookup(tmp2, 1L, RESP)
report(!is.null(got) && identical(names(got), RESP) &&
         isTRUE(all.equal(as.numeric(got), c(7, 8, 9))),
       "a lookup for the responses alone ignores the standard-deviation columns")

sd_got <- cache_lookup(tmp2, 1L, paste0("sd_", RESP))
report(!is.null(sd_got) &&
         isTRUE(all.equal(as.numeric(sd_got), c(0.1, 0.2, 0.3))),
       "the standard-deviation columns are readable in their own right")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All screen cache checks passed.\n")
quit(status = 0)
