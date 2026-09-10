#!/usr/bin/env Rscript
##############################################################################
## scripts/check_replication_loss_reporting.R                               ##
## Regression check — a lost replication is reported, not silently dropped  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_replication_loss_reporting.R
#   Rscript scripts/check_replication_loss_reporting.R --days 2
#
# Exits 0 when every check passes, 1 otherwise. Most assertions construct a
# dispatch result directly and run no simulation; two short runs cover the
# framework's own return value.
#
# Why this check exists. run_replications() used to continue on the survivors
# when a worker process was killed, and to publish the count it had asked for
# while computing the metrics from the survivors, so a run that lost workers
# reported a replication count it never achieved and every interval beside it
# was read against that number (Issue #320).
#
# Continuing at all is the part that needed the closer look. It is tempting to
# say that losing a few of fifty costs precision and nothing else, but that
# assumes the replications that die are a random subset of those dispatched. A
# worker is killed because the host ran out of memory, the killer takes the
# largest process, and a replication generating more casualties carries more
# monitoring data, so the losses skew toward the heavier campaigns and the
# survivors are biased low on queue depth, occupancy and mortality. Any loss
# therefore stops the run by default, and a caller who would rather lose a
# point than lose a four-day screen raises the threshold at the call site.
#
# The failure is silent by construction. A killed worker leaves a NULL where an
# environment should be, the warning scrolls past in a run that takes hours,
# and nothing downstream re-reads it. It is also not rare: a 50-replication
# died-of-wounds calibration lost 12 of its 50 to the host's memory limit
# during Issue #312's work, and 50 replications is the count the shipped
# comparative scenario analysis uses.
#
# What it asserts:
#
#   1. A clean dispatch is unchanged: every replication survives, and the
#      framework reports the count it was asked for.
#   2. Any loss stops the run under the shipped threshold of zero.
#   3. A caller that deliberately raises the threshold gets the old behaviour
#      within it: a warning, and only the failed entries dropped.
#   4. A loss beyond a raised threshold still stops.
#   5. A total loss stops however the threshold is set.
#   6. A figure's caption names the count it was given, so the realised count
#      passed to it is the count a reader sees.
#   7. The Welch analysis hands its plot the realised count rather than the
#      requested one. This is asserted on the call itself rather than on the
#      rendered figure: the two counts agree on any run that loses nothing, so
#      a behavioural test would need a killed worker inside a 90-day analysis
#      to tell the fixed code from the broken code, and would pass on both
#      otherwise.
#
# The failure is injected by handing drop_failed_replications() a dispatch
# result with NULL entries, which is exactly what mclapply returns for a killed
# worker. Exhausting memory to produce one would make the check unrunnable on
# the per-PR gate and would not test anything further.

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
source("R/warmup.R")

args <- commandArgs(trailingOnly = TRUE)

#' Read one flagged command line argument
#'
#' @param flag Flag to look for, including its leading dashes.
#' @param default Value returned when the flag is absent or carries no value.
#' @return The argument following the flag, or `default`.
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

#' Run length in days for the two short runs that exercise the return value
#'
#' @details Two days is enough for a replication to complete and be counted,
#'   which is all these assertions read; nothing here depends on the campaign
#'   being long enough to be realistic.
CHECK_DAYS <- as.integer(arg_value("--days", 2L))

#' Replications dispatched by the runs that exercise the return value
CHECK_REPS <- 4L

#' Replications in the constructed dispatch results
#'
#' @details Twenty, so that the raised threshold the tolerance assertions pass
#'   admits one loss and rejects three, letting both sides of that rule be
#'   exercised on whole replications rather than on a fraction that rounds.
N_DISPATCH <- 20L

failures <- character(0)

#' Record a failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated failures, invisibly; called for its side effect.
fail <- function(...) failures <<- c(failures, sprintf(...))

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

#' A dispatch result with a given number of its entries killed
#'
#' @param n Total replications dispatched.
#' @param n_killed How many of them to replace with NULL.
#' @return A list of length `n`, the first `n - n_killed` entries standing in
#'   for surviving environments and the remainder NULL.
#' @details The survivors need only satisfy the validity test
#'   drop_failed_replications() applies, which asks for an environment, so a
#'   bare new.env() stands in for a wrapped simmer environment and the check
#'   needs no simulation to exercise the accounting.
dispatch_with_losses <- function(n, n_killed) {
  out <- replicate(n, new.env(), simplify = FALSE)
  if (n_killed > 0) out[seq(n - n_killed + 1, n)] <- list(NULL)
  out
}

# ── 1. A clean dispatch is unchanged ────────────────────────────────────────

clean <- drop_failed_replications(dispatch_with_losses(N_DISPATCH, 0), N_DISPATCH)
report(length(clean$envs) == N_DISPATCH && all(clean$valid),
       "a dispatch losing nothing keeps all %d replications", N_DISPATCH)

# ── 2. Any loss stops under the shipped threshold ───────────────────────────

default_loss <- try(
  suppressWarnings(
    drop_failed_replications(dispatch_with_losses(N_DISPATCH, 1L), N_DISPATCH)
  ),
  silent = TRUE
)
report(inherits(default_loss, "try-error"),
       "losing even 1 of %d stops under the shipped threshold of %g",
       N_DISPATCH, MAX_REPLICATION_LOSS)

# ── 3. A raised threshold restores tolerance within it ──────────────────────

n_small <- 1L
raised  <- 0.10
warned  <- FALSE
inside  <- withCallingHandlers(
  drop_failed_replications(dispatch_with_losses(N_DISPATCH, n_small), N_DISPATCH,
                           max_loss = raised),
  warning = function(w) {
    warned <<- TRUE
    invokeRestart("muffleWarning")
  }
)
report(warned, "a loss a caller has deliberately allowed warns rather than passing silently")
report(length(inside$envs) == N_DISPATCH - n_small,
       "an allowed loss keeps the %d survivors, not the %d requested",
       N_DISPATCH - n_small, N_DISPATCH)
report(sum(!inside$valid) == n_small,
       "the valid vector marks exactly the %d lost replication(s)", n_small)

# ── 4. A loss beyond a raised threshold still stops ─────────────────────────

n_large <- as.integer(ceiling(N_DISPATCH * raised)) + 1L
beyond  <- try(
  suppressWarnings(
    drop_failed_replications(dispatch_with_losses(N_DISPATCH, n_large), N_DISPATCH,
                             max_loss = raised)
  ),
  silent = TRUE
)
report(inherits(beyond, "try-error"),
       "losing %d of %d, beyond a raised threshold of %.0f%%, still stops",
       n_large, N_DISPATCH, 100 * raised)

# ── 5. A total loss stops however the threshold is set ──────────────────────

total <- try(
  suppressWarnings(
    drop_failed_replications(dispatch_with_losses(N_DISPATCH, N_DISPATCH), N_DISPATCH,
                             max_loss = raised)
  ),
  silent = TRUE
)
report(inherits(total, "try-error"), "losing every replication stops")

# ── 6. The framework reports the count that contributed ─────────────────────

env_data <<- load_scenario("env_data.json", "default")
day_min  <<- DAY_MIN
counts   <<- sapply(env_data$elms, length)

set.seed(42)
mon <- run_replications(CHECK_REPS, CHECK_DAYS)
report(!is.null(mon$n_replications) && !is.null(mon$n_requested),
       "run_replications() returns both the realised and the requested count")
report(identical(mon$n_replications, CHECK_REPS) &&
         identical(mon$n_requested, CHECK_REPS),
       "on a clean run both counts equal the %d requested", CHECK_REPS)
report(identical(mon$n_replications, length(mon$seeds)),
       "the realised count agrees with the seeds of the replications that ran")

# ── 7. A caption names the count it is given ────────────────────────────────

cma <- data.frame(bin_min = seq(0, DAY_MIN, by = 60), cma = seq(0, 1, length.out = 25))
img <- file.path(tempdir(), "welch_caption_check")
dir.create(img, showWarnings = FALSE, recursive = TRUE)
on.exit(unlink(img, recursive = TRUE), add = TRUE)
p <- plot_welch(cma, 0L, n_reps = 7L, n_days = 90L, images_dir = img)
report(grepl("^7 replications", p$labels$subtitle),
       "the Welch caption names the replication count it is given, not another")

# ── 8. The Welch analysis hands its plot the realised count ─────────────────

welch_src <- paste(deparse(body(run_welch_analysis)), collapse = " ")
report(grepl("n_reps\\s*=\\s*mon\\$n_replications", welch_src),
       "run_welch_analysis() passes the realised count to plot_welch()")
report(!grepl("plot_welch\\([^)]*n_reps\\s*=\\s*n_reps", welch_src),
       "run_welch_analysis() does not pass its requested count to plot_welch()")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All replication loss reporting checks passed.\n")
quit(status = 0)
