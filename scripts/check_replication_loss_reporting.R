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
# Why this check exists. run_replications() tolerates a replication whose
# worker process is killed, continuing on the survivors, and that tolerance is
# correct: losing one replication of fifty costs precision and nothing else,
# and stopping would waste the other forty-nine. What was not correct is what
# reached the outputs. The requested count was returned and published while the
# metrics were computed from the survivors, so a run that lost workers reported
# a replication count it never achieved, and every interval beside it was read
# against that number (Issue #320).
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
#   2. A loss inside the threshold warns, drops only the failed entries, and
#      reports the realised count rather than the requested one.
#   3. A loss beyond the threshold stops the run rather than returning a
#      smaller measurement of a different experiment.
#   4. A total loss stops, which the threshold subsumes.
#   5. A figure's caption names the count it was given, so the realised count
#      passed to it is the count a reader sees.
#   6. The Welch analysis hands its plot the realised count rather than the
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
#' @details Twenty, so that a single loss sits inside the threshold and two
#'   more carry it beyond, letting both sides of the rule be exercised without
#'   depending on the threshold's exact value.
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

# ── 2. A loss inside the threshold warns and drops only the failures ────────

n_small <- 1L
warned  <- FALSE
inside  <- withCallingHandlers(
  drop_failed_replications(dispatch_with_losses(N_DISPATCH, n_small), N_DISPATCH),
  warning = function(w) {
    warned <<- TRUE
    invokeRestart("muffleWarning")
  }
)
report(warned, "a survivable loss warns rather than passing silently")
report(length(inside$envs) == N_DISPATCH - n_small,
       "a survivable loss keeps the %d survivors, not the %d requested",
       N_DISPATCH - n_small, N_DISPATCH)
report(sum(!inside$valid) == n_small,
       "the valid vector marks exactly the %d lost replication(s)", n_small)

# ── 3. A loss beyond the threshold stops the run ────────────────────────────

n_large <- as.integer(ceiling(N_DISPATCH * MAX_REPLICATION_LOSS)) + 1L
beyond  <- try(
  suppressWarnings(
    drop_failed_replications(dispatch_with_losses(N_DISPATCH, n_large), N_DISPATCH)
  ),
  silent = TRUE
)
report(inherits(beyond, "try-error"),
       "losing %d of %d, beyond the %.0f%% threshold, stops rather than reporting",
       n_large, N_DISPATCH, 100 * MAX_REPLICATION_LOSS)

# ── 4. A total loss stops ───────────────────────────────────────────────────

total <- try(
  suppressWarnings(
    drop_failed_replications(dispatch_with_losses(N_DISPATCH, N_DISPATCH), N_DISPATCH)
  ),
  silent = TRUE
)
report(inherits(total, "try-error"), "losing every replication stops")

# ── 5. The framework reports the count that contributed ─────────────────────

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

# ── 6. A caption names the count it is given ────────────────────────────────

cma <- data.frame(bin_min = seq(0, DAY_MIN, by = 60), cma = seq(0, 1, length.out = 25))
img <- file.path(tempdir(), "welch_caption_check")
dir.create(img, showWarnings = FALSE, recursive = TRUE)
on.exit(unlink(img, recursive = TRUE), add = TRUE)
p <- plot_welch(cma, 0L, n_reps = 7L, n_days = 90L, images_dir = img)
report(grepl("^7 replications", p$labels$subtitle),
       "the Welch caption names the replication count it is given, not another")

# ── 7. The Welch analysis hands its plot the realised count ─────────────────

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
