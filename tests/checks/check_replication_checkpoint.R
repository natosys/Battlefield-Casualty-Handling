#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_replication_checkpoint.R                              ##
## Regression check — a measurement resumed from its checkpoints writes     ##
## what an uninterrupted one writes                                         ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_replication_checkpoint.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Reproducing the tracked evidence takes days of
# computation (docs/Methods.md, The Cost of Reproducing the Evidence), so every
# experiment runner saves each finished replication, or for the transport sweep
# each finished fleet-size point, and reads it back after an interruption rather
# than starting again. A resume is only acceptable if it cannot change the
# result: a checkpoint read back under the wrong seed, a partial file left by a
# killed worker, or a skipped point that shifted the random stream of the points
# after it would each write a different evidence set without any error. This
# check interrupts real measurements at small sizes and asserts that the resumed
# result is identical to an uninterrupted one.
#
# What this asserts:
#
#   1. checkpoint_replication() computes without saving when given no
#      directory, saves under a name carrying index, seed and horizon, reads a
#      saved result back without computing it again, and leaves no temporary
#      file behind.
#   2. A replicated measurement run through it is identical with and without
#      checkpoints, and identical again when one replication's file is removed
#      (the replication in flight at an interruption) and the run resumed; the
#      surviving files are read back, not recomputed.
#   3. The transport sweep resumed with one point already saved is identical to
#      the uninterrupted sweep, so a skipped point does not move the points
#      after it.
#   4. Every module function behind the six runners made resumable here
#      takes a checkpoint directory, and every one of those runners passes one.

suppressPackageStartupMessages({
  library(simmer)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/cli.R")
source("R/hold_window.R")
source("R/long_horizon.R")
source("R/role4_demand.R")
source("R/icu_gate.R")
source("R/casualty_surge.R")

#' Replications in each small measurement
CHECK_REPS <- 3L

#' Campaign length of each small measurement, in days
CHECK_DAYS <- 10L

#' Control seed every measurement is set to before it runs
CHECK_SEED <- 605L

#' The runners made resumable and the module function each calls
RUNNERS <- c(
  "scripts/run_long_horizon.R"    = "run_long_horizon",
  "scripts/run_role4_demand.R"    = "run_role4_demand",
  "scripts/run_hold_window.R"     = "run_hold_window_measurement",
  "scripts/run_icu_gate.R"        = "run_icu_gate_measurement",
  "scripts/run_casualty_surge.R"  = "run_casualty_surge_measurement",
  "scripts/run_transport_sweep.R" = "plot_transport_capacity_margin_by_fleet_size"
)

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

#' A fresh empty directory under the session's temporary directory
#'
#' @param name Directory name.
#' @return The directory's path.
fresh_dir <- function(name) {
  path <- file.path(tempdir(), name)
  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE)
  path
}

# ── 1. The helper ───────────────────────────────────────────────────────────

cat("-- checkpoint_replication() --\n")

calls <- new.env(parent = emptyenv())
calls$n <- 0L
#' A computation that counts how often it runs
#'
#' @return A one-row data frame.
counted <- function() {
  calls$n <- calls$n + 1L
  data.frame(value = 7)
}
unsaved <- checkpoint_replication(counted, NULL, 1L, 99L, 30L)
report(calls$n == 1L && identical(unsaved, data.frame(value = 7)),
       "with no directory the computation runs and nothing is saved")
dir_1 <- fresh_dir("ckpt_helper")
first <- checkpoint_replication(counted, dir_1, 4L, 99L, 30L)
report(file.exists(file.path(dir_1, "replication_004_99_d30.rds")),
       "the saved file is named by index, seed and horizon")
again <- checkpoint_replication(counted, dir_1, 4L, 99L, 30L)
report(calls$n == 2L && identical(first, again),
       "a saved result is read back without computing it again (%d computations)", calls$n)
report(length(list.files(dir_1, pattern = "\\.tmp$")) == 0L,
       "no temporary file is left behind")

# ── 2. A replicated measurement, interrupted and resumed ────────────────────

cat("\n-- a measurement resumed from its checkpoints --\n")

json_data <- read_config("env_data.json")
apply_hold_window_setting(json_data, "default", 60L)

#' Run the small hold window measurement at the control seed
#'
#' @param checkpoint Checkpoint directory, or NULL.
#' @return The per-replication responses.
measure <- function(checkpoint) {
  set.seed(CHECK_SEED)
  invisible(capture.output(suppressMessages(
    rows <- run_hold_window_measurement(60L, n_iterations = CHECK_REPS, n_days = CHECK_DAYS,
                                        checkpoint_dir = checkpoint)
  )))
  rows
}

plain <- measure(NULL)
dir_2 <- fresh_dir("ckpt_measure")
saved_run <- measure(dir_2)
files <- sort(list.files(dir_2, pattern = "^replication_.*\\.rds$", full.names = TRUE))
report(identical(plain, saved_run) && length(files) == CHECK_REPS,
       "a checkpointed run equals an uninterrupted one and saves all %d replications",
       CHECK_REPS)

unlink(files[2])
resumed <- measure(dir_2)
report(identical(plain, resumed) && file.exists(files[2]),
       "with the second replication's file removed, the resumed run is identical")

marked <- readRDS(files[1])
marked$total_casualties <- -1L
saveRDS(marked, files[1])
tampered <- measure(dir_2)
report(tampered$total_casualties[tampered$replication == 1L] == -1 &&
         identical(tampered[tampered$replication != 1L, ], plain[plain$replication != 1L, ]),
       "a surviving file is read back, not recomputed")

# ── 3. The transport sweep, resumed at a point ──────────────────────────────

cat("\n-- the transport sweep resumed at a point --\n")

#' Run the small transport sweep over two fleet sizes
#'
#' @param checkpoint Checkpoint directory, or NULL.
#' @return The sweep's summary rows.
sweep <- function(checkpoint) {
  set.seed(CHECK_SEED)
  out_dir <- fresh_dir("ckpt_sweep_out")
  invisible(capture.output(suppressMessages(
    res <- plot_transport_capacity_margin_by_fleet_size(
      fleet_sizes = list(PMVAmb = 2:3), n_days = CHECK_DAYS, n_rep = 2L,
      output_dir = out_dir, images_dir = out_dir, checkpoint_dir = checkpoint
    )
  )))
  res$data
}

whole <- sweep(NULL)
dir_3 <- fresh_dir("ckpt_sweep")
first_points <- sweep(dir_3)
points <- list.files(dir_3, pattern = "^point_.*\\.rds$", full.names = TRUE)
report(identical(whole, first_points) && length(points) == 2L,
       "a checkpointed sweep equals an uninterrupted one and saves both points")
unlink(points[grepl("PMVAmb_3_", points)])
resumed_sweep <- sweep(dir_3)
report(identical(whole, resumed_sweep),
       "resumed with the first point saved, the second point is unchanged")

# ── 4. Every runner is resumable ────────────────────────────────────────────

cat("\n-- every runner passes a checkpoint directory --\n")

for (script in names(RUNNERS)) {
  fn <- RUNNERS[[script]]
  body <- paste(readLines(script, warn = FALSE), collapse = "\n")
  report("checkpoint_dir" %in% names(formals(get(fn))) &&
           grepl("checkpoint_dir = ", body, fixed = TRUE),
         "%s passes a checkpoint directory to %s()", script, fn)
}

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nA measurement resumed from its checkpoints writes what an uninterrupted one writes.\n")
quit(status = 0L)
