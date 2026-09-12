#!/usr/bin/env Rscript
##############################################################################
## scripts/check_replication_memory.R                                       ##
## Regression check — peak memory does not grow with the replication count  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_replication_memory.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Building the casualty trajectory retains roughly
# 175 MB that neither releasing it nor gc() reclaims. The retention is a
# per-activity cost in simmer rather than in this project's code: a minimal
# trajectory of nothing but timeouts retains a constant 6.0 R cons cells per
# activity, linear from 250 to 8,000 activities, and the casualty trajectory
# carries 12,612 of them (Issue #312).
#
# That cost is survivable only if a forked worker runs one replication and
# exits, returning its retention to the operating system. Under mclapply's
# default the jobs are pre-divided into one batch per core and each fork runs
# its whole batch, so the retention accumulates and peak memory grows with the
# replication count: measured at 30 days on two cores, peak tree RSS ran
# 1,340 MB at 4 replications and 2,084 MB at 8, against 884 MB and 796 MB with
# one fork per job. At 50 replications the first arrangement is what has been
# exhausting memory on long and wide runs.
#
# What it asserts:
#
#   1. dispatch_replications() asks mclapply for one fork per job, structurally.
#   2. Peak memory across the process tree does not grow with the number of
#      jobs dispatched. Asserted behaviourally, against a worker that retains
#      a known amount and then exits, so the check measures the dispatch
#      arrangement rather than the model, and runs in seconds rather than
#      minutes.
#   3. The dispatch still returns one result per job, in order, so the memory
#      arrangement has not been bought by losing or reordering work.

suppressPackageStartupMessages({
  library(parallel)
})

source("R/replication.R")

#' Megabytes each synthetic worker retains before it returns
#'
#' @details Large enough that an accumulating fork is separable from a fork
#'   that releases, small enough that the check cannot itself exhaust a
#'   constrained host at the larger job count.
WORKER_MB <- 60L

#' Job counts the peak is compared across
#'
#' @details The larger is four times the smaller, so accumulation shows as a
#'   multiple rather than as a margin that a noisy host could produce.
JOB_COUNTS <- c(4L, 16L)

#' Cores the dispatch is held to, so the comparison is not confounded by a
#' different fork width at each job count
#'
#' @details Applied through the mc.cores option rather than through the
#'   max_cores argument, because max_cores selects the capped caller's path.
#'   That path already forked per job, so a check that used it would exercise
#'   the arrangement this defect never applied to and pass whatever the
#'   uncapped path did.
CHECK_CORES <- 2L

#' Fraction of the smaller run's peak the larger run may exceed it by
#'
#' @details Peak resident memory is noisy, and a forked child shares pages with
#'   its parent until it writes, so the two runs are not expected to match
#'   exactly. Growth proportional to the job count, which is what this check
#'   exists to catch, is a factor of four here and clears this bound by a wide
#'   margin.
PEAK_TOLERANCE <- 0.60

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
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

#' Summed resident memory of one process and its children, in megabytes
#'
#' @param pid Process whose tree is measured.
#' @return Megabytes, or NA where the platform does not report it.
#' @details Read from ps rather than from gc(), because the retention this
#'   check is about is returned to the operating system by a process exiting
#'   and is therefore invisible to R's own accounting. The pid is passed in
#'   rather than taken from Sys.getpid(), because the sampler that calls this
#'   is itself a forked child and would otherwise measure its own tree, which
#'   is empty, and pass whatever the dispatch did.
tree_rss_mb <- function(pid) {
  out <- suppressWarnings(system2("ps", c("-o", "rss=", "-p", pid, "--ppid", pid),
                                  stdout = TRUE, stderr = FALSE))
  if (length(out) == 0) return(NA_real_)
  sum(as.numeric(out), na.rm = TRUE) / 1024
}

#' Peak tree memory while dispatching n retaining jobs
#'
#' @param n Number of jobs to dispatch.
#' @return Peak megabytes observed across the dispatch.
#' @details The worker holds WORKER_MB and returns its index, standing in for
#'   a replication that retains and then exits. Sampling from the parent while
#'   mclapply blocks is not possible, so the peak is taken from a forked
#'   sampler running alongside it.
peak_during_dispatch <- function(n) {
  sample_file <- tempfile("peak-")
  parent_pid  <- Sys.getpid()
  sampler <- parallel::mcparallel({
    best <- 0
    repeat {
      best <- max(best, tree_rss_mb(parent_pid), na.rm = TRUE)
      writeLines(format(best), sample_file)
      Sys.sleep(0.05)
    }
  })
  on.exit(tools::pskill(sampler$pid), add = TRUE)

  # Retained in a process global rather than in a local, so that it survives
  # the job as the real retention does and is released only when the fork
  # exits. A local would be reclaimed between jobs in the same fork and no
  # arrangement would ever accumulate.
  #' Retain WORKER_MB and return the job index
  #'
  #' @param i Index of the job.
  #' @return The index, so the caller can assert nothing was lost or reordered.
  worker <- function(i) {
    ballast <- rep(0.0, WORKER_MB * 1024 * 1024 / 8)
    ballast[1] <- i
    held <- c(get0("replication_memory_ballast", globalenv(), ifnotfound = list()),
              list(ballast))
    assign("replication_memory_ballast", held, envir = globalenv())
    i
  }
  old_cores <- getOption("mc.cores")
  options(mc.cores = CHECK_CORES)
  on.exit(options(mc.cores = old_cores), add = TRUE)
  results <- dispatch_replications(worker, n, NULL)
  Sys.sleep(0.2)
  peak <- if (file.exists(sample_file)) as.numeric(readLines(sample_file)[1]) else NA_real_
  list(peak = peak, results = results)
}

cat("Replication memory check\n\n")

# ── 1. The dispatch asks for one fork per job ──────────────────────────────

cat("-- one fork per job --\n")

src <- paste(deparse(dispatch_replications), collapse = " ")
report(grepl("mc.preschedule\\s*=\\s*FALSE", src),
       "dispatch_replications() passes mc.preschedule = FALSE, so mclapply forks per job")
report(!grepl("mc.preschedule\\s*=\\s*is.null", src),
       "the per-job arrangement is unconditional, not limited to capped callers")

# ── 2 and 3. Peak does not grow, and no work is lost ───────────────────────

cat("\n-- peak memory against job count --\n")

if (.Platform$OS.type == "windows") {
  report(TRUE, "skipped on Windows, where the dispatch does not fork")
} else {
  small <- peak_during_dispatch(JOB_COUNTS[1])
  large <- peak_during_dispatch(JOB_COUNTS[2])

  report(!is.na(small$peak) && !is.na(large$peak),
         "peak memory was observable for both runs (%.0f MB, %.0f MB)",
         small$peak, large$peak)

  bound <- small$peak * (1 + PEAK_TOLERANCE)
  report(!is.na(large$peak) && large$peak <= bound,
         paste("%d jobs peak at %.0f MB against %.0f MB for %d, inside the %.0f MB bound;",
               "accumulation would put it near %.0f MB"),
         JOB_COUNTS[2], large$peak, small$peak, JOB_COUNTS[1], bound,
         small$peak * JOB_COUNTS[2] / JOB_COUNTS[1])

  for (r in list(list(n = JOB_COUNTS[1], d = small), list(n = JOB_COUNTS[2], d = large))) {
    got <- suppressWarnings(as.numeric(unlist(r$d$results)))
    report(length(got) == r$n && isTRUE(all.equal(got, as.numeric(seq_len(r$n)))),
           "%d jobs returned %d results in order", r$n, length(got))
  }
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All replication memory checks passed.\n")
quit(status = 0)
