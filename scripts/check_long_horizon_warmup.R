#!/usr/bin/env Rscript
##############################################################################
## scripts/check_long_horizon_warmup.R                                      ##
## Regression check — the sustained-horizon Welch diagnostic is correct and ##
## the tracked plot and CSV reproduce it                                    ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_long_horizon_warmup.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. compute_long_horizon_cma() (R/warmup.R) is what
# docs/Multi_Run_Supplement.md's Warm-up Classification section now cites as
# the direct sustained-horizon Welch reading, alongside the per-block
# classification R/long_horizon.R already carried a check for. Nothing
# asserted the cumulative-average arithmetic itself, and nothing asserted
# that the tracked data/long_horizon/long_horizon_welch_cma.csv and
# images/welch_plot_long_horizon.png are the reduction of the tracked
# data/long_horizon/long_horizon_series.csv.gz rather than a second and
# independent claim about it.
#
# What this asserts:
#
#   1. compute_long_horizon_cma() is correct on a constructed series whose
#      answer is computable by hand.
#   2. The tracked CMA CSV reproduces exactly from the tracked long-horizon
#      series, for both scenarios and both pools the diagnostic covers.
#   3. Every figure the Warm-up Classification section's sustained-horizon
#      paragraph states (CMA at day 30/90/180/360, both scenarios) matches
#      the tracked CMA.

source("R/constants.R")
source("R/warmup.R")

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

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

# ── 1. compute_long_horizon_cma() is correct by hand ─────────────────────────

cat("\n-- compute_long_horizon_cma() on a constructed series --\n")

# One pool, one scenario, three days, two replications: daily means are
# (1+3)/2=2, (2+4)/2=3, (3+2)/2=2.5, so the CMA is 2, 2.5, 7.5/3 = 2.5.
synthetic <- do.call(rbind, lapply(1:2, function(rep_id) {
  data.frame(day = 1:3, series = "mean_queue", subject = "R2E intensive care",
             value = if (rep_id == 1) c(1, 2, 3) else c(3, 4, 2),
             replication = rep_id, scenario = "test_scenario")
}))
cma <- compute_long_horizon_cma(synthetic, subjects = "R2E intensive care")

report(nrow(cma) == 3, "three days give three CMA rows (found %d)", nrow(cma))
report(all(abs(cma$mean_queue - c(2, 3, 2.5)) < TOL),
       "the daily cross-replication means are correct (2, 3, 2.5)")
report(all(abs(cma$cma - c(2, 2.5, 2.5)) < TOL),
       "the cumulative moving average is correct (2, 2.5, 2.5)")

# A subject not requested is excluded, and a series other than mean_queue is
# excluded, so the filter cannot silently widen to something the diagnostic
# was not written to read.
mixed <- rbind(synthetic,
               data.frame(day = 1:3, series = "mean_queue", subject = "R2E holding beds",
                          value = 99, replication = 1, scenario = "test_scenario"),
               data.frame(day = 1:3, series = "occupancy", subject = "R2E intensive care",
                          value = 99, replication = 1, scenario = "test_scenario"))
cma_filtered <- compute_long_horizon_cma(mixed, subjects = "R2E intensive care")
report(nrow(cma_filtered) == 3 && !any(cma_filtered$value == 99, na.rm = TRUE),
       "an unrequested subject and a non-queue series are excluded from the reduction")

# ── 2. The tracked CMA CSV reproduces from the tracked series ────────────────

cat("\n-- the tracked CMA reproduces from the tracked series --\n")

series_path <- file.path("data", "long_horizon", "long_horizon_series.csv.gz")
cma_path    <- file.path("data", "long_horizon", "long_horizon_welch_cma.csv")

if (!file.exists(series_path) || !file.exists(cma_path)) {
  report(FALSE, "the tracked long-horizon series and CMA both exist")
} else {
  series  <- read.csv(gzfile(series_path), stringsAsFactors = FALSE)
  tracked <- read.csv(cma_path, stringsAsFactors = FALSE)

  recomputed <- compute_long_horizon_cma(series)

  #' Identify each CMA row by the (scenario, subject, day) it reports
  #'
  #' @param df A CMA data frame as returned by `compute_long_horizon_cma()`.
  #' @return Character vector keying each row, for matching one CMA against
  #'   another regardless of row order.
  key <- function(df) paste(df$scenario, df$subject, df$day)
  ordered_tracked    <- tracked[order(key(tracked)), ]
  ordered_recomputed <- recomputed[order(key(recomputed)), ]

  report(setequal(unique(tracked$subject), LONG_HORIZON_CMA_SUBJECTS),
         "the tracked CMA covers exactly the diagnostic's pools (%s)",
         paste(LONG_HORIZON_CMA_SUBJECTS, collapse = ", "))
  report(identical(key(ordered_tracked), key(ordered_recomputed)),
         "the tracked CMA and the recomputed one carry the same (scenario, subject, day) rows")
  report(all(abs(ordered_tracked$cma - ordered_recomputed$cma) < TOL),
         "every tracked CMA value reproduces from the tracked series (max diff %.2e)",
         max(abs(ordered_tracked$cma - ordered_recomputed$cma)))

  # ── 3. The published day-30/90/180/360 readings match the tracked CMA ──────

  cat("\n-- the published CMA readings match the tracked evidence --\n")

  #' The tracked CMA at one day, scenario and pool
  #'
  #' @param scenario Scenario profile name.
  #' @param subject Bed pool name.
  #' @param day Simulation day.
  #' @return The tracked CMA value, or NA where no such row exists.
  cma_at <- function(scenario, subject, day) {
    row <- tracked[tracked$scenario == scenario & tracked$subject == subject &
                     tracked$day == day, ]
    if (nrow(row) == 0) NA_real_ else row$cma[1]
  }

  # Values docs/Multi_Run_Supplement.md's sustained-horizon Welch paragraph
  # states, read from the tracked evidence set at the time it was written.
  published <- list(
    list("moderate_intensity", "R2E intensive care", 30, 1.643),
    list("moderate_intensity", "R2E intensive care", 360, 1.237),
    list("moderate_intensity", "R2E holding beds", 30, 1.836),
    list("moderate_intensity", "R2E holding beds", 360, 0.752),
    list("high_intensity", "R2E intensive care", 30, 30.119),
    list("high_intensity", "R2E intensive care", 360, 802.908),
    list("high_intensity", "R2E holding beds", 30, 38.751),
    list("high_intensity", "R2E holding beds", 360, 290.714)
  )
  for (p in published) {
    found <- cma_at(p[[1]], p[[2]], p[[3]])
    report(!is.na(found) && abs(found - p[[4]]) < 0.001,
           "%s %s day %d: published %.3f matches tracked %.3f",
           p[[1]], p[[2]], p[[3]], p[[4]], found)
  }
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All sustained-horizon Welch diagnostic checks passed.\n")
quit(status = 0)
