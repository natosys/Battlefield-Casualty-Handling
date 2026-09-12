#!/usr/bin/env Rscript
##############################################################################
## scripts/check_time_series_figures.R                                      ##
## Regression check — the campaign time series, and the claims the paper    ##
## makes from them, agree with the tracked measurement                      ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_time_series_figures.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The two campaign time series settle a planning
# question the paper's summary statistics cannot: whether a queue recurs in
# peaks and clears between them, which surge capability answers, or never
# clears, which only establishment does. The paper states that answer as a
# percentage in its prose, the figure states it again in a panel annotation,
# and both are derived from data/time_series/queue_clearance.csv. Nothing
# recomputes either from the other. A re-measurement that lands a new series
# without the prose being rewritten would leave the paper asserting a
# clearance share the evidence no longer supports, which is the same drift
# that left the tracked Morris plots describing a different screen from the
# tracked rankings (Issue #342's precedent, closed for that case by
# scripts/render_morris_plots.R).
#
# What this asserts:
#
#   1. The tracked series carry every pool and stage the model defines, at the
#      replication count and horizon the paper names, for both intensities.
#   2. The clearance statistics are internally consistent: a share lies in
#      [0, 1], a longest busy spell lies in [0, horizon], and a pool that is
#      empty for the whole campaign has no busy spell.
#   3. Every clearance percentage the paper states in prose matches the
#      tracked measurement, rounded as the paper rounds it.
#   4. The step-function estimators the series rest on are correct on inputs
#      whose answers can be computed by hand, so a series that agrees with
#      the paper is not merely two copies of the same error.
#
# Assertion 4 is what keeps the first three from being circular. The rest
# compare two derived artifacts against one another; only this one checks the
# estimator against arithmetic.

source("R/constants.R")
suppressPackageStartupMessages({
  library(dplyr)
})
source("R/analysis.R")

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

#' Directory holding the tracked campaign series
SERIES_DIR <- file.path("data", "time_series")

#' The paper the clearance percentages are quoted in
PAPER_PATH <- file.path("docs", "Multi_Run_Analysis.md")

#' Campaign length the tracked series was measured over, in days
HORIZON_DAYS <- 30

#' Replications the tracked series was measured at, per intensity
N_REPLICATIONS <- 50

#' Tolerance on a floating point comparison of two computed reals
TOL <- 1e-9

# ── 1. The tracked series is complete ────────────────────────────────────────

cat("\n-- the tracked series covers every pool, stage and intensity --\n")

queue     <- read.csv(file.path(SERIES_DIR, "queue_series.csv"), stringsAsFactors = FALSE)
clearance <- read.csv(file.path(SERIES_DIR, "queue_clearance.csv"), stringsAsFactors = FALSE)
degraded  <- read.csv(file.path(SERIES_DIR, "degraded_care_series.csv"),
                      stringsAsFactors = FALSE)

intensities <- c("Moderate intensity", "High intensity")

report(setequal(unique(clearance$pool), names(TIME_SERIES_POOLS)),
       "the clearance table carries every pool TIME_SERIES_POOLS defines (%d)",
       length(TIME_SERIES_POOLS))
report(setequal(unique(degraded$stage), names(PATHWAY_STAGES)),
       "the degraded-care series carries every stage PATHWAY_STAGES defines (%d)",
       length(PATHWAY_STAGES))
report(setequal(unique(queue$intensity), intensities) &&
         setequal(unique(clearance$intensity), intensities) &&
         setequal(unique(degraded$intensity), intensities),
       "all three series carry both casualty intensities")

reps_per_arm <- clearance %>%
  count(intensity, pool, name = "n_reps")
report(all(reps_per_arm$n_reps == N_REPLICATIONS),
       "every intensity and pool carries %d replications (found %s)",
       N_REPLICATIONS, paste(sort(unique(reps_per_arm$n_reps)), collapse = ", "))

report(abs(max(queue$bin_start_day) - (HORIZON_DAYS - TIME_SERIES_BIN_MIN / DAY_MIN)) < TOL,
       "the queue series runs to the %d-day horizon the paper names", HORIZON_DAYS)
report(max(degraded$day) == HORIZON_DAYS,
       "the degraded-care series runs to day %d", HORIZON_DAYS)

# ── 2. The clearance statistics are internally consistent ────────────────────

cat("\n-- the clearance statistics are self-consistent --\n")

report(all(clearance$zero_share >= 0 & clearance$zero_share <= 1),
       "every zero share lies in [0, 1]")
report(all(clearance$longest_busy_days >= 0 &
             clearance$longest_busy_days <= HORIZON_DAYS + TOL),
       "every longest busy spell lies in [0, %d] days", HORIZON_DAYS)

never_busy <- clearance %>% filter(zero_share >= 1 - TOL)
report(nrow(never_busy) == 0 || all(never_busy$longest_busy_days < TOL),
       "a pool empty for the whole campaign records no busy spell (%d such rows)",
       nrow(never_busy))

always_busy <- clearance %>% filter(zero_share <= TOL)
report(nrow(always_busy) == 0 ||
         all(abs(always_busy$longest_busy_days - HORIZON_DAYS) < 1e-6),
       "a pool never empty records a busy spell of the whole campaign (%d such rows)",
       nrow(always_busy))

report(all(clearance$longest_busy_days <=
             (1 - clearance$zero_share) * HORIZON_DAYS + 1e-6),
       "no busy spell exceeds the total time the pool was busy")

# ── 3. The paper's stated percentages match the measurement ──────────────────

cat("\n-- the paper's clearance claims match the tracked measurement --\n")

paper <- readLines(PAPER_PATH, warn = FALSE)

medians <- clearance %>%
  group_by(intensity, pool) %>%
  summarise(median_zero_share = median(zero_share), .groups = "drop")

#' Every clearance claim the paper states, as intensity, pool and percentage
#'
#' @return Data frame of intensity, pool and stated_pct, one row per claim.
#'
#' @details A claim is written in the paper as an HTML comment naming the
#'   intensity and pool, immediately followed by the sentence stating the
#'   figure. Marking the claims rather than parsing the prose for percentages
#'   is deliberate: prose changes, and a check that guesses which number in a
#'   paragraph is the measured one fails for reasons that have nothing to do
#'   with the measurement. The marker is the author's statement of what the
#'   sentence beneath it is claiming.
paper_claims <- function() {
  marks <- grep("^<!-- CLEARANCE ", paper)
  if (length(marks) == 0) return(NULL)
  bind_rows(lapply(marks, function(i) {
    parts <- regmatches(paper[i],
                        regexec("^<!-- CLEARANCE ([^|]+)\\|([^|]+)\\|([0-9.]+) -->$",
                                paper[i]))[[1]]
    if (length(parts) != 4) {
      fail("unparseable clearance marker at %s line %d: %s", PAPER_PATH, i, paper[i])
      return(NULL)
    }
    data.frame(intensity = trimws(parts[2]), pool = trimws(parts[3]),
               stated_pct = as.numeric(parts[4]))
  }))
}

claims <- paper_claims()
report(!is.null(claims) && nrow(claims) > 0,
       "the paper carries at least one marked clearance claim (%d found)",
       if (is.null(claims)) 0L else nrow(claims))

if (!is.null(claims) && nrow(claims) > 0) {
  checked <- claims %>% left_join(medians, by = c("intensity", "pool"))
  report(all(!is.na(checked$median_zero_share)),
         "every marked claim names a pool and intensity the series measures")
  matched <- checked %>%
    filter(!is.na(median_zero_share)) %>%
    mutate(measured_pct = round(100 * median_zero_share),
           agrees = abs(measured_pct - stated_pct) < TOL)
  report(all(matched$agrees),
         "every marked claim matches the measurement (%d of %d agree)",
         sum(matched$agrees), nrow(matched))
  for (row in which(!matched$agrees)) {
    fail("%s / %s: paper states %.0f%%, series measures %.0f%%",
         matched$intensity[row], matched$pool[row],
         matched$stated_pct[row], matched$measured_pct[row])
  }
}

# ── 4. The estimators are correct on inputs computable by hand ───────────────

cat("\n-- the step-function estimators are correct on known inputs --\n")

#' One pool's monitor rows, as the resource monitor writes them
#'
#' @param resource Resource name the rows belong to.
#' @param time Times the queue changed.
#' @param queue Queue depth from each of those times.
#' @return Data frame with the columns pool_queue_steps() reads.
monitor_rows <- function(resource, time, queue) {
  data.frame(resource = resource, time = time, queue = queue)
}

# Two beds, each queueing one casualty over an interval that overlaps the
# other's: the pool total is 1 from 10 to 20, 2 from 20 to 30, 1 from 30 to 40
# and 0 thereafter.
two_beds <- bind_rows(
  monitor_rows("bed_1", c(0, 10, 30), c(0, 1, 0)),
  monitor_rows("bed_2", c(0, 20, 40), c(0, 1, 0))
)
steps <- pool_queue_steps(two_beds)
report(identical(steps$time, c(0, 10, 20, 30, 40)) &&
         identical(steps$total, c(0, 1, 2, 1, 0)),
       "the pool total recovers a known two-bed overlap exactly")

# Over 0 to 100 the area under that total is 10*1 + 10*2 + 10*1 = 40, so the
# mean over one 100-minute bin is 0.40. All of that area falls before minute
# 50, so over two 50-minute bins the means are 0.80 and 0.
report(abs(step_bin_means(steps, c(0, 100)) - 0.40) < TOL,
       "the bin mean over one bin is the time-weighted mean (0.40)")
report(all(abs(step_bin_means(steps, c(0, 50, 100)) - c(0.80, 0.00)) < TOL),
       "the bin means over two bins are 0.80 and 0")

# The pool is busy from 10 to 40 and empty otherwise, so 70 of 100 minutes are
# empty and the single busy spell is 30 minutes long.
stats <- step_clearance_stats(steps, 100)
report(abs(stats[["zero_share"]] - 0.70) < TOL,
       "the zero share of a known series is 0.70")
report(abs(stats[["longest_busy_min"]] - 30) < TOL,
       "the longest busy spell of a known series is 30 minutes")

# A pool busy in two separate spells reports the longer, not their sum: busy
# from 10 to 20 and from 50 to 80, so the longest spell is 30 and not 40.
split_steps <- pool_queue_steps(
  monitor_rows("bed_1", c(0, 10, 20, 50, 80), c(0, 1, 0, 1, 0))
)
split_stats <- step_clearance_stats(split_steps, 100)
report(abs(split_stats[["longest_busy_min"]] - 30) < TOL,
       "two separate busy spells report the longer (30), not their total (40)")
report(abs(split_stats[["zero_share"]] - 0.60) < TOL,
       "two separate busy spells leave a zero share of 0.60")

# A pool that never queues is empty throughout and has no busy spell.
idle_stats <- step_clearance_stats(pool_queue_steps(
  monitor_rows("bed_1", c(0, 10, 20), c(0, 0, 0))
), 100)
report(abs(idle_stats[["zero_share"]] - 1) < TOL &&
         abs(idle_stats[["longest_busy_min"]]) < TOL,
       "a pool that never queues is empty throughout with no busy spell")

# Sampling the series at the bin edges rather than integrating over the bin
# would miss a peak entirely. A queue of 10 lasting from 20 to 30 inside a
# single 0 to 100 bin contributes a mean of 1.0; an edge sample reads 0.
peak_steps <- pool_queue_steps(monitor_rows("bed_1", c(0, 20, 30), c(0, 10, 0)))
report(abs(step_bin_means(peak_steps, c(0, 100)) - 1.0) < TOL,
       "a peak entirely inside one bin is carried by the bin mean, not missed")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All campaign time series checks passed.\n")
quit(status = 0)
