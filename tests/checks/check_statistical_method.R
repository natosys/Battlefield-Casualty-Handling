#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_statistical_method.R                                  ##
## Regression check — the Holm adjustment and the per-replication stability ##
## classification are correct, and every tracked paired file is adjusted    ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_statistical_method.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Two estimators decide what the planning paper may call
# measured. A paired difference counts as resolved only where it survives the
# Holm adjustment within its experiment (holm_adjust(), R/analysis_common.R),
# so that a family of comparisons cannot read one chance effect in twenty as
# established; and a pool is classified converged or drifting from one late-half
# slope per replication with an interval across replications
# (classify_stability(), R/long_horizon.R), the replication being the unit of
# analysis, rather than from a fit across serially correlated block means. Each
# replaced an estimator that gave plausible numbers, so an error in either would
# pass unnoticed through every protocol check that compares a file with the code
# that wrote it.
#
# What this asserts:
#
#   1. holm_adjust() reproduces Holm's step-down values on p-values computable
#      by hand, keeps a row with no p-value out of the family, and refuses a
#      file without one.
#   2. Every tracked paired-difference file carries a `p_holm` column equal to
#      the Holm adjustment of its own `p_value` column.
#   3. On a constructed series whose replications trend at different rates, the
#      late slope is the mean of the per-replication slopes and its interval the
#      Student t interval across them, so a response a fit to the mean series
#      would call drifting is converged; and replications trending together
#      are drifting.
#   4. Given a single replication, the classification falls back to the fit to
#      that replication's own block means.
#   5. The late window is the middle block and every block after it, blocks 6
#      to 12 of the protocol's twelve, as the methods paper's marker states: a
#      series flat from block 7 that rises into it from block 6 is drifting,
#      and its late level counts block 6. A classification whose interval bound
#      lies near zero can change with the window's first block, so the paper
#      and the code are held to one window.

source("R/analysis_common.R")
source("R/long_horizon.R")

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

#' Tolerance on a comparison of two computed reals
TOL <- 1e-10

#' Directory of the tracked evidence sets
DATA_DIR <- "data"

#' Block length of the constructed series, in days
BLOCK_DAYS <- 30L

#' Blocks in the constructed series
N_BLOCKS <- 12L

#' The methods paper, whose marker states the late window
METHODS <- "docs/Methods.md"

# ── 1. holm_adjust() on hand-computed p-values ──────────────────────────────

cat("-- the Holm adjustment on hand-computed p-values --\n")

# Ordered 0.005, 0.01, 0.03, 0.04 with m = 4: multiplied by 4, 3, 2 and 1 they are
# 0.02, 0.03, 0.06 and 0.04, and the running maximum lifts the last to 0.06. The
# NA row is not a test and does not count in m.
toy <- data.frame(response = c("a", "b", "c", "d", "e"),
                  p_value = c(0.01, 0.04, 0.03, 0.005, NA))
adjusted <- holm_adjust(toy)
report(isTRUE(all.equal(adjusted$p_holm[1:4], c(0.03, 0.06, 0.06, 0.02), tolerance = TOL)),
       "the adjusted values are 0.03, 0.06, 0.06 and 0.02 (found %s)",
       paste(format(adjusted$p_holm[1:4]), collapse = ", "))
report(is.na(adjusted$p_holm[5]), "a row without a p-value keeps no adjusted value")
report(inherits(try(holm_adjust(data.frame(response = "a")), silent = TRUE), "try-error"),
       "a file without a p-value column is refused")

# ── 2. Every tracked paired file is adjusted within itself ──────────────────

cat("\n-- every tracked paired-difference file carries its Holm adjustment --\n")

paired_files <- list.files(DATA_DIR, pattern = "_paired(_high_intensity)?\\.csv$",
                           recursive = TRUE, full.names = TRUE)
report(length(paired_files) > 0, "the tracked evidence carries paired-difference files (%d)",
       length(paired_files))
for (f in paired_files) {
  d <- read.csv(f, stringsAsFactors = FALSE)
  ok <- "p_holm" %in% names(d) &&
    isTRUE(all.equal(d$p_holm, holm_adjust(d[, setdiff(names(d), "p_holm")])$p_holm,
                     tolerance = 1e-8))
  report(ok, "%s carries p_holm, the Holm adjustment of its own p-values", f)
}

# ── 3 and 4. The per-replication stability classification ───────────────────

cat("\n-- the stability classification takes its interval across replications --\n")

#' A constructed daily series whose block means rise linearly in each replication
#'
#' @param slopes Per-replication rise per block.
#' @return Daily series in the long form replication_block_means() takes, every
#'   day of block $b$ of replication $r$ carrying `10 + slopes[r] * b`.
toy_series <- function(slopes) {
  days <- seq_len(N_BLOCKS * BLOCK_DAYS)
  block <- floor((days - 1L) / BLOCK_DAYS) + 1L
  do.call(rbind, lapply(seq_along(slopes), function(r) {
    data.frame(replication = r, day = days, series = "mean_queue", subject = "pool",
               value = 10 + slopes[r] * block)
  }))
}

# Rises of 1, 3 and 2 per block: the mean series rises exactly 2 a block, so a fit
# to it has no residual and a zero-width interval, and would call the response
# drifting; across replications the slopes have mean 2 and standard deviation 1,
# so the interval is 2 +/- qt(0.975, 2) / sqrt(3), which spans zero.
spread <- toy_series(c(1, 3, 2))
st <- classify_stability(block_means(spread, BLOCK_DAYS),
                         replication_block_means(spread, BLOCK_DAYS))
half <- qt(0.975, 2) * 1 / sqrt(3)
report(isTRUE(all.equal(st$slope, 2, tolerance = TOL)) &&
         isTRUE(all.equal(c(st$ci_lower, st$ci_upper), c(2 - half, 2 + half),
                          tolerance = 1e-8)),
       "the slope is the mean replication slope, 2, in [%.4f, %.4f] (found [%.4f, %.4f])",
       2 - half, 2 + half, st$ci_lower, st$ci_upper)
report(identical(as.character(st$stability), "converged"),
       "replications trending apart are converged, their interval spanning zero (found %s)",
       st$stability)
# An exact linear fit draws lm()'s perfect-fit warning, which is expected here.
fit_only <- suppressWarnings(classify_stability(block_means(spread, BLOCK_DAYS)))
report(identical(as.character(fit_only$stability), "drifting"),
       "where a fit to the mean series alone would call the same response drifting")

together <- toy_series(c(2, 2, 2))
st2 <- classify_stability(block_means(together, BLOCK_DAYS),
                          replication_block_means(together, BLOCK_DAYS))
report(identical(as.character(st2$stability), "drifting") &&
         isTRUE(all.equal(st2$slope, 2, tolerance = TOL)),
       "replications rising together by 2 a block are drifting (found %s)", st2$stability)

one <- toy_series(3)
st3 <- suppressWarnings(classify_stability(block_means(one, BLOCK_DAYS),
                                           replication_block_means(one, BLOCK_DAYS)))
report(isTRUE(all.equal(st3$slope, 3, tolerance = TOL)) &&
         identical(as.character(st3$stability), "drifting"),
       "a single replication is classified by the fit to its own block means")

# ── 5. The late window ──────────────────────────────────────────────────────

cat("\n-- the late window is the methods paper's --\n")

methods_text <- paste(readLines(METHODS, warn = FALSE), collapse = "\n")
marker <- regmatches(methods_text,
                     regexpr("<!-- STABILITY late_from_block=([0-9]+) -->", methods_text))
stated <- suppressWarnings(as.integer(sub(".*=([0-9]+) -->", "\\1", marker)))
window <- late_window_blocks(N_BLOCKS)
report(length(stated) == 1 && identical(stated, window[1]) && identical(max(window), N_BLOCKS),
       "the paper states the late window from block %s and the code fits blocks %d to %d",
       if (length(stated) == 1) stated else "(no marker)", window[1], max(window))
first_day <- (window[1] - 1L) * BLOCK_DAYS + 1L
days_named <- sprintf("days %d to %d", first_day, N_BLOCKS * BLOCK_DAYS)
report(grepl(days_named, methods_text, fixed = TRUE),
       "the paper names the window's days, %d to %d", first_day, N_BLOCKS * BLOCK_DAYS)

# Every block from 7 holds 10 plus a replication's own offset, and block 6 holds
# zero: on blocks 7 to 12 alone the series is flat, so a classification that
# reads drifting with a late level below 10 has fitted block 6.
offsets <- c(0, 0.5, 1)
step_series <- do.call(rbind, lapply(seq_along(offsets), function(r) {
  days <- seq_len(N_BLOCKS * BLOCK_DAYS)
  block <- floor((days - 1) / BLOCK_DAYS) + 1L
  data.frame(replication = r, day = days, series = "mean_queue", subject = "pool",
             value = ifelse(block >= 7L, 10 + offsets[r], ifelse(block == 6L, 0, 5)))
}))
st5 <- suppressWarnings(classify_stability(block_means(step_series, BLOCK_DAYS),
                                           replication_block_means(step_series, BLOCK_DAYS)))
report(identical(as.character(st5$stability), "drifting") && st5$late_mean < 10,
       "a series flat from block 7 that rises into it from block 6 is drifting, late level %.2f",
       st5$late_mean)

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nThe Holm adjustment and the stability classification are correct.\n")
quit(status = 0L)
