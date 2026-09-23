#!/usr/bin/env Rscript
##############################################################################
## scripts/check_hold_window_protocol.R                                     ##
## Regression check — the R2B pre-open hold window's parameters, its        ##
## responses and its published table agree                                  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_hold_window_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Option 3 of docs/Multi_Run_Analysis.md prints eight
# paired differences comparing the shipped 60-minute R2B pre-open window
# against a window of zero, three of them established effects the section's
# recommendation rests on. Until Issue #387 none of the twenty-four numbers
# behind that table existed in a tracked file: the experiment's entry point
# was run.R under a parameter override rather than a driver script, so there
# was no command to give a --refresh-baseline flag to, and auditing one figure
# meant re-running 100 replications and trusting that the environment
# reproduced.
#
# What this asserts:
#
#   1. Every protocol parameter in R/hold_window.R equals the value
#      docs/Multi_Run_Supplement.md documents in a marker comment.
#   2. The tracked evidence set is that experiment: both documented arms, the
#      documented replication count, and the response set the published table
#      prints, with the summary and the paired differences each the reduction
#      of the tracked per-replication responses beside them.
#   3. Every figure the paper's eight-row table prints matches the tracked
#      measurement, and a missing row or column fails rather than passing
#      quietly.
#   4. summarise_hold_window() and hold_window_paired_difference() are correct
#      on inputs whose answers are computable by hand, so a table agreeing
#      with the summary is not two copies of one error.
#
# Assertion 4 is what keeps assertion 3 from being circular: 3 would hold for
# any summary the code happened to produce.

suppressPackageStartupMessages({
  library(dplyr)
})

source("R/constants.R")
source("R/hold_window.R")

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
#' @details Anything other than TRUE is a failure, NA included, so a quantity
#'   that becomes NA because the code under test is wrong is reported rather
#'   than raising an error that stops the run at the first such assertion.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' The supplement, which documents the experiment's design
SUPPLEMENT_PATH <- file.path("docs", "Multi_Run_Supplement.md")

#' The companion paper, which prints the experiment's table
PAPER_PATH <- file.path("docs", "Multi_Run_Analysis.md")

#' Tracked per-replication responses, both arms
REPLICATIONS_PATH <- file.path("data", "hold_window", "hold_window_replications.csv")

#' Tracked per-arm mean and interval
SUMMARY_PATH <- file.path("data", "hold_window", "hold_window_summary.csv")

#' Tracked paired differences between arms
PAIRED_PATH <- file.path("data", "hold_window", "hold_window_paired.csv")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Tolerance on a figure the paper prints rounded to its last decimal place
#'
#' @details Half a unit in the last printed place, the paper printing every
#'   row of this table to two decimals.
PRINT_TOL <- 0.005

# ── 1. The code's parameters are the ones the supplement documents ───────────

cat("\n-- the protocol's parameters match the supplement --\n")

supplement <- paste(readLines(SUPPLEMENT_PATH, warn = FALSE), collapse = "\n")

#' Read one hold window protocol parameter the supplement states in a marker
#'
#' @param name Marker name, as it appears after "HOLD_WINDOW ".
#' @return The marker's value as a character string, or NA where absent.
#'
#' @details Marked rather than parsed out of the prose, on the convention
#'   `scripts/check_airlift_protocol.R` established: a check that guesses
#'   which number in a paragraph is the replication count fails for reasons
#'   that have nothing to do with the protocol.
hold_window_marker <- function(name) {
  m <- regmatches(supplement,
                  regexpr(sprintf("<!-- HOLD_WINDOW %s=[^ ]+ -->", name), supplement))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- HOLD_WINDOW [^=]+=(.*) -->$", "\\1", m)
}

held <- list(replications = HOLD_WINDOW_REPLICATIONS, days = HOLD_WINDOW_DAYS,
             seed = HOLD_WINDOW_SEED)
for (param in names(held)) {
  stated <- suppressWarnings(as.numeric(hold_window_marker(param)))
  report(!is.na(stated) && stated == held[[param]],
         "the supplement states %s = %s and the code holds %s",
         param, format(stated), format(held[[param]]))
}

stated_arms <- hold_window_marker("arms")
parsed_arms <- if (is.na(stated_arms)) {
  integer(0)
} else {
  as.integer(trimws(strsplit(stated_arms, ",")[[1]]))
}
report(identical(parsed_arms, as.integer(HOLD_WINDOW_ARMS)),
       "the supplement states the arms %s and the code holds %s",
       paste(parsed_arms, collapse = ","), paste(HOLD_WINDOW_ARMS, collapse = ","))

# ── 2. The tracked responses are the experiment the supplement documents ─────

cat("\n-- the tracked evidence set is that experiment --\n")

per_rep <- if (file.exists(REPLICATIONS_PATH)) {
  read.csv(REPLICATIONS_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked per-replication responses %s exist", REPLICATIONS_PATH)
  NULL
}

summary_rows <- if (file.exists(SUMMARY_PATH)) {
  read.csv(SUMMARY_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked summary %s exists", SUMMARY_PATH)
  NULL
}

paired <- if (file.exists(PAIRED_PATH)) {
  read.csv(PAIRED_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked paired differences %s exist", PAIRED_PATH)
  NULL
}

if (!is.null(per_rep)) {
  report(setequal(unique(per_rep$window_min), HOLD_WINDOW_ARMS),
         "the tracked replications carry the documented arms")
  counts <- tapply(per_rep$replication, per_rep$window_min, length)
  report(all(counts == HOLD_WINDOW_REPLICATIONS),
         "every arm carries %d replications (found %s)", HOLD_WINDOW_REPLICATIONS,
         paste(counts, collapse = ","))
  report(all(HOLD_WINDOW_RESPONSES %in% names(per_rep)),
         "every response the table prints is carried (%s)",
         paste(setdiff(HOLD_WINDOW_RESPONSES, names(per_rep)), collapse = ","))
}

if (!is.null(summary_rows)) {
  report(setequal(unique(summary_rows$window_min), HOLD_WINDOW_ARMS),
         "the tracked summary carries the documented arms")
  report(all(summary_rows$n_reps == HOLD_WINDOW_REPLICATIONS),
         "every tracked summary row carries %d replications", HOLD_WINDOW_REPLICATIONS)
}

if (!is.null(paired)) {
  report(setequal(paired$response, HOLD_WINDOW_RESPONSES),
         "the tracked paired differences carry every response the table prints")
  report(all(paired$from == HOLD_WINDOW_ARMS[1] & paired$to == HOLD_WINDOW_ARMS[2]),
         "every paired difference runs from %d to %d minutes",
         HOLD_WINDOW_ARMS[1], HOLD_WINDOW_ARMS[2])
}

# The summary and the paired differences have to be the reduction of the
# per-replication set beside them, or the three tracked files are independent
# claims rather than one measurement.
if (!is.null(per_rep) && !is.null(summary_rows)) {
  recomputed <- do.call(rbind, lapply(HOLD_WINDOW_RESPONSES, function(r) {
    agg <- aggregate(per_rep[[r]], by = list(window_min = per_rep$window_min), FUN = mean)
    data.frame(response = r, window_min = agg$window_min, mean = agg$x)
  }))
  joined <- merge(summary_rows, recomputed, by = c("response", "window_min"))
  report(nrow(joined) == nrow(summary_rows) &&
           all(abs(joined$mean.x - joined$mean.y) < TOL),
         "the tracked summary is the reduction of the tracked per-replication responses")
}

if (!is.null(per_rep) && !is.null(paired)) {
  recomputed_diff <- do.call(rbind, lapply(HOLD_WINDOW_RESPONSES, function(r) {
    a <- per_rep[per_rep$window_min == HOLD_WINDOW_ARMS[1], c("replication", r)]
    b <- per_rep[per_rep$window_min == HOLD_WINDOW_ARMS[2], c("replication", r)]
    m <- merge(a, b, by = "replication", suffixes = c("_a", "_b"))
    data.frame(response = r, difference = mean(m[[paste0(r, "_b")]] - m[[paste0(r, "_a")]]))
  }))
  joined_diff <- merge(paired, recomputed_diff, by = "response")
  report(nrow(joined_diff) == nrow(paired) &&
           all(abs(joined_diff$difference.x - joined_diff$difference.y) < TOL),
         "the tracked paired differences are the reduction of the per-replication responses")
}

# ── 3. The published table matches the tracked measurement ───────────────────

cat("\n-- every published figure matches the tracked measurement --\n")

paper <- readLines(PAPER_PATH, warn = FALSE)

#' The rows of the marked hold window table in the paper
#'
#' @param marker The HTML comment marking the table.
#' @return The table's lines, or NULL where the marker is absent or repeated.
paper_table <- function(marker) {
  at <- grep(marker, paper, fixed = TRUE)
  if (length(at) != 1) {
    report(FALSE, "the paper carries exactly one '%s' marker (found %d)",
           marker, length(at))
    return(NULL)
  }
  rows <- paper[at:length(paper)]
  rows <- rows[seq_len(which(!grepl("^\\|", rows) & seq_along(rows) > 2)[1] - 1)]
  rows[grepl("^\\|", rows)]
}

#' Split one markdown table row into its cells, dropping the label column
#'
#' @param row The row's text.
#' @return Character vector of the row's cells after the first.
table_cells <- function(row) {
  trimws(strsplit(sub("^\\|", "", sub("\\|$", "", row)), "\\|")[[1]])[-1]
}

#' The leading figure of one printed cell
#'
#' @param cell The cell's text.
#' @return The number the cell opens with, or NA where it carries none.
#'
#' @details The paper prints a negative difference with the Unicode minus sign
#'   (U+2212) rather than a hyphen, so that character is normalised to a
#'   hyphen before the figure is stripped down; left alone, `as.numeric()`
#'   would silently drop the sign and read a negative figure as positive.
leading_figure <- function(cell) {
  cell <- gsub("−", "-", cell)
  suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", sub("[ %].*$", "", cell))))
}

#' One tracked summary mean, or NA where the tracked set carries no such row
#'
#' @param window_min Arm to read.
#' @param response Response to read.
#' @return The mean, or NA where the row is absent or repeated.
tracked_mean <- function(window_min, response) {
  if (is.null(summary_rows)) return(NA_real_)
  hit <- summary_rows[summary_rows$window_min == window_min &
                        summary_rows$response == response, ]
  if (nrow(hit) != 1) return(NA_real_)
  hit$mean
}

#' Check one published table row against the tracked measurement
#'
#' @param rows The table's lines.
#' @param label Regular expression matching the row's label cell.
#' @param response Response name the row reports.
#' @return Invisible NULL.
check_published_row <- function(rows, label, response) {
  row <- rows[grepl(paste0("^\\| ", label), rows)]
  if (length(row) != 1) {
    report(FALSE, "the paper prints one '%s' row (found %d)", label, length(row))
    return(invisible(NULL))
  }
  cells <- table_cells(row)
  expected <- c(tracked_mean(HOLD_WINDOW_ARMS[1], response),
                tracked_mean(HOLD_WINDOW_ARMS[2], response))
  if (!is.null(paired)) {
    hit <- paired[paired$response == response, ]
    expected <- c(expected, if (nrow(hit) == 1) hit$difference else NA_real_)
  }
  for (k in seq_along(expected)) {
    printed <- if (k <= length(cells)) leading_figure(cells[k]) else NA_real_
    ok <- !is.na(printed) && !is.na(expected[k]) &&
      abs(printed - round(expected[k], 2)) < PRINT_TOL
    report(ok, "'%s' in column %d prints %s against the tracked %s",
           label, k, format(printed), format(round(expected[k], 2)))
  }
  invisible(NULL)
}

table_rows <- paper_table("<!-- HOLD WINDOW TABLE -->")
if (!is.null(table_rows)) {
  labels <- list(
    list("Casualties held at R2B", "held_r2b"),
    list("R2B surgeries", "r2b_surgeries"),
    list("Diverted, team off shift", "diverted_offshift"),
    list("Diverted, theatre busy", "diverted_busy"),
    list("R2E first surgeries", "r2e_first_surgeries"),
    list("R2E theatre entry deferred", "r2e_theatre_deferred"),
    list("Died of wounds per run", "total_dow"),
    list("Total casualties", "total_casualties")
  )
  for (spec in labels) check_published_row(table_rows, spec[[1]], spec[[2]])
}

# ── 4. The reduction and its interval are right on a known input ─────────────

cat("\n-- the reduction functions are correct on a known input --\n")

# Five values whose mean is 3 and whose sample standard deviation is exactly
# sqrt(2.5), so the half-width is qt(0.975, 4) * sqrt(2.5 / 5) and can be
# written down without reference to the function under test.
known <- data.frame(window_min = 0L, replication = 1:5, held_r2b = c(1, 2, 3, 4, 5))
computed <- summarise_hold_window(known)
expected_half <- qt(0.975, df = 4) * sqrt(2.5) / sqrt(5)

report(nrow(computed) == 1, "the summary returns one row per response")
if (nrow(computed) == 1) {
  report(abs(computed$mean - 3) < TOL, "the mean is the arithmetic mean (%.6f against 3)",
         computed$mean)
  report(abs(computed$ci_lower - (3 - expected_half)) < TOL &&
           abs(computed$ci_upper - (3 + expected_half)) < TOL,
         "the interval is the Student t one at 95%% ([%.6f, %.6f] against [%.6f, %.6f])",
         computed$ci_lower, computed$ci_upper, 3 - expected_half, 3 + expected_half)
  report(computed$n_reps == 5, "the replication count is the number of values (%d)",
         computed$n_reps)
}

# A response measured in one replication alone carries an interval equal to
# its mean, since there is no spread to estimate a half-width from.
one_rep <- data.frame(window_min = 0L, replication = 1L, held_r2b = 4)
single <- summarise_hold_window(one_rep)
report(nrow(single) == 1 && single$ci_lower == 4 && single$ci_upper == 4,
       "a single-replication response carries an interval equal to its mean")

# Two arms of five paired replications each, whose difference is a constant 2
# in replications 1-4 and 10 in replication 5: mean 3.6, computable by hand.
pair_known <- data.frame(
  window_min  = rep(c(0L, 60L), each = 5),
  replication = rep(1:5, 2),
  held_r2b    = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 11)
)
diff <- hold_window_paired_difference(pair_known, "held_r2b", 0L, 60L)
report(nrow(diff) == 1 && diff$n_pairs == 5,
       "the paired difference is computed over every matched replication (%d pairs)",
       if (nrow(diff) == 1) diff$n_pairs else NA)
if (nrow(diff) == 1) {
  report(abs(diff$difference - 3.6) < TOL,
         "the paired difference is the within-replication mean (%.6f against 3.6)",
         diff$difference)
}

# A pair with no counterpart in the other arm is dropped from the difference
# rather than treated as a zero change.
one_sided <- rbind(pair_known,
                   data.frame(window_min = 0L, replication = 6L, held_r2b = 99))
diff_dropped <- hold_window_paired_difference(one_sided, "held_r2b", 0L, 60L)
report(diff_dropped$n_pairs == 5,
       "a replication present in one arm alone is dropped from the paired difference (%d pairs)",
       diff_dropped$n_pairs)

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All R2B pre-open hold window protocol checks passed.\n")
quit(status = 0)
