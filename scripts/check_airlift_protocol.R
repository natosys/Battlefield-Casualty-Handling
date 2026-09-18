#!/usr/bin/env Rscript
##############################################################################
## scripts/check_airlift_protocol.R                                         ##
## Regression check — the strategic evacuation experiment's parameters, its  ##
## interval construction and its published tables agree                     ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_airlift_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Four replicated experiments have a protocol check
# asserting that the figures docs/Multi_Run_Analysis.md prints agree with the
# tracked evidence set behind them. This one had none: data/airlift/'s baseline
# and sweep summaries were read by no check at all, the collapse check covering
# only the collapse files.
#
# The gap was not theoretical. The section drifted from its own evidence set
# between PR #379 and PR #381 and nothing reported it, the worst of it printing
# 26.96 casualties still waiting at high intensity against a tracked 108.28, a
# fourfold understatement in a section whose thesis is that evacuation is bound
# by when sorties arrive. Nobody re-runs 650 replications to audit a paragraph,
# so the agreement has to be checked without running anything.
#
# What this asserts:
#
#   1. Every protocol parameter in R/airlift.R equals the value
#      docs/Multi_Run_Supplement.md documents in a marker comment.
#   2. The tracked per-replication responses carry the documented replication
#      count, the documented arms and the response set the summary reports.
#   3. Every figure the paper prints from this experiment matches the tracked
#      summary, across all three published tables, and the check fails rather
#      than passing quietly when a table, a row or a column it expects is
#      absent.
#   4. summarise_airlift()'s mean and interval are correct on an input whose
#      answer is computable by hand, so a table agreeing with the summary is
#      not two copies of one error.
#
# Assertion 4 is what keeps assertion 3 from being circular: 3 would hold for
# any summary the code happened to produce.

suppressPackageStartupMessages({
  library(dplyr)
})

source("R/constants.R")
source("R/airlift.R")

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

#' The companion paper, which prints the experiment's figures
PAPER_PATH <- file.path("docs", "Multi_Run_Analysis.md")

#' Tracked per-replication responses
RESPONSE_PATH <- file.path("data", "airlift", "airlift_replications.csv")

#' Tracked summary the published tables are printed from
SUMMARY_PATH <- file.path("data", "airlift", "airlift_summary.csv")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Tolerance on a figure the paper prints rounded to two decimal places
#'
#' @details Half a unit in the last printed place. A share printed as a whole
#'   percentage is compared on its own scale, so the same tolerance serves both.
PRINT_TOL <- 0.005

# ── 1. The code's parameters are the ones the supplement documents ───────────

cat("\n-- the protocol's parameters match the supplement --\n")

supplement <- paste(readLines(SUPPLEMENT_PATH, warn = FALSE), collapse = "\n")

#' Read one airlift parameter the supplement states in a marker comment
#'
#' @param name Marker name, as it appears after "AIRLIFT ".
#' @return The marker's value as a character string, or NA where absent.
#'
#' @details Marked rather than parsed out of the prose, on the convention
#'   `scripts/check_airlift_collapse_protocol.R` established: a check that
#'   guesses which number in a paragraph is the replication count fails for
#'   reasons that have nothing to do with the protocol.
airlift_marker <- function(name) {
  m <- regmatches(supplement,
                  regexpr(sprintf("<!-- AIRLIFT %s=[^ ]+ -->", name), supplement))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- AIRLIFT [^=]+=(.*) -->$", "\\1", m)
}

for (param in list(list("days", AIRLIFT_DAYS),
                   list("replications", AIRLIFT_REPLICATIONS))) {
  stated <- suppressWarnings(as.numeric(airlift_marker(param[[1]])))
  report(!is.na(stated) && stated == param[[2]],
         "the supplement states %s = %s and the code holds %s",
         param[[1]], format(stated), format(param[[2]]))
}

#' Compare a documented comma-separated vector against the code's own
#'
#' @param name Marker name, as it appears after "AIRLIFT ".
#' @param held The vector the code holds.
#' @return Invisible NULL.
check_swept_vector <- function(name, held) {
  stated <- airlift_marker(name)
  parsed <- if (is.na(stated)) {
    numeric(0)
  } else {
    suppressWarnings(as.numeric(strsplit(stated, ",")[[1]]))
  }
  report(length(parsed) == length(held) && !any(is.na(parsed)) &&
           all(abs(parsed - held) < TOL),
         "the supplement states %s %s and the code holds %s", name,
         paste(format(parsed), collapse = ","),
         paste(format(held), collapse = ","))
  invisible(NULL)
}

check_swept_vector("failure_probabilities", AIRLIFT_FAILURE_PROBABILITIES)
check_swept_vector("sortie_intervals", AIRLIFT_SORTIE_INTERVALS)

# The shipped configuration must be a point of each sweep, or the sweeps say
# nothing about what departing from it costs.
report(any(abs(AIRLIFT_FAILURE_PROBABILITIES) < TOL),
       "the reliability sweep contains the shipped cancellation probability of zero")
report(7L %in% AIRLIFT_SORTIE_INTERVALS,
       "the interval sweep contains the shipped seven-day interval")

# ── 2. The tracked responses are the experiment the supplement documents ─────

cat("\n-- the tracked evidence set is that experiment --\n")

if (!file.exists(RESPONSE_PATH)) {
  report(FALSE, "the tracked per-replication responses %s exist", RESPONSE_PATH)
} else {
  responses <- read.csv(RESPONSE_PATH, stringsAsFactors = FALSE)
  per_arm <- responses %>%
    group_by(arm, setting, value) %>%
    summarise(n = n(), .groups = "drop")

  report(all(per_arm$n == AIRLIFT_REPLICATIONS),
         "every one of the %d configurations carries %d replications",
         nrow(per_arm), AIRLIFT_REPLICATIONS)

  expected_arms <- length(AIRLIFT_FAILURE_PROBABILITIES) +
    length(AIRLIFT_SORTIE_INTERVALS) + 2L
  report(nrow(per_arm) == expected_arms,
         "the tracked set carries the %d documented configurations (found %d)",
         expected_arms, nrow(per_arm))

  report(setequal(unique(responses$arm), c("baseline", "high", "reliability", "interval")),
         "the tracked set carries the four documented arms")

  swept_failures <- sort(unique(responses$value[responses$arm == "reliability"]))
  report(length(swept_failures) == length(AIRLIFT_FAILURE_PROBABILITIES) &&
           all(abs(swept_failures - sort(AIRLIFT_FAILURE_PROBABILITIES)) < TOL),
         "the reliability arm carries the swept probabilities the code holds")

  swept_intervals <- sort(unique(responses$value[responses$arm == "interval"]))
  report(length(swept_intervals) == length(AIRLIFT_SORTIE_INTERVALS) &&
           all(abs(swept_intervals - sort(AIRLIFT_SORTIE_INTERVALS)) < TOL),
         "the interval arm carries the swept intervals the code holds")

  # A response the paper prints but the tracked set does not carry would make
  # every comparison below vacuous rather than failing.
  needed <- c("boarded", "queued_at_end", "mean_wait_days", "hold_evac_share",
              "role4_peak", "role4_peak_after_end", "sorties_flown",
              "cancellation_rate", "ventilated_hold_hours")
  report(all(needed %in% names(responses)),
         "every response the paper prints is carried (%s)",
         paste(setdiff(needed, names(responses)), collapse = ",") )
}

# ── 3. The published tables match the tracked summary ────────────────────────

cat("\n-- every published figure matches the tracked summary --\n")

paper <- readLines(PAPER_PATH, warn = FALSE)

#' The rows of one marked table in the paper
#'
#' @param marker The HTML comment marking the table.
#' @return The table's lines, or NULL where the marker is absent or repeated.
#'
#' @details Returns NULL rather than an empty set on a missing marker, and the
#'   caller reports it, so a renamed or deleted table fails here rather than
#'   silently checking nothing.
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

#' Check one published table against the summary it is printed from
#'
#' @param marker The HTML comment marking the table in the paper.
#' @param arms Data frame of `arm` and `value`, one row per table column, in
#'   the order the table prints them.
#' @param printed_rows List of label, response and print scale triples.
#' @return Invisible NULL.
#'
#' @details The printed figure is compared against the summary's own mean, so a
#'   table edited by hand, or a summary re-measured without the table being
#'   re-rendered, fails rather than standing as a second and disagreeing copy.
#'   Only the leading figure of each cell is read; the interval beside it is the
#'   same mean's neighbourhood and adds nothing a comparison of means misses.
check_published_table <- function(marker, arms, printed_rows) {
  rows <- paper_table(marker)
  if (is.null(rows)) return(invisible(NULL))

  header <- table_cells(rows[1])
  report(length(header) == nrow(arms),
         "'%s' prints %d columns, one per documented arm (found %d)",
         marker, nrow(arms), length(header))
  if (length(header) != nrow(arms)) return(invisible(NULL))

  for (spec in printed_rows) {
    label <- spec[[1]]
    row <- rows[grepl(paste0("^\\| ", label), rows, fixed = FALSE)]
    if (length(row) != 1) {
      report(FALSE, "the paper prints one '%s' row under %s (found %d)",
             label, marker, length(row))
      next
    }
    cells <- table_cells(row)
    for (k in seq_len(nrow(arms))) {
      printed <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "",
                                                  sub(" .*$", "", cells[k]))))
      arm <- summary_rows[summary_rows$arm == arms$arm[k] &
                            abs(summary_rows$value - arms$value[k]) < TOL &
                            summary_rows$response == spec[[2]], ]
      expected <- if (nrow(arm) == 1) spec[[3]] * arm$mean else NA_real_
      ok <- nrow(arm) == 1 && !is.na(printed) &&
        abs(printed - round(expected, spec[[4]])) < PRINT_TOL
      report(ok, "'%s' in column %d of %s prints %s against the data's %s",
             label, k, marker, format(printed),
             if (is.na(expected)) "no row" else format(round(expected, spec[[4]])))
    }
  }
  invisible(NULL)
}

if (!file.exists(SUMMARY_PATH)) {
  report(FALSE, "the tracked summary %s exists", SUMMARY_PATH)
} else {
  summary_rows <- read.csv(SUMMARY_PATH, stringsAsFactors = FALSE)

  report(all(summary_rows$n_reps[!is.na(summary_rows$mean)] == AIRLIFT_REPLICATIONS),
         "every measured response of the summary carries %d replications",
         AIRLIFT_REPLICATIONS)

  # The peak-before-end response is negative in the data, the peak falling
  # before the campaign ends, and the paper prints the margin as a positive
  # number of days, which is why this row alone carries a scale of -1.
  check_published_table(
    "<!-- AIRLIFT BASELINE TABLE -->",
    data.frame(arm = c("baseline", "high"), value = c(0, 0)),
    list(list("Casualties boarded", "boarded", 1, 2),
         list("Still waiting at the close", "queued_at_end", 1, 2),
         list("Mean wait \\(days\\)", "mean_wait_days", 1, 2),
         list("Share of R2E holding beds", "hold_evac_share", 100, 0),
         list("Role 4 peak occupancy", "role4_peak", 1, 2),
         list("Days the peak falls before", "role4_peak_after_end", -1, 2)))

  check_published_table(
    "<!-- AIRLIFT INTERVAL TABLE -->",
    data.frame(arm = "interval", value = AIRLIFT_SORTIE_INTERVALS),
    list(list("Sorties flown", "sorties_flown", 1, 2),
         list("Mean wait \\(days\\)", "mean_wait_days", 1, 2),
         list("Share of R2E holding beds", "hold_evac_share", 100, 0),
         list("Ventilated pre-flight", "ventilated_hold_hours", 1, 2)))

  check_published_table(
    "<!-- AIRLIFT RELIABILITY TABLE -->",
    data.frame(arm = "reliability", value = AIRLIFT_FAILURE_PROBABILITIES),
    list(list("Sorties flown", "sorties_flown", 1, 2),
         list("Realised cancellation rate", "cancellation_rate", 100, 0),
         list("Mean wait \\(days\\)", "mean_wait_days", 1, 2),
         list("Share of R2E holding beds", "hold_evac_share", 100, 0)))
}

# ── 4. The interval construction is right on a hand-computable input ─────────

cat("\n-- the summary's mean and interval are correct on a known input --\n")

# Five values whose mean is 3 and whose sample standard deviation is exactly
# sqrt(2.5), so the half-width is qt(0.975, 4) * sqrt(2.5 / 5) and can be
# written down without reference to the function under test.
known <- data.frame(replication = 1:5, response_a = c(1, 2, 3, 4, 5))
computed <- summarise_airlift(known)
row <- computed[computed$response == "response_a", ]

expected_mean <- 3
expected_half <- qt(0.975, df = 4) * sqrt(2.5) / sqrt(5)

report(nrow(row) == 1, "the summary returns one row per response")
if (nrow(row) == 1) {
  report(abs(row$mean - expected_mean) < TOL,
         "the mean is the arithmetic mean (%.6f against %.6f)",
         row$mean, expected_mean)
  report(abs(row$ci_lower - (expected_mean - expected_half)) < TOL &&
           abs(row$ci_upper - (expected_mean + expected_half)) < TOL,
         "the interval is the Student t one at 95%% ([%.6f, %.6f] against [%.6f, %.6f])",
         row$ci_lower, row$ci_upper,
         expected_mean - expected_half, expected_mean + expected_half)
  report(row$n_reps == 5, "the replication count is the number of values (%d)",
         row$n_reps)
}

# A response no replication produced is reported with a count of zero rather
# than dropped, which is what lets a reader tell an absent measurement from a
# measured zero.
absent <- summarise_airlift(data.frame(replication = 1:3,
                                       response_b = c(NA_real_, NA_real_, NA_real_)))
report(nrow(absent) == 1 && absent$n_reps == 0 && is.na(absent$mean),
       "a response nobody produced is reported with a count of zero, not dropped")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All strategic evacuation protocol checks passed.\n")
quit(status = 0)
