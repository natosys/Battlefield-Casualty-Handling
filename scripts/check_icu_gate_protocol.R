#!/usr/bin/env Rscript
##############################################################################
## scripts/check_icu_gate_protocol.R                                        ##
## Regression check — the post-operative intensive care gate's parameters,  ##
## its responses and its published table agree                             ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_icu_gate_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The Post-Operative Intensive Care Gate section of
# docs/Results.md prints intensive care utilisation and mortality
# figures comparing the shipped rationing rule against a configuration that
# reconstructs the model as it stood before the rule existed. Until Issue
# #388 none of the numbers behind that section existed in a tracked file: the
# experiment's entry point was run.R under a parameter override rather than a
# driver script, so there was no command to give a --refresh-baseline flag to
# and auditing one figure meant re-running 100 replications and trusting that
# the environment reproduced.
#
# What this asserts:
#
#   1. Every protocol parameter in R/icu_gate.R equals the value
#      docs/Methods.md documents in a marker comment.
#   2. The tracked evidence set is that experiment: both documented arms, the
#      documented replication count, and the response set the published
#      section prints, with the summary and the paired differences each the
#      reduction of the tracked per-replication responses beside them.
#   3. Every figure the paper's section prints matches the tracked
#      measurement, and a missing row fails rather than passing quietly.
#   4. summarise_icu_gate() and icu_gate_paired_difference() are correct on
#      inputs whose answers are computable by hand, so a table agreeing with
#      the summary is not two copies of one error.
#
# Assertion 4 is what keeps assertion 3 from being circular: 3 would hold for
# any summary the code happened to produce. It also exercises the interval
# construction specifically, on a constructed input whose standard deviation
# is known, because the experiment this check backs supersedes an earlier
# measurement whose intervals were computed over antithetically paired
# replications while still dividing by the replication count and were
# therefore narrower than the runs entitled them to be
# (docs/Methods.md, "The Withdrawn Antithetic Pairing").
#
# This check covers the magnitudes the section prints. It sits alongside
# scripts/check_icu_gate_switch.R, which covers the mechanism (that the
# disabled arm defers nobody and diverts nobody, that both pathways are
# reachable when the gate is in force, and that a malformed value is
# rejected); neither subsumes the other.

suppressPackageStartupMessages({
  library(dplyr)
})

source("R/constants.R")
source("R/icu_gate.R")

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
#' @details Anything other than TRUE is a failure, NA included, so a
#'   quantity that becomes NA because the code under test is wrong is
#'   reported rather than raising an error that stops the run at the first
#'   such assertion.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' The methods_text, which documents the experiment's design
METHODS_PATH <- file.path("docs", "Methods.md")

#' The companion paper, which prints the experiment's section
PAPER_PATH <- file.path("docs", "Results.md")

#' Tracked per-replication responses, both arms
REPLICATIONS_PATH <- file.path("data", "icu_gate", "icu_gate_replications.csv")

#' Tracked per-arm mean and interval
SUMMARY_PATH <- file.path("data", "icu_gate", "icu_gate_summary.csv")

#' Tracked paired differences between arms
PAIRED_PATH <- file.path("data", "icu_gate", "icu_gate_paired.csv")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

# ── 1. The code's parameters are the ones the methods paper documents ─────────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one intensive care gate protocol parameter the methods paper states
#'
#' @param name Marker name, as it appears after "ICU_GATE ".
#' @return The marker's value as a character string, or NA where absent.
#'
#' @details Marked rather than parsed out of the prose, on the convention
#'   `scripts/check_hold_window_protocol.R` established: a check that
#'   guesses which number in a paragraph is the replication count fails for
#'   reasons that have nothing to do with the protocol.
icu_gate_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- ICU_GATE %s=[^ ]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- ICU_GATE [^=]+=(.*) -->$", "\\1", m)
}

held <- list(replications = ICU_GATE_REPLICATIONS, days = ICU_GATE_DAYS,
             window_days = ICU_GATE_WINDOW_DAYS, seed = ICU_GATE_SEED)
for (param in names(held)) {
  stated <- suppressWarnings(as.numeric(icu_gate_marker(param)))
  report(!is.na(stated) && stated == held[[param]],
         "the methods paper states %s = %s and the code holds %s",
         param, format(stated), format(held[[param]]))
}

stated_arms <- icu_gate_marker("arms")
parsed_arms <- if (is.na(stated_arms)) {
  integer(0)
} else {
  as.integer(trimws(strsplit(stated_arms, ",")[[1]]))
}
report(identical(parsed_arms, as.integer(ICU_GATE_ARMS)),
       "the methods paper states the arms %s and the code holds %s",
       paste(parsed_arms, collapse = ","), paste(ICU_GATE_ARMS, collapse = ","))

# ── 2. The tracked responses are the experiment the methods paper documents ────

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
  report(setequal(unique(per_rep$gate_enabled), ICU_GATE_ARMS),
         "the tracked replications carry the documented arms")
  counts <- tapply(per_rep$replication, per_rep$gate_enabled, length)
  report(all(counts == ICU_GATE_REPLICATIONS),
         "every arm carries %d replications (found %s)", ICU_GATE_REPLICATIONS,
         paste(counts, collapse = ","))
  report(all(ICU_GATE_RESPONSES %in% names(per_rep)),
         "every response the section reports is carried (%s)",
         paste(setdiff(ICU_GATE_RESPONSES, names(per_rep)), collapse = ","))
}

if (!is.null(summary_rows)) {
  report(setequal(unique(summary_rows$gate_enabled), ICU_GATE_ARMS),
         "the tracked summary carries the documented arms")
  report(all(summary_rows$n_reps == ICU_GATE_REPLICATIONS),
         "every tracked summary row carries %d replications", ICU_GATE_REPLICATIONS)
}

if (!is.null(paired)) {
  report(setequal(paired$response, ICU_GATE_RESPONSES),
         "the tracked paired differences carry every response the section reports")
  report(all(paired$from == ICU_GATE_ARMS[1] & paired$to == ICU_GATE_ARMS[2]),
         "every paired difference runs from gate %d to gate %d",
         ICU_GATE_ARMS[1], ICU_GATE_ARMS[2])
}

# The summary and the paired differences have to be the reduction of the
# per-replication set beside them, or the three tracked files are independent
# claims rather than one measurement.
if (!is.null(per_rep) && !is.null(summary_rows)) {
  recomputed <- do.call(rbind, lapply(ICU_GATE_RESPONSES, function(r) {
    agg <- aggregate(per_rep[[r]], by = list(gate_enabled = per_rep$gate_enabled), FUN = mean)
    data.frame(response = r, gate_enabled = agg$gate_enabled, mean = agg$x)
  }))
  joined <- merge(summary_rows, recomputed, by = c("response", "gate_enabled"))
  report(nrow(joined) == nrow(summary_rows) &&
           all(abs(joined$mean.x - joined$mean.y) < TOL),
         "the tracked summary is the reduction of the tracked per-replication responses")
}

if (!is.null(per_rep) && !is.null(paired)) {
  recomputed_diff <- do.call(rbind, lapply(ICU_GATE_RESPONSES, function(r) {
    a <- per_rep[per_rep$gate_enabled == ICU_GATE_ARMS[1], c("replication", r)]
    b <- per_rep[per_rep$gate_enabled == ICU_GATE_ARMS[2], c("replication", r)]
    m <- merge(a, b, by = "replication", suffixes = c("_a", "_b"))
    data.frame(response = r, difference = mean(m[[paste0(r, "_b")]] - m[[paste0(r, "_a")]]))
  }))
  joined_diff <- merge(paired, recomputed_diff, by = "response")
  report(nrow(joined_diff) == nrow(paired) &&
           all(abs(joined_diff$difference.x - joined_diff$difference.y) < TOL),
         "the tracked paired differences are the reduction of the per-replication responses")
}

# ── 3. The published section matches the tracked measurement ───────────────

cat("\n-- every published figure matches the tracked measurement --\n")

#' One tracked summary mean, or NA where the tracked set carries no such row
#'
#' @param gate_enabled Arm to read (0 or 1).
#' @param response Response to read.
#' @return The mean, or NA where the row is absent or repeated.
tracked_mean <- function(gate_enabled, response) {
  if (is.null(summary_rows)) return(NA_real_)
  hit <- summary_rows[summary_rows$gate_enabled == gate_enabled &
                        summary_rows$response == response, ]
  if (nrow(hit) != 1) return(NA_real_)
  hit$mean
}

#' One tracked paired-difference figure
#'
#' @param response Response to read.
#' @param field Column of `paired` to read (difference, ci_lower or ci_upper).
#' @return The figure, or NA where the row is absent or repeated.
tracked_paired <- function(response, field) {
  if (is.null(paired)) return(NA_real_)
  hit <- paired[paired$response == response, ]
  if (nrow(hit) != 1) return(NA_real_)
  hit[[field]]
}

paper <- readLines(PAPER_PATH, encoding = "UTF-8", warn = FALSE)

#' The cells of one row of a generated table in the results paper
#'
#' @param table Name of the generated span holding the table.
#' @param label The row's first cell.
#' @return Character vector of the row's cells after its label, or NULL where the span or
#'   the row is absent or repeated.
#'
#' @details Reads the document rather than regenerating the table, so a figure that has
#'   drifted from the tracked evidence is seen here. A missing span or row returns NULL
#'   rather than an empty vector, and every caller reports it.
table_row <- function(table, label) {
  open <- grep(sprintf("<!-- GEN %s -->", table), paper, fixed = TRUE)
  if (length(open) != 1) return(NULL)
  rest <- paper[(open + 1):length(paper)]
  rows <- rest[seq_len(which(grepl("^<!-- /GEN", rest))[1] - 1)]
  cells <- lapply(rows, function(r) {
    trimws(strsplit(sub("^\\|", "", sub("\\|$", "", r)), "\\|")[[1]])
  })
  hit <- Filter(function(cl) identical(cl[1], label), cells)
  if (length(hit) != 1) return(NULL)
  hit[[1]][-1]
}

#' The leading figure of a printed cell
#'
#' @param cell The cell's text, such as `-7.7 [-9.4, -6.0]` or `0.11%`.
#' @return The figure as a numeric with the Unicode minus read as a sign, or NA.
cell_figure <- function(cell) {
  if (is.null(cell) || is.na(cell)) return(NA_real_)
  lead <- sub("^([+\u2212-]?[0-9][0-9,]*\\.?[0-9]*).*$", "\\1", cell)
  suppressWarnings(as.numeric(gsub(",", "", gsub("\u2212", "-", lead))))
}

#' Whether a printed figure is the tracked one rounded to the printed places
#'
#' @param printed The figure as printed.
#' @param tracked The tracked figure, unrounded.
#' @param digits Decimal places printed.
#' @return TRUE where they agree to half a unit in the last place.
printed_matches <- function(printed, tracked, digits) {
  !is.na(printed) && !is.na(tracked) && abs(printed - tracked) <= 0.5 * 10^(-digits) + TOL
}

util_row <- table_row("icu_gate", "R2E ICU utilisation (%)")
dow_row  <- table_row("icu_gate", "Died of wounds per run")
report(!is.null(util_row) && !is.null(dow_row),
       "the results paper prints the utilisation and died-of-wounds rows of the gate table")

if (!is.null(util_row) && !is.null(dow_row)) {
  report(printed_matches(cell_figure(util_row[1]), 100 * tracked_mean(0, "icu_occupancy"), 1),
         "R2E ICU utilisation without the rule prints %s against the tracked %s",
         util_row[1], format(100 * tracked_mean(0, "icu_occupancy")))
  report(printed_matches(cell_figure(util_row[2]), 100 * tracked_mean(1, "icu_occupancy"), 1),
         "R2E ICU utilisation with the rule prints %s against the tracked %s",
         util_row[2], format(100 * tracked_mean(1, "icu_occupancy")))
  report(printed_matches(cell_figure(util_row[3]),
                         100 * tracked_paired("icu_occupancy", "difference"), 1),
         "the paired utilisation difference prints %s against the tracked %s",
         util_row[3], format(100 * tracked_paired("icu_occupancy", "difference")))
  report(printed_matches(cell_figure(dow_row[1]), tracked_mean(0, "total_dow"), 2),
         "died of wounds without the rule prints %s against the tracked %s",
         dow_row[1], format(tracked_mean(0, "total_dow")))
  report(printed_matches(cell_figure(dow_row[2]), tracked_mean(1, "total_dow"), 2),
         "died of wounds with the rule prints %s against the tracked %s",
         dow_row[2], format(tracked_mean(1, "total_dow")))
  report(printed_matches(cell_figure(dow_row[3]), tracked_paired("total_dow", "difference"), 2),
         "the paired died-of-wounds difference prints %s against the tracked %s",
         dow_row[3], format(tracked_paired("total_dow", "difference")))
}

# The pathway death-rate figures are pooled counts within the enabled arm
# rather than a mean of per-replication rates, on the reasoning
# R/icu_gate.R's header records: most replications carry a handful of deaths
# on each pathway, so the rate a reader can check is the one taken over the
# whole tracked evidence set.
if (!is.null(per_rep)) {
  enabled <- per_rep[per_rep$gate_enabled == 1, ]
  pathways <- list(list("Intensive care bed", sum(enabled$icu_pathway_n),
                        sum(enabled$icu_pathway_dow)),
                   list("Holding bed", sum(enabled$hold_pathway_n),
                        sum(enabled$hold_pathway_dow)))
  for (pw in pathways) {
    row <- table_row("icu_gate_pathways", pw[[1]])
    report(!is.null(row) && pw[[2]] > 0 &&
             printed_matches(cell_figure(row[3]), 100 * pw[[3]] / pw[[2]], 2) &&
             cell_figure(row[1]) == pw[[2]] && cell_figure(row[2]) == pw[[3]],
           "the %s pathway prints %s casualty-replications, %s deaths and %s against %d, %d",
           tolower(pw[[1]]), if (is.null(row)) "no row" else row[1],
           if (is.null(row)) "none" else row[2], if (is.null(row)) "none" else row[3],
           pw[[2]], pw[[3]])
  }
}

# ── 4. The reduction and its interval are right on a known input ───────────

cat("\n-- the reduction functions are correct on a known input --\n")

# Five values whose mean is 3 and whose sample standard deviation is exactly
# sqrt(2.5), so the half-width is qt(0.975, 4) * sqrt(2.5 / 5) and can be
# written down without reference to the function under test.
known <- data.frame(gate_enabled = 1L, replication = 1:5, icu_occupancy = c(1, 2, 3, 4, 5))
computed <- summarise_icu_gate(known)
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
one_rep <- data.frame(gate_enabled = 1L, replication = 1L, icu_occupancy = 0.4)
single <- summarise_icu_gate(one_rep)
report(nrow(single) == 1 && single$ci_lower == 0.4 && single$ci_upper == 0.4,
       "a single-replication response carries an interval equal to its mean")

# Two arms of five paired replications each, whose difference is a constant
# 0.2 in replications 1-4 and 1.0 in replication 5: mean 0.36, computable by
# hand, and its half-width is qt(0.975, 4) * sd(d) / sqrt(5) on
# d = (0.2, 0.2, 0.2, 0.2, 1.0), the paired reduction's own interval
# construction being what supersedes the antithetically paired one
# (docs/Methods.md, "The Withdrawn Antithetic Pairing").
pair_known <- data.frame(
  gate_enabled  = rep(c(0L, 1L), each = 5),
  replication   = rep(1:5, 2),
  icu_occupancy = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.3, 0.3, 0.3, 0.3, 1.1)
)
diff <- icu_gate_paired_difference(pair_known, "icu_occupancy", 0L, 1L)
expected_diff_half <- qt(0.975, df = 4) * sd(c(0.2, 0.2, 0.2, 0.2, 1.0)) / sqrt(5)
report(nrow(diff) == 1 && diff$n_pairs == 5,
       "the paired difference is computed over every matched replication (%d pairs)",
       if (nrow(diff) == 1) diff$n_pairs else NA)
if (nrow(diff) == 1) {
  report(abs(diff$difference - 0.36) < TOL,
         "the paired difference is the within-replication mean (%.6f against 0.36)",
         diff$difference)
  report(abs(diff$ci_lower - (0.36 - expected_diff_half)) < TOL &&
           abs(diff$ci_upper - (0.36 + expected_diff_half)) < TOL,
         "the paired interval is the Student t one ([%.6f, %.6f] against [%.6f, %.6f])",
         diff$ci_lower, diff$ci_upper, 0.36 - expected_diff_half, 0.36 + expected_diff_half)
}

# A pair with no counterpart in the other arm is dropped from the difference
# rather than treated as a zero change.
one_sided <- rbind(pair_known,
                   data.frame(gate_enabled = 0L, replication = 6L, icu_occupancy = 0.99))
diff_dropped <- icu_gate_paired_difference(one_sided, "icu_occupancy", 0L, 1L)
report(diff_dropped$n_pairs == 5,
       "a replication present in one arm alone is dropped from the paired difference (%d pairs)",
       diff_dropped$n_pairs)

# ── Result ───────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All post-operative intensive care gate protocol checks passed.\n")
quit(status = 0)
