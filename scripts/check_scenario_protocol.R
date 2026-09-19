#!/usr/bin/env Rscript
##############################################################################
## scripts/check_scenario_protocol.R                                        ##
## Regression check — the comparative scenario analysis's parameters, its    ##
## responses and its published tables agree                                  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_scenario_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The comparative scenario analysis is the companion
# paper's centrepiece, the experiment its opening finding rests on and the one
# `CLAUDE.md`'s Key Parameters note quotes. Until Issue #384 it wrote its
# results to the gitignored outputs/ alone, so no tracked file held the numbers
# the paper printed and there was nothing a reader or a check could compare
# them against. Auditing one figure meant re-running a hundred replications and
# trusting that the environment reproduced.
#
# That is the arrangement Issue #382 measured the cost of elsewhere. The
# strategic evacuation section drifted from its own tracked evidence set for a
# whole pull request cycle, printing a figure four times too small, and it
# drifted despite having a tracked set to drift from. A section with no tracked
# set leaves no such trace at all.
#
# What this asserts:
#
#   1. Every protocol parameter in R/scenario_runner.R equals the value
#      docs/Multi_Run_Supplement.md documents in a marker comment.
#   2. The tracked evidence set is that experiment: the documented profiles,
#      the documented replication count, and the response set the published
#      tables print.
#   3. Every figure the paper's two tables print matches the tracked
#      measurement, and a missing table, row or column fails rather than
#      passing quietly.
#   4. The queue-group reduction and its interval are correct on inputs whose
#      answers are computable by hand, so a table agreeing with the summary is
#      not two copies of one error.
#
# Assertion 4 is what keeps assertion 3 from being circular: 3 would hold for
# any summary the code happened to produce.

suppressPackageStartupMessages({
  library(dplyr)
})

source("R/scenario_runner.R")

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

#' Tracked casualty totals the published totals table is printed from
TOTALS_PATH <- file.path("data", "scenarios", "scenario_comparison_totals.csv")

#' Tracked per-pool queue summary the published queue table is printed from
GROUPS_PATH <- file.path("data", "scenarios", "scenario_queue_groups.csv")

#' Tracked per-replication per-pool queue series the summary is reduced from
GROUP_REPS_PATH <- file.path("data", "scenarios",
                             "scenario_queue_group_replications.csv")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Tolerance on a figure the paper prints rounded to its last decimal place
#'
#' @details Half a unit in the last printed place, on the scale each row is
#'   compared at, so a share printed as a percentage and a queue printed to
#'   three decimals are both covered.
PRINT_TOL <- 0.005

# ── 1. The code's parameters are the ones the supplement documents ───────────

cat("\n-- the protocol's parameters match the supplement --\n")

held <- list(
  replications = SCENARIO_REPLICATIONS,
  days         = SCENARIO_DAYS,
  seed         = SCENARIO_SEED
)

supplement <- paste(readLines(SUPPLEMENT_PATH, warn = FALSE), collapse = "\n")

#' Read one scenario protocol parameter the supplement states in a marker
#'
#' @param name Marker name, as it appears after "SCENARIO ".
#' @return The marker's value as a character string, or NA where absent.
#'
#' @details Marked rather than parsed out of the prose, on the convention
#'   `scripts/check_airlift_protocol.R` established: a check that guesses which
#'   number in a paragraph is the replication count fails for reasons that have
#'   nothing to do with the protocol.
scenario_marker <- function(name) {
  m <- regmatches(supplement,
                  regexpr(sprintf("<!-- SCENARIO %s=[^ ]+ -->", name), supplement))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- SCENARIO [^=]+=(.*) -->$", "\\1", m)
}

for (param in names(held)) {
  stated <- suppressWarnings(as.numeric(scenario_marker(param)))
  report(!is.na(stated) && stated == held[[param]],
         "the supplement states %s = %s and the code holds %s",
         param, format(stated), format(held[[param]]))
}

stated_profiles <- scenario_marker("profiles")
held_profiles <- SCENARIO_PROTOCOL_PROFILES
parsed_profiles <- if (is.na(stated_profiles)) {
  character(0)
} else {
  trimws(strsplit(stated_profiles, ",")[[1]])
}
report(identical(parsed_profiles, held_profiles),
       "the supplement states the profiles %s and the code holds %s",
       paste(parsed_profiles, collapse = ","), paste(held_profiles, collapse = ","))

# ── 2. The tracked responses are the experiment the supplement documents ─────

cat("\n-- the tracked evidence set is that experiment --\n")

totals <- if (file.exists(TOTALS_PATH)) {
  read.csv(TOTALS_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked casualty totals %s exist", TOTALS_PATH)
  NULL
}

groups <- if (file.exists(GROUPS_PATH)) {
  read.csv(GROUPS_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked queue-group summary %s exists", GROUPS_PATH)
  NULL
}

group_reps <- if (file.exists(GROUP_REPS_PATH)) {
  read.csv(GROUP_REPS_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked per-replication queue series %s exists", GROUP_REPS_PATH)
  NULL
}

if (!is.null(totals)) {
  report(setequal(unique(totals$scenario), held_profiles),
         "the tracked totals carry the documented profiles")
  report(all(totals$n_reps == held$replications),
         "every tracked totals row carries %s replications", format(held$replications))
  report(all(SCENARIO_TOTAL_METRICS %in% totals$metric),
         "every metric the totals table prints is carried (%s)",
         paste(setdiff(SCENARIO_TOTAL_METRICS, totals$metric), collapse = ","))
}

if (!is.null(groups)) {
  report(setequal(unique(groups$scenario), held_profiles),
         "the tracked queue summary carries the documented profiles")
  report(all(groups$n_reps == held$replications),
         "every tracked queue-group row carries %s replications",
         format(held$replications))
  report(setequal(unique(groups$group), SCENARIO_QUEUE_GROUPS),
         "the tracked queue summary carries the six groups the table prints")
}

# The summary has to be the reduction of the series beside it, or the two
# tracked files are independent claims rather than one measurement.
if (!is.null(groups) && !is.null(group_reps)) {
  recomputed <- group_reps %>%
    group_by(scenario, group) %>%
    summarise(n = n(), mean_q = mean(mean_q), .groups = "drop")
  joined <- merge(groups, recomputed, by = c("scenario", "group"))
  report(nrow(joined) == nrow(groups) &&
           all(joined$n == joined$n_reps) &&
           all(abs(joined$mean_q.x - joined$mean_q.y) < 1e-9),
         "the tracked summary is the reduction of the tracked per-replication series")
}

# ── 3. The published tables match the tracked measurement ────────────────────

cat("\n-- every published figure matches the tracked measurement --\n")

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

#' The leading figure of one printed cell
#'
#' @param cell The cell's text.
#' @return The number the cell opens with, or NA where it carries none.
#'
#' @details Only the leading figure is read; the interval and percentile band
#'   beside it are the same mean's neighbourhood and add nothing a comparison
#'   of means misses. Thousands separators are stripped, the paper printing
#'   casualty counts with them.
leading_figure <- function(cell) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "",
                                   gsub(",", "", sub("[ %].*$", "", cell)))))
}

#' Check one published table row against the tracked measurement
#'
#' @param marker The HTML comment marking the table in the paper.
#' @param rows The table's lines.
#' @param label Regular expression matching the row's label cell.
#' @param expected Numeric vector of the values the row's columns should print,
#'   in the table's column order, already on the printed scale.
#' @param digits Decimal places the paper prints the row to.
#' @return Invisible NULL.
check_published_row <- function(marker, rows, label, expected, digits) {
  row <- rows[grepl(paste0("^\\| ", label), rows)]
  if (length(row) != 1) {
    report(FALSE, "the paper prints one '%s' row under %s (found %d)",
           label, marker, length(row))
    return(invisible(NULL))
  }
  cells <- table_cells(row)
  for (k in seq_along(expected)) {
    printed <- if (k <= length(cells)) leading_figure(cells[k]) else NA_real_
    ok <- !is.na(printed) && !is.na(expected[k]) &&
      abs(printed - round(expected[k], digits)) < PRINT_TOL
    report(ok, "'%s' in column %d of %s prints %s against the data's %s",
           label, k, marker, format(printed), format(round(expected[k], digits)))
  }
  invisible(NULL)
}

#' One tracked value, or NA where the tracked set carries no such row
#'
#' @param data The tracked data frame.
#' @param key Name of the column identifying the row within a scenario.
#' @param value The row's value of that column.
#' @param scenario The profile the row belongs to.
#' @param column Name of the column to read.
#' @return The value, or NA where the row is absent or repeated.
tracked_value <- function(data, key, value, scenario, column) {
  hit <- data[data[[key]] == value & data$scenario == scenario, ]
  if (nrow(hit) != 1) return(NA_real_)
  hit[[column]]
}

totals_rows <- paper_table("<!-- SCENARIO TOTALS TABLE -->")
if (!is.null(totals_rows) && !is.null(totals)) {
  header <- table_cells(totals_rows[1])
  # Two intensity columns and a ratio column the paper computes from them.
  report(length(header) == length(held_profiles) + 1,
         "the totals table prints one column per profile and a ratio (found %d)",
         length(header))

  specs <- list(
    list("Total casualties", "total_casualties", 1, 1),
    list("Wounded in action", "wia_count", 1, 1),
    list("Died of wounds/run", "dow_count", 1, 2),
    list("Died of wounds, as share", "dow_rate", 100, 2)
  )
  for (spec in specs) {
    expected <- vapply(held_profiles, function(s) {
      spec[[3]] * tracked_value(totals, "metric", spec[[2]], s, "mean")
    }, numeric(1))
    check_published_row("<!-- SCENARIO TOTALS TABLE -->", totals_rows,
                        spec[[1]], expected, spec[[4]])
  }
}

queue_rows <- paper_table("<!-- SCENARIO QUEUE TABLE -->")
if (!is.null(queue_rows) && !is.null(groups)) {
  header <- table_cells(queue_rows[1])
  report(length(header) == length(held_profiles) + 1,
         "the queue table prints one column per profile and a ratio (found %d)",
         length(header))

  labels <- list(
    list("R2B operating theatre", "R2B OT"),
    list("R2B holding beds", "R2B Hold"),
    list("R2E operating theatre", "R2E OT"),
    list("R2E intensive care", "R2E ICU"),
    list("R2E holding beds", "R2E Hold"),
    list("Ambulance and truck fleets", "Transport")
  )
  for (spec in labels) {
    expected <- vapply(held_profiles, function(s) {
      tracked_value(groups, "group", spec[[2]], s, "mean_q")
    }, numeric(1))
    check_published_row("<!-- SCENARIO QUEUE TABLE -->", queue_rows,
                        spec[[1]], expected, 3)
  }
}

# ── 4. The reduction and its interval are right on a known input ─────────────

cat("\n-- the queue reduction is correct on a hand-computable input --\n")

# Two beds of one pool. Bed 1 queues one casualty over the first half of a
# 100-minute window, bed 2 queues two over the second half, so the pool total
# is 1 for 50 minutes and 2 for 50 and its time-weighted mean is exactly 1.5.
known_steps <- pool_queue_steps(
  resource = c("bed_1", "bed_1", "bed_2", "bed_2"),
  time     = c(0, 50, 50, 100),
  queue    = c(1, 0, 2, 0)
)
# suppressWarnings: the window's last step coincides with its closing edge, so
# approx() sees a repeated abscissa and says so. Both copies carry the same
# cumulative integral, so the interpolation is unaffected.
known_mean <- suppressWarnings(step_bin_means(known_steps, c(0, 100)))
report(abs(known_mean - 1.5) < TOL,
       "the pool's time-weighted mean queue is 1.5 (found %.6f)", known_mean)

# A peak falling entirely inside the window is carried by the mean rather than
# missed, which sampling at the window's edges would do.
spike_steps <- pool_queue_steps(resource = c("bed_1", "bed_1"),
                                time = c(40, 60), queue = c(10, 0))
spike_mean <- suppressWarnings(step_bin_means(spike_steps, c(0, 100)))
report(abs(spike_mean - 2) < TOL,
       "a peak inside the window reaches the mean (%.6f against 2)", spike_mean)

# Five values whose mean is 3 and whose sample standard deviation is exactly
# sqrt(2.5), so the half-width is qt(0.975, 4) * sqrt(2.5 / 5) and can be
# written down without reference to the function under test.
known <- data.frame(group = "R2E OT", replication = 1:5, mean_q = c(1, 2, 3, 4, 5))
computed <- summarise_scenario_queue_groups(known)
expected_half <- qt(0.975, df = 4) * sqrt(2.5) / sqrt(5)

report(nrow(computed) == 1, "the summary returns one row per group")
if (nrow(computed) == 1) {
  report(abs(computed$mean_q - 3) < TOL,
         "the mean is the arithmetic mean (%.6f against 3)", computed$mean_q)
  report(abs(computed$ci_lower - (3 - expected_half)) < TOL &&
           abs(computed$ci_upper - (3 + expected_half)) < TOL,
         "the interval is the Student t one at 95%% ([%.6f, %.6f] against [%.6f, %.6f])",
         computed$ci_lower, computed$ci_upper, 3 - expected_half, 3 + expected_half)
  report(computed$n_reps == 5, "the replication count is the number of values (%d)",
         computed$n_reps)
}

# A group measured in one replication alone carries no interval rather than a
# zero-width one, which would read as a mean known exactly.
one_replication <- data.frame(group = "R2E OT", replication = 1L, mean_q = 4)
single <- summarise_scenario_queue_groups(one_replication)
report(nrow(single) == 1 && is.na(single$ci_lower) && is.na(single$ci_upper),
       "a single-replication group carries no interval rather than a zero-width one")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All comparative scenario protocol checks passed.\n")
quit(status = 0)
