#!/usr/bin/env Rscript
##############################################################################
## scripts/check_mass_casualty_protocol.R                                   ##
## Regression check — the mass casualty stress test's parameters, its       ##
## responses and its published table agree                                 ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_mass_casualty_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Mass Casualty Events Degrade Care Without Revealing
# New Constraints in docs/Multi_Run_Analysis.md prints a four-row table from
# replicated arms, and until Issue #389 none of the figures behind it existed
# in a tracked file: the experiment's entry point was run.R under a parameter
# override rather than a driver script, so there was no command to give a
# --refresh-baseline flag to and auditing one figure meant re-running the
# comparison and trusting that the environment reproduced.
#
# What this asserts:
#
#   1. Every protocol parameter in R/mass_casualty.R equals the value
#      docs/Multi_Run_Supplement.md documents in a marker comment.
#   2. The tracked evidence set is that experiment: both documented arms, the
#      documented replication count, and the response set the published table
#      prints, with the count and died-of-wounds summaries each the reduction
#      of the tracked per-replication responses beside them.
#   3. Every figure the paper's four-row table prints matches the tracked
#      measurement, and a missing row or column fails rather than passing
#      quietly.
#   4. summarise_mass_casualty_counts() and mass_casualty_dow_rate() are
#      correct on inputs whose answers are computable by hand, so a table
#      agreeing with the summary is not two copies of one error.
#
# Assertion 4 is what keeps assertion 3 from being circular: 3 would hold for
# any summary the code happened to produce.

suppressPackageStartupMessages({
  library(dplyr)
})

source("R/constants.R")
source("R/mass_casualty.R")

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
REPLICATIONS_PATH <- file.path("data", "mass_casualty", "mass_casualty_replications.csv")

#' Tracked per-arm count summary (total casualties, events)
COUNT_SUMMARY_PATH <- file.path("data", "mass_casualty", "mass_casualty_count_summary.csv")

#' Tracked per-arm, per-origin died-of-wounds summary
DOW_SUMMARY_PATH <- file.path("data", "mass_casualty", "mass_casualty_dow_summary.csv")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Tolerance on a figure the paper prints rounded to its last decimal place
PRINT_TOL <- 0.005

# ── 1. The code's parameters are the ones the supplement documents ───────────

cat("\n-- the protocol's parameters match the supplement --\n")

supplement <- paste(readLines(SUPPLEMENT_PATH, warn = FALSE), collapse = "\n")

#' Read one mass casualty protocol parameter the supplement states in a marker
#'
#' @param name Marker name, as it appears after "MASS_CASUALTY ".
#' @return The marker's value as a character string, or NA where absent.
mass_casualty_marker <- function(name) {
  m <- regmatches(supplement,
                  regexpr(sprintf("<!-- MASS_CASUALTY %s=[^ ]+ -->", name), supplement))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- MASS_CASUALTY [^=]+=(.*) -->$", "\\1", m)
}

held <- list(replications = MASS_CASUALTY_REPLICATIONS, days = MASS_CASUALTY_DAYS,
             seed = MASS_CASUALTY_SEED)
for (param in names(held)) {
  stated <- suppressWarnings(as.numeric(mass_casualty_marker(param)))
  report(!is.na(stated) && stated == held[[param]],
         "the supplement states %s = %s and the code holds %s",
         param, format(stated), format(held[[param]]))
}

stated_arms <- mass_casualty_marker("arms")
parsed_arms <- if (is.na(stated_arms)) {
  numeric(0)
} else {
  as.numeric(trimws(strsplit(stated_arms, ",")[[1]]))
}
report(isTRUE(all.equal(parsed_arms, MASS_CASUALTY_ARMS)),
       "the supplement states the arms %s and the code holds %s",
       paste(parsed_arms, collapse = ","), paste(MASS_CASUALTY_ARMS, collapse = ","))

# ── 2. The tracked responses are the experiment the supplement documents ─────

cat("\n-- the tracked evidence set is that experiment --\n")

per_rep <- if (file.exists(REPLICATIONS_PATH)) {
  read.csv(REPLICATIONS_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked per-replication responses %s exist", REPLICATIONS_PATH)
  NULL
}

count_summary <- if (file.exists(COUNT_SUMMARY_PATH)) {
  read.csv(COUNT_SUMMARY_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked count summary %s exists", COUNT_SUMMARY_PATH)
  NULL
}

dow_summary <- if (file.exists(DOW_SUMMARY_PATH)) {
  read.csv(DOW_SUMMARY_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked died-of-wounds summary %s exists", DOW_SUMMARY_PATH)
  NULL
}

if (!is.null(per_rep)) {
  report(setequal(unique(per_rep$rate_per_day), MASS_CASUALTY_ARMS),
         "the tracked replications carry the documented arms")
  counts <- tapply(per_rep$replication, per_rep$rate_per_day, length)
  report(all(counts == MASS_CASUALTY_REPLICATIONS),
         "every arm carries %d replications (found %s)", MASS_CASUALTY_REPLICATIONS,
         paste(counts, collapse = ","))
  required_cols <- c("total_casualties", "n_events", "n_ordinary", "dow_ordinary",
                     "n_event", "dow_event")
  report(all(required_cols %in% names(per_rep)),
         "every response the table needs is carried (missing: %s)",
         paste(setdiff(required_cols, names(per_rep)), collapse = ", "))
}

if (!is.null(count_summary)) {
  report(setequal(unique(count_summary$rate_per_day), MASS_CASUALTY_ARMS),
         "the tracked count summary carries the documented arms")
  report(all(count_summary$n_reps == MASS_CASUALTY_REPLICATIONS),
         "every tracked count summary row carries %d replications", MASS_CASUALTY_REPLICATIONS)
}

if (!is.null(dow_summary)) {
  report(setequal(unique(dow_summary$rate_per_day), MASS_CASUALTY_ARMS),
         "the tracked died-of-wounds summary carries the documented arms")
  report(setequal(unique(dow_summary$origin), c("ordinary", "event")),
         "the tracked died-of-wounds summary carries both origins")
}

# The summaries have to be the reduction of the per-replication set beside
# them, or the tracked files are independent claims rather than one
# measurement.
if (!is.null(per_rep) && !is.null(count_summary)) {
  recomputed <- do.call(rbind, lapply(c("total_casualties", "n_events"), function(r) {
    agg <- aggregate(per_rep[[r]], by = list(rate_per_day = per_rep$rate_per_day), FUN = mean)
    data.frame(response = r, rate_per_day = agg$rate_per_day, mean = agg$x)
  }))
  joined <- merge(count_summary, recomputed, by = c("response", "rate_per_day"))
  report(nrow(joined) == nrow(count_summary) &&
           all(abs(joined$mean.x - joined$mean.y) < TOL),
         "the tracked count summary is the reduction of the tracked per-replication responses")
}

if (!is.null(per_rep) && !is.null(dow_summary)) {
  recomputed_dow <- do.call(rbind, lapply(MASS_CASUALTY_ARMS, function(rate) {
    arm <- per_rep[per_rep$rate_per_day == rate, ]
    rbind(
      data.frame(rate_per_day = rate, origin = "ordinary",
                 n = sum(arm$n_ordinary), dow = sum(arm$dow_ordinary)),
      data.frame(rate_per_day = rate, origin = "event",
                 n = sum(arm$n_event), dow = sum(arm$dow_event))
    )
  }))
  joined_dow <- merge(dow_summary, recomputed_dow, by = c("rate_per_day", "origin"))
  dow_pools_ok <- nrow(joined_dow) == nrow(dow_summary) &&
    all(joined_dow$n.x == joined_dow$n.y) && all(joined_dow$dow.x == joined_dow$dow.y)
  report(dow_pools_ok,
         "the tracked died-of-wounds summary pools the counts the per-replication responses carry")
}

# ── 3. The published table matches the tracked measurement ───────────────────

cat("\n-- every published figure matches the tracked measurement --\n")

paper <- readLines(PAPER_PATH, warn = FALSE)

#' The rows of the marked mass casualty table in the paper
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
#' @return The number the cell opens with, or NA where it carries none (e.g.
#'   "not applicable").
leading_figure <- function(cell) {
  cell <- gsub("−", "-", cell)
  suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", sub("[ %].*$", "", cell))))
}

#' Check one published count-summary row against the tracked measurement
#'
#' @param rows The table's lines.
#' @param label Regular expression matching the row's label cell.
#' @param response Response name the row reports.
#' @param digits Decimal places the paper prints this row to, matching
#'   `scripts/run_mass_casualty.R`'s `print_count_row()`.
#' @return Invisible NULL.
check_count_row <- function(rows, label, response, digits = 1) {
  row <- rows[grepl(paste0("^\\| ", label), rows)]
  if (length(row) != 1) {
    report(FALSE, "the paper prints one '%s' row (found %d)", label, length(row))
    return(invisible(NULL))
  }
  cells <- table_cells(row)
  tol <- 0.5 * 10^(-digits) + PRINT_TOL
  for (k in seq_along(MASS_CASUALTY_ARMS)) {
    tracked <- if (is.null(count_summary)) NA_real_ else {
      hit <- count_summary[count_summary$rate_per_day == MASS_CASUALTY_ARMS[k] &
                             count_summary$response == response, ]
      if (nrow(hit) == 1) hit$mean else NA_real_
    }
    printed <- if (k <= length(cells)) leading_figure(cells[k]) else NA_real_
    ok <- !is.na(printed) && !is.na(tracked) && abs(printed - round(tracked, digits)) < tol
    report(ok, "'%s' in column %d prints %s against the tracked %s",
           label, k, format(printed), format(round(tracked, digits)))
  }
  invisible(NULL)
}

#' Check one published died-of-wounds row against the tracked measurement
#'
#' @param rows The table's lines.
#' @param label Regular expression matching the row's label cell.
#' @param origin "ordinary" or "event".
#' @return Invisible NULL.
check_dow_row <- function(rows, label, origin) {
  row <- rows[grepl(paste0("^\\| ", label), rows)]
  if (length(row) != 1) {
    report(FALSE, "the paper prints one '%s' row (found %d)", label, length(row))
    return(invisible(NULL))
  }
  cells <- table_cells(row)
  for (k in seq_along(MASS_CASUALTY_ARMS)) {
    hit <- if (is.null(dow_summary)) NULL else {
      dow_summary[dow_summary$rate_per_day == MASS_CASUALTY_ARMS[k] &
                    dow_summary$origin == origin, ]
    }
    printed_cell <- if (k <= length(cells)) cells[k] else NA_character_
    if (!is.null(hit) && nrow(hit) == 1 && (is.na(hit$n) || hit$n == 0)) {
      report(!is.na(printed_cell) && grepl("not applicable", printed_cell),
             "'%s' in column %d prints 'not applicable' where the tracked set has no casualties",
             label, k)
      next
    }
    tracked_pct <- if (!is.null(hit) && nrow(hit) == 1) 100 * hit$rate else NA_real_
    printed <- if (!is.na(printed_cell)) leading_figure(printed_cell) else NA_real_
    ok <- !is.na(printed) && !is.na(tracked_pct) && abs(printed - round(tracked_pct, 2)) < PRINT_TOL
    report(ok, "'%s' in column %d prints %s against the tracked %s",
           label, k, format(printed), format(round(tracked_pct, 2)))
  }
  invisible(NULL)
}

table_rows <- paper_table("<!-- MASS CASUALTY TABLE -->")
if (!is.null(table_rows)) {
  check_count_row(table_rows, "Average total casualties/run", "total_casualties", digits = 1)
  check_count_row(table_rows, "Average events/run", "n_events", digits = 2)
  check_dow_row(table_rows, "Died-of-wounds rate, ordinary casualties", "ordinary")
  check_dow_row(table_rows, "Died-of-wounds rate, event casualties", "event")
}

# ── 4. The reduction functions are correct on a known input ──────────────────

cat("\n-- the reduction functions are correct on a known input --\n")

# Five values whose mean is 3 and whose sample standard deviation is exactly
# sqrt(2.5), so the half-width is qt(0.975, 4) * sqrt(2.5 / 5) and can be
# written down without reference to the function under test.
known <- data.frame(rate_per_day = 0, replication = 1:5,
                    total_casualties = c(1, 2, 3, 4, 5), n_events = c(0, 0, 0, 0, 0))
computed <- summarise_mass_casualty_counts(known)
expected_half <- qt(0.975, df = 4) * sqrt(2.5) / sqrt(5)

tc_row <- computed[computed$response == "total_casualties", ]
report(nrow(tc_row) == 1, "the count summary returns one row per response")
if (nrow(tc_row) == 1) {
  report(abs(tc_row$mean - 3) < TOL, "the mean is the arithmetic mean (%.6f against 3)",
         tc_row$mean)
  report(abs(tc_row$ci_lower - (3 - expected_half)) < TOL &&
           abs(tc_row$ci_upper - (3 + expected_half)) < TOL,
         "the interval is the Student t one at 95%% ([%.6f, %.6f] against [%.6f, %.6f])",
         tc_row$ci_lower, tc_row$ci_upper, 3 - expected_half, 3 + expected_half)
  report(tc_row$n_reps == 5, "the replication count is the number of values (%d)",
         tc_row$n_reps)
}

# A response measured in one replication alone carries an interval equal to
# its mean, since there is no spread to estimate a half-width from.
one_rep <- data.frame(rate_per_day = 0, replication = 1L,
                      total_casualties = 4, n_events = 0)
single <- summarise_mass_casualty_counts(one_rep)
single_tc <- single[single$response == "total_casualties", ]
report(nrow(single_tc) == 1 && single_tc$ci_lower == 4 && single_tc$ci_upper == 4,
       "a single-replication response carries an interval equal to its mean")

# 6 deaths of 2000 at risk: an exact binomial (Clopper-Pearson) 95% interval
# on that proportion is computable independently of the function under test.
known_dow <- data.frame(n_ordinary = c(1000, 1000), dow_ordinary = c(3, 3))
dow_rate_computed <- mass_casualty_dow_rate(known_dow, "n_ordinary", "dow_ordinary")
expected_test <- binom.test(6, 2000)
report(dow_rate_computed$n == 2000 && dow_rate_computed$dow == 6,
       "the died-of-wounds rate pools counts across replications (n=%d, dow=%d)",
       dow_rate_computed$n, dow_rate_computed$dow)
report(abs(dow_rate_computed$rate - 6 / 2000) < TOL,
       "the pooled rate is dow / n (%.6f against %.6f)", dow_rate_computed$rate, 6 / 2000)
report(abs(dow_rate_computed$ci_lower - expected_test$conf.int[1]) < TOL &&
         abs(dow_rate_computed$ci_upper - expected_test$conf.int[2]) < TOL,
       "the interval is the exact binomial one ([%.6f, %.6f] against [%.6f, %.6f])",
       dow_rate_computed$ci_lower, dow_rate_computed$ci_upper,
       expected_test$conf.int[1], expected_test$conf.int[2])

# No casualties at risk in a subset (the background-only arm's event column)
# is reported as not applicable rather than as a rate of zero.
empty_dow <- data.frame(n_event = c(0, 0), dow_event = c(0, 0))
empty_rate <- mass_casualty_dow_rate(empty_dow, "n_event", "dow_event")
report(empty_rate$n == 0 && is.na(empty_rate$rate),
       "an origin with no casualties at risk reports rate NA rather than 0")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All mass casualty stress test protocol checks passed.\n")
quit(status = 0)
