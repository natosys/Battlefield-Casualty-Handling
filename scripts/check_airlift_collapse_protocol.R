#!/usr/bin/env Rscript
##############################################################################
## scripts/check_airlift_collapse_protocol.R                                ##
## Regression check — the airlift collapse experiment's parameters, its      ##
## classifier and its published table agree                                  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_airlift_collapse_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The strategic airlift reliability sweep is the longest
# experiment in the project, at 180 replication-years, and its response is a
# classification rather than a mean: a campaign is counted as collapsed where
# its R2E holding queue over the closing 90 days averages twenty casualties or
# more. Nobody re-runs it to audit a change, so three things have to be checked
# without running it. Its parameters must be the ones the supplement documents,
# or the paper describes an experiment the code does not perform. Its classifier
# must be correct on inputs whose answers are computable by hand, since a
# classifier tested only against the experiment's own output would agree with
# any threshold or window it happened to apply. And the tracked evidence set
# must carry the table the companion paper prints, since the paper's cliff at
# 15% is read straight off it.
#
# What this asserts:
#
#   1. Every protocol parameter in R/airlift.R equals the value
#      docs/Multi_Run_Supplement.md documents in a marker comment.
#   2. collapse_response() selects the closing window, and nothing before it.
#   3. It averages each replication over that window's days rather than
#      sampling one of them, and classifies at an inclusive threshold.
#   4. It reads its own subject and series, ignoring the other pools and
#      responses the same long-horizon series carries.
#   5. summarise_collapse() returns the Clopper-Pearson interval, which on a
#      zero-count arm has a positive upper bound where a normal interval has
#      zero width.
#   6. The tracked data/airlift/ collapse summary carries the documented
#      probabilities and replication count, and matches the table
#      docs/Multi_Run_Analysis.md prints, row for row.
#
# Assertions 2 to 4 are made against series constructed here rather than
# measured, so each has an answer arrived at without the function under test.

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
#' @details Anything other than TRUE is a failure, NA included. A quantity
#'   compared here becomes NA when the code under test is wrong rather than when
#'   the assertion is inapplicable, and `if (ok)` on an NA raises an error that
#'   stops the run at the first such assertion instead of reporting it and
#'   carrying on.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' Companion paper the published table is read from
PAPER_PATH <- file.path("docs", "Multi_Run_Analysis.md")

#' Supplement the protocol parameters are read from
SUPPLEMENT_PATH <- file.path("docs", "Multi_Run_Supplement.md")

#' Tracked collapse summary the paper's table derives from
SUMMARY_PATH <- file.path("data", "airlift", "airlift_collapse.csv")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Tolerance on a figure the paper prints rounded to one decimal place
PRINT_TOL <- 0.05

# ── 1. The code's parameters are the ones the supplement documents ───────────

cat("\n-- the protocol's parameters match the supplement --\n")

supplement <- paste(readLines(SUPPLEMENT_PATH, warn = FALSE), collapse = "\n")

#' Read one collapse parameter the supplement states in a marker comment
#'
#' @param name Marker name, as it appears after "COLLAPSE ".
#' @return The marker's value as a character string, or NA where absent.
#'
#' @details The parameters are marked rather than parsed out of the prose,
#'   because a check that guesses which number in a paragraph is the replication
#'   count fails for reasons that have nothing to do with the protocol. The
#'   marker is the author's statement of what the sentence beneath it claims.
collapse_marker <- function(name) {
  m <- regmatches(supplement,
                  regexpr(sprintf("<!-- COLLAPSE %s=[^ ]+ -->", name), supplement))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- COLLAPSE [^=]+=(.*) -->$", "\\1", m)
}

for (param in list(list("days", AIRLIFT_COLLAPSE_DAYS),
                   list("replications", AIRLIFT_COLLAPSE_REPLICATIONS),
                   list("window_days", AIRLIFT_COLLAPSE_WINDOW_DAYS),
                   list("threshold", AIRLIFT_COLLAPSE_THRESHOLD))) {
  stated <- suppressWarnings(as.numeric(collapse_marker(param[[1]])))
  report(!is.na(stated) && stated == param[[2]],
         "the supplement states %s = %s and the code holds %s",
         param[[1]], format(stated), format(param[[2]]))
}

stated_probabilities <- collapse_marker("probabilities")
parsed <- if (is.na(stated_probabilities)) {
  numeric(0)
} else {
  suppressWarnings(as.numeric(strsplit(stated_probabilities, ",")[[1]]))
}
report(length(parsed) == length(AIRLIFT_COLLAPSE_PROBABILITIES) &&
         !any(is.na(parsed)) &&
         all(abs(parsed - AIRLIFT_COLLAPSE_PROBABILITIES) < TOL),
       "the supplement states probabilities %s and the code holds %s",
       paste(format(parsed), collapse = ","),
       paste(format(AIRLIFT_COLLAPSE_PROBABILITIES), collapse = ","))

report(identical(AIRLIFT_COLLAPSE_SUBJECT, "R2E holding beds"),
       "the classified subject is the R2E holding pool, found '%s'",
       AIRLIFT_COLLAPSE_SUBJECT)
report(AIRLIFT_COLLAPSE_WINDOW_DAYS < AIRLIFT_COLLAPSE_DAYS,
       "the closing window (%d days) is shorter than the horizon (%d days)",
       AIRLIFT_COLLAPSE_WINDOW_DAYS, AIRLIFT_COLLAPSE_DAYS)

# ── 2-4. The classifier is right on inputs with hand-computed answers ────────

cat("\n-- the classifier is right on a constructed series --\n")

#' Build a long-horizon series carrying one value per replication-day
#'
#' @param values Matrix of values, replications in rows and days in columns.
#' @param subject Subject label the rows carry.
#' @param series Series label the rows carry.
#' @return A data frame shaped as run_long_horizon() returns.
make_series <- function(values, subject = AIRLIFT_COLLAPSE_SUBJECT,
                        series = "mean_queue") {
  n_reps <- nrow(values)
  n_days <- ncol(values)
  data.frame(
    replication = rep(seq_len(n_reps), each = n_days),
    day         = rep(seq_len(n_days), times = n_reps),
    series      = series,
    subject     = subject,
    value       = as.numeric(t(values)),
    stringsAsFactors = FALSE
  )
}

# Four replications over ten days. The closing window of four days is days 7 to
# 10, so each replication's response is the mean of its last four values:
#   rep 1: 100, 100, 100, 0, 0, 0, 10, 10, 10, 10 -> 10.0, clear
#   rep 2:   0,   0,   0, 0, 0, 0, 19, 20, 21, 20 -> 20.0, collapsed (inclusive)
#   rep 3:   0,   0,   0, 0, 0, 0,  0,  0,  0, 79 -> 19.75, clear
#   rep 4:   0,   0,   0, 0, 0, 0, 40, 40, 40, 40 -> 40.0, collapsed
constructed <- rbind(
  c(100, 100, 100, 0, 0, 0, 10, 10, 10, 10),
  c(0, 0, 0, 0, 0, 0, 19, 20, 21, 20),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 79),
  c(0, 0, 0, 0, 0, 0, 40, 40, 40, 40)
)
expected_closing <- c(10, 20, 19.75, 40)
expected_collapsed <- c(FALSE, TRUE, FALSE, TRUE)

response <- collapse_response(make_series(constructed), n_days = 10,
                              window_days = 4, threshold = 20)

report(nrow(response) == 4 && identical(response$replication, 1:4),
       "one row per replication, in replication order (%d rows)", nrow(response))
report(all(abs(response$closing_queue - expected_closing) < TOL),
       "the closing means are %s, expected %s",
       paste(format(response$closing_queue), collapse = ", "),
       paste(format(expected_closing), collapse = ", "))
report(identical(as.logical(response$collapsed), expected_collapsed),
       "the classification is %s, expected %s",
       paste(response$collapsed, collapse = ", "),
       paste(expected_collapsed, collapse = ", "))

# Replication 1 carries its largest values before the window and replication 3
# carries a single value inside it that exceeds the threshold four-fold. Both
# are clear, so a classifier reading the whole campaign or the window's worst
# day rather than its mean would disagree here.
report(!isTRUE(response$collapsed[1]),
       "a replication whose peak precedes the window is clear (mean %.2f)",
       response$closing_queue[1])
report(!isTRUE(response$collapsed[3]),
       "a replication averaging below the threshold over the window is clear")
report(abs(response$closing_queue[3] - 19.75) < TOL,
       "its single day of 79 averages to %.2f rather than being taken alone",
       response$closing_queue[3])

# The same series with the other pools and responses a long-horizon run carries
# interleaved. The classifier must return the same answer.
noise <- rbind(
  make_series(constructed * 3, subject = "R2E intensive care beds"),
  make_series(constructed * 5, series = "arrivals"),
  make_series(constructed)
)
noisy_response <- collapse_response(noise, n_days = 10, window_days = 4,
                                    threshold = 20)
report(isTRUE(all.equal(noisy_response, response)),
       "the answer is unchanged by other pools and responses in the series")

absent_subject <- make_series(constructed, subject = "R2B holding beds")
absent <- collapse_response(absent_subject, n_days = 10, window_days = 4,
                            threshold = 20)
report(nrow(absent) == 0,
       "a series carrying no rows for the subject returns no responses")

# ── 5. The interval is Clopper-Pearson ───────────────────────────────────────

cat("\n-- the interval is the exact binomial one --\n")

summary_of <- summarise_collapse(response)
report(summary_of$n_reps == 4 && summary_of$n_collapsed == 2,
       "2 of 4 constructed replications collapse (%d of %d)",
       summary_of$n_collapsed, summary_of$n_reps)
report(abs(summary_of$median_queue - 19.875) < TOL,
       "the median closing queue is %.3f, expected 19.875", summary_of$median_queue)
report(abs(summary_of$worst_queue - 40) < TOL,
       "the worst closing queue is %.1f, expected 40", summary_of$worst_queue)
report(abs(summary_of$ci_lower - qbeta(0.025, 2, 3)) < TOL &&
         abs(summary_of$ci_upper - qbeta(0.975, 3, 2)) < TOL,
       "the interval is [%.4f, %.4f], the exact binomial one for 2 of 4",
       summary_of$ci_lower, summary_of$ci_upper)

zero_arm <- data.frame(replication = 1:30, closing_queue = rep(0.5, 30),
                       collapsed = rep(FALSE, 30))
zero_summary <- summarise_collapse(zero_arm)
report(zero_summary$rate == 0 && zero_summary$ci_lower == 0,
       "a zero-count arm reports a rate and lower bound of zero")
report(zero_summary$ci_upper > 0.1 && zero_summary$ci_upper < 0.13,
       "its upper bound is %.4f, positive where a normal interval is zero-width",
       zero_summary$ci_upper)

full_arm <- data.frame(replication = 1:30, closing_queue = rep(90, 30),
                       collapsed = rep(TRUE, 30))
report(summarise_collapse(full_arm)$ci_upper == 1,
       "an all-collapsed arm reports an upper bound of one")

# ── 6. The tracked summary is the table the paper prints ─────────────────────

cat("\n-- the tracked summary matches the published table --\n")

if (!file.exists(SUMMARY_PATH)) {
  report(FALSE, "the tracked collapse summary %s exists", SUMMARY_PATH)
} else {
  tracked <- read.csv(SUMMARY_PATH, stringsAsFactors = FALSE)

  report(nrow(tracked) == length(AIRLIFT_COLLAPSE_PROBABILITIES) &&
           all(abs(tracked$probability - AIRLIFT_COLLAPSE_PROBABILITIES) < TOL),
         "the tracked summary carries the %d documented probabilities",
         length(AIRLIFT_COLLAPSE_PROBABILITIES))
  report(all(tracked$n_reps == AIRLIFT_COLLAPSE_REPLICATIONS),
         "every arm carries %d replications", AIRLIFT_COLLAPSE_REPLICATIONS)

  paper <- readLines(PAPER_PATH, warn = FALSE)
  pattern <- "^\\| ([0-9]+)% ?[^|]*\\| ([0-9]+) of ([0-9]+) \\|"
  rows <- paper[grepl(pattern, paper)]
  report(length(rows) == nrow(tracked),
         "the paper prints %d collapse rows against %d tracked arms",
         length(rows), nrow(tracked))

  for (row in rows) {
    fields <- trimws(strsplit(sub("^\\|", "", sub("\\|$", "", row)), "\\|")[[1]])
    probability <- as.numeric(sub("%.*$", "", fields[1])) / 100
    arm <- tracked[abs(tracked$probability - probability) < TOL, ]
    if (nrow(arm) != 1) {
      report(FALSE, "the paper's %s row has one tracked arm (found %d)",
             fields[1], nrow(arm))
      next
    }
    counts <- as.numeric(regmatches(fields[2],
                                    gregexpr("[0-9]+", fields[2]))[[1]])
    median_queue <- as.numeric(fields[5])
    worst_queue <- as.numeric(fields[6])
    report(counts[1] == arm$n_collapsed && counts[2] == arm$n_reps,
           "the paper's %s row states %d of %d and the data holds %d of %d",
           fields[1], counts[1], counts[2], arm$n_collapsed, arm$n_reps)
    report(abs(median_queue - arm$median_queue) < PRINT_TOL &&
             abs(worst_queue - arm$worst_queue) < PRINT_TOL,
           "its median %.2f and worst %.1f match the data's %.2f and %.1f",
           median_queue, worst_queue, arm$median_queue, arm$worst_queue)
  }
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All airlift collapse protocol checks passed.\n")
quit(status = 0)
