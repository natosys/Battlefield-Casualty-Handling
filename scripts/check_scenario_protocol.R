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
# paper's centrepiece, the experiment its opening finding rests on. Until Issue #384 it wrote its
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
#      docs/Methods.md documents in a marker comment.
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

#' The methods_text, which documents the experiment's design
METHODS_PATH <- file.path("docs", "Methods.md")

#' The companion paper, which prints the experiment's figures
PAPER_PATH <- file.path("docs", "Results.md")

#' Tracked casualty totals the published totals table is printed from
TOTALS_PATH <- file.path("data", "scenarios", "scenario_comparison_totals.csv")

#' Tracked per-pool queue summary the published queue table is printed from
GROUPS_PATH <- file.path("data", "scenarios", "scenario_queue_groups.csv")

#' Tracked per-replication per-pool queue series the summary is reduced from
GROUP_REPS_PATH <- file.path("data", "scenarios",
                             "scenario_queue_group_replications.csv")

#' Tracked per-holder queue and utilisation summary the holders table is printed from
HOLDERS_PATH <- file.path("data", "scenarios", "scenario_transport_holders.csv")

#' Tracked per-replication holder series the holder summary is reduced from
HOLDER_REPS_PATH <- file.path("data", "scenarios",
                              "scenario_transport_holder_replications.csv")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Tolerance on a figure the paper prints rounded to its last decimal place
#'
#' @details Half a unit in the last printed place, on the scale each row is
#'   compared at, so a share printed as a percentage and a queue printed to
#'   three decimals are both covered.
PRINT_TOL <- 0.005

# ── 1. The code's parameters are the ones the methods paper documents ───────────

cat("\n-- the protocol's parameters match the methods paper --\n")

held <- list(
  replications = SCENARIO_REPLICATIONS,
  days         = SCENARIO_DAYS,
  window_days  = SCENARIO_WINDOW_DAYS,
  seed         = SCENARIO_SEED
)

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one scenario protocol parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "SCENARIO ".
#' @return The marker's value as a character string, or NA where absent.
#'
#' @details Marked rather than parsed out of the prose, on the convention
#'   `scripts/check_airlift_protocol.R` established: a check that guesses which
#'   number in a paragraph is the replication count fails for reasons that have
#'   nothing to do with the protocol.
scenario_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- SCENARIO %s=[^ ]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- SCENARIO [^=]+=(.*) -->$", "\\1", m)
}

for (param in names(held)) {
  stated <- suppressWarnings(as.numeric(scenario_marker(param)))
  report(!is.na(stated) && stated == held[[param]],
         "the methods paper states %s = %s and the code holds %s",
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
       "the methods paper states the profiles %s and the code holds %s",
       paste(parsed_profiles, collapse = ","), paste(held_profiles, collapse = ","))

# The four holders, their shared or integral kind and the lead-medic rule the
# integral ones are measured on are part of the design, so the set is asserted
# rather than inferred from whatever the tracked file happens to hold.
report(identical(SCENARIO_TRANSPORT_HOLDERS$holder,
                 c("PMV Ambulance fleet", "HX2 40M fleet",
                   "R2B evacuation crews", "R2E evacuation sections")) &&
         identical(SCENARIO_TRANSPORT_HOLDERS$kind,
                   c("shared", "shared", "integral", "integral")),
       "the transport holders are the two shared fleets then the two integral elements")
lead_only <- c("c_r2b_evac_1_medic_1_t1", "c_r2eheavy_evac_3_medic_1_t2")
second    <- c("c_r2b_evac_1_medic_2_t1", "c_r2eheavy_evac_3_medic_2_t2")
report(all(mapply(grepl, SCENARIO_TRANSPORT_HOLDERS$pattern[3:4], lead_only)) &&
         !any(mapply(grepl, SCENARIO_TRANSPORT_HOLDERS$pattern[3:4], second)),
       "an evacuation crew is measured on its lead medic alone")

# The closing window is the one part of the estimator that depends on the
# horizon, so it is asserted on a constructed pool whose answer is computable by
# hand: one bed queueing a single casualty from day 8 of 10. Over a window
# longer than the campaign the mean is the whole campaign's, 2 queued days in
# 10 or 0.2; over a closing window of four days it is 2 queued days in 4 or 0.5.
window_mon <- list(resources = data.frame(
  replication = 1, resource = "b_r2b_hold_1_t1",
  time = c(0, 8) * DAY_MIN, server = 0, queue = c(0, 1), capacity = 1
))
whole <- scenario_queue_groups_by_replication(window_mon, n_days = 10, window_days = 90)
closing <- scenario_queue_groups_by_replication(window_mon, n_days = 10, window_days = 4)
report(abs(whole$mean_q - 0.2) < 1e-9,
       "a campaign shorter than the window is averaged whole: %.4f, expected 0.2000",
       whole$mean_q)
report(abs(closing$mean_q - 0.5) < 1e-9,
       "a longer campaign reads its closing window: %.4f, expected 0.5000",
       closing$mean_q)

# Utilisation on a constructed crew whose answer is computable by hand: a
# two-crew pool whose lead medic is busy for the second half of a one-day window
# is 0.5 busy units over two established, 0.25. The second medic is busy
# throughout and must not reach the figure, and a holder with no rows at all
# reports zeros rather than dropping out of the replication.
crew_mon <- list(resources = data.frame(
  replication = 1,
  resource    = c("c_r2b_evac_1_medic_1_t1", "c_r2b_evac_1_medic_1_t1",
                  "c_r2b_evac_1_medic_2_t1"),
  time        = c(0, DAY_MIN / 2, 0),
  server      = c(0, 1, 1), queue = c(0, 0, 0), capacity = 1
))
crew_est <- setNames(c(1L, 1L, 2L, 1L), SCENARIO_TRANSPORT_HOLDERS$holder)
crew <- scenario_transport_holders_by_replication(crew_mon, n_days = 1, crew_est,
                                                  window_days = 90)
crew_row <- crew[crew$holder == "R2B evacuation crews", ]
report(nrow(crew) == nrow(SCENARIO_TRANSPORT_HOLDERS) &&
         abs(crew_row$utilisation - 0.25) < 1e-9,
       "crew utilisation is the lead medic's busy share over the established crews (%.4f, expected 0.2500)",
       crew_row$utilisation)
report(all(crew$utilisation[crew$holder != "R2B evacuation crews"] == 0),
       "a holder never seized reports zero utilisation rather than dropping out")

# ── 2. The tracked responses are the experiment the methods paper documents ─────

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

holders <- if (file.exists(HOLDERS_PATH)) {
  read.csv(HOLDERS_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked holder summary %s exists", HOLDERS_PATH)
  NULL
}

holder_reps <- if (file.exists(HOLDER_REPS_PATH)) {
  read.csv(HOLDER_REPS_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked per-replication holder series %s exists", HOLDER_REPS_PATH)
  NULL
}

if (!is.null(holders)) {
  report(setequal(unique(holders$scenario), held_profiles),
         "the tracked holder summary carries the documented profiles")
  report(all(holders$n_reps == held$replications),
         "every tracked holder row carries %s replications", format(held$replications))
  report(setequal(unique(holders$holder), SCENARIO_TRANSPORT_HOLDERS$holder) &&
           nrow(holders) == length(held_profiles) * nrow(SCENARIO_TRANSPORT_HOLDERS),
         "the tracked holder summary carries every holder at every profile once")
  report(all(holders$util_mean >= 0 & holders$util_mean <= 1),
         "every tracked utilisation is a share between 0 and 1")
}

if (!is.null(holders) && !is.null(holder_reps)) {
  recomputed <- bind_rows(lapply(split(holder_reps, holder_reps$scenario), function(r) {
    cbind(scenario = r$scenario[1], summarise_scenario_transport_holders(r))
  }))
  joined <- merge(holders, recomputed, by = c("scenario", "holder"))
  report(nrow(joined) == nrow(holders) &&
           all(abs(joined$q_mean.x - joined$q_mean.y) < 1e-9) &&
           all(abs(joined$util_mean.x - joined$util_mean.y) < 1e-9) &&
           all(abs(joined$util_ci_lower.x - joined$util_ci_lower.y) < 1e-9) &&
           all(abs(joined$util_ci_upper.x - joined$util_ci_upper.y) < 1e-9),
         "the tracked holder summary is the reduction of the tracked per-replication series")
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

totals_rows <- paper_table("<!-- GEN scenario_totals -->")
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
    check_published_row("<!-- GEN scenario_totals -->", totals_rows,
                        spec[[1]], expected, spec[[4]])
  }
}

queue_rows <- paper_table("<!-- GEN scenario_queue -->")
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
    check_published_row("<!-- GEN scenario_queue -->", queue_rows,
                        spec[[1]], expected, 3)
  }
}

holder_rows <- paper_table("<!-- GEN transport_holders -->")
if (!is.null(holder_rows) && !is.null(holders)) {
  report(length(table_cells(holder_rows[1])) == 2 * length(held_profiles) + 1,
         "the holders table prints a kind and a queue and utilisation per profile")
  for (h in SCENARIO_TRANSPORT_HOLDERS$holder) {
    expected <- unlist(lapply(held_profiles, function(s) {
      c(tracked_value(holders, "holder", h, s, "q_mean"),
        100 * tracked_value(holders, "holder", h, s, "util_mean"))
    }))
    row <- holder_rows[grepl(paste0("^\\| ", h), holder_rows)]
    cells <- if (length(row) == 1) table_cells(row)[-1] else character(0)
    report(length(cells) == 4, "the paper prints one '%s' row under the holders table", h)
    if (length(cells) == 4) {
      digits <- c(3, 1, 3, 1)
      for (k in 1:4) {
        printed <- leading_figure(cells[k])
        report(!is.na(printed) && abs(printed - round(expected[k], digits[k])) <
                 10^-digits[k] / 2 + 1e-9,
               "'%s' column %d of the holders table prints %s against the data's %s",
               h, k, format(printed), format(round(expected[k], digits[k])))
      }
    }
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

# The holder summary uses the same Student t construction for both responses.
# Utilisation 0.1 to 0.5 in steps of 0.1 has mean 0.3 and the same half-width
# as the queue example scaled by a tenth.
known_holder <- data.frame(holder = "R2B evacuation crews", kind = "integral",
                           replication = 1:5, mean_q = c(1, 2, 3, 4, 5),
                           utilisation = c(1, 2, 3, 4, 5) / 10)
holder_summary <- summarise_scenario_transport_holders(known_holder)
report(nrow(holder_summary) == 1 &&
         abs(holder_summary$q_ci_upper - (3 + expected_half)) < TOL &&
         abs(holder_summary$util_mean - 0.3) < TOL &&
         abs(holder_summary$util_ci_lower - (0.3 - expected_half / 10)) < TOL,
       "the holder summary's means and Student t intervals are right on a known input")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All comparative scenario protocol checks passed.\n")
quit(status = 0)
