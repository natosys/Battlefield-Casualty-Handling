#!/usr/bin/env Rscript
##############################################################################
## scripts/check_role4_demand_protocol.R                                    ##
## Regression check — the Role 4 bed demand experiment's parameters, its    ##
## reduction and its published figures agree                                ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_role4_demand_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The Role 4 bed demand section of docs/Results.md
# prints a daily census divided by ward phase and origin, the theatre demand
# owed with it and the response of the peak and the closing-window mean to the
# forward levers. Nobody re-runs 360 replication-years to audit a paragraph, so
# the agreement between the section, the tracked evidence behind it and the
# protocol docs/Methods.md documents has to be checked without running the
# model.
#
# What this asserts:
#
#   1. Every protocol parameter in R/role4_demand.R, and the swept
#      cancellation probabilities, equal the values docs/Methods.md documents
#      in a marker comment.
#   2. The tracked evidence set carries both intensities at the documented
#      replication count and horizon, and the cancellation arm carries every
#      documented probability at that count.
#   3. The tracked responses are the reduction of the tracked series, the
#      tracked summary is the reduction of the responses, the tracked daily
#      mean is the reduction of the series, and the census a composition
#      divides is conserved: the wards sum to the total on every day, and so
#      do the origins.
#   4. The census agrees with the evidence sets that already report a Role 4
#      peak: the peak of each replication equals the strategic airlift
#      baseline's under the same seed, the cancellation arm's equals the
#      airlift reliability arm's, and the shipped policy's equals the policy
#      sweep's, with the operations owed equal to its as well.
#   5. Every figure the section prints matches the tracked summary.
#   6. The reductions are correct on inputs whose answers are computable by
#      hand, so a table agreeing with the summary is not two copies of one
#      error.
#   7. Demand only: no tracked file or module carries a capacity, queue or
#      shortfall for the national support base.
#
# Assertion 6 is what keeps 3 and 5 from being circular: both would hold for
# any reduction the code happened to produce.

source("R/constants.R")
source("R/role4_demand.R")

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

#' The methods paper, which documents the experiment's design
METHODS_PATH <- file.path("docs", "Methods.md")

#' The results paper, which prints the experiment's figures
PAPER_PATH <- file.path("docs", "Results.md")

#' Directory of the tracked evidence set
EVIDENCE_DIR <- file.path("data", "role4_demand")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Probabilities the cancellation arm sweeps, those of the strategic airlift sweep
CANCELLATION_PROBABILITIES <- c(0, 0.05, 0.10, 0.15, 0.25, 0.40)

#' Read one tracked CSV of the evidence set, or record that it is absent
#'
#' @param name File name within the evidence directory.
#' @param dir Directory to read from.
#' @return The data frame, or NULL where the file is absent.
read_evidence <- function(name, dir = EVIDENCE_DIR) {
  path <- file.path(dir, name)
  if (!file.exists(path)) {
    report(FALSE, "the tracked file %s exists", path)
    return(NULL)
  }
  read.csv(path, stringsAsFactors = FALSE)
}

# ── 1. The code's parameters are the ones the methods paper documents ───────────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one parameter the methods paper states in a marker comment
#'
#' @param name Marker name, as it appears after "ROLE4DEMAND ".
#' @return The marker's value as a character string, or NA where absent.
role4_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- ROLE4DEMAND %s=[^ ]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- ROLE4DEMAND [^=]+=(.*) -->$", "\\1", m)
}

for (param in list(list("days", ROLE4_DEMAND_DAYS),
                   list("replications", ROLE4_DEMAND_REPLICATIONS),
                   list("window_days", ROLE4_DEMAND_WINDOW_DAYS),
                   list("seed", ROLE4_DEMAND_SEED))) {
  stated <- suppressWarnings(as.numeric(role4_marker(param[[1]])))
  report(!is.na(stated) && stated == param[[2]],
         "the methods paper states %s = %s and the code holds %s",
         param[[1]], format(stated), format(param[[2]]))
}

stated_scenarios <- role4_marker("scenarios")
report(!is.na(stated_scenarios) &&
         identical(strsplit(stated_scenarios, ",")[[1]], ROLE4_DEMAND_SCENARIOS),
       "the methods paper states the scenarios the code holds (%s)",
       paste(ROLE4_DEMAND_SCENARIOS, collapse = ","))

stated_text <- role4_marker("failure_probabilities")
stated_probs <- suppressWarnings(as.numeric(strsplit(stated_text, ",")[[1]]))
report(length(stated_probs) == length(CANCELLATION_PROBABILITIES) && !any(is.na(stated_probs)) &&
         all(abs(stated_probs - CANCELLATION_PROBABILITIES) < TOL),
       "the methods paper states the cancellation probabilities the sweep covers")

# ── 2. The tracked evidence is the experiment the methods paper documents ───────

cat("\n-- the tracked evidence set is that experiment --\n")

series <- read_evidence("role4_demand_series.csv.gz")
responses <- read_evidence("role4_demand_replications.csv")
summary_rows <- read_evidence("role4_demand_summary.csv")
daily <- read_evidence("role4_demand_daily.csv")
stability <- read_evidence("role4_demand_stability.csv")
reliability <- read_evidence("role4_demand_reliability_replications.csv")
reliability_summary <- read_evidence("role4_demand_reliability_summary.csv")

if (!is.null(series)) {
  per_scenario <- tapply(series$replication, series$scenario, function(x) length(unique(x)))
  report(setequal(names(per_scenario), ROLE4_DEMAND_SCENARIOS) &&
           all(per_scenario == ROLE4_DEMAND_REPLICATIONS),
         "both intensities carry %d replications in the series", ROLE4_DEMAND_REPLICATIONS)
  report(setequal(unique(series$day), seq_len(ROLE4_DEMAND_DAYS)),
         "the series spans days 1 to %d", ROLE4_DEMAND_DAYS)
  report(setequal(unique(series$series),
                  c("census", "operations", "theatre_minutes", "operations_after_horizon",
                    "operations_admitted")),
         "the series carries the census and the demand owed within, after and in all")
  census_subjects <- unique(series$subject[series$series == "census"])
  report(setequal(census_subjects, c(ROLE4_TOTAL, "icu", "hold", unname(ROLE4_ORIGINS))),
         "the census is divided by the total, both ward phases and the three origins (%s)",
         paste(sort(census_subjects), collapse = ", "))
}

if (!is.null(reliability)) {
  per_prob <- tapply(reliability$replication, reliability$failure_probability,
                     function(x) length(unique(x)))
  report(length(per_prob) == length(CANCELLATION_PROBABILITIES) &&
           all(abs(sort(as.numeric(names(per_prob))) - CANCELLATION_PROBABILITIES) < TOL) &&
           all(per_prob == ROLE4_DEMAND_REPLICATIONS),
         "the cancellation arm carries every documented probability at %d replications",
         ROLE4_DEMAND_REPLICATIONS)
}

# ── 3. The reductions are consistent with the series they reduce ────────────────

cat("\n-- the tracked responses, summary and daily mean are the reduction of the series --\n")

#' Whether two data frames agree on every numeric column to the tolerance
#'
#' @param a,b Data frames of the same shape.
#' @return TRUE when both frames have the same dimensions and agree.
frames_agree <- function(a, b) {
  if (!identical(dim(a), dim(b))) return(FALSE)
  for (col in names(a)) {
    if (is.numeric(a[[col]])) {
      if (!all(abs(a[[col]] - b[[col]]) < TOL * pmax(1, abs(a[[col]])))) return(FALSE)
    } else if (!identical(as.character(a[[col]]), as.character(b[[col]]))) {
      return(FALSE)
    }
  }
  TRUE
}

if (!is.null(series) && !is.null(responses)) {
  again <- role4_demand_responses(series)
  rownames(again) <- NULL
  tracked <- responses
  tracked <- tracked[order(tracked$scenario, tracked$series, tracked$subject, tracked$replication),
                     names(again)]
  rownames(tracked) <- NULL
  report(frames_agree(again, tracked),
         "the tracked responses are role4_demand_responses() of the tracked series")
}

if (!is.null(responses) && !is.null(summary_rows)) {
  again <- summarise_role4_demand(responses)
  #' Row key of a summary: scenario, series, subject and response
  #'
  #' @param d A summary frame.
  #' @return Character vector, one key per row.
  key <- function(d) paste(d$scenario, d$series, d$subject, d$response)
  again <- again[match(key(summary_rows), key(again)), names(summary_rows)]
  rownames(again) <- NULL
  report(frames_agree(again, summary_rows),
         "the tracked summary is summarise_role4_demand() of the tracked responses")
}

if (!is.null(series) && !is.null(daily)) {
  again <- role4_census_daily(series)
  rownames(again) <- NULL
  tracked <- daily[order(daily$scenario, daily$subject, daily$day), names(again)]
  rownames(tracked) <- NULL
  report(frames_agree(again, tracked),
         "the tracked daily mean is role4_census_daily() of the tracked series")
}

if (!is.null(reliability) && !is.null(reliability_summary)) {
  again <- do.call(rbind, lapply(sort(unique(reliability$failure_probability)), function(p) {
    cbind(failure_probability = p,
          summarise_role4_demand(reliability[reliability$failure_probability == p, ]),
          row.names = NULL)
  }))
  #' Row key of a cancellation summary: probability, scenario, series, subject and response
  #'
  #' @param d A summary frame.
  #' @return Character vector, one key per row.
  key <- function(d) paste(d$failure_probability, d$scenario, d$series, d$subject, d$response)
  again <- again[match(key(reliability_summary), key(again)), names(reliability_summary)]
  rownames(again) <- NULL
  report(frames_agree(again, reliability_summary),
         "the cancellation summary is summarise_role4_demand() of its responses")
}

# A composition is only a composition if its parts sum to what it divides.
if (!is.null(series)) {
  census <- series[series$series == "census", ]
  total <- tapply(census$value[census$subject == ROLE4_TOTAL],
                  census[census$subject == ROLE4_TOTAL, c("scenario", "replication", "day")],
                  sum)
  #' Largest absolute gap between the total and the sum of a set of parts
  #'
  #' @param parts Subjects that should sum to the total.
  #' @return The maximum absolute difference over every scenario, replication and day.
  gap <- function(parts) {
    p <- census[census$subject %in% parts, ]
    sums <- tapply(p$value, p[, c("scenario", "replication", "day")], sum)
    max(abs(sums - total), na.rm = TRUE)
  }
  report(gap(c("icu", "hold")) == 0,
         "the two ward phases sum to the total census on every day of every replication")
  report(gap(unname(ROLE4_ORIGINS)) == 0,
         "the three origins sum to the total census on every day of every replication")
}

# ── 4. The census agrees with the evidence sets that already report a peak ──────

cat("\n-- the census agrees with the Role 4 peak the other evidence sets report --\n")

#' Per-replication peak of the total census, in replication order
#'
#' @param d Responses frame, filtered to one scenario or probability.
#' @return Numeric vector of peaks ordered by replication.
total_peak <- function(d) {
  d <- d[d$series == "census" & d$subject == ROLE4_TOTAL, ]
  d$peak[order(d$replication)]
}

airlift <- if (file.exists("data/airlift/airlift_replications.csv")) {
  read.csv("data/airlift/airlift_replications.csv", stringsAsFactors = FALSE)
}
policy <- if (file.exists("data/policy/policy_sweep_replications.csv")) {
  read.csv("data/policy/policy_sweep_replications.csv", stringsAsFactors = FALSE)
}

if (!is.null(responses) && !is.null(airlift)) {
  for (arm in list(c("baseline", "moderate_intensity"), c("high", "high_intensity"))) {
    rows <- airlift[airlift$arm == arm[1], ]
    theirs <- rows$role4_peak[order(rows$replication)]
    ours <- total_peak(responses[responses$scenario == arm[2], ])
    report(length(theirs) == length(ours) && all(abs(theirs - ours) < TOL),
           "the %s census peak equals the strategic airlift %s arm's, replication by replication",
           arm[2], arm[1])
  }
}

if (!is.null(reliability) && !is.null(airlift)) {
  ok <- TRUE
  for (p in CANCELLATION_PROBABILITIES) {
    rows <- airlift[airlift$arm == "reliability" & abs(airlift$value - p) < TOL, ]
    theirs <- rows$role4_peak[order(rows$replication)]
    ours <- total_peak(reliability[abs(reliability$failure_probability - p) < TOL, ])
    ok <- ok && length(theirs) == length(ours) && all(abs(theirs - ours) < TOL)
  }
  report(ok, "the census peak at every probability equals the airlift reliability arm's")
}

if (!is.null(responses) && !is.null(policy)) {
  rows <- policy[abs(policy$policy_days - 21) < TOL, ]
  rows <- rows[order(rows$replication), ]
  mine <- responses[responses$scenario == "moderate_intensity", ]
  ops <- mine[mine$series == "operations_admitted" & mine$subject == ROLE4_TOTAL, ]
  ours_ops <- ops$total[order(ops$replication)]
  report(length(rows$role4_peak) == ROLE4_DEMAND_REPLICATIONS &&
           all(abs(rows$role4_peak - total_peak(mine)) < TOL),
         "the moderate census peak equals the shipped policy's, replication by replication")
  report(length(rows$role4_operations) == length(ours_ops) &&
           all(abs(rows$role4_operations - ours_ops) < TOL),
         "the operations owed by admitted casualties equal the shipped policy's, per replication")
}

# The cancellation arm at probability zero is the moderate arm's configuration run again
# by a later invocation, so it has to reproduce it exactly: the one test that the
# reduction is a function of the seed and the configuration alone.
if (!is.null(responses) && !is.null(reliability)) {
  moderate <- responses[responses$scenario == "moderate_intensity", ]
  zero <- reliability[abs(reliability$failure_probability) < TOL, names(moderate)]
  #' Row key of a responses frame: series, subject and replication
  #'
  #' @param d A responses frame.
  #' @return Character vector, one key per row.
  key <- function(d) paste(d$series, d$subject, d$replication)
  zero <- zero[match(key(moderate), key(zero)), ]
  report(nrow(zero) == nrow(moderate) && all(abs(zero$mean - moderate$mean) < TOL) &&
           all(abs(zero$peak - moderate$peak) < TOL) && all(abs(zero$total - moderate$total) < TOL),
         "the cancellation arm at probability zero reproduces the moderate arm exactly")
}

# ── 5. The published figures match the tracked summary ──────────────────────────

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
    report(FALSE, "the paper carries exactly one '%s' marker (found %d)", marker, length(at))
    return(NULL)
  }
  rows <- paper[at:length(paper)]
  rows <- rows[seq_len(which(!grepl("^\\|", rows) & seq_along(rows) > 2)[1] - 1)]
  rows[grepl("^\\|", rows)]
}

#' Split one markdown table row into its cells, the label column included
#'
#' @param row The row's text.
#' @return Character vector of the row's cells.
table_cells <- function(row) {
  trimws(strsplit(sub("^\\|", "", sub("\\|$", "", row)), "\\|")[[1]])
}

#' Leading number of a printed cell
#'
#' @param cell A cell such as `1,234.50 [1,200.00, 1,260.00]`.
#' @return The first number, with thousands separators and minus signs read.
leading_number <- function(cell) {
  as.numeric(gsub(",", "", sub("^−", "-", sub(" .*$", "", cell))))
}

#' Check printed census rows against the tracked summary
#'
#' @param marker The HTML comment marking the table.
#' @param subjects Named character vector: row label prefix to summary subject.
#' @return Invisible NULL.
check_census_table <- function(marker, subjects) {
  rows <- paper_table(marker)
  if (is.null(rows) || is.null(summary_rows)) return(invisible(NULL))
  measures <- c("mean beds" = "mean", "peak beds" = "peak",
                "closing 90-day mean beds" = "closing_mean")
  for (label in names(subjects)) {
    for (m in names(measures)) {
      row <- rows[startsWith(sub("^\\| ", "", rows), paste0(label, ", ", m, " |"))]
      if (length(row) != 1) {
        report(FALSE, "'%s' prints one '%s, %s' row (found %d)", marker, label, m, length(row))
        next
      }
      cells <- table_cells(row)[-1]
      for (k in seq_along(ROLE4_DEMAND_SCENARIOS)) {
        x <- summary_rows[summary_rows$scenario == ROLE4_DEMAND_SCENARIOS[k] &
                            summary_rows$series == "census" &
                            summary_rows$subject == subjects[[label]] &
                            summary_rows$response == measures[[m]], ]
        report(nrow(x) == 1 && abs(leading_number(cells[k]) - x$mean) <= 0.005 + TOL,
               "'%s, %s' for %s prints %s against the data's %s", label, m,
               ROLE4_DEMAND_SCENARIOS[k], cells[k],
               if (nrow(x) == 1) format(round(x$mean, 2)) else "no row")
      }
    }
  }
  invisible(NULL)
}

check_census_table("<!-- GEN role4_census_ward -->",
                   c("Total" = ROLE4_TOTAL, "Intensive care phase" = "icu",
                     "Step-down ward phase" = "hold"))
check_census_table("<!-- GEN role4_census_origin -->",
                   c("Total" = ROLE4_TOTAL, "Battle injury" = ROLE4_ORIGINS[["battle_injury"]],
                     "Disease and non-battle injury" = ROLE4_ORIGINS[["dnbi"]],
                     "Reconstruction cohort" = ROLE4_ORIGINS[["reconstruction"]]))

ops_rows <- paper_table("<!-- GEN role4_operations -->")
if (!is.null(ops_rows) && !is.null(summary_rows)) {
  spec <- list(c("Operations owed within the 360 days", "operations", "total"),
               c("Operations owed after day 360", "operations_after_horizon", "total"),
               c("Operations owed by casualties admitted during the campaign",
                 "operations_admitted", "total"),
               c("Operations owed on the busiest day", "operations", "peak"),
               c("Closing 90-day mean operations owed per day", "operations", "closing_mean"),
               c("Theatre minutes owed for definitive repairs", "theatre_minutes", "total"))
  for (s in spec) {
    # Totals print as whole numbers and the rest to two decimals, so each is
    # compared to half a unit of its own last printed place.
    tol <- if (s[3] == "total") 0.5 else 0.005
    row <- ops_rows[startsWith(sub("^\\| ", "", ops_rows), paste0(s[1], " |"))]
    if (length(row) != 1) {
      report(FALSE, "the operations table prints one '%s' row (found %d)", s[1], length(row))
      next
    }
    cells <- table_cells(row)[-1]
    for (k in seq_along(ROLE4_DEMAND_SCENARIOS)) {
      x <- summary_rows[summary_rows$scenario == ROLE4_DEMAND_SCENARIOS[k] &
                          summary_rows$series == s[2] & summary_rows$subject == ROLE4_TOTAL &
                          summary_rows$response == s[3], ]
      report(nrow(x) == 1 && abs(leading_number(cells[k]) - x$mean) < tol,
             "'%s' for %s prints %s against the data's %s", s[1], ROLE4_DEMAND_SCENARIOS[k],
             cells[k], if (nrow(x) == 1) format(round(x$mean, 2)) else "no row")
    }
  }
}

canc_rows <- paper_table("<!-- GEN role4_levers_cancellation -->")
if (!is.null(canc_rows) && !is.null(reliability_summary)) {
  for (s in list(c("Role 4 peak beds", "census", "peak"),
                 c("Role 4 closing 90-day mean beds", "census", "closing_mean"))) {
    row <- canc_rows[startsWith(sub("^\\| ", "", canc_rows), paste0(s[1], " |"))]
    if (length(row) != 1) {
      report(FALSE, "the cancellation table prints one '%s' row (found %d)", s[1], length(row))
      next
    }
    cells <- table_cells(row)[-1]
    ok <- length(cells) == length(CANCELLATION_PROBABILITIES)
    for (k in seq_along(cells)) {
      x <- reliability_summary[abs(reliability_summary$failure_probability -
                                     CANCELLATION_PROBABILITIES[k]) < TOL &
                                 reliability_summary$series == s[2] &
                                 reliability_summary$subject == ROLE4_TOTAL &
                                 reliability_summary$response == s[3], ]
      ok <- ok && nrow(x) == 1 && abs(leading_number(cells[k]) - round(x$mean, 1)) < 0.05
    }
    report(ok, "'%s' prints the cancellation arm's mean at each probability", s[1])
  }
}

# ── 6. The reductions are correct on hand-computable inputs ─────────────────────

cat("\n-- the reductions are correct on inputs with known answers --\n")

# Origin: the reconstruction cohort takes precedence over injury type, and
# disease and non-battle injury is separated from battle injury by injury_type.
origin_in <- data.frame(injury_type = c(1, 2, 1, 2, NA),
                        reconstruction_required = c(0, 0, 1, 1, NA))
report(identical(role4_origin(origin_in),
                 c("battle_injury", "dnbi", "reconstruction", "reconstruction", "battle_injury")),
       "a reconstruction casualty is classed by the reconstruction, whatever their injury type")
report(identical(role4_origin(data.frame(injury_type = c(1, 2))), c("battle_injury", "dnbi")),
       "a configuration drawing no reconstruction classes everyone by injury type")

# Census: three phases over eight days, [1,2] intensive care, [3,6] step-down,
# and [5,12] intensive care, which runs past the horizon and is counted to it.
phases <- data.frame(phase_ward = c("icu", "hold", "icu"),
                     origin = unname(ROLE4_ORIGINS[c("battle_injury", "battle_injury",
                                                     "reconstruction")]),
                     phase_start = c(1, 3, 5), phase_end = c(2, 6, 12),
                     stringsAsFactors = FALSE)
cen <- role4_census_from_phases(phases, c("icu", "hold"), 8L)
#' Census of one subject over the campaign, in day order
#'
#' @param subject The census subject to read.
#' @return Integer-valued vector of daily occupancy.
at <- function(subject) {
  rows <- cen[cen$subject == subject, ]
  rows$occupancy[order(rows$day)]
}
report(identical(as.integer(at(ROLE4_TOTAL)), c(1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L)),
       "the total census is the number of phases in a bed on each day, clipped to the horizon")
report(identical(as.integer(at("icu")), c(1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L)) &&
         identical(as.integer(at("hold")), c(0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L)),
       "each ward carries exactly its own phases")
report(identical(as.integer(at(ROLE4_ORIGINS[["reconstruction"]])),
                 c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L)),
       "an origin carries exactly its own casualties")
report(nrow(role4_census_from_phases(phases[0, ], c("icu", "hold"), 8L)) == 8L * 6L &&
         all(role4_census_from_phases(phases[0, ], c("icu", "hold"), 8L)$occupancy == 0L),
       "a campaign in which nobody reaches Role 4 reports a zero census on every day")

# Responses: two replications of a four-day census, closing window of two days.
hand <- data.frame(
  replication = rep(1:2, each = 4), day = rep(1:4, 2), series = "census",
  subject = ROLE4_TOTAL, value = c(2, 4, 6, 8, 1, 1, 3, 3), stringsAsFactors = FALSE
)
r <- role4_demand_responses(hand, n_days = 4L, window_days = 2L)
report(all(r$mean == c(5, 2)) && all(r$peak == c(8, 3)) && all(r$closing_mean == c(7, 3)) &&
         all(r$total == c(20, 8)),
       "the mean, peak, closing-window mean and total of a census are those computed by hand")
s <- summarise_role4_demand(r)
m <- s[s$response == "mean", ]
half <- qt(0.975, df = 1) * sd(c(5, 2)) / sqrt(2)
report(abs(m$mean - 3.5) < TOL && abs(m$ci_lower - (3.5 - half)) < TOL &&
         abs(m$ci_upper - (3.5 + half)) < TOL && m$n_reps == 2L,
       "the interval about the mean of two replications is the Student t one")
d <- role4_census_daily(transform(hand, scenario = "default"))
report(all(d$mean == c(1.5, 2.5, 4.5, 5.5)) && all(d$n_reps == 2L),
       "the daily mean is the mean across replications of each day")

# ── 7. Demand only ──────────────────────────────────────────────────────────

cat("\n-- the evidence and the module report demand only --\n")

forbidden <- "capacity|shortfall|queue"
tracked_columns <- unlist(lapply(list(series, responses, summary_rows, daily, stability,
                                      reliability, reliability_summary), names))
report(!any(grepl(forbidden, tracked_columns, ignore.case = TRUE)),
       "no tracked column of the evidence set names a capacity, queue or shortfall")
module_text <- paste(readLines("R/role4_demand.R", warn = FALSE), collapse = "\n")
report(!grepl("beds? *(<-|=) *[0-9]|shortfall *<-|capacity *<-", module_text),
       "the module assigns the national support base no bed count, capacity or shortfall")

# ── Result ──────────────────────────────────────────────────────────────────

if (length(state$failures) > 0) {
  cat(sprintf("\n%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1)
}
cat("\nAll Role 4 demand protocol checks passed.\n")
quit(status = 0)
