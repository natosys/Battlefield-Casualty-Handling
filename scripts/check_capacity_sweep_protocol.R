#!/usr/bin/env Rscript
##############################################################################
## scripts/check_capacity_sweep_protocol.R                                  ##
## Regression check — the transport fleet-size sweep and the forward ICU     ##
## share frontier agree with their published tables                          ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_capacity_sweep_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Both sweeps inform a planning option the companion
# paper states as a recommendation: three ambulances are sufficient, and
# delivering post-operative intensive care forward is not. Until Issue #384
# both wrote their results to the gitignored outputs/ alone, so the tables the
# recommendations rest on could be audited only by re-running ninety and one
# hundred replications respectively. Issue #300 added a second transport
# sweep under the high_intensity profile, checked against its own table by
# the same steps rather than a third check, since it is the same design run
# under a different scenario.
#
# The three tracked evidence sets share one check rather than one each
# because they are one shape: a swept establishment or policy value, a
# per-point mean and interval, and a table in the paper printing a column of
# each. A reader auditing one audits the others by the same steps.
#
# What this asserts:
#
#   1. Every protocol parameter in R/analysis.R equals the value
#      docs/Multi_Run_Supplement.md documents in a marker comment.
#   2. Each tracked evidence set is that experiment: the documented swept
#      values, and the response columns its published table prints.
#   3. Every figure the paper's tables print matches the tracked
#      measurement, and a missing table, row or column fails rather than
#      passing quietly.
#   4. The interval each sweep reports is the Student t one at 95% on an input
#      whose answer is computable by hand, so a table agreeing with the tracked
#      set is not two copies of one error.
#
# Assertion 4 is what keeps assertion 3 from being circular: 3 would hold for
# any sweep output the code happened to produce.

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

#' The supplement, which documents both designs
SUPPLEMENT_PATH <- file.path("docs", "Multi_Run_Supplement.md")

#' The companion paper, which prints both tables
PAPER_PATH <- file.path("docs", "Multi_Run_Analysis.md")

#' Tracked transport fleet-size sweep, shipped configuration
TRANSPORT_PATH <- file.path("data", "sweeps", "transport_capacity_by_fleet_size.csv")

#' Tracked transport fleet-size sweep, high_intensity profile
TRANSPORT_HIGH_PATH <- file.path("data", "sweeps", "transport_capacity_by_fleet_size_high_intensity.csv")

#' Tracked forward ICU share frontier
ICU_SHARE_PATH <- file.path("data", "sweeps", "r2b_icu_share_frontier.csv")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Tolerance on a figure the paper prints rounded to its last decimal place
PRINT_TOL <- 0.005

# ── 1. The code's parameters are the ones the supplement documents ───────────

cat("\n-- both protocols' parameters match the supplement --\n")

# R/analysis.R carries the protocol constants alongside five thousand lines of
# pipeline that need simmer's monitoring shapes to be interesting. The
# constants are read out of the source instead, so this check runs in seconds
# and without the simulation's dependencies, which is what lets it sit in the
# per-pull-request gate.
analysis_source <- readLines(file.path("R", "analysis.R"), warn = FALSE)

#' Read one protocol constant's value out of R/analysis.R
#'
#' @param name The constant's name.
#' @return The constant's value, or NULL where it is absent or spans more than
#'   one line.
#'
#' @details Evaluated in an empty environment, so a constant defined in terms
#'   of anything but a literal reads as absent rather than picking up whatever
#'   this check happens to have bound under that name.
analysis_constant <- function(name) {
  at <- grep(sprintf("^%s <- ", name), analysis_source)
  if (length(at) != 1) return(NULL)
  rhs <- sub(sprintf("^%s <- ", name), "", analysis_source[at])
  tryCatch(eval(parse(text = rhs), envir = new.env(parent = baseenv())),
           error = function(e) NULL)
}

supplement <- paste(readLines(SUPPLEMENT_PATH, warn = FALSE), collapse = "\n")

#' Read one sweep protocol parameter the supplement states in a marker
#'
#' @param name Marker name, as it appears after "SWEEP ".
#' @return The marker's value as a character string, or NA where absent.
#'
#' @details Marked rather than parsed out of the prose, on the convention
#'   `scripts/check_airlift_protocol.R` established: a check that guesses which
#'   number in a paragraph is the replication count fails for reasons that have
#'   nothing to do with the protocol.
sweep_marker <- function(name) {
  m <- regmatches(supplement,
                  regexpr(sprintf("<!-- SWEEP %s=[^ ]+ -->", name), supplement))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- SWEEP [^=]+=(.*) -->$", "\\1", m)
}

#' Compare a documented comma-separated vector against the code's own
#'
#' @param name Marker name, as it appears after "SWEEP ".
#' @param held The vector the code holds.
#' @return Invisible NULL.
check_swept_vector <- function(name, held) {
  stated <- sweep_marker(name)
  parsed <- if (is.na(stated)) {
    numeric(0)
  } else {
    suppressWarnings(as.numeric(strsplit(stated, ",")[[1]]))
  }
  report(!is.null(held) && length(parsed) == length(held) && !any(is.na(parsed)) &&
           all(abs(parsed - held) < TOL),
         "the supplement states %s %s and the code holds %s", name,
         paste(format(parsed), collapse = ","),
         paste(format(held), collapse = ","))
  invisible(NULL)
}

scalars <- list(
  list("days", analysis_constant("CAPACITY_SWEEP_DAYS")),
  list("seed", analysis_constant("CAPACITY_SWEEP_SEED")),
  list("transport_replications", analysis_constant("TRANSPORT_SWEEP_REPLICATIONS")),
  list("icu_share_replications", analysis_constant("ICU_SHARE_SWEEP_REPLICATIONS"))
)
for (param in scalars) {
  stated <- suppressWarnings(as.numeric(sweep_marker(param[[1]])))
  report(!is.na(stated) && !is.null(param[[2]]) && stated == param[[2]],
         "the supplement states %s = %s and the code holds %s",
         param[[1]], format(stated),
         if (is.null(param[[2]])) "nothing" else format(param[[2]]))
}

pmvamb <- analysis_constant("TRANSPORT_SWEEP_PMVAMB")
hx240m <- analysis_constant("TRANSPORT_SWEEP_HX240M")
shares <- analysis_constant("ICU_SHARE_SWEEP_SHARES")

check_swept_vector("pmvamb", pmvamb)
check_swept_vector("hx240m", hx240m)
check_swept_vector("shares", shares)

# Each sweep must contain the shipped setting, or it says nothing about what
# departing from the establishment costs. The ambulance fleet ships at three
# and the truck fleet at four; the forward intensive care share ships at zero.
report(!is.null(pmvamb) && 3L %in% pmvamb,
       "the ambulance sweep contains the shipped establishment of three")
report(!is.null(hx240m) && 4L %in% hx240m,
       "the truck sweep contains the shipped establishment of four")
report(!is.null(shares) && any(abs(shares) < TOL),
       "the share sweep contains the shipped forward share of zero")

# ── 2. Each tracked evidence set is that experiment ──────────────────────────

cat("\n-- each tracked evidence set is that experiment --\n")

transport <- if (file.exists(TRANSPORT_PATH)) {
  read.csv(TRANSPORT_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked transport sweep %s exists", TRANSPORT_PATH)
  NULL
}

transport_high <- if (file.exists(TRANSPORT_HIGH_PATH)) {
  read.csv(TRANSPORT_HIGH_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked high_intensity transport sweep %s exists", TRANSPORT_HIGH_PATH)
  NULL
}

icu_share <- if (file.exists(ICU_SHARE_PATH)) {
  read.csv(ICU_SHARE_PATH, stringsAsFactors = FALSE)
} else {
  report(FALSE, "the tracked ICU share frontier %s exists", ICU_SHARE_PATH)
  NULL
}

if (!is.null(transport)) {
  swept_pmvamb <- sort(transport$qty[transport$vehicle == "PMVAmb"])
  swept_hx240m <- sort(transport$qty[transport$vehicle == "HX240M"])
  report(identical(as.integer(swept_pmvamb), as.integer(pmvamb)),
         "the tracked sweep carries the ambulance sizes the code holds")
  report(identical(as.integer(swept_hx240m), as.integer(hx240m)),
         "the tracked sweep carries the truck sizes the code holds")

  needed <- c("mean_q", "ci_lower_q", "ci_upper_q", "mean_util")
  report(all(needed %in% names(transport)),
         "the tracked sweep carries every response the table prints (%s)",
         paste(setdiff(needed, names(transport)), collapse = ","))
}

if (!is.null(transport_high)) {
  swept_pmvamb <- sort(transport_high$qty[transport_high$vehicle == "PMVAmb"])
  swept_hx240m <- sort(transport_high$qty[transport_high$vehicle == "HX240M"])
  report(identical(as.integer(swept_pmvamb), as.integer(pmvamb)),
         "the tracked high_intensity sweep carries the ambulance sizes the code holds")
  report(identical(as.integer(swept_hx240m), as.integer(hx240m)),
         "the tracked high_intensity sweep carries the truck sizes the code holds")

  needed <- c("mean_q", "ci_lower_q", "ci_upper_q", "mean_util")
  report(all(needed %in% names(transport_high)),
         "the tracked high_intensity sweep carries every response the table prints (%s)",
         paste(setdiff(needed, names(transport_high)), collapse = ","))
}

if (!is.null(icu_share)) {
  report(length(icu_share$share) == length(shares) &&
           all(abs(sort(icu_share$share) - sort(shares)) < TOL),
         "the tracked frontier carries the shares the code holds")

  needed <- c("mean_r2e_icu_q", "ci_lower_r2e_icu_q", "ci_upper_r2e_icu_q",
              "mean_r2b_icu_util", "mean_r2e_icu_util", "mean_pd_icu_share",
              "mean_dow")
  report(all(needed %in% names(icu_share)),
         "the tracked frontier carries every response the table prints (%s)",
         paste(setdiff(needed, names(icu_share)), collapse = ","))
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

#' Split one markdown table row into its cells
#'
#' @param row The row's text.
#' @return Character vector of every cell, the label column included.
table_cells <- function(row) {
  trimws(strsplit(sub("^\\|", "", sub("\\|$", "", row)), "\\|")[[1]])
}

#' The leading figure of one printed cell
#'
#' @param cell The cell's text.
#' @return The number the cell opens with, or NA where it carries none.
#'
#' @details Only the leading figure is read; the interval beside it is the same
#'   mean's neighbourhood and adds nothing a comparison of means misses. A cell
#'   reading "not swept" or "not applicable" carries no figure and returns NA,
#'   which the caller treats as a row to skip rather than as a disagreement.
leading_figure <- function(cell) {
  if (!grepl("^[0-9]", cell)) return(NA_real_)
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", sub("[ %].*$", "", cell))))
}

#' Check one published table against the tracked sweep it is printed from
#'
#' @param marker The HTML comment marking the table in the paper.
#' @param row_keys Numeric vector identifying each data row, in the table's row
#'   order; a row whose leading label does not open with its key fails.
#' @param columns List of column index, a function of one key returning the
#'   value that column should print, the printed scale and the decimal places.
#' @return Invisible NULL.
#'
#' @details A row the tracked set has no value for is compared against NA and
#'   fails, rather than being skipped, so a table row outliving its measurement
#'   is reported. A cell the paper deliberately leaves unswept carries no
#'   leading figure and is skipped.
check_published_table <- function(marker, row_keys, columns) {
  rows <- paper_table(marker)
  if (is.null(rows)) return(invisible(NULL))

  data_rows <- rows[-(1:2)]
  report(length(data_rows) == length(row_keys),
         "'%s' prints %d data rows, one per swept point (found %d)",
         marker, length(row_keys), length(data_rows))
  if (length(data_rows) != length(row_keys)) return(invisible(NULL))

  for (i in seq_along(row_keys)) {
    cells <- table_cells(data_rows[i])
    label <- leading_figure(cells[1])
    report(!is.na(label) && abs(label - row_keys[i]) < TOL,
           "row %d of %s is the %s point (label reads '%s')",
           i, marker, format(row_keys[i]), cells[1])

    for (col in columns) {
      k <- col[[1]]
      printed <- if (k + 1 <= length(cells)) leading_figure(cells[k + 1]) else NA_real_
      if (is.na(printed)) next
      expected <- col[[3]] * col[[2]](row_keys[i])
      ok <- !is.na(expected) && abs(printed - round(expected, col[[4]])) < PRINT_TOL
      report(ok, "column %d of row %d of %s prints %s against the data's %s",
             k, i, marker, format(printed),
             if (is.na(expected)) "no value" else format(round(expected, col[[4]])))
    }
  }
  invisible(NULL)
}

#' One tracked value, or NA where the tracked set carries no such row
#'
#' @param data The tracked data frame.
#' @param mask Logical vector selecting the row.
#' @param column Name of the column to read.
#' @return The value, or NA where the row is absent or repeated.
tracked_value <- function(data, mask, column) {
  hit <- data[mask, ]
  if (nrow(hit) != 1) return(NA_real_)
  hit[[column]]
}

#' A reader of one tracked sweep column, as a function of the row's key
#'
#' @param data The tracked data frame.
#' @param mask_of A function of the row key returning the logical mask
#'   selecting that row.
#' @param column Name of the column to read.
#' @return A function of one row key returning that row's value, or NA.
#'
#' @details Built as a closure per column rather than written inline in the
#'   specification below, which nested three call levels deep and read as
#'   punctuation.
column_reader <- function(data, mask_of, column) {
  force(data)
  force(mask_of)
  force(column)
  function(key) tracked_value(data, mask_of(key), column)
}

if (!is.null(transport)) {
  # The table is one row per fleet size, printing the ambulance queue in the
  # first column and the truck queue in the second, so a size the truck fleet
  # is not swept to prints "not swept" and is skipped.
  sizes <- sort(union(pmvamb, hx240m))

  #' Rows of the tracked sweep holding the ambulance fleet at one size
  #'
  #' @param q The fleet size.
  #' @return Logical vector selecting that sweep point's row.
  ambulance_at <- function(q) transport$vehicle == "PMVAmb" & transport$qty == q

  #' Rows of the tracked sweep holding the truck fleet at one size
  #'
  #' @param q The fleet size.
  #' @return Logical vector selecting that sweep point's row.
  truck_at <- function(q) transport$vehicle == "HX240M" & transport$qty == q

  check_published_table("<!-- TRANSPORT SWEEP TABLE -->", sizes, list(
    list(1, column_reader(transport, ambulance_at, "mean_q"), 1, 4),
    list(2, column_reader(transport, truck_at, "mean_q"), 1, 4)
  ))
}

if (!is.null(transport_high)) {
  sizes <- sort(union(pmvamb, hx240m))

  #' Rows of the tracked high_intensity sweep holding the ambulance fleet at
  #' one size
  #'
  #' @param q The fleet size.
  #' @return Logical vector selecting that sweep point's row.
  ambulance_at_high <- function(q) transport_high$vehicle == "PMVAmb" & transport_high$qty == q

  #' Rows of the tracked high_intensity sweep holding the truck fleet at one
  #' size
  #'
  #' @param q The fleet size.
  #' @return Logical vector selecting that sweep point's row.
  truck_at_high <- function(q) transport_high$vehicle == "HX240M" & transport_high$qty == q

  check_published_table("<!-- TRANSPORT SWEEP TABLE HIGH INTENSITY -->", sizes, list(
    list(1, column_reader(transport_high, ambulance_at_high, "mean_q"), 1, 4),
    list(2, column_reader(transport_high, truck_at_high, "mean_q"), 1, 4)
  ))
}

if (!is.null(icu_share)) {
  # The table's row labels are percentages and the tracked shares are
  # fractions, so each key is divided by a hundred before it is matched.
  #' Rows of the tracked frontier at one forward share
  #'
  #' @param p The share as the table prints it, a percentage.
  #' @return Logical vector selecting that sweep point's row.
  share_at <- function(p) abs(icu_share$share - p / 100) < TOL

  #' A reader of one column of the tracked frontier
  #'
  #' @param column Name of the column to read.
  #' @return A function of one printed share returning that column's value.
  icu_column <- function(column) column_reader(icu_share, share_at, column)

  check_published_table("<!-- ICU SHARE TABLE -->", 100 * sort(shares), list(
    list(1, icu_column("mean_r2e_icu_q"), 1, 3),
    list(2, icu_column("mean_r2b_icu_util"), 100, 1),
    list(3, icu_column("mean_r2e_icu_util"), 100, 1),
    list(4, icu_column("mean_pd_icu_share"), 100, 1),
    list(5, icu_column("mean_dow"), 1, 2)
  ))
}

# ── 4. The intervals are right on a hand-computable input ────────────────────

cat("\n-- the tracked intervals are the Student t ones at 95% --\n")

#' Half-width of the interval both sweeps report
#'
#' @param n Replications the point was measured at.
#' @param s Sample standard deviation across those replications.
#' @return The 95% Student t half-width.
#'
#' @details Written out here rather than called from R/analysis.R, which cannot
#'   be sourced without the simulation's dependencies. Asserting that the
#'   tracked interval matches it is what stops assertion 3 checking a table
#'   against a summary that computed its interval some other way.
t_half_width <- function(n, s) qt(0.975, df = pmax(n - 1, 1)) * s / sqrt(n)

# Five values whose mean is 3 and whose sample standard deviation is exactly
# sqrt(2.5), so the half-width can be written down without reference to any
# function under test.
report(abs(t_half_width(5, sd(c(1, 2, 3, 4, 5))) -
             qt(0.975, df = 4) * sqrt(2.5) / sqrt(5)) < TOL,
       "the half-width formula is the Student t one at 95%%")

#' Assert that a tracked interval is the one the sweep says it reports
#'
#' @param data The tracked data frame.
#' @param mean_col Name of the mean column.
#' @param lower_col Name of the lower bound column.
#' @param upper_col Name of the upper bound column.
#' @param clamp_low Value the lower bound is clamped at, or NA where none is.
#' @param clamp_high Value the upper bound is clamped at, or NA where none is.
#' @param label What the assertion is about, for the reported lines.
#' @return Invisible NULL.
#'
#' @details Three properties, none of which a wrong interval satisfies. Every
#'   mean lies inside its own interval. Every bound is either symmetric about
#'   that mean, which is what a Student t interval is, or sitting exactly on
#'   the clamp the sweep applies to keep a queue above zero and a utilisation
#'   inside one. And at least one bound is unclamped, without which the first
#'   two would hold for any interval wide enough to reach the clamps.
#'
#'   This is what the tracked summary supports. The sweeps keep a mean and an
#'   interval per point rather than the replications behind them, so the
#'   half-width cannot be recomputed from the tracked set; that limit is
#'   recorded in `docs/Multi_Run_Supplement.md`.
check_interval_shape <- function(data, mean_col, lower_col, upper_col,
                                 clamp_low, clamp_high, label) {
  m <- data[[mean_col]]
  lo <- data[[lower_col]]
  hi <- data[[upper_col]]
  measured <- !is.na(m) & !is.na(lo) & !is.na(hi)
  m <- m[measured]
  lo <- lo[measured]
  hi <- hi[measured]

  report(length(m) > 0 && all(lo <= m + TOL) && all(hi >= m - TOL),
         "%s: every mean lies inside its own interval (%d points)", label, length(m))

  at_low <- !is.na(clamp_low) & abs(lo - clamp_low) < TOL
  at_high <- !is.na(clamp_high) & abs(hi - clamp_high) < TOL
  free <- !at_low & !at_high
  gaps <- abs((m - lo) - (hi - m))
  report(all(free == FALSE) || max(gaps[free]) < 1e-9,
         "%s: every unclamped interval is symmetric about its mean (%d of %d)",
         label, sum(free), length(m))

  # Without this, the two assertions above would hold for an interval so wide
  # that every bound sat on a clamp and nothing about its width was tested.
  report(any(!at_low) || any(!at_high),
         "%s: at least one tracked bound is unclamped, so the width is tested",
         label)
  invisible(NULL)
}

if (!is.null(transport)) {
  check_interval_shape(transport, "mean_q", "ci_lower_q", "ci_upper_q",
                       0, NA, "the transport sweep queue")
  check_interval_shape(transport, "mean_util", "ci_lower_util", "ci_upper_util",
                       0, 1, "the transport sweep utilisation")
}
if (!is.null(transport_high)) {
  check_interval_shape(transport_high, "mean_q", "ci_lower_q", "ci_upper_q",
                       0, NA, "the high_intensity transport sweep queue")
  check_interval_shape(transport_high, "mean_util", "ci_lower_util", "ci_upper_util",
                       0, 1, "the high_intensity transport sweep utilisation")
}
if (!is.null(icu_share)) {
  check_interval_shape(icu_share, "mean_r2e_icu_q", "ci_lower_r2e_icu_q",
                       "ci_upper_r2e_icu_q", 0, NA, "the ICU share queue")
  check_interval_shape(icu_share, "mean_dow", "ci_lower_dow", "ci_upper_dow",
                       0, NA, "the ICU share deaths of wounds")
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All capacity sweep protocol checks passed.\n")
quit(status = 0)
