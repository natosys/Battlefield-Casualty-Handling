#!/usr/bin/env Rscript
##############################################################################
## scripts/check_results_tables.R                                           ##
## Regression check — docs/Results.md is what the tracked evidence says     ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_results_tables.R
#
# Exits 0 when every assertion passes and 1 otherwise, so it can gate a pull
# request.
#
# Why this check exists. docs/Results.md reports measurements and nothing else,
# which is only worth saying if no figure in it can differ from the evidence it
# reports. The measurements drifted from their prose in the documents that
# preceded it because a table was guarded and the sentences around it were not,
# so a figure re-run in one place stayed stale in another. In Results.md every
# table and every quoted figure is a generated span, and this check asserts
# three things about that arrangement: that re-rendering the document from the
# tracked data reproduces it exactly, that the builders and the cell reader are
# correct on inputs whose answers are computable by hand (so a document agreeing
# with its builders is not two copies of one error), and that nothing in
# Results.md states a figure outside a span or an interpretation the document
# disclaims. The protocol checks read the tables this document prints, so each
# experiment's table is asserted against its tracked evidence there.

invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

source("R/results.R")

#' The document under test
RESULTS_PATH <- file.path("docs", "Results.md")

#' Percentages a sentence may state without being a measured figure
#'
#' @details Design labels such as the swept cancellation probabilities and the
#'   confidence level, which describe an experiment rather than report a result.
ALLOWED_PERCENTAGES <- c("0%", "5%", "10%", "15%", "20%", "25%", "40%", "50%", "75%", "95%",
                         "100%")

failures <- character(0)

#' Record a failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated failures, invisibly; called for its side effect.
fail <- function(...) {
  assign("failures", c(failures, sprintf(...)), envir = globalenv())
}

#' Print one PASS or FAIL line and record a failure
#'
#' @param ok Whether the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Values interpolated into `fmt`.
#' @return The printed line, invisibly; called for its side effect.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
}

# ── 1. The document reproduces from the tracked evidence ────────────────────

cat("-- docs/Results.md equals its own re-rendering --\n")

text <- paste(readLines(RESULTS_PATH, encoding = "UTF-8", warn = FALSE), collapse = "\n")
rendered <- render_results(text)
report(identical(text, rendered),
       "re-rendering %s from data/ reproduces it exactly", RESULTS_PATH)

spans <- regmatches(text, gregexpr(RESULTS_SPAN_PATTERN, text, perl = TRUE))[[1]]
names_used <- sub(RESULTS_SPAN_PATTERN, "\\1", spans, perl = TRUE)
report(length(spans) > 0L, "the document carries generated spans (found %d)", length(spans))

table_names <- names(RESULTS_TABLES)
tables_used <- names_used[!startsWith(names_used, "cell:")]
report(all(table_names %in% tables_used),
       "every registered table is printed in the document (missing: %s)",
       paste(setdiff(table_names, tables_used), collapse = ", "))
report(!any(grepl("\\?<!-- /GEN", text)), "no cell span is left holding its placeholder")

# ── 2. The builders are correct on inputs computable by hand ────────────────

cat("\n-- the builders and the cell reader are correct on hand-computed inputs --\n")

report(identical(res_num(1234.5678, 2L, big = TRUE), "1,234.57"),
       "res_num groups thousands and rounds: %s", res_num(1234.5678, 2L, big = TRUE))
report(identical(res_num(-0.25, 1L), "−0.2") || identical(res_num(-0.25, 1L), "−0.3"),
       "res_num prints a true minus sign: %s", res_num(-0.25, 1L))
report(identical(res_num(3, 1L, plus = TRUE), "+3.0"), "res_num prints an explicit plus: %s",
       res_num(3, 1L, plus = TRUE))
report(identical(res_ci(0.5, -0.1, 0.9, 2L, floor0 = TRUE), "0.50 [0.00, 0.90]"),
       "res_ci clamps a negative lower bound when asked: %s",
       res_ci(0.5, -0.1, 0.9, 2L, floor0 = TRUE))
report(identical(res_ci(0.123, 0.1, 0.2, 1L, scale = 100, unit = "%"), "12.3% [10.0%, 20.0%]"),
       "res_ci scales to a percentage with its unit on every figure: %s",
       res_ci(0.123, 0.1, 0.2, 1L, scale = 100, unit = "%"))

toy <- res_table(c("Metric", "A", "B"), list(c("x", "1.5 [1.0, 2.0]", "7"), c("y", "3", "4")))
report(identical(toy[2], "|---|---|---|"), "res_table writes the rule row: %s", toy[2])
report(identical(res_cells(toy[3]), c("x", "1.5 [1.0, 2.0]", "7")),
       "res_cells splits a row into its cells")

toy_tables <- list(toy = function(dd) toy)
report(identical(res_cell_value("toy|x|A|full", ".", toy_tables), "1.5 [1.0, 2.0]"),
       "a cell reference returns the whole cell")
report(identical(res_cell_value("toy|x|A|mean", ".", toy_tables), "1.5"),
       "a cell reference returns the leading number")
report(identical(res_cell_value("toy|x|A|ci", ".", toy_tables), "[1.0, 2.0]"),
       "a cell reference returns the interval")
report(inherits(try(res_cell_value("toy|x|C|full", ".", toy_tables), silent = TRUE), "try-error"),
       "a cell reference to a missing column is an error rather than an empty string")
report(inherits(try(res_cell_value("toy|z|A|full", ".", toy_tables), silent = TRUE), "try-error"),
       "a cell reference to a missing row is an error rather than an empty string")
doc <- "before <!-- GEN cell:toy|x|B|full -->old<!-- /GEN --> after"
once <- render_results(doc, ".", toy_tables)
report(identical(once, "before <!-- GEN cell:toy|x|B|full -->7<!-- /GEN --> after"),
       "render_results replaces a span's content and leaves the text around it")
report(identical(render_results(once, ".", toy_tables), once),
       "rendering twice gives the same text as rendering once")

# ── 3. The resolution and pathway tables are correct on inputs computable by hand ──

cat("\n-- the resolution and pathway tables are correct on hand-computed inputs --\n")

toy_dir <- file.path(tempdir(), "results_toy")
for (d in c("hold_window", "icu_gate", "policy")) {
  dir.create(file.path(toy_dir, d), recursive = TRUE, showWarnings = FALSE)
}
#' Write one paired-difference file whose every row carries the same known figures
#'
#' @param path Path under the toy data directory.
#' @param keys Data frame of response, from and to, one row per paired comparison.
#' @param extra Named list of further columns, such as the swept arm column.
#' @return The path written, invisibly; called for its side effect.
write_toy_paired <- function(path, keys, extra = NULL) {
  d <- cbind(keys, n_pairs = 30L, difference = 0.5, ci_lower = -1, ci_upper = 2,
             p_value = 0.4, reps_needed = 1234L)
  if (!is.null(extra)) d <- cbind(d, extra)
  write.csv(d, file.path(toy_dir, path), row.names = FALSE)
}
spec_files <- unique(vapply(RESOLUTION_ROWS, function(r) r[[1]], character(1)))
for (path in spec_files) {
  rows <- Filter(function(r) identical(r[[1]], path), RESOLUTION_ROWS)
  write_toy_paired(path, data.frame(response = vapply(rows, function(r) r[[2]], character(1)),
                                    from = vapply(rows, function(r) r[[4]], numeric(1)),
                                    to = vapply(rows, function(r) r[[5]], numeric(1))))
}
resolution <- build_resolution(toy_dir)
report(length(resolution) == length(RESOLUTION_ROWS) + 2L,
       "the resolution table has one row per comparison (%d lines)", length(resolution))
report(identical(res_cells(resolution[3]),
                 c("Hold window, R2E first surgeries", "+0.50 [\u22121.00, +2.00]", "2.0",
                   "1,234")),
       "a resolution row prints the difference, the half-width and the replications: %s",
       resolution[3])
report(inherits(try(build_resolution(file.path(toy_dir, "missing")), silent = TRUE), "try-error"),
       "a resolution table over absent evidence is an error rather than an empty table")

write.csv(data.frame(gate_enabled = c(1, 1, 0), icu_pathway_n = c(100, 300, 5),
                     icu_pathway_dow = c(1, 1, 0), hold_pathway_n = c(1000, 3000, 7),
                     hold_pathway_dow = c(2, 6, 0)),
          file.path(toy_dir, "icu_gate", "icu_gate_replications.csv"), row.names = FALSE)
pathways <- build_icu_gate_pathways(toy_dir)
report(identical(res_cells(pathways[3]), c("Intensive care bed", "400", "2", "0.50%")),
       "the intensive care pathway pools its counts over the enabled arm only: %s", pathways[3])
report(identical(res_cells(pathways[4]), c("Holding bed", "4,000", "8", "0.20%")),
       "the holding pathway pools its counts and rate over the enabled arm only: %s", pathways[4])

# ── 4. Nothing outside a span states a figure or an interpretation ──────────

cat("\n-- the document states no figure outside a span and no recommendation --\n")

prose <- gsub(RESULTS_SPAN_PATTERN, "", text, perl = TRUE)
prose <- sub("(?s)\n## References.*$", "", prose, perl = TRUE)
lines <- strsplit(prose, "\n", fixed = TRUE)[[1]]
lines <- lines[!grepl("^(#|\\|)", lines)]
lines <- gsub("`[^`]*`", "", lines)
lines <- gsub("\\$[^$]*\\$", "", lines)
lines <- gsub("\\([^)]*\\)", "", lines)
words <- paste(lines, collapse = "\n")

decimals <- regmatches(words, gregexpr("[0-9][0-9,]*\\.[0-9]+", words))[[1]]
report(length(decimals) == 0L, "no decimal figure is typed outside a span (found: %s)",
       paste(utils::head(decimals, 5), collapse = ", "))
pcts <- regmatches(words, gregexpr("[0-9]+(\\.[0-9]+)?%", words))[[1]]
stray <- setdiff(unique(pcts), ALLOWED_PERCENTAGES)
report(length(stray) == 0L, "no measured percentage is typed outside a span (found: %s)",
       paste(stray, collapse = ", "))

neutral <- sub("It makes no recommendation", "", words, fixed = TRUE)
advice <- regmatches(neutral, gregexpr("\\b(should|recommend[a-z]*|must|ought)\\b", neutral,
                                       ignore.case = TRUE))[[1]]
report(length(advice) == 0L, "the document carries no recommendation (found: %s)",
       paste(unique(advice), collapse = ", "))
report(!grepl("—", text), "the document uses no em dash")

if (length(failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(failures)))
  for (f in failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nEvery result table and quoted figure agrees with the tracked evidence.\n")
quit(status = 0L)
