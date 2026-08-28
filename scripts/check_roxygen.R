#!/usr/bin/env Rscript
##############################################
## scripts/check_roxygen.R                  ##
## Roxygen ratchet against R1 and R2        ##
##############################################
#
#   Rscript scripts/check_roxygen.R [--refresh-baseline] [--list]
#
# Scans every R source file in the repository for named function definitions,
# asserts the two documentation rules of `docs/STYLE_GUIDE.md` that a machine
# can decide, and compares the finding count per rule against the tracked
# baseline in `scripts/roxygen_baseline.csv`. Exits non-zero if any count has
# risen above its baseline. `--refresh-baseline` is the only way to rewrite the
# tracked baseline; `--list` prints every finding with its file and line and
# decides nothing.
#
# The rules checked are R1, every function carries a roxygen header; the part
# of R2 that is decidable from the source: a header opens with a title,
# carries an `@param` for every argument and no `@param` for an argument the
# function does not take, and carries a `@return`; and the presence half of
# R3, every file-scope constant carrying a roxygen header. Whether an
# `@details` is warranted, and whether a constant's header states where its
# value comes from and what would break if it changed, are judgements about
# content rather than structure, so they stay with the reviewer, as
# `docs/STYLE_GUIDE.md`'s enforcement summary records.
#
# Only a named definition is checked. An anonymous function passed to `lapply`
# or to a simmer verb has no name to document and no place to put a header, so
# counting it would put a permanent floor under the baseline; a function bound
# to a name is checked wherever it sits, including inside another function,
# which is what R1 requires. The scan reads the parse data rather than matching
# a pattern, so a `function` written inside a string or a comment cannot enter
# the count and an argument list broken across lines is still read whole.
#
# The gate is a ratchet, for the reason `scripts/check_lint.R` states at
# greater length: a gate that is red on arrival is a gate that gets ignored. A
# count that has fallen is reported and does not fail the check; refreshing the
# baseline afterwards is the maintainer action that tightens the ratchet.

#' The directories and files the standard's documentation rules apply to
#'
#' @details The same set `scripts/check_lint.R` lints, so a file gated by one
#'   is gated by the other. A directory omitted here is a directory whose
#'   documentation nothing checks.
SOURCE_DIRS   <- c("R", "scripts", "tests")

#' The two entry-point files at the repository root
#'
#' @return Character vector of paths, scanned alongside SOURCE_DIRS.
SOURCE_FILES  <- c("app.R", "run.R")

#' Path of the tracked per-rule baseline the ratchet defends
#'
#' @details Written only under `--refresh-baseline`, which is what makes a
#'   count fall deliberately rather than by a run that happened to see fewer
#'   files.
BASELINE_PATH <- file.path("scripts", "roxygen_baseline.csv")

#' The rule names the baseline is keyed by
#'
#' @details Fixed here rather than taken from whatever the scan finds, so a
#'   rule falling to zero still appears in the report with a count of zero
#'   instead of vanishing from it, which would read as the rule having been
#'   removed.
RULES <- c("missing_header", "missing_title", "missing_param",
           "undocumented_param", "missing_return", "missing_constant_header")

failures <- character(0)
findings <- list()

#' Record a failure and print its line
#'
#' @param msg One-line description of the failed assertion.
#' @return Invisible NULL; called for its side effects.
fail <- function(msg) {
  failures <<- c(failures, msg)
  invisible(NULL)
}

#' Print one PASS or FAIL line, recording a failure
#'
#' @param ok Logical: whether the assertion held.
#' @param msg One-line description of the assertion.
#' @return Invisible `ok`.
report <- function(ok, msg) {
  if (isTRUE(ok)) {
    message(sprintf("  [PASS] %s", msg))
  } else {
    message(sprintf("  [FAIL] %s", msg))
    fail(msg)
  }
  invisible(ok)
}

#' Record one finding against a rule
#'
#' @param rule Name of the rule the finding is counted under.
#' @param file Path of the file the finding sits in.
#' @param line Line number of the function definition.
#' @param name Name the function is bound to.
#' @param detail One-line description of what is missing.
#' @return Invisible NULL; called for its side effect on the finding list.
note <- function(rule, file, line, name, detail) {
  findings[[length(findings) + 1L]] <<- data.frame(
    rule = rule, file = file, line = line, name = name, detail = detail,
    stringsAsFactors = FALSE
  )
  invisible(NULL)
}

#' Every R source file the standard applies to
#'
#' @return A character vector of file paths, sorted.
#' @details Recurses, because the test suite nests its files a directory deeper
#'   than `R/` and `scripts/` do and R1 applies to them too.
source_files <- function() {
  found <- unlist(lapply(SOURCE_DIRS, function(d) {
    if (!dir.exists(d)) return(character(0))
    list.files(d, pattern = "[.]R$", full.names = TRUE, recursive = TRUE)
  }))
  sort(c(found, SOURCE_FILES[file.exists(SOURCE_FILES)]))
}

#' Terminal text of a parse-data expression, in source order
#'
#' @param pd Parse data frame for one file.
#' @param id Identifier of the expression to read.
#' @return A character vector of the terminal tokens' text.
terminal_text <- function(pd, id) {
  kids <- pd[pd$parent == id, ]
  if (nrow(kids) == 0L) return(character(0))
  kids <- kids[order(kids$line1, kids$col1), ]
  unlist(lapply(seq_len(nrow(kids)), function(i) {
    if (kids$terminal[[i]]) kids$text[[i]] else terminal_text(pd, kids$id[[i]])
  }))
}

#' Named function definitions in one file
#'
#' @param file Path of the R source file to read.
#' @return A data frame of name, line (the line the assignment starts on) and
#'   args (a list column of formal argument names), one row per definition;
#'   empty when the file defines no named function.
#' @details A definition counts as named when the `function` expression is the
#'   right-hand side of an assignment whose left-hand side is a single symbol
#'   or string. Assignment into a list element or a slot is not a name a
#'   roxygen header can document, so it is not counted.
function_defs <- function(file) {
  pd <- utils::getParseData(parse(file, keep.source = TRUE))
  empty <- data.frame(name = character(0), line = integer(0),
                      stringsAsFactors = FALSE)
  empty$args <- list()
  if (is.null(pd) || nrow(pd) == 0L) return(empty)

  heads <- pd[pd$token %in% c("FUNCTION", "OP-LAMBDA"), ]
  rows <- list()
  for (i in seq_len(nrow(heads))) {
    fn_expr <- heads$parent[[i]]
    assign_expr <- pd$parent[pd$id == fn_expr]
    if (length(assign_expr) != 1L || assign_expr == 0L) next
    kids <- pd[pd$parent == assign_expr, ]
    kids <- kids[order(kids$line1, kids$col1), ]
    if (nrow(kids) != 3L) next
    if (!kids$token[[2L]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) next
    if (kids$id[[3L]] != fn_expr) next
    lhs <- terminal_text(pd, kids$id[[1L]])
    if (length(lhs) == 0L && kids$terminal[[1L]]) lhs <- kids$text[[1L]]
    if (length(lhs) != 1L) next
    if (!grepl("^[.A-Za-z\"`'][^ ()$@\\[]*$", lhs)) next

    args <- pd$text[pd$parent == fn_expr & pd$token == "SYMBOL_FORMALS"]
    rows[[length(rows) + 1L]] <- data.frame(
      name = gsub("^[\"`']|[\"`']$", "", lhs),
      line = min(kids$line1),
      stringsAsFactors = FALSE
    )
    rows[[length(rows)]]$args <- list(args)
  }
  if (length(rows) == 0L) return(empty)
  do.call(rbind, rows)
}

#' File-scope constants in one file
#'
#' @param file Path of the R source file to read.
#' @return A data frame of name and line, one row per constant; empty when
#'   the file defines none.
#' @details A constant is an assignment at the top level of the file whose
#'   left-hand side is a single UPPER_SNAKE_CASE symbol and whose right-hand
#'   side is not a function, which is what N1 reserves that casing for. An
#'   assignment inside a function is a local and is not counted, however it
#'   is cased.
constant_defs <- function(file) {
  pd <- utils::getParseData(parse(file, keep.source = TRUE))
  empty <- data.frame(name = character(0), line = integer(0),
                      stringsAsFactors = FALSE)
  if (is.null(pd) || nrow(pd) == 0L) return(empty)

  top <- pd$id[pd$parent == 0L & pd$token == "expr"]
  rows <- list()
  for (id in top) {
    kids <- pd[pd$parent == id, ]
    kids <- kids[order(kids$line1, kids$col1), ]
    if (nrow(kids) != 3L) next
    if (!kids$token[[2L]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) next
    lhs <- terminal_text(pd, kids$id[[1L]])
    if (length(lhs) == 0L && kids$terminal[[1L]]) lhs <- kids$text[[1L]]
    if (length(lhs) != 1L || !grepl("^[A-Z][A-Z0-9_]*$", lhs)) next
    rhs <- pd[pd$parent == kids$id[[3L]], ]
    if (any(rhs$token %in% c("FUNCTION", "OP-LAMBDA"))) next
    rows[[length(rows) + 1L]] <- data.frame(name = lhs, line = min(kids$line1),
                                            stringsAsFactors = FALSE)
  }
  if (length(rows) == 0L) return(empty)
  do.call(rbind, rows)
}

#' Roxygen block immediately above a line
#'
#' @param lines Character vector of the file's lines.
#' @param line Line number the function definition starts on.
#' @return A character vector of the block's lines with the `#'` prefix
#'   stripped, empty when no block sits immediately above.
#' @details The block must be contiguous with the definition. A blank line
#'   between a header and its function separates them, which is what makes a
#'   header orphaned by an edit visible to this check rather than silently
#'   accepted.
roxygen_block <- function(lines, line) {
  i <- line - 1L
  block <- character(0)
  while (i >= 1L && grepl("^\\s*#'", lines[[i]])) {
    block <- c(sub("^\\s*#'\\s?", "", lines[[i]]), block)
    i <- i - 1L
  }
  block
}

#' Argument names a roxygen block documents
#'
#' @param block Character vector of the block's lines, prefix stripped.
#' @return A character vector of documented argument names.
#' @details One `@param` may name several arguments separated by commas, which
#'   roxygen2 itself allows, so the tag's name field is split before matching.
#'   Dots are read as the `...` argument.
documented_params <- function(block) {
  tags <- grep("^@param\\s", block, value = TRUE)
  if (length(tags) == 0L) return(character(0))
  names <- sub("^@param\\s+([^ ]+).*$", "\\1", tags)
  unique(trimws(unlist(strsplit(names, ","))))
}

#' Whether a roxygen block opens with a title line
#'
#' @param block Character vector of the block's lines, prefix stripped.
#' @return TRUE when the first non-empty line is prose rather than a tag.
has_title <- function(block) {
  content <- trimws(block)
  content <- content[nzchar(content)]
  length(content) > 0L && !grepl("^@", content[[1L]])
}

#' Scan one file for R1 and R2 findings
#'
#' @param file Path of the R source file to scan.
#' @return Invisible NULL; called for its side effect on the finding list.
scan_file <- function(file) {
  defs <- function_defs(file)
  if (nrow(defs) == 0L) return(invisible(NULL))
  lines <- readLines(file, warn = FALSE)

  for (i in seq_len(nrow(defs))) {
    name  <- defs$name[[i]]
    line  <- defs$line[[i]]
    args  <- defs$args[[i]]
    block <- roxygen_block(lines, line)

    if (length(block) == 0L) {
      note("missing_header", file, line, name, "no roxygen header")
      next
    }
    if (!has_title(block)) {
      note("missing_title", file, line, name, "header opens with a tag")
    }
    documented <- documented_params(block)
    for (arg in setdiff(args, documented)) {
      note("missing_param", file, line, name, sprintf("@param %s", arg))
    }
    for (arg in setdiff(documented, args)) {
      note("undocumented_param", file, line, name,
           sprintf("@param %s names no argument", arg))
    }
    if (!any(grepl("^@return\\b", block))) {
      note("missing_return", file, line, name, "no @return")
    }
  }

  consts <- constant_defs(file)
  for (i in seq_len(nrow(consts))) {
    if (length(roxygen_block(lines, consts$line[[i]])) == 0L) {
      note("missing_constant_header", file, consts$line[[i]], consts$name[[i]],
           "no roxygen header")
    }
  }
  invisible(NULL)
}

#' Read the tracked baseline counts
#'
#' @return A named integer vector of counts, empty when no baseline exists.
read_baseline <- function() {
  if (!file.exists(BASELINE_PATH)) return(integer(0))
  df <- utils::read.csv(BASELINE_PATH, stringsAsFactors = FALSE)
  stats::setNames(as.integer(df$count), df$rule)
}

#' Write the baseline file from a set of counts
#'
#' @param counts Named integer vector of counts, keyed by rule.
#' @return Invisible NULL; called for its side effect on disk.
write_baseline <- function(counts) {
  df <- data.frame(rule = names(counts), count = as.integer(counts),
                   stringsAsFactors = FALSE)
  df <- df[order(df$rule), ]
  utils::write.csv(df, BASELINE_PATH, row.names = FALSE, quote = FALSE)
  invisible(NULL)
}

args <- commandArgs(trailingOnly = TRUE)
unknown <- setdiff(args, c("--refresh-baseline", "--list"))
if (length(unknown) > 0L) {
  stop(sprintf("unrecognised argument: %s", paste(unknown, collapse = ", ")))
}
refresh <- "--refresh-baseline" %in% args
listing <- "--list" %in% args

files <- source_files()
message(sprintf("Scanning %d R source files under %s\n", length(files),
                paste(c(SOURCE_DIRS, SOURCE_FILES), collapse = ", ")))
for (f in files) scan_file(f)

found <- if (length(findings) > 0L) do.call(rbind, findings) else NULL
counts <- stats::setNames(rep(0L, length(RULES)), RULES)
if (!is.null(found)) {
  tally <- table(found$rule)
  counts[names(tally)] <- as.integer(tally)
}

if (listing) {
  if (is.null(found)) {
    message("No findings.")
  } else {
    found <- found[order(found$rule, found$file, found$line), ]
    for (i in seq_len(nrow(found))) {
      message(sprintf("%-18s %s:%d  %s()  %s", found$rule[[i]], found$file[[i]],
                      found$line[[i]], found$name[[i]], found$detail[[i]]))
    }
  }
  quit(status = 0L)
}

if (refresh) {
  write_baseline(counts)
  message(sprintf("Baseline refreshed: %d findings across %d rules written to %s",
                  sum(counts), length(counts), BASELINE_PATH))
  quit(status = 0L)
}

baseline <- read_baseline()
if (length(baseline) == 0L) {
  message(sprintf("[FAIL] no baseline at %s; write one with --refresh-baseline",
                  BASELINE_PATH))
  quit(status = 1L)
}

message("Per-rule counts against baseline")
rules <- sort(union(names(counts), names(baseline)))
improved <- 0L
for (rule in rules) {
  now <- if (rule %in% names(counts)) counts[[rule]] else 0L
  was <- if (rule %in% names(baseline)) baseline[[rule]] else 0L
  if (now < was) improved <- improved + (was - now)
  report(now <= was,
         sprintf("%-20s %5d (baseline %5d)%s", rule, now, was,
                 if (now < was) "  improved" else if (now > was) "  RISEN" else ""))
}

message("")
message(sprintf("Total findings %d against a baseline of %d",
                sum(counts), sum(baseline)))
if (improved > 0L) {
  message(sprintf("%d finding(s) removed since the baseline was taken; refresh it",
                  improved))
  message(sprintf("with `Rscript scripts/check_roxygen.R --refresh-baseline` to %s",
                  "tighten the ratchet."))
}

if (length(failures) > 0L) {
  message(sprintf("\n%d rule(s) FAILED:", length(failures)))
  for (f in failures) message("  - ", f)
  message("\nRun `Rscript scripts/check_roxygen.R --list` to see the findings.")
  quit(status = 1L)
}

message("\nAll roxygen rules are at or below their baseline.")
quit(status = 0L)
