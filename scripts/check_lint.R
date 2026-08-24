#!/usr/bin/env Rscript
##############################################
## scripts/check_lint.R                     ##
## Lint ratchet against the code standard   ##
##############################################
#
#   Rscript scripts/check_lint.R [--refresh-baseline]
#
# Runs `lintr` over every R source file in the repository under the rules
# `.lintr` encodes, adds the one machine-checkable rule `lintr` has no linter
# for, and compares the finding count per rule against the tracked baseline in
# `scripts/lint_baseline.csv`. Exits non-zero if any count has risen above its
# baseline. `--refresh-baseline` is the only way to rewrite the tracked
# baseline.
#
# The check is a ratchet rather than a gate on zero. The codebase carries 919
# over-long lines and seventeen functions past the length limit, recorded under
# Current conformance in `docs/STYLE_GUIDE.md`, and a gate that is red on
# arrival is a gate that gets ignored. Ratcheting defends the conformance the
# code already has: a pull request may not add a finding, and each one that
# removes findings lowers the bar for the next.
#
# A count that has fallen is reported but does not fail the check, because a
# contributor who improves the code should not be made to update a baseline to
# get a green gate. Refreshing the baseline after such a pull request is what
# tightens the ratchet, and is a maintainer action.
#
# Two rules are added here because `lintr` ships no linter for them.
#
# R9 forbids emoji and pictographic characters in R source, because their
# treatment depends on the session locale. The scan below covers the
# pictographic and symbol ranges the rule is aimed at. Box-drawing characters
# are permitted by R9 itself, for the file banner and the section rule;
# mathematical and typographic characters (the em dash, the plus-minus sign,
# the Greek letters of a Morris index) are outside the scan, since a locale
# that renders them wrongly would already have broken every document in the
# repository.
#
# D1 caps a function body at 100 lines. The count below is taken from the
# parse data rather than from a brace-matching scan, so a brace inside a string
# or a comment cannot move it, and it measures the whole `function(...) { ... }`
# expression, which is the span `docs/STYLE_GUIDE.md` lists its seventeen
# over-long functions by.

SOURCE_DIRS   <- c("R", "scripts")
SOURCE_FILES  <- c("app.R", "run.R")
BASELINE_PATH <- file.path("scripts", "lint_baseline.csv")
R9_RULE       <- "pictographic_character"
D1_RULE       <- "function_length"
D1_MAX_LINES  <- 100L

# Codepoint ranges scanned for R9: arrows, enclosed alphanumerics, geometric
# shapes through dingbats, the miscellaneous symbols supplement, the emoji
# variation selector and the emoji planes. Box drawing (U+2500 to U+257F) sits
# below the first of these ranges and is therefore permitted, as R9 requires
# for the banner and the section rule. The scan compares codepoints rather
# than matching a pattern, so it neither depends on the session locale nor
# needs a non-ASCII character in this file to enforce a rule against them.
R9_RANGES <- list(c(0x2190L, 0x21FFL), c(0x2460L, 0x24FFL),
                  c(0x25A0L, 0x27BFL), c(0x2B00L, 0x2BFFL),
                  c(0xFE0FL, 0xFE0FL), c(0x1F000L, 0x1FAFFL))

failures <- character(0)

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

#' Every R source file the standard applies to
#'
#' @return A character vector of file paths.
source_files <- function() {
  found <- unlist(lapply(SOURCE_DIRS, function(d) {
    list.files(d, pattern = "[.]R$", full.names = TRUE)
  }))
  sort(c(found, SOURCE_FILES[file.exists(SOURCE_FILES)]))
}

#' Finding counts per linter, from lintr under the repository's .lintr
#'
#' @param files Character vector of file paths to lint.
#' @return A named integer vector of counts, keyed by linter name.
#' @details Files are linted individually rather than through `lint_dir()` so
#'   that the file set is this script's decision rather than a glob's, and so
#'   that a file outside `SOURCE_DIRS` cannot enter the count silently.
lint_counts <- function(files) {
  findings <- do.call(rbind, lapply(files, function(f) {
    as.data.frame(lintr::lint(f))
  }))
  if (is.null(findings) || nrow(findings) == 0L) return(integer(0))
  counts <- table(findings$linter)
  stats::setNames(as.integer(counts), names(counts))
}

#' Count of R9 pictographic characters across the source files
#'
#' @param files Character vector of file paths to scan.
#' @return A single integer: the number of matching characters found.
pictographic_count <- function(files) {
  total <- 0L
  for (f in files) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    points <- unlist(lapply(lines, utf8ToInt))
    points <- points[!is.na(points) & points > 127L]
    for (range in R9_RANGES) {
      total <- total + sum(points >= range[[1L]] & points <= range[[2L]])
    }
  }
  total
}

#' Count of functions whose body exceeds the D1 line limit
#'
#' @param files Character vector of file paths to scan.
#' @return A single integer: the number of over-long function definitions.
#' @details A function's span is the line range of the `function` expression
#'   itself, taken from the parse data, so a nested function is counted in its
#'   own right as well as within its parent.
function_length_count <- function(files) {
  total <- 0L
  for (f in files) {
    parsed <- parse(f, keep.source = TRUE)
    pd     <- utils::getParseData(parsed)
    if (is.null(pd) || nrow(pd) == 0L) next
    heads  <- pd[pd$token %in% c("FUNCTION", "OP-LAMBDA"), ]
    for (i in seq_len(nrow(heads))) {
      body_expr <- pd[pd$id == heads$parent[[i]], ]
      if (nrow(body_expr) == 0L) next
      span <- body_expr$line2[[1L]] - body_expr$line1[[1L]] + 1L
      if (span > D1_MAX_LINES) total <- total + 1L
    }
  }
  total
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
unknown <- setdiff(args, "--refresh-baseline")
if (length(unknown) > 0L) {
  stop(sprintf("unrecognised argument: %s", paste(unknown, collapse = ", ")))
}
refresh <- "--refresh-baseline" %in% args

if (!requireNamespace("lintr", quietly = TRUE)) {
  message("[FAIL] lintr is not installed; the lint ratchet cannot be evaluated.")
  message("       Install it with install.packages(\"lintr\").")
  quit(status = 1L)
}

files <- source_files()
message(sprintf("Linting %d R source files under %s\n", length(files),
                paste(c(SOURCE_DIRS, SOURCE_FILES), collapse = ", ")))

counts <- lint_counts(files)
counts[[R9_RULE]] <- pictographic_count(files)
counts[[D1_RULE]] <- function_length_count(files)

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
  now  <- if (rule %in% names(counts)) counts[[rule]] else 0L
  was  <- if (rule %in% names(baseline)) baseline[[rule]] else 0L
  if (now < was) improved <- improved + (was - now)
  report(now <= was,
         sprintf("%-32s %5d (baseline %5d)%s", rule, now, was,
                 if (now < was) "  improved" else if (now > was) "  RISEN" else ""))
}

message("")
message(sprintf("Total findings %d against a baseline of %d",
                sum(counts), sum(baseline)))
if (improved > 0L) {
  message(sprintf("%d finding(s) removed since the baseline was taken; refresh it",
                  improved))
  message(sprintf("with `Rscript %s --refresh-baseline` to tighten the ratchet.",
                  BASELINE_PATH))
}

if (length(failures) > 0L) {
  message(sprintf("\n%d rule(s) FAILED:", length(failures)))
  for (f in failures) message("  - ", f)
  message("\nRun `Rscript -e 'print(lintr::lint_dir(\".\"))'` to see the findings.")
  quit(status = 1L)
}

message("\nAll lint rules are at or below their baseline.")
quit(status = 0L)
