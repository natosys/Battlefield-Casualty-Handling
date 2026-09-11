#!/usr/bin/env Rscript
##############################################################################
## scripts/check_structure_tables.R                                         ##
## Regression check — the two structure tables list every module and        ##
## script they claim to                                                     ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_structure_tables.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Two tables claim to be complete indexes of the
# codebase: CLAUDE.md's Repository Structure table, which says it covers the
# repository as a whole, and README.md's Codebase Structure table, which is
# what CLAUDE.md points a reader at for detail on each R/ module. Nothing
# checked either claim and both had drifted (Issue #342).
#
# A missing row breaks nothing that runs, which is why it survives: the only
# thing that ever caught one was a reviewer noticing. R/cli.R had no row in
# the README table from the day it was created, and PR #340 added two files
# and gave neither a row, corrected by hand afterwards. A structural index
# that is silently incomplete is worse than none, because a reader who checks
# it and finds nothing concludes the file does not exist.
#
# What the tables promise, and therefore what this asserts:
#
#   1. Every R/*.R module has a row in the README's Codebase Structure table.
#   2. Every R/*.R module has a row in CLAUDE.md's Repository Structure table.
#   3. Every scripts/check_*.R has a row in CLAUDE.md's table.
#   4. Every other scripts/*.R and scripts/*.sh entry point has a row in it.
#   5. Neither table names a path that does not exist, so a renamed or deleted
#      file cannot leave a row behind pointing at nothing.
#
# The file set comes from `git ls-files` rather than a filesystem glob, so an
# untracked scratch file under scripts/ cannot fail the check. A row may name
# several paths, as CLAUDE.md's do for the Playwright toolchain and the renv
# files, so every backticked path in a row's first cell is collected rather
# than assuming one path per row.

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
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

#' Every path named in the first cell of a markdown table's rows
#'
#' @param path File the table lives in.
#' @param heading Regular expression matching the heading the table follows.
#' @return Character vector of the backticked paths, empty where the heading
#'   is not found.
#' @details The table is taken to run from the first line beginning with a
#'   pipe after the heading to the first non-empty line that does not, which
#'   is what separates it from the prose beneath. Only the first cell is read,
#'   the later cells naming functions and files the row merely discusses.
table_paths <- function(path, heading) {
  lines <- readLines(path, warn = FALSE)
  start <- grep(heading, lines)
  if (length(start) == 0) return(character(0))
  tail_lines <- lines[seq(start[1] + 1L, length(lines))]
  in_table <- FALSE
  found <- character(0)
  for (line in tail_lines) {
    if (startsWith(line, "|")) {
      in_table <- TRUE
      cell <- strsplit(line, "|", fixed = TRUE)[[1]][2]
      if (!is.na(cell)) {
        found <- c(found, regmatches(cell, gregexpr("`[^`]+`", cell))[[1]])
      }
    } else if (in_table && nzchar(trimws(line))) {
      break
    }
  }
  unique(gsub("`", "", found, fixed = TRUE))
}

#' The repository's tracked files matching a pattern
#'
#' @param pattern Regular expression the whole path must match.
#' @return Character vector of tracked paths, sorted.
#' @details Tracked rather than globbed, so an untracked scratch file cannot
#'   fail the check.
tracked_files <- function(pattern) {
  all_files <- system2("git", c("ls-files"), stdout = TRUE)
  sort(grep(pattern, all_files, value = TRUE))
}

#' Assert that every file in a set has a row in a table
#'
#' @param files Paths that must appear.
#' @param paths Paths the table names.
#' @param what Plain-English name of the file set, for the message.
#' @param table_name Plain-English name of the table, for the message.
#' @return Invisible NULL.
assert_covered <- function(files, paths, what, table_name) {
  missing <- setdiff(files, paths)
  report(length(missing) == 0,
         "every %s (%d) has a row in %s%s",
         what, length(files), table_name,
         if (length(missing)) sprintf("; absent: %s", paste(missing, collapse = ", ")) else "")
}

#' Heading the README's module index follows
README_TABLE <- "^#+\\s*Codebase Structure\\s*$"

#' Heading CLAUDE.md's repository index follows
CLAUDE_TABLE <- "^##\\s*Repository Structure\\s*$"

readme_paths <- table_paths("README.md", README_TABLE)
claude_paths <- table_paths("CLAUDE.md", CLAUDE_TABLE)

cat("Structure table completeness check\n\n")

report(length(readme_paths) > 0, "the README's Codebase Structure table parses (%d paths)",
       length(readme_paths))
report(length(claude_paths) > 0, "CLAUDE.md's Repository Structure table parses (%d paths)",
       length(claude_paths))

# ── 1 to 4. Every file the tables promise to list is listed ────────────────

cat("\n-- every module and script has a row --\n")

modules      <- tracked_files("^R/[^/]+\\.R$")
check_scripts <- tracked_files("^scripts/check_[^/]+\\.R$")
other_scripts <- setdiff(tracked_files("^scripts/[^/]+\\.(R|sh)$"), check_scripts)

assert_covered(modules, readme_paths, "R/ module", "the README's Codebase Structure table")
assert_covered(modules, claude_paths, "R/ module", "CLAUDE.md's Repository Structure table")
assert_covered(check_scripts, claude_paths, "regression check script",
               "CLAUDE.md's Repository Structure table")
assert_covered(other_scripts, claude_paths, "other scripts/ entry point",
               "CLAUDE.md's Repository Structure table")

# ── 5. No row names a path that no longer exists ───────────────────────────

cat("\n-- no row points at nothing --\n")

#' Assert that every path a table names exists on disk
#'
#' @param paths Paths the table names.
#' @param table_name Plain-English name of the table, for the message.
#' @return Invisible NULL.
#' @details A directory row carries a trailing slash and an entry naming no
#'   separator or extension is prose rather than a path, so neither is
#'   resolved. What this catches is a row left behind by a rename.
assert_no_stale <- function(paths, table_name) {
  candidates <- paths[!endsWith(paths, "/") & grepl("[./]", paths)]
  stale <- candidates[!file.exists(candidates)]
  report(length(stale) == 0,
         "every path %s names exists (%d checked)%s",
         table_name, length(candidates),
         if (length(stale)) sprintf("; absent: %s", paste(stale, collapse = ", ")) else "")
}

assert_no_stale(readme_paths, "the README's Codebase Structure table")
assert_no_stale(claude_paths, "CLAUDE.md's Repository Structure table")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All structure table checks passed.\n")
quit(status = 0)
