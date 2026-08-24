#!/usr/bin/env Rscript
##############################################
## scripts/run_all_checks.R                 ##
## Regression check suite runner            ##
##############################################
#
#   Rscript scripts/run_all_checks.R [--fast | --slow | --all]
#                                    [--only <pattern>] [--list]
#                                    [--log-dir <path>] [--no-tree-check]
#
# Runs the repository's `scripts/check_*.R` regression checks, reports one
# PASS/FAIL line and a runtime per check, and exits non-zero if any check
# fails. Checks are discovered by glob rather than from a list, so a newly
# added `check_*.R` is picked up without editing this file.
#
# The suite divides on cost. `scripts/README.md` records that fourteen checks
# together take under twelve minutes while `check_dow_calibration.R` alone
# takes forty-five, because at its defaults it runs 450 replications across
# three scenario profiles. The `--fast` selection is what a per-pull-request
# gate can afford; `--slow` is what a scheduled or on-demand job runs. A check
# not named in SLOW_CHECKS is fast, so the default classification of a new
# check is the one that gets it executed on every pull request.
#
# Two checks rewrite tracked documents in place rather than only inspecting
# them, so exit status alone does not report their finding: they regenerate the
# block or the table of contents and succeed, leaving the drift they found as a
# modification in the working tree. For those the runner compares `git status`
# before and after and treats a change as a failure, which is the signal a gate
# needs. `--no-tree-check` disables the comparison for a working tree that is
# already dirty for unrelated reasons.
#
# Check output is written to a per-check log file rather than to the console,
# and only a failing check's tail is printed. Several checks emit roughly one
# hundred `simmer` end-of-run warnings apiece, which would otherwise bury the
# one line a reader of a gate's log needs.

CHECK_DIR       <- "scripts"
CHECK_PATTERN   <- "^check_.*[.]R$"
DEFAULT_LOG_DIR <- file.path("outputs", "checks")
FAIL_TAIL_LINES <- 40L

# Checks too slow for a per-pull-request gate, with the runtime measured in
# scripts/README.md. Add a check here only on the evidence of a measurement.
SLOW_CHECKS <- c("check_dow_calibration.R")

# Checks that rewrite tracked files in place; see the banner above.
MUTATING_CHECKS <- c("check_env_data_summary.R", "check_markdown.R")

#' Print a message to standard error without a call prefix
#'
#' @param ... Parts of the line, pasted together with no separator.
#' @return Invisible NULL; called for its output.
say <- function(...) {
  message(paste0(..., collapse = ""))
}

#' Parse the command line into a selection, a filter and runner options
#'
#' @param args Character vector of command line arguments, as returned by
#'   `commandArgs(trailingOnly = TRUE)`.
#' @return A list with elements `selection` ("fast", "slow" or "all"), `only`
#'   (a regular expression or NA), `list_only` (logical), `log_dir` and
#'   `tree_check` (logical).
#' @details Fails with `stop()` on an unrecognised argument rather than
#'   silently running the whole suite, so a mistyped flag in a workflow file
#'   surfaces as a red job rather than as a forty-five minute one.
parse_args <- function(args) {
  opts <- list(selection = "all", only = NA_character_, list_only = FALSE,
               log_dir = DEFAULT_LOG_DIR, tree_check = TRUE)
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (a == "--fast") {
      opts$selection <- "fast"
    } else if (a == "--slow") {
      opts$selection <- "slow"
    } else if (a == "--all") {
      opts$selection <- "all"
    } else if (a == "--list") {
      opts$list_only <- TRUE
    } else if (a == "--no-tree-check") {
      opts$tree_check <- FALSE
    } else if (a == "--only") {
      if (i == length(args)) stop("--only requires a pattern argument")
      opts$only <- args[[i + 1L]]
      i <- i + 1L
    } else if (a == "--log-dir") {
      if (i == length(args)) stop("--log-dir requires a path argument")
      opts$log_dir <- args[[i + 1L]]
      i <- i + 1L
    } else {
      stop(sprintf("unrecognised argument: %s", a))
    }
    i <- i + 1L
  }
  opts
}

#' Discover the check scripts the selection asks for
#'
#' @param selection One of "fast", "slow" or "all".
#' @param only A regular expression matched against the file name, or NA for
#'   no filtering.
#' @return A character vector of check file names, sorted alphabetically.
discover_checks <- function(selection, only) {
  found <- sort(list.files(CHECK_DIR, pattern = CHECK_PATTERN))
  if (selection == "fast") found <- setdiff(found, SLOW_CHECKS)
  if (selection == "slow") found <- intersect(found, SLOW_CHECKS)
  if (!is.na(only)) found <- grep(only, found, value = TRUE)
  found
}

#' The tracked working tree state, as a single string
#'
#' @return A one-element character vector holding `git status --porcelain`
#'   output, or NA when git is unavailable or the directory is not a
#'   repository.
tree_state <- function() {
  args <- c("status", "--porcelain")
  out <- suppressWarnings(tryCatch(
    system2("git", args, stdout = TRUE, stderr = FALSE),
    error = function(e) NULL
  ))
  if (is.null(out) || !is.null(attr(out, "status"))) return(NA_character_)
  paste(out, collapse = "\n")
}

#' Format a duration in seconds as minutes and seconds
#'
#' @param secs Elapsed time in seconds.
#' @return A one-element character vector such as "9 s" or "4 min 04 s".
format_runtime <- function(secs) {
  secs <- round(secs)
  if (secs < 60) return(sprintf("%d s", secs))
  sprintf("%d min %02d s", secs %/% 60, secs %% 60)
}

#' Print the tail of a failing check's log
#'
#' @param log_path Path to the log file the check's output was written to.
#' @return Invisible NULL; called for its output.
print_log_tail <- function(log_path) {
  if (!file.exists(log_path)) return(invisible(NULL))
  lines <- readLines(log_path, warn = FALSE)
  lines <- lines[!grepl("leaving without releasing", lines, fixed = TRUE)]
  omitted <- max(0L, length(lines) - FAIL_TAIL_LINES)
  if (omitted > 0L) {
    say(sprintf("      | ... %d earlier line(s) omitted; the full log is %s",
                omitted, log_path))
  }
  for (l in utils::tail(lines, FAIL_TAIL_LINES)) say("      | ", l)
  invisible(NULL)
}

#' Run one check script and report its result
#'
#' @param check File name of the check, relative to `scripts/`.
#' @param log_dir Directory the check's output log is written to.
#' @param tree_check Whether a working tree modification counts as a failure
#'   for a check named in MUTATING_CHECKS.
#' @return A list with elements `check`, `ok` (logical), `status` (the exit
#'   status), `seconds` and `note` (a reason string, empty when the check
#'   passed).
#' @details Output is captured to `log_dir` rather than inherited, so that the
#'   suite's own PASS/FAIL lines stay readable; a failing check's tail is
#'   printed by the caller.
run_check <- function(check, log_dir, tree_check) {
  path     <- file.path(CHECK_DIR, check)
  log_path <- file.path(log_dir, sub("[.]R$", ".log", check))
  before   <- if (tree_check && check %in% MUTATING_CHECKS) tree_state() else NA_character_
  started  <- Sys.time()
  status   <- system2("Rscript", shQuote(path), stdout = log_path, stderr = log_path)
  seconds  <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  note     <- ""
  ok       <- identical(as.integer(status), 0L)
  if (!ok) note <- sprintf("exit status %s", status)
  if (ok && !is.na(before)) {
    after <- tree_state()
    if (!is.na(after) && !identical(before, after)) {
      ok   <- FALSE
      note <- "modified tracked files: the document it regenerates had drifted"
    }
  }
  list(check = check, ok = ok, status = status, seconds = seconds, note = note,
       log = log_path)
}

#' Print the closing summary table and return the suite's exit status
#'
#' @param results A list of `run_check()` results.
#' @return 0L when every check passed, 1L otherwise.
summarise_results <- function(results) {
  failed <- Filter(function(r) !r$ok, results)
  total  <- sum(vapply(results, function(r) r$seconds, numeric(1)))
  say("")
  say(sprintf("%d of %d checks passed in %s",
              length(results) - length(failed), length(results),
              format_runtime(total)))
  if (length(failed) == 0L) return(0L)
  say("")
  say("Failed:")
  for (r in failed) say(sprintf("  %-40s %s", r$check, r$note))
  1L
}

# Every check reads env_data.json and the R/ modules by relative path, so the
# suite is only meaningful from the repository root.
if (!file.exists("env_data.json") || !dir.exists(CHECK_DIR)) {
  stop("run_all_checks.R must be run from the repository root, and was run in ",
       getwd(), call. = FALSE)
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
checks <- discover_checks(args$selection, args$only)

if (length(checks) == 0L) {
  say("No checks matched the selection.")
  quit(status = 1L)
}

if (args$list_only) {
  for (check in checks) {
    say(sprintf("  %-42s %s", check,
                if (check %in% SLOW_CHECKS) "slow" else "fast"))
  }
  quit(status = 0L)
}

dir.create(args$log_dir, recursive = TRUE, showWarnings = FALSE)
say(sprintf("Running %d %s check(s); output in %s",
            length(checks), args$selection, args$log_dir))
say("")

results <- vector("list", length(checks))
for (i in seq_along(checks)) {
  result <- run_check(checks[[i]], args$log_dir, args$tree_check)
  results[[i]] <- result
  say(sprintf("  [%s] %-42s %8s%s",
              if (result$ok) "PASS" else "FAIL", result$check,
              format_runtime(result$seconds),
              if (nzchar(result$note)) paste0("  ", result$note) else ""))
  if (!result$ok) print_log_tail(result$log)
}

quit(status = summarise_results(results))
