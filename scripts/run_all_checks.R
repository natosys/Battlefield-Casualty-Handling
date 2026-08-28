#!/usr/bin/env Rscript
##############################################
## scripts/run_all_checks.R                 ##
## Regression check suite runner            ##
##############################################
#
#   Rscript scripts/run_all_checks.R [--fast | --slow | --all]
#                                    [--only <pattern>] [--list]
#                                    [--log-dir <path>] [--no-tree-check]
#                                    [--jobs <n> | --jobs auto]
#                                    [--refresh-runtimes]
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
#
# `--jobs` runs several checks at once. Each check is a separate `Rscript`
# process reading the repository and writing its own log and its own temporary
# directory, so concurrency costs nothing in isolation; the two exceptions are
# the checks that regenerate a tracked document, which are detected through a
# repository-wide `git status` comparison that cannot attribute a change to one
# of two concurrent writers, and so are run on their own before the pool
# starts. Checks are dispatched longest first, from the runtimes recorded in
# scripts/check_runtimes.csv, because a suite whose longest check starts last
# finishes no sooner than that check does. `--refresh-runtimes` rewrites those
# recorded runtimes from the run's own measurements, which is the only way the
# tracked file is written.
#
# A check that runs replications forks its own workers, so the runner divides
# the machine between the checks it has in flight: each child is given an
# MC_CORES the `parallel` package turns into its default `mc.cores`, at the
# detected core count divided by the number of jobs or at half the machine,
# whichever is larger. Every published figure is
# a function of its seed rather than of the core count it was measured on,
# which is what `check_measurement_reproducibility.R` asserts, so dividing the
# cores changes what a check costs and not what it concludes.

CHECK_DIR       <- "scripts"
CHECK_PATTERN   <- "^check_.*[.]R$"
DEFAULT_LOG_DIR <- file.path("outputs", "checks")
FAIL_TAIL_LINES <- 40L

#' Measured runtimes, used to dispatch the longest check first
#'
#' @details A check absent from the file is scheduled as if it cost DEFAULT_COST_SECS.
RUNTIME_PATH      <- file.path("scripts", "check_runtimes.csv")
DEFAULT_COST_SECS <- 30
# --jobs auto is capped: past a handful of concurrent checks the gain is the
# machine's, not the runner's, and each job holds a full R process open.
AUTO_JOBS_CAP     <- 8L

# Checks too slow for a per-pull-request gate, with the runtime measured in
# scripts/README.md. Add a check here only on the evidence of a measurement.
SLOW_CHECKS <- c("check_dow_calibration.R")

#' Checks that rewrite tracked files in place; see the banner above
#'
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
#'   (a regular expression or NA), `list_only` (logical), `log_dir`,
#'   `tree_check` (logical), `jobs` (the string given to --jobs) and
#'   `refresh_runtimes` (logical).
#' @details Fails with `stop()` on an unrecognised argument rather than
#'   silently running the whole suite, so a mistyped flag in a workflow file
#'   surfaces as a red job rather than as a forty-five minute one.
parse_args <- function(args) {
  opts <- list(selection = "all", only = NA_character_, list_only = FALSE,
               log_dir = DEFAULT_LOG_DIR, tree_check = TRUE, jobs = "1",
               refresh_runtimes = FALSE)
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
    } else if (a == "--jobs") {
      if (i == length(args)) stop("--jobs requires a count or 'auto'")
      opts$jobs <- args[[i + 1L]]
      i <- i + 1L
    } else if (a == "--refresh-runtimes") {
      opts$refresh_runtimes <- TRUE
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

#' Resolve the --jobs argument into a count of concurrent checks
#'
#' @param spec The string given to --jobs: a positive integer, or "auto".
#' @return An integer count of checks to run at once, at least 1.
#' @details "auto" takes the machine's logical core count, capped at
#'   AUTO_JOBS_CAP, and falls back to one job where the count is unavailable.
#'   Forking is unavailable on Windows, where the runner stays serial whatever
#'   was asked for.
resolve_jobs <- function(spec) {
  if (.Platform$OS.type == "windows") return(1L)
  if (identical(spec, "auto")) {
    detected <- suppressWarnings(parallel::detectCores(logical = TRUE))
    if (is.na(detected)) return(1L)
    return(max(1L, min(AUTO_JOBS_CAP, as.integer(detected))))
  }
  n <- suppressWarnings(as.integer(spec))
  if (is.na(n) || n < 1L) {
    stop(sprintf("--jobs takes a positive integer or 'auto', and was given: %s", spec))
  }
  n
}

#' Cores to allow each concurrently running check to fork into
#'
#' @param jobs Number of checks the runner has in flight at once.
#' @return An integer core count, at least 1, or NA when the machine's core
#'   count is unavailable and the child should be left at its own default.
#' @details Passed to the child as MC_CORES, which `parallel` reads into the
#'   `mc.cores` option the replication framework dispatches on. At one job the
#'   child is left alone, so a serial run behaves exactly as it did before this
#'   division existed.
#'
#'   The share is the machine divided by the job count, or half the machine,
#'   whichever is larger, so the division deliberately oversubscribes. A check
#'   forks only while it is running replications and is otherwise a single
#'   process reading files, so an exact division leaves the machine idle
#'   whenever the pool holds no replication-running check; and the suite cannot
#'   finish before its longest check does, which at the fast selection is a
#'   replication-running one. Dividing exactly measured 8 min 44 s for
#'   check_measurement_reproducibility.R on one core against 2 min 55 s on
#'   four, which made that one check the whole suite's critical path.
child_cores <- function(jobs) {
  if (jobs <= 1L) return(NA_integer_)
  detected <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if (is.na(detected)) return(NA_integer_)
  detected <- as.integer(detected)
  max(1L, detected %/% jobs, detected %/% 2L)
}

#' Read the recorded per-check runtimes
#'
#' @return A named numeric vector of seconds, keyed by check file name, empty
#'   when the file is absent or unreadable.
#' @details The file is a scheduling hint and nothing else: a stale or missing
#'   entry costs a little wall clock and cannot change a result, so a parse
#'   failure is swallowed rather than stopping the suite.
read_runtimes <- function() {
  if (!file.exists(RUNTIME_PATH)) return(stats::setNames(numeric(0), character(0)))
  recorded <- tryCatch(
    utils::read.csv(RUNTIME_PATH, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (is.null(recorded) || !all(c("check", "seconds") %in% names(recorded))) {
    return(stats::setNames(numeric(0), character(0)))
  }
  stats::setNames(as.numeric(recorded$seconds), recorded$check)
}

#' Write the observed runtimes to the tracked file
#'
#' @param results A list of `run_check()` results.
#' @return Invisible NULL; called for its effect on RUNTIME_PATH.
#' @details Merges over what is already recorded rather than replacing it, so
#'   that refreshing from a `--fast` run leaves the slow check's measurement in
#'   place.
write_runtimes <- function(results) {
  observed <- stats::setNames(
    round(vapply(results, function(r) r$seconds, numeric(1))),
    vapply(results, function(r) r$check, character(1))
  )
  merged <- read_runtimes()
  merged[names(observed)] <- observed
  merged <- merged[order(names(merged))]
  utils::write.csv(
    data.frame(check = names(merged), seconds = as.integer(merged)),
    RUNTIME_PATH, row.names = FALSE, quote = FALSE
  )
  say(sprintf("Recorded %d runtime(s) in %s", length(merged), RUNTIME_PATH))
  invisible(NULL)
}

#' Order checks longest first, on the recorded runtimes
#'
#' @param checks Character vector of check file names.
#' @param runtimes Named numeric vector of recorded seconds.
#' @return The same names, ordered by descending recorded cost, ties broken
#'   alphabetically so the order is deterministic.
order_by_cost <- function(checks, runtimes) {
  cost <- unname(runtimes[checks])
  cost[is.na(cost)] <- DEFAULT_COST_SECS
  checks[order(-cost, checks)]
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
#' @param cores Cores the check may fork into, passed as MC_CORES, or NA to
#'   leave the child at its own default.
#' @return A list with elements `check`, `ok` (logical), `status` (the exit
#'   status), `seconds` and `note` (a reason string, empty when the check
#'   passed).
#' @details Output is captured to `log_dir` rather than inherited, so that the
#'   suite's own PASS/FAIL lines stay readable; a failing check's tail is
#'   printed by the caller.
run_check <- function(check, log_dir, tree_check, cores = NA_integer_) {
  path     <- file.path(CHECK_DIR, check)
  log_path <- file.path(log_dir, sub("[.]R$", ".log", check))
  before   <- if (tree_check && check %in% MUTATING_CHECKS) tree_state() else NA_character_
  child    <- if (is.na(cores)) character(0) else sprintf("MC_CORES=%d", cores)
  started  <- Sys.time()
  status   <- system2("Rscript", shQuote(path), stdout = log_path, stderr = log_path,
                      env = child)
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

#' Print one check's result line, and a failing check's log tail
#'
#' @param result One `run_check()` result.
#' @return Invisible NULL; called for its output.
report_result <- function(result) {
  say(sprintf("  [%s] %-42s %8s%s",
              if (result$ok) "PASS" else "FAIL", result$check,
              format_runtime(result$seconds),
              if (nzchar(result$note)) paste0("  ", result$note) else ""))
  if (!result$ok) print_log_tail(result$log)
  invisible(NULL)
}

#' Run checks one after another
#'
#' @param checks Character vector of check file names.
#' @param opts Parsed options, for `log_dir` and `tree_check`.
#' @param cores Cores each check may fork into, or NA for the child default.
#' @return A list of `run_check()` results, in the order given.
run_checks_serial <- function(checks, opts, cores) {
  lapply(checks, function(check) {
    result <- run_check(check, opts$log_dir, opts$tree_check, cores)
    report_result(result)
    result
  })
}

#' Run checks concurrently, at most `jobs` of them at once
#'
#' @param checks Character vector of check file names, longest expected first.
#' @param opts Parsed options, for `log_dir` and `tree_check`.
#' @param jobs Number of checks to have in flight at once.
#' @param cores Cores each check may fork into, or NA for the child default.
#' @return A list of `run_check()` results, in the order the checks were given.
#' @details mc.preschedule = FALSE forks one process per check rather than
#'   dividing the list into `jobs` batches up front, which is what lets a short
#'   check follow a short check while a long one is still running. A worker
#'   that dies outright returns no result, so its check is reported as failed
#'   with the reason rather than dropped from the count.
run_checks_parallel <- function(checks, opts, jobs, cores) {
  results <- parallel::mclapply(checks, function(check) {
    result <- run_check(check, opts$log_dir, opts$tree_check, cores)
    report_result(result)
    result
  }, mc.cores = jobs, mc.preschedule = FALSE)
  lapply(seq_along(results), function(i) {
    result <- results[[i]]
    if (is.list(result) && !is.null(result$ok)) return(result)
    list(check = checks[[i]], ok = FALSE, status = NA_integer_, seconds = 0,
         note = "the worker running this check did not return a result",
         log = file.path(opts$log_dir, sub("[.]R$", ".log", checks[[i]])))
  })
}

#' Print the closing summary table and return the suite's exit status
#'
#' @param results A list of `run_check()` results.
#' @param wall Elapsed time in seconds for the whole suite.
#' @return 0L when every check passed, 1L otherwise.
#' @details Reports the elapsed time and, where the two differ because checks
#'   ran concurrently, the summed check time beside it, so a reader can see
#'   what the concurrency bought.
summarise_results <- function(results, wall) {
  failed <- Filter(function(r) !r$ok, results)
  total  <- sum(vapply(results, function(r) r$seconds, numeric(1)))
  spent  <- if (round(total) > round(wall)) {
    sprintf("%s (%s of check time)", format_runtime(wall), format_runtime(total))
  } else {
    format_runtime(wall)
  }
  say("")
  say(sprintf("%d of %d checks passed in %s",
              length(results) - length(failed), length(results), spent))
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
jobs  <- resolve_jobs(args$jobs)
cores <- child_cores(jobs)
say(sprintf("Running %d %s check(s)%s; output in %s",
            length(checks), args$selection,
            if (jobs > 1L) sprintf(" %d at a time", jobs) else "",
            args$log_dir))
say("")

# The document-regenerating checks are recognised by a repository-wide git
# comparison, which cannot say which of two concurrent writers touched a file,
# so they are taken first and on their own. Everything else is dispatched
# longest first: the suite cannot finish before its longest check does.
started  <- Sys.time()
mutating <- intersect(checks, MUTATING_CHECKS)
pooled   <- order_by_cost(setdiff(checks, mutating), read_runtimes())
results  <- run_checks_serial(mutating, args, cores)
if (jobs > 1L) {
  results <- c(results, run_checks_parallel(pooled, args, jobs, cores))
} else {
  results <- c(results, run_checks_serial(pooled, args, cores))
}
wall <- as.numeric(difftime(Sys.time(), started, units = "secs"))

status <- summarise_results(results, wall)
if (args$refresh_runtimes) write_runtimes(results)
quit(status = status)
