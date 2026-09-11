#!/usr/bin/env Rscript
##############################################################################
## scripts/check_ci_apt_guards.R                                            ##
## Regression check — every CI system-library install carries the same      ##
## stall guards                                                             ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_ci_apt_guards.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The workflow's `Install system libraries` step runs
# apt-get inside the pinned container, and it is duplicated verbatim in four
# jobs. With no retry configuration, no transport timeout and no per-step
# timeout, a stalled Ubuntu archive mirror hung the step until the job timeout
# fired, 60 minutes for three of these jobs and 180 for the slow suite, and
# reported a failure naming nothing useful. One run was observed wedged past
# 25 minutes and had to be cancelled by hand, against a 7m39s worst case that
# did complete (Issue #337).
#
# The guards are applied inline in all four copies rather than factored into a
# composite action, because `timeout-minutes` is not among the step properties
# a composite action supports, so the per-step timeout could not live inside
# one. That leaves four copies which must not drift, and drift is silent: three
# jobs hardened and one missed looks exactly like four hardened until the
# unhardened one wedges. This check is what makes the duplication safe.
#
# What it asserts:
#
#   1. Every job that installs system libraries does so with a step of the
#      expected name, and there are as many such steps as jobs needing them.
#   2. Every one of those steps carries a per-step `timeout-minutes`.
#   3. Every one sets apt's retry count and both transport timeouts before it
#      runs `apt-get update`, so the guards cover the update as well as the
#      install.
#   4. The four step bodies are byte-identical, so a change to one is a change
#      to all or a failure here.

#' The workflow file the guards live in
WORKFLOW <- ".github/workflows/checks.yml"

#' Name of the workflow step the guards belong to
STEP_NAME <- "Install system libraries"

#' Number of jobs expected to install system libraries
#'
#' @details fast-checks, baseline-reproduction, browser-checks and slow-checks.
#'   The classify job runs no R and installs nothing. Stated as a constant so
#'   that a fifth job added without the guards fails here rather than passing
#'   unnoticed.
EXPECTED_STEPS <- 4L

#' apt settings each step must apply before apt-get update
REQUIRED_SETTINGS <- c(
  "Acquire::Retries",
  "Acquire::http::Timeout",
  "Acquire::https::Timeout"
)

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

#' Split the workflow into its `Install system libraries` steps
#'
#' @param lines The workflow file, one element per line.
#' @return A list of character vectors, one per step, each running from the
#'   step's `- name:` line to the line before the next step at the same
#'   indentation.
#' @details A step ends at the next line beginning with the same `      - `
#'   prefix, which is how a YAML sequence entry at this depth opens, or at the
#'   end of the file. Parsed as text rather than as YAML so that the check
#'   needs no YAML parser in the R dependency set.
install_steps <- function(lines) {
  starts <- grep(sprintf("^      - name: %s$", STEP_NAME), lines)
  lapply(starts, function(start) {
    rest <- lines[seq(start + 1L, length(lines))]
    next_step <- grep("^      - ", rest)
    end <- if (length(next_step)) start + next_step[1] - 1L else length(lines)
    lines[seq(start, end)]
  })
}

lines <- readLines(WORKFLOW, warn = FALSE)
steps <- install_steps(lines)

cat("CI apt guard check\n\n")

# ── 1. Every job that installs has a step, and none is missing ─────────────

cat("-- the steps are all present --\n")

report(length(steps) == EXPECTED_STEPS,
       "the workflow carries %d '%s' steps, one per job that needs one (found %d)",
       EXPECTED_STEPS, STEP_NAME, length(steps))

# ── 2 and 3. Each step is bounded and each guard is set before the update ──

cat("\n-- each step is bounded and guarded --\n")

for (i in seq_along(steps)) {
  step <- steps[[i]]
  report(any(grepl("^        timeout-minutes: [0-9]+$", step)),
         "step %d carries a per-step timeout-minutes", i)

  update_at <- grep("apt-get update", step)
  if (length(update_at) == 0) {
    report(FALSE, "step %d runs apt-get update", i)
    next
  }
  before_update <- step[seq_len(update_at[1] - 1L)]
  missing <- REQUIRED_SETTINGS[!vapply(REQUIRED_SETTINGS,
                                       function(k) any(grepl(k, before_update, fixed = TRUE)),
                                       logical(1))]
  report(length(missing) == 0,
         "step %d sets %s before apt-get update%s",
         i, paste(REQUIRED_SETTINGS, collapse = ", "),
         if (length(missing)) sprintf("; absent: %s", paste(missing, collapse = ", ")) else "")
}

# ── 4. The copies have not drifted ─────────────────────────────────────────

cat("\n-- the copies have not drifted --\n")

#' One step's body, with the properties that legitimately differ removed
#'
#' @param step The step's lines.
#' @return The lines that must be identical across every copy.
#' @details The browser-checks copy carries an `if:` the others do not, that
#'   job running only when code changed, so the condition is dropped before
#'   the comparison. Everything else, the timeout included, must match.
comparable <- function(step) step[!grepl("^        if: ", step)]

bodies <- lapply(steps, comparable)
identical_to_first <- vapply(bodies, function(b) identical(b, bodies[[1]]), logical(1))
report(all(identical_to_first),
       "all %d step bodies are identical%s", length(bodies),
       if (all(identical_to_first)) "" else
         sprintf("; differing: %s", paste(which(!identical_to_first), collapse = ", ")))

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All CI apt guard checks passed.\n")
quit(status = 0)
