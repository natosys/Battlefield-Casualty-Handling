#!/usr/bin/env Rscript
##############################################################################
## scripts/check_testthat.R                                                 ##
## Regression check — the Shiny console's unit and testServer suites        ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_testthat.R
#
# Exits 0 when every test in `tests/testthat` passes, 1 otherwise. It runs no
# simulation: the console is loaded, its helpers are exercised directly, and
# its server function is driven through `shiny::testServer()`, which advances
# the reactive graph in process without a browser or an HTTP server. Most of
# its two minutes is spent loading the console once; the assertions themselves
# take seconds.
#
# Why this check exists. The console's server function is the largest single
# function in the repository, and it is being decomposed into per-tab modules.
# The reactive state machine inside it (the run, screening and sweep states,
# the configuration reactives derived from the loaded file, and the validation
# that stops a run before it starts) is the part such a refactor is most
# likely to break, and until now nothing asserted any of it. A test that only
# exists on the branch that does the refactor proves nothing; this suite is
# committed first so that it passes before the decomposition and is required
# to still pass after it.
#
# The split between this check and the Playwright suite is deliberate.
# `testServer` covers reactive state and needs no browser, so it runs in the
# pinned container as part of the per-pull-request gate. Rendered markup is
# covered by `tests/playwright`, which needs a running app and a Node
# toolchain and therefore runs as its own continuous integration job; see
# `docs/Continuous_Integration.md`.
#
# What it asserts: whatever `tests/testthat` asserts. The check is a runner,
# not a place to add assertions, so a new test goes in that directory and is
# picked up here automatically.

TEST_DIR <- file.path("tests", "testthat")

failures <- character(0)

#' Record a failure for the summary at the foot of the run
#'
#' @param ... `sprintf()` format string and its arguments.
#' @return Invisibly NULL; called for the side effect on `failures`.
fail <- function(...) {
  failures <<- c(failures, sprintf(...))
  invisible(NULL)
}

#' Report one assertion's outcome and record it if it failed
#'
#' @param ok TRUE when the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Arguments for `fmt`.
#' @return Invisibly NULL; called for its printed line and side effect.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  cat("[FAIL] testthat is not installed; the console test suite cannot run.\n")
  cat("       Restore the project library with renv::restore().\n")
  quit(status = 1)
}

if (!dir.exists(TEST_DIR)) {
  cat(sprintf("[FAIL] %s does not exist; there is no test suite to run.\n", TEST_DIR))
  quit(status = 1)
}

# ── Run the suite ───────────────────────────────────────────────────────────

# stop_on_failure = FALSE so the whole suite reports rather than aborting at
# the first failing file, which is what the accumulate-then-summarise
# convention every other check in this directory follows asks for.
results <- testthat::test_dir(TEST_DIR, stop_on_failure = FALSE, reporter = "summary")
summary <- as.data.frame(results)

n_failed  <- sum(summary$failed)
n_errored <- sum(summary$error)
n_passed  <- sum(summary$passed)

cat("\n")
report(n_failed == 0, "no test failed (%d assertion(s) passed)", n_passed)
report(n_errored == 0, "no test file errored")
report(n_passed > 0, "the suite contains at least one assertion")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All Shiny console test suite checks passed.\n")
quit(status = 0)
