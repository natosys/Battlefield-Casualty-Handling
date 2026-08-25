#!/usr/bin/env Rscript
##############################################################################
## scripts/check_config_restore.R                                           ##
## Regression check — a failed sweep or screen restores the configuration   ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_config_restore.R            # all checks
#   Rscript scripts/check_config_restore.R --days 2   # shorter live run
#
# Exits 0 when every check passes, 1 otherwise. The stubbed checks are
# instantaneous; the one live check runs a single two-day replication.
#
# Why this check exists: the model's configuration lives in three globals,
# env_data, day_min and counts, because run_once(), build_env() and the
# trajectory closures resolve them from the global environment rather than
# receiving them as arguments. Every entry point that varies the
# configuration -- the two capacity sweeps, the Morris and Sobol screens and
# the scenario runner -- therefore mutates those globals and is responsible
# for putting them back. Four of the five restored them by an assignment at
# the foot of the function, which the error path never reaches, and the fifth
# never restored them at all (Issue #236). The consequence was silent and
# serious: a sweep that failed partway through left the session on the swept
# configuration, so a baseline run made next in the same session was computed
# against the wrong parameters and reported as the baseline.
#
# What it asserts. Each entry point is called with its expensive interior
# stubbed out by a function that mutates the configuration and then raises an
# error, which is the failure mode at issue, and the globals are compared
# against the values they held before the call. The scenario runner is also
# checked on its success path, where the restore is a change of behaviour
# rather than a repair, and in a session that never bound the globals at all,
# where restoring means removing them again rather than leaving the scenario's
# configuration behind.
#
# What it does not assert is that a failed sweep's partial results are
# meaningful; they are not, and nothing here makes them so. The check is only
# that the failure does not contaminate what runs next.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
  library(sensitivity)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/sensitivity.R")
source("R/scenario_runner.R")

args <- commandArgs(trailingOnly = TRUE)

#' Read a flag's value from the command line
#'
#' @param flag Flag name, e.g. "--days"
#' @param default Value returned when the flag is absent or has no argument
#' @return The argument following the flag, or `default`
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

LIVE_DAYS <- as.integer(arg_value("--days", 2L))
JSON_PATH <- "env_data.json"

failures <- character(0)

#' Record a failure
#'
#' @param ... sprintf() format string and its arguments
#' @return Invisible NULL; called for its side effect on `failures`
fail <- function(...) {
  failures <<- c(failures, sprintf(...))
  invisible(NULL)
}

#' Print one PASS/FAIL line
#'
#' @param ok TRUE for a passing assertion
#' @param fmt sprintf() format string describing the assertion
#' @param ... Arguments to `fmt`
#' @return Invisible NULL; called for its output
report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
  invisible(NULL)
}

#' Capture the configuration globals in a form two calls can be compared on
#'
#' @return Named list, one element per configuration global, each the value
#'   held or the string "<unbound>" for a name not currently bound
#'
#' @details Deliberately independent of capture_config_globals(), the function
#'   under test: a check that measured the state through the mechanism it is
#'   checking would pass whatever that mechanism did.
config_state <- function() {
  setNames(lapply(CONFIG_GLOBALS, function(nm) {
    if (exists(nm, envir = globalenv(), inherits = FALSE)) {
      get(nm, envir = globalenv(), inherits = FALSE)
    } else {
      "<unbound>"
    }
  }), CONFIG_GLOBALS)
}

#' Evaluate an expression with a global function temporarily replaced
#'
#' @param name Name of the global function to replace
#' @param replacement Function to bind to `name` for the duration
#' @param expr Expression to evaluate
#' @return The value of `expr`
#'
#' @details The entry points under test resolve their interior calls from the
#'   global environment, so rebinding the name there is enough to stand a stub
#'   in front of a multi-hour call. The original binding is restored on exit
#'   whether or not `expr` raises.
with_stub <- function(name, replacement, expr) {
  original <- get(name, envir = globalenv())
  assign(name, replacement, envir = globalenv())
  on.exit(assign(name, original, envir = globalenv()), add = TRUE)
  expr
}

# A stub standing in for a long-running interior call: it clobbers the
# configuration exactly as the real call's caller does, then fails. Any entry
# point that restores only at the foot of its body leaves the sentinel behind.
SENTINEL <- "clobbered-by-stub"

#' Clobber the configuration globals and raise an error
#'
#' @param ... Ignored; present so the stub can stand in for any interior call
#' @return Never returns; always raises
clobber_and_fail <- function(...) {
  env_data <<- SENTINEL
  day_min  <<- -1L
  counts   <<- SENTINEL
  stop("injected failure")
}

#' Bind the configuration globals to the reference configuration
#'
#' @return Invisible NULL; called for its effect on the global environment
#'
#' @details Called before each assertion so that every one of them starts
#'   from the same known configuration. Without it an assertion would compare
#'   against whatever the previous one left behind, and a chain of entry
#'   points that all fail to restore would agree with each other and pass.
set_reference_config <- function() {
  env_data <<- REFERENCE_ENV_DATA
  day_min  <<- DAY_MIN
  counts   <<- REFERENCE_COUNTS
  invisible(NULL)
}

#' Assert that an expression fails and leaves the configuration untouched
#'
#' @param label Description of the entry point under test
#' @param expr Expression expected to raise
#' @return Invisible NULL; reports and records a failure if either the error
#'   or the restoration did not happen
assert_restores_on_error <- function(label, expr) {
  set_reference_config()
  before <- config_state()
  raised <- tryCatch({
    force(expr)
    FALSE
  }, error = function(e) TRUE)
  after <- config_state()

  if (!raised) {
    fail("%s: the stubbed interior did not raise, so the error path was not exercised", label)
    report(FALSE, "%s restores the configuration on the error path", label)
    return(invisible(NULL))
  }
  ok <- identical(before, after)
  if (!ok) fail("%s: configuration globals differ after a failed call", label)
  report(ok, "%s restores the configuration on the error path", label)
  invisible(NULL)
}

json <- jsonlite::fromJSON(JSON_PATH, simplifyVector = FALSE)
REFERENCE_ENV_DATA <- build_environment(json)
REFERENCE_COUNTS   <- sapply(REFERENCE_ENV_DATA$elms, length)
set_reference_config()

cat("Configuration save/restore check\n\n")

# ── 1. The two capacity sweeps ──────────────────────────────────────────────

with_stub("run_replications", clobber_and_fail, {
  assert_restores_on_error("plot_transport_capacity_margin_by_fleet_size", {
    plot_transport_capacity_margin_by_fleet_size(
      fleet_sizes = list(PMVAmb = 1L), n_days = 1L, n_rep = 1L,
      path = JSON_PATH, output_dir = tempdir(), images_dir = tempdir()
    )
  })

  assert_restores_on_error("plot_r2b_icu_share_frontier", {
    plot_r2b_icu_share_frontier(
      shares = 0.5, n_days = 1L, n_rep = 1L,
      path = JSON_PATH, output_dir = tempdir(), images_dir = tempdir()
    )
  })

  assert_restores_on_error("run_scenario", {
    run_scenario("default", n_iterations = 1L, n_days = 1L, path = JSON_PATH)
  })
})

# ── 2. The two screens ──────────────────────────────────────────────────────
#
# A screen swallows a failed design point by design, converting it to a row of
# NAs so one bad point does not discard a multi-hour run. The failure that
# does propagate is the one raised outside that tryCatch, when the completed
# point is written to the resumption cache, so that is the call stubbed here.

#' Stand in for a design point evaluation without running the model
#'
#' @param params_row Ignored; the design point's parameter values
#' @param ... Ignored; the remaining eval_params() arguments
#' @return Named numeric vector, one element per row of `morris_kpis`
#'
#' @details Mutates the configuration exactly as the real eval_params() does,
#'   so the state the entry point has to restore is the state a real screen
#'   would leave behind.
stub_eval_params <- function(params_row, ...) {
  env_data <<- SENTINEL
  setNames(rep(1, nrow(morris_kpis)), morris_kpis$name)
}

with_stub("eval_params", stub_eval_params, {
  with_stub("cache_append", function(...) stop("injected failure"), {
    assert_restores_on_error("run_morris", {
      run_morris(n_days = 1L, n_rep = 1L, r = 1L, levels = 4L,
                 output_dir = tempdir(), images_dir = tempdir(),
                 cache_dir = tempfile("morris_cache_"))
    })

    assert_restores_on_error("run_sobol", {
      run_sobol(morris_params$name[1], n_days = 1L, n_rep = 1L, n_sobol = 2L,
                output_dir = tempdir(), dirichlet = FALSE,
                cache_dir = tempfile("sobol_cache_"), nboot = 10L)
    })
  })
})

# ── 3. run_scenario() on its success path ───────────────────────────────────

set_reference_config()
before <- config_state()
invisible(capture.output(suppressWarnings(
  run_scenario("high_intensity", n_iterations = 1L, n_days = LIVE_DAYS, path = JSON_PATH)
)))
after <- config_state()
ok <- identical(before, after)
if (!ok) fail("run_scenario: configuration globals differ after a successful call")
report(ok, "run_scenario restores the configuration on the success path")

# ── 4. A session that never bound the globals ───────────────────────────────
#
# scripts/run_scenarios.R reaches run_scenario() without having set the
# globals itself, so restoring has to remove them again rather than leave the
# scenario's configuration behind for whatever the session does next.

saved <- capture_config_globals()
rm(list = intersect(CONFIG_GLOBALS, ls(envir = globalenv())), envir = globalenv())

with_stub("run_replications", clobber_and_fail, {
  invisible(tryCatch(
    run_scenario("default", n_iterations = 1L, n_days = 1L, path = JSON_PATH),
    error = function(e) NULL
  ))
})

still_unbound <- !any(vapply(CONFIG_GLOBALS, exists, logical(1),
                             envir = globalenv(), inherits = FALSE))
if (!still_unbound) {
  fail("run_scenario: globals unbound before the call were left bound after it")
}
report(still_unbound, "run_scenario leaves globals unbound that were unbound before the call")

restore_config_globals(saved)

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All configuration save/restore checks passed.\n")
quit(status = 0)
