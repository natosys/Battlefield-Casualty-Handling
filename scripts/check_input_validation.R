#!/usr/bin/env Rscript
##############################################################################
## scripts/check_input_validation.R                                         ##
## Regression check — entry points reject malformed input by name           ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_input_validation.R
#
# Exits 0 when every check passes, 1 otherwise. Nothing here runs the model,
# so the check completes in seconds.
#
# Why this check exists: R/analysis.R is the project's largest module and
# consumed its monitoring data frames by assuming column presence throughout,
# so a malformed input surfaced as a subscript or dplyr error raised inside a
# pipeline the caller never wrote, several hundred lines from the entry point
# that accepted it (Issue #236). The same held of a hand-edited or uploaded
# env_data.json reaching the Shiny console. Neither is a correctness defect in
# the model, but a model meant for planners who are not its author has to say
# what it found wrong, and a diagnosis that names the missing column or the
# offending field is the difference between a five-minute fix and an
# afternoon in a debugger.
#
# What it asserts. Each validator rejects a representative malformed input
# and names the element at fault in the message it raises, and accepts what
# the model actually produces or ships: a monitoring list from a short live
# run, and the tracked env_data.json. Naming is asserted rather than assumed,
# since a validator that rejected everything with the same opaque message
# would satisfy an exit-status-only check while helping nobody.
#
# What it does not assert is that a configuration the validator accepts is
# operationally sensible. The checks here are structural: the blocks and
# fields the model indexes by are present and are of the type it indexes
# them with. Whether a rate or a probability is plausible is a modelling
# question, checked where that parameter is read.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")

JSON_PATH  <- "env_data.json"
CHECK_DAYS <- 2L

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

#' Assert that an expression is rejected with a message naming an element
#'
#' @param label Description of the assertion, for the PASS/FAIL line
#' @param expected Substring the raised message must contain, naming the
#'   field or column at fault
#' @param expr Expression expected to raise
#' @return Invisible NULL; reports and records a failure if the expression
#'   did not raise, or raised without naming `expected`
assert_rejects <- function(label, expected, expr) {
  msg <- tryCatch({
    force(expr)
    NA_character_
  }, error = function(e) conditionMessage(e))

  if (is.na(msg)) {
    fail("%s: accepted a malformed input that should have been rejected", label)
    report(FALSE, "%s", label)
    return(invisible(NULL))
  }
  ok <- grepl(expected, msg, fixed = TRUE)
  if (!ok) {
    fail("%s: rejected, but the message did not name '%s': %s", label, expected, msg)
  }
  report(ok, "%s", label)
  invisible(NULL)
}

#' Assert that an expression is accepted
#'
#' @param label Description of the assertion, for the PASS/FAIL line
#' @param expr Expression expected not to raise
#' @return Invisible NULL; reports and records a failure if it raised
assert_accepts <- function(label, expr) {
  msg <- tryCatch({
    force(expr)
    NA_character_
  }, error = function(e) conditionMessage(e))

  ok <- is.na(msg)
  if (!ok) fail("%s: rejected a well-formed input: %s", label, msg)
  report(ok, "%s", label)
  invisible(NULL)
}

cat("Entry-point input validation check\n\n")

# ── 1. Monitoring data (analyse_run, analyse_replications) ──────────────────

json     <- jsonlite::fromJSON(JSON_PATH, simplifyVector = FALSE)
env_data <<- build_environment(json)
day_min  <<- 1440L
counts   <<- sapply(env_data$elms, length)

invisible(capture.output(suppressWarnings(
  wrapped <- run_once(n_days = CHECK_DAYS, seed = 42L, write_files = FALSE)
)))
mon <- list(
  arrivals   = get_mon_arrivals(list(wrapped), ongoing = TRUE),
  attributes = get_mon_attributes(list(wrapped)),
  resources  = get_mon_resources(list(wrapped))
)

assert_accepts("a live run's monitoring data is accepted",
               validate_monitoring(mon, "analyse_run"))

assert_rejects("a missing monitoring element is named", "mon$attributes is missing",
               validate_monitoring(mon[c("arrivals", "resources")], "analyse_run"))

dropped <- mon
dropped$resources <- dropped$resources[, setdiff(names(dropped$resources), "queue"),
                                       drop = FALSE]
assert_rejects("a missing monitoring column is named", "missing column(s): queue",
               validate_monitoring(dropped, "analyse_run"))

emptied <- mon
emptied$arrivals <- emptied$arrivals[0, , drop = FALSE]
assert_rejects("an empty arrivals frame is rejected", "mon$arrivals is empty",
               validate_monitoring(emptied, "analyse_run"))

assert_rejects("a monitoring argument that is not a list is rejected", "named list",
               validate_monitoring(mon$arrivals, "analyse_run"))

assert_rejects("analyse_run() rejects malformed monitoring data at its entry point",
               "analyse_run: malformed monitoring data",
               analyse_run(mon[c("arrivals", "resources")], output_dir = tempdir()))

assert_rejects("analyse_replications() rejects malformed monitoring data at its entry point",
               "analyse_replications: malformed monitoring data",
               analyse_replications(mon[c("arrivals", "resources")], output_dir = tempdir()))

assert_rejects("a negative warm-up is named",
               "analyse_run: warm_up_days",
               analyse_run(mon, output_dir = tempdir(), warm_up_days = -1))

# ── 2. Sweep arguments ──────────────────────────────────────────────────────

assert_rejects("a zero replication count is named", "n_rep",
               plot_r2b_icu_share_frontier(shares = 0.5, n_days = 1L, n_rep = 0L,
                                           path = JSON_PATH, output_dir = tempdir(),
                                           images_dir = tempdir()))

assert_rejects("a share outside [0, 1] is named", "shares",
               plot_r2b_icu_share_frontier(shares = c(0.5, 1.5), n_days = 1L, n_rep = 1L,
                                           path = JSON_PATH, output_dir = tempdir(),
                                           images_dir = tempdir()))

# Aliased purely to keep the calls below inside the line length: the sweep's
# own name is 44 characters before its first argument.
sweep <- plot_transport_capacity_margin_by_fleet_size

assert_rejects("a configuration path that does not exist is named",
               "path does not exist",
               sweep(fleet_sizes = list(PMVAmb = 1L), n_days = 1L, n_rep = 1L,
                     path = "no_such_env_data.json", output_dir = tempdir(),
                     images_dir = tempdir()))

assert_rejects("a malformed fleet size is named", "fleet_sizes$PMVAmb",
               sweep(fleet_sizes = list(PMVAmb = c(1, 2.5)), n_days = 1L, n_rep = 1L,
                     path = JSON_PATH, output_dir = tempdir(),
                     images_dir = tempdir()))

assert_rejects("an unnamed fleet_sizes list is rejected", "non-empty named list",
               sweep(fleet_sizes = list(1L), n_days = 1L, n_rep = 1L,
                     path = JSON_PATH, output_dir = tempdir(),
                     images_dir = tempdir()))

# ── 3. Configuration files (the Shiny console's loading boundary) ───────────

assert_accepts("the tracked env_data.json is accepted",
               validate_env_data_json(json, JSON_PATH))

assert_rejects("a missing top-level block is named", "block 'transports' is missing",
               validate_env_data_json(json[setdiff(names(json), "transports")], "edited.json"))

bad_pop <- json
bad_pop$pops[[1]]$count <- "many"
assert_rejects("a non-numeric population count is named", "pops[[1]]$count",
               validate_env_data_json(bad_pop, "edited.json"))

bad_elm <- json
bad_elm$elms[[2]]$qty <- -1
assert_rejects("a negative element quantity is named", "elms[[2]]$qty",
               validate_env_data_json(bad_elm, "edited.json"))

bad_var <- json
bad_var$vars[[1]]$actys[[1]]$vals[[1]]$var <- NULL
assert_rejects("a variable with no name is named",
               "vars[[1]]$actys[[1]]$vals[[1]]$var is missing",
               validate_env_data_json(bad_var, "edited.json"))

assert_rejects("a configuration that is not a named list is rejected",
               "did not parse to a named list",
               validate_env_data_json("{}", "edited.json"))

# Every fault is reported, not just the first: a hand-edited file usually
# carries more than one, and a validator that stopped at the first would make
# fixing it an iteration rather than a single pass.
two_faults <- json
two_faults$pops[[1]]$count <- "many"
two_faults$elms[[1]]$qty   <- NULL
faults_msg <- tryCatch(validate_env_data_json(two_faults, "edited.json"),
                       error = function(e) conditionMessage(e))
both_named <- is.character(faults_msg) &&
  grepl("pops[[1]]$count", faults_msg, fixed = TRUE) &&
  grepl("elms[[1]]$qty",   faults_msg, fixed = TRUE)
if (!both_named) fail("validate_env_data_json: reported only one of two faults")
report(both_named, "every fault in a configuration is reported, not only the first")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All entry-point input validation checks passed.\n")
quit(status = 0)
