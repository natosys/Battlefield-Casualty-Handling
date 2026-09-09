##############################################
## R/cli.R                                  ##
## Command-line argument validation         ##
##############################################
#
# Dependency-free (base R only) so a regression check can source this file
# alone and exercise every rule without loading simmer, without running the
# model, and without triggering an entry point's own CLI block.
#
# Why this lives in its own file. Command-line arguments are input from
# outside the program exactly as env_data.json is, and the style guide (E1,
# E2) requires them validated before use, failing with a message that names
# the field and the value found. Before Issue #310 they were not: `--days 0`
# was accepted, a full simulation attempted, and the failure raised inside
# the analysis pipeline naming a monitoring data frame the user never
# referred to. Keeping the rules here rather than inside an entry point's
# `if (!interactive())` block is what lets them be tested in milliseconds
# instead of by spawning a process per case.

# ── Value validators ─────────────────────────────────────────────────────────

#' Reject a switch whose value is not a single number within a range
#'
#' @param value Value parsed for the switch.
#' @param switch_name Switch as the user typed it, used in the message.
#' @param min_value Smallest permitted value.
#' @param max_value Largest permitted value (default Inf).
#' @return Invisibly `value`, having passed; stops otherwise.
#'
#' @details Length and finiteness are checked alongside the range so that NA,
#'   NULL and a vector of values are rejected by the same rule rather than
#'   reaching a comparison that would return NA and be treated as FALSE.
require_number_in_range <- function(value, switch_name, min_value, max_value = Inf) {
  if (length(value) != 1L || !is.numeric(value) || !is.finite(value) ||
        value < min_value || value > max_value) {
    stop(sprintf("%s must be a single number between %s and %s, and was %s",
                 switch_name, format(min_value), format(max_value),
                 paste(format(value), collapse = ", ")), call. = FALSE)
  }
  invisible(value)
}

#' Reject a directory switch that names nothing
#'
#' @param value Value parsed for the switch, or NULL where it is optional.
#' @param switch_name Switch as the user typed it, used in the message.
#' @return Invisibly `value`, having passed; stops otherwise.
#'
#' @details NULL passes, an optional directory switch being absent rather
#'   than empty; an empty or multi-element string does not.
require_directory <- function(value, switch_name) {
  if (is.null(value)) return(invisible(value))
  if (length(value) != 1L || !is.character(value) || !nzchar(value)) {
    stop(sprintf("%s must name a directory, and was empty", switch_name),
         call. = FALSE)
  }
  invisible(value)
}

# ── Rules spanning more than one switch ──────────────────────────────────────

#' Validate a warm-up period against the run length it is taken from
#'
#' @param warm_up Warm-up days to exclude from the analysis window.
#' @param days Run length in days.
#' @return Invisibly `warm_up`, having passed; stops otherwise.
#'
#' @details A warm-up at or beyond the run length leaves nothing to analyse,
#'   which surfaces downstream as summary statistics taken over no rows rather
#'   than as a rejected argument.
validate_warm_up <- function(warm_up, days) {
  require_number_in_range(warm_up, "--warm-up", 0)
  if (warm_up >= days) {
    stop(sprintf(paste("--warm-up must be less than the run length, and was %s",
                       "against %s days: a warm-up at or beyond the run length",
                       "leaves the analysis window empty"),
                 format(warm_up), format(days)), call. = FALSE)
  }
  invisible(warm_up)
}

#' Resolve the execution mode, checking it against the replication count
#'
#' @param mode "single", "multi", or NULL to infer from `iterations`.
#' @param iterations Replication count.
#' @return The resolved mode, "single" or "multi"; stops on a contradiction.
#'
#' @details The two modes produce different artifact sets: only single-run
#'   mode writes logs.txt and the arrival diagnostics, those being records of
#'   one event stream. Inferring the mode when it is omitted is what keeps
#'   every invocation documented before this switch existed behaving as it did.
resolve_run_mode <- function(mode, iterations) {
  implied <- if (iterations == 1L) "single" else "multi"
  if (is.null(mode)) return(implied)

  if (length(mode) != 1L || !mode %in% c("single", "multi")) {
    stop(sprintf("--mode must be 'single' or 'multi', and was %s",
                 sQuote(paste(mode, collapse = ", "))), call. = FALSE)
  }
  if (mode != implied) {
    stop(sprintf(paste("--mode %s contradicts --iterations %s, which implies %s:",
                       "pass one or the other, or make them agree"),
                 mode, format(iterations), implied), call. = FALSE)
  }
  mode
}

#' Validate a baseline refresh against the run it would be written from
#'
#' @param refresh_baseline TRUE where the tracked evidence set is to be written.
#' @param iterations Replication count.
#' @param scenario Name of the scenario profile in force.
#' @return Invisibly TRUE, having passed; stops otherwise.
#'
#' @details Two conditions protect the tracked set, and each names its own
#'   reason rather than surfacing as arithmetic about the iteration count. A
#'   multi-run refresh could only ever write part of the set (Issue #154); a
#'   refresh under a profile would leave the set describing a configuration
#'   the repository does not ship (Issue #310).
validate_baseline_refresh <- function(refresh_baseline, iterations, scenario) {
  if (!isTRUE(refresh_baseline)) return(invisible(TRUE))

  if (iterations != 1L) {
    stop(sprintf(paste("--refresh-baseline requires single-run mode, and --iterations",
                       "was %s: logs.txt and the arrival diagnostics are records of one",
                       "run's event stream and have no multi-replication equivalent, so",
                       "a multi-run refresh could only write part of the set"),
                 format(iterations)), call. = FALSE)
  }
  if (!identical(scenario, "default")) {
    stop(sprintf(paste("--refresh-baseline requires --scenario default, and was %s:",
                       "the tracked baseline evidence set describes the shipped",
                       "configuration, so a profile must not write it"),
                 sQuote(scenario)), call. = FALSE)
  }
  invisible(TRUE)
}
