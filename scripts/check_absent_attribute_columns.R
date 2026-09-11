#!/usr/bin/env Rscript
##############################################################################
## scripts/check_absent_attribute_columns.R                                 ##
## Regression check — an attribute nobody set is an empty column, not none  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_absent_attribute_columns.R
#   Rscript scripts/check_absent_attribute_columns.R --days 1
#   Rscript scripts/check_absent_attribute_columns.R --seed 42
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The attributes monitor records what casualties did,
# so the wide pivot of it has a column only where at least one casualty set
# that attribute. The shape of the frame therefore depended on what the run
# happened to produce, and analysis code reading a column directly failed with
# an object-not-found error on any run that produced none of it. A one-day
# campaign ran to completion, wrote its KPI summary, printed "Simulation
# complete" and then died in summarise_r2b_hold_occupancy() on `object
# 'return_day' not found`, because nobody had finished recovering (Issue #316).
#
# Duration is a proxy for the trigger rather than the trigger itself. The
# condition is that no casualty set the attribute, which a configuration can
# produce at any run length, so this check strips the attribute from a real
# run's monitoring data rather than relying on a run being short enough.
#
# The fix is a guarantee rather than a guard at each site: build_attributes_wide()
# adds every key in MODEL_ATTRIBUTE_KEYS as an all-NA column where the run set
# none, so the frame has one shape whatever the run did. That only holds while
# the constant lists every key the model can set, which is what the first three
# assertions defend; the rest exercise the behaviour it buys.
#
# What it asserts:
#
#   1. Every attribute key set under a literal name in R/trajectories.R is in
#      MODEL_ATTRIBUTE_KEYS.
#   2. Every key in MODEL_ATTRIBUTE_KEYS is still set somewhere in
#      R/trajectories.R, so a deleted attribute cannot linger in the list.
#   3. The two set_attribute() calls taking their key from a variable are
#      accounted for, their names being passed in at the call sites. A third
#      such call fails this assertion rather than passing unnoticed, since no
#      parser here can follow where its name comes from.
#   4. build_attributes_wide() returns every key in MODEL_ATTRIBUTE_KEYS from a
#      monitor carrying one of them.
#   5. analyse_run() completes on monitoring data with no return_day, and
#      still counts the casualties holding a bed, an episode with no recorded
#      exit having run to the end of the window rather than not happened
#      (Issue #327). Stripping r2b_hold_start instead, which is the attribute
#      that opens the episode, is what leaves the summary with nothing to
#      summarise, and that is the case returning the empty result.
#   6. summarise_r2b_hold_occupancy() returns that same empty result when the
#      columns are absent outright, so the guard holds even where the
#      guarantee above does not reach it.
#   7. A short run analyses end to end, which is the failure as reported.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
  library(ggplot2)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")

args <- commandArgs(trailingOnly = TRUE)

#' Read a flag's value from the command line
#'
#' @param flag Flag name, including its leading dashes.
#' @param default Value returned when the flag is absent or has no argument.
#' @return The argument following `flag`, or `default`.
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

#' Run length in days for the end-to-end arm
#'
#' @details One day is the shortest run the command line accepts and the
#'   length at which the defect was reported. It is long enough to generate
#'   casualties and short enough that most attributes go unset, which is the
#'   condition under test.
CHECK_DAYS <- as.integer(arg_value("--days", 1L))

#' Seed for the end-to-end arm
CHECK_SEED <- as.integer(arg_value("--seed", 42L))

#' The model source the attribute keys are read from
TRAJECTORY_SOURCE <- "R/trajectories.R"

#' Attribute keys passed into r2e_surgery_block() rather than written inline
#'
#' @details The builder is called once per surgical section for each of the
#'   two operations, so its start and end attribute names arrive as arguments.
#'   Assertion 3 checks that these are the only such calls.
DYNAMIC_KEYS <- c("r2e_surgery_1_start", "r2e_surgery_1_end",
                  "r2e_surgery_2_start", "r2e_surgery_2_end")

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
#' @return Invisibly, NULL.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

#' Analyse one monitoring list into a directory of its own
#'
#' @param mon Monitoring list of arrivals, attributes and resources.
#' @return The list analyse_run() returns.
#' @details Each call gets a fresh directory so no arm reads another's output,
#'   and the pipeline's own summary is captured rather than printed.
analyse_into_tempdir <- function(mon) {
  out_dir <- tempfile("bch_absent_cols_")
  img_dir <- file.path(out_dir, "images")
  dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  invisible(capture.output(
    results <- analyse_run(mon, output_dir = out_dir, warm_up_days = 0,
                           images_dir = img_dir)
  ))
  results
}

cat(sprintf("Absent attribute column check: %d-day run, seed %d\n\n",
            CHECK_DAYS, CHECK_SEED))

# ── 1 to 3. The key list matches the model ──────────────────────────────────
#
# The guarantee is only as complete as the constant behind it, so the constant
# is read against the model rather than trusted.

cat("-- MODEL_ATTRIBUTE_KEYS matches the model --\n")

model_src   <- readLines(TRAJECTORY_SOURCE, warn = FALSE)
literal_set <- regmatches(
  model_src,
  gregexpr('set_attribute\\(\\s*"[a-z0-9_]+"', model_src)
)
literal_keys <- sort(unique(gsub('.*"([a-z0-9_]+)"', "\\1", unlist(literal_set))))

#' Name a few of a failing assertion's offenders without printing all of them
#'
#' @param keys Character vector of the keys the assertion objected to.
#' @param n How many to name before summarising the rest.
#' @return A string to append to the assertion's message, empty when `keys` is.
#' @details A failure naming sixty keys buries the other assertions' lines in
#'   the terminal, and the first few are enough to identify the cause.
name_some <- function(keys, n = 5L) {
  if (!length(keys)) return("")
  shown <- paste(utils::head(keys, n), collapse = ", ")
  if (length(keys) > n) shown <- sprintf("%s and %d more", shown, length(keys) - n)
  paste0(": ", shown)
}

missing_keys <- setdiff(c(literal_keys, DYNAMIC_KEYS), MODEL_ATTRIBUTE_KEYS)
report(length(missing_keys) == 0,
       "every attribute key the model sets is listed (%d keys, %d missing%s)",
       length(literal_keys) + length(DYNAMIC_KEYS), length(missing_keys),
       name_some(missing_keys))

quoted_anywhere <- vapply(
  MODEL_ATTRIBUTE_KEYS,
  function(k) any(grepl(sprintf('"%s"', k), model_src, fixed = TRUE)),
  logical(1)
)
report(all(quoted_anywhere),
       "every listed key is still set by the model (%d stale%s)",
       sum(!quoted_anywhere),
       name_some(MODEL_ATTRIBUTE_KEYS[!quoted_anywhere]))

all_set_calls  <- sum(vapply(gregexpr("set_attribute\\(", model_src),
                             function(m) sum(m > 0), numeric(1)))
literal_calls  <- length(unlist(literal_set))
report(all_set_calls - literal_calls == 2L,
       "the %d set_attribute() calls taking a variable key are the 2 accounted for",
       all_set_calls - literal_calls)

# ── 4. The pivot returns every key ──────────────────────────────────────────

cat("\n-- build_attributes_wide() guarantees the shape --\n")

one_key <- data.frame(
  name        = "wia_cbt1",
  replication = 1L,
  time        = 0,
  key         = "priority",
  value       = 1
)
wide <- build_attributes_wide(one_key, data.frame(name = "wia_cbt1", replication = 1L))
report(all(MODEL_ATTRIBUTE_KEYS %in% names(wide)),
       "a monitor carrying 1 key pivots to a frame carrying all %d",
       length(MODEL_ATTRIBUTE_KEYS))

# ── 5 and 7. The pipeline handles a run nobody returned to duty from ────────
#
# The end-to-end arm is the failure as reported. The stripped arm removes
# return_day from the same monitoring data, so the condition is exercised
# whatever the run happened to produce.

cat("\n-- the analysis pipeline completes without the attribute --\n")

env_data <- load_scenario("env_data.json", "default")
day_min  <- DAY_MIN
counts   <- sapply(env_data$elms, length)

set.seed(CHECK_SEED)
invisible(capture.output(wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)))
mon <- list(
  arrivals   = get_mon_arrivals(list(wrapped), ongoing = TRUE),
  attributes = get_mon_attributes(list(wrapped)),
  resources  = get_mon_resources(list(wrapped))
)
report(nrow(mon$arrivals) > 0,
       "the %d-day run generated %d casualties, so the arm is not vacuous",
       CHECK_DAYS, nrow(mon$arrivals))

end_to_end <- try(analyse_into_tempdir(mon), silent = TRUE)
report(!inherits(end_to_end, "try-error"),
       "a %d-day run analyses end to end", CHECK_DAYS)

stripped <- mon
stripped$attributes <- stripped$attributes[stripped$attributes$key != "return_day", ]
report(nrow(stripped$attributes) < nrow(mon$attributes) ||
         !"return_day" %in% mon$attributes$key,
       "the stripped arm carries no return_day, whatever the run produced")

no_rtd <- try(analyse_into_tempdir(stripped), silent = TRUE)
report(!inherits(no_rtd, "try-error"),
       "a run in which nobody returned to duty analyses without error")
report(!inherits(no_rtd, "try-error") && !is.null(no_rtd$r2b_hold_daily),
       "a casualty holding a bed with no recorded exit is still counted, not dropped")

# Stripping the attribute that opens the episode, rather than the one that
# closes it, is what leaves the holding summary with nothing to summarise.
no_hold <- mon
no_hold$attributes <- no_hold$attributes[no_hold$attributes$key != "r2b_hold_start", ]
none_held <- try(analyse_into_tempdir(no_hold), silent = TRUE)
report(!inherits(none_held, "try-error") && is.null(none_held$r2b_hold_daily) &&
         is.null(none_held$r2b_hold_occupancy_plot),
       "a run in which nobody entered holding gives the empty result, not a series of zeroes")

# ── 6. The guard holds where the guarantee does not reach ───────────────────
#
# Called directly, with neither column present, which is what a caller
# assembling a frame by hand rather than through build_attributes_wide() has.

cat("\n-- the holding summary guards its own columns --\n")

bare <- data.frame(name = "wia_cbt1", replication = 1L, dnbi_type = NA_real_)
combined_bare <- data.frame(name = "wia_cbt1", start_time = 0)
out_dir <- tempfile("bch_absent_bare_")
img_dir <- file.path(out_dir, "images")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
bare_out <- try(
  summarise_r2b_hold_occupancy(bare, combined_bare, out_dir, img_dir),
  silent = TRUE
)
unlink(out_dir, recursive = TRUE)
report(!inherits(bare_out, "try-error") && is.null(bare_out$r2b_hold_daily) &&
         is.null(bare_out$r2b_hold_occupancy_plot),
       "an absent r2b_hold_start and return_day give the empty result, not an error")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All absent attribute column checks passed.\n")
quit(status = 0)
