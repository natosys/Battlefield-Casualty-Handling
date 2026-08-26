#!/usr/bin/env Rscript
##############################################################################
## scripts/check_analysis_idempotence.R                                     ##
## Regression check — the analysis pipeline is idempotent and RNG-neutral   ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_analysis_idempotence.R              # default 30 days
#   Rscript scripts/check_analysis_idempotence.R --days 40    # longer run
#   Rscript scripts/check_analysis_idempotence.R --reps 3     # multi-run arm
#   Rscript scripts/check_analysis_idempotence.R --seed 42    # other seed
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step.
#
# Why this check exists: the analysis pipeline is a report over monitoring
# data the simulation has already produced, so analysing one run's output
# twice has to give one answer. It did not. assign_role4_los() draws each
# evacuated casualty's Role 4 length of stay from a triangular distribution
# at analysis time, and the pooled mass casualty timeline jitters its points
# at render time; neither reset the stream, so a second call continued it and
# returned a different Role 4 census from identical monitoring data. The
# published figures were reproducible only because run.R happens to call
# set.seed(), run_once() and analyse_run() in a fixed order, which is a
# property of the calling sequence rather than of the seed (Issue #233).
#
# What it asserts:
#
#   1. analyse_run() is idempotent: two consecutive calls on one monitoring
#      list return the same Role 4 census and write the same CSV outputs.
#
#   2. analyse_replications() is idempotent on the same terms, including the
#      jittered mass casualty timeline image.
#
#   3. The pipeline is RNG-neutral: .Random.seed is left exactly as the
#      caller had it. This is the mechanism the two properties above rest on,
#      and asserting it directly is what catches a newly added draw that
#      happens not to reach a compared output yet.
#
# The run length is set by what the check has to exercise rather than by the
# model: the length-of-stay draw is made only for a casualty who has actually
# boarded a sortie, and sorties are scheduled every seven days, so a run too
# short to fly one would pass over an empty census and assert nothing. A
# scheduled sortie can also be cancelled, and at seed 42 the first two are, so
# the run is the baseline's own 30 days rather than the shorter run the
# pipeline alone would need. The check confirms rather than assumes this, by
# failing when either arm's census comes back empty.

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

args <- commandArgs(trailingOnly = TRUE)

#' Read a flag's value from the command line
#'
#' @param flag Flag name, including its leading dashes
#' @param default Value returned when the flag is absent or has no argument
#' @return The argument following `flag`, or `default`
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

SCENARIO   <- arg_value("--scenario", "default")
CHECK_DAYS <- as.integer(arg_value("--days", 30L))
CHECK_REPS <- as.integer(arg_value("--reps", 2L))
CHECK_SEED <- as.integer(arg_value("--seed", 42L))

# Failures accumulate in an environment rather than through `<<-`, so that
# fail() writes to a named place instead of reaching into the enclosing scope.
state <- new.env(parent = emptyenv())
state$failures <- character(0)

#' Record a failure, deferring the non-zero exit to the end of the run
#'
#' @param ... sprintf() format string and its arguments
#' @return Invisibly, the accumulated failure vector
fail <- function(...) state$failures <- c(state$failures, sprintf(...))

#' Print one PASS/FAIL line
#'
#' @param ok TRUE when the assertion held
#' @param fmt sprintf() format string describing the assertion
#' @param ... Arguments to `fmt`
#' @return Invisibly, NULL
report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}

json     <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
# Assigned at file scope, which is the global environment under Rscript, so
# the four globals the execution model requires reach the sourced modules
# without `<<-`.
env_data <- build_environment(resolve_scenario(json, SCENARIO))
day_min  <- DAY_MIN
counts   <- sapply(env_data$elms, length)

#' Reduce a directory of analysis outputs to something comparable
#'
#' @param dir Directory an analysis call wrote to
#' @return Named character vector, one MD5 digest per file, named by the
#'   file's path relative to `dir`
#'
#' @details Covers the images as well as the CSV and markdown outputs, so the
#'   jittered mass casualty timeline is compared too. The directory is the
#'   whole of what a caller sees besides the returned list, so digesting all
#'   of it needs no judgement about which outputs a future draw might reach.
output_digests <- function(dir) {
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  files <- sort(files)
  setNames(vapply(files, function(f) tools::md5sum(f)[[1]], character(1)),
           substring(files, nchar(dir) + 2L))
}

#' Name the outputs that differ between two digest sets
#'
#' @param a Digest vector from the first call
#' @param b Digest vector from the second call
#' @return Character vector of differing or one-sided file names
differing_outputs <- function(a, b) {
  all_names <- union(names(a), names(b))
  all_names[vapply(all_names,
                   function(n) !identical(unname(a[n]), unname(b[n])),
                   logical(1))]
}

#' Analyse one monitoring list into a fresh directory
#'
#' @param mon Monitoring list (arrivals, attributes, resources)
#' @param multi TRUE to exercise analyse_replications(), FALSE for analyse_run()
#' @return Named list: `role4` (the Role 4 daily census, or NULL) and
#'   `digests` (see output_digests())
#'
#' @details Each call gets its own output directory so the two calls under
#'   comparison cannot overwrite one another's files. Both functions print a
#'   summary to stdout that is not wanted here.
analyse_into_tempdir <- function(mon, multi) {
  out_dir <- tempfile("bch_idempotence_")
  img_dir <- file.path(out_dir, "images")
  dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
  invisible(capture.output(
    results <- if (multi) {
      analyse_replications(mon, warm_up_period = 0, output_dir = out_dir,
                           images_dir = img_dir)
    } else {
      analyse_run(mon, output_dir = out_dir, warm_up_days = 0,
                  images_dir = img_dir)
    }
  ))
  list(role4 = results$role4_census_daily, digests = output_digests(out_dir))
}

cat(sprintf("Analysis idempotence check: %s, %d-day runs, seed %d\n\n",
            SCENARIO, CHECK_DAYS, CHECK_SEED))

# ── 1. analyse_run() is idempotent ──────────────────────────────────────────
#
# One monitoring list, analysed twice. Anything that differs between the two
# results came from the analysis pipeline rather than from the simulation,
# since the input is the same object in both calls.

cat("-- analyse_run() is idempotent --\n")

set.seed(CHECK_SEED)
invisible(capture.output(wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)))
single_mon <- list(
  arrivals   = get_mon_arrivals(list(wrapped), ongoing = TRUE),
  attributes = get_mon_attributes(list(wrapped)),
  resources  = get_mon_resources(list(wrapped))
)

first  <- analyse_into_tempdir(single_mon, multi = FALSE)
second <- analyse_into_tempdir(single_mon, multi = FALSE)

role4_censused <- !is.null(first$role4) && nrow(first$role4) > 0
if (!role4_censused) {
  fail(paste0("no casualty reached Role 4 in a %d-day run at seed %d, so the length-of-stay ",
              "draw this check is about was never made. Lengthen the run (--days) or choose ",
              "a seed that evacuates"),
       CHECK_DAYS, CHECK_SEED)
}
report(role4_censused, "the run censuses Role 4, so the length-of-stay draw is exercised")

role4_identical <- identical(first$role4, second$role4)
if (!role4_identical) {
  fail(paste0("analyse_run() returned a different Role 4 census on the second call over ",
              "identical monitoring data. The pipeline is advancing the RNG stream, so the ",
              "second call draws each casualty's length of stay from where the first left ",
              "off rather than repeating it"))
}
report(role4_identical, "two calls return an identical Role 4 daily census (%d rows)",
       if (is.null(first$role4)) 0L else nrow(first$role4))

single_diff <- differing_outputs(first$digests, second$digests)
if (length(single_diff)) {
  fail("analyse_run() wrote %d differing output(s) on the second call: %s",
       length(single_diff), paste(single_diff, collapse = ", "))
}
report(length(single_diff) == 0,
       "two calls write byte-identical outputs (%d files compared)",
       length(first$digests))

# ── 2. analyse_replications() is idempotent ─────────────────────────────────
#
# The multi-run pipeline has the second consumer: the pooled mass casualty
# timeline jitters its points at render time, which the image digests cover.

cat("\n-- analyse_replications() is idempotent --\n")

set.seed(CHECK_SEED)
invisible(capture.output(multi_mon <- run_replications(CHECK_REPS, CHECK_DAYS)))

first_mr  <- analyse_into_tempdir(multi_mon, multi = TRUE)
second_mr <- analyse_into_tempdir(multi_mon, multi = TRUE)

role4_mr_censused <- !is.null(first_mr$role4) && nrow(first_mr$role4) > 0
if (!role4_mr_censused) {
  fail(paste0("no casualty reached Role 4 across %d replications of %d days, so the multi-run ",
              "arm compared two empty censuses and asserted nothing"),
       CHECK_REPS, CHECK_DAYS)
}
report(role4_mr_censused, "the replications census Role 4, so the draw is exercised across them")

role4_mr_identical <- identical(first_mr$role4, second_mr$role4)
if (!role4_mr_identical) {
  fail(paste0("analyse_replications() returned a different Role 4 census on the second call ",
              "over identical monitoring data"))
}
report(role4_mr_identical,
       "two calls return an identical Role 4 daily census across %d replications",
       CHECK_REPS)

multi_diff <- differing_outputs(first_mr$digests, second_mr$digests)
if (length(multi_diff)) {
  fail(paste0("analyse_replications() wrote %d differing output(s) on the second call: %s. ",
              "A differing .png with identical .csv companions is the render-time jitter ",
              "rather than the length-of-stay draw"),
       length(multi_diff), paste(multi_diff, collapse = ", "))
}
report(length(multi_diff) == 0,
       "two calls write byte-identical outputs (%d files compared)",
       length(first_mr$digests))

# ── 3. The pipeline leaves the caller's RNG stream where it found it ────────
#
# Idempotence follows from this, and this is the property a newly added draw
# breaks first — before it reaches any output the comparisons above cover.

cat("\n-- the pipeline consumes no net randomness --\n")

set.seed(CHECK_SEED)
seed_before <- get(".Random.seed", envir = globalenv())
invisible(analyse_into_tempdir(single_mon, multi = FALSE))
seed_after_single <- get(".Random.seed", envir = globalenv())

single_neutral <- identical(seed_before, seed_after_single)
if (!single_neutral) {
  fail(paste0("analyse_run() left .Random.seed advanced, so a draw inside it is not ",
              "wrapped in with_preserved_rng(). Every later draw in the session shifts ",
              "with the number of casualties analysed"))
}
report(single_neutral, "analyse_run() leaves .Random.seed unchanged")

set.seed(CHECK_SEED)
seed_before_mr <- get(".Random.seed", envir = globalenv())
invisible(analyse_into_tempdir(multi_mon, multi = TRUE))
seed_after_mr <- get(".Random.seed", envir = globalenv())

multi_neutral <- identical(seed_before_mr, seed_after_mr)
if (!multi_neutral) {
  fail("analyse_replications() left .Random.seed advanced")
}
report(multi_neutral, "analyse_replications() leaves .Random.seed unchanged")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All analysis idempotence checks passed.\n")
quit(status = 0)
