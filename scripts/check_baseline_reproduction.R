#!/usr/bin/env Rscript
##############################################
## scripts/check_baseline_reproduction.R    ##
## Seed-42 tracked evidence set reproduction##
##############################################
#
#   Rscript scripts/check_baseline_reproduction.R
#
# Runs the model at the seed, duration and replication count the tracked
# baseline was produced at, then compares the run's console log and arrival
# diagnostics byte for byte against the tracked files under `logs/` and
# `data/`. Exits non-zero on any difference.
#
# This is the check that protects every published figure. `CLAUDE.md`'s Key
# Parameters table, `docs/Single_Run_Analysis.md` and the seed-42 rows of
# `docs/Multi_Run_Analysis.md` all derive from one run of one code state, and
# the provenance note that says so rests on the claim that the run reproduces.
# A change that shifts the random number stream, whether or not it changes the
# model, silently invalidates all three documents at once; nothing else in the
# suite would notice.
#
# The comparison is byte for byte rather than tolerance based deliberately. A
# reproduction that agrees to a tolerance is evidence that the model is close
# to what it was, not that the published run is still the run the code
# produces, and the tracked set is a record of a specific event stream.
#
# The run writes to a temporary directory rather than to `outputs/`, so the
# check leaves no artifact behind and cannot be confused with an ordinary run.
# It never passes `--refresh-baseline`: repairing a difference by overwriting
# the tracked set is a deliberate maintainer action, taken with the provenance
# note updated in the same commit, not something a check does on its way past.

BASELINE_SEED       <- 42L
BASELINE_DAYS       <- 30L
BASELINE_ITERATIONS <- 1L

# Tracked artifact to the name the run writes it under, relative to the run's
# output directory. images/ is excluded: a PNG carries a creation timestamp and
# a device fingerprint, so it does not compare byte for byte across runs.
TRACKED_FILES <- list(
  c("logs/logs.txt",                  "logs.txt"),
  c("data/arrivals_wia_cbt.txt",      "data/arrivals_wia_cbt.txt"),
  c("data/arrivals_wia_spt.txt",      "data/arrivals_wia_spt.txt"),
  c("data/arrivals_kia_cbt.txt",      "data/arrivals_kia_cbt.txt"),
  c("data/arrivals_kia_spt.txt",      "data/arrivals_kia_spt.txt"),
  c("data/arrivals_dnbi_cbt.txt",     "data/arrivals_dnbi_cbt.txt"),
  c("data/arrivals_dnbi_spt.txt",     "data/arrivals_dnbi_spt.txt"),
  c("data/arrivals_mass_casualty.txt", "data/arrivals_mass_casualty.txt"),
  c("data/mass_casualty_events.csv",  "data/mass_casualty_events.csv")
)

failures <- character(0)

#' Record a failure
#'
#' @param msg One-line description of the failed assertion.
#' @return Invisible NULL; called for its side effect.
fail <- function(msg) {
  failures <<- c(failures, msg)
  invisible(NULL)
}

#' Print one PASS or FAIL line, recording a failure
#'
#' @param ok Logical: whether the assertion held.
#' @param msg One-line description of the assertion.
#' @return Invisible `ok`.
report <- function(ok, msg) {
  if (isTRUE(ok)) {
    message(sprintf("  [PASS] %s", msg))
  } else {
    message(sprintf("  [FAIL] %s", msg))
    fail(msg)
  }
  invisible(ok)
}

#' Whether two files hold identical bytes
#'
#' @param a Path to the first file.
#' @param b Path to the second file.
#' @return TRUE when both exist and their bytes are identical.
identical_bytes <- function(a, b) {
  if (!file.exists(a) || !file.exists(b)) return(FALSE)
  identical(readBin(a, "raw", file.size(a)), readBin(b, "raw", file.size(b)))
}

#' The first line at which two text files differ
#'
#' @param a Path to the first file.
#' @param b Path to the second file.
#' @return A one-line description of the first difference, for the report.
first_difference <- function(a, b) {
  if (!file.exists(b)) return("the run produced no such file")
  la <- readLines(a, warn = FALSE)
  lb <- readLines(b, warn = FALSE)
  n  <- min(length(la), length(lb))
  differing <- which(la[seq_len(n)] != lb[seq_len(n)])
  if (length(differing) == 0L) {
    return(sprintf("identical for %d lines, then lengths differ (%d tracked, %d run)",
                   n, length(la), length(lb)))
  }
  sprintf("first differs at line %d: tracked %s, run %s",
          differing[[1L]], sQuote(la[differing[[1L]]]), sQuote(lb[differing[[1L]]]))
}

run_dir <- file.path(tempdir(), "baseline_reproduction")
unlink(run_dir, recursive = TRUE)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

message(sprintf("Running the model at seed %d for %d days, %d replication",
                BASELINE_SEED, BASELINE_DAYS, BASELINE_ITERATIONS))
message(sprintf("Output directory: %s\n", run_dir))

status <- system2("Rscript",
                  c("run.R",
                    "--seed", BASELINE_SEED,
                    "--days", BASELINE_DAYS,
                    "--iterations", BASELINE_ITERATIONS,
                    "--output-dir", shQuote(run_dir)),
                  stdout = file.path(run_dir, "console.txt"),
                  stderr = file.path(run_dir, "console.txt"))

message("Byte-for-byte comparison against the tracked evidence set")
report(identical(as.integer(status), 0L),
       sprintf("the run completed (exit status %s)", status))

if (identical(as.integer(status), 0L)) {
  for (pair in TRACKED_FILES) {
    tracked  <- pair[[1L]]
    produced <- file.path(run_dir, pair[[2L]])
    ok <- identical_bytes(tracked, produced)
    report(ok, sprintf("%-38s %s", tracked,
                       if (ok) "identical" else first_difference(tracked, produced)))
  }
}

if (length(failures) > 0L) {
  message(sprintf("\n%d check(s) FAILED:", length(failures)))
  for (f in failures) message("  - ", f)
  message("\nThe tracked seed-42 baseline no longer reproduces. Either the change ",
          "under test\nshifted the random number stream, in which case the ",
          "published figures in CLAUDE.md,\ndocs/Single_Run_Analysis.md and ",
          "docs/Multi_Run_Analysis.md need regenerating with\n",
          "`Rscript run.R --seed 42 --days 30 --iterations 1 --refresh-baseline`, ",
          "or it did not,\nin which case this is a defect.")
  quit(status = 1L)
}

message("\nThe tracked seed-42 evidence set reproduces byte for byte.")
quit(status = 0L)
