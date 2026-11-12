#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_checkpoint_fingerprint.R                              ##
## Regression check: a checkpoint resumes only what produced it             ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_checkpoint_fingerprint.R
#
# Exits 0 when every check passes, 1 otherwise. Base R only and instantaneous:
# it runs no simulation.
#
# Why this check exists: every experiment runner saves its finished
# replications under outputs/checkpoints/ and reads them back on the next run,
# and a saved replication is named by its index, seed and horizon alone. A
# sweep re-run against another `--config`, or after a change to the model,
# therefore resumed replications measured under the earlier configuration or
# code, and wrote a manifest naming the new configuration beside them.
# `checkpoint_dir()` (R/cli.R) now puts every checkpoint under a fingerprint of
# the configuration's content, the sources under R/, the entry point and the
# package lockfile, so a run resumes only a measurement of the same thing.
# tests/checks/check_replication_checkpoint.R asserts that a resumed run writes
# what an uninterrupted one writes; this asserts that a resume cannot cross a
# change.
#
# What it asserts. The fingerprint is the same for the same bytes under another
# file name and differs for a changed configuration or a changed source; the
# checkpoint directory follows the fingerprint, a baseline refresh and an
# ordinary run keep separate directories, and an arm nests under its
# experiment's fingerprinted directory; and every checkpoint call in an entry
# point passes the configuration the run resolved.

source("R/cli.R")

#' Experiment name the directory assertions create and remove
PROBE_EXPERIMENT <- "check_checkpoint_fingerprint_probe"

state <- new.env(parent = emptyenv())
state$failures <- character(0)

#' Record a failure
#'
#' @param ... sprintf() format string and its arguments
#' @return Invisible NULL; called for its side effect on the failure list
fail <- function(...) {
  state$failures <- c(state$failures, sprintf(...))
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
  if (!ok) fail(fmt, ...)
  invisible(NULL)
}

work_dir <- file.path(tempdir(), "checkpoint_fingerprint")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

same_bytes <- file.path(work_dir, "renamed.json")
invisible(file.copy(SHIPPED_CONFIG, same_bytes, overwrite = TRUE))
changed <- file.path(work_dir, "changed.json")
writeLines(c(readLines(SHIPPED_CONFIG, warn = FALSE), ""), changed)

# ── 1. The fingerprint ──────────────────────────────────────────────────────

cat("\n-- checkpoint_fingerprint --\n")

sources <- checkpoint_sources(entry = character(0))
shipped_fp <- checkpoint_fingerprint(SHIPPED_CONFIG, sources)

report(grepl("^[0-9a-f]{12}$", shipped_fp), "the fingerprint is twelve hex characters")
report(any(startsWith(sources, "R/")) && "R/cli.R" %in% sources,
       "the sources include the modules under R/")
report(identical(checkpoint_fingerprint(same_bytes, sources), shipped_fp),
       "the same bytes under another name give the same fingerprint")
report(!identical(checkpoint_fingerprint(changed, sources), shipped_fp),
       "a changed configuration gives another fingerprint")

source_copy <- file.path(work_dir, basename(sources[1L]))
writeLines(c(readLines(sources[1L], warn = FALSE), "# changed"), source_copy)
report(!identical(checkpoint_fingerprint(SHIPPED_CONFIG, c(source_copy, sources[-1L])),
                  shipped_fp),
       "a changed source gives another fingerprint")
report(!identical(checkpoint_fingerprint(SHIPPED_CONFIG, c(sources, "renv.lock")),
                  checkpoint_fingerprint(SHIPPED_CONFIG, sources)) ||
         !file.exists("renv.lock"),
       "an added source gives another fingerprint")
report(!is.na(tryCatch({
  checkpoint_fingerprint(file.path(work_dir, "absent.json"), sources)
  NA_character_
}, error = function(e) conditionMessage(e))),
"an absent configuration is refused")

# ── 2. The checkpoint directories ───────────────────────────────────────────

cat("\n-- checkpoint_dir and arm_checkpoint_path --\n")

probe_root <- file.path("outputs", "checkpoints", c("run", "baseline"), PROBE_EXPERIMENT)
on.exit(unlink(probe_root, recursive = TRUE), add = TRUE)

run_shipped <- checkpoint_dir(PROBE_EXPERIMENT, FALSE, SHIPPED_CONFIG)
report(identical(run_shipped, checkpoint_dir(PROBE_EXPERIMENT, FALSE, same_bytes)),
       "the same bytes resume from the same directory")
report(!identical(run_shipped, checkpoint_dir(PROBE_EXPERIMENT, FALSE, changed)),
       "a changed configuration writes to another directory")
report(!identical(run_shipped, checkpoint_dir(PROBE_EXPERIMENT, TRUE, SHIPPED_CONFIG)),
       "a baseline refresh and an ordinary run keep separate directories")
report(dir.exists(run_shipped), "the directory is created")
arm <- arm_checkpoint_path(PROBE_EXPERIMENT, "arm_a", FALSE, SHIPPED_CONFIG)
report(identical(arm, file.path(run_shipped, "arm_a")) && dir.exists(arm),
       "an arm nests under its experiment's fingerprinted directory")
unlink(probe_root, recursive = TRUE)

# ── 3. Every entry point passes its configuration ───────────────────────────

cat("\n-- entry points --\n")

for (script in sort(Sys.glob("scripts/run_*.R"))) {
  lines <- readLines(script, warn = FALSE)
  code <- lines[!grepl("^\\s*#", lines)]
  calls <- grep("(^|[^_])(checkpoint_dir|arm_checkpoint_path)\\(", code, value = TRUE)
  calls <- calls[!grepl("checkpoint_dir = [a-z_]+[,)]?\\s*$", calls)]
  if (length(calls) == 0L) next
  # A call wrapped across two lines carries its configuration on the second.
  idx <- match(calls, code)
  joined <- paste(code[idx], code[pmin(idx + 1L, length(code))])
  report(all(grepl("config_path)", joined, fixed = TRUE)),
         "%s passes config_path to every checkpoint call", script)
}

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) failed.\n", length(state$failures)))
  quit(status = 1)
}
cat("\nAll checkpoint fingerprint checks passed.\n")
quit(status = 0)
