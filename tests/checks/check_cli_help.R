#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_cli_help.R                                            ##
## Regression check: every entry point's --help shows its banner examples   ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_cli_help.R
#
# Exits 0 when every check passes, 1 otherwise. The source checks are
# instantaneous; the live check starts one entry point under --help.
#
# Why this check exists: every entry point carried its worked invocations in
# its banner, where a user reading `--help` never saw them, and the help text
# itself had no description and no examples. `cli_help()` (R/cli.R) now builds
# the usage line, description and examples from the banner, so the examples
# have one home. That holds only while every entry point builds its parser from
# it, names itself correctly, and keeps a banner the reader can parse; a new
# entry point written on the old pattern, or a banner whose examples lose their
# indented `Rscript` form, would silently drop back to a bare option list.
#
# What it asserts. For `run.R`, every `scripts/run_*.R` and
# `scripts/calibrate_dow.R`: the parser is built through `cli_help()` naming the
# file's own path; the banner yields a description and at least one example;
# and every example invokes that entry point. Live, that `--help` on one entry
# point exits 0 and prints its description, its examples and its option list.

source("R/cli.R")

#' Entry points whose --help is checked
ENTRY_POINTS <- c("run.R", sort(Sys.glob("scripts/run_*.R")), "scripts/calibrate_dow.R")

#' Entry point started live under --help, chosen for loading the fewest modules
LIVE_ENTRY_POINT <- "scripts/run_warmup.R"

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

# ── 1. Every entry point builds its parser from its banner ──────────────────

cat("\n-- parser construction and banner --\n")

for (script in ENTRY_POINTS) {
  src <- paste(readLines(script, warn = FALSE), collapse = "\n")
  report(grepl(sprintf("cli_help(\"%s\")", script), src, fixed = TRUE),
         "%s builds its parser through cli_help(\"%s\")", script, script)
  report(!grepl("OptionParser(option_list = option_list))", src, fixed = TRUE),
         "%s has no parser built without its help text", script)

  help <- cli_help(script)
  report(nzchar(help$description), "%s: the banner gives a description", script)
  examples <- banner_examples(readLines(script, warn = FALSE))
  commands <- grep("^ *Rscript ", examples, value = TRUE)
  report(length(commands) > 0L, "%s: the banner gives at least one example", script)
  report(all(startsWith(trimws(commands), sprintf("Rscript %s", script))),
         "%s: every example invokes %s", script, script)
}

# ── 2. --help, live ─────────────────────────────────────────────────────────

cat("\n-- --help, live --\n")

out <- suppressWarnings(system2("Rscript", c(LIVE_ENTRY_POINT, "--help"),
                                stdout = TRUE, stderr = FALSE))
status <- attr(out, "status")
live_help <- cli_help(LIVE_ENTRY_POINT)
first_example <- banner_examples(readLines(LIVE_ENTRY_POINT, warn = FALSE))[1L]

report(is.null(status) || identical(as.integer(status), 0L),
       "%s --help exits 0", LIVE_ENTRY_POINT)
report(any(out == sprintf("Usage: Rscript %s [options]", LIVE_ENTRY_POINT)),
       "--help prints the usage line")
report(any(out == live_help$description), "--help prints the banner description")
report(any(out == "Examples:") && any(out == first_example),
       "--help prints the banner examples")
report(any(grepl("--help", out, fixed = TRUE)) && any(grepl("--days", out, fixed = TRUE)),
       "--help prints the option list")

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) failed.\n", length(state$failures)))
  quit(status = 1)
}
cat("\nAll CLI help checks passed.\n")
quit(status = 0)
