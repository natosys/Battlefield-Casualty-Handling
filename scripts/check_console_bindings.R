#!/usr/bin/env Rscript
##############################################################################
## scripts/check_console_bindings.R                                         ##
## Regression check — no console panel reads another panel's local          ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_console_bindings.R
#
# Exits 0 when every check passes, 1 otherwise. It loads `app.R` into an
# environment of its own and inspects the functions it defines; it starts no
# server, opens no browser and runs no simulation, so it finishes in seconds.
#
# Why this check exists. The Shiny console's server function was decomposed
# into per-panel functions, and every panel now takes the values it reads as
# parameters. A panel that instead reads a name another panel binds locally
# still parses, still loads, and fails only when a user opens that panel: the
# output renders "Error: object '<name>' not found" where its content should
# be. Nothing in the R test suite sees it, because `shiny::testServer()`
# exercises the reactive graph rather than the rendered markup.
#
# This is not hypothetical. Splitting the Quick Run utilisation tab in two left
# the second half reading `ph`, the per-panel height list the first half
# computes. Every R test passed. The browser suite caught it, on one tab of
# ten, and only because that tab happens to be exercised; a panel with no
# browser coverage would have shipped broken.
#
# What it asserts:
#
#   1. No function defined by `app.R` has a free variable that some other
#      function defined by `app.R` binds as a local. That is the signature of
#      a value that used to be in scope and no longer is. A name bound at file
#      scope is fine, and so is a data-mask column: neither is another
#      function's local.
#
#   2. Every panel function is reachable, in the sense of being called
#      somewhere in `app.R` rather than left behind by an extraction.
#
# The check reasons about bindings rather than about behaviour, so it covers
# every panel at once, including those no browser test opens.

SOURCE <- "app.R"

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

#' Names a function binds locally, at any depth inside its body
#'
#' @param f A function.
#' @return Character vector of locally bound names.
#' @details Includes names bound inside nested blocks and closures, because the
#'   question this check asks is whether a name belongs to some other function
#'   at all, not at what depth it sits.
local_names <- function(f) {
  out <- character(0)
  #' Collect the names one expression binds, recursing into it
  #'
  #' @param x The expression to walk.
  #' @return Invisible NULL; called for its side effect on the name list.
  rec <- function(x) {
    if (!is.call(x)) return(invisible(NULL))
    op <- tryCatch(as.character(x[[1]]), error = function(e) "")
    if (length(op) == 1 && op %in% c("<-", "=", "<<-") && is.name(x[[2]])) {
      out <<- c(out, as.character(x[[2]]))
    }
    lapply(as.list(x), rec)
    invisible(NULL)
  }
  rec(body(f))
  unique(setdiff(out, names(formals(f))))
}

if (!file.exists(SOURCE)) {
  cat(sprintf("[FAIL] %s does not exist.\n", SOURCE))
  quit(status = 1)
}

if (!requireNamespace("codetools", quietly = TRUE)) {
  cat("[FAIL] codetools is not installed; free variables cannot be found.\n")
  quit(status = 1)
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(jsonlite)
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(DT)
  library(future)
  library(promises)
})

env <- new.env(parent = globalenv())
suppressMessages(sys.source(SOURCE, envir = env, keep.source = TRUE))

objs  <- ls(env, all.names = TRUE)
fnames <- objs[vapply(objs, function(n) is.function(get(n, envir = env)), logical(1))]
report(length(fnames) > 0, "%s defines %d function(s)", SOURCE, length(fnames))

locals_of <- lapply(setNames(fnames, fnames), function(n) local_names(get(n, envir = env)))

# ── 1. No function reads another function's local ───────────────────────────

leaks <- character(0)
for (n in fnames) {
  f <- get(n, envir = env)
  free <- tryCatch(codetools::findGlobals(f, merge = FALSE)$variables,
                   error = function(e) character(0))
  free <- setdiff(free, objs)
  for (v in free) {
    owners <- names(Filter(function(l) v %in% l, locals_of[setdiff(fnames, n)]))
    if (length(owners)) {
      leaks <- c(leaks, sprintf("%s reads `%s`, a local of %s",
                                n, v, paste(owners, collapse = "/")))
    }
  }
}
report(length(leaks) == 0, "no function reads another function's local%s",
       if (length(leaks)) sprintf(" (%s)", paste(leaks, collapse = "; ")) else "")

# ── 2. Every panel function is called somewhere ─────────────────────────────

src <- paste(readLines(SOURCE, warn = FALSE), collapse = "\n")
panels <- grep("^(wire|build|create|register|analyse_tab|start)_", fnames, value = TRUE)
orphans <- Filter(function(n) {
  length(gregexpr(sprintf("(?<![\\w.])%s\\(", n), src, perl = TRUE)[[1]]) < 2 &&
    gregexpr(sprintf("(?<![\\w.])%s\\(", n), src, perl = TRUE)[[1]][1] == -1
}, panels)
report(length(orphans) == 0, "every panel function is called%s",
       if (length(orphans)) sprintf(" (orphaned: %s)", paste(orphans, collapse = ", ")) else "")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All console binding checks passed.\n")
quit(status = 0)
