#!/usr/bin/env Rscript
##############################################################################
## scripts/check_analysis_decomposition.R                                   ##
## Regression check — every analysis stage binds what it returns            ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_analysis_decomposition.R
#
# Exits 0 when every check passes, 1 otherwise. It parses `R/analysis.R` and
# runs no simulation, so it finishes in about a second.
#
# Why this check exists. The analysis pipeline is a sequence of stages, each a
# function that takes the frames it reads and returns the values the stages
# after it consume. A stage that returns a name bound only inside a conditional
# fails whenever that conditional does not fire, and the failure is invisible
# until a run happens to take that path: an analysis at four replications
# exercises a branch that an analysis at one does not, and a configuration with
# no mass casualty events exercises neither.
#
# This is not hypothetical. Decomposing `analyse_run()` produced exactly this
# fault three times over. Two stages returned a value computed only where more
# than one replication contributed, and one returned a frame local to the guard
# that built it; all three reproduced byte for byte at four replications and
# only one of them failed at one. An artifact comparison cannot find this class
# of fault, because it can only compare the paths it happens to take. Reading
# the binding structure finds all of them at once.
#
# What it asserts, for every function in `R/analysis.R` whose body ends in a
# `list(...)` of bare names, which is the shape every decomposed stage has:
#
#   1. Each returned name is bound on every path through the function. A name
#      qualifies when it is a parameter, when it is assigned at the function's
#      own top level, or when it is assigned in both arms of an `if`/`else`.
#      A name assigned only inside an unguarded `if`, and not also taken as a
#      parameter, does not qualify: that is the fault above.
#
#   2. Each returned name is either read or reassigned by the caller. A stage
#      returning a value nothing consumes is a signature that has drifted from
#      what the pipeline needs, which is the other direction the same edit can
#      go wrong in.
#
# Both are properties of the source rather than of any particular run, which is
# what lets one check cover every path at once.

SOURCE  <- file.path("R", "analysis.R")
ENTRIES <- c("analyse_run", "analyse_replications")

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

#' Names bound on every path through a sequence of expressions
#'
#' @param body_list List of expressions, as `as.list(body(f))[-1]` gives.
#' @return Character vector of names bound on every path.
#' @details An `if` carrying an `else` binds on both paths, so both arms are
#'   descended into and only names bound in both are kept. An `if` without an
#'   `else`, and every other conditional construct, binds on some paths only
#'   and so contributes nothing.
bound_on_every_path <- function(body_list) {
  out <- character(0)
  for (e in body_list) {
    if (!is.call(e)) next
    op <- as.character(e[[1]])
    if (length(op) != 1) next
    if (op %in% c("<-", "=", "<<-") && is.name(e[[2]])) {
      out <- c(out, as.character(e[[2]]))
    } else if (op == "if" && length(e) == 4L) {
      #' Names bound on every path through one arm of an if
      #'
      #' @param x The arm's expression, a block or a single call.
      #' @return A character vector of the names it binds unconditionally.
      arm <- function(x) {
        if (is.call(x) && identical(as.character(x[[1]]), "{")) {
          bound_on_every_path(as.list(x)[-1])
        } else {
          bound_on_every_path(list(x))
        }
      }
      out <- c(out, intersect(arm(e[[3]]), arm(e[[4]])))
    }
  }
  unique(out)
}

#' The bare names a body's terminal `list(...)` returns
#'
#' @param body_list List of expressions, as `as.list(body(f))[-1]` gives.
#' @return Character vector of returned names, empty where the body does not
#'   end in a `list()` of bare names.
returned_names <- function(body_list) {
  if (!length(body_list)) return(character(0))
  last <- body_list[[length(body_list)]]
  if (!is.call(last) || !identical(as.character(last[[1]]), "list")) {
    return(character(0))
  }
  vals <- as.list(last)[-1]
  nms <- vapply(vals, function(v) if (is.name(v)) as.character(v) else NA_character_,
                character(1))
  nms[!is.na(nms)]
}

#' Every top-level function definition in a parsed file
#'
#' @param exprs Parsed expressions, as `parse()` returns.
#' @return A named list of functions, keyed by the name each is assigned to.
top_level_functions <- function(exprs) {
  out <- list()
  for (e in exprs) {
    if (!(is.call(e) && identical(as.character(e[[1]]), "<-"))) next
    if (!is.name(e[[2]])) next
    if (!is.call(e[[3]]) || !identical(as.character(e[[3]][[1]]), "function")) next
    out[[as.character(e[[2]])]] <- eval(e[[3]])
  }
  out
}

if (!file.exists(SOURCE)) {
  cat(sprintf("[FAIL] %s does not exist.\n", SOURCE))
  quit(status = 1)
}

exprs <- parse(SOURCE, keep.source = TRUE)
funcs <- top_level_functions(exprs)
source_text <- readLines(SOURCE, warn = FALSE)

# ── 1. Every returned name is bound on every path ───────────────────────────

stages <- 0L
unbound <- character(0)
for (nm in names(funcs)) {
  bl <- as.list(body(funcs[[nm]]))[-1]
  prov <- returned_names(bl)
  if (!length(prov)) next
  stages <- stages + 1L
  safe <- union(names(formals(funcs[[nm]])), bound_on_every_path(bl))
  missing <- setdiff(prov, safe)
  if (length(missing)) {
    unbound <- c(unbound, sprintf("%s returns %s", nm, paste(missing, collapse = ", ")))
  }
}

report(stages > 0, "%d stage function(s) return a list of names", stages)
report(length(unbound) == 0,
       "every returned name is bound on every path%s",
       if (length(unbound)) sprintf(" (offending: %s)", paste(unbound, collapse = "; ")) else "")

# ── 2. Every returned name is consumed by a caller ──────────────────────────

unread <- character(0)
for (nm in names(funcs)) {
  bl <- as.list(body(funcs[[nm]]))[-1]
  prov <- returned_names(bl)
  if (!length(prov)) next
  for (p in prov) {
    # The caller unpacks as `<name> <- <var>$<name>`; anything else reading it
    # counts too, so the search is for the name outside its own definition.
    pat <- sprintf("(^|[^[:alnum:]_.])%s([^[:alnum:]_.]|$)", p)
    hits <- grep(pat, source_text)
    if (length(hits) < 2) unread <- c(unread, sprintf("%s$%s", nm, p))
  }
}
report(length(unread) == 0,
       "every returned name is read somewhere%s",
       if (length(unread)) sprintf(" (offending: %s)", paste(unread, collapse = ", ")) else "")

# ── 3. The entry points are still present and still orchestrate ─────────────

for (entry in ENTRIES) {
  report(entry %in% names(funcs), "%s is a top-level function", entry)
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All analysis decomposition checks passed.\n")
quit(status = 0)
