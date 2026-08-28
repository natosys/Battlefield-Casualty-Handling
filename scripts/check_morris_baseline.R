#!/usr/bin/env Rscript
##############################################
## scripts/check_morris_baseline.R          ##
## Screened-parameter baseline agreement    ##
##############################################
#
#   Rscript scripts/check_morris_baseline.R
#
# Asserts that the `mode` column of `morris_params` (R/sensitivity.R) describes
# the configuration the model actually ships: that every row's mode lies inside
# that row's own screening bounds, that applying the whole mode vector through
# apply_params() reproduces `env_data.json` unchanged, and that each screened
# parameter's mode equals the value at the path it writes to. Exits non-zero on
# failure.
#
# The check exists because the mode column is read on two paths that an
# ordinary run never touches: run_sobol() holds every parameter not selected
# into the decomposition at its mode, at every one of the N x (p + 2) design
# points, and the Shiny Sensitivity Calibration tab displays the column to a
# planner under the heading `Baseline`. A drifted mode is therefore invisible
# until it has already mis-anchored a variance decomposition. Three had drifted
# when the check was written (Issue #186).
#
# The parameter-to-path mapping is derived from apply_params() rather than
# restated here: each parameter is perturbed away from its mode in turn and the
# paths whose values move are the paths it writes. A second copy of the mapping
# would be one more thing able to drift from the code it describes, which is
# the class of defect this check exists to catch.

suppressPackageStartupMessages({
  source("R/environment.R")
  source("R/sensitivity.R")
})

env_data <<- load_elms("env_data.json")
day_min  <<- DAY_MIN

failures <- character(0)
#' Print one PASS or FAIL line, recording a failure
#'
#' @param ok Logical: whether the assertion held.
#' @param msg One-line description of the assertion.
#' @return Invisible NULL; called for its side effects.
check    <- function(ok, msg) {
  if (isTRUE(ok)) {
    message(sprintf("  PASS  %s", msg))
  } else {
    message(sprintf("  FAIL  %s", msg))
    failures <<- c(failures, msg)
  }
}

#' Every leaf of the vars tree as a flat named list, keyed "elm.acty.var"
#'
#' @param ed A built environment data list.
#' @return A named list of every leaf value, keyed by its dotted path.
flatten_vars <- function(ed) {
  out <- list()
  for (elm in names(ed$vars)) {
    for (acty in names(ed$vars[[elm]])) {
      vals <- ed$vars[[elm]][[acty]]
      for (v in names(vals)) {
        out[[paste(elm, acty, v, sep = ".")]] <- vals[[v]]
      }
    }
  }
  out
}

#' Whether two leaf values agree
#'
#' @param a First value.
#' @param b Second value.
#' @param tol Relative tolerance applied to a numeric comparison.
#' @return TRUE where the two agree.
#' @details Leaves are not all numeric, the airframe selector and the timing
#'   mode being strings and the scheduled event days a list, so the
#'   comparison falls back to identity for anything it cannot difference.
same <- function(a, b, tol = 1e-9) {
  if (is.numeric(a) && is.numeric(b) && length(a) == length(b)) {
    all(abs(a - b) <= tol * pmax(1, abs(b)))
  } else {
    identical(a, b)
  }
}

#' Read one key of a flattened tree, tolerating its absence
#'
#' @param x A flattened vars tree.
#' @param k Dotted path to read.
#' @return The value at that path, NULL where the tree does not carry it.
#' @details `[[` on an absent name is an error rather than a NULL, and the two
#'   trees compared below need not carry identical key sets: `apply_params()`
#'   creates a leaf `env_data.json` does not hold, which is worth reporting as
#'   a difference rather than dying on.
at <- function(x, k) if (k %in% names(x)) x[[k]] else NULL

#' Format a leaf value for a difference report
#'
#' @param x The value to format.
#' @return The value as a comma-separated string, "absent" where it is NULL.
fmt <- function(x) if (is.null(x)) "absent" else paste(format(x), collapse = ", ")

#' Paths at which two flattened trees disagree
#'
#' @param x First flattened tree.
#' @param y Second flattened tree.
#' @return A character vector of the dotted paths that differ, including one
#'   present in only one of the two.
changed_paths <- function(x, y) {
  keys <- union(names(x), names(y))
  keys[!vapply(keys, function(k) same(at(x, k), at(y, k)), logical(1))]
}

modes   <- setNames(morris_params$mode, morris_params$name)
shipped <- flatten_vars(env_data)

# ── 1. Every mode lies inside its own screening bounds ────────────────────────
#
# Restated here rather than left to the assertion R/sensitivity.R now makes at
# source time, so that the check reports the invariant as one of its own lines
# rather than appearing to pass a property it never looked at. In practice a
# violation stops the source() above first, which is the point of asserting it
# there: every caller of the screen fails, not only this check.

message("\nBounds containment")
inside <- morris_params$mode >= morris_params$lower &
          morris_params$mode <= morris_params$upper
check(all(inside),
      sprintf("all %d rows carry a mode within their own bounds (%d outside)",
              nrow(morris_params), sum(!inside)))
for (i in which(!inside)) {
  message(sprintf("        %s: mode %g, bounds %g to %g",
                  morris_params$name[i], morris_params$mode[i],
                  morris_params$lower[i], morris_params$upper[i]))
}

# ── 2. The mode vector reproduces the shipped configuration ───────────────────
#
# The strongest of the three assertions, and the one that matches what
# run_sobol() actually does: it evaluates every design point at the mode vector
# with the selected parameters overwritten, so if applying the whole vector
# leaves env_data.json unchanged, the held-fixed background is the shipped
# configuration whatever subset a run happens to select.

message("\nMode vector against env_data.json")
applied <- flatten_vars(apply_params(env_data, modes))
drift   <- changed_paths(shipped, applied)
check(length(drift) == 0,
      sprintf("applying every mode leaves all %d vars leaves untouched (%d changed)",
              length(shipped), length(drift)))
for (k in drift) {
  message(sprintf("        %s: env_data.json %s, mode vector writes %s",
                  k, fmt(at(shipped, k)), fmt(at(applied, k))))
}

# ── 3. Each parameter's mode equals the value at the path it writes ───────────
#
# Narrower than the check above but named per parameter, which is what a
# maintainer adding a row needs to see. A parameter writing more than one path
# is checked against all of them, so a mode shared by two paths that hold
# different shipped values is reported rather than passed on one of the two.

message("\nPer-parameter agreement")
unmapped <- character(0)
mismatch <- character(0)
for (nm in morris_params$name) {
  if (nm %in% names(MORRIS_MODE_CHECK_EXCLUSIONS)) next
  perturbed        <- modes
  perturbed[[nm]]  <- perturbed[[nm]] + 1
  written <- changed_paths(applied, flatten_vars(apply_params(env_data, perturbed)))
  if (length(written) == 0) {
    unmapped <- c(unmapped, nm)
    next
  }
  for (k in written) {
    if (!same(at(shipped, k), modes[[nm]])) {
      mismatch <- c(mismatch, sprintf("%s (mode %g) against %s (%s)",
                                      nm, modes[[nm]], k, fmt(at(shipped, k))))
    }
  }
}
check(length(mismatch) == 0,
      sprintf("every screened parameter's mode equals its env_data.json value (%d mismatched)",
              length(mismatch)))
for (m in mismatch) message("        ", m)

# A parameter apply_params() does not write reaches no design point at all, so
# it is screened in name only — the same class of silent defect, caught here
# because the mapping is derived rather than declared.
check(length(unmapped) == 0,
      sprintf("every screened parameter reaches a vars path through apply_params() (%d do not)",
              length(unmapped)))
for (u in unmapped) message("        ", u)

# ── 4. The exclusion list is exactly the parameters that need excluding ───────

message("\nExclusion list")
excluded <- names(MORRIS_MODE_CHECK_EXCLUSIONS)
check(all(excluded %in% morris_params$name),
      sprintf("all %d excluded names are screened parameters", length(excluded)))
for (nm in excluded) {
  message(sprintf("        %s: %s", nm, MORRIS_MODE_CHECK_EXCLUSIONS[[nm]]))
}

if (length(failures) > 0) {
  message(sprintf("\n%d check(s) FAILED:", length(failures)))
  for (f in failures) message("  - ", f)
  quit(status = 1L)
}
message("\nAll screened-parameter baseline checks passed.")
