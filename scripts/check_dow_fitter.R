#!/usr/bin/env Rscript
##############################################################################
## scripts/check_dow_fitter.R                                               ##
## Regression check — the died-of-wounds ceiling fitter recovers a known    ##
## ceiling, and the documented calibration table is the shipped one         ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_dow_fitter.R
#
# Exits 0 when every check passes, 1 otherwise. It needs neither simmer nor a
# simulation, and runs in seconds.
#
# Why this check exists. scripts/calibrate_dow.R searches for the Priority 1
# and Priority 2 ceilings that make the model's treated-cohort died-of-wounds
# rate meet a target. The search is only worth trusting if it finds a ceiling
# it is known to be able to find, and a fitter that returned the wrong
# ceiling would look exactly like a successful calibration: a number, an
# interval and an exit status of zero. So the search in R/dow_calibration.R is
# given a response whose root is computable by hand and asked to recover it.
# That assertion is then shown to reject fitters that are deliberately wrong,
# so it cannot pass on a search that never used the response at all.
#
# What it asserts:
#
#   1. On a linear response, a convex response and a response carrying
#      deterministic noise of the size a pooled rate carries, the fitter returns
#      the ceiling whose rate meets the target, within tolerance.
#   2. A bracket that does not contain the target is reported rather than
#      searched, naming which end missed; a bracket end that meets the target is
#      returned at once; and the iteration cap is respected.
#   3. The assertion in (1) fails for a fitter that returns the bracket's
#      midpoint, one that returns its lower end, and one that searches the
#      wrong side of the target.
#   4. `pooled_rate_summary()` returns the interval a hand computation gives,
#      clamped at zero.
#   5. The Parameter Calibration table of the README states the ceilings the
#      shipped configuration carries, and the ceilings the README gives for
#      `high_intensity` are the ones that profile carries. A table showing the
#      modern-care pairing as though it were shipped would have a reader
#      configuring a ceiling more than twice the shipped one.
#   6. Every scenario of `env_data.json` has a historical anchor, so the
#      fitter and the regression check can each be run against it.

source("R/dow_calibration.R")

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
#' @return Invisible NULL.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

#' Tolerance on the rate the fitter is asked to meet
RATE_TOL <- 1e-5
#' Relative tolerance on the ceiling the fitter recovers
CEILING_REL_TOL <- 0.02
#' README the calibration table is read from
README <- "README.md"
#' Configuration whose shipped ceilings the table is compared with
CONFIG <- "env_data.json"

#' A response of the pooled rate to the Priority 1 ceiling
#'
#' @param shape Function of the ceiling giving the noise-free rate.
#' @param noise Amplitude of deterministic noise added to it, standing for the
#'   sampling variation of a pooled rate at a fixed set of control seeds.
#' @return A function of the ceiling returning `rate`, `ci_lower` and `ci_upper`.
make_response <- function(shape, noise = 0) {
  function(p) {
    r <- shape(p) + noise * sin(4000 * p)
    c(rate = r, ci_lower = r - 0.0005, ci_upper = r + 0.0005)
  }
}

#' Whether a fitter recovers the ceiling at which a response meets a target
#'
#' @param fitter Function with the signature of `fit_dow_ceiling()`.
#' @param shape Noise-free response function.
#' @param noise Deterministic noise amplitude.
#' @param target Rate to reach.
#' @param truth Ceiling at which `shape` equals `target`.
#' @return TRUE when the returned ceiling is within tolerance of `truth`, its
#'   rate within tolerance of the target, and the fit is reported as converged.
recovers <- function(fitter, shape, noise, target, truth) {
  fit <- fitter(make_response(shape, noise), target, lower = 0.5 * truth, upper = 2 * truth,
                tol_rate = RATE_TOL + noise, tol_p = 1e-7, max_iter = 40L)
  isTRUE(fit$converged) &&
    abs(fit$p1_p_max - truth) <= CEILING_REL_TOL * truth &&
    abs(fit$rate - target) <= RATE_TOL + noise + 1e-12
}

# ── The fitter recovers a known ceiling ─────────────────────────────────────

cat("DOW ceiling fitter check\n\n-- the fitter recovers a known ceiling --\n")

TARGET <- 0.0046
linear <- function(p) 0.5 * p
convex <- function(p) 40 * p^2
report(recovers(fit_dow_ceiling, linear, 0, TARGET, truth = TARGET / 0.5),
       "a linear response is fitted to its root (%.4f)", TARGET / 0.5)
report(recovers(fit_dow_ceiling, convex, 0, TARGET, truth = sqrt(TARGET / 40)),
       "a convex response is fitted to its root (%.4f)", sqrt(TARGET / 40))
report(recovers(fit_dow_ceiling, linear, 2e-5, TARGET, truth = TARGET / 0.5),
       "a linear response carrying deterministic noise is fitted within the noise")

# ── Brackets, endpoints and the iteration cap ───────────────────────────────

cat("\n-- brackets, endpoints and the iteration cap --\n")

below <- fit_dow_ceiling(make_response(linear), target = 0.0001, lower = 0.005, upper = 0.02)
report(!below$converged && grepl("lower end", below$reason),
       "a target below the bracket is reported, naming the lower end")
above <- fit_dow_ceiling(make_response(linear), target = 0.5, lower = 0.005, upper = 0.02)
report(!above$converged && grepl("upper end", above$reason),
       "a target above the bracket is reported, naming the upper end")
report(nrow(below$trace) <= 2L && nrow(above$trace) == 2L,
       "an unbracketed target costs the two bracket evaluations and no more")

hit <- fit_dow_ceiling(make_response(linear), target = 0.5 * 0.005, lower = 0.005, upper = 0.02,
                       tol_rate = 1e-9)
report(hit$converged && nrow(hit$trace) == 1L && hit$p1_p_max == 0.005,
       "a bracket end that meets the target is returned after one evaluation")

capped <- fit_dow_ceiling(make_response(convex), target = TARGET, lower = 0.002, upper = 0.04,
                          tol_rate = 1e-15, tol_p = 1e-15, max_iter = 3L)
report(!capped$converged && nrow(capped$trace) <= 5L && grepl("cap", capped$reason),
       "the iteration cap is respected (%d evaluations at a cap of 3)", nrow(capped$trace))
report(identical(capped$trace$iteration, seq_len(nrow(capped$trace)) - 1L) &&
         capped$p1_p_max == capped$trace$p1_p_max[which.min(abs(capped$trace$rate - TARGET))],
       "the trace is in evaluation order and the result is its closest point")

# ── The recovery assertion rejects fitters that are wrong ───────────────────

cat("\n-- the recovery assertion rejects a wrong fitter (fault injection) --\n")

#' A fitter that ignores the response and returns the middle of the bracket
#'
#' @param evaluate,target,lower,upper,... Arguments of `fit_dow_ceiling()`.
#' @return A result list of the same shape as `fit_dow_ceiling()`'s.
midpoint_fitter <- function(evaluate, target, lower, upper, ...) {
  p <- (lower + upper) / 2
  m <- evaluate(p)
  list(p1_p_max = p, rate = m[["rate"]], converged = TRUE)
}
#' A fitter that returns the lower end of the bracket as though it had converged
#'
#' @inheritParams midpoint_fitter
#' @return A result list of the same shape as `fit_dow_ceiling()`'s.
lower_fitter <- function(evaluate, target, lower, upper, ...) {
  m <- evaluate(lower)
  list(p1_p_max = lower, rate = m[["rate"]], converged = TRUE)
}
#' A fitter that searches the wrong side of the target
#'
#' @inheritParams midpoint_fitter
#' @return The result of `fit_dow_ceiling()` run against a response of
#'   opposite sign about the target.
mirrored_fitter <- function(evaluate, target, lower, upper, ...) {
  fit_dow_ceiling(function(p) {
    m <- evaluate(p)
    m[["rate"]] <- 2 * target - m[["rate"]]
    m
  }, target, lower, upper, ...)
}

truth <- TARGET / 0.5
report(!recovers(midpoint_fitter, linear, 0, TARGET, truth),
       "a fitter that returns the bracket's midpoint is rejected")
report(!recovers(lower_fitter, linear, 0, TARGET, truth),
       "a fitter that returns the bracket's lower end is rejected")
report(!recovers(mirrored_fitter, linear, 0, TARGET, truth),
       "a fitter that searches the wrong side of the target is rejected")

# ── Pooled interval ─────────────────────────────────────────────────────────

cat("\n-- the pooled interval is the Student t one, clamped at zero --\n")

s <- pooled_rate_summary(c(0.10, 0.20, 0.30))
hw <- qt(0.975, df = 2) * 0.1 / sqrt(3)
report(s$n == 3L && abs(s$mean - 0.2) < 1e-12 && abs(s$half_width - hw) < 1e-12,
       "three rates of 0.1, 0.2 and 0.3 give a mean of 0.2 and the t half-width %.4f", hw)
report(s$ci_lower == 0 && abs(s$ci_upper - (0.2 + hw)) < 1e-12,
       "a lower bound below zero is clamped at zero and the upper bound is not")

# ── The documented table is the shipped one ─────────────────────────────────

cat("\n-- the documented calibration is the shipped calibration --\n")

#' Died-of-wounds ceilings of a configuration's variable list
#'
#' @param vars A `vars` list of element entries, as in `env_data.json`.
#' @return Named numeric of `p1_p_max` and `p2_p_max`, or NULL where the list
#'   carries no `dow` element.
dow_ceilings <- function(vars) {
  for (entry in vars) {
    if (!identical(entry$elm, "dow")) next
    for (acty in entry$actys) {
      if (!identical(acty$acty, "params")) next
      vals <- setNames(vapply(acty$vals, function(v) as.numeric(v$val), numeric(1)),
                       vapply(acty$vals, function(v) v$var, ""))
      return(vals[c("p1_p_max", "p2_p_max")])
    }
  }
  NULL
}

config <- jsonlite::fromJSON(CONFIG, simplifyVector = FALSE)
base <- dow_ceilings(config$vars)
report(!is.null(base) && all(!is.na(base)), "the base configuration carries both ceilings")

readme <- readLines(README, warn = FALSE)
start <- grep("^### Parameter Calibration\\s*$", readme)
report(length(start) == 1L, "the README has one Parameter Calibration section")
table_rows <- readme[start + seq_len(8L)]

#' The p_max cell of one priority's row in the calibration table
#'
#' @param label Text the row opens with, such as "P1".
#' @return The numeric ceiling in that row's third cell, NA where the row is absent.
table_ceiling <- function(label) {
  row <- grep(paste0("^\\|\\s*", label, "\\b"), table_rows, value = TRUE)
  if (length(row) != 1L) return(NA_real_)
  as.numeric(trimws(strsplit(row, "|", fixed = TRUE)[[1]][4]))
}
report(isTRUE(all.equal(table_ceiling("P1"), unname(base[["p1_p_max"]]))),
       "the table's Priority 1 ceiling (%s) is the shipped %s",
       format(table_ceiling("P1")), format(base[["p1_p_max"]]))
report(isTRUE(all.equal(table_ceiling("P2"), unname(base[["p2_p_max"]]))),
       "the table's Priority 2 ceiling (%s) is the shipped %s",
       format(table_ceiling("P2")), format(base[["p2_p_max"]]))

high <- dow_ceilings(config$scenarios$high_intensity$vars)
text <- paste(readme, collapse = "\n")
report(!is.null(high) && grepl(sprintf("`p1_p_max` = %s and `p2_p_max` = %s", high[["p1_p_max"]],
                                       high[["p2_p_max"]]), text, fixed = TRUE),
       "the README states the ceilings `high_intensity` carries (%s and %s)",
       format(high[["p1_p_max"]]), format(high[["p2_p_max"]]))
report(grepl(sprintf("`p1_p_max` = %s and `p2_p_max` = %s", base[["p1_p_max"]],
                     base[["p2_p_max"]]), text, fixed = TRUE),
       "the README states the shipped base ceilings (%s and %s)",
       format(base[["p1_p_max"]]), format(base[["p2_p_max"]]))

# ── Every scenario has an anchor ────────────────────────────────────────────

cat("\n-- every scenario has a historical anchor --\n")

scenarios <- c("default", names(config$scenarios))
anchored <- scenarios %in% names(DOW_TARGETS)
report(all(anchored | scenarios == "high_intensity_modern_care"),
       "every shipped scenario but the load-only profile has a historical anchor%s",
       if (any(!anchored & scenarios != "high_intensity_modern_care")) {
         paste0("; absent: ", paste(scenarios[!anchored & scenarios != "high_intensity_modern_care"],
                                    collapse = ", "))
       } else "")
report(all(vapply(DOW_TARGETS, function(t) t$rate > 0 && t$rate < 1 && t$kind %in% c("bound", "point"),
                  logical(1))),
       "every anchor is a proportion of a recognised kind")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All died-of-wounds fitter checks passed.\n")
quit(status = 0)
