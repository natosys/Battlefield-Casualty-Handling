#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_dow_fitter.R                                          ##
## Regression check — the died-of-wounds fitter recovers a known value,   ##
## and the documented calibration table is the shipped one                  ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_dow_fitter.R
#
# Exits 0 when every check passes, 1 otherwise. It needs neither simmer nor a
# simulation, and runs in seconds.
#
# Why this check exists. scripts/calibrate_dow.R searches for the base rates
# that make the model's treated-cohort died-of-wounds rate meet a target, and
# the post-operative multiplier that makes the post-operative share meet its
# own. The search is only worth trusting if it finds a value it is known to be
# able to find, and a fitter that returned the wrong value would look exactly
# like a successful calibration: a number, an
# interval and an exit status of zero. So the search in R/dow_calibration.R is
# given a response whose root is computable by hand and asked to recover it.
# That assertion is then shown to reject fitters that are deliberately wrong,
# so it cannot pass on a search that never used the response at all.
#
# What it asserts:
#
#   1. On a linear response, a convex response and a response carrying
#      deterministic noise of the size a pooled rate carries, the fitter returns
#      the base rate whose rate meets the target, within tolerance.
#   2. A bracket that does not contain the target is reported rather than
#      searched, naming which end missed; a bracket end that meets the target is
#      returned at once; and the iteration cap is respected.
#   3. The assertion in (1) fails for a fitter that returns the bracket's
#      midpoint, one that returns its lower end, and one that searches the
#      wrong side of the target.
#   4. `pooled_rate_summary()` returns the interval a hand computation gives,
#      clamped at zero.
#   5. The Calibration table of the README states the base rates and the
#      post-operative multiplier the shipped configuration carries, so a refit
#      cannot leave the document describing values the model no longer runs.
#   6. No scenario overrides the died-of-wounds model, the profiles sharing
#      one by design, so the one anchor the default carries is the only
#      calibration a profile can have; `high_intensity` carries none, its
#      mortality being a model output.
#   7. `fit_post_op()` recovers a known multiplier on a share whose root is
#      computable by hand.

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
#' Relative tolerance on the base rate the fitter recovers
CEILING_REL_TOL <- 0.02
#' README the calibration table is read from
README <- "README.md"
#' Configuration whose shipped base rates the table is compared with
CONFIG <- "env_data.json"

#' A response of the pooled rate to the Priority 1 base rate
#'
#' @param shape Function of the base rate giving the noise-free rate.
#' @param noise Amplitude of deterministic noise added to it, standing for the
#'   sampling variation of a pooled rate at a fixed set of control seeds.
#' @return A function of the base rate returning `rate`, `ci_lower` and `ci_upper`.
make_response <- function(shape, noise = 0) {
  function(p) {
    r <- shape(p) + noise * sin(4000 * p)
    c(rate = r, ci_lower = r - 0.0005, ci_upper = r + 0.0005)
  }
}

#' Whether a fitter recovers the base rate at which a response meets a target
#'
#' @param fitter Function with the signature of `fit_dow_rate()`.
#' @param shape Noise-free response function.
#' @param noise Deterministic noise amplitude.
#' @param target Rate to reach.
#' @param truth Base rate at which `shape` equals `target`.
#' @return TRUE when the returned base rate is within tolerance of `truth`, its
#'   rate within tolerance of the target, and the fit is reported as converged.
recovers <- function(fitter, shape, noise, target, truth) {
  fit <- fitter(make_response(shape, noise), target, lower = 0.5 * truth, upper = 2 * truth,
                tol_rate = RATE_TOL + noise, tol_p = 1e-7, max_iter = 40L)
  isTRUE(fit$converged) &&
    abs(fit$p1_rate - truth) <= CEILING_REL_TOL * truth &&
    abs(fit$rate - target) <= RATE_TOL + noise + 1e-12
}

# ── The fitter recovers a known base rate ─────────────────────────────────────

cat("DOW base rate fitter check\n\n-- the fitter recovers a known base rate --\n")

#' Rate the fitter is asked to reach in the recovery assertions, the Ajax Bay bound
TARGET <- 0.0046
#' Noise-free linear response of the rate to the base rate, reaching the target at 0.0092
#'
#' @param p Priority 1 base rate.
#' @return The rate at that base rate.
linear <- function(p) 0.5 * p
#' Noise-free convex response of the rate to the base rate
#'
#' @param p Priority 1 base rate.
#' @return The rate at that base rate.
convex <- function(p) 40 * p^2
report(recovers(fit_dow_rate, linear, 0, TARGET, truth = TARGET / 0.5),
       "a linear response is fitted to its root (%.4f)", TARGET / 0.5)
report(recovers(fit_dow_rate, convex, 0, TARGET, truth = sqrt(TARGET / 40)),
       "a convex response is fitted to its root (%.4f)", sqrt(TARGET / 40))
report(recovers(fit_dow_rate, linear, 2e-5, TARGET, truth = TARGET / 0.5),
       "a linear response carrying deterministic noise is fitted within the noise")

# ── Brackets, endpoints and the iteration cap ───────────────────────────────

cat("\n-- brackets, endpoints and the iteration cap --\n")

below <- fit_dow_rate(make_response(linear), target = 0.0001, lower = 0.005, upper = 0.02)
report(!below$converged && grepl("lower end", below$reason),
       "a target below the bracket is reported, naming the lower end")
above <- fit_dow_rate(make_response(linear), target = 0.5, lower = 0.005, upper = 0.02)
report(!above$converged && grepl("upper end", above$reason),
       "a target above the bracket is reported, naming the upper end")
report(nrow(below$trace) <= 2L && nrow(above$trace) == 2L,
       "an unbracketed target costs the two bracket evaluations and no more")

hit <- fit_dow_rate(make_response(linear), target = 0.5 * 0.005, lower = 0.005, upper = 0.02,
                       tol_rate = 1e-9)
report(hit$converged && nrow(hit$trace) == 1L && hit$p1_rate == 0.005,
       "a bracket end that meets the target is returned after one evaluation")

capped <- fit_dow_rate(make_response(convex), target = TARGET, lower = 0.002, upper = 0.04,
                          tol_rate = 1e-15, tol_p = 1e-15, max_iter = 3L)
report(!capped$converged && nrow(capped$trace) <= 5L && grepl("cap", capped$reason),
       "the iteration cap is respected (%d evaluations at a cap of 3)", nrow(capped$trace))
report(identical(capped$trace$iteration, seq_len(nrow(capped$trace)) - 1L) &&
         capped$p1_rate == capped$trace$p1_rate[which.min(abs(capped$trace$rate - TARGET))],
       "the trace is in evaluation order and the result is its closest point")

# ── The recovery assertion rejects fitters that are wrong ───────────────────

cat("\n-- the recovery assertion rejects a wrong fitter (fault injection) --\n")

#' A fitter that ignores the response and returns the middle of the bracket
#'
#' @param evaluate,target,lower,upper,... Arguments of `fit_dow_rate()`.
#' @return A result list of the same shape as `fit_dow_rate()`'s.
midpoint_fitter <- function(evaluate, target, lower, upper, ...) {
  p <- (lower + upper) / 2
  m <- evaluate(p)
  list(p1_rate = p, rate = m[["rate"]], converged = TRUE)
}
#' A fitter that returns the lower end of the bracket as though it had converged
#'
#' @param evaluate,target,lower,upper,... Arguments of `fit_dow_rate()`.
#' @return A result list of the same shape as `fit_dow_rate()`'s.
lower_fitter <- function(evaluate, target, lower, upper, ...) {
  m <- evaluate(lower)
  list(p1_rate = lower, rate = m[["rate"]], converged = TRUE)
}
#' A fitter that searches the wrong side of the target
#'
#' @param evaluate,target,lower,upper,... Arguments of `fit_dow_rate()`.
#' @return The result of `fit_dow_rate()` run against a response of
#'   opposite sign about the target.
mirrored_fitter <- function(evaluate, target, lower, upper, ...) {
  fit_dow_rate(function(p) {
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

#' The fitted died-of-wounds values of a configuration's variable list
#'
#' @param vars A `vars` list of element entries, as in `env_data.json`.
#' @return Named numeric of `p1_rate`, `p2_rate`, `p3_rate` and `post_op`, or
#'   NULL where the list carries no `dow` element.
dow_fitted <- function(vars) {
  out <- c()
  for (entry in vars) {
    if (!identical(entry$elm, "dow")) next
    for (acty in entry$actys) {
      vals <- setNames(vapply(acty$vals, function(v) as.numeric(v$val), numeric(1)),
                       vapply(acty$vals, function(v) v$var, ""))
      if (identical(acty$acty, "rates")) out <- c(out, vals[c("p1_rate", "p2_rate", "p3_rate")])
      if (identical(acty$acty, "multipliers")) out <- c(out, vals["post_op"])
    }
  }
  if (!length(out)) NULL else out
}

config <- jsonlite::fromJSON(CONFIG, simplifyVector = FALSE)
base <- dow_fitted(config$vars)
report(!is.null(base) && length(base) == 4L && all(!is.na(base)),
       "the base configuration carries the three base rates and the post-operative multiplier")

readme <- readLines(README, warn = FALSE)
start <- grep("^### Calibration\\s*$", readme)
report(length(start) == 1L, "the README's Died of Wounds chapter has one Calibration section")
table_rows <- readme[start + seq_len(14L)]

#' The shipped-value cell of one fitted value's row in the calibration table
#'
#' @param key The value's configuration key, set in backticks in its row.
#' @return The number in that row's second cell, NA where the row is absent.
table_value <- function(key) {
  row <- grep(sprintf("`%s`", key), table_rows, value = TRUE, fixed = TRUE)
  if (length(row) != 1L) return(NA_real_)
  cell <- trimws(strsplit(row, "|", fixed = TRUE)[[1]][3])
  suppressWarnings(as.numeric(sub("^([0-9.]+).*$", "\\1", cell)))
}
for (key in names(base)) {
  report(isTRUE(all.equal(table_value(key), unname(base[[key]]))),
         "the table's `%s` (%s) is the shipped %s",
         key, format(table_value(key)), format(base[[key]]))
}

# ── One mortality model, one anchor ─────────────────────────────────────────

cat("\n-- the profiles share one mortality model --\n")

#' Whether a scenario's overrides touch the died-of-wounds model
#'
#' @param sc One entry of the configuration's `scenarios` block.
#' @return TRUE when the profile overrides an element named "dow".
overrides_dow <- function(sc) {
  any(vapply(sc$vars, function(e) identical(e$elm, "dow"), logical(1)))
}
overriding <- names(Filter(overrides_dow, config$scenarios))
report(length(overriding) == 0,
       "no scenario overrides the died-of-wounds model%s",
       if (length(overriding)) paste0("; overriding: ", paste(overriding, collapse = ", ")) else "")
report(setequal(names(DOW_TARGETS), c("default", "moderate_intensity")),
       "the anchors are the default's and its alias's alone (%s)",
       paste(names(DOW_TARGETS), collapse = ", "))
#' Whether one anchor is a proportion of a recognised kind
#'
#' @param t An entry of `DOW_TARGETS`.
#' @return TRUE when its rate lies in (0, 1) and its kind is "bound" or "point".
anchor_ok <- function(t) t$rate > 0 && t$rate < 1 && t$kind %in% c("bound", "point")
report(all(vapply(DOW_TARGETS, anchor_ok, logical(1))),
       "every anchor is a proportion of a recognised kind")

# ── The post-operative multiplier search ───────────────────────────────────

cat("\n-- fit_post_op() recovers a known multiplier --\n")

# A share of the form L*s / (E + L*s), the post-operative expected deaths
# growing in proportion to the multiplier against a fixed early count, has its root at
# s = target * E / (L * (1 - target)): with E = 20, L = 2.5 and a target of
# 0.2, s = 2.
#' A post-operative share whose root is known
#'
#' @param s Multiplier to evaluate at.
#' @return The share at `s`.
known_share <- function(s) 2.5 * s / (20 + 2.5 * s)
hz <- fit_post_op(known_share, 0.2, lower = 0.25, upper = 8, tol = 1e-4)
report(isTRUE(hz$converged) && abs(hz$post_op - 2) < 0.01,
       "the search returns a multiplier of %.4f against the known 2", hz$post_op)
report("post_op" %in% names(hz$trace) && is.null(hz$p1_rate),
       "the result names the fitted value the post-operative multiplier")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All died-of-wounds fitter checks passed.\n")
quit(status = 0)
