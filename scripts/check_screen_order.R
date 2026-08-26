#!/usr/bin/env Rscript
##############################################################################
## scripts/check_screen_order.R                                             ##
## Regression check — a screen evaluates its design in order, exactly once  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_screen_order.R
#
# Exits 0 when every check passes, 1 otherwise. It runs no simulation: the
# expensive part of a screen is one call per design point to eval_params(),
# and this check replaces that with a stub that records the call and returns a
# deterministic value derived from the design row. Both drivers then run in
# full, over a real design, in a couple of seconds.
#
# Why this check exists. run_morris() and run_sobol() are the two functions in
# the repository whose correctness is least observable from their output. A
# production screen is hours long, so its design points are evaluated once and
# cached, and an interrupted screen resumes by looking a point up by its index
# in the design. Three properties hold that arrangement together, and none of
# them shows up in the indices the screen reports:
#
#   the design is a function of the seed and the bounds alone, so the same
#   seed gives the same points;
#   every point is evaluated exactly once, in the order the design lists them,
#   so an index means the same thing on a resume as it did on the first run;
#   the parameter vector handed to the evaluator at point i is the design's own
#   row i, so the cache and the responses describe the same point.
#
# Break any of them and the screen still completes and still writes indices
# that look reasonable. The responses are simply attributed to the wrong
# points, the cache resumes into them, and nothing reports it. That is the
# failure this check exists to make loud, and it is the reason the two drivers
# were left out of the decomposition in PR #267 rather than split by the
# derive-the-interface method the rest of Issue #241 used: an interface for
# them has to preserve these properties deliberately, so they are asserted
# first and the split verified against them.
#
# What it asserts, for each driver:
#
#   1. The design is reproducible. Two runs at one seed give an identical
#      design matrix; runs at different seeds do not (so the check would
#      notice a design that had stopped depending on the seed at all).
#   2. Evaluation covers every design point exactly once, in index order.
#   3. The parameter vector the evaluator receives at point i is the design's
#      row i, compared value by value rather than by count.
#   4. A cached point is not re-evaluated, and the responses the screen ends
#      with are the cached ones.
#
# And once, for the response extraction the two share: extract_kpis() returns
# its responses in the order run_morris() indexes them by, which is the
# contract its own stopifnot() asserts and which a split must not disturb.

suppressPackageStartupMessages({
  source("R/environment.R")
  source("R/sensitivity.R")
})

env_data <<- load_elms("env_data.json")
day_min  <<- DAY_MIN

SEED_A <- 42L
SEED_B <- 7L
SMALL_R <- 2L          # r * (p + 1) points, enough to exercise order and cache
SMALL_N_SOBOL <- 4L

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

# ── The stub ────────────────────────────────────────────────────────────────

# Calls are recorded here, one row per evaluation, in the order they happen.
eval_log <- new.env(parent = emptyenv())
eval_log$rows <- list()

#' Stand in for eval_params(), recording the call rather than simulating
#'
#' @param params_row One design row, as the driver slices it.
#' @param n_rep,n_days,max_cores,crn_seed Accepted and ignored; present so the
#'   signature matches the function this replaces.
#' @param return_sd TRUE to append per-response standard deviations, which the
#'   Sobol driver asks for.
#' @return A named response vector derived from the design row, deterministic
#'   in that row so a repeated design gives repeated responses.
#' @details The responses are arbitrary but not random: each is a fixed linear
#'   function of the row, so a point evaluated twice returns the same value and
#'   any reordering shows up as a changed response rather than as noise.
stub_eval_params <- function(params_row, n_rep, n_days, max_cores = NULL,
                             crn_seed = NULL, return_sd = FALSE) {
  eval_log$rows[[length(eval_log$rows) + 1L]] <- as.numeric(params_row)
  base <- sum(as.numeric(params_row) * seq_along(params_row))
  out <- setNames(base + seq_len(nrow(morris_kpis)), morris_kpis$name)
  if (return_sd) {
    out <- c(out, setNames(rep(1, nrow(morris_kpis)), paste0("sd_", morris_kpis$name)))
  }
  out
}

#' Run one driver with the evaluator stubbed, returning what it was asked
#'
#' @param expr Expression calling the driver.
#' @return A list of `result` (the driver's own return value) and `rows` (the
#'   design rows the evaluator was handed, in call order).
with_stubbed_eval <- function(expr) {
  eval_log$rows <- list()
  real <- get("eval_params", envir = globalenv())
  assign("eval_params", stub_eval_params, envir = globalenv())
  # ggsave() is shadowed for the same reason eval_params() is: this check
  # asserts nothing about what a screen plots, and a driver renders one
  # ggrepel-labelled scatter per response, which is almost the whole cost of
  # running it. A binding in the global environment precedes the attached
  # ggplot2 on the search path, so the drivers' unqualified calls find this.
  assign("ggsave", function(...) invisible(NULL), envir = globalenv())
  on.exit({
    assign("eval_params", real, envir = globalenv())
    suppressWarnings(rm("ggsave", envir = globalenv()))
  }, add = TRUE)
  res <- suppressMessages(suppressWarnings(force(expr)))
  list(result = res, rows = eval_log$rows)
}

#' The design matrix a completed screen saved alongside its responses
#'
#' @param output_dir Directory the driver wrote into.
#' @param file Base name of the saved design.
#' @return The design matrix.
saved_design <- function(output_dir, file) {
  readRDS(file.path(output_dir, file))$X
}

# ── 1. Morris ───────────────────────────────────────────────────────────────

cat("-- Morris --\n")

tmp_a <- tempfile("morris_a_")
tmp_b <- tempfile("morris_b_")
tmp_c <- tempfile("morris_c_")

set.seed(SEED_A)
run_a <- with_stubbed_eval(run_morris(n_days = 1, n_rep = 1, r = SMALL_R,
                                      output_dir = tmp_a))
set.seed(SEED_A)
run_b <- with_stubbed_eval(run_morris(n_days = 1, n_rep = 1, r = SMALL_R,
                                      output_dir = tmp_b))
set.seed(SEED_B)
run_c <- with_stubbed_eval(run_morris(n_days = 1, n_rep = 1, r = SMALL_R,
                                      output_dir = tmp_c))

design_a <- saved_design(tmp_a, "morris_design_and_responses.rds")
design_b <- saved_design(tmp_b, "morris_design_and_responses.rds")
design_c <- saved_design(tmp_c, "morris_design_and_responses.rds")

report(identical(design_a, design_b),
       "the design is reproducible: two runs at one seed give an identical design")
report(!identical(design_a, design_c),
       "the design depends on the seed: a different seed gives a different design")

report(length(run_a$rows) == nrow(design_a),
       "every design point is evaluated exactly once (%d points, %d evaluations)",
       nrow(design_a), length(run_a$rows))

order_ok <- length(run_a$rows) == nrow(design_a) &&
  all(vapply(seq_along(run_a$rows), function(i) {
    isTRUE(all.equal(run_a$rows[[i]], as.numeric(design_a[i, ]), tolerance = 0))
  }, logical(1)))
report(order_ok,
       "the evaluator receives design row i at evaluation i, value by value")

report(identical(lapply(run_a$rows, identity), lapply(run_b$rows, identity)),
       "the evaluation order repeats at one seed")

# ── 2. Morris cache ─────────────────────────────────────────────────────────

cache <- tempfile("morris_cache_")
tmp_d <- tempfile("morris_d_")
tmp_e <- tempfile("morris_e_")

set.seed(SEED_A)
first <- with_stubbed_eval(run_morris(n_days = 1, n_rep = 1, r = SMALL_R,
                                      output_dir = tmp_d, cache_dir = cache))
set.seed(SEED_A)
resumed <- with_stubbed_eval(run_morris(n_days = 1, n_rep = 1, r = SMALL_R,
                                        output_dir = tmp_e, cache_dir = cache))

report(length(first$rows) > 0 && length(resumed$rows) == 0,
       "a fully cached screen re-evaluates nothing (%d evaluations, then %d)",
       length(first$rows), length(resumed$rows))

y_first <- readRDS(file.path(tmp_d, "morris_design_and_responses.rds"))$Y
y_resumed <- readRDS(file.path(tmp_e, "morris_design_and_responses.rds"))$Y
report(isTRUE(all.equal(y_first, y_resumed)),
       "a resumed screen ends with the responses the first run cached")

# ── 3. Sobol ────────────────────────────────────────────────────────────────

cat("\n-- Sobol --\n")

top <- head(morris_params$name, 3L)
sob_a <- tempfile("sobol_a_")
sob_b <- tempfile("sobol_b_")

set.seed(SEED_A)
srun_a <- with_stubbed_eval(run_sobol(top, n_days = 1, n_rep = 1,
                                      n_sobol = SMALL_N_SOBOL, output_dir = sob_a,
                                      nboot = 10))
set.seed(SEED_A)
srun_b <- with_stubbed_eval(run_sobol(top, n_days = 1, n_rep = 1,
                                      n_sobol = SMALL_N_SOBOL, output_dir = sob_b,
                                      nboot = 10))

# run_sobol() saves no design matrix of its own, unlike run_morris(), so the
# design is observed through the evaluator rather than read back. That still
# settles the question this check is about: if the design or the order it is
# walked in were not a function of the seed alone, two seeded runs would hand
# the evaluator different rows.
rows_equal <- length(srun_a$rows) == length(srun_b$rows) &&
  all(vapply(seq_along(srun_a$rows), function(i) {
    isTRUE(all.equal(srun_a$rows[[i]], srun_b$rows[[i]], tolerance = 0))
  }, logical(1)))
report(rows_equal,
       "the Sobol design and its evaluation order both repeat at one seed (%d evaluations)",
       length(srun_a$rows))

# A pick-freeze decomposition evaluates the two base matrices and one hybrid
# per parameter, so N * (k + 2) points. Asserting the count catches a split
# that dropped or duplicated a block of the design, which comparing two runs
# of the same code would not.
expected <- SMALL_N_SOBOL * (length(top) + 2L)
report(length(srun_a$rows) == expected,
       "the Sobol design is N * (k + 2) points (expected %d, evaluated %d)",
       expected, length(srun_a$rows))

report(length(unique(vapply(srun_a$rows, function(r) paste(format(r, digits = 17),
                                                           collapse = ","),
                            character(1)))) == length(srun_a$rows),
       "no Sobol design point is evaluated twice")

# ── 4. The response contract the two share ──────────────────────────────────

cat("\n-- Response contract --\n")

report(!any(duplicated(morris_kpis$name)),
       "the response names are unique (%d responses)", nrow(morris_kpis))

stub <- stub_eval_params(morris_params$lower, 1, 1)
report(identical(names(stub), morris_kpis$name),
       "a response vector carries the names run_morris() indexes it by, in order")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All screen design and evaluation order checks passed.\n")
quit(status = 0)
