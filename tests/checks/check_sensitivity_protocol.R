#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_sensitivity_protocol.R                                ##
## Regression check — the sensitivity screens' documented design is the      ##
## design their tracked run metadata records                                 ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_sensitivity_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The Morris and Sobol screens are the longest
# computations in the project, about fifteen and a half hours and a hundred
# hours, and nobody re-runs them to audit a change. docs/Methods.md states
# their design (trajectories, parameter count, replications per point, horizon,
# design size), and the tracked run metadata records what was actually run. A
# design the document states and the metadata does not record is a screen the
# paper describes and nobody ran. This check holds the two together without
# running either screen. It defends the design, and that the primary rankings
# the results paper prints are the reduction of the tracked design and cache;
# the Sobol indices are not re-derived here, the bootstrap taking minutes.
#
# What this asserts:
#
#   1. Every design parameter docs/Methods.md states in a marker comment equals
#      the value the corresponding tracked run metadata records.
#   2. Each design size is the product its parameters imply: the Morris design
#      has r (k + 1) points and the Sobol design N (k + 2).
#   3. Each screen's tracked ranking files exist, so a metadata file cannot
#      describe a run whose results were never kept.
#   4. The Morris cache holds every design point once and agrees with the
#      saved response matrix, and the tracked rankings on the R2E OT queue and
#      the surgical delivery share are the elementary effects of that matrix,
#      so a ranking cannot describe a different screen from its cache.

#' The document the screens' design is published in
METHODS_PATH <- file.path("docs", "Methods.md")

#' Tracked directory of the Morris screen the methods paper documents
MORRIS_DIR <- file.path("data", "sensitivity", "morris_r20_d360")

#' Tracked directory of the Sobol decomposition the methods paper documents
SOBOL_DIR <- file.path("data", "sensitivity", "sobol_n96_d360")

#' Tracked run metadata of the Morris screen
MORRIS_META <- file.path(MORRIS_DIR, "morris_run_metadata.csv")

#' Tracked run metadata of the Sobol decomposition
SOBOL_META <- file.path(SOBOL_DIR, "sobol_run_metadata.csv")

#' Tracked ranking the Morris screen's results were kept in
MORRIS_RANKING <- file.path(MORRIS_DIR, "morris_ranking.csv")

#' Responses whose tracked ranking is recomputed from the tracked design and cache
RANKED_RESPONSES <- c("r2e_ot_q", "surgery_delivery_rate")

#' Relative tolerance a recomputed mu* or sigma must agree with its ranking to
RANKING_TOLERANCE <- 1e-8

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
#'
#' @details Anything other than TRUE is a failure, NA included, since a
#'   quantity compared here becomes NA when the thing under test is wrong.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' Read one tracked run metadata file as a named character vector
#'
#' @param path Path to a `*_run_metadata.csv` file.
#' @return Named character vector of field values, or NULL where the file is
#'   absent.
read_metadata <- function(path) {
  if (!file.exists(path)) {
    report(FALSE, "the tracked run metadata %s exists", path)
    return(NULL)
  }
  m <- read.csv(path, stringsAsFactors = FALSE)
  stats::setNames(as.character(m$value), m$field)
}

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one design parameter the methods paper states in a marker comment
#'
#' @param name Marker name, as it appears after "SENSITIVITY ".
#' @return The marker's numeric value, or NA where absent.
sensitivity_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- SENSITIVITY %s=[0-9]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_real_)
  as.numeric(gsub("[^0-9]", "", sub("^<!-- SENSITIVITY [a-z_]+=", "", m)))
}

# ── 1. The stated design is the recorded design ───────────────────────────────

cat("\n-- the documented design matches the tracked run metadata --\n")

morris <- read_metadata(MORRIS_META)
sobol  <- read_metadata(SOBOL_META)

checks <- list(
  list("morris_trajectories", morris, "r"),
  list("morris_parameters", morris, "n_params"),
  list("morris_points", morris, "n_design_points"),
  list("morris_replications", morris, "n_rep"),
  list("sobol_n", sobol, "n_sobol"),
  list("sobol_parameters", sobol, "n_params"),
  list("sobol_points", sobol, "n_design_points"),
  list("sobol_replications", sobol, "n_rep")
)
for (spec in checks) {
  stated <- sensitivity_marker(spec[[1]])
  recorded <- if (is.null(spec[[2]])) NA_real_ else as.numeric(spec[[2]][[spec[[3]]]])
  report(!is.na(stated) && !is.na(recorded) && stated == recorded,
         "the methods paper states %s = %s and the metadata records %s",
         spec[[1]], format(stated), format(recorded))
}

stated_days <- sensitivity_marker("days")
for (screen in list(list("Morris", morris), list("Sobol", sobol))) {
  recorded <- if (is.null(screen[[2]])) NA_real_ else as.numeric(screen[[2]][["n_days"]])
  report(!is.na(stated_days) && !is.na(recorded) && stated_days == recorded,
         "the methods paper states a %s-day horizon and the %s metadata records %s",
         format(stated_days), screen[[1]], format(recorded))
}

# ── 2. Each design is the size its parameters imply ──────────────────────────

cat("\n-- each design has the size its parameters imply --\n")

if (!is.null(morris)) {
  r <- as.numeric(morris[["r"]])
  k <- as.numeric(morris[["n_params"]])
  report(r * (k + 1) == as.numeric(morris[["n_design_points"]]),
         "the Morris design has r (k + 1) = %d points (recorded %s)", r * (k + 1),
         morris[["n_design_points"]])
}
if (!is.null(sobol)) {
  n <- as.numeric(sobol[["n_sobol"]])
  k <- as.numeric(sobol[["n_params"]])
  report(n * (k + 2) == as.numeric(sobol[["n_design_points"]]),
         "the Sobol design has N (k + 2) = %d points (recorded %s)", n * (k + 2),
         sobol[["n_design_points"]])
}

# ── 3. The results behind each metadata file were kept ───────────────────────

cat("\n-- the results behind each metadata file were kept --\n")

report(file.exists(MORRIS_RANKING), "the tracked Morris ranking %s exists", MORRIS_RANKING)
report(length(list.files(SOBOL_DIR, pattern = "sobol_.*\\.csv$")) > 1,
       "the tracked Sobol decomposition carries its result files")

# ── 4. The tracked rankings are the reduction of the tracked cache ───────────

cat("\n-- the tracked rankings are the reduction of the tracked design and cache --\n")

saved <- readRDS(file.path(MORRIS_DIR, "morris_design_and_responses.rds"))
cache <- read.csv(file.path(MORRIS_DIR, "points.csv"), stringsAsFactors = FALSE)
cache <- cache[order(cache$i), , drop = FALSE]
report(identical(cache$i, seq_len(nrow(saved$X))),
       "the Morris cache holds every one of the %d design points once", nrow(saved$X))

suppressPackageStartupMessages(library(sensitivity))
k <- ncol(saved$X)
sa <- morris(model = NULL, factors = colnames(saved$X), r = nrow(saved$X) / (k + 1L),
             design = list(type = "oat", levels = 4, grid.jump = 2),
             binf = saved$binf, bsup = saved$bsup, scale = TRUE)
sa$X <- saved$X
for (resp in RANKED_RESPONSES) {
  same <- isTRUE(all.equal(as.numeric(cache[[resp]]), as.numeric(saved$Y[, resp]),
                           tolerance = 1e-12))
  report(same, "the cache and the saved responses agree on %s", resp)
  obj <- sa
  tell(obj, saved$Y[, resp])
  mu_star <- apply(obj$ee, 2, function(v) mean(abs(v), na.rm = TRUE))
  sigma <- apply(obj$ee, 2, function(v) sd(v, na.rm = TRUE))
  ranked <- read.csv(file.path(MORRIS_DIR, sprintf("morris_ranking_%s.csv", resp)),
                     stringsAsFactors = FALSE)
  j <- match(ranked$parameter, names(mu_star))
  worst <- max(abs(mu_star[j] - ranked$mu_star) / pmax(1e-12, abs(ranked$mu_star)),
               abs(sigma[j] - ranked$sigma_ee) / pmax(1e-12, abs(ranked$sigma_ee)))
  report(is.finite(worst) && worst <= RANKING_TOLERANCE,
         "the tracked %s ranking is the reduction of the cache (worst relative gap %.1e)",
         resp, worst)
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All sensitivity protocol checks passed.\n")
quit(status = 0)
