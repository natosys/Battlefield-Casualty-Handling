#!/usr/bin/env Rscript
##############################################################################
## scripts/compare_sobol_estimators.R                                       ##
## Robustness check — do three pick-freeze estimators agree on the same     ##
## responses?                                                               ##
##############################################################################
#
# Usage:
#   Rscript scripts/compare_sobol_estimators.R \
#     --cache outputs/cache/sobol_r20/points.csv \
#     --params pri1_surg_prob,mass_casualty_rate,mass_casualty_max_cas,mass_casualty_min_cas,pri1_dcs_rate \
#     --output outputs
#
# Why this exists. `run_sobol()` estimates its indices with `sobol2007`. That
# is one of several pick-freeze estimators, and they differ in how they use
# the same model evaluations: `sobol2007` (Saltelli et al., 2010),
# `soboljansen` (Jansen, 1999) and `sobolmartinez` (Martinez, 2011) each
# combine the identical design into different sample formulas, with different
# small-sample behaviour where an index sits near zero. Where the reported
# indices are wide, a reader is entitled to ask whether the finding is a
# property of the model or of the estimator.
#
# The comparison is free. All three estimators are built on the same design:
# `sensitivity` constructs X identically for the three, so the model
# evaluations already cached answer all three without a single further run.
# Nothing here re-simulates anything.
#
# This is a robustness check and not a substitution. `sobol2007` remains the
# reported estimator; running a second one after seeing the first and then
# reporting whichever looked better would be choosing a result rather than
# checking one. What the comparison establishes is only whether the reported
# ordering and the reported separations survive a change of estimator.
#
# The design values themselves do not enter a pick-freeze estimate — each of
# the three is a formula over the response vector and the fixed row layout
# (N, then N, then N per parameter). The design is therefore reconstructed
# here at the right shape and column names rather than re-derived from the
# original run's generator state, which the cache does not record.

suppressPackageStartupMessages({
  library(sensitivity)
  library(boot)
})

args <- commandArgs(trailingOnly = TRUE)

#' Read one flagged command line argument
#'
#' @param flag Flag to look for, including its leading dashes.
#' @param default Value returned when the flag is absent or carries no value.
#' @return The argument following the flag, or `default`.
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

CACHE   <- arg_value("--cache")
PARAMS  <- strsplit(arg_value("--params", ""), ",")[[1]]
OUTPUT  <- arg_value("--output", "outputs")
NBOOT   <- as.integer(arg_value("--nboot", 1000L))
BOOT_SEED <- as.integer(arg_value("--seed", 20250819L))

if (is.null(CACHE) || !file.exists(CACHE)) {
  stop("--cache must name an existing design point cache CSV")
}
if (length(PARAMS) == 0 || !nzchar(PARAMS[1])) {
  stop("--params must list the decomposed parameters, in the order run_sobol() used")
}

dir.create(OUTPUT, recursive = TRUE, showWarnings = FALSE)

tab <- utils::read.csv(CACHE, stringsAsFactors = FALSE)
tab <- tab[order(tab$i), , drop = FALSE]

p <- length(PARAMS)
if (nrow(tab) %% (p + 2L) != 0L) {
  stop(sprintf(
    "cache holds %d points, which is not N * (p + 2) for p = %d — the cache is incomplete or belongs to a different design",
    nrow(tab), p
  ))
}
if (!identical(tab$i, seq_len(nrow(tab)))) {
  stop("cache point indices are not a complete run of 1..n — resume the screen before comparing estimators")
}
n_sobol <- nrow(tab) %/% (p + 2L)

responses <- setdiff(names(tab), c("i", grep("^sd_", names(tab), value = TRUE)))
message(sprintf("Cache: %d points = N %d x (p %d + 2); responses: %s",
                nrow(tab), n_sobol, p, paste(responses, collapse = ", ")))

#' Build one half of the shape-only design matrix
#'
#' @return A data frame of the design's shape, every cell at 0.5.
#' @details The values do not enter the recomputation, only the layout does;
#'   see the header note for why.
mk <- function() {
  d <- as.data.frame(matrix(0.5, nrow = n_sobol, ncol = p))
  names(d) <- PARAMS
  d
}
X1 <- mk()
X2 <- mk()

ESTIMATORS <- list(
  sobol2007     = sobol2007,
  soboljansen   = soboljansen,
  sobolmartinez = sobolmartinez
)

# The bootstrap intervals are resampled, so the comparison would otherwise
# differ between runs on the same cache. A parameter sitting near the
# separation boundary crosses it on the resampling alone at this sample
# size, which was observed while writing this script. The seed is pinned so
# a reported comparison reproduces.
set.seed(BOOT_SEED)

rows <- list()

for (resp in responses) {
  y <- tab[[resp]]
  if (sum(is.finite(y)) < nrow(tab)) {
    warning(sprintf("Skipping %s: %d of %d points are not finite",
                    resp, sum(!is.finite(y)), nrow(tab)), call. = FALSE)
    next
  }
  for (est_name in names(ESTIMATORS)) {
    sb <- ESTIMATORS[[est_name]](model = NULL, X1 = X1, X2 = X2, nboot = NBOOT)
    ok <- tryCatch({ tell(sb, y); TRUE }, error = function(e) {
      warning(sprintf("%s / %s: tell() failed (%s)", resp, est_name,
                      conditionMessage(e)), call. = FALSE)
      FALSE
    })
    if (!ok) next
    if (length(sb$S$original) != p || length(sb$T$original) != p) {
      warning(sprintf("%s / %s: incomplete indices, skipped", resp, est_name),
              call. = FALSE)
      next
    }
    rows[[length(rows) + 1L]] <- data.frame(
      response  = resp,
      estimator = est_name,
      parameter = PARAMS,
      S1        = sb$S$original,
      S1_lower  = sb$S$`min. c.i.`,
      S1_upper  = sb$S$`max. c.i.`,
      ST        = sb$T$original,
      ST_lower  = sb$T$`min. c.i.`,
      ST_upper  = sb$T$`max. c.i.`,
      stringsAsFactors = FALSE
    )
  }
}

if (length(rows) == 0L) stop("no estimator produced a usable decomposition")

out <- do.call(rbind, rows)

# A Sobol index is a variance share in [0, 1] with ST >= S1, but none of the
# three estimators is range-constrained, so a parameter whose true index sits
# near zero routinely returns a value outside it. Flagged, not corrected.
out$flag <- vapply(seq_len(nrow(out)), function(k) {
  f <- character(0)
  if (isTRUE(out$ST[k] > 1))            f <- c(f, "ST>1")
  if (isTRUE(out$S1[k] < 0))            f <- c(f, "S1<0")
  if (isTRUE(out$S1[k] > out$ST[k]))    f <- c(f, "S1>ST")
  if (length(f) == 0L) "ok" else paste(f, collapse = ";")
}, character(1))

path <- file.path(OUTPUT, "sobol_estimator_comparison.csv")
utils::write.csv(out, path, row.names = FALSE)
message(sprintf("\nEstimator comparison written to %s", path))

# ── Agreement summary ───────────────────────────────────────────────────────
#
# Two questions a reader actually asks of a robustness check: does the ranking
# move, and does a parameter that the reported estimator separated from zero
# stay separated? Both are answered per response, against sobol2007.

for (resp in unique(out$response)) {
  sub  <- out[out$response == resp, , drop = FALSE]
  base <- sub[sub$estimator == "sobol2007", , drop = FALSE]
  if (nrow(base) == 0L) next
  base_order <- base$parameter[order(-base$ST)]

  cat(sprintf("\n== %s ==\n", resp))
  cat(sprintf("  sobol2007 ST ranking: %s\n", paste(base_order, collapse = " > ")))
  for (est_name in setdiff(unique(sub$estimator), "sobol2007")) {
    e <- sub[sub$estimator == est_name, , drop = FALSE]
    e_order <- e$parameter[order(-e$ST)]
    rho <- suppressWarnings(stats::cor(
      rank(-base$ST[match(PARAMS, base$parameter)]),
      rank(-e$ST[match(PARAMS, e$parameter)])
    ))
    cat(sprintf("  %-14s ST ranking: %s  (rank correlation %+.2f, order %s)\n",
                est_name, paste(e_order, collapse = " > "), rho,
                if (identical(e_order, base_order)) "identical" else "differs"))
  }

  # An estimator's lower bound sitting above zero means something only where
  # that estimator can return a value below zero. The Jansen total-order
  # estimator is a mean of squared differences and so is non-negative by
  # construction, and the Martinez one is bounded likewise over this design;
  # neither can place a bound at or below zero however uninfluential the
  # parameter, so their bounds are reported alongside the observed minimum
  # rather than read as separations.
  #' Parameters one estimator separates from zero
  #'
  #' @param d One estimator's index table for this response.
  #' @return A character vector of the parameters whose total-order lower
  #'   bound sits above zero.
  sep <- function(d) d$parameter[is.finite(d$ST_lower) & d$ST_lower > 0]
  for (est_name in unique(sub$estimator)) {
    e <- sub[sub$estimator == est_name, , drop = FALSE]
    reaches_zero <- any(e$ST_lower <= 0)
    cat(sprintf("  ST lower bound above zero — %-14s %s%s\n", paste0(est_name, ":"),
                if (length(sep(e))) paste(sep(e), collapse = ", ") else "none",
                if (reaches_zero) "" else
                  sprintf("  [all of them; this estimator's ST never reached zero here (min %.3f), so its bounds do not separate]",
                          min(e$ST))))
  }
}

cat("\nDone. sobol2007 remains the reported estimator; the other two are a robustness check on it.\n")
