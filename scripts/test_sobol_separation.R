#!/usr/bin/env Rscript
##############################################################################
## scripts/test_sobol_separation.R                                          ##
## Can the decomposition tell one parameter from the next, and at what N?   ##
##############################################################################
#
# Usage:
#   Rscript scripts/test_sobol_separation.R \
#     --cache outputs/cache/sobol_r20/points.csv \
#     --params pri1_surg_prob,mass_casualty_rate,... \
#     --output outputs
#
# Why this exists, and why it is not answered by the reported intervals.
#
# A decomposition is usually read for a ranking, and a ranking is a set of
# claims about differences: that the leading parameter outranks the second,
# that the second outranks the group beneath it. The obvious way to check such
# a claim is to look at whether the two reported confidence intervals overlap.
# That test is both wrong and needlessly expensive here. Two total-order
# indices from a pick-freeze design are estimated from the same model
# evaluations and are strongly positively correlated, so the error in their
# difference is much smaller than the errors in each. Requiring their
# intervals to separate demands a sample far larger than establishing the
# difference does, and can call a well-established ordering unresolved.
#
# The difference is therefore estimated directly, by bootstrapping the design
# rather than the indices. Each replicate resamples the N base rows and
# carries that same resample through every block of the pick-freeze layout,
# preserving the pairing the estimator depends on, then recomputes the whole
# index vector. Differences are taken within a replicate, so their correlation
# is retained rather than assumed away.
#
# The script also reports what a larger run would buy. The standard error of a
# difference falls as 1/sqrt(N), so for any separation that has not been
# established the sample size that would establish it follows directly, and
# is reported per comparison. That figure is what turns "run a bigger
# decomposition" into a number.
#
# It costs no simulation. Everything here is recomputed from responses the
# completed run already cached.

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
NBOOT   <- as.integer(arg_value("--nboot", 2000L))
SEED    <- as.integer(arg_value("--seed", 20250819L))
#' The group the caller is content to leave unordered
#'
#' @details Separations inside it are reported but not counted as targets, so the
#'   sample size the script recommends is the one the caller's actual reading
#'   requires.
GROUP_FROM <- as.integer(arg_value("--group-from", 3L))

if (is.null(CACHE) || !file.exists(CACHE)) stop("--cache must name an existing cache CSV")
if (length(PARAMS) == 0 || !nzchar(PARAMS[1])) stop("--params must list the decomposed parameters")

dir.create(OUTPUT, recursive = TRUE, showWarnings = FALSE)

tab <- utils::read.csv(CACHE, stringsAsFactors = FALSE)
tab <- tab[order(tab$i), , drop = FALSE]

p <- length(PARAMS)
if (nrow(tab) %% (p + 2L) != 0L) stop("cache size is not N * (p + 2) for the given parameters")
if (!identical(tab$i, seq_len(nrow(tab)))) stop("cache indices are not a complete run of 1..n")
N <- nrow(tab) %/% (p + 2L)

responses <- setdiff(names(tab), c("i", grep("^sd_", names(tab), value = TRUE)))

#' Build one half of the shape-only design matrix
#'
#' @return A data frame of the design's shape, every cell at 0.5.
mk <- function() { d <- as.data.frame(matrix(0.5, nrow = N, ncol = p)); names(d) <- PARAMS; d }

#' Total-order indices for one response vector, at the fixed design layout
#'
#' @param y The response vector, in design-point order.
#' @return A numeric vector of one total-order index per parameter, all NA
#'   where the estimator could not be told this response.
st_of <- function(y) {
  sb <- sobol2007(model = NULL, X1 = mk(), X2 = mk(), nboot = 0)
  ok <- tryCatch({ tell(sb, y); TRUE }, error = function(e) FALSE)
  if (!ok || length(sb$T$original) != p) return(rep(NA_real_, p))
  as.numeric(sb$T$original)
}

#' One bootstrap replicate's response vector
#'
#' The same resampled base rows are taken from every block of the layout, so
#' a replicate is a resample of the design rather than of the responses, and
#' the pick-freeze pairing survives it.
#'
#' @param y The response vector, in design-point order.
#' @param idx Indices of the base rows this replicate resamples.
#' @return The resampled response vector, in the same design-point order.
resample_y <- function(y, idx) {
  as.numeric(vapply(seq_len(p + 2L), function(b) y[(b - 1L) * N + idx],
                    numeric(N)))
}

set.seed(SEED)

rows <- list()

for (resp in responses) {
  y <- tab[[resp]]
  if (sum(is.finite(y)) < nrow(tab)) {
    warning(sprintf("Skipping %s: %d non-finite points", resp, sum(!is.finite(y))),
            call. = FALSE)
    next
  }

  st <- st_of(y)
  if (all(is.na(st))) { warning(sprintf("Skipping %s: no indices", resp), call. = FALSE); next }
  names(st) <- PARAMS
  ord <- order(-st)

  boot_st <- matrix(NA_real_, nrow = NBOOT, ncol = p,
                    dimnames = list(NULL, PARAMS))
  for (b in seq_len(NBOOT)) {
    boot_st[b, ] <- st_of(resample_y(y, sample.int(N, N, replace = TRUE)))
  }

  cat(sprintf("\n== %s ==\n", resp))
  cat(sprintf("  ST ranking: %s\n",
              paste(sprintf("%s %.3f", PARAMS[ord], st[ord]), collapse = " > ")))

  # Adjacent comparisons down the ranking, plus each parameter against zero.
  for (k in seq_len(p - 1L)) {
    a <- PARAMS[ord[k]]
    bb <- PARAMS[ord[k + 1L]]
    d  <- boot_st[, a] - boot_st[, bb]
    d  <- d[is.finite(d)]
    if (length(d) < 100L) next
    est <- st[[a]] - st[[bb]]
    ci  <- stats::quantile(d, c(0.025, 0.975), names = FALSE)
    se  <- stats::sd(d)
    pgt <- mean(d > 0)
    sep <- ci[1] > 0

    # The sample that would establish this difference. The standard error of
    # a difference falls as 1/sqrt(N), so separation at 95% needs an SE of
    # about est/1.96, and N scales as the square of the ratio.
    n_need <- if (sep || !is.finite(est) || est <= 0) NA_real_ else
      ceiling(N * (se / (est / 1.96))^2)

    # A comparison inside the group the caller leaves unordered is reported
    # but not treated as something the sample must achieve.
    in_group <- (k + 1L) > GROUP_FROM

    rows[[length(rows) + 1L]] <- data.frame(
      response = resp, comparison = sprintf("%s - %s", a, bb),
      rank_a = k, rank_b = k + 1L, difference = est,
      lower = ci[1], upper = ci[2], se = se, p_a_gt_b = pgt,
      separated = sep, within_unordered_group = in_group,
      n_for_separation = n_need, stringsAsFactors = FALSE
    )

    cat(sprintf("  rank %d vs %d  %-46s d = %+.3f [%+.3f, %+.3f]  P(>0) = %.3f  %s%s\n",
                k, k + 1L, sprintf("%s - %s", a, bb), est, ci[1], ci[2], pgt,
                if (sep) "separated" else "not separated",
                if (!sep && !in_group && is.finite(n_need))
                  sprintf(" (needs N ~ %s)", format(n_need, big.mark = ",")) else
                if (in_group) " [inside unordered group]" else ""))
  }

  for (k in seq_len(p)) {
    a  <- PARAMS[ord[k]]
    d  <- boot_st[, a]
    d  <- d[is.finite(d)]
    if (length(d) < 100L) next
    ci <- stats::quantile(d, c(0.025, 0.975), names = FALSE)
    rows[[length(rows) + 1L]] <- data.frame(
      response = resp, comparison = sprintf("%s - zero", a),
      rank_a = k, rank_b = NA_integer_, difference = st[[a]],
      lower = ci[1], upper = ci[2], se = stats::sd(d), p_a_gt_b = mean(d > 0),
      separated = ci[1] > 0, within_unordered_group = FALSE,
      n_for_separation = NA_real_, stringsAsFactors = FALSE
    )
  }
}

if (length(rows) == 0L) stop("no response produced a usable comparison")

out <- do.call(rbind, rows)
path <- file.path(OUTPUT, "sobol_separation.csv")
utils::write.csv(out, path, row.names = FALSE)
cat(sprintf("\nSeparation tests written to %s\n", path))

# ── What the caller's reading requires ──────────────────────────────────────

targets <- out[!is.na(out$rank_b) & !out$within_unordered_group, , drop = FALSE]
unmet   <- targets[!targets$separated & is.finite(targets$n_for_separation), , drop = FALSE]

cat(sprintf("\nAt N = %d, %d of %d required separations hold.\n",
            N, sum(targets$separated), nrow(targets)))
if (nrow(unmet)) {
  need <- max(unmet$n_for_separation)
  cat("Not established at this sample:\n")
  for (k in seq_len(nrow(unmet))) {
    cat(sprintf("  %-16s %-46s needs N ~ %s\n", unmet$response[k], unmet$comparison[k],
                format(unmet$n_for_separation[k], big.mark = ",")))
  }
  cat(sprintf("\nThe binding requirement is N ~ %s, %.1fx the current sample.\n",
              format(need, big.mark = ","), need / N))
  cat("A noise floor that is not negligible raises this further, since it caps\n")
  cat("the difference a larger design can resolve; see scripts/measure_noise_floor.R.\n")
} else {
  cat("Every separation the reading requires already holds at this sample.\n")
}
