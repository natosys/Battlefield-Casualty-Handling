#!/usr/bin/env Rscript
##############################################################################
## scripts/measure_noise_floor.R                                            ##
## How much of a Sobol decomposition's variance is replication noise?       ##
##############################################################################
#
# Usage:
#   Rscript scripts/measure_noise_floor.R \
#     --params pri1_surg_prob,mass_casualty_rate,... \
#     --cache outputs/cache/sobol_r20/points.csv \
#     --points 20 --reps 20 --design-reps 4 --days 30 --output outputs
#
# Why this exists, and why it comes before a larger run.
#
# A Sobol estimator treats each design point's response as a fixed number. In
# a stochastic simulation it is not: it is a mean over a handful of
# replications, and it carries the sampling error of that mean. That error is
# not a nuisance the estimator absorbs. It enters the total output variance
# the indices are shares *of*, so every S1 and ST is deflated by the ratio of
# genuine design-driven variance to the total. The pathology this produces is
# already visible in the reported decomposition, where several indices fall
# outside the theoretical [0, 1] range.
#
# The consequence for planning a larger run is the point. Sobol precision
# improves as 1/sqrt(N), so a wider sample narrows the confidence intervals;
# it does not touch the deflation, which is a bias and is set by the
# replication count per point, not by the number of points. A larger run
# against an unmeasured noise floor therefore converges tightly onto a
# systematically understated index. Measuring the floor first is what turns
# "spend three days of compute" into a decision rather than a hope.
#
# The measurement. Replication noise is estimated directly, by evaluating a
# sample of design points at many more replications than the decomposition
# used and taking the spread across those replications. The across-design
# variance is read from the completed decomposition's own cache. Their ratio
# gives the noise share, and one minus it the factor the reported indices are
# deflated by.
#
# Two honest limits on what this returns. The design points are drawn afresh
# from the same bounds rather than re-evaluated from the decomposition's own
# design, which the cache does not record; the within-point variance is
# therefore an average over the design measure, not a point-by-point match,
# which is why its spread is reported and not only its mean. And the
# within-point variance is itself estimated from a finite number of
# replications, so it carries its own error, reported as a confidence
# interval on each estimate.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/sensitivity.R")

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

PARAMS      <- strsplit(arg_value("--params", ""), ",")[[1]]
CACHE       <- arg_value("--cache")
N_POINTS    <- as.integer(arg_value("--points", 20L))
N_REPS      <- as.integer(arg_value("--reps", 20L))
DESIGN_REPS <- as.integer(arg_value("--design-reps", 4L))
N_DAYS      <- as.integer(arg_value("--days", 30L))
OUTPUT      <- arg_value("--output", "outputs")
SEED        <- as.integer(arg_value("--seed", 20250819L))
MAX_CORES   <- {
  v <- arg_value("--max-cores", NA)
  if (is.na(v)) NULL else as.integer(v)
}
POINT_CACHE <- arg_value("--point-cache")

if (length(PARAMS) == 0 || !nzchar(PARAMS[1])) {
  stop("--params must list the decomposed parameters, in the order run_sobol() used")
}
if (is.null(CACHE) || !file.exists(CACHE)) {
  stop("--cache must name the completed decomposition's design point cache")
}

dir.create(OUTPUT, recursive = TRUE, showWarnings = FALSE)
if (!is.null(POINT_CACHE)) {
  dir.create(dirname(POINT_CACHE), recursive = TRUE, showWarnings = FALSE)
}

env_data <<- load_elms("env_data.json")
day_min  <<- 1440L
counts   <<- sapply(env_data$elms, length)
env_data_base <<- env_data

# ── The design points to measure at ─────────────────────────────────────────
#
# Drawn from the same bounds the decomposition sampled, with a composition
# group drawn whole from its Dirichlet rather than coordinate-wise, so the
# sample is from the decomposition's own design measure and not from a box
# that contains it.

p_idx <- which(morris_params$name %in% PARAMS)
if (length(p_idx) != length(PARAMS)) {
  stop(sprintf("parameters not found in morris_params: %s",
               paste(setdiff(PARAMS, morris_params$name), collapse = ", ")))
}
p_def <- morris_params[p_idx, ]

set.seed(SEED)

X <- as.data.frame(mapply(function(lo, hi) runif(N_POINTS, lo, hi),
                          p_def$lower, p_def$upper, SIMPLIFY = FALSE))
names(X) <- p_def$name

for (nm in names(MORRIS_COMPOSITIONS)) {
  g <- MORRIS_COMPOSITIONS[[nm]]
  if (all(g$coords %in% names(X))) {
    message(sprintf("  %s composition drawn whole from its Dirichlet", nm))
    X[, g$coords] <- rdirichlet_coords(N_POINTS, g)
  }
}

full_params <- setNames(morris_params$mode, morris_params$name)

message(sprintf(
  "Noise floor: %d design points x %d replications (%d days each) = %d runs",
  N_POINTS, N_REPS, N_DAYS, N_POINTS * N_REPS
))
message(sprintf(
  "Comparing against a decomposition run at %d replications per point.",
  DESIGN_REPS
))

# ── Within-point standard deviation at each sampled point ───────────────────

sd_rows <- list()

for (i in seq_len(N_POINTS)) {
  if (!is.null(POINT_CACHE) && file.exists(POINT_CACHE)) {
    cached <- cache_lookup(POINT_CACHE, i)
    if (!is.null(cached)) {
      message(sprintf("  Point %d / %d (cached)", i, N_POINTS))
      sd_rows[[length(sd_rows) + 1L]] <- cached
      next
    }
  }
  message(sprintf("  Point %d / %d", i, N_POINTS))
  row <- full_params
  row[p_def$name] <- as.numeric(X[i, ])
  res <- tryCatch({
    kpis <- eval_params(row, N_REPS, N_DAYS, max_cores = MAX_CORES,
                        crn_seed = NULL, return_sd = TRUE)
    sdv <- attr(kpis, "sd")
    stats::setNames(as.numeric(sdv), paste0("sd_", names(sdv)))
  }, error = function(e) {
    warning(sprintf("point %d failed: %s", i, conditionMessage(e)), call. = FALSE)
    NULL
  })
  if (is.null(res)) next
  if (!is.null(POINT_CACHE)) cache_append(POINT_CACHE, i, res)
  sd_rows[[length(sd_rows) + 1L]] <- res
  gc(full = TRUE)
}

env_data <<- env_data_base

if (length(sd_rows) < 2L) stop("too few points evaluated to estimate a noise floor")

common <- Reduce(intersect, lapply(sd_rows, names))
SD <- do.call(rbind, lapply(sd_rows, function(r) r[common]))
message(sprintf("\n%d of %d points evaluated.", nrow(SD), N_POINTS))

# ── Across-design variance, from the completed decomposition's cache ────────

tab <- utils::read.csv(CACHE, stringsAsFactors = FALSE)
responses <- setdiff(names(tab), c("i", grep("^sd_", names(tab), value = TRUE)))

rows <- lapply(responses, function(resp) {
  col <- paste0("sd_", resp)
  if (!(col %in% colnames(SD))) return(NULL)
  sw <- SD[, col]
  sw <- sw[is.finite(sw) & sw >= 0]
  y  <- tab[[resp]]
  y  <- y[is.finite(y)]
  if (length(sw) < 2L || length(y) < 2L) return(NULL)

  # Variance of a single replication at a point, averaged over the design.
  # Averaged as variances rather than as standard deviations, since it is the
  # variance that is additive with the design-driven component.
  var_within <- mean(sw^2)

  # What that leaves in a point estimate the decomposition actually used.
  var_noise  <- var_within / DESIGN_REPS
  var_total  <- stats::var(y)
  var_signal <- var_total - var_noise

  # The estimate's own error. The sampling variance of a variance is
  # 2 sigma^4 / (n - 1) under normality; over the sampled points that gives
  # the interval on var_within, which carries through to the share.
  se_within  <- sqrt(2 / (N_REPS - 1)) * var_within / sqrt(length(sw))
  share      <- var_noise / var_total
  share_lo   <- max(0, (var_within - 1.96 * se_within) / DESIGN_REPS / var_total)
  share_hi   <- (var_within + 1.96 * se_within) / DESIGN_REPS / var_total

  data.frame(
    response          = resp,
    n_points          = length(sw),
    sd_within_mean    = sqrt(var_within),
    sd_within_min     = min(sw),
    sd_within_max     = max(sw),
    var_noise_at_design_reps = var_noise,
    var_across_design = var_total,
    noise_share       = share,
    noise_share_lower = share_lo,
    noise_share_upper = share_hi,
    # Where the measured noise meets or exceeds the observed across-design
    # variance, the design-driven component is not distinguishable from zero
    # and no deflation factor describes it. That is a finding and not an
    # error, so it is named rather than printed as a negative share.
    deflation_factor  = if (share >= 1) NA_real_ else var_signal / var_total,
    reps_for_5pc_share = ceiling(var_within / (0.05 * var_total)),
    note              = if (!is.finite(share)) "response constant across the design"
                        else if (share >= 1) "noise exceeds across-design variance — design-driven component not distinguishable from zero"
                        else "",
    stringsAsFactors  = FALSE
  )
})

out <- do.call(rbind, Filter(Negate(is.null), rows))
if (is.null(out)) stop("no response could be compared against the cache")

path <- file.path(OUTPUT, "sobol_noise_floor.csv")
utils::write.csv(out, path, row.names = FALSE)

cat("\n== Replication noise as a share of total variance ==\n\n")
print(out[, c("response", "n_points", "sd_within_mean", "var_across_design",
              "noise_share", "noise_share_lower", "noise_share_upper",
              "deflation_factor", "reps_for_5pc_share")],
      digits = 3, row.names = FALSE)

flagged <- out[nzchar(out$note), , drop = FALSE]
if (nrow(flagged)) {
  cat("\n")
  for (k in seq_len(nrow(flagged))) {
    cat(sprintf("  %s: %s\n", flagged$response[k], flagged$note[k]))
  }
}

cat("\nReading this table:\n")
cat("  noise_share      the fraction of the variance the indices are shares of\n")
cat("                   that is replication noise rather than parameter effect,\n")
cat(sprintf("                   at the %d replications per point the run used.\n", DESIGN_REPS))
cat("  deflation_factor every reported S1 and ST is multiplied by roughly this.\n")
cat("                   Divide a reported index by it for a noise-corrected value.\n")
cat("  reps_for_5pc_share replications per point that would hold the noise share\n")
cat("                   under 5%, at which point the deflation is negligible.\n")
cat("\nWhat follows for a larger run. Sobol precision improves as 1/sqrt(N), so\n")
cat("more design points narrow the intervals and leave the deflation untouched;\n")
cat("only more replications per point reduce it. Where noise_share is small the\n")
cat("existing indices need no correction and a wider sample is the whole answer.\n")
cat("Where it is large, a wider sample at the same replication count converges\n")
cat("on an understated index, and the replication count must rise with it.\n")

write_screen_metadata(OUTPUT, "noise_floor", list(
  parameters   = PARAMS,
  n_points     = N_POINTS,
  n_reps       = N_REPS,
  design_reps  = DESIGN_REPS,
  n_days       = N_DAYS,
  seed         = SEED,
  cache        = CACHE
))

cat("\nDone.\n")
