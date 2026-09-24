#!/usr/bin/env Rscript
##############################################################################
## scripts/render_long_horizon_warmup.R                                     ##
## Welch diagnostic at the sustained-operations horizon                     ##
##############################################################################
#
# Usage:
#   Rscript scripts/render_long_horizon_warmup.R --refresh-baseline
#   Rscript scripts/render_long_horizon_warmup.R
#
# Why this exists. The warm-up classification in docs/Multi_Run_Supplement.md
# is re-derived at the sustained-operations horizon by classifying each
# response's 30-day block means (R/long_horizon.R's classify_stability()), not
# by running the Welch graphical diagnostic itself past the 90 days
# scripts/run_warmup.R covers. A block mean forgets the opening transient once
# it has passed; a cumulative moving average never does, carrying every early
# day for the rest of the series. The two can therefore disagree in a
# response's favour: a pool the block classification calls converged can still
# show a moving CMA, which this script exists to show directly rather than
# leave inferred from the block reading alone.
#
# It renders from the tracked data/long_horizon/long_horizon_series.csv.gz
# rather than re-running the model: that series already carries the
# daily, per-replication values the diagnostic needs, at the horizon and
# replication count docs/Multi_Run_Supplement.md's protocol section
# documents, so no simulation is repeated here. --refresh-baseline is the
# only way to write the tracked images/welch_plot_long_horizon.png and
# data/long_horizon/long_horizon_welch_cma.csv, matching the contract every
# other render script in this project carries.

source("R/constants.R")
source("R/warmup.R")

args <- commandArgs(trailingOnly = TRUE)

#' Whether this invocation may write the tracked image and CSV
REFRESH <- "--refresh-baseline" %in% args

#' Tracked long-horizon series this diagnostic is always read from
#'
#' @details Read regardless of --refresh-baseline, so a render-only invocation
#'   reproduces the published diagnostic rather than silently rendering
#'   whatever a previous experimental run happened to leave behind, on the
#'   same convention scripts/render_time_series_figures.R uses for its own
#'   tracked series.
SERIES_PATH <- file.path("data", "long_horizon", "long_horizon_series.csv.gz")

if (!file.exists(SERIES_PATH)) {
  stop(sprintf(paste("no tracked long-horizon series at %s; run",
                     "scripts/run_long_horizon.R --refresh-baseline first"),
               SERIES_PATH), call. = FALSE)
}

series <- read.csv(gzfile(SERIES_PATH), stringsAsFactors = FALSE)
n_reps <- length(unique(series$replication))
n_days <- max(series$day)

cma_df <- compute_long_horizon_cma(series)

#' Directory the CSV is written to
OUTPUT_DIR <- if (REFRESH) file.path("data", "long_horizon") else {
  file.path("outputs", "data", "long_horizon")
}

#' Directory the figure is written to
IMAGES_DIR <- if (REFRESH) "images" else file.path("outputs", "images")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
write.csv(cma_df, file.path(OUTPUT_DIR, "long_horizon_welch_cma.csv"), row.names = FALSE)

plot_long_horizon_welch(cma_df, n_reps = n_reps, n_days = n_days, images_dir = IMAGES_DIR)

cat("\nCMA at day 30 / 90 / 180 / 360, by scenario and pool:\n")
for (sc in unique(cma_df$scenario)) {
  for (su in unique(cma_df$subject)) {
    g <- cma_df[cma_df$scenario == sc & cma_df$subject == su, ]
    #' The CMA at one day within this scenario and pool's series
    #'
    #' @param d Simulation day.
    #' @return The rounded CMA value, or NA where the series carries no such day.
    at <- function(d) if (d %in% g$day) round(g$cma[g$day == d], 3) else NA
    cat(sprintf("%-19s %-19s day30=%7.3f day90=%7.3f day180=%7.3f day360=%7.3f\n",
                sc, su, at(30), at(90), at(180), at(n_days)))
  }
}
