#!/usr/bin/env Rscript
##############################################################################
## scripts/run_airlift_collapse.R                                           ##
## The strategic airlift collapse experiment, at the sustained horizon      ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_airlift_collapse.R --refresh-baseline
#   Rscript scripts/run_airlift_collapse.R --iterations 4 --days 90
#   Rscript scripts/run_airlift_collapse.R --probabilities 0,0.15,0.25
#
# Why this exists. docs/Multi_Run_Supplement.md recorded this as the one
# experiment a reader could not re-execute from a tracked command: it was run
# from a driver script, and neither of the two entry points that later arrived
# could reproduce it alone. scripts/run_long_horizon.R runs a reducing 360-day
# replicated campaign but sweeps nothing; scripts/run_airlift_sweep.R sweeps
# sortie cancellation but at a 30-day horizon, reporting means rather than this
# experiment's response. This script is the two together, plus the response
# itself: whether a campaign collapses, measured as the mean R2E holding queue
# over its closing 90 days against a threshold of twenty casualties.
#
# The response is a classification rather than a mean because the
# per-replication values are bimodal rather than spread, so a mean over them
# would describe no campaign in the population. See R/airlift.R for the
# threshold's basis and docs/Multi_Run_Supplement.md for the design.
#
# --refresh-baseline is the only way to write the tracked data/airlift/ copy.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/long_horizon.R")
source("R/airlift.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--probabilities", type = "character",
              default = paste(AIRLIFT_COLLAPSE_PROBABILITIES, collapse = ","),
              help = "Comma-separated sortie cancellation probabilities [default: %default]"),
  make_option("--iterations", type = "integer", default = AIRLIFT_COLLAPSE_REPLICATIONS,
              help = "Replications per probability [default: %default]"),
  make_option("--days", type = "integer", default = AIRLIFT_COLLAPSE_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--window", type = "integer", default = AIRLIFT_COLLAPSE_WINDOW_DAYS,
              help = "Closing window the response is measured over [default: %default]"),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = 42L,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/airlift/ copy")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (opt$iterations < 1L) {
  stop("--iterations must be at least 1, found ", opt$iterations, call. = FALSE)
}
if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)
if (opt$window < 1L || opt$window > opt$days) {
  stop(sprintf("--window must lie between 1 and --days (%d), found %d", opt$days,
               opt$window), call. = FALSE)
}

probabilities <- as.numeric(trimws(strsplit(opt$probabilities, ",")[[1]]))
if (any(is.na(probabilities)) || any(probabilities < 0 | probabilities > 1)) {
  stop("--probabilities must be numbers between 0 and 1, found '", opt$probabilities, "'",
       call. = FALSE)
}

#' Directory the measurement is written to
OUTPUT_DIR <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "airlift")
} else {
  file.path("outputs", "data", "airlift")
}

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Measure one cancellation probability and return its per-replication response
#'
#' @param probability Sortie cancellation probability to run at.
#' @return The arm's collapse responses, carrying the probability.
#'
#' @details The configuration globals are restored on exit, on the error path as
#'   well as the success path, so an arm that fails part-way leaves the session
#'   as it found it. The control seed is set once per arm, so replication $i$ of
#'   every arm draws the same per-replication seed and the arms are paired.
measure_arm <- function(probability) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  apply_airlift_setting(json_data, opt$scenario, "failure_probability", probability)

  message(sprintf("Cancellation probability %s: %d replications x %d days",
                  format(probability), opt$iterations, opt$days))
  set.seed(opt$seed)
  series <- run_long_horizon(n_iterations = opt$iterations, n_days = opt$days,
                             max_cores = opt$`max-cores`)
  response <- collapse_response(series, opt$days, window_days = opt$window)
  response$probability <- probability

  # The classified pool's own daily series is retained so that a statement about
  # how a campaign's opening relates to its outcome is recomputable from the
  # tracked evidence set rather than only from a run nobody repeats.
  subject <- series[series$series == "mean_queue" &
                      series$subject == AIRLIFT_COLLAPSE_SUBJECT,
                    c("replication", "day", "value")]
  subject$probability <- probability
  list(response = response, series = subject)
}

arms <- lapply(probabilities, measure_arm)
per_replication <- do.call(rbind, lapply(arms, `[[`, "response"))
daily_series <- do.call(rbind, lapply(arms, `[[`, "series"))

summary_rows <- do.call(rbind, lapply(probabilities, function(probability) {
  arm <- per_replication[per_replication$probability == probability, ]
  cbind(data.frame(probability = probability), summarise_collapse(arm))
}))

write.csv(per_replication, file.path(OUTPUT_DIR, "airlift_collapse_replications.csv"),
          row.names = FALSE)
write.csv(summary_rows, file.path(OUTPUT_DIR, "airlift_collapse.csv"), row.names = FALSE)
series_path <- file.path(OUTPUT_DIR, "airlift_collapse_series.csv.gz")
write.csv(daily_series, gzfile(series_path), row.names = FALSE)
message(sprintf("Collapse responses, summary and daily series written to %s", OUTPUT_DIR))

cat("\n| Sortie cancellation | Campaigns collapsed | Rate | 95% CI |",
    "Median holding queue | Worst holding queue |\n")
cat("|---|---|---|---|---|---|\n")
for (i in seq_len(nrow(summary_rows))) {
  r <- summary_rows[i, ]
  cat(sprintf("| %.0f%% | %d of %d | %.1f%% | [%.1f%%, %.1f%%] | %.2f | %.1f |\n",
              100 * r$probability, r$n_collapsed, r$n_reps, 100 * r$rate,
              100 * r$ci_lower, 100 * r$ci_upper, r$median_queue, r$worst_queue))
}
