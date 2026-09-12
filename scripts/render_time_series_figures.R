#!/usr/bin/env Rscript
##############################################################################
## scripts/render_time_series_figures.R                                     ##
## Campaign time series of queue length and degraded post-operative care    ##
##############################################################################
#
# Usage:
#   Rscript scripts/render_time_series_figures.R                    # render only
#   Rscript scripts/render_time_series_figures.R --run              # re-measure
#   Rscript scripts/render_time_series_figures.R --run --refresh-baseline
#   Rscript scripts/render_time_series_figures.R --run --iterations 10 --days 30
#
# Why this exists. The paper's central claim is that queues grow
# disproportionately to casualty volume, and it rests on one mean queue per
# resource per intensity. A mean cannot distinguish a queue that recurs in
# peaks and clears between them from one that never clears, and those are
# different resourcing problems: the first is answered by surge capability,
# the second only by permanent establishment. The recommendation the paper
# makes assumes the second without establishing it. The same is true of the
# degraded post-operative care rate, reported as a total, which cannot show
# whether the shortfall is concentrated on particular days or worsens as the
# campaign proceeds.
#
# The two halves are separated deliberately. `--run` executes the model at
# both casualty intensities and writes the aggregated series as CSV; an
# ordinary invocation reads those CSVs and renders the figures from them and
# nothing else. A figure is therefore a function of tracked data rather than
# of a run nobody can repeat, and re-rendering without `--run` reproduces both
# images exactly. Only the aggregated series is kept: the monitoring data
# behind it runs to hundreds of megabytes and nothing published derives from
# a single replication of it.
#
# --refresh-baseline is the only way to write the tracked images/ and
# data/time_series/ copies, matching the contract every other render script
# and run.R itself carry: an ordinary run writes under outputs/ and cannot
# disturb tracked evidence.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/scenario_runner.R")
source("R/analysis.R")

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

#' Read one flagged command line argument as a positive whole number
#'
#' @param flag Flag to look for, including its leading dashes.
#' @param default Value returned when the flag is absent.
#' @return The argument following the flag as an integer.
arg_count <- function(flag, default) {
  raw <- arg_value(flag, NULL)
  if (is.null(raw)) return(as.integer(default))
  value <- suppressWarnings(as.integer(raw))
  if (is.na(value) || value < 1L) {
    stop(sprintf("%s must be a whole number of at least 1, found '%s'", flag, raw),
         call. = FALSE)
  }
  value
}

#' Whether this invocation may write the tracked images and series
REFRESH   <- "--refresh-baseline" %in% args

#' Whether this invocation executes the model before rendering
#'
#' @details Without it the figures are rendered from the tracked series and
#'   nothing is simulated, which is what makes an ordinary invocation cheap
#'   and reproducible.
RUN_MODEL <- "--run" %in% args

#' The casualty intensities the figures compare
#'
#' @details Named by the panel label each carries, mapping to the scenario
#'   profile in env_data.json that produces it. The order is the order the
#'   legend and every colour scale places them in.
INTENSITIES <- c(
  "Moderate intensity" = "moderate_intensity",
  "High intensity"     = "high_intensity"
)

#' Replications run per intensity when --run is given
N_ITERATIONS <- arg_count("--iterations", 50L)

#' Campaign length in days
N_DAYS <- arg_count("--days", 30L)

#' Control seed the measurement is drawn under
#'
#' @details Set once before each intensity rather than once for the pair, so
#'   the two arms share their arrival streams and the figure compares the same
#'   campaigns under two configurations rather than two unrelated sets.
SEED <- arg_count("--seed", 42L)

#' Directory the aggregated series is read from and written to
SERIES_DIR <- if (REFRESH) {
  file.path("data", "time_series")
} else {
  file.path("outputs", "data", "time_series")
}

#' Directory the figures are written to
IMAGES_DIR <- if (REFRESH) "images" else file.path("outputs", "images")

#' Directory the tracked series is always read from when not re-measuring
#'
#' @details A render-only invocation reads the tracked series even when it
#'   writes its figures under outputs/, so that an ordinary run reproduces the
#'   published figures rather than silently rendering whatever a previous
#'   experimental run happened to leave behind.
TRACKED_SERIES_DIR <- file.path("data", "time_series")

#' File holding the binned queue series, one row per pool, replication and bin
QUEUE_CSV      <- "queue_series.csv"

#' File holding the clearance statistics, one row per pool and replication
CLEARANCE_CSV  <- "queue_clearance.csv"

#' File holding the degraded-care rates, one row per stage, replication and day
DEGRADED_CSV   <- "degraded_care_series.csv"

#' Measure both series at every intensity and write them as CSV
#'
#' @param series_dir Directory the three CSVs are written to.
#' @return Invisibly, the named list of the three data frames written.
#'
#' @details Each intensity is run under the same control seed, and only the
#'   aggregated per-replication series is retained: the monitoring data is
#'   discarded as each intensity completes, so peak memory is set by one
#'   intensity rather than by all of them.
measure_series <- function(series_dir) {
  horizon_min <- N_DAYS * DAY_MIN
  queue <- list()
  clearance <- list()
  degraded <- list()

  for (label in names(INTENSITIES)) {
    set.seed(SEED)
    result <- run_scenario(INTENSITIES[[label]], n_iterations = N_ITERATIONS,
                           n_days = N_DAYS)
    mon <- result$mon
    queue[[label]] <- pool_queue_series(mon$resources, horizon_min) %>%
      mutate(intensity = label)
    clearance[[label]] <- pool_clearance_series(mon$resources, horizon_min) %>%
      mutate(intensity = label)
    degraded[[label]] <- pathway_degraded_series(mon$attributes, horizon_min) %>%
      mutate(intensity = label)
    rm(mon, result)
    invisible(gc())
  }

  dir.create(series_dir, recursive = TRUE, showWarnings = FALSE)
  # The queue values are counts of casualties averaged over a bin, plotted on
  # axes running to the hundreds, so four decimal places is already far below
  # anything a figure can show and the digits past it are noise the tracked
  # file would carry for nothing. Rounding here rather than at render time
  # keeps the tracked series and the figures rendered from it in agreement.
  out <- list(
    queue     = bind_rows(queue) %>% mutate(queue = round(queue, 4)),
    clearance = bind_rows(clearance),
    degraded  = bind_rows(degraded)
  )
  write.csv(out$queue, file.path(series_dir, QUEUE_CSV), row.names = FALSE)
  write.csv(out$clearance, file.path(series_dir, CLEARANCE_CSV), row.names = FALSE)
  write.csv(out$degraded, file.path(series_dir, DEGRADED_CSV), row.names = FALSE)
  message(sprintf("Series written to %s", series_dir))
  invisible(out)
}

#' Read the three aggregated series back
#'
#' @param series_dir Directory holding the three CSVs.
#' @return Named list of the queue, clearance and degraded data frames.
read_series <- function(series_dir) {
  paths <- file.path(series_dir, c(QUEUE_CSV, CLEARANCE_CSV, DEGRADED_CSV))
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop(sprintf(paste("no measured series at %s (missing %s); run with --run to",
                       "measure it before rendering"),
                 series_dir, paste(basename(missing), collapse = ", ")),
         call. = FALSE)
  }
  lapply(setNames(paths, c("queue", "clearance", "degraded")), read.csv,
         stringsAsFactors = FALSE)
}

#' Order the intensity column so every figure places the two arms alike
#'
#' @param df Data frame carrying an intensity column.
#' @return `df` with intensity as a factor in the order INTENSITIES declares.
order_intensity <- function(df) {
  df$intensity <- factor(df$intensity, levels = names(INTENSITIES))
  df
}

if (RUN_MODEL) {
  measure_series(SERIES_DIR)
}

series <- read_series(if (RUN_MODEL) SERIES_DIR else TRACKED_SERIES_DIR)

n_reps <- max(series$queue$replication)

queue_ci <- series$queue %>%
  order_intensity() %>%
  series_quantiles(c("intensity", "pool", "bin_start_day"), "queue")

clearance_summary <- series$clearance %>%
  order_intensity() %>%
  group_by(intensity, pool) %>%
  summarise(
    median_zero_share        = median(zero_share),
    median_longest_busy_days = median(longest_busy_days),
    .groups = "drop"
  )

degraded <- series$degraded %>% order_intensity()
daily_ci <- series_quantiles(degraded, c("intensity", "stage", "day"), "daily_rate")
cumulative_ci <- series_quantiles(degraded, c("intensity", "stage", "day"),
                                  "cumulative_rate")

dir.create(IMAGES_DIR, recursive = TRUE, showWarnings = FALSE)

p_queue <- plot_queue_series(queue_ci, clearance_summary, n_reps)
ggsave(file.path(IMAGES_DIR, "queue_length_over_time.png"), p_queue,
       width = 10, height = 12, dpi = 150)

p_degraded <- plot_degraded_care_series(daily_ci, cumulative_ci, n_reps)
ggsave(file.path(IMAGES_DIR, "degraded_care_rate_over_time.png"), p_degraded,
       width = 10, height = 8, dpi = 150)

message(sprintf("Figures written to %s", IMAGES_DIR))
print(as.data.frame(clearance_summary), row.names = FALSE)
