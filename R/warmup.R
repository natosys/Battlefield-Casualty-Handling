##############################################
## R/warmup.R                               ##
## Welch warm-up period analysis            ##
##############################################

library(dplyr)
library(ggplot2)

source("R/constants.R")

#' Days excluded from the head of a run as warm-up
#'
#' @details Zero, because the model is a terminating simulation (Law, 2020):
#'   a finite campaign horizon with no steady state to warm up into. The
#'   Welch cumulative moving average across ten 90-day replications is
#'   episodically non-stationary, peaking on days 13 and 38 without
#'   converging, so excluding a head would discard campaign behaviour rather
#'   than initialisation bias. Pass --warm-up N for a parametric comparison.
#'   The same diagnostic run at the 360-day sustained-operations horizon
#'   (`compute_long_horizon_cma()`) confirms the classification rather than
#'   revising it: the R2E bed pools' CMA is still moving at day 360 even under
#'   `moderate_intensity`, where the per-block classification in
#'   `R/long_horizon.R` calls the same pools converged, because a cumulative
#'   average carries the empty opening days for the run's whole length while a
#'   block mean does not. Both readings are correct for what each measures;
#'   see docs/Multi_Run_Supplement.md's Warm-up Classification section.
WARM_UP_DAYS <- 0L

#' Bin total ICU queue into regular time intervals using step interpolation
#'
#' @param resources Resource monitor data frame from run_replications()
#' @param bin_size_min Width of each time bin in minutes (default 60)
#' @return Data frame with columns: replication, bin_min, total_queue
bin_icu_queue <- function(resources, bin_size_min = 60) {
  icu_data <- resources %>%
    filter(grepl("^b_r2eheavy_icu_", resource)) %>%
    group_by(replication, time) %>%
    summarise(total_queue = sum(queue), .groups = "drop") %>%
    arrange(replication, time)

  max_time <- max(icu_data$time, na.rm = TRUE)
  bins     <- seq(0, max_time, by = bin_size_min)

  icu_data %>%
    group_by(replication) %>%
    group_modify(function(df, key) {
      q_at_bin <- approx(df$time, df$total_queue,
                         xout = bins, method = "constant", rule = 2)$y
      data.frame(bin_min = bins, total_queue = q_at_bin)
    }) %>%
    ungroup()
}

#' Compute cross-replication cumulative moving average for Welch plot
#'
#' @param binned Data frame as returned by bin_icu_queue()
#' @return Data frame with columns: bin_min, mean_queue, cma
compute_welch_cma <- function(binned) {
  binned %>%
    group_by(bin_min) %>%
    summarise(mean_queue = mean(total_queue, na.rm = TRUE), .groups = "drop") %>%
    arrange(bin_min) %>%
    mutate(cma = cumsum(mean_queue) / seq_along(mean_queue))
}

#' Generate and save the Welch plot
#'
#' @param cma_df      Data frame as returned by compute_welch_cma()
#' @param warm_up_days Identified warm-up period in days
#' @param n_reps      Number of replications that contributed to the curve, for
#'   the subtitle; the realised count rather than the requested one
#' @param n_days      Total run length in days (for subtitle)
#' @param images_dir  Output directory for the PNG (default "images")
#' @return Invisibly returns the ggplot object
plot_welch <- function(cma_df, warm_up_days, n_reps, n_days,
                       images_dir = "images") {
  dir.create(images_dir, showWarnings = FALSE, recursive = TRUE)

  max_day <- ceiling(max(cma_df$bin_min, na.rm = TRUE) / DAY_MIN)
  y_max   <- max(cma_df$cma, na.rm = TRUE)

  p <- ggplot(cma_df, aes(x = bin_min / DAY_MIN, y = cma)) +
    geom_line(colour = "steelblue", linewidth = 1)

  if (warm_up_days > 0L) {
    p <- p +
      geom_vline(xintercept = warm_up_days,
                 linetype = "dashed", colour = "firebrick", linewidth = 0.8) +
      annotate("text",
               x     = warm_up_days + 0.3,
               y     = y_max * 0.97,
               label = sprintf("Warm-up: Day %d\n(%d min)", warm_up_days,
                               warm_up_days * DAY_MIN),
               hjust = 0, vjust = 1,
               colour = "firebrick", size = 3.5)
  }

  p <- p +
    labs(
      title    = "Welch Plot — R2E ICU Queue (Cumulative Moving Average)",
      subtitle = sprintf("%d replications × %d days; bin = 60 min", n_reps, n_days),
      x        = "Simulation Day",
      y        = "CMA of Total ICU Queue Length"
    ) +
    scale_x_continuous(breaks = seq(0, max_day, by = 5), limits = c(0, max_day)) +
    theme_minimal(base_size = 13)

  out_path <- file.path(images_dir, "welch_plot_icu_queue.png")
  ggsave(out_path, p, width = 10, height = 5, dpi = 150)
  message(sprintf("Welch plot saved to %s", out_path))
  invisible(p)
}

#' Run Welch warm-up analysis: replicate, bin ICU queue, plot CMA
#'
#' @param n_reps     Number of replications (default 10)
#' @param n_days     Simulation duration in days (default 90)
#' @param output_dir Output directory for CSV (default "outputs")
#' @param images_dir Output directory for plots (default "images")
#' @return Invisibly returns list(cma, warm_up_days)
run_welch_analysis <- function(n_reps = 10, n_days = 90,
                               output_dir = "outputs", images_dir = "images") {
  message(sprintf("Welch warm-up analysis: %d reps × %d days", n_reps, n_days))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  mon    <- run_replications(n_reps, n_days)
  binned <- bin_icu_queue(mon$resources)
  cma_df <- compute_welch_cma(binned)

  write.csv(cma_df, file.path(output_dir, "welch_cma.csv"), row.names = FALSE)
  # The realised count rather than the requested one: the curve is an average
  # over the replications that contributed, so that is the number its subtitle
  # has to name (Issue #320).
  plot_welch(cma_df, WARM_UP_DAYS, n_reps = mon$n_replications, n_days = n_days,
             images_dir = images_dir)

  if (WARM_UP_DAYS > 0L) {
    message(sprintf("Warm-up period: %d days (%d minutes)",
                    WARM_UP_DAYS, WARM_UP_DAYS * DAY_MIN))
  } else {
    message("Warm-up exclusion: none (terminating simulation — full window retained)")
  }
  invisible(list(cma = cma_df, warm_up_days = WARM_UP_DAYS))
}

#' Bed pools the sustained-horizon Welch diagnostic is computed for
#'
#' @details The two pools `R/long_horizon.R`'s per-block classification finds
#'   slowest to settle at `moderate_intensity` (`R2E intensive care`, the
#'   pool the 90-day diagnostic above already tracks, and `R2E holding
#'   beds`), so the sustained-horizon reading is taken on the responses the
#'   classification itself flags as closest to the boundary rather than on an
#'   arbitrary pair.
LONG_HORIZON_CMA_SUBJECTS <- c("R2E intensive care", "R2E holding beds")

#' Cross-replication cumulative moving average from a long-horizon series
#'
#' @param series Long-horizon daily series as returned by `run_long_horizon()`
#'   (`R/long_horizon.R`) or read back from its tracked
#'   `data/long_horizon/long_horizon_series.csv.gz`: one row per day, series,
#'   subject, replication and scenario.
#' @param subjects Pool names to compute the diagnostic for (default
#'   `LONG_HORIZON_CMA_SUBJECTS`).
#' @return Data frame of day, subject, scenario, mean_queue and cma, ordered
#'   by scenario, subject and day.
#'
#' @details `compute_welch_cma()` above bins one run's minute-resolution
#'   monitor into hourly steps; this instead averages the `mean_queue` series
#'   `run_long_horizon()` already reduced each replication to one value per
#'   day for, so no monitoring data needs to survive the call. The two
#'   estimators answer the same question, whether the cumulative average has
#'   settled, at two different horizons and two different sampling
#'   resolutions, and neither substitutes for the other: a block mean (see
#'   `classify_stability()`, `R/long_horizon.R`) averages each block's days
#'   independently and so forgets the opening transient once it has passed,
#'   while a cumulative average never does, carrying every early day for the
#'   rest of the series. A response can therefore read as converged on the
#'   block classification while its CMA is still moving, which is expected
#'   rather than a contradiction between the two readings.
compute_long_horizon_cma <- function(series, subjects = LONG_HORIZON_CMA_SUBJECTS) {
  queue <- series[series$series == "mean_queue" & series$subject %in% subjects, ]
  daily <- aggregate(value ~ day + subject + scenario, data = queue, FUN = mean)
  names(daily)[names(daily) == "value"] <- "mean_queue"
  daily <- daily[order(daily$scenario, daily$subject, daily$day), ]

  groups <- split(daily, list(daily$subject, daily$scenario), drop = TRUE)
  do.call(rbind, lapply(groups, function(g) {
    g <- g[order(g$day), ]
    g$cma <- cumsum(g$mean_queue) / seq_along(g$mean_queue)
    g
  }))
}

#' Generate and save the sustained-horizon Welch plot
#'
#' @param cma_df Data frame as returned by `compute_long_horizon_cma()`.
#' @param n_reps Number of replications the series was reduced from, for the
#'   subtitle.
#' @param n_days Total run length in days, for the subtitle.
#' @param images_dir Output directory for the PNG (default "images").
#' @return Invisibly returns the ggplot object.
#'
#' @details One panel per scenario, one line per bed pool, so a reader sees
#'   directly that the two casualty intensities this diagnostic covers behave
#'   differently: at `moderate_intensity` both pools' CMAs are still moving at
#'   day 360 but by a small and shrinking amount, while at `high_intensity`
#'   neither pool's queue has a level for the CMA to approach at all, both
#'   growing without bound over the same window (`R/long_horizon.R`'s
#'   `classify_stability()` reports both as drifting).
plot_long_horizon_welch <- function(cma_df, n_reps, n_days, images_dir = "images") {
  dir.create(images_dir, showWarnings = FALSE, recursive = TRUE)

  p <- ggplot(cma_df, aes(x = day, y = cma, colour = subject)) +
    geom_line(linewidth = 1) +
    facet_wrap(~scenario, scales = "free_y", ncol = 1) +
    labs(
      title    = "Welch Plot at the Sustained-Operations Horizon",
      subtitle = sprintf("%d replications x %d days; daily resolution", n_reps, n_days),
      x        = "Simulation Day",
      y        = "CMA of Mean Pool Queue Length",
      colour   = "Bed pool"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")

  out_path <- file.path(images_dir, "welch_plot_long_horizon.png")
  ggsave(out_path, p, width = 10, height = 8, dpi = 150)
  message(sprintf("Sustained-horizon Welch plot saved to %s", out_path))
  invisible(p)
}
