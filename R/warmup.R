##############################################
## R/warmup.R                               ##
## Welch warm-up period analysis            ##
##############################################

library(dplyr)
library(ggplot2)

# Terminating simulation (Law 2020): finite campaign horizon, no steady state.
# Welch CMA across 10 × 90-day reps shows episodic non-stationary behaviour
# (peaks Days 13, 38; no convergence) — warm-up exclusion is not appropriate.
# WARM_UP_DAYS = 0L (no exclusion). Pass --warm-up N for parametric comparisons.
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
#' @param n_reps      Number of replications used (for subtitle)
#' @param n_days      Total run length in days (for subtitle)
#' @param images_dir  Output directory for the PNG (default "images")
#' @return Invisibly returns the ggplot object
plot_welch <- function(cma_df, warm_up_days, n_reps, n_days,
                       images_dir = "images") {
  dir.create(images_dir, showWarnings = FALSE, recursive = TRUE)

  max_day <- ceiling(max(cma_df$bin_min, na.rm = TRUE) / 1440)
  y_max   <- max(cma_df$cma, na.rm = TRUE)

  p <- ggplot(cma_df, aes(x = bin_min / 1440, y = cma)) +
    geom_line(colour = "steelblue", linewidth = 1)

  if (warm_up_days > 0L) {
    p <- p +
      geom_vline(xintercept = warm_up_days,
                 linetype = "dashed", colour = "firebrick", linewidth = 0.8) +
      annotate("text",
               x     = warm_up_days + 0.3,
               y     = y_max * 0.97,
               label = sprintf("Warm-up: Day %d\n(%d min)", warm_up_days,
                               warm_up_days * 1440L),
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
  plot_welch(cma_df, WARM_UP_DAYS, n_reps = n_reps, n_days = n_days,
             images_dir = images_dir)

  if (WARM_UP_DAYS > 0L) {
    message(sprintf("Warm-up period: %d days (%d minutes)",
                    WARM_UP_DAYS, WARM_UP_DAYS * 1440L))
  } else {
    message("Warm-up exclusion: none (terminating simulation — full window retained)")
  }
  invisible(list(cma = cma_df, warm_up_days = WARM_UP_DAYS))
}
