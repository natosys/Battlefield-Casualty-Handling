##############################################
## R/analysis_time_series.R                 ##
## Campaign time series                     ##
##############################################
#
# The binned queue, queue clearance and degraded post-operative care series
# behind the two campaign time series figures, and the figures themselves.
#
# Sourced by R/analysis.R, which loads the packages and the shared modules
# these functions use; source that file rather than this one alone.

# ── Campaign time series ────────────────────────────────────────
# Every other figure in this pipeline collapses a campaign to a scalar, which
# cannot distinguish a queue that recurs in peaks from one that never clears.
# Those are different resourcing problems: the first is answered by surge
# capability, the second only by establishment. The functions below keep the
# time axis, aggregating across replications rather than across time, and are
# called by scripts/render_time_series_figures.R rather than by
# analyse_replications(), the figures they produce being paper figures rather
# than per-run diagnostics.

#' Resource pools and overflow points the campaign queue series covers
#'
#' @details Each entry maps a panel label to a regular expression matching the
#'   individual monitored resources of one pool. The pools are those that carry
#'   a queue, or for an overflow point a census, at either casualty intensity;
#'   a pool whose queue is zero throughout would contribute an empty panel, which
#'   is why R2E intensive care, which nobody queues for, is absent and its
#'   saturation shows as the post-operative overflow census. The patterns anchor on the bed-index and
#'   team suffixes so that a pool cannot accidentally match another whose name
#'   it is a prefix of. The order is the order a casualty meets the pools, and
#'   is the order the figure's panels are drawn in.
TIME_SERIES_POOLS <- c(
  "R2B reception overflow"      = OVERFLOW_POOLS[["R2B reception overflow"]],
  "R2B holding beds"            = "^b_r2b_hold_[0-9]+_t[0-9]+$",
  "R2E operating theatres"      = "^b_r2eheavy_ot_[0-9]+_t[0-9]+$",
  "R2E post-operative overflow" = OVERFLOW_POOLS[["R2E post-operative overflow"]],
  "R2E holding beds"            = "^b_r2eheavy_hold_[0-9]+_t[0-9]+$",
  "R2E staging overflow"        = OVERFLOW_POOLS[["R2E staging overflow"]]
)

#' The monitor column a pool's series is read from
#'
#' @param pool A panel label.
#' @return "server" for an overflow point, whose occupants are its census, and
#'   "queue" for a bed pool.
series_column <- function(pool) if (pool %in% names(OVERFLOW_POOLS)) "server" else "queue"

#' Width of the bins the queue series is averaged over, in minutes
#'
#' @details Four hours. Narrow enough that a queue clearing overnight is
#'   visible, wide enough that a 30-day campaign is 180 points rather than
#'   tens of thousands. The bin mean is time-weighted, so the width changes
#'   the resolution of the figure and not the area under it.
TIME_SERIES_BIN_MIN <- 240L

#' The two post-operative decisions whose degraded share is tracked over time
#'
#' @details Each names an attribute recording which pathway a casualty took,
#'   1 being the intensive care bed the pathway is designed around, 2 a holding
#'   bed taken when no intensive care bed is free and 3 overflow when no holding
#'   bed is free either; 2 and 3 are both degraded care.
#'   Both are reported because they degrade independently: stabilisation sits
#'   between a damage control casualty's two operations, post-definitive care
#'   after the last one.
PATHWAY_STAGES <- c(
  "Stabilisation"      = "post_op_pathway",
  "Post-definitive care" = "post_definitive_pathway"
)

#' Queue length over time for every pool, one row per pool, replication and bin
#'
#' @param resources_raw Resource-monitor rows as read, carrying a replication
#'   column.
#' @param horizon_min End of the campaign window, in minutes.
#' @param pools Named vector of panel label to resource-name pattern
#'   (default TIME_SERIES_POOLS).
#' @param bin_size_min Bin width in minutes (default TIME_SERIES_BIN_MIN).
#' @return Data frame of pool, replication, bin_start_day and queue.
#'
#' @details A pool a run did not field contributes no rows rather than a row of
#'   zeroes, so a configuration without one of these pools produces a figure
#'   with one panel fewer rather than a flat panel implying it was never busy.
pool_queue_series <- function(resources_raw, horizon_min, pools = TIME_SERIES_POOLS,
                              bin_size_min = TIME_SERIES_BIN_MIN) {
  edges <- seq(0, horizon_min, by = bin_size_min)
  if (edges[length(edges)] < horizon_min) edges <- c(edges, horizon_min)

  bind_rows(lapply(names(pools), function(pool) {
    members <- resources_raw %>% filter(grepl(pool_queue_pattern(pool, pools[[pool]]), resource))
    if (nrow(members) == 0) return(NULL)
    bind_rows(lapply(sort(unique(members$replication)), function(rep_id) {
      rows  <- members %>% filter(replication == rep_id)
      steps <- pool_queue_steps(rows$resource, rows$time, rows[[series_column(pool)]])
      data.frame(
        pool          = pool,
        replication   = rep_id,
        bin_start_day = head(edges, -1) / DAY_MIN,
        queue         = step_bin_means(steps, edges)
      )
    }))
  }))
}

#' Queue clearance statistics for every pool, one row per pool and replication
#'
#' @param resources_raw Resource-monitor rows as read, carrying a replication
#'   column.
#' @param horizon_min End of the campaign window, in minutes.
#' @param pools Named vector of panel label to resource-name pattern
#'   (default TIME_SERIES_POOLS).
#' @return Data frame of pool, replication, zero_share and longest_busy_days.
pool_clearance_series <- function(resources_raw, horizon_min, pools = TIME_SERIES_POOLS) {
  bind_rows(lapply(names(pools), function(pool) {
    members <- resources_raw %>% filter(grepl(pool_queue_pattern(pool, pools[[pool]]), resource))
    if (nrow(members) == 0) return(NULL)
    bind_rows(lapply(sort(unique(members$replication)), function(rep_id) {
      rows  <- members %>% filter(replication == rep_id)
      steps <- pool_queue_steps(rows$resource, rows$time, rows[[series_column(pool)]])
      stats <- step_clearance_stats(steps, horizon_min)
      data.frame(
        pool               = pool,
        replication        = rep_id,
        zero_share         = stats[["zero_share"]],
        longest_busy_days  = stats[["longest_busy_min"]] / DAY_MIN
      )
    }))
  }))
}

#' Degraded-care rate over time, one row per stage, replication and day
#'
#' @param attributes_raw Attribute-monitor rows as read, carrying name, key,
#'   value, time and replication columns.
#' @param horizon_min End of the campaign window, in minutes.
#' @param stages Named vector of stage label to attribute key
#'   (default PATHWAY_STAGES).
#' @return Data frame of stage, replication, day, n_decisions, daily_rate and
#'   cumulative_rate.
#'
#' @details A casualty's pathway is read at the last instant the attribute was
#'   set, which is the decision that stands; the placement sets it inside the
#'   branch that places the stay, and a later step with no stay of its own
#'   keeps the value already set, so an earlier row can restate a decision but
#'   never contradicts the final one. `daily_rate` is NA on a day no casualty reached the
#'   decision, which leaves a gap in the figure rather than drawing a rate of
#'   zero that no casualty supports; `cumulative_rate` carries the last value
#'   forward across such a day, being a running total rather than a daily one.
pathway_degraded_series <- function(attributes_raw, horizon_min, stages = PATHWAY_STAGES) {
  n_days <- ceiling(horizon_min / DAY_MIN)
  bind_rows(lapply(names(stages), function(stage) {
    rows <- attributes_raw %>%
      filter(key == stages[[stage]], time <= horizon_min)
    if (nrow(rows) == 0) return(NULL)
    decisions <- rows %>%
      arrange(replication, name, time) %>%
      group_by(replication, name) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      mutate(day = pmin(floor(time / DAY_MIN) + 1L, n_days),
             degraded = as.integer(value >= 2))
    decisions %>%
      count(replication, day, wt = degraded, name = "n_degraded") %>%
      full_join(count(decisions, replication, day, name = "n_decisions"),
                by = c("replication", "day")) %>%
      complete(replication = sort(unique(decisions$replication)), day = seq_len(n_days),
               fill = list(n_degraded = 0L, n_decisions = 0L)) %>%
      arrange(replication, day) %>%
      group_by(replication) %>%
      mutate(
        daily_rate      = ifelse(n_decisions > 0, n_degraded / n_decisions, NA_real_),
        cumulative_rate = cumsum(n_degraded) / cumsum(n_decisions)
      ) %>%
      ungroup() %>%
      mutate(stage = stage) %>%
      dplyr::select(stage, replication, day, n_decisions, daily_rate, cumulative_rate)
  }))
}

#' Median and interquartile band of one series across replications
#'
#' @param df Series carrying a replication column and the value column.
#' @param group_cols Columns identifying a point on the time axis.
#' @param value_col Name of the column to summarise.
#' @return `df` collapsed to one row per group, with n_reps, median, q25 and
#'   q75 columns.
#'
#' @details The median and quartiles across replications are reported rather
#'   than a mean and a confidence interval, because the question these figures
#'   are drawn to answer is what a campaign looks like rather than how
#'   precisely its average is known. An interval on the mean narrows as
#'   replications are added and would imply agreement between campaigns that
#'   the replications do not show; the quartile band does not move with the
#'   replication count and is the spread itself.
series_quantiles <- function(df, group_cols, value_col) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n_reps = sum(!is.na(.data[[value_col]])),
      median = median(.data[[value_col]], na.rm = TRUE),
      q25    = as.numeric(quantile(.data[[value_col]], 0.25, na.rm = TRUE)),
      q75    = as.numeric(quantile(.data[[value_col]], 0.75, na.rm = TRUE)),
      .groups = "drop"
    )
}

#' Colour per casualty intensity, held constant across both time series figures
#'
#' @details The same two colours scripts/render_paper_figures.R uses, so that a
#'   reader who has met the intensities in one figure of the paper meets them
#'   in the same colours in the next.
TIME_SERIES_INTENSITY_COLOURS <- c(
  "Moderate intensity" = "#1f5566",
  "High intensity"     = "#9c4a35"
)

#' Plot queue length over the campaign, by resource pool and casualty intensity
#'
#' @param queue_ci Queue series summarised by series_quantiles(), carrying
#'   intensity, pool, bin_start_day, median, q25 and q75 columns.
#' @param clearance Clearance statistics summarised per intensity and pool,
#'   carrying intensity, pool and median_zero_share columns; annotated onto
#'   each panel so the figure states whether the queue clears.
#' @param n_reps Replications behind each intensity, named by the subtitle.
#' @return The ggplot object.
plot_queue_series <- function(queue_ci, clearance, n_reps) {
  queue_ci$pool  <- factor(queue_ci$pool, levels = names(TIME_SERIES_POOLS))
  clearance$pool <- factor(clearance$pool, levels = names(TIME_SERIES_POOLS))

  subtitle <- sprintf(paste(
    "%d replications per intensity; line is the median across replications, band the",
    "interquartile range.\nEach point is the time-weighted mean queue, or for an overflow",
    "point the mean census of casualties without a bed, over a %d-hour bin."
  ), n_reps, TIME_SERIES_BIN_MIN %/% 60L)

  labels <- clearance %>%
    mutate(label = sprintf("%s: empty %.0f%% of the campaign",
                           intensity, 100 * median_zero_share)) %>%
    group_by(pool) %>%
    summarise(label = paste(label, collapse = "\n"), .groups = "drop")

  ggplot(queue_ci, aes(x = bin_start_day, colour = intensity, fill = intensity)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.25, colour = NA) +
    geom_line(aes(y = median), linewidth = 0.8) +
    geom_text(data = labels, aes(x = 0, y = Inf, label = label), inherit.aes = FALSE,
              hjust = 0, vjust = 1.2, size = 3.1, lineheight = 1.1, colour = "grey20") +
    facet_wrap(~ pool, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = TIME_SERIES_INTENSITY_COLOURS) +
    scale_fill_manual(values = TIME_SERIES_INTENSITY_COLOURS) +
    expand_limits(y = 0) +
    labs(
      title = "Queue and Overflow Over the Campaign, by Pool and Casualty Intensity",
      subtitle = subtitle,
      x = "Campaign day", y = "Casualties queued, or in overflow",
      colour = NULL, fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", hjust = 0)
    )
}

#' Plot the degraded-care rate over the campaign, by pathway stage and intensity
#'
#' @param daily_ci Daily rate summarised by series_quantiles(), carrying
#'   intensity, stage, day, median, q25 and q75 columns.
#' @param cumulative_ci Cumulative rate summarised the same way.
#' @param n_reps Replications behind each intensity, named by the subtitle.
#' @return The ggplot object.
#'
#' @details Both series are drawn on one pair of axes because they answer
#'   different halves of the same question: the daily rate shows whether the
#'   shortfall is concentrated on particular days, the cumulative rate whether
#'   it worsens as the campaign proceeds. Drawing them apart would make the
#'   reader hold one panel in mind while reading the other.
plot_degraded_care_series <- function(daily_ci, cumulative_ci, n_reps) {
  subtitle <- sprintf(paste(
    "%d replications per intensity; lines are medians across replications, band the",
    "interquartile range of the daily rate.\nThe rate is the share of that stage's",
    "casualties recovering in a holding bed rather than an intensive care bed."
  ), n_reps)

  daily_ci$stage      <- factor(daily_ci$stage, levels = names(PATHWAY_STAGES))
  cumulative_ci$stage <- factor(cumulative_ci$stage, levels = names(PATHWAY_STAGES))

  ggplot(mapping = aes(x = day, colour = intensity, fill = intensity)) +
    geom_ribbon(data = daily_ci, aes(ymin = q25, ymax = q75), alpha = 0.20, colour = NA) +
    geom_line(data = daily_ci, aes(y = median, linetype = "Daily rate"), linewidth = 0.6) +
    geom_line(data = cumulative_ci, aes(y = median, linetype = "Cumulative rate"),
              linewidth = 1.0) +
    facet_wrap(~ stage, ncol = 1) +
    scale_colour_manual(values = TIME_SERIES_INTENSITY_COLOURS) +
    scale_fill_manual(values = TIME_SERIES_INTENSITY_COLOURS) +
    scale_linetype_manual(values = c("Daily rate" = "dotted", "Cumulative rate" = "solid")) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(
      title = "Degraded Post-Operative Care Rate Over the Campaign",
      subtitle = subtitle,
      x = "Campaign day", y = "Share recovering in a holding bed",
      colour = NULL, fill = NULL, linetype = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", hjust = 0)
    )
}
