##############################################
## R/figures.R                              ##
## Result figures drawn from tracked data   ##
##############################################
#
# Every function here takes a data frame read from a tracked evidence set under
# data/ and returns a ggplot object; none runs the model. That is what lets
# scripts/render_sweep_figures.R regenerate a figure from the repository alone,
# and what lets a change to a title, label or layout cost seconds rather than
# the hours of compute the measurement took. ggplot2 and dplyr only, and
# independent of every other module under R/, so a regression check can source
# it without simmer.

library(dplyr)
library(ggplot2)

source("R/constants.R")

#' Fill of every interval ribbon
FIGURE_INTERVAL_FILL <- "steelblue"

#' Colour of every mean line and point
FIGURE_MEAN_COLOUR <- "steelblue4"

#' Colour of the dashed reference line marking the shipped value
FIGURE_SHIPPED_COLOUR <- "firebrick"

#' Responses of the policy, establishment and saturation figures
#'
#' @details Ten responses in plotting order, each with the label its panel
#'   carries and the factor that puts the stored value on the scale the label
#'   names. Occupancy and shares are stored as fractions and drawn as
#'   percentages, as the tables of docs/Results.md print them.
POLICY_PANELS <- data.frame(
  response = c("total_rtd", "total_dow", "in_theatre_share", "hold_mean_queue",
               "icu_mean_queue", "role4_peak", "hold_occupancy", "icu_occupancy",
               "post_definitive_icu_share", "mean_evac_wait_days"),
  label    = c("Returns to duty", "Died of wounds", "In-theatre share (%)",
               "R2E hold mean queue", "R2E ICU mean queue", "Role 4 peak beds",
               "R2E hold occupancy (%)", "R2E ICU occupancy (%)",
               "Post-definitive ICU access (%)", "Mean evacuation wait (d)"),
  scale    = c(1, 1, 100, 1, 1, 1, 100, 100, 100, 1),
  stringsAsFactors = FALSE
)

#' Number of leading POLICY_PANELS rows each lever figure draws
#'
#' @details Six for the evacuation policy, and the same six for the establishment
#'   and saturation sweeps so that the three lever figures read against one another.
FIGURE_POLICY_PANEL_COUNT <- 6L

#' Attach a panel label and display scale to a long sweep summary
#'
#' @param summary_df Long summary with columns `response`, `mean`, `ci_lower`
#'   and `ci_upper`, as the policy, establishment and saturation sweeps write.
#' @param panels Data frame of `response`, `label` and `scale`, in plotting order.
#' @return `summary_df` restricted to the panel responses, with the three value
#'   columns rescaled and `panel` a factor in panel order.
#'
#' @details A response absent from `summary_df` stops the call rather than
#'   leaving an empty panel, so a renamed response cannot silently shrink a figure.
label_panels <- function(summary_df, panels) {
  missing <- setdiff(panels$response, summary_df$response)
  if (length(missing) > 0) {
    stop("summary lacks response(s) the figure draws: ", paste(missing, collapse = ", "))
  }
  summary_df %>%
    inner_join(panels, by = "response") %>%
    mutate(
      mean      = mean * scale,
      ci_lower  = ci_lower * scale,
      ci_upper  = ci_upper * scale,
      panel     = factor(label, levels = panels$label)
    )
}

#' Draw one lever's sweep as faceted panels with 95% intervals
#'
#' @param plot_df Output of `label_panels()` with an `x` column holding the
#'   swept value.
#' @param shipped Swept value of the shipped configuration, or NULL for none.
#' @param x_label Axis title naming the lever and its unit.
#' @param title Figure title.
#' @param subtitle Figure subtitle, or NULL.
#' @return ggplot object.
#'
#' @details The shipped value is a dashed reference line on every panel, so the
#'   cost of departing from the shipped configuration reads off the figure. Each
#'   panel has its own vertical scale, the responses sharing no unit.
sweep_panels <- function(plot_df, shipped, x_label, title, subtitle = NULL) {
  p <- ggplot(plot_df, aes(x = x, y = mean)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% confidence interval"),
                alpha = 0.3) +
    geom_line(aes(color = "Mean across replications"), linewidth = 1) +
    geom_point(aes(color = "Mean across replications"), size = 2) +
    facet_wrap(~ panel, scales = "free_y", ncol = 2) +
    scale_fill_manual(name = NULL, values = c("95% confidence interval" = FIGURE_INTERVAL_FILL)) +
    scale_color_manual(name = NULL, values = c("Mean across replications" = FIGURE_MEAN_COLOUR)) +
    labs(title = title, subtitle = subtitle, x = x_label, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
          legend.position = "bottom")
  if (!is.null(shipped)) {
    p <- p + geom_vline(aes(xintercept = shipped, linetype = "Shipped value"),
                        color = FIGURE_SHIPPED_COLOUR, linewidth = 0.6) +
      scale_linetype_manual(name = NULL, values = c("Shipped value" = "dashed"))
  }
  p
}

#' Evacuation policy figure
#'
#' @param summary_df Tracked `data/policy/policy_sweep.csv`.
#' @param shipped_days Shipped evacuation policy in days.
#' @return ggplot object of six responses against policy days.
plot_policy_sweep <- function(summary_df, shipped_days) {
  plot_df <- summary_df %>%
    label_panels(POLICY_PANELS[seq_len(FIGURE_POLICY_PANEL_COUNT), ]) %>%
    mutate(x = policy_days)
  n_reps <- unique(plot_df$n_reps)
  sweep_panels(plot_df, shipped_days, "Evacuation policy (days recoverable in theatre)",
               "Evacuation Policy Sweep at the Sustained Horizon",
               sprintf("%d replications per policy over 360 days", n_reps))
}

#' R2E holding establishment figure
#'
#' @param summary_df Tracked `data/policy/establishment_sweep.csv`.
#' @param shipped_beds Shipped R2E holding bed establishment.
#' @return ggplot object of six responses against holding beds at the shipped policy.
plot_establishment_sweep <- function(summary_df, shipped_beds) {
  plot_df <- summary_df %>%
    label_panels(POLICY_PANELS[seq_len(FIGURE_POLICY_PANEL_COUNT), ]) %>%
    mutate(x = hold_beds)
  sweep_panels(plot_df, shipped_beds, "R2E holding beds per unit",
               "R2E Holding Establishment Sweep at the Shipped Evacuation Policy",
               sprintf("%d replications per establishment over 360 days", unique(plot_df$n_reps)))
}

#' Forward surgical saturation release figure
#'
#' @param summary_df Tracked `data/policy/saturation_sweep.csv`.
#' @param shipped_threshold Shipped saturation queue threshold.
#' @return ggplot object of six responses against the release threshold.
plot_saturation_sweep <- function(summary_df, shipped_threshold) {
  plot_df <- summary_df %>%
    label_panels(POLICY_PANELS[seq_len(FIGURE_POLICY_PANEL_COUNT), ]) %>%
    mutate(x = saturation_threshold)
  sweep_panels(plot_df, shipped_threshold, "R2E theatre queue at which a casualty is released",
               "Forward Surgical Saturation Release Threshold Sweep",
               sprintf("%d replications per threshold over 360 days", unique(plot_df$n_reps)))
}

#' Responses of the post-operative intensive care gate figure
#'
#' @details The pathway counts are the casualties each post-operative route
#'   received, so the figure shows what the gate redirected as well as its cost.
ICU_GATE_PANELS <- data.frame(
  response = c("icu_occupancy", "total_dow", "icu_pathway_n", "hold_pathway_n"),
  label    = c("R2E ICU occupancy (%)", "Died of wounds",
               "Casualties recovering in ICU", "Casualties recovering in a holding bed"),
  scale    = c(100, 1, 1, 1),
  stringsAsFactors = FALSE
)

#' Post-operative intensive care gate figure
#'
#' @param summary_df Tracked `data/icu_gate/icu_gate_summary.csv`.
#' @return ggplot object comparing the gate disabled with the shipped gate.
#'
#' @details Two arms rather than a swept lever, so each panel is a pair of
#'   points with 95% intervals instead of a line.
plot_icu_gate <- function(summary_df) {
  plot_df <- summary_df %>%
    label_panels(ICU_GATE_PANELS) %>%
    mutate(arm = factor(gate_enabled, levels = c(0, 1),
                        labels = c("Gate disabled", "Gate enabled (shipped)")))
  ggplot(plot_df, aes(x = arm, y = mean)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15,
                  color = FIGURE_MEAN_COLOUR) +
    geom_point(size = 3, color = FIGURE_MEAN_COLOUR) +
    facet_wrap(~ panel, scales = "free_y", ncol = 2) +
    labs(title = "Post-Operative Intensive Care Gate",
         subtitle = sprintf("%d replications per arm over 360 days; bars are 95%% intervals",
                            unique(plot_df$n_reps)),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
}

#' Responses of the strategic airlift sweep figure, in plotting order
AIRLIFT_PANELS <- data.frame(
  response = c("mean_wait_days", "queued_at_end", "role4_peak", "hold_evac_share"),
  label    = c("Mean wait for a sortie (d)", "Awaiting a sortie at the horizon",
               "Role 4 peak beds", "Holding bed-days awaiting a sortie (%)"),
  scale    = c(1, 1, 1, 100),
  stringsAsFactors = FALSE
)

#' Strategic airlift sweep figure
#'
#' @param summary_df Tracked `data/airlift/airlift_summary.csv`.
#' @param shipped Named list with `failure_probability` and `interval_days`,
#'   the shipped values marked on each sweep.
#' @return ggplot object, one column per sweep and one row per response.
#'
#' @details The two sweeps share the responses but not the axis, so each is
#'   drawn in its own column with a free horizontal scale.
plot_airlift_sweeps <- function(summary_df, shipped) {
  sweeps <- data.frame(
    arm    = c("reliability", "interval"),
    column = c("Sortie cancellation probability", "Days between sorties"),
    shipped = c(shipped$failure_probability, shipped$interval_days),
    stringsAsFactors = FALSE
  )
  plot_df <- summary_df %>%
    filter(arm %in% sweeps$arm) %>%
    rename(x = value) %>%
    label_panels(AIRLIFT_PANELS) %>%
    inner_join(sweeps, by = "arm") %>%
    mutate(column = factor(column, levels = sweeps$column))
  shipped_df <- sweeps %>% mutate(column = factor(column, levels = sweeps$column))
  ggplot(plot_df, aes(x = x, y = mean)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = FIGURE_INTERVAL_FILL, alpha = 0.3) +
    geom_line(color = FIGURE_MEAN_COLOUR, linewidth = 1) +
    geom_point(color = FIGURE_MEAN_COLOUR, size = 2) +
    geom_vline(data = shipped_df, aes(xintercept = shipped), color = FIGURE_SHIPPED_COLOUR,
               linetype = "dashed", linewidth = 0.6) +
    facet_grid(panel ~ column, scales = "free", switch = "y",
               labeller = labeller(panel = label_wrap_gen(22))) +
    labs(title = "Strategic Airlift: Sortie Reliability and Interval",
         subtitle = sprintf(paste("%d replications per point over 360 days; bands are 95%%",
                                  "intervals; dashed line marks the shipped value"),
                            unique(plot_df$n_reps)),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
          strip.placement = "outside")
}

#' Strategic airlift collapse figure
#'
#' @param collapse_df Tracked `data/airlift/airlift_collapse.csv`.
#' @return ggplot object of the share of campaigns collapsing against the
#'   sortie cancellation probability, with exact binomial 95% intervals.
plot_airlift_collapse <- function(collapse_df) {
  ggplot(collapse_df, aes(x = probability, y = rate * 100)) +
    geom_ribbon(aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
                fill = FIGURE_INTERVAL_FILL, alpha = 0.3) +
    geom_line(color = FIGURE_MEAN_COLOUR, linewidth = 1) +
    geom_point(color = FIGURE_MEAN_COLOUR, size = 2) +
    labs(title = "Campaigns Collapsing under Sortie Cancellation",
         subtitle = sprintf(paste("A campaign collapses where its closing R2E holding queue",
                                  "averages 20 or more; %d replications per point;",
                                  "exact binomial 95%% intervals"), unique(collapse_df$n_reps)),
         x = "Sortie cancellation probability", y = "Campaigns collapsing (%)") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())
}

#' Sustained operations block means figure
#'
#' @param blocks Tracked `data/long_horizon/long_horizon_blocks.csv`.
#' @param stability Tracked `data/long_horizon/long_horizon_stability.csv`.
#' @return ggplot object, one panel per response with a line per intensity,
#'   each panel's strip naming the stability classification of both intensities.
#'
#' @details The classification is read from the tracked stability table rather
#'   than recomputed, so the figure and the table of `docs/Results.md` that
#'   prints it cannot disagree.
plot_long_horizon_blocks <- function(blocks, stability) {
  intensity <- c(moderate_intensity = "Moderate", high_intensity = "High")
  tags <- stability %>%
    mutate(key = paste(series, subject), tag = paste0(intensity[scenario], ": ", stability)) %>%
    group_by(key) %>%
    summarise(tag = paste(tag, collapse = "; "), .groups = "drop")
  plot_df <- blocks %>%
    mutate(key = paste(series, subject),
           scenario_label = factor(intensity[scenario], levels = intensity)) %>%
    inner_join(tags, by = "key") %>%
    mutate(panel = paste0(subject, " (", series, ")\n", tag))
  ggplot(plot_df, aes(x = block_start_day, y = mean, color = scenario_label,
                      fill = scenario_label)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    facet_wrap(~ panel, scales = "free_y", ncol = 3) +
    labs(title = "Sustained Operations: 30-Day Block Means over a 360-Day Campaign",
         subtitle = sprintf("%d replications per intensity; bands are 95%% intervals",
                            max(plot_df$n_reps)),
         x = "Campaign day (start of block)", y = NULL, color = "Casualty intensity",
         fill = "Casualty intensity") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(size = 8),
          legend.position = "bottom")
}

#' Casualty surge event size figure
#'
#' @param size_df Tracked `data/casualty_surge/casualty_surge_size_summary.csv`.
#' @return ggplot object of the died-of-wounds rate of event and ordinary
#'   casualties against the configured event size, with 95% intervals.
#'
#' @details The no-event arm (size 0) has no event casualties, so only its
#'   ordinary-casualty point is drawn.
plot_casualty_surge_size <- function(size_df) {
  plot_df <- bind_rows(
    size_df %>% transmute(size, origin = "Event casualties", rate = dow_event_rate * 100,
                          lower = dow_event_lower * 100, upper = dow_event_upper * 100),
    size_df %>% transmute(size, origin = "Ordinary casualties", rate = dow_ordinary_rate * 100,
                          lower = dow_ordinary_lower * 100, upper = dow_ordinary_upper * 100)
  ) %>%
    filter(!is.na(rate))
  ggplot(plot_df, aes(x = size, y = rate, color = origin, fill = origin)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(title = "Died-of-Wounds Rate against Casualty Surge Event Size",
         subtitle = sprintf(paste("%d replications per size over 360 days;",
                                  "pooled exact binomial 95%% intervals"), max(size_df$n_reps)),
         x = "Casualties per event (0 = no events injected)", y = "Died of wounds (%)",
         color = NULL, fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")
}

#' Horizontal axis breaks of the casualty surge timeline
#'
#' @param n_sim_days Length of the campaign in days.
#' @return Every second day for a campaign of 30 days or fewer, else ggplot's
#'   default breaks, which a campaign of a year would otherwise crowd.
timeline_breaks <- function(n_sim_days) {
  if (n_sim_days <= 30) seq(0, n_sim_days, by = 2) else waiver()
}

#' Casualty surge event timeline figure
#'
#' @param events Event summary with `event_start` (minutes), `n_cas` and, for
#'   several replications, `replication`.
#' @param n_sim_days Length of the campaign in days, bounding the horizontal axis.
#' @return ggplot object of each event's casualty count at its start day.
#'
#' @details Shared by the analysis pipeline, which draws it from a run it has
#'   just reconstructed, and by `scripts/render_sweep_figures.R`, which draws it
#'   from the tracked event summary.
plot_casualty_surge_timeline <- function(events, n_sim_days) {
  p <- ggplot(events, aes(x = event_start / DAY_MIN, y = n_cas)) +
    geom_segment(aes(xend = event_start / DAY_MIN, y = 0, yend = n_cas), color = "#D62828") +
    geom_point(size = 3, color = "#D62828") +
    scale_x_continuous(limits = c(0, n_sim_days), breaks = timeline_breaks(n_sim_days)) +
    labs(
      title    = "Casualty Surge Event Timeline",
      subtitle = sprintf("%d event(s) across the simulation period (compound Poisson injection)",
                         nrow(events)),
      x = "Simulation Day", y = "Casualties Injected by Event"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())
  if (dplyr::n_distinct(events$replication) > 1) {
    p <- p + facet_wrap(~ replication, ncol = 1)
  }
  p
}

#' Display names of the Role 4 wards the census figure draws
#'
#' @details Keyed by the ward level `env_data.json` names, so a ward the figure
#'   has no name for is drawn under its own level rather than dropped.
ROLE4_FIGURE_WARDS <- c(icu = "Intensive care phase", hold = "Step-down ward phase")

#' Role 4 census over time figure
#'
#' @param daily_df Tracked `data/role4_demand/role4_demand_daily.csv`.
#' @return ggplot object of the daily census against campaign day, one column
#'   per casualty intensity and two rows, the census divided by ward phase and
#'   by origin, each beside the total it sums to.
#'
#' @details The line is the mean across replications and the band its 95%
#'   interval. The total is drawn in both rows so that each composition reads
#'   against the quantity it divides rather than against an unlabelled sum.
plot_role4_census_over_time <- function(daily_df) {
  intensity <- c(moderate_intensity = "Moderate intensity", high_intensity = "High intensity")
  origins <- c("Battle injury", "Disease and non-battle injury", "Reconstruction cohort")
  wards <- ifelse(daily_df$subject %in% names(ROLE4_FIGURE_WARDS),
                  ROLE4_FIGURE_WARDS[daily_df$subject], daily_df$subject)
  daily_df$subject <- unname(wards)
  by_ward <- daily_df %>% filter(subject %in% c(unname(ROLE4_FIGURE_WARDS), "Total")) %>%
    mutate(composition = "By ward phase")
  by_origin <- daily_df %>% filter(subject %in% c(origins, "Total")) %>%
    mutate(composition = "By origin")
  plot_df <- bind_rows(by_ward, by_origin) %>%
    mutate(scenario = factor(intensity[scenario], levels = intensity),
           composition = factor(composition, levels = c("By ward phase", "By origin")),
           subject = factor(subject, levels = c("Total", unname(ROLE4_FIGURE_WARDS), origins)))
  ggplot(plot_df, aes(x = day, y = mean, colour = subject, fill = subject)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, colour = NA) +
    geom_line(linewidth = 0.8) +
    facet_grid(composition ~ scenario) +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    labs(title = "Role 4 Bed Demand over a 360-Day Campaign",
         subtitle = sprintf("%d replications per intensity; bands are 95%% intervals",
                            max(plot_df$n_reps)),
         x = "Campaign day", y = "Beds occupied (concurrent patients)",
         colour = NULL, fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
          legend.position = "bottom")
}

#' Levers the Role 4 demand figure draws, in plotting order
ROLE4_LEVER_LABELS <- c(
  policy = "Evacuation policy (days recoverable in theatre)",
  establishment = "R2E holding beds per unit",
  saturation = "R2E theatre queue at which a casualty is released",
  cancellation = "Sortie cancellation probability"
)

#' Responses of the Role 4 demand figure, in plotting order
ROLE4_LEVER_RESPONSES <- c(peak = "Peak beds", closing_mean = "Closing 90-day mean beds",
                           operations = "Operations owed")

#' Role 4 demand against each forward lever figure
#'
#' @param policy Tracked `data/policy/policy_sweep.csv`.
#' @param establishment Tracked `data/policy/establishment_sweep.csv`.
#' @param saturation Tracked `data/policy/saturation_sweep.csv`.
#' @param reliability Tracked
#'   `data/role4_demand/role4_demand_reliability_summary.csv`.
#' @param shipped Named list of the shipped value of each lever, with
#'   `policy`, `establishment`, `saturation` and `cancellation`.
#' @return ggplot object, one column per lever and one row per response (peak
#'   beds, closing 90-day mean beds and operations owed), with the shipped value
#'   of each marked by a dashed line.
#'
#' @details The three policy sets carry the peak as `role4_peak`, the closing
#'   90-day mean as `role4_sustained` and the operations owed by the casualties
#'   admitted as `role4_operations`; the reliability set carries the first two
#'   for the census total and the operations as `operations_admitted`. Each is
#'   renamed to a shared response so one figure draws all four levers.
plot_role4_demand_levers <- function(policy, establishment, saturation, reliability, shipped) {
  keep <- c("role4_peak" = "peak", "role4_sustained" = "closing_mean",
            "role4_operations" = "operations")
  #' One lever's evidence set in the figure's shared shape
  #'
  #' @param d Summary of one lever's sweep.
  #' @param x_col Column holding the swept value.
  #' @param lever Key of the lever, one of `ROLE4_LEVER_LABELS`.
  #' @return Data frame of lever, x, response, mean, ci_lower, ci_upper and n_reps.
  from_set <- function(d, x_col, lever) {
    d <- d[d$response %in% names(keep), ]
    data.frame(lever = lever, x = d[[x_col]], response = unname(keep[d$response]),
               mean = d$mean, ci_lower = d$ci_lower, ci_upper = d$ci_upper,
               n_reps = d$n_reps, stringsAsFactors = FALSE)
  }
  census <- reliability[reliability$subject == "Total" &
                          ((reliability$series == "census" &
                              reliability$response %in% c("peak", "closing_mean")) |
                             (reliability$series == "operations_admitted" &
                                reliability$response == "total")), ]
  census$response[census$response == "total"] <- "operations"
  cancel <- data.frame(lever = "cancellation", x = census$failure_probability,
                       response = census$response, mean = census$mean,
                       ci_lower = census$ci_lower, ci_upper = census$ci_upper,
                       n_reps = census$n_reps, stringsAsFactors = FALSE)
  plot_df <- bind_rows(from_set(policy, "policy_days", "policy"),
                       from_set(establishment, "hold_beds", "establishment"),
                       from_set(saturation, "saturation_threshold", "saturation"),
                       cancel) %>%
    mutate(lever = factor(ROLE4_LEVER_LABELS[lever], levels = ROLE4_LEVER_LABELS),
           response = factor(ROLE4_LEVER_RESPONSES[response], levels = ROLE4_LEVER_RESPONSES))
  shipped_df <- data.frame(lever = factor(ROLE4_LEVER_LABELS[names(shipped)],
                                          levels = ROLE4_LEVER_LABELS),
                           x = unlist(shipped), stringsAsFactors = FALSE)
  ggplot(plot_df, aes(x = x, y = mean)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = FIGURE_INTERVAL_FILL, alpha = 0.3) +
    geom_line(color = FIGURE_MEAN_COLOUR, linewidth = 1) +
    geom_point(color = FIGURE_MEAN_COLOUR, size = 2) +
    geom_vline(data = shipped_df, aes(xintercept = x), color = FIGURE_SHIPPED_COLOUR,
               linetype = "dashed", linewidth = 0.6) +
    facet_grid(response ~ lever, scales = "free", switch = "y",
               labeller = labeller(lever = label_wrap_gen(24))) +
    labs(title = "Role 4 Bed Demand against Each Forward Lever",
         subtitle = sprintf(paste("%d replications per point over 360 days; bands are 95%%",
                                  "intervals; dashed line marks the shipped value"),
                            max(plot_df$n_reps)),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
          strip.placement = "outside")
}

#' Role 4 operations owed over time figure
#'
#' @param weekly_df Tracked
#'   `data/role4_demand/role4_demand_operations_weekly.csv`.
#' @return ggplot object of the mean operations owed per day over each week of
#'   the campaign, one column per casualty intensity, by source and in all.
#'
#' @details The line is the mean across replications and the band its 95%
#'   interval. A week is the interval between scheduled sorties, so each point
#'   holds one whole cycle of the evacuation schedule.
plot_role4_operations_over_time <- function(weekly_df) {
  intensity <- c(moderate_intensity = "Moderate intensity", high_intensity = "High intensity")
  levels_subject <- c("Total", "Definitive repair", "Debridement", "Reconstruction")
  plot_df <- weekly_df %>%
    mutate(scenario = factor(intensity[scenario], levels = intensity),
           subject = factor(subject, levels = levels_subject))
  ggplot(plot_df, aes(x = block_start_day, y = mean, colour = subject, fill = subject)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, colour = NA) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~ scenario) +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    labs(title = "Role 4 Operating Theatre Demand over a 360-Day Campaign",
         subtitle = sprintf(paste("Mean operations owed per day over each week; %d replications",
                                  "per intensity; bands are 95%% intervals"),
                            max(plot_df$n_reps)),
         x = "Campaign day (start of week)", y = "Operations owed per day",
         colour = NULL, fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
          legend.position = "bottom")
}
