##############################################
## R/analysis_run_r1_r2b.R                  ##
## Single run: R1 and R2B                   ##
##############################################
#
# The single-run stages reporting R1 and R2B: their bed queues, the R2B
# holding episodes, occupancy and routing, the theatre bypass reasons and the
# pre-open hold, R2B surgery and the R2B bed gantt.
#
# Sourced by R/analysis.R, which loads the packages and the shared modules
# these functions use; source that file rather than this one alone.

#' Plot each R1 team's treatment queue over time
#'
#' @param resources Resource monitor rows, warm-up excluded.
#' @param images_dir Directory the plots are written to.
#' @return The p_r1_queues object.
plot_r1_queues <- function(resources, images_dir) {
  queue_plot_data_r1 <- resources %>%
    as.data.frame() %>%
    filter(grepl("^c_r1_.*_\\d+_t\\d+$", resource)) %>%
    dplyr::select(time, resource, queue) %>%
    mutate(
      r1_id      = str_extract(resource, "_t\\d+$") %>% str_remove("_t") %>% as.integer(),
      role       = str_extract(resource, "(?<=c_r1_)[^_]+_[^_]+") %>% str_replace_all("_", " ") %>% tools::toTitleCase(),
      role_idx   = str_extract(resource, "(?<=_)[0-9]+(?=_t)") %>% as.integer(),
      r1_label   = paste0("R1 ", r1_id),
      role_label = paste0(role, " ", role_idx)
    )

  max_days_r1 <- ceiling(max(queue_plot_data_r1$time, na.rm = TRUE) / DAY_MIN)

  p_r1_queues <- ggplot(queue_plot_data_r1,
                        aes(x = time / DAY_MIN, y = queue, color = role_label)) +
    geom_step(linewidth = 1) +
    labs(title = "Queue Length Over Time by R1 Team", x = "Time (Days)", y = "Queue Size", color = "Role") +
    scale_x_continuous(breaks = seq(0, max_days_r1, by = 1), limits = c(0, max_days_r1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(~ r1_label, ncol = 1, scales = "free_y") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray"), legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file.path(images_dir, "r1_queues.png"), p_r1_queues,
         width = 12, height = 8, dpi = 150)
  p_r1_queues
}

#' Plot each R2B team's bed queues over time
#'
#' @param resources Resource monitor rows, warm-up excluded.
#' @param images_dir Directory the plots are written to.
#' @return The p_r2b_bed_queues object.
plot_r2b_bed_queues <- function(resources, images_dir) {
  queue_plot_data <- resources %>%
    as.data.frame() %>%
    filter(grepl("^b_r2b_.*_\\d+_t\\d+$", resource)) %>%
    dplyr::select(time, resource, queue) %>%
    mutate(
      r2b_id    = str_extract(resource, "_t\\d+$") %>% str_remove("_t") %>% as.integer(),
      bed_type  = str_extract(resource, "(?<=b_r2b_)[^_]+") %>% toupper(),
      bed_index = str_extract(resource, "(?<=_)[0-9]+(?=_t)") %>% as.integer(),
      r2b_label = paste0("R2B ", r2b_id),
      bed_label = paste0(bed_type, " ", bed_index)
    )

  p_r2b_bed_queues <- ggplot(queue_plot_data,
                             aes(x = time / DAY_MIN, y = queue, color = bed_label)) +
    geom_step(linewidth = 1) +
    labs(title = "Queue Length Over Time by R2B", x = "Time (Days)", y = "Queue Size", color = "Bed") +
    scale_x_continuous(breaks = seq(0, max(queue_plot_data$time) / DAY_MIN, by = 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(~ r2b_label, ncol = 1, scales = "free_x") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray"), legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file.path(images_dir, "r2b_bed_queues.png"), p_r2b_bed_queues,
         width = 12, height = 8, dpi = 150)
  p_r2b_bed_queues
}

#' Hold-bed capacity of each R2B unit, from the establishment
#'
#' @return A data frame of `r2b_label` and `n_hold_beds`, one row per R2B unit.
#' @details Read from the resource identifiers `build_environment()` created
#'   rather than restated here, so a change to the establishment reaches the
#'   capacity line on the occupancy figure without a change to this file. Units
#'   are not assumed to be identical: each contributes its own row, and the
#'   figure draws each unit's line against its own panel.
r2b_hold_capacity <- function() {
  data.frame(
    r2b_label   = paste("R2B", seq_along(env_data$elms$r2b)),
    n_hold_beds = vapply(env_data$elms$r2b, function(u) length(u$hold_bed), integer(1))
  )
}

#' Reconstruct each R2B holding episode from the attributes that bound it
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param window_min End of the observation window, in minutes.
#' @return One row per holding episode, carrying `hold_start_min`,
#'   `hold_end_min` and the `stream` competing for the bed, alongside every
#'   column the caller already held. Empty where no casualty entered holding.
#' @details A casualty leaves R2B holding by one of three routes, and the
#'   attribute that ends the episode differs by route, so reading any single
#'   one of them mis-measures the other two.
#'
#'   A casualty sent on under the evacuation threshold releases the bed at
#'   once and carries `r2b_hold_served`, the bed time actually served. Their
#'   `return_day` is set later and at another echelon, so charging the bed to
#'   it would hold them at R2B for the whole of a stay they spent at R2E.
#'
#'   A casualty who recovers in place sets `return_day` as it releases the
#'   bed, which is the episode's true end.
#'
#'   A casualty still holding a bed when the run ends sets neither, and
#'   occupied the bed for every day of the window from its start. Dropping
#'   them, which reading `return_day` alone does, undercounts the days nearest
#'   the run's end, which is where the unfinished episodes are.
r2b_hold_episodes <- function(attributes_wide, window_min) {
  if (!any(!is.na(attributes_wide$r2b_hold_start))) {
    return(attributes_wide[0, , drop = FALSE] %>%
             mutate(stream = character(0), hold_start_min = numeric(0),
                    hold_end_min = numeric(0)))
  }
  attributes_wide %>%
    filter(!is.na(r2b_hold_start)) %>%
    mutate(
      stream = case_when(
        !is.na(dnbi_type) & dnbi_type == 1L ~ "Battle fatigue",
        !is.na(dnbi_type) & dnbi_type == 2L ~ "Disease DNBI",
        !is.na(dnbi_type) & dnbi_type == 3L ~ "NBI",
        TRUE                                 ~ "WIA"
      ),
      hold_start_min = as.numeric(r2b_hold_start),
      hold_end_min = case_when(
        !is.na(r2b_hold_served) ~ hold_start_min + as.numeric(r2b_hold_served),
        !is.na(return_day)      ~ as.numeric(return_day),
        TRUE                    ~ window_min
      )
    )
}

#' Summarise and plot R2B holding-bed occupancy by patient stream
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `r2b_hold_daily`, `r2b_hold_occupancy_plot`.
#' @details Decomposes daily concurrent occupancy into the streams competing for the
#'   same beds, against the configured per-unit capacity.
summarise_r2b_hold_occupancy <- function(attributes_wide, combined, output_dir, images_dir) {
  r2b_hold_occupancy_plot <- NULL
  r2b_hold_daily          <- NULL

  if (any(!is.na(attributes_wide$r2b_hold_start))) {

    n_sim_days <- ceiling(max(combined$start_time, na.rm = TRUE) / DAY_MIN)
    n_reps_hold <- max(1L, n_distinct(attributes_wide$replication))

    hold_patients <- r2b_hold_episodes(attributes_wide, n_sim_days * DAY_MIN)
    hold_capacity  <- r2b_hold_capacity()

    # Expanded to one row per simulation day the episode touches, so a casualty
    # counts once on a day whatever fraction of it they spent in the bed. The
    # series is therefore a count of casualties occupying a bed during the day
    # rather than a time-weighted mean concurrent occupancy, and reads above the
    # latter; the axis label says which.
    r2b_hold_daily <- hold_patients %>%
      rowwise() %>%
      mutate(
        day = list(
          seq(
            max(1L, floor(hold_start_min / DAY_MIN) + 1L),
            min(n_sim_days, ceiling(hold_end_min / DAY_MIN))
          )
        )
      ) %>%
      unnest(day) %>%
      ungroup() %>%
      mutate(r2b_label = paste("R2B", r2b)) %>%
      group_by(replication, day, stream, r2b_label) %>%
      summarise(occupancy = n(), .groups = "drop") %>%
      group_by(day, stream, r2b_label) %>%
      summarise(mean_occupancy = mean(occupancy), .groups = "drop") %>%
      complete(
        day       = seq_len(n_sim_days),
        stream    = c("Battle fatigue", "Disease DNBI", "NBI", "WIA"),
        r2b_label = hold_capacity$r2b_label,
        fill      = list(mean_occupancy = 0)
      )

    # One panel per R2B unit. A single panel carried the load of both units
    # against a capacity line drawn for one of them, so a bar could sit above
    # the line while neither unit was over its own establishment.
    r2b_hold_occupancy_plot <- ggplot(
      r2b_hold_daily,
      aes(x = day, y = mean_occupancy, fill = stream)
    ) +
      geom_bar(stat = "identity", position = "stack") +
      geom_hline(data = hold_capacity, aes(yintercept = n_hold_beds),
                 linetype = "dashed", color = "red", linewidth = 0.8) +
      facet_wrap(~ r2b_label, ncol = 1) +
      scale_fill_brewer(palette = "Set2") +
      scale_x_continuous(
        breaks = seq(1, n_sim_days, by = 2),
        expand = c(0, 0)
      ) +
      labs(
        title    = "R2B Hold Bed Daily Occupancy by Patient Stream",
        subtitle = sprintf(
          "One panel per R2B unit; dashed line = that unit's holding establishment (%s)",
          paste(sprintf("%s: %d beds", hold_capacity$r2b_label, hold_capacity$n_hold_beds),
                collapse = ", ")
        ),
        x    = "Simulation Day",
        y    = "Casualties Occupying a Hold Bed",
        fill = "Stream"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor  = element_blank(),
        legend.position   = "bottom"
      )

    ggsave(file.path(images_dir, "r2b_hold_occupancy.png"), r2b_hold_occupancy_plot,
           width = 12, height = 6, dpi = 150)

    write.csv(
      r2b_hold_daily,
      file.path(output_dir, "r2b_hold_occupancy.csv"),
      row.names = FALSE
    )
  }
  list(
    r2b_hold_daily = r2b_hold_daily,
    r2b_hold_occupancy_plot = r2b_hold_occupancy_plot
  )
}

#' Count the three R2B holding-bed routing outcomes
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @return A list of `r2b_hold_bypass_count`, `r2b_hold_queued_count`, `r2b_pre_bypass_count`.
#' @details The three counts are independent and do not sum: a casualty bypassed
#'   upstream never reaches the at-R2B decision the other two describe.
count_r2b_hold_routing <- function(attributes_wide) {
  r2b_pre_bypass_count <- 0L
  if ("r2b_bypassed" %in% names(attributes_wide)) {
    r2b_pre_bypass_count <- sum(!is.na(attributes_wide$r2b_bypassed) &
                                  attributes_wide$r2b_bypassed == 1L,
                                na.rm = TRUE)
  }
  r2b_hold_bypass_count <- 0L
  if ("r2b_hold_bypass" %in% names(attributes_wide)) {
    r2b_hold_bypass_count <- sum(!is.na(attributes_wide$r2b_hold_bypass) &
                                   attributes_wide$r2b_hold_bypass == 1L,
                                 na.rm = TRUE)
  }
  r2b_hold_queued_count <- 0L
  if ("r2b_hold_queued" %in% names(attributes_wide)) {
    r2b_hold_queued_count <- sum(!is.na(attributes_wide$r2b_hold_queued) &
                                   attributes_wide$r2b_hold_queued == 1L,
                                 na.rm = TRUE)
  }
  cat(sprintf(
    "R2B routing: pre-bypass at R1 (threshold): %d | at-R2B bypass (hold full): %d | R2B queue (both full): %d\n",
    r2b_pre_bypass_count, r2b_hold_bypass_count, r2b_hold_queued_count
  ))
  list(
    r2b_hold_bypass_count = r2b_hold_bypass_count,
    r2b_hold_queued_count = r2b_hold_queued_count,
    r2b_pre_bypass_count = r2b_pre_bypass_count
  )
}

#' Count why a casualty at R2B was not operated on there
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @return A list of `r2b_ot_bypass_busy_count`, `r2b_ot_bypass_count`,
#'   `r2b_ot_bypass_offshift_count`.
#' @details Decomposes the at-R2B operating theatre bypasses into the two reasons the
#'   trajectory records, an off-shift surgical section and a busy theatre.
count_r2b_ot_bypass_reasons <- function(attributes_wide) {
  r2b_ot_bypass_offshift_count <- 0L
  r2b_ot_bypass_busy_count     <- 0L
  if ("r2b_bypass_reason" %in% names(attributes_wide)) {
    r2b_ot_bypass_offshift_count <- sum(!is.na(attributes_wide$r2b_bypass_reason) &
                                          attributes_wide$r2b_bypass_reason == 1L,
                                        na.rm = TRUE)
    r2b_ot_bypass_busy_count     <- sum(!is.na(attributes_wide$r2b_bypass_reason) &
                                          attributes_wide$r2b_bypass_reason == 2L,
                                        na.rm = TRUE)
  }
  r2b_ot_bypass_count <- r2b_ot_bypass_offshift_count + r2b_ot_bypass_busy_count
  cat(sprintf(
    "R2B OT bypass reason (at-R2B, surgical decision point): team off-shift: %d | OT busy/queued: %d | total: %d\n",
    r2b_ot_bypass_offshift_count, r2b_ot_bypass_busy_count, r2b_ot_bypass_count
  ))
  list(
    r2b_ot_bypass_busy_count = r2b_ot_bypass_busy_count,
    r2b_ot_bypass_count = r2b_ot_bypass_count,
    r2b_ot_bypass_offshift_count = r2b_ot_bypass_offshift_count
  )
}

#' Summarise casualties held forward for a section about to reopen
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @param r2b_ot_bypass_busy_count Bypasses whose theatre was busy or queued.
#' @param r2b_ot_bypass_count Total at-R2B operating theatre bypasses.
#' @param r2b_ot_bypass_offshift_count Bypasses whose surgical section was off shift.
#' @param images_dir Directory the plots are written to.
#' @return The r2b_bypass_reason_plot object.
#' @details Also plots the bypass reasons per day, the pre-open hold being the
#'   mechanism that removes one of them.
summarise_r2b_pre_open_hold <- function(attributes_wide, combined, r2b_ot_bypass_busy_count,
                                        r2b_ot_bypass_count, r2b_ot_bypass_offshift_count,
                                        images_dir) {
  r2b_pre_open_count <- 0L
  r2b_pre_open_waits <- numeric(0)
  if ("r2b_pre_open_wait" %in% names(attributes_wide)) {
    r2b_pre_open_count <- sum(!is.na(attributes_wide$r2b_pre_open_wait) &
                                attributes_wide$r2b_pre_open_wait == 1L,
                              na.rm = TRUE)
  }
  if ("r2b_pre_open_wait_min" %in% names(attributes_wide)) {
    r2b_pre_open_waits <- attributes_wide$r2b_pre_open_wait_min[
      !is.na(attributes_wide$r2b_pre_open_wait_min)]
  }
  cat(sprintf(
    "R2B pre-open hold (window %d min): %d held forward, %d operated, mean hold %.1f min, max %.1f min\n",
    as.integer(env_data$vars$r2b$surgery$pre_open_window_min),
    r2b_pre_open_count, length(r2b_pre_open_waits),
    if (length(r2b_pre_open_waits) > 0) mean(r2b_pre_open_waits) else 0,
    if (length(r2b_pre_open_waits) > 0) max(r2b_pre_open_waits) else 0
  ))

  # Averaged per replication before plotting (matches the r2b_hold_daily
  # convention) so multi-replication runs show mean bypasses per
  # simulation day rather than a raw sum across replications, which would
  # scale with n_iterations and misrepresent a single run's daily pattern.
  r2b_bypass_reason_levels <- c("Team off-shift", "OT busy / queued")
  day_range   <- seq(min(floor(combined$start_time / DAY_MIN) + 1),
                      max(floor(combined$start_time / DAY_MIN) + 1))
  n_reps_bypass <- max(1L, n_distinct(attributes_wide$replication))

  r2b_bypass_reason_daily <- attributes_wide %>%
    filter(!is.na(r2b_bypass_reason)) %>%
    mutate(
      day    = floor(r2b_bypass_time / DAY_MIN) + 1,
      reason = factor(
        ifelse(r2b_bypass_reason == 1, "Team off-shift", "OT busy / queued"),
        levels = r2b_bypass_reason_levels
      )
    ) %>%
    group_by(replication, day, reason) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(day, reason) %>%
    summarise(mean_n = mean(n), .groups = "drop") %>%
    complete(day = day_range, reason = r2b_bypass_reason_levels, fill = list(mean_n = 0))

  y_top <- max(1, tapply(r2b_bypass_reason_daily$mean_n, r2b_bypass_reason_daily$day, sum)) + 1

  r2b_bypass_reason_plot <- ggplot(r2b_bypass_reason_daily, aes(x = factor(day), y = mean_n, fill = reason)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    scale_fill_manual(values = c("Team off-shift" = "#2a78d6", "OT busy / queued" = "#1baf7a")) +
    scale_y_continuous(breaks = 0:y_top, limits = c(0, y_top)) +
    labs(
      title    = "R2B OT Bypass Reason per Simulation Day",
      subtitle = sprintf(
        "Mean bypasses per day across %d replication%s (%d total: %d off-shift, %d OT busy/queued)",
        n_reps_bypass, if (n_reps_bypass == 1) "" else "s",
        r2b_ot_bypass_count, r2b_ot_bypass_offshift_count, r2b_ot_bypass_busy_count
      ),
      x = "Simulation Day", y = "Mean Casualties Bypassed", fill = "Reason"
    ) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")

  ggsave(file.path(images_dir, "r2b_ot_bypass_reason.png"), r2b_bypass_reason_plot,
         width = 12, height = 6, dpi = 150)
  r2b_bypass_reason_plot
}

#' Plot casualties treated at each R2B station per day
#'
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @return Panel of casualties treated per R2B station per day.
plot_r2b_casualty_treatment <- function(combined) {
  r2b_casualties <- combined %>%
    filter(!is.na(r2b_treated) & r2b_treated > 0) %>%
    mutate(
      r2b_station              = paste0("R2B ", r2b_treated),
      r2b_treatment_start_time = floor(start_time / DAY_MIN) + 1
    )

  daily_station_summary <- r2b_casualties %>% count(r2b_treatment_start_time, r2b_station)

  r2b_levels <- unique(daily_station_summary$r2b_station)
  r2b_colors <- setNames(RColorBrewer::brewer.pal(n = max(3, length(r2b_levels)), name = "Set2")[seq_along(r2b_levels)], r2b_levels)

  plot_r2b_treated <- ggplot(daily_station_summary, aes(x = factor(r2b_treatment_start_time), y = n, fill = r2b_station)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    labs(title = "Casualties Treated at R2B Stations per Simulation Day", x = "Simulation Day", y = "Casualties", fill = "R2B Station") +
    scale_fill_manual(values = r2b_colors) +
    scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
    theme_minimal(base_size = 14)
  plot_r2b_treated
}

#' Summarise and plot R2B surgeries started per day
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @return A list of `plot_r2b_summary`, `r2b_summary`.
summarise_r2b_surgery <- function(attributes_wide) {
  r2b_summary <- attributes_wide %>%
    filter(!is.na(r2b_surgery_start)) %>%
    mutate(
      r2b_day     = floor(as.numeric(r2b_surgery_start) / DAY_MIN) + 1,
      r2b_station = paste0("R2B ", r2b_treated)
    ) %>%
    count(r2b_day, r2b_station, name = "surgeries")

  plot_r2b_summary <- ggplot(r2b_summary, aes(x = factor(r2b_day), y = surgeries, fill = r2b_station)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    labs(title = "R2B Surgeries Started per Simulation Day", x = "Simulation Day", y = "Casualties", fill = "R2B Station") +
    scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal(base_size = 14)
  list(
    plot_r2b_summary = plot_r2b_summary,
    r2b_summary = r2b_summary
  )
}

#' Assemble the three-panel R2B handling figure
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @param plot_r2b_summary Panel of R2B surgeries started per day.
#' @param plot_r2b_treated Panel of casualties treated per R2B station per day.
#' @param images_dir Directory the plots are written to.
#' @return The p_r2b_handling object.
#' @details Stacks the treatment, surgery and bypass panels the two callers above
#'   produce, adding the count of casualties that skipped R2B entirely.
plot_r2b_handling <- function(attributes_wide, combined, plot_r2b_summary, plot_r2b_treated,
                              images_dir) {
  skipped_r2b_daily <- attributes_wide %>%
    mutate(day = floor(time / DAY_MIN) + 1) %>%
    filter(!is.na(r2e_treated) & is.na(r2b_treated)) %>%
    group_by(day) %>%
    summarise(skipped_r2b = n(), .groups = "drop") %>%
    complete(day = seq(min(floor(combined$start_time / (24 * 60)) + 1),
                       max(floor(combined$start_time / (24 * 60)) + 1)),
             fill = list(skipped_r2b = 0))

  plot_r2b_skipped <- ggplot(skipped_r2b_daily, aes(x = factor(day), y = skipped_r2b)) +
    geom_bar(stat = "identity", width = 0.7, fill = "#6A737B") +
    labs(title = "Casualties Skipping R2B per Simulation Day", x = "Simulation Day", y = "Casualties") +
    scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
    theme_minimal(base_size = 14)

  p_r2b_handling <- plot_r2b_treated / plot_r2b_summary / plot_r2b_skipped
  ggsave(file.path(images_dir, "r2b_handling.png"), p_r2b_handling,
         width = 12, height = 10, dpi = 150)
  p_r2b_handling
}

#' Plot R2B bed occupancy as a Gantt chart
#'
#' @param resources Resource monitor rows, warm-up excluded.
#' @param images_dir Directory the plots are written to.
#' @return A list of `max_days`, `p_r2b_gantt`.
plot_r2b_bed_gantt <- function(resources, images_dir) {
  r2b_bed_usage <- resources %>%
    as.data.frame() %>%
    filter(str_detect(resource, "^b_r2b_\\w+_\\d+_t\\d+$")) %>%
    arrange(replication, resource, time) %>%
    group_by(replication) %>%
    mutate(rep_end_time = if (all(is.na(time))) NA_real_ else max(time, na.rm = TRUE)) %>%
    group_by(replication, resource) %>%
    mutate(
      start_time = time,
      end_time   = lead(time),
      end_time   = if_else(is.na(end_time), rep_end_time, end_time)
    ) %>%
    filter(server > 0, end_time > start_time) %>%
    ungroup() %>%
    mutate(
      bed_type       = str_match(resource, "^b_r2b_([^_]+)_")[,2],
      bed_num        = str_match(resource, "^b_r2b_[^_]+_(\\d+)_")[,2],
      r2b_team       = str_match(resource, "_t(\\d+)$")[,2],
      resource_label = paste(toupper(bed_type), "Bed", bed_num),
      bed_num_i      = as.integer(bed_num),
      r2b_team_i     = as.integer(r2b_team)
    ) %>%
    arrange(r2b_team_i, bed_type, bed_num_i) %>%
    mutate(r2b_team = factor(r2b_team, levels = unique(r2b_team))) %>%
    group_by(r2b_team) %>%
    mutate(resource_label = factor(resource_label, levels = unique(resource_label))) %>%
    ungroup()

  max_days <- {
    max_end <- suppressWarnings(max(r2b_bed_usage$end_time, na.rm = TRUE))
    if (!is.finite(max_end)) NA_real_ else ceiling(max_end / DAY_MIN)
  }
  x_breaks <- if (is.finite(max_days) && max_days >= 1) seq(1, max_days, by = 1) else waiver()

  p_r2b_gantt <- ggplot(r2b_bed_usage, aes(y = resource_label, color = toupper(bed_type))) +
    geom_segment(aes(x = start_time / DAY_MIN, xend = end_time / DAY_MIN, yend = resource_label), linewidth = 6, lineend = "butt") +
    labs(title = "R2B Bed Resource Usage (Gantt) by Team", x = "Time (Days)", y = "Bed Resource", color = "Bed Type") +
    scale_x_continuous(breaks = x_breaks, labels = function(x) paste0(x), expand = c(0, 0)) +
    facet_wrap(~ r2b_team, ncol = 1, scales = "free_y", labeller = labeller(r2b_team = function(x) paste0("R2B ", x))) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray"), legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file.path(images_dir, "r2b_gantt.png"), p_r2b_gantt,
         width = 14, height = 10, dpi = 150)
  list(
    max_days = max_days,
    p_r2b_gantt = p_r2b_gantt
  )
}
