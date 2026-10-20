##############################################
## R/analysis_run_r2e.R                     ##
## Single run: R2E                          ##
##############################################
#
# The single-run stages reporting R2E: surgery counts, bed queues and the
# bed gantt, the post-operative intensive care gate and pathways, and
# operating theatre utilisation.
#
# Sourced by R/analysis.R, which loads the packages and the shared modules
# these functions use; source that file rather than this one alone.

#' Summarise and plot R2E surgeries started per day
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param max_days Run length in whole days, for a shared x scale.
#' @param images_dir Directory the plots are written to.
#' @return A list of `p_r2e_surgeries`, `r2e_summary`.
summarise_r2e_surgeries <- function(attributes_wide, max_days, images_dir) {
  r2e_summary <- attributes_wide %>%
    dplyr::select(name, starts_with("r2e_surgery_")) %>%
    pivot_longer(cols = starts_with("r2e_surgery_"), names_to = "surgery_type", values_to = "start_min") %>%
    filter(!is.na(start_min)) %>%
    mutate(r2e_day = floor(start_min / DAY_MIN) + 1) %>%
    count(r2e_day, name = "surgeries")

  p_r2e_surgeries <- ggplot(r2e_summary, aes(x = r2e_day, y = surgeries)) +
    geom_bar(stat = "identity", fill = brewer.pal(n = 3, name = "Set2")[1]) +
    labs(title = "R2E Heavy Surgeries Completed Per Simulation Day", x = "Simulation Day", y = "Number of Surgeries") +
    scale_x_continuous(breaks = seq(1, max_days, by = 1), expand = c(0, 0)) +
    scale_y_continuous(breaks = 0:25, limits = c(0, 25)) +
    theme_minimal(base_size = 14)
  ggsave(file.path(images_dir, "r2eheavy_surgeries.png"), p_r2e_surgeries,
         width = 12, height = 6, dpi = 150)
  list(
    p_r2e_surgeries = p_r2e_surgeries,
    r2e_summary = r2e_summary
  )
}

#' Distinct colours for a bed type scale of any size
#'
#' @param n Number of bed types to colour.
#' @return A character vector of `n` colours.
#' @details `brewer.pal()` caps at the palette's own size and warns rather than
#'   failing, which leaves a discrete scale short of values and drops the
#'   surplus levels from the figure. Interpolating covers any count, and is
#'   applied only past the palette's size so that the counts the model actually
#'   fields keep the palette's own well-separated hues.
bed_type_colours <- function(n) {
  palette_size <- 8L
  if (n <= palette_size) brewer.pal(palette_size, "Set2")[seq_len(n)]
  else colorRampPalette(brewer.pal(palette_size, "Set2"))(n)
}

#' Plot every R2E Heavy bed type's queues over time
#'
#' @param resources Resource monitor rows, warm-up excluded.
#' @param images_dir Directory the plots are written to.
#' @return The p_r2e_bed_queues object.
#' @details One panel per bed type the echelon fields, one step line per bed,
#'   every panel on one shared vertical scale, so that a queue of one and a
#'   queue of fourteen are drawn at the sizes they are. Bed types are derived
#'   from the resource names rather than named here, so a type added to the
#'   establishment appears without a change to this function.
plot_r2e_bed_queues <- function(resources, images_dir) {
  # Selected by pattern and typed from the resource name, matching
  # plot_r2b_bed_queues(), so that a bed type added to the establishment
  # appears without a change here. Naming the types instead is what left the
  # holding pool out of this figure entirely.
  queue_plot_data <- resources %>%
    as.data.frame() %>%
    filter(grepl("^b_r2eheavy_.*_\\d+_t\\d+$", resource)) %>%
    dplyr::select(time, resource, queue) %>%
    mutate(
      bed_type  = str_extract(resource, "(?<=b_r2eheavy_)[^_]+") %>% toupper(),
      bed_index = str_extract(resource, "(?<=_)[0-9]+(?=_t)") %>% as.integer()
    )

  # Each facet is labelled with the size of its pool, because a queue of one
  # means something different behind two theatres than behind thirty holding
  # beds, and the panel alone does not say which it is. The count is of the
  # beds drawn, which is every bed the monitor recorded an event for: a bed
  # never seized has no rows and so no line. Over the tracked 30-day baseline
  # every bed of every R2E pool sees use, so the two coincide there.
  #
  # The panels share one vertical scale. Freeing it would let each pool fill
  # its own panel whatever it queued, so a pool never exceeding one casualty
  # would be drawn exactly as a pool reaching fourteen, which is the reading
  # this figure most needs to get right.
  bed_counts <- queue_plot_data %>%
    group_by(bed_type) %>%
    summarise(n_beds = n_distinct(resource), .groups = "drop") %>%
    mutate(facet_label = sprintf("%s (%d bed%s)", bed_type, n_beds,
                                 ifelse(n_beds == 1, "", "s")))
  queue_plot_data <- queue_plot_data %>% left_join(bed_counts, by = "bed_type")

  p_r2e_bed_queues <- ggplot(
    queue_plot_data,
    aes(x = time / DAY_MIN, y = queue, group = resource, color = bed_type)
  ) +
    geom_step(linewidth = 0.7, alpha = 0.8) +
    labs(
      title    = "R2E Heavy Bed Queue Length Over Time by Bed Type",
      subtitle = paste("One line per bed that saw use. Panels share a vertical",
                       "scale, so queue depths are comparable between pools"),
      x = "Time (Days)", y = "Queue Size"
    ) +
    facet_wrap(~ facet_label, ncol = 1, scales = "fixed") +
    # The palette caps at eight, and a ninth bed type would be assigned no
    # colour and its lines dropped from the figure, which is the failure this
    # plot exists to stop rather than one to reintroduce further along. Taken
    # straight from the palette while it has enough colours, so the shipped
    # four keep the distinct hues, and interpolated only past that point.
    scale_color_manual(values = bed_type_colours(n_distinct(queue_plot_data$bed_type)),
                       guide = "none") +
    scale_x_continuous(breaks = seq(1, ceiling(max(queue_plot_data$time) / DAY_MIN), by = 1),
                       labels = function(x) paste0(x), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_minimal(base_size = 14) +
    theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(linetype = "dotted", color = "gray"))
  ggsave(file.path(images_dir, "r2eheavy_bed_queue_3_teams.png"), p_r2e_bed_queues,
         width = 12, height = 8, dpi = 150)
  p_r2e_bed_queues
}

#' Plot R2E bed occupancy as a Gantt chart
#'
#' @param resources Resource monitor rows, warm-up excluded.
#' @param images_dir Directory the plots are written to.
#' @return The p_r2e_gantt object.
plot_r2e_bed_gantt <- function(resources, images_dir) {
  r2e_bed_usage <- resources %>%
    as.data.frame() %>%
    filter(str_detect(resource, "^b_r2eheavy_\\w+_\\d+_t\\d+$")) %>%
    arrange(replication, resource, time) %>%
    group_by(replication) %>%
    mutate(rep_end_time = max(time)) %>%
    group_by(replication, resource) %>%
    mutate(
      start_time = time,
      end_time   = lead(time),
      end_time   = if_else(is.na(end_time), rep_end_time, end_time)
    ) %>%
    filter(server > 0, end_time > start_time) %>%
    ungroup() %>%
    mutate(
      bed_type       = str_match(resource, "^b_r2eheavy_([^_]+)_")[,2],
      bed_num        = str_match(resource, "^b_r2eheavy_[^_]+_(\\d+)_")[,2],
      r2e_team       = str_match(resource, "_t(\\d+)$")[,2],
      resource_label = paste(toupper(bed_type), "Bed", bed_num)
    ) %>%
    mutate(
      bed_num_i  = as.integer(bed_num),
      r2e_team_i = as.integer(r2e_team)
    ) %>%
    arrange(bed_type, bed_num_i, r2e_team_i) %>%
    mutate(resource_label = factor(resource_label, levels = unique(resource_label)))

  max_days_r2e <- ceiling(max(r2e_bed_usage$end_time, na.rm = TRUE) / DAY_MIN)

  p_r2e_gantt <- ggplot(r2e_bed_usage, aes(y = resource_label, color = toupper(bed_type))) +
    geom_segment(aes(x = start_time / DAY_MIN, xend = end_time / DAY_MIN, yend = resource_label), linewidth = 6, lineend = "butt") +
    labs(title = "R2E Bed Resource Usage (Gantt)", x = "Time (Days)", y = "Bed Resource", color = "Bed Type") +
    scale_x_continuous(breaks = seq(1, max_days_r2e, by = 1), labels = function(x) paste0(x), expand = c(0, 0)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray"), legend.position = "bottom")
  ggsave(file.path(images_dir, "r2eheavy_gantt.png"), p_r2e_gantt,
         width = 14, height = 10, dpi = 150)
  p_r2e_gantt
}

#' Summarise care degraded or delayed by intensive care saturation
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `r2e_icu_gating_daily`, `r2e_icu_gating_plot`.
summarise_r2e_icu_gating <- function(attributes_wide, combined, output_dir, images_dir) {
  r2e_icu_gating_plot  <- NULL
  r2e_icu_gating_daily <- NULL

  if ("post_op_pathway" %in% names(attributes_wide) &&
      "r2e_surgery_1_start" %in% names(attributes_wide) &&
      any(!is.na(attributes_wide$post_op_pathway))) {

    icu_gating_patients <- attributes_wide %>%
      filter(!is.na(post_op_pathway)) %>%
      mutate(
        care_category = case_when(
          !is.na(surgery_deferred) & surgery_deferred == 1 ~ "Delayed (ICU Wait)",
          post_op_pathway == 2                              ~ "Sub-Optimal (Hold Bed Override)",
          TRUE                                               ~ "Normal (ICU Access)"
        ),
        day = floor(as.numeric(r2e_surgery_1_start) / DAY_MIN) + 1
      )

    n_sim_days_icu <- ceiling(max(combined$start_time, na.rm = TRUE) / DAY_MIN)
    care_levels <- c("Normal (ICU Access)", "Sub-Optimal (Hold Bed Override)", "Delayed (ICU Wait)")

    r2e_icu_gating_daily <- icu_gating_patients %>%
      count(day, care_category, name = "n") %>%
      complete(
        day           = seq_len(n_sim_days_icu),
        care_category = care_levels,
        fill          = list(n = 0)
      ) %>%
      mutate(care_category = factor(care_category, levels = care_levels))

    n_suboptimal  <- sum(icu_gating_patients$care_category == "Sub-Optimal (Hold Bed Override)")
    n_delayed     <- sum(icu_gating_patients$care_category == "Delayed (ICU Wait)")
    n_total_gated <- nrow(icu_gating_patients)
    n_max_daily   <- r2e_icu_gating_daily %>% group_by(day) %>% summarise(t = sum(n), .groups = "drop") %>% pull(t) %>% max()

    r2e_icu_gating_plot <- ggplot(r2e_icu_gating_daily, aes(x = day, y = n, fill = care_category)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c(
        "Normal (ICU Access)"             = "#4C956C",
        "Sub-Optimal (Hold Bed Override)" = "#D62828",
        "Delayed (ICU Wait)"              = "#F4A259"
      )) +
      annotate("text", x = 1, y = n_max_daily + 0.5,
               label = sprintf(
                 "%d of %d R2E surgical patients affected by ICU saturation: %d sub-optimal (hold-bed recovery), %d delayed",
                 n_suboptimal + n_delayed, n_total_gated, n_suboptimal, n_delayed
               ),
               hjust = 0, size = 3.2, color = "gray20") +
      scale_x_continuous(breaks = seq(1, n_sim_days_icu, by = 2), expand = c(0, 0)) +
      labs(
        title    = "R2E OT-ICU Gating — Sub-Optimal and Delayed Care by Simulation Day",
        subtitle = "Red = surgery proceeded despite ICU saturation (Priority 1 override, hold-bed recovery); Orange = OT entry deferred pending ICU availability (Priority 2+)",
        x = "Simulation Day (day of surgery start)", y = "Casualties", fill = "Care Pathway"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom")

    ggsave(file.path(images_dir, "r2e_icu_gating_impact.png"), r2e_icu_gating_plot,
           width = 12, height = 6, dpi = 150)

    write.csv(r2e_icu_gating_daily, file.path(output_dir, "r2e_icu_gating_daily.csv"), row.names = FALSE)
  }
  list(
    r2e_icu_gating_daily = r2e_icu_gating_daily,
    r2e_icu_gating_plot = r2e_icu_gating_plot
  )
}

#' Compute operating theatre utilisation per echelon
#'
#' @param resources_raw See analyse_run().
#' @return A list of `ot_utilisation`, `obs_window`.
#' @details Server time as a proportion of capacity multiplied by the observation
#'   window, the window taken from the arrivals rather than assumed.
compute_ot_utilisation <- function(resources_raw) {
  # KPI 7: OT utilisation rate per echelon
  # Server time as proportion of (capacity × observation window)
  obs_window <- max(resources_raw$time, na.rm = TRUE)
  ot_utilisation <- resources_raw %>%
    filter(grepl("^b_r2(b|eheavy)_ot_", resource)) %>%
    mutate(
      echelon = if_else(grepl("^b_r2b_", resource), "R2B", "R2E"),
      duration = lead(time) - time
    ) %>%
    filter(!is.na(duration) & duration > 0) %>%
    group_by(echelon, resource) %>%
    summarise(
      busy_time = sum(server * duration, na.rm = TRUE),
      capacity  = max(capacity, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    group_by(echelon) %>%
    summarise(
      utilisation = sum(busy_time) / (sum(capacity) * obs_window),
      .groups     = "drop"
    )
  list(
    ot_utilisation = ot_utilisation,
    obs_window = obs_window
  )
}

#' Summarise the post-operative pathway each operated casualty took
#'
#' @param attributes_wide See analyse_run().
#' @param evacuation_policy_summary See analyse_run().
#' @param in_theatre_share See analyse_run().
#' @param output_dir See analyse_run().
#' @return A list of `post_op_pathway_summary`, `surgery_deferred_count`,
#'   `definitive_repair_outstanding_count`, `surgical_pathway_summary`.
#' @details Splits both the post-operative destination, an intensive care bed or the
#'   degraded holding-bed fallback, and the damage control against single-stage
#'   surgical pathway.
summarise_post_operative_pathways <- function(attributes_wide, evacuation_policy_summary,
                                              in_theatre_share, output_dir) {
  # KPI 8: R2E OT-ICU gating post-operative pathway summary
  # post_op_pathway: 1 = ICU (nominal), 2 = Post-Op Hold (ICU saturated, P1 override).
  # postop_dow uses dow_echelon == 4, the post-operative checkpoint,
  # kept distinct from the Phase 1 R2E arrival DOW checkpoint (dow_echelon == 3) so the
  # two pathways' realised post-operative mortality can be directly compared.
  pathway_labels <- c("1" = "icu", "2" = "hold")
  post_op_pathway_summary <- NULL
  surgery_deferred_count  <- 0L
  definitive_repair_outstanding_count <- 0L
  if ("post_op_pathway" %in% names(attributes_wide)) {
    post_op_pathway_summary <- attributes_wide %>%
      filter(!is.na(post_op_pathway)) %>%
      mutate(
        pathway    = pathway_labels[as.character(as.integer(post_op_pathway))],
        postop_dow = as.integer(!is.na(dow_echelon) & dow_echelon == 4 & dow == 1)
      ) %>%
      group_by(pathway) %>%
      summarise(
        total           = n(),
        died            = sum(postop_dow),
        postop_dow_rate = died / total,
        .groups         = "drop"
      )
    write.csv(post_op_pathway_summary, file.path(output_dir, "post_op_pathway_summary.csv"), row.names = FALSE)
  }
  if ("surgery_deferred" %in% names(attributes_wide)) {
    surgery_deferred_count <- sum(!is.na(attributes_wide$surgery_deferred) &
                                    attributes_wide$surgery_deferred == 1L,
                                  na.rm = TRUE)
  }
  # Casualties released to strategic evacuation with the definitive repair
  # outstanding, the surgical queue having reached the configured saturation
  # threshold. Zero at the shipped threshold, which disables the release.
  if ("definitive_repair_outstanding" %in% names(attributes_wide)) {
    definitive_repair_outstanding_count <-
      sum(!is.na(attributes_wide$definitive_repair_outstanding) &
            attributes_wide$definitive_repair_outstanding == 1L, na.rm = TRUE)
  }
  # KPI 8a: surgical pathway split
  # dcs_pathway: 1 = staged damage control (abbreviated operation, stabilisation,
  # definitive repair), 0 = single-stage definitive procedure. Reported over
  # casualties who actually reached theatre, by priority, so the realised share
  # can be read against the configured `pri*_dcs_rate` it is drawn from.
  surgical_pathway_summary <- NULL
  if (all(c("dcs_pathway", "priority") %in% names(attributes_wide))) {
    operated <- attributes_wide %>%
      filter(!is.na(dcs_pathway)) %>%
      mutate(
        had_surgery = (!is.na(r2b_surgery) & r2b_surgery == 1) |
                      (!is.na(r2e_surgery) & r2e_surgery == 1)
      ) %>%
      filter(had_surgery)

    if (nrow(operated)) {
      surgical_pathway_summary <- operated %>%
        mutate(priority_group = ifelse(is.na(priority), "unclassified",
                                       paste0("Priority ", as.integer(priority)))) %>%
        group_by(priority_group) %>%
        summarise(
          operated       = n(),
          damage_control = sum(dcs_pathway == 1),
          single_stage   = sum(dcs_pathway == 0),
          dcs_share      = damage_control / operated,
          .groups        = "drop"
        )
      write.csv(surgical_pathway_summary,
                file.path(output_dir, "surgical_pathway_summary.csv"), row.names = FALSE)
      cat(sprintf(
        "Surgical pathway split: %d operated, %d damage control (%.1f%%), %d single-stage\n",
        nrow(operated), sum(operated$dcs_pathway == 1),
        100 * mean(operated$dcs_pathway == 1), sum(operated$dcs_pathway == 0)))
      print(surgical_pathway_summary)
    }
  }

  cat(sprintf("R2E OT-ICU gating: surgery deferred (ICU saturated, P2+): %d\n", surgery_deferred_count))
  cat(sprintf(paste("R2E evacuated with definitive repair outstanding",
                    "(surgical queue saturated): %d\n"),
              definitive_repair_outstanding_count))
  if (!is.na(evacuation_policy_summary$in_theatre_share)) {
    cat(sprintf(
      "R2E evacuation policy: %d-day policy, %d dispositions, realised in-theatre share %.1f%%\n",
      evacuation_policy_summary$policy_days, evacuation_policy_summary$decisions,
      100 * evacuation_policy_summary$in_theatre_share))
  }
  if (!is.null(post_op_pathway_summary)) print(post_op_pathway_summary)
  list(
    post_op_pathway_summary = post_op_pathway_summary,
    surgery_deferred_count = surgery_deferred_count,
    definitive_repair_outstanding_count = definitive_repair_outstanding_count,
    surgical_pathway_summary = surgical_pathway_summary
  )
}
