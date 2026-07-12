##############################################
## R/analysis.R                             ##
## Analysis and visualisation pipeline      ##
##############################################

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(knitr)
library(data.table)
library(RColorBrewer)

#' Runs the full analysis and visualisation pipeline on monitoring data
#'
#' @param mon Named list with elements arrivals, attributes, resources as
#'   returned by run_single()
#' @param output_dir Directory path for saving CSV and plot outputs
#'   (default: "outputs")
#' @param warm_up_days Days to exclude from the start of the analysis window
#'   (applied to arrivals by start_time and resources by time; default 0)
#' @param images_dir Directory path for saving PNG plots (default: "images")
#' @return Invisibly returns a named list. Nine elements are always-present
#'   ggplot objects (Issue #14 — embeddable in a Shiny reactive context via
#'   e.g. renderPlot()): casualty_flow, r1_queues, r2b_treatment,
#'   r2b_bed_queues, r2b_gantt, r2e_surgery, r2e_bed_queues, waiting_times,
#'   r2e_gantt. Remaining elements are summary data frames, scalar KPIs, and
#'   conditional ggplot objects (NULL when their trigger condition — e.g. a
#'   run with zero R2B hold occupants — is not met).
#'
#' @details Writes CSVs to output_dir and PNGs to images_dir; does not print
#'   plots to the active graphics device (the caller is responsible for
#'   display — see run.R for the CLI path, which prints each plot in the
#'   original on-screen order for interactive/RStudio use).
analyse_run <- function(mon, output_dir = "outputs", warm_up_days = 0,
                        images_dir = "images") {
  dir.create(output_dir,  showWarnings = FALSE, recursive = TRUE)
  dir.create(images_dir,  showWarnings = FALSE, recursive = TRUE)

  warm_up_min    <- as.integer(warm_up_days) * 1440L
  arrivals_raw   <- mon$arrivals    %>% filter(start_time >= warm_up_min)
  attributes_raw <- mon$attributes
  resources_raw  <- mon$resources   %>% filter(time >= warm_up_min)

  write.csv(arrivals_raw,   file.path(output_dir, "mon_arrivals.csv"),   row.names = FALSE)
  write.csv(attributes_raw, file.path(output_dir, "mon_attributes.csv"), row.names = FALSE)
  write.csv(resources_raw,  file.path(output_dir, "mon_resources.csv"),  row.names = FALSE)

  arrivals <- arrivals_raw %>%
    mutate(waiting_time = end_time - start_time - activity_time)

  attributes <- attributes_raw

  resources <- resources_raw

  # Pivot attributes wide (last value per key per casualty)
  attributes_wide <- attributes %>%
    pivot_wider(
      id_cols    = c(name, replication, time),
      names_from = key,
      values_fn  = ~ dplyr::first(.x)
    ) %>%
    arrange(name, replication, time) %>%
    group_by(name, replication) %>%
    fill(everything(), .direction = "down") %>%
    slice_tail(n = 1) %>%
    ungroup()

  attributes_wide <- attributes_wide %>%
    semi_join(dplyr::select(arrivals, name, replication), by = c("name", "replication"))

  # pivot_wider only creates a column for an attribute key when at least one
  # casualty in the run had it set; guard against runs with zero DOW events
  # (or, for post_op_pathway/surgery_deferred, zero R2E surgeries — Issue #43).
  for (dow_col in c("dow", "dow_echelon", "post_op_pathway", "surgery_deferred",
                    "mass_casualty_event")) {
    if (!dow_col %in% names(attributes_wide)) {
      attributes_wide[[dow_col]] <- NA_real_
    }
  }

  combined <- arrivals %>%
    left_join(attributes_wide, by = c("name", "replication")) %>%
    mutate(
      casualty_type     = str_extract(name, "^[^_]+"),
      population_source = str_extract(name, "(?<=_)[a-zA-Z]+"),
      arrival_day       = floor(start_time / (24 * 60)) + 1
    )

  casualty_summary <- combined %>%
    group_by(arrival_day, casualty_type, population_source) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(arrival_day, casualty_type, population_source)

  casualty_priority_summary <- combined %>%
    mutate(
      priority_group = case_when(
        casualty_type == "kia"       ~ "KIA",
        priority %in% c(1, 2, 3)    ~ paste0("Priority ", priority),
        TRUE                         ~ NA_character_
      )
    ) %>%
    filter(!is.na(priority_group)) %>%
    group_by(arrival_day, priority_group) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(arrival_day, priority_group) %>%
    mutate(priority_group = factor(priority_group, levels = c("Priority 1", "Priority 2", "Priority 3", "KIA")))

  max_daily_total <- casualty_summary %>%
    group_by(arrival_day) %>%
    summarise(total = sum(count)) %>%
    pull(total) %>%
    max()

  # ── Casualty overview plots ───────────────────────────────────────────────

  plot_type <- ggplot(casualty_summary, aes(x = arrival_day, y = count, fill = casualty_type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(min(casualty_priority_summary$arrival_day), max(casualty_priority_summary$arrival_day), by = 1)) +
    ylim(0, max_daily_total) +
    labs(title = "Total Casualties by Type and Arrival Day", x = "Arrival Day", y = "Number of Casualties", fill = "Casualty Type") +
    theme_minimal()

  plot_source <- ggplot(casualty_summary, aes(x = arrival_day, y = count, fill = population_source)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(min(casualty_priority_summary$arrival_day), max(casualty_priority_summary$arrival_day), by = 1)) +
    ylim(0, max_daily_total) +
    labs(title = "Total Casualties by Population Source and Arrival Day", x = "Arrival Day", y = "Number of Casualties", fill = "Population Source") +
    theme_minimal()

  plot_priority <- ggplot(casualty_priority_summary, aes(x = arrival_day, y = count, fill = priority_group)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_brewer(palette = "Pastel1") +
    scale_x_continuous(breaks = seq(min(casualty_priority_summary$arrival_day), max(casualty_priority_summary$arrival_day), by = 1)) +
    ylim(0, max_daily_total) +
    labs(title = "Total Casualties by Priority Level and Arrival Day", x = "Arrival Day", y = "Number of Casualties", fill = "Priority Group") +
    theme_minimal()

  p_casualty_summary <- plot_type / plot_source / plot_priority
  ggsave(file.path(images_dir, "casualty_summary.png"), p_casualty_summary,
         width = 12, height = 10, dpi = 150)

  # ── Summary tables ────────────────────────────────────────────────────────

  casualty_type_table_wide <- casualty_summary %>%
    pivot_wider(names_from = arrival_day, values_from = count, values_fill = 0) %>%
    mutate(total = rowSums(across(where(is.numeric)))) %>%
    arrange(casualty_type, population_source)

  casualty_type_total_row <- casualty_type_table_wide %>%
    summarise(across(where(is.numeric), sum)) %>%
    mutate(casualty_type = "Total", population_source = "")

  casualty_type_table_wide <- bind_rows(casualty_type_table_wide, casualty_type_total_row)

  population_source_table_wide <- casualty_summary %>%
    group_by(arrival_day, population_source) %>%
    summarise(count = sum(count), .groups = "drop") %>%
    pivot_wider(names_from = arrival_day, values_from = count, values_fill = 0) %>%
    mutate(total = rowSums(across(where(is.numeric)))) %>%
    arrange(population_source)

  population_source_total_row <- population_source_table_wide %>%
    summarise(across(where(is.numeric), sum)) %>%
    mutate(population_source = "Total")

  population_source_table_wide <- bind_rows(population_source_table_wide, population_source_total_row)

  priority_table_wide <- casualty_priority_summary %>%
    pivot_wider(names_from = arrival_day, values_from = count, values_fill = 0) %>%
    mutate(total = rowSums(across(where(is.numeric)))) %>%
    arrange(priority_group)

  priority_total_row <- priority_table_wide %>%
    summarise(across(where(is.numeric), sum)) %>%
    mutate(priority_group = "Total")

  priority_table_wide <- bind_rows(priority_table_wide, priority_total_row)

  writeLines(kable(priority_table_wide,            format = "markdown"), file.path(output_dir, "priority_table.md"))
  writeLines(kable(population_source_table_wide,   format = "markdown"), file.path(output_dir, "population_source_table.md"))
  writeLines(kable(casualty_type_table_wide,       format = "markdown"), file.path(output_dir, "casualty_table.md"))

  # ── R1 resource queue graphs ──────────────────────────────────────────────

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

  max_days_r1 <- ceiling(max(queue_plot_data_r1$time, na.rm = TRUE) / 1440)

  p_r1_queues <- ggplot(queue_plot_data_r1, aes(x = time / 1440, y = queue, color = role_label)) +
    geom_step(linewidth = 1) +
    labs(title = "Queue Length Over Time by R1 Team", x = "Time (Days)", y = "Queue Size", color = "Role") +
    scale_x_continuous(breaks = seq(0, max_days_r1, by = 1), limits = c(0, max_days_r1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1), expand = c(0, 0)) +
    facet_wrap(~ r1_label, ncol = 1, scales = "free_y") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray"), legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file.path(images_dir, "r1_queues.png"), p_r1_queues,
         width = 12, height = 8, dpi = 150)

  # ── R2B bed queue graphs ──────────────────────────────────────────────────

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

  p_r2b_bed_queues <- ggplot(queue_plot_data, aes(x = time / 1440, y = queue, color = bed_label)) +
    geom_step(linewidth = 1) +
    labs(title = "Queue Length Over Time by R2B", x = "Time (Days)", y = "Queue Size", color = "Bed") +
    scale_x_continuous(breaks = seq(0, max(queue_plot_data$time) / 1440, by = 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1), expand = c(0, 0)) +
    facet_wrap(~ r2b_label, ncol = 1, scales = "free_x") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray"), legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file.path(images_dir, "r2b_bed_queues.png"), p_r2b_bed_queues,
         width = 12, height = 8, dpi = 150)

  # ── R2B hold bed occupancy by patient stream ─────────────────────────────
  # Requires r2b_hold_start attribute (added in Issue #39 trajectory change).
  # Decomposes daily concurrent hold bed occupancy into disease DNBI, NBI, and
  # WIA streams. Verifies that battle_fatigue (dnbi_type == 1) never reaches
  # R2B hold beds.

  r2b_hold_occupancy_plot <- NULL
  r2b_hold_daily          <- NULL

  if ("r2b_hold_start" %in% names(attributes_wide) &&
      any(!is.na(attributes_wide$r2b_hold_start))) {

    # Verify: battle_fatigue must not appear in R2B hold
    bf_in_hold <- attributes_wide %>%
      filter(!is.na(r2b_hold_start) & !is.na(dnbi_type) & dnbi_type == 1L)
    stopifnot(
      "Battle fatigue (dnbi_type==1) must not occupy R2B hold beds" = nrow(bf_in_hold) == 0
    )

    n_sim_days <- ceiling(max(combined$start_time, na.rm = TRUE) / 1440)
    n_reps_hold <- max(1L, n_distinct(attributes_wide$replication))

    hold_patients <- attributes_wide %>%
      filter(!is.na(r2b_hold_start) & !is.na(return_day)) %>%
      mutate(
        stream = case_when(
          !is.na(dnbi_type) & dnbi_type == 2L ~ "Disease DNBI",
          !is.na(dnbi_type) & dnbi_type == 3L ~ "NBI",
          TRUE                                 ~ "WIA"
        ),
        hold_start_min = as.numeric(r2b_hold_start),
        hold_end_min   = as.numeric(return_day)
      )

    # Expand each patient into one row per simulation day they occupy a hold bed
    r2b_hold_daily <- hold_patients %>%
      rowwise() %>%
      mutate(
        day = list(
          seq(
            max(1L, floor(hold_start_min / 1440) + 1L),
            min(n_sim_days, ceiling(hold_end_min / 1440))
          )
        )
      ) %>%
      unnest(day) %>%
      ungroup() %>%
      group_by(replication, day, stream) %>%
      summarise(occupancy = n(), .groups = "drop") %>%
      group_by(day, stream) %>%
      summarise(mean_occupancy = mean(occupancy), .groups = "drop") %>%
      complete(
        day    = seq_len(n_sim_days),
        stream = c("Disease DNBI", "NBI", "WIA"),
        fill   = list(mean_occupancy = 0)
      )

    n_hold_beds <- 5   # per R2B unit (from env_data.json)

    r2b_hold_occupancy_plot <- ggplot(
      r2b_hold_daily,
      aes(x = day, y = mean_occupancy, fill = stream)
    ) +
      geom_bar(stat = "identity", position = "stack") +
      geom_hline(yintercept = n_hold_beds,
                 linetype = "dashed", color = "red", linewidth = 0.8) +
      annotate("text", x = 1, y = n_hold_beds + 0.3,
               label = sprintf("Capacity per R2B unit (%d beds)", n_hold_beds),
               hjust = 0, size = 3.2, color = "red") +
      scale_fill_brewer(palette = "Set2") +
      scale_x_continuous(
        breaks = seq(1, n_sim_days, by = 2),
        expand = c(0, 0)
      ) +
      labs(
        title    = "R2B Hold Bed Daily Occupancy by Patient Stream",
        subtitle = paste0(
          "Dashed line = capacity per R2B unit (", n_hold_beds, " beds); ",
          "total capacity = ", n_hold_beds * 2, " beds across 2 R2B units"
        ),
        x    = "Simulation Day",
        y    = "Mean Concurrent Patients in Hold",
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

  # ── R2B hold routing diagnostics ─────────────────────────────────────────
  # r2b_bypassed     = 1: routed to R2E at R1 (hold_threshold exceeded before transport)
  # r2b_hold_bypass  = 1: arrived at R2B but hold full at Step 4 → R2E
  # r2b_hold_queued  = 1: both echelons full, queue within cap → queued at R2B hold
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

  # ── R2B OT bypass reason decomposition (Issue #40) ───────────────────────
  # r2b_bypass_reason is only set for casualties who reached R2B (r2b_treated
  # non-NA) and were bypassed to R2E at the surgical decision point — it does
  # not apply to the pre-transport r2b_bypassed rows above (those never carry
  # r2b_treated, since they are routed from R1 before ever arriving at R2B).
  # 1 = surgical team off-shift; 2 = OT bed busy or queued.
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

  # Averaged per replication before plotting (matches the r2b_hold_daily
  # convention, Issue #39) so multi-replication runs show mean bypasses per
  # simulation day rather than a raw sum across replications, which would
  # scale with n_iterations and misrepresent a single run's daily pattern.
  r2b_bypass_reason_levels <- c("Team off-shift", "OT busy / queued")
  day_range   <- seq(min(floor(combined$start_time / 1440) + 1),
                      max(floor(combined$start_time / 1440) + 1))
  n_reps_bypass <- max(1L, n_distinct(attributes_wide$replication))

  r2b_bypass_reason_daily <- attributes_wide %>%
    filter(!is.na(r2b_bypass_reason)) %>%
    mutate(
      day    = floor(r2b_bypass_time / 1440) + 1,
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

  # ── R2B casualty treatment summary ───────────────────────────────────────

  r2b_casualties <- combined %>%
    filter(!is.na(r2b_treated) & r2b_treated > 0) %>%
    mutate(
      r2b_station              = paste0("R2B ", r2b_treated),
      r2b_treatment_start_time = floor(start_time / 1440) + 1
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

  # ── R2B surgery summary ───────────────────────────────────────────────────

  r2b_summary <- attributes_wide %>%
    filter(!is.na(r2b_surgery_start)) %>%
    mutate(
      r2b_day     = floor(as.numeric(r2b_surgery_start) / 1440) + 1,
      r2b_station = paste0("R2B ", r2b_treated)
    ) %>%
    count(r2b_day, r2b_station, name = "surgeries")

  plot_r2b_summary <- ggplot(r2b_summary, aes(x = factor(r2b_day), y = surgeries, fill = r2b_station)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    labs(title = "R2B Surgeries Started per Simulation Day", x = "Simulation Day", y = "Casualties", fill = "R2B Station") +
    scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal(base_size = 14)

  # ── Casualties skipping R2B ───────────────────────────────────────────────

  skipped_r2b_daily <- attributes_wide %>%
    mutate(day = floor(time / 1440) + 1) %>%
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

  # ── R2B resource usage (Gantt) ────────────────────────────────────────────

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
    if (!is.finite(max_end)) NA_real_ else ceiling(max_end / 1440)
  }
  x_breaks <- if (is.finite(max_days) && max_days >= 1) seq(1, max_days, by = 1) else waiver()

  p_r2b_gantt <- ggplot(r2b_bed_usage, aes(y = resource_label, color = toupper(bed_type))) +
    geom_segment(aes(x = start_time / 1440, xend = end_time / 1440, yend = resource_label), linewidth = 6, lineend = "butt") +
    labs(title = "R2B Bed Resource Usage (Gantt) by Team", x = "Time (Days)", y = "Bed Resource", color = "Bed Type") +
    scale_x_continuous(breaks = x_breaks, labels = function(x) paste0(x), expand = c(0, 0)) +
    facet_wrap(~ r2b_team, ncol = 1, scales = "free_y", labeller = labeller(r2b_team = function(x) paste0("R2B ", x))) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray"), legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file.path(images_dir, "r2b_gantt.png"), p_r2b_gantt,
         width = 14, height = 10, dpi = 150)

  # ── R2E surgeries ─────────────────────────────────────────────────────────

  r2e_summary <- attributes_wide %>%
    dplyr::select(name, starts_with("r2e_surgery_")) %>%
    pivot_longer(cols = starts_with("r2e_surgery_"), names_to = "surgery_type", values_to = "start_min") %>%
    filter(!is.na(start_min)) %>%
    mutate(r2e_day = floor(start_min / 1440) + 1) %>%
    count(r2e_day, name = "surgeries")

  p_r2e_surgeries <- ggplot(r2e_summary, aes(x = r2e_day, y = surgeries)) +
    geom_bar(stat = "identity", fill = brewer.pal(n = 3, name = "Set2")[1]) +
    labs(title = "R2E Heavy Surgeries Completed Per Simulation Day", x = "Simulation Day", y = "Number of Surgeries") +
    scale_x_continuous(breaks = seq(1, max_days, by = 1), expand = c(0, 0)) +
    scale_y_continuous(breaks = 0:25, limits = c(0, 25)) +
    theme_minimal(base_size = 14)
  ggsave(file.path(images_dir, "r2eheavy_surgeries.png"), p_r2e_surgeries,
         width = 12, height = 6, dpi = 150)

  # ── R2E bed queue graphs ──────────────────────────────────────────────────

  prepare_queue_data <- function(resource_type, data) {
    pattern <- paste0("^b_r2eheavy_", resource_type, "_\\d+_t\\d+$")
    data %>%
      as.data.frame() %>%
      filter(grepl(pattern, resource)) %>%
      dplyr::select(time, resource, queue) %>%
      mutate(
        resource_type  = toupper(resource_type),
        bed_number     = gsub("^b_r2eheavy_.*?_(\\d+)_t\\d+$", "\\1", resource),
        r2e_number     = gsub("^b_r2eheavy_.*?_\\d+_t(\\d+)$", "\\1", resource),
        resource_label = paste(resource_type, "Bed", bed_number)
      )
  }

  combined_queue_data <- bind_rows(
    prepare_queue_data("ot",  resources),
    prepare_queue_data("icu", resources)
  )

  p_r2e_bed_queues <- ggplot(combined_queue_data, aes(x = time / 1440, y = queue, color = resource_label)) +
    geom_step(linewidth = 1) +
    labs(title = "R2E Heavy Bed Queue Length Over Time by Resource Type", x = "Time (Days)", y = "Queue Size", color = "Resource") +
    facet_wrap(~ resource_type, ncol = 1, scales = "fixed") +
    scale_x_continuous(breaks = seq(1, ceiling(max(combined_queue_data$time) / 1440), by = 1), labels = function(x) paste0(x), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1), expand = c(0, 0)) +
    theme_minimal(base_size = 14) +
    theme(strip.text = element_text(face = "bold"), legend.position = "bottom", panel.grid.minor = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray"))
  ggsave(file.path(images_dir, "r2eheavy_bed_queue_3_teams.png"), p_r2e_bed_queues,
         width = 12, height = 8, dpi = 150)

  # ── Waiting time scatter ──────────────────────────────────────────────────

  p_waiting_time <- ggplot(arrivals, aes(x = start_time / (60 * 24), y = waiting_time)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    labs(title = "Casualty Waiting Time Over Simulation", x = "Simulation Day", y = "Waiting Time (min)") +
    geom_smooth(method = "loess", se = FALSE, color = "darkred") +
    theme_minimal(base_size = 14)
  ggsave(file.path(images_dir, "waiting_time.png"), p_waiting_time,
         width = 12, height = 6, dpi = 150)

  # ── Transport fleet capacity margin (Issue #6) ────────────────────────────
  # Queue-over-time per pooled transport asset (PMV Ambulance, HX240M) shows
  # how much headroom the current fleet size has under the dead-heading
  # round trip: a queue that stays at 0 throughout indicates spare capacity;
  # sustained queue > 0 indicates the fleet is a binding constraint.

  transport_queue_data <- resources %>%
    as.data.frame() %>%
    filter(grepl("^t_(PMVAmb|HX240M)_\\d+$", resource)) %>%
    dplyr::select(time, resource, queue) %>%
    mutate(
      platform   = str_extract(resource, "(?<=^t_)[^_]+"),
      unit_id    = str_extract(resource, "\\d+$") %>% as.integer(),
      unit_label = paste(platform, unit_id)
    )

  p_transport_capacity_margin <- ggplot(transport_queue_data, aes(x = time / 1440, y = queue, color = unit_label)) +
    geom_step(linewidth = 1) +
    labs(title = "Transport Fleet Capacity Margin — Queue Over Time",
         subtitle = "Queue length by vehicle; sustained queue > 0 indicates the fleet is at capacity",
         x = "Time (Days)", y = "Queue Size", color = "Vehicle") +
    scale_x_continuous(breaks = seq(0, ceiling(max(transport_queue_data$time) / 1440), by = 1), expand = c(0, 0)) +
    facet_wrap(~ platform, ncol = 1, scales = "free_y") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray"), legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file.path(images_dir, "transport_capacity_margin.png"), p_transport_capacity_margin,
         width = 12, height = 8, dpi = 150)

  # KPI: transport utilisation by platform (mirrors the ot_utilisation pattern
  # below, but explicitly groups by resource before lead() so duration is
  # never computed across a resource boundary).
  transport_utilisation <- resources_raw %>%
    filter(grepl("^t_(PMVAmb|HX240M)_\\d+$", resource)) %>%
    arrange(resource, time) %>%
    group_by(resource) %>%
    mutate(
      platform = str_extract(resource, "(?<=^t_)[^_]+"),
      duration = lead(time) - time
    ) %>%
    ungroup() %>%
    filter(!is.na(duration) & duration > 0) %>%
    group_by(platform, resource) %>%
    summarise(
      busy_time = sum(server * duration, na.rm = TRUE),
      capacity  = max(capacity, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    group_by(platform) %>%
    summarise(
      utilisation = sum(busy_time) / (sum(capacity) * max(resources_raw$time, na.rm = TRUE)),
      .groups     = "drop"
    )
  write.csv(transport_utilisation, file.path(output_dir, "transport_utilisation.csv"), row.names = FALSE)

  # ── R2E resource usage (Gantt) ────────────────────────────────────────────

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

  max_days_r2e <- ceiling(max(r2e_bed_usage$end_time, na.rm = TRUE) / 1440)

  p_r2e_gantt <- ggplot(r2e_bed_usage, aes(y = resource_label, color = toupper(bed_type))) +
    geom_segment(aes(x = start_time / 1440, xend = end_time / 1440, yend = resource_label), linewidth = 6, lineend = "butt") +
    labs(title = "R2E Bed Resource Usage (Gantt)", x = "Time (Days)", y = "Bed Resource", color = "Bed Type") +
    scale_x_continuous(breaks = seq(1, max_days_r2e, by = 1), labels = function(x) paste0(x), expand = c(0, 0)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray"), legend.position = "bottom")
  ggsave(file.path(images_dir, "r2eheavy_gantt.png"), p_r2e_gantt,
         width = 14, height = 10, dpi = 150)

  # ── Output Variable Register derived KPIs ────────────────────────────────

  # KPI 1: Time from R1 arrival to first surgical incision (minutes)
  # Excludes KIA cases and DOW cases where death preceded any surgery.
  time_to_first_surgery <- combined %>%
    mutate(
      first_surgery_start = pmin(
        as.numeric(r2b_surgery_start),
        as.numeric(r2e_surgery_1_start),
        na.rm = TRUE
      ),
      time_to_surgery_min = first_surgery_start - start_time
    ) %>%
    filter(casualty_type != "kia",
           !(dow == 1 & is.na(first_surgery_start))) %>%
    filter(!is.na(time_to_surgery_min)) %>%
    summarise(
      mean_min = mean(time_to_surgery_min),
      p10_min  = quantile(time_to_surgery_min, 0.10),
      p90_min  = quantile(time_to_surgery_min, 0.90),
      n        = n()
    )

  # KPI 2: R2B dwell time (minutes) — arrival to departure
  r2b_dwell_time <- combined %>%
    filter(!is.na(r2b_treatment_start_time) & !is.na(r2b_departure_time)) %>%
    mutate(dwell_min = as.numeric(r2b_departure_time) - as.numeric(r2b_treatment_start_time)) %>%
    filter(dwell_min >= 0) %>%
    summarise(
      mean_min = mean(dwell_min),
      p90_min  = quantile(dwell_min, 0.90),
      n        = n()
    )

  # KPI 3: R2B → R2E transit time (minutes)
  r2b_r2e_transit_time <- combined %>%
    filter(!is.na(r2b_departure_time) & !is.na(r2e_arrival_time)) %>%
    mutate(transit_min = as.numeric(r2e_arrival_time) - as.numeric(r2b_departure_time)) %>%
    filter(transit_min >= 0) %>%
    summarise(
      mean_min = mean(transit_min),
      p90_min  = quantile(transit_min, 0.90),
      n        = n()
    )

  # KPI 4: R2E dwell time (minutes)
  r2e_dwell_time <- combined %>%
    filter(!is.na(r2e_arrival_time) & !is.na(r2e_departure_time)) %>%
    mutate(dwell_min = as.numeric(r2e_departure_time) - as.numeric(r2e_arrival_time)) %>%
    filter(dwell_min >= 0) %>%
    summarise(
      mean_min = mean(dwell_min),
      p90_min  = quantile(dwell_min, 0.90),
      n        = n()
    )

  # KPI 5: DOW count and rate by echelon
  # dow_echelon encoding: 1 = R1, 2 = R2B, 3 = R2E (arrival), 4 = R2E (post-operative, Issue #43)
  echelon_labels <- c("1" = "r1", "2" = "r2b", "3" = "r2e", "4" = "r2e_postop")
  total_dow <- sum(attributes_wide$dow == 1, na.rm = TRUE)
  dow_by_echelon <- attributes_wide %>%
    filter(dow == 1 & !is.na(dow_echelon)) %>%
    mutate(dow_echelon = echelon_labels[as.character(as.integer(dow_echelon))]) %>%
    count(dow_echelon, name = "dow_count") %>%
    mutate(dow_rate = dow_count / nrow(arrivals_raw))
  stopifnot(sum(dow_by_echelon$dow_count) == total_dow)

  # KPI 6: RTD count and rate by echelon
  # return_echelon encoding: 1 = R1, 2 = R2B, 3 = R2E
  # battle_fatigue RTDs (dnbi_type == 1) are returned at R1 without clinical treatment;
  # all other RTDs (WIA/NBI/disease recovery at R1, R2B/R2E hold-bed discharge) are clinical RTDs.
  bf_rtd <- sum(
    !is.na(attributes_wide$return_day) &
      !is.na(attributes_wide$dnbi_type) &
      attributes_wide$dnbi_type == 1L,
    na.rm = TRUE
  )
  clinical_rtd <- sum(
    !is.na(attributes_wide$return_day) &
      (is.na(attributes_wide$dnbi_type) | attributes_wide$dnbi_type != 1L),
    na.rm = TRUE
  )
  total_rtd <- bf_rtd + clinical_rtd
  stopifnot(total_rtd == sum(!is.na(attributes_wide$return_day)))
  rtd_by_echelon <- attributes_wide %>%
    filter(!is.na(return_day) & !is.na(return_echelon)) %>%
    mutate(
      return_echelon = echelon_labels[as.character(as.integer(return_echelon))],
      rtd_type       = if_else(!is.na(dnbi_type) & dnbi_type == 1L, "battle_fatigue", "clinical")
    ) %>%
    count(return_echelon, rtd_type, name = "rtd_count") %>%
    mutate(rtd_rate = rtd_count / nrow(arrivals_raw))
  stopifnot(sum(rtd_by_echelon$rtd_count) == total_rtd)

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

  # KPI 8: R2E OT-ICU gating post-operative pathway summary (Issue #43)
  # post_op_pathway: 1 = ICU (nominal), 2 = Post-Op Hold (ICU saturated, P1 override).
  # postop_dow uses dow_echelon == 4, the post-operative checkpoint added by Issue #43,
  # kept distinct from the Phase 1 R2E arrival DOW checkpoint (dow_echelon == 3) so the
  # two pathways' realised post-operative mortality can be directly compared.
  pathway_labels <- c("1" = "icu", "2" = "hold")
  post_op_pathway_summary <- NULL
  surgery_deferred_count  <- 0L
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
  cat(sprintf("R2E OT-ICU gating: surgery deferred (ICU saturated, P2+): %d\n", surgery_deferred_count))
  if (!is.null(post_op_pathway_summary)) print(post_op_pathway_summary)

  # ── R2E OT-ICU gating impact — sub-optimal and delayed care (Issue #43) ──
  # Visualises, by simulation day, where casualties experienced degraded care
  # specifically attributable to ICU saturation at the point of OT entry:
  # - Sub-Optimal Care: a Priority 1 candidate was operated on despite ICU
  #   being full; post-operative recovery occurred in a holding bed instead
  #   of ICU, carrying an elevated dow_ceiling (README — Died of Wounds,
  #   Post-Operative Checkpoint).
  # - Delayed Care: a Priority 2+ candidate had OT entry deferred while ICU
  #   was saturated, polling on a timer until a bed freed.
  # - Normal: ICU was available at the point of OT entry (gate had no effect).
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
        day = floor(as.numeric(r2e_surgery_1_start) / 1440) + 1
      )

    n_sim_days_icu <- ceiling(max(combined$start_time, na.rm = TRUE) / 1440)
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

  # ── KPI 9: Mass casualty event stress test analysis (Issue #9) ──
  # mass_casualty_event: 1 = casualty originated from a compound-Poisson
  # mass casualty injection event (R/environment.R::generate_mass_casualty_events()),
  # 0 = background lognormal generation. Individual events are reconstructed
  # from tagged casualties' arrival times by clustering consecutive arrivals
  # (within each replication) whose inter-arrival gap does not exceed the
  # configured mass casualty injection window (env_data$vars$mass_casualty$event$
  # window_max) — casualties from the same event arrive closer together than
  # this gap by construction (see generate_mass_casualty_events()).
  mass_casualty_gap_min <- env_data$vars$mass_casualty$event$window_max

  mass_casualty_tagged <- combined %>%
    filter(!is.na(mass_casualty_event) & mass_casualty_event == 1)

  if (nrow(mass_casualty_tagged) > 0) {
    mass_casualty_events_summary <- mass_casualty_tagged %>%
      arrange(replication, start_time) %>%
      group_by(replication) %>%
      mutate(
        gap      = start_time - lag(start_time, default = -Inf),
        event_id = cumsum(gap > mass_casualty_gap_min)
      ) %>%
      group_by(replication, event_id) %>%
      summarise(
        event_start = min(start_time),
        event_end   = max(start_time),
        n_cas       = n(),
        .groups     = "drop"
      ) %>%
      mutate(event_day = floor(event_start / 1440) + 1)
  } else {
    mass_casualty_events_summary <- data.frame(
      replication = integer(0), event_id = integer(0), event_start = numeric(0),
      event_end = numeric(0), n_cas = integer(0), event_day = numeric(0)
    )
  }

  mass_casualty_event_count <- nrow(mass_casualty_events_summary)

  # DOW rate comparison: casualties originating from a mass casualty surge vs.
  # background-generated casualties, as a proxy for elevated mortality under
  # mass casualty surge conditions (see README Limitations for the
  # window-vs-origin comparison design choice).
  mass_casualty_dow_summary <- combined %>%
    filter(!is.na(mass_casualty_event)) %>%
    mutate(origin = if_else(mass_casualty_event == 1, "Mass Casualty Event", "Background")) %>%
    group_by(origin) %>%
    summarise(
      total    = n(),
      dow      = sum(dow == 1, na.rm = TRUE),
      dow_rate = dow / total,
      .groups  = "drop"
    )

  mass_casualty_timeline_plot <- NULL
  if (mass_casualty_event_count > 0) {
    n_sim_days_mass_casualty <- ceiling(max(combined$start_time, na.rm = TRUE) / 1440)

    mass_casualty_timeline_plot <- ggplot(mass_casualty_events_summary,
                                   aes(x = event_start / 1440, y = n_cas)) +
      geom_segment(aes(xend = event_start / 1440, y = 0, yend = n_cas), color = "#D62828") +
      geom_point(size = 3, color = "#D62828") +
      scale_x_continuous(limits = c(0, n_sim_days_mass_casualty),
                         breaks = seq(0, n_sim_days_mass_casualty, by = 2)) +
      labs(
        title    = "Mass Casualty Event Timeline",
        subtitle = sprintf(
          "%d event(s) across the simulation period (compound Poisson injection, Issue #9)",
          mass_casualty_event_count
        ),
        x = "Simulation Day", y = "Casualties Injected by Event"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    if (n_distinct(mass_casualty_events_summary$replication) > 1) {
      mass_casualty_timeline_plot <- mass_casualty_timeline_plot + facet_wrap(~ replication, ncol = 1)
    }

    ggsave(file.path(images_dir, "mass_casualty_events.png"), mass_casualty_timeline_plot,
           width = 12, height = 6, dpi = 150)
  }

  write.csv(mass_casualty_events_summary, file.path(output_dir, "mass_casualty_events_summary.csv"),
           row.names = FALSE)
  write.csv(mass_casualty_dow_summary,    file.path(output_dir, "mass_casualty_dow_summary.csv"),
           row.names = FALSE)

  write.csv(dow_by_echelon,  file.path(output_dir, "dow_by_echelon.csv"),  row.names = FALSE)
  write.csv(rtd_by_echelon,  file.path(output_dir, "rtd_by_echelon.csv"),  row.names = FALSE)
  write.csv(ot_utilisation,  file.path(output_dir, "ot_utilisation.csv"),  row.names = FALSE)

  invisible(list(
    # Named ggplot objects (Issue #14) — embeddable directly in a Shiny
    # reactive context (e.g. renderPlot(results()$casualty_flow)) without
    # re-running the analysis pipeline. ggsave() above still writes the same
    # PNGs to images_dir for the CLI/report path.
    casualty_flow               = p_casualty_summary,
    r1_queues                   = p_r1_queues,
    r2b_treatment               = p_r2b_handling,
    r2b_bed_queues              = p_r2b_bed_queues,
    r2b_gantt                   = p_r2b_gantt,
    r2e_surgery                 = p_r2e_surgeries,
    r2e_bed_queues              = p_r2e_bed_queues,
    waiting_times               = p_waiting_time,
    r2e_gantt                   = p_r2e_gantt,
    combined                    = combined,
    casualty_summary            = casualty_summary,
    casualty_priority_summary   = casualty_priority_summary,
    attributes_wide             = attributes_wide,
    r2b_summary                 = r2b_summary,
    r2e_summary                 = r2e_summary,
    time_to_first_surgery       = time_to_first_surgery,
    r2b_dwell_time              = r2b_dwell_time,
    r2b_r2e_transit_time        = r2b_r2e_transit_time,
    r2e_dwell_time              = r2e_dwell_time,
    dow_by_echelon              = dow_by_echelon,
    bf_rtd                      = bf_rtd,
    clinical_rtd                = clinical_rtd,
    total_rtd                   = total_rtd,
    rtd_by_echelon              = rtd_by_echelon,
    ot_utilisation              = ot_utilisation,
    r2b_hold_daily              = r2b_hold_daily,
    r2b_hold_occupancy_plot     = r2b_hold_occupancy_plot,
    r2b_pre_bypass_count        = r2b_pre_bypass_count,
    r2b_hold_bypass_count       = r2b_hold_bypass_count,
    r2b_hold_queued_count       = r2b_hold_queued_count,
    r2b_ot_bypass_offshift_count = r2b_ot_bypass_offshift_count,
    r2b_ot_bypass_busy_count    = r2b_ot_bypass_busy_count,
    r2b_ot_bypass_count         = r2b_ot_bypass_count,
    r2b_bypass_reason_plot      = r2b_bypass_reason_plot,
    transport_utilisation       = transport_utilisation,
    transport_capacity_margin_plot = p_transport_capacity_margin,
    post_op_pathway_summary     = post_op_pathway_summary,
    surgery_deferred_count      = surgery_deferred_count,
    r2e_icu_gating_daily        = r2e_icu_gating_daily,
    r2e_icu_gating_plot         = r2e_icu_gating_plot,
    mass_casualty_events_summary       = mass_casualty_events_summary,
    mass_casualty_event_count          = mass_casualty_event_count,
    mass_casualty_dow_summary          = mass_casualty_dow_summary,
    mass_casualty_timeline_plot        = mass_casualty_timeline_plot
  ))
}

#' Bin a resource-queue subset into regular time bins per (replication,
#' resource), then summarise mean ± 95% CI per bin across replications,
#' grouped by the given label column(s) already present in `data`.
#'
#' @param data Resource-monitor subset with columns replication, resource,
#'   time, queue, and every column named in group_cols.
#' @param group_cols Character vector of label columns to facet/aggregate
#'   by, e.g. c("r1_label", "role_label") — each (replication, resource)
#'   combination must map to exactly one combination of these labels.
#' @param bin_size_min Width of each time bin in minutes (default 240 — 6
#'   bins/day; coarse enough to stay tractable at up to 1000 replications
#'   while still showing queue dynamics across the run).
#' @return list(traces = per-(replication, resource) binned queue, for a
#'   faint per-replication overlay trace; ci = mean/CI per bin per group,
#'   for the ribbon).
#'
#' @details Step-interpolates each resource's queue trace onto a shared grid
#'   of time bins (mirrors bin_icu_queue(), R/warmup.R), then computes a
#'   t-distribution 95% CI across replications at each bin. sd is coerced to
#'   0 when only one replication contributes to a bin (n < 2), so the CI
#'   collapses to the mean rather than propagating NA.
bin_queue_ci <- function(data, group_cols, bin_size_min = 240) {
  max_time <- max(data$time, na.rm = TRUE)
  bins     <- seq(0, max_time, by = bin_size_min)

  traces <- data %>%
    group_by(across(all_of(c("replication", "resource", group_cols)))) %>%
    group_modify(function(d, key) {
      q <- approx(d$time, d$queue, xout = bins, method = "constant", rule = 2)$y
      data.frame(bin_min = bins, queue = q)
    }) %>%
    ungroup()

  ci <- traces %>%
    group_by(across(all_of(c("bin_min", group_cols)))) %>%
    summarise(
      n      = n(),
      mean_q = mean(queue, na.rm = TRUE),
      sd_q   = sd(queue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      sd_q     = ifelse(n < 2 | is.na(sd_q), 0, sd_q),
      ci_lower = pmax(mean_q - qt(0.975, df = pmax(n - 1, 1)) * sd_q / sqrt(n), 0),
      ci_upper = mean_q + qt(0.975, df = pmax(n - 1, 1)) * sd_q / sqrt(n)
    )

  list(traces = traces, ci = ci)
}

#' Per-replication resource utilisation (busy-time fraction) matching a
#' resource-name pattern, one row per replication.
#'
#' @param resources Resource-monitor data frame (with replication column)
#' @param pattern   Regex pattern matched against the resource column
#' @return Data frame: replication, utilisation (0-1)
#'
#' @details Mirrors compute_utilisation() (R/sensitivity.R), but keeps the
#'   per-replication value rather than collapsing straight to a single
#'   across-replication mean, so callers can compute a CI across
#'   replications rather than only a point estimate.
utilisation_per_replication <- function(resources, pattern) {
  sub <- resources %>% filter(grepl(pattern, resource))
  if (nrow(sub) == 0) {
    return(data.frame(replication = integer(0), utilisation = numeric(0)))
  }
  obs_window <- sub %>%
    group_by(replication) %>%
    summarise(obs_window = max(time, na.rm = TRUE), .groups = "drop")
  sub %>%
    arrange(replication, resource, time) %>%
    group_by(replication, resource) %>%
    mutate(duration = lead(time, default = dplyr::last(time)) - time) %>%
    ungroup() %>%
    filter(!is.na(duration) & duration > 0) %>%
    group_by(replication, resource) %>%
    summarise(busy_time = sum(server * duration, na.rm = TRUE),
              capacity  = max(capacity, na.rm = TRUE), .groups = "drop") %>%
    group_by(replication) %>%
    summarise(busy_time = sum(busy_time), capacity = sum(capacity), .groups = "drop") %>%
    left_join(obs_window, by = "replication") %>%
    mutate(utilisation = busy_time / (capacity * obs_window)) %>%
    dplyr::select(replication, utilisation)
}

#' Mean and 95% CI (t-distribution) of a numeric vector across replications
#'
#' @param x Numeric vector, one value per replication
#' @return Named numeric vector: mean, lower, upper, n. When n < 2, lower and
#'   upper both equal mean (no CI is estimable from a single replication).
ci_mean <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- if (n > 0) mean(x) else NA_real_
  if (n < 2) return(c(mean = m, lower = m, upper = m, n = n))
  s <- sd(x)
  e <- qt(0.975, df = n - 1) * s / sqrt(n)
  c(mean = m, lower = m - e, upper = m + e, n = n)
}

#' Runs the Full Analysis (multi-replication) visualisation pipeline
#'
#' @param mon Named list with elements arrivals, attributes, resources as
#'   returned by run_replications() — each carrying a `replication` column
#' @param warm_up_period Days to exclude from the start of the analysis
#'   window (Welch warm-up period; default WARM_UP_DAYS, R/warmup.R — 0 for
#'   this terminating-simulation model, i.e. no exclusion at baseline)
#' @param output_dir Directory path for saving CSV outputs (default "outputs")
#' @param images_dir Directory path for saving PNG plots (default "images")
#' @return Invisibly returns a named list: casualty_flow, r1_queues,
#'   r2b_bed_queues, r2e_bed_queues, utilisation, waiting_times (ggplot
#'   objects, CI-ribbon/error-bar variants of analyse_run()'s single-run
#'   plots — embeddable directly via renderPlot()); kpi_summary (named list
#'   of ci_mean() vectors: total_casualties, dow_count, r2e_icu_peak_queue,
#'   r2b_ot_peak_queue); n_reps; arrivals/attributes/resources (the filtered
#'   monitoring data frames, for CSV export).
#'
#' @details Issue #15's Full Analysis counterpart to analyse_run(): every
#'   plot shows a mean ± 95% CI ribbon (or error bar, for the single-number
#'   utilisation summary) across replications, with faint per-replication
#'   traces overlaid on the queue-depth plots (alpha ≈ 0.1) so an individual
#'   replication's trajectory remains visible under the aggregate. There is
#'   no per-replication Gantt or per-bed Gantt-style plot — a Gantt of
#'   individual bed occupancy has no meaningful multi-replication analogue —
#'   so the Bed & Resource Utilisation tab is instead a single mean ± CI
#'   utilisation bar chart per resource group. Replication count guidance,
#'   CI interpretation, and warm-up exclusion are documented in the README
#'   (Shiny Application — Full Analysis Mode), citing Romero-Brufau et al.
#'   (2020) for minimum replication counts in DES healthcare studies.
analyse_replications <- function(mon, warm_up_period = WARM_UP_DAYS,
                                 output_dir = "outputs", images_dir = "images") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(images_dir,  showWarnings = FALSE, recursive = TRUE)

  warm_up_min    <- as.integer(warm_up_period) * 1440L
  arrivals_raw   <- mon$arrivals    %>% filter(start_time >= warm_up_min)
  attributes_raw <- mon$attributes
  resources_raw  <- mon$resources   %>% filter(time >= warm_up_min)

  write.csv(arrivals_raw,   file.path(output_dir, "mon_arrivals.csv"),   row.names = FALSE)
  write.csv(attributes_raw, file.path(output_dir, "mon_attributes.csv"), row.names = FALSE)
  write.csv(resources_raw,  file.path(output_dir, "mon_resources.csv"),  row.names = FALSE)

  n_reps <- n_distinct(arrivals_raw$replication)

  arrivals <- arrivals_raw %>%
    mutate(arrival_day = floor(start_time / 1440) + 1)

  # ── KPI summary cards ─────────────────────────────────────────────────────

  total_cas_per_rep <- arrivals_raw %>% count(replication, name = "total")

  dow_per_rep <- attributes_raw %>%
    filter(key == "dow", value == 1) %>%
    count(replication, name = "dow_count") %>%
    right_join(data.frame(replication = unique(arrivals_raw$replication)), by = "replication") %>%
    mutate(dow_count = coalesce(dow_count, 0L))

  peak_queue_per_rep <- function(resources, pattern) {
    resources %>%
      filter(grepl(pattern, resource)) %>%
      group_by(replication, time) %>%
      summarise(total_q = sum(queue), .groups = "drop") %>%
      group_by(replication) %>%
      summarise(peak_q = max(total_q), .groups = "drop")
  }
  r2e_icu_peak_per_rep <- peak_queue_per_rep(resources_raw, "^b_r2eheavy_icu_")
  r2b_ot_peak_per_rep  <- peak_queue_per_rep(resources_raw, "^b_r2b_ot_")

  # All four KPIs here are non-negative counts by construction; clamp the
  # lower CI bound at 0 rather than let a small-n/high-variance t-interval
  # (e.g. DOW count at low replication counts) report a nonsensical negative
  # lower bound.
  clamp_ci <- function(cm) { cm[["lower"]] <- max(cm[["lower"]], 0); cm }

  kpi_summary <- list(
    total_casualties   = clamp_ci(ci_mean(total_cas_per_rep$total)),
    dow_count          = clamp_ci(ci_mean(dow_per_rep$dow_count)),
    r2e_icu_peak_queue = clamp_ci(ci_mean(r2e_icu_peak_per_rep$peak_q)),
    r2b_ot_peak_queue  = clamp_ci(ci_mean(r2b_ot_peak_per_rep$peak_q))
  )

  # ── Casualty Flow — total casualties per day, mean ± CI across reps ──────

  day_range <- seq(min(arrivals$arrival_day), max(arrivals$arrival_day))
  daily_totals <- expand.grid(replication = unique(arrivals$replication), arrival_day = day_range) %>%
    left_join(count(arrivals, replication, arrival_day, name = "count"),
              by = c("replication", "arrival_day")) %>%
    mutate(count = coalesce(count, 0L))

  daily_ci <- daily_totals %>%
    group_by(arrival_day) %>%
    summarise(n = n(), mean_n = mean(count), sd_n = sd(count), .groups = "drop") %>%
    mutate(
      sd_n     = ifelse(n < 2 | is.na(sd_n), 0, sd_n),
      ci_lower = pmax(mean_n - qt(0.975, df = pmax(n - 1, 1)) * sd_n / sqrt(n), 0),
      ci_upper = mean_n + qt(0.975, df = pmax(n - 1, 1)) * sd_n / sqrt(n)
    )

  p_casualty_flow_ci <- ggplot() +
    geom_line(data = daily_totals, aes(x = arrival_day, y = count, group = replication),
              color = "steelblue", alpha = 0.1) +
    geom_ribbon(data = daily_ci, aes(x = arrival_day, ymin = ci_lower, ymax = ci_upper),
                fill = "steelblue", alpha = 0.3) +
    geom_line(data = daily_ci, aes(x = arrival_day, y = mean_n), color = "steelblue4", linewidth = 1) +
    scale_x_continuous(breaks = day_range) +
    labs(title = "Total Casualties by Arrival Day — Mean ± 95% CI Across Replications",
         subtitle = sprintf("%d replications; faint lines are individual replication traces", n_reps),
         x = "Arrival Day", y = "Casualties per Day") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())

  # ── Queue depth CI ribbons by echelon ─────────────────────────────────────

  r1_data <- resources_raw %>%
    filter(grepl("^c_r1_.*_\\d+_t\\d+$", resource)) %>%
    mutate(
      r1_id      = str_extract(resource, "_t\\d+$") %>% str_remove("_t") %>% as.integer(),
      role       = str_extract(resource, "(?<=c_r1_)[^_]+_[^_]+") %>% str_replace_all("_", " ") %>% tools::toTitleCase(),
      role_idx   = str_extract(resource, "(?<=_)[0-9]+(?=_t)") %>% as.integer(),
      r1_label   = paste0("R1 ", r1_id),
      role_label = paste0(role, " ", role_idx)
    )
  r1_bins <- bin_queue_ci(r1_data, c("r1_label", "role_label"))
  p_r1_queues_ci <- ggplot() +
    geom_step(data = r1_bins$traces,
              aes(x = bin_min / 1440, y = queue, group = interaction(replication, resource)),
              color = "steelblue", alpha = 0.06) +
    geom_ribbon(data = r1_bins$ci, aes(x = bin_min / 1440, ymin = ci_lower, ymax = ci_upper, fill = role_label),
                alpha = 0.3) +
    geom_line(data = r1_bins$ci, aes(x = bin_min / 1440, y = mean_q, color = role_label), linewidth = 0.9) +
    facet_wrap(~ r1_label, ncol = 1, scales = "free_y") +
    labs(title = "R1 Queue Length Over Time — Mean ± 95% CI Across Replications",
         subtitle = sprintf("%d replications; faint traces are individual replications", n_reps),
         x = "Time (Days)", y = "Queue Size", color = "Role", fill = "Role") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom", strip.text = element_text(face = "bold"))

  r2b_data <- resources_raw %>%
    filter(grepl("^b_r2b_.*_\\d+_t\\d+$", resource)) %>%
    mutate(
      r2b_id    = str_extract(resource, "_t\\d+$") %>% str_remove("_t") %>% as.integer(),
      bed_type  = str_extract(resource, "(?<=b_r2b_)[^_]+") %>% toupper(),
      bed_index = str_extract(resource, "(?<=_)[0-9]+(?=_t)") %>% as.integer(),
      r2b_label = paste0("R2B ", r2b_id),
      bed_label = paste0(bed_type, " ", bed_index)
    )
  r2b_bins <- bin_queue_ci(r2b_data, c("r2b_label", "bed_label"))
  p_r2b_queues_ci <- ggplot() +
    geom_step(data = r2b_bins$traces,
              aes(x = bin_min / 1440, y = queue, group = interaction(replication, resource)),
              color = "steelblue", alpha = 0.06) +
    geom_ribbon(data = r2b_bins$ci, aes(x = bin_min / 1440, ymin = ci_lower, ymax = ci_upper, fill = bed_label),
                alpha = 0.3) +
    geom_line(data = r2b_bins$ci, aes(x = bin_min / 1440, y = mean_q, color = bed_label), linewidth = 0.9) +
    facet_wrap(~ r2b_label, ncol = 1, scales = "free_x") +
    labs(title = "R2B Queue Length Over Time — Mean ± 95% CI Across Replications",
         subtitle = sprintf("%d replications; faint traces are individual replications", n_reps),
         x = "Time (Days)", y = "Queue Size", color = "Bed", fill = "Bed") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom", strip.text = element_text(face = "bold"))

  r2e_prepare <- function(resource_type) {
    pattern <- paste0("^b_r2eheavy_", resource_type, "_\\d+_t\\d+$")
    resources_raw %>%
      filter(grepl(pattern, resource)) %>%
      mutate(
        resource_type  = toupper(resource_type),
        bed_number     = gsub("^b_r2eheavy_.*?_(\\d+)_t\\d+$", "\\1", resource),
        resource_label = paste(resource_type, "Bed", bed_number)
      )
  }
  r2e_data <- bind_rows(r2e_prepare("ot"), r2e_prepare("icu"))
  r2e_bins <- bin_queue_ci(r2e_data, c("resource_type", "resource_label"))
  p_r2e_queues_ci <- ggplot() +
    geom_step(data = r2e_bins$traces,
              aes(x = bin_min / 1440, y = queue, group = interaction(replication, resource)),
              color = "steelblue", alpha = 0.06) +
    geom_ribbon(data = r2e_bins$ci,
                aes(x = bin_min / 1440, ymin = ci_lower, ymax = ci_upper, fill = resource_label), alpha = 0.3) +
    geom_line(data = r2e_bins$ci, aes(x = bin_min / 1440, y = mean_q, color = resource_label), linewidth = 0.9) +
    facet_wrap(~ resource_type, ncol = 1, scales = "fixed") +
    labs(title = "R2E Heavy Bed Queue Length Over Time — Mean ± 95% CI Across Replications",
         subtitle = sprintf("%d replications; faint traces are individual replications", n_reps),
         x = "Time (Days)", y = "Queue Size", color = "Resource", fill = "Resource") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom", strip.text = element_text(face = "bold"))

  # ── Bed & Resource Utilisation — mean ± CI bar chart (no multi-run Gantt) ─

  util_groups <- list(
    "R2B OT"                     = "^b_r2b_ot_",
    "R2E OT"                     = "^b_r2eheavy_ot_",
    "R2E ICU"                    = "^b_r2eheavy_icu_",
    "Transport (PMVAmb + HX240M)" = "^t_(PMVAmb|HX240M)_"
  )
  util_ci <- bind_rows(lapply(names(util_groups), function(g) {
    df <- utilisation_per_replication(resources_raw, util_groups[[g]])
    if (nrow(df) == 0) return(NULL)
    cm <- ci_mean(df$utilisation)
    data.frame(resource_group = g, mean_u = cm[["mean"]], ci_lower = max(cm[["lower"]], 0),
               ci_upper = min(cm[["upper"]], 1))
  }))

  p_utilisation_ci <- ggplot(util_ci, aes(x = resource_group, y = mean_u, fill = resource_group)) +
    geom_col(width = 0.6) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
    labs(title = "Resource Utilisation — Mean ± 95% CI Across Replications",
         subtitle = sprintf("%d replications; bar = mean busy-time fraction, error bar = 95%% CI", n_reps),
         x = NULL, y = "Utilisation") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))

  # ── Waiting Times — p10-p90 quantile band across the pooled replications ─

  arrivals_wait <- arrivals_raw %>%
    mutate(waiting_time = end_time - start_time - activity_time,
           arrival_day  = floor(start_time / 1440) + 1) %>%
    filter(!is.na(waiting_time))

  waiting_by_day <- arrivals_wait %>%
    group_by(arrival_day) %>%
    summarise(p10 = quantile(waiting_time, 0.10, na.rm = TRUE),
              median = quantile(waiting_time, 0.50, na.rm = TRUE),
              p90 = quantile(waiting_time, 0.90, na.rm = TRUE),
              .groups = "drop")

  p_waiting_times_ci <- ggplot(waiting_by_day, aes(x = arrival_day)) +
    geom_ribbon(aes(ymin = p10, ymax = p90), fill = "steelblue", alpha = 0.3) +
    geom_line(aes(y = median), color = "steelblue4", linewidth = 1) +
    scale_x_continuous(breaks = day_range) +
    labs(title = "Casualty Waiting Time by Arrival Day — p10-p90 Band Across Replications",
         subtitle = sprintf("%d replications pooled; band = 10th-90th percentile, line = median", n_reps),
         x = "Arrival Day", y = "Waiting Time (min)") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())

  invisible(list(
    casualty_flow  = p_casualty_flow_ci,
    r1_queues      = p_r1_queues_ci,
    r2b_bed_queues = p_r2b_queues_ci,
    r2e_bed_queues = p_r2e_queues_ci,
    utilisation    = p_utilisation_ci,
    waiting_times  = p_waiting_times_ci,
    kpi_summary    = kpi_summary,
    n_reps         = n_reps,
    arrivals       = arrivals_raw,
    attributes     = attributes_raw,
    resources      = resources_raw
  ))
}

#' Compute replication-level mean queue and utilisation for matching resources
#'
#' @param mon Named list with a `resources` element as returned by
#'   run_replications() (R/replication.R)
#' @param pattern Regex matched against resource IDs (e.g. "^t_PMVAmb_")
#' @return Data frame with one row per replication: replication, mean_q, mean_util
#'
#' @details Combines the time-weighted per-resource queue mean used by
#'   summarise_replications() and the time-weighted per-resource utilisation
#'   mean used by compute_utilisation() (both R/replication.R /
#'   R/sensitivity.R) in a single pass over the matching resources, then
#'   averages across resources within each replication. Unlike those two
#'   functions, which aggregate straight across all replications,
#'   this returns one row per replication — the unit
#'   plot_transport_capacity_margin_by_fleet_size() needs to compute a 95%
#'   CI across replications at each fleet-size sweep point.
transport_rep_kpis <- function(mon, pattern) {
  resource_means <- mon$resources %>%
    filter(grepl(pattern, resource)) %>%
    group_by(replication, resource) %>%
    arrange(time) %>%
    mutate(dt = lead(time, default = max(time)) - time) %>%
    summarise(
      resource_mean_q    = weighted.mean(queue, w = pmax(dt, 0), na.rm = TRUE),
      resource_mean_util = weighted.mean(server / pmax(capacity, 1), w = pmax(dt, 0), na.rm = TRUE),
      .groups = "drop"
    )

  resource_means %>%
    group_by(replication) %>%
    summarise(
      mean_q    = mean(resource_mean_q, na.rm = TRUE),
      mean_util = mean(resource_mean_util, na.rm = TRUE),
      .groups   = "drop"
    )
}

#' Plot medevac fleet capacity margin across a range of fleet sizes
#'
#' @param fleet_sizes Named list of integer vectors to sweep, e.g.
#'   list(PMVAmb = 1:5, HX240M = 1:4). Each element replaces the `qty` for
#'   the matching entry in env_data.json's `transports` block for that sweep
#'   point; the other vehicle type is held at its current establishment qty.
#' @param n_days Simulation duration per replication (default 30).
#' @param n_rep  Replications per fleet-size point, for CI bounds (default 5).
#' @param path File path to env_data.json (default "env_data.json").
#' @param output_dir Directory for CSV output (default "outputs").
#' @param images_dir Directory for the saved plot (default "images").
#' @return Named list: data (swept results, one row per vehicle x fleet
#'   size, with mean/95% CI for queue and utilisation), plot (ggplot object,
#'   also saved to images_dir/transport_capacity_margin_by_fleet_size.png)
#'
#' @details For each vehicle type in `fleet_sizes` and each qty in its swept
#'   range, deep-copies the parsed env_data.json, overwrites that vehicle's
#'   `transports[[]]$qty` (the other vehicle type stays at its current
#'   establishment qty), rebuilds via build_environment(), and runs n_rep
#'   replications via run_replications() (R/replication.R) — the same
#'   replication engine the comparative scenario runner (Issue #10,
#'   R/scenario_runner.R) uses, reused here directly rather than duplicated.
#'   Fleet size is a structural (`elms`/`transports`) change, which
#'   load_scenario()'s scenario-profile overlay mechanism does not cover
#'   (see load_scenario()'s @details, R/environment.R) — the sweep therefore
#'   edits the parsed JSON directly rather than going through run_scenario().
#'   Per-replication queue/utilisation means are extracted with
#'   transport_rep_kpis() (built on the same time-weighted-mean logic as
#'   summarise_replications() and compute_utilisation(), used by
#'   R/sensitivity.R's extract_kpis()) and aggregated to a mean and
#'   t-distribution 95% CI across replications at each sweep point,
#'   mirroring summarise_scenario_totals()'s aggregation convention
#'   (R/scenario_runner.R). Queue CI lower bounds are clamped to 0 and
#'   utilisation CI bounds to [0, 1], consistent with the CI-clamping
#'   convention used elsewhere in this file (e.g. p_r1_queues_ci,
#'   p_utilisation_ci). The global env_data/day_min/counts are restored to
#'   their pre-call values on completion, consistent with run_morris()'s
#'   env_data_base restore pattern (R/sensitivity.R).
plot_transport_capacity_margin_by_fleet_size <- function(fleet_sizes = list(PMVAmb = 1:5, HX240M = 1:4),
                                                          n_days = 30, n_rep = 5,
                                                          path = "env_data.json",
                                                          output_dir = "outputs", images_dir = "images") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(images_dir,  showWarnings = FALSE, recursive = TRUE)

  env_data_base <- env_data
  day_min_base  <- day_min
  counts_base   <- counts

  json_data_base <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  current_qty <- setNames(
    vapply(json_data_base$transports, function(t) t$qty, numeric(1)),
    vapply(json_data_base$transports, function(t) t$name, character(1))
  )

  summarise_ci <- function(x) {
    n <- length(x)
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    data.frame(
      mean     = m,
      ci_lower = m - qt(0.975, df = pmax(n - 1, 1)) * s / sqrt(n),
      ci_upper = m + qt(0.975, df = pmax(n - 1, 1)) * s / sqrt(n)
    )
  }

  sweep_df <- bind_rows(lapply(names(fleet_sizes), function(vehicle) {
    pattern <- paste0("^t_", vehicle, "_")

    bind_rows(lapply(fleet_sizes[[vehicle]], function(qty) {
      message(sprintf("Transport fleet-size sweep: %s = %d (%d reps x %d days)...",
                      vehicle, qty, n_rep, n_days))

      json_data <- json_data_base
      for (i in seq_along(json_data$transports)) {
        if (identical(json_data$transports[[i]]$name, vehicle)) {
          json_data$transports[[i]]$qty <- qty
        }
      }

      env_data <<- build_environment(json_data)
      day_min  <<- 1440L
      counts   <<- sapply(env_data$elms, length)

      mon      <- run_replications(n_rep, n_days)
      rep_kpis <- transport_rep_kpis(mon, pattern)

      q_stats    <- summarise_ci(rep_kpis$mean_q)
      util_stats <- summarise_ci(rep_kpis$mean_util)

      data.frame(
        vehicle       = vehicle,
        qty           = qty,
        mean_q        = q_stats$mean,
        ci_lower_q    = pmax(q_stats$ci_lower, 0),
        ci_upper_q    = q_stats$ci_upper,
        mean_util     = util_stats$mean,
        ci_lower_util = pmax(util_stats$ci_lower, 0),
        ci_upper_util = pmin(util_stats$ci_upper, 1)
      )
    }))
  }))

  env_data <<- env_data_base
  day_min  <<- day_min_base
  counts   <<- counts_base

  write.csv(sweep_df, file.path(output_dir, "transport_capacity_by_fleet_size.csv"), row.names = FALSE)
  message(sprintf("Transport fleet-size sweep results written to %s/transport_capacity_by_fleet_size.csv",
                  output_dir))

  current_qty_df <- data.frame(
    vehicle = names(current_qty),
    qty     = as.numeric(current_qty)
  ) %>% filter(vehicle %in% names(fleet_sizes))

  plot_df <- bind_rows(
    sweep_df %>% transmute(vehicle, qty, metric = "Mean Queue",
                           mean = mean_q, ci_lower = ci_lower_q, ci_upper = ci_upper_q),
    sweep_df %>% transmute(vehicle, qty, metric = "Mean Utilisation",
                           mean = mean_util, ci_lower = ci_lower_util, ci_upper = ci_upper_util)
  ) %>%
    mutate(
      vehicle = factor(vehicle, levels = names(fleet_sizes)),
      metric  = factor(metric, levels = c("Mean Queue", "Mean Utilisation"))
    )

  vline_df <- expand.grid(vehicle = names(fleet_sizes),
                          metric  = levels(plot_df$metric),
                          stringsAsFactors = FALSE) %>%
    mutate(vehicle = factor(vehicle, levels = names(fleet_sizes))) %>%
    left_join(current_qty_df %>% mutate(vehicle = factor(vehicle, levels = names(fleet_sizes))),
              by = "vehicle")

  p <- ggplot(plot_df, aes(x = qty, y = mean)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.3) +
    geom_line(color = "steelblue4", linewidth = 1) +
    geom_point(color = "steelblue4", size = 2) +
    geom_vline(data = vline_df, aes(xintercept = qty),
              linetype = "dashed", color = "firebrick") +
    facet_grid(metric ~ vehicle, scales = "free_y") +
    scale_x_continuous(breaks = function(lims) seq(floor(lims[1]), ceiling(lims[2]), by = 1)) +
    labs(title = "Transport Fleet Capacity Margin by Fleet Size",
         subtitle = sprintf("%d replications per fleet-size point; ribbon = 95%% CI; dashed line = current establishment",
                            n_rep),
         x = "Fleet Size (vehicles)", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))

  ggsave(file.path(images_dir, "transport_capacity_margin_by_fleet_size.png"), p,
        width = 12, height = 8, dpi = 150)

  list(data = sweep_df, plot = p)
}
