##############################################
## R/analysis_single_run.R                  ##
## Single-run analysis pipeline             ##
##############################################
#
# The stage functions of one run's analysis, each producing one domain's
# summaries, tables or figures, and analyse_run(), the orchestrator over them.
#
# Sourced by R/analysis.R, which loads the packages and the shared modules
# these functions use; source that file rather than this one alone.

#' Group-wise mean +/- 95% t-distribution CI across replications
#'
#' @param df Per-replication values, one row per (group_cols, replication)
#'   combination — every combination that could occur should have an
#'   explicit row (fill with 0 first) so a replication with zero occurrences
#'   is counted as 0, not silently dropped from the mean (see
#'   role4_census_daily's roxygen in analyse_run() for why this matters)
#' @param group_cols Character vector of columns to group by (may be
#'   length 0, for a single overall CI)
#' @param value_col Column name (string) holding the per-replication value
#' @param clamp_lower_zero Clamp `ci_lower` to 0 (default TRUE) — every KPI
#'   this is currently used for is a non-negative count or rate
#' @return `df` grouped by `group_cols`, collapsed to one row per group, with
#'   columns n, mean, sd, ci_lower, ci_upper
#'
#' @details Same t-distribution construction used ad hoc elsewhere in this
#'   file (daily_ci/force_regeneration_ci in analyse_replications()) —
#'   factored out here so the several new group-wise CI outputs
#'   added by that issue share one implementation.
ci_by_group <- function(df, group_cols, value_col, clamp_lower_zero = TRUE) {
  out <- df %>%
    { if (length(group_cols) > 0) group_by(., across(all_of(group_cols))) else . } %>%
    summarise(n = n(), mean = mean(.data[[value_col]]), sd = sd(.data[[value_col]]), .groups = "drop") %>%
    mutate(
      sd       = ifelse(n < 2 | is.na(sd), 0, sd),
      ci_lower = mean - qt(0.975, df = pmax(n - 1, 1)) * sd / sqrt(n),
      ci_upper = mean + qt(0.975, df = pmax(n - 1, 1)) * sd / sqrt(n)
    )
  if (clamp_lower_zero) out$ci_lower <- pmax(out$ci_lower, 0)
  out
}

#' Plot casualties per day by type, population source and priority
#'
#' @param casualty_priority_summary Casualty counts per arrival day and priority group.
#' @param casualty_summary Casualty counts per arrival day, type and population source.
#' @param max_daily_total Largest single-day casualty total, used as a shared y limit.
#' @param images_dir Directory the plots are written to.
#' @return The p_casualty_summary object.
#' @details The three panels share a y scale, so a reader compares them directly; the
#'   shared maximum is computed by the caller and passed in.
plot_casualty_overview <- function(casualty_priority_summary, casualty_summary, max_daily_total,
                                   images_dir) {
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
  p_casualty_summary
}

#' Write the day-by-day casualty count tables
#'
#' @param casualty_priority_summary Casualty counts per arrival day and priority group.
#' @param casualty_summary Casualty counts per arrival day, type and population source.
#' @param output_dir Directory the tables are written to.
#' @return Invisibly NULL; called for the files it writes.
#' @details Each table is written wide, one column per arrival day, with a total row
#'   and a total column, which is the form the report consumes.
write_casualty_summary_tables <- function(casualty_priority_summary, casualty_summary, output_dir) {
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
  invisible(NULL)
}

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

    # Verify: battle_fatigue must not appear in R2B hold
    bf_in_hold <- attributes_wide %>%
      filter(!is.na(r2b_hold_start) & !is.na(dnbi_type) & dnbi_type == 1L)
    stopifnot(
      "Battle fatigue (dnbi_type==1) must not occupy R2B hold beds" = nrow(bf_in_hold) == 0
    )

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
        stream    = c("Disease DNBI", "NBI", "WIA"),
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

#' Plot each casualty's waiting time against its arrival time
#'
#' @param arrivals Arrival monitor rows with a waiting_time column.
#' @param images_dir Directory the plots are written to.
#' @return The p_waiting_time object.
plot_waiting_times <- function(arrivals, images_dir) {
  p_waiting_time <- ggplot(arrivals, aes(x = start_time / (60 * 24), y = waiting_time)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    labs(title = "Casualty Waiting Time Over Simulation", x = "Simulation Day", y = "Waiting Time (min)") +
    geom_smooth(method = "loess", se = FALSE, color = "darkred") +
    theme_minimal(base_size = 14)
  ggsave(file.path(images_dir, "waiting_time.png"), p_waiting_time,
         width = 12, height = 6, dpi = 150)
  p_waiting_time
}

#' Summarise transport utilisation and plot the capacity margin
#'
#' @param resources Resource monitor rows, warm-up excluded.
#' @param resources_raw Resource monitor rows as read.
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `p_transport_capacity_margin`, `transport_utilisation`.
summarise_transport_capacity_margin <- function(resources, resources_raw, output_dir, images_dir) {
  transport_queue_data <- resources %>%
    as.data.frame() %>%
    filter(grepl("^t_(PMVAmb|HX240M)_\\d+$", resource)) %>%
    dplyr::select(time, resource, queue) %>%
    mutate(
      platform   = str_extract(resource, "(?<=^t_)[^_]+"),
      unit_id    = str_extract(resource, "\\d+$") %>% as.integer(),
      unit_label = paste(platform, unit_id)
    )

  p_transport_capacity_margin <- ggplot(transport_queue_data, aes(x = time / DAY_MIN, y = queue, color = unit_label)) +
    geom_step(linewidth = 1) +
    labs(title = "Transport Fleet Capacity Margin — Queue Over Time",
         subtitle = "Queue length by vehicle; sustained queue > 0 indicates the fleet is at capacity",
         x = "Time (Days)", y = "Queue Size", color = "Vehicle") +
    scale_x_continuous(breaks = seq(0, ceiling(max(transport_queue_data$time) / DAY_MIN), by = 1), expand = c(0, 0)) +
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
  list(
    p_transport_capacity_margin = p_transport_capacity_margin,
    transport_utilisation = transport_utilisation
  )
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

#' Summarise effective force size over the run
#'
#' @param attributes_raw Attribute monitor rows as read.
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `force_regeneration_daily`, `force_regeneration_plot`.
summarise_force_regeneration <- function(attributes_raw, output_dir, images_dir) {
  force_regeneration_plot  <- NULL
  force_regeneration_daily <- NULL

  force_keys <- c("effective_force_combat", "effective_force_support")
  if (any(force_keys %in% attributes_raw$key)) {
    force_regeneration_daily <- attributes_raw %>%
      filter(key %in% force_keys) %>%
      transmute(
        replication,
        day   = time / DAY_MIN,
        pool  = recode(key,
                        effective_force_combat  = "Combat",
                        effective_force_support = "Support"),
        value = as.numeric(value)
      ) %>%
      arrange(replication, pool, day)

    initial_force <- data.frame(
      pool  = c("Combat", "Support"),
      value = c(env_data$pops$combat, env_data$pops$support)
    )

    force_regeneration_plot <- ggplot(force_regeneration_daily, aes(x = day, y = value, color = pool)) +
      geom_step() +
      geom_hline(data = initial_force, aes(yintercept = value, color = pool),
                 linetype = "dashed", alpha = 0.5) +
      scale_color_manual(values = c("Combat" = "#2a78d6", "Support" = "#D62828")) +
      labs(
        title    = "Effective Force Size Over Time",
        subtitle = "Solid = live effective force size (debited at injury, credited at RTD/reinforcement); dashed = initial establishment strength",
        x = "Simulation Day", y = "Effective Force Size", color = "Pool"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")

    if (n_distinct(force_regeneration_daily$replication) > 1) {
      force_regeneration_plot <- force_regeneration_plot + facet_wrap(~ replication, ncol = 1)
    }

    ggsave(file.path(images_dir, "force_regeneration.png"), force_regeneration_plot,
           width = 12, height = 6, dpi = 150)

    write.csv(force_regeneration_daily, file.path(output_dir, "force_regeneration.csv"), row.names = FALSE)
  }
  list(
    force_regeneration_daily = force_regeneration_daily,
    force_regeneration_plot = force_regeneration_plot
  )
}

#' Reconstruct one row per injected surge event from its tagged casualties
#'
#' @param tagged Casualty rows carrying `replication`, `start_time`,
#'   `injury_type` and `casualty_surge_event_id` (the event each casualty
#'   was injected by), restricted to event-origin casualties.
#' @return Data frame of `replication`, `event_id`, `event_start`, `event_end`,
#'   `n_cas`, `n_kia`, `n_wia` and `event_day`, one row per generated event.
#'
#' @details Events are grouped by the id the generator assigned rather than
#'   clustered on arrival gaps. A gap rule merges two events whose starts fall
#'   within one injection window of each other into a single apparent event
#'   larger than the configured `max_cas`.
reconstruct_surge_events <- function(tagged) {
  tagged %>%
    group_by(replication, event_id = casualty_surge_event_id) %>%
    summarise(
      event_start = min(start_time),
      event_end   = max(start_time),
      n_cas       = n(),
      n_kia       = sum(!is.na(injury_type) & injury_type == 3),
      n_wia       = n_cas - n_kia,
      .groups     = "drop"
    ) %>%
    mutate(event_day = floor(event_start / DAY_MIN) + 1)
}

#' Summarise the casualty surge events the run injected
#'
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `casualty_surge_dow_summary`, `casualty_surge_event_count`,
#'   `casualty_surge_events_summary`, `casualty_surge_timeline_plot`.
summarise_casualty_surge_events <- function(combined, output_dir, images_dir) {
  casualty_surge_tagged <- combined %>%
    filter(!is.na(casualty_surge_event) & casualty_surge_event == 1)

  if (nrow(casualty_surge_tagged) > 0) {
    casualty_surge_events_summary <- reconstruct_surge_events(casualty_surge_tagged)
  } else {
    casualty_surge_events_summary <- data.frame(
      replication = integer(0), event_id = integer(0), event_start = numeric(0),
      event_end = numeric(0), n_cas = integer(0), n_kia = integer(0),
      n_wia = integer(0), event_day = numeric(0)
    )
  }

  casualty_surge_event_count <- nrow(casualty_surge_events_summary)

  # DOW rate comparison: casualties originating from a casualty surge vs.
  # background-generated casualties, as a proxy for elevated mortality under
  # casualty surge conditions (see README Limitations for the
  # window-vs-origin comparison design choice).
  casualty_surge_dow_summary <- combined %>%
    filter(!is.na(casualty_surge_event)) %>%
    mutate(origin = if_else(casualty_surge_event == 1, "Casualty Surge Event", "Background")) %>%
    group_by(origin) %>%
    summarise(
      total    = n(),
      dow      = sum(dow == 1, na.rm = TRUE),
      dow_rate = dow / total,
      .groups  = "drop"
    )

  casualty_surge_timeline_plot <- NULL
  if (casualty_surge_event_count > 0) {
    n_sim_days_casualty_surge <- ceiling(max(combined$start_time, na.rm = TRUE) / DAY_MIN)

    casualty_surge_timeline_plot <- plot_casualty_surge_timeline(casualty_surge_events_summary,
                                                                 n_sim_days_casualty_surge)

    ggsave(file.path(images_dir, "casualty_surge_events.png"), casualty_surge_timeline_plot,
           width = 12, height = 6, dpi = 150)
  }

  write.csv(casualty_surge_events_summary,
            file.path(output_dir, "casualty_surge_events_summary.csv"),
            row.names = FALSE)
  write.csv(casualty_surge_dow_summary,    file.path(output_dir, "casualty_surge_dow_summary.csv"),
            row.names = FALSE)
  list(
    casualty_surge_dow_summary = casualty_surge_dow_summary,
    casualty_surge_event_count = casualty_surge_event_count,
    casualty_surge_events_summary = casualty_surge_events_summary,
    casualty_surge_timeline_plot = casualty_surge_timeline_plot
  )
}

#' Summarise realised strategic aeromedical evacuation performance
#'
#' @param attributes Attribute monitor rows, one per attribute write.
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `ame_backlog_data`, `ame_backlog_plot`, `ame_wait_time_summary`,
#'   `n_sim_days_role4`.
summarise_ame_performance <- function(attributes, combined, output_dir, images_dir) {
  ame_wait_time_summary <- NULL
  ame_backlog_plot       <- NULL

  route_labels <- c("1" = "Critical (ICU, CCATT/CCAST)", "2" = "Standard (Hold, CSU)")

  ame_evac_decisions <- combined %>%
    filter(!is.na(r2e_evac) & r2e_evac == 1) %>%
    mutate(route = route_labels[as.character(ame_route)])

  if (nrow(ame_evac_decisions) > 0) {
    #' Summarise strategic evacuation wait times for one route
    #'
    #' @param df Evacuation decision rows, one per casualty.
    #' @return A one-row data frame of the evacuated and awaiting counts and
    #'   the mean, p10 and p90 wait in minutes.
    #' @details A casualty still queued at the end of the run carries no wait
    #'   time, so the quantiles are taken over the evacuated alone and the
    #'   remainder are reported as awaiting rather than as a zero wait.
    wait_stats <- function(df) {
      completed <- df %>% filter(!is.na(ame_wait_minutes))
      data.frame(
        n_evacuated       = nrow(completed),
        n_awaiting        = nrow(df) - nrow(completed),
        mean_wait_minutes = if (nrow(completed) > 0) mean(completed$ame_wait_minutes) else NA_real_,
        p10_wait_minutes  = if (nrow(completed) > 0) quantile(completed$ame_wait_minutes, 0.10) else NA_real_,
        p90_wait_minutes  = if (nrow(completed) > 0) quantile(completed$ame_wait_minutes, 0.90) else NA_real_
      )
    }

    ame_wait_time_summary <- bind_rows(
      data.frame(route = "Overall", wait_stats(ame_evac_decisions)),
      lapply(setNames(route_labels, route_labels), function(r) {
        wait_stats(ame_evac_decisions %>% filter(route == r))
      }) %>%
        bind_rows(.id = "route")
    )
    write.csv(ame_wait_time_summary, file.path(output_dir, "ame_wait_time_summary.csv"), row.names = FALSE)

    overall <- ame_wait_time_summary %>% filter(route == "Overall")
    cat(sprintf(
      "AME actual performance (Issue #23 follow-up): %d evacuated (mean wait %.1f days, p10-p90 %.1f-%.1f days), %d still awaiting AME at end of run\n",
      overall$n_evacuated, overall$mean_wait_minutes / DAY_MIN,
      overall$p10_wait_minutes / DAY_MIN, overall$p90_wait_minutes / DAY_MIN,
      overall$n_awaiting
    ))
  }

  # n_sim_days_role4 is recomputed here (it may not be in scope if the
  # role4-census block further above — which only runs when a casualty has
  # actually reached Role 4 — didn't fire) so a run with AME activity but no
  # completed Role 4 arrivals still gets a bounded backlog/sortie timeline.
  n_sim_days_role4 <- ceiling(max(combined$start_time, na.rm = TRUE) / DAY_MIN)

  ame_backlog_data <- compute_ame_backlog(attributes, n_sim_days_role4)
  ame_backlog_plot <- plot_ame_queue(ame_backlog_data)
  if (!is.null(ame_backlog_plot)) {
    write.csv(ame_backlog_data, file.path(output_dir, "ame_backlog_data.csv"), row.names = FALSE)
    ggsave(file.path(images_dir, "ame_backlog.png"), ame_backlog_plot, width = 12, height = 8, dpi = 150)
  }
  list(
    ame_backlog_data = ame_backlog_data,
    ame_backlog_plot = ame_backlog_plot,
    ame_wait_time_summary = ame_wait_time_summary,
    n_sim_days_role4 = n_sim_days_role4
  )
}

#' Plot the strategic aeromedical evacuation sortie timeline
#'
#' @param resources Resource monitor rows, warm-up excluded.
#' @param n_sim_days_role4 See analyse_run().
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `ame_sortie_data`, `ame_sortie_plot`.
plot_ame_sortie_timeline <- function(resources, n_sim_days_role4, output_dir, images_dir) {
  ame_sortie_data <- NULL
  ame_sortie_plot <- NULL
  if (!is.null(env_data$vars$role4)) {
    ame_sortie_data <- compute_ame_sorties(resources, env_data$vars$role4, n_sim_days_role4)
    ame_sortie_plot <- plot_ame_sortie(ame_sortie_data,
                                       resolve_ame_airframe(env_data$vars$role4)$label)
    if (!is.null(ame_sortie_plot)) {
      write.csv(ame_sortie_data, file.path(output_dir, "ame_sortie_data.csv"), row.names = FALSE)
      ggsave(file.path(images_dir, "ame_sortie_timeline.png"), ame_sortie_plot, width = 12, height = 8, dpi = 150)
    }
  }
  list(
    ame_sortie_data = ame_sortie_data,
    ame_sortie_plot = ame_sortie_plot
  )
}

#' Prepare the data frames every later stage of a single-run analysis reads
#'
#' @param mon See analyse_run().
#' @param warm_up_days See analyse_run().
#' @param output_dir See analyse_run().
#' @return A list of `arrivals`, `arrivals_raw`, `attributes`, `attributes_raw`, `attributes_wide`,
#'   `casualty_priority_summary`, `casualty_summary`, `combined`, `max_daily_total`, `resources`,
#'   `resources_raw`.
#' @details Writes the three monitor CSVs unchanged, then derives the per-casualty
#'   wide view and the daily casualty counts the plotting and summary stages
#'   share. Warm-up is applied to arrivals and resources by time; attributes
#'   carry no time filter, being read per casualty.
prepare_run_frames <- function(mon, warm_up_days, output_dir) {
  warm_up_min    <- as.integer(warm_up_days) * DAY_MIN
  arrivals_raw   <- mon$arrivals    %>% filter(start_time >= warm_up_min)
  attributes_raw <- mon$attributes
  resources_raw  <- mon$resources   %>% filter(time >= warm_up_min)

  write.csv(arrivals_raw,   file.path(output_dir, "mon_arrivals.csv"),   row.names = FALSE)
  write.csv(attributes_raw, file.path(output_dir, "mon_attributes.csv"), row.names = FALSE)
  write.csv(resources_raw,  file.path(output_dir, "mon_resources.csv"),  row.names = FALSE)

  arrivals <- arrivals_raw %>%
    mutate(waiting_time = end_time - start_time - activity_time)

  # Pivot attributes wide (last value per key per casualty); see roxygen on
  # build_attributes_wide() for the guarantee that every attribute key the
  # model can set is present, all-NA where no casualty set it.
  attributes_wide <- build_attributes_wide(attributes_raw, arrivals)

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
  list(
    arrivals = arrivals,
    arrivals_raw = arrivals_raw,
    attributes = attributes_raw,
    attributes_raw = attributes_raw,
    attributes_wide = attributes_wide,
    casualty_priority_summary = casualty_priority_summary,
    casualty_summary = casualty_summary,
    combined = combined,
    max_daily_total = max_daily_total,
    resources = resources_raw,
    resources_raw = resources_raw
  )
}

#' Compute the four treatment-interval indicators, in minutes
#'
#' @param combined See analyse_run().
#' @param window_min End of the observation window, in minutes.
#' @return A list of `time_to_first_surgery`, `r2b_dwell_time`, `r2b_r2e_transit_time`,
#'   `r2e_dwell_time`, each carrying `n_censored` and `censored_share` alongside
#'   its mean.
#' @details Covers time from R1 arrival to first surgical incision, dwell at R2B and at
#'   R2E, and the transit between them. Killed-in-action cases, and
#'   died-of-wounds cases whose death preceded any surgery, are excluded.
#'
#'   The three intervals whose cohort at risk is known, the two dwells and the
#'   transit between them, are summarised by censored_interval_stats(), so a
#'   casualty still in the echelon when the run ends contributes the lower
#'   bound they represent instead of being dropped. Time to first surgery is
#'   not, because its cohort is not identifiable: a casualty with no surgery
#'   time may be waiting for an operation or may never have needed one, and
#'   the attributes do not distinguish them. It reports the completed-case mean
#'   with the count of casualties carrying no surgery time, which is an upper
#'   bound on how many were censored rather than the censored count itself.
compute_treatment_interval_kpis <- function(combined, window_min) {
  # KPI 1: Time from R1 arrival to first surgical incision (minutes)
  # Excludes KIA cases and DOW cases where death preceded any surgery.
  time_to_first_surgery_cohort <- combined %>%
    mutate(
      first_surgery_start = pmin(
        as.numeric(r2b_surgery_start),
        as.numeric(r2e_surgery_1_start),
        na.rm = TRUE
      ),
      time_to_surgery_min = first_surgery_start - start_time
    ) %>%
    filter(casualty_type != "kia")
  # Counted before the surgery-time filter below, which drops exactly the rows
  # this is counting: `dow` is NA for every survivor, so `dow == 1 & is.na(...)`
  # is NA for a casualty with no surgery time and filter() removes them there.
  no_surgery_time <- sum(is.na(time_to_first_surgery_cohort$time_to_surgery_min))
  time_to_first_surgery <- time_to_first_surgery_cohort %>%
    filter(!(dow == 1 & is.na(first_surgery_start))) %>%
    filter(!is.na(time_to_surgery_min)) %>%
    summarise(
      mean_min = mean(time_to_surgery_min),
      p10_min  = quantile(time_to_surgery_min, 0.10),
      p90_min  = quantile(time_to_surgery_min, 0.90),
      n        = n()
    ) %>%
    mutate(n_no_surgery_time = no_surgery_time)

  # KPI 2: R2B dwell time (minutes) — treatment start to departure
  r2b_dwell_time <- censored_interval_stats(
    combined$r2b_treatment_start_time, combined$r2b_departure_time, window_min
  )

  # KPI 3: R2B → R2E transit time (minutes)
  r2b_r2e_transit_time <- censored_interval_stats(
    combined$r2b_departure_time, combined$r2e_arrival_time, window_min
  )

  # KPI 4: R2E dwell time (minutes)
  r2e_dwell_time <- censored_interval_stats(
    combined$r2e_arrival_time, combined$r2e_departure_time, window_min
  )
  list(
    time_to_first_surgery = time_to_first_surgery,
    r2b_dwell_time = r2b_dwell_time,
    r2b_r2e_transit_time = r2b_r2e_transit_time,
    r2e_dwell_time = r2e_dwell_time
  )
}

#' Summarise died-of-wounds, return-to-duty and in-theatre retention by echelon
#'
#' @param arrivals_raw See analyse_run().
#' @param attributes_wide See analyse_run().
#' @return A list of `dow_by_echelon`, `total_dow`, `bf_rtd`, `clinical_rtd`, `total_rtd`,
#'   `rtd_by_echelon`, `evacuation_policy_summary`, `in_theatre_share`.
#' @details The realised in-theatre share is an output of the evacuation policy rather
#'   than an input to it, so it is reported alongside the decisions it derives
#'   from rather than on its own.
summarise_outcomes_by_echelon <- function(arrivals_raw, attributes_wide) {
  # KPI 5: DOW count and rate by echelon
  # dow_echelon encoding: 1 = R1, 2 = R2B, 3 = R2E (arrival), 4 = R2E
  # (post-operative), 5 = awaiting strategic AME
  echelon_labels <- c("1" = "r1", "2" = "r2b", "3" = "r2e", "4" = "r2e_postop", "5" = "ame_wait")
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

  # KPI 6b: realised in-theatre share at the R2E evacuation-policy decision.
  # The share is an output of the policy, not an input to it:
  # every casualty reaching R2E disposition draws recovery_to_duty_days and
  # is retained when that falls within recovery$evacuation_policy_days. It is
  # measured at the decision, not on completed returns to duty, because a
  # retained casualty's convalescence routinely outlasts the run. Compared
  # against the 7.6%-42.1% historical range in README Return to Duty.
  # The attribute is absent entirely when no casualty reached R2E disposition
  # within the observation window, which a short or low-intensity run can
  # produce, so the column is guarded rather than assumed.
  policy_decisions <- if ("recovery_to_duty_days" %in% names(attributes_wide)) {
    attributes_wide %>% filter(!is.na(recovery_to_duty_days))
  } else {
    attributes_wide[0, , drop = FALSE]
  }
  in_theatre_share <- if (nrow(policy_decisions) == 0) NA_real_ else {
    mean(policy_decisions$recovery_to_duty_days <=
           env_data$vars$r2eheavy$recovery$evacuation_policy_days)
  }
  evacuation_policy_summary <- list(
    policy_days      = env_data$vars$r2eheavy$recovery$evacuation_policy_days,
    decisions        = nrow(policy_decisions),
    in_theatre_share = in_theatre_share
  )
  list(
    dow_by_echelon = dow_by_echelon,
    total_dow = total_dow,
    bf_rtd = bf_rtd,
    clinical_rtd = clinical_rtd,
    total_rtd = total_rtd,
    rtd_by_echelon = rtd_by_echelon,
    evacuation_policy_summary = evacuation_policy_summary,
    in_theatre_share = in_theatre_share
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
        "Surgical pathway split (Issue #173): %d operated, %d damage control (%.1f%%), %d single-stage\n",
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
      "R2E evacuation policy (Issue #156): %d-day policy, %d dispositions, realised in-theatre share %.1f%%\n",
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

#' Summarise and plot daily Role 4 bed occupancy by ward
#'
#' @param combined See analyse_run().
#' @param role4_daily_by_rep See analyse_run().
#' @param n_reps_role4 See analyse_run().
#' @param n_sim_days_role4 See analyse_run().
#' @param role4_census_daily See analyse_run().
#' @param role4_summary See analyse_run().
#' @param role4_params See analyse_run().
#' @param output_dir See analyse_run().
#' @param images_dir See analyse_run().
#' @return A list of `role4_census_daily`, `role4_census_plot`, `role4_summary`, `n_reps_role4`,
#'   `n_sim_days_role4`.
#' @details The occupancy is an unconstrained demand signal rather than a modelled
#'   Role 4: it counts the casualties strategic evacuation delivered, against no
#'   capacity limit at the receiving end.
plot_role4_census <- function(combined, role4_daily_by_rep, n_reps_role4, n_sim_days_role4,
                              role4_census_daily, role4_summary, role4_params,
                              output_dir, images_dir) {
  n_reps_role4      <- n_distinct(role4_daily_by_rep$replication)
  n_sim_days_role4  <- ceiling(max(combined$start_time, na.rm = TRUE) / DAY_MIN)
  max_discharge_day <- max(role4_daily_by_rep$day, na.rm = TRUE)
  ward_levels       <- role4_ward_levels(role4_params)
  # evacuation_day is only set on completed AME boarding; r2e_evac == 1 alone would also count
  # casualties still
  # queued awaiting a sortie at end of run, which have not reached Role 4.
  total_evacuated   <- sum(!is.na(combined$evacuation_day)) / n_reps_role4

  role4_census_daily <- role4_daily_by_rep %>%
    group_by(day, ward) %>%
    # sum()/n_reps_role4 rather than mean(occupancy): a replication with
    # zero occupants for this (day, ward) has no row at all in
    # role4_daily_by_rep, so mean() would silently divide by only the
    # replications that had a nonzero count, biasing the estimate upward.
    summarise(mean_occupancy = sum(occupancy) / n_reps_role4, .groups = "drop") %>%
    complete(
      day  = seq_len(max_discharge_day),
      ward = ward_levels,
      fill = list(mean_occupancy = 0)
    ) %>%
    mutate(ward = factor(ward, levels = ward_levels)) %>%
    arrange(day, ward)

  peak_total_daily <- role4_census_daily %>%
    group_by(day) %>%
    summarise(total_occupancy = sum(mean_occupancy), .groups = "drop")

  role4_summary <- data.frame(
    total_evacuated = total_evacuated,
    peak_occupancy  = max(peak_total_daily$total_occupancy),
    peak_day        = peak_total_daily$day[which.max(peak_total_daily$total_occupancy)]
  )

  role4_census_plot <- ggplot(role4_census_daily, aes(x = day, y = mean_occupancy, fill = ward)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_vline(xintercept = n_sim_days_role4 + 0.5, linetype = "dotted", color = "gray40") +
    scale_fill_brewer(palette = "Set2") +
    scale_x_continuous(breaks = seq(1, max_discharge_day, by = 2), expand = c(0, 0)) +
    labs(
      title    = "Role 4 (National Support Base) Daily Bed Occupancy by Bed Type",
      subtitle = sprintf(
        "Unconstrained demand signal from %.0f strategically evacuated casualties; dotted line = end of %d-day engagement window",
        role4_summary$total_evacuated, n_sim_days_role4
      ),
      x = "Simulation Day", y = "Mean Concurrent Patients", fill = "Bed type"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")

  ggsave(file.path(images_dir, "role4_census.png"), role4_census_plot,
         width = 12, height = 6, dpi = 150)
  write.csv(role4_census_daily, file.path(output_dir, "role4_census_daily.csv"), row.names = FALSE)
  list(
    role4_census_daily = role4_census_daily,
    role4_census_plot = role4_census_plot,
    role4_summary = role4_summary,
    n_reps_role4 = n_reps_role4,
    n_sim_days_role4 = n_sim_days_role4
  )
}

#' Summarise the strategic aeromedical sortie demand the census implies
#'
#' @param ame_capacity See analyse_run().
#' @param role4_daily_by_rep See analyse_run().
#' @param ame_by_rep See analyse_run().
#' @param n_reps_role4 See analyse_run().
#' @param n_sim_days_role4 See analyse_run().
#' @param role4_summary See analyse_run().
#' @param ame_demand_daily See analyse_run().
#' @param role4_replication_summary Across-replication Role 4 summary. It is computed
#'   only where more than one replication contributes, so it is taken in at the
#'   caller's NULL and returned unchanged at a single replication.
#' @param ame_replication_summary Across-replication sortie summary, on the same terms.
#' @param output_dir See analyse_run().
#' @return A list of `ame_demand_daily`, `ame_summary`, `role4_replication_summary`,
#'   `ame_replication_summary`.
#' @details Sorties required per day is the daily evacuation count over the configured
#'   airframe's combined capacity, so it is a best-case, same-day, uncapped
#'   figure rather than a schedule.
summarise_ame_sortie_demand <- function(ame_capacity, role4_daily_by_rep, ame_by_rep, n_reps_role4,
                                        n_sim_days_role4, role4_summary, ame_demand_daily,
                                        role4_replication_summary, ame_replication_summary,
                                        output_dir) {
  ame_demand_daily <- ame_by_rep %>%
    group_by(day) %>%
    summarise(evacuation_count = sum(evacuation_count) / n_reps_role4, .groups = "drop") %>%
    complete(day = seq_len(n_sim_days_role4), fill = list(evacuation_count = 0)) %>%
    arrange(day) %>%
    mutate(
      sorties_required   = ceiling(evacuation_count / ame_capacity),
      cumulative_sorties = cumsum(sorties_required)
    )

  ame_summary <- data.frame(
    total_sorties      = sum(ame_demand_daily$sorties_required),
    peak_daily_sorties = max(ame_demand_daily$sorties_required),
    mean_daily_sorties = mean(ame_demand_daily$sorties_required)
  )

  write.csv(ame_demand_daily, file.path(output_dir, "ame_demand_daily.csv"), row.names = FALSE)

  cat(sprintf(
    "Role 4 demand (Issue #23): %.0f reached Role 4 via AME, peak occupancy %.1f (day %d); unconstrained-baseline demand would need %d total AME sorties (capacity %d/sortie)\n",
    role4_summary$total_evacuated, role4_summary$peak_occupancy, role4_summary$peak_day,
    ame_summary$total_sorties, ame_capacity
  ))

  if (n_reps_role4 > 1) {
    role4_peak_by_rep <- role4_daily_by_rep %>%
      group_by(replication, day) %>%
      summarise(total_occupancy = sum(occupancy), .groups = "drop") %>%
      group_by(replication) %>%
      summarise(peak_occupancy = max(total_occupancy), .groups = "drop")

    role4_replication_summary <- role4_peak_by_rep %>%
      summarise(
        n_reps   = n(),
        mean_peak_occupancy = mean(peak_occupancy),
        sd_peak_occupancy   = sd(peak_occupancy),
        ci_lower = mean_peak_occupancy - qt(0.975, df = n() - 1) * sd_peak_occupancy / sqrt(n()),
        ci_upper = mean_peak_occupancy + qt(0.975, df = n() - 1) * sd_peak_occupancy / sqrt(n())
      )

    ame_total_by_rep <- ame_by_rep %>%
      group_by(replication) %>%
      summarise(total_sorties = sum(sorties_required), .groups = "drop")

    ame_replication_summary <- ame_total_by_rep %>%
      summarise(
        n_reps  = n(),
        mean_total_sorties = mean(total_sorties),
        sd_total_sorties   = sd(total_sorties),
        ci_lower = mean_total_sorties - qt(0.975, df = n() - 1) * sd_total_sorties / sqrt(n()),
        ci_upper = mean_total_sorties + qt(0.975, df = n() - 1) * sd_total_sorties / sqrt(n())
      )

    write.csv(role4_replication_summary, file.path(output_dir, "role4_replication_summary.csv"), row.names = FALSE)
    write.csv(ame_replication_summary,   file.path(output_dir, "ame_replication_summary.csv"),   row.names = FALSE)
  }
  list(
    ame_demand_daily = ame_demand_daily,
    ame_summary = ame_summary,
    role4_replication_summary = role4_replication_summary,
    ame_replication_summary = ame_replication_summary
  )
}

#' Runs the full analysis and visualisation pipeline on monitoring data
#'
#' @param mon Named list with elements arrivals, attributes, resources as
#'   extracted from a run with get_mon_arrivals(), get_mon_attributes() and
#'   get_mon_resources()
#' @param output_dir Directory path for saving CSV and plot outputs
#'   (default: "outputs")
#' @param warm_up_days Days to exclude from the start of the analysis window
#'   (applied to arrivals by start_time and resources by time; default 0)
#' @param images_dir Directory path for saving PNG plots. Defaults to
#'   `file.path(output_dir, "images")`, which is gitignored, so calling this
#'   function cannot overwrite the tracked seed-42 baseline plots in `images/`
#'   unless the caller names that directory explicitly. run.R
#'   does so only under `--refresh-baseline`.
#' @return Invisibly returns a named list. Nine elements are always-present
#'   ggplot objects (embeddable in a Shiny reactive context via
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
                        images_dir = file.path(output_dir, "images")) {
  validate_monitoring(mon, "analyse_run")
  usable_warm_up <- is.numeric(warm_up_days) && length(warm_up_days) == 1L &&
    !is.na(warm_up_days) && warm_up_days >= 0
  if (!usable_warm_up) {
    stop(sprintf("analyse_run: warm_up_days must be a single non-negative number, found %s",
                 paste(format(warm_up_days), collapse = ", ")), call. = FALSE)
  }

  dir.create(output_dir,  showWarnings = FALSE, recursive = TRUE)
  dir.create(images_dir,  showWarnings = FALSE, recursive = TRUE)

  run_frames_out <- prepare_run_frames(mon, warm_up_days, output_dir)
  arrivals <- run_frames_out$arrivals
  arrivals_raw <- run_frames_out$arrivals_raw
  attributes <- run_frames_out$attributes
  attributes_raw <- run_frames_out$attributes_raw
  attributes_wide <- run_frames_out$attributes_wide
  casualty_priority_summary <- run_frames_out$casualty_priority_summary
  casualty_summary <- run_frames_out$casualty_summary
  combined <- run_frames_out$combined
  max_daily_total <- run_frames_out$max_daily_total
  resources <- run_frames_out$resources
  resources_raw <- run_frames_out$resources_raw

  # ── Casualty overview plots ───────────────────────────────────────────────


  # Casualty overview plots
  p_casualty_summary <- plot_casualty_overview(casualty_priority_summary, casualty_summary,
                                               max_daily_total, images_dir)

  # ── Summary tables ────────────────────────────────────────────────────────


  # Summary tables
  write_casualty_summary_tables(casualty_priority_summary, casualty_summary, output_dir)

  # ── R1 resource queue graphs ──────────────────────────────────────────────


  # R1 resource queue graphs
  p_r1_queues <- plot_r1_queues(resources, images_dir)

  # ── R2B bed queue graphs ──────────────────────────────────────────────────


  # R2B bed queue graphs
  p_r2b_bed_queues <- plot_r2b_bed_queues(resources, images_dir)

  # ── R2B hold bed occupancy by patient stream ─────────────────────────────
  # Requires the r2b_hold_start attribute the R2B holding trajectory sets.
  # Decomposes daily concurrent hold bed occupancy into disease DNBI, NBI, and
  # WIA streams. Verifies that battle_fatigue (dnbi_type == 1) never reaches
  # R2B hold beds.


  # R2B hold bed occupancy by patient stream
  r2b_hold_occupancy_out <- summarise_r2b_hold_occupancy(attributes_wide, combined, output_dir,
                                                         images_dir)
  r2b_hold_daily <- r2b_hold_occupancy_out$r2b_hold_daily
  r2b_hold_occupancy_plot <- r2b_hold_occupancy_out$r2b_hold_occupancy_plot

  # ── R2B hold routing diagnostics ─────────────────────────────────────────
  # r2b_bypassed     = 1: routed to R2E at R1 (hold_threshold exceeded before transport)
  # r2b_hold_bypass  = 1: arrived at R2B but hold full at Step 4 → R2E
  # r2b_hold_queued  = 1: both echelons full, queue within cap → queued at R2B hold

  # R2B hold routing diagnostics
  r2b_hold_routing_out <- count_r2b_hold_routing(attributes_wide)
  r2b_hold_bypass_count <- r2b_hold_routing_out$r2b_hold_bypass_count
  r2b_hold_queued_count <- r2b_hold_routing_out$r2b_hold_queued_count
  r2b_pre_bypass_count <- r2b_hold_routing_out$r2b_pre_bypass_count

  # ── R2B OT bypass reason decomposition ───────────────────────────────────
  # r2b_bypass_reason is only set for casualties who reached R2B (r2b_treated
  # non-NA) and were bypassed to R2E at the surgical decision point — it does
  # not apply to the pre-transport r2b_bypassed rows above (those never carry
  # r2b_treated, since they are routed from R1 before ever arriving at R2B).
  # 1 = surgical team off-shift; 2 = OT bed busy or queued.

  # R2B OT bypass reason decomposition
  r2b_ot_bypass_reasons_out <- count_r2b_ot_bypass_reasons(attributes_wide)
  r2b_ot_bypass_busy_count <- r2b_ot_bypass_reasons_out$r2b_ot_bypass_busy_count
  r2b_ot_bypass_count <- r2b_ot_bypass_reasons_out$r2b_ot_bypass_count
  r2b_ot_bypass_offshift_count <- r2b_ot_bypass_reasons_out$r2b_ot_bypass_offshift_count

  # ── R2B pre-open hold ────────────────────────────────────────────────────
  # The complement of the off-shift bypass count above: casualties who found
  # the surgical section closed but close enough to reopening to be held
  # forward for it rather than diverted. r2b_pre_open_wait_min is set only
  # once theatre and section are both in hand, so a casualty still holding at
  # the end of the run carries the marker without a duration and is counted
  # but not averaged.

  # R2B pre-open hold
  r2b_bypass_reason_plot <- summarise_r2b_pre_open_hold(attributes_wide, combined,
                                                        r2b_ot_bypass_busy_count,
                                                        r2b_ot_bypass_count,
                                                        r2b_ot_bypass_offshift_count, images_dir)

  # ── R2B casualty treatment summary ───────────────────────────────────────


  # R2B casualty treatment summary
  plot_r2b_treated <- plot_r2b_casualty_treatment(combined)

  # ── R2B surgery summary ───────────────────────────────────────────────────


  # R2B surgery summary
  r2b_surgery_out <- summarise_r2b_surgery(attributes_wide)
  plot_r2b_summary <- r2b_surgery_out$plot_r2b_summary
  r2b_summary <- r2b_surgery_out$r2b_summary

  # ── Casualties skipping R2B ───────────────────────────────────────────────


  # Casualties skipping R2B
  p_r2b_handling <- plot_r2b_handling(attributes_wide, combined, plot_r2b_summary, plot_r2b_treated,
                                      images_dir)

  # ── R2B resource usage (Gantt) ────────────────────────────────────────────


  # R2B resource usage (Gantt)
  r2b_bed_gantt_out <- plot_r2b_bed_gantt(resources, images_dir)
  max_days <- r2b_bed_gantt_out$max_days
  p_r2b_gantt <- r2b_bed_gantt_out$p_r2b_gantt

  # ── R2E surgeries ─────────────────────────────────────────────────────────


  # R2E surgeries
  r2e_surgeries_out <- summarise_r2e_surgeries(attributes_wide, max_days, images_dir)
  p_r2e_surgeries <- r2e_surgeries_out$p_r2e_surgeries
  r2e_summary <- r2e_surgeries_out$r2e_summary

  # ── R2E bed queue graphs ──────────────────────────────────────────────────


  # R2E bed queue graphs
  p_r2e_bed_queues <- plot_r2e_bed_queues(resources, images_dir)

  # ── Waiting time scatter ──────────────────────────────────────────────────


  # Waiting time scatter
  p_waiting_time <- plot_waiting_times(arrivals, images_dir)

  # ── Transport fleet capacity margin ───────────────────────────────────────
  # Queue-over-time per pooled transport asset (PMV Ambulance, HX240M) shows
  # how much headroom the current fleet size has under the dead-heading
  # round trip: a queue that stays at 0 throughout indicates spare capacity;
  # sustained queue > 0 indicates the fleet is a binding constraint.


  # Transport fleet capacity margin
  transport_capacity_margin_out <- summarise_transport_capacity_margin(resources, resources_raw,
                                                                       output_dir, images_dir)
  p_transport_capacity_margin <- transport_capacity_margin_out$p_transport_capacity_margin
  transport_utilisation <- transport_capacity_margin_out$transport_utilisation

  # ── R2E resource usage (Gantt) ────────────────────────────────────────────


  # R2E resource usage (Gantt)
  p_r2e_gantt <- plot_r2e_bed_gantt(resources, images_dir)

  # ── Output Variable Register derived KPIs ────────────────────────────────

  treatment_interval_kpis_out <- compute_treatment_interval_kpis(
    combined, observation_window_min(combined)
  )
  time_to_first_surgery <- treatment_interval_kpis_out$time_to_first_surgery
  r2b_dwell_time <- treatment_interval_kpis_out$r2b_dwell_time
  r2b_r2e_transit_time <- treatment_interval_kpis_out$r2b_r2e_transit_time
  r2e_dwell_time <- treatment_interval_kpis_out$r2e_dwell_time

  outcomes_by_echelon_out <- summarise_outcomes_by_echelon(arrivals_raw, attributes_wide)
  dow_by_echelon <- outcomes_by_echelon_out$dow_by_echelon
  total_dow <- outcomes_by_echelon_out$total_dow
  bf_rtd <- outcomes_by_echelon_out$bf_rtd
  clinical_rtd <- outcomes_by_echelon_out$clinical_rtd
  total_rtd <- outcomes_by_echelon_out$total_rtd
  rtd_by_echelon <- outcomes_by_echelon_out$rtd_by_echelon
  evacuation_policy_summary <- outcomes_by_echelon_out$evacuation_policy_summary
  in_theatre_share <- outcomes_by_echelon_out$in_theatre_share

  ot_utilisation_out <- compute_ot_utilisation(resources_raw)
  ot_utilisation <- ot_utilisation_out$ot_utilisation
  obs_window <- ot_utilisation_out$obs_window

  post_operative_pathways_out <- summarise_post_operative_pathways(attributes_wide,
                                                                   evacuation_policy_summary,
                                                                   in_theatre_share, output_dir)
  post_op_pathway_summary <- post_operative_pathways_out$post_op_pathway_summary
  surgery_deferred_count <- post_operative_pathways_out$surgery_deferred_count
  definitive_repair_outstanding_count <-
    post_operative_pathways_out$definitive_repair_outstanding_count
  surgical_pathway_summary <- post_operative_pathways_out$surgical_pathway_summary

  # ── R2E OT-ICU gating impact — sub-optimal and delayed care ──────────────
  # Visualises, by simulation day, where casualties experienced degraded care
  # specifically attributable to ICU saturation at the point of OT entry:
  # - Sub-Optimal Care: a Priority 1 candidate was operated on despite ICU
  #   being full; post-operative recovery occurred in a holding bed instead
  #   of ICU, carrying an elevated dow_ceiling (README — Died of Wounds,
  #   Post-Operative Checkpoint).
  # - Delayed Care: a Priority 2+ candidate had OT entry deferred while ICU
  #   was saturated, polling on a timer until a bed freed.
  # - Normal: ICU was available at the point of OT entry (gate had no effect).

  # R2E OT-ICU gating impact — sub-optimal and delayed care
  r2e_icu_gating_out <- summarise_r2e_icu_gating(attributes_wide, combined, output_dir, images_dir)
  r2e_icu_gating_daily <- r2e_icu_gating_out$r2e_icu_gating_daily
  r2e_icu_gating_plot <- r2e_icu_gating_out$r2e_icu_gating_plot

  # ── Force regeneration — effective force size over time ──────────────
  # Visualises effective_force_combat/effective_force_support (simmer
  # globals set in run_once(), R/replication.R; debited/credited in
  # R/trajectories.R) across the run: debited at each casualty's
  # injury_time, credited at each RTD event, and stepped up by any
  # configured reinforcement schedule (env_data.json
  # force_regeneration.reinforcement). A flat line at the dashed initial
  # establishment strength means casualty production and RTD/reinforcement
  # regeneration are in balance; a declining line shows net depletion.

  # Force regeneration — effective force size over time
  force_regeneration_out <- summarise_force_regeneration(attributes_raw, output_dir, images_dir)
  force_regeneration_daily <- force_regeneration_out$force_regeneration_daily
  force_regeneration_plot <- force_regeneration_out$force_regeneration_plot

  # ── KPI 9: Casualty surge event stress test analysis ─────────────
  # casualty_surge_event: 1 = casualty originated from a compound-Poisson
  # casualty surge injection event (R/environment.R::generate_casualty_surge_events()),
  # 0 = background lognormal generation. Both of the event's pathways carry
  # the tag, the wounded and the immediately killed, so n_cas
  # below is an event's total and n_wia/n_kia its two components.
  # Individual events are reconstructed by grouping tagged casualties on the
  # casualty_surge_event_id the generator assigned (reconstruct_surge_events()),
  # not by clustering arrival gaps, which merged events starting close together.

  # KPI 9: Casualty surge event stress test analysis
  casualty_surge_events_out <- summarise_casualty_surge_events(combined, output_dir, images_dir)
  casualty_surge_dow_summary <- casualty_surge_events_out$casualty_surge_dow_summary
  casualty_surge_event_count <- casualty_surge_events_out$casualty_surge_event_count
  casualty_surge_events_summary <- casualty_surge_events_out$casualty_surge_events_summary
  casualty_surge_timeline_plot <- casualty_surge_events_out$casualty_surge_timeline_plot

  # ── Role 4 (national support base) census and AME sortie demand ──────────────
  # compute_role4_census()/compute_ame_demand() (above) return per-replication
  # granular tables; this block aggregates them for display (mean across
  # replications) and, for multi-run mode, for the peak-occupancy/total-sorties
  # CI (within replications), following the t-distribution CI convention used
  # by summarise_replications() (R/replication.R).
  role4_census_daily        <- NULL
  role4_census_plot         <- NULL
  role4_summary              <- NULL
  role4_replication_summary <- NULL
  ame_demand_daily          <- NULL
  ame_summary                <- NULL
  ame_replication_summary   <- NULL
  role4_surgical_demand     <- NULL

  if (!is.null(env_data$vars$role4)) {
    role4_params <- env_data$vars$role4
    # Best-case per-sortie throughput: the unconstrained baseline
    # (compute_ame_demand()) does not distinguish acuity/route, so it
    # compares against the configured airframe's combined critical +
    # standard capacity — same-day, uncapped, best-case throughput.
    ame_airframe <- resolve_ame_airframe(role4_params)
    ame_capacity <- ame_airframe$critical_capacity + ame_airframe$standard_capacity
    role4_daily_by_rep <- compute_role4_census(combined, role4_params)
    ame_by_rep         <- compute_ame_demand(combined, ame_capacity)
    # The operating theatre requirement the stream places on the national
    # support base, reported beside the bed demand rather than inside it: a
    # theatre is a throughput resource, so its demand is in operations and
    # minutes rather than in bed-days.
    role4_surgical_demand <- compute_role4_surgical_demand(combined, role4_params)
    write.csv(role4_surgical_demand,
              file.path(output_dir, "role4_surgical_demand.csv"), row.names = FALSE)
    cat(sprintf(paste("Role 4 surgical demand: %d operation(s) owed,",
                      "%.0f theatre-minutes, over %d day(s)\n"),
                sum(role4_surgical_demand$operations),
                sum(role4_surgical_demand$theatre_minutes),
                length(unique(role4_surgical_demand$day))))
  }

  if (!is.null(env_data$vars$role4) && nrow(role4_daily_by_rep) > 0) {
    role4_census_out <- plot_role4_census(combined, role4_daily_by_rep, n_reps_role4,
                                          n_sim_days_role4, role4_census_daily, role4_summary,
                                          env_data$vars$role4, output_dir, images_dir)
    role4_census_daily <- role4_census_out$role4_census_daily
    role4_census_plot <- role4_census_out$role4_census_plot
    role4_summary <- role4_census_out$role4_summary
    n_reps_role4 <- role4_census_out$n_reps_role4
    n_sim_days_role4 <- role4_census_out$n_sim_days_role4

    ame_sortie_demand_out <- summarise_ame_sortie_demand(ame_capacity, role4_daily_by_rep,
                                                         ame_by_rep, n_reps_role4, n_sim_days_role4,
                                                         role4_summary, ame_demand_daily,
                                                         role4_replication_summary,
                                                         ame_replication_summary, output_dir)
    ame_demand_daily <- ame_sortie_demand_out$ame_demand_daily
    ame_summary <- ame_sortie_demand_out$ame_summary
    role4_replication_summary <- ame_sortie_demand_out$role4_replication_summary
    ame_replication_summary <- ame_sortie_demand_out$ame_replication_summary
  }

  # ── Strategic AME actual performance ────────────────────────────────────
  # ame_demand_daily/ame_summary above are an unconstrained theoretical
  # baseline (ceiling(daily_evacuation_count / capacity), ignoring the
  # schedule entirely). These outputs instead measure the REAL constrained
  # "ame"/"ame_critical" simmer resources: wait time from evacuation
  # decision (r2e_departure_time) to actual boarding (ame_departure_time),
  # decomposed by route (ame_route: 1 = critical/ICU/CCATT-CCAST,
  # 2 = standard/Hold/Casualty Staging Unit), and the backlog of casualties
  # awaiting a sortie over time on each pool, from the resource monitor's
  # queue column.

  # Strategic AME actual performance
  ame_performance_out <- summarise_ame_performance(attributes, combined, output_dir, images_dir)
  ame_backlog_data <- ame_performance_out$ame_backlog_data
  ame_backlog_plot <- ame_performance_out$ame_backlog_plot
  ame_wait_time_summary <- ame_performance_out$ame_wait_time_summary
  n_sim_days_role4 <- ame_performance_out$n_sim_days_role4

  # ── Strategic AME sortie timeline ────────────────────────────────────────
  # compute_ame_sorties() only needs `resources` and the schedule/airframe
  # parameters, not `combined` — so it does not require any strategic
  # evacuation decisions to have occurred yet, unlike the backlog output
  # above.

  # Strategic AME sortie timeline
  ame_sortie_timeline_out <- plot_ame_sortie_timeline(resources, n_sim_days_role4, output_dir,
                                                      images_dir)
  ame_sortie_data <- ame_sortie_timeline_out$ame_sortie_data
  ame_sortie_plot <- ame_sortie_timeline_out$ame_sortie_plot

  write.csv(dow_by_echelon,  file.path(output_dir, "dow_by_echelon.csv"),  row.names = FALSE)
  write.csv(rtd_by_echelon,  file.path(output_dir, "rtd_by_echelon.csv"),  row.names = FALSE)
  write.csv(ot_utilisation,  file.path(output_dir, "ot_utilisation.csv"),  row.names = FALSE)

  invisible(list(
    # Named ggplot objects — embeddable directly in a Shiny
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
    evacuation_policy_summary   = evacuation_policy_summary,
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
    surgical_pathway_summary    = surgical_pathway_summary,
    surgery_deferred_count      = surgery_deferred_count,
    definitive_repair_outstanding_count = definitive_repair_outstanding_count,
    r2e_icu_gating_daily        = r2e_icu_gating_daily,
    r2e_icu_gating_plot         = r2e_icu_gating_plot,
    casualty_surge_events_summary       = casualty_surge_events_summary,
    casualty_surge_event_count          = casualty_surge_event_count,
    casualty_surge_dow_summary          = casualty_surge_dow_summary,
    casualty_surge_timeline_plot        = casualty_surge_timeline_plot,
    force_regeneration_daily           = force_regeneration_daily,
    force_regeneration_plot            = force_regeneration_plot,
    role4_census_daily          = role4_census_daily,
    role4_surgical_demand       = role4_surgical_demand,
    role4_census_plot           = role4_census_plot,
    role4_summary                = role4_summary,
    role4_replication_summary   = role4_replication_summary,
    ame_demand_daily            = ame_demand_daily,
    ame_summary                  = ame_summary,
    ame_replication_summary     = ame_replication_summary,
    ame_wait_time_summary       = ame_wait_time_summary,
    ame_backlog_data             = ame_backlog_data,
    ame_backlog_plot            = ame_backlog_plot,
    ame_sortie_data              = ame_sortie_data,
    ame_sortie_plot              = ame_sortie_plot
  ))
}
