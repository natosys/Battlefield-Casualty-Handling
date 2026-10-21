##############################################
## R/analysis_run_evacuation.R              ##
## Single run: strategic evacuation         ##
##############################################
#
# The single-run stages reporting strategic evacuation: airlift performance
# and the sortie timeline, the Role 4 census and the sortie demand it implies.
#
# Sourced by R/analysis.R, which loads the packages and the shared modules
# these functions use; source that file rather than this one alone.

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
      "AME actual performance: %d evacuated (mean wait %.1f days, p10-p90 %.1f-%.1f days), %d still awaiting AME at end of run\n",
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
    "Role 4 demand: %.0f reached Role 4 via AME, peak occupancy %.1f (day %d); unconstrained-baseline demand would need %d total AME sorties (capacity %d/sortie)\n",
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
