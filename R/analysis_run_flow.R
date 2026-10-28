##############################################
## R/analysis_run_flow.R                    ##
## Single run: casualty flow and outcomes   ##
##############################################
#
# The single-run stages reporting the casualty stream as a whole: the casualty
# overview and summary tables, waiting times, transport capacity margin,
# force regeneration, casualty surge events, treatment intervals and outcomes
# by echelon.
#
# Sourced by R/analysis.R, which loads the packages and the shared modules
# these functions use; source that file rather than this one alone.

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
  # (post-operative), 5 = awaiting strategic AME, 6 = R2E (post-definitive)
  echelon_labels <- c("1" = "r1", "2" = "r2b", "3" = "r2e", "4" = "r2e_postop", "5" = "ame_wait",
                      "6" = "r2e_post_definitive")
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
  # against the Vietnam War return-to-duty rates in README Return to Duty.
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
