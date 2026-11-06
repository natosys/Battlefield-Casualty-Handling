##############################################
## R/analysis_replications.R                ##
## Multi-replication analysis pipeline      ##
##############################################
#
# The stage functions of a replicated run's analysis, each reporting one
# domain with its interval across replications, and analyse_replications(),
# the orchestrator over them.
#
# Sourced by R/analysis.R, which loads the packages and the shared modules
# these functions use; source that file rather than this one alone.

# Jitter applied to the pooled casualty surge timeline is cosmetic — it
# separates events that fall on the same day — but it is drawn at render
# time, so an unseeded jitter gives a different image every time the same
# monitoring data is plotted. A fixed seed makes the tracked image a function
# of the data alone; position_jitter() restores the caller's stream itself,
# so the pipeline stays stream-neutral.
#' Seed the casualty surge timeline's jitter is drawn under
#'
#' @details position_jitter() draws from the session stream at plot time, so
#'   an unseeded jitter gives a different image every time the same
#'   monitoring data is plotted. Fixing the seed makes the tracked image a
#'   function of the data alone. Any value serves; changing it changes the
#'   tracked image without changing what it says.
CASUALTY_SURGE_JITTER_SEED <- 233L

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
#'   of time bins (mirrors bin_post_op_overflow(), R/warmup.R), then computes a
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

#' Prepare the pooled frames every later stage of a multi-run analysis reads
#'
#' @param mon See analyse_run().
#' @param warm_up_period See analyse_run().
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `arrivals`, `arrivals_raw`, `attributes_raw`, `n_reps`, `resources_raw`.
prepare_replication_frames <- function(mon, warm_up_period, output_dir, images_dir) {
  validate_monitoring(mon, "analyse_replications")
  usable_warm_up <- is.numeric(warm_up_period) && length(warm_up_period) == 1L &&
    !is.na(warm_up_period) && warm_up_period >= 0
  if (!usable_warm_up) {
    stop(sprintf(paste0("analyse_replications: warm_up_period must be a single ",
                        "non-negative number, found %s"),
                 paste(format(warm_up_period), collapse = ", ")), call. = FALSE)
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(images_dir,  showWarnings = FALSE, recursive = TRUE)

  warm_up_min    <- as.integer(warm_up_period) * DAY_MIN
  arrivals_raw   <- mon$arrivals    %>% filter(start_time >= warm_up_min)
  attributes_raw <- mon$attributes
  resources_raw  <- mon$resources   %>% filter(time >= warm_up_min)

  write.csv(arrivals_raw,   file.path(output_dir, "mon_arrivals.csv"),   row.names = FALSE)
  write.csv(attributes_raw, file.path(output_dir, "mon_attributes.csv"), row.names = FALSE)
  write.csv(resources_raw,  file.path(output_dir, "mon_resources.csv"),  row.names = FALSE)

  n_reps <- n_distinct(arrivals_raw$replication)

  arrivals <- arrivals_raw %>%
    mutate(arrival_day = floor(start_time / DAY_MIN) + 1)
  list(
    arrivals = arrivals,
    arrivals_raw = arrivals_raw,
    attributes_raw = attributes_raw,
    n_reps = n_reps,
    resources_raw = resources_raw
  )
}

#' Summarise the headline indicators as mean and 95% confidence interval
#'
#' @param arrivals_raw Arrival monitor rows as read, before waiting time is derived.
#' @param attributes_raw Attribute monitor rows as read.
#' @param resources_raw Resource monitor rows as read.
#' @return A list of `clamp_ci`, `kpi_summary`.
summarise_replication_kpi_cards <- function(arrivals_raw, attributes_raw, resources_raw) {
  total_cas_per_rep <- arrivals_raw %>% count(replication, name = "total")

  dow_per_rep <- attributes_raw %>%
    filter(key == "dow", value == 1) %>%
    count(replication, name = "dow_count") %>%
    right_join(data.frame(replication = unique(arrivals_raw$replication)), by = "replication") %>%
    mutate(dow_count = coalesce(dow_count, 0L))

  #' Peak total queue length per replication for a resource group
  #'
  #' @param resources Resource monitor rows across every replication.
  #' @param pattern Regular expression selecting the resources to pool.
  #' @return A data frame of replication and its peak pooled queue length.
  #' @details The queue is summed across the matching resources at each
  #'   timestamp before the maximum is taken, so the figure is the peak demand
  #'   on the group rather than the largest queue any one member saw.
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

  #' Clamp a confidence interval's lower bound at zero
  #'
  #' @param cm A `ci_mean()` result, carrying `lower`, `mean` and `upper`.
  #' @return The same list with `lower` raised to 0 where it fell below it.
  #' @details Every KPI this is applied to is a non-negative count by
  #'   construction, so a t-interval at low replication count and high variance
  #'   (the DOW count is the usual case) can otherwise report a negative lower
  #'   bound that the quantity cannot take.
  clamp_ci <- function(cm) { cm[["lower"]] <- max(cm[["lower"]], 0); cm }

  kpi_summary <- list(
    total_casualties   = clamp_ci(ci_mean(total_cas_per_rep$total)),
    dow_count          = clamp_ci(ci_mean(dow_per_rep$dow_count)),
    r2e_icu_peak_queue = clamp_ci(ci_mean(r2e_icu_peak_per_rep$peak_q)),
    r2b_ot_peak_queue  = clamp_ci(ci_mean(r2b_ot_peak_per_rep$peak_q))
  )
  list(
    clamp_ci = clamp_ci,
    kpi_summary = kpi_summary
  )
}

#' Pool every replication's per-casualty attributes into one frame
#'
#' @param arrivals Arrival monitor rows with a waiting_time column.
#' @param arrivals_raw Arrival monitor rows as read, before waiting time is derived.
#' @param attributes_raw Attribute monitor rows as read.
#' @return A list of `attributes_wide`, `combined`, `echelon_labels`, `rep_ids`.
build_replication_attributes <- function(arrivals, arrivals_raw, attributes_raw) {
  attributes_wide <- build_attributes_wide(attributes_raw, arrivals_raw)
  combined <- arrivals %>%
    left_join(attributes_wide, by = c("name", "replication")) %>%
    mutate(
      casualty_type     = str_extract(name, "^[^_]+"),
      population_source = str_extract(name, "(?<=_)[a-zA-Z]+")
    )

  echelon_labels <- c("1" = "r1", "2" = "r2b", "3" = "r2e", "4" = "r2e_postop", "5" = "ame_wait",
                      "6" = "r2e_post_definitive")
  rep_ids        <- unique(arrivals_raw$replication)
  list(
    attributes_wide = attributes_wide,
    combined = combined,
    echelon_labels = echelon_labels,
    rep_ids = rep_ids
  )
}

#' Summarise died-of-wounds counts by echelon across replications
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param echelon_labels See analyse_run().
#' @param rep_ids See analyse_run().
#' @param output_dir Directory the tables are written to.
#' @return The dow_by_echelon_ci object.
summarise_dow_by_echelon_ci <- function(attributes_wide, echelon_labels, rep_ids, output_dir) {
  dow_by_echelon_rep <- attributes_wide %>%
    filter(dow == 1 & !is.na(dow_echelon)) %>%
    mutate(dow_echelon = echelon_labels[as.character(as.integer(dow_echelon))]) %>%
    count(replication, dow_echelon, name = "dow_count") %>%
    complete(replication = rep_ids, dow_echelon = unname(echelon_labels), fill = list(dow_count = 0L))

  dow_by_echelon_ci <- ci_by_group(dow_by_echelon_rep, "dow_echelon", "dow_count") %>%
    rename(dow_count_mean = mean, dow_count_ci_lower = ci_lower, dow_count_ci_upper = ci_upper) %>%
    dplyr::select(-sd)
  write.csv(dow_by_echelon_ci, file.path(output_dir, "dow_by_echelon_ci.csv"), row.names = FALSE)
  dow_by_echelon_ci
}

#' Summarise return-to-duty counts by type and echelon across replications
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param clamp_ci See analyse_run().
#' @param echelon_labels See analyse_run().
#' @param rep_ids See analyse_run().
#' @param output_dir Directory the tables are written to.
#' @return A list of `rtd_by_echelon_ci`, `rtd_summary_ci`.
summarise_rtd_by_echelon_ci <- function(attributes_wide, clamp_ci, echelon_labels, rep_ids,
                                        output_dir) {
  rtd_type_per_rep <- attributes_wide %>%
    filter(!is.na(return_day)) %>%
    mutate(rtd_type = if_else(!is.na(dnbi_type) & dnbi_type == 1L, "battle_fatigue", "clinical")) %>%
    count(replication, rtd_type, name = "rtd_count") %>%
    complete(replication = rep_ids, rtd_type = c("battle_fatigue", "clinical"), fill = list(rtd_count = 0L))

  bf_rtd_ci         <- clamp_ci(ci_mean(rtd_type_per_rep$rtd_count[rtd_type_per_rep$rtd_type == "battle_fatigue"]))
  clinical_rtd_ci   <- clamp_ci(ci_mean(rtd_type_per_rep$rtd_count[rtd_type_per_rep$rtd_type == "clinical"]))
  total_rtd_per_rep <- rtd_type_per_rep %>% group_by(replication) %>% summarise(total = sum(rtd_count), .groups = "drop")
  total_rtd_ci      <- clamp_ci(ci_mean(total_rtd_per_rep$total))
  rtd_summary_ci    <- list(bf_rtd = bf_rtd_ci, clinical_rtd = clinical_rtd_ci, total_rtd = total_rtd_ci)

  rtd_by_echelon_rep <- attributes_wide %>%
    filter(!is.na(return_day) & !is.na(return_echelon)) %>%
    mutate(
      return_echelon = echelon_labels[as.character(as.integer(return_echelon))],
      rtd_type       = if_else(!is.na(dnbi_type) & dnbi_type == 1L, "battle_fatigue", "clinical")
    ) %>%
    count(replication, return_echelon, rtd_type, name = "rtd_count") %>%
    complete(replication = rep_ids, return_echelon = unname(echelon_labels),
             rtd_type = c("battle_fatigue", "clinical"), fill = list(rtd_count = 0L))

  rtd_by_echelon_ci <- ci_by_group(rtd_by_echelon_rep, c("return_echelon", "rtd_type"), "rtd_count") %>%
    rename(rtd_count_mean = mean, rtd_count_ci_lower = ci_lower, rtd_count_ci_upper = ci_upper) %>%
    dplyr::select(-sd)
  write.csv(rtd_by_echelon_ci, file.path(output_dir, "rtd_by_echelon_ci.csv"), row.names = FALSE)
  list(
    rtd_by_echelon_ci = rtd_by_echelon_ci,
    rtd_summary_ci = rtd_summary_ci
  )
}

#' Summarise the six R2B routing counts across replications
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param clamp_ci See analyse_run().
#' @param rep_ids See analyse_run().
#' @return The r2b_routing_summary_ci object.
summarise_r2b_routing_ci <- function(attributes_wide, clamp_ci, rep_ids) {
  #' Confidence interval for one R2B routing indicator's per-run count
  #'
  #' @param col Name of the indicator column in `attributes_wide`.
  #' @return A clamped `ci_mean()` result over the per-replication counts.
  #' @details A replication in which the indicator never fired contributes a
  #'   zero rather than dropping out, and an indicator absent from the whole
  #'   monitoring set is treated as zero in every replication, so the interval
  #'   is taken over the same denominator either way.
  r2b_diag_col_ci <- function(col) {
    if (!col %in% names(attributes_wide)) return(clamp_ci(ci_mean(rep(0, length(rep_ids)))))
    per_rep <- attributes_wide %>%
      filter(!is.na(.data[[col]]) & .data[[col]] == 1L) %>%
      count(replication, name = "n_evt") %>%
      complete(replication = rep_ids, fill = list(n_evt = 0L))
    clamp_ci(ci_mean(per_rep$n_evt))
  }
  r2b_routing_summary_ci <- list(
    r2b_pre_bypass_count  = r2b_diag_col_ci("r2b_bypassed"),
    r2b_hold_bypass_count = r2b_diag_col_ci("r2b_hold_bypass"),
    r2b_hold_queued_count = r2b_diag_col_ci("r2b_hold_queued")
  )
  if ("r2b_bypass_reason" %in% names(attributes_wide)) {
    r2b_bypass_reason_per_rep <- attributes_wide %>%
      filter(!is.na(r2b_bypass_reason)) %>%
      mutate(reason = ifelse(r2b_bypass_reason == 1, "offshift", "busy")) %>%
      count(replication, reason, name = "n_evt") %>%
      complete(replication = rep_ids, reason = c("offshift", "busy"), fill = list(n_evt = 0L))
  } else {
    r2b_bypass_reason_per_rep <- expand.grid(replication = rep_ids, reason = c("offshift", "busy"), n_evt = 0L)
  }
  r2b_bypass_total_per_rep <- r2b_bypass_reason_per_rep %>% group_by(replication) %>% summarise(total = sum(n_evt), .groups = "drop")
  r2b_routing_summary_ci$r2b_ot_bypass_offshift_count <-
    clamp_ci(ci_mean(r2b_bypass_reason_per_rep$n_evt[r2b_bypass_reason_per_rep$reason == "offshift"]))
  r2b_routing_summary_ci$r2b_ot_bypass_busy_count <-
    clamp_ci(ci_mean(r2b_bypass_reason_per_rep$n_evt[r2b_bypass_reason_per_rep$reason == "busy"]))
  r2b_routing_summary_ci$r2b_ot_bypass_count <- clamp_ci(ci_mean(r2b_bypass_total_per_rep$total))
  r2b_routing_summary_ci
}

#' Plot R2B holding-bed occupancy by stream, mean and confidence interval
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @param n_reps See analyse_run().
#' @param rep_ids See analyse_run().
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `r2b_hold_daily_ci`, `r2b_hold_occupancy_plot`.
plot_r2b_hold_occupancy_ci <- function(attributes_wide, combined, n_reps, rep_ids, output_dir,
                                       images_dir) {
  r2b_hold_occupancy_plot <- NULL
  r2b_hold_daily_ci       <- NULL
  if ("r2b_hold_start" %in% names(attributes_wide) && any(!is.na(attributes_wide$r2b_hold_start))) {
    n_sim_days_hold <- ceiling(max(combined$start_time, na.rm = TRUE) / DAY_MIN)

    hold_patients <- r2b_hold_episodes(attributes_wide, n_sim_days_hold * DAY_MIN)

    hold_capacity <- r2b_hold_capacity()

    r2b_hold_daily_rep <- hold_patients %>%
      rowwise() %>%
      mutate(day = list(seq(max(1L, floor(hold_start_min / DAY_MIN) + 1L),
                            min(n_sim_days_hold, ceiling(hold_end_min / DAY_MIN))))) %>%
      unnest(day) %>%
      ungroup() %>%
      mutate(r2b_label = paste("R2B", r2b)) %>%
      count(replication, day, stream, r2b_label, name = "occupancy") %>%
      complete(replication = rep_ids, day = seq_len(n_sim_days_hold),
               stream = c("Disease DNBI", "NBI", "WIA"),
               r2b_label = hold_capacity$r2b_label, fill = list(occupancy = 0))

    r2b_hold_daily_ci <- ci_by_group(r2b_hold_daily_rep, c("day", "stream", "r2b_label"),
                                     "occupancy")

    # One panel per R2B unit, as in the single-run figure, so each unit's load
    # is drawn against its own establishment rather than both against one line.
    r2b_hold_occupancy_plot <- ggplot(r2b_hold_daily_ci,
                                      aes(x = day, y = mean, color = stream, fill = stream)) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.25, color = NA) +
      geom_line(linewidth = 0.9) +
      geom_hline(data = hold_capacity, aes(yintercept = n_hold_beds),
                 linetype = "dashed", color = "red", linewidth = 0.8,
                 inherit.aes = FALSE) +
      facet_wrap(~ r2b_label, ncol = 1) +
      scale_x_continuous(breaks = seq(1, n_sim_days_hold, by = 2), expand = c(0, 0)) +
      labs(
        title    = "R2B Hold Bed Daily Occupancy by Patient Stream — Mean ± 95% CI Across Replications",
        subtitle = sprintf(
          "%d replications; one panel per R2B unit, dashed line = that unit's establishment (%s)",
          n_reps,
          paste(sprintf("%s: %d beds", hold_capacity$r2b_label, hold_capacity$n_hold_beds),
                collapse = ", ")
        ),
        x = "Simulation Day", y = "Casualties Occupying a Hold Bed", color = "Stream", fill = "Stream"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom")

    ggsave(file.path(images_dir, "r2b_hold_occupancy_multirun.png"), r2b_hold_occupancy_plot, width = 12, height = 6, dpi = 150)
    write.csv(r2b_hold_daily_ci, file.path(output_dir, "r2b_hold_occupancy_ci.csv"), row.names = FALSE)
  }
  list(
    r2b_hold_daily_ci = r2b_hold_daily_ci,
    r2b_hold_occupancy_plot = r2b_hold_occupancy_plot
  )
}

#' Plot why casualties were not operated on at R2B, per day
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @param n_reps See analyse_run().
#' @param rep_ids See analyse_run().
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return The r2b_bypass_reason_plot_full object.
plot_r2b_bypass_reason_ci <- function(attributes_wide, combined, n_reps, rep_ids, output_dir,
                                      images_dir) {
  r2b_bypass_reason_plot_full <- NULL
  if ("r2b_bypass_reason" %in% names(attributes_wide) && any(!is.na(attributes_wide$r2b_bypass_reason))) {
    day_range_bypass         <- seq(min(floor(combined$start_time / DAY_MIN) + 1), max(floor(combined$start_time / DAY_MIN) + 1))
    r2b_bypass_reason_levels <- c("Team off-shift", "OT busy / queued")

    r2b_bypass_daily_rep <- attributes_wide %>%
      filter(!is.na(r2b_bypass_reason)) %>%
      mutate(
        day    = floor(r2b_bypass_time / DAY_MIN) + 1,
        reason = factor(ifelse(r2b_bypass_reason == 1, "Team off-shift", "OT busy / queued"),
                        levels = r2b_bypass_reason_levels)
      ) %>%
      count(replication, day, reason, name = "n_evt") %>%
      complete(replication = rep_ids, day = day_range_bypass, reason = r2b_bypass_reason_levels,
               fill = list(n_evt = 0L))

    r2b_bypass_daily_ci <- ci_by_group(r2b_bypass_daily_rep, c("day", "reason"), "n_evt")

    r2b_bypass_reason_plot_full <- ggplot(r2b_bypass_daily_ci, aes(x = factor(day), y = mean, fill = reason)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.65) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = 0.7), width = 0.25) +
      scale_fill_manual(values = c("Team off-shift" = "#2a78d6", "OT busy / queued" = "#1baf7a")) +
      labs(
        title    = "R2B OT Bypass Reason per Simulation Day — Mean ± 95% CI Across Replications",
        subtitle = sprintf("%d replications", n_reps),
        x = "Simulation Day", y = "Mean Casualties Bypassed", fill = "Reason"
      ) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom")

    ggsave(file.path(images_dir, "r2b_ot_bypass_reason_multirun.png"), r2b_bypass_reason_plot_full, width = 12, height = 6, dpi = 150)
    write.csv(r2b_bypass_daily_ci, file.path(output_dir, "r2b_bypass_reason_ci.csv"), row.names = FALSE)
  }
  r2b_bypass_reason_plot_full
}

#' Summarise the post-operative pathway split across replications
#'
#' @param attributes_wide One row per casualty, attributes pivoted to columns.
#' @param clamp_ci See analyse_run().
#' @param rep_ids See analyse_run().
#' @param output_dir Directory the tables are written to.
#' @return A list of `post_op_pathway_ci`, `surgery_deferred_summary_ci`,
#'   `definitive_repair_outstanding_summary_ci`.
summarise_post_op_pathway_ci <- function(attributes_wide, clamp_ci, rep_ids, output_dir) {
  post_op_pathway_ci  <- NULL
  if ("post_op_pathway" %in% names(attributes_wide) && any(!is.na(attributes_wide$post_op_pathway))) {
    pathway_labels <- c("1" = "icu", "2" = "hold", "3" = "overflow")
    post_op_per_rep <- attributes_wide %>%
      filter(!is.na(post_op_pathway)) %>%
      mutate(
        pathway    = pathway_labels[as.character(as.integer(post_op_pathway))],
        postop_dow = as.integer(!is.na(dow_echelon) & dow_echelon == 4 & dow == 1)
      ) %>%
      count(replication, pathway, postop_dow, name = "n_cas") %>%
      complete(replication = rep_ids, pathway = c("icu", "hold", "overflow"),
               postop_dow = c(0L, 1L), fill = list(n_cas = 0L)) %>%
      group_by(replication, pathway) %>%
      summarise(total = sum(n_cas), died = sum(n_cas[postop_dow == 1]), .groups = "drop")

    total_ci_pop <- ci_by_group(post_op_per_rep, "pathway", "total") %>%
      rename(total_mean = mean, total_ci_lower = ci_lower, total_ci_upper = ci_upper) %>%
      dplyr::select(-sd)
    died_ci_pop <- ci_by_group(post_op_per_rep, "pathway", "died") %>%
      rename(died_mean = mean, died_ci_lower = ci_lower, died_ci_upper = ci_upper) %>%
      dplyr::select(pathway, died_mean, died_ci_lower, died_ci_upper)

    post_op_pathway_ci <- total_ci_pop %>%
      left_join(died_ci_pop, by = "pathway") %>%
      mutate(postop_dow_rate = died_mean / total_mean)
    write.csv(post_op_pathway_ci, file.path(output_dir, "post_op_pathway_summary_ci.csv"), row.names = FALSE)
  }

  surgery_deferred_summary_ci <- clamp_ci(ci_mean(rep(0, length(rep_ids))))
  if ("surgery_deferred" %in% names(attributes_wide)) {
    surgery_deferred_per_rep <- attributes_wide %>%
      filter(!is.na(surgery_deferred) & surgery_deferred == 1L) %>%
      count(replication, name = "n_evt") %>%
      complete(replication = rep_ids, fill = list(n_evt = 0L))
    surgery_deferred_summary_ci <- clamp_ci(ci_mean(surgery_deferred_per_rep$n_evt))
  }

  definitive_repair_outstanding_summary_ci <- clamp_ci(ci_mean(rep(0, length(rep_ids))))
  if ("definitive_repair_outstanding" %in% names(attributes_wide)) {
    outstanding_per_rep <- attributes_wide %>%
      filter(!is.na(definitive_repair_outstanding) & definitive_repair_outstanding == 1L) %>%
      count(replication, name = "n_evt") %>%
      complete(replication = rep_ids, fill = list(n_evt = 0L))
    definitive_repair_outstanding_summary_ci <- clamp_ci(ci_mean(outstanding_per_rep$n_evt))
  }
  list(
    post_op_pathway_ci = post_op_pathway_ci,
    surgery_deferred_summary_ci = surgery_deferred_summary_ci,
    definitive_repair_outstanding_summary_ci = definitive_repair_outstanding_summary_ci
  )
}

#' Plot transport queue depth per vehicle, mean and confidence interval
#'
#' @param clamp_ci See analyse_run().
#' @param n_reps See analyse_run().
#' @param resources_raw Resource monitor rows as read.
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `p_transport_capacity_margin_ci`, `transport_utilisation_ci`.
plot_transport_capacity_margin_ci <- function(clamp_ci, n_reps, resources_raw, output_dir,
                                              images_dir) {
  transport_queue_data_mr <- resources_raw %>%
    filter(grepl("^t_(PMVAmb|HX240M)_\\d+$", resource)) %>%
    mutate(
      platform   = str_extract(resource, "(?<=^t_)[^_]+"),
      unit_id    = str_extract(resource, "\\d+$") %>% as.integer(),
      unit_label = paste(platform, unit_id)
    )
  transport_bins_mr <- bin_queue_ci(transport_queue_data_mr, c("platform", "unit_label"))
  p_transport_capacity_margin_ci <- ggplot() +
    geom_step(data = transport_bins_mr$traces,
              aes(x = bin_min / DAY_MIN, y = queue, group = interaction(replication, resource)),
              color = "steelblue", alpha = 0.06) +
    geom_ribbon(data = transport_bins_mr$ci, aes(x = bin_min / DAY_MIN, ymin = ci_lower, ymax = ci_upper, fill = unit_label),
                alpha = 0.3) +
    geom_line(data = transport_bins_mr$ci, aes(x = bin_min / DAY_MIN, y = mean_q, color = unit_label), linewidth = 0.9) +
    facet_wrap(~ platform, ncol = 1, scales = "free_y") +
    labs(title = "Transport Fleet Capacity Margin — Queue Over Time — Mean ± 95% CI Across Replications",
         subtitle = sprintf("%d replications; faint traces are individual replications", n_reps),
         x = "Time (Days)", y = "Queue Size", color = "Vehicle", fill = "Vehicle") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file.path(images_dir, "transport_capacity_margin_multirun.png"), p_transport_capacity_margin_ci,
         width = 12, height = 8, dpi = 150)

  transport_utilisation_ci <- bind_rows(lapply(c("PMVAmb", "HX240M"), function(veh) {
    df <- utilisation_per_replication(resources_raw, paste0("^t_", veh, "_"))
    if (nrow(df) == 0) return(NULL)
    cm <- clamp_ci(ci_mean(df$utilisation))
    data.frame(platform = veh, mean_u = cm[["mean"]], ci_lower = cm[["lower"]], ci_upper = min(cm[["upper"]], 1))
  }))
  write.csv(transport_utilisation_ci, file.path(output_dir, "transport_utilisation_ci.csv"), row.names = FALSE)
  list(
    p_transport_capacity_margin_ci = p_transport_capacity_margin_ci,
    transport_utilisation_ci = transport_utilisation_ci
  )
}

#' Summarise the treatment-interval indicators across replications
#'
#' @param clamp_ci See analyse_run().
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @return The dwell_time_summary_ci object.
summarise_treatment_intervals_ci <- function(clamp_ci, combined) {
  #' Confidence interval over the per-replication means of one interval
  #'
  #' @param df Rows carrying a `.val` column of the interval in minutes.
  #' @return A clamped `ci_mean()` result over the per-replication means.
  #' @details The mean is taken within a replication before the interval is
  #'   taken across them, so a replication carrying more casualties does not
  #'   weigh more heavily than one carrying fewer.
  kpi_per_rep_ci <- function(df) {
    per_rep <- df %>% group_by(replication) %>% summarise(mean_val = mean(.val), .groups = "drop")
    clamp_ci(ci_mean(per_rep$mean_val))
  }
  time_to_first_surgery_rep <- combined %>%
    mutate(first_surgery_start = pmin(as.numeric(r2b_surgery_start), as.numeric(r2e_surgery_1_start), na.rm = TRUE),
           .val = first_surgery_start - start_time) %>%
    filter(casualty_type != "kia", !(dow == 1 & is.na(first_surgery_start)), !is.na(.val))
  time_to_first_surgery_ci <- kpi_per_rep_ci(time_to_first_surgery_rep)

  window_min <- observation_window_min(combined)
  #' Confidence interval over the per-replication restricted means of one interval
  #'
  #' @param start_col Name of the column holding the interval's start, in minutes.
  #' @param end_col Name of the column holding its end, in minutes.
  #' @return A clamped `ci_mean()` result over the per-replication restricted means.
  #' @details Each replication is summarised by censored_interval_stats() before
  #'   the interval is taken across replications, so a stay still running when
  #'   one replication ends is carried as the lower bound it is rather than
  #'   dropped from that replication's mean.
  censored_per_rep_ci <- function(start_col, end_col) {
    per_rep <- combined %>%
      group_by(replication) %>%
      summarise(
        mean_val = censored_interval_stats(.data[[start_col]], .data[[end_col]],
                                           window_min)$mean_min,
        .groups = "drop"
      )
    clamp_ci(ci_mean(per_rep$mean_val[is.finite(per_rep$mean_val)]))
  }

  combined$r2b_leave_time <- r2b_leave_time(combined)
  r2b_dwell_time_ci <- censored_per_rep_ci("r2b_treatment_start_time", "r2b_leave_time")
  r2b_r2e_transit_time_ci <- censored_per_rep_ci("r2b_departure_time", "r2e_arrival_time")
  r2e_dwell_time_ci <- censored_per_rep_ci("r2e_arrival_time", "r2e_departure_time")

  dwell_time_summary_ci <- list(
    time_to_first_surgery = time_to_first_surgery_ci,
    r2b_dwell_time         = r2b_dwell_time_ci,
    r2b_r2e_transit_time   = r2b_r2e_transit_time_ci,
    r2e_dwell_time          = r2e_dwell_time_ci
  )
  dwell_time_summary_ci
}

#' Summarise Role 4 occupancy and sortie demand across replications
#'
#' @param clamp_ci See analyse_run().
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @param n_reps See analyse_run().
#' @param rep_ids See analyse_run().
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `ame_summary_ci`, `role4_census_ci_plot`, `role4_census_daily_ci`,
#'   `role4_summary_ci`.
summarise_role4_demand_ci <- function(clamp_ci, combined, n_reps, rep_ids, output_dir, images_dir) {
  role4_census_ci_plot     <- NULL
  role4_census_daily_ci    <- NULL
  role4_summary_ci         <- NULL
  ame_summary_ci           <- NULL
  role4_daily_by_rep       <- data.frame(replication = integer(0), day = integer(0), ward = character(0), occupancy = integer(0))

  if (!is.null(env_data$vars$role4)) {
    role4_params <- env_data$vars$role4
    ame_airframe <- resolve_ame_airframe(role4_params)
    ame_capacity <- ame_airframe$critical_capacity + ame_airframe$standard_capacity
    role4_daily_by_rep <- compute_role4_census(combined, role4_params)
    ame_by_rep         <- compute_ame_demand(combined, ame_capacity)
  }

  if (!is.null(env_data$vars$role4) && nrow(role4_daily_by_rep) > 0) {
    n_sim_days_role4  <- ceiling(max(combined$start_time, na.rm = TRUE) / DAY_MIN)
    ward_levels       <- role4_ward_levels(role4_params)
    max_discharge_day <- max(role4_daily_by_rep$day, na.rm = TRUE)

    role4_census_daily_rep <- role4_daily_by_rep %>%
      complete(replication = rep_ids, day = seq_len(max_discharge_day), ward = ward_levels,
               fill = list(occupancy = 0))

    role4_census_daily_ci <- ci_by_group(role4_census_daily_rep, c("day", "ward"), "occupancy") %>%
      mutate(ward = factor(ward, levels = ward_levels))

    role4_census_ci_plot <- ggplot(role4_census_daily_ci, aes(x = day, y = mean, fill = ward)) +
      geom_col(position = "stack") +
      geom_vline(xintercept = n_sim_days_role4 + 0.5, linetype = "dotted", color = "gray40") +
      scale_fill_brewer(palette = "Set2") +
      scale_x_continuous(breaks = seq(1, max_discharge_day, by = 2), expand = c(0, 0)) +
      labs(
        title    = "Role 4 (National Support Base) Daily Bed Occupancy by Bed Type — Mean Across Replications",
        subtitle = sprintf(
          "%d replications; dotted line = end of %d-day engagement window; stacked mean shown, per-bed-type 95%% CI in downloadable data",
          n_reps, n_sim_days_role4
        ),
        x = "Simulation Day", y = "Mean Concurrent Patients", fill = "Bed type"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom")

    ggsave(file.path(images_dir, "role4_census_multirun.png"), role4_census_ci_plot, width = 12, height = 6, dpi = 150)
    write.csv(role4_census_daily_ci, file.path(output_dir, "role4_census_daily_ci.csv"), row.names = FALSE)

    role4_peak_by_rep <- role4_daily_by_rep %>%
      group_by(replication, day) %>%
      summarise(total_occupancy = sum(occupancy), .groups = "drop") %>%
      group_by(replication) %>%
      summarise(peak_occupancy = max(total_occupancy), .groups = "drop") %>%
      complete(replication = rep_ids, fill = list(peak_occupancy = 0))

    total_evac_by_rep <- combined %>%
      filter(!is.na(evacuation_day)) %>%
      count(replication, name = "total_evacuated") %>%
      complete(replication = rep_ids, fill = list(total_evacuated = 0L))

    role4_summary_ci <- list(
      total_evacuated = clamp_ci(ci_mean(total_evac_by_rep$total_evacuated)),
      peak_occupancy  = clamp_ci(ci_mean(role4_peak_by_rep$peak_occupancy))
    )

    ame_demand_by_rep <- ame_by_rep %>%
      complete(replication = rep_ids, day = seq_len(n_sim_days_role4),
               fill = list(evacuation_count = 0, sorties_required = 0)) %>%
      group_by(replication) %>%
      summarise(total_sorties = sum(sorties_required), peak_daily_sorties = max(sorties_required), .groups = "drop")

    ame_summary_ci <- list(
      total_sorties      = clamp_ci(ci_mean(ame_demand_by_rep$total_sorties)),
      peak_daily_sorties = clamp_ci(ci_mean(ame_demand_by_rep$peak_daily_sorties))
    )

    write.csv(role4_peak_by_rep, file.path(output_dir, "role4_peak_by_rep.csv"), row.names = FALSE)
    write.csv(ame_demand_by_rep, file.path(output_dir, "ame_demand_by_rep.csv"), row.names = FALSE)
  }
  list(
    ame_summary_ci = ame_summary_ci,
    role4_census_ci_plot = role4_census_ci_plot,
    role4_census_daily_ci = role4_census_daily_ci,
    role4_summary_ci = role4_summary_ci
  )
}

#' Summarise realised evacuation wait time by route, pooled across replications
#'
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @param output_dir Directory the tables are written to.
#' @return The ame_wait_time_summary_mr object.
summarise_ame_wait_by_route <- function(combined, output_dir) {
  ame_wait_time_summary_mr <- NULL
  route_labels <- c("1" = "Critical (ICU, CCATT/CCAST)", "2" = "Standard (Hold, CSU)")
  ame_evac_decisions_mr <- combined %>%
    filter(!is.na(r2e_evac) & r2e_evac == 1) %>%
    mutate(route = route_labels[as.character(ame_route)])

  if (nrow(ame_evac_decisions_mr) > 0) {
    #' Summarise strategic evacuation wait times for one route
    #'
    #' @param df Evacuation decision rows pooled across replications.
    #' @return A one-row data frame of the evacuated and awaiting counts and
    #'   the mean, p10 and p90 wait in minutes.
    #' @details Pooled across replications rather than summarised per
    #'   replication, because the quantity reported is the distribution of one
    #'   casualty's wait and not the distribution of a run's mean wait.
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
    ame_wait_time_summary_mr <- bind_rows(
      data.frame(route = "Overall", wait_stats(ame_evac_decisions_mr)),
      lapply(setNames(route_labels, route_labels), function(r) {
        wait_stats(ame_evac_decisions_mr %>% filter(route == r))
      }) %>%
        bind_rows(.id = "route")
    )
    write.csv(ame_wait_time_summary_mr, file.path(output_dir, "ame_wait_time_summary_multirun.csv"), row.names = FALSE)
  }
  ame_wait_time_summary_mr
}

#' Summarise the casualty surge stress test, pooled across replications
#'
#' @param clamp_ci See analyse_run().
#' @param combined Arrivals joined to attributes_wide, with casualty type, population
#'   source and arrival day derived.
#' @param n_reps See analyse_run().
#' @param rep_ids See analyse_run().
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `casualty_surge_dow_summary_mr`, `casualty_surge_event_count_ci`,
#'   `casualty_surge_events_summary_mr`, `casualty_surge_timeline_plot_mr`.
summarise_casualty_surge_ci <- function(clamp_ci, combined, n_reps, rep_ids, output_dir,
                                        images_dir) {
  casualty_surge_tagged_mr <- combined %>%
    filter(!is.na(casualty_surge_event) & casualty_surge_event == 1)

  casualty_surge_events_summary_mr <- if (nrow(casualty_surge_tagged_mr) > 0) {
    reconstruct_surge_events(casualty_surge_tagged_mr)
  } else {
    data.frame(replication = integer(0), event_id = integer(0), event_start = numeric(0),
               event_end = numeric(0), n_cas = integer(0), n_kia = integer(0),
               n_wia = integer(0), event_day = numeric(0))
  }

  casualty_surge_event_count_per_rep <- casualty_surge_events_summary_mr %>%
    count(replication, name = "n_events") %>%
    complete(replication = rep_ids, fill = list(n_events = 0L))
  casualty_surge_event_count_ci <- clamp_ci(ci_mean(casualty_surge_event_count_per_rep$n_events))

  casualty_surge_dow_summary_mr <- combined %>%
    filter(!is.na(casualty_surge_event)) %>%
    mutate(origin = if_else(casualty_surge_event == 1, "Casualty Surge Event", "Background")) %>%
    group_by(origin) %>%
    summarise(total = n(), dow = sum(dow == 1, na.rm = TRUE), dow_rate = dow / total, .groups = "drop")

  write.csv(casualty_surge_events_summary_mr, file.path(output_dir, "casualty_surge_events_summary_multirun.csv"), row.names = FALSE)
  write.csv(casualty_surge_dow_summary_mr, file.path(output_dir, "casualty_surge_dow_summary_multirun.csv"), row.names = FALSE)

  casualty_surge_timeline_plot_mr <- NULL
  if (nrow(casualty_surge_events_summary_mr) > 0) {
    n_sim_days_mc <- ceiling(max(combined$start_time, na.rm = TRUE) / DAY_MIN)
    casualty_surge_timeline_plot_mr <- ggplot(casualty_surge_events_summary_mr, aes(x = event_start / DAY_MIN, y = n_cas)) +
      geom_point(position = position_jitter(width = 0.15, height = 0,
                                            seed = CASUALTY_SURGE_JITTER_SEED),
                 alpha = 0.35, color = "#D62828", size = 2) +
      scale_x_continuous(limits = c(0, n_sim_days_mc), breaks = seq(0, n_sim_days_mc, by = 2)) +
      labs(
        title    = "Casualty Surge Event Timeline — All Replications Pooled",
        subtitle = sprintf(
          "%d events across %d replications (mean %.2f events/replication); points jittered to reduce overplotting",
          nrow(casualty_surge_events_summary_mr), n_reps, casualty_surge_event_count_ci[["mean"]]
        ),
        x = "Simulation Day", y = "Casualties Injected by Event"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
    ggsave(file.path(images_dir, "casualty_surge_events_multirun.png"), casualty_surge_timeline_plot_mr, width = 12, height = 6, dpi = 150)
  }
  list(
    casualty_surge_dow_summary_mr = casualty_surge_dow_summary_mr,
    casualty_surge_event_count_ci = casualty_surge_event_count_ci,
    casualty_surge_events_summary_mr = casualty_surge_events_summary_mr,
    casualty_surge_timeline_plot_mr = casualty_surge_timeline_plot_mr
  )
}

#' Plot casualties per day, mean and confidence interval across replications
#'
#' @param arrivals Arrival monitor rows with a waiting_time column.
#' @param n_reps See analyse_run().
#' @return A list of `day_range`, `p_casualty_flow_ci`.
plot_casualty_flow_ci <- function(arrivals, n_reps) {
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
  list(
    day_range = day_range,
    p_casualty_flow_ci = p_casualty_flow_ci
  )
}

#' Plot queue depth per echelon, mean and confidence interval across replications
#'
#' @param n_reps See analyse_run().
#' @param resources_raw Resource monitor rows as read.
#' @return A list of `p_r1_queues_ci`, `p_r2b_queues_ci`, `p_r2e_queues_ci`.
plot_queue_depth_ci <- function(n_reps, resources_raw) {
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
              aes(x = bin_min / DAY_MIN, y = queue, group = interaction(replication, resource)),
              color = "steelblue", alpha = 0.06) +
    geom_ribbon(data = r1_bins$ci, aes(x = bin_min / DAY_MIN, ymin = ci_lower, ymax = ci_upper, fill = role_label),
                alpha = 0.3) +
    geom_line(data = r1_bins$ci, aes(x = bin_min / DAY_MIN, y = mean_q, color = role_label), linewidth = 0.9) +
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
              aes(x = bin_min / DAY_MIN, y = queue, group = interaction(replication, resource)),
              color = "steelblue", alpha = 0.06) +
    geom_ribbon(data = r2b_bins$ci, aes(x = bin_min / DAY_MIN, ymin = ci_lower, ymax = ci_upper, fill = bed_label),
                alpha = 0.3) +
    geom_line(data = r2b_bins$ci, aes(x = bin_min / DAY_MIN, y = mean_q, color = bed_label), linewidth = 0.9) +
    facet_wrap(~ r2b_label, ncol = 1, scales = "free_x") +
    labs(title = "R2B Queue Length Over Time — Mean ± 95% CI Across Replications",
         subtitle = sprintf("%d replications; faint traces are individual replications", n_reps),
         x = "Time (Days)", y = "Queue Size", color = "Bed", fill = "Bed") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom", strip.text = element_text(face = "bold"))

  #' Select and label one R2E bed type's monitor rows for the CI plot
  #'
  #' @param resource_type Bed type to select, "ot" or "icu".
  #' @return Resource monitor rows for that bed type, carrying the bed number
  #'   and label columns the plot groups by.
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
              aes(x = bin_min / DAY_MIN, y = queue, group = interaction(replication, resource)),
              color = "steelblue", alpha = 0.06) +
    geom_ribbon(data = r2e_bins$ci,
                aes(x = bin_min / DAY_MIN, ymin = ci_lower, ymax = ci_upper, fill = resource_label), alpha = 0.3) +
    geom_line(data = r2e_bins$ci, aes(x = bin_min / DAY_MIN, y = mean_q, color = resource_label), linewidth = 0.9) +
    facet_wrap(~ resource_type, ncol = 1, scales = "fixed") +
    labs(title = "R2E Heavy Bed Queue Length Over Time — Mean ± 95% CI Across Replications",
         subtitle = sprintf("%d replications; faint traces are individual replications", n_reps),
         x = "Time (Days)", y = "Queue Size", color = "Resource", fill = "Resource") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom", strip.text = element_text(face = "bold"))
  list(
    p_r1_queues_ci = p_r1_queues_ci,
    p_r2b_queues_ci = p_r2b_queues_ci,
    p_r2e_queues_ci = p_r2e_queues_ci
  )
}

#' Plot resource utilisation, mean and confidence interval across replications
#'
#' @param n_reps See analyse_run().
#' @param resources_raw Resource monitor rows as read.
#' @return A list of `p_utilisation_ci`, `util_ci`.
plot_utilisation_ci <- function(n_reps, resources_raw) {
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
  list(
    p_utilisation_ci = p_utilisation_ci,
    util_ci = util_ci
  )
}

#' Plot the pooled waiting-time distribution as a p10 to p90 band
#'
#' @param arrivals_raw Arrival monitor rows as read, before waiting time is derived.
#' @param day_range See analyse_run().
#' @param n_reps See analyse_run().
#' @return The p_waiting_times_ci object.
plot_waiting_times_ci <- function(arrivals_raw, day_range, n_reps) {
  arrivals_wait <- arrivals_raw %>%
    mutate(waiting_time = end_time - start_time - activity_time,
           arrival_day  = floor(start_time / DAY_MIN) + 1) %>%
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
  p_waiting_times_ci
}

#' Plot effective force size, mean and confidence interval across replications
#'
#' @param attributes_raw Attribute monitor rows as read.
#' @param n_reps See analyse_run().
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `force_regeneration_ci`, `p_force_regeneration_ci`.
plot_force_regeneration_ci <- function(attributes_raw, n_reps, output_dir, images_dir) {
  p_force_regeneration_ci  <- NULL
  force_regeneration_ci    <- NULL
  force_keys <- c("effective_force_combat", "effective_force_support")

  if (any(force_keys %in% attributes_raw$key)) {
    force_raw <- attributes_raw %>%
      filter(key %in% force_keys) %>%
      transmute(
        replication, time,
        pool  = recode(key, effective_force_combat = "Combat", effective_force_support = "Support"),
        value = as.numeric(value)
      )

    force_day_bins <- seq(0, max(force_raw$time, na.rm = TRUE), by = DAY_MIN)
    force_binned <- force_raw %>%
      group_by(replication, pool) %>%
      group_modify(function(d, key) {
        v <- approx(d$time, d$value, xout = force_day_bins, method = "constant", rule = 2)$y
        data.frame(bin_day = force_day_bins / DAY_MIN, value = v)
      }) %>%
      ungroup()

    force_regeneration_ci <- force_binned %>%
      group_by(pool, bin_day) %>%
      summarise(n = n(), mean_v = mean(value), sd_v = sd(value), .groups = "drop") %>%
      mutate(
        sd_v     = ifelse(n < 2 | is.na(sd_v), 0, sd_v),
        ci_lower = pmax(mean_v - qt(0.975, df = pmax(n - 1, 1)) * sd_v / sqrt(n), 0),
        ci_upper = mean_v + qt(0.975, df = pmax(n - 1, 1)) * sd_v / sqrt(n)
      )

    force_initial <- data.frame(
      pool  = c("Combat", "Support"),
      value = c(env_data$pops$combat, env_data$pops$support)
    )

    p_force_regeneration_ci <- ggplot() +
      geom_line(data = force_binned,
                aes(x = bin_day, y = value, group = interaction(replication, pool), color = pool),
                alpha = 0.06) +
      geom_ribbon(data = force_regeneration_ci,
                  aes(x = bin_day, ymin = ci_lower, ymax = ci_upper, fill = pool), alpha = 0.3) +
      geom_line(data = force_regeneration_ci, aes(x = bin_day, y = mean_v, color = pool), linewidth = 1) +
      geom_hline(data = force_initial, aes(yintercept = value, color = pool),
                 linetype = "dashed", alpha = 0.5) +
      scale_color_manual(values = c("Combat" = "#2a78d6", "Support" = "#D62828")) +
      scale_fill_manual(values = c("Combat" = "#2a78d6", "Support" = "#D62828")) +
      labs(
        title    = "Effective Force Size Over Time — Mean ± 95% CI Across Replications",
        subtitle = sprintf("%d replications; faint traces are individual replications; dashed = initial establishment strength", n_reps),
        x = "Simulation Day", y = "Effective Force Size", color = "Pool", fill = "Pool"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom")

    ggsave(file.path(images_dir, "force_regeneration_multirun.png"), p_force_regeneration_ci,
           width = 12, height = 6, dpi = 150)
    write.csv(force_regeneration_ci, file.path(output_dir, "force_regeneration_ci.csv"), row.names = FALSE)
  }
  list(
    force_regeneration_ci = force_regeneration_ci,
    p_force_regeneration_ci = p_force_regeneration_ci
  )
}

#' Plot the strategic evacuation queue depth and sortie timeline
#'
#' @param day_range See analyse_run().
#' @param attributes_raw Attribute monitor rows as read.
#' @param resources_raw Resource monitor rows as read.
#' @param output_dir Directory the tables are written to.
#' @param images_dir Directory the plots are written to.
#' @return A list of `ame_backlog_data`, `ame_backlog_plot`, `ame_sortie_data`, `ame_sortie_plot`.
plot_ame_queue_and_sorties_ci <- function(day_range, attributes_raw, resources_raw, output_dir,
                                          images_dir) {
  n_sim_days_ame <- max(day_range)

  ame_backlog_data <- compute_ame_backlog(attributes_raw, n_sim_days_ame)
  ame_backlog_plot <- plot_ame_queue(ame_backlog_data)
  if (!is.null(ame_backlog_plot)) {
    write.csv(ame_backlog_data, file.path(output_dir, "ame_backlog_data.csv"), row.names = FALSE)
    ggsave(file.path(images_dir, "ame_backlog_multirun.png"), ame_backlog_plot, width = 12, height = 8, dpi = 150)
  }

  ame_sortie_data <- NULL
  ame_sortie_plot <- NULL
  if (!is.null(env_data$vars$role4)) {
    ame_sortie_data <- compute_ame_sorties(resources_raw, env_data$vars$role4, n_sim_days_ame)
    ame_sortie_plot <- plot_ame_sortie(ame_sortie_data,
                                       resolve_ame_airframe(env_data$vars$role4)$label)
    if (!is.null(ame_sortie_plot)) {
      write.csv(ame_sortie_data, file.path(output_dir, "ame_sortie_data.csv"), row.names = FALSE)
      ggsave(file.path(images_dir, "ame_sortie_timeline_multirun.png"), ame_sortie_plot, width = 12, height = 8, dpi = 150)
    }
  }

  list(
    ame_backlog_data = ame_backlog_data,
    ame_backlog_plot = ame_backlog_plot,
    ame_sortie_data = ame_sortie_data,
    ame_sortie_plot = ame_sortie_plot
  )
}

#' Runs the Full Analysis (multi-replication) visualisation pipeline
#'
#' @param mon Named list with elements arrivals, attributes, resources as
#'   returned by run_replications() — each carrying a `replication` column
#' @param warm_up_period Days to exclude from the start of the analysis
#'   window (Welch warm-up period; default WARM_UP_DAYS, R/warmup.R — 0 for
#'   this terminating-simulation model, i.e. no exclusion at baseline)
#' @param output_dir Directory path for saving CSV outputs (default "outputs")
#' @param images_dir Directory path for saving PNG plots. Defaults to
#'   `file.path(output_dir, "images")` for the same reason as analyse_run()'s:
#'   the tracked `images/` directory is written only when a
#'   caller names it explicitly.
#' @return Invisibly returns a named list: casualty_flow, r1_queues,
#'   r2b_bed_queues, r2e_bed_queues, utilisation, waiting_times (ggplot
#'   objects, CI-ribbon/error-bar variants of analyse_run()'s single-run
#'   plots — embeddable directly via renderPlot()); kpi_summary (named list
#'   of ci_mean() vectors: total_casualties, dow_count, r2e_icu_peak_queue,
#'   r2b_ot_peak_queue); n_reps; arrivals/attributes/resources (the filtered
#'   monitoring data frames, for CSV export).
#'
#' @details The multi-replication counterpart to analyse_run(): every
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
                                 output_dir = "outputs",
                                 images_dir = file.path(output_dir, "images")) {

  # preamble
  replication_frames_out <- prepare_replication_frames(mon, warm_up_period, output_dir, images_dir)
  arrivals <- replication_frames_out$arrivals
  arrivals_raw <- replication_frames_out$arrivals_raw
  attributes_raw <- replication_frames_out$attributes_raw
  n_reps <- replication_frames_out$n_reps
  resources_raw <- replication_frames_out$resources_raw

  # ── KPI summary cards ─────────────────────────────────────────────────────


  # KPI summary cards
  replication_kpi_cards_out <- summarise_replication_kpi_cards(arrivals_raw, attributes_raw,
                                                               resources_raw)
  clamp_ci <- replication_kpi_cards_out$clamp_ci
  kpi_summary <- replication_kpi_cards_out$kpi_summary

  # ── Per-casualty breakdowns, mean ± 95% CI across replications ───────────
  # Everything below reuses build_attributes_wide()/the same combined join
  # analyse_run() builds, pooling every replication's casualties into one
  # wide pivot — group_by(name, replication) inside build_attributes_wide()
  # already keeps casualties from different replications distinct, so the
  # identical per-casualty logic analyse_run() uses applies unchanged; only
  # the final aggregation step (mean ± 95% CI across replications, via
  # ci_by_group()/ci_mean(), rather than a single-run value) is new. Every
  # group-wise CI table below completes zero-count combinations before
  # aggregating (not after) so a replication with no occurrences of a given
  # group is counted as 0 in the mean rather than silently excluded — the
  # same bias role4_census_daily's original single-run roxygen (above)
  # warns against, generalised here to every new breakdown.

  # Per-casualty breakdowns, mean ± 95% CI across replications
  replication_attributes_out <- build_replication_attributes(arrivals, arrivals_raw, attributes_raw)
  attributes_wide <- replication_attributes_out$attributes_wide
  combined <- replication_attributes_out$combined
  echelon_labels <- replication_attributes_out$echelon_labels
  rep_ids <- replication_attributes_out$rep_ids

  # KPI: DOW count by echelon

  # KPI: DOW count by echelon
  dow_by_echelon_ci <- summarise_dow_by_echelon_ci(attributes_wide, echelon_labels, rep_ids,
                                                   output_dir)

  # KPI: RTD count by type (battle fatigue vs clinical) and by echelon x type

  # KPI: RTD count by type (battle fatigue vs clinical) and by echelon x type
  rtd_by_echelon_ci_out <- summarise_rtd_by_echelon_ci(attributes_wide, clamp_ci, echelon_labels,
                                                       rep_ids, output_dir)
  rtd_by_echelon_ci <- rtd_by_echelon_ci_out$rtd_by_echelon_ci
  rtd_summary_ci <- rtd_by_echelon_ci_out$rtd_summary_ci

  # KPI: R2B routing diagnostics — six counts, mean ± CI

  # KPI: R2B routing diagnostics — six counts, mean ± CI
  r2b_routing_summary_ci <- summarise_r2b_routing_ci(attributes_wide, clamp_ci, rep_ids)

  # R2B Hold Bed Occupancy — mean ± 95% CI ribbon by stream

  # R2B Hold Bed Occupancy — mean ± 95% CI ribbon by stream
  r2b_hold_occupancy_ci_out <- plot_r2b_hold_occupancy_ci(attributes_wide, combined, n_reps,
                                                          rep_ids, output_dir, images_dir)
  r2b_hold_daily_ci <- r2b_hold_occupancy_ci_out$r2b_hold_daily_ci
  r2b_hold_occupancy_plot <- r2b_hold_occupancy_ci_out$r2b_hold_occupancy_plot

  # R2B OT Bypass Reason — mean ± 95% CI per day

  # R2B OT Bypass Reason — mean ± 95% CI per day
  r2b_bypass_reason_plot_full <- plot_r2b_bypass_reason_ci(attributes_wide, combined, n_reps,
                                                           rep_ids, output_dir, images_dir)

  # KPI: R2E post-op pathway (ICU vs Hold) — mean ± CI total/died

  # KPI: R2E post-op pathway (ICU vs Hold) — mean ± CI total/died
  post_op_pathway_ci_out <- summarise_post_op_pathway_ci(attributes_wide, clamp_ci, rep_ids,
                                                         output_dir)
  post_op_pathway_ci <- post_op_pathway_ci_out$post_op_pathway_ci
  surgery_deferred_summary_ci <- post_op_pathway_ci_out$surgery_deferred_summary_ci
  definitive_repair_outstanding_summary_ci <-
    post_op_pathway_ci_out$definitive_repair_outstanding_summary_ci

  # Transport Fleet Capacity Margin — mean ± CI queue by vehicle

  # Transport Fleet Capacity Margin — mean ± CI queue by vehicle
  transport_capacity_margin_ci_out <- plot_transport_capacity_margin_ci(clamp_ci, n_reps,
                                                                        resources_raw, output_dir,
                                                                        images_dir)
  p_transport_capacity_margin_ci <- transport_capacity_margin_ci_out$p_transport_capacity_margin_ci
  transport_utilisation_ci <- transport_capacity_margin_ci_out$transport_utilisation_ci

  # KPI: time-to-treatment KPIs — mean ± CI across replications (per-rep mean pooled)

  # KPI: time-to-treatment KPIs — mean ± CI across replications (per-rep mean pooled)
  dwell_time_summary_ci <- summarise_treatment_intervals_ci(clamp_ci, combined)

  # Role 4 census and unconstrained AME demand — mean ± CI across reps

  # Role 4 census and unconstrained AME demand — mean ± CI across reps
  role4_demand_ci_out <- summarise_role4_demand_ci(clamp_ci, combined, n_reps, rep_ids, output_dir,
                                                   images_dir)
  ame_summary_ci <- role4_demand_ci_out$ame_summary_ci
  role4_census_ci_plot <- role4_demand_ci_out$role4_census_ci_plot
  role4_census_daily_ci <- role4_demand_ci_out$role4_census_daily_ci
  role4_summary_ci <- role4_demand_ci_out$role4_summary_ci

  # Actual AME wait time by route — pooled across all replications

  # Actual AME wait time by route — pooled across all replications
  ame_wait_time_summary_mr <- summarise_ame_wait_by_route(combined, output_dir)

  # Casualty surge event stress test — pooled across replications

  # Casualty surge event stress test — pooled across replications
  casualty_surge_ci_out <- summarise_casualty_surge_ci(clamp_ci, combined, n_reps, rep_ids,
                                                       output_dir, images_dir)
  casualty_surge_dow_summary_mr <- casualty_surge_ci_out$casualty_surge_dow_summary_mr
  casualty_surge_event_count_ci <- casualty_surge_ci_out$casualty_surge_event_count_ci
  casualty_surge_events_summary_mr <- casualty_surge_ci_out$casualty_surge_events_summary_mr
  casualty_surge_timeline_plot_mr <- casualty_surge_ci_out$casualty_surge_timeline_plot_mr

  # ── Casualty Flow — total casualties per day, mean ± CI across reps ──────


  # Casualty Flow — total casualties per day, mean ± CI across reps
  casualty_flow_ci_out <- plot_casualty_flow_ci(arrivals, n_reps)
  day_range <- casualty_flow_ci_out$day_range
  p_casualty_flow_ci <- casualty_flow_ci_out$p_casualty_flow_ci

  # ── Queue depth CI ribbons by echelon ─────────────────────────────────────


  # Queue depth CI ribbons by echelon
  queue_depth_ci_out <- plot_queue_depth_ci(n_reps, resources_raw)
  p_r1_queues_ci <- queue_depth_ci_out$p_r1_queues_ci
  p_r2b_queues_ci <- queue_depth_ci_out$p_r2b_queues_ci
  p_r2e_queues_ci <- queue_depth_ci_out$p_r2e_queues_ci

  # ── Bed & Resource Utilisation — mean ± CI bar chart (no multi-run Gantt) ─


  # Bed & Resource Utilisation — mean ± CI bar chart (no multi-run Gantt)
  utilisation_ci_out <- plot_utilisation_ci(n_reps, resources_raw)
  p_utilisation_ci <- utilisation_ci_out$p_utilisation_ci
  util_ci <- utilisation_ci_out$util_ci

  # ── Waiting Times — p10-p90 quantile band across the pooled replications ─


  # Waiting Times — p10-p90 quantile band across the pooled replications
  p_waiting_times_ci <- plot_waiting_times_ci(arrivals_raw, day_range, n_reps)

  # ── Force regeneration — effective force size, mean ± CI across reps ──────────────
  # Same step-interpolate-onto-a-day-grid approach as bin_queue_ci() above,
  # applied to the two force-size globals instead of resource queues, since
  # get_mon_attributes() records them at irregular event times (each
  # debit/credit), not on a fixed grid.

  # Force regeneration — effective force size, mean ± CI across reps
  force_regeneration_ci_out <- plot_force_regeneration_ci(attributes_raw, n_reps, output_dir,
                                                          images_dir)
  force_regeneration_ci <- force_regeneration_ci_out$force_regeneration_ci
  p_force_regeneration_ci <- force_regeneration_ci_out$p_force_regeneration_ci

  # ── Strategic AME queue depth and sortie timeline ───────────────────────
  # compute_ame_sorties()/plot_ame_sortie() (defined above, ahead of
  # analyse_run()) only depend on the resource monitor and the role4
  # schedule/config parameters — not the per-casualty `combined` join
  # analyse_run() builds — so they apply unchanged to the pooled
  # multi-replication resource data, and already average across
  # replications. compute_ame_backlog() reads `attributes_raw` directly
  # (the long-format monitor, not a wide `combined` join) so it too applies
  # unchanged here; plot_ame_queue() already facets by replication when
  # more than one is present.

  # Strategic AME queue depth and sortie timeline
  ame_queue_and_sorties_ci_out <- plot_ame_queue_and_sorties_ci(day_range, attributes_raw,
                                                                resources_raw, output_dir,
                                                                images_dir)
  ame_backlog_data <- ame_queue_and_sorties_ci_out$ame_backlog_data
  ame_backlog_plot <- ame_queue_and_sorties_ci_out$ame_backlog_plot
  ame_sortie_data <- ame_queue_and_sorties_ci_out$ame_sortie_data
  ame_sortie_plot <- ame_queue_and_sorties_ci_out$ame_sortie_plot
  invisible(list(
    casualty_flow       = p_casualty_flow_ci,
    r1_queues           = p_r1_queues_ci,
    r2b_bed_queues      = p_r2b_queues_ci,
    r2e_bed_queues      = p_r2e_queues_ci,
    utilisation         = p_utilisation_ci,
    utilisation_summary = util_ci,
    waiting_times       = p_waiting_times_ci,
    force_regeneration_plot  = p_force_regeneration_ci,
    force_regeneration_daily = force_regeneration_ci,
    ame_backlog_data    = ame_backlog_data,
    ame_backlog_plot    = ame_backlog_plot,
    ame_sortie_data     = ame_sortie_data,
    ame_sortie_plot     = ame_sortie_plot,
    kpi_summary    = kpi_summary,
    n_reps         = n_reps,
    arrivals       = arrivals_raw,
    attributes     = attributes_raw,
    resources      = resources_raw,

    # ── Mean ± 95% CI equivalents of analyse_run()'s per-casualty
    # breakdowns (see the block above, right after kpi_summary). ─────────
    dow_by_echelon_ci              = dow_by_echelon_ci,
    rtd_summary_ci                 = rtd_summary_ci,
    rtd_by_echelon_ci              = rtd_by_echelon_ci,
    r2b_routing_summary_ci         = r2b_routing_summary_ci,
    r2b_hold_occupancy_plot        = r2b_hold_occupancy_plot,
    r2b_hold_daily_ci              = r2b_hold_daily_ci,
    r2b_bypass_reason_plot         = r2b_bypass_reason_plot_full,
    post_op_pathway_summary_ci     = post_op_pathway_ci,
    surgery_deferred_summary_ci    = surgery_deferred_summary_ci,
    definitive_repair_outstanding_summary_ci = definitive_repair_outstanding_summary_ci,
    transport_capacity_margin_plot = p_transport_capacity_margin_ci,
    transport_utilisation_ci       = transport_utilisation_ci,
    dwell_time_summary_ci          = dwell_time_summary_ci,
    role4_census_plot              = role4_census_ci_plot,
    role4_census_daily_ci          = role4_census_daily_ci,
    role4_summary_ci               = role4_summary_ci,
    ame_summary_ci                 = ame_summary_ci,
    ame_wait_time_summary          = ame_wait_time_summary_mr,
    casualty_surge_timeline_plot    = casualty_surge_timeline_plot_mr,
    casualty_surge_events_summary   = casualty_surge_events_summary_mr,
    casualty_surge_event_count_ci   = casualty_surge_event_count_ci,
    casualty_surge_dow_summary      = casualty_surge_dow_summary_mr
  ))
}
