##############################################
## R/analysis_single_run.R                  ##
## Single-run analysis pipeline             ##
##############################################
#
# analyse_run(), the orchestrator of one run's analysis, and
# prepare_run_frames(), the frames every stage reads. The stages themselves,
# each producing one domain's summaries, tables or figures, are in
# R/analysis_run_r1_r2b.R, R/analysis_run_r2e.R, R/analysis_run_flow.R and
# R/analysis_run_evacuation.R.
#
# Sourced by R/analysis.R, which loads the packages and the shared modules
# these functions use; source that file rather than this one alone.

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
