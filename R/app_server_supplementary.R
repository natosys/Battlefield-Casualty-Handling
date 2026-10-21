##############################################
## R/app_server_supplementary.R           ##
## Server: supplementary outputs          ##
##############################################
#
# The supplementary cards of the Analyse panel: R2B and theatre, dwell
# times, outcomes, Role 4, casualty surge and calibration outputs, with
# their downloads.
#
# Sourced by app.R after the packages and modules it loads; source that
# file rather than this one alone.

#' Build the table and value-card helpers the panels share
#'
#' @return A list of `count_value_card`, `small_dt`.
build_supplementary_card_helpers <- function() {
  #' Render a data frame as a compact, unpaged table
  #'
  #' @param df The data frame to render.
  #' @return A DT datatable with no search box, pager or row names.
  small_dt <- function(df) datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 20))

  #' One value card showing a bare scalar.
  #'
  #' @param label Card heading.
  #' @param n The figure to show.
  #' @return A card holding the figure under its heading.
  count_value_card <- function(label, n) card(card_header(label), div(style = "font-size: 1.4rem; font-weight: 700;", n))
  list(
    count_value_card = count_value_card,
    small_dt = small_dt
  )
}

#' Wire the data downloads for the supplementary plots
#'
#' @param analysis_results See wire_supplementary_outputs().
#' @param output See wire_supplementary_outputs().
#' @return Invisibly NULL; called for the outputs it registers.
wire_supplementary_plot_downloads <- function(analysis_results, output) {
  output$dl_r2b_hold_occupancy_csv <- downloadHandler(
    filename = "r2b_hold_occupancy.csv",
    content  = function(file) {
      df <- analysis_results()$r2b_hold_daily
      write.csv(df, file, row.names = FALSE)
    }
  )
  output$dl_r2b_bypass_reason_csv <- downloadHandler(
    filename = "r2b_bypass_reason.csv",
    content  = function(file) write.csv(analysis_results()$r2b_bypass_reason_plot$data, file, row.names = FALSE)
  )
  output$dl_transport_capacity_margin_csv <- downloadHandler(
    filename = "transport_capacity_margin.csv",
    content  = function(file) write.csv(analysis_results()$transport_capacity_margin_plot$data, file, row.names = FALSE)
  )
  output$dl_role4_census_csv <- downloadHandler(
    filename = "role4_census_daily.csv",
    content  = function(file) {
      df <- analysis_results()$role4_census_daily
      write.csv(df, file, row.names = FALSE)
    }
  )
  output$dl_casualty_surge_timeline_csv <- downloadHandler(
    filename = "casualty_surge_events_summary.csv",
    content  = function(file) write.csv(analysis_results()$casualty_surge_events_summary, file, row.names = FALSE)
  )
  invisible(NULL)
}

#' Wire the R2B routing, pathway and theatre utilisation outputs
#'
#' @param analysis_results See wire_supplementary_outputs().
#' @param output See wire_supplementary_outputs().
#' @param count_value_card See wire_supplementary_outputs().
#' @param small_dt See wire_supplementary_outputs().
#' @return Invisibly NULL; called for the outputs it registers.
wire_r2b_and_theatre_outputs <- function(analysis_results, output, count_value_card, small_dt) {
  output$r2b_routing_cards <- renderUI({
    req(analysis_results())
    res <- analysis_results()
    layout_column_wrap(
      width = "220px",
      count_value_card("Upstream R1-Threshold Bypass", res$r2b_pre_bypass_count),
      count_value_card("At-R2B Hold-Full Bypass", res$r2b_hold_bypass_count),
      count_value_card("R2B Hold Queue (Both Full)", res$r2b_hold_queued_count)
    )
  })

  output$surgery_deferred_card <- renderUI({
    req(analysis_results())
    count_value_card("Surgery Deferred (ICU Saturated, P2+)", analysis_results()$surgery_deferred_count)
  })

  output$post_op_pathway_table <- renderDT({
    df <- analysis_results()$post_op_pathway_summary
    req(df)
    small_dt(df %>% mutate(postop_dow_rate = round(postop_dow_rate, 4)))
  })
  output$dl_post_op_pathway_csv <- downloadHandler(
    filename = "post_op_pathway_summary.csv",
    content  = function(file) {
      df <- analysis_results()$post_op_pathway_summary
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$ot_utilisation_table <- renderDT({
    req(analysis_results()$ot_utilisation)
    small_dt(analysis_results()$ot_utilisation %>% mutate(utilisation = round(utilisation, 4)))
  })
  output$dl_ot_utilisation_csv <- downloadHandler(
    filename = "ot_utilisation.csv",
    content  = function(file) write.csv(analysis_results()$ot_utilisation, file, row.names = FALSE)
  )

  output$transport_utilisation_table <- renderDT({
    df <- analysis_results()$transport_utilisation
    req(df)
    small_dt(df %>% mutate(utilisation = round(utilisation, 4)))
  })
  output$dl_transport_utilisation_csv <- downloadHandler(
    filename = "transport_utilisation.csv",
    content  = function(file) {
      df <- analysis_results()$transport_utilisation
      write.csv(df, file, row.names = FALSE)
    }
  )
  invisible(NULL)
}

#' Wire the time-to-treatment table and its download
#'
#' @param analysis_results See wire_supplementary_outputs().
#' @param output See wire_supplementary_outputs().
#' @param small_dt See wire_supplementary_outputs().
#' @return Invisibly NULL; called for the outputs it registers.
wire_dwell_time_outputs <- function(analysis_results, output, small_dt) {
  output$dwell_time_table <- renderDT({
    res <- analysis_results()
    req(res$time_to_first_surgery, res$r2b_dwell_time, res$r2b_r2e_transit_time, res$r2e_dwell_time)
    # Censored_pct is the share of the cohort still in the echelon when the
    # run ended. It is shown rather than left to the CSV because the mean is
    # a restricted mean over that whole cohort, and a reader who cannot see
    # how much of it was censored cannot tell how far the figure reaches.
    # Time to first surgery has no identifiable censored cohort, so it shows
    # nothing here and reports its no-surgery-time count in the CSV instead.
    df <- bind_rows(
      data.frame(KPI = "Time to First Surgery", Mean_min = res$time_to_first_surgery$mean_min,
                 P10_min = res$time_to_first_surgery$p10_min, P90_min = res$time_to_first_surgery$p90_min,
                 N = res$time_to_first_surgery$n, Censored_pct = NA_real_),
      data.frame(KPI = "R2B Dwell Time", Mean_min = res$r2b_dwell_time$mean_min,
                 P10_min = NA_real_, P90_min = res$r2b_dwell_time$p90_min, N = res$r2b_dwell_time$n,
                 Censored_pct = 100 * res$r2b_dwell_time$censored_share),
      data.frame(KPI = "R2B to R2E Transit Time", Mean_min = res$r2b_r2e_transit_time$mean_min,
                 P10_min = NA_real_, P90_min = res$r2b_r2e_transit_time$p90_min,
                 N = res$r2b_r2e_transit_time$n,
                 Censored_pct = 100 * res$r2b_r2e_transit_time$censored_share),
      data.frame(KPI = "R2E Dwell Time", Mean_min = res$r2e_dwell_time$mean_min,
                 P10_min = NA_real_, P90_min = res$r2e_dwell_time$p90_min, N = res$r2e_dwell_time$n,
                 Censored_pct = 100 * res$r2e_dwell_time$censored_share)
    ) %>% mutate(across(c(Mean_min, P10_min, P90_min, Censored_pct), ~ round(., 1)))
    small_dt(df)
  })
  output$dl_dwell_time_csv <- downloadHandler(
    filename = "dwell_times.csv",
    content  = function(file) {
      res <- analysis_results()
      write.csv(bind_rows(
        data.frame(kpi = "time_to_first_surgery", res$time_to_first_surgery),
        data.frame(kpi = "r2b_dwell_time", res$r2b_dwell_time),
        data.frame(kpi = "r2b_r2e_transit_time", res$r2b_r2e_transit_time),
        data.frame(kpi = "r2e_dwell_time", res$r2e_dwell_time)
      ), file, row.names = FALSE)
    }
  )
  invisible(NULL)
}

#' Wire the return-to-duty and died-of-wounds outputs
#'
#' @param analysis_results See wire_supplementary_outputs().
#' @param output See wire_supplementary_outputs().
#' @param count_value_card See wire_supplementary_outputs().
#' @param small_dt See wire_supplementary_outputs().
#' @return Invisibly NULL; called for the outputs it registers.
wire_outcome_outputs <- function(analysis_results, output, count_value_card, small_dt) {
  output$rtd_dow_cards <- renderUI({
    req(analysis_results())
    res <- analysis_results()
    layout_column_wrap(
      width = "220px",
      count_value_card("Battle Fatigue RTD", res$bf_rtd),
      count_value_card("Clinical RTD", res$clinical_rtd),
      count_value_card("Total RTD", res$total_rtd)
    )
  })

  output$dow_by_echelon_table <- renderDT({
    df <- analysis_results()$dow_by_echelon
    req(df)
    small_dt(df %>% mutate(dow_rate = round(dow_rate, 4)))
  })
  output$dl_dow_by_echelon_csv <- downloadHandler(
    filename = "dow_by_echelon.csv",
    content  = function(file) {
      df <- analysis_results()$dow_by_echelon
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$rtd_by_echelon_table <- renderDT({
    df <- analysis_results()$rtd_by_echelon
    req(df)
    small_dt(df %>% mutate(rtd_rate = round(rtd_rate, 4)))
  })
  output$dl_rtd_by_echelon_csv <- downloadHandler(
    filename = "rtd_by_echelon.csv",
    content  = function(file) {
      df <- analysis_results()$rtd_by_echelon
      write.csv(df, file, row.names = FALSE)
    }
  )
  invisible(NULL)
}

#' Wire the Role 4 and strategic evacuation outputs
#'
#' @param analysis_results See wire_supplementary_outputs().
#' @param output See wire_supplementary_outputs().
#' @param small_dt See wire_supplementary_outputs().
#' @return Invisibly NULL; called for the outputs it registers.
wire_role4_outputs <- function(analysis_results, output, small_dt) {
  output$role4_summary_cards <- renderUI({
    req(analysis_results())
    req(analysis_results()$role4_summary)
    s <- analysis_results()$role4_summary
    #' Render one single-run value card
    #'
    #' @param label Card heading.
    #' @param v The figure to show.
    #' @param digits Decimal places shown.
    #' @return A card holding the figure under its heading.
    val_card <- function(label, v, digits = 1) {
      card(card_header(label), div(style = "font-size: 1.4rem; font-weight: 700;", formatC(v, format = "f", digits = digits)))
    }
    layout_column_wrap(
      width = "220px",
      val_card("Total Evacuated to Role 4", s$total_evacuated),
      val_card("Peak Occupancy", s$peak_occupancy),
      val_card("Peak Occupancy Day", s$peak_day, digits = 0)
    )
  })

  output$ame_demand_cards <- renderUI({
    req(analysis_results())
    req(analysis_results()$ame_summary)
    s <- analysis_results()$ame_summary
    #' Render one single-run value card
    #'
    #' @param label Card heading.
    #' @param v The figure to show.
    #' @param digits Decimal places shown.
    #' @return A card holding the figure under its heading.
    val_card <- function(label, v, digits = 1) {
      card(card_header(label), div(style = "font-size: 1.4rem; font-weight: 700;", formatC(v, format = "f", digits = digits)))
    }
    layout_column_wrap(
      width = "220px",
      val_card("Total Sorties Required", s$total_sorties, digits = 0),
      val_card("Peak Daily Sorties", s$peak_daily_sorties, digits = 0),
      val_card("Mean Daily Sorties", s$mean_daily_sorties)
    )
  })

  output$ame_wait_time_table <- renderDT({
    req(analysis_results()$ame_wait_time_summary)
    df <- analysis_results()$ame_wait_time_summary %>%
      mutate(across(c(mean_wait_minutes, p10_wait_minutes, p90_wait_minutes),
                     list(days = ~ round(. / DAY_MIN, 1))))
    small_dt(df)
  })
  output$dl_ame_wait_time_csv <- downloadHandler(
    filename = "ame_wait_time_summary.csv",
    content  = function(file) write.csv(analysis_results()$ame_wait_time_summary, file, row.names = FALSE)
  )
  invisible(NULL)
}

#' Wire the casualty surge event outputs
#'
#' @param analysis_results See wire_supplementary_outputs().
#' @param output See wire_supplementary_outputs().
#' @param count_value_card See wire_supplementary_outputs().
#' @param small_dt See wire_supplementary_outputs().
#' @return Invisibly NULL; called for the outputs it registers.
wire_casualty_surge_outputs <- function(analysis_results, output, count_value_card, small_dt) {
  output$casualty_surge_event_count_card <- renderUI({
    req(analysis_results())
    count_value_card("Casualty Surge Events", analysis_results()$casualty_surge_event_count)
  })

  output$casualty_surge_events_table <- renderDT({
    req(analysis_results()$casualty_surge_events_summary)
    small_dt(analysis_results()$casualty_surge_events_summary)
  })

  output$casualty_surge_dow_table <- renderDT({
    req(analysis_results()$casualty_surge_dow_summary)
    small_dt(analysis_results()$casualty_surge_dow_summary %>% mutate(dow_rate = round(dow_rate, 4)))
  })
  output$dl_casualty_surge_dow_csv <- downloadHandler(
    filename = "casualty_surge_dow_summary.csv",
    content  = function(file) write.csv(analysis_results()$casualty_surge_dow_summary, file, row.names = FALSE)
  )
  invisible(NULL)
}

#' Wire the sensitivity calibration table and its download
#'
#' @param output See wire_supplementary_outputs().
#' @return `calibration_df`.
wire_calibration_outputs <- function(output) {
  calibration_df <- reactive({
    data.frame(
      Variable      = morris_params$name,
      Parameter     = MORRIS_LABELS[morris_params$name],
      Category      = MORRIS_CATEGORY_LABELS[morris_params$category],
      `Lower Bound` = morris_params$lower,
      `Baseline`    = morris_params$mode,
      `Upper Bound` = morris_params$upper,
      check.names   = FALSE
    )
  })
  # `Variable` (the raw morris_params$name / MORRIS_LABELS key used in
  # outputs/morris_ranking.csv and the axis labels on every saved
  # images/morris_*.png scatter) is included alongside the plain-English
  # `Parameter` title specifically so this table doubles as the variable
  # name -> human-readable title lookup a planner needs when cross-
  # referencing a raw code seen in the CSV or plots back to what it means
  # — not just a slider-bounds reference.
  output$calibration_table <- renderDT({
    datatable(calibration_df(), rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })
  output$dl_calibration_csv <- downloadHandler(
    filename = "sensitivity_calibration.csv",
    content  = function(file) write.csv(calibration_df(), file, row.names = FALSE)
  )
  calibration_df
}

#' Wire the supplementary analysis outputs and their downloads
#'
#' @param analysis_results See analyse_run().
#' @param output See analyse_run().
#' @return A list of `calibration_df`, `count_value_card`, `small_dt`.
wire_supplementary_outputs <- function(analysis_results, output) {
  supplementary_card_helpers_out <- build_supplementary_card_helpers()
  count_value_card <- supplementary_card_helpers_out$count_value_card
  small_dt <- supplementary_card_helpers_out$small_dt

  wire_supplementary_plot_downloads(analysis_results, output)

  # R2B routing diagnostics — pre-transport and at-R2B-hold
  # bypass/queue counts; independent, non-summing counts (see Annex A of
  # docs/Results.md).
  wire_r2b_and_theatre_outputs(analysis_results, output, count_value_card,
                               small_dt)

  # Time-to-treatment KPIs (Output Variable Register) — combined into one
  # table; not every KPI has a p10 (only time_to_first_surgery does).
  wire_dwell_time_outputs(analysis_results, output, small_dt)

  wire_outcome_outputs(analysis_results, output, count_value_card,
                       small_dt)

  wire_role4_outputs(analysis_results, output, small_dt)

  wire_casualty_surge_outputs(analysis_results, output, count_value_card,
                              small_dt)

  calibration_df <- wire_calibration_outputs(output)
  list(
    calibration_df = calibration_df,
    count_value_card = count_value_card,
    small_dt = small_dt
  )
}
