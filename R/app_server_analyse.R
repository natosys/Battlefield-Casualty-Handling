##############################################
## R/app_server_analyse.R                 ##
## Server: Analyse panel                  ##
##############################################
#
# The Analyse panel: its summary outputs, one function per result tab, the
# plot registry and the image and data downloads.
#
# Sourced by app.R after the packages and modules it loads; source that
# file rather than this one alone.

#' Wire the Analyse panel's plot selection and sizing
#'
#' @param analysis_results See analyse_run().
#' @return A list of `gantt_min_section_height_px`, `gantt_row_height_px`, `tab_plot`,
#'   `utilisation_panel_heights`.
wire_analyse_panel <- function(analysis_results) {
  tab_plot <- reactive({
    res <- analysis_results()
    req(res)
    list(
      casualty_flow       = res$casualty_flow,
      waiting_times       = res$waiting_times,
      force_regeneration  = res$force_regeneration_plot,
      ame_backlog         = res$ame_backlog_plot,
      ame_sortie          = res$ame_sortie_plot
    )
  })

  # A fixed height for the Bed & Resource Utilisation plot overlaps its Gantt
  # rows once an echelon's individual bed count grows (R2E routinely plots 20+
  # beds). Each Gantt's rendered height therefore scales with its own distinct
  # resource row count, at a fixed per-row convention
  # (25px, ~150px floor per section so an empty/small section isn't squashed
  # to nothing).
  gantt_row_height_px <- 25
  gantt_min_section_height_px <- 150

  # Natural height for each of Quick Run's 4 split Bed & Resource Utilisation
  # panels (r2b_treatment, r2b_gantt, r2e_surgery, r2e_gantt) — each panel's
  # own shrink_to_fit_plot_ui()/new_shrink_to_fit_plot() render height.
  utilisation_panel_heights <- reactive({
    res <- analysis_results()
    req(res)

    r2b_data <- res$r2b_gantt$data
    r2b_gantt_height <- if (is.null(r2b_data) || nrow(r2b_data) == 0) {
      gantt_min_section_height_px
    } else {
      rows_per_team <- tapply(r2b_data$resource_label, r2b_data$r2b_team,
                               function(x) length(unique(x)))
      sum(pmax(rows_per_team * gantt_row_height_px, gantt_min_section_height_px))
    }

    r2e_data <- res$r2e_gantt$data
    r2e_rows <- if (is.null(r2e_data)) 0 else length(unique(r2e_data$resource_label))
    r2e_gantt_height <- max(r2e_rows * gantt_row_height_px, gantt_min_section_height_px)

    # r2b_treatment is itself a 3-panel patchwork stack (casualties treated /
    # surgeries started / skipping R2B), like Casualty Flow's 3-panel stack —
    # so it gets the same generous height as that tab (700px), rather than
    # r2e_surgery's plain single-panel bar chart height (400px); 400px for
    # three stacked panels left too little room per panel, causing each
    # sub-panel's "Casualties" y-axis title to overlap its neighbour's.
    list(r2b_treatment = 700, r2b_gantt = r2b_gantt_height,
         r2e_surgery   = 400, r2e_gantt = r2e_gantt_height,
         r2e_icu_gating = 500, r2b_hold_occupancy = 450, r2b_bypass_reason = 450)
  })
  list(
    gantt_min_section_height_px = gantt_min_section_height_px,
    gantt_row_height_px = gantt_row_height_px,
    tab_plot = tab_plot,
    utilisation_panel_heights = utilisation_panel_heights
  )
}

#' Build the shrink-to-fit plot container and registration helpers
#'
#' @param input See wire_analyse_outputs().
#' @param output See wire_analyse_outputs().
#' @return A list of `ANALYSE_PLOT_CHROME_PX`, `ANALYSE_PLOT_CHROME_WITH_HEADING_PX`,
#'   `ANALYSE_PLOT_CHROME_WITH_INTRO_PX`, `new_shrink_to_fit_plot`, `shrink_to_fit_plot_ui`.
build_shrink_to_fit_helpers <- function(input, output) {
  ANALYSE_PLOT_CHROME_PX             <- 260
  ANALYSE_PLOT_CHROME_WITH_INTRO_PX  <- 360
  # Split-out panels (Queue Depths' 3; Bed & Resource Utilisation's 4 Quick
  # Run panels) each carry a small heading + an `hr` separator above them,
  # rather than a solo plot's bare tab content or a full paragraph intro —
  # an allowance between the other two.
  ANALYSE_PLOT_CHROME_WITH_HEADING_PX <- 300

  #' UI for one shrink-to-fit Analyse-tab plot: a container whose CSS
  #' height is managed client-side by shrink_to_fit_script(), with the
  #' plot image inside tracking that height (`bch_shrink_to_fit_css()`)
  #' rather than being rendered at a different resolution, plus an
  #' "Expand" link that opens the same plot at full natural size in a
  #' modal (wired server-side by new_shrink_to_fit_plot()).
  #'
  #' @param plot_id Output id the plot is/will be rendered to.
  #' @param natural_height_px Data-driven or fixed pixel height the plot
  #'   renders at server-side (unchanged by shrinking — see
  #'   new_shrink_to_fit_plot()) and the container's initial CSS height,
  #'   before shrink_to_fit_script() adjusts it.
  #' @param chrome_px Vertical space assumed to be consumed by chrome
  #'   surrounding this plot; see roxygen above.
  #' @return A tag list of the sized container and its "Expand" link.
  shrink_to_fit_plot_ui <- function(plot_id, natural_height_px, chrome_px = ANALYSE_PLOT_CHROME_PX) {
    natural_height_px <- round(natural_height_px)
    tagList(
      div(class = "bch-shrink-fit",
          style = paste0("height:", natural_height_px, "px;"),
          `data-natural-height` = natural_height_px,
          `data-chrome-px`      = chrome_px,
          plotOutput(plot_id, height = "100%")),
      div(class = "mt-1",
          actionLink(paste0(plot_id, "_expand"), HTML("&#128269; Expand to full size")))
    )
  }

  #' Server wiring for one shrink-to-fit plot: the (shrinkable) primary
  #' output, a full-size modal companion bound to the same plot content,
  #' and the "Expand" link's click handler. Both outputs share `plot_fn`
  #' and both explicitly force `height_fn()` as their render height (never
  #' "auto") — see shrink_to_fit_script()'s roxygen for why an explicit,
  #' un-auto-detected height is what makes the client-side shrinking safe.
  #' The modal always shows the plot at this same full natural size,
  #' unscaled by any container CSS, making it the regression check that the
  #' computed row/label spacing survives the shrinking.
  #'
  #' @param plot_id Output id (matches shrink_to_fit_plot_ui()'s plot_id).
  #' @param title Modal dialog title.
  #' @param plot_fn Zero-arg function returning the ggplot/patchwork object.
  #' @param height_fn Zero-arg function returning the natural pixel height
  #'   (may itself read a reactive, e.g. utilisation_plot_height()).
  #' @return Invisibly NULL; called for the two outputs and the click
  #'   handler it registers.
  new_shrink_to_fit_plot <- function(plot_id, title, plot_fn, height_fn) {
    output[[plot_id]] <- renderPlot({ plot_fn() }, height = function() height_fn())
    output[[paste0(plot_id, "_modal")]] <- renderPlot({ plot_fn() }, height = function() height_fn())
    observeEvent(input[[paste0(plot_id, "_expand")]], {
      showModal(modalDialog(
        title = title, size = "xl", easyClose = TRUE, footer = modalButton("Close"),
        plotOutput(paste0(plot_id, "_modal"), height = paste0(round(isolate(height_fn())), "px"))
      ))
    }, ignoreInit = TRUE)
  }
  list(
    ANALYSE_PLOT_CHROME_PX = ANALYSE_PLOT_CHROME_PX,
    ANALYSE_PLOT_CHROME_WITH_HEADING_PX = ANALYSE_PLOT_CHROME_WITH_HEADING_PX,
    ANALYSE_PLOT_CHROME_WITH_INTRO_PX = ANALYSE_PLOT_CHROME_WITH_INTRO_PX,
    new_shrink_to_fit_plot = new_shrink_to_fit_plot,
    shrink_to_fit_plot_ui = shrink_to_fit_plot_ui
  )
}

#' Wire the whole-run download
#'
#' @param mon_data See wire_analyse_outputs().
#' @param output See wire_analyse_outputs().
#' @return Invisibly NULL; called for the outputs it registers.
wire_analyse_summary_outputs <- function(mon_data, output) {
  # Download All — zip of the three monitoring CSVs from the most recent run
  # (Quick Run or Full Analysis), regardless of which produced mon_data()).
  output$download_all_ui <- renderUI({
    req(mon_data())
    downloadButton("dl_all_zip", "Download All (mon_*.csv, ZIP)", class = "btn-outline-secondary")
  })
  output$dl_all_zip <- downloadHandler(
    filename = function() sprintf("bch_monitoring_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      mon <- mon_data()
      req(mon)
      tmp_dir <- tempfile("bch_dl_all_")
      dir.create(tmp_dir)
      write.csv(mon$arrivals,   file.path(tmp_dir, "mon_arrivals.csv"),   row.names = FALSE)
      write.csv(mon$attributes, file.path(tmp_dir, "mon_attributes.csv"), row.names = FALSE)
      write.csv(mon$resources,  file.path(tmp_dir, "mon_resources.csv"),  row.names = FALSE)
      old_wd <- setwd(tmp_dir)
      on.exit(setwd(old_wd), add = TRUE)
      zip(file, files = c("mon_arrivals.csv", "mon_attributes.csv", "mon_resources.csv"))
    },
    contentType = "application/zip"
  )
  invisible(NULL)
}

#' Build the Analyse panel's Casualty Flow tab
#'
#' The panel shows casualties per day by type, source and priority.
#'
#' @param shrink_to_fit_plot_ui See wire_analyse_body().
#' @param ANALYSE_PLOT_CHROME_WITH_INTRO_PX See wire_analyse_body().
#' @return A `nav_panel()` for the Analyse panel's tab set.
analyse_tab_casualty_flow <- function(shrink_to_fit_plot_ui, ANALYSE_PLOT_CHROME_WITH_INTRO_PX) {
    nav_panel("Casualty Flow",
      p(class = "text-muted mt-2",
        "Total casualties per simulation day, stacked by type, population source, and triage ",
        "priority (top to bottom) — check this first to confirm the casualty-generation rates you ",
        "configured are producing the daily intensity and mix you expect, since this demand drives ",
        "every queue and utilisation result elsewhere on this page."),
      shrink_to_fit_plot_ui("plot_casualty_flow", 700, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
      downloadButton("dl_casualty_flow_png", "Download PNG"),
      downloadButton("dl_casualty_flow_pdf", "Download PDF"),
      downloadButton("dl_casualty_flow_csv", "Download Data (CSV)")
    )
}

#' Build the Analyse panel's Queue Depths tab
#'
#' The panel shows queue length over time at each echelon.
#'
#' @param shrink_to_fit_plot_ui See wire_analyse_body().
#' @param ANALYSE_PLOT_CHROME_WITH_INTRO_PX See wire_analyse_body().
#' @return A `nav_panel()` for the Analyse panel's tab set.
analyse_tab_queue_depths <- function(shrink_to_fit_plot_ui, ANALYSE_PLOT_CHROME_WITH_INTRO_PX) {
    nav_panel("Queue Depths",
      h6(class = "text-muted mt-2", "R1 Queues"),
      p(class = "text-muted small",
        "Queue length over time for each R1 team's treatment slots, faceted by team. A queue above ",
        "0 means casualties are waiting; a queue that persists or grows over the run suggests that ",
        "team is under-resourced for its casualty inflow."),
      shrink_to_fit_plot_ui("plot_r1_queues", 600, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
      downloadButton("dl_r1_queues_png", "Download PNG"),
      downloadButton("dl_r1_queues_pdf", "Download PDF"),
      downloadButton("dl_r1_queues_csv", "Download Data (CSV)"),
      tags$hr(),
      h6(class = "text-muted mt-2", "R2B Bed Queues"),
      p(class = "text-muted small",
        "Queue length over time for each R2B team's beds, faceted by team and coloured by bed type — ",
        "a bed type with a sustained queue is a capacity bottleneck worth increasing."),
      shrink_to_fit_plot_ui("plot_r2b_bed_queues", 600, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
      downloadButton("dl_r2b_bed_queues_png", "Download PNG"),
      downloadButton("dl_r2b_bed_queues_pdf", "Download PDF"),
      downloadButton("dl_r2b_bed_queues_csv", "Download Data (CSV)"),
      tags$hr(),
      h6(class = "text-muted mt-2", "R2E Bed Queues"),
      p(class = "text-muted small",
        "Queue length over time for R2E's OT and ICU beds, faceted by resource type — a sustained ",
        "queue on either identifies which resource is constraining R2E throughput."),
      shrink_to_fit_plot_ui("plot_r2e_bed_queues", 600, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
      downloadButton("dl_r2e_bed_queues_png", "Download PNG"),
      downloadButton("dl_r2e_bed_queues_pdf", "Download PDF"),
      downloadButton("dl_r2e_bed_queues_csv", "Download Data (CSV)")
    )
}

#' Build the R2E half of the Quick Run utilisation tab
#'
#' Split from the R2B half only by length; the two are read together and
#' the tab renders them one after the other.
#'
#' @param ph Per-panel render heights, as `utilisation_panel_heights()` returns
#'   them; computed once by the R2B half and passed on rather than recomputed.
#' @param analysis_results See analyse_tab_utilisation().
#' @param shrink_to_fit_plot_ui See analyse_tab_utilisation().
#' @param ANALYSE_PLOT_CHROME_WITH_INTRO_PX See analyse_tab_utilisation().
#' @param ANALYSE_PLOT_CHROME_WITH_HEADING_PX See analyse_tab_utilisation().
#' @return A `tagList()` of the tab's R2E content.
analyse_tab_utilisation_quick_r2e <- function(ph, analysis_results, shrink_to_fit_plot_ui,
                                              ANALYSE_PLOT_CHROME_WITH_INTRO_PX,
                                              ANALYSE_PLOT_CHROME_WITH_HEADING_PX) {
  tagList(
    h6(class = "text-muted mt-2", "R2E Surgery"),
    p(class = "text-muted small",
      "Number of R2E surgeries completed per simulation day — compare against the Gantt chart ",
      "below to see whether surgical throughput is keeping pace with casualty arrivals."),
    shrink_to_fit_plot_ui("plot_r2e_surgery", ph$r2e_surgery, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
    downloadButton("dl_r2e_surgery_png", "Download PNG"),
    downloadButton("dl_r2e_surgery_pdf", "Download PDF"),
    downloadButton("dl_r2e_surgery_csv", "Download Data (CSV)"),
    tags$hr(),
    h6(class = "text-muted mt-2", "R2E Bed Resource Usage (Gantt)"),
    p(class = "text-muted small",
      "Each horizontal bar is one bed's occupied time window, faceted by team. A bed with few ",
      "or no gaps is running near-continuously and is a candidate for added capacity; long ",
      "gaps mean spare capacity."),
    shrink_to_fit_plot_ui("plot_r2e_gantt", ph$r2e_gantt, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
    downloadButton("dl_r2e_gantt_png", "Download PNG"),
    downloadButton("dl_r2e_gantt_pdf", "Download PDF"),
    downloadButton("dl_r2e_gantt_csv", "Download Data (CSV)"),
    tags$hr(),
    h6(class = "text-muted mt-2", "R2E OT-ICU Gating — Hold Bed Used in Lieu of ICU"),
    p(class = "text-muted small",
      "Daily breakdown of R2E post-operative casualties: Sub-Optimal (red) recovered in a Hold bed ",
      "instead of ICU because ICU was full at OT entry; Delayed (orange) had OT entry deferred pending ",
      "an ICU bed; Normal (green) means ICU was available. A meaningful Sub-Optimal or Delayed ",
      "share signals R2E ICU capacity is undersized for the casualty load."),
    shrink_to_fit_plot_ui("plot_r2e_icu_gating", ph$r2e_icu_gating, chrome_px = ANALYSE_PLOT_CHROME_WITH_HEADING_PX),
    downloadButton("dl_r2e_icu_gating_png", "Download PNG"),
    downloadButton("dl_r2e_icu_gating_pdf", "Download PDF"),
    downloadButton("dl_r2e_icu_gating_csv", "Download Data (CSV)"),
    tags$hr(),
    h6(class = "text-muted mt-2", "R2E Post-Operative Pathway — ICU vs Hold Bed"),
    p(class = "text-muted small",
      "Casualty counts and post-operative DOW rate by recovery pathway: icu = nominal ",
      "ICU recovery; hold = ICU was saturated at OT entry, so a Priority 1 casualty recovered in a ",
      "hold bed instead (see R2E OT-ICU Gating above). A materially higher DOW rate on hold than ",
      "icu is the clinical cost of that ICU saturation."),
    if (is.null(analysis_results()$post_op_pathway_summary)) {
      div(class = "alert alert-secondary", "No R2E surgeries completed this run.")
    } else DTOutput("post_op_pathway_table"),
    downloadButton("dl_post_op_pathway_csv", "Download Data (CSV)"),
    div(class = "mt-2", uiOutput("surgery_deferred_card")),
    tags$hr(),
    h6(class = "text-muted mt-2", "Operating Theatre Utilisation"),
    p(class = "text-muted small",
      "Busy-time fraction of OT capacity by echelon (busy time / (capacity × observation window)) — ",
      "a figure near 100% signals that echelon's OT capacity is a binding constraint."),
    DTOutput("ot_utilisation_table"),
    downloadButton("dl_ot_utilisation_csv", "Download Data (CSV)")
  )
}

#' Build the Quick Run arm of the Analyse panel's utilisation tab
#'
#' A single replication has no confidence interval to plot, so it gets the
#' per-bed Gantt charts and their own data-driven heights instead of the
#' mean-and-interval bar chart Full Analysis shows.
#'
#' @param analysis_results See analyse_tab_utilisation().
#' @param shrink_to_fit_plot_ui See analyse_tab_utilisation().
#' @param ANALYSE_PLOT_CHROME_WITH_INTRO_PX See analyse_tab_utilisation().
#' @param ANALYSE_PLOT_CHROME_WITH_HEADING_PX See analyse_tab_utilisation().
#' @param utilisation_panel_heights See analyse_tab_utilisation().
#' @return A `tagList()` of the tab's Quick Run content.
analyse_tab_utilisation_quick <- function(analysis_results, shrink_to_fit_plot_ui,
                                          ANALYSE_PLOT_CHROME_WITH_INTRO_PX,
                                          ANALYSE_PLOT_CHROME_WITH_HEADING_PX,
                                          utilisation_panel_heights) {
  ph <- utilisation_panel_heights()
  tagList(
    h6(class = "text-muted mt-2", "R2B Treatment"),
    p(class = "text-muted small",
      "Three daily counts, top to bottom: casualties treated at each R2B station, R2B ",
      "surgeries started, and casualties who skipped R2B entirely. A rising skip count ",
      "alongside flat treatment counts suggests R2B capacity, not casualty demand, is the ",
      "limiting factor."),
    shrink_to_fit_plot_ui("plot_r2b_treatment", ph$r2b_treatment, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
    downloadButton("dl_r2b_treatment_png", "Download PNG"),
    downloadButton("dl_r2b_treatment_pdf", "Download PDF"),
    downloadButton("dl_r2b_treatment_csv", "Download Data (CSV)"),
    tags$hr(),
    h6(class = "text-muted mt-2", "R2B Bed Resource Usage (Gantt)"),
    p(class = "text-muted small",
      "Each horizontal bar is one bed's occupied time window, faceted by team. A bed with few ",
      "or no gaps is running near-continuously and is a candidate for added capacity; long ",
      "gaps mean spare capacity."),
    shrink_to_fit_plot_ui("plot_r2b_gantt", ph$r2b_gantt, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
    downloadButton("dl_r2b_gantt_png", "Download PNG"),
    downloadButton("dl_r2b_gantt_pdf", "Download PDF"),
    downloadButton("dl_r2b_gantt_csv", "Download Data (CSV)"),
    tags$hr(),
    h6(class = "text-muted mt-2", "R2B Hold Bed Occupancy by Patient Stream"),
    p(class = "text-muted small",
      "Daily concurrent occupancy of R2B hold beds, decomposed by patient stream ",
      "(disease DNBI, NBI, WIA). Compare the stacked total against the dashed per-unit ",
      "capacity line to see which days risk saturating hold beds, and which stream drives it. ",
      "No plot means no casualty occupied an R2B hold bed this run."),
    if (is.null(analysis_results()$r2b_hold_occupancy_plot)) {
      div(class = "alert alert-secondary", "No R2B hold bed occupancy recorded this run.")
    } else tagList(
      shrink_to_fit_plot_ui("plot_r2b_hold_occupancy", ph$r2b_hold_occupancy, chrome_px = ANALYSE_PLOT_CHROME_WITH_HEADING_PX),
      downloadButton("dl_r2b_hold_occupancy_png", "Download PNG"),
      downloadButton("dl_r2b_hold_occupancy_pdf", "Download PDF"),
      downloadButton("dl_r2b_hold_occupancy_csv", "Download Data (CSV)")
    ),
    uiOutput("r2b_routing_cards"),
    tags$hr(),
    h6(class = "text-muted mt-2", "R2B OT Bypass Reason"),
    p(class = "text-muted small",
      "Why an R2B-capable casualty was instead routed straight to R2E at the surgical decision ",
      "point: the R2B surgical team was off-shift, or the OT bed was busy/queued. A high bypass ",
      "count suggests R2B surgical shift coverage or OT capacity is undersized for demand."),
    if (is.null(analysis_results()$r2b_bypass_reason_plot)) {
      div(class = "alert alert-secondary", "No R2B OT bypasses recorded this run.")
    } else tagList(
      shrink_to_fit_plot_ui("plot_r2b_bypass_reason", ph$r2b_bypass_reason, chrome_px = ANALYSE_PLOT_CHROME_WITH_HEADING_PX),
      downloadButton("dl_r2b_bypass_reason_png", "Download PNG"),
      downloadButton("dl_r2b_bypass_reason_pdf", "Download PDF"),
      downloadButton("dl_r2b_bypass_reason_csv", "Download Data (CSV)")
    ),
    tags$hr(),
    analyse_tab_utilisation_quick_r2e(ph, analysis_results, shrink_to_fit_plot_ui,
                                      ANALYSE_PLOT_CHROME_WITH_INTRO_PX,
                                      ANALYSE_PLOT_CHROME_WITH_HEADING_PX)
  )
}

#' Build the Analyse panel's Bed & Resource Utilisation tab
#'
#' The panel shows bed and resource occupancy.
#'
#' @param analysis_results See wire_analyse_body().
#' @param shrink_to_fit_plot_ui See wire_analyse_body().
#' @param ANALYSE_PLOT_CHROME_WITH_INTRO_PX See wire_analyse_body().
#' @param ANALYSE_PLOT_CHROME_WITH_HEADING_PX See wire_analyse_body().
#' @param utilisation_panel_heights See wire_analyse_body().
#' @return A `nav_panel()` for the Analyse panel's tab set.
analyse_tab_utilisation <- function(analysis_results, shrink_to_fit_plot_ui,
                                    ANALYSE_PLOT_CHROME_WITH_INTRO_PX,
                                    ANALYSE_PLOT_CHROME_WITH_HEADING_PX, utilisation_panel_heights) {
    nav_panel("Bed & Resource Utilisation",
      analyse_tab_utilisation_quick(analysis_results, shrink_to_fit_plot_ui,
                                    ANALYSE_PLOT_CHROME_WITH_INTRO_PX,
                                    ANALYSE_PLOT_CHROME_WITH_HEADING_PX,
                                    utilisation_panel_heights)
    )
}

#' Build the Analyse panel's Transport tab
#'
#' The panel shows transport fleet queue and capacity margin.
#'
#' @param shrink_to_fit_plot_ui See wire_analyse_body().
#' @param ANALYSE_PLOT_CHROME_WITH_INTRO_PX See wire_analyse_body().
#' @return A `nav_panel()` for the Analyse panel's tab set.
analyse_tab_transport <- function(shrink_to_fit_plot_ui, ANALYSE_PLOT_CHROME_WITH_INTRO_PX) {
    nav_panel("Transport",
      p(class = "text-muted mt-2",
        "Transport fleet capacity margin — queue length over time by vehicle for the ",
        "dead-heading PMV Ambulance and HX240M fleets. A queue that stays at 0 throughout indicates ",
        "spare capacity; a sustained queue indicates the fleet is a binding constraint."),
      shrink_to_fit_plot_ui("plot_transport_capacity_margin", 600, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
      downloadButton("dl_transport_capacity_margin_png", "Download PNG"),
      downloadButton("dl_transport_capacity_margin_pdf", "Download PDF"),
      downloadButton("dl_transport_capacity_margin_csv", "Download Data (CSV)"),
      tags$hr(),
      h6(class = "text-muted mt-2", "Transport Utilisation by Platform"),
      p(class = "text-muted small",
        "Busy-time fraction of each vehicle fleet's capacity — a figure near 100% confirms that ",
        "fleet is the binding constraint behind a sustained queue in the plot above."),
      DTOutput("transport_utilisation_table"),
      downloadButton("dl_transport_utilisation_csv", "Download Data (CSV)")
    )
}

#' Build the Analyse panel's Waiting Times tab
#'
#' The panel shows waiting time against arrival time.
#'
#' @param shrink_to_fit_plot_ui See wire_analyse_body().
#' @param ANALYSE_PLOT_CHROME_WITH_INTRO_PX See wire_analyse_body().
#' @return A `nav_panel()` for the Analyse panel's tab set.
analyse_tab_waiting_times <- function(shrink_to_fit_plot_ui, ANALYSE_PLOT_CHROME_WITH_INTRO_PX) {
    nav_panel("Waiting Times",
      p(class = "text-muted mt-2",
        "Each point is one casualty's total time spent waiting for treatment (not treatment time ",
        "itself), plotted against when they arrived; the red line is the smoothed trend. A trend ",
        "that rises over the run signals growing system-wide congestion, not a one-off delay."),
      shrink_to_fit_plot_ui("plot_waiting_times", 600, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
      downloadButton("dl_waiting_times_png", "Download PNG"),
      downloadButton("dl_waiting_times_pdf", "Download PDF"),
      downloadButton("dl_waiting_times_csv", "Download Data (CSV)"),
      tags$hr(),
      h6(class = "text-muted mt-2", "Time-to-Treatment KPIs"),
      p(class = "text-muted small",
        "Time to first surgical incision (from R1 arrival), and dwell/transit times through R2B and R2E ",
        "(Output Variable Register KPIs) — benchmark these against your own planning timelines for ",
        "time-critical intervention."),
      DTOutput("dwell_time_table"),
      downloadButton("dl_dwell_time_csv", "Download Data (CSV)")
    )
}

#' Build the Analyse panel's Return to Duty & DOW tab
#'
#' The panel shows return-to-duty and died-of-wounds outcomes.
#'
#' @return A `nav_panel()` for the Analyse panel's tab set.
analyse_tab_outcomes <- function() {
    nav_panel("Return to Duty & DOW",
      p(class = "text-muted mt-2",
        "Return-to-duty (RTD) casualties — battle fatigue RTDs return at R1 without clinical ",
        "treatment; clinical RTDs are discharged from an R1/R2B/R2E hold bed — and died-of-wounds ",
        "(DOW) casualties, broken down by the echelon at which the outcome occurred. Use this to see ",
        "how much of the casualty stream is recovered versus lost, and where deaths concentrate."),
      uiOutput("rtd_dow_cards"),
      tags$hr(),
      h6(class = "text-muted mt-2", "DOW Count by Echelon"),
      DTOutput("dow_by_echelon_table"),
      downloadButton("dl_dow_by_echelon_csv", "Download Data (CSV)"),
      tags$hr(),
      h6(class = "text-muted mt-2", "RTD Count by Echelon"),
      DTOutput("rtd_by_echelon_table"),
      downloadButton("dl_rtd_by_echelon_csv", "Download Data (CSV)")
    )
}

#' Build the Analyse panel's Force Regeneration tab
#'
#' The panel shows effective force size over the run.
#'
#' @param shrink_to_fit_plot_ui See wire_analyse_body().
#' @param ANALYSE_PLOT_CHROME_WITH_INTRO_PX See wire_analyse_body().
#' @return A `nav_panel()` for the Analyse panel's tab set.
analyse_tab_force_regeneration <- function(shrink_to_fit_plot_ui,
                                           ANALYSE_PLOT_CHROME_WITH_INTRO_PX) {
    nav_panel("Force Regeneration",
      p(class = "text-muted mt-2",
        "Effective force size over the run — debited at each injury, credited at each return-to-duty ",
        "event, and stepped up by reinforcement demands after a lag and a partial fill, if enabled ",
        "(Configure tab's Reinforcement Demand & Fulfillment group). A persistently declining line ",
        "signals the force is attritting faster than it recovers or is reinforced."),
      shrink_to_fit_plot_ui("plot_force_regeneration", 500, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
      downloadButton("dl_force_regeneration_png", "Download PNG"),
      downloadButton("dl_force_regeneration_pdf", "Download PDF"),
      downloadButton("dl_force_regeneration_csv", "Download Data (CSV)")
    )
}

#' Build the Analyse panel's Strategic AME tab
#'
#' The panel shows strategic evacuation demand and performance.
#'
#' @param analysis_results See wire_analyse_body().
#' @param shrink_to_fit_plot_ui See wire_analyse_body().
#' @param ANALYSE_PLOT_CHROME_WITH_INTRO_PX See wire_analyse_body().
#' @param ANALYSE_PLOT_CHROME_WITH_HEADING_PX See wire_analyse_body().
#' @return A `nav_panel()` for the Analyse panel's tab set.
analyse_tab_strategic_ame <- function(analysis_results, shrink_to_fit_plot_ui,
                                      ANALYSE_PLOT_CHROME_WITH_INTRO_PX,
                                      ANALYSE_PLOT_CHROME_WITH_HEADING_PX) {
    nav_panel("Strategic AME",
      p(class = "text-muted mt-2",
        "Strategic aeromedical evacuation — casualties simultaneously awaiting a scheduled ",
        "AME sortie, split by critical (ICU, CCATT/CCAST) vs standard (Hold, Casualty Staging Unit) pool, ",
        "and the outcome of every scheduled sortie opportunity (flown or cancelled, seats boarded ",
        "against capacity added). Use the panels below to judge whether the AME schedule ",
        "you've configured can keep pace with strategic evacuation demand."),
      h6(class = "text-muted mt-2", "Role 4 (National Support Base) Census"),
      p(class = "text-muted small",
        "Unconstrained demand signal: daily bed occupancy by ward if every strategically evacuated ",
        "casualty were admitted with no Role 4 capacity limit — use this to size Role 4 (national ",
        "support base) bed capacity, independent of how well the current AME schedule keeps pace."),
      if (is.null(analysis_results()$role4_census_plot)) {
        div(class = "alert alert-secondary", "No strategic evacuations recorded across these replications.")
      } else tagList(
        shrink_to_fit_plot_ui("plot_role4_census", 500, chrome_px = ANALYSE_PLOT_CHROME_WITH_HEADING_PX),
        downloadButton("dl_role4_census_png", "Download PNG"),
        downloadButton("dl_role4_census_pdf", "Download PDF"),
        downloadButton("dl_role4_census_csv", "Download Data (CSV)"),
        uiOutput("role4_summary_cards")
      ),
      tags$hr(),
      h6(class = "text-muted mt-2", "Unconstrained AME Sortie Demand"),
      p(class = "text-muted small",
        "Same-day, uncapped, best-case sorties that would be required to keep pace with strategic ",
        "evacuation decisions, ignoring the real sortie schedule entirely — an upper-bound demand ",
        "signal, not the actual constrained performance below."),
      uiOutput("ame_demand_cards"),
      tags$hr(),
      h6(class = "text-muted mt-2", "AME Backlog Over Time"),
      p(class = "text-muted small",
        "Casualties awaiting AME sortie capacity over time, by pool — a rising line means the ",
        "sortie schedule isn't keeping pace with strategic evacuation demand."),
      shrink_to_fit_plot_ui("plot_ame_backlog", 500, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
      downloadButton("dl_ame_backlog_png", "Download PNG"),
      downloadButton("dl_ame_backlog_pdf", "Download PDF"),
      downloadButton("dl_ame_backlog_csv", "Download Data (CSV)"),
      tags$hr(),
      h6(class = "text-muted mt-2", "AME Sortie Timeline"),
      p(class = "text-muted small",
        "Grey bar = seats added at each scheduled sortie; coloured bar = seats actually boarded ",
        "before the next sortie, coloured by whether the sortie flew or was cancelled. A coloured bar ",
        "consistently at or above grey signals a backlog is being drawn down; consistently below ",
        "means capacity exceeds demand at that point."),
      shrink_to_fit_plot_ui("plot_ame_sortie", 500, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
      downloadButton("dl_ame_sortie_png", "Download PNG"),
      downloadButton("dl_ame_sortie_pdf", "Download PDF"),
      downloadButton("dl_ame_sortie_csv", "Download Data (CSV)"),
      tags$hr(),
      h6(class = "text-muted mt-2", "Actual AME Wait Time by Route"),
      p(class = "text-muted small",
        "Real wait time from evacuation decision to actual sortie boarding on the constrained AME ",
        "resources, by route (critical vs standard). Compare against the unconstrained demand above ",
        "to see how much the real schedule lags theoretical best-case throughput."),
      if (is.null(analysis_results()$ame_wait_time_summary)) {
        div(class = "alert alert-secondary", "No strategic evacuation decisions recorded across these replications.")
      } else tagList(
        DTOutput("ame_wait_time_table"),
        downloadButton("dl_ame_wait_time_csv", "Download Data (CSV)")
      )
    )
}

#' Build the Analyse panel's Casualty Surge Events tab
#'
#' The panel shows the casualty surge events the run injected.
#'
#' @param analysis_results See wire_analyse_body().
#' @param shrink_to_fit_plot_ui See wire_analyse_body().
#' @param ANALYSE_PLOT_CHROME_WITH_INTRO_PX See wire_analyse_body().
#' @return A `nav_panel()` for the Analyse panel's tab set.
analyse_tab_casualty_surge <- function(analysis_results, shrink_to_fit_plot_ui,
                                       ANALYSE_PLOT_CHROME_WITH_INTRO_PX) {
    nav_panel("Casualty Surge Events",
      p(class = "text-muted mt-2",
        "Compound-Poisson casualty surge injection events, reconstructed from tagged ",
        "casualties' arrival times, and a comparison of died-of-wounds rate for casualties originating ",
        "from a casualty surge event vs. background generation."),
      uiOutput("casualty_surge_event_count_card"),
      if (nrow(analysis_results()$casualty_surge_events_summary) == 0) {
        div(class = "alert alert-secondary",
            "No casualty surge events occurred across these replications ",
            "(env_data.json casualty_surge configuration may be disabled, or none were drawn).")
      } else tagList(
        h6(class = "text-muted mt-2", "Casualty Surge Event Timeline"),
        p(class = "text-muted small",
          "Each point is one casualty surge event: when it occurred and how many casualties it ",
          "injected — use this to confirm your configured event schedule or rate produced the timing ",
          "and scale of surge you intended."),
        shrink_to_fit_plot_ui("plot_casualty_surge_timeline", 500, chrome_px = ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
        downloadButton("dl_casualty_surge_timeline_png", "Download PNG"),
        downloadButton("dl_casualty_surge_timeline_pdf", "Download PDF"),
        downloadButton("dl_casualty_surge_timeline_csv", "Download Data (CSV)"),
        tags$hr(),
        h6(class = "text-muted mt-2", "Individual Events"),
        DTOutput("casualty_surge_events_table")
      ),
      tags$hr(),
      h6(class = "text-muted mt-2", "DOW Rate — Casualty Surge Event vs Background"),
      p(class = "text-muted small",
        "Compares died-of-wounds rate for casualties from a casualty surge event against ",
        "background-generated casualties — a higher event-origin rate suggests surge conditions ",
        "are degrading care, not just adding volume."),
      DTOutput("casualty_surge_dow_table"),
      downloadButton("dl_casualty_surge_dow_csv", "Download Data (CSV)")
    )
}

#' Build the Analyse panel's Sensitivity Calibration tab
#'
#' The panel shows the screened-parameter reference table.
#'
#' @return A `nav_panel()` for the Analyse panel's tab set.
analyse_tab_sensitivity <- function() {
    nav_panel("Sensitivity Screening",
      p(class = "text-muted mt-2",
        "Parameters screened in the project's Morris sensitivity analysis; their bounds set the ",
        "Configure tab's slider ranges. The Variable column is the raw code used in ",
        "outputs/morris_ranking.csv and the morris_*.png plots."),
      DTOutput("calibration_table"),
      downloadButton("dl_calibration_csv", "Download Table (CSV)"),
      tags$hr(),
      p(class = "text-muted small",
        "Screens run from a terminal, not from this console: ",
        tags$code("Rscript scripts/run_sensitivity.R"),
        ". See the README's Sensitivity Analysis section for the design and run time.")
    )
}

#' Wire the Analyse panel's tab layout
#'
#' @param analysis_results See wire_analyse_outputs().
#' @param run_state See wire_analyse_outputs().
#' @param utilisation_panel_heights See wire_analyse_outputs().
#' @param output See wire_analyse_outputs().
#' @param ANALYSE_PLOT_CHROME_WITH_HEADING_PX See wire_analyse_outputs().
#' @param ANALYSE_PLOT_CHROME_WITH_INTRO_PX See wire_analyse_outputs().
#' @param shrink_to_fit_plot_ui See wire_analyse_outputs().
#' @return Invisibly NULL; called for the outputs it registers.
wire_analyse_body <- function(analysis_results, run_state,
                              utilisation_panel_heights, output,
                              ANALYSE_PLOT_CHROME_WITH_HEADING_PX,
                              ANALYSE_PLOT_CHROME_WITH_INTRO_PX, shrink_to_fit_plot_ui) {
  output$analyse_body <- renderUI({
    if (run_state() != "done" || is.null(analysis_results())) {
      return(div(class = "alert alert-info mt-3",
                 "No results yet. Run a Quick Run from the Run tab to populate this page."))
    }
    tagList(
      navset_tab(
        analyse_tab_casualty_flow(shrink_to_fit_plot_ui, ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
        analyse_tab_queue_depths(shrink_to_fit_plot_ui, ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
        analyse_tab_utilisation(analysis_results, shrink_to_fit_plot_ui,
                                ANALYSE_PLOT_CHROME_WITH_INTRO_PX,
                                ANALYSE_PLOT_CHROME_WITH_HEADING_PX,
                                utilisation_panel_heights),
        analyse_tab_transport(shrink_to_fit_plot_ui, ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
        analyse_tab_waiting_times(shrink_to_fit_plot_ui, ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
        analyse_tab_outcomes(),
        analyse_tab_force_regeneration(shrink_to_fit_plot_ui, ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
        analyse_tab_strategic_ame(analysis_results, shrink_to_fit_plot_ui,
                                  ANALYSE_PLOT_CHROME_WITH_INTRO_PX,
                                  ANALYSE_PLOT_CHROME_WITH_HEADING_PX),
        analyse_tab_casualty_surge(analysis_results, shrink_to_fit_plot_ui,
                                   ANALYSE_PLOT_CHROME_WITH_INTRO_PX),
        analyse_tab_sensitivity()
      )
    )
  })
  invisible(NULL)
}

#' Register every plot the Analyse panel renders
#'
#' @param analysis_results See wire_analyse_outputs().
#' @param tab_plot See wire_analyse_outputs().
#' @param utilisation_panel_heights See wire_analyse_outputs().
#' @param new_shrink_to_fit_plot See wire_analyse_outputs().
#' @return Invisibly NULL; called for the outputs it registers.
register_analyse_plots <- function(analysis_results, tab_plot,
                                   utilisation_panel_heights,
                                   new_shrink_to_fit_plot) {
  new_shrink_to_fit_plot("plot_casualty_flow", "Casualty Flow — Full Size",
                         function() tab_plot()$casualty_flow, function() 700)
  new_shrink_to_fit_plot("plot_r1_queues", "R1 Queues — Full Size",
                         function() analysis_results()$r1_queues, function() 600)
  new_shrink_to_fit_plot("plot_r2b_bed_queues", "R2B Bed Queues — Full Size",
                         function() analysis_results()$r2b_bed_queues, function() 600)
  new_shrink_to_fit_plot("plot_r2e_bed_queues", "R2E Bed Queues — Full Size",
                         function() analysis_results()$r2e_bed_queues, function() 600)
  new_shrink_to_fit_plot("plot_r2b_treatment", "R2B Treatment — Full Size",
                         function() { analysis_results()$r2b_treatment },
                         function() utilisation_panel_heights()$r2b_treatment)
  new_shrink_to_fit_plot("plot_r2b_gantt", "R2B Bed Resource Usage (Gantt) — Full Size",
                         function() { analysis_results()$r2b_gantt },
                         function() utilisation_panel_heights()$r2b_gantt)
  new_shrink_to_fit_plot("plot_r2e_surgery", "R2E Surgery — Full Size",
                         function() { analysis_results()$r2e_surgery },
                         function() utilisation_panel_heights()$r2e_surgery)
  new_shrink_to_fit_plot("plot_r2e_gantt", "R2E Bed Resource Usage (Gantt) — Full Size",
                         function() { analysis_results()$r2e_gantt },
                         function() utilisation_panel_heights()$r2e_gantt)
  new_shrink_to_fit_plot("plot_r2e_icu_gating", "R2E OT-ICU Gating — Full Size",
                         function() { analysis_results()$r2e_icu_gating_plot },
                         function() utilisation_panel_heights()$r2e_icu_gating)
  new_shrink_to_fit_plot("plot_waiting_times", "Waiting Times — Full Size",
                         function() tab_plot()$waiting_times, function() 600)
  new_shrink_to_fit_plot("plot_force_regeneration", "Force Regeneration — Full Size",
                         function() tab_plot()$force_regeneration, function() 500)
  new_shrink_to_fit_plot("plot_ame_backlog", "AME Backlog Over Time — Full Size",
                         function() tab_plot()$ame_backlog, function() 500)
  new_shrink_to_fit_plot("plot_ame_sortie", "AME Sortie Timeline — Full Size",
                         function() tab_plot()$ame_sortie, function() 500)
  # These five plots are computed by both analyse_run() and
  # analyse_replications() under the identical field name (the latter
  # returning a mean +/- 95% CI ribbon/bar version rather than a single-run
  # trace), so the plot itself renders unchanged
  # regardless of run_mode(); only the cards/tables surrounding it in the UI
  # differ by mode (scalar counts for Quick Run vs CI'd counts for Full
  # Analysis — see r2b_routing_cards/role4_summary_cards/etc. below).
  new_shrink_to_fit_plot("plot_r2b_hold_occupancy", "R2B Hold Bed Occupancy — Full Size",
                         function() analysis_results()$r2b_hold_occupancy_plot, function() 450)
  new_shrink_to_fit_plot("plot_r2b_bypass_reason", "R2B OT Bypass Reason — Full Size",
                         function() analysis_results()$r2b_bypass_reason_plot, function() 450)
  new_shrink_to_fit_plot("plot_transport_capacity_margin", "Transport Fleet Capacity Margin — Full Size",
                         function() analysis_results()$transport_capacity_margin_plot, function() 600)
  new_shrink_to_fit_plot("plot_role4_census", "Role 4 Census — Full Size",
                         function() analysis_results()$role4_census_plot, function() 500)
  new_shrink_to_fit_plot("plot_casualty_surge_timeline", "Casualty Surge Event Timeline — Full Size",
                         function() analysis_results()$casualty_surge_timeline_plot, function() 500)
  invisible(NULL)
}

#' Wire the image downloads for every Analyse plot
#'
#' @param analysis_results See wire_analyse_outputs().
#' @param tab_plot See wire_analyse_outputs().
#' @param output See wire_analyse_outputs().
#' @return `plot_download_handler`.
wire_plot_image_downloads <- function(analysis_results, tab_plot, output) {
  #' Build a download handler saving one plot as an image
  #'
  #' @param plot_fn Zero-argument function returning the plot object.
  #' @param width Image width in inches.
  #' @param height Image height in inches.
  #' @param device Output device, "png" or "pdf".
  #' @return A Shiny download handler.
  plot_download_handler <- function(plot_fn, width, height, device) {
    downloadHandler(
      filename = function() sprintf("plot.%s", device),
      content  = function(file) {
        ggsave(file, plot = plot_fn(), width = width, height = height,
               dpi = 300, device = device)
      }
    )
  }
  output$dl_casualty_flow_png <- plot_download_handler(function() tab_plot()$casualty_flow, 10, 12, "png")
  output$dl_casualty_flow_pdf <- plot_download_handler(function() tab_plot()$casualty_flow, 10, 12, "pdf")
  output$dl_r1_queues_png       <- plot_download_handler(function() analysis_results()$r1_queues, 10, 5, "png")
  output$dl_r1_queues_pdf       <- plot_download_handler(function() analysis_results()$r1_queues, 10, 5, "pdf")
  output$dl_r2b_bed_queues_png  <- plot_download_handler(function() analysis_results()$r2b_bed_queues, 10, 5, "png")
  output$dl_r2b_bed_queues_pdf  <- plot_download_handler(function() analysis_results()$r2b_bed_queues, 10, 5, "pdf")
  output$dl_r2e_bed_queues_png  <- plot_download_handler(function() analysis_results()$r2e_bed_queues, 10, 5, "png")
  output$dl_r2e_bed_queues_pdf  <- plot_download_handler(function() analysis_results()$r2e_bed_queues, 10, 5, "pdf")
  output$dl_utilisation_png   <- plot_download_handler(function() analysis_results()$utilisation, 12, 8, "png")
  output$dl_utilisation_pdf   <- plot_download_handler(function() analysis_results()$utilisation, 12, 8, "pdf")
  output$dl_r2b_treatment_png <- plot_download_handler(function() analysis_results()$r2b_treatment, 10, 5, "png")
  output$dl_r2b_treatment_pdf <- plot_download_handler(function() analysis_results()$r2b_treatment, 10, 5, "pdf")
  output$dl_r2b_gantt_png     <- plot_download_handler(function() analysis_results()$r2b_gantt, 12, 8, "png")
  output$dl_r2b_gantt_pdf     <- plot_download_handler(function() analysis_results()$r2b_gantt, 12, 8, "pdf")
  output$dl_r2e_surgery_png   <- plot_download_handler(function() analysis_results()$r2e_surgery, 10, 5, "png")
  output$dl_r2e_surgery_pdf   <- plot_download_handler(function() analysis_results()$r2e_surgery, 10, 5, "pdf")
  output$dl_r2e_gantt_png     <- plot_download_handler(function() analysis_results()$r2e_gantt, 12, 8, "png")
  output$dl_r2e_gantt_pdf     <- plot_download_handler(function() analysis_results()$r2e_gantt, 12, 8, "pdf")
  output$dl_r2e_icu_gating_png <- plot_download_handler(function() analysis_results()$r2e_icu_gating_plot, 12, 6, "png")
  output$dl_r2e_icu_gating_pdf <- plot_download_handler(function() analysis_results()$r2e_icu_gating_plot, 12, 6, "pdf")
  output$dl_waiting_times_png <- plot_download_handler(function() tab_plot()$waiting_times, 10, 6, "png")
  output$dl_waiting_times_pdf <- plot_download_handler(function() tab_plot()$waiting_times, 10, 6, "pdf")
  output$dl_force_regeneration_png <- plot_download_handler(function() tab_plot()$force_regeneration, 10, 6, "png")
  output$dl_force_regeneration_pdf <- plot_download_handler(function() tab_plot()$force_regeneration, 10, 6, "pdf")
  output$dl_ame_backlog_png <- plot_download_handler(function() tab_plot()$ame_backlog, 12, 8, "png")
  output$dl_ame_backlog_pdf <- plot_download_handler(function() tab_plot()$ame_backlog, 12, 8, "pdf")
  output$dl_ame_sortie_png  <- plot_download_handler(function() tab_plot()$ame_sortie, 12, 8, "png")
  output$dl_ame_sortie_pdf  <- plot_download_handler(function() tab_plot()$ame_sortie, 12, 8, "pdf")
  output$dl_r2b_hold_occupancy_png <- plot_download_handler(function() analysis_results()$r2b_hold_occupancy_plot, 12, 6, "png")
  output$dl_r2b_hold_occupancy_pdf <- plot_download_handler(function() analysis_results()$r2b_hold_occupancy_plot, 12, 6, "pdf")
  output$dl_r2b_bypass_reason_png  <- plot_download_handler(function() analysis_results()$r2b_bypass_reason_plot, 12, 6, "png")
  output$dl_r2b_bypass_reason_pdf  <- plot_download_handler(function() analysis_results()$r2b_bypass_reason_plot, 12, 6, "pdf")
  output$dl_transport_capacity_margin_png <- plot_download_handler(function() analysis_results()$transport_capacity_margin_plot, 12, 8, "png")
  output$dl_transport_capacity_margin_pdf <- plot_download_handler(function() analysis_results()$transport_capacity_margin_plot, 12, 8, "pdf")
  output$dl_role4_census_png <- plot_download_handler(function() analysis_results()$role4_census_plot, 12, 6, "png")
  output$dl_role4_census_pdf <- plot_download_handler(function() analysis_results()$role4_census_plot, 12, 6, "pdf")
  output$dl_casualty_surge_timeline_png <- plot_download_handler(function() analysis_results()$casualty_surge_timeline_plot, 12, 6, "png")
  output$dl_casualty_surge_timeline_pdf <- plot_download_handler(function() analysis_results()$casualty_surge_timeline_plot, 12, 6, "pdf")
  plot_download_handler
}

#' Wire the data downloads for every Analyse plot
#'
#' @param analysis_results See wire_analyse_outputs().
#' @param mon_data See wire_analyse_outputs().
#' @param output See wire_analyse_outputs().
#' @return A list of `echelon_ot_utilisation_csv`, `filtered_resources_csv`.
wire_plot_data_downloads <- function(analysis_results, mon_data, output) {
  output$dl_casualty_flow_csv <- downloadHandler(
    filename = "casualty_flow.csv",
    content  = function(file) write.csv(analysis_results()$casualty_summary, file, row.names = FALSE)
  )
  #' Build a download handler for one echelon's resource monitor rows
  #'
  #' @param pattern Regular expression selecting the resources to write.
  #' @return A Shiny download handler.
  #' @details The pattern is the same one `analyse_run()` and
  #'   `analyse_replications()` build each split plot from, so a panel's CSV
  #'   matches its own image rather than carrying every echelon's rows.
  filtered_resources_csv <- function(pattern) {
    downloadHandler(
      filename = function() "resource_data.csv",
      content  = function(file) {
        df <- mon_data()$resources
        write.csv(df[grepl(pattern, df$resource), ], file, row.names = FALSE)
      }
    )
  }
  output$dl_r1_queues_csv      <- filtered_resources_csv("^c_r1_.*_\\d+_t\\d+$")
  output$dl_r2b_bed_queues_csv <- filtered_resources_csv("^b_r2b_\\w+_\\d+_t\\d+$")
  output$dl_r2e_bed_queues_csv <- filtered_resources_csv("^b_r2eheavy_\\w+_\\d+_t\\d+$")
  output$dl_r2b_gantt_csv      <- filtered_resources_csv("^b_r2b_\\w+_\\d+_t\\d+$")
  output$dl_r2e_gantt_csv      <- filtered_resources_csv("^b_r2eheavy_\\w+_\\d+_t\\d+$")
  #' Build a download handler for one echelon's theatre utilisation rows
  #'
  #' @param echelon Echelon label to filter on, "R2B" or "R2E".
  #' @return A Shiny download handler.
  #' @details The R2B treatment and R2E surgery panels have no per-panel
  #'   frame of their own, being derived while plotting, so each panel's CSV
  #'   is its own echelon's subset of the theatre utilisation frame.
  echelon_ot_utilisation_csv <- function(echelon) {
    downloadHandler(
      filename = function() "ot_utilisation.csv",
      content  = function(file) {
        df <- analysis_results()$ot_utilisation
        write.csv(df[df$echelon == echelon, ], file, row.names = FALSE)
      }
    )
  }
  output$dl_r2b_treatment_csv <- echelon_ot_utilisation_csv("R2B")
  output$dl_r2e_surgery_csv   <- echelon_ot_utilisation_csv("R2E")
  output$dl_r2e_icu_gating_csv <- downloadHandler(
    filename = "r2e_icu_gating_daily.csv",
    content  = function(file) write.csv(analysis_results()$r2e_icu_gating_daily, file, row.names = FALSE)
  )
  output$dl_waiting_times_csv <- downloadHandler(
    filename = "waiting_times.csv",
    content  = function(file) {
      df <- analysis_results()$combined
      write.csv(df[, intersect(c("name", "start_time", "waiting_time", "priority"), names(df))],
                file, row.names = FALSE)
    }
  )
  output$dl_force_regeneration_csv <- downloadHandler(
    filename = "force_regeneration.csv",
    content  = function(file) write.csv(analysis_results()$force_regeneration_daily, file, row.names = FALSE)
  )
  output$dl_ame_backlog_csv <- downloadHandler(
    filename = "ame_backlog_data.csv",
    content  = function(file) write.csv(analysis_results()$ame_backlog_data, file, row.names = FALSE)
  )
  output$dl_ame_sortie_csv <- downloadHandler(
    filename = "ame_sortie_data.csv",
    content  = function(file) write.csv(analysis_results()$ame_sortie_data, file, row.names = FALSE)
  )
  list(
    echelon_ot_utilisation_csv = echelon_ot_utilisation_csv,
    filtered_resources_csv = filtered_resources_csv
  )
}

#' Wire every rendered output on the Analyse panel
#'
#' @param analysis_results See analyse_run().
#' @param mon_data See analyse_run().
#' @param run_state See analyse_run().
#' @param tab_plot See analyse_run().
#' @param utilisation_panel_heights See analyse_run().
#' @param input See analyse_run().
#' @param output See analyse_run().
#' @return A list of `ANALYSE_PLOT_CHROME_PX`, `ANALYSE_PLOT_CHROME_WITH_HEADING_PX`,
#'   `ANALYSE_PLOT_CHROME_WITH_INTRO_PX`, `echelon_ot_utilisation_csv`, `filtered_resources_csv`,
#'   `new_shrink_to_fit_plot`, `plot_download_handler`, `shrink_to_fit_plot_ui`.
wire_analyse_outputs <- function(analysis_results, mon_data, run_state, tab_plot,
                                 utilisation_panel_heights, input,
                                 output) {
  shrink_to_fit_helpers_out <- build_shrink_to_fit_helpers(input, output)
  ANALYSE_PLOT_CHROME_PX <- shrink_to_fit_helpers_out$ANALYSE_PLOT_CHROME_PX
  ANALYSE_PLOT_CHROME_WITH_HEADING_PX <- shrink_to_fit_helpers_out$ANALYSE_PLOT_CHROME_WITH_HEADING_PX
  ANALYSE_PLOT_CHROME_WITH_INTRO_PX <- shrink_to_fit_helpers_out$ANALYSE_PLOT_CHROME_WITH_INTRO_PX
  new_shrink_to_fit_plot <- shrink_to_fit_helpers_out$new_shrink_to_fit_plot
  shrink_to_fit_plot_ui <- shrink_to_fit_helpers_out$shrink_to_fit_plot_ui

  # KPI summary cards (Full Analysis only) — mean (95% CI) headline figures
  # shown above the output tabs.
  wire_analyse_summary_outputs(mon_data, output)

  wire_analyse_body(analysis_results, run_state,
                    utilisation_panel_heights, output, ANALYSE_PLOT_CHROME_WITH_HEADING_PX,
                    ANALYSE_PLOT_CHROME_WITH_INTRO_PX, shrink_to_fit_plot_ui)

  register_analyse_plots(analysis_results, tab_plot,
                         utilisation_panel_heights, new_shrink_to_fit_plot)

  plot_download_handler <- wire_plot_image_downloads(analysis_results, tab_plot, output)

  plot_data_downloads_out <- wire_plot_data_downloads(analysis_results, mon_data, output)
  echelon_ot_utilisation_csv <- plot_data_downloads_out$echelon_ot_utilisation_csv
  filtered_resources_csv <- plot_data_downloads_out$filtered_resources_csv
  list(
    ANALYSE_PLOT_CHROME_PX = ANALYSE_PLOT_CHROME_PX,
    ANALYSE_PLOT_CHROME_WITH_HEADING_PX = ANALYSE_PLOT_CHROME_WITH_HEADING_PX,
    ANALYSE_PLOT_CHROME_WITH_INTRO_PX = ANALYSE_PLOT_CHROME_WITH_INTRO_PX,
    echelon_ot_utilisation_csv = echelon_ot_utilisation_csv,
    filtered_resources_csv = filtered_resources_csv,
    new_shrink_to_fit_plot = new_shrink_to_fit_plot,
    plot_download_handler = plot_download_handler,
    shrink_to_fit_plot_ui = shrink_to_fit_plot_ui
  )
}
