##############################################
## app.R                                    ##
## Battlefield Casualty Handling Simulation ##
## Shiny app — Configure / Run / Analyse    ##
## (Issue #14)                              ##
##############################################
#
# Launch with:
#   shiny::runApp("app.R")
#
# Three-panel workflow:
#   Configure — plain-English parameter editor grouped by operational concept
#   Run       — Quick Run (single replication); Full Analysis mode is a
#               disabled placeholder pending Issue #15
#   Analyse   — four result tabs (rendered from R/analysis.R's ggplot
#               objects) plus a read-only Sensitivity Calibration tab
#
# controller_legacy.R (the previous raw-JSON editor) is superseded by this
# app and retained only for reference.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(jsonlite)
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(DT)
  library(future)
  library(promises)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/sensitivity.R")
source("R/app_params.R")

plan(multisession)
# run_once()/analyse_run() are called from inside future() with a seed
# supplied explicitly (or intentionally NULL for a random Quick Run); the
# future package's own parallel-RNG-safety warning does not apply here.
options(future.rng.onMisuse = "ignore")

APP_DIR         <- normalizePath(".")
DEFAULT_JSON    <- "env_data.json"
PARAM_REGISTRY  <- build_param_registry()

MORRIS_LABELS <- c(
  surg_mode              = "Surgery Duration (Mode)",
  long_resus_mode        = "Long Resuscitation Duration (Mode)",
  p1_p_max               = "Priority 1 DOW Ceiling",
  r1_transport           = "R1 Transport Time (Mode)",
  r2b_transport          = "R2B Transport Time (Mode)",
  long_icu_mode          = "Long ICU Stay (Mode)",
  pri1_surg_prob         = "Priority 1 Surgical Candidacy",
  in_theatre_rate        = "In-Theatre Recovery Rate",
  ot_hours               = "OT Shift Length (Hours per Shift)",
  return_leg_multiplier  = "Transport Return-Leg Multiplier"
)

# ── UI helpers ────────────────────────────────────────────────────────────

field_label <- function(f) {
  tooltip(
    tags$span(f$label, tags$span(style = "color:#888; cursor:help;", HTML("&nbsp;&#9432;"))),
    f$tooltip,
    placement = "right"
  )
}

field_input <- function(f, value) {
  lbl <- field_label(f)
  if (isTRUE(f$morris)) {
    # Defensive widening: the current env_data.json baseline should always
    # be representable on the slider, even where a screened Morris range
    # (R/sensitivity.R) has drifted from the baseline after a later
    # recalibration (e.g. p1_p_max's screening bounds of 0.25-0.75 predate
    # Issue #5's DOW recalibration to a 0.023 baseline) — flagged as a
    # follow-up data-consistency issue rather than silently clipping here.
    lo <- f$min; hi <- f$max
    if (!is.null(value) && !is.na(value)) {
      lo <- min(lo, value)
      hi <- max(hi, value)
    }
    sliderInput(f$id, lbl, min = lo, max = hi, value = value,
                step = if (identical(f$type, "integer")) 1 else f$step)
  } else if (identical(f$type, "integer")) {
    numericInput(f$id, lbl, value = value, min = f$min, max = f$max, step = 1)
  } else {
    numericInput(f$id, lbl, value = value, min = f$min, max = f$max, step = f$step)
  }
}

#' Render one top-level Configure accordion panel body for a field group
render_group_body <- function(fields, defaults) {
  subgroups <- vapply(fields, function(f) if (is.null(f$subgroup)) "" else f$subgroup, character(1))

  if (all(subgroups == "")) {
    return(layout_column_wrap(
      width = "300px",
      !!!lapply(fields, function(f) field_input(f, defaults[[f$id]]))
    ))
  }

  tagList(lapply(unique(subgroups), function(sg) {
    sg_fields <- fields[subgroups == sg]
    tagList(
      h6(class = "text-muted mt-2", sg),
      layout_column_wrap(
        width = "300px",
        !!!lapply(sg_fields, function(f) field_input(f, defaults[[f$id]]))
      )
    )
  }))
}

# ── UI ────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = "Battlefield Casualty Handling — Simulation Console",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  id    = "main_nav",

  nav_panel(
    "Configure",
    p(class = "text-muted",
      "Adjust simulation parameters below, grouped by operational concept. ",
      "Hover the ", tags$b("ⓘ"), " icon next to any field for an explanation. ",
      "Sliders show the plausible range screened in the project's Morris sensitivity analysis (Issue #3)."),
    fluidRow(
      column(3, fileInput("upload_json", "Load Configuration (.json)", accept = ".json")),
      column(3, downloadButton("download_json", "Save Configuration", class = "btn-outline-secondary mt-4"))
    ),
    accordion(
      id = "config_accordion", open = c(GRP_FORCE),
      !!!lapply(c(GRP_FORCE, GRP_CASUALTY, GRP_R1, GRP_R2B, GRP_R2E, GRP_TRANSPORT), function(g) {
        accordion_panel(g, uiOutput(paste0("group_ui_", make.names(g))))
      })
    )
  ),

  nav_panel(
    "Run",
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Run Configuration"),
        numericInput("n_days", "Simulation Duration (days)", value = 30, min = 1, max = 180, step = 1),
        textInput("seed", "Random Seed (blank = random)", value = "42"),
        sliderInput("ot_hours",
                    field_label(list(label = "OT Shift Length (hours per shift)",
                                     tooltip = paste0(
                                       "Hours the first operating theatre shift is active each day ",
                                       "(the second shift covers the remainder of the 24h day). ",
                                       sprintf("Screened in Morris sensitivity analysis (Issue #3); plausible range %s–%s.",
                                               morris_params$lower[morris_params$name == "ot_hours"],
                                               morris_params$upper[morris_params$name == "ot_hours"])))),
                    min = morris_params$lower[morris_params$name == "ot_hours"],
                    max = morris_params$upper[morris_params$name == "ot_hours"],
                    value = morris_params$mode[morris_params$name == "ot_hours"], step = 1)
      ),
      card(
        card_header("Run Mode"),
        actionButton("run_quick", "▶ Run Quick Run (Single Replication)", class = "btn-primary btn-lg"),
        tags$div(style = "margin-top: 10px;",
          tooltip(
            tags$span(actionButton("run_full", "\U0001F512 Full Analysis (Multi-Run, 95% CI)", disabled = NA)),
            "Full Analysis mode (multi-replication with confidence intervals) requires Issue #15. Not yet available."
          )
        ),
        tags$hr(),
        uiOutput("run_status")
      )
    )
  ),

  nav_panel(
    "Analyse",
    uiOutput("analyse_body")
  )
)

# ── Server ────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  raw_env_data     <- reactiveVal(fromJSON(DEFAULT_JSON, simplifyVector = FALSE))
  run_state        <- reactiveVal("idle")   # idle | running | done | error
  run_error        <- reactiveVal(NULL)
  progress_pct     <- reactiveVal(0)
  mon_data         <- reactiveVal(NULL)
  analysis_results <- reactiveVal(NULL)
  pending_future   <- reactiveVal(NULL)

  # ── Configure panel ───────────────────────────────────────────────────────

  fields_by_group <- split(PARAM_REGISTRY, vapply(PARAM_REGISTRY, `[[`, character(1), "group"))

  lapply(names(fields_by_group), function(g) {
    output_id <- paste0("group_ui_", make.names(g))
    output[[output_id]] <- renderUI({
      defaults <- registry_defaults(fields_by_group[[g]], raw_env_data())
      render_group_body(fields_by_group[[g]], defaults)
    })
    # Accordion panels other than the initially-open one are hidden
    # (display:none) at first render; Shiny suspends output bindings it
    # judges invisible, so their inputs would never register unless the
    # user happens to open every panel first. Force eager rendering so
    # every field's input exists (and its value is capturable) regardless
    # of which panels are currently expanded.
    outputOptions(output, output_id, suspendWhenHidden = FALSE)
  })

  observeEvent(input$upload_json, {
    req(input$upload_json)
    tryCatch({
      new_json <- fromJSON(input$upload_json$datapath, simplifyVector = FALSE)
      raw_env_data(new_json)
      showNotification("Configuration loaded.", type = "message")
    }, error = function(e) {
      showNotification(paste("Failed to load configuration:", conditionMessage(e)), type = "error")
    })
  })

  current_json <- reactive({
    apply_registry_values(PARAM_REGISTRY, raw_env_data(), reactiveValuesToList(input))
  })

  output$download_json <- downloadHandler(
    filename = function() sprintf("env_data_%s.json", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) write_json(current_json(), file, pretty = TRUE, auto_unbox = TRUE)
  )

  # ── Validation ────────────────────────────────────────────────────────────

  validate_config <- function(values) {
    errors <- character(0)
    req_pos <- list(pop_combat = "Combat force size", pop_support = "Support force size",
                     r1_team_count = "Number of R1 teams", r2b_team_count = "Number of R2B teams",
                     r2e_team_count = "Number of R2E teams")
    for (id in names(req_pos)) {
      v <- values[[id]]
      if (is.null(v) || is.na(v) || v <= 0) {
        errors <- c(errors, sprintf("%s must be greater than zero (got %s).", req_pos[[id]], format(v)))
      }
    }

    tri_ids <- unique(sub("_(min|mode|max)$", "", grep("_(min|mode|max)$", names(values), value = TRUE)))
    for (pfx in tri_ids) {
      mn <- values[[paste0(pfx, "_min")]]; md <- values[[paste0(pfx, "_mode")]]; mx <- values[[paste0(pfx, "_max")]]
      if (!any(vapply(list(mn, md, mx), is.null, logical(1))) && !(mn <= md && md <= mx)) {
        errors <- c(errors, sprintf("%s: minimum must be ≤ most likely ≤ maximum (got %s / %s / %s).",
                                     pfx, mn, md, mx))
      }
    }

    for (veh in c("PMVAmb", "HX240M")) {
      qty <- values[[paste0("transport_", veh, "_qty")]]
      cap <- values[[paste0("transport_", veh, "_capacity")]]
      if (!is.null(qty) && qty > 0 && (is.null(cap) || cap < 1)) {
        errors <- c(errors, sprintf("%s: capacity per vehicle must be at least 1 when fleet size > 0.", veh))
      }
    }

    psum <- sum(values$pri_one, values$pri_two, values$pri_three)
    if (abs(psum - 1) > 0.02) {
      errors <- c(errors, sprintf("Triage priority split (Priority 1/2/3) must sum to 1 (currently %.2f).", psum))
    }
    dsum <- sum(values$dnbi_bf_pct, values$dnbi_disease_pct, values$dnbi_nbi_pct)
    if (abs(dsum - 1) > 0.02) {
      errors <- c(errors, sprintf("DNBI sub-type split (Battle Fatigue/Disease/NBI) must sum to 1 (currently %.2f).", dsum))
    }

    errors
  }

  # ── Quick Run execution (async via future + promises) ────────────────────

  observeEvent(input$run_quick, {
    values <- reactiveValuesToList(input)
    errors <- validate_config(values)
    if (length(errors) > 0) {
      showModal(modalDialog(
        title = "Configuration error",
        tags$ul(lapply(errors, tags$li)),
        easyClose = TRUE
      ))
      return(invisible(NULL))
    }

    seed_val <- suppressWarnings(as.integer(trimws(input$seed)))
    seed_val <- if (length(seed_val) == 0 || is.na(seed_val)) NULL else seed_val
    days_val     <- input$n_days
    ot_hours_val <- input$ot_hours
    built_env    <- build_environment(current_json())
    app_dir      <- APP_DIR

    run_state("running")
    run_error(NULL)
    progress_pct(5)

    fut <- future({
      setwd(app_dir)
      source("R/environment.R"); source("R/trajectories.R")
      source("R/replication.R"); source("R/analysis.R")
      # future's own multisession bootstrap attaches package:future in the
      # worker; depending on exactly when that finishes relative to the
      # library(simmer) calls above, simmer::run() can occasionally end up
      # masked by future's own run() S3 generic (both packages export a
      # generic named `run`). Force the correct binding explicitly rather
      # than relying on attach-order timing.
      assign("run", simmer::run, envir = .GlobalEnv)
      env_data <<- built_env
      day_min  <<- 1440L
      counts   <<- sapply(env_data$elms, length)

      wrapped <- run_once(days_val, seed = seed_val, write_files = FALSE, ot_hours = ot_hours_val)
      mon <- list(
        arrivals   = get_mon_arrivals(list(wrapped),   ongoing = TRUE),
        attributes = get_mon_attributes(list(wrapped)),
        resources  = get_mon_resources(list(wrapped))
      )

      out_dir <- tempfile("bch_outputs_")
      img_dir <- tempfile("bch_images_")
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
      dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)

      results <- analyse_run(mon, output_dir = out_dir, images_dir = img_dir, warm_up_days = 0)
      list(mon = mon, results = results)
    }, seed = NULL)

    prom <- fut %...>% (function(res) {
      mon_data(res$mon)
      analysis_results(res$results)
      run_state("done")
      progress_pct(100)
    })
    prom <- catch(prom, function(e) {
      run_state("error")
      run_error(conditionMessage(e))
    })
    pending_future(prom)
    invisible(NULL)
  })

  # Simulated progress ticker: future/multisession runs in a separate R
  # process, so exact percentage progress isn't observable from the main
  # session without the progressr package; this ticks toward 90% while the
  # run is in flight and snaps to 100% on completion, keeping the UI
  # responsive rather than frozen for the ~20s a typical Quick Run takes.
  observe({
    req(run_state() == "running")
    invalidateLater(400, session)
    isolate({
      p <- progress_pct()
      if (p < 90) progress_pct(min(90, p + 4))
    })
  })

  output$run_status <- renderUI({
    state <- run_state()
    if (state == "idle") {
      p(class = "text-muted", "No run yet. Configure parameters, then click Run Quick Run.")
    } else if (state == "running") {
      tagList(
        p(sprintf("Running simulation (seed %s, %d days)...",
                   if (trimws(input$seed) == "") "random" else trimws(input$seed), input$n_days)),
        div(class = "progress",
            div(class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar", style = sprintf("width: %d%%", progress_pct())))
      )
    } else if (state == "done") {
      div(class = "alert alert-success", "Run complete — see the Analyse tab for results.")
    } else {
      div(class = "alert alert-danger", paste("Run failed:", run_error()))
    }
  })

  # ── Analyse panel ─────────────────────────────────────────────────────────

  combined_plot <- function(...) Reduce(`/`, list(...))

  tab_plot <- reactive({
    res <- analysis_results()
    req(res)
    list(
      casualty_flow = res$casualty_flow,
      queue_depths  = combined_plot(res$r1_queues, res$r2b_bed_queues, res$r2e_bed_queues),
      utilisation   = combined_plot(res$r2b_treatment, res$r2b_gantt, res$r2e_surgery, res$r2e_gantt),
      waiting_times = res$waiting_times
    )
  })

  output$analyse_body <- renderUI({
    if (run_state() != "done" || is.null(analysis_results())) {
      return(div(class = "alert alert-info mt-3",
                  "No results yet. Run a Quick Run from the Run tab to populate this page."))
    }
    navset_tab(
      nav_panel("Casualty Flow",
        plotOutput("plot_casualty_flow", height = "700px"),
        downloadButton("dl_casualty_flow_png", "Download PNG"),
        downloadButton("dl_casualty_flow_pdf", "Download PDF"),
        downloadButton("dl_casualty_flow_csv", "Download Data (CSV)")
      ),
      nav_panel("Queue Depths",
        plotOutput("plot_queue_depths", height = "900px"),
        downloadButton("dl_queue_depths_png", "Download PNG"),
        downloadButton("dl_queue_depths_pdf", "Download PDF"),
        downloadButton("dl_queue_depths_csv", "Download Data (CSV)")
      ),
      nav_panel("Bed & Resource Utilisation",
        plotOutput("plot_utilisation", height = "1400px"),
        downloadButton("dl_utilisation_png", "Download PNG"),
        downloadButton("dl_utilisation_pdf", "Download PDF"),
        downloadButton("dl_utilisation_csv", "Download Data (CSV)")
      ),
      nav_panel("Waiting Times",
        plotOutput("plot_waiting_times", height = "600px"),
        downloadButton("dl_waiting_times_png", "Download PNG"),
        downloadButton("dl_waiting_times_pdf", "Download PDF"),
        downloadButton("dl_waiting_times_csv", "Download Data (CSV)")
      ),
      nav_panel("Sensitivity Calibration",
        p(class = "text-muted mt-2",
          "Parameters screened in the project's Morris Elementary Effects sensitivity analysis (Issue #3). ",
          "These bounds are used as slider ranges for the corresponding fields in the Configure tab."),
        DTOutput("calibration_table"),
        tags$div(style = "margin-top: 10px;",
          tooltip(
            tags$span(actionButton("run_sensitivity", "Run Sensitivity Screening", disabled = NA)),
            "Running the full Morris/Sobol screen from the app requires Issue #15. Use scripts/run_sensitivity.R until then."
          )
        ),
        downloadButton("dl_calibration_csv", "Download Table (CSV)")
      )
    )
  })

  output$plot_casualty_flow <- renderPlot(tab_plot()$casualty_flow)
  output$plot_queue_depths  <- renderPlot(tab_plot()$queue_depths)
  output$plot_utilisation   <- renderPlot(tab_plot()$utilisation)
  output$plot_waiting_times <- renderPlot(tab_plot()$waiting_times)

  plot_download_handler <- function(plot_key, width, height, device) {
    downloadHandler(
      filename = function() sprintf("%s.%s", plot_key, device),
      content  = function(file) {
        ggsave(file, plot = tab_plot()[[plot_key]], width = width, height = height,
               dpi = 300, device = device)
      }
    )
  }
  output$dl_casualty_flow_png <- plot_download_handler("casualty_flow", 10, 12, "png")
  output$dl_casualty_flow_pdf <- plot_download_handler("casualty_flow", 10, 12, "pdf")
  output$dl_queue_depths_png  <- plot_download_handler("queue_depths", 10, 14, "png")
  output$dl_queue_depths_pdf  <- plot_download_handler("queue_depths", 10, 14, "pdf")
  output$dl_utilisation_png   <- plot_download_handler("utilisation", 12, 20, "png")
  output$dl_utilisation_pdf   <- plot_download_handler("utilisation", 12, 20, "pdf")
  output$dl_waiting_times_png <- plot_download_handler("waiting_times", 10, 6, "png")
  output$dl_waiting_times_pdf <- plot_download_handler("waiting_times", 10, 6, "pdf")

  output$dl_casualty_flow_csv <- downloadHandler(
    filename = "casualty_flow.csv",
    content  = function(file) write.csv(analysis_results()$casualty_summary, file, row.names = FALSE)
  )
  output$dl_queue_depths_csv <- downloadHandler(
    filename = "queue_depths.csv",
    content  = function(file) write.csv(mon_data()$resources, file, row.names = FALSE)
  )
  output$dl_utilisation_csv <- downloadHandler(
    filename = "utilisation.csv",
    content  = function(file) write.csv(analysis_results()$ot_utilisation, file, row.names = FALSE)
  )
  output$dl_waiting_times_csv <- downloadHandler(
    filename = "waiting_times.csv",
    content  = function(file) {
      df <- analysis_results()$combined
      write.csv(df[, intersect(c("name", "start_time", "waiting_time", "priority"), names(df))],
                file, row.names = FALSE)
    }
  )

  calibration_df <- reactive({
    data.frame(
      Parameter    = MORRIS_LABELS[morris_params$name],
      `Lower Bound` = morris_params$lower,
      `Baseline`    = morris_params$mode,
      `Upper Bound` = morris_params$upper,
      check.names   = FALSE
    )
  })
  output$calibration_table <- renderDT({
    datatable(calibration_df(), rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })
  output$dl_calibration_csv <- downloadHandler(
    filename = "sensitivity_calibration.csv",
    content  = function(file) write.csv(calibration_df(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
