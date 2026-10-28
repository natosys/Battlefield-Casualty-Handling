##############################################
## R/app_server_configure.R               ##
## Server: Configure and Run panels       ##
##############################################
#
# The console state, the Configure panel's wiring (scenario selection and
# resolution, group bodies, previews and configuration transfer) and the Run
# panel's full-protocol command.
#
# Sourced by app.R after the packages and modules it loads; source that
# file rather than this one alone.

#' Read and validate the configuration the console starts from
#'
#' @return A list of `raw_env_data`, the reactive holding the loaded
#'   configuration, and `startup_json`, the configuration as read at startup.
create_console_state <- function() {
  # Both boundaries where a configuration enters the app validate it before
  # anything reads it: a malformed file names its offending field here rather
  # than surfacing as a subscript error further in. A malformed file on disk at
  # startup is unrecoverable within the session, so it stops; a malformed
  # upload leaves the loaded configuration in place and reports through the UI.
  startup_json <- fromJSON(DEFAULT_JSON, simplifyVector = FALSE)
  validate_env_data_json(startup_json, DEFAULT_JSON)

  list(raw_env_data = reactiveVal(startup_json), startup_json = startup_json)
}

#' Wire the Configure panel's field synchronisation and previews
#'
#' @param input See analyse_run().
#' @param output See analyse_run().
#' @param session See analyse_run().
#' @return A list of `fields_by_group`, `slider_field_ids`, `tri_mode_ids`.
wire_configure_panel <- function(input, output, session) {
  fields_by_group <- split(PARAM_REGISTRY, vapply(PARAM_REGISTRY, `[[`, character(1), "group"))

  # Every slider gets a paired "type an exact value" numeric box (see
  # field_input()/slider_with_text_input()); wire the two-way sync once per
  # id here rather than inside the per-group renderUI blocks, since the
  # sync only needs to exist once regardless of how many times the UI
  # containing it is regenerated. Triangular mode fields are excluded even
  # when they carry a plausible range (f$ranged == TRUE): render_field_grid() always
  # renders a detected triangular triple's min/mode/max as plain compact
  # inputs (tri_input_row()), never a slider, so there is no "<id>_txt"
  # companion for this sync to wire up.
  tri_mode_ids <- vapply(TRI_TRIPLES, `[[`, character(1), "mode_id")
  slider_field_ids <- vapply(
    Filter(function(f) (isTRUE(f$ranged) || isTRUE(f$slider)) && !(f$id %in% tri_mode_ids), PARAM_REGISTRY),
    function(f) f$id, character(1)
  )
  lapply(slider_field_ids, function(id) wire_slider_text_sync(input, session, id))
  wire_range_slider_text_sync(input, session, "pri_split")
  wire_range_slider_text_sync(input, session, "dnbi_split")
  wire_range_slider_text_sync(input, session, "mc_pri_split")
  lapply(seq_len(CASUALTY_SURGE_SCHEDULE_SLOTS), function(i) {
    wire_range_slider_text_sync(input, session, sprintf("mc_event_pri_split_%d", i))
  })

  # Live distribution-curve preview for every triangular (min/mode/max)
  # field group in the registry (see TRI_TRIPLES/render_field_grid()),
  # reading straight from the three inputs so it redraws as any of them
  # is edited — the same pattern as the casualty-generation curves above.
  # Left at Shiny's default suspendWhenHidden = TRUE: a curve
  # only needs to render once its accordion panel is actually opened, and
  # forcing it eager was part of what made every page load pay the cost of
  # rendering all 19 of these regardless of which panel (if any) the user
  # ever opens.
  lapply(names(TRI_TRIPLES), function(prefix) {
    tt <- TRI_TRIPLES[[prefix]]
    output_id <- paste0("tri_curve_", prefix)
    output[[output_id]] <- renderPlot({
      render_tri_curve(input[[tt$min_id]], input[[tt$mode_id]], input[[tt$max_id]])
    })
  })
  list(
    fields_by_group = fields_by_group,
    slider_field_ids = slider_field_ids,
    tri_mode_ids = tri_mode_ids
  )
}

#' Build the dropdown's display labels for each intensity profile
#'
#' @return A list of `SCENARIO_DROPDOWN_LABELS`, `shorten_scenario_label`.
build_scenario_labels <- function() {
  #' Trim a profile's parenthetical qualifier from its label
  #'
  #' @param lbl Full display label of an intensity profile.
  #' @return The label up to its first parenthesis, whitespace trimmed.
  shorten_scenario_label <- function(lbl) {
    trimws(sub("\\s*\\(.*$", "", lbl))
  }

  # The dropdown's own naming, overriding env_data.json's canonical scenario
  # labels for display only. The shipped default is the moderate-intensity
  # Falklands calibration; "high_intensity" overlays Okinawa casualty rates
  # and nothing else, the two sharing one mortality model. A scenario id absent
  # from this map falls back to its own env_data.json label.
  SCENARIO_DROPDOWN_LABELS <- c(
    default         = "Moderate Intensity",
    high_intensity  = "High Intensity"
  )
  list(
    SCENARIO_DROPDOWN_LABELS = SCENARIO_DROPDOWN_LABELS,
    shorten_scenario_label = shorten_scenario_label
  )
}

#' Wire the intensity profile dropdown and the configuration it resolves
#'
#' @param raw_env_data See wire_scenario_resolution().
#' @param input See wire_scenario_resolution().
#' @param output See wire_scenario_resolution().
#' @param SCENARIO_DROPDOWN_LABELS See wire_scenario_resolution().
#' @param shorten_scenario_label See wire_scenario_resolution().
#' @return A list of `current_scenario`, `scenario_choices`, `scenario_json`,
#'   `scenario_overridden_paths`.
wire_scenario_dropdown <- function(raw_env_data, input, output, SCENARIO_DROPDOWN_LABELS,
                                   shorten_scenario_label) {
  scenario_choices <- reactive({
    base <- raw_env_data()
    # An overlay that overrides nothing (the `moderate_intensity` alias of the
    # default) is not offered, so each option runs a distinct configuration.
    overlaid <- vapply(base$scenarios, function(sc) length(sc$vars) > 0, logical(1))
    ids  <- c("default", names(base$scenarios)[overlaid])
    labels <- vapply(ids, function(s) {
      if (s %in% names(SCENARIO_DROPDOWN_LABELS)) return(SCENARIO_DROPDOWN_LABELS[[s]])
      lbl <- base$scenarios[[s]]$label
      if (is.null(lbl)) s else shorten_scenario_label(lbl)
    }, character(1))
    setNames(ids, labels)
  })

  output$scenario_selector_ui <- renderUI({
    selectInput(
      "scenario_select",
      field_label(list(
        label = "Casualty Intensity Profile",
        tooltip = paste(
          "Applies a named profile's casualty and DOW parameters over the base",
          "configuration (structural fields are unaffected). Pick one to see what",
          "it changes below; see README Scenario Profiles for details."
        )
      )),
      choices  = scenario_choices(),
      selected = "default",
      selectize = FALSE
    )
  })
  # Only depends on raw_env_data() (via scenario_choices()), so re-renders
  # (resetting the widget to "default") only when a new file is loaded —
  # not on every scenario pick the user makes.
  outputOptions(output, "scenario_selector_ui", suspendWhenHidden = FALSE)

  current_scenario <- reactive({
    if (is.null(input$scenario_select)) "default" else input$scenario_select
  })

  # The scenario-resolved view of the configuration: what the Configure
  # panel's field defaults and Save Configuration both consume.
  # raw_env_data() itself stays pristine (only changes on initial load or
  # Load Configuration) so switching profiles is always computed fresh
  # rather than compounding onto a previously-overlaid state.
  scenario_json <- reactive({
    resolve_scenario(raw_env_data(), current_scenario())
  })

  # "elm.acty" paths the active profile overrides, e.g. "generators.wia_cbt".
  # Shared by the panel-level scope note and each affected field's own
  # tooltip (via field$path, set in R/app_params.R).
  scenario_overridden_paths <- reactive({
    scen <- current_scenario()
    if (identical(scen, "default")) return(character(0))
    scen_def <- raw_env_data()$scenarios[[scen]]
    if (is.null(scen_def)) return(character(0))
    unlist(lapply(scen_def$vars, function(elm) {
      vapply(elm$actys, function(acty) paste0(elm$elm, ".", acty$acty), character(1))
    }))
  })

  output$scenario_scope_note <- renderUI({
    paths <- scenario_overridden_paths()
    if (length(paths) == 0) return(NULL)
    div(class = "alert alert-info py-2 px-3 small",
        sprintf(
          "This profile overrides: %s (flagged with ⚠ on the affected fields below). All other fields — including force size, team/bed counts, and transport fleet — retain the base configuration's values.",
          paste(paths, collapse = ", ")
        ))
  })
  list(
    current_scenario = current_scenario,
    scenario_choices = scenario_choices,
    scenario_json = scenario_json,
    scenario_overridden_paths = scenario_overridden_paths
  )
}

#' Derive the distribution families and died-of-wounds shape a profile implies
#'
#' @param scenario_json See wire_scenario_resolution().
#' @return A list of `dow_shape`, `gen_distributions`.
build_scenario_derivations <- function(scenario_json) {
  gen_distributions <- reactive({
    setNames(vapply(GEN_STREAM_ACTYS, function(acty) {
      d <- get_raw_var(scenario_json(), "generators", acty, "distribution")
      if (is.null(d)) "lognormal" else d
    }, character(1)), GEN_STREAM_ACTYS)
  })

  # Fixed DOW logistic shape parameters (p_base, k, t_mid) per priority,
  # read from the resolved scenario JSON — not user-editable, but displayed
  # read-only alongside the P1/P2 DOW curve cards (render_dow_curve()) and
  # used to draw them, so the curve's floor/shape track whichever Casualty
  # Intensity Profile is active exactly as its ceiling (p_max) does. The
  # fitted post-operative hazard rates are displayed read-only beside them,
  # Priority 3's under its own key, having no curve card.
  dow_shape <- reactive({
    sj <- scenario_json()
    shapes <- setNames(lapply(c("p1", "p2"), function(pr) {
      list(
        p_base = get_raw_var(sj, "dow", "params", paste0(pr, "_p_base")),
        k      = get_raw_var(sj, "dow", "params", paste0(pr, "_k")),
        t_mid  = get_raw_var(sj, "dow", "params", paste0(pr, "_t_mid")),
        hazard = get_raw_var(sj, "dow", "post_op_hazard", paste0(pr, "_rate"))
      )
    }), c("p1", "p2"))
    shapes$p3 <- list(hazard = get_raw_var(sj, "dow", "post_op_hazard", "p3_rate"))
    shapes
  })
  list(
    dow_shape = dow_shape,
    gen_distributions = gen_distributions
  )
}

#' Wire the profile dropdown and the configuration it resolves
#'
#' @param raw_env_data See wire_scenario_selector().
#' @param input See wire_scenario_selector().
#' @param output See wire_scenario_selector().
#' @param SCENARIO_DROPDOWN_LABELS See wire_scenario_selector().
#' @param shorten_scenario_label See wire_scenario_selector().
#' @return A list of `current_scenario`, `dow_shape`, `gen_distributions`, `scenario_choices`,
#'   `scenario_json`, `scenario_overridden_paths`.
wire_scenario_resolution <- function(raw_env_data, input, output, SCENARIO_DROPDOWN_LABELS,
                                     shorten_scenario_label) {
  scenario_dropdown_out <- wire_scenario_dropdown(raw_env_data, input, output,
                                                  SCENARIO_DROPDOWN_LABELS, shorten_scenario_label)
  current_scenario <- scenario_dropdown_out$current_scenario
  scenario_choices <- scenario_dropdown_out$scenario_choices
  scenario_json <- scenario_dropdown_out$scenario_json
  scenario_overridden_paths <- scenario_dropdown_out$scenario_overridden_paths

  # Resolved distribution family per casualty-generation stream under the
  # active Casualty Intensity Profile — same lookup render_gen_curve()'s
  # own renderPlot() uses below, shared here so the Std. Dev. field itself
  # (not just its curve) reflects which streams are currently exponential.
  scenario_derivations_out <- build_scenario_derivations(scenario_json)
  dow_shape <- scenario_derivations_out$dow_shape
  gen_distributions <- scenario_derivations_out$gen_distributions
  list(
    current_scenario = current_scenario,
    dow_shape = dow_shape,
    gen_distributions = gen_distributions,
    scenario_choices = scenario_choices,
    scenario_json = scenario_json,
    scenario_overridden_paths = scenario_overridden_paths
  )
}

#' Wire the scheduled casualty surge event rows
#'
#' @param raw_env_data See wire_scenario_selector().
#' @param input See wire_scenario_selector().
#' @param session See wire_scenario_selector().
#' @return `mc_event_count`.
wire_casualty_surge_rows <- function(raw_env_data, input, session) {
  mc_event_count <- reactiveVal(
    max(1L, count_active_casualty_surge_events(isolate(raw_env_data())))
  )

  observeEvent(input$mc_event_add, {
    n <- mc_event_count()
    if (n < CASUALTY_SURGE_SCHEDULE_SLOTS) {
      n <- n + 1L
      mc_event_count(n)
      session$sendCustomMessage("mc_toggle_row", list(index = n, show = TRUE))
    }
  })
  observeEvent(input$mc_event_remove, {
    n <- mc_event_count()
    if (n > 1L) {
      # Reset the row about to be hidden back to its defaults — hiding via
      # display:none does not clear its inputs' values, and every Scheduled
      # Event Days field is always rendered/bound (see CASUALTY_SURGE_
      # SCHEDULE_SLOTS' own comment), so a hidden-but-nonzero Day would
      # otherwise be saved silently by Save Configuration.
      updateNumericInput(session, sprintf("mc_sched_day_%d", n),      value = 0)
      updateNumericInput(session, sprintf("mc_sched_prob_%d_txt", n), value = 1)
      updateSliderInput(session,  sprintf("mc_sched_prob_%d", n),     value = 1)
      updateNumericInput(session, sprintf("mc_sched_min_cas_%d", n),  value = 20)
      updateNumericInput(session, sprintf("mc_sched_max_cas_%d", n),  value = 60)
      updateSliderInput(session,  sprintf("mc_event_pri_split_%d", n), value = c(0.7, 0.9))
      session$sendCustomMessage("mc_toggle_row", list(index = n, show = FALSE))
      mc_event_count(n - 1L)
    }
  })
  mc_event_count
}

#' Wire each Configure accordion panel and its diagrams
#'
#' @param fields_by_group See wire_scenario_selector().
#' @param input See wire_scenario_selector().
#' @param output See wire_scenario_selector().
#' @param dow_shape See wire_scenario_selector().
#' @param gen_distributions See wire_scenario_selector().
#' @param scenario_json See wire_scenario_selector().
#' @param scenario_overridden_paths See wire_scenario_selector().
#' @return Invisibly NULL; called for the outputs it registers.
wire_configure_group_bodies <- function(fields_by_group, input, output, dow_shape,
                                        gen_distributions, scenario_json,
                                        scenario_overridden_paths) {
  lapply(names(fields_by_group), function(g) {
    output_id <- paste0("group_ui_", make.names(g))
    output[[output_id]] <- renderUI({
      defaults <- registry_defaults(fields_by_group[[g]], scenario_json())
      render_group_body(fields_by_group[[g]], defaults, scenario_overridden_paths(),
                        gen_distributions(), dow_shape())
    })
    # Left at Shiny's default suspendWhenHidden = TRUE: a panel other than
    # the initially-open one renders only once actually opened, rather
    # than every panel's ~20 fields and curve previews all rendering and
    # registering their initial values at once on every page load. Values
    # for a never-opened panel's fields (never bound, so absent from
    # `input`) are supplied by apply_registry_values()'s existing fallback
    # to scenario_json() (see current_json()) — every field remains
    # correctly captured by Save Configuration regardless of which panels
    # are open, exactly as
    # eager rendering was there to guarantee, just without needing every
    # field's input to actually exist in `input` to do it.
  })

  # Live structural capacity diagram at the top of the Health System
  # Architecture panel
  # (see force_structure_diagram()) — reads the same team-count/bed-count
  # inputs the fields below it edit, so it stays in sync automatically as
  # they change. Bed fields only exist for R2B/R2E (R1 has none).
  output$force_design_diagram <- renderUI({
    r2b_beds <- c(
      OT    = input$r2b_bed_ot    %||% 0,
      Resus = input$r2b_bed_resus %||% 0,
      ICU   = input$r2b_bed_icu   %||% 0,
      Hold  = input$r2b_bed_hold  %||% 0
    )
    r2e_beds <- c(
      OT    = input$r2e_bed_ot    %||% 0,
      Resus = input$r2e_bed_resus %||% 0,
      ICU   = input$r2e_bed_icu   %||% 0,
      Hold  = input$r2e_bed_hold  %||% 0
    )
    force_structure_diagram(
      input$r1_team_count  %||% 0, input$r2b_team_count %||% 0, r2b_beds,
      input$r2e_team_count %||% 0, r2e_beds
    )
  })
  # Left at the default suspendWhenHidden = TRUE: this lives in
  # the same accordion panel as the Health System Architecture fields it
  # illustrates, so there is nothing to show while that panel is collapsed.

  # Live medevac chain diagram at the top of the Medevac panel
  # (see medevac_diagram()) — reads the same transport mode inputs the
  # fields below it edit. All four legs carry an unmodified dead-heading
  # return leg; R2E's own local mortuary transfer (mort2e_mode)
  # remains a marker, not a leg, since no vehicle asset is used for that
  # final in-place movement.
  output$medevac_diagram <- renderUI({
    # The airframe selector picks which pair of capacity fields the AME leg
    # labels read, so the diagram follows a change of aircraft without the
    # user having to re-run anything.
    airframe <- input$ame_airframe %||% "c17a"
    airframe_labels <- c(c17a  = "C-17A Globemaster III",
                         c130j = "C-130J-30 Hercules",
                         c27j  = "C-27J Spartan")
    medevac_diagram(
      wia1_mode = input$r1_wia_transport_mode  %||% 0,
      kia1_mode = input$r1_kia_transport_mode  %||% 0,
      wia2_mode = input$r2b_wia_transport_mode %||% 0,
      kia2_mode = input$r2b_kia_transport_mode %||% 0,
      mort2e_mode = input$r2e_kia_transport_mode %||% 0,
      ame_critical            = input[[paste0("ame_", airframe, "_critical")]] %||% 0,
      ame_standard            = input[[paste0("ame_", airframe, "_standard")]] %||% 0,
      ame_airframe_label      = unname(airframe_labels[airframe]),
      ame_schedule_interval   = input$ame_schedule_interval   %||% 0,
      ame_failure_probability = input$ame_failure_probability %||% 0
    )
  })
  invisible(NULL)
}

#' Wire the casualty generation and died-of-wounds curve previews
#'
#' @param input See wire_scenario_selector().
#' @param output See wire_scenario_selector().
#' @param dow_shape See wire_scenario_selector().
#' @param scenario_json See wire_scenario_selector().
#' @return Invisibly NULL; called for the outputs it registers.
wire_configure_previews <- function(input, output, dow_shape, scenario_json) {
  lapply(GEN_STREAM_ACTYS, function(acty) {
    output_id <- paste0("curve_", acty)
    output[[output_id]] <- renderPlot({
      mean_v <- input[[paste0("gen_", acty, "_mean")]]
      sd_v   <- input[[paste0("gen_", acty, "_sd")]]
      dist   <- get_raw_var(scenario_json(), "generators", acty, "distribution")
      if (is.null(dist)) dist <- "lognormal"
      render_gen_curve(mean_v, sd_v, dist)
    })
    # Left at the default suspendWhenHidden = TRUE — same
    # reasoning as the triangular curve previews above.
  })

  # Live DOW survival-function curve previews (Casualty Rates group, Died
  # of Wounds Ceilings subgroup). p_max is read live from the P1/P2
  # Configure slider so the curve's ceiling redraws as it is dragged;
  # p_base/k/t_mid come from dow_shape() (scenario-resolved, not
  # user-editable), matching the same live/fixed split used for the
  # casualty-generation curves above.
  lapply(c("p1", "p2"), function(pr) {
    output_id <- paste0("dow_curve_", pr)
    output[[output_id]] <- renderPlot({
      p_max <- input[[paste0("dow_", pr, "_pmax")]]
      shp   <- dow_shape()[[pr]]
      render_dow_curve(shp$p_base, p_max, shp$k, shp$t_mid)
    })
    # Left at the default suspendWhenHidden = TRUE — same
    # reasoning as the triangular curve previews above.
  })
  invisible(NULL)
}

#' Wire loading a configuration and saving the current one
#'
#' @param raw_env_data See wire_scenario_selector().
#' @param input See wire_scenario_selector().
#' @param output See wire_scenario_selector().
#' @param scenario_json See wire_scenario_selector().
#' @return `current_json`.
wire_configuration_transfer <- function(raw_env_data, input, output, scenario_json) {
  observeEvent(input$upload_json, {
    req(input$upload_json)
    tryCatch({
      new_json <- fromJSON(input$upload_json$datapath, simplifyVector = FALSE)
      validate_env_data_json(new_json, input$upload_json$name)
      raw_env_data(new_json)
      showNotification("Configuration loaded.", type = "message")
    }, error = function(e) {
      showNotification(paste("Failed to load configuration:", conditionMessage(e)), type = "error")
    })
  })

  current_json <- reactive({
    apply_registry_values(PARAM_REGISTRY, scenario_json(), inject_all_splits(reactiveValuesToList(input)))
  })

  output$download_json <- downloadHandler(
    filename = function() sanitise_config_filename(input$config_filename),
    content  = function(file) write_json(current_json(), file, pretty = TRUE, auto_unbox = TRUE)
  )
  current_json
}

#' Wire the casualty intensity profile selector and the configuration it resolves
#'
#' @param fields_by_group See analyse_run().
#' @param raw_env_data See analyse_run().
#' @param input See analyse_run().
#' @param output See analyse_run().
#' @param session See analyse_run().
#' @return A list of `current_json`, `current_scenario`, `dow_shape`, `gen_distributions`,
#'   `mc_event_count`, `scenario_choices`, `SCENARIO_DROPDOWN_LABELS`, `scenario_json`,
#'   `scenario_overridden_paths`, `shorten_scenario_label`.
wire_scenario_selector <- function(fields_by_group, raw_env_data, input, output, session) {
  scenario_labels_out <- build_scenario_labels()
  SCENARIO_DROPDOWN_LABELS <- scenario_labels_out$SCENARIO_DROPDOWN_LABELS
  shorten_scenario_label <- scenario_labels_out$shorten_scenario_label

  scenario_resolution_out <- wire_scenario_resolution(raw_env_data, input, output,
                                                      SCENARIO_DROPDOWN_LABELS,
                                                      shorten_scenario_label)
  current_scenario <- scenario_resolution_out$current_scenario
  dow_shape <- scenario_resolution_out$dow_shape
  gen_distributions <- scenario_resolution_out$gen_distributions
  scenario_choices <- scenario_resolution_out$scenario_choices
  scenario_json <- scenario_resolution_out$scenario_json
  scenario_overridden_paths <- scenario_resolution_out$scenario_overridden_paths

  # Number of Scheduled Event Days rows currently visible in the Mass
  # Casualty panel (see render_group_body()'s "Scheduled Event Days" case).
  # Seeded from however many slots already have a non-zero Day in the
  # loaded config (at least 1, so a fresh/default config shows one empty
  # starter row rather than none) — raw_env_data() is a plain reactiveVal
  # seeded synchronously at app startup, so isolate() here just avoids
  # taking an unnecessary reactive dependency on it for this one-time read.
  # Tracks how many Scheduled Event Days rows are currently visible. This is
  # internal server state only — it is never read inside a renderUI, so
  # changing it does not invalidate (and re-render from JSON defaults) the
  # Casualty Surge group body. Visibility itself is toggled client-side via
  # the "mc_toggle_row" custom message (handler registered once in
  # render_group_body()'s "Scheduled Event Days" case).
  mc_event_count <- wire_casualty_surge_rows(raw_env_data, input, session)

  wire_configure_group_bodies(fields_by_group, input, output, dow_shape, gen_distributions,
                              scenario_json, scenario_overridden_paths)
  # Left at the default suspendWhenHidden = TRUE — same
  # reasoning as force_design_diagram above, for the Medevac panel.

  # Live casualty-generation curve previews (Casualty Rates group). Read
  # live from the mean/sd inputs so the curve redraws as the user edits
  # them; the distribution family comes from scenario_json() (not itself
  # user-editable — see README Scenario Profiles for why picking a family
  # independently of a sourced rate would be uncited guesswork).
  wire_configure_previews(input, output, dow_shape, scenario_json)

  current_json <- wire_configuration_transfer(raw_env_data, input, output, scenario_json)
  list(
    current_json = current_json,
    current_scenario = current_scenario,
    dow_shape = dow_shape,
    gen_distributions = gen_distributions,
    mc_event_count = mc_event_count,
    scenario_choices = scenario_choices,
    SCENARIO_DROPDOWN_LABELS = SCENARIO_DROPDOWN_LABELS,
    scenario_json = scenario_json,
    scenario_overridden_paths = scenario_overridden_paths,
    shorten_scenario_label = shorten_scenario_label
  )
}

#' Wire the hand-off card that shows the full protocol command
#'
#' @param input See wire_run_status().
#' @param output See wire_run_status().
#' @return Invisibly NULL; called for the outputs it registers.
#' @details The command names the file Save Configuration will download, which
#'   follows the same file name box, so saving and then pasting the command is
#'   the whole procedure. The copy button writes the command to the clipboard in
#'   the browser; the text stays selectable where the clipboard is unavailable.
wire_full_protocol_command <- function(input, output) {
  output$full_protocol_command_ui <- renderUI({
    cmd <- full_protocol_command(input$config_filename, input$seed)
    tagList(
      tags$pre(id = "full_protocol_command", class = "p-2 border rounded", cmd),
      tags$button(type = "button", class = "btn btn-outline-secondary btn-sm",
                  onclick = "navigator.clipboard && navigator.clipboard.writeText(
                    document.getElementById('full_protocol_command').innerText);",
                  "Copy command")
    )
  })
  invisible(NULL)
}
