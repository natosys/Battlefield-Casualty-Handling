##############################################
## R/app_ui_fields.R                      ##
## Configure panel field builders         ##
##############################################
#
# The input widgets and cards the Configure panel is built from, and the
# renderers for each parameter group's body, its splits, rates, ceilings
# and casualty surge events.
#
# Sourced by app.R after the packages and modules it loads; source that
# file rather than this one alone.

#' Build a field's label, flagging one the active profile overrides
#'
#' @param f A field spec, or a list carrying its label, tooltip and path.
#' @param overridden_paths Character vector of "elm.acty" paths the active
#'   Casualty Intensity Profile is currently overriding (from
#'   scenario_overridden_paths()); NULL/empty under the default profile.
#' @return A tooltip-wrapped label carrying the information icon, or the
#'   warning icon and an added note where the field is overridden.
field_label <- function(f, overridden_paths = NULL) {
  is_overridden <- !is.null(f$path) && f$path %in% overridden_paths
  tt <- if (is_overridden) {
    paste0(f$tooltip, " ⚠ Currently overridden by the selected Casualty Intensity Profile — see the panel note above.")
  } else {
    f$tooltip
  }
  icon <- if (is_overridden) {
    tags$span(style = "color:#c0392b; cursor:help; font-weight:bold;", HTML("&nbsp;&#9888;"))
  } else {
    tags$span(style = "color:#888; cursor:help;", HTML("&nbsp;&#9432;"))
  }
  tooltip(
    tags$span(f$label, icon),
    tt,
    placement = "right"
  )
}

#' Render a small, visually inert numeric display — a real `<input
#' disabled>` element styled to match the surrounding form controls, not an
#' editable Shiny widget (no `input$id` is ever registered for it). Used
#' for constants shown for context (e.g. the DOW logistic shape parameters)
#' that are not exposed as editable Configure fields.
#'
#' @param label,tooltip_text Short label and hover tooltip
#'   (provenance/citation).
#' @param value Numeric value to display; formatted with `digits` decimals.
#' @param digits Decimal places the value is rounded to.
#' @param path "elm.acty" path for the scenario-override flag, matching
#'   field_label()'s convention — pass the same path an editable sibling
#'   field in the same acty block would use, so this constant is flagged
#'   consistently when a scenario overrides the block it lives in.
#' @param overridden_paths Character vector of paths the active profile
#'   overrides, as field_label() reads it.
#' @return A division holding the disabled input and its label.
readonly_numeric <- function(label, tooltip_text, value, digits = 3,
                             path = NULL, overridden_paths = NULL) {
  lbl <- field_label(list(label = label, tooltip = tooltip_text, path = path), overridden_paths)
  tags$div(class = "form-group mb-0",
    tags$label(class = "small mb-0", lbl),
    tags$input(type = "number", class = "form-control form-control-sm",
               value = if (is.null(value) || is.na(value)) "" else round(value, digits),
               disabled = NA, style = "background-color:#eee; color:#555;")
  )
}

#' Pair a single-value slider with a small numeric box for typed entry.
#' The slider (`id`) remains the authoritative reactive value everything
#' else in the app reads via `input[[id]]`; the numeric box (`<id>_txt`)
#' is a secondary UI-only control kept in sync by wire_slider_text_sync()
#' (registered once at server startup — see server()). The field's own
#' label sits beside the numeric box rather than above the slider, since
#' sliderInput(label = NULL) omits its built-in label row entirely.
#'
#' @param id Input id of the slider, which the numeric box extends.
#' @param lbl Label element shown beside the numeric box.
#' @param min Lower bound of both controls.
#' @param max Upper bound of both controls.
#' @param value Initial value of both controls.
#' @param step Step size of both controls.
#' @return A tag list of the label, the numeric box and the slider.
slider_with_text_input <- function(id, lbl, min, max, value, step) {
  tagList(
    div(style = "display:flex; justify-content:space-between; align-items:flex-end; gap:8px;",
        div(style = "flex:1; min-width:0;", lbl),
        numericInput(paste0(id, "_txt"), label = NULL, value = value,
                     min = min, max = max, step = step, width = "80px")
    ),
    sliderInput(id, label = NULL, min = min, max = max, value = value, step = step, width = "100%")
  )
}

#' As slider_with_text_input(), for a two-handle range slider: one small
#' numeric box per handle (`<id>_txt1`, `<id>_txt2`), matching the two
#' breakpoint values already shown in the slider's own handle bubbles.
#'
#' @param id Input id of the range slider, which the two boxes extend.
#' @param lbl Label element shown beside the numeric boxes.
#' @param value Two-element vector of the initial breakpoints.
#' @param min Lower bound of every control.
#' @param max Upper bound of every control.
#' @param step Step size of every control.
#' @return A tag list of the label, the two numeric boxes and the slider.
range_slider_with_text_input <- function(id, lbl, value, min = 0, max = 1, step = 0.01) {
  tagList(
    div(style = "display:flex; justify-content:space-between; align-items:flex-end; gap:8px;",
        div(style = "flex:1; min-width:0;", lbl),
        div(style = "display:flex; gap:4px;",
            numericInput(paste0(id, "_txt1"), label = NULL, value = value[1],
                         min = min, max = max, step = step, width = "70px"),
            numericInput(paste0(id, "_txt2"), label = NULL, value = value[2],
                         min = min, max = max, step = step, width = "70px")
        )
    ),
    sliderInput(id, label = NULL, min = min, max = max, step = step, value = value, width = "100%")
  )
}

#' Wire bidirectional sync between a single-value slider (`id`) and its
#' paired "type an exact value" numeric input (`<id>_txt`, see
#' slider_with_text_input()). Registered once at server startup for every
#' slider field — Shiny inputs persist by id regardless of which renderUI
#' call currently has them in the DOM, so this does not need to be re-run
#' when a group's UI regenerates (e.g. on a Casualty Intensity Profile
#' change). The equality check on each side breaks the update loop: an
#' update that didn't actually change the value doesn't trigger another.
#'
#' @param input Shiny input object.
#' @param session Shiny session object.
#' @param id Input id of the slider.
#' @return Invisibly NULL; called for the two observers it registers.
wire_slider_text_sync <- function(input, session, id) {
  txt_id <- paste0(id, "_txt")
  observeEvent(input[[txt_id]], {
    v <- input[[txt_id]]; cur <- input[[id]]
    if (!is.null(v) && !is.na(v) && (is.null(cur) || !isTRUE(all.equal(v, cur)))) {
      updateSliderInput(session, id, value = v)
    }
  }, ignoreInit = TRUE)
  observeEvent(input[[id]], {
    v <- input[[id]]; cur <- input[[txt_id]]
    if (!is.null(v) && !is.na(v) && (is.null(cur) || !isTRUE(all.equal(v, cur)))) {
      updateNumericInput(session, txt_id, value = v)
    }
  }, ignoreInit = TRUE)
}

#' As wire_slider_text_sync(), for a two-handle range slider paired with
#' two numeric inputs (`<id>_txt1`, `<id>_txt2`, see
#' range_slider_with_text_input()) — one per breakpoint.
#'
#' @param input Shiny input object.
#' @param session Shiny session object.
#' @param id Input id of the range slider.
#' @return Invisibly NULL; called for the observers it registers.
wire_range_slider_text_sync <- function(input, session, id) {
  txt1 <- paste0(id, "_txt1"); txt2 <- paste0(id, "_txt2")
  observeEvent(input[[txt1]], {
    v <- input[[txt1]]; cur <- input[[id]]
    if (!is.null(v) && !is.na(v) && !is.null(cur) && !isTRUE(all.equal(v, cur[1]))) {
      updateSliderInput(session, id, value = c(v, cur[2]))
    }
  }, ignoreInit = TRUE)
  observeEvent(input[[txt2]], {
    v <- input[[txt2]]; cur <- input[[id]]
    if (!is.null(v) && !is.na(v) && !is.null(cur) && !isTRUE(all.equal(v, cur[2]))) {
      updateSliderInput(session, id, value = c(cur[1], v))
    }
  }, ignoreInit = TRUE)
  observeEvent(input[[id]], {
    v <- input[[id]]
    if (!is.null(v) && length(v) == 2) {
      cur1 <- input[[txt1]]; cur2 <- input[[txt2]]
      if (is.null(cur1) || !isTRUE(all.equal(v[1], cur1))) updateNumericInput(session, txt1, value = v[1])
      if (is.null(cur2) || !isTRUE(all.equal(v[2], cur2))) updateNumericInput(session, txt2, value = v[2])
    }
  }, ignoreInit = TRUE)
}

#' Render one registry field as its Shiny input control
#'
#' @param f A field spec from the parameter registry.
#' @param value The field's current value, used as the control's default.
#' @param overridden_paths Character vector of vars-tree paths the active
#'   scenario overrides, used to flag the label.
#' @return A Shiny input control: a dropdown, a slider or a numeric box,
#'   according to the field's own type and bounds.
field_input <- function(f, value, overridden_paths = NULL) {
  lbl <- field_label(f, overridden_paths)
  if (!is.null(f$choices)) {
    selectInput(f$id, lbl, choices = f$choices, selected = value)
  } else if (isTRUE(f$morris) || isTRUE(f$slider)) {
    # Defensive widening: the current env_data.json baseline should always
    # be representable on the slider, even where a screened Morris range
    # (R/sensitivity.R) has drifted from the baseline after a later
    # recalibration — flagged as a data-consistency check
    # rather than silently clipping here.
    lo <- f$min; hi <- f$max
    if (!is.null(value) && !is.na(value)) {
      lo <- min(lo, value)
      hi <- max(hi, value)
    }
    step <- if (identical(f$type, "integer")) 1 else f$step
    slider_with_text_input(f$id, lbl, lo, hi, value, step)
  } else if (identical(f$type, "integer")) {
    numericInput(f$id, lbl, value = value, min = f$min, max = f$max, step = 1)
  } else {
    numericInput(f$id, lbl, value = value, min = f$min, max = f$max, step = f$step)
  }
}

#' Wrap a single field's input in a bordered card tile, matching the visual
#' structure used for the Casualty Generation Rates curve cards (see
#' render_group_body()) so every Configure field — not just the curve
#' previews — reads as a distinct tile in a responsive grid rather than a
#' bare label/widget pair floating in a flex-wrap row.
#'
#' @param f A field spec from the parameter registry.
#' @param value The field's current value.
#' @param overridden_paths Character vector of paths the active profile
#'   overrides, used to flag the label.
#' @return A card holding the field's input control.
field_card <- function(f, value, overridden_paths = NULL) {
  card(field_input(f, value, overridden_paths))
}

#' Render a triangular field triple's min/mode/max as three small plain
#' numeric inputs side by side, rather than three full-width rows — the
#' mode field's own tooltip (still stating its Morris-screened range and
#' source, where applicable) is preserved via field_label(), just with a
#' short "Min"/"Mode"/"Max" visible label instead of the field's full
#' descriptive name, since that's already shown once in the card header.
#' Always plain numericInput()s (never a slider, even for a
#' Morris-screened mode field) so all three fit uniformly in one narrow
#' row under the curve.
#'
#' @param min_f Field spec of the distribution's minimum.
#' @param mode_f Field spec of the distribution's mode.
#' @param max_f Field spec of the distribution's maximum.
#' @param defaults Named list of current values, keyed by field id.
#' @param overridden_paths Character vector of vars-tree paths the active
#'   scenario overrides, used to flag each label.
#' @return A Shiny row of three numeric inputs.
tri_input_row <- function(min_f, mode_f, max_f, defaults, overridden_paths = NULL) {
  #' Render one vertex of the triple as a narrow numeric input
  #'
  #' @param f Field spec of the vertex.
  #' @param short_label Visible label, "Min", "Mode" or "Max".
  #' @return A Shiny numeric input.
  compact_input <- function(f, short_label) {
    lbl <- field_label(list(label = short_label, tooltip = f$tooltip, path = f$path), overridden_paths)
    numericInput(f$id, lbl, value = defaults[[f$id]], min = f$min, max = f$max,
                 step = if (identical(f$type, "integer")) 1 else f$step, width = "100%")
  }
  div(style = "display:flex; gap:6px;",
      div(style = "flex:1; min-width:0;", compact_input(min_f,  "Min")),
      div(style = "flex:1; min-width:0;", compact_input(mode_f, "Mode")),
      div(style = "flex:1; min-width:0;", compact_input(max_f,  "Max"))
  )
}

#' Render a set of fields as a responsive grid of cards
#'
#' @param fields List of field specs to render.
#' @param defaults Named list of current values, keyed by field id.
#' @param overridden_paths Character vector of vars-tree paths the active
#'   scenario overrides, used to flag each label.
#' @param width CSS minimum column width for the grid.
#' @return A Shiny division holding one card per field or field triple.
#' @details A triangular (min/mode/max) triple is detected through
#'   TRI_TRIPLES and rendered as one curve card, the plot above the three
#'   inputs, matching the Casualty Generation Rates pattern; any field not
#'   part of a detected triple renders as a plain field card, so a subgroup
#'   carrying no triangular fields renders as a plain grid.
render_field_grid <- function(fields, defaults, overridden_paths = NULL, width = "300px") {
  ids  <- vapply(fields, function(f) f$id, character(1))
  used <- character(0)
  items <- list()
  for (f in fields) {
    if (f$id %in% used) next
    prefix <- if (grepl("_min$", f$id)) sub("_min$", "", f$id) else NA_character_
    tt <- if (!is.na(prefix)) TRI_TRIPLES[[prefix]] else NULL
    if (!is.null(tt) && tt$mode_id %in% ids && tt$max_id %in% ids) {
      min_f  <- Find(function(x) identical(x$id, tt$min_id),  fields)
      mode_f <- Find(function(x) identical(x$id, tt$mode_id), fields)
      max_f  <- Find(function(x) identical(x$id, tt$max_id),  fields)
      items[[length(items) + 1]] <- card(
        card_header(tt$label),
        plotOutput(paste0("tri_curve_", prefix), height = "90px"),
        tri_input_row(min_f, mode_f, max_f, defaults, overridden_paths)
      )
      used <- c(used, tt$min_id, tt$mode_id, tt$max_id)
    } else {
      items[[length(items) + 1]] <- field_card(f, defaults[[f$id]], overridden_paths)
    }
  }
  layout_column_wrap(width = width, !!!items)
}

#' NULL-coalesce (base R only gained `%||%` in 4.4; this app targets 4.3).
#'
#' @param x Value to return when it is neither NULL nor NA.
#' @param y Fallback value.
#' @return `x` where it is usable, `y` otherwise.
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

#' Render the Triage Priority Split subgroup as one shared card
#'
#' @param sg See render_group_body().
#' @param sg_fields See render_group_body().
#' @param fields See render_group_body().
#' @param subgroups See render_group_body().
#' @param defaults See render_group_body().
#' @param overridden_paths See render_group_body().
#' @return A shiny tag or tag list for this subgroup.
#' @details Priority 1, 2 and 3 share a single two-handle range slider, so the three
#'   shares sum to one by construction rather than by validation. The DNBI split
#'   is drawn into the same card, which is why this builder reads the whole field
#'   set rather than its own subgroup's alone.
render_triage_split_subgroup <- function(sg, sg_fields, fields, subgroups, defaults,
                                         overridden_paths) {
  one_f <- Find(function(f) identical(f$id, "pri_one"), sg_fields)
  p1 <- defaults[["pri_one"]]; p2 <- defaults[["pri_two"]]
  pri_lbl <- field_label(list(
    label   = "Priority Split (P1 | P2 | P3)",
    tooltip = paste0(
      "Drag either handle to reallocate share between adjacent priorities — ",
      "Priority 1, 2, and 3 always sum to 100% by construction."
    ),
    path = one_f$path
  ), overridden_paths)
  pri_card <- card(
    card_header(sg),
    range_slider_with_text_input("pri_split", pri_lbl, c(p1, p1 + p2))
  )

  dnbi_fields <- fields[subgroups == "DNBI Sub-Type Split"]
  dnbi_card <- NULL
  if (length(dnbi_fields) > 0) {
    bf_f  <- Find(function(f) identical(f$id, "dnbi_bf_pct"), dnbi_fields)
    p_bf  <- defaults[["dnbi_bf_pct"]]; p_dis <- defaults[["dnbi_disease_pct"]]
    dnbi_lbl <- field_label(list(
      label   = "DNBI Split (Battle Fatigue | Disease | NBI)",
      tooltip = paste0(
        "Drag either handle to reallocate share between adjacent DNBI sub-types — ",
        "Battle Fatigue, Disease, and Non-Battle Injury always sum to 100% by construction."
      ),
      path = bf_f$path
    ), overridden_paths)
    dnbi_card <- card(
      card_header("DNBI Sub-Type Split"),
      range_slider_with_text_input("dnbi_split", dnbi_lbl, c(p_bf, p_bf + p_dis))
    )
  }

  return(layout_column_wrap(width = "480px", pri_card, dnbi_card))
}

#' Render the DNBI Sub-Type Split subgroup
#'
#' @return A shiny tag or tag list for this subgroup.
#' @details Returns nothing of its own: the split is drawn inside the Triage Priority
#'   Split card above, so the two sliders can share a row.
render_dnbi_split_subgroup <- function() {
  return(NULL)
}

#' Render the Casualty Generation Rates subgroup with its distribution previews
#'
#' @param sg See render_group_body().
#' @param sg_fields See render_group_body().
#' @param defaults See render_group_body().
#' @param overridden_paths See render_group_body().
#' @param gen_distributions See render_group_body().
#' @return A shiny tag or tag list for this subgroup.
render_generation_rates_subgroup <- function(sg, sg_fields, defaults, overridden_paths,
                                             gen_distributions) {
  return(tagList(
    h6(class = "text-muted mt-2", sg),
    p(class = "text-muted small",
      "These define a distribution shape, not a fixed value — the shaded curve shows daily ",
      "casualty-rate variability (dashed line = mean) and updates live as you edit. Streams using ",
      "an exponential distribution take only a mean, so Std. Dev. is hidden for those."),
    layout_column_wrap(
      width = "320px",
      !!!lapply(GEN_STREAM_ACTYS, function(acty) {
        mean_f <- Find(function(f) identical(f$id, paste0("gen_", acty, "_mean")), sg_fields)
        sd_f   <- Find(function(f) identical(f$id, paste0("gen_", acty, "_sd")),   sg_fields)
        # useBytes: see detect_tri_triples() above
        stream_label <- sub(" — Mean Daily Rate$", "", mean_f$label, useBytes = TRUE)
        dist <- if (is.null(gen_distributions)) "lognormal" else (gen_distributions[[acty]] %||% "lognormal")
        sd_input <- if (identical(dist, "exponential")) {
          p(class = "text-muted small mb-0",
            "Std. Dev. not applicable — exponential distribution, fully described by the mean.")
        } else {
          field_input(sd_f, defaults[[sd_f$id]], overridden_paths)
        }
        card(
          card_header(stream_label),
          plotOutput(paste0("curve_", acty), height = "110px"),
          field_input(mean_f, defaults[[mean_f$id]], overridden_paths),
          sd_input
        )
      })
    )
  ))
}

#' Render the Died of Wounds Ceilings subgroup with its logistic curve previews
#'
#' @param sg See render_group_body().
#' @param sg_fields See render_group_body().
#' @param defaults See render_group_body().
#' @param overridden_paths See render_group_body().
#' @param dow_shape See render_group_body().
#' @return A shiny tag or tag list for this subgroup.
render_dow_ceilings_subgroup <- function(sg, sg_fields, defaults, overridden_paths, dow_shape) {
  return(tagList(
    h6(class = "text-muted mt-2", sg),
    p(class = "text-muted small",
      "The ceiling below is the only editable value; the curve shows the full survival function evaluated at each DOW check, using the fixed shape parameters (p_base, k, t_mid) shown alongside it."),
    layout_column_wrap(
      width = "340px",
      !!!lapply(list(
        list(prio = "p1", id = "dow_p1_pmax", label = "Priority 1 (Urgent)"),
        list(prio = "p2", id = "dow_p2_pmax", label = "Priority 2 (Priority)")
      ), function(pr) {
        pmax_f <- Find(function(f) identical(f$id, pr$id), sg_fields)
        shp <- if (is.null(dow_shape)) NULL else dow_shape[[pr$prio]]
        card(
          card_header(pr$label),
          plotOutput(paste0("dow_curve_", pr$prio), height = "110px"),
          field_input(pmax_f, defaults[[pmax_f$id]], overridden_paths),
          div(style = "display:flex; gap:6px; margin-top:4px;",
            div(style = "flex:1; min-width:0;",
                readonly_numeric("p_base", "Baseline (time-zero) DOW probability in the logistic curve.", shp$p_base, digits = 4,
                                 path = "dow.params", overridden_paths = overridden_paths)),
            div(style = "flex:1; min-width:0;",
                readonly_numeric("k", "Logistic curve steepness constant.", shp$k, digits = 3,
                                 path = "dow.params", overridden_paths = overridden_paths)),
            div(style = "flex:1; min-width:0;",
                readonly_numeric("t_mid", "Logistic curve's midpoint time, minutes.", shp$t_mid, digits = 0,
                                 path = "dow.params", overridden_paths = overridden_paths))
          )
        )
      })
    )
  ))
}

#' Render the Random Event Rate subgroup, shown in Poisson mode alone
#'
#' @param sg See render_group_body().
#' @param sg_fields See render_group_body().
#' @param defaults See render_group_body().
#' @param overridden_paths See render_group_body().
#' @return A shiny tag or tag list for this subgroup.
render_random_event_rate_subgroup <- function(sg, sg_fields, defaults, overridden_paths) {
  return(conditionalPanel(
    condition = "input.mc_mode == 'poisson'",
    tagList(
      h6(class = "text-muted mt-2", sg),
      p(class = "text-muted small",
        "Event timing is drawn from a Poisson process at this rate, varying between replications. All events share the casualty-count range below; use Scheduled mode for per-event control."),
      render_field_grid(sg_fields, defaults, overridden_paths)
    )
  ))
}

#' Render the Scheduled Event Days subgroup, shown in scheduled mode alone
#'
#' @param sg See render_group_body().
#' @param sg_fields See render_group_body().
#' @param defaults See render_group_body().
#' @param overridden_paths See render_group_body().
#' @return A shiny tag or tag list for this subgroup.
render_scheduled_events_subgroup <- function(sg, sg_fields, defaults, overridden_paths) {
  # Initial visible-row count only — a one-time value read from this
  # render's defaults, not a reactive dependency. All 20 slots are
  # always present in the DOM; the +/- buttons (server observeEvent
  # handlers, see mc_event_count in the server) toggle a single row's
  # display via a custom message + the client-side handler registered
  # below, rather than triggering a re-render of this whole group —
  # re-rendering the group here would also rebuild the Event Timing
  # Mode dropdown and every other Casualty Surge field at its JSON
  # default, discarding whatever the user currently has live in the UI.
  mc_count <- max(1L, sum(vapply(seq_len(CASUALTY_SURGE_SCHEDULE_SLOTS), function(i) {
    d <- defaults[[sprintf("mc_sched_day_%d", i)]]
    !is.null(d) && !is.na(d) && d > 0
  }, logical(1))))
  return(conditionalPanel(
    condition = "input.mc_mode == 'scheduled'",
    tagList(
      h6(class = "text-muted mt-2", sg),
      p(class = "text-muted small",
        sprintf(
          "Each event fires independently at its own probability (1 = always) and has its own casualty count and priority mix. Use +/− to add or remove rows (up to %d); removing resets a row rather than deleting it. For more events, edit env_data.json directly.",
          CASUALTY_SURGE_SCHEDULE_SLOTS
        )),
      tags$script(HTML(
        "if (!window.__mcToggleHandlerRegistered) {
           window.__mcToggleHandlerRegistered = true;
           Shiny.addCustomMessageHandler('mc_toggle_row', function(msg) {
             var el = document.getElementById('mc_event_slot_' + msg.index);
             if (el) el.style.display = msg.show ? '' : 'none';
           });
         }"
      )),
      div(style = "display:flex; gap:8px; margin-bottom:10px;",
          actionButton("mc_event_add",    "+ Add Event",           class = "btn-outline-primary btn-sm"),
          actionButton("mc_event_remove", "− Remove Last Event", class = "btn-outline-secondary btn-sm")
      ),
      # A plain flex-wrap container rather than layout_column_wrap():
      # bslib's grid locks in a fixed number of explicit row tracks
      # sized for all CASUALTY_SURGE_SCHEDULE_SLOTS cards, so hidden
      # (display:none) slots still reserved their row's height — every
      # +/- click left a wall of dead space below the visible cards.
      # Flexbox correctly drops display:none items from layout, so the
      # container's height always matches only what's actually shown.
      div(
        style = "display:flex; flex-wrap:wrap; gap:12px; align-items:flex-start;",
        !!!lapply(seq_len(CASUALTY_SURGE_SCHEDULE_SLOTS), function(i) {
          day_f      <- Find(function(f) identical(f$id, sprintf("mc_sched_day_%d", i)),       sg_fields)
          prob_f     <- Find(function(f) identical(f$id, sprintf("mc_sched_prob_%d", i)),      sg_fields)
          min_cas_f  <- Find(function(f) identical(f$id, sprintf("mc_sched_min_cas_%d", i)),   sg_fields)
          max_cas_f  <- Find(function(f) identical(f$id, sprintf("mc_sched_max_cas_%d", i)),   sg_fields)
          pri_one_f  <- Find(function(f) identical(f$id, sprintf("mc_sched_pri_one_%d", i)),   sg_fields)
          p1 <- defaults[[pri_one_f$id]]; p2 <- defaults[[sprintf("mc_sched_pri_two_%d", i)]]
          pri_lbl <- field_label(list(
            label   = "Priority Split (P1 | P2 | P3)",
            tooltip = "This event's own triage priority mix — independent of every other event.",
            path = pri_one_f$path
          ), overridden_paths)
          div(
            id = sprintf("mc_event_slot_%d", i),
            style = paste0("flex: 1 1 420px; min-width: 340px; max-width: 460px;",
                           if (i <= mc_count) "" else " display:none;"),
            card(
              card_header(sprintf("Event %d", i)),
              div(style = "display:flex; gap:8px;",
                  div(style = "flex:1; min-width:0;", field_input(day_f,     defaults[[day_f$id]],     overridden_paths)),
                  div(style = "flex:1; min-width:0;", field_input(prob_f,    defaults[[prob_f$id]],    overridden_paths))
              ),
              div(style = "display:flex; gap:8px;",
                  div(style = "flex:1; min-width:0;", field_input(min_cas_f, defaults[[min_cas_f$id]], overridden_paths)),
                  div(style = "flex:1; min-width:0;", field_input(max_cas_f, defaults[[max_cas_f$id]], overridden_paths))
              ),
              range_slider_with_text_input(sprintf("mc_event_pri_split_%d", i), pri_lbl, c(p1, p1 + p2))
            )
          )
        })
      )
    )
  ))
}

#' Render the Surge Event Priority Split subgroup, shown in Poisson mode alone
#'
#' @param sg See render_group_body().
#' @param sg_fields See render_group_body().
#' @param defaults See render_group_body().
#' @param overridden_paths See render_group_body().
#' @return A shiny tag or tag list for this subgroup.
render_casualty_surge_split_subgroup <- function(sg, sg_fields, defaults, overridden_paths) {
  one_f <- Find(function(f) identical(f$id, "mc_pri_one"), sg_fields)
  p1 <- defaults[["mc_pri_one"]]; p2 <- defaults[["mc_pri_two"]]
  mc_pri_lbl <- field_label(list(
    label   = "Priority Split (P1 | P2 | P3)",
    tooltip = paste0(
      "Drag either handle to reallocate this event's Priority 1/2/3 share — ",
      "independent of the background Triage Priority Split."
    ),
    path = one_f$path
  ), overridden_paths)
  return(conditionalPanel(
    condition = "input.mc_mode == 'poisson'",
    tagList(
      h6(class = "text-muted mt-2", sg),
      p(class = "text-muted small",
        "Applies to every Poisson-mode event equally — the background Triage Priority Split in the Casualty Rates panel is unaffected by this. In Scheduled mode, each event has its own priority split instead (see Scheduled Event Days above)."),
      card(
        card_header(sg),
        range_slider_with_text_input("mc_pri_split", mc_pri_lbl, c(p1, p1 + p2))
      )
    )
  ))
}

#' Render one top-level Configure accordion panel body for a field group
#'
#' @param overridden_paths Character vector of "elm.acty" paths the active
#'   Casualty Intensity Profile is overriding (see scenario_overridden_paths()
#'   in the server); passed through to every field's tooltip so overridden
#'   fields are flagged individually, not just in the panel-level scope note.
#' @param gen_distributions Named character vector (name = GEN_STREAM_ACTYS
#'   entry, value = "lognormal"/"exponential"), the active Casualty Intensity
#'   Profile's resolved distribution family per casualty-generation stream
#'   (see scenario_json() in the server). Only used by the Casualty
#'   Generation Rates subgroup, to suppress the Std. Dev. field for streams
#'   an exponential distribution — fully described by its mean alone —
#'   currently governs.
#' @param fields List of the group's field specs.
#' @param defaults Named list of current values, keyed by field id.
#' @param dow_shape Named list of the resolved died-of-wounds logistic shape
#'   parameters, used by the Died of Wounds subgroup's curve preview.
#' @return A shiny tag list holding the group's subgroup accordions.
render_group_body <- function(fields, defaults, overridden_paths = NULL, gen_distributions = NULL,
                              dow_shape = NULL) {
  subgroups <- vapply(fields, function(f) if (is.null(f$subgroup)) "" else f$subgroup, character(1))

  if (all(subgroups == "")) {
    return(render_field_grid(fields, defaults, overridden_paths))
  }

  tagList(lapply(unique(subgroups), function(sg) {
    sg_fields <- fields[subgroups == sg]

    # Triage Priority Split and DNBI Sub-Type Split are rendered together in
    # one shared grid (rather than each getting its own single-card grid,
    # which always stretches that lone card to fill the row) so the two
    # sliders can sit side by side when there's room. The card's own header
    # replaces the previous subgroup-title h6, since there are now two
    # titles sharing one row rather than one title per row.
    if (identical(sg, "Triage Priority Split")) {
      return(render_triage_split_subgroup(sg, sg_fields, fields, subgroups, defaults,
                                          overridden_paths))
    }

    # Already rendered above, alongside Triage Priority Split.
    if (identical(sg, "DNBI Sub-Type Split")) {
      return(render_dnbi_split_subgroup())
    }

    if (identical(sg, "Casualty Generation Rates")) {
      return(render_generation_rates_subgroup(sg, sg_fields, defaults, overridden_paths,
                                              gen_distributions))
    }

    if (identical(sg, "Died of Wounds Ceilings")) {
      return(render_dow_ceilings_subgroup(sg, sg_fields, defaults, overridden_paths, dow_shape))
    }

    # Event Timing Mode (the mode dropdown itself) always renders via the
    # default fallback below. Random Event Rate, Scheduled Event Days, and
    # Surge Event Priority Split are each wrapped in a conditionalPanel()
    # keyed off input$mc_mode — the shared casualty-count/priority-split
    # settings (Random Event Rate, Surge Event Priority Split) apply only
    # to Poisson-mode events; Scheduled mode instead gives each event its
    # own casualty-count and priority fields, rendered inline in its card.
    # Injection Window (window_min/mode/max) and Wounded and Killed Split
    # (kia_fraction), both rendered via the default fallback below, are the
    # subgroups that are never mode-gated — neither is customisable per
    # event in either mode.
    if (identical(sg, "Random Event Rate")) {
      return(render_random_event_rate_subgroup(sg, sg_fields, defaults, overridden_paths))
    }

    if (identical(sg, "Scheduled Event Days")) {
      return(render_scheduled_events_subgroup(sg, sg_fields, defaults, overridden_paths))
    }

    if (identical(sg, "Surge Event Priority Split")) {
      return(render_casualty_surge_split_subgroup(sg, sg_fields, defaults, overridden_paths))
    }

    tagList(
      h6(class = "text-muted mt-2", sg),
      render_field_grid(sg_fields, defaults, overridden_paths)
    )
  }))
}
