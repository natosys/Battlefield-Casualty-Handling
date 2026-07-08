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

GEN_STREAM_ACTYS <- c("wia_cbt", "kia_cbt", "dnbi_cbt", "wia_spt", "kia_spt", "dnbi_spt")

#' Render a small density-curve preview for a casualty generation stream
#'
#' @details Mirrors the exact distributional transform used by
#'   generate_ln_arrivals()/generate_exp_arrivals() (R/environment.R), so the
#'   curve shown is the real shape that mean_daily/sd_daily imply — not a
#'   generic approximation. distribution is read live from the resolved
#'   scenario JSON (not user-editable), so switching intensity profiles
#'   changes the curve shape for the streams that scenario actually
#'   overrides (see README Scenario Profiles) and leaves the others alone.
render_gen_curve <- function(mean_daily, sd_daily, distribution) {
  if (is.null(mean_daily) || is.na(mean_daily) || mean_daily <= 0) {
    return(ggplot() + theme_void())
  }
  if (identical(distribution, "exponential")) {
    x <- seq(1e-3, mean_daily * 5, length.out = 200)
    y <- dexp(x, rate = 1 / mean_daily)
    dist_label <- "Exponential"
  } else {
    sd_daily  <- max(if (is.null(sd_daily) || is.na(sd_daily)) 0 else sd_daily, 1e-6)
    mu_log    <- log(mean_daily^2 / sqrt(sd_daily^2 + mean_daily^2))
    sigma_log <- sqrt(log(1 + (sd_daily^2 / mean_daily^2)))
    x <- seq(1e-3, max(mean_daily + 4 * sd_daily, mean_daily * 2), length.out = 200)
    y <- dlnorm(x, meanlog = mu_log, sdlog = sigma_log)
    dist_label <- "Lognormal"
  }
  ggplot(data.frame(x = x, y = y), aes(x, y)) +
    geom_area(fill = "#2a78d6", alpha = 0.25) +
    geom_line(color = "#2a78d6", linewidth = 0.9) +
    geom_vline(xintercept = mean_daily, linetype = "dashed", color = "#c0392b", linewidth = 0.5) +
    labs(x = NULL, y = NULL, subtitle = dist_label) +
    theme_minimal(base_size = 9) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.minor = element_blank(),
          plot.subtitle = element_text(size = 8, color = "#888888"),
          plot.margin = margin(2, 4, 2, 4))
}

#' Colours and short labels for each three-way compositional split, in the
#' same left-to-right order as the slider's segments (0–handle1, handle1–
#' handle2, handle2–1). Consumed entirely by split_slider_recolor_script();
#' there is no separate server-rendered breakdown bar (removed — it was a
#' full duplicate of what the slider itself now shows).
SPLIT_SLIDER_META <- list(
  pri_split  = list(colors = c("#c0392b", "#e08e2d", "#2a78d6"),
                     labels = c("P1", "P2", "P3")),
  dnbi_split = list(colors = c("#8e44ad", "#e08e2d", "#2a78d6"),
                     labels = c("BF", "Disease", "NBI"))
)

#' Recolour a two-handle ion.rangeSlider's track to show all three
#' compositional segments, and overlay a live per-segment value label
#' directly above it — replacing the default styling, which paints only
#' the region between the handles (implying a "selected range") and gives
#' no indication of each segment's actual share.
#'
#' Entirely client-side (a MutationObserver watching `.irs-bar`'s inline
#' style, which ion.rangeSlider updates continuously while dragging) so
#' labels track the handles with no server round-trip — unlike the
#' ggplot bar this replaces, which required a renderPlot() invalidation
#' on every drag.
#'
#' @param meta_map Named list; each name is a slider input id (e.g.
#'   "pri_split") and each value a list(colors = <length-3 hex vector>,
#'   labels = <length-3 short label vector>), both in slider segment order.
split_slider_recolor_script <- function(meta_map) {
  json <- jsonlite::toJSON(meta_map, auto_unbox = TRUE)
  js <- paste0("
(function() {
  var meta = ", json, ";
  $(document).on('shiny:bound', function(e) {
    var id = e.target.id;
    var cfg = meta[id];
    if (!cfg) return;
    var colors = cfg.colors, labels = cfg.labels;
    var $wrap = $(e.target).closest('.form-group');
    var sliderEl = $wrap.find('.irs--shiny')[0];
    var bar = $wrap.find('.irs-bar')[0];
    var line = $wrap.find('.irs-line')[0];
    if (!bar || !line || !sliderEl) return;

    var labelRow = $wrap.find('.split-slider-labels')[0];
    if (!labelRow) {
      labelRow = document.createElement('div');
      labelRow.className = 'split-slider-labels';
      labelRow.style.position = 'relative';
      labelRow.style.height = '26px';
      labelRow.style.marginBottom = '4px';
      labelRow.style.width = sliderEl.offsetWidth + 'px';
      for (var i = 0; i < 3; i++) {
        var seg = document.createElement('div');
        seg.style.position = 'absolute';
        seg.style.top = '0';
        seg.style.bottom = '0';
        seg.style.display = 'flex';
        seg.style.alignItems = 'center';
        seg.style.justifyContent = 'center';
        seg.style.overflow = 'hidden';
        seg.style.whiteSpace = 'nowrap';
        seg.style.color = '#fff';
        seg.style.fontWeight = '600';
        seg.style.borderRadius = '3px';
        seg.style.background = colors[i];
        labelRow.appendChild(seg);
      }
      sliderEl.parentNode.insertBefore(labelRow, sliderEl);
    }
    var segs = labelRow.children;

    function paint() {
      var left = parseFloat(bar.style.left) || 0;
      var width = parseFloat(bar.style.width) || 0;
      var right = Math.min(100, left + width);
      var bounds = [0, left, right, 100];
      // Derive the displayed shares from the slider's own bound value
      // (exact, step-quantized) rather than the rendered CSS percentage
      // of `.irs-bar`, which is only an approximate pixel-rounded proxy
      // for the true value and can drift by a percentage point or more.
      var parts = (e.target.value || '').split(';').map(parseFloat);
      var from = parts[0], to = parts[1];
      if (isNaN(from) || isNaN(to)) { from = left / 100; to = right / 100; }
      var shares = [from, to - from, 1 - to];
      for (var i = 0; i < 3; i++) {
        var segLeft = bounds[i], segWidth = bounds[i + 1] - bounds[i];
        segs[i].style.left = segLeft + '%';
        segs[i].style.width = segWidth + '%';
        segs[i].style.fontSize = segWidth < 8 ? '0px' : '12px';
        segs[i].textContent = labels[i] + ': ' + shares[i].toFixed(2);
      }
      line.style.background =
        'linear-gradient(to right,' +
        colors[0] + ' 0%,' + colors[0] + ' ' + left + '%,' +
        colors[1] + ' ' + left + '%,' + colors[1] + ' ' + right + '%,' +
        colors[2] + ' ' + right + '%,' + colors[2] + ' 100%)';
    }
    bar.style.background = 'transparent';
    bar.style.boxShadow = 'none';
    paint();
    new MutationObserver(paint).observe(bar, {attributes: true, attributeFilter: ['style']});
  });
})();
")
  tags$script(HTML(js))
}

#' Merge a two-handle range-slider value into a values list as three
#' underlying field values, matching the ids apply_registry_values()/
#' validate_config() expect. Needed because the Configure panel replaces
#' three-way compositional splits (Triage Priority, DNBI Sub-Type) with a
#' single slider each (see render_group_body()'s special cases) — the
#' split sums to 1 by construction, so this is purely a representation
#' change, not a new validation rule.
#'
#' @param values Named list (from reactiveValuesToList(input))
#' @param split_id Input id of the range slider, e.g. "pri_split"
#' @param part_ids Character vector of length 3: the three field ids the
#'   slider's breakpoints expand into, in slider order
inject_split <- function(values, split_id, part_ids) {
  v <- values[[split_id]]
  if (!is.null(v) && length(v) == 2) {
    values[[part_ids[1]]] <- v[1]
    values[[part_ids[2]]] <- v[2] - v[1]
    values[[part_ids[3]]] <- 1 - v[2]
  }
  values
}

#' Apply inject_split() for every compositional split the Configure panel
#' represents as a slider
inject_all_splits <- function(values) {
  values <- inject_split(values, "pri_split",  c("pri_one", "pri_two", "pri_three"))
  values <- inject_split(values, "dnbi_split", c("dnbi_bf_pct", "dnbi_disease_pct", "dnbi_nbi_pct"))
  values
}

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

#' @param overridden_paths Character vector of "elm.acty" paths the active
#'   Casualty Intensity Profile is currently overriding (from
#'   scenario_overridden_paths()); NULL/empty under the default profile.
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

#' Pair a single-value slider with a small numeric box for typed entry.
#' The slider (`id`) remains the authoritative reactive value everything
#' else in the app reads via `input[[id]]`; the numeric box (`<id>_txt`)
#' is a secondary UI-only control kept in sync by wire_slider_text_sync()
#' (registered once at server startup — see server()). The field's own
#' label sits beside the numeric box rather than above the slider, since
#' sliderInput(label = NULL) omits its built-in label row entirely.
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

field_input <- function(f, value, overridden_paths = NULL) {
  lbl <- field_label(f, overridden_paths)
  if (isTRUE(f$morris) || isTRUE(f$slider)) {
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
field_card <- function(f, value, overridden_paths = NULL) {
  card(field_input(f, value, overridden_paths))
}

#' NULL-coalesce (base R only gained `%||%` in 4.4; this app targets 4.3).
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

#' Render an SVG node graph: one node per deployed team at each echelon,
#' full bipartite mesh of lines between adjacent echelons (R1<->R2B,
#' R2B<->R2E). A full mesh — every node on one side connected to every
#' node on the other — is deliberate, not a simplification: resources at
#' each echelon are pooled and seized on availability (see
#' R/trajectories.R's select()/seize_selected() calls), not partitioned
#' into fixed lanes, so a casualty from any R1 team can in principle be
#' routed to any R2B team. Node counts >~6 per column make for a dense
#' mesh; that density is itself informative (it is the same pooling that
#' produces it), so it is left as-is rather than simplified away.
#'
#' @param r1_teams,r2b_teams,r2e_teams Team counts (numeric scalars).
force_node_graph <- function(r1_teams, r2b_teams, r2e_teams) {
  r1_teams  <- max(0, round(r1_teams %||% 0))
  r2b_teams <- max(0, round(r2b_teams %||% 0))
  r2e_teams <- max(0, round(r2e_teams %||% 0))

  col_x   <- c(60, 300, 540)
  row_gap <- 30
  top_pad <- 34
  max_n   <- max(r1_teams, r2b_teams, r2e_teams, 1)
  height  <- top_pad + (max_n - 1) * row_gap + 30

  node_y <- function(n) if (n == 0) numeric(0) else top_pad + (seq_len(n) - 1) * row_gap

  y1 <- node_y(r1_teams); y2 <- node_y(r2b_teams); y3 <- node_y(r2e_teams)

  mesh <- function(x_from, y_from, x_to, y_to, color) {
    if (length(y_from) == 0 || length(y_to) == 0) return(NULL)
    pts <- expand.grid(yf = y_from, yt = y_to)
    lapply(seq_len(nrow(pts)), function(i) {
      tags$line(x1 = x_from, y1 = pts$yf[i], x2 = x_to, y2 = pts$yt[i],
                stroke = color, `stroke-width` = 1, `stroke-opacity` = 0.3)
    })
  }
  nodes <- function(x, ys, fill) {
    lapply(seq_along(ys), function(i) {
      tagList(
        tags$circle(cx = x, cy = ys[i], r = 11, fill = fill, stroke = "#333", `stroke-width` = 1),
        tags$text(x = x, y = ys[i] + 4, `text-anchor` = "middle", `font-size` = 11,
                   `font-weight` = "600", fill = "#fff", i)
      )
    })
  }
  header <- function(x, label) {
    tags$text(x = x, y = 14, `text-anchor` = "middle", `font-size` = 12, `font-weight` = "bold", fill = "#333", label)
  }

  tags$svg(
    xmlns = "http://www.w3.org/2000/svg", viewBox = sprintf("0 0 %d %d", 600, height),
    style = sprintf("width:100%%; max-width:640px; height:%dpx; display:block;", height),
    mesh(col_x[1], y1, col_x[2], y2, "#c0392b"),
    mesh(col_x[2], y2, col_x[3], y3, "#2a78d6"),
    nodes(col_x[1], y1, "#c0392b"), nodes(col_x[2], y2, "#e08e2d"), nodes(col_x[3], y3, "#2a78d6"),
    header(col_x[1], sprintf("R1 (%g)", r1_teams)),
    header(col_x[2], sprintf("R2B (%g)", r2b_teams)),
    header(col_x[3], sprintf("R2E (%g)", r2e_teams))
  )
}

#' Tabulate aggregate bed capacity by echelon x bed type: total = per-team
#' count x team count, with the per-team/team factors shown underneath
#' each total for traceability back to the fields that produced it.
force_bed_table <- function(r2b_teams, r2b_beds, r2e_teams, r2e_beds) {
  bed_types <- names(r2b_beds)
  cell <- function(per_team, teams) {
    tags$td(
      tags$div(style = "font-weight:600;", sprintf("%g", per_team * teams)),
      tags$div(style = "font-size:10px; color:#888;", sprintf("(%g × %g)", per_team, teams))
    )
  }
  row <- function(label, teams, beds) {
    tags$tr(tags$td(tags$b(label)), lapply(bed_types, function(nm) cell(beds[[nm]], teams)))
  }
  tags$table(
    class = "table table-sm table-bordered",
    style = "margin-top:6px; max-width:480px;",
    tags$thead(tags$tr(tags$th("Echelon"), lapply(bed_types, tags$th))),
    tags$tbody(row("R2B", r2b_teams, r2b_beds), row("R2E", r2e_teams, r2e_beds))
  )
}

#' Render the Force Design panel's live structural capacity diagram: a
#' node graph (one node per deployed team, see force_node_graph()) above
#' a tabulation of aggregate bed capacity (see force_bed_table()). This
#' visualises the *structural* capacity implied by the Force Design
#' panel's own numbers as they're edited; it is not a simulated outcome
#' — it cannot show queueing, wait times, or casualty outcomes under that
#' configuration, only Quick Run can. The caption says as much, so the
#' diagram isn't mistaken for one.
#'
#' @param r1_teams,r2b_teams,r2e_teams Team counts (numeric scalars).
#' @param r2b_beds,r2e_beds Named numeric vectors, one entry per bed
#'   type, each the *per-team* bed count for that echelon.
force_structure_diagram <- function(r1_teams, r2b_teams, r2b_beds, r2e_teams, r2e_beds) {
  tagList(
    h6(class = "text-muted mt-2", "Force Structure Overview"),
    p(class = "text-muted small",
      "Each node is one deployed team; lines show that casualties from any team at one echelon can be routed to any team at the next — resources are pooled per echelon, not fixed lanes. This shows what you're configuring, not a simulated outcome; run Quick Run to see actual queueing, wait times, and casualty outcomes under this configuration."),
    force_node_graph(r1_teams, r2b_teams, r2e_teams),
    p(class = "text-muted small", style = "margin-top:2px;",
      "R1 has no bed capacity of its own — casualties are treated and returned to duty, or transported to R2B."),
    h6(class = "text-muted", style = "font-size:13px; margin-top:8px;", "Aggregate Bed Capacity (Total = Per-Team × Teams)"),
    force_bed_table(r2b_teams, r2b_beds, r2e_teams, r2e_beds)
  )
}

#' Render one top-level Configure accordion panel body for a field group
#'
#' @param overridden_paths Character vector of "elm.acty" paths the active
#'   Casualty Intensity Profile is overriding (see scenario_overridden_paths()
#'   in the server); passed through to every field's tooltip so overridden
#'   fields are flagged individually, not just in the panel-level scope note.
render_group_body <- function(fields, defaults, overridden_paths = NULL) {
  subgroups <- vapply(fields, function(f) if (is.null(f$subgroup)) "" else f$subgroup, character(1))

  if (all(subgroups == "")) {
    return(layout_column_wrap(
      width = "300px",
      !!!lapply(fields, function(f) field_card(f, defaults[[f$id]], overridden_paths))
    ))
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
      one_f <- Find(function(f) identical(f$id, "pri_one"), sg_fields)
      p1 <- defaults[["pri_one"]]; p2 <- defaults[["pri_two"]]
      pri_lbl <- field_label(list(
        label   = "Priority Split (P1 | P2 | P3)",
        tooltip = paste0(
          "Drag either handle to reallocate share between adjacent priorities — ",
          "Priority 1, 2, and 3 always sum to 100% by construction. Source: ", SRC_PRIORITY_SPLIT
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
            "Battle Fatigue, Disease, and Non-Battle Injury always sum to 100% by construction. ",
            "Sources: Battle Fatigue — ", SRC_DNBI_BF_PCT, " Disease — ", SRC_DNBI_DISEASE_PCT,
            " NBI — ", SRC_DNBI_NBI_PCT
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

    # Already rendered above, alongside Triage Priority Split.
    if (identical(sg, "DNBI Sub-Type Split")) {
      return(NULL)
    }

    if (identical(sg, "Casualty Generation Rates")) {
      return(tagList(
        h6(class = "text-muted mt-2", sg),
        p(class = "text-muted small",
          "These are curve parameters, not single values — the shaded area is the actual shape of daily casualty-rate variability implied by the mean and standard deviation below it (dashed line = mean). The curve updates live as you edit the numbers, and its shape (lognormal vs exponential) follows the Casualty Intensity Profile selected above."),
        layout_column_wrap(
          width = "320px",
          !!!lapply(GEN_STREAM_ACTYS, function(acty) {
            mean_f <- Find(function(f) identical(f$id, paste0("gen_", acty, "_mean")), sg_fields)
            sd_f   <- Find(function(f) identical(f$id, paste0("gen_", acty, "_sd")),   sg_fields)
            stream_label <- sub(" — Mean Daily Rate$", "", mean_f$label)
            card(
              card_header(stream_label),
              plotOutput(paste0("curve_", acty), height = "110px"),
              field_input(mean_f, defaults[[mean_f$id]], overridden_paths),
              field_input(sd_f, defaults[[sd_f$id]], overridden_paths)
            )
          })
        )
      ))
    }

    tagList(
      h6(class = "text-muted mt-2", sg),
      layout_column_wrap(
        width = "300px",
        !!!lapply(sg_fields, function(f) field_card(f, defaults[[f$id]], overridden_paths))
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
    split_slider_recolor_script(SPLIT_SLIDER_META),
    p(class = "text-muted",
      "Adjust simulation parameters below, grouped by operational concept. ",
      "Hover the ", tags$b("ⓘ"), " icon next to any field for an explanation. ",
      "Sliders show the plausible range screened in the project's Morris sensitivity analysis (Issue #3)."),
    fluidRow(
      column(4, uiOutput("scenario_selector_ui")),
      column(3, fileInput("upload_json", "Load Configuration (.json)", accept = ".json")),
      column(3, downloadButton("download_json", "Save Configuration", class = "btn-outline-secondary mt-4"))
    ),
    uiOutput("scenario_scope_note"),
    accordion(
      id = "config_accordion", open = c(GRP_FORCE),
      !!!lapply(c(GRP_FORCE, GRP_FORCE_DESIGN, GRP_CASUALTY, GRP_TRANSPORT), function(g) {
        if (identical(g, GRP_FORCE_DESIGN)) {
          accordion_panel(g, uiOutput("force_design_diagram"), uiOutput(paste0("group_ui_", make.names(g))))
        } else {
          accordion_panel(g, uiOutput(paste0("group_ui_", make.names(g))))
        }
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
        slider_with_text_input("ot_hours",
                    field_label(list(label = "OT Shift Length (hours per shift)",
                                     tooltip = paste0(
                                       "Hours the first operating theatre shift is active each day ",
                                       "(the second shift covers the remainder of the 24h day). ",
                                       sprintf("Screened in Morris sensitivity analysis (Issue #3); plausible range %s–%s.",
                                               morris_params$lower[morris_params$name == "ot_hours"],
                                               morris_params$upper[morris_params$name == "ot_hours"]),
                                       " Source: README Schedules and Rosters — surgical team shift length; not independently cited (informed establishment planning assumption)."))),
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

  # Every slider gets a paired "type an exact value" numeric box (see
  # field_input()/slider_with_text_input()); wire the two-way sync once per
  # id here rather than inside the per-group renderUI blocks, since the
  # sync only needs to exist once regardless of how many times the UI
  # containing it is regenerated.
  slider_field_ids <- vapply(
    Filter(function(f) isTRUE(f$morris) || isTRUE(f$slider), PARAM_REGISTRY),
    function(f) f$id, character(1)
  )
  lapply(slider_field_ids, function(id) wire_slider_text_sync(input, session, id))
  wire_slider_text_sync(input, session, "ot_hours")
  wire_range_slider_text_sync(input, session, "pri_split")
  wire_range_slider_text_sync(input, session, "dnbi_split")

  # ── Casualty Intensity Profile selector ──────────────────────────────────
  # Offers exactly the scenario profiles defined in the loaded env_data.json
  # (today: "default" plus whatever is under its `scenarios` block) — no
  # fabricated intermediate tiers. resolve_scenario() (R/scenario.R) only
  # overlays the `vars` paths a given scenario actually defines; structural
  # config (force size, team/bed counts, transport fleet) is never touched.

  # env_data.json's scenario labels carry a full descriptive parenthetical
  # (e.g. "Moderate Intensity — Falklands 1982 (Operation CORPORATE, British
  # Task Force, South Atlantic)") intended for the README/CSV outputs; the
  # dropdown only needs the short form before that parenthetical.
  shorten_scenario_label <- function(lbl) {
    trimws(sub("\\s*\\(.*$", "", lbl))
  }

  scenario_choices <- reactive({
    base <- raw_env_data()
    ids  <- c("default", names(base$scenarios))
    labels <- vapply(ids, function(s) {
      if (identical(s, "default")) return("Default")
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
          "Selects a named scenario profile that overlays casualty-generation,",
          "DOW, and treatment-efficacy parameters onto the base configuration.",
          "Structural fields (force size, team/bed counts, transport fleet)",
          "are never affected by this selector.",
          "Source: README Scenario Profiles (Issue #54); only the profiles",
          "actually defined and cited in env_data.json are offered — no",
          "fabricated intermediate intensity tiers."
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
  # panel's field defaults, Save Configuration, and Quick Run all consume.
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

  lapply(names(fields_by_group), function(g) {
    output_id <- paste0("group_ui_", make.names(g))
    output[[output_id]] <- renderUI({
      defaults <- registry_defaults(fields_by_group[[g]], scenario_json())
      render_group_body(fields_by_group[[g]], defaults, scenario_overridden_paths())
    })
    # Accordion panels other than the initially-open one are hidden
    # (display:none) at first render; Shiny suspends output bindings it
    # judges invisible, so their inputs would never register unless the
    # user happens to open every panel first. Force eager rendering so
    # every field's input exists (and its value is capturable) regardless
    # of which panels are currently expanded.
    outputOptions(output, output_id, suspendWhenHidden = FALSE)
  })

  # Live structural capacity diagram at the top of the Force Design panel
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
  outputOptions(output, "force_design_diagram", suspendWhenHidden = FALSE)

  # Live casualty-generation curve previews (Casualty Rates group). Read
  # live from the mean/sd inputs so the curve redraws as the user edits
  # them; the distribution family comes from scenario_json() (not itself
  # user-editable — see README Scenario Profiles for why picking a family
  # independently of a sourced rate would be uncited guesswork).
  lapply(GEN_STREAM_ACTYS, function(acty) {
    output_id <- paste0("curve_", acty)
    output[[output_id]] <- renderPlot({
      mean_v <- input[[paste0("gen_", acty, "_mean")]]
      sd_v   <- input[[paste0("gen_", acty, "_sd")]]
      dist   <- get_raw_var(scenario_json(), "generators", acty, "distribution")
      if (is.null(dist)) dist <- "lognormal"
      render_gen_curve(mean_v, sd_v, dist)
    })
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
    apply_registry_values(PARAM_REGISTRY, scenario_json(), inject_all_splits(reactiveValuesToList(input)))
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

    # Triage priority split and DNBI sub-type split have no corresponding
    # check here — pri_split and dnbi_split (two-handle range sliders, see
    # render_group_body()) make summing to 1 a structural guarantee rather
    # than something to validate after the fact.

    errors
  }

  # ── Quick Run execution (async via future + promises) ────────────────────

  observeEvent(input$run_quick, {
    values <- inject_all_splits(reactiveValuesToList(input))
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
