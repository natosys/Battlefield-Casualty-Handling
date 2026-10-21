##############################################
## R/app_ui_diagrams.R                    ##
## Force and evacuation diagrams          ##
##############################################
#
# The force structure, bed table and evacuation chain diagrams the
# Configure panel draws from the live configuration.
#
# Sourced by app.R after the packages and modules it loads; source that
# file rather than this one alone.

#' Render an SVG node graph: one node per deployed team at each echelon,
#' full bipartite mesh of lines between adjacent echelons (R1<->R2B,
#' R2B<->R2E). A full mesh — every node on one side connected to every
#' node on the other — is deliberate, not a simplification: resources at
#' each echelon are pooled and seized on availability (see
#' R/trajectories.R's select()/seize_selected() calls), not partitioned
#' into fixed lanes, so a casualty from any R1 team can in principle be
#' routed to any R2B team. Node counts >~6 per band make for a dense
#' mesh; that density is itself informative (it is the same pooling that
#' produces it), so it is left as-is rather than simplified away.
#'
#' Laid out vertically (R1 top, R2B middle, R2E bottom, each a horizontal
#' band of nodes) rather than as three left-to-right columns, so the
#' whole diagram fits a narrow sidebar column next to — and roughly
#' level with — the echelon's own field sections, which render in the
#' same top-to-bottom order immediately to its right.
#'
#' @param r1_teams,r2b_teams,r2e_teams Team counts (numeric scalars).
#' @return An SVG tag of the three bands of nodes and the mesh between them.
force_node_graph <- function(r1_teams, r2b_teams, r2e_teams) {
  r1_teams  <- max(0, round(r1_teams %||% 0))
  r2b_teams <- max(0, round(r2b_teams %||% 0))
  r2e_teams <- max(0, round(r2e_teams %||% 0))

  col_gap <- 26
  base_w  <- 240
  max_n   <- max(r1_teams, r2b_teams, r2e_teams, 1)
  width   <- max(base_w, (max_n - 1) * col_gap + 40)

  label_y <- c(12, 118, 224)
  node_y  <- label_y + 24
  height  <- node_y[3] + 26

  #' Horizontal positions of one echelon's team nodes
  #'
  #' @param n Number of teams at the echelon.
  #' @return A numeric vector of x positions, centred in the diagram, empty
  #'   where the echelon has no teams.
  node_x <- function(n) {
    if (n == 0) return(numeric(0))
    total_w <- (n - 1) * col_gap
    start_x <- (width - total_w) / 2
    start_x + (seq_len(n) - 1) * col_gap
  }
  x1 <- node_x(r1_teams); x2 <- node_x(r2b_teams); x3 <- node_x(r2e_teams)

  #' Draw the full mesh of lines between two echelons' nodes
  #'
  #' @param y_from Vertical position of the upper row.
  #' @param x_from Horizontal positions of the upper row's nodes.
  #' @param y_to Vertical position of the lower row.
  #' @param x_to Horizontal positions of the lower row's nodes.
  #' @param color Stroke colour of the lines.
  #' @return A list of SVG line tags, NULL where either row is empty.
  #' @details Every node is joined to every node below it, the evacuation
  #'   chain placing no restriction on which team receives from which.
  mesh <- function(y_from, x_from, y_to, x_to, color) {
    if (length(x_from) == 0 || length(x_to) == 0) return(NULL)
    pts <- expand.grid(xf = x_from, xt = x_to)
    lapply(seq_len(nrow(pts)), function(i) {
      tags$line(x1 = pts$xf[i], y1 = y_from, x2 = pts$xt[i], y2 = y_to,
                stroke = color, `stroke-width` = 1, `stroke-opacity` = 0.3)
    })
  }
  #' Draw one echelon's numbered team nodes
  #'
  #' @param y Vertical position of the row.
  #' @param xs Horizontal positions of the nodes.
  #' @param fill Fill colour of the circles.
  #' @return A list of SVG tag lists, one circle and label per node.
  nodes <- function(y, xs, fill) {
    lapply(seq_along(xs), function(i) {
      tagList(
        tags$circle(cx = xs[i], cy = y, r = 10, fill = fill, stroke = "#333", `stroke-width` = 1),
        tags$text(x = xs[i], y = y + 4, `text-anchor` = "middle", `font-size` = 10,
                   `font-weight` = "600", fill = "#fff", i)
      )
    })
  }
  #' Draw one echelon's row label
  #'
  #' @param y Vertical position of the label.
  #' @param label Text of the label.
  #' @return An SVG text tag.
  header <- function(y, label) {
    tags$text(x = 4, y = y, `text-anchor` = "start", `font-size` = 12, `font-weight` = "bold", fill = "#333", label)
  }

  tags$svg(
    xmlns = "http://www.w3.org/2000/svg", viewBox = sprintf("0 0 %d %d", width, height),
    style = sprintf("width:100%%; max-width:%dpx; height:%dpx; display:block;", width, height),
    mesh(node_y[1], x1, node_y[2], x2, "#c0392b"),
    mesh(node_y[2], x2, node_y[3], x3, "#2a78d6"),
    nodes(node_y[1], x1, "#c0392b"), nodes(node_y[2], x2, "#e08e2d"), nodes(node_y[3], x3, "#2a78d6"),
    header(label_y[1], sprintf("R1 (%g)", r1_teams)),
    header(label_y[2], sprintf("R2B (%g)", r2b_teams)),
    header(label_y[3], sprintf("R2E (%g)", r2e_teams))
  )
}

#' Tabulate aggregate bed capacity by echelon x bed type: total = per-team
#' count x team count, with the per-team/team factors shown underneath
#' each total for traceability back to the fields that produced it.
#' Transposed (rows = bed type, columns = echelon) so the table stays
#' narrow enough for the sidebar column it now shares with the node
#' graph — 3 columns fits comfortably where the original 5-column
#' (echelon x bed type) layout would have been cramped or overflowed.
#'
#' @param r2b_teams Number of R2B teams deployed.
#' @param r2b_beds Named numeric vector of R2B bed counts per team.
#' @param r2e_teams Number of R2E teams deployed.
#' @param r2e_beds Named numeric vector of R2E bed counts per team.
#' @return A table tag of one row per bed type across the two echelons.
force_bed_table <- function(r2b_teams, r2b_beds, r2e_teams, r2e_beds) {
  bed_types <- names(r2b_beds)
  #' Render one bed total with the factors that produced it
  #'
  #' @param per_team Bed count per team.
  #' @param teams Number of teams at the echelon.
  #' @return A table cell holding the total above its two factors.
  cell <- function(per_team, teams) {
    tags$td(
      tags$div(style = "font-weight:600;", sprintf("%g", per_team * teams)),
      tags$div(style = "font-size:9px; color:#888;", sprintf("(%g × %g)", per_team, teams))
    )
  }
  #' Render one bed type's row across the two echelons
  #'
  #' @param nm Bed type name.
  #' @return A table row of the bed type and its two echelon totals.
  row <- function(nm) {
    tags$tr(tags$td(tags$b(nm)), cell(r2b_beds[[nm]], r2b_teams), cell(r2e_beds[[nm]], r2e_teams))
  }
  tags$table(
    class = "table table-sm table-bordered",
    style = "margin-top:6px; font-size:12px;",
    tags$thead(tags$tr(tags$th("Bed Type"), tags$th("R2B"), tags$th("R2E"))),
    tags$tbody(lapply(bed_types, row))
  )
}

#' Render the Health System Architecture panel's live structural capacity
#' diagram: a node graph (one node per deployed team, see
#' force_node_graph()) above a tabulation of aggregate bed capacity (see
#' force_bed_table()). This visualises the *structural* capacity implied by
#' the Health System Architecture panel's own numbers as they're edited;
#' it is not a simulated outcome
#' — it cannot show queueing, wait times, or casualty outcomes under that
#' configuration, only a run can. The caption says as much, so the
#' diagram isn't mistaken for one.
#'
#' @param r1_teams,r2b_teams,r2e_teams Team counts (numeric scalars).
#' @param r2b_beds,r2e_beds Named numeric vectors, one entry per bed
#'   type, each the *per-team* bed count for that echelon.
#' @return A tag list of the heading, the node graph, the bed table and the
#'   caption saying what the diagram does not show.
force_structure_diagram <- function(r1_teams, r2b_teams, r2b_beds, r2e_teams, r2e_beds) {
  tagList(
    h6(class = "text-muted mt-2", "Force Structure"),
    p(class = "text-muted", style = "font-size:11px;",
      "Each node is one deployed team; lines show pooled routing to the next echelon, not fixed lanes. This is structural, not a simulated outcome; run the saved configuration from the command line for queueing and casualty results."),
    force_node_graph(r1_teams, r2b_teams, r2e_teams),
    p(class = "text-muted", style = "font-size:11px; margin-top:2px;",
      "R1 has no beds — treats and returns to duty, or transports to R2B."),
    h6(class = "text-muted", style = "font-size:12px; margin-top:8px;", "Bed Capacity (Total = Per-Team × Teams)"),
    force_bed_table(r2b_teams, r2b_beds, r2e_teams, r2e_beds)
  )
}

#' Render the Medevac panel's live medevac chain diagram: a
#' compact, fixed-topology diagram of the vehicle-transport legs the
#' trajectory code actually models (R/trajectories.R), labelled with each
#' leg's current mode duration as the fields beside it are edited. Each leg
#' also carries an unmodified dead-heading return leg — a fresh draw from
#' the same outbound distribution, with no configurable multiplier (Issue
#' #74). Unlike force_node_graph()
#' (Health System Architecture), whose topology scales with team counts,
#' the medevac chain's topology is fixed — three echelons, a small fixed
#' set of legs — so only the text labels are dynamic, not the layout.
#'
#' Every line drawn corresponds to an actual seize()/timeout() sequence in
#' R/trajectories.R, not an aspirational or simplified one. All four legs
#' carry a working dead-heading return leg, the R2B to R2E legs on the same
#' basis as the R1 to R2B ones:
#'   - R1 → R2B, WIA (PMVAmb): r1_transport_wia().
#'   - R1 → R2B, KIA (HX240M): r1_transport_kia() ("KIA transport from
#'     Role 1 to mortuary at Role 2").
#'   - R2B → R2E, WIA: the inline evacuation step inside r2b_treat_wia()
#'     (all of its "R2B Hold"/"Hold Full"/"Hold Queue"/wait-for-evac
#'     sub-paths — i.e. every R2B→R2E movement, surgical or bypassed,
#'     funnels through r2b_evac_leg()/r2b_evac_return_leg()), which seizes
#'     each R2B team's own `evac` resource rather than the shared PMVAmb
#'     fleet — a deliberate design: this leg represents an
#'     organic R2B unit asset, distinct from the brigade-pooled fleet used
#'     for R1 → R2B. The dead-heading return leg is modelled on that same
#'     organic resource rather than a pooled vehicle.
#'   - R2B → R2E, KIA/mortuary: r2b_transport_kia(), using the shared
#'     HX2 40M fleet. The mortuary is modelled as collocated with R2E, not
#'     R2B; R2B has no organic mortuary asset, so a
#'     KIA at R2B is transported by road to the R2E-collocated mortuary
#'     rather than processed in place.
#' KIA reaching R2E (whether they died there, or arrived by road from R1 or
#' R2B) travel no further — r2e_transport_kia() moves the casualty to the
#' *collocated* R2E mortuary via the receiving team's own evacuation-team
#' resource — so this renders as a small in-place ⚱ marker at R2E only,
#' rather than a line, to avoid implying a further cross-echelon leg the
#' model doesn't have.
#'
#' @param wia1_mode R1→R2B WIA transport mode (minutes); return leg is an
#'   unmodified fresh draw from the same distribution.
#' @param kia1_mode R1→R2B KIA transport mode (minutes).
#' @param wia2_mode R2B→R2E WIA transport mode (minutes), applied to the
#'   R2B team's own organic evac resource rather than a pooled vehicle.
#' @param kia2_mode R2B→R2E KIA/mortuary road-move transport mode
#'   (minutes), using the shared HX2 40M fleet.
#' @param mort2e_mode R2E collocated-mortuary local transport mode
#'   (minutes) — no return leg, since no vehicle asset is used for this
#'   final in-place movement.
#' @param ame_critical,ame_standard Critical (ICU-bed, CCATT/CCAST-supported)
#'   and standard (Hold-bed, Casualty Staging Unit-equivalent) capacity per
#'   successful sortie, taken from the selected airframe's fitted patient
#'   capacity (build_ame_sortie_trajectory(), R/trajectories_schedules.R). Both pools
#'   fill on the same sortie.
#' @param ame_airframe_label Name of the selected aircraft, shown so the
#'   capacities above are attributable to a platform.
#' @param ame_schedule_interval Days between scheduled AME sortie
#'   opportunities.
#' @param ame_failure_probability Probability a scheduled sortie is
#'   cancelled, carrying zero capacity.
#' @return An SVG tag of the four echelon nodes and the legs between them.
evac_chain_diagram <- function(wia1_mode, kia1_mode, wia2_mode, kia2_mode,
                                mort2e_mode, ame_critical = NA,
                                ame_standard = NA,
                                ame_airframe_label = NULL,
                                ame_schedule_interval = NA,
                                ame_failure_probability = NA) {
  #' Format a count for the diagram, tolerating an absent value
  #'
  #' @param x The value to format.
  #' @return The rounded value with thousands separators, "?" where absent.
  fmt  <- function(x) if (is.null(x) || is.na(x)) "?" else format(round(x), big.mark = ",")

  #' Format a probability as a percentage, tolerating an absent value
  #'
  #' @param x The probability to format.
  #' @return The value as a whole percentage, "?" where absent.
  fmt_pct <- function(x) if (is.null(x) || is.na(x)) "?" else paste0(round(x * 100), "%")

  width <- 300
  y     <- c(20, 130, 240, 350)
  cx    <- width / 2
  height <- y[4] + 44

  #' Draw one echelon node of the evacuation chain
  #'
  #' @param y Vertical position of the node.
  #' @param label Node label.
  #' @param sub Sub-label shown beneath it.
  #' @return An SVG tag list of the box and its two labels.
  node <- function(y, label, sub) {
    tagList(
      tags$rect(x = cx - 44, y = y - 16, width = 88, height = 32, rx = 6,
                fill = "#f5f5f5", stroke = "#333", `stroke-width` = 1),
      tags$text(x = cx, y = y - 2, `text-anchor` = "middle", `font-size` = 12,
                 `font-weight` = "700", fill = "#333", label),
      tags$text(x = cx, y = y + 12, `text-anchor` = "middle", `font-size` = 9,
                 fill = "#888", sub)
    )
  }

  #' Draw one transport leg between two nodes
  #'
  #' @param y1 Vertical position of the upper node.
  #' @param y2 Vertical position of the lower node.
  #' @param side -1 to draw left of centre, +1 to draw right of it.
  #' @param color Stroke and label colour.
  #' @param label_line1 First line of the leg's label.
  #' @param label_line2 Optional second line of the label.
  #' @return An SVG tag list of the line and its labels.
  #' @details The side argument grows a leg's label away from the midline,
  #'   left-anchored on one side and right-anchored on the other, so the WIA
  #'   and KIA labels of the same leg diverge rather than colliding.
  leg <- function(y1, y2, side, color, label_line1, label_line2 = NULL) {
    x <- cx + side * 38
    anchor <- if (side < 0) "end" else "start"
    label_x <- x + side * 6
    tagList(
      tags$line(x1 = x, y1 = y1 + 16, x2 = x, y2 = y2 - 16,
                stroke = color, `stroke-width` = 2),
      tags$text(x = label_x, y = (y1 + y2) / 2 - 4, `text-anchor` = anchor,
                 `font-size` = 9, fill = color, label_line1),
      if (!is.null(label_line2)) {
        tags$text(x = label_x, y = (y1 + y2) / 2 + 8, `text-anchor` = anchor,
                   `font-size` = 9, fill = "#888", label_line2)
      }
    )
  }

  #' Draw the mortuary branch marker beside a node
  #'
  #' @param y Vertical position of the node it sits beside.
  #' @param mode Modal mortuary handling duration in minutes.
  #' @return An SVG tag list of the marker and its label.
  mortuary_marker <- function(y, mode) {
    tagList(
      tags$text(x = cx + 50, y = y + 4, `font-size` = 13, "⚱"),
      tags$text(x = cx + 64, y = y + 4, `font-size` = 9, fill = "#888",
                 sprintf("Mortuary: %s min", fmt(mode)))
    )
  }

  #' Draw the strategic evacuation schedule note beneath the last node
  #'
  #' @param y Vertical position of the node it sits beneath.
  #' @return An SVG tag list of the cadence, cancellation rate and airframe.
  ame_schedule_note <- function(y) {
    tagList(
      tags$text(x = cx, y = y + 26, `text-anchor` = "middle", `font-size` = 9, fill = "#888",
                 sprintf("Every %s d · %s cancelled", fmt(ame_schedule_interval), fmt_pct(ame_failure_probability))),
      tags$text(x = cx, y = y + 37, `text-anchor` = "middle", `font-size` = 9, fill = "#888",
                 if (is.null(ame_airframe_label)) "" else ame_airframe_label)
    )
  }

  tags$svg(
    xmlns = "http://www.w3.org/2000/svg", viewBox = sprintf("0 0 %d %d", width, height),
    style = sprintf("width:100%%; max-width:%dpx; height:%dpx; display:block;", width, height),

    leg(y[1], y[2], -1, "#2a78d6",
        sprintf("PMVAmb: %s min", fmt(wia1_mode)),
        "+ dead-heading return"),
    leg(y[1], y[2], 1, "#6c757d",
        sprintf("HX240M: %s min", fmt(kia1_mode)),
        "+ dead-heading return"),
    leg(y[2], y[3], -1, "#1e824c",
        sprintf("R2B Evac Team: %s min", fmt(wia2_mode)),
        "+ dead-heading return"),
    leg(y[2], y[3], 1, "#922b21",
        sprintf("HX240M (mortuary): %s min", fmt(kia2_mode)),
        "+ dead-heading return"),
    leg(y[3], y[4], -1, "#6f42c1",
        sprintf("Critical: %s/sortie", fmt(ame_critical)),
        "ICU, CCATT/CCAST"),
    leg(y[3], y[4], 1, "#17a2b8",
        sprintf("Standard: %s/sortie", fmt(ame_standard)),
        "Hold, CSU-equivalent"),

    mortuary_marker(y[3], mort2e_mode),
    ame_schedule_note(y[4]),

    node(y[1], "R1", "Forward Aid Post"),
    node(y[2], "R2B", "Battalion Aid Post"),
    node(y[3], "R2E", "Field Hospital"),
    node(y[4], "AME", "Strategic Evac (Role 4)")
  )
}

#' Wrap evac_chain_diagram() with the heading/caption pattern matching
#' force_structure_diagram() (Health System Architecture), for the same
#' sticky-sidebar treatment on the Medevac panel.
#'
#' @param wia1_mode Modal R1 to R2B wounded transport duration, in minutes.
#' @param kia1_mode Modal R1 to mortuary transport duration, in minutes.
#' @param wia2_mode Modal R2B to R2E wounded transport duration, in minutes.
#' @param kia2_mode Modal R2B to mortuary transport duration, in minutes.
#' @param mort2e_mode Modal mortuary handling duration, in minutes.
#' @param ame_critical Critical-route places per strategic sortie.
#' @param ame_standard Standard-route places per strategic sortie.
#' @param ame_airframe_label Display label of the configured airframe.
#' @param ame_schedule_interval Days between scheduled sorties.
#' @param ame_failure_probability Probability a scheduled sortie is
#'   cancelled, carrying zero capacity.
#' @return A tag list of the heading, the chain diagram and its caption.
medevac_diagram <- function(wia1_mode, kia1_mode, wia2_mode, kia2_mode,
                             mort2e_mode, ame_critical = NA,
                             ame_standard = NA,
                             ame_airframe_label = NULL,
                             ame_schedule_interval = NA,
                             ame_failure_probability = NA) {
  tagList(
    h6(class = "text-muted mt-2", "Medevac Chain"),
    p(class = "text-muted", style = "font-size:11px;",
      "Each line is a transport leg (duration shown); every leg also has a return trip. ",
      tags$span(style = "color:#2a78d6; font-weight:600;", "Blue = WIA (PMVAmb)"), "; ",
      tags$span(style = "color:#6c757d; font-weight:600;", "grey = KIA (HX240M)"), "; ",
      tags$span(style = "color:#1e824c; font-weight:600;", "green = R2B → R2E WIA evac"),
      " (R2B's own Evac Team); ",
      tags$span(style = "color:#922b21; font-weight:600;", "maroon = R2B → R2E KIA/mortuary move"),
      " (mortuary is collocated with R2E, not R2B).",
      " R2B is bypassed straight to R2E when its OT beds are full, or the team is off-shift/busy.",
      " KIA reaching R2E go no further — ⚱ is a local mortuary transfer, not a vehicle leg.",
      " ", tags$span(style = "color:#6f42c1; font-weight:600;", "Purple = Critical AME (ICU bed)"),
      "; ", tags$span(style = "color:#17a2b8; font-weight:600;", "teal = Standard AME (Hold bed)"),
      " — post-surgery Priority 1 boards critical, everyone else boards standard; both pools fill on the same sortie",
      " (the airframe and its capacity are editable in the Strategic AME group above).",
      " Structural diagram only; run the saved configuration from the command line",
      " for queueing and casualty results."),
    evac_chain_diagram(wia1_mode, kia1_mode, wia2_mode, kia2_mode, mort2e_mode,
                        ame_critical, ame_standard, ame_airframe_label,
                        ame_schedule_interval, ame_failure_probability)
  )
}
