##############################################
## app.R                                    ##
## Battlefield Casualty Handling Simulation ##
## Shiny app — Configure / Run / Analyse    ##
##                              ##
##############################################
#
# Launch with:
#   shiny::runApp("app.R")
#
# Four-panel workflow:
#   Getting Started — in-app onboarding guide (docs/Getting_Started.md),
#               rendered via includeMarkdown()
#   Configure — plain-English parameter editor grouped by operational concept,
#               with Save and Load Configuration
#   Run       — Quick Run (a single-replication check that the configuration
#               is sane) and the command that runs the saved configuration at
#               the full protocol from a terminal
#   Analyse   — result tabs rendered from R/analysis.R's ggplot objects, plus a
#               Sensitivity Screening tab holding the screened-parameter
#               reference table
#
# Replicated runs and sensitivity screens are not offered here: a run in a
# browser session records no configuration, seed or commit, and cannot reach
# the protocol the published experiments use. run.R and scripts/run_*.R do.

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
source("R/warmup.R")
source("R/app_params.R")
# The console's parts are sourced into this file's own environment rather
# than the global one: runApp() evaluates app.R in an environment of its own,
# and the parts read the constants defined here (DEFAULT_JSON and others).
source("R/app_config_helpers.R", local = TRUE)
source("R/app_ui_fields.R", local = TRUE)
source("R/app_ui_diagrams.R", local = TRUE)
source("R/app_server_configure.R", local = TRUE)
source("R/app_server_analyse.R", local = TRUE)
source("R/app_server_supplementary.R", local = TRUE)

# Quick Run calls run_once() directly with no inner forking, so a plain
# multisession future is sufficient: the worker is a separate process and the
# Shiny event loop stays responsive while the replication runs.
plan(multisession)
# run_once()/analyse_run() are called from inside future() with a seed
# supplied explicitly (or intentionally NULL for a random Quick Run); the
# future package's own parallel-RNG-safety warning does not apply here.
options(future.rng.onMisuse = "ignore")

APP_DIR         <- normalizePath(".")
DEFAULT_JSON    <- "env_data.json"

#' File name the Save Configuration download and the displayed command start from
DEFAULT_CONFIG_FILENAME <- "my_config.json"

#' Replications of the published experiments, in the displayed command
FULL_PROTOCOL_ITERATIONS <- 30L

#' Horizon, in days, of the published experiments, in the displayed command
FULL_PROTOCOL_DAYS <- 360L

PARAM_REGISTRY  <- build_param_registry()

TRI_TRIPLES <- detect_tri_triples(PARAM_REGISTRY)

GEN_STREAM_ACTYS <- c("wia_cbt", "kia_cbt", "dnbi_cbt", "wia_spt", "kia_spt", "dnbi_spt")

#' Colours and short labels for each three-way compositional split, in the
#' same left-to-right order as the slider's segments (0–handle1, handle1–
#' handle2, handle2–1). Consumed entirely by split_slider_recolor_script();
#' there is no separate server-rendered breakdown bar (removed — it was a
#' full duplicate of what the slider itself now shows).
SPLIT_SLIDER_META <- list(
  pri_split    = list(colors = c("#c0392b", "#e08e2d", "#2a78d6"),
                       labels = c("P1", "P2", "P3")),
  dnbi_split   = list(colors = c("#8e44ad", "#e08e2d", "#2a78d6"),
                       labels = c("BF", "Disease", "NBI")),
  mc_pri_split = list(colors = c("#c0392b", "#e08e2d", "#2a78d6"),
                       labels = c("P1", "P2", "P3"))
)
# One compositional-split slider per Scheduled Event Days row —
# generated rather than hand-written since CASUALTY_SURGE_SCHEDULE_SLOTS
# entries are all identical in colour/labels, just keyed by event index.
SPLIT_SLIDER_META <- c(SPLIT_SLIDER_META, setNames(
  lapply(seq_len(CASUALTY_SURGE_SCHEDULE_SLOTS), function(i) {
    list(colors = c("#c0392b", "#e08e2d", "#2a78d6"), labels = c("P1", "P2", "P3"))
  }),
  sprintf("mc_event_pri_split_%d", seq_len(CASUALTY_SURGE_SCHEDULE_SLOTS))
))

MORRIS_LABELS <- c(
  surg_mode              = "Surgery Duration (Mode)",
  long_resus_mode        = "Long Resuscitation Duration (Mode)",
  p1_p_max               = "Priority 1 DOW Ceiling",
  r1_transport           = "R1 Transport Time (Mode)",
  r2b_transport          = "R2B Transport Time (Mode)",
  stabilisation_icu_mode = "Stabilisation ICU Requirement (Mode)",
  pri1_surg_prob         = "Priority 1 Surgical Candidacy",
  evacuation_policy_days = "Theatre Evacuation Policy (Days)",
  ot_hours               = "OT Shift Length (Hours per Shift)",
  casualty_surge_rate     = "Casualty Surge Event Rate (per day)",
  casualty_surge_max_cas  = "Casualty Surge Event Size (Maximum)",

  # ── Labels for the parameters the full-coverage audit added ─────────────
  short_resus_mode            = "R2E Short Resuscitation Duration (Mode)",
  r2e_hold_mode                 = "R2E Base Recovery-to-Duty Duration (Mode)",
  post_op_hold_mode             = "R2E Post-Op Holding-Bed Duration (Mode)",
  r1_recovery_mode               = "R1 Battle Fatigue Hold Duration (Mode)",
  r1_wia_treat_mode              = "R1 WIA Treatment Time (Mode)",

  pri2_surg_prob            = "Priority 2 Surgical Candidacy",
  pri3_dnbi_surg_prob       = "Priority 3 DNBI Surgical Candidacy",
  pri3_other_surg_prob      = "Priority 3 Other Surgical Candidacy",
  disease_surgery_pct        = "Disease Surgical Candidacy",
  pri1_evac_prob             = "Priority 1 Strategic Evacuation Rate",
  pri2_evac_prob             = "Priority 2 Strategic Evacuation Rate",

  p1_p_base = "Priority 1 DOW Base Probability",
  p1_k      = "Priority 1 DOW Logistic Steepness",
  p1_t_mid  = "Priority 1 DOW Logistic Midpoint",
  p2_p_base = "Priority 2 DOW Base Probability",
  p2_p_max  = "Priority 2 DOW Ceiling",
  p2_k      = "Priority 2 DOW Logistic Steepness",
  p2_t_mid  = "Priority 2 DOW Logistic Midpoint",
  p3_flat   = "Priority 3 Flat DOW Probability",

  r1_tccc_factor           = "R1 TCCC Efficacy Factor",
  r2b_resus_factor         = "R2B/R2E DCR (Resus) Efficacy Factor",
  r2b_dcs_factor           = "R2B DCS Efficacy Factor",
  r2e_resus_factor         = "R2E DCR (Resus) Efficacy Factor",
  r2e_dcs1_factor          = "R2E DCS 1st-Op Efficacy Factor",
  r2e_dcs2_factor          = "R2E DCS 2nd-Op Efficacy Factor",
  r2e_postop_hold_penalty  = "R2E Post-Op Hold DOW Penalty (Multiplier)",
  r2b_icu_penalty          = "R2B Forward ICU DOW Penalty (Multiplier)",

  wia_cbt_mean  = "WIA — Combat Mean Daily Rate",
  kia_cbt_mean  = "KIA — Combat Mean Daily Rate",
  dnbi_cbt_mean = "DNBI — Combat Mean Daily Rate",
  wia_spt_mean  = "WIA — Support Mean Daily Rate",
  kia_spt_mean  = "KIA — Support Mean Daily Rate",
  dnbi_spt_mean = "DNBI — Support Mean Daily Rate",

  casualty_surge_min_cas = "Casualty Surge Event Size (Minimum)",

  fr_demand_interval_days = "Reinforcement Demand Cycle (Days)",
  fr_fulfillment_lag_days = "Reinforcement Fulfillment Lag (Days)",
  fr_fill_mode_frac       = "Reinforcement Fill Distribution (Mode)",

  ame_schedule_interval_days = "AME Sortie Interval (Days)",
  ame_failure_probability    = "AME Sortie Cancellation Probability",

  r2b_stability_window_dcs = "R2B Forward Stability Window, Damage Control (Minutes)",
  r2b_forward_hold_max = "R2B Forward Hold Time Limit (Minutes)",
  post_definitive_icu_mode = "R2E Post-Definitive ICU Duration (Mode)",
  r2b_hold_threshold  = "R2B Hold-Bed Reroute Threshold",
  r2b_pre_open_window = "R2B Pre-Open Hold Window (Minutes)",

  pri1_dcs_rate = "Priority 1 Damage Control Rate",
  pri2_dcs_rate = "Priority 2 Damage Control Rate",
  pri3_dcs_rate = "Priority 3 Damage Control Rate",

  # ── R2E establishment bed counts ────────────────────────────
  r2e_icu_beds  = "R2E Intensive Care Bed Count",
  r2e_hold_beds = "R2E Holding Bed Count",

  # ── Composition balance coordinates ──────────────────────────────────────
  # A balance carries a mu* for a contrast between parts, not for a single
  # named share, so each label names the two sides of its contrast — without
  # that, a reader has no way to tell what a high mu* on one of these means.
  triage_p1_balance    = "Triage Balance — Priority 1 against Priority 2 and 3",
  triage_p2_p3_balance = "Triage Balance — Priority 2 against Priority 3",
  dnbi_disease_balance = "DNBI Balance — Disease against Battle Fatigue and NBI",
  dnbi_bf_nbi_balance  = "DNBI Balance — Battle Fatigue against Non-Battle Injury",
  mc_p1_balance        = "Casualty Surge Triage Balance — Priority 1 against Priority 2 and 3",
  mc_p2_p3_balance     = "Casualty Surge Triage Balance — Priority 2 against Priority 3"
)

# ── UI helpers ────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = "Battlefield Casualty Handling — Simulation Console",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  id    = "main_nav",

  nav_panel(
    "Getting Started",
    div(style = "max-width: 900px; margin: 0 auto; padding: 1rem 0 3rem;",
        includeMarkdown("docs/Getting_Started.md"))
  ),

  nav_panel(
    "Configure",
    split_slider_recolor_script(SPLIT_SLIDER_META),
    p(class = "text-muted",
      "Adjust simulation parameters below, grouped by operational concept. ",
      "Hover the ", tags$b("ⓘ"), " icon next to any field for an explanation. ",
      "Sliders show the plausible range screened in the project's Morris sensitivity analysis."),
    fluidRow(
      column(4, uiOutput("scenario_selector_ui")),
      column(3, fileInput("upload_json", "Load Configuration (.json)", accept = ".json")),
      column(3, textInput("config_filename", "Configuration file name", value = DEFAULT_CONFIG_FILENAME),
             downloadButton("download_json", "Save Configuration", class = "btn-outline-secondary"))
    ),
    uiOutput("scenario_scope_note"),
    accordion(
      id = "config_accordion", open = c(GRP_FORCE),
      !!!lapply(c(GRP_FORCE, GRP_HEALTH_ARCH, GRP_LOGISTICS, GRP_PROVISION, GRP_CASUALTY, GRP_CASUALTY_SURGE), function(g) {
        sidebar_output_id <- if (identical(g, GRP_HEALTH_ARCH)) {
          "force_design_diagram"
        } else if (identical(g, GRP_LOGISTICS)) {
          "medevac_diagram"
        } else {
          NULL
        }
        if (!is.null(sidebar_output_id)) {
          accordion_panel(g,
            div(style = "display:flex; gap:20px; align-items:flex-start; flex-wrap:wrap;",
                div(style = "flex:0 0 280px; max-width:280px; position:sticky; top:12px;",
                    uiOutput(sidebar_output_id)),
                div(style = "flex:1; min-width:280px;",
                    uiOutput(paste0("group_ui_", make.names(g))))
            )
          )
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
        textInput("seed", "Random Seed (blank = random)", value = "42")
        # OT shift length is not offered here. It is a property of the health
        # system being simulated, not of this particular execution, so it is
        # edited in the Configure panel (Health System Architecture →
        # Surgical Shift Roster) along with every other model parameter. That
        # keeps it inside the configuration a run is saved and reproduced
        # from; a Run tab override would have governed the run without
        # appearing in the saved JSON.
      ),
      card(
        card_header("Quick Run"),
        p(class = "text-muted small",
          "A single replication to check that the configuration behaves sensibly before it is ",
          "run properly. Its results are for orientation, not for citation."),
        actionButton("run_quick", "\u25B6 Run Quick Run (Single Replication)", class = "btn-primary btn-lg"),
        tags$hr(),
        uiOutput("run_status"),
        tags$hr(),
        uiOutput("download_all_ui")
      ),
      card(
        card_header("Run at Full Protocol"),
        p(class = "text-muted small",
          "Replicated analysis belongs in a terminal, where the run records its configuration, ",
          "seed and commit beside its outputs. Save the configuration from the Configure tab under the ",
          "file name set there, then run this command from the repository root."),
        uiOutput("full_protocol_command_ui"),
        p(class = "text-muted small mt-2",
          "The README's Reproducing the Published Results section maps each published experiment ",
          "to its own script.")
      )
    )
  ),

  nav_panel(
    "Analyse",
    shrink_to_fit_script(),
    bch_shrink_to_fit_css(),
    p(class = "text-muted small",
      "New to these results? ",
      actionLink("goto_getting_started", "See Getting Started for how to read each graph below.")),
    uiOutput("analyse_body")
  )
)

# ── Server ────────────────────────────────────────────────────────────────

#' Shiny server for the simulation console
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @return Invisibly NULL; called for the reactives and outputs it wires.
#' @details An orchestrator over the per-panel wiring functions above, one
#'   per tab and one per asynchronous run. A change to one panel belongs in
#'   that panel's function rather than here.
server <- function(input, output, session) {

  # preamble
  console_state_out <- create_console_state(input, session)
  analysis_results <- console_state_out$analysis_results
  mon_data <- console_state_out$mon_data
  pending_future <- console_state_out$pending_future
  progress_pct <- console_state_out$progress_pct
  raw_env_data <- console_state_out$raw_env_data
  run_error <- console_state_out$run_error
  run_state <- console_state_out$run_state
  startup_json <- console_state_out$startup_json

  # ── Configure panel ───────────────────────────────────────────────────────

  # Configure panel
  configure_panel_out <- wire_configure_panel(input, output, session)
  fields_by_group <- configure_panel_out$fields_by_group
  slider_field_ids <- configure_panel_out$slider_field_ids
  tri_mode_ids <- configure_panel_out$tri_mode_ids

  # ── Casualty Intensity Profile selector ──────────────────────────────────
  # Offers exactly the scenario profiles defined in the loaded env_data.json
  # (today: "default" plus whatever is under its `scenarios` block) — no
  # fabricated intermediate tiers. resolve_scenario() (R/scenario.R) only
  # overlays the `vars` paths a given scenario actually defines; structural
  # config (force size, team/bed counts, transport fleet) is never touched.

  # env_data.json's scenario labels carry a full descriptive parenthetical
  # (e.g. "Moderate Intensity — Falklands 1982 (Operation CORPORATE, British
  # Task Force, South Atlantic)") intended for the README/CSV outputs
  # (R/scenario.R's active_scenario_label, R/scenario_runner.R) — kept
  # unchanged there since that is the citation-quality name. The dropdown
  # instead uses SCENARIO_DROPDOWN_LABELS below: a display-only override,
  # not a rename of the underlying scenario identity.

  # Casualty Intensity Profile selector
  scenario_selector_out <- wire_scenario_selector(fields_by_group, raw_env_data, input, output,
                                                  session)
  current_json <- scenario_selector_out$current_json
  current_scenario <- scenario_selector_out$current_scenario
  dow_shape <- scenario_selector_out$dow_shape
  gen_distributions <- scenario_selector_out$gen_distributions
  mc_event_count <- scenario_selector_out$mc_event_count
  scenario_choices <- scenario_selector_out$scenario_choices
  SCENARIO_DROPDOWN_LABELS <- scenario_selector_out$SCENARIO_DROPDOWN_LABELS
  scenario_json <- scenario_selector_out$scenario_json
  scenario_overridden_paths <- scenario_selector_out$scenario_overridden_paths
  shorten_scenario_label <- scenario_selector_out$shorten_scenario_label

  # ── Validation ────────────────────────────────────────────────────────────

  # Validation
  validate_config <- build_config_validator()

  # ── Quick Run execution (async via future + promises) ────────────────────

  # Quick Run execution (async via future + promises)
  wire_quick_run(analysis_results, current_json, mon_data, pending_future, progress_pct, run_error,
                 run_state, scenario_json, validate_config, input)

  # ── Full Analysis execution (async via future + promises) ────────────────

  # Full Analysis execution (async via future + promises)
  wire_run_status(progress_pct, run_error, run_state, input, output, session)
  wire_full_protocol_command(input, output)

  # ── Analyse panel ─────────────────────────────────────────────────────────

  # tab_plot() previously combined Queue Depths' 3 panels and Quick Run's
  # Bed & Resource Utilisation's 4 panels into one patchwork image each; both
  # are split into individually shrink-to-fit plots so each panel can be shrunk against its own
  # full viewport-height
  # budget, rather than every constituent plot in a combined image sharing
  # (and being squashed to fit) one. Only the three tabs that are still ever
  # a single combined/standalone plot remain here; every split-out panel is
  # read directly from analysis_results() at its own new_shrink_to_fit_plot()
  # call instead.

  # Analyse panel
  analyse_panel_out <- wire_analyse_panel(analysis_results)
  gantt_min_section_height_px <- analyse_panel_out$gantt_min_section_height_px
  gantt_row_height_px <- analyse_panel_out$gantt_row_height_px
  tab_plot <- analyse_panel_out$tab_plot
  utilisation_panel_heights <- analyse_panel_out$utilisation_panel_heights

  # ── Shrink-to-fit plot sizing ───────────────────────────────────────────
  # Every plot in the Analyse tab (Quick Run and Full Analysis alike)
  # defaults to a size that fits the user's browser viewport without page
  # scrolling, while remaining fully readable on demand via an "Expand"
  # link. Each plot still renders server-side at its own natural, data-
  # driven height (e.g. utilisation_panel_heights() above) so row/label
  # geometry is computed at full size; only
  # the *displayed* size is capped, client-side, by shrink_to_fit_script()
  # (see its roxygen for why this is done in JS rather than by feeding a
  # viewport height back into a reactive renderUI). chrome_px is a per-tab
  # allowance for the surrounding navbar/tab-strip/heading/download-button
  # chrome that a plot's own container does not occupy; tabs with an
  # explanatory paragraph above the plot use the larger allowance.

  # Shrink-to-fit plot sizing
  analyse_outputs_out <- wire_analyse_outputs(analysis_results, mon_data, run_state, tab_plot,
                                              utilisation_panel_heights, input, output)
  ANALYSE_PLOT_CHROME_PX <- analyse_outputs_out$ANALYSE_PLOT_CHROME_PX
  ANALYSE_PLOT_CHROME_WITH_HEADING_PX <- analyse_outputs_out$ANALYSE_PLOT_CHROME_WITH_HEADING_PX
  ANALYSE_PLOT_CHROME_WITH_INTRO_PX <- analyse_outputs_out$ANALYSE_PLOT_CHROME_WITH_INTRO_PX
  echelon_ot_utilisation_csv <- analyse_outputs_out$echelon_ot_utilisation_csv
  filtered_resources_csv <- analyse_outputs_out$filtered_resources_csv
  new_shrink_to_fit_plot <- analyse_outputs_out$new_shrink_to_fit_plot
  plot_download_handler <- analyse_outputs_out$plot_download_handler
  shrink_to_fit_plot_ui <- analyse_outputs_out$shrink_to_fit_plot_ui

  # ── analyse_run() outputs and their Full Analysis mean +/- 95% CI
  # equivalents from analyse_replications() ───────────────────────────────
  # analyse_run() returns bare scalars/simple data frames (one seed, one
  # value); analyse_replications() returns the *_ci-suffixed equivalent —
  # a mean +/- 95% CI across replications instead (see the new block in
  # R/analysis.R, right after kpi_summary). Every renderer below branches
  # on run_mode() to pick the correctly-shaped source field.

  # analyse_run() outputs no other tab shows, and the helpers they share
  supplementary_outputs_out <- wire_supplementary_outputs(analysis_results, output)
  calibration_df <- supplementary_outputs_out$calibration_df
  count_value_card <- supplementary_outputs_out$count_value_card
  small_dt <- supplementary_outputs_out$small_dt
}

shinyApp(ui, server)
