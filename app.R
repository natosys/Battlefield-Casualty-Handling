##############################################
## app.R                                    ##
## Battlefield Casualty Handling Simulation ##
## Shiny app — Configure / Run              ##
##############################################
#
# Launch with:
#   shiny::runApp("app.R")
#
# Three-panel workflow:
#   Getting Started — in-app onboarding guide (docs/Getting_Started.md),
#               rendered via includeMarkdown()
#   Configure — plain-English parameter editor grouped by operational concept,
#               with Save and Load Configuration
#   Run       — the command that runs the saved configuration at the full
#               protocol from a terminal
#
# The console builds and saves a configuration; it runs nothing. A run in a
# browser session records no configuration, seed or commit, so every run
# belongs to run.R and scripts/run_*.R, which do.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(jsonlite)
  library(ggplot2)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/sensitivity.R")
source("R/app_params.R")
# The console's parts are sourced into this file's own environment rather
# than the global one: runApp() evaluates app.R in an environment of its own,
# and the parts read the constants defined here (DEFAULT_JSON and others).
source("R/app_config_helpers.R", local = TRUE)
source("R/app_ui_fields.R", local = TRUE)
source("R/app_ui_diagrams.R", local = TRUE)
source("R/app_server_configure.R", local = TRUE)

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
    card(
      card_header("Run at Full Protocol"),
      p(class = "text-muted small",
        "Every run belongs in a terminal, where it records its configuration, seed and commit ",
        "beside its outputs. Save the configuration from the Configure tab under the file name ",
        "set there, then run this command from the repository root."),
      textInput("seed", "Random Seed", value = "42"),
      uiOutput("full_protocol_command_ui"),
      p(class = "text-muted small mt-2",
        "The README's Reproducing the Published Results section maps each published experiment ",
        "to its own script.")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────

#' Shiny server for the simulation console
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @return Invisibly NULL; called for the reactives and outputs it wires.
#' @details An orchestrator over the per-panel wiring functions, one per
#'   panel. A change to one panel belongs in that panel's function rather
#'   than here.
server <- function(input, output, session) {
  raw_env_data <- create_console_state()$raw_env_data

  # Configure panel
  fields_by_group <- wire_configure_panel(input, output, session)$fields_by_group

  # Casualty Intensity Profile selector. It offers exactly the scenario
  # profiles defined in the loaded env_data.json; resolve_scenario()
  # (R/scenario.R) overlays only the `vars` paths a profile defines, so
  # structural configuration is never touched. Its reactives are named here,
  # where the console's tests read them.
  scenario_selector_out <- wire_scenario_selector(fields_by_group, raw_env_data, input, output,
                                                  session)
  current_scenario <- scenario_selector_out$current_scenario
  dow_shape <- scenario_selector_out$dow_shape
  gen_distributions <- scenario_selector_out$gen_distributions
  mc_event_count <- scenario_selector_out$mc_event_count
  scenario_choices <- scenario_selector_out$scenario_choices
  scenario_json <- scenario_selector_out$scenario_json
  scenario_overridden_paths <- scenario_selector_out$scenario_overridden_paths

  # Run panel
  wire_full_protocol_command(input, output)
}

shinyApp(ui, server)
