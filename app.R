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

#' Reduce a typed file name to one safe to download and to paste into a shell
#'
#' @param name Text typed into the configuration file name box.
#' @return A bare `.json` file name of letters, digits, dots, hyphens and
#'   underscores, `DEFAULT_CONFIG_FILENAME` where nothing usable remains.
#' @details Any directory part is dropped and every other character replaced,
#'   so the name needs no quoting where `full_protocol_command()` puts it.
sanitise_config_filename <- function(name) {
  name <- if (is.null(name) || length(name) != 1 || is.na(name)) "" else basename(trimws(name))
  name <- gsub("[^A-Za-z0-9._-]", "_", name)
  name <- sub("\\.json$", "", name, ignore.case = TRUE)
  name <- sub("^[._-]+", "", name)
  if (!nzchar(name)) return(DEFAULT_CONFIG_FILENAME)
  paste0(name, ".json")
}

#' The terminal command that runs a saved configuration at the full protocol
#'
#' @param config_filename File name the configuration was saved under.
#' @param seed Text of the console's seed box.
#' @return A one-line `Rscript run.R` command at the replication count and
#'   horizon of the published experiments.
#' @details A saved file already holds the profile in force, so no `--scenario`
#'   is given. A blank or non-integer seed falls back to the published seed 42
#'   rather than the console's random draw, a cited run needing a seed to repeat.
full_protocol_command <- function(config_filename, seed) {
  seed_val <- suppressWarnings(as.integer(trimws(if (is.null(seed)) "" else seed)))
  if (length(seed_val) != 1 || is.na(seed_val)) seed_val <- 42L
  sprintf("Rscript run.R --config %s --seed %d --iterations %d --days %d",
          sanitise_config_filename(config_filename), seed_val,
          FULL_PROTOCOL_ITERATIONS, FULL_PROTOCOL_DAYS)
}

PARAM_REGISTRY  <- build_param_registry()

#' Detect every triangular (min/mode/max) field triple in a registry, by
#' shared id prefix (tri_fields() in R/app_params.R always names them
#' "<prefix>_min"/"<prefix>_mode"/"<prefix>_max"). Computed once from the
#' full registry so every triangular field group in the app — not just
#' one hand-picked group — gets a live distribution curve card (see
#' render_field_grid()) without hardcoding each one by name.
#'
#' @return Named list (name = prefix); each value has min_id/mode_id/
#'   max_id and label (the mode field's label with its " — Most Likely
#'   (Mode)" suffix stripped, used as the curve card's header).
#' @param registry List of field specs from build_param_registry().
detect_tri_triples <- function(registry) {
  ids <- vapply(registry, function(f) f$id, character(1))
  mode_ids <- grep("_mode$", ids, value = TRUE)
  prefixes <- sub("_mode$", "", mode_ids)
  keep <- vapply(prefixes, function(p) paste0(p, "_min") %in% ids && paste0(p, "_max") %in% ids, logical(1))
  prefixes <- prefixes[keep]
  setNames(lapply(prefixes, function(p) {
    mode_f <- registry[[which(ids == paste0(p, "_mode"))]]
    list(min_id = paste0(p, "_min"), mode_id = paste0(p, "_mode"), max_id = paste0(p, "_max"),
         # useBytes: the pattern is non-ASCII, so a locale-dependent match
         # would break if the label ever moved into env_data.json
         label = sub(" — Most Likely \\(Mode\\)$", "", mode_f$label, useBytes = TRUE))
  }), prefixes)
}
TRI_TRIPLES <- detect_tri_triples(PARAM_REGISTRY)

GEN_STREAM_ACTYS <- c("wia_cbt", "kia_cbt", "dnbi_cbt", "wia_spt", "kia_spt", "dnbi_spt")

#' Render a small density-curve preview for a casualty generation stream
#'
#' @details Mirrors the exact distributional transform used by
#'   make_ln_arrival_generator()/make_exp_arrival_generator() (R/environment.R),
#'   so the curve shown is the real shape that mean_daily/sd_daily imply — not a
#'   generic approximation. distribution is read live from the resolved
#'   scenario JSON (not user-editable), so switching intensity profiles
#'   changes the curve shape for the streams that scenario actually
#'   overrides (see README Scenario Profiles) and leaves the others alone.
#' @param mean_daily Configured mean arrivals per day for the stream.
#' @param sd_daily Configured between-day standard deviation.
#' @param distribution Distribution family, "lognormal" or "exponential".
#' @return A ggplot of the implied daily arrival density, blank where the
#'   configured values do not describe a distribution.
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

#' Render a small density-curve preview for a triangular-distribution
#' field group (min/mode/max), matching the visual language of
#' render_gen_curve() for the casualty-generation streams.
#'
#' @details Uses `triangle::dtriangle()` — the same package the
#'   simulation itself draws durations from via `rtriangle()`
#'   (R/trajectories.R), so the curve shown is the exact shape those
#'   three numbers imply, not an approximation.
#' @param mn Minimum of the distribution, in minutes.
#' @param mode Mode of the distribution, in minutes.
#' @param mx Maximum of the distribution, in minutes.
#' @return A ggplot of the triangular density, blank where the three values
#'   are not ordered or do not span a range.
render_tri_curve <- function(mn, mode, mx) {
  invalid <- is.null(mn) || is.null(mode) || is.null(mx) ||
    is.na(mn) || is.na(mode) || is.na(mx) ||
    !(mn <= mode && mode <= mx) || mn == mx
  if (invalid) return(ggplot() + theme_void())
  x <- seq(mn, mx, length.out = 200)
  y <- triangle::dtriangle(x, a = mn, b = mx, c = mode)
  ggplot(data.frame(x = x, y = y), aes(x, y)) +
    geom_area(fill = "#2a78d6", alpha = 0.25) +
    geom_line(color = "#2a78d6", linewidth = 0.9) +
    geom_vline(xintercept = mode, linetype = "dashed", color = "#c0392b", linewidth = 0.5) +
    labs(x = NULL, y = NULL, subtitle = "Triangular") +
    theme_minimal(base_size = 9) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.minor = element_blank(),
          plot.subtitle = element_text(size = 8, color = "#888888"),
          plot.margin = margin(2, 4, 2, 4))
}

#' Render a small live curve preview of the DOW survival function F(t) for
#' one priority cohort
#'
#' @details Uses dow_prob() (R/trajectories_common.R) directly, so the curve shown
#'   is the exact shifted-logistic function the simulation evaluates at
#'   each DOW check — not an approximation. p_max is the live Configure
#'   slider value for this priority; p_base, k, and t_mid are read from the
#'   resolved scenario JSON and are not user-editable in this app (see the
#'   read-only fields alongside the curve), so the curve's ceiling moves as
#'   the slider is dragged while its floor and shape track whichever
#'   Casualty Intensity Profile is active.
#' @param p_base,p_max,k,t_mid Parameters as for dow_prob() (R/trajectories_common.R).
#' @param window_max Upper bound of the plotted time window in minutes; when
#'   NULL (default), sized to comfortably show the curve reach its plateau.
#' @return A ggplot of the survival function, blank where the parameters do
#'   not describe a rising logistic.
render_dow_curve <- function(p_base, p_max, k, t_mid, window_max = NULL) {
  invalid <- is.null(p_base) || is.null(p_max) || is.null(k) || is.null(t_mid) ||
    any(is.na(c(p_base, p_max, k, t_mid))) || p_max <= p_base || k <= 0
  if (invalid) return(ggplot() + theme_void())
  if (is.null(window_max)) window_max <- t_mid + 6 / k
  x <- seq(0, window_max, length.out = 200)
  y <- dow_prob(x, p_base, p_max, k, t_mid)
  ggplot(data.frame(x = x, y = y), aes(x, y)) +
    geom_area(fill = "#c0392b", alpha = 0.15) +
    geom_line(color = "#c0392b", linewidth = 0.9) +
    geom_hline(yintercept = p_max, linetype = "dashed", color = "#888888", linewidth = 0.5) +
    geom_vline(xintercept = t_mid, linetype = "dotted", color = "#888888", linewidth = 0.5) +
    scale_y_continuous(limits = c(0, max(p_max * 1.1, 1e-3)), labels = scales::percent) +
    labs(x = NULL, y = NULL, subtitle = "DOW probability F(t) vs. minutes since injury") +
    theme_minimal(base_size = 9) +
    theme(axis.text.y = element_text(size = 6), axis.ticks.y = element_blank(),
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
#' @return A script tag installing the client-side recolouring.
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

#' Client-side "shrink-to-fit" scaling for Analyse-tab plots.
#'
#' Every Analyse-tab plot still renders server-side at its own natural,
#' data-driven pixel height (e.g. the Gantt row-count height from Issue
#' #111) via an explicit, non-"auto" `renderPlot(height = ...)` (see
#' `new_shrink_to_fit_plot()`) — so the full-detail image is always
#' generated at the same resolution regardless of how it is later
#' displayed, and Shiny is never asked to auto-detect a size from (and so
#' never re-renders in response to) the container's on-screen size. This
#' script instead shrinks a `.bch-shrink-fit` container's CSS *height*
#' only; a matching stylesheet rule (`bch_shrink_to_fit_css()`, in the same
#' `nav_panel`) makes the `<img>` inside track that height at `height:100%`
#' with `width:auto`, so the browser itself scales the already-rendered,
#' full-detail image down losslessly (as a photo, not a re-render) to fit —
#' preserving every relative proportion, so the computed row/label spacing
#' cannot re-overlap as a side effect of shrinking — and centres it via
#' `margin: 0 auto`, since its auto-computed display width will generally
#' be narrower than the container once shrunk. An earlier version of this
#' script tried two other approaches that were each dropped for a distinct
#' failure mode: (1) leaving `renderPlot()` on "auto" sizing so Shiny's own
#' redraw would fill a JS-shrunk container — this genuinely refits the
#' image, but ggplot's fixed font sizes then take up a much larger fraction
#' of a heavily-shrunk canvas, reintroducing label/row overlap; (2) a CSS
#' `transform: scale()` on a full-size render — but Shiny's resize-sensing
#' measures a container's on-screen (transformed) size, not its pre-
#' transform layout size, so it would eventually re-render at the already-
#' transformed (shrunk) size while the transform remained active,
#' compounding into a progressively narrower, uncentred image. Deliberately
#' done client-side (rather than by feeding a Shiny `input$window_height`
#' back into a `renderUI`) so a window resize only restyles the existing
#' DOM nodes — it does not tear down and rebuild the surrounding
#' `navset_tab`, which would otherwise reset the user's selected tab on
#' every resize. The "Expand" link next to each plot (wired server-side,
#' see `new_shrink_to_fit_plot()`) opens the same plot at full natural size
#' in a modal for users who find the shrunk version too dense.
#'
#' Each `.bch-shrink-fit` container is sized independently, against the
#' *full* viewport height budget, not shared with any other plot on the
#' page — even where several used-to-be-combined panels (e.g. Queue
#' Depths' R1/R2B/R2E panels, split apart from one combined image so
#' shrinking wouldn't squash them all into a single tiny image) are now
#' stacked in the same tab. The page as a whole is free to scroll *between*
#' plots in that case; what this script guarantees is that a user is never
#' forced to scroll *within* a single plot to see the rest of it.
#'
#' @return A script tag installing the client-side shrinking.
shrink_to_fit_script <- function() {
  tags$script(HTML("
(function() {
  function applyShrink(el) {
    var natural = parseFloat(el.getAttribute('data-natural-height'));
    var chrome  = parseFloat(el.getAttribute('data-chrome-px')) || 260;
    if (!natural || natural <= 0) return;
    var avail  = Math.max(window.innerHeight - chrome, 200);
    var factor = Math.min(1, avail / natural);
    el.style.height = Math.round(natural * factor) + 'px';
  }
  function applyAll() {
    document.querySelectorAll('.bch-shrink-fit').forEach(applyShrink);
  }
  $(document).on('shiny:connected', applyAll);
  $(document).on('shiny:value', applyAll);
  var resizeTimer;
  window.addEventListener('resize', function() {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(applyAll, 150);
  });
})();
"))
}

#' Stylesheet paired with shrink_to_fit_script(): makes every plot image
#' inside a `.bch-shrink-fit` container track that container's (JS-managed)
#' height while auto-computing and centring its width, rather than being
#' stretched or left clinging to one edge. `!important` is needed since
#' Shiny's `plotOutput()` sets the `<img>`'s own inline width/height styles
#' directly, which would otherwise take precedence over a stylesheet rule.
#'
#' @return A style tag holding the paired stylesheet.
bch_shrink_to_fit_css <- function() {
  tags$style(HTML("
.bch-shrink-fit { overflow: hidden; }
.bch-shrink-fit img {
  height: 100% !important;
  width: auto !important;
  max-width: 100%;
  display: block;
  margin: 0 auto;
}
"))
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
#' @return The values list with the three part ids set from the slider's two
#'   breakpoints, unchanged where the slider is absent
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
#'
#' @param values Named list from reactiveValuesToList(input)
#' @return The values list with every slider expanded into its three parts
inject_all_splits <- function(values) {
  values <- inject_split(values, "pri_split",  c("pri_one", "pri_two", "pri_three"))
  values <- inject_split(values, "dnbi_split", c("dnbi_bf_pct", "dnbi_disease_pct", "dnbi_nbi_pct"))
  values <- inject_split(values, "mc_pri_split", c("mc_pri_one", "mc_pri_two", "mc_pri_three"))
  for (i in seq_len(CASUALTY_SURGE_SCHEDULE_SLOTS)) {
    values <- inject_split(values, sprintf("mc_event_pri_split_%d", i),
                           sprintf(c("mc_sched_pri_one_%d", "mc_sched_pri_two_%d", "mc_sched_pri_three_%d"), i))
  }
  values
}

#' Fill in any registry field missing from `values` with its scenario-
#' resolved default. A field is missing whenever its Configure
#' accordion panel has never been opened in this session — deferred
#' rendering (see the group_ui_* renderUI loop in server()) means such a
#' field's input never bound, so it is simply absent from `values`, not
#' present with a NULL/default value Shiny populated itself.
#' apply_registry_values() already falls back to `json` (scenario_json())
#' for any field missing from its own `values` argument, so current_json()
#' — and everything built from it (Quick Run, Full Analysis, Morris,
#' Sobol, Save Configuration) — is unaffected by deferred rendering.
#' validate_config() runs on the flat `values` list *before* that merge,
#' though, so it needs the same fallback applied explicitly here or it
#' would see an absent field as an error-worthy NULL rather than a valid,
#' merely-unedited default.
#'
#' @param values Named list from inject_all_splits(reactiveValuesToList(input))
#' @param registry List of field specs from build_param_registry()
#' @param json Parsed, scenario-resolved env_data.json (scenario_json())
#' @return The values list with every absent field filled from its default
fill_missing_defaults <- function(values, registry, json) {
  defaults <- registry_defaults(registry, json)
  for (id in names(defaults)) {
    if (is.null(values[[id]])) values[[id]] <- defaults[[id]]
  }
  values
}

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
#' configuration, only Quick Run can. The caption says as much, so the
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
      "Each node is one deployed team; lines show pooled routing to the next echelon, not fixed lanes. This is structural, not a simulated outcome — run Quick Run for actual queueing and casualty results."),
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
      " Structural diagram only — run Quick Run for actual queueing and casualty results."),
    evac_chain_diagram(wia1_mode, kia1_mode, wia2_mode, kia2_mode, mort2e_mode,
                        ame_critical, ame_standard, ame_airframe_label,
                        ame_schedule_interval, ame_failure_probability)
  )
}

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

# ── UI ────────────────────────────────────────────────────────────────────

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

#' Create every reactive value the console's panels share
#'
#' @param input See analyse_run().
#' @param session See analyse_run().
#' @return A list of `analysis_results`, `mon_data`, `pending_future`, `progress_pct`,
#'   `raw_env_data`, `run_error`, `run_state`, `startup_json`.
create_console_state <- function(input, session) {
  observeEvent(input$goto_getting_started, {
    updateNavbarPage(session, "main_nav", selected = "Getting Started")
  })

  # Both boundaries where a configuration enters the app validate it before
  # anything reads it: a malformed file names its offending field here rather
  # than surfacing as a subscript error inside build_environment() once a run
  # has already started. A malformed file on disk at startup is
  # unrecoverable within the session, so it stops; a malformed upload leaves
  # the loaded configuration in place and reports through the UI.
  startup_json <- fromJSON(DEFAULT_JSON, simplifyVector = FALSE)
  validate_env_data_json(startup_json, DEFAULT_JSON)

  raw_env_data     <- reactiveVal(startup_json)
  run_state        <- reactiveVal("idle")   # idle | running | done | error
  run_error        <- reactiveVal(NULL)
  progress_pct     <- reactiveVal(0)
  mon_data         <- reactiveVal(NULL)
  analysis_results <- reactiveVal(NULL)
  pending_future   <- reactiveVal(NULL)
  list(
    analysis_results = analysis_results,
    mon_data = mon_data,
    pending_future = pending_future,
    progress_pct = progress_pct,
    raw_env_data = raw_env_data,
    run_error = run_error,
    run_state = run_state,
    startup_json = startup_json
  )
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
  # when Morris-screened (f$morris == TRUE): render_field_grid() always
  # renders a detected triangular triple's min/mode/max as plain compact
  # inputs (tri_input_row()), never a slider, so there is no "<id>_txt"
  # companion for this sync to wire up.
  tri_mode_ids <- vapply(TRI_TRIPLES, `[[`, character(1), "mode_id")
  slider_field_ids <- vapply(
    Filter(function(f) (isTRUE(f$morris) || isTRUE(f$slider)) && !(f$id %in% tri_mode_ids), PARAM_REGISTRY),
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
  # and its own died-of-wounds model; "high_intensity_modern_care" keeps those
  # casualty rates under the shipped treatment model. A scenario id absent from this map
  # falls back to its own env_data.json label.
  SCENARIO_DROPDOWN_LABELS <- c(
    default         = "Moderate Intensity",
    high_intensity  = "High Intensity",
    high_intensity_modern_care = "High Intensity, Modern Care"
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
  # Intensity Profile is active exactly as its ceiling (p_max) does.
  dow_shape <- reactive({
    sj <- scenario_json()
    setNames(lapply(c("p1", "p2"), function(pr) {
      list(
        p_base = get_raw_var(sj, "dow", "params", paste0(pr, "_p_base")),
        k      = get_raw_var(sj, "dow", "params", paste0(pr, "_k")),
        t_mid  = get_raw_var(sj, "dow", "params", paste0(pr, "_t_mid"))
      )
    }), c("p1", "p2"))
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
      # otherwise keep firing silently in Quick Run / Save Configuration.
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
    # correctly capturable by Save Configuration/Quick Run/Full
    # Analysis/Morris/Sobol regardless of which panels are open, exactly as
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

#' Build the validator a run's configuration must pass
#'
#' @return The validate_config object.
build_config_validator <- function() {
  #' Validate a Configure panel state before a run is dispatched
  #'
  #' @param values Named list of the panel's current input values.
  #' @return A character vector of error messages, empty when the
  #'   configuration is usable.
  #' @details Every fault is collected rather than the first being returned,
  #'   so a user correcting one does not then meet the next.
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

    # Casualty surge casualties-per-event min/max pairs — the
    # shared Poisson-mode pair and every Scheduled Event Days row, whether
    # or not that row is currently visible (a hidden row is always reset to
    # defaults, but validating it regardless costs nothing and stays
    # correct if that assumption ever changes).
    mc_min_max_ids <- c(list(c("mc_min_cas", "mc_max_cas")),
                        lapply(seq_len(CASUALTY_SURGE_SCHEDULE_SLOTS), function(i) {
                          sprintf(c("mc_sched_min_cas_%d", "mc_sched_max_cas_%d"), i)
                        }))
    for (ids in mc_min_max_ids) {
      mn <- values[[ids[1]]]; mx <- values[[ids[2]]]
      if (!is.null(mn) && !is.null(mx) && mn > mx) {
        errors <- c(errors, sprintf("%s: minimum casualties (%s) must be ≤ maximum casualties (%s).",
                                     sub("_min_cas$|_min$", "", ids[1]), mn, mx))
      }
    }

    # Triage priority split and DNBI sub-type split have no corresponding
    # check here — pri_split and dnbi_split (two-handle range sliders, see
    # render_group_body()) make summing to 1 a structural guarantee rather
    # than something to validate after the fact.

    errors
  }
  validate_config
}

#' Wire the Quick Run button to a single-replication run
#'
#' @param analysis_results See analyse_run().
#' @param current_json See analyse_run().
#' @param mon_data See analyse_run().
#' @param pending_future See analyse_run().
#' @param progress_pct See analyse_run().
#' @param run_error See analyse_run().
#' @param run_state See analyse_run().
#' @param scenario_json See analyse_run().
#' @param validate_config See analyse_run().
#' @param input See analyse_run().
#' @return Invisibly NULL; called for the files it writes.
wire_quick_run <- function(analysis_results, current_json, mon_data, pending_future, progress_pct,
                           run_error, run_state, scenario_json, validate_config, input) {
  observeEvent(input$run_quick, {
    values <- inject_all_splits(reactiveValuesToList(input))
    values <- fill_missing_defaults(values, PARAM_REGISTRY, scenario_json())
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
      day_min  <<- DAY_MIN
      counts   <<- sapply(env_data$elms, length)

      wrapped <- run_once(days_val, seed = seed_val, write_files = FALSE)
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
  invisible(NULL)
}

#' Wire the Quick Run progress ticker and run status message
#'
#' @param progress_pct See wire_quick_run().
#' @param run_error See wire_quick_run().
#' @param run_state See wire_quick_run().
#' @param input See wire_quick_run().
#' @param output See wire_quick_run().
#' @param session See wire_quick_run().
#' @return Invisibly NULL; called for the outputs it registers.
#' @details The ticker is simulated, since the future runs in a separate process
#'   and a single replication offers no finer progress than start and done: it
#'   ticks toward 90% while the run is in flight and the completion handler
#'   sets 100%.
wire_run_status <- function(progress_pct, run_error, run_state, input, output, session) {
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
        p(sprintf("Running Quick Run (seed %s, %d days)...",
                  if (trimws(input$seed) == "") "random" else trimws(input$seed), input$n_days)),
        div(class = "progress",
            div(class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar", style = sprintf("width: %d%%", progress_pct())))
      )
    } else if (state == "done") {
      div(class = "alert alert-success", "Run complete \u2014 see the Analyse tab for results.")
    } else {
      div(class = "alert alert-danger", paste("Run failed:", run_error()))
    }
  })
  invisible(NULL)
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
