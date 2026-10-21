##############################################
## R/app_config_helpers.R                 ##
## Configure panel helpers                ##
##############################################
#
# Helpers the Configure panel draws on: the saved configuration's file name
# and full-protocol command, the triangular field detection, the density
# previews, the compositional split sliders and their scripts.
#
# Sourced by app.R after the packages and modules it loads; source that
# file rather than this one alone.

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

#' Merge a two-handle range-slider value into a values list as three
#' underlying field values, matching the ids apply_registry_values()
#' expects. Needed because the Configure panel replaces
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
