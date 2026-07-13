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
#   Run       — Quick Run (single replication) and Full Analysis (multi-run,
#               95% CI)
#   Analyse   — four result tabs (rendered from R/analysis.R's ggplot
#               objects) plus a Sensitivity Calibration tab hosting Morris
#               screening, Sobol decomposition, and the Transport Fleet
#               Capacity Margin Sweep (Issue #57)
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
source("R/warmup.R")
source("R/app_params.R")

# Both future backends are unsafe for a future body that itself calls
# mclapply() (as run_replications()/run_morris()/run_sobol() do), for two
# different reasons (Issue #15 follow-up): multisession runs the future in
# a separate process connected over a socket, and a grandchild forked from
# inside it inherits a copy of that control socket, which a grandchild
# exiting mid-run can desynchronise — observed as the worker being reported
# "interrupted" with no OOM or R-level cause. multicore forks the Shiny
# process directly, including its live httpuv event loop; a forked child
# can inherit a lock httpuv held on another thread at the instant of the
# fork, a lock nothing in the child will ever release — observed as the
# future hanging indefinitely with near-zero CPU in the forked process,
# never erroring or completing. Full Analysis / Morris / Sobol therefore no
# longer call these functions in-process at all — see run_shiny_worker()
# below, which shells out to scripts/shiny_worker.R via system2(). That
# subprocess is a genuine fork()-then-exec (like any process launch),
# replacing the child's memory image entirely rather than duplicating this
# process's state, so it carries neither risk; mclapply inside it is exactly
# as safe as it already is for run.R's CLI path. Quick Run calls run_once()
# directly with no inner forking, so it is unaffected by either failure mode
# and keeps running in-process. Plain multisession is therefore sufficient
# and correct for every future() in this app now — no per-future strategy
# selection needed.
plan(multisession)
# run_once()/analyse_run() are called from inside future() with a seed
# supplied explicitly (or intentionally NULL for a random Quick Run); the
# future package's own parallel-RNG-safety warning does not apply here.
options(future.rng.onMisuse = "ignore")

APP_DIR         <- normalizePath(".")
DEFAULT_JSON    <- "env_data.json"

#' Detect a safe number of concurrent mclapply forks for *this* machine or
#' container, at *this* moment, instead of guessing a fixed number.
#'
#' @param n_days Simulation duration (days) for the run this cap is being
#'   computed for. Each forked worker's peak memory scales with how much
#'   monitoring data (arrivals/attributes/resources) accumulates over the
#'   run, which scales with duration — a fixed per-worker estimate
#'   calibrated at one duration understates the requirement at a longer one.
#'   Default 30 matches this app's own default simulation duration and the
#'   duration `mem_per_worker_base_mb` was directly measured at.
#' @param mem_per_worker_base_mb Assumed peak RSS of one forked replication
#'   worker at a 30-day run, in MB. Default 900 is the directly-measured
#'   mid-simulation RSS of a single forked worker at n_days = 30 (Issue #15
#'   follow-up — the crash this guards against was memory exhaustion, not a
#'   CPU-count problem, so bounding by detected core count alone is
#'   insufficient). Scaled linearly by `n_days / 30` below, floored at 400MB
#'   (package-loading overhead alone — simmer, ggplot2, dplyr, data.table,
#'   etc. — is substantial even for a very short run).
#' @return Integer >= 1.
#'
#' @details getOption("mc.cores", parallel::detectCores(logical = FALSE))
#'   respects the mc.cores option the project's Dockerfile sets in
#'   Rprofile.site (physical core count), falling back to
#'   detectCores(logical = FALSE) outside that container — either way, a
#'   more conservative starting point than parallel::detectCores() alone,
#'   which reports the host's — or, inside a local Docker Desktop dev
#'   container without an explicit CPU limit, the container's full
#'   VM-visible, hyperthread-inclusive — core count. That number bears no
#'   relation to how much memory is actually available to run that many
#'   concurrent forked R sessions (each carrying a full duplicate
#'   simmer/ggplot2/dplyr/data.table session). Forking one such session per
#'   detected core has been observed to exhaust a local dev container's
#'   memory and crash the
#'   whole container even at a modest replication count. Two rounds of fixed
#'   guessed caps (4, then 2) both still saw the container crash on a real
#'   local dev container — the second time at 88% of a 7.4GB container
#'   memory limit, right up against the ceiling despite the earlier
#'   cgroup-aware detection. Root cause of that miss: this function was
#'   originally called once, when the R process starts, and cached in a
#'   top-level constant — by the time a user actually clicked Full Analysis
#'   after using the app for a while, real memory usage had grown well past
#'   that stale startup snapshot. Callers must now call this function fresh,
#'   immediately before each run (see the three call sites below), not cache
#'   its result. Reads two real, machine-specific signals that actually
#'   bound safe concurrency:
#'   1. CPU quota — Linux cgroups (v2 `cpu.max`, or v1
#'      `cpu.cfs_quota_us`/`cpu.cfs_period_us`) expose the real per-container
#'      CPU allotment when the container was started with an explicit CPU
#'      limit, which `detectCores()` does not see (it reads `/proc/cpuinfo`,
#'      reflecting the whole VM/host).
#'   2. Memory headroom, measured right now — the real constraint that
#'      actually caused both crashes. Prefers the cgroup's own memory
#'      ceiling minus *current* usage (v2 `memory.max`/`memory.current`, or
#'      v1 `memory.limit_in_bytes`/`memory.usage_in_bytes`) — the
#'      container's own budget, not the host's, since a memory-limited
#'      container can be OOM-killed well before the host itself runs low.
#'      Falls back to the host's `/proc/meminfo` `MemAvailable` only when no
#'      cgroup memory limit is set (or unreadable) — the next-best real
#'      signal, still far better than a fixed guess. A 50% safety factor
#'      (tightened from an initial 70%, which still wasn't conservative
#'      enough) is applied to the memory-derived bound, to leave headroom
#'      for the main Shiny process's own further growth during the run,
#'      RStudio Server, and any other processes sharing the same container.
#'   Falls back to `parallel::detectCores()` alone (CPU-only, as before) on
#'   any platform where none of these files are readable (e.g. non-Linux),
#'   and always returns at least 1.
#'
#'   This estimate is inherently best-effort — it cannot know a
#'   configuration's casualty volume or how much a *different* concurrently
#'   running process might grow by mid-run. run_replications()'s own
#'   killed-worker detection (R/replication.R) remains the last line of
#'   defence when a single forked *child* is OOM-killed while the main
#'   process survives; it cannot help if memory pressure is severe enough
#'   that the kernel OOM-kills the main process (or the whole container)
#'   instead, which is why keeping this estimate conservative matters more
#'   than keeping it tight.
detect_safe_cores <- function(n_days = 30, mem_per_worker_base_mb = 900) {
  mem_per_worker_mb <- max(400, mem_per_worker_base_mb * (n_days / 30))
  cpu_cores <- getOption("mc.cores", parallel::detectCores(logical = FALSE))

  read_num1 <- function(path) {
    if (!file.exists(path)) return(NA_real_)
    tryCatch(suppressWarnings(as.numeric(trimws(readLines(path, warn = FALSE, n = 1)))),
             error = function(e) NA_real_)
  }

  # --- CPU: respect a cgroup CPU quota if the container was started with one
  if (file.exists("/sys/fs/cgroup/cpu.max")) {                    # cgroup v2
    parts <- strsplit(trimws(readLines("/sys/fs/cgroup/cpu.max", warn = FALSE, n = 1)), "\\s+")[[1]]
    if (length(parts) == 2 && parts[1] != "max") {
      quota <- suppressWarnings(as.numeric(parts[1])); period <- suppressWarnings(as.numeric(parts[2]))
      if (!is.na(quota) && !is.na(period) && period > 0) {
        cpu_cores <- min(cpu_cores, max(1, floor(quota / period)))
      }
    }
  } else {                                                         # cgroup v1
    quota  <- read_num1("/sys/fs/cgroup/cpu/cpu.cfs_quota_us")
    period <- read_num1("/sys/fs/cgroup/cpu/cpu.cfs_period_us")
    if (!is.na(quota) && quota > 0 && !is.na(period) && period > 0) {
      cpu_cores <- min(cpu_cores, max(1, floor(quota / period)))
    }
  }

  # --- Memory: bound concurrency by this container's own real headroom ----
  mem_cores   <- Inf
  mem_limit   <- read_num1("/sys/fs/cgroup/memory.max")                     # cgroup v2
  mem_current <- read_num1("/sys/fs/cgroup/memory.current")
  if (is.na(mem_limit)) {
    mem_limit   <- read_num1("/sys/fs/cgroup/memory/memory.limit_in_bytes") # cgroup v1
    mem_current <- read_num1("/sys/fs/cgroup/memory/memory.usage_in_bytes")
  }
  if (!is.na(mem_limit) && !is.na(mem_current)) {
    headroom_mb <- (mem_limit - mem_current) / (1024^2)
    if (headroom_mb > 0) mem_cores <- max(1, floor(0.5 * headroom_mb / mem_per_worker_mb))
  } else if (file.exists("/proc/meminfo")) {
    avail_line <- grep("^MemAvailable:", readLines("/proc/meminfo", warn = FALSE), value = TRUE)
    if (length(avail_line) == 1) {
      avail_kb <- suppressWarnings(as.numeric(regmatches(avail_line, regexpr("[0-9]+", avail_line))))
      if (!is.na(avail_kb)) mem_cores <- max(1, floor(0.5 * (avail_kb / 1024) / mem_per_worker_mb))
    }
  }

  as.integer(max(1, min(cpu_cores, mem_cores)))
}

#' Run scripts/shiny_worker.R as a subprocess and return its saved result
#'
#' @param args Character vector of command-line arguments for
#'   scripts/shiny_worker.R (e.g. c("--mode", "full", "--json", json_path,
#'   ...)); see that script's option_list for the full set.
#' @param output_rds Path the worker was told (via --output-rds in `args`)
#'   to saveRDS() its result to; read back and returned on success.
#' @return The R object scripts/shiny_worker.R saved to `output_rds`.
#'
#' @details Full Analysis / Morris / Sobol dispatch through this rather than
#' calling run_replications()/run_morris()/run_sobol() directly inside a
#' future() body — see the comment above plan(multisession) for why. This
#' function itself is called from inside a future() (so it runs on a
#' multisession worker, safely — it does no forking of its own, only
#' launching a subprocess via system2(), which is fork-then-exec, not
#' fork()-without-exec), and blocks that worker until the subprocess exits,
#' which is fine since the worker is already off the main Shiny process.
run_shiny_worker <- function(args, output_rds) {
  worker_script <- file.path(APP_DIR, "scripts", "shiny_worker.R")
  log_file <- tempfile("bch_worker_log_")
  status <- system2("Rscript", args = c(worker_script, args),
                    stdout = log_file, stderr = log_file, wait = TRUE)
  log_text <- if (file.exists(log_file)) paste(readLines(log_file), collapse = "\n") else "(no log)"
  if (!identical(status, 0L) || !file.exists(output_rds)) {
    stop(sprintf("Worker subprocess failed (exit status %s):\n%s", status, log_text))
  }
  readRDS(output_rds)
}

# Cap on mclapply's mc.cores (R/replication.R's run_replications()) for every
# multi-replication path this app triggers (Full Analysis, Morris, Sobol).
# CLI/scripted callers (scripts/run_sensitivity.R, run.R, R/warmup.R,
# R/scenario_runner.R) do not pass max_cores and are unaffected — this cap
# only applies to the interactive, casually-clicked Shiny paths below. NOT a
# cached top-level constant (deliberately) — see detect_safe_cores()'s own
# roxygen for why a stale app-startup snapshot previously understated real
# usage. Each of the three call sites below calls detect_safe_cores(n_days)
# fresh, immediately before submitting that run.
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
detect_tri_triples <- function(registry) {
  ids <- vapply(registry, function(f) f$id, character(1))
  mode_ids <- grep("_mode$", ids, value = TRUE)
  prefixes <- sub("_mode$", "", mode_ids)
  keep <- vapply(prefixes, function(p) paste0(p, "_min") %in% ids && paste0(p, "_max") %in% ids, logical(1))
  prefixes <- prefixes[keep]
  setNames(lapply(prefixes, function(p) {
    mode_f <- registry[[which(ids == paste0(p, "_mode"))]]
    list(min_id = paste0(p, "_min"), mode_id = paste0(p, "_mode"), max_id = paste0(p, "_max"),
         label = sub(" — Most Likely \\(Mode\\)$", "", mode_f$label))
  }), prefixes)
}
TRI_TRIPLES <- detect_tri_triples(PARAM_REGISTRY)

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

#' Render a small density-curve preview for a triangular-distribution
#' field group (min/mode/max), matching the visual language of
#' render_gen_curve() for the casualty-generation streams.
#'
#' @details Uses `triangle::dtriangle()` — the same package the
#'   simulation itself draws durations from via `rtriangle()`
#'   (R/trajectories.R), so the curve shown is the exact shape those
#'   three numbers imply, not an approximation.
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
#' @details Uses dow_prob() (R/trajectories.R) directly, so the curve shown
#'   is the exact shifted-logistic function the simulation evaluates at
#'   each DOW check — not an approximation. p_max is the live Configure
#'   slider value for this priority; p_base, k, and t_mid are read from the
#'   resolved scenario JSON and are not user-editable in this app (see the
#'   read-only fields alongside the curve), so the curve's ceiling moves as
#'   the slider is dragged while its floor and shape track whichever
#'   Casualty Intensity Profile is active.
#' @param p_base,p_max,k,t_mid Parameters as for dow_prob() (R/trajectories.R).
#' @param window_max Upper bound of the plotted time window in minutes; when
#'   NULL (default), sized to comfortably show the curve reach its plateau.
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
# One compositional-split slider per Scheduled Event Days row (Issue #9) —
# generated rather than hand-written since MASS_CASUALTY_SCHEDULE_SLOTS
# entries are all identical in colour/labels, just keyed by event index.
SPLIT_SLIDER_META <- c(SPLIT_SLIDER_META, setNames(
  lapply(seq_len(MASS_CASUALTY_SCHEDULE_SLOTS), function(i) {
    list(colors = c("#c0392b", "#e08e2d", "#2a78d6"), labels = c("P1", "P2", "P3"))
  }),
  sprintf("mc_event_pri_split_%d", seq_len(MASS_CASUALTY_SCHEDULE_SLOTS))
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
  values <- inject_split(values, "mc_pri_split", c("mc_pri_one", "mc_pri_two", "mc_pri_three"))
  for (i in seq_len(MASS_CASUALTY_SCHEDULE_SLOTS)) {
    values <- inject_split(values, sprintf("mc_event_pri_split_%d", i),
                           sprintf(c("mc_sched_pri_one_%d", "mc_sched_pri_two_%d", "mc_sched_pri_three_%d"), i))
  }
  values
}

#' Fill in any registry field missing from `values` with its scenario-
#' resolved default (Issue #77). A field is missing whenever its Configure
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
  long_icu_mode          = "Long ICU Stay (Mode)",
  pri1_surg_prob         = "Priority 1 Surgical Candidacy",
  in_theatre_rate        = "In-Theatre Recovery Rate",
  ot_hours               = "OT Shift Length (Hours per Shift)",
  mass_casualty_rate     = "Mass Casualty Event Rate (per day)",
  mass_casualty_max_cas  = "Mass Casualty Event Size (Maximum)"
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

#' Render a small, visually inert numeric display — a real `<input
#' disabled>` element styled to match the surrounding form controls, not an
#' editable Shiny widget (no `input$id` is ever registered for it). Used
#' for constants shown for context (e.g. the DOW logistic shape parameters)
#' that are not exposed as editable Configure fields.
#'
#' @param label,tooltip Short label and hover tooltip (provenance/citation).
#' @param value Numeric value to display; formatted with `digits` decimals.
#' @param path "elm.acty" path for the ⚠ scenario-override flag, matching
#'   field_label()'s convention — pass the same path an editable sibling
#'   field in the same acty block would use, so this constant is flagged
#'   consistently when a scenario overrides the block it lives in.
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
  if (!is.null(f$choices)) {
    selectInput(f$id, lbl, choices = f$choices, selected = value)
  } else if (isTRUE(f$morris) || isTRUE(f$slider)) {
    # Defensive widening: the current env_data.json baseline should always
    # be representable on the slider, even where a screened Morris range
    # (R/sensitivity.R) has drifted from the baseline after a later
    # recalibration (Issue #75) — flagged as a data-consistency check
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
field_card <- function(f, value, overridden_paths = NULL) {
  card(field_input(f, value, overridden_paths))
}

#' Render a set of fields as a responsive grid, automatically detecting
#' triangular (min/mode/max) field triples via TRI_TRIPLES and rendering
#' each as a single curve card (plot + 3 inputs, matching the Casualty
#' Generation Rates pattern) instead of three separate plain field cards.
#' Any field not part of a detected triple still renders as a plain
#' field_card(), so this is a superset of the previous plain-grid
#' behaviour — a subgroup with no triangular fields renders identically
#' to before.
#' Render a triangular field triple's min/mode/max as three small plain
#' numeric inputs side by side, rather than three full-width rows — the
#' mode field's own tooltip (still stating its Morris-screened range and
#' source, where applicable) is preserved via field_label(), just with a
#' short "Min"/"Mode"/"Max" visible label instead of the field's full
#' descriptive name, since that's already shown once in the card header.
#' Always plain numericInput()s (never a slider, even for a
#' Morris-screened mode field) so all three fit uniformly in one narrow
#' row under the curve.
tri_input_row <- function(min_f, mode_f, max_f, defaults, overridden_paths = NULL) {
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

  node_x <- function(n) {
    if (n == 0) return(numeric(0))
    total_w <- (n - 1) * col_gap
    start_x <- (width - total_w) / 2
    start_x + (seq_len(n) - 1) * col_gap
  }
  x1 <- node_x(r1_teams); x2 <- node_x(r2b_teams); x3 <- node_x(r2e_teams)

  mesh <- function(y_from, x_from, y_to, x_to, color) {
    if (length(x_from) == 0 || length(x_to) == 0) return(NULL)
    pts <- expand.grid(xf = x_from, xt = x_to)
    lapply(seq_len(nrow(pts)), function(i) {
      tags$line(x1 = pts$xf[i], y1 = y_from, x2 = pts$xt[i], y2 = y_to,
                stroke = color, `stroke-width` = 1, `stroke-opacity` = 0.3)
    })
  }
  nodes <- function(y, xs, fill) {
    lapply(seq_along(xs), function(i) {
      tagList(
        tags$circle(cx = xs[i], cy = y, r = 10, fill = fill, stroke = "#333", `stroke-width` = 1),
        tags$text(x = xs[i], y = y + 4, `text-anchor` = "middle", `font-size` = 10,
                   `font-weight` = "600", fill = "#fff", i)
      )
    })
  }
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
force_bed_table <- function(r2b_teams, r2b_beds, r2e_teams, r2e_beds) {
  bed_types <- names(r2b_beds)
  cell <- function(per_team, teams) {
    tags$td(
      tags$div(style = "font-weight:600;", sprintf("%g", per_team * teams)),
      tags$div(style = "font-size:9px; color:#888;", sprintf("(%g × %g)", per_team, teams))
    )
  }
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
#' now carry a working dead-heading return leg (Issue #73 follow-up made
#' the R2B ↔ R2E legs consistent with the R1 ↔ R2B ones, which have had
#' this since Issue #6):
#'   - R1 → R2B, WIA (PMVAmb): r1_transport_wia().
#'   - R1 → R2B, KIA (HX240M): r1_transport_kia() ("KIA transport from
#'     Role 1 to mortuary at Role 2").
#'   - R2B → R2E, WIA: the inline evacuation step inside r2b_treat_wia()
#'     (all of its "R2B Hold"/"Hold Full"/"Hold Queue"/wait-for-evac
#'     sub-paths — i.e. every R2B→R2E movement, surgical or bypassed,
#'     funnels through r2b_evac_leg()/r2b_evac_return_leg()), which seizes
#'     each R2B team's own `evac` resource rather than the shared PMVAmb
#'     fleet — a deliberate design (Issue #73): this leg represents an
#'     organic R2B unit asset, distinct from the brigade-pooled fleet used
#'     for R1 → R2B. The dead-heading return leg is modelled on that same
#'     organic resource rather than a pooled vehicle.
#'   - R2B → R2E, KIA/mortuary: r2b_transport_kia(), using the shared
#'     HX2 40M fleet. The mortuary is modelled as collocated with R2E, not
#'     R2B (Issue #73 follow-up) — R2B has no organic mortuary asset, so a
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
#'   R2B team's own organic evac resource rather than a pooled vehicle
#'   (Issue #73).
#' @param kia2_mode R2B→R2E KIA/mortuary road-move transport mode
#'   (minutes), using the shared HX2 40M fleet (Issue #73 follow-up).
#' @param mort2e_mode R2E collocated-mortuary local transport mode
#'   (minutes) — no return leg, since no vehicle asset is used for this
#'   final in-place movement.
evac_chain_diagram <- function(wia1_mode, kia1_mode, wia2_mode, kia2_mode,
                                mort2e_mode) {
  fmt  <- function(x) if (is.null(x) || is.na(x)) "?" else format(round(x), big.mark = ",")

  width <- 300
  y     <- c(20, 130, 240)
  cx    <- width / 2
  height <- y[3] + 26

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

  # side: -1 puts the line left-of-centre with its label growing further
  # left (text-anchor "end"); +1 puts it right-of-centre with its label
  # growing further right (text-anchor "start") — so the WIA and KIA
  # labels for the same leg diverge away from each other instead of
  # colliding at the shared midline.
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

  mortuary_marker <- function(y, mode) {
    tagList(
      tags$text(x = cx + 50, y = y + 4, `font-size` = 13, "⚱"),
      tags$text(x = cx + 64, y = y + 4, `font-size` = 9, fill = "#888",
                 sprintf("Mortuary: %s min", fmt(mode)))
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

    mortuary_marker(y[3], mort2e_mode),

    node(y[1], "R1", "Forward Aid Post"),
    node(y[2], "R2B", "Battalion Aid Post"),
    node(y[3], "R2E", "Field Hospital")
  )
}

#' Wrap evac_chain_diagram() with the heading/caption pattern matching
#' force_structure_diagram() (Health System Architecture), for the same
#' sticky-sidebar treatment on the Medevac panel.
medevac_diagram <- function(wia1_mode, kia1_mode, wia2_mode, kia2_mode,
                             mort2e_mode) {
  tagList(
    h6(class = "text-muted mt-2", "Medevac Chain"),
    p(class = "text-muted", style = "font-size:11px;",
      "Each line is one transport leg modelled in the trajectory code (R/trajectories.R), labelled with its current mode duration — all four legs also carry an unmodified dead-heading return leg (a fresh draw from the same outbound distribution, no configurable multiplier; Issue #74). ",
      tags$span(style = "color:#2a78d6; font-weight:600;", "Blue = WIA (PMVAmb, dead-heading return leg)"), "; ",
      tags$span(style = "color:#6c757d; font-weight:600;", "grey = KIA (HX240M, dead-heading return leg)"), "; ",
      tags$span(style = "color:#1e824c; font-weight:600;", "green = R2B → R2E WIA evacuation"),
      " (each R2B team's own organic Evac Team resource, not the shared PMVAmb fleet — a deliberate design distinct from the R1 → R2B legs — with a dead-heading return leg on that same organic resource); ",
      tags$span(style = "color:#922b21; font-weight:600;", "maroon = R2B → R2E KIA/mortuary road move"),
      " (shared HX2 40M fleet — the mortuary is modelled as collocated with R2E, not R2B, since R2B has no organic mortuary asset of its own).",
      " R2B is bypassed to R2E two ways, both reusing the R1→R2B leg time or the green evacuation step already shown rather than drawing a new leg: upstream, before transport starts, if every R2B team's OT beds are fully occupied (",
      tags$code("select_available_r2b_team()"), "); or after arrival at R2B, at the surgical decision point, if the selected team is off-shift or its OT is busy/queued — surgery is skipped but the casualty still evacuates to R2E via the same green step.",
      " KIA reaching R2E (whether dying there, or arriving by road from R1 or R2B) travel no further — the ⚱ marker is a local transfer to the collocated R2E mortuary, not a cross-echelon vehicle leg.",
      " This is structural, not a simulated outcome — run Quick Run for actual queueing and casualty results."),
    evac_chain_diagram(wia1_mode, kia1_mode, wia2_mode, kia2_mode, mort2e_mode)
  )
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
          "These are curve parameters, not single values — the shaded area is the actual shape of daily casualty-rate variability implied by the mean and standard deviation below it (dashed line = mean). The curve updates live as you edit the numbers, and its shape (lognormal vs exponential) follows the Casualty Intensity Profile selected above. A stream currently governed by an exponential distribution accepts only the mean — exponential is a single-parameter distribution, so its Std. Dev. field is not applicable and is suppressed rather than shown as an editable (but functionally inert) input."),
        layout_column_wrap(
          width = "320px",
          !!!lapply(GEN_STREAM_ACTYS, function(acty) {
            mean_f <- Find(function(f) identical(f$id, paste0("gen_", acty, "_mean")), sg_fields)
            sd_f   <- Find(function(f) identical(f$id, paste0("gen_", acty, "_sd")),   sg_fields)
            stream_label <- sub(" — Mean Daily Rate$", "", mean_f$label)
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

    if (identical(sg, "Died of Wounds Ceilings")) {
      return(tagList(
        h6(class = "text-muted mt-2", sg),
        p(class = "text-muted small",
          "The ceiling below is the only editable value; the curve shows the full time-dependent survival function F(t) the simulation evaluates at each DOW check, using the fixed shape parameters shown alongside it — p_base (floor at t=0) and k/t_mid (rise steepness/inflection) are not user-editable in this app. See README Died of Wounds — Survival Function."),
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
                    readonly_numeric("p_base", SRC_DOW_CEILING, shp$p_base, digits = 4,
                                     path = "dow.params", overridden_paths = overridden_paths)),
                div(style = "flex:1; min-width:0;",
                    readonly_numeric("k", SRC_DOW_SHAPE, shp$k, digits = 3,
                                     path = "dow.params", overridden_paths = overridden_paths)),
                div(style = "flex:1; min-width:0;",
                    readonly_numeric("t_mid", SRC_DOW_SHAPE, shp$t_mid, digits = 0,
                                     path = "dow.params", overridden_paths = overridden_paths))
              )
            )
          })
        )
      ))
    }

    # Event Timing Mode (the mode dropdown itself) always renders via the
    # default fallback below. Random Event Rate, Scheduled Event Days, and
    # Mass Casualty Priority Split are each wrapped in a conditionalPanel()
    # keyed off input$mc_mode — the shared casualty-count/priority-split
    # settings (Random Event Rate, Mass Casualty Priority Split) apply only
    # to Poisson-mode events; Scheduled mode instead gives each event its
    # own casualty-count and priority fields, rendered inline in its card.
    # Injection Window (window_min/mode/max, rendered via the default
    # fallback below) is the one subgroup that is never mode-gated — it is
    # not customisable per event in either mode.
    if (identical(sg, "Random Event Rate")) {
      return(conditionalPanel(
        condition = "input.mc_mode == 'poisson'",
        tagList(
          h6(class = "text-muted mt-2", sg),
          p(class = "text-muted small",
            "Event start times are drawn from a Poisson process at this rate — event count and timing vary stochastically between replications. Casualty count is shared across every event this mode generates; for events with independently varying casualty counts, use Scheduled (Deliberate Days) mode instead."),
          render_field_grid(sg_fields, defaults, overridden_paths)
        )
      ))
    }

    if (identical(sg, "Scheduled Event Days")) {
      # Initial visible-row count only — a one-time value read from this
      # render's defaults, not a reactive dependency. All 20 slots are
      # always present in the DOM; the +/- buttons (server observeEvent
      # handlers, see mc_event_count in the server) toggle a single row's
      # display via a custom message + the client-side handler registered
      # below, rather than triggering a re-render of this whole group —
      # re-rendering the group here would also rebuild the Event Timing
      # Mode dropdown and every other Mass Casualty field at its JSON
      # default, discarding whatever the user currently has live in the UI.
      mc_count <- max(1L, sum(vapply(seq_len(MASS_CASUALTY_SCHEDULE_SLOTS), function(i) {
        d <- defaults[[sprintf("mc_sched_day_%d", i)]]
        !is.null(d) && !is.na(d) && d > 0
      }, logical(1))))
      return(conditionalPanel(
        condition = "input.mc_mode == 'scheduled'",
        tagList(
          h6(class = "text-muted mt-2", sg),
          p(class = "text-muted small",
            sprintf(
              "Each event is independently drawn (Bernoulli) at its own probability every replication — a day at probability 1 fires identically in every replication, a lower value introduces controlled randomness — and has its own casualty count and triage priority mix. Use + / − to add or remove event rows (up to %d); removing a row resets it rather than deleting it, so it stops firing. For more than %d explicit events, edit env_data.json directly.",
              MASS_CASUALTY_SCHEDULE_SLOTS, MASS_CASUALTY_SCHEDULE_SLOTS
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
          # sized for all MASS_CASUALTY_SCHEDULE_SLOTS cards, so hidden
          # (display:none) slots still reserved their row's height — every
          # +/- click left a wall of dead space below the visible cards.
          # Flexbox correctly drops display:none items from layout, so the
          # container's height always matches only what's actually shown.
          div(
            style = "display:flex; flex-wrap:wrap; gap:12px; align-items:flex-start;",
            !!!lapply(seq_len(MASS_CASUALTY_SCHEDULE_SLOTS), function(i) {
              day_f      <- Find(function(f) identical(f$id, sprintf("mc_sched_day_%d", i)),       sg_fields)
              prob_f     <- Find(function(f) identical(f$id, sprintf("mc_sched_prob_%d", i)),      sg_fields)
              min_cas_f  <- Find(function(f) identical(f$id, sprintf("mc_sched_min_cas_%d", i)),   sg_fields)
              max_cas_f  <- Find(function(f) identical(f$id, sprintf("mc_sched_max_cas_%d", i)),   sg_fields)
              pri_one_f  <- Find(function(f) identical(f$id, sprintf("mc_sched_pri_one_%d", i)),   sg_fields)
              p1 <- defaults[[pri_one_f$id]]; p2 <- defaults[[sprintf("mc_sched_pri_two_%d", i)]]
              pri_lbl <- field_label(list(
                label   = "Priority Split (P1 | P2 | P3)",
                tooltip = paste0(
                  "This event's own triage priority mix — independent of every other event. Source: ", SRC_MASS_CASUALTY_PRI
                ),
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

    if (identical(sg, "Mass Casualty Priority Split")) {
      one_f <- Find(function(f) identical(f$id, "mc_pri_one"), sg_fields)
      p1 <- defaults[["mc_pri_one"]]; p2 <- defaults[["mc_pri_two"]]
      mc_pri_lbl <- field_label(list(
        label   = "Priority Split (P1 | P2 | P3)",
        tooltip = paste0(
          "Drag either handle to reallocate share between adjacent priorities for mass-casualty-derived casualties specifically — ",
          "Priority 1, 2, and 3 always sum to 100% by construction. Independent of the background Triage Priority Split (Casualty Rates panel). Source: ",
          SRC_MASS_CASUALTY_PRI
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
      !!!lapply(c(GRP_FORCE, GRP_HEALTH_ARCH, GRP_LOGISTICS, GRP_PROVISION, GRP_CASUALTY, GRP_MASS_CASUALTY), function(g) {
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
        tags$hr(),
        slider_with_text_input("n_reps",
                    field_label(list(label = "Replication Count (Full Analysis)",
                                     tooltip = paste0(
                                       "Number of independent replications for Full Analysis mode. ",
                                       "Higher counts narrow the 95% CI but take proportionally longer to run. ",
                                       "Minimum replication-count guidance for planning-grade output follows ",
                                       "Romero-Brufau et al. (2020) — see README Shiny Application, Full Analysis Mode."))),
                    min = 10, max = 1000, value = 100, step = 10),
        actionButton("run_full", "\U0001F4CA Run Full Analysis (Multi-Run, 95% CI)", class = "btn-success btn-lg"),
        tags$hr(),
        uiOutput("run_status"),
        tags$hr(),
        uiOutput("download_all_ui")
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
  run_mode         <- reactiveVal("quick")  # quick | full — which button started the current/last run
  run_error        <- reactiveVal(NULL)
  progress_pct     <- reactiveVal(0)
  mon_data         <- reactiveVal(NULL)
  analysis_results <- reactiveVal(NULL)
  pending_future    <- reactiveVal(NULL)

  # Full Analysis replication-level progress: rep_progress_dir holds the
  # marker-file directory run_replications() writes rep_<i>.done into (see
  # R/replication.R); rep_progress_total/done are polled from it below so
  # the UI shows real "k of N replications complete" progress rather than
  # the simulated ticker Quick Run uses (a single replication can't offer
  # finer-grained real progress than start/done).
  rep_progress_dir   <- reactiveVal(NULL)
  rep_progress_total <- reactiveVal(0)
  rep_progress_done  <- reactiveVal(0)

  # Sensitivity Screening (Morris) state
  morris_state        <- reactiveVal("idle")  # idle | running | done | error
  morris_error         <- reactiveVal(NULL)
  morris_results       <- reactiveVal(NULL)
  morris_png_bytes     <- reactiveVal(NULL)
  morris_progress_dir  <- reactiveVal(NULL)
  morris_progress_total <- reactiveVal(0)
  morris_progress_done  <- reactiveVal(0)
  pending_morris        <- reactiveVal(NULL)

  # Sobol Decomposition state
  sobol_state         <- reactiveVal("idle")  # idle | running | done | error
  sobol_error          <- reactiveVal(NULL)
  sobol_results        <- reactiveVal(NULL)
  sobol_progress_dir   <- reactiveVal(NULL)
  sobol_progress_total <- reactiveVal(0)
  sobol_progress_done  <- reactiveVal(0)
  pending_sobol         <- reactiveVal(NULL)

  # Transport Fleet Capacity Margin Sweep state (Issue #57)
  transport_sweep_state         <- reactiveVal("idle")  # idle | running | done | error
  transport_sweep_error         <- reactiveVal(NULL)
  transport_sweep_results       <- reactiveVal(NULL)
  transport_sweep_progress_dir  <- reactiveVal(NULL)
  transport_sweep_progress_total <- reactiveVal(0)
  transport_sweep_progress_done  <- reactiveVal(0)
  pending_transport_sweep        <- reactiveVal(NULL)

  # ── Configure panel ───────────────────────────────────────────────────────

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
  wire_slider_text_sync(input, session, "ot_hours")
  wire_slider_text_sync(input, session, "n_reps")
  wire_range_slider_text_sync(input, session, "pri_split")
  wire_range_slider_text_sync(input, session, "dnbi_split")
  wire_range_slider_text_sync(input, session, "mc_pri_split")
  lapply(seq_len(MASS_CASUALTY_SCHEDULE_SLOTS), function(i) {
    wire_range_slider_text_sync(input, session, sprintf("mc_event_pri_split_%d", i))
  })

  # Live distribution-curve preview for every triangular (min/mode/max)
  # field group in the registry (see TRI_TRIPLES/render_field_grid()),
  # reading straight from the three inputs so it redraws as any of them
  # is edited — the same pattern as the casualty-generation curves above.
  # Left at Shiny's default suspendWhenHidden = TRUE (Issue #77): a curve
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
  shorten_scenario_label <- function(lbl) {
    trimws(sub("\\s*\\(.*$", "", lbl))
  }

  # The dropdown's own naming for what each option actually changes,
  # distinct from — and overriding, for display only — env_data.json's
  # canonical scenario labels above. Neither "Default" nor "Moderate
  # Intensity" being labelled by battle intensity alone made it obvious
  # that *both* are Falklands-sourced and differ only in whether treatment
  # efficacy has been re-derived for the era (see README Scenario Profiles
  # — "The base env_data.json configuration conflates two historical
  # contexts"): "Falklands — Modified" (was "Default") keeps Falklands
  # casualty generation and DOW ceiling but pairs them with modern
  # (OIF/OEF-era) treatment efficacy; "Falklands — Unmodified" (was
  # "Moderate Intensity") re-derives both the DOW ceiling and treatment
  # efficacy to be internally consistent for 1982. "Okinawa — Casualty
  # Rates" (was "High Intensity") is named for exactly what it overrides
  # rather than implied as a complete second scenario — it still inherits
  # the Falklands DOW ceiling and modern treatment efficacy from the base
  # configuration, since it is a "demonstration skeleton" per Issue #54's
  # acceptance criteria (Issue #10 owns extending it to a full scenario).
  # A scenario id absent from this map (e.g. a future addition) falls back
  # to its own env_data.json label, so this isn't a hardcoded assumption
  # that only these three will ever exist.
  SCENARIO_DROPDOWN_LABELS <- c(
    default             = "Falklands — Modified",
    moderate_intensity  = "Falklands — Unmodified",
    high_intensity      = "Okinawa — Casualty Rates"
  )

  scenario_choices <- reactive({
    base <- raw_env_data()
    ids  <- c("default", names(base$scenarios))
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
          "Selects a named scenario profile that overlays casualty-generation,",
          "DOW, and treatment-efficacy parameters onto the base configuration.",
          "Structural fields (force size, team/bed counts, transport fleet)",
          "are never affected by this selector.",
          "\"Falklands — Modified\" (the base configuration) keeps",
          "Falklands-sourced casualty generation and a Falklands-calibrated",
          "DOW ceiling but pairs them with modern (OIF/OEF-era) treatment",
          "efficacy factors. \"Falklands — Unmodified\" re-derives both the",
          "DOW ceiling and the treatment efficacy factors to be internally",
          "consistent for 1982, so the two produce different DOW outcomes",
          "despite both being Falklands-sourced.",
          "\"Okinawa — Casualty Rates\" overrides only the WIA/KIA",
          "casualty-generation distributions; it still inherits the",
          "Falklands DOW ceiling and modern treatment efficacy from the",
          "base configuration rather than being a complete second scenario",
          "(a documented demonstration skeleton — see README Scenario",
          "Profiles).",
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

  # Resolved distribution family per casualty-generation stream under the
  # active Casualty Intensity Profile — same lookup render_gen_curve()'s
  # own renderPlot() uses below, shared here so the Std. Dev. field itself
  # (not just its curve) reflects which streams are currently exponential.
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
  # Mass Casualty group body. Visibility itself is toggled client-side via
  # the "mc_toggle_row" custom message (handler registered once in
  # render_group_body()'s "Scheduled Event Days" case).
  mc_event_count <- reactiveVal(max(1L, count_active_mass_casualty_events(isolate(raw_env_data()))))

  observeEvent(input$mc_event_add, {
    n <- mc_event_count()
    if (n < MASS_CASUALTY_SCHEDULE_SLOTS) {
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
      # Event Days field is always rendered/bound (see MASS_CASUALTY_
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

  lapply(names(fields_by_group), function(g) {
    output_id <- paste0("group_ui_", make.names(g))
    output[[output_id]] <- renderUI({
      defaults <- registry_defaults(fields_by_group[[g]], scenario_json())
      render_group_body(fields_by_group[[g]], defaults, scenario_overridden_paths(),
                        gen_distributions(), dow_shape())
    })
    # Left at Shiny's default suspendWhenHidden = TRUE (Issue #77, reversing
    # the eager-render override Issue #14 introduced): a panel other than
    # the initially-open one now renders only once actually opened, rather
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
  # Left at the default suspendWhenHidden = TRUE (Issue #77): this lives in
  # the same accordion panel as the Health System Architecture fields it
  # illustrates, so there is nothing to show while that panel is collapsed.

  # Live medevac chain diagram at the top of the Medevac panel
  # (see medevac_diagram()) — reads the same transport mode inputs the
  # fields below it edit. All four legs carry an unmodified dead-heading
  # return leg (Issue #74); R2E's own local mortuary transfer (mort2e_mode)
  # remains a marker, not a leg, since no vehicle asset is used for that
  # final in-place movement.
  output$medevac_diagram <- renderUI({
    medevac_diagram(
      wia1_mode = input$r1_wia_transport_mode  %||% 0,
      kia1_mode = input$r1_kia_transport_mode  %||% 0,
      wia2_mode = input$r2b_wia_transport_mode %||% 0,
      kia2_mode = input$r2b_kia_transport_mode %||% 0,
      mort2e_mode = input$r2e_kia_transport_mode %||% 0
    )
  })
  # Left at the default suspendWhenHidden = TRUE (Issue #77) — same
  # reasoning as force_design_diagram above, for the Medevac panel.

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
    # Left at the default suspendWhenHidden = TRUE (Issue #77) — same
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
    # Left at the default suspendWhenHidden = TRUE (Issue #77) — same
    # reasoning as the triangular curve previews above.
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

    # Mass casualty casualties-per-event min/max pairs (Issue #9) — the
    # shared Poisson-mode pair and every Scheduled Event Days row, whether
    # or not that row is currently visible (a hidden row is always reset to
    # defaults, but validating it regardless costs nothing and stays
    # correct if that assumption ever changes).
    mc_min_max_ids <- c(list(c("mc_min_cas", "mc_max_cas")),
                        lapply(seq_len(MASS_CASUALTY_SCHEDULE_SLOTS), function(i) {
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

  # ── Quick Run execution (async via future + promises) ────────────────────

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
    ot_hours_val <- input$ot_hours
    built_env    <- build_environment(current_json())
    app_dir      <- APP_DIR

    run_mode("quick")
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

  # ── Full Analysis execution (async via future + promises) ────────────────

  observeEvent(input$run_full, {
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

    n_reps_val   <- as.integer(input$n_reps)
    days_val     <- input$n_days
    ot_hours_val <- input$ot_hours
    app_dir      <- APP_DIR
    # Computed fresh right now, not cached — see detect_safe_cores()'s roxygen.
    max_cores_val <- detect_safe_cores(days_val)

    json_path <- tempfile("bch_full_config_", fileext = ".json")
    write_json(current_json(), json_path, pretty = TRUE, auto_unbox = TRUE)

    prog_dir <- tempfile("bch_repprogress_")
    dir.create(prog_dir, recursive = TRUE)
    rep_progress_dir(prog_dir)
    rep_progress_total(n_reps_val)
    rep_progress_done(0)

    run_mode("full")
    run_state("running")
    run_error(NULL)
    progress_pct(0)

    output_rds <- tempfile("bch_full_result_", fileext = ".rds")
    worker_args <- c(
      "--mode", "full", "--json", json_path, "--days", days_val,
      "--ot-hours", ot_hours_val, "--n-reps", n_reps_val,
      "--max-cores", max_cores_val, "--progress-dir", prog_dir,
      "--output-rds", output_rds
    )

    fut <- future({
      setwd(app_dir)
      run_shiny_worker(worker_args, output_rds)
    }, seed = NULL)

    prom <- fut %...>% (function(res) {
      mon_data(res$mon)
      analysis_results(res$results)
      run_state("done")
      progress_pct(100)
      rep_progress_done(rep_progress_total())
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
  # Full Analysis instead has real per-replication progress (see the poller
  # below, which drives rep_progress_done from prog_dir's marker files), so
  # this ticker only fires in "quick" mode.
  observe({
    req(run_state() == "running", identical(run_mode(), "quick"))
    invalidateLater(400, session)
    isolate({
      p <- progress_pct()
      if (p < 90) progress_pct(min(90, p + 4))
    })
  })

  # Real per-replication progress poller for Full Analysis: counts the
  # rep_<i>.done marker files run_replications() writes from its (possibly
  # forked) workers into rep_progress_dir() as each replication completes.
  observe({
    req(run_state() == "running", identical(run_mode(), "full"))
    invalidateLater(500, session)
    pd <- isolate(rep_progress_dir())
    if (!is.null(pd) && dir.exists(pd)) {
      n_done <- length(list.files(pd, pattern = "\\.done$"))
      isolate({
        rep_progress_done(n_done)
        total <- rep_progress_total()
        if (total > 0) progress_pct(min(99, round(100 * n_done / total)))
      })
    }
  })

  output$run_status <- renderUI({
    state <- run_state()
    mode  <- run_mode()
    if (state == "idle") {
      p(class = "text-muted", "No run yet. Configure parameters, then click Run Quick Run or Run Full Analysis.")
    } else if (state == "running" && identical(mode, "quick")) {
      tagList(
        p(sprintf("Running Quick Run (seed %s, %d days)...",
                   if (trimws(input$seed) == "") "random" else trimws(input$seed), input$n_days)),
        div(class = "progress",
            div(class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar", style = sprintf("width: %d%%", progress_pct())))
      )
    } else if (state == "running" && identical(mode, "full")) {
      tagList(
        p(sprintf("Running Full Analysis: %d of %d replications complete (%d days each)...",
                   rep_progress_done(), rep_progress_total(), input$n_days)),
        div(class = "progress",
            div(class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar", style = sprintf("width: %d%%", progress_pct())))
      )
    } else if (state == "done" && identical(mode, "full")) {
      div(class = "alert alert-success",
          sprintf("Full Analysis complete (%d replications) — see the Analyse tab for mean ± 95%% CI results.",
                   rep_progress_total()))
    } else if (state == "done") {
      div(class = "alert alert-success", "Run complete — see the Analyse tab for results.")
    } else {
      div(class = "alert alert-danger", paste("Run failed:", run_error()))
    }
  })

  # ── Analyse panel ─────────────────────────────────────────────────────────

  combined_plot <- function(...) Reduce(`/`, list(...))

  # tab_plot() reads the same four keys regardless of which mode produced
  # analysis_results() — analyse_run() (Quick Run) and analyse_replications()
  # (Full Analysis) both name their per-echelon queue plots identically, so
  # only the "utilisation" tab (no multi-run Gantt equivalent — see
  # analyse_replications()'s roxygen) needs a mode-specific branch.
  tab_plot <- reactive({
    res <- analysis_results()
    req(res)
    utilisation_plot <- if (identical(run_mode(), "full")) {
      res$utilisation
    } else {
      combined_plot(res$r2b_treatment, res$r2b_gantt, res$r2e_surgery, res$r2e_gantt)
    }
    list(
      casualty_flow = res$casualty_flow,
      queue_depths  = combined_plot(res$r1_queues, res$r2b_bed_queues, res$r2e_bed_queues),
      utilisation   = utilisation_plot,
      waiting_times = res$waiting_times
    )
  })

  # KPI summary cards (Full Analysis only) — mean (95% CI) headline figures
  # shown above the output tabs, per Issue #15's acceptance criteria.
  output$kpi_summary_cards <- renderUI({
    req(identical(run_mode(), "full"), run_state() == "done", analysis_results())
    kpi <- analysis_results()$kpi_summary
    card_ui <- function(label, cm, digits = 1) {
      fmt <- function(x) formatC(x, format = "f", digits = digits)
      card(
        card_header(label),
        div(style = "font-size: 1.6rem; font-weight: 700;", fmt(cm[["mean"]])),
        div(class = "text-muted", style = "font-size: 0.85rem;",
            sprintf("95%% CI [%s, %s] (n = %d reps)", fmt(cm[["lower"]]), fmt(cm[["upper"]]), cm[["n"]]))
      )
    }
    layout_column_wrap(
      width = "220px",
      card_ui("Total Casualties", kpi$total_casualties, digits = 1),
      card_ui("DOW Count", kpi$dow_count, digits = 2),
      card_ui("R2E ICU Peak Queue", kpi$r2e_icu_peak_queue, digits = 1),
      card_ui("R2B OT Peak Queue", kpi$r2b_ot_peak_queue, digits = 1)
    )
  })

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

  output$analyse_body <- renderUI({
    if (run_state() != "done" || is.null(analysis_results())) {
      return(div(class = "alert alert-info mt-3",
                  "No results yet. Run a Quick Run or Full Analysis from the Run tab to populate this page."))
    }
    tagList(
    uiOutput("kpi_summary_cards"),
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
        downloadButton("dl_calibration_csv", "Download Table (CSV)"),
        tags$hr(),
        h5("Run Sensitivity Screening"),
        p(class = "text-muted small",
          "Morris Elementary Effects screening (Morris, 1991) perturbs each parameter across its ",
          "plausible range while holding the others fixed, repeated over r trajectories, to rank ",
          "parameters by influence on the R2E ICU queue and other KPIs. Wall-clock time scales with ",
          "r × (parameters + 1) × replications-per-point × simulation duration — the ",
          "production configuration (r = 20, 5 reps, 30 days) takes roughly 2-3 hours; use a small r ",
          "(e.g. 3) for a quick smoke test. See README Shiny Application — Sensitivity Panel."),
        layout_column_wrap(
          width = "220px",
          numericInput("morris_r", "Trajectories (r)", value = 20, min = 3, max = 50, step = 1),
          numericInput("morris_nrep", "Replications per Point", value = 5, min = 3, max = 20, step = 1)
        ),
        actionButton("run_sensitivity", "Run Sensitivity Screening", class = "btn-primary"),
        tags$div(style = "margin-top: 10px;", uiOutput("morris_status")),
        uiOutput("morris_results_ui"),
        tags$hr(),
        h5("Transport Fleet Capacity Margin Sweep"),
        p(class = "text-muted small",
          "Answers \"do we have enough ambulances and trucks?\" by re-running the simulation at each ",
          "fleet size in the ranges below and checking whether casualties start queueing for transport. ",
          "Use this to confirm the current fleet has spare capacity, or to see how far it could shrink ",
          "before evacuation becomes a bottleneck. Each fleet size runs its own set of replications, ",
          "reusing the Replications per Point count set above — a wider range or higher count takes ",
          "longer to complete."),
        layout_column_wrap(
          width = "280px",
          sliderInput("sweep_pmvamb_range", "PMV Ambulance Fleet Size Range",
                     min = 1, max = 10, value = c(1, 5), step = 1),
          sliderInput("sweep_hx240m_range", "HX240M Fleet Size Range",
                     min = 1, max = 10, value = c(1, 4), step = 1)
        ),
        actionButton("run_transport_sweep", "Run Transport Fleet Sweep", class = "btn-primary"),
        tags$div(style = "margin-top: 10px;", uiOutput("transport_sweep_status")),
        uiOutput("transport_sweep_results_ui")
      )
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

  # ── Sensitivity Screening — Morris (async via future + promises) ─────────

  observeEvent(input$run_sensitivity, {
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

    r_val     <- as.integer(input$morris_r)
    nrep_val  <- as.integer(input$morris_nrep)
    days_val  <- input$n_days
    app_dir   <- APP_DIR
    # Computed fresh right now, not cached — see detect_safe_cores()'s roxygen.
    max_cores_val <- detect_safe_cores(days_val)

    json_path <- tempfile("bch_morris_config_", fileext = ".json")
    write_json(current_json(), json_path, pretty = TRUE, auto_unbox = TRUE)

    # run_morris() writes "images/<...>.png" and "outputs/*.csv" relative to
    # the working directory — a scratch work_dir (rather than threading a
    # new images_dir parameter through run_morris()) keeps a Shiny-triggered
    # screen from ever touching the repo's own tracked images/ or outputs/.
    work_dir <- tempfile("bch_morris_work_")
    dir.create(work_dir)

    prog_dir <- tempfile("bch_morrisprogress_")
    dir.create(prog_dir, recursive = TRUE)
    morris_progress_dir(prog_dir)
    morris_progress_total(r_val * (nrow(morris_params) + 1L))
    morris_progress_done(0)

    morris_state("running")
    morris_error(NULL)
    sobol_state("idle")
    sobol_results(NULL)

    output_rds <- tempfile("bch_morris_result_", fileext = ".rds")
    worker_args <- c(
      "--mode", "morris", "--json", json_path, "--days", days_val,
      "--r", r_val, "--n-rep", nrep_val, "--max-cores", max_cores_val,
      "--progress-dir", prog_dir, "--work-dir", work_dir,
      "--output-rds", output_rds
    )

    fut <- future({
      setwd(app_dir)
      run_shiny_worker(worker_args, output_rds)
    }, seed = NULL)

    prom <- fut %...>% (function(out) {
      morris_results(out$res)
      morris_png_bytes(out$png_bytes)
      morris_state("done")
    })
    prom <- catch(prom, function(e) {
      morris_state("error")
      morris_error(conditionMessage(e))
    })
    pending_morris(prom)
    invisible(NULL)
  })

  observe({
    req(morris_state() == "running")
    invalidateLater(500, session)
    pd <- isolate(morris_progress_dir())
    if (!is.null(pd) && dir.exists(pd)) {
      n_done <- length(list.files(pd, pattern = "\\.done$"))
      isolate(morris_progress_done(n_done))
    }
  })

  output$morris_status <- renderUI({
    state <- morris_state()
    if (state == "idle") return(NULL)
    if (state == "running") {
      total <- morris_progress_total(); done <- morris_progress_done()
      pct <- if (total > 0) min(99, round(100 * done / total)) else 5
      tagList(
        p(sprintf("Evaluating Morris design point %d of %d...", done, total)),
        div(class = "progress",
            div(class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar", style = sprintf("width: %d%%", pct)))
      )
    } else if (state == "done") {
      div(class = "alert alert-success", "Morris screening complete.")
    } else {
      div(class = "alert alert-danger", paste("Morris screening failed:", morris_error()))
    }
  })

  #' μ*/σ scatter data for one KPI's Morris object: μ* is the mean absolute
  #' elementary effect (importance); σ is the standard deviation of
  #' elementary effects (nonlinearity/interaction — a large σ relative to μ*
  #' indicates the effect is not consistent across the parameter's range).
  morris_scatter_df <- function(obj) {
    ee <- obj$ee
    data.frame(
      parameter = colnames(ee),
      mu_star   = apply(abs(ee), 2, mean, na.rm = TRUE),
      sigma     = apply(ee, 2, sd, na.rm = TRUE)
    )
  }

  output$morris_mu_sigma_plot <- renderPlot({
    req(morris_results())
    df <- morris_scatter_df(morris_results()$morris_objs$r2e_icu_q)
    df$label <- MORRIS_LABELS[df$parameter]
    ggplot(df, aes(x = mu_star, y = sigma, label = label)) +
      geom_point(size = 3, color = "#2a78d6") +
      geom_text(vjust = -0.9, size = 3.2, check_overlap = TRUE) +
      labs(title = "Morris Screening — μ* vs σ (R2E ICU Queue)",
           subtitle = "μ* = mean absolute elementary effect (importance); σ = std. dev. of elementary effects (nonlinearity/interaction)",
           x = "μ* (Importance)", y = "σ (Nonlinearity / Interaction)") +
      theme_minimal(base_size = 13)
  })

  output$morris_ranking_table <- renderDT({
    req(morris_results())
    rk <- morris_results()$ranking
    df <- data.frame(
      Rank      = seq_len(nrow(rk)),
      Parameter = MORRIS_LABELS[rk$parameter],
      `Mu Star` = round(rk$mu_star, 4),
      Sigma     = round(rk$sigma_ee, 4),
      check.names = FALSE
    )
    datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 20)) %>%
      formatStyle("Rank", target = "row",
                  backgroundColor = styleInterval(5.5, c("#fff3cd", "white")))
  })

  output$dl_morris_ranking_csv <- downloadHandler(
    filename = "morris_ranking.csv",
    content  = function(file) write.csv(morris_results()$ranking, file, row.names = FALSE)
  )
  output$dl_morris_png_zip <- downloadHandler(
    filename = "morris_plots.zip",
    content  = function(file) {
      pngs <- morris_png_bytes()
      req(pngs)
      tmp_dir <- tempfile("bch_morris_dl_")
      dir.create(tmp_dir)
      for (nm in names(pngs)) writeBin(pngs[[nm]], file.path(tmp_dir, nm))
      old_wd <- setwd(tmp_dir)
      on.exit(setwd(old_wd), add = TRUE)
      zip(file, files = names(pngs))
    },
    contentType = "application/zip"
  )

  output$morris_results_ui <- renderUI({
    req(morris_state() == "done", morris_results())
    top5 <- head(morris_results()$ranking$parameter, 5)
    tagList(
      tags$hr(),
      h5("Morris Screening Results — R2E ICU Queue (Primary KPI)"),
      plotOutput("morris_mu_sigma_plot", height = "450px"),
      downloadButton("dl_morris_png_zip", "Download Morris PNGs — All KPIs (ZIP)"),
      tags$hr(),
      h5("Ranked Parameter Influence (μ* on System OT Queue)"),
      p(class = "text-muted small", "Top 5 parameters (highlighted) are pre-selected below for Sobol Decomposition."),
      DTOutput("morris_ranking_table"),
      downloadButton("dl_morris_ranking_csv", "Download Ranked Parameter Table (CSV)"),
      tags$hr(),
      h5("Sobol Variance Decomposition"),
      p(class = "text-muted small",
        "Decomposes each selected parameter's contribution to output variance into first-order (S1 — ",
        "the parameter acting alone) and total-order (ST — including interactions with every other ",
        "parameter) indices (Saltelli et al., 2010). Parameters with high ST but low S1 have significant ",
        "interaction effects. Reuses the \"Replications per Point\" value set above for Morris; n = 200 ",
        "Sobol design points (Saltelli 2007 estimator default), so wall-clock time is comparable to or ",
        "greater than the Morris screen above."),
      checkboxGroupInput("sobol_params", "Parameters to include (top 5 by μ* pre-selected)",
                        choices  = setNames(morris_params$name, MORRIS_LABELS[morris_params$name]),
                        selected = top5),
      actionButton("run_sobol", "Run Sobol Decomposition", class = "btn-primary"),
      tags$div(style = "margin-top: 10px;", uiOutput("sobol_status")),
      uiOutput("sobol_results_ui")
    )
  })

  # ── Sobol Decomposition (async via future + promises) ────────────────────

  observeEvent(input$run_sobol, {
    req(morris_state() == "done")
    top_params <- input$sobol_params
    if (length(top_params) == 0) {
      showModal(modalDialog(title = "Configuration error",
                            "Select at least one parameter for Sobol Decomposition.", easyClose = TRUE))
      return(invisible(NULL))
    }

    days_val  <- input$n_days
    nrep_val  <- as.integer(input$morris_nrep)
    app_dir   <- APP_DIR
    # Computed fresh right now, not cached — see detect_safe_cores()'s roxygen.
    max_cores_val <- detect_safe_cores(days_val)

    json_path <- tempfile("bch_sobol_config_", fileext = ".json")
    write_json(current_json(), json_path, pretty = TRUE, auto_unbox = TRUE)

    work_dir <- tempfile("bch_sobol_work_")
    dir.create(work_dir)

    prog_dir <- tempfile("bch_sobolprogress_")
    dir.create(prog_dir, recursive = TRUE)
    sobol_progress_dir(prog_dir)
    n_sobol_default <- 200
    sobol_progress_total(n_sobol_default * (length(top_params) + 2L))
    sobol_progress_done(0)

    sobol_state("running")
    sobol_error(NULL)

    output_rds <- tempfile("bch_sobol_result_", fileext = ".rds")
    worker_args <- c(
      "--mode", "sobol", "--json", json_path, "--days", days_val,
      "--n-rep", nrep_val, "--top-params", paste(top_params, collapse = ","),
      "--max-cores", max_cores_val, "--progress-dir", prog_dir,
      "--work-dir", work_dir, "--output-rds", output_rds
    )

    fut <- future({
      setwd(app_dir)
      run_shiny_worker(worker_args, output_rds)
    }, seed = NULL)

    prom <- fut %...>% (function(out) {
      sobol_results(out)
      sobol_state("done")
    })
    prom <- catch(prom, function(e) {
      sobol_state("error")
      sobol_error(conditionMessage(e))
    })
    pending_sobol(prom)
    invisible(NULL)
  })

  observe({
    req(sobol_state() == "running")
    invalidateLater(500, session)
    pd <- isolate(sobol_progress_dir())
    if (!is.null(pd) && dir.exists(pd)) {
      n_done <- length(list.files(pd, pattern = "\\.done$"))
      isolate(sobol_progress_done(n_done))
    }
  })

  output$sobol_status <- renderUI({
    state <- sobol_state()
    if (state == "idle") return(NULL)
    if (state == "running") {
      total <- sobol_progress_total(); done <- sobol_progress_done()
      pct <- if (total > 0) min(99, round(100 * done / total)) else 5
      tagList(
        p(sprintf("Evaluating Sobol design point %d of %d...", done, total)),
        div(class = "progress",
            div(class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar", style = sprintf("width: %d%%", pct)))
      )
    } else if (state == "done") {
      div(class = "alert alert-success", "Sobol decomposition complete.")
    } else {
      div(class = "alert alert-danger", paste("Sobol decomposition failed:", sobol_error()))
    }
  })

  output$sobol_plot <- renderPlot({
    out <- sobol_results()
    req(out, length(out$res) > 0)
    sb <- out$res$system_ot_q
    req(!is.null(sb))
    df <- bind_rows(
      data.frame(parameter = sb$parameter, index = "S1 (First-Order)", value = sb$S1,
                lower = sb$S1_lower, upper = sb$S1_upper),
      data.frame(parameter = sb$parameter, index = "ST (Total-Order)", value = sb$ST,
                lower = sb$ST_lower, upper = sb$ST_upper)
    )
    df$label <- MORRIS_LABELS[df$parameter]
    ggplot(df, aes(x = label, y = value, fill = index)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_errorbar(aes(ymin = pmax(lower, 0), ymax = upper),
                    position = position_dodge(width = 0.7), width = 0.2) +
      labs(title = "Sobol Indices — System OT Queue (R2B + R2E)",
           subtitle = "S1 = variance from the parameter alone; ST = variance including interactions with other parameters",
           x = NULL, y = "Sobol Index", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 20, hjust = 1))
  })

  output$dl_sobol_csv_zip <- downloadHandler(
    filename = "sobol_indices.zip",
    content  = function(file) {
      csvs <- sobol_results()$csv_bytes
      req(csvs)
      tmp_dir <- tempfile("bch_sobol_dl_")
      dir.create(tmp_dir)
      for (nm in names(csvs)) writeBin(csvs[[nm]], file.path(tmp_dir, nm))
      old_wd <- setwd(tmp_dir)
      on.exit(setwd(old_wd), add = TRUE)
      zip(file, files = names(csvs))
    },
    contentType = "application/zip"
  )

  output$sobol_results_ui <- renderUI({
    req(sobol_state() == "done", sobol_results())
    if (length(sobol_results()$res) == 0 || is.null(sobol_results()$res$system_ot_q)) {
      return(div(class = "alert alert-warning",
                  "Sobol indices could not be estimated for the selected parameters (a near-zero-variance response — see console warnings). Try a different parameter selection."))
    }
    tagList(
      tags$hr(),
      h5("Sobol S1 / ST Indices"),
      plotOutput("sobol_plot", height = "450px"),
      p(class = "text-muted small",
        "Parameters with high ST but low S1 have significant interaction effects — their influence on ",
        "system OT queue depends on the value of at least one other parameter, not just their own value."),
      downloadButton("dl_sobol_csv_zip", "Download Sobol Indices — All KPIs (ZIP)")
    )
  })

  # ── Transport Fleet Capacity Margin Sweep (async via future + promises,
  # Issue #57) ───────────────────────────────────────────────────────────────

  observeEvent(input$run_transport_sweep, {
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

    pmvamb_range <- as.integer(input$sweep_pmvamb_range)
    hx240m_range <- as.integer(input$sweep_hx240m_range)

    days_val <- input$n_days
    nrep_val <- as.integer(input$morris_nrep)
    app_dir  <- APP_DIR
    # Computed fresh right now, not cached — see detect_safe_cores()'s roxygen.
    max_cores_val <- detect_safe_cores(days_val)

    json_path <- tempfile("bch_transportsweep_config_", fileext = ".json")
    write_json(current_json(), json_path, pretty = TRUE, auto_unbox = TRUE)

    # plot_transport_capacity_margin_by_fleet_size() writes "images/<...>.png"
    # and "outputs/*.csv" relative to the working directory — a scratch
    # work_dir (mirroring Morris/Sobol's own work_dir convention above) keeps
    # a Shiny-triggered sweep from ever touching the repo's own tracked
    # images/ or outputs/.
    work_dir <- tempfile("bch_transportsweep_work_")
    dir.create(work_dir)

    prog_dir <- tempfile("bch_transportsweepprogress_")
    dir.create(prog_dir, recursive = TRUE)
    transport_sweep_progress_dir(prog_dir)
    n_points <- (pmvamb_range[2] - pmvamb_range[1] + 1L) + (hx240m_range[2] - hx240m_range[1] + 1L)
    transport_sweep_progress_total(n_points)
    transport_sweep_progress_done(0)

    transport_sweep_state("running")
    transport_sweep_error(NULL)

    output_rds <- tempfile("bch_transportsweep_result_", fileext = ".rds")
    worker_args <- c(
      "--mode", "transport_sweep", "--json", json_path, "--days", days_val,
      "--n-rep", nrep_val,
      "--pmvamb", sprintf("%d:%d", pmvamb_range[1], pmvamb_range[2]),
      "--hx240m", sprintf("%d:%d", hx240m_range[1], hx240m_range[2]),
      "--max-cores", max_cores_val, "--progress-dir", prog_dir,
      "--work-dir", work_dir, "--output-rds", output_rds
    )

    fut <- future({
      setwd(app_dir)
      run_shiny_worker(worker_args, output_rds)
    }, seed = NULL)

    prom <- fut %...>% (function(out) {
      transport_sweep_results(out$res)
      transport_sweep_state("done")
    })
    prom <- catch(prom, function(e) {
      transport_sweep_state("error")
      transport_sweep_error(conditionMessage(e))
    })
    pending_transport_sweep(prom)
    invisible(NULL)
  })

  observe({
    req(transport_sweep_state() == "running")
    invalidateLater(500, session)
    pd <- isolate(transport_sweep_progress_dir())
    if (!is.null(pd) && dir.exists(pd)) {
      n_done <- length(list.files(pd, pattern = "\\.done$"))
      isolate(transport_sweep_progress_done(n_done))
    }
  })

  output$transport_sweep_status <- renderUI({
    state <- transport_sweep_state()
    if (state == "idle") return(NULL)
    if (state == "running") {
      total <- transport_sweep_progress_total(); done <- transport_sweep_progress_done()
      pct <- if (total > 0) min(99, round(100 * done / total)) else 5
      tagList(
        p(sprintf("Evaluating fleet-size sweep point %d of %d...", done, total)),
        div(class = "progress",
            div(class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar", style = sprintf("width: %d%%", pct)))
      )
    } else if (state == "done") {
      div(class = "alert alert-success", "Transport fleet sweep complete.")
    } else {
      div(class = "alert alert-danger", paste("Transport fleet sweep failed:", transport_sweep_error()))
    }
  })

  output$transport_sweep_plot <- renderPlot({
    df <- transport_sweep_results()
    req(df)

    json <- current_json()
    current_qty <- setNames(
      vapply(json$transports, function(t) t$qty, numeric(1)),
      vapply(json$transports, function(t) t$name, character(1))
    )

    render_transport_sweep_plot(df, current_qty, n_rep = as.integer(input$morris_nrep))
  })

  output$dl_transport_sweep_csv <- downloadHandler(
    filename = "transport_capacity_by_fleet_size.csv",
    content  = function(file) write.csv(transport_sweep_results(), file, row.names = FALSE)
  )

  output$transport_sweep_results_ui <- renderUI({
    req(transport_sweep_state() == "done", transport_sweep_results())
    tagList(
      tags$hr(),
      h5("Transport Fleet Capacity Margin Sweep Results"),
      p(class = "text-muted small",
        "How to read this: in the top (queue) row, find where the line stops being flat at zero — that ",
        "fleet size is where transport becomes a bottleneck. If it's still flat at the dashed current-fleet ",
        "line, that fleet has spare capacity."),
      plotOutput("transport_sweep_plot", height = "500px"),
      downloadButton("dl_transport_sweep_csv", "Download Sweep Results (CSV)")
    )
  })
}

shinyApp(ui, server)
