##############################################
## R/analysis_sweeps.R                      ##
## Capacity sweeps                          ##
##############################################
#
# The protocol constants of the transport fleet, forward holding and R2B
# holding capacity sweeps, the per-replication measurements they reduce each
# point to, and the runners and figures of each sweep.
#
# Sourced by R/analysis.R, which loads the packages and the shared modules
# these functions use; source that file rather than this one alone.

# ── The published capacity sweeps' protocol ──────────────────────────────────
# The designs docs/Methods.md documents for the transport
# fleet-size sweep and the forward holding frontier, held here so that the
# entry points' baseline refreshes, the methods paper's marker comments and
# tests/checks/check_capacity_sweep_protocol.R read one definition rather than
# three copies of it.

#' Campaign length all three published capacity sweeps run over, in days
#'
#' @details 360, the sustained-operations protocol `R/long_horizon.R`
#'   establishes. All three sweeps (transport fleet size, forward holding,
#'   R2B holding threshold) share it, having moved together from 30 days,
#'   so that a reader comparing their tables is comparing one horizon rather
#'   than asking why they differ; the R2B holding threshold sweep's own
#'   design record explains why the third is checked by no protocol script
#'   but shares this constant regardless.
CAPACITY_SWEEP_DAYS <- 360L

#' Control seed all three published capacity sweeps run under
CAPACITY_SWEEP_SEED <- 42L

#' Closing window the capacity sweeps' queue and occupancy responses are measured over, in days
#'
#' @details 90, the closing window `R/policy_sweep.R` measures the same pools
#'   over. A campaign's closing state rather than its average, because the
#'   question a capacity sweep asks is what the system settles to at a given
#'   establishment; an average over the whole horizon mixes a settled system
#'   with one still filling.
CAPACITY_SWEEP_WINDOW_DAYS <- 90L

#' Replications the published transport fleet-size sweep runs at, per point
#'
#' @details 30, the sustained-operations protocol's replication count,
#'   adopted alongside the horizon. The unpaired
#'   design carries no between-arm stream divergence to offset the longer
#'   horizon's gain, and the queue response it is sized against is a
#'   time-weighted mean rather than an accumulating count, so it behaves like
#'   the post-operative intensive care gate's `icu_occupancy` response
#'   (which improved on migration) rather than like a raw casualty count
#'   (which did not); `docs/Methods.md` records the measured
#'   outcome.
TRANSPORT_SWEEP_REPLICATIONS <- 30L

#' PMV Ambulance fleet sizes the published sweep covers
#'
#' @details Spans the shipped establishment of three and the sizes either side
#'   of it, so the sweep says what departing from the establishment costs
#'   rather than only that the establishment is adequate.
TRANSPORT_SWEEP_PMVAMB <- 1:5

#' HX2 40M truck fleet sizes the published sweep covers
TRANSPORT_SWEEP_HX240M <- 1:4

#' Replications the published forward holding frontier runs at, per point
#'
#' @details 30, set to match the other two
#'   capacity sweeps' replication count at the shared sustained-operations
#'   horizon, rather than because this sweep's own responses were found
#'   under-resolved: it runs under the shipped default configuration, below
#'   the casualty load at which any response fails to converge.
FORWARD_HOLD_SWEEP_REPLICATIONS <- 30L

#' Labels of the forward holding arms the published frontier covers
#'
#' @details One per arm, in the order the sweep runs them. The first arm is the
#'   shipped configuration, so the frontier's first point is the campaign the
#'   rest of the paper measures.
FORWARD_HOLD_SWEEP_LABELS <- c("Off (current)", "120 min", "240 min", "480 min", "1,440 min", "Capacity only", "240 min + capacity")

#' Stability windows of the forward holding arms, in minutes
#'
#' @details Applied to both surgical pathways. They run from two hours to the
#'   shipped forward hold limit of 24 hours, beyond which the limit binds and a
#'   longer window changes nothing.
FORWARD_HOLD_SWEEP_WINDOWS <- c(0, 120, 240, 480, 1440, 0, 240)

#' Capacity trigger of the forward holding arms
#'
#' @details 1 where the arm holds a casualty forward while R2E intensive care
#'   is saturated. The last two arms add it alone and beside a four-hour window.
FORWARD_HOLD_SWEEP_TRIGGERS <- c(0, 0, 0, 0, 0, 1, 1)

#' Forward holding arms the published frontier covers, as one data frame
#'
#' @details Assembled from the three vectors above, which stay single-line
#'   literals so that `tests/checks/check_capacity_sweep_protocol.R` can read them
#'   out of this file without sourcing it.
FORWARD_HOLD_SWEEP_ARMS <- data.frame(
  label   = FORWARD_HOLD_SWEEP_LABELS,
  window  = FORWARD_HOLD_SWEEP_WINDOWS,
  trigger = FORWARD_HOLD_SWEEP_TRIGGERS,
  stringsAsFactors = FALSE
)

# ── The published R2B holding capacity / evacuation threshold sweep's protocol
# The joint sweep Further Development L4 and the Option 2 research agenda item
# propose: the shortfall between R2B's fielded holding beds and its expected
# occupancy can be closed by adding beds, by an evacuation threshold that moves
# a long-stay convalescent rearward, or some mix of the two, and the two levers
# are substitutes a planner would price against each other. Held here for the
# same reason as the sweep constants above it: one definition read by this
# entry point's baseline refresh and by the methods paper's marker comment.

#' Replications the published R2B holding threshold sweep runs at, per point
#'
#' @details 30, set to match the other two
#'   capacity sweeps at the shared sustained-operations horizon, for the same
#'   reason `FORWARD_HOLD_SWEEP_REPLICATIONS`'s own migration states: this sweep
#'   also runs under the shipped default configuration.
HOLD_THRESHOLD_SWEEP_REPLICATIONS <- 30L

#' R2B holding beds per unit the published sweep covers
#'
#' @details Five is the shipped establishment; ten is the figure Option 2
#'   names as the capacity remedy ("increasing holding capacity to ten beds
#'   per facility"), so the sweep tests the paper's own proposed remedy rather
#'   than an arbitrary range. Seven is the intermediate point between them.
HOLD_THRESHOLD_SWEEP_BEDS <- c(5L, 7L, 10L)

#' R2B holding evacuation thresholds the published sweep covers, in minutes
#'
#' @details Zero is the shipped value, meaning the threshold is disabled and
#'   the whole drawn convalescence (`r2b.holding.min`/`max`/`mode`, a
#'   720-14400 minute triangular distribution with a 7200-minute mode) is
#'   served forward. Every value above zero is a duration in its own right:
#'   1440 minutes (one day) sits just above the distribution's 720-minute
#'   floor, so it binds nearly every draw and evacuates the large majority of
#'   casualties early; 4320 (three days) and 7200 (the distribution's own
#'   mode, five days) sit inside the range a casualty is actually drawn from
#'   and bind a shrinking share of it; 10080 (seven days) sits above the mode
#'   and binds only the longer stays. The step from zero to 1440 is therefore
#'   the discontinuity the parameter's disabled state creates, and the
#'   remaining steps trace the smooth region above it; reading the two apart
#'   is why the parameter is swept rather than screened (see Further
#'   Development L4 and `docs/Methods.md`'s discussion of this
#'   sweep for the reasoning).
HOLD_THRESHOLD_SWEEP_MINUTES <- c(0, 1440, 4320, 7200, 10080)

#' Compute replication-level pool queue and occupancy for matching resources
#'
#' @param mon Named list with a `resources` element as returned by
#'   run_replications() (R/replication.R)
#' @param pattern Regex matched against resource IDs (e.g. "^t_PMVAmb_")
#' @param n_days Campaign length in days.
#' @param establishment Number of units established in the pool, from
#'   `pool_establishment()`.
#' @param window_days Closing window to measure over, in days.
#' @return Data frame with one row per replication: replication, mean_q (the
#'   pool's total queue) and mean_util (the pool's occupancy, the fraction of
#'   its established units in use)
#'
#' @details Both quantities are time-weighted means over the closing window,
#'   of the pool taken as a whole rather than of each bed or vehicle taken
#'   separately and then averaged. The pool total is in none of the monitor's
#'   rows, so it is recovered by `pool_queue_steps()` (`R/queue_series.R`),
#'   the estimator the scenario comparison, the campaign time series and the
#'   policy sweep use, so every published queue figure measures one quantity.
#'   Occupancy divides by the established units rather than by those the
#'   monitor reports, a unit never seized having no row. Returns one row per
#'   replication, the unit the sweeps need to compute a 95% CI across
#'   replications at each sweep point.
pool_rep_kpis <- function(mon, pattern, n_days, establishment,
                          window_days = CAPACITY_SWEEP_WINDOW_DAYS) {
  rows  <- mon$resources[grepl(pattern, mon$resources$resource), ]
  # A smoke run shorter than the window is measured over its whole length.
  edges <- c(max(0, n_days - window_days) * DAY_MIN, n_days * DAY_MIN)

  per_replication <- lapply(split(rows, rows$replication), function(r) {
    queue  <- pool_queue_steps(r$resource, r$time, r$queue)
    server <- pool_queue_steps(r$resource, r$time, r$server)
    data.frame(
      replication = r$replication[1],
      mean_q      = step_bin_means(queue, edges),
      mean_util   = step_bin_means(server, edges) / establishment
    )
  })
  do.call(rbind, unname(per_replication))
}

#' Overflow census of every point of care across replications, as interval columns
#'
#' @param mon Named list with a `resources` element as returned by
#'   run_replications().
#' @param n_days Campaign length in days.
#' @param window_days Closing window to measure over, in days.
#' @return One-row data frame carrying, for each `OVERFLOW_RESPONSES` entry,
#'   its mean across replications and the 95% interval as `mean_<response>`,
#'   `ci_lower_<response>` and `ci_upper_<response>`.
#'
#' @details The census of each replication is overflow_window_census()'s over
#'   the same closing window the bed pools are measured over, so a sweep point
#'   reports where its saturation spills as well as how full its beds are. A
#'   replication in which no casualty reached a point contributes a zero.
overflow_rep_census <- function(mon, n_days, window_days = CAPACITY_SWEEP_WINDOW_DAYS) {
  from <- max(0, n_days - window_days) * DAY_MIN
  reps <- sort(unique(mon$resources$replication))
  per_rep <- do.call(rbind, lapply(reps, function(r) {
    overflow_window_census(mon$resources[mon$resources$replication == r, ], from,
                           n_days * DAY_MIN)
  }))
  out <- lapply(names(per_rep), function(col) {
    st <- summarise_ci(per_rep[[col]])
    setNames(data.frame(st$mean, pmax(st$ci_lower, 0), st$ci_upper),
             paste0(c("mean_", "ci_lower_", "ci_upper_"), col))
  })
  do.call(cbind, out)
}

#' Build the fleet-size sweep ggplot from already-computed sweep results
#'
#' @param sweep_df Data frame as returned in the `data` element of
#'   plot_transport_capacity_margin_by_fleet_size(): one row per vehicle x
#'   fleet size, with vehicle/qty/mean_q/ci_lower_q/ci_upper_q/mean_util/
#'   ci_lower_util/ci_upper_util columns.
#' @param current_qty Named numeric vector, vehicle name -> current
#'   establishment qty (e.g. from env_data.json's `transports` block).
#' @param n_rep Replications per fleet-size point, for the plot subtitle;
#'   NULL (default) uses a subtitle with no replication count.
#' @param scenario Name of the scenario profile the sweep ran under, for the
#'   plot subtitle; "default" (the default) omits it, since that is the
#'   configuration every other figure in this file is drawn from.
#' @return ggplot object: four panels arranged as a 2x2 grid — one column
#'   per vehicle type (PMV Ambulance, HX240M), one row per metric (Mean
#'   Queue, Pool Occupancy) — each showing that metric vs fleet size, with
#'   a 95% CI ribbon and a dashed vertical line at that vehicle's current
#'   establishment qty. A legend below the plot identifies the ribbon, the
#'   mean line, and the dashed reference line.
#'
#' @details Factored out of plot_transport_capacity_margin_by_fleet_size() so
#'   a caller other than the sweep itself
#'   can render the identical plot from a worker-returned sweep_df without
#'   duplicating the ggplot specification — app.R's transport_sweep_plot
#'   renderPlot() calls this directly. Facet column labels use the same
#'   human-readable vehicle names as the Configure panel's fleet-size
#'   sliders ("PMV Ambulance" rather than the raw env_data.json name
#'   "PMVAmb"), and the ribbon/mean-line/reference-line are mapped to named
#'   aesthetics purely so ggplot2 draws an explanatory legend for them,
#'   rather than leaving their meaning to prose alone.
render_transport_sweep_plot <- function(sweep_df, current_qty, n_rep = NULL, scenario = "default") {
  vehicles <- unique(sweep_df$vehicle)
  vehicle_labels <- c(PMVAmb = "PMV Ambulance", HX240M = "HX240M")
  vehicle_labels <- vehicle_labels[vehicles]
  vehicle_labels[is.na(vehicle_labels)] <- vehicles[is.na(vehicle_labels)]

  plot_df <- bind_rows(
    sweep_df %>% transmute(vehicle, qty, metric = "Pool Queue",
                           mean = mean_q, ci_lower = ci_lower_q, ci_upper = ci_upper_q),
    sweep_df %>% transmute(vehicle, qty, metric = "Pool Occupancy",
                           mean = mean_util, ci_lower = ci_lower_util, ci_upper = ci_upper_util)
  ) %>%
    mutate(
      vehicle = factor(vehicle, levels = vehicles),
      metric  = factor(metric, levels = c("Pool Queue", "Pool Occupancy"))
    )

  vline_df <- expand.grid(vehicle = vehicles, metric = levels(plot_df$metric),
                          stringsAsFactors = FALSE) %>%
    mutate(vehicle = factor(vehicle, levels = vehicles),
           qty     = as.numeric(current_qty[as.character(vehicle)]))

  subtitle <- if (!is.null(n_rep)) {
    sprintf(paste("%d replications per fleet-size point; queue and occupancy are",
                  "fleet totals over the closing %d days"),
            n_rep, CAPACITY_SWEEP_WINDOW_DAYS)
  } else {
    NULL
  }
  if (!identical(scenario, "default")) {
    scenario_note <- sprintf("%s profile", scenario)
    subtitle <- if (is.null(subtitle)) scenario_note else paste(subtitle, scenario_note, sep = ", ")
  }

  ggplot(plot_df, aes(x = qty, y = mean)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% Confidence Interval"), alpha = 0.3) +
    geom_line(aes(color = "Mean (across replications)"), linewidth = 1) +
    geom_point(aes(color = "Mean (across replications)"), size = 2) +
    geom_vline(data = vline_df, aes(xintercept = qty, linetype = "Current Fleet Size"),
              color = "firebrick", linewidth = 0.6) +
    facet_grid(metric ~ vehicle, scales = "free_y", switch = "y",
              labeller = labeller(vehicle = vehicle_labels)) +
    scale_x_continuous(breaks = function(lims) seq(floor(lims[1]), ceiling(lims[2]), by = 1)) +
    scale_fill_manual(name = NULL, values = c("95% Confidence Interval" = "steelblue")) +
    scale_color_manual(name = NULL, values = c("Mean (across replications)" = "steelblue4")) +
    scale_linetype_manual(name = NULL, values = c("Current Fleet Size" = "dashed")) +
    labs(title = "Transport Fleet Capacity Margin by Fleet Size",
         subtitle = subtitle,
         x = "Fleet Size (vehicles)", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
         strip.placement = "outside", legend.position = "bottom")
}

#' Plot medevac fleet capacity margin across a range of fleet sizes
#'
#' @param fleet_sizes Named list of integer vectors to sweep, e.g.
#'   list(PMVAmb = 1:5, HX240M = 1:4). Each element replaces the `qty` for
#'   the matching entry in env_data.json's `transports` block for that sweep
#'   point; the other vehicle type is held at its current establishment qty.
#' @param scenario Name of a scenario profile to run the sweep under
#'   (default "default", the shipped configuration). Resolved by
#'   resolve_scenario() (R/scenario.R) before the fleet-size overrides are
#'   applied, since a scenario overlays `vars` (casualty-generation
#'   parameters) while the sweep itself edits `transports`, the two never
#'   touching the same field. A non-default scenario suffixes the written
#'   CSV and PNG filenames with `_<scenario>` so a moderate- and a
#'   high-intensity sweep can both be tracked without one overwriting the
#'   other.
#' @param n_days Simulation duration per replication (default 30).
#' @param n_rep  Replications per fleet-size point, for CI bounds (default 5).
#' @param path File path to env_data.json (default "env_data.json").
#' @param output_dir Directory for CSV output (default "outputs").
#' @param images_dir Directory for the saved plot (default `outputs/images`).
#' @param progress_dir Optional directory path; when supplied, an empty
#'   marker file ("point_<i>.done") is written to it as each sweep point
#'   (one vehicle/qty combination) finishes evaluating, letting a caller on
#'   another process (e.g. the Shiny app's main session) observe real
#'   "point M of N" progress, mirroring run_morris()'s equivalent parameter
#'   (R/sensitivity.R). NULL (default) disables this.
#' @param max_cores Optional integer cap on mclapply's mc.cores at each
#'   sweep point, passed through to run_replications() (see that function's
#'   own @param, and run_morris()'s equivalent parameter, for why this
#'   matters for Shiny-triggered, locally-run sweeps). NULL preserves prior
#'   behaviour.
#' @return Named list: data (swept results, one row per vehicle x fleet
#'   size, with mean/95% CI for queue and utilisation), plot (ggplot object,
#'   also saved to images_dir/transport_capacity_margin_by_fleet_size.png,
#'   or that name with `_<scenario>` appended for a non-default scenario)
#'
#' @details For each vehicle type in `fleet_sizes` and each qty in its swept
#'   range, deep-copies the scenario-resolved env_data.json, overwrites that
#'   vehicle's `transports[[]]$qty` (the other vehicle type stays at its
#'   current establishment qty), rebuilds via build_environment(), and runs
#'   n_rep replications via run_replications() (R/replication.R) — the same
#'   replication engine the comparative scenario runner
#'   (R/scenario_runner.R) uses, reused here directly rather than duplicated.
#'   Fleet size is a structural (`elms`/`transports`) change, which
#'   resolve_scenario()'s overlay mechanism does not cover (R/scenario.R),
#'   so `scenario` is resolved once against the parsed JSON before the
#'   per-point fleet-size edits are applied on top of it, rather than the
#'   sweep going through run_scenario() for each point.
#'   Per-replication queue and occupancy are the fleet's pool total over the
#'   campaign's closing window, extracted with pool_rep_kpis() and aggregated
#'   to a mean and t-distribution 95% CI across replications at each sweep
#'   point,
#'   mirroring summarise_scenario_totals()'s aggregation convention
#'   (R/scenario_runner.R). Queue CI lower bounds are clamped to 0 and
#'   utilisation CI bounds to [0, 1], consistent with the CI-clamping
#'   convention used elsewhere in this file (e.g. p_r1_queues_ci,
#'   p_utilisation_ci). The plot itself is built by render_transport_sweep_plot()
#'   so that another caller can draw the same figure. The global
#'   env_data/day_min/counts are restored to their pre-call values on
#'   completion, consistent with run_morris()'s env_data_base restore
#'   pattern (R/sensitivity.R). The restore is registered with on.exit() at
#'   the point of save as well as run below the sweep loop, so a sweep point
#'   that fails cannot leave the session on the swept configuration; the
#'   explicit restore is what makes the CSV and plot written after the loop
#'   the caller's configuration's rather than the last sweep point's.
plot_transport_capacity_margin_by_fleet_size <- function(fleet_sizes = list(PMVAmb = 1:5, HX240M = 1:4),
                                                          scenario = "default",
                                                          n_days = CAPACITY_SWEEP_DAYS,
                                                          n_rep = TRANSPORT_SWEEP_REPLICATIONS,
                                                          path = "env_data.json",
                                                          output_dir = "outputs",
                                                          images_dir = file.path("outputs", "images"),
                                                          progress_dir = NULL, max_cores = NULL) {
  caller <- "plot_transport_capacity_margin_by_fleet_size"
  validate_sweep_args(n_days, n_rep, path, progress_dir, caller)
  validate_fleet_sizes(fleet_sizes, caller)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(images_dir,  showWarnings = FALSE, recursive = TRUE)

  config_snapshot <- capture_config_globals()
  env_data_base <- env_data
  day_min_base  <- day_min
  counts_base   <- counts
  # Registered at the point of save, so the error path restores too; see the
  # roxygen @details above.
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  json_data_base <- resolve_scenario(jsonlite::fromJSON(path, simplifyVector = FALSE), scenario)
  current_qty <- setNames(
    vapply(json_data_base$transports, function(t) t$qty, numeric(1)),
    vapply(json_data_base$transports, function(t) t$name, character(1))
  )

  scenario_suffix <- if (identical(scenario, "default")) "" else paste0("_", scenario)

  #' Mean and 95% confidence interval of a sweep point's replications
  #'
  #' @param x Numeric vector of one metric's per-replication values.
  #' @return A one-row data frame of mean, ci_lower and ci_upper.
  #' @details The t quantile takes `n - 1` degrees of freedom floored at one,
  #'   so a single-replication sweep point returns a wide interval rather than
  #'   failing.
  summarise_ci <- function(x) {
    n <- length(x)
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    data.frame(
      mean     = m,
      ci_lower = m - qt(0.975, df = pmax(n - 1, 1)) * s / sqrt(n),
      ci_upper = m + qt(0.975, df = pmax(n - 1, 1)) * s / sqrt(n)
    )
  }

  # Flattened to one row per (vehicle, qty) sweep point, rather than a
  # nested vehicle-then-qty lapply, so a single running index can drive the
  # progress_dir marker-file count across the whole sweep.
  sweep_points <- do.call(rbind, lapply(names(fleet_sizes), function(vehicle) {
    data.frame(vehicle = vehicle, qty = fleet_sizes[[vehicle]])
  }))

  sweep_df <- bind_rows(lapply(seq_len(nrow(sweep_points)), function(i) {
    vehicle <- sweep_points$vehicle[i]
    qty     <- sweep_points$qty[i]
    pattern <- paste0("^t_", vehicle, "_")

    message(sprintf("Transport fleet-size sweep: %s = %d (%d reps x %d days)...",
                    vehicle, qty, n_rep, n_days))

    json_data <- json_data_base
    for (j in seq_along(json_data$transports)) {
      if (identical(json_data$transports[[j]]$name, vehicle)) {
        json_data$transports[[j]]$qty <- qty
      }
    }

    env_data <<- build_environment(json_data)
    day_min  <<- DAY_MIN
    counts   <<- sapply(env_data$elms, length)

    mon      <- run_replications(n_rep, n_days, max_cores = max_cores)
    rep_kpis <- pool_rep_kpis(mon, pattern, n_days,
                              pool_establishment(env_data$transports, pattern))

    q_stats    <- summarise_ci(rep_kpis$mean_q)
    util_stats <- summarise_ci(rep_kpis$mean_util)

    if (!is.null(progress_dir)) {
      file.create(file.path(progress_dir, sprintf("point_%d.done", i)))
    }

    data.frame(
      vehicle       = vehicle,
      qty           = qty,
      mean_q        = q_stats$mean,
      ci_lower_q    = pmax(q_stats$ci_lower, 0),
      ci_upper_q    = q_stats$ci_upper,
      mean_util     = util_stats$mean,
      ci_lower_util = pmax(util_stats$ci_lower, 0),
      ci_upper_util = pmin(util_stats$ci_upper, 1)
    )
  }))

  env_data <<- env_data_base
  day_min  <<- day_min_base
  counts   <<- counts_base

  csv_name <- sprintf("transport_capacity_by_fleet_size%s.csv", scenario_suffix)
  png_name <- sprintf("transport_capacity_margin_by_fleet_size%s.png", scenario_suffix)

  write.csv(sweep_df, file.path(output_dir, csv_name), row.names = FALSE)
  message(sprintf("Transport fleet-size sweep results written to %s/%s", output_dir, csv_name))

  p <- render_transport_sweep_plot(sweep_df, current_qty, n_rep = n_rep, scenario = scenario)

  ggsave(file.path(images_dir, png_name), p, width = 12, height = 8, dpi = 150)

  list(data = sweep_df, plot = p)
}

#' Build the forward holding frontier ggplot from computed sweep results
#'
#' @param sweep_df Data frame as returned in the `data` element of
#'   plot_r2b_forward_hold_frontier(): one row per arm, with
#'   arm/window/capacity_trigger and mean_*/ci_lower_*/ci_upper_* columns for
#'   the R2E ICU queue, the R2B and R2E ICU utilisations, the post-definitive
#'   ICU share and the DOW count.
#' @param baseline_arm The label of the shipped arm, marked on each panel with
#'   a dashed reference line; NULL omits the line.
#' @param n_rep Replications per sweep point, for the plot subtitle; NULL
#'   (default) uses a subtitle with no replication count.
#' @return ggplot object: five stacked panels sharing the arm axis, R2E ICU
#'   mean queue, R2B ICU utilisation, R2E ICU utilisation, the share of
#'   post-definitive care served in an ICU bed rather than the degraded
#'   holding-bed fallback, and DOW count, each with a 95% CI ribbon.
#'
#' @details Factored out of plot_r2b_forward_hold_frontier() on the same
#'   rationale as render_transport_sweep_plot() above, so a caller holding
#'   only a sweep_df can redraw the figure without re-running the sweep.
#'   The two quantities a planner is actually trading off, the R2E ICU queue
#'   and the DOW count, are the first and last panels; the panels between
#'   them show where the load moved and whether moving it bought better
#'   post-definitive care. The arms are ordered as the sweep ran them, so the
#'   x axis is categorical rather than a numeric scale the capacity arms
#'   would have no place on.
render_forward_hold_sweep_plot <- function(sweep_df, baseline_arm = NULL, n_rep = NULL) {
  metrics <- c("R2E ICU Pool Queue", "R2B ICU Occupancy",
               "R2E ICU Occupancy", "Post-Definitive Care in ICU", "DOW Count")
  sweep_df$arm <- factor(sweep_df$arm, levels = unique(sweep_df$arm))

  plot_df <- bind_rows(
    sweep_df %>% transmute(arm, metric = metrics[1],
                           mean = mean_r2e_icu_q, ci_lower = ci_lower_r2e_icu_q, ci_upper = ci_upper_r2e_icu_q),
    sweep_df %>% transmute(arm, metric = metrics[2],
                           mean = mean_r2b_icu_util, ci_lower = ci_lower_r2b_icu_util, ci_upper = ci_upper_r2b_icu_util),
    sweep_df %>% transmute(arm, metric = metrics[3],
                           mean = mean_r2e_icu_util, ci_lower = ci_lower_r2e_icu_util, ci_upper = ci_upper_r2e_icu_util),
    sweep_df %>% transmute(arm, metric = metrics[4],
                           mean = mean_pd_icu_share, ci_lower = ci_lower_pd_icu_share, ci_upper = ci_upper_pd_icu_share),
    sweep_df %>% transmute(arm, metric = metrics[5],
                           mean = mean_dow, ci_lower = ci_lower_dow, ci_upper = ci_upper_dow)
  ) %>%
    mutate(metric = factor(metric, levels = metrics))

  subtitle <- if (!is.null(n_rep)) {
    sprintf(paste("%d replications per arm; queue and occupancy are pool totals",
                  "over the closing %d days; the ribbon is a 95%% confidence interval",
                  "across replications"),
            n_rep, CAPACITY_SWEEP_WINDOW_DAYS)
  } else {
    NULL
  }

  p <- ggplot(plot_df, aes(x = arm, y = mean, group = 1)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% Confidence Interval"), alpha = 0.3) +
    geom_line(aes(color = "Mean (across replications)"), linewidth = 1) +
    geom_point(aes(color = "Mean (across replications)"), size = 2)

  if (!is.null(baseline_arm)) {
    p <- p + geom_vline(aes(xintercept = match(baseline_arm, levels(sweep_df$arm)),
                            linetype = "Shipped Default"),
                        color = "firebrick", linewidth = 0.6) +
      scale_linetype_manual(name = NULL, values = c("Shipped Default" = "dashed"))
  }

  p +
    facet_wrap(~ metric, ncol = 1, scales = "free_y", strip.position = "left") +
    scale_fill_manual(name = NULL, values = c("95% Confidence Interval" = "steelblue")) +
    scale_color_manual(name = NULL, values = c("Mean (across replications)" = "steelblue4")) +
    labs(title = "Forward Holding at R2B: Decision Frontier",
         subtitle = subtitle,
         x = "Forward holding rule (stability window, capacity trigger)", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
          strip.placement = "outside", legend.position = "bottom",
          axis.text.x = element_text(angle = 30, hjust = 1))
}

#' Mean and 95% confidence interval for one sweep response
#'
#' @param x Numeric vector, one observation per replication.
#' @return A one-row data frame of mean, ci_lower and ci_upper.
#' @details Shared by the two capacity sweeps. The degrees of freedom are
#'   floored at one so a single-replication sweep returns a degenerate
#'   interval rather than NaN.
summarise_ci <- function(x) {
  n <- length(x)
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  data.frame(
    mean     = m,
    ci_lower = m - qt(0.975, df = pmax(n - 1, 1)) * s / sqrt(n),
    ci_upper = m + qt(0.975, df = pmax(n - 1, 1)) * s / sqrt(n)
  )

}

#' Per-replication share of post-definitive care served in an ICU bed
#'
#' @param mon Wrapped monitoring list from the replication framework.
#' @return A data frame of replication and its ICU share of post-definitive
#'   care, the remainder having taken the degraded holding-bed fallback.
#' @details This is the quality measure the forward ICU share lever is for:
#'   relieving stabilisation load off R2E only matters if the beds it frees go
#'   to the casualties needing intensive care after their definitive repair.
pd_icu_share_rep <- function(mon) {
  mon$attributes %>%
    filter(key == "post_definitive_pathway") %>%
    group_by(replication, name) %>%
    summarise(pathway = dplyr::last(value), .groups = "drop") %>%
    group_by(replication) %>%
    summarise(pd_icu_share = mean(pathway == 1), .groups = "drop")
}

#' Died-of-wounds count per replication
#'
#' @param mon Wrapped monitoring list from the replication framework.
#' @return A data frame of replication and its DOW count, zero where a
#'   replication recorded none.
#' @details Reduced per replication rather than pooled, because a confidence
#'   interval across replications is what the sweeps report.
#'   `pool_rep_kpis()` already returns queue and occupancy this way for
#'   an arbitrary resource pattern; the DOW count is the one metric needing
#'   its own reduction.
dow_rep_counts <- function(mon) {
  mon$attributes %>%
    filter(key == "dow", value == 1) %>%
    count(replication, name = "dow") %>%
    right_join(data.frame(replication = unique(mon$attributes$replication)),
               by = "replication") %>%
    mutate(dow = ifelse(is.na(dow), 0, dow))
}

#' Measure one forward holding arm
#'
#' @param env_data_base The built configuration to override.
#' @param arm One row of the arm table: label, window and trigger.
#' @param n_rep Replications.
#' @param n_days Simulation duration in days.
#' @param max_cores Cap on concurrent forks, or NULL.
#' @return A one-row data frame of the arm's responses with 95% intervals.
#'
#' @details Factored out of plot_r2b_forward_hold_frontier() so that an arm can
#'   be checkpointed as it completes.
measure_forward_hold_arm <- function(env_data_base, arm, n_rep, n_days, max_cores) {
  ed <- env_data_base
  ed$vars$r2b$post_op_icu$stability_window_dcs <- arm$window
  ed$vars$r2b$post_op_icu$stability_window_single_stage <- arm$window
  ed$vars$r2b$post_op_icu$capacity_trigger <- arm$trigger
  assign("env_data", ed, envir = globalenv())

  mon <- run_replications(n_rep, n_days, max_cores = max_cores)

  r2b_icu <- pool_rep_kpis(mon, "^b_r2b_icu_", n_days,
                           pool_establishment(ed$elms, "^b_r2b_icu_"))
  r2e_icu <- pool_rep_kpis(mon, "^b_r2eheavy_icu_", n_days,
                           pool_establishment(ed$elms, "^b_r2eheavy_icu_"))
  #' Mean and 95% interval of one response's replications
  #'
  #' @param x Numeric vector of per-replication values.
  #' @return Named vector of mean, lower and upper.
  ci <- function(x) {
    n <- length(x)
    m <- mean(x, na.rm = TRUE)
    h <- qt(0.975, df = pmax(n - 1, 1)) * sd(x, na.rm = TRUE) / sqrt(n)
    c(mean = m, lower = m - h, upper = m + h)
  }
  q   <- ci(r2e_icu$mean_q)
  rb  <- ci(r2b_icu$mean_util)
  re  <- ci(r2e_icu$mean_util)
  dow <- ci(dow_rep_counts(mon)$dow)
  pd  <- ci(pd_icu_share_rep(mon)$pd_icu_share)

  data.frame(
    arm                   = arm$label,
    window                = arm$window,
    capacity_trigger      = arm$trigger,
    mean_r2e_icu_q        = q[["mean"]],
    ci_lower_r2e_icu_q    = max(q[["lower"]], 0),
    ci_upper_r2e_icu_q    = q[["upper"]],
    mean_r2b_icu_util     = rb[["mean"]],
    ci_lower_r2b_icu_util = max(rb[["lower"]], 0),
    ci_upper_r2b_icu_util = min(rb[["upper"]], 1),
    mean_r2e_icu_util     = re[["mean"]],
    ci_lower_r2e_icu_util = max(re[["lower"]], 0),
    ci_upper_r2e_icu_util = min(re[["upper"]], 1),
    mean_pd_icu_share     = pd[["mean"]],
    ci_lower_pd_icu_share = max(pd[["lower"]], 0),
    ci_upper_pd_icu_share = min(pd[["upper"]], 1),
    mean_dow              = dow[["mean"]],
    ci_lower_dow          = max(dow[["lower"]], 0),
    ci_upper_dow          = dow[["upper"]]
  ) %>%
    cbind(overflow_rep_census(mon, n_days))
}

#' Sweep the forward holding rule and report the resulting decision frontier
#'
#' @param arms Data frame of forward holding arms to sweep: `label`, the
#'   `window` in minutes applied as both `r2b.post_op_icu.stability_window_dcs`
#'   and `.stability_window_single_stage`, and the `trigger` of 0 or 1 set as
#'   `capacity_trigger` (default `FORWARD_HOLD_SWEEP_ARMS`).
#' @param n_days Simulation duration per replication (default 30).
#' @param n_rep Replications per arm, for CI bounds (default 10).
#' @param path File path to env_data.json (default "env_data.json").
#' @param output_dir Directory for CSV output (default "outputs").
#' @param images_dir Directory for the saved plot (default `outputs/images`).
#' @param progress_dir Optional directory path; when supplied, an empty
#'   marker file ("point_<i>.done") is written to it as each arm
#'   finishes, mirroring plot_transport_capacity_margin_by_fleet_size().
#' @param max_cores Optional integer cap on mclapply's mc.cores at each
#'   arm, passed through to run_replications().
#' @param scenario Scenario profile to run under; a non-default profile suffixes the
#'   written CSV and PNG with `_<scenario>` (`scenario_output_suffix()`).
#' @param checkpoint_dir Directory each arm is checkpointed to as it completes, or
#'   NULL to measure without checkpointing.
#' @return Named list: data (one row per arm, with mean and 95% CI
#'   for the R2E ICU queue, R2B and R2E ICU utilisation, and DOW count),
#'   plot (ggplot object, also saved to
#'   images_dir/r2b_forward_hold_frontier.png)
#'
#' @details Answers the question forward holding exists to pose: how
#'   much of the R2E ICU constraint can be relieved by holding
#'   operated casualties forward at R2B, and what that costs in
#'   mortality. Holding a casualty forward moves ICU load from an
#'   intensivist-led facility to one without an intensivist, and
#'   `r2b_icu_penalty` prices that difference into the DOW ceiling (see
#'   r2b_post_op_stabilisation(), R/trajectories_r2b.R), so the two panels move
#'   in opposite directions and the frontier between them is the planner's
#'   actual decision surface. Reporting the frontier is more useful than
#'   choosing a single rule on the planner's behalf, which is why the
#'   shipped default holds nobody forward.
#'
#'   Structurally this mirrors plot_transport_capacity_margin_by_fleet_size()
#'   above and reuses the same replication engine, aggregation convention
#'   (mean and t-distribution 95% CI across replications) and
#'   global-state save/restore. It differs in that the swept quantity is a
#'   `vars` entry rather than an `elms`/`transports` one, so the rule is
#'   overwritten on the built env_data directly instead of on the parsed
#'   JSON before building.
plot_r2b_forward_hold_frontier <- function(arms = FORWARD_HOLD_SWEEP_ARMS,
                                           n_days = CAPACITY_SWEEP_DAYS,
                                           n_rep = FORWARD_HOLD_SWEEP_REPLICATIONS,
                                           path = "env_data.json",
                                           output_dir = "outputs",
                                           images_dir = file.path("outputs", "images"),
                                           progress_dir = NULL, max_cores = NULL,
                                           scenario = "default", checkpoint_dir = NULL) {
  caller <- "plot_r2b_forward_hold_frontier"
  validate_sweep_args(n_days, n_rep, path, progress_dir, caller)
  validate_forward_hold_arms(arms, caller)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(images_dir, showWarnings = FALSE, recursive = TRUE)

  config_snapshot <- capture_config_globals()
  # See plot_transport_capacity_margin_by_fleet_size() for why the restore is
  # registered here as well as run explicitly below the sweep loop.
  on.exit(restore_config_globals(config_snapshot), add = TRUE)
  if (!identical(scenario, "default")) {
    apply_config_globals(resolve_scenario(jsonlite::fromJSON(path, simplifyVector = FALSE),
                                          scenario))
  }
  env_data_base <- env_data
  day_min_base  <- day_min
  counts_base   <- counts

  rule <- env_data_base$vars$r2b$post_op_icu
  shipped <- arms$window == rule$stability_window_dcs &
    arms$window == rule$stability_window_single_stage &
    arms$trigger == rule$capacity_trigger
  baseline_arm <- if (any(shipped)) arms$label[shipped][1] else NULL

  sweep_df <- bind_rows(lapply(seq_len(nrow(arms)), function(i) {
    message(sprintf("Forward holding sweep: %s (%d reps x %d days)...",
                    arms$label[i], n_rep, n_days))
    ckpt <- if (is.null(checkpoint_dir)) NULL else {
      dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
      stem <- sprintf("forward_hold_arm_%d_%dr_%dd", i, n_rep, n_days)
      scenario_output_path(checkpoint_dir, stem, scenario)
    }
    point <- cached_hold_threshold_point(ckpt, function() {
      measure_forward_hold_arm(env_data_base, arms[i, ], n_rep, n_days, max_cores)
    })
    if (!is.null(progress_dir)) {
      file.create(file.path(progress_dir, sprintf("point_%d.done", i)))
    }
    point
  }))

  env_data <<- env_data_base
  day_min  <<- day_min_base
  counts   <<- counts_base

  csv_path <- scenario_output_path(output_dir, "r2b_forward_hold_frontier", scenario)
  write.csv(sweep_df, csv_path, row.names = FALSE)
  message(sprintf("Forward holding sweep results written to %s", csv_path))

  p <- render_forward_hold_sweep_plot(sweep_df, baseline_arm = baseline_arm, n_rep = n_rep)

  ggsave(scenario_output_path(images_dir, "r2b_forward_hold_frontier", scenario, ".png"), p,
         width = 10, height = 14, dpi = 150)

  list(data = sweep_df, plot = p)
}

#' Returns-to-duty count per replication
#'
#' @param mon Wrapped monitoring list from the replication framework.
#' @return A data frame of replication and its RTD count, zero where a
#'   replication recorded none.
#' @details `return_day` is set once per casualty who returns to duty, at any
#'   echelon (R/trajectories.R), so counting its rows per replication counts
#'   every return rather than one echelon's. Mirrors dow_rep_counts() above,
#'   which reduces the same monitoring list's `dow` key the same way.
rtd_rep_counts <- function(mon) {
  mon$attributes %>%
    filter(key == "return_day") %>%
    count(replication, name = "rtd") %>%
    right_join(data.frame(replication = unique(mon$attributes$replication)),
               by = "replication") %>%
    mutate(rtd = ifelse(is.na(rtd), 0, rtd))
}

#' Set the per-unit R2B holding bed establishment on parsed env_data.json
#'
#' @param json_data Parsed env_data.json, as `jsonlite::fromJSON(...,
#'   simplifyVector = FALSE)` returns it.
#' @param hold_beds Number of holding beds to establish at each R2B unit.
#' @return The parsed configuration with that establishment set on every R2B
#'   unit's `elms` entry.
#'
#' @details Every `elms` entry named `r2b` gets the same establishment, both
#'   R2B units being fielded to the same holding capacity in the current
#'   design; the two units cannot be swept independently because nothing
#'   distinguishes their establishments in `env_data.json`. Fails rather than
#'   returning the configuration unchanged where no R2B holding pool is found,
#'   on the convention set_hold_establishment() (R/policy_sweep.R) follows for
#'   R2E, so a sweep cannot silently measure the shipped establishment at
#'   every point and report it as a frontier.
set_r2b_hold_beds <- function(json_data, hold_beds) {
  found <- FALSE
  for (i in seq_along(json_data$elms)) {
    if (!identical(json_data$elms[[i]]$elm, "r2b")) next
    for (j in seq_along(json_data$elms[[i]]$beds)) {
      if (!identical(json_data$elms[[i]]$beds[[j]]$name, "hold")) next
      json_data$elms[[i]]$beds[[j]]$qty <- as.integer(hold_beds)
      found <- TRUE
    }
  }
  if (!found) {
    stop("no R2B holding bed pool found in elms, so the establishment ",
         "cannot be swept", call. = FALSE)
  }
  json_data
}

#' Build the R2B holding threshold sweep ggplot from computed sweep results
#'
#' @param sweep_df Data frame as returned in the `data` element of
#'   plot_r2b_hold_threshold_sweep(): one row per hold_beds x
#'   evac_threshold_min point, with mean_*/ci_lower_*/ci_upper_* columns for
#'   each response.
#' @param baseline_beds The shipped per-unit R2B holding establishment, used
#'   to label that series distinctly; NULL omits the distinction.
#' @param n_rep Replications per sweep point, for the plot subtitle; NULL
#'   (default) uses a subtitle with no replication count.
#' @return ggplot object: eight panels stacked by response, one line per swept
#'   `hold_beds` value against the evacuation threshold in days, each with a
#'   95% CI ribbon.
#'
#' @details Factored out of plot_r2b_hold_threshold_sweep() on the same
#'   rationale as render_forward_hold_sweep_plot() above. The forward pool (R2B
#'   holding) and the two pools it can transfer load onto (R2E holding and R2E
#'   intensive care) are grouped first, then the two campaign outcomes the
#'   trade is ultimately priced against, returns to duty and died of wounds.
#'   The x-axis is drawn in days rather than the minutes the parameter is
#'   configured in, because a planner reads a hold duration in days and the
#'   model's own doctrinal source states the range in days.
render_hold_threshold_sweep_plot <- function(sweep_df, baseline_beds = NULL, n_rep = NULL) {
  metrics <- c("R2B Hold Pool Queue", "R2B Hold Occupancy",
               "R2E Hold Pool Queue", "R2E Hold Occupancy",
               "R2E ICU Pool Queue", "R2E ICU Occupancy",
               "Returns to Duty", "Died of Wounds")

  #' One response's columns, renamed to the plot's shared mean/ci_lower/
  #' ci_upper names and labelled with its panel title
  #'
  #' @param base Column name stem, e.g. "r2b_hold_q" for `mean_r2b_hold_q`.
  #' @param metric_label One of `metrics`, naming this response's panel.
  #' @return `sweep_df`'s hold_beds and evac_threshold_days columns, plus
  #'   metric, mean, ci_lower and ci_upper for this response alone.
  one_metric <- function(base, metric_label) {
    sweep_df %>%
      transmute(hold_beds, evac_threshold_days, metric = metric_label,
                mean     = .data[[paste0("mean_", base)]],
                ci_lower = .data[[paste0("ci_lower_", base)]],
                ci_upper = .data[[paste0("ci_upper_", base)]])
  }

  plot_df <- bind_rows(
    one_metric("r2b_hold_q",    metrics[1]),
    one_metric("r2b_hold_util", metrics[2]),
    one_metric("r2e_hold_q",    metrics[3]),
    one_metric("r2e_hold_util", metrics[4]),
    one_metric("r2e_icu_q",     metrics[5]),
    one_metric("r2e_icu_util",  metrics[6]),
    one_metric("rtd",           metrics[7]),
    one_metric("dow",           metrics[8])
  ) %>%
    mutate(metric = factor(metric, levels = metrics),
           hold_beds_label = if (!is.null(baseline_beds)) {
             ifelse(hold_beds == baseline_beds,
                    sprintf("%d (shipped)", hold_beds), sprintf("%d", hold_beds))
           } else {
             sprintf("%d", hold_beds)
           })

  subtitle <- if (!is.null(n_rep)) {
    sprintf(paste("%d replications per point; queue and occupancy are pool totals over",
                  "the closing %d days; the ribbon is a 95%% confidence interval"),
            n_rep, CAPACITY_SWEEP_WINDOW_DAYS)
  } else {
    NULL
  }

  ggplot(plot_df, aes(x = evac_threshold_days, y = mean, color = hold_beds_label,
                      fill = hold_beds_label)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ metric, ncol = 2, scales = "free_y", strip.position = "top") +
    scale_color_brewer(name = "R2B holding beds\nper unit", palette = "Dark2") +
    scale_fill_brewer(name = "R2B holding beds\nper unit", palette = "Dark2") +
    labs(title = "R2B Holding Capacity vs. Evacuation Threshold — Joint Sweep",
         subtitle = subtitle,
         x = "Evacuation threshold (days convalescence served forward; 0 = disabled)", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
          legend.position = "bottom")
}

#' Measure one point of the R2B holding capacity by evacuation threshold grid
#'
#' @param json_data_base Parsed, scenario-resolved env_data.json.
#' @param beds R2B holding beds per unit at this point.
#' @param threshold Evacuation threshold at this point, in minutes.
#' @param n_rep Replications at the point.
#' @param n_days Simulation duration per replication.
#' @param max_cores Optional cap on mclapply's mc.cores.
#' @return A one-row data frame of the point's means and 95% intervals.
#'
#' @details Sets the global env_data, day_min and counts for the point; the
#'   caller restores them.
measure_hold_threshold_point <- function(json_data_base, beds, threshold, n_rep, n_days,
                                         max_cores) {
  json_data <- set_r2b_hold_beds(json_data_base, beds)
  ed <- build_environment(json_data)
  ed$vars$r2b$holding$evac_threshold <- threshold
  assign("env_data", ed, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(ed$elms, length), envir = globalenv())

  mon <- run_replications(n_rep, n_days, max_cores = max_cores)

  r2b_hold <- pool_rep_kpis(mon, "^b_r2b_hold_", n_days,
                            pool_establishment(env_data$elms, "^b_r2b_hold_"))
  r2e_hold <- pool_rep_kpis(mon, "^b_r2eheavy_hold_", n_days,
                            pool_establishment(env_data$elms, "^b_r2eheavy_hold_"))
  r2e_icu  <- pool_rep_kpis(mon, "^b_r2eheavy_icu_", n_days,
                            pool_establishment(env_data$elms, "^b_r2eheavy_icu_"))
  rtd      <- rtd_rep_counts(mon)
  dow      <- dow_rep_counts(mon)

  r2b_hold_q_stats    <- summarise_ci(r2b_hold$mean_q)
  r2b_hold_util_stats <- summarise_ci(r2b_hold$mean_util)
  r2e_hold_q_stats    <- summarise_ci(r2e_hold$mean_q)
  r2e_hold_util_stats <- summarise_ci(r2e_hold$mean_util)
  r2e_icu_q_stats     <- summarise_ci(r2e_icu$mean_q)
  r2e_icu_util_stats  <- summarise_ci(r2e_icu$mean_util)
  rtd_stats           <- summarise_ci(rtd$rtd)
  dow_stats           <- summarise_ci(dow$dow)

  data.frame(
    hold_beds               = beds,
    evac_threshold_min      = threshold,
    evac_threshold_days     = threshold / DAY_MIN,
    mean_r2b_hold_q         = r2b_hold_q_stats$mean,
    ci_lower_r2b_hold_q     = pmax(r2b_hold_q_stats$ci_lower, 0),
    ci_upper_r2b_hold_q     = r2b_hold_q_stats$ci_upper,
    mean_r2b_hold_util      = r2b_hold_util_stats$mean,
    ci_lower_r2b_hold_util  = pmax(r2b_hold_util_stats$ci_lower, 0),
    ci_upper_r2b_hold_util  = pmin(r2b_hold_util_stats$ci_upper, 1),
    mean_r2e_hold_q         = r2e_hold_q_stats$mean,
    ci_lower_r2e_hold_q     = pmax(r2e_hold_q_stats$ci_lower, 0),
    ci_upper_r2e_hold_q     = r2e_hold_q_stats$ci_upper,
    mean_r2e_hold_util      = r2e_hold_util_stats$mean,
    ci_lower_r2e_hold_util  = pmax(r2e_hold_util_stats$ci_lower, 0),
    ci_upper_r2e_hold_util  = pmin(r2e_hold_util_stats$ci_upper, 1),
    mean_r2e_icu_q          = r2e_icu_q_stats$mean,
    ci_lower_r2e_icu_q      = pmax(r2e_icu_q_stats$ci_lower, 0),
    ci_upper_r2e_icu_q      = r2e_icu_q_stats$ci_upper,
    mean_r2e_icu_util       = r2e_icu_util_stats$mean,
    ci_lower_r2e_icu_util   = pmax(r2e_icu_util_stats$ci_lower, 0),
    ci_upper_r2e_icu_util   = pmin(r2e_icu_util_stats$ci_upper, 1),
    mean_rtd                = rtd_stats$mean,
    ci_lower_rtd            = pmax(rtd_stats$ci_lower, 0),
    ci_upper_rtd            = rtd_stats$ci_upper,
    mean_dow                = dow_stats$mean,
    ci_lower_dow            = pmax(dow_stats$ci_lower, 0),
    ci_upper_dow            = dow_stats$ci_upper
  ) %>%
    cbind(overflow_rep_census(mon, n_days))
}

#' Read a grid point's checkpoint, or measure it and write the checkpoint
#'
#' @param path Checkpoint file, or NULL to measure without checkpointing.
#' @param measure Function of no arguments returning the point's data frame.
#' @return The point's data frame.
#'
#' @details A sweep of this size runs for hours, so each point is written as it
#'   completes and read back rather than re-measured after an interruption.
cached_hold_threshold_point <- function(path, measure) {
  if (!is.null(path) && file.exists(path)) return(read.csv(path, stringsAsFactors = FALSE))
  point <- measure()
  if (!is.null(path)) write.csv(point, path, row.names = FALSE)
  point
}

#' Sweep R2B holding capacity jointly against the evacuation threshold
#'
#' @param hold_beds Numeric vector of R2B holding beds per unit to sweep
#'   (default HOLD_THRESHOLD_SWEEP_BEDS).
#' @param evac_threshold_min Numeric vector of evacuation thresholds to sweep,
#'   in minutes (default HOLD_THRESHOLD_SWEEP_MINUTES); zero disables the
#'   threshold.
#' @param n_days Simulation duration per replication (default 30).
#' @param n_rep Replications per grid point, for CI bounds (default 10).
#' @param path File path to env_data.json (default "env_data.json").
#' @param output_dir Directory for CSV output (default "outputs").
#' @param images_dir Directory for the saved plot (default `outputs/images`).
#' @param progress_dir Optional directory path; when supplied, an empty
#'   marker file ("point_<i>.done") is written to it as each grid point
#'   finishes, mirroring plot_transport_capacity_margin_by_fleet_size().
#' @param max_cores Optional integer cap on mclapply's mc.cores at each grid
#'   point, passed through to run_replications().
#' @param scenario Name of a scenario profile to run the sweep under, resolved
#'   once against the parsed JSON before the grid is run. A non-default
#'   scenario suffixes the written CSV and PNG filenames with `_<scenario>`
#'   (`scenario_output_suffix()`), so it cannot overwrite the default's.
#' @param checkpoint_dir Optional directory each grid point is checkpointed to as
#'   it completes and resumed from, named for its replications and days so a
#'   checkpoint from another protocol is never reused. NULL measures without.
#' @return Named list: data (one row per hold_beds x evac_threshold_min grid
#'   point, with mean and 95% CI for R2B holding queue and utilisation, R2E
#'   holding queue and utilisation, R2E intensive care queue and utilisation,
#'   returns to duty and died of wounds), plot (ggplot object, also saved to
#'   images_dir/r2b_hold_threshold_sweep.png).
#'
#' @details Answers Option 2's open question directly: whether to close the
#'   R2B holding shortfall (Further Development L4) with more beds, an
#'   evacuation threshold, or some mix of the two, and what either buys the
#'   two pools it can transfer load onto. The grid is the cross product of
#'   both axes, following the shape scripts/run_policy_sweep.R's establishment
#'   sweep uses for the analogous R2E trade, but built the way the other two
#'   capacity sweeps in this file are: `hold_beds` changes an `elms` bed count
#'   and so is set on the parsed JSON before build_environment() runs for that
#'   point (set_r2b_hold_beds(), mirroring the transport fleet-size sweep's
#'   `transports` edit above), while `evac_threshold_min` changes a `vars`
#'   entry and so is set on the already-built env_data directly (mirroring
#'   plot_r2b_forward_hold_frontier() above it). The global env_data/day_min/
#'   counts are restored to their pre-call values on completion, including on
#'   the error path, matching every sweep in this file.
plot_r2b_hold_threshold_sweep <- function(hold_beds = HOLD_THRESHOLD_SWEEP_BEDS,
                                          evac_threshold_min = HOLD_THRESHOLD_SWEEP_MINUTES,
                                          n_days = CAPACITY_SWEEP_DAYS,
                                          n_rep = HOLD_THRESHOLD_SWEEP_REPLICATIONS,
                                          path = "env_data.json",
                                          output_dir = "outputs",
                                          images_dir = file.path("outputs", "images"),
                                          progress_dir = NULL, max_cores = NULL,
                                          scenario = "default", checkpoint_dir = NULL) {
  caller <- "plot_r2b_hold_threshold_sweep"
  validate_sweep_args(n_days, n_rep, path, progress_dir, caller)
  validate_hold_threshold_sweep(hold_beds, evac_threshold_min, caller)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(images_dir, showWarnings = FALSE, recursive = TRUE)

  config_snapshot <- capture_config_globals()
  env_data_base <- env_data
  day_min_base  <- day_min
  counts_base   <- counts
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  json_data_base <- resolve_scenario(jsonlite::fromJSON(path, simplifyVector = FALSE), scenario)

  baseline_beds_vals <- vapply(json_data_base$elms, function(e) {
    if (!identical(e$elm, "r2b")) return(NA_integer_)
    beds <- Filter(function(b) identical(b$name, "hold"), e$beds)
    if (length(beds) == 0) return(NA_integer_)
    as.integer(beds[[1]]$qty)
  }, integer(1))
  baseline_beds <- baseline_beds_vals[!is.na(baseline_beds_vals)][1]

  # Flattened to one row per (hold_beds, evac_threshold_min) grid point,
  # rather than a nested loop, so a single running index drives the
  # progress_dir marker-file count across the whole sweep (mirrors the
  # transport fleet-size sweep's sweep_points construction above).
  grid <- expand.grid(hold_beds = hold_beds, evac_threshold_min = evac_threshold_min,
                      KEEP.OUT.ATTRS = FALSE)

  sweep_df <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
    beds      <- grid$hold_beds[i]
    threshold <- grid$evac_threshold_min[i]

    message(sprintf("R2B holding sweep: %d beds/unit, threshold %.0f min (%d reps x %d days)...",
                    beds, threshold, n_rep, n_days))

    ckpt <- if (is.null(checkpoint_dir)) NULL else {
      scenario_output_path(checkpoint_dir, sprintf("hold_threshold_point_%db_%.0fmin_%dr_%dd",
                                                   beds, threshold, n_rep, n_days), scenario)
    }
    point <- cached_hold_threshold_point(ckpt, function() {
      measure_hold_threshold_point(json_data_base, beds, threshold, n_rep, n_days, max_cores)
    })

    if (!is.null(progress_dir)) {
      file.create(file.path(progress_dir, sprintf("point_%d.done", i)))
    }
    point
  }))

  env_data <<- env_data_base
  day_min  <<- day_min_base
  counts   <<- counts_base

  csv_path <- scenario_output_path(output_dir, "r2b_hold_threshold_sweep", scenario)
  write.csv(sweep_df, csv_path, row.names = FALSE)
  message("R2B holding threshold sweep results written to ", csv_path)

  p <- render_hold_threshold_sweep_plot(sweep_df, baseline_beds = baseline_beds, n_rep = n_rep)

  ggsave(scenario_output_path(images_dir, "r2b_hold_threshold_sweep", scenario, ".png"), p,
         width = 12, height = 16, dpi = 150)

  list(data = sweep_df, plot = p)
}
