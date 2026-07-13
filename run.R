#!/usr/bin/env Rscript
##############################################
## run.R — Simulation entry point           ##
## Battlefield Casualty Handling Simulation ##
##############################################
#
# Terminal / Claude Code cloud:
#   Rscript run.R --seed 42 --days 30 --iterations 1
#   Rscript run.R --seed 42 --days 30 --iterations 10
#   Rscript run.R --quick
#
# RStudio Console:
#   source("run.R")          # loads run_bch() function
#   run_bch()                # default run: seed 42, 30 days, 1 iteration
#   run_bch(quick = TRUE)    # smoke test: 5 iterations, 5 days
#   run_bch(seed = 99, days = 10, iterations = 5)

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/warmup.R")

# ── Main function ─────────────────────────────────────────────────────────────

#' Print each analyse_run() plot to the active graphics device, in the same
#' order the pipeline previously auto-printed them, for interactive/RStudio
#' use. analyse_run() itself no longer calls print() (Issue #14 — it must be
#' safe to call from a headless Shiny session), so the CLI path reproduces
#' the original on-screen sequence here. Conditional plots are NULL when
#' their trigger condition (e.g. zero R2B hold occupants) was not met.
#'
#' @param results Named list returned by analyse_run()
print_analysis_plots <- function(results) {
  plot_order <- c(
    "casualty_flow", "r1_queues", "r2b_bed_queues",
    "r2b_hold_occupancy_plot", "r2b_bypass_reason_plot", "r2b_treatment",
    "r2b_gantt", "r2e_surgery", "r2e_bed_queues", "waiting_times",
    "transport_capacity_margin_plot", "r2e_gantt", "r2e_icu_gating_plot",
    "mass_casualty_timeline_plot", "force_regeneration_plot", "role4_census_plot"
  )
  for (plot_name in plot_order) {
    if (!is.null(results[[plot_name]])) print(results[[plot_name]])
  }
}

#' Run the BCH simulation
#'
#' @param seed        Random seed for single-run mode (default 42; ignored in
#'   multi-run mode — each replication uses an independent random draw)
#' @param days        Simulation duration in days (default 30)
#' @param iterations  Number of replications (default 1; >1 activates
#'   parallel multi-run via mclapply)
#' @param quick       Smoke-test mode: seed 42, 5 days, 5 iterations
#' @param output_dir  Directory for output files (default "outputs")
#' @param warm_up_days Days to exclude from the start of the analysis window
#'   (Welch warm-up period; default WARM_UP_DAYS constant from warmup.R)
#' @return Invisibly returns the monitoring data list
run_bch <- function(seed = 42L, days = 30L, iterations = 1L,
                    quick = FALSE, output_dir = "outputs",
                    warm_up_days = WARM_UP_DAYS) {
  if (quick) {
    seed <- 42L; days <- 5L; iterations <- 5L
    message("Quick mode: iterations=5, days=5, seed=42")
  }

  message(sprintf("Run configuration: iterations=%d, days=%d, seed=%d",
                  iterations, days, seed))

  env_data <<- load_elms("env_data.json")
  day_min  <<- 1440L
  counts   <<- sapply(env_data$elms, length)

  if (iterations == 1L) {
    # ── Single-run path ────────────────────────────────────────────────────────
    # run_once() builds env, adds generators, runs, and returns wrap(env).
    # write_files = TRUE so arrival diagnostics land in data/.
    # Logs are captured to logs/logs.txt for single-run inspection.
    set.seed(seed)
    sink(file.path("logs", "logs.txt"))
    wrapped <- run_once(days, seed = NULL, write_files = TRUE)
    sink()

    mon <- list(
      arrivals   = get_mon_arrivals(list(wrapped),   ongoing = TRUE),
      attributes = get_mon_attributes(list(wrapped)),
      resources  = get_mon_resources(list(wrapped))
    )

    message(sprintf("Simulation complete. Total arrivals: %d", nrow(mon$arrivals)))

    results <- analyse_run(mon, output_dir = output_dir, warm_up_days = warm_up_days)
    print_analysis_plots(results)

    message(sprintf("Analysis complete. Outputs written to %s/", output_dir))

  } else {
    # ── Multi-replication path ─────────────────────────────────────────────────
    # run_replications() uses L'Ecuyer-CMRG parallel RNG streams (mc.set.seed =
    # TRUE) for provably non-overlapping per-worker streams. set.seed() here
    # seeds the parent stream before mclapply advances substreams per worker.
    set.seed(seed)
    mon <- run_replications(iterations, days)

    message(sprintf("Replications complete. Total arrivals across all runs: %d",
                    nrow(mon$arrivals)))

    kpi <- summarise_replications(mon, warm_up_days = warm_up_days)
    kpi_path <- file.path(output_dir, "replication_summary.csv")
    write.csv(kpi, kpi_path, row.names = FALSE)
    message(sprintf("Replication KPI summary written to %s", kpi_path))

    results <- analyse_run(mon, output_dir = output_dir, warm_up_days = warm_up_days)
    print_analysis_plots(results)

    message(sprintf("Analysis complete. Outputs written to %s/", output_dir))
  }

  invisible(mon)
}

# ── CLI entry point (Rscript only) ───────────────────────────────────────────

if (!interactive()) {
  suppressPackageStartupMessages(library(optparse))

  option_list <- list(
    make_option("--iterations", type = "integer", default = 1L,
                help = "Number of replications [default: %default]"),
    make_option("--days",       type = "integer", default = 30L,
                help = "Simulation duration in days [default: %default]"),
    make_option("--seed",       type = "integer", default = 42L,
                help = "Random seed [default: %default]"),
    make_option("--quick",      action = "store_true", default = FALSE,
                help = "Smoke-test mode: 5 iterations, 5 days, seed 42"),
    make_option("--warm-up", type = "integer", default = NULL,
                help = "Warm-up days to exclude from analysis (default: WARM_UP_DAYS constant)")
  )

  opt <- parse_args(OptionParser(option_list = option_list))

  warm_up <- if (is.null(opt$`warm-up`)) WARM_UP_DAYS else opt$`warm-up`
  run_bch(seed = opt$seed, days = opt$days,
          iterations = opt$iterations, quick = opt$quick,
          warm_up_days = warm_up)
}
