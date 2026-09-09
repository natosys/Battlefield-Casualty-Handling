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
#   Rscript run.R --seed 42 --days 30 --iterations 1 --refresh-baseline
#   Rscript run.R --seed 42 --days 30 --iterations 1 --output-dir /tmp/run42
#   Rscript run.R --scenario moderate_intensity --days 360 --iterations 1
#   Rscript run.R --mode multi --iterations 30 --days 360 --max-cores 4
#   Rscript run.R --days 30 --iterations 1 --images-dir /tmp/plots
#
# RStudio Console:
#   source("run.R")          # loads run_bch() function
#   run_bch()                # default run: seed 42, 30 days, 1 iteration
#   run_bch(quick = TRUE)    # smoke test: 5 iterations, 5 days
#   run_bch(seed = 99, days = 10, iterations = 5)
#   run_bch(refresh_baseline = TRUE)   # regenerate tracked baseline evidence
#
# Artifact policy (Issue #154). Every run writes its analysis artifacts to
# output_dir ("outputs/", gitignored) and nothing else. The tracked seed-42
# baseline evidence set — images/, logs/logs.txt, data/arrivals_*.txt and
# data/mass_casualty_events.csv — is written only when refresh_baseline is
# TRUE, and then all of it is written together from the one run, so the set
# can never describe a mixture of runs. See the README's "Running the
# simulation" section for the full artifact table.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/warmup.R")
source("R/cli.R")

# ── Main function ─────────────────────────────────────────────────────────────

#' Print each analyse_run() plot to the active graphics device, in the same
#' order the pipeline previously auto-printed them, for interactive/RStudio
#' use. analyse_run() itself no longer calls print() (Issue #14 — it must be
#' safe to call from a headless Shiny session), so the CLI path reproduces
#' the original on-screen sequence here. Conditional plots are NULL when
#' their trigger condition (e.g. zero R2B hold occupants) was not met.
#'
#' @param results Named list returned by analyse_run()
#' @return Invisibly NULL; called for the plots it prints
print_analysis_plots <- function(results) {
  plot_order <- c(
    "casualty_flow", "r1_queues", "r2b_bed_queues",
    "r2b_hold_occupancy_plot", "r2b_bypass_reason_plot", "r2b_treatment",
    "r2b_gantt", "r2e_surgery", "r2e_bed_queues", "waiting_times",
    "transport_capacity_margin_plot", "r2e_gantt", "r2e_icu_gating_plot",
    "mass_casualty_timeline_plot", "force_regeneration_plot", "role4_census_plot",
    "ame_backlog_plot", "ame_sortie_plot"
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
#' @param refresh_baseline Write this run's artifacts to the tracked baseline
#'   locations (images/, logs/logs.txt, data/) instead of under output_dir
#'   (default FALSE). Requires iterations == 1: the console log and the
#'   arrival diagnostics describe one specific run's event stream and have no
#'   multi-replication equivalent, so a multi-run refresh could only ever
#'   produce a partial set, which is the failure mode this flag exists to
#'   prevent (Issue #154).
#' @param scenario    Name of the scenario profile to apply (default
#'   "default", the base parameters). Resolved by load_scenario(), which
#'   overlays only the scenario-specific subset of `vars`.
#' @param images_dir  Directory for this run's plots (default NULL, which
#'   derives output_dir/images). Ignored under refresh_baseline, where the
#'   tracked images/ is the only permitted destination.
#' @param max_cores   Cap on the cores mclapply forks across in multi-run
#'   mode (default NULL, leaving the option or detected core count in force).
#' @return Invisibly returns the monitoring data list
#'
#' @details A scenario profile and a baseline refresh are mutually exclusive.
#'   The tracked evidence set describes the shipped configuration, so letting a
#'   profile write it would reintroduce by another route the drift Issue #154
#'   closed: the set would no longer describe the configuration the repository
#'   ships.
run_bch <- function(seed = 42L, days = 30L, iterations = 1L,
                    quick = FALSE, output_dir = "outputs",
                    warm_up_days = WARM_UP_DAYS,
                    refresh_baseline = FALSE,
                    scenario = "default", images_dir = NULL,
                    max_cores = NULL) {
  if (quick) {
    seed <- 42L; days <- 5L; iterations <- 5L
    message("Quick mode: iterations=5, days=5, seed=42")
  }

  # A baseline refresh must produce the complete evidence set or none of it.
  # logs/logs.txt and data/arrivals_*.txt are records of one run's event
  # stream; there is no multi-replication analogue, so refusing here is what
  # keeps images/ from being refreshed against logs/ and data/ that describe
  # a different run.
  if (refresh_baseline && iterations != 1L) {
    stop("refresh_baseline = TRUE requires iterations = 1: the console log and ",
         "arrival diagnostics are single-run artifacts, so a multi-run refresh ",
         "would leave the tracked baseline set internally inconsistent.",
         call. = FALSE)
  }

  # A profile changes the parameters the run is made under, so the tracked set
  # would stop describing what the repository ships. Refused here as well as at
  # the CLI, run_bch() being documented for direct interactive use.
  if (refresh_baseline && !identical(scenario, "default")) {
    stop("refresh_baseline = TRUE requires scenario = \"default\", and scenario was ",
         sQuote(scenario), ": the tracked baseline evidence set describes the shipped ",
         "configuration, so a profile must not write it.", call. = FALSE)
  }

  # Two destination sets. Everything downstream writes through these, so the
  # tracked directories are reachable only via the flag above.
  images_dir <- if (refresh_baseline) {
    "images"
  } else if (is.null(images_dir)) {
    file.path(output_dir, "images")
  } else {
    images_dir
  }
  data_dir   <- if (refresh_baseline) "data"   else file.path(output_dir, "data")
  log_path   <- if (refresh_baseline) file.path("logs", "logs.txt") else file.path(output_dir, "logs.txt")

  if (refresh_baseline) {
    message("Baseline refresh: writing tracked artifacts to images/, logs/logs.txt and data/")
  }

  message(sprintf("Run configuration: iterations=%d, days=%d, seed=%d, scenario=%s",
                  iterations, days, seed, scenario))

  env_data <<- load_scenario("env_data.json", scenario)
  day_min  <<- DAY_MIN
  counts   <<- sapply(env_data$elms, length)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)

  if (iterations == 1L) {
    # ── Single-run path ────────────────────────────────────────────────────────
    # run_once() builds env, adds generators, runs, and returns wrap(env).
    # write_files = TRUE so arrival diagnostics land in data_dir.
    # Per-entity trajectory logging is captured to log_path for inspection.
    set.seed(seed)
    sink(log_path)
    # Unwinds the redirect whether or not run_once() succeeds. Under Rscript
    # the process exits either way, but run.R is documented for interactive
    # RStudio use, where an error would otherwise leave the console silently
    # redirected to a file with no indication why output had stopped.
    on.exit(sink(), add = TRUE)
    wrapped <- run_once(days, seed = NULL, write_files = TRUE, data_dir = data_dir)
    sink()
    on.exit()

    mon <- list(
      arrivals   = get_mon_arrivals(list(wrapped),   ongoing = TRUE),
      attributes = get_mon_attributes(list(wrapped)),
      resources  = get_mon_resources(list(wrapped))
    )

    message(sprintf("Simulation complete. Total arrivals: %d", nrow(mon$arrivals)))

  } else {
    # ── Multi-replication path ─────────────────────────────────────────────────
    # run_replications() uses L'Ecuyer-CMRG parallel RNG streams (mc.set.seed =
    # TRUE) for provably non-overlapping per-worker streams. set.seed() here
    # seeds the parent stream before mclapply advances substreams per worker.
    # write_files stays FALSE inside the forked workers (concurrent writes to
    # one path), which is why arrival diagnostics are single-run only.
    set.seed(seed)
    mon <- run_replications(iterations, days, max_cores = max_cores)

    message(sprintf("Replications complete. Total arrivals across all runs: %d",
                    nrow(mon$arrivals)))
  }

  # ── Artifacts common to both run modes ───────────────────────────────────────
  # summarise_replications() operates on the monitoring structure both paths
  # produce, so the KPI table is emitted in both modes rather than multi-run
  # only. At iterations = 1 the dispersion columns are NA by construction
  # (one observation), which is the honest representation of a single run.
  kpi <- summarise_replications(mon, warm_up_days = warm_up_days)
  kpi_path <- file.path(output_dir, "replication_summary.csv")
  write.csv(kpi, kpi_path, row.names = FALSE)
  message(sprintf("Replication KPI summary written to %s", kpi_path))

  results <- analyse_run(mon, output_dir = output_dir, warm_up_days = warm_up_days,
                         images_dir = images_dir)
  print_analysis_plots(results)

  message(sprintf("Analysis complete. CSV outputs in %s/, plots in %s/",
                  output_dir, images_dir))

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
                help = "Warm-up days to exclude from analysis (default: WARM_UP_DAYS constant)"),
    make_option("--output-dir", type = "character", default = "outputs",
                help = paste("Directory this run's artifacts are written to.",
                             "Ignored for the tracked baseline set, which",
                             "--refresh-baseline alone writes",
                             "[default: %default]")),
    make_option("--refresh-baseline", action = "store_true", default = FALSE,
                help = paste("Regenerate the tracked seed-42 baseline evidence set",
                             "(images/, logs/logs.txt, data/) from this run.",
                             "Requires --mode single and the default scenario.",
                             "Without it, no tracked file is written",
                             "[default: %default]")),
    make_option("--mode", type = "character", default = NULL,
                help = paste("Execution mode, 'single' or 'multi'. Omitted, it is",
                             "inferred from --iterations (1 gives single).",
                             "The two modes produce different artifact sets:",
                             "only single writes logs.txt and the arrival",
                             "diagnostics [default: inferred]")),
    make_option("--scenario", type = "character", default = "default",
                help = paste("Scenario profile from env_data.json's scenarios",
                             "block, or 'default' for the base parameters",
                             "[default: %default]")),
    make_option("--images-dir", type = "character", default = NULL,
                help = paste("Directory this run's plots are written to.",
                             "Ignored under --refresh-baseline",
                             "[default: <output-dir>/images]")),
    make_option("--max-cores", type = "integer", default = NULL,
                help = paste("Cap on cores used in multi-run mode. Unset, the",
                             "mc.cores option or the detected physical core",
                             "count is used [default: unset]"))
  )

  opt <- parse_args(OptionParser(option_list = option_list))

  # Every rule below is defined in R/cli.R and runs before run_bch(), so a
  # malformed argument costs no simulation and writes no file. --quick
  # overrides days, seed and iterations, so those are checked only when it is
  # absent.
  if (!opt$quick) {
    require_number_in_range(opt$days, "--days", 1)
    require_number_in_range(opt$iterations, "--iterations", 1)
    require_number_in_range(opt$seed, "--seed", -.Machine$integer.max,
                            .Machine$integer.max)
  }
  require_directory(opt$`output-dir`, "--output-dir")
  require_directory(opt$`images-dir`, "--images-dir")
  if (!is.null(opt$`max-cores`)) require_number_in_range(opt$`max-cores`, "--max-cores", 1)

  warm_up    <- if (is.null(opt$`warm-up`)) WARM_UP_DAYS else opt$`warm-up`
  iterations <- if (opt$quick) 5L else opt$iterations
  validate_warm_up(warm_up, if (opt$quick) 5L else opt$days)
  resolve_run_mode(opt$mode, iterations)
  validate_baseline_refresh(opt$`refresh-baseline`, iterations, opt$scenario)

  run_bch(seed = opt$seed, days = opt$days,
          iterations = opt$iterations, quick = opt$quick,
          output_dir = opt$`output-dir`,
          warm_up_days = warm_up,
          refresh_baseline = opt$`refresh-baseline`,
          scenario = opt$scenario,
          images_dir = opt$`images-dir`,
          max_cores = opt$`max-cores`)
}
