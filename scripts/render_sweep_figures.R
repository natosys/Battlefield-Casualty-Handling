#!/usr/bin/env Rscript
##############################################################################
## scripts/render_sweep_figures.R                                           ##
## Result figures of the replicated experiments, rendered from tracked data ##
##############################################################################
#
# Usage:
#   Rscript scripts/render_sweep_figures.R                      # to outputs/images
#   Rscript scripts/render_sweep_figures.R --refresh-baseline   # to images/
#   Rscript scripts/render_sweep_figures.R --images-dir <dir>   # to <dir>
#
# Why this exists. The figures of docs/Results.md for the sweeps were written
# only by the script that ran each experiment, so a change to a title, a label
# or a layout cost the whole measurement again, hours of compute for the
# sweeps at the sustained horizon, or an untracked one-off call. The sweeps
# the paper reports with a table alone had no figure at all. Every plotted
# quantity is already a tracked CSV under data/, so a figure is a function of
# that data and nothing else. This script reads each CSV and calls the plot
# function its run script already used (R/analysis.R, R/scenario_runner.R) or
# the one R/figures.R holds, and runs no simulation: it sources no module that
# loads simmer, and scripts/check_figure_provenance.R asserts that.
#
# --refresh-baseline is the only way to write the tracked images/ copies,
# matching the contract every other render script carries: an ordinary run
# writes under outputs/ and cannot disturb tracked evidence.
#
# Each figure below is declared with a `RENDERS:` marker naming the image it
# writes; scripts/check_figure_provenance.R reads those markers to establish
# that every image docs/Results.md references has a producer.

suppressPackageStartupMessages({
  source("R/analysis.R")
  source("R/scenario_runner.R")
  source("R/casualty_surge.R")
})

args <- commandArgs(trailingOnly = TRUE)

#' Directory holding the tracked evidence sets
DATA_DIR <- "data"

#' Read one flagged command line argument
#'
#' @param flag Flag to look for, including its leading dashes.
#' @param default Value returned when the flag is absent or carries no value.
#' @return The argument following the flag, or `default`.
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

refresh <- "--refresh-baseline" %in% args
images_dir <- arg_value("--images-dir",
                        if (refresh) "images" else file.path("outputs", "images"))
dir.create(images_dir, recursive = TRUE, showWarnings = FALSE)

#' Read a tracked CSV from the data directory
#'
#' @param ... Path components under `DATA_DIR`.
#' @return The CSV as a data frame; stops naming the file if it is absent.
read_tracked <- function(...) {
  path <- file.path(DATA_DIR, ...)
  if (!file.exists(path)) stop("tracked evidence set not found: ", path)
  read.csv(path)
}

#' Write one figure and report it
#'
#' @param plot ggplot object.
#' @param name File name under the images directory.
#' @param width Width in inches.
#' @param height Height in inches.
#' @return The path written, invisibly.
save_figure <- function(plot, name, width, height) {
  path <- file.path(images_dir, name)
  ggsave(path, plot, width = width, height = height, dpi = 150, bg = "white")
  message("wrote ", path)
  invisible(path)
}

env_json <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Collect values from every node of a nested configuration
#'
#' @param node A list read from `env_data.json`, or one of its elements.
#' @param extract Function of one node returning a list of the values it holds
#'   (empty for a node holding none).
#' @return List of every value `extract` found, in document order.
collect_config <- function(node, extract) {
  if (!is.list(node)) return(list())
  found <- extract(node)
  for (child in node) found <- c(found, collect_config(child, extract))
  found
}

#' First value of one named variable in a configuration
#'
#' @param extract Function of one node returning a list of candidate values.
#' @param what Description of the variable for the error message.
#' @return The first value found; stops naming `what` when there is none.
first_config_value <- function(extract, what) {
  hits <- collect_config(env_json, extract)
  if (length(hits) == 0) stop("env_data.json carries no ", what)
  hits[[1]]
}

#' Value of one configured variable of one activity
#'
#' @param activity Activity name, e.g. `"recovery"`.
#' @param var Variable name within that activity.
#' @return The shipped value from `env_data.json`.
shipped_var <- function(activity, var) {
  first_config_value(function(node) {
    if (!identical(node$acty, activity)) return(list())
    Filter(Negate(is.null), lapply(node$vals, function(v) if (identical(v$var, var)) v$val))
  }, paste(activity, var, sep = "/"))
}

#' Shipped beds of one bed type at one element
#'
#' @param elm Element name, e.g. `"r2eheavy"`.
#' @param bed Bed name, e.g. `"hold"`.
#' @return The establishment from `env_data.json`.
shipped_beds <- function(elm, bed) {
  elms <- Filter(function(e) identical(e$elm, elm), env_json$elms)
  beds <- Filter(function(b) identical(b$name, bed), elms[[1]]$beds)
  as.integer(beds[[1]]$qty)
}

#' Shipped value of one strategic evacuation variable
#'
#' @param var Variable name of the strategic evacuation configuration.
#' @return The shipped value from `env_data.json`.
shipped_airlift <- function(var) {
  first_config_value(function(node) {
    if (identical(node$var, var) && !is.null(node$val)) list(node$val) else list()
  }, var)
}

# RENDERS: scenario_comparison.png
plot_scenario_comparison(read_tracked("scenarios", "scenario_comparison_queues.csv"),
                         images_dir = images_dir)
message("wrote ", file.path(images_dir, "scenario_comparison.png"))

current_qty <- setNames(
  vapply(env_json$transports, function(t) t$qty, numeric(1)),
  vapply(env_json$transports, function(t) t$name, character(1))
)

transport <- read_tracked("sweeps", "transport_capacity_by_fleet_size.csv")
transport_high <- read_tracked("sweeps", "transport_capacity_by_fleet_size_high_intensity.csv")

# RENDERS: transport_capacity_margin_by_fleet_size.png
p_transport <- render_transport_sweep_plot(transport, current_qty,
                                           n_rep = TRANSPORT_SWEEP_REPLICATIONS)
save_figure(p_transport, "transport_capacity_margin_by_fleet_size.png", 12, 8)

# RENDERS: transport_capacity_margin_by_fleet_size_high_intensity.png
p_transport_high <- render_transport_sweep_plot(transport_high, current_qty,
                                                n_rep = TRANSPORT_SWEEP_REPLICATIONS,
                                                scenario = "high_intensity")
save_figure(p_transport_high, "transport_capacity_margin_by_fleet_size_high_intensity.png", 12, 8)

post_op_rule <- env_json$vars$r2b$post_op_icu
forward_shipped <- FORWARD_HOLD_SWEEP_ARMS$window == post_op_rule$stability_window_dcs &
  FORWARD_HOLD_SWEEP_ARMS$window == post_op_rule$stability_window_single_stage &
  FORWARD_HOLD_SWEEP_ARMS$trigger == post_op_rule$capacity_trigger
forward_baseline <- if (any(forward_shipped)) FORWARD_HOLD_SWEEP_ARMS$label[forward_shipped][1]

# RENDERS: r2b_forward_hold_frontier.png
p_forward <- render_forward_hold_sweep_plot(read_tracked("sweeps", "r2b_forward_hold_frontier.csv"),
                                            baseline_arm = forward_baseline,
                                            n_rep = FORWARD_HOLD_SWEEP_REPLICATIONS)
save_figure(p_forward, "r2b_forward_hold_frontier.png", 10, 14)

# RENDERS: r2b_forward_hold_frontier_high_intensity.png
forward_high <- read_tracked("sweeps", "r2b_forward_hold_frontier_high_intensity.csv")
p_forward_high <- render_forward_hold_sweep_plot(forward_high, baseline_arm = forward_baseline,
                                                 n_rep = FORWARD_HOLD_SWEEP_REPLICATIONS)
save_figure(p_forward_high, "r2b_forward_hold_frontier_high_intensity.png", 10, 14)

# RENDERS: r2b_hold_threshold_sweep.png
p_threshold <- render_hold_threshold_sweep_plot(
  read_tracked("sweeps", "r2b_hold_threshold_sweep.csv"),
  baseline_beds = shipped_beds("r2b", "hold"),
  n_rep = HOLD_THRESHOLD_SWEEP_REPLICATIONS
)
save_figure(p_threshold, "r2b_hold_threshold_sweep.png", 12, 16)

surge_events <- read_tracked("casualty_surge", "casualty_surge_illustrative_events.csv")

# RENDERS: casualty_surge_events.png
save_figure(plot_casualty_surge_timeline(surge_events, CASUALTY_SURGE_DAYS),
            "casualty_surge_events.png", 12, 6)

surge_size <- read_tracked("casualty_surge", "casualty_surge_size_summary.csv")

# RENDERS: casualty_surge_size_sweep.png
save_figure(plot_casualty_surge_size(surge_size), "casualty_surge_size_sweep.png", 10, 6)

policy <- read_tracked("policy", "policy_sweep.csv")

# RENDERS: policy_sweep.png
save_figure(plot_policy_sweep(policy, shipped_var("recovery", "evacuation_policy_days")),
            "policy_sweep.png", 11, 10)

establishment <- read_tracked("policy", "establishment_sweep.csv")

# RENDERS: establishment_sweep.png
save_figure(plot_establishment_sweep(establishment, shipped_beds("r2eheavy", "hold")),
            "establishment_sweep.png", 11, 10)

saturation <- read_tracked("policy", "saturation_sweep.csv")
saturation_shipped <- shipped_var("second_surgery", "saturation_queue_threshold")

# RENDERS: saturation_sweep.png
save_figure(plot_saturation_sweep(saturation, saturation_shipped), "saturation_sweep.png", 11, 10)

# RENDERS: icu_gate.png
save_figure(plot_icu_gate(read_tracked("icu_gate", "icu_gate_summary.csv")), "icu_gate.png", 10, 8)

airlift_shipped <- list(
  failure_probability = shipped_airlift("failure_probability"),
  interval_days = shipped_airlift("schedule_interval_days")
)

# RENDERS: airlift_sweeps.png
p_airlift <- plot_airlift_sweeps(read_tracked("airlift", "airlift_summary.csv"), airlift_shipped)
save_figure(p_airlift, "airlift_sweeps.png", 11, 12)

# RENDERS: airlift_sweeps_high_intensity.png
p_airlift_high <- plot_airlift_sweeps(read_tracked("airlift", "airlift_summary_high_intensity.csv"),
                                      airlift_shipped, profile = "(High Intensity)")
save_figure(p_airlift_high, "airlift_sweeps_high_intensity.png", 11, 12)

# RENDERS: airlift_collapse.png
save_figure(plot_airlift_collapse(read_tracked("airlift", "airlift_collapse.csv")),
            "airlift_collapse.png", 9, 6)

# RENDERS: role4_demand_census.png
save_figure(plot_role4_census_over_time(read_tracked("role4_demand", "role4_demand_daily.csv")),
            "role4_demand_census.png", 12, 9)

# RENDERS: role4_demand_operations.png
save_figure(plot_role4_operations_over_time(
  read_tracked("role4_demand", "role4_demand_operations_weekly.csv")
), "role4_demand_operations.png", 11, 5)

# RENDERS: role4_demand_levers.png
save_figure(plot_role4_demand_levers(
  policy, establishment, saturation,
  read_tracked("role4_demand", "role4_demand_reliability_summary.csv"),
  shipped = list(policy = shipped_var("recovery", "evacuation_policy_days"),
                 establishment = shipped_beds("r2eheavy", "hold"),
                 saturation = saturation_shipped,
                 cancellation = airlift_shipped$failure_probability)
), "role4_demand_levers.png", 12, 10)

long_blocks <- read_tracked("long_horizon", "long_horizon_blocks.csv")
long_stability <- read_tracked("long_horizon", "long_horizon_stability.csv")

# RENDERS: long_horizon_blocks.png
save_figure(plot_long_horizon_blocks(long_blocks, long_stability),
            "long_horizon_blocks.png", 14, 16)

if (!refresh) {
  cat(sprintf("\nTracked images/ untouched. Re-run with --refresh-baseline to write them.\n"))
}
