##############################################
## R/scenario_runner.R                      ##
## Comparative scenario runner (Issue #10)  ##
##############################################
#
# Executes the multi-replication framework (R/replication.R) against a named
# scenario profile (R/scenario.R) and aggregates results for cross-scenario
# comparison. Depends on R/environment.R (load_scenario/build_environment),
# R/replication.R (run_replications/summarise_replications), and dplyr for
# aggregation — source those before this file.

library(dplyr)
library(ggplot2)

# ── Single-scenario execution ─────────────────────────────────────────────────

#' Compute replication-level totals (casualty counts, DOW count, DOW/WIA rate)
#'
#' @param mon Named list with arrivals, attributes, resources as returned by
#'   run_replications()
#' @param warm_up_days Days to exclude from the start of each replication
#'   (default 0 = no exclusion)
#' @return Data frame with one row per metric (total_casualties, wia_count,
#'   dow_count, dow_rate): n_reps, mean, p10, p90, and 95% CI bounds across
#'   replications
#'
#' @details `dow_rate` is DOW count as a proportion of WIA count (combat +
#'   support), not of total casualties — this matches the "DOW/WIA rate"
#'   convention used throughout this project (see Died of Wounds and
#'   Scenario Profiles sections in README.md), since only WIA/NBI casualties
#'   pass through the DOW check (disease and battle-fatigue DNBI, and KIA,
#'   are exempt — see R/trajectories.R). Mirrors the aggregation convention
#'   of summarise_replications() (R/replication.R) so scenario-level totals
#'   report in the same mean (p10-p90), 95% CI format used throughout this
#'   project's analysis.
summarise_scenario_totals <- function(mon, warm_up_days = 0) {
  warm_up_min <- as.integer(warm_up_days) * 1440L

  arrivals <- mon$arrivals %>%
    filter(start_time >= warm_up_min) %>%
    mutate(casualty_type = stringr::str_extract(name, "^[^_]+"))

  totals_by_rep <- arrivals %>%
    group_by(replication) %>%
    summarise(total_casualties = n(), .groups = "drop")

  wia_by_rep <- arrivals %>%
    filter(casualty_type == "wia") %>%
    group_by(replication) %>%
    summarise(wia_count = n(), .groups = "drop")

  dow_by_rep <- mon$attributes %>%
    filter(key == "dow", value == 1) %>%
    group_by(replication) %>%
    summarise(dow_count = n(), .groups = "drop")

  rep_totals <- totals_by_rep %>%
    left_join(wia_by_rep, by = "replication") %>%
    left_join(dow_by_rep, by = "replication") %>%
    mutate(
      wia_count = ifelse(is.na(wia_count), 0, wia_count),
      dow_count = ifelse(is.na(dow_count), 0, dow_count),
      dow_rate  = dow_count / wia_count
    )

  summarise_metric <- function(x) {
    n <- length(x)
    m <- mean(x)
    s <- sd(x)
    data.frame(
      n_reps   = n,
      mean     = m,
      p10      = as.numeric(quantile(x, 0.10, na.rm = TRUE)),
      p90      = as.numeric(quantile(x, 0.90, na.rm = TRUE)),
      ci_lower = m - qt(0.975, df = n - 1) * s / sqrt(n),
      ci_upper = m + qt(0.975, df = n - 1) * s / sqrt(n)
    )
  }

  bind_rows(
    cbind(metric = "total_casualties", summarise_metric(rep_totals$total_casualties)),
    cbind(metric = "wia_count",        summarise_metric(rep_totals$wia_count)),
    cbind(metric = "dow_count",        summarise_metric(rep_totals$dow_count)),
    cbind(metric = "dow_rate",         summarise_metric(rep_totals$dow_rate))
  )
}

#' Run n_iterations replications under a named scenario profile
#'
#' @param scenario Name of scenario profile ("default", or a key under the
#'   top-level `scenarios` block in env_data.json, e.g. "moderate_intensity",
#'   "high_intensity"). Passed to resolve_scenario() (R/scenario.R), which
#'   raises an explicit error listing available profiles if unrecognised.
#' @param n_iterations Number of replications (default 10)
#' @param n_days Simulation duration in days (default 30)
#' @param path File path to env_data.json (default "env_data.json")
#' @param ot_hours Hours per day the first OT shift is active (default 12);
#'   threaded to run_replications() -> build_env()
#' @param warm_up_days Days to exclude from the start of each replication
#'   (default 0 = no exclusion)
#' @return Named list: scenario (name), label (scenario's `label` field, or
#'   "Default (base configuration)"), n_iterations, n_days, mon (raw
#'   monitoring data), queue_kpi (summarise_replications() output),
#'   totals (summarise_scenario_totals() output)
#'
#' @details Sets env_data, day_min, and counts globally (<<-), consistent
#'   with run.R and scripts/run_sensitivity.R, since run_once()/build_env()
#'   resolve these from the global environment. Parses env_data.json once and
#'   resolves the scenario via resolve_scenario() + build_environment()
#'   directly (rather than calling load_scenario() a second time) purely to
#'   recover the scenario's `label` field for reporting.
run_scenario <- function(scenario, n_iterations = 10, n_days = 30,
                         path = "env_data.json", ot_hours = 12,
                         warm_up_days = 0) {
  json_data <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  resolved  <- resolve_scenario(json_data, scenario)

  env_data <<- build_environment(resolved)
  day_min  <<- 1440L
  counts   <<- sapply(env_data$elms, length)

  label <- if (!is.null(resolved$active_scenario_label)) {
    resolved$active_scenario_label
  } else {
    "Default (base configuration)"
  }

  message(sprintf("Running scenario '%s' (%s): %d replications x %d days",
                  scenario, label, n_iterations, n_days))

  mon <- run_replications(n_iterations, n_days, ot_hours = ot_hours)

  list(
    scenario     = scenario,
    label        = label,
    n_iterations = n_iterations,
    n_days       = n_days,
    mon          = mon,
    queue_kpi    = summarise_replications(mon, warm_up_days = warm_up_days),
    totals       = summarise_scenario_totals(mon, warm_up_days = warm_up_days)
  )
}

# ── Multi-scenario comparison ─────────────────────────────────────────────────

#' Classify individual bed/transport resource IDs into a comparison group
#'
#' @param resource Character vector of resource IDs as they appear in
#'   mon$resources / summarise_replications() output
#' @return Character vector of group labels ("R2B OT", "R2E OT", "R2E ICU",
#'   "Transport"), or NA for resources outside the comparison scope
#'
#' @details Mirrors the resource-matching patterns used by extract_kpis()
#'   (R/sensitivity.R) so scenario-level and Morris-screening KPI groupings
#'   stay consistent.
classify_resource_group <- function(resource) {
  dplyr::case_when(
    grepl("^b_r2b_ot_",         resource) ~ "R2B OT",
    grepl("^b_r2eheavy_ot_",    resource) ~ "R2E OT",
    grepl("^b_r2eheavy_icu_",   resource) ~ "R2E ICU",
    grepl("^t_(PMVAmb|HX240M)_", resource) ~ "Transport",
    TRUE ~ NA_character_
  )
}

#' Faceted bar chart comparing mean queue length by resource group and scenario
#'
#' @param queue_table Combined per-resource queue KPI table, as produced by
#'   compare_scenarios() (columns include scenario_label, resource, mean_q,
#'   p10_q, p90_q)
#' @param images_dir Directory for the saved plot (default "images")
#' @return ggplot object (also saved to images_dir/scenario_comparison.png)
#'
#' @details Per-resource p10_q/p90_q are averaged across resources within a
#'   group (consistent with extract_kpis()'s mean-of-resource-means
#'   convention) rather than recomputed from pooled replication data.
plot_scenario_comparison <- function(queue_table, images_dir = "images") {
  group_summary <- queue_table %>%
    mutate(
      group      = classify_resource_group(resource),
      short_label = sub(" — .*", "", scenario_label)
    ) %>%
    filter(!is.na(group)) %>%
    group_by(short_label, group) %>%
    summarise(
      mean_q = mean(mean_q, na.rm = TRUE),
      p10_q  = mean(p10_q,  na.rm = TRUE),
      p90_q  = mean(p90_q,  na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot(group_summary, aes(x = short_label, y = mean_q, fill = short_label)) +
    geom_col() +
    geom_errorbar(aes(ymin = p10_q, ymax = p90_q), width = 0.2) +
    facet_wrap(~ group, scales = "free_y") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Comparative Scenario Analysis — Mean Queue by Resource Group",
         subtitle = "Error bars show mean of per-resource p10-p90 across replications",
         x = NULL, y = "Mean Queue Length", fill = "Scenario") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none",
          strip.text      = element_text(face = "bold"))

  ggsave(file.path(images_dir, "scenario_comparison.png"), p, width = 10, height = 7, dpi = 150)
  p
}

#' Run and compare the multi-replication framework across named scenarios
#'
#' @param scenarios Character vector of scenario names to compare (default
#'   c("moderate_intensity", "high_intensity") — the two profiles defined in
#'   env_data.json by Issue #54; "default" can also be included)
#' @param n_iterations Replications per scenario (default 10)
#' @param n_days Simulation duration per replication (default 30)
#' @param path File path to env_data.json (default "env_data.json")
#' @param ot_hours Hours per day the first OT shift is active (default 12)
#' @param warm_up_days Days to exclude from the start of each replication
#'   (default 0)
#' @param output_dir Directory for CSV outputs (default "outputs")
#' @param images_dir Directory for the saved comparison plot (default "images")
#' @return Named list: results (one run_scenario() output per scenario,
#'   named by scenario), queue_table (combined per-resource KPI table with
#'   scenario/scenario_label columns), totals_table (combined casualty/DOW
#'   totals table with scenario/scenario_label columns), plot (ggplot object)
#'
#' @details Writes outputs/scenario_comparison_queues.csv,
#'   outputs/scenario_comparison_totals.csv, and images/scenario_comparison.png.
#'   Each scenario is executed via run_scenario(), which sets env_data
#'   globally per scenario in turn — scenarios are run sequentially, not
#'   nested in parallel, since run_replications() already parallelises
#'   across replications within a scenario via mclapply.
compare_scenarios <- function(scenarios = c("moderate_intensity", "high_intensity"),
                              n_iterations = 10, n_days = 30,
                              path = "env_data.json", ot_hours = 12,
                              warm_up_days = 0,
                              output_dir = "outputs", images_dir = "images") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(images_dir,  showWarnings = FALSE, recursive = TRUE)

  message(sprintf("Comparing scenarios: %s (%d reps x %d days each)",
                  paste(scenarios, collapse = ", "), n_iterations, n_days))

  results <- setNames(
    lapply(scenarios, function(s) {
      run_scenario(s, n_iterations = n_iterations, n_days = n_days, path = path,
                  ot_hours = ot_hours, warm_up_days = warm_up_days)
    }),
    scenarios
  )

  queue_table <- bind_rows(lapply(results, function(r) {
    cbind(scenario = r$scenario, scenario_label = r$label, r$queue_kpi)
  }))

  totals_table <- bind_rows(lapply(results, function(r) {
    cbind(scenario = r$scenario, scenario_label = r$label, r$totals)
  }))

  write.csv(queue_table,  file.path(output_dir, "scenario_comparison_queues.csv"),  row.names = FALSE)
  write.csv(totals_table, file.path(output_dir, "scenario_comparison_totals.csv"), row.names = FALSE)
  message(sprintf("Comparative scenario tables written to %s/", output_dir))

  comparison_plot <- plot_scenario_comparison(queue_table, images_dir = images_dir)

  list(
    results      = results,
    queue_table  = queue_table,
    totals_table = totals_table,
    plot         = comparison_plot
  )
}
