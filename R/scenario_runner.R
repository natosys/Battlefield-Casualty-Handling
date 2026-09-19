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

source("R/constants.R")
source("R/queue_series.R")

# ── The published comparison's protocol ───────────────────────────────────────
# The design docs/Multi_Run_Supplement.md documents for the comparative
# scenario analysis, held here so that the entry point's baseline refresh, the
# supplement's marker comments and scripts/check_scenario_protocol.R all read
# one definition rather than three copies of it.

#' Replications the comparative scenario analysis runs at, per profile
#'
#' @details Fifty, the count `docs/Multi_Run_Supplement.md` derives for a
#'   time-weighted queue or casualty-count response, and the count every other
#'   replicated experiment in the companion paper uses.
SCENARIO_REPLICATIONS <- 50L

#' Campaign length the comparison runs over, in days
SCENARIO_DAYS <- 30L

#' Control seed the comparison runs under
#'
#' @details Set once before each profile rather than once for the pair, so
#'   replication `i` of one intensity draws the same per-replication seed as
#'   replication `i` of the other.
SCENARIO_SEED <- 42L

#' Scenario profiles the published comparison covers, in the order it prints them
#'
#' @details The two casualty intensities defined in the `scenarios` block of
#'   `env_data.json`. The base configuration is not a third arm of this
#'   experiment; `moderate_intensity` is the shipped campaign the rest of the
#'   paper measures.
SCENARIO_PROTOCOL_PROFILES <- c("moderate_intensity", "high_intensity")

#' Casualty-total metrics the published totals table prints, in its row order
SCENARIO_TOTAL_METRICS <- c("total_casualties", "wia_count", "dow_count", "dow_rate")

#' Resource groups the published queue table prints, in its row order
#'
#' @details The groups `classify_queue_group()` assigns every monitored
#'   resource to. A group absent from the tracked queue table would make the
#'   published row unverifiable rather than merely unchecked, so the protocol
#'   check asserts the set rather than inferring it from the data.
SCENARIO_QUEUE_GROUPS <- c("R2B OT", "R2B Hold", "R2E OT", "R2E ICU",
                           "R2E Hold", "Transport")

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
  warm_up_min <- as.integer(warm_up_days) * DAY_MIN

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

  #' Mean, deciles and 95% confidence interval of one metric
  #'
  #' @param x Numeric vector of the metric's per-replication values.
  #' @return A one-row data frame of n_reps, mean, p10, p90, ci_lower and
  #'   ci_upper.
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
#' @param ot_hours Hours per day the first OT shift is active. NULL (the
#'   default) uses the value configured in env_data.json; threaded to
#'   run_replications() -> build_env()
#' @param warm_up_days Days to exclude from the start of each replication
#'   (default 0 = no exclusion)
#' @return Named list: scenario (name), label (scenario's `label` field, or
#'   "Default (base configuration)"), n_iterations (requested), n_replications
#'   (realised, and the count any label should name), n_days, mon (raw
#'   monitoring data), queue_kpi (summarise_replications() output),
#'   totals (summarise_scenario_totals() output)
#'
#' @details Sets env_data, day_min, and counts globally (<<-), consistent
#'   with run.R and scripts/run_sensitivity.R, since run_once()/build_env()
#'   resolve these from the global environment; they are restored to their
#'   pre-call state on exit, on the error path as well as the success path.
#'   Parses env_data.json once and resolves the scenario via
#'   resolve_scenario() + build_environment()
#'   directly (rather than calling load_scenario() a second time) purely to
#'   recover the scenario's `label` field for reporting.
run_scenario <- function(scenario, n_iterations = 10, n_days = 30,
                         path = "env_data.json", ot_hours = NULL,
                         warm_up_days = 0) {
  # The scenario's configuration is the caller's for the duration of this
  # call and no longer: everything derived from it (mon, queue_kpi, totals)
  # is computed before the function returns, so leaving the session on the
  # last scenario run would only mislabel whatever the caller runs next
  # (Issue #236). A caller that never set the globals gets them removed
  # again rather than left behind.
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  json_data <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  resolved  <- resolve_scenario(json_data, scenario)

  env_data <<- build_environment(resolved)
  day_min  <<- DAY_MIN
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
    # Both counts, because they answer different questions: n_iterations is
    # what the design asked for and belongs in a description of the experiment,
    # n_replications is what contributed and belongs anywhere a result is
    # labelled with its sample size (Issue #320).
    n_replications = mon$n_replications,
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

#' Classify a resource ID into one of the published queue-comparison groups
#'
#' @param resource Character vector of resource IDs as they appear in
#'   mon$resources.
#' @return Character vector of group labels drawn from SCENARIO_QUEUE_GROUPS,
#'   or NA for a resource outside the comparison's scope.
#'
#' @details Distinct from classify_resource_group() above, which drives the
#'   four-panel comparison figure and mirrors the Morris screening's KPI
#'   grouping. The published queue table covers six groups rather than four,
#'   the two holding pools included, so the two classifications are kept apart
#'   rather than one being widened and silently changing the figure.
classify_queue_group <- function(resource) {
  dplyr::case_when(
    grepl("^b_r2b_ot_",          resource) ~ "R2B OT",
    grepl("^b_r2b_hold_",        resource) ~ "R2B Hold",
    grepl("^b_r2eheavy_ot_",     resource) ~ "R2E OT",
    grepl("^b_r2eheavy_icu_",    resource) ~ "R2E ICU",
    grepl("^b_r2eheavy_hold_",   resource) ~ "R2E Hold",
    grepl("^t_(PMVAmb|HX240M)_", resource) ~ "Transport",
    TRUE ~ NA_character_
  )
}

#' Mean queue of each published resource group, per replication
#'
#' @param mon Named list with a `resources` monitor as returned by
#'   run_replications().
#' @param n_days Campaign length in days, which bounds the averaging window.
#' @return Data frame of replication, group and mean_q: one row per group per
#'   replication.
#'
#' @details A pool's total queue is in none of the monitor's rows, each bed
#'   being monitored separately, so it is recovered by pool_queue_steps()
#'   (`R/queue_series.R`) and averaged over the campaign by step_bin_means()
#'   with a single bin. That is the estimator the campaign time series uses, so
#'   the published queue table and the queue-over-time figure measure one
#'   quantity rather than two that happen to agree. A group whose beds never
#'   queued contributes a zero rather than dropping out, so the table describes
#'   the full establishment rather than only its busy parts.
scenario_queue_groups_by_replication <- function(mon, n_days) {
  horizon <- n_days * DAY_MIN
  rows <- mon$resources %>%
    mutate(group = classify_queue_group(resource)) %>%
    filter(!is.na(group), time <= horizon)

  keys <- unique(rows[, c("replication", "group")])
  keys <- keys[order(keys$replication, keys$group), ]

  bind_rows(lapply(seq_len(nrow(keys)), function(i) {
    sub <- rows[rows$replication == keys$replication[i] & rows$group == keys$group[i], ]
    steps <- pool_queue_steps(sub$resource, sub$time, sub$queue)
    data.frame(
      replication = keys$replication[i],
      group       = keys$group[i],
      mean_q      = step_bin_means(steps, c(0, horizon))
    )
  }))
}

#' Summarise each published resource group's queue across replications
#'
#' @param per_replication Data frame as returned by
#'   scenario_queue_groups_by_replication().
#' @return Data frame of group, n_reps, mean_q, p10_q, p90_q, ci_lower and
#'   ci_upper, in SCENARIO_QUEUE_GROUPS order.
#'
#' @details The interval is the Student t one at 95%, matching every other
#'   interval this project publishes (`docs/Multi_Run_Supplement.md`, Interval
#'   Construction). A group measured in one replication alone carries an NA
#'   interval rather than a zero-width one.
summarise_scenario_queue_groups <- function(per_replication) {
  groups <- SCENARIO_QUEUE_GROUPS[SCENARIO_QUEUE_GROUPS %in% per_replication$group]
  bind_rows(lapply(groups, function(g) {
    x <- per_replication$mean_q[per_replication$group == g]
    n <- length(x)
    m <- mean(x)
    half <- if (n > 1) qt(0.975, df = n - 1) * sd(x) / sqrt(n) else NA_real_
    data.frame(
      group    = g,
      n_reps   = n,
      mean_q   = m,
      p10_q    = as.numeric(quantile(x, 0.10, na.rm = TRUE)),
      p90_q    = as.numeric(quantile(x, 0.90, na.rm = TRUE)),
      ci_lower = m - half,
      ci_upper = m + half
    )
  }))
}

#' Short display label for a scenario, derived from its identifier
#'
#' @param scenario Character vector of scenario identifiers as they appear in
#'   the `scenarios` block of env_data.json (e.g. "moderate_intensity"), or
#'   "default"
#' @return Character vector of title-cased labels with underscores replaced by
#'   spaces (e.g. "Moderate Intensity")
#'
#' @details The identifier is used in preference to trimming the scenario's
#'   long `label` field, which cannot be done portably. A label read from
#'   env_data.json is flagged UTF-8 by jsonlite::fromJSON(), whereas a pattern
#'   written as an R source literal carries the session's native encoding, so
#'   under a C locale R cannot translate a non-ASCII pattern for matching and
#'   sub() rejects it outright (Issue #153). Identifiers are ASCII, so this
#'   derivation needs no such match and behaves identically in every locale.
scenario_short_label <- function(scenario) {
  vapply(strsplit(gsub("_", " ", scenario), " ", fixed = TRUE), function(words) {
    paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "", collapse = " ")
  }, character(1))
}

#' Faceted bar chart comparing mean queue length by resource group and scenario
#'
#' @param queue_table Combined per-resource queue KPI table, as produced by
#'   compare_scenarios() (columns include scenario, resource, mean_q,
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
      short_label = scenario_short_label(scenario)
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
    # The em dash is written as a \u escape, not a literal: an escape yields a
    # UTF-8-flagged string in any locale, whereas a literal carries the
    # session's native encoding and the PNG device renders it as raw bytes
    # under a C locale (Issue #153)
    labs(title = "Comparative Scenario Analysis \u2014 Mean Queue by Resource Group",
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
#' @param ot_hours Hours per day the first OT shift is active. NULL (the
#'   default) uses the value configured in env_data.json
#' @param warm_up_days Days to exclude from the start of each replication
#'   (default 0)
#' @param output_dir Directory for CSV outputs (default "outputs")
#' @param images_dir Directory for the saved comparison plot (default "images")
#' @return Named list: results (one run_scenario() output per scenario,
#'   named by scenario), queue_table (combined per-resource KPI table with
#'   scenario/scenario_label columns), totals_table (combined casualty/DOW
#'   totals table with scenario/scenario_label columns), queue_group_table
#'   (the published per-pool queue comparison), queue_group_reps (its
#'   per-replication values), plot (ggplot object)
#'
#' @details Writes four CSVs under output_dir, scenario_comparison_queues.csv,
#'   scenario_comparison_totals.csv, scenario_queue_group_replications.csv and
#'   scenario_queue_groups.csv, and scenario_comparison.png under images_dir.
#'   Each scenario is executed via run_scenario(), which sets env_data
#'   globally per scenario in turn — scenarios are run sequentially, not
#'   nested in parallel, since run_replications() already parallelises
#'   across replications within a scenario via mclapply.
compare_scenarios <- function(scenarios = c("moderate_intensity", "high_intensity"),
                              n_iterations = 10, n_days = 30,
                              path = "env_data.json", ot_hours = NULL,
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

  # The published queue table reports pools rather than individual beds, and a
  # pool's interval cannot be recovered from per-bed summaries, so the
  # per-replication group series is carried alongside them rather than
  # reconstructed later from a table that no longer holds it.
  queue_group_replications <- bind_rows(lapply(results, function(r) {
    cbind(scenario = r$scenario,
          scenario_queue_groups_by_replication(r$mon, r$n_days))
  }))

  queue_group_table <- bind_rows(lapply(results, function(r) {
    per_rep <- queue_group_replications[queue_group_replications$scenario == r$scenario, ]
    cbind(scenario = r$scenario, scenario_label = r$label,
          summarise_scenario_queue_groups(per_rep))
  }))

  write.csv(queue_table,  file.path(output_dir, "scenario_comparison_queues.csv"),  row.names = FALSE)
  write.csv(totals_table, file.path(output_dir, "scenario_comparison_totals.csv"), row.names = FALSE)
  write.csv(queue_group_replications,
            file.path(output_dir, "scenario_queue_group_replications.csv"), row.names = FALSE)
  write.csv(queue_group_table,
            file.path(output_dir, "scenario_queue_groups.csv"), row.names = FALSE)
  message(sprintf("Comparative scenario tables written to %s/", output_dir))

  comparison_plot <- plot_scenario_comparison(queue_table, images_dir = images_dir)

  list(
    results            = results,
    queue_table        = queue_table,
    totals_table       = totals_table,
    queue_group_table  = queue_group_table,
    queue_group_reps   = queue_group_replications,
    plot               = comparison_plot
  )
}
