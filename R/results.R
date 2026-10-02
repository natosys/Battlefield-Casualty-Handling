##############################################################################
## R/results.R                                                              ##
## Result tables generated from the tracked evidence sets                   ##
##############################################################################
#
# docs/Results.md reports what the model measured and nothing else, so every
# number in it must come from a tracked file rather than from a sentence
# somebody typed. This module is how that is guaranteed. A generated span in
# the document is a pair of HTML comments around its content,
#
#   <!-- GEN name -->
#   ...table or value...
#   <!-- /GEN -->
#
# and `render_results()` replaces the content of every span with what the
# builder registered under `name` produces from the tracked data. Two kinds of
# name exist: a table name, whose builder returns the lines of a markdown
# table, and a cell reference of the form `cell:<table>|<row>|<column>|<part>`,
# which reads one cell back out of a generated table so that a figure quoted in
# a sentence is always a copy of the table cell beside it. Base R only, so a
# regression check can source it without loading simmer or running the model.

#' Directory the tracked evidence sets are read from
RESULTS_DATA_DIR <- "data"

#' Opening and closing marker of a generated span
#'
#' @details A span is replaced wholesale on render, so nothing a person types
#'   between the two markers survives; that is the point of the markers.
RESULTS_SPAN_PATTERN <- "<!-- GEN ([^ ]+) -->(.*?)<!-- /GEN -->"

#' Format a number with a fixed number of decimals
#'
#' @param x A numeric vector.
#' @param dp Decimal places.
#' @param big Whether to group thousands with a comma.
#' @param plus Whether to print a leading plus sign on a non-negative value.
#' @return A character vector, with a true minus sign (U+2212) for negatives.
res_num <- function(x, dp = 2L, big = FALSE, plus = FALSE) {
  out <- formatC(x, format = "f", digits = dp, big.mark = if (big) "," else "")
  out <- sub("^-", "−", out)
  if (plus) out <- ifelse(x >= 0, paste0("+", out), out)
  out
}

#' Format a mean with its 95% interval as the paper prints it
#'
#' @param m,lo,hi Mean and interval bounds.
#' @param dp Decimal places.
#' @param scale Multiplier applied to all three, 100 for a percentage.
#' @param big Whether to group thousands.
#' @param floor0 Whether to clamp the lower bound at zero.
#' @param unit Suffix placed after each figure, such as `%`.
#' @return The cell text `m [lo, hi]`.
res_ci <- function(m, lo, hi, dp = 2L, scale = 1, big = FALSE, floor0 = FALSE, unit = "") {
  lo <- lo * scale
  if (floor0) lo <- max(lo, 0)
  sprintf("%s%s [%s%s, %s%s]", res_num(m * scale, dp, big), unit, res_num(lo, dp, big), unit,
          res_num(hi * scale, dp, big), unit)
}

#' Read one tracked evidence file
#'
#' @param path Path under the data directory.
#' @param data_dir The data directory.
#' @return A data frame.
res_read <- function(path, data_dir = RESULTS_DATA_DIR) {
  full <- file.path(data_dir, path)
  if (!file.exists(full)) stop(sprintf("evidence file %s does not exist", full), call. = FALSE)
  read.csv(full, stringsAsFactors = FALSE)
}

#' One row of a long-format summary
#'
#' @param d A summary with a `response` column.
#' @param response Response key.
#' @param ... Named column values selecting the row, such as `policy_days = 21`.
#' @return The single matching row.
res_row <- function(d, response, ...) {
  keep <- d$response == response
  for (nm in names(list(...))) {
    v <- list(...)[[nm]]
    keep <- keep & !is.na(d[[nm]]) & abs(d[[nm]] - v) < 1e-9
  }
  hit <- d[keep, ]
  if (nrow(hit) != 1L) {
    stop(sprintf("expected one '%s' row for %s, found %d", response,
                 paste(names(list(...)), unlist(list(...)), sep = "=", collapse = ", "),
                 nrow(hit)), call. = FALSE)
  }
  hit
}

#' Build a markdown table from a header and rows of cells
#'
#' @param header Character vector of column headings.
#' @param rows A list of character vectors, one per row.
#' @param rule The rule row style, `"short"` for `|---|` or `"spaced"` for `| --- |`.
#' @return The table as a character vector of lines.
res_table <- function(header, rows, rule = "short") {
  bar <- if (rule == "short") rep("---", length(header)) else rep(" --- ", length(header))
  c(paste0("| ", paste(header, collapse = " | "), " |"),
    paste0("|", paste(bar, collapse = "|"), "|"),
    vapply(rows, function(r) paste0("| ", paste(r, collapse = " | "), " |"), character(1)))
}

#' Build a table whose columns are the arms of one sweep
#'
#' @param d A long-format summary with `response`, `mean`, `ci_lower` and `ci_upper`.
#' @param header Column headings, first the row label heading.
#' @param arms List of named lists, each the column values selecting one arm.
#' @param rows List of row specifications, each `list(label, response, dp, scale)`.
#' @return The table lines.
res_sweep_table <- function(d, header, arms, rows) {
  body <- lapply(rows, function(r) {
    cells <- vapply(arms, function(a) {
      x <- do.call(res_row, c(list(d, r[[2]]), a))
      res_ci(x$mean, x$ci_lower, x$ci_upper, dp = r[[3]], scale = r[[4]], floor0 = TRUE)
    }, character(1))
    c(r[[1]], cells)
  })
  res_table(header, body)
}

# ── Table builders ───────────────────────────────────────────────────────────
# Each builder takes the data directory and returns the lines of one table. The
# formats are those the paper's tables have always printed, which the protocol
# checks parse, so a builder and its check agree by construction.

#' The two scenario profiles the comparative tables compare
SCENARIO_PROFILES <- c("moderate_intensity", "high_intensity")

#' Comparative scenario totals table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_scenario_totals <- function(data_dir) {
  d <- res_read("scenarios/scenario_comparison_totals.csv", data_dir)
  pick <- function(metric, prof) d[d$metric == metric & d$scenario == prof, ]
  spec <- list(
    list("Total casualties/run", "total_casualties", 1L, 1, FALSE, 2L),
    list("Wounded in action/run", "wia_count", 1L, 1, FALSE, 2L),
    list("Died of wounds/run", "dow_count", 2L, 1, FALSE, 1L),
    list("Died of wounds, as share of wounded", "dow_rate", 2L, 100, TRUE, 2L))
  rows <- lapply(spec, function(s) {
    cells <- vapply(SCENARIO_PROFILES, function(p) {
      x <- pick(s[[2]], p)
      base <- res_ci(x$mean, x$ci_lower, x$ci_upper, dp = s[[3]], scale = s[[4]], big = TRUE,
                     unit = if (s[[5]]) "%" else "")
      if (s[[5]]) return(base)
      f <- function(v) if (v == 0) "0" else res_num(v, 1L, big = TRUE)
      sprintf("%s (p10–p90: %s–%s)", base, f(x$p10), f(x$p90))
    }, character(1))
    a <- pick(s[[2]], SCENARIO_PROFILES[1])$mean
    b <- pick(s[[2]], SCENARIO_PROFILES[2])$mean
    c(s[[1]], cells, sprintf("%.*f×", s[[6]], b / a))
  })
  res_table(c("Metric", "Moderate intensity", "High intensity", "Ratio"), rows)
}

#' Comparative scenario queue table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_scenario_queue <- function(data_dir) {
  d <- res_read("scenarios/scenario_queue_groups.csv", data_dir)
  groups <- list(c("R2B operating theatre", "R2B OT"), c("R2B holding beds", "R2B Hold"),
                 c("R2E operating theatre", "R2E OT"), c("R2E intensive care", "R2E ICU"),
                 c("R2E holding beds", "R2E Hold"), c("Ambulance and truck fleets", "Transport"))
  rows <- lapply(groups, function(g) {
    x <- lapply(SCENARIO_PROFILES, function(p) d[d$group == g[2] & d$scenario == p, ])
    cells <- vapply(x, function(r) {
      sprintf("%s [%s, %s]", res_num(r$mean_q, 3L, big = TRUE),
              res_num(r$ci_lower, 3L, big = TRUE), res_num(r$ci_upper, 3L, big = TRUE))
    }, character(1))
    ratio <- if (x[[1]]$mean_q == 0) "not applicable" else
      sprintf("%.2f×", x[[2]]$mean_q / x[[1]]$mean_q)
    c(g[1], cells, ratio)
  })
  res_table(c("Resource group", "Moderate intensity mean queue", "High intensity mean queue",
              "Ratio"), rows)
}

#' Sustained-horizon stability classification table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_long_horizon_stability <- function(data_dir) {
  d <- res_read("long_horizon/long_horizon_stability.csv", data_dir)
  level <- function(x) {
    if (abs(x) < 10) formatC(x, format = "f", digits = 2) else
      formatC(x, format = "f", digits = 1, big.mark = ",")
  }
  cell <- function(r) {
    if (r$stability == "drifting") {
      sprintf("**drifting, %+.1f%%/block**, %s to %s", 100 * r$drift_per_block,
              formatC(r$first, format = "f", digits = 1, big.mark = ","),
              formatC(r$last, format = "f", digits = 1, big.mark = ","))
    } else {
      sprintf("converged at %s", level(r$late_mean))
    }
  }
  spec <- list(
    list("R2E operating theatre queue", "mean_queue", "R2E operating theatres"),
    list("R2E holding bed queue", "mean_queue", "R2E holding beds"),
    list("R2E intensive care queue", "mean_queue", "R2E intensive care"),
    list("Strategic evacuation backlog", "evac_backlog", "system"),
    list("R2B holding bed queue", "mean_queue", "R2B holding beds"),
    list("Casualty arrivals per day", "arrivals", "system"),
    list("Deaths of wounds per day", "dow", "system"))
  rows <- lapply(spec, function(s) {
    c(s[[1]], vapply(SCENARIO_PROFILES, function(p) {
      cell(d[d$scenario == p & d$series == s[[2]] & d$subject == s[[3]], ])
    }, character(1)))
  })
  res_table(c("Response", "Moderate intensity", "High intensity"), rows)
}

#' The two R2B holding sweep tables
#'
#' @param data_dir The data directory.
#' @param axis `"beds"` for the establishment axis, `"threshold"` for the threshold axis.
#' @return The table lines.
build_hold_threshold <- function(data_dir, axis) {
  d <- res_read("sweeps/r2b_hold_threshold_sweep.csv", data_dir)
  pick <- function(beds, days) d[d$hold_beds == beds & d$evac_threshold_days == days, ]
  row <- function(label, x) {
    c(label, res_ci(x$mean_r2b_hold_q, x$ci_lower_r2b_hold_q, x$ci_upper_r2b_hold_q, 2L,
                    floor0 = TRUE),
      sprintf("%.1f%% [%.1f, %.1f]", 100 * x$mean_r2b_hold_util, 100 * x$ci_lower_r2b_hold_util,
              100 * x$ci_upper_r2b_hold_util),
      res_ci(x$mean_r2e_hold_q, x$ci_lower_r2e_hold_q, x$ci_upper_r2e_hold_q, 3L, floor0 = TRUE),
      res_ci(x$mean_r2e_icu_q, x$ci_lower_r2e_icu_q, x$ci_upper_r2e_icu_q, 3L, floor0 = TRUE))
  }
  tail_header <- c("R2B hold mean queue", "R2B hold utilisation", "R2E hold mean queue",
                   "R2E ICU mean queue")
  if (axis == "beds") {
    rows <- lapply(c(5, 7, 10), function(b) {
      row(if (b == 5) "5 (shipped)" else as.character(b), pick(b, 0))
    })
    res_table(c("R2B holding beds per unit", tail_header), rows)
  } else {
    labels <- c("Disabled (shipped)", "1 day", "3 days", "5 days (mode)", "7 days")
    rows <- Map(function(days, label) row(label, pick(5, days)), c(0, 1, 3, 5, 7), labels)
    res_table(c("Evacuation threshold", tail_header), rows)
  }
}

#' R2B pre-open hold window table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_hold_window <- function(data_dir) {
  s <- res_read("hold_window/hold_window_summary.csv", data_dir)
  p <- res_read("hold_window/hold_window_paired.csv", data_dir)
  spec <- list(c("Casualties held at R2B", "held_r2b"), c("R2B surgeries", "r2b_surgeries"),
               c("Diverted, team off shift", "diverted_offshift"),
               c("Diverted, theatre busy", "diverted_busy"),
               c("R2E first surgeries", "r2e_first_surgeries"),
               c("R2E theatre entry deferred", "r2e_theatre_deferred"),
               c("Died of wounds per run", "total_dow"), c("Total casualties", "total_casualties"))
  rows <- lapply(spec, function(r) {
    a <- s[s$response == r[2] & s$window_min == 0, "mean"]
    b <- s[s$response == r[2] & s$window_min == 60, "mean"]
    d <- p[p$response == r[2], ]
    c(r[1], res_num(a, 2L), res_num(b, 2L),
      sprintf("%s [%s, %s]", res_num(d$difference, 2L, plus = TRUE),
              res_num(d$ci_lower, 2L, plus = TRUE), res_num(d$ci_upper, 2L, plus = TRUE)))
  })
  res_table(c("Measure", "Window 0", "Window 60 min", "Difference"), rows, rule = "spaced")
}

#' Transport fleet-size sweep tables
#'
#' @param data_dir The data directory.
#' @param high Whether to build the high-intensity table.
#' @return The table lines.
build_transport <- function(data_dir, high = FALSE) {
  d <- res_read(if (high) "sweeps/transport_capacity_by_fleet_size_high_intensity.csv" else
    "sweeps/transport_capacity_by_fleet_size.csv", data_dir)
  fmt <- function(x) {
    sprintf("%.4f [%.4f, %.4f]", x$mean_q, max(x$ci_lower_q, 0), x$ci_upper_q)
  }
  rows <- lapply(1:5, function(q) {
    a <- d[d$vehicle == "PMVAmb" & d$qty == q, ]
    t <- d[d$vehicle == "HX240M" & d$qty == q, ]
    label <- if (q == 3) "3 (current ambulance)" else if (q == 4) "4 (current truck)" else
      as.character(q)
    c(label, fmt(a), if (nrow(t)) fmt(t) else "not swept")
  })
  res_table(c("Fleet size", "Ambulance mean queue", "Truck mean queue"), rows)
}

#' Forward intensive care share frontier table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_icu_share <- function(data_dir) {
  d <- res_read("sweeps/r2b_icu_share_frontier.csv", data_dir)
  rows <- lapply(seq_len(nrow(d)), function(i) {
    x <- d[i, ]
    c(if (x$share == 0) "0% (current)" else paste0(round(x$share * 100), "%"),
      res_ci(x$mean_r2e_icu_q, x$ci_lower_r2e_icu_q, x$ci_upper_r2e_icu_q, 3L, floor0 = TRUE),
      sprintf("%.1f%%", 100 * x$mean_r2b_icu_util), sprintf("%.1f%%", 100 * x$mean_r2e_icu_util),
      sprintf("%.1f [%.1f, %.1f]", 100 * x$mean_pd_icu_share, 100 * x$ci_lower_pd_icu_share,
              100 * x$ci_upper_pd_icu_share),
      res_ci(x$mean_dow, x$ci_lower_dow, x$ci_upper_dow, 2L))
  })
  res_table(c("Forward share", "R2E ICU mean queue", "R2B ICU utilisation", "R2E ICU utilisation",
              "Post-definitive care in ICU", "Died of wounds per run"), rows)
}

#' Evacuation policy sweep table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_policy <- function(data_dir) {
  d <- res_read("policy/policy_sweep.csv", data_dir)
  arms <- lapply(c(15, 21, 30, 45, 60), function(p) list(policy_days = p))
  rows <- list(
    list("R2E hold occupancy (%)", "hold_occupancy", 1L, 100),
    list("R2E hold mean queue", "hold_mean_queue", 2L, 1),
    list("R2E ICU occupancy (%)", "icu_occupancy", 1L, 100),
    list("R2E ICU mean queue", "icu_mean_queue", 2L, 1),
    list("Post-definitive ICU access (%)", "post_definitive_icu_share", 1L, 100),
    list("In-theatre share (%)", "in_theatre_share", 1L, 100),
    list("Returns to duty", "total_rtd", 1L, 1),
    list("Died of wounds", "total_dow", 2L, 1),
    list("Never evacuated by horizon", "never_evacuated", 1L, 1),
    list("Mean evacuation wait (d)", "mean_evac_wait_days", 2L, 1),
    list("Role 4 peak beds", "role4_peak", 1L, 1))
  res_sweep_table(d, c("Response", "15 d", "21 d (shipped)", "30 d", "45 d", "60 d"), arms, rows)
}

#' R2E holding establishment sweep table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_establishment <- function(data_dir) {
  d <- res_read("policy/establishment_sweep.csv", data_dir)
  arms <- lapply(c(30, 45, 60, 90), function(b) list(hold_beds = b))
  rows <- list(
    list("R2E hold occupancy (%)", "hold_occupancy", 1L, 100),
    list("R2E hold mean queue", "hold_mean_queue", 2L, 1),
    list("R2E ICU mean queue", "icu_mean_queue", 2L, 1),
    list("Post-definitive ICU access (%)", "post_definitive_icu_share", 1L, 100),
    list("In-theatre share (%)", "in_theatre_share", 1L, 100),
    list("Returns to duty", "total_rtd", 1L, 1),
    list("Died of wounds", "total_dow", 1L, 1),
    list("Never evacuated by horizon", "never_evacuated", 1L, 1),
    list("Role 4 peak beds", "role4_peak", 1L, 1))
  res_sweep_table(d, c("Response", "30 beds (shipped)", "45 beds", "60 beds", "90 beds"), arms, rows)
}

#' Forward surgical saturation release sweep table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_saturation <- function(data_dir) {
  d <- res_read("policy/saturation_sweep.csv", data_dir)
  thresholds <- c(0, 1, 2, 3, 5, 8, 12, 16, 24)
  arms <- lapply(thresholds, function(t) list(saturation_threshold = t))
  rows <- list(
    list("Theatre mean queue", "theatre_mean_queue", 2L, 1),
    list("Released with repair outstanding", "released_unrepaired", 1L, 1),
    list("Role 4 operations owed", "role4_operations", 1L, 1),
    list("Post-definitive ICU access (%)", "post_definitive_icu_share", 1L, 100),
    list("Died of wounds", "total_dow", 1L, 1),
    list("Returns to duty", "total_rtd", 1L, 1))
  header <- c("Response", "0 (disabled)", as.character(thresholds[2:5]), "8 (shipped)",
              as.character(thresholds[7:9]))
  res_sweep_table(d, header, arms, rows)
}

#' Mass casualty event stress test table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_mass_casualty <- function(data_dir) {
  cnt <- res_read("mass_casualty/mass_casualty_count_summary.csv", data_dir)
  dow <- res_read("mass_casualty/mass_casualty_dow_summary.csv", data_dir)
  rep <- res_read("mass_casualty/mass_casualty_replications.csv", data_dir)
  tot <- function(r) cnt[cnt$rate_per_day == r & cnt$response == "total_casualties", "mean"]
  ev <- cnt[cnt$rate_per_day == 0.2 & cnt$response == "n_events", "mean"]
  range_ev <- range(rep$n_events[rep$rate_per_day == 0.2])
  pct <- function(r, o) {
    x <- dow[abs(dow$rate_per_day - r) < 1e-9 & dow$origin == o, ]
    sprintf("%.2f%% [%.2f%%, %.2f%%]", 100 * x$rate, 100 * x$ci_lower, 100 * x$ci_upper)
  }
  rows <- list(
    c("Average total casualties/run", res_num(tot(0), 1L), res_num(tot(0.2), 1L)),
    c("Average events/run", "0", sprintf("%s (range %d–%d)", res_num(ev, 2L), range_ev[1],
                                         range_ev[2])),
    c("Died-of-wounds rate, ordinary casualties", pct(0, "ordinary"), pct(0.2, "ordinary")),
    c("Died-of-wounds rate, event casualties", "not applicable", pct(0.2, "event")))
  res_table(c("Metric", "No events injected", "Events injected"), rows)
}

#' Strategic evacuation tables
#'
#' @param data_dir The data directory.
#' @param which One of `"baseline"`, `"interval"` or `"reliability"`.
#' @return The table lines.
build_airlift <- function(data_dir, which) {
  d <- res_read("airlift/airlift_summary.csv", data_dir)
  g <- function(arm, sc, val, resp) {
    x <- d[d$arm == arm & d$scenario == sc & abs(d$value - val) < 1e-9 & d$response == resp, ]
    stopifnot(nrow(x) == 1L)
    x
  }
  f <- function(x, neg = FALSE, m = 1, dp = 2L, ci = TRUE, unit = "") {
    a <- x$mean * m; l <- x$ci_lower * m; u <- x$ci_upper * m
    if (neg) { a <- -a; t <- -u; u <- -l; l <- t }
    if (!ci) return(paste0(res_num(a, dp), unit))
    sprintf("%s%s [%s%s, %s%s]", res_num(a, dp), unit, res_num(l, dp), unit, res_num(u, dp), unit)
  }
  if (which == "baseline") {
    cols <- list(c("baseline", "moderate_intensity"), c("high", "high_intensity"))
    G <- function(c, resp) g(c[1], c[2], 0, resp)
    rows <- list(
      c("Casualties boarded", vapply(cols, function(c) f(G(c, "boarded")), "")),
      c("Still waiting at the close", vapply(cols, function(c) f(G(c, "queued_at_end")), "")),
      c("Mean wait (days)", vapply(cols, function(c) f(G(c, "mean_wait_days")), "")),
      c("Share of R2E holding beds held by the evacuation wait",
        vapply(cols, function(c) f(G(c, "hold_evac_share"), m = 100, dp = 0L, unit = "%"), "")),
      c("Role 4 peak occupancy (concurrent patients)",
        vapply(cols, function(c) f(G(c, "role4_peak")), "")),
      c("Days the peak falls before the campaign ends",
        vapply(cols, function(c) f(G(c, "role4_peak_after_end"), neg = TRUE), "")))
    return(res_table(c("Response at the shipped schedule", "Moderate intensity", "High intensity"),
                     rows))
  }
  if (which == "interval") {
    v <- c(3, 5, 7, 10, 14)
    H <- function(x, resp) g("interval", "moderate_intensity", x, resp)
    rows <- list(
      c("Sorties flown", vapply(v, function(x) f(H(x, "sorties_flown"), ci = FALSE), "")),
      c("Mean wait (days)", vapply(v, function(x) f(H(x, "mean_wait_days")), "")),
      c("Share of R2E holding beds held by the evacuation wait",
        vapply(v, function(x) f(H(x, "hold_evac_share"), m = 100, dp = 0L, unit = "%"), "")),
      c("Ventilated pre-flight intensive care hold (hours)",
        vapply(v, function(x) f(H(x, "ventilated_hold_hours")), "")))
    return(res_table(c("Response by interval between sorties", "3 days", "5 days",
                       "7 days (shipped)", "10 days", "14 days"), rows))
  }
  v <- c(0, 0.05, 0.10, 0.15, 0.25, 0.40)
  H <- function(x, resp) g("reliability", "moderate_intensity", x, resp)
  rows <- list(
    c("Sorties flown", vapply(v, function(x) f(H(x, "sorties_flown"), ci = FALSE), "")),
    c("Realised cancellation rate",
      vapply(v, function(x) f(H(x, "cancellation_rate"), m = 100, dp = 0L, ci = FALSE, unit = "%"),
             "")),
    c("Mean wait (days)", vapply(v, function(x) f(H(x, "mean_wait_days")), "")),
    c("Share of R2E holding beds held by the evacuation wait",
      vapply(v, function(x) f(H(x, "hold_evac_share"), m = 100, dp = 0L, unit = "%"), "")))
  res_table(c("Response by configured cancellation probability", "0%", "5%", "10%", "15%", "25%",
              "40%"), rows)
}

#' Registry of generated tables
#'
#' @details Each entry is a function of the data directory returning the lines
#'   of one table. The names are the ones a `GEN` marker in docs/Results.md
#'   uses.
RESULTS_TABLES <- list(
  scenario_totals = build_scenario_totals,
  scenario_queue = build_scenario_queue,
  long_horizon_stability = build_long_horizon_stability,
  hold_threshold_beds = function(dd) build_hold_threshold(dd, "beds"),
  hold_threshold_threshold = function(dd) build_hold_threshold(dd, "threshold"),
  hold_window = build_hold_window,
  transport = function(dd) build_transport(dd, FALSE),
  transport_high = function(dd) build_transport(dd, TRUE),
  icu_share = build_icu_share,
  policy = build_policy,
  establishment = build_establishment,
  saturation = build_saturation,
  mass_casualty = build_mass_casualty,
  airlift_baseline = function(dd) build_airlift(dd, "baseline"),
  airlift_interval = function(dd) build_airlift(dd, "interval"),
  airlift_reliability = function(dd) build_airlift(dd, "reliability")
)

#' Split a markdown table row into trimmed cells
#'
#' @param line One table row.
#' @return Character vector of cells.
res_cells <- function(line) {
  trimws(strsplit(sub("\\|\\s*$", "", sub("^\\|", "", line)), "|", fixed = TRUE)[[1]])
}

#' Resolve a cell reference against the generated tables
#'
#' @param ref The reference without the `cell:` prefix: `table|row|column|part`.
#' @param data_dir The data directory.
#' @return The cell text, or its leading number (`mean`) or interval (`ci`).
res_cell_value <- function(ref, data_dir) {
  p <- strsplit(ref, "|", fixed = TRUE)[[1]]
  if (length(p) != 4L) stop(sprintf("cell reference '%s' needs table|row|column|part", ref),
                            call. = FALSE)
  builder <- RESULTS_TABLES[[p[1]]]
  if (is.null(builder)) stop(sprintf("no table named '%s'", p[1]), call. = FALSE)
  lines <- builder(data_dir)
  head <- res_cells(lines[1])
  col <- match(p[3], head)
  if (is.na(col)) stop(sprintf("table '%s' has no column '%s'", p[1], p[3]), call. = FALSE)
  rows <- lapply(lines[-(1:2)], res_cells)
  hit <- Filter(function(r) identical(r[1], p[2]), rows)
  if (length(hit) != 1L) stop(sprintf("table '%s' has %d rows labelled '%s'", p[1], length(hit),
                                      p[2]), call. = FALSE)
  cell <- hit[[1]][col]
  switch(p[4],
         full = cell,
         mean = sub(" .*$", "", cell),
         ci = regmatches(cell, regexpr("\\[[^]]*\\]", cell)),
         stop(sprintf("cell part '%s' is not full, mean or ci", p[4]), call. = FALSE))
}

#' Content a generated span should hold
#'
#' @param name The span's name from its marker.
#' @param data_dir The data directory.
#' @return A character vector of lines for a table, or a single string for a cell.
res_span_content <- function(name, data_dir) {
  if (startsWith(name, "cell:")) return(res_cell_value(sub("^cell:", "", name), data_dir))
  builder <- RESULTS_TABLES[[name]]
  if (is.null(builder)) stop(sprintf("no generated table named '%s'", name), call. = FALSE)
  builder(data_dir)
}

#' Regenerate every generated span of a document
#'
#' @param text The document as one string.
#' @param data_dir The data directory.
#' @return The document with each span's content replaced.
#'
#' @details A table span puts its lines on their own lines between the markers
#'   and a cell span keeps its value inline, so a sentence reads normally in
#'   the source. Rendering twice gives the same text, which is what lets a
#'   check compare a document with its own re-rendering.
render_results <- function(text, data_dir = RESULTS_DATA_DIR) {
  m <- gregexpr(RESULTS_SPAN_PATTERN, text, perl = TRUE)[[1]]
  if (m[1] == -1L) return(text)
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  out <- character(0)
  pos <- 1L
  for (i in seq_along(starts)) {
    span <- substr(text, starts[i], starts[i] + lens[i] - 1L)
    name <- sub(RESULTS_SPAN_PATTERN, "\\1", span, perl = TRUE)
    content <- res_span_content(name, data_dir)
    block <- if (length(content) > 1L) {
      paste0("<!-- GEN ", name, " -->\n", paste(content, collapse = "\n"), "\n<!-- /GEN -->")
    } else {
      paste0("<!-- GEN ", name, " -->", content, "<!-- /GEN -->")
    }
    out <- c(out, substr(text, pos, starts[i] - 1L), block)
    pos <- starts[i] + lens[i]
  }
  paste0(paste(out, collapse = ""), substr(text, pos, nchar(text)))
}
