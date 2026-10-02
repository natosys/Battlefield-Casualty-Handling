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

#' Minutes per simulated day, from the single definition
if (!exists("DAY_MIN")) source(file.path("R", "constants.R"))

#' Directory the tracked evidence sets are read from
RESULTS_DATA_DIR <- "data"

#' Opening and closing marker of a generated span
#'
#' @details A span is replaced wholesale on render, so nothing a person types
#'   between the two markers survives; that is the point of the markers.
RESULTS_SPAN_PATTERN <- "(?s)<!-- GEN (.+?) -->(.*?)<!-- /GEN -->"

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

#' Queue clearance by resource pool at the two casualty intensities
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_queue_clearance <- function(data_dir) {
  d <- res_read("time_series/queue_clearance.csv", data_dir)
  pools <- c("R2B holding beds", "R2E operating theatres", "R2E intensive care", "R2E holding beds")
  cell <- function(pool, intensity) {
    x <- d[d$pool == pool & d$intensity == intensity, ]
    q <- quantile(x$longest_busy_days, c(0.25, 0.75))
    c(sprintf("%.0f%%", 100 * median(x$zero_share)),
      sprintf("%.1f (%.1f\u2013%.1f)", median(x$longest_busy_days), q[[1]], q[[2]]))
  }
  rows <- lapply(pools, function(p) {
    m <- cell(p, "Moderate intensity")
    h <- cell(p, "High intensity")
    c(p, m, h)
  })
  res_table(c("Resource pool", "Moderate: queue empty", "Moderate: longest run above zero (days)",
              "High: queue empty", "High: longest run above zero (days)"), rows)
}

#' Degraded post-operative care rate by stage and intensity
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_degraded_care <- function(data_dir) {
  d <- res_read("time_series/degraded_care_series.csv", data_dir)
  last <- max(d$day)
  first <- min(d$day)
  rows <- list()
  for (it in c("Moderate intensity", "High intensity")) {
    for (st in c("Stabilisation", "Post-definitive care")) {
      x <- d[d$intensity == it & d$stage == st, ]
      w <- function(a, b) {
        y <- x[x$day >= a & x$day <= b & !is.na(x$daily_rate), ]
        sum(y$daily_rate * y$n_decisions) / sum(y$n_decisions)
      }
      m <- tapply(x$daily_rate, x$day, median, na.rm = TRUE)
      hit <- as.integer(names(m))[which(m >= 0.999)[1]]
      cum <- median(x$cumulative_rate[x$day == last], na.rm = TRUE)
      rows[[length(rows) + 1L]] <- c(paste(it, st, sep = ": "),
        if (is.na(hit)) "not reached" else as.character(hit),
        sprintf("%.1f%%", 100 * w(first, first + 9)), sprintf("%.1f%%", 100 * w(last - 9, last)),
        sprintf("%.1f%%", 100 * cum))
    }
  }
  res_table(c("Intensity and stage", "First day the median daily rate reaches 100%",
              "First ten days", "Last ten days", "Whole campaign"), rows)
}

#' Post-operative intensive care gate comparison table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_icu_gate <- function(data_dir) {
  s <- res_read("icu_gate/icu_gate_summary.csv", data_dir)
  p <- res_read("icu_gate/icu_gate_paired.csv", data_dir)
  spec <- list(c("R2E ICU utilisation (%)", "icu_occupancy", 100, 1L),
               c("Died of wounds per run", "total_dow", 1, 2L),
               c("Total casualties", "total_casualties", 1, 1L))
  rows <- lapply(spec, function(r) {
    a <- s[s$response == r[2] & s$gate_enabled == 0, ]
    b <- s[s$response == r[2] & s$gate_enabled == 1, ]
    d <- p[p$response == r[2], ]
    sc <- as.numeric(r[3]); dp <- as.integer(r[4])
    c(r[1], res_ci(a$mean, a$ci_lower, a$ci_upper, dp, sc, big = TRUE),
      res_ci(b$mean, b$ci_lower, b$ci_upper, dp, sc, big = TRUE),
      sprintf("%s [%s, %s]", res_num(d$difference * sc, dp, TRUE, TRUE),
              res_num(d$ci_lower * sc, dp, TRUE, TRUE), res_num(d$ci_upper * sc, dp, TRUE, TRUE)))
  })
  res_table(c("Measure", "Without the rule", "With the rule", "Paired difference"), rows)
}

#' Strategic airlift collapse classification table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_airlift_collapse <- function(data_dir) {
  d <- res_read("airlift/airlift_collapse.csv", data_dir)
  rows <- lapply(seq_len(nrow(d)), function(i) {
    x <- d[i, ]
    c(sprintf("%.0f%%", 100 * x$probability), sprintf("%d of %d", x$n_collapsed, x$n_reps),
      sprintf("%.1f%% [%.1f%%, %.1f%%]", 100 * x$rate, 100 * x$ci_lower, 100 * x$ci_upper),
      res_num(x$median_queue, 2L, TRUE), res_num(x$worst_queue, 2L, TRUE))
  })
  res_table(c("Sortie cancellation", "Collapsed", "Collapse rate (exact 95% interval)",
              "Median closing-window queue", "Worst closing-window queue"), rows)
}

#' Morris elementary effects ranking, leading parameters
#'
#' @param data_dir The data directory.
#' @param n Number of leading parameters to print.
#' @return The table lines.
build_morris_top <- function(data_dir, n = 20L) {
  d <- res_read("sensitivity/morris_r20/morris_ranking.csv", data_dir)
  d <- d[order(-d$mu_star), ][seq_len(n), ]
  rows <- lapply(seq_len(n), function(i) {
    c(as.character(i), sprintf("`%s`", d$parameter[i]), res_num(d$mu_star[i], 2L),
      res_num(d$sigma_ee[i], 2L))
  })
  res_table(c("Rank", "Parameter", "\u00b5*", "\u03c3"), rows)
}

#' Sobol total-order decomposition of the system operating theatre queue
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_sobol <- function(data_dir) {
  d <- res_read("sensitivity/sobol_n800/sobol_system_ot_q.csv", data_dir)
  d <- d[order(-d$ST), ]
  rows <- lapply(seq_len(nrow(d)), function(i) {
    c(sprintf("`%s`", d$parameter[i]), res_ci(d$ST[i], d$ST_lower[i], d$ST_upper[i], 2L),
      res_ci(d$S1[i], d$S1_lower[i], d$S1_upper[i], 2L))
  })
  res_table(c("Parameter", "Total-order index", "First-order index"), rows)
}

#' Value of a step function at given times
#'
#' @param t_ev Times at which the function changes, increasing.
#' @param v_ev Value the function takes from each of those times.
#' @param at Times to evaluate at.
#' @return The function's value at each of `at`, taking the first value before the
#'   first change.
res_step_at <- function(t_ev, v_ev, at) {
  v_ev[pmax(findInterval(at, t_ev), 1L)]
}

#' Time-weighted statistics of one resource over a campaign
#'
#' @param resources The resource monitor, with `resource`, `time`, `server`,
#'   `queue` and `capacity` columns.
#' @param name The resource's name.
#' @param window_min The campaign length in minutes.
#' @return A list of `util` (servers in use over capacity, both time-weighted,
#'   so a resource open half the campaign is measured over that half), `open`
#'   (the share of the campaign with capacity above zero), `queue_share` (the
#'   share of the campaign with a queue of one or more) and `mean_in_use`.
res_resource_stats <- function(resources, name, window_min) {
  x <- resources[resources$resource == name & resources$time <= window_min, ]
  x <- x[order(x$time), ]
  dt <- pmax(c(x$time[-1], window_min) - x$time, 0)
  list(util = sum(x$server * dt) / max(sum(x$capacity * dt), 1e-9),
       open = sum(dt * (x$capacity > 0)) / window_min,
       queue_share = sum(dt * (x$queue >= 1)) / window_min,
       mean_in_use = sum(x$server * dt) / window_min)
}

#' Share of a surgical section's open time with a queue for any of its staff
#'
#' @param resources The resource monitor.
#' @param section The R2E surgical section number.
#' @param window_min The campaign length in minutes.
#' @return The share of the time the section was rostered open during which
#'   one or more casualties queued for any role in it.
res_section_queue_share <- function(resources, section, window_min) {
  r <- resources[resources$time <= window_min, ]
  mine <- r[grepl(sprintf("^c_r2eheavy_surg_%d_", section), r$resource), ]
  mine <- mine[order(mine$resource, mine$time), ]
  mine$dq <- ave(mine$queue, mine$resource, FUN = function(q) q - c(0, head(q, -1)))
  ev <- aggregate(dq ~ time, mine, sum)
  ev <- ev[order(ev$time), ]
  ev$q <- cumsum(ev$dq)
  anchor <- mine[grepl("surgeon_1_", mine$resource), ]
  anchor <- anchor[order(anchor$time), ]
  tt <- sort(unique(c(ev$time, anchor$time, 0)))
  dur <- diff(c(tt, window_min))
  queued <- res_step_at(ev$time, ev$q, tt) >= 1
  open <- res_step_at(anchor$time, anchor$capacity, tt) > 0
  sum(dur * (queued & open)) / sum(dur * open)
}

#' Verification measurements of one seed-42 campaign
#'
#' @param mon The monitoring list of one run, with `arrivals` and `attributes`.
#' @param cfg The parsed configuration the run used.
#' @param days The campaign length in days.
#' @return A data frame of `section`, `metric`, `value` and `configured`, the
#'   last the configured expectation the realised value is read against or `NA`.
#'   A generator's configured daily mean is per thousand personnel, so a stream's
#'   expectation is that mean times its population over a thousand times the
#'   days; the force regeneration cycle moves the population a little over the
#'   campaign, which the comparison therefore carries as a small drift.
#'
#' @details Only what verifies a mechanism that replication cannot show: that
#'   each arrival stream realises its configured rate, that the triage and
#'   damage control splits realise their configured shares, that the strategic
#'   evacuation timeline closes, and the force regeneration trace. A run's
#'   performance is measured by the replicated experiments, not by this.
seed42_verification_rows <- function(mon, cfg, days) {
  att <- mon$attributes[order(mon$attributes$time), ]
  last <- att[!duplicated(paste(att$name, att$key), fromLast = TRUE), ]
  wide <- function(key) {
    x <- last[last$key == key, ]
    setNames(x$value, x$name)
  }
  arr_names <- mon$arrivals$name
  rows <- list()
  add <- function(section, metric, value, configured = NA_real_) {
    rows[[length(rows) + 1L]] <<- data.frame(section = section, metric = metric,
                                             value = value, configured = configured,
                                             stringsAsFactors = FALSE)
  }
  streams <- c("wia_cbt", "wia_spt", "kia_cbt", "kia_spt", "dnbi_cbt", "dnbi_spt")
  for (st in streams) {
    n <- sum(grepl(paste0("^", st, "[0-9]+$"), arr_names))
    add("generation", paste0(st, "_total"), n, cfg$vars$generators[[st]]$mean_daily *
        (if (grepl("_cbt$", st)) cfg$pops$combat else cfg$pops$support) / 1000 * days)
  }
  add("generation", "arrivals_total", length(arr_names))
  pri <- wide("priority")
  share <- c(cfg$vars$r1$priority$one, cfg$vars$r1$priority$two, cfg$vars$r1$priority$three)
  for (k in 1:3) {
    add("triage", paste0("priority_", k), sum(pri == k, na.rm = TRUE),
        share[k] * sum(!is.na(pri)))
  }
  add("triage", "killed_in_action", sum(grepl("^kia_", arr_names)))
  surg <- unique(c(names(wide("r2b_surgery")[wide("r2b_surgery") == 1]),
                   names(wide("r2e_surgery")[wide("r2e_surgery") == 1])))
  dcs <- wide("dcs_pathway")
  for (k in 1:2) {
    ids <- surg[pri[surg] == k & !is.na(pri[surg])]
    add("damage_control", paste0("priority_", k, "_operated"), length(ids))
    add("damage_control", paste0("priority_", k, "_damage_control"), sum(dcs[ids] == 1, na.rm = TRUE),
        cfg$vars$r1$other[[paste0("pri", k, "_dcs_rate")]] * length(ids))
  }
  dec <- wide("evacuation_decision_day")
  add("evacuation", "decisions", sum(!is.na(dec)))
  add("evacuation", "boarded", sum(!is.na(wide("ame_departure_time"))))
  add("evacuation", "still_waiting_at_close", sum(!is.na(dec)) - sum(!is.na(wide("ame_departure_time"))))
  wait <- wide("ame_wait_minutes")
  add("evacuation", "mean_wait_days", mean(wait, na.rm = TRUE) / DAY_MIN)
  add("evacuation", "p90_wait_days", unname(quantile(wait, 0.9, na.rm = TRUE)) / DAY_MIN)
  res <- mon$resources
  window <- days * DAY_MIN
  bypass <- wide("r2b_bypass_reason")
  add("surgical_load", "r2b_diverted_team_off_shift", sum(bypass == 1, na.rm = TRUE))
  add("surgical_load", "r2b_diverted_theatre_busy", sum(bypass == 2, na.rm = TRUE))
  hold_beds <- unique(res$resource[grepl("^b_r2b_hold_", res$resource)])
  add("surgical_load", "r2b_hold_mean_beds_in_use",
      sum(vapply(hold_beds, function(n) res_resource_stats(res, n, window)$mean_in_use, 0)))
  for (k in 1:3) {
    anchor <- sprintf("c_r2eheavy_surg_%d_surgeon_1_t1", k)
    st <- res_resource_stats(res, anchor, window)
    add("surgical_load", sprintf("r2e_section_%d_utilisation_of_open_time", k), 100 * st$util)
    add("surgical_load", sprintf("r2e_section_%d_queued_share_of_open_time", k),
        100 * res_section_queue_share(res, k, window))
  }
  force <- att[att$key %in% c("effective_force_combat", "effective_force_support"), ]
  for (key in c("effective_force_combat", "effective_force_support")) {
    f <- force[force$key == key, ]
    for (d in c(0, 90, 180, 270, days)) {
      add("force", paste0(key, "_day_", d), f$value[max(which(f$time <= d * DAY_MIN))])
    }
  }
  do.call(rbind, rows)
}

#' Treated-cohort died-of-wounds rate against each campaign's historical anchor
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_dow_calibration <- function(data_dir) {
  d <- res_read("calibration/dow_calibration.csv", data_dir)
  labels <- c(default = "Shipped default", moderate_intensity = "Moderate intensity",
              high_intensity = "High intensity")
  rows <- lapply(seq_len(nrow(d)), function(i) {
    x <- d[i, ]
    c(labels[[x$scenario]], sprintf("%s, %.2f%% (%s)", if (x$kind == "bound") "at or below" else
      "reported", 100 * x$target, x$anchor),
      sprintf("%.3f%% [%.3f%%, %.3f%%]", 100 * x$rate, 100 * x$ci_lower, 100 * x$ci_upper),
      as.character(x$replications))
  })
  res_table(c("Configuration", "Historical anchor", "Treated-cohort died-of-wounds rate",
              "Replications"), rows)
}

#' One section of the seed-42 verification measurements as a table
#'
#' @param data_dir The data directory.
#' @param section The section name in `seed42_verification.csv`.
#' @param labels Named character vector mapping each metric to its row label.
#' @param dp Decimal places for the realised and configured columns, one value for
#'   the table or a vector named by metric.
#' @return The table lines.
build_annex <- function(data_dir, section, labels, dp = 0L) {
  d <- res_read("seed42_verification.csv", data_dir)
  d <- d[d$section == section, ]
  rows <- lapply(names(labels), function(m) {
    x <- d[d$metric == m, ]
    if (nrow(x) != 1L) stop(sprintf("expected one '%s' metric, found %d", m, nrow(x)), call. = FALSE)
    k <- if (length(dp) > 1L) dp[[m]] else dp
    c(labels[[m]], res_num(x$value, k, TRUE),
      if (is.na(x$configured)) "not applicable" else res_num(x$configured, k, TRUE))
  })
  res_table(c("Measure", "Realised", "Configured expectation"), rows)
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
  airlift_reliability = function(dd) build_airlift(dd, "reliability"),
  queue_clearance = build_queue_clearance,
  degraded_care = build_degraded_care,
  icu_gate = build_icu_gate,
  airlift_collapse = build_airlift_collapse,
  morris_top = build_morris_top,
  sobol = build_sobol,
  dow_calibration = build_dow_calibration,
  annex_generation = function(dd) build_annex(dd, "generation", c(
    wia_cbt_total = "Combat wounded in action", wia_spt_total = "Support wounded in action",
    kia_cbt_total = "Combat killed in action", kia_spt_total = "Support killed in action",
    dnbi_cbt_total = "Combat disease and non-battle injury",
    dnbi_spt_total = "Support disease and non-battle injury")),
  annex_triage = function(dd) build_annex(dd, "triage", c(
    priority_1 = "Priority 1", priority_2 = "Priority 2", priority_3 = "Priority 3",
    killed_in_action = "Killed in action")),
  annex_damage_control = function(dd) build_annex(dd, "damage_control", c(
    priority_1_operated = "Priority 1 operated",
    priority_1_damage_control = "Priority 1 damage control",
    priority_2_operated = "Priority 2 operated",
    priority_2_damage_control = "Priority 2 damage control")),
  annex_evacuation = function(dd) build_annex(dd, "evacuation", c(
    decisions = "Strategic evacuation decisions", boarded = "Boarded",
    still_waiting_at_close = "Still waiting at the close")),
  annex_surgical_load = function(dd) build_annex(dd, "surgical_load", c(
    r2b_diverted_team_off_shift = "Diverted from R2B, surgical team off shift",
    r2b_diverted_theatre_busy = "Diverted from R2B, theatre busy",
    r2b_hold_mean_beds_in_use = "R2B holding beds in use, both facilities (mean)",
    r2e_section_1_utilisation_of_open_time = "R2E section 1 utilisation of open time (%)",
    r2e_section_2_utilisation_of_open_time = "R2E section 2 utilisation of open time (%)",
    r2e_section_3_utilisation_of_open_time = "R2E section 3 utilisation of open time (%)",
    r2e_section_1_queued_share_of_open_time = "R2E section 1 queued share of open time (%)",
    r2e_section_2_queued_share_of_open_time = "R2E section 2 queued share of open time (%)",
    r2e_section_3_queued_share_of_open_time = "R2E section 3 queued share of open time (%)"),
    c(r2b_diverted_team_off_shift = 0L, r2b_diverted_theatre_busy = 0L,
      r2b_hold_mean_beds_in_use = 2L, r2e_section_1_utilisation_of_open_time = 1L,
      r2e_section_2_utilisation_of_open_time = 1L, r2e_section_3_utilisation_of_open_time = 1L,
      r2e_section_1_queued_share_of_open_time = 1L, r2e_section_2_queued_share_of_open_time = 1L,
      r2e_section_3_queued_share_of_open_time = 1L)),
  annex_force = function(dd) build_annex(dd, "force", c(
    effective_force_combat_day_0 = "Combat force, day 0",
    effective_force_combat_day_180 = "Combat force, day 180",
    effective_force_combat_day_360 = "Combat force, day 360",
    effective_force_support_day_0 = "Support force, day 0",
    effective_force_support_day_180 = "Support force, day 180",
    effective_force_support_day_360 = "Support force, day 360"))
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
