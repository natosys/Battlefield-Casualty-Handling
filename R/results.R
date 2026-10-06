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
  #' Rows of the totals summary for one metric and profile
  #'
  #' @param metric The response key.
  #' @param prof The scenario profile.
  #' @return A one-row data frame.
  pick <- function(metric, prof) d[d$metric == metric & d$scenario == prof, ]
  spec <- list(
    list("Total casualties/run", "total_casualties", 1L, 1, FALSE, 2L),
    list("Wounded in action/run", "wia_count", 1L, 1, FALSE, 2L),
    list("Died of wounds/run", "dow_count", 2L, 1, FALSE, 1L),
    list("Died of wounds, as share of wounded", "dow_rate", 2L, 100, TRUE, 2L)
  )
  rows <- lapply(spec, function(s) {
    cells <- vapply(SCENARIO_PROFILES, function(p) {
      x <- pick(s[[2]], p)
      base <- res_ci(x$mean, x$ci_lower, x$ci_upper, dp = s[[3]], scale = s[[4]], big = TRUE,
                     unit = if (s[[5]]) "%" else "")
      if (s[[5]]) return(base)
      #' Format a p10 to p90 bound, printing zero as `0`
      #'
      #' @param v A value.
      #' @return The formatted bound.
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
  #' Format a level with two decimals below ten and one above
  #'
  #' @param x A value or row.
  #' @return The formatted level.
  level <- function(x) {
    if (abs(x) < 10) formatC(x, format = "f", digits = 2) else
      formatC(x, format = "f", digits = 1, big.mark = ",")
  }
  #' One cell of the stability table
  #'
  #' @param r A response key.
  #' @return The cell text.
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
    list("Deaths of wounds per day", "dow", "system")
  )
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
  #' One establishment row of the hold sweep grid
  #'
  #' @param beds Holding beds per unit.
  #' @param days Evacuation threshold in days.
  #' @return A one-row data frame.
  pick <- function(beds, days) d[d$hold_beds == beds & d$evac_threshold_days == days, ]
  #' One row of the hold sweep table
  #'
  #' @param label The row label.
  #' @param x A value or row.
  #' @return The row's cells.
  row <- function(label, x) {
    c(label, res_ci(x$mean_r2b_hold_q, x$ci_lower_r2b_hold_q, x$ci_upper_r2b_hold_q, 2L,
                    floor0 = TRUE),
      sprintf("%.1f%% [%.1f, %.1f]", 100 * x$mean_r2b_hold_util, 100 * x$ci_lower_r2b_hold_util,
              100 * x$ci_upper_r2b_hold_util),
      res_ci(x$mean_r2e_hold_q, x$ci_lower_r2e_hold_q, x$ci_upper_r2e_hold_q, 3L, floor0 = TRUE),
      res_ci(x$mean_r2e_icu_q, x$ci_lower_r2e_icu_q, x$ci_upper_r2e_icu_q, 3L, floor0 = TRUE),
      res_ci(x$mean_rtd, x$ci_lower_rtd, x$ci_upper_rtd, 0L, floor0 = TRUE),
      res_ci(x$mean_dow, x$ci_lower_dow, x$ci_upper_dow, 1L, floor0 = TRUE))
  }
  tail_header <- c("R2B hold mean queue", "R2B hold utilisation", "R2E hold mean queue",
                   "R2E ICU mean queue", "Returns to duty", "Deaths of wounds")
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
  file <- if (high) {
    "sweeps/transport_capacity_by_fleet_size_high_intensity.csv"
  } else {
    "sweeps/transport_capacity_by_fleet_size.csv"
  }
  d <- res_read(file, data_dir)
  #' One transport queue cell
  #'
  #' @param x A value or row.
  #' @return The cell text.
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

#' Transport holder queue and utilisation table
#'
#' @param data_dir The data directory.
#' @return The table lines.
#'
#' @details One row per holder, the shared brigade fleets then the facility's
#'   integral evacuation elements, each with its closing-window mean queue and
#'   utilisation at both intensities. Utilisation is a share, so its interval
#'   is clamped to the zero to 100% it can take; the queue's lower bound is
#'   clamped at zero as in the other pool tables.
build_transport_holders <- function(data_dir) {
  d <- res_read("scenarios/scenario_transport_holders.csv", data_dir)
  #' One utilisation cell, as a percentage with its clamped interval
  #'
  #' @param x A row of the summary.
  #' @return The cell text.
  util <- function(x) {
    sprintf("%.1f%% [%.1f%%, %.1f%%]", 100 * x$util_mean,
            100 * max(x$util_ci_lower, 0), 100 * min(x$util_ci_upper, 1))
  }
  rows <- lapply(unique(d$holder), function(h) {
    x <- lapply(SCENARIO_PROFILES, function(p) d[d$holder == h & d$scenario == p, ])
    c(h, if (x[[1]]$kind == "shared") "Shared" else "Integral",
      unlist(lapply(x, function(r) {
        c(res_ci(r$q_mean, r$q_ci_lower, r$q_ci_upper, 3L, floor0 = TRUE), util(r))
      })))
  })
  res_table(c("Holder", "Asset", "Moderate intensity mean queue",
              "Moderate intensity utilisation", "High intensity mean queue",
              "High intensity utilisation"), rows)
}

#' Forward holding frontier table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_forward_hold <- function(data_dir) {
  d <- res_read("sweeps/r2b_forward_hold_frontier.csv", data_dir)
  rows <- lapply(seq_len(nrow(d)), function(i) {
    x <- d[i, ]
    c(x$arm,
      res_ci(x$mean_r2e_icu_q, x$ci_lower_r2e_icu_q, x$ci_upper_r2e_icu_q, 3L, floor0 = TRUE),
      sprintf("%.1f%%", 100 * x$mean_r2b_icu_util), sprintf("%.1f%%", 100 * x$mean_r2e_icu_util),
      sprintf("%.1f [%.1f, %.1f]", 100 * x$mean_pd_icu_share, 100 * x$ci_lower_pd_icu_share,
              100 * x$ci_upper_pd_icu_share),
      res_ci(x$mean_dow, x$ci_lower_dow, x$ci_upper_dow, 2L))
  })
  res_table(c("Forward holding rule", "R2E ICU mean queue", "R2B ICU utilisation",
              "R2E ICU utilisation", "Post-definitive care in ICU", "Died of wounds per run"), rows)
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
    list("Role 4 peak beds", "role4_peak", 1L, 1)
  )
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
    list("Role 4 peak beds", "role4_peak", 1L, 1)
  )
  header <- c("Response", "30 beds (shipped)", "45 beds", "60 beds", "90 beds")
  res_sweep_table(d, header, arms, rows)
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
    list("Returns to duty", "total_rtd", 1L, 1)
  )
  header <- c("Response", "0 (disabled)", as.character(thresholds[2:5]), "8 (shipped)",
              as.character(thresholds[7:9]))
  res_sweep_table(d, header, arms, rows)
}

#' Casualty surge event stress test table
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_casualty_surge <- function(data_dir) {
  cnt <- res_read("casualty_surge/casualty_surge_count_summary.csv", data_dir)
  dow <- res_read("casualty_surge/casualty_surge_dow_summary.csv", data_dir)
  rep <- res_read("casualty_surge/casualty_surge_replications.csv", data_dir)
  #' Mean total casualties of one arm
  #'
  #' @param r A response key.
  #' @return The mean.
  tot <- function(r) cnt[cnt$rate_per_day == r & cnt$response == "total_casualties", "mean"]
  ev <- cnt[cnt$rate_per_day == 0.2 & cnt$response == "n_events", "mean"]
  range_ev <- range(rep$n_events[rep$rate_per_day == 0.2])
  #' Pooled died-of-wounds rate cell of one arm and origin
  #'
  #' @param r A response key.
  #' @param o An origin.
  #' @return The cell text.
  pct <- function(r, o) {
    x <- dow[abs(dow$rate_per_day - r) < 1e-9 & dow$origin == o, ]
    sprintf("%.2f%% [%.2f%%, %.2f%%]", 100 * x$rate, 100 * x$ci_lower, 100 * x$ci_upper)
  }
  events <- sprintf("%s (range %d\u2013%d)", res_num(ev, 2L), range_ev[1], range_ev[2])
  rows <- list(
    c("Average total casualties/run", res_num(tot(0), 1L), res_num(tot(0.2), 1L)),
    c("Average events/run", "0", events),
    c("Died-of-wounds rate, ordinary casualties", pct(0, "ordinary"), pct(0.2, "ordinary")),
    c("Died-of-wounds rate, event casualties", "not applicable", pct(0.2, "event"))
  )
  res_table(c("Metric", "No events injected", "Events injected"), rows)
}

#' Casualty surge event size sweep table
#'
#' @param data_dir The data directory.
#' @return The table lines.
#'
#' @details One row per swept event size, the no-event arm first. Peak queues
#'   are the largest four-hour mean queue of each pool over the campaign.
build_casualty_surge_size <- function(data_dir) {
  d <- res_read("casualty_surge/casualty_surge_size_summary.csv", data_dir)
  d <- d[order(d$size), ]
  #' Pooled died-of-wounds rate cell with its exact interval
  #'
  #' @param r Rate.
  #' @param lo Lower bound.
  #' @param hi Upper bound.
  #' @return The cell text.
  pct <- function(r, lo, hi) {
    ifelse(is.na(r), "not applicable",
           sprintf("%.2f%% [%.2f%%, %.2f%%]", 100 * r, 100 * lo, 100 * hi))
  }
  #' Mean peak queue cell with its half-width
  #'
  #' @param i Pool index.
  #' @param k Row index.
  #' @return The cell text.
  peak <- function(i, k) {
    sprintf("%s \u00b1 %s", res_num(d[[paste0("peak_queue_", i)]][k], 1L),
            res_num(d[[paste0("peak_queue_", i, "_ci")]][k], 1L))
  }
  rows <- lapply(seq_len(nrow(d)), function(k) {
    c(if (d$size[k] == 0) "None" else as.character(d$size[k]),
      res_num(d$mean_events[k], 1L),
      pct(d$dow_event_rate[k], d$dow_event_lower[k], d$dow_event_upper[k]),
      pct(d$dow_ordinary_rate[k], d$dow_ordinary_lower[k], d$dow_ordinary_upper[k]),
      peak(1L, k), peak(2L, k), peak(3L, k), peak(4L, k))
  })
  res_table(c("Event size", "Events/run", "Died of wounds, event casualties",
              "Died of wounds, ordinary casualties", "Peak R2B holding queue",
              "Peak R2E theatre queue", "Peak R2E intensive care queue",
              "Peak R2E holding queue"), rows)
}

#' Defaults for one cell of a strategic evacuation table
AIRLIFT_CELL_DEFAULTS <- list(m = 1, dp = 2L, ci = TRUE, unit = "", neg = FALSE)

#' One cell of a strategic evacuation table
#'
#' @param x One row of the airlift summary.
#' @param opt Formatting options, overriding `AIRLIFT_CELL_DEFAULTS`.
#' @return The cell text: the mean, with its interval where `ci` is set.
air_cell <- function(x, opt) {
  opt <- modifyList(AIRLIFT_CELL_DEFAULTS, opt)
  a <- x$mean * opt$m
  lo <- x$ci_lower * opt$m
  hi <- x$ci_upper * opt$m
  if (opt$neg) {
    flipped <- c(-a, -hi, -lo)
    a <- flipped[1]
    lo <- flipped[2]
    hi <- flipped[3]
  }
  if (!opt$ci) return(paste0(res_num(a, opt$dp), opt$unit))
  sprintf("%s%s [%s%s, %s%s]", res_num(a, opt$dp), opt$unit, res_num(lo, opt$dp), opt$unit,
          res_num(hi, opt$dp), opt$unit)
}

#' Strategic evacuation tables
#'
#' @param data_dir The data directory.
#' @param which One of `"baseline"`, `"interval"` or `"reliability"`.
#' @return The table lines.
build_airlift <- function(data_dir, which) {
  d <- res_read("airlift/airlift_summary.csv", data_dir)
  #' One airlift summary row
  #'
  #' @param col The column specification.
  #' @param resp The response key.
  #' @return A one-row data frame.
  pick <- function(col, resp) {
    x <- d[d$arm == col[[1]] & d$scenario == col[[2]] & abs(d$value - col[[3]]) < 1e-9 &
             d$response == resp, ]
    stopifnot(nrow(x) == 1L)
    x
  }
  share <- list(m = 100, dp = 0L, unit = "%")
  spec <- switch(which,
    baseline = list(
      header = c("Response at the shipped schedule", "Moderate intensity", "High intensity"),
      cols = list(list("baseline", "moderate_intensity", 0), list("high", "high_intensity", 0)),
      rows = list(
        list("Casualties boarded", "boarded", list()),
        list("Still waiting at the close", "queued_at_end", list()),
        list("Mean wait (days)", "mean_wait_days", list()),
        list("Share of R2E holding beds held by the evacuation wait", "hold_evac_share", share),
        list("Role 4 peak occupancy (concurrent patients)", "role4_peak", list()),
        list("Days the peak falls before the campaign ends", "role4_peak_after_end",
             list(neg = TRUE))
      )
    ),
    interval = list(
      header = c("Response by interval between sorties", "3 days", "5 days", "7 days (shipped)",
                 "10 days", "14 days"),
      cols = lapply(c(3, 5, 7, 10, 14), function(v) list("interval", "moderate_intensity", v)),
      rows = list(
        list("Sorties flown", "sorties_flown", list(ci = FALSE)),
        list("Mean wait (days)", "mean_wait_days", list()),
        list("Share of R2E holding beds held by the evacuation wait", "hold_evac_share", share),
        list("Ventilated pre-flight intensive care hold (hours)", "ventilated_hold_hours",
             list())
      )
    ),
    reliability = list(
      header = c("Response by configured cancellation probability", "0%", "5%", "10%", "15%",
                 "25%", "40%"),
      cols = lapply(c(0, 0.05, 0.10, 0.15, 0.25, 0.40), function(v) {
        list("reliability", "moderate_intensity", v)
      }),
      rows = list(
        list("Sorties flown", "sorties_flown", list(ci = FALSE)),
        list("Realised cancellation rate", "cancellation_rate", c(share, list(ci = FALSE))),
        list("Mean wait (days)", "mean_wait_days", list()),
        list("Share of R2E holding beds held by the evacuation wait", "hold_evac_share", share)
      )
    )
  )
  rows <- lapply(spec$rows, function(r) {
    c(r[[1]], vapply(spec$cols, function(col) air_cell(pick(col, r[[2]]), r[[3]]), ""))
  })
  res_table(spec$header, rows)
}

#' Queue clearance by resource pool at the two casualty intensities
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_queue_clearance <- function(data_dir) {
  d <- res_read("time_series/queue_clearance.csv", data_dir)
  pools <- c("R2B holding beds", "R2E operating theatres", "R2E intensive care", "R2E holding beds")
  #' One stage and intensity row of the degraded care table
  #'
  #' @param pool The resource pool.
  #' @param intensity See the enclosing function.
  #' @return The row's cells.
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
      #' Unweighted rate over a day range
      #'
      #' @param a First bound.
      #' @param b Second bound.
      #' @return The rate.
      w <- function(a, b) {
        y <- x[x$day >= a & x$day <= b & !is.na(x$daily_rate), ]
        sum(y$daily_rate * y$n_decisions) / sum(y$n_decisions)
      }
      m <- tapply(x$daily_rate, x$day, median, na.rm = TRUE)
      hit <- as.integer(names(m))[which(m >= 0.999)[1]]
      cum <- median(x$cumulative_rate[x$day == last], na.rm = TRUE)
      reached <- if (is.na(hit)) "not reached" else as.character(hit)
      #' Format a share as a percentage with one decimal
      #'
      #' @param v A value.
      #' @return The formatted share.
      pct <- function(v) sprintf("%.1f%%", 100 * v)
      rows[[length(rows) + 1L]] <- c(paste(it, st, sep = ": "), reached, pct(w(first, first + 9)),
                                     pct(w(last - 9, last)), pct(cum))
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
    sc <- as.numeric(r[3])
    dp <- as.integer(r[4])
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

#' Died-of-wounds rate by post-operative recovery pathway under the rationing rule
#'
#' @param data_dir The data directory.
#' @return The table lines: casualty-replications, deaths and the pooled rate for
#'   each pathway in the arm where the rule is in force.
#'
#' @details Pooled over replications rather than averaged per replication, most
#'   replications carrying a handful of deaths on each pathway.
build_icu_gate_pathways <- function(data_dir) {
  r <- res_read("icu_gate/icu_gate_replications.csv", data_dir)
  on <- r[r$gate_enabled == 1, ]
  #' One pathway row of the table
  #'
  #' @param label The pathway's name.
  #' @param n Pooled casualty-replications on the pathway.
  #' @param d Pooled deaths of wounds on the pathway.
  #' @return The row's four cells.
  one <- function(label, n, d) {
    c(label, res_num(n, 0L, big = TRUE), res_num(d, 0L, big = TRUE),
      paste0(res_num(100 * d / n, 2L), "%"))
  }
  res_table(c("Recovery pathway", "Casualty-replications", "Died of wounds", "Rate"),
            list(one("Intensive care bed", sum(on$icu_pathway_n), sum(on$icu_pathway_dow)),
                 one("Holding bed", sum(on$hold_pathway_n), sum(on$hold_pathway_dow))))
}

#' Paired comparisons and the half-width each was sized against
#'
#' @details One entry per paired difference the resolution table prints: the
#'   evidence file, the response, the arm it is measured from and to, the arm
#'   column, the half-width the experiment's own script sized it against (its
#'   `PAIRED_HALF_WIDTHS`), and the row label. A half-width is a target the
#'   script chose in the response's own units, so it is held beside the
#'   difference rather than recomputed from it.
RESOLUTION_ROWS <- list(
  list("hold_window/hold_window_paired.csv", "r2e_first_surgeries", NA, 0, 60, 2,
       "Hold window, R2E first surgeries"),
  list("hold_window/hold_window_paired.csv", "r2e_theatre_deferred", NA, 0, 60, 1,
       "Hold window, R2E theatre entry deferred"),
  list("hold_window/hold_window_paired.csv", "diverted_busy", NA, 0, 60, 2,
       "Hold window, diverted for a busy theatre"),
  list("hold_window/hold_window_paired.csv", "total_dow", NA, 0, 60, 0.5,
       "Hold window, died of wounds"),
  list("icu_gate/icu_gate_paired.csv", "total_dow", NA, 0, 1, 0.5,
       "Intensive care gate, died of wounds"),
  list("policy/policy_sweep_paired.csv", "total_dow", "policy_days", 21, 15, 1,
       "Policy 15 days against 21, died of wounds"),
  list("policy/policy_sweep_paired.csv", "total_dow", "policy_days", 21, 45, 1,
       "Policy 45 days against 21, died of wounds"),
  list("policy/policy_sweep_paired.csv", "total_dow", "policy_days", 21, 60, 1,
       "Policy 60 days against 21, died of wounds"),
  list("policy/saturation_sweep_paired.csv", "total_dow", NA, 0, 8, 1,
       "Saturation release at 8, died of wounds"),
  list("policy/saturation_sweep_paired.csv", "total_rtd", NA, 0, 8, 10,
       "Saturation release at 8, returns to duty")
)

#' Resolution of the paired differences the experiments leave open
#'
#' @param data_dir The data directory.
#' @return The table lines: each difference with its interval, the half-width its
#'   experiment sized it against, and the replications that half-width requires.
build_resolution <- function(data_dir) {
  rows <- lapply(RESOLUTION_ROWS, function(r) {
    p <- res_read(r[[1]], data_dir)
    hit <- p[p$response == r[[2]] & p$from == r[[4]] & p$to == r[[5]], ]
    if (nrow(hit) != 1L) {
      stop(sprintf("expected one '%s' paired row from %s to %s in %s, found %d", r[[2]], r[[4]],
                   r[[5]], r[[1]], nrow(hit)), call. = FALSE)
    }
    c(r[[7]],
      sprintf("%s [%s, %s]", res_num(hit$difference, 2L, plus = TRUE),
              res_num(hit$ci_lower, 2L, plus = TRUE), res_num(hit$ci_upper, 2L, plus = TRUE)),
      res_num(r[[6]], 1L), res_num(hit$reps_needed, 0L, big = TRUE))
  })
  res_table(c("Comparison", "Paired difference", "Half-width sought", "Replications needed"),
            rows)
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
  #' Last value of one attribute for every casualty
  #'
  #' @param key The attribute key.
  #' @return A named numeric vector.
  wide <- function(key) {
    x <- last[last$key == key, ]
    setNames(x$value, x$name)
  }
  arr_names <- mon$arrivals$name
  acc <- new.env()
  acc$rows <- list()
  #' Append one measurement to the verification table
  #'
  #' @param section The section name.
  #' @param metric The response key.
  #' @param value The measured value.
  #' @param configured The configured expectation.
  #' @return Invisibly NULL.
  add <- function(section, metric, value, configured = NA_real_) {
    row <- data.frame(section = section, metric = metric, value = value,
                      configured = configured, stringsAsFactors = FALSE)
    acc$rows[[length(acc$rows) + 1L]] <- row
  }
  streams <- c("wia_cbt", "wia_spt", "kia_cbt", "kia_spt", "dnbi_cbt", "dnbi_spt")
  for (st in streams) {
    n <- sum(grepl(paste0("^", st, "[0-9]+$"), arr_names))
    pop <- if (grepl("_cbt$", st)) cfg$pops$combat else cfg$pops$support
    expected <- cfg$vars$generators[[st]]$mean_daily * pop / 1000 * days
    add("generation", paste0(st, "_total"), n, expected)
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
    rate <- cfg$vars$r1$other[[paste0("pri", k, "_dcs_rate")]]
    add("damage_control", paste0("priority_", k, "_damage_control"),
        sum(dcs[ids] == 1, na.rm = TRUE), rate * length(ids))
  }
  dec <- wide("evacuation_decision_day")
  add("evacuation", "decisions", sum(!is.na(dec)))
  add("evacuation", "boarded", sum(!is.na(wide("ame_departure_time"))))
  boarded <- sum(!is.na(wide("ame_departure_time")))
  add("evacuation", "still_waiting_at_close", sum(!is.na(dec)) - boarded)
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
  do.call(rbind, acc$rows)
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
    kind <- if (x$kind == "bound") "at or below" else "reported"
    c(labels[[x$scenario]], sprintf("%s, %.2f%% (%s)", kind, 100 * x$target, x$anchor),
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
    if (nrow(x) != 1L) {
      stop(sprintf("expected one '%s' metric, found %d", m, nrow(x)), call. = FALSE)
    }
    k <- if (length(dp) > 1L) dp[[m]] else dp
    c(labels[[m]], res_num(x$value, k, TRUE),
      if (is.na(x$configured)) "not applicable" else res_num(x$configured, k, TRUE))
  })
  res_table(c("Measure", "Realised", "Configured expectation"), rows)
}

#' Row labels of the seed-42 casualty generation table, by metric
ANNEX_GENERATION <- c(
  wia_cbt_total = "Combat wounded in action",
  wia_spt_total = "Support wounded in action",
  kia_cbt_total = "Combat killed in action",
  kia_spt_total = "Support killed in action",
  dnbi_cbt_total = "Combat disease and non-battle injury",
  dnbi_spt_total = "Support disease and non-battle injury"
)

#' Row labels of the seed-42 triage table, by metric
ANNEX_TRIAGE <- c(
  priority_1 = "Priority 1",
  priority_2 = "Priority 2",
  priority_3 = "Priority 3",
  killed_in_action = "Killed in action"
)

#' Row labels of the seed-42 damage control table, by metric
ANNEX_DAMAGE_CONTROL <- c(
  priority_1_operated = "Priority 1 operated",
  priority_1_damage_control = "Priority 1 damage control",
  priority_2_operated = "Priority 2 operated",
  priority_2_damage_control = "Priority 2 damage control"
)

#' Row labels of the seed-42 strategic evacuation table, by metric
ANNEX_EVACUATION <- c(
  decisions = "Strategic evacuation decisions",
  boarded = "Boarded",
  still_waiting_at_close = "Still waiting at the close"
)

#' Row labels of the seed-42 surgical load table, by metric
ANNEX_SURGICAL_LOAD <- c(
  r2b_diverted_team_off_shift = "Diverted from R2B, surgical team off shift",
  r2b_diverted_theatre_busy = "Diverted from R2B, theatre busy",
  r2b_hold_mean_beds_in_use = "R2B holding beds in use, both facilities (mean)",
  r2e_section_1_utilisation_of_open_time = "R2E section 1 utilisation of open time (%)",
  r2e_section_2_utilisation_of_open_time = "R2E section 2 utilisation of open time (%)",
  r2e_section_3_utilisation_of_open_time = "R2E section 3 utilisation of open time (%)",
  r2e_section_1_queued_share_of_open_time = "R2E section 1 queued share of open time (%)",
  r2e_section_2_queued_share_of_open_time = "R2E section 2 queued share of open time (%)",
  r2e_section_3_queued_share_of_open_time = "R2E section 3 queued share of open time (%)"
)

#' Decimal places of each metric of the seed-42 surgical load table
ANNEX_SURGICAL_LOAD_DP <- c(
  r2b_diverted_team_off_shift = 0L,
  r2b_diverted_theatre_busy = 0L,
  r2b_hold_mean_beds_in_use = 2L,
  r2e_section_1_utilisation_of_open_time = 1L,
  r2e_section_2_utilisation_of_open_time = 1L,
  r2e_section_3_utilisation_of_open_time = 1L,
  r2e_section_1_queued_share_of_open_time = 1L,
  r2e_section_2_queued_share_of_open_time = 1L,
  r2e_section_3_queued_share_of_open_time = 1L
)

#' Row labels of the seed-42 force regeneration table, by metric
ANNEX_FORCE <- c(
  effective_force_combat_day_0 = "Combat force, day 0",
  effective_force_combat_day_180 = "Combat force, day 180",
  effective_force_combat_day_360 = "Combat force, day 360",
  effective_force_support_day_0 = "Support force, day 0",
  effective_force_support_day_180 = "Support force, day 180",
  effective_force_support_day_360 = "Support force, day 360"
)

# ── National support base bed demand ─────────────────────────────────────────
# The census, its composition, the theatre demand owed with it and whether it
# has settled come from data/role4_demand/ (R/role4_demand.R); the response of
# demand to the forward levers comes from the evidence sets of those levers,
# which already carry the Role 4 peak, the closing 90-day mean and the
# operations owed, and from the cancellation sweep of the same directory.

#' Casualty intensities of the Role 4 demand tables, as their column headings
ROLE4_INTENSITY_HEADINGS <- c(moderate_intensity = "Moderate intensity",
                              high_intensity = "High intensity")

#' Measures of the census, as the row label prints them and the response key
ROLE4_CENSUS_MEASURES <- list(c("mean beds", "mean"), c("peak beds", "peak"),
                              c("closing 90-day mean beds", "closing_mean"))

#' Role 4 census table, by ward phase or by origin
#'
#' @param data_dir The data directory.
#' @param which `"ward"` for the census by ward phase or `"origin"` for the
#'   census by origin of the casualty.
#' @return The table lines: for the total and each part, the mean, the peak and
#'   the closing 90-day mean of the daily census at each casualty intensity.
build_role4_census <- function(data_dir, which) {
  d <- res_read("role4_demand/role4_demand_summary.csv", data_dir)
  subjects <- switch(which,
    ward = list(c("Total", "Total"), c("Intensive care phase", "icu"),
                c("Step-down ward phase", "hold")),
    origin = list(c("Total", "Total"), c("Battle injury", "Battle injury"),
                  c("Disease and non-battle injury", "Disease and non-battle injury"),
                  c("Reconstruction cohort", "Reconstruction cohort")))
  rows <- list()
  for (sub in subjects) {
    for (meas in ROLE4_CENSUS_MEASURES) {
      cells <- vapply(names(ROLE4_INTENSITY_HEADINGS), function(sc) {
        x <- d[d$scenario == sc & d$series == "census" & d$subject == sub[2] &
                 d$response == meas[2], ]
        stopifnot(nrow(x) == 1L)
        res_ci(x$mean, x$ci_lower, x$ci_upper, dp = 2L, floor0 = TRUE)
      }, character(1))
      rows[[length(rows) + 1L]] <- c(paste0(sub[1], ", ", meas[1]), cells)
    }
  }
  res_table(c(if (which == "ward") "Census by ward phase" else "Census by origin",
              unname(ROLE4_INTENSITY_HEADINGS)), rows)
}

#' Role 4 operating theatre demand table
#'
#' @param data_dir The data directory.
#' @return The table lines: the operations owed at the national support base, inside
#'   the campaign, after it and in all by the casualties it admitted, and the theatre
#'   minutes of the definitive repairs among them, at each casualty intensity.
build_role4_operations <- function(data_dir) {
  d <- res_read("role4_demand/role4_demand_summary.csv", data_dir)
  spec <- list(
    list("Operations owed within the 360 days", "operations", "total", 0L),
    list("Operations owed after day 360", "operations_after_horizon", "total", 0L),
    list("Operations owed by casualties admitted during the campaign", "operations_admitted",
         "total", 0L),
    list("Operations owed on the busiest day", "operations", "peak", 2L),
    list("Closing 90-day mean operations owed per day", "operations", "closing_mean", 2L),
    list("Theatre minutes owed for definitive repairs", "theatre_minutes", "total", 0L)
  )
  rows <- lapply(spec, function(r) {
    c(r[[1]], vapply(names(ROLE4_INTENSITY_HEADINGS), function(sc) {
      x <- d[d$scenario == sc & d$series == r[[2]] & d$subject == "Total" &
               d$response == r[[3]], ]
      stopifnot(nrow(x) == 1L)
      res_ci(x$mean, x$ci_lower, x$ci_upper, dp = r[[4]], big = TRUE, floor0 = TRUE)
    }, character(1)))
  })
  res_table(c("Demand owed alongside the census", unname(ROLE4_INTENSITY_HEADINGS)), rows)
}

#' Role 4 census stationarity table
#'
#' @param data_dir The data directory.
#' @return The table lines: for the total and each part of the census, the
#'   classification of its 30-day block means over the campaign, the first
#'   block from which it stays within its late band, and its late mean, at each
#'   casualty intensity.
build_role4_stability <- function(data_dir) {
  d <- res_read("role4_demand/role4_demand_stability.csv", data_dir)
  subjects <- list(c("Total", "Total"), c("Intensive care phase", "icu"),
                   c("Step-down ward phase", "hold"), c("Battle injury", "Battle injury"),
                   c("Disease and non-battle injury", "Disease and non-battle injury"),
                   c("Reconstruction cohort", "Reconstruction cohort"))
  rows <- lapply(subjects, function(sub) {
    cells <- unlist(lapply(names(ROLE4_INTENSITY_HEADINGS), function(sc) {
      x <- d[d$scenario == sc & d$subject == sub[2], ]
      stopifnot(nrow(x) == 1L)
      c(x$stability, if (is.na(x$settles_by_block)) "none" else as.character(x$settles_by_block),
        res_num(x$late_mean, 2L))
    }))
    c(sub[1], cells)
  })
  header <- c("Census", unlist(lapply(unname(ROLE4_INTENSITY_HEADINGS), function(h) {
    paste(h, c("classification", "settles by block", "late mean beds"))
  })))
  res_table(header, rows)
}

#' Days at which the cumulative moving average of the census is read
ROLE4_CMA_DAYS <- c(30L, 90L, 180L, 360L)

#' Role 4 census cumulative moving average table
#'
#' @param data_dir The data directory.
#' @return The table lines: the cumulative moving average of the cross-replication
#'   mean total census at each reading day, at each casualty intensity.
#'
#' @details The Welch cumulative moving average, as docs/Methods.md applies it to
#'   the sustained-horizon pools, of the mean across replications of the total
#'   daily census. A series that has settled flattens; one still filling keeps
#'   rising.
build_role4_cma <- function(data_dir) {
  d <- res_read("role4_demand/role4_demand_daily.csv", data_dir)
  d <- d[d$subject == "Total", ]
  rows <- lapply(ROLE4_CMA_DAYS, function(day) {
    c(sprintf("Day %d", day), vapply(names(ROLE4_INTENSITY_HEADINGS), function(sc) {
      m <- d[d$scenario == sc, ]
      m <- m[order(m$day), ]
      res_num(mean(m$mean[m$day <= day]), 2L)
    }, character(1)))
  })
  res_table(c("Cumulative moving average of the mean total census (beds)",
              unname(ROLE4_INTENSITY_HEADINGS)), rows)
}

#' Rows of the Role 4 demand tables for the three lever evidence sets
ROLE4_LEVER_ROWS <- list(
  list("Role 4 peak beds", "role4_peak", 1L, 1),
  list("Role 4 closing 90-day mean beds", "role4_sustained", 1L, 1),
  list("Role 4 operations owed", "role4_operations", 0L, 1)
)

#' Role 4 demand against one forward lever
#'
#' @param data_dir The data directory.
#' @param which One of `"policy"`, `"establishment"`, `"saturation"` or
#'   `"cancellation"`.
#' @return The table lines: the peak, the closing 90-day mean and the operations
#'   owed at each swept value of the lever.
build_role4_levers <- function(data_dir, which) {
  if (which == "cancellation") {
    d <- res_read("role4_demand/role4_demand_reliability_summary.csv", data_dir)
    probs <- c(0, 0.05, 0.10, 0.15, 0.25, 0.40)
    spec <- list(list("Role 4 peak beds", "census", "peak", 1L),
                 list("Role 4 closing 90-day mean beds", "census", "closing_mean", 1L),
                 list("Role 4 operations owed", "operations_admitted", "total", 0L))
    rows <- lapply(spec, function(r) {
      c(r[[1]], vapply(probs, function(p) {
        x <- d[abs(d$failure_probability - p) < 1e-9 & d$series == r[[2]] &
                 d$subject == "Total" & d$response == r[[3]], ]
        stopifnot(nrow(x) == 1L)
        res_ci(x$mean, x$ci_lower, x$ci_upper, dp = r[[4]], floor0 = TRUE)
      }, character(1)))
    })
    return(res_table(c("Response", "0% (shipped)", "5%", "10%", "15%", "25%", "40%"), rows))
  }
  switch(which,
    policy = res_sweep_table(res_read("policy/policy_sweep.csv", data_dir),
      c("Response", "15 d", "21 d (shipped)", "30 d", "45 d", "60 d"),
      lapply(c(15, 21, 30, 45, 60), function(v) list(policy_days = v)), ROLE4_LEVER_ROWS),
    establishment = res_sweep_table(res_read("policy/establishment_sweep.csv", data_dir),
      c("Response", "30 beds (shipped)", "45 beds", "60 beds", "90 beds"),
      lapply(c(30, 45, 60, 90), function(v) list(hold_beds = v)), ROLE4_LEVER_ROWS),
    saturation = res_sweep_table(res_read("policy/saturation_sweep.csv", data_dir),
      c("Response", "0 (disabled)", "1", "2", "3", "5", "8 (shipped)", "12", "16", "24"),
      lapply(c(0, 1, 2, 3, 5, 8, 12, 16, 24), function(v) list(saturation_threshold = v)),
      ROLE4_LEVER_ROWS),
    stop(sprintf("no Role 4 lever table named '%s'", which), call. = FALSE))
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
  transport_holders = build_transport_holders,
  forward_hold = build_forward_hold,
  policy = build_policy,
  establishment = build_establishment,
  saturation = build_saturation,
  casualty_surge = build_casualty_surge,
  casualty_surge_size = build_casualty_surge_size,
  airlift_baseline = function(dd) build_airlift(dd, "baseline"),
  airlift_interval = function(dd) build_airlift(dd, "interval"),
  airlift_reliability = function(dd) build_airlift(dd, "reliability"),
  queue_clearance = build_queue_clearance,
  degraded_care = build_degraded_care,
  icu_gate = build_icu_gate,
  icu_gate_pathways = build_icu_gate_pathways,
  airlift_collapse = build_airlift_collapse,
  role4_census_ward = function(dd) build_role4_census(dd, "ward"),
  role4_census_origin = function(dd) build_role4_census(dd, "origin"),
  role4_operations = build_role4_operations,
  role4_stability = build_role4_stability,
  role4_cma = build_role4_cma,
  role4_levers_policy = function(dd) build_role4_levers(dd, "policy"),
  role4_levers_establishment = function(dd) build_role4_levers(dd, "establishment"),
  role4_levers_saturation = function(dd) build_role4_levers(dd, "saturation"),
  role4_levers_cancellation = function(dd) build_role4_levers(dd, "cancellation"),
  resolution = build_resolution,
  morris_top = build_morris_top,
  sobol = build_sobol,
  dow_calibration = build_dow_calibration,
  annex_generation = function(dd) build_annex(dd, "generation", ANNEX_GENERATION),
  annex_triage = function(dd) build_annex(dd, "triage", ANNEX_TRIAGE),
  annex_damage_control = function(dd) build_annex(dd, "damage_control", ANNEX_DAMAGE_CONTROL),
  annex_evacuation = function(dd) build_annex(dd, "evacuation", ANNEX_EVACUATION),
  annex_surgical_load = function(dd) {
    build_annex(dd, "surgical_load", ANNEX_SURGICAL_LOAD, ANNEX_SURGICAL_LOAD_DP)
  },
  annex_force = function(dd) build_annex(dd, "force", ANNEX_FORCE)
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
#' @param ref The reference without the `cell:` prefix: `table|row|column|part`, or the
#'   same four fields separated by `::` where the span sits inside a markdown table row,
#'   whose cells a pipe would split.
#' @param data_dir The data directory.
#' @param tables The registry of table builders to resolve the table name against.
#' @return The cell text, or its leading number (`mean`) or interval (`ci`).
res_cell_value <- function(ref, data_dir, tables = RESULTS_TABLES) {
  sep <- if (grepl("::", ref, fixed = TRUE)) "::" else "|"
  p <- strsplit(ref, sep, fixed = TRUE)[[1]]
  if (length(p) != 4L) {
    stop(sprintf("cell reference '%s' needs table%srow%scolumn%spart", ref, sep, sep, sep),
         call. = FALSE)
  }
  builder <- tables[[p[1]]]
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
#' @param tables The registry of table builders.
#' @return A character vector of lines for a table, or a single string for a cell.
res_span_content <- function(name, data_dir, tables = RESULTS_TABLES) {
  if (startsWith(name, "cell:")) {
    return(res_cell_value(sub("^cell:", "", name), data_dir, tables))
  }
  builder <- tables[[name]]
  if (is.null(builder)) stop(sprintf("no generated table named '%s'", name), call. = FALSE)
  builder(data_dir)
}

#' Regenerate every generated span of a document
#'
#' @param text The document as one string.
#' @param data_dir The data directory.
#' @param tables The registry of table builders.
#' @return The document with each span's content replaced.
#'
#' @details A table span puts its lines on their own lines between the markers
#'   and a cell span keeps its value inline, so a sentence reads normally in
#'   the source. Rendering twice gives the same text, which is what lets a
#'   check compare a document with its own re-rendering.
render_results <- function(text, data_dir = RESULTS_DATA_DIR, tables = RESULTS_TABLES) {
  m <- gregexpr(RESULTS_SPAN_PATTERN, text, perl = TRUE)[[1]]
  if (m[1] == -1L) return(text)
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  out <- character(0)
  pos <- 1L
  for (i in seq_along(starts)) {
    span <- substr(text, starts[i], starts[i] + lens[i] - 1L)
    name <- sub(RESULTS_SPAN_PATTERN, "\\1", span, perl = TRUE)
    content <- res_span_content(name, data_dir, tables)
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
