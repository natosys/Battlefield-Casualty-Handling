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

#' File name of an evidence file under a scenario's suffix
#'
#' @param stem The path without the suffix or the extension.
#' @param suffix The scenario suffix, `""` for the shipped configuration or
#'   `"_high_intensity"` for the high-intensity evidence set.
#' @return The path `<stem><suffix>.csv`.
res_file <- function(stem, suffix = "") paste0(stem, suffix, ".csv")

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

#' The profile that separates the care era from the casualty load
#'
#' @details Carries the high-intensity casualty generators under the shipped
#'   treatment model. The three comparative tables print it after their ratio
#'   column, so the two-profile columns they have always printed keep their
#'   positions; the difference between it and `high_intensity` is the care era,
#'   and the difference between it and `moderate_intensity` is the load.
SCENARIO_CARE_PROFILE <- "high_intensity_modern_care"

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
  #' One totals cell: the mean with its interval, and the p10 to p90 band for a count
  #'
  #' @param s One row of `spec`.
  #' @param p The scenario profile.
  #' @return The cell text.
  total_cell <- function(s, p) {
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
  }
  #' One totals cell of the profile that separates the care era from the load
  #'
  #' @param s One row of `spec`.
  #' @return The cell text.
  care_cell <- function(s) total_cell(s, SCENARIO_CARE_PROFILE)
  rows <- lapply(spec, function(s) {
    cells <- vapply(SCENARIO_PROFILES, function(p) total_cell(s, p), character(1))
    a <- pick(s[[2]], SCENARIO_PROFILES[1])$mean
    b <- pick(s[[2]], SCENARIO_PROFILES[2])$mean
    m <- pick(s[[2]], SCENARIO_CARE_PROFILE)$mean
    c(s[[1]], cells, sprintf("%.*f×", s[[6]], b / a), care_cell(s),
      sprintf("%.*f×", s[[6]], m / a))
  })
  res_table(c("Metric", "Moderate intensity", "High intensity", "Ratio",
              "High intensity, modern care", "Ratio, modern care"), rows)
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
    m <- d[d$group == g[2] & d$scenario == SCENARIO_CARE_PROFILE, ]
    care <- sprintf("%s [%s, %s]", res_num(m$mean_q, 3L, big = TRUE),
                    res_num(m$ci_lower, 3L, big = TRUE), res_num(m$ci_upper, 3L, big = TRUE))
    care_ratio <- if (x[[1]]$mean_q == 0) "not applicable" else
      sprintf("%.2f×", m$mean_q / x[[1]]$mean_q)
    c(g[1], cells, ratio, care, care_ratio)
  })
  res_table(c("Resource group", "Moderate intensity mean queue", "High intensity mean queue",
              "Ratio", "High intensity, modern care mean queue", "Ratio, modern care"), rows)
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

#' Length of a stability block and of the closing window, in days
#'
#' @details The block length is the sustained-operations protocol's, so the slope
#'   a stability classification reports per block converts to a rate per day
#'   by dividing by it; the closing window is the one every pool figure of this
#'   paper is measured over.
LOAD_BLOCK_DAYS <- 30L

#' Days of the closing window utilisation is measured over
LOAD_CLOSING_DAYS <- 90L

#' Mean and Student t interval of a sample
#'
#' @param x A numeric vector of per-replication values.
#' @return A named vector of `mean`, `lower` and `upper`; the interval is the
#'   95% Student t one about the mean, and degenerate to the mean for a sample
#'   with no spread.
res_t_interval <- function(x) {
  n <- length(x)
  m <- mean(x)
  half <- if (n > 1L && sd(x) > 0) qt(0.975, n - 1L) * sd(x) / sqrt(n) else 0
  c(mean = m, lower = m - half, upper = m + half)
}

#' A late-campaign slope as a shortfall per day
#'
#' @param slope Slope of the block means over the late half of the campaign, in
#'   casualties per block.
#' @param lower,upper Bounds of the interval on that slope.
#' @param block_days Block length in days.
#' @return A named vector of `mean`, `lower` and `upper` in casualties per day.
#'
#' @details A queue whose block means rise by `slope` casualties from one block
#'   to the next is growing by `slope / block_days` casualties a day, which is
#'   the throughput the pool is short by while it grows. The interval is the
#'   slope's own, scaled by the same divisor.
res_shortfall_per_day <- function(slope, lower, upper, block_days = LOAD_BLOCK_DAYS) {
  c(mean = slope / block_days, lower = lower / block_days, upper = upper / block_days)
}

#' Per-replication mean of a daily series over the closing window
#'
#' @param series The long-horizon daily series restricted to one response.
#' @param closing_days Days in the closing window.
#' @return One mean per replication over the last `closing_days` days of the
#'   campaign.
res_closing_means <- function(series, closing_days = LOAD_CLOSING_DAYS) {
  last <- max(series$day)
  inside <- series[series$day > last - closing_days, ]
  as.numeric(tapply(inside$value, inside$replication, mean))
}

#' Shortfall per day and utilisation of each R2E pool and the evacuation backlog
#'
#' @param data_dir The data directory.
#' @return The table lines.
#'
#' @details The shortfall per day is the late-half slope of the 30-day block means
#'   that the stability classification fits, divided by the block length, with the
#'   slope's own 95% interval. Utilisation is the occupancy averaged over the
#'   closing 90 days within each replication, summarised by a Student t interval
#'   across replications. The strategic evacuation backlog is a count of
#'   casualties awaiting a sortie and has no bed establishment, so it carries no
#'   utilisation.
build_load_shortfall <- function(data_dir) {
  st <- res_read("long_horizon/long_horizon_stability.csv", data_dir)
  series <- res_read("long_horizon/long_horizon_series.csv.gz", data_dir)
  spec <- list(
    list("R2E operating theatres", "mean_queue", "R2E operating theatres", TRUE),
    list("R2E holding beds", "mean_queue", "R2E holding beds", TRUE),
    list("R2E intensive care", "mean_queue", "R2E intensive care", TRUE),
    list("Strategic evacuation backlog", "evac_backlog", "system", FALSE)
  )
  rows <- lapply(spec, function(s) {
    cells <- unlist(lapply(SCENARIO_PROFILES, function(p) {
      r <- st[st$scenario == p & st$series == s[[2]] & st$subject == s[[3]], ]
      if (nrow(r) != 1L) {
        stop(sprintf("expected one stability row for %s %s under %s, found %d", s[[2]], s[[3]],
                     p, nrow(r)), call. = FALSE)
      }
      f <- res_shortfall_per_day(r$slope, r$ci_lower, r$ci_upper)
      f[abs(f) < 0.0005] <- 0
      use <- if (s[[4]]) {
        o <- series[series$scenario == p & series$series == "occupancy" &
                      series$subject == s[[3]], ]
        if (nrow(o) == 0L) stop(sprintf("no occupancy series for %s under %s", s[[3]], p),
                                call. = FALSE)
        u <- res_t_interval(res_closing_means(o))
        res_ci(u[["mean"]], u[["lower"]], min(u[["upper"]], 1), dp = 1L, scale = 100, unit = "%")
      } else "not applicable"
      c(res_ci(f[["mean"]], f[["lower"]], f[["upper"]], dp = 3L), use)
    }))
    c(s[[1]], cells)
  })
  res_table(c("Pool", "Moderate: shortfall per day", "Moderate: utilisation",
              "High: shortfall per day", "High: utilisation"), rows)
}

#' Demand ratios between the two casualty intensities
#'
#' @param data_dir The data directory.
#' @return The table lines: the wounded in action, the first surgeries at R2B and
#'   R2E, and the post-definitive recovery dispositions per campaign at each
#'   intensity, with their ratio.
#'
#' @details The surgeries and dispositions are per-replication counts from the
#'   shipped configuration's arm of the tracked hold window and evacuation policy
#'   evidence, summarised by a Student t interval; the wounded in action are the
#'   comparative scenario analysis's.
build_load_demand <- function(data_dir) {
  tot <- res_read("scenarios/scenario_comparison_totals.csv", data_dir)
  hw <- lapply(c("", "_high_intensity"), function(sfx) {
    r <- res_read(res_file("hold_window/hold_window_replications", sfx), data_dir)
    r <- r[r$window_min == 60, ]
    r$r2b_surgeries + r$r2e_first_surgeries
  })
  pol <- list(res_read("policy/policy_sweep_replications.csv", data_dir),
              res_read("policy/establishment_sweep_high_intensity_replications.csv", data_dir))
  disp <- lapply(pol, function(r) {
    r <- r[r$policy_days == 21 & (is.na(r$hold_beds) | r$hold_beds == 30), ]
    r$dispositions
  })
  wia <- lapply(SCENARIO_PROFILES, function(p) {
    x <- tot[tot$metric == "wia_count" & tot$scenario == p, ]
    c(mean = x$mean, lower = x$ci_lower, upper = x$ci_upper)
  })
  #' One row of the demand table
  #'
  #' @param label Row label.
  #' @param ints List of two interval vectors, moderate then high.
  #' @return The row's four cells.
  one <- function(label, ints) {
    c(label, vapply(ints, function(i) {
      res_ci(i[["mean"]], i[["lower"]], i[["upper"]], dp = 1L, big = TRUE)
    }, character(1)), sprintf("%.2f×", ints[[2]][["mean"]] / ints[[1]][["mean"]]))
  }
  res_table(c("Demand measure", "Moderate intensity", "High intensity", "Ratio"),
            list(one("Wounded in action/run", wia),
                 one("R2B and R2E first surgeries/run", lapply(hw, res_t_interval)),
                 one("Post-definitive recovery dispositions/run",
                     lapply(disp, res_t_interval))))
}

#' Arm that the surgical team sweep's differences are measured against
SURGICAL_TEAM_BASELINE <- "R2E 3, R2B 1"

#' Responses of the surgical team sweep, as `list(label, response, dp, scale)`
#'
#' @details The same set, in the same order, in the summary table and the paired
#'   table, so a reader finds a response in the same row of both. The scale of
#'   100 turns a share into percentage points.
SURGICAL_TEAM_ROWS <- list(
  list("R2E theatre mean queue", "theatre_mean_queue", 2L, 1),
  list("Diverted, team off shift", "diverted_offshift", 1L, 1),
  list("Diverted, theatre busy", "diverted_busy", 1L, 1),
  list("R2B surgeries", "r2b_surgeries", 1L, 1),
  list("R2E first surgeries", "r2e_first_surgeries", 1L, 1),
  list("R2E theatre entry deferred", "r2e_theatre_deferred", 1L, 1),
  list("Released with repair outstanding", "released_unrepaired", 1L, 1),
  list("Died of wounds", "total_dow", 2L, 1),
  list("Returns to duty", "total_rtd", 1L, 1),
  list("Post-definitive ICU access (%)", "post_definitive_icu_share", 1L, 100)
)

#' Surgical team sweep table: each response and the queue's classification, by arm
#'
#' @param data_dir The data directory.
#' @param suffix Scenario suffix of the evidence files, `""` for the shipped
#'   configuration or `"_high_intensity"` for the high-intensity set.
#' @return The table lines, one column per arm.
#'
#' @details The classification and the shortfall per day are those of the R2E
#'   theatre queue's late-half fit, read from the stability file the sweep wrote
#'   from its own daily series, and are printed in the stability table's form
#'   so the two sections state the same verdict in the same words.
build_surgical_team <- function(data_dir, suffix = "") {
  d <- res_read(res_file("surgical_teams/surgical_team_sweep_summary", suffix), data_dir)
  st <- res_read(res_file("surgical_teams/surgical_team_sweep_stability", suffix), data_dir)
  arms <- unique(d$arm)
  #' One response's row across the arms
  #'
  #' @param label Row label.
  #' @param response Response key.
  #' @param dp Decimal places.
  #' @param scale Multiplier, 100 for a share.
  #' @return The row's cells.
  one <- function(label, response, dp, scale) {
    c(label, vapply(arms, function(a) {
      x <- d[d$arm == a & d$response == response, ]
      if (nrow(x) != 1L) stop(sprintf("expected one '%s' row for arm '%s', found %d", response,
                                      a, nrow(x)), call. = FALSE)
      res_ci(x$mean, x$ci_lower, x$ci_upper, dp = dp, scale = scale, floor0 = TRUE)
    }, character(1)))
  }
  #' The queue's classification for one arm, in the stability table's words
  #'
  #' @param a Arm label.
  #' @return The cell text.
  verdict <- function(a) {
    r <- st[st$arm == a, ]
    if (nrow(r) != 1L) stop(sprintf("expected one stability row for arm '%s', found %d", a,
                                    nrow(r)), call. = FALSE)
    if (r$stability == "drifting") {
      sprintf("**drifting, %+.1f%%/block**, %s to %s", 100 * r$drift_per_block,
              formatC(r$first, format = "f", digits = 1, big.mark = ","),
              formatC(r$last, format = "f", digits = 1, big.mark = ","))
    } else {
      sprintf("converged at %s", formatC(r$late_mean, format = "f",
                                         digits = if (abs(r$late_mean) < 10) 2 else 1,
                                         big.mark = ","))
    }
  }
  #' The queue's shortfall per day for one arm, with its interval
  #'
  #' @param a Arm label.
  #' @return The cell text.
  shortfall <- function(a) {
    r <- st[st$arm == a, ]
    f <- c(r$shortfall_per_day, r$shortfall_lower, r$shortfall_upper)
    f[abs(f) < 0.0005] <- 0
    res_ci(f[1], f[2], f[3], dp = 3L)
  }
  rows <- c(
    list(one(SURGICAL_TEAM_ROWS[[1]][[1]], SURGICAL_TEAM_ROWS[[1]][[2]],
             SURGICAL_TEAM_ROWS[[1]][[3]], SURGICAL_TEAM_ROWS[[1]][[4]]),
         c("R2E theatre queue, late-half classification", vapply(arms, verdict, character(1))),
         c("R2E theatre queue shortfall per day", vapply(arms, shortfall, character(1)))),
    lapply(SURGICAL_TEAM_ROWS[-1], function(r) one(r[[1]], r[[2]], r[[3]], r[[4]]))
  )
  header <- c("Response", ifelse(arms == SURGICAL_TEAM_BASELINE, paste(arms, "(shipped)"), arms))
  res_table(header, rows)
}

#' Surgical team sweep table of paired differences against the shipped establishment
#'
#' @param data_dir The data directory.
#' @param suffix Scenario suffix of the evidence files.
#' @return The table lines, one column per arm other than the shipped one.
build_surgical_team_paired <- function(data_dir, suffix = "") {
  d <- res_read(res_file("surgical_teams/surgical_team_sweep_paired", suffix), data_dir)
  arms <- unique(d$to)
  rows <- lapply(SURGICAL_TEAM_ROWS, function(r) {
    c(r[[1]], vapply(arms, function(a) {
      x <- d[d$to == a & d$response == r[[2]], ]
      if (nrow(x) != 1L) stop(sprintf("expected one '%s' paired row for arm '%s', found %d",
                                      r[[2]], a, nrow(x)), call. = FALSE)
      sprintf("%s [%s, %s]", res_num(x$difference * r[[4]], r[[3]], plus = TRUE),
              res_num(x$ci_lower * r[[4]], r[[3]], plus = TRUE),
              res_num(x$ci_upper * r[[4]], r[[3]], plus = TRUE))
    }, character(1)))
  })
  res_table(c("Paired difference against the shipped establishment", arms), rows)
}

#' Arm the intensive care bed sweep's differences are measured against
ICU_BED_BASELINE <- "4 beds"

#' R2E intensive care nurses fielded in total at the shipped three sections
#'
#' @details Three sections of four nurses and an intensivist each, from
#'   `env_data.json`; the ratio of nurses to beds the sweep reports is this
#'   figure over the bed count. It is a staffing cost the model does not charge,
#'   no intensive care stay seizing the section.
ICU_BED_NURSES_TOTAL <- 12

#' Responses of the intensive care bed sweep, as `list(label, response, dp, scale)`
#'
#' @details The same set, in the same order, in the summary table and the paired
#'   table, so a reader finds a response in the same row of both. The scale of
#'   100 turns a share into percentage points.
ICU_BED_ROWS <- list(
  list("R2E ICU mean queue", "icu_mean_queue", 2L, 1),
  list("R2E ICU occupancy (%)", "icu_occupancy", 1L, 100),
  list("R2E holding mean queue", "hold_mean_queue", 2L, 1),
  list("Post-operative care in a holding bed (%)", "postop_degraded_share", 1L, 100),
  list("Post-definitive ICU access (%)", "post_definitive_icu_share", 1L, 100),
  list("R2E theatre entry deferred", "r2e_theatre_deferred", 1L, 1),
  list("Died of wounds", "total_dow", 2L, 1),
  list("Returns to duty", "total_rtd", 1L, 1),
  list("Critical-route evacuation wait (days)", "mean_evac_wait_critical", 2L, 1),
  list("Role 4 peak census", "role4_peak", 1L, 1),
  list("Role 4 closing mean census", "role4_sustained", 1L, 1)
)

#' Intensive care bed sweep table: each response and the queues' classification, by arm
#'
#' @param data_dir The data directory.
#' @param suffix Scenario suffix of the evidence files, `""` for the shipped
#'   configuration or `"_high_intensity"` for the high-intensity set.
#' @return The table lines, one column per arm.
#'
#' @details The classifications and shortfalls per day are those of the R2E
#'   intensive care and holding queues' late-half fits, read from the stability
#'   file the sweep wrote from its own daily series, in the stability table's
#'   words. The nurses per bed row is the shipped total over the bed count.
build_icu_bed <- function(data_dir, suffix = "") {
  d <- res_read(res_file("icu_beds/icu_bed_sweep_summary", suffix), data_dir)
  st <- res_read(res_file("icu_beds/icu_bed_sweep_stability", suffix), data_dir)
  arms <- unique(d$arm)
  #' One response's row across the arms
  #'
  #' @param label Row label.
  #' @param response Response key.
  #' @param dp Decimal places.
  #' @param scale Multiplier, 100 for a share.
  #' @return The row's cells.
  one <- function(label, response, dp, scale) {
    c(label, vapply(arms, function(a) {
      x <- d[d$arm == a & d$response == response, ]
      if (nrow(x) != 1L) stop(sprintf("expected one '%s' row for arm '%s', found %d", response,
                                      a, nrow(x)), call. = FALSE)
      res_ci(x$mean, x$ci_lower, x$ci_upper, dp = dp, scale = scale, floor0 = TRUE)
    }, character(1)))
  }
  #' One pool's stability row for one arm
  #'
  #' @param a Arm label.
  #' @param pool Pool name.
  #' @return The one stability row.
  stab <- function(a, pool) {
    r <- st[st$arm == a & st$pool == pool, ]
    if (nrow(r) != 1L) stop(sprintf("expected one stability row for arm '%s', pool '%s', found %d",
                                    a, pool, nrow(r)), call. = FALSE)
    r
  }
  #' A pool's classification for one arm, in the stability table's words
  #'
  #' @param pool Pool name.
  #' @return A function of the arm label returning the cell text.
  verdict <- function(pool) function(a) {
    r <- stab(a, pool)
    if (r$stability == "drifting") {
      sprintf("**drifting, %+.1f%%/block**, %s to %s", 100 * r$drift_per_block,
              formatC(r$first, format = "f", digits = 1, big.mark = ","),
              formatC(r$last, format = "f", digits = 1, big.mark = ","))
    } else {
      sprintf("converged at %s", formatC(r$late_mean, format = "f",
                                         digits = if (abs(r$late_mean) < 10) 2 else 1,
                                         big.mark = ","))
    }
  }
  #' A pool's shortfall per day for one arm, with its interval
  #'
  #' @param pool Pool name.
  #' @return A function of the arm label returning the cell text.
  shortfall <- function(pool) function(a) {
    r <- stab(a, pool)
    f <- c(r$shortfall_per_day, r$shortfall_lower, r$shortfall_upper)
    f[abs(f) < 0.0005] <- 0
    res_ci(f[1], f[2], f[3], dp = 3L)
  }
  reps <- res_read(res_file("icu_beds/icu_bed_sweep_replications", suffix), data_dir)
  #' Replications in which either R2E pool's closing queue reached the threshold
  #'
  #' @param a Arm label.
  #' @return The count as "k of n".
  collapsed <- function(a) {
    x <- reps[reps$arm == a, ]
    sprintf("%d of %d", sum(x$icu_mean_queue >= RESULTS_QUEUE_THRESHOLD |
                              x$hold_mean_queue >= RESULTS_QUEUE_THRESHOLD, na.rm = TRUE),
            nrow(x))
  }
  beds <- as.numeric(sub(" beds$", "", arms))
  rows <- c(
    list(c("Nurses per ICU bed at three sections", sprintf("%.2f", ICU_BED_NURSES_TOTAL / beds)),
         one(ICU_BED_ROWS[[1]][[1]], ICU_BED_ROWS[[1]][[2]], ICU_BED_ROWS[[1]][[3]],
             ICU_BED_ROWS[[1]][[4]]),
         c("R2E ICU queue, late-half classification",
           vapply(arms, verdict("R2E intensive care"), character(1))),
         c("R2E ICU queue shortfall per day",
           vapply(arms, shortfall("R2E intensive care"), character(1)))),
    lapply(ICU_BED_ROWS[2:3], function(r) one(r[[1]], r[[2]], r[[3]], r[[4]])),
    list(c("R2E holding queue, late-half classification",
           vapply(arms, verdict("R2E holding beds"), character(1))),
         c("R2E holding queue shortfall per day",
           vapply(arms, shortfall("R2E holding beds"), character(1))),
         c("Replications with an R2E pool queue of 20 or more",
           vapply(arms, collapsed, character(1)))),
    lapply(ICU_BED_ROWS[-(1:3)], function(r) one(r[[1]], r[[2]], r[[3]], r[[4]]))
  )
  header <- c("Response", ifelse(arms == ICU_BED_BASELINE, paste(arms, "(shipped)"), arms))
  res_table(header, rows)
}

#' Intensive care bed sweep table of paired differences against the shipped establishment
#'
#' @param data_dir The data directory.
#' @param suffix Scenario suffix of the evidence files.
#' @return The table lines, one column per arm other than the shipped one.
build_icu_bed_paired <- function(data_dir, suffix = "") {
  d <- res_read(res_file("icu_beds/icu_bed_sweep_paired", suffix), data_dir)
  arms <- unique(d$to)
  rows <- lapply(ICU_BED_ROWS, function(r) {
    c(r[[1]], vapply(arms, function(a) {
      x <- d[d$to == a & d$response == r[[2]], ]
      if (nrow(x) != 1L) stop(sprintf("expected one '%s' paired row for arm '%s', found %d",
                                      r[[2]], a, nrow(x)), call. = FALSE)
      sprintf("%s [%s, %s]", res_num(x$difference * r[[4]], r[[3]], plus = TRUE),
              res_num(x$ci_lower * r[[4]], r[[3]], plus = TRUE),
              res_num(x$ci_upper * r[[4]], r[[3]], plus = TRUE))
    }, character(1)))
  })
  res_table(c("Paired difference against the shipped establishment", arms), rows)
}

#' The two R2B holding sweep tables
#'
#' @param data_dir The data directory.
#' @param axis `"beds"` for the establishment axis, `"threshold"` for the threshold axis.
#' @param suffix Scenario suffix of the evidence file, `""` for the shipped configuration.
#' @return The table lines.
build_hold_threshold <- function(data_dir, axis, suffix = "") {
  d <- res_read(res_file("sweeps/r2b_hold_threshold_sweep", suffix), data_dir)
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
#' @param suffix Scenario suffix of the evidence files, `""` for the shipped configuration.
#' @return The table lines.
build_hold_window <- function(data_dir, suffix = "") {
  s <- res_read(res_file("hold_window/hold_window_summary", suffix), data_dir)
  p <- res_read(res_file("hold_window/hold_window_paired", suffix), data_dir)
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
    x <- lapply(c(SCENARIO_PROFILES, SCENARIO_CARE_PROFILE),
                function(p) d[d$holder == h & d$scenario == p, ])
    c(h, if (x[[1]]$kind == "shared") "Shared" else "Integral",
      unlist(lapply(x, function(r) {
        c(res_ci(r$q_mean, r$q_ci_lower, r$q_ci_upper, 3L, floor0 = TRUE), util(r))
      })))
  })
  res_table(c("Holder", "Asset", "Moderate intensity mean queue",
              "Moderate intensity utilisation", "High intensity mean queue",
              "High intensity utilisation", "High intensity, modern care mean queue",
              "High intensity, modern care utilisation"), rows)
}

#' Forward holding frontier table
#'
#' @param data_dir The data directory.
#' @param suffix File suffix selecting the scenario's copy, `"_high_intensity"` for
#'   the high-intensity measurement.
#' @return The table lines.
build_forward_hold <- function(data_dir, suffix = "") {
  d <- res_read(paste0("sweeps/", res_file("r2b_forward_hold_frontier", suffix)), data_dir)
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

#' Closing-window queue below which a pool is counted as not collapsed
#'
#' @details The same twenty casualties the strategic airlift collapse experiment
#'   classifies against (`AIRLIFT_COLLAPSE_THRESHOLD` in `R/airlift.R`), restated
#'   here because this module is independent of every other. A replication whose
#'   closing 90-day mean queue is below it is counted as holding the pool.
RESULTS_QUEUE_THRESHOLD <- 20

#' Evacuation policy by R2E holding establishment grid
#'
#' @param data_dir The data directory.
#' @param stem Stem of the grid's evidence files under `data/policy/`.
#' @return The table lines: one row per policy and establishment.
build_policy_grid <- function(data_dir, stem) {
  d <- res_read(paste0("policy/", stem, ".csv"), data_dir)
  keys <- unique(d[, c("policy_days", "hold_beds")])
  keys <- keys[order(keys$policy_days, keys$hold_beds), ]
  cols <- list(list("hold_mean_queue", 1L), list("icu_mean_queue", 1L),
               list("total_rtd", 1L), list("total_dow", 1L),
               list("never_evacuated", 1L), list("role4_peak", 1L))
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    cells <- vapply(cols, function(cl) {
      x <- res_row(d, cl[[1]], policy_days = keys$policy_days[i], hold_beds = keys$hold_beds[i])
      res_ci(x$mean, x$ci_lower, x$ci_upper, dp = cl[[2]], big = TRUE, floor0 = TRUE)
    }, character(1))
    c(sprintf("%d d, %d beds", keys$policy_days[i], keys$hold_beds[i]), cells)
  })
  res_table(c("Policy and holding beds", "R2E hold mean queue", "R2E ICU mean queue",
              "Returns to duty", "Died of wounds", "Never evacuated by horizon",
              "Role 4 peak beds"), rows)
}

#' Replications holding each R2E pool, by grid cell
#'
#' @param data_dir The data directory.
#' @param stem Stem of the grid's evidence files under `data/policy/`.
#' @return The table lines: for each policy and establishment, the replications of
#'   the campaign's thirty whose closing-window mean queue was below
#'   `RESULTS_QUEUE_THRESHOLD` in the holding pool, the intensive care pool and both.
build_policy_grid_stability <- function(data_dir, stem) {
  r <- res_read(paste0("policy/", stem, "_replications.csv"), data_dir)
  keys <- unique(r[, c("policy_days", "hold_beds")])
  keys <- keys[order(keys$policy_days, keys$hold_beds), ]
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    x <- r[r$policy_days == keys$policy_days[i] & r$hold_beds == keys$hold_beds[i], ]
    hold <- x$hold_mean_queue < RESULTS_QUEUE_THRESHOLD
    icu <- x$icu_mean_queue < RESULTS_QUEUE_THRESHOLD
    c(sprintf("%d d, %d beds", keys$policy_days[i], keys$hold_beds[i]),
      sprintf("%d of %d", sum(hold), nrow(x)), sprintf("%d of %d", sum(icu), nrow(x)),
      sprintf("%d of %d", sum(hold & icu), nrow(x)))
  })
  res_table(c("Policy and holding beds", "Holding queue below threshold",
              "ICU queue below threshold", "Both below threshold"), rows)
}

#' Paired differences against the shipped policy and establishment, by grid cell
#'
#' @param data_dir The data directory.
#' @param stem Stem of the grid's evidence files under `data/policy/`.
#' @return The table lines: for each grid cell other than the baseline, the paired
#'   difference in returns to duty, died of wounds and Role 4 peak beds with its
#'   95% interval.
build_policy_grid_paired <- function(data_dir, stem) {
  d <- res_read(paste0("policy/", stem, "_paired.csv"), data_dir)
  arms <- unique(d$to)
  pol <- as.integer(sub("d_.*", "", arms))
  bed <- as.integer(sub(".*_([0-9]+)b$", "\\1", arms))
  arms <- arms[order(pol, bed)]
  cols <- c("total_rtd", "total_dow", "role4_peak")
  rows <- lapply(arms, function(a) {
    cells <- vapply(cols, function(cl) {
      x <- d[d$to == a & d$response == cl, ]
      stopifnot(nrow(x) == 1L)
      sprintf("%s [%s, %s]", res_num(x$difference, 1L, TRUE, TRUE),
              res_num(x$ci_lower, 1L, TRUE, TRUE), res_num(x$ci_upper, 1L, TRUE, TRUE))
    }, character(1))
    c(sprintf("%s d, %s beds", sub("d_.*", "", a), sub(".*_([0-9]+)b$", "\\1", a)), cells)
  })
  res_table(c("Policy and holding beds", "Returns to duty", "Died of wounds",
              "Role 4 peak beds"), rows)
}

#' Forward surgical saturation release sweep table
#'
#' @param data_dir The data directory.
#' @param suffix Scenario suffix of the evidence file, `""` for the shipped configuration.
#' @return The table lines.
build_saturation <- function(data_dir, suffix = "") {
  d <- res_read(res_file("policy/saturation_sweep", suffix), data_dir)
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
#' @param suffix Scenario suffix of the evidence files, `""` for the shipped configuration.
#' @return The table lines.
build_casualty_surge <- function(data_dir, suffix = "") {
  cnt <- res_read(res_file("casualty_surge/casualty_surge_count_summary", suffix), data_dir)
  dow <- res_read(res_file("casualty_surge/casualty_surge_dow_summary", suffix), data_dir)
  rep <- res_read(res_file("casualty_surge/casualty_surge_replications", suffix), data_dir)
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
#' @param suffix Scenario suffix of the evidence file, `""` for the shipped configuration.
#' @return The table lines.
#'
#' @details One row per swept event size, the no-event arm first. Peak queues
#'   are the largest four-hour mean queue of each pool over the campaign.
build_casualty_surge_size <- function(data_dir, suffix = "") {
  d <- res_read(res_file("casualty_surge/casualty_surge_size_summary", suffix), data_dir)
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
#' @param suffix File suffix selecting the scenario's copy; `"_high_intensity"` reads
#'   the sweeps measured at high intensity, which carry no baseline arm.
#' @return The table lines.
build_airlift <- function(data_dir, which, suffix = "") {
  d <- res_read(paste0("airlift/", res_file("airlift_summary", suffix)), data_dir)
  sweep_scenario <- if (nzchar(suffix)) "high_intensity" else "moderate_intensity"
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
      cols = lapply(c(3, 5, 7, 10, 14), function(v) list("interval", sweep_scenario, v)),
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
        list("reliability", sweep_scenario, v)
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
#' @param suffix Scenario suffix of the evidence files, `""` for the shipped configuration.
#' @return The table lines.
build_icu_gate <- function(data_dir, suffix = "") {
  s <- res_read(res_file("icu_gate/icu_gate_summary", suffix), data_dir)
  p <- res_read(res_file("icu_gate/icu_gate_paired", suffix), data_dir)
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
#' @param suffix Scenario suffix of the evidence file, `""` for the shipped configuration.
#' @return The table lines.
build_airlift_collapse <- function(data_dir, suffix = "") {
  d <- res_read(res_file("airlift/airlift_collapse", suffix), data_dir)
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
#' @param suffix Scenario suffix of the evidence file, `""` for the shipped configuration.
#' @return The table lines: casualty-replications, deaths and the pooled rate for
#'   each pathway in the arm where the rule is in force.
#'
#' @details Pooled over replications rather than averaged per replication, most
#'   replications carrying a handful of deaths on each pathway.
build_icu_gate_pathways <- function(data_dir, suffix = "") {
  r <- res_read(res_file("icu_gate/icu_gate_replications", suffix), data_dir)
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

#' Every paired comparison the resolution tables draw on, read from its evidence
#'
#' @param data_dir The data directory.
#' @return One list per entry of `RESOLUTION_ROWS`, holding the entry (`spec`) and its
#'   single paired-difference row (`hit`).
res_resolution_hits <- function(data_dir) {
  lapply(RESOLUTION_ROWS, function(r) {
    p <- res_read(r[[1]], data_dir)
    hit <- p[p$response == r[[2]] & p$from == r[[4]] & p$to == r[[5]], ]
    if (nrow(hit) != 1L) {
      stop(sprintf("expected one '%s' paired row from %s to %s in %s, found %d", r[[2]], r[[4]],
                   r[[5]], r[[1]], nrow(hit)), call. = FALSE)
    }
    list(spec = r, hit = hit)
  })
}

#' Whether a paired difference's interval contains zero
#'
#' @param hit One paired-difference row.
#' @return TRUE where the 95% interval spans zero, so the difference is not resolved.
res_spans_zero <- function(hit) {
  hit$ci_lower <= 0 && hit$ci_upper >= 0
}

#' A paired difference and its interval as one cell
#'
#' @param hit One paired-difference row.
#' @return The cell text, signed to two decimal places.
res_paired_cell <- function(hit) {
  sprintf("%s [%s, %s]", res_num(hit$difference, 2L, plus = TRUE),
          res_num(hit$ci_lower, 2L, plus = TRUE), res_num(hit$ci_upper, 2L, plus = TRUE))
}

#' Resolution of the paired differences the experiments leave open
#'
#' @param data_dir The data directory.
#' @return The table lines: each difference whose interval contains zero, with the
#'   half-width its experiment sized it against and the replications that half-width
#'   requires.
#'
#' @details A comparison whose interval excludes zero is resolved at the replications
#'   run, so it is left out here and printed by `build_resolved()` instead. The split is
#'   taken from the evidence on every render, so a re-run that resolves a difference
#'   moves it between the tables rather than leaving it listed as open.
build_resolution <- function(data_dir) {
  hits <- Filter(function(h) res_spans_zero(h$hit), res_resolution_hits(data_dir))
  rows <- lapply(hits, function(h) {
    c(h$spec[[7]], res_paired_cell(h$hit), res_num(h$spec[[6]], 1L),
      res_num(h$hit$reps_needed, 0L, big = TRUE))
  })
  res_table(c("Comparison", "Paired difference", "Half-width sought", "Replications needed"),
            rows)
}

#' The paired differences the experiments resolved
#'
#' @param data_dir The data directory.
#' @return The table lines: each comparison of `RESOLUTION_ROWS` whose 95% interval
#'   excludes zero, with its difference and interval.
build_resolved <- function(data_dir) {
  hits <- Filter(function(h) !res_spans_zero(h$hit), res_resolution_hits(data_dir))
  rows <- lapply(hits, function(h) c(h$spec[[7]], res_paired_cell(h$hit)))
  res_table(c("Comparison", "Paired difference"), rows)
}

#' Rows of a printed resolution table whose interval excludes zero
#'
#' @param text A document as one string.
#' @param span The name of the generated span holding the table.
#' @return The comparison labels of every row whose printed interval excludes zero;
#'   empty where every interval contains it. An error where the span is absent.
#'
#' @details Reads the interval as the document prints it rather than from the
#'   evidence, so a row typed or rendered into the table is caught whatever produced it.
res_resolution_excluding_zero <- function(text, span = "resolution") {
  open <- sprintf("<!-- GEN %s -->", span)
  at <- regexpr(open, text, fixed = TRUE)
  if (at < 0L) stop(sprintf("no '%s' span in the document", span), call. = FALSE)
  body <- substr(text, at + nchar(open), nchar(text))
  body <- substr(body, 1L, regexpr("<!-- /GEN -->", body, fixed = TRUE) - 1L)
  lines <- strsplit(trimws(body), "\n")[[1]]
  rows <- lapply(lines[-(1:2)], res_cells)
  #' Parse one printed bound
  #'
  #' @param x The bound as printed, signed with a plus or a minus sign.
  #' @return The bound as a numeric.
  bound <- function(x) as.numeric(gsub(",", "", gsub("\u2212", "-", sub("^\\+", "", x))))
  out <- character(0)
  for (r in rows) {
    ci <- regmatches(r[2], regexec("\\[([^]]+), ([^]]+)\\]", r[2]))[[1]]
    if (length(ci) != 3L || bound(ci[2]) > 0 || bound(ci[3]) < 0) out <- c(out, r[1])
  }
  out
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
  ward_subjects <- list(c("Total", "Total"), c("Intensive care phase", "icu"),
                        c("Step-down ward phase", "hold"))
  origin_subjects <- list(c("Total", "Total"), c("Battle injury", "Battle injury"),
                          c("Disease and non-battle injury", "Disease and non-battle injury"),
                          c("Reconstruction cohort", "Reconstruction cohort"))
  subjects <- if (which == "ward") ward_subjects else origin_subjects
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
    list("Operations owed within the 360 days", "operations", "Total", "total", 0L),
    list("Definitive repairs owed within the 360 days", "operations", "Definitive repair",
         "total", 0L),
    list("Debridements owed within the 360 days", "operations", "Debridement", "total", 0L),
    list("Reconstructions owed within the 360 days", "operations", "Reconstruction", "total",
         0L),
    list("Operations owed after day 360", "operations_after_horizon", "Total", "total", 0L),
    list("Operations owed by casualties admitted during the campaign", "operations_admitted",
         "Total", "total", 0L),
    list("Operations owed on the busiest day", "operations", "Total", "peak", 2L),
    list("Closing 90-day mean operations owed per day", "operations", "Total", "closing_mean",
         2L),
    list("Theatre minutes owed for definitive repairs", "theatre_minutes", "Total", "total", 0L)
  )
  rows <- lapply(spec, function(r) {
    c(r[[1]], vapply(names(ROLE4_INTENSITY_HEADINGS), function(sc) {
      x <- d[d$scenario == sc & d$series == r[[2]] & d$subject == r[[3]] &
               d$response == r[[4]], ]
      stopifnot(nrow(x) == 1L)
      res_ci(x$mean, x$ci_lower, x$ci_upper, dp = r[[5]], big = TRUE, floor0 = TRUE)
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
  sweeps <- list(
    policy = list(file = "policy/policy_sweep.csv", key = "policy_days",
                  header = c("Response", "15 d", "21 d (shipped)", "30 d", "45 d", "60 d"),
                  values = c(15, 21, 30, 45, 60)),
    establishment = list(file = "policy/establishment_sweep.csv", key = "hold_beds",
                         header = c("Response", "30 beds (shipped)", "45 beds", "60 beds",
                                    "90 beds"),
                         values = c(30, 45, 60, 90)),
    saturation = list(file = "policy/saturation_sweep.csv", key = "saturation_threshold",
                      header = c("Response", "0 (disabled)", "1", "2", "3", "5", "8 (shipped)",
                                 "12", "16", "24"),
                      values = c(0, 1, 2, 3, 5, 8, 12, 16, 24))
  )
  sweep <- sweeps[[which]]
  if (is.null(sweep)) stop(sprintf("no Role 4 lever table named '%s'", which), call. = FALSE)
  arms <- lapply(sweep$values, function(v) setNames(list(v), sweep$key))
  res_sweep_table(res_read(sweep$file, data_dir), sweep$header, arms, ROLE4_LEVER_ROWS)
}

#' Closing-window utilisation at or above which a pool counts as marginal
#'
#' @details An informed estimate rather than a sourced value. A queue's wait grows
#'   steeply as utilisation approaches one, so a pool used for more than 85% of the
#'   closing 90 days has too little headroom to absorb a heavy day.
ADEQUACY_UTIL_MARGINAL <- 0.85

#' Share of the campaign a queue must stand empty for a pool to count as adequate
#'
#' @details An informed estimate. A queue occupied for more than half the campaign
#'   leaves no headroom in which it clears, whatever its trend.
ADEQUACY_EMPTY_MARGINAL <- 0.5

#' Utilisation below which an element with no queue counts as over-provisioned
#'
#' @details An informed estimate, set low because utilisation is a time average and
#'   an element needs slack to answer a surge.
ADEQUACY_UTIL_OVER <- 0.10

#' Share of the campaign a queue must stand empty for an element to count as over-provisioned
ADEQUACY_EMPTY_OVER <- 0.95

#' Mean queue at or above which an element counts as marginal
ADEQUACY_QUEUE_MARGINAL <- 0.25

#' Mean queue at or above which an element counts as inadequate
#'
#' @details One casualty waiting on average.
ADEQUACY_QUEUE_INADEQUATE <- 1

#' Mean queue below which an element with low utilisation counts as over-provisioned
ADEQUACY_QUEUE_OVER <- 0.05

#' Share of casualties requiring surgery routed past R2B at or above which it is marginal
ADEQUACY_EXPORT_MARGINAL <- 0.25

#' Share of casualties requiring surgery routed past R2B at or above which it is inadequate
ADEQUACY_EXPORT_INADEQUATE <- 0.5

#' Mean strategic evacuation backlog at or above which the airlift counts as marginal
ADEQUACY_BACKLOG_MARGINAL <- 1

#' Casualties one sortie of the shipped airframe carries
#'
#' @details The critical and standard places of the C-17A in `env_data.json`.
ADEQUACY_PLACES_PER_SORTIE <- 90

#' Days between strategic evacuation sorties in the shipped configuration
ADEQUACY_SORTIE_INTERVAL_DAYS <- 7

#' R2E surgical sections in the shipped establishment
ADEQUACY_R2E_SECTIONS <- 3

#' Days of the campaign the throughput estimates are taken over
ADEQUACY_CAMPAIGN_DAYS <- 360

#' Verdict for a pool that carries a queue, a trend and a utilisation
#'
#' @param drifting Whether the pool's queue is classified drifting upward.
#' @param util Closing-window utilisation as a fraction.
#' @param empty Share of the campaign the queue stands empty, as a fraction.
#' @return One of `Inadequate`, `Marginal`, `Over-provisioned` and `Adequate`.
#'
#' @details Criteria, in order: a drifting queue is inadequate; a pool used at or
#'   above `ADEQUACY_UTIL_MARGINAL`, or whose queue stands empty for less than
#'   `ADEQUACY_EMPTY_MARGINAL` of the campaign, is marginal; a pool used below
#'   `ADEQUACY_UTIL_OVER` whose queue stands empty for at least
#'   `ADEQUACY_EMPTY_OVER` is over-provisioned; any other pool is adequate.
adequacy_pool_verdict <- function(drifting, util, empty) {
  if (drifting) return("Inadequate")
  if (util >= ADEQUACY_UTIL_MARGINAL || empty < ADEQUACY_EMPTY_MARGINAL) return("Marginal")
  if (util < ADEQUACY_UTIL_OVER && empty >= ADEQUACY_EMPTY_OVER) return("Over-provisioned")
  "Adequate"
}

#' Verdict for an element measured by a mean queue and, where it has one, a utilisation
#'
#' @param queue Mean queue.
#' @param util Utilisation as a fraction, or `NA` where the element has none.
#' @return One of `Inadequate`, `Marginal`, `Over-provisioned` and `Adequate`.
#'
#' @details A mean queue at or above `ADEQUACY_QUEUE_INADEQUATE` is inadequate; one
#'   at or above `ADEQUACY_QUEUE_MARGINAL`, or a utilisation at or above
#'   `ADEQUACY_UTIL_MARGINAL`, is marginal; a utilisation below `ADEQUACY_UTIL_OVER`
#'   with a queue below `ADEQUACY_QUEUE_OVER` is over-provisioned.
adequacy_queue_verdict <- function(queue, util = NA_real_) {
  if (queue >= ADEQUACY_QUEUE_INADEQUATE) return("Inadequate")
  if (queue >= ADEQUACY_QUEUE_MARGINAL || (!is.na(util) && util >= ADEQUACY_UTIL_MARGINAL)) {
    return("Marginal")
  }
  if (!is.na(util) && util < ADEQUACY_UTIL_OVER && queue < ADEQUACY_QUEUE_OVER) {
    return("Over-provisioned")
  }
  "Adequate"
}

#' Verdict for R2B surgery from the share of casualties routed past it
#'
#' @param export Share of casualties requiring surgery diverted away from R2B, as a
#'   fraction.
#' @param util Closing-window utilisation of the R2B theatres as a fraction.
#' @return One of `Inadequate`, `Marginal`, `Over-provisioned` and `Adequate`.
#'
#' @details R2B surgery never queues, because the routing policy sends a casualty
#'   rearward when the theatre or its team is unavailable, so the shortfall shows
#'   as casualties exported. At or above `ADEQUACY_EXPORT_INADEQUATE` is
#'   inadequate; at or above `ADEQUACY_EXPORT_MARGINAL` is marginal; below
#'   `ADEQUACY_EXPORT_MARGINAL` with utilisation under `ADEQUACY_UTIL_OVER` is
#'   over-provisioned.
adequacy_export_verdict <- function(export, util) {
  if (export >= ADEQUACY_EXPORT_INADEQUATE) return("Inadequate")
  if (export >= ADEQUACY_EXPORT_MARGINAL) return("Marginal")
  if (util < ADEQUACY_UTIL_OVER) return("Over-provisioned")
  "Adequate"
}

#' Verdict for the strategic evacuation backlog
#'
#' @param drifting Whether the backlog is classified drifting upward.
#' @param level Mean backlog over the late half of the campaign.
#' @return One of `Inadequate`, `Marginal` and `Adequate`.
#'
#' @details A drifting backlog is inadequate; a settled one at or above
#'   `ADEQUACY_BACKLOG_MARGINAL` casualties is marginal; any other is adequate.
adequacy_backlog_verdict <- function(drifting, level) {
  if (drifting) return("Inadequate")
  if (level >= ADEQUACY_BACKLOG_MARGINAL) return("Marginal")
  "Adequate"
}

#' Profile key and queue-clearance label of each intensity the matrix reports
ADEQUACY_INTENSITIES <- list(
  list(profile = "moderate_intensity", label = "Moderate intensity", suffix = ""),
  list(profile = "high_intensity", label = "High intensity", suffix = "_high_intensity")
)

#' A verdict and its evidence as one matrix cell
#'
#' @param verdict The verdict.
#' @param evidence The evidence text.
#' @return The cell text.
adequacy_cell <- function(verdict, evidence) sprintf("**%s**: %s", verdict, evidence)

#' Percentage with one decimal
#'
#' @param x A fraction.
#' @return The percentage text.
adequacy_pct <- function(x) sprintf("%.1f%%", 100 * x)

#' Adequacy verdict of a bed or theatre pool at one intensity
#'
#' @param data_dir The data directory.
#' @param pool Pool name as the long-horizon series and queue clearance files print it.
#' @param int One entry of `ADEQUACY_INTENSITIES`.
#' @return The matrix cell.
adequacy_pool_cell <- function(data_dir, pool, int) {
  st <- res_read("long_horizon/long_horizon_stability.csv", data_dir)
  r <- st[st$scenario == int$profile & st$series == "mean_queue" & st$subject == pool, ]
  if (nrow(r) != 1L) {
    stop(sprintf("expected one stability row for %s under %s, found %d", pool, int$profile,
                 nrow(r)), call. = FALSE)
  }
  series <- res_read("long_horizon/long_horizon_series.csv.gz", data_dir)
  o <- series[series$scenario == int$profile & series$series == "occupancy" &
                series$subject == pool, ]
  util <- mean(res_closing_means(o))
  qc <- res_read("time_series/queue_clearance.csv", data_dir)
  empty <- mean(qc$zero_share[qc$pool == pool & qc$intensity == int$label])
  drifting <- identical(r$stability, "drifting") && r$drift_per_block > 0
  trend <- if (drifting) sprintf("drifting, %+.1f%%/block", 100 * r$drift_per_block) else "settled"
  adequacy_cell(adequacy_pool_verdict(drifting, util, empty),
                sprintf("queue %s, used %s, queue empty %s", trend, adequacy_pct(util),
                        adequacy_pct(empty)))
}

#' Adequacy verdict of a transport holder at one intensity
#'
#' @param data_dir The data directory.
#' @param holders The holder names whose worst case decides the verdict.
#' @param int One entry of `ADEQUACY_INTENSITIES`.
#' @return The matrix cell.
adequacy_holder_cell <- function(data_dir, holders, int) {
  d <- res_read("scenarios/scenario_transport_holders.csv", data_dir)
  d <- d[d$scenario == int$profile & d$holder %in% holders, ]
  if (nrow(d) != length(holders)) {
    stop(sprintf("expected %d holder rows under %s, found %d", length(holders), int$profile,
                 nrow(d)), call. = FALSE)
  }
  verdicts <- mapply(adequacy_queue_verdict, d$q_mean, d$util_mean)
  order <- c("Inadequate", "Marginal", "Adequate", "Over-provisioned")
  worst <- order[min(match(verdicts, order))]
  adequacy_cell(worst, sprintf("used %s, mean queue %s", adequacy_pct(max(d$util_mean)),
                               res_num(max(d$q_mean), 2L)))
}

#' Adequacy verdict of the R1 staff at one intensity
#'
#' @param data_dir The data directory.
#' @param int One entry of `ADEQUACY_INTENSITIES`.
#' @return The matrix cell, decided by the longest mean queue of any R1 staff resource.
adequacy_r1_cell <- function(data_dir, int) {
  d <- res_read("scenarios/scenario_comparison_queues.csv", data_dir)
  d <- d[d$scenario == int$profile & grepl("^c_r1_", d$resource), ]
  if (nrow(d) == 0L) stop(sprintf("no R1 staff queues under %s", int$profile), call. = FALSE)
  q <- max(d$mean_q)
  adequacy_cell(adequacy_queue_verdict(q), sprintf("longest R1 staff queue %s", res_num(q, 2L)))
}

#' Adequacy verdict of R2B surgery at one intensity
#'
#' @param data_dir The data directory.
#' @param int One entry of `ADEQUACY_INTENSITIES`.
#' @return The matrix cell, decided by the share of casualties requiring surgery routed
#'   past R2B and the R2B theatres' utilisation.
adequacy_r2b_surgery_cell <- function(data_dir, int) {
  hw <- res_read(res_file("hold_window/hold_window_replications", int$suffix), data_dir)
  hw <- hw[hw$window_min == 60, ]
  diverted <- hw$diverted_offshift + hw$diverted_busy
  export <- mean(diverted / (diverted + hw$r2b_surgeries))
  series <- res_read("long_horizon/long_horizon_series.csv.gz", data_dir)
  o <- series[series$scenario == int$profile & series$series == "occupancy" &
                series$subject == "R2B operating theatres", ]
  util <- mean(res_closing_means(o))
  adequacy_cell(adequacy_export_verdict(export, util),
                sprintf("%s of casualties requiring surgery routed past, theatres used %s",
                        adequacy_pct(export), adequacy_pct(util)))
}

#' Adequacy verdict of the strategic airlift at one intensity
#'
#' @param data_dir The data directory.
#' @param int One entry of `ADEQUACY_INTENSITIES`.
#' @return The matrix cell, decided by the evacuation backlog's classification and level.
adequacy_airlift_cell <- function(data_dir, int) {
  st <- res_read("long_horizon/long_horizon_stability.csv", data_dir)
  r <- st[st$scenario == int$profile & st$series == "evac_backlog", ]
  if (nrow(r) != 1L) {
    stop(sprintf("expected one backlog row under %s, found %d", int$profile, nrow(r)),
         call. = FALSE)
  }
  drifting <- identical(r$stability, "drifting") && r$drift_per_block > 0
  trend <- if (drifting) sprintf("drifting, %+.1f%%/block", 100 * r$drift_per_block) else "settled"
  adequacy_cell(adequacy_backlog_verdict(drifting, r$late_mean),
                sprintf("backlog %s, late-half mean %s awaiting a sortie", trend,
                        res_num(r$late_mean, 1L)))
}

#' Role 4 cell: demand stated, adequacy not assessed
#'
#' @param data_dir The data directory.
#' @param int One entry of `ADEQUACY_INTENSITIES`.
#' @return The matrix cell: the model sets the demand on the national support base and
#'   gives it no capacity, so no verdict is possible.
adequacy_role4_cell <- function(data_dir, int) {
  d <- res_read("role4_demand/role4_demand_summary.csv", data_dir)
  d <- d[d$scenario == int$profile & d$series == "census" & d$subject == "Total" &
           d$response == "peak", ]
  total <- d$mean
  sprintf("**Not assessed**: demand only, peak census %s beds", res_num(total, 0L, big = TRUE))
}

#' Integral evacuation holders whose worst verdict the matrix reports
INTEGRAL_HOLDERS <- c("R2B evacuation crews", "R2E evacuation sections")

#' Adequacy matrix: a verdict and its evidence for each element at each intensity
#'
#' @param data_dir The data directory.
#' @return The table lines.
#'
#' @details The criteria are the `ADEQUACY_*` constants above, stated in
#'   docs/Methods.md and asserted equal to its marker comments by
#'   scripts/check_planning_implications.R. Role 4 carries no verdict because the
#'   model reports the demand placed on the national support base and assigns it no
#'   capacity.
build_adequacy_matrix <- function(data_dir) {
  #' One cell per intensity
  #'
  #' @param f A function of one intensity returning a cell.
  #' @return The two cells.
  cells <- function(f) vapply(ADEQUACY_INTENSITIES, f, character(1))
  #' One pool's cells
  #'
  #' @param name The pool name.
  #' @return The two cells.
  pool <- function(name) cells(function(i) adequacy_pool_cell(data_dir, name, i))
  rows <- list(
    c("R1", cells(function(i) adequacy_r1_cell(data_dir, i))),
    c("Shared transport fleets",
      cells(function(i) adequacy_holder_cell(data_dir, c("PMV Ambulance fleet", "HX2 40M fleet"),
                                             i))),
    c("Integral evacuation elements",
      cells(function(i) adequacy_holder_cell(data_dir, INTEGRAL_HOLDERS, i))),
    c("R2B surgery", cells(function(i) adequacy_r2b_surgery_cell(data_dir, i))),
    c("R2B holding", pool("R2B holding beds")),
    c("R2E theatres and surgical teams", pool("R2E operating theatres")),
    c("R2E intensive care", pool("R2E intensive care")),
    c("R2E holding", pool("R2E holding beds")),
    c("Strategic airlift", cells(function(i) adequacy_airlift_cell(data_dir, i))),
    c("Role 4", cells(function(i) adequacy_role4_cell(data_dir, i)))
  )
  res_table(c("Element", "Moderate intensity", "High intensity"), rows)
}

#' Zero crossing of a straight line through a sweep's shortfall per day
#'
#' @param units Swept unit counts.
#' @param shortfall Shortfall per day at each count.
#' @return The count at which the fitted line reaches zero, or `NA` when the fitted
#'   shortfall does not fall as units are added.
adequacy_zero_crossing <- function(units, shortfall) {
  fit <- stats::lm(shortfall ~ units)
  b <- unname(stats::coef(fit))
  if (!is.finite(b[2]) || b[2] >= 0) return(NA_real_)
  -b[1] / b[2]
}

#' Late-campaign shortfall per day of one pool at one intensity
#'
#' @param data_dir The data directory.
#' @param series,subject The stability file's series and subject keys.
#' @param int One entry of `ADEQUACY_INTENSITIES`.
#' @return The slope of the late-half block means divided by the block length.
adequacy_shortfall <- function(data_dir, series, subject, int) {
  st <- res_read("long_horizon/long_horizon_stability.csv", data_dir)
  r <- st[st$scenario == int$profile & st$series == series & st$subject == subject, ]
  if (nrow(r) != 1L) {
    stop(sprintf("expected one stability row for %s %s under %s, found %d", series, subject,
                 int$profile, nrow(r)), call. = FALSE)
  }
  r$slope / LOAD_BLOCK_DAYS
}

#' Sweep result text for a swept establishment
#'
#' @param st The sweep's stability file, with `arm`, `shortfall_per_day` and `stability`.
#' @param units Swept unit count of each row of `st`, in row order.
#' @param noun The unit name.
#' @return Text stating the shortfall per day across the sweep, whether any count settles,
#'   and the count at which a straight line through the shortfalls reaches zero.
adequacy_sweep_text <- function(st, units, noun) {
  settled <- any(st$stability != "drifting")
  zero <- adequacy_zero_crossing(units, st$shortfall_per_day)
  sprintf("Measured: %s per day from %s at %s %s to %s at %s; %s; line reaches zero near %s",
          "shortfall", res_num(st$shortfall_per_day[1], 2L), res_num(units[1], 0L), noun,
          res_num(st$shortfall_per_day[length(units)], 2L), res_num(units[length(units)], 0L),
          if (settled) "a count settles" else "no tested count settles",
          if (is.na(zero)) "never" else sprintf("%s %s", res_num(zero, 0L), noun))
}

#' Sizing rows for the elements the matrix finds inadequate
#'
#' @param data_dir The data directory.
#' @return The table lines.
#'
#' @details Each row is an element and intensity whose matrix verdict is
#'   `Inadequate`; a matrix cell with no sizing rule stops the build so a new verdict
#'   cannot go unsized. The shortfall per day is the late-half slope of the block
#'   means over the block length, or for R2B surgery the casualties routed past per
#'   day. The estimated uplift divides it by the delivered throughput per day, as a
#'   share of today's throughput and, where the establishment is a count of sections
#'   or sorties, as extra units. The sweep column states what the sweeps measured,
#'   or the measurement that remains to be made.
build_adequacy_sizing <- function(data_dir) {
  rows <- list()
  for (int in ADEQUACY_INTENSITIES) {
    sfx <- int$suffix
    hw <- res_read(res_file("hold_window/hold_window_replications", sfx), data_dir)
    hw <- hw[hw$window_min == 60, ]
    sr <- res_read(res_file("surgical_teams/surgical_team_sweep_replications", sfx), data_dir)
    st_s <- res_read(res_file("surgical_teams/surgical_team_sweep_stability", sfx), data_dir)
    #' Case label of a sizing row
    #'
    #' @param element The element name.
    #' @return The element with this intensity.
    case <- function(element) paste0(element, ", ", tolower(int$label))
    #' The verdict a matrix cell opens with
    #'
    #' @param txt The cell text.
    #' @return The verdict.
    verdict <- function(txt) sub("^\\*\\*([^*]+)\\*\\*.*$", "\\1", txt)
    if (verdict(adequacy_r2b_surgery_cell(data_dir, int)) == "Inadequate") {
      exported <- mean(hw$diverted_offshift + hw$diverted_busy) / ADEQUACY_CAMPAIGN_DAYS
      delivered <- mean(hw$r2b_surgeries) / ADEQUACY_CAMPAIGN_DAYS
      two <- sr[sr$arm == "R2E 3, R2B 2", ]
      one <- sr[sr$arm == "R2E 3, R2B 1", ]
      sweep <- sprintf(paste("Measured: casualties routed past per run fall from %s to %s",
                             "with a second section per unit"),
                       res_num(mean(one$diverted_offshift + one$diverted_busy), 0L, big = TRUE),
                       res_num(mean(two$diverted_offshift + two$diverted_busy), 0L, big = TRUE))
      rows[[length(rows) + 1L]] <- c(
        case("R2B surgery"), res_num(exported, 2L),
        res_num(delivered, 2L),
        sprintf("%s of today's output", adequacy_pct(exported / delivered)), sweep
      )
    }
    if (verdict(adequacy_pool_cell(data_dir, "R2E operating theatres", int)) == "Inadequate") {
      short <- adequacy_shortfall(data_dir, "mean_queue", "R2E operating theatres", int)
      base <- sr[sr$arm == "R2E 3, R2B 1", ]
      delivered <- mean(base$r2e_first_surgeries) / ADEQUACY_CAMPAIGN_DAYS
      arms <- st_s[grepl("^R2E [0-9], R2B 1$", st_s$arm), ]
      units <- as.numeric(sub("^R2E ([0-9]).*$", "\\1", arms$arm))
      rows[[length(rows) + 1L]] <- c(
        case("R2E theatres and surgical teams"),
        res_num(short, 2L), res_num(delivered, 2L),
        sprintf("%s of today's output, %s extra sections", adequacy_pct(short / delivered),
                res_num(ADEQUACY_R2E_SECTIONS * short / delivered, 1L)),
        adequacy_sweep_text(arms, units, "sections")
      )
    }
    if (verdict(adequacy_pool_cell(data_dir, "R2E intensive care", int)) == "Inadequate") {
      short <- adequacy_shortfall(data_dir, "mean_queue", "R2E intensive care", int)
      ib <- res_read(res_file("icu_beds/icu_bed_sweep_stability", sfx), data_dir)
      ib <- ib[ib$pool == "R2E intensive care", ]
      units <- as.numeric(sub("^([0-9]+) beds$", "\\1", ib$arm))
      rows[[length(rows) + 1L]] <- c(
        case("R2E intensive care"), res_num(short, 2L),
        "not measured per bed", "not estimable from a per-bed throughput; see sweep",
        adequacy_sweep_text(ib, units, "beds")
      )
    }
    if (verdict(adequacy_pool_cell(data_dir, "R2E holding beds", int)) == "Inadequate") {
      short <- adequacy_shortfall(data_dir, "mean_queue", "R2E holding beds", int)
      rows[[length(rows) + 1L]] <- c(
        case("R2E holding"), res_num(short, 2L),
        "not measured per bed", "not estimable from a per-bed throughput",
        "Untested: the holding establishment by policy grid at this intensity awaits measurement"
      )
    }
    if (verdict(adequacy_airlift_cell(data_dir, int)) == "Inadequate") {
      short <- adequacy_shortfall(data_dir, "evac_backlog", "system", int)
      per_stream <- ADEQUACY_PLACES_PER_SORTIE / ADEQUACY_SORTIE_INTERVAL_DAYS
      rows[[length(rows) + 1L]] <- c(
        case("Strategic airlift"), res_num(short, 2L),
        res_num(per_stream, 2L),
        sprintf("%s extra sortie streams of one sortie per interval",
                res_num(short / per_stream, 1L)),
        "Untested: the sortie interval, policy and holding grid awaits measurement here"
      )
    }
  }
  res_table(c("Case", "Shortfall per day", "Delivered per day", "Estimated uplift",
              "Sweep evidence"), rows)
}

#' Evacuation policy trade: the outcome, stability and cost rows of the policy sweep
#'
#' @param data_dir The data directory.
#' @return The table lines: the policy sweep restricted to returns to duty, deaths
#'   of wounds, the in-theatre share, R2E occupancy and post-definitive intensive
#'   care access, the casualties never evacuated and the Role 4 peak beds, one
#'   column per policy.
build_policy_trade <- function(data_dir) {
  lines <- build_policy(data_dir)
  keep <- c("Returns to duty", "Died of wounds", "In-theatre share (%)",
            "R2E hold occupancy (%)", "R2E ICU occupancy (%)", "Post-definitive ICU access (%)",
            "Never evacuated by horizon", "Role 4 peak beds")
  labels <- vapply(lines[-(1:2)], function(l) res_cells(l)[1], character(1))
  rows <- lines[-(1:2)][match(keep, labels)]
  if (anyNA(rows)) stop("the policy sweep lacks a row the trade table prints", call. = FALSE)
  c(lines[1:2], rows)
}

#' The grid at high intensity, as `build_policy_grid()` reads it
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_policy_establishment_high <- function(data_dir) {
  build_policy_grid(data_dir, "establishment_sweep_high_intensity")
}

#' Stability counts of the high-intensity grid
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_policy_establishment_high_stability <- function(data_dir) {
  build_policy_grid_stability(data_dir, "establishment_sweep_high_intensity")
}

#' Paired differences of the high-intensity grid
#'
#' @param data_dir The data directory.
#' @return The table lines.
build_policy_establishment_high_paired <- function(data_dir) {
  build_policy_grid_paired(data_dir, "establishment_sweep_high_intensity")
}

#' Stem of the moderate-intensity grid's evidence files
POLICY_GRID_MODERATE_STEM <- "establishment_sweep_moderate_grid"

#' Largest policy that holds both R2E pools in every replication, by establishment
#'
#' @param data_dir The data directory.
#' @param stem Stem of the grid's evidence files under `data/policy/`.
#' @return The table lines: for each establishment, the largest swept policy at
#'   which the closing-window queue of both R2E pools was below
#'   `RESULTS_QUEUE_THRESHOLD` in every replication, the returns to duty at that
#'   policy, the paired gain over the shipped policy and establishment and the gain
#'   per bed added beyond the shipped establishment.
#'
#' @details A policy is stable at an establishment when no replication of the
#'   cell exceeds the threshold in either pool; the largest stable policy is the
#'   greatest swept value for which that holds, so a stable value beyond an
#'   unstable one is not skipped to.
build_policy_grid_largest_stable <- function(data_dir, stem) {
  r <- res_read(paste0("policy/", stem, "_replications.csv"), data_dir)
  s <- res_read(paste0("policy/", stem, ".csv"), data_dir)
  p <- res_read(paste0("policy/", stem, "_paired.csv"), data_dir)
  base_beds <- POLICY_GRID_BASELINE_BEDS
  rows <- lapply(sort(unique(r$hold_beds)), function(b) {
    cell <- r[r$hold_beds == b, ]
    stable <- vapply(sort(unique(cell$policy_days)), function(d) {
      x <- cell[cell$policy_days == d, ]
      all(x$hold_mean_queue < RESULTS_QUEUE_THRESHOLD & x$icu_mean_queue < RESULTS_QUEUE_THRESHOLD)
    }, logical(1))
    days <- sort(unique(cell$policy_days))
    if (!any(stable)) return(c(as.character(b), "none in the grid", "not applicable",
                               "not applicable", "not applicable"))
    best <- max(days[stable])
    rtd <- res_row(s, "total_rtd", policy_days = best, hold_beds = b)
    arm <- sprintf("%dd_%db", best, b)
    gain <- if (arm == sprintf("%dd_%db", POLICY_GRID_BASELINE_DAYS, base_beds)) {
      "0 (the shipped setting)"
    } else {
      g <- p[p$to == arm & p$response == "total_rtd", ]
      sprintf("%s [%s, %s]", res_num(g$difference, 1L, TRUE, TRUE),
              res_num(g$ci_lower, 1L, TRUE, TRUE), res_num(g$ci_upper, 1L, TRUE, TRUE))
    }
    is_baseline <- arm == sprintf("%dd_%db", POLICY_GRID_BASELINE_DAYS, base_beds)
    per_bed <- if (b > base_beds && !is_baseline) {
      g <- p[p$to == arm & p$response == "total_rtd", ]
      res_num(g$difference / (b - base_beds), 1L, FALSE, TRUE)
    } else "not applicable"
    c(as.character(b), sprintf("%d d", best),
      res_ci(rtd$mean, rtd$ci_lower, rtd$ci_upper, dp = 1L, big = TRUE), gain, per_bed)
  })
  res_table(c("Holding beds", "Largest policy holding both pools in every replication",
              "Returns to duty there", "Paired gain over 21 d and 30 beds",
              "Gain per added bed"), rows)
}

#' Policy of the grid's paired baseline arm, in days
POLICY_GRID_BASELINE_DAYS <- 21L

#' Holding establishment of the grid's paired baseline arm, in beds
POLICY_GRID_BASELINE_BEDS <- 30L

#' Tables registered for the planning paper alone
#'
#' @details The adequacy matrix and its sizing table interpret the results paper's
#'   measurements, so the results paper does not print them; the planning paper does.
PLANNING_ONLY_TABLES <- c("adequacy_matrix", "adequacy_sizing", "policy_trade")

#' Registry of generated tables
#'
#' @details Each entry is a function of the data directory returning the lines
#'   of one table. The names are the ones a `GEN` marker in docs/Results.md
#'   uses.
RESULTS_TABLES <- list(
  scenario_totals = build_scenario_totals,
  scenario_queue = build_scenario_queue,
  long_horizon_stability = build_long_horizon_stability,
  load_demand = build_load_demand,
  load_shortfall = build_load_shortfall,
  hold_threshold_beds = function(dd) build_hold_threshold(dd, "beds"),
  hold_threshold_threshold = function(dd) build_hold_threshold(dd, "threshold"),
  hold_window = build_hold_window,
  hold_window_high = function(dd) build_hold_window(dd, "_high_intensity"),
  hold_threshold_beds_high = function(dd) build_hold_threshold(dd, "beds", "_high_intensity"),
  hold_threshold_threshold_high = function(dd) {
    build_hold_threshold(dd, "threshold", "_high_intensity")
  },
  saturation_high = function(dd) build_saturation(dd, "_high_intensity"),
  casualty_surge_high = function(dd) build_casualty_surge(dd, "_high_intensity"),
  casualty_surge_size_high = function(dd) build_casualty_surge_size(dd, "_high_intensity"),
  airlift_collapse_high = function(dd) build_airlift_collapse(dd, "_high_intensity"),
  icu_gate_high = function(dd) build_icu_gate(dd, "_high_intensity"),
  icu_gate_pathways_high = function(dd) build_icu_gate_pathways(dd, "_high_intensity"),
  transport = function(dd) build_transport(dd, FALSE),
  transport_high = function(dd) build_transport(dd, TRUE),
  transport_holders = build_transport_holders,
  forward_hold = build_forward_hold,
  policy = build_policy,
  establishment = build_establishment,
  surgical_team = build_surgical_team,
  surgical_team_high = function(dd) build_surgical_team(dd, "_high_intensity"),
  surgical_team_paired = build_surgical_team_paired,
  surgical_team_paired_high = function(dd) build_surgical_team_paired(dd, "_high_intensity"),
  icu_bed = build_icu_bed,
  icu_bed_high = function(dd) build_icu_bed(dd, "_high_intensity"),
  icu_bed_paired = build_icu_bed_paired,
  icu_bed_paired_high = function(dd) build_icu_bed_paired(dd, "_high_intensity"),
  policy_establishment_high = build_policy_establishment_high,
  policy_establishment_high_stability = build_policy_establishment_high_stability,
  policy_establishment_high_paired = build_policy_establishment_high_paired,
  policy_grid_largest_high = function(dd) {
    build_policy_grid_largest_stable(dd, "establishment_sweep_high_intensity")
  },
  policy_establishment_moderate = function(dd) build_policy_grid(dd, POLICY_GRID_MODERATE_STEM),
  policy_establishment_moderate_stability = function(dd) {
    build_policy_grid_stability(dd, POLICY_GRID_MODERATE_STEM)
  },
  policy_establishment_moderate_paired = function(dd) {
    build_policy_grid_paired(dd, POLICY_GRID_MODERATE_STEM)
  },
  policy_grid_largest_moderate = function(dd) {
    build_policy_grid_largest_stable(dd, POLICY_GRID_MODERATE_STEM)
  },
  saturation = build_saturation,
  casualty_surge = build_casualty_surge,
  casualty_surge_size = build_casualty_surge_size,
  airlift_baseline = function(dd) build_airlift(dd, "baseline"),
  airlift_interval = function(dd) build_airlift(dd, "interval"),
  airlift_reliability = function(dd) build_airlift(dd, "reliability"),
  airlift_interval_high = function(dd) build_airlift(dd, "interval", "_high_intensity"),
  airlift_reliability_high = function(dd) build_airlift(dd, "reliability", "_high_intensity"),
  forward_hold_high = function(dd) build_forward_hold(dd, "_high_intensity"),
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
  adequacy_matrix = build_adequacy_matrix,
  adequacy_sizing = build_adequacy_sizing,
  policy_trade = build_policy_trade,
  resolution = build_resolution,
  resolved = build_resolved,
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
