#!/usr/bin/env Rscript

##############################################################################
## scripts/render_paper_figures.R                                           ##
## Renders the multi-run paper's result tables as figures                   ##
##############################################################################
#
# Usage:
#   Rscript scripts/render_paper_figures.R
#   Rscript scripts/render_paper_figures.R --refresh-baseline
#
# Without the flag every figure is written to outputs/images/, leaving the
# tracked set untouched; --refresh-baseline is the only way to write the
# tracked images/ copies.
#
# Three tables in docs/Multi_Run_Analysis.md carry the paper's central
# findings and are read as columns of numbers. This script renders each as a
# figure, and it does so by parsing the tables out of the document itself
# rather than by holding its own copy of the values. A figure therefore
# cannot drift away from the table it illustrates: there is one set of
# numbers, and editing the table changes the figure on the next render. The
# script fails rather than writing where a table is missing, has moved, or no
# longer parses, which is what makes that guarantee hold.

suppressPackageStartupMessages({
  library(ggplot2)
})

PAPER_PATH <- "docs/Multi_Run_Analysis.md"
TRACKED_DIR <- "images"
UNTRACKED_DIR <- file.path("outputs", "images")
FIG_WIDTH_IN <- 9.0
FIG_DPI <- 150
CASUALTY_RATIO <- 2.33
INTENSITY_COLOURS <- c("Moderate intensity" = "#1f5566", "High intensity" = "#9c4a35")

#' Report a fatal condition and stop the script
#'
#' @param msg Character message naming what could not be done.
#' @return Never returns; exits with status 1.
fail <- function(msg) {
  cat(sprintf("[FAIL] %s\n", msg))
  quit(status = 1)
}

#' Report a completed step
#'
#' @param msg Character message naming the step.
#' @return Invisible NULL.
report <- function(msg) {
  cat(sprintf("[ OK ] %s\n", msg))
  invisible(NULL)
}

#' Normalise the typographic characters a markdown table uses into ASCII
#'
#' @param x Character vector taken from the document.
#' @return The same vector with minus signs, en dashes and non-breaking
#'   spaces replaced by their ASCII equivalents, so numbers parse.
#' @details The paper is typeset with a Unicode minus in its difference
#'   columns and an en dash in its percentile ranges. Both read as text to
#'   as.numeric(), so every value would silently become NA without this.
ascii_numbers <- function(x) {
  x <- gsub("\u2212", "-", x, fixed = TRUE)
  x <- gsub("\u2013", "-", x, fixed = TRUE)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x
}

#' Rows of the markdown table that follows a given heading
#'
#' @param lines Character vector of the document's lines.
#' @param heading Exact heading text the table sits under.
#' @param skip Number of tables under that heading to pass over before
#'   taking one.
#' @return Character vector of the table's body rows, header and rule
#'   removed.
extract_table <- function(lines, heading, skip = 0L) {
  start <- which(trimws(lines) == heading)
  if (length(start) != 1L) fail(sprintf("heading not found exactly once: %s", heading))
  idx <- start + 1L
  passed <- 0L
  while (idx <= length(lines)) {
    if (grepl("^\\|", trimws(lines[idx]))) {
      first <- idx
      while (idx <= length(lines) && grepl("^\\|", trimws(lines[idx]))) idx <- idx + 1L
      if (passed == skip) {
        rows <- trimws(lines[first:(idx - 1L)])
        if (length(rows) < 3L) fail(sprintf("table under %s has no body rows", heading))
        return(rows[-(1:2)])
      }
      passed <- passed + 1L
    }
    idx <- idx + 1L
  }
  fail(sprintf("no table found under heading: %s", heading))
}

#' Split one markdown table row into its cells
#'
#' @param row A single table row, pipes included.
#' @return Character vector of trimmed cell contents.
cells <- function(row) {
  parts <- strsplit(sub("^\\|", "", sub("\\|$", "", row)), "\\|")[[1]]
  trimws(parts)
}

#' First number in a cell, ignoring any thousands separator
#'
#' @param cell Cell text.
#' @return Numeric value, or NA where the cell holds no number.
lead_number <- function(cell) {
  cell <- ascii_numbers(gsub(",", "", cell))
  m <- regmatches(cell, regexpr("-?[0-9]+\\.?[0-9]*", cell))
  if (length(m) == 0L) return(NA_real_)
  as.numeric(m)
}

#' The bracketed confidence interval in a cell
#'
#' @param cell Cell text of the form "value [low, high]".
#' @return Numeric vector of length two, NA where absent.
interval <- function(cell) {
  cell <- ascii_numbers(gsub(",", "", cell))
  m <- regmatches(cell, regexpr("\\[[^]]*\\]", cell))
  if (length(m) == 0L) return(c(NA_real_, NA_real_))
  nums <- as.numeric(regmatches(m, gregexpr("-?[0-9]+\\.?[0-9]*", m))[[1]])
  if (length(nums) != 2L) return(c(NA_real_, NA_real_))
  nums
}

#' The parenthesised p10 to p90 range in a cell
#'
#' @param cell Cell text possibly carrying "(p10-p90: low-high)".
#' @return Numeric vector of length two, NA where absent.
percentiles <- function(cell) {
  cell <- ascii_numbers(gsub(",", "", cell))
  m <- regmatches(cell, regexpr("\\(p10-p90:[^)]*\\)", cell))
  if (length(m) == 0L) return(c(NA_real_, NA_real_))
  nums <- as.numeric(regmatches(m, gregexpr("[0-9]+\\.?[0-9]*", m))[[1]])
  if (length(nums) < 2L) return(c(NA_real_, NA_real_))
  c(nums[length(nums) - 1L], nums[length(nums)])
}

#' The house theme shared by every figure this script writes
#'
#' @return A ggplot2 theme object.
paper_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(colour = "grey30", size = 10.5, lineheight = 1.15),
      plot.caption = element_text(colour = "grey40", size = 9, hjust = 0),
      strip.text = element_text(face = "bold", size = 11),
      axis.title = element_text(size = 11),
      legend.position = "top",
      legend.title = element_blank()
    )
}

#' Figure 1: casualty and mortality totals at the two casualty intensities
#'
#' @param lines Character vector of the document's lines.
#' @return A ggplot object.
#' @details Each metric gets its own panel because the metrics carry
#'   different units. The wide bar is the 10th-to-90th-percentile spread
#'   across campaigns and the narrow one the confidence interval on the mean,
#'   which the paper is explicit must not be read as the same quantity.
build_totals_figure <- function(lines) {
  rows <- extract_table(lines, "### Comparative Scenario Analysis", skip = 0L)
  recs <- list()
  for (row in rows) {
    cl <- cells(row)
    if (length(cl) < 3L) next
    metric <- gsub("/run", " per campaign", cl[1], fixed = TRUE)
    for (k in seq_len(2L)) {
      cell <- cl[k + 1L]
      ci <- interval(cell)
      pc <- percentiles(cell)
      recs[[length(recs) + 1L]] <- data.frame(
        metric = metric,
        intensity = c("Moderate intensity", "High intensity")[k],
        mean = lead_number(cell), lo = ci[1], hi = ci[2],
        p10 = pc[1], p90 = pc[2], stringsAsFactors = FALSE
      )
    }
  }
  df <- do.call(rbind, recs)
  if (nrow(df) == 0L || any(is.na(df$mean))) fail("totals table did not parse")
  df$metric <- factor(df$metric, levels = unique(df$metric))
  df$intensity <- factor(df$intensity, levels = names(INTENSITY_COLOURS))

  ggplot(df, aes(x = mean, y = intensity, colour = intensity)) +
    geom_linerange(aes(xmin = p10, xmax = p90), linewidth = 5, alpha = 0.22,
                   na.rm = TRUE) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.16, linewidth = 0.9,
                   na.rm = TRUE) +
    geom_point(size = 3.2) +
    facet_wrap(~metric, scales = "free_x", ncol = 2) +
    scale_x_continuous(expand = expansion(mult = 0.14)) +
    scale_colour_manual(values = INTENSITY_COLOURS) +
    labs(
      title = "Casualty and mortality totals at two casualty intensities",
      subtitle = paste0("Point is the mean over 50 campaigns; narrow bar the 95% ",
                        "confidence interval;\nwide band the 10th-to-90th-percentile ",
                        "spread across campaigns."),
      x = NULL, y = NULL
    ) +
    paper_theme()
}

#' Figure 2: how far each resource queue outgrows casualty volume
#'
#' @param lines Character vector of the document's lines.
#' @return A ggplot object.
#' @details The reference line is the casualty volume ratio between the two
#'   intensities, so a resource to the right of it queues disproportionately
#'   to the load placed on it, which is the paper's central claim.
build_queue_figure <- function(lines) {
  rows <- extract_table(lines, "### Comparative Scenario Analysis", skip = 1L)
  recs <- list()
  for (row in rows) {
    cl <- cells(row)
    if (length(cl) < 4L) next
    mod <- lead_number(cl[2])
    high <- lead_number(cl[3])
    if (is.na(mod) || is.na(high) || mod <= 0 || high <= 0) next
    recs[[length(recs) + 1L]] <- data.frame(
      group = cl[1], moderate = mod, high = high, ratio = high / mod,
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, recs)
  if (is.null(df) || nrow(df) == 0L) fail("queue table did not parse")
  df <- df[order(df$ratio), ]
  df$group <- factor(df$group, levels = df$group)
  df$label <- sprintf("%.3g \u2192 %.3g", df$moderate, df$high)

  ggplot(df, aes(x = ratio, y = group)) +
    geom_vline(xintercept = CASUALTY_RATIO, linetype = "dashed",
               colour = "grey35", linewidth = 0.6) +
    geom_segment(aes(x = CASUALTY_RATIO, xend = ratio, yend = group),
                 colour = "#1f5566", linewidth = 0.8, alpha = 0.6) +
    geom_point(aes(size = high), colour = "#1f5566") +
    geom_text(aes(label = label), hjust = -0.3, size = 3.3, colour = "grey25") +
    scale_size(range = c(1.6, 9), trans = "sqrt", guide = "none") +
    scale_x_continuous(trans = "log10", expand = expansion(mult = c(0.04, 0.34))) +
    labs(
      title = "Queues grow disproportionately to casualty volume",
      subtitle = sprintf(paste0("Dashed line marks the %.2f\u00d7 rise in casualty volume. ",
                                "Point area is the queue at high intensity,\n",
                                "so a resource may grow fast from a negligible base. ",
                                "Labels give the mean queue at each intensity."),
                         CASUALTY_RATIO),
      x = "Queue growth factor (log scale)", y = NULL,
      caption = paste("R2B operating theatre is omitted: its queue is zero at both",
                      "intensities by routing policy, so no growth factor exists.")
    ) +
    paper_theme() +
    theme(panel.grid.major.y = element_line(colour = "grey92"))
}

#' Figure 3: paired differences from the R2B pre-open hold window
#'
#' @param lines Character vector of the document's lines.
#' @return A ggplot object.
#' @details An interval crossing the zero line is an effect the runs did not
#'   establish, which is the distinction the paper turns on and the reason
#'   this table is worth plotting rather than only tabulating.
build_window_figure <- function(lines) {
  rows <- extract_table(lines, "#### The R2B Pre-Open Hold Window", skip = 0L)
  recs <- list()
  for (row in rows) {
    cl <- cells(row)
    if (length(cl) < 4L) next
    ci <- interval(cl[4])
    est <- lead_number(cl[4])
    if (any(is.na(ci)) || is.na(est)) next
    recs[[length(recs) + 1L]] <- data.frame(
      measure = cl[1], est = est, lo = ci[1], hi = ci[2],
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, recs)
  if (is.null(df) || nrow(df) == 0L) fail("hold window table did not parse")
  df$established <- ifelse(df$lo > 0 | df$hi < 0, "Established", "Not established")
  df <- df[order(df$est), ]
  df$measure <- factor(df$measure, levels = df$measure)

  ggplot(df, aes(x = est, y = measure, colour = established)) +
    geom_vline(xintercept = 0, colour = "grey35", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 0.9) +
    geom_point(size = 3.2) +
    scale_colour_manual(values = c("Established" = "#2d6a4f",
                                   "Not established" = "#8a8f8c")) +
    labs(
      title = "What the R2B pre-open hold window changes",
      subtitle = paste0("Mean difference per campaign, 60-minute window against none, ",
                        "with 95% confidence\nintervals. An interval crossing zero is an ",
                        "effect these runs did not establish."),
      x = "Difference per campaign", y = NULL
    ) +
    paper_theme() +
    theme(panel.grid.major.y = element_line(colour = "grey92"))
}

#' Render every figure and write it to the chosen directory
#'
#' @param refresh TRUE to write the tracked images/ copies.
#' @return Invisible NULL.
main <- function(refresh) {
  if (!file.exists(PAPER_PATH)) fail(sprintf("paper not found: %s", PAPER_PATH))
  lines <- readLines(PAPER_PATH, encoding = "UTF-8", warn = FALSE)
  out_dir <- if (refresh) TRACKED_DIR else UNTRACKED_DIR
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  specs <- list(
    list(name = "paper_casualty_totals.png", height = 5.9, fn = build_totals_figure),
    list(name = "paper_queue_growth.png", height = 4.6, fn = build_queue_figure),
    list(name = "paper_hold_window_effects.png", height = 4.8, fn = build_window_figure)
  )
  for (spec in specs) {
    plot <- spec$fn(lines)
    path <- file.path(out_dir, spec$name)
    ggsave(path, plot, width = FIG_WIDTH_IN, height = spec$height,
           dpi = FIG_DPI, bg = "white")
    report(sprintf("wrote %s", path))
  }
  if (!refresh) {
    cat("\nTracked images/ untouched. Re-run with --refresh-baseline to write them.\n")
  }
  invisible(NULL)
}

args <- commandArgs(trailingOnly = TRUE)
main(refresh = "--refresh-baseline" %in% args)
quit(status = 0)
