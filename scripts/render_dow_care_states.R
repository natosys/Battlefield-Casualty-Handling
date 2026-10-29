#!/usr/bin/env Rscript
##############################################################################
## scripts/render_dow_care_states.R                                         ##
## Render the care-state died-of-wounds figure from the live env_data.json  ##
## rates and multipliers                                                    ##
##############################################################################
#
# Usage:
#   Rscript scripts/render_dow_care_states.R                     # to outputs/images
#   Rscript scripts/render_dow_care_states.R --refresh-baseline  # to images/
#
# RENDERS: dow_care_states.png
#
# Why this exists. The README describes the died-of-wounds model by four
# worked Priority 1 casualties, and a figure drawn by hand would carry the
# rates and multipliers of the day it was drawn as literals, so a refit would
# move the configuration and leave the figure behind. Reading them from
# `env_data.json` means the figure and the configuration cannot disagree
# without the file itself being wrong.
#
# The figure is a statement about the model's mortality function, not a
# measurement of a run, so nothing here simulates anything: the casualties and
# their timings are those of R/dow_examples.R, which the console's preview
# draws as well, and their risk is the model's closed form.

suppressPackageStartupMessages({
  library(jsonlite)
  library(ggplot2)
})

source("R/dow_examples.R")

args <- commandArgs(trailingOnly = TRUE)

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

# The tracked figure is reachable only by explicit request, matching the
# contract analyse_run() and the other renderers carry.
IMAGES <- if ("--refresh-baseline" %in% args) {
  "images"
} else {
  arg_value("--images", "outputs/images")
}

env_data <- fromJSON("env_data.json", simplifyDataFrame = FALSE)

#' Pull one activity's variables out of a `vars` element as a named list
#'
#' @param vars The `vars` block to search
#' @param elm Element name, e.g. "dow"
#' @param acty Activity name, e.g. "rates"
#' @return Named list of the activity's values, or NULL if the element or the
#'   activity is absent
acty_vals <- function(vars, elm, acty) {
  e <- Filter(function(x) identical(x$elm, elm), vars)
  if (length(e) == 0) return(NULL)
  a <- Filter(function(x) identical(x$acty, acty), e[[1]]$actys)
  if (length(a) == 0) return(NULL)
  setNames(lapply(a[[1]]$vals, function(v) v$val),
           vapply(a[[1]]$vals, function(v) v$var, character(1)))
}

rates <- acty_vals(env_data$vars, "dow", "rates")
multipliers <- acty_vals(env_data$vars, "dow", "multipliers")
if (is.null(rates) || is.null(multipliers)) {
  stop("env_data.json carries no dow/rates or dow/multipliers block")
}

# ── The four casualties ──────────────────────────────────────────────────────

paths <- dow_example_paths(multipliers)
COLOURS <- setNames(c("#0f7c6a", "#a88412", "#c46a12", "#b3261e"), names(paths))
PANELS <- c("Death rate per hour, first three days", "Chance of having died, first ten days")

#' The plotted series of one casualty
#'
#' @param name The casualty's legend label.
#' @return Data frame of `hours`, `value`, `casualty` and `panel` rows for both
#'   panels.
series <- function(name) {
  rate_h <- seq(0, 72, by = 0.05)
  risk_h <- seq(0.25, 240, by = 0.25)
  rbind(
    data.frame(hours = rate_h,
               value = rates$p1_rate / 24 * dow_example_multiplier(paths[[name]], rate_h),
               casualty = name, panel = PANELS[1]),
    data.frame(hours = risk_h,
               value = dow_example_risk(paths[[name]], rates$p1_rate, risk_h),
               casualty = name, panel = PANELS[2])
  )
}
df <- do.call(rbind, lapply(names(paths), series))
df$casualty <- factor(df$casualty, levels = names(paths))
df$panel <- factor(df$panel, levels = PANELS)
# A rate of zero has no place on a logarithmic axis; it is dropped from the
# drawing, where it reads as the line ending.
df <- df[df$value > 0, ]

totals <- vapply(paths, function(p) dow_example_risk(p, rates$p1_rate, 240), numeric(1))

# ── Render ───────────────────────────────────────────────────────────────────

dir.create(IMAGES, recursive = TRUE, showWarnings = FALSE)

p <- ggplot(df, aes(hours, value, colour = casualty)) +
  geom_line(linewidth = 1) +
  facet_wrap(~panel, ncol = 2, scales = "free") +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE, digits = 1)) +
  scale_colour_manual(values = COLOURS, name = NULL,
                      labels = sprintf("%s (%.2f%% by day 10)", names(paths), 100 * totals)) +
  labs(
    title    = "Died of wounds under the care-state model: four Priority 1 casualties",
    subtitle = sprintf(paste("Base rate %.4f per day; multipliers untreated 1, resuscitated %s,",
                             "after surgery %s in intensive care, x%s at R2B, x%s outside intensive care"),
                       rates$p1_rate, multipliers$resuscitation, multipliers$post_op,
                       multipliers$r2b_icu_penalty, multipliers$hold_bed_penalty),
    x        = "Hours since injury",
    y        = NULL,
    caption  = paste(
      "Timings: R1 treatment at 20 min, resuscitation ends at 95 min, a prompt operation ends at 4.6 h;",
      "each operated casualty spends a day in post-operative care, 2 h outside it for a second operation,",
      "a second day in post-operative care, then five days awaiting airlift. Drawn from env_data.json.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey35", size = 9),
    plot.caption     = element_text(colour = "grey40", size = 8, hjust = 1),
    strip.text       = element_text(face = "bold", hjust = 0),
    panel.grid.minor = element_blank()
  ) +
  guides(colour = guide_legend(nrow = 2))

out <- file.path(IMAGES, "dow_care_states.png")
ggsave(out, plot = p, width = 12, height = 6, dpi = 150)

message(sprintf("Written %s", out))
for (n in names(totals)) message(sprintf("  %s: %.4f%% by day 10", n, 100 * totals[[n]]))
