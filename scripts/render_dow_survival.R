#!/usr/bin/env Rscript
##############################################################################
## scripts/render_dow_survival.R                                            ##
## Render the DOW survival function figure from the live env_data.json      ##
## parameters                                                               ##
##############################################################################
#
# Usage:
#   Rscript scripts/render_dow_survival.R                     # to outputs/images
#   Rscript scripts/render_dow_survival.R --refresh-baseline  # to images/
#   Rscript scripts/render_dow_survival.R --scenario high_intensity
#
# Why this exists. `images/dow_survival_function.png` is the only figure in
# README.md that no code in the repository produced: it was drawn once, by
# hand, with the ceilings of the day written into it as literals. When Issue
# #203 re-fitted those ceilings from 2.3% and 1.9% to 2.0% and 1.6%, the
# calibration table beneath the figure moved and the figure did not, so the
# document stated one pair of ceilings in prose and drew another.
#
# Reading the curve's parameters from `env_data.json` rather than restating
# them removes the possibility. A re-fit changes the configuration, the
# configuration is what the figure is drawn from, and the two cannot disagree
# again without the file itself being wrong.
#
# The figure is a statement about the model's mortality function, not a
# measurement of a run, so nothing here simulates anything: the curve is the
# closed-form logistic evaluated over the plotted range.

suppressPackageStartupMessages({
  library(jsonlite)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

SCENARIO <- arg_value("--scenario", "default")

# The tracked figure is reachable only by explicit request, matching the
# contract run_morris(), analyse_run() and render_morris_plots.R carry.
IMAGES <- if ("--refresh-baseline" %in% args) {
  "images"
} else {
  arg_value("--images", "outputs/images")
}

env_data <- fromJSON("env_data.json", simplifyDataFrame = FALSE)

#' Pull one activity's variables out of a `vars` element as a named list
#'
#' @param vars The `vars` block to search (the base block, or a scenario
#'   profile's own overriding block, which carries the same shape)
#' @param elm Element name, e.g. "dow"
#' @param acty Activity name, e.g. "params"
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

params <- acty_vals(env_data$vars, "dow", "params")
if (is.null(params)) stop("env_data.json carries no dow/params block")

# A scenario profile overrides the base ceilings, so the figure can be drawn
# for a profile as well as for the shipped configuration. The overrides sit in
# the profile's own `vars` block and replace the base values variable by
# variable, which is what merge_scenario_vars() does at run time.
profile_label <- "Falklands 1982 calibration"
if (!identical(SCENARIO, "default")) {
  profiles <- env_data$scenarios
  if (!(SCENARIO %in% names(profiles))) {
    stop(sprintf("env_data.json defines no scenario named %s (available: %s)",
                 SCENARIO, paste(names(profiles), collapse = ", ")))
  }
  overrides <- acty_vals(profiles[[SCENARIO]]$vars, "dow", "params")
  for (nm in names(overrides)) params[[nm]] <- overrides[[nm]]
  profile_label <- sprintf("%s profile", SCENARIO)
}

# ── The curve ────────────────────────────────────────────────────────────────

#' Shifted logistic DOW probability, the function R/trajectories.R applies
#'
#' @param t Elapsed minutes since injury
#' @param p_base Irreducible probability at t = 0
#' @param p_max Asymptotic ceiling
#' @param k Steepness, per minute
#' @param t_mid Inflection point, minutes
#' @return DOW probability at each t
dow_probability <- function(t, p_base, p_max, k, t_mid) {
  p_base + (p_max - p_base) / (1 + exp(-k * (t - t_mid)))
}

t_grid <- seq(0, 360, by = 1)

curves <- rbind(
  data.frame(
    t      = t_grid,
    p      = dow_probability(t_grid, params$p1_p_base, params$p1_p_max,
                             params$p1_k, params$p1_t_mid),
    cohort = sprintf("P1 (urgent)  p_max = %.1f%%", 100 * params$p1_p_max),
    stringsAsFactors = FALSE
  ),
  data.frame(
    t      = t_grid,
    p      = dow_probability(t_grid, params$p2_p_base, params$p2_p_max,
                             params$p2_k, params$p2_t_mid),
    cohort = sprintf("P2 (priority)  p_max = %.1f%%", 100 * params$p2_p_max),
    stringsAsFactors = FALSE
  )
)
cohort_levels <- unique(curves$cohort)
curves$cohort <- factor(curves$cohort, levels = cohort_levels)

COHORT_COLOURS <- setNames(c("#D7191C", "#2C7BB6"), cohort_levels)

asymptotes <- data.frame(
  p      = c(params$p1_p_max, params$p2_p_max),
  cohort = factor(cohort_levels, levels = cohort_levels)
)

inflections <- data.frame(
  t     = c(params$p1_t_mid, params$p2_t_mid),
  label = c("(P1 inflection)", "(P2 inflection)")
)

# ── Render ───────────────────────────────────────────────────────────────────

dir.create(IMAGES, recursive = TRUE, showWarnings = FALSE)

p <- ggplot(curves, aes(x = t, y = p, colour = cohort)) +
  annotate("rect", xmin = 60, xmax = 180, ymin = -Inf, ymax = Inf,
           fill = "grey50", alpha = 0.12) +
  annotate("text", x = 120, y = max(asymptotes$p) * 0.93,
           label = "haemorrhagic shock\ncritical window",
           colour = "grey35", size = 3.4, lineheight = 0.95) +
  geom_hline(data = asymptotes, aes(yintercept = p, colour = cohort),
             linetype = "dashed", linewidth = 0.4, alpha = 0.55,
             show.legend = FALSE) +
  geom_vline(data = inflections, aes(xintercept = t),
             linetype = "dotted", linewidth = 0.35, colour = "grey40") +
  geom_text(data = inflections, aes(x = t, y = 0, label = label),
            inherit.aes = FALSE, vjust = 1.9, size = 3, colour = "grey35") +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = COHORT_COLOURS, name = NULL) +
  scale_x_continuous(breaks = seq(0, 360, by = 60), expand = c(0.01, 0)) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 1), "%"),
                     limits = c(0, NA), expand = expansion(mult = c(0.06, 0.08))) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "Died of Wounds (DOW) Survival Function by Priority Cohort",
    subtitle = sprintf("F(t) = p_base + (p_max - p_base) / (1 + exp(-k * (t - t_mid)))    %s",
                       profile_label),
    x        = "Elapsed time since injury (minutes)",
    y        = "DOW probability F(t) (%)",
    caption  = paste(
      "Shaded band = haemorrhagic shock critical window (Eastridge et al. 2012).",
      "Dashed asymptotes show p_max ceilings; dotted verticals show each cohort's inflection point.",
      sprintf("Drawn from env_data.json: P1 k = %s, t_mid = %s; P2 k = %s, t_mid = %s.",
              params$p1_k, params$p1_t_mid, params$p2_k, params$p2_t_mid),
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "top",
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey35", size = 10),
    plot.caption     = element_text(colour = "grey40", size = 8, hjust = 1),
    panel.grid.minor = element_blank()
  )

out <- file.path(IMAGES, "dow_survival_function.png")
ggsave(out, plot = p, width = 11, height = 6, dpi = 150)

message(sprintf("Written %s", out))
message(sprintf("  P1: p_base = %s, p_max = %s, k = %s, t_mid = %s",
                params$p1_p_base, params$p1_p_max, params$p1_k, params$p1_t_mid))
message(sprintf("  P2: p_base = %s, p_max = %s, k = %s, t_mid = %s",
                params$p2_p_base, params$p2_p_max, params$p2_k, params$p2_t_mid))
message(sprintf("  P3: flat %s", params$p3_flat))
