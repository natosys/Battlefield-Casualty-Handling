#!/usr/bin/env Rscript
##############################################################################
## scripts/render_morris_plots.R                                            ##
## Re-render a completed Morris screen's scatter plots from its saved       ##
## design and responses, without re-running the screen                      ##
##############################################################################
#
# Usage:
#   Rscript scripts/render_morris_plots.R                       # to outputs/images
#   Rscript scripts/render_morris_plots.R --refresh-baseline    # to images/
#   Rscript scripts/render_morris_plots.R \
#     --screen data/sensitivity/morris_r20 --images outputs/images
#
# Why this exists. `run_morris()` writes a screen's rankings and its scatter
# plots in the same pass, so the two agree at the moment they are written and
# nothing afterwards holds them together. A screen re-run that lands its
# rankings in the tracked evidence set without also landing its plots leaves
# the two describing different screens, which is what happened between the
# r = 5 screen of Issue #155 and the r = 20 screen that superseded it: the
# tracked rankings moved to r = 20 and the tracked plots stayed at r = 5, so
# every published plot disagreed with the table printed above it.
#
# The re-render is free. `run_morris()` persists the design matrix and the
# response matrix as `morris_design_and_responses.rds` precisely so a
# plotting-only change need not pay for the screen again, and every quantity
# a scatter plot shows is derived from those two matrices. Nothing here
# re-simulates anything.
#
# The elementary effects are recomputed rather than read from the ranking
# CSVs, because a scatter plot needs the per-trajectory effects and the CSVs
# record only their summaries. Recomputing them means the plot could in
# principle disagree with the ranking it is drawn to accompany, which is the
# defect this script exists to close, so every response's recomputed mu* and
# sigma are checked against the tracked ranking CSV before anything is
# written. A mismatch stops the run rather than producing a plot that
# disagrees with the table more subtly than the one it replaces.

suppressPackageStartupMessages({
  source("R/sensitivity.R")
})

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

SCREEN <- arg_value("--screen", "data/sensitivity/morris_r20")

# The tracked plot directory is reachable only by explicit request, matching
# the contract run_morris() and analyse_run() already carry: an ordinary run
# writes to outputs/ and cannot disturb the tracked evidence set.
IMAGES <- if ("--refresh-baseline" %in% args) {
  "images"
} else {
  arg_value("--images", "outputs/images")
}

#' The seven responses the tracked baseline carries
#'
#' @details A screen writes a plot for every response in `morris_kpis`; these are the
#'   ones README.md embeds, and the ones --refresh-baseline is allowed to
#'   overwrite.
TRACKED_RESPONSES <- c("system_ot_q", "r2b_ot_q", "r2e_ot_q", "r2e_icu_q",
                       "dow_count", "transport_q", "transport_util")

RESPONSES <- {
  named <- arg_value("--responses")
  if (is.null(named)) TRACKED_RESPONSES else strsplit(named, ",")[[1]]
}

rds_path <- file.path(SCREEN, "morris_design_and_responses.rds")
if (!file.exists(rds_path)) {
  stop(sprintf("%s does not exist — --screen must name a completed screen's output directory",
               rds_path))
}

saved <- readRDS(rds_path)
dir.create(IMAGES, recursive = TRUE, showWarnings = FALSE)

message(sprintf("Re-rendering from %s (%d design points, %d responses)",
                rds_path, nrow(saved$X), ncol(saved$Y)))
message(sprintf("Writing to %s", IMAGES))

# The saved design's own bounds are used rather than morris_params', so a
# screen run under bounds that have since changed still re-renders as it was
# run. Its column order is the design's, which is the order tell() reads.
if (!identical(colnames(saved$X), morris_params$name)) {
  stop("the saved design's parameters differ from morris_params — this screen ",
       "predates the current parameter set and cannot be re-rendered against it")
}

# ── Reconstruct the design object tell() needs ────────────────────────────────
#
# The elementary effects are a function of the design matrix alone: ee.oat()
# reads each trajectory's consecutive rows out of X and differences them, so
# a morris object carrying the saved X yields exactly the effects the screen
# reported, whatever generator state produced X in the first place. The call
# below is made only to obtain a correctly-shaped object of the right class;
# its randomly-generated X is discarded and replaced.
r <- nrow(saved$X) / (nrow(morris_params) + 1L)
if (r != as.integer(r)) {
  stop(sprintf("the saved design has %d rows, which is not r x (p + 1) for p = %d",
               nrow(saved$X), nrow(morris_params)))
}

sa <- morris(
  model   = NULL,
  factors = morris_params$name,
  r       = as.integer(r),
  design  = list(type = "oat", levels = 4, grid.jump = 2),
  binf    = saved$binf,
  bsup    = saved$bsup,
  scale   = TRUE
)
sa$X <- saved$X

# ── Check each response against the ranking it accompanies ───────────────────

#' Compare a re-rendered response's mu*/sigma against its tracked ranking
#'
#' @param obj A tell()-populated morris object for this response
#' @param kpi Response name
#' @return TRUE if the ranking CSV is absent (nothing to check against);
#'   otherwise TRUE once every parameter agrees, stopping if any does not
check_against_ranking <- function(obj, kpi) {
  csv <- file.path(SCREEN, sprintf("morris_ranking_%s.csv", kpi))
  if (!file.exists(csv)) {
    warning(sprintf("no tracked ranking for %s — plot written unchecked", kpi))
    return(TRUE)
  }
  tracked <- utils::read.csv(csv, stringsAsFactors = FALSE)

  ee      <- obj$ee
  mu_star <- apply(ee, 2, function(v) if (any(is.finite(v))) mean(abs(v), na.rm = TRUE) else NA_real_)
  sigma   <- apply(ee, 2, function(v) if (sum(is.finite(v)) > 1) sd(v, na.rm = TRUE) else NA_real_)

  i <- match(tracked$parameter, names(mu_star))
  # A degenerate response's ranking records NA for every parameter by design
  # (see rank_response() in R/sensitivity.R), so there is nothing to compare.
  if (all(is.na(tracked$mu_star))) return(TRUE)

  worst <- max(
    abs(mu_star[i] - tracked$mu_star) / pmax(1e-12, abs(tracked$mu_star)),
    abs(sigma[i]   - tracked$sigma_ee) / pmax(1e-12, abs(tracked$sigma_ee)),
    na.rm = TRUE
  )
  if (!is.finite(worst) || worst > 1e-8) {
    stop(sprintf("%s: re-rendered mu*/sigma differ from %s by up to %.3g relative — ",
                 kpi, csv, worst),
         "the plot would disagree with the ranking it accompanies")
  }
  message(sprintf("  %s: agrees with its ranking to %.1e relative", kpi, worst))
  TRUE
}

# ── Render ───────────────────────────────────────────────────────────────────

for (kpi in RESPONSES) {
  if (!(kpi %in% colnames(saved$Y))) {
    stop(sprintf("the saved screen carries no response named %s", kpi))
  }
  obj <- sa
  tell(obj, saved$Y[, kpi])

  degenerate <- {
    y <- saved$Y[, kpi]
    y <- y[is.finite(y)]
    length(y) < 2 || sd(y) <= 1e-9 * max(1, abs(mean(y)))
  }
  check_against_ranking(obj, kpi)

  plot_title <- sprintf("Morris Screening — %s", kpi_labels[[kpi]])
  p <- tryCatch(
    if (degenerate) stop("degenerate response") else plot_morris_scatter(obj, plot_title),
    error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0(plot_title, "\n(insufficient variation to plot)")) +
        theme_void()
    }
  )
  # Identical to run_morris()'s own ggsave(), so a plot written here and a
  # plot written by a screen are the same file for the same screen.
  ggsave(file.path(IMAGES, sprintf("morris_%s.png", kpi)), plot = p,
         width = 12, height = 9, dpi = 130)
  message(sprintf("  %s written", file.path(IMAGES, sprintf("morris_%s.png", kpi))))
}

message(sprintf("Done — %d plot(s) rendered from %s", length(RESPONSES), rds_path))
