#!/usr/bin/env Rscript
##############################################################################
## scripts/check_bed_queue_coverage.R                                       ##
## Regression check — a bed queue figure covers every bed type its echelon  ##
## fields                                                                   ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_bed_queue_coverage.R
#   Rscript scripts/check_bed_queue_coverage.R --days 3
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. plot_r2e_bed_queues() named the bed types it plotted,
# "ot" and "icu", so the holding pool was excluded by construction and no
# amount of data could put it in the figure. The tracked image was used in
# docs/Single_Run_Analysis.md to characterise R2E, and it showed the queues for
# two pools while omitting a third whose aggregate queue at seed 42 is larger
# than one of them. plot_r2b_bed_queues() selected on a wildcard and derived
# the type from the resource name, so the same pipeline plotted the two
# echelons to different rules with nothing stating a reason (Issue #315).
#
# A figure omitting a series is invisible in a way a wrong number is not. The
# reader sees pools queueing and has no way to know that another was left out,
# and the omission survives every check that compares numbers, because no
# number is wrong.
#
# What it asserts:
#
#   1. Every R2E Heavy bed type present in the resource monitor appears in the
#      R2E queue figure.
#   2. Every R2B bed type present in the resource monitor appears in the R2B
#      queue figure.
#   3. Each figure's panel labels count the beds in the pool they name.
#   4. The R2E panels share one vertical scale. A free scale lets every pool
#      fill its own panel whatever it queued, so a pool never exceeding one
#      casualty is drawn exactly as a pool reaching fourteen, which invites the
#      reader to conclude they are under equal strain. At this model's depths
#      the shallow pools stay legible against the deep one, so the comparison
#      costs nothing to keep.
#   5. A bed type the establishment does not currently field appears in each
#      figure without a code change. This is the assertion that defends the
#      rule rather than today's establishment: it injects a bed type into the
#      monitor that no configuration ships, and a figure naming its types
#      instead of deriving them fails it.
#   6. Every series in a figure's data is drawn in the figure, at a bed-type
#      count past the eight colours the palette supplies. A discrete colour
#      scale with fewer values than levels assigns the surplus none and drops
#      their rows at render time, which would reintroduce the omission this
#      check exists to stop, one bed type further along and just as silently.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
  library(ggplot2)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")

args <- commandArgs(trailingOnly = TRUE)

#' Read a flag's value from the command line
#'
#' @param flag Flag name, including its leading dashes.
#' @param default Value returned when the flag is absent or has no argument.
#' @return The argument following `flag`, or `default`.
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

#' Run length in days for the run the figures are built from
#'
#' @details Three days is enough for every bed type to appear in the monitor,
#'   which is all these assertions read. The check asserts coverage rather than
#'   any queue's depth, so a longer run would cost time and settle nothing.
CHECK_DAYS <- as.integer(arg_value("--days", 3L))

#' Seed for that run
CHECK_SEED <- as.integer(arg_value("--seed", 42L))

#' Bed type injected to prove the selection rule is a rule
#'
#' @details No shipped configuration fields a bed type of this name, so a
#'   figure that plots it can only be deriving its types from the monitor.
INJECTED_TYPE <- "stepdown"

#' Further bed types injected to push a figure past its colour palette
#'
#' @details Eight more takes either echelon past the eight colours the Set2
#'   palette supplies, which is the count at which a discrete scale stops
#'   assigning colours and quietly drops the rows that have none.
EXTRA_TYPES <- c("burns", "isolation", "triage", "paediatric",
                 "recovery", "transit", "observation", "decontamination")

state <- new.env(parent = emptyenv())
state$failures <- character(0)

#' Record a failure, deferring the non-zero exit to the end of the run
#'
#' @param ... sprintf() format string and its arguments.
#' @return Invisibly, the accumulated failure vector.
fail <- function(...) state$failures <- c(state$failures, sprintf(...))

#' Print one PASS or FAIL line, recording a failure
#'
#' @param ok TRUE when the assertion held.
#' @param fmt sprintf() format string describing the assertion.
#' @param ... Arguments to `fmt`.
#' @return Invisible NULL.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

#' The bed types an echelon fields, read from the resource monitor
#'
#' @param resources Resource monitor rows.
#' @param echelon Echelon element of the resource name, "r2eheavy" or "r2b".
#' @return Upper-case bed types, sorted.
monitor_bed_types <- function(resources, echelon) {
  pattern <- sprintf("^b_%s_.*_[0-9]+_t[0-9]+$", echelon)
  beds <- grep(pattern, unique(resources$resource), value = TRUE)
  sort(unique(toupper(sub(sprintf("^b_%s_(.*?)_[0-9]+_t[0-9]+$", echelon), "\\1", beds))))
}

#' The bed types a queue figure plots
#'
#' @param p A ggplot object returned by one of the bed queue plotters.
#' @return Upper-case bed types, sorted.
figure_bed_types <- function(p) sort(unique(as.character(p$data$bed_type)))

#' Whether every colour level in a figure's data is actually drawn
#'
#' @param p A ggplot object returned by one of the bed queue plotters.
#' @return TRUE when the rendered layer draws every level of the colour
#'   aesthetic, none of them left without a colour.
#' @details The plot's data and the figure can disagree. A discrete colour
#'   scale with fewer values than levels assigns the surplus levels no colour
#'   and drops their rows at render time, so a series can sit in the data and
#'   appear nowhere in the figure. Reading the built layer is what tells the
#'   two apart; reading `p$data` alone would report a series present that a
#'   reader cannot see, which is the failure mode this whole check exists for.
#'
#'   The colour variable is read from the plot's own mapping rather than
#'   assumed, the two echelons colouring by different things: R2E by bed type,
#'   one panel per type, and R2B by bed label within a panel per unit.
all_series_drawn <- function(p) {
  built     <- ggplot_build(p)$data[[1]]
  colour_by <- rlang::as_name(p$mapping$colour)
  n_levels  <- length(unique(as.character(p$data[[colour_by]])))
  !any(is.na(built$colour)) && length(unique(built$colour)) == n_levels
}

#' Add one bed of a given type to a resource monitor
#'
#' @param resources Resource monitor rows.
#' @param echelon Echelon element of the resource name.
#' @param bed_type Bed type to inject.
#' @return `resources` with the injected bed's rows appended.
#' @details The injected rows copy an existing row's shape so that every column
#'   the plotters read is present and of the right type.
inject_bed_type <- function(resources, echelon, bed_type) {
  template <- resources[grepl(sprintf("^b_%s_", echelon), resources$resource), ][1, ]
  template$resource <- sprintf("b_%s_%s_1_t1", echelon, bed_type)
  bind_rows(resources, template)
}

#' Plot both echelons' bed queues into a directory of their own
#'
#' @param resources Resource monitor rows to plot.
#' @return Named list of the two ggplot objects, `r2e` and `r2b`.
plot_both <- function(resources) {
  img <- tempfile("bch_bed_queue_")
  dir.create(img, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(img, recursive = TRUE), add = TRUE)
  list(
    r2e = plot_r2e_bed_queues(resources, img),
    r2b = plot_r2b_bed_queues(resources, img)
  )
}

cat(sprintf("Bed queue coverage check: %d-day run, seed %d\n\n", CHECK_DAYS, CHECK_SEED))

env_data <- load_scenario("env_data.json", "default")
day_min  <- DAY_MIN
counts   <- sapply(env_data$elms, length)

set.seed(CHECK_SEED)
invisible(capture.output(wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)))
resources <- as.data.frame(get_mon_resources(list(wrapped)))

# ── 1 and 2. Every bed type the echelon fields is plotted ───────────────────

cat("-- every bed type appears in its echelon's figure --\n")

plots <- plot_both(resources)

for (arm in list(list(key = "r2e", echelon = "r2eheavy", label = "R2E Heavy"),
                 list(key = "r2b", echelon = "r2b",      label = "R2B"))) {
  fielded <- monitor_bed_types(resources, arm$echelon)
  plotted <- figure_bed_types(plots[[arm$key]])
  missing <- setdiff(fielded, plotted)
  report(length(fielded) > 1 && length(missing) == 0,
         "%s fields %d bed types (%s) and the figure plots all of them%s",
         arm$label, length(fielded), paste(fielded, collapse = ", "),
         if (length(missing)) sprintf("; missing %s", paste(missing, collapse = ", ")) else "")
}

# ── 3. Panel labels count their own pool ────────────────────────────────────

cat("\n-- panel labels count the beds in the pool they name --\n")

labelled <- plots$r2e$data %>%
  group_by(bed_type, facet_label) %>%
  summarise(n_beds = n_distinct(resource), .groups = "drop") %>%
  mutate(stated = as.integer(sub(".*\\((\\d+) bed.*", "\\1", facet_label)))
report(nrow(labelled) > 0 && all(labelled$stated == labelled$n_beds),
       "each R2E panel label states its own bed count (%s)",
       paste(labelled$facet_label, collapse = ", "))

# ── 4. The panels share one vertical scale ──────────────────────────────────

cat("\n-- the panels share a vertical scale --\n")

free_y <- isTRUE(plots$r2e$facet$params$free$y)
report(!free_y,
       "the R2E panels share a vertical scale, so a pool queueing one deep is not drawn as though it queued fourteen")

# ── 5. A bed type nothing ships appears without a code change ───────────────

cat("\n-- the selection rule is a rule, not a list --\n")

for (arm in list(list(key = "r2e", echelon = "r2eheavy", label = "R2E Heavy"),
                 list(key = "r2b", echelon = "r2b",      label = "R2B"))) {
  injected <- plot_both(inject_bed_type(resources, arm$echelon, INJECTED_TYPE))
  plotted  <- figure_bed_types(injected[[arm$key]])
  report(toupper(INJECTED_TYPE) %in% plotted,
         "a '%s' bed added to %s appears in its figure with no change to the plotter",
         INJECTED_TYPE, arm$label)
}

# ── 6. Every series in the data reaches the figure ──────────────────────────
#
# Injecting eight further bed types takes each echelon past the eight colours
# the palette supplies, which is where a discrete scale silently stops drawing.

cat("\n-- every series in the data is drawn, past the palette's limit --\n")

for (arm in list(list(key = "r2e", echelon = "r2eheavy", label = "R2E Heavy"),
                 list(key = "r2b", echelon = "r2b",      label = "R2B"))) {
  many <- resources
  for (extra in EXTRA_TYPES) many <- inject_bed_type(many, arm$echelon, extra)
  crowded <- plot_both(many)[[arm$key]]
  n_types <- length(figure_bed_types(crowded))
  report(n_types > 8 && all_series_drawn(crowded),
         "%s at %d bed types draws every bed, past the 8 its palette supplies",
         arm$label, n_types)
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All bed queue coverage checks passed.\n")
quit(status = 0)
