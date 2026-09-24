#!/usr/bin/env Rscript
##############################################################################
## scripts/run_mass_casualty.R                                              ##
## The mass casualty event stress test, at its two arms                     ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_mass_casualty.R --refresh-baseline
#   Rscript scripts/run_mass_casualty.R --iterations 4 --days 10
#
# Why this exists. Mass Casualty Events Degrade Care Without Revealing New
# Constraints in docs/Multi_Run_Analysis.md prints a four-row table from 10
# replications of each of two arms, and until this script existed the
# comparison had no driver: it was invoked as run.R under a parameter
# override, so there was no command to give a --refresh-baseline flag to and
# no tracked evidence set behind the printed figures (Issue #389). This is
# that driver, on the arrangement scripts/run_hold_window.R establishes for a
# two-arm comparison whose arms are not paired on one control seed.
#
# --refresh-baseline is the only way to write the tracked data/mass_casualty/,
# and it runs the documented protocol (62 replications x 30 days x 2 arms at
# seed 42) rather than whatever arguments accompany it, so the tracked set and
# the design docs/Multi_Run_Supplement.md documents cannot diverge through a
# mistyped argument. Without it the run writes under outputs/ alone.
#
# The illustrative single run behind images/mass_casualty_events.png is
# written alongside the replicated evidence set under --refresh-baseline,
# since injection ships disabled and run.R --refresh-baseline cannot write it
# (docs/Multi_Run_Supplement.md, "Mass Casualty Event Stress Test").

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/mass_casualty.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--iterations", type = "integer", default = MASS_CASUALTY_REPLICATIONS,
              help = "Replications per arm [default: %default]"),
  make_option("--days", type = "integer", default = MASS_CASUALTY_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = MASS_CASUALTY_SEED,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/mass_casualty/ copy")
)

opt <- parse_args(OptionParser(option_list = option_list))

# A baseline refresh runs the documented protocol rather than whatever the
# caller passed, on the convention scripts/run_hold_window.R establishes.
if (isTRUE(opt$`refresh-baseline`)) {
  opt$iterations <- MASS_CASUALTY_REPLICATIONS
  opt$days       <- MASS_CASUALTY_DAYS
  opt$seed       <- MASS_CASUALTY_SEED
  message("Baseline refresh: running the documented protocol, ",
          sprintf("%d replications x %d days x %d arms at seed %d",
                  MASS_CASUALTY_REPLICATIONS, MASS_CASUALTY_DAYS,
                  length(MASS_CASUALTY_ARMS), MASS_CASUALTY_SEED))
}

if (opt$iterations < 1L) {
  stop("--iterations must be at least 1, found ", opt$iterations, call. = FALSE)
}
if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)

#' Directory the measurement is written to
OUTPUT_DIR <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "mass_casualty")
} else {
  file.path("outputs", "data", "mass_casualty")
}
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

IMAGES_DIR <- if (isTRUE(opt$`refresh-baseline`)) "images" else file.path("outputs", "images")
dir.create(IMAGES_DIR, recursive = TRUE, showWarnings = FALSE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Measure one arm and return its per-replication responses
#'
#' @param rate_per_day Injection rate to run at, in events per day.
#' @return The arm's response rows.
#'
#' @details The configuration globals are restored on exit, on the error path
#'   as well as the success path, so an arm that fails part-way leaves the
#'   session as it found it.
measure_arm <- function(rate_per_day) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  apply_mass_casualty_setting(json_data, opt$scenario, rate_per_day)

  message(sprintf("Injection rate %.2f/day: %d replications x %d days",
                  rate_per_day, opt$iterations, opt$days))
  set.seed(opt$seed)
  run_mass_casualty_measurement(rate_per_day, n_iterations = opt$iterations,
                                n_days = opt$days, max_cores = opt$`max-cores`)
}

per_replication <- do.call(rbind, lapply(MASS_CASUALTY_ARMS, measure_arm))

count_summary <- do.call(rbind, lapply(MASS_CASUALTY_ARMS, function(rate) {
  arm <- per_replication[per_replication$rate_per_day == rate, ]
  cbind(data.frame(rate_per_day = rate), summarise_mass_casualty_counts(arm))
}))

dow_summary <- do.call(rbind, lapply(MASS_CASUALTY_ARMS, function(rate) {
  arm <- per_replication[per_replication$rate_per_day == rate, ]
  rbind(
    cbind(data.frame(rate_per_day = rate, origin = "ordinary"),
          mass_casualty_dow_rate(arm, "n_ordinary", "dow_ordinary")),
    cbind(data.frame(rate_per_day = rate, origin = "event"),
          mass_casualty_dow_rate(arm, "n_event", "dow_event"))
  )
}))

write.csv(per_replication, file.path(OUTPUT_DIR, "mass_casualty_replications.csv"),
          row.names = FALSE)
write.csv(count_summary, file.path(OUTPUT_DIR, "mass_casualty_count_summary.csv"),
          row.names = FALSE)
write.csv(dow_summary, file.path(OUTPUT_DIR, "mass_casualty_dow_summary.csv"),
          row.names = FALSE)
message(sprintf("Mass casualty responses and summaries written to %s", OUTPUT_DIR))

# The illustrative single run behind images/mass_casualty_events.png, at the
# documented override and the module's own seed and horizon, written only
# under a baseline refresh since it is the one tracked image run.R
# --refresh-baseline cannot produce (injection ships disabled).
if (isTRUE(opt$`refresh-baseline`)) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  apply_mass_casualty_setting(json_data, opt$scenario, MASS_CASUALTY_ARMS[2])
  set.seed(MASS_CASUALTY_SEED)
  illustrative_env <- run_once(MASS_CASUALTY_DAYS, seed = MASS_CASUALTY_SEED, write_files = FALSE)
  illustrative_mon <- list(
    arrivals   = get_mon_arrivals(list(illustrative_env),   ongoing = TRUE),
    attributes = get_mon_attributes(list(illustrative_env)),
    resources  = get_mon_resources(list(illustrative_env))
  )
  # analyse_run() writes every plot it produces to images_dir, not the mass
  # casualty timeline alone, and every CSV of a full single-run analysis to
  # output_dir, several of them (mass_casualty_dow_summary.csv among them)
  # under names this module's own replicated evidence set already uses for a
  # different quantity. Both go to a scratch directory of their own, and only
  # the one file this experiment needs, the mass casualty timeline, is copied
  # out into IMAGES_DIR, on the convention docs/Multi_Run_Supplement.md
  # documents for this image: it is copied into place from the run's own
  # output directory rather than written there directly.
  illustrative_scratch <- file.path(tempdir(), "mass_casualty_illustrative")
  illustrative_images   <- file.path(illustrative_scratch, "images")
  illustrative <- analyse_run(illustrative_mon, output_dir = illustrative_scratch,
                              images_dir = illustrative_images)
  file.copy(file.path(illustrative_images, "mass_casualty_events.png"),
           file.path(IMAGES_DIR, "mass_casualty_events.png"), overwrite = TRUE)
  message(sprintf("Illustrative run: %d event(s), image written to %s",
                  illustrative$mass_casualty_event_count,
                  file.path(IMAGES_DIR, "mass_casualty_events.png")))
}

cat("\n| Metric | No events injected | Events injected |\n")
cat("| --- | --- | --- |\n")

#' Print a count-summary row (mean-only, no CI, matching the paper's table)
#'
#' @param response Name of the response to print.
#' @param label Row label.
#' @param digits Decimal places to print the mean to.
#' @return Invisible NULL.
print_count_row <- function(response, label, digits = 1) {
  cells <- vapply(MASS_CASUALTY_ARMS, function(rate) {
    r <- count_summary[count_summary$rate_per_day == rate & count_summary$response == response, ]
    if (nrow(r) != 1) return("n/a")
    sprintf(sprintf("%%.%df", digits), r$mean)
  }, character(1))
  cat(sprintf("| %s | %s |\n", label, paste(cells, collapse = " | ")))
  invisible(NULL)
}

#' Print a died-of-wounds rate row (pooled proportion, percent)
#'
#' @param origin "ordinary" or "event".
#' @param label Row label.
#' @return Invisible NULL.
print_dow_row <- function(origin, label) {
  cells <- vapply(MASS_CASUALTY_ARMS, function(rate) {
    r <- dow_summary[dow_summary$rate_per_day == rate & dow_summary$origin == origin, ]
    if (nrow(r) != 1 || r$n == 0) return("not applicable")
    sprintf("%.2f%%", 100 * r$rate)
  }, character(1))
  cat(sprintf("| %s | %s |\n", label, paste(cells, collapse = " | ")))
  invisible(NULL)
}

print_count_row("total_casualties", "Average total casualties/run")
print_count_row("n_events", "Average events/run", digits = 2)
print_dow_row("ordinary", "Died-of-wounds rate, ordinary casualties")
print_dow_row("event", "Died-of-wounds rate, event casualties")
