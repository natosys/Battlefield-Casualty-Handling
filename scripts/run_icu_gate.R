#!/usr/bin/env Rscript
##############################################################################
## scripts/run_icu_gate.R                                                   ##
## The post-operative intensive care gate, at its two arms                  ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_icu_gate.R --refresh-baseline
#   Rscript scripts/run_icu_gate.R --iterations 4 --days 10
#
# Why this exists. The Post-Operative Intensive Care Gate section of
# docs/Multi_Run_Analysis.md prints the effect of `icu_gating.enabled` on
# intensive care utilisation and mortality, and until this script existed the
# comparison had no driver: it was invoked as run.R under a parameter
# override, so there was no command to give a --refresh-baseline flag to and
# no tracked evidence set behind the printed figures (Issue #388). This is
# that driver, on the arrangement scripts/run_hold_window.R establishes for a
# paired two-arm comparison; the two experiments are close enough in shape to
# share that arrangement without sharing a runner module
# (docs/Multi_Run_Supplement.md, "The Post-Operative Intensive Care Gate"
# records the decision).
#
# Both arms run under one control seed, so each draws the same
# per-replication seeds from R's PRNG, pairing replication $i$ of one arm
# with replication $i$ of the other.
#
# --refresh-baseline is the only way to write the tracked data/icu_gate/, and
# it runs the documented protocol (30 replications x 360 days x 2 arms at seed
# 42, the sustained-operations protocol R/long_horizon.R establishes) rather
# than whatever arguments accompany it, so the tracked set and the design
# docs/Multi_Run_Supplement.md documents cannot diverge through a mistyped
# argument. Without it the run writes under outputs/ alone.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/queue_series.R")
source("R/icu_gate.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--iterations", type = "integer", default = ICU_GATE_REPLICATIONS,
              help = "Replications per arm [default: %default]"),
  make_option("--days", type = "integer", default = ICU_GATE_DAYS,
              help = "Campaign length in days [default: %default]"),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = ICU_GATE_SEED,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/icu_gate/ copy")
)

opt <- parse_args(OptionParser(option_list = option_list))

# A baseline refresh runs the documented protocol rather than whatever the
# caller passed, on the convention scripts/run_hold_window.R establishes.
if (isTRUE(opt$`refresh-baseline`)) {
  opt$iterations <- ICU_GATE_REPLICATIONS
  opt$days       <- ICU_GATE_DAYS
  opt$seed       <- ICU_GATE_SEED
  message("Baseline refresh: running the documented protocol, ",
          sprintf("%d replications x %d days x %d arms at seed %d",
                  ICU_GATE_REPLICATIONS, ICU_GATE_DAYS,
                  length(ICU_GATE_ARMS), ICU_GATE_SEED))
}

if (opt$iterations < 1L) {
  stop("--iterations must be at least 1, found ", opt$iterations, call. = FALSE)
}
if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)

#' Directory the measurement is written to
OUTPUT_DIR <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "icu_gate")
} else {
  file.path("outputs", "data", "icu_gate")
}
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Measure one arm and return its per-replication responses
#'
#' @param gate_enabled Gate state to run at (0 or 1).
#' @return The arm's response rows.
#'
#' @details The configuration globals are restored on exit, on the error
#'   path as well as the success path, so an arm that fails part-way leaves
#'   the session as it found it. The control seed is set once per arm, from
#'   the caller rather than inside R/icu_gate.R, which is what pairs the
#'   arms.
measure_arm <- function(gate_enabled) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  apply_icu_gate_setting(json_data, opt$scenario, gate_enabled)

  message(sprintf("Gate %s: %d replications x %d days",
                  if (gate_enabled == 1) "enabled" else "disabled",
                  opt$iterations, opt$days))
  set.seed(opt$seed)
  run_icu_gate_measurement(gate_enabled, n_iterations = opt$iterations,
                           n_days = opt$days, max_cores = opt$`max-cores`)
}

per_replication <- do.call(rbind, lapply(ICU_GATE_ARMS, measure_arm))

summary_rows <- do.call(rbind, lapply(ICU_GATE_ARMS, function(gate_enabled) {
  arm <- per_replication[per_replication$gate_enabled == gate_enabled, ]
  cbind(data.frame(gate_enabled = gate_enabled), summarise_icu_gate(arm))
}))

#' Half-width each paired response is sized against, in its own units
#'
#' @details Chosen as roughly two percentage points of occupancy or one death
#'   of wounds, the scale the companion paper's replication-count statement
#'   is made at.
PAIRED_HALF_WIDTHS <- c(icu_occupancy = 0.02, total_dow = 0.5, total_casualties = 5,
                        icu_pathway_n = 5, icu_pathway_dow = 0.5,
                        hold_pathway_n = 5, hold_pathway_dow = 0.5)

paired <- do.call(rbind, lapply(ICU_GATE_RESPONSES, function(response) {
  row <- icu_gate_paired_difference(per_replication, response,
                                    ICU_GATE_ARMS[1], ICU_GATE_ARMS[2])
  half_width <- PAIRED_HALF_WIDTHS[[response]]
  row$reps_needed <- icu_gate_replications_for(per_replication, response,
                                               ICU_GATE_ARMS[1], ICU_GATE_ARMS[2],
                                               half_width)
  row
}))

write.csv(per_replication, file.path(OUTPUT_DIR, "icu_gate_replications.csv"),
          row.names = FALSE)
write.csv(summary_rows, file.path(OUTPUT_DIR, "icu_gate_summary.csv"),
          row.names = FALSE)
write.csv(paired, file.path(OUTPUT_DIR, "icu_gate_paired.csv"), row.names = FALSE)
message(sprintf("Intensive care gate responses, summary and paired differences written to %s",
                OUTPUT_DIR))

#' Print one response's arm means and paired difference as a markdown table row
#'
#' @param response Name of the response to print.
#' @param label Column label for the response.
#' @return Invisible NULL.
print_row <- function(response, label) {
  cells <- vapply(ICU_GATE_ARMS, function(gate_enabled) {
    r <- summary_rows[summary_rows$gate_enabled == gate_enabled &
                        summary_rows$response == response, ]
    if (nrow(r) != 1 || is.na(r$mean)) return("n/a")
    sprintf("%.4f", r$mean)
  }, character(1))
  diff <- paired[paired$response == response, ]
  diff_cell <- if (nrow(diff) != 1 || is.na(diff$difference)) {
    "n/a"
  } else {
    sprintf("%+.4f [%+.4f, %+.4f]", diff$difference, diff$ci_lower, diff$ci_upper)
  }
  cat(sprintf("| %s | %s | %s |\n", label, paste(cells, collapse = " | "), diff_cell))
  invisible(NULL)
}

cat("\n| Measure | Gate disabled | Gate enabled (shipped) | Difference |\n")
cat("| --- | --- | --- | --- |\n")
print_row("icu_occupancy", "R2E ICU occupancy (share)")
print_row("total_dow", "Died of wounds per run")

icu_dow <- sum(per_replication$icu_pathway_dow[per_replication$gate_enabled == 1])
icu_n   <- sum(per_replication$icu_pathway_n[per_replication$gate_enabled == 1])
hold_dow <- sum(per_replication$hold_pathway_dow[per_replication$gate_enabled == 1])
hold_n   <- sum(per_replication$hold_pathway_n[per_replication$gate_enabled == 1])
within_fmt <- paste0("\nWithin the enabled arm: intensive care pathway %d/%d deaths (%.2f%%), ",
                     "holding-bed pathway %d/%d (%.2f%%)\n")
cat(sprintf(within_fmt, icu_dow, icu_n, 100 * icu_dow / icu_n,
            hold_dow, hold_n, 100 * hold_dow / hold_n))
