#!/usr/bin/env Rscript
##############################################################################
## scripts/run_policy_sweep.R                                               ##
## The evacuation policy sweep, at the sustained-operations horizon          ##
##############################################################################
#
# Usage:
#   Rscript scripts/run_policy_sweep.R --refresh-baseline
#   Rscript scripts/run_policy_sweep.R --iterations 4 --days 90 --window 30
#   Rscript scripts/run_policy_sweep.R --policies 21,30
#
# Why this exists. The shipped evacuation policy of 21 days rests on a single
# comparison against 30, run to test a mechanism rather than to find a value.
# This script sweeps the parameter across the range its doctrinal source states
# is a command decision, at the horizon the effect needs to develop over, and
# reports the trade the policy consists of: what a shorter policy buys in
# forward stability and post-operative intensive care access, and what it costs
# in returns to duty and in demand on the national support base.
#
# The arms are paired. One control seed is set per arm, so replication k of
# every arm runs the same parent stream and a difference between two policies is
# measured within replication rather than between arm means.
#
# Each arm is checkpointed as it completes and resumed rather than re-run, so an
# interruption costs the arm in flight rather than all 150 replication-years.
#
# --refresh-baseline is the only way to write the tracked data/policy/ copy.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/policy_sweep.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--policies", type = "character",
              default = paste(POLICY_DAYS, collapse = ","),
              help = "Comma-separated evacuation policies in days [default: %default]"),
  make_option("--iterations", type = "integer", default = POLICY_REPLICATIONS,
              help = "Replications per policy [default: %default]"),
  make_option("--days", type = "integer", default = POLICY_DAYS_HORIZON,
              help = "Campaign length in days [default: %default]"),
  make_option("--window", type = "integer", default = POLICY_WINDOW_DAYS,
              help = "Closing window the stability responses use [default: %default]"),
  make_option("--hold-beds", type = "character", default = NULL,
              help = paste("Comma-separated R2E holding establishments to sweep.",
                           "Omitted, the shipped establishment is used and the",
                           "policy axis alone is swept [default: shipped]")),
  make_option("--scenario", type = "character", default = "default",
              help = "Scenario profile to run under [default: %default]"),
  make_option("--seed", type = "integer", default = 42L,
              help = "Control seed [default: %default]"),
  make_option("--max-cores", type = "integer", default = NULL,
              help = "Cap on concurrent forks [default: the machine's cores]"),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/policy/ copy")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (opt$iterations < 1L) {
  stop("--iterations must be at least 1, found ", opt$iterations, call. = FALSE)
}
if (opt$days < 1L) stop("--days must be at least 1, found ", opt$days, call. = FALSE)
if (opt$window < 1L || opt$window > opt$days) {
  stop(sprintf("--window must lie between 1 and --days (%d), found %d", opt$days,
               opt$window), call. = FALSE)
}

policies <- as.integer(trimws(strsplit(opt$policies, ",")[[1]]))
if (any(is.na(policies)) || any(policies < 1L)) {
  stop("--policies must be whole numbers of days of at least 1, found '",
       opt$policies, "'", call. = FALSE)
}

establishments <- if (is.null(opt$`hold-beds`)) {
  NA_integer_
} else {
  as.integer(trimws(strsplit(opt$`hold-beds`, ",")[[1]]))
}
if (any(is.na(establishments)) && !is.null(opt$`hold-beds`)) {
  stop("--hold-beds must be whole bed counts, found '", opt$`hold-beds`, "'",
       call. = FALSE)
}
if (!all(is.na(establishments)) && any(establishments < 1L)) {
  stop("--hold-beds must be at least 1, found '", opt$`hold-beds`, "'", call. = FALSE)
}

#' Whether the establishment axis is being swept alongside the policy
#'
#' @details The two experiments are written to separate files. The policy sweep
#'   at the shipped establishment is a published result with a tracked evidence
#'   set and a regression check reading it, and a run that adds an establishment
#'   axis measures something else; writing both to one file would leave the
#'   check unable to tell which rows it was asserting.
SWEEPING_ESTABLISHMENT <- !all(is.na(establishments))

#' Directory the measurement is written to
OUTPUT_DIR <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "policy")
} else {
  file.path("outputs", "data", "policy")
}

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Measure one evacuation policy and return its per-replication responses
#'
#' @param policy_days Evacuation policy to run at, in days.
#' @param hold_beds R2E holding establishment to run at, or NA for the shipped
#'   one.
#' @return The arm's response rows, carrying the policy and the establishment.
#'
#' @details The configuration globals are restored on exit, on the error path as
#'   well as the success path, so an arm that fails part-way leaves the session
#'   as it found it. The control seed is set once per arm, which is what pairs
#'   the arms.
measure_arm <- function(policy_days, hold_beds = NA_integer_) {
  config_snapshot <- capture_config_globals()
  on.exit(restore_config_globals(config_snapshot), add = TRUE)

  apply_policy_setting(json_data, opt$scenario, policy_days,
                       hold_beds = if (is.na(hold_beds)) NULL else hold_beds)

  message(sprintf("Evacuation policy %d days%s: %d replications x %d days",
                  policy_days,
                  if (is.na(hold_beds)) "" else sprintf(", %d holding beds", hold_beds),
                  opt$iterations, opt$days))
  set.seed(opt$seed)
  rows <- run_policy_measurement(policy_days, n_iterations = opt$iterations,
                                 n_days = opt$days, window_days = opt$window,
                                 max_cores = opt$`max-cores`)
  rows$hold_beds <- hold_beds
  rows
}

#' Path one arm's checkpointed responses are written to and resumed from
#'
#' @param policy_days Evacuation policy the arm ran at, in days.
#' @param hold_beds Establishment the arm ran at, or NA for the shipped one.
#' @return The file path for that arm.
arm_path <- function(policy_days, hold_beds = NA_integer_) {
  if (is.na(hold_beds)) {
    return(file.path(OUTPUT_DIR, sprintf("policy_sweep_arm_%dd.csv", policy_days)))
  }
  file.path(OUTPUT_DIR,
            sprintf("establishment_sweep_arm_%dd_%db.csv", policy_days, hold_beds))
}

#' Stem the run's outputs are written under
#'
#' @details The establishment sweep writes its own files rather than adding rows
#'   to the policy sweep's. The policy sweep at the shipped establishment is a
#'   published result with a tracked evidence set and a regression check reading
#'   it; a run that varies the establishment measures something else, and one
#'   file carrying both would leave that check unable to tell which rows it was
#'   asserting.
OUTPUT_STEM <- if (SWEEPING_ESTABLISHMENT) "establishment_sweep" else "policy_sweep"

#' Measure one arm, or read it back where it has already been measured
#'
#' @param policy_days Evacuation policy to run at, in days.
#' @param hold_beds Establishment to run at, or NA for the shipped one.
#' @return The arm's response rows.
#'
#' @details Each arm is written as it completes and read back rather than re-run
#'   on a later invocation, so an environment that reclaims its filesystem or a
#'   host that stops the process costs the arm in flight rather than the whole
#'   measurement. This experiment is 150 replication-years, long enough that
#'   losing it to an interruption is a real cost; `scripts/screen_cache.sh`
#'   exists for the same reason on the sensitivity screen. Delete the arm files
#'   to force a fresh measurement.
measure_or_resume <- function(policy_days, hold_beds = NA_integer_) {
  path <- arm_path(policy_days, hold_beds)
  label <- sprintf("Evacuation policy %d days%s", policy_days,
                   if (is.na(hold_beds)) "" else sprintf(" at %d beds", hold_beds))
  if (file.exists(path)) {
    rows <- read.csv(path, stringsAsFactors = FALSE)
    if (nrow(rows) == opt$iterations) {
      message(sprintf("%s: resumed %d replications from %s", label, nrow(rows), path))
      return(rows)
    }
    message(sprintf("%s: discarding %d of %d checkpointed", label, nrow(rows),
                    opt$iterations))
  }
  rows <- measure_arm(policy_days, hold_beds)
  write.csv(rows, path, row.names = FALSE)
  rows
}

# The grid is the cross product of the two axes. With no establishment given it
# is the policy axis alone at the shipped establishment, which is the published
# experiment; with one policy and several establishments it is the substitution
# L32 records as unmeasured; with several of each it is the interaction between
# them, which is what a force structure review would ask for and is the
# expensive shape.
grid <- expand.grid(policy_days = policies, hold_beds = establishments,
                    KEEP.OUT.ATTRS = FALSE)

per_replication <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  measure_or_resume(grid$policy_days[i], grid$hold_beds[i])
}))

summary_rows <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  policy_days <- grid$policy_days[i]
  hold_beds   <- grid$hold_beds[i]
  arm <- per_replication[per_replication$policy_days == policy_days &
                           (is.na(hold_beds) | per_replication$hold_beds == hold_beds), ]
  cbind(data.frame(policy_days = policy_days, hold_beds = hold_beds),
        summarise_policy(arm))
}))

#' Responses the paired comparison against the shipped policy is reported for
PAIRED_RESPONSES <- c("total_dow", "total_rtd", "role4_peak", "hold_occupancy",
                      "icu_occupancy", "post_definitive_icu_share")

#' Half-width each paired response is sized against, in its own units
#'
#' @details Chosen as the smallest difference that would change a planning
#'   decision for each response: one death per campaign-year, ten returns to
#'   duty, five beds at the national support base, and one percentage point on
#'   each of the three shares.
PAIRED_HALF_WIDTHS <- c(total_dow = 1, total_rtd = 10, role4_peak = 5,
                        hold_occupancy = 0.01, icu_occupancy = 0.01,
                        post_definitive_icu_share = 0.01)

#' Policy the paired differences are measured against
BASELINE_POLICY <- 21L

paired <- NULL
if (BASELINE_POLICY %in% policies && length(policies) > 1) {
  others <- setdiff(policies, BASELINE_POLICY)
  paired <- do.call(rbind, lapply(others, function(policy_days) {
    do.call(rbind, lapply(PAIRED_RESPONSES, function(response) {
      row <- policy_paired_difference(per_replication, response,
                                      BASELINE_POLICY, policy_days)
      half_width <- PAIRED_HALF_WIDTHS[[response]]
      row$reps_needed <- policy_replications_for(per_replication, response,
                                                 BASELINE_POLICY, policy_days,
                                                 half_width)
      row
    }))
  }))
}

write.csv(per_replication,
          file.path(OUTPUT_DIR, sprintf("%s_replications.csv", OUTPUT_STEM)),
          row.names = FALSE)
write.csv(summary_rows, file.path(OUTPUT_DIR, sprintf("%s.csv", OUTPUT_STEM)),
          row.names = FALSE)
if (!is.null(paired)) {
  write.csv(paired, file.path(OUTPUT_DIR, sprintf("%s_paired.csv", OUTPUT_STEM)),
            row.names = FALSE)
}
message(sprintf("Policy sweep responses, summary and paired differences written to %s",
                OUTPUT_DIR))

#' Print one response's swept means as a markdown table row
#'
#' @param response Name of the response to print.
#' @param label Column label for the response.
#' @param scale Multiplier applied before printing, 100 for a share.
#' @param digits Decimal places to print.
#' @return Invisible NULL.
print_row <- function(response, label, scale = 1, digits = 2) {
  cells <- vapply(seq_len(nrow(grid)), function(i) {
    r <- summary_rows[summary_rows$policy_days == grid$policy_days[i] &
                        (is.na(grid$hold_beds[i]) |
                           summary_rows$hold_beds == grid$hold_beds[i]) &
                        summary_rows$response == response, ]
    if (nrow(r) != 1 || is.na(r$mean)) return("n/a")
    sprintf("%.*f [%.*f, %.*f]", digits, scale * r$mean, digits,
            scale * r$ci_lower, digits, scale * r$ci_upper)
  }, character(1))
  cat(sprintf("| %s | %s |\n", label, paste(cells, collapse = " | ")))
  invisible(NULL)
}

#' Column heading for each point of the grid
#'
#' @details Names whichever axes vary, so a policy sweep reads as days, an
#'   establishment sweep as bed counts, and a joint sweep as both.
grid_labels <- vapply(seq_len(nrow(grid)), function(i) {
  parts <- character(0)
  if (length(policies) > 1 || !SWEEPING_ESTABLISHMENT) {
    parts <- c(parts, sprintf("%d d", grid$policy_days[i]))
  }
  if (SWEEPING_ESTABLISHMENT) {
    parts <- c(parts, sprintf("%d beds", grid$hold_beds[i]))
  }
  paste(parts, collapse = ", ")
}, character(1))

cat("\n| Response |", paste(grid_labels, collapse = " | "), "|\n")
cat("|---", strrep("|---", nrow(grid)), "|\n", sep = "")
print_row("hold_occupancy", "R2E hold occupancy (%)", 100, 1)
print_row("hold_mean_queue", "R2E hold mean queue", 1, 2)
print_row("icu_occupancy", "R2E ICU occupancy (%)", 100, 1)
print_row("icu_mean_queue", "R2E ICU mean queue", 1, 2)
print_row("post_definitive_icu_share", "Post-definitive ICU access (%)", 100, 1)
print_row("in_theatre_share", "In-theatre share (%)", 100, 1)
print_row("total_rtd", "Returns to duty", 1, 1)
print_row("total_dow", "Died of wounds", 1, 2)
print_row("never_evacuated", "Never evacuated by horizon", 1, 1)
print_row("mean_evac_wait_days", "Mean evacuation wait (d)", 1, 2)
print_row("role4_sustained", "Role 4 sustained beds", 1, 1)
print_row("role4_peak", "Role 4 peak beds", 1, 1)

if (!is.null(paired)) {
  cat("\nPaired differences against the shipped", BASELINE_POLICY, "day policy:\n\n")
  cat("| Response | To | Difference | 95% CI | p | Reps for half-width |\n")
  cat("|---|---|---|---|---|---|\n")
  for (i in seq_len(nrow(paired))) {
    r <- paired[i, ]
    cat(sprintf("| %s | %d d | %+.2f | [%+.2f, %+.2f] | %.3f | %s |\n",
                r$response, r$to, r$difference, r$ci_lower, r$ci_upper, r$p_value,
                if (is.na(r$reps_needed)) "n/a" else format(r$reps_needed)))
  }
}
