#!/usr/bin/env Rscript
##############################################################################
## scripts/check_high_intensity_evidence.R                                  ##
## Regression check — the high-intensity replications of the lever          ##
## experiments are the documented design and sit beside the default sets    ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_high_intensity_evidence.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Seven lever experiments were repeated under the
# high_intensity profile, each writing its own `_high_intensity` files beside
# the default set. The per-experiment protocol checks defend the default sets
# only, so nothing would notice a high-intensity run that carried the wrong
# replication count, left an arm out, or overwrote a default file.
#
# What this asserts:
#
#   1. The marker comments in docs/Methods.md state the profile, replication
#      count and horizon the tracked files carry.
#   2. Every experiment's tracked high-intensity per-replication file carries
#      each documented arm at the documented replication count, numbered from
#      one with no replication repeated.
#   3. The tracked summaries are the reduction of the per-replication files
#      beside them for one response of three experiments, so a summary is not
#      an independent claim.
#   4. Each experiment's default file is still tracked beside its
#      high-intensity file and differs from it, so the profile did not
#      overwrite the default set.
#   5. scenario_output_suffix() and scenario_output_path() are correct on
#      inputs whose answers are computable by hand.

source("R/scenario.R")

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
#'
#' @details Anything other than TRUE is a failure, NA included, so a quantity
#'   that becomes NA because the code under test is wrong is reported rather
#'   than raising an error that stops the run at the first such assertion.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' The methods paper, which documents the design
METHODS_PATH <- file.path("docs", "Methods.md")

#' Suffix the high-intensity files carry
HIGH_SUFFIX <- "_high_intensity"

#' Replications per arm the high-intensity sets carry
HIGH_REPLICATIONS <- 30L

#' The replicated experiments: label, per-replication file stem, arm column, arms
#'
#' @details Each entry names the file stem without the suffix, the column that
#'   identifies an arm and the arm values the design documents.
HIGH_EXPERIMENTS <- list(
  list(label = "hold window", stem = "data/hold_window/hold_window_replications",
       arm = "window_min", arms = c(0, 60)),
  list(label = "intensive care gate", stem = "data/icu_gate/icu_gate_replications",
       arm = "gate_enabled", arms = c(0, 1)),
  list(label = "casualty surge", stem = "data/casualty_surge/casualty_surge_replications",
       arm = "rate_per_day", arms = c(0, 0.2)),
  list(label = "saturation release", stem = "data/policy/saturation_sweep_replications",
       arm = "saturation_threshold", arms = c(0, 1, 2, 3, 5, 8, 12, 16, 24)),
  list(label = "surge event size", stem = "data/casualty_surge/casualty_surge_size_replications",
       arm = "size", arms = c(0, 10, 20, 40, 60, 90, 120, 180))
)

#' Read one marker the methods paper states for the high-intensity runs
#'
#' @param name Marker name, as it appears after "HIGHINT ".
#' @return The marker's value as a character string, or NA where absent.
highint_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- HIGHINT %s=[^ ]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- HIGHINT [^=]+=(.*) -->$", "\\1", m)
}

#' Read a tracked CSV, or NULL where it does not exist
#'
#' @param path The file path.
#' @return A data frame, or NULL.
read_tracked <- function(path) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE)
}

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

# ── 1. The design states what the files carry ────────────────────────────────

cat("-- the methods paper states the design --\n")

report(identical(highint_marker("scenario"), "high_intensity"),
       "the methods paper states the profile as high_intensity")
report(identical(suppressWarnings(as.integer(highint_marker("replications"))), HIGH_REPLICATIONS),
       "the methods paper states %d replications", HIGH_REPLICATIONS)
report(identical(suppressWarnings(as.integer(highint_marker("days"))), 360L),
       "the methods paper states a 360-day horizon")

# ── 2. Every arm at the documented replication count ─────────────────────────

cat("\n-- every experiment carries each arm at the documented replications --\n")

for (e in HIGH_EXPERIMENTS) {
  d <- read_tracked(paste0(e$stem, HIGH_SUFFIX, ".csv"))
  if (is.null(d)) {
    report(FALSE, "%s: the high-intensity replications are tracked", e$label)
    next
  }
  counts <- vapply(e$arms, function(a) sum(abs(d[[e$arm]] - a) < 1e-9), numeric(1))
  report(all(counts == HIGH_REPLICATIONS) && nrow(d) == length(e$arms) * HIGH_REPLICATIONS,
         "%s: %d arms of %d replications each (found %s rows)", e$label,
         length(e$arms), HIGH_REPLICATIONS, format(nrow(d)))
  numbered <- vapply(e$arms, function(a) {
    r <- d$replication[abs(d[[e$arm]] - a) < 1e-9]
    identical(sort(as.integer(r)), seq_len(HIGH_REPLICATIONS))
  }, logical(1))
  report(all(numbered), "%s: replications are numbered 1 to %d within every arm",
         e$label, HIGH_REPLICATIONS)
}

hold_sweep <- read_tracked(paste0("data/sweeps/r2b_hold_threshold_sweep", HIGH_SUFFIX, ".csv"))
report(!is.null(hold_sweep) && nrow(hold_sweep) == 15L &&
         !anyDuplicated(hold_sweep[, c("hold_beds", "evac_threshold_min")]),
       "holding threshold sweep: fifteen distinct grid points are tracked")

# ── 3. Summaries are the reduction of the replications ───────────────────────

cat("\n-- the tracked summaries are the reduction of the replications --\n")

hw_rep <- read_tracked(paste0("data/hold_window/hold_window_replications", HIGH_SUFFIX, ".csv"))
hw_sum <- read_tracked(paste0("data/hold_window/hold_window_summary", HIGH_SUFFIX, ".csv"))
stated <- hw_sum$mean[hw_sum$window_min == 60 & hw_sum$response == "total_dow"]
report(length(stated) == 1L && abs(stated - mean(hw_rep$total_dow[hw_rep$window_min == 60])) < 1e-9,
       "hold window: the summary mean of deaths of wounds is the replications' mean")

sat_rep <- read_tracked(paste0("data/policy/saturation_sweep_replications", HIGH_SUFFIX, ".csv"))
sat_sum <- read_tracked(paste0("data/policy/saturation_sweep", HIGH_SUFFIX, ".csv"))
stated <- sat_sum$mean[sat_sum$saturation_threshold == 8 & sat_sum$response == "total_rtd"]
report(length(stated) == 1L &&
         abs(stated - mean(sat_rep$total_rtd[sat_rep$saturation_threshold == 8])) < 1e-9,
       "saturation: the summary mean of returns to duty is the replications' mean")

cs_dir <- file.path("data", "casualty_surge")
cs_rep <- read_tracked(paste0(cs_dir, "/casualty_surge_replications", HIGH_SUFFIX, ".csv"))
cs_sum <- read_tracked(paste0(cs_dir, "/casualty_surge_count_summary", HIGH_SUFFIX, ".csv"))

#' Rows of the arm with events injected
#'
#' @param d A data frame carrying `rate_per_day`.
#' @return A logical vector.
is_injected <- function(d) abs(d$rate_per_day - 0.2) < 1e-9
stated <- cs_sum$mean[is_injected(cs_sum) & cs_sum$response == "total_casualties"]
report(length(stated) == 1L &&
         abs(stated - mean(cs_rep$total_casualties[is_injected(cs_rep)])) < 1e-9,
       "casualty surge: the summary mean of total casualties is the replications' mean")

# ── 4. The default sets were not overwritten ─────────────────────────────────

cat("\n-- the default sets remain beside the high-intensity sets --\n")

for (e in HIGH_EXPERIMENTS) {
  base <- read_tracked(paste0(e$stem, ".csv"))
  high <- read_tracked(paste0(e$stem, HIGH_SUFFIX, ".csv"))
  report(!is.null(base) && !is.null(high) && !isTRUE(all.equal(base, high)),
         "%s: the default file is tracked and differs from the high-intensity file", e$label)
}

# ── 5. The path helpers ──────────────────────────────────────────────────────

cat("\n-- the scenario path helpers are correct --\n")

report(identical(scenario_output_suffix("default"), "") &&
         identical(scenario_output_suffix(NULL), ""),
       "the shipped configuration carries no suffix")
report(identical(scenario_output_suffix("high_intensity"), HIGH_SUFFIX),
       "a named profile carries its name")
report(identical(scenario_output_path("d", "stem", "high_intensity"),
                 "d/stem_high_intensity.csv") &&
         identical(scenario_output_path("d", "stem", "default", ".png"), "d/stem.png"),
       "scenario_output_path() joins directory, stem, suffix and extension")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All high-intensity evidence checks passed.\n")
quit(status = 0)
