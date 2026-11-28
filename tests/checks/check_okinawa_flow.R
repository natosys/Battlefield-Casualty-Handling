#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_okinawa_flow.R                                        ##
## Regression check: the Okinawa flow and capacity arms set what they claim, ##
## and the experiment's protocol, evidence and published section agree      ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_okinawa_flow.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The experiment brings the high-intensity profile's
# evacuation tempo, beds, surgery and surgical pathway towards Okinawa's one at
# a time and reads the treated-cohort died-of-wounds rate against Okinawa's
# 3.4%. It means that only if each arm changes exactly the establishment or
# variable it names and nothing else, if a setter that cannot find its target
# fails rather than leaving the shipped value in force, and if the tracked
# evidence is the documented protocol.
#
# What this asserts:
#
#   1. Every protocol parameter in R/okinawa_flow.R equals the value
#      docs/Methods.md documents in a marker comment.
#   2. Each arm, applied to the high-intensity configuration, sets the sortie
#      interval, the R2B and R2E intensive care and holding beds, theatres and
#      surgical sections, the R2E section count, the damage control rates and
#      the whole establishment exactly as OKINAWA_FLOW_ARMS states, and leaves
#      every other count as shipped.
#   3. The setters refuse a factor they cannot apply, a pool they cannot find
#      and a configuration without the variable they set.
#   4. The tracked evidence set carries every arm at the documented replication
#      counts under both protocols, its summaries are the reduction of the rows
#      beside them, and the shipped arm's anchor rate reproduces the
#      high-intensity rate in data/calibration/dow_calibration_anchor.csv.
#   5. Results.md prints the experiment's tables as generated spans.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/long_horizon.R")
source("R/policy_sweep.R")
source("R/hold_window.R")
source("R/airlift.R")
source("R/surgical_team_sweep.R")
source("R/icu_bed_sweep.R")
source("R/triage_sweep.R")
source("R/dow_calibration.R")
source("R/okinawa_flow.R")

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
#' @details Anything other than TRUE is a failure, NA included.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' The methods paper, which documents the experiment's design
METHODS_PATH <- file.path("docs", "Methods.md")

#' The results paper, which prints the experiment's tables
PAPER_PATH <- file.path("docs", "Results.md")

#' Directory of the tracked evidence set
EVIDENCE_DIR <- file.path("data", "okinawa_flow")

#' The calibration check's tracked anchor measurements
ANCHOR_PATH <- file.path("data", "calibration", "dow_calibration_anchor.csv")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Generated spans Results.md must carry for the experiment
SPANS <- c("table:okinawa_flow", "table:okinawa_flow_paired")

# ── 1. The code's parameters are the ones the methods paper documents ───────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one protocol parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "OKINAWA_FLOW ".
#' @return The marker's value as a character string, or NA where absent.
marker <- function(name) {
  pattern <- sprintf("<!-- OKINAWA_FLOW %s=[^>]+ -->", name)
  m <- regmatches(methods_text, regexpr(pattern, methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- OKINAWA_FLOW [^=]+=(.*) -->$", "\\1", m)
}

protocol <- list(
  replications = OKINAWA_FLOW_REPLICATIONS, days = OKINAWA_FLOW_DAYS,
  seed = OKINAWA_FLOW_SEED, window_days = OKINAWA_FLOW_WINDOW_DAYS,
  anchor_days = OKINAWA_FLOW_ANCHOR_DAYS, anchor_replications = OKINAWA_FLOW_ANCHOR_REPLICATIONS,
  anchor_seeds = paste(OKINAWA_FLOW_ANCHOR_SEEDS, collapse = ";"),
  baseline = OKINAWA_FLOW_BASELINE_ARM, scenario = OKINAWA_FLOW_SCENARIO,
  arms = paste(OKINAWA_FLOW_ARMS$stem, collapse = ";")
)
for (name in names(protocol)) {
  report(identical(marker(name), as.character(protocol[[name]])),
         "%s: code '%s', methods paper '%s'", name, protocol[[name]], marker(name))
}

# ── 2. Each arm sets what it names and nothing else ─────────────────────────

cat("\n-- each arm sets exactly what it names --\n")

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
shipped <- resolve_scenario(json_data, OKINAWA_FLOW_SCENARIO)

#' Every establishment count of a resolved configuration, keyed by location
#'
#' @param resolved Configuration as `resolve_scenario()` returns it.
#' @return A named integer vector: `<elm>/sub/<sub_elm>`, `<elm>/bed/<name>`
#'   and `transport/<name>`.
counts_of <- function(resolved) {
  out <- integer(0)
  for (e in resolved$elms) {
    for (s in e$sub_elms) out[sprintf("%s/sub/%s", e$elm, s$sub_elm)] <- as.integer(s$qty)
    for (b in e$beds) out[sprintf("%s/bed/%s", e$elm, b$name)] <- as.integer(b$qty)
  }
  for (t in resolved$transports) out[sprintf("transport/%s", t$name)] <- as.integer(t$qty)
  out
}

#' The counts an arm should produce from the shipped counts
#'
#' @param base Shipped counts, as counts_of() returns them.
#' @param arm One row of OKINAWA_FLOW_ARMS.
#' @return The expected counts.
expected_counts <- function(base, arm) {
  out <- base * arm$all_scale
  r2 <- "^(r2b|r2eheavy)/"
  beds <- grepl(paste0(r2, "bed/(icu|hold)$"), names(out))
  out[beds] <- out[beds] * arm$bed_scale
  surg <- grepl(paste0(r2, "(bed/ot|sub/surg)$"), names(out))
  out[surg] <- out[surg] * arm$surgery_scale
  if (!is.na(arm$r2e_sections)) out["r2eheavy/sub/surg"] <- arm$r2e_sections
  out
}

base_counts <- counts_of(shipped)
base_described <- build_environment(shipped)
for (i in seq_len(nrow(OKINAWA_FLOW_ARMS))) {
  arm <- OKINAWA_FLOW_ARMS[i, ]
  got <- counts_of(set_okinawa_establishment(shipped, arm))
  want <- expected_counts(base_counts, arm)
  report(identical(got, want), "%s: every establishment count is as the arm states", arm$arm)
  described <- set_okinawa_variables(base_described, arm)
  report(identical(described$vars$role4$ame$schedule_interval_days, arm$interval_days),
         "%s: sortie interval %g days", arm$arm, arm$interval_days)
  dcs <- vapply(sprintf("pri%d_dcs_rate", 1:3),
                function(k) as.numeric(described$vars$r1$other[[k]]), numeric(1))
  shipped_dcs <- vapply(sprintf("pri%d_dcs_rate", 1:3),
                        function(k) as.numeric(base_described$vars$r1$other[[k]]), numeric(1))
  report(identical(unname(dcs), if (arm$single_stage) c(0, 0, 0) else unname(shipped_dcs)),
         "%s: damage control rates %s", arm$arm, paste(dcs, collapse = "/"))
}
report(identical(OKINAWA_FLOW_ARMS$arm[1], OKINAWA_FLOW_BASELINE_ARM) &&
         identical(unlist(OKINAWA_FLOW_ARMS[1, c("bed_scale", "surgery_scale", "all_scale")],
                          use.names = FALSE), c(1L, 1L, 1L)) &&
         OKINAWA_FLOW_ARMS$interval_days[1] ==
           base_described$vars$role4$ame$schedule_interval_days,
       "the baseline arm is the shipped high-intensity configuration")

# ── 3. The setters refuse what they cannot apply ────────────────────────────

cat("\n-- the setters refuse what they cannot apply --\n")

for (bad in list(0, 1.5, NA_real_, c(2, 3), "2")) {
  report(inherits(try(scale_establishment(shipped, "beds", "icu", bad), silent = TRUE),
                  "try-error"),
         "a factor of '%s' is rejected", paste(format(bad), collapse = ","))
}
report(inherits(try(scale_establishment(shipped, "beds", "no_such_pool", 2), silent = TRUE),
                "try-error"),
       "a pool that does not exist is rejected, not left unchanged")
no_interval <- base_described
no_interval$vars$role4$ame$schedule_interval_days <- NULL
report(inherits(try(set_okinawa_variables(no_interval, OKINAWA_FLOW_ARMS[2, ]), silent = TRUE),
                "try-error"),
       "a configuration without a sortie interval is rejected")
no_dcs <- base_described
no_dcs$vars$r1$other$pri1_dcs_rate <- NULL
report(inherits(try(set_okinawa_variables(no_dcs, OKINAWA_FLOW_ARMS[5, ]), silent = TRUE),
                "try-error"),
       "a configuration without a damage control rate is rejected for a single-stage arm")

# ── 4. The tracked evidence set is the documented protocol ──────────────────

cat("\n-- the tracked evidence set carries the documented design --\n")

#' Read one tracked file of the experiment
#'
#' @param stem File stem without extension.
#' @return The data frame, or NULL where the file is absent.
read_evidence <- function(stem) {
  path <- file.path(EVIDENCE_DIR, paste0(stem, ".csv"))
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE)
}

rows <- read_evidence("okinawa_flow_replications")
summ <- read_evidence("okinawa_flow_summary")
anchor <- read_evidence("okinawa_flow_anchor_replications")
anchor_summ <- read_evidence("okinawa_flow_anchor_summary")
report(!is.null(rows) && !is.null(summ) && !is.null(anchor) && !is.null(anchor_summ) &&
         !is.null(read_evidence("okinawa_flow_paired")) &&
         !is.null(read_evidence("okinawa_flow_stability")),
       "every tracked file is present in %s", EVIDENCE_DIR)

if (!is.null(rows) && !is.null(anchor)) {
  for (a in OKINAWA_FLOW_ARMS$arm) {
    r <- rows[rows$arm == a, ]
    report(identical(sort(as.integer(r$replication)), seq_len(OKINAWA_FLOW_REPLICATIONS)),
           "%s: %d sustained replications numbered from one", a, OKINAWA_FLOW_REPLICATIONS)
    an <- anchor[anchor$arm == a, ]
    report(nrow(an) == OKINAWA_FLOW_ANCHOR_REPLICATIONS * length(OKINAWA_FLOW_ANCHOR_SEEDS) &&
             setequal(an$seed, OKINAWA_FLOW_ANCHOR_SEEDS),
           "%s: %d anchor replications under each documented seed", a,
           OKINAWA_FLOW_ANCHOR_REPLICATIONS)
  }
}
if (!is.null(anchor) && !is.null(anchor_summ)) {
  rebuilt <- summarise_okinawa_anchor(anchor)
  report(identical(rebuilt$arm, anchor_summ$arm) &&
           isTRUE(all.equal(rebuilt$rate, anchor_summ$rate, tolerance = TOL)) &&
           isTRUE(all.equal(rebuilt$ci_upper, anchor_summ$ci_upper, tolerance = TOL)),
         "the anchor summary is the reduction of the anchor replications")
  calib <- read.csv(ANCHOR_PATH, stringsAsFactors = FALSE)
  hi <- calib[calib$scenario == OKINAWA_FLOW_SCENARIO, ]
  ship <- anchor_summ[anchor_summ$arm == OKINAWA_FLOW_BASELINE_ARM, ]
  report(nrow(hi) == 1L && isTRUE(all.equal(ship$rate, hi$rate, tolerance = TOL)) &&
           hi$days == OKINAWA_FLOW_ANCHOR_DAYS,
         "the shipped arm's anchor rate %.4f%% reproduces the calibration check's %.4f%%",
         100 * ship$rate, 100 * hi$rate)
}
if (!is.null(rows) && !is.null(summ)) {
  ok <- all(vapply(OKINAWA_FLOW_ARMS$arm, function(a) {
    s <- summ$mean[summ$arm == a & summ$response == "treated_dow_rate"]
    isTRUE(all.equal(s, mean(rows$treated_dow_rate[rows$arm == a], na.rm = TRUE),
                     tolerance = TOL))
  }, logical(1)))
  report(ok, "the sustained summary's treated-cohort rate is the mean of the replications")
}

# ── 5. The published section prints the experiment's tables ─────────────────

cat("\n-- the results paper prints the experiment's tables --\n")

paper <- paste(readLines(PAPER_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (name in SPANS) {
  report(grepl(sprintf("<!-- GEN %s -->", name), paper, fixed = TRUE),
         "Results.md carries the generated span %s", name)
}

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat(paste("\nThe Okinawa flow and capacity arms set what they claim, and the experiment's",
          "protocol, evidence and published section agree.\n"))
quit(status = 0L)
