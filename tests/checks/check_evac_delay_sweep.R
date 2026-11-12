#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_evac_delay_sweep.R                                    ##
## Regression check: the forward evacuation delay holds no vehicle, and its ##
## sweep's parameters, responses and published section agree               ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_evac_delay_sweep.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The forward evacuation delay represents casualties
# held forward under prolonged casualty care while the evacuation axis is
# closed. It means that only if the wait holds no vehicle, so the ambulance is
# not tied up for days, and if switching it off leaves the shipped model as it
# was; and its sweep is only the documented experiment if the protocol, the
# tracked files and the published tables agree.
#
# What this asserts:
#
#   1. Every protocol parameter in R/evac_delay_sweep.R equals the value
#      docs/Methods.md documents in a marker comment, and the shipped
#      configuration ships the delay disabled.
#   2. Each tracked evidence set, the shipped configuration's and the
#      high-intensity profile's, carries every documented arm at the documented
#      replication count numbered from one and the documented responses, with
#      the summary, the paired differences and the queue classification each the
#      reduction of the evidence beside them.
#   3. In the evidence, deaths while held forward rise with the delay and are
#      zero without it, the time to first incision grows by at least most of
#      the mean delay, and the PMV ambulance fleet's utilisation does not grow
#      with the delay.
#   4. The arm without a delay reproduces the shipped configuration: replication
#      by replication it equals the triage share sweep's shipped arm, run on the
#      same protocol and control seed.
#   5. In a short live run with the delay on, no casualty boards a PMV ambulance
#      before its wait has ended, and with it off no casualty is held.
#   6. Results.md prints the sweep's tables for both intensities as generated
#      spans, and the setter refuses a delay it cannot apply.

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
source("R/evac_delay_sweep.R")

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
EVIDENCE_DIR <- file.path("data", "evac_delay")

#' Directory of the triage share sweep, whose shipped arm the arm without a
#' delay must reproduce
TRIAGE_EVIDENCE_DIR <- file.path("data", "triage_split")

#' Scenario suffixes of the two tracked evidence sets
SUFFIXES <- c(moderate = "", high = "_high_intensity")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Share of the mean delay the time to first incision must at least grow by
INCISION_SHARE <- 0.8

#' Rise in PMV fleet utilisation over the shipped arm that would show a vehicle held
PMV_TOLERANCE <- 0.02

#' Campaign length of the live run, in days
LIVE_DAYS <- 20L

#' Nominal delay of the live run, in hours
LIVE_HOURS <- 72

#' Responses the arm without a delay must share with the triage sweep's shipped arm
SHARED_RESPONSES <- c("total_dow", "total_rtd", "theatre_mean_queue", "hold_mean_queue",
                      "icu_occupancy", "overflow_r2e_post_op", "overflow_r2e_staging",
                      "first_incision_hours", "diverted_offshift", "treated_dow_rate",
                      "role4_peak_icu")

# ── 1. The code's parameters are the ones the methods paper documents ───────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one protocol parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "EVAC_DELAY ".
#' @return The marker's value as a character string, or NA where absent.
marker <- function(name) {
  pattern <- sprintf("<!-- EVAC_DELAY %s=[^>]+ -->", name)
  m <- regmatches(methods_text, regexpr(pattern, methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- EVAC_DELAY [^=]+=(.*) -->$", "\\1", m)
}

report(identical(as.integer(marker("replications")), EVAC_DELAY_SWEEP_REPLICATIONS),
       "replications: code %d, methods paper %s", EVAC_DELAY_SWEEP_REPLICATIONS,
       marker("replications"))
report(identical(as.integer(marker("days")), EVAC_DELAY_SWEEP_DAYS),
       "days: code %d, methods paper %s", EVAC_DELAY_SWEEP_DAYS, marker("days"))
report(identical(as.integer(marker("seed")), EVAC_DELAY_SWEEP_SEED),
       "seed: code %d, methods paper %s", EVAC_DELAY_SWEEP_SEED, marker("seed"))
report(identical(as.integer(marker("window_days")), EVAC_DELAY_SWEEP_WINDOW_DAYS),
       "closing window: code %d, methods paper %s", EVAC_DELAY_SWEEP_WINDOW_DAYS,
       marker("window_days"))
report(identical(marker("baseline"), EVAC_DELAY_SWEEP_BASELINE_ARM),
       "baseline arm: code '%s', methods paper '%s'", EVAC_DELAY_SWEEP_BASELINE_ARM,
       marker("baseline"))
report(identical(marker("hours"), paste(EVAC_DELAY_SWEEP_HOURS, collapse = ";")),
       "nominal delays: code '%s', methods paper '%s'",
       paste(EVAC_DELAY_SWEEP_HOURS, collapse = ";"), marker("hours"))
report(identical(marker("spread"), paste(EVAC_DELAY_SPREAD, collapse = ";")),
       "delay spread: code '%s', methods paper '%s'",
       paste(EVAC_DELAY_SPREAD, collapse = ";"), marker("spread"))

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
described <- build_environment(resolve_scenario(json_data, "default"))
report(identical(as.numeric(described$vars$r1$evacuation_delay$enabled), 0),
       "the shipped configuration ships the forward evacuation delay disabled")

# ── 2. The tracked evidence sets are that experiment ────────────────────────

cat("\n-- the tracked evidence sets carry the documented design --\n")

#' Read one tracked file of a sweep
#'
#' @param stem File stem without the scenario suffix or extension.
#' @param suffix Scenario suffix.
#' @param dir Evidence directory (default the delay sweep's).
#' @return The data frame, or NULL where the file is absent.
read_sweep <- function(stem, suffix, dir = EVIDENCE_DIR) {
  path <- file.path(dir, paste0(stem, suffix, ".csv"))
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE)
}

#' Whether two data frames agree on every numeric column to a tolerance
#'
#' @param a,b Data frames of the same shape.
#' @return TRUE where every numeric column agrees within TOL.
numerics_agree <- function(a, b) {
  cols <- names(a)[vapply(a, is.numeric, logical(1))]
  all(vapply(cols, function(cl) {
    isTRUE(all.equal(a[[cl]], b[[cl]], tolerance = TOL, check.attributes = FALSE))
  }, logical(1)))
}

#' Whether a summary file is the reduction of the per-replication responses
#'
#' @param rows Per-replication responses of every arm.
#' @param summ The tracked summary.
#' @return TRUE where every arm and response agrees.
summary_agrees <- function(rows, summ) {
  recomputed <- do.call(rbind, lapply(EVAC_DELAY_SWEEP_ARMS$arm, function(a) {
    cbind(data.frame(arm = a),
          summarise_policy(rows[rows$arm == a, setdiff(names(rows), "arm")],
                           arm_column = "arm_placeholder"))
  }))
  key <- paste(summ$arm, summ$response)
  again <- paste(recomputed$arm, recomputed$response)
  identical(sort(key), sort(again)) &&
    numerics_agree(summ[order(key), ], recomputed[order(again), ])
}

#' Whether a paired file is the within-replication difference of the responses
#'
#' @param rows Per-replication responses of every arm.
#' @param paired The tracked paired differences.
#' @return TRUE where every arm and response agrees and no row is missing.
paired_agrees <- function(rows, paired) {
  base <- EVAC_DELAY_SWEEP_BASELINE_ARM
  others <- setdiff(EVAC_DELAY_SWEEP_ARMS$arm, base)
  ok <- nrow(paired) == length(others) * length(EVAC_DELAY_SWEEP_PAIRED_RESPONSES)
  for (a in others) for (response in EVAC_DELAY_SWEEP_PAIRED_RESPONSES) {
    mine <- policy_paired_difference(rows, response, base, a, arm_column = "arm")
    theirs <- paired[paired$to == a & paired$response == response, ]
    ok <- ok && nrow(theirs) == 1L &&
      isTRUE(all.equal(mine$difference, theirs$difference, tolerance = TOL)) &&
      isTRUE(all.equal(mine$ci_lower, theirs$ci_lower, tolerance = TOL))
  }
  ok
}

#' Every arm's tracked daily series, bound together with its arm label
#'
#' @param suffix Scenario suffix.
#' @return The series, or NULL where any arm's file is absent.
read_series <- function(suffix) {
  hours <- as.integer(EVAC_DELAY_SWEEP_ARMS$hours)
  stems <- sprintf("evac_delay_sweep_arm_h%03d%s_series.csv.gz", hours, suffix)
  files <- file.path(EVIDENCE_DIR, stems)
  if (!all(file.exists(files))) return(NULL)
  do.call(rbind, lapply(seq_along(files), function(i) {
    d <- read.csv(files[i], stringsAsFactors = FALSE)
    d$arm <- EVAC_DELAY_SWEEP_ARMS$arm[i]
    d
  }))
}

#' Each arm's mean of one response, in ascending order of delay
#'
#' @param rows Per-replication responses of every arm.
#' @param response Response column.
#' @return Named numeric vector, one mean per arm.
arm_means <- function(rows, response) {
  vapply(EVAC_DELAY_SWEEP_ARMS$arm, function(a) mean(rows[[response]][rows$arm == a]),
         numeric(1))
}

for (label in names(SUFFIXES)) {
  suffix <- SUFFIXES[[label]]
  rows <- read_sweep("evac_delay_sweep_replications", suffix)
  summ <- read_sweep("evac_delay_sweep_summary", suffix)
  paired <- read_sweep("evac_delay_sweep_paired", suffix)
  stab <- read_sweep("evac_delay_sweep_stability", suffix)
  present <- !is.null(rows) && !is.null(summ) && !is.null(paired) && !is.null(stab)
  report(present, "%s: the replications, summary, paired and stability files are tracked",
         label)
  if (!present) next

  report(setequal(unique(rows$arm), EVAC_DELAY_SWEEP_ARMS$arm),
         "%s: the arms are the documented seven delays (%s)", label,
         paste(sort(unique(rows$arm)), collapse = "; "))
  per_arm <- table(rows$arm)
  report(all(per_arm == EVAC_DELAY_SWEEP_REPLICATIONS),
         "%s: every arm carries %d replications (%s)", label,
         EVAC_DELAY_SWEEP_REPLICATIONS, paste(per_arm, collapse = ", "))
  numbered <- all(vapply(split(rows$replication, rows$arm), function(r) {
    identical(sort(as.integer(r)), seq_len(EVAC_DELAY_SWEEP_REPLICATIONS))
  }, logical(1)))
  report(numbered, "%s: replications are numbered from one in every arm", label)
  needed <- c(EVAC_DELAY_SWEEP_PAIRED_RESPONSES, "held_forward", "mean_delay_days")
  report(all(needed %in% names(rows)), "%s: the response set is present (missing: %s)", label,
         paste(setdiff(needed, names(rows)), collapse = ", "))
  report(summary_agrees(rows, summ),
         "%s: the summary is the reduction of the per-replication responses", label)
  report(paired_agrees(rows, paired),
         "%s: every paired difference is taken within replication against '%s'", label,
         EVAC_DELAY_SWEEP_BASELINE_ARM)

  series <- read_series(suffix)
  report(!is.null(series), "%s: every arm's daily series is tracked", label)
  if (!is.null(series)) {
    st <- evac_delay_stability(series)
    st <- st[match(paste(stab$arm, stab$pool), paste(st$arm, st$pool)), ]
    report(identical(as.character(st$stability), as.character(stab$stability)) &&
             numerics_agree(stab, st[, names(stab)]) &&
             setequal(unique(stab$pool), EVAC_DELAY_SWEEP_POOLS),
           "%s: the stability file is the classification of every pool's tracked series", label)
  }

  # ── 3. The delay does what it represents ─────────────────────────────────
  died <- arm_means(rows, "died_held_forward")
  report(died[[1]] == 0 && all(diff(died) >= 0),
         "%s: deaths while held forward are zero without a delay and rise with it (%s)",
         label, paste(sprintf("%.1f", died), collapse = ", "))
  incision_rise <- arm_means(rows, "first_incision_hours") -
    arm_means(rows, "first_incision_hours")[[1]]
  delay_hours <- arm_means(rows, "mean_delay_days") * 24
  report(all((incision_rise >= INCISION_SHARE * delay_hours)[-1]),
         "%s: time to first incision grows by at least %.0f%% of the mean delay in every arm",
         label, 100 * INCISION_SHARE)
  pmv <- arm_means(rows, "pmv_utilisation")
  report(all(pmv <= pmv[[1]] + PMV_TOLERANCE),
         "%s: PMV ambulance utilisation does not grow with the delay (%s)", label,
         paste(sprintf("%.3f", pmv), collapse = ", "))

  # ── 4. The arm without a delay reproduces the shipped configuration ──────
  triage <- read_sweep("triage_sweep_replications", suffix, TRIAGE_EVIDENCE_DIR)
  if (is.null(triage)) {
    report(FALSE, "%s: the triage share sweep to compare against is tracked", label)
    next
  }
  mine <- rows[rows$arm == EVAC_DELAY_SWEEP_BASELINE_ARM, ]
  mine <- mine[order(mine$replication), SHARED_RESPONSES]
  theirs <- triage[triage$arm == TRIAGE_SWEEP_BASELINE_ARM, ]
  theirs <- theirs[order(theirs$replication), SHARED_RESPONSES]
  report(nrow(mine) == nrow(theirs) && numerics_agree(mine, theirs),
         paste("%s: the arm without a delay equals the triage sweep's shipped arm,",
               "replication by replication, on %d responses"), label, length(SHARED_RESPONSES))
}

# ── 5. A live run holds no vehicle during the wait ──────────────────────────

cat("\n-- a short live run with the delay on and off --\n")

#' Run one short campaign under a described configuration
#'
#' @param config Described configuration to run.
#' @return The wrapped simmer environment.
live_run <- function(config) {
  assign("env_data", config, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(config$elms, length), envir = globalenv())
  invisible(capture.output(suppressWarnings(suppressMessages(
    env <- run_once(LIVE_DAYS, seed = EVAC_DELAY_SWEEP_SEED, write_files = FALSE)
  ))))
  env
}
env_on <- live_run(set_evacuation_delay(described, LIVE_HOURS))
attrs <- simmer::get_mon_attributes(env_on)
held <- attrs[attrs$key == "evac_delay_start", c("name", "value")]
names(held)[2] <- "start"
mins <- attrs[attrs$key == "evac_delay_min", c("name", "value")]
names(mins)[2] <- "minutes"
held <- merge(held, mins, by = "name")
boarding <- simmer::get_mon_arrivals(env_on, per_resource = TRUE)
boarding <- boarding[grepl("^t_PMVAmb_", boarding$resource), c("name", "start_time")]
boarded <- merge(held, boarding, by = "name")
report(nrow(held) > 0 && nrow(boarded) > 0 &&
         all(boarded$start_time >= boarded$start + boarded$minutes - TOL),
       paste("with a %d-hour delay, %d casualties are held and none of the %d who board",
             "a PMV ambulance boards before the wait ends"),
       LIVE_HOURS, nrow(held), nrow(boarded))
env_off <- live_run(described)
report(!any(simmer::get_mon_attributes(env_off)$key == "evac_delay_min"),
       "with the delay off, no casualty is held forward")

# ── 6. The results paper and the setter ─────────────────────────────────────

cat("\n-- the results paper prints the sweep's tables --\n")

paper <- paste(readLines(PAPER_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (name in c("evac_delay", "evac_delay_high", "evac_delay_paired",
               "evac_delay_paired_high")) {
  report(grepl(sprintf("<!-- GEN %s -->", name), paper, fixed = TRUE),
         "Results.md prints the generated table '%s'", name)
}

cat("\n-- the delay setter --\n")

set <- set_evacuation_delay(described, 168)
d <- set$vars$r1$evacuation_delay
expected <- 168 * 60 * c(EVAC_DELAY_SPREAD[1], 1, EVAC_DELAY_SPREAD[2])
report(identical(as.numeric(d$enabled), 1) &&
         isTRUE(all.equal(c(d$min, d$mode, d$max), expected)),
       "a seven-day delay is enabled and triangular about its nominal value")
off <- set_evacuation_delay(set, 0)
report(identical(as.numeric(off$vars$r1$evacuation_delay$enabled), 0),
       "a delay of zero disables it")
#' A described configuration with the delay block removed
#'
#' @param x Described configuration.
#' @return The configuration without `vars.r1.evacuation_delay`.
strip <- function(x) {
  x$vars$r1$evacuation_delay <- NULL
  x
}
report(identical(strip(set), strip(described)),
       "and the setter leaves every other variable and every element as it was")
for (bad in list(-1, NA_real_, c(6, 24), "24", Inf)) {
  report(inherits(try(set_evacuation_delay(described, bad), silent = TRUE), "try-error"),
         "a delay of '%s' is rejected", paste(format(bad), collapse = ","))
}
report(inherits(try(set_evacuation_delay(strip(described), 24), silent = TRUE), "try-error"),
       "a configuration without the delay is rejected, not left unchanged")

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat(paste("\nThe forward evacuation delay holds no vehicle, and its sweep's protocol,",
          "evidence and published section agree.\n"))
quit(status = 0L)
