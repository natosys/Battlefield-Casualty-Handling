#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_post_definitive_sweep.R                               ##
## Regression check — the post-definitive intensive care share sweep's      ##
## parameters, its responses and its published section agree               ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_post_definitive_sweep.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The shipped post-definitive intensive care share of 1
# is an upper bound no open-access source confirms, and the sweep that lowers it
# is the evidence for how far the intensive care findings depend on it. Its arms
# only measure the share if each changes the share alone and the shipped arm is
# the shipped configuration rather than a value near it, and a lower share only
# means anything if it sends casualties to the ward; and its tracked set is only
# the documented experiment if the protocol, the files and the published tables
# agree.
#
# What this asserts:
#
#   1. Every protocol parameter in R/post_definitive_sweep.R equals the value
#      docs/Methods.md documents in a marker comment, and the baseline arm's
#      share is the shipped `vars.r2eheavy.post_definitive_icu.share` exactly.
#   2. Each tracked evidence set, the shipped configuration's and the
#      high-intensity profile's, carries every documented arm at the documented
#      replication count numbered from one and the documented responses, with
#      the summary, the paired differences and the queue classification each the
#      reduction of the evidence beside them.
#   3. Ward recoveries rise as the share falls: none at the shipped share, and
#      more in every replication's mean at each lower share.
#   4. The shipped arm reproduces the shipped configuration: replication by
#      replication it equals the R2E intensive care bed sweep's shipped arm, run
#      on the same protocol and control seed; and two short replications with
#      the share set to 1 through the setter equal those without it, while a
#      share of 0.25 sends casualties to the ward.
#   5. Results.md prints the sweep's tables for both intensities as generated
#      spans.
#   6. The setter changes only the share and refuses a share it cannot apply.

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
source("R/post_definitive_sweep.R")

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
EVIDENCE_DIR <- file.path("data", "post_definitive")

#' Directory of the intensive care bed sweep, whose shipped arm the shipped
#' share arm must reproduce
ICU_EVIDENCE_DIR <- file.path("data", "icu_beds")

#' Scenario suffixes of the two tracked evidence sets
SUFFIXES <- c(moderate = "", high = "_high_intensity")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Campaign length of the short run the setter is exercised on, in days
LIVE_DAYS <- 10L

#' Responses the shipped arm must share with the intensive care bed sweep's
SHARED_RESPONSES <- c("total_dow", "total_rtd", "hold_occupancy", "hold_mean_queue",
                      "icu_occupancy", "icu_mean_queue", "post_definitive_icu_share",
                      "postop_degraded_share", "overflow_r2e_post_op", "overflow_r2e_staging",
                      "evac_decisions", "role4_peak", "mean_evac_wait_critical")

# ── 1. The code's parameters are the ones the methods paper documents ───────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one protocol parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "POST_DEFINITIVE ".
#' @return The marker's value as a character string, or NA where absent.
marker <- function(name) {
  pattern <- sprintf("<!-- POST_DEFINITIVE %s=[^>]+ -->", name)
  m <- regmatches(methods_text, regexpr(pattern, methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- POST_DEFINITIVE [^=]+=(.*) -->$", "\\1", m)
}

report(identical(as.integer(marker("replications")), POST_DEFINITIVE_SWEEP_REPLICATIONS),
       "replications: code %d, methods paper %s", POST_DEFINITIVE_SWEEP_REPLICATIONS,
       marker("replications"))
report(identical(as.integer(marker("days")), POST_DEFINITIVE_SWEEP_DAYS),
       "days: code %d, methods paper %s", POST_DEFINITIVE_SWEEP_DAYS, marker("days"))
report(identical(as.integer(marker("seed")), POST_DEFINITIVE_SWEEP_SEED),
       "seed: code %d, methods paper %s", POST_DEFINITIVE_SWEEP_SEED, marker("seed"))
report(identical(as.integer(marker("window_days")), POST_DEFINITIVE_SWEEP_WINDOW_DAYS),
       "closing window: code %d, methods paper %s", POST_DEFINITIVE_SWEEP_WINDOW_DAYS,
       marker("window_days"))
report(identical(marker("baseline"), POST_DEFINITIVE_SWEEP_BASELINE_ARM),
       "baseline arm: code '%s', methods paper '%s'", POST_DEFINITIVE_SWEEP_BASELINE_ARM,
       marker("baseline"))
report(identical(strsplit(marker("shares"), ";", fixed = TRUE)[[1]],
                 format(POST_DEFINITIVE_SWEEP_SHARES, nsmall = 2)),
       "shares: code '%s', methods paper '%s'",
       paste(format(POST_DEFINITIVE_SWEEP_SHARES, nsmall = 2), collapse = ";"),
       marker("shares"))
report(anyDuplicated(POST_DEFINITIVE_SWEEP_ARMS$arm) == 0L &&
         POST_DEFINITIVE_SWEEP_BASELINE_ARM %in% POST_DEFINITIVE_SWEEP_ARMS$arm,
       "every arm's label is distinct and the baseline arm is swept")

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
described <- build_environment(resolve_scenario(json_data, "default"))
shipped_share <- described$vars$r2eheavy$post_definitive_icu$share
report(identical(as.numeric(shipped_share), POST_DEFINITIVE_SWEEP_BASELINE),
       "the baseline arm's share is the shipped share exactly (%s)", format(shipped_share))

# ── 2. The tracked evidence sets are that experiment ────────────────────────

cat("\n-- the tracked evidence sets carry the documented design --\n")

#' Read one tracked file of a sweep
#'
#' @param stem File stem without the scenario suffix or extension.
#' @param suffix Scenario suffix.
#' @param dir Evidence directory (default the share sweep's).
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
  recomputed <- do.call(rbind, lapply(POST_DEFINITIVE_SWEEP_ARMS$arm, function(a) {
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
  base <- POST_DEFINITIVE_SWEEP_BASELINE_ARM
  others <- setdiff(POST_DEFINITIVE_SWEEP_ARMS$arm, base)
  ok <- nrow(paired) == length(others) * length(POST_DEFINITIVE_SWEEP_PAIRED_RESPONSES)
  for (a in others) for (response in POST_DEFINITIVE_SWEEP_PAIRED_RESPONSES) {
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
  percent <- as.integer(round(100 * POST_DEFINITIVE_SWEEP_ARMS$share))
  stems <- sprintf("post_definitive_sweep_arm_share_%03d%s_series.csv.gz", percent, suffix)
  files <- file.path(EVIDENCE_DIR, stems)
  if (!all(file.exists(files))) return(NULL)
  do.call(rbind, lapply(seq_along(files), function(i) {
    d <- read.csv(files[i], stringsAsFactors = FALSE)
    d$arm <- POST_DEFINITIVE_SWEEP_ARMS$arm[i]
    d
  }))
}

for (label in names(SUFFIXES)) {
  suffix <- SUFFIXES[[label]]
  rows <- read_sweep("post_definitive_sweep_replications", suffix)
  summ <- read_sweep("post_definitive_sweep_summary", suffix)
  paired <- read_sweep("post_definitive_sweep_paired", suffix)
  stab <- read_sweep("post_definitive_sweep_stability", suffix)
  present <- !is.null(rows) && !is.null(summ) && !is.null(paired) && !is.null(stab)
  report(present, "%s: the replications, summary, paired and stability files are tracked",
         label)
  if (!present) next

  report(setequal(unique(rows$arm), POST_DEFINITIVE_SWEEP_ARMS$arm),
         "%s: the arms are the documented four shares (%s)", label,
         paste(sort(unique(rows$arm)), collapse = "; "))
  per_arm <- table(rows$arm)
  report(all(per_arm == POST_DEFINITIVE_SWEEP_REPLICATIONS),
         "%s: every arm carries %d replications (%s)", label,
         POST_DEFINITIVE_SWEEP_REPLICATIONS, paste(per_arm, collapse = ", "))
  numbered <- all(vapply(split(rows$replication, rows$arm), function(r) {
    identical(sort(as.integer(r)), seq_len(POST_DEFINITIVE_SWEEP_REPLICATIONS))
  }, logical(1)))
  report(numbered, "%s: replications are numbered from one in every arm", label)
  needed <- c(POST_DEFINITIVE_SWEEP_PAIRED_RESPONSES, "ward_overflow", "post_definitive_icu_n",
              "hold_total_bed_days", "evacuation_bed_days", "recovery_bed_days",
              "post_op_hold_bed_days")
  report(all(needed %in% names(rows)), "%s: the response set is present (missing: %s)", label,
         paste(setdiff(needed, names(rows)), collapse = ", "))
  report(summary_agrees(rows, summ),
         "%s: the summary is the reduction of the per-replication responses", label)
  report(paired_agrees(rows, paired),
         "%s: every paired difference is taken within replication against '%s'", label,
         POST_DEFINITIVE_SWEEP_BASELINE_ARM)

  series <- read_series(suffix)
  report(!is.null(series), "%s: every arm's daily series is tracked", label)
  if (!is.null(series)) {
    st <- post_definitive_stability(series)
    st <- st[match(paste(stab$arm, stab$pool), paste(st$arm, st$pool)), ]
    report(identical(as.character(st$stability), as.character(stab$stability)) &&
             numerics_agree(stab, st[, names(stab)]) &&
             setequal(unique(stab$pool), ICU_BED_SWEEP_POOLS),
           "%s: the stability file is the classification of every pool's tracked series", label)
  }

  # ── 3. Ward recoveries rise as the share falls ───────────────────────────
  base_rows <- rows[rows$arm == POST_DEFINITIVE_SWEEP_BASELINE_ARM, ]
  report(all(base_rows$ward_recoveries == 0) && all(base_rows$ward_bed_days == 0),
         "%s: at the shipped share no casualty recovers on the ward", label)
  by_share <- vapply(POST_DEFINITIVE_SWEEP_ARMS$arm, function(a) {
    mean(rows$ward_recoveries[rows$arm == a])
  }, numeric(1))
  ordered <- by_share[order(POST_DEFINITIVE_SWEEP_ARMS$share)]
  report(all(diff(ordered) < 0),
         "%s: mean ward recoveries fall as the share rises (%s)", label,
         paste(sprintf("%.1f", ordered), collapse = ", "))

  # ── 4. The shipped arm reproduces the shipped configuration ──────────────
  icu <- read_sweep("icu_bed_sweep_replications", suffix, ICU_EVIDENCE_DIR)
  if (is.null(icu)) {
    report(FALSE, "%s: the intensive care bed sweep to compare against is tracked", label)
    next
  }
  mine <- base_rows[order(base_rows$replication), SHARED_RESPONSES]
  theirs <- icu[icu$arm == ICU_BED_SWEEP_BASELINE_ARM, ]
  theirs <- theirs[order(theirs$replication), SHARED_RESPONSES]
  report(nrow(mine) == nrow(theirs) && numerics_agree(mine, theirs),
         paste("%s: the shipped arm equals the intensive care bed sweep's shipped arm,",
               "replication by replication, on %d responses"), label, length(SHARED_RESPONSES))
}

cat("\n-- short replications through the setter --\n")

#' Measure two short replications under a described configuration
#'
#' @param config Described configuration to run.
#' @return The per-replication responses at the control seed.
live_rows <- function(config) {
  assign("env_data", config, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(config$elms, length), envir = globalenv())
  set.seed(POST_DEFINITIVE_SWEEP_SEED)
  invisible(capture.output(suppressWarnings(suppressMessages(
    out <- run_post_definitive_measurement(n_iterations = 2L, n_days = LIVE_DAYS,
                                           window_days = LIVE_DAYS, max_cores = 1L)
  ))))
  out$rows
}
plain <- live_rows(described)
through_setter <- live_rows(set_post_definitive_share(described, 1))
report(identical(plain, through_setter) && all(plain$ward_recoveries == 0),
       "%d-day replications with the share set to 1 through the setter equal the shipped ones",
       LIVE_DAYS)
lowered <- live_rows(set_post_definitive_share(described, 0.25))
report(sum(lowered$ward_recoveries) > 0,
       "at a share of 0.25 the same replications send %d casualties to the ward",
       sum(lowered$ward_recoveries))

# ── 5. The results paper prints the tables ──────────────────────────────────

cat("\n-- the results paper prints the sweep's tables --\n")

paper <- paste(readLines(PAPER_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (name in c("post_definitive", "post_definitive_high", "post_definitive_paired",
               "post_definitive_paired_high")) {
  report(grepl(sprintf("<!-- GEN %s -->", name), paper, fixed = TRUE),
         "Results.md prints the generated table '%s'", name)
}

# ── 6. The setter ───────────────────────────────────────────────────────────

cat("\n-- the share setter --\n")

set <- set_post_definitive_share(described, 0.5)
report(identical(set$vars$r2eheavy$post_definitive_icu$share, 0.5),
       "the setter sets the share")
#' A described configuration with the post-definitive share removed
#'
#' @param x Described configuration.
#' @return The configuration without `vars.r2eheavy.post_definitive_icu.share`.
strip <- function(x) {
  x$vars$r2eheavy$post_definitive_icu$share <- NULL
  x
}
report(identical(strip(set), strip(described)),
       "and leaves every other variable and every element as it was")
for (bad in list(-0.1, 1.2, NA_real_, c(0.2, 0.3), "0.5", Inf)) {
  report(inherits(try(set_post_definitive_share(described, bad), silent = TRUE), "try-error"),
         "a share of '%s' is rejected", paste(format(bad), collapse = ","))
}
report(inherits(try(set_post_definitive_share(strip(described), 0.5), silent = TRUE),
                "try-error"),
       "a configuration without the share is rejected, not left unchanged")

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nThe post-definitive share sweep's protocol, evidence and published section agree.\n")
quit(status = 0L)
