#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_triage_sweep_protocol.R                               ##
## Regression check — the triage share sweep's parameters, its responses   ##
## and its published section agree                                          ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_triage_sweep_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The background triage split is the model's only proxy
# for severity, and the sweep that varies it is the evidence for whether the
# capacity findings depend on that informed estimate. Its arms only measure
# acuity if each changes the Priority 1 share alone, keeping the Priority 2 to
# Priority 3 ratio, and if the shipped arm is the shipped configuration rather
# than a value near it; and its tracked set is only the documented experiment
# if the protocol, the files and the published tables agree.
#
# What this asserts:
#
#   1. Every protocol parameter in R/triage_sweep.R equals the value
#      docs/Methods.md documents in a marker comment, every arm's split sums to
#      one and keeps the shipped 20:15 ratio, and the baseline arm's split is
#      the shipped `vars.r1.priority` exactly.
#   2. Each tracked evidence set, the shipped configuration's and the
#      high-intensity profile's, carries every documented arm at the documented
#      replication count numbered from one and the documented responses, with
#      the summary, the paired differences, the pooled treated cohort and the
#      queue classification each the reduction of the evidence beside them.
#   3. The shipped arm reproduces the shipped configuration: replication by
#      replication it equals the R2E intensive care bed sweep's shipped arm, run
#      on the same protocol and control seed.
#   4. Results.md prints the sweep's tables for both intensities as generated
#      spans.
#   5. The split setter changes only the background split and refuses a split
#      it cannot apply, and the share function refuses a share outside (0, 1).

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
})

source("R/environment.R")
source("R/scenario.R")
source("R/long_horizon.R")
source("R/policy_sweep.R")
source("R/hold_window.R")
source("R/surgical_team_sweep.R")
source("R/icu_bed_sweep.R")
source("R/triage_sweep.R")

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
EVIDENCE_DIR <- file.path("data", "triage_split")

#' Directory of the intensive care bed sweep, whose shipped arm the shipped
#' triage arm must reproduce
ICU_EVIDENCE_DIR <- file.path("data", "icu_beds")

#' Scenario suffixes of the two tracked evidence sets
SUFFIXES <- c(moderate = "", high = "_high_intensity")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Responses the shipped arm must share with the intensive care bed sweep's
SHARED_RESPONSES <- c("total_dow", "total_rtd", "hold_mean_queue", "icu_occupancy",
                      "overflow_r2e_post_op", "overflow_r2e_staging", "evac_decisions",
                      "role4_peak")

# ── 1. The code's parameters are the ones the methods paper documents ───────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one triage sweep protocol parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "TRIAGE ".
#' @return The marker's value as a character string, or NA where absent.
triage_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- TRIAGE %s=[^>]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- TRIAGE [^=]+=(.*) -->$", "\\1", m)
}

report(identical(as.integer(triage_marker("replications")), TRIAGE_SWEEP_REPLICATIONS),
       "replications: code %d, methods paper %s", TRIAGE_SWEEP_REPLICATIONS,
       triage_marker("replications"))
report(identical(as.integer(triage_marker("days")), TRIAGE_SWEEP_DAYS),
       "days: code %d, methods paper %s", TRIAGE_SWEEP_DAYS, triage_marker("days"))
report(identical(as.integer(triage_marker("seed")), TRIAGE_SWEEP_SEED),
       "seed: code %d, methods paper %s", TRIAGE_SWEEP_SEED, triage_marker("seed"))
report(identical(as.integer(triage_marker("window_days")), TRIAGE_SWEEP_WINDOW_DAYS),
       "closing window: code %d, methods paper %s", TRIAGE_SWEEP_WINDOW_DAYS,
       triage_marker("window_days"))
report(identical(triage_marker("baseline"), TRIAGE_SWEEP_BASELINE_ARM),
       "baseline arm: code '%s', methods paper '%s'", TRIAGE_SWEEP_BASELINE_ARM,
       triage_marker("baseline"))
report(identical(strsplit(triage_marker("p1_shares"), ";", fixed = TRUE)[[1]],
                 format(TRIAGE_SWEEP_P1_SHARES, nsmall = 2)),
       "Priority 1 shares: code '%s', methods paper '%s'",
       paste(format(TRIAGE_SWEEP_P1_SHARES, nsmall = 2), collapse = ";"),
       triage_marker("p1_shares"))

splits <- TRIAGE_SWEEP_ARMS[, c("p1", "p2", "p3")]
report(all(abs(rowSums(splits) - 1) < 1e-9), "every arm's split sums to one")
report(all(abs(splits$p2 / splits$p3 - 20 / 15) < 1e-9),
       "every arm keeps the shipped 20:15 ratio of Priority 2 to Priority 3")
report(all(TRIAGE_SWEEP_ARMS$arm == sprintf("P1 %d%%", round(100 * TRIAGE_SWEEP_ARMS$p1))) &&
         anyDuplicated(TRIAGE_SWEEP_ARMS$arm) == 0L,
       "every arm's label states its Priority 1 share, and no arm is listed twice")

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
described <- build_environment(resolve_scenario(json_data, "default"))
shipped <- unlist(described$vars$r1$priority[c("one", "two", "three")])
base_split <- triage_split(TRIAGE_SWEEP_BASELINE)
report(identical(unname(base_split), unname(shipped)),
       "the baseline arm's split is the shipped split exactly (%s)",
       paste(format(shipped), collapse = " / "))

# ── 2. The tracked evidence sets are that experiment ────────────────────────

cat("\n-- the tracked evidence sets carry the documented design --\n")

#' Read one tracked file of a sweep
#'
#' @param stem File stem without the scenario suffix or extension.
#' @param suffix Scenario suffix.
#' @param dir Evidence directory (default the triage set's).
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

for (label in names(SUFFIXES)) {
  suffix <- SUFFIXES[[label]]
  rows <- read_sweep("triage_sweep_replications", suffix)
  summ <- read_sweep("triage_sweep_summary", suffix)
  paired <- read_sweep("triage_sweep_paired", suffix)
  stab <- read_sweep("triage_sweep_stability", suffix)
  treated <- read_sweep("triage_sweep_treated_cohort", suffix)
  present <- !is.null(rows) && !is.null(summ) && !is.null(paired) && !is.null(stab) &&
    !is.null(treated)
  report(present,
         "%s: the replications, summary, paired, stability and treated cohort files are tracked",
         label)
  if (!present) next

  report(setequal(unique(rows$arm), TRIAGE_SWEEP_ARMS$arm),
         "%s: the arms are the documented four shares (%s)", label,
         paste(sort(unique(rows$arm)), collapse = "; "))
  per_arm <- table(rows$arm)
  report(all(per_arm == TRIAGE_SWEEP_REPLICATIONS),
         "%s: every arm carries %d replications (%s)", label, TRIAGE_SWEEP_REPLICATIONS,
         paste(per_arm, collapse = ", "))
  numbered <- all(vapply(split(rows$replication, rows$arm), function(r) {
    identical(sort(as.integer(r)), seq_len(TRIAGE_SWEEP_REPLICATIONS))
  }, logical(1)))
  report(numbered, "%s: replications are numbered from one in every arm", label)
  needed <- c(TRIAGE_SWEEP_PAIRED_RESPONSES, "treated_n", "treated_dow", "expected_early",
              "expected_late", "wounded")
  report(all(needed %in% names(rows)), "%s: the response set is present (missing: %s)", label,
         paste(setdiff(needed, names(rows)), collapse = ", "))

  recomputed <- do.call(rbind, lapply(TRIAGE_SWEEP_ARMS$arm, function(a) {
    cbind(data.frame(arm = a),
          summarise_policy(rows[rows$arm == a, setdiff(names(rows), "arm")],
                           arm_column = "arm_placeholder"))
  }))
  key <- paste(summ$arm, summ$response)
  report(identical(sort(key), sort(paste(recomputed$arm, recomputed$response))) &&
           numerics_agree(summ[order(key), ],
                          recomputed[order(paste(recomputed$arm, recomputed$response)), ]),
         "%s: the summary is the reduction of the per-replication responses", label)

  others <- setdiff(TRIAGE_SWEEP_ARMS$arm, TRIAGE_SWEEP_BASELINE_ARM)
  ok <- TRUE
  for (a in others) for (response in TRIAGE_SWEEP_PAIRED_RESPONSES) {
    mine <- policy_paired_difference(rows, response, TRIAGE_SWEEP_BASELINE_ARM, a,
                                     arm_column = "arm")
    theirs <- paired[paired$to == a & paired$response == response, ]
    ok <- ok && nrow(theirs) == 1L &&
      isTRUE(all.equal(mine$difference, theirs$difference, tolerance = TOL)) &&
      isTRUE(all.equal(mine$ci_lower, theirs$ci_lower, tolerance = TOL))
  }
  report(ok, "%s: every paired difference is taken within replication against '%s'", label,
         TRIAGE_SWEEP_BASELINE_ARM)
  report(nrow(paired) == length(others) * length(TRIAGE_SWEEP_PAIRED_RESPONSES),
         "%s: the paired file has one row per arm and response (%d)", label, nrow(paired))

  again <- triage_treated_pooled(rows)
  again <- again[match(treated$arm, again$arm), ]
  report(numerics_agree(treated, again[, names(treated)]),
         "%s: the treated cohort file is the pooled reduction of the replications", label)

  series_files <- file.path(
    EVIDENCE_DIR,
    sprintf("triage_sweep_arm_p1_%02d%s_series.csv.gz",
            as.integer(round(100 * TRIAGE_SWEEP_ARMS$p1)), suffix)
  )
  have <- file.exists(series_files)
  report(all(have), "%s: every arm's daily series is tracked (%d of %d)", label, sum(have),
         length(have))
  if (all(have)) {
    series <- do.call(rbind, lapply(seq_along(series_files), function(i) {
      d <- read.csv(series_files[i], stringsAsFactors = FALSE)
      d$arm <- TRIAGE_SWEEP_ARMS$arm[i]
      d
    }))
    st <- triage_stability(series)
    st <- st[match(paste(stab$arm, stab$pool), paste(st$arm, st$pool)), ]
    report(identical(as.character(st$stability), as.character(stab$stability)) &&
             numerics_agree(stab, st[, names(stab)]) &&
             setequal(unique(stab$pool), TRIAGE_SWEEP_POOLS),
           "%s: the stability file is the classification of every pool's tracked series", label)
  }

  # ── 3. The shipped arm reproduces the shipped configuration ──────────────
  icu <- read_sweep("icu_bed_sweep_replications", suffix, ICU_EVIDENCE_DIR)
  if (is.null(icu)) {
    report(FALSE, "%s: the intensive care bed sweep to compare against is tracked", label)
    next
  }
  mine <- rows[rows$arm == TRIAGE_SWEEP_BASELINE_ARM, ]
  theirs <- icu[icu$arm == ICU_BED_SWEEP_BASELINE_ARM, ]
  mine <- mine[order(mine$replication), SHARED_RESPONSES]
  theirs <- theirs[order(theirs$replication), SHARED_RESPONSES]
  report(nrow(mine) == nrow(theirs) && numerics_agree(mine, theirs),
         paste("%s: the shipped arm equals the intensive care bed sweep's shipped arm,",
               "replication by replication, on %d responses"), label, length(SHARED_RESPONSES))
}

# ── 4. The results paper prints the tables ──────────────────────────────────

cat("\n-- the results paper prints the sweep's tables --\n")

paper <- paste(readLines(PAPER_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (name in c("triage_split", "triage_split_high", "triage_split_paired",
               "triage_split_paired_high")) {
  report(grepl(sprintf("<!-- GEN %s -->", name), paper, fixed = TRUE),
         "Results.md prints the generated table '%s'", name)
}

# ── 5. The setter and the share function ────────────────────────────────────

cat("\n-- the split setter and the share function --\n")

set <- set_priority_split(described, triage_split(0.15))
report(isTRUE(all.equal(unlist(set$vars$r1$priority[c("one", "two", "three")]),
                        triage_split(0.15), check.attributes = FALSE)),
       "the setter sets the background split")
others_unchanged <- identical(set$vars[names(set$vars) != "r1"],
                              described$vars[names(described$vars) != "r1"]) &&
  identical(set$vars$r1[names(set$vars$r1) != "priority"],
            described$vars$r1[names(described$vars$r1) != "priority"]) &&
  identical(set$elms, described$elms)
report(others_unchanged, "and leaves every other variable and every element as it was")
for (bad in list(c(one = 0.5, two = 0.3, three = 0.3), c(one = 0.5, two = 0.5),
                 c(one = -0.1, two = 0.6, three = 0.5), c(one = NA, two = 0.5, three = 0.5))) {
  report(inherits(try(set_priority_split(described, bad), silent = TRUE), "try-error"),
         "a split of %s is rejected", paste(names(bad), format(bad), sep = "=", collapse = ", "))
}
stripped <- described
stripped$vars$r1$priority <- NULL
report(inherits(try(set_priority_split(stripped, triage_split(0.3)), silent = TRUE),
                "try-error"),
       "a configuration without the background split is rejected, not left unchanged")
for (bad in list(0, 1, -0.2, NA, c(0.2, 0.3))) {
  report(inherits(try(triage_split(bad), silent = TRUE), "try-error"),
         "a Priority 1 share of '%s' is rejected", paste(format(bad), collapse = ","))
}

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nThe triage share sweep's protocol, evidence and published section agree.\n")
quit(status = 0L)
