#!/usr/bin/env Rscript
##############################################################################
## scripts/check_surgical_sweep_protocol.R                                  ##
## Regression check — the surgical team sweep's parameters, its responses   ##
## and its published section agree                                          ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_surgical_sweep_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The planning paper's first recommendation, extending
# surgical cover, was argued from the model's structure until the surgical team
# sweep measured it. A recommendation that now rests on an experiment is only
# as good as that experiment's agreement with the document that reports it, and
# the experiments before this one drifted from their documentation more than
# once. This check keeps the sweep's protocol, its tracked evidence set and its
# published section in step.
#
# What this asserts:
#
#   1. Every protocol parameter in R/surgical_team_sweep.R equals the value
#      docs/Methods.md documents in a marker comment, the baseline arm is the
#      shipped establishment in env_data.json, and each arm's section counts
#      differ from the baseline in what its label says.
#   2. Each tracked evidence set, the shipped configuration's and the
#      high-intensity profile's, carries every documented arm at the documented
#      replication count numbered from one and the documented responses, with
#      the summary, the paired differences and the queue classification each the
#      reduction of the per-replication evidence beside them.
#   3. Results.md prints the sweep's summary and paired tables for both
#      intensities as generated spans.
#   4. The classification and its shortfall per day are correct on a series
#      whose answer is computable by hand, so a table agreeing with the tracked
#      classification is not two copies of one error.
#
# Assertion 4 is what keeps assertion 2 from being circular: the stability file
# would match any classification the code happened to produce.

source("R/constants.R")
source("R/long_horizon.R")
source("R/policy_sweep.R")
source("R/hold_window.R")
source("R/surgical_team_sweep.R")

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

#' The methods paper, which documents the experiment's design
METHODS_PATH <- file.path("docs", "Methods.md")

#' The results paper, which prints the experiment's tables
PAPER_PATH <- file.path("docs", "Results.md")

#' Directory of the tracked evidence set
EVIDENCE_DIR <- file.path("data", "surgical_teams")

#' Scenario suffixes of the two tracked evidence sets
SUFFIXES <- c(moderate = "", high = "_high_intensity")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

# ── 1. The code's parameters are the ones the methods paper documents ───────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one surgical team protocol parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "SURGICAL_TEAMS ".
#' @return The marker's value as a character string, or NA where absent.
#'
#' @details Marked rather than parsed out of the prose, on the convention
#'   `scripts/check_airlift_protocol.R` established.
surgical_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- SURGICAL_TEAMS %s=[^>]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- SURGICAL_TEAMS [^=]+=(.*) -->$", "\\1", m)
}

report(identical(as.integer(surgical_marker("replications")), SURGICAL_SWEEP_REPLICATIONS),
       "replications: code %d, methods paper %s", SURGICAL_SWEEP_REPLICATIONS,
       surgical_marker("replications"))
report(identical(as.integer(surgical_marker("days")), SURGICAL_SWEEP_DAYS),
       "days: code %d, methods paper %s", SURGICAL_SWEEP_DAYS, surgical_marker("days"))
report(identical(as.integer(surgical_marker("seed")), SURGICAL_SWEEP_SEED),
       "seed: code %d, methods paper %s", SURGICAL_SWEEP_SEED, surgical_marker("seed"))
report(identical(as.integer(surgical_marker("window_days")), SURGICAL_SWEEP_WINDOW_DAYS),
       "closing window: code %d, methods paper %s", SURGICAL_SWEEP_WINDOW_DAYS,
       surgical_marker("window_days"))
report(identical(surgical_marker("baseline"), SURGICAL_SWEEP_BASELINE),
       "baseline arm: code '%s', methods paper '%s'", SURGICAL_SWEEP_BASELINE,
       surgical_marker("baseline"))
report(identical(strsplit(surgical_marker("arms"), ";", fixed = TRUE)[[1]],
                 SURGICAL_SWEEP_ARMS$arm),
       "arms: code '%s', methods paper '%s'", paste(SURGICAL_SWEEP_ARMS$arm, collapse = ";"),
       surgical_marker("arms"))

shipped <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
#' Surgical sections an echelon fields per unit in the shipped configuration
#'
#' @param echelon Element name, `"r2eheavy"` or `"r2b"`.
#' @return The `qty` of the element's surgical sub-element.
shipped_sections <- function(echelon) {
  for (e in shipped$elms) {
    if (!identical(e$elm, echelon)) next
    for (s in e$sub_elms) if (identical(s$sub_elm, "surg")) return(s$qty)
  }
  NA_integer_
}
base_row <- SURGICAL_SWEEP_ARMS[SURGICAL_SWEEP_ARMS$arm == SURGICAL_SWEEP_BASELINE, ]
report(nrow(base_row) == 1L && base_row$r2e_sections == shipped_sections("r2eheavy") &&
         base_row$r2b_sections == shipped_sections("r2b"),
       "the baseline arm is the shipped establishment (R2E %s, R2B %s in env_data.json)",
       shipped_sections("r2eheavy"), shipped_sections("r2b"))
labels_ok <- all(SURGICAL_SWEEP_ARMS$arm ==
                   sprintf("R2E %d, R2B %d", SURGICAL_SWEEP_ARMS$r2e_sections,
                           SURGICAL_SWEEP_ARMS$r2b_sections))
report(labels_ok, "every arm's label states the section counts it runs")
report(anyDuplicated(SURGICAL_SWEEP_ARMS$arm) == 0L, "no arm is listed twice")

# ── 2. The tracked evidence sets are that experiment ────────────────────────

cat("\n-- the tracked evidence sets carry the documented design --\n")

#' Read one tracked file of the sweep
#'
#' @param stem File stem without the scenario suffix or extension.
#' @param suffix Scenario suffix.
#' @return The data frame, or NULL where the file is absent.
read_sweep <- function(stem, suffix) {
  path <- file.path(EVIDENCE_DIR, paste0(stem, suffix, ".csv"))
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
  rows <- read_sweep("surgical_team_sweep_replications", suffix)
  summ <- read_sweep("surgical_team_sweep_summary", suffix)
  paired <- read_sweep("surgical_team_sweep_paired", suffix)
  stab <- read_sweep("surgical_team_sweep_stability", suffix)
  report(!is.null(rows) && !is.null(summ) && !is.null(paired) && !is.null(stab),
         "%s: the replications, summary, paired and stability files are tracked", label)
  if (is.null(rows) || is.null(summ) || is.null(paired) || is.null(stab)) next

  report(setequal(unique(rows$arm), SURGICAL_SWEEP_ARMS$arm),
         "%s: the arms are the documented five (%s)", label,
         paste(sort(unique(rows$arm)), collapse = "; "))
  per_arm <- table(rows$arm)
  report(all(per_arm == SURGICAL_SWEEP_REPLICATIONS),
         "%s: every arm carries %d replications (%s)", label, SURGICAL_SWEEP_REPLICATIONS,
         paste(per_arm, collapse = ", "))
  numbered <- all(vapply(split(rows$replication, rows$arm), function(r) {
    identical(sort(as.integer(r)), seq_len(SURGICAL_SWEEP_REPLICATIONS))
  }, logical(1)))
  report(numbered, "%s: replications are numbered from one in every arm", label)
  needed <- c(SURGICAL_SWEEP_PAIRED_RESPONSES, "total_casualties")
  report(all(needed %in% names(rows)), "%s: the response set is present (missing: %s)", label,
         paste(setdiff(needed, names(rows)), collapse = ", "))

  recomputed <- do.call(rbind, lapply(SURGICAL_SWEEP_ARMS$arm, function(a) {
    cbind(data.frame(arm = a),
          summarise_policy(rows[rows$arm == a, setdiff(names(rows), "arm")],
                           arm_column = "arm_placeholder"))
  }))
  key <- paste(summ$arm, summ$response)
  report(identical(sort(key), sort(paste(recomputed$arm, recomputed$response))) &&
           numerics_agree(summ[order(key), ],
                          recomputed[order(paste(recomputed$arm, recomputed$response)), ]),
         "%s: the summary is the reduction of the per-replication responses", label)

  others <- setdiff(SURGICAL_SWEEP_ARMS$arm, SURGICAL_SWEEP_BASELINE)
  ok <- TRUE
  for (a in others) for (response in SURGICAL_SWEEP_PAIRED_RESPONSES) {
    mine <- policy_paired_difference(rows, response, SURGICAL_SWEEP_BASELINE, a,
                                     arm_column = "arm")
    theirs <- paired[paired$to == a & paired$response == response, ]
    ok <- ok && nrow(theirs) == 1L &&
      isTRUE(all.equal(mine$difference, theirs$difference, tolerance = TOL)) &&
      isTRUE(all.equal(mine$ci_lower, theirs$ci_lower, tolerance = TOL))
  }
  report(ok, "%s: every paired difference is taken within replication against '%s'", label,
         SURGICAL_SWEEP_BASELINE)
  report(nrow(paired) == length(others) * length(SURGICAL_SWEEP_PAIRED_RESPONSES),
         "%s: the paired file has one row per arm and response (%d)", label, nrow(paired))

  series_files <- file.path(
    EVIDENCE_DIR,
    sprintf("surgical_team_sweep_arm_r2e%d_r2b%d%s_series.csv.gz",
            SURGICAL_SWEEP_ARMS$r2e_sections, SURGICAL_SWEEP_ARMS$r2b_sections, suffix)
  )
  have <- file.exists(series_files)
  report(all(have), "%s: every arm's daily series is tracked (%d of %d)", label, sum(have),
         length(have))
  if (all(have)) {
    series <- do.call(rbind, lapply(seq_along(series_files), function(i) {
      d <- read.csv(series_files[i], stringsAsFactors = FALSE)
      d$arm <- SURGICAL_SWEEP_ARMS$arm[i]
      d
    }))
    again <- surgical_stability(series)
    again <- again[match(stab$arm, again$arm), ]
    report(identical(as.character(again$stability), as.character(stab$stability)) &&
             numerics_agree(stab, again[, names(stab)]),
           "%s: the stability file is the classification of the tracked daily series", label)
  }
}

# ── 3. The results paper prints the tables ──────────────────────────────────

cat("\n-- the results paper prints the sweep's tables --\n")

paper <- paste(readLines(PAPER_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (name in c("surgical_team", "surgical_team_high", "surgical_team_paired",
               "surgical_team_paired_high")) {
  report(grepl(sprintf("<!-- GEN %s -->", name), paper, fixed = TRUE),
         "Results.md prints the generated table '%s'", name)
}

# ── 4. The classification is correct on inputs computable by hand ───────────

cat("\n-- the classification on hand-computed input --\n")

#' A constructed daily series for one arm
#'
#' @param arm Arm label.
#' @param f Function of the day giving the queue.
#' @param n_reps Replications, each offset by its index so they differ.
#' @param n_days Campaign length in days.
#' @return The series in the long form surgical_stability() takes.
toy_series <- function(arm, f, n_reps = 3L, n_days = 360L) {
  do.call(rbind, lapply(seq_len(n_reps), function(r) {
    data.frame(day = seq_len(n_days), series = "mean_queue", subject = SURGICAL_SWEEP_POOL,
               value = f(seq_len(n_days)) + r, replication = r, arm = arm)
  }))
}

# A queue growing three casualties a day has block means rising 90 a block, so
# its shortfall is exactly three a day; a flat queue has none.
toy <- rbind(toy_series("growing", function(d) 3 * d),
             toy_series("flat", function(d) rep(5, length(d))))
got <- suppressWarnings(surgical_stability(toy))
grow <- got[got$arm == "growing", ]
flat <- got[got$arm == "flat", ]
report(isTRUE(all.equal(grow$shortfall_per_day, 3, tolerance = 1e-6)),
       "a queue growing three a day has a shortfall of three a day (%.6f)",
       grow$shortfall_per_day)
report(identical(as.character(grow$stability), "drifting"),
       "and is classified drifting (%s)", grow$stability)
report(isTRUE(all.equal(flat$shortfall_per_day, 0, tolerance = 1e-6)) &&
         identical(as.character(flat$stability), "converged"),
       "a flat queue has no shortfall and is classified converged (%.6f, %s)",
       flat$shortfall_per_day, flat$stability)
report(inherits(try(surgical_stability(toy_series("short", function(d) d, n_days = 90L)),
                    silent = TRUE), "try-error"),
       "a campaign too short for the late-half fit is refused rather than classified")

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nThe surgical team sweep's protocol, evidence and published section agree.\n")
quit(status = 0L)
