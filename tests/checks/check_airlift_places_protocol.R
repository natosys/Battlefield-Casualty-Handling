#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_airlift_places_protocol.R                             ##
## Regression check — the critical-care places sweep's parameters, its      ##
## responses and its published section agree                                ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_airlift_places_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The critical-care places are the binding strategic
# airlift constraint, and every airlift sweep held each sortie's split at the
# shipped 36 critical and 54 standard places; this sweep moves places between
# the two pools, crossed with the sortie interval. A finding that rests on an
# experiment is only as good as that experiment's agreement with the document
# that reports it, so this check keeps the sweep's protocol, its tracked
# evidence set and its published section in step, and defends the three facts
# the design depends on: that the split set on the configuration is the split
# the sorties fly, that every arm flies the airframe's full complement of
# places, and that the collapse rule judges each arm against its own places.
#
# What this asserts:
#
#   1. Every protocol parameter in R/airlift_places_sweep.R equals the value
#      docs/Methods.md documents in a marker comment, the baseline arm is the
#      shipped split and interval in env_data.json, and every arm's places sum
#      to the shipped airframe's total.
#   2. Each tracked evidence set, the shipped configuration's and the
#      high-intensity profile's, carries every documented arm at the documented
#      replication count numbered from one and the documented responses, with
#      the summary, the paired differences and the collapse classification each
#      the reduction of the per-replication evidence and daily series beside
#      them.
#   3. The shipped split, at the weekly and at the three-day interval and at
#      both intensities, reproduces in every response the two sweeps share the
#      tracked replications of the strategic airlift sweep's identical
#      configurations under the same seed.
#   4. Results.md prints the sweep's summary, paired and collapse tables for
#      both intensities as generated spans.
#   5. The collapse rule judges a route against the places its own sorties
#      open, on a series computable by hand.
#   6. The places setter changes only the configured airframe's split, refuses
#      a split it cannot apply, and the split it sets is the one the airframe
#      resolver reads.
#
# Assertions 3 and 5 are what keep assertion 2 from being circular: the summary
# and collapse files would match any responses the code happened to produce.

suppressPackageStartupMessages({
  library(simmer)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/policy_sweep.R")
source("R/long_horizon.R")
source("R/airlift.R")
source("R/airlift_places_sweep.R")

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
EVIDENCE_DIR <- file.path("data", "airlift_places")

#' Directory of the strategic airlift sweep's tracked evidence set
AIRLIFT_DIR <- file.path("data", "airlift")

#' Scenario suffixes of the two tracked evidence sets
SUFFIXES <- c(moderate = "", high = "_high_intensity")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

# ── 1. The code's parameters are the ones the methods paper documents ───────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one places sweep protocol parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "AIRLIFT_PLACES ".
#' @return The marker's value as a character string, or NA where absent.
places_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- AIRLIFT_PLACES %s=[^>]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- AIRLIFT_PLACES [^=]+=(.*) -->$", "\\1", m)
}

report(identical(as.integer(places_marker("replications")), AIRLIFT_PLACES_REPLICATIONS),
       "replications: code %d, methods paper %s", AIRLIFT_PLACES_REPLICATIONS,
       places_marker("replications"))
report(identical(as.integer(places_marker("days")), AIRLIFT_PLACES_DAYS),
       "days: code %d, methods paper %s", AIRLIFT_PLACES_DAYS, places_marker("days"))
report(identical(as.integer(places_marker("seed")), AIRLIFT_PLACES_SEED),
       "seed: code %d, methods paper %s", AIRLIFT_PLACES_SEED, places_marker("seed"))
report(identical(as.integer(places_marker("window_days")), AIRLIFT_PLACES_WINDOW_DAYS),
       "closing window: code %d, methods paper %s", AIRLIFT_PLACES_WINDOW_DAYS,
       places_marker("window_days"))
report(identical(places_marker("baseline"), AIRLIFT_PLACES_BASELINE_ARM),
       "baseline arm: code '%s', methods paper '%s'", AIRLIFT_PLACES_BASELINE_ARM,
       places_marker("baseline"))
arms_code <- paste(sprintf("%d/%d/%d", AIRLIFT_PLACES_ARMS$critical, AIRLIFT_PLACES_ARMS$standard,
                           AIRLIFT_PLACES_ARMS$interval), collapse = ";")
report(identical(places_marker("arms"), arms_code),
       "arms (critical/standard/interval): code '%s', methods paper '%s'", arms_code,
       places_marker("arms"))

json_data <- read_config("env_data.json")
shipped <- build_environment(resolve_scenario(json_data, "default"))
frame <- resolve_ame_airframe(shipped$vars$role4)
base_row <- AIRLIFT_PLACES_ARMS[AIRLIFT_PLACES_ARMS$arm == AIRLIFT_PLACES_BASELINE_ARM, ]
report(nrow(base_row) == 1L && base_row$critical == frame$critical_capacity &&
         base_row$standard == frame$standard_capacity &&
         base_row$interval == shipped$vars$role4$ame$schedule_interval_days,
       "the baseline arm is the shipped split and interval (%s/%s every %s days)",
       frame$critical_capacity, frame$standard_capacity,
       shipped$vars$role4$ame$schedule_interval_days)
report(all(AIRLIFT_PLACES_ARMS$critical + AIRLIFT_PLACES_ARMS$standard ==
             frame$critical_capacity + frame$standard_capacity),
       "every arm flies the airframe's %s places",
       frame$critical_capacity + frame$standard_capacity)
labels <- airlift_places_label(AIRLIFT_PLACES_ARMS$critical, AIRLIFT_PLACES_ARMS$standard,
                               AIRLIFT_PLACES_ARMS$interval)
report(all(AIRLIFT_PLACES_ARMS$arm == labels) && anyDuplicated(AIRLIFT_PLACES_ARMS$arm) == 0L,
       "every arm's label states its split and interval, and none is listed twice")

# ── 2. The tracked evidence sets are that experiment ────────────────────────

cat("\n-- the tracked evidence sets carry the documented design --\n")

#' Read one tracked file of a sweep
#'
#' @param dir Evidence directory.
#' @param stem File stem without the scenario suffix or extension.
#' @param suffix Scenario suffix.
#' @return The data frame, or NULL where the file is absent.
read_sweep <- function(dir, stem, suffix) {
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

#' The airlift sweep's replications for one configuration
#'
#' @param suffix Scenario suffix of the places sweep's set.
#' @param interval Sortie interval of the arm.
#' @return The matching rows of the strategic airlift sweep, or NULL.
#'
#' @details The airlift sweep's moderate file carries the shipped arm of both
#'   intensities ("baseline" and "high") and the moderate interval arms; its
#'   high file carries the high interval arms.
airlift_rows <- function(suffix, interval) {
  if (identical(suffix, "")) {
    d <- read_sweep(AIRLIFT_DIR, "airlift_replications", "")
    d[d$arm == "interval" & d$setting == "schedule_interval_days" & d$value == interval, ]
  } else {
    d <- read_sweep(AIRLIFT_DIR, "airlift_replications", "_high_intensity")
    d[d$arm == "interval" & d$setting == "schedule_interval_days" & d$value == interval, ]
  }
}

for (label in names(SUFFIXES)) {
  suffix <- SUFFIXES[[label]]
  rows <- read_sweep(EVIDENCE_DIR, "airlift_places_replications", suffix)
  summ <- read_sweep(EVIDENCE_DIR, "airlift_places_summary", suffix)
  paired <- read_sweep(EVIDENCE_DIR, "airlift_places_paired", suffix)
  coll <- read_sweep(EVIDENCE_DIR, "airlift_places_collapse_replications", suffix)
  coll_summ <- read_sweep(EVIDENCE_DIR, "airlift_places_collapse", suffix)
  present <- !is.null(rows) && !is.null(summ) && !is.null(paired) && !is.null(coll) &&
    !is.null(coll_summ)
  report(present, "%s: the replications, summary, paired and collapse files are tracked", label)
  if (!present) next

  report(setequal(unique(rows$arm), AIRLIFT_PLACES_ARMS$arm),
         "%s: the arms are the documented eight (%s)", label,
         paste(sort(unique(rows$arm)), collapse = "; "))
  per_arm <- table(rows$arm)
  report(all(per_arm == AIRLIFT_PLACES_REPLICATIONS),
         "%s: every arm carries %d replications (%s)", label, AIRLIFT_PLACES_REPLICATIONS,
         paste(per_arm, collapse = ", "))
  numbered <- all(vapply(split(rows$replication, rows$arm), function(r) {
    identical(sort(as.integer(r)), seq_len(AIRLIFT_PLACES_REPLICATIONS))
  }, logical(1)))
  report(numbered, "%s: replications are numbered from one in every arm", label)
  report(all(AIRLIFT_PLACES_PAIRED_RESPONSES %in% names(rows)),
         "%s: the response set is present (missing: %s)", label,
         paste(setdiff(AIRLIFT_PLACES_PAIRED_RESPONSES, names(rows)), collapse = ", "))

  recomputed <- do.call(rbind, lapply(AIRLIFT_PLACES_ARMS$arm, function(a) {
    cbind(data.frame(arm = a),
          summarise_policy(rows[rows$arm == a, setdiff(names(rows), "arm")],
                           arm_column = "arm_placeholder"))
  }))
  key <- paste(summ$arm, summ$response)
  report(identical(sort(key), sort(paste(recomputed$arm, recomputed$response))) &&
           numerics_agree(summ[order(key), ],
                          recomputed[order(paste(recomputed$arm, recomputed$response)), ]),
         "%s: the summary is the reduction of the per-replication responses", label)

  others <- setdiff(AIRLIFT_PLACES_ARMS$arm, AIRLIFT_PLACES_BASELINE_ARM)
  ok <- TRUE
  for (a in others) for (response in AIRLIFT_PLACES_PAIRED_RESPONSES) {
    mine <- policy_paired_difference(rows, response, AIRLIFT_PLACES_BASELINE_ARM, a,
                                     arm_column = "arm")
    theirs <- paired[paired$to == a & paired$response == response, ]
    ok <- ok && nrow(theirs) == 1L &&
      isTRUE(all.equal(mine$difference, theirs$difference, tolerance = TOL)) &&
      isTRUE(all.equal(mine$ci_lower, theirs$ci_lower, tolerance = TOL))
  }
  report(ok, "%s: every paired difference is taken within replication against '%s'", label,
         AIRLIFT_PLACES_BASELINE_ARM)
  unadjusted <- paired[, setdiff(names(paired), "p_holm")]
  report(nrow(paired) == length(others) * length(AIRLIFT_PLACES_PAIRED_RESPONSES) &&
           isTRUE(all.equal(paired$p_holm, holm_adjust(unadjusted)$p_holm)),
         "%s: one paired row per arm and response, Holm-adjusted over the file (%d)", label,
         nrow(paired))

  series_files <- file.path(
    EVIDENCE_DIR, sprintf("airlift_places_arm_c%d_i%d%s_series.csv.gz",
                          AIRLIFT_PLACES_ARMS$critical, AIRLIFT_PLACES_ARMS$interval, suffix)
  )
  have <- file.exists(series_files)
  report(all(have), "%s: every arm's route series is tracked (%d of %d)", label, sum(have),
         length(have))
  if (all(have)) {
    series <- do.call(rbind, lapply(seq_along(series_files), function(i) {
      d <- read.csv(series_files[i], stringsAsFactors = FALSE)
      d$arm <- AIRLIFT_PLACES_ARMS$arm[i]
      d
    }))
    again <- airlift_places_collapse(series, AIRLIFT_PLACES_DAYS)
    k_mine <- paste(again$arm, again$route, again$replication)
    k_tracked <- paste(coll$arm, coll$route, coll$replication)
    again <- again[match(k_tracked, k_mine), ]
    report(identical(as.logical(again$collapsed), as.logical(coll$collapsed)) &&
             numerics_agree(coll, again[, names(coll)]),
           "%s: the collapse file is the classification of the tracked route series", label)
  }
  summ_again <- do.call(rbind, lapply(seq_len(nrow(coll_summ)), function(i) {
    summarise_collapse(coll[coll$arm == coll_summ$arm[i] & coll$route == coll_summ$route[i], ])
  }))
  report(numerics_agree(coll_summ[, names(summ_again)], summ_again),
         "%s: the collapse summary is the reduction of the per-campaign classification", label)

  # ── 3. The shipped split reproduces the strategic airlift sweep ───────────

  for (interval in AIRLIFT_PLACES_INTERVALS) {
    mine_arm <- airlift_places_label(36L, 54L, interval)
    a <- rows[rows$arm == mine_arm, ]
    b <- airlift_rows(suffix, interval)
    shared <- setdiff(intersect(names(a), names(b)), c("arm", "setting", "value", "scenario"))
    if (is.null(b) || nrow(b) == 0) {
      report(FALSE, "%s: the strategic airlift sweep tracks the %d-day interval", label, interval)
      next
    }
    a <- a[order(a$replication), shared]
    b <- b[order(b$replication), shared]
    report(nrow(a) == AIRLIFT_PLACES_REPLICATIONS && nrow(b) == nrow(a) && numerics_agree(a, b),
           "%s: '%s' reproduces the strategic airlift sweep in all %d shared responses",
           label, mine_arm, length(shared))
  }
}

# ── 4. The results paper prints the tables ──────────────────────────────────

cat("\n-- the results paper prints the sweep's tables --\n")

paper <- paste(readLines(PAPER_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (name in c("airlift_places", "airlift_places_high", "airlift_places_paired",
               "airlift_places_paired_high")) {
  report(grepl(sprintf("<!-- GEN %s -->", name), paper, fixed = TRUE),
         "Results.md prints the generated table '%s'", name)
}

# ── 5. The collapse rule judges each arm against its own places ─────────────

cat("\n-- the collapse rule on hand-computed input --\n")

# A critical backlog rising from 30 to about 66 over the year drifts upwards
# with a late-half mean near 57: above the 36 places a shipped sortie opens, so
# a collapse, and below the 72 a reconfigured one opens, so not a collapse.
#' A constructed route series for one arm
#'
#' @param arm Arm label.
#' @return The series in the long form airlift_places_collapse() takes.
toy_series <- function(arm) {
  do.call(rbind, lapply(1:3, function(r) {
    day <- seq_len(AIRLIFT_PLACES_DAYS)
    rbind(
      data.frame(day = day, series = "backlog", subject = "Critical airlift",
                 value = 30 + day / 10 + r, replication = r, arm = arm),
      data.frame(day = day, series = "decisions", subject = "Critical airlift",
                 value = 5, replication = r, arm = arm),
      data.frame(day = day, series = "places_opened", subject = "Critical airlift",
                 value = 5, replication = r, arm = arm)
    )
  }))
}
toy <- rbind(toy_series(airlift_places_label(36L, 54L, 7L)),
             toy_series(airlift_places_label(72L, 18L, 7L)))
got <- suppressWarnings(airlift_places_collapse(toy, AIRLIFT_PLACES_DAYS))
report(all(got$collapsed[got$arm == airlift_places_label(36L, 54L, 7L)]),
       "a backlog rising past 36 places collapses the shipped split's critical route")
report(!any(got$collapsed[got$arm == airlift_places_label(72L, 18L, 7L)]),
       "the same backlog does not collapse a route opening 72 places a sortie")

# ── 6. The setter ───────────────────────────────────────────────────────────

cat("\n-- the places setter --\n")

set <- set_airframe_places(shipped, 54L, 36L)
moved <- resolve_ame_airframe(set$vars$role4)
report(moved$critical_capacity == 54L && moved$standard_capacity == 36L,
       "the split set is the split the airframe resolver reads (54/36)")
report(identical(set$vars$role4$ame, shipped$vars$role4$ame) &&
         identical(set$elms, shipped$elms),
       "and the sortie schedule and the establishment are left as they were")
report(isTRUE(all(unname(airlift_sortie_places(set$vars$role4)) == c(54, 36))),
       "the collapse threshold the airlift module reads follows the split")
for (bad in list(c(40L, 40L), c(-1L, 91L), c(36.5, 53.5), c(NA, 54L))) {
  report(inherits(try(set_airframe_places(shipped, bad[1], bad[2]), silent = TRUE),
                  "try-error"),
         "a split of '%s' is rejected", paste(format(bad), collapse = "/"))
}
unknown <- shipped
unknown$vars$role4$ame$airframe <- "absent"
report(inherits(try(set_airframe_places(unknown, 36L, 54L), silent = TRUE), "try-error"),
       "a configuration naming an airframe with no capacity block is rejected")

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nThe critical-care places sweep's protocol, evidence and published section agree.\n")
quit(status = 0L)
