#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_ot_bed_sweep_protocol.R                               ##
## Regression check — the R2E operating theatre sweep's parameters, its     ##
## responses and its published section agree                                ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_ot_bed_sweep_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The R2E theatre count was held at two in every arm of
# the surgical team sweep, so whether theatres or surgical sections limit R2E
# surgery past the shipped establishment could not be read from it; this sweep
# crosses the two. A finding that rests on an experiment is only as good as that
# experiment's agreement with the document that reports it, so this check keeps
# the sweep's protocol, its tracked evidence set and its published section in
# step, and defends the two facts the design depends on: that a theatre count
# set on the configuration reaches the model's theatre pool, and that the arms
# the surgical team sweep also ran reproduce its tracked replications.
#
# What this asserts:
#
#   1. Every protocol parameter in R/ot_bed_sweep.R equals the value
#      docs/Methods.md documents in a marker comment, the baseline arm is the
#      shipped establishment in env_data.json, and each arm's label states the
#      theatre and section counts it runs.
#   2. Each tracked evidence set, the shipped configuration's and the
#      high-intensity profile's, carries every documented arm at the documented
#      replication count numbered from one and the documented responses, with
#      the summary, the paired differences and the pool classification each the
#      reduction of the per-replication evidence beside them.
#   3. The shipped arm and the six-section arm at two theatres reproduce, in
#      every response the two sweeps share, the tracked replications of the
#      surgical team sweep's identical configurations under the same seed.
#   4. Results.md prints the sweep's summary and paired tables for both
#      intensities as generated spans.
#   5. The classification and its shortfall per day are correct on a series
#      whose answer is computable by hand, for each classified pool.
#   6. The theatre setter changes only the R2E theatres and refuses a count it
#      cannot apply, and the count it sets is the capacity of the theatre pool
#      the model builds.
#
# Assertions 3 and 5 are what keep assertion 2 from being circular: the summary
# and stability files would match any responses the code happened to produce.

suppressPackageStartupMessages({
  library(simmer)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/scenario.R")
source("R/analysis_common.R")
source("R/long_horizon.R")
source("R/policy_sweep.R")
source("R/hold_window.R")
source("R/surgical_team_sweep.R")
source("R/ot_bed_sweep.R")

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
EVIDENCE_DIR <- file.path("data", "ot_beds")

#' Directory of the surgical team sweep's tracked evidence set
SURGICAL_DIR <- file.path("data", "surgical_teams")

#' Scenario suffixes of the two tracked evidence sets
SUFFIXES <- c(moderate = "", high = "_high_intensity")

#' Arms this sweep shares with the surgical team sweep, as this sweep's label
#' and that sweep's label for the same configuration
SHARED_ARMS <- c("2 theatres, 3 sections" = "R2E 3, R2B 1",
                 "2 theatres, 6 sections" = "R2E 6, R2B 1")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

# ── 1. The code's parameters are the ones the methods paper documents ───────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one theatre sweep protocol parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "OT_BEDS ".
#' @return The marker's value as a character string, or NA where absent.
#'
#' @details Marked rather than parsed out of the prose, on the convention
#'   `tests/checks/check_airlift_protocol.R` established.
ot_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- OT_BEDS %s=[^>]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- OT_BEDS [^=]+=(.*) -->$", "\\1", m)
}

report(identical(as.integer(ot_marker("replications")), OT_SWEEP_REPLICATIONS),
       "replications: code %d, methods paper %s", OT_SWEEP_REPLICATIONS,
       ot_marker("replications"))
report(identical(as.integer(ot_marker("days")), OT_SWEEP_DAYS),
       "days: code %d, methods paper %s", OT_SWEEP_DAYS, ot_marker("days"))
report(identical(as.integer(ot_marker("seed")), OT_SWEEP_SEED),
       "seed: code %d, methods paper %s", OT_SWEEP_SEED, ot_marker("seed"))
report(identical(as.integer(ot_marker("window_days")), OT_SWEEP_WINDOW_DAYS),
       "closing window: code %d, methods paper %s", OT_SWEEP_WINDOW_DAYS,
       ot_marker("window_days"))
report(identical(ot_marker("baseline"), OT_SWEEP_BASELINE_ARM),
       "baseline arm: code '%s', methods paper '%s'", OT_SWEEP_BASELINE_ARM,
       ot_marker("baseline"))
arms_code <- paste(sprintf("%d/%d", OT_SWEEP_ARMS$theatres, OT_SWEEP_ARMS$sections),
                   collapse = ";")
report(identical(ot_marker("arms"), arms_code),
       "arms (theatres/sections): code '%s', methods paper '%s'", arms_code,
       ot_marker("arms"))

shipped <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
#' Quantity of one bed pool of one element in a configuration
#'
#' @param cfg A list carrying `elms`.
#' @param elm Element name.
#' @param bed Bed pool name.
#' @return The pool's `qty`.
bed_qty <- function(cfg, elm, bed) {
  for (e in cfg$elms) {
    if (!identical(e$elm, elm)) next
    for (b in e$beds) if (identical(b$name, bed)) return(b$qty)
  }
  NA_integer_
}
#' Quantity of one sub-element of one element in a configuration
#'
#' @param cfg A list carrying `elms`.
#' @param elm Element name.
#' @param sub Sub-element name.
#' @return The sub-element's `qty`.
sub_qty <- function(cfg, elm, sub) {
  for (e in cfg$elms) {
    if (!identical(e$elm, elm)) next
    for (s in e$sub_elms) if (identical(s$sub_elm, sub)) return(s$qty)
  }
  NA_integer_
}
base_row <- OT_SWEEP_ARMS[OT_SWEEP_ARMS$arm == OT_SWEEP_BASELINE_ARM, ]
report(nrow(base_row) == 1L && base_row$theatres == bed_qty(shipped, "r2eheavy", "ot") &&
         base_row$sections == sub_qty(shipped, "r2eheavy", "surg"),
       "the baseline arm is the shipped establishment (%s theatres, %s sections)",
       bed_qty(shipped, "r2eheavy", "ot"), sub_qty(shipped, "r2eheavy", "surg"))
report(all(OT_SWEEP_ARMS$arm == sprintf("%d theatres, %d sections", OT_SWEEP_ARMS$theatres,
                                        OT_SWEEP_ARMS$sections)),
       "every arm's label states the theatre and section counts it runs")
report(anyDuplicated(OT_SWEEP_ARMS$arm) == 0L, "no arm is listed twice")
report(all(OT_SWEEP_COMPARISONS$from %in% OT_SWEEP_ARMS$arm) &&
         all(OT_SWEEP_COMPARISONS$to %in% OT_SWEEP_ARMS$arm),
       "every paired comparison names two swept arms")

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

for (label in names(SUFFIXES)) {
  suffix <- SUFFIXES[[label]]
  rows <- read_sweep(EVIDENCE_DIR, "ot_bed_sweep_replications", suffix)
  summ <- read_sweep(EVIDENCE_DIR, "ot_bed_sweep_summary", suffix)
  paired <- read_sweep(EVIDENCE_DIR, "ot_bed_sweep_paired", suffix)
  stab <- read_sweep(EVIDENCE_DIR, "ot_bed_sweep_stability", suffix)
  report(!is.null(rows) && !is.null(summ) && !is.null(paired) && !is.null(stab),
         "%s: the replications, summary, paired and stability files are tracked", label)
  if (is.null(rows) || is.null(summ) || is.null(paired) || is.null(stab)) next

  report(setequal(unique(rows$arm), OT_SWEEP_ARMS$arm),
         "%s: the arms are the documented five establishments (%s)", label,
         paste(sort(unique(rows$arm)), collapse = "; "))
  per_arm <- table(rows$arm)
  report(all(per_arm == OT_SWEEP_REPLICATIONS),
         "%s: every arm carries %d replications (%s)", label, OT_SWEEP_REPLICATIONS,
         paste(per_arm, collapse = ", "))
  numbered <- all(vapply(split(rows$replication, rows$arm), function(r) {
    identical(sort(as.integer(r)), seq_len(OT_SWEEP_REPLICATIONS))
  }, logical(1)))
  report(numbered, "%s: replications are numbered from one in every arm", label)
  needed <- c(OT_SWEEP_PAIRED_RESPONSES, "hold_occupancy", "icu_mean_queue",
              "overflow_r2e_staging")
  report(all(needed %in% names(rows)), "%s: the response set is present (missing: %s)", label,
         paste(setdiff(needed, names(rows)), collapse = ", "))

  recomputed <- do.call(rbind, lapply(OT_SWEEP_ARMS$arm, function(a) {
    cbind(data.frame(arm = a),
          summarise_policy(rows[rows$arm == a, setdiff(names(rows), "arm")],
                           arm_column = "arm_placeholder"))
  }))
  key <- paste(summ$arm, summ$response)
  report(identical(sort(key), sort(paste(recomputed$arm, recomputed$response))) &&
           numerics_agree(summ[order(key), ],
                          recomputed[order(paste(recomputed$arm, recomputed$response)), ]),
         "%s: the summary is the reduction of the per-replication responses", label)

  ok <- TRUE
  for (k in seq_len(nrow(OT_SWEEP_COMPARISONS))) {
    from <- OT_SWEEP_COMPARISONS$from[k]
    to <- OT_SWEEP_COMPARISONS$to[k]
    for (response in OT_SWEEP_PAIRED_RESPONSES) {
      mine <- policy_paired_difference(rows, response, from, to, arm_column = "arm")
      theirs <- paired[paired$from == from & paired$to == to & paired$response == response, ]
      ok <- ok && nrow(theirs) == 1L &&
        isTRUE(all.equal(mine$difference, theirs$difference, tolerance = TOL)) &&
        isTRUE(all.equal(mine$ci_lower, theirs$ci_lower, tolerance = TOL))
    }
  }
  report(ok, "%s: every paired difference is taken within replication between its arms",
         label)
  report(nrow(paired) == nrow(OT_SWEEP_COMPARISONS) * length(OT_SWEEP_PAIRED_RESPONSES),
         "%s: the paired file has one row per comparison and response (%d)", label,
         nrow(paired))
  unadjusted <- paired[, setdiff(names(paired), "p_holm")]
  report(!is.null(paired$p_holm) &&
           isTRUE(all.equal(paired$p_holm, holm_adjust(unadjusted)$p_holm)),
         "%s: Holm's adjustment is taken over every row of the paired file", label)

  series_files <- file.path(
    EVIDENCE_DIR,
    sprintf("ot_bed_sweep_arm_ot%d_s%d%s_series.csv.gz", OT_SWEEP_ARMS$theatres,
            OT_SWEEP_ARMS$sections, suffix)
  )
  have <- file.exists(series_files)
  report(all(have), "%s: every arm's daily series is tracked (%d of %d)", label, sum(have),
         length(have))
  if (all(have)) {
    series <- do.call(rbind, lapply(seq_along(series_files), function(i) {
      d <- read.csv(series_files[i], stringsAsFactors = FALSE)
      d$arm <- OT_SWEEP_ARMS$arm[i]
      d
    }))
    again <- ot_bed_stability(series)
    again <- again[match(paste(stab$arm, stab$pool), paste(again$arm, again$pool)), ]
    report(identical(as.character(again$stability), as.character(stab$stability)) &&
             numerics_agree(stab, again[, names(stab)]),
           "%s: the stability file is the classification of the tracked daily series", label)
  }

  # ── 3. The arms the surgical team sweep also ran reproduce its rows ───────

  surgical <- read_sweep(SURGICAL_DIR, "surgical_team_sweep_replications", suffix)
  report(!is.null(surgical), "%s: the surgical team sweep's replications are tracked", label)
  if (is.null(surgical)) next
  for (mine_arm in names(SHARED_ARMS)) {
    a <- rows[rows$arm == mine_arm, ]
    b <- surgical[surgical$arm == SHARED_ARMS[[mine_arm]], ]
    shared <- setdiff(intersect(names(a), names(b)), "arm")
    a <- a[order(a$replication), shared]
    b <- b[order(b$replication), shared]
    report(nrow(a) == OT_SWEEP_REPLICATIONS && nrow(b) == nrow(a) && numerics_agree(a, b),
           "%s: '%s' reproduces the surgical team sweep's '%s' in all %d shared responses",
           label, mine_arm, SHARED_ARMS[[mine_arm]], length(shared))
  }
}

# ── 4. The results paper prints the tables ──────────────────────────────────

cat("\n-- the results paper prints the sweep's tables --\n")

paper <- paste(readLines(PAPER_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (name in c("ot_bed", "ot_bed_high", "ot_bed_paired", "ot_bed_paired_high")) {
  report(grepl(sprintf("<!-- GEN %s -->", name), paper, fixed = TRUE),
         "Results.md prints the generated table '%s'", name)
}

# ── 5. The classification is correct on inputs computable by hand ───────────

cat("\n-- the classification on hand-computed input --\n")

#' A constructed daily series for one arm and pool
#'
#' @param arm Arm label.
#' @param f Function of the day giving the queue.
#' @param pool Pool the series is for.
#' @param n_reps Replications, each offset by its index so they differ.
#' @param n_days Campaign length in days.
#' @return The series in the long form ot_bed_stability() takes.
toy_series <- function(arm, f, pool, n_reps = 3L, n_days = 360L) {
  do.call(rbind, lapply(seq_len(n_reps), function(r) {
    data.frame(day = seq_len(n_days), series = "mean_queue", subject = pool,
               value = f(seq_len(n_days)) + r, replication = r, arm = arm)
  }))
}

# A queue growing three casualties a day has block means rising 90 a block, so
# its shortfall is exactly three a day; a flat queue has none and, never having
# moved, is classified degenerate rather than converged. Each classified pool is
# given the growing queue in one arm and the flat one in another, so a pool the
# function ignored would be reported absent.
toy <- do.call(rbind, lapply(OT_SWEEP_POOLS, function(p) {
  rbind(toy_series("growing", function(d) 3 * d, p),
        toy_series("flat", function(d) rep(5, length(d)), p))
}))
got <- suppressWarnings(ot_bed_stability(toy))
for (p in OT_SWEEP_POOLS) {
  grow <- got[got$arm == "growing" & got$pool == p, ]
  flat <- got[got$arm == "flat" & got$pool == p, ]
  report(nrow(grow) == 1L && isTRUE(all.equal(grow$shortfall_per_day, 3, tolerance = 1e-6)) &&
           identical(as.character(grow$stability), "drifting"),
         "%s: a queue growing three a day has a shortfall of three a day and drifts", p)
  report(nrow(flat) == 1L && isTRUE(all.equal(flat$shortfall_per_day, 0, tolerance = 1e-6)) &&
           identical(as.character(flat$stability), "degenerate"),
         "%s: a flat queue has no shortfall and is classified degenerate", p)
}
short <- toy_series("short", function(d) d, OT_SWEEP_POOLS[1], n_days = 90L)
report(inherits(try(ot_bed_stability(short), silent = TRUE), "try-error"),
       "a campaign too short for the late-half fit is refused rather than classified")

# ── 6. The setter, and the theatre pool the model builds ────────────────────

cat("\n-- the theatre setter and the pool it sizes --\n")

resolved <- list(elms = shipped$elms)
set <- set_ot_establishment(resolved, 3L)
report(bed_qty(set, "r2eheavy", "ot") == 3L, "the setter sets the R2E theatres")
report(bed_qty(set, "r2eheavy", "icu") == bed_qty(resolved, "r2eheavy", "icu") &&
         bed_qty(set, "r2b", "ot") == bed_qty(resolved, "r2b", "ot"),
       "and leaves the R2E intensive care beds and the R2B theatres as they were")
for (bad in list(0, 2.5, NA, c(2, 3))) {
  report(inherits(try(set_ot_establishment(resolved, bad), silent = TRUE), "try-error"),
         "a theatre count of '%s' is rejected", paste(format(bad), collapse = ","))
}
report(inherits(try(set_ot_establishment(list(elms = list()), 3L), silent = TRUE),
                "try-error"),
       "a configuration without the R2E theatre pool is rejected, not left unchanged")

json_data <- read_config("env_data.json")
for (theatres in c(2L, 4L)) {
  described <- apply_ot_setting(json_data, "default", theatres, 6L)
  env <- simmer()
  env <- build_env(env, described)
  report(simmer::get_capacity(env, section_pool("r2eheavy", "ot", 1L)) == theatres &&
           length(described$elms$r2eheavy[[1]]$ot_bed) == theatres,
         "%d theatres set on the configuration is the capacity of the theatre pool built",
         theatres)
}

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nThe theatre establishment sweep's protocol, evidence and published section agree.\n")
quit(status = 0L)
