#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_surgical_package_protocol.R                           ##
## Regression check — the surgical package sweep's parameters, its          ##
## responses and its published section agree                                ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_surgical_package_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The planning paper's first recommendation adds R2B and
# R2E surgery and R2E intensive care beds together, and this sweep is the one
# experiment that fields them together, so what the recommendation says each
# piece buys once the others are in place rests on it alone. This check keeps
# the sweep's protocol, its tracked evidence set and its published section in
# step, and defends the two facts the design depends on: that the five counts
# set on the configuration reach the pools and sections the model builds, and
# that the arms the R2B second theatre sweep also ran reproduce its tracked
# replications.
#
# What this asserts:
#
#   1. Every protocol parameter in R/surgical_package_sweep.R equals the value
#      docs/Methods.md documents in a marker comment, and the baseline arm is
#      the shipped establishment in env_data.json.
#   2. The tracked evidence set carries every documented arm at the documented
#      replication count numbered from one and the documented responses, with
#      the summary, the paired differences and the pool classification each the
#      reduction of the per-replication evidence beside them.
#   3. The shipped arm and the R2B package alone reproduce, in every response
#      the two sweeps share, the tracked high-intensity replications of the R2B
#      second theatre sweep's identical configurations under the same seed.
#   4. Results.md prints the sweep's summary and paired tables as generated
#      spans.
#   5. The setter sets the five counts it is given, each as the capacity of the
#      pool or the sections the model builds, and refuses a count it cannot
#      apply.
#
# Assertion 3 is what keeps assertion 2 from being circular: the summary and
# stability files would match any responses the code happened to produce.

suppressPackageStartupMessages({
  library(simmer)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/scenario.R")
source("R/analysis_common.R")
source("R/queue_series.R")
source("R/long_horizon.R")
source("R/policy_sweep.R")
source("R/hold_window.R")
source("R/surgical_team_sweep.R")
source("R/icu_bed_sweep.R")
source("R/ot_bed_sweep.R")
source("R/r2b_theatre_sweep.R")
source("R/surgical_package_sweep.R")

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
EVIDENCE_DIR <- file.path("data", "surgical_package")

#' Directory of the R2B second theatre sweep's tracked evidence set
THEATRE_DIR <- file.path("data", "r2b_theatre")

#' Scenario suffix of the tracked evidence set
SUFFIX <- paste0("_", PACKAGE_SWEEP_SCENARIO)

#' Arms this sweep shares with the R2B second theatre sweep, as this sweep's
#' label and that sweep's label for the same configuration
SHARED_ARMS <- stats::setNames(c("R2B 1 section, 1 theatre", "R2B 2 sections, 2 theatres"),
                               c(PACKAGE_SWEEP_BASELINE_ARM, PACKAGE_SWEEP_R2B_ARM))

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

# ── 1. The code's parameters are the ones the methods paper documents ───────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one protocol parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "PACKAGE ".
#' @return The marker's value as a character string, or NA where absent.
package_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- PACKAGE %s=[^>]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- PACKAGE [^=]+=(.*) -->$", "\\1", m)
}

for (p in list(c("replications", PACKAGE_SWEEP_REPLICATIONS), c("days", PACKAGE_SWEEP_DAYS),
               c("seed", PACKAGE_SWEEP_SEED), c("window_days", PACKAGE_SWEEP_WINDOW_DAYS))) {
  report(identical(as.integer(package_marker(p[1])), as.integer(p[2])),
         "%s: code %s, methods paper %s", p[1], p[2], package_marker(p[1]))
}
report(identical(package_marker("scenario"), PACKAGE_SWEEP_SCENARIO),
       "scenario: code '%s', methods paper '%s'", PACKAGE_SWEEP_SCENARIO,
       package_marker("scenario"))
report(identical(package_marker("baseline"), PACKAGE_SWEEP_BASELINE_ARM),
       "baseline arm: code '%s', methods paper '%s'", PACKAGE_SWEEP_BASELINE_ARM,
       package_marker("baseline"))
arms_code <- paste(sprintf("%d/%d/%d/%d/%d", PACKAGE_SWEEP_ARMS$r2b_sections,
                           PACKAGE_SWEEP_ARMS$r2b_theatres, PACKAGE_SWEEP_ARMS$r2e_sections,
                           PACKAGE_SWEEP_ARMS$r2e_theatres, PACKAGE_SWEEP_ARMS$icu_beds),
                   collapse = ";")
report(identical(package_marker("arms"), arms_code),
       "arms (R2B sections/theatres, R2E sections/theatres, ICU): code '%s', methods paper '%s'",
       arms_code, package_marker("arms"))

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

base_row <- PACKAGE_SWEEP_ARMS[PACKAGE_SWEEP_ARMS$arm == PACKAGE_SWEEP_BASELINE_ARM, ]
report(nrow(base_row) == 1L && base_row$r2b_theatres == bed_qty(shipped, "r2b", "ot") &&
         base_row$r2b_sections == sub_qty(shipped, "r2b", "surg") &&
         base_row$r2e_theatres == bed_qty(shipped, "r2eheavy", "ot") &&
         base_row$r2e_sections == sub_qty(shipped, "r2eheavy", "surg") &&
         base_row$icu_beds == bed_qty(shipped, "r2eheavy", "icu"),
       "the baseline arm is the shipped establishment")
report(anyDuplicated(PACKAGE_SWEEP_ARMS$arm) == 0L, "no arm is listed twice")
report(all(PACKAGE_SWEEP_COMPARISONS$from %in% PACKAGE_SWEEP_ARMS$arm) &&
         all(PACKAGE_SWEEP_COMPARISONS$to %in% PACKAGE_SWEEP_ARMS$arm),
       "every paired comparison names two swept arms")

# ── 2. The tracked evidence set is that experiment ──────────────────────────

cat("\n-- the tracked evidence set carries the documented design --\n")

#' Read one tracked file of a sweep
#'
#' @param dir Evidence directory.
#' @param stem File stem without the scenario suffix or extension.
#' @return The data frame, or NULL where the file is absent.
read_sweep <- function(dir, stem) {
  path <- file.path(dir, paste0(stem, SUFFIX, ".csv"))
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

rows <- read_sweep(EVIDENCE_DIR, "surgical_package_replications")
summ <- read_sweep(EVIDENCE_DIR, "surgical_package_summary")
paired <- read_sweep(EVIDENCE_DIR, "surgical_package_paired")
stab <- read_sweep(EVIDENCE_DIR, "surgical_package_stability")
tracked <- !is.null(rows) && !is.null(summ) && !is.null(paired) && !is.null(stab)
report(tracked, "the replications, summary, paired and stability files are tracked")

if (tracked) {
  report(setequal(unique(rows$arm), PACKAGE_SWEEP_ARMS$arm),
         "the arms are the documented %d establishments", nrow(PACKAGE_SWEEP_ARMS))
  per_arm <- table(rows$arm)
  report(all(per_arm == PACKAGE_SWEEP_REPLICATIONS),
         "every arm carries %d replications (%s)", PACKAGE_SWEEP_REPLICATIONS,
         paste(per_arm, collapse = ", "))
  numbered <- all(vapply(split(rows$replication, rows$arm), function(r) {
    identical(sort(as.integer(r)), seq_len(PACKAGE_SWEEP_REPLICATIONS))
  }, logical(1)))
  report(numbered, "replications are numbered from one in every arm")
  needed <- c(PACKAGE_SWEEP_PAIRED_RESPONSES, "icu_mean_queue", "overflow_r2e_staging")
  report(all(needed %in% names(rows)), "the response set is present (missing: %s)",
         paste(setdiff(needed, names(rows)), collapse = ", "))

  recomputed <- do.call(rbind, lapply(PACKAGE_SWEEP_ARMS$arm, function(a) {
    cbind(data.frame(arm = a),
          summarise_policy(rows[rows$arm == a, setdiff(names(rows), "arm")],
                           arm_column = "arm_placeholder"))
  }))
  key <- paste(summ$arm, summ$response)
  report(identical(sort(key), sort(paste(recomputed$arm, recomputed$response))) &&
           numerics_agree(summ[order(key), ],
                          recomputed[order(paste(recomputed$arm, recomputed$response)), ]),
         "the summary is the reduction of the per-replication responses")

  ok <- TRUE
  for (k in seq_len(nrow(PACKAGE_SWEEP_COMPARISONS))) {
    from <- PACKAGE_SWEEP_COMPARISONS$from[k]
    to <- PACKAGE_SWEEP_COMPARISONS$to[k]
    for (response in PACKAGE_SWEEP_PAIRED_RESPONSES) {
      mine <- policy_paired_difference(rows, response, from, to, arm_column = "arm")
      theirs <- paired[paired$from == from & paired$to == to & paired$response == response, ]
      ok <- ok && nrow(theirs) == 1L &&
        isTRUE(all.equal(mine$difference, theirs$difference, tolerance = TOL)) &&
        isTRUE(all.equal(mine$ci_lower, theirs$ci_lower, tolerance = TOL))
    }
  }
  report(ok, "every paired difference is taken within replication between its arms")
  report(nrow(paired) ==
           nrow(PACKAGE_SWEEP_COMPARISONS) * length(PACKAGE_SWEEP_PAIRED_RESPONSES),
         "the paired file has one row per comparison and response (%d)", nrow(paired))
  unadjusted <- paired[, setdiff(names(paired), "p_holm")]
  report(!is.null(paired$p_holm) &&
           isTRUE(all.equal(paired$p_holm, holm_adjust(unadjusted)$p_holm)),
         "Holm's adjustment is taken over every row of the paired file")

  series_names <- sprintf("surgical_package_arm_r2b_s%d_ot%d_r2e_s%d_ot%d_icu%d%s_series.csv.gz",
                          PACKAGE_SWEEP_ARMS$r2b_sections, PACKAGE_SWEEP_ARMS$r2b_theatres,
                          PACKAGE_SWEEP_ARMS$r2e_sections, PACKAGE_SWEEP_ARMS$r2e_theatres,
                          PACKAGE_SWEEP_ARMS$icu_beds, SUFFIX)
  series_files <- file.path(EVIDENCE_DIR, series_names)
  have <- file.exists(series_files)
  report(all(have), "every arm's daily series is tracked (%d of %d)", sum(have), length(have))
  if (all(have)) {
    series <- do.call(rbind, lapply(seq_along(series_files), function(i) {
      d <- read.csv(series_files[i], stringsAsFactors = FALSE)
      d$arm <- PACKAGE_SWEEP_ARMS$arm[i]
      d
    }))
    again <- ot_bed_stability(series)
    again <- again[match(paste(stab$arm, stab$pool), paste(again$arm, again$pool)), ]
    report(identical(as.character(again$stability), as.character(stab$stability)) &&
             numerics_agree(stab, again[, names(stab)]),
           "the stability file is the classification of the tracked daily series")
  }

  # ── 3. The arms the R2B second theatre sweep also ran reproduce its rows ──

  cat("\n-- the arms shared with the R2B second theatre sweep reproduce it --\n")

  theatre <- read_sweep(THEATRE_DIR, "r2b_theatre_sweep_replications")
  report(!is.null(theatre),
         "the R2B second theatre sweep's high-intensity replications are tracked")
  if (!is.null(theatre)) {
    for (mine_arm in names(SHARED_ARMS)) {
      a <- rows[rows$arm == mine_arm, ]
      b <- theatre[theatre$arm == SHARED_ARMS[[mine_arm]], ]
      shared <- setdiff(intersect(names(a), names(b)), "arm")
      a <- a[order(a$replication), shared]
      b <- b[order(b$replication), shared]
      report(nrow(a) == PACKAGE_SWEEP_REPLICATIONS && nrow(b) == nrow(a) &&
               numerics_agree(a, b),
             "'%s' reproduces the R2B second theatre sweep's '%s' in all %d shared responses",
             mine_arm, SHARED_ARMS[[mine_arm]], length(shared))
    }
  }
}

# ── 4. The results paper prints the tables ──────────────────────────────────

cat("\n-- the results paper prints the sweep's tables --\n")

paper <- paste(readLines(PAPER_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (name in c("surgical_package", "surgical_package_paired",
               "surgical_package_paired_steps")) {
  report(grepl(sprintf("<!-- GEN %s -->", name), paper, fixed = TRUE),
         "Results.md prints the generated table '%s'", name)
}

# ── 5. The setter, and the pools the model builds ───────────────────────────

cat("\n-- the setter and the pools it sizes --\n")

json_data <- read_config("env_data.json")
arm <- data.frame(r2b_sections = 2L, r2b_theatres = 2L, r2e_sections = 4L, r2e_theatres = 3L,
                  icu_beds = 8L)
described <- apply_package_setting(json_data, "default", arm)
#' Establishment of one pool of the described configuration
#'
#' @param pool Pool label, a name of LONG_HORIZON_POOLS.
#' @return The count of resources the pool's pattern matches.
built <- function(pool) pool_establishment(described$elms, LONG_HORIZON_POOLS[[pool]])
units <- length(described$elms$r2b)
report(built("R2B operating theatres") == arm$r2b_theatres * units &&
         built("R2E operating theatres") == arm$r2e_theatres &&
         built("R2E intensive care") == arm$icu_beds,
       paste("the R2B theatres, R2E theatres and intensive care beds set are the pools",
             "built (%d, %d, %d)"),
       built("R2B operating theatres"), built("R2E operating theatres"),
       built("R2E intensive care"))
r2b_built <- vapply(described$elms$r2b, function(u) {
  length(unique(sub("^c_r2b_surg_([0-9]+)_.*$", "\\1", unlist(u$surg))))
}, integer(1))
report(all(r2b_built == arm$r2b_sections), "every R2B unit carries the sections set (%s)",
       paste(r2b_built, collapse = ", "))
r2e_built <- length(described$elms$r2eheavy[[1]]$surg)
report(r2e_built == arm$r2e_sections, "the R2E surgical sections set are the sections built (%d)",
       r2e_built)
report(built("R2E holding beds") == bed_qty(shipped, "r2eheavy", "hold"),
       "R2E keeps its shipped holding beds (%d)", built("R2E holding beds"))
for (field in names(arm)) {
  bad <- arm
  bad[[field]] <- 0L
  report(inherits(try(apply_package_setting(json_data, "default", bad), silent = TRUE),
                  "try-error"),
         "a %s count of zero is rejected rather than applied", field)
}

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nThe surgical package sweep's protocol, evidence and published section agree.\n")
quit(status = 0L)
