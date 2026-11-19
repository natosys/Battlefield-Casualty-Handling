#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_r2b_theatre_protocol.R                                ##
## Regression check — the R2B second theatre sweep's parameters, its        ##
## responses and its published section agree                                ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_r2b_theatre_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. A second R2B surgical section per unit ends the
# off-shift diversions to R2E but doubles the diversions because the single
# theatre is busy, and this sweep is the one experiment that adds a second R2B
# theatre, so the planning paper's statement of whether the theatre pays, and
# with how many sections, rests on it alone. This check keeps the sweep's
# protocol, its tracked evidence set and its published section in step, and
# defends the two facts the design depends on: that the counts set on the
# configuration reach the pools the model builds, and that the arms the surgical
# team sweep also ran reproduce its tracked replications.
#
# What this asserts:
#
#   1. Every protocol parameter in R/r2b_theatre_sweep.R equals the value
#      docs/Methods.md documents in a marker comment, the baseline arm is the
#      shipped establishment in env_data.json, and each arm's label states the
#      counts it runs.
#   2. At each intensity the tracked evidence set carries every documented arm
#      at the documented replication count numbered from one and the documented
#      responses, with the summary, the paired differences and the pool
#      classification each the reduction of the per-replication evidence
#      beside them.
#   3. At each intensity the shipped arm and the arm at two sections and one
#      theatre reproduce, in every response the two sweeps share, the surgical
#      team sweep's tracked replications of the identical configurations under
#      the same seed.
#   4. Results.md prints the sweep's summary and paired tables as generated
#      spans.
#   5. The setter sets the counts it is given, as the theatres built at every
#      R2B unit and the sections each unit carries, and refuses a count it
#      cannot apply.
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
source("R/ot_bed_sweep.R")
source("R/r2b_theatre_sweep.R")

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
EVIDENCE_DIR <- file.path("data", "r2b_theatre")

#' Directory of the surgical team sweep's tracked evidence set
SURGICAL_DIR <- file.path("data", "surgical_teams")

#' Scenario suffix of each intensity's tracked evidence set
SUFFIXES <- c(default = "", high_intensity = "_high_intensity")

#' Arms this sweep shares with the surgical team sweep, as this sweep's label
#' and that sweep's label for the same configuration
SHARED_ARMS <- stats::setNames(c("R2E 3, R2B 1", "R2E 3, R2B 2"),
                               c(r2b_theatre_arm_label(1L, 1L),
                                 r2b_theatre_arm_label(2L, 1L)))

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

# ── 1. The code's parameters are the ones the methods paper documents ───────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one protocol parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "R2B_THEATRE ".
#' @return The marker's value as a character string, or NA where absent.
r2b_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- R2B_THEATRE %s=[^>]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- R2B_THEATRE [^=]+=(.*) -->$", "\\1", m)
}

for (p in list(c("replications", R2B_THEATRE_SWEEP_REPLICATIONS),
               c("days", R2B_THEATRE_SWEEP_DAYS), c("seed", R2B_THEATRE_SWEEP_SEED),
               c("window_days", R2B_THEATRE_SWEEP_WINDOW_DAYS))) {
  report(identical(as.integer(r2b_marker(p[1])), as.integer(p[2])),
         "%s: code %s, methods paper %s", p[1], p[2], r2b_marker(p[1]))
}
report(identical(r2b_marker("baseline"), R2B_THEATRE_SWEEP_BASELINE_ARM),
       "baseline arm: code '%s', methods paper '%s'", R2B_THEATRE_SWEEP_BASELINE_ARM,
       r2b_marker("baseline"))
arms_code <- paste(sprintf("%d/%d", R2B_THEATRE_SWEEP_ARMS$sections,
                           R2B_THEATRE_SWEEP_ARMS$theatres), collapse = ";")
report(identical(r2b_marker("arms"), arms_code),
       "arms (sections/theatres per unit): code '%s', methods paper '%s'", arms_code,
       r2b_marker("arms"))

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

base_row <- R2B_THEATRE_SWEEP_ARMS[R2B_THEATRE_SWEEP_ARMS$arm == R2B_THEATRE_SWEEP_BASELINE_ARM, ]
report(nrow(base_row) == 1L && base_row$theatres == bed_qty(shipped, "r2b", "ot") &&
         base_row$sections == sub_qty(shipped, "r2b", "surg"),
       "the baseline arm is the shipped establishment")
report(all(R2B_THEATRE_SWEEP_ARMS$arm ==
             mapply(r2b_theatre_arm_label, R2B_THEATRE_SWEEP_ARMS$sections,
                    R2B_THEATRE_SWEEP_ARMS$theatres)),
       "every arm's label states the counts it runs")
report(anyDuplicated(R2B_THEATRE_SWEEP_ARMS$arm) == 0L, "no arm is listed twice")
report(all(R2B_THEATRE_SWEEP_COMPARISONS$from %in% R2B_THEATRE_SWEEP_ARMS$arm) &&
         all(R2B_THEATRE_SWEEP_COMPARISONS$to %in% R2B_THEATRE_SWEEP_ARMS$arm),
       "every paired comparison names two swept arms")

# ── 2. The tracked evidence set is that experiment ──────────────────────────

#' Read one tracked file of a sweep
#'
#' @param dir Evidence directory.
#' @param stem File stem without the scenario suffix or extension.
#' @param suffix Scenario suffix of the intensity read.
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

for (scenario in names(SUFFIXES)) {
  suffix <- SUFFIXES[[scenario]]
  cat(sprintf("\n-- %s: the tracked evidence set carries the documented design --\n",
              scenario))
  rows <- read_sweep(EVIDENCE_DIR, "r2b_theatre_sweep_replications", suffix)
  summ <- read_sweep(EVIDENCE_DIR, "r2b_theatre_sweep_summary", suffix)
  paired <- read_sweep(EVIDENCE_DIR, "r2b_theatre_sweep_paired", suffix)
  stab <- read_sweep(EVIDENCE_DIR, "r2b_theatre_sweep_stability", suffix)
  tracked <- !is.null(rows) && !is.null(summ) && !is.null(paired) && !is.null(stab)
  report(tracked, "%s: the replications, summary, paired and stability files are tracked", scenario)

  if (tracked) {
    report(setequal(unique(rows$arm), R2B_THEATRE_SWEEP_ARMS$arm),
           "%s: the arms are the documented %d establishments", scenario,
           nrow(R2B_THEATRE_SWEEP_ARMS))
    per_arm <- table(rows$arm)
    report(all(per_arm == R2B_THEATRE_SWEEP_REPLICATIONS),
           "%s: every arm carries %d replications (%s)", scenario, R2B_THEATRE_SWEEP_REPLICATIONS,
           paste(per_arm, collapse = ", "))
    numbered <- all(vapply(split(rows$replication, rows$arm), function(r) {
      identical(sort(as.integer(r)), seq_len(R2B_THEATRE_SWEEP_REPLICATIONS))
    }, logical(1)))
    report(numbered, "%s: replications are numbered from one in every arm", scenario)
    needed <- c(R2B_THEATRE_SWEEP_PAIRED_RESPONSES, "diverted_offshift", "held_r2b",
                "overflow_r2e_staging")
    report(all(needed %in% names(rows)), "%s: the response set is present (missing: %s)", scenario,
           paste(setdiff(needed, names(rows)), collapse = ", "))

    recomputed <- do.call(rbind, lapply(R2B_THEATRE_SWEEP_ARMS$arm, function(a) {
      cbind(data.frame(arm = a),
            summarise_policy(rows[rows$arm == a, setdiff(names(rows), "arm")],
                             arm_column = "arm_placeholder"))
    }))
    key <- paste(summ$arm, summ$response)
    report(identical(sort(key), sort(paste(recomputed$arm, recomputed$response))) &&
             numerics_agree(summ[order(key), ],
                            recomputed[order(paste(recomputed$arm, recomputed$response)), ]),
           "%s: the summary is the reduction of the per-replication responses", scenario)

    ok <- TRUE
    for (k in seq_len(nrow(R2B_THEATRE_SWEEP_COMPARISONS))) {
      from <- R2B_THEATRE_SWEEP_COMPARISONS$from[k]
      to <- R2B_THEATRE_SWEEP_COMPARISONS$to[k]
      for (response in R2B_THEATRE_SWEEP_PAIRED_RESPONSES) {
        mine <- policy_paired_difference(rows, response, from, to, arm_column = "arm")
        theirs <- paired[paired$from == from & paired$to == to & paired$response == response, ]
        ok <- ok && nrow(theirs) == 1L &&
          isTRUE(all.equal(mine$difference, theirs$difference, tolerance = TOL)) &&
          isTRUE(all.equal(mine$ci_lower, theirs$ci_lower, tolerance = TOL))
      }
    }
    report(ok, "%s: every paired difference is taken within replication between its arms", scenario)
    report(nrow(paired) ==
             nrow(R2B_THEATRE_SWEEP_COMPARISONS) * length(R2B_THEATRE_SWEEP_PAIRED_RESPONSES),
           "%s: the paired file has one row per comparison and response (%d)", scenario,
           nrow(paired))
    unadjusted <- paired[, setdiff(names(paired), "p_holm")]
    report(!is.null(paired$p_holm) &&
             isTRUE(all.equal(paired$p_holm, holm_adjust(unadjusted)$p_holm)),
           "%s: Holm's adjustment is taken over every row of the paired file", scenario)

    series_names <- sprintf("r2b_theatre_sweep_arm_s%d_ot%d%s_series.csv.gz",
                            R2B_THEATRE_SWEEP_ARMS$sections, R2B_THEATRE_SWEEP_ARMS$theatres,
                            suffix)
    series_files <- file.path(EVIDENCE_DIR, series_names)
    have <- file.exists(series_files)
    report(all(have), "%s: every arm's daily series is tracked (%d of %d)", scenario,
           sum(have), length(have))
    if (all(have)) {
      series <- do.call(rbind, lapply(seq_along(series_files), function(i) {
        d <- read.csv(series_files[i], stringsAsFactors = FALSE)
        d$arm <- R2B_THEATRE_SWEEP_ARMS$arm[i]
        d
      }))
      again <- ot_bed_stability(series)
      again <- again[match(paste(stab$arm, stab$pool), paste(again$arm, again$pool)), ]
      report(identical(as.character(again$stability), as.character(stab$stability)) &&
               numerics_agree(stab, again[, names(stab)]),
             "%s: the stability file is the classification of the tracked daily series", scenario)
    }

    # ── 3. The arms the theatre sweep also ran reproduce its rows ─────────────

    cat(sprintf("\n-- %s: the arms shared with the surgical team sweep reproduce it --\n",
                scenario))

    surgical <- read_sweep(SURGICAL_DIR, "surgical_team_sweep_replications", suffix)
    report(!is.null(surgical), "%s: the surgical team sweep's replications are tracked", scenario)
    if (!is.null(surgical)) {
      for (mine_arm in names(SHARED_ARMS)) {
        a <- rows[rows$arm == mine_arm, ]
        b <- surgical[surgical$arm == SHARED_ARMS[[mine_arm]], ]
        shared <- setdiff(intersect(names(a), names(b)), "arm")
        a <- a[order(a$replication), shared]
        b <- b[order(b$replication), shared]
        report(nrow(a) == R2B_THEATRE_SWEEP_REPLICATIONS && nrow(b) == nrow(a) &&
                 numerics_agree(a, b),
               "%s: '%s' reproduces the surgical team sweep's '%s' in all %d shared responses",
               scenario, mine_arm, SHARED_ARMS[[mine_arm]], length(shared))
      }
    }
  }
}

# ── 4. The results paper prints the tables ──────────────────────────────────

cat("\n-- the results paper prints the sweep's tables --\n")

paper <- paste(readLines(PAPER_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (name in c("r2b_theatre", "r2b_theatre_paired", "r2b_theatre_high",
               "r2b_theatre_paired_high")) {
  report(grepl(sprintf("<!-- GEN %s -->", name), paper, fixed = TRUE),
         "Results.md prints the generated table '%s'", name)
}

# ── 5. The setter, and the pools the model builds ───────────────────────────

cat("\n-- the setter and the pools it sizes --\n")

json_data <- read_config("env_data.json")
arm <- data.frame(sections = 4L, theatres = 2L)
described <- apply_r2b_theatre_setting(json_data, "default", arm)
units <- length(described$elms$r2b)
theatres_built <- pool_establishment(described$elms, LONG_HORIZON_POOLS[["R2B operating theatres"]])
report(theatres_built == arm$theatres * units,
       "the R2B theatres set are the pools built (%d across %d units)", theatres_built, units)
sections_built <- vapply(described$elms$r2b, function(u) {
  length(unique(sub("^c_r2b_surg_([0-9]+)_.*$", "\\1", unlist(u$surg))))
}, integer(1))
report(all(sections_built == arm$sections),
       "every R2B unit carries the sections set (%s)", paste(sections_built, collapse = ", "))
r2e_theatres <- pool_establishment(described$elms, LONG_HORIZON_POOLS[["R2E operating theatres"]])
report(r2e_theatres == bed_qty(shipped, "r2eheavy", "ot"),
       "R2E keeps its shipped theatres (%d)", r2e_theatres)
for (field in c("sections", "theatres")) {
  bad <- arm
  bad[[field]] <- 0L
  report(inherits(try(apply_r2b_theatre_setting(json_data, "default", bad), silent = TRUE),
                  "try-error"),
         "a %s count of zero is rejected rather than applied", field)
}

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nThe R2B second theatre sweep's protocol, evidence and published section agree.\n")
quit(status = 0L)
