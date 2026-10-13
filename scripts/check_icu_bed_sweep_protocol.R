#!/usr/bin/env Rscript
##############################################################################
## scripts/check_icu_bed_sweep_protocol.R                                   ##
## Regression check — the R2E intensive care bed sweep's parameters, its    ##
## responses and its published section agree                                ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_icu_bed_sweep_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. R2E intensive care is the pool whose shortfall the
# results paper measures most severely, and its establishment was never varied
# until this sweep. A finding that rests on an experiment is only as good as that
# experiment's agreement with the document that reports it, and earlier
# experiments drifted from their documentation more than once. This check keeps
# the sweep's protocol, its tracked evidence set and its published section in
# step, and defends the one modelling fact the design depends on: that an R2E
# intensive care stay seizes the bed alone.
#
# What this asserts:
#
#   1. Every protocol parameter in R/icu_bed_sweep.R equals the value
#      docs/Methods.md documents in a marker comment, the baseline arm is the
#      shipped establishment in env_data.json, and each arm's label states its
#      bed count.
#   2. Each tracked evidence set, the shipped configuration's and the
#      high-intensity profile's, carries every documented arm at the documented
#      replication count numbered from one and the documented responses, with
#      the summary, the paired differences and the queue classification each the
#      reduction of the per-replication evidence beside them.
#   3. Results.md prints the sweep's summary and paired tables for both
#      intensities as generated spans.
#   4. The classification and its shortfall per day are correct on a series
#      whose answer is computable by hand, for each classified pool.
#   5. The establishment setter changes only the R2E intensive care beds and
#      refuses a count it cannot apply, and the staffing ratio is arithmetic.
#   6. As an absence: no stay of the R2E treatment trajectory seizes the
#      intensive care section, which is what lets the sweep vary beds alone. If
#      a later change makes a stay seize staff this fails, and the design in
#      docs/Methods.md must be revisited.
#
# Assertion 4 is what keeps assertion 2 from being circular: the stability file
# would match any classification the code happened to produce.

source("R/constants.R")
source("R/long_horizon.R")
source("R/policy_sweep.R")
source("R/hold_window.R")
source("R/surgical_team_sweep.R")
source("R/icu_bed_sweep.R")

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
EVIDENCE_DIR <- file.path("data", "icu_beds")

#' Scenario suffixes of the two tracked evidence sets
SUFFIXES <- c(moderate = "", high = "_high_intensity")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

# ── 1. The code's parameters are the ones the methods paper documents ───────

cat("\n-- the protocol's parameters match the methods paper --\n")

methods_text <- paste(readLines(METHODS_PATH, warn = FALSE), collapse = "\n")

#' Read one intensive care bed protocol parameter the methods paper states in a marker
#'
#' @param name Marker name, as it appears after "ICU_BEDS ".
#' @return The marker's value as a character string, or NA where absent.
#'
#' @details Marked rather than parsed out of the prose, on the convention
#'   `scripts/check_airlift_protocol.R` established.
icu_marker <- function(name) {
  m <- regmatches(methods_text,
                  regexpr(sprintf("<!-- ICU_BEDS %s=[^>]+ -->", name), methods_text))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- ICU_BEDS [^=]+=(.*) -->$", "\\1", m)
}

report(identical(as.integer(icu_marker("replications")), ICU_BED_SWEEP_REPLICATIONS),
       "replications: code %d, methods paper %s", ICU_BED_SWEEP_REPLICATIONS,
       icu_marker("replications"))
report(identical(as.integer(icu_marker("days")), ICU_BED_SWEEP_DAYS),
       "days: code %d, methods paper %s", ICU_BED_SWEEP_DAYS, icu_marker("days"))
report(identical(as.integer(icu_marker("seed")), ICU_BED_SWEEP_SEED),
       "seed: code %d, methods paper %s", ICU_BED_SWEEP_SEED, icu_marker("seed"))
report(identical(as.integer(icu_marker("window_days")), ICU_BED_SWEEP_WINDOW_DAYS),
       "closing window: code %d, methods paper %s", ICU_BED_SWEEP_WINDOW_DAYS,
       icu_marker("window_days"))
report(identical(icu_marker("baseline"), ICU_BED_SWEEP_BASELINE_ARM),
       "baseline arm: code '%s', methods paper '%s'", ICU_BED_SWEEP_BASELINE_ARM,
       icu_marker("baseline"))

shipped <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
#' R2E intensive care beds in the shipped configuration
#'
#' @return The `qty` of the R2E element's intensive care bed pool.
shipped_icu_beds <- function() {
  for (e in shipped$elms) {
    if (!identical(e$elm, "r2eheavy")) next
    for (b in e$beds) if (identical(b$name, "icu")) return(b$qty)
  }
  NA_integer_
}
base_row <- ICU_BED_SWEEP_ARMS[ICU_BED_SWEEP_ARMS$arm == ICU_BED_SWEEP_BASELINE_ARM, ]
report(nrow(base_row) == 1L && base_row$icu_beds == shipped_icu_beds() &&
         ICU_BED_SWEEP_BASELINE == shipped_icu_beds(),
       "the baseline arm is the shipped establishment (%s beds in env_data.json)",
       shipped_icu_beds())
report(all(ICU_BED_SWEEP_ARMS$arm == sprintf("%d beds", ICU_BED_SWEEP_ARMS$icu_beds)),
       "every arm's label states the bed count it runs")
report(anyDuplicated(ICU_BED_SWEEP_ARMS$arm) == 0L, "no arm is listed twice")
report(identical(strsplit(icu_marker("beds"), ";", fixed = TRUE)[[1]],
                 as.character(ICU_BED_SWEEP_BEDS)),
       "beds: code '%s', methods paper '%s'", paste(ICU_BED_SWEEP_BEDS, collapse = ";"),
       icu_marker("beds"))

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
  rows <- read_sweep("icu_bed_sweep_replications", suffix)
  summ <- read_sweep("icu_bed_sweep_summary", suffix)
  paired <- read_sweep("icu_bed_sweep_paired", suffix)
  stab <- read_sweep("icu_bed_sweep_stability", suffix)
  report(!is.null(rows) && !is.null(summ) && !is.null(paired) && !is.null(stab),
         "%s: the replications, summary, paired and stability files are tracked", label)
  if (is.null(rows) || is.null(summ) || is.null(paired) || is.null(stab)) next

  report(setequal(unique(rows$arm), ICU_BED_SWEEP_ARMS$arm),
         "%s: the arms are the documented five establishments (%s)", label,
         paste(sort(unique(rows$arm)), collapse = "; "))
  per_arm <- table(rows$arm)
  report(all(per_arm == ICU_BED_SWEEP_REPLICATIONS),
         "%s: every arm carries %d replications (%s)", label, ICU_BED_SWEEP_REPLICATIONS,
         paste(per_arm, collapse = ", "))
  numbered <- all(vapply(split(rows$replication, rows$arm), function(r) {
    identical(sort(as.integer(r)), seq_len(ICU_BED_SWEEP_REPLICATIONS))
  }, logical(1)))
  report(numbered, "%s: replications are numbered from one in every arm", label)
  needed <- c(ICU_BED_SWEEP_PAIRED_RESPONSES, "total_casualties")
  report(all(needed %in% names(rows)), "%s: the response set is present (missing: %s)", label,
         paste(setdiff(needed, names(rows)), collapse = ", "))

  recomputed <- do.call(rbind, lapply(ICU_BED_SWEEP_ARMS$arm, function(a) {
    cbind(data.frame(arm = a),
          summarise_policy(rows[rows$arm == a, setdiff(names(rows), "arm")],
                           arm_column = "arm_placeholder"))
  }))
  key <- paste(summ$arm, summ$response)
  report(identical(sort(key), sort(paste(recomputed$arm, recomputed$response))) &&
           numerics_agree(summ[order(key), ],
                          recomputed[order(paste(recomputed$arm, recomputed$response)), ]),
         "%s: the summary is the reduction of the per-replication responses", label)

  others <- setdiff(ICU_BED_SWEEP_ARMS$arm, ICU_BED_SWEEP_BASELINE_ARM)
  ok <- TRUE
  for (a in others) for (response in ICU_BED_SWEEP_PAIRED_RESPONSES) {
    mine <- policy_paired_difference(rows, response, ICU_BED_SWEEP_BASELINE_ARM, a,
                                     arm_column = "arm")
    theirs <- paired[paired$to == a & paired$response == response, ]
    ok <- ok && nrow(theirs) == 1L &&
      isTRUE(all.equal(mine$difference, theirs$difference, tolerance = TOL)) &&
      isTRUE(all.equal(mine$ci_lower, theirs$ci_lower, tolerance = TOL))
  }
  report(ok, "%s: every paired difference is taken within replication against '%s'", label,
         ICU_BED_SWEEP_BASELINE_ARM)
  report(nrow(paired) == length(others) * length(ICU_BED_SWEEP_PAIRED_RESPONSES),
         "%s: the paired file has one row per arm and response (%d)", label, nrow(paired))

  series_files <- file.path(
    EVIDENCE_DIR,
    sprintf("icu_bed_sweep_arm_%02d%s_series.csv.gz", ICU_BED_SWEEP_ARMS$icu_beds, suffix)
  )
  have <- file.exists(series_files)
  report(all(have), "%s: every arm's daily series is tracked (%d of %d)", label, sum(have),
         length(have))
  if (all(have)) {
    series <- do.call(rbind, lapply(seq_along(series_files), function(i) {
      d <- read.csv(series_files[i], stringsAsFactors = FALSE)
      d$arm <- ICU_BED_SWEEP_ARMS$arm[i]
      d
    }))
    again <- icu_bed_stability(series)
    again <- again[match(paste(stab$arm, stab$pool), paste(again$arm, again$pool)), ]
    report(identical(as.character(again$stability), as.character(stab$stability)) &&
             numerics_agree(stab, again[, names(stab)]),
           "%s: the stability file is the classification of the tracked daily series", label)
  }
}

# ── 3. The results paper prints the tables ──────────────────────────────────

cat("\n-- the results paper prints the sweep's tables --\n")

paper <- paste(readLines(PAPER_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (name in c("icu_bed", "icu_bed_high", "icu_bed_paired", "icu_bed_paired_high")) {
  report(grepl(sprintf("<!-- GEN %s -->", name), paper, fixed = TRUE),
         "Results.md prints the generated table '%s'", name)
}

# ── 4. The classification is correct on inputs computable by hand ───────────

cat("\n-- the classification on hand-computed input --\n")

#' A constructed daily series for one arm and pool
#'
#' @param arm Arm label.
#' @param f Function of the day giving the queue.
#' @param pool Pool the series is for.
#' @param n_reps Replications, each offset by its index so they differ.
#' @param n_days Campaign length in days.
#' @return The series in the long form icu_bed_stability() takes.
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
toy <- do.call(rbind, lapply(ICU_BED_SWEEP_POOLS, function(p) {
  rbind(toy_series("growing", function(d) 3 * d, p),
        toy_series("flat", function(d) rep(5, length(d)), p))
}))
got <- suppressWarnings(icu_bed_stability(toy))
for (p in ICU_BED_SWEEP_POOLS) {
  grow <- got[got$arm == "growing" & got$pool == p, ]
  flat <- got[got$arm == "flat" & got$pool == p, ]
  report(nrow(grow) == 1L && isTRUE(all.equal(grow$shortfall_per_day, 3, tolerance = 1e-6)) &&
           identical(as.character(grow$stability), "drifting"),
         "%s: a queue growing three a day has a shortfall of three a day and drifts", p)
  report(nrow(flat) == 1L && isTRUE(all.equal(flat$shortfall_per_day, 0, tolerance = 1e-6)) &&
           identical(as.character(flat$stability), "degenerate"),
         "%s: a flat queue has no shortfall and is classified degenerate", p)
}
report(inherits(try(icu_bed_stability(toy_series("short", function(d) d,
                                                  ICU_BED_SWEEP_POOLS[1], n_days = 90L)),
                    silent = TRUE), "try-error"),
       "a campaign too short for the late-half fit is refused rather than classified")

# ── 5. The setter and the staffing ratio ────────────────────────────────────

cat("\n-- the establishment setter and the staffing ratio --\n")

resolved <- list(elms = shipped$elms)
set <- set_icu_establishment(resolved, 9L)
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
report(bed_qty(set, "r2eheavy", "icu") == 9L, "the setter sets the R2E intensive care beds")
report(bed_qty(set, "r2eheavy", "hold") == bed_qty(resolved, "r2eheavy", "hold") &&
         bed_qty(set, "r2b", "icu") == bed_qty(resolved, "r2b", "icu"),
       "and leaves the R2E holding beds and the R2B intensive care beds as they were")
for (bad in list(0, 2.5, NA, c(4, 6))) {
  report(inherits(try(set_icu_establishment(resolved, bad), silent = TRUE), "try-error"),
         "a bed count of '%s' is rejected", paste(format(bad), collapse = ","))
}
report(inherits(try(set_icu_establishment(list(elms = list()), 6L), silent = TRUE),
                "try-error"),
       "a configuration without the R2E intensive care pool is rejected, not left unchanged")
report(icu_nurses_per_bed(4L, 3L) == 3 && icu_nurses_per_bed(12L, 3L) == 1,
       "nurses per bed is sections x nurses per section over beds (3 and 1)")

# ── 6. As an absence: no R2E stay seizes the intensive care section ─────────

cat("\n-- the intensive care section is not seized at R2E --\n")

traj <- readLines("R/trajectories.R", warn = FALSE)
start <- grep("^r2e_treat_wia <- function", traj)
body <- traj[start:(start + 40L)]
uses <- grep("icu_team", body, value = TRUE)
report(length(start) == 1L && length(uses) == 1L && grepl("<-", uses[1]),
       paste("icu_team appears once in r2e_treat_wia(), at its definition, and is passed",
             "to no stay (%d occurrences)"), length(uses))

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nThe intensive care bed sweep's protocol, evidence and published section agree.\n")
quit(status = 0L)
