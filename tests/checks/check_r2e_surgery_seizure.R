#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_r2e_surgery_seizure.R                                 ##
## Regression check — R2E surgery seizes a surgical section                 ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_r2e_surgery_seizure.R
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step.
#
# Why this check exists: R2E surgery has twice been built to seize an
# operating theatre bed without seizing the surgical section that staffs it,
# leaving R2E throughput limited by beds alone and the shift roster with no
# effect at that echelon. The defect is invisible in aggregate casualty
# counts (the model still runs, and still operates on everyone) and shows up
# only as surgical resources that report zero utilisation, so it survives
# ordinary inspection of run output. The two halves below check for it
# structurally, by reading the built trajectory, and behaviourally, by
# running the model and inspecting the resource monitor.
#
# The structural half locates the surgical blocks by the name formats
# R2E_SURGERY_SECTION_FMT and R2E_SURGERY_RELEASE_FMT, which
# R/trajectories_common.R owns and this script reads, so renaming those arms
# cannot leave the check searching for a label the model no longer uses. A
# casualty takes the first section free in one arm per section and releases
# it in another (seize_first_free_section(), release_held_section()), so each
# seize arm must seize a section, each release arm must release one, and each
# operation's start must fall between the two. A pattern that matches nothing
# is reported as a check-integrity problem rather than a model defect, since
# the two call for entirely different responses.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")

CHECK_DAYS <- 30L
CHECK_SEED <- 42L

failures  <- character(0)
integrity <- character(0)

#' Record a failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated failures, invisibly; called for its side effect.
fail <- function(...) failures <<- c(failures, sprintf(...))

#' Record a check-integrity failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated integrity failures, invisibly.
#' @details A check-integrity failure means the check can no longer see the
#'   model: the blocks it reads were renamed, or the model was restructured
#'   out from under it. That is a different problem from the model being
#'   wrong, and reporting it as a model defect sends the next reader looking
#'   in the wrong place, so the two are accumulated and printed separately.
broken <- function(...) integrity <<- c(integrity, sprintf(...))

#' Print one PASS or FAIL line
#'
#' @param ok Logical: whether the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Values interpolated into `fmt`.
#' @return The printed line, invisibly; called for its side effect.
report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}

#' Regex matching a surgical section block header, derived from its name format
#'
#' The format is owned by R/trajectories_common.R, so the check follows a rename
#' rather than carrying its own copy of the label. Literal regex metacharacters
#' in the format are escaped and the "%d" becomes a capturing group, so the
#' section index can be read back off each matched header.
#'
#' @param fmt sprintf format string naming one section block
#' @return Regex with one capturing group over the section index
section_header_pattern <- function(fmt) {
  escaped <- gsub("([][{}()*+?.\\\\^$|])", "\\\\\\1", fmt)
  paste0("trajectory: ", sub("%d", "([0-9]+)", escaped, fixed = TRUE), ",")
}

# ── Setup ───────────────────────────────────────────────────────────────────

# Globals the model reads directly, mirroring run_bch()'s setup in run.R.
env_data <<- load_elms("env_data.json")
day_min  <<- DAY_MIN
counts   <<- sapply(env_data$elms, length)

# ── Check 1: structural ─────────────────────────────────────────────────────
# Every surgery block in the built R2E trajectory must seize and release the
# surgical section it operates under. simmer's verbose print recurses into
# branch forks and names the resource on each Seize/Release activity, so the
# built trajectory can be read back directly rather than inferred from the
# source text.

cat("\n-- Structural: R2E surgery blocks seize a surgical section --\n")

seize_pattern   <- section_header_pattern(R2E_SURGERY_SECTION_FMT)
release_pattern <- section_header_pattern(R2E_SURGERY_RELEASE_FMT)

for (fmt in c(R2E_SURGERY_SECTION_FMT, R2E_SURGERY_RELEASE_FMT)) {
  if (!grepl("%d", fmt, fixed = TRUE)) {
    broken(paste("section arm name format '%s' carries no '%%d' section index,",
                 "so section coverage cannot be read back from the arm names"), fmt)
    report(FALSE, "section arm name format '%s' carries no section index", fmt)
  }
}

#' The listing lines of one named arm, from its header to the next header
#'
#' @param listing The verbose printed trajectory, one line per element.
#' @param start Index of the arm's header line.
#' @param header_lines Indices of every trajectory header line.
#' @return The arm's lines.
arm_lines <- function(listing, start, header_lines) {
  following <- header_lines[header_lines > start]
  listing[start:(if (length(following)) following[1] - 1 else length(listing))]
}

#' Assert that one kind of section arm covers every section evenly
#'
#' @param team_id Integer index of the Role 2E Heavy team.
#' @param starts Header line indices of the arms found.
#' @param pattern The arms' header regex, its one group the section index.
#' @param listing The verbose printed trajectory.
#' @param n_sections Configured surgical sections of the team.
#' @param kind "seize" or "release", for the message.
#' @return Invisible NULL; records a failure where coverage is uneven.
#'
#' @details Each section's arm is built from the same call sites, so every
#'   section appears the same number of times. A branch dropped or a section
#'   left uncovered shrinks what the per-arm assertions examine, which would
#'   otherwise pass silently on less.
check_coverage <- function(team_id, starts, pattern, listing, n_sections, kind) {
  found_ids   <- as.integer(sub(paste0(".*", pattern, ".*"), "\\1", listing[starts]))
  per_section <- table(factor(found_ids, levels = seq_len(n_sections)))
  ok <- all(per_section > 0) && length(unique(as.integer(per_section))) == 1 &&
    !any(found_ids > n_sections)
  if (!ok) {
    fail(paste("team %d: %d %s arms do not cover the %d configured",
               "surgical sections evenly (arms per section: %s)"),
         team_id, length(starts), kind, n_sections,
         paste(sprintf("%d:%d", seq_len(n_sections), as.integer(per_section)),
               collapse = ", "))
  }
  report(ok, "team %d: %d %s arms cover all %d surgical sections at %g site(s) each",
         team_id, length(starts), kind, n_sections, length(starts) / n_sections)
  invisible(NULL)
}

for (team_id in seq_along(env_data$elms$r2eheavy)) {
  env <<- simmer("check") %>% build_env(env_data)

  listing <- capture.output(print(r2e_treat_wia(team_id), verbose = TRUE))

  surg_names <- unlist(env_data$elms$r2eheavy[[team_id]][["surg"]])
  surg_pattern <- paste0("resource: (", paste(surg_names, collapse = "|"), ")")

  seize_starts   <- grep(seize_pattern, listing)
  release_starts <- grep(release_pattern, listing)
  header_lines   <- grep("trajectory:", listing)

  if (length(seize_starts) == 0 || length(release_starts) == 0) {
    broken(paste("team %d: this check could not locate the R2E surgical section",
                 "arms matching /%s/ and /%s/ — the model may be sound and the",
                 "check unable to see it, so confirm the arm names in",
                 "R/trajectories_r2e.R before treating this as a model defect"),
           team_id, seize_pattern, release_pattern)
    report(FALSE, "team %d: no arms matched /%s/ or /%s/ — check integrity, not model",
           team_id, seize_pattern, release_pattern)
    next
  }

  n_sections <- length(env_data$elms$r2eheavy[[team_id]][["surg"]])
  check_coverage(team_id, seize_starts, seize_pattern, listing, n_sections, "seize")
  check_coverage(team_id, release_starts, release_pattern, listing, n_sections, "release")

  for (start in c(seize_starts, release_starts)) {
    arm <- arm_lines(listing, start, header_lines)
    is_seize <- start %in% seize_starts
    hits <- grepl(if (is_seize) "Activity: Seize" else "Activity: Release", arm) &
      grepl(surg_pattern, arm)
    label <- trimws(sub(".*trajectory: ", "", listing[start]))
    ok <- any(hits)
    if (!ok) {
      fail("team %d, %s: arm %s no surgical section", team_id, label,
           if (is_seize) "seizes" else "releases")
    }
    report(ok, "team %d, %s: %d surgical %s", team_id, label, sum(hits),
           if (is_seize) "seizes" else "releases")
  }

  # Both procedures, the Phase 3 first and the Phase 4 second, must start while
  # a section is held: each start attribute must fall after a seize arm and
  # before the release arm that follows it, so a branch left unconverted fails.
  arm_starts <- sort(c(seize_starts, release_starts))
  for (attr_name in c("r2e_surgery_1_start", "r2e_surgery_2_start")) {
    at <- grep(attr_name, listing, fixed = TRUE)
    covered <- length(at) > 0 && all(vapply(at, function(i) {
      before <- arm_starts[arm_starts < i]
      after  <- arm_starts[arm_starts > i]
      length(before) > 0 && length(after) > 0 &&
        max(before) %in% seize_starts && min(after) %in% release_starts
    }, logical(1)))
    if (!covered) {
      fail("team %d: %s does not fall between a section seize and its release",
           team_id, attr_name)
    }
    report(covered, "team %d: %s falls between a section seize and its release",
           team_id, attr_name)
  }
}

# ── Check 2: behavioural ────────────────────────────────────────────────────
# A structural check alone would pass on a block that seizes a section nobody
# ever routes to. Running the model confirms the seizures bind: surgical
# resources must accumulate real usage, and concurrent R2E occupancy of a
# section must never exceed the one case that section can staff.

cat("\n-- Behavioural: seizures bind during a run --\n")

# run_once() writes a per-arrival trace to stdout; it is not wanted here.
invisible(capture.output(suppressWarnings(
  wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)
)))
resources <- get_mon_resources(wrapped)

for (team_id in seq_along(env_data$elms$r2eheavy)) {
  sections <- env_data$elms$r2eheavy[[team_id]][["surg"]]

  for (section_id in seq_along(sections)) {
    members <- sections[[section_id]]
    usage   <- resources %>% filter(resource %in% members)

    peak <- if (nrow(usage)) max(usage$server, na.rm = TRUE) else 0

    # Team-block seizure takes one unit of every member resource, each of
    # capacity 1, so a section in theatre shows server == 1 on its members
    # and a section that never operated shows 0 throughout.
    ok <- peak <= 1
    if (!ok) {
      fail(paste("team %d section %d: peak concurrent usage %s exceeds the",
                 "single case the section can staff"),
           team_id, section_id, peak)
    }
    report(ok, "team %d section %d: peak concurrent usage %s (limit 1)",
           team_id, section_id, peak)
  }

  # At least one section must have operated. Zero usage across all of them is
  # the exact signature of the defect this check guards against.
  all_members <- unlist(sections)
  operated <- resources %>%
    filter(resource %in% all_members) %>%
    summarise(peak = if (n()) max(server, na.rm = TRUE) else 0) %>%
    pull(peak)

  ok <- length(operated) && operated > 0
  if (!ok) {
    fail(paste("team %d: no surgical section was ever seized across %d days",
               "— R2E surgery is running unstaffed"), team_id, CHECK_DAYS)
  }
  report(ok, "team %d: surgical sections were seized during the run", team_id)

  # The shift roster must bind. build_env() gives each section an alternating
  # day or night schedule, which takes effect only if a seizure is attempted
  # against it: a section off shift holds zero capacity, so no seizure may
  # begin there. A procedure already under way when the shift closes keeps
  # what it holds until release, which is why this looks at the moment usage
  # rises rather than at every sample.
  usage <- resources %>%
    filter(resource %in% all_members) %>%
    arrange(resource, time) %>%
    group_by(resource) %>%
    mutate(previous = lag(server)) %>%
    ungroup() %>%
    filter(!is.na(previous), server > previous)

  off_shift <- usage %>% filter(capacity == 0)

  ok <- nrow(off_shift) == 0
  if (!ok) {
    fail("team %d: %d surgical seizures began while the section was off shift",
         team_id, nrow(off_shift))
  }
  report(ok, "team %d: %d surgical seizures, %d of them off shift",
         team_id, nrow(usage), nrow(off_shift))
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(integrity)) {
  cat(sprintf("%d check-integrity problem(s) — this check could not read the model,\n",
              length(integrity)))
  cat("which is not in itself evidence that the model is wrong:\n")
  for (f in integrity) cat(" - ", f, "\n", sep = "")
}
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
}
if (length(integrity) || length(failures)) quit(status = 1)

cat("All R2E surgical seizure checks passed.\n")
quit(status = 0)
