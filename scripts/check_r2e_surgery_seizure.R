#!/usr/bin/env Rscript
##############################################################################
## scripts/check_r2e_surgery_seizure.R                                      ##
## Regression check — R2E surgery seizes a surgical section                 ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_r2e_surgery_seizure.R
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

failures <- character(0)

fail <- function(...) failures <<- c(failures, sprintf(...))

report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}

# ── Setup ───────────────────────────────────────────────────────────────────

# Globals the model reads directly, mirroring run_bch()'s setup in run.R.
env_data <<- load_elms("env_data.json")
day_min  <<- 1440L
counts   <<- sapply(env_data$elms, length)

# ── Check 1: structural ─────────────────────────────────────────────────────
# Every surgery block in the built R2E trajectory must seize and release the
# surgical section it operates under. simmer's verbose print recurses into
# branch forks and names the resource on each Seize/Release activity, so the
# built trajectory can be read back directly rather than inferred from the
# source text.

cat("\n-- Structural: R2E surgery blocks seize a surgical section --\n")

for (team_id in seq_along(env_data$elms$r2eheavy)) {
  env <<- simmer("check") %>% build_env(env_data)

  listing <- capture.output(print(r2e_treat_wia(team_id), verbose = TRUE))

  surg_names <- unlist(env_data$elms$r2eheavy[[team_id]][["surg"]])
  surg_pattern <- paste0("resource: (", paste(surg_names, collapse = "|"), ")")

  # Each surgery block is a named fork; the activities belonging to it run
  # from its header line to the next trajectory header.
  block_starts <- grep("trajectory: R2E DAMCON Surgery", listing)
  header_lines <- grep("trajectory:", listing)

  if (length(block_starts) == 0) {
    fail("team %d: no R2E DAMCON surgery blocks found in the built trajectory",
         team_id)
    report(FALSE, "team %d: no surgery blocks found", team_id)
    next
  }

  for (start in block_starts) {
    following <- header_lines[header_lines > start]
    end   <- if (length(following)) following[1] - 1 else length(listing)
    block <- listing[start:end]

    seizes   <- grepl("Activity: Seize",   block) & grepl(surg_pattern, block)
    releases <- grepl("Activity: Release", block) & grepl(surg_pattern, block)

    label <- trimws(sub(".*trajectory: ", "", listing[start]))

    ok <- any(seizes) && any(releases)
    if (!ok) {
      fail(paste("team %d, %s: surgery block seizes no surgical section",
                 "(seize: %s, release: %s)"),
           team_id, label, any(seizes), any(releases))
    }
    report(ok, "team %d, %s: %d surgical seizes, %d releases",
           team_id, label, sum(seizes), sum(releases))
  }

  # Both surgery branches, the Phase 3 first procedure and the Phase 4 second,
  # must be covered. Each records its own start attribute, so requiring both
  # inside surgery blocks catches a branch that was left unconverted.
  for (attr_name in c("r2e_surgery_1_start", "r2e_surgery_2_start")) {
    covered <- FALSE
    for (start in block_starts) {
      following <- header_lines[header_lines > start]
      end <- if (length(following)) following[1] - 1 else length(listing)
      if (any(grepl(attr_name, listing[start:end], fixed = TRUE))) covered <- TRUE
    }
    if (!covered) {
      fail("team %d: %s does not occur inside a surgical-seizure block",
           team_id, attr_name)
    }
    report(covered, "team %d: %s sits inside a surgical-seizure block",
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
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All R2E surgical seizure checks passed.\n")
quit(status = 0)
