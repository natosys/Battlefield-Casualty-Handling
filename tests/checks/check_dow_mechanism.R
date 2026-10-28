#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_dow_mechanism.R                                       ##
## Regression check — the died-of-wounds accrual mechanism's properties,    ##
## and the legacy setting's reproduction of the earlier model               ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_dow_mechanism.R
#   Rscript tests/checks/check_dow_mechanism.R --skip-reproduction   # omit the 360-day run
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The accrual mechanism (`dow.accrual.enabled` = 1) was
# introduced to make died-of-wounds risk respond to delayed and degraded care,
# which the earlier mechanism did not: it credited care retroactively, ended
# risk within hours of injury, and raised the ceiling for degraded care above
# the untreated one. Each of those defects is silent in an ordinary run, since
# a model with any of them still reports a plausible death count, so each
# property the mechanism exists to provide is asserted here:
#
#   1. Delay before surgery raises the death probability, the risk accrued
#      before an operation being charged at the ceiling in force before it.
#   2. For a Priority 1 casualty, recovery in intensive care carries less risk
#      than recovery in a holding bed, which carries less than no treatment at
#      all, at R2E and on the R2B forward stay.
#   3. A post-operative episode carries one care-quality multiplier, never a
#      product of two; an R2B forward holding-bed stay carries the holding-bed
#      multiplier alone.
#   4. A Priority 3 casualty's flat risk is charged once, at the first R2
#      arrival check, and never while awaiting airlift; a disease casualty
#      faces no risk at any check.
#   5. Over a campaign forced onto every degraded route, no casualty's ceiling
#      ever rises, and every banked hazard implies one of the configured
#      multipliers.
#   6. The legacy setting (`dow.accrual.enabled` = 0) reproduces the seed-42
#      evidence set of the model before the accrual mechanism, byte for byte,
#      so the change can be measured against a supported configuration.
#
# Assertions 1 to 4 drive the mechanism's helpers through a single casualty in
# a minimal simmer environment, so their probabilities are exact rather than
# estimated. Assertion 6 compares digests rather than files, the tracked set
# having since been regenerated under the accrual mechanism; LEGACY_DIGESTS are
# those of the set as it stood when the switch was introduced. A later change
# that alters the model outside the died-of-wounds mechanism changes the legacy
# setting's output too, and is the one occasion for revising them.

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
source("R/icu_bed_sweep.R")

args <- commandArgs(trailingOnly = TRUE)
SKIP_REPRODUCTION <- "--skip-reproduction" %in% args

#' Campaign length of the degraded-route run, in days
CAMPAIGN_DAYS <- 60L
#' Replications of the degraded-route run
CAMPAIGN_REPS <- 2L
#' Control seed of the degraded-route run
CAMPAIGN_SEED <- 42L
#' Forward stability window forcing R2B holding in the degraded-route run, minutes
FORWARD_WINDOW <- 720L

#' Seed-42, 360-day run whose output the legacy setting must reproduce
LEGACY_SEED <- 42L
LEGACY_DAYS <- 360L

#' SHA-256 of each file of the seed-42 set before the accrual mechanism
LEGACY_DIGESTS <- c(
  "logs.txt"                       = "3593d3e85df46ccf460a023ccc1b3ece190ec185226ca6b8ed6228361d4335c9",
  "data/arrivals_casualty_surge.txt" = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
  "data/arrivals_dnbi_cbt.txt"     = "2fe77f2693e2673855e113b39a02015a3abaf5f4b63dddd6177193d77c14e137",
  "data/arrivals_dnbi_spt.txt"     = "6b69962bdf21d57db0c93dbe7ee0963ebbdb0498d22e4dad9e9481613b874837",
  "data/arrivals_kia_cbt.txt"      = "d49b70dc1879e9730e04679227eaaf25221991d8c7b703161f6d96d146ad1e17",
  "data/arrivals_kia_spt.txt"      = "927beaac5dc61c8d2772b43c60198ee77357f7ccbe972880edf947d0a832f22f",
  "data/arrivals_wia_cbt.txt"      = "e9a4b4b30013e29e747f9a692fc1aa0523a90513d132094b667f90511dfb82d9",
  "data/arrivals_wia_spt.txt"      = "dbda3044b27fb6bea937d08909bd3025658849bda11e109bcb2206c2c4049b05",
  "data/casualty_surge_events.csv" = "92a3d3f38f6545948482b81f01d1006ec92be1affb48d5c55849e7a8d084db70",
  "data/seed42_verification.csv"   = "7d0357bb8f807382861d81d89e9554e71bcafe7f7fac7fc9b61e13ab43d7fc62"
)

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
#' @return Invisibly, `ok`.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (isTRUE(ok)) "PASS" else "FAIL", msg))
  if (!isTRUE(ok)) fail("%s", msg)
  invisible(ok)
}

json_data <- read_config("env_data.json")
shipped <- build_environment(resolve_scenario(json_data, "default"))
day_min <- DAY_MIN
te <- shipped$vars$dow$treatment_efficacy
dp <- shipped$vars$dow$params

# ── The single-casualty harness ─────────────────────────────────────────────

#' Run one casualty through a sequence of mechanism steps
#'
#' @param attrs Named numeric of the casualty's starting attributes; priority,
#'   dow_ceiling and dnbi_type at least.
#' @param steps Function of a trajectory returning it with the steps appended,
#'   built under the accrual mechanism.
#' @param seed Seed set before the run, so a death draw cannot vary between runs.
#' @return The attribute monitor of the run.
run_casualty <- function(attrs, steps, seed = 1L) {
  env_data <<- shipped
  env_data$vars$dow$accrual$enabled <<- 1
  env <<- simmer()
  trj <- trajectory() %>%
    set_attribute(c("injury_time", names(attrs)), function() c(now(env), attrs)) %>%
    steps()
  set.seed(seed)
  env %>% add_generator("casualty", trj, at(0), mon = 2) %>% run() %>% invisible()
  get_mon_attributes(env)
}

#' A died-of-wounds check that records its probability and lets the casualty on
#'
#' @param trj Trajectory to append to.
#' @param arrival,cohort As for dow_check().
#' @return The trajectory, with the check appended.
check_at <- function(trj, arrival = FALSE, cohort = TRUE) {
  dow_check(trj, arrival = arrival, cohort = cohort, died = trajectory(),
            survived = trajectory())
}

#' A post-operative episode of a given length in a bed of a given multiplier
#'
#' @param trj Trajectory to append to.
#' @param queue Minutes spent waiting for the bed.
#' @param stay Minutes in the bed.
#' @param multiplier Zero-argument function returning the bed's multiplier.
#' @return The trajectory, with the episode and its hazard appended.
episode <- function(trj, queue, stay, multiplier) {
  trj %>%
    mark_episode_entry() %>%
    timeout(queue) %>%
    mark_episode_bed() %>%
    timeout(stay) %>%
    accrue_post_op_hazard(multiplier)
}

#' The death probability over every check a monitored casualty passed
#'
#' @param mon Attribute monitor from run_casualty().
#' @return One less the product of each check's survival.
total_risk <- function(mon) {
  p <- mon$value[mon$key == "dow_check_p"]
  1 - prod(1 - p)
}

#' The probabilities of each check a monitored casualty passed, in order
#'
#' @param mon Attribute monitor from run_casualty().
#' @return Numeric vector of `dow_check_p` values.
check_probs <- function(mon) mon$value[mon$key == "dow_check_p"]

p1 <- c(priority = 1, dow_ceiling = dp$p1_p_max, dnbi_type = NA)

# ── 1. Delay before surgery ─────────────────────────────────────────────────

cat("-- delay before surgery raises the risk --\n")

#' A Priority 1 path operated on at a given time after injury
#'
#' @param surgery_at Minutes from injury to the end of the operation.
#' @return The path's total death probability.
operated_at <- function(surgery_at) {
  total_risk(run_casualty(p1, function(trj) {
    trj %>%
      timeout(20) %>% check_at(cohort = FALSE) %>%
      timeout(40) %>% apply_dow_factor(function() te$r2e_resus_factor) %>%
      timeout(surgery_at - 60) %>% apply_dow_factor(function() te$r2e_dcs1_factor) %>%
      timeout(900 - surgery_at) %>% check_at()
  }))
}
early_op <- operated_at(90)
late_op  <- operated_at(300)
report(late_op > early_op,
       "an operation ending 300 minutes after injury carries more risk (%.5f) than one at 90 (%.5f)",
       late_op, early_op)

# ── 2. Intensive care < holding bed < no treatment ──────────────────────────

cat("\n-- intensive care, then a holding bed, then no treatment --\n")

#' A Priority 1 path through R2E with its recovery in a bed of one multiplier
#'
#' @param multiplier Zero-argument function returning the recovery bed's
#'   multiplier, or NULL for a casualty who is neither treated nor operated on.
#' @return The path's total death probability.
r2e_path <- function(multiplier) {
  treated <- !is.null(multiplier)
  total_risk(run_casualty(p1, function(trj) {
    trj <- trj %>% timeout(20) %>% check_at(cohort = FALSE) %>%
      timeout(100) %>% check_at(arrival = TRUE) %>% timeout(45)
    if (treated) trj <- trj %>% apply_dow_factor(function() te$r2e_resus_factor)
    trj <- trj %>% timeout(95)
    if (treated) trj <- trj %>% apply_dow_factor(function() te$r2e_dcs1_factor)
    trj <- if (treated) episode(trj, 0, 1440, multiplier) else timeout(trj, 1440)
    trj <- trj %>% check_at() %>% timeout(120)
    if (treated) trj <- trj %>% apply_dow_factor(function() te$r2e_dcs2_factor)
    trj <- if (treated) episode(trj, 0, 1440, multiplier) else timeout(trj, 1440)
    trj %>% check_at()
  }))
}
icu_r2e  <- r2e_path(r2e_icu_multiplier)
hold_r2e <- r2e_path(hold_bed_multiplier)
none     <- r2e_path(NULL)
report(icu_r2e < hold_r2e && hold_r2e < none,
       "at R2E: intensive care %.5f < holding bed %.5f < no treatment %.5f",
       icu_r2e, hold_r2e, none)

#' A Priority 1 path operated on at R2B and held forward in a bed of one multiplier
#'
#' @param multiplier Zero-argument function returning the forward bed's multiplier.
#' @return The path's total death probability.
r2b_path <- function(multiplier) {
  total_risk(run_casualty(p1, function(trj) {
    trj %>% timeout(20) %>% check_at(cohort = FALSE) %>%
      timeout(40) %>% check_at(arrival = TRUE) %>%
      timeout(45) %>% apply_dow_factor(function() te$r2b_resus_factor) %>%
      timeout(95) %>% apply_dow_factor(function() te$r2b_dcs_factor) %>%
      episode(0, FORWARD_WINDOW, multiplier) %>%
      timeout(30) %>% check_at(arrival = TRUE)
  }))
}
icu_r2b  <- r2b_path(r2b_icu_multiplier)
hold_r2b <- r2b_path(hold_bed_multiplier)
report(icu_r2b < hold_r2b && hold_r2b < none,
       "on the R2B forward stay: intensive care %.5f < holding bed %.5f < no treatment %.5f",
       icu_r2b, hold_r2b, none)

# ── 3. One multiplier per episode ───────────────────────────────────────────

cat("\n-- one care-quality multiplier per episode --\n")

rate <- shipped$vars$dow$post_op_hazard$p1_rate / DAY_MIN
#' The hazard survival banked by one episode
#'
#' @param multiplier Zero-argument function returning the bed's multiplier.
#' @param queue Minutes spent waiting for the bed.
#' @param stay Minutes in the bed.
#' @return The `dow_hazard_s` the episode leaves.
banked <- function(multiplier, queue, stay) {
  mon <- run_casualty(p1, function(trj) episode(trj, queue, stay, multiplier))
  tail(mon$value[mon$key == "dow_hazard_s"], 1)
}
hold_bank <- banked(hold_bed_multiplier, 60, FORWARD_WINDOW)
report(isTRUE(all.equal(hold_bank, exp(-rate * te$hold_bed_penalty * (60 + FORWARD_WINDOW)))),
       "a forward holding-bed stay carries the holding-bed multiplier alone (%.4f), not %.4f",
       te$hold_bed_penalty, te$hold_bed_penalty * te$r2b_icu_penalty)
icu_bank <- banked(r2b_icu_multiplier, 60, FORWARD_WINDOW)
report(isTRUE(all.equal(icu_bank, exp(-rate * (te$hold_bed_penalty * 60 +
                                                  te$r2b_icu_penalty * FORWARD_WINDOW)))),
       "a forward intensive care stay takes %.2f in the bed and %.2f only while waiting for it",
       te$r2b_icu_penalty, te$hold_bed_penalty)

# ── 4. Priority 3 once; disease never ───────────────────────────────────────

cat("\n-- Priority 3 risk is charged once; disease faces none --\n")

p3 <- c(priority = 3, dow_ceiling = dp$p3_flat, dnbi_type = NA)
p3_probs <- check_probs(run_casualty(p3, function(trj) {
  trj %>% timeout(20) %>% check_at(cohort = FALSE) %>%
    timeout(40) %>% check_at(arrival = TRUE) %>%
    timeout(60) %>% check_at(arrival = TRUE) %>%
    timeout(600) %>% check_at()
}))
report(isTRUE(all.equal(p3_probs, c(0, dp$p3_flat, 0, 0))),
       "a Priority 3 casualty is charged %s at R1, R2B, R2E and the post-operative check",
       paste(format(p3_probs, digits = 3), collapse = ", "))

env_data <- shipped
env_data$vars$dow$accrual$enabled <- 1
env <- simmer()
probe <- trajectory() %>%
  set_attribute(c("priority", "r2e_surgery"), function() c(3, as.numeric(now(env) > 0))) %>%
  set_attribute("ame_death_time", draw_ame_death_time)
env %>% add_generator("p3_wait", probe, at(0, 1), mon = 2) %>% run() %>% invisible()
deaths <- get_mon_attributes(env) %>% filter(key == "ame_death_time")
report(is.na(deaths$value[deaths$name == "p3_wait0"]) &&
         !is.na(deaths$value[deaths$name == "p3_wait1"]),
       "awaiting airlift, an unoperated Priority 3 casualty faces no risk and an operated one the hazard alone")

disease <- c(priority = 1, dow_ceiling = dp$p1_p_max, dnbi_type = 2)
disease_probs <- check_probs(run_casualty(disease, function(trj) {
  trj %>% timeout(60) %>% check_at(arrival = TRUE) %>%
    episode(0, 1440, hold_bed_multiplier) %>% check_at()
}))
report(all(disease_probs == 0),
       "a disease casualty faces no risk at any check (%s)",
       paste(format(disease_probs), collapse = ", "))

# ── 5. A campaign forced onto every degraded route ──────────────────────────

cat(sprintf("\n-- a %d-day campaign with forward holding and one R2E ICU bed --\n", CAMPAIGN_DAYS))

degraded <- set_icu_establishment(resolve_scenario(json_data, "default"), 1)
env_data <- build_environment(degraded)
env_data$vars$r2b$post_op_icu$stability_window_dcs <- FORWARD_WINDOW
env_data$vars$r2b$post_op_icu$stability_window_single_stage <- FORWARD_WINDOW
counts <- sapply(env_data$elms, length)
set.seed(CAMPAIGN_SEED)
invisible(capture.output(mon <- run_replications(CAMPAIGN_REPS, CAMPAIGN_DAYS)))
attrs <- mon$attributes

routes <- attrs %>%
  filter((key == "r2b_post_op_pathway" & value == 2) |
           (key == "post_op_pathway" & value == 2) |
           (key == "post_definitive_pathway" & value == 2)) %>%
  distinct(key)
report(nrow(routes) == 3,
       "the campaign reaches all three holding-bed routes (%s)",
       paste(routes$key, collapse = ", "))

rises <- attrs %>%
  filter(key == "dow_ceiling") %>%
  arrange(replication, name, time) %>%
  group_by(replication, name) %>%
  summarise(rose = any(diff(value) > 1e-15, na.rm = TRUE), .groups = "drop")
report(nrow(rises) > 0 && !any(rises$rose),
       "no casualty's ceiling rises (%d casualties, %d with a rise)",
       nrow(rises), sum(rises$rose))

# Each banked hazard is checked against the multipliers it could imply: the
# exposure an update records, less the holding-bed charge for any wait, over
# the time in the bed, must be one of the configured multipliers.
rates <- shipped$vars$dow$post_op_hazard
hazard_rows <- attrs %>%
  filter(key %in% c("dow_hazard_s", "dow_episode_entry", "dow_episode_bed", "priority")) %>%
  arrange(replication, name, time)
#' The multipliers one casualty's banked hazards imply
#'
#' @param rows Attribute rows of one casualty, in time order.
#' @return Numeric vector of implied multipliers, one per episode.
implied <- function(rows) {
  prio <- rows$value[rows$key == "priority"][1]
  r <- c(rates$p1_rate, rates$p2_rate, rates$p3_rate)[prio] / DAY_MIN
  s <- rows[rows$key == "dow_hazard_s", ]
  out <- numeric(0)
  prev <- 1
  for (i in seq_len(nrow(s))) {
    if (s$value[i] == 1) {
      prev <- 1
      next
    }
    entry <- tail(rows$value[rows$key == "dow_episode_entry" & rows$time <= s$time[i]], 1)
    bed <- tail(rows$value[rows$key == "dow_episode_bed" & rows$time <= s$time[i]], 1)
    exposure <- -log(s$value[i] / prev) / r
    out <- c(out, (exposure - te$hold_bed_penalty * (bed - entry)) / (s$time[i] - bed))
    prev <- s$value[i]
  }
  out
}
multipliers <- unlist(lapply(split(hazard_rows, list(hazard_rows$replication, hazard_rows$name),
                                   drop = TRUE), implied))
multipliers <- multipliers[is.finite(multipliers)]
allowed <- c(1, te$r2b_icu_penalty, te$hold_bed_penalty)
off <- multipliers[vapply(multipliers, function(m) min(abs(m - allowed)) > 1e-6, logical(1))]
report(length(multipliers) > 0 && length(off) == 0,
       "every banked hazard implies a configured multiplier (%d episodes, %d otherwise)",
       length(multipliers), length(off))

# ── 6. The legacy setting reproduces the earlier model ──────────────────────

cat("\n-- the legacy setting reproduces the earlier seed-42 set --\n")

if (SKIP_REPRODUCTION) {
  cat("[SKIP] --skip-reproduction given\n")
} else {
  run_dir <- file.path(tempdir(), "dow_legacy")
  unlink(run_dir, recursive = TRUE)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  legacy <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
  for (i in seq_along(legacy$vars)) {
    if (!identical(legacy$vars[[i]]$elm, "dow")) next
    for (j in seq_along(legacy$vars[[i]]$actys)) {
      if (identical(legacy$vars[[i]]$actys[[j]]$acty, "accrual")) {
        legacy$vars[[i]]$actys[[j]]$vals[[1]]$val <- 0
      }
    }
  }
  config <- file.path(run_dir, "legacy.json")
  jsonlite::write_json(legacy, config, auto_unbox = TRUE, digits = NA, pretty = TRUE)
  status <- system2("Rscript", c("run.R", "--seed", LEGACY_SEED, "--days", LEGACY_DAYS,
                                 "--iterations", 1, "--config", shQuote(config),
                                 "--output-dir", shQuote(run_dir)),
                    stdout = file.path(run_dir, "console.txt"),
                    stderr = file.path(run_dir, "console.txt"))
  report(identical(as.integer(status), 0L), "the legacy run completed (exit status %s)", status)
  for (f in names(LEGACY_DIGESTS)) {
    path <- file.path(run_dir, f)
    got <- if (file.exists(path)) digest::digest(file = path, algo = "sha256") else "absent"
    report(identical(got, LEGACY_DIGESTS[[f]]), "%-34s %s", f,
           if (identical(got, LEGACY_DIGESTS[[f]])) "identical" else "differs")
  }
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(sprintf("  - %s\n", f))
  quit(status = 1)
}
cat("All died-of-wounds mechanism checks passed.\n")
quit(status = 0)
