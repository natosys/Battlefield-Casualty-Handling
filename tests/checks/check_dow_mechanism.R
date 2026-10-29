#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_dow_mechanism.R                                       ##
## Regression check — the care-state died-of-wounds model's properties,   ##
## and the legacy setting's reproduction of the earlier model               ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_dow_mechanism.R
#   Rscript tests/checks/check_dow_mechanism.R --skip-reproduction   # omit the 360-day run
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The care-state model (`dow.care_state.enabled` = 1)
# replaced a logistic curve that credited care retroactively, ended risk within
# hours of injury, and raised the ceiling for degraded care above the untreated
# one. Each of those defects is silent in an ordinary run, since a model with
# any of them still reports a plausible death count, so each property the
# care-state model exists to provide is asserted here:
#
#   1. Delay before surgery raises the death probability, every hour waited
#      being charged at the multiplier in force during it.
#   2. For a Priority 1 casualty, recovery in intensive care carries less risk
#      than recovery in a holding bed, which carries less than no treatment at
#      all, at R2E and on the R2B forward stay.
#   3. A post-operative stay carries one multiplier, never a product of two:
#      an R2B forward holding-bed stay carries the holding-bed penalty alone.
#   4. A Priority 3 casualty is charged its own rate and nothing else, and is
#      at no risk awaiting airlift unless operated on; a disease casualty faces
#      no risk at any check.
#   5. Over a campaign forced onto every degraded route, every care state a
#      casualty takes carries one of the configured multipliers, and none after
#      surgery exceeds the holding-bed one.
#   6. The legacy setting (`dow.care_state.enabled` = 0) reproduces its
#      recorded seed-42 output byte for byte, so the earlier mortality
#      mechanism stays a supported configuration the care-state one can be
#      measured against.
#
# Assertions 1 to 4 drive the model's helpers through a single casualty in a
# minimal simmer environment, so their probabilities are exact rather than
# estimated. Assertion 6 compares digests rather than files, the tracked set
# being generated under the care-state model. LEGACY_DIGESTS are the legacy
# setting's output under the present casualty routing: a change that alters the
# model outside the died-of-wounds mechanism changes that output too, and is the
# one occasion for revising them, as the evacuation of every casualty from R1
# did.

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

#' SHA-256 of each file of the legacy setting's seed-42 output
LEGACY_DIGESTS <- c(
  "logs.txt"                         = "26c26610e5021834a61fa211c15199e67d1a0c33fa2b6681320d54a3fe885de6",
  "data/arrivals_casualty_surge.txt" = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
  "data/arrivals_dnbi_cbt.txt"       = "bec9a2cf9fa22c125f6bd94e99f5a1f2d8452dc68643f1add0842c5d92ba0612",
  "data/arrivals_dnbi_spt.txt"       = "aa8cbc039f721847d47473d113468dcde19b4e435a50aa043adfeb8d58c2f313",
  "data/arrivals_kia_cbt.txt"        = "5b0414d9b992fc7ce5ac01beb685bb81fb1f0f2e0ee84e2cdd5393c6e6d88af9",
  "data/arrivals_kia_spt.txt"        = "783bf1ac925c80b88ec214e1eee49341222a7438457a964e921236c2cf811e08",
  "data/arrivals_wia_cbt.txt"        = "4b9d58ad1bd122a02fec3c9f5a81c730318343cc77f59f7b22088c6d3ee8fda8",
  "data/arrivals_wia_spt.txt"        = "1cac8219cacd254449fba6eea019a4ffbcbeb17b1441d78c967459268467a482",
  "data/casualty_surge_events.csv"   = "92a3d3f38f6545948482b81f01d1006ec92be1affb48d5c55849e7a8d084db70",
  "data/seed42_verification.csv"     = "c05ed9bd8a447c1a9294ea01905a31c5dbb1abce4475578844982e17fad40e26"
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
mult <- shipped$vars$dow$multipliers
rates <- shipped$vars$dow$rates

# ── The single-casualty harness ─────────────────────────────────────────────

#' Run one casualty through a sequence of model steps
#'
#' @param attrs Named numeric of the casualty's starting attributes; priority,
#'   surgery and dnbi_type at least.
#' @param steps Function of a trajectory returning it with the steps appended,
#'   built under the care-state model.
#' @param seed Seed set before the run, so a death draw cannot vary between runs.
#' @return The attribute monitor of the run.
run_casualty <- function(attrs, steps, seed = 1L) {
  config <- shipped
  config$vars$dow$care_state$enabled <- 1
  assign("env_data", config, envir = globalenv())
  assign("env", simmer(), envir = globalenv())
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
#' @param cohort As for dow_check().
#' @return The trajectory, with the check appended.
check_at <- function(trj, cohort = TRUE) {
  dow_check(trj, cohort = cohort, died = trajectory(), survived = trajectory())
}

#' R1 treatment, as the casualty pathway applies it
#'
#' @param trj Trajectory to append to.
#' @return The trajectory, with the R1 treatment multiplier applied.
r1_treatment <- function(trj) {
  dow_treat(trj, function() mult$r1_treatment, function() 1, resuscitation = FALSE)
}

#' Resuscitation, as both R2 echelons apply it
#'
#' @param trj Trajectory to append to.
#' @return The trajectory, with the resuscitation multiplier applied.
resuscitation <- function(trj) {
  dow_treat(trj, function() mult$resuscitation, function() 1, resuscitation = TRUE)
}

#' A post-operative stay in a bed of one placement
#'
#' @param trj Trajectory to append to.
#' @param placement Zero-argument function returning the bed's multiplier.
#' @param stay Minutes in the bed.
#' @return The trajectory, with the stay appended.
stay_in <- function(trj, placement, stay) {
  trj %>% dow_placement(placement) %>% timeout(stay) %>% dow_placement(post_op_outside_icu)
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

p1 <- c(priority = 1, surgery = 1, dnbi_type = NA)

# ── 1. Delay before surgery ─────────────────────────────────────────────────

cat("-- delay before surgery raises the risk --\n")

#' A Priority 1 path operated on at a given time after injury
#'
#' @param surgery_at Minutes from injury to the end of the operation.
#' @return The path's total death probability.
operated_at <- function(surgery_at) {
  total_risk(run_casualty(p1, function(trj) {
    trj %>%
      timeout(20) %>% r1_treatment() %>% check_at(cohort = FALSE) %>%
      timeout(30) %>% check_at() %>%
      timeout(45) %>% resuscitation() %>%
      timeout(surgery_at - 95) %>% dow_operated(function() 1) %>%
      stay_in(post_op_r2e_icu, 1440) %>% check_at()
  }))
}
early_op <- operated_at(180)
late_op  <- operated_at(720)
report(late_op > early_op,
       "an operation ending 12 hours after injury carries more risk (%.5f) than one at 3 (%.5f)",
       late_op, early_op)

# ── 2. Intensive care < holding bed < no treatment ──────────────────────────

cat("\n-- intensive care, then a holding bed, then no treatment --\n")

#' A Priority 1 path through R2E with its recovery in a bed of one placement
#'
#' @param placement Zero-argument function returning the recovery bed's
#'   multiplier, or NULL for a casualty who is neither treated nor operated on.
#' @return The path's total death probability.
r2e_path <- function(placement) {
  treated <- !is.null(placement)
  total_risk(run_casualty(p1, function(trj) {
    trj <- trj %>% timeout(20) %>% check_at(cohort = FALSE) %>%
      timeout(30) %>% check_at() %>% timeout(45)
    if (treated) trj <- trj %>% resuscitation()
    trj <- trj %>% timeout(180)
    if (treated) trj <- trj %>% dow_operated(function() 1) %>% stay_in(placement, 1440)
    else trj <- trj %>% timeout(1440)
    trj <- trj %>% check_at() %>% timeout(120)
    if (treated) trj <- trj %>% stay_in(placement, 1440) else trj <- trj %>% timeout(1440)
    trj %>% check_at()
  }))
}
icu_r2e  <- r2e_path(post_op_r2e_icu)
hold_r2e <- r2e_path(post_op_outside_icu)
none     <- r2e_path(NULL)
report(icu_r2e < hold_r2e && hold_r2e < none,
       "at R2E: intensive care %.5f < holding bed %.5f < no treatment %.5f",
       icu_r2e, hold_r2e, none)

#' A Priority 1 path operated on at R2B and held forward in a bed of one placement
#'
#' @param placement Zero-argument function returning the forward bed's multiplier.
#' @return The path's total death probability.
r2b_path <- function(placement) {
  total_risk(run_casualty(p1, function(trj) {
    trj %>% timeout(20) %>% check_at(cohort = FALSE) %>%
      timeout(30) %>% check_at() %>%
      timeout(45) %>% resuscitation() %>%
      timeout(180) %>% dow_operated(function() 1) %>%
      stay_in(placement, FORWARD_WINDOW) %>%
      timeout(30) %>% check_at() %>%
      timeout(2880 - FORWARD_WINDOW) %>% check_at()
  }))
}
icu_r2b  <- r2b_path(post_op_r2b_icu)
hold_r2b <- r2b_path(post_op_outside_icu)
none_r2b <- total_risk(run_casualty(p1, function(trj) {
  trj %>% timeout(20) %>% check_at(cohort = FALSE) %>% timeout(30) %>% check_at() %>%
    timeout(225 + FORWARD_WINDOW + 30) %>% check_at() %>%
    timeout(2880 - FORWARD_WINDOW) %>% check_at()
}))
report(icu_r2b < hold_r2b && hold_r2b < none_r2b,
       "on the R2B forward stay: intensive care %.5f < holding bed %.5f < no treatment %.5f",
       icu_r2b, hold_r2b, none_r2b)

# ── 3. One multiplier per stay ──────────────────────────────────────────────

cat("\n-- one multiplier per post-operative stay --\n")

rate <- rates$p1_rate / DAY_MIN
#' The risk banked after surgery by one forward stay
#'
#' @param placement Zero-argument function returning the bed's multiplier.
#' @return The `dow_risk_late` the stay leaves.
banked <- function(placement) {
  mon <- run_casualty(p1, function(trj) {
    trj %>% dow_operated(function() 1) %>% stay_in(placement, FORWARD_WINDOW)
  })
  tail(mon$value[mon$key == "dow_risk_late"], 1)
}
hold_bank <- banked(post_op_outside_icu)
report(isTRUE(all.equal(hold_bank, rate * mult$post_op * mult$hold_bed_penalty * FORWARD_WINDOW)),
       "a forward holding-bed stay carries the holding-bed penalty alone (%.2f), not %.4f",
       mult$hold_bed_penalty, mult$hold_bed_penalty * mult$r2b_icu_penalty)
icu_bank <- banked(post_op_r2b_icu)
report(isTRUE(all.equal(icu_bank, rate * mult$post_op * mult$r2b_icu_penalty * FORWARD_WINDOW)),
       "a forward intensive care stay carries the R2B intensive care penalty alone (%.2f)",
       mult$r2b_icu_penalty)

# ── 4. Priority 3 at its own rate; disease never ────────────────────────────

cat("\n-- Priority 3 is charged its rate alone; disease faces no risk --\n")

p3 <- c(priority = 3, surgery = 0, dnbi_type = NA)
p3_probs <- check_probs(run_casualty(p3, function(trj) {
  trj %>% timeout(60) %>% check_at() %>% resuscitation() %>%
    timeout(600) %>% check_at()
}))
expected_p3 <- 1 - exp(-rates$p3_rate / DAY_MIN * 60)
report(isTRUE(all.equal(p3_probs, c(expected_p3, 0))),
       "an unoperated Priority 3 casualty is charged %.6f by its first check and nothing after resuscitation",
       expected_p3)

env_data <- shipped
env_data$vars$dow$care_state$enabled <- 1
env <- simmer()
probe <- trajectory() %>%
  set_attribute(c("priority", "r2e_surgery"), function() c(3, as.numeric(now(env) > 0))) %>%
  set_attribute("ame_death_time", draw_ame_death_time)
env %>% add_generator("p3_wait", probe, at(0, 1), mon = 2) %>% run() %>% invisible()
deaths <- get_mon_attributes(env) %>% filter(key == "ame_death_time")
report(is.na(deaths$value[deaths$name == "p3_wait0"]) &&
         !is.na(deaths$value[deaths$name == "p3_wait1"]),
       "awaiting airlift, an unoperated Priority 3 casualty faces no risk and an operated one does")

disease <- c(priority = 1, surgery = 1, dnbi_type = 2)
disease_probs <- check_probs(run_casualty(disease, function(trj) {
  trj %>% timeout(60) %>% check_at() %>% dow_operated(function() 1) %>%
    stay_in(post_op_outside_icu, 1440) %>% check_at()
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

allowed <- c(0, 1, mult$r1_treatment, mult$r1_treatment * mult$resuscitation,
             mult$post_op, mult$post_op * mult$r2b_icu_penalty,
             mult$post_op * mult$hold_bed_penalty)
taken <- unique(attrs$value[attrs$key == "dow_mult"])
off <- taken[vapply(taken, function(m) min(abs(m - allowed)) > 1e-12, logical(1))]
report(length(taken) > 0 && length(off) == 0,
       "every care state taken carries a configured multiplier (%d distinct, %d otherwise)",
       length(taken), length(off))
after <- attrs %>%
  filter(key %in% c("dow_mult", "dow_phase")) %>%
  group_by(replication, name, time) %>%
  filter(any(key == "dow_phase" & value == 1)) %>%
  ungroup() %>%
  filter(key == "dow_mult")
report(nrow(after) > 0 && all(after$value <= mult$post_op * mult$hold_bed_penalty + 1e-12),
       "no state after surgery exceeds the holding-bed multiplier (%d post-operative states)",
       nrow(after))

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
      if (identical(legacy$vars[[i]]$actys[[j]]$acty, "care_state")) {
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
cat("All died-of-wounds model checks passed.\n")
quit(status = 0)
