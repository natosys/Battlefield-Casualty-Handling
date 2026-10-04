#!/usr/bin/env Rscript
##############################################################################
## scripts/check_convalescence_invariance.R                                 ##
## Regression check — convalescence follows the casualty, not the facility  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_convalescence_invariance.R
#   Rscript scripts/check_convalescence_invariance.R --quick   # shorter runs
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step.
#
# Why this check exists: time to fitness for duty is a property of a
# casualty's injury and treatment, not of the bed that happened to be free.
# The model once drew it from two unrelated distributions, a triangular 0.5 to
# 10 days at R2B holding and a 3 to 63 day base scaled by severity at R2E, so
# the same casualty needed about five days or about thirty depending on the
# route taken. The R2B holding evacuation threshold appeared to raise returns
# to duty by about 20% only because it moved casualties from the second
# distribution to the first. The convalescence is now drawn once, where
# severity and treatment are first known, and each echelon serves part of that
# one duration. This check asserts:
#   1. one draw: the duration a casualty carries equals the one drawn at R2B
#      holding, and a casualty evacuated part-way serves the same total across
#      the two echelons whichever the threshold,
#   2. routing invariance: casualties of one severity class draw the same
#      duration whether the draw was taken at R2B or at R2E,
#   3. severity and treatment response: the mean follows the configured
#      severity factors, and an operated Priority 1 casualty draws more than an
#      unoperated one,
#   4. eligibility: a casualty whose duration exceeds the evacuation policy is
#      never held forward, and one within it is not evacuated for duration.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
  library(tidyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")

args       <- commandArgs(trailingOnly = TRUE)
quick      <- "--quick" %in% args
#' Control seed of every arm
CHECK_SEED <- 42L

#' Run length in days, long enough that each severity class has a group to compare
CHECK_DAYS <- if (quick) 60L else 120L

#' Evacuation threshold of the threshold arm, in minutes
#'
#' @details One day, the value at which the retired two-distribution model
#'   showed its largest apparent gain.
THRESHOLD_ARM_MIN <- 1440

#' Smallest group a class mean is compared over
MIN_GROUP <- 30L

#' Tolerance on the ratio of two class means drawn at different echelons
#'
#' @details Both are means of draws from one distribution over a few dozen
#'   casualties, so the ratio is noisy; a facility-dependent distribution
#'   differs by a factor of several.
RATIO_TOLERANCE <- 0.3

failures <- character(0)

#' Record a failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated failures, invisibly; called for its side effect.
fail <- function(...) assign("failures", c(failures, sprintf(...)), envir = globalenv())

#' Print one PASS or FAIL line
#'
#' @param ok Logical: whether the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Values interpolated into `fmt`.
#' @return The printed line, invisibly; called for its side effect.
report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}

#' Assert one condition, recording a failure when it does not hold
#'
#' @param ok Logical: whether the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Values interpolated into `fmt`.
#' @return `ok`, invisibly.
assert <- function(ok, fmt, ...) {
  if (!isTRUE(ok)) fail(fmt, ...)
  report(isTRUE(ok), fmt, ...)
  invisible(ok)
}

env_data <- load_elms("env_data.json")
day_min  <- DAY_MIN
counts   <- sapply(env_data$elms, length)
env_data_base <- env_data

#' Attribute keys the check reads from each casualty
KEYS <- c("recovery_to_duty_days", "r2b_hold_drawn", "r2b_hold_served", "r2b_hold_evac",
          "r2b_hold_ineligible", "r2b_hold_bypass", "r2b_surgery", "r2e_surgery",
          "priority", "injury_type", "evacuation_reason", "return_day",
          "r2e_recovery_hold_start", "r2e_arrival_time")

#' Reshapes a monitor's long attribute log into one row per arrival
#'
#' @param attrs get_mon_attributes() output
#' @param keys Attribute keys to retain
#' @return Data frame with one row per named arrival and one column per key,
#'   carrying each arrival's last recorded value
per_arrival <- function(attrs, keys) {
  out <- attrs %>%
    filter(key %in% keys, name != "") %>%
    group_by(name, key) %>%
    summarise(value = dplyr::last(value), .groups = "drop") %>%
    pivot_wider(names_from = key, values_from = value)
  for (k in setdiff(keys, names(out))) out[[k]] <- NA_real_
  out
}

#' Runs one arm of the check and returns its per-casualty attributes
#'
#' @param threshold_min R2B holding evacuation threshold in minutes, or NULL
#'   for the shipped value.
#' @return Data frame from per_arrival(), with a `severity` class column.
run_arm <- function(threshold_min = NULL) {
  ed <- env_data_base
  if (!is.null(threshold_min)) ed$vars$r2b$holding$evac_threshold <- threshold_min
  assign("env_data", ed, envir = globalenv())
  invisible(capture.output(suppressWarnings(
    wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)
  )))
  d <- per_arrival(get_mon_attributes(wrapped), KEYS)
  had <- (!is.na(d$r2b_surgery) & d$r2b_surgery == 1) |
    (!is.na(d$r2e_surgery) & d$r2e_surgery == 1)
  d$operated <- had
  d$severity <- dplyr::case_when(
    !is.na(d$injury_type) & d$injury_type == 2 ~ "p3_dnbi",
    !is.na(d$priority) & d$priority == 3 ~ "p3_dnbi",
    !is.na(d$priority) & d$priority == 1 & had ~ "p1_surgical",
    !is.na(d$priority) & d$priority == 1 ~ "p1_nonsurgical",
    !is.na(d$priority) & d$priority == 2 ~ "p2",
    TRUE ~ "p3_dnbi"
  )
  d
}

cat(sprintf("\n-- Convalescence invariance (%d days, seed %d) --\n", CHECK_DAYS, CHECK_SEED))
shipped   <- run_arm()
threshold <- run_arm(THRESHOLD_ARM_MIN)
env_data  <- env_data_base

policy <- env_data_base$vars$r2eheavy$recovery$evacuation_policy_days
f      <- env_data_base$vars$r2eheavy$recovery_to_duty
hold   <- env_data_base$vars$r2eheavy$holding
base_mean <- (hold$min + hold$max + hold$mode) / 3 / DAY_MIN

##############################################################################
## 1. One draw                                                              ##
##############################################################################

for (arm in c("shipped", "threshold")) {
  d <- get(arm)
  held <- d %>% filter(!is.na(r2b_hold_drawn))
  assert(nrow(held) > 0, "%s arm: casualties entered R2B holding (%d)", arm, nrow(held))
  if (nrow(held)) {
    gap <- abs(held$recovery_to_duty_days * DAY_MIN - held$r2b_hold_drawn)
    assert(all(gap < 1e-6),
           paste("%s arm: the duration carried is the one drawn at R2B for all %d held",
                 "casualties (worst gap %.2e min)"),
           arm, nrow(held), max(gap))
  }
}

split <- threshold %>%
  filter(!is.na(r2b_hold_evac), r2b_hold_evac == 1, !is.na(return_day),
         !is.na(r2e_recovery_hold_start))
assert(nrow(split) > 0,
       paste("threshold arm: casualties served part of their convalescence at R2B and",
             "the rest at R2E (%d)"),
       nrow(split))
if (nrow(split)) {
  r2e_served <- split$return_day - split$r2e_recovery_hold_start
  total_gap  <- abs(split$r2b_hold_served + r2e_served - split$recovery_to_duty_days * DAY_MIN)
  assert(all(total_gap < 1e-3),
         paste("threshold arm: forward plus R2E bed time equals the one duration for all",
               "%d split casualties (worst gap %.2e min)"),
         nrow(split), max(total_gap))
}

##############################################################################
## 2. Routing invariance                                                    ##
##############################################################################

both <- bind_rows(shipped, threshold) %>% filter(!is.na(recovery_to_duty_days))
# Drawn at R2B when the casualty reached the holding decision; drawn at R2E
# otherwise (diverted past R2B, or operated).
at_r2b <- !is.na(both$r2b_hold_drawn) |
  (!is.na(both$r2b_hold_ineligible) & both$r2b_hold_ineligible == 1)
both$drawn_at <- ifelse(at_r2b, "R2B", "R2E")
compared <- 0L
for (sev in c("p1_nonsurgical", "p2", "p3_dnbi")) {
  g <- both %>% filter(severity == sev)
  a <- g$recovery_to_duty_days[g$drawn_at == "R2B"]
  b <- g$recovery_to_duty_days[g$drawn_at == "R2E"]
  if (length(a) < MIN_GROUP || length(b) < MIN_GROUP) {
    report(TRUE, "%s: too few casualties on one route to compare (R2B %d, R2E %d)", sev,
           length(a), length(b))
    next
  }
  compared <- compared + 1L
  ratio <- mean(a) / mean(b)
  assert(abs(ratio - 1) < RATIO_TOLERANCE,
         paste("%s: mean convalescence drawn at R2B (%.1f d, n=%d) matches R2E",
               "(%.1f d, n=%d), ratio %.2f"),
         sev, mean(a), length(a), mean(b), length(b), ratio)
}
assert(compared > 0, "at least one severity class was compared across routes (%d)", compared)

##############################################################################
## 3. Severity and treatment response                                       ##
##############################################################################

means <- both %>% group_by(severity) %>%
  summarise(n = n(), m = mean(recovery_to_duty_days), .groups = "drop")
#' Mean drawn convalescence of one severity class
#'
#' @param sev Severity class name.
#' @return The class's mean `recovery_to_duty_days`.
get_mean <- function(sev) means$m[means$severity == sev]
for (sev in c("p1_surgical", "p1_nonsurgical", "p2", "p3_dnbi")) {
  n <- means$n[means$severity == sev]
  if (!length(n) || n < MIN_GROUP) {
    report(TRUE, "%s: too few casualties to compare against its factor (%d)", sev,
           if (length(n)) n else 0L)
    next
  }
  # Draws above the evacuation policy are still drawn, so the mean is of the
  # whole distribution; the factor scales the base mean.
  expected <- base_mean * f[[sev]]
  assert(abs(get_mean(sev) / expected - 1) < 0.25,
         paste("%s: mean convalescence %.1f d is within 25%% of the base mean x factor",
               "%.2f (%.1f d), n=%d"),
         sev, get_mean(sev), f[[sev]], expected, n)
}
if (all(c("p1_surgical", "p1_nonsurgical") %in% means$severity)) {
  assert(get_mean("p1_surgical") > get_mean("p1_nonsurgical"),
         paste("treatment response: an operated Priority 1 casualty draws longer",
               "(%.1f d) than an unoperated one (%.1f d)"),
         get_mean("p1_surgical"), get_mean("p1_nonsurgical"))
}

##############################################################################
## 4. Eligibility for forward holding                                       ##
##############################################################################

ineligible <- both %>% filter(!is.na(r2b_hold_ineligible), r2b_hold_ineligible == 1)
assert(nrow(ineligible) > 0, "casualties were ruled out of forward holding by duration (%d)",
       nrow(ineligible))
assert(all(ineligible$recovery_to_duty_days > policy) && all(is.na(ineligible$r2b_hold_drawn)),
       paste("every ineligible casualty drew a duration beyond the %g-day policy and",
             "held no forward bed"), policy)
held_all <- both %>% filter(!is.na(r2b_hold_drawn))
assert(all(held_all$recovery_to_duty_days <= policy),
       "every casualty held forward drew a duration within the %g-day policy (%d held)",
       policy, nrow(held_all))

if (length(failures)) {
  cat(sprintf("\n%d check(s) failed:\n", length(failures)))
  for (m in failures) cat(" - ", m, "\n", sep = "")
  quit(status = 1)
}
cat("\nAll convalescence invariance checks passed.\n")
quit(status = 0)
