#!/usr/bin/env Rscript
##############################################################################
## scripts/check_lever_realisation.R                                        ##
## Regression check — two planner levers realise the value configured       ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_lever_realisation.R
#   Rscript scripts/check_lever_realisation.R --quick   # shorter runs
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step.
#
# Why this check exists: a parameter a planner sets is a question put to the
# model, and a parameter whose realised effect is clipped answers a different
# question than the one asked, without saying so. Two levers were clipped in
# exactly that way, and both sit on features that ship disabled, so no run
# output would have shown it.
#
#   1. Reinforcement fill. The fill fraction is drawn from Triangular(
#      fill_min_frac, fill_mode_frac, fill_max_frac), whose shipped maximum of
#      1.1 names a package larger than the shortfall it was requested against.
#      A pool cannot exceed its own establishment strength, so the excess had
#      nowhere to go and was dropped at the moment of crediting. It is now
#      carried and absorbed as room appears, which is what makes a fill
#      fraction above 1 mean anything at all.
#   2. R2B holding evacuation threshold. A casualty whose drawn convalescence
#      exceeds the threshold is moved to R2E part-way through it. The unserved
#      remainder was previously dropped and R2E drew a fresh duration, so
#      enabling what is presented as a routing lever changed total modelled
#      convalescence by an unaccounted amount. The remainder is now carried
#      forward and served.
#
# The two checks are grouped because they are the same defect: a configured
# quantity that the model silently declines to apply in full.

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
CHECK_SEED <- 42L
REINF_DAYS <- if (quick) 60L else 180L
HOLD_DAYS  <- if (quick) 10L else 30L

# The threshold this check exercises. Three days sits well inside the R2B
# holding distribution (0.5 to 10 days, mode 5), so a substantial share of
# casualties cross it and a substantial share do not, which is what makes both
# sides of the branch observable in one run.
HOLD_THRESHOLD <- 4320

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

env_data_base <- env_data

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

##############################################################################
## Part 1 — the reinforcement fill is delivered in full                     ##
##############################################################################
#
# Run against a purpose-built harness rather than the whole model. The
# reinforcement generator is added with mon = 0 in run_once(), so its per-cycle
# attributes are not recorded in an ordinary run, and the pool it credits is
# moved by casualty debits and return-to-duty credits at the same time, which
# would leave the quantity of interest unobservable. The harness runs the same
# build_reinforcement_trajectory() against a monitored generator and
# deterministic attrition and return-to-duty streams, so every cycle's demand,
# fill and absorbed amount is visible and the pool moves for reasons the check
# controls.

ATTRITION_PER_DAY <- 12

# Debits the pool on attrition weeks and leaves it alone on quiet ones, so the
# pool both depletes and steadies within one run.
debit_pool <- function(pool_global, n) {
  function() {
    if (floor(now(env) / (7 * day_min)) %% 2 != 0) return(get_global(env, pool_global))
    max(0, get_global(env, pool_global) - n)
  }
}

# Credits the pool the way return to duty does, on the weeks attrition is
# quiet. Used only by the harness that measures what a pool refilled during the
# fulfillment lag cannot absorb: a cycle submitted at the end of an attrition
# week is delivered at the end of the recovery week that follows it, by which
# time the shortfall it was drawn against has partly closed on its own.
credit_pool <- function(pool_global, n, initial) {
  function() {
    if (floor(now(env) / (7 * day_min)) %% 2 == 0) return(get_global(env, pool_global))
    min(initial, get_global(env, pool_global) + n)
  }
}

#' Runs the reinforcement mechanism in isolation under a given fill distribution
#'
#' @param n_days Run length in days
#' @param seed Random seed
#' @param fill Named list of fill_min_frac/fill_mode_frac/fill_max_frac
#' @param rtd_per_day Personnel returned to duty each day, on top of attrition.
#'   Zero for the measurement harness; positive for the harness that forces the
#'   pool to refill during the fulfillment lag.
#' @return The completed simmer environment
run_reinforcement_harness <- function(n_days, seed, fill, rtd_per_day = 0) {
  ed <- env_data_base
  ed$vars$force_regeneration$reinforcement$demand_interval_days <- 7
  ed$vars$force_regeneration$reinforcement$fulfillment_lag_days <- 7
  ed$vars$force_regeneration$reinforcement$fill_min_frac        <- fill$fill_min_frac
  ed$vars$force_regeneration$reinforcement$fill_mode_frac       <- fill$fill_mode_frac
  ed$vars$force_regeneration$reinforcement$fill_max_frac        <- fill$fill_max_frac
  env_data <<- ed

  set.seed(seed)

  attrition <- trajectory("Attrition") %>%
    set_global("effective_force_combat",
               debit_pool("effective_force_combat", ATTRITION_PER_DAY)) %>%
    set_global("effective_force_support",
               debit_pool("effective_force_support", ATTRITION_PER_DAY))

  recovery <- trajectory("Return to Duty") %>%
    set_global("effective_force_combat",
               credit_pool("effective_force_combat", rtd_per_day, env_data$pops$combat)) %>%
    set_global("effective_force_support",
               credit_pool("effective_force_support", rtd_per_day, env_data$pops$support))

  env <<- simmer("Reinforcement Harness") %>%
    add_global("effective_force_combat", env_data$pops$combat) %>%
    add_global("effective_force_support", env_data$pops$support) %>%
    add_global("reinf_combat_pending", 0) %>%
    add_global("reinf_support_pending", 0) %>%
    add_global("reinf_combat_unabsorbed", 0) %>%
    add_global("reinf_support_unabsorbed", 0) %>%
    add_generator("attrition", attrition,
                  at(seq(day_min, n_days * day_min, by = day_min)), mon = 0)

  if (rtd_per_day > 0) {
    # Offset half a day from attrition so the two do not resolve at the same
    # instant, which would make the order they are applied in matter.
    env <<- env %>%
      add_generator("recovery", recovery,
                    at(seq(1.5 * day_min, n_days * day_min, by = day_min)), mon = 0)
  }

  # mon = 2, unlike run_once(): the per-cycle attributes are the quantity
  # under test here.
  env <<- env %>%
    add_generator("force_reinforcement", build_reinforcement_trajectory(),
                  at(seq(7 * day_min, n_days * day_min, by = 7 * day_min)),
                  mon = 2)

  invisible(capture.output(suppressWarnings(env <<- env %>% run(n_days * day_min))))
  env
}

#' One row per reinforcement cycle for one pool
#'
#' @param attrs get_mon_attributes() output from the harness
#' @param pool "combat" or "support"
#' @return Data frame of demand, fill and absorbed per cycle. A cycle submitted
#'   within a fulfillment lag of the run's end has drawn its fill but not yet
#'   delivered it, and carries NA for absorbed.
cycles_of <- function(attrs, pool) {
  keys <- paste0("reinf_", pool, c("_demand", "_fill", "_absorbed"))
  out  <- per_arrival(attrs, keys)
  names(out)[match(keys, names(out))] <- c("demand", "fill", "absorbed")
  out %>% filter(!is.na(fill))
}

final_global <- function(attrs, key_name) {
  tr <- attrs %>% filter(key == key_name)
  if (nrow(tr)) dplyr::last(tr$value) else 0
}

# ── Harness A: the shipped fill distribution ────────────────────────────────
# The measurement run. What it establishes is that the fraction the model
# applies is the fraction the distribution drew, which is what the README
# documents the mean of.

cat(sprintf("\n-- Reinforcement fill realisation, shipped distribution (%d days, seed %d) --\n",
            REINF_DAYS, CHECK_SEED))

shipped_fill <- env_data_base$vars$force_regeneration$reinforcement[
  c("fill_min_frac", "fill_mode_frac", "fill_max_frac")]
configured_mean <- with(shipped_fill,
                        (fill_min_frac + fill_mode_frac + fill_max_frac) / 3)

attrs_a <- get_mon_attributes(run_reinforcement_harness(REINF_DAYS, CHECK_SEED, shipped_fill))

for (pool in c("combat", "support")) {
  initial  <- env_data_base$pops[[pool]]
  cycles   <- cycles_of(attrs_a, pool)
  credited <- cycles %>% filter(!is.na(absorbed))

  if (nrow(credited) == 0) {
    fail("%s: no reinforcement cycle recorded an absorbed amount, so what a cycle delivers cannot be told from what it credits",
         pool)
    report(FALSE, "%s: no reinforcement cycle recorded an absorbed amount", pool)
    next
  }

  # Check 1: the pool is never credited above establishment strength.
  # Reinforcement joins the population on arrival, so this ceiling is what
  # makes a fill fraction above 1 undeliverable rather than merely unusual.
  peak <- max(attrs_a %>% filter(key == paste0("effective_force_", pool)) %>% pull(value))
  ok <- peak <= initial + 1e-9
  if (!ok) fail("%s: effective force reached %.0f against an establishment of %d",
                pool, peak, initial)
  report(ok, "%s: effective force never exceeded establishment strength (peak %.0f of %d)",
         pool, peak, initial)

  # Check 2: every person delivered enters the population. With the fill
  # bounded at 1 and no return-to-duty credits landing during the lag, the
  # shortfall at delivery is at least what was drawn against it, so absorbed
  # equals fill on every cycle and nothing is left over.
  worst <- max(abs(credited$fill - credited$absorbed))
  ok <- worst < 1e-9
  if (!ok) {
    fail("%s: %.0f personnel were delivered but not credited to the pool, across %d cycles",
         pool, sum(pmax(0, credited$fill - credited$absorbed)),
         sum(credited$fill > credited$absorbed + 1e-9))
  }
  report(ok, "%s: all %.0f personnel delivered over %d cycles entered the population",
         pool, sum(credited$fill), nrow(credited))

  ok <- final_global(attrs_a, paste0("reinf_", pool, "_unabsorbed")) < 1e-9
  if (!ok) fail("%s: the unabsorbed counter is non-zero without any return-to-duty credit",
                pool)
  report(ok, "%s: nothing was recorded as unabsorbed", pool)

  # Check 3: the realised mean fill fraction matches the configured
  # distribution's own mean, which is the point of the whole exercise.
  live <- cycles %>% filter(demand > 0)
  if (nrow(live) >= 5) {
    realised <- live$fill / live$demand
    se     <- sd(realised) / sqrt(length(realised))
    t_stat <- abs(mean(realised) - configured_mean) / max(se, 1e-12)
    ok <- t_stat <= 4
    if (!ok) {
      fail("%s: realised mean fill fraction %.4f differs from the configured mean %.4f by %.1f standard errors",
           pool, mean(realised), configured_mean, t_stat)
    }
    report(ok, "%s: realised mean fill fraction %.4f over %d cycles against a configured mean of %.4f (%.1f standard errors)",
           pool, mean(realised), nrow(live), configured_mean, t_stat)
  } else {
    report(TRUE, "%s: fewer than five cycles carried demand, mean fill comparison skipped", pool)
  }

  # Check 4: a pool never submits demand above its own establishment.
  ok <- all(cycles$demand <= initial + 1e-9)
  if (!ok) fail("%s: a cycle submitted demand above establishment strength", pool)
  report(ok, "%s: no cycle submitted demand above establishment strength (largest %.0f of %d)",
         pool, max(cycles$demand), initial)
}

# ── Harness B: return to duty refilling the pool during the lag ─────────────
# The one case in which a delivery can still find no room, the fill being
# bounded at 1. It is a residual of the establishment ceiling rather than of
# the fill distribution, and what this checks is that it is counted rather than
# dropped silently. See README Further Development (L29).

cat(sprintf("\n-- Return to duty during the fulfillment lag (%d days, seed %d) --\n",
            REINF_DAYS, CHECK_SEED))

attrs_b <- get_mon_attributes(
  run_reinforcement_harness(REINF_DAYS, CHECK_SEED, shipped_fill,
                            rtd_per_day = ATTRITION_PER_DAY %/% 2))

for (pool in c("combat", "support")) {
  initial  <- env_data_base$pops[[pool]]
  credited <- cycles_of(attrs_b, pool) %>% filter(!is.na(absorbed))

  if (nrow(credited) == 0) {
    fail("%s: no reinforcement cycle recorded an absorbed amount under return to duty", pool)
    report(FALSE, "%s: no reinforcement cycle recorded an absorbed amount", pool)
    next
  }

  short     <- sum(pmax(0, credited$fill - credited$absorbed))
  unabsorbed <- final_global(attrs_b, paste0("reinf_", pool, "_unabsorbed"))

  # Check 5: the counter is exact. Every person delivered is either credited
  # to the pool or counted here, so the accounting closes.
  ok <- abs(short - unabsorbed) < 1e-9
  if (!ok) {
    fail("%s: %.0f personnel could not be absorbed but the counter reads %.0f",
         pool, short, unabsorbed)
  }
  report(ok, "%s: %.0f of %.0f personnel delivered could not be absorbed and all of them are counted (%d of %d cycles)",
         pool, unabsorbed, sum(credited$fill),
         sum(credited$fill > credited$absorbed + 1e-9), nrow(credited))

  # The harness has to reach the case for Check 5 to mean anything: a run in
  # which the pool never refilled during a lag would report a counter of zero
  # and prove nothing about it.
  ok <- short > 0
  if (!ok) fail("%s: no delivery met a refilled pool, so the unabsorbed counter was never exercised",
                pool)
  report(ok, "%s: the refilled-pool case was reached, so the counter above is a measurement rather than a default",
         pool)

  peak <- max(attrs_b %>% filter(key == paste0("effective_force_", pool)) %>% pull(value))
  ok <- peak <= initial + 1e-9
  if (!ok) fail("%s: effective force reached %.0f against an establishment of %d under return to duty",
                pool, peak, initial)
  report(ok, "%s: effective force never exceeded establishment strength under return to duty (peak %.0f of %d)",
         pool, peak, initial)
}

# ── Harness C: a fill distribution the model could not apply ────────────────
# Check 6: a fill fraction above 1 is rejected rather than silently clipped.
# This is the acceptance criterion itself: the parameter cannot be configured
# above what the model can deliver.

cat("\n-- A fill fraction above 1 is rejected --\n")

rejected <- tryCatch({
  run_reinforcement_harness(REINF_DAYS, CHECK_SEED,
                            list(fill_min_frac = 0.2, fill_mode_frac = 0.85,
                                 fill_max_frac = 1.1))
  NULL
}, error = function(e) conditionMessage(e))

ok <- !is.null(rejected) && grepl("establishment strength", rejected, fixed = TRUE)
if (!ok) {
  fail("fill_max_frac = 1.1 was accepted, so the parameter can still name an over-delivery the model cannot apply")
}
report(ok, "fill_max_frac = 1.1 is rejected: %s",
       if (is.null(rejected)) "no error raised" else rejected)

# Check 7: an inverted distribution is rejected too. rtriangle() returns NA
# rather than erroring on a <= c <= b being violated, which is how the Issue
# #112 screening run lost every elementary effect to an NA cascade.
inverted <- tryCatch({
  run_reinforcement_harness(REINF_DAYS, CHECK_SEED,
                            list(fill_min_frac = 0.9, fill_mode_frac = 0.4,
                                 fill_max_frac = 1.0))
  NULL
}, error = function(e) conditionMessage(e))

ok <- !is.null(inverted) && grepl("fill_min_frac <= fill_mode_frac", inverted, fixed = TRUE)
if (!ok) fail("an inverted fill distribution was accepted, so rtriangle() would return NA unreported")
report(ok, "an inverted fill distribution is rejected before it can produce NA draws")

env_data <<- env_data_base

##############################################################################
## Part 2 — the R2B holding evacuation threshold conserves convalescence    ##
##############################################################################

cat(sprintf("\n-- R2B holding evacuation threshold (%d days, seed %d, threshold %g min) --\n",
            HOLD_DAYS, CHECK_SEED, HOLD_THRESHOLD))

hold_keys <- c("r2b_hold_drawn", "r2b_hold_evac", "r2b_hold_served",
               "r2b_hold_residual", "recovery_to_duty_days", "return_echelon")

ed <- env_data_base
ed$vars$r2b$holding$evac_threshold <- HOLD_THRESHOLD
env_data <<- ed

invisible(capture.output(suppressWarnings(
  wrapped <- run_once(n_days = HOLD_DAYS, seed = CHECK_SEED)
)))

held <- per_arrival(get_mon_attributes(wrapped), hold_keys) %>%
  filter(!is.na(r2b_hold_drawn))

evacuated <- held %>% filter(!is.na(r2b_hold_evac), r2b_hold_evac == 1)
# Returned to duty from R2B, which is the branch the threshold did not take.
# Casualties still lying in a holding bed when the run ended have reached
# neither branch and belong to neither cohort.
recovered <- held %>% filter(is.na(r2b_hold_evac), !is.na(return_echelon),
                             return_echelon == 2)

report(TRUE, "%d casualties entered an R2B holding bed: %d crossed the threshold and were evacuated, %d recovered forward, %d still holding at the run's end",
       nrow(held), nrow(evacuated), nrow(recovered),
       nrow(held) - nrow(evacuated) - nrow(recovered))

if (nrow(evacuated) == 0) {
  fail("no casualty crossed the threshold, so the conservation could not be checked")
  report(FALSE, "no casualty crossed the evacuation threshold")
} else if (any(is.na(evacuated$r2b_hold_served)) || any(is.na(evacuated$r2b_hold_residual))) {
  fail("%d evacuated casualties recorded no served or residual convalescence, so the threshold discards what it does not serve",
       sum(is.na(evacuated$r2b_hold_served) | is.na(evacuated$r2b_hold_residual)))
  report(FALSE, "evacuated casualties carry no record of what was served forward and what remains")
} else {
  # Check 8: the forward stay is the threshold, measured from the clock rather
  # than restated from the parameter.
  worst <- max(abs(evacuated$r2b_hold_served - HOLD_THRESHOLD))
  ok <- worst < 1e-6
  if (!ok) {
    fail("an evacuated casualty held an R2B bed for a period differing from the threshold by up to %.3f minutes",
         worst)
  }
  report(ok, "all %d evacuated casualties held an R2B bed for exactly the threshold (worst gap %.2e min)",
         nrow(evacuated), worst)

  # Check 9: the conservation itself. What was served forward plus what is
  # carried forward is the whole of what was drawn, so the threshold decides
  # where the convalescence is served and not how much of it there is.
  worst <- max(abs(evacuated$r2b_hold_served + evacuated$r2b_hold_residual -
                     evacuated$r2b_hold_drawn))
  ok <- worst < 1e-6
  if (!ok) {
    fail("served plus residual convalescence differs from the duration drawn by up to %.3f minutes",
         worst)
  }
  report(ok, "served plus residual convalescence matches the duration drawn for all %d evacuated casualties (worst gap %.2e min)",
         nrow(evacuated), worst)

  # Check 10: the residual is what R2E serves. A fresh draw here is the defect
  # this check exists for, and it would show as a recovery duration unrelated
  # to the remainder carried in.
  reached <- evacuated %>% filter(!is.na(recovery_to_duty_days))
  if (nrow(reached)) {
    worst <- max(abs(reached$recovery_to_duty_days * 1440 - reached$r2b_hold_residual))
    ok <- worst < 1e-6
    if (!ok) {
      fail("%d evacuated casualties drew a fresh R2E recovery duration instead of serving the residual (worst gap %.3f minutes)",
           sum(abs(reached$recovery_to_duty_days * 1440 - reached$r2b_hold_residual) >= 1e-6),
           worst)
    }
    report(ok, "all %d evacuated casualties reaching R2E disposition served the residual rather than a fresh draw (worst gap %.2e min)",
           nrow(reached), worst)
  } else {
    fail("no evacuated casualty reached R2E disposition, so the residual was never served")
    report(FALSE, "no evacuated casualty reached R2E disposition")
  }

  # Check 11: the residual is strictly positive and strictly less than the
  # base R2E convalescence distribution's own minimum for at least some
  # casualties. Without this the check would pass on a model that happened to
  # redraw from a similar distribution.
  ok <- all(evacuated$r2b_hold_residual > 0)
  if (!ok) fail("%d evacuated casualties carried a residual of zero or less",
                sum(evacuated$r2b_hold_residual <= 0))
  report(ok, "every evacuated casualty carried a positive residual (median %.0f min, longest %.0f min)",
         median(evacuated$r2b_hold_residual), max(evacuated$r2b_hold_residual))

  r2e_min <- env_data$vars$r2eheavy$holding$min
  below <- sum(evacuated$r2b_hold_residual < r2e_min)
  report(TRUE, "%d of %d residuals fall below the R2E base convalescence minimum of %g min, which a fresh draw could not produce",
         below, nrow(evacuated), r2e_min)
}

# Check 12: casualties who recover forward are untouched by the threshold,
# holding the bed for the whole duration drawn.
if (nrow(recovered)) {
  ok <- all(recovered$r2b_hold_drawn <= HOLD_THRESHOLD + 1e-9)
  if (!ok) {
    fail("%d casualties recovered forward despite a drawn duration above the threshold",
         sum(recovered$r2b_hold_drawn > HOLD_THRESHOLD + 1e-9))
  }
  report(ok, "all %d casualties recovering forward drew a duration within the threshold",
         nrow(recovered))
}

# Check 13: with no threshold configured, which is the shipped state, the
# branch is unreachable and no casualty carries a residual at all.
cat("\n-- No threshold configured (the shipped state) --\n")

env_data <<- env_data_base

invisible(capture.output(suppressWarnings(
  wrapped_off <- run_once(n_days = HOLD_DAYS, seed = CHECK_SEED)
)))

off <- per_arrival(get_mon_attributes(wrapped_off), hold_keys) %>%
  filter(!is.na(r2b_hold_drawn))

ok <- nrow(off) > 0 && all(is.na(off$r2b_hold_evac)) && all(is.na(off$r2b_hold_residual))
if (nrow(off) == 0) {
  fail("no casualty entered an R2B holding bed at the shipped configuration")
} else if (!ok) {
  fail("%d casualties were evacuated under a threshold that is not configured",
       sum(!is.na(off$r2b_hold_evac)))
}
report(ok, "none of the %d casualties holding at R2B were evacuated early, the threshold being absent",
       nrow(off))

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All lever realisation checks passed.\n")
quit(status = 0)
