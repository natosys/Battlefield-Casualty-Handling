#!/usr/bin/env Rscript
##############################################################################
## scripts/check_icu_time_conservation.R                                    ##
## Regression check — post-operative ICU time is conserved across routes    ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_icu_time_conservation.R
#   Rscript scripts/check_icu_time_conservation.R --quick   # 10 days, fewer shares
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step.
#
# Why this check exists: a casualty's post-operative ICU requirement follows
# from the injury, so the total should not depend on which mix of echelons
# delivers it. The model once failed this badly and silently — R2B provided
# no post-operative ICU at all, while R2E separately shortened its own
# episode for the very casualties R2B had operated on, so an R2B-operated
# casualty received about 28% of the ICU time an otherwise identical
# R2E-operated one did. Nothing in the run output said so: both routes
# produced plausible casualty counts, plausible bed utilisation and plausible
# mortality. The invariant is now structural — one requirement is drawn and
# split by `r2b.post_op_icu.share` — and this check confirms the structure
# holds at every share, across all three routes a surgical casualty can take.

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
CHECK_DAYS <- if (quick) 10L else 30L
CHECK_SEED <- 42L
SHARES     <- if (quick) c(0, 1) else c(0, 0.25, 0.5, 1)

# Triangular draws mean two routes' sample means never match exactly. The
# tolerance is on the relative difference between route means, generous
# enough to absorb ordinary sampling noise at a 30-day cohort size and tight
# enough that the 72% shortfall this check was written for could not pass.
ROUTE_TOLERANCE <- 0.15

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

#' One casualty per row, with the post-operative minutes each echelon served
#'
#' @param attrs get_mon_attributes() output for a completed run
#' @return Data frame: name, total (the drawn requirement), r2b and r2e (the
#'   minutes each echelon served, zero where it served none), r2b_surgery and
#'   r2b_bypassed route markers
per_casualty <- function(attrs) {
  wanted <- c("post_op_icu_total", "r2b_post_op_min", "r2e_post_op_min",
              "r2b_surgery", "r2b_bypassed", "post_op_pathway", "dow")

  attrs %>%
    filter(key %in% wanted) %>%
    group_by(name, key) %>%
    summarise(value = dplyr::last(value), .groups = "drop") %>%
    pivot_wider(names_from = key, values_from = value) %>%
    # A casualty who never entered a given step has no row for its attribute;
    # absent means zero minutes served there, not an unknown quantity.
    mutate(across(any_of(wanted), ~ ifelse(is.na(.x), 0, .x))) %>%
    filter(post_op_icu_total > 0) %>%
    transmute(
      name,
      total        = post_op_icu_total,
      r2b          = if ("r2b_post_op_min" %in% names(.)) r2b_post_op_min else 0,
      r2e          = if ("r2e_post_op_min" %in% names(.)) r2e_post_op_min else 0,
      r2b_surgery  = if ("r2b_surgery" %in% names(.)) r2b_surgery else 0,
      r2b_bypassed = if ("r2b_bypassed" %in% names(.)) r2b_bypassed else 0,
      pathway      = if ("post_op_pathway" %in% names(.)) post_op_pathway else 0,
      dow          = if ("dow" %in% names(.)) dow else 0
    )
}

route_of <- function(df) {
  ifelse(df$r2b_surgery == 1, "operated at R2B",
         ifelse(df$r2b_bypassed == 1, "bypassed R2B for want of a theatre",
                "reached R2E without an R2B surgical decision"))
}

results <- list()

for (share in SHARES) {
  cat(sprintf("\n-- Forward ICU share = %.2f (%d days, seed %d) --\n",
              share, CHECK_DAYS, CHECK_SEED))

  ed <- env_data_base
  ed$vars$r2b$post_op_icu$share <- share
  env_data <<- ed

  invisible(capture.output(suppressWarnings(
    wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)
  )))

  cas <- per_casualty(get_mon_attributes(wrapped))

  if (nrow(cas) == 0) {
    fail("share %.2f: no casualty reached post-operative ICU recovery, so nothing could be checked",
         share)
    report(FALSE, "share %.2f: no post-operative casualties in the run", share)
    next
  }

  cas$route <- route_of(cas)
  cas$served <- cas$r2b + cas$r2e

  # Check 1: served == drawn, per casualty. This is the invariant itself, and
  # it binds on casualties who completed the nominal post-operative pathway
  # (post_op_pathway == 1: an R2E ICU bed was available to serve whatever the
  # forward leg left). Two populations are excluded and counted instead:
  #
  #  - Casualties whose journey ended between the echelons: they died at the
  #    R2E arrival check, or the run ended while they were still in transit.
  #    They served the forward leg and never reached the rear one. Their
  #    shortfall is the model working, not the invariant breaking.
  #  - Casualties routed to the R2E post-operative holding bed because ICU
  #    was saturated (post_op_pathway == 2). That pathway substitutes a
  #    shorter holding-bed stay for the ICU episode by design and predates
  #    the forward share, so the requirement is genuinely not conserved for
  #    them at an intermediate share. See README Further Development (L20).
  nominal  <- cas %>% filter(pathway == 1)
  degraded <- cas %>% filter(pathway == 2, r2b > 0, r2b < total)
  unfinished <- cas %>% filter(pathway == 0)

  worst <- if (nrow(nominal)) max(abs(nominal$served - nominal$total)) else 0
  ok <- worst < 1e-6
  if (!ok) {
    fail("share %.2f: post-operative minutes served differ from the requirement drawn by up to %.3f minutes",
         share, worst)
  }
  report(ok, "share %.2f: %d casualties completed the nominal pathway, served minutes match the drawn requirement (worst gap %.2e min)",
         share, nrow(nominal), worst)
  cat(sprintf("       (%d ended between echelons, %d took the saturated-ICU holding pathway)\n",
              nrow(unfinished), nrow(degraded)))

  completed <- nominal

  # Check 2: route means agree. Conservation per casualty would still permit
  # the routes to draw from systematically different requirements, which is
  # the shape the original defect took.
  by_route <- completed %>%
    group_by(route) %>%
    summarise(n = n(), mean_total = mean(total), mean_r2b = mean(r2b),
              mean_r2e = mean(r2e), .groups = "drop")

  print(as.data.frame(by_route), row.names = FALSE)

  comparable <- by_route %>% filter(n >= 5)
  if (nrow(comparable) >= 2) {
    spread <- (max(comparable$mean_total) - min(comparable$mean_total)) /
      mean(comparable$mean_total)
    ok <- spread <= ROUTE_TOLERANCE
    if (!ok) {
      fail("share %.2f: mean post-operative requirement differs by %.1f%% across routes (tolerance %.0f%%)",
           share, 100 * spread, 100 * ROUTE_TOLERANCE)
    }
    report(ok, "share %.2f: route means agree within %.1f%% (tolerance %.0f%%)",
           share, 100 * spread, 100 * ROUTE_TOLERANCE)
  } else {
    report(TRUE, "share %.2f: fewer than two routes carried 5+ casualties, route comparison skipped",
           share)
  }

  # Check 3: the split matches the share, for casualties operated at R2B.
  # Casualties on the other two routes must serve their whole requirement at
  # R2E whatever the share is set to.
  r2b_route <- completed %>% filter(r2b_surgery == 1)
  if (nrow(r2b_route)) {
    realised <- sum(r2b_route$r2b) / sum(r2b_route$total)
    ok <- abs(realised - share) < 1e-6
    if (!ok) {
      fail("share %.2f: R2B-operated casualties served %.4f of their requirement forward, not %.4f",
           share, realised, share)
    }
    report(ok, "share %.2f: R2B-operated casualties served %.4f of the requirement forward",
           share, realised)
  }

  other_route <- completed %>% filter(r2b_surgery != 1)
  if (nrow(other_route)) {
    ok <- all(other_route$r2b == 0) && all(abs(other_route$r2e - other_route$total) < 1e-6)
    if (!ok) {
      fail("share %.2f: a casualty not operated on at R2B served post-operative time forward anyway",
           share)
    }
    report(ok, "share %.2f: all %d casualties not operated at R2B served their whole requirement at R2E",
           share, nrow(other_route))
  }

  # Check 4: the boundary shares behave as their definitions demand.
  if (share == 0) {
    ok <- all(completed$r2b == 0)
    if (!ok) fail("share 0: %d casualties held an R2B bed for post-operative recovery",
                  sum(completed$r2b > 0))
    report(ok, "share 0: no casualty held an R2B bed for post-operative recovery")
  }
  if (share == 1 && nrow(r2b_route)) {
    ok <- all(r2b_route$r2e == 0)
    if (!ok) fail("share 1: %d R2B-operated casualties still spent post-operative time at R2E",
                  sum(r2b_route$r2e > 0))
    report(ok, "share 1: no R2B-operated casualty spent post-operative time at R2E")
  }

  results[[as.character(share)]] <- by_route
}

# ── Check 5: surgical time is unchanged across routes ───────────────────────
# The second R2E procedure is skipped for anyone who had R2B damage control
# surgery, which is what conserves surgical time. That behaviour predates this
# check and must survive it.

cat("\n-- Surgical time remains conserved --\n")

env_data <<- env_data_base
invisible(capture.output(suppressWarnings(
  wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)
)))

surg <- get_mon_attributes(wrapped) %>%
  filter(key %in% c("r2b_surgery", "r2e_surgery_2_start")) %>%
  group_by(name, key) %>%
  summarise(value = dplyr::last(value), .groups = "drop") %>%
  pivot_wider(names_from = key, values_from = value)

if (!"r2b_surgery" %in% names(surg)) surg$r2b_surgery <- NA_real_
if (!"r2e_surgery_2_start" %in% names(surg)) surg$r2e_surgery_2_start <- NA_real_

both <- surg %>% filter(!is.na(r2b_surgery), r2b_surgery == 1, !is.na(r2e_surgery_2_start))

ok <- nrow(both) == 0
if (!ok) {
  fail("%d casualties received both R2B damage control surgery and the R2E second procedure",
       nrow(both))
}
report(ok, "no casualty received both R2B damage control surgery and the R2E second procedure")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All post-operative ICU time conservation checks passed.\n")
quit(status = 0)
