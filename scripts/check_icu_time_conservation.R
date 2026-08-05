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
# Why this check exists: a casualty's intensive care requirement follows from
# the injury, so the total should not depend on which mix of echelons delivers
# it. The model once failed this badly and silently — R2B provided no
# post-operative intensive care at all, while R2E separately shortened its own
# episode for the very casualties R2B had operated on, so an R2B-operated
# casualty received about 28% of the ICU time an otherwise identical
# R2E-operated one did. Nothing in the run output said so: both routes
# produced plausible casualty counts, plausible bed utilisation and plausible
# mortality.
#
# Two invariants are now structural, and this check confirms both hold at
# every forward share:
#   1. The stabilisation requirement is drawn once and split between the
#      echelons by the forward share and the forward-hold time cap, so the
#      total is the same on all three routes a casualty requiring surgery
#      can take.
#   2. Post-definitive care follows the definitive repair, which only R2E
#      performs, so no amount of forward holding may reduce it. Without this,
#      raising the forward share would quietly empty out the intensive care
#      that has to come after the final operation.
#
# The first invariant applies to the damage control cohort alone. A
# single-stage casualty has no stabilisation phase to conserve, since their
# one operation is their definitive repair, so they are excluded from the
# conservation universe rather than counted as a shortfall. That exclusion is
# itself checked: a single-stage casualty must draw no stabilisation
# requirement, hold no bed for one, and never return to theatre. The second
# invariant applies to both pathways, every operated casualty receiving
# post-definitive care.

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

# Two routes' sample means never match exactly, so the route comparison needs
# a criterion that scales with the noise rather than a fixed percentage. A
# flat relative tolerance fails on both sides: too tight at small cohort
# sizes or wide distributions, too loose at large ones. The test is instead a
# Welch t statistic on the drawn requirement between routes, flagged only
# beyond ROUTE_T_MAX standard errors. At 4 SE, ordinary sampling noise
# essentially never trips it, while the 72% shortfall this check was written
# for would register at many times that.
ROUTE_T_MAX <- 4

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
#' @return Data frame covering every casualty who reached a surgical decision:
#'   name, dcs (1 damage control, 0 single-stage), total (the stabilisation
#'   requirement drawn, zero where none was), r2b and r2e (the minutes each
#'   echelon served, zero where it served none), the r2b_surgery and
#'   r2b_bypassed route markers, and the post-definitive care outcome
per_casualty <- function(attrs) {
  # dcs_pathway is kept out of the zero-fill below: 0 is a meaningful value
  # there (single-stage), so an absent attribute must stay absent rather than
  # become a casualty this check believes took the single-stage pathway.
  numeric_fill <- c("stabilisation_total", "r2b_post_op_min", "r2e_post_op_min",
                    "r2b_surgery", "r2b_bypassed", "post_op_pathway", "dow",
                    "post_definitive_min", "post_definitive_pathway",
                    "r2e_surgery_2_start")
  wanted <- c(numeric_fill, "surgery", "dcs_pathway")

  attrs %>%
    filter(key %in% wanted) %>%
    group_by(name, key) %>%
    summarise(value = dplyr::last(value), .groups = "drop") %>%
    pivot_wider(names_from = key, values_from = value) %>%
    # A casualty who never entered a given step has no row for its attribute;
    # absent means zero minutes served there, not an unknown quantity.
    mutate(across(any_of(numeric_fill), ~ ifelse(is.na(.x), 0, .x))) %>%
    filter(!is.na(dcs_pathway)) %>%
    transmute(
      name,
      dcs          = dcs_pathway,
      total        = if ("stabilisation_total" %in% names(.)) stabilisation_total else 0,
      r2b          = if ("r2b_post_op_min" %in% names(.)) r2b_post_op_min else 0,
      r2e          = if ("r2e_post_op_min" %in% names(.)) r2e_post_op_min else 0,
      r2b_surgery  = if ("r2b_surgery" %in% names(.)) r2b_surgery else 0,
      r2b_bypassed = if ("r2b_bypassed" %in% names(.)) r2b_bypassed else 0,
      pathway      = if ("post_op_pathway" %in% names(.)) post_op_pathway else 0,
      dow          = if ("dow" %in% names(.)) dow else 0,
      pd_min       = if ("post_definitive_min" %in% names(.)) post_definitive_min else 0,
      pd_pathway   = if ("post_definitive_pathway" %in% names(.)) post_definitive_pathway else 0,
      second_op    = if ("r2e_surgery_2_start" %in% names(.)) r2e_surgery_2_start else 0
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

  all_cas <- per_casualty(get_mon_attributes(wrapped))

  # Check 0: the pathway split is real and the single-stage cohort carries
  # none of the staged pathway's consumption. This is what makes the
  # conservation universe below a pathway-aware subset rather than a silently
  # shrinking one: a single-stage casualty missing from the conservation
  # counts must be missing because they have no stabilisation phase, not
  # because a stabilisation phase went unserved.
  single <- all_cas %>% filter(dcs == 0)
  report(TRUE, "share %.2f: %d casualties on the damage control pathway, %d single-stage",
         share, sum(all_cas$dcs == 1), nrow(single))

  if (nrow(single)) {
    ok <- all(single$total == 0) && all(single$r2b == 0) && all(single$r2e == 0)
    if (!ok) {
      fail("share %.2f: %d single-stage casualties drew or served a stabilisation requirement",
           share, sum(single$total > 0 | single$r2b > 0 | single$r2e > 0))
    }
    report(ok, "share %.2f: none of the %d single-stage casualties drew or served a stabilisation requirement",
           share, nrow(single))

    ok <- all(single$second_op == 0)
    if (!ok) {
      fail("share %.2f: %d single-stage casualties returned to theatre for a second procedure",
           share, sum(single$second_op > 0))
    }
    report(ok, "share %.2f: none of the %d single-stage casualties returned to theatre",
           share, nrow(single))
  }

  # The conservation universe: the damage control cohort, which is the only
  # cohort with a stabilisation requirement to conserve.
  cas <- all_cas %>% filter(dcs == 1, total > 0)

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
  #    them at an intermediate share. See README Further Development (L24).
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

  route_t <- function(df, value_col) {
    grps <- split(df[[value_col]], df$route)
    grps <- grps[vapply(grps, length, integer(1)) >= 5]
    if (length(grps) < 2) return(NULL)
    pairs <- utils::combn(names(grps), 2, simplify = FALSE)
    stats <- vapply(pairs, function(pr) {
      x <- grps[[pr[1]]]; y <- grps[[pr[2]]]
      se <- sqrt(var(x) / length(x) + var(y) / length(y))
      if (!is.finite(se) || se == 0) return(0)
      abs(mean(x) - mean(y)) / se
    }, numeric(1))
    list(max_t = max(stats), spread = (max(vapply(grps, mean, numeric(1))) -
                                       min(vapply(grps, mean, numeric(1)))) /
                                      mean(vapply(grps, mean, numeric(1))))
  }

  rt <- route_t(completed, "total")
  if (!is.null(rt)) {
    ok <- rt$max_t <= ROUTE_T_MAX
    if (!ok) {
      fail("share %.2f: mean requirement differs across routes by %.1f standard errors (limit %.0f); relative gap %.1f%%",
           share, rt$max_t, ROUTE_T_MAX, 100 * rt$spread)
    }
    report(ok, "share %.2f: route means agree within %.1f standard errors (limit %.0f); relative gap %.1f%%",
           share, rt$max_t, ROUTE_T_MAX, 100 * rt$spread)
  } else {
    report(TRUE, "share %.2f: fewer than two routes carried 5+ casualties, route comparison skipped",
           share)
  }

  # Check 3: the split matches the policy. The forward minutes are the lesser
  # of the intended share and the forward-hold time cap, so this tests both
  # levers at once: a casualty whose share of a long requirement exceeds the
  # cap must be moved on at the cap, not held for the whole share.
  cap <- env_data$vars$r2b$post_op_icu$forward_hold_max
  r2b_route <- completed %>%
    filter(r2b_surgery == 1) %>%
    mutate(expected_r2b = pmin(total * share, cap))

  if (nrow(r2b_route)) {
    worst_split <- max(abs(r2b_route$r2b - r2b_route$expected_r2b))
    ok <- worst_split < 1e-6
    if (!ok) {
      fail("share %.2f: forward minutes differ from min(share x requirement, cap) by up to %.3f minutes",
           share, worst_split)
    }
    n_capped <- sum(r2b_route$total * share > cap + 1e-9)
    report(ok, "share %.2f: forward minutes match min(share x requirement, %g min cap) for all %d R2B-operated casualties (%d capped)",
           share, cap, nrow(r2b_route), n_capped)
  }

  other_route <- completed %>% filter(r2b_surgery != 1)
  if (nrow(other_route)) {
    ok <- all(other_route$r2b == 0) && all(abs(other_route$r2e - other_route$total) < 1e-6)
    if (!ok) {
      fail("share %.2f: a casualty not operated on at R2B served stabilisation time forward anyway",
           share)
    }
    report(ok, "share %.2f: all %d casualties not operated at R2B served their whole requirement at R2E",
           share, nrow(other_route))
  }

  # Check 4: the boundary shares behave as their definitions demand. At a
  # share of one the cap, not the share, is what can still send stabilisation
  # rearward, so the assertion is conditional on it not binding.
  if (share == 0) {
    ok <- all(completed$r2b == 0)
    if (!ok) fail("share 0: %d casualties held an R2B bed for stabilisation",
                  sum(completed$r2b > 0))
    report(ok, "share 0: no casualty held an R2B bed for stabilisation")
  }
  if (share == 1 && nrow(r2b_route)) {
    uncapped <- r2b_route %>% filter(total <= cap + 1e-9)
    if (nrow(uncapped)) {
      ok <- all(uncapped$r2e == 0)
      if (!ok) fail("share 1: %d R2B-operated casualties within the hold cap still stabilised at R2E",
                    sum(uncapped$r2e > 0))
      report(ok, "share 1: none of the %d R2B-operated casualties within the hold cap stabilised at R2E",
             nrow(uncapped))
    }
    capped <- r2b_route %>% filter(total > cap + 1e-9)
    if (nrow(capped)) {
      report(TRUE, "share 1: %d casualties exceeded the %g min hold cap and finished stabilising at R2E, as intended",
             nrow(capped), cap)
    }
  }

  # Check 5: post-definitive care is served at R2E on every route and is not
  # eroded by the forward share. It follows the definitive repair, which only
  # R2E performs, so no amount of forward holding may reduce it. Unlike
  # stabilisation this applies to both surgical pathways, every operated
  # casualty having a definitive repair for it to follow, so the whole cohort
  # is in scope here rather than the damage control subset.
  pd <- all_cas %>% filter(pd_pathway > 0)
  if (nrow(pd)) {
    pd$route <- route_of(pd)
    rt_pd <- route_t(pd, "pd_min")
    if (!is.null(rt_pd)) {
      ok <- rt_pd$max_t <= ROUTE_T_MAX
      if (!ok) {
        fail("share %.2f: mean post-definitive ICU differs across routes by %.1f standard errors (limit %.0f)",
             share, rt_pd$max_t, ROUTE_T_MAX)
      }
      report(ok, "share %.2f: post-definitive ICU route means agree within %.1f standard errors; relative gap %.1f%%",
             share, rt_pd$max_t, 100 * rt_pd$spread)
    }
    report(TRUE, "share %.2f: %d casualties received post-definitive care (%d in ICU, %d in a holding bed)",
           share, nrow(pd), sum(pd$pd_pathway == 1), sum(pd$pd_pathway == 2))
  } else {
    fail("share %.2f: no casualty received post-definitive care", share)
    report(FALSE, "share %.2f: no casualty received post-definitive care", share)
  }

  results[[as.character(share)]] <- by_route
}

# ── Check 6: surgical time is unchanged across routes ───────────────────────
# The second R2E procedure is skipped for anyone who had R2B damage control
# surgery, which is what conserves surgical time. That behaviour predates this
# check and must survive it. Every operated casualty must also receive
# post-definitive care on one pathway or the other, which is the assertion the
# single-stage split most easily breaks: a casualty routed around the
# stabilisation phase must not be routed around the episode that follows their
# definitive repair as well.

cat("\n-- Surgical time remains conserved --\n")

env_data <<- env_data_base
invisible(capture.output(suppressWarnings(
  wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)
)))

attrs_final <- get_mon_attributes(wrapped)

surg <- attrs_final %>%
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

# Every operated casualty receives post-definitive care, on either pathway.
# The universe is casualties who reached R2E's final disposition, marked by
# recovery_to_duty_days, which is drawn immediately after post-definitive
# care. Anyone short of that point either died or was still in the pipeline
# when the run ended, and has no missing episode to explain; anyone past it
# has had their one chance at the episode and must have taken it.
reached_disposition <- attrs_final %>%
  filter(key == "recovery_to_duty_days") %>%
  distinct(name)

operated <- per_casualty(attrs_final) %>%
  semi_join(reached_disposition, by = "name")
missing_pd <- operated %>% filter(pd_pathway == 0)

ok <- nrow(operated) > 0 && nrow(missing_pd) == 0
if (nrow(operated) == 0) {
  fail("no casualty was operated on, so post-definitive coverage could not be checked")
} else if (nrow(missing_pd)) {
  fail("%d surviving operated casualties received no post-definitive care", nrow(missing_pd))
}
report(ok, "all %d surviving operated casualties received post-definitive care (%d damage control, %d single-stage)",
       nrow(operated), sum(operated$dcs == 1), sum(operated$dcs == 0))

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All post-operative ICU time conservation checks passed.\n")
quit(status = 0)
