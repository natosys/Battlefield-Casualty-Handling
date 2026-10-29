#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_r1_evacuation.R                                       ##
## Regression check — every casualty leaves R1, and Role 2 receives each    ##
## by what it needs                                                         ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_r1_evacuation.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The casualty streams are FORECAS medical admissions,
# casualties requiring at least three days of Echelon II care, so every one of
# them leaves R1 (README Casualty Generation). The model once held about a
# fifth of them at R1 and returned them to duty there, surgical casualties
# among them, which understated Role 2 surgical, intensive care and holding
# demand. The rule that replaced it changes how casualties are admitted at
# Role 2 as well, and a run with any part of it undone still produces plausible
# totals, so each part is asserted here:
#
#   1. No casualty returns to duty from R1, and every casualty requiring
#      surgery who survives the R1 check reaches Role 2.
#   2. Battle fatigue is Priority 3 and returns to duty from a Role 2 holding
#      bed, without strategic evacuation; disease is Priority 1 at the
#      configured share, otherwise Priority 2 if surgical and 3 if not.
#   3. No battle fatigue or non-surgical disease casualty below Priority 1 is
#      resuscitated, and no disease casualty dies of wounds.
#   4. Convalescence follows the casualty's group: a non-battle injury takes
#      the wounded factors, a Priority 1 disease casualty the Priority 2 factor,
#      and every other disease casualty the minor one.
#   5. A casualty requiring surgery is sent to an R2B team with an idle theatre
#      and every other casualty to one below the holding threshold, so neither
#      is diverted rearward for want of what it does not need.
#   6. A casualty bypassing R2B travels the R1 to R2B and R2B to R2E legs
#      together.
#   7. Under forced holding saturation, a retained casualty's return to duty
#      falls at its retention time plus its convalescence whether or not it
#      queued, and none is given a bed after its convalescence has elapsed.
#
# Assertions 4 and 5 drive the model's helpers in a minimal simmer environment,
# so their outcomes are exact; the others read a campaign at the shipped
# configuration and one with the holding establishment cut to force queues.

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

#' Campaign length, in days
CHECK_DAYS <- 60L
#' Control seed of both campaigns
CHECK_SEED <- 42L
#' Holding beds per R2B and R2E team in the saturated campaign
SATURATED_HOLD_BEDS <- 1L
#' Tolerance on a time identity, in minutes
TIME_TOL <- 1e-6

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

#' Enter the shipped configuration, optionally with the holding beds cut
#'
#' @param hold_beds Holding beds per R2B and R2E team, or NULL for the shipped
#'   establishment.
#' @return Invisibly, the described configuration entered.
enter_config <- function(hold_beds = NULL) {
  resolved <- resolve_scenario(json_data, "default")
  if (!is.null(hold_beds)) {
    found <- 0L
    for (i in seq_along(resolved$elms)) {
      if (!resolved$elms[[i]]$elm %in% c("r2b", "r2eheavy")) next
      for (j in seq_along(resolved$elms[[i]]$beds)) {
        if (!identical(resolved$elms[[i]]$beds[[j]]$name, "hold")) next
        resolved$elms[[i]]$beds[[j]]$qty <- as.integer(hold_beds)
        found <- found + 1L
      }
    }
    if (found < 2L) stop("holding bed pools not found in elms", call. = FALSE)
  }
  described <- build_environment(resolved)
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Run one campaign and pivot its attributes to one row per casualty
#'
#' @param hold_beds As for enter_config().
#' @return Data frame of each casualty's last value of every attribute, with
#'   `first_transport` and `first_r2e_arrival`, the first value of each, the
#'   move from R1 being a casualty's first transport and its first arrival at
#'   R2E.
campaign <- function(hold_beds = NULL) {
  enter_config(hold_beds)
  invisible(capture.output(suppressWarnings(
    wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)
  )))
  att <- get_mon_attributes(wrapped) %>% filter(name != "")
  wide <- att %>%
    group_by(name, key) %>%
    summarise(value = dplyr::last(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = key, values_from = value)
  for (k in MODEL_ATTRIBUTE_KEYS) if (!k %in% names(wide)) wide[[k]] <- NA_real_
  firsts <- att %>%
    filter(key %in% c("transport_start_time", "r2e_arrival_time")) %>%
    group_by(name, key) %>%
    summarise(value = dplyr::first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = key, values_from = value, names_prefix = "first_")
  wide %>% left_join(firsts, by = "name") %>%
    rename(first_transport = first_transport_start_time,
           first_r2e_arrival = first_r2e_arrival_time)
}

#' Whether a value is set and equal to one
#'
#' @param x Numeric vector.
#' @return Logical vector.
is1 <- function(x) !is.na(x) & x == 1

#' Whether each casualty is of a DNBI sub-type
#'
#' @param x The `dnbi_type` column.
#' @param type 1 for battle fatigue, 2 for disease, 3 for non-battle injury.
#' @return Logical vector.
is_type <- function(x, type) !is.na(x) & x == type

cat(sprintf("R1 evacuation check: %d-day campaigns, seed %d\n\n", CHECK_DAYS, CHECK_SEED))
d <- campaign()
casualties <- d %>% filter(grepl("^(wia|dnbi)", name))
disease <- casualties %>% filter(is_type(dnbi_type, 2))
bf <- casualties %>% filter(is_type(dnbi_type, 1))

# ── 1. Every casualty leaves R1 ─────────────────────────────────────────────

cat("-- every casualty leaves R1 --\n")
report(!any(!is.na(casualties$return_echelon) & casualties$return_echelon == 1),
       "no casualty returns to duty from R1 (%d returned to duty in all)",
       sum(!is.na(casualties$return_day)))
surgical <- casualties %>% filter(is1(surgery), is.na(dow_echelon) | dow_echelon != 1)
reached <- !is.na(surgical$r2b_treated) | !is.na(surgical$r2e_treated)
report(nrow(surgical) > 0 && all(reached),
       "every casualty requiring surgery who survives R1 reaches Role 2 (%d of %d)",
       sum(reached), nrow(surgical))

# ── 2. Priority of disease and battle fatigue ───────────────────────────────

cat("\n-- priority of disease and battle fatigue --\n")
report(nrow(bf) > 0 && all(bf$priority == 3), "battle fatigue is Priority 3 (%d casualties)",
       nrow(bf))
returned <- bf %>% filter(!is.na(return_day))
report(nrow(returned) > 0 && all(returned$return_echelon %in% c(2, 3)) &&
         all(!is.na(returned$bf_restoration)),
       "battle fatigue returns to duty from a Role 2 holding bed (%d of %d returned)",
       nrow(returned), nrow(bf))
report(!any(is1(bf$r2e_evac)) && !any(is1(bf$dow)),
       "no battle fatigue casualty is evacuated strategically or dies of wounds")
share <- mean(disease$priority == 1)
p1_share <- env_data$vars$r1$other$disease_p1_pct
# Binomial bound at four standard errors, wide enough that the seed cannot
# fail it and narrow enough to reject the trauma split's 65%.
bound <- 4 * sqrt(p1_share * (1 - p1_share) / nrow(disease))
report(abs(share - p1_share) <= bound,
       "disease is Priority 1 at the configured share (%.3f against %.2f, +/- %.3f)",
       share, p1_share, bound)
rest <- disease %>% filter(priority != 1)
report(nrow(rest) > 0 && all((rest$priority == 2) == is1(rest$surgery)) &&
         any(rest$priority == 2),
       "disease below Priority 1 is Priority 2 exactly when surgical (%d of %d surgical)",
       sum(rest$priority == 2), nrow(rest))

# ── 3. Resuscitation and mortality ──────────────────────────────────────────

cat("\n-- resuscitation and mortality --\n")
no_resus <- casualties %>%
  filter(is_type(dnbi_type, 1) | (is_type(dnbi_type, 2) & priority != 1 & !is1(surgery)))
report(nrow(no_resus) > 0 && !any(is1(no_resus$r2b_resus) | is1(no_resus$r2e_resus)),
       "no battle fatigue or non-surgical disease casualty below Priority 1 is resuscitated (%d)",
       nrow(no_resus))
report(!any(is1(disease$dow)), "no disease casualty dies of wounds (%d disease casualties)",
       nrow(disease))

# ── 4. Convalescence by group ───────────────────────────────────────────────

cat("\n-- convalescence by group --\n")

#' Draw a casualty's convalescence in a minimal environment
#'
#' @param attrs Named numeric of the casualty's attributes.
#' @return The drawn `recovery_to_duty_days`.
draw_for <- function(attrs) {
  assign("env", simmer(), envir = globalenv())
  trj <- trajectory() %>%
    set_attribute(names(attrs), function() attrs) %>%
    set_attribute("recovery_to_duty_days", draw_recovery_to_duty)
  set.seed(1L)
  env %>% add_generator("c", trj, at(0), mon = 2) %>% run() %>% invisible()
  a <- get_mon_attributes(env)
  a$value[a$key == "recovery_to_duty_days"]
}
enter_config()
f <- env_data$vars$r2eheavy$recovery_to_duty
wia_p1_surg <- draw_for(c(priority = 1, injury_type = 1, r2e_surgery = 1))
nbi_p1_surg <- draw_for(c(priority = 1, injury_type = 2, dnbi_type = 3, r2e_surgery = 1))
report(isTRUE(all.equal(nbi_p1_surg, wia_p1_surg)),
       "a Priority 1 surgical non-battle injury convalesces as a wounded casualty (%.2f days)",
       nbi_p1_surg)
dis_p1 <- draw_for(c(priority = 1, injury_type = 2, dnbi_type = 2))
wia_p2 <- draw_for(c(priority = 2, injury_type = 1))
report(isTRUE(all.equal(dis_p1, wia_p2)),
       "a Priority 1 disease casualty takes the Priority 2 factor (%.2f days)", dis_p1)
dis_p2_surg <- draw_for(c(priority = 2, injury_type = 2, dnbi_type = 2, r2e_surgery = 1))
wia_p3 <- draw_for(c(priority = 3, injury_type = 1))
report(isTRUE(all.equal(dis_p2_surg, wia_p3)) &&
         isTRUE(all.equal(dis_p2_surg / wia_p2, f$p3_dnbi / f$p2)),
       "a surgical Priority 2 disease casualty takes the minor factor (%.2f days)", dis_p2_surg)

# ── 5. Routing by need ──────────────────────────────────────────────────────

cat("\n-- routing by need --\n")

#' Select an R2B team for a probe casualty against occupied resources
#'
#' @param surgery The probe's surgical requirement, 0 or 1.
#' @param occupy Resource names held for the probe's arrival.
#' @return The selected team index, or -1 for a bypass.
probe_route <- function(surgery, occupy) {
  assign("env", simmer(), envir = globalenv())
  for (b in unlist(lapply(env_data$elms$r2b, function(t) c(t$hold_bed, t$ot_bed)))) {
    env %>% add_resource(b, 1) %>% invisible()
  }
  holder <- trajectory()
  for (r in occupy) holder <- holder %>% seize(r, 1)
  holder <- holder %>% timeout(10)
  for (r in occupy) holder <- holder %>% release(r, 1)
  probe <- trajectory() %>%
    set_attribute("surgery", surgery) %>%
    set_attribute("r2b", select_r2b_for_casualty)
  env %>%
    add_generator("holder", holder, at(0), mon = 2) %>%
    add_generator("probe", probe, at(1), mon = 2) %>%
    run() %>% invisible()
  a <- get_mon_attributes(env)
  a$value[a$key == "r2b"]
}
all_ot   <- unlist(lapply(env_data$elms$r2b, `[[`, "ot_bed"))
all_hold <- unlist(lapply(env_data$elms$r2b, `[[`, "hold_bed"))
report(probe_route(0, all_ot) > 0,
       "a non-surgical casualty is sent forward when every R2B theatre is busy")
report(probe_route(1, all_ot) == -1,
       "a surgical casualty bypasses R2B when every R2B theatre is busy")
report(probe_route(0, all_hold) == -1,
       "a non-surgical casualty bypasses R2B when every R2B team's holding is full")
report(probe_route(1, all_hold) > 0,
       "a surgical casualty is sent forward when holding is full but a theatre is idle")

# ── 6. The bypass move ──────────────────────────────────────────────────────

cat("\n-- the bypass move --\n")
legs <- list(env_data$vars$r1$wia_transport, env_data$vars$r2b$wia_transport)
lo <- sum(vapply(legs, `[[`, numeric(1), "min"))
hi <- sum(vapply(legs, `[[`, numeric(1), "max"))
bypassed <- casualties %>% filter(!is.na(r2b), r2b == -1, !is.na(first_r2e_arrival))
move <- bypassed$first_r2e_arrival - bypassed$first_transport
report(nrow(bypassed) > 0 && all(move >= lo - TIME_TOL & move <= hi + TIME_TOL) &&
         any(move > env_data$vars$r1$wia_transport$max),
       "a casualty bypassing R2B takes both legs, %.0f to %.0f minutes (%d moves, longest %.1f)",
       lo, hi, nrow(bypassed), if (length(move)) max(move) else NA_real_)

# ── 7. Convalescence while queued ───────────────────────────────────────────

cat("\n-- convalescence while queued, holding cut to one bed a team --\n")
s <- campaign(SATURATED_HOLD_BEDS)
retained <- s %>% filter(!is.na(bed_retained_t), !is.na(return_day))
gap <- retained$return_day - retained$bed_retained_t - retained$bed_budget
queued <- retained %>% filter(bed_waited > 0 | is1(rtd_while_queued))
report(nrow(queued) > 0 && all(abs(gap) <= TIME_TOL),
       paste("a retained casualty returns to duty at retention plus convalescence",
             "(%d returned, %d of them after queueing)"), nrow(retained), nrow(queued))
seized <- s %>% filter(!is.na(bed_waited))
report(all(seized$bed_waited <= seized$bed_budget + TIME_TOL),
       "no casualty is given a bed after its convalescence has elapsed (%d bed stays)",
       nrow(seized))
report(sum(is1(s$rtd_while_queued)) > 0 &&
         all(is.na(s$bed_waited[is1(s$rtd_while_queued)])),
       "casualties whose convalescence elapses in the queue return without a bed (%d)",
       sum(is1(s$rtd_while_queued)))

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (msg in state$failures) cat(sprintf("  - %s\n", msg))
  quit(status = 1)
}
cat("All R1 evacuation checks passed.\n")
quit(status = 0)
