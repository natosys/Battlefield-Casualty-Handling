#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_r2e_placement.R                                       ##
## Regression check — a casualty is placed in the best bed free when its    ##
## care begins, never queued for one, at R2E and at R2B reception           ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_r2e_placement.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. A casualty whose care calls for a bed takes the best
# one free at that instant: the bed the care calls for, otherwise a holding bed,
# otherwise overflow (placement_now(), R/trajectories_common.R). The model this
# replaced queued casualties for particular beds at four points of the R2E
# pathway and at R2B reception, so a casualty could lie in no bed for days with
# its post-operative stabilisation, its staging for flight or its resuscitation
# not begun, and the measured "queues" were mostly casualties the model had
# nowhere to put. Each point can regress on its own, a placement becoming a
# seize again, and nothing downstream would flag it, since a queue of unbedded
# casualties reads like saturation.
#
# What it asserts, on a high-intensity campaign, where every point saturates:
#
#   1. Nobody queues for an R2E intensive care bed, and the R2E holding queue
#      is exactly the time casualties retained to convalesce spent queueing
#      (queue_for_bed()), so nobody else queues for a holding bed.
#   2. At R2B nobody queues for a resuscitation bed, and the holding queue is
#      exactly the time the convalescents of the R2B holding queue spent in it.
#   3. Every post-operative stay begins when its operation ends: the
#      stabilisation after an abbreviated operation at R2E, and the
#      post-definitive stay after the definitive repair.
#   4. A damage control casualty operated on at R2B serves its remaining
#      stabilisation before its definitive repair here, in whatever bed, and
#      a casualty operated on at R2E serves its whole drawn requirement.
#   5. An evacuee is staged at the instant its disposition, or its pre-flight
#      care, allows, so it is never kept from a place for want of a bed.
#   6. A casualty waiting at R2B reception waits only while every
#      resuscitation bed of its unit is occupied, and one finding a bed free
#      waits not at all.
#   7. Every placement arm is reached: intensive care, holding bed and overflow
#      for each post-operative stay and for pre-flight care, holding bed and
#      overflow for staging, and resuscitation, holding bed and overflow at R2B
#      reception, so none of the above holds vacuously.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
  library(ggplot2)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/airlift.R")

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

#' Campaign length of the run, in days
#'
#' @details Long enough at high intensity for every placement arm to be
#'   reached, the holding-bed arm of pre-flight care being the rarest.
CHECK_DAYS <- 90L

#' Control seed the run is made under
CHECK_SEED <- 42L

#' Scenario profile the run uses
CHECK_SCENARIO <- "high_intensity"

#' Tolerance on a comparison of simulation times, in minutes
TOL <- 1e-6

config_snapshot <- capture_config_globals()
on.exit(restore_config_globals(config_snapshot), add = TRUE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
apply_airlift_setting(json_data, CHECK_SCENARIO)

cat(sprintf("Running seed %d for %d days under %s\n", CHECK_SEED, CHECK_DAYS, CHECK_SCENARIO))
set.seed(CHECK_SEED)
env <- run_once(CHECK_DAYS, seed = CHECK_SEED)

resources  <- simmer::get_mon_resources(env)
attributes <- simmer::get_mon_attributes(env)
attributes$replication <- 1L
arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
arrivals$replication <- 1L
arrivals   <- arrivals[!duplicated(arrivals$name), ]
wide <- build_attributes_wide(attributes, arrivals) %>%
  dplyr::right_join(arrivals, by = c("name", "replication"), suffix = c("", "_arrival"))

#' Value of one attribute for named casualties
#'
#' @param names Casualty names.
#' @param key Attribute key.
#' @return The attribute's last value for each, NA where unset.
attr_of <- function(names, key) wide[[key]][match(names, wide$name)]

#' Area under one pool's queue over the campaign, from the resource monitor
#'
#' @param pattern Regular expression matching the pool's beds.
#' @return Casualty-minutes queued.
queue_area <- function(pattern) {
  rows <- resources[grepl(pattern, resources$resource), ]
  if (nrow(rows) == 0) return(0)
  step_bin_means(pool_queue_steps(rows$resource, rows$time, rows$queue),
                 c(0, horizon)) * horizon
}

#' Casualty-minutes queued for a bed by convalescents retained at one echelon
#'
#' @param who Logical vector over `wide` selecting the casualties retained there.
#' @return The sum of their queueing intervals, each running from retention to
#'   the bed, to the return to duty in the queue, or to the campaign's close.
#'
#' @details queue_for_bed() records the retention (`bed_retained_t`), and the
#'   wait on reaching a bed (`bed_waited`); a casualty whose recovery elapsed in
#'   the queue left it at retention plus its budget (`bed_budget`).
retained_area <- function(who) {
  w <- wide[who & !is.na(wide$bed_retained_t), ]
  end <- ifelse(!is.na(w$bed_waited), w$bed_retained_t + w$bed_waited,
                ifelse(!is.na(w$rtd_while_queued) & w$rtd_while_queued == 1,
                       w$bed_retained_t + w$bed_budget, horizon))
  sum(pmax(pmin(end, horizon) - w$bed_retained_t, 0))
}

horizon <- CHECK_DAYS * DAY_MIN

# ── 1. R2E: no queue for intensive care, and for holding only convalescence ──

cat("\n-- R2E beds are placed, not queued for --\n")

report(max(resources$queue[grepl("^b_r2eheavy_icu_", resources$resource)]) == 0,
       "nobody queues for an R2E intensive care bed")
r2e_retained <- (!is.na(wide$bf_restoration) & wide$bf_restoration == 3) |
  (!is.na(wide$evacuation_reason) & wide$evacuation_reason == 0 &
     !is.na(wide$r2e_arrival_time))
r2e_area <- queue_area("^b_r2eheavy_hold_")
r2e_expected <- retained_area(r2e_retained)
report(r2e_area > 0 && abs(r2e_area - r2e_expected) < 1e-3 * max(1, r2e_expected),
       paste("the R2E holding queue is the retained convalescents' alone",
             "(%.0f casualty-minutes queued, %.0f by retained convalescents)"),
       r2e_area, r2e_expected)

# ── 2. R2B: no queue for resuscitation, and for holding only convalescence ───

cat("\n-- R2B reception is placed, not queued for --\n")

report(max(resources$queue[grepl("^b_r2b_resus_", resources$resource)]) == 0,
       "nobody queues for an R2B resuscitation bed")
r2b_retained <- (!is.na(wide$bf_restoration) & wide$bf_restoration == 2) |
  (!is.na(wide$r2b_hold_queued) & wide$r2b_hold_queued == 1)
r2b_area <- queue_area("^b_r2b_hold_")
r2b_expected <- retained_area(r2b_retained)
report(abs(r2b_area - r2b_expected) < 1e-3 * max(1, r2b_expected),
       paste("the R2B holding queue is the holding queue's convalescents' alone",
             "(%.0f casualty-minutes queued, %.0f by those convalescents)"),
       r2b_area, r2b_expected)

# ── 3. Post-operative stays begin when the operation ends ────────────────────

cat("\n-- recovery begins when the operation ends --\n")

r2b_operated <- !is.na(wide$r2b_surgery) & wide$r2b_surgery == 1
abbreviated <- wide[!r2b_operated & !is.na(wide$r2e_post_op_start) &
                      !is.na(wide$r2e_surgery_1_end), ]
report(nrow(abbreviated) > 0 &&
         all(abs(abbreviated$r2e_post_op_start - abbreviated$r2e_surgery_1_end) < TOL),
       "every stabilisation after an abbreviated R2E operation begins as it ends (%d)",
       nrow(abbreviated))

definitive_end <- ifelse(!is.na(wide$r2e_surgery_2_end), wide$r2e_surgery_2_end,
                         wide$r2e_surgery_1_end)
post_def <- !is.na(wide$post_definitive_start) & !is.na(definitive_end)
report(sum(post_def) > 0 &&
         all(abs(wide$post_definitive_start[post_def] - definitive_end[post_def]) < TOL),
       "every post-definitive stay begins as the definitive repair ends (%d)", sum(post_def))

# ── 4. Stabilisation is served before the repair, in full ───────────────────

cat("\n-- stabilisation precedes the definitive repair and is served in full --\n")

pre_def <- wide[r2b_operated & !is.na(wide$r2e_post_op_start) &
                  !is.na(wide$r2e_surgery_1_start), ]
report(nrow(pre_def) > 0 &&
         all(pre_def$r2e_post_op_start + pre_def$r2e_post_op_min <=
               pre_def$r2e_surgery_1_start + TOL),
       paste("every damage control casualty operated on at R2B serves its remaining",
             "stabilisation before its definitive repair (%d)"), nrow(pre_def))
report(any(pre_def$post_op_pathway >= 2),
       "and some of them are stabilised outside intensive care, when it was full (%d)",
       sum(pre_def$post_op_pathway >= 2))
second <- abbreviated[!is.na(abbreviated$r2e_surgery_2_start), ]
report(nrow(second) > 0 &&
         all(abs(second$r2e_post_op_min - second$stabilisation_total) < TOL) &&
         all(second$r2e_post_op_start + second$r2e_post_op_min <=
               second$r2e_surgery_2_start + TOL),
       paste("every casualty operated on at R2E serves its whole drawn stabilisation",
             "before its second operation (%d)"), nrow(second))

# ── 5. Evacuees are staged without waiting ───────────────────────────────────

cat("\n-- evacuees are staged when their care allows --\n")

evac <- wide[!is.na(wide$ame_stage_start), ]
stable <- evac[is.na(evac$ame_icu_hold) | evac$ame_icu_hold == 0, ]
report(nrow(stable) > 0 &&
         all(abs(stable$ame_stage_start - stable$r2e_departure_time) < TOL),
       "every evacuee needing no pre-flight care is staged at its disposition (%d)",
       nrow(stable))
vent <- evac[!is.na(evac$ame_icu_hold) & evac$ame_icu_hold == 1, ]
expected <- vent$r2e_departure_time +
  ifelse(vent$r2e_preflight_bed == 2, 0, vent$ame_preflight_min)
report(nrow(vent) > 0 && all(abs(vent$ame_stage_start - expected) < TOL),
       paste("every ventilated evacuee is staged when its pre-flight care ends, or at",
             "its disposition when that care is in a holding bed (%d)"), nrow(vent))

# ── 6. R2B reception waits only while every resuscitation bed is in use ─────

cat("\n-- R2B reception waits only for a full resuscitation area --\n")

received <- wide[!is.na(wide$r2b_reception_bed), ]
direct <- received[received$r2b_reception_bed == 1, ]
report(nrow(direct) > 0 && all(abs(direct$r2b_reception_wait) < TOL),
       "a casualty finding a resuscitation bed free waits not at all (%d)", nrow(direct))

waited <- received[received$r2b_reception_bed >= 2, ]
violations <- 0L
for (i in seq_len(nrow(waited))) {
  team <- waited$r2b_treated[i]
  beds <- env_data$elms$r2b[[team]][["resus_bed"]]
  rows <- resources[resources$resource %in% beds, ]
  steps <- pool_queue_steps(rows$resource, rows$time, rows$server)
  from <- waited$r2b_treatment_start_time[i]
  to   <- from + waited$r2b_reception_wait[i]
  at_start <- steps$total[max(c(0L, which(steps$time <= from + TOL)))]
  inside <- steps$total[steps$time > from + TOL & steps$time < to - TOL]
  if (length(at_start) == 0 || any(c(at_start, inside) < length(beds))) {
    violations <- violations + 1L
  }
}
report(nrow(waited) > 0 && violations == 0L,
       paste("every casualty waiting at reception waits only while each resuscitation",
             "bed of its unit is occupied (%d waits, %d violations)"),
       nrow(waited), violations)

# ── 7. Every placement arm is reached ────────────────────────────────────────

cat("\n-- every placement arm is reached --\n")

#' Count of casualties whose attribute takes one value
#'
#' @param key Attribute key.
#' @param val Value counted.
#' @return Integer count.
reached <- function(key, val) sum(!is.na(wide[[key]]) & wide[[key]] == val)

arms <- list(
  list("post_op_pathway", 1:3, "stabilisation"),
  list("post_definitive_pathway", 1:3, "post-definitive care"),
  list("r2e_preflight_bed", 1:3, "pre-flight care"),
  list("r2e_stage_bed", 2:3, "staging"),
  list("r2b_reception_bed", 1:3, "R2B reception")
)
for (a in arms) {
  counts <- vapply(a[[2]], function(v) reached(a[[1]], v), numeric(1))
  report(all(counts > 0), "%s reaches every placement (%s)", a[[3]],
         paste(sprintf("%d: %d", a[[2]], counts), collapse = ", "))
}

if (length(state$failures) > 0) {
  cat(sprintf("\n%d check(s) failed.\n", length(state$failures)))
  quit(status = 1)
}
cat("\nAll checks passed.\n")
quit(status = 0)
