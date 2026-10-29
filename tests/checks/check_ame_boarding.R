#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_ame_boarding.R                                        ##
## Regression check — evacuees board a strategic sortie only when it        ##
## departs, and places nobody takes depart with it                          ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_ame_boarding.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. A sortie opens the two airlift seat pools by its
# airframe's places, boards the casualties queued on them and closes them again
# in the same instant (build_ame_sortie_trajectory(), R/trajectories_schedules.R).
# The model it replaced added the places and left them open, so places an
# under-subscribed sortie did not fill stayed on the pool and a casualty
# reaching the evacuation decision days later boarded at once, between flights.
# That understated the evacuation wait, the staging bed-days it consumes and the
# died-of-wounds risk it carries, and nothing in the evidence pipeline would
# have flagged it, since a wait is a wait whatever produced it. Either half of
# the mechanism can regress on its own: a capacity left open restores boarding
# between flights, and a waiter that is not queued on its pool when the sortie
# fires (a polling wait, for example) misses a flight it could have taken.
#
# What it asserts, on a campaign under each died-of-wounds mechanism, the
# care-state one at high intensity with sorties cancelled so that a pool fills
# and waiters are carried across a lost flight:
#
#   1. No casualty boards between sorties: every increase in a pool's boarded
#      count, and every recorded boarding time, falls on a sortie instant.
#   2. A sortie boards at most its airframe's places on each pool, and a
#      cancelled one opens nothing and boards nobody.
#   3. After every sortie each pool's capacity equals the number ever boarded
#      on it, so the places nobody took have been removed.
#   4. No waiter misses a sortie that had a place free on its pool: where a
#      flown sortie left places empty, nobody is queued on that pool when it
#      closes, and no standard-route casualty staged before it is still waiting
#      after it. A casualty can still join the queue at the sortie's own
#      instant once it has closed, because a casualty is queued for a seat
#      only once it holds a staging bed, and at a full holding pool the beds a
#      sortie frees by boarding are what admit the next casualties. The check
#      asserts that every such late joiner is one of those, staged at that
#      instant, so the only way to miss a flight is the staging bed gate.
#   5. The cases are reached: places depart empty, a sortie fills a pool, and a
#      sortie is cancelled, so none of the above holds vacuously.

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

#' Campaign length of each run, in days
#'
#' @details Long enough for the critical pool to fill on some sorties and leave
#'   places empty on others, which a shorter campaign does not reliably reach.
CHECK_DAYS <- 90L

#' Control seed both runs are made under
CHECK_SEED <- 42L

#' Scenario profile of the care-state run
#'
#' @details High intensity, so that sorties fill at the reduced places below and
#'   leave casualties queued for the next flight.
CHECK_SCENARIO_CARE <- "high_intensity"

#' Airframe places per pool in the care-state run, critical then standard
#'
#' @details Below the shipped airframe's, because a casualty is queued for a
#'   seat only once it holds a staging bed, so the seat queue can never exceed
#'   the beds it stages in and the shipped places are never all taken. Fewer
#'   places let a sortie fill a pool and carry the rest to the next flight,
#'   which is the cap on boarding this check needs to reach.
CHECK_PLACES_CARE <- c(critical = 4, standard = 6)

#' Scenario profile of the legacy run
CHECK_SCENARIO_LEGACY <- "moderate_intensity"

#' Sortie cancellation probability of the care-state run
#'
#' @details High enough that some sorties are lost and their waiters must be
#'   carried by the next flight; the legacy run keeps the shipped zero.
CHECK_FAILURE <- 0.3

#' Monitor pool names and the attribute route code of each
POOLS <- c(ame_critical = 1, ame = 2)

config_snapshot <- capture_config_globals()
on.exit(restore_config_globals(config_snapshot), add = TRUE)

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)

#' Run one campaign and return its monitors and sortie reconstruction
#'
#' @param scenario Scenario profile to run under.
#' @param care_state The `dow.care_state.enabled` value to run under.
#' @param failure The sortie cancellation probability to run at.
#' @param places Airframe places per pool, critical then standard, or NULL for
#'   the configured airframe's.
#' @return A list of `resources` (the seat pools' monitor rows), `wide` (one
#'   row per casualty), `sorties` (compute_ame_sorties()) and `airframe`.
run_campaign <- function(scenario, care_state, failure, places = NULL) {
  apply_airlift_setting(json_data, scenario, "failure_probability", failure)
  env_data$vars$dow$care_state$enabled <<- care_state
  if (!is.null(places)) {
    block <- paste0("airframe_", env_data$vars$role4$ame$airframe)
    env_data$vars$role4[[block]]$critical_capacity <<- places[["critical"]]
    env_data$vars$role4[[block]]$standard_capacity <<- places[["standard"]]
  }
  set.seed(CHECK_SEED)
  env <- run_once(CHECK_DAYS, seed = CHECK_SEED)
  resources <- simmer::get_mon_resources(env)
  resources$replication <- 1L
  attributes <- simmer::get_mon_attributes(env)
  attributes$replication <- 1L
  arrivals <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  arrivals$replication <- 1L
  arrivals <- arrivals[!duplicated(arrivals$name), ]
  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"), suffix = c("", "_arrival"))
  list(resources = resources[resources$resource %in% names(POOLS), ],
       wide = wide,
       sorties = compute_ame_sorties(resources, env_data$vars$role4, CHECK_DAYS),
       airframe = resolve_ame_airframe(env_data$vars$role4))
}

#' Assert every boarding property on one campaign
#'
#' @param run Output of run_campaign().
#' @param label Name of the campaign, printed with each assertion.
#' @return Invisibly, the sortie reconstruction with a `filled` column marking
#'   each flown sortie that took every place on a pool.
check_campaign <- function(run, label) {
  cat(sprintf("\n-- %s --\n", label))
  res <- run$resources
  res <- res[order(res$time, seq_len(nrow(res))), ]
  interval <- env_data$vars$role4$ame$schedule_interval_days * DAY_MIN
  #' Whether each time falls on a scheduled sortie
  #'
  #' @param t Times in minutes.
  #' @return A logical vector, FALSE at time zero, when no sortie flies.
  on_sortie <- function(t) abs(t / interval - round(t / interval)) < 1e-9 & t > 0

  # 1. Boarding only at a sortie instant.
  for (pool in names(POOLS)) {
    r <- res[res$resource == pool, ]
    rises <- r$time[c(r$server[1] > 0, diff(r$server) > 0)]
    report(all(on_sortie(rises)),
           "%s: every rise in the %s boarded count falls on a sortie (%d rises)",
           label, pool, length(rises))
  }
  boarded <- run$wide$ame_departure_time[!is.na(run$wide$ame_departure_time)]
  report(length(boarded) > 0 && all(on_sortie(boarded)),
         "%s: every recorded boarding time is a sortie instant (%d boardings)",
         label, length(boarded))

  # 2. At most the airframe's places per pool; nothing on a cancelled sortie.
  s <- run$sorties[run$sorties$sortie_day < CHECK_DAYS, ]
  places <- ifelse(startsWith(as.character(s$pool), "Critical"),
                   run$airframe$critical_capacity, run$airframe$standard_capacity)
  flown <- s$outcome == "Flown"
  report(all(s$outcome %in% c("Flown", "Cancelled")),
         "%s: every sortie reads as flown or cancelled, none unknown", label)
  report(all(s$seats_used[flown] <= places[flown]) && all(s$seats_used >= 0),
         "%s: no sortie boards more than its airframe's places on a pool", label)
  report(all(s$capacity_added[!flown] == 0 & s$seats_used[!flown] == 0),
         "%s: a cancelled sortie opens nothing and boards nobody (%d cancelled)",
         label, sum(!flown) / 2)

  # 3. Capacity closes to the boarded count after every sortie.
  closed <- vapply(unique(res$time[on_sortie(res$time)]), function(t) {
    all(vapply(names(POOLS), function(pool) {
      at_t <- res[res$resource == pool & res$time == t, ]
      nrow(at_t) == 0 || at_t$capacity[nrow(at_t)] == at_t$server[nrow(at_t)]
    }, logical(1)))
  }, logical(1))
  report(length(closed) > 0 && all(closed),
         "%s: after every sortie each pool's capacity equals its boarded count (%d sorties)",
         label, length(closed))

  # 4. Nobody left behind by a sortie with a place free. The pool closes at
  # the first row after its peak whose capacity has returned to the boarded
  # count; anyone joining after that row joined a departed aircraft.
  w <- run$wide
  left <- 0L
  late <- 0L
  unexplained <- 0L
  for (i in which(flown & s$places_empty > 0)) {
    t <- s$sortie_day[i] * DAY_MIN
    critical <- startsWith(as.character(s$pool[i]), "Critical")
    pool <- if (critical) "ame_critical" else "ame"
    at_t <- res[res$resource == pool & res$time == t, ]
    peak <- which.max(at_t$capacity)
    close <- peak + which(at_t$capacity[-seq_len(peak)] == at_t$server[-seq_len(peak)])[1]
    if (is.na(close) || at_t$queue[close] > 0) left <- left + 1L
    if (is.na(close)) next
    joined <- at_t$queue[nrow(at_t)] - at_t$queue[close]
    staged_now <- sum(!is.na(w$ame_route) & w$ame_route == POOLS[[pool]] &
                        !is.na(w$ame_hold_start) & w$ame_hold_start == t)
    late <- late + max(joined, 0)
    if (joined > staged_now) unexplained <- unexplained + 1L
  }
  report(left == 0L, "%s: no flown sortie with a place free closes with anyone queued on its pool",
         label)
  report(unexplained == 0L,
         "%s: every casualty joining a seat queue after its sortie closed was staged at that instant (%d)",
         label, late)
  staged <- w[!is.na(w$ame_route) & w$ame_route == POOLS[["ame"]] & !is.na(w$ame_hold_start), ]
  std_empty <- s$sortie_day[flown & !startsWith(as.character(s$pool), "Critical") &
                              s$places_empty > 0] * DAY_MIN
  missed <- sum(vapply(std_empty, function(t) {
    sum(staged$ame_hold_start < t & (is.na(staged$ame_hold_end) | staged$ame_hold_end > t))
  }, numeric(1)))
  report(missed == 0,
         "%s: no standard-route casualty staged before a sortie with places free waits past it",
         label)

  s$filled <- flown & s$places_empty == 0 & s$capacity_added > 0
  invisible(s)
}

care <- check_campaign(run_campaign(CHECK_SCENARIO_CARE, 1, CHECK_FAILURE, CHECK_PLACES_CARE),
                       sprintf("care-state mechanism, %s, cancellation %.1f, %d/%d places",
                               CHECK_SCENARIO_CARE, CHECK_FAILURE,
                               CHECK_PLACES_CARE[["critical"]], CHECK_PLACES_CARE[["standard"]]))
legacy <- check_campaign(run_campaign(CHECK_SCENARIO_LEGACY, 0, 0),
                         sprintf("legacy mechanism, %s, no cancellation", CHECK_SCENARIO_LEGACY))

# ── 5. The cases are reached ─────────────────────────────────────────────────

cat("\n-- the cases are reached --\n")
both <- rbind(care, legacy)
report(any(both$outcome == "Flown" & both$places_empty > 0),
       "a flown sortie departs with places empty (%d pool-sorties)",
       sum(both$outcome == "Flown" & both$places_empty > 0))
report(any(both$filled),
       "a flown sortie fills a pool, so the cap on boarding is exercised (%d pool-sorties)",
       sum(both$filled))
report(any(care$outcome == "Cancelled"),
       "a sortie is cancelled, so waiters are carried across a lost flight (%d)",
       sum(care$outcome == "Cancelled") / 2)

if (length(state$failures) > 0) {
  cat(sprintf("\n%d check(s) failed.\n", length(state$failures)))
  quit(status = 1)
}
cat("\nAll checks passed.\n")
quit(status = 0)
