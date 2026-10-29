##############################################################################
## R/dow_examples.R                                                         ##
## Worked example casualties of the care-state died-of-wounds model         ##
##############################################################################
#
# The care-state model (README Died of Wounds) charges each casualty a death
# rate per hour, their priority's base rate times the multiplier of the care
# they are receiving. This module holds four worked Priority 1 casualties whose
# care follows fixed timings, and the closed-form risk each accrues, so the
# README figure (scripts/render_dow_care_states.R) and the console's live
# preview (R/app_config_helpers.R) draw the same casualties from one definition.
# Nothing here runs the model: the timings are typical of a run rather than
# drawn from one, and are stated in DOW_EXAMPLE_TIMES.
#
# Base R only, and independent of every other module under R/, so the console
# and a renderer can source it without simmer.

#' Timings of the worked example casualties, in hours from injury
#'
#' @details R1 treatment ends at 20 minutes and resuscitation at R2 at 95
#'   minutes; a prompt operation ends at 4.6 hours, close to the model's median
#'   for Priority 1, and a delayed one 12 hours later. Each operated casualty
#'   then spends a day in post-operative care, two hours outside it for a
#'   second operation, a second day in post-operative care, and five days
#'   awaiting an airlift sortie.
DOW_EXAMPLE_TIMES <- list(r1 = 20 / 60, resus = 95 / 60, close = 4.6, delay = 12,
                          stay = 24, between = 2, wait = 120)

#' The care-state timeline of one worked example casualty
#'
#' @param kind One of "icu", "hold", "delayed" or "unoperated".
#' @param multipliers The `dow.multipliers` block: `r1_treatment`,
#'   `resuscitation`, `post_op`, `r2b_icu_penalty` and `hold_bed_penalty`.
#' @return Data frame of `start` (hours from injury) and `multiplier`, one row
#'   per care state in order.
dow_example_path <- function(kind, multipliers) {
  m <- multipliers
  t <- DOW_EXAMPLE_TIMES
  pre <- data.frame(start = c(0, t$r1, t$resus),
                    multiplier = c(1, m$r1_treatment, m$r1_treatment * m$resuscitation))
  if (identical(kind, "unoperated")) return(pre)
  close <- t$close + if (identical(kind, "delayed")) t$delay else 0
  bed <- m$post_op * if (identical(kind, "hold")) m$hold_bed_penalty else 1
  outside <- m$post_op * m$hold_bed_penalty
  post <- data.frame(
    start = close + c(0, t$stay, t$stay + t$between, 2 * t$stay + t$between,
                      2 * t$stay + t$between + t$wait),
    multiplier = c(bed, outside, bed, m$post_op, 0)
  )
  rbind(pre, post)
}

#' The worked example casualties, labelled for a figure legend
#'
#' @param multipliers The `dow.multipliers` block.
#' @return Named list of timelines, in the order a legend prints them.
dow_example_paths <- function(multipliers) {
  list(
    "Prompt operation, intensive care" = dow_example_path("icu", multipliers),
    "Prompt operation, holding bed" = dow_example_path("hold", multipliers),
    "Operation after a 12-hour wait" = dow_example_path("delayed", multipliers),
    "Resuscitated, never operated" = dow_example_path("unoperated", multipliers)
  )
}

#' The multiplier in force at given times on a timeline
#'
#' @param path A timeline from dow_example_path().
#' @param hours Numeric vector of hours from injury.
#' @return The multiplier in force at each time.
dow_example_multiplier <- function(path, hours) {
  path$multiplier[findInterval(hours, path$start)]
}

#' The chance of having died by given times on a timeline
#'
#' @param path A timeline from dow_example_path().
#' @param rate_per_day The base rate, deaths per day of untreated injury.
#' @param hours Numeric vector of hours from injury.
#' @return The cumulative death probability at each time, one less the
#'   survival of the risk accrued, exp of minus the rate times the sum of each
#'   state's multiplier and the hours spent in it.
dow_example_risk <- function(path, rate_per_day, hours) {
  ends <- c(path$start[-1], Inf)
  vapply(hours, function(h) {
    spent <- pmax(0, pmin(h, ends) - path$start)
    1 - exp(-rate_per_day / 24 * sum(path$multiplier * spent))
  }, numeric(1))
}
