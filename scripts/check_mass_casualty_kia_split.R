#!/usr/bin/env Rscript
##############################################################################
## scripts/check_mass_casualty_kia_split.R                                  ##
## Regression check — a mass casualty event's casualty count is a total,    ##
## split between the wounded and the immediately killed                     ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_mass_casualty_kia_split.R             # 30-day runs
#   Rscript scripts/check_mass_casualty_kia_split.R --days 10   # shorter runs
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step.
#
# Why this check exists. An event's drawn casualty count is a total, of which
# `mass_casualty.event.kia_fraction` are killed at or near the point of injury.
# Three properties of that split are worth holding fixed against later edits,
# and none of them is visible in the output of a single run:
#
#   1. The count is conserved. Wounded plus killed must equal the number drawn,
#      at every fraction including its two degenerate values. A split that
#      loses or duplicates casualties would show up only as a casualty count
#      slightly off a figure nobody recomputes.
#
#   2. The killed take the mortuary pathway, not the wounded trajectory. That
#      is the whole point of routing them through the kia_cbt stream rather
#      than tagging them inside the wounded one, and it is a property of the
#      run rather than of the generator: a killed casualty must carry
#      injury_type 3 and reach mortuary handling, and must never be triaged.
#
#   3. The shipped configuration is unaffected. Mass casualty injection ships
#      disabled (`rate_per_day = 0`), so no event fires, no casualty is split
#      and the fraction must not reach the random stream at all. A run at any
#      fraction has to be bit-identical to a run at any other, or the tracked
#      seed-42 baseline stops describing the shipped model.
#
# What this check deliberately does not assert is that enabling the split
# raises mortuary or transport contention at a given seed. Adding a draw per
# event shifts simmer's single global random stream, so two runs either side of
# it are different realisations rather than a controlled comparison.

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

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

SCENARIO   <- arg_value("--scenario", "default")
CHECK_DAYS <- as.integer(arg_value("--days", 30L))
CHECK_SEED <- as.integer(arg_value("--seed", 42L))

# Enough events for a share to mean something; the generator is called
# directly here, so this costs no simulation time.
GEN_DAYS  <- 2000L
GEN_RATE  <- 0.5

failures <- character(0)
fail     <- function(...) failures <<- c(failures, sprintf(...))

report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}

json <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
base_env_data <- build_environment(resolve_scenario(json, SCENARIO))

day_min <<- 1440L

SHIPPED_RATE     <- as.numeric(base_env_data$vars$mass_casualty$event$rate_per_day)
SHIPPED_FRACTION <- as.numeric(base_env_data$vars$mass_casualty$event$kia_fraction)

#' Draw a long run of events at a given killed share, without simulating
#'
#' @param fraction Value for mass_casualty.event.kia_fraction
#' @param rate Value for mass_casualty.event.rate_per_day
#' @return generate_mass_casualty_events() output
draw_events <- function(fraction, rate = GEN_RATE, n_days = GEN_DAYS) {
  params <- base_env_data$vars$mass_casualty
  params$event$kia_fraction <- fraction
  params$event$rate_per_day <- rate
  generate_mass_casualty_events(n_days, params, seed = CHECK_SEED, write_file = FALSE)
}

cat(sprintf("Mass casualty killed-share check: %s, %d-day runs at seed %d (shipped fraction %g)\n\n",
            SCENARIO, CHECK_DAYS, CHECK_SEED, SHIPPED_FRACTION))

# ── 1. The count is conserved at every fraction ─────────────────────────────

cat("-- an event's casualties are split, not lost or duplicated --\n")

for (frac in c(0, 0.28, 0.5, 1)) {
  ev <- draw_events(frac)
  events <- ev$events

  # An arrival falling past the run's end is dropped by construction, so the
  # per-event columns are compared against the arrivals actually emitted
  # rather than against the drawn count.
  wia_tab <- tabulate(ev$casualty_event_id, nbins = nrow(events))
  kia_tab <- tabulate(ev$kia_casualty_event_id, nbins = nrow(events))

  conserved <- identical(as.integer(events$n_wia), as.integer(wia_tab)) &&
    identical(as.integer(events$n_kia), as.integer(kia_tab)) &&
    all(events$n_cas == events$n_wia + events$n_kia)
  if (!conserved) {
    fail("at kia_fraction %g the per-event wounded/killed columns do not match the arrivals emitted",
         frac)
  }

  # Every arrival belongs to exactly one pathway, and the two pathways share
  # one set of event ids.
  ids_ok <- all(ev$kia_casualty_event_id %in% events$event_id) &&
    all(ev$casualty_event_id %in% events$event_id)
  if (!ids_ok) fail("at kia_fraction %g an arrival carries an event id no event has", frac)

  sorted_ok <- !is.unsorted(ev$arrival_times) && !is.unsorted(ev$kia_arrival_times)
  if (!sorted_ok) fail("at kia_fraction %g a pathway's arrival times are not in chronological order", frac)

  report(conserved && ids_ok && sorted_ok,
         "kia_fraction %.2f: %d events, %d wounded + %d killed = %d casualties",
         frac, nrow(events), sum(events$n_wia), sum(events$n_kia), sum(events$n_cas))
}

# ── 2. The degenerate fractions are degenerate ──────────────────────────────

cat("\n-- 0 gives no killed, 1 gives no wounded --\n")

none <- draw_events(0)
all_ <- draw_events(1)

zero_ok <- length(none$kia_arrival_times) == 0 && all(none$events$n_kia == 0)
if (!zero_ok) fail("kia_fraction 0 still produced %d killed", length(none$kia_arrival_times))
report(zero_ok, "kia_fraction 0 produces no killed across %d events", nrow(none$events))

one_ok <- length(all_$arrival_times) == 0 && all(all_$events$n_wia == 0)
if (!one_ok) fail("kia_fraction 1 still produced %d wounded", length(all_$arrival_times))
report(one_ok, "kia_fraction 1 produces no wounded across %d events", nrow(all_$events))

# ── 3. The realised share tracks the configured one ─────────────────────────

cat("\n-- the realised share is the configured one --\n")

ev    <- draw_events(SHIPPED_FRACTION)
n_tot <- sum(ev$events$n_cas)
share <- sum(ev$events$n_kia) / n_tot

# Binomial standard error over the pooled draw; four of them is wide enough
# that a passing check is not luck and narrow enough to catch a share that is
# wrong by a fraction of itself.
se        <- sqrt(SHIPPED_FRACTION * (1 - SHIPPED_FRACTION) / n_tot)
share_ok  <- abs(share - SHIPPED_FRACTION) <= 4 * se
if (!share_ok) {
  fail("realised killed share %.4f is %.1f standard errors from the configured %.4f over %d casualties",
       share, abs(share - SHIPPED_FRACTION) / se, SHIPPED_FRACTION, n_tot)
}
report(share_ok, "realised %.4f against configured %.4f over %d casualties (%.1f SE)",
       share, SHIPPED_FRACTION, n_tot, abs(share - SHIPPED_FRACTION) / se)

# ── 4. The killed reach the mortuary, and are never triaged ────────────────

cat("\n-- an event's killed take the mortuary pathway --\n")

#' Run the model once with mass casualty injection enabled
#'
#' @param fraction Value for mass_casualty.event.kia_fraction
#' @param rate Value for mass_casualty.event.rate_per_day
#' @return Named list of the monitored quantities the checks below read
#'
#' @details Both values are written into the built env_data rather than into
#'   env_data.json, so the check never touches the tracked configuration.
run_at <- function(fraction, rate) {
  ed <- base_env_data
  ed$vars$mass_casualty$event$kia_fraction <- fraction
  ed$vars$mass_casualty$event$rate_per_day <- rate
  env_data <<- ed
  counts   <<- sapply(ed$elms, length)

  invisible(capture.output(
    wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)
  ))

  arr <- get_mon_arrivals(wrapped, ongoing = TRUE)
  arr <- arr[order(arr$name), ]
  att <- get_mon_attributes(wrapped)

  who <- function(key, val) unique(att$name[att$key == key & att$value == val])

  list(
    digest   = paste(sprintf("%s:%.10f", arr$name, arr$end_time), collapse = "|"),
    n        = nrow(arr),
    tagged   = who("mass_casualty_event", 1),
    killed   = who("injury_type", 3),
    wounded  = who("injury_type", 1),
    mortuary = who("mortuary_treated", 1),
    triaged  = unique(att$name[att$key == "priority" & !is.na(att$value)])
  )
}

run <- run_at(SHIPPED_FRACTION, GEN_RATE)

tagged_killed <- intersect(run$tagged, run$killed)
any_killed    <- length(tagged_killed) > 0
if (!any_killed) {
  fail(paste0("no event produced a killed casualty over %d days at rate %g. Either the split is ",
              "not reaching the run or no event fired, and both make the parameter a no-op"),
       CHECK_DAYS, GEN_RATE)
}
report(any_killed, "%d of %d event-tagged casualties are immediate killed",
       length(tagged_killed), length(run$tagged))

# A killed casualty is handled by the mortuary and is never given a triage
# priority, which is what separates the two pathways in the trajectory.
to_mortuary <- setdiff(tagged_killed, run$mortuary)
mortuary_ok <- length(to_mortuary) == 0
if (!mortuary_ok) {
  fail("%d of an event's killed casualties never reached mortuary handling: %s",
       length(to_mortuary), paste(head(to_mortuary, 5), collapse = ", "))
}
report(mortuary_ok, "every one of them reached mortuary handling")

triaged_killed <- intersect(tagged_killed, run$triaged)
untriaged_ok   <- length(triaged_killed) == 0
if (!untriaged_ok) {
  fail("%d of an event's killed casualties were given a triage priority: %s",
       length(triaged_killed), paste(head(triaged_killed, 5), collapse = ", "))
}
report(untriaged_ok, "none of them was given a triage priority")

# The event's wounded must still be wounded, or the overlay has been attached
# to the wrong stream.
tagged_wounded <- intersect(run$tagged, run$wounded)
both_ok <- length(tagged_wounded) + length(tagged_killed) == length(run$tagged)
if (!both_ok) {
  fail("%d event-tagged casualties are neither wounded nor killed",
       length(run$tagged) - length(tagged_wounded) - length(tagged_killed))
}
report(both_ok, "the other %d are wounded, and every tagged casualty is one or the other",
       length(tagged_wounded))

# ── 5. The shipped configuration cannot see the fraction ────────────────────

cat("\n-- with injection disabled, the fraction reaches nothing --\n")

if (SHIPPED_RATE > 0) {
  cat(sprintf("   shipped rate is %g, not zero; the baseline does fire events\n", SHIPPED_RATE))
} else {
  off_a <- run_at(0, SHIPPED_RATE)
  off_b <- run_at(1, SHIPPED_RATE)

  inert_ok <- identical(off_a$digest, off_b$digest)
  if (!inert_ok) {
    fail(paste0("at the shipped rate of 0 the run differs between kia_fraction 0 and 1, so the ",
                "split is consuming random draws where no event fires and the tracked seed-%d ",
                "baseline no longer describes the shipped model"),
         CHECK_SEED)
  }
  report(inert_ok, "identical runs at kia_fraction 0 and 1 (%d arrivals)", off_a$n)

  no_tags <- length(off_a$tagged) == 0
  if (!no_tags) fail("%d casualties were tagged to an event at a rate of 0", length(off_a$tagged))
  report(no_tags, "no casualty is tagged to an event")
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All mass casualty killed-share checks passed.\n")
quit(status = 0)
