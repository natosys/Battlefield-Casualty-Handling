#!/usr/bin/env Rscript
##############################################################################
## scripts/check_long_horizon_protocol.R                                    ##
## Regression check — the long-horizon protocol's parameters, its reduction ##
## and its published series agree with one another                          ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_long_horizon_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The sustained-operations horizon is documented in
# docs/Multi_Run_Supplement.md as a duration, a replication count and a block
# length, and it is executed from constants in R/long_horizon.R. Nothing held
# the two together, so a protocol parameter changed in code would leave the
# supplement describing an experiment the project no longer runs, and a tracked
# series measured under the old parameters would go on being quoted under the
# new ones.
#
# The reduction needs defending for a second reason. A long run's monitoring
# data is discarded inside the worker that produced it, so the reduced series is
# the only record of the run: an error in the reduction cannot be found later by
# re-reading the monitors, because there are none to re-read. The check
# therefore exercises the reduction against a run whose answers are known from
# the monitors directly, rather than only checking that its output has the right
# shape.
#
# What this asserts:
#
#   1. Every protocol parameter in R/long_horizon.R equals the value the
#      supplement documents.
#   2. The tracked series carries that duration, that replication count and
#      every response and scenario the protocol names.
#   3. The daily reduction agrees with the monitors it was reduced from: pool
#      occupancy and queue reproduce what the resource monitor reports over the
#      same window, and the daily arrival counts sum to the run's arrivals.
#   4. The reduction of any day uses that day's events alone, so reducing one
#      run's monitors over a shorter horizon reproduces the shorter horizon's
#      days exactly. That is what makes twelve block means from one run
#      legitimate, and it is asserted alongside the fact that a long run and a
#      short one at the same seed are NOT the same campaign: the arrival stream
#      is drawn over the requested horizon, so two runs of different length at
#      one seed diverge from their first arrival.
#   5. block_means() averages over the block rather than sampling it, and
#      carries the replication count into its interval.
#
# Assertion 4 is the one the protocol's economy rests on. An earlier draft of
# this check asserted the stronger and false property, that block one of a long
# run equals a standalone run of one block, and failed on it. The reduction also
# clamped events beyond the horizon into the final day, which made a day's count
# depend on what happened after it; that is fixed, and this assertion is what
# would catch its return.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/long_horizon.R")

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
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

#' The document the protocol's parameters are published in
SUPPLEMENT_PATH <- file.path("docs", "Multi_Run_Supplement.md")

#' Directory holding the tracked long-horizon evidence set
SERIES_DIR <- file.path("data", "long_horizon")

#' Scenario profile the reduction is exercised against
#'
#' @details The moderate profile, which is the cheaper of the two at a horizon
#'   long enough to exercise the reduction and short enough for a per-PR gate.
CHECK_SCENARIO <- "moderate_intensity"

#' Campaign length the reduction is exercised over, in days
#'
#' @details Two blocks, so that the block-one equivalence of assertion 4 is a
#'   comparison between different run lengths rather than between a run and
#'   itself.
CHECK_DAYS <- 4L

#' Block length the reduction is exercised at, in days
CHECK_BLOCK_DAYS <- 2L

#' Control seed every measurement here is made under
CHECK_SEED <- 42L

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

# ── 1. The code's parameters are the ones the supplement documents ───────────

cat("\n-- the protocol's parameters match the supplement --\n")

supplement <- paste(readLines(SUPPLEMENT_PATH, warn = FALSE), collapse = "\n")

#' Read one protocol parameter the supplement states in a marker comment
#'
#' @param name Marker name, as it appears after "PROTOCOL ".
#' @return The numeric value the marker carries, or NA where absent.
#'
#' @details The parameters are marked rather than parsed out of the prose,
#'   because a check that guesses which number in a paragraph is the replication
#'   count fails for reasons that have nothing to do with the protocol. The
#'   marker is the author's statement of what the sentence beneath it claims.
protocol_value <- function(name) {
  m <- regmatches(supplement,
                  regexpr(sprintf("<!-- PROTOCOL %s=([0-9]+) -->", name), supplement))
  if (length(m) == 0) return(NA_real_)
  as.numeric(gsub("[^0-9]", "", m))
}

for (param in list(list("days", LONG_HORIZON_DAYS),
                   list("replications", LONG_HORIZON_REPLICATIONS),
                   list("block_days", LONG_HORIZON_BLOCK_DAYS))) {
  stated <- protocol_value(param[[1]])
  report(!is.na(stated) && stated == param[[2]],
         "the supplement states %s = %s and the code holds %s",
         param[[1]], format(stated), format(param[[2]]))
}

# ── 2. The tracked series is what the protocol says it is ────────────────────

cat("\n-- the tracked series matches the protocol --\n")

series_path <- file.path(SERIES_DIR, "long_horizon_series.csv.gz")
blocks_path <- file.path(SERIES_DIR, "long_horizon_blocks.csv")

if (!file.exists(series_path) || !file.exists(blocks_path)) {
  report(FALSE, "the tracked long-horizon evidence set exists at %s", SERIES_DIR)
} else {
  series <- read.csv(series_path, stringsAsFactors = FALSE)
  blocks <- read.csv(blocks_path, stringsAsFactors = FALSE)

  report(max(series$day) == LONG_HORIZON_DAYS,
         "the tracked series runs to the protocol's %d days (found %d)",
         LONG_HORIZON_DAYS, max(series$day))
  report(length(unique(series$replication)) == LONG_HORIZON_REPLICATIONS,
         "the tracked series carries %d replications (found %d)",
         LONG_HORIZON_REPLICATIONS, length(unique(series$replication)))
  report(setequal(unique(series$subject),
                  c(names(LONG_HORIZON_POOLS), "system")),
         "the tracked series carries every pool the protocol names, and the system")
  report(setequal(unique(series$series),
                  c("mean_queue", "occupancy", "arrivals", "dow", "evac_backlog")),
         "the tracked series carries every response the protocol names")
  report(max(blocks$block) == LONG_HORIZON_DAYS %/% LONG_HORIZON_BLOCK_DAYS,
         "the tracked block means carry %d blocks (found %d)",
         LONG_HORIZON_DAYS %/% LONG_HORIZON_BLOCK_DAYS, max(blocks$block))
  report(all(blocks$n_reps == LONG_HORIZON_REPLICATIONS),
         "every block mean is taken over all %d replications",
         LONG_HORIZON_REPLICATIONS)
}

# ── 3. The reduction agrees with the monitors it reduced ─────────────────────

cat("\n-- the daily reduction agrees with the monitors --\n")

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
apply_config_globals(resolve_scenario(json_data, CHECK_SCENARIO))

set.seed(CHECK_SEED)
long_env <- run_once(CHECK_DAYS, seed = CHECK_SEED)
reduced  <- reduce_long_replication(long_env, CHECK_DAYS)

resources <- simmer::get_mon_resources(long_env)
arrivals  <- simmer::get_mon_arrivals(long_env, ongoing = TRUE)

report(nrow(reduced) > 0, "the reduction returns a series (%d rows)", nrow(reduced))

daily_arrivals <- reduced[reduced$series == "arrivals", ]
report(sum(daily_arrivals$value) == sum(arrivals$start_time < CHECK_DAYS * DAY_MIN),
       "the daily arrival counts sum to the run's arrivals (%d)",
       sum(daily_arrivals$value))

#' Time-weighted mean of one pool's queue over the whole run, from the monitor
#'
#' @param pattern Regular expression matching the pool's monitored beds.
#' @return The pool's mean queue over the run, or NA where it fields no beds.
monitor_mean_queue <- function(pattern) {
  rows <- resources[grepl(pattern, resources$resource), ]
  if (nrow(rows) == 0) return(NA_real_)
  steps <- pool_queue_steps(rows$resource, rows$time, rows$queue)
  step_bin_means(steps, c(0, CHECK_DAYS * DAY_MIN))
}

for (pool in names(LONG_HORIZON_POOLS)) {
  from_series <- mean(reduced$value[reduced$series == "mean_queue" &
                                      reduced$subject == pool])
  from_monitor <- monitor_mean_queue(LONG_HORIZON_POOLS[[pool]])
  report(!is.na(from_monitor) && abs(from_series - from_monitor) < TOL,
         "%s: the mean of the daily queues equals the monitor's run mean (%.6f)",
         pool, from_monitor)
}

occupancy <- reduced$value[reduced$series == "occupancy"]
report(all(occupancy >= -TOL & occupancy <= 1 + TOL),
       "every daily occupancy lies in [0, 1]")

# ── 4. The reduction of a day uses only that day's events ────────────────────

cat("\n-- each day's reduced values depend only on that day and those before --\n")

# The economy of the protocol rests on this and not on realisation identity. A
# long run's block one is NOT the same campaign as a standalone 30-day run at
# the same seed: the arrival streams are force-size-reactive closures sampled by
# thinning over the requested horizon, so changing the horizon changes the draws
# and the two runs diverge from their first arrival. What does hold, and what
# makes twelve block means from one run legitimate, is that the reduction of any
# day uses that day's events alone. Reducing one run's monitors over a shorter
# horizon must therefore reproduce the shorter run's days exactly.
resources_early <- resources[resources$time <= CHECK_BLOCK_DAYS * DAY_MIN, ]
pools_full  <- reduce_pool_series(resources, CHECK_DAYS)
pools_early <- reduce_pool_series(resources_early, CHECK_BLOCK_DAYS)

#' Identify each reduced pool row by the day and pool it reports
#'
#' @param df A reduced pool series.
#' @return Character vector keying each row, for matching one reduction's rows
#'   against another's.
pool_key <- function(df) paste(df$day, df$pool)
shared   <- intersect(pool_key(pools_full), pool_key(pools_early))
full_q   <- pools_full$mean_queue[match(shared, pool_key(pools_full))]
early_q  <- pools_early$mean_queue[match(shared, pool_key(pools_early))]

report(length(shared) > 0 && all(abs(full_q - early_q) < TOL),
       "every one of the %d early-day pool queues is unchanged by the later days (max diff %.2e)",
       length(shared),
       if (length(shared) > 0) max(abs(full_q - early_q)) else NA_real_)

flow_full  <- reduce_flow_series(arrivals, simmer::get_mon_attributes(long_env), CHECK_DAYS)
flow_early <- reduce_flow_series(arrivals, simmer::get_mon_attributes(long_env),
                                 CHECK_BLOCK_DAYS)
early_days <- seq_len(CHECK_BLOCK_DAYS)
report(all(flow_full$arrivals[early_days] == flow_early$arrivals[early_days]),
       "the early-day arrival counts are unchanged by the later days")

# And the property that would make the identity claim true, asserted as false so
# that nobody reinstates it from the shape of the block means: a long run and a
# short one at one seed are different campaigns, which is why the protocol
# compares block means across replications rather than runs against one another.
set.seed(CHECK_SEED)
short_env      <- run_once(CHECK_BLOCK_DAYS, seed = CHECK_SEED)
short_arrivals <- simmer::get_mon_arrivals(short_env, ongoing = TRUE)
early_long  <- sort(arrivals$start_time[arrivals$start_time < CHECK_BLOCK_DAYS * DAY_MIN])
early_short <- sort(short_arrivals$start_time[short_arrivals$start_time <
                                                CHECK_BLOCK_DAYS * DAY_MIN])
report(!isTRUE(all.equal(early_long, early_short)),
       "a long run and a short one at one seed are different campaigns, as documented (%d against %d early arrivals)",
       length(early_long), length(early_short))

# ── 5. block_means() averages the block rather than sampling it ──────────────

cat("\n-- block_means() averages over the block --\n")

synthetic <- do.call(rbind, lapply(1:3, function(rep_id) {
  data.frame(replication = rep_id, day = 1:4, series = "x", subject = "system",
             value = c(0, 10, 2, 4) + rep_id)
}))
b <- block_means(synthetic, block_days = 2L)

report(nrow(b) == 2, "two blocks of two days give two rows (found %d)", nrow(b))
report(abs(b$mean[b$block == 1] - 7) < TOL,
       "block one's mean is the mean of its days across replications (7)")
report(abs(b$mean[b$block == 2] - 5) < TOL,
       "block two's mean is the mean of its days across replications (5)")
report(all(b$n_reps == 3), "each block records the replications behind it (3)")
report(all(b$ci_lower < b$mean & b$mean < b$ci_upper),
       "each block's interval brackets its mean")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All long-horizon protocol checks passed.\n")
quit(status = 0)
