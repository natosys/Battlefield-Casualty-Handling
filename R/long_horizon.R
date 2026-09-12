##############################################
## R/long_horizon.R                         ##
## Sustained-operations replicated runner    ##
##############################################
#
# Every experiment this project publishes outside this module runs for 30 days,
# a window inherited from the campaign the baseline models rather than chosen by
# measurement. A window that short cannot tell a system in equilibrium from one
# thirty days into a divergence, and the model has responses of both kinds. This
# module runs the same model over a sustained-operations horizon and reduces
# each replication to a daily series before the next begins.
#
# The reduction is the point, not an optimisation. At the protocol's horizon and
# replication count the monitoring data is some two orders of magnitude larger
# than any analysis of it needs, and it is discarded inside the forked worker
# that produced it, so the parent process holds only the series. Depends on
# R/replication.R (run_once/dispatch_replications) and R/queue_series.R; source
# those before this file.

source("R/constants.R")
source("R/queue_series.R")

#' Length of the sustained-operations horizon, in days
#'
#' @details Twelve 30-day blocks. The duration is set by measurement rather than
#'   by assumption: `scripts/run_long_horizon.R` reports each response's mean
#'   per block, and a response whose block mean stops moving has converged
#'   within the horizon while one that does not has been shown not to. A single
#'   run of this length answers the question for every shorter horizon too,
#'   because the model is causal and a block's mean cannot depend on what
#'   happens after it.
LONG_HORIZON_DAYS <- 360L

#' Replications the protocol runs per configuration
#'
#' @details Thirty, which is the count `docs/Multi_Run_Supplement.md` derives
#'   for a block-level mean and the count the strategic airlift measurement this
#'   protocol supersedes was made at.
LONG_HORIZON_REPLICATIONS <- 30L

#' Length of the block the protocol's stability statement is made over, in days
#'
#' @details Thirty days, so that block one of a long run covers exactly the
#'   window every 30-day experiment in this project measures and the two can be
#'   compared directly.
LONG_HORIZON_BLOCK_DAYS <- 30L

#' Resource pools the protocol reports a queue and an occupancy for
#'
#' @details The same pools `TIME_SERIES_POOLS` (R/analysis.R) covers, plus the
#'   two R2E clinical pools' upstream theatre. A pool is named here by the
#'   regular expression matching its individual monitored beds; the patterns
#'   anchor on the bed-index and team suffixes so that a pool cannot match
#'   another whose name it is a prefix of.
LONG_HORIZON_POOLS <- c(
  "R2B holding beds"       = "^b_r2b_hold_[0-9]+_t[0-9]+$",
  "R2B operating theatres" = "^b_r2b_ot_[0-9]+_t[0-9]+$",
  "R2E operating theatres" = "^b_r2eheavy_ot_[0-9]+_t[0-9]+$",
  "R2E intensive care"     = "^b_r2eheavy_icu_[0-9]+_t[0-9]+$",
  "R2E holding beds"       = "^b_r2eheavy_hold_[0-9]+_t[0-9]+$"
)

#' Daily queue and occupancy for every pool of one replication
#'
#' @param resources Resource-monitor rows for one replication.
#' @param n_days Campaign length in days.
#' @return Data frame of pool, day, mean_queue and occupancy, one row per pool
#'   and day.
#'
#' @details Both quantities are time-weighted means over the day rather than
#'   samples at its boundary, and occupancy is the pool's total in-use server
#'   count divided by its total capacity, so it reads as the fraction of the
#'   establishment in use rather than as a count.
reduce_pool_series <- function(resources, n_days) {
  edges <- seq(0, n_days * DAY_MIN, by = DAY_MIN)
  do.call(rbind, lapply(names(LONG_HORIZON_POOLS), function(pool) {
    rows <- resources[grepl(LONG_HORIZON_POOLS[[pool]], resources$resource), ]
    if (nrow(rows) == 0) return(NULL)
    queue  <- pool_queue_steps(rows$resource, rows$time, rows$queue)
    server <- pool_queue_steps(rows$resource, rows$time, rows$server)
    capacity <- sum(tapply(rows$capacity, rows$resource, max))
    data.frame(
      pool       = pool,
      day        = seq_len(n_days),
      mean_queue = step_bin_means(queue, edges),
      occupancy  = step_bin_means(server, edges) / capacity
    )
  }))
}

#' Daily casualty, mortality and evacuation counts for one replication
#'
#' @param arrivals Arrival-monitor rows for one replication.
#' @param attributes Attribute-monitor rows for one replication.
#' @param n_days Campaign length in days.
#' @return Data frame of day, arrivals, dow and evac_backlog, one row per day.
#'
#' @details `evac_backlog` is the number of casualties whose evacuation had been
#'   decided by the end of the day and who had not yet departed, which is the
#'   quantity a strategic lift plan is sized against; it is a level at the end of
#'   the day rather than a count over it, so a day on which nothing happened
#'   carries the previous day's value rather than a zero.
reduce_flow_series <- function(arrivals, attributes, n_days) {
  #' Campaign day a simulated time falls in
  #'
  #' @param minutes Simulated times, in minutes from the start of the campaign.
  #' @return Integer day numbers from 1, unclamped, so a time beyond the horizon
  #'   yields a day beyond it rather than being folded into the last one.
  day_of <- function(minutes) floor(minutes / DAY_MIN) + 1L

  #' Count of events falling in each campaign day
  #'
  #' @param minutes Simulated times of the events, in minutes.
  #' @return Integer vector of length `n_days`, zero on a day with no events.
  #'
  #' @details Events beyond the horizon are dropped rather than clamped into the
  #'   final day. Clamping would make a day's count depend on what happened
  #'   after it, so reducing one run's monitors over a shorter horizon would not
  #'   reproduce the shorter horizon's days, which is the property that makes
  #'   twelve block means from one run legitimate.
  per_day <- function(minutes) {
    days <- day_of(minutes[!is.na(minutes)])
    tabulate(days[days >= 1L & days <= n_days], nbins = n_days)
  }

  #' Times at which one attribute was set
  #'
  #' @param key Attribute key to select.
  #' @return The times the attribute was set, or a vector of NA where no
  #'   casualty set it, so a campaign that never reached a decision reduces to
  #'   an empty series rather than failing.
  value_at <- function(key) {
    rows <- attributes[attributes$key == key, ]
    if (nrow(rows) == 0) return(rep(NA_real_, n_days))
    rows$time
  }

  decided  <- cumsum(per_day(value_at("evacuation_decision_day")))
  departed <- cumsum(per_day(value_at("ame_departure_time")))

  data.frame(
    day          = seq_len(n_days),
    arrivals     = per_day(arrivals$start_time),
    dow          = per_day(attributes$time[attributes$key == "dow" &
                                             attributes$value == 1]),
    evac_backlog = decided - departed
  )
}

#' Reduce one finished replication to the protocol's daily series
#'
#' @param env Wrapped simmer environment as returned by run_once().
#' @param n_days Campaign length in days.
#' @return Data frame of day, series, subject and value in long form, one row
#'   per day and reported quantity.
#'
#' @details Long form rather than one column per quantity, because the pool
#'   series carries one value per pool per day and the flow series one value per
#'   day, and a single shape lets the caller bind replications and block them
#'   without knowing which quantities a run happened to produce.
reduce_long_replication <- function(env, n_days) {
  resources  <- simmer::get_mon_resources(env)
  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  attributes <- simmer::get_mon_attributes(env)

  pools <- reduce_pool_series(resources, n_days)
  flow  <- reduce_flow_series(arrivals, attributes, n_days)

  #' Reshape one reduced frame into the module's long form
  #'
  #' @param df Reduced frame carrying a day column and one column per quantity.
  #' @param id_col Name of the column identifying the subject, or NULL where the
  #'   frame's quantities describe the system as a whole.
  #' @return Data frame of day, series, subject and value.
  long <- function(df, id_col) {
    value_cols <- setdiff(names(df), c("day", id_col))
    do.call(rbind, lapply(value_cols, function(v) {
      data.frame(day = df$day, series = v,
                 subject = if (is.null(id_col)) "system" else df[[id_col]],
                 value = df[[v]])
    }))
  }
  rbind(long(pools, "pool"), long(flow, NULL))
}

#' Run a replicated campaign over a long horizon, reducing as it goes
#'
#' @param n_iterations Replications to run (default LONG_HORIZON_REPLICATIONS).
#' @param n_days Campaign length in days (default LONG_HORIZON_DAYS).
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @return Data frame of replication, day, series, subject and value.
#'
#' @details Each replication's monitoring data is reduced inside the forked
#'   worker that produced it and discarded there, so the parent holds only the
#'   series and peak memory is set by one replication rather than by their sum.
#'   The seeds are drawn exactly as `run_replications()` draws them, under the
#'   caller's generator kind and from the caller's control seed, and the
#'   caller's stream is restored on exit, so a long run at a control seed is
#'   reproducible on the same terms as every other measurement in this project.
run_long_horizon <- function(n_iterations = LONG_HORIZON_REPLICATIONS,
                             n_days = LONG_HORIZON_DAYS, max_cores = NULL) {
  message(sprintf("Running %d replications over %d days, reducing as they finish...",
                  n_iterations, n_days))

  rng_state <- capture_rng_state()
  on.exit(restore_rng_state(rng_state), add = TRUE)

  rep_seeds <- sample.int(.Machine$integer.max, n_iterations)
  RNGkind("L'Ecuyer-CMRG")

  #' Run one replication and return its reduced series alone
  #'
  #' @param i Index of the replication, into `rep_seeds`.
  #' @return The replication's reduced series, carrying its index.
  worker <- function(i) {
    env <- run_once(n_days, seed = rep_seeds[i], write_files = FALSE)
    out <- reduce_long_replication(env, n_days)
    out$replication <- i
    out
  }

  dispatched <- dispatch_replications(worker, n_iterations, max_cores)
  usable     <- vapply(dispatched, is.data.frame, logical(1))
  if (!all(usable)) {
    stop(sprintf("%d of %d replications did not complete", sum(!usable), n_iterations),
         call. = FALSE)
  }
  do.call(rbind, dispatched)
}

#' Mean of each response over each block of the campaign, across replications
#'
#' @param series Long-horizon series as returned by run_long_horizon().
#' @param block_days Block length in days (default LONG_HORIZON_BLOCK_DAYS).
#' @return Data frame of series, subject, block, block_start_day, n_reps, mean,
#'   ci_lower and ci_upper.
#'
#' @details A level series (one whose value is a state at the end of the day
#'   rather than a count over it) and a rate series are both averaged over the
#'   block's days, which is the quantity a stability statement is made about:
#'   a response whose block mean stops moving has converged within the horizon.
#'   The interval is the usual t-distribution interval across replications,
#'   which is available here because `run_long_horizon()` makes the replications
#'   independent in exactly the way `run_replications()` does.
block_means <- function(series, block_days = LONG_HORIZON_BLOCK_DAYS) {
  series$block <- floor((series$day - 1L) / block_days) + 1L

  per_rep <- aggregate(value ~ replication + series + subject + block,
                       data = series, FUN = mean)

  stats <- aggregate(value ~ series + subject + block, data = per_rep,
                     FUN = function(x) {
                       n <- length(x)
                       m <- mean(x)
                       s <- if (n > 1) sd(x) else 0
                       e <- if (n > 1) qt(0.975, df = n - 1) * s / sqrt(n) else 0
                       c(n = n, mean = m, lower = m - e, upper = m + e)
                     })
  out <- data.frame(stats[, c("series", "subject", "block")], stats$value)
  names(out) <- c("series", "subject", "block", "n_reps", "mean", "ci_lower", "ci_upper")
  out$block_start_day <- (out$block - 1L) * block_days + 1L
  out[order(out$series, out$subject, out$block), ]
}

#' Smallest per-block drift, as a share of a response's own level, that is
#' reported as a trend rather than as flat
#'
#' @details One percent of the response's mean per 30-day block, which is 12%
#'   over the protocol's horizon. A slope whose interval excludes zero but whose
#'   size is below this is reported as flat, because a response the model holds
#'   within a few percent over a simulated year is in equilibrium for every
#'   planning purpose, and at thirty replications an interval can exclude zero
#'   at a drift far too small to matter. Statistical significance and practical
#'   stability are different questions and this threshold is where the second is
#'   answered.
STABILITY_DRIFT_THRESHOLD <- 0.01

#' Half-width of the band a converged response must stay inside to count as
#' settled, in standard deviations of its own late block means
#'
#' @details Two. The band is scaled to the response's own between-block
#'   variation rather than to its level, because the question it answers is
#'   whether the opening blocks are distinguishable from the ordinary variation
#'   the response shows once settled, and a band fixed as a share of the level
#'   answers that question differently for a queue averaging half a casualty
#'   than for one averaging fifty. A response settled by block one had no
#'   opening transient worth removing, which is the finding a warm-up period
#'   would otherwise be read from.
STABILITY_SETTLE_SDS <- 2

#' Classify each response as converged, drifting or degenerate over the horizon
#'
#' @param blocks Per-block means as returned by block_means().
#' @return Data frame of series, subject, first, last, late_mean, slope,
#'   ci_lower, ci_upper, drift_per_block, settles_by_block and stability, one
#'   row per response.
#'
#' @details The classification is the protocol's finding, so it is computed here
#'   rather than read off a figure.
#'
#'   The trend is fitted over the second half of the horizon rather than over
#'   all of it, because a response that decays from an initial transient to a
#'   steady level and one that grows without bound both have a non-zero slope
#'   across the whole horizon and are the two cases this classification exists
#'   to separate. A run starts from an empty system, so a decaying opening is
#'   expected and is not evidence against convergence; what distinguishes the
#'   two is whether the response is still moving once that opening has passed.
#'
#'   `stability` is "degenerate" where the response never moved, which carries
#'   no information about convergence and would otherwise be reported as a
#'   perfectly determined zero trend; "converged" where the late slope's
#'   interval spans zero, or where it does not but the drift is under
#'   STABILITY_DRIFT_THRESHOLD of the response's own late level; and "drifting"
#'   otherwise. A drifting response has no steady state within the horizon, and
#'   no warm-up period can be defined for it.
#'
#'   `settles_by_block` is the first block from which every later block mean
#'   lies within STABILITY_SETTLE_SDS standard deviations of the late mean,
#'   the deviation being the response's own across the late blocks, and is NA for a
#'   drifting or degenerate response. For a converged response it is the
#'   quantity a warm-up period would be read from.
classify_stability <- function(blocks) {
  parts <- split(blocks, list(blocks$series, blocks$subject), drop = TRUE)
  out <- do.call(rbind, lapply(parts, function(d) {
    d <- d[order(d$block), ]
    n <- nrow(d)

    #' One classified row for this response
    #'
    #' @param slope Fitted late slope.
    #' @param ci Two-element interval on that slope.
    #' @param drift Late slope as a share of the late level.
    #' @param settles First block from which the response stays in band, or NA.
    #' @param stability One of "degenerate", "converged" or "drifting".
    #' @return A one-row data frame.
    row_for <- function(slope, ci, drift, settles, stability) {
      data.frame(series = d$series[1], subject = d$subject[1],
                 first = d$mean[1], last = d$mean[n],
                 late_mean = mean(d$mean[ceiling(n / 2):n]),
                 slope = slope, ci_lower = ci[1], ci_upper = ci[2],
                 drift_per_block = drift, settles_by_block = settles,
                 stability = stability)
    }

    if (isTRUE(all.equal(max(d$mean), min(d$mean)))) {
      return(row_for(0, c(0, 0), 0, NA_integer_, "degenerate"))
    }

    late  <- d[ceiling(n / 2):n, ]
    level <- mean(late$mean)

    # A response that varied early and is constant over the late blocks has a
    # perfectly determined zero slope, which lm() fits with a zero residual and
    # warns about. It has converged, to that constant, and settled by the first
    # late block; reporting it here avoids both the warning and an interval of
    # width zero that would read as a precise measurement rather than as a
    # degenerate one.
    if (isTRUE(all.equal(max(late$mean), min(late$mean)))) {
      return(row_for(0, c(0, 0), 0, as.integer(ceiling(n / 2)), "converged"))
    }

    fit   <- lm(mean ~ block, data = late)
    ci    <- unname(confint(fit)["block", ])
    slope <- unname(coef(fit)["block"])
    drift <- slope / max(abs(level), .Machine$double.eps)

    converged <- (ci[1] < 0 && ci[2] > 0) || abs(drift) < STABILITY_DRIFT_THRESHOLD
    if (!converged) return(row_for(slope, ci, drift, NA_integer_, "drifting"))

    # The first block from which the response never again leaves the band
    # around its late level. Searched from the last block backwards, so a
    # response that re-enters the band and leaves it again reports the later
    # crossing rather than the earlier one.
    spread  <- sd(late$mean)
    if (!is.finite(spread) || spread == 0) spread <- .Machine$double.eps
    in_band <- abs(d$mean - level) <= STABILITY_SETTLE_SDS * spread
    settles <- n
    while (settles > 1 && in_band[settles - 1]) settles <- settles - 1

    row_for(slope, ci, drift, as.integer(settles), "converged")
  }))
  out[order(out$stability, -abs(out$drift_per_block)), ]
}
