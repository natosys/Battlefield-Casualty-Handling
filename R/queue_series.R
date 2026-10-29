##############################################
## R/queue_series.R                         ##
## Pool queue as a step function of time    ##
##############################################
#
# The resource monitor records each bed's queue separately, so a pool's total
# queue is in none of its rows and cannot be read off any one of them. These
# functions recover it, summarise it over bins, and report whether it ever
# cleared. Base R only and independent of every other module, so both
# R/analysis.R and R/long_horizon.R source it: the campaign figures of
# docs/Results.md and the long-horizon protocol's block series then
# measure the same quantity by the same estimator rather than by two
# implementations that can drift apart.

#' Overflow points of care the analysis reports a census for
#'
#' @details One pattern per point of care, matching every team's overflow
#'   resource (named by `OVERFLOW_RESOURCE_FMT`, R/constants.R). A casualty is
#'   placed in overflow when neither the bed its care calls for nor a holding bed
#'   is free, and overflow has no capacity to queue behind, so the quantity
#'   reported for it is its census, the number of casualties in it, read from the
#'   server count by the same estimator as a bed pool's queue
#'   (pool_queue_steps()). An overflow point no casualty reached has no monitor
#'   row, and its census is zero rather than absent.
OVERFLOW_POOLS <- c(
  "R2B reception overflow"      = "^overflow_r2b_reception_[0-9]+$",
  "R2E post-operative overflow" = "^overflow_r2e_post_op_[0-9]+$",
  "R2E pre-flight overflow"     = "^overflow_r2e_pre_flight_[0-9]+$",
  "R2E staging overflow"        = "^overflow_r2e_staging_[0-9]+$"
)

#' Mean overflow census of one point of care over a series of bins
#'
#' @param resources Resource-monitor rows carrying resource, time and server.
#' @param pattern One of `OVERFLOW_POOLS`.
#' @param edges Bin edges in minutes.
#' @return The time-weighted mean census in each bin, zero throughout where no
#'   casualty reached the point.
overflow_census_bin_means <- function(resources, pattern, edges) {
  rows <- resources[grepl(pattern, resources$resource), ]
  if (nrow(rows) == 0) return(rep(0, length(edges) - 1L))
  step_bin_means(pool_queue_steps(rows$resource, rows$time, rows$server), edges)
}

#' Response column naming each overflow point's census in a per-replication row
OVERFLOW_RESPONSES <- c(
  "R2B reception overflow"      = "overflow_r2b_reception",
  "R2E post-operative overflow" = "overflow_r2e_post_op",
  "R2E pre-flight overflow"     = "overflow_r2e_pre_flight",
  "R2E staging overflow"        = "overflow_r2e_staging"
)

#' Mean overflow census of every point of care over one window
#'
#' @param resources Resource-monitor rows for one replication, carrying
#'   resource, time and server.
#' @param from Start of the window in minutes.
#' @param to End of the window in minutes.
#' @return One-row data frame with one column per `OVERFLOW_RESPONSES` entry,
#'   the time-weighted mean census over the window.
#'
#' @details The columns an experiment's per-replication row gains so that
#'   every runner reports overflow by the same names and the same estimator.
overflow_window_census <- function(resources, from, to) {
  values <- vapply(names(OVERFLOW_POOLS), function(pool) {
    overflow_census_bin_means(resources, OVERFLOW_POOLS[[pool]], c(from, to))
  }, numeric(1))
  as.data.frame(as.list(setNames(values, OVERFLOW_RESPONSES[names(OVERFLOW_POOLS)])))
}

#' Number of beds established in one resource pool
#'
#' @param elms A built configuration's resource identifiers, as
#'   `build_environment()` returns them: `elms` for beds or `transports` for
#'   vehicles, each with its identifiers already expanded.
#' @param pattern Resource-name pattern selecting the pool's beds.
#' @return Integer count of established beds matching `pattern`.
#'
#' @details Counted from the configuration rather than from the resource
#'   monitor, which carries no row for a bed that was never seized: dividing by
#'   the beds the monitor happens to report drops every idle bed from the
#'   denominator and overstates occupancy wherever the establishment exceeds
#'   the realised peak. Every bed is registered at a capacity of one, so the
#'   count is the pool's capacity.
pool_establishment <- function(elms, pattern) {
  sum(grepl(pattern, unlist(elms, use.names = FALSE)))
}

#' Total queue across one resource pool as a step function of time
#'
#' @param resource Character vector naming the monitored bed each row belongs to.
#' @param time Numeric vector of the times the rows were recorded at.
#' @param queue Numeric vector of each row's queue depth.
#' @return A list of `time` and `total`, the instants at which the pool's total
#'   queue changed and its value from each, in increasing time order.
#'
#' @details Recovered by differencing each bed's own series into changes and
#'   accumulating those changes in time order, which is exact rather than
#'   interpolated: the total after any event is the sum of the values every bed
#'   most recently reported. Events at coinciding times collapse to the last, so
#'   the returned series is a function of time, and a leading zero is supplied
#'   where the monitor's first row is not at time zero.
pool_queue_steps <- function(resource, time, queue) {
  ord      <- order(resource, time)
  resource <- resource[ord]
  time     <- time[ord]
  queue    <- queue[ord]

  first_of_bed <- !duplicated(resource)
  previous     <- c(0, queue[-length(queue)])
  previous[first_of_bed] <- 0
  delta <- queue - previous

  ord2  <- order(time)
  time  <- time[ord2]
  total <- cumsum(delta[ord2])

  keep  <- c(diff(time) > 0, TRUE)
  time  <- time[keep]
  total <- total[keep]

  if (length(time) == 0 || time[1] > 0) {
    time  <- c(0, time)
    total <- c(0, total)
  }
  list(time = time, total = total)
}

#' Time-weighted mean of a step function over each of a series of bins
#'
#' @param steps Step function as returned by pool_queue_steps().
#' @param edges Increasing bin edges, in minutes; n edges give n - 1 bins.
#' @return Numeric vector of length `length(edges) - 1`, the mean value of the
#'   step function over each bin.
#'
#' @details Computed from the cumulative integral rather than by splitting each
#'   segment at every bin edge it crosses. The integral of a step function is
#'   piecewise linear with knots at the step times, so evaluating it at the bin
#'   edges by linear interpolation is exact, and the bin mean is the difference
#'   between consecutive edge values divided by the bin width. Sampling the step
#'   function at the edges instead would report whatever the queue happened to
#'   be at one instant per bin and would miss a peak falling between two of them.
step_bin_means <- function(steps, edges) {
  horizon <- edges[length(edges)]
  keep    <- steps$time <= horizon
  t       <- c(steps$time[keep], horizon)
  v       <- steps$total[keep]
  integral <- c(0, cumsum(v * diff(t)))
  at_edges <- approx(t, integral, xout = edges, method = "linear", rule = 2)$y
  diff(at_edges) / diff(edges)
}

#' Share of the window a step function spent at zero, and its longest unbroken
#' run above zero
#'
#' @param steps Step function as returned by pool_queue_steps().
#' @param horizon End of the observation window, in minutes.
#' @return Named numeric vector of `zero_share` (0 to 1) and `longest_busy_min`.
#'
#' @details These are the two statistics that separate a queue recurring in
#'   peaks from a standing backlog, and they are computed from the unbinned step
#'   function so that neither can be hidden by the bin width a figure is drawn
#'   at. A pool that never queues returns a zero share of one and a longest run
#'   of zero.
step_clearance_stats <- function(steps, horizon) {
  keep <- steps$time <= horizon
  t    <- c(steps$time[keep], horizon)
  dur  <- diff(t)
  busy <- steps$total[keep] > 0

  zero_share <- sum(dur[!busy]) / horizon
  longest    <- 0
  if (any(busy)) {
    runs   <- rle(busy)
    ends   <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    spans  <- mapply(function(a, b) sum(dur[a:b]), starts, ends)
    longest <- max(spans[runs$values])
  }
  c(zero_share = zero_share, longest_busy_min = longest)
}
