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
# docs/Multi_Run_Analysis.md and the long-horizon protocol's block series then
# measure the same quantity by the same estimator rather than by two
# implementations that can drift apart.

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
