##############################################
## R/censoring.R                            ##
## Right-censored interval estimation       ##
##############################################

# An interval measured inside a finite observation window can still be open
# when that window closes, and the ones still open are not a random subset:
# they are the casualties still present, which is both the late arrivals and
# the long stayers. Differencing a start attribute against an end attribute
# and keeping the rows that have both therefore biases every such mean low,
# and by more the longer the interval being measured.
#
# The estimators here treat an open interval as the lower bound it is. They
# are base R and depend on no other module, so R/analysis.R and R/sensitivity.R
# both source them and a KPI and the screened response of the same name cannot
# drift apart.

source("R/constants.R")

#' Horizon the interval estimators restrict their means to, in minutes
#'
#' @details A restricted mean is only as good as the data supporting it. Beyond
#'   the last observed completion the survival curve can only be held flat, and
#'   the area under that flat tail is extrapolation rather than measurement: at
#'   the shipped configuration and seed 42, restricting to the 30-day window
#'   instead would put 63% of the reported R2B dwell beyond the last completion
#'   at day 9, where the curve plateaus and never moves again.
#'
#'   Seven days is inside the observed support of both dwell intervals, so
#'   neither estimate rests on that tail, and it is the planning cycle the
#'   model already runs on: the reinforcement demand interval and the strategic
#'   aeromedical sortie interval are both seven days. It is held fixed rather
#'   than fitted to each run because a horizon that moved with the data would
#'   confound a screened response, the parameter under test shifting the
#'   horizon as well as the quantity measured against it.
#'
#'   It makes the reported figure a mean stay within seven days, not a mean
#'   stay. A casualty holding a bed for twenty days contributes seven.
INTERVAL_RESTRICTION_MIN <- 7 * DAY_MIN

#' End of the observation window, in minutes
#'
#' @param combined Arrivals joined to attributes_wide, with casualty type,
#'   population source and arrival day derived.
#' @return The window end, in minutes.
#' @details Every reconstruction that has to decide where an unfinished
#'   interval was cut needs the same window, and one that used a different one
#'   would disagree with the others about which intervals are censored at all.
#'   Derived from the last arrival rather than passed in, the monitoring frames
#'   carrying no record of the run length that produced them.
observation_window_min <- function(combined) {
  ceiling(max(combined$start_time, na.rm = TRUE) / DAY_MIN) * DAY_MIN
}

#' Kaplan-Meier product-limit survival curve for a censored duration
#'
#' @param time Observed duration, in minutes: the full duration for a closed
#'   interval, the elapsed time at the window for a censored one.
#' @param event 1 where the interval closed, 0 where it was still open.
#' @return A data frame of `t` and `s`, the survival estimate stepping down at
#'   each distinct closing time, opening at `t = 0`, `s = 1`. Empty input
#'   returns that opening row alone.
#' @details The estimator of Kaplan and Meier (1958). The curve is defined only
#'   as far as the largest observed duration; a caller reading it beyond that
#'   is holding it flat, which is a convention rather than a measurement, and
#'   censored_interval_stats() reports how much of its answer rests on it.
km_survival_curve <- function(time, event) {
  keep  <- is.finite(time) & time >= 0
  time  <- time[keep]
  event <- as.integer(event[keep])
  curve <- data.frame(t = 0, s = 1)
  surv  <- 1
  for (t in sort(unique(time[event == 1L]))) {
    n_risk <- sum(time >= t)
    surv   <- surv * (1 - sum(time == t & event == 1L) / n_risk)
    curve  <- rbind(curve, data.frame(t = t, s = surv))
  }
  curve
}

#' Kaplan-Meier restricted mean of a censored duration
#'
#' @param time See km_survival_curve().
#' @param event See km_survival_curve().
#' @param tau Restriction horizon, in minutes.
#' @return The restricted mean, in minutes, or NA where nothing was observed.
#' @details The area under the survival curve on [0, tau], which is the mean
#'   duration a casualty spends in the echelon within a campaign of that
#'   length. It uses a censored interval as the lower bound it is rather than
#'   dropping it, and reduces to the plain arithmetic mean, exactly, when
#'   nothing is censored.
#'
#'   It is restricted because the unrestricted mean is not identifiable here:
#'   the longest stays are the ones still running when the window closes, so
#'   the curve need not reach zero and the tail beyond the last observation is
#'   not estimable from the run. Reporting a horizon the reader can see is
#'   preferable to extrapolating one they cannot.
km_restricted_mean <- function(time, event, tau) {
  if (!any(is.finite(time) & time >= 0)) return(NA_real_)
  curve <- km_survival_curve(time, event)
  curve <- curve[curve$t <= tau, , drop = FALSE]
  ends  <- c(curve$t[-1], tau)
  sum(curve$s * (ends - curve$t))
}

#' Kaplan-Meier quantile of a censored duration
#'
#' @param time See km_survival_curve().
#' @param event See km_survival_curve().
#' @param p Quantile to locate, between 0 and 1.
#' @return The quantile in minutes, or NA where the curve never falls to
#'   `1 - p` and the quantile is therefore not located by the data.
#' @details Returning NA rather than the same quantile of the closed intervals
#'   alone is the point. That figure is a quantile of the casualties who
#'   finished, which under heavy censoring is a different and shorter-staying
#'   population, and presenting it as a quantile of the whole would assert a
#'   value the run cannot locate.
#'
#'   Unlike the mean it carries no restriction horizon. A quantile is a point
#'   on the curve rather than an area under it, so it is either located by the
#'   observed completions or it is not, and capping it at a horizon would
#'   report the horizon itself as though it were the measurement.
km_quantile <- function(time, event, p) {
  curve   <- km_survival_curve(time, event)
  reached <- curve$s <= 1 - p
  if (!any(reached)) return(NA_real_)
  min(curve$t[reached])
}

#' Summarise one interval that may still be open when the window closes
#'
#' @param start_min Interval start, in minutes; NA for a casualty never at risk.
#' @param end_min Interval end, in minutes; NA where it had not closed.
#' @param window_min End of the observation window, in minutes.
#' @param tau Horizon the mean is restricted to, in minutes.
#' @return A one-row data frame of `mean_min`, `p90_min`, `n`, `n_censored`,
#'   `censored_share` and `tail_share`, over every casualty who entered the
#'   interval.
#' @details The cohort is every casualty who reached the start, not those who
#'   reached both ends. A casualty still in the echelon when the run ends is
#'   right-censored, and the censored are not a random subset: they are the
#'   ones still present, which is both the late arrivals and the long stayers.
#'   Dropping them biases the mean low, and by more the longer the interval
#'   being measured.
#'
#'   The censored count and share are reported whatever the interval, including
#'   those that never censor at the shipped configuration, so a configuration
#'   that starts censoring one of them is visible rather than silent.
#'   `tail_share` reports how much of the mean lies beyond the last observed
#'   completion, where the survival curve can only be held flat; a figure that
#'   is not near zero is extrapolation and should be read as such.
censored_interval_stats <- function(start_min, end_min, window_min,
                                    tau = INTERVAL_RESTRICTION_MIN) {
  start_min <- as.numeric(start_min)
  end_min   <- as.numeric(end_min)
  at_risk   <- !is.na(start_min) & start_min <= window_min
  start_min <- start_min[at_risk]
  end_min   <- end_min[at_risk]

  closed <- !is.na(end_min) & end_min >= start_min
  time   <- ifelse(closed, end_min - start_min, window_min - start_min)
  usable <- is.finite(time) & time >= 0
  time   <- time[usable]
  closed <- closed[usable]

  event <- as.integer(closed)
  mean_min <- km_restricted_mean(time, event, tau)
  curve    <- km_survival_curve(time, event)
  last_obs <- max(curve$t)
  tail_min <- if (tau > last_obs) min(curve$s) * (tau - last_obs) else 0

  data.frame(
    mean_min       = mean_min,
    p90_min        = km_quantile(time, event, 0.90),
    n              = length(time),
    n_censored     = sum(!closed),
    censored_share = if (length(time) > 0) sum(!closed) / length(time) else NA_real_,
    tail_share     = if (is.na(mean_min) || mean_min == 0) NA_real_ else tail_min / mean_min
  )
}
