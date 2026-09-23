##############################################################################
## R/hold_window.R                                                          ##
## The R2B pre-open hold window, at its two arms                            ##
##############################################################################
#
# r2b.surgery.pre_open_window_min lets a casualty who arrives while the R2B
# surgical section is off shift be held forward for it, rather than diverted
# to R2E, when the section is due back within the window. This module
# measures what that hold buys and costs: casualties kept forward, diversions
# it avoids, and what changes downstream at R2E, comparing the shipped
# 60-minute window against a window of zero, which reproduces the
# instant-diversion model exactly (scripts/check_pre_open_window.R asserts
# that reproduction; this module measures the populated comparison rather than
# the mechanism).
#
# The two arms are not the same realisation. Introducing the hold shifts
# simmer's single global random stream from the first hold onward, so the two
# arms drift into different casualty streams rather than sharing one; pairing
# on a common control seed removes none of the between-run variance this
# implies (docs/Multi_Run_Supplement.md, "The R2B Pre-Open Hold Window"). Each
# replication is reduced to one row of the response set inside the forked
# worker that produced it, on the arrangement R/policy_sweep.R and R/airlift.R
# both use.
#
# Depends on R/analysis.R for build_attributes_wide(); source that before this
# file.

#' Replications per arm
HOLD_WINDOW_REPLICATIONS <- 50L

#' Campaign length in days each replication runs for
HOLD_WINDOW_DAYS <- 30L

#' Control seed the per-replication seeds are drawn from, one arm at a time
HOLD_WINDOW_SEED <- 42L

#' Pre-open window values compared, in minutes
#'
#' @details Zero reproduces the instant-diversion model exactly
#'   (`scripts/check_pre_open_window.R`); 60 is the shipped value
#'   (`r2b.surgery.pre_open_window_min` in `env_data.json`).
HOLD_WINDOW_ARMS <- c(0L, 60L)

#' Response columns the published table reports, in its printed order
HOLD_WINDOW_RESPONSES <- c(
  "held_r2b", "r2b_surgeries", "diverted_offshift", "diverted_busy",
  "r2e_first_surgeries", "r2e_theatre_deferred", "total_dow", "total_casualties"
)

#' Reduce one replication to the hold window's response row
#'
#' @param env Wrapped simmer environment for one replication.
#' @param window_min Pre-open window the replication ran under, in minutes.
#' @return One-row data frame of the response set.
#'
#' @details Every response is a casualty count read off the attributes a
#'   single arrival either does or does not carry, so none needs its own
#'   estimator. `r2e_theatre_deferred` counts `surgery_deferred`, which is set
#'   at both R2B's and R2E's intensive care gate (`R/trajectories.R`); the R2B
#'   gate is close to inert at the shipped forward intensive care share of
#'   zero, so the count is read as R2E's, matching how `R/analysis.R` reports
#'   it in the single-run pathway summary.
reduce_hold_window_replication <- function(env, window_min) {
  arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  attributes <- simmer::get_mon_attributes(env)

  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"),
                      suffix = c("", "_arrival"))

  #' Count of casualties carrying one attribute at one value
  #'
  #' @param key Attribute key to match.
  #' @param val Attribute value to match.
  #' @return Integer count.
  at <- function(key, val) {
    if (!key %in% names(wide)) return(0L)
    sum(!is.na(wide[[key]]) & wide[[key]] == val)
  }

  data.frame(
    window_min           = window_min,
    held_r2b             = at("r2b_pre_open_wait", 1),
    r2b_surgeries        = at("r2b_surgery", 1),
    diverted_offshift    = at("r2b_bypass_reason", 1),
    diverted_busy        = at("r2b_bypass_reason", 2),
    r2e_first_surgeries  = at("r2e_surgery", 1),
    r2e_theatre_deferred = at("surgery_deferred", 1),
    total_dow            = at("dow", 1),
    total_casualties     = nrow(arrivals)
  )
}

#' Set the R2B pre-open window on a resolved configuration
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param window_min Pre-open window to set, in minutes.
#' @return Invisibly, the described configuration that was applied.
#'
#' @details The window is a variable rather than a bed count, so it is set
#'   after `build_environment()` on the described form, on the convention
#'   `R/policy_sweep.R`'s `apply_policy_setting()` establishes: `vars` is a
#'   list of name and value pairs only once `build_environment()` has named
#'   them, and setting it on the raw resolved form writes a value nothing
#'   reads.
apply_hold_window_setting <- function(json_data, scenario, window_min) {
  resolved <- resolve_scenario(json_data, scenario)
  described <- build_environment(resolved)
  described$vars$r2b$surgery$pre_open_window_min <- window_min
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Measure the hold window response set across replications at one arm
#'
#' @param window_min Pre-open window in force, in minutes.
#' @param n_iterations Replications to run (default HOLD_WINDOW_REPLICATIONS).
#' @param n_days Campaign length in days (default HOLD_WINDOW_DAYS).
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @return Data frame with one row per replication, carrying the replication
#'   index and the responses reduce_hold_window_replication() reports.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed under the caller's generator kind, and the caller's
#'   stream is restored on exit. A caller who sets the same control seed
#'   before each arm gives replication $k$ of every arm the same parent seed,
#'   though not the same casualty stream, for the reason this module's header
#'   records.
run_hold_window_measurement <- function(window_min, n_iterations = HOLD_WINDOW_REPLICATIONS,
                                        n_days = HOLD_WINDOW_DAYS, max_cores = NULL) {
  rng_state <- capture_rng_state()
  on.exit(restore_rng_state(rng_state), add = TRUE)

  rep_seeds <- sample.int(.Machine$integer.max, n_iterations)
  RNGkind("L'Ecuyer-CMRG")

  #' Run one replication and return its reduced response row alone
  #'
  #' @param i Index of the replication, into `rep_seeds`.
  #' @return The replication's one-row response frame, carrying its index.
  worker <- function(i) {
    env <- run_once(n_days, seed = rep_seeds[i], write_files = FALSE)
    row <- reduce_hold_window_replication(env, window_min)
    row$replication <- i
    row
  }

  dispatched <- dispatch_replications(worker, n_iterations, max_cores)
  usable     <- vapply(dispatched, is.data.frame, logical(1))
  if (!all(usable)) {
    stop(sprintf("%d of %d replications did not complete", sum(!usable), n_iterations),
         call. = FALSE)
  }
  do.call(rbind, dispatched)
}

#' Mean and 95% confidence interval of every response across replications
#'
#' @param rows Per-replication responses as returned by
#'   run_hold_window_measurement().
#' @param arm_column Name of the column identifying which arm a row belongs
#'   to, which is summarised over rather than as a response.
#' @return Data frame of response, n_reps, mean, ci_lower and ci_upper, one row
#'   per response.
#'
#' @details A response no replication produced is reported with an `n_reps` of
#'   zero rather than dropped, so a reader can tell an absent measurement from
#'   a measured zero.
summarise_hold_window <- function(rows, arm_column = "window_min") {
  responses <- setdiff(names(rows), c("replication", arm_column))
  do.call(rbind, lapply(responses, function(r) {
    x <- rows[[r]]
    x <- x[!is.na(x)]
    n <- length(x)
    m <- if (n > 0) mean(x) else NA_real_
    e <- if (n > 1) qt(0.975, df = n - 1) * sd(x) / sqrt(n) else 0
    data.frame(response = r, n_reps = n, mean = m,
               ci_lower = if (n > 1) m - e else m,
               ci_upper = if (n > 1) m + e else m)
  }))
}

#' Paired difference in one response between two arms
#'
#' @param rows Per-replication responses carrying both arms.
#' @param response Name of the response to difference.
#' @param from Arm to measure the change from, in minutes.
#' @param to Arm to measure the change to, in minutes.
#' @param arm_column Name of the column `from` and `to` are values of.
#' @return One-row data frame of response, from, to, n_pairs, mean difference,
#'   its 95% interval and the paired t-test's p-value.
#'
#' @details The difference is taken within replication rather than between arm
#'   means, which is the more precise comparison of the two even though the
#'   arms do not share a casualty stream: it still removes whatever variance
#'   the shared parent seed carries between the two draws of `sample.int()`,
#'   and is reported alongside each arm's own interval rather than in place of
#'   it. `n_pairs` is reported so that a difference resting on few pairs is
#'   visible, and the p-value so that a difference the count cannot establish
#'   is reported as unresolved rather than as absent.
hold_window_paired_difference <- function(rows, response, from, to,
                                          arm_column = "window_min") {
  a <- rows[rows[[arm_column]] == from, c("replication", response)]
  b <- rows[rows[[arm_column]] == to, c("replication", response)]
  pairs <- merge(a, b, by = "replication", suffixes = c("_from", "_to"))
  d <- pairs[[paste0(response, "_to")]] - pairs[[paste0(response, "_from")]]
  d <- d[!is.na(d)]
  n <- length(d)
  m <- if (n > 0) mean(d) else NA_real_
  e <- if (n > 1) qt(0.975, df = n - 1) * sd(d) / sqrt(n) else NA_real_
  data.frame(
    response = response, from = from, to = to, n_pairs = n, difference = m,
    ci_lower = if (n > 1) m - e else NA_real_,
    ci_upper = if (n > 1) m + e else NA_real_,
    p_value  = if (n > 1 && sd(d) > 0) t.test(d)$p.value else NA_real_
  )
}

#' Replications a paired difference would need for a given half-width
#'
#' @param rows Per-replication responses carrying both arms.
#' @param response Name of the response to size.
#' @param from Arm to measure the change from, in minutes.
#' @param to Arm to measure the change to, in minutes.
#' @param half_width Half-width wanted on the difference, in the response's
#'   own units.
#' @param arm_column Name of the column `from` and `to` are values of.
#' @return The replication count required, rounded up, or NA where the
#'   measured pairs give no usable standard deviation.
#'
#' @details Uses the measured paired standard deviation and the normal
#'   approximation $n = (1.96 s / h)^2$, on the convention
#'   `R/policy_sweep.R`'s `policy_replications_for()` establishes and
#'   `docs/Multi_Run_Supplement.md`'s replication-count derivation uses.
hold_window_replications_for <- function(rows, response, from, to, half_width,
                                         arm_column = "window_min") {
  a <- rows[rows[[arm_column]] == from, c("replication", response)]
  b <- rows[rows[[arm_column]] == to, c("replication", response)]
  pairs <- merge(a, b, by = "replication", suffixes = c("_from", "_to"))
  d <- pairs[[paste0(response, "_to")]] - pairs[[paste0(response, "_from")]]
  d <- d[!is.na(d)]
  if (length(d) < 2 || sd(d) == 0 || half_width <= 0) return(NA_real_)
  ceiling((qnorm(0.975) * sd(d) / half_width)^2)
}
