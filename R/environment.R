##############################################
## R/environment.R                          ##
## Environment construction and data import ##
##############################################

library(jsonlite)
library(simmer)
library(simmer.bricks)
library(triangle)

source("R/scenario.R")

# ── Data import ──────────────────────────────────────────────────────────────

#' Builds structured environment data from parsed JSON
#'
#' @param data Parsed JSON list from env_data.json
#' @return Named list with elements: pops, elms, transports, vars
build_environment <- function(data) {
  env_list <- list()

  for (elm in data$elms) {
    elm_name <- elm$elm
    elm_qty  <- elm$qty
    elm_instances <- vector("list", elm_qty)

    for (i in seq_len(elm_qty)) {
      instance <- list()

      if (!is.null(elm$sub_elms)) {
        resource_vector <- c()

        for (sub in elm$sub_elms) {
          if (sub$sub_elm == 1) {
            for (res in sub$resources) {
              res_type <- res$type
              res_name <- if ("name" %in% names(res)) res$name else res$resource
              for (j in seq_len(res$qty)) {
                res_id <- paste("c", elm_name, res_type, res_name, j, paste0("t", i), sep = "_")
                resource_vector <- c(resource_vector, res_id)
              }
            }
          } else {
            sub_name <- sub$sub_elm
            sub_qty <- if (!is.null(sub$qty)) sub$qty else 1
            sub_teams <- vector("list", sub_qty)

            for (team_index in seq_len(sub_qty)) {
              sub_vector <- c()
              for (res in sub$resources) {
                res_name <- if ("name" %in% names(res)) res$name else res$resource
                for (j in seq_len(res$qty)) {
                  res_id <- paste("c", elm_name, sub_name, team_index, res_name, j, paste0("t", i), sep = "_")
                  sub_vector <- c(sub_vector, res_id)
                }
              }
              sub_teams[[team_index]] <- sub_vector
            }

            instance[[sub_name]] <- sub_teams
          }
        }

        if (elm_name == "r1") {
          instance <- resource_vector
        }
      }

      if (!is.null(elm$beds)) {
        for (bed in elm$beds) {
          bed_type <- bed$name
          bed_ids <- if (bed$qty > 0) {
            paste0("b_", elm_name, "_", bed_type, "_", seq_len(bed$qty), "_t", i)
          } else {
            character(0)
          }
          instance[[paste0(bed_type, "_bed")]] <- bed_ids
        }
      }

      elm_instances[[i]] <- instance
    }

    env_list[[elm_name]] <- elm_instances
  }

  transports_list <- list()
  if (!is.null(data$transports)) {
    for (vehicle in data$transports) {
      transports_list[[vehicle$name]] <- if (vehicle$qty > 0) {
        paste0("t_", vehicle$name, "_", seq_len(vehicle$qty))
      } else {
        character(0)
      }
    }
  }

  pops_list <- list()
  if (!is.null(data$pops)) {
    pops_list <- setNames(
      lapply(data$pops, function(p) p$count),
      sapply(data$pops, function(p) p$name)
    )
  }

  vars_list <- list()
  if (!is.null(data$vars)) {
    for (elm_def in data$vars) {
      elm_name <- elm_def$elm
      acty_defs <- elm_def$actys

      acty_list <- list()
      for (acty_def in acty_defs) {
        acty_name <- acty_def$acty
        vals_list <- setNames(
          lapply(acty_def$vals, function(v) v$val),
          sapply(acty_def$vals, function(v) v$var)
        )
        acty_list[[acty_name]] <- vals_list
      }

      vars_list[[elm_name]] <- acty_list
    }
  }

  return(list(
    pops = pops_list,
    elms = env_list,
    transports = transports_list,
    vars = vars_list
  ))
}

#' Loads and parses env_data.json into a structured environment list
#'
#' @param path File path to env_data.json
#' @return Named list with elements: pops, elms, transports, vars
load_elms <- function(path) {
  json_data <- fromJSON(path, simplifyVector = FALSE)
  build_environment(json_data)
}

#' Loads env_data.json and applies a named scenario profile overlay
#'
#' @param path File path to env_data.json
#' @param scenario Name of scenario profile to apply (default "default" —
#'   base parameters, no override; reproduces the existing baseline exactly)
#' @return Named list with elements: pops, elms, transports, vars
#'
#' @details Scenario profiles are defined under the top-level `scenarios`
#'   key in env_data.json and override only the scenario-specific subset of
#'   `vars` (casualty generation rates, DOW parameters and treatment
#'   efficacy factors, priority distribution, evacuation/surgery
#'   probabilities, DNBI composition, transport time distributions).
#'   Structural configuration (`elms`, `transports`, `pops`) is never
#'   overridden. See `resolve_scenario()` and `merge_scenario_vars()` in
#'   R/scenario.R.
load_scenario <- function(path, scenario = "default") {
  json_data <- fromJSON(path, simplifyVector = FALSE)
  json_data <- resolve_scenario(json_data, scenario)
  build_environment(json_data)
}

# ── Casualty rate generation ──────────────────────────────────────────────────

#' Generates lognormal arrival timestamps using capped log-normal rates
#'
#' @param type Character string identifying casualty stream (e.g. "wia_cbt")
#' @param mean_daily Expected daily rate
#' @param sd_daily Standard deviation of daily rate
#' @param pop Size of target population
#' @param n_days Duration in days
#' @param cap Maximum per-minute rate cap (default 5)
#' @param seed Optional random seed for reproducibility
#' @param write_file Write arrival times to data/ directory (default TRUE;
#'   set FALSE for parallel replication workers to avoid file-write conflicts)
#' @param antithetic Logical; when TRUE the antithetic variate U' = 1 - U is
#'   substituted for U in both the log-normal rate draw and the within-minute
#'   arrival jitter. Enables antithetic pairing in run_replications().
#' @return Vector of arrival times in simulation minutes
generate_ln_arrivals <- function(type, mean_daily, sd_daily, pop, n_days,
                                 cap = 5, seed = NULL, write_file = TRUE,
                                 antithetic = FALSE) {
  if (!is.null(seed)) set.seed(seed)

  n_minutes <- day_min * n_days

  mu_log    <- log(mean_daily^2 / sqrt(sd_daily^2 + mean_daily^2))
  sigma_log <- sqrt(log(1 + (sd_daily^2 / mean_daily^2)))

  # Explicit inverse-CDF transform so U can be reflected for antithetic pairing
  u_rate <- runif(n_minutes)
  if (antithetic) u_rate <- 1 - u_rate
  rates      <- pmin(qlnorm(u_rate, meanlog = mu_log, sdlog = sigma_log), cap)
  rates      <- rates / 1440 * pop / 1000
  cumulative <- cumsum(rates)

  arrival_idx  <- which(floor(cumulative) > floor(cumulative - rates))
  u_jitter     <- runif(length(arrival_idx))
  if (antithetic) u_jitter <- 1 - u_jitter
  arrival_times <- sort(arrival_idx + u_jitter)

  if (write_file) {
    filename <- file.path("data", paste0("arrivals_", type, ".txt"))
    write.table(arrival_times, file = filename, row.names = FALSE, col.names = FALSE)
  }

  return(arrival_times)
}

#' Generates exponential arrival timestamps using capped exponential rates
#'
#' @param type Character string identifying casualty stream (e.g. "wia_cbt")
#' @param mean_daily Expected daily rate. Fully parameterises the exponential
#'   rate distribution (rate = 1 / mean_daily); unlike generate_ln_arrivals(),
#'   there is no separate sd_daily shape parameter for a single-parameter
#'   exponential distribution.
#' @param pop Size of target population
#' @param n_days Duration in days
#' @param cap_multiplier Per-minute rate cap, expressed as a multiple of
#'   mean_daily rather than an absolute value (default 3). Because
#'   P(Exponential(mean) > k * mean) = exp(-k) regardless of mean, this
#'   yields the same ~5% truncation probability for every exponential
#'   stream irrespective of intensity — unlike a fixed absolute cap (as
#'   used by generate_ln_arrivals()), which truncates a rapidly growing
#'   share of the distribution as mean_daily approaches the cap (e.g.
#'   ~48% for a mean of 6.86 against a fixed cap of 5; see README
#'   Casualty Generation section).
#' @param seed Optional random seed for reproducibility
#' @param write_file Write arrival times to data/ directory (default TRUE;
#'   set FALSE for parallel replication workers to avoid file-write conflicts)
#' @param antithetic Logical; when TRUE the antithetic variate U' = 1 - U is
#'   substituted for U in both the exponential rate draw and the
#'   within-minute arrival jitter. Enables antithetic pairing in
#'   run_replications().
#' @return Vector of arrival times in simulation minutes
#'
#' @details FORECAS (Blood, Zouris & Rotblatt, 1998) fits lognormal and
#'   exponential distributions to different battle intensities/troop types.
#'   Used for the high_intensity scenario profile (Issue #54), whose
#'   higher-intensity casualty streams are exponential-distributed rather
#'   than lognormal-distributed like the moderate_intensity/default streams.
generate_exp_arrivals <- function(type, mean_daily, pop, n_days,
                                  cap_multiplier = 3, seed = NULL, write_file = TRUE,
                                  antithetic = FALSE) {
  if (!is.null(seed)) set.seed(seed)

  n_minutes <- day_min * n_days
  cap <- cap_multiplier * mean_daily

  # Explicit inverse-CDF transform so U can be reflected for antithetic pairing
  u_rate <- runif(n_minutes)
  if (antithetic) u_rate <- 1 - u_rate
  rates      <- pmin(qexp(u_rate, rate = 1 / mean_daily), cap)
  rates      <- rates / 1440 * pop / 1000
  cumulative <- cumsum(rates)

  arrival_idx  <- which(floor(cumulative) > floor(cumulative - rates))
  u_jitter     <- runif(length(arrival_idx))
  if (antithetic) u_jitter <- 1 - u_jitter
  arrival_times <- sort(arrival_idx + u_jitter)

  if (write_file) {
    filename <- file.path("data", paste0("arrivals_", type, ".txt"))
    write.table(arrival_times, file = filename, row.names = FALSE, col.names = FALSE)
  }

  return(arrival_times)
}

#' Dispatches to the appropriate arrival generator for a casualty stream
#'
#' @param type Character string identifying casualty stream (e.g. "wia_cbt")
#' @param gen_vars List with mean_daily and (for lognormal streams) sd_daily,
#'   as read from env_data$vars$generators[[type]]; an optional `distribution`
#'   field selects "lognormal" (default, if absent) or "exponential"
#' @param pop Size of target population
#' @param n_days Duration in days
#' @param write_file Write arrival times to data/ directory
#' @param antithetic Logical; antithetic variate pairing (see
#'   generate_ln_arrivals() / generate_exp_arrivals())
#' @return Vector of arrival times in simulation minutes
generate_casualty_arrivals <- function(type, gen_vars, pop, n_days,
                                       write_file = FALSE, antithetic = FALSE) {
  distribution <- if (!is.null(gen_vars$distribution)) gen_vars$distribution else "lognormal"

  if (distribution == "exponential") {
    generate_exp_arrivals(type, gen_vars$mean_daily, pop, n_days,
                         write_file = write_file, antithetic = antithetic)
  } else {
    generate_ln_arrivals(type, gen_vars$mean_daily, gen_vars$sd_daily, pop, n_days,
                        write_file = write_file, antithetic = antithetic)
  }
}

#' Draws event start times for the "poisson" mass casualty mode
#'
#' @param n_days Duration in days
#' @param event_params List with rate_per_day, as read from
#'   env_data$vars$mass_casualty$event
#' @param antithetic Logical; antithetic variate pairing (see
#'   generate_mass_casualty_events())
#' @return Numeric vector of event start times (simulation minutes),
#'   ascending; empty if rate_per_day <= 0
#'
#' @details Event inter-arrival times are drawn from an
#'   Exponential(rate_per_day) distribution via inverse-CDF, so U can be
#'   reflected for antithetic pairing (matches
#'   generate_ln_arrivals()/generate_exp_arrivals()). `rate_per_day = 0`
#'   returns immediately with no RNG draws consumed, so the stream
#'   downstream of this call is unaffected — the basis for Issue #9's
#'   disable-path acceptance criterion.
mass_casualty_event_starts_poisson <- function(n_days, event_params, antithetic = FALSE) {
  n_minutes    <- day_min * n_days
  rate_per_min <- event_params$rate_per_day / day_min

  if (rate_per_min <= 0) return(numeric(0))

  event_starts <- c()
  t <- 0
  repeat {
    u <- runif(1)
    if (antithetic) u <- 1 - u
    t <- t - log(1 - u) / rate_per_min
    if (t >= n_minutes) break
    event_starts <- c(event_starts, t)
  }
  event_starts
}

#' Draws event start times and per-event parameters for the "scheduled"
#' mass casualty mode
#'
#' @param n_days Duration in days
#' @param schedule_params List with `days` (simulation day, 1-indexed, on
#'   which a candidate event may occur), `probabilities` (per-day Bernoulli
#'   occurrence probability), and `min_cas`/`max_cas`/`pri_one`/`pri_two`/
#'   `pri_three` (per-day casualty-count bounds and triage priority split),
#'   all parallel arrays as read from env_data$vars$mass_casualty$schedule.
#'   Any array empty or omitted defaults every day to the same value
#'   (probability 1; min_cas/max_cas 20/60; priority 0.7/0.2/0.1 — the
#'   Issue #9 Recommended Approach values), so a planner can specify only
#'   `days` and accept sensible defaults for the rest.
#' @param antithetic Logical; antithetic variate pairing (see
#'   generate_mass_casualty_events())
#' @return Data frame (one row per *fired* event, ascending by start time):
#'   `start` (simulation minutes), `min_cas`, `max_cas`, `pri_one`,
#'   `pri_two`, `pri_three` — empty (0 rows) if no scheduled days are
#'   configured or none fire this draw
#'
#' @details Lets a planner specify a fixed number of events, the exact
#'   simulation days they may occur on, and each event's own casualty
#'   count and triage priority mix independently — e.g. scripting a
#'   specific historical or exercise timeline where one event is a small,
#'   low-severity incident and another is a large blast-dominant one. Each
#'   configured day is independently included via a Bernoulli(probability)
#'   draw, so per-replication variation is still possible (a day with
#'   probability 1 always fires; a lower probability introduces controlled
#'   randomness across replications). A fired day's exact start minute is
#'   drawn Uniform(0, 1440) within that day, so intra-day timing remains
#'   stochastic even though the day itself is planner-specified. The
#'   injection window (window_min/mode/max) is not customisable per event —
#'   it remains a single shared value read from `params$event` by the
#'   caller (generate_mass_casualty_events()) regardless of mode.
mass_casualty_event_starts_scheduled <- function(n_days, schedule_params, antithetic = FALSE) {
  n_minutes <- day_min * n_days
  empty <- data.frame(start = numeric(0), min_cas = numeric(0), max_cas = numeric(0),
                      pri_one = numeric(0), pri_two = numeric(0), pri_three = numeric(0))

  days <- unlist(schedule_params$days)
  if (length(days) == 0) return(empty)
  n <- length(days)

  fill <- function(var, default) {
    v <- unlist(schedule_params[[var]])
    if (length(v) == 0) rep(default, n) else v
  }
  probs     <- fill("probabilities", 1)
  min_cas   <- fill("min_cas",   20)
  max_cas   <- fill("max_cas",   60)
  pri_one   <- fill("pri_one",   0.7)
  pri_two   <- fill("pri_two",   0.2)
  pri_three <- fill("pri_three", 0.1)

  lens <- c(length(probs), length(min_cas), length(max_cas), length(pri_one), length(pri_two), length(pri_three))
  if (any(lens != n)) {
    stop("mass_casualty.schedule arrays must each be empty (defaulted) or match schedule.days in length")
  }

  u_occur <- runif(n)
  if (antithetic) u_occur <- 1 - u_occur
  fire <- u_occur < probs
  if (!any(fire)) return(empty)

  u_intraday <- runif(sum(fire))
  if (antithetic) u_intraday <- 1 - u_intraday
  starts <- (days[fire] - 1) * day_min + u_intraday * day_min

  out <- data.frame(start = starts, min_cas = min_cas[fire], max_cas = max_cas[fire],
                    pri_one = pri_one[fire], pri_two = pri_two[fire], pri_three = pri_three[fire])
  out <- out[out$start >= 0 & out$start < n_minutes, , drop = FALSE]
  out[order(out$start), , drop = FALSE]
}

#' Draws casualty arrival times, count, and injection window for one mass casualty event
#'
#' @param event_start Event start time (simulation minutes)
#' @param event_params List with min_cas, max_cas, window_min, window_mode,
#'   window_max, as read from env_data$vars$mass_casualty$event
#' @param n_minutes Total simulation duration in minutes (arrivals at or
#'   after this are dropped)
#' @param antithetic Logical; antithetic variate pairing (see
#'   generate_mass_casualty_events())
#' @return Named list: `times` (numeric vector of casualty arrival times)
#'   and `window` (the drawn injection window duration, minutes)
mass_casualty_event_casualties <- function(event_start, event_params, n_minutes, antithetic = FALSE) {
  u_cas <- runif(1)
  if (antithetic) u_cas <- 1 - u_cas
  n_cas_draw <- round(event_params$min_cas + u_cas * (event_params$max_cas - event_params$min_cas))

  window <- rtriangle(1, a = event_params$window_min, b = event_params$window_max,
                      c = event_params$window_mode)

  u_offset <- runif(n_cas_draw)
  if (antithetic) u_offset <- 1 - u_offset
  offsets <- sort(u_offset * window)

  times <- event_start + offsets
  times <- times[times >= 0 & times < n_minutes]

  list(times = times, window = window)
}

#' Generates mass casualty event arrival timestamps
#'
#' @param n_days Duration in days
#' @param params The full env_data$vars$mass_casualty list, with `event`
#'   (mode ["poisson"|"scheduled"], rate_per_day, min_cas, max_cas,
#'   window_min, window_mode, window_max), `schedule` (days, probabilities,
#'   min_cas, max_cas, pri_one/two/three — read only when
#'   `event$mode == "scheduled"`), and `priority` (one/two/three, the
#'   shared blast-dominant split used for "poisson"-mode events, since
#'   only "scheduled" mode supports a per-event priority override)
#' @param seed Optional random seed for reproducibility
#' @param write_file Write the arrival stream and event log to data/
#'   (default TRUE; set FALSE for parallel replication workers to avoid
#'   file-write conflicts)
#' @param antithetic Logical; when TRUE the antithetic variate U' = 1 - U is
#'   substituted for U in every draw this function and its mode-specific
#'   helpers make. Enables antithetic pairing in run_replications().
#' @return Named list: `arrival_times` (sorted numeric vector of individual
#'   casualty arrival times, simulation minutes), `casualty_event_id`
#'   (integer vector parallel to `arrival_times`, giving the 1-indexed
#'   event each casualty belongs to — matches `events$event_id`; consumed
#'   by build_casualty_trajectory() for per-event priority lookup), and
#'   `events` (data frame with one row per event: event_id, event_start,
#'   n_cas, window_min, pri_one, pri_two, pri_three — the pri_* columns
#'   are NA for "poisson"-mode events, meaning "use the shared
#'   params$priority split"; used for the mass casualty event timeline
#'   plot in R/analysis.R)
#'
#' @details Two event-timing modes are supported, selected by
#'   `params$event$mode`: "poisson" (default) implements a compound
#'   Poisson process for mass casualty injection (Fischer et al., 2025;
#'   Debacker et al., 2016) — event inter-arrival times are drawn from an
#'   Exponential(rate_per_day) distribution (`mass_casualty_event_starts_poisson()`),
#'   with every event sharing the same min_cas/max_cas and priority split;
#'   "scheduled" instead takes a planner-specified list of candidate
#'   simulation days, each with its own independent occurrence probability,
#'   casualty-count bounds, and priority split
#'   (`mass_casualty_event_starts_scheduled()`). Both modes then draw each
#'   fired event's casualty count and per-casualty offsets from a shared
#'   injection window (`mass_casualty_event_casualties()`): Uniform(min_cas,
#'   max_cas) casualties distributed across a Triangular(window_min,
#'   window_mode, window_max)-minute window — the window itself is not
#'   customisable per event in either mode. An event schedule/rate that
#'   produces zero events returns an empty arrival stream — background
#'   lognormal generation is unaffected, satisfying Issue #9's
#'   disable-path acceptance criterion (shipped default: "poisson" mode,
#'   rate_per_day = 0).
generate_mass_casualty_events <- function(n_days, params, seed = NULL,
                                          write_file = TRUE, antithetic = FALSE) {
  if (!is.null(seed)) set.seed(seed)

  n_minutes <- day_min * n_days
  mode <- if (!is.null(params$event$mode)) params$event$mode else "poisson"

  empty_events <- data.frame(event_id = integer(0), event_start = numeric(0),
                             n_cas = integer(0), window_min = numeric(0),
                             pri_one = numeric(0), pri_two = numeric(0), pri_three = numeric(0))

  sched <- if (identical(mode, "scheduled")) {
    mass_casualty_event_starts_scheduled(n_days, params$schedule, antithetic = antithetic)
  } else {
    starts <- mass_casualty_event_starts_poisson(n_days, params$event, antithetic = antithetic)
    # Built explicitly per-column (not data.frame(start = starts, min_cas =
    # params$event$min_cas, ...)) because data.frame() cannot recycle a
    # length-1 scalar against a length-0 `starts` (rate_per_day = 0, the
    # shipped default) — "arguments imply differing number of rows".
    n <- length(starts)
    data.frame(start = starts, min_cas = rep(params$event$min_cas, n), max_cas = rep(params$event$max_cas, n),
              pri_one = rep(NA_real_, n), pri_two = rep(NA_real_, n), pri_three = rep(NA_real_, n))
  }

  if (nrow(sched) == 0) {
    if (write_file) {
      write.table(numeric(0), file = file.path("data", "arrivals_mass_casualty.txt"),
                 row.names = FALSE, col.names = FALSE)
      write.csv(empty_events, file.path("data", "mass_casualty_events.csv"),
               row.names = FALSE)
    }
    return(list(arrival_times = numeric(0), casualty_event_id = integer(0), events = empty_events))
  }

  arrival_times     <- c()
  casualty_event_id <- c()
  window_dur        <- c()
  n_cas_actual      <- c()

  for (i in seq_len(nrow(sched))) {
    event_params <- list(min_cas = sched$min_cas[i], max_cas = sched$max_cas[i],
                         window_min = params$event$window_min, window_mode = params$event$window_mode,
                         window_max = params$event$window_max)
    cas <- mass_casualty_event_casualties(sched$start[i], event_params, n_minutes, antithetic = antithetic)

    arrival_times     <- c(arrival_times, cas$times)
    casualty_event_id <- c(casualty_event_id, rep(i, length(cas$times)))
    window_dur         <- c(window_dur, cas$window)
    n_cas_actual        <- c(n_cas_actual, length(cas$times))
  }

  # Sort arrivals but keep casualty_event_id correctly paired per-casualty
  # (order(), not sort(), so the two vectors share one permutation).
  ord <- order(arrival_times)
  arrival_times     <- arrival_times[ord]
  casualty_event_id <- casualty_event_id[ord]

  events <- data.frame(
    event_id    = seq_len(nrow(sched)),
    event_start = sched$start,
    n_cas       = n_cas_actual,
    window_min  = window_dur,
    pri_one     = sched$pri_one,
    pri_two     = sched$pri_two,
    pri_three   = sched$pri_three
  )

  if (write_file) {
    write.table(arrival_times, file = file.path("data", "arrivals_mass_casualty.txt"),
               row.names = FALSE, col.names = FALSE)
    write.csv(events, file.path("data", "mass_casualty_events.csv"), row.names = FALSE)
  }

  list(arrival_times = arrival_times, casualty_event_id = casualty_event_id, events = events)
}

# ── Simmer environment construction ─────────────────────────────────────────

#' Initializes the simmer environment by adding all resources from env_data
#'
#' @param env A simmer environment object
#' @param env_data Nested list defining resources for each echelon/unit type
#' @param ot_hours Hours per day that the first OT shift is active (default 12).
#'   Shift 1 covers 0 to ot_hours; Shift 2 covers ot_hours to 24. Used by
#'   sensitivity analysis to screen OT availability as a screened parameter.
#' @return Modified simmer environment with all resources added
#'
#' @details Schedules operating theatre shifts for surgical staff and beds.
#' Counters r2e_surg_counter, r2e_ot_bed_counter, r2b_surg_counter, and
#' r2b_ot_bed_counter alternate shift assignments across teams.
build_env <- function(env, env_data, ot_hours = 12) {
  ot_break   <- as.integer(ot_hours * 60L)
  ot_shift_1 <- simmer::schedule(c(0, ot_break),        c(1, 0), period = 1440)
  ot_shift_2 <- simmer::schedule(c(ot_break, 1440), c(1, 0), period = 1440)

  r2e_surg_counter   <- 1
  r2b_surg_counter   <- 1

  for (elm_type in names(env_data$elms)) {
    for (team in env_data$elms[[elm_type]]) {
      if (is.character(team)) {
        for (res_name in team) {
          env <- env %>% add_resource(res_name)
        }

      } else if (is.list(team)) {
        apply_schedule <- elm_type %in% c("r2b", "r2eheavy")

        if (apply_schedule) {
          for (section_name in names(team)) {
            section <- team[[section_name]]

            for (res_name in section) {
              if (section_name == "surg") {
                if (elm_type == "r2b") {
                  team_shift <- if (r2b_surg_counter %% 2 == 1) ot_shift_1 else ot_shift_2
                  env <- env %>% add_resource(res_name, team_shift)
                  r2b_surg_counter <- r2b_surg_counter + 1

                } else if (elm_type == "r2eheavy") {
                  team_shift <- if (r2e_surg_counter %% 2 == 1) ot_shift_1 else ot_shift_2
                  env <- env %>% add_resource(res_name, team_shift)
                  r2e_surg_counter <- r2e_surg_counter + 1
                }

              } else if (section_name == "ot_bed") {
                # OT rooms are physical spaces available 24 h; only the surgical
                # team (surg section) carries the shift schedule.
                env <- env %>% add_resource(res_name)

              } else {
                env <- env %>% add_resource(res_name)
              }
            }
          }

        } else {
          for (section in team) {
            for (res_name in section) {
              env <- env %>% add_resource(res_name)
            }
          }
        }
      }
    }
  }

  for (transport_type in names(env_data$transports)) {
    for (res_name in env_data$transports[[transport_type]]) {
      env <- env %>% add_resource(res_name)
    }
  }

  return(env)
}
