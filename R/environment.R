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

#' Generates mass casualty (MASCAL) event arrival timestamps via a compound
#' Poisson process
#'
#' @param n_days Duration in days
#' @param params List with rate_per_day, min_cas, max_cas, window_min,
#'   window_mode, window_max (as read from
#'   env_data$vars$mass_casualty$event)
#' @param seed Optional random seed for reproducibility
#' @param write_file Write the arrival stream and event log to data/
#'   (default TRUE; set FALSE for parallel replication workers to avoid
#'   file-write conflicts)
#' @param antithetic Logical; when TRUE the antithetic variate U' = 1 - U is
#'   substituted for U in the event inter-arrival, casualty-count, window
#'   duration, and within-window offset draws. Enables antithetic pairing
#'   in run_replications().
#' @return Named list: `arrival_times` (sorted numeric vector of individual
#'   casualty arrival times, simulation minutes) and `events` (data frame
#'   with one row per event: event_id, event_start, n_cas, window_min —
#'   used for the mass casualty event timeline plot in R/analysis.R)
#'
#' @details Implements a compound Poisson process for mass casualty
#'   injection (Fischer et al., 2025; Debacker et al., 2016): event
#'   inter-arrival times are drawn from an Exponential(rate_per_day)
#'   distribution, each event injects Uniform(min_cas, max_cas) casualties
#'   distributed across a Triangular(window_min, window_mode,
#'   window_max)-minute injection window following the event start.
#'   `rate_per_day = 0` returns an empty arrival stream — background
#'   lognormal generation is unaffected, satisfying Issue #9's disable-path
#'   acceptance criterion.
generate_mass_casualty_events <- function(n_days, params, seed = NULL,
                                          write_file = TRUE, antithetic = FALSE) {
  if (!is.null(seed)) set.seed(seed)

  n_minutes    <- day_min * n_days
  rate_per_min <- params$rate_per_day / day_min

  empty_events <- data.frame(event_id = integer(0), event_start = numeric(0),
                             n_cas = integer(0), window_min = numeric(0))

  if (rate_per_min <= 0) {
    if (write_file) {
      write.table(numeric(0), file = file.path("data", "arrivals_mascal.txt"),
                 row.names = FALSE, col.names = FALSE)
      write.csv(empty_events, file.path("data", "mass_casualty_events.csv"),
               row.names = FALSE)
    }
    return(list(arrival_times = numeric(0), events = empty_events))
  }

  # Exponential inter-arrival times via inverse-CDF, so U can be reflected
  # for antithetic pairing (matches generate_ln_arrivals()/generate_exp_arrivals()).
  event_starts <- c()
  t <- 0
  repeat {
    u <- runif(1)
    if (antithetic) u <- 1 - u
    t <- t - log(1 - u) / rate_per_min
    if (t >= n_minutes) break
    event_starts <- c(event_starts, t)
  }

  arrival_times <- c()
  event_id      <- c()
  window_dur    <- c()
  n_cas_actual  <- c()

  for (i in seq_along(event_starts)) {
    u_cas <- runif(1)
    if (antithetic) u_cas <- 1 - u_cas
    n_cas_draw <- round(params$min_cas + u_cas * (params$max_cas - params$min_cas))

    window <- rtriangle(1, a = params$window_min, b = params$window_max,
                        c = params$window_mode)

    u_offset <- runif(n_cas_draw)
    if (antithetic) u_offset <- 1 - u_offset
    offsets <- sort(u_offset * window)

    times <- event_starts[i] + offsets
    times <- times[times < n_minutes]

    arrival_times <- c(arrival_times, times)
    event_id      <- c(event_id, rep(i, length(times)))
    window_dur    <- c(window_dur, window)
    n_cas_actual  <- c(n_cas_actual, length(times))
  }

  arrival_times <- sort(arrival_times)

  events <- data.frame(
    event_id    = seq_along(event_starts),
    event_start = event_starts,
    n_cas       = n_cas_actual,
    window_min  = window_dur
  )

  if (write_file) {
    write.table(arrival_times, file = file.path("data", "arrivals_mascal.txt"),
               row.names = FALSE, col.names = FALSE)
    write.csv(events, file.path("data", "mass_casualty_events.csv"), row.names = FALSE)
  }

  list(arrival_times = arrival_times, events = events)
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
