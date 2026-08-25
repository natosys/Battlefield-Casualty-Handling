##############################################
## R/environment.R                          ##
## Environment construction and data import ##
##############################################

library(jsonlite)
library(simmer)
library(simmer.bricks)
library(triangle)

source("R/constants.R")
source("R/scenario.R")

# ── Global configuration save/restore ────────────────────────────────────────

# The three globals that carry the model's configuration. run.R, app.R, the
# scripts under scripts/ and the sweep/screen entry points all set these with
# `<<-`, because run_once()/build_env() and the trajectory closures resolve
# them from the global environment rather than receiving them as arguments.
CONFIG_GLOBALS <- c("env_data", "day_min", "counts")


#' Snapshot the global configuration variables so they can be restored
#'
#' @param var_names Character vector of global variable names to snapshot
#'   (default CONFIG_GLOBALS)
#' @return Named list, one element per requested name, each a list of
#'   `bound` (was the name bound in the global environment?) and `value`
#'   (its value if it was, NULL otherwise)
#'
#' @details Paired with restore_config_globals() in an
#'   `on.exit(..., add = TRUE)` at the point of save, so a function that
#'   mutates the configuration globals leaves them as it found them on the
#'   error path as well as the success path. The unbound case is recorded
#'   rather than treated as an error because a caller such as
#'   scripts/run_scenarios.R reaches run_scenario() without ever having set
#'   the globals itself, and restoring must then remove them rather than
#'   leave the last scenario's configuration behind.
capture_config_globals <- function(var_names = CONFIG_GLOBALS) {
  setNames(lapply(var_names, function(nm) {
    if (exists(nm, envir = globalenv(), inherits = FALSE)) {
      list(bound = TRUE, value = get(nm, envir = globalenv(), inherits = FALSE))
    } else {
      list(bound = FALSE, value = NULL)
    }
  }), var_names)
}

#' Restore global configuration variables from a capture_config_globals() snapshot
#'
#' @param snapshot Named list as returned by capture_config_globals()
#' @return The snapshot, invisibly
#'
#' @details A name that was bound when the snapshot was taken is reassigned
#'   its saved value; a name that was not is removed if the intervening code
#'   created it. Assignment is to globalenv() explicitly, which is the same
#'   environment the `<<-` in these entry points reaches (they are defined in
#'   the global environment, and in a forked mclapply worker both reach that
#'   fork's own copy).
restore_config_globals <- function(snapshot) {
  for (nm in names(snapshot)) {
    entry <- snapshot[[nm]]
    if (isTRUE(entry$bound)) {
      assign(nm, entry$value, envir = globalenv())
    } else if (exists(nm, envir = globalenv(), inherits = FALSE)) {
      rm(list = nm, envir = globalenv())
    }
  }
  invisible(snapshot)
}

# ── Data import ──────────────────────────────────────────────────────────────

#' Assert that a parsed env_data.json has the structure the model requires
#'
#' @param data Parsed JSON list, as returned by
#'   jsonlite::fromJSON(path, simplifyVector = FALSE)
#' @param source_label Label naming where the configuration came from, used in the
#'   error message (default "env_data.json")
#' @return TRUE, invisibly, if the structure is sound; otherwise stops
#'
#' @details Checked at the boundary where a configuration enters the program
#'   (app.R's startup load and its upload handler), so a malformed file fails
#'   with a message naming the field at fault rather than as a subscript
#'   error raised deep inside build_environment() or, worse, as a silently
#'   emptied population or resource pool. Every fault found is reported, not
#'   just the first, since a hand-edited file often carries more than one.
#'   The check is structural: it asserts that the blocks the model reads are
#'   present and carry the fields it indexes by, not that any particular
#'   parameter is present or that its value is operationally plausible.
validate_env_data_json <- function(data, source_label = "env_data.json") {
  problems <- character(0)
  note <- function(...) problems <<- c(problems, sprintf(...))

  if (!is.list(data) || is.null(names(data))) {
    stop(sprintf("validate_env_data_json: %s did not parse to a named list", source_label),
         call. = FALSE)
  }

  for (block in c("pops", "elms", "transports", "vars")) {
    if (is.null(data[[block]])) {
      note("top-level block '%s' is missing", block)
    } else if (!is.list(data[[block]]) || length(data[[block]]) == 0) {
      note("top-level block '%s' is empty or is not a list", block)
    }
  }

  # A count/qty must be a single finite non-negative number: these are
  # multiplied out into population sizes and resource identifiers, where a
  # vector, a string or an NA becomes a length error thousands of lines away.
  check_qty <- function(value, field) {
    usable <- is.numeric(value) && length(value) == 1L &&
      !is.na(value) && is.finite(value) && value >= 0
    if (is.null(value)) {
      note("%s is missing", field)
    } else if (!usable) {
      note("%s must be a single finite non-negative number, found %s",
           field, paste(format(value), collapse = ", "))
    }
  }

  check_name <- function(value, field) {
    if (is.null(value)) {
      note("%s is missing", field)
    } else if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
      note("%s must be a single non-empty string, found %s",
           field, paste(format(value), collapse = ", "))
    }
  }

  for (i in seq_along(data$pops)) {
    check_name(data$pops[[i]]$name,  sprintf("pops[[%d]]$name", i))
    check_qty(data$pops[[i]]$count, sprintf("pops[[%d]]$count", i))
  }

  for (i in seq_along(data$elms)) {
    check_name(data$elms[[i]]$elm, sprintf("elms[[%d]]$elm", i))
    check_qty(data$elms[[i]]$qty,  sprintf("elms[[%d]]$qty", i))
  }

  for (i in seq_along(data$transports)) {
    check_name(data$transports[[i]]$name, sprintf("transports[[%d]]$name", i))
    check_qty(data$transports[[i]]$qty,   sprintf("transports[[%d]]$qty", i))
  }

  # vars is read as vars[[elm]][[acty]][[var]], so every level needs the key
  # it is indexed by; a missing one silently drops the whole sub-tree.
  for (i in seq_along(data$vars)) {
    elm_field <- sprintf("vars[[%d]]$elm", i)
    check_name(data$vars[[i]]$elm, elm_field)
    actys <- data$vars[[i]]$actys
    if (is.null(actys) || !is.list(actys)) {
      note("vars[[%d]]$actys is missing or is not a list", i)
      next
    }
    for (j in seq_along(actys)) {
      check_name(actys[[j]]$acty, sprintf("vars[[%d]]$actys[[%d]]$acty", i, j))
      vals <- actys[[j]]$vals
      if (is.null(vals) || !is.list(vals)) {
        note("vars[[%d]]$actys[[%d]]$vals is missing or is not a list", i, j)
        next
      }
      for (k in seq_along(vals)) {
        check_name(vals[[k]]$var, sprintf("vars[[%d]]$actys[[%d]]$vals[[%d]]$var", i, j, k))
        if (is.null(vals[[k]]$val)) {
          note("vars[[%d]]$actys[[%d]]$vals[[%d]]$val is missing", i, j, k)
        }
      }
    }
  }

  if (length(problems) > 0) {
    stop(sprintf("validate_env_data_json: %s is malformed:\n  - %s",
                 source_label, paste(problems, collapse = "\n  - ")),
         call. = FALSE)
  }

  invisible(TRUE)
}

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

# ── Strategic AME airframe selection ─────────────────────────────────────────
#
# The strategic aeromedical evacuation sortie carries the patient capacity of
# whichever airframe the run is configured to fly. Each airframe the model
# knows about is held as its own `role4.airframe_<id>` block (a label plus a
# critical/standard capacity pair, sourced per airframe), and
# `role4.ame.airframe` names the one in use. Keeping the pairs separate from
# the selection means a scenario profile, or the Shiny selector, changes the
# aircraft by naming it rather than by overwriting two capacity numbers whose
# provenance is then lost.

#' Resolves the strategic AME airframe a run is configured to fly
#'
#' @param role4_params `env_data$vars$role4` list — supplies the selector
#'   (`ame$airframe`) and the `airframe_<id>` capacity blocks
#' @return Named list with `id`, `label`, `critical_capacity` and
#'   `standard_capacity`
#'
#' @details Errors rather than falling back when the named airframe has no
#'   matching block, since a silent default would fly an aircraft the planner
#'   did not choose and report its capacity as theirs.
resolve_ame_airframe <- function(role4_params) {
  id <- role4_params$ame$airframe
  if (is.null(id)) {
    stop("resolve_ame_airframe: role4.ame.airframe is not set in env_data.json")
  }

  block <- role4_params[[paste0("airframe_", id)]]
  if (is.null(block)) {
    available <- sub("^airframe_", "", grep("^airframe_", names(role4_params), value = TRUE))
    stop(sprintf(
      "resolve_ame_airframe: unknown airframe '%s'. Available airframes: %s",
      id,
      if (length(available) == 0) "(none defined)" else paste(available, collapse = ", ")
    ))
  }

  list(
    id                = id,
    label             = if (!is.null(block$label)) block$label else id,
    critical_capacity = block$critical_capacity,
    standard_capacity = block$standard_capacity
  )
}

# ── Casualty rate generation (live, force-size-reactive) ───────────────────────
#
# Issue #18: casualty arrival rate is scaled by a time-varying effective force
# size (env's `effective_force_combat`/`effective_force_support` simmer
# globals — initialised in run_once(), decremented/incremented by
# R/trajectories.R at injury/return-to-duty events) rather than a fixed
# population constant. Because that global can only be known by actually
# running the simulation, arrival generation uses simmer's function-based
# generator mode: add_generator() is given a closure with no arguments that is
# called once per arrival, returning the interarrival gap.
#
# Issue #206: the closure previously drew a fresh rate for every simulated
# minute and emitted a casualty at each whole-casualty crossing of the running
# total. A day's count was therefore an average of 1,440 draws, and the central
# limit theorem flattened the stream long before the draws could reach a daily
# total: the combat WIA stream realised a daily standard deviation of 0.50
# against the 2.10 of a Poisson process at the same rate, and in 5,000
# simulated days never produced a day worse than six casualties. Peak-day
# volume is what drives contention for theatres, intensive care beds and
# airlift, so a generator that reproduces the mean while suppressing the peaks
# understates every queue the model exists to measure.
#
# The minute grid is replaced by direct arrival-time sampling, which fixes both
# the timescale the configured distribution acts on and the way arrivals are
# placed within it:
#
#   Intensity. The stream is a Cox process whose rate is redrawn once per
#   simulated day rather than once per minute. FORECAS (Blood, Zouris &
#   Rotblatt, 1998) fits a *daily* casualty rate, so `mean_daily`/`sd_daily`
#   are between-day quantities; drawing at that timescale is what makes the
#   configured distribution the between-day distribution it was fitted as.
#   Within a day the rate is constant apart from the live force-size factor.
#
#   Placement. Arrivals within the day are Poisson, sampled by thinning (Lewis
#   & Shedler, 1979): candidate gaps are drawn under a dominating rate that
#   holds the pool at establishment strength, and each candidate is accepted
#   with probability F/P_max for the force size F read at that point. A
#   candidate falling past the day's end is discarded and sampling restarts at
#   the boundary under the next day's rate, which is exact for a
#   piecewise-constant intensity by the memorylessness of the exponential.
#
# Together these give a daily count that is Poisson conditional on the day's
# rate, so by the law of total variance the stream realises
#
#   E[N] = mu * P / 1000            Var[N] = mu * P / 1000 + (sigma * P / 1000)^2
#
# per day at force size P: the configured mean is preserved, and the configured
# between-day standard deviation is honoured on top of the Poisson term rather
# than averaged away. scripts/check_arrival_rate_fidelity.R asserts both.
#
# P_max has to bound F for the whole run, or the acceptance probability
# saturates at 1 and the stream generates at the dominating rate instead of the
# true one. With reinforcement disabled, the shipped default, establishment
# strength is that bound: every casualty debits the pool and every return to
# duty credits back a casualty already debited, so nothing can carry F above
# the value it starts at. Reinforcement can, a package larger than the
# shortfall it was requested against leaving the pool over strength, so an
# enabled configuration needs a wider bound: reinforcement_force_bound()
# computes it below.

#' The largest force size a reinforcement configuration can produce
#'
#' @param initial Establishment strength of the pool
#' @return An upper bound on the pool's effective force size for the whole run
#'
#' @details A cycle's demand is the shortfall floored at zero, net of what
#'   earlier still-pending cycles have claimed, so the demands in flight at any
#'   moment sum to at most `initial` and the fill they deliver to at most
#'   `fill_max_frac x initial`. An over-strength pool has zero shortfall and so
#'   requests nothing, which is what stops surpluses accumulating across
#'   cycles. The worst case is therefore one delivery landing on a pool that has
#'   recovered to full strength in the meantime, giving
#'   `initial x (1 + fill_max_frac)`.
#'
#'   This is a guaranteed bound rather than a realised one, which is what
#'   thinning needs: the dominating rate has to dominate everywhere, not
#'   usually. Widening it costs proposal draws in proportion, since a day
#'   proposes `X * P_max / 1000` candidates and rejects the ones the live force
#'   size does not support. That cost falls only on runs that enable
#'   reinforcement; a disabled configuration returns `initial` unchanged and
#'   samples exactly as it did before.
reinforcement_force_bound <- function(initial) {
  reinf <- env_data$vars$force_regeneration$reinforcement
  if (is.null(reinf) || is.null(reinf$demand_interval_days) ||
      is.na(reinf$demand_interval_days) || reinf$demand_interval_days <= 0) {
    return(initial)
  }

  fill_max <- reinf$fill_max_frac
  if (is.null(fill_max) || is.na(fill_max) || fill_max <= 1) return(initial)

  ceiling(initial * (1 + fill_max))
}

#' Builds the thinning arrival closure shared by both generator families
#'
#' @param draw_daily_rate Zero-argument function returning one day's rate, in
#'   casualties per day per 1,000 personnel
#' @param force_global Name of the simmer global holding the current effective
#'   force size for this stream's population pool
#' @param force_bound Upper bound on that pool's force size for the whole run,
#'   and so the dominating rate's population term (reinforcement_force_bound())
#' @param n_days Duration in days
#' @return A zero-argument function suitable for add_generator()'s
#'   `distribution` argument: returns the next interarrival gap (simulation
#'   minutes), or -1 once n_days has been exhausted (simmer's convention for
#'   ending a generator)
#'
#' @details The two families differ only in how a day's rate is drawn, so the
#'   sampler itself lives here and each constructor supplies `draw_daily_rate`.
#'
#'   Arrival times are continuous, so no jitter step is needed to separate
#'   them: two streams tie with probability zero, which is what the retired
#'   minute grid needed a sub-minute offset to achieve.
#'
#'   The force size is read once per candidate. simmer calls this closure at
#'   the previous arrival's time, so the clock does not advance while it runs
#'   and every candidate within one call sees the same value; that was equally
#'   true of the retired minute walk, which re-read the global at each of its
#'   minutes without the clock having moved between them. The coupling is
#'   therefore as live as a generator closure can be, resolving at each arrival
#'   rather than at each of the arrivals the run has yet to produce.
#'
#'   Cost is linear in the drawn rate rather than fixed by the horizon: a day
#'   proposes `X * P_max / 1000` candidates in expectation, where the minute
#'   walk performed 1,440 iterations whatever the draws. The expectation is
#'   finite for both families, so a heavy-tailed draw costs time in proportion
#'   to the casualties it generates.
make_thinned_arrival_generator <- function(draw_daily_rate, force_global,
                                           force_bound, n_days) {
  n_minutes <- day_min * n_days

  if (force_bound <= 0) return(function() -1)

  t          <- 0
  last_time  <- 0
  day_end    <- 0
  lambda_bar <- 0

  function() {
    repeat {
      if (t >= n_minutes) return(-1)

      if (t >= day_end) {
        day_end    <<- (floor(t / day_min) + 1) * day_min
        lambda_bar <<- draw_daily_rate() * force_bound / (1000 * day_min)
      }

      # A rate of zero generates nothing for the rest of the day.
      if (lambda_bar <= 0) {
        t <<- day_end
        next
      }

      t <<- t - log(1 - runif(1)) / lambda_bar

      if (t >= day_end) {
        t <<- day_end
        next
      }
      if (t >= n_minutes) return(-1)

      if (runif(1) * force_bound < get_global(env, force_global)) {
        gap <- t - last_time
        last_time <<- t
        return(gap)
      }
    }
  }
}

#' Builds a live, force-size-reactive lognormal arrival generator closure
#'
#' @param mean_daily Expected daily rate
#' @param sd_daily Standard deviation of daily rate
#' @param force_global Name of the simmer global holding the current
#'   effective force size for this stream's population pool (e.g.
#'   "effective_force_combat")
#' @param force_bound Upper bound on that pool's force size for the whole run
#' @param n_days Duration in days
#' @return A zero-argument function suitable for add_generator()'s
#'   `distribution` argument: returns the next interarrival gap (simulation
#'   minutes), or -1 once n_days has been exhausted (simmer's convention for
#'   ending a generator)
#'
#' @details The day's rate is drawn from the configured lognormal untrimmed, so
#'   the stream realises the mean and the coefficient of variation its
#'   configuration names, and editing `sd_daily` alone leaves the realised mean
#'   alone. Both properties are asserted by
#'   scripts/check_arrival_rate_fidelity.R, alongside the realised daily
#'   variance.
make_ln_arrival_generator <- function(mean_daily, sd_daily, force_global,
                                      force_bound, n_days) {
  sigma_log <- sqrt(log(1 + (sd_daily^2 / mean_daily^2)))
  mu_log    <- log(mean_daily^2 / sqrt(sd_daily^2 + mean_daily^2))

  make_thinned_arrival_generator(
    function() qlnorm(runif(1), meanlog = mu_log, sdlog = sigma_log),
    force_global, force_bound, n_days
  )
}

#' Builds a live, force-size-reactive exponential arrival generator closure
#'
#' @param mean_daily Expected daily rate. Fully parameterises the exponential
#'   rate distribution (rate = 1 / mean_daily); unlike
#'   make_ln_arrival_generator(), there is no separate sd_daily shape
#'   parameter for a single-parameter exponential distribution.
#' @param force_global Name of the simmer global holding the current
#'   effective force size for this stream's population pool
#' @param force_bound Upper bound on that pool's force size for the whole run
#' @param n_days Duration in days
#' @return A zero-argument function suitable for add_generator()'s
#'   `distribution` argument (see make_ln_arrival_generator())
#'
#' @details FORECAS (Blood, Zouris & Rotblatt, 1998) fits lognormal and
#'   exponential distributions to different battle intensities/troop types.
#'   Used for the high_intensity scenario profile (Issue #54), whose
#'   higher-intensity casualty streams are exponential-distributed rather
#'   than lognormal-distributed like the moderate_intensity/default streams.
#'   An exponential's standard deviation equals its mean, so such a stream's
#'   between-day variation is fixed by `mean_daily` alone.
make_exp_arrival_generator <- function(mean_daily, force_global, force_bound,
                                       n_days) {
  make_thinned_arrival_generator(
    function() qexp(runif(1), rate = 1 / mean_daily),
    force_global, force_bound, n_days
  )
}

#' Dispatches to the appropriate live arrival generator for a casualty stream
#'
#' @param gen_vars List with mean_daily and (for lognormal streams) sd_daily,
#'   as read from env_data$vars$generators[[type]]; an optional `distribution`
#'   field selects "lognormal" (default, if absent) or "exponential"
#' @param force_global Name of the simmer global holding the current
#'   effective force size for this stream's population pool
#' @param force_bound Upper bound on that pool's force size for the whole run
#' @param n_days Duration in days
#' @return A zero-argument distribution function (see
#'   make_ln_arrival_generator())
generate_casualty_arrivals <- function(gen_vars, force_global, force_bound, n_days) {
  distribution <- if (!is.null(gen_vars$distribution)) gen_vars$distribution else "lognormal"

  if (distribution == "exponential") {
    make_exp_arrival_generator(gen_vars$mean_daily, force_global, force_bound, n_days)
  } else {
    make_ln_arrival_generator(gen_vars$mean_daily, gen_vars$sd_daily, force_global,
                              force_bound, n_days)
  }
}

#' Wraps a background arrival generator closure to interleave pre-computed
#' mass casualty events into the same generator stream
#'
#' @param background_fn A zero-argument distribution function as returned by
#'   generate_casualty_arrivals() (the wia_cbt or kia_cbt combat stream)
#' @param mass_casualty_times Sorted numeric vector of mass casualty casualty
#'   arrival times (simulation minutes), as returned by
#'   generate_mass_casualty_events()$arrival_times
#' @param mass_casualty_ids Integer vector parallel to mass_casualty_times
#'   giving each casualty's 1-indexed source event id
#' @param id_sink Name of the global vector this wrapper appends each
#'   emitted entity's event id to — "wia_cbt_mass_casualty_event_id" for
#'   the wounded overlay, "kia_cbt_mass_casualty_event_id" for the
#'   immediate-killed one
#' @return A zero-argument distribution function that, on each call, emits
#'   whichever of (next background candidate, next pending mass casualty
#'   arrival) is chronologically earliest, preserving a single strictly
#'   ordered arrival stream through one generator/trajectory
#'
#' @details Mass casualty timing is exogenous (an imposed shock, not
#'   population-scaled — see README Casualty Generation), so it is still
#'   computed up front by generate_mass_casualty_events() exactly as before;
#'   only the background stream is force-size-reactive. As a side effect,
#'   appends 0 (background) or the event id (mass casualty) to the global
#'   `id_sink` vector in strict emission order, which
#'   build_casualty_trajectory() indexes by each entity's generator-assigned
#'   position to recover its mass_casualty_event_id attribute. Two streams
#'   are wrapped, the combat wounded and the combat killed, each keeping its
#'   own sink because each generator numbers its own entities from zero.
wrap_with_mass_casualty <- function(background_fn, mass_casualty_times, mass_casualty_ids,
                                    id_sink = "wia_cbt_mass_casualty_event_id") {
  mc_ptr <- 1L
  n_mc <- length(mass_casualty_times)
  pending_bg <- NA_real_
  bg_exhausted <- FALSE
  last_time <- 0

  # The sink is global rather than an enclosed vector because the trajectory
  # reads it by name at run time (R/trajectories.R); assign() rather than
  # <<- because the name is a parameter. In a forked mclapply worker this
  # reaches only that fork's own global environment, as every other <<- in
  # the run path does.
  append_event_id <- function(id) {
    assign(id_sink, c(get(id_sink, envir = globalenv()), id), envir = globalenv())
  }

  function() {
    if (is.na(pending_bg) && !bg_exhausted) {
      gap <- background_fn()
      if (gap < 0) {
        bg_exhausted <<- TRUE
        pending_bg <<- NA_real_
      } else {
        pending_bg <<- last_time + gap
      }
    }

    mc_due <- mc_ptr <= n_mc && (bg_exhausted || mass_casualty_times[mc_ptr] <= pending_bg)

    if (mc_due) {
      t  <- mass_casualty_times[mc_ptr]
      id <- mass_casualty_ids[mc_ptr]
      mc_ptr <<- mc_ptr + 1L
      append_event_id(id)
    } else {
      if (bg_exhausted) return(-1)
      t <- pending_bg
      pending_bg <<- NA_real_
      append_event_id(0L)
    }

    gap <- t - last_time
    last_time <<- t
    gap
  }
}

#' Reconstructs the data/arrivals_<type>.txt diagnostic files from a
#' completed run's monitored arrivals
#'
#' @param env A simmer environment that has already been run() to completion
#' @param data_dir Directory to write the arrivals_<type>.txt files into
#'   (default "data", the tracked baseline location). run_bch() passes a
#'   run-scoped directory under outputs/ unless the caller has explicitly
#'   asked to refresh the tracked baseline, so that an ordinary run cannot
#'   overwrite tracked evidence (Issue #154).
#' @return Invisibly NULL; called for its file-writing side effect
#'
#' @details The six background casualty streams' arrival times are no
#'   longer known before run() — they depend on the live, force-size-
#'   reactive generators above — so the arrival-time diagnostics previously
#'   written inside generate_ln_arrivals()/generate_exp_arrivals() are
#'   instead reconstructed here from get_mon_arrivals() after the run
#'   completes, filtered by each stream's generator-name prefix. Mass
#'   casualty's diagnostic file is unaffected (still written by
#'   generate_mass_casualty_events(), since that stream remains pre-computed).
write_arrival_diagnostics <- function(env, data_dir = "data") {
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  arr <- get_mon_arrivals(env)
  streams <- c("wia_cbt", "kia_cbt", "dnbi_cbt", "wia_spt", "kia_spt", "dnbi_spt")
  for (type in streams) {
    times <- sort(arr$start_time[startsWith(arr$name, type)])
    write.table(times, file = file.path(data_dir, paste0("arrivals_", type, ".txt")),
               row.names = FALSE, col.names = FALSE)
  }
  invisible(NULL)
}

#' Draws event start times for the "poisson" mass casualty mode
#'
#' @param n_days Duration in days
#' @param event_params List with rate_per_day, as read from
#'   env_data$vars$mass_casualty$event
#' @return Numeric vector of event start times (simulation minutes),
#'   ascending; empty if rate_per_day <= 0
#'
#' @details Event inter-arrival times are drawn from an
#'   Exponential(rate_per_day) distribution via inverse-CDF. `rate_per_day = 0`
#'   returns immediately with no RNG draws consumed, so the stream
#'   downstream of this call is unaffected — the basis for Issue #9's
#'   disable-path acceptance criterion.
mass_casualty_event_starts_poisson <- function(n_days, event_params) {
  n_minutes    <- day_min * n_days
  rate_per_min <- event_params$rate_per_day / day_min

  if (rate_per_min <= 0) return(numeric(0))

  event_starts <- c()
  t <- 0
  repeat {
    t <- t - log(1 - runif(1)) / rate_per_min
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
mass_casualty_event_starts_scheduled <- function(n_days, schedule_params) {
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

  fire <- runif(n) < probs
  if (!any(fire)) return(empty)

  starts <- (days[fire] - 1) * day_min + runif(sum(fire)) * day_min

  out <- data.frame(start = starts, min_cas = min_cas[fire], max_cas = max_cas[fire],
                    pri_one = pri_one[fire], pri_two = pri_two[fire], pri_three = pri_three[fire])
  out <- out[out$start >= 0 & out$start < n_minutes, , drop = FALSE]
  out[order(out$start), , drop = FALSE]
}

#' Draws casualty arrival times, count, injection window, and wounded/killed
#' split for one mass casualty event
#'
#' @param event_start Event start time (simulation minutes)
#' @param event_params List with min_cas, max_cas, window_min, window_mode,
#'   window_max, kia_fraction, as read from
#'   env_data$vars$mass_casualty$event
#' @param n_minutes Total simulation duration in minutes (arrivals at or
#'   after this are dropped)
#' @return Named list: `times` (numeric vector of wounded casualty arrival
#'   times), `kia_times` (numeric vector of immediate-killed arrival times),
#'   and `window` (the drawn injection window duration, minutes)
#'
#' @details The event's drawn casualty count is a total, not a survivor
#'   count, and is split between the two pathways by a single
#'   Binomial(n, kia_fraction) draw. The killed are then taken as the first
#'   `n_kia` of the *unsorted* offsets: those offsets are independent
#'   Uniform(0, window) draws and so are exchangeable, which makes the
#'   subset a uniformly random one without spending a further draw on
#'   choosing it. Each pathway's own offsets are sorted afterwards, since
#'   the caller merges each into a chronological stream. `kia_fraction = 0`
#'   consumes no additional draw beyond the binomial itself and yields an
#'   empty `kia_times`.
mass_casualty_event_casualties <- function(event_start, event_params, n_minutes) {
  n_cas_draw <- round(event_params$min_cas +
                        runif(1) * (event_params$max_cas - event_params$min_cas))

  window <- rtriangle(1, a = event_params$window_min, b = event_params$window_max,
                      c = event_params$window_mode)

  kia_fraction <- if (!is.null(event_params$kia_fraction)) event_params$kia_fraction else 0
  n_kia <- rbinom(1, n_cas_draw, kia_fraction)

  offsets <- runif(n_cas_draw) * window
  kia_offsets <- sort(offsets[seq_len(n_kia)])
  wia_offsets <- sort(offsets[seq_len(n_cas_draw - n_kia) + n_kia])

  in_run <- function(offs) {
    t <- event_start + offs
    t[t >= 0 & t < n_minutes]
  }

  list(times = in_run(wia_offsets), kia_times = in_run(kia_offsets), window = window)
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
#' @param write_file Write the arrival stream and event log to `data_dir`
#'   (default TRUE; set FALSE for parallel replication workers to avoid
#'   file-write conflicts)
#' @param data_dir Directory the two diagnostic files are written to when
#'   `write_file` is TRUE (default "data", the tracked baseline location).
#'   Threaded from run_once() so that only an explicit baseline refresh
#'   writes to the tracked directory (Issue #154).
#' @return Named list: `arrival_times` (sorted numeric vector of individual
#'   wounded casualty arrival times, simulation minutes),
#'   `casualty_event_id` (integer vector parallel to `arrival_times`,
#'   giving the 1-indexed event each casualty belongs to — matches
#'   `events$event_id`; consumed by build_casualty_trajectory() for
#'   per-event priority lookup), `kia_arrival_times` and
#'   `kia_casualty_event_id` (the same pair for the event's immediate
#'   killed, overlaid on the `kia_cbt` stream instead), and `events`
#'   (data frame with one row per event: event_id, event_start, n_cas,
#'   n_wia, n_kia, window_min, pri_one, pri_two, pri_three — `n_cas` is
#'   the event's total, of which `n_wia` and `n_kia` are the two
#'   pathways; the pri_* columns are NA for "poisson"-mode events,
#'   meaning "use the shared params$priority split"; used for the mass
#'   casualty event timeline plot in R/analysis.R)
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
#'   customisable per event in either mode. That count is a total, of which
#'   a Binomial(n, `event$kia_fraction`) share are immediate killed,
#'   returned separately so the caller can overlay them on the `kia_cbt`
#'   stream and leave them to the mortuary pathway the background killed
#'   stream already takes; the fraction is shared by both modes, as the
#'   injection window is. An event schedule/rate that
#'   produces zero events returns an empty arrival stream — background
#'   lognormal generation is unaffected, satisfying Issue #9's
#'   disable-path acceptance criterion (shipped default: "poisson" mode,
#'   rate_per_day = 0).
generate_mass_casualty_events <- function(n_days, params, seed = NULL,
                                          write_file = TRUE, data_dir = "data") {
  if (!is.null(seed)) set.seed(seed)
  if (write_file) dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  n_minutes <- day_min * n_days
  mode <- if (!is.null(params$event$mode)) params$event$mode else "poisson"

  empty_events <- data.frame(event_id = integer(0), event_start = numeric(0),
                             n_cas = integer(0), n_wia = integer(0), n_kia = integer(0),
                             window_min = numeric(0),
                             pri_one = numeric(0), pri_two = numeric(0), pri_three = numeric(0))

  sched <- if (identical(mode, "scheduled")) {
    mass_casualty_event_starts_scheduled(n_days, params$schedule)
  } else {
    starts <- mass_casualty_event_starts_poisson(n_days, params$event)
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
      write.table(numeric(0), file = file.path(data_dir, "arrivals_mass_casualty.txt"),
                 row.names = FALSE, col.names = FALSE)
      write.csv(empty_events, file.path(data_dir, "mass_casualty_events.csv"),
               row.names = FALSE)
    }
    return(list(arrival_times = numeric(0), casualty_event_id = integer(0),
                kia_arrival_times = numeric(0), kia_casualty_event_id = integer(0),
                events = empty_events))
  }

  arrival_times         <- c()
  casualty_event_id     <- c()
  kia_arrival_times     <- c()
  kia_casualty_event_id <- c()
  window_dur            <- c()
  n_wia_actual          <- c()
  n_kia_actual          <- c()

  for (i in seq_len(nrow(sched))) {
    event_params <- list(min_cas = sched$min_cas[i], max_cas = sched$max_cas[i],
                         window_min = params$event$window_min, window_mode = params$event$window_mode,
                         window_max = params$event$window_max,
                         kia_fraction = params$event$kia_fraction)
    cas <- mass_casualty_event_casualties(sched$start[i], event_params, n_minutes)

    arrival_times     <- c(arrival_times, cas$times)
    casualty_event_id <- c(casualty_event_id, rep(i, length(cas$times)))
    kia_arrival_times     <- c(kia_arrival_times, cas$kia_times)
    kia_casualty_event_id <- c(kia_casualty_event_id, rep(i, length(cas$kia_times)))
    window_dur         <- c(window_dur, cas$window)
    n_wia_actual        <- c(n_wia_actual, length(cas$times))
    n_kia_actual        <- c(n_kia_actual, length(cas$kia_times))
  }

  # Sort arrivals but keep casualty_event_id correctly paired per-casualty
  # (order(), not sort(), so the two vectors share one permutation). The
  # wounded and killed streams are sorted separately because each is
  # merged into a different background generator downstream.
  ord <- order(arrival_times)
  arrival_times     <- arrival_times[ord]
  casualty_event_id <- casualty_event_id[ord]

  kia_ord <- order(kia_arrival_times)
  kia_arrival_times     <- kia_arrival_times[kia_ord]
  kia_casualty_event_id <- kia_casualty_event_id[kia_ord]

  events <- data.frame(
    event_id    = seq_len(nrow(sched)),
    event_start = sched$start,
    n_cas       = n_wia_actual + n_kia_actual,
    n_wia       = n_wia_actual,
    n_kia       = n_kia_actual,
    window_min  = window_dur,
    pri_one     = sched$pri_one,
    pri_two     = sched$pri_two,
    pri_three   = sched$pri_three
  )

  if (write_file) {
    # Both pathways' arrivals, since the file records when an event's
    # casualties reach the system rather than which stream carries them.
    write.table(sort(c(arrival_times, kia_arrival_times)),
               file = file.path(data_dir, "arrivals_mass_casualty.txt"),
               row.names = FALSE, col.names = FALSE)
    write.csv(events, file.path(data_dir, "mass_casualty_events.csv"), row.names = FALSE)
  }

  list(arrival_times = arrival_times, casualty_event_id = casualty_event_id,
       kia_arrival_times = kia_arrival_times, kia_casualty_event_id = kia_casualty_event_id,
       events = events)
}

# ── Simmer environment construction ─────────────────────────────────────────

#' Reads the configured operating theatre shift length from a built env_data
#'
#' @param env_data Built environment list (build_environment() output)
#' @return Numeric hours per day the first OT shift is active
#'
#' @details The single source of truth for OT shift length is
#'   `vars.surgical_roster.shift.ot_hours` in env_data.json. Every caller
#'   that used to carry its own literal default reaches the value through
#'   here, so changing the configuration file changes the shift length
#'   everywhere at once. A configuration missing the field is an error
#'   rather than a silently substituted default, on the same basis as every
#'   other required parameter in the vars tree.
get_ot_hours <- function(env_data) {
  v <- env_data$vars$surgical_roster$shift$ot_hours
  if (is.null(v)) {
    stop("env_data.json is missing vars.surgical_roster.shift.ot_hours ",
         "(operating theatre shift length).")
  }
  as.numeric(v)
}

#' Minutes from a simulation time until the surgical roster next opens a shift
#'
#' @param t Simulation time in minutes
#' @return Numeric minutes until the currently closed shift reopens, or Inf
#'   when the roster leaves one shift permanently closed
#'
#' @details The roster is two alternating shifts over a 1,440-minute day, the
#'   first covering 0 to `ot_shift_break_min` and the second the remainder
#'   (build_env() below). A section that is closed at time `t` is therefore
#'   the second shift when `t` falls before the break and the first shift when
#'   it falls at or after it, which fixes the next opening without needing to
#'   know which of the two the caller's section is on. Callers reach this only
#'   after finding a section closed, so the ambiguous case (both shifts open,
#'   which cannot happen) never arises. A degenerate roster, one whose break
#'   falls at either end of the day and so leaves a shift that never opens,
#'   returns Inf.
minutes_to_shift_open <- function(t) {
  brk <- ot_shift_break_min
  if (is.null(brk) || is.na(brk) || brk <= 0 || brk >= DAY_MIN) return(Inf)
  m <- t %% DAY_MIN
  if (m < brk) brk - m else DAY_MIN - m
}

#' Initializes the simmer environment by adding all resources from env_data
#'
#' @param env A simmer environment object
#' @param env_data Nested list defining resources for each echelon/unit type
#' @param ot_hours Hours per day that the first OT shift is active. NULL
#'   (the default) reads the configured value from `env_data`; an explicit
#'   value overrides it for this build only, which is how sensitivity
#'   screening and the Shiny Run tab vary OT availability without editing
#'   the configuration. Shift 1 covers 0 to ot_hours; Shift 2 covers
#'   ot_hours to 24.
#' @return Modified simmer environment with all resources added
#'
#' @details Schedules operating theatre shifts for surgical staff and beds.
#' Counters r2e_surg_counter, r2e_ot_bed_counter, r2b_surg_counter, and
#' r2b_ot_bed_counter alternate shift assignments across teams.
build_env <- function(env, env_data, ot_hours = NULL) {
  if (is.null(ot_hours)) ot_hours <- get_ot_hours(env_data)
  ot_break   <- as.integer(ot_hours * 60L)
  ot_shift_1 <- simmer::schedule(c(0, ot_break),        c(1, 0), period = DAY_MIN)
  ot_shift_2 <- simmer::schedule(c(ot_break, DAY_MIN), c(1, 0), period = DAY_MIN)

  # Published for minutes_to_shift_open() above, which trajectory closures call
  # to find how long a closed surgical section has left before it reopens. The
  # break is republished here rather than re-read from env_data because an
  # explicit ot_hours argument overrides the configured value for this build
  # only, and the trajectories must see the roster they are actually running
  # against. Global assignment mirrors env/env_data (R/replication.R); in
  # forked mclapply workers it modifies only the fork's own state.
  ot_shift_break_min <<- ot_break

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
