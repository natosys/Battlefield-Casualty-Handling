##############################################
## R/environment.R                          ##
## Environment construction and data import ##
##############################################

library(jsonlite)
library(simmer)
library(simmer.bricks)
library(triangle)

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
          bed_ids <- paste0("b_", elm_name, "_", bed_type, "_", seq_len(bed$qty), "_t", i)
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
      transports_list[[vehicle$name]] <- paste0("t_", vehicle$name, "_", seq_len(vehicle$qty))
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
#' @return Vector of arrival times in simulation minutes
generate_ln_arrivals <- function(type, mean_daily, sd_daily, pop, n_days,
                                 cap = 5, seed = NULL, write_file = TRUE) {
  if (!is.null(seed)) set.seed(seed)

  n_minutes <- day_min * n_days

  mu_log    <- log(mean_daily^2 / sqrt(sd_daily^2 + mean_daily^2))
  sigma_log <- sqrt(log(1 + (sd_daily^2 / mean_daily^2)))

  rates      <- pmin(rlnorm(n_minutes, meanlog = mu_log, sdlog = sigma_log), cap)
  rates      <- rates / 1440 * pop / 1000
  cumulative <- cumsum(rates)

  arrival_idx   <- which(floor(cumulative) > floor(cumulative - rates))
  arrival_times <- sort(arrival_idx + runif(length(arrival_idx), 0, 1))

  if (write_file) {
    filename <- file.path("data", paste0("arrivals_", type, ".txt"))
    write.table(arrival_times, file = filename, row.names = FALSE, col.names = FALSE)
  }

  return(arrival_times)
}

# ── Simmer environment construction ─────────────────────────────────────────

#' Initializes the simmer environment by adding all resources from env_data
#'
#' @param env A simmer environment object
#' @param env_data Nested list defining resources for each echelon/unit type
#' @return Modified simmer environment with all resources added
#'
#' @details Schedules operating theatre shifts for surgical staff and beds:
#'   - Shift 1: 0000–1200 (ot_shift_1)
#'   - Shift 2: 1200–2400 (ot_shift_2)
#'
#' Counters r2e_surg_counter, r2e_ot_bed_counter, r2b_surg_counter, and
#' r2b_ot_bed_counter alternate shift assignments across teams.
build_env <- function(env, env_data, ot_hours = 12) {
  ot_break <- as.integer(ot_hours * 60)
  ot_shift_1 <- simmer::schedule(c(0, ot_break),        c(1, 0), period = 1440)
  ot_shift_2 <- simmer::schedule(c(ot_break, 1440), c(1, 0), period = 1440)

  r2e_surg_counter   <- 1
  r2e_ot_bed_counter <- 1
  r2b_surg_counter   <- 1
  r2b_ot_bed_counter <- 1

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
                if (elm_type == "r2eheavy") {
                  if (r2e_ot_bed_counter %% 2 == 1) {
                    env <- env %>% add_resource(res_name)
                  } else {
                    env <- env %>% add_resource(res_name, ot_shift_1)
                  }
                  r2e_ot_bed_counter <- r2e_ot_bed_counter + 1
                } else {
                  team_shift <- if (r2b_ot_bed_counter %% 2 == 1) ot_shift_1 else ot_shift_2
                  env <- env %>% add_resource(res_name, team_shift)
                  r2b_ot_bed_counter <- r2b_ot_bed_counter + 1
                }

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
