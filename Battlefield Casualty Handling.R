##############################################
## ENVIRONMENT SETUP                        ##
##############################################

#' Clears workspace and initializes libraries
rm(list = ls())
gc()
set.seed(42)

# Load required libraries
library(simmer)
library(simmer.bricks)
library(simmer.plot)
library(ggplot2)
library(scales)
library(reshape2)
library(truncnorm)
library(jsonlite)
library(triangle)

# Import data import functions
source("data_import.R")

# Create the simulation environment
env <- simmer("Battlefield Casualty Handling")

# Load environment data from JSON file
env_data <- load_elms("env_data.json")

##############################################
## CONSTANTS                                ##
##############################################
# Counts of different element types (r1, r2b, r2eheavy, etc)
counts <- sapply(env_data$elms, length)
# Duration of a day in simulation time units (minutes)
day_min <- 1440
total_population <- env_data$pops$combat + env_data$pops$support
# Calculate Combat Ineffective (CIE) threshold
cie_threshold <- (2/3) * total_population
n_days <- 30
n_iterations <- 1000

##############################################
## CASUALTY RATE GENERATION                 ##
##############################################

#' Generate Lognorm arrival timestamps using capped log-normal rates
#' @param mean_daily expected daily rate
#' @param sd_daily standard deviation of daily rate
#' @param pop size of target population
#' @param n_days duration in days
#' @return vector of arrival times in simulation minutes
generate_ln_arrivals <- function(type, mean_daily, sd_daily, pop, n_days, cap = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  n_minutes <- day_min * n_days
  
  # Step 1: Convert mean/SD to log-space
  mu_log <- log(mean_daily^2 / sqrt(sd_daily^2 + mean_daily^2))
  sigma_log <- sqrt(log(1 + (sd_daily^2 / mean_daily^2)))
  
  # Step 2: Generate capped per-minute rates
  rates <- pmin(rlnorm(n_minutes, meanlog = mu_log, sdlog = sigma_log), cap)
  rates <- rates / 1440 * pop / 1000
  
  # Step 3: Compute cumulative arrivals
  cumulative <- cumsum(rates)
  
  # Step 4: Extract timestamps where arrivals occur
  arrival_idx <- which(floor(cumulative) > floor(cumulative - rates))
  
  # Step 5: Add random jitter within minute and sort
  arrival_times <- sort(arrival_idx + runif(length(arrival_idx), 0, 1))
  
  # Step 6: Write to file
  filename <- paste0("data/arrivals_", type, ".txt")
  write.table(arrival_times, file = filename, row.names = FALSE, col.names = FALSE)
  
  return(arrival_times)
}

##############################################
## Environment Initialisation               ##
##############################################

#' Initializes simulation environment by adding resources from structured env_data
#'
#' @param env A simmer environment object to populate with resources
#' @param env_data Nested list defining resources for each echelon/team/unit type
#'
#' @return A modified simmer environment with scheduled and unscheduled resources added
#'
#' @details
#' This function sets up all treatment, transport, and staff resources from the env_data input.
#' It includes scheduled operating theatre shifts for surgical staff and beds, with role-specific
#' configuration for R2B and R2EHeavy medical teams.
#'
#' ### Resource Scheduling
#' - Shift 1: 0000–1200 (ot_shift_1)
#' - Shift 2: 1200–2400 (ot_shift_2)
#'
#' ### Counters for Scheduling Logic
#' - `r2e_surg_counter`: assigns operating schedules to R2E surgical teams
#' - `r2e_ot_bed_counter`: controls scheduling for R2E OT beds
#' - `r2b_surg_counter`: alternates schedules across R2B teams
#'
build_env <- function(env, env_data) {
  #' Define operating theatre resource schedules
  ot_shift_1 <- simmer::schedule(c(0, 720), c(1, 0), period = 1440)
  ot_shift_2 <- simmer::schedule(c(720, 1440), c(1, 0), period = 1440)
  
  r2e_surg_counter <- 1
  r2e_ot_bed_counter <- 1
  r2b_surg_counter <- 1
  r2b_ot_bed_counter <- 1
  
  # Add resources based on `elms`
  for (elm_type in names(env_data$elms)) {
    for (team in env_data$elms[[elm_type]]) {
      if (is.character(team)) {
        for (res_name in team) {
          env <- env %>% add_resource(res_name)
        }
        
      } else if (is.list(team)) {
        apply_schedule <- elm_type %in% c("r2b", "r2eheavy")
        surg_index <- 1
        
        if (apply_schedule) {
          for (section_name in names(team)) {
            section <- team[[section_name]]
            
            for (res_name in section) {
              if (section_name == "surg") {
                if (elm_type == "r2b") {
                  if (r2b_surg_counter %% 2 == 1) {
                    team_shift <- ot_shift_1
                  } else {
                    team_shift <- ot_shift_2
                  }
                  env <- env %>% add_resource(res_name, team_shift)
                  r2b_surg_counter <- r2b_surg_counter + 1
                  
                } else if (elm_type == "r2eheavy") {
                  if (r2e_surg_counter %% 2 == 1) {
                    team_shift <- ot_shift_1
                  } else {
                    team_shift <- ot_shift_2
                  }
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
                  if (r2b_ot_bed_counter %% 2 == 1) {
                    team_shift <- ot_shift_1
                  } else {
                    team_shift <- ot_shift_2
                  }
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
  
  # Add transports
  for (transport_type in names(env_data$transports)) {
    for (res_name in env_data$transports[[transport_type]]) {
      env <- env %>% add_resource(res_name)
    }
  }
  
  return(env)
}

env <- build_env(env, env_data)

##############################################
## HELPER FUNCTIONS                         ##
##############################################

#' Seizes one unit of each resource in the provided list
#'
#' @param trj a simmer trajectory object
#' @param resources a character vector of resource names to be seized
#'
#' @return the modified trajectory with seize activities appended
#'
#' @details This helper function is used to acquire multiple resources simultaneously. 
#'          Commonly applied during casualty intake, surgery preparation, or 
#'          evacuation staging phases where grouped resource allocation is required.
seize_resources <- function(trj, resources) {
  for (res in resources) {
    trj <- trj %>% seize(res, 1)
  }
  trj
}

#' Releases one unit of each resource in the provided list
#'
#' @param trj a simmer trajectory object
#' @param resources a character vector of resource names to be released
#'
#' @return the modified trajectory with release activities appended
#' 
#' @details This utility is used to efficiently release multiple resources
#'          that were previously seized within a trajectory.
#'          Commonly applied after recovery, surgery, or casualty handoff phases.
release_resources <- function(trj, resources) {
  for (res in resources) {
    trj <- trj %>% release(res, 1)
  }
  trj
}

#' Randomly selects one subteam of the specified type from the given team
#'
#' @param elm_type element type (e.g. "r1", "r2b", "r2eheavy") identifying the unit category
#' @param team_id index of the team within the specified element type
#' @param subteam_type string identifying the subteam type to select (e.g. "surg", "evac")
#'
#' @return the name of a randomly selected subteam as a character string
#'
#' @details This function accesses nested team data stored in `env_data$elms`, then randomly selects
#'          one member from the specified subteam type (e.g. within a modular R2B or R2E team).
#'          It throws an error if no such subteam exists or the list is empty.
#'          Commonly used for resource allocation, staffing, or routing logic within simulation trajectories.
select_subteam <- function(elm_type, team_id, subteam_type) {
  subteams <- env_data$elms[[elm_type]][[team_id]][[subteam_type]]
  
  if (is.null(subteams) || length(subteams) == 0) {
    stop(sprintf("No subteams of type '%s' found for %s team %d", subteam_type, elm_type, team_id))
  }
  
  index <- sample(seq_along(subteams), 1)
  return(subteams[[index]])
}

#' Selects a randomly ordered R2B team with at least one free OT bed
#'
#' @param env the simmer simulation environment object
#'
#' @return the index of the selected R2B team (integer), or -1 if none are available
#'
#' @details Iterates through R2B teams in randomized order and checks current usage
#'          of their associated operating theatre (OT) beds using `get_server_count()`.
#'          Returns the first team whose OT beds are completely free (i.e., zero usage).
#'          If all teams are occupied, returns -1. Useful for routing casualties to 
#'          damage-control surgery pathways during simulation flow.
#'
#'          Logs bed usage and selection outcomes using `cat()` for real-time visibility.
select_available_r2b_team <- function(env) {
  for (i in sample(1:counts[["r2b"]])) {
    ot_beds <- env_data$elms$r2b[[i]]$ot_bed
    bed_usage <- sapply(ot_beds, function(b) get_server_count(env, b))
    total_in_use <- sum(bed_usage)
    
    # cat(sprintf("Team %d OT usage: %s → total in use = %d\n", i, toString(bed_usage), total_in_use))
    
    if (total_in_use == 0) {
      # cat(sprintf("✔ R2B %d selected (OT beds are free)\n", i))
      return(i)
    }
  }
  
  # cat("✖ No R2B team available (all OT beds are in use)\n")
  return(-1)
}

#' Selects an R2E team with the highest available OT bed capacity
#'
#' @return Integer index of the selected R2E team (1-based). If no beds are available,
#'         returns a randomly selected team index.
#'
#' @details This function iterates through all R2E Heavy teams listed in `env_data$elms`,
#'          calculates available operating theatre (OT) bed slots by subtracting current
#'          usage from total capacity, and identifies the team(s) with maximum free slots.
#'
#'          If all teams are at full capacity (i.e., all OT beds are in use), the function
#'          logs a warning and randomly selects one team to continue the simulation flow.
#'
#'          Used for routing surgical casualties to appropriate forward-deployed surgical
#'          teams when Role 2 Enhanced capacity varies dynamically throughout simulation.
#'
#'          Logs selection results using `cat()` for visibility and debugging.
select_r2e_team <- function() {
  capacities <- sapply(seq_along(env_data$elms$r2eheavy), function(team_id) {
    ot_beds <- env_data$elms$r2eheavy[[team_id]][["ot_bed"]]
    sum(sapply(ot_beds, function(bed) get_capacity(env, bed) - get_server_count(env, bed)))
  })
  
  if (all(capacities <= 0)) {
    selected <- sample(seq_along(env_data$elms$r2eheavy), 1)
    # cat(sprintf("⚠ No OT capacity available, randomly selected R2E team %d\n", selected))
  } else {
    max_capacity <- max(capacities)
    candidates <- which(capacities == max_capacity)
    selected <- sample(candidates, 1)
    # cat(sprintf("✅ Selected R2E team %d with %d available OT slots\n", selected, max_capacity))
  }
  
  return(selected)
}

##############################################
## ROLE 1 TRAJECTORIES                      ##
##############################################

#' Simulates mortuary treatment pathway for KIA casualties from Role 1
#'
#' @param team index referring to R1 team array
#' @return simmer trajectory object for KIA treatment flow
#'
#' @details Selects technician from specified Role 1 team and applies a fixed-duration
#'          mortuary treatment process. Sets attributes `r1_treated` and `mortuary_treated`
#'          to record team and disposition status.
r1_treat_kia <- function(team) {
  medics <- env_data$elms$r1[[team]][grepl("_technician_", env_data$elms$r1[[team]])]
  trajectory(paste("KIA Team", team)) %>%
    set_attribute("r1_treated", team) %>%
    set_attribute("mortuary_treated", 1) %>%
    simmer::select(medics, policy = "shortest-queue") %>%
    seize_selected() %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r1$kia_treat$min,
        b = env_data$vars$r1$kia_treat$max,
        c = env_data$vars$r1$kia_treat$mode
      )
    }) %>%
    release_selected()
}

#' Simulates KIA transport from Role 1 to mortuary at Role 2
#'
#' @return simmer trajectory object for KIA vehicle transport
#'
#' @details Uses HX240M transport asset with shortest-queue selection policy.
#'          Applies log-normal delay to simulate movement time, and records start time
#'          with attribute `transport_start_time`.
r1_transport_kia <- function() {
  trajectory("Transport KIA") %>%
    simmer::select(env_data$transports$HX240M, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r1$kia_transport$min,
        b = env_data$vars$r1$kia_transport$max,
        c = env_data$vars$r1$kia_transport$mode
      )
    }) %>%
    release_selected()
}

#' Executes Role 1 treatment sequence for WIA casualties
#'
#' @param team index referring to R1 team array
#' @return simmer trajectory object for WIA treatment logic
#'
#' @details Seizes technician and clinician from specified Role 1 team.
#'          Treatment duration is based on casualty priority level (attribute `priority`)
#'          using a switch-weighted log-normal distribution. Start time logged via
#'          `treatment_start_time` attribute.
#'
#' @note Releases all seized resources at end of trajectory using `release_all()`
r1_treat_wia <- function(team) {
  medics <- env_data$elms$r1[[team]][grepl("_technician_", env_data$elms$r1[[team]])]
  clinicians <- env_data$elms$r1[[team]][grepl("_clinician_", env_data$elms$r1[[team]])]
  
  trajectory(paste("WIA Team", team)) %>%
    # set_attribute("r1_treated", team) %>%
    simmer::select(medics, policy = "shortest-queue") %>%
    seize_selected() %>%
    simmer::select(clinicians, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("treatment_start_time", function() now(env)) %>%  # Track treatment start time
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r1$wia_treat$min,
        b = env_data$vars$r1$wia_treat$max,
        c = env_data$vars$r1$wia_treat$mode
      )
    }) %>%
    release_all()
}

#' Simulates Role 1 transport of WIA or DNBI casualties to Role 2 facilities
#'
#' @return simmer trajectory object modeling casualty movement phase
#'
#' @details Selects a PMV Ambulance asset from `env_data$transports$PMVAmb` using
#'          shortest-queue policy to minimize dispatch delay. Logs transport start
#'          time using `transport_start_time` attribute. Applies log-normal distributed
#'          timeout to simulate transit duration from Role 1 to Role 2. Releases vehicle
#'          resource upon arrival to allow re-tasking.
#'
#'          This trajectory is typically invoked after initial treatment or triage,
#'          and supports both WIA (battle injuries) and DNBI (non-battle injuries).
r1_transport_wia <- function() {
  trajectory("Transport WIA") %>%
    simmer::select(env_data$transports$PMVAmb, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r1$wia_transport$min,
        b = env_data$vars$r1$wia_transport$max,
        c = env_data$vars$r1$wia_transport$mode
      )
    }) %>%
    release_selected()
}

##############################################
## ROLE 2B TRAJECTORIES                     ##
##############################################

#' Applies mortuary preparation for KIA casualties using Role 2B evacuation team
#'
#' @param traj a simmer trajectory object (typically originating at Role 1 or evac path)
#' @param team_id integer index for selected Role 2B team
#'
#' @return the modified trajectory with KIA treatment logic appended
#'
#' @details Seizes evacuation team resources and applies fixed-duration mortuary prep.
#'          Does not modify any attributes beyond resource usage.
r2b_treat_kia <- function(traj, team_id) {
  evacuation_team <- env_data$elms$r2b[[team_id]][["evac"]][[1]]
  traj %>%
    seize_resources(evacuation_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$kia_treat$min,
        b = env_data$vars$r2b$kia_treat$max,
        c = env_data$vars$r2b$kia_treat$mode
      )
    }) %>%
    # timeout(function() rnorm(1, 15)) %>%
    release_resources(evacuation_team)
}

#' Simulates Role 2B transport of KIA casualties to collocated mortuary
#'
#' @param traj a simmer trajectory object
#' @param team_id integer index for selected Role 2B team
#'
#' @return the modified trajectory with transport logic included
#'
#' @details Seizes evacuation team, logs transport duration with log-normal distribution,
#'          and sets `mortuary_treated = 1` attribute to confirm completion.
r2b_transport_kia <- function(traj, team_id) {
  evacuation_team <- env_data$elms$r2b[[team_id]][["evac"]][[1]]
  traj %>%
    seize_resources(evacuation_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$kia_transport$min,
        b = env_data$vars$r2b$kia_transport$max,
        c = env_data$vars$r2b$kia_transport$mode
      )
    }) %>%
    set_attribute("mortuary_treated", 1) %>%
    release_resources(evacuation_team)
}

#' Transports WIA casualties from Role 2B to Role 2E Heavy
#'
#' @return a new trajectory object for casualty movement
#'
#' @details Selects PMV Ambulance asset (ID = 7) using shortest-queue policy.
#'          Logs transport initiation via `r2b_r2e_transport_start` and simulates
#'          full round-trip duration (~30 minutes) using log-normal variation.
r2b_transport_wia <- function() {
  trajectory("R2B to R2E Heavy transport") %>%
    # log_("R2B to R2E Heavy Transport - start") %>%
    simmer::select(env_data$transports$PMVAmb, policy = "shortest-queue", id = 7) %>%
    seize_selected(id = 7) %>%
    set_attribute("r2b_r2e_transport_start", function() now(env)) %>%
    
    # Simulate full round-trip transport time (e.g., 30 min each way)
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$wia_transport$min,
        b = env_data$vars$r2b$wia_transport$max,
        c = env_data$vars$r2b$wia_transport$mode
      )
    }) %>%
    
    release_selected(id = 7)
}

#' Executes the full treatment pathway for WIA casualties at Role 2B
#'
#' @param team_id integer index of the Role 2B team handling treatment
#'
#' @return a simmer trajectory object representing the entire WIA care pathway
#'
#' @details This function models complex branching logic and resource allocation:
#'
#' ### Steps:
#' 1. **Hold Bed** – Initial stabilization
#' 2. **DOW Check (~1%)** – Casualties who die after arrival
#'     - Executes KIA treatment + mortuary transport
#' 3. **Resus Phase** – Seizes emergency team and resus bed
#' 4. **Surgical Decision Branch**
#'     - If surgery required:
#'         - Checks OT bed availability
#'         - Executes surgery if available (bounded by `rtruncnorm`)
#'         - Logs `r2b_surgery = 1`
#'     - If surgery not required:
#'         - Enters recovery via hold bed with random duration
#'         - Logs `return_day`
#' 5. **Evacuation Decision Branch**
#'     - If evacuation team available → immediate transfer to Role 2E Heavy
#'         - Logs `r2b_to_r2e = 1`, assigns `r2e` index via selector
#'         - Invokes appropriate `r2e_treat_wia()` branch
#'     - If evacuation team not available → fallback to ICU waiting
#'         - Tracks `evac_wait_count`, sets `r2b_to_r2e`, and reuses evacuation logic
r2b_treat_wia <- function(team_id) {
  hold_beds <- env_data$elms$r2b[[team_id]][["hold_bed"]]
  resus_beds <- env_data$elms$r2b[[team_id]][["resus_bed"]]
  ot_beds <- env_data$elms$r2b[[team_id]][["ot_bed"]]
  icu_beds <- env_data$elms$r2b[[team_id]][["icu_bed"]]
  emergency_team <- env_data$elms$r2b[[team_id]][["emerg"]][[1]]
  evacuation_team <- env_data$elms$r2b[[team_id]][["evac"]][[1]]
  surg_team <- env_data$elms$r2b[[team_id]][["surg"]][[1]]
  icu_team <- env_data$elms$r2b[[team_id]][["icu"]][[1]]
  
  # Fallback path: Wait in an ICU bed for evacuation
  wait_for_evac <- trajectory("Wait in Hold Bed for Evac") %>%
    set_global("evac_wait_count", function() {
      current <- get_global(env, "evac_wait_count")
      updated <- current + 1
      # cat("evac_wait_count:", updated, "\n")  # Now prints the incremented value
      return(updated)
    }) %>%
    # log_("wait_for_evac") %>%
    simmer::select(icu_beds, policy = "shortest-queue", id = 3) %>%
    seize_selected(id = 3) %>%
    seize_resources(icu_team) %>%
    seize_resources(evacuation_team) %>%
    release_resources(icu_team) %>%
    release_selected(id = 3) %>%
    set_attribute("r2b_to_r2e", 1) %>%
    set_attribute("r2e", function() select_r2e_team()) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$wia_transport$min,
        b = env_data$vars$r2b$wia_transport$max,
        c = env_data$vars$r2b$wia_transport$mode
      )
    }) %>%
    release_resources(evacuation_team) %>%
    branch(
      option = function() get_attribute(env, "r2e"),
      continue = TRUE,
      lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
    ) %>%
    leave(1)
  
  trajectory("R2B Basic Flow") %>%
    set_attribute("r2b_treated", team_id) %>%
    
    # Step 1: Initial hold bed
    simmer::select(hold_beds, policy = "shortest-queue", id = 1) %>%
    seize_selected(id = 1) %>%
    
    # Step 1.5: DOW branch (~1%)
    branch(
      option = function() {
        if (runif(1) < 0.01) return(1)
        return(2)
      },
      continue = TRUE,
      
      # Path 1: DOW
      trajectory("Died of Wounds") %>%
        set_attribute("dow", 1) %>%
        r2b_treat_kia(team_id) %>%
        r2b_transport_kia(team_id) %>%
        release_selected(id = 1) %>%
        simmer::leave(1),
      
      # Path 2: Continue treatment
      trajectory("Continue R2B Treatment")
    ) %>%
    
    # Step 2: Transfer to Resus
    simmer::select(resus_beds, policy = "shortest-queue", id = 2) %>%
    seize_selected(id = 2) %>%
    release_selected(id = 1) %>%
    
    # Step 3: Emergency treatment
    seize_resources(emergency_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$long_resus$min,
        b = env_data$vars$r2b$long_resus$max,
        c = env_data$vars$r2b$long_resus$mode
      )
    }) %>%
    set_attribute("r2b_resus", 1) %>%
    release_resources(emergency_team) %>%
    release_selected(id = 2) %>%
    
    # Step 4: Surgery
    branch(
      option = function() {
        needs_surg <- get_attribute(env, "surgery")
        if (!is.na(needs_surg) && needs_surg == 1) return(1)
        return(2)
      },
      continue = TRUE,
      
      # Branch 1: Surgery
      trajectory("Needs Surgery") %>%
        branch(
          option = function() {
            usage <- sum(get_server_count(env, resources = ot_beds))
            cap <- sum(get_capacity(env, resources = ot_beds))
            if (!is.na(usage) && !is.na(cap) && usage <= cap) return(1)
            return(2)
          },
          continue = TRUE,
          
          # Sub-branch 1: OT bed available → Surgery
          trajectory("Surgery Path") %>%
            simmer::select(ot_beds, policy = "shortest-queue", id = 4) %>%
            seize_selected(id = 4) %>%
            seize_resources(surg_team) %>%
            timeout(function() {
              rtriangle(
                n = 1,
                a = env_data$vars$r2b$surgery$min,
                b = env_data$vars$r2b$surgery$max,
                c = env_data$vars$r2b$surgery$mode
              )
            }) %>%
            set_attribute("r2b_surgery", 1) %>%
            release_resources(surg_team) %>%
            release_selected(id = 4),
          
          # Sub-branch 2: No OT bed available → Skip surgery (and proceed to evac)
          trajectory("No OT Available – Skip Surgery")
        ),
      
      # Branch 2: No surgery required
      trajectory("R2B No Surgery") %>%
        simmer::select(hold_beds, policy = "first-available", id = 5) %>%
        seize_selected(id = 5) %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2b$holding$min,
            b = env_data$vars$r2b$holding$max,
            c = env_data$vars$r2b$holding$mode
          )
        }) %>%
        set_attribute("return_day", function() now(env)) %>%
        release_selected(id = 5) %>%
        simmer::leave(1)
    ) %>%
    
    # Step 5: Try immediate evac, fallback if not possible
    branch(
      option = function() {
        usage <- sum(get_server_count(env, resources = evacuation_team))
        cap <- sum(get_capacity(env, resources = evacuation_team))
        if (!is.na(usage) && !is.na(cap) && usage < cap) return(1)
        return(2)
      },
      continue = TRUE,
      
      # Path 1: Immediate evacuation possible
      trajectory("Immediate Evac") %>%
        # log_("Immediate evacuation to R2E Heavy") %>%
        set_attribute("r2b_to_r2e", 1) %>%
        set_attribute("r2e", function() select_r2e_team()) %>%
        seize_resources(evacuation_team) %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2b$wia_transport$min,
            b = env_data$vars$r2b$wia_transport$max,
            c = env_data$vars$r2b$wia_transport$mode
          )
        }) %>%
        release_resources(evacuation_team) %>%
        # log_("Resources Released") %>%
        branch(
          option = function() get_attribute(env, "r2e"),
          continue = TRUE,
          lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
        ) %>%
        simmer::leave(1),
      
      # Path 2: Immediate evacuation not possible → Wait in ICU
      join(wait_for_evac)
    )
}

##############################################
## ROLE 2E HEAVY TRAJECTORIES               ##
##############################################

#' Applies treatment for KIA casualties in preparation for mortuary using R2E evacuation team
#'
#' @param traj simmer trajectory object to append treatment steps
#' @param team_id integer index of the Role 2E Heavy team
#' @param evac_team character vector of resource names assigned for evacuation duties
#'
#' @return modified trajectory with mortuary prep sequence
#'
#' @details Seizes evacuation team resources for a fixed treatment duration (normal distribution),
#'          then releases them. Used in Died of Wounds (DOW) casualties.
r2e_treat_kia <- function(traj, team_id, evac_team) {
  traj %>%
    seize_resources(evac_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2eheavy$kia_treat$min,
        b = env_data$vars$r2eheavy$kia_treat$max,
        c = env_data$vars$r2eheavy$kia_treat$mode
      )
    }) %>%
    release_resources(evac_team)
}

#' Simulates transport of KIA casualty to mortuary zone
#'
#' @param traj simmer trajectory object to append movement steps
#' @param team_id integer index of the Role 2E Heavy team
#' @param evac_team evacuation team resources associated with the selected R2E team
#'
#' @return modified trajectory with transport sequence and disposition marking
#'
#' @details Applies log-normal timeout to simulate movement duration and sets
#'          attribute `mortuary_treated = 1` to track postmortem disposition.
r2e_transport_kia <- function(traj, team_id, evac_team) {
  traj %>%
    seize_resources(evac_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2eheavy$kia_transport$min,
        b = env_data$vars$r2eheavy$kia_transport$max,
        c = env_data$vars$r2eheavy$kia_transport$mode
      )
    }) %>%
    set_attribute("mortuary_treated", 1) %>%
    release_resources(evac_team)
}

#' Models the R2E Heavy treatment flow for WIA casualties bypassing R2B or routed directly
#'
#' @param team_id Integer ID of the R2E team assigned to receive the casualty
#'
#' @return A simmer trajectory object representing clinical care and disposition at R2E
#'
#' @details This trajectory simulates doctrinal R2E workflows for wounded casualties requiring
#'          advanced treatment, resuscitation, surgical care, ICU stabilization, and eventual
#'          recovery or strategic evacuation. It preserves procedural logic from DOW check through
#'          surgical sequencing, and models transitions based on prior R2B interventions and outcomes.
#'
#' ### Phase 1: DOW Filtering (~1%)
#' - Casualty routed to `r2e_treat_kia()` and `r2e_transport_kia()` if flagged
#' - `dow = 1` set for outcome tracking
#'
#' ### Phase 2: Initial Resuscitation
#' - Casualty assigned resus bed and emergency team
#' - If previously resuscitated at R2B (`r2b_resus = 1`), receives short-form resus
#' - Otherwise, full-length emergency stabilization executed
#' - `r2e_resus = 1` set when primary interventions occur at R2E
#'
#' ### Phase 3: Surgical Branch
#' - Casualties with `surgery = 1` flag attempt to seize OT bed
#' - If seized, DAMCON surgery performed using triangular duration
#' - `r2e_surgery = 1` set post-procedure
#'
#' ### Phase 4: ICU Handling
#' - ICU treatment varies based on prior surgery history:
#'     - If R2B surgery occurred (`r2b_surgery = 1`), probabilistic branch to short ICU recovery
#'     - Else, full ICU recovery executed
#' - ICU duration modeled using doctrinal distributions (see [[14]]–[[16]])
#'
#' ### Phase 5: Final Disposition
#' - 10% of casualties routed to recovery (`return_day` logged) via hold beds
#' - Remaining 90% assigned `r2e_evac = 1` for strategic evacuation
#' - Within recovery branch:
#'     - If no prior R2B surgery, secondary surgery is performed at R2E
#'     - Else, casualty proceeds directly to recovery bed
#' - Hold bed durations modeled using triangular distribution
#'
#' ### Attributes Set
#' - `r2e_treated`, `r2e_handling`: for team and flow tracking
#' - `dow`, `r2e_resus`, `r2e_surgery`, `return_day`: event-based state flags
#' - `r2e_evac`: strategic evacuation signal
#' - Resource IDs and selections handled dynamically by shortest queue policy
#'
#' ### Logging
#' - Inline `log_()` calls trace surgical decision, ICU path, and disposition outcome
#' - Simulation audits enabled via attribute set and log hooks
r2e_treat_wia <- function(team_id) {
  hold_beds <- env_data$elms$r2eheavy[[team_id]][["hold_bed"]]
  resus_beds <- env_data$elms$r2eheavy[[team_id]][["resus_bed"]]
  ot_beds <- env_data$elms$r2eheavy[[team_id]][["ot_bed"]]
  icu_beds <- env_data$elms$r2eheavy[[team_id]][["icu_bed"]]
  emergency_teams <- env_data$elms$r2eheavy[[team_id]][["emerg"]]
  evacuation_teams <- env_data$elms$r2eheavy[[team_id]][["evac"]]
  surg_teams <- env_data$elms$r2eheavy[[team_id]][["surg"]]
  icu_teams <- env_data$elms$r2eheavy[[team_id]][["icu"]]
  
  emergency_team <- select_subteam("r2eheavy", team_id, "emerg")
  surg_team <- select_subteam("r2eheavy", team_id, "surg")
  evac_team <- select_subteam("r2eheavy", team_id, "evac")
  icu_team <- select_subteam("r2eheavy", team_id, "icu")
  
  trajectory("R2E Treatment (Fallback)") %>%
    set_attribute("r2e_treated", team_id) %>%
    set_attribute("r2e_handling", 1) %>%
    
    # Step 1: Early mortality check (Dead on Withdrawal ~1%)
    # If casualty dies immediately upon arrival, route to mortuary handling and exit simulation
    branch(
      option = function() {
        if (runif(1) < 0.01) return(1)
        return(2)
      },
      continue = TRUE,
      
      # Path 1: DOW
      trajectory("Dead on Withdrawal") %>%
        set_attribute("dow", 1) %>%
        r2e_treat_kia(team_id, evac_team) %>%
        r2e_transport_kia(team_id, evac_team) %>%
        simmer::leave(1),
      
      # Step 2: Proceed with emergency treatment at R2E Heavy
      trajectory("Continue R2E Treatment")
    ) %>%
    
    # Step 3: Seize resuscitation bed (shortest available queue)
    # Prepares for initial stabilization prior to surgery or disposition
    simmer::select(resus_beds, policy = "shortest-queue", id = 2) %>%
    seize_selected(id = 2) %>%
    
    # Step 4: Emergency resuscitation (short or full)
    # If previously treated at R2B → apply short resus model
    # Otherwise → perform full emergency treatment with logging
    seize_resources(emergency_team) %>%
    branch(
      option = function() {
        attr <- get_attribute(env, "r2b_resus")
        if (!is.na(attr) && attr == 1) return(1)
        return(2)
      },
      continue = TRUE,
      
      # Short resus time if R2B treatment occurred
      trajectory() %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$short_resus$min,
            b = env_data$vars$r2eheavy$short_resus$max,
            c = env_data$vars$r2eheavy$short_resus$mode
          )
        }) %>%
        release_resources(emergency_team) %>%
        release_selected(id = 2),
      
      # Full resus time otherwise
      trajectory() %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$long_resus$min,
            b = env_data$vars$r2eheavy$long_resus$max,
            c = env_data$vars$r2eheavy$long_resus$mode
          )
        }) %>%
        set_attribute("r2e_resus", 1) %>%
        release_resources(emergency_team) %>%
        release_selected(id = 2)
    ) %>%
    
    # Step 5: Surgery check
    # Casualties flagged for surgery are routed to OT bed if available
    branch(
      option = function() {
        needs_surg <- get_attribute(env, "surgery")
        if (!is.na(needs_surg) && needs_surg == 1) return(1)
        return(2)
      },
      continue = TRUE,
      
      # If surgery required → perform surgery
      trajectory("R2E Surgery") %>%
        simmer::select(ot_beds, policy = "shortest-queue", id = 4) %>%
        seize_selected(id = 4) %>%
        seize_resources(surg_team) %>%
        set_attribute("r2e_surgery", 1) %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$surgery$min,
            b = env_data$vars$r2eheavy$surgery$max,
            c = env_data$vars$r2eheavy$surgery$mode
          )
        }) %>%
        release_resources(surg_team) %>%
        release_selected(id = 4) %>%
        
        # Post-surgical ICU routing
        # If prior R2B surgery → branch based on DCS stage
        branch(
          option = function() {
            prior_surg <- get_attribute(env, "r2b_surgery")
            
            # DCS-III complete → short ICU recovery
            if (!is.na(prior_surg) && prior_surg == 1) {
              outcome <- sample(1:2, size = 1, prob = c(env_data$vars$r2eheavy$recovery$post_surgery, (1-env_data$vars$r2eheavy$recovery$post_surgery)))
              return(outcome)
            }
            
            # Otherwise, full recovery
            return(2)
          },
          continue = TRUE,
          
          # Second-time surgery → ICU short recovery
          trajectory("Short ICU Recovery – Stable after Second Surgery") %>%
            # log_("Stable post-op after second surgery – 1hr ICU recovery") %>%
            simmer::select(icu_beds, policy = "shortest-queue", id = 6) %>%
            seize_selected(id = 6) %>%
            timeout(function() {
              rtriangle(
                n = 1,
                a = env_data$vars$r2eheavy$short_icu$min,
                b = env_data$vars$r2eheavy$short_icu$max,
                c = env_data$vars$r2eheavy$short_icu$mode
              )
            }) %>%
            release_selected(id = 6),
          
          # First-time surgery → full ICU recovery
          trajectory("Full ICU Recovery – Unstable or First Surgery") %>%
            # log_("Standard ICU recovery required") %>%
            simmer::select(icu_beds, policy = "shortest-queue", id = 6) %>%
            seize_selected(id = 6) %>%
            timeout(function() {
              rtriangle(
                n = 1,
                a = env_data$vars$r2eheavy$long_icu$min,
                b = env_data$vars$r2eheavy$long_icu$max,
                c = env_data$vars$r2eheavy$long_icu$mode
              )
            }) %>%
            release_selected(id = 6)
        ),
      
      # No surgery required → skip to disposition logic
      trajectory("No Surgery Needed")
        # log_("No surgery needed")
    ) %>%
    
    # Step 6: Recovery or strategic evacuation branch
    # Based on doctrinal 10% recovery vs 90% evac split
    branch(
      option = function() sample(1:2, 1, prob = c(env_data$vars$r2eheavy$recovery$in_theatre_rate, (1-env_data$vars$r2eheavy$recovery$in_theatre_rate))),
      continue = TRUE,
      
      # In-theatre recovery path (10% rate)
      trajectory("Recover at R2E") %>%
        branch(
          option = function() {
            prior_surg <- get_attribute(env, "r2b_surgery")
            if (!is.na(prior_surg) && prior_surg == 1) return(2)  # Skip second surgery
            return(1)  # Perform second surgery
          },
          continue = TRUE,
          
          # If no prior surgery at R2B → perform second surgery at R2E
          trajectory("Second Surgery Before Recovery") %>%
            # log_("Initiating second surgery before R2E recovery") %>%
            simmer::select(ot_beds, policy = "shortest-queue", id = 7) %>%
            seize_selected(id = 7) %>%
            seize_resources(surg_team) %>%
            timeout(function() {
              rtriangle(
                n = 1,
                a = env_data$vars$r2eheavy$surgery$min,
                b = env_data$vars$r2eheavy$surgery$max,
                c = env_data$vars$r2eheavy$surgery$mode
              )
            }) %>%
            release_resources(surg_team) %>%
            release_selected(id = 7),
            # log_("Second surgery complete – proceeding to recovery"),
          
          # If prior surgery already occurred → proceed directly to recovery
          trajectory("No Second Surgery")
        ) %>%
        
        # Final recovery in hold bed
        # log_("Selecting hold bed for recovery") %>%
        simmer::select(hold_beds, policy = "shortest-queue", id = 5) %>%
        seize_selected(id = 5) %>%
        # log_("Recovering at R2E Heavy") %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$holding$min,
            b = env_data$vars$r2eheavy$holding$max,
            c = env_data$vars$r2eheavy$holding$mode
          )
        }) %>%
        release_selected(id = 5) %>%
        set_attribute("return_day", function() now(env)),
        # log_("Recovery complete – hold bed released"),
      
      # Strategic evacuation path (90% rate)
      trajectory("Strategic Evac") %>%
        set_attribute("r2e_evac", 1)
        # log_("Commencing strategic evacuation from R2E Heavy")
    )
}

##############################################
## CORE TRAJECTORY                          ##
##############################################

#' Models full casualty flow from point of injury through Role 1 and onward to Role 2 (B/E)
#'
#' @return A simmer trajectory object representing branching decisions based on casualty attributes
#'
#' @details This trajectory encapsulates initial triage, Role 1 stabilization, early mortality assessment,
#'          and evacuation decisions for all casualty categories: Killed in Action (KIA), Wounded in Action (WIA),
#'          and Disease/Non-Battle Injury (DNBI). Routing logic and attribute initialization are grounded in
#'          doctrinal estimates and probabilistic modeling inputs from `env_data`.
#'
#' ### Phase 1: Attribute Assignment
#' - Randomly assigns a Role 1 treatment team (`team`)
#' - Flags NBI probability (`nbi`) based on case name
#' - Assigns treatment priority (`priority`) using doctrinal distributions for WIA and DNBI cases
#' - Computes `surgery` requirement based on priority tier and casualty class
#'
#' ### Phase 2: Casualty Type Branch
#' - **Branch A – WIA/DNBI**
#'     - Casualty receives care from assigned R1 team (`r1_treat_wia()`)
#'     - Dead of Wounds (DOW) check performed (~5% P1, ~2.5% P2); DOW cases routed to KIA handling
#'     - Evacuation Decision:
#'         - If `priority == 1 or 2` and evac criteria met → attempt R2B team assignment
#'         - If R2B team unavailable → casualty bypasses to R2E and `r2b_bypassed = 1`
#'         - Else (typically P3) → recovery occurs at Role 1 (`return_day` marked post Beta-distributed timeout)
#'
#' - **Branch B – KIA**
#'     - Casualty receives simulated mortuary care via assigned R1 team (`r1_treat_kia()`) and is transported accordingly
#'
#' ### Trajectory Attributes and Logging
#' - `team`: Assigned R1 treatment team
#' - `priority`: Triage urgency code (1 = Immediate; 2 = Urgent; 3 = Delayed)
#' - `nbi`: Flag for DNBI-type cases
#' - `surgery`: Binary flag indicating surgical need (0/1)
#' - `dow`: DOW flag if casualty dies post-R1 treatment
#' - `r2b`, `r2e`, `r2b_bypassed`: Routing indicators for evacuation tracking
#' - `return_day`: Time the casualty exits simulation via recovery
#'
#' ### Notes on Flow Integrity
#' - All branching logic uses `continue = TRUE` to allow non-terminal transitions
#' - Selected R2B team is printed via `cat()` to support live simulation tracing
#' - This trajectory serves as the foundation for all casualty entry points and ensures doctrinal traceability from point of injury through recovery or disposition
casualty <- trajectory("Casualty") %>%
  log_(function() {
    paste0(get_name(env))
  }) %>%
  # Step 1: Set initial attributes
  # Set team and priority attributes
  set_attribute("team", function() sample(1:counts[["r1"]], 1)) %>% 
  set_attribute("priority", function() {
    if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) {
      sample(1:3, 1, prob = c(env_data$vars$r1$priority$one, 
                              env_data$vars$r1$priority$two, 
                              env_data$vars$r1$priority$three))
    } else {
      NA
    }
  }) %>%
  set_attribute("nbi", function() {
    name <- get_name(env)
    if (startsWith(name, "dnbi")) {
      return(as.numeric(runif(1) < env_data$vars$r1$other$nbi))
    } else {
      return(NA)
    }
  }) %>%
  # Set surgery attribute based on priority and name
  set_attribute("surgery", function() {
    prio <- get_attribute(env, "priority")
    name <- get_name(env)
    
    if (is.na(prio)) return(0)
    
    if (prio == 1) return(as.numeric(runif(1) < env_data$vars$r1$other$pri1_surgery))
    if (prio == 2) return(as.numeric(runif(1) < env_data$vars$r1$other$pri2_surgery))
    
    # For priority 3
    if (startsWith(name, "dnbi")) {
      return(as.numeric(runif(1) < env_data$vars$r1$other$pri3_dnbi_surgery))
    } else {
      return(as.numeric(runif(1) < env_data$vars$r1$other$pri3_other_surgery))
    }
  }) %>%
  
  # Branch 1: WIA/DNBI or KIA handling
  branch(
    option = function() {
      if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) 1 else 2
    },
    continue = TRUE,
    
    # Path 1: WIA/DNBI Handling
    trajectory("WIA/DNBI Branch") %>%
      # Role 1 treatment by team
      branch(
        option = function() get_attribute(env, "team"),
        continue = TRUE,
        lapply(1:counts[["r1"]], r1_treat_wia)
      ) %>%
      
      # Branch 1.1: DOW
      branch(
        option = function() {
          prio <- get_attribute(env, "priority")
          if (prio == 1 && runif(1) < env_data$vars$r1$other$pri1_dow) return(1)  # 5% P1 die of wounds
          if (prio == 2 && runif(1) < env_data$vars$r1$other$pri2_dow) return(1) # 2.5% P2 die of wounds
          return(2)  # Proceed to R2B or recovery
        },
        continue = TRUE,
        
        # Path 1: Died of wounds (treated like KIA)
        trajectory("Died of Wounds at Role 1") %>%
          set_attribute("dow", 1) %>%
          branch(
            option = function() get_attribute(env, "team"),
            continue = TRUE,
            lapply(1:counts[["r1"]], function(i) {
              r1_treat_kia(i) %>% join(r1_transport_kia())
            })
          ),
        
        # Path 2: Continue with handling
        trajectory("Post-Treatment Decision") %>%
          # Branch 1.1.1: Majority of WIA/DNBI Pri 1/2 cases
          branch(
            option = function() {
              prio <- get_attribute(env, "priority")
              if (is.na(prio)) return(2)
              if (prio == 1 && runif(1) < env_data$vars$r1$other$pri1_evac) return(1)
              if (prio == 2 && runif(1) < env_data$vars$r1$other$pri2_evac) return(1)
              return(2)
            },
            continue = TRUE,
            
            # Path 1: Transport To next echelon
            trajectory("Transport to R2b") %>%
              set_attribute("r2b", function() select_available_r2b_team(env)) %>%
              join(r1_transport_wia()) %>%
              
              # Branch 1.1.1.1: Select R2B or R2E for Evacuation
              branch(
                option = function() {
                  r2b <- get_attribute(env, "r2b")
                  # cat("Selected R2B:", r2b, "\n")  # This prints to the console
                  if (r2b > 0) return(1) else return(2)
                },
                continue = TRUE,
                
                # Path 1: Evacuate to R2B Path
                trajectory("To R2B") %>%
                  branch(
                    option = function() get_attribute(env, "r2b"),
                    continue = TRUE,
                    lapply(1:counts[["r2b"]], r2b_treat_wia)
                  ),
                
                # Path 2: Evacuate to R2E
                trajectory("Bypass R2B → To R2E") %>%
                  set_attribute("r2b_bypassed", 1) %>%
                  branch(
                    option = function() sample(1:counts[["r2eheavy"]], 1),
                    continue = TRUE,
                    lapply(1:counts[["r2eheavy"]], r2e_treat_wia)
                  )
              ),
            
            # Path 2: Recover at Role 1
            trajectory("Monitor Recovery") %>%
              timeout(function() {
                rtriangle(
                  n = 1,
                  a = env_data$vars$r1$recovery$min,
                  b = env_data$vars$r1$recovery$max,
                  c = env_data$vars$r1$recovery$mode
                )
              }) %>%
              set_attribute("return_day", function() now(env))
          )
      ),
    
    # Path 2: KIA Handling
    trajectory("KIA Branch") %>%
      branch(
        option = function() get_attribute(env, "team"),
        continue = TRUE,
        lapply(1:counts[["r1"]], function(i) {
          r1_treat_kia(i) %>% join(r1_transport_kia())
        })
      )
  )

##############################################
## BUILD ENVIRONMENT                        ##
##############################################
sink("logs/logs.txt")

# Add casualty generators to simulation
env %>%
  # add_generator("wia_cbt", casualty, distribution = at(wia_rate_cbt), mon = 2) %>%
  add_generator("wia_cbt", 
                casualty, 
                distribution = at(generate_ln_arrivals(type = "wia_cbt",
                                                       mean_daily = env_data$vars$generators$wia_cbt$mean_daily,
                                                       sd_daily = env_data$vars$generators$wia_cbt$sd_daily,
                                                       pop = env_data$pops$combat,
                                                       n_days)), mon = 2) %>%
  add_generator("kia_cbt", 
                casualty, 
                distribution = at(generate_ln_arrivals(type = "kia_cbt",
                                                       mean_daily = env_data$vars$generators$kia_cbt$mean_daily,
                                                       sd_daily = env_data$vars$generators$kia_cbt$sd_daily,
                                                       pop = env_data$pops$combat,
                                                       n_days)), mon = 2) %>%
  add_generator("dnbi_cbt", 
                casualty, 
                distribution = at(generate_ln_arrivals(type = "dnbi_cbt",
                                                       mean_daily = env_data$vars$generators$dnbi_cbt$mean_daily,
                                                       sd_daily = env_data$vars$generators$dnbi_cbt$sd_daily,
                                                       pop = env_data$pops$combat,
                                                       n_days)), mon = 2) %>%
  add_generator("wia_spt", 
                casualty, 
                distribution = at(generate_ln_arrivals(type = "wia_spt",
                                                       mean_daily = env_data$vars$generators$wia_spt$mean_daily,
                                                       sd_daily = env_data$vars$generators$wia_spt$sd_daily,
                                                       pop = env_data$pops$support,
                                                       n_days)), mon = 2) %>%
  add_generator("kia_spt", 
                casualty, 
                distribution = at(generate_ln_arrivals(type = "kia_spt",
                                                       mean_daily = env_data$vars$generators$kia_spt$mean_daily,
                                                       sd_daily = env_data$vars$generators$kia_spt$sd_daily,
                                                       pop = env_data$pops$support,
                                                       n_days)), mon = 2) %>%
  add_generator("dnbi_spt", 
                casualty, 
                distribution = at(generate_ln_arrivals(type = "dnbi_spt",
                                                       mean_daily = env_data$vars$generators$dnbi_spt$mean_daily,
                                                       sd_daily = env_data$vars$generators$dnbi_spt$sd_daily,
                                                       pop = env_data$pops$support,
                                                       n_days)), mon = 2) %>%
  add_global("evac_wait_count", 0)

env %>% run(until = n_days * day_min)

sink()

# results <- replicate(n_iterations, {
#   env %>% run(until = n_days * day_min)
#   
#   # Extract custom metrics or statistics
#   list(
#     arrivals = get_mon_arrivals(env),
#     resources = get_mon_resources(env)  # Includes usage, capacity, queue sizes over time
#   )
# }, simplify = FALSE)

##############################################
## FORMAT DATA                              ##
##############################################
# all_resources <- get_mon_resources(env)
# transport_resources <- unique(all_resources$resource[grepl("^t_", all_resources$resource)])
# resources <- all_resources[!grepl("^t_", all_resources$resource), ]  # Exclude transport resources
# 
# # Get arrival logs and annotate with casualty type
# arrivals <- get_mon_arrivals(env)
# arrivals$day <- floor(arrivals$start_time / day_min)
# arrivals$type <- ifelse(
#   grepl("^wia", arrivals$name), "WIA",
#   ifelse(grepl("^kia", arrivals$name), "KIA", "DNBI")
# )
# 
# # Extract priority values from attributes and merge with arrival data
# attributes <- get_mon_attributes(env)
# priority <- attributes[attributes$key == "priority", ]
# priority_latest <- priority[!duplicated(priority$name, fromLast = TRUE), c("name", "value")]
# colnames(priority_latest)[2] <- "priority"
# 
# arrivals <- merge(arrivals, priority_latest, by = "name", all.x = TRUE)
# arrivals$priority <- ifelse(is.na(arrivals$priority), 5, arrivals$priority)  # Assign 5 = KIA
# arrivals$priority <- factor(arrivals$priority, levels = c(1, 2, 3, 5), labels = c("P1", "P2", "P3", "KIA"))
# 
# ########################################
# ## Build Arrivals Table Data          ##
# ########################################
# # Utility function to extract and merge an attribute into the arrivals table
# merge_attribute <- function(attr_name, arrivals, attributes) {
#   subset <- attributes[attributes$key == attr_name, c("name", "value")]
#   colnames(subset) <- c("name", attr_name)
#   merge(arrivals, subset, by = "name", all.x = TRUE)
# }
# 
# # List of attributes to merge
# attribute_keys <- c("r2b_treated", "dow", "surgery", "r2b_surgery", "r2e_handling", "r2b_bypassed", "r2b_to_r2e", "r2e_surgery")
# 
# # Apply the function for each attribute
# for (attr in attribute_keys) {
#   arrivals <- merge_attribute(attr, arrivals, attributes)
# }
# 
# arrivals$team <- get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$value[match(arrivals$name, get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$name)]
# arrivals$team <- factor(arrivals$team, labels = paste("Team", 1:counts[["r1"]]))
# 
# # Categorize casualties by source: combat vs support
# arrivals$source <- ifelse(
#   grepl("_cbt", arrivals$name), "Combat",
#   ifelse(grepl("_spt", arrivals$name), "Support", "Unknown")
# )
# 
# # Get return to duty attributes
# attributes <- get_mon_attributes(env)
# returns <- attributes[attributes$key == "return_day", ]
# returns$return_day <- floor(returns$value / day_min)  # Rename before merge
# 
# # Merge return days into arrivals
# arrivals <- merge(arrivals, returns[, c("name", "return_day")], by = "name", all.x = TRUE)
# 
# ########################################
# ## Build Cumulative Losses Table Data ##
# ########################################
# # Step 1: Build daily casualty counts
# daily_counts <- as.data.frame(table(arrivals$day))
# colnames(daily_counts) <- c("day", "daily_count")
# daily_counts$day <- as.numeric(as.character(daily_counts$day))
# daily_counts$cumulative_total <- cumsum(daily_counts$daily_count)
# 
# # Step 2: Count daily returns to duty
# daily_returns <- as.data.frame(table(arrivals$return_day))
# colnames(daily_returns) <- c("day", "daily_returns")
# daily_returns$day <- as.numeric(as.character(daily_returns$day))
# 
# # Step 3: Merge returns into casualty table
# daily_counts <- merge(daily_counts, daily_returns, by = "day", all.x = TRUE)
# daily_counts$daily_returns[is.na(daily_counts$daily_returns)] <- 0  # Replace NA with 0
# 
# # Step 4: Compute cumulative returns and loss
# daily_counts$cumulative_returns <- cumsum(daily_counts$daily_returns)
# daily_counts$cumulative_loss <- daily_counts$cumulative_total - daily_counts$cumulative_returns
# 
# # Step 5: Convert to long format for plotting
# cumulative_counts <- data.frame(
#   day = daily_counts$day,
#   `Total Casualties` = daily_counts$cumulative_total,
#   `Force Loss` = daily_counts$cumulative_loss
# )
# 
# cumulative_counts_long <- melt(cumulative_counts, id.vars = "day", variable.name = "Metric", value.name = "Count")
# 
# # Compute resource usage per team/role/day
# resources <- resources[order(resources$resource, resources$time), ]
# resources$time_diff <- ave(resources$time, resources$resource, FUN = function(x) c(diff(x), 0))
# resources$busy_time <- resources$server * resources$time_diff
# resources$day <- floor(resources$time / day_min)
# resources$team <- paste("Team", sub(".*_t(\\d+)$", "\\1", resources$resource))
# resources$role <- sub("^[a-zA-Z]_(r[0-9]+[a-z]*_).*?_(\\w+_[0-9]+).*", "\\1\\2", resources$resource)
# resources$resource_type <- sub("^([cbt])_.*", "\\1", resources$resource)
# 
# resources_r1 <- resources[grepl("r1", resources$resource), ]
# 
# agg_resources <- aggregate(busy_time ~ team + role + day, data = resources_r1, sum)
# agg_resources$percent_seized <- round((agg_resources$busy_time / day_min) * 100, 2)
# 
# # Count casualties per team per day
# casualty_counts <- as.data.frame(table(arrivals$team, arrivals$day))
# colnames(casualty_counts) <- c("team", "day", "casualties")
# casualty_counts$day <- as.numeric(as.character(casualty_counts$day))
# 
# # Merge utilization and casualty data for plotting
# utilisation_data <- merge(agg_resources, casualty_counts, by = c("team", "day"), all.x = TRUE)
# total_casualties <- aggregate(casualties ~ team, data = casualty_counts, sum)
# utilisation_data <- merge(utilisation_data, total_casualties, by = "team", suffixes = c("", "_total"))
# utilisation_data$casualty_percent_of_total <- round((utilisation_data$casualties / utilisation_data$casualties_total) * 100, 2)
# 
# ########################################
# ## FILE IMPORTS                       ##
# ########################################
# 
# source("visualisations.R")
# source("r2b_visualisation.R")
# source("r2e_visualisation.R")
# source("sankey_flow.R")
