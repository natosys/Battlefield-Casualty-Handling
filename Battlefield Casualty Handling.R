### ENVIRONMENT SETUP ###

rm(list = ls())
gc()
set.seed(42)

library(simmer)
library(simmer.bricks)
library(simmer.plot)
library(ggplot2)
library(scales)
library(reshape2)
library(truncnorm)
library(jsonlite)

source("data_import.R")

# Create the simulation environment
env <- simmer("Battlefield Casualty Handling")

env_data <- load_elms("env_data.json")

## Constants ##
# Counts of different element types (r1, r2b, r2eheavy, etc)
counts <- sapply(env_data$elms, length)
# Duration of a day in simulation time units (minutes)
day_min <- 1440
total_population <- env_data$pops$combat + env_data$pops$support
# Calculate Combat Ineffective (CIE) threshold
cie_threshold <- (2/3) * total_population
n_days <- 30

# Casualty arrival rates
wia_rate_cbt <- function() rexp(1, rate = (env_data$pops$combat / 1000 * 6.86) / day_min)
wia_rate_spt <- function() rexp(1, rate = (env_data$pops$support / 1000 * 6.86) / day_min)
kia_rate_cbt <- function() rexp(1, rate = (env_data$pops$combat / 1000 * 1.63) / day_min)
kia_rate_spt <- function() rexp(1, rate = (env_data$pops$support / 1000 * 1.63) / day_min)

generate_dnbi_arrivals <- function(mean_daily, sd_daily, pop, n_days, cap = 5, seed = NULL) {
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
  
  return(arrival_times)
}

dnbi_rate_spt <- generate_dnbi_arrivals(mean_daily = 0.94,
                                            sd_daily = 0.56,
                                            pop = env_data$pops$support,
                                            n_days)

dnbi_rate_cbt <- generate_dnbi_arrivals(mean_daily = 2.04,
                                            sd_daily = 1.89,
                                            pop = env_data$pops$combat,
                                            n_days)

# Separate by team index
r1_teams_technicians <- lapply(env_data$elms$r1, function(team) {
  team[grepl("_technician_", team)]
})
r1_teams_clinicians <- lapply(env_data$elms$r1, function(team) {
  team[grepl("_clinician_", team)]
})

### SCHEDULING FOR ROLE 2B SURGERY AND OT BEDS ###
ot_shift_1 <- simmer::schedule(c(0, 720), c(1, 0), period = 1440)
ot_shift_2 <- simmer::schedule(c(720, 1440), c(1, 0), period = 1440)

surg_counter <- 0  # Used to stagger shifts

### RESOURCE ALLOCATION ###
for (elm_type in names(env_data$elms)) {
  for (team in env_data$elms[[elm_type]]) {
    
    if (is.character(team)) {
      # R1 flat teams—no scheduling
      for (res_name in team) {
        env <- env %>% add_resource(res_name)
      }
      
    } else if (is.list(team)) {
      # Apply scheduling only for R2B teams
      apply_schedule <- elm_type == "r2b"
      
      if (apply_schedule) {
        # Assign team shift once per team
        team_shift <- if (surg_counter %% 2 == 1) ot_shift_1 else ot_shift_2
        shift_label <- ifelse(surg_counter %% 2 == 1, "Shift 1", "Shift 2")
        
        for (section_name in names(team)) {
          section <- team[[section_name]]
          
          for (res_name in section) {
            if (section_name %in% c("surg", "ot_bed")) {
              env <- env %>% add_resource(res_name, team_shift)
            } else {
              env <- env %>% add_resource(res_name)
            }
          }
        }
        
        surg_counter <- surg_counter + 1
        
      } else {
        # For R2E or other teams: no scheduling applied
        for (section in team) {
          for (res_name in section) {
            env <- env %>% add_resource(res_name)
          }
        }
      }
    }
  }
}

# Add all transport resources
for (transport_type in names(env_data$transports)) {
  for (res_name in env_data$transports[[transport_type]]) {
    env <- env %>% add_resource(res_name)
  }
}

### ROLE 1 HANDLING ###

# Treatment logic for KIA casualties
r1_treat_kia <- function(team) {
  medics <- r1_teams_technicians[[team]]
  trajectory(paste("KIA Team", team)) %>%
    select(medics, policy = "shortest-queue") %>%
    seize_selected() %>%
    timeout(function() rnorm(1, 15)) %>%
    release_selected()
}

# Transport KIA to mortuary (collocated with Role 2 facility)
r1_transport_kia <- function() {
  trajectory("Transport KIA") %>%
    select(env_data$transports$HX240M, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    timeout(function() rlnorm(1, log(30), 0.5)) %>%
    release_selected()
}

# Treatment logic for WIA casualties
r1_treat_wia <- function(team) {
  medics <- r1_teams_technicians[[team]]
  clinicians <- r1_teams_clinicians[[team]]
  
  trajectory(paste("WIA Team", team)) %>%
    select(medics, policy = "shortest-queue") %>%
    seize_selected() %>%
    select(clinicians, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("treatment_start_time", function() now(env)) %>%  # Track treatment start time
    timeout(function() {  # Set treatment duration based on priority
      prio <- get_attribute(env, "priority")
      switch(
        as.character(prio),
        "1" = rlnorm(1, log(120), 0.125),
        "2" = rlnorm(1, log(60), 0.25),
        "3" = rlnorm(1, log(20), 0.5),
        45
      )
    }) %>%
    release_all()
}

# Transport WIA/DNBI to Role 2 using one of the PMV_Amb resources
r1_transport_wia <- function() {
  trajectory("Transport WIA") %>%
    select(env_data$transports$PMVAmb, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    timeout(function() rlnorm(1, log(30), 0.5)) %>%
    release_selected()
}

r2b_transport_wia <- function() {
  trajectory("R2B to R2E Heavy transport") %>%
    log_("R2B to R2E Heavy Transport - start") %>%
    select(env_data$transports$PMVAmb, policy = "shortest-queue", id = 7) %>%
    seize_selected(id = 7) %>%
    set_attribute("r2b_r2e_transport_start", function() now(env)) %>%
    
    # Simulate full round-trip transport time (e.g., 30 min each way)
    timeout(function() {
      rlnorm(1, log(30), 0.3)
    }) %>%
    
    release_selected(id = 7)
}

seize_resources <- function(trj, resources) {
  for (res in resources) {
    trj <- trj %>% seize(res, 1)
  }
  trj
}

release_resources <- function(trj, resources) {
  for (res in resources) {
    trj <- trj %>% release(res, 1)
  }
  trj
}

select_subteam <- function(elm_type, team_id, subteam_type) {
  subteams <- env_data$elms[[elm_type]][[team_id]][[subteam_type]]
  
  if (is.null(subteams) || length(subteams) == 0) {
    stop(sprintf("No subteams of type '%s' found for %s team %d", subteam_type, elm_type, team_id))
  }
  
  index <- sample(seq_along(subteams), 1)
  return(subteams[[index]])
}

### ROLE 2 ENHANCED HANDLING ###
select_r2e_team <- function() {
  selected <- sample(1:length(env_data$elms$r2eheavy), 1)
  cat(sprintf("⚠ Randomly selected R2E team %d (no capacity check)\n", selected))
  return(selected)
}

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
  
  trajectory("R2E Treatment (Fallback)") %>%
    log_("r2e_treat_wia") %>%
    set_attribute("r2e_handling", 1) %>%
    
    # Step 1: Initial hold bed
    log_("Initially occupy R2E holding bed") %>%
    select(hold_beds, policy = "shortest-queue", id = 1) %>%
    seize_selected(id = 1) %>%
    
    # Step 2: Transfer to Resus
    log_("Occupy R2E Resus bed") %>%
    select(resus_beds, policy = "shortest-queue", id = 2) %>%
    seize_selected(id = 2) %>%
    release_selected(id = 1) %>%

    # Step 3: Emergency treatment
    seize_resources(emergency_team) %>%
    log_("Emergency treatment in R2E Resus") %>%
    branch(
      option = function() {
        attr <- get_attribute(env, "r2b_resus")
        if (!is.na(attr) && attr == 1) return(1)
        return(2)
      },
      continue = TRUE,
      
      # Shorter resus time if previously resus at R2B
      trajectory() %>%
        timeout(function() rlnorm(1, log(15), 0.3)) %>%
        release_resources(emergency_team) %>%
        release_selected(id = 2),
      
      # Longer resus time otherwise
      trajectory() %>%
        timeout(function() rlnorm(1, log(45), 0.3)) %>%
        set_attribute("r2e_resus", 1) %>%
        release_resources(emergency_team) %>%
        release_selected(id = 2)
    )

  # Step 4: Surgery
  # if surgery == 1 && r2b_surgery != 1 then do surgery
}

### ROLE 2 BASIC HANDLING ###
select_available_r2b_team <- function(env) {
  for (i in sample(1:counts[["r2b"]])) {
    ot_beds <- env_data$elms$r2b[[i]]$ot_bed
    bed_usage <- sapply(ot_beds, function(b) get_server_count(env, b))
    total_in_use <- sum(bed_usage)
    
    cat(sprintf("Team %d OT usage: %s → total in use = %d\n", i, toString(bed_usage), total_in_use))
    
    if (total_in_use == 0) {
      cat(sprintf("✔ R2B %d selected (OT beds are free)\n", i))
      return(i)
    }
  }
  
  cat("✖ No R2B team available (all OT beds are in use)\n")
  return(-1)
}

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
    log_("wait_for_evac") %>%
    select(icu_beds, policy = "shortest-queue", id = 3) %>%
    seize_selected(id = 3) %>%
    seize_resources(icu_team) %>%
    seize_resources(evacuation_team) %>%
    release_resources(icu_team) %>%
    release_selected(id = 3) %>%
    timeout(function() rlnorm(1, log(30), 0.2)) %>%
    # Patient arrives at next location here and other process initiates
    timeout(function() rlnorm(1, log(30), 0.2)) %>%
    release_resources(evacuation_team)
  
  trajectory("R2B Basic Flow") %>%
    set_attribute("r2b_treated", team_id) %>%
    
    # Step 1: Initial hold bed
    select(hold_beds, policy = "shortest-queue", id = 1) %>%
    seize_selected(id = 1) %>%
    
    # Step 2: Transfer to Resus
    select(resus_beds, policy = "shortest-queue", id = 2) %>%
    seize_selected(id = 2) %>%
    release_selected(id = 1) %>%
    
    # Step 3: Emergency treatment
    seize_resources(emergency_team) %>%
    timeout(function() rlnorm(1, log(45), 0.3)) %>%
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
      
      # Branch 1: Needs surgery
      trajectory("Needs Surgery") %>%
        branch(
          option = function() {
            usage <- sum(get_server_count(env, resources = ot_beds))
            cap <- sum(get_capacity(env, resources = ot_beds))
            if (!is.na(usage) && !is.na(cap) && usage < cap) return(1)
            return(2)
          },
          continue = TRUE,
          
          # Sub-branch 1: OT bed available → Surgery
          trajectory("Surgery Path") %>%
            select(ot_beds, policy = "shortest-queue", id = 4) %>%
            seize_selected(id = 4) %>%
            seize_resources(surg_team) %>%
            timeout(function() rtruncnorm(1, a = 60, b = 180, mean = 135, sd = 20)) %>%
            set_attribute("r2b_surgery", 1) %>%
            release_resources(surg_team) %>%
            release_selected(id = 4),
          
          # Sub-branch 2: No OT bed available → Skip surgery (and proceed to evac)
          trajectory("No OT Available – Skip Surgery")
        ),
      
      # Branch 2: No surgery required
      trajectory("R2B No Surgery") %>%
        select(hold_beds, policy = "first-available", id = 5) %>%
        seize_selected(id = 5) %>%
        timeout(function() {
          recovery_days <- rbeta(1, shape1 = 2, shape2 = 3) * 3
          recovery_days * day_min
        }) %>%
        set_attribute("return_day", function() now(env)) %>%
        release_selected(id = 5)
    ) %>%
    
    # Step 4: Try immediate evac, fallback if not possible
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
        set_attribute("r2b_to_r2e", 1) %>%
        set_attribute("r2e", function() select_r2e_team()) %>%
        seize_resources(evacuation_team) %>%
        timeout(function() rlnorm(1, log(30), 0.2)) %>%
        log_("Immediate evacuation to R2E Heavy") %>%
        release_resources(evacuation_team) %>%
        log_("Resources Released") %>%
        branch(
          option = function() get_attribute(env, "r2e"),
          continue = TRUE,
          lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
        ),
      
      # Path 2: Immediate evacuation not possible → Wait in ICU
      join(wait_for_evac)
    )
}


### CORE TRAJECTORY ###
casualty <- trajectory("Casualty") %>%
  set_attribute("team", function() sample(1:counts[["r1"]], 1)) %>% 
  set_attribute("priority", function() {
    if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) {
      sample(1:3, 1, prob = c(0.65, 0.2, 0.15))
    } else {
      NA
    }
  }) %>%
  set_attribute("nbi", function() {
    name <- get_name(env)
    if (startsWith(name, "dnbi")) {
      return(as.numeric(runif(1) < 0.17))
    } else {
      return(NA)
    }
  }) %>%
  set_attribute("surgery", function() {
    prio <- get_attribute(env, "priority")
    name <- get_name(env)
    
    if (is.na(prio)) return(0)
    
    if (prio == 1) return(as.numeric(runif(1) < 0.9))
    if (prio == 2) return(as.numeric(runif(1) < 0.8))
    
    # For priority 3
    if (startsWith(name, "dnbi")) {
      return(as.numeric(runif(1) < 0.4))
    } else {
      return(as.numeric(runif(1) < 0.6))
    }
  }) %>%
  
  
  branch(
    option = function() {
      if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) 1 else 2
    },
    continue = TRUE,
    
    # ----- WIA/DNBI Branch -----
    trajectory("WIA/DNBI Branch") %>%
      
      # Role 1 treatment by team
      branch(
        option = function() get_attribute(env, "team"),
        continue = TRUE,
        lapply(1:counts[["r1"]], r1_treat_wia)
      ) %>%
      
      # Branch to DoW or continue
      branch(
        option = function() {
          prio <- get_attribute(env, "priority")
          if (prio == 1 && runif(1) < 0.05) return(1)  # 5% P1 die of wounds
          if (prio == 2 && runif(1) < 0.025) return(1) # 2.5% P2 die of wounds
          return(2)  # Proceed to R2B or recovery
        },
        continue = TRUE,
        
        # Path: Died of wounds (treated like KIA)
        trajectory("Died of Wounds at Role 1") %>%
          set_attribute("dow", 1) %>%
          branch(
            option = function() get_attribute(env, "team"),
            continue = TRUE,
            lapply(1:counts[["r1"]], function(i) {
              r1_treat_kia(i) %>% join(r1_transport_kia())
            })
          ),
        
        # Path: Continue with evac/recovery
        trajectory("Post-Treatment Decision") %>%
          branch(
            option = function() {
              prio <- get_attribute(env, "priority")
              if (is.na(prio)) return(2)
              if (prio == 1 && runif(1) < 0.95) return(1)
              if (prio == 2 && runif(1) < 0.90) return(1)
              return(2)
            },
            continue = TRUE,
            
            # Path: To R2B
            trajectory("Transport to R2b") %>%
              set_attribute("r2b", function() select_available_r2b_team(env)) %>%
              join(r1_transport_wia()) %>%
              
              branch(
                option = function() {
                  r2b <- get_attribute(env, "r2b")
                  cat("Selected R2B:", r2b, "\n")  # This prints to the console
                  if (r2b > 0) return(1) else return(2)
                },
                continue = TRUE,
                
                # R2B Path
                trajectory("To R2B") %>%
                  branch(
                    option = function() get_attribute(env, "r2b"),
                    continue = TRUE,
                    lapply(1:counts[["r2b"]], r2b_treat_wia)
                  ),
                
                # Fallback to R2E
                trajectory("Bypass R2B → To R2E") %>%
                  set_attribute("r2b_bypassed", 1) %>%
                  branch(
                    option = function() sample(1:counts[["r2eheavy"]], 1),
                    continue = TRUE,
                    lapply(1:counts[["r2eheavy"]], r2e_treat_wia)
                  )
              ),
            
            # Path: Recover at Role 1
            trajectory("Monitor Recovery") %>%
              timeout(function() {
                recovery_days <- rbeta(1, shape1 = 2, shape2 = 3) * 5
                recovery_days * day_min
              }) %>%
              set_attribute("return_day", function() now(env))
          )
      ),
    
    # ----- KIA Branch -----
    trajectory("KIA Branch") %>%
      branch(
        option = function() get_attribute(env, "team"),
        continue = TRUE,
        lapply(1:counts[["r1"]], function(i) {
          r1_treat_kia(i) %>% join(r1_transport_kia())
        })
      )
  )

# Add casualty generators to simulation
env %>%
  add_generator("wia_cbt", casualty, distribution = wia_rate_cbt, mon = 2) %>%
  add_generator("kia_cbt", casualty, distribution = kia_rate_cbt, mon = 2) %>%
  add_generator("dnbi_cbt", casualty, distribution = at(dnbi_rate_cbt), mon = 2) %>%
  add_generator("wia_spt", casualty, distribution = wia_rate_spt, mon = 2) %>%
  add_generator("kia_spt", casualty, distribution = kia_rate_spt, mon = 2) %>%
  add_generator("dnbi_spt", casualty, distribution = at(dnbi_rate_spt), mon = 2)

# Run the simulation for 30 days
env %>% run(until = n_days * day_min)

#### DATA FORMATTING ####

all_resources <- get_mon_resources(env)
transport_resources <- unique(all_resources$resource[grepl("^t_", all_resources$resource)])
resources <- all_resources[!grepl("^t_", all_resources$resource), ]  # Exclude transport resources

# Get arrival logs and annotate with casualty type
arrivals <- get_mon_arrivals(env)
arrivals$day <- floor(arrivals$start_time / day_min)
arrivals$type <- ifelse(
  grepl("^wia", arrivals$name), "WIA",
  ifelse(grepl("^kia", arrivals$name), "KIA", "DNBI")
)

# Extract priority values from attributes and merge with arrival data
attributes <- get_mon_attributes(env)
priority <- attributes[attributes$key == "priority", ]
priority_latest <- priority[!duplicated(priority$name, fromLast = TRUE), c("name", "value")]
colnames(priority_latest)[2] <- "priority"

arrivals <- merge(arrivals, priority_latest, by = "name", all.x = TRUE)
arrivals$priority <- ifelse(is.na(arrivals$priority), 5, arrivals$priority)  # Assign 5 = KIA
arrivals$priority <- factor(arrivals$priority, levels = c(1, 2, 3, 5), labels = c("P1", "P2", "P3", "KIA"))

########################################
## Build Arrivals Table Data          ##
########################################
# Utility function to extract and merge an attribute into the arrivals table
merge_attribute <- function(attr_name, arrivals, attributes) {
  subset <- attributes[attributes$key == attr_name, c("name", "value")]
  colnames(subset) <- c("name", attr_name)
  merge(arrivals, subset, by = "name", all.x = TRUE)
}

# List of attributes to merge
attribute_keys <- c("r2b_treated", "dow", "surgery", "r2b_surgery", "r2e_handling", "r2b_bypassed", "r2b_to_r2e")

# Apply the function for each attribute
for (attr in attribute_keys) {
  arrivals <- merge_attribute(attr, arrivals, attributes)
}

arrivals$team <- get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$value[match(arrivals$name, get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$name)]
arrivals$team <- factor(arrivals$team, labels = paste("Team", 1:counts[["r1"]]))

# Categorize casualties by source: combat vs support
arrivals$source <- ifelse(
  grepl("_cbt", arrivals$name), "Combat",
  ifelse(grepl("_spt", arrivals$name), "Support", "Unknown")
)

# Get return to duty attributes
attributes <- get_mon_attributes(env)
returns <- attributes[attributes$key == "return_day", ]
returns$return_day <- floor(returns$value / day_min)  # Rename before merge

# Merge return days into arrivals
arrivals <- merge(arrivals, returns[, c("name", "return_day")], by = "name", all.x = TRUE)

########################################
## Build Cumulative Losses Table Data ##
########################################
# Step 1: Build daily casualty counts
daily_counts <- as.data.frame(table(arrivals$day))
colnames(daily_counts) <- c("day", "daily_count")
daily_counts$day <- as.numeric(as.character(daily_counts$day))
daily_counts$cumulative_total <- cumsum(daily_counts$daily_count)

# Step 2: Count daily returns to duty
daily_returns <- as.data.frame(table(arrivals$return_day))
colnames(daily_returns) <- c("day", "daily_returns")
daily_returns$day <- as.numeric(as.character(daily_returns$day))

# Step 3: Merge returns into casualty table
daily_counts <- merge(daily_counts, daily_returns, by = "day", all.x = TRUE)
daily_counts$daily_returns[is.na(daily_counts$daily_returns)] <- 0  # Replace NA with 0

# Step 4: Compute cumulative returns and loss
daily_counts$cumulative_returns <- cumsum(daily_counts$daily_returns)
daily_counts$cumulative_loss <- daily_counts$cumulative_total - daily_counts$cumulative_returns

# Step 5: Convert to long format for plotting
cumulative_counts <- data.frame(
  day = daily_counts$day,
  `Total Casualties` = daily_counts$cumulative_total,
  `Force Loss` = daily_counts$cumulative_loss
)

cumulative_counts_long <- melt(cumulative_counts, id.vars = "day", variable.name = "Metric", value.name = "Count")

# Compute resource usage per team/role/day
resources <- resources[order(resources$resource, resources$time), ]
resources$time_diff <- ave(resources$time, resources$resource, FUN = function(x) c(diff(x), 0))
resources$busy_time <- resources$server * resources$time_diff
resources$day <- floor(resources$time / day_min)
resources$team <- paste("Team", sub(".*_t(\\d+)$", "\\1", resources$resource))
resources$role <- sub("^[a-zA-Z]_(r[0-9]+[a-z]*_).*?_(\\w+_[0-9]+).*", "\\1\\2", resources$resource)
resources$resource_type <- sub("^([cbt])_.*", "\\1", resources$resource)

resources_r1 <- resources[grepl("r1", resources$resource), ]

agg_resources <- aggregate(busy_time ~ team + role + day, data = resources_r1, sum)
agg_resources$percent_seized <- round((agg_resources$busy_time / day_min) * 100, 2)

# Count casualties per team per day
casualty_counts <- as.data.frame(table(arrivals$team, arrivals$day))
colnames(casualty_counts) <- c("team", "day", "casualties")
casualty_counts$day <- as.numeric(as.character(casualty_counts$day))

# Merge utilization and casualty data for plotting
utilisation_data <- merge(agg_resources, casualty_counts, by = c("team", "day"), all.x = TRUE)
total_casualties <- aggregate(casualties ~ team, data = casualty_counts, sum)
utilisation_data <- merge(utilisation_data, total_casualties, by = "team", suffixes = c("", "_total"))
utilisation_data$casualty_percent_of_total <- round((utilisation_data$casualties / utilisation_data$casualties_total) * 100, 2)

source("visualisations.R")
source("r2b_visualisation.R")
