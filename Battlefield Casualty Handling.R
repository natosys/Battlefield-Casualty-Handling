### ENVIRONMENT SETUP ###

library(simmer)
library(simmer.bricks)
library(simmer.plot)
library(ggplot2)
library(scales)
library(reshape2)
library(truncnorm)
library(jsonlite)

# Create the simulation environment
env <- simmer("Battlefield Casualty Handling")

## Constants ##
# Duration of a day in simulation time units (minutes)
day_min <- 1440
# Total population affected by the simulation
population_cbt <- 2500
population_spt <- 1250
total_population <- population_cbt + population_spt
# Calculate Combat Ineffective (CIE) threshold
cie_threshold <- (2/3) * total_population

# Casualty arrival rates
wia_rate_cbt <- function() rexp(1, rate = (population_cbt / 1000 * 6.86) / day_min)
kia_rate_cbt <- function() rexp(1, rate = (population_cbt / 1000 * 1.63) / day_min)
dnbi_mean_cbt <- 2.04 - log(population_cbt / (1000 * 1440))
dnbi_stddev_cbt <- 1.89
dnbi_rate_cbt <- function() rlnorm(1, dnbi_mean_cbt, dnbi_stddev_cbt)
dnbi_mean_spt <- 0.94 - log(population_spt / (1000 * 1440))
dnbi_stddev_spt <- 0.56
dnbi_rate_spt <- function() rlnorm(1, dnbi_mean_spt, dnbi_stddev_spt)

# Import Resource and Element Lists
# elements.json and resources.json is where the elements (role facility) and 
# resources for those elements are defined for the simulation.
element_list <- fromJSON("elements.json", simplifyDataFrame = TRUE)
resource_list <- fromJSON("resources.json", simplifyDataFrame = FALSE)
resource_map <- fromJSON("resources.json", simplifyDataFrame = TRUE)

# Number of R1 treatment teams available
r1_count <- element_list$qty[element_list$elm == "r1"]
# Number of PMV_Amb available (count is a multiple of treatment teams available)
PMV_Amb_count <- r1_count * 2
# Number of 40M available (count is a multiple of treatment teams available)
HX2_40M_count <- r1_count
# Number of R2B teams available
r2b_count <- element_list$qty[element_list$elm == "r2b"]

expanded_resources <- do.call(rbind, lapply(seq_len(nrow(element_list)), function(i) {
  elm <- element_list$elm[i]
  team_qty <- element_list$qty[i]
  # Subset matching resources
  matching_resources <- resource_map[resource_map$elm == elm, ]
  # For each team
  do.call(rbind, lapply(seq_len(team_qty), function(team_number) {
    # For each resource, replicate according to its own qty
    do.call(rbind, lapply(seq_len(nrow(matching_resources)), function(r) {
      this_resource <- matching_resources[r, ]
      res_qty <- this_resource$qty
      # replicate row res_qty times, add suffix and team number
      replicated <- this_resource[rep(1, res_qty), ]
      # Create resource_name with suffix and team
      # Collect components (excluding NA)
      components <- c(
        this_resource$type,
        this_resource$elm,
        this_resource$sub_elm,
        this_resource$name
      )
      # Remove NA components
      components <- components[!is.na(components)]
      # Now construct resource_name
      replicated$resource_name <- paste0(
        paste(components, collapse = "_"), "_",
        seq_len(res_qty), "_t", team_number
      )
      replicated$team <- team_number
      replicated
    }))
  }))
}))
rownames(expanded_resources) <- NULL
resource_map$resource_name <- apply(resource_map, 1, function(row) {
  paste(row[!is.na(row)], collapse = "_")
})

# Filter for r1 only
r1_resources <- expanded_resources[expanded_resources$elm == "r1", ]
# Split into a nested list by team
r1_teams <- split(r1_resources$resource_name, r1_resources$team)
# Filter for r1 clinicians only
r1_clinician_resources <- expanded_resources[expanded_resources$elm == "r1" & expanded_resources$sub_elm == "clinician", ]
# Split into a nested list by team
r1_teams_clinicians <- split(r1_clinician_resources$resource_name, r1_clinician_resources$team)
# Filter for r1 technicians only
r1_technician_resources <- expanded_resources[expanded_resources$elm == "r1" & expanded_resources$sub_elm == "technician", ]
# Split into a nested list by team
r1_teams_technicians <- split(r1_technician_resources$resource_name, r1_technician_resources$team)

# Filter for r2b only
r2b_resources <- expanded_resources[expanded_resources$elm == "r2b", ]
# Split by team
by_team <- split(r2b_resources, r2b_resources$team)
# Build nested list per team
r2b_teams <- lapply(by_team, function(team_df) {
  # Initial grouping by sub_elm
  nested <- split(team_df$resource_name, team_df$sub_elm)
  # Filter bed-type rows
  bed_rows <- team_df[team_df$type == "b", ]
  # Group bed resources by name (e.g. icu, hold) and store under "name_bed"
  if (nrow(bed_rows) > 0) {
    bed_groups <- split(bed_rows$resource_name, bed_rows$name)
    for (bed_name in names(bed_groups)) {
      key <- paste0(bed_name, "_bed")
      nested[[key]] <- bed_groups[[bed_name]]
    }
  }
  return(nested)
})

## Transport Resources ##
# Add HX2_40M resources to environment
for (i in 1:HX2_40M_count) {
  env %>% add_resource(paste0("t_HX2_40M", i), 50)
}
# Add PMV_Amb resources to environment
for (i in 1:PMV_Amb_count) {
  env %>% add_resource(paste0("t_PMV_Amb", i), 4)
}

## R2B Resources ##
for (i in 1:r1_count) {
  for (item in resource_list) {
    type <- item$type
    elm <- item$elm
    sub_elm <- item$sub_elm
    name <- item$name
    qty <- as.integer(item$qty)
    for (j in seq_len(item$qty)) {
      if (type =="c" && elm == "r1") {
        resource_name <- paste(type, elm, sub_elm, name, j, paste0("t", i), sep = "_")
        env %>% add_resource(resource_name, capacity = 1)
      }
    }
  }
}

# Add R2B teams to the environment
for (i in 1:r2b_count) {
  for (item in resource_list) {
    type <- item$type
    elm <- item$elm
    sub_elm <- item$sub_elm
    name <- item$name
    qty <- as.integer(item$qty)
    for (j in seq_len(item$qty)) {
      if (type == "b") {
        resource_name <- paste(type, elm, name, j, paste0("t", i), sep = "_")
      }
      else if (type =="c" && elm == "r2b") {
        resource_name <- paste(type, elm, sub_elm, name, j, paste0("t", i), sep = "_")
      }
      env %>% add_resource(resource_name, capacity = 1)
      
    }
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
  # Dynamically identify all HX2_40M resources in the environment
  ambulances <- get_resources(env)[grepl("^t_HX2_40M", get_resources(env))]
  
  trajectory("Transport KIA") %>%
    select(ambulances, policy = "shortest-queue") %>%
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
  # Dynamically identify all PMV_Amb resources in the environment
  ambulances <- get_resources(env)[grepl("^t_PMV_Amb", get_resources(env))]
  
  trajectory("Transport WIA") %>%
    select(ambulances, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    timeout(function() rlnorm(1, log(30), 0.5)) %>%
    release_selected()
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

### ROLE 2 ENHANCED HANDLING ###
r2e_treat_wia <- function() {
  trajectory("R2E Treatment (Fallback)") %>%
    log_("r2e_treat_wia") %>%
    set_attribute("r2e_handling", 1) %>%
    timeout(function() {
      # Use <<- to modify global patient_count
      patient_count <<- patient_count + 1
      rlnorm(1, log(90), 0.3)  # Treatment duration
    })
}

### ROLE 2 BASIC HANDLING ###
# select_available_r2b_team <- function(env) {
#   # for (i in 1:r2b_count) {
#   for (i in sample(1:r2b_count)) {  # <-- randomize the order
#     beds <- r2b_teams[[i]][["hold_bed"]]
#     
#     total_capacity <- 0
#     total_in_use <- 0
#     
#     for (bed in beds) {
#       cap <- get_capacity(env, bed)
#       use <- get_server_count(env, bed)
#       
#       if (!is.null(cap) && !is.null(use)) {
#         total_capacity <- total_capacity + cap
#         total_in_use <- total_in_use + use
#       }
#     }
#     
#     if (total_in_use < (total_capacity)) {
#       return(i)  # Found an R2B team with available holding bed
#     }
#   }
#   
#   cat("No available R2B team found.\n")
#   return(-1)  # No available R2B teams
# }
# select_available_r2b_team <- function(env) {
#   for (i in sample(1:r2b_count)) {  # Randomize order
#     ot_beds <- r2b_teams[[i]][["ot_bed"]]
#     
#     queued <- 0
#     for (bed in ot_beds) {
#       qlen <- get_queue_count(env, bed)
#       if (!is.null(qlen)) {
#         queued <- queued + qlen
#       }
#     }
#     
#     cat(sprintf("R2B %d: queued for OT = %d\n", i, queued))
#     
#     if (queued <= 1) {
#       return(i)  # One queued → R2B is available
#     }
#   }
#   
#   cat("No available R2B team found (all have OT queues).\n")
#   return(-1)
# }
select_available_r2b_team <- function(env) {
  for (i in sample(1:r2b_count)) {
    usage <- sum(sapply(r2b_teams[[i]][["ot_bed"]], function(b) get_server_count(env, b)))
    if (usage == 0) {
      cat(sprintf("✔ R2B %d selected (OT not in use)\n", i))
      return(i)
    }
  }
  cat("✖ No R2B available (OT beds in use)\n")
  return(-1)
}

patient_count <- 0

r2b_treat_wia <- function(team_id) {
  hold_beds <- r2b_teams[[team_id]][["hold_bed"]]
  resus_beds <- r2b_teams[[team_id]][["resus_bed"]]
  ot_beds <- r2b_teams[[team_id]][["ot_bed"]]
  emergency_team <- r2b_teams[[team_id]][["emerg"]]
  evacuation_team <- r2b_teams[[team_id]][["evac"]]
  surg_team <- r2b_teams[[team_id]][["surg"]]
  
  # Fallback path: Wait in a hold bed for evacuation
  wait_for_evac <- trajectory("Wait in Hold Bed for Evac") %>%
    log_("wait_for_evac") %>%
    select(hold_beds, policy = "shortest-queue", id = 3) %>%
    seize_selected(id = 3) %>%
    seize_resources(evacuation_team) %>%
    release_selected(id = 3) %>%
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
    release_resources(emergency_team) %>%
    release_selected(id = 2) %>%
    
    ### SURGERY ###
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
          
          # Sub-branch 2: No OT bed → Skip surgery
          trajectory("No OT Available – Skip Surgery")
        ),
      
      # Branch 2: No surgery required
      trajectory("No Surgery Required")
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
      
      trajectory("Immediate Evac") %>%
        seize_resources(evacuation_team) %>%
        timeout(function() rlnorm(1, log(30), 0.2)) %>%
        release_resources(evacuation_team),
      
      join(wait_for_evac)
    )
}


### CORE TRAJECTORY ###
casualty <- trajectory("Casualty") %>%
  set_attribute("team", function() sample(1:r1_count, 1)) %>% 
  set_attribute("priority", function() {
    if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) {
      sample(1:3, 1, prob = c(0.65, 0.2, 0.15))
    } else {
      NA
    }
  }) %>%  set_attribute("surgery", function() {
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
        lapply(1:r1_count, r1_treat_wia)
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
            lapply(1:r1_count, function(i) {
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
              # set_attribute("r2b", function() sample(1:r2b_count, 1))  %>% # force random R2B assignment
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
                    lapply(1:r2b_count, r2b_treat_wia)
                  ),
                
                # Fallback to R2E
                # join(r2e_treat_wia())
                trajectory("Bypass R2B → To R2E") %>%
                  set_attribute("r2b_bypassed", 1) %>%
                  join(r2e_treat_wia())
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
        lapply(1:r1_count, function(i) {
          r1_treat_kia(i) %>% join(r1_transport_kia())
        })
      )
  )

# Add casualty generators to simulation
env %>%
  add_generator("wia_cbt", casualty, distribution = wia_rate_cbt, mon = 2) %>%
  add_generator("kia_cbt", casualty, distribution = kia_rate_cbt, mon = 2) %>%
  add_generator("dnbi_cbt", casualty, distribution = dnbi_rate_cbt, mon = 2) %>%
  add_generator("wia_spt", casualty, distribution = wia_rate_cbt, mon = 2) %>%
  add_generator("kia_spt", casualty, distribution = kia_rate_cbt, mon = 2) %>%
  add_generator("dnbi_spt", casualty, distribution = dnbi_rate_spt, mon = 2)

# Run the simulation for 30 days
env %>% run(until = 30 * day_min)

#### DATA FORMATTING ####

# Get arrival logs and annotate with casualty type
arrivals <- get_mon_arrivals(env)
arrivals$day <- floor(arrivals$start_time / day_min)
arrivals$type <- ifelse(
  grepl("^wia", arrivals$name), "WIA",
  ifelse(grepl("^kia", arrivals$name), "KIA", "DNBI")
)

# resources <- get_mon_resources(env)
all_resources <- get_mon_resources(env)
transport_resources <- unique(all_resources$resource[grepl("^t_", all_resources$resource)])
resources <- all_resources[!grepl("^t_", all_resources$resource), ]  # Exclude transport resources

#### VISUALISATIONS ####

##############################################
## Daily Casualties by Type (WIA/KIA/DNBI)  ##
##############################################
# Plot daily WIA, KIA and DNBI totals
ggplot(arrivals, aes(x = day, fill = type)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  scale_x_continuous(breaks = seq(0, max(arrivals$day), by = 2)) +
  labs(title = "Daily Casualties by Type", x = "Day", y = "Count", fill = "Type") +
  scale_fill_manual(values = c("WIA" = "steelblue", "KIA" = "darkred", "DNBI" = "seagreen")) +
  theme_minimal()

##############################################
## Resource Usage Plots                     ##
##############################################
# Generate and display usage plots for clinical resources by team
for (team in 1:r1_count) {
  clinical_resources_team <- r1_teams[[team]]
  
  # Filter resources to include only those relevant to this team
  team_resources_filtered <- all_resources[all_resources$resource %in% clinical_resources_team, ]
  
  team_plot <- plot(team_resources_filtered, metric = "usage") +
    ggtitle(paste("Resource Usage Over Time - Team", team)) +
    theme_minimal()
  
  print(team_plot)
}

# Generate and display usage plots for r2b clinical resources by team
for (team in 1:r2b_count) {
  clinical_resources_team <- r2b_teams[[team]][["emerg"]]
  
  # Filter resources to include only those relevant to this team
  team_resources_filtered <- all_resources[all_resources$resource %in% clinical_resources_team, ]
  
  team_plot <- plot(team_resources_filtered, metric = "usage") +
    ggtitle(paste("R2B Resource Usage Over Time - Team", team)) +
    theme_minimal()
  
  print(team_plot)
}

plot(all_resources, metric = "usage", transport_resources) +
  ggtitle("Transport Resource Usage Over Time") +
  theme_minimal()

##############################################
## Daily Casualties by Priority             ##
##############################################
# Extract priority values from attributes and merge with arrival data
attributes <- get_mon_attributes(env)
priority <- attributes[attributes$key == "priority", ]
priority_latest <- priority[!duplicated(priority$name, fromLast = TRUE), c("name", "value")]
colnames(priority_latest)[2] <- "priority"

arrivals <- merge(arrivals, priority_latest, by = "name", all.x = TRUE)
arrivals$priority <- ifelse(is.na(arrivals$priority), 5, arrivals$priority)  # Assign 5 = KIA
arrivals$priority <- factor(arrivals$priority, levels = c(1, 2, 3, 5), labels = c("P1", "P2", "P3", "KIA"))

# Plot daily casualties by priority
ggplot(arrivals, aes(x = day, fill = priority)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  scale_fill_manual(values = c("P1" = "#d73027", "P2" = "#fc8d59", "P3" = "#91bfdb", "KIA" = "black")) +
  labs(title = "Daily Casualties by Priority", x = "Day", y = "Count", fill = "Priority") +
  theme_minimal()

##############################################
## Cumulative Casualty and Force Loss Graph ##
##############################################
# Step 1: Build daily casualty counts
daily_counts <- as.data.frame(table(arrivals$day))
colnames(daily_counts) <- c("day", "daily_count")
daily_counts$day <- as.numeric(as.character(daily_counts$day))
daily_counts$cumulative_total <- cumsum(daily_counts$daily_count)

# Step 2: Get return to duty attributes
attributes <- get_mon_attributes(env)
returns <- attributes[attributes$key == "return_day", ]
returns$return_day <- floor(returns$value / day_min)  # Rename before merge

# Step 3: Merge return days into arrivals
arrivals <- merge(arrivals, returns[, c("name", "return_day")], by = "name", all.x = TRUE)

# Step 4: Count daily returns to duty
daily_returns <- as.data.frame(table(arrivals$return_day))
colnames(daily_returns) <- c("day", "daily_returns")
daily_returns$day <- as.numeric(as.character(daily_returns$day))

# Step 5: Merge returns into casualty table
daily_counts <- merge(daily_counts, daily_returns, by = "day", all.x = TRUE)
daily_counts$daily_returns[is.na(daily_counts$daily_returns)] <- 0  # Replace NA with 0

# Step 6: Compute cumulative returns and loss
daily_counts$cumulative_returns <- cumsum(daily_counts$daily_returns)
daily_counts$cumulative_loss <- daily_counts$cumulative_total - daily_counts$cumulative_returns

# Step 7: Convert to long format for plotting
plot_data <- data.frame(
  day = daily_counts$day,
  `Total Casualties` = daily_counts$cumulative_total,
  `Force Loss` = daily_counts$cumulative_loss
)

plot_data_long <- melt(plot_data, id.vars = "day", variable.name = "Metric", value.name = "Count")

# Step 8: Plot
ggplot(plot_data_long, aes(x = day, y = Count, color = Metric)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = cie_threshold, ymax = total_population,
           fill = "red", alpha = 0.2) +
  geom_hline(yintercept = cie_threshold, linetype = "dashed", color = "red", size = 1) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, total_population)) +
  labs(
    title = "Cumulative Casualties vs Force Loss Over Time",
    x = "Day",
    y = "Personnel",
    color = "Metric"
  ) +
  theme_minimal()

##############################################
## Composite Utilisation and Casualty Load  ##
##############################################
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
arrivals$team <- get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$value[match(arrivals$name, get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$name)]
arrivals$team <- factor(arrivals$team, labels = paste("Team", 1:r1_count))
casualty_counts <- as.data.frame(table(arrivals$team, arrivals$day))
colnames(casualty_counts) <- c("team", "day", "casualties")
casualty_counts$day <- as.numeric(as.character(casualty_counts$day))

# Merge utilization and casualty data for plotting
plot_data <- merge(agg_resources, casualty_counts, by = c("team", "day"), all.x = TRUE)
total_casualties <- aggregate(casualties ~ team, data = casualty_counts, sum)
plot_data <- merge(plot_data, total_casualties, by = "team", suffixes = c("", "_total"))
plot_data$casualty_percent_of_total <- round((plot_data$casualties / plot_data$casualties_total) * 100, 2)

# Final composite plot: utilization + casualty load
ggplot(plot_data, aes(x = day)) +
  facet_wrap(~ team) +
  
  # Bar plot for casualty load behind lines
  geom_bar(
    aes(y = casualty_percent_of_total),
    stat = "identity",
    fill = "gray50",
    alpha = 0.3,
    width = 0.6
  ) +
  
  # Red/amber shaded risk zones
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 66, ymax = 100, fill = "red", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 66, fill = "orange", alpha = 0.3) +
  
  # Resource utilization trends
  geom_line(aes(y = percent_seized, color = role), linewidth = 1) +
  geom_point(aes(y = percent_seized, color = role), size = 2) +
  
  # Dashed reference lines
  geom_hline(yintercept = 66, linetype = "dashed", color = "red", linewidth = 1) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "orange", linewidth = 1) +
  
  # Y-axis for utilization and casualties
  scale_y_continuous(
    name = "Resource Utilization (%)",
    limits = c(0, 100),
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(
      transform = ~ .,
      name = "Daily Casualties (% of Team Total)",
      labels = scales::percent_format(scale = 1)
    ),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # Plot title and legend formatting
  labs(
    title = "Daily Resource Utilization by Team with Casualty Load",
    x = "Day",
    color = "Resource Role"
  ) +
  scale_color_manual(values = c(
    "r1_medic_1" = "#1b9e77",
    "r1_medic_2" = "#d95f02",
    "r1_medic_3" = "#d9ff02",
    "r1_nurse_1" = "#7570b3",
    "r1_doctor_1" = "#e7298a"
  )) +
  
  # Theme customization
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray80", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black")
  )

##############################################
## Casualty Breakdown by Source and Type    ##
##############################################
# Categorize casualties by source: combat vs support
arrivals$source <- ifelse(
  grepl("_cbt", arrivals$name), "Combat",
  ifelse(grepl("_spt", arrivals$name), "Support", "Unknown")
)

# Summarize total casualties by source and type
casualty_breakdown <- as.data.frame(table(arrivals$source, arrivals$type))
colnames(casualty_breakdown) <- c("Source", "Type", "Count")

# Plot: Casualty breakdown by source and type
ggplot(casualty_breakdown, aes(x = Source, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("WIA" = "steelblue", "KIA" = "darkred", "DNBI" = "seagreen")) +
  labs(
    title = "Casualty Breakdown by Source",
    x = "Casualty Source",
    y = "Total Count",
    fill = "Casualty Type"
  ) +
  theme_minimal()

########################################
## R2B Composite Utilisation per Day  ##
########################################

# Filter for R2B resources only
resources_r2b <- resources[grepl("r2b", resources$resource), ]

# Exclude resource from graphing (while they are managed as a group)
excluded_resources <- c(
  "c_r2b_emerg_medic_1_t1",
  "c_r2b_emerg_nurse_1_t1",
  "c_r2b_emerg_nurse_2_t1",
  "c_r2b_emerg_nurse_3_t1",
  "c_r2b_evac_medic_2_t1",
  "c_r2b_surg_medic_1_t1",
  "c_r2b_surg_surgeon_1_t1",
  "c_r2b_surg_surgeon_2_t1",
  
  "c_r2b_emerg_medic_1_t2",
  "c_r2b_emerg_nurse_1_t2",
  "c_r2b_emerg_nurse_2_t2",
  "c_r2b_emerg_nurse_3_t2",
  "c_r2b_evac_medic_2_t2",
  "c_r2b_surg_medic_1_t2",
  "c_r2b_surg_surgeon_1_t2",
  "c_r2b_surg_surgeon_2_t2"
)
resources_r2b <- resources_r2b[!resources_r2b$resource %in% excluded_resources, ]


# Aggregate daily busy time per role and team
agg_resources_r2b <- aggregate(busy_time ~ team + resource + day, data = resources_r2b, sum)
agg_resources_r2b$percent_seized <- round((agg_resources_r2b$busy_time / day_min) * 100, 2)

# Filter only the 'r2b_treated' attributes
r2b_flags <- attributes[attributes$key == "r2b_treated", c("name", "value")]

colnames(r2b_flags)[colnames(r2b_flags) == "value"] <- "r2b_treated"

arrivals <- merge(arrivals, r2b_flags, by = "name", all.x = TRUE)

casualty_counts_r2b <- arrivals[!is.na(arrivals$r2b_treated), ]

# Create the table
casualty_counts_r2b <- as.data.frame(table(
  r2b_treated = arrivals$r2b_treated[!is.na(arrivals$r2b_treated)],
  day = arrivals$day[!is.na(arrivals$r2b_treated)]
))

# Prepend "Team" to the r2b_treated values
casualty_counts_r2b$r2b_treated <- paste("Team", as.character(casualty_counts_r2b$r2b_treated))

# Convert day to numeric
casualty_counts_r2b$day <- as.numeric(as.character(casualty_counts_r2b$day))

# Rename column to match agg_resources_r2b
colnames(casualty_counts_r2b)[colnames(casualty_counts_r2b) == "r2b_treated"] <- "team"
colnames(casualty_counts_r2b)[colnames(casualty_counts_r2b) == "Freq"] <- "casualties"

# Merge utilization and casualty data for plotting
plot_data_r2b <- merge(agg_resources_r2b, casualty_counts_r2b, by = c("team", "day"), all.x = TRUE)
total_casualties_r2b <- aggregate(casualties ~ team, data = casualty_counts_r2b, sum)
plot_data_r2b <- merge(plot_data_r2b, total_casualties_r2b, by = "team", suffixes = c("", "_total"))
plot_data_r2b$casualty_percent_of_total <- round((plot_data_r2b$casualties / plot_data_r2b$casualties_total) * 100, 2)
plot_data_r2b$role <- sub("_t.*$", "", plot_data_r2b$resource)


# R2B Utilization + Casualty Load Plot
ggplot(plot_data_r2b, aes(x = day)) +
  facet_wrap(~ team) +

  # Bar plot for casualty load
  geom_bar(
    aes(y = casualty_percent_of_total),
    stat = "identity",
    fill = "gray50",
    alpha = 0.3,
    width = 0.6
  ) +

  # Risk zones
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 66, ymax = 100, fill = "red", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 66, fill = "orange", alpha = 0.3) +

  # Utilization trends
  geom_line(aes(y = percent_seized, color = role), linewidth = 1) +
  geom_point(aes(y = percent_seized, color = role), size = 2) +

  # Dashed reference lines
  geom_hline(yintercept = 66, linetype = "dashed", color = "red", linewidth = 1) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "orange", linewidth = 1) +

  # Axis settings
  scale_y_continuous(
    name = "Resource Utilization (%)",
    # limits = c(0, 100),
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(
      transform = ~ .,
      name = "Daily Casualties (% of Team Total)",
      labels = scales::percent_format(scale = 1)
    ),
    expand = expansion(mult = c(0, 0.05))
  ) +

  # Titles and colors
  labs(
    title = "R2B Daily Resource Utilization by Team with Casualty Load",
    x = "Day",
    color = "Resource Role"
  ) +
  scale_color_manual(
    values = c(
      "b_r2b_ot_1" = "#ffff99",
      "b_r2b_resus_1" = "#b3ffff",
      "b_r2b_resus_2" = "#fcae91",
      "b_r2b_icu_1" = "#a6cee3",
      "b_r2b_icu_2" = "#fb9a99",
      "b_r2b_hold_1" = "#b2df8a",
      "b_r2b_hold_2" = "#fdbf6f",
      "b_r2b_hold_3" = "#cab2d6",
      "b_r2b_hold_4" =  "#1f78b4",
      "b_r2b_hold_5" =  "#33a02c",
      "c_r2b_emerg_facem_1" = "#6a3d9a",
      "c_r2b_evac_medic_1" = "#fdb462",
      "c_r2b_surg_anesthetist_1" = "#fccde5"
    ),
    labels = c(
      "b_r2b_ot_1" = "Operating Theatre 1",
      "b_r2b_resus_1" = "Resus Bed 1",
      "b_r2b_resus_2" = "Resus Bed 2",
      "b_r2b_icu_1" = "ICU Bed 1",
      "b_r2b_icu_2" = "ICU Bed 2",
      "b_r2b_hold_1" = "Holding Bed 1",
      "b_r2b_hold_2" = "Holding Bed 2",
      "b_r2b_hold_3" = "Holding Bed 3",
      "b_r2b_hold_4" = "Holding Bed 4",
      "b_r2b_hold_5" = "Holding Bed 5",
      "c_r2b_emerg_facem_1" = "Emergency Team",
      "c_r2b_evac_medic_1" = "Evacuation Team",
      "c_r2b_surg_anesthetist_1" = "Surgical Team"
    )
  ) +

  # Theme
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray80", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black")
  )

# Extract only the 'dow' attribute
dow <- attributes[attributes$key == "dow", ]

# Ensure dow only contains necessary columns
dow_subset <- dow[, c("name", "value")]
colnames(dow_subset) <- c("name", "dow")

# Merge with arrivals on 'name'
arrivals <- merge(arrivals, dow_subset, by = "name", all.x = TRUE)

# Extract surgery attributes
surgery <- attributes[attributes$key == "surgery", ]

# Ensure surgery only contains necessary columns
surgery_extract <- surgery[, c("name", "value")]
colnames(surgery_extract) <- c("name", "surgery")

# Merge into arrivals table
arrivals <- merge(arrivals, surgery_extract, by = "name", all.x = TRUE)

# Extract surgery at r2b attributes
r2b_surgery <- attributes[attributes$key == "r2b_surgery", ]

# Ensure surgery at r2b only contains necessary columns
r2b_surgery_extract <- r2b_surgery[, c("name", "value")]
colnames(r2b_surgery_extract) <- c("name", "r2b_surgery")

# Merge into arrivals table
arrivals <- merge(arrivals, r2b_surgery_extract, by = "name", all.x = TRUE)

###
# Extract only the 'r2e_handling' attribute
r2e_handling <- attributes[attributes$key == "r2e_handling", ]

# Ensure r2e_handling only contains necessary columns
r2e_handling_subset <- r2e_handling[, c("name", "value")]
colnames(r2e_handling_subset) <- c("name", "r2e_handling")

# Merge with arrivals on 'name'
arrivals <- merge(arrivals, r2e_handling_subset, by = "name", all.x = TRUE)
###
# Extract only the 'r2b_bypassed' attribute
r2b_bypassed <- attributes[attributes$key == "r2b_bypassed", ]

# Ensure dow only contains necessary columns
r2b_bypassed_subset <- r2b_bypassed[, c("name", "value")]
colnames(r2b_bypassed_subset) <- c("name", "r2b_bypassed")

# Merge with arrivals on 'name'
arrivals <- merge(arrivals, r2b_bypassed_subset, by = "name", all.x = TRUE)
###

# Filter only rows where surgery occurred
surgery_rows <- arrivals[!is.na(arrivals$r2b_surgery) & arrivals$r2b_surgery == 1, ]

# Convert simulation time (in minutes) to days
surgery_rows$day <- floor(surgery_rows$start_time / 1440)

# Tabulate counts of surgeries by day and team
surgery_counts <- table(surgery_rows$day, surgery_rows$r2b_treated)
surgery_df <- as.data.frame(surgery_counts)
colnames(surgery_df) <- c("day", "r2b_team", "surgeries")

# Ensure proper types
surgery_df$day <- as.numeric(as.character(surgery_df$day))
surgery_df$r2b_team <- as.factor(surgery_df$r2b_team)

# Create the stacked bar plot
ggplot(surgery_df, aes(x = day, y = surgeries, fill = r2b_team)) +
  geom_bar(stat = "identity") +
  labs(
    title = "R2B Surgeries Per Day",
    x = "Day",
    y = "Number of Surgeries",
    fill = "R2B Team"
  ) +
  theme_minimal()

# Convert simulation time to days (assuming start_time is in minutes)
arrivals$r2e_day <- floor(arrivals$start_time / (60 * 24))

# Filter: only those handled by R2E
r2b_bypassed <- arrivals[!is.na(arrivals$r2b_bypassed) & arrivals$r2b_bypassed == 1, ]

# Aggregate counts per day
r2b_bypassed_counts <- as.data.frame(table(r2b_bypassed$r2e_day))
colnames(r2b_bypassed_counts) <- c("Day", "Count")
r2b_bypassed_counts$Day <- as.numeric(as.character(r2b_bypassed_counts$Day))

# Plot with ggplot2
ggplot(r2b_bypassed_counts, aes(x = Day, y = Count)) +
  geom_col(fill = "firebrick", color = "black") +
  scale_y_continuous(breaks = function(x) seq(0, ceiling(max(x)), by = 1)) +
  labs(
    title = "Casualties Sent Directly to R2E per Day",
    x = "Day",
    y = "Number of Casualties"
  ) +
  theme_minimal(base_size = 14)