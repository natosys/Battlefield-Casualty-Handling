library(simmer)
library(simmer.bricks)
library(simmer.plot)
library(ggplot2)
library(scales)

# Create the simulation environment
env <- simmer("Battlefield Casualty Handling")

# Constants
# Duration of a day in simulation time units (minutes)
day_min <- 1440
# Total population affected by the simulation
population_cbt <- 2250
population_spt <- 1000
# Number of treatment teams available
team_count <- 6
# Number of PMV_Amb available (count is a multiple of treatment teams available)
PMV_Amb_count <- team_count * 2
# Number of 40M available (count is a multiple of treatment teams available)
HX2_40M_count <- team_count

# Function to generate names of treatment team resources based on team ID
team_resources <- function(team_id) {
  c(paste0("c_medic_1_t", team_id),
    paste0("c_medic_2_t", team_id),
    paste0("c_medic_3_t", team_id),
    paste0("c_nurse_t", team_id),
    paste0("c_doctor_t", team_id))
}

# Establish a list of all team resources
all_team_resources <- list()

# Add identical treatment teams with one of each resource per team
for (team in 1:team_count) {
  for (res in team_resources(team)) {
    env %>% add_resource(res, 1)
  }
  # Add team resource lists to the all_team_resources list
  all_team_resources <- append(all_team_resources, list(team_resources(team)))
}

# List of team medics used for medic specific tasking
team_medics <- function(team_id) {
  c(paste0("c_medic_1_t", team_id), paste0("c_medic_2_t", team_id), paste0("c_medic_3_t", team_id))
}

# List of team clinicians used for clinician specific tasking
team_clinicians <- function(team_id) {
  c(paste0("c_nurse_t", team_id), paste0("c_doctor_t", team_id))
}

# Add HX2_40M resources to environment
for (i in 1:HX2_40M_count) {
  env %>%
    add_resource(paste0("t_HX2_40M", i), 50)
}

# Add PMV_Amb resources to environment
for (i in 1:PMV_Amb_count) {
  env %>%
    add_resource(paste0("t_PMV_Amb", i), 4)
}

# Treatment logic for KIA casualties
treat_kia <- function(team) {
  medics <- team_medics(team)
  trajectory(paste("KIA Team", team)) %>%
    select(medics, policy = "shortest-queue") %>%
    seize_selected() %>%
    timeout(function() rnorm(1, 15)) %>%
    release_selected()
}

# Transport KIA to mortuary (collocated with Role 2 facility)
transport_kia <- function() {
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
treat_wia <- function(team) {
  medics <- team_medics(team)
  clinicians <- team_clinicians(team)
  
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
transport_wia <- function() {
  # Dynamically identify all PMV_Amb resources in the environment
  ambulances <- get_resources(env)[grepl("^t_PMV_Amb", get_resources(env))]
  
  trajectory("Transport WIA") %>%
    select(ambulances, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    timeout(function() rlnorm(1, log(30), 0.5)) %>%
    release_selected()
}

# Main casualty trajectory
casualty <- trajectory("Casualty") %>%
  # Randomly assign to team
  set_attribute("team", function() sample(1:team_count, 1)) %>% 
  # Assign casualty priority based on historical statistical outcomes
  set_attribute("priority", function() {  # Assign priority only if WIA or DNBI
    if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) {
      sample(1:3, 1, prob = c(0.65, 0.2, 0.15))
    } else {
      NA
    }
  }) %>%
  # Branch based on casualty type (wia and dnbi branch 1; kia branch 2)
  branch(
    option = function() {
      if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) 1 else 2
    },
    continue = TRUE,
    trajectory("WIA/DNBI Branch") %>%
      # if wia/dnbi call the treat_wia sub-trajectory
      branch(
        option = function() get_attribute(env, "team"),
        continue = TRUE,
        lapply(1:team_count, treat_wia)
      ) %>%
      # if the casualty is priority 1 or 2 call the transport_wia sub-trajectory
      branch(
        option = function() {
          prio <- get_attribute(env, "priority")
          if (!is.na(prio) && prio %in% c(1, 2)) 1 else 2
        },
        continue = TRUE,
        transport_wia(),
        trajectory("Monitor WIA recovery")
      ),
    # elseif kia follow the treat_kia, then transport_kia sub-trajectories
    trajectory("KIA Branch") %>%
      branch(
        option = function() get_attribute(env, "team"),
        continue = TRUE,
        lapply(1:team_count, function(i) {
          treat_kia(i) %>% join(transport_kia())
        })
      )
  )

# Casualty arrival rates
wia_rate_cbt <- function() rexp(1, rate = (population_cbt / 1000 * 6.86) / day_min)
kia_rate_cbt <- function() rexp(1, rate = (population_cbt / 1000 * 1.63) / day_min)
dnbi_mean_cbt <- 2.04 - log(population_cbt / (1000 * 1440))
dnbi_stddev_cbt <- 1.89
dnbi_rate_cbt <- function() rlnorm(1, dnbi_mean_cbt, dnbi_stddev_cbt)
dnbi_mean_spt <- 0.94 - log(population_spt / (1000 * 1440))
dnbi_stddev_spt <- 0.56
dnbi_rate_spt <- function() rlnorm(1, dnbi_mean_spt, dnbi_stddev_spt)

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

#### VISUALISATIONS ####

plot(casualty)

# Get arrival logs and annotate with casualty type
arrivals <- get_mon_arrivals(env)
arrivals$day <- floor(arrivals$start_time / day_min)
arrivals$type <- ifelse(
  grepl("^wia", arrivals$name), "WIA",
  ifelse(grepl("^kia", arrivals$name), "KIA", "DNBI")
)

# Plot daily WIA, KIA and DNBI totals
ggplot(arrivals, aes(x = day, fill = type)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  scale_x_continuous(breaks = seq(0, max(arrivals$day), by = 2)) +
  labs(title = "Daily Casualties by Type", x = "Day", y = "Count", fill = "Type") +
  scale_fill_manual(values = c("WIA" = "steelblue", "KIA" = "darkred", "DNBI" = "seagreen")) +
  theme_minimal()

# Plot resource usage
resources <- get_mon_resources(env)

# Generate and display usage plots for clinical resources by team
for (team in 1:team_count) {
  clinical_resources_team <- all_team_resources[[team]]
  
  # Filter resources to include only those relevant to this team
  team_resources_filtered <- resources[resources$resource %in% clinical_resources_team, ]
  
  team_plot <- plot(team_resources_filtered, metric = "usage") +
    ggtitle(paste("Resource Usage Over Time - Team", team)) +
    theme_minimal()
  
  print(team_plot)
}

transport_resources <- unique(resources$resource[grepl("^t_", resources$resource)])

plot(resources, metric = "usage", transport_resources) +
  ggtitle("Transport Resource Usage Over Time") +
  theme_minimal()

resources <- resources[!grepl("^t_", resources$resource), ]  # Exclude transport resources

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

# Compute resource usage per team/role/day
resources <- resources[order(resources$resource, resources$time), ]
resources$time_diff <- ave(resources$time, resources$resource, FUN = function(x) c(diff(x), 0))
resources$busy_time <- resources$server * resources$time_diff
resources$day <- floor(resources$time / day_min)
# resources$team <- ifelse(grepl("_t1$", resources$resource), "Team 1", "Team 2")
resources$team <- paste("Team", sub(".*_t(\\d+)$", "\\1", resources$resource))
# resources$role <- sub("_t[12]$", "", resources$resource)
resources$role <- sub("_t\\d+$", "", resources$resource)

agg_resources <- aggregate(busy_time ~ team + role + day, data = resources, sum)
agg_resources$percent_seized <- round((agg_resources$busy_time / day_min) * 100, 2)

# Count casualties per team per day
arrivals$team <- get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$value[match(arrivals$name, get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$name)]
# arrivals$team <- factor(arrivals$team, labels = c("Team 1", "Team 2"))
arrivals$team <- factor(arrivals$team, labels = paste("Team", 1:team_count))
casualty_counts <- as.data.frame(table(arrivals$team, arrivals$day))
colnames(casualty_counts) <- c("team", "day", "casualties")
casualty_counts$day <- as.numeric(as.character(casualty_counts$day))

# Merge utilization and casualty data for plotting
plot_data <- merge(agg_resources, casualty_counts, by = c("team", "day"), all.x = TRUE)
total_casualties <- aggregate(casualties ~ team, data = casualty_counts, sum)
plot_data <- merge(plot_data, total_casualties, by = "team", suffixes = c("", "_total"))
plot_data$casualty_percent_of_total <- round((plot_data$casualties / plot_data$casualties_total) * 100, 2)

# Prepare for Daily Resource Utilisation by Team with Casualty Load Plot
# Base roles and colors
roles <- c("c_medic_1", "c_medic_2", "c_medic_3", "c_nurse", "c_doctor")
base_colors <- c("#1b9e77", "#d95f02", "#d9ff02", "#7570b3", "#e7298a")

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
    "c_medic_1" = "#1b9e77",
    "c_medic_2" = "#d95f02",
    "c_medic_3" = "#d9ff02",
    "c_nurse" = "#7570b3",
    "c_doctor" = "#e7298a"
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