library(simmer)
library(simmer.bricks)
library(ggplot2)
library(scales)

# Constants
# Duration of a day in simulation time units (minutes)
day_min <- 1440
# Total population affected by the simulation
population <- 1000
# Number of treatment teams available
team_count <- 2
# Number of PMV_Amb available (count is a multiple of treatment teams available)
PMV_Amb_count <- team_count * 2
# Number of 40M available (count is a multiple of treatment teams available)
HX2_40M_count <- team_count


# Create the simulation environment
env <- simmer("Battlefield Casualty Handling")

# Function to generate names of treatment team resources based on team ID
team_resources <- function(team_id) {
  c(paste0("medic_1_t", team_id),
    paste0("medic_2_t", team_id),
    paste0("medic_3_t", team_id),
    paste0("nurse_t", team_id),
    paste0("doctor_t", team_id))
}

# Add two identical treatment teams with one of each resource per team
for (team in 1:2) {
  env %>%
    add_resource(paste0("medic_1_t", team), 1) %>%
    add_resource(paste0("medic_2_t", team), 1) %>%
    add_resource(paste0("medic_3_t", team), 1) %>%
    add_resource(paste0("nurse_t", team), 1) %>%
    add_resource(paste0("doctor_t", team), 1)
}

for (i in 1:HX2_40M_count) {
  env %>%
    add_resource(paste0("HX2_40M", i), 50)
}

for (i in 1:PMV_Amb_count) {
  env %>%
    add_resource(paste0("PMV_Amb", i), 4)
}

# Resources used for treating KIA patients
kia_resources <- function(team_id) {
  c(paste0("medic_1_t", team_id), paste0("medic_2_t", team_id), paste0("medic_3_t", team_id))
}

# Treatment logic for KIA casualties
treat_kia <- function(team) {
  team_res <- kia_resources(team)
  trajectory(paste("KIA Team", team)) %>%
    select(team_res, policy = "shortest-queue") %>%
    seize_selected() %>%
    timeout(function() rnorm(1, 15)) %>%
    release_selected()
}

# Transport KIA to mortuary (collocated with Role 2 facility)
transport_kia <- function() {
  # Dynamically identify all HX2_40M resources in the environment
  ambulance_resources <- names(env$get_resources())
  ambulances <- ambulance_resources[grepl("^HX2_40M", ambulance_resources)]
  
  trajectory("Transport KIA") %>%
    select(ambulances, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    timeout(function() rlnorm(1, log(30), 0.5)) %>%
    release_selected()
}

# Treatment logic for WIA casualties
treat_wia <- function(team) {
  medics <- c(paste0("medic_1_t", team), paste0("medic_2_t", team), paste0("medic_2_t", team))
  clinicians <- c(paste0("nurse_t", team), paste0("doctor_t", team))
  
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
        "3" = rlnorm(1, log(30), 0.5),
        45
      )
    }) %>%
    release_all()
}

# Transport WIA/DNBI to Role 2 using one of the PMV_Amb resources
transport_wia <- function() {
  # Dynamically identify all PMV_Amb resources in the environment
  ambulance_resources <- names(env$get_resources())
  ambulances <- ambulance_resources[grepl("^PMV_Amb", ambulance_resources)]
  
  trajectory("Transport WIA") %>%
    select(ambulances, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    timeout(function() rlnorm(1, log(30), 0.5)) %>%
    release_selected()
}

# Main casualty arrival trajectory
casualty <- trajectory("Casualty") %>%
  set_attribute("team", function() sample(1:team_count, 1)) %>%  # Randomly assign to team
  set_attribute("priority", function() {  # Assign priority only if WIA
    if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) {
      sample(1:3, 1, prob = c(0.65, 0.2, 0.15))
    } else {
      NA
    }
  }) %>%
  branch(
    option = function() {
      if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) 1 else 2
    },
    continue = TRUE,
    trajectory("WIA/DNBI Branch") %>%
      branch(
        option = function() get_attribute(env, "team"),
        continue = TRUE,
        treat_wia(1),
        treat_wia(2)
      ),
    trajectory("KIA Branch") %>%
      branch(
        option = function() get_attribute(env, "team"),
        continue = TRUE,
        treat_kia(1),
        treat_kia(2)
      )
  )

# Casualty arrival rates
wia_rate <- function() rexp(1, rate = (population / 1000 * 6.86) / day_min)
kia_rate <- function() rexp(1, rate = (population / 1000 * 1.63) / day_min)
dnbi_median <- 2.04 - log(population / (1000 * 1440))
dnbi_stddev <- 1.89
dnbi_rate <- function() rlnorm(1, dnbi_median, dnbi_stddev)

# Add casualty generators to simulation
env %>%
  add_generator("wia", casualty, distribution = wia_rate, mon = 2) %>%
  add_generator("kia", casualty, distribution = kia_rate, mon = 2) %>%
  add_generator("dnbi", casualty, distribution = dnbi_rate, mon = 2)

# Run the simulation for 30 days
env %>% run(until = 30 * day_min)

#### VISUALISATIONS ####

# Get arrival logs and annotate with casualty type
arrivals <- get_mon_arrivals(env)
arrivals$day <- floor(arrivals$start_time / day_min)
arrivals$type <- ifelse(
  grepl("^wia", arrivals$name), "WIA",
  ifelse(grepl("^kia", arrivals$name), "KIA", "DNBI")
)
# arrivals$type <- ifelse(grepl("^wia", arrivals$name), "WIA", "KIA")

# Plot daily WIA, KIA and DNBI totals
ggplot(arrivals, aes(x = day, fill = type)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  scale_x_continuous(breaks = seq(0, max(arrivals$day), by = 2)) +
  labs(title = "Daily Casualties by Type", x = "Day", y = "Count", fill = "Type") +
  scale_fill_manual(values = c("WIA" = "steelblue", "KIA" = "darkred", "DNBI" = "seagreen")) +
  theme_minimal()

# Plot resource usage
resources <- get_mon_resources(env)
plot(resources, metric = "usage", c(
  "medic_1_t1", "medic_2_t1", "nurse_t1", "doctor_t1",
  "medic_1_t2", "medic_2_t2", "nurse_t2", "doctor_t2"
)) +
  ggtitle("Resource Usage Over Time") +
  theme_minimal()

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
resources$team <- ifelse(grepl("_t1$", resources$resource), "Team 1", "Team 2")
resources$role <- sub("_t[12]$", "", resources$resource)

agg_resources <- aggregate(busy_time ~ team + role + day, data = resources, sum)
agg_resources$percent_seized <- round((agg_resources$busy_time / day_min) * 100, 2)

# Count casualties per team per day
arrivals$team <- get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$value[match(arrivals$name, get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$name)]
arrivals$team <- factor(arrivals$team, labels = c("Team 1", "Team 2"))
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
    title = "Daily Resource Utilization by Role with Casualty Load",
    x = "Day",
    color = "Resource Role"
  ) +
  scale_color_manual(values = c(
    "medic_1" = "#1b9e77",
    "medic_2" = "#d95f02",
    "nurse" = "#7570b3",
    "doctor" = "#e7298a"
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
