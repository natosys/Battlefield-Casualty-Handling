# Load the simmer package
library(simmer)
library(simmer.bricks)
library(simmer.plot)
library(ggplot2)
library(scales)

# Define global variables
day_min <- 1440     # Number of minutes in a day
population <- 500   # Number of Soldiers in the battlespace

# Create the simulation environment
env <- simmer("Battlefield Casualty Handling")

# # WIA treatment
treat_wia <- trajectory("WIA Treatment") %>%
  log_(function() paste0("priority: ", get_attribute(env, "priority"))) %>%
  select(c("doctor", "nurse"), policy = "shortest-queue") %>%
  seize_selected() %>%
  timeout(function() {
    priority <- get_attribute(env, "priority")
    if (is.na(priority)) return(0)  # safety check, shouldn't happen
    # Map priority: 1 → 120, 2 → 60, 3 → 30
    duration <- switch(as.character(priority),
                       "1" = rlnorm(1, meanlog = log(120), sdlog = 0.125),
                       "2" = rlnorm(1, meanlog = log(60),  sdlog = 0.25),
                       "3" = rlnorm(1, meanlog = log(30),  sdlog = 0.5),
                       45)  # default fallback
    duration
  }) %>%
  release_selected()

# KIA treatment
treat_kia <- trajectory("KIA Treatment") %>%
  select(c("medic_1", "medic_2"), policy = "shortest-queue") %>%
  seize_selected() %>%
  timeout(function() rnorm(1, 15, 1)) %>%
  release_selected()

# The main casualty trajectory for each arrival
casualty <- trajectory("Casualty Trajectory") %>%
  select(c("medic_1", "medic_2", "nurse", "doctor"), policy = "shortest-queue") %>%
  seize_selected() %>%
  # Casualty triage takes 15 min, normally distributed with std dev of 1 min
  timeout(function() rnorm(1, 15)) %>%
  release_selected() %>%
  
  # Conditionally assign priority if it's a WIA
  set_attribute("priority", function() {
    if (startsWith(get_name(env), "wia")) {
      sample(1:3, size = 1, prob = c(0.65, 0.2, 0.15))  # Adjust weights as needed
    } else {
      NA_real_  # Set to NA for KIA or leave unset
    }
  }) %>%
  
  # Branch based on generator prefix
  branch(
    option = function() {
      name <- get_name(env)
      if (startsWith(name, "wia")) return(1)
      else return(2)
    },
    continue = TRUE,
    treat_wia,
    treat_kia
  )

# WIA generator (6.86 per 1000 per day)
# Rates available are daily per 1000 soldiers so must be converted to a per minute basis 
# and scaled for use in this simulation to allow for the timescale and size of the 
# population simulation. 
wia_rate <- function() {
  lambda_daily = 6.86
  scaled_lambda_daily <- (population / 1000) * lambda_daily
  lambda_per_minute <- scaled_lambda_daily / day_min
  rexp(1, rate = lambda_per_minute)
}

# KIA generator (1.63 per 1000 per day)
# Rates available are daily per 1000 soldiers so must be converted to a per minute basis 
# and scaled for use in this simulation to allow for the timescale and size of the 
# population simulation. 
kia_rate <- function() {
  lambda_daily <- 1.63
  scaled_lambda_daily <- (population / 1000) * lambda_daily
  lambda_per_minute <- scaled_lambda_daily / day_min
  rexp(1, rate = lambda_per_minute)
}

# Add generator with exponential interarrival times
env %>%
  add_resource("medic_1", 1) %>%
  add_resource("medic_2", 1) %>%
  add_resource("nurse", 1) %>%
  add_resource("doctor",1) %>%
  add_generator(
    name_prefix = "wia",
    trajectory = casualty,
    distribution = wia_rate,
    mon = 2
  ) %>%
  add_generator(
    name_prefix = "kia",
    trajectory = casualty,
    distribution = kia_rate
  )

# Run the simulation for 30 days * 1440 minutes (1 day)
env %>% run(until = 30*day_min)

# Get the arrival data
arrivals <- get_mon_arrivals(env)
# Get the resource monitoring data
resources <- get_mon_resources(env)

# Add a day column for easier plotting
arrivals$day <- floor(arrivals$start_time / day_min)
daily_counts <- table(arrivals$day)
max_y <- max(daily_counts)
arrivals$type <- ifelse(grepl("^wia", arrivals$name), "WIA", "KIA")

# Get attribute data
attributes <- get_mon_attributes(env)

# Filter for "priority" attributes only
priority_attrs <- attributes[attributes$key == "priority", ]

# If any entity has multiple entries (shouldn't happen if set once), take the last one
# Aggregate to latest priority by name
priority_latest <- priority_attrs[!duplicated(priority_attrs$name, fromLast = TRUE), c("name", "value")]
colnames(priority_latest)[2] <- "priority"

# Merge with arrivals using entity name
arrivals <- merge(arrivals, priority_latest, by = "name", all.x = TRUE)

# Plot casualty arrivals (both WIA and KIA) per day as a histogram
ggplot(arrivals, aes(x = day, fill = type)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  scale_x_continuous(breaks = seq(0, max(arrivals$day), by = 2)) +
  scale_y_continuous(breaks = seq(0, max_y, by = 1)) +
  scale_fill_manual(values = c("WIA" = "steelblue", "KIA" = "darkred")) +
  labs(
    title = "Daily Casualties by Type",
    x = "Day",
    y = "Number of Casualties",
    fill = "Type"
  ) +
  theme_minimal()

# Plot usage over time
plot(resources, metric = "usage", c("medic_1", "medic_2", "nurse", "doctor")) +
  ggtitle("Resource Usage Over Time") +
  theme_minimal()

# Assign priority to WIA and KIA
arrivals$priority <- ifelse(
  is.na(arrivals$priority),
  5,  # Assign priority 5 to KIA
  arrivals$priority
)

# Convert to factor for consistent ordering and labeling
arrivals$priority <- factor(
  arrivals$priority,
  levels = c(1, 2, 3, 5),
  labels = c("P1", "P2", "P3", "KIA")
)

# Plot the stacked histogram including KIA as "Priority 5"
ggplot(arrivals, aes(x = day, fill = priority)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  scale_x_continuous(breaks = seq(0, max(arrivals$day), by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("P1" = "#d73027", "P2" = "#fc8d59", "P3" = "#91bfdb", "KIA" = "black")) +
  labs(
    title = "Daily Casualties by Priority (Including KIA)",
    x = "Day",
    y = "Number of Casualties",
    fill = "Priority"
  ) +
  theme_minimal()

# Sort by resource and time to prepare for interval calculations
resources <- resources[order(resources$resource, resources$time), ]

# Calculate time difference between successive rows per resource
resources$time_diff <- ave(resources$time, resources$resource, FUN = function(x) c(diff(x), 0))

# Calculate seized (busy) time = busy servers * time interval
resources$busy_time <- resources$server * resources$time_diff

# Add a day column for grouping
resources$day <- floor(resources$time / day_min)

# Aggregate total busy time per resource per day
daily_seized_time <- aggregate(
  busy_time ~ resource + day,
  data = resources,
  FUN = sum
)

# Optional: round to 1 decimal place for clarity
daily_seized_time$busy_time <- round(daily_seized_time$busy_time, 1)

# Rename column for clarity
colnames(daily_seized_time)[3] <- "seized_time_minutes"

# Calculate percent seized (utilization) per day per resource
daily_seized_time$percent_seized <- round((daily_seized_time$seized_time_minutes / day_min) * 100, 2)

# First, prepare casualty counts per day
casualty_counts <- as.data.frame(table(arrivals$day))
colnames(casualty_counts) <- c("day", "casualties")
casualty_counts$day <- as.numeric(as.character(casualty_counts$day))

# Add percent of total casualties in the simulation
total_casualties <- sum(casualty_counts$casualties)
casualty_counts$casualty_percent_of_total <- round((casualty_counts$casualties / total_casualties) * 100, 2)


# Merge casualty data with resource utilization for aligned plotting
plot_data <- merge(daily_seized_time, casualty_counts, by = "day", all.x = TRUE)

ggplot(plot_data, aes(x = day)) +
  # Background shading for utilization thresholds
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 66, ymax = 100, fill = "red", alpha = 0.1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 66, fill = "orange", alpha = 0.1) +
  
  # Resource utilization lines
  geom_line(aes(y = percent_seized, color = resource), size = 1) +
  geom_point(aes(y = percent_seized, color = resource), size = 2) +
  
  # Horizontal threshold lines
  geom_hline(yintercept = 66, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1) +
  
  # Overlay casualty percent bars
  geom_bar(
    aes(y = casualty_percent_of_total),
    stat = "identity",
    fill = "gray70",
    alpha = 0.4
  ) +
  
  scale_y_continuous(
    name = "Resource Utilization (%)",
    limits = c(0, 100),
    labels = scales::percent_format(scale = 1),
    sec.axis = sec_axis(
      trans = ~ .,
      name = "Daily Casualties (% of Total)",
      labels = scales::percent_format(scale = 1)
    ),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(breaks = seq(min(plot_data$day), max(plot_data$day), by = 2)) +
  scale_color_manual(values = c("medic_1" = "red", "medic_2" = "orange", "nurse" = "green", "doctor" = "blue")) +
  
  labs(
    title = "Daily Resource Utilization (%) with Casualty Load",
    x = "Day",
    color = "Resource"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y.right = element_text(color = "gray30"),
    axis.text.y.right = element_text(color = "gray30")
  )