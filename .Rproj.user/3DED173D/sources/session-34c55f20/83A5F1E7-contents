library(simmer)
library(simmer.bricks)
library(ggplot2)
library(scales)

# Constants
day_min <- 1440
population <- 1000

env <- simmer("Battlefield Casualty Handling")

# Function to generate treatment team names
team_resources <- function(team_id) {
  c(paste0("medic_1_t", team_id),
    paste0("medic_2_t", team_id),
    paste0("nurse_t", team_id),
    paste0("doctor_t", team_id))
}

# Add two identical treatment teams
for (team in 1:2) {
  env %>%
    add_resource(paste0("medic_1_t", team), 1) %>%
    add_resource(paste0("medic_2_t", team), 1) %>%
    add_resource(paste0("nurse_t", team), 1) %>%
    add_resource(paste0("doctor_t", team), 1)
}

# KIA-specific treatment logic
kia_resources <- function(team_id) {
  c(paste0("medic_1_t", team_id), paste0("medic_2_t", team_id))
}

treat_kia <- function(team) {
  team_res <- kia_resources(team)
  trajectory(paste("KIA Team", team)) %>%
    select(team_res, policy = "shortest-queue") %>%
    seize_selected() %>%
    timeout(function() rnorm(1, 15)) %>%
    release_selected()
}

# WIA-specific treatment logic
wia_resources <- function(team_id) {
  c(paste0("nurse_t", team_id), paste0("doctor_t", team_id))
}

treat_wia <- function(team) {
  team_res <- wia_resources(team)
  trajectory(paste("WIA Team", team)) %>%
    select(team_res, policy = "shortest-queue") %>%
    seize_selected() %>%
    timeout(function() {
      prio <- get_attribute(env, "priority")
      switch(
        as.character(prio),
        "1" = rlnorm(1, log(120), 0.125),
        "2" = rlnorm(1, log(60), 0.25),
        "3" = rlnorm(1, log(30), 0.5),
        45
      )
    }) %>%
    release_selected()
}

# Main trajectory: assign team, triage, and branch to treatment
casualty <- trajectory("Casualty") %>%
  set_attribute("team", function() sample(1:2, 1)) %>%
  set_attribute("priority", function() {
    if (startsWith(get_name(env), "wia")) {
      sample(1:3, 1, prob = c(0.65, 0.2, 0.15))
    } else {
      NA
    }
  }) %>%
  branch(
    option = function() {
      if (startsWith(get_name(env), "wia")) 1 else 2
    },
    continue = TRUE,
    trajectory("WIA Branch") %>%
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

# Generator functions
wia_rate <- function() rexp(1, rate = (population / 1000 * 6.86) / day_min)
kia_rate <- function() rexp(1, rate = (population / 1000 * 1.63) / day_min)

# Add generators
env %>%
  add_generator("wia", casualty, distribution = wia_rate, mon = 2) %>%
  add_generator("kia", casualty, distribution = kia_rate, mon = 2)

# Run simulation
env %>% run(until = 30 * day_min)

#### VISUALISATIONS ####

arrivals <- get_mon_arrivals(env)
arrivals$day <- floor(arrivals$start_time / day_min)
arrivals$type <- ifelse(grepl("^wia", arrivals$name), "WIA", "KIA")

# Attach priority info
attributes <- get_mon_attributes(env)
priority <- attributes[attributes$key == "priority", ]
priority_latest <- priority[!duplicated(priority$name, fromLast = TRUE), c("name", "value")]
colnames(priority_latest)[2] <- "priority"

arrivals <- merge(arrivals, priority_latest, by = "name", all.x = TRUE)
arrivals$priority <- ifelse(is.na(arrivals$priority), 5, arrivals$priority)  # Assign 5 to KIA
arrivals$priority <- factor(arrivals$priority, levels = c(1, 2, 3, 5), labels = c("P1", "P2", "P3", "KIA"))

# Plot daily casualty type
ggplot(arrivals, aes(x = day, fill = type)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  scale_x_continuous(breaks = seq(0, max(arrivals$day), by = 2)) +
  labs(title = "Daily Casualties by Type", x = "Day", y = "Count", fill = "Type") +
  scale_fill_manual(values = c("WIA" = "steelblue", "KIA" = "darkred")) +
  theme_minimal()

# Plot resource usage over time
resources <- get_mon_resources(env)

plot(resources, metric = "usage", c(
  "medic_1_t1", "medic_2_t1", "nurse_t1", "doctor_t1",
  "medic_1_t2", "medic_2_t2", "nurse_t2", "doctor_t2"
)) +
  ggtitle("Resource Usage Over Time") +
  theme_minimal()

# Plot daily casualties by priority
ggplot(arrivals, aes(x = day, fill = priority)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  scale_fill_manual(values = c("P1" = "#d73027", "P2" = "#fc8d59", "P3" = "#91bfdb", "KIA" = "black")) +
  labs(title = "Daily Casualties by Priority", x = "Day", y = "Count", fill = "Priority") +
  theme_minimal()

# Calculate daily resource utilization per role and team
resources <- resources[order(resources$resource, resources$time), ]
resources$time_diff <- ave(resources$time, resources$resource, FUN = function(x) c(diff(x), 0))
resources$busy_time <- resources$server * resources$time_diff
resources$day <- floor(resources$time / day_min)
resources$team <- ifelse(grepl("_t1$", resources$resource), "Team 1", "Team 2")
resources$role <- sub("_t[12]$", "", resources$resource)

agg_resources <- aggregate(
  busy_time ~ team + role + day,
  data = resources,
  sum
)
agg_resources$percent_seized <- round((agg_resources$busy_time / day_min) * 100, 2)

# Casualty counts per team per day
arrivals$team <- get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$value[match(arrivals$name, get_mon_attributes(env)[get_mon_attributes(env)$key == "team", ]$name)]
arrivals$team <- factor(arrivals$team, labels = c("Team 1", "Team 2"))
casualty_counts <- as.data.frame(table(arrivals$team, arrivals$day))
colnames(casualty_counts) <- c("team", "day", "casualties")
casualty_counts$day <- as.numeric(as.character(casualty_counts$day))

# Merge for plotting
plot_data <- merge(agg_resources, casualty_counts, by = c("team", "day"), all.x = TRUE)
total_casualties <- aggregate(casualties ~ team, data = casualty_counts, sum)
plot_data <- merge(plot_data, total_casualties, by = "team", suffixes = c("", "_total"))
plot_data$casualty_percent_of_total <- round((plot_data$casualties / plot_data$casualties_total) * 100, 2)

# Plot utilization and casualty load per team and role
ggplot(plot_data, aes(x = day)) +
  facet_wrap(~ team) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 66, ymax = 100, fill = "red", alpha = 0.1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 66, fill = "orange", alpha = 0.1) +
  geom_line(aes(y = percent_seized, color = role), linewidth = 1) +
  geom_point(aes(y = percent_seized, color = role), size = 2) +
  geom_hline(yintercept = 66, linetype = "dashed", color = "red", linewidth = 1) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "orange", linewidth = 1) +
  geom_bar(
    aes(y = casualty_percent_of_total),
    stat = "identity",
    fill = "gray50",
    alpha = 0.3
  ) +
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
  labs(
    title = "Daily Resource Utilization by Role with Casualty Load",
    x = "Day",
    color = "Resource Role"
  ) +
  scale_color_manual(values = c(
    "medic_1" = "red",
    "medic_2" = "orange",
    "nurse" = "green",
    "doctor" = "blue"
  )) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y.right = element_text(color = "gray30"),
    axis.text.y.right = element_text(color = "gray30")
  )
