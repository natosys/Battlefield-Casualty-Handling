library(ggplot2)

##############################################
## Resource Usage Plots                     ##
##############################################
# Generate and display usage plots for clinical resources by team
for (team in seq_len(counts[["r1"]])) {
  clinical_resources_team <- env_data$elms$r1[[team]]
  team_resources_filtered <- all_resources[all_resources$resource %in% clinical_resources_team, ]
  
  team_plot <- plot(team_resources_filtered, metric = "usage") +
    ggtitle(paste("Resource Usage Over Time - R1 Team", team)) +
    theme_minimal()
  
  print(team_plot)  # Force plot to render
}

# Generate and display usage plots for r2b clinical resources by team
for (team in seq_len(counts[["r2b"]])) {
  clinical_resources_team <- env_data$elms$r2b[[team]][["emerg"]]
  team_resources_filtered <- all_resources[all_resources$resource %in% clinical_resources_team, ]
  
  team_plot <- plot(team_resources_filtered, metric = "usage") +
    ggtitle(paste("R2B Emergency Resource Usage - Team", team)) +
    theme_minimal()
  
  print(team_plot)  # Force plot to render
}

transport_plot <- plot(all_resources, metric = "usage", transport_resources) +
  ggtitle("Transport Resource Usage Over Time") +
  theme_minimal()

print(transport_plot)  # Force plot to display

##############################################
## Daily Casualties by Type (WIA/KIA/DNBI)  ##
##############################################
# Plot daily WIA, KIA and DNBI totals
casualty_plot <- ggplot(arrivals, aes(x = day, fill = type)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  scale_x_continuous(breaks = seq(0, max(arrivals$day), by = 2)) +
  labs(title = "Daily Casualties by Type", x = "Day", y = "Count", fill = "Type") +
  scale_fill_manual(values = c("WIA" = "steelblue", "KIA" = "darkred", "DNBI" = "seagreen")) +
  theme_minimal()

print(casualty_plot)

##############################################
## Daily Casualties by Priority             ##
##############################################
# Plot daily casualties by priority
daily_casualties <- ggplot(arrivals, aes(x = day, fill = priority)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  scale_fill_manual(values = c("P1" = "#d73027", "P2" = "#fc8d59", "P3" = "#91bfdb", "KIA" = "black")) +
  labs(title = "Daily Casualties by Priority", x = "Day", y = "Count", fill = "Priority") +
  theme_minimal()

print(daily_casualties)

##############################################
## Cumulative Casualty and Force Loss Graph ##
##############################################
# Step 6: Plot
cumulative_losses_plot <- ggplot(cumulative_counts_long, aes(x = day, y = Count, color = Metric)) +
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

print(cumulative_losses_plot)

##############################################
## Composite Utilisation and Casualty Load  ##
##############################################
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

# Final composite plot: utilization + casualty load
r1_utilisation_plot <- ggplot(utilisation_data, aes(x = day)) +
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

print(r1_utilisation_plot)

##############################################
## Casualty Breakdown by Source and Type    ##
##############################################
# Summarize total casualties by source and type
casualty_breakdown <- as.data.frame(table(arrivals$source, arrivals$type))
colnames(casualty_breakdown) <- c("Source", "Type", "Count")

# Plot: Casualty breakdown by source and type
source_type_plot <- ggplot(casualty_breakdown, aes(x = Source, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("WIA" = "steelblue", "KIA" = "darkred", "DNBI" = "seagreen")) +
  labs(
    title = "Casualty Breakdown by Source",
    x = "Casualty Source",
    y = "Total Count",
    fill = "Casualty Type"
  ) +
  theme_minimal()

print(source_type_plot)

########################################
## R2B Surgeries Per Day              ##
########################################
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

total_per_day <- aggregate(surgeries ~ day, data = surgery_df, sum)
max_total <- max(total_per_day$surgeries)

# Create the stacked bar plot
r2b_daily_surgeries <- ggplot(surgery_df, aes(x = day, y = surgeries, fill = r2b_team)) +
  geom_bar(stat = "identity") +
  labs(
    title = "R2B Surgeries Per Day",
    x = "Day",
    y = "Number of Surgeries",
    fill = "R2B Team"
  ) +
  scale_y_continuous(breaks = seq(0, max_total, by = 1)) +
  theme_minimal()

print(r2b_daily_surgeries)

########################################
## Casualties Handled by R2E          ##
########################################
# Filter R2E-handled casualties
r2e_cases <- arrivals[!is.na(arrivals$r2e_handling), ]

# Mark bypass status
r2e_cases$r2b_bypassed[is.na(r2e_cases$r2b_bypassed)] <- 0
r2e_cases$r2e_path <- ifelse(r2e_cases$r2b_bypassed == 1, "Direct from R1", "From R2B")

# Calculate simulation day
r2e_cases$day <- floor(r2e_cases$start_time / 1440) + 1

# Tabulate counts
counts_table <- table(r2e_cases$day, r2e_cases$r2e_path)
counts_df <- as.data.frame(counts_table)
colnames(counts_df) <- c("day", "path", "count")
counts_df$day <- as.numeric(as.character(counts_df$day))

# Calculate max stacked total per day
total_per_day <- aggregate(count ~ day, data = counts_df, sum)
max_total <- max(total_per_day$count)

# Plot
r2e_cases <- ggplot(counts_df, aes(x = day, y = count, fill = path)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Casualties Handled by R2E per Day",
    x = "Day",
    y = "Number of Casualties",
    fill = "Source of Casualty"
  ) +
  scale_y_continuous(breaks = seq(0, max_total, by = 1)) +
  theme_minimal()

print(r2e_cases)