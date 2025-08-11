library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(knitr)
library(data.table)

# Save as CSV (readable, good for Excel or reports)
write.csv(get_mon_arrivals(env, ongoing = TRUE), "data/mon_arrivals.csv", row.names = FALSE)
write.csv(get_mon_attributes(env), "data/mon_attributes.csv", row.names = FALSE)
write.csv(get_mon_resources(env), "data/mon_resources.csv", row.names = FALSE)

# Read in the monitoring data
arrivals <- read.csv("data/mon_arrivals.csv")
arrivals <- arrivals %>%
  mutate(waiting_time = end_time - start_time - activity_time)
attributes <- read.csv("data/mon_attributes.csv")
resources <- read.csv("data/mon_resources.csv")

# Pivot attributes wide so each 'key' becomes a column, values are the attribute values
attributes_wide <- attributes %>%
  pivot_wider(
    id_cols = c(name, replication),
    names_from = key,
    values_fn = ~ first(.x)
  )

# Join with arrivals by name and replication
combined <- arrivals %>%
  left_join(attributes_wide, by = c("name", "replication"))

combined <- combined %>%
  mutate(
    # Extract casualty type before the first underscore
    casualty_type = str_extract(name, "^[^_]+"),
    
    # Extract everything after the underscore except the trailing number
    population_source = str_extract(name, "(?<=_)[a-zA-Z]+"),
    
    arrival_day = floor(start_time / (24 * 60)) + 1
  )

casualty_summary <- combined %>%
  group_by(arrival_day, casualty_type, population_source) %>%
  summarise(
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(arrival_day, casualty_type, population_source)

casualty_priority_summary <- combined %>%
  mutate(
    priority_group = case_when(
      casualty_type == "kia" ~ "KIA",
      priority %in% c(1, 2, 3) ~ paste0("Priority ", priority),
      TRUE ~ NA_character_  # optional: exclude undefined categories
    )
  ) %>%
  filter(!is.na(priority_group)) %>%
  group_by(arrival_day, priority_group) %>%
  summarise(
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(arrival_day, priority_group)

# Re-order for visually logical stacking
casualty_priority_summary <- casualty_priority_summary %>%
  mutate(priority_group = factor(priority_group, levels = c("Priority 1", "Priority 2", "Priority 3", "KIA")))

# Step 1: Get max daily total
max_daily_total <- casualty_summary %>%
  group_by(arrival_day) %>%
  summarise(total = sum(count)) %>%
  pull(total) %>%
  max()

# Step 2: Use it in both ggplot charts
plot_type <- ggplot(casualty_summary, aes(x = arrival_day, y = count, fill = casualty_type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(
    breaks = seq(min(casualty_priority_summary$arrival_day), max(casualty_priority_summary$arrival_day), by = 1)
  ) +
  ylim(0, max_daily_total) +
  labs(
    title = "Total Casualties by Type and Arrival Day",
    x = "Arrival Day", y = "Number of Casualties", fill = "Casualty Type"
  ) +
  theme_minimal()

plot_source <- ggplot(casualty_summary, aes(x = arrival_day, y = count, fill = population_source)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(
    breaks = seq(min(casualty_priority_summary$arrival_day), max(casualty_priority_summary$arrival_day), by = 1)
  ) +
  ylim(0, max_daily_total) +
  labs(
    title = "Total Casualties by Population Source and Arrival Day",
    x = "Arrival Day", y = "Number of Casualties", fill = "Population Source"
  ) +
  theme_minimal()

plot_priority <- ggplot(casualty_priority_summary, aes(x = arrival_day, y = count, fill = priority_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Pastel1") +
  scale_x_continuous(
    breaks = seq(min(casualty_priority_summary$arrival_day), max(casualty_priority_summary$arrival_day), by = 1)
  ) +
  ylim(0, max_daily_total) +
  labs(
    title = "Total Casualties by Priority Level and Arrival Day",
    x = "Arrival Day", y = "Number of Casualties", fill = "Priority Group"
  ) +
  theme_minimal()

plot_type / plot_source / plot_priority # or plot_type + plot_source for side-by-side

##############################################
## SUMMARY TABLES                           ##
##############################################
# Create tables summarising data matched to the graphs produced
# 1. Casualties by Type (with Total Row)
casualty_type_table_wide <- casualty_summary %>%
  pivot_wider(
    names_from = arrival_day,
    values_from = count,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  arrange(casualty_type, population_source)

# Add total row
casualty_type_total_row <- casualty_type_table_wide %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(casualty_type = "Total", population_source = "")

casualty_type_table_wide <- bind_rows(casualty_type_table_wide, casualty_type_total_row)

# 2. Casualties by Population Source (with Total Row)
population_source_table_wide <- casualty_summary %>%
  group_by(arrival_day, population_source) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  pivot_wider(
    names_from = arrival_day,
    values_from = count,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  arrange(population_source)

# Add total row
population_source_total_row <- population_source_table_wide %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(population_source = "Total")

population_source_table_wide <- bind_rows(population_source_table_wide, population_source_total_row)

# 3. Casualties by Priority Group (with Total Row)
priority_table_wide <- casualty_priority_summary %>%
  pivot_wider(
    names_from = arrival_day,
    values_from = count,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  arrange(priority_group)

# Add total row
priority_total_row <- priority_table_wide %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(priority_group = "Total")

priority_table_wide <- bind_rows(priority_table_wide, priority_total_row)

# Priority table
writeLines(kable(priority_table_wide, format = "markdown"), "priority_table.md")

# Population source table
writeLines(kable(population_source_table_wide, format = "markdown"), "population_source_table.md")

# Casualty type table
writeLines(kable(casualty_type_table_wide, format = "markdown"), "casualty_table.md")

##############################################
## R2B Bed QUEUE GRAPHs                     ##
##############################################
queue_plot_data <- resources %>%
  as.data.frame() %>%
  filter(grepl("^b_r2b_.*_\\d+_t\\d+$", resource)) %>%
  select(time, resource, queue) %>%
  mutate(
    r2b_id    = str_extract(resource, "_t\\d+$") %>% str_remove("_t") %>% as.integer(),
    bed_type  = str_extract(resource, "(?<=b_r2b_)[^_]+") %>% toupper(),
    bed_index = str_extract(resource, "(?<=_)[0-9]+(?=_t)") %>% as.integer(),
    r2b_label = paste0("R2B ", r2b_id),
    bed_label = paste0(bed_type, " ", bed_index)
  )

ggplot(queue_plot_data, aes(x = time / 1440, y = queue, color = bed_label)) +
  geom_step(linewidth = 1) +
  labs(
    title = "Queue Length Over Time by R2B",
    x = "Time (Days)",
    y = "Queue Size",
    color = "Bed"
  ) +
  scale_x_continuous(
    breaks = seq(0, max(queue_plot_data$time) / 1440, by = 1),  # One tick per day
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 1),
    expand = c(0, 0)
  ) +
  facet_wrap(~ r2b_label, ncol = 1, scales = "free_x") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", color = "gray"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

##############################################
## WIDE ATTRIBUTES FOR GRAPHS               ##
##############################################
attributes_wide <- attributes %>%
  pivot_wider(
    id_cols = c(name, replication, time),
    names_from = key,
    values_fn = ~ first(.x)
  ) %>%
  arrange(name, replication, time) %>%
  group_by(name, replication) %>%
  fill(everything(), .direction = "down") %>%
  slice_tail(n = 1) %>%
  ungroup()

##############################################
## CASUALTY TREATMENT SUMMARY               ##
##############################################
# 1. Join arrivals with attributes_wide
combined <- arrivals %>%
  left_join(attributes_wide, by = c("name", "replication"))

# 2. Filter for any R2B-treated casualties
r2b_casualties <- combined %>%
  filter(!is.na(r2b_treated) & r2b_treated > 0)

# 3. Convert r2b_treated to station label
r2b_casualties <- r2b_casualties %>%
  mutate(r2b_station = paste0("R2B ", r2b_treated))

# 4. Convert start_time to simulation day
r2b_casualties <- r2b_casualties %>%
  mutate(day = floor(start_time / 1440) + 1)

# 5. Summarize daily counts by R2B station
daily_station_summary <- r2b_casualties %>%
  count(day, r2b_station)

# 6. Define custom colors
r2b_levels <- unique(daily_station_summary$r2b_station)
r2b_colors <- setNames(
  RColorBrewer::brewer.pal(n = length(r2b_levels), name = "Set2"),
  r2b_levels
)

# 7. Plot stacked bar chart
ggplot(daily_station_summary, aes(x = factor(day), y = n, fill = r2b_station)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(
    title = "Casualties Treated at R2B Stations by Simulation Day",
    x = "Simulation Day",
    y = "Number of Casualties",
    fill = "R2B Station"
  ) +
  scale_fill_manual(values = r2b_colors) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  theme_minimal(base_size = 14)

##############################################
## R2B SURGERY SUMMARY                      ##
##############################################
r2b_summary <- attributes_wide %>%
  filter(!is.na(r2b_surgery_start)) %>%
  mutate(
    r2b_day = floor(as.numeric(r2b_surgery_start) / 1440) + 1,
    r2b_station = paste0("R2B ", r2b_treated)
  ) %>%
  count(r2b_day, r2b_station, name = "surgeries")

ggplot(r2b_summary, aes(x = factor(r2b_day), y = surgeries, fill = r2b_station)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(title = "R2B Surgeries Started Per Simulation Day by Station",
       x = "Simulation Day", y = "Number of Surgeries", fill = "R2B Station") +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)

##############################################
## R2B RESOURCE USAGE (clean facet version) ##
##############################################
r2b_bed_usage <- resources %>%
  as.data.frame() %>%
  filter(str_detect(resource, "^b_r2b_\\w+_\\d+_t\\d+$")) %>%
  arrange(replication, resource, time) %>%
  group_by(replication) %>%
  mutate(
    rep_end_time = if (all(is.na(time))) NA_real_ else max(time, na.rm = TRUE)
  ) %>%
  group_by(replication, resource) %>%
  mutate(
    start_time = time,
    end_time   = lead(time),
    end_time   = if_else(is.na(end_time), rep_end_time, end_time)
  ) %>%
  filter(server > 0, end_time > start_time) %>%
  ungroup() %>%
  mutate(
    bed_type  = str_match(resource, "^b_r2b_([^_]+)_")[,2],
    bed_num   = str_match(resource, "^b_r2b_[^_]+_(\\d+)_")[,2],
    r2b_team  = str_match(resource, "_t(\\d+)$")[,2],
    resource_label = paste(toupper(bed_type), "Bed", bed_num),
    bed_num_i = as.integer(bed_num),
    r2b_team_i = as.integer(r2b_team)
  ) %>%
  arrange(r2b_team_i, bed_type, bed_num_i) %>%
  mutate(
    r2b_team = factor(r2b_team, levels = unique(r2b_team))
  )

# Drop unused factor levels per facet
r2b_bed_usage <- r2b_bed_usage %>%
  group_by(r2b_team) %>%
  mutate(resource_label = factor(resource_label, levels = unique(resource_label))) %>%
  ungroup()

# Axis prep
max_days <- {
  max_end <- suppressWarnings(max(r2b_bed_usage$end_time, na.rm = TRUE))
  if (!is.finite(max_end)) NA_real_ else ceiling(max_end / 1440)
}
x_breaks <- if (is.finite(max_days) && max_days >= 1) seq(1, max_days, by = 1) else waiver()

# Faceted Gantt plot
ggplot(
  r2b_bed_usage,
  aes(y = resource_label, color = toupper(bed_type))
) +
  geom_segment(
    aes(
      x = start_time / 1440,
      xend = end_time / 1440,
      yend = resource_label
    ),
    linewidth = 6,
    lineend = "butt"
  ) +
  labs(
    title = "R2B Bed Resource Usage (Gantt) by Team",
    x = "Time (Days)",
    y = "Bed Resource",
    color = "Bed Type"
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = function(x) paste0(x),
    expand = c(0, 0)
  ) +
  facet_wrap(
    ~ r2b_team,
    ncol = 1,
    scales = "free_y",  # allows each facet to drop unused rows
    labeller = labeller(r2b_team = function(x) paste0("R2B ", x))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", color = "gray"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

##############################################
## R2E SURGERIES                            ##
##############################################
r2e_summary <- attributes_wide %>%
  select(name, starts_with("r2e_surgery_")) %>%
  pivot_longer(cols = starts_with("r2e_surgery_"), names_to = "surgery_type", values_to = "start_min") %>%
  filter(!is.na(start_min)) %>%
  mutate(r2e_day = floor(start_min / 1440)) %>%
  count(r2e_day, name = "surgeries")

ggplot(r2e_summary, aes(x = r2e_day, y = surgeries)) +
  geom_bar(stat = "identity", fill = "#007ACC") +
  labs(title = "R2E-Heavy Surgeries Completed Per Simulation Day",
       x = "Simulation Day", y = "Number of Surgeries") +
  scale_x_continuous(
    breaks = seq(1, max_days, by = 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(breaks = 0:25, limits = c(0, 25)) +
  theme_minimal(base_size = 14)

##############################################
## R2E BED QUEUE GRAPH                      ##
##############################################
prepare_queue_data <- function(resource_type, data) {
  pattern <- paste0("^b_r2eheavy_", resource_type, "_\\d+_t\\d+$")
  
  queue_plot_data <- data %>%
    as.data.frame() %>%
    filter(grepl(pattern, resource)) %>%
    select(time, resource, queue) %>%
    mutate(
      resource_type = toupper(resource_type),
      bed_number = gsub("^b_r2eheavy_.*?_(\\d+)_t\\d+$", "\\1", resource),
      r2e_number = gsub("^b_r2eheavy_.*?_\\d+_t(\\d+)$", "\\1", resource),
      resource_label = paste(resource_type, "Bed", bed_number)
    )
  
  return(queue_plot_data)
}

combined_queue_data <- bind_rows(
  prepare_queue_data("ot", resources),
  prepare_queue_data("icu", resources)
)

ggplot(combined_queue_data, aes(x = time / 1440, y = queue, color = resource_label)) +
  geom_step(linewidth = 1) +
  labs(
    title = "R2E Heavy Bed Queue Length Over Time by Resource Type",
    x = "Time (Days)",
    y = "Queue Size",
    color = "Resource"
  ) +
  facet_wrap(~ resource_type, ncol = 1, scales = "fixed") +
  scale_x_continuous(
    breaks = seq(1, ceiling(max(combined_queue_data$time) / 1440), by = 1),
    labels = function(x) paste0(x),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 1),
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", color = "gray")
  )

ggplot(arrivals, aes(x = start_time / (60 * 24), y = waiting_time)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(
    title = "Casualty Waiting Time Over Simulation",
    x = "Simulation Day",
    y = "Waiting Time (min)"
  ) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred") +
  theme_minimal(base_size = 14)

##############################################
## R2E RESOURCE USAGE                       ##
##############################################
# Filter to R2E bed resources and build usage intervals (server > 0)
r2e_bed_usage <- resources %>%
  as.data.frame() %>%
  filter(str_detect(resource, "^b_r2eheavy_\\w+_\\d+_t\\d+$")) %>%
  arrange(replication, resource, time) %>%
  group_by(replication) %>%
  mutate(rep_end_time = max(time)) %>%          # simulation end per replication
  group_by(replication, resource) %>%
  mutate(
    start_time = time,
    end_time   = lead(time),
    end_time   = if_else(is.na(end_time), rep_end_time, end_time)
  ) %>%
  filter(server > 0, end_time > start_time) %>% # keep only intervals when bed is in use
  ungroup() %>%
  mutate(
    bed_type   = str_match(resource, "^b_r2eheavy_([^_]+)_")[,2],
    bed_num    = str_match(resource, "^b_r2eheavy_[^_]+_(\\d+)_")[,2],
    r2e_team   = str_match(resource, "_t(\\d+)$")[,2],
    resource_label = paste(toupper(bed_type), "Bed", bed_num)
  )

# Order beds by type, bed number, then team for readable y-axis
r2e_bed_usage <- r2e_bed_usage %>%
  mutate(
    bed_num_i = as.integer(bed_num),
    r2e_team_i = as.integer(r2e_team)
  ) %>%
  arrange(bed_type, bed_num_i, r2e_team_i) %>%
  mutate(resource_label = factor(resource_label, levels = unique(resource_label)))

# Compute max days for axis ticks
max_days <- ceiling(max(r2e_bed_usage$end_time, na.rm = TRUE) / 1440)

# Gantt-style plot (one row per bed, colored by bed type)
ggplot(
  r2e_bed_usage,
  aes(y = resource_label, color = toupper(bed_type))
) +
  geom_segment(
    aes(
      x = start_time / 1440,
      xend = end_time / 1440,
      yend = resource_label
    ),
    linewidth = 6,
    lineend = "butt"
  ) +
  labs(
    title = "R2E Bed Resource Usage (Gantt)",
    x = "Time (Days)",
    y = "Bed Resource",
    color = "Bed Type"
  ) +
  scale_x_continuous(
    breaks = seq(1, max_days, by = 1),
    labels = function(x) paste0(x),
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", color = "gray"),
    legend.position = "bottom"
  )

