library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(knitr)

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
  dplyr::select(time, resource, queue) %>%
  mutate(
    r2b_id = str_extract(resource, "_t\\d+$") %>% str_remove("_t") %>% as.integer(),
    r2b_label = paste0("R2B ", r2b_id),
    bed_type = str_extract(resource, "(?<=b_r2b_)[^_]+") %>% toupper()
  )

ggplot(queue_plot_data, aes(x = time, y = queue, color = bed_type)) +
  geom_line(size = 1) +
  labs(
    title = "R2B Bed Queue Length Over Time by Bed Type",
    x = "Simulation Day",
    y = "Queue Size",
    color = "Bed Type",
    caption = "Day 1 corresponds to simulation start (time = 0 min)"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(queue_plot_data$time), by = 60 * 24),
    labels = function(x) round(x / (60 * 24) + 1, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 1),
    expand = c(0, 0)
  ) +
  facet_wrap(~r2b_label, ncol = 1, scales = "fixed") +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

##############################################
## CASUALTY TREATMENT SUMMARY               ##
##############################################
# 1. Pivot attributes wide
attributes_wide <- attributes %>%
  pivot_wider(
    id_cols = c(name, replication),
    names_from = key,
    values_fn = ~ first(.x)
  )

# 2. Join with arrivals
combined <- arrivals %>%
  left_join(attributes_wide, by = c("name", "replication"))

# 3. Filter for R2E-treated casualties
r2e_casualties <- combined %>%
  filter(!is.na(r2e_treated) & r2e_treated > 0)

# 4. Convert r2b_treated to station label
r2e_casualties <- r2e_casualties %>%
  mutate(r2b_station = case_when(
    is.na(r2b_treated) | r2b_treated == 0 ~ "Skipped R2B",
    TRUE ~ paste0("R2B", r2b_treated)
  ))

# 5. Convert start_time to simulation day
r2e_casualties <- r2e_casualties %>%
  mutate(day = floor(start_time / 1440) + 1)

# 6. Summarize daily counts by R2B station
daily_station_summary <- r2e_casualties %>%
  count(day, r2b_station)

# 7. Optional: define custom colors
r2b_levels <- unique(daily_station_summary$r2b_station)
r2b_colors <- setNames(
  c(RColorBrewer::brewer.pal(n = length(r2b_levels) - 1, name = "Set2"), "#ff7f0e"),
  c(setdiff(r2b_levels, "Skipped R2B"), "Skipped R2B")
)

# 8. Plot stacked bar chart
ggplot(daily_station_summary, aes(x = factor(day), y = n, fill = r2b_station)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(
    title = "Casualties Routed Through Each R2B Station by Simulation Day",
    x = "Simulation Day",
    y = "Number of Casualties",
    fill = "R2B Station"
  ) +
  scale_fill_manual(values = r2b_colors) +
  theme_minimal(base_size = 14)

##############################################
## R2E OT Bed QUEUE GRAPH                   ##
##############################################
queue_plot_data <- resources %>%
  as.data.frame() %>%
  filter(resource %in% c("b_r2eheavy_ot_1_t1", "b_r2eheavy_ot_2_t1", "b_r2eheavy_ot_3_t1")) %>%
  dplyr::select(time, resource, queue)

ggplot(queue_plot_data, aes(x = time, y = queue, color = resource)) +
  geom_line(size = 1) +
  labs(
    title = "R2E Heavy OT Bed Queue Length Over Time – 3 Surgical Teams",
    x = "Simulation Day",
    y = "Queue Size",
    color = "Resource",
    caption = "Day 1 corresponds to simulation start (time = 0 min)"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(queue_plot_data$time), by = 60 * 24),
    labels = function(x) round(x / (60 * 24) + 1, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, by = 1),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    values = c(
      "b_r2eheavy_ot_1_t1" = "#1f77b4",
      "b_r2eheavy_ot_2_t1" = "#ff7f0e",
      "b_r2eheavy_ot_3_t1" = "#2ca02c"
    ),
    labels = c(
      "b_r2eheavy_ot_1_t1" = "OT 1",
      "b_r2eheavy_ot_2_t1" = "OT 2",
      "b_r2eheavy_ot_3_t1" = "OT 3"
    )
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

