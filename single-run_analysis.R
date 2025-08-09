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
## QUEUE GRAPHS                             ##
##############################################
queue_plot_data <- resources %>%
  # as.data.frame() %>%
  # as_tibble() %>%  # Ensures compatibility with dplyr verbs
  filter(resource %in% c("b_r2eheavy_ot_1_t1", "b_r2eheavy_ot_2_t1")) %>%
  dplyr::select(time, resource, queue)

ggplot(queue_plot_data, aes(x = time, y = queue, color = resource)) +
  geom_line(size = 1) +
  labs(
    title = "R2E Heavy OT Bed Queue Length Over Time",
    x = "Simulation Day",
    y = "Queue Size",
    color = "Resource"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(queue_plot_data$time), by = 60 * 24),
    labels = function(x) round(x / (60 * 24) + 1, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    values = c("b_r2eheavy_ot_1_t1" = "#1f77b4", "b_r2eheavy_ot_2_t1" = "#ff7f0e"),
    labels = c("b_r2eheavy_ot_1_t1" = "OT 1", "b_r2eheavy_ot_2_t1" = "OT 2")
  ) + 
  labs(
    caption = "Day 1 corresponds to simulation start (time = 0 min)"
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

#TEMP
# read_arrivals_file <- function(filename, day_min = 1440) {
#   # Extract file stem (without extension)
#   stem <- tools::file_path_sans_ext(basename(filename))
#   
#   # Parse type and pop source
#   parts <- strsplit(stem, "_")[[1]]
#   if (length(parts) != 3 || parts[1] != "arrivals") {
#     stop("Filename must follow format 'arrivals_<type>_<source>.txt'")
#   }
#   
#   cas_type <- parts[2]
#   pop_source <- parts[3]
#   
#   # Read arrival times
#   arrival_times <- scan(filename, quiet = TRUE)
#   
#   tibble(
#     start_time = arrival_times,
#     arrival_day = floor(arrival_times / day_min) + 1,
#     cas_type = cas_type,
#     pop_source = pop_source
#   )
# }

# arrivals_tbl <- bind_rows(
#   read_arrivals_file("data/arrivals_dnbi_cbt.txt"),
#   read_arrivals_file("data/arrivals_dnbi_spt.txt"),
#   read_arrivals_file("data/arrivals_kia_cbt.txt"),
#   read_arrivals_file("data/arrivals_kia_spt.txt"),
#   read_arrivals_file("data/arrivals_wia_cbt.txt"),
#   read_arrivals_file("data/arrivals_wia_spt.txt")
# )
# 
# p_type <- ggplot(arrivals_tbl, aes(x = arrival_day, fill = cas_type)) +
#   geom_bar(stat = "count", position = "stack") +
#   scale_fill_brewer(palette = "Set1") +
#   scale_x_continuous(
#     breaks = seq(min(arrivals_tbl$arrival_day), max(arrivals_tbl$arrival_day), by = 1)
#   ) +
#   labs(
#     title = "Arrivals by Casualty Type",
#     x = "Arrival Day",
#     y = "Number of Casualties",
#     fill = "Casualty Type"
#   ) +
#   theme_minimal()
# 
# p_source <- ggplot(arrivals_tbl, aes(x = arrival_day, fill = pop_source)) +
#   geom_bar(stat = "count", position = "stack") +
#   scale_fill_brewer(palette = "Dark2") +
#   scale_x_continuous(
#     breaks = seq(min(arrivals_tbl$arrival_day), max(arrivals_tbl$arrival_day), by = 1)
#   ) +
#   labs(
#     title = "Arrivals by Population Source",
#     x = "Arrival Day",
#     y = "Number of Casualties",
#     fill = "Population Source"
#   ) +
#   theme_minimal()
# 
# # Combine side-by-side
# p_type / p_source

# ggplot() +
#   # Background: stacked count from raw arrivals, more transparent
#   geom_bar(
#     data = arrivals_tbl,
#     aes(x = arrival_day, fill = pop_source),
#     position = "stack", stat = "count", alpha = 0.3
#   ) +
#   
#   # Foreground: modeled totals from casualty_summary
#   geom_bar(
#     data = casualty_summary,
#     aes(x = arrival_day, y = count, fill = population_source),
#     position = "stack", stat = "identity", alpha = 1
#   ) +
#   
#   scale_fill_brewer(palette = "Dark2") +
#   scale_x_continuous(
#     breaks = seq(
#       min(casualty_priority_summary$arrival_day),
#       max(casualty_priority_summary$arrival_day),
#       by = 1
#     )
#   ) +
#   ylim(0, max_daily_total) +
#   labs(
#     title = "Overlay: Raw Arrival Counts vs Casualties Processed",
#     x = "Arrival Day",
#     y = "Number of Casualties",
#     fill = "Population Source"
#   ) +
#   theme_minimal()

# #TEMP
# resource_data <- get_mon_resources(env)
# 
# end_time <- max(resource_data$time)
# 
# resource_queues <- resource_data %>%
#   filter(time == end_time) %>%
#   select(resource, queue, server)
# 
# resource_queues %>%
#   filter(resource == "c_r1_clinician_nurse_1_t2")
# 
# missing_from_arrivals <- arrivals_tbl %>%
#   anti_join(arrivals, by = "start_time")
# 
# log_lines <- readLines("logs.txt")
# 
# # Split each line into 3 parts using ": " as delimiter
# split_matrix <- str_split_fixed(log_lines, pattern = ": ", n = 3)
# 
# # Convert to tibble and name columns
# log_df <- as_tibble(split_matrix) %>%
#   rename(start_time = V1, entity = V2, message = V3) %>%
#   mutate(start_time = as.numeric(start_time))
# 
# missing_from_logs <- arrivals_tbl %>%
#   anti_join(log_df, by = "start_time")
