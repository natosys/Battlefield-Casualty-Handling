library(ggplot2)
library(patchwork)

########################################
## R2B Bed Usage Data                 ##
########################################
bed_log <- get_mon_resources(env)
# Filter only bed resources
bed_log <- bed_log[grepl("^b_", bed_log$resource), ]

# Order events chronologically
bed_log <- bed_log[order(bed_log$resource, bed_log$time), ]

# Identify occupancy intervals for each bed
bed_log$time_diff <- ave(bed_log$time, bed_log$resource, FUN = function(x) c(diff(x), 0))
bed_log$start <- bed_log$time
bed_log$end <- bed_log$time + bed_log$time_diff

# Keep only rows where server > 0 (i.e., bed in use)
bed_use <- subset(bed_log, server > 0 & time_diff > 0)
bed_use$team <- sub(".*_t(\\d+)$", "Team \\1", bed_use$resource)

# Transform start/end from minutes to days
bed_use$start_day <- bed_use$start / day_min
bed_use$end_day <- bed_use$end / day_min

# Step 1: Create a Lookup table from resource patterns
bed_use$resource_readable <- sub(".*_(ot|icu|resus|hold)_", "\\1_", bed_use$resource)
bed_use$resource_readable <- gsub("^ot_", "OT ", bed_use$resource_readable)
bed_use$resource_readable <- gsub("^icu_", "ICU ", bed_use$resource_readable)
bed_use$resource_readable <- gsub("^resus_", "Resus ", bed_use$resource_readable)
bed_use$resource_readable <- gsub("^hold_", "Holding ", bed_use$resource_readable)
# Strip _t<number> from end of resource_readable names
bed_use$resource_readable <- sub("_t\\d+$", "", bed_use$resource_readable)

# Step 1: Classify beds by type
bed_use$bed_type <- NA
bed_use$bed_type[grepl("_ot_", bed_use$resource)] <- "OT"
bed_use$bed_type[grepl("_icu_", bed_use$resource)] <- "ICU"
bed_use$bed_type[grepl("_resus_", bed_use$resource)] <- "Resus"
bed_use$bed_type[grepl("_hold_", bed_use$resource)] <- "Holding"
bed_use$bed_type[is.na(bed_use$bed_type)] <- "Other"

# Step 2: Order resources by Severity
severity_levels <- c("Other", "Holding", "ICU", "OT", "Resus")

# Ensure bed_type is a factor in desired order
bed_use$bed_type <- factor(bed_use$bed_type, levels = severity_levels)

# Order resources within team based on severity
bed_use <- bed_use[order(bed_use$team, bed_use$bed_type, bed_use$resource_readable), ]
bed_use$resource_readable <- factor(bed_use$resource_readable, levels = unique(bed_use$resource_readable))

##### ***
  # Extract off-duty intervals for each ot_bed
  ot_bed_roster <- data.frame()

for (i in seq_along(env_data$elms$r2b)) {
  bed_name <- env_data$elms$r2b[[i]]$ot_bed
  start_hour <- (i %% 2) * 12  # same stagger logic
  
  off_start <- (start_hour + 12) %% 24
  ot_bed_roster <- rbind(ot_bed_roster, data.frame(
    resource = bed_name,
    team = paste("Team", i),
    off_start_day = seq(0, max(bed_use$end_day), by = 1),
    off_start_hour = off_start,
    duration_hours = 12
  ))
}

# Expand into day-level shading intervals
ot_bed_roster$start_day <- ot_bed_roster$off_start_day + ot_bed_roster$off_start_hour / 24
ot_bed_roster$end_day   <- ot_bed_roster$start_day + ot_bed_roster$duration_hours / 24
##### ***

resources <- all_resources[!grepl("^t_", all_resources$resource), ]  # Exclude transport resources

# Compute resource usage per team/role/day
resources <- resources[order(resources$resource, resources$time), ]
resources$time_diff <- ave(resources$time, resources$resource, FUN = function(x) c(diff(x), 0))
resources$busy_time <- resources$server * resources$time_diff
resources$day <- floor(resources$time / day_min)
resources$team <- paste("Team", sub(".*_t(\\d+)$", "\\1", resources$resource))
resources$role <- sub("^[a-zA-Z]_(r[0-9]+[a-z]*_).*?_(\\w+_[0-9]+).*", "\\1\\2", resources$resource)
resources$resource_type <- sub("^([cbt])_.*", "\\1", resources$resource)

# Filter for R2B resources only
resources_r2b <- resources[grepl("r2b", resources$resource), ]
resources_r2b <- resources_r2b[order(resources_r2b$resource, resources_r2b$time), ]

# Determine start and end of each busy interval
resources_r2b$start <- resources_r2b$time
resources_r2b$end <- resources_r2b$start + resources_r2b$time_diff

# Exclude resource from graphing (while they are managed as a group)
base_roles <- c(
  "c_r2b_emerg_facem_1",
  "c_r2b_evac_medic_1",
  "c_r2b_surg_surgeon_1",
  "c_r2b_icu_nurse_1"
)
team_ids <- unique(sub(".*(_t\\d+)$", "\\1", resources_r2b$resource))
included_resources <- as.vector(outer(base_roles, team_ids, paste0))
resources_r2b <- resources_r2b[resources_r2b$resource %in% included_resources, ]
resources_r2b <- resources_r2b[resources_r2b$busy_time > 0, ]

# Aggregate daily busy time per role and team
split_across_days <- function(resource_row, day_min) {
  start <- as.numeric(resource_row["start"])
  end <- as.numeric(resource_row["end"])
  team <- resource_row["team"]
  resource <- resource_row["resource"]
  
  # Which days this interval spans
  day_start <- floor(start / day_min)
  day_end <- floor((end - 1e-6) / day_min)
  
  rows <- list()
  
  for (day in day_start:day_end) {
    seg_start <- max(start, day * day_min)
    seg_end <- min(end, (day + 1) * day_min)
    busy_time <- seg_end - seg_start
    
    rows[[length(rows) + 1]] <- data.frame(
      team = team,
      resource = resource,
      day = day,
      busy_time = busy_time,
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, rows)
}

# Prepare: set start/end times
resources_r2b$start <- resources_r2b$time
resources_r2b$end <- resources_r2b$time + resources_r2b$time_diff

# Apply splitting to all rows
split_rows <- do.call(rbind, apply(resources_r2b, 1, split_across_days, day_min = day_min))

# Aggregate by team/resource/day
agg_matrix <- tapply(
  split_rows$busy_time,
  list(split_rows$team, split_rows$resource, split_rows$day),
  sum
)

# Reshape into a data.frame
agg_list <- list()
for (team in dimnames(agg_matrix)[[1]]) {
  for (res in dimnames(agg_matrix)[[2]]) {
    for (d in dimnames(agg_matrix)[[3]]) {
      bt <- agg_matrix[team, res, d]
      if (!is.na(bt)) {
        agg_list[[length(agg_list) + 1]] <- data.frame(
          team = team,
          resource = res,
          day = as.integer(d),
          busy_time = bt,
          percent_seized = round(100 * bt / day_min, 2),
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

agg_resources_r2b <- do.call(rbind, agg_list)

# Step 1: Extract full combinations
teams <- unique(agg_resources_r2b$team)
resources <- unique(agg_resources_r2b$resource)
days <- unique(agg_resources_r2b$day)

# Step 2: Full grid of combinations
full_combos <- expand.grid(
  team = teams,
  resource = resources,
  day = days,
  stringsAsFactors = FALSE
)

# Step 3: Merge with existing data
agg_full <- merge(
  full_combos,
  agg_resources_r2b,
  by = c("team", "resource", "day"),
  all.x = TRUE
)

# Step 4: Fill in 0s for missing data
agg_full$busy_time[is.na(agg_full$busy_time)] <- 0
agg_full$percent_seized[is.na(agg_full$percent_seized)] <- 0

# Step 5: Sort (optional for readability)
agg_resources_r2b <- agg_full[order(agg_full$team, agg_full$resource, agg_full$day), ]

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
utilisation_data_r2b <- merge(agg_resources_r2b, casualty_counts_r2b, by = c("team", "day"), all.x = TRUE)
total_casualties_r2b <- aggregate(casualties ~ team, data = casualty_counts_r2b, sum)
utilisation_data_r2b <- merge(utilisation_data_r2b, total_casualties_r2b, by = "team", suffixes = c("", "_total"))
utilisation_data_r2b$casualty_percent_of_total <- round((utilisation_data_r2b$casualties / utilisation_data_r2b$casualties_total) * 100, 2)
utilisation_data_r2b$role <- sub("_t.*$", "", utilisation_data_r2b$resource)

# For Gantt-style bed plot (in minutes)
vlines_minutes <- seq(0, max(bed_use$end, na.rm = TRUE), by = day_min)

# For bar chart (already in 'day' units)
vlines_days <- seq(min(utilisation_data_r2b$day), max(utilisation_data_r2b$day), by = 1)

day_breaks <- seq(0, max(utilisation_data_r2b$day), by = 1)

########################################
# Combined Graph: Bed and Resource Use #
########################################

# Function for bed Gantt plot by team (if not already defined)
# bed_plot_by_team <- function(team_label) {
#   ggplot(subset(bed_use, team == team_label)) +
#     geom_segment(
#       aes(x = start_day, xend = end_day, y = resource_readable, yend = resource_readable, color = bed_type),
#       linewidth = 4
#     ) +
#     scale_x_continuous(
#       breaks = seq(0, ceiling(max(bed_use$end_day)), by = 1),
#       expand = expansion(mult = c(0, 0))
#     ) +
#     coord_cartesian(xlim = range(day_breaks)) +
#     geom_vline(
#       xintercept = day_breaks,
#       linetype = "dotted",
#       color = "gray20",
#       linewidth = 0.4
#     ) +
#     scale_color_manual(
#       name = "Bed Occupancy",
#       values = c(
#         "OT" = "firebrick",
#         "ICU" = "darkorange",
#         "Resus" = "goldenrod",
#         "Holding" = "khaki3"
#       )
#     ) +
#     labs(title = paste(team_label, "Bed Occupancy"), y = NULL, x = NULL) +
#     theme_minimal() +
#     theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# }
bed_plot_by_team <- function(team_label) {
  bed_subset <- subset(bed_use, team == team_label)
  roster_subset <- subset(ot_bed_roster, team == team_label)
  
  ggplot(bed_subset) +
    # Add shaded OT bed downtime
    geom_rect(data = roster_subset,
              aes(xmin = start_day, xmax = end_day, ymin = -Inf, ymax = Inf),
              fill = "gray85", alpha = 0.4) +
    
    # Bed occupancy segments
    geom_segment(
      aes(x = start_day, xend = end_day, y = resource_readable, yend = resource_readable, color = bed_type),
      linewidth = 4
    ) +
    scale_x_continuous(
      breaks = seq(0, ceiling(max(bed_use$end_day)), by = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(xlim = range(day_breaks)) +
    geom_vline(
      xintercept = day_breaks,
      linetype = "dotted",
      color = "gray20",
      linewidth = 0.4
    ) +
    scale_color_manual(
      name = "Bed Occupancy",
      values = c(
        "OT" = "firebrick",
        "ICU" = "darkorange",
        "Resus" = "goldenrod",
        "Holding" = "khaki3"
      )
    ) +
    labs(title = paste(team_label, "Bed Occupancy"), y = NULL, x = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}

# Function for resource utilization plot per team
util_plot_by_team <- function(team_label) {
  ggplot(subset(utilisation_data_r2b, team == team_label), aes(x = day)) +
    geom_bar(
      aes(x = day + 0.5, y = casualty_percent_of_total),
      stat = "identity", fill = "gray20", alpha = 0.3, width = 0.8
    ) +
    geom_bar(
      aes(x = day + 0.5, y = percent_seized, fill = role),
      stat = "identity", position = position_dodge(width = 0.7), width = 0.6
    ) +
    geom_vline(
      xintercept = day_breaks,
      linetype = "dotted",
      color = "gray20",
      linewidth = 0.4
    ) +
    coord_cartesian(xlim = range(day_breaks)) +
    scale_x_continuous(
      breaks = seq(0, ceiling(max(bed_use$end_day)), by = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    geom_hline(yintercept = c(50, 66), linetype = "dashed", color = c("orange", "red"), linewidth = 1) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 66, fill = "orange", alpha = 0.3) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 66, ymax = 100, fill = "red", alpha = 0.3) +
    scale_fill_manual(
      name = "Daily Resource Usage",
      values = c(
        "c_r2b_surg_surgeon_1" = "#33a02c",
        "c_r2b_emerg_facem_1" = "#fb9a99",
        "c_r2b_icu_nurse_1" = "#ff7f00",
        "c_r2b_evac_medic_1" = "#6a3d9a"
      ),
      labels = c(
        "c_r2b_surg_surgeon_1" = "Surgical Team",
        "c_r2b_emerg_facem_1" = "Emergency Team",
        "c_r2b_icu_nurse_1" = "ICU Team",
        "c_r2b_evac_medic_1" = "Evacuation Team"
      )
    ) +
    scale_y_continuous(
      name = "Utilization (%)",
      # sec.axis = sec_axis(~ ., name = "Daily Casualties (%)"),
      labels = scales::percent_format(scale = 1)
    ) +
    labs(title = paste(team_label, "Daily Resource Utilization"), x = "Day") +
    theme_minimal()
}

# Get all team labels dynamically
teams <- unique(utilisation_data_r2b$team)

# Combine bed + utilization vertically per team
combined_plots <- lapply(teams, function(team_label) {
  bed_plot_by_team(team_label) / util_plot_by_team(team_label)
})

# # Arrange them all on one page
final_plot <- wrap_plots(combined_plots, ncol = 1) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "R2B Bed Utilization and Team Workload by Day")

print(final_plot)
