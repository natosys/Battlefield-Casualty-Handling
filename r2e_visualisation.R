library(ggplot2)
library(patchwork)

# Setup
day_min <- 1440  # 1 day = 1440 minutes

# Get R2E bed log
r2e_bed_log <- get_mon_resources(env)
r2e_bed_log <- r2e_bed_log[grepl("^b_r2e", r2e_bed_log$resource), ]
r2e_bed_log <- r2e_bed_log[order(r2e_bed_log$resource, r2e_bed_log$time), ]
r2e_bed_log$time_diff <- ave(r2e_bed_log$time, r2e_bed_log$resource, FUN = function(x) c(diff(x), 0))
r2e_bed_log$start <- r2e_bed_log$time
r2e_bed_log$end <- r2e_bed_log$time + r2e_bed_log$time_diff
r2e_bed_use <- subset(r2e_bed_log, server > 0 & time_diff > 0)

# Extract team id
r2e_bed_use$team_id <- as.integer(sub(".*_t(\\d+)$", "\\1", r2e_bed_use$resource))

# Extract bed type and standardize
r2e_bed_use$bed_type_raw <- sub("^b_r2e(\\w+)_t\\d+$", "\\1", r2e_bed_use$resource)

r2e_bed_use$bed_type_code <- sub("^.*_(resus|icu|ot|hold)_.*$", "\\1", r2e_bed_use$bed_type_raw)

# Human-readable bed type names
r2e_bed_use$bed_type <- dplyr::recode(r2e_bed_use$bed_type_code,
                               "resus" = "Resuscitation Bed",
                               "icu" = "Intensive Care Unit Bed",
                               "ot" = "Operating Theatre Bed",
                               "hold" = "Holding Bed",
                               .default = "Other"
)

# Add time in days
r2e_bed_use$start_day <- r2e_bed_use$start / day_min
r2e_bed_use$end_day <- r2e_bed_use$end / day_min

# Build resource label
r2e_bed_use$resource_label <- paste0(r2e_bed_use$bed_type, " (", r2e_bed_use$resource, ")")

# Define custom color palette for readability
bed_colors <- c(
  "Resuscitation Bed" = "#1f78b4",
  "Intensive Care Unit Bed" = "#e31a1c",
  "Operating Theatre Bed" = "#33a02c",
  "Holding Bed" = "#ff7f00"
)

# Ensure correct factor levels
r2e_bed_use$bed_type <- factor(r2e_bed_use$bed_type, levels = names(bed_colors))
r2e_bed_use$resource_label <- factor(r2e_bed_use$resource_label,
                                     levels = unique(r2e_bed_use[order(r2e_bed_use$bed_type, r2e_bed_use$resource_label), "resource_label"]))

# Clean resource names (e.g., hold_1 → Hold 01)
r2e_bed_use$resource_readable <- sub("^.*_(resus|icu|ot|hold)_", "\\1_", r2e_bed_use$resource)
r2e_bed_use$resource_readable <- sub("_t\\d+$", "", r2e_bed_use$resource_readable)
r2e_bed_use$resource_readable <- gsub("_", " ", r2e_bed_use$resource_readable)
r2e_bed_use$resource_readable <- tools::toTitleCase(r2e_bed_use$resource_readable)

# Add leading zero to single-digit bed numbers
r2e_bed_use$resource_readable <- gsub("(\\D)(\\d)(\\b)", "\\10\\2", r2e_bed_use$resource_readable)


# Generate and stack plots
team_ids <- sort(unique(r2e_bed_use$team_id))
team_plots <- list()

for (team in team_ids) {
  team_data <- r2e_bed_use[r2e_bed_use$team_id == team, ]
  
  p <- ggplot(team_data,
              aes(x = start_day, xend = end_day,
                  y = resource_readable, yend = resource_readable,
                  color = bed_type)) +
    geom_segment(size = 4, alpha = 0.9) +
    labs(
      title = paste("R2E Heavy Bed Usage - Team", team),
      x = "Time (Days)",
      y = "Bed",
      color = "Bed Type"
    ) +
    scale_color_manual(values = bed_colors) +
    scale_x_continuous(
      breaks = seq(0, ceiling(max(r2e_bed_use$end_day)), by = 1),
      expand = c(0, 0)
    ) +
    guides(color = "none") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  team_plots[[team]] <- p
}

# Stack vertically
final_plot <- wrap_plots(team_plots, ncol = 1)
print(final_plot)
