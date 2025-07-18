library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)


# Extract and reshape attributes
flow_data <- get_mon_attributes(env) %>%
  as.data.frame() %>%
  filter(key %in% c("r1_treated", "r2b_treated", "r2e_treated", "return_day", "mortuary_treated", "r2e_evac")) %>%
  pivot_wider(names_from = key, values_from = value) %>%
  mutate(across(everything(), as.character))

flow_data <- flow_data %>% dplyr::select(-time, -replication)

flow_data <- flow_data %>%
  group_by(name) %>%
  summarise(across(everything(), ~ paste(unique(na.omit(.)), collapse = "; ")), .groups = "drop")

flow_data$return_day <- ifelse(flow_data$return_day != "" & !is.na(flow_data$return_day), "1", flow_data$return_day)

flow_data <- flow_data %>%
  rename(return_to_force = return_day)

# Define the ordered stages
stage_cols <- c("r1_treated", "r2b_treated", "r2e_treated", "return_to_force", "mortuary_treated", "r2e_evac")

# Reshape to long format
long_flow <- flow_data %>%
  dplyr::select(name, all_of(stage_cols)) %>%
  pivot_longer(cols = all_of(stage_cols), names_to = "stage", values_to = "unit") %>%
  filter(unit != "" & !is.na(unit)) %>%
  mutate(node = paste(stage, unit))

# Group by casualty and create ordered transitions
flow_pairs <- long_flow %>%
  group_by(name) %>%
  arrange(name, match(stage, stage_cols)) %>%
  mutate(source = node,
         target = lead(node)) %>%
  filter(!is.na(target)) %>%
  ungroup()

flow_counts <- flow_pairs %>%
  count(source, target, name = "value")

# Create a unique list of all nodes
nodes <- data.frame(name = unique(c(flow_counts$source, flow_counts$target)))

# Remove "_treated"
nodes$name <- gsub("_treated", "", nodes$name)

# Replace "return_to_force" with "Return to Force"
nodes$name <- gsub("return_to_force", "Return to Force", nodes$name)

# Remove trailing " 1" from Return to Force and Mortuary
nodes$name <- gsub("Return to Force 1", "Return to Force", nodes$name)
nodes$name <- gsub("Mortuary 1", "Mortuary", nodes$name)

# Map source and target labels to indices
flow_counts$source_id <- match(flow_counts$source, nodes$name) - 1
flow_counts$target_id <- match(flow_counts$target, nodes$name) - 1

# Clean up source and target labels
flow_counts$source <- gsub("_treated", "", flow_counts$source)
flow_counts$target <- gsub("_treated", "", flow_counts$target)

flow_counts$source <- gsub("return_to_force", "Return to Force", flow_counts$source)
flow_counts$target <- gsub("return_to_force", "Return to Force", flow_counts$target)

flow_counts$source <- gsub("Mortuary 1", "Mortuary", flow_counts$source)
flow_counts$target <- gsub("Mortuary 1", "Mortuary", flow_counts$target)

flow_counts$source <- gsub("r2e_evac 1", "Evacuation", flow_counts$source)
flow_counts$target <- gsub("r2e_evac 1", "Evacuation", flow_counts$target)

flow_counts$source <- gsub("Return to Force 1", "Return to Force", flow_counts$source)
flow_counts$target <- gsub("Return to Force 1", "Return to Force", flow_counts$target)

# Create the Sankey diagram
fig <- plotly::plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = nodes$name,
    pad = 15,
    thickness = 20,
    line = list(color = "black", width = 0.5)
  ),
  link = list(
    source = flow_counts$source_id,
    target = flow_counts$target_id,
    value  = flow_counts$value
  )
)

# Add layout using base R syntax
fig <- plotly::layout(fig, title = "Casualty Flow Sankey Diagram", font = list(size = 12))

# Display the plot
print(fig)

# Extract node type by removing trailing numbers
extract_type <- function(x) sub(" [0-9]+$", "", x)

flow_counts_simple <- flow_counts
flow_counts_simple$source_type <- extract_type(flow_counts$source)
flow_counts_simple$target_type <- extract_type(flow_counts$target)

flow_counts_agg <- aggregate(value ~ source_type + target_type, data = flow_counts_simple, sum)

node_labels <- unique(c(flow_counts_agg$source_type, flow_counts_agg$target_type))
nodes_simple <- data.frame(name = node_labels)

flow_counts_agg$source_id <- match(flow_counts_agg$source_type, nodes_simple$name) - 1
flow_counts_agg$target_id <- match(flow_counts_agg$target_type, nodes_simple$name) - 1

fig <- plotly::plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = nodes_simple$name,
    pad = 15,
    thickness = 20,
    line = list(color = "black", width = 0.5)
  ),
  link = list(
    source = flow_counts_agg$source_id,
    target = flow_counts_agg$target_id,
    value  = flow_counts_agg$value
  )
)

fig <- plotly::layout(fig, title = list(text = "Simplified Casualty Flow by Node Type"), font = list(size = 12))
print(fig)
