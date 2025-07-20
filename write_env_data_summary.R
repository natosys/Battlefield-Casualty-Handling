library(jsonlite)
library(purrr)
library(stringr)

# Utility
`%||%` <- function(x, y) if (is.null(x)) y else x

# Load env_data.json
env_data <- fromJSON("env_data.json", simplifyVector = FALSE)

# Capitalization helpers
capitalize <- function(x) str_to_title(as.character(x))
toupper_safe <- function(x) toupper(as.character(x))
capitalize_bed <- function(name) {
  name <- tolower(name)
  if (name %in% c("icu", "ot")) return(toupper(name))
  capitalize(name)
}
capitalize_platform <- function(name) toupper(as.character(name))
capitalize_team <- function(name) {
  name <- tolower(name)
  if (name == "icu") return("ICU")
  if (name == "ot") return("OT")
  str_to_title(name)
}


# --- 1. Population Section ---
pop_section <- c(
  "## Environment Data Summary",
  "",
  "### 👥 Population Groups",
  "",
  "The following population groups are defined in the simulation environment:",
  "",
  "| Population | Count |",
  "|------------|-------|",
  paste0("| ", map_chr(env_data$pops, ~ capitalize(.x$name)), " | ", map_chr(env_data$pops, ~ .x$count), " |"),
  ""
)

# --- 2. Transport Assets Section ---
transport_section <- c(
  "### 🚑 Transport Resources",
  "",
  "These are the available transport platforms and their characteristics:",
  "",
  "| Platform | Quantity | Capacity |",
  "|----------|----------|----------|",
  paste0("| ", map_chr(env_data$transports, ~ capitalize_platform(.x$name)),
         " | ", map_chr(env_data$transports, ~ .x$qty),
         " | ", map_chr(env_data$transports, ~ .x$capacity), " |"),
  ""
)

# --- 3. ELMs Section ---
summarise_resources <- function(resources) {
  paste(
    map_chr(resources, ~ sprintf("%s (%s)", capitalize(.x$name %||% .x$resource), .x$qty)),
    collapse = ", "
  )
}

summarise_beds <- function(beds) {
  if (is.null(beds)) return(NA)
  paste(map_chr(beds, ~ sprintf("%s (%s)", capitalize_bed(.x$name), .x$qty)), collapse = "; ")
}

# All distinct team types, formatted
all_team_types <- unique(unlist(map(env_data$elms, function(elm) {
  map_chr(elm$sub_elms, ~ .x$sub_elm %||% "base")
})))
formatted_team_types <- map_chr(all_team_types, ~ capitalize(.x))

# Build ELM table rows
elm_rows <- map(env_data$elms, function(elm) {
  team_resources <- setNames(rep(NA, length(formatted_team_types)), formatted_team_types)
  for (sub in elm$sub_elms) {
    key <- capitalize(sub$sub_elm %||% "base")
    team_resources[[key]] <- summarise_resources(sub$resources)
  }
  c(
    list(
      Element = toupper_safe(elm$elm),
      Quantity = elm$qty,
      Beds = summarise_beds(elm$beds)
    ),
    team_resources
  )
})

elm_colnames <- c("Element", "Quantity", "Beds", formatted_team_types)

elm_section <- c(
  "### 🏥 Medical Resources",
  "",
  "The following table summarises the medical elements configured in `env_data.json`, including team types, personnel, and beds:",
  "",
  paste0("| ", paste(elm_colnames, collapse = " | "), " |"),
  paste0("| ", paste(rep("---", length(elm_colnames)), collapse = " | "), " |"),
  map_chr(elm_rows, function(row) {
    paste0("| ", paste(map_chr(elm_colnames, ~ row[[.x]] %||% ""), collapse = " | "), " |")
  }),
  ""
)

# --- Final Output to Markdown ---
readme_content <- c(pop_section, transport_section, elm_section)
writeLines(readme_content, "README_inputs.md")
cat("✅ Markdown input summary written to: README_inputs.md\n")