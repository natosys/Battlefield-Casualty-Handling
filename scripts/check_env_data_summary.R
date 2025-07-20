# library(jsonlite)
# library(purrr)
# library(stringr)
# 
# # Utility
# `%||%` <- function(x, y) if (is.null(x)) y else x
# 
# # Load env_data.json
# env_data <- fromJSON("env_data.json", simplifyVector = FALSE)
# 
# # Capitalization helpers
# capitalize <- function(x) str_to_title(as.character(x))
# toupper_safe <- function(x) toupper(as.character(x))
# capitalize_bed <- function(name) {
#   name <- tolower(name)
#   if (name %in% c("icu", "ot")) return(toupper(name))
#   capitalize(name)
# }
# capitalize_platform <- function(name) toupper(as.character(name))
# capitalize_team <- function(name) {
#   name <- tolower(name)
#   if (name == "icu") return("ICU")
#   if (name == "ot") return("OT")
#   str_to_title(name)
# }
# 
# 
# # --- 1. Population Section ---
# pop_section <- c(
#   "## Environment Data Summary",
#   "",
#   "### 👥 Population Groups",
#   "",
#   "The following population groups are defined in the simulation environment:",
#   "",
#   "| Population | Count |",
#   "|------------|-------|",
#   paste0("| ", map_chr(env_data$pops, ~ capitalize(.x$name)), " | ", map_chr(env_data$pops, ~ .x$count), " |"),
#   ""
# )
# 
# # --- 2. Transport Assets Section ---
# transport_section <- c(
#   "### 🚑 Transport Resources",
#   "",
#   "These are the available transport platforms and their characteristics:",
#   "",
#   "| Platform | Quantity | Capacity |",
#   "|----------|----------|----------|",
#   paste0("| ", map_chr(env_data$transports, ~ capitalize_platform(.x$name)),
#          " | ", map_chr(env_data$transports, ~ .x$qty),
#          " | ", map_chr(env_data$transports, ~ .x$capacity), " |"),
#   ""
# )
# 
# # --- 3. ELMs Section ---
# summarise_resources <- function(resources) {
#   paste(
#     map_chr(resources, ~ sprintf("%s (%s)", capitalize(.x$name %||% .x$resource), .x$qty)),
#     collapse = ", "
#   )
# }
# 
# summarise_beds <- function(beds) {
#   if (is.null(beds)) return(NA)
#   paste(map_chr(beds, ~ sprintf("%s (%s)", capitalize_bed(.x$name), .x$qty)), collapse = "; ")
# }
# 
# # All distinct team types, formatted
# all_team_types <- unique(unlist(map(env_data$elms, function(elm) {
#   map_chr(elm$sub_elms, ~ .x$sub_elm %||% "base")
# })))
# formatted_team_types <- map_chr(all_team_types, ~ capitalize(.x))
# 
# # Build ELM table rows
# elm_rows <- map(env_data$elms, function(elm) {
#   team_resources <- setNames(rep(NA, length(formatted_team_types)), formatted_team_types)
#   for (sub in elm$sub_elms) {
#     key <- capitalize(sub$sub_elm %||% "base")
#     team_resources[[key]] <- summarise_resources(sub$resources)
#   }
#   c(
#     list(
#       Element = toupper_safe(elm$elm),
#       Quantity = elm$qty,
#       Beds = summarise_beds(elm$beds)
#     ),
#     team_resources
#   )
# })
# 
# elm_colnames <- c("Element", "Quantity", "Beds", formatted_team_types)
# 
# elm_section <- c(
#   "### 🏥 Medical Resources",
#   "",
#   "The following table summarises the medical elements configured in `env_data.json`, including team types, personnel, and beds:",
#   "",
#   paste0("| ", paste(elm_colnames, collapse = " | "), " |"),
#   paste0("| ", paste(rep("---", length(elm_colnames)), collapse = " | "), " |"),
#   map_chr(elm_rows, function(row) {
#     paste0("| ", paste(map_chr(elm_colnames, ~ row[[.x]] %||% ""), collapse = " | "), " |")
#   }),
#   ""
# )
# 
# # --- Final Output to Markdown ---
# readme_content <- c(pop_section, transport_section, elm_section)
# writeLines(readme_content, "README_inputs.md")
# cat("✅ Markdown input summary written to: README_inputs.md\n")

library(jsonlite)
library(purrr)
library(stringr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# === Capitalization Helpers ===
capitalize <- function(x) str_to_title(as.character(x))
toupper_safe <- function(x) toupper(as.character(x))
capitalize_bed <- function(name) {
  name <- tolower(name)
  if (name %in% c("icu", "ot")) return(toupper(name))
  capitalize(name)
}
capitalize_platform <- function(name) toupper(as.character(name))

# === Section Generator ===
generate_env_summary_section <- function(env_data) {
  pop_section <- c(
    "### 👥 Population Groups",
    "",
    "The following population groups are defined in the simulation environment:",
    "",
    "| Population | Count |",
    "|------------|-------|",
    paste0("| ", map_chr(env_data$pops, ~ capitalize(.x$name)), " | ", map_chr(env_data$pops, ~ as.character(.x$count)), " |"),
    ""
  )
  
  transport_section <- c(
    "### 🚑 Transport Resources",
    "",
    "These are the available transport platforms and their characteristics:",
    "",
    "| Platform | Quantity | Capacity |",
    "|----------|----------|----------|",
    paste0("| ", map_chr(env_data$transports, ~ capitalize_platform(.x$name)),
           " | ", map_chr(env_data$transports, ~ as.character(.x$qty)),
           " | ", map_chr(env_data$transports, ~ as.character(.x$capacity)), " |"),
    ""
  )
  
  summarise_resources <- function(resources) {
    paste(map_chr(resources, ~ sprintf("%s (%s)", capitalize(.x$name %||% .x$resource), as.character(.x$qty))), collapse = ", ")
  }
  
  summarise_beds <- function(beds) {
    if (is.null(beds)) return(NA)
    paste(map_chr(beds, ~ sprintf("%s (%s)", capitalize_bed(.x$name), as.character(.x$qty))), collapse = "; ")
  }
  
  all_team_types <- unique(unlist(map(env_data$elms, function(elm) {
    map_chr(elm$sub_elms, ~ .x$sub_elm %||% "base")
  })))
  formatted_team_types <- map_chr(all_team_types, ~ capitalize(.x))
  
  elm_rows <- map(env_data$elms, function(elm) {
    team_resources <- setNames(rep(NA, length(formatted_team_types)), formatted_team_types)
    for (sub in elm$sub_elms) {
      key <- capitalize(sub$sub_elm %||% "base")
      team_resources[[key]] <- summarise_resources(sub$resources)
    }
    c(
      list(
        Element = toupper_safe(elm$elm),
        Quantity = as.character(elm$qty),
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
      paste0("| ", paste(map_chr(elm_colnames, ~ as.character(row[[.x]] %||% "")), collapse = " | "), " |")
    }),
    ""
  )
  
  c(
    "<!-- ENV SUMMARY START -->",
    "<!-- This section is auto-generated. Do not edit manually. -->",
    "",
    "## 📊 Environment Data Summary",
    "",
    pop_section,
    transport_section,
    elm_section,
    "<!-- ENV SUMMARY END -->"
  )
}

# === Execution Logic ===
env_data <- fromJSON("env_data.json", simplifyVector = FALSE)
expected_block <- generate_env_summary_section(env_data)

readme <- readLines("README.md")
start_line <- grep("<!-- ENV SUMMARY START -->", readme)
end_line   <- grep("<!-- ENV SUMMARY END -->", readme)

if (length(start_line) == 1 && length(end_line) == 1 && start_line < end_line) {
  existing_block <- readme[start_line:end_line]
  
  if (!identical(trimws(existing_block), trimws(expected_block))) {
    updated_readme <- c(
      readme[1:(start_line - 1)],
      expected_block,
      readme[(end_line + 1):length(readme)]
    )
    writeLines(updated_readme, "README.md")
    cat("✅ Environment summary block updated in README.md\n")
    
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_entry <- sprintf("[%s] 📊 ENV SUMMARY block replaced in README.md\n", timestamp)
    dir.create("logs", showWarnings = FALSE)
    write(log_entry, file = "log.log", append = TRUE)
  } else {
    cat("✓ Environment summary block is up to date.\n")
  }
} else {
  cat("⚠️ ENV SUMMARY START/END markers not found or malformed.\n")
  quit(status = 1)
}