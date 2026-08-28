##############################################
## scripts/check_env_data_summary.R         ##
## README environment summary regeneration  ##
##############################################

library(jsonlite)
library(purrr)
library(stringr)

#' NULL-coalesce
#'
#' @param x Value to return when it is not NULL.
#' @param y Fallback value.
#' @return `x` where it is not NULL, `y` otherwise.
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Resolve a sub-element name, defaulting the absent case
#'
#' @param x The `sub_elm` field as the file carries it.
#' @return The name, or "base" where the element has no sub-team split.
#' @details A non-character sub_elm (R1's placeholder integer, which has no
#'   named sub-team split in the trajectory logic) denotes the same case as a
#'   missing one, so both resolve to the same label.
resolve_sub_elm <- function(x) if (is.null(x) || !is.character(x)) "base" else x

# === Capitalization Helpers ===
#' Title-case a value for a summary table cell
#'
#' @param x The value to format.
#' @return The value as a title-cased string.
capitalize <- function(x) str_to_title(as.character(x))

#' Upper-case a value for a summary table cell
#'
#' @param x The value to format.
#' @return The value as an upper-cased string.
toupper_safe <- function(x) toupper(as.character(x))

#' Format a bed type's name for a summary table cell
#'
#' @param name The bed type's name.
#' @return The name title-cased, or upper-cased where it is an initialism.
capitalize_bed <- function(name) {
  name <- tolower(name)
  if (name %in% c("icu", "ot")) return(toupper(name))
  capitalize(name)
}
#' Format a transport platform's name for a summary table cell
#'
#' @param name The platform's name.
#' @return The name upper-cased.
capitalize_platform <- function(name) toupper(as.character(name))

#' Read one `vars.<elm>.<acty>.<var>` leaf value
#'
#' @param env_data Parsed `env_data.json`.
#' @param elm_name Element name to search under.
#' @param acty_name Activity name to search under.
#' @param var_name Variable name to read.
#' @return The leaf's value, NA where the path is not present.
#' @details The path convention is the one `var_field()` uses in
#'   `R/app_params.R`, so a field named there addresses the same leaf here.
get_var_value <- function(env_data, elm_name, acty_name, var_name) {
  elm_entry <- keep(env_data$vars, ~ .x$elm == elm_name)
  if (length(elm_entry) == 0) return(NA)
  acty_entry <- keep(elm_entry[[1]]$actys, ~ .x$acty == acty_name)
  if (length(acty_entry) == 0) return(NA)
  val_entry <- keep(acty_entry[[1]]$vals, ~ .x$var == var_name)
  if (length(val_entry) == 0) return(NA)
  val_entry[[1]]$val
}

#' The population and reinforcement rows of the environment summary
#'
#' @param env_data Parsed `env_data.json`.
#' @return A character vector of markdown lines for this section.
env_summary_population_section <- function(env_data) {
  reinforcement_params <- list(
    list("Demand Submission Cycle (days)", "—", get_var_value(env_data, "force_regeneration", "reinforcement", "demand_interval_days")),
    list("Fulfillment Lag (days)", "—", get_var_value(env_data, "force_regeneration", "reinforcement", "fulfillment_lag_days")),
    list("Fill Distribution — Minimum (fraction of demand)", "a", get_var_value(env_data, "force_regeneration", "reinforcement", "fill_min_frac")),
    list("Fill Distribution — Mode (fraction of demand)", "c", get_var_value(env_data, "force_regeneration", "reinforcement", "fill_mode_frac")),
    list("Fill Distribution — Maximum (fraction of demand)", "b", get_var_value(env_data, "force_regeneration", "reinforcement", "fill_max_frac"))
  )

  pop_section <- c(
    "### Force Size",
    "",
    "#### Population",
    "",
    "The following population groups are defined in the simulation environment:",
    "",
    "| Population | Count |",
    "|------------|-------|",
    paste0("| ", map_chr(env_data$pops, ~ capitalize(.x$name)), " | ", map_chr(env_data$pops, ~ as.character(.x$count)), " |"),
    "",
    "#### Reinforcement Demand & Fulfillment",
    "",
    "A demand submission cycle of 0 days disables reinforcement (the shipped default); the fulfillment lag and fill distribution parameters are then unused.",
    "",
    "| Parameter | Variable | Value |",
    "|-----------|----------|-------|",
    map_chr(reinforcement_params, ~ paste0("| ", .x[[1]], " | ", .x[[2]], " | ", as.character(.x[[3]]), " |")),
    "",
    "Each reinforcement cycle computes a pool's demand as its shortfall against initial establishment strength, net of any shortfall an earlier, still-pending cycle has already claimed (`initial − current − pending`, floored at 0) — this prevents overlapping cycles from independently re-claiming the same shortfall when the demand submission cycle is shorter than the fulfillment lag. The amount actually delivered is drawn, at submission time rather than at fulfillment, as a fraction of that demand from a Triangular(*a*, *b*, *c*) distribution parameterised by the three fill values above, then credited to the pool in full once the fulfillment lag elapses. Reinforcement joins the population on arrival, so a fill fraction above 1 delivers more than the shortfall it was requested against and carries the pool over establishment strength until casualties bring it back down. The model has no sortie-failure rate or binary success/failure roll for reinforcement.",
    "",
    "The fraction of demand *x* actually delivered in a single cycle is drawn from the following probability density function:",
    "",
    "$$",
    "f(x) =",
    "\\begin{cases}",
    "\\dfrac{2(x-a)}{(b-a)(c-a)} & a \\le x < c \\\\[4pt]",
    "\\dfrac{2}{b-a} & x = c \\\\[4pt]",
    "\\dfrac{2(b-x)}{(b-a)(b-c)} & c < x \\le b",
    "\\end{cases}",
    "$$",
    "",
    "Where *a*, *b*, and *c* are the Fill Distribution Minimum, Maximum, and Mode values in the table above respectively.",
    ""
  )

  pop_section
}

#' The transport fleet rows of the environment summary
#'
#' @param env_data Parsed `env_data.json`.
#' @return A character vector of markdown lines for this section.
env_summary_transport_section <- function(env_data) {
  transport_section <- c(
    "### Medevac — Transport Fleet",
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
  
  transport_section
}

#' The deployed element rows of the environment summary, and the shift roster
#'
#' @param env_data Parsed `env_data.json`.
#' @return A character vector of markdown lines for this section.
env_summary_element_section <- function(env_data) {
  #' Format one element's resource counts for a table cell
  #'
  #' @param resources List of the element's resource entries.
  #' @return A comma-separated string of name and quantity pairs.
  summarise_resources <- function(resources) {
    paste(map_chr(resources, ~ sprintf("%s (%s)", capitalize(.x$name %||% .x$resource), as.character(.x$qty))), collapse = ", ")
  }
  
  #' Format one element's bed counts for a table cell
  #'
  #' @param beds List of the element's bed entries, or NULL.
  #' @return A semicolon-separated string of name and quantity pairs, NA
  #'   where the element carries no beds.
  summarise_beds <- function(beds) {
    if (is.null(beds)) return(NA)
    paste(map_chr(beds, ~ sprintf("%s (%s)", capitalize_bed(.x$name), as.character(.x$qty))), collapse = "; ")
  }
  
  all_team_types <- unique(unlist(map(env_data$elms, function(elm) {
    map_chr(elm$sub_elms, ~ resolve_sub_elm(.x$sub_elm))
  })))
  formatted_team_types <- map_chr(all_team_types, ~ capitalize(.x))
  
  elm_rows <- map(env_data$elms, function(elm) {
    team_resources <- setNames(rep(NA, length(formatted_team_types)), formatted_team_types)
    for (sub in elm$sub_elms) {
      key <- capitalize(resolve_sub_elm(sub$sub_elm))
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
    "### Health System Architecture",
    "",
    "The following table summarises the medical elements configured in `env_data.json`, including team types, personnel, and beds. `Quantity` (team counts) and `Beds` (bed counts per team) are editable directly in `env_data.json` or via the Shiny Configure panel's Health System Architecture group (`app.R`); the personnel/team composition columns (`Base`/`Surg`/`Emerg`/`Icu`/`Evac`) are a fixed part of the establishment definition and are not independently configurable.",
    "",
    paste0("| ", paste(elm_colnames, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(elm_colnames)), collapse = " | "), " |"),
    map_chr(elm_rows, function(row) {
      paste0("| ", paste(map_chr(elm_colnames, ~ as.character(row[[.x]] %||% "")), collapse = " | "), " |")
    }),
    "",
    "#### Surgical Shift Roster",
    "",
    "One shift length rosters every surgical section in the theatre, at R2B and R2E alike. It sets the first shift's length; the second covers the remainder of the 24-hour day. See [Schedules and Rosters](#schedules-and-rosters) for how the sections alternate across the two shifts.",
    "",
    "| Parameter | Value |",
    "|-----------|-------|",
    paste0("| OT Shift Length (hours per shift) | ",
           as.character(get_var_value(env_data, "surgical_roster", "shift", "ot_hours")), " |"),
    ""
  )
  elm_section
}

# === Section Generator ===

#' Build the whole environment summary block
#'
#' @param env_data Parsed `env_data.json`.
#' @return A character vector of the markdown lines the README's
#'   `<!-- ENV SUMMARY -->` block holds.
generate_env_summary_section <- function(env_data) {
  # Field labels match R/app_params.R's GRP_FORCE / "Reinforcement Demand &
  # Fulfillment" subgroup exactly, so this table and the Configure panel
  # read as the same parameter set under the same names.
  # Symbol column matches the triangular distribution formula below: a/b/c
  # are the PDF's own lower-limit/upper-limit/mode variables (not tied to
  # this project's other symbol conventions elsewhere in the README).
  pop_section <- env_summary_population_section(env_data)
  transport_section <- env_summary_transport_section(env_data)
  elm_section <- env_summary_element_section(env_data)
  
  c(
    "<!-- ENV SUMMARY START -->",
    "<!-- This section is auto-generated. Do not edit manually. -->",
    "",
    pop_section,
    elm_section,
    transport_section,
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