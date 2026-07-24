library(jsonlite)
library(purrr)
library(stringr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# A non-character sub_elm (e.g. R1's placeholder integer, which has no named
# sub-team split in the trajectory logic) denotes the same "no sub-team"
# case as a missing sub_elm, so both resolve to the same "base" label.
resolve_sub_elm <- function(x) if (is.null(x) || !is.character(x)) "base" else x

# === Capitalization Helpers ===
capitalize <- function(x) str_to_title(as.character(x))
toupper_safe <- function(x) toupper(as.character(x))
capitalize_bed <- function(name) {
  name <- tolower(name)
  if (name %in% c("icu", "ot")) return(toupper(name))
  capitalize(name)
}
capitalize_platform <- function(name) toupper(as.character(name))

# Looks up a single `vars.<elm>.<acty>.<var>` leaf value (the same JSON path
# convention var_field() uses in R/app_params.R) from a parsed env_data.json
# tree, returning NA if the elm/acty/var isn't present.
get_var_value <- function(env_data, elm_name, acty_name, var_name) {
  elm_entry <- keep(env_data$vars, ~ .x$elm == elm_name)
  if (length(elm_entry) == 0) return(NA)
  acty_entry <- keep(elm_entry[[1]]$actys, ~ .x$acty == acty_name)
  if (length(acty_entry) == 0) return(NA)
  val_entry <- keep(acty_entry[[1]]$vals, ~ .x$var == var_name)
  if (length(val_entry) == 0) return(NA)
  val_entry[[1]]$val
}

# === Section Generator ===
generate_env_summary_section <- function(env_data) {
  # Field labels match R/app_params.R's GRP_FORCE / "Reinforcement Demand &
  # Fulfillment" subgroup exactly, so this table and the Configure panel
  # read as the same parameter set under the same names.
  # Symbol column matches the triangular distribution formula below: a/b/c
  # are the PDF's own lower-limit/upper-limit/mode variables (not tied to
  # this project's other symbol conventions elsewhere in the README).
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
    "Each reinforcement cycle computes a pool's demand as its shortfall against initial establishment strength, net of any shortfall an earlier, still-pending cycle has already claimed (`initial − current − pending`, floored at 0) — this prevents overlapping cycles from independently re-claiming the same shortfall when the demand submission cycle is shorter than the fulfillment lag. The amount actually delivered is drawn, at submission time rather than at fulfillment, as a fraction of that demand from a Triangular(*a*, *b*, *c*) distribution parameterised by the three fill values above, then credited to the pool once the fulfillment lag elapses, clamped so a pool can never be credited above its initial establishment strength. The model has no sortie-failure rate or binary success/failure roll for reinforcement.",
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
  
  summarise_resources <- function(resources) {
    paste(map_chr(resources, ~ sprintf("%s (%s)", capitalize(.x$name %||% .x$resource), as.character(.x$qty))), collapse = ", ")
  }
  
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
    ""
  )
  
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