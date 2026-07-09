##############################################
## controller_legacy.R                      ##
## SUPERSEDED by app.R (Issue #14)          ##
##############################################
#
# This raw env_data.json editor predates the Configure/Run/Analyse Shiny
# console. It is retained only for reference — launch app.R instead
# (shiny::runApp("app.R")) for parameter editing, Quick Run execution, and
# result visualisation with plain-English labels and tooltips.

library(shiny)
library(jsonlite)
library(shinyBS)

source("R/scenario.R")

getSafeVal <- function(x, default = NULL) {
  if (is.null(x) || is.na(x)) return(default)
  x
}

generateInputs <- function(json, parent = "") {
  keys <- names(json)
  if (is.null(keys)) keys <- as.character(seq_along(json))
  if (length(json) == 0) return(list())
  
  ui_list <- list()
  editable_fields <- c("val", "count", "capacity", "qty")
  
  for (key in keys) {
    val <- json[[key]]
    full_key <- if (parent == "") key else paste0(parent, "$", key)
    input_id <- gsub("\\$", "_", full_key)
    is_editable <- key %in% editable_fields
    label <- full_key
    
    is_root <- !grepl("\\$", full_key)
    
    if (is.list(val)) {
      if (length(val) > 0 && any(sapply(val, is.list))) {
        panels <- lapply(seq_along(val), function(i) {
          child <- val[[i]]
          child_key <- paste0(full_key, "$", i)
          
          descriptor_keys <- c("elm", "sub_elm", "resource", "type", "name")
          panel_type <- NULL
          panel_label <- NULL
          
          for (k in descriptor_keys) {
            if (!is.null(child[[k]]) && length(child[[k]]) == 1) {
              panel_type <- k
              panel_label <- as.character(child[[k]])
              break
            }
          }
          
          title_text <- if (!is.null(panel_type) && !is.null(panel_label)) {
            paste0(panel_type, ": ", panel_label)
          } else {
            paste0(key, " [", i, "]")
          }
          
          bsCollapsePanel(
            title = title_text,
            generateInputs(child, child_key),
            style = "info"
          )
        })
        
        collapse <- do.call(bsCollapse, panels)
        
        if (is_root) {
          ui_list[[length(ui_list) + 1]] <- bsCollapsePanel(
            title = paste0("📁 ", key),
            collapse,
            style = "primary"
          )
        } else {
          ui_list[[length(ui_list) + 1]] <- collapse
        }
        
      } else {
        child_ui <- generateInputs(val, full_key)
        if (is_root) {
          ui_list[[length(ui_list) + 1]] <- bsCollapsePanel(
            title = paste0("📁 ", key),
            child_ui,
            style = "primary"
          )
        } else {
          ui_list <- c(ui_list, child_ui)
        }
      }
      
    } else {
      val_class <- as.character(class(val))[1]
      input_ui <- switch(
        val_class,
        numeric = if (!is_editable) {
          tags$div(style = "opacity: 0.6;", paste(label, ":", val))
        } else {
          numericInput(input_id, label, value = getSafeVal(val), min = 0)
        },
        integer = if (!is_editable) {
          tags$div(style = "opacity: 0.6;", paste(label, ":", val))
        } else {
          numericInput(input_id, label, value = getSafeVal(val), min = 0, step = 1)
        },
        character = if (!is_editable) {
          tags$div(style = "opacity: 0.6;", paste(label, ":", val))
        } else {
          textInput(input_id, label, value = getSafeVal(val, ""))
        },
        logical = if (!is_editable) {
          tags$div(style = "opacity: 0.6;", paste(label, ": TRUE/FALSE"))
        } else {
          checkboxInput(input_id, label, value = getSafeVal(val, FALSE))
        },
        NULL
      )
      
      if (!is.null(input_ui)) {
        if (is_root) {
          ui_list[[length(ui_list) + 1]] <- bsCollapsePanel(
            title = paste0("📁 ", key),
            input_ui,
            style = "primary"
          )
        } else {
          ui_list[[length(ui_list) + 1]] <- input_ui
        }
      }
    }
  }
  
  tagList(ui_list)
}

updateJsonFromInputs <- function(json, input_list, parent = "") {
  keys <- names(json)
  if (is.null(keys)) keys <- as.character(seq_along(json))
  
  updated <- if (is.null(names(json))) vector("list", length(json)) else json
  
  for (i in seq_along(keys)) {
    key <- keys[i]
    val <- json[[i]]
    full_key <- if (parent == "") key else paste0(parent, "$", key)
    input_id <- gsub("\\$", "_", full_key)
    
    if (is.list(val)) {
      updated[[i]] <- updateJsonFromInputs(val, input_list, full_key)
    } else if (input_id %in% names(input_list)) {
      new_val <- input_list[[input_id]]
      
      # Interpret empty string "" as untouched
      if (is.null(new_val) || (is.character(new_val) && new_val == "")) {
        updated[[i]] <- val  # Preserve original value
      } else {
        if (inherits(val, "numeric")) {
          new_val <- suppressWarnings(as.numeric(new_val))
        } else if (inherits(val, "integer")) {
          new_val <- suppressWarnings(as.integer(new_val))
        } else if (inherits(val, "logical")) {
          new_val <- as.logical(new_val)
        } else if (inherits(val, "character")) {
          new_val <- as.character(new_val)
        }
        updated[[i]] <- new_val
      }
    } else {
      updated[[i]] <- val
    }
  }
  
  names(updated) <- names(json)
  updated
}

# Load initial JSON
default_path <- "env_data.json"
initial_env_data <- fromJSON(default_path, simplifyVector = FALSE)
initial_scenario_choices <- c("default", names(initial_env_data$scenarios))

ui <- fillPage(
  padding = 10,
  titlePanel("Grouped env_data.json Editor"),
  fileInput("upload_json", "📤 Load JSON File", accept = ".json"),
  actionButton("save_json", "💾 Save JSON"),
  selectInput("scenario_select", "🌍 Active Scenario (preview)",
              choices = initial_scenario_choices, selected = "default"),
  tags$div(
    style = "opacity: 0.7; margin-top: -10px; margin-bottom: 10px;",
    "Previews the effective parameters for the selected scenario profile ",
    "(base env_data.json values overlaid with the scenario's overrides — see ",
    "the 'scenarios' block below). Saving is only enabled for 'default'; edit ",
    "a scenario's own override values directly in the 'scenarios' panel."
  ),
  fluidRow(
    column(6,
           tags$hr(),
           h4("Editable Inputs"),
           tags$div(
             style = "height: calc(100vh - 300px); overflow-y: auto; background-color: #f0f8ff; border: 1px solid #ccc; padding: 10px;",
             uiOutput("dynamic_inputs")
           )
    ),
    column(6,
           tags$hr(),
           h4("JSON Preview"),
           tags$div(
             style = "height: calc(100vh - 300px); overflow-y: auto; background-color: #f9f9f9; border: 1px solid #ccc; padding: 10px;",
             verbatimTextOutput("json_preview")
           )
    )
  )
)

server <- function(input, output, session) {
  # raw_env_data is the authoritative on-disk structure (base + scenarios
  # block); it is what gets written back to env_data.json on save. Selecting
  # a non-default scenario only changes the *preview* (display_env_data),
  # never raw_env_data, so scenario overlays can never be accidentally
  # flattened into the base file.
  raw_env_data <- reactiveVal(initial_env_data)
  editable_fields <- c("val", "count", "capacity", "qty")

  display_env_data <- reactive({
    if (identical(input$scenario_select, "default") || is.null(input$scenario_select)) {
      raw_env_data()
    } else {
      resolve_scenario(raw_env_data(), input$scenario_select)
    }
  })

  observeEvent(input$upload_json, {
    req(input$upload_json)
    new_data <- fromJSON(input$upload_json$datapath, simplifyVector = FALSE)
    raw_env_data(new_data)
    updateSelectInput(session, "scenario_select",
                       choices = c("default", names(new_data$scenarios)),
                       selected = "default")
    showNotification("New JSON loaded.", type = "message")
  })

  output$dynamic_inputs <- renderUI({
    generateInputs(display_env_data())
  })

  output$json_preview <- renderText({
    current_inputs <- reactiveValuesToList(input)
    updated_json <- updateJsonFromInputs(display_env_data(), current_inputs)
    toJSON(updated_json, pretty = TRUE, auto_unbox = TRUE)
  })

  observeEvent(input$save_json, {
    if (!identical(input$scenario_select, "default")) {
      showNotification(
        "Saving is disabled while previewing a scenario — switch to 'default' to save, or edit the scenario's own values in the 'scenarios' panel.",
        type = "error"
      )
      return(invisible(NULL))
    }

    current_inputs <- reactiveValuesToList(input)

    # 🔍 Validation block
    invalid_fields <- Filter(function(k) {
      field_name <- sub(".*_", "", k)
      field_name %in% editable_fields &&
        (!is.numeric(current_inputs[[k]]) || current_inputs[[k]] <= 0)
    }, names(current_inputs))

    if (length(invalid_fields) > 0) {
      showNotification(
        paste("Validation error in:", paste(invalid_fields, collapse = ", ")),
        type = "error"
      )
    } else {
      updated_json <- updateJsonFromInputs(raw_env_data(), current_inputs)
      write_json(updated_json, default_path, pretty = TRUE, auto_unbox = TRUE)
      raw_env_data(updated_json)
      showNotification("env_data.json saved.", type = "message")
    }
  })
}

shinyApp(ui, server)
