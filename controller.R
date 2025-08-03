library(shiny)
library(jsonlite)
library(shinyBS)

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

ui <- fillPage(
  padding = 10,
  titlePanel("Grouped env_data.json Editor"),  
  fileInput("upload_json", "📤 Load JSON File", accept = ".json"),
  actionButton("save_json", "💾 Save JSON"),
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
  env_data_reactive <- reactiveVal(initial_env_data)
  editable_fields <- c("val", "count", "capacity", "qty")
  
  observeEvent(input$upload_json, {
    req(input$upload_json)
    new_data <- fromJSON(input$upload_json$datapath, simplifyVector = FALSE)
    env_data_reactive(new_data)
    showNotification("New JSON loaded.", type = "message")
  })
  
  output$dynamic_inputs <- renderUI({
    generateInputs(env_data_reactive())
  })
  
  output$json_preview <- renderText({
    current_inputs <- reactiveValuesToList(input)
    updated_json <- updateJsonFromInputs(env_data_reactive(), current_inputs)
    toJSON(updated_json, pretty = TRUE, auto_unbox = TRUE)
  })
  
  observeEvent(input$save_json, {
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
      updated_json <- updateJsonFromInputs(env_data_reactive(), current_inputs)
      write_json(updated_json, default_path, pretty = TRUE, auto_unbox = TRUE)
      env_data_reactive(updated_json)
      showNotification("env_data.json saved.", type = "message")
    }
  })
}

shinyApp(ui, server)
