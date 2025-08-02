library(shiny)
library(jsonlite)
library(shinyBS)

getSafeVal <- function(x, default = 0) {
  if (is.null(x) || is.na(x)) default else x
}

generateInputs <- function(json, parent = "") {
  keys <- names(json)
  if (is.null(keys)) keys <- as.character(seq_along(json))
  
  ui_list <- list()
  readonly_fields <- c("elm", "sub_elm", "name", "resource", "type")
  
  for (key in keys) {
    val <- json[[key]]
    full_key <- if (parent == "") key else paste0(parent, "$", key)
    input_id <- gsub("\\$", "_", full_key)
    is_readonly <- key %in% readonly_fields
    label <- full_key
    
    if (is.list(val)) {
      if (length(val) > 0 && any(sapply(val, is.list))) {
        panels <- lapply(seq_along(val), function(i) {
          child <- val[[i]]
          title_text <- paste0(key, " [", i, "]")
          bsCollapsePanel(
            title = title_text,
            generateInputs(child, paste0(full_key, "$", i)),
            style = "info"
          )
        })
        ui_list[[length(ui_list) + 1]] <- do.call(bsCollapse, panels)
      } else {
        ui_list <- c(ui_list, generateInputs(val, full_key))
      }
    } else {
      input_ui <- switch(
        class(val)[1],
        numeric = if (is_readonly) {
          tags$div(style = "opacity: 0.6;", paste(label, ":", val))
        } else {
          numericInput(input_id, label, value = getSafeVal(val))
        },
        integer = if (is_readonly) {
          tags$div(style = "opacity: 0.6;", paste(label, ":", val))
        } else {
          numericInput(input_id, label, value = getSafeVal(val))
        },
        character = if (is_readonly) {
          tags$div(style = "opacity: 0.6;", paste(label, ":", val))
        } else {
          textInput(input_id, label, value = getSafeVal(val, ""))
        },
        logical = if (is_readonly) {
          tags$div(style = "opacity: 0.6;", paste(label, ": TRUE/FALSE"))
        } else {
          checkboxInput(input_id, label, value = getSafeVal(val, FALSE))
        },
        NULL
      )
      if (!is.null(input_ui)) {
        ui_list[[length(ui_list) + 1]] <- input_ui
      }
    }
  }
  
  tagList(ui_list)
}

updateJsonFromInputs <- function(json, input_list, parent = "") {
  keys <- names(json)
  if (is.null(keys)) keys <- as.character(seq_along(json))  # Handle arrays
  
  updated <- if (is.null(names(json))) vector("list", length(json)) else json
  
  for (i in seq_along(keys)) {
    key <- keys[i]
    val <- json[[i]]
    full_key <- if (parent == "") key else paste0(parent, "$", key)
    input_id <- gsub("\\$", "_", full_key)
    
    if (is.list(val)) {
      updated[[i]] <- updateJsonFromInputs(val, input_list, full_key)
    } else if (!is.null(input_list[[input_id]])) {
      original_val <- val
      new_val <- input_list[[input_id]]
      
      if (is.numeric(original_val)) {
        new_val <- as.numeric(new_val)
      } else if (is.integer(original_val)) {
        new_val <- as.integer(new_val)
      } else if (is.logical(original_val)) {
        new_val <- as.logical(new_val)
      } else {
        new_val <- as.character(new_val)
      }
      
      updated[[i]] <- new_val
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

ui <- fluidPage(
  titlePanel("Grouped env_data.json Editor"),
  
  # Upload & Buttons
  fileInput("upload_json", "📤 Load JSON File", accept = ".json"),
  actionButton("save_json", "💾 Save JSON"),
  tags$hr(),
  
  # Inputs stacked
  uiOutput("dynamic_inputs"),
  tags$hr(),
  
  # JSON Preview below
  h3("JSON Preview"),
  verbatimTextOutput("json_preview")
)


server <- function(input, output, session) {
  env_data_reactive <- reactiveVal(initial_env_data)
  
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
    updated_json <- updateJsonFromInputs(env_data_reactive(), current_inputs)
    write_json(updated_json, default_path, pretty = TRUE, auto_unbox = TRUE)
    env_data_reactive(updated_json)
    showNotification("env_data.json saved.", type = "message")
  })
}

shinyApp(ui, server)
