# Loads the Shiny console into the test session.
#
# app.R sources every R/ module and then evaluates shinyApp(ui, server) as its
# final expression. source() does not print that value and shinyApp() does not
# start a server, so sourcing the file here defines every helper, `ui` and
# `server` without launching anything.
#
# Two globals are rebound afterwards. app.R holds APP_DIR and DEFAULT_JSON as
# paths resolved against the working directory it was loaded from, and
# testthat restores tests/testthat as the working directory for each test
# file it runs; both are therefore made absolute so the server function's own
# startup read of the configuration finds it wherever the runner was invoked.
# The future plan is set back to sequential because app.R starts multisession
# workers that nothing in this suite uses.

APP_ROOT <- normalizePath(file.path(testthat::test_path(), "..", ".."))

local({
  old_wd <- setwd(APP_ROOT)
  on.exit(setwd(old_wd), add = TRUE)
  suppressMessages(source(file.path(APP_ROOT, "app.R"), local = FALSE))
})

assign("APP_DIR",      APP_ROOT,                             envir = globalenv())
assign("DEFAULT_JSON", file.path(APP_ROOT, "env_data.json"), envir = globalenv())
future::plan(future::sequential)

#' Read the shipped configuration as the app reads it at startup
#'
#' @return The parsed `env_data.json` as a nested list, unsimplified, exactly
#'   as app.R's `startup_json` holds it.
test_startup_json <- function() {
  jsonlite::fromJSON(DEFAULT_JSON, simplifyVector = FALSE)
}

#' Write a configuration to a temporary file and describe it as an upload
#'
#' @param json Configuration to write, or a single string to write verbatim
#'   when `raw` is TRUE.
#' @param name File name to report as the upload's original name.
#' @param raw TRUE to write `json` as literal file content rather than
#'   serialising it, so a malformed file can be offered to the app.
#' @return A one-row data frame in the shape Shiny's `fileInput` produces,
#'   suitable for `session$setInputs(upload_json = ...)`.
test_upload <- function(json, name = "env_data.json", raw = FALSE) {
  path <- tempfile(fileext = ".json")
  if (raw) {
    writeLines(json, path)
  } else {
    jsonlite::write_json(json, path, auto_unbox = TRUE)
  }
  data.frame(name = name, size = file.size(path), type = "application/json",
             datapath = path, stringsAsFactors = FALSE)
}
