#!/usr/bin/env Rscript
##################################################################
## scripts/check_scenario_labels.R                              ##
## Regression check: comparative scenario plotting is           ##
## independent of the session's character locale                ##
##################################################################
#
# Terminal / Claude Code cloud:
#   Rscript scripts/check_scenario_labels.R
#
# Exits 0 when every assertion holds, 1 otherwise.
#
# The scenario labels in env_data.json contain an em dash, and jsonlite flags
# the strings it parses as UTF-8 whatever locale the session runs in. A C
# locale cannot represent that character natively, so any handling of these
# strings that goes through the locale is a latent failure: matching a
# non-ASCII pattern against them raises "'pattern' is invalid" outright, and a
# non-ASCII string literal reaches the PNG device as raw bytes and is drawn as
# such. Neither is visible to a developer working in a UTF-8 shell, and the
# comparative runner's plotting stage is reached only after a full multi-hour
# replication run, so the failure surfaces at the most expensive moment.
#
# This check reaches the same code path in seconds, by rendering the
# comparison plot from a synthetic queue table rather than from a simulation.

suppressPackageStartupMessages(source("R/scenario_runner.R"))

failures <- character(0)

#' Print one PASS or FAIL line, recording a failure
#'
#' @param condition Logical: whether the assertion held.
#' @param description One-line description of the assertion.
#' @return Invisible NULL; called for its side effects.
check <- function(condition, description) {
  if (isTRUE(condition)) {
    message("  PASS  ", description)
  } else {
    message("  FAIL  ", description)
    failures <<- c(failures, description)
  }
}

#' Minimal queue table carrying the same columns compare_scenarios() builds
#'
#' @param path File path to env_data.json (default "env_data.json")
#' @return Data frame with scenario, scenario_label, resource, mean_q, p10_q
#'   and p90_q columns, one row per resource per shipped scenario
#'
#' @details The labels are read from env_data.json through jsonlite rather
#'   than written as literals here, so the strings under test carry the same
#'   UTF-8 flag they carry in a real run.
synthetic_queue_table <- function(path = "env_data.json") {
  scenarios <- jsonlite::fromJSON(path, simplifyVector = FALSE)$scenarios
  resources <- c("b_r2b_ot_1", "b_r2eheavy_ot_1", "b_r2eheavy_icu_1", "t_PMVAmb_1")

  do.call(rbind, lapply(names(scenarios), function(s) {
    data.frame(
      scenario       = s,
      scenario_label = scenarios[[s]]$label,
      resource       = resources,
      mean_q         = seq_along(resources) / 10,
      p10_q          = 0,
      p90_q          = seq_along(resources) / 5,
      stringsAsFactors = FALSE
    )
  }))
}

#' Render the comparison plot under a named LC_CTYPE and return the PNG bytes
#'
#' @param queue_table Table as returned by synthetic_queue_table()
#' @param locale LC_CTYPE to set for the render
#' @return Raw vector of the written PNG's bytes, or NULL if the locale is
#'   unavailable on this system
render_under_locale <- function(queue_table, locale) {
  previous <- Sys.getlocale("LC_CTYPE")
  if (!nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", locale)))) return(NULL)
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", previous)), add = TRUE)

  images_dir <- file.path(tempdir(), paste0("scenario_plot_", make.names(locale)))
  dir.create(images_dir, showWarnings = FALSE, recursive = TRUE)
  # A locale-sensitive pattern raises an error rather than returning a wrong
  # answer, so it is caught here and reported as a failed check
  tryCatch(plot_scenario_comparison(queue_table, images_dir = images_dir),
           error = function(e) message("  ...render failed: ", conditionMessage(e)))

  png_file <- file.path(images_dir, "scenario_comparison.png")
  if (!file.exists(png_file)) return(raw(0))
  readBin(png_file, "raw", file.size(png_file))
}

queue_table <- synthetic_queue_table()

message("Short labels are derived from the scenario identifier")
check(identical(scenario_short_label(c("moderate_intensity", "high_intensity", "default")),
                c("Moderate Intensity", "High Intensity", "Default")),
      "scenario_short_label() title-cases each shipped identifier")
check(!any(grepl("[^ -~]", scenario_short_label(unique(queue_table$scenario)))),
      "every derived short label is ASCII, so no locale can fail to represent it")

message("The plot renders in a C locale")
c_png <- render_under_locale(queue_table, "C")
check(!is.null(c_png), "LC_CTYPE=C is available to test against")
check(length(c_png) > 0, "plot_scenario_comparison() writes a PNG under LC_CTYPE=C")

message("The plot is byte-identical across locales")
utf8_png <- NULL
for (loc in c("C.UTF-8", "en_US.UTF-8", "en_AU.UTF-8")) {
  utf8_png <- render_under_locale(queue_table, loc)
  if (!is.null(utf8_png)) break
}
if (is.null(utf8_png)) {
  message("  SKIP  no UTF-8 locale available on this system to compare against")
} else {
  check(identical(c_png, utf8_png),
        "the PNG rendered under a C locale matches the one rendered under UTF-8")
}

if (length(failures) > 0) {
  message("\nFAILED: ", length(failures), " check(s) did not hold")
  quit(status = 1)
}
message("\nAll scenario label checks passed")
