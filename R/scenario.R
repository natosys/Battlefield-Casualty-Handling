##############################################
## R/scenario.R                             ##
## Scenario profile overlay mechanism       ##
##############################################
#
# Dependency-free (base R only) so this file can be sourced by both the
# simulation engine (R/environment.R) and the Shiny parameter editor
# (controller.R) without pulling in simmer/simmer.bricks.

# ── Scenario overlay ─────────────────────────────────────────────────────────

#' Overlays a scenario's variable overrides onto the base `vars` block
#'
#' @param base_vars Raw `vars` list as parsed from env_data.json (list of
#'   elm blocks, each with `elm` and `actys`; each acty has `acty` and `vals`,
#'   each val has `var` and `val`)
#' @param override_vars Raw `vars` list in the same shape, sourced from a
#'   named scenario's `vars` block. May be NULL (no override).
#' @return Merged `vars` list in the same shape as `base_vars`. Only vars
#'   named in `override_vars` are replaced; every other elm/acty/var in
#'   `base_vars` is left untouched. Elm/acty/var names present in the
#'   override but absent from the base are appended.
merge_scenario_vars <- function(base_vars, override_vars) {
  if (is.null(override_vars)) return(base_vars)

  elm_names <- vapply(base_vars, function(e) e$elm, character(1))

  for (override_elm in override_vars) {
    elm_idx <- match(override_elm$elm, elm_names)

    if (is.na(elm_idx)) {
      base_vars[[length(base_vars) + 1]] <- override_elm
      elm_names <- c(elm_names, override_elm$elm)
      next
    }

    base_actys <- base_vars[[elm_idx]]$actys
    acty_names <- vapply(base_actys, function(a) a$acty, character(1))

    for (override_acty in override_elm$actys) {
      acty_idx <- match(override_acty$acty, acty_names)

      if (is.na(acty_idx)) {
        base_actys[[length(base_actys) + 1]] <- override_acty
        acty_names <- c(acty_names, override_acty$acty)
        next
      }

      base_vals <- base_actys[[acty_idx]]$vals
      var_names <- vapply(base_vals, function(v) v$var, character(1))

      for (override_val in override_acty$vals) {
        var_idx <- match(override_val$var, var_names)

        if (is.na(var_idx)) {
          base_vals[[length(base_vals) + 1]] <- override_val
          var_names <- c(var_names, override_val$var)
        } else {
          base_vals[[var_idx]]$val <- override_val$val
        }
      }

      base_actys[[acty_idx]]$vals <- base_vals
    }

    base_vars[[elm_idx]]$actys <- base_actys
  }

  base_vars
}

#' Resolves a named scenario against parsed env_data.json content
#'
#' @param json_data Raw parsed JSON list (fromJSON(..., simplifyVector = FALSE))
#' @param scenario Name of scenario profile to apply. "default" (or NULL)
#'   returns json_data unchanged — this is the no-regression path guaranteed
#'   by Issue #54's acceptance criteria.
#' @return json_data with `vars` overlaid by the named scenario's overrides,
#'   plus `active_scenario` / `active_scenario_label` metadata fields
#'
#' @details Only `vars` is overlaid. Structural configuration — `elms`
#'   (resource/bed/team counts), `transports` (fleet sizes), and `pops`
#'   (population counts) — is never scenario-specific and is left untouched,
#'   consistent with the parameter classification in Issue #54.
resolve_scenario <- function(json_data, scenario = "default") {
  if (is.null(scenario) || identical(scenario, "default")) return(json_data)

  available <- names(json_data$scenarios)

  if (is.null(available) || !(scenario %in% available)) {
    stop(sprintf(
      "Unknown scenario '%s'. Available scenarios: %s",
      scenario,
      if (is.null(available) || length(available) == 0) "(none defined)" else paste(available, collapse = ", ")
    ))
  }

  scenario_def <- json_data$scenarios[[scenario]]
  json_data$vars <- merge_scenario_vars(json_data$vars, scenario_def$vars)
  json_data$active_scenario <- scenario
  json_data$active_scenario_label <- if (!is.null(scenario_def$label)) scenario_def$label else scenario

  json_data
}
