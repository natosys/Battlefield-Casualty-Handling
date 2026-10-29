##############################################
## R/analysis_common.R                      ##
## Shared analysis helpers                  ##
##############################################
#
# Input validation for the analysis entry points, the stream-neutral
# evaluator the Role 4 draws run under, and build_attributes_wide(), the pivot
# of the attributes monitor every other analysis module and several
# experiment modules read.
#
# Sourced by R/analysis.R, which loads the packages and the shared modules
# these functions use; source that file rather than this one alone.

# ── Entry-point input validation ─────────────────────────────────────────────
# This module's public entry points (analyse_run(), analyse_replications(),
# and the two capacity sweeps) are the boundary between data produced
# elsewhere and several thousand lines of pipeline that index it by name.
# Validating the shape once, here, turns a malformed input into a message
# naming what is missing rather than a subscript or dplyr error raised deep
# inside a summarise() the caller never wrote. Interior helpers are then
# entitled to assume well-formed input.

# Columns each monitoring data frame must carry for the pipeline to run.
# `replication` is required of all three even for a single run, which
# run_once()'s wrap() supplies as 1: the pipeline joins arrivals to
# attributes on it throughout.
#' Columns each monitoring frame must carry for the pipeline to read it
#'
#' @details Taken from what the pipeline itself indexes, so a frame passing
#'   this check reaches every stage without a missing-column error. Removing
#'   an entry would let a malformed frame through to the stage that reads it.
MON_REQUIRED_COLUMNS <- list(
  arrivals   = c("name", "start_time", "end_time", "activity_time", "replication"),
  attributes = c("name", "key", "value", "replication"),
  resources  = c("resource", "time", "server", "queue", "replication")
)

#' Assert that a monitoring list carries what the analysis pipeline reads
#'
#' @param mon Named list with elements arrivals, attributes and resources,
#'   as returned by run_once() or run_replications()
#' @param caller Name of the calling entry point, used in the error message
#' @return TRUE, invisibly, if the monitoring list is well formed; otherwise
#'   stops with a message naming the element and column at fault
#'
#' @details Checks presence and class of the three elements, presence of the
#'   columns listed in MON_REQUIRED_COLUMNS, and that `arrivals` is non-empty
#'   (a run with no arrivals has nothing to report on, and every summary in
#'   the pipeline is keyed off it). `attributes` and `resources` are allowed
#'   to be empty, which a short run with no recorded attribute or no seized
#'   resource legitimately produces.
validate_monitoring <- function(mon, caller) {
  problems <- character(0)

  if (!is.list(mon) || is.data.frame(mon)) {
    stop(sprintf("%s: mon must be a named list of monitoring data frames, found %s",
                 caller, paste(class(mon), collapse = "/")), call. = FALSE)
  }

  for (element in names(MON_REQUIRED_COLUMNS)) {
    df <- mon[[element]]
    if (is.null(df)) {
      problems <- c(problems, sprintf("mon$%s is missing", element))
      next
    }
    if (!is.data.frame(df)) {
      problems <- c(problems, sprintf("mon$%s must be a data frame, found %s",
                                      element, paste(class(df), collapse = "/")))
      next
    }
    missing_cols <- setdiff(MON_REQUIRED_COLUMNS[[element]], names(df))
    if (length(missing_cols) > 0) {
      problems <- c(problems, sprintf("mon$%s is missing column(s): %s",
                                      element, paste(missing_cols, collapse = ", ")))
    }
  }

  if (is.data.frame(mon$arrivals) && nrow(mon$arrivals) == 0) {
    problems <- c(problems, "mon$arrivals is empty: the run produced no casualties")
  }

  if (length(problems) > 0) {
    stop(sprintf("%s: malformed monitoring data:\n  - %s",
                 caller, paste(problems, collapse = "\n  - ")), call. = FALSE)
  }

  invisible(TRUE)
}

#' Assert that a value is a single positive whole number
#'
#' @param value Value to check
#' @param field Field name to name in the error message
#' @param caller Name of the calling entry point, used in the error message
#' @return TRUE, invisibly, if the value is a single positive whole number;
#'   otherwise stops
#'
#' @details Used for the replication and duration arguments of the sweep
#'   entry points, where a zero, a fraction or a vector produces either an
#'   empty sweep or a confusing error several minutes into a long run.
validate_positive_count <- function(value, field, caller) {
  usable <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.finite(value) && value >= 1 && value == as.integer(value)
  if (!usable) {
    stop(sprintf("%s: %s must be a single positive whole number, found %s",
                 caller, field, paste(format(value), collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Assert that a sweep entry point's common arguments are usable
#'
#' @param n_days Simulation duration in days
#' @param n_rep Replications per sweep point
#' @param path File path to env_data.json
#' @param progress_dir Directory for progress marker files, or NULL
#' @param caller Name of the calling entry point, used in the error message
#' @return TRUE, invisibly, if the arguments are usable; otherwise stops
#'
#' @details A sweep is a multi-hour call, so its arguments are checked before
#'   the first replication rather than discovered at the point of use. The
#'   configuration path is checked for existence here because the sweeps read
#'   it themselves rather than taking the already-loaded globals.
validate_sweep_args <- function(n_days, n_rep, path, progress_dir, caller) {
  validate_positive_count(n_days, "n_days", caller)
  validate_positive_count(n_rep,  "n_rep",  caller)

  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop(sprintf("%s: path must be a single non-empty string, found %s",
                 caller, paste(format(path), collapse = ", ")), call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("%s: path does not exist: %s", caller, path), call. = FALSE)
  }
  if (!is.null(progress_dir) && !dir.exists(progress_dir)) {
    stop(sprintf("%s: progress_dir does not exist: %s", caller, progress_dir),
         call. = FALSE)
  }

  invisible(TRUE)
}

#' Assert that a transport sweep's fleet sizes are usable
#'
#' @param fleet_sizes Named list, one element per vehicle, each a vector of
#'   fleet quantities to sweep
#' @param caller Name of the calling entry point, used in the error message
#' @return TRUE, invisibly, if the fleet sizes are usable; otherwise stops
#'
#' @details The names are the vehicle identifiers matched against the
#'   `transports` block of the configuration, so an unnamed list sweeps
#'   nothing and is rejected rather than silently returning an empty result.
validate_fleet_sizes <- function(fleet_sizes, caller) {
  usable <- is.list(fleet_sizes) && length(fleet_sizes) > 0 &&
    !is.null(names(fleet_sizes)) && all(nzchar(names(fleet_sizes)))
  if (!usable) {
    stop(sprintf("%s: fleet_sizes must be a non-empty named list, one element per vehicle",
                 caller), call. = FALSE)
  }
  for (vehicle in names(fleet_sizes)) {
    qtys <- fleet_sizes[[vehicle]]
    usable <- is.numeric(qtys) && length(qtys) > 0 && !any(is.na(qtys)) &&
      all(qtys >= 0) && all(qtys == as.integer(qtys))
    if (!usable) {
      stop(sprintf(paste0("%s: fleet_sizes$%s must be a non-empty vector of ",
                          "non-negative whole numbers, found %s"),
                   caller, vehicle, paste(format(qtys), collapse = ", ")), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Assert that a forward holding sweep's arms are usable
#'
#' @param arms Data frame with a character `label`, a numeric `window` in
#'   minutes and a `trigger` of 0 or 1, one row per arm
#' @param caller Name of the calling entry point, used in the error message
#' @return TRUE, invisibly, if the arms are usable; otherwise stops
#'
#' @details A window is a duration and the trigger a switch, so a negative
#'   window or a trigger other than 0 or 1 has no meaning and would be swept
#'   for hours before producing one.
validate_forward_hold_arms <- function(arms, caller) {
  usable <- is.data.frame(arms) && nrow(arms) > 0 &&
    all(c("label", "window", "trigger") %in% names(arms)) &&
    is.character(arms$label) && !anyDuplicated(arms$label) &&
    is.numeric(arms$window) && !any(is.na(arms$window)) && all(arms$window >= 0) &&
    is.numeric(arms$trigger) && all(arms$trigger %in% c(0, 1))
  if (!usable) {
    stop(sprintf(paste("%s: arms must be a non-empty data frame of unique labels, windows",
                       "of at least zero and triggers of 0 or 1, found %s"),
                 caller, paste(utils::capture.output(print(arms)), collapse = " ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Assert that a R2B holding threshold sweep's two axes are usable
#'
#' @param hold_beds Numeric vector of R2B holding beds per unit to sweep.
#' @param evac_threshold_min Numeric vector of evacuation thresholds to sweep,
#'   in minutes.
#' @param caller Name of the calling entry point, used in the error message.
#' @return TRUE, invisibly, if both axes are usable; otherwise stops.
#'
#' @details A bed count is a whole number of at least one, an evacuation
#'   threshold zero or a positive number of minutes (zero disabling it, per
#'   `r2b_evac_threshold()`, R/trajectories_common.R), so either axis carrying a
#'   negative or fractional value would sweep for hours before producing a
#'   result no configuration could reach.
validate_hold_threshold_sweep <- function(hold_beds, evac_threshold_min, caller) {
  beds_ok <- is.numeric(hold_beds) && length(hold_beds) > 0 && !any(is.na(hold_beds)) &&
    all(hold_beds >= 1) && all(hold_beds == as.integer(hold_beds))
  if (!beds_ok) {
    stop(sprintf(paste0("%s: hold_beds must be a non-empty vector of whole numbers of at ",
                        "least 1, found %s"),
                 caller, paste(format(hold_beds), collapse = ", ")), call. = FALSE)
  }
  thresholds_ok <- is.numeric(evac_threshold_min) && length(evac_threshold_min) > 0 &&
    !any(is.na(evac_threshold_min)) && all(evac_threshold_min >= 0)
  if (!thresholds_ok) {
    stop(sprintf(paste0("%s: evac_threshold_min must be a non-empty vector of non-negative ",
                        "minutes, found %s"),
                 caller, paste(format(evac_threshold_min), collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Evaluates an expression without leaving the caller's RNG stream advanced
#'
#' @param expr Expression to evaluate
#' @return The value of `expr`
#'
#' @details The analysis pipeline is a report over monitoring data already
#'   produced, so analysing the same data twice has to give the same answer.
#'   Any random draw made inside it breaks that: the second call resumes the
#'   stream where the first left it and draws different numbers. Saving
#'   `.Random.seed` and restoring it on exit makes the enclosed draw consume
#'   no net randomness, so the next call starts from the position this one
#'   did. The seed is deliberately not set here — the draw is left at exactly
#'   the stream position it already occupies, which is what keeps the tracked
#'   seed-42 baseline unmoved. The stream may not exist yet when nothing has
#'   drawn in the session, in which case the exit handler removes the one this
#'   evaluation created rather than restoring a value. `with_fixed_rng()`
#'   (R/sensitivity.R) performs the same save and restore around a fixed seed,
#'   and repeats it rather than calling this function because three of the
#'   sensitivity entry points source that file without this one.
with_preserved_rng <- function(expr) {
  seed_symbol <- ".Random.seed"
  if (exists(seed_symbol, envir = globalenv())) {
    old <- get(seed_symbol, envir = globalenv())
    on.exit(assign(seed_symbol, old, envir = globalenv()), add = TRUE)
  } else {
    on.exit(suppressWarnings(rm(list = seed_symbol, envir = globalenv())), add = TRUE)
  }
  force(expr)
}

#' Pivot the long-format attribute monitor to one row per (casualty,
#' replication) — the last non-NA value of every attribute key set on that
#' casualty within that replication.
#'
#' @param attributes Long-format attribute monitor (name, replication, time,
#'   key, value), as returned by get_mon_attributes()
#' @param arrivals Arrivals monitor (or a tibble with at least name,
#'   replication) — used only to drop stray attribute rows for casualties
#'   excluded from `arrivals` (e.g. by a warm_up filter applied upstream)
#' @return One row per (name, replication), with one column per attribute
#'   key. Guarantees the presence (as all-NA) of every key in
#'   `MODEL_ATTRIBUTE_KEYS`, which is every key the model can set, so the
#'   frame has the same shape whatever a run happened to produce: a campaign
#'   with no death of wounds, no R2E surgery, no strategic evacuation decision
#'   or nobody returned to duty still yields the same columns as one with all
#'   of them. A column that depends on the run makes analysis code fail with an
#'   object-not-found error instead of reading an empty result, which is the
#'   hazard this removes.
#'
#' @details Shared by analyse_run() and analyse_replications() —
#'   grouping by (name, replication) rather than name alone already makes
#'   this replication-aware, so the identical pivot serves both a
#'   single-replication and a pooled multi-replication monitor.
build_attributes_wide <- function(attributes, arrivals) {
  attributes_wide <- attributes %>%
    pivot_wider(
      id_cols    = c(name, replication, time),
      names_from = key,
      values_fn  = ~ dplyr::first(.x)
    ) %>%
    arrange(name, replication, time) %>%
    group_by(name, replication) %>%
    fill(everything(), .direction = "down") %>%
    slice_tail(n = 1) %>%
    ungroup()

  attributes_wide <- attributes_wide %>%
    semi_join(dplyr::select(arrivals, name, replication), by = c("name", "replication"))

  for (key in MODEL_ATTRIBUTE_KEYS) {
    if (!key %in% names(attributes_wide)) {
      attributes_wide[[key]] <- NA_real_
    }
  }

  attributes_wide
}

#' Group-wise mean +/- 95% t-distribution CI across replications
#'
#' @param df Per-replication values, one row per (group_cols, replication)
#'   combination — every combination that could occur should have an
#'   explicit row (fill with 0 first) so a replication with zero occurrences
#'   is counted as 0, not silently dropped from the mean (see
#'   role4_census_daily's roxygen in analyse_run() for why this matters)
#' @param group_cols Character vector of columns to group by (may be
#'   length 0, for a single overall CI)
#' @param value_col Column name (string) holding the per-replication value
#' @param clamp_lower_zero Clamp `ci_lower` to 0 (default TRUE) — every KPI
#'   this is currently used for is a non-negative count or rate
#' @return `df` grouped by `group_cols`, collapsed to one row per group, with
#'   columns n, mean, sd, ci_lower, ci_upper
#'
#' @details The t-distribution construction analyse_replications() also uses
#'   ad hoc (daily_ci, force_regeneration_ci), factored out so the group-wise
#'   CI outputs share one implementation.
ci_by_group <- function(df, group_cols, value_col, clamp_lower_zero = TRUE) {
  out <- df %>%
    { if (length(group_cols) > 0) group_by(., across(all_of(group_cols))) else . } %>%
    summarise(n = n(), mean = mean(.data[[value_col]]), sd = sd(.data[[value_col]]), .groups = "drop") %>%
    mutate(
      sd       = ifelse(n < 2 | is.na(sd), 0, sd),
      ci_lower = mean - qt(0.975, df = pmax(n - 1, 1)) * sd / sqrt(n),
      ci_upper = mean + qt(0.975, df = pmax(n - 1, 1)) * sd / sqrt(n)
    )
  if (clamp_lower_zero) out$ci_lower <- pmax(out$ci_lower, 0)
  out
}

#' The instant each casualty left R2B, for the R2B dwell interval
#'
#' @param combined One row per casualty, carrying `r2b_departure_time`,
#'   `return_day` and `return_echelon`.
#' @return Numeric vector of minutes: the departure towards R2E, or the return
#'   to duty from R2B, NA for a casualty still there.
#'
#' @details A casualty returned to duty from an R2B holding bed never departs
#'   towards R2E, and reading the departure alone would carry every such
#'   casualty as still at R2B when the run ends, a censored stay that never
#'   closes.
r2b_leave_time <- function(combined) {
  departed <- suppressWarnings(as.numeric(combined$r2b_departure_time))
  returned <- suppressWarnings(as.numeric(combined$return_day))
  echelon  <- suppressWarnings(as.numeric(combined$return_echelon))
  ifelse(!is.na(departed), departed,
         ifelse(!is.na(echelon) & echelon == 2, returned, NA_real_))
}
