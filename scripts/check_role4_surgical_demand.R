#!/usr/bin/env Rscript
##############################################################################
## scripts/check_role4_surgical_demand.R                                    ##
## Regression check — the operating theatre requirement a released casualty ##
## carries to the national support base                                     ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_role4_surgical_demand.R
#   Rscript scripts/check_role4_surgical_demand.R --days 10 --seed 42
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. A casualty released to strategic evacuation with the
# definitive repair outstanding used to lose that operation at the moment of
# boarding: the Role 4 census reports bed types and that echelon is given no
# operating theatre, so the work the deployed hospital did not do arrived
# nowhere. Reporting it is what makes the release redistribute demand rather
# than delete it, and it is the condition on the release ever shipping enabled.
#
# The reported quantity is a requirement, not an activity. Nothing queues for a
# theatre at Role 4, nothing is refused and nothing pushes back into theatre,
# because the model sets the demand a deployed trauma system places on the
# national support base and does not simulate that echelon. This check asserts
# that boundary as an absence, the same way the ward phases check asserts the
# absence of a Role 4 bed capacity: a queue, a capacity or a shortfall
# introduced here would be a different kind of claim.
#
# Four properties, and the first is the one the issue turns on.
#
#   1. Conservation. The operation is drawn once, by the theatre that would
#      have performed it, and carried rearward on the casualty; the analysis
#      layer reads it and draws nothing. So the surgical time a released
#      casualty accounts for across both echelons is exactly the time the
#      distribution gave them, and re-analysing one run cannot move it.
#
#   2. Coverage. Every released casualty is owed exactly one operation, and no
#      casualty who arrived with their repair complete is owed any.
#
#   3. Ordering. The operation falls on the day of admission, ahead of the
#      intensive care that follows it, so it cannot land after the step-down
#      the ward phases apply. A released casualty is also owed their whole
#      post-operative intensive care requirement here, theatre having served
#      none of it; without that they would hold an intensive care ward for a
#      whole stay with no intensive care phase in it.
#
#   4. The shipped configuration reports nothing, the release being disabled,
#      and the run reproduces what it produced before this report existed.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
  library(ggplot2)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")

args <- commandArgs(trailingOnly = TRUE)

#' Read a flag's value from the command line
#'
#' @param flag Flag name, including its leading dashes.
#' @param default Value returned when the flag is absent or has no argument.
#' @return The argument following `flag`, or `default`.
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

#' Run length in days for the live runs
CHECK_DAYS <- as.integer(arg_value("--days", 30L))

#' Seed for the live runs
CHECK_SEED <- as.integer(arg_value("--seed", 42L))

#' Threshold that puts the release in force
#'
#' @details One casualty waiting to be operated on. The sections are rostered
#'   in alternating shifts, so a queue forms readily and the released cohort is
#'   non-empty without contriving an establishment.
CHECK_THRESHOLD <- 1

state <- new.env(parent = emptyenv())
state$failures <- character(0)

#' Record a failure, deferring the non-zero exit to the end of the run
#'
#' @param ... sprintf() format string and its arguments.
#' @return Invisibly, the accumulated failure vector.
fail <- function(...) state$failures <- c(state$failures, sprintf(...))

#' Print one PASS or FAIL line, recording a failure
#'
#' @param ok TRUE when the assertion held.
#' @param fmt sprintf() format string describing the assertion.
#' @param ... Arguments to `fmt`.
#' @return Invisible NULL.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

#' Install a configuration into the globals the model reads
#'
#' @param ed The configuration list.
#' @return Invisible NULL.
install_config <- function(ed) {
  assign("env_data", ed, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(ed$elms, length), envir = globalenv())
  invisible(NULL)
}

#' Run the model at a given saturation threshold
#'
#' @param threshold The threshold to configure.
#' @return A list of the configuration used and the casualty-wide attributes.
run_at <- function(threshold) {
  ed <- load_scenario("env_data.json", "default")
  ed$vars$r2eheavy$second_surgery$saturation_queue_threshold <- threshold
  install_config(ed)
  set.seed(CHECK_SEED)
  invisible(capture.output(wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)))
  wide <- build_attributes_wide(get_mon_attributes(list(wrapped)),
                                get_mon_arrivals(list(wrapped), ongoing = TRUE))
  wide$replication <- 1L
  list(params = ed$vars$role4, wide = wide)
}

#' Read a wide-pivot column as numeric, tolerating its absence
#'
#' @param frame The wide pivot.
#' @param name The column name.
#' @return The column as numeric, or a column of NA of the right length.
num <- function(frame, name) {
  if (!name %in% names(frame)) return(rep(NA_real_, nrow(frame)))
  as.numeric(frame[[name]])
}

cat(sprintf("Role 4 surgical demand check: %d-day runs, seed %d\n\n",
            CHECK_DAYS, CHECK_SEED))

# ── 1. The shipped configuration reports no theatre demand ─────────────────

cat("-- the shipped configuration --\n")

shipped <- load_scenario("env_data.json", "default")
report(role4_surgery_enabled(shipped$vars$role4),
       "role4.surgery ships enabled, so the report exists whenever the release does")
report(identical(role4_theatre_label(shipped$vars$role4), "ot"),
       "the demand is labelled 'ot', the theatre type the deployed echelons field")

off <- run_at(0)
off_demand <- compute_role4_surgical_demand(off$wide, off$params)
report(nrow(off_demand) == 0,
       "no theatre demand is reported at the shipped threshold, the release being disabled")

# ── 2. Malformed configuration is rejected, naming the field ───────────────

cat("\n-- malformed configuration --\n")

for (bad in list(2, "yes", NA)) {
  params <- shipped$vars$role4
  params$surgery$enabled <- bad
  err <- tryCatch({
    role4_surgery_enabled(params)
    NULL
  }, error = function(e) conditionMessage(e))
  report(!is.null(err) && grepl("role4.surgery.enabled", err, fixed = TRUE),
         "enabled = %s is rejected with a message naming the field",
         paste(format(bad), collapse = "/"))
}
params <- shipped$vars$role4
params$surgery$theatre_label <- ""
err <- tryCatch({
  role4_theatre_label(params)
  NULL
}, error = function(e) conditionMessage(e))
report(!is.null(err) && grepl("theatre_label", err, fixed = TRUE),
       "an empty theatre label is rejected with a message naming the field")

# ── 3. In force: coverage and conservation ─────────────────────────────────

cat("\n-- in force: what is owed, and by whom --\n")

on <- run_at(CHECK_THRESHOLD)
demand <- compute_role4_surgical_demand(on$wide, on$params)
released <- on$wide[which(num(on$wide, "definitive_repair_outstanding") == 1), , drop = FALSE]
# Only a released casualty who reached Role 4 is owed anything here: one still
# queued for a sortie when the window closed has no admission day to report on.
admitted <- released[!is.na(as.numeric(released$evacuation_day)), , drop = FALSE]

report(nrow(released) > 0,
       "casualties are released at a threshold of %s (%d released, %d reaching Role 4)",
       format(CHECK_THRESHOLD), nrow(released), nrow(admitted))
report(sum(demand$operations) == nrow(admitted),
       paste("exactly one operation is owed per released casualty who reached",
             "Role 4 (%d owed, %d casualties)"),
       sum(demand$operations), nrow(admitted))
report(all(!is.na(num(released, "definitive_repair_minutes"))),
       "every released casualty carries the duration the releasing theatre drew")

drawn <- sum(as.numeric(admitted$definitive_repair_minutes))
report(isTRUE(all.equal(sum(demand$theatre_minutes), drawn)),
       paste("the reported theatre-minutes equal the minutes drawn, so the",
             "operation is conserved rather than re-estimated"))

surgery_params <- on$params
bounds <- c(env_data$vars$r2eheavy$surgery$min, env_data$vars$r2eheavy$surgery$max)
mins <- as.numeric(released$definitive_repair_minutes)
report(all(mins >= bounds[1] & mins <= bounds[2]),
       "each duration lies inside the R2E surgery distribution [%g, %g] it was drawn from",
       bounds[1], bounds[2])

again <- compute_role4_surgical_demand(on$wide, on$params)
report(identical(demand, again),
       "the report takes no draw of its own, so re-analysing one run reproduces it exactly")

complete <- on$wide[which(is.na(num(on$wide, "definitive_repair_outstanding"))), , drop = FALSE]
report(nrow(complete) > 0 && sum(demand$operations) < nrow(on$wide),
       "a casualty who arrived with their repair complete is owed nothing (%d such casualties)",
       nrow(complete))

# ── 4. Ordering against the ward phases ────────────────────────────────────

cat("\n-- ordering, and the intensive care that follows --\n")

assigned <- assign_role4_los(on$wide, on$params)
owed_rows <- assigned %>%
  filter(!is.na(definitive_repair_outstanding) & definitive_repair_outstanding == 1)
joined <- demand %>%
  left_join(owed_rows %>%
              group_by(replication, r4_admit_day) %>%
              summarise(n_admitted = dplyr::n(), .groups = "drop"),
            by = c("replication" = "replication", "day" = "r4_admit_day"))
report(all(!is.na(joined$n_admitted)),
       "every operation is owed on its casualty's day of admission, not on some later day")

phases <- role4_ward_phases(assigned, on$params)
owed_phases <- phases %>%
  filter(!is.na(definitive_repair_outstanding) & definitive_repair_outstanding == 1)
report(all(owed_phases$phase_start >= owed_phases$r4_admit_day),
       "no ward phase of a released casualty begins before the day the operation is owed")

icu_ward <- as.character(on$params$icu_continuation$icu_ward)
icu_first <- owed_phases %>%
  group_by(name) %>%
  arrange(phase_start, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()
report(nrow(icu_first) > 0 && all(icu_first$phase_ward == icu_ward),
       paste("a released casualty's first phase is the intensive care that",
             "follows the operation, not a step-down ward"))
report(all(as.numeric(owed_rows$r4_icu_days) > 0),
       "every released casualty is owed intensive care at Role 4, theatre having served none of it")

# ── 5. The echelon is still not simulated ──────────────────────────────────

cat("\n-- the boundary this report does not cross --\n")

analysis_source <- readLines("R/analysis.R", warn = FALSE)
for (absent in c("role4_theatre_capacity", "role4_theatre_shortfall", "role4_ot_queue")) {
  report(!any(grepl(absent, analysis_source, fixed = TRUE)),
         "the analysis module carries no '%s'", absent)
}
report(!any(grepl("theatre_capacity", readLines("env_data.json", warn = FALSE), fixed = TRUE)),
       "env_data.json gives the national support base no theatre capacity to be short of")

# ── Result ─────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (msg in state$failures) cat(sprintf("  - %s\n", msg))
  quit(status = 1)
}
cat("All checks passed.\n")
quit(status = 0)
