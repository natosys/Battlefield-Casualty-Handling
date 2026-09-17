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
#   4. The reconstruction sequence is interval-driven, not count-driven. A
#      casualty whose wounds require staged soft-tissue coverage returns to
#      theatre on the configured interval until reconstruction, so the number of
#      procedures follows from how long their debridement phase runs rather than
#      from a configured count. They are evacuated whatever their drawn
#      recovery, reconstruction being a reason to evacuate in its own right, and
#      evacuation_reason records which of the three reasons decided it.
#
#   5. The report is idempotent. The interval is drawn, so without the preserved
#      stream two analyses of one run would disagree; that is the property this
#      check exists to hold, the reported demand being a function of the
#      campaign rather than of how many times it is read.

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

#' Reconstruction share that puts the cohort in force
#'
#' @details The shipped share is zero, both defaults being flipped together in
#'   a later measurement campaign, so the cohort assertions are made at the
#'   share the calibration supports.
CHECK_SHARE <- 0.2

#' Run the model at a given saturation threshold and reconstruction share
#'
#' @param threshold The saturation threshold to configure.
#' @param share The reconstruction share to configure, or NULL to leave it
#'   shipped, or NA to remove the field altogether.
#' @return A list of the configuration used and the casualty-wide attributes.
run_at <- function(threshold, share = NULL) {
  ed <- load_scenario("env_data.json", "default")
  ed$vars$r2eheavy$second_surgery$saturation_queue_threshold <- threshold
  if (!is.null(share)) {
    if (length(share) == 1L && is.na(share)) {
      ed$vars$role4$surgery$reconstruction_share <- NULL
    } else {
      ed$vars$role4$surgery$reconstruction_share <- share
    }
  }
  install_config(ed)
  set.seed(CHECK_SEED)
  invisible(capture.output(wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)))
  wide <- build_attributes_wide(get_mon_attributes(list(wrapped)),
                                get_mon_arrivals(list(wrapped), ongoing = TRUE))
  wide$replication <- 1L
  list(params = ed$vars$role4, wide = wide,
       arrivals = get_mon_arrivals(list(wrapped), ongoing = TRUE),
       policy_days = ed$vars$r2eheavy$recovery$evacuation_policy_days)
}

#' A run's arrival monitor in a canonical row order
#'
#' @param run The list `run_at()` returned.
#' @return The arrival monitor, ordered by casualty name.
#' @details The monitor's own row order is the order arrivals completed, which
#'   two identical campaigns need not share.
arrivals_of <- function(run) {
  arr <- run$arrivals
  arr[order(arr$name), , drop = FALSE]
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

shipped_share <- shipped$vars$role4$surgery$reconstruction_share
report(!is.null(shipped_share) && length(shipped_share) == 1L &&
         !is.na(shipped_share) && shipped_share == 0,
       "role4.surgery.reconstruction_share ships at 0, disabling the cohort")

off <- run_at(0)
off_demand <- compute_role4_surgical_demand(off$wide, off$params)
report(nrow(off_demand) == 0,
       paste("no theatre demand is reported at the shipped configuration, both",
             "the capacity release and the reconstruction cohort being disabled"))

# The degenerate share must consume no draw, which is what keeps the published
# seed-42 evidence set exactly as it was rather than merely close to it.
absent <- run_at(0, share = NA)
report(isTRUE(all.equal(arrivals_of(off), arrivals_of(absent), check.attributes = FALSE)),
       paste("a share of zero and no share at all produce the same campaign, so",
             "the cohort costs the published model nothing"))

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
timed <- demand %>% filter(theatre_minutes > 0)
report(nrow(admitted) > 0 && nrow(timed) > 0,
       paste("released casualties who reached Role 4 contribute timed",
             "operations (%d casualties, %d timed operation-days)"),
       nrow(admitted), nrow(timed))
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
       paste("re-analysing one run reproduces the report exactly, the interval",
             "draw running inside the preserved stream"))
before_seed <- if (exists(".Random.seed", envir = globalenv())) {
  get(".Random.seed", envir = globalenv())
} else {
  NULL
}
invisible(compute_role4_surgical_demand(on$wide, on$params))
after_seed <- if (exists(".Random.seed", envir = globalenv())) {
  get(".Random.seed", envir = globalenv())
} else {
  NULL
}
report(identical(before_seed, after_seed),
       "the report leaves the caller's random number stream where it found it")

complete <- on$wide[which(is.na(num(on$wide, "definitive_repair_outstanding"))), , drop = FALSE]
report(nrow(complete) > 0 && sum(demand$operations) < nrow(on$wide),
       "a casualty who arrived with their repair complete is owed nothing (%d such casualties)",
       nrow(complete))

# ── 4. Ordering against the ward phases ────────────────────────────────────

cat("\n-- ordering, and the intensive care that follows --\n")

assigned <- assign_role4_los(on$wide, on$params)
owed_rows <- assigned %>%
  filter(!is.na(definitive_repair_outstanding) & definitive_repair_outstanding == 1)
timed_days <- demand %>%
  filter(theatre_minutes > 0) %>%
  left_join(owed_rows %>%
              group_by(replication, r4_admit_day) %>%
              summarise(n_admitted = dplyr::n(), .groups = "drop"),
            by = c("replication" = "replication", "day" = "r4_admit_day"))
report(all(!is.na(timed_days$n_admitted)),
       paste("every conserved repair is owed on its casualty's day of",
             "admission, not on some later day"))

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

# ── 5. The reconstruction sequence ─────────────────────────────────────────

cat("\n-- the reconstruction sequence --\n")

live <- run_at(0, share = CHECK_SHARE)
seq_assigned <- assign_role4_los(live$wide, live$params)
sequence <- with_preserved_rng(role4_reconstruction_sequence(seq_assigned, live$params))
cohort <- seq_assigned %>%
  filter(!is.na(reconstruction_required) & reconstruction_required == 1)

report(nrow(cohort) > 0,
       "casualties are drawn into the reconstruction cohort (%d reaching Role 4)",
       nrow(cohort))
report(sum(sequence$procedure == "reconstruction") == nrow(cohort),
       paste("each casualty in the cohort receives exactly one reconstruction",
             "(%d reconstructions, %d casualties)"),
       sum(sequence$procedure == "reconstruction"), nrow(cohort))
report(sum(sequence$procedure == "debridement") > 0,
       paste("debridements precede the reconstruction (%d across the cohort),",
             "so the sequence is not the reconstruction alone"),
       sum(sequence$procedure == "debridement"))

# The count must follow from the interval rather than from a configured number:
# widening the interval with everything else held fixed has to reduce it.
slow_params <- live$params
slow_params$surgery$return_interval_min  <- 6
slow_params$surgery$return_interval_mode <- 7
slow_params$surgery$return_interval_max  <- 8
slow <- with_preserved_rng(role4_reconstruction_sequence(seq_assigned, slow_params))
report(nrow(slow) < nrow(sequence),
       paste("lengthening the return interval reduces the procedure count",
             "(%d against %d), so the count is derived rather than configured"),
       nrow(slow), nrow(sequence))
report(sum(slow$procedure == "reconstruction") == nrow(cohort),
       "the reconstruction survives a longer interval, only the debridements thin")

bad_params <- live$params
bad_params$surgery$return_interval_mode <- 99
err <- tryCatch({
  role4_reconstruction_sequence(seq_assigned, bad_params)
  NULL
}, error = function(e) conditionMessage(e))
report(!is.null(err) && grepl("return_interval", err, fixed = TRUE),
       "an out-of-order return interval is rejected with a message naming the field")

# ── 6. Reconstruction is a reason to evacuate in its own right ─────────────

cat("\n-- reconstruction as an evacuation reason --\n")

reasons   <- num(live$wide, "evacuation_reason")
required  <- num(live$wide, "reconstruction_required")
evacuated <- num(live$wide, "r2e_evac")
recon     <- which(!is.na(required) & required == 1)

report(length(recon) > 0 && all(!is.na(evacuated[recon]) & evacuated[recon] == 1),
       paste("every casualty whose wounds require reconstruction is evacuated,",
             "whatever their drawn recovery (%d casualties)"),
       length(recon))
report(all(reasons[recon] == 2),
       "each of them carries evacuation_reason 2, reconstruction required")
#' Evacuation policy the redundancy assertion is made against, in days
#'
#' @details The reason has to change an outcome somewhere in the doctrinal
#'   range, or it is decoration. At the shipped 21-day policy this cohort's
#'   severity-scaled recovery exceeds the threshold anyway, so the test is made
#'   at 60 days, the upper bound the policy's doctrinal source states is a
#'   command decision: there the reconstruction requirement is what evacuates
#'   them and the policy is not.
LONG_POLICY_DAYS <- 60
within <- num(live$wide, "recovery_to_duty_days")[recon]
report(any(!is.na(within) & within <= LONG_POLICY_DAYS),
       paste("at a %g-day policy %d of them would be retained on recovery alone,",
             "so the reason changes the disposition rather than restating it"),
       LONG_POLICY_DAYS, sum(!is.na(within) & within <= LONG_POLICY_DAYS))
report(all(!is.na(within)),
       paste("every one of them still carries a drawn recovery, the reason",
             "overriding it rather than replacing it"))
report(sum(!is.na(reasons) & reasons == 0) > 0 && sum(!is.na(reasons) & reasons == 1) > 0,
       paste("the other reasons stay reachable: %d retained, %d evacuated on",
             "recovery beyond the policy"),
       sum(!is.na(reasons) & reasons == 0), sum(!is.na(reasons) & reasons == 1))

for (key in c("definitive_repair_outstanding", "reconstruction_required",
              "evacuation_reason")) {
  report(key %in% MODEL_ATTRIBUTE_KEYS,
         "%s is listed in MODEL_ATTRIBUTE_KEYS", key)
}

# ── 7. The echelon is still not simulated ──────────────────────────────────

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
