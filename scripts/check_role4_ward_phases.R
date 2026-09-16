#!/usr/bin/env Rscript
##############################################################################
## scripts/check_role4_ward_phases.R                                        ##
## Regression check — the Role 4 ward split conserves the length of stay    ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_role4_ward_phases.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. A Role 4 casualty used to hold one ward for the whole
# of a length of stay drawn at admission, so every Priority 1 surgical casualty
# was counted in an intensive care bed for a stay of up to 45 days and the
# reported intensive care demand was a ceiling rather than an estimate
# (Issue #314). The stay can now be split, an intensive care phase followed by
# a step-down ward, and the split is where a census can silently stop adding
# up: a phase computed independently of the stay it divides can lose a day to
# rounding, double-count the boundary, or leave a gap the census simply does
# not count. None of those would error, and all of them would move the figure
# a planner sizes a national support base from.
#
# The split is therefore taken as a difference rather than drawn: the step-down
# ward carries whatever the intensive care phase leaves. That makes
# conservation structural, and this check is what holds it structural, across
# the boundary cases where a difference-based split still goes wrong, an
# intensive care requirement longer than the stay, one of zero, and a casualty
# theatre never operated on.
#
# The disabled case matters as much as the enabled one. The block ships
# disabled, so the shipped configuration must reproduce the single-ward census
# it replaced exactly rather than approximately, and it must do so without
# consuming a random draw: the length-of-stay draw sits inside a preserved
# stream, so one extra draw would move every stay after it.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(triangle)
  library(ggplot2)
})

source("R/constants.R")
source("R/environment.R")
source("R/analysis.R")

# ── Run parameters ───────────────────────────────────────────────────────────

#' Control seed the constructed cohorts are drawn under
CHECK_SEED <- 42L

#' Casualties in each constructed cohort
CHECK_CASUALTIES <- 400L

#' Intensive care requirement, in days, that splits a stay into two phases
#'
#' @details Chosen to sit inside the Priority 1 surgical stay of 10 to 45 days
#'   so that both phases are non-empty for most casualties, which is what makes
#'   the conservation assertion non-vacuous.
CHECK_ICU_DAYS <- list(min = 3, mode = 8, max = 20)

#' Intensive care requirement longer than any stay it could divide
#'
#' @details The boundary the difference-based split is most exposed to: the
#'   step-down phase's length is the stay less the intensive care phase, which
#'   is negative here unless the phase is capped at the stay first.
CHECK_ICU_DAYS_LONG <- list(min = 60, mode = 90, max = 120)

#' Tolerance on a comparison of two computed reals
TOL <- 1e-9

state <- new.env(parent = emptyenv())
state$failures <- character(0)

#' Record a failure message
#'
#' @param ... sprintf() arguments describing the failure.
#' @return Invisible NULL.
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

#' Build a Role 4 configuration with the continuation block in a given state
#'
#' @param enabled 1 to split a stay across wards, 0 for the single-ward census.
#' @param icu_days List of min/mode/max intensive care days.
#' @param step_down Ward the remainder of a split stay is served in.
#' @return A `env_data$vars$role4`-shaped list.
role4_config <- function(enabled = 0, icu_days = CHECK_ICU_DAYS,
                         step_down = "Surgical Ward") {
  list(
    wards = list(p1_surgical = "ICU", p1_nonsurgical = "Surgical Ward",
                 p2 = "Surgical Ward", p3_dnbi = "General Ward",
                 levels = list("ICU", "Surgical Ward", "General Ward")),
    icu_continuation = list(enabled = enabled, min = icu_days$min,
                            mode = icu_days$mode, max = icu_days$max,
                            step_down_ward = step_down),
    los_p1_surgical = list(min = 10, mode = 21, max = 45),
    los_p1_nonsurgical = list(min = 7, mode = 14, max = 30),
    los_p2 = list(min = 5, mode = 10, max = 21),
    los_p3_dnbi = list(min = 2, mode = 5, max = 14)
  )
}

#' Build a cohort of evacuated casualties for the census to divide
#'
#' @param n Casualties in the cohort.
#' @param operated TRUE to give each casualty a post-operative episode.
#' @return A data frame shaped like analyse_run()'s `combined`, filtered to
#'   evacuated casualties.
build_cohort <- function(n = CHECK_CASUALTIES, operated = TRUE) {
  set.seed(CHECK_SEED)
  data.frame(
    case_id = seq_len(n),
    replication = 1L,
    evacuation_day = sample.int(30, n, replace = TRUE),
    priority = 1,
    injury_type = 1,
    treatment_received = 1,
    post_definitive_min = if (operated) 1440 else NA_real_,
    r2e_evac = 1
  )
}

#' Days each casualty's phases account for, against the stay they divide
#'
#' @param assigned Frame returned by assign_role4_los().
#' @param phases Frame returned by role4_ward_phases().
#' @return Data frame with case_id, phase_days and stay_days.
phase_totals <- function(assigned, phases) {
  phases %>%
    mutate(served = phase_end - phase_start + 1) %>%
    group_by(case_id) %>%
    summarise(phase_days = sum(served), .groups = "drop") %>%
    left_join(assigned %>%
                transmute(case_id,
                          stay_days = r4_discharge_day - r4_admit_day + 1),
              by = "case_id")
}

# ── 1. The shipped configuration is the single-ward census ───────────────────

cat("\n-- the shipped configuration reproduces the single-ward census --\n")

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
shipped <- build_environment(resolve_scenario(json_data, "default"))$vars$role4

report(as.numeric(shipped$icu_continuation$enabled) == 0,
       "the intensive care continuation ships disabled (found %s)",
       format(shipped$icu_continuation$enabled))

shipped_map <- role4_ward_map(shipped)
historical <- c(p1_surgical = "ICU", p1_nonsurgical = "Surgical Ward",
                p2 = "Surgical Ward", p3_dnbi = "General Ward")
report(identical(shipped_map[names(historical)], historical),
       "the shipped ward mapping is the one the hard-coded rule applied (%s)",
       paste(sprintf("%s=%s", names(shipped_map), shipped_map), collapse = ", "))

cohort <- build_cohort()
off_assigned <- assign_role4_los(cohort, role4_config(enabled = 0))
off_phases <- role4_ward_phases(off_assigned, role4_config(enabled = 0))

report(nrow(off_phases) == nrow(off_assigned),
       "a disabled block yields one phase per casualty (%d phases, %d casualties)",
       nrow(off_phases), nrow(off_assigned))
report(all(off_phases$phase_ward == off_phases$ward),
       "every phase is served in the casualty's admission ward")
off_totals <- phase_totals(off_assigned, off_phases)
report(all(off_totals$phase_days == off_totals$stay_days),
       "the single phase spans the whole stay for all %d casualties",
       nrow(off_totals))

# The draw the disabled path must not take. Two assignments from one seed have
# to agree exactly; an extra draw inside the preserved stream would move every
# length of stay after it and this would differ in the tail rather than
# everywhere, which is why it is compared over the whole vector.
set.seed(CHECK_SEED)
first <- assign_role4_los(cohort, role4_config(enabled = 0))$los_days
set.seed(CHECK_SEED)
second <- assign_role4_los(cohort, role4_config(enabled = 0))$los_days
report(length(first) == length(second) && max(abs(first - second)) < TOL,
       "a disabled block consumes no draw of its own, so the stays repeat exactly")

# ── 2. A split stay conserves its length exactly ─────────────────────────────

cat("\n-- a split stay conserves its length --\n")

on_config <- role4_config(enabled = 1)
on_assigned <- assign_role4_los(cohort, on_config)
on_phases <- role4_ward_phases(on_assigned, on_config)

report(nrow(on_phases) > nrow(on_assigned),
       "an enabled block splits at least one stay in two (%d phases, %d casualties)",
       nrow(on_phases), nrow(on_assigned))

on_totals <- phase_totals(on_assigned, on_phases)
report(all(on_totals$phase_days == on_totals$stay_days),
       "the phases account for the whole stay for all %d casualties",
       nrow(on_totals))

# A gap loses bed-days the census never counts; an overlap counts one casualty
# in two beds on one day. Neither errors, and both move the published figure.
ordered <- on_phases %>% arrange(case_id, phase_start) %>% group_by(case_id)
boundaries <- ordered %>%
  summarise(gap = any(lead(phase_start) > phase_end + 1, na.rm = TRUE),
            overlap = any(lead(phase_start) <= phase_end, na.rm = TRUE),
            .groups = "drop")
report(!any(boundaries$gap), "no casualty's phases leave a gap in the stay")
report(!any(boundaries$overlap), "no casualty's phases overlap on a day")

report(all(on_phases$phase_ward %in% role4_ward_levels(on_config)),
       "every phase names a configured ward")

# ── 3. The requirement is what theatre has not already served ────────────────

cat("\n-- Role 4 serves the requirement theatre did not --\n")

served_days <- cohort$post_definitive_min[1] / DAY_MIN
report(all(on_assigned$r4_icu_days <= on_assigned$los_days + TOL),
       "no casualty is owed more intensive care than its stay is long")
report(all(on_assigned$r4_icu_days >= 0),
       "no casualty is owed a negative number of intensive care days")

mean_total <- (CHECK_ICU_DAYS$min + CHECK_ICU_DAYS$mode + CHECK_ICU_DAYS$max) / 3
expected <- mean_total - served_days
observed <- mean(on_assigned$r4_icu_days)
# The cap at the stay length biases the observed mean down a little, so the
# assertion is that theatre's contribution is visibly deducted rather than that
# the two agree exactly.
report(observed < mean_total && observed > expected - 1,
       "the mean requirement of %.2f days less theatre's %.2f reads %.2f",
       mean_total, served_days, observed)

unoperated <- build_cohort(operated = FALSE)
unoperated$treatment_received <- 0
un_assigned <- assign_role4_los(unoperated, on_config)
report(all(un_assigned$r4_icu_days == 0),
       "a casualty theatre never operated on is owed no intensive care")
un_phases <- role4_ward_phases(un_assigned, on_config)
report(nrow(un_phases) == nrow(un_assigned) &&
         all(un_phases$phase_ward == un_phases$ward),
       "an unoperated casualty keeps the whole stay on its admission ward")

# ── 4. A requirement longer than the stay ────────────────────────────────────

cat("\n-- a requirement longer than the stay is capped, not overrun --\n")

long_config <- role4_config(enabled = 1, icu_days = CHECK_ICU_DAYS_LONG)
long_assigned <- assign_role4_los(cohort, long_config)
long_phases <- role4_ward_phases(long_assigned, long_config)

report(all(abs(long_assigned$r4_icu_days - long_assigned$los_days) < TOL),
       "a requirement beyond the stay fills it exactly rather than exceeding it")
report(all(long_phases$phase_ward == "ICU"),
       "no step-down phase is emitted where the stay is wholly intensive care")
long_totals <- phase_totals(long_assigned, long_phases)
report(all(long_totals$phase_days == long_totals$stay_days),
       "the capped stay still conserves its length")

# ── 5. Malformed configuration is rejected at the boundary ───────────────────

cat("\n-- malformed configuration is rejected naming its field --\n")

#' Assert that a configuration is rejected with a message naming a field
#'
#' @param config The Role 4 configuration under test.
#' @param needle Text the rejection message must contain.
#' @param label Description of the malformation, for the assertion line.
#' @return Invisible NULL.
expect_rejected <- function(config, needle, label) {
  result <- try(validate_role4_wards(config), silent = TRUE)
  rejected <- inherits(result, "try-error")
  named <- rejected && grepl(needle, conditionMessage(attr(result, "condition")),
                             fixed = TRUE)
  report(named, "%s is rejected with a message naming '%s'", label, needle)
  invisible(NULL)
}

report(isTRUE(validate_role4_wards(role4_config(enabled = 1))),
       "a well-formed configuration validates")

bad_ward <- role4_config()
bad_ward$wards$p2 <- "Recovery Ward"
expect_rejected(bad_ward, "role4.wards.p2", "a ward naming no configured level")

bad_enabled <- role4_config()
bad_enabled$icu_continuation$enabled <- 2
expect_rejected(bad_enabled, "role4.icu_continuation.enabled",
                "an enabled flag that is neither 0 nor 1")

bad_order <- role4_config(enabled = 1, icu_days = list(min = 20, mode = 8, max = 3))
expect_rejected(bad_order, "min <= mode <= max", "a requirement with min above max")

bad_degenerate <- role4_config(enabled = 1,
                               icu_days = list(min = 5, mode = 5, max = 5))
expect_rejected(bad_degenerate, "draws no distribution",
                "a requirement whose bounds coincide")

bad_step <- role4_config(enabled = 1, step_down = "Recovery Ward")
expect_rejected(bad_step, "role4.icu_continuation.step_down_ward",
                "a step-down ward naming no configured level")

bad_levels <- role4_config()
bad_levels$wards$levels <- list("ICU", "ICU", "General Ward")
expect_rejected(bad_levels, "names a ward twice", "a duplicated ward level")

# A negative requirement is rejected before it can produce negative bed-days.
bad_negative <- role4_config(enabled = 1,
                             icu_days = list(min = -1, mode = 8, max = 20))
expect_rejected(bad_negative, "non-negative", "a negative requirement bound")

# ── 6. The census counts what the phases say ─────────────────────────────────

cat("\n-- the census counts the phases rather than the admission ward --\n")

census_off <- compute_role4_census(cohort, role4_config(enabled = 0))
census_on <- compute_role4_census(cohort, on_config)

report(setequal(unique(census_off$ward), "ICU"),
       "the disabled census reports the admission ward alone (%s)",
       paste(unique(census_off$ward), collapse = ", "))
report(setequal(unique(census_on$ward), c("ICU", "Surgical Ward")),
       "the enabled census reports both phases' wards (%s)",
       paste(sort(unique(census_on$ward)), collapse = ", "))
report(sum(census_off$occupancy) == sum(census_on$occupancy),
       "both censuses count the same bed-days (%d against %d)",
       sum(census_off$occupancy), sum(census_on$occupancy))

icu_off <- sum(census_off$occupancy[census_off$ward == "ICU"])
icu_on <- sum(census_on$occupancy[census_on$ward == "ICU"])
report(icu_on < icu_off,
       "the split moves bed-days out of intensive care (%d against %d)",
       icu_on, icu_off)

# ── 7. Demand is measured against a stated establishment ─────────────────────

cat("\n-- demand is measured against a stated establishment --\n")

report(all(is.na(role4_capacity(shipped))),
       "every ward ships with no stated establishment, so nothing is bounded")

# A census whose answers are computable by hand, so a shortfall that agreed
# with the model but not with arithmetic would still fail.
hand_census <- data.frame(replication = 1L, day = 1:10, ward = "ICU",
                          occupancy = c(1, 2, 12, 14, 9, 10, 11, 3, 2, 1))
unlimited <- c(ICU = NA_real_, `Surgical Ward` = NA_real_, `General Ward` = NA_real_)
report(nrow(role4_capacity_shortfall(hand_census, unlimited)) == 0,
       "an unlimited establishment reports no shortfall at all")

bounded <- unlimited
bounded[["ICU"]] <- 10
measured <- role4_capacity_shortfall(hand_census, bounded)
report(nrow(measured) == 1 && measured$days_above == 3,
       "three days stand above an establishment of 10 (found %s)",
       if (nrow(measured) == 1) format(measured$days_above) else "no row")
report(nrow(measured) == 1 && abs(measured$peak_overshoot - 4) < TOL,
       "the peak overshoot is 4 beds (found %s)",
       if (nrow(measured) == 1) format(measured$peak_overshoot) else "no row")
report(nrow(measured) == 1 && abs(measured$unmet_bed_days - 7) < TOL,
       "unmet demand is 7 bed-days, the sum of the shortfalls (found %s)",
       if (nrow(measured) == 1) format(measured$unmet_bed_days) else "no row")

# Summing the shortfall rather than counting its days is what distinguishes a
# brief deep shortfall from a long shallow one, which are different problems.
deep <- data.frame(replication = 1L, day = 1L, ward = "ICU", occupancy = 30)
shallow <- data.frame(replication = 1L, day = 1:20, ward = "ICU", occupancy = 11)
deep_measured <- role4_capacity_shortfall(deep, bounded)
shallow_measured <- role4_capacity_shortfall(shallow, bounded)
report(deep_measured$days_above == 1 && shallow_measured$days_above == 20 &&
         deep_measured$unmet_bed_days == 20 && shallow_measured$unmet_bed_days == 20,
       "a deep one-day shortfall and a shallow twenty-day one are distinguishable")

# A ward stating no establishment is omitted rather than reported as meeting
# its demand, which would read as a pass it was never measured for.
mixed_census <- rbind(hand_census,
                      data.frame(replication = 1L, day = 1:10,
                                 ward = "Surgical Ward", occupancy = 99))
mixed <- role4_capacity_shortfall(mixed_census, bounded)
report(setequal(unique(mixed$ward), "ICU"),
       "a ward with no stated establishment is omitted rather than passed (%s)",
       paste(unique(mixed$ward), collapse = ", "))

bad_capacity <- role4_config()
bad_capacity$capacity <- list(wards = list("ICU"), beds = list(-5))
#' Assert that a capacity block is rejected with a message naming a field
#'
#' @param config The Role 4 configuration under test.
#' @param needle Text the rejection message must contain.
#' @param label Description of the malformation, for the assertion line.
#' @return Invisible NULL.
expect_rejected2 <- function(config, needle, label) {
  result <- try(validate_role4_capacity(config), silent = TRUE)
  rejected <- inherits(result, "try-error")
  named <- rejected && grepl(needle, conditionMessage(attr(result, "condition")),
                             fixed = TRUE)
  report(named, "%s is rejected with a message naming '%s'", label, needle)
  invisible(NULL)
}
expect_rejected2(bad_capacity, "role4.capacity.beds", "a negative establishment")

bad_unknown <- role4_config()
bad_unknown$capacity <- list(wards = list("Recovery Ward"), beds = list(10))
expect_rejected2(bad_unknown, "role4.capacity.wards",
                 "an establishment for a ward that does not exist")

bad_length <- role4_config()
bad_length$capacity <- list(wards = list("ICU", "General Ward"), beds = list(10))
expect_rejected2(bad_length, "differ in length",
                 "a ward list longer than its bed list")

null_capacity <- role4_config()
null_capacity$capacity <- list(wards = list("ICU"), beds = list(NULL))
report(isTRUE(validate_role4_capacity(null_capacity)) &&
         all(is.na(role4_capacity(null_capacity))),
       "a null establishment validates and reads as unlimited")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All Role 4 ward phase checks passed.\n")
quit(status = 0)
