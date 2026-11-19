##############################################################################
## R/post_op_capacity_sweep.R                                               ##
## Added R2E surgery crossed with R2E intensive care and holding beds       ##
##############################################################################
#
# The theatre sweep (R/ot_bed_sweep.R) shows that added R2E surgical capacity
# at high intensity moves load onto the intensive care and holding beds behind
# it, and the intensive care bed sweep (R/icu_bed_sweep.R) adds beds at the
# shipped surgical establishment, but no experiment adds surgery and beds
# together. This module does: surgery is held at six R2E sections and three
# theatres, the theatre sweep's arm that relieves the theatre queue without the
# idle rooms of four, and crossed with the R2E intensive care beds
# (elms.r2eheavy.beds.icu.qty) and holding beds (elms.r2eheavy.beds.hold.qty).
# The shipped establishment is carried as an arm so every arm can be read
# against what a planner fields today.
#
# All four counts are structure in `elms`, from which build_environment()
# constructs the resources, so they are set on the resolved configuration
# before that call with the setters the earlier sweeps own, and no trajectory
# changes.
#
# Each replication is reduced inside the forked worker that produced it by
# reduce_ot_bed_replication(), so every response and daily pool series is the
# theatre sweep's and the two experiments can be read side by side. The arm at
# six sections, three theatres, four intensive care beds and 30 holding beds is
# the theatre sweep's "3 theatres, 6 sections" arm and the shipped arm its
# shipped arm, under the same seed, so both reproduce that sweep's replications.
#
# Depends on R/analysis.R, R/policy_sweep.R, R/hold_window.R, R/long_horizon.R,
# R/surgical_team_sweep.R, R/icu_bed_sweep.R and R/ot_bed_sweep.R; source those
# first.

#' Replications per arm
POST_OP_SWEEP_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
POST_OP_SWEEP_DAYS <- 360L

#' Control seed every arm is set to before it runs
POST_OP_SWEEP_SEED <- 42L

#' Closing window the queue and occupancy responses are measured over, in days
POST_OP_SWEEP_WINDOW_DAYS <- 90L

#' Scenario the protocol is run under
#'
#' @details The shift of load onto the post-operative beds is measured at high
#'   intensity alone; at moderate intensity the theatres already keep pace and
#'   the shift is not resolved by the theatre sweep.
POST_OP_SWEEP_SCENARIO <- "high_intensity"

#' R2E intensive care bed counts crossed with the holding beds
POST_OP_SWEEP_ICU_BEDS <- c(4L, 8L, 12L)

#' R2E holding bed counts crossed with the intensive care beds
POST_OP_SWEEP_HOLD_BEDS <- c(30L, 60L, 90L)

#' Arm label of a surgical and bed establishment, as the output files name it
#'
#' @param theatres R2E operating theatres.
#' @param sections R2E surgical sections.
#' @param icu_beds R2E intensive care beds.
#' @param hold_beds R2E holding beds.
#' @return The label, such as "6 sections, 3 theatres, 8 ICU, 45 hold".
post_op_arm_label <- function(theatres, sections, icu_beds, hold_beds) {
  sprintf("%d sections, %d theatres, %d ICU, %d hold", sections, theatres, icu_beds,
          hold_beds)
}

#' The arms swept, as an R2E surgical and bed establishment
#'
#' @details The shipped establishment first, then the added surgery crossed
#'   with every intensive care and holding bed count, intensive care varying
#'   slowest, the order the results table reads them in.
POST_OP_SWEEP_ARMS <- local({
  grid <- expand.grid(hold_beds = POST_OP_SWEEP_HOLD_BEDS, icu_beds = POST_OP_SWEEP_ICU_BEDS)
  arms <- data.frame(theatres = c(2L, rep(3L, nrow(grid))),
                     sections = c(3L, rep(6L, nrow(grid))),
                     icu_beds = c(4L, grid$icu_beds),
                     hold_beds = c(30L, grid$hold_beds))
  arms$arm <- post_op_arm_label(arms$theatres, arms$sections, arms$icu_beds, arms$hold_beds)
  arms[, c("arm", "theatres", "sections", "icu_beds", "hold_beds")]
})

#' Arm of the shipped establishment
POST_OP_SWEEP_BASELINE_ARM <- post_op_arm_label(2L, 3L, 4L, 30L)

#' Arm of the added surgery alone, at the shipped beds
POST_OP_SWEEP_SURGERY_ARM <- post_op_arm_label(3L, 6L, 4L, 30L)

#' The paired comparisons reported, as a from and to arm
#'
#' @details Every arm against the shipped establishment, which prices the whole
#'   change, and every bed arm against the added surgery alone, which isolates
#'   what the beds buy once the surgery is in place. Holm's adjustment is taken
#'   over every row of the file.
POST_OP_SWEEP_COMPARISONS <- local({
  bed_arms <- setdiff(POST_OP_SWEEP_ARMS$arm, c(POST_OP_SWEEP_BASELINE_ARM,
                                                POST_OP_SWEEP_SURGERY_ARM))
  rbind(
    data.frame(from = POST_OP_SWEEP_BASELINE_ARM,
               to = setdiff(POST_OP_SWEEP_ARMS$arm, POST_OP_SWEEP_BASELINE_ARM),
               stringsAsFactors = FALSE),
    data.frame(from = POST_OP_SWEEP_SURGERY_ARM, to = bed_arms, stringsAsFactors = FALSE)
  )
})

#' Responses the paired comparisons report
#'
#' @details The theatre sweep's paired responses with the holding occupancy,
#'   the beds being swept here.
POST_OP_SWEEP_PAIRED_RESPONSES <- c(OT_SWEEP_PAIRED_RESPONSES, "hold_occupancy")

#' Half-width each paired response is sized against, in its own units
#'
#' @details The theatre sweep's, with a percentage point on the holding
#'   occupancy as on every other occupancy.
POST_OP_SWEEP_HALF_WIDTHS <- c(OT_SWEEP_HALF_WIDTHS, hold_occupancy = 0.01)

#' Enter a scenario's configuration with the R2E surgery and beds overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param arm One row of POST_OP_SWEEP_ARMS.
#' @return Invisibly, the described configuration that was applied.
#'
#' @details Each count is set by the setter of the sweep that owns it, each of
#'   which fails rather than returning the configuration unchanged, so an arm
#'   cannot silently measure the shipped establishment. R2B keeps its shipped
#'   section count.
apply_post_op_setting <- function(json_data, scenario, arm) {
  resolved <- resolve_scenario(json_data, scenario)
  resolved <- set_ot_establishment(resolved, arm$theatres)
  resolved <- set_surgical_sections(resolved, "r2eheavy", arm$sections)
  resolved <- set_icu_establishment(resolved, arm$icu_beds)
  resolved <- set_hold_establishment(resolved, arm$hold_beds)
  described <- build_environment(resolved)
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}
