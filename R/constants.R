##############################################
## R/constants.R                            ##
## Values shared by every module            ##
##############################################
#
# Sourced by each module that needs one of these rather than by one module on
# every other's behalf, because the modules under R/ are otherwise independent:
# an entry point sources the set it needs, and a regression check often sources
# only one. Sourcing this file more than once is harmless.

#' Minutes in a simulated day
#'
#' @details This is the single definition of the quantity. The `day_min` global
#'   that the execution model carries (see CLAUDE.md's Code Standards) is
#'   assigned from it by every entry point; use that global inside the model and
#'   the analysis pipeline, and this constant where no entry point has run yet,
#'   or in a parameter default, which cannot name a global of its own name
#'   without resolving to the parameter itself.
DAY_MIN <- 1440L

#' Every per-casualty attribute key the model can set
#'
#' @details `build_attributes_wide()` (R/analysis.R) guarantees one column per
#'   key, all-NA where no casualty set it, so that the pivot of the attributes monitor has
#'   the same shape whatever a run happened to produce. Without that guarantee
#'   the shape depends on the run: a campaign in which nobody returned to duty
#'   has no `return_day` column at all, and analysis code referring to one fails
#'   with an object-not-found error rather than reading an empty result. The list
#'   is the model's, not the analysis pipeline's, which is why it lives here;
#'   tests/checks/check_absent_attribute_columns.R asserts it against the
#'   set_attribute() calls in R/trajectories.R so the two cannot drift.
MODEL_ATTRIBUTE_KEYS <- c(
  "ame_death_time", "ame_departure_time", "ame_hold_end", "ame_hold_start", "ame_icu_hold",
  "ame_icu_hold_minutes",
  "ame_icu_hold_start", "ame_preflight_min", "ame_route", "ame_stage_start",
  "ame_wait_minutes", "bed_budget", "bed_retained_t",
  "bed_waited", "bf_restoration", "dcs_pathway",
  "definitive_repair_minutes", "definitive_repair_outstanding",
  "dnbi_type", "dow", "dow_care", "dow_ceiling", "dow_check_p", "dow_echelon",
  "dow_expected_early", "dow_expected_late", "dow_mult", "dow_mult_t", "dow_phase",
  "dow_risk_early", "dow_risk_late", "evacuation_day", "evacuation_decision_day",
  "evacuation_reason",
  "injury_time", "injury_type", "last_dow_t", "casualty_surge_event", "casualty_surge_event_id",
  "mortuary_treated", "overflow_since", "post_definitive_hold_start",
  "post_definitive_start", "post_definitive_min",
  "post_definitive_pathway", "post_definitive_total", "post_op_pathway", "priority", "r1_treated", "r2b",
  "r2b_bypass_reason", "r2b_bypass_time", "r2b_bypassed", "r2b_departure_time", "r2b_hold_bypass",
  "r2b_hold_drawn", "r2b_hold_evac", "r2b_hold_ineligible", "r2b_hold_queued",
  "r2b_hold_residual", "r2b_hold_served", "r2b_hold_waited",
  "r2b_hold_start", "r2b_post_op_min", "r2b_post_op_pathway", "r2b_pre_open_start",
  "r2b_pre_open_wait", "r2b_pre_open_wait_min", "r2b_r2e_mortuary_transport_start",
  "r2b_reception_bed", "r2b_reception_wait", "r2b_resus",
  "r2b_surgery", "r2b_surgery_end", "r2b_surgery_start", "r2b_to_r2e", "r2b_treated",
  "r2b_treatment_start_time", "r2e", "r2e_arrival_time", "r2e_departure_time", "r2e_evac",
  "r2e_handling", "r2e_overflow_min", "r2e_post_op_hold_start", "r2e_post_op_min",
  "r2e_post_op_start",
  "r2e_preflight_bed",
  "r2e_recovery_hold_min", "r2e_recovery_hold_start", "r2e_resus", "r2e_surgery", "r2e_surgery_1_end",
  "r2e_stage_bed", "r2e_surgery_1_start", "r2e_surgery_2_end", "r2e_surgery_2_start",
  "r2e_treated",
  "reconstruction_required", "recovery_to_duty_days", "reinf_combat_demand",
  "reinf_combat_fill", "reinf_support_demand",
  "reinf_support_fill", "return_day", "return_echelon", "rtd_while_queued",
  "stabilisation_total", "surgery",
  "surgery_deferred", "team", "transport_start_time", "treatment_received",
  "treatment_start_time"
)

#' Points of care where a casualty is placed in overflow when no bed is free
#'
#' @details Overflow is where a casualty is cared for when neither the bed its
#'   care calls for nor a holding bed is free. Each point of care where the model
#'   places a casualty rather than queueing it for a bed has its own unlimited
#'   resource per team, so the resource monitor's server count for it is the
#'   overflow census at that point and no casualty ever queues there. The points
#'   are named by echelon (the configuration's r2b and r2eheavy elements, written
#'   r2b and r2e here).
OVERFLOW_POINTS <- list(
  r2b = "reception",
  r2e = c("post_op", "pre_flight", "staging")
)

#' Name format of one overflow resource: echelon, point of care, team index
OVERFLOW_RESOURCE_FMT <- "overflow_%s_%s_%d"

#' Name of the overflow resource at one point of care of one team
#'
#' @param echelon "r2b" or "r2e".
#' @param point One of that echelon's entries in `OVERFLOW_POINTS`.
#' @param team_id Integer index of the team.
#' @return The resource name.
overflow_resource <- function(echelon, point, team_id) {
  if (!echelon %in% names(OVERFLOW_POINTS) || !point %in% OVERFLOW_POINTS[[echelon]]) {
    stop(sprintf("overflow_resource: no overflow point '%s' at echelon '%s'", point, echelon),
         call. = FALSE)
  }
  sprintf(OVERFLOW_RESOURCE_FMT, echelon, point, as.integer(team_id))
}
