##############################################
## R/constants.R                            ##
## Values shared by every module            ##
##############################################
#
# Sourced by each module that needs one of these rather than by one module on
# every other's behalf, because the modules under R/ are otherwise independent:
# an entry point sources the set it needs, and a regression check often sources
# only one. Sourcing this file more than once is harmless.

# Minutes in a simulated day. This is the single definition of the quantity.
# The `day_min` global that the execution model carries (see CLAUDE.md's Code
# Standards) is assigned from it by every entry point; use that global inside
# the model and the analysis pipeline, and this constant where no entry point
# has run yet, or in a parameter default, which cannot name a global of its own
# name without resolving to the parameter itself.
DAY_MIN <- 1440L

# Every per-casualty attribute key the model can set. `build_attributes_wide()`
# (R/analysis.R) guarantees one column per key, all-NA where no casualty set it,
# so that the pivot of the attributes monitor has the same shape whatever a run
# happened to produce. Without that guarantee the shape depends on the run: a
# campaign in which nobody returned to duty has no `return_day` column at all,
# and analysis code referring to one fails with an object-not-found error rather
# than reading an empty result. The list is the model's, not the analysis
# pipeline's, which is why it lives here; scripts/check_attribute_keys.R asserts
# it against the set_attribute() calls in R/trajectories.R so the two cannot
# drift.
MODEL_ATTRIBUTE_KEYS <- c(
  "ame_departure_time", "ame_icu_hold", "ame_icu_hold_minutes", "ame_icu_hold_start",
  "ame_route", "ame_wait_minutes", "dcs_pathway", "dnbi_bf_hold", "dnbi_type", "dow",
  "dow_ceiling", "dow_echelon", "evacuation_day", "evacuation_decision_day", "injury_time",
  "injury_type", "last_dow_t", "mass_casualty_event", "mass_casualty_event_id",
  "mortuary_treated", "post_definitive_min", "post_definitive_pathway", "post_op_pathway",
  "priority", "r1_treated", "r2b", "r2b_bypass_reason", "r2b_bypass_time", "r2b_bypassed",
  "r2b_departure_time", "r2b_hold_bypass", "r2b_hold_drawn", "r2b_hold_evac", "r2b_hold_queued",
  "r2b_hold_residual", "r2b_hold_served", "r2b_hold_start", "r2b_post_op_min",
  "r2b_post_op_pathway", "r2b_pre_open_start", "r2b_pre_open_wait", "r2b_pre_open_wait_min",
  "r2b_r2e_mortuary_transport_start", "r2b_resus", "r2b_surgery", "r2b_surgery_end",
  "r2b_surgery_start", "r2b_to_r2e", "r2b_treated", "r2b_treatment_start_time", "r2e",
  "r2e_arrival_time", "r2e_departure_time", "r2e_evac", "r2e_handling", "r2e_post_op_min",
  "r2e_resus", "r2e_surgery", "r2e_surgery_1_end", "r2e_surgery_1_start", "r2e_surgery_2_end",
  "r2e_surgery_2_start", "r2e_treated", "recovery_to_duty_days", "reinf_combat_demand",
  "reinf_combat_fill", "reinf_support_demand", "reinf_support_fill", "return_day",
  "return_echelon", "stabilisation_total", "surgery", "surgery_deferred", "team",
  "transport_start_time", "treatment_received", "treatment_start_time"
)
