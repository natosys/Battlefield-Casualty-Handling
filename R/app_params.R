##############################################
## R/app_params.R                           ##
## Parameter registry for the Shiny         ##
## configuration editor (Issue #14)         ##
##############################################
#
# env_data.json is manipulated here as the *raw* parsed JSON tree (the
# on-disk schema: elms/actys/vals arrays, as returned by
# jsonlite::fromJSON(path, simplifyVector = FALSE)) rather than the *built*
# nested-list structure produced by build_environment() that the simulation
# engine consumes. Editing the raw tree means Save Configuration can write
# it back to env_data.json unchanged in shape; build_environment() converts
# a (possibly edited, in-memory) copy to the engine's structure immediately
# before each Quick Run.
#
# Assumes morris_params (R/sensitivity.R) is already loaded in the calling
# session, so Morris-screened fields can borrow their lower/upper bounds.

# ── Raw JSON accessors ───────────────────────────────────────────────────

#' Read a single scalar value from the vars$elm$acty$vals tree
get_raw_var <- function(json, elm, acty, var) {
  for (e in json$vars) {
    if (identical(e$elm, elm)) {
      for (a in e$actys) {
        if (identical(a$acty, acty)) {
          for (v in a$vals) if (identical(v$var, var)) return(v$val)
        }
      }
    }
  }
  NULL
}

#' Write a single scalar value into the vars$elm$acty$vals tree
set_raw_var <- function(json, elm, acty, var, value) {
  for (ei in seq_along(json$vars)) {
    if (identical(json$vars[[ei]]$elm, elm)) {
      for (ai in seq_along(json$vars[[ei]]$actys)) {
        if (identical(json$vars[[ei]]$actys[[ai]]$acty, acty)) {
          for (vi in seq_along(json$vars[[ei]]$actys[[ai]]$vals)) {
            if (identical(json$vars[[ei]]$actys[[ai]]$vals[[vi]]$var, var)) {
              json$vars[[ei]]$actys[[ai]]$vals[[vi]]$val <- value
              return(json)
            }
          }
        }
      }
    }
  }
  stop(sprintf("set_raw_var: path not found elm=%s acty=%s var=%s", elm, acty, var))
}

get_pop_count <- function(json, name) {
  for (p in json$pops) if (identical(p$name, name)) return(p$count)
  NULL
}
set_pop_count <- function(json, name, value) {
  for (i in seq_along(json$pops)) {
    if (identical(json$pops[[i]]$name, name)) { json$pops[[i]]$count <- value; return(json) }
  }
  stop(sprintf("set_pop_count: population not found: %s", name))
}

get_elm_qty <- function(json, elm) {
  for (e in json$elms) if (identical(e$elm, elm)) return(e$qty)
  NULL
}
set_elm_qty <- function(json, elm, value) {
  for (i in seq_along(json$elms)) {
    if (identical(json$elms[[i]]$elm, elm)) { json$elms[[i]]$qty <- value; return(json) }
  }
  stop(sprintf("set_elm_qty: element not found: %s", elm))
}

get_bed_qty <- function(json, elm, bed_name) {
  for (e in json$elms) {
    if (identical(e$elm, elm)) {
      for (b in e$beds) if (identical(b$name, bed_name)) return(b$qty)
    }
  }
  NULL
}
set_bed_qty <- function(json, elm, bed_name, value) {
  for (ei in seq_along(json$elms)) {
    if (identical(json$elms[[ei]]$elm, elm)) {
      for (bi in seq_along(json$elms[[ei]]$beds)) {
        if (identical(json$elms[[ei]]$beds[[bi]]$name, bed_name)) {
          json$elms[[ei]]$beds[[bi]]$qty <- value
          return(json)
        }
      }
    }
  }
  stop(sprintf("set_bed_qty: bed not found: %s/%s", elm, bed_name))
}

get_transport_field <- function(json, name, field) {
  for (t in json$transports) if (identical(t$name, name)) return(t[[field]])
  NULL
}
set_transport_field <- function(json, name, field, value) {
  for (i in seq_along(json$transports)) {
    if (identical(json$transports[[i]]$name, name)) { json$transports[[i]][[field]] <- value; return(json) }
  }
  stop(sprintf("set_transport_field: transport not found: %s", name))
}

#' Read one indexed slot of a mass_casualty.schedule array field (days or
#' probabilities), defaulting missing/short-array slots rather than erroring
#'
#' @param json Parsed env_data.json (raw tree)
#' @param var "days" or "probabilities" (mass_casualty.schedule.<var>)
#' @param index 1-indexed slot position (1..MASS_CASUALTY_SCHEDULE_SLOTS)
#' @param default_fill Value to return when the underlying array is shorter
#'   than `index` — 0 for "days" (an unused slot), 1 for "probabilities"
#'   (always fires if the day is set)
get_mass_casualty_schedule_slot <- function(json, var, index, default_fill) {
  arr <- unlist(get_raw_var(json, "mass_casualty", "schedule", var))
  if (length(arr) >= index) arr[index] else default_fill
}

#' Write one indexed slot of a mass_casualty.schedule array field, padding
#' any shorter existing array up to MASS_CASUALTY_SCHEDULE_SLOTS with
#' `default_fill` first so every slot's set() independently produces a
#' full-length array regardless of application order (see
#' apply_registry_values(), which calls every registered field's set() once)
set_mass_casualty_schedule_slot <- function(json, var, index, value, default_fill) {
  arr <- unlist(get_raw_var(json, "mass_casualty", "schedule", var))
  if (length(arr) < MASS_CASUALTY_SCHEDULE_SLOTS) {
    arr <- c(arr, rep(default_fill, MASS_CASUALTY_SCHEDULE_SLOTS - length(arr)))
  }
  arr[index] <- value
  set_raw_var(json, "mass_casualty", "schedule", var, arr)
}

# ── Registry group names ─────────────────────────────────────────────────

GRP_FORCE        <- "Force Size"
GRP_HEALTH_ARCH  <- "Health System Architecture"
GRP_LOGISTICS    <- "Medevac"
GRP_PROVISION    <- "Health Provision"
GRP_CASUALTY     <- "Casualty Rates"
GRP_MASS_CASUALTY <- "Mass Casualty"

# Maximum candidate event slots the registry supports for the Configure
# panel's Scheduled Event Days grid (mass_casualty.schedule.*). The
# registry itself is still a fixed-size list (this project's field-registry
# architecture — Save/Load/Quick-Run/validation all read a static list, see
# R/app_params.R header comment), but app.R's "+ Add Event"/"− Remove Last
# Event" controls only reveal/hide rows up to this cap, giving the
# Configure panel's own UX a dynamic-list feel without a deeper
# architectural change. A slot's Day field left at 0 is treated as unused
# (see get_mass_casualty_schedule_slot()/generate_mass_casualty_events() in
# R/environment.R, which drops any event start time outside the simulation
# window) — app.R also explicitly resets a slot's fields to their defaults
# when its row is hidden via "− Remove Last Event", so a removed event
# cannot silently keep firing in the background. Edit env_data.json
# directly for more than this many explicit events.
MASS_CASUALTY_SCHEDULE_SLOTS <- 20

# ── Field constructors ───────────────────────────────────────────────────

# Fallback disclosure for any field not given an explicit `source=` —
# every field must state its provenance one way or another, consistent
# with this project's Assumption Handling standard (CLAUDE.md): a value
# is either cited, or explicitly flagged as an uncited informed estimate.
SRC_UNCITED <- "Not independently cited — informed estimate. See README (Simulation Design / Limitations) for the design rationale."

# ── Citation constants (mirrors README numbered references where cited) ──
SRC_FORECAS_WIA_CBT   <- "FORECAS (Blood, Zouris & Rotblatt, 1998), Falklands combat-troop WIA rate, Table A.8 p.32."
SRC_FORECAS_KIA_CBT   <- "FORECAS (Blood, Zouris & Rotblatt, 1998), Falklands combat-troop KIA rate, Table A.8 p.32."
SRC_FORECAS_DNBI_CBT  <- "FORECAS (Blood, Zouris & Rotblatt, 1998), Vietnam combat-troop DNBI rate, Table A.5 p.31."
SRC_FORECAS_DNBI_SPT  <- "FORECAS (Blood, Zouris & Rotblatt, 1998), Okinawa support-troop DNBI rate, Table A.2 p.29."
SRC_SUPPORT_INCLUSION <- "Same distribution as the combat stream, applied to the support population; support-troop inclusion practice follows Izaguirre et al. (2025) and FORECAS (Blood, Zouris & Rotblatt, 1998) pp.2-4."
SRC_PRIORITY_SPLIT    <- "ADF operational planning norms (not open-access). NATO AJP-4.10 establishes the triage framework but not specific percentages. High uncertainty — see README Casualty Priorities."
SRC_DNBI_BF_PCT       <- "Izaguirre et al. (2025), historical LSCO psychiatric/battle-fatigue evacuation proportion (~25-30% of DNBI). Medium uncertainty."
SRC_DNBI_DISEASE_PCT  <- "Residual estimate (100% minus the NBI and battle-fatigue shares); no direct empirical source identified. High uncertainty — see README DNBI Sub-Categorisation."
SRC_DNBI_NBI_PCT      <- "FORECAS (Blood, Zouris & Rotblatt, 1998) empirical data, pp.22-23."
SRC_DISEASE_SURGERY   <- "Informed estimate from population-level appendicitis/cholecystitis/perforated-ulcer incidence in military-age males; see README DNBI Sub-Categorisation. High uncertainty."
SRC_EVAC_CANDIDACY    <- "ADF operational planning norms (not open-access), paired with the Priority 1/2/3 split. High uncertainty — see README Casualty Priorities."
SRC_DOW_CEILING       <- "Calibrated to the Falklands War 1982 DOW/WIA rate of 0.52% (Payne, 1983; Jolly, 2018) via 50-replication Monte Carlo; entangled with the OIF/OEF-era treatment efficacy factors below it. See README Died of Wounds."
SRC_DOW_SHAPE         <- "Logistic shape (k, t_mid) anchored to the haemorrhagic shock critical window, Eastridge et al. (2012) and Kotwal et al. (2011). Not user-editable in this app. See README Died of Wounds — Survival Function."
SRC_R1_WIA_TREAT      <- "README Core Trajectory (cited source for R1 treatment duration)."
SRC_R1_RECOVERY       <- "Field estimates of minor injury convalescence — see README Core Trajectory. Not independently cited."
SRC_ESTABLISHMENT     <- "Establishment/staffing planning assumption for a brigade-level ADF deployment; not independently cited. See docs/BCH_Task_Role_Allocation.md (Issue #4) for a proposed evidence-based staffing revision (not yet implemented)."
SRC_RESUS_TASK_TABLE  <- "Derived from a collated task-duration table for the likely resuscitation steps (see README R2B/R2E Trajectory); constrained to complete within 90 minutes per the cited source there. Not independently cited as a single total."
SRC_DCS_SURGERY       <- "First-look DCS operative-time data (median 96 min, range 41-210) reported for Sohn et al. (2018) within Zizzo et al.'s (2020) systematic review — see README R2B Trajectory for citations."
SRC_ICU_STABILISATION <- "Post-DCS-I stabilisation window described as 24-36h in cited DCS research — see README R2E Trajectory."
SRC_TRANSPORT_GENERIC <- "Informed estimate of transport duration between echelons; not independently cited. See README Simulation Design for the triangular-distribution modelling rationale."
SRC_HOLD_THRESHOLD    <- "Design threshold introduced by Issue #39 (R2B hold-bed saturation routing policy); not literature-derived."
SRC_ICU_GATING        <- "Design parameter introduced by Issue #43 (OT-ICU gating); not literature-derived."
SRC_POST_OP_HOLD      <- "Informed estimate (Issue #43); no open-access source quantifies a ward-vs-ICU post-operative recovery duration for this patient population. See README Limitations (L11)."
SRC_FORCE_REGEN       <- "Planner-configured reinforcement demand/fulfillment model introduced by Issue #18 (endogenous casualty generation / force regeneration feedback loop); not literature-derived — this project does not attempt to auto-balance the demand cycle or fill distribution against a scenario's observed attrition rate. See README Force Regeneration and the Endogenous Feedback Loop."
SRC_IN_THEATRE_RATE   <- "Derived from Vietnam-era return-to-duty data (~31% RTD, ~42% in-theatre, implying ~13% in-theatre recovery) — see README R2E Heavy Trajectory."
SRC_VEHICLE_CAPACITY  <- "Real-world vehicle specification (see README Transport Assets); fleet size is a planning assumption, not independently cited."
SRC_MASS_CASUALTY     <- "Issue #9 Recommended Approach, informed by the compound Poisson parameterisation of Fischer et al. (2025) and blast-dominant LSCO injury context; no open-access source tabulates event-level MASCAL rate/size distributions, so these are informed engineering estimates, not literature-calibrated values. See README Casualty Generation — Mass Casualty Event Injection."
SRC_MASS_CASUALTY_PRI <- "Issue #9 Recommended Approach (blast-dominant injury pattern, ~70% blast/fragmentation share in contemporary LSCO); informed engineering estimate, independent of the background Triage Priority Split above. See README Casualty Generation — Mass Casualty Event Injection."
SRC_AME_SCHEDULE      <- "Issue #23 follow-up. AJP-4.10(B) establishes strategic AME, Casualty Staging Unit (CSU) patient holding, and CCATT/CCAST critical-care augmentation as planning functions but does not prescribe a specific sortie cadence, failure rate, or per-pool patient capacity — informed estimate. See README Role 4 (National Support Base) Demand Modelling."

#' Build a single field spec, optionally borrowing Morris screening bounds
#'
#' @param source One-sentence statement of where this field's baseline value
#'   comes from — a citation (matching the README's numbered references) or
#'   an explicit "informed estimate" disclosure. Defaults to SRC_UNCITED so
#'   no field is silently left without a provenance statement.
#' @param path "elm.acty" string identifying this field's location in
#'   env_data.json's vars tree (e.g. "generators.wia_cbt"), or NULL for
#'   fields outside that tree (force size, team/bed counts, transport —
#'   never scenario-overridden, see R/scenario.R). Lets app.R's Configure
#'   panel flag which fields the active Casualty Intensity Profile is
#'   currently overriding, without introspecting get()/set() closures.
#'
#' @details When morris_name matches a row in morris_params, the field's
#'   min/max are overridden with the screened lower/upper bounds and the
#'   tooltip gains a note naming the screened range, per Issue #14
#'   acceptance criteria ("parameters appearing in morris_params use
#'   lower/upper as slider bounds").
#' @param slider Force single-value slider rendering in the Configure panel
#'   even when this field is not Morris-screened (`morris_name = NULL`).
#'   Used for probability/rate fields where a slider communicates the 0–1
#'   bound more directly than a bare numeric box.
#' @param choices Fixed set of values (e.g. `1:3`), rendered as a dropdown
#'   `selectInput()` instead of a numeric input or slider. Used for small,
#'   discrete-valued fields (e.g. a priority level) where the field isn't a
#'   quantity or rate a bare number or slider communicates well.
field <- function(id, group, subgroup, label, tooltip, get, set,
                   type = "numeric", min = NA, max = NA, step = NA,
                   morris_name = NULL, source = SRC_UNCITED, path = NULL,
                   slider = FALSE, choices = NULL) {
  if (!is.null(morris_name)) {
    mp <- morris_params[morris_params$name == morris_name, ]
    if (nrow(mp) == 1) {
      min <- mp$lower
      max <- mp$upper
      tooltip <- paste0(
        tooltip,
        sprintf(" Screened in Morris sensitivity analysis (Issue #3); plausible range %s–%s.",
                format(mp$lower), format(mp$upper))
      )
    }
  }
  tooltip <- paste0(tooltip, " Source: ", source)
  list(id = id, group = group, subgroup = subgroup, label = label, tooltip = tooltip,
       get = get, set = set, type = type, min = min, max = max, step = step,
       morris = !is.null(morris_name), path = path, slider = isTRUE(slider), choices = choices)
}

#' Triangular-distribution (min/mode/max) triple of field specs
tri_fields <- function(id_prefix, group, subgroup, elm, acty, label, tooltip,
                        morris_mode_name = NULL, bound = c(0, 40000),
                        source = SRC_UNCITED) {
  path <- paste0(elm, ".", acty)
  list(
    field(paste0(id_prefix, "_min"), group, subgroup,
          paste0(label, " — Minimum"),
          paste0(tooltip, " Minimum duration (triangular distribution), minutes."),
          get = function(json) get_raw_var(json, elm, acty, "min"),
          set = function(json, v) set_raw_var(json, elm, acty, "min", v),
          type = "integer", min = bound[1], max = bound[2], step = 1, source = source, path = path),
    field(paste0(id_prefix, "_mode"), group, subgroup,
          paste0(label, " — Most Likely (Mode)"),
          paste0(tooltip, " Most likely duration (triangular distribution mode), minutes."),
          get = function(json) get_raw_var(json, elm, acty, "mode"),
          set = function(json, v) set_raw_var(json, elm, acty, "mode", v),
          type = "integer", min = bound[1], max = bound[2], step = 1,
          morris_name = morris_mode_name, source = source, path = path),
    field(paste0(id_prefix, "_max"), group, subgroup,
          paste0(label, " — Maximum"),
          paste0(tooltip, " Maximum duration (triangular distribution), minutes."),
          get = function(json) get_raw_var(json, elm, acty, "max"),
          set = function(json, v) set_raw_var(json, elm, acty, "max", v),
          type = "integer", min = bound[1], max = bound[2], step = 1, source = source, path = path)
  )
}

#' Single named var field within an elm/acty (e.g. a probability or rate)
#'
#' @details elm/acty/var are forced immediately (rather than left as lazy
#'   promises tied to the caller's environment) so this is safe to call from
#'   inside a `for` loop — R's lazy evaluation would otherwise mean every
#'   field's get()/set() closure resolves elm/acty/var to whatever the loop
#'   variable holds when the closure is *called*, not when it was *created*,
#'   silently making every field from the same loop resolve to the last
#'   iteration's path. (Found via a scenario-selector regression: all six
#'   casualty-generation streams were reading/writing generators.dnbi_spt.)
var_field <- function(id, group, subgroup, elm, acty, var, label, tooltip,
                       type = "numeric", min = 0, max = 1, step = 0.01,
                       morris_name = NULL, source = SRC_UNCITED, slider = FALSE,
                       choices = NULL) {
  force(elm); force(acty); force(var)
  field(id, group, subgroup, label, tooltip,
        get = function(json) get_raw_var(json, elm, acty, var),
        set = function(json, v) set_raw_var(json, elm, acty, var, v),
        type = type, min = min, max = max, step = step, morris_name = morris_name,
        source = source, path = paste0(elm, ".", acty), slider = slider, choices = choices)
}

#' One mass_casualty.schedule event slot's full field set: Day, Occurrence
#' Probability, Casualties per Event (min/max), and Priority Split
#' (one/two/three) — each event independently configurable
#'
#' @param index 1-indexed slot position (1..MASS_CASUALTY_SCHEDULE_SLOTS)
#' @return List of seven field specs: `mc_sched_day_<index>`,
#'   `mc_sched_prob_<index>`, `mc_sched_min_cas_<index>`,
#'   `mc_sched_max_cas_<index>`, `mc_sched_pri_one_<index>`,
#'   `mc_sched_pri_two_<index>`, `mc_sched_pri_three_<index>`. The three
#'   pri_* fields are registry-only (get/set); the Configure panel renders
#'   them as one range slider per event (`mc_event_pri_split_<index>`, see
#'   render_group_body() in app.R), matching the shared Mass Casualty
#'   Priority Split's own compositional-slider pattern.
mass_casualty_schedule_slot_fields <- function(index) {
  force(index)
  list(
    field(sprintf("mc_sched_day_%d", index), GRP_MASS_CASUALTY, "Scheduled Event Days",
          sprintf("Event %d — Simulation Day", index),
          "Simulation day (1-indexed) this candidate event may occur on. Set to 0 to leave this slot unused.",
          get = function(json) get_mass_casualty_schedule_slot(json, "days", index, 0),
          set = function(json, v) set_mass_casualty_schedule_slot(json, "days", index, v, 0),
          type = "integer", min = 0, max = 180, step = 1, source = SRC_MASS_CASUALTY,
          path = "mass_casualty.schedule"),
    field(sprintf("mc_sched_prob_%d", index), GRP_MASS_CASUALTY, "Scheduled Event Days",
          sprintf("Event %d — Occurrence Probability", index),
          "Probability this event actually fires on its configured day, drawn independently (Bernoulli) each replication — 1 always fires, a lower value introduces controlled replication-to-replication variation.",
          get = function(json) get_mass_casualty_schedule_slot(json, "probabilities", index, 1),
          set = function(json, v) set_mass_casualty_schedule_slot(json, "probabilities", index, v, 1),
          type = "numeric", min = 0, max = 1, step = 0.01, source = SRC_MASS_CASUALTY,
          path = "mass_casualty.schedule", slider = TRUE),
    field(sprintf("mc_sched_min_cas_%d", index), GRP_MASS_CASUALTY, "Scheduled Event Days",
          sprintf("Event %d — Casualties per Event (Minimum)", index),
          "Minimum number of casualties this event injects if it fires (Uniform distribution) — independent of every other event's casualty count.",
          get = function(json) get_mass_casualty_schedule_slot(json, "min_cas", index, 20),
          set = function(json, v) set_mass_casualty_schedule_slot(json, "min_cas", index, v, 20),
          type = "integer", min = 1, max = 500, step = 1, source = SRC_MASS_CASUALTY,
          path = "mass_casualty.schedule"),
    field(sprintf("mc_sched_max_cas_%d", index), GRP_MASS_CASUALTY, "Scheduled Event Days",
          sprintf("Event %d — Casualties per Event (Maximum)", index),
          "Maximum number of casualties this event injects if it fires (Uniform distribution) — independent of every other event's casualty count.",
          get = function(json) get_mass_casualty_schedule_slot(json, "max_cas", index, 60),
          set = function(json, v) set_mass_casualty_schedule_slot(json, "max_cas", index, v, 60),
          type = "integer", min = 1, max = 500, step = 1, source = SRC_MASS_CASUALTY,
          path = "mass_casualty.schedule"),
    field(sprintf("mc_sched_pri_one_%d", index), GRP_MASS_CASUALTY, "Scheduled Event Days",
          sprintf("Event %d — Priority 1 Share", index),
          "Proportion of this event's casualties triaged as Priority 1 — independent of every other event's priority mix.",
          get = function(json) get_mass_casualty_schedule_slot(json, "pri_one", index, 0.7),
          set = function(json, v) set_mass_casualty_schedule_slot(json, "pri_one", index, v, 0.7),
          type = "numeric", min = 0, max = 1, step = 0.01, source = SRC_MASS_CASUALTY_PRI,
          path = "mass_casualty.schedule"),
    field(sprintf("mc_sched_pri_two_%d", index), GRP_MASS_CASUALTY, "Scheduled Event Days",
          sprintf("Event %d — Priority 2 Share", index),
          "Proportion of this event's casualties triaged as Priority 2 — independent of every other event's priority mix.",
          get = function(json) get_mass_casualty_schedule_slot(json, "pri_two", index, 0.2),
          set = function(json, v) set_mass_casualty_schedule_slot(json, "pri_two", index, v, 0.2),
          type = "numeric", min = 0, max = 1, step = 0.01, source = SRC_MASS_CASUALTY_PRI,
          path = "mass_casualty.schedule"),
    field(sprintf("mc_sched_pri_three_%d", index), GRP_MASS_CASUALTY, "Scheduled Event Days",
          sprintf("Event %d — Priority 3 Share", index),
          "Proportion of this event's casualties triaged as Priority 3 — independent of every other event's priority mix.",
          get = function(json) get_mass_casualty_schedule_slot(json, "pri_three", index, 0.1),
          set = function(json, v) set_mass_casualty_schedule_slot(json, "pri_three", index, v, 0.1),
          type = "numeric", min = 0, max = 1, step = 0.01, source = SRC_MASS_CASUALTY_PRI,
          path = "mass_casualty.schedule")
  )
}

#' Count how many mass_casualty.schedule slots currently have a non-zero
#' Day value, used to seed the Configure panel's initial visible event-row
#' count (see mc_event_count reactiveVal in app.R)
#'
#' @param json Parsed env_data.json (raw tree)
#' @return Integer count of active (Day > 0) slots, 0 if none
count_active_mass_casualty_events <- function(json) {
  sum(vapply(seq_len(MASS_CASUALTY_SCHEDULE_SLOTS), function(i) {
    d <- get_mass_casualty_schedule_slot(json, "days", i, 0)
    !is.null(d) && !is.na(d) && d > 0
  }, logical(1)))
}

# ── Registry assembly ────────────────────────────────────────────────────

#' Build the full parameter registry for the Configure panel
#'
#' @return List of field specs (see field()); each has $id, $group,
#'   $subgroup, $label, $tooltip, $get(json), $set(json, value), $type,
#'   $min, $max, $step, $morris
build_param_registry <- function() {
  registry <- list()

  # ── Force Size ──────────────────────────────────────────────────────────
  registry <- c(registry, list(
    field("pop_combat", GRP_FORCE, NULL, "Combat Force Size",
          "Number of combat-role personnel exposed to the casualty-generation model.",
          get = function(json) get_pop_count(json, "combat"),
          set = function(json, v) set_pop_count(json, "combat", v),
          type = "integer", min = 1, max = 20000, step = 50, source = SRC_ESTABLISHMENT),
    field("pop_support", GRP_FORCE, NULL, "Support Force Size",
          "Number of support-role personnel exposed to the casualty-generation model.",
          get = function(json) get_pop_count(json, "support"),
          set = function(json, v) set_pop_count(json, "support", v),
          type = "integer", min = 1, max = 20000, step = 50, source = SRC_ESTABLISHMENT),
    var_field("force_regen_demand_interval", GRP_FORCE, "Reinforcement Demand & Fulfillment", "force_regeneration", "reinforcement", "demand_interval_days",
              "Demand Submission Cycle (days)", "How often a reinforcement demand can be submitted for each pool. Each submission asks for the pool's full current shortfall against establishment strength. 0 disables reinforcement entirely (the shipped default) — casualty production and return-to-duty are then the only forces acting on effective force size.",
              type = "integer", min = 0, max = 30, step = 1, source = SRC_FORCE_REGEN),
    var_field("force_regen_lag", GRP_FORCE, "Reinforcement Demand & Fulfillment", "force_regeneration", "reinforcement", "fulfillment_lag_days",
              "Fulfillment Lag (days)", "Delay between a reinforcement demand being submitted and the delivered amount being credited to the effective force pool (ignored if the demand cycle is 0).",
              type = "integer", min = 0, max = 60, step = 1, source = SRC_FORCE_REGEN),
    var_field("force_regen_fill_min", GRP_FORCE, "Reinforcement Demand & Fulfillment", "force_regeneration", "reinforcement", "fill_min_frac",
              "Fill Distribution — Minimum (fraction of demand)", "Lower bound of the triangular distribution governing what fraction of a submitted demand is actually delivered — the low end of the long under-fill tail.",
              type = "numeric", min = 0, max = 1, step = 0.05, source = SRC_FORCE_REGEN, slider = TRUE),
    var_field("force_regen_fill_mode", GRP_FORCE, "Reinforcement Demand & Fulfillment", "force_regeneration", "reinforcement", "fill_mode_frac",
              "Fill Distribution — Mode (fraction of demand)", "Most likely fraction of a submitted demand actually delivered — set close to (but below) 1, so full fulfillment is the peak of the distribution but under-fill is still more probable in aggregate than over-fill.",
              type = "numeric", min = 0, max = 1.5, step = 0.05, source = SRC_FORCE_REGEN, slider = TRUE),
    var_field("force_regen_fill_max", GRP_FORCE, "Reinforcement Demand & Fulfillment", "force_regeneration", "reinforcement", "fill_max_frac",
              "Fill Distribution — Maximum (fraction of demand)", "Upper bound of the triangular distribution — kept close to 1 so over-supply is possible but limited, unlike the long lower tail toward under-fill.",
              type = "numeric", min = 1, max = 2, step = 0.05, source = SRC_FORCE_REGEN, slider = TRUE)
  ))

  # ── Casualty Rates ──────────────────────────────────────────────────────
  gen_streams <- list(
    list("wia_cbt",  "WIA — Combat",   SRC_FORECAS_WIA_CBT),
    list("kia_cbt",  "KIA — Combat",   SRC_FORECAS_KIA_CBT),
    list("dnbi_cbt", "DNBI — Combat",  SRC_FORECAS_DNBI_CBT),
    list("wia_spt",  "WIA — Support",  SRC_SUPPORT_INCLUSION),
    list("kia_spt",  "KIA — Support",  SRC_SUPPORT_INCLUSION),
    list("dnbi_spt", "DNBI — Support", SRC_FORECAS_DNBI_SPT)
  )
  # lapply(), not `for`, so each stream's get()/set() closures capture their
  # own acty/label/src rather than all resolving to the loop's final value
  # (see the note on var_field()'s force() calls above).
  registry <- c(registry, unlist(lapply(gen_streams, function(s) {
    acty <- s[[1]]; label <- s[[2]]; src <- s[[3]]
    list(
      var_field(paste0("gen_", acty, "_mean"), GRP_CASUALTY, "Casualty Generation Rates",
                "generators", acty, "mean_daily",
                paste0(label, " — Mean Daily Rate"),
                "Expected daily casualties per 1,000 population (lognormal rate model).",
                type = "numeric", min = 0, max = 20, step = 0.01, source = src),
      var_field(paste0("gen_", acty, "_sd"), GRP_CASUALTY, "Casualty Generation Rates",
                "generators", acty, "sd_daily",
                paste0(label, " — Daily Rate Std. Dev."),
                "Day-to-day variability in the casualty rate (lognormal shape parameter).",
                type = "numeric", min = 0, max = 20, step = 0.01, source = src)
    )
  }), recursive = FALSE))
  registry <- c(registry, list(
    var_field("pri_one", GRP_CASUALTY, "Triage Priority Split", "r1", "priority", "one",
              "Priority 1 (Immediate) Share", "Proportion of WIA triaged as Priority 1 at R1.",
              min = 0, max = 1, step = 0.01, source = SRC_PRIORITY_SPLIT),
    var_field("pri_two", GRP_CASUALTY, "Triage Priority Split", "r1", "priority", "two",
              "Priority 2 (Urgent) Share", "Proportion of WIA triaged as Priority 2 at R1.",
              min = 0, max = 1, step = 0.01, source = SRC_PRIORITY_SPLIT),
    var_field("pri_three", GRP_CASUALTY, "Triage Priority Split", "r1", "priority", "three",
              "Priority 3 (Delayed) Share", "Proportion of WIA triaged as Priority 3 at R1.",
              min = 0, max = 1, step = 0.01, source = SRC_PRIORITY_SPLIT),
    var_field("dnbi_bf_pct", GRP_CASUALTY, "DNBI Sub-Type Split", "r1", "other", "battle_fatigue_pct",
              "Battle Fatigue Share of DNBI", "Proportion of DNBI casualties returned to duty at R1 without clinical treatment (Issue #7).",
              min = 0, max = 1, step = 0.01, source = SRC_DNBI_BF_PCT),
    var_field("dnbi_disease_pct", GRP_CASUALTY, "DNBI Sub-Type Split", "r1", "other", "disease_pct",
              "Disease Share of DNBI", "Proportion of DNBI casualties routed to R2B holding for disease management (Issue #7).",
              min = 0, max = 1, step = 0.01, source = SRC_DNBI_DISEASE_PCT),
    var_field("dnbi_nbi_pct", GRP_CASUALTY, "DNBI Sub-Type Split", "r1", "other", "nbi_pct",
              "Non-Battle Injury Share of DNBI", "Proportion of DNBI casualties following the full WIA-equivalent clinical pathway (Issue #7).",
              min = 0, max = 1, step = 0.01, source = SRC_DNBI_NBI_PCT),
    var_field("surg_pri1", GRP_CASUALTY, "Surgical Candidacy", "r1", "other", "pri1_surgery",
              "Priority 1 Surgical Candidacy", "Proportion of Priority 1 WIA requiring surgery.",
              min = 0, max = 1, step = 0.01, morris_name = "pri1_surg_prob", source = SRC_PRIORITY_SPLIT),
    var_field("surg_pri2", GRP_CASUALTY, "Surgical Candidacy", "r1", "other", "pri2_surgery",
              "Priority 2 Surgical Candidacy", "Proportion of Priority 2 WIA requiring surgery.",
              min = 0, max = 1, step = 0.01, source = SRC_PRIORITY_SPLIT, slider = TRUE),
    var_field("surg_pri3_dnbi", GRP_CASUALTY, "Surgical Candidacy", "r1", "other", "pri3_dnbi_surgery",
              "Priority 3 DNBI Surgical Candidacy", "Proportion of Priority 3 DNBI casualties requiring surgery.",
              min = 0, max = 1, step = 0.01, source = SRC_PRIORITY_SPLIT, slider = TRUE),
    var_field("surg_pri3_other", GRP_CASUALTY, "Surgical Candidacy", "r1", "other", "pri3_other_surgery",
              "Priority 3 Other Surgical Candidacy", "Proportion of other Priority 3 casualties requiring surgery.",
              min = 0, max = 1, step = 0.01, source = SRC_PRIORITY_SPLIT, slider = TRUE),
    var_field("dnbi_disease_surgery_pct", GRP_CASUALTY, "Surgical Candidacy", "r1", "other", "disease_surgery_pct",
              "Disease Surgical Candidacy", "Proportion of disease DNBI casualties who nonetheless require surgery.",
              min = 0, max = 1, step = 0.01, source = SRC_DISEASE_SURGERY, slider = TRUE),
    var_field("evac_pri1", GRP_CASUALTY, "Strategic Evacuation Rates", "r1", "other", "pri1_evac",
              "Priority 1 Strategic Evacuation Rate", "Proportion of treated Priority 1 casualties evacuated out of theatre.",
              min = 0, max = 1, step = 0.01, source = SRC_EVAC_CANDIDACY, slider = TRUE),
    var_field("evac_pri2", GRP_CASUALTY, "Strategic Evacuation Rates", "r1", "other", "pri2_evac",
              "Priority 2 Strategic Evacuation Rate", "Proportion of treated Priority 2 casualties evacuated out of theatre.",
              min = 0, max = 1, step = 0.01, source = SRC_EVAC_CANDIDACY, slider = TRUE),
    var_field("dow_p1_pmax", GRP_CASUALTY, "Died of Wounds Ceilings", "dow", "params", "p1_p_max",
              "Priority 1 DOW Ceiling", "Asymptotic maximum cumulative Died-of-Wounds probability for an untreated Priority 1 casualty (Falklands 1982 calibration, Issue #5).",
              min = 0, max = 1, step = 0.001, morris_name = "p1_p_max", source = SRC_DOW_CEILING),
    var_field("dow_p2_pmax", GRP_CASUALTY, "Died of Wounds Ceilings", "dow", "params", "p2_p_max",
              "Priority 2 DOW Ceiling", "Asymptotic maximum cumulative Died-of-Wounds probability for an untreated Priority 2 casualty (Falklands 1982 calibration, Issue #5).",
              min = 0, max = 1, step = 0.001, source = SRC_DOW_CEILING, slider = TRUE)
  ))

  # ── Health System Architecture: R1 — Forward Aid Post ─────────────────────
  registry <- c(registry, list(
    field("r1_team_count", GRP_HEALTH_ARCH, "R1 — Establishment", "Number of R1 Teams",
          "Number of R1 (forward aid post) teams deployed.",
          get = function(json) get_elm_qty(json, "r1"),
          set = function(json, v) set_elm_qty(json, "r1", v),
          type = "integer", min = 1, max = 10, step = 1, source = SRC_ESTABLISHMENT)
  ))

  # ── Health Provision: R1 — Forward Aid Post ───────────────────────────────
  registry <- c(registry, tri_fields("r1_wia_treat", GRP_PROVISION, "R1 — Treatment Durations", "r1", "wia_treat",
                                     "WIA Treatment Time", "Time to stabilise a WIA casualty at R1.", bound = c(0, 200),
                                     source = SRC_R1_WIA_TREAT))
  registry <- c(registry, tri_fields("r1_kia_treat", GRP_PROVISION, "R1 — Treatment Durations", "r1", "kia_treat",
                                     "KIA Processing Time", "Time to process a KIA casualty at R1.", bound = c(0, 200)))
  registry <- c(registry, tri_fields("r1_recovery", GRP_PROVISION, "R1 — Treatment Durations", "r1", "recovery",
                                     "Battle Fatigue Hold Duration", "Time a battle fatigue casualty spends in R1 hold before returning to duty.", bound = c(0, 20000),
                                     source = SRC_R1_RECOVERY))

  # ── Medevac: Transport Fleet ──────────────────────────────────────────────
  # Placed first so fleet-level totals (how many vehicles exist) precede the
  # per-leg duration detail below, matching how a planner would naturally
  # read the panel top-to-bottom: capacity first, then the legs that draw on it.
  registry <- c(registry, unlist(lapply(c("PMVAmb", "HX240M"), function(veh) {
    list(
      field(paste0("transport_", veh, "_qty"), GRP_LOGISTICS, "Transport Fleet", paste0(veh, " — Fleet Size"),
            paste0("Number of ", veh, " vehicles available for casualty evacuation."),
            get = function(json) get_transport_field(json, veh, "qty"),
            set = function(json, v) set_transport_field(json, veh, "qty", v),
            type = "integer", min = 0, max = 20, step = 1, source = SRC_VEHICLE_CAPACITY),
      field(paste0("transport_", veh, "_capacity"), GRP_LOGISTICS, "Transport Fleet", paste0(veh, " — Capacity per Vehicle"),
            paste0("Number of casualties a single ", veh, " can carry per load."),
            get = function(json) get_transport_field(json, veh, "capacity"),
            set = function(json, v) set_transport_field(json, veh, "capacity", v),
            type = "integer", min = 1, max = 100, step = 1, source = SRC_VEHICLE_CAPACITY)
    )
  }), recursive = FALSE))

  # ── Medevac: R1 — Forward Aid Post ────────────────────────────────────────
  registry <- c(registry, tri_fields("r1_wia_transport", GRP_LOGISTICS, "R1 — Transport (R1 → R2B)", "r1", "wia_transport",
                                     "WIA Transport Time", "Transport time from point of injury to R1/R2B for a WIA casualty.",
                                     morris_mode_name = "r1_transport", bound = c(0, 200), source = SRC_TRANSPORT_GENERIC))
  registry <- c(registry, tri_fields("r1_kia_transport", GRP_LOGISTICS, "R1 — Transport (R1 → R2B)", "r1", "kia_transport",
                                     "KIA Transport Time", "Transport time to move a KIA casualty from point of injury.", bound = c(0, 200),
                                     source = SRC_TRANSPORT_GENERIC))

  # ── Health System Architecture: R2B — Battalion Aid Post ──────────────────
  registry <- c(registry, list(
    field("r2b_team_count", GRP_HEALTH_ARCH, "R2B — Establishment", "Number of R2B Teams",
          "Number of R2B (battalion aid post) teams deployed.",
          get = function(json) get_elm_qty(json, "r2b"),
          set = function(json, v) set_elm_qty(json, "r2b", v),
          type = "integer", min = 1, max = 10, step = 1, source = SRC_ESTABLISHMENT)
  ))
  # lapply(), not `for` — see the note on gen_streams above; a `for` loop
  # here previously made every R2B bed field silently get/set "hold" (the
  # last bed type in the list), regardless of which bed type the field's
  # own label said it was.
  registry <- c(registry, lapply(list(c("ot", "Operating Theatre Beds"), c("resus", "Resuscitation Beds"),
                                       c("icu", "ICU Beds"), c("hold", "Holding Beds")), function(bed) {
    field(paste0("r2b_bed_", bed[1]), GRP_HEALTH_ARCH, "R2B — Bed Capacity (per Team)", bed[2],
          paste0("Number of ", tolower(bed[2]), " per R2B team."),
          get = function(json) get_bed_qty(json, "r2b", bed[1]),
          set = function(json, v) set_bed_qty(json, "r2b", bed[1], v),
          type = "integer", min = 0, max = 50, step = 1, source = SRC_ESTABLISHMENT)
  }))

  # ── Health Provision: R2B — Battalion Aid Post ────────────────────────────
  registry <- c(registry, tri_fields("r2b_surgery", GRP_PROVISION, "R2B — Surgical & Resuscitation Durations", "r2b", "surgery",
                                     "Surgery Duration", "Time occupying an R2B operating theatre per case.",
                                     morris_mode_name = "surg_mode", bound = c(0, 400), source = SRC_DCS_SURGERY))
  registry <- c(registry, tri_fields("r2b_long_resus", GRP_PROVISION, "R2B — Surgical & Resuscitation Durations", "r2b", "long_resus",
                                     "Long Resuscitation Duration", "Time occupying an R2B resuscitation bay for a complex case.",
                                     morris_mode_name = "long_resus_mode", bound = c(0, 200), source = SRC_RESUS_TASK_TABLE))
  registry <- c(registry, tri_fields("r2b_holding", GRP_PROVISION, "R2B — Holding & Routing", "r2b", "holding",
                                     "Holding Bed Duration", "Time occupying an R2B holding bed.", bound = c(0, 20000)))
  registry <- c(registry, list(
    var_field("r2b_hold_threshold", GRP_PROVISION, "R2B — Holding & Routing", "r2b", "holding", "hold_threshold",
              "Hold-Bed Reroute Threshold", "Occupancy fraction above which new patients are rerouted to R2E rather than queuing at R2B (Issue #39).",
              min = 0, max = 1, step = 0.05, source = SRC_HOLD_THRESHOLD, slider = TRUE)
  ))
  registry <- c(registry, list(
    field("r2b_icu_defer_interval", GRP_PROVISION, "R2B — ICU Gating", "OT-Entry Defer Poll Interval",
          "Interval between ICU-availability checks while OT entry is deferred pending a bed (Issue #43).",
          get = function(json) get_raw_var(json, "r2b", "icu_gating", "defer_check_interval"),
          set = function(json, v) set_raw_var(json, "r2b", "icu_gating", "defer_check_interval", v),
          type = "integer", min = 5, max = 180, step = 5, source = SRC_ICU_GATING)
  ))

  # ── Medevac: R2B — Battalion Aid Post ─────────────────────────────────────
  registry <- c(registry, tri_fields("r2b_wia_transport", GRP_LOGISTICS, "R2B — Transport (R2B ↔ R2E)", "r2b", "wia_transport",
                                     "WIA Transport Time", "Transport time from R2B to R2E for a WIA casualty. Seizes each R2B team's own organic evac resource, not the shared PMVAmb fleet, with a dead-heading return leg on that same resource (Issue #73).",
                                     morris_mode_name = "r2b_transport", bound = c(0, 200), source = SRC_TRANSPORT_GENERIC))
  registry <- c(registry, tri_fields("r2b_kia_transport", GRP_LOGISTICS, "R2B — Transport (R2B ↔ R2E)", "r2b", "kia_transport",
                                     "KIA/Mortuary Transport Time", "Road-move transport time for a KIA casualty from R2B to the mortuary, modelled as collocated with R2E rather than R2B (Issue #73). Uses the shared HX2 40M fleet with a dead-heading return leg.",
                                     bound = c(0, 200), source = SRC_TRANSPORT_GENERIC))

  # ── Health System Architecture: R2E — Field Hospital ──────────────────────
  registry <- c(registry, list(
    field("r2e_team_count", GRP_HEALTH_ARCH, "R2E — Establishment", "Number of R2E Teams",
          "Number of R2E (field hospital) teams deployed.",
          get = function(json) get_elm_qty(json, "r2eheavy"),
          set = function(json, v) set_elm_qty(json, "r2eheavy", v),
          type = "integer", min = 1, max = 5, step = 1, source = SRC_ESTABLISHMENT)
  ))
  registry <- c(registry, lapply(list(c("ot", "Operating Theatre Beds"), c("resus", "Resuscitation Beds"),
                                       c("icu", "ICU Beds"), c("hold", "Holding Beds")), function(bed) {
    field(paste0("r2e_bed_", bed[1]), GRP_HEALTH_ARCH, "R2E — Bed Capacity (per Team)", bed[2],
          paste0("Number of ", tolower(bed[2]), " per R2E team."),
          get = function(json) get_bed_qty(json, "r2eheavy", bed[1]),
          set = function(json, v) set_bed_qty(json, "r2eheavy", bed[1], v),
          type = "integer", min = 0, max = 50, step = 1, source = SRC_ESTABLISHMENT)
  }))

  # ── Health Provision: R2E — Field Hospital ────────────────────────────────
  registry <- c(registry, tri_fields("r2e_surgery", GRP_PROVISION, "R2E — Surgical & Resuscitation Durations", "r2eheavy", "surgery",
                                     "Surgery Duration", "Time occupying an R2E operating theatre per case.",
                                     morris_mode_name = "surg_mode", bound = c(0, 400), source = SRC_DCS_SURGERY))
  registry <- c(registry, tri_fields("r2e_short_resus", GRP_PROVISION, "R2E — Surgical & Resuscitation Durations", "r2eheavy", "short_resus",
                                     "Short Resuscitation Duration", "Time occupying an R2E resuscitation bay for a straightforward case.", bound = c(0, 200),
                                     source = SRC_RESUS_TASK_TABLE))
  registry <- c(registry, tri_fields("r2e_long_resus", GRP_PROVISION, "R2E — Surgical & Resuscitation Durations", "r2eheavy", "long_resus",
                                     "Long Resuscitation Duration", "Time occupying an R2E resuscitation bay for a complex case.",
                                     morris_mode_name = "long_resus_mode", bound = c(0, 200), source = SRC_RESUS_TASK_TABLE))
  registry <- c(registry, tri_fields("r2e_short_icu", GRP_PROVISION, "R2E — ICU & Holding Durations", "r2eheavy", "short_icu",
                                     "Short ICU Stay", "Time occupying an R2E ICU bed after a short-recovery case.", bound = c(0, 500)))
  registry <- c(registry, tri_fields("r2e_long_icu", GRP_PROVISION, "R2E — ICU & Holding Durations", "r2eheavy", "long_icu",
                                     "Long ICU Stay", "Time occupying an R2E ICU bed after a full-recovery case.",
                                     morris_mode_name = "long_icu_mode", bound = c(0, 5000), source = SRC_ICU_STABILISATION))
  registry <- c(registry, tri_fields("r2e_holding", GRP_PROVISION, "R2E — ICU & Holding Durations", "r2eheavy", "holding",
                                     "Holding Bed Duration", "Time occupying an R2E holding bed.", bound = c(0, 40000),
                                     source = SRC_IN_THEATRE_RATE))
  registry <- c(registry, tri_fields("r2e_post_op_hold", GRP_PROVISION, "R2E — ICU & Holding Durations", "r2eheavy", "post_op_hold",
                                     "Post-Op Holding-Bed Duration", "Time in a holding bed for a Priority 1 casualty recovering outside ICU due to ICU saturation (Issue #43).", bound = c(0, 3000),
                                     source = SRC_POST_OP_HOLD))
  registry <- c(registry, list(
    var_field("r2e_in_theatre_rate", GRP_PROVISION, "R2E — Recovery & Evacuation", "r2eheavy", "recovery", "in_theatre_rate",
              "In-Theatre Recovery Rate", "Proportion of R2E casualties who recover in theatre rather than being strategically evacuated.",
              min = 0, max = 1, step = 0.01, morris_name = "in_theatre_rate", source = SRC_IN_THEATRE_RATE),
    var_field("r2e_post_surgery_rate", GRP_PROVISION, "R2E — Recovery & Evacuation", "r2eheavy", "recovery", "post_surgery",
              "Post-Surgery Full-Recovery Rate", "Proportion of surgical patients routed to full (long) ICU recovery rather than short recovery.",
              min = 0, max = 1, step = 0.01, slider = TRUE),
    var_field("r2e_icu_p1_bypass", GRP_PROVISION, "R2E — ICU Gating", "r2eheavy", "icu_gating", "p1_bypass_priority_max",
              "ICU-Full Priority Override Threshold", "Maximum priority level (1 = most severe) permitted to bypass a full ICU by recovering in a holding bed instead (Issue #43).",
              type = "integer", min = 1, max = 3, step = 1, source = SRC_ICU_GATING, choices = 1:3),
    field("r2e_icu_defer_interval", GRP_PROVISION, "R2E — ICU Gating", "OT-Entry Defer Poll Interval",
          "Interval between ICU-availability checks while OT entry is deferred pending a bed (Issue #43).",
          get = function(json) get_raw_var(json, "r2eheavy", "icu_gating", "defer_check_interval"),
          set = function(json, v) set_raw_var(json, "r2eheavy", "icu_gating", "defer_check_interval", v),
          type = "integer", min = 5, max = 180, step = 5, source = SRC_ICU_GATING)
  ))
  registry <- c(registry, tri_fields("r2e_kia_treat", GRP_PROVISION, "R2E — KIA Treatment", "r2eheavy", "kia_treat",
                                     "KIA Processing Time", "Time to process a KIA casualty at R2E.", bound = c(0, 200)))

  # ── Medevac: R2E — Field Hospital ─────────────────────────────────────────
  registry <- c(registry, tri_fields("r2e_kia_transport", GRP_LOGISTICS, "R2E — KIA Transport", "r2eheavy", "kia_transport",
                                     "KIA Transport Time", "Transport time to move a KIA casualty from R2E.", bound = c(0, 200),
                                     source = SRC_TRANSPORT_GENERIC))

  # ── Medevac: Strategic AME (Issue #23 second follow-up) ─────────────────
  # Two named aircraft configurations (fixed critical/standard capacity
  # pairs — a CCATT/CCAST-augmented sortie trades standard slots for
  # critical-care capacity on the same airframe) share one sortie
  # schedule. At each scheduled opportunity the simulation flies whichever
  # configuration minimises total unmet need across the critical
  # (CCATT/CCAST-supported, ICU-bed Priority 1 surgical evacuees) and
  # standard (Casualty Staging Unit-equivalent, Hold-bed everyone else)
  # queues — see build_ame_sortie_trajectory(), R/trajectories.R. See
  # README Role 4 (National Support Base) Demand Modelling for the
  # AJP-4.10(B) basis for the critical/standard split, and MODEL
  # ASSUMPTION — AME Configuration Selection Rule for the selection logic.
  registry <- c(registry, list(
    var_field("ame_schedule_interval", GRP_LOGISTICS, "Strategic AME", "role4", "ame", "schedule_interval_days",
              "Sortie Interval (days)", "Days between scheduled strategic AME sortie opportunities. Every strategic evacuee queues indefinitely if this exceeds the run length or is set to 0 — AME never opens.",
              type = "integer", min = 1, max = 30, step = 1, source = SRC_AME_SCHEDULE),
    var_field("ame_failure_probability", GRP_LOGISTICS, "Strategic AME", "role4", "ame", "failure_probability",
              "Sortie Cancellation Probability", "Probability any given scheduled sortie is cancelled (weather, tasking, airframe availability) and carries zero capacity, regardless of which configuration would otherwise have flown.",
              min = 0, max = 1, step = 0.01, source = SRC_AME_SCHEDULE, slider = TRUE),
    var_field("ame_config_a_critical", GRP_LOGISTICS, "Strategic AME", "role4", "ame_config_a", "critical_capacity",
              "Configuration A — Critical Capacity per Sortie", "Critical (CCATT/CCAST-supported, ICU-bed Priority 1 surgical) casualties carried if Configuration A flies.",
              type = "integer", min = 0, max = 200, step = 1, source = SRC_AME_SCHEDULE),
    var_field("ame_config_a_standard", GRP_LOGISTICS, "Strategic AME", "role4", "ame_config_a", "standard_capacity",
              "Configuration A — Standard Capacity per Sortie", "Standard (Casualty Staging Unit-equivalent, Hold-bed) casualties carried if Configuration A flies.",
              type = "integer", min = 0, max = 200, step = 1, source = SRC_AME_SCHEDULE),
    var_field("ame_config_b_critical", GRP_LOGISTICS, "Strategic AME", "role4", "ame_config_b", "critical_capacity",
              "Configuration B — Critical Capacity per Sortie", "Critical (CCATT/CCAST-supported, ICU-bed Priority 1 surgical) casualties carried if Configuration B flies.",
              type = "integer", min = 0, max = 200, step = 1, source = SRC_AME_SCHEDULE),
    var_field("ame_config_b_standard", GRP_LOGISTICS, "Strategic AME", "role4", "ame_config_b", "standard_capacity",
              "Configuration B — Standard Capacity per Sortie", "Standard (Casualty Staging Unit-equivalent, Hold-bed) casualties carried if Configuration B flies.",
              type = "integer", min = 0, max = 200, step = 1, source = SRC_AME_SCHEDULE)
  ))

  # ── Mass Casualty (Issue #9) ───────────────────────────────────────────────
  registry <- c(registry, list(
    var_field("mc_mode", GRP_MASS_CASUALTY, "Event Timing Mode", "mass_casualty", "event", "mode",
              "Event Timing Mode",
              "Selects how mass casualty events are timed: a stochastic Poisson process (random inter-event interval, event count varies by replication) or a fixed set of planner-specified simulation days.",
              source = SRC_MASS_CASUALTY,
              choices = c("Random (Poisson Rate)" = "poisson", "Scheduled (Deliberate Days)" = "scheduled")),
    var_field("mc_rate", GRP_MASS_CASUALTY, "Random Event Rate", "mass_casualty", "event", "rate_per_day",
              "Event Rate (per day)", "Mean number of mass casualty events per day (Poisson process); 0 disables random-mode injection entirely.",
              min = 0, max = 1, step = 0.01, morris_name = "mass_casualty_rate", source = SRC_MASS_CASUALTY),
    # Casualties-per-event only applies as a *shared* setting in "poisson"
    # mode — every Poisson-drawn event uses the same range. "scheduled" mode
    # instead gives each event its own min/max (mc_sched_min_cas_<i>/
    # mc_sched_max_cas_<i> below), so these two fields live in the
    # Random Event Rate subgroup (rendered only when mode = poisson, see
    # render_group_body() in app.R) rather than a mode-independent subgroup.
    var_field("mc_min_cas", GRP_MASS_CASUALTY, "Random Event Rate", "mass_casualty", "event", "min_cas",
              "Casualties per Event — Minimum", "Minimum number of casualties injected by a single fired event (Uniform distribution). Shared across every Poisson-mode event.",
              type = "integer", min = 1, max = 500, step = 1, source = SRC_MASS_CASUALTY),
    var_field("mc_max_cas", GRP_MASS_CASUALTY, "Random Event Rate", "mass_casualty", "event", "max_cas",
              "Casualties per Event — Maximum", "Maximum number of casualties injected by a single fired event (Uniform distribution). Shared across every Poisson-mode event.",
              type = "integer", min = 1, max = 500, step = 1, morris_name = "mass_casualty_max_cas", source = SRC_MASS_CASUALTY)
  ))
  registry <- c(registry, unlist(
    lapply(seq_len(MASS_CASUALTY_SCHEDULE_SLOTS), mass_casualty_schedule_slot_fields),
    recursive = FALSE
  ))
  # Not tri_fields(): that helper hardcodes var names "min"/"mode"/"max"
  # within a dedicated acty, but window_min/window_mode/window_max share the
  # "event" acty with mode/rate_per_day/min_cas/max_cas above, so each var
  # is prefixed to disambiguate. detect_tri_triples() (app.R) still finds
  # this triple automatically via the "mc_window_min/_mode/_max" id suffixes
  # — the live curve card renders exactly as it would via tri_fields(). The
  # injection window is not customisable per event in either mode (see
  # generate_mass_casualty_events(), R/environment.R), so this subgroup
  # renders unconditionally, unlike Random Event Rate/Scheduled Event Days.
  registry <- c(registry, list(
    var_field("mc_window_min", GRP_MASS_CASUALTY, "Injection Window", "mass_casualty", "event", "window_min",
              "Injection Window — Minimum", "Minimum duration over which a fired event's casualties are distributed (triangular distribution), minutes. Applies to every event regardless of timing mode.",
              type = "integer", min = 1, max = 1440, step = 1, source = SRC_MASS_CASUALTY),
    var_field("mc_window_mode", GRP_MASS_CASUALTY, "Injection Window", "mass_casualty", "event", "window_mode",
              "Injection Window — Most Likely (Mode)", "Most likely duration over which a fired event's casualties are distributed (triangular distribution mode), minutes. Applies to every event regardless of timing mode.",
              type = "integer", min = 1, max = 1440, step = 1, source = SRC_MASS_CASUALTY),
    var_field("mc_window_max", GRP_MASS_CASUALTY, "Injection Window", "mass_casualty", "event", "window_max",
              "Injection Window — Maximum", "Maximum duration over which a fired event's casualties are distributed (triangular distribution), minutes. Applies to every event regardless of timing mode.",
              type = "integer", min = 1, max = 1440, step = 1, source = SRC_MASS_CASUALTY)
  ))
  # Shared priority split — like Casualties per Event above, this only
  # applies in "poisson" mode; "scheduled" mode uses each event's own
  # mc_sched_pri_one/two/three_<i> fields instead (rendered only when
  # mode = scheduled, see render_group_body() in app.R).
  registry <- c(registry, list(
    var_field("mc_pri_one", GRP_MASS_CASUALTY, "Mass Casualty Priority Split", "mass_casualty", "priority", "one",
              "Priority 1 (Immediate) Share", "Proportion of Poisson-mode mass-casualty-derived casualties triaged as Priority 1 — independent of the background Triage Priority Split.",
              min = 0, max = 1, step = 0.01, source = SRC_MASS_CASUALTY_PRI),
    var_field("mc_pri_two", GRP_MASS_CASUALTY, "Mass Casualty Priority Split", "mass_casualty", "priority", "two",
              "Priority 2 (Urgent) Share", "Proportion of Poisson-mode mass-casualty-derived casualties triaged as Priority 2 — independent of the background Triage Priority Split.",
              min = 0, max = 1, step = 0.01, source = SRC_MASS_CASUALTY_PRI),
    var_field("mc_pri_three", GRP_MASS_CASUALTY, "Mass Casualty Priority Split", "mass_casualty", "priority", "three",
              "Priority 3 (Delayed) Share", "Proportion of Poisson-mode mass-casualty-derived casualties triaged as Priority 3 — independent of the background Triage Priority Split.",
              min = 0, max = 1, step = 0.01, source = SRC_MASS_CASUALTY_PRI)
  ))

  registry
}

#' Read current values for every field in a registry from a parsed env_data.json
#'
#' @param registry List of field specs from build_param_registry()
#' @param json Parsed env_data.json (raw tree)
#' @return Named list of current values, keyed by field id
registry_defaults <- function(registry, json) {
  setNames(lapply(registry, function(f) f$get(json)), vapply(registry, `[[`, character(1), "id"))
}

#' Apply a named list of edited values back onto a parsed env_data.json
#'
#' @param registry List of field specs from build_param_registry()
#' @param json Parsed env_data.json (raw tree) to modify
#' @param values Named list/vector of values keyed by field id (as produced
#'   by Shiny's `input` reactive values)
#' @return Modified copy of json
apply_registry_values <- function(registry, json, values) {
  for (f in registry) {
    if (!is.null(values[[f$id]])) {
      v <- values[[f$id]]
      # as.numeric() first: selectInput() (used for f$choices fields, e.g.
      # r2e_icu_p1_bypass) always returns its selected value as a character
      # string regardless of the type of the choices it was built from, and
      # round() errors on a plain character vector.
      if (f$type == "integer") v <- as.integer(round(as.numeric(v)))
      json <- f$set(json, v)
    }
  }
  json
}
