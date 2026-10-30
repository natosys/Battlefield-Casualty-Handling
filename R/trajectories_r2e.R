##############################################
## R/trajectories_r2e.R                   ##
## Role 2E Heavy trajectories             ##
##############################################
#
# The R2E Heavy trajectories: KIA handling and the mortuary, surgery and
# the intensive care gate, stabilisation and post-definitive care, the
# second operation, strategic evacuation and disposition.
#
# Sourced by R/trajectories.R, which loads the packages and R/constants.R
# these functions use; source that file rather than this one alone.

# ── Role 2E Heavy trajectories ────────────────────────────────────────────────

#' Applies treatment for KIA casualties at R2E using the evacuation team
#'
#' @param traj Simmer trajectory object to append treatment steps to
#' @param team_id Integer index of the Role 2E Heavy team
#' @param evac_team This R2E team's evacuation sections, as team_sections()
#'   returns them; the first to come free processes the casualty
#' @return Modified trajectory with mortuary prep sequence appended
r2e_treat_kia <- function(traj, team_id, evac_team) {
  traj %>%
    seize_first_free_section(evac_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2eheavy$kia_treat$min,
        b = env_data$vars$r2eheavy$kia_treat$max,
        c = env_data$vars$r2eheavy$kia_treat$mode
      )
    }) %>%
    release_held_section(evac_team)
}

#' Simulates transport of KIA casualty to mortuary at R2E
#'
#' @param traj Simmer trajectory object to append movement steps to
#' @param team_id Integer index of the Role 2E Heavy team
#' @param evac_team This R2E team's evacuation sections, as team_sections()
#'   returns them; the first to come free makes the move
#' @return Modified trajectory with transport sequence and mortuary_treated flag
r2e_transport_kia <- function(traj, team_id, evac_team) {
  traj %>%
    seize_first_free_section(evac_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2eheavy$kia_transport$min,
        b = env_data$vars$r2eheavy$kia_transport$max,
        c = env_data$vars$r2eheavy$kia_transport$mode
      )
    }) %>%
    set_attribute("mortuary_treated", 1) %>%
    release_held_section(evac_team)
}

#' Builds a per-R2E-team mortuary intake sub-trajectory for KIA casualties
#' arriving by road from R2B (the mortuary is modelled as collocated with
#' R2E, not R2B). One small trajectory is built per R2E
#' team at model-graph-construction time — mirroring r2e_treat_wia()'s own
#' team-selection pattern — and dispatched to at runtime via the "r2e"
#' attribute set by r2b_transport_kia() before the branch(). The evacuation
#' section is the first of the team's to come free, chosen per casualty.
#'
#' @param team_id Integer index of the Role 2E Heavy team
#' @return Simmer trajectory appending that team's KIA mortuary intake
r2e_mortuary_intake <- function(team_id) {
  evac_team <- team_sections("r2eheavy", team_id, "evac")
  trajectory("R2E Mortuary Intake") %>%
    r2e_treat_kia(team_id, evac_team) %>%
    r2e_transport_kia(team_id, evac_team)
}

# ── R2E Heavy WIA pathway builders ────────────────────────────────────────────

#' Builds one operation at R2E, from the first surgical section free
#'
#' @param select_id  simmer selection id used for the OT bed in this block
#' @param start_attr Attribute name recording incision time
#' @param end_attr   Attribute name recording closure time
#' @param efficacy   Zero-argument function returning the dow_ceiling
#'   multiplier to apply on completion, so the multiplier can depend on the
#'   casualty's surgical pathway
#' @param set_flag   Whether to set the r2e_surgery marker attribute
#' @param ot_beds    This R2E team's operating theatre beds
#' @param surg       This R2E team's surgical sections, as team_sections()
#'   returns them
#' @param ot_pool    This R2E team's theatre pool
#' @return A simmer trajectory performing one operation
#'
#' @details The casualty waits for the first of its unit's sections to be on
#'   shift and free (seize_first_free_section()) and only then takes a theatre:
#'   any free theatre, or the first to free where none is, which happens only
#'   while an operation overruns a shift change, on-shift sections never
#'   outnumbering theatres. No casualty is bound to a section or a theatre
#'   before it can use both, so no theatre stands held for a section that is off
#'   shift or busy. Released theatre then section. A section already
#'   mid-procedure when its shift closes keeps what it holds until release, so a
#'   shift change cannot interrupt an operation in progress.
r2e_surgery_block <- function(select_id, start_attr, end_attr, efficacy, set_flag,
                              ot_beds, surg, ot_pool) {
  force(select_id); force(start_attr); force(end_attr)
  force(efficacy);  force(set_flag)

  trj <- trajectory("R2E Surgery") %>%
    seize_first_free_section(surg, R2E_SURGERY_SECTION_FMT) %>%
    seize(ot_pool, 1) %>%
    simmer::select(ot_beds, policy = "first-available", id = select_id) %>%
    seize_selected(id = select_id)

  if (set_flag) trj <- trj %>% set_attribute("r2e_surgery", 1)

  trj %>%
    set_attribute(start_attr, function() now(env)) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2eheavy$surgery$min,
        b = env_data$vars$r2eheavy$surgery$max,
        c = env_data$vars$r2eheavy$surgery$mode
      )
    }) %>%
    set_attribute(end_attr, function() now(env)) %>%
    release_selected(id = select_id) %>%
    release(ot_pool, 1) %>%
    release_held_section(surg, paste(R2E_SURGERY_SECTION_FMT, "Released")) %>%
    dow_operated(efficacy)
}

#' Builds the shared post-operative died-of-wounds check at R2E
#'
#' @param team_id   Integer index of the Role 2E Heavy team
#' @param evac_team This R2E team's evacuation section
#' @return Simmer trajectory applying the check and stamping the time of it.
#'
#' @details Both the intensive care and the post-operative hold recovery paths
#'   converge here, so that the two pathways' realised mortality can be directly
#'   compared (see README "Died of Wounds" — Post-Operative Checkpoint).
#'   dow_echelon = 4 distinguishes this checkpoint from the Phase 1 R2E arrival
#'   check (dow_echelon = 3).
r2e_post_op_dow_check <- function(team_id, evac_team) {
  if (dow_care_state_enabled()) {
    died <- trajectory("Died of Wounds — Post-Operative") %>%
      set_attribute("dow", 1) %>%
      set_attribute("dow_echelon", 4) %>%
      r2e_treat_kia(team_id, evac_team) %>%
      r2e_transport_kia(team_id, evac_team) %>%
      simmer::leave(1)
    return(trajectory("R2E Post-Operative DOW Check") %>%
             dow_check(cohort = TRUE, died = died,
                       survived = trajectory("Survived Post-Operative Recovery")))
  }

  trajectory("R2E Post-Operative DOW Check") %>%
    branch(
      option = function() {
        injury  <- get_attribute(env, "injury_time")
        t_prev  <- get_attribute(env, "last_dow_t") - injury
        t_now   <- now(env) - injury
        prio    <- get_attribute(env, "priority")
        dp      <- LEGACY_DOW_PARAMS
        ceiling <- get_attribute(env, "dow_ceiling")
        if (!is.na(prio) && prio == 1) {
          p <- dow_prob_conditional(t_now, t_prev, dp$p1_p_base, ceiling, dp$p1_k, dp$p1_t_mid)
        } else if (!is.na(prio) && prio == 2) {
          p <- dow_prob_conditional(t_now, t_prev, dp$p2_p_base, ceiling, dp$p2_k, dp$p2_t_mid)
        } else {
          p <- dp$p3_flat
        }
        if (runif(1) < p) return(1)
        return(2)
      },
      continue = TRUE,
      trajectory("Died of Wounds — Post-Operative") %>%
        set_attribute("dow", 1) %>%
        set_attribute("dow_echelon", 4) %>%
        r2e_treat_kia(team_id, evac_team) %>%
        r2e_transport_kia(team_id, evac_team) %>%
        simmer::leave(1),
      trajectory("Survived Post-Operative Recovery")
    ) %>%
    set_attribute("last_dow_t", function() now(env))
}

#' Builds the died-of-wounds check that ends the post-definitive episode
#'
#' @param team_id   Integer index of the Role 2E Heavy team
#' @param evac_team This R2E team's evacuation section
#' @return Simmer trajectory settling the risk accrued over the episode, or an
#'   empty trajectory where there is nothing for it to settle.
#'
#' @details Present under the care-state model only, and only where the
#'   post-operative multiplier is positive: the episode's post-operative risk is
#'   what the check exists to settle, so a zero setting builds no check and
#'   consumes no draw. dow_echelon = 6 distinguishes it from the post-operative check that
#'   follows stabilisation (dow_echelon = 4).
r2e_post_definitive_dow_check <- function(team_id, evac_team) {
  if (!dow_care_state_enabled() || dow_multipliers()$post_op <= 0) {
    return(trajectory("No Post-Definitive Check"))
  }
  died <- trajectory("Died of Wounds — Post-Definitive") %>%
    set_attribute("dow", 1) %>%
    set_attribute("dow_echelon", 6) %>%
    r2e_treat_kia(team_id, evac_team) %>%
    r2e_transport_kia(team_id, evac_team) %>%
    simmer::leave(1)
  trajectory("R2E Post-Definitive DOW Check") %>%
    dow_check(cohort = TRUE, died = died,
              survived = trajectory("Survived Post-Definitive Care"))
}

#' Applies the legacy mechanism's holding-bed penalty to the ceiling
#'
#' @param trj Trajectory to append to
#' @return The trajectory, with the ceiling raised by LEGACY_POSTOP_HOLD_PENALTY
#'   under the legacy mechanism and unchanged under the care-state model, where
#'   the holding bed instead sets the post-operative multiplier.
legacy_hold_penalty <- function(trj) {
  if (dow_care_state_enabled()) return(trj)
  trj %>% set_attribute("dow_ceiling", function() {
    ceiling <- get_attribute(env, "dow_ceiling")
    if (is.na(ceiling)) return(ceiling)
    ceiling * LEGACY_POSTOP_HOLD_PENALTY
  })
}

#' Builds one post-operative stay, placed in the best bed free when it begins
#'
#' @param label     Name of the stay, used in its trajectory names
#' @param stay      "stabilisation" or "post_definitive", which selects the
#'   attributes the stay reads and records
#' @param icu_beds  This R2E team's intensive care beds
#' @param hold_beds This R2E team's holding beds
#' @param overflow  This R2E team's post-operative overflow resource
#' @return Simmer trajectory serving the stay and leaving the casualty in
#'   post-operative care outside intensive care when it ends.
#'
#' @details The placement is read when the stay begins and kept for the whole of
#'   it (placement_now()), and the full drawn requirement is served whichever bed
#'   it falls in, so a full intensive care unit changes where and how well the
#'   care is given rather than how long it lasts or when it starts. A holding bed
#'   and overflow both carry the holding-bed multiplier (post_op_outside_icu()),
#'   overflow being at best holding-standard care, or under the legacy mechanism
#'   the raised ceiling (legacy_hold_penalty()).
#'
#'   A stabilisation serves `r2e_post_op_min` and records `r2e_post_op_start`,
#'   `post_op_pathway` and, in a holding bed, `r2e_post_op_hold_start`; a
#'   post-definitive stay serves `post_definitive_min` and records
#'   `post_definitive_start`, `post_definitive_pathway` and
#'   `post_definitive_hold_start`. The holding start is what the holding
#'   occupancy split reads (R/airlift.R).
#'
#'   MODEL ASSUMPTION — P1 SURGERY WITHOUT ICU: a surgeon operates on a
#'   Priority 1 casualty even when no post-operative intensive care bed is
#'   available, accepting elevated post-operative mortality risk rather than
#'   withholding surgery. See README Further Development for basis,
#'   uncertainty, and consequence.
r2e_placed_stay <- function(label, stay, icu_beds, hold_beds, overflow) {
  stabilisation <- identical(stay, "stabilisation")
  minutes_attr <- if (stabilisation) "r2e_post_op_min" else "post_definitive_min"
  #' Records the placement taken
  #'
  #' @param bed 1 intensive care, 2 holding bed, 3 overflow.
  #' @return A trajectory setting this stay's placement attribute.
  record_bed <- function(bed) {
    force(bed)
    if (stabilisation) return(trajectory() %>% set_attribute("post_op_pathway", bed))
    trajectory() %>% set_attribute("post_definitive_pathway", bed)
  }
  record_start <- if (stabilisation) {
    trajectory() %>% set_attribute("r2e_post_op_start", function() now(env))
  } else {
    trajectory() %>% set_attribute("post_definitive_start", function() now(env))
  }
  record_hold <- if (stabilisation) {
    trajectory() %>% set_attribute("r2e_post_op_hold_start", function() now(env))
  } else {
    trajectory() %>% set_attribute("post_definitive_hold_start", function() now(env))
  }

  # Branches on the best placement free at this instant (placement_now()):
  # - 1 an intensive care bed is free         -> intensive care
  # - 2 none, but a holding bed is free        -> holding bed
  # - 3 neither                                -> overflow, with no bed
  trajectory(label) %>%
    join(record_start) %>%
    branch(
      option = function() placement_now(icu_beds, hold_beds),
      continue = TRUE,
      trajectory(paste(label, "in Intensive Care")) %>%
        join(record_bed(1)) %>%
        simmer::select(icu_beds, policy = "first-available", id = 6) %>%
        seize_selected(id = 6) %>%
        dow_placement(post_op_r2e_icu) %>%
        timeout_from_attribute(minutes_attr) %>%
        release_selected(id = 6),
      trajectory(paste(label, "in a Holding Bed")) %>%
        join(record_bed(2)) %>%
        legacy_hold_penalty() %>%
        simmer::select(hold_beds, policy = "first-available", id = 8) %>%
        seize_selected(id = 8) %>%
        join(record_hold) %>%
        dow_placement(post_op_outside_icu) %>%
        timeout_from_attribute(minutes_attr) %>%
        release_selected(id = 8),
      trajectory(paste(label, "in Overflow")) %>%
        join(record_bed(3)) %>%
        legacy_hold_penalty() %>%
        seize(overflow, 1) %>%
        overflow_enter() %>%
        dow_placement(post_op_outside_icu) %>%
        timeout_from_attribute(minutes_attr) %>%
        release(overflow, 1) %>%
        overflow_leave()
    ) %>%
    dow_placement(post_op_outside_icu)
}

#' Builds the stabilisation phase served between two operations at R2E
#'
#' @param icu_beds    This R2E team's intensive care beds
#' @param hold_beds   This R2E team's holding beds
#' @param overflow    This R2E team's post-operative overflow resource
#' @param post_op_dow The post-operative died-of-wounds check this phase ends in
#' @return Simmer trajectory serving whatever stabilisation the forward echelon
#'   did not, then applying the post-operative check.
#'
#' @details Where the stabilisation phase sits in the sequence depends on which
#'   operation this casualty's definitive repair is. A casualty operated on at
#'   R2B has their definitive repair here, so any remaining stabilisation must
#'   come before it (r2e_pre_surgery_stabilisation()). A casualty not operated
#'   on forward has their abbreviated operation here and their definitive one in
#'   Phase 4, so stabilisation sits between them, which is where this trajectory
#'   is joined. Splitting it this way is what puts the resuscitation phase
#'   between the two operations on both routes, rather than after both of them
#'   on one.
#'
#'   A single-stage casualty has no interval between two operations and so no
#'   stabilisation phase; they pass straight to the post-operative check. The
#'   stay is placed when the operation ends (r2e_placed_stay()), and
#'   post_op_pathway records where it was served.
r2e_stabilisation_recovery <- function(icu_beds, hold_beds, overflow, post_op_dow) {
  stay <- r2e_placed_stay("R2E Damage Control Stabilisation", "stabilisation",
                          icu_beds, hold_beds, overflow)
  trajectory("R2E Stabilisation") %>%
    branch(
      # A casualty operated on at R2B took their stabilisation before this
      # procedure, and must not momentarily occupy a bed to repeat it.
      option = function() {
        if (single_stage()) return(2)
        prior <- get_attribute(env, "r2b_surgery")
        if (!is.na(prior) && prior == 1) return(2)
        return(1)
      },
      continue = TRUE,
      trajectory("R2E Damage Control Stabilisation Stay") %>%
        set_attribute("stabilisation_total", function() draw_stabilisation_icu()) %>%
        set_attribute("r2e_post_op_min", function() r2e_stabilisation_minutes()) %>%
        join(stay),
      # The nominal pathway is recorded where no stay is due here, unless a
      # stabilisation served before the repair has already recorded its bed.
      trajectory("No Stabilisation Due at This Step") %>%
        set_attribute("post_op_pathway", function() {
          served <- get_attribute(env, "post_op_pathway")
          if (is.na(served)) 1 else served
        })
    ) %>%
    join(post_op_dow)
}

#' Builds the stabilisation outstanding before a definitive repair at R2E
#'
#' @param icu_beds  This R2E team's intensive care beds
#' @param hold_beds This R2E team's holding beds
#' @param overflow  This R2E team's post-operative overflow resource
#' @return Simmer trajectory serving the remaining stabilisation of a casualty
#'   operated on at R2B, before their definitive repair here.
#'
#' @details Nothing is due when the forward echelon served the whole
#'   requirement, which is what a forward share of one produces. Served before
#'   the repair whatever the state of intensive care, the stay being placed
#'   rather than queued for (r2e_placed_stay()).
r2e_pre_surgery_stabilisation <- function(icu_beds, hold_beds, overflow) {
  stay <- r2e_placed_stay("R2E Pre-Definitive Stabilisation", "stabilisation",
                          icu_beds, hold_beds, overflow)
  trajectory("R2E Pre-Definitive Stabilisation") %>%
    branch(
      option = function() {
        if (single_stage()) return(2)
        prior <- get_attribute(env, "r2b_surgery")
        if (is.na(prior) || prior != 1) return(2)
        if (r2e_stabilisation_minutes() <= 0) return(2)
        return(1)
      },
      continue = TRUE,
      trajectory("R2E Pre-Definitive Stabilisation Stay") %>%
        set_attribute("r2e_post_op_min", function() r2e_stabilisation_minutes()) %>%
        join(stay),
      trajectory("No Stabilisation Outstanding")
    )
}

#' Builds the operation itself at R2E, from theatre seizure to closure
#'
#' @param ot_beds This R2E team's operating theatre beds
#' @param surg    This R2E team's surgical sections, as team_sections() returns
#'   them
#' @param ot_pool This R2E team's theatre pool
#' @return Simmer trajectory performing one operation on the first section
#'   free.
#'
#' @details Recovery is placed when the operation ends (r2e_placed_stay()) and
#'   joined on afterwards, so this portion is the same on every route.
r2e_ot_surgery <- function(ot_beds, surg, ot_pool) {
  r2e_surgery_block(
    4, "r2e_surgery_1_start", "r2e_surgery_1_end",
    definitive_efficacy(
      LEGACY_DOW_EFFICACY$r2e_dcs1_factor,
      LEGACY_DOW_EFFICACY$r2e_dcs2_factor
    ), TRUE, ot_beds, surg, ot_pool
  )
}

#' Minutes of post-definitive intensive care still to be served at R2E
#'
#' @return The requirement drawn at R2B less the minutes served there, for a
#'   single-stage casualty whose post-definitive care was held forward; a fresh
#'   draw otherwise
#'
#' @details A casualty whose requirement was drawn at R2B (`post_definitive_total`
#'   set) owes R2E the remainder, so the total is the same on every route. Where
#'   no requirement was drawn forward the draw is taken here, as it always was,
#'   which is what leaves the disabled configuration's stream untouched.
r2e_post_definitive_minutes <- function() {
  total <- get_attribute(env, "post_definitive_total")
  if (is.na(total)) return(draw_post_definitive_icu())
  max(0, total - forward_served())
}

#' Builds the post-definitive intensive care episode at R2E
#'
#' @param icu_beds  This R2E team's intensive care beds
#' @param hold_beds This R2E team's holding beds
#' @param overflow  This R2E team's post-operative overflow resource
#' @param team_id   Integer index of the Role 2E Heavy team
#' @param evac_team This R2E team's evacuation section
#' @return Simmer trajectory serving the episode that follows the definitive
#'   repair, placed in the best bed free when it begins.
#'
#' @details Joined after Phase 4 for every casualty who had an operation, on
#'   either route. This is the episode that follows the definitive repair,
#'   ventilation weaning, organ support, watching for complications, as distinct
#'   from the stabilisation phase that precedes it. Guideline-recommended
#'   standard after major trauma surgery; see README "Died of Wounds —
#'   Post-Operative Checkpoint" for the citation.
#'
#'   Unlike stabilisation, this episode is never served forward: R2B performs no
#'   definitive repair, so there is nothing for it to follow there. That is what
#'   stops the forward-holding lever from emptying out post-definitive care as
#'   the share rises.
#'
#'   The episode is placed like every post-operative stay (r2e_placed_stay()):
#'   intensive care if a bed is free, otherwise a holding bed, otherwise
#'   overflow. The placement is recorded in post_definitive_pathway (1 = ICU,
#'   2 = hold, 3 = overflow). The requirement is read before the placement, which
#'   draws no random number of its own. The episode ends in its own
#'   died-of-wounds check (r2e_post_definitive_dow_check()).
r2e_post_definitive_care <- function(icu_beds, hold_beds, overflow, team_id, evac_team) {
  post_definitive_check <- r2e_post_definitive_dow_check(team_id, evac_team)
  stay <- r2e_placed_stay("R2E Post-Definitive Care", "post_definitive",
                          icu_beds, hold_beds, overflow)

  # Branches on what the casualty owes this echelon:
  # - 1 an operation with post-definitive care still owed -> the placed stay
  # - 2 no operation, or released with the repair outstanding -> nothing
  # - 3 operated, the care served in full at R2B -> nothing, recorded as zero
  trajectory("R2E Post-Definitive Care") %>%
    branch(
      option = function() {
        had_surgery <- get_attribute(env, "surgery")
        if (is.na(had_surgery) || had_surgery != 1) return(2)
        # A casualty released with the definitive repair outstanding has no
        # post-definitive episode to serve: the episode this trajectory models
        # follows a repair that has not happened.
        outstanding <- get_attribute(env, "definitive_repair_outstanding")
        if (!is.na(outstanding) && outstanding == 1) return(2)
        # Served in full at R2B: nothing remains for this echelon. The
        # attribute is set to zero so the casualty still reads as operated.
        owed <- get_attribute(env, "post_definitive_total") - forward_served()
        if (!is.na(owed) && owed <= 0) return(3)
        return(1)
      },
      continue = TRUE,
      trajectory("R2E Post-Definitive Stay") %>%
        set_attribute("post_definitive_min", function() r2e_post_definitive_minutes()) %>%
        join(stay) %>%
        join(post_definitive_check),
      trajectory("No Operation, No Post-Definitive Care"),
      trajectory("Post-Definitive Care Served Forward in Full") %>%
        set_attribute("post_definitive_min", 0)
    )
}

#' Builds the deferred surgical pathway at R2E
#'
#' @param icu_beds This R2E team's intensive care beds
#' @param icu_path The nominal intensive-care-available surgical pathway, taken
#'   once a bed frees
#' @return Simmer trajectory polling for intensive care headroom, then
#'   proceeding exactly as the nominal path.
#'
#' @details Reached when intensive care is full and the casualty's priority is
#'   above the P1 override threshold. Theatre entry is deferred rather than
#'   proceeding without intensive care backup; the casualty polls availability
#'   every icu_gating$defer_check_interval minutes (timeout plus rollback, no
#'   resources held while waiting) until a bed frees.
r2e_surgery_defer_path <- function(icu_beds, icu_path) {
  trajectory("R2E Surgery — Deferred (ICU Full, P2+)") %>%
    set_attribute("surgery_deferred", 1) %>%
    timeout(function() env_data$vars$r2eheavy$icu_gating$defer_check_interval) %>%
    rollback(target = 1, check = function() {
      usage <- sum(get_server_count(env, resources = icu_beds))
      cap   <- sum(get_capacity(env, resources = icu_beds))
      !(!is.na(usage) && !is.na(cap) && usage < cap)
    }) %>%
    join(icu_path)
}

#' Builds the wait for aeromedical evacuation capacity and the boarding itself
#'
#' @param resource_name Name of the aeromedical evacuation pool to board,
#'   "ame" or "ame_critical"
#' @param bed_id        simmer selection id of the bed the casualty is staged in
#' @param team_id       Integer index of the Role 2E Heavy team
#' @param evac_team     This R2E team's evacuation section
#' @return Simmer trajectory queueing the casualty on the pool until a sortie
#'   boards them.
#'
#' @details A pool has capacity only at the instant a sortie boards it
#'   (build_ame_sortie_trajectory()), so the casualty waits queued on it and
#'   boards the first sortie with a place free, in arrival order. Under the
#'   legacy mechanism the wait carries no died-of-wounds risk: its daily poll
#'   could not be kept, a polling casualty missing every sortie, and its
#'   increments were near zero for Priority 1 and 2, whose curve has risen
#'   fully within hours of injury.
r2e_ame_wait_and_board <- function(resource_name, bed_id, team_id, evac_team) {
  if (dow_care_state_enabled()) {
    return(r2e_ame_queue_and_board(resource_name, bed_id, team_id, evac_team))
  }
  trajectory("Awaiting AME") %>% seize(resource_name, 1)
}

#' The expected deaths of the evacuation wait so far, added to the running total
#'
#' @return The running `dow_expected_late` total with the risk accrued over the
#'   wait added, for a casualty carrying a death time; unchanged otherwise
#'
#' @details The wait's contribution to a cohort's expected deaths is the risk
#'   it accrues, the rate times the time at risk, which ends at boarding
#'   or at death, whichever comes first. See dow_settle_check() for how the
#'   checks contribute.
ame_wait_expected_late <- function() {
  late <- get_attribute(env, "dow_expected_late")
  if (is.na(late)) late <- 0
  if (is.na(get_attribute(env, "ame_death_time"))) return(late)
  late + ame_wait_rate() * (now(env) - get_attribute(env, "r2e_departure_time"))
}

#' The death rate of an operated casualty awaiting airlift, per minute
#'
#' @return The base rate times the post-operative multiplier, the care of a
#'   casualty staged for flight being taken as equal to intensive care's
ame_wait_rate <- function() dow_base_rate() * dow_multipliers()$post_op

#' Draws the moment an operated casualty would die awaiting evacuation
#'
#' @return An absolute simulation time drawn at the wait's rate
#'   (ame_wait_rate()) from the present, or NA for a casualty not operated on,
#'   one exempt from died-of-wounds risk, or one whose rate is zero (no random
#'   number is drawn for any of them)
#'
#' @details Drawn once, when the evacuation disposition is decided, so the wait
#'   is a single exponential competing risk rather than a series of polls: a
#'   casualty boards the first sortie with a seat for them unless the drawn
#'   moment comes first.
draw_ame_death_time <- function() {
  r2b_surg <- get_attribute(env, "r2b_surgery")
  r2e_surg <- get_attribute(env, "r2e_surgery")
  operated <- (!is.na(r2b_surg) && r2b_surg == 1) || (!is.na(r2e_surg) && r2e_surg == 1)
  rate <- ame_wait_rate()
  if (!operated || rate <= 0) return(NA_real_)
  now(env) + stats::rexp(1, rate)
}

#' Builds the queued wait for an airlift seat under the care-state model
#'
#' @param resource_name Name of the aeromedical evacuation pool to board,
#'   "ame" or "ame_critical"
#' @param bed_id        simmer selection id of the bed the casualty is staged in
#' @param team_id       Integer index of the Role 2E Heavy team
#' @param evac_team     This R2E team's evacuation section
#' @return Simmer trajectory queueing the casualty on the pool and boarding
#'   them, or ending the wait in death at their drawn moment.
#'
#' @details The casualty stays queued on the seat pool throughout, so they
#'   board at the instant a sortie brings seats rather than at the next poll.
#'   A casualty carrying a death time reneges from the queue at it, keeping the
#'   bed they are staged in so the death arm releases it and records the
#'   release in ame_hold_end; boarding cancels the renege. A death time already
#'   passed when the casualty is staged, which a ventilated casualty's
#'   pre-flight intensive care stay can carry them beyond, ends the wait at
#'   once. dow_echelon = 5 records a death here.
r2e_ame_queue_and_board <- function(resource_name, bed_id, team_id, evac_team) {
  died <- trajectory("Died of Wounds — Awaiting AME") %>%
    set_attribute("dow_expected_late", ame_wait_expected_late) %>%
    set_attribute("dow", 1) %>%
    set_attribute("dow_echelon", 5) %>%
    release_selected(id = bed_id) %>%
    overflow_leave(staged_in_overflow) %>%
    set_attribute("ame_hold_end", function() now(env)) %>%
    r2e_treat_kia(team_id, evac_team) %>%
    r2e_transport_kia(team_id, evac_team) %>%
    simmer::leave(1)

  # Branches on the casualty's drawn death time:
  # - already passed      -> dies on staging
  # - still to come       -> queues for a seat, reneging to death at it
  # - none (not at risk)  -> queues for a seat
  trajectory("Awaiting AME") %>%
    branch(
      option = function() {
        death <- get_attribute(env, "ame_death_time")
        if (is.na(death)) return(3)
        if (death <= now(env)) return(1)
        2
      },
      continue = TRUE,
      died,
      trajectory("Queued, At Risk") %>%
        renege_in(function() get_attribute(env, "ame_death_time") - now(env),
                  out = died, keep_seized = TRUE) %>%
        seize(resource_name, 1) %>%
        renege_abort(),
      trajectory("Queued") %>%
        seize(resource_name, 1)
    ) %>%
    set_attribute("dow_expected_late", ame_wait_expected_late)
}

#' Draws a casualty's expected recovery-to-duty duration in days
#'
#' @return A single duration in days, drawn from the shared convalescence
#'   distribution scaled by the casualty's severity category.
#'
#' @details A theatre evacuation policy is a duration threshold, "a theater that
#'   evacuates out of the theater all patients requiring 30 or more days of
#'   hospitalization is said to have a '30-day evacuation policy'" [[55]], so
#'   disposition needs a per-casualty clinical duration to compare against the
#'   policy rather than a fixed share of admissions. The draw is the shared base
#'   convalescence distribution (r2eheavy$holding) scaled by a severity factor
#'   keyed to the same four categories R/analysis_role4_ame.R::assign_role4_los() uses, so
#'   a casualty's prognosis, its Role 4 ward and its evacuation route all follow
#'   from one severity classification rather than from independent draws. The
#'   one exception is a Priority 1 disease casualty, who convalesces on the
#'   Priority 2 factor but keeps the disease ward category at Role 4.
draw_recovery_to_duty <- function() {
  # A casualty's convalescence is drawn once, where their severity and
  # treatment are first known (R2B no-surgery path, or here), and every echelon
  # serves part of that one duration.
  drawn <- get_attribute(env, "recovery_to_duty_days")
  if (!is.na(drawn)) return(drawn)

  prio  <- get_attribute(env, "priority")
  dtype <- get_attribute(env, "dnbi_type")
  surg  <- get_attribute(env, "r2b_surgery")
  surg2 <- get_attribute(env, "r2e_surgery")
  had_surgery <- (!is.na(surg) && surg == 1) || (!is.na(surg2) && surg2 == 1)
  f <- env_data$vars$r2eheavy$recovery_to_duty
  # Disease keys on priority alone: a medical emergency convalesces as a
  # Priority 2 casualty, and every other disease casualty, surgical included,
  # as a minor one. A non-battle injury is trauma and takes the wounded
  # factors below, by priority and surgery.
  severity <- if (!is.na(dtype) && dtype == 2L) {
    if (!is.na(prio) && prio == 1) f$p2 else f$p3_dnbi
  } else if (!is.na(prio) && prio == 3) {
    f$p3_dnbi
  } else if (!is.na(prio) && prio == 1 && had_surgery) {
    f$p1_surgical
  } else if (!is.na(prio) && prio == 1) {
    f$p1_nonsurgical
  } else if (!is.na(prio) && prio == 2) {
    f$p2
  } else {
    f$p3_dnbi
  }
  base <- rtriangle(
    n = 1,
    a = env_data$vars$r2eheavy$holding$min,
    b = env_data$vars$r2eheavy$holding$max,
    c = env_data$vars$r2eheavy$holding$mode
  )
  (base * severity) / DAY_MIN
}

#' Builds the staging of an evacuee for flight
#'
#' @param hold_beds This R2E team's holding beds
#' @param overflow  This R2E team's staging overflow resource
#' @return Simmer trajectory staging the casualty in a holding bed if one is
#'   free and in overflow otherwise, selected into id 9 either way.
#'
#' @details Either placement is selected into the same id, so boarding a sortie
#'   and death while waiting each release it with one call whichever it was.
#'   Staging is never queued for, so no evacuee is kept from a place on a sortie
#'   for want of a bed. ame_hold_start is recorded only for a holding bed, the
#'   holding occupancy split (R/airlift.R) reading it as bed time.
r2e_staging_placement <- function(hold_beds, overflow) {
  # Branches on whether a holding bed is free at this instant:
  # - 1 free -> staged in it (r2e_stage_bed = 2)
  # - 2 none -> staged in overflow (r2e_stage_bed = 3)
  trajectory("Staged for Flight") %>%
    set_attribute("ame_stage_start", function() now(env)) %>%
    branch(
      option = function() if (beds_free(hold_beds)) 1 else 2,
      continue = TRUE,
      trajectory("Staged in a Holding Bed") %>%
        set_attribute("r2e_stage_bed", 2) %>%
        simmer::select(hold_beds, policy = "first-available", id = 9) %>%
        seize_selected(id = 9) %>%
        set_attribute("ame_hold_start", function() now(env)),
      trajectory("Staged in Overflow") %>%
        set_attribute("r2e_stage_bed", 3) %>%
        simmer::select(overflow, id = 9) %>%
        seize_selected(id = 9) %>%
        overflow_enter()
    )
}

#' Whether the evacuee is staged in overflow
#'
#' @return TRUE when `r2e_stage_bed` records overflow.
staged_in_overflow <- function() {
  bed <- get_attribute(env, "r2e_stage_bed")
  !is.na(bed) && bed == 3
}

#' Builds the pre-flight care of a casualty awaiting a critical-care sortie
#'
#' @param icu_beds  This R2E team's intensive care beds
#' @param hold_beds This R2E team's holding beds
#' @param overflow  This R2E team's pre-flight overflow resource
#' @param staging   The staging placement built by r2e_staging_placement()
#' @return Simmer trajectory staging the casualty for flight, through a bounded
#'   period of pre-flight care for the ventilated share and directly for the
#'   rest.
#'
#' @details A ventilated casualty awaiting a critical-care sortie genuinely
#'   needs intensive-care-level care, but a deployed intensive care study at
#'   Camp Bastion records that coalition soldiers "are usually evacuated within
#'   24 h of admission" [[56]], so that need is bounded rather than lasting the
#'   whole evacuation wait. A configurable share of the critical pool
#'   (critical_hold$ventilated_share) receives a critical_hold-distributed
#'   period of pre-flight care and then steps down to staging; the rest are
#'   staged immediately.
#'
#'   The pre-flight care is placed like a post-operative stay: intensive care if
#'   a bed is free, otherwise a holding bed, otherwise overflow
#'   (r2e_preflight_bed). A casualty in a holding bed stays in it for staging.
#'   One in intensive care or overflow steps down to staging when the period
#'   ends, a holding bed if free and overflow if not, taken before the
#'   intensive care bed is released and never waited for, so a full holding pool
#'   cannot block intensive care.
r2e_critical_pre_flight_care <- function(icu_beds, hold_beds, overflow, staging) {
  # Branches on whether the casualty is ventilated:
  # - 1 the definitive repair is outstanding, or the casualty is drawn into the
  #     ventilated share -> pre-flight care, then staging
  # - 2 otherwise -> staged at once
  trajectory("Pre-Flight Critical Care") %>%
    branch(
      option = function() {
        # The ventilated share is the rate among casualties whose repair is
        # complete. One awaiting a definitive repair holds an intensive care bed
        # for the whole pre-flight period rather than being sampled into it, and
        # consumes no draw, so the shipped configuration is unaffected.
        outstanding <- get_attribute(env, "definitive_repair_outstanding")
        if (!is.na(outstanding) && outstanding == 1) return(1)
        if (runif(1) < env_data$vars$r2eheavy$critical_hold$ventilated_share) return(1)
        return(2)
      },
      continue = TRUE,
      trajectory("Ventilated — Pre-Flight Care") %>%
        set_attribute("ame_icu_hold", 1) %>%
        set_attribute("ame_preflight_min", function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$critical_hold$min,
            b = env_data$vars$r2eheavy$critical_hold$max,
            c = env_data$vars$r2eheavy$critical_hold$mode
          )
        }) %>%
        # Branches on the best placement free at this instant (placement_now()):
        # - 1 intensive care, stepping down to staging when the period ends
        # - 2 a holding bed, kept for staging
        # - 3 overflow, stepping down to staging when the period ends
        branch(
          option = function() placement_now(icu_beds, hold_beds),
          continue = TRUE,
          trajectory("Pre-Flight in Intensive Care") %>%
            set_attribute("r2e_preflight_bed", 1) %>%
            simmer::select(icu_beds, policy = "first-available", id = 10) %>%
            seize_selected(id = 10) %>%
            set_attribute("ame_icu_hold_start", function() now(env)) %>%
            timeout_from_attribute("ame_preflight_min") %>%
            join(staging) %>%
            release_selected(id = 10) %>%
            set_attribute("ame_icu_hold_minutes", function() {
              now(env) - get_attribute(env, "ame_icu_hold_start")
            }),
          trajectory("Pre-Flight in a Holding Bed") %>%
            set_attribute("r2e_preflight_bed", 2) %>%
            set_attribute("r2e_stage_bed", 2) %>%
            set_attribute("ame_stage_start", function() now(env)) %>%
            simmer::select(hold_beds, policy = "first-available", id = 9) %>%
            seize_selected(id = 9) %>%
            set_attribute("ame_hold_start", function() now(env)) %>%
            timeout_from_attribute("ame_preflight_min"),
          trajectory("Pre-Flight in Overflow") %>%
            set_attribute("r2e_preflight_bed", 3) %>%
            seize(overflow, 1) %>%
            overflow_enter() %>%
            timeout_from_attribute("ame_preflight_min") %>%
            release(overflow, 1) %>%
            overflow_leave() %>%
            join(staging)
        ),
      trajectory("Stable — Staged for Flight") %>%
        set_attribute("ame_icu_hold", 0) %>%
        join(staging)
    )
}

#' Applies the R2E arrival died-of-wounds check
#'
#' @param trj       Trajectory to append the check to
#' @param team_id   Integer index of the Role 2E Heavy team
#' @param evac_team This R2E team's evacuation section
#' @return The trajectory, with the check, its mortuary arm and the stamp of the
#'   check time appended.
#'
#' @details The conditional increment runs from the last check to the elapsed
#'   time on arrival here, so the journey to this echelon is priced at this
#'   checkpoint. dow_echelon = 3 distinguishes it from the post-operative check.
r2e_arrival_dow_check <- function(trj, team_id, evac_team) {
  died <- trajectory("Died of Wounds") %>%
    set_attribute("dow", 1) %>%
    set_attribute("dow_echelon", 3) %>%
    r2e_treat_kia(team_id, evac_team) %>%
    r2e_transport_kia(team_id, evac_team) %>%
    simmer::leave(1)

  if (dow_care_state_enabled()) {
    return(trj %>% dow_check(cohort = TRUE, died = died,
                             survived = trajectory("Continue R2E Treatment")))
  }

  # Time-dependent logistic.
  # Disease DNBI (dnbi_type == 2) exempt — medical pathway, not trauma.
  # P3 casualties use a flat probability (minor wounds, not time-critical).
  trj %>%
    branch(
      option = function() {
        dtype  <- get_attribute(env, "dnbi_type")
        if (!is.na(dtype) && dtype == 2L) return(2)  # disease: exempt from DOW
        injury <- get_attribute(env, "injury_time")
        t_prev <- get_attribute(env, "last_dow_t") - injury
        t_now  <- now(env) - injury
        prio   <- get_attribute(env, "priority")
        dp      <- LEGACY_DOW_PARAMS
        ceiling <- get_attribute(env, "dow_ceiling")
        if (prio == 1) {
          p <- dow_prob_conditional(t_now, t_prev,
                 dp$p1_p_base, ceiling, dp$p1_k, dp$p1_t_mid)
          if (runif(1) < p) return(1)
        } else if (prio == 2) {
          p <- dow_prob_conditional(t_now, t_prev,
                 dp$p2_p_base, ceiling, dp$p2_k, dp$p2_t_mid)
          if (runif(1) < p) return(1)
        } else {
          if (runif(1) < dp$p3_flat) return(1)
        }
        return(2)
      },
      continue = TRUE,
      died,
      trajectory("Continue R2E Treatment")
    ) %>%
    set_attribute("last_dow_t", function() now(env))
}

#' Applies resuscitation at R2E
#'
#' @param trj            Trajectory to append the resuscitation to
#' @param resus_beds     This R2E team's resuscitation beds
#' @param emergency_team This R2E team's emergency sections, as team_sections()
#'   returns them; the first to come free resuscitates the casualty
#' @return The trajectory, with the resuscitation bed seizure and the
#'   resuscitation itself appended.
#'
#' @details A casualty resuscitated at R2B takes the short form here; one
#'   arriving without a forward resuscitation takes the full form, which sets
#'   r2e_resus and earns the resuscitation treatment effect.
r2e_resuscitation <- function(trj, resus_beds, emergency_team) {
  trj %>%
    simmer::select(resus_beds, policy = "shortest-queue", id = 2) %>%
    seize_selected(id = 2) %>%

    # Phase 2 (cont.): Emergency resuscitation
    # Branches based on "r2b_resus":
    # - r2b_resus == 1 → short resus (prior resus at R2B)
    # - else           → full resus, sets r2e_resus = 1
    seize_first_free_section(emergency_team) %>%
    set_attribute("r2e_emerg_section", function() get_attribute(env, "section_held")) %>%
    branch(
      option = function() {
        attr <- get_attribute(env, "r2b_resus")
        if (!is.na(attr) && attr == 1) return(1)
        return(2)
      },
      continue = TRUE,
      trajectory() %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$short_resus$min,
            b = env_data$vars$r2eheavy$short_resus$max,
            c = env_data$vars$r2eheavy$short_resus$mode
          )
        }) %>%
        release_held_section(emergency_team) %>%
        release_selected(id = 2),
      trajectory() %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$long_resus$min,
            b = env_data$vars$r2eheavy$long_resus$max,
            c = env_data$vars$r2eheavy$long_resus$mode
          )
        }) %>%
        set_attribute("r2e_resus", 1) %>%
        dow_treat(function() dow_multipliers()$resuscitation,
                  function() LEGACY_DOW_EFFICACY$r2e_resus_factor, resuscitation = TRUE) %>%
        release_held_section(emergency_team) %>%
        release_selected(id = 2)
    )
}

#' Applies the R2E surgical branch and its pre-theatre intensive care gate
#'
#' @param trj         Trajectory to append the branch to
#' @param icu_beds    This R2E team's intensive care beds
#' @param icu_path    The surgical pathway, taken when a bed is free and by a
#'   Priority 1 casualty when none is
#' @param defer_path  The deferred pathway, taken by a Priority 2+ casualty when
#'   none is
#' @param post_op_dow The post-operative check, applied on its own to a casualty
#'   whose definitive repair was performed forward
#' @return The trajectory, with the surgical branch appended.
r2e_surgical_branch <- function(trj, icu_beds, icu_path, defer_path, post_op_dow) {
  trj %>%
    # Branches based on attribute "surgery":
    # - surgery == 1 → check this team's ICU bed availability before OT entry:
    #     - ICU available, or ICU full and priority <=
    #       icu_gating$p1_bypass_priority_max (P1)            → icu_path
    #         (any stabilisation owed, the operation and the stabilisation
    #         after it, each placed in the best bed free when it begins)
    #     - ICU full, priority above threshold (P2+)           → defer_path
    #         (OT entry deferred; polls ICU availability on a timer, then
    #         proceeds as icu_path)
    # - surgery == 1 but the definitive repair was already performed forward
    #   (single-stage casualty operated on at R2B) → no theatre here; the
    #   post-operative DOW checkpoint alone, so the checkpoint is not skipped
    #   for the one route that reaches R2E already repaired
    # - surgery != 1 → no surgery needed
    branch(
      option = function() {
        needs_surg <- get_attribute(env, "surgery")
        if (is.na(needs_surg) || needs_surg != 1) return(3)

        prior <- get_attribute(env, "r2b_surgery")
        if (single_stage() && !is.na(prior) && prior == 1) return(4)

        # Gate disabled: the pre-gate model, in which no operation is deferred
        # for want of an intensive care bed.
        if (!icu_gate_enabled("r2eheavy")) return(1)
        if (beds_free(icu_beds)) return(1)

        prio      <- get_attribute(env, "priority")
        threshold <- env_data$vars$r2eheavy$icu_gating$p1_bypass_priority_max
        if (!is.na(prio) && prio <= threshold) return(1)
        return(2)
      },
      continue = TRUE,
      icu_path,
      defer_path,
      trajectory("No Surgery Needed"),
      trajectory("Definitive Repair Already Performed at R2B") %>%
        join(post_op_dow)
    )
}

#' The configured R2E surgical saturation threshold, in queued casualties
#'
#' @return The threshold, or NA when it is disabled, which is the shipped
#'   default.
#'
#' @details The threshold counts casualties waiting on a surgical section rather
#'   than minutes already waited, so the decision reads the quantity a surgical
#'   team on the ground would: how many cases stand ahead of this one. A
#'   non-positive value disables the release, the convention
#'   `r2b$holding$evac_threshold` also follows. Zero cannot mean an empty queue,
#'   an empty queue being the state in which forward surgical capacity is least
#'   constrained and so the state in which no casualty should be released
#'   unrepaired.
r2e_second_surgery_threshold <- function() {
  thresh <- env_data$vars$r2eheavy$second_surgery$saturation_queue_threshold
  if (is.null(thresh)) return(NA_real_)
  if (length(thresh) != 1L || !is.numeric(thresh) || is.na(thresh) || thresh < 0) {
    stop(sprintf(paste("r2eheavy.second_surgery.saturation_queue_threshold must be",
                       "a single non-negative number, found %s"),
                 paste(format(thresh), collapse = ", ")), call. = FALSE)
  }
  if (thresh <= 0) return(NA_real_)
  as.numeric(thresh)
}

#' The number of casualties waiting to be operated on at one R2E team
#'
#' @param team_id Integer index of the Role 2E Heavy team
#' @return The casualties queued for a surgical section or for a theatre.
#'
#' @details A casualty ready for theatre waits first for the first section to
#'   come free and then, rarely, for a theatre (r2e_surgery_block()), each on
#'   its team's pool, so the backlog is the two pools' queues together. simmer
#'   blocks an arrival on one resource at a time, so each casualty is counted
#'   once, and this is the backlog a surgical team would recognise.
r2e_theatre_queue <- function(team_id) {
  get_queue_count(env, section_pool("r2eheavy", "surg", team_id)) +
    get_queue_count(env, section_pool("r2eheavy", "ot", team_id))
}

#' Whether this R2E team's theatre is saturated
#'
#' @param team_id Integer index of the Role 2E Heavy team
#' @param thresh  The configured threshold, or NA where it is disabled
#' @return TRUE where a threshold is in force and the number of casualties
#'   waiting to be operated on has reached it.
#'
#' @details The queue is read at the instant of the decision and no random
#'   number is drawn, so a disabled threshold leaves the arrival stream exactly
#'   where the model without this branch left it. The threshold is passed in
#'   rather than re-read, having been read and validated once when the
#'   trajectory was built.
r2e_surgery_saturated <- function(team_id, thresh) {
  if (is.na(thresh)) return(FALSE)
  r2e_theatre_queue(team_id) >= thresh
}

#' Applies the second operation of the damage control pathway at R2E
#'
#' @param trj     Trajectory to append the branch to
#' @param team_id Integer index of the Role 2E Heavy team
#' @param ot_beds This R2E team's operating theatre beds
#' @param surg    This R2E team's surgical sections, as team_sections() returns
#'   them
#' @param ot_pool This R2E team's theatre pool
#' @return The trajectory, with the second-operation branch appended.
r2e_second_surgery <- function(trj, team_id, ot_beds, surg, ot_pool) {
  # Read and validated here, at trajectory build, so a malformed threshold is a
  # named configuration error rather than an error raised mid-campaign.
  saturation_threshold <- r2e_second_surgery_threshold()

  trj %>%
    # operation was the R2E Phase 3 one. A second procedure is only meaningful
    # for patients who underwent Phase 3 surgery at R2E (r2e_surgery == 1)
    # without a prior R2B DAMCON (r2b_surgery != 1), and only on the damage
    # control pathway: a single-stage casualty's Phase 3 procedure was already
    # their definitive repair, so there is nothing to return to theatre for.
    # Patients with surgery == 0 never set r2e_surgery, so is.na(r2e_surg) guards them out.
    #
    # Arms:
    #   1 - eligible, and forward surgical capacity is available: return to
    #       theatre for the definitive repair
    #   2 - not eligible: single-stage, no R2E operation, or the abbreviated
    #       operation was performed at R2B and the definitive repair here was
    #       the Phase 3 one
    #   3 - eligible, but the number of casualties waiting to be operated on
    #       has reached second_surgery$saturation_queue_threshold: released to strategic
    #       evacuation with the definitive repair outstanding. In force at the
    #       shipped threshold of 8; a threshold of zero disables it and makes
    #       this arm unreachable.
    branch(
      option = function() {
        if (single_stage()) return(2)
        r2e_surg   <- get_attribute(env, "r2e_surgery")
        prior_surg <- get_attribute(env, "r2b_surgery")
        if (!is.na(r2e_surg) && r2e_surg == 1 &&
            (is.na(prior_surg) || prior_surg != 1)) {
          if (r2e_surgery_saturated(team_id, saturation_threshold)) return(3)
          return(1)
        }
        return(2)
      },
      continue = TRUE,
      # Second procedure, on the first section free when the casualty is due
      # back, the stabilisation between the operations usually spanning a shift
      # change.
      trajectory("Second Surgery Before Disposition") %>%
        join(r2e_surgery_block(
          7, "r2e_surgery_2_start", "r2e_surgery_2_end",
          local({
            factor <- LEGACY_DOW_EFFICACY$r2e_dcs2_factor
            function() factor
          }), FALSE, ot_beds, surg, ot_pool
        )),
      trajectory("No Second Surgery Needed"),
      # Released to strategic evacuation with the definitive repair outstanding.
      # The abbreviated operation and its recovery have already been served
      # here; what is given up is the return to theatre, which becomes demand on
      # the national support base rather than care withheld.
      trajectory("Released Before Definitive Repair — Theatre Saturated") %>%
        set_attribute("definitive_repair_outstanding", 1) %>%
        # The operation is drawn here rather than at the echelon that performs
        # it, so the requirement is conserved by construction: one draw, from
        # the distribution this theatre would have served it under, carried
        # rearward on the casualty. The analysis layer reads it and takes no
        # draw of its own, which is what keeps the Role 4 report a function of
        # the run rather than of the caller's stream position.
        set_attribute("definitive_repair_minutes", function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$surgery$min,
            b = env_data$vars$r2eheavy$surgery$max,
            c = env_data$vars$r2eheavy$surgery$mode
          )
        })
    )
}

#' Builds the strategic evacuation disposition at R2E
#'
#' @param staging       The staging placement built by r2e_staging_placement()
#' @param critical_care The pre-flight critical care trajectory built by
#'   r2e_critical_pre_flight_care()
#' @param team_id       Integer index of the Role 2E Heavy team
#' @param evac_team     This R2E team's evacuation section
#' @return Simmer trajectory staging the casualty for flight, boarding them and
#'   recording the wait.
#'
#' @details r2e_departure_time keeps its original meaning, clinical care
#'   concluded and disposition decided, so the R2E dwell time measure is
#'   unaffected by this branch; the evacuation wait is tracked separately
#'   (ame_departure_time, ame_wait_minutes) so clinical dwell and evacuation
#'   logistics wait remain distinguishable.
#'
#'   Both routes stage in a holding bed if one is free and in overflow if not
#'   (r2e_staging_placement()), the critical/standard split being a
#'   distinction in airlift seat rather than in bed type; ame_route records it
#'   (1 = critical, 2 = standard) for the route-decomposed outputs. See README
#'   Role 4 (National Support Base) Demand Modelling for which casualty each
#'   pool is for and the doctrine that decides it.
#'
#'   ame_hold_end records when the staging placement was released, which
#'   boarding and death while waiting both reach and only boarding sets a
#'   departure time for, so the bed time a staged casualty consumed can be
#'   recovered whichever way it left the pool.
#'
#'   The pool seat is never released: a boarded casualty permanently consumes
#'   that sortie's capacity, and casualties board strictly in decision order, no
#'   further acuity-based boarding priority beyond the critical/standard split
#'   being modelled. See README Further Development.
r2e_strategic_evac <- function(staging, critical_care, team_id, evac_team) {
  # The death time is drawn under the care-state model alone; under the legacy
  # mechanism the wait carries no risk (r2e_ame_wait_and_board()).
  #' Draws the evacuation wait's death time, under the care-state model alone
  #'
  #' @param trj Trajectory to append to.
  #' @return The trajectory, unchanged under the legacy mechanism.
  draw_ame_death <- function(trj) {
    if (!dow_care_state_enabled()) return(trj)
    trj %>% set_attribute("ame_death_time", draw_ame_death_time)
  }

  trajectory("Strategic Evac — Awaiting AME") %>%
    # r2e_departure_time keeps its original meaning — clinical care
    # concluded, disposition decided — so the existing R2E Dwell Time
    # KPI (Domain 2) is unaffected by this branch. AME wait is tracked
    # separately below (ame_departure_time, ame_wait_minutes) rather
    # than folded into r2e_departure_time, so "clinical dwell" and
    # "evacuation logistics wait" remain distinguishable.
    set_attribute("r2e_departure_time", function() now(env)) %>%
    set_attribute("r2e_evac", 1) %>%
    draw_ame_death() %>%
    # evacuation_decision_day / treatment_received: captured
    # when the Strategic Evac disposition is decided. Feeds the Role 4
    # ward/LoS category assignment (R/analysis_role4_ame.R::assign_role4_los())
    # — see README Role 4 sub-section.
    set_attribute("evacuation_decision_day", function() floor(now(env) / DAY_MIN) + 1) %>%
    set_attribute("treatment_received", function() {
      r2b_surg <- get_attribute(env, "r2b_surgery")
      r2e_surg <- get_attribute(env, "r2e_surgery")
      had_surgery <- (!is.na(r2b_surg) && r2b_surg == 1) ||
        (!is.na(r2e_surg) && r2e_surg == 1)
      if (had_surgery) 1 else 0
    }) %>%

    # Branches on which airlift seat the casualty waits for, deciding on
    # priority and whether an operation was performed:
    # - If Priority 1 and operated on, or the definitive repair is still
    #   outstanding, wait on the smaller "ame_critical" pool after any
    #   pre-flight care (r2e_critical_pre_flight_care())
    # - Otherwise be staged and wait on the standard "ame" pool
    # Both arms set ame_route (1 = critical, 2 = standard), which
    # R/analysis.R reads for the route-decomposed wait-time and backlog
    # outputs. The same population routed here is the one assigned the Role 4
    # intensive care ward by assign_role4_los() (R/analysis_role4_ame.R), so the two
    # must be changed together.
    branch(
      option = function() {
        # A casualty flown out with the definitive repair outstanding is by
        # construction not stable enough for a holding bed, whatever their
        # triage priority: the operation they are waiting for is the one that
        # would have made them so.
        outstanding <- get_attribute(env, "definitive_repair_outstanding")
        if (!is.na(outstanding) && outstanding == 1) return(1)
        prio <- get_attribute(env, "priority")
        tx   <- get_attribute(env, "treatment_received")
        if (!is.na(prio) && prio == 1 && !is.na(tx) && tx == 1) return(1)
        return(2)
      },
      continue = TRUE,
      trajectory("Await Critical AME") %>%
        set_attribute("ame_route", 1) %>%
        join(critical_care) %>%
        # The AME pool seat is never released — a boarded casualty
        # permanently consumes that sortie's capacity; no seats are
        # handed back. Casualties board strictly in queue (decision)
        # order — no further acuity-based boarding priority beyond the
        # critical/standard split itself is modelled; see README
        # Further Development.
        join(r2e_ame_wait_and_board("ame_critical", 9, team_id, evac_team)),
      trajectory("Await Standard AME") %>%
        set_attribute("ame_route", 2) %>%
        join(staging) %>%
        join(r2e_ame_wait_and_board("ame", 9, team_id, evac_team))
    ) %>%
    release_selected(id = 9) %>%
    overflow_leave(staged_in_overflow) %>%
    set_attribute("ame_hold_end", function() now(env)) %>%
    set_attribute("ame_departure_time", function() now(env)) %>%
    set_attribute("evacuation_day", function() floor(now(env) / DAY_MIN) + 1) %>%
    set_attribute("ame_wait_minutes", function() {
      now(env) - get_attribute(env, "r2e_departure_time")
    })
}

#' The configured share of Priority 1 surgical casualties needing reconstruction
#'
#' @return The share, or NA where the mechanism is configured off.
#'
#' @details Read from `role4.surgery.reconstruction_share`. An absent block or a
#'   share of zero reads as off, which is what lets a configuration written
#'   before this existed reproduce the model without it.
r4_reconstruction_share <- function() {
  surgery <- env_data$vars$role4$surgery
  if (is.null(surgery) || is.null(surgery$reconstruction_share)) return(NA_real_)
  share <- suppressWarnings(as.numeric(surgery$reconstruction_share))
  if (length(share) != 1L || is.na(share) || share < 0 || share > 1) {
    stop("role4.surgery.reconstruction_share must be a number between 0 and 1; found: ",
         paste(format(surgery$reconstruction_share), collapse = ", "), call. = FALSE)
  }
  if (share <= 0) return(NA_real_)
  share
}

#' Whether this casualty's wounds require reconstruction at the national support base
#'
#' @return 1 where the casualty is drawn into the reconstruction cohort, 0
#'   otherwise.
#'
#' @details The draw is restricted to a Priority 1 casualty operated on in
#'   theatre, which is the population the calibrating cohort is drawn from, and
#'   is taken on the wound rather than on the casualty's expected recovery: a
#'   casualty needing staged soft-tissue coverage is identified by what they are
#'   wounded with, not by how long they would take to return to duty. See README
#'   Role 4 (National Support Base) Demand Modelling for the source and its
#'   cohort.
#'
#'   A share of zero or one consumes no random draw, on the degenerate-rate
#'   convention `draw_dcs_pathway()` also follows, so a configuration turning
#'   this off reproduces the model without it rather than merely approximating
#'   it.
draw_reconstruction_required <- function() {
  share <- r4_reconstruction_share()
  if (is.na(share)) return(0)
  prio     <- get_attribute(env, "priority")
  operated <- get_attribute(env, "surgery")
  if (is.na(prio) || prio != 1) return(0)
  if (is.na(operated) || operated != 1) return(0)
  if (share >= 1) return(1)
  as.numeric(runif(1) < share)
}

#' Why this casualty leaves theatre, or 0 where they are retained
#'
#' @return 0 for a casualty retained in theatre, or the reason they are
#'   evacuated: 1 recovery beyond the theatre evacuation policy, 2 wounds
#'   requiring reconstruction at the national support base, 3 released with the
#'   definitive repair outstanding under forward surgical saturation.
#'
#' @details Reasons 2 and 3 override the policy comparison rather than
#'   competing with it: neither casualty can be returned to duty in theatre
#'   whatever their drawn recovery, one because the operation they are waiting
#'   for cannot be performed here and the other because staged soft-tissue
#'   coverage is weeks of repeated theatre visits this echelon does not provide.
#'   Reason 3 is tested first, being the more acute of the two.
#'
#'   Computed once and recorded on the casualty rather than re-derived in the
#'   branch, so the disposition taken and the reason reported cannot disagree.
draw_evacuation_reason <- function() {
  outstanding <- get_attribute(env, "definitive_repair_outstanding")
  if (!is.na(outstanding) && outstanding == 1) return(3)
  reconstruct <- get_attribute(env, "reconstruction_required")
  if (!is.na(reconstruct) && reconstruct == 1) return(2)
  rtd    <- get_attribute(env, "recovery_to_duty_days")
  policy <- env_data$vars$r2eheavy$recovery$evacuation_policy_days
  if (!is.na(rtd) && rtd <= policy) return(0)
  1
}


#' Minutes of the casualty's one convalescence still to serve at R2E
#'
#' @return The drawn `recovery_to_duty_days` less the bed time already served
#'   forward at R2B (`r2b_hold_served`) and the time spent queueing for that
#'   bed (`r2b_hold_waited`), during which convalescence also ran; floored at
#'   zero.
r2e_convalescence_remaining_minutes <- function() {
  served <- get_attribute(env, "r2b_hold_served")
  waited <- get_attribute(env, "r2b_hold_waited")
  if (is.na(served)) served <- 0
  if (is.na(waited)) waited <- 0
  max(0, get_attribute(env, "recovery_to_duty_days") * DAY_MIN - served - waited)
}

#' Applies the final disposition at R2E under the theatre evacuation policy
#'
#' @param trj           Trajectory to append the disposition to
#' @param hold_beds     This R2E team's holding beds
#' @param staging       The staging placement built by r2e_staging_placement()
#' @param critical_care The pre-flight critical care trajectory reached by a
#'   Priority 1 surgical evacuee
#' @param team_id       Integer index of the Role 2E Heavy team
#' @param evac_team     This R2E team's evacuation section
#' @return The trajectory, with the recovery-to-duty draw and the disposition
#'   branch appended.
r2e_disposition <- function(trj, hold_beds, staging, critical_care, team_id, evac_team) {
  # recovery_to_duty_days is drawn first (draw_recovery_to_duty()), then the
  # reconstruction requirement and the evacuation reason it may decide.
  trj %>%
    # Risk accrued as a rate ends here: convalescence carries none, and the
    # evacuation wait's is a drawn death time (r2e_strategic_evac()).
    dow_risk_ends() %>%
    set_attribute("recovery_to_duty_days", draw_recovery_to_duty) %>%
    set_attribute("reconstruction_required", draw_reconstruction_required) %>%
    set_attribute("evacuation_reason", draw_evacuation_reason) %>%
    branch(
      # The reason is decided once, above, and recorded on the casualty; this
      # branch only acts on it, so the two cannot disagree and the analysis can
      # decompose the evacuated population by what sent each casualty rearward.
      # - 0 → retain in theatre: seize a hold bed for the drawn recovery
      # - anything else → strategic evacuation (r2e_strategic_evac())
      option = function() {
        if (get_attribute(env, "evacuation_reason") == 0) return(1)
        return(2)
      },
      continue = TRUE,
      trajectory("Recover at R2E") %>%
        # Convalescence runs from retention, while the casualty queues for a
        # bed as well as in it, so bed time plus time queued equals the
        # duration the disposition was decided on.
        queue_for_bed(hold_beds, id = 5, budget = r2e_convalescence_remaining_minutes,
                      echelon = 3) %>%
        set_attribute(c("r2e_recovery_hold_start", "r2e_recovery_hold_min"),
                      function() c(now(env), bed_minutes_left())) %>%
        timeout(bed_minutes_left) %>%
        release_selected(id = 5) %>%
        set_attribute("r2e_departure_time", function() now(env)) %>%
        set_attribute("return_day", function() now(env)) %>%
        set_attribute("return_echelon", 3) %>%
        credit_rtd(),
      r2e_strategic_evac(staging, critical_care, team_id, evac_team)
    )
}

#' Models the full R2E Heavy treatment flow for WIA casualties
#'
#' @param team_id Integer ID of the R2E team assigned to receive the casualty
#' @return Simmer trajectory representing clinical care and disposition at R2E
#'
#' @details The body is the sequence of phases a casualty passes through, each a
#'   named builder above: the arrival died-of-wounds check
#'   (r2e_arrival_dow_check()), resuscitation (r2e_resuscitation()), the
#'   surgical branch and its pre-theatre intensive care gate
#'   (r2e_surgical_branch()), the second operation of the damage control
#'   pathway (r2e_second_surgery()), the post-definitive intensive care episode
#'   (r2e_post_definitive_care()) and the final disposition (r2e_disposition()).
#'
#'   Each phase is appended to the same trajectory object rather than joined
#'   into it, so the assembled trajectory is the one the undivided body built.
#'   The sub-trajectories that were joined before are joined still, and are
#'   built here and passed down rather than rebuilt at each use, so the model
#'   holds one object where it held one before.
#'
#'   The two surgical pathways are compositions of those sub-trajectories: the
#'   nominal one takes any outstanding stabilisation, the operation and the
#'   stabilisation phase between the two operations, each stay placed in the
#'   best bed free when it begins; the deferred one waits for intensive care
#'   headroom and then takes the nominal one.
#'
#'   The builders take the resources they use rather than reading this team's
#'   establishment themselves, so what each one touches is visible in its
#'   signature.
r2e_treat_wia <- function(team_id) {
  hold_beds       <- env_data$elms$r2eheavy[[team_id]][["hold_bed"]]
  resus_beds      <- env_data$elms$r2eheavy[[team_id]][["resus_bed"]]
  ot_beds         <- env_data$elms$r2eheavy[[team_id]][["ot_bed"]]
  icu_beds        <- env_data$elms$r2eheavy[[team_id]][["icu_bed"]]
  surg            <- team_sections("r2eheavy", team_id, "surg")
  ot_pool         <- section_pool("r2eheavy", "ot", team_id)

  # The emergency and evacuation sections are taken per casualty, first free,
  # rather than fixed when this route is built.
  emergency_team <- team_sections("r2eheavy", team_id, "emerg")
  evac_team      <- team_sections("r2eheavy", team_id, "evac")

  overflow_post_op    <- overflow_resource("r2e", "post_op", team_id)
  overflow_pre_flight <- overflow_resource("r2e", "pre_flight", team_id)
  overflow_staging    <- overflow_resource("r2e", "staging", team_id)

  post_op_dow     <- r2e_post_op_dow_check(team_id, evac_team)
  stab_recovery   <- r2e_stabilisation_recovery(icu_beds, hold_beds, overflow_post_op,
                                                post_op_dow)
  pre_stab        <- r2e_pre_surgery_stabilisation(icu_beds, hold_beds, overflow_post_op)
  ot_surgery      <- r2e_ot_surgery(ot_beds, surg, ot_pool)
  post_definitive <- r2e_post_definitive_care(icu_beds, hold_beds, overflow_post_op,
                                              team_id, evac_team)
  staging         <- r2e_staging_placement(hold_beds, overflow_staging)
  critical_care   <- r2e_critical_pre_flight_care(icu_beds, hold_beds, overflow_pre_flight,
                                                  staging)

  icu_path <- trajectory("R2E Surgery") %>%
    join(pre_stab) %>%
    join(ot_surgery) %>%
    join(stab_recovery)

  defer_path <- r2e_surgery_defer_path(icu_beds, icu_path)

  trajectory("R2E Treatment") %>%
    set_attribute("r2e_treated", team_id) %>%
    set_attribute("r2e_handling", 1) %>%
    set_attribute("r2e_arrival_time", function() now(env)) %>%

    # Phase 1: DOW check on arrival
    r2e_arrival_dow_check(team_id, evac_team) %>%

    # Phase 2: Resuscitation
      # Branches on needs_resuscitation(): trauma, and disease that is Priority 1
    # or needs surgery, is resuscitated; any other disease is not.
    branch(
      option = function() if (needs_resuscitation()) 1 else 2,
      continue = TRUE,
      trajectory("R2E Resuscitation") %>% r2e_resuscitation(resus_beds, emergency_team),
      trajectory("R2E No Resuscitation")
    ) %>%

    # Phase 3: Surgical branch, gated on intensive care availability
    r2e_surgical_branch(icu_beds, icu_path, defer_path, post_op_dow) %>%

    # Phase 4: Second surgery, for a damage control casualty whose abbreviated
    # operation was the Phase 3 one here
    r2e_second_surgery(team_id, ot_beds, surg, ot_pool) %>%

    # Post-definitive intensive care, after whichever operation was this
    # casualty's definitive repair — the Phase 4 procedure for a casualty
    # operated on only at R2E, the Phase 3 one for a casualty who had their
    # abbreviated operation at R2B.
    join(post_definitive) %>%

    # Phase 5: Final disposition — theatre evacuation policy
    r2e_disposition(hold_beds, staging, critical_care, team_id, evac_team)
}
