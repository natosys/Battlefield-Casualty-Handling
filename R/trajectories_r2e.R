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
#' @param evac_team Character vector of evacuation resource names
#' @return Modified trajectory with mortuary prep sequence appended
r2e_treat_kia <- function(traj, team_id, evac_team) {
  traj %>%
    seize_resources(evac_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2eheavy$kia_treat$min,
        b = env_data$vars$r2eheavy$kia_treat$max,
        c = env_data$vars$r2eheavy$kia_treat$mode
      )
    }) %>%
    release_resources(evac_team)
}

#' Simulates transport of KIA casualty to mortuary at R2E
#'
#' @param traj Simmer trajectory object to append movement steps to
#' @param team_id Integer index of the Role 2E Heavy team
#' @param evac_team Evacuation team resources for the selected R2E team
#' @return Modified trajectory with transport sequence and mortuary_treated flag
r2e_transport_kia <- function(traj, team_id, evac_team) {
  traj %>%
    seize_resources(evac_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2eheavy$kia_transport$min,
        b = env_data$vars$r2eheavy$kia_transport$max,
        c = env_data$vars$r2eheavy$kia_transport$mode
      )
    }) %>%
    set_attribute("mortuary_treated", 1) %>%
    release_resources(evac_team)
}

#' Builds a per-R2E-team mortuary intake sub-trajectory for KIA casualties
#' arriving by road from R2B (the mortuary is modelled as collocated with
#' R2E, not R2B). One small trajectory is built per R2E
#' team at model-graph-construction time — mirroring r2e_treat_wia()'s own
#' team-selection pattern — and dispatched to at runtime via the "r2e"
#' attribute set by r2b_transport_kia() before the branch(). evac_team is
#' resolved once per team here, at build time, matching how r2e_treat_wia()
#' resolves its own evac_team.
#'
#' @param team_id Integer index of the Role 2E Heavy team
#' @return Simmer trajectory appending that team's KIA mortuary intake
r2e_mortuary_intake <- function(team_id) {
  evac_team <- select_subteam("r2eheavy", team_id, "evac")
  trajectory("R2E Mortuary Intake") %>%
    r2e_treat_kia(team_id, evac_team) %>%
    r2e_transport_kia(team_id, evac_team)
}

# ── R2E Heavy WIA pathway builders ────────────────────────────────────────────

#' Builds one operation at an R2E surgical section
#'
#' @param section_id Index of the surgical section within surg_teams
#' @param select_id  simmer selection id used for the OT bed in this block
#' @param start_attr Attribute name recording incision time
#' @param end_attr   Attribute name recording closure time
#' @param efficacy   Zero-argument function returning the dow_ceiling
#'   multiplier to apply on completion, so the multiplier can depend on the
#'   casualty's surgical pathway
#' @param set_flag   Whether to set the r2e_surgery marker attribute
#' @param ot_beds    This R2E team's operating theatre beds
#' @param surg_teams This R2E team's surgical sections
#' @return A simmer trajectory performing one operation
#'
#' @details Surgical sections are seized per casualty, not fixed at build time
#'   as the emergency, evacuation and intensive care sections are, because R2E
#'   fields three of them against two operating theatres and each carries its
#'   own shift. One block is built per section; the caller branches over them on
#'   select_r2e_surg_section().
#'
#'   Seizure order is bed then team, released team then bed, matching
#'   r2b_ot_check_path() so the two echelons cannot deadlock against each
#'   other's ordering. A section already mid-procedure when its shift closes
#'   retains the resources it holds until release, so a shift change cannot
#'   interrupt an operation in progress.
r2e_surgery_block <- function(section_id, select_id, start_attr, end_attr,
                              efficacy, set_flag, ot_beds, surg_teams) {
  force(section_id); force(select_id); force(start_attr)
  force(end_attr);   force(efficacy);  force(set_flag)

  trj <- trajectory(sprintf(R2E_SURGERY_SECTION_FMT, section_id)) %>%
    simmer::select(ot_beds, policy = "shortest-queue", id = select_id) %>%
    seize_selected(id = select_id) %>%
    seize_resources(surg_teams[[section_id]])

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
    release_resources(surg_teams[[section_id]]) %>%
    release_selected(id = select_id) %>%
    set_attribute("dow_ceiling", function() {
      ceiling <- get_attribute(env, "dow_ceiling")
      if (is.na(ceiling)) return(ceiling)
      ceiling * efficacy()
    })
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
  trajectory("R2E Post-Operative DOW Check") %>%
    branch(
      option = function() {
        injury  <- get_attribute(env, "injury_time")
        t_prev  <- get_attribute(env, "last_dow_t") - injury
        t_now   <- now(env) - injury
        prio    <- get_attribute(env, "priority")
        dp      <- env_data$vars$dow$params
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

#' Builds the stabilisation phase served between two operations at R2E
#'
#' @param icu_beds    This R2E team's intensive care beds
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
#'   stabilisation phase; they pass straight to the post-operative check.
r2e_stabilisation_recovery <- function(icu_beds, post_op_dow) {
  trajectory("R2E Stabilisation") %>%
    set_attribute("post_op_pathway", 1) %>%
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
        simmer::select(icu_beds, policy = "shortest-queue", id = 6) %>%
        seize_selected(id = 6) %>%
        timeout(function() r2e_stabilisation_minutes()) %>%
        release_selected(id = 6),
      trajectory("No Stabilisation Due at This Step")
    ) %>%
    join(post_op_dow)
}

#' Builds the stabilisation outstanding before a definitive repair at R2E
#'
#' @param icu_beds This R2E team's intensive care beds
#' @return Simmer trajectory serving the remaining stabilisation of a casualty
#'   operated on at R2B, before their definitive repair here.
#'
#' @details Nothing is due when the forward echelon served the whole
#'   requirement, which is what a forward share of one produces.
r2e_pre_surgery_stabilisation <- function(icu_beds) {
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
        set_attribute("post_op_pathway", 1) %>%
        set_attribute("r2e_post_op_min", function() r2e_stabilisation_minutes()) %>%
        simmer::select(icu_beds, policy = "shortest-queue", id = 6) %>%
        seize_selected(id = 6) %>%
        timeout(function() r2e_stabilisation_minutes()) %>%
        release_selected(id = 6),
      trajectory("No Stabilisation Outstanding")
    )
}

#' Builds the degraded post-operative holding pathway at R2E
#'
#' @param hold_beds   This R2E team's holding beds
#' @param post_op_dow The post-operative died-of-wounds check this pathway ends
#'   in
#' @return Simmer trajectory recovering the casualty in a holding bed at an
#'   elevated died-of-wounds ceiling, then applying the post-operative check.
#'
#' @details Reached when intensive care is full and the casualty's priority is
#'   within the P1 override threshold
#'   (env_data$vars$r2eheavy$icu_gating$p1_bypass_priority_max). Surgery
#'   proceeds; recovery is in a holding bed instead of intensive care.
#'   dow_ceiling is multiplied by r2e_postop_hold_penalty (> 1) to reflect the
#'   elevated mortality risk of reduced post-operative monitoring; see README
#'   "Died of Wounds — Treatment Efficacy Modifiers".
#'
#'   MODEL ASSUMPTION — P1 SURGERY WITHOUT ICU: a surgeon operates on a
#'   Priority 1 casualty even when no post-operative intensive care bed is
#'   available, accepting elevated post-operative mortality risk rather than
#'   withholding surgery (which would expose a Priority 1 casualty who has not
#'   undergone surgery to near-certain death of wounds). See README Further
#'   Development for basis, uncertainty, and consequence.
#'
#'   This is the degraded form of the stabilisation phase, so it applies only to
#'   the damage control pathway. A single-stage casualty has no stabilisation
#'   phase to degrade; their post-operative intensive care is the
#'   post-definitive episode, which carries its own holding-bed fallback.
r2e_hold_recovery <- function(hold_beds, post_op_dow) {
  trajectory("R2E Post-Op Hold Recovery") %>%
    branch(
      option = function() if (single_stage()) 2 else 1,
      continue = TRUE,

      trajectory("R2E Damage Control Post-Op Hold") %>%
        set_attribute("post_op_pathway", 2) %>%
        set_attribute("dow_ceiling", function() {
          ceiling <- get_attribute(env, "dow_ceiling")
          if (is.na(ceiling)) return(ceiling)
          ceiling * env_data$vars$dow$treatment_efficacy$r2e_postop_hold_penalty
        }) %>%
        simmer::select(hold_beds, policy = "shortest-queue", id = 8) %>%
        seize_selected(id = 8) %>%
        # The stay is drawn into an attribute and served from it, rather than
        # drawn at the timeout, so the bed time this arm consumes is recorded
        # rather than inferred; without it the holding pool's occupancy split
        # can only reach this arm as a residual, which absorbs every error in
        # the other components. The draw sits after the seize, where the
        # timeout used to take it, rather than before: a casualty can wait for
        # a bed, and drawing before that wait would move this draw ahead of
        # every arrival drawn during it and so shift the arrival stream.
        set_attribute("r2e_post_op_hold_start", function() now(env)) %>%
        set_attribute("r2e_post_op_hold_min", function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$post_op_hold$min,
            b = env_data$vars$r2eheavy$post_op_hold$max,
            c = env_data$vars$r2eheavy$post_op_hold$mode
          )
        }) %>%
        timeout_from_attribute("r2e_post_op_hold_min") %>%
        release_selected(id = 8),

      trajectory("Single-Stage — No Stabilisation Phase to Degrade")
    ) %>%
    join(post_op_dow)
}

#' Builds the operation itself at R2E, from theatre seizure to closure
#'
#' @param team_id    Integer index of the Role 2E Heavy team
#' @param ot_beds    This R2E team's operating theatre beds
#' @param surg_teams This R2E team's surgical sections
#' @return Simmer trajectory performing one operation at whichever section is
#'   selected on entry.
#'
#' @details Recovery (intensive care against post-operative hold) is decided
#'   upstream at the pre-theatre gate and joined on afterwards, so this portion
#'   is identical for both paths.
r2e_ot_surgery <- function(team_id, ot_beds, surg_teams) {
  # Branch structure: one sub-trajectory per R2E surgical section, selected on
  # entry by select_r2e_surg_section(). Every branch performs the same
  # procedure and differs only in which section's resources it seizes, so the
  # choice affects contention and shift availability, not clinical outcome.
  trajectory("R2E OT — Surgery") %>%
    branch(
      option = function() select_r2e_surg_section(team_id),
      continue = TRUE,
      lapply(seq_along(surg_teams), function(section_id) {
        r2e_surgery_block(
          section_id, 4, "r2e_surgery_1_start", "r2e_surgery_1_end",
          definitive_efficacy(
            env_data$vars$dow$treatment_efficacy$r2e_dcs1_factor,
            env_data$vars$dow$treatment_efficacy$r2e_dcs2_factor
          ), TRUE, ot_beds, surg_teams
        )
      })
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
#' @return Simmer trajectory serving the episode that follows the definitive
#'   repair, in an intensive care bed or, where none is free, a holding bed.
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
#'   It takes the same degraded-care fallback as the post-operative hold
#'   pathway, for the same reason: a casualty who has already been operated on
#'   cannot be made to wait indefinitely for a bed, so when intensive care is
#'   saturated they recover in a holding bed at an elevated dow_ceiling. The
#'   pathway taken is recorded in post_definitive_pathway (1 = ICU, 2 = hold).
r2e_post_definitive_care <- function(icu_beds, hold_beds) {
  trajectory("R2E Post-Definitive Care") %>%
    branch(
      option = function() {
        had_surgery <- get_attribute(env, "surgery")
        if (is.na(had_surgery) || had_surgery != 1) return(3)
        # A casualty released with the definitive repair outstanding has no
        # post-definitive episode to serve: the episode this trajectory models
        # follows a repair that has not happened.
        outstanding <- get_attribute(env, "definitive_repair_outstanding")
        if (!is.na(outstanding) && outstanding == 1) return(3)
        # Served in full at R2B: nothing remains for this echelon. The
        # attribute is set to zero so the casualty still reads as operated.
        owed <- get_attribute(env, "post_definitive_total") - forward_served()
        if (!is.na(owed) && owed <= 0) return(4)
        usage <- sum(get_server_count(env, resources = icu_beds))
        cap   <- sum(get_capacity(env, resources = icu_beds))
        if (!is.na(usage) && !is.na(cap) && usage < cap) return(1)
        return(2)
      },
      continue = TRUE,

      trajectory("R2E Post-Definitive ICU") %>%
        set_attribute("post_definitive_pathway", 1) %>%
        set_attribute("post_definitive_min", function() r2e_post_definitive_minutes()) %>%
        simmer::select(icu_beds, policy = "shortest-queue", id = 6) %>%
        seize_selected(id = 6) %>%
        timeout(function() get_attribute(env, "post_definitive_min")) %>%
        release_selected(id = 6),

      trajectory("R2E Post-Definitive Hold — ICU Full") %>%
        set_attribute("post_definitive_pathway", 2) %>%
        set_attribute("post_definitive_min", function() r2e_post_definitive_minutes()) %>%
        set_attribute("dow_ceiling", function() {
          ceiling <- get_attribute(env, "dow_ceiling")
          if (is.na(ceiling)) return(ceiling)
          ceiling * env_data$vars$dow$treatment_efficacy$r2e_postop_hold_penalty
        }) %>%
        simmer::select(hold_beds, policy = "shortest-queue", id = 8) %>%
        seize_selected(id = 8) %>%
        set_attribute("post_definitive_hold_start", function() now(env)) %>%
        timeout(function() get_attribute(env, "post_definitive_min")) %>%
        release_selected(id = 8),

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

#' Builds the died-of-wounds poll a casualty faces while awaiting evacuation
#'
#' @param resource_name Name of the aeromedical evacuation pool being queued
#'   for, "ame" or "ame_critical"
#' @param bed_id        simmer selection id of the bed the casualty is staged in,
#'   released, and the release recorded in ame_hold_end, if they die during the
#'   wait
#' @param team_id       Integer index of the Role 2E Heavy team
#' @param evac_team     This R2E team's evacuation section
#' @return Simmer trajectory polling the casualty's risk on an interval until
#'   evacuation capacity is available.
#'
#' @details Mirrors r2e_post_op_dow_check()'s conditional-increment roll (the
#'   same dow_prob_conditional() formula, the same priority-based parameters and
#'   dow_ceiling), but polled on an interval (role4.ame.dow_check_interval)
#'   while queued rather than checked once at a fixed transition point. The
#'   evacuation wait itself is unbounded, and unlike every earlier checkpoint
#'   (each of which precedes a further step that would otherwise price in
#'   delay-accrued risk), this is the last checkpoint in the casualty's journey:
#'   without this poll, a casualty who reached strategic evacuation faced zero
#'   further mortality risk regardless of how long the wait was. See README
#'   Died of Wounds — AME Wait Checkpoint.
#'
#'   Availability is checked by the caller immediately on entry, so boarding
#'   proceeds with no artificial minimum wait when capacity is already free,
#'   matching a plain seize()'s fast path exactly. Only if unavailable does the
#'   casualty enter this loop: wait dow_check_interval minutes, roll, then (if
#'   surviving) re-check availability through rollback()'s `check` and loop
#'   again if still unavailable, the same timeout-then-rollback polling pattern
#'   the R2E theatre gate uses for deferral. dow_echelon = 5 distinguishes this
#'   checkpoint from the post-operative check (dow_echelon = 4).
r2e_ame_dow_poll <- function(resource_name, bed_id, team_id, evac_team) {
  trajectory("Awaiting AME — DOW Poll") %>%
    timeout(function() env_data$vars$role4$ame$dow_check_interval, tag = "ame_dow_poll_start") %>%
    branch(
      option = function() {
        injury  <- get_attribute(env, "injury_time")
        t_prev  <- get_attribute(env, "last_dow_t") - injury
        t_now   <- now(env) - injury
        prio    <- get_attribute(env, "priority")
        dp      <- env_data$vars$dow$params
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
      trajectory("Died of Wounds — Awaiting AME") %>%
        set_attribute("dow", 1) %>%
        set_attribute("dow_echelon", 5) %>%
        release_selected(id = bed_id) %>%
        # The staging bed ends here as well as at boarding, and this is the
        # only route out of it that sets no departure time. Recording the
        # release on the casualty rather than inferring it from the departure
        # is what keeps the holding pool's occupancy split from charging this
        # casualty's bed to the window's close.
        set_attribute("ame_hold_end", function() now(env)) %>%
        r2e_treat_kia(team_id, evac_team) %>%
        r2e_transport_kia(team_id, evac_team) %>%
        simmer::leave(1),
      trajectory("Survived Poll Interval")
    ) %>%
    set_attribute("last_dow_t", function() now(env)) %>%
    rollback(target = "ame_dow_poll_start", check = function() {
      usage <- get_server_count(env, resource_name)
      cap   <- get_capacity(env, resource_name)
      !(!is.na(usage) && !is.na(cap) && usage < cap)
    })
}

#' Builds the wait for aeromedical evacuation capacity and the boarding itself
#'
#' @param resource_name Name of the aeromedical evacuation pool to board,
#'   "ame" or "ame_critical"
#' @param bed_id        simmer selection id of the bed the casualty is staged in
#' @param team_id       Integer index of the Role 2E Heavy team
#' @param evac_team     This R2E team's evacuation section
#' @return Simmer trajectory boarding the casualty, at once where the pool has
#'   capacity and after the polled wait where it does not.
r2e_ame_wait_and_board <- function(resource_name, bed_id, team_id, evac_team) {
  trajectory("Awaiting AME") %>%
    branch(
      option = function() {
        usage <- get_server_count(env, resource_name)
        cap   <- get_capacity(env, resource_name)
        if (!is.na(usage) && !is.na(cap) && usage < cap) return(2)
        return(1)
      },
      continue = TRUE,
      r2e_ame_dow_poll(resource_name, bed_id, team_id, evac_team),
      trajectory("Available Immediately")
    ) %>%
    seize(resource_name, 1)
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
#'   from one severity classification rather than from independent draws.
draw_recovery_to_duty <- function() {
  # A casualty's convalescence is drawn once, where their severity and
  # treatment are first known (R2B no-surgery path, or here), and every echelon
  # serves part of that one duration.
  drawn <- get_attribute(env, "recovery_to_duty_days")
  if (!is.na(drawn)) return(drawn)

  prio  <- get_attribute(env, "priority")
  itype <- get_attribute(env, "injury_type")
  surg  <- get_attribute(env, "r2b_surgery")
  surg2 <- get_attribute(env, "r2e_surgery")
  had_surgery <- (!is.na(surg) && surg == 1) || (!is.na(surg2) && surg2 == 1)
  f <- env_data$vars$r2eheavy$recovery_to_duty
  severity <- if (!is.na(itype) && itype == 2) {
    f$p3_dnbi
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

#' Builds the pre-flight care of a casualty awaiting a critical-care sortie
#'
#' @param icu_beds  This R2E team's intensive care beds
#' @param hold_beds This R2E team's holding beds
#' @return Simmer trajectory staging the casualty for flight, through a bounded
#'   period of intensive care for the ventilated share and directly in a holding
#'   bed for the rest.
#'
#' @details A ventilated casualty awaiting a critical-care sortie genuinely
#'   needs intensive-care-level care, but a deployed intensive care study at
#'   Camp Bastion records that coalition soldiers "are usually evacuated within
#'   24 h of admission" [[56]], so that need is bounded rather than lasting the
#'   whole evacuation wait. A configurable share of the critical pool
#'   (critical_hold$ventilated_share) holds an intensive care bed for a
#'   critical_hold-distributed period and then steps down to a Casualty Staging
#'   Unit hold bed; the rest stage in a hold bed immediately.
#'
#'   The step-down seizes the hold bed before releasing the intensive care bed,
#'   so a casualty is never discharged from intensive care to nowhere. The cost
#'   is that a full hold pool blocks the intensive care bed for as long as the
#'   casualty queues, which ame_icu_hold_minutes measures. This is bed-blocking,
#'   not deadlock: no trajectory in the model holds a hold bed while waiting on
#'   intensive care, so the two acquisition orders cannot form a cycle.
r2e_critical_pre_flight_care <- function(icu_beds, hold_beds) {
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
      trajectory("Ventilated — ICU Pending Flight") %>%
        set_attribute("ame_icu_hold", 1) %>%
        simmer::select(icu_beds, policy = "shortest-queue", id = 10) %>%
        seize_selected(id = 10) %>%
        set_attribute("ame_icu_hold_start", function() now(env)) %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$critical_hold$min,
            b = env_data$vars$r2eheavy$critical_hold$max,
            c = env_data$vars$r2eheavy$critical_hold$mode
          )
        }) %>%
        simmer::select(hold_beds, policy = "shortest-queue", id = 9) %>%
        seize_selected(id = 9) %>%
        set_attribute("ame_hold_start", function() now(env)) %>%
        release_selected(id = 10) %>%
        # Realised ICU occupancy pending evacuation. Exceeds the drawn
        # pre-flight period whenever the holding pool is full at step-down,
        # since the casualty is not moved out of ICU until a holding bed
        # exists for it; that gap is the measure of bed-blocking pushed back
        # into ICU by the strategic evacuation backlog.
        set_attribute("ame_icu_hold_minutes", function() {
          now(env) - get_attribute(env, "ame_icu_hold_start")
        }),
      trajectory("Stable — Staged for Flight") %>%
        set_attribute("ame_icu_hold", 0) %>%
        simmer::select(hold_beds, policy = "shortest-queue", id = 9) %>%
        seize_selected(id = 9) %>%
        set_attribute("ame_hold_start", function() now(env))
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
        dp      <- env_data$vars$dow$params
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
      trajectory("Died of Wounds") %>%
        set_attribute("dow", 1) %>%
        set_attribute("dow_echelon", 3) %>%
        r2e_treat_kia(team_id, evac_team) %>%
        r2e_transport_kia(team_id, evac_team) %>%
        simmer::leave(1),
      trajectory("Continue R2E Treatment")
    ) %>%
    set_attribute("last_dow_t", function() now(env))
}

#' Applies resuscitation at R2E
#'
#' @param trj            Trajectory to append the resuscitation to
#' @param resus_beds     This R2E team's resuscitation beds
#' @param emergency_team This R2E team's emergency section
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
    seize_resources(emergency_team) %>%
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
        release_resources(emergency_team) %>%
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
        set_attribute("dow_ceiling", function() {
          ceiling <- get_attribute(env, "dow_ceiling")
          if (is.na(ceiling)) return(ceiling)
          ceiling * env_data$vars$dow$treatment_efficacy$r2e_resus_factor
        }) %>%
        release_resources(emergency_team) %>%
        release_selected(id = 2)
    )
}

#' Applies the R2E surgical branch and its pre-theatre intensive care gate
#'
#' @param trj         Trajectory to append the branch to
#' @param icu_beds    This R2E team's intensive care beds
#' @param icu_path    The nominal pathway, taken when a bed is free
#' @param hold_path   The degraded pathway, taken by a Priority 1 casualty when
#'   none is
#' @param defer_path  The deferred pathway, taken by a Priority 2+ casualty when
#'   none is
#' @param post_op_dow The post-operative check, applied on its own to a casualty
#'   whose definitive repair was performed forward
#' @return The trajectory, with the surgical branch appended.
r2e_surgical_branch <- function(trj, icu_beds, icu_path, hold_path, defer_path,
                                post_op_dow) {
  trj %>%
    # Branches based on attribute "surgery":
    # - surgery == 1 → check this team's ICU bed availability before OT entry:
    #     - ICU available                                     → r2e_surgery_icu_path
    #         (unchanged short/full ICU recovery logic, then post-op DOW check)
    #     - ICU full, priority <= icu_gating$p1_bypass_priority_max (P1)
    #                                                          → r2e_surgery_hold_path
    #         (surgery proceeds; recovery in a holding bed with elevated
    #         dow_ceiling, then post-op DOW check)
    #     - ICU full, priority above threshold (P2+)           → r2e_surgery_defer_path
    #         (OT entry deferred; polls ICU availability on a timer, then
    #         proceeds as the ICU-available path)
    # - surgery == 1 but the definitive repair was already performed forward
    #   (single-stage casualty operated on at R2B) → no theatre here; the
    #   post-operative DOW checkpoint alone, so the checkpoint is not skipped
    #   for the one route that reaches R2E already repaired
    # - surgery != 1 → no surgery needed
    branch(
      option = function() {
        needs_surg <- get_attribute(env, "surgery")
        if (is.na(needs_surg) || needs_surg != 1) return(4)

        prior <- get_attribute(env, "r2b_surgery")
        if (single_stage() && !is.na(prior) && prior == 1) return(5)

        # Gate disabled: the pre-gate model, in which every operated casualty
        # takes the intensive care pathway and queues for a bed rather than
        # being diverted or deferred by one being full.
        if (!icu_gate_enabled("r2eheavy")) return(1)

        usage  <- sum(get_server_count(env, resources = icu_beds))
        cap    <- sum(get_capacity(env, resources = icu_beds))
        icu_ok <- !is.na(usage) && !is.na(cap) && usage < cap
        if (icu_ok) return(1)

        prio      <- get_attribute(env, "priority")
        threshold <- env_data$vars$r2eheavy$icu_gating$p1_bypass_priority_max
        if (!is.na(prio) && prio <= threshold) return(2)
        return(3)
      },
      continue = TRUE,
      icu_path,
      hold_path,
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

#' Whether this R2E team's surgical sections are saturated
#'
#' @param team_id Integer index of the Role 2E Heavy team
#' @return The number of casualties waiting to be operated on at this team.
#'
#' @details Theatre entry is a two-stage seizure, the operating theatre bed and
#'   then the surgical section, so a casualty waiting for theatre is queued on
#'   one or the other and counting either alone counts half the backlog. A
#'   casualty queued for a room has not reached a section, and one holding a
#'   room while its section is off shift is no longer queued for a room; the sum
#'   counts each casualty exactly once, simmer blocking an arrival on one
#'   resource at a time. Counting sections alone would also cap the measurable
#'   queue at the number of theatre beds, which is two, making any larger
#'   threshold unreachable.
r2e_theatre_queue <- function(team_id) {
  ot_beds  <- env_data$elms$r2eheavy[[team_id]][["ot_bed"]]
  sections <- env_data$elms$r2eheavy[[team_id]][["surg"]]
  waiting_for_room <- sum(sapply(ot_beds, function(bed) get_queue_count(env, bed)))
  waiting_for_staff <- sum(sapply(sections, function(members) {
    sum(sapply(members, function(res) get_queue_count(env, res)))
  }))
  waiting_for_room + waiting_for_staff
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
#' @param trj        Trajectory to append the branch to
#' @param team_id    Integer index of the Role 2E Heavy team
#' @param ot_beds    This R2E team's operating theatre beds
#' @param surg_teams This R2E team's surgical sections
#' @return The trajectory, with the second-operation branch appended.
r2e_second_surgery <- function(trj, team_id, ot_beds, surg_teams) {
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
    #       evacuation with the definitive repair outstanding. Disabled at the
    #       shipped threshold of zero, in which case this arm is unreachable.
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
      # Second procedure. Re-selects a surgical section rather than reusing the
      # one that performed the first: the two operations are separated by ICU
      # or post-operative hold recovery, over which the shift will usually have
      # turned over.
      trajectory("Second Surgery Before Disposition") %>%
        branch(
          option = function() select_r2e_surg_section(team_id),
          continue = TRUE,
          lapply(seq_along(surg_teams), function(section_id) {
            r2e_surgery_block(
              section_id, 7, "r2e_surgery_2_start", "r2e_surgery_2_end",
              local({
                factor <- env_data$vars$dow$treatment_efficacy$r2e_dcs2_factor
                function() factor
              }), FALSE, ot_beds, surg_teams
            )
          })
        ),
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
#' @param hold_beds     This R2E team's holding beds
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
#'   Both routes stage in a hold bed, the critical/standard split being a
#'   distinction in airlift seat rather than in bed type; ame_route records it
#'   (1 = critical, 2 = standard) for the route-decomposed outputs. See README
#'   Role 4 (National Support Base) Demand Modelling for which casualty each
#'   pool is for and the doctrine that decides it.
#'
#'   ame_hold_end records when the staging bed was released, which boarding and
#'   death while waiting both reach and only boarding sets a departure time for,
#'   so the bed time a staged casualty consumed can be recovered whichever way
#'   it left the pool.
#'
#'   The pool seat is never released: a boarded casualty permanently consumes
#'   that sortie's capacity, and casualties board strictly in decision order, no
#'   further acuity-based boarding priority beyond the critical/standard split
#'   being modelled. See README Further Development.
r2e_strategic_evac <- function(hold_beds, critical_care, team_id, evac_team) {
  trajectory("Strategic Evac — Awaiting AME") %>%
    # r2e_departure_time keeps its original meaning — clinical care
    # concluded, disposition decided — so the existing R2E Dwell Time
    # KPI (Domain 2) is unaffected by this branch. AME wait is tracked
    # separately below (ame_departure_time, ame_wait_minutes) rather
    # than folded into r2e_departure_time, so "clinical dwell" and
    # "evacuation logistics wait" remain distinguishable.
    set_attribute("r2e_departure_time", function() now(env)) %>%
    set_attribute("r2e_evac", 1) %>%
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
    #   outstanding, wait on the smaller "ame_critical" pool, holding an
    #   intensive care bed already seized upstream
    # - Otherwise seize a hold bed and wait on the standard "ame" pool
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
      trajectory("Await Standard AME — Hold Bed") %>%
        set_attribute("ame_route", 2) %>%
        simmer::select(hold_beds, policy = "shortest-queue", id = 9) %>%
        seize_selected(id = 9) %>%
        set_attribute("ame_hold_start", function() now(env)) %>%
        join(r2e_ame_wait_and_board("ame", 9, team_id, evac_team))
    ) %>%
    release_selected(id = 9) %>%
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
#'   forward at R2B (`r2b_hold_served`), floored at zero.
r2e_convalescence_remaining_minutes <- function() {
  served <- get_attribute(env, "r2b_hold_served")
  if (is.na(served)) served <- 0
  max(0, get_attribute(env, "recovery_to_duty_days") * DAY_MIN - served)
}

#' Applies the final disposition at R2E under the theatre evacuation policy
#'
#' @param trj           Trajectory to append the disposition to
#' @param hold_beds     This R2E team's holding beds
#' @param critical_care The pre-flight critical care trajectory reached by a
#'   Priority 1 surgical evacuee
#' @param team_id       Integer index of the Role 2E Heavy team
#' @param evac_team     This R2E team's evacuation section
#' @return The trajectory, with the recovery-to-duty draw and the disposition
#'   branch appended.
r2e_disposition <- function(trj, hold_beds, critical_care, team_id, evac_team) {
  # recovery_to_duty_days is drawn first (draw_recovery_to_duty()), then the
  # reconstruction requirement and the evacuation reason it may decide.
  trj %>%
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
        simmer::select(hold_beds, policy = "shortest-queue", id = 5) %>%
        seize_selected(id = 5) %>%
        set_attribute("r2e_recovery_hold_start", function() now(env)) %>%
        # The hold bed is held for the same duration the disposition was
        # decided on, so a retained casualty's bed-days and the prognosis
        # that retained them cannot disagree.
        timeout(r2e_convalescence_remaining_minutes) %>%
        release_selected(id = 5) %>%
        set_attribute("r2e_departure_time", function() now(env)) %>%
        set_attribute("return_day", function() now(env)) %>%
        set_attribute("return_echelon", 3) %>%
        credit_rtd(),
      r2e_strategic_evac(hold_beds, critical_care, team_id, evac_team)
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
#'   The three surgical pathways are compositions of those sub-trajectories: the
#'   nominal one takes any outstanding stabilisation, the operation and the
#'   stabilisation phase between the two operations; the degraded one takes the
#'   operation and the post-operative hold; and the deferred one waits for
#'   intensive care headroom and then takes the nominal one.
#'
#'   The builders take the resources they use rather than reading this team's
#'   establishment themselves, so what each one touches is visible in its
#'   signature.
r2e_treat_wia <- function(team_id) {
  hold_beds       <- env_data$elms$r2eheavy[[team_id]][["hold_bed"]]
  resus_beds      <- env_data$elms$r2eheavy[[team_id]][["resus_bed"]]
  ot_beds         <- env_data$elms$r2eheavy[[team_id]][["ot_bed"]]
  icu_beds        <- env_data$elms$r2eheavy[[team_id]][["icu_bed"]]
  surg_teams      <- env_data$elms$r2eheavy[[team_id]][["surg"]]

  emergency_team <- select_subteam("r2eheavy", team_id, "emerg")
  evac_team      <- select_subteam("r2eheavy", team_id, "evac")
  icu_team       <- select_subteam("r2eheavy", team_id, "icu")

  post_op_dow     <- r2e_post_op_dow_check(team_id, evac_team)
  stab_recovery   <- r2e_stabilisation_recovery(icu_beds, post_op_dow)
  pre_stab        <- r2e_pre_surgery_stabilisation(icu_beds)
  hold_recovery   <- r2e_hold_recovery(hold_beds, post_op_dow)
  ot_surgery      <- r2e_ot_surgery(team_id, ot_beds, surg_teams)
  post_definitive <- r2e_post_definitive_care(icu_beds, hold_beds)
  critical_care   <- r2e_critical_pre_flight_care(icu_beds, hold_beds)

  icu_path <- trajectory("R2E Surgery — ICU Available") %>%
    join(pre_stab) %>%
    join(ot_surgery) %>%
    join(stab_recovery)

  hold_path <- trajectory("R2E Surgery — ICU Full, P1 to Post-Op Hold") %>%
    join(ot_surgery) %>%
    join(hold_recovery)

  defer_path <- r2e_surgery_defer_path(icu_beds, icu_path)

  trajectory("R2E Treatment") %>%
    set_attribute("r2e_treated", team_id) %>%
    set_attribute("r2e_handling", 1) %>%
    set_attribute("r2e_arrival_time", function() now(env)) %>%

    # Phase 1: DOW check on arrival
    r2e_arrival_dow_check(team_id, evac_team) %>%

    # Phase 2: Resuscitation
    r2e_resuscitation(resus_beds, emergency_team) %>%

    # Phase 3: Surgical branch, gated on intensive care availability
    r2e_surgical_branch(icu_beds, icu_path, hold_path, defer_path, post_op_dow) %>%

    # Phase 4: Second surgery, for a damage control casualty whose abbreviated
    # operation was the Phase 3 one here
    r2e_second_surgery(team_id, ot_beds, surg_teams) %>%

    # Post-definitive intensive care, after whichever operation was this
    # casualty's definitive repair — the Phase 4 procedure for a casualty
    # operated on only at R2E, the Phase 3 one for a casualty who had their
    # abbreviated operation at R2B.
    join(post_definitive) %>%

    # Phase 5: Final disposition — theatre evacuation policy
    r2e_disposition(hold_beds, critical_care, team_id, evac_team)
}
