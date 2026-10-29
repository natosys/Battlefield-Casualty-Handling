##############################################
## R/trajectories_r2b.R                   ##
## Role 2B trajectories                   ##
##############################################
#
# The R2B trajectories: KIA handling and transport, evacuation to R2E,
# post-operative stabilisation and forward holding, the surgical and
# intensive care gates, holding recovery and resuscitation.
#
# Sourced by R/trajectories.R, which loads the packages and R/constants.R
# these functions use; source that file rather than this one alone.

# ── Role 2B trajectories ──────────────────────────────────────────────────────

#' Applies mortuary preparation for KIA casualties using the R2B evacuation team
#'
#' @param traj A simmer trajectory object
#' @param team_id Integer index of the selected Role 2B team
#' @return Modified trajectory with KIA treatment logic appended
r2b_treat_kia <- function(traj, team_id) {
  evacuation_team <- env_data$elms$r2b[[team_id]][["evac"]][[1]]
  traj %>%
    seize_resources(evacuation_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$kia_treat$min,
        b = env_data$vars$r2b$kia_treat$max,
        c = env_data$vars$r2b$kia_treat$mode
      )
    }) %>%
    release_resources(evacuation_team)
}

#' Simulates Role 2B road-move transport of KIA casualties to the mortuary,
#' modelled as collocated with Role 2E Heavy rather than Role 2B; R2B has no organic mortuary
#' asset of its own. Uses the
#' shared HX2 40M fleet with a dead-heading return leg, mirroring
#' r1_transport_kia()'s pattern: after drop-off, the entity clones into a
#' vehicle branch (unladen return timeout, then release) and a casualty
#' branch (no further activity); synchronize(wait = FALSE) lets the
#' casualty continue immediately while the vehicle clone completes its
#' return leg independently and is discarded on arrival. On arrival, the
#' casualty is handed to a selected R2E team's own mortuary intake
#' (r2e_mortuary_intake()), which sets mortuary_treated.
#'
#' @param traj A simmer trajectory object
#' @param team_id Integer index of the selected Role 2B team (retained for
#'   call-site signature parity with r2b_treat_kia(); not otherwise used —
#'   the road move uses the shared HX240M fleet, not an R2B-team-specific
#'   resource)
#' @return Modified trajectory with transport and R2E mortuary intake appended
r2b_transport_kia <- function(traj, team_id) {
  traj %>%
    simmer::select(env_data$transports$HX240M, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("r2b_r2e_mortuary_transport_start", function() now(env)) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$kia_transport$min,
        b = env_data$vars$r2b$kia_transport$max,
        c = env_data$vars$r2b$kia_transport$mode
      )
    }) %>%
    clone(
      n = 2,
      trajectory("HX240M Return Leg (R2B Mortuary Transfer)") %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2b$kia_transport$min,
            b = env_data$vars$r2b$kia_transport$max,
            c = env_data$vars$r2b$kia_transport$mode
          )
        }) %>%
        release_selected(),
      trajectory("KIA Arrived at R2E Mortuary")
    ) %>%
    synchronize(wait = FALSE) %>%
    set_attribute("r2e", function() select_r2e_team()) %>%
    # Dispatch on attribute "r2e", the team selected above:
    # - Routes to that team's mortuary intake, one arm per R2E team
    branch(
      option = function() get_attribute(env, "r2e"),
      continue = TRUE,
      lapply(1:length(env_data$elms$r2eheavy), r2e_mortuary_intake)
    )
}

#' Seizes an already-available R2B team evac resource, models the outbound
#' transport draw, then a dead-heading return leg on that same resource
#' before releasing it. The casualty continues
#' immediately via synchronize(wait = FALSE); the escort/vehicle clone
#' returns to R2B independently and is discarded on arrival. Assumes
#' `evacuation_team` is not yet seized — use r2b_evac_return_leg() directly
#' when the caller has already seized it as an availability gate.
#'
#' @param traj A simmer trajectory object
#' @param evacuation_team Character vector naming the R2B team's evac resource
#' @return Modified trajectory with the seize + outbound leg + dead-head
#'   return leg appended
r2b_evac_leg <- function(traj, evacuation_team) {
  traj %>%
    seize_resources(evacuation_team) %>%
    set_attribute("r2b_departure_time", function() now(env)) %>%
    r2b_evac_return_leg(evacuation_team)
}

#' Outbound transport draw plus dead-heading return leg for an R2B team's
#' evac resource that the caller has already seized.
#' Split out from r2b_evac_leg() for the wait_for_evac fallback path, which
#' seizes `evacuation_team` earlier as its own availability gate.
#'
#' @param traj A simmer trajectory object, with `evacuation_team` already seized
#' @param evacuation_team Character vector naming the R2B team's evac resource
#' @return Modified trajectory with the outbound leg and dead-head return
#'   leg appended; `evacuation_team` is released only after the return leg
r2b_evac_return_leg <- function(traj, evacuation_team) {
  traj %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$wia_transport$min,
        b = env_data$vars$r2b$wia_transport$max,
        c = env_data$vars$r2b$wia_transport$mode
      )
    }) %>%
    clone(
      n = 2,
      trajectory("R2B Evac Team Return Leg") %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2b$wia_transport$min,
            b = env_data$vars$r2b$wia_transport$max,
            c = env_data$vars$r2b$wia_transport$mode
          )
        }) %>%
        release_resources(evacuation_team),
      trajectory("Casualty Dropped Off at R2E")
    ) %>%
    synchronize(wait = FALSE)
}
# ── R2B WIA pathway builders ──────────────────────────────────────────────────

#' Evacuate a casualty from R2B to a selected R2E Heavy element
#'
#' @param trj             Trajectory to append the evacuation to
#' @param evacuation_team The R2B team's own organic evacuation resource
#' @return The trajectory, with the R2E selection, the road move and the R2E
#'   pathway appended; the casualty leaves this trajectory at the end of it.
#'
#' @details Reached from four places in the R2B pathway (the two holding-bed
#'   threshold evacuations, the holding-bed bypass and the immediate
#'   evacuation), which is why it is a segment rather than part of any one of
#'   them: they differ in what brings the casualty here, not in the movement
#'   that follows.
r2b_evacuate_to_r2e <- function(trj, evacuation_team) {
  trj %>%
    set_attribute("r2b_to_r2e", 1) %>%
    set_attribute("r2e", function() select_r2e_team()) %>%
    r2b_evac_leg(evacuation_team) %>%
    branch(
      option = function() get_attribute(env, "r2e"),
      continue = TRUE,
      lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
    ) %>%
    simmer::leave(1)
}

#' Builds the fallback wait for evacuation capacity at R2B
#'
#' @param icu_beds        This R2B team's intensive care beds
#' @param icu_team        This R2B team's intensive care section
#' @param evacuation_team This R2B team's own organic evacuation resource
#' @return Simmer trajectory holding the casualty in an intensive care bed
#'   until the evacuation asset is free, then moving them to R2E.
#'
#' @details Reached when the evacuation asset is already committed. The
#'   casualty is held in an intensive care bed rather than queued unattended,
#'   and the bed and section are released the moment the asset is in hand.
#'   evac_wait_count counts the casualties this path serves.
r2b_wait_for_evac <- function(icu_beds, icu_team, evacuation_team) {
  trajectory("Wait in Hold Bed for Evac") %>%
    set_global("evac_wait_count", function() {
      current <- get_global(env, "evac_wait_count")
      return(current + 1)
    }) %>%
    simmer::select(icu_beds, policy = "shortest-queue", id = 3) %>%
    seize_selected(id = 3) %>%
    dow_placement(post_op_r2b_icu) %>%
    seize_resources(icu_team) %>%
    seize_resources(evacuation_team) %>%
    release_resources(icu_team) %>%
    release_selected(id = 3) %>%
    dow_placement(post_op_outside_icu) %>%
    set_attribute("r2b_to_r2e", 1) %>%
    set_attribute("r2e", function() select_r2e_team()) %>%
    set_attribute("r2b_departure_time", function() now(env)) %>%
    r2b_evac_return_leg(evacuation_team) %>%
    # Dispatch on attribute "r2e", the team selected before the road move:
    # - Routes to that team's treatment trajectory, one arm per R2E team
    branch(
      option = function() get_attribute(env, "r2e"),
      continue = TRUE,
      lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
    ) %>%
    leave(1)
}

#' Builds the forward post-operative intensive care phase at R2B
#'
#' @param icu_beds  This R2B team's intensive care beds
#' @param hold_beds This R2B team's holding beds
#' @return Simmer trajectory serving the part of a casualty's post-operative
#'   intensive care requirement that is held forward, for either surgical
#'   pathway.
#'
#' @details Where a casualty's post-operative intensive care is served is
#'   decided per casualty, when their R2B operation ends, on two grounds
#'   (forward_hold_rule()): clinical stability, a window they are held for
#'   before they are fit to transfer, and capacity, a further hold while R2E
#'   intensive care is saturated. Both are limited by
#'   `r2b.post_op_icu.forward_hold_max`, and neither can exceed the casualty's
#'   requirement, the remainder falling to R2E.
#'
#'   The episode held forward is the one that follows the operation. For a
#'   damage control casualty that is stabilisation, between the abbreviated
#'   operation and the definitive one waiting at R2E, and the remainder is
#'   served at R2E before the definitive operation. For a single-stage casualty
#'   it is post-definitive care, drawn here rather than at R2E and served at R2E
#'   after transfer in the amount that remains; see
#'   r2e_post_definitive_care().
#'
#'   The forward stay carries a care-quality cost: an R2B ICU section fields
#'   two nurses and two medics and no intensivist, against R2E's
#'   intensivist-led section. Without it, forward holding would be free in the
#'   model and any sweep of the rule would recommend its maximum as an artefact
#'   of relieving the R2E queue at no cost. Under the care-state model the stay
#'   is charged at the post-operative multiplier times r2b_icu_penalty in an
#'   intensive care bed, and times hold_bed_penalty alone in a holding bed;
#'   under the legacy mechanism the cost is a raised ceiling. See README "Died
#'   of Wounds".
#'
#'   The stay opens no DOW checkpoint of its own: what it accrues is settled at
#'   the next checkpoint the casualty reaches, the R2E arrival check.
r2b_post_op_stabilisation <- function(icu_beds, hold_beds) {
  forward_stay <- r2b_forward_stay(icu_beds, hold_beds)

  # Branches on the surgical pathway, then on whether the rule holds the
  # casualty forward at all:
  # - damage control, rule in force -> draw the stabilisation requirement
  #                                   and, if any is held, the forward stay
  # - single-stage, rule in force   -> draw the post-definitive requirement
  #                                   here and, if any is held, the forward
  #                                   stay; with the rule off no draw is taken
  # - rule off for the pathway      -> no forward stay; the whole requirement
  #                                   is served at R2E
  # The draw sits inside the branch so a casualty the rule does not apply to
  # consumes no requirement they will not serve forward, which is what keeps
  # the disabled configuration's random stream unchanged.
  trajectory("R2B Post-Operative Stabilisation") %>%
    branch(
      option = function() if (single_stage()) 2 else 1,
      continue = TRUE,

      trajectory("R2B Damage Control Stabilisation") %>%
        set_attribute("stabilisation_total", function() draw_stabilisation_icu()) %>%
        join(forward_stay),

      trajectory("R2B Single-Stage Post-Operative Care") %>%
        branch(
          option = function() if (forward_hold_enabled()) 1 else 2,
          continue = TRUE,
          trajectory("R2B Single-Stage Forward Requirement") %>%
            set_attribute("post_definitive_total", function() draw_post_definitive_icu()) %>%
            join(forward_stay),
          trajectory("Single-Stage — Served Entirely at R2E")
        )
    )
}

#' Builds the forward stay itself, shared by both surgical pathways
#'
#' @param icu_beds  This R2B team's intensive care beds
#' @param hold_beds This R2B team's holding beds
#' @return Simmer trajectory holding a casualty forward on stability grounds,
#'   then on capacity grounds, in an intensive care bed or, where none is
#'   free, a holding bed.
#'
#' @details Reached once the casualty's requirement has been drawn. A casualty
#'   is held forward when a stability window applies to them or R2E intensive
#'   care is saturated at this instant; otherwise they leave at once. The bed
#'   is chosen when the stay begins and kept through the capacity hold, so the
#'   same bed carries both grounds. `r2b_post_op_min` accumulates the minutes
#'   served and is what R2E subtracts from the requirement, so conservation
#'   holds by construction.
r2b_forward_stay <- function(icu_beds, hold_beds) {
  # Capacity hold: polled, the bed kept throughout, until R2E intensive care
  # has room or the casualty's headroom is spent. Each poll is at most the
  # remaining headroom, so the stay can never exceed the requirement or the cap.
  capacity_hold <- trajectory("R2B Capacity Hold") %>%
    branch(
      option = function() if (forward_capacity_due()) 1 else 2,
      continue = TRUE,
      trajectory("R2B Capacity Poll") %>%
        timeout(function() min(forward_hold_rule()$poll_interval, forward_headroom())) %>%
        set_attribute("r2b_post_op_min", function() {
          forward_served() + min(forward_hold_rule()$poll_interval, forward_headroom())
        }) %>%
        rollback(amount = 2, times = Inf, check = function() forward_capacity_due()),
      trajectory("Transfer — R2E Can Receive")
    )

  # The two grounds in sequence: stability window, then capacity hold.
  #' One forward stay on a given bed type, stability window then capacity hold
  #'
  #' @param res_id The `select()` identifier: 6 for an intensive care bed, 7
  #'   for a holding bed.
  #' @return A trajectory seizing the bed, serving both grounds and releasing it.
  stay <- function(res_id) {
    trajectory() %>%
      set_attribute("r2b_post_op_min", function() forward_stability_minutes()) %>%
      simmer::select(if (res_id == 6) icu_beds else hold_beds,
                     policy = "shortest-queue", id = res_id) %>%
      seize_selected(id = res_id) %>%
      dow_placement(if (res_id == 6) post_op_r2b_icu else post_op_outside_icu) %>%
      # Read back from the attribute rather than recomputed: the attribute now
      # counts the minutes served, so recomputing would see the headroom already
      # spent and shorten the stay below the minutes recorded for it.
      timeout(function() get_attribute(env, "r2b_post_op_min")) %>%
      join(capacity_hold) %>%
      release_selected(id = res_id) %>%
      dow_placement(post_op_outside_icu)
  }

  # Under the legacy mechanism the forward placement raises the ceiling: the
  # intensivist penalty on either bed, and the holding-bed penalty on top of it
  # in a holding bed. Under the care-state model the placement instead sets the
  # stay's multiplier, one per stay.
  legacy <- !dow_care_state_enabled()
  #' Raises the legacy ceiling by a factor, under the legacy mechanism alone
  #'
  #' @param trj Trajectory to append to.
  #' @param factor Zero-argument function returning the factor.
  #' @return The trajectory, unchanged under the care-state model.
  penalise <- function(trj, factor) {
    if (!legacy) return(trj)
    trj %>% set_attribute("dow_ceiling", function() {
      ceiling <- get_attribute(env, "dow_ceiling")
      if (is.na(ceiling)) return(ceiling)
      ceiling * factor()
    })
  }
  #' The legacy intensivist penalty
  #'
  #' @return LEGACY_DOW_EFFICACY's r2b_icu_penalty.
  icu_penalty  <- function() LEGACY_DOW_EFFICACY$r2b_icu_penalty
  #' The legacy holding-bed penalty
  #'
  #' @return LEGACY_POSTOP_HOLD_PENALTY.
  hold_penalty <- function() LEGACY_POSTOP_HOLD_PENALTY

  # Branches on whether the casualty is held forward, then on R2B ICU bed
  # availability, read at this instant rather than queued for:
  # - nothing to hold forward   -> no forward stay (r2b_post_op_pathway unset)
  # - ICU bed free              -> ICU bed (r2b_post_op_pathway = 1)
  # - ICU saturated             -> holding bed for the same stay, at the
  #                               holding-bed care cost (r2b_post_op_pathway = 2),
  #                               mirroring the R2E post-op hold pathway
  trajectory("R2B Forward Stay") %>%
    branch(
      option = function() {
        if (forward_stability_minutes() <= 0 && !forward_capacity_due()) return(3)
        usage <- sum(get_server_count(env, resources = icu_beds))
        cap   <- sum(get_capacity(env, resources = icu_beds))
        if (!is.na(usage) && !is.na(cap) && usage < cap) return(1)
        return(2)
      },
      continue = TRUE,

      trajectory("R2B Post-Op ICU") %>%
        penalise(icu_penalty) %>%
        set_attribute("r2b_post_op_pathway", 1) %>%
        join(stay(6)),

      # ICU saturated — the same stay in a holding bed, at the same elevated
      # risk the R2E post-op hold pathway carries for the same reason (reduced
      # post-operative monitoring). The duration is the casualty's requirement
      # either way: the bed changes what the stay is worth clinically, not how
      # long they need it for.
      trajectory("R2B Post-Op Hold — ICU Full") %>%
        penalise(icu_penalty) %>%
        set_attribute("r2b_post_op_pathway", 2) %>%
        penalise(hold_penalty) %>%
        join(stay(7)),

      trajectory("No Forward Stay")
    )
}

#' Builds one forward operation at R2B, from theatre seizure to stabilisation
#'
#' @param pre_open  Whether this copy is the held route, reached by a casualty
#'   waiting for a section about to reopen rather than one finding it open
#' @param ot_beds   This R2B team's operating theatre beds
#' @param surg_team This R2B team's surgical section
#' @param post_op   The forward stabilisation trajectory built by
#'   r2b_post_op_stabilisation(), joined at the end of the operation
#' @return Simmer trajectory performing one forward operation
#'
#' @details Built rather than written once because it is reached by two routes,
#'   a casualty whose theatre and section are both free on arrival and one held
#'   briefly for a section about to reopen, and only the second records how long
#'   it waited.
r2b_surgery_path <- function(pre_open, ot_beds, surg_team, post_op) {
  force(pre_open)

  trj <- trajectory("Surgery Path")

  if (pre_open) {
    trj <- trj %>%
      set_attribute("r2b_pre_open_wait", 1) %>%
      set_attribute("r2b_pre_open_start", function() now(env))
  }

  trj <- trj %>%
    simmer::select(ot_beds, policy = "shortest-queue", id = 4) %>%
    seize_selected(id = 4) %>%
    seize_resources(surg_team)

  # Realised hold, recorded once both theatre and section are in hand. It
  # can exceed the window the casualty was admitted on, the section
  # reopening to a theatre that another case has since taken.
  if (pre_open) {
    trj <- trj %>%
      set_attribute("r2b_pre_open_wait_min", function() {
        now(env) - get_attribute(env, "r2b_pre_open_start")
      })
  }

  trj %>%
    set_attribute("r2b_surgery_start", function() now(env)) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$surgery$min,
        b = env_data$vars$r2b$surgery$max,
        c = env_data$vars$r2b$surgery$mode
      )
    }) %>%
    set_attribute("r2b_surgery", 1) %>%
    set_attribute("r2b_surgery_end", function() now(env)) %>%
    release_resources(surg_team) %>%
    release_selected(id = 4) %>%
    # A damage control casualty's forward operation is abbreviated and
    # earns r2b_dcs_factor; a single-stage casualty's is their definitive
    # repair and earns what the staged route earns on completing one at
    # R2E as well (definitive_efficacy(), top of this file).
    dow_operated(function() {
      te <- LEGACY_DOW_EFFICACY
      definitive_efficacy(te$r2b_dcs_factor, te$r2e_dcs1_factor)()
    }) %>%
    join(post_op)
}

#' Builds one forward operation at R2B on whichever section takes the case
#'
#' @param pre_open  Whether this copy is the held route (see r2b_surgery_path())
#' @param team_id   Index of this R2B team within env_data$elms$r2b
#' @param ot_beds   This R2B team's operating theatre beds
#' @param surg_teams This R2B team's surgical sections
#' @param post_op   The forward stabilisation trajectory joined at the end
#' @return Simmer trajectory performing one forward operation
#'
#' @details A unit fielding one section gets r2b_surgery_path() on it directly,
#'   which is the trajectory the model has always had. A unit fielding several
#'   chooses among them per casualty, on the R2E arrangement.
r2b_surgery_by_section <- function(pre_open, team_id, ot_beds, surg_teams, post_op) {
  if (length(surg_teams) == 1L) {
    return(r2b_surgery_path(pre_open, ot_beds, surg_teams[[1]], post_op))
  }

  # Branch structure: one arm per surgical section of this unit, each the
  # same forward operation seizing that section alone. The decision criterion
  # is select_r2b_surg_section(): an on-shift section is preferred, then the
  # least loaded, ties broken at random.
  trajectory("Surgery By Section") %>%
    branch(
      option = function() select_r2b_surg_section(team_id),
      continue = TRUE,
      lapply(seq_along(surg_teams), function(section_id) {
        r2b_surgery_path(pre_open, ot_beds, surg_teams[[section_id]], post_op)
      })
    )
}

#' Builds the operating theatre availability check at R2B
#'
#' @param team_id   Index of this R2B team within env_data$elms$r2b
#' @param ot_beds   This R2B team's operating theatre beds
#' @param surg_teams This R2B team's surgical sections
#' @param post_op   The forward stabilisation trajectory the operation ends in
#' @return Simmer trajectory operating on the casualty forward, holding them
#'   for a section about to reopen, or bypassing them to R2E.
#'
#' @details Joined directly when the pre-OT ICU gate clears immediately, and
#'   again after the P2+ ICU-defer wait loop resolves.
r2b_ot_check_path <- function(team_id, ot_beds, surg_teams, post_op) {
  # The unit's sections taken together: the section is available when any of
  # them is on shift, and a single section is the case this has always been.
  surg_members <- unlist(surg_teams, use.names = FALSE)

  trajectory("R2B OT Check") %>%
    branch(
      option = function() {
        usage    <- sum(get_server_count(env, resources = ot_beds))
        cap      <- sum(get_capacity(env, resources = ot_beds))
        queue    <- sum(get_queue_count(env, resources = ot_beds))
        team_cap <- sum(get_capacity(env, resources = surg_members))
        # A theatre is available when a bed is free and nobody is queued for
        # it; the section is available when its shift is open.
        bed_ok  <- !is.na(usage) && !is.na(cap) && usage < cap && queue == 0
        team_ok <- !is.na(team_cap) && team_cap > 0
        if (bed_ok && team_ok) return(1)
        # Theatre free, section closed, and closed for no longer than the
        # pre-open window: hold the casualty forward rather than divert them.
        # The bed condition is required here as well as above, so at most one
        # casualty per team is ever held; a second arriving to find the first
        # already holding the theatre sees a full bed and diverts as before.
        if (bed_ok && !team_ok) {
          window <- env_data$vars$r2b$surgery$pre_open_window_min
          if (!is.null(window) && !is.na(window) && window > 0 &&
              minutes_to_shift_open(now(env)) <= window) return(3)
        }
        return(2)
      },
      continue = TRUE,

      # Sub-branch 1: theatre free and section on shift — operate now
      r2b_surgery_by_section(FALSE, team_id, ot_beds, surg_teams, post_op),

      # Sub-branch 2: OT busy, queued, or the section closed for longer than
      # the pre-open window — bypass to R2E. r2b_bypass_reason decomposes the
      # cause: 1 = surgical section off shift (team_cap <= 0), 2 = OT bed busy
      # or queued. Re-reads the same resource state as the option() check
      # above — no timeout intervenes between the branch decision and this
      # sub-trajectory, so state cannot have changed.
      trajectory("OT Unavailable – Bypass to R2E") %>%
        set_attribute("r2b_bypassed", 1) %>%
        set_attribute("r2b_bypass_reason", function() {
          team_cap <- sum(get_capacity(env, resources = surg_members))
          if (!is.na(team_cap) && team_cap <= 0) return(1)
          return(2)
        }) %>%
        set_attribute("r2b_bypass_time", function() now(env)),

      # Sub-branch 3: section reopening within the pre-open window — hold the
      # casualty forward for it. The seizes inside the surgery path do the
      # waiting: a section whose shift is closed carries zero capacity, so
      # seizing it queues the casualty until the shift reopens, with no
      # timeout to align against the roster's own capacity change. Holding
      # the theatre through the wait is what the hold means, the casualty
      # being received into it rather than moved on.
      #
      # The hold opens no died-of-wounds checkpoint of its own. As with
      # forward stabilisation above, dow_prob_conditional() prices elapsed
      # time at the next checkpoint the casualty reaches, so the delay is
      # charged there rather than going unpriced.
      r2b_surgery_by_section(TRUE, team_id, ot_beds, surg_teams, post_op)
    )
}

#' Whether one echelon's pre-theatre intensive care gate is in force
#'
#' @param echelon `env_data$vars` key of the echelon, "r2b" or "r2eheavy".
#' @return TRUE where the gate applies, FALSE where it is disabled.
#' @details Disabling it reproduces the model as it stood before the gate
#'   existed: theatre entry does not depend on an intensive care bed being
#'   free, and a casualty needing stabilisation is admitted to intensive care
#'   whether or not one is, queueing if none is. That is the arm the published
#'   before-and-after comparison needs, and reconstructing it as a supported
#'   configuration is what makes the comparison repeatable after a later model
#'   change rather than recoverable only from a dead code state.
#'
#'   Validated here rather than at load, the field being read once per
#'   trajectory build and a malformed value being a configuration error the
#'   caller should see named.
icu_gate_enabled <- function(echelon) {
  value <- env_data$vars[[echelon]]$icu_gating$enabled
  if (is.null(value) || length(value) != 1L || is.na(value) ||
        !value %in% c(0, 1)) {
    stop(sprintf("%s.icu_gating.enabled must be 0 or 1, found %s",
                 echelon, paste(format(value), collapse = ", ")), call. = FALSE)
  }
  value == 1
}

#' Builds the pre-theatre intensive care gate at R2B
#'
#' @param icu_beds This R2B team's intensive care beds
#' @param ot_check The theatre availability trajectory built by
#'   r2b_ot_check_path(), reached whether the gate clears at once or after a
#'   wait
#' @return Simmer trajectory admitting the casualty to the theatre check, at
#'   once or once intensive care headroom exists.
#'
#' @details Mirrors the R2E pattern. Priority 1 casualties proceed
#'   unconditionally; Priority 2+ casualties defer theatre entry while this
#'   unit's intensive care is fully saturated, preserving headroom for those
#'   already in it. How much work the gate does depends on
#'   `r2b.post_op_icu.share`: at zero the two intensive care beds per team serve
#'   only the wait-for-evacuation fallback and the gate is close to inert, while
#'   at a non-zero share every casualty operated on here also recovers here, and
#'   the gate becomes a real constraint on forward surgical throughput.
r2b_surgery_gate <- function(icu_beds, ot_check) {
  trajectory("Needs Surgery") %>%
    branch(
      option = function() {
        if (!icu_gate_enabled("r2b")) return(1)   # gate disabled: pre-gate model
        prio <- get_attribute(env, "priority")
        if (!is.na(prio) && prio == 1) return(1)  # P1 always proceeds
        usage  <- sum(get_server_count(env, resources = icu_beds))
        cap    <- sum(get_capacity(env, resources = icu_beds))
        icu_ok <- !is.na(usage) && !is.na(cap) && usage < cap
        if (icu_ok) return(1)
        return(2)  # P2+, ICU saturated: defer OT entry
      },
      continue = TRUE,
      ot_check,
      trajectory("ICU Full — Defer Surgery (P2+)") %>%
        set_attribute("surgery_deferred", 1) %>%
        timeout(function() env_data$vars$r2b$icu_gating$defer_check_interval) %>%
        rollback(target = 1, check = function() {
          usage <- sum(get_server_count(env, resources = icu_beds))
          cap   <- sum(get_capacity(env, resources = icu_beds))
          !(!is.na(usage) && !is.na(cap) && usage < cap)
        }) %>%
        join(ot_check)
    )
}

#' Builds the R2B holding-bed recovery pathway
#'
#' @param hold_beds       This R2B team's holding beds
#' @param evacuation_team This R2B team's own organic evacuation resource
#' @return Simmer trajectory holding the casualty at R2B for their drawn
#'   convalescence, then returning them to duty or evacuating them to R2E.
#'
#' @details Reached when this team's holding beds have capacity. Where
#'   `r2b.holding.evac_threshold` is set and the drawn duration exceeds it, the
#'   casualty is evacuated part-way through and carries the unserved remainder
#'   of that draw to R2E as their convalescence there
#'   (r2b_hold_residual_minutes()).
r2b_hold_recovery <- function(hold_beds, evacuation_team) {
  trajectory("R2B Hold") %>%
    simmer::select(hold_beds, policy = "first-available", id = 5) %>%
    seize_selected(id = 5) %>%
    set_attribute("r2b_hold_start", function() now(env)) %>%
    set_attribute("r2b_hold_drawn", function() {
      get_attribute(env, "recovery_to_duty_days") * DAY_MIN
    }) %>%
    timeout(r2b_hold_minutes) %>%
    # Evac-threshold branch
    # - residual > 0 → release hold bed, transport to R2E (r2b_hold_evac = 1),
    #   carrying the unserved remainder of the draw forward
    # - residual = 0 → return to duty (return_day set, leave)
    branch(
      option = function() {
        if (r2b_hold_residual_minutes() > 0) return(1)
        return(2)
      },
      continue = TRUE,
      trajectory("R2B Hold Threshold — Early Evac") %>%
        set_attribute("r2b_hold_evac", 1) %>%
        # Measured from the clock rather than restated from the
        # threshold, so the conservation the residual asserts is
        # checked against the bed time actually served.
        set_attribute("r2b_hold_served", function() {
          now(env) - get_attribute(env, "r2b_hold_start")
        }) %>%
        set_attribute("r2b_hold_residual", r2b_hold_residual_minutes) %>%
        release_selected(id = 5) %>%
        r2b_evacuate_to_r2e(evacuation_team),
      trajectory("R2B Hold RTD") %>%
        set_attribute("return_day", function() now(env)) %>%
        set_attribute("return_echelon", 2) %>%
        credit_rtd() %>%
        release_selected(id = 5) %>%
        simmer::leave(1)
    )
}

#' Whether a casualty's convalescence rules out holding them forward at R2B
#'
#' @return TRUE when the casualty's drawn `recovery_to_duty_days` exceeds the
#'   theatre evacuation policy.
#' @details Such a casualty is evacuated from theatre whichever echelon holds
#'   them (draw_evacuation_reason()), so a forward bed would be occupied only
#'   to be vacated by the same decision. Reading the policy here keeps one
#'   convalescence governing both the disposition and the forward stay.
r2b_hold_ineligible <- function() {
  drawn  <- get_attribute(env, "recovery_to_duty_days")
  policy <- env_data$vars$r2eheavy$recovery$evacuation_policy_days
  !is.na(drawn) && drawn > policy
}

#' Builds the R2B route of a casualty ineligible for forward holding
#'
#' @param evacuation_team This R2B team's own organic evacuation resource
#' @return Simmer trajectory moving the casualty straight to R2E.
r2b_hold_ineligible_path <- function(evacuation_team) {
  trajectory("R2B Hold Ineligible, Convalescence Beyond Policy") %>%
    set_attribute("r2b_hold_ineligible", 1) %>%
    r2b_evacuate_to_r2e(evacuation_team)
}

#' Builds the R2B holding-bed bypass to R2E
#'
#' @param evacuation_team This R2B team's own organic evacuation resource
#' @return Simmer trajectory moving the casualty straight to R2E.
#'
#' @details Reached when this team's holding beds are full and R2E's have
#'   capacity, and again when the proportional holding queue cap is exceeded.
r2b_hold_bypass <- function(evacuation_team) {
  trajectory("R2B Hold Full — Bypass to R2E") %>%
    set_attribute("r2b_hold_bypass", 1) %>%
    r2b_evacuate_to_r2e(evacuation_team)
}

#' Builds the queued R2B holding-bed recovery pathway
#'
#' @param hold_beds       This R2B team's holding beds
#' @param evacuation_team This R2B team's own organic evacuation resource
#' @return Simmer trajectory queueing the casualty for a holding bed at R2B,
#'   then serving the same convalescence and disposition as an unqueued one.
#'
#' @details Reached when both this team's holding beds and R2E's are full and
#'   the R2B holding queue is within its proportional cap; the casualty waits
#'   here rather than being moved to an echelon with nothing free either.
r2b_hold_queue_recovery <- function(hold_beds, evacuation_team) {
  trajectory("R2B Hold Queue — R2E Full") %>%
    set_attribute("r2b_hold_queued", 1) %>%
    simmer::select(hold_beds, policy = "shortest-queue", id = 5) %>%
    seize_selected(id = 5) %>%
    set_attribute("r2b_hold_start", function() now(env)) %>%
    set_attribute("r2b_hold_drawn", function() {
      get_attribute(env, "recovery_to_duty_days") * DAY_MIN
    }) %>%
    timeout(r2b_hold_minutes) %>%
    # Branches on the convalescence left after the evacuation threshold:
    # - If time remains, evacuate early and serve the remainder at R2E
    # - If none remains, the casualty has convalesced here and returns to duty
    branch(
      option = function() {
        if (r2b_hold_residual_minutes() > 0) return(1)
        return(2)
      },
      continue = TRUE,
      trajectory("R2B Hold Queue Threshold — Early Evac") %>%
        set_attribute("r2b_hold_evac", 1) %>%
        # Measured from the clock rather than restated from the
        # threshold, so the conservation the residual asserts is
        # checked against the bed time actually served.
        set_attribute("r2b_hold_served", function() {
          now(env) - get_attribute(env, "r2b_hold_start")
        }) %>%
        set_attribute("r2b_hold_residual", r2b_hold_residual_minutes) %>%
        release_selected(id = 5) %>%
        r2b_evacuate_to_r2e(evacuation_team),
      trajectory("R2B Hold Queue RTD") %>%
        set_attribute("return_day", function() now(env)) %>%
        set_attribute("return_echelon", 2) %>%
        credit_rtd() %>%
        release_selected(id = 5) %>%
        simmer::leave(1)
    )
}

#' Builds the R2B disposition of a casualty needing no surgery
#'
#' @param hold_beds       This R2B team's holding beds
#' @param evacuation_team This R2B team's own organic evacuation resource
#' @return Simmer trajectory routing the casualty to a holding bed here, to
#'   R2E, or to this team's holding queue.
r2b_no_surgery_path <- function(hold_beds, evacuation_team) {
  # Four-way routing policy:
  #
  # Branch 2d: drawn convalescence exceeds the evacuation policy: straight to
  #            R2E, where the policy evacuates the casualty from theatre
  # Branch 2a: this R2B unit's hold beds have capacity → seize immediately
  # Branch 2b: R2B hold full; R2E hold has capacity → bypass to R2E
  #            Also used when R2B hold queue cap is exceeded (fallback)
  # Branch 2c: R2B hold full; R2E hold full; queue within cap → join R2B queue
  #            Cap = floor(R2B_beds / (R2B_beds + R2E_beds) * R2B_beds)
  #            With 10 R2B and 30 R2E beds: cap = floor(10/40 * 10) = 2 patients
  trajectory("R2B No Surgery") %>%
    set_attribute("recovery_to_duty_days", draw_recovery_to_duty) %>%
    branch(
      option = function() {
        # Branch 2d: the casualty's convalescence exceeds the theatre
        # evacuation policy, so they are not held forward at all
        if (r2b_hold_ineligible()) return(4)

        # Branch 2a: this R2B unit has hold capacity
        r2b_usage <- sum(get_server_count(env, resources = hold_beds))
        r2b_cap   <- sum(get_capacity(env, resources = hold_beds))
        if (!is.na(r2b_usage) && !is.na(r2b_cap) && r2b_usage < r2b_cap) return(1)

        # R2B hold full — check R2E hold capacity
        all_r2e_hold <- unlist(lapply(env_data$elms$r2eheavy, `[[`, "hold_bed"))
        r2e_usage <- sum(get_server_count(env, resources = all_r2e_hold))
        r2e_cap   <- sum(get_capacity(env, resources = all_r2e_hold))
        if (!is.na(r2e_usage) && !is.na(r2e_cap) && r2e_usage < r2e_cap) return(2)

        # Both full — check global R2B hold queue against proportional cap
        all_r2b_hold  <- unlist(lapply(env_data$elms$r2b, `[[`, "hold_bed"))
        r2b_total_cap <- length(all_r2b_hold)
        r2e_total_cap <- length(all_r2e_hold)
        queue_cap     <- floor(r2b_total_cap / (r2b_total_cap + r2e_total_cap) *
                               r2b_total_cap)
        r2b_queue <- sum(get_queue_count(env, resources = all_r2b_hold))
        if (!is.na(r2b_queue) && r2b_queue < queue_cap) return(3)

        return(2)  # Queue cap exceeded — bypass to R2E regardless
      },
      continue = TRUE,
      # Branch 2a: Hold capacity available — seize and recover or evac
      r2b_hold_recovery(hold_beds, evacuation_team),
      # Branch 2b: R2B full, R2E has capacity (or queue cap exceeded) — bypass
      r2b_hold_bypass(evacuation_team),
      # Branch 2c: Both full, queue within proportional cap — queue at R2B
      r2b_hold_queue_recovery(hold_beds, evacuation_team),
      # Branch 2d: convalescence beyond the evacuation policy, no forward hold
      r2b_hold_ineligible_path(evacuation_team)
    )
}

#' Applies the R2B arrival died-of-wounds check
#'
#' @param trj     Trajectory to append the check to
#' @param team_id Integer index of the Role 2B team handling treatment
#' @return The trajectory, with the check and its mortuary arm appended.
#'
#' @details The conditional increment runs from the last check (at R1) to the
#'   elapsed time on arrival here, so the journey between the two echelons is
#'   priced at this checkpoint.
r2b_dow_check <- function(trj, team_id) {
  died <- trajectory("Died of Wounds") %>%
    set_attribute("dow", 1) %>%
    set_attribute("dow_echelon", 2) %>%
    r2b_treat_kia(team_id) %>%
    release_selected(id = 1) %>%
    r2b_transport_kia(team_id) %>%
    simmer::leave(1)

  if (dow_care_state_enabled()) {
    return(trj %>% dow_check(cohort = TRUE, died = died,
                             survived = trajectory("Continue R2B Treatment")))
  }

  # Time-dependent logistic.
  # Disease DNBI (dnbi_type == 2) remain exempt — medical pathway, not trauma.
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
      trajectory("Continue R2B Treatment")
    )
}

#' Applies emergency resuscitation at R2B
#'
#' @param trj            Trajectory to append the resuscitation to
#' @param resus_beds     This R2B team's resuscitation beds
#' @param emergency_team This R2B team's emergency section
#' @return The trajectory, with the transfer to a resuscitation bed, the
#'   resuscitation itself and its effect on the died-of-wounds ceiling
#'   appended.
#'
#' @details The holding bed seized on arrival is released once the
#'   resuscitation bed is in hand, so a casualty never holds two beds except
#'   across the transfer itself.
r2b_resuscitation <- function(trj, resus_beds, emergency_team) {
  trj %>%
    simmer::select(resus_beds, policy = "shortest-queue", id = 2) %>%
    seize_selected(id = 2) %>%
    release_selected(id = 1) %>%

    # Step 3: Emergency resuscitation
    seize_resources(emergency_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$long_resus$min,
        b = env_data$vars$r2b$long_resus$max,
        c = env_data$vars$r2b$long_resus$mode
      )
    }) %>%
    set_attribute("r2b_resus", 1) %>%
    release_resources(emergency_team) %>%
    release_selected(id = 2) %>%
    dow_treat(function() dow_multipliers()$resuscitation,
              function() LEGACY_DOW_EFFICACY$r2b_resus_factor, resuscitation = TRUE)
}

#' Builds the R2B evacuation decision
#'
#' @param trj             Trajectory to append the decision to
#' @param evacuation_team This R2B team's own organic evacuation resource
#' @param wait_for_evac   The fallback trajectory built by r2b_wait_for_evac(),
#'   reached when the evacuation asset is already committed
#' @return The trajectory, with the evacuation decision appended.
r2b_evac_decision <- function(trj, evacuation_team, wait_for_evac) {
  # Branches based on evacuation team availability:
  # - evac available     → immediate transfer to R2E (r2b_to_r2e = 1)
  # - evac not available → wait in ICU bed until evac is free
  trj %>%
    branch(
      option = function() {
        usage <- sum(get_server_count(env, resources = evacuation_team))
        cap   <- sum(get_capacity(env, resources = evacuation_team))
        if (!is.na(usage) && !is.na(cap) && usage < cap) return(1)
        return(2)
      },
      continue = TRUE,

      # Path 1: Immediate evacuation to R2E
      trajectory("Immediate Evac") %>%
        r2b_evacuate_to_r2e(evacuation_team),

      # Path 2: Immediate evacuation not possible — wait in ICU
      join(wait_for_evac)
    )
}

#' Executes the full treatment pathway for WIA casualties at Role 2B
#'
#' @param team_id Integer index of the Role 2B team handling treatment
#' @return Simmer trajectory representing the entire WIA care pathway at R2B
#'
#' @details The body is the sequence of steps a casualty passes through, each a
#'   named builder above: a holding bed on arrival and the died-of-wounds check
#'   against the journey from R1 (r2b_dow_check()), resuscitation
#'   (r2b_resuscitation()), the surgical decision, and the evacuation decision
#'   (r2b_evac_decision()).
#'
#'   A casualty requiring surgery passes the pre-theatre intensive care gate
#'   (r2b_surgery_gate()) and then the theatre availability check
#'   (r2b_ot_check_path()), which operates on them forward, holds them for a
#'   section about to reopen, or bypasses them to R2E; a forward operation ends
#'   in the stabilisation phase (r2b_post_op_stabilisation()). A casualty
#'   requiring none takes r2b_no_surgery_path().
#'
#'   The builders take the resources they use rather than reading this team's
#'   establishment themselves, so what each one touches is visible in its
#'   signature. Two trajectories are built here and passed down rather than
#'   rebuilt at each use, the stabilisation phase and the theatre check, each
#'   being reached by two routes.
#'
#'   R2B → R2E WIA movement seizes each R2B team's own `evac` resource, not the
#'   shared PMVAmb fleet, a deliberate design: this leg represents
#'   an organic R2B unit asset, distinct from the brigade-pooled transport fleet
#'   used for R1 → R2B. It does model a dead-heading return leg on that same
#'   organic resource: once the R2B team's evac asset
#'   drops a casualty at R2E, it is unavailable to its own team until it
#'   completes the return trip.
r2b_treat_wia <- function(team_id) {
  hold_beds       <- env_data$elms$r2b[[team_id]][["hold_bed"]]
  resus_beds      <- env_data$elms$r2b[[team_id]][["resus_bed"]]
  ot_beds         <- env_data$elms$r2b[[team_id]][["ot_bed"]]
  icu_beds        <- env_data$elms$r2b[[team_id]][["icu_bed"]]
  emergency_team  <- env_data$elms$r2b[[team_id]][["emerg"]][[1]]
  evacuation_team <- env_data$elms$r2b[[team_id]][["evac"]][[1]]
  surg_teams      <- env_data$elms$r2b[[team_id]][["surg"]]
  icu_team        <- env_data$elms$r2b[[team_id]][["icu"]][[1]]

  post_op        <- r2b_post_op_stabilisation(icu_beds, hold_beds)
  ot_check       <- r2b_ot_check_path(team_id, ot_beds, surg_teams, post_op)
  wait_for_evac  <- r2b_wait_for_evac(icu_beds, icu_team, evacuation_team)

  trajectory("R2B Basic Flow") %>%
    set_attribute("r2b_treated", team_id) %>%
    set_attribute("r2b_treatment_start_time", function() now(env)) %>%

    # Step 1: Initial hold bed
    simmer::select(hold_beds, policy = "shortest-queue", id = 1) %>%
    seize_selected(id = 1) %>%

    # Step 1.5: DOW check on arrival
    r2b_dow_check(team_id) %>%
    set_attribute("last_dow_t", function() now(env)) %>%

    # Steps 2 and 3: Resuscitation bed and emergency resuscitation
    r2b_resuscitation(resus_beds, emergency_team) %>%

    # Step 4: Surgery decision
    # Branches based on attribute "surgery":
    # - surgery == 1 → intensive care gate, then the OT availability check
    # - surgery != 1 → hold bed recovery, R2E bypass, or hold queue
    branch(
      option = function() {
        needs_surg <- get_attribute(env, "surgery")
        if (!is.na(needs_surg) && needs_surg == 1) return(1)
        return(2)
      },
      continue = TRUE,

      # Branch 1: Surgery required
      r2b_surgery_gate(icu_beds, ot_check),

      # Branch 2: Surgery not required
      r2b_no_surgery_path(hold_beds, evacuation_team)
    ) %>%

    # Step 5: Evacuation decision
    r2b_evac_decision(evacuation_team, wait_for_evac)
}
