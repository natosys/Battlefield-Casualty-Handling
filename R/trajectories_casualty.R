##############################################
## R/trajectories_casualty.R              ##
## Role 1 and the casualty trajectory     ##
##############################################
#
# The Role 1 trajectories and the core casualty trajectory that assigns
# each casualty's injury and clinical attributes and routes it from R1.
#
# Sourced by R/trajectories.R, which loads the packages and R/constants.R
# these functions use; source that file rather than this one alone.

# ── Role 1 trajectories ───────────────────────────────────────────────────────

#' Simulates mortuary treatment pathway for KIA casualties at Role 1
#'
#' @param team Index of R1 team array
#' @return Simmer trajectory for KIA treatment flow
#'
#' @details Selects technician from specified R1 team and applies a
#'   fixed-duration mortuary treatment process. Sets attributes r1_treated
#'   and mortuary_treated to record team and disposition status.
r1_treat_kia <- function(team) {
  medics <- env_data$elms$r1[[team]][grepl("_technician_", env_data$elms$r1[[team]])]
  trajectory(paste("KIA Team", team)) %>%
    set_attribute("r1_treated", team) %>%
    set_attribute("mortuary_treated", 1) %>%
    simmer::select(medics, policy = "shortest-queue") %>%
    seize_selected() %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r1$kia_treat$min,
        b = env_data$vars$r1$kia_treat$max,
        c = env_data$vars$r1$kia_treat$mode
      )
    }) %>%
    release_selected()
}

#' Simulates KIA transport from Role 1 to mortuary at Role 2
#'
#' @return Simmer trajectory for KIA vehicle transport
#'
#' @details Uses HX240M transport asset with shortest-queue selection policy.
#'   Applies triangular delay to simulate movement time and records start
#'   time with attribute transport_start_time. Models the dead-head return
#'   leg: after drop-off, the entity clones into a vehicle
#'   branch (unladen return timeout, then release — listed first so it
#'   inherits the pre-clone seize record) and a casualty branch (no
#'   further activity). The trailing synchronize(wait = FALSE) lets the
#'   casualty continue immediately once it reaches that point, while the
#'   vehicle clone is discarded when it later arrives there after
#'   completing its return leg.
r1_transport_kia <- function() {
  trajectory("Transport KIA") %>%
    simmer::select(env_data$transports$HX240M, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r1$kia_transport$min,
        b = env_data$vars$r1$kia_transport$max,
        c = env_data$vars$r1$kia_transport$mode
      )
    }) %>%
    clone(
      n = 2,
      trajectory("Vehicle Return Leg") %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r1$kia_transport$min,
            b = env_data$vars$r1$kia_transport$max,
            c = env_data$vars$r1$kia_transport$mode
          )
        }) %>%
        release_selected(),
      trajectory("Casualty Dropped Off")
    ) %>%
    synchronize(wait = FALSE)
}

#' Executes Role 1 treatment sequence for WIA casualties
#'
#' @param team Index of R1 team array
#' @return Simmer trajectory for WIA treatment logic
#'
#' @details Seizes technician and clinician from specified R1 team.
#'   Treatment duration is based on casualty priority using a triangular
#'   distribution. Start time logged via treatment_start_time attribute.
#'   Releases all seized resources at end of trajectory using release_all().
r1_treat_wia <- function(team) {
  medics     <- env_data$elms$r1[[team]][grepl("_technician_", env_data$elms$r1[[team]])]
  clinicians <- env_data$elms$r1[[team]][grepl("_clinician_", env_data$elms$r1[[team]])]

  trajectory(paste("r1-", team, " treat wia")) %>%
    simmer::select(medics, policy = "shortest-queue") %>%
    seize_selected() %>%
    simmer::select(clinicians, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("treatment_start_time", function() now(env)) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r1$wia_treat$min,
        b = env_data$vars$r1$wia_treat$max,
        c = env_data$vars$r1$wia_treat$mode
      )
    }) %>%
    release_all()
}

#' Minutes of one road move from R1 to the Role 2 facility selected for it
#'
#' @return One draw from the R1 to R2B leg (`r1.wia_transport`) and, for a
#'   casualty bypassing R2B (attribute `r2b` not positive), a second from the
#'   R2B to R2E leg (`r2b.wia_transport`).
#'
#' @details R2E lies behind the R2B facilities on the same evacuation axis
#'   (README Scenario Context), so a casualty sent from R1 directly to R2E
#'   travels both legs without stopping. Called once for the laden move and once
#'   for the unladen return, each drawing afresh.
r1_evacuation_leg_minutes <- function() {
  #' One draw from a leg's triangular distribution
  #'
  #' @param v The leg's `min`, `max` and `mode`, in minutes.
  #' @return Minutes.
  leg <- function(v) rtriangle(n = 1, a = v$min, b = v$max, c = v$mode)
  minutes <- leg(env_data$vars$r1$wia_transport)
  if (get_attribute(env, "r2b") <= 0) minutes <- minutes + leg(env_data$vars$r2b$wia_transport)
  minutes
}

#' Simulates Role 1 transport of WIA or DNBI casualties to Role 2 facilities
#'
#' @return Simmer trajectory modeling casualty movement from R1 to R2
#'
#' @details Selects a PMV Ambulance asset using shortest-queue policy. Logs
#'   transport start time via transport_start_time attribute. The move is one
#'   leg to R2B or two to R2E (r1_evacuation_leg_minutes()), so the facility is
#'   chosen before the casualty boards. Models the dead-head return leg: after
#'   drop-off, the entity clones into a vehicle branch (unladen return timeout
#'   over the same legs, then release — listed first so it inherits the
#'   pre-clone seize record) and a casualty branch (no further activity). The
#'   trailing synchronize(wait = FALSE) lets the casualty continue immediately
#'   once it reaches that point, while the vehicle clone is discarded when it
#'   later arrives there after completing its return leg.
r1_transport_wia <- function() {
  trajectory("Transport WIA") %>%
    simmer::select(env_data$transports$PMVAmb, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    timeout(r1_evacuation_leg_minutes) %>%
    clone(
      n = 2,
      trajectory("Vehicle Return Leg") %>%
        timeout(r1_evacuation_leg_minutes) %>%
        release_selected(),
      trajectory("Casualty Dropped Off")
    ) %>%
    synchronize(wait = FALSE)
}

# ── Core casualty trajectory ──────────────────────────────────────────────────

#' Assign the attributes a casualty carries from the point of injury
#'
#' @param trj Trajectory to append the assignments to
#' @return The trajectory, with the identity attributes assigned.
#'
#' @details Sets injury_time and last_dow_t, debits the casualty's pool
#'   (`debit_force_size()`), and assigns injury_type (1 = WIA, 2 = DNBI,
#'   3 = KIA, from the casualty's name prefix; read by the Role 4 census in
#'   `R/analysis.R` at strategic evacuation), casualty_surge_event_id (the
#'   1-indexed event the casualty originated from, 0 for a background
#'   casualty) and casualty_surge_event (1 where that id is non-zero).
assign_injury_attributes <- function(trj) {
  trj %>%
    log_(function() paste0(get_name(env))) %>%
    set_attribute("injury_time", function() now(env)) %>%
    debit_force_size() %>%
    set_attribute("last_dow_t",  function() now(env)) %>%
    # injury_type: 1 = WIA, 2 = DNBI, 3 = KIA. Read downstream by
    # the Role 4 census (R/analysis_role4_ame.R::assign_role4_los()) to route DNBI
    # strategic evacuees to the P3/DNBI length-of-stay category regardless of
    # their triage priority — see README Role 4 sub-section.
    set_attribute("injury_type", function() {
      name <- get_name(env)
      if (startsWith(name, "wia"))  return(1L)
      if (startsWith(name, "dnbi")) return(2L)
      3L
    }) %>%
    set_attribute("casualty_surge_event_id", function() {
      name <- get_name(env)
      # Two streams carry a casualty surge overlay, the combat wounded and
      # the combat killed, each with its own sink built in emission order
      # by wrap_with_casualty_surge() (R/environment.R).
      sink <- if (startsWith(name, "wia_cbt")) {
        wia_cbt_casualty_surge_event_id
      } else if (startsWith(name, "kia_cbt")) {
        kia_cbt_casualty_surge_event_id
      } else {
        return(0L)
      }
      idx <- as.integer(sub("^[a-z]+_cbt", "", name)) + 1L
      if (idx >= 1L && idx <= length(sink)) sink[idx] else 0L
    }) %>%
    set_attribute("casualty_surge_event", function() {
      if (get_attribute(env, "casualty_surge_event_id") > 0) 1 else 0
    })
}

#' Assign the clinical attributes that route a casualty through the model
#'
#' @param trj Trajectory to append the assignments to
#' @return The trajectory, with the clinical attributes assigned.
#'
#' @details Sets dnbi_type (1 = battle fatigue, 2 = disease, 3 = non-battle
#'   injury) for DNBI casualties; priority, by the rule for the casualty's
#'   kind (disease_priority() below); the surgical requirement, which follows
#'   from priority and dnbi_type; under the legacy mechanism, dow_ceiling, the
#'   priority's died-of-wounds ceiling; the damage control pathway; and the R1
#'   team that treats the casualty.
#'
#'   A wounded or non-battle injury casualty draws priority from the trauma
#'   split, a casualty-surge-tagged one from its own event's split in
#'   "scheduled" mode and from the shared blast-dominant split in "poisson"
#'   mode. Battle fatigue is Priority 3 without a draw. A disease casualty is
#'   Priority 1 at the medical emergency share, and otherwise Priority 2 if it
#'   needs emergency surgery and Priority 3 if not, so its priority is settled
#'   only once its surgical requirement is known.
assign_clinical_attributes <- function(trj) {
  #' Sets the legacy mechanism's starting ceiling
  #'
  #' @param trj Trajectory to append to.
  #' @return The trajectory, unchanged under the care-state model, whose rate
  #'   follows from priority and needs no per-casualty parameter.
  legacy_dow_ceiling <- function(trj) {
    if (dow_care_state_enabled()) return(trj)
    trj %>% set_attribute("dow_ceiling", function() {
      prio <- get_attribute(env, "priority")
      dp   <- LEGACY_DOW_PARAMS
      if (is.na(prio)) return(NA_real_)
      if (prio == 1) dp$p1_p_max
      else if (prio == 2) dp$p2_p_max
      else dp$p3_flat
    })
  }

  trj %>%
    set_attribute("dnbi_type", function() {
      name <- get_name(env)
      if (startsWith(name, "dnbi")) {
        sample(1:3, 1, prob = c(
          env_data$vars$r1$other$battle_fatigue_pct,
          env_data$vars$r1$other$disease_pct,
          env_data$vars$r1$other$nbi_pct
        ))
      } else {
        NA_integer_
      }
    }) %>%
    set_attribute("priority", function() {
      dtype <- get_attribute(env, "dnbi_type")
      # Battle fatigue and disease follow their own rule rather than the trauma
      # split: battle fatigue is Priority 3, and disease is Priority 1 at the
      # medical emergency share, provisionally 3 otherwise until its surgical
      # requirement is drawn below.
      if (!is.na(dtype) && dtype == 1L) return(3)
      if (!is.na(dtype) && dtype == 2L) return(if (runif(1) < disease_p1_share()) 1 else 3)
      if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) {
        if (get_attribute(env, "casualty_surge_event") == 1) {
          eid    <- get_attribute(env, "casualty_surge_event_id")
          ev_row <- casualty_surge_event_priority_table[casualty_surge_event_priority_table$event_id == eid, ]
          # Per-event priority (scheduled mode) if the event's own row has
          # one; poisson-mode events carry NA pri_one, falling back to the
          # shared casualty_surge priority split.
          prob <- if (nrow(ev_row) == 1 && !is.na(ev_row$pri_one[1])) {
            c(ev_row$pri_one[1], ev_row$pri_two[1], ev_row$pri_three[1])
          } else {
            c(env_data$vars$casualty_surge$priority$one,
              env_data$vars$casualty_surge$priority$two,
              env_data$vars$casualty_surge$priority$three)
          }
          sample(1:3, 1, prob = prob)
        } else {
          sample(1:3, 1, prob = c(env_data$vars$r1$priority$one,
                                  env_data$vars$r1$priority$two,
                                  env_data$vars$r1$priority$three))
        }
      } else {
        NA
      }
    }) %>%
    set_attribute("surgery", function() {
      prio      <- get_attribute(env, "priority")
      dnbi_type <- get_attribute(env, "dnbi_type")
      name      <- get_name(env)

      if (is.na(prio)) return(0)

      # Battle fatigue: no surgery candidacy
      if (!is.na(dnbi_type) && dnbi_type == 1L) return(0)

      # Disease: small probability for emergency surgical conditions (appendicitis,
      # cholecystitis, perforated ulcer), drawn whatever the medical emergency
      # draw gave, so a medical emergency can also need surgery
      if (!is.na(dnbi_type) && dnbi_type == 2L) {
        return(as.numeric(runif(1) < env_data$vars$r1$other$disease_surgery_pct))
      }

      if (prio == 1) return(as.numeric(runif(1) < env_data$vars$r1$other$pri1_surgery))
      if (prio == 2) return(as.numeric(runif(1) < env_data$vars$r1$other$pri2_surgery))

      # P3: NBI DNBI or WIA
      if (!is.na(dnbi_type) && dnbi_type == 3) {
        return(as.numeric(runif(1) < env_data$vars$r1$other$pri3_dnbi_surgery))
      } else {
        return(as.numeric(runif(1) < env_data$vars$r1$other$pri3_other_surgery))
      }
    }) %>%
    # A disease casualty below Priority 1 who needs emergency surgery is
    # Priority 2, treatment within 6 to 12 hours; one who does not stays 3.
    set_attribute("priority", function() {
      prio  <- get_attribute(env, "priority")
      dtype <- get_attribute(env, "dnbi_type")
      if (is.na(dtype) || dtype != 2L || prio == 1) return(prio)
      if (get_attribute(env, "surgery") == 1) 2 else 3
    }) %>%
    legacy_dow_ceiling() %>%

    # Staged damage control or a single-stage definitive procedure, decided
    # here rather than at either theatre so both echelons read one value; see
    # draw_dcs_pathway() above.
    set_attribute("dcs_pathway", draw_dcs_pathway) %>%
    set_attribute("team", function() sample(1:counts[["r1"]], 1))
}

#' Route a casualty to its R1 team's mortuary handling and onward transport
#'
#' @param trj Trajectory to append the mortuary branch to
#' @return The trajectory, with the per-team mortuary branch appended.
#'
#' @details Reached both by a casualty killed in action and by one who dies of
#'   wounds at R1, which is why it is a segment rather than part of either arm:
#'   the two differ in the attributes they set beforehand, not in the handling
#'   that follows.
r1_mortuary_handling <- function(trj) {
  trj %>%
    branch(
      option = function() get_attribute(env, "team"),
      continue = TRUE,
      lapply(1:counts[["r1"]], function(i) {
        r1_treat_kia(i) %>% join(r1_transport_kia())
      })
    )
}

#' The share of disease casualties who are medical emergencies, Priority 1
#'
#' @return `r1.other.disease_p1_pct`, validated as a probability.
disease_p1_share <- function() {
  share <- env_data$vars$r1$other$disease_p1_pct
  if (is.null(share) || length(share) != 1L || !is.numeric(share) || is.na(share) ||
      share < 0 || share > 1) {
    stop(sprintf("r1.other.disease_p1_pct must be a probability, found %s",
                 paste(format(share), collapse = ", ")), call. = FALSE)
  }
  share
}

#' Selects the R2B team a casualty leaving R1 is sent to
#'
#' @return Index of the selected R2B team, or -1 to bypass R2B for R2E.
#'
#' @details Which forward facility can receive a casualty depends on what the
#'   casualty needs: a casualty requiring surgery needs a theatre, and goes to a
#'   team whose theatre is idle (`select_available_r2b_team()`); every other
#'   casualty needs a bed, and goes to a team whose holding beds are below the
#'   holding threshold (`select_r2b_for_hold()`). The rule reads surgical need,
#'   not casualty type, so a non-surgical casualty is not diverted rearward
#'   because the forward theatres are busy.
select_r2b_for_casualty <- function() {
  needs_surg <- get_attribute(env, "surgery")
  if (!is.na(needs_surg) && needs_surg == 1) return(select_available_r2b_team(env))
  select_r2b_for_hold(env)
}

#' Builds the move of a casualty from R1 to the Role 2 facility selected for it
#'
#' @param to_r2b Function of a team index returning the trajectory a casualty
#'   takes at that R2B team.
#' @param to_r2e Function of a team index returning the trajectory a casualty
#'   takes at that R2E Heavy team.
#' @return Simmer trajectory selecting the facility, moving the casualty there
#'   and handing it to that facility's pathway.
#'
#' @details The facility is chosen before the road move, so the move is one leg
#'   to R2B or two to R2E (r1_evacuation_leg_minutes()). A casualty for whom no
#'   R2B team qualifies is tagged r2b_bypassed and routed to a randomly selected
#'   R2E Heavy element.
r1_evacuate_to_r2 <- function(to_r2b, to_r2e) {
  trajectory("Evacuate from R1") %>%
    set_attribute("r2b", select_r2b_for_casualty) %>%
    join(r1_transport_wia()) %>%
    # Branches on the R2B team selected above:
    # - a team was selected -> that team's pathway, one arm per team
    # - none qualified      -> bypass R2B to a uniformly drawn R2E team
    branch(
      option = function() if (get_attribute(env, "r2b") > 0) 1 else 2,
      continue = TRUE,
      trajectory("To R2B") %>%
        branch(
          option = function() get_attribute(env, "r2b"),
          continue = TRUE,
          lapply(1:counts[["r2b"]], to_r2b)
        ),
      trajectory("Bypass R2B → To R2E") %>%
        set_attribute("r2b_bypassed", 1) %>%
        branch(
          option = function() sample(1:counts[["r2eheavy"]], 1),
          continue = TRUE,
          lapply(1:counts[["r2eheavy"]], to_r2e)
        )
    )
}

#' Builds the standard R1 pathway for every casualty other than battle fatigue
#'
#' @return Simmer trajectory applying the died-of-wounds check at R1 and
#'   evacuating every survivor to Role 2.
#'
#' @details The casualty streams are Role 2 admissions by construction
#'   (README Casualty Generation), so every casualty who survives the check
#'   leaves R1. A disease casualty faces no risk at the check: under the
#'   care-state model its rate is zero (dow_exempt()), and the legacy branch
#'   passes it without a draw.
r1_standard_path <- function() {
  died <- trajectory("Died of Wounds at Role 1") %>%
    set_attribute("dow", 1) %>%
    set_attribute("dow_echelon", 1) %>%
    r1_mortuary_handling()

  evacuate <- trajectory("Evacuate") %>%
    set_attribute("last_dow_t", function() now(env)) %>%
    join(r1_evacuate_to_r2(r2b_treat_wia, r2e_treat_wia))

  # Under the care-state model the check settles the risk accrued since injury.
  if (dow_care_state_enabled()) {
    return(trajectory("Standard Path") %>%
             dow_check(cohort = FALSE, died = died, survived = evacuate))
  }

  trajectory("Standard Path") %>%
    # DOW branch — time-dependent logistic
    # Probability is a shifted logistic function of elapsed time since injury.
    # At R1, t_prev = 0 (first DOW check), so the conditional increment equals
    # the cumulative DOW probability adjusted for the non-zero p_base floor.
    # P3 casualties and disease have no DOW check at R1.
    branch(
      option = function() {
        if (dow_exempt()) return(2)
        prio   <- get_attribute(env, "priority")
        injury <- get_attribute(env, "injury_time")
        t_prev <- get_attribute(env, "last_dow_t") - injury
        t_now  <- now(env) - injury
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
        }
        return(2)
      },
      continue = TRUE,

      # Path 1: Died of wounds — treated as KIA
      died,

      # Path 2: Evacuate to Role 2
      evacuate
    )
}

#' Builds the R1 pathway for wounded and disease or non-battle injury casualties
#'
#' @return Simmer trajectory treating the casualty at its R1 team, applying the
#'   treatment effect on the died-of-wounds ceiling, and evacuating it to Role 2
#'   by the pathway its DNBI sub-type takes there.
#'
#' @details R1 treatment (`r1_treat_wia()`) moves the casualty to the R1
#'   treatment multiplier before any check is made, which is what lets
#'   treatment at R1 reduce mortality downstream. Every casualty then leaves
#'   R1: battle fatigue for a restoration stay in a Role 2 holding bed, without
#'   a died-of-wounds check, and every other casualty through the R1 check to
#'   the Role 2 treatment pathway.
r1_wia_dnbi_path <- function() {
  trajectory("WIA/DNBI Branch") %>%
    branch(
      option = function() get_attribute(env, "team"),
      continue = TRUE,
      lapply(1:counts[["r1"]], r1_treat_wia)
    ) %>%
    dow_treat(function() dow_multipliers()$r1_treatment,
              function() LEGACY_DOW_EFFICACY$r1_tccc_factor, resuscitation = FALSE) %>%

    # DNBI sub-type routing branch
    # - dnbi_type == 1 (battle fatigue) -> Role 2 holding bed for restoration,
    #   then return to duty; no died-of-wounds check, no strategic evacuation
    # - otherwise (WIA, non-battle injury, disease) -> R1 died-of-wounds check,
    #   then evacuation to the Role 2 treatment pathway
    branch(
      option = function() {
        dtype <- get_attribute(env, "dnbi_type")
        if (!is.na(dtype) && dtype == 1L) return(1)
        2
      },
      continue = TRUE,
      r1_evacuate_to_r2(
        function(i) battle_fatigue_restoration(env_data$elms$r2b[[i]][["hold_bed"]], 2),
        function(i) battle_fatigue_restoration(env_data$elms$r2eheavy[[i]][["hold_bed"]], 3)
      ),
      r1_standard_path()
    )
}

#' Builds the core casualty trajectory covering R1 through R2 disposition
#'
#' @return Simmer trajectory for all casualty types from point of injury
#'
#' @details The body is the assembly sequence: the attributes every casualty
#'   carries from the point of injury, the clinical attributes that route it,
#'   and the branch on casualty type. Each step is a named function above.
#'
#'   Phase 1 assigns the attributes (`assign_injury_attributes()` and
#'   `assign_clinical_attributes()`). Phase 2 branches on the casualty's name
#'   prefix: a wounded or DNBI casualty takes the R1 treatment pathway
#'   (`r1_wia_dnbi_path()`), and a casualty killed in action goes directly to
#'   its R1 team's mortuary handling and transport.
build_casualty_trajectory <- function() {
  trajectory("Casualty") %>%
    assign_injury_attributes() %>%
    assign_clinical_attributes() %>%

    # Phase 2: Casualty type branch
    # Branches on name prefix:
    # - "wia" or "dnbi" → WIA/DNBI handling (path 1)
    # - else             → KIA handling (path 2)
    branch(
      option = function() {
        if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) 1 else 2
      },
      continue = TRUE,

      # Path 1: WIA/DNBI handling
      r1_wia_dnbi_path(),

      # Path 2: KIA handling
      trajectory("KIA Branch") %>%
        r1_mortuary_handling()
    )
}
