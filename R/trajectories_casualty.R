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

#' Simulates Role 1 transport of WIA or DNBI casualties to Role 2 facilities
#'
#' @return Simmer trajectory modeling casualty movement from R1 to R2
#'
#' @details Selects a PMV Ambulance asset using shortest-queue policy. Logs
#'   transport start time via transport_start_time attribute. Applies
#'   triangular distributed timeout to simulate transit duration. Models
#'   the dead-head return leg: after drop-off, the entity
#'   clones into a vehicle branch (unladen return timeout, then release —
#'   listed first so it inherits the pre-clone seize record) and a
#'   casualty branch (no further activity). The trailing
#'   synchronize(wait = FALSE) lets the casualty continue immediately
#'   once it reaches that point, while the vehicle clone is discarded
#'   when it later arrives there after completing its return leg.
r1_transport_wia <- function() {
  trajectory("Transport WIA") %>%
    simmer::select(env_data$transports$PMVAmb, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r1$wia_transport$min,
        b = env_data$vars$r1$wia_transport$max,
        c = env_data$vars$r1$wia_transport$mode
      )
    }) %>%
    clone(
      n = 2,
      trajectory("Vehicle Return Leg") %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r1$wia_transport$min,
            b = env_data$vars$r1$wia_transport$max,
            c = env_data$vars$r1$wia_transport$mode
          )
        }) %>%
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
#' @details Sets priority (WIA and DNBI only) by weighted draw, a
#'   casualty-surge-tagged casualty drawing from its own event's priority split
#'   in "scheduled" mode and from the shared blast-dominant split in "poisson"
#'   mode; dow_ceiling, the priority's died-of-wounds ceiling; dnbi_type
#'   (1 = battle fatigue, 2 = disease, 3 = non-battle injury) for DNBI
#'   casualties; the surgical requirement, which follows from priority and
#'   dnbi_type; the damage control pathway; and the R1 team that treats the
#'   casualty.
assign_clinical_attributes <- function(trj) {
  trj %>%
    set_attribute("priority", function() {
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
    set_attribute("dow_ceiling", function() {
      prio <- get_attribute(env, "priority")
      dp   <- env_data$vars$dow$params
      if (is.na(prio)) return(NA_real_)
      if (prio == 1) dp$p1_p_max
      else if (prio == 2) dp$p2_p_max
      else dp$p3_flat
    }) %>%
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
    set_attribute("surgery", function() {
      prio      <- get_attribute(env, "priority")
      dnbi_type <- get_attribute(env, "dnbi_type")
      name      <- get_name(env)

      if (is.na(prio)) return(0)

      # Battle fatigue: no surgery candidacy
      if (!is.na(dnbi_type) && dnbi_type == 1L) return(0)

      # Disease: small probability for emergency surgical conditions (appendicitis,
      # cholecystitis, perforated ulcer); applied unconditionally across priorities
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

    # Staged damage control or a single-stage definitive procedure, decided
    # here rather than at either theatre so both echelons read one value; see
    # draw_dcs_pathway() above.
    set_attribute("dcs_pathway", draw_dcs_pathway) %>%
    set_attribute("team", function() sample(1:counts[["r1"]], 1))
}

#' Builds the R1 convalescence and return-to-duty pathway
#'
#' @param name Quoted trajectory name, which differs between the arms that use
#'   this pathway and so appears in the model's structure under both.
#' @return Simmer trajectory holding the casualty at R1 for a drawn
#'   convalescence, then returning it to duty.
#'
#' @details Serves a casualty who is not evacuated forward: a triangular
#'   convalescence, then return_day and return_echelon, then the credit back to
#'   the casualty's own pool (`credit_rtd()`).
r1_monitor_recovery <- function(name) {
  trajectory(name) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r1$recovery$min,
        b = env_data$vars$r1$recovery$max,
        c = env_data$vars$r1$recovery$mode
      )
    }) %>%
    set_attribute("return_day", function() now(env)) %>%
    set_attribute("return_echelon", 1) %>%
    credit_rtd()
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

#' Builds the battle fatigue holding pathway at R1
#'
#' @return Simmer trajectory holding a battle fatigue casualty at R1 until
#'   return to duty, then leaving the model.
#'
#' @details Battle fatigue is held forward and returned to duty without R2
#'   routing and without a died-of-wounds check, so the casualty leaves the
#'   trajectory rather than continuing to the evacuation decision.
r1_battle_fatigue_hold <- function() {
  trajectory("Battle Fatigue R1 Hold") %>%
    set_attribute("dnbi_bf_hold", 1) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r1$recovery$min,
        b = env_data$vars$r1$recovery$max,
        c = env_data$vars$r1$recovery$mode
      )
    }) %>%
    set_attribute("return_day", function() now(env)) %>%
    set_attribute("return_echelon", 1) %>%
    credit_rtd() %>%
    simmer::leave(1)
}

#' Builds the disease evacuation decision at R1
#'
#' @return Simmer trajectory deciding whether a disease casualty is evacuated
#'   to R2B, bypassed to R2E where no R2B holding capacity is available, or
#'   held at R1 for convalescence.
#'
#' @details A disease casualty faces no died-of-wounds check and carries no
#'   surgical requirement, so its forward routing is a holding-bed placement:
#'   `select_r2b_for_hold()` chooses the R2B team, and a casualty finding none
#'   bypasses to R2E.
r1_disease_evac_decision <- function() {
  trajectory("Disease Evac Decision") %>%
    branch(
      option = function() {
        prio <- get_attribute(env, "priority")
        if (is.na(prio)) return(2)
        if (prio == 1 && runif(1) < env_data$vars$r1$other$pri1_evac) return(1)
        if (prio == 2 && runif(1) < env_data$vars$r1$other$pri2_evac) return(1)
        return(2)
      },
      continue = TRUE,

      trajectory("Disease Transport to R2B") %>%
        set_attribute("r2b", function() select_r2b_for_hold(env)) %>%
        join(r1_transport_wia()) %>%
        # Branches on whether select_r2b_for_hold() found a holding bed:
        # - If a team was selected, treat at that R2B team
        # - If none had a bed, bypass R2B and continue to R2E
        branch(
          option = function() {
            r2b <- get_attribute(env, "r2b")
            if (r2b > 0) return(1) else return(2)
          },
          continue = TRUE,
          trajectory("Disease To R2B") %>%
            # Dispatch on attribute "r2b", the team selected above:
            # - Routes to that team's treatment trajectory, one arm per team
            branch(
              option = function() get_attribute(env, "r2b"),
              continue = TRUE,
              lapply(1:counts[["r2b"]], r2b_treat_wia)
            ),
          trajectory("Disease Bypass R2B → R2E") %>%
            set_attribute("r2b_bypassed", 1) %>%
            # Dispatch on a uniformly drawn R2E team, no team having been
            # selected forward:
            # - Routes to that team's treatment trajectory, one arm per team
            branch(
              option = function() sample(1:counts[["r2eheavy"]], 1),
              continue = TRUE,
              lapply(1:counts[["r2eheavy"]], r2e_treat_wia)
            )
        ),

      r1_monitor_recovery("Disease Monitor Recovery")
    )
}

#' Builds the forward evacuation of a casualty leaving R1
#'
#' @return Simmer trajectory transporting the casualty to a selected R2B team,
#'   or bypassing R2B for R2E where no team is available.
#'
#' @details `select_available_r2b_team()` chooses the team before the road
#'   move, so a casualty with no team available is tagged r2b_bypassed and
#'   routed to a randomly selected R2E Heavy element instead.
r1_forward_evacuation <- function() {
  trajectory("Transport to R2b") %>%
    set_attribute("r2b", function() select_available_r2b_team(env)) %>%
    join(r1_transport_wia()) %>%

    # R2B availability branch
    # - r2b > 0 → evacuate to selected R2B team
    # - r2b <= 0 → bypass R2B, send directly to R2E
    branch(
      option = function() {
        r2b <- get_attribute(env, "r2b")
        if (r2b > 0) return(1) else return(2)
      },
      continue = TRUE,

      # Path 1: R2B treatment
      trajectory("To R2B") %>%
        branch(
          option = function() get_attribute(env, "r2b"),
          continue = TRUE,
          lapply(1:counts[["r2b"]], r2b_treat_wia)
        ),

      # Path 2: Bypass R2B, route directly to R2E
      trajectory("Bypass R2B → To R2E") %>%
        set_attribute("r2b_bypassed", 1) %>%
        branch(
          option = function() sample(1:counts[["r2eheavy"]], 1),
          continue = TRUE,
          lapply(1:counts[["r2eheavy"]], r2e_treat_wia)
        )
    )
}

#' Builds the post-treatment disposition of a casualty surviving R1
#'
#' @return Simmer trajectory deciding between forward evacuation and
#'   convalescence at R1.
#'
#' @details The last died-of-wounds check time is stamped before the decision,
#'   so the next check upstream measures from here. Priority 1 and 2 casualties
#'   are evacuated at their configured rates; every other casualty convalesces
#'   at R1.
r1_post_treatment_decision <- function() {
  trajectory("Post-Treatment Decision") %>%
    set_attribute("last_dow_t", function() now(env)) %>%
    # Evacuation decision branch
    # - P1 with runif < pri1_evac → evacuate to next echelon
    # - P2 with runif < pri2_evac → evacuate to next echelon
    # - else                      → recover at R1
    branch(
      option = function() {
        prio <- get_attribute(env, "priority")
        if (is.na(prio)) return(2)
        if (prio == 1 && runif(1) < env_data$vars$r1$other$pri1_evac) return(1)
        if (prio == 2 && runif(1) < env_data$vars$r1$other$pri2_evac) return(1)
        return(2)
      },
      continue = TRUE,

      # Path 1: Evacuate to R2B or bypass to R2E
      r1_forward_evacuation(),

      # Path 2: Recover at R1
      r1_monitor_recovery("Monitor Recovery")
    )
}

#' Builds the standard R1 pathway for wounded and non-battle injury casualties
#'
#' @return Simmer trajectory applying the died-of-wounds check at R1 and, for
#'   the survivors, the post-treatment disposition.
#'
#' @details This is the pathway every casualty other than battle fatigue and
#'   disease takes, and the only one of the three carrying a died-of-wounds
#'   check at R1.
r1_nbi_wia_standard_path <- function() {
  trajectory("NBI/WIA Standard Path") %>%
    # DOW branch — time-dependent logistic
    # Probability is a shifted logistic function of elapsed time since injury.
    # At R1, t_prev = 0 (first DOW check), so the conditional increment equals
    # the cumulative DOW probability adjusted for the non-zero p_base floor.
    # P3 casualties have no DOW check at R1 (minor wounds, not time-critical).
    branch(
      option = function() {
        prio   <- get_attribute(env, "priority")
        injury <- get_attribute(env, "injury_time")
        t_prev <- get_attribute(env, "last_dow_t") - injury
        t_now  <- now(env) - injury
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
        }
        return(2)
      },
      continue = TRUE,

      # Path 1: Died of wounds — treated as KIA
      trajectory("Died of Wounds at Role 1") %>%
        set_attribute("dow", 1) %>%
        set_attribute("dow_echelon", 1) %>%
        r1_mortuary_handling(),

      # Path 2: Continue to evacuation decision
      r1_post_treatment_decision()
    )
}

#' Builds the R1 pathway for wounded and disease or non-battle injury casualties
#'
#' @return Simmer trajectory treating the casualty at its R1 team, applying the
#'   treatment effect on the died-of-wounds ceiling, and routing it by DNBI
#'   sub-type.
#'
#' @details R1 treatment (`r1_treat_wia()`) multiplies the casualty's
#'   died-of-wounds ceiling by the tactical combat casualty care factor before
#'   any check is made against it, which is what makes treatment at R1 reduce
#'   mortality downstream.
r1_wia_dnbi_path <- function() {
  trajectory("WIA/DNBI Branch") %>%
    branch(
      option = function() get_attribute(env, "team"),
      continue = TRUE,
      lapply(1:counts[["r1"]], r1_treat_wia)
    ) %>%
    set_attribute("dow_ceiling", function() {
      ceiling <- get_attribute(env, "dow_ceiling")
      if (is.na(ceiling)) return(ceiling)
      ceiling * env_data$vars$dow$treatment_efficacy$r1_tccc_factor
    }) %>%

    # DNBI sub-type routing branch
    # Applies differentiated pathways based on dnbi_type attribute:
    # - dnbi_type == 1 (battle_fatigue) → R1 hold → RTD; no R2 routing, no DOW
    # - dnbi_type == 2 (disease)        → evac decision (no DOW); placed by R2B holding occupancy
    # - dnbi_type == 3 (nbi) or WIA     → standard DOW + evac logic
    branch(
      option = function() {
        dtype <- get_attribute(env, "dnbi_type")
        if (!is.na(dtype) && dtype == 1L) return(1)  # battle fatigue
        if (!is.na(dtype) && dtype == 2L) return(2)  # disease
        return(3)                                      # nbi or WIA
      },
      continue = TRUE,

      # Branch 1: Battle fatigue — hold at R1, return to duty; no R2 routing
      r1_battle_fatigue_hold(),

      # Branch 2: Disease — evacuation decision (no DOW check). A disease
      # casualty carries the emergency surgical draw (disease_surgery_pct) and,
      # if it needs surgery, takes the R2B surgical pathway; otherwise holding
      r1_disease_evac_decision(),

      # Branch 3: NBI or WIA — standard DOW + evac logic
      r1_nbi_wia_standard_path()
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
