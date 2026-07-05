##############################################
## R/trajectories.R                         ##
## All simmer trajectory definitions        ##
##############################################

library(simmer)
library(simmer.bricks)
library(triangle)

# ── Helper functions ──────────────────────────────────────────────────────────

#' Seizes one unit of each resource in the provided list
#'
#' @param trj A simmer trajectory object
#' @param resources Character vector of resource names to seize
#' @return Modified trajectory with seize activities appended
seize_resources <- function(trj, resources) {
  for (res in resources) {
    trj <- trj %>% seize(res, 1)
  }
  trj
}

#' Releases one unit of each resource in the provided list
#'
#' @param trj A simmer trajectory object
#' @param resources Character vector of resource names to release
#' @return Modified trajectory with release activities appended
release_resources <- function(trj, resources) {
  for (res in resources) {
    trj <- trj %>% release(res, 1)
  }
  trj
}

#' Randomly selects one subteam of the specified type from the given team
#'
#' @param elm_type Element type (e.g. "r1", "r2b", "r2eheavy")
#' @param team_id Index of the team within the element type
#' @param subteam_type String identifying the subteam type (e.g. "surg", "evac")
#' @return Name of a randomly selected subteam as a character string
select_subteam <- function(elm_type, team_id, subteam_type) {
  subteams <- env_data$elms[[elm_type]][[team_id]][[subteam_type]]

  if (is.null(subteams) || length(subteams) == 0) {
    stop(sprintf("No subteams of type '%s' found for %s team %d", subteam_type, elm_type, team_id))
  }

  index <- sample(seq_along(subteams), 1)
  return(subteams[[index]])
}

#' Selects a randomly ordered R2B team with at least one free OT bed
#'
#' @param env The simmer simulation environment object
#' @return Index of the selected R2B team (integer), or -1 if none available
#'
#' @details Iterates through R2B teams in randomized order and returns the first
#'   team whose OT beds are completely free. Returns -1 if all are occupied.
select_available_r2b_team <- function(env) {
  for (i in sample(1:counts[["r2b"]])) {
    ot_beds    <- env_data$elms$r2b[[i]]$ot_bed
    bed_usage  <- sapply(ot_beds, function(b) get_server_count(env, b))
    total_in_use <- sum(bed_usage)

    if (total_in_use == 0) {
      return(i)
    }
  }

  return(-1)
}

#' Selects a randomly ordered R2B team whose hold bed occupancy is below threshold
#'
#' @param env The simmer simulation environment object
#' @return Index of the selected R2B team (integer), or -1 if none available
#'
#' @details Used for disease DNBI routing, which does not require OT availability.
#'   When env_data$vars$r2b$holding$hold_threshold is set (0–1 fraction), a team
#'   is only selected if its hold occupancy is strictly below that fraction of
#'   capacity. This reserves headroom for incoming Step 1 patients and prevents
#'   long-duration Step 4 holders from starving new arrivals. Returns -1 (route
#'   to R2E) if no R2B team is below threshold, routing the decision upstream.
#'   When hold_threshold is absent, falls back to selecting any team with at least
#'   one free bed (original behaviour).
select_r2b_for_hold <- function(env) {
  threshold <- env_data$vars$r2b$holding$hold_threshold
  use_threshold <- !is.null(threshold) && !is.na(threshold)

  for (i in sample(1:counts[["r2b"]])) {
    hold_beds <- env_data$elms$r2b[[i]]$hold_bed
    usage     <- sum(sapply(hold_beds, function(b) get_server_count(env, b)))
    cap       <- sum(sapply(hold_beds, function(b) get_capacity(env, b)))
    limit     <- if (use_threshold) threshold * cap else cap
    if (usage < limit) {
      return(i)
    }
  }
  return(-1)
}

#' Selects the R2E team with the highest available OT bed capacity
#'
#' @return Integer index of the selected R2E team (1-based). If no beds are
#'   available, returns a randomly selected team index.
#'
#' @details Calculates available OT slots per team and returns the team with
#'   maximum free capacity. Breaks ties by random selection among candidates.
select_r2e_team <- function() {
  capacities <- sapply(seq_along(env_data$elms$r2eheavy), function(team_id) {
    ot_beds <- env_data$elms$r2eheavy[[team_id]][["ot_bed"]]
    sum(sapply(ot_beds, function(bed) get_capacity(env, bed) - get_server_count(env, bed)))
  })

  if (all(capacities <= 0)) {
    selected <- sample(seq_along(env_data$elms$r2eheavy), 1)
  } else {
    max_capacity <- max(capacities)
    candidates   <- which(capacities == max_capacity)
    selected     <- sample(candidates, 1)
  }

  return(selected)
}

# ── DOW survival functions ────────────────────────────────────────────────────

#' Time-dependent DOW probability (shifted logistic)
#'
#' @param t_elapsed Elapsed minutes since injury
#' @param p_base    Minimum DOW probability at t = 0 (floor)
#' @param p_max     Asymptotic maximum DOW probability
#' @param k         Logistic steepness (min^-1)
#' @param t_mid     Inflection point in minutes (time at which DOW = (p_base + p_max) / 2)
#' @return DOW probability in [p_base, p_max]
dow_prob <- function(t_elapsed, p_base, p_max, k, t_mid) {
  p_base + (p_max - p_base) / (1 + exp(-k * (t_elapsed - t_mid)))
}

#' Conditional DOW probability increment between two check points
#'
#' @param t_now  Elapsed minutes since injury at the current check
#' @param t_prev Elapsed minutes since injury at the previous DOW check (0 = first check)
#' @param p_base,p_max,k,t_mid Parameters as for dow_prob()
#' @return Conditional probability P(die in [t_prev, t_now] | survived to t_prev)
#'
#' @details Used at R2B and R2E to avoid double-counting mortality already
#'   screened at R1. When t_prev = 0 (first check), reduces to the cumulative
#'   probability adjusted for the non-zero floor at t = 0.
dow_prob_conditional <- function(t_now, t_prev, p_base, p_max, k, t_mid) {
  f_now  <- dow_prob(t_now,  p_base, p_max, k, t_mid)
  f_prev <- dow_prob(t_prev, p_base, p_max, k, t_mid)
  if (f_prev >= 1) return(1)
  pmax(0, (f_now - f_prev) / (1 - f_prev))
}

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
#'   leg (Issue #6): after drop-off, the entity clones into a vehicle
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
          ) * env_data$vars$r1$kia_transport$return_leg_multiplier
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
#'   the dead-head return leg (Issue #6): after drop-off, the entity
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
          ) * env_data$vars$r1$wia_transport$return_leg_multiplier
        }) %>%
        release_selected(),
      trajectory("Casualty Dropped Off")
    ) %>%
    synchronize(wait = FALSE)
}

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

#' Simulates Role 2B transport of KIA casualties to collocated mortuary
#'
#' @param traj A simmer trajectory object
#' @param team_id Integer index of the selected Role 2B team
#' @return Modified trajectory with transport logic and mortuary_treated flag
r2b_transport_kia <- function(traj, team_id) {
  evacuation_team <- env_data$elms$r2b[[team_id]][["evac"]][[1]]
  traj %>%
    seize_resources(evacuation_team) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$kia_transport$min,
        b = env_data$vars$r2b$kia_transport$max,
        c = env_data$vars$r2b$kia_transport$mode
      )
    }) %>%
    set_attribute("mortuary_treated", 1) %>%
    release_resources(evacuation_team)
}

#' Transports WIA casualties from Role 2B to Role 2E Heavy
#'
#' @return New trajectory for casualty movement from R2B to R2E
#'
#' @details Selects PMV Ambulance asset (id = 7) using shortest-queue policy.
#'   Logs transport initiation via r2b_r2e_transport_start attribute and
#'   simulates outbound leg duration using triangular distribution. Models
#'   the dead-head return leg (Issue #6): after drop-off, the entity
#'   clones into a vehicle branch (unladen return timeout, then release
#'   under id = 7 — listed first so it inherits the pre-clone seize
#'   record) and a casualty branch (no further activity). The trailing
#'   synchronize(wait = FALSE) lets the casualty continue immediately
#'   once it reaches that point, while the vehicle clone is discarded
#'   when it later arrives there after completing its return leg.
r2b_transport_wia <- function() {
  trajectory("R2B to R2E Heavy transport") %>%
    simmer::select(env_data$transports$PMVAmb, policy = "shortest-queue", id = 7) %>%
    seize_selected(id = 7) %>%
    set_attribute("r2b_r2e_transport_start", function() now(env)) %>%
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
      trajectory("Vehicle Return Leg") %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2b$wia_transport$min,
            b = env_data$vars$r2b$wia_transport$max,
            c = env_data$vars$r2b$wia_transport$mode
          ) * env_data$vars$r2b$wia_transport$return_leg_multiplier
        }) %>%
        release_selected(id = 7),
      trajectory("Casualty Dropped Off")
    ) %>%
    synchronize(wait = FALSE)
}

#' Executes the full treatment pathway for WIA casualties at Role 2B
#'
#' @param team_id Integer index of the Role 2B team handling treatment
#' @return Simmer trajectory representing the entire WIA care pathway at R2B
#'
#' @details Models the following sequential steps:
#'
#' # Step 1: Hold bed — initial stabilization
#' # Step 2: DOW branch (~1%) — KIA treatment and mortuary transport
#' # Step 3: Resuscitation — seizes emergency team and resus bed
#'
#' # Step 4: Surgical decision branch
#' # Branches based on attribute "surgery":
#' # - surgery == 1 → pre-OT ICU availability gate (Issue #43; P1 always
#' #     proceeds, P2+ defers OT entry while this unit's ICU is saturated),
#' #     then check OT bed AND surgical team availability
#' #     - OT bed free, no queue, team on shift → seize OT + team, perform DAMCON surgery
#' #     - OT full, OR queued, OR team off-shift → bypass immediately to R2E
#' #         (r2b_bypassed = 1; r2b_bypass_reason = 1 team off-shift, 2 OT busy/queued)
#' # - surgery != 1 → hold bed recovery, set return_day, leave trajectory
#'
#' # Step 5: Evacuation decision branch
#' # Branches based on evacuation team availability:
#' # - evac available     → immediate transfer to R2E (r2b_to_r2e = 1)
#' # - evac not available → wait in ICU bed until evac is free
r2b_treat_wia <- function(team_id) {
  hold_beds       <- env_data$elms$r2b[[team_id]][["hold_bed"]]
  resus_beds      <- env_data$elms$r2b[[team_id]][["resus_bed"]]
  ot_beds         <- env_data$elms$r2b[[team_id]][["ot_bed"]]
  icu_beds        <- env_data$elms$r2b[[team_id]][["icu_bed"]]
  emergency_team  <- env_data$elms$r2b[[team_id]][["emerg"]][[1]]
  evacuation_team <- env_data$elms$r2b[[team_id]][["evac"]][[1]]
  surg_team       <- env_data$elms$r2b[[team_id]][["surg"]][[1]]
  icu_team        <- env_data$elms$r2b[[team_id]][["icu"]][[1]]

  # Fallback path: wait in ICU bed until evacuation resources become available
  wait_for_evac <- trajectory("Wait in Hold Bed for Evac") %>%
    set_global("evac_wait_count", function() {
      current <- get_global(env, "evac_wait_count")
      return(current + 1)
    }) %>%
    simmer::select(icu_beds, policy = "shortest-queue", id = 3) %>%
    seize_selected(id = 3) %>%
    seize_resources(icu_team) %>%
    seize_resources(evacuation_team) %>%
    release_resources(icu_team) %>%
    release_selected(id = 3) %>%
    set_attribute("r2b_to_r2e", 1) %>%
    set_attribute("r2e", function() select_r2e_team()) %>%
    set_attribute("r2b_departure_time", function() now(env)) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2b$wia_transport$min,
        b = env_data$vars$r2b$wia_transport$max,
        c = env_data$vars$r2b$wia_transport$mode
      )
    }) %>%
    release_resources(evacuation_team) %>%
    branch(
      option = function() get_attribute(env, "r2e"),
      continue = TRUE,
      lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
    ) %>%
    leave(1)

  # OT availability check (unchanged from pre-Issue-43 logic). Joined
  # directly when the pre-OT ICU gate above clears immediately, and again
  # after the P2+ ICU-defer wait loop resolves.
  r2b_ot_check_path <- trajectory("R2B OT Check") %>%
    branch(
      option = function() {
        usage    <- sum(get_server_count(env, resources = ot_beds))
        cap      <- sum(get_capacity(env, resources = ot_beds))
        queue    <- sum(get_queue_count(env, resources = ot_beds))
        team_cap <- sum(get_capacity(env, resources = surg_team))
        # OT available only when a bed is free, no queue exists, and team is on shift.
        # Any of these failing means the patient should bypass to R2E immediately.
        if (!is.na(usage) && !is.na(cap) && usage < cap && queue == 0 &&
            !is.na(team_cap) && team_cap > 0) return(1)
        return(2)
      },
      continue = TRUE,

      # Sub-branch 1: OT bed free and team on shift — perform DAMCON surgery
      trajectory("Surgery Path") %>%
        simmer::select(ot_beds, policy = "shortest-queue", id = 4) %>%
        seize_selected(id = 4) %>%
        seize_resources(surg_team) %>%
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
        set_attribute("dow_ceiling", function() {
          ceiling <- get_attribute(env, "dow_ceiling")
          if (is.na(ceiling)) return(ceiling)
          ceiling * env_data$vars$dow$treatment_efficacy$r2b_dcs_factor
        }),

      # Sub-branch 2: OT busy, queued, or team off-shift — bypass to R2E.
      # r2b_bypass_reason decomposes the cause (Issue #40): 1 = surgical team
      # off-shift (team_cap <= 0), 2 = OT bed busy or queued. Re-reads the same
      # resource state as the option() check above — no timeout intervenes
      # between the branch decision and this sub-trajectory, so state cannot
      # have changed.
      trajectory("OT Unavailable – Bypass to R2E") %>%
        set_attribute("r2b_bypassed", 1) %>%
        set_attribute("r2b_bypass_reason", function() {
          team_cap <- sum(get_capacity(env, resources = surg_team))
          if (!is.na(team_cap) && team_cap <= 0) return(1)
          return(2)
        })
    )

  trajectory("R2B Basic Flow") %>%
    set_attribute("r2b_treated", team_id) %>%
    set_attribute("r2b_treatment_start_time", function() now(env)) %>%

    # Step 1: Initial hold bed
    simmer::select(hold_beds, policy = "shortest-queue", id = 1) %>%
    seize_selected(id = 1) %>%

    # Step 1.5: DOW branch (time-dependent logistic, Issue #5)
    # Conditional increment from last DOW check (R1) to current elapsed time.
    # Disease DNBI (dnbi_type == 2) remain exempt — medical pathway, not trauma.
    # P3 casualties use a flat probability (minor wounds, not time-critical).
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
        set_attribute("dow_echelon", 2) %>%
        r2b_treat_kia(team_id) %>%
        release_selected(id = 1) %>%
        r2b_transport_kia(team_id) %>%
        simmer::leave(1),
      trajectory("Continue R2B Treatment")
    ) %>%
    set_attribute("last_dow_t", function() now(env)) %>%

    # Step 2: Transfer to resus bed
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
    set_attribute("dow_ceiling", function() {
      ceiling <- get_attribute(env, "dow_ceiling")
      if (is.na(ceiling)) return(ceiling)
      ceiling * env_data$vars$dow$treatment_efficacy$r2b_resus_factor
    }) %>%

    # Step 4: Surgery decision
    # Branches based on attribute "surgery":
    # - surgery == 1 → check OT availability
    #     - capacity available → seize OT, perform DAMCON surgery
    #     - no capacity        → skip surgery, proceed to evac
    # - surgery != 1 → hold bed recovery, set return_day, leave trajectory
    branch(
      option = function() {
        needs_surg <- get_attribute(env, "surgery")
        if (!is.na(needs_surg) && needs_surg == 1) return(1)
        return(2)
      },
      continue = TRUE,

      # Branch 1: Surgery required
      # Pre-OT ICU availability gate (Issue #43), mirroring the R2E pattern for
      # consistency and forward compatibility. R2B surgery does not seize
      # icu_beds for post-operative recovery (icu_beds here are used only by
      # the wait_for_evac fallback path above), so Priority 1 candidates
      # proceed unconditionally; only Priority 2+ candidates defer OT entry
      # while this unit's ICU is fully saturated, preserving ICU headroom for
      # existing evac-holding patients. Under current establishment sizing,
      # R2B ICU utilisation is effectively zero, so this branch is expected to
      # be inert in practice.
      trajectory("Needs Surgery") %>%
        branch(
          option = function() {
            prio <- get_attribute(env, "priority")
            if (!is.na(prio) && prio == 1) return(1)  # P1 always proceeds
            usage  <- sum(get_server_count(env, resources = icu_beds))
            cap    <- sum(get_capacity(env, resources = icu_beds))
            icu_ok <- !is.na(usage) && !is.na(cap) && usage < cap
            if (icu_ok) return(1)
            return(2)  # P2+, ICU saturated: defer OT entry
          },
          continue = TRUE,
          r2b_ot_check_path,
          trajectory("ICU Full — Defer Surgery (P2+)") %>%
            set_attribute("surgery_deferred", 1) %>%
            timeout(function() env_data$vars$r2b$icu_gating$defer_check_interval) %>%
            rollback(target = 1, check = function() {
              usage <- sum(get_server_count(env, resources = icu_beds))
              cap   <- sum(get_capacity(env, resources = icu_beds))
              !(!is.na(usage) && !is.na(cap) && usage < cap)
            }) %>%
            join(r2b_ot_check_path)
        ),

      # Branch 2: Surgery not required — recover in holding bed, queue, or bypass
      # Three-stage routing policy:
      #
      # Branch 2a: this R2B unit's hold beds have capacity → seize immediately
      # Branch 2b: R2B hold full; R2E hold has capacity → bypass to R2E
      #            Also used when R2B hold queue cap is exceeded (fallback)
      # Branch 2c: R2B hold full; R2E hold full; queue within cap → join R2B queue
      #            Cap = floor(R2B_beds / (R2B_beds + R2E_beds) * R2B_beds)
      #            With 10 R2B and 30 R2E beds: cap = floor(10/40 * 10) = 2 patients
      #
      # When env_data$vars$r2b$holding$evac_threshold is set (minutes), patients
      # in branches 2a and 2c whose drawn hold duration exceeds the threshold are
      # evacuated to R2E rather than returned to duty.
      trajectory("R2B No Surgery") %>%
        branch(
          option = function() {
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
          trajectory("R2B Hold") %>%
            simmer::select(hold_beds, policy = "first-available", id = 5) %>%
            seize_selected(id = 5) %>%
            set_attribute("r2b_hold_start", function() now(env)) %>%
            set_attribute("r2b_hold_drawn", function() {
              rtriangle(
                n = 1,
                a = env_data$vars$r2b$holding$min,
                b = env_data$vars$r2b$holding$max,
                c = env_data$vars$r2b$holding$mode
              )
            }) %>%
            timeout(function() {
              drawn  <- get_attribute(env, "r2b_hold_drawn")
              thresh <- env_data$vars$r2b$holding$evac_threshold
              if (!is.null(thresh) && !is.na(thresh)) min(drawn, thresh) else drawn
            }) %>%
            # Evac-threshold branch
            # - drawn > threshold → release hold bed, transport to R2E (r2b_hold_evac = 1)
            # - drawn <= threshold → return to duty (return_day set, leave)
            branch(
              option = function() {
                drawn  <- get_attribute(env, "r2b_hold_drawn")
                thresh <- env_data$vars$r2b$holding$evac_threshold
                if (!is.null(thresh) && !is.na(thresh) && drawn > thresh) return(1)
                return(2)
              },
              continue = TRUE,
              trajectory("R2B Hold Threshold — Early Evac") %>%
                set_attribute("r2b_hold_evac", 1) %>%
                release_selected(id = 5) %>%
                set_attribute("r2b_to_r2e", 1) %>%
                set_attribute("r2e", function() select_r2e_team()) %>%
                seize_resources(evacuation_team) %>%
                set_attribute("r2b_departure_time", function() now(env)) %>%
                timeout(function() {
                  rtriangle(
                    n = 1,
                    a = env_data$vars$r2b$wia_transport$min,
                    b = env_data$vars$r2b$wia_transport$max,
                    c = env_data$vars$r2b$wia_transport$mode
                  )
                }) %>%
                release_resources(evacuation_team) %>%
                branch(
                  option = function() get_attribute(env, "r2e"),
                  continue = TRUE,
                  lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
                ) %>%
                simmer::leave(1),
              trajectory("R2B Hold RTD") %>%
                set_attribute("return_day", function() now(env)) %>%
                set_attribute("return_echelon", 2) %>%
                release_selected(id = 5) %>%
                simmer::leave(1)
            ),
          # Branch 2b: R2B full, R2E has capacity (or queue cap exceeded) — bypass
          trajectory("R2B Hold Full — Bypass to R2E") %>%
            set_attribute("r2b_hold_bypass", 1) %>%
            set_attribute("r2b_to_r2e", 1) %>%
            set_attribute("r2e", function() select_r2e_team()) %>%
            seize_resources(evacuation_team) %>%
            set_attribute("r2b_departure_time", function() now(env)) %>%
            timeout(function() {
              rtriangle(
                n = 1,
                a = env_data$vars$r2b$wia_transport$min,
                b = env_data$vars$r2b$wia_transport$max,
                c = env_data$vars$r2b$wia_transport$mode
              )
            }) %>%
            release_resources(evacuation_team) %>%
            branch(
              option = function() get_attribute(env, "r2e"),
              continue = TRUE,
              lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
            ) %>%
            simmer::leave(1),
          # Branch 2c: Both full, queue within proportional cap — queue at R2B
          trajectory("R2B Hold Queue — R2E Full") %>%
            set_attribute("r2b_hold_queued", 1) %>%
            simmer::select(hold_beds, policy = "shortest-queue", id = 5) %>%
            seize_selected(id = 5) %>%
            set_attribute("r2b_hold_start", function() now(env)) %>%
            set_attribute("r2b_hold_drawn", function() {
              rtriangle(
                n = 1,
                a = env_data$vars$r2b$holding$min,
                b = env_data$vars$r2b$holding$max,
                c = env_data$vars$r2b$holding$mode
              )
            }) %>%
            timeout(function() {
              drawn  <- get_attribute(env, "r2b_hold_drawn")
              thresh <- env_data$vars$r2b$holding$evac_threshold
              if (!is.null(thresh) && !is.na(thresh)) min(drawn, thresh) else drawn
            }) %>%
            branch(
              option = function() {
                drawn  <- get_attribute(env, "r2b_hold_drawn")
                thresh <- env_data$vars$r2b$holding$evac_threshold
                if (!is.null(thresh) && !is.na(thresh) && drawn > thresh) return(1)
                return(2)
              },
              continue = TRUE,
              trajectory("R2B Hold Queue Threshold — Early Evac") %>%
                set_attribute("r2b_hold_evac", 1) %>%
                release_selected(id = 5) %>%
                set_attribute("r2b_to_r2e", 1) %>%
                set_attribute("r2e", function() select_r2e_team()) %>%
                seize_resources(evacuation_team) %>%
                set_attribute("r2b_departure_time", function() now(env)) %>%
                timeout(function() {
                  rtriangle(
                    n = 1,
                    a = env_data$vars$r2b$wia_transport$min,
                    b = env_data$vars$r2b$wia_transport$max,
                    c = env_data$vars$r2b$wia_transport$mode
                  )
                }) %>%
                release_resources(evacuation_team) %>%
                branch(
                  option = function() get_attribute(env, "r2e"),
                  continue = TRUE,
                  lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
                ) %>%
                simmer::leave(1),
              trajectory("R2B Hold Queue RTD") %>%
                set_attribute("return_day", function() now(env)) %>%
                set_attribute("return_echelon", 2) %>%
                release_selected(id = 5) %>%
                simmer::leave(1)
            )
        )
    ) %>%

    # Step 5: Evacuation decision
    # Branches based on evacuation team availability:
    # - evac available     → immediate transfer to R2E (r2b_to_r2e = 1)
    # - evac not available → wait in ICU bed until evac is free
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
        set_attribute("r2b_to_r2e", 1) %>%
        set_attribute("r2e", function() select_r2e_team()) %>%
        seize_resources(evacuation_team) %>%
        set_attribute("r2b_departure_time", function() now(env)) %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2b$wia_transport$min,
            b = env_data$vars$r2b$wia_transport$max,
            c = env_data$vars$r2b$wia_transport$mode
          )
        }) %>%
        release_resources(evacuation_team) %>%
        branch(
          option = function() get_attribute(env, "r2e"),
          continue = TRUE,
          lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
        ) %>%
        simmer::leave(1),

      # Path 2: Immediate evacuation not possible — wait in ICU
      join(wait_for_evac)
    )
}

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

#' Models the full R2E Heavy treatment flow for WIA casualties
#'
#' @param team_id Integer ID of the R2E team assigned to receive the casualty
#' @return Simmer trajectory representing clinical care and disposition at R2E
#'
#' @details Implements the following phases:
#'
#' # Phase 1: DOW check (~1%)
#' # - If DOW: route to r2e_treat_kia() and r2e_transport_kia(), then leave
#' # - Else:   continue to resuscitation
#'
#' # Phase 2: Initial resuscitation
#' # Branches based on attribute "r2b_resus":
#' # - r2b_resus == 1 → short resus (prior R2B resus occurred)
#' # - else           → full resus, sets r2e_resus = 1
#'
#' # Phase 3: Surgical branch (OT–ICU gating, Issue #43)
#' # Branches based on attribute "surgery":
#' # - surgery == 1 → pre-OT ICU availability check, then:
#' #     - ICU available            → seize OT, DAMCON surgery, then ICU recovery
#' #                                   (short/full per prior R2B surgery + recovery
#' #                                   prob, unchanged from pre-Issue-43 behaviour)
#' #     - ICU full, priority <= icu_gating$p1_bypass_priority_max (P1)
#' #                                → seize OT, DAMCON surgery, then post-operative
#' #                                   HOLDING bed recovery (post_op_pathway = 2) with
#' #                                   an elevated dow_ceiling (r2e_postop_hold_penalty)
#' #                                   reflecting reduced post-op monitoring
#' #     - ICU full, priority > threshold (P2+)
#' #                                → defer OT entry (surgery_deferred = 1); poll ICU
#' #                                   availability every icu_gating$defer_check_interval
#' #                                   minutes until free, then proceed as "ICU available"
#' #   Both ICU and post-op-hold recovery paths converge on a shared post-operative
#' #   DOW check (time-dependent conditional increment, Issue #5) before Phase 4.
#' # - surgery != 1 → no surgery needed
#'
#' # Phase 4: Second surgery (only if R2E Phase 3 surgery occurred without prior R2B DAMCON)
#' # Branches based on attributes "r2e_surgery" and "r2b_surgery":
#' # - r2e_surgery == 1 AND r2b_surgery != 1 → perform second surgery at R2E
#' # - else (not a surgical candidate, or had R2B DAMCON)  → skip second surgery
#'
#' # Phase 5: Final disposition
#' # Branches on in_theatre_rate probability:
#' # - recover in theatre → seize hold bed, set return_day
#' # - strategic evac     → set r2e_evac = 1
r2e_treat_wia <- function(team_id) {
  hold_beds       <- env_data$elms$r2eheavy[[team_id]][["hold_bed"]]
  resus_beds      <- env_data$elms$r2eheavy[[team_id]][["resus_bed"]]
  ot_beds         <- env_data$elms$r2eheavy[[team_id]][["ot_bed"]]
  icu_beds        <- env_data$elms$r2eheavy[[team_id]][["icu_bed"]]
  emergency_teams <- env_data$elms$r2eheavy[[team_id]][["emerg"]]
  evacuation_teams <- env_data$elms$r2eheavy[[team_id]][["evac"]]
  surg_teams      <- env_data$elms$r2eheavy[[team_id]][["surg"]]
  icu_teams       <- env_data$elms$r2eheavy[[team_id]][["icu"]]

  emergency_team <- select_subteam("r2eheavy", team_id, "emerg")
  surg_team      <- select_subteam("r2eheavy", team_id, "surg")
  evac_team      <- select_subteam("r2eheavy", team_id, "evac")
  icu_team       <- select_subteam("r2eheavy", team_id, "icu")

  # ── OT–ICU gating sub-trajectories (Issue #43) ──────────────────────────────
  # Built once per team and joined at the points below. Shared post-operative
  # DOW check: both the ICU and post-op-hold recovery paths converge here so
  # that the two pathways' realised mortality can be directly compared (see
  # README "Died of Wounds" — Post-Operative Checkpoint). dow_echelon = 4
  # distinguishes this checkpoint from the Phase 1 R2E arrival DOW check
  # (dow_echelon = 3).
  r2e_post_op_dow_check <- trajectory("R2E Post-Operative DOW Check") %>%
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

  # Nominal pathway: ICU bed available at time of OT entry. Recovery duration
  # (short vs full ICU) unchanged from pre-Issue-43 behaviour.
  r2e_icu_recovery <- trajectory("R2E ICU Recovery") %>%
    set_attribute("post_op_pathway", 1) %>%
    branch(
      option = function() {
        prior_surg <- get_attribute(env, "r2b_surgery")
        if (!is.na(prior_surg) && prior_surg == 1) {
          return(sample(1:2, size = 1, prob = c(env_data$vars$r2eheavy$recovery$post_surgery, 1 - env_data$vars$r2eheavy$recovery$post_surgery)))
        }
        return(2)
      },
      continue = TRUE,
      trajectory("Short ICU Recovery") %>%
        simmer::select(icu_beds, policy = "shortest-queue", id = 6) %>%
        seize_selected(id = 6) %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$short_icu$min,
            b = env_data$vars$r2eheavy$short_icu$max,
            c = env_data$vars$r2eheavy$short_icu$mode
          )
        }) %>%
        release_selected(id = 6),
      trajectory("Full ICU Recovery") %>%
        simmer::select(icu_beds, policy = "shortest-queue", id = 6) %>%
        seize_selected(id = 6) %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$long_icu$min,
            b = env_data$vars$r2eheavy$long_icu$max,
            c = env_data$vars$r2eheavy$long_icu$mode
          )
        }) %>%
        release_selected(id = 6)
    ) %>%
    join(r2e_post_op_dow_check)

  # Bypass pathway: ICU full and priority within the P1 override threshold
  # (env_data$vars$r2eheavy$icu_gating$p1_bypass_priority_max). Surgery
  # proceeds; recovery is in a holding bed instead of ICU. dow_ceiling is
  # multiplied by r2e_postop_hold_penalty (> 1) to reflect the elevated
  # mortality risk of reduced post-operative monitoring — see README "Died
  # of Wounds — Treatment Efficacy Modifiers".
  #
  # MODEL ASSUMPTION — P1 SURGERY WITHOUT ICU: a surgeon operates on a
  # Priority 1 candidate even when no post-operative ICU bed is available,
  # accepting elevated post-operative mortality risk rather than withholding
  # surgery (which would expose an unsurgicated P1 casualty to near-certain
  # DOW). See README Limitations for basis, uncertainty, and consequence.
  r2e_hold_recovery <- trajectory("R2E Post-Op Hold Recovery") %>%
    set_attribute("post_op_pathway", 2) %>%
    set_attribute("dow_ceiling", function() {
      ceiling <- get_attribute(env, "dow_ceiling")
      if (is.na(ceiling)) return(ceiling)
      ceiling * env_data$vars$dow$treatment_efficacy$r2e_postop_hold_penalty
    }) %>%
    simmer::select(hold_beds, policy = "shortest-queue", id = 8) %>%
    seize_selected(id = 8) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2eheavy$post_op_hold$min,
        b = env_data$vars$r2eheavy$post_op_hold$max,
        c = env_data$vars$r2eheavy$post_op_hold$mode
      )
    }) %>%
    release_selected(id = 8) %>%
    join(r2e_post_op_dow_check)

  # Shared surgery portion (OT seizure through DAMCON surgery). Recovery
  # (ICU vs post-op hold) is decided upstream at the pre-OT gating branch
  # and joined on afterwards, so this portion is identical for both paths.
  r2e_ot_surgery <- trajectory("R2E OT — DAMCON Surgery") %>%
    simmer::select(ot_beds, policy = "shortest-queue", id = 4) %>%
    seize_selected(id = 4) %>%
    set_attribute("r2e_surgery", 1) %>%
    set_attribute("r2e_surgery_1_start", function() now(env)) %>%
    timeout(function() {
      rtriangle(
        n = 1,
        a = env_data$vars$r2eheavy$surgery$min,
        b = env_data$vars$r2eheavy$surgery$max,
        c = env_data$vars$r2eheavy$surgery$mode
      )
    }) %>%
    set_attribute("r2e_surgery_1_end", function() now(env)) %>%
    release_selected(id = 4) %>%
    set_attribute("dow_ceiling", function() {
      ceiling <- get_attribute(env, "dow_ceiling")
      if (is.na(ceiling)) return(ceiling)
      ceiling * env_data$vars$dow$treatment_efficacy$r2e_dcs1_factor
    })

  r2e_surgery_icu_path <- trajectory("R2E Surgery — ICU Available") %>%
    join(r2e_ot_surgery) %>%
    join(r2e_icu_recovery)

  r2e_surgery_hold_path <- trajectory("R2E Surgery — ICU Full, P1 to Post-Op Hold") %>%
    join(r2e_ot_surgery) %>%
    join(r2e_hold_recovery)

  # Deferral pathway: ICU full and priority above the P1 override threshold
  # (P2+). OT entry is deferred rather than proceeding without ICU backup;
  # the candidate polls ICU availability every icu_gating$defer_check_interval
  # minutes (timeout + rollback, no resources held while waiting) until a bed
  # frees, then proceeds exactly as the nominal ICU-available path.
  r2e_surgery_defer_path <- trajectory("R2E Surgery — Deferred (ICU Full, P2+)") %>%
    set_attribute("surgery_deferred", 1) %>%
    timeout(function() env_data$vars$r2eheavy$icu_gating$defer_check_interval) %>%
    rollback(target = 1, check = function() {
      usage <- sum(get_server_count(env, resources = icu_beds))
      cap   <- sum(get_capacity(env, resources = icu_beds))
      !(!is.na(usage) && !is.na(cap) && usage < cap)
    }) %>%
    join(r2e_surgery_icu_path)

  trajectory("R2E Treatment") %>%
    set_attribute("r2e_treated", team_id) %>%
    set_attribute("r2e_handling", 1) %>%
    set_attribute("r2e_arrival_time", function() now(env)) %>%

    # Phase 1: DOW check (time-dependent logistic, Issue #5)
    # Conditional increment from last DOW check to current elapsed time since injury.
    # Disease DNBI (dnbi_type == 2) exempt — medical pathway, not trauma.
    # P3 casualties use a flat probability (minor wounds, not time-critical).
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
    set_attribute("last_dow_t", function() now(env)) %>%

    # Phase 2: Resuscitation bed seizure
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
    ) %>%

    # Phase 3: Surgical branch — pre-OT ICU availability gate (Issue #43)
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
    # - surgery != 1 → no surgery needed
    branch(
      option = function() {
        needs_surg <- get_attribute(env, "surgery")
        if (is.na(needs_surg) || needs_surg != 1) return(4)

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
      r2e_surgery_icu_path,
      r2e_surgery_hold_path,
      r2e_surgery_defer_path,
      trajectory("No Surgery Needed")
    ) %>%

    # Phase 4: Second surgery if patient had R2E Phase 3 surgery but not prior R2B DAMCON
    # A second procedure is only meaningful for patients who underwent Phase 3 surgery
    # at R2E (r2e_surgery == 1) without a prior R2B DAMCON (r2b_surgery != 1).
    # Patients with surgery == 0 never set r2e_surgery, so is.na(r2e_surg) guards them out.
    branch(
      option = function() {
        r2e_surg   <- get_attribute(env, "r2e_surgery")
        prior_surg <- get_attribute(env, "r2b_surgery")
        if (!is.na(r2e_surg) && r2e_surg == 1 &&
            (is.na(prior_surg) || prior_surg != 1)) return(1)
        return(2)
      },
      continue = TRUE,
      trajectory("Second Surgery Before Disposition") %>%
        simmer::select(ot_beds, policy = "shortest-queue", id = 7) %>%
        seize_selected(id = 7) %>%
        set_attribute("r2e_surgery_2_start", function() now(env)) %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$surgery$min,
            b = env_data$vars$r2eheavy$surgery$max,
            c = env_data$vars$r2eheavy$surgery$mode
          )
        }) %>%
        set_attribute("r2e_surgery_2_end", function() now(env)) %>%
        release_selected(id = 7) %>%
        set_attribute("dow_ceiling", function() {
          ceiling <- get_attribute(env, "dow_ceiling")
          if (is.na(ceiling)) return(ceiling)
          ceiling * env_data$vars$dow$treatment_efficacy$r2e_dcs2_factor
        }),
      trajectory("No Second Surgery Needed")
    ) %>%

    # Phase 5: Final disposition
    # Branches on in_theatre_rate probability:
    # - in-theatre recovery → seize hold bed, log return_day
    # - strategic evac      → set r2e_evac = 1
    branch(
      option = function() sample(1:2, 1, prob = c(env_data$vars$r2eheavy$recovery$in_theatre_rate, 1 - env_data$vars$r2eheavy$recovery$in_theatre_rate)),
      continue = TRUE,
      trajectory("Recover at R2E") %>%
        simmer::select(hold_beds, policy = "shortest-queue", id = 5) %>%
        seize_selected(id = 5) %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$holding$min,
            b = env_data$vars$r2eheavy$holding$max,
            c = env_data$vars$r2eheavy$holding$mode
          )
        }) %>%
        release_selected(id = 5) %>%
        set_attribute("r2e_departure_time", function() now(env)) %>%
        set_attribute("return_day", function() now(env)) %>%
        set_attribute("return_echelon", 3),
      trajectory("Strategic Evac") %>%
        set_attribute("r2e_departure_time", function() now(env)) %>%
        set_attribute("r2e_evac", 1)
    )
}

# ── Core casualty trajectory ──────────────────────────────────────────────────

#' Builds the core casualty trajectory covering R1 through R2 disposition
#'
#' @return Simmer trajectory for all casualty types from point of injury
#'
#' @details Encapsulates initial triage, R1 stabilization, early mortality,
#'   and evacuation decisions for KIA, WIA, and DNBI casualties.
#'
#' # Phase 1: Attribute assignment
#' # - Assigns R1 team (random selection)
#' # - Sets priority (WIA/DNBI) via weighted sample
#' # - Sets dnbi_type (DNBI cases only): 1=battle_fatigue, 2=disease, 3=nbi
#' # - Computes surgery requirement based on priority tier and dnbi_type
#'
#' # Phase 2: Casualty type branch
#' # Branches based on casualty name prefix:
#' # - "wia" or "dnbi" → WIA/DNBI handling path
#' # - else             → KIA handling path
#'
#' # WIA/DNBI path:
#' # - R1 treatment by assigned team
#' # - DOW branch (~5% P1, ~2.5% P2) → KIA processing if flagged
#' # - Evacuation decision for P1/P2 → R2B or R2E bypass
#' # - P3/no-evac → recover at R1, set return_day
#'
#' # KIA path:
#' # - R1 mortuary treatment and KIA transport
build_casualty_trajectory <- function() {
  trajectory("Casualty") %>%
    log_(function() paste0(get_name(env))) %>%
    set_attribute("injury_time", function() now(env)) %>%
    set_attribute("last_dow_t",  function() now(env)) %>%
    set_attribute("priority", function() {
      if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) {
        sample(1:3, 1, prob = c(env_data$vars$r1$priority$one,
                                env_data$vars$r1$priority$two,
                                env_data$vars$r1$priority$three))
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
    set_attribute("team", function() sample(1:counts[["r1"]], 1)) %>%

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
        # - dnbi_type == 2 (disease)        → evac decision (no DOW); surgery=0 forces R2B hold path
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
            simmer::leave(1),

          # Branch 2: Disease — evacuation decision (no DOW, no surgery candidacy)
          # surgery attribute is forced to 0 for disease; R2B routes them to hold path
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
                branch(
                  option = function() {
                    r2b <- get_attribute(env, "r2b")
                    if (r2b > 0) return(1) else return(2)
                  },
                  continue = TRUE,
                  trajectory("Disease To R2B") %>%
                    branch(
                      option = function() get_attribute(env, "r2b"),
                      continue = TRUE,
                      lapply(1:counts[["r2b"]], r2b_treat_wia)
                    ),
                  trajectory("Disease Bypass R2B → R2E") %>%
                    set_attribute("r2b_bypassed", 1) %>%
                    branch(
                      option = function() sample(1:counts[["r2eheavy"]], 1),
                      continue = TRUE,
                      lapply(1:counts[["r2eheavy"]], r2e_treat_wia)
                    )
                ),

              trajectory("Disease Monitor Recovery") %>%
                timeout(function() {
                  rtriangle(
                    n = 1,
                    a = env_data$vars$r1$recovery$min,
                    b = env_data$vars$r1$recovery$max,
                    c = env_data$vars$r1$recovery$mode
                  )
                }) %>%
                set_attribute("return_day", function() now(env)) %>%
                set_attribute("return_echelon", 1)
            ),

          # Branch 3: NBI or WIA — standard DOW + evac logic
          trajectory("NBI/WIA Standard Path") %>%
            # DOW branch — time-dependent logistic (Issue #5)
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
                branch(
                  option = function() get_attribute(env, "team"),
                  continue = TRUE,
                  lapply(1:counts[["r1"]], function(i) {
                    r1_treat_kia(i) %>% join(r1_transport_kia())
                  })
                ),

              # Path 2: Continue to evacuation decision
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
                    ),

                  # Path 2: Recover at R1
                  trajectory("Monitor Recovery") %>%
                    timeout(function() {
                      rtriangle(
                        n = 1,
                        a = env_data$vars$r1$recovery$min,
                        b = env_data$vars$r1$recovery$max,
                        c = env_data$vars$r1$recovery$mode
                      )
                    }) %>%
                    set_attribute("return_day", function() now(env)) %>%
                    set_attribute("return_echelon", 1)
                )
            )
        ),

      # Path 2: KIA handling
      trajectory("KIA Branch") %>%
        branch(
          option = function() get_attribute(env, "team"),
          continue = TRUE,
          lapply(1:counts[["r1"]], function(i) {
            r1_treat_kia(i) %>% join(r1_transport_kia())
          })
        )
    )
}
