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

#' Selects a randomly ordered R2B team with at least one free hold bed
#'
#' @param env The simmer simulation environment object
#' @return Index of the selected R2B team (integer), or -1 if none available
#'
#' @details Used for disease DNBI routing, which does not require OT availability.
#'   Returns the first team (in random order) whose hold bed capacity is not
#'   fully occupied. Returns -1 if all R2B teams have full hold beds.
select_r2b_for_hold <- function(env) {
  for (i in sample(1:counts[["r2b"]])) {
    hold_beds <- env_data$elms$r2b[[i]]$hold_bed
    usage     <- sum(sapply(hold_beds, function(b) get_server_count(env, b)))
    cap       <- sum(sapply(hold_beds, function(b) get_capacity(env, b)))
    if (usage < cap) {
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
#'   time with attribute transport_start_time.
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
    release_selected()
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
#'   triangular distributed timeout to simulate transit duration.
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
    release_selected()
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
#'   simulates round-trip duration using triangular distribution.
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
    release_selected(id = 7)
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
#' # - surgery == 1 → check OT bed AND surgical team availability
#' #     - OT bed free, no queue, team on shift → seize OT + team, perform DAMCON surgery
#' #     - OT full, OR queued, OR team off-shift → bypass immediately to R2E (r2b_bypassed = 1)
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

  trajectory("R2B Basic Flow") %>%
    set_attribute("r2b_treated", team_id) %>%
    set_attribute("r2b_treatment_start_time", function() now(env)) %>%

    # Step 1: Initial hold bed
    simmer::select(hold_beds, policy = "shortest-queue", id = 1) %>%
    seize_selected(id = 1) %>%

    # Step 1.5: DOW branch (~1%)
    # Branches on runif:
    # - < 0.01 → casualty dies of wounds; routed to KIA handling, trajectory exits
    # - else   → continue treatment
    # Disease DNBI (dnbi_type == 2) are exempt: their pathway is medical/surgical,
    # not trauma resuscitation, and the 1% DOW rate is calibrated for combat trauma.
    branch(
      option = function() {
        dtype <- get_attribute(env, "dnbi_type")
        if (!is.na(dtype) && dtype == 2L) return(2)  # disease: exempt from DOW
        if (runif(1) < 0.01) return(1)
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
      trajectory("Needs Surgery") %>%
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
            release_selected(id = 4),

          # Sub-branch 2: OT busy, queued, or team off-shift — bypass to R2E
          trajectory("OT Unavailable – Bypass to R2E") %>%
            set_attribute("r2b_bypassed", 1)
        ),

      # Branch 2: Surgery not required — recover in holding bed, return to duty
      trajectory("R2B No Surgery") %>%
        simmer::select(hold_beds, policy = "first-available", id = 5) %>%
        seize_selected(id = 5) %>%
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2b$holding$min,
            b = env_data$vars$r2b$holding$max,
            c = env_data$vars$r2b$holding$mode
          )
        }) %>%
        set_attribute("return_day", function() now(env)) %>%
        set_attribute("return_echelon", 2) %>%
        release_selected(id = 5) %>%
        simmer::leave(1)
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
#' # Phase 3: Surgical branch
#' # Branches based on attribute "surgery":
#' # - surgery == 1 → seize OT bed, perform DAMCON surgery
#' #   - Sub-branch on r2b_surgery:
#' #       - r2b_surgery == 1 and recovery prob → short ICU
#' #       - else                               → full ICU
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

  trajectory("R2E Treatment") %>%
    set_attribute("r2e_treated", team_id) %>%
    set_attribute("r2e_handling", 1) %>%
    set_attribute("r2e_arrival_time", function() now(env)) %>%

    # Phase 1: DOW check (~1%)
    # - DOW → KIA handling and leave
    # - else → continue to resuscitation
    branch(
      option = function() {
        if (runif(1) < 0.01) return(1)
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
        release_resources(emergency_team) %>%
        release_selected(id = 2)
    ) %>%

    # Phase 3: Surgical branch
    # Branches based on attribute "surgery":
    # - surgery == 1 → seize OT bed, perform DAMCON surgery
    #   - Sub-branch on r2b_surgery + recovery probability:
    #       - prior R2B surgery + recovery prob → short ICU
    #       - else                              → full ICU
    # - surgery != 1 → no surgery needed
    branch(
      option = function() {
        needs_surg <- get_attribute(env, "surgery")
        if (!is.na(needs_surg) && needs_surg == 1) return(1)
        return(2)
      },
      continue = TRUE,
      trajectory("R2E Surgery") %>%
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
        ),
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
        release_selected(id = 7),
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
    set_attribute("priority", function() {
      if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) {
        sample(1:3, 1, prob = c(env_data$vars$r1$priority$one,
                                env_data$vars$r1$priority$two,
                                env_data$vars$r1$priority$three))
      } else {
        NA
      }
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
            # DOW branch
            # - P1 with runif < pri1_dow → DOW (KIA processing)
            # - P2 with runif < pri2_dow → DOW (KIA processing)
            # - else                     → proceed to evacuation decision
            branch(
              option = function() {
                prio <- get_attribute(env, "priority")
                if (prio == 1 && runif(1) < env_data$vars$r1$other$pri1_dow) return(1)
                if (prio == 2 && runif(1) < env_data$vars$r1$other$pri2_dow) return(1)
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
