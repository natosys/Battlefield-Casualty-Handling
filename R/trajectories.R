##############################################
## R/trajectories.R                         ##
## All simmer trajectory definitions        ##
##############################################

library(simmer)
library(simmer.bricks)
library(triangle)

# ── Shared trajectory names ───────────────────────────────────────────────────

#' Name format for the per-section R2E surgical blocks
#'
#' Used by r2e_treat_wia() to name each seize-operate-release block it builds,
#' one per surgical section, and by scripts/check_r2e_surgery_seizure.R to
#' locate those blocks in the printed trajectory. Holding the format in one
#' place means renaming the blocks cannot leave the regression check searching
#' for a label the model no longer uses. The single "%d" is the section index.
R2E_SURGERY_SECTION_FMT <- "R2E Surgery — Section %d"

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

#' Debits an entity's population pool (combat or support) from the
#' effective force size at the moment they become a casualty (Issue #18)
#'
#' @param trj A simmer trajectory object
#' @return Modified trajectory with the force-size debit appended
#'
#' @details Applied once, at `build_casualty_trajectory()`'s injury_time
#'   assignment, for every casualty (WIA/KIA/DNBI) in either pool — every
#'   casualty is momentarily removed from effective fighting strength the
#'   instant they occur. Pool membership is read from the entity's
#'   generator-assigned name (e.g. "wia_cbt3", "dnbi_spt1"), the same
#'   startsWith()/grepl() convention already used elsewhere in this file
#'   (e.g. the mass_casualty_event_id/priority attributes below) to recover
#'   stream identity from an entity deep in its trajectory. KIA and
#'   strategic-evac (r2e_evac = 1) entities never reach credit_rtd() (see
#'   below), so they remain a permanent loss without a separate subtraction
#'   term — see README Casualty Generation for the full mechanism.
debit_force_size <- function(trj) {
  trj %>%
    set_global("effective_force_combat", function() {
      cur <- get_global(env, "effective_force_combat")
      if (grepl("_cbt", get_name(env))) cur - 1 else cur
    }) %>%
    set_global("effective_force_support", function() {
      cur <- get_global(env, "effective_force_support")
      if (grepl("_spt", get_name(env))) cur - 1 else cur
    })
}

#' Credits an entity's population pool (combat or support) back to the
#' effective force size at the moment they return to duty (Issue #18)
#'
#' @param trj A simmer trajectory object
#' @return Modified trajectory with the force-size credit appended
#'
#' @details Applied at each of R1/R2B/R2E's existing
#'   `set_attribute("return_day", function() now(env))` RTD sites. Because
#'   `now(env)` already reflects the actual simulation time each echelon's
#'   own recovery/hold-bed timeout completes, crediting at this event
#'   inherently reflects each echelon's real recovery duration — no
#'   additional `return_echelon`-weighted delay is needed on top (see
#'   README Casualty Generation, MODEL ASSUMPTION — CONTINUOUS RTD/INJURY
#'   CREDITING).
credit_rtd <- function(trj) {
  trj %>%
    set_global("effective_force_combat", function() {
      cur <- get_global(env, "effective_force_combat")
      if (grepl("_cbt", get_name(env))) cur + 1 else cur
    }) %>%
    set_global("effective_force_support", function() {
      cur <- get_global(env, "effective_force_support")
      if (grepl("_spt", get_name(env))) cur + 1 else cur
    })
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

#' Selects the R2E surgical section best placed to take the next case
#'
#' @param team_id Index of the R2E team within env_data$elms$r2eheavy
#' @return Integer index (1-based) of the chosen surgical section within that
#'   team's "surg" sub-element list
#'
#' @details R2E fields more surgical sections than operating theatres, and
#'   build_env() gives each section its own alternating day/night shift, so the
#'   section that takes a case must be chosen per casualty rather than fixed
#'   when the trajectory is built. Sections are scored on current load, the sum
#'   of in-use and queued units across the section's member resources, which is
#'   the section-level analogue of the "shortest-queue" policy used for bed
#'   selection elsewhere in this file. On-shift sections (every member holding
#'   non-zero capacity) are preferred outright: an off-shift section is chosen
#'   only when no section is on shift, in which case the casualty queues until
#'   the next shift opens. Ties are broken by random selection among the
#'   equally-loaded candidates, matching select_r2e_team() above.
select_r2e_surg_section <- function(team_id) {
  sections <- env_data$elms$r2eheavy[[team_id]][["surg"]]

  section_load <- sapply(sections, function(members) {
    sum(sapply(members, function(res) {
      get_server_count(env, res) + get_queue_count(env, res)
    }))
  })

  on_shift <- sapply(sections, function(members) {
    all(sapply(members, function(res) get_capacity(env, res) > 0))
  })

  eligible   <- if (any(on_shift)) which(on_shift) else seq_along(sections)
  candidates <- eligible[section_load[eligible] == min(section_load[eligible])]

  if (length(candidates) == 1) return(candidates)
  return(sample(candidates, 1))
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

# ── Post-operative intensive care requirement ─────────────────────────────────
#
# Damage control is a staged sequence, and a casualty who goes through it needs
# intensive care at two separate points for two different reasons:
#
#   1. STABILISATION, between the abbreviated operation and the definitive one.
#      This is the classic damage control resuscitation phase: rewarming, and
#      correcting the coagulopathy and acidosis that would make definitive
#      repair unsurvivable. It is what the 24 to 36 hour window in the cited
#      literature refers to.
#   2. POST-DEFINITIVE care, after the final operation. Ventilation weaning,
#      organ support and monitoring for complications.
#
# The model draws each separately, because they answer to different things: a
# stabilisation episode can be delivered at either echelon and is what the
# forward-holding policy moves, whereas post-definitive care necessarily
# follows the definitive operation and so is always served at R2E, which is
# the only echelon that performs one. Keeping them apart is what lets the
# forward-holding lever move stabilisation forward without hollowing out the
# care that has to come afterwards.
#
# Both are theatre-level episodes. A casualty evacuated out of theatre
# continues critical care at Role 4, which this model treats as unconstrained
# demand rather than as a resource (README Further Development L16), so the
# post-definitive episode here is bounded by the deployed evacuation norm and
# not by a civilian intensive care length of stay.

#' Draws a casualty's whole stabilisation intensive care requirement, in minutes
#'
#' @return Minutes drawn from the R2E stabilisation-ICU triangular distribution
#'
#' @details How much stabilisation a casualty needs follows from the injury
#'   rather than from the facility that happens to hold them, so one draw
#'   covers the whole episode wherever it is served.
#'   r2b_stabilisation_minutes() and r2e_stabilisation_minutes() below divide
#'   that single draw between the echelons, which is what makes the total the
#'   same on every route by construction rather than by two parameters being
#'   kept consistent with each other. Each echelon records the minutes it
#'   served as an attribute (`r2b_post_op_min`, `r2e_post_op_min`) so the
#'   invariant is observable in a run's output rather than merely asserted
#'   here; scripts/check_icu_time_conservation.R checks it across all three
#'   routes a casualty requiring surgery can take.
draw_stabilisation_icu <- function() {
  rtriangle(
    n = 1,
    a = env_data$vars$r2eheavy$stabilisation_icu$min,
    b = env_data$vars$r2eheavy$stabilisation_icu$max,
    c = env_data$vars$r2eheavy$stabilisation_icu$mode
  )
}

#' Draws whether a casualty's operation is staged damage control or single-stage
#'
#' @return 1 for the damage control pathway, 0 for a single-stage definitive
#'   procedure, NA for a casualty who does not require surgery
#'
#' @details Damage control is indicated by physiology rather than by the
#'   presence of an injury: it is chosen for a casualty exhausted by
#'   hypothermia, coagulopathy and acidosis, who would not survive a prolonged
#'   definitive procedure. A casualty stable on the table receives their
#'   definitive repair in one operation and needs neither a stabilisation phase
#'   nor a return to theatre. The rate is keyed to triage priority, which is
#'   the model's only representation of physiological derangement.
#'
#'   Drawn once, where surgical candidacy itself is decided, so that both
#'   echelons read the same value: a casualty's physiology does not change
#'   because a forward theatre happened to be free.
#'
#'   A rate of exactly zero or one consumes no random number, since a
#'   degenerate Bernoulli trial has only one outcome. That keeps the run's
#'   random stream identical to a model without the split when every rate is
#'   set to one, which is what makes the all-damage-control configuration a
#'   reproducible special case of this one rather than merely a similar run.
draw_dcs_pathway <- function() {
  needs_surg <- get_attribute(env, "surgery")
  if (is.na(needs_surg) || needs_surg != 1) return(NA_real_)

  prio  <- get_attribute(env, "priority")
  other <- env_data$vars$r1$other
  rate  <- if (is.na(prio)) other$pri3_dcs_rate
           else if (prio == 1) other$pri1_dcs_rate
           else if (prio == 2) other$pri2_dcs_rate
           else other$pri3_dcs_rate

  if (is.null(rate) || is.na(rate)) return(1)
  if (rate >= 1) return(1)
  if (rate <= 0) return(0)
  as.numeric(runif(1) < rate)
}

#' Whether this casualty is on the single-stage surgical pathway
#'
#' @return TRUE when the casualty's operation is a single-stage definitive
#'   procedure, FALSE for damage control and for anyone not requiring surgery
single_stage <- function() {
  pathway <- get_attribute(env, "dcs_pathway")
  !is.na(pathway) && pathway == 0
}

#' Treatment efficacy multiplier earned by an operation, by surgical pathway
#'
#' @param abbreviated_factor The multiplier the staged pathway earns at this
#'   operation, which for damage control is an abbreviated procedure
#' @param definitive_factor The multiplier the staged pathway earns later, at
#'   the operation that completes its definitive repair
#' @return A closure returning the multiplier to apply to `dow_ceiling`
#'
#' @details Two casualties who both leave theatre with their definitive repair
#'   complete have reached the same clinical state, so the single-stage
#'   pathway earns at its one operation what the staged pathway earns across
#'   its two. What separates them is how long the staged casualty took to get
#'   there, and the model already prices that: dow_prob_conditional() charges
#'   elapsed time at every checkpoint, so the interval between an abbreviated
#'   operation and the definitive one carries its own mortality. Giving the
#'   single-stage operation only the abbreviated multiplier would instead
#'   leave a casualty who needed no staging at a higher residual ceiling than
#'   one who did, which inverts the indication.
definitive_efficacy <- function(abbreviated_factor, definitive_factor) {
  force(abbreviated_factor); force(definitive_factor)
  function() {
    if (single_stage()) return(abbreviated_factor * definitive_factor)
    abbreviated_factor
  }
}

#' Draws a casualty's post-definitive intensive care requirement, in minutes
#'
#' @return Minutes drawn from the R2E post-definitive-ICU triangular
#'   distribution
#'
#' @details Served at R2E after the casualty's final operation, on every route.
#'   Independent of the stabilisation draw: needing a long resuscitation phase
#'   before definitive repair does not imply a long recovery after it.
draw_post_definitive_icu <- function() {
  rtriangle(
    n = 1,
    a = env_data$vars$r2eheavy$post_definitive_icu$min,
    b = env_data$vars$r2eheavy$post_definitive_icu$max,
    c = env_data$vars$r2eheavy$post_definitive_icu$mode
  )
}

#' Minutes of the stabilisation requirement delivered forward at R2B
#'
#' @return The lesser of `r2b.post_op_icu.share` x the casualty's
#'   `stabilisation_total` attribute and `r2b.post_op_icu.forward_hold_max`
#'
#' @details Two levers, because a commander sets forward holding in two
#'   different terms. The share is the intent: how much of the stabilisation
#'   phase to attempt forward at all. The cap is the operational limit: how
#'   long a scarce forward intensive care bed may be tied up by one casualty
#'   before they are moved on regardless. The cap binds first, so setting it
#'   to zero disables forward holding whatever the share says, and setting it
#'   above the longest drawn requirement leaves the share acting alone.
r2b_stabilisation_minutes <- function() {
  intended <- get_attribute(env, "stabilisation_total") *
    env_data$vars$r2b$post_op_icu$share
  cap <- env_data$vars$r2b$post_op_icu$forward_hold_max
  if (is.null(cap) || is.na(cap)) return(intended)
  min(intended, cap)
}

#' Minutes of the stabilisation requirement remaining for R2E
#'
#' @return The casualty's `stabilisation_total`, less whatever was served
#'   forward. A casualty who was not operated on at R2B served nothing
#'   forward, whatever the share and cap are set to, so receives the whole
#'   requirement here — the same amount, on either route.
r2e_stabilisation_minutes <- function() {
  total <- get_attribute(env, "stabilisation_total")
  prior <- get_attribute(env, "r2b_surgery")
  if (!is.na(prior) && prior == 1) {
    return(max(0, total - r2b_stabilisation_minutes()))
  }
  total
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
          )
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

#' Simulates Role 2B road-move transport of KIA casualties to the mortuary,
#' modelled as collocated with Role 2E Heavy rather than Role 2B (Issue #73
#' follow-up) — R2B has no organic mortuary asset of its own. Uses the
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
    branch(
      option = function() get_attribute(env, "r2e"),
      continue = TRUE,
      lapply(1:length(env_data$elms$r2eheavy), r2e_mortuary_intake)
    )
}

#' Seizes an already-available R2B team evac resource, models the outbound
#' transport draw, then a dead-heading return leg on that same resource
#' before releasing it (Issue #73 follow-up). The casualty continues
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
#' evac resource that the caller has already seized (Issue #73 follow-up).
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
#' #     - OT bed free, no queue, team on shift → seize OT + team, perform
#' #         surgery, then, on the damage control pathway only, post-operative
#' #         stabilisation for the share of the ICU requirement served forward
#' #         (r2b.post_op_icu.share; ICU bed, or holding bed at elevated risk
#' #         when ICU is saturated). A single-stage casualty's operation is
#' #         their definitive repair, so no stabilisation phase follows it here
#' #     - OT bed free, no queue, team off shift but reopening within
#' #         r2b.surgery.pre_open_window_min → hold forward for the section and
#' #         then operate as above (r2b_pre_open_wait = 1;
#' #         r2b_pre_open_wait_min = realised hold in minutes)
#' #     - OT full, OR queued, OR team off-shift for longer than the window →
#' #         bypass immediately to R2E
#' #         (r2b_bypassed = 1; r2b_bypass_reason = 1 team off-shift, 2 OT busy/queued;
#' #          r2b_bypass_time = simulation time of the bypass decision)
#' # - surgery != 1 → hold bed recovery, set return_day, leave trajectory
#'
#' # Step 5: Evacuation decision branch
#' # Branches based on evacuation team availability:
#' # - evac available     → immediate transfer to R2E (r2b_to_r2e = 1)
#' # - evac not available → wait in ICU bed until evac is free
#' # R2B → R2E WIA movement seizes each R2B team's own `evac` resource, not
#' # the shared PMVAmb fleet — a deliberate design (Issue #73): this leg
#' # represents an organic R2B unit asset, distinct from the brigade-pooled
#' # transport fleet used for R1 → R2B. It does model a dead-heading return
#' # leg on that same organic resource (Issue #73 follow-up): once the R2B
#' # team's evac asset drops a casualty at R2E, it is unavailable to its own
#' # team until it completes the return trip.
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
    r2b_evac_return_leg(evacuation_team) %>%
    branch(
      option = function() get_attribute(env, "r2e"),
      continue = TRUE,
      lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
    ) %>%
    leave(1)

  # Forward stabilisation, joined at the end of the Surgery Path below. This
  # is the damage control resuscitation phase, which belongs between the
  # abbreviated operation just performed here and the definitive one waiting
  # at R2E — see the post-operative intensive care requirement block at the
  # top of this file for why the two episodes are modelled separately.
  # `r2b.post_op_icu.share` and `r2b.post_op_icu.forward_hold_max` set how
  # much of it is served here, the remainder falling to R2E, where it is
  # served before the definitive operation rather than after it.
  #
  # A single-stage casualty has no stabilisation phase at all: their operation
  # was their definitive repair, so there is no interval between two
  # procedures for one to occupy. They take neither the draw nor a bed here,
  # and their post-operative intensive care is the post-definitive episode
  # served at R2E.
  #
  # Branches on the surgical pathway, then on the forward minutes, then on R2B
  # ICU availability:
  # - single-stage               → no stabilisation phase exists
  # - nothing to serve forward   → no forward stay (r2b_post_op_pathway unset);
  #                                the whole requirement is served at R2E
  # - ICU bed free               → ICU bed for the forward minutes
  #                                (r2b_post_op_pathway = 1)
  # - ICU saturated              → holding bed for the same duration, at a
  #                                further elevated dow_ceiling
  #                                (r2b_post_op_pathway = 2), mirroring the
  #                                R2E post-op hold pathway
  #
  # Either forward path multiplies dow_ceiling by r2b_icu_penalty: an R2B ICU
  # section fields two nurses and two medics and no intensivist, against R2E's
  # intensivist-led section, and the penalty is the mortality cost of that
  # difference. Without it, forward holding would be free in the model and any
  # sweep of the share would recommend its maximum as an artefact of relieving
  # the R2E queue at no cost. See README "Died of Wounds — Treatment Efficacy
  # Modifiers".
  #
  # The stay lengthens the casualty's journey but opens no DOW checkpoint of
  # its own: dow_prob_conditional() prices elapsed time at the next checkpoint
  # the casualty reaches (the R2E arrival check), so the delay is charged
  # there, against the ceiling this trajectory has already raised.
  r2b_post_op_stabilisation <- trajectory("R2B Post-Operative Stabilisation") %>%
    branch(
      # The draw itself sits inside the branch, not before it, so a
      # single-stage casualty consumes no requirement they will never serve.
      option = function() if (single_stage()) 2 else 1,
      continue = TRUE,

      trajectory("R2B Damage Control Stabilisation") %>%
        set_attribute("stabilisation_total", function() draw_stabilisation_icu()) %>%
        branch(
          option = function() {
            if (r2b_stabilisation_minutes() <= 0) return(2)
            return(1)
          },
          continue = TRUE,

          trajectory("R2B Forward Stabilisation") %>%
            set_attribute("dow_ceiling", function() {
              ceiling <- get_attribute(env, "dow_ceiling")
              if (is.na(ceiling)) return(ceiling)
              ceiling * env_data$vars$dow$treatment_efficacy$r2b_icu_penalty
            }) %>%
            branch(
              option = function() {
                usage <- sum(get_server_count(env, resources = icu_beds))
                cap   <- sum(get_capacity(env, resources = icu_beds))
                if (!is.na(usage) && !is.na(cap) && usage < cap) return(1)
                return(2)
              },
              continue = TRUE,

              # ICU bed available — the nominal forward pathway
              trajectory("R2B Post-Op ICU") %>%
                set_attribute("r2b_post_op_pathway", 1) %>%
                set_attribute("r2b_post_op_min", function() r2b_stabilisation_minutes()) %>%
                simmer::select(icu_beds, policy = "shortest-queue", id = 6) %>%
                seize_selected(id = 6) %>%
                timeout(function() r2b_stabilisation_minutes()) %>%
                release_selected(id = 6),

              # ICU saturated — the same stay in a holding bed, at the same
              # elevated risk the R2E post-op hold pathway carries for the same
              # reason (reduced post-operative monitoring). The duration is the
              # casualty's requirement either way: the bed changes what the stay
              # is worth clinically, not how long they need it for.
              trajectory("R2B Post-Op Hold — ICU Full") %>%
                set_attribute("r2b_post_op_pathway", 2) %>%
                set_attribute("r2b_post_op_min", function() r2b_stabilisation_minutes()) %>%
                set_attribute("dow_ceiling", function() {
                  ceiling <- get_attribute(env, "dow_ceiling")
                  if (is.na(ceiling)) return(ceiling)
                  ceiling * env_data$vars$dow$treatment_efficacy$r2e_postop_hold_penalty
                }) %>%
                simmer::select(hold_beds, policy = "shortest-queue", id = 7) %>%
                seize_selected(id = 7) %>%
                timeout(function() r2b_stabilisation_minutes()) %>%
                release_selected(id = 7)
            ),

          trajectory("No Forward Stabilisation")
        ),

      trajectory("Single-Stage — No Stabilisation Phase")
    )

  # Forward surgery itself, from theatre seizure through to the stabilisation
  # phase. Built rather than written once because it is reached by two routes,
  # a casualty whose theatre and section are both free on arrival and one held
  # briefly for a section about to reopen, and only the second records how
  # long it waited.
  #
  # @param pre_open Whether this copy is the held route
  # @return A simmer trajectory performing one forward operation
  build_r2b_surgery_path <- function(pre_open) {
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
      set_attribute("dow_ceiling", function() {
        ceiling <- get_attribute(env, "dow_ceiling")
        if (is.na(ceiling)) return(ceiling)
        te <- env_data$vars$dow$treatment_efficacy
        ceiling * definitive_efficacy(te$r2b_dcs_factor, te$r2e_dcs1_factor)()
      }) %>%
      join(r2b_post_op_stabilisation)
  }

  # OT availability check. Joined directly when the pre-OT ICU gate above
  # clears immediately, and again after the P2+ ICU-defer wait loop resolves.
  r2b_ot_check_path <- trajectory("R2B OT Check") %>%
    branch(
      option = function() {
        usage    <- sum(get_server_count(env, resources = ot_beds))
        cap      <- sum(get_capacity(env, resources = ot_beds))
        queue    <- sum(get_queue_count(env, resources = ot_beds))
        team_cap <- sum(get_capacity(env, resources = surg_team))
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
      build_r2b_surgery_path(pre_open = FALSE),

      # Sub-branch 2: OT busy, queued, or the section closed for longer than
      # the pre-open window — bypass to R2E. r2b_bypass_reason decomposes the
      # cause: 1 = surgical section off shift (team_cap <= 0), 2 = OT bed busy
      # or queued. Re-reads the same resource state as the option() check
      # above — no timeout intervenes between the branch decision and this
      # sub-trajectory, so state cannot have changed.
      trajectory("OT Unavailable – Bypass to R2E") %>%
        set_attribute("r2b_bypassed", 1) %>%
        set_attribute("r2b_bypass_reason", function() {
          team_cap <- sum(get_capacity(env, resources = surg_team))
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
      build_r2b_surgery_path(pre_open = TRUE)
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
      # Pre-OT ICU availability gate (Issue #43), mirroring the R2E pattern.
      # Priority 1 casualties proceed unconditionally; Priority 2+ casualties
      # defer OT entry while this unit's ICU is fully saturated, preserving
      # ICU headroom for those already in it. How much work the gate does
      # depends on `r2b.post_op_icu.share`: at zero the two ICU beds per team
      # serve only the wait_for_evac fallback and the gate is close to inert,
      # while at a non-zero share every casualty operated on here also
      # recovers here (r2b_post_op_stabilisation above), and the gate becomes
      # a real constraint on forward surgical throughput.
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
                r2b_evac_leg(evacuation_team) %>%
                branch(
                  option = function() get_attribute(env, "r2e"),
                  continue = TRUE,
                  lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
                ) %>%
                simmer::leave(1),
              trajectory("R2B Hold RTD") %>%
                set_attribute("return_day", function() now(env)) %>%
                set_attribute("return_echelon", 2) %>%
                credit_rtd() %>%
                release_selected(id = 5) %>%
                simmer::leave(1)
            ),
          # Branch 2b: R2B full, R2E has capacity (or queue cap exceeded) — bypass
          trajectory("R2B Hold Full — Bypass to R2E") %>%
            set_attribute("r2b_hold_bypass", 1) %>%
            set_attribute("r2b_to_r2e", 1) %>%
            set_attribute("r2e", function() select_r2e_team()) %>%
            r2b_evac_leg(evacuation_team) %>%
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
                r2b_evac_leg(evacuation_team) %>%
                branch(
                  option = function() get_attribute(env, "r2e"),
                  continue = TRUE,
                  lapply(1:length(env_data$elms$r2eheavy), r2e_treat_wia)
                ) %>%
                simmer::leave(1),
              trajectory("R2B Hold Queue RTD") %>%
                set_attribute("return_day", function() now(env)) %>%
                set_attribute("return_echelon", 2) %>%
                credit_rtd() %>%
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
        r2b_evac_leg(evacuation_team) %>%
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

#' Builds a per-R2E-team mortuary intake sub-trajectory for KIA casualties
#' arriving by road from R2B (Issue #73 follow-up: the mortuary is modelled
#' as collocated with R2E, not R2B). One small trajectory is built per R2E
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
#' #                                   for whatever remains of the post-operative
#' #                                   ICU requirement after any share served
#' #                                   forward at R2B (r2e_post_op_icu_minutes())
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
#' #   The stabilisation episode itself is served only on the damage control
#' #   pathway; a single-stage casualty passes straight to the DOW check.
#' # - surgery == 1, single-stage, already operated on at R2B → no theatre here,
#' #   their definitive repair having been performed forward; the post-operative
#' #   DOW check alone
#' # - surgery != 1 → no surgery needed
#'
#' # Phase 4: Second surgery (damage control pathway only, and only if R2E
#' # Phase 3 surgery occurred without prior R2B DAMCON)
#' # Branches based on attributes "dcs_pathway", "r2e_surgery" and "r2b_surgery":
#' # - dcs_pathway == 1 AND r2e_surgery == 1 AND r2b_surgery != 1 → second surgery
#' # - else (single-stage, not a surgical candidate, or had R2B DAMCON) → skip
#'
#' # Phase 5: Final disposition — theatre evacuation policy
#' # Draws recovery_to_duty_days (draw_recovery_to_duty(), severity-keyed)
#' # and branches on it against recovery$evacuation_policy_days:
#' # - recovery within policy → recover in theatre: seize hold bed for the
#' #   drawn duration, set return_day
#' # - recovery beyond policy → strategic evac: set r2e_evac = 1,
#' #   evacuation_decision_day, treatment_received; route by acuity to one of
#' #   two AME pools sharing a single sortie schedule
#' #   (build_ame_sortie_trajectory() below): Priority 1 surgical evacuees
#' #   queue on the smaller "ame_critical" (CCATT/CCAST-supported) pool,
#' #   everyone else on the standard "ame" pool. Both stage in a Hold bed
#' #   (Casualty Staging Unit-equivalent); the ventilated share of the
#' #   critical pool first holds an ICU bed for a bounded pre-flight period
#' #   (critical_pre_flight_care()). Release the bed only once actually
#' #   evacuated, setting ame_departure_time, evacuation_day,
#' #   ame_wait_minutes (Issue #23 follow-up — casualties consume R2E beds
#' #   until strategic AME is actually available, not merely decided upon)
r2e_treat_wia <- function(team_id) {
  hold_beds       <- env_data$elms$r2eheavy[[team_id]][["hold_bed"]]
  resus_beds      <- env_data$elms$r2eheavy[[team_id]][["resus_bed"]]
  ot_beds         <- env_data$elms$r2eheavy[[team_id]][["ot_bed"]]
  icu_beds        <- env_data$elms$r2eheavy[[team_id]][["icu_bed"]]
  surg_teams      <- env_data$elms$r2eheavy[[team_id]][["surg"]]

  emergency_team <- select_subteam("r2eheavy", team_id, "emerg")
  evac_team      <- select_subteam("r2eheavy", team_id, "evac")
  icu_team       <- select_subteam("r2eheavy", team_id, "icu")

  # Surgical sections are seized per casualty, not fixed at build time as the
  # emergency, evacuation and ICU sections above are, because R2E fields three
  # of them against two operating theatres and each carries its own shift.
  # build_r2e_surgery_block() below constructs one seize-operate-release block
  # per section; the caller branches over them on select_r2e_surg_section().
  #
  # @param section_id Index of the surgical section within surg_teams
  # @param select_id  simmer selection id used for the OT bed in this block
  # @param start_attr Attribute name recording incision time
  # @param end_attr   Attribute name recording closure time
  # @param efficacy   Zero-argument function returning the dow_ceiling
  #                   multiplier to apply on completion, so the multiplier can
  #                   depend on the casualty's surgical pathway
  # @param set_flag   Whether to set the r2e_surgery marker attribute
  # @return A simmer trajectory performing one operation
  #
  # Seizure order is bed then team, released team then bed, matching
  # r2b_ot_check_path() so the two echelons cannot deadlock against each
  # other's ordering. A section already mid-procedure when its shift closes
  # retains the resources it holds until release, so a shift change cannot
  # interrupt an operation in progress.
  build_r2e_surgery_block <- function(section_id, select_id, start_attr,
                                      end_attr, efficacy, set_flag) {
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

  # Stabilisation at R2E, for whatever the forward echelon did not serve.
  # Where it sits in the sequence depends on which operation this casualty's
  # definitive repair is:
  #
  #  - Operated at R2B already: the R2E procedure IS their definitive repair,
  #    so any remaining stabilisation must come BEFORE it. Joined at
  #    r2e_pre_surgery_stabilisation below.
  #  - Not operated at R2B: the R2E procedure is their abbreviated operation
  #    and the Phase 4 procedure is the definitive one, so stabilisation sits
  #    BETWEEN them, which is where this trajectory is joined.
  #
  # Splitting it this way is what puts the resuscitation phase between the
  # two operations on both routes, rather than after both of them on one.
  # A single-stage casualty has no interval between two operations and so no
  # stabilisation phase; they pass straight to the post-operative DOW check.
  r2e_stabilisation_recovery <- trajectory("R2E Stabilisation") %>%
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
    join(r2e_post_op_dow_check)

  # Remaining stabilisation for a casualty operated on at R2B, served before
  # their definitive repair. Nothing to do when the forward echelon served
  # the whole requirement, which is what a forward share of one produces.
  r2e_pre_surgery_stabilisation <- trajectory("R2E Pre-Definitive Stabilisation") %>%
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
  #
  # This is the degraded form of the stabilisation phase, so it applies only to
  # the damage control pathway. A single-stage casualty has no stabilisation
  # phase to degrade; their post-operative intensive care is the
  # post-definitive episode, which carries its own holding-bed fallback.
  r2e_hold_recovery <- trajectory("R2E Post-Op Hold Recovery") %>%
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
        timeout(function() {
          rtriangle(
            n = 1,
            a = env_data$vars$r2eheavy$post_op_hold$min,
            b = env_data$vars$r2eheavy$post_op_hold$max,
            c = env_data$vars$r2eheavy$post_op_hold$mode
          )
        }) %>%
        release_selected(id = 8),

      trajectory("Single-Stage — No Stabilisation Phase to Degrade")
    ) %>%
    join(r2e_post_op_dow_check)

  # Shared surgery portion (OT seizure through the operation itself). Recovery
  # (ICU vs post-op hold) is decided upstream at the pre-OT gating branch
  # and joined on afterwards, so this portion is identical for both paths.
  # Branch structure: one sub-trajectory per R2E surgical section, selected on
  # entry by select_r2e_surg_section(). Every branch performs the same
  # procedure and differs only in which section's resources it seizes, so the
  # choice affects contention and shift availability, not clinical outcome.
  r2e_ot_surgery <- trajectory("R2E OT — Surgery") %>%
    branch(
      option = function() select_r2e_surg_section(team_id),
      continue = TRUE,
      lapply(seq_along(surg_teams), function(section_id) {
        build_r2e_surgery_block(
          section_id, 4, "r2e_surgery_1_start", "r2e_surgery_1_end",
          definitive_efficacy(
            env_data$vars$dow$treatment_efficacy$r2e_dcs1_factor,
            env_data$vars$dow$treatment_efficacy$r2e_dcs2_factor
          ), TRUE
        )
      })
    )

  r2e_surgery_icu_path <- trajectory("R2E Surgery — ICU Available") %>%
    join(r2e_pre_surgery_stabilisation) %>%
    join(r2e_ot_surgery) %>%
    join(r2e_stabilisation_recovery)

  r2e_surgery_hold_path <- trajectory("R2E Surgery — ICU Full, P1 to Post-Op Hold") %>%
    join(r2e_ot_surgery) %>%
    join(r2e_hold_recovery)

  # Post-definitive intensive care, joined after Phase 4 for every casualty
  # who had an operation, on either route. This is the episode that follows
  # the definitive repair — ventilation weaning, organ support, watching for
  # complications — as distinct from the stabilisation phase that precedes
  # it. Guideline-recommended standard after major trauma surgery; see README
  # "Died of Wounds — Post-Operative Checkpoint" for the citation.
  #
  # Unlike stabilisation, this episode is never served forward: R2B performs
  # no definitive repair, so there is nothing for it to follow there. That is
  # what stops the forward-holding lever from emptying out post-definitive
  # care as the share rises.
  #
  # Takes the same degraded-care fallback as the post-operative hold pathway
  # above, for the same reason: a casualty who has already been operated on
  # cannot be made to wait indefinitely for a bed, so when intensive care is
  # saturated they recover in a holding bed at an elevated dow_ceiling. The
  # pathway taken is recorded in post_definitive_pathway (1 = ICU, 2 = hold).
  r2e_post_definitive_care <- trajectory("R2E Post-Definitive Care") %>%
    branch(
      option = function() {
        had_surgery <- get_attribute(env, "surgery")
        if (is.na(had_surgery) || had_surgery != 1) return(3)
        usage <- sum(get_server_count(env, resources = icu_beds))
        cap   <- sum(get_capacity(env, resources = icu_beds))
        if (!is.na(usage) && !is.na(cap) && usage < cap) return(1)
        return(2)
      },
      continue = TRUE,

      trajectory("R2E Post-Definitive ICU") %>%
        set_attribute("post_definitive_pathway", 1) %>%
        set_attribute("post_definitive_min", function() draw_post_definitive_icu()) %>%
        simmer::select(icu_beds, policy = "shortest-queue", id = 6) %>%
        seize_selected(id = 6) %>%
        timeout(function() get_attribute(env, "post_definitive_min")) %>%
        release_selected(id = 6),

      trajectory("R2E Post-Definitive Hold — ICU Full") %>%
        set_attribute("post_definitive_pathway", 2) %>%
        set_attribute("post_definitive_min", function() draw_post_definitive_icu()) %>%
        set_attribute("dow_ceiling", function() {
          ceiling <- get_attribute(env, "dow_ceiling")
          if (is.na(ceiling)) return(ceiling)
          ceiling * env_data$vars$dow$treatment_efficacy$r2e_postop_hold_penalty
        }) %>%
        simmer::select(hold_beds, policy = "shortest-queue", id = 8) %>%
        seize_selected(id = 8) %>%
        timeout(function() get_attribute(env, "post_definitive_min")) %>%
        release_selected(id = 8),

      trajectory("No Operation, No Post-Definitive Care")
    )

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

  # Strategic-Evac AME-wait DOW poll (Issue #23 third follow-up): mirrors
  # r2e_post_op_dow_check's conditional-increment DOW roll (same
  # dow_prob_conditional() formula, same priority-based parameters and
  # dow_ceiling), but polled on an interval (role4.ame.dow_check_interval)
  # while queued for "ame"/"ame_critical" rather than checked once at a
  # fixed transition point — the AME wait itself is unbounded, and unlike
  # every earlier checkpoint (which precedes a further step that would
  # otherwise price in delay-accrued risk), this is the last checkpoint in
  # the casualty's journey: without this poll, a casualty who reached
  # Strategic Evac faced zero further mortality risk regardless of how
  # long the subsequent AME wait was. See README Died of Wounds — AME Wait
  # Checkpoint.
  #
  # Availability is checked immediately on entry — if capacity is already
  # free, boarding proceeds with no artificial minimum wait, matching a
  # plain seize()'s fast path exactly. Only if unavailable does the
  # casualty enter the poll loop: wait dow_check_interval minutes, roll
  # DOW, then (if surviving) re-check availability via rollback()'s
  # `check` and loop again if still unavailable — the same
  # timeout-then-rollback polling pattern used for R2E OT-ICU gating
  # deferral (icu_gating$defer_check_interval, above). dow_echelon = 5
  # distinguishes this checkpoint from the R2E post-operative check
  # (dow_echelon = 4).
  ame_dow_poll <- function(resource_name, bed_id) {
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

  # Expected recovery-to-duty duration, in days, drawn at the end of R2E
  # clinical care. A theatre evacuation policy is a duration threshold — "a
  # theater that evacuates out of the theater all patients requiring 30 or
  # more days of hospitalization is said to have a '30-day evacuation
  # policy'" [[55]] — so disposition needs a per-casualty clinical duration
  # to compare against the policy rather than a fixed share of admissions.
  # The draw is the shared base convalescence distribution
  # (r2eheavy$holding) scaled by a severity factor keyed to the same four
  # categories R/analysis.R::assign_role4_los() uses, so a casualty's
  # prognosis, its Role 4 ward and its AME route all follow from one
  # severity classification rather than from independent draws.
  draw_recovery_to_duty <- function() {
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
    (base * severity) / 1440
  }

  # Pre-flight critical care (Issue #156). A ventilated casualty awaiting a
  # critical-care sortie genuinely needs ICU-level care, but a deployed ICU
  # study at Camp Bastion records that coalition soldiers "are usually
  # evacuated within 24 h of admission" [[56]], so that need is bounded
  # rather than lasting the whole evacuation wait. A configurable share of
  # the critical pool (critical_hold$ventilated_share) holds an ICU bed for
  # a critical_hold-distributed period and then steps down to a Casualty
  # Staging Unit hold bed; the rest stage in a hold bed immediately.
  #
  # The step-down seizes the hold bed before releasing the ICU bed, so a
  # casualty is never discharged from intensive care to nowhere. The cost is
  # that a full hold pool blocks the ICU bed for as long as the casualty
  # queues, which ame_icu_hold_minutes measures. This is bed-blocking, not
  # deadlock: no trajectory in the model holds a hold bed while waiting on
  # ICU, so the two acquisition orders cannot form a cycle.
  critical_pre_flight_care <- trajectory("Pre-Flight Critical Care") %>%
    branch(
      option = function() {
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
        seize_selected(id = 9)
    )

  ame_wait_and_board <- function(resource_name, bed_id) {
    trajectory("Awaiting AME") %>%
      branch(
        option = function() {
          usage <- get_server_count(env, resource_name)
          cap   <- get_capacity(env, resource_name)
          if (!is.na(usage) && !is.na(cap) && usage < cap) return(2)
          return(1)
        },
        continue = TRUE,
        ame_dow_poll(resource_name, bed_id),
        trajectory("Available Immediately")
      ) %>%
      seize(resource_name, 1)
  }

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
      trajectory("No Surgery Needed"),
      trajectory("Definitive Repair Already Performed at R2B") %>%
        join(r2e_post_op_dow_check)
    ) %>%

    # Phase 4: Second surgery, for a damage control casualty whose abbreviated
    # operation was the R2E Phase 3 one. A second procedure is only meaningful
    # for patients who underwent Phase 3 surgery at R2E (r2e_surgery == 1)
    # without a prior R2B DAMCON (r2b_surgery != 1), and only on the damage
    # control pathway: a single-stage casualty's Phase 3 procedure was already
    # their definitive repair, so there is nothing to return to theatre for.
    # Patients with surgery == 0 never set r2e_surgery, so is.na(r2e_surg) guards them out.
    branch(
      option = function() {
        if (single_stage()) return(2)
        r2e_surg   <- get_attribute(env, "r2e_surgery")
        prior_surg <- get_attribute(env, "r2b_surgery")
        if (!is.na(r2e_surg) && r2e_surg == 1 &&
            (is.na(prior_surg) || prior_surg != 1)) return(1)
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
            build_r2e_surgery_block(
              section_id, 7, "r2e_surgery_2_start", "r2e_surgery_2_end",
              local({
                factor <- env_data$vars$dow$treatment_efficacy$r2e_dcs2_factor
                function() factor
              }), FALSE
            )
          })
        ),
      trajectory("No Second Surgery Needed")
    ) %>%

    # Post-definitive intensive care, after whichever operation was this
    # casualty's definitive repair — the Phase 4 procedure for a casualty
    # operated on only at R2E, the Phase 3 one for a casualty who had their
    # abbreviated operation at R2B.
    join(r2e_post_definitive_care) %>%

    # Phase 5: Final disposition — theatre evacuation policy
    # recovery_to_duty_days is drawn first (draw_recovery_to_duty(), above)
    # and the branch compares it against recovery$evacuation_policy_days:
    # - expected recovery within the policy → retain in theatre: seize hold
    #   bed for that drawn duration, log return_day
    # - expected recovery beyond the policy → strategic evac: set r2e_evac = 1,
    #   evacuation_decision_day, treatment_received; route by acuity to one of
    #   two AME pools sharing a single sortie schedule — Priority 1 surgical
    #   evacuees queue on "ame_critical", everyone else on the standard "ame"
    #   pool; both stage in a Hold bed, released only once actually evacuated,
    #   setting ame_departure_time, evacuation_day, ame_wait_minutes
    #   (Issue #23 follow-up). Casualties face a periodic DOW poll while
    #   queued (Issue #23 third follow-up, ame_dow_poll() above).
    set_attribute("recovery_to_duty_days", draw_recovery_to_duty) %>%
    branch(
      option = function() {
        rtd    <- get_attribute(env, "recovery_to_duty_days")
        policy <- env_data$vars$r2eheavy$recovery$evacuation_policy_days
        if (!is.na(rtd) && rtd <= policy) return(1)
        return(2)
      },
      continue = TRUE,
      trajectory("Recover at R2E") %>%
        simmer::select(hold_beds, policy = "shortest-queue", id = 5) %>%
        seize_selected(id = 5) %>%
        # The hold bed is held for the same duration the disposition was
        # decided on, so a retained casualty's bed-days and the prognosis
        # that retained them cannot disagree.
        timeout(function() get_attribute(env, "recovery_to_duty_days") * 1440) %>%
        release_selected(id = 5) %>%
        set_attribute("r2e_departure_time", function() now(env)) %>%
        set_attribute("return_day", function() now(env)) %>%
        set_attribute("return_echelon", 3) %>%
        credit_rtd(),
      trajectory("Strategic Evac — Awaiting AME") %>%
        # r2e_departure_time keeps its original meaning — clinical care
        # concluded, disposition decided — so the existing R2E Dwell Time
        # KPI (Domain 2) is unaffected by this branch. AME wait is tracked
        # separately below (ame_departure_time, ame_wait_minutes) rather
        # than folded into r2e_departure_time, so "clinical dwell" and
        # "evacuation logistics wait" remain distinguishable.
        set_attribute("r2e_departure_time", function() now(env)) %>%
        set_attribute("r2e_evac", 1) %>%
        # evacuation_decision_day / treatment_received (Issue #23): captured
        # when the Strategic Evac disposition is decided. Feeds the Role 4
        # ward/LoS category assignment (R/analysis.R::assign_role4_los())
        # — see README Role 4 sub-section.
        set_attribute("evacuation_decision_day", function() floor(now(env) / 1440) + 1) %>%
        set_attribute("treatment_received", function() {
          r2b_surg <- get_attribute(env, "r2b_surgery")
          r2e_surg <- get_attribute(env, "r2e_surgery")
          had_surgery <- (!is.na(r2b_surg) && r2b_surg == 1) ||
            (!is.na(r2e_surg) && r2e_surg == 1)
          if (had_surgery) 1 else 0
        }) %>%

        # Awaiting-AME routing (Issue #23 follow-up, revised per AJP-4.10(B)
        # [[21]]): a Casualty Staging Unit "collocate[s] already stabilized
        # patients" pending transport — every casualty reaching this branch
        # has, by construction, already completed R2E's post-operative
        # ICU/Hold recovery timeout, so staging is in a Hold bed on both
        # routes. Priority 1 surgical evacuees (the same population assigned
        # the Role 4 ICU ward — R/analysis.R::assign_role4_los()) queue on
        # the smaller "ame_critical" pool instead — a critical care air
        # transport team (CCATT) or critical care aeromedical evacuation
        # support team (CCAST) "augment[ing] the standard aeromedical
        # evacuation crew" on the same sortie (see build_ame_sortie_
        # trajectory(), below), "limited by capacity" per AJP-4.10(B). The
        # critical/standard split is therefore a distinction in airlift seat
        # type, not in bed type.
        # ame_route: 1 = critical (ame_critical pool), 2 = standard (ame
        # pool) — read by R/analysis.R for the route-decomposed wait-time/
        # backlog outputs.
        branch(
          option = function() {
            prio <- get_attribute(env, "priority")
            tx   <- get_attribute(env, "treatment_received")
            if (!is.na(prio) && prio == 1 && !is.na(tx) && tx == 1) return(1)
            return(2)
          },
          continue = TRUE,
          trajectory("Await Critical AME") %>%
            set_attribute("ame_route", 1) %>%
            join(critical_pre_flight_care) %>%
            # The AME pool seat is never released — a boarded casualty
            # permanently consumes that sortie's capacity; no seats are
            # handed back. Casualties board strictly in queue (decision)
            # order — no further acuity-based boarding priority beyond the
            # critical/standard split itself is modelled; see README
            # Further Development.
            join(ame_wait_and_board("ame_critical", 9)),
          trajectory("Await Standard AME — Hold Bed") %>%
            set_attribute("ame_route", 2) %>%
            simmer::select(hold_beds, policy = "shortest-queue", id = 9) %>%
            seize_selected(id = 9) %>%
            join(ame_wait_and_board("ame", 9))
        ) %>%
        release_selected(id = 9) %>%
        set_attribute("ame_departure_time", function() now(env)) %>%
        set_attribute("evacuation_day", function() floor(now(env) / 1440) + 1) %>%
        set_attribute("ame_wait_minutes", function() {
          now(env) - get_attribute(env, "r2e_departure_time")
        })
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
#' # - Sets injury_type (Issue #23): 1=WIA, 2=DNBI, 3=KIA, from casualty name
#' #   prefix; read by the Role 4 census (R/analysis.R) at strategic evac
#' # - Sets mass_casualty_event_id (Issue #9): the 1-indexed mass casualty
#' #   event this casualty originated from (R/environment.R::
#' #   generate_mass_casualty_events()), looked up via the entity's
#' #   generator-assigned index into wia_cbt_mass_casualty_event_id, or
#' #   kia_cbt_mass_casualty_event_id for an event's immediate killed
#' #   (Issue #149); 0 for background-generated casualties
#' # - Sets mass_casualty_event: 1 if mass_casualty_event_id > 0, else 0
#' # - Sets priority (WIA/DNBI) via weighted sample — mass-casualty-tagged
#' #   casualties draw from that event's own priority split in "scheduled"
#' #   mode (mass_casualty_event_priority_table), or the shared blast-dominant
#' #   mass_casualty priority distribution in "poisson" mode, instead of the
#' #   standard r1 priority distribution
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
    debit_force_size() %>%
    set_attribute("last_dow_t",  function() now(env)) %>%
    # injury_type (Issue #23): 1 = WIA, 2 = DNBI, 3 = KIA. Read downstream by
    # the Role 4 census (R/analysis.R::assign_role4_los()) to route DNBI
    # strategic evacuees to the P3/DNBI length-of-stay category regardless of
    # their triage priority — see README Role 4 sub-section.
    set_attribute("injury_type", function() {
      name <- get_name(env)
      if (startsWith(name, "wia"))  return(1L)
      if (startsWith(name, "dnbi")) return(2L)
      3L
    }) %>%
    set_attribute("mass_casualty_event_id", function() {
      name <- get_name(env)
      # Two streams carry a mass casualty overlay, the combat wounded and
      # the combat killed, each with its own sink built in emission order
      # by wrap_with_mass_casualty() (R/environment.R).
      sink <- if (startsWith(name, "wia_cbt")) {
        wia_cbt_mass_casualty_event_id
      } else if (startsWith(name, "kia_cbt")) {
        kia_cbt_mass_casualty_event_id
      } else {
        return(0L)
      }
      idx <- as.integer(sub("^[a-z]+_cbt", "", name)) + 1L
      if (idx >= 1L && idx <= length(sink)) sink[idx] else 0L
    }) %>%
    set_attribute("mass_casualty_event", function() {
      if (get_attribute(env, "mass_casualty_event_id") > 0) 1 else 0
    }) %>%
    set_attribute("priority", function() {
      if (startsWith(get_name(env), "wia") || startsWith(get_name(env), "dnbi")) {
        if (get_attribute(env, "mass_casualty_event") == 1) {
          eid    <- get_attribute(env, "mass_casualty_event_id")
          ev_row <- mass_casualty_event_priority_table[mass_casualty_event_priority_table$event_id == eid, ]
          # Per-event priority (scheduled mode) if the event's own row has
          # one; poisson-mode events carry NA pri_one, falling back to the
          # shared mass_casualty priority split.
          prob <- if (nrow(ev_row) == 1 && !is.na(ev_row$pri_one[1])) {
            c(ev_row$pri_one[1], ev_row$pri_two[1], ev_row$pri_three[1])
          } else {
            c(env_data$vars$mass_casualty$priority$one,
              env_data$vars$mass_casualty$priority$two,
              env_data$vars$mass_casualty$priority$three)
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
            credit_rtd() %>%
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
                set_attribute("return_echelon", 1) %>%
                credit_rtd()
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
                    set_attribute("return_echelon", 1) %>%
                    credit_rtd()
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

# ── Force reinforcement (Issue #18) ─────────────────────────────────────────────

#' Builds the reinforcement demand/fulfillment trajectory
#'
#' @return A simmer trajectory representing one reinforcement demand cycle:
#'   at each firing (a "submission"), computes each pool's shortfall against
#'   its initial establishment strength, draws a fill fraction of that
#'   shortfall from a triangular distribution, waits
#'   fulfillment_lag_days, then credits the drawn amount to the pool.
#'
#' @details Driven by its own generator in run_once() (R/replication.R),
#'   scheduled every demand_interval_days (env_data$vars$force_regeneration$
#'   reinforcement) — this is the one genuinely periodic/daily-cycle
#'   mechanism in the Issue #18 force regeneration model (injury/RTD
#'   crediting is continuous — see debit_force_size()/credit_rtd() above).
#'
#'   Demand is assessed at submission time (shortfall = initial - current
#'   effective force, floored at 0 — a pool already at or above full
#'   strength submits no demand and fills nothing). The actual fill amount
#'   is also drawn at submission time (rather than at fulfillment), modelling
#'   the outcome of a reinforcement request being substantially known when
#'   submitted, with delivery simply delayed — not a second, independent
#'   source of uncertainty layered on top of delivery timing.
#'
#'   fill_min_frac/fill_mode_frac/fill_max_frac parameterise a
#'   Triangular(min, mode, max) distribution over the fraction of demand
#'   actually delivered: mode_frac close to (but below) 1 with min_frac far
#'   below it gives a long left tail (severe under-fill is more probable
#'   than a request being fully met), while max_frac only slightly above 1
#'   models a single, non-overlapping fulfillment slightly overshooting its
#'   own submission-time shortfall estimate. demand_interval_days <= 0 (the
#'   shipped default) disables reinforcement entirely — no generator is
#'   added in run_once() in that case, so no RNG draws are consumed,
#'   reproducing the pre-Issue-18 constant-force baseline exactly.
#'
#'   Issue #124: a pool's global value only moves at credit time, so a
#'   naive re-read of the live shortfall on every cycle would let
#'   overlapping cycles (demand_interval_days < fulfillment_lag_days)
#'   independently re-claim the same shortfall an earlier, still-pending
#'   cycle already committed to filling — and, even in the single-cycle
#'   case, crediting the submission-time fill amount unconditionally
#'   (rather than re-checking it against the shortfall actually remaining
#'   fulfillment_lag_days later) could push the pool above its initial
#'   establishment strength. Two guards address this:
#'     - A per-pool "pending" global (reinf_*_pending, R/replication.R)
#'       tracks fill amounts already committed to an in-flight
#'       (submitted-but-not-yet-credited) cycle: incremented by the drawn
#'       fill at submission, decremented by that same amount at credit
#'       time. demand_fn() nets this out of the live shortfall, so a new
#'       cycle only claims shortfall no earlier cycle has already claimed.
#'       Because pending tracks the fill actually committed rather than
#'       the full demand, an under-filled cycle's uncovered remainder is
#'       never removed from view — it stays visible to the next demand
#'       computation instead of being silently written off.
#'     - credit_fn() clamps the credited value to `initial`
#'       (min(initial, current + fill)) rather than adding the
#'       submission-time fill unconditionally, so a pool can never be
#'       credited above establishment strength regardless of how much the
#'       live shortfall has moved during the fulfillment lag (e.g. RTD
#'       credits landing on the same pool in the interim).
build_reinforcement_trajectory <- function() {
  demand_fn <- function(pool_global, pending_global, initial) {
    function() {
      max(0, initial - get_global(env, pool_global) - get_global(env, pending_global))
    }
  }

  fill_fn <- function(demand_attr) {
    function() {
      params <- env_data$vars$force_regeneration$reinforcement
      demand <- get_attribute(env, demand_attr)
      frac <- rtriangle(
        n = 1,
        a = params$fill_min_frac,
        b = params$fill_max_frac,
        c = params$fill_mode_frac
      )
      round(demand * frac)
    }
  }

  claim_fn <- function(pending_global, fill_attr) {
    function() get_global(env, pending_global) + get_attribute(env, fill_attr)
  }

  release_fn <- function(pending_global, fill_attr) {
    function() get_global(env, pending_global) - get_attribute(env, fill_attr)
  }

  credit_fn <- function(pool_global, fill_attr, initial) {
    function() min(initial, get_global(env, pool_global) + get_attribute(env, fill_attr))
  }

  trajectory("Force Reinforcement") %>%
    set_attribute("reinf_combat_demand",
      demand_fn("effective_force_combat", "reinf_combat_pending", env_data$pops$combat)) %>%
    set_attribute("reinf_support_demand",
      demand_fn("effective_force_support", "reinf_support_pending", env_data$pops$support)) %>%
    set_attribute("reinf_combat_fill", fill_fn("reinf_combat_demand")) %>%
    set_attribute("reinf_support_fill", fill_fn("reinf_support_demand")) %>%
    set_global("reinf_combat_pending", claim_fn("reinf_combat_pending", "reinf_combat_fill")) %>%
    set_global("reinf_support_pending", claim_fn("reinf_support_pending", "reinf_support_fill")) %>%
    timeout(function() {
      env_data$vars$force_regeneration$reinforcement$fulfillment_lag_days * day_min
    }) %>%
    set_global("effective_force_combat",
      credit_fn("effective_force_combat", "reinf_combat_fill", env_data$pops$combat)) %>%
    set_global("effective_force_support",
      credit_fn("effective_force_support", "reinf_support_fill", env_data$pops$support)) %>%
    set_global("reinf_combat_pending", release_fn("reinf_combat_pending", "reinf_combat_fill")) %>%
    set_global("reinf_support_pending", release_fn("reinf_support_pending", "reinf_support_fill"))
}

# ── Strategic AME (aeromedical evacuation) sortie schedule (Issue #23 follow-up) ──
#
# Two capacity pools share a single sortie schedule, per AJP-4.10(B)
# [[21]]: a Casualty Staging Unit "collocate[s] already stabilized
# patients" pending transport, so the "ame" pool (standard capacity) is the
# default for the great majority of evacuees; a critical care air
# transport team (CCATT) or critical care aeromedical evacuation support
# team (CCAST) "augment[s] the standard aeromedical evacuation crew" on the
# same sortie to provide in-transit critical care for patients who still
# need it, "limited by capacity" — modelled as the smaller "ame_critical"
# pool.
#
# Aircraft capacity: a sortie that flies carries the fitted patient
# capacity of the airframe the run is configured to fly, resolved by
# resolve_ame_airframe() (R/environment.R) from env_data$vars$role4. The
# two pools are filled together rather than traded against each other,
# because the RAAF describes an AME-configured C-17A carrying both
# categories on the same sortie ("54 ambulatory and 36 high dependency
# stretcher patients"), not one loadout or the other. See README
# Role 4 (National Support Base) Demand Modelling for the source.

#' Builds the AME sortie trajectory: one firing = one scheduled sortie
#' opportunity, which either flies — adding the configured airframe's
#' critical and standard capacity to the "ame" (standard) and
#' "ame_critical" (CCATT/CCAST-supported) resources — or is cancelled
#' (weather, tasking, airframe availability, etc.) and adds nothing to
#' either.
#'
#' @return A simmer trajectory
#'
#' @details Driven by its own generator in run_once() (R/replication.R),
#'   scheduled every schedule_interval_days
#'   (env_data$vars$role4$ame$schedule_interval_days). Capacity is additive
#'   (mod = "+"), never reset to an absolute value: casualties who seize
#'   "ame"/"ame_critical" (r2e_treat_wia()'s Strategic Evac branch, below)
#'   never release it, so a fixed/reset capacity value would permanently
#'   cap total-ever-admitted at that value the first time the server count
#'   catches up to it — the resource would never "reopen" for a later
#'   sortie. Additive capacity avoids this, at the cost of a simplification
#'   documented as MODEL ASSUMPTION — AME Capacity Banking in the README:
#'   unclaimed capacity from an under-subscribed sortie is not lost, and
#'   can be claimed by a later arrival even between scheduled sorties,
#'   rather than being wasted the way a real aircraft's empty seats would
#'   be once it departs.
build_ame_sortie_trajectory <- function() {
  airframe <- resolve_ame_airframe(env_data$vars$role4)

  trajectory("AME Sortie") %>%
    branch(
      option = function() {
        if (runif(1) < env_data$vars$role4$ame$failure_probability) return(2)
        return(1)
      },
      continue = TRUE,
      trajectory("Sortie Flies") %>%
        set_capacity("ame", value = airframe$standard_capacity, mod = "+") %>%
        set_capacity("ame_critical", value = airframe$critical_capacity, mod = "+"),
      trajectory("Sortie Cancelled")
    )
}
