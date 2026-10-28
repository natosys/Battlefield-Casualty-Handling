##############################################
## R/trajectories_common.R                ##
## Shared trajectory helpers              ##
##############################################
#
# Helpers every echelon's trajectories use: resource seizure and release,
# force accounting, team and surgical section selection, the died-of-wounds
# survival functions, the post-operative intensive care requirement draws,
# and the R2B holding evacuation threshold.
#
# Sourced by R/trajectories.R, which loads the packages and R/constants.R
# these functions use; source that file rather than this one alone.

# ── Shared trajectory names ───────────────────────────────────────────────────

#' Name format for the per-section R2E surgical blocks
#'
#' Used by r2e_treat_wia() to name each seize-operate-release block it builds,
#' one per surgical section, and by tests/checks/check_r2e_surgery_seizure.R to
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
#' effective force size at the moment they become a casualty
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
#'   (e.g. the casualty_surge_event_id/priority attributes below) to recover
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
#' effective force size at the moment they return to duty
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

#' Selects the surgical section best placed to take the next case
#'
#' @param sections List of surgical sections, each a character vector of the
#'   resource names of that section's members
#' @return Integer index (1-based) of the chosen section within `sections`
#'
#' @details An echelon can field more surgical sections than operating
#'   theatres, and build_env() gives each section its own alternating day/night
#'   shift, so the section that takes a case must be chosen per casualty rather
#'   than fixed when the trajectory is built. Sections are scored on current
#'   load, the sum of in-use and queued units across the section's member
#'   resources, which is the section-level analogue of the "shortest-queue"
#'   policy used for bed selection elsewhere in this file. On-shift sections
#'   (every member holding non-zero capacity) are preferred outright: an
#'   off-shift section is chosen only when no section is on shift, in which
#'   case the casualty queues until the next shift opens. Ties are broken by
#'   random selection among the equally-loaded candidates, matching
#'   select_r2e_team() above, and a single candidate is returned without
#'   drawing, so an echelon fielding one section consumes no random number
#'   here.
select_surg_section <- function(sections) {
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

#' Selects the R2E surgical section best placed to take the next case
#'
#' @param team_id Index of the R2E team within env_data$elms$r2eheavy
#' @return Integer index (1-based) of the chosen surgical section within that
#'   team's "surg" sub-element list
#'
#' @details R2E fields more surgical sections than operating theatres; the
#'   choice is select_surg_section()'s.
select_r2e_surg_section <- function(team_id) {
  select_surg_section(env_data$elms$r2eheavy[[team_id]][["surg"]])
}

#' Selects the R2B surgical section best placed to take the next case
#'
#' @param team_id Index of the R2B team within env_data$elms$r2b
#' @return Integer index (1-based) of the chosen surgical section within that
#'   team's "surg" sub-element list
#'
#' @details Called only for a unit that fields more than one section; a unit
#'   fielding one has nothing to choose between. The choice is
#'   select_surg_section()'s.
select_r2b_surg_section <- function(team_id) {
  select_surg_section(env_data$elms$r2b[[team_id]][["surg"]])
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

# ── Died-of-wounds accrual ────────────────────────────────────────────────────
#
# Two mechanisms charge died-of-wounds risk, chosen by `dow.accrual.enabled`.
# The accrual mechanism (1, shipped) charges the early, logistic risk at the
# ceiling in force at each moment, banking what has accrued whenever care
# changes the ceiling, and adds a constant post-operative hazard over each
# post-operative intensive care episode and the evacuation wait of an operated
# casualty. The legacy mechanism (0) is the model before it: risk is charged at
# the ceiling in force at the check, the post-operative penalties raise the
# ceiling, and Priority 3 is charged its flat rate at every check. README
# Died of Wounds sets out both.
#
# The switch is read when a trajectory is built, and under the legacy setting
# every builder below appends exactly the activities the model held before, so
# that configuration reproduces the earlier model's random stream and outputs
# byte for byte (tests/checks/check_dow_mechanism.R). Under the accrual setting
# the banks are attributes, each unset until first written: an unset logistic
# bank reads as full survival from the moment of injury, which is why no
# activity initialises them.

#' Ceiling multiplier the legacy mechanism applies to recovery in a holding bed
#'
#' @details The value `dow.treatment_efficacy.r2e_postop_hold_penalty` carried
#'   before the accrual mechanism replaced it. Held here rather than in the
#'   configuration because it exists only so the legacy setting reproduces the
#'   earlier model, and is not a parameter a planner should vary.
LEGACY_POSTOP_HOLD_PENALTY <- 3.0

#' Whether the accrual mechanism charges died-of-wounds risk
#'
#' @return TRUE under `dow.accrual.enabled` = 1, FALSE under 0
#' @details Fails on an absent or malformed value rather than defaulting, on
#'   the pattern of `icu_gate_enabled()`, so a configuration cannot silently
#'   run the mechanism it did not name.
dow_accrual_enabled <- function() {
  value <- env_data$vars$dow$accrual$enabled
  if (is.null(value) || length(value) != 1L || is.na(value) || !value %in% c(0, 1)) {
    stop(sprintf("dow.accrual.enabled must be 0 or 1, found %s",
                 paste(format(value), collapse = ", ")), call. = FALSE)
  }
  value == 1
}

#' Whether this casualty is exempt from died-of-wounds risk
#'
#' @return TRUE for a disease casualty (`dnbi_type` 2), whose pathway is
#'   medical rather than traumatic
dow_exempt <- function() {
  dtype <- get_attribute(env, "dnbi_type")
  !is.na(dtype) && dtype == 2L
}

#' Survival over an interval on this casualty's logistic curve
#'
#' @param t_from Absolute simulation time the interval opens
#' @param t_to   Absolute simulation time it closes
#' @param ceiling The ceiling in force over the interval
#' @return The probability of surviving the interval given survival to its
#'   start; 1 for a casualty with no logistic curve (Priority 3 or no priority)
dow_segment_survival <- function(t_from, t_to, ceiling) {
  prio <- get_attribute(env, "priority")
  if (is.na(prio) || prio > 2 || is.na(ceiling) || t_to <= t_from) return(1)
  dp <- env_data$vars$dow$params
  injury <- get_attribute(env, "injury_time")
  shape <- if (prio == 1) c(dp$p1_p_base, dp$p1_k, dp$p1_t_mid)
           else c(dp$p2_p_base, dp$p2_k, dp$p2_t_mid)
  1 - dow_prob_conditional(t_to - injury, t_from - injury,
                           shape[1], ceiling, shape[2], shape[3])
}

#' The logistic bank, brought up to the present at the current ceiling
#'
#' @return Named numeric: `s`, the survival accrued since the last check, and
#'   `t`, the present, from which the next segment runs
dow_bank_now <- function() {
  s <- get_attribute(env, "dow_bank_s")
  t <- get_attribute(env, "dow_bank_t")
  if (is.na(s)) s <- 1
  if (is.na(t)) t <- get_attribute(env, "injury_time")
  c(s = s * dow_segment_survival(t, now(env), get_attribute(env, "dow_ceiling")),
    t = now(env))
}

#' Applies a treatment effect to the died-of-wounds ceiling
#'
#' @param trj    Trajectory to append to
#' @param factor Zero-argument function returning the multiplier
#' @return The trajectory, with the ceiling change appended.
#'
#' @details Under the accrual mechanism the risk accrued since the last bank
#'   point is banked at the outgoing ceiling before the new one takes effect,
#'   so time spent before a treatment is charged at the rate in force before
#'   it. Under the legacy mechanism the ceiling alone changes.
apply_dow_factor <- function(trj, factor) {
  force(factor)
  if (!dow_accrual_enabled()) {
    return(trj %>% set_attribute("dow_ceiling", function() {
      ceiling <- get_attribute(env, "dow_ceiling")
      if (is.na(ceiling)) return(ceiling)
      ceiling * factor()
    }))
  }
  trj %>% set_attribute(c("dow_bank_s", "dow_bank_t", "dow_ceiling"), function() {
    bank <- dow_bank_now()
    ceiling <- get_attribute(env, "dow_ceiling")
    c(bank[["s"]], bank[["t"]], if (is.na(ceiling)) ceiling else ceiling * factor())
  })
}

#' This casualty's post-operative hazard, per minute
#'
#' @return The priority's `dow.post_op_hazard` rate converted from per day to
#'   per minute; 0 for an exempt casualty or one without a priority
post_op_hazard_rate <- function() {
  prio <- get_attribute(env, "priority")
  if (is.na(prio) || dow_exempt()) return(0)
  h <- env_data$vars$dow$post_op_hazard
  rate <- if (prio == 1) h$p1_rate else if (prio == 2) h$p2_rate else h$p3_rate
  rate / day_min
}

#' Whether any priority carries a post-operative hazard
#'
#' @return TRUE when a `dow.post_op_hazard` rate is positive
post_op_hazard_configured <- function() {
  h <- env_data$vars$dow$post_op_hazard
  any(c(h$p1_rate, h$p2_rate, h$p3_rate) > 0)
}

#' Marks the start of a post-operative episode, before its bed is sought
#'
#' @param trj Trajectory to append to
#' @return The trajectory, stamped under the accrual mechanism and unchanged
#'   under the legacy one.
mark_episode_entry <- function(trj) {
  if (!dow_accrual_enabled()) return(trj)
  trj %>% set_attribute(c("dow_episode_entry", "dow_episode_bed"),
                        function() c(now(env), NA_real_))
}

#' Marks the moment a post-operative episode's bed is in hand
#'
#' @param trj Trajectory to append to
#' @return The trajectory, stamped under the accrual mechanism and unchanged
#'   under the legacy one.
mark_episode_bed <- function(trj) {
  if (!dow_accrual_enabled()) return(trj)
  trj %>% set_attribute("dow_episode_bed", function() now(env))
}

#' Accrues the post-operative hazard over an episode just ended
#'
#' @param trj        Trajectory to append to
#' @param multiplier Zero-argument function returning the care-quality
#'   multiplier of the bed the episode was served in
#' @return The trajectory, with the hazard banked under the accrual mechanism
#'   and unchanged under the legacy one.
#'
#' @details The episode runs from `dow_episode_entry` to the present. Time
#'   before `dow_episode_bed`, spent waiting for the bed with none, takes the
#'   holding-bed multiplier, the care a casualty without a bed receives being
#'   no better than a holding bed's; the rest takes the bed's own multiplier.
#'   One multiplier applies to each part, never a product of two.
accrue_post_op_hazard <- function(trj, multiplier) {
  force(multiplier)
  if (!dow_accrual_enabled()) return(trj)
  trj %>% set_attribute("dow_hazard_s", function() {
    s <- get_attribute(env, "dow_hazard_s")
    if (is.na(s)) s <- 1
    entry <- get_attribute(env, "dow_episode_entry")
    bed   <- get_attribute(env, "dow_episode_bed")
    if (is.na(entry)) return(s)
    if (is.na(bed)) bed <- entry
    overflow <- env_data$vars$dow$treatment_efficacy$hold_bed_penalty
    exposure <- overflow * (bed - entry) + multiplier() * (now(env) - bed)
    s * exp(-post_op_hazard_rate() * exposure)
  })
}

#' Care-quality multiplier of a holding bed in place of intensive care
#'
#' @return `dow.treatment_efficacy.hold_bed_penalty`
hold_bed_multiplier <- function() env_data$vars$dow$treatment_efficacy$hold_bed_penalty

#' Care-quality multiplier of intensive care served at R2B, without an
#' intensivist
#'
#' @return `dow.treatment_efficacy.r2b_icu_penalty`
r2b_icu_multiplier <- function() env_data$vars$dow$treatment_efficacy$r2b_icu_penalty

#' Care-quality multiplier of intensive care served at R2E
#'
#' @return 1, the reference care every multiplier is stated against
r2e_icu_multiplier <- function() 1

#' Settles a died-of-wounds check under the accrual mechanism
#'
#' @param arrival Whether the check is an R2 arrival check, the only kind at
#'   which a Priority 3 casualty's flat risk is charged
#' @param cohort  Whether the check falls within the treated cohort, so that
#'   its expected deaths are added to the casualty's running totals
#' @return Numeric vector, in the order of the keys `dow_check()` sets: the
#'   death probability, the reset banks, the running expected deaths split by
#'   mechanism, and whether Priority 3 risk has been charged.
#'
#' @details The probability is one less the product of the logistic survival
#'   banked since the last check, the segment since the last bank point, and
#'   the post-operative hazard survival banked since the last check. Its split
#'   between the two mechanisms is in proportion to their cumulative hazards;
#'   summed over every check a cohort reaches, the totals are the cohort's
#'   expected deaths from each, which is what the post-operative hazard is
#'   calibrated against (README Died of Wounds, Post-Operative Hazard).
dow_settle_check <- function(arrival, cohort) {
  early <- get_attribute(env, "dow_expected_early")
  late  <- get_attribute(env, "dow_expected_late")
  done  <- get_attribute(env, "dow_p3_charged")
  if (is.na(early)) early <- 0
  if (is.na(late)) late <- 0
  if (is.na(done)) done <- 0
  if (dow_exempt()) return(c(0, 1, now(env), 1, early, late, done))

  s_l <- dow_bank_now()[["s"]]
  prio <- get_attribute(env, "priority")
  if (!is.na(prio) && prio == 3 && arrival && done == 0) {
    s_l <- s_l * (1 - get_attribute(env, "dow_ceiling"))
    done <- 1
  }
  s_h <- get_attribute(env, "dow_hazard_s")
  if (is.na(s_h)) s_h <- 1

  p <- 1 - s_l * s_h
  c_l <- -log(s_l)
  c_h <- -log(s_h)
  share_l <- if (c_l + c_h > 0) c_l / (c_l + c_h) else 0
  if (cohort) {
    early <- early + p * share_l
    late  <- late + p * (1 - share_l)
  }
  c(p, 1, now(env), 1, early, late, done)
}

#' Appends a died-of-wounds check under the accrual mechanism
#'
#' @param trj     Trajectory to append to
#' @param arrival Whether this is an R2 arrival check (see dow_settle_check())
#' @param cohort  Whether the check falls within the treated cohort
#' @param died    Trajectory a casualty who dies here takes; it sets the death
#'   attributes and disposes of the body
#' @param survived Trajectory a survivor takes, empty by default
#' @return The trajectory, with the settlement and the draw appended.
#'
#' @details The probability is settled into an attribute and the banks reset in
#'   one activity before the draw, so the draw reads a number rather than
#'   recomputing it, and a casualty facing no risk consumes no random number.
dow_check <- function(trj, arrival, cohort, died, survived = trajectory()) {
  trj %>%
    set_attribute(c("dow_check_p", "dow_bank_s", "dow_bank_t", "dow_hazard_s",
                    "dow_expected_early", "dow_expected_late", "dow_p3_charged"),
                  function() dow_settle_check(arrival, cohort)) %>%
    # Branches on the settled probability:
    # - a draw beneath it -> died of wounds at this check
    # - otherwise, or no risk at all (no draw taken) -> survives
    branch(
      option = function() {
        p <- get_attribute(env, "dow_check_p")
        if (p > 0 && runif(1) < p) return(1)
        2
      },
      continue = TRUE,
      died,
      survived
    )
}

# ── Post-operative intensive care requirement ─────────────────────────────────
#
# Two distinct episodes are drawn here, stabilisation and post-definitive care,
# from separate configuration blocks and by separate functions. README R2E
# Heavy Trajectory explains what each is for clinically and why they are not
# one quantity; what matters below is the contract that follows from it.
#
# Each episode is drawn once, whole, and what R2B serves forward is subtracted
# from it at R2E rather than redrawn, so no setting of the forward holding rule
# and no route can change the total a casualty receives. Stabilisation (damage
# control) is drawn at R2B or R2E, wherever the casualty first needs it;
# post-definitive care (single-stage) is drawn at R2B only where the rule holds
# it forward, and at R2E otherwise. A change here that draws a second time,
# rather than dividing, breaks that property silently;
# tests/checks/check_icu_time_conservation.R is what catches it.

#' Draws a casualty's whole stabilisation intensive care requirement, in minutes
#'
#' @return Minutes drawn from the R2E stabilisation-ICU triangular distribution
#'
#' @details How much stabilisation a casualty needs follows from the injury
#'   rather than from the facility that happens to hold them, so one draw
#'   covers the whole episode wherever it is served.
#'   forward_headroom() and r2e_stabilisation_minutes() below divide
#'   that single draw between the echelons, which is what makes the total the
#'   same on every route by construction rather than by two parameters being
#'   kept consistent with each other. Each echelon records the minutes it
#'   served as an attribute (`r2b_post_op_min`, `r2e_post_op_min`) so the
#'   invariant is observable in a run's output rather than merely asserted
#'   here; tests/checks/check_icu_time_conservation.R checks it across all three
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

#' The validated forward holding rule for post-operative intensive care at R2B
#'
#' @return Named list: `window_dcs` and `window_single` (minutes a casualty
#'   of that surgical pathway is held forward to stabilise, zero for none),
#'   `capacity_trigger` (logical), `poll_interval` (minutes) and `hold_max`
#'   (the longest forward stay, NA for no limit)
#'
#' @details Reads `r2b.post_op_icu` and rejects a malformed field with a
#'   message naming it. A field absent from the configuration reads as the
#'   disabled value, so a configuration written before the rule existed is
#'   the shipped model. Forward holding is decided per casualty on two
#'   grounds, whichever the casualty meets: clinical stability (a window the
#'   casualty is held for before they are fit to transfer, keyed to damage
#'   control versus single-stage) and capacity (held on while R2E intensive
#'   care is saturated), both limited by `forward_hold_max`.
forward_hold_rule <- function() {
  cfg <- env_data$vars$r2b$post_op_icu
  #' Read and validate one numeric field of the forward holding rule
  #'
  #' @param field Field name under `r2b.post_op_icu`.
  #' @param default Value the field takes when absent.
  #' @param lower Lower bound the value must reach, or exceed where `strict`.
  #' @param strict Whether the value must exceed `lower` rather than reach it.
  #' @return The value as a number; stops, naming the field, where malformed.
  num <- function(field, default, lower, strict = FALSE) {
    val <- cfg[[field]]
    if (is.null(val)) return(default)
    bad <- length(val) != 1L || !is.numeric(val) || is.na(val) ||
      (if (strict) val <= lower else val < lower)
    if (bad) {
      stop(sprintf("r2b.post_op_icu.%s must be a single number %s %s, found %s", field,
                   if (strict) "above" else "of at least", lower,
                   paste(format(val), collapse = ", ")), call. = FALSE)
    }
    as.numeric(val)
  }
  trigger <- num("capacity_trigger", 0, 0)
  if (!trigger %in% c(0, 1)) {
    stop(sprintf("r2b.post_op_icu.capacity_trigger must be 0 or 1, found %s", format(trigger)),
         call. = FALSE)
  }
  hold_max <- num("forward_hold_max", NA_real_, 0)
  list(
    window_dcs    = num("stability_window_dcs", 0, 0),
    window_single = num("stability_window_single_stage", 0, 0),
    capacity_trigger = trigger == 1,
    poll_interval = num("capacity_poll_interval", 30, 0, strict = TRUE),
    hold_max      = hold_max
  )
}

#' Whether forward post-operative holding is in force for the casualty's pathway
#'
#' @return TRUE when a stability window or the capacity trigger can hold this
#'   casualty's post-operative intensive care forward at R2B
forward_hold_enabled <- function() {
  rule <- forward_hold_rule()
  window <- if (single_stage()) rule$window_single else rule$window_dcs
  window > 0 || rule$capacity_trigger
}

#' The post-operative intensive care requirement that may be held forward
#'
#' @return The casualty's whole requirement for the episode that follows their
#'   R2B operation, in minutes: stabilisation for damage control, post-definitive
#'   care for single-stage; zero where none has been drawn
forward_requirement <- function() {
  total <- get_attribute(env, if (single_stage()) "post_definitive_total" else "stabilisation_total")
  if (is.na(total)) 0 else total
}

#' Minutes of the post-operative requirement already served forward at R2B
#'
#' @return The `r2b_post_op_min` attribute, zero where nothing was served
forward_served <- function() {
  served <- get_attribute(env, "r2b_post_op_min")
  if (is.na(served)) 0 else served
}

#' The most this casualty may still be held forward at R2B
#'
#' @return The lesser of the requirement and `forward_hold_max`, less the
#'   minutes already served, never negative
forward_headroom <- function() {
  limit <- forward_requirement()
  cap <- forward_hold_rule()$hold_max
  if (!is.na(cap)) limit <- min(limit, cap)
  max(0, limit - forward_served())
}

#' Minutes held forward on stability grounds
#'
#' @return The stability window for the casualty's surgical pathway, limited by
#'   the requirement and `forward_hold_max`; zero when no window is configured
forward_stability_minutes <- function() {
  rule <- forward_hold_rule()
  window <- if (single_stage()) rule$window_single else rule$window_dcs
  min(window, forward_headroom())
}

#' Whether R2E intensive care is saturated
#'
#' @return TRUE when every R2E intensive care bed is occupied; a casualty
#'   waiting for one counts, so a pool with a queue is saturated
#'
#' @details The capacity ground for holding a casualty forward: R2E cannot
#'   take them yet. Occupancy plus queue is read against capacity across every
#'   R2E team's beds, the transfer destination being whichever team the
#'   casualty is sent to.
r2e_icu_saturated <- function() {
  beds <- unlist(lapply(env_data$elms$r2eheavy, function(team) team[["icu_bed"]]))
  if (length(beds) == 0) return(FALSE)
  load <- sum(get_server_count(env, resources = beds)) + sum(get_queue_count(env, resources = beds))
  cap <- sum(get_capacity(env, resources = beds))
  !is.na(load) && !is.na(cap) && cap > 0 && load >= cap
}

#' Whether this casualty is held forward further on capacity grounds
#'
#' @return TRUE when the capacity trigger is configured, R2E intensive care is
#'   saturated and the casualty has requirement and forward headroom left
forward_capacity_due <- function() {
  forward_hold_rule()$capacity_trigger && forward_headroom() > 0 && r2e_icu_saturated()
}

#' Minutes of the stabilisation requirement remaining for R2E
#'
#' @return The casualty's `stabilisation_total`, less whatever was served
#'   forward. A casualty who was not operated on at R2B served nothing
#'   forward, so receives the whole requirement here: the same amount, on
#'   either route.
r2e_stabilisation_minutes <- function() {
  total <- get_attribute(env, "stabilisation_total")
  prior <- get_attribute(env, "r2b_surgery")
  if (!is.na(prior) && prior == 1) return(max(0, total - forward_served()))
  total
}

# ── R2B holding evacuation threshold ──────────────────────────────────────────
#
# The three functions below divide one convalescence draw between the echelons,
# exactly as forward_served() and r2e_stabilisation_minutes() divide
# the stabilisation requirement above. README R2B Trajectory states what the
# threshold is for and why the time is divided rather than redrawn.
#
# The property to preserve is that the R2B holding load the threshold removes
# reappears at R2E as the same quantity, so any movement in total system load
# is attributable to the routing and not to a second, unaccounted draw.
# tests/checks/check_lever_realisation.R asserts it.

#' The configured R2B holding evacuation threshold, in minutes
#'
#' @return The threshold, or NA when it is disabled, which is the shipped
#'   default.
#' @details Named for the field it reads. `hold_threshold` is a different
#'   parameter entirely: an occupancy fraction deciding whether a casualty is
#'   sent to an R2B unit at all, read by select_r2b_for_hold(). This one is a
#'   duration deciding how long they may stay once there. The Morris parameter
#'   named `r2b_hold_threshold` is the former, not this.
#'
#'   A non-positive value disables the threshold, the convention
#'   `force_regeneration.reinforcement.demand_interval_days` also follows. Zero
#'   cannot mean a zero-minute forward stay: a casualty held forward for no
#'   time is one who was never held, which is what `hold_threshold` already
#'   decides upstream.
r2b_evac_threshold <- function() {
  thresh <- env_data$vars$r2b$holding$evac_threshold
  if (is.null(thresh) || is.na(thresh) || thresh <= 0) return(NA_real_)
  thresh
}

#' Minutes of a casualty's drawn convalescence served forward at R2B
#'
#' @return The lesser of the casualty's `r2b_hold_drawn` attribute and the
#'   threshold. With no threshold configured the whole draw is served forward,
#'   which is the shipped behaviour.
r2b_hold_minutes <- function() {
  drawn  <- get_attribute(env, "r2b_hold_drawn")
  thresh <- r2b_evac_threshold()
  if (is.na(thresh)) return(drawn)
  min(drawn, thresh)
}

#' Minutes of the drawn convalescence remaining when the threshold evacuates
#'
#' @return What the draw leaves after the forward stay, which is zero for a
#'   casualty who recovers forward and returns to duty from R2B. A positive
#'   value selects the evacuation branch; R2E serves the remainder of the
#'   casualty's one `recovery_to_duty_days` draw
#'   (r2e_convalescence_remaining_minutes()), so nothing is redrawn.
r2b_hold_residual_minutes <- function() {
  max(0, get_attribute(env, "r2b_hold_drawn") - r2b_hold_minutes())
}
