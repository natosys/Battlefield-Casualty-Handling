##############################################
## R/analysis_role4_ame.R                   ##
## Role 4 demand and strategic airlift      ##
##############################################
#
# The national support base demand model (length of stay, ward phases,
# intensive care days, the operations owed) and the strategic aeromedical
# evacuation reconstruction (sortie demand, sorties flown, the backlog of each
# airlift pool) with their figures.
#
# Sourced by R/analysis.R, which loads the packages and the shared modules
# these functions use; source that file rather than this one alone.

# ── Role 4 (national support base) demand model ────────────────────────────
# Strategically evacuated casualties (r2e_evac == 1) depart the theatre
# system and are not simulated further. These functions estimate the
# resulting demand signal at the national support base as a post-simulation
# calculation over the evacuation event log — Role 4 is modelled as
# unconstrained demand, not a simmer resource (see README Role 4 sub-section
# and Limitations).

#' Ward levels the Role 4 census reports, in display order
#'
#' @param r4_params `env_data$vars$role4` list
#' @return Character vector of ward names, outermost acuity first
#'
#' @details The levels are configured rather than fixed so that a ward added
#'   to the mapping appears in the census and the plots without three separate
#'   literal vectors having to be found and edited. Validated by
#'   validate_role4_wards(), which every caller reaches first.
role4_ward_levels <- function(r4_params) {
  as.character(unlist(r4_params$wards$levels))
}

#' The ward each length-of-stay category is admitted to
#'
#' @param r4_params `env_data$vars$role4` list
#' @return Named character vector, one ward per length-of-stay category
role4_ward_map <- function(r4_params) {
  categories <- c("p1_surgical", "p1_nonsurgical", "p2", "p3_dnbi")
  vapply(categories, function(cat) as.character(r4_params$wards[[cat]]),
         character(1))
}

#' Validate the Role 4 ward mapping and intensive care continuation block
#'
#' @param r4_params `env_data$vars$role4` list
#' @return Invisible TRUE, or stops naming the field and the value found
#'
#' @details Read from env_data.json, so validated at the boundary rather than
#'   trusted (E1, E2). A ward naming no configured level is the failure worth
#'   guarding: the census counts by ward and the plot fills by a factor over
#'   these levels, so an unknown ward would silently drop that category's
#'   casualties from both rather than erroring.
validate_role4_wards <- function(r4_params) {
  levels <- role4_ward_levels(r4_params)
  if (length(levels) == 0 || any(is.na(levels)) || any(!nzchar(levels))) {
    stop("role4.wards.levels must name at least one ward; found: ",
         paste(levels, collapse = ", "))
  }
  if (anyDuplicated(levels)) {
    stop("role4.wards.levels names a ward twice: ",
         paste(levels[duplicated(levels)], collapse = ", "))
  }

  mapping <- role4_ward_map(r4_params)
  for (cat in names(mapping)) {
    if (is.na(mapping[[cat]]) || !nzchar(mapping[[cat]])) {
      stop("role4.wards.", cat, " is missing a ward name")
    }
    if (!mapping[[cat]] %in% levels) {
      stop("role4.wards.", cat, " is '", mapping[[cat]],
           "', which is not one of role4.wards.levels (",
           paste(levels, collapse = ", "), ")")
    }
  }

  cont <- r4_params$icu_continuation
  if (is.null(cont)) return(invisible(TRUE))
  enabled <- as.numeric(cont$enabled)
  if (is.na(enabled) || !enabled %in% c(0, 1)) {
    stop("role4.icu_continuation.enabled must be 0 or 1; found: ",
         format(cont$enabled))
  }
  if (enabled == 0) return(invisible(TRUE))

  bounds <- vapply(c("min", "mode", "max"),
                   function(nm) as.numeric(cont[[nm]]), numeric(1))
  if (any(is.na(bounds)) || any(bounds < 0)) {
    stop("role4.icu_continuation min/mode/max must be non-negative numbers; found: ",
         paste(format(bounds), collapse = ", "))
  }
  if (!(bounds[["min"]] <= bounds[["mode"]] && bounds[["mode"]] <= bounds[["max"]])) {
    stop("role4.icu_continuation requires min <= mode <= max; found: ",
         paste(format(bounds), collapse = ", "))
  }
  if (bounds[["min"]] == bounds[["max"]]) {
    stop("role4.icu_continuation min and max are both ", format(bounds[["min"]]),
         ", which draws no distribution; disable it with enabled = 0 instead")
  }
  step_down <- as.character(cont$step_down_ward)
  if (length(step_down) != 1 || is.na(step_down) || !step_down %in% levels) {
    stop("role4.icu_continuation.step_down_ward is '", format(step_down),
         "', which is not one of role4.wards.levels (",
         paste(levels, collapse = ", "), ")")
  }
  icu_ward <- as.character(cont$icu_ward)
  if (length(icu_ward) != 1 || is.na(icu_ward) || !icu_ward %in% levels) {
    stop("role4.icu_continuation.icu_ward is '", format(icu_ward),
         "', which is not one of role4.wards.levels (",
         paste(levels, collapse = ", "), ")")
  }
  if (identical(icu_ward, step_down)) {
    stop("role4.icu_continuation.icu_ward and step_down_ward are both '",
         icu_ward, "', so the step-down moves nobody")
  }
  invisible(TRUE)
}

#' Assigns each strategically evacuated casualty a Role 4 length-of-stay
#' category and ward, and draws a length of stay from the matching
#' triangular distribution
#'
#' @param arrivals_log Tidy arrivals+attributes data frame (analyse_run()'s
#'   `combined`); must include r2e_evac, injury_type, priority,
#'   treatment_received, evacuation_day, replication
#' @param r4_los_params `env_data$vars$role4` list (los_p1_surgical,
#'   los_p1_nonsurgical, los_p2, los_p3_dnbi, each with min/mode/max in days)
#' @return `arrivals_log` filtered to r2e_evac == 1, with added columns
#'   los_category, ward, los_days, r4_admit_day, r4_discharge_day
#'
#' @details Length-of-stay category (MODEL ASSUMPTION — Role 4 Ward/LoS
#'   Category Mapping, see README Role 4 sub-section): DNBI casualties (any
#'   priority) and Priority 3 WIA share the p3_dnbi category and General
#'   Ward; Priority 1 casualties split into ICU (if `treatment_received`) or
#'   Surgical Ward (if not); Priority 2 casualties are assigned Surgical
#'   Ward. rtriangle() (package `triangle`) only accepts scalar a/b/c, hence
#'   the row-at-a-time draw (matches the per-entity draw pattern used
#'   throughout R/trajectories.R).
assign_role4_los <- function(arrivals_log, r4_los_params) {
  # evacuation_day requires a completed AME boarding
  # — r2e_evac == 1 marks the disposition decision, but a casualty can still
  # be queued awaiting a scheduled sortie at the end of the run (see
  # ame_wait_minutes/evacuation_day left NA in that case). Only casualties
  # who actually reached Role 4 have a real admission date to census.
  role4_evac <- arrivals_log %>%
    filter(!is.na(r2e_evac) & r2e_evac == 1 & !is.na(evacuation_day))

  if (nrow(role4_evac) == 0) return(role4_evac)

  los_lookup <- data.frame(
    los_category = c("p1_surgical", "p1_nonsurgical", "p2", "p3_dnbi"),
    los_min  = c(r4_los_params$los_p1_surgical$min,  r4_los_params$los_p1_nonsurgical$min,
                 r4_los_params$los_p2$min,  r4_los_params$los_p3_dnbi$min),
    los_mode = c(r4_los_params$los_p1_surgical$mode, r4_los_params$los_p1_nonsurgical$mode,
                 r4_los_params$los_p2$mode, r4_los_params$los_p3_dnbi$mode),
    los_max  = c(r4_los_params$los_p1_surgical$max,  r4_los_params$los_p1_nonsurgical$max,
                 r4_los_params$los_p2$max,  r4_los_params$los_p3_dnbi$max)
  )

  validate_role4_wards(r4_los_params)
  ward_map <- role4_ward_map(r4_los_params)

  # The length-of-stay draw is the analysis pipeline's only RNG consumer, and
  # the pipeline is a report over monitoring data already produced: analysing
  # one run twice has to give one answer. Preserving the stream around the
  # draw is what makes it do so; the draw itself is unmoved.
  with_preserved_rng(
    role4_evac %>%
      mutate(
        outstanding_repair = if ("definitive_repair_outstanding" %in% names(.)) {
          !is.na(definitive_repair_outstanding) & definitive_repair_outstanding == 1
        } else {
          FALSE
        },
        los_category = case_when(
          # A casualty flown out with the definitive repair still to be
          # performed is the sickest arrival this echelon receives, whatever
          # their triage priority: the operation they are waiting for is the
          # one that would have stabilised them. They take the operated
          # Priority 1 category, which is the same reasoning that puts them on
          # the critical airlift route and in an intensive care bed before the
          # flight (R/trajectories.R), rather than the category their priority
          # alone would give them.
          outstanding_repair                                           ~ "p1_surgical",
          !is.na(injury_type) & injury_type == 2                       ~ "p3_dnbi",
          !is.na(priority) & priority == 3                             ~ "p3_dnbi",
          !is.na(priority) & priority == 1 &
            !is.na(treatment_received) & treatment_received == 1       ~ "p1_surgical",
          !is.na(priority) & priority == 1                             ~ "p1_nonsurgical",
          !is.na(priority) & priority == 2                             ~ "p2",
          TRUE                                                          ~ "p3_dnbi"
        ),
        ward = unname(ward_map[los_category])
      ) %>%
      left_join(los_lookup, by = "los_category") %>%
      rowwise() %>%
      mutate(
        los_days          = rtriangle(1, a = los_min, b = los_max, c = los_mode),
        r4_admit_day      = evacuation_day,
        r4_discharge_day  = evacuation_day + ceiling(los_days) - 1
      ) %>%
      ungroup() %>%
      dplyr::select(-los_min, -los_mode, -los_max, -outstanding_repair) %>%
      add_role4_icu_days(r4_los_params)
  )
}

#' Days of the Role 4 stay served in intensive care rather than on a ward
#'
#' @param assigned Frame returned by assign_role4_los()'s draw, carrying
#'   ward, los_days and post_definitive_min
#' @param r4_params `env_data$vars$role4` list
#' @return `assigned` with an added r4_icu_days column
#'
#' @details Role 4 continues the post-operative intensive care episode that
#'   R2E begins rather than starting a new one, so the days owed here are the
#'   casualty's whole post-operative requirement less what theatre already
#'   served. That is the contract draw_stabilisation_icu() already applies
#'   across R2B and R2E (R/trajectories.R): one requirement, divided between
#'   the echelons that serve it, so the total cannot depend on the route.
#'   post_definitive_min records the minutes theatre served and is set for
#'   operated casualties alone, so an unoperated evacuee is owed nothing here
#'   and keeps the whole stay on its admission ward, which is what the static
#'   mapping already did for every category but p1_surgical.
#'
#'   With the block disabled no draw is taken at all, which is what makes the
#'   shipped configuration reproduce the census this function replaced rather
#'   than merely approximate it: an extra draw inside the preserved stream
#'   would move every subsequent length of stay.
add_role4_icu_days <- function(assigned, r4_params) {
  cont <- r4_params$icu_continuation
  enabled <- !is.null(cont) && !is.na(as.numeric(cont$enabled)) &&
    as.numeric(cont$enabled) == 1
  if (!enabled) return(assigned %>% mutate(r4_icu_days = los_days))

  served_days <- if ("post_definitive_min" %in% names(assigned)) {
    ifelse(is.na(assigned$post_definitive_min), 0,
           assigned$post_definitive_min / DAY_MIN)
  } else {
    rep(0, nrow(assigned))
  }
  # A single-stage casualty may have served part of the post-definitive
  # requirement forward at R2B, which counts towards the same requirement.
  forward_cols <- c("post_definitive_total", "r2b_post_op_min")
  if (all(forward_cols %in% names(assigned))) {
    forward <- !is.na(assigned$post_definitive_total) & !is.na(assigned$r2b_post_op_min)
    served_days <- served_days + ifelse(forward, assigned$r2b_post_op_min / DAY_MIN, 0)
  }
  # Two conditions, and the second is the one a first implementation missed.
  # A casualty theatre never operated on carries no post-operative episode for
  # Role 4 to continue. And a casualty whose category is not admitted to
  # intensive care has no intensive care phase to divide either: continuing one
  # for them would step a general ward casualty down into a surgical ward,
  # moving bed-days between two wards neither of which is the one under study.
  icu_ward <- as.character(cont$icu_ward)
  # A casualty released with the definitive repair outstanding is the third
  # case, and sets neither condition above: theatre performed no definitive
  # repair for them, so they carry no post_definitive_min at all. Their whole
  # post-operative requirement falls here, served_days being zero, which is the
  # same conservation contract read from the other end.
  outstanding <- if ("definitive_repair_outstanding" %in% names(assigned)) {
    !is.na(assigned$definitive_repair_outstanding) &
      assigned$definitive_repair_outstanding == 1 & assigned$ward == icu_ward
  } else {
    rep(FALSE, nrow(assigned))
  }
  operated <- if ("post_definitive_min" %in% names(assigned)) {
    !is.na(assigned$post_definitive_min) & assigned$ward == icu_ward
  } else {
    rep(FALSE, nrow(assigned))
  }
  operated <- operated | outstanding

  total <- rep(0, nrow(assigned))
  if (any(operated)) {
    total[operated] <- rtriangle(sum(operated), a = as.numeric(cont$min),
                                 b = as.numeric(cont$max),
                                 c = as.numeric(cont$mode))
  }
  owed <- pmax(0, total - served_days)
  assigned %>%
    mutate(r4_icu_days = pmin(pmax(0, owed), los_days))
}

#' Expand each Role 4 stay into the ward phases it is served in
#'
#' @param assigned Frame returned by assign_role4_los()
#' @param r4_params `env_data$vars$role4` list
#' @return One row per casualty-phase, with ward, phase_start and phase_end
#'
#' @details A stay is at most two phases, an intensive care phase followed by
#'   the step-down ward that carries the remainder. The remainder is taken as
#'   the difference rather than drawn, so the phases sum to the drawn length of
#'   stay exactly whatever the intensive care requirement comes to;
#'   tests/checks/check_role4_ward_phases.R asserts that conservation. A stay owed
#'   no intensive care, or owed all of it, yields the single phase the static
#'   mapping produced.
role4_ward_phases <- function(assigned, r4_params) {
  cont <- r4_params$icu_continuation
  enabled <- !is.null(cont) && !is.na(as.numeric(cont$enabled)) &&
    as.numeric(cont$enabled) == 1

  base <- assigned %>%
    mutate(phase_ward = ward,
           phase_days = if (enabled) r4_icu_days else los_days)

  if (!enabled) {
    return(base %>%
             mutate(phase_start = r4_admit_day, phase_end = r4_discharge_day) %>%
             dplyr::select(-phase_days))
  }

  step_down_ward <- as.character(cont$step_down_ward)
  icu_phase <- base %>%
    filter(phase_days > 0) %>%
    mutate(phase_start = r4_admit_day,
           phase_end   = r4_admit_day + ceiling(phase_days) - 1) %>%
    dplyr::select(-phase_days)
  # The step-down ward applies only where an intensive care phase preceded it.
  # A casualty owed no intensive care never left their admission ward, so
  # renaming them here would move bed-days between two wards on the strength of
  # a phase that did not happen.
  ward_phase <- base %>%
    filter(los_days - phase_days > 0) %>%
    mutate(phase_ward  = dplyr::if_else(phase_days > 0, step_down_ward, phase_ward),
           phase_start = r4_admit_day + ceiling(phase_days),
           phase_end   = r4_discharge_day) %>%
    dplyr::select(-phase_days) %>%
    filter(phase_start <= phase_end)

  bind_rows(icu_phase, ward_phase)
}

#' Whether the Role 4 theatre demand report is configured on
#'
#' @param r4_params `env_data$vars$role4` list
#' @return TRUE where `role4.surgery.enabled` is 1
#' @details Absent block reads as off, so a configuration written before the
#'   block existed reports no theatre demand rather than erroring.
role4_surgery_enabled <- function(r4_params) {
  surgery <- r4_params$surgery
  if (is.null(surgery)) return(FALSE)
  enabled <- suppressWarnings(as.numeric(surgery$enabled))
  if (is.na(enabled) || !enabled %in% c(0, 1)) {
    stop("role4.surgery.enabled must be 0 or 1; found: ", format(surgery$enabled))
  }
  enabled == 1
}

#' The label the Role 4 theatre demand is reported under
#'
#' @param r4_params `env_data$vars$role4` list
#' @return The theatre label, or stops naming the field
#' @details Named for the bed type the deployed echelons field rather than for
#'   a service, on the same reasoning as the ward levels: a Role 4 theatre hour
#'   is the same hour a forward one is.
role4_theatre_label <- function(r4_params) {
  label <- as.character(r4_params$surgery$theatre_label)
  if (length(label) != 1L || is.na(label) || !nzchar(label)) {
    stop("role4.surgery.theatre_label must name a theatre type; found: ",
         paste(format(r4_params$surgery$theatre_label), collapse = ", "))
  }
  label
}

#' The reconstruction sequence a casualty is owed at the national support base
#'
#' @param assigned Frame returned by assign_role4_los(), carrying
#'   reconstruction_required, r4_admit_day and r4_icu_days
#' @param r4_params `env_data$vars$role4` list
#' @return One row per casualty-procedure, with replication, day and a
#'   `procedure` label. Empty where nobody is owed a reconstruction sequence.
#'
#' @details A casualty whose wounds require staged soft-tissue coverage returns
#'   to theatre repeatedly before anything definitive is done: irrigation,
#'   debridement and a vacuum dressing change, on the interval
#'   `role4.surgery.return_interval_*` states, until the wound is clean enough
#'   to close. The sequence then ends in one reconstruction.
#'
#'   The interval is the parameter and the count is the consequence, which is
#'   what keeps the count out of the configuration: how many procedures a
#'   casualty receives follows from how long their debridement phase runs, and
#'   that is the intensive care phase the census already derives from severity.
#'   Reconstruction is what steps the casualty down, so debridement runs while
#'   they are critically ill and the ward phase that follows carries none.
#'
#'   Returns after the reconstruction are governed by
#'   `post_reconstruction_return_rate` and ship at zero. That is close to what
#'   the reported flap failure rates imply, and the rate exists so the small
#'   residual is representable rather than denied; see README Role 4
#'   (National Support Base) Demand Modelling.
#'
#'   The interval is drawn per procedure, and the caller runs the draw inside
#'   `with_preserved_rng()`, so the sequence is a function of the run rather
#'   than of the caller's stream position and repeats on re-analysis.
role4_reconstruction_sequence <- function(assigned, r4_params) {
  empty <- data.frame(replication = integer(0), day = integer(0),
                      procedure = character(0))
  if (!"reconstruction_required" %in% names(assigned)) return(empty)
  cohort <- assigned %>%
    filter(!is.na(reconstruction_required) & reconstruction_required == 1)
  if (nrow(cohort) == 0) return(empty)

  surgery <- r4_params$surgery
  bounds <- vapply(c("return_interval_min", "return_interval_mode", "return_interval_max"),
                   function(nm) suppressWarnings(as.numeric(surgery[[nm]])), numeric(1))
  if (any(is.na(bounds)) || any(bounds <= 0) ||
        !(bounds[[1]] <= bounds[[2]] && bounds[[2]] <= bounds[[3]])) {
    stop("role4.surgery.return_interval_min/mode/max must be positive with ",
         "min <= mode <= max; found: ", paste(format(bounds), collapse = ", "),
         call. = FALSE)
  }

  rows <- lapply(seq_len(nrow(cohort)), function(i) {
    admit <- as.numeric(cohort$r4_admit_day[i])
    phase <- as.numeric(cohort$r4_icu_days[i])
    days <- numeric(0)
    offset <- 0
    # Each return is one interval after the last, and the phase ends at the
    # reconstruction, so a phase shorter than one interval yields the
    # reconstruction alone rather than no procedure at all.
    repeat {
      offset <- offset + rtriangle(1, a = bounds[[1]], b = bounds[[3]], c = bounds[[2]])
      if (offset >= phase) break
      days <- c(days, admit + floor(offset))
    }
    data.frame(replication = rep(cohort$replication[i], length(days) + 1),
               day = c(days, admit + ceiling(max(phase, 0))),
               procedure = c(rep("debridement", length(days)), "reconstruction"),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Computes the operating theatre demand a casualty stream places on Role 4
#'
#' @param arrivals_log Tidy arrivals+attributes data frame (analyse_run()'s
#'   `combined`) — see assign_role4_los()
#' @param r4_params `env_data$vars$role4` list
#' @return Tidy data frame with columns replication, day, theatre, operations,
#'   theatre_minutes — one row per replication-day on which an operation is
#'   owed. Empty where the configuration releases nobody.
#'
#' @details This reports a requirement rather than simulating an activity,
#'   which is the same footing the bed census stands on: nothing queues for a
#'   theatre here, nothing is refused and nothing is pushed back into theatre,
#'   because the model sets the demand a deployed trauma system places on the
#'   national support base and does not simulate that echelon.
#'
#'   The population is the casualties released under
#'   `r2eheavy.second_surgery.saturation_queue_threshold` with their definitive
#'   repair outstanding (R/trajectories.R). The duration is the one the
#'   releasing theatre drew and carried rearward on the casualty
#'   (`definitive_repair_minutes`), so the operation is conserved rather than
#'   re-estimated and this function takes no draw at all. A casualty who
#'   arrives with their repair complete is owed nothing and contributes no row.
#'
#'   The operation is owed on the day of admission, ahead of the intensive care
#'   that follows it, so it cannot fall after the step-down the ward phases
#'   apply; tests/checks/check_role4_surgical_demand.R asserts that ordering.
#'
#'   Two populations contribute. A casualty released with their definitive
#'   repair outstanding carries that one operation rearward with the duration
#'   the releasing theatre drew, so it is conserved rather than re-estimated. A
#'   casualty whose wounds require staged soft-tissue coverage carries the
#'   reconstruction sequence `role4_reconstruction_sequence()` builds. The two
#'   are independent and a casualty can be in both.
#'
#'   Reported in operations and theatre-minutes rather than as bed-days, because
#'   a theatre is a throughput resource: an operation of 95 minutes occupies no
#'   bed for a day, and adding it to the bed census would report a quantity in
#'   two units at once. The minutes column counts only the conserved definitive
#'   repairs, no open-access source reporting how long a debridement or a flap
#'   takes at this echelon; the operations column counts every procedure.
compute_role4_surgical_demand <- function(arrivals_log, r4_params) {
  empty <- data.frame(replication = integer(0), day = integer(0),
                      theatre = character(0), operations = integer(0),
                      theatre_minutes = numeric(0))
  if (!role4_surgery_enabled(r4_params)) return(empty)
  needed <- c("definitive_repair_outstanding", "definitive_repair_minutes")
  if (!all(needed %in% names(arrivals_log))) return(empty)

  theatre <- role4_theatre_label(r4_params)
  assigned <- assign_role4_los(arrivals_log, r4_params)
  if (nrow(assigned) == 0) return(empty)

  # The outstanding definitive repair, carried rearward with its drawn duration.
  repairs <- assigned %>%
    filter(!is.na(definitive_repair_outstanding) & definitive_repair_outstanding == 1) %>%
    mutate(day = r4_admit_day,
           theatre_minutes = ifelse(is.na(definitive_repair_minutes), 0,
                                    definitive_repair_minutes)) %>%
    dplyr::select(replication, day, theatre_minutes) %>%
    mutate(operations = 1L)

  # The reconstruction sequence, counted but not timed: no open-access source
  # reports the theatre time a debridement or a flap takes at this echelon, so
  # the minutes column carries only the durations the model actually conserves
  # and the count carries every procedure.
  # The interval draw runs inside the preserved stream, as the length-of-stay
  # draw above it does, so the report is idempotent and leaves the caller's
  # stream where it found it: analysing one run twice has to give one answer.
  sequence <- with_preserved_rng(role4_reconstruction_sequence(assigned, r4_params)) %>%
    mutate(theatre_minutes = 0, operations = 1L) %>%
    dplyr::select(replication, day, theatre_minutes, operations)

  combined_demand <- bind_rows(repairs, sequence)
  if (nrow(combined_demand) == 0) return(empty)

  combined_demand %>%
    mutate(theatre = theatre) %>%
    group_by(replication, day, theatre) %>%
    summarise(operations = sum(operations),
              theatre_minutes = sum(theatre_minutes), .groups = "drop") %>%
    as.data.frame()
}

#' Computes daily Role 4 (national support base) bed occupancy by ward
#' category
#'
#' @param arrivals_log Tidy arrivals+attributes data frame (analyse_run()'s
#'   `combined`) — see assign_role4_los()
#' @param r4_los_params `env_data$vars$role4` list — see assign_role4_los()
#' @return Tidy data frame with columns replication, day, ward, occupancy —
#'   one row per casualty-occupied ward-day per replication. The caller
#'   aggregates across replications for display (mean occupancy) and within
#'   replications for the peak-occupancy multi-run CI (see analyse_run()).
compute_role4_census <- function(arrivals_log, r4_los_params) {
  role4_evac <- assign_role4_los(arrivals_log, r4_los_params)

  if (nrow(role4_evac) == 0) {
    return(data.frame(replication = integer(0), day = integer(0),
                      ward = character(0), occupancy = integer(0)))
  }

  role4_ward_phases(role4_evac, r4_los_params) %>%
    rowwise() %>%
    mutate(day = list(seq(phase_start, phase_end))) %>%
    unnest(day) %>%
    ungroup() %>%
    mutate(ward = phase_ward) %>%
    count(replication, day, ward, name = "occupancy")
}

#' Computes daily strategic AME (aeromedical evacuation) sortie demand under
#' an unconstrained-baseline assumption
#'
#' @param arrivals_log Tidy arrivals+attributes data frame (analyse_run()'s
#'   `combined`); must include r2e_evac, evacuation_decision_day, replication
#' @param ame_capacity Casualties per AME sortie — the caller passes the
#'   configured airframe's combined critical + standard capacity
#'   (`resolve_ame_airframe()`, R/environment.R), since this baseline does
#'   not distinguish acuity/route and is meant to represent same-day,
#'   uncapped best-case throughput
#' @return Tidy data frame with columns replication, day, evacuation_count,
#'   sorties_required — a derived planning metric
#'   (`ceiling(evacuation_count / ame_capacity)`), not a simulated resource
#'   constraint. The caller aggregates across replications for display
#'   (mean daily count, cumulative sorties) and within replications for the
#'   total-sorties multi-run CI (see analyse_run()).
#'
#' @details Uses `evacuation_decision_day` (when the Strategic Evac
#'   disposition was decided), not `evacuation_day` (when a casualty
#'   actually boarded, gated by the real scheduled
#'   "ame" resource). This function models the *unconstrained* theoretical
#'   demand a same-day sortie capacity would need to clear that day's
#'   decisions; grouping by the constrained actual departure day would
#'   circularly smooth demand by the very schedule this baseline is meant
#'   to be compared against. See analyse_run()'s ame_wait_time_summary and
#'   ame_backlog_plot for the real constrained-resource outputs.
compute_ame_demand <- function(arrivals_log, ame_capacity) {
  role4_evac <- arrivals_log %>%
    filter(!is.na(r2e_evac) & r2e_evac == 1)

  if (nrow(role4_evac) == 0) {
    return(data.frame(replication = integer(0), day = integer(0),
                      evacuation_count = integer(0), sorties_required = integer(0)))
  }

  role4_evac %>%
    count(replication, evacuation_decision_day, name = "evacuation_count") %>%
    rename(day = evacuation_decision_day) %>%
    mutate(sorties_required = ceiling(evacuation_count / ame_capacity))
}

# ── Strategic AME queue depth and sortie timeline visualisation ───────────────
# build_ame_sortie_trajectory() (R/trajectories.R) keeps no sortie log of its
# own — every outcome (flown or cancelled) has to be reconstructed from
# the "ame"/"ame_critical" resource monitor. The schedule
# itself is fully deterministic (fixed `at(seq(...))` times, R/replication.R),
# so every scheduled opportunity — including cancelled ones, which leave no
# capacity change at all — can be enumerated without needing to scan the
# monitor for events; only the *outcome* at each known time is read from it.

#' Reconstructs the outcome of every scheduled strategic AME sortie
#' opportunity from the "ame"/"ame_critical" resource capacity
#' monitor
#'
#' @param resources Tidy resource monitor data frame (analyse_run()'s
#'   `resources` or analyse_replications()'s `resources_raw`); must include
#'   replication, resource, time, capacity, queue
#' @param role4_params `env_data$vars$role4` list — supplies
#'   `ame$schedule_interval_days` and the configured airframe's capacity
#'   pair (via `resolve_ame_airframe()`, R/environment.R)
#' @param n_days Simulation duration in days, used to bound the schedule the
#'   same way `run_once()` (R/replication.R) does
#' @param day_min Minutes per simulation day (1440)
#' @return Tidy data frame, one row per (replication, sortie_day, pool) —
#'   pool one of "Critical (ICU, CCATT/CCAST)"/"Standard (Hold, CSU)" — with
#'   `capacity_added`, `seats_used`, and `outcome` ("Flown"/"Cancelled"/
#'   "Unknown"). Empty (zero rows) when AME scheduling is disabled (mirrors `run_once()`'s own disable condition).
#'
#' @details `seats_used` is derived, not directly observed, and is *not* the
#'   backlog waiting at the exact instant this sortie fires — a first-pass
#'   implementation used that instantaneous reading and, verified against a
#'   real seed-42 run, was consistently zero: `r2e_ame_wait_and_board()`
#'   (R/trajectories.R) lets a casualty seize freed capacity the moment it
#'   arrives, with no queueing step, so a sortie's newly added (or
#'   previously banked — see MODEL ASSUMPTION — AME Capacity Banking) seats
#'   are typically claimed by arrivals in the days *following* the sortie,
#'   not by anyone already queued at the sortie's own moment. `seats_used`
#'   is instead the number of new `seize()`s of that pool's resource between
#'   this sortie (inclusive of any instantaneous drain at its own moment)
#'   and the next scheduled sortie exclusive (or end of run for the last
#'   sortie) — read from the resource monitor's `server` column, which is
#'   monotonically non-decreasing for `"ame"`/`"ame_critical"` since a
#'   boarded casualty never releases that seize (see `build_ame_sortie_
#'   trajectory()`'s roxygen). This attributes banked-capacity boarding to
#'   whichever sortie's window it fell in, which is the closest available
#'   reading to "how many seats this sortie's contribution ultimately
#'   served" given capacity is fungible across sorties by design.
compute_ame_sorties <- function(resources, role4_params, n_days, day_min = DAY_MIN) {
  pool_levels <- c("Critical (ICU, CCATT/CCAST)", "Standard (Hold, CSU)")
  empty <- data.frame(replication = integer(0), sortie_day = numeric(0),
                      pool = character(0), capacity_added = numeric(0),
                      seats_used = numeric(0), outcome = character(0))

  ame_sched <- role4_params$ame
  if (is.null(ame_sched$schedule_interval_days) ||
      ame_sched$schedule_interval_days <= 0 ||
      ame_sched$schedule_interval_days > n_days) {
    return(empty)
  }

  sortie_times <- seq(ame_sched$schedule_interval_days * day_min, n_days * day_min,
                      by = ame_sched$schedule_interval_days * day_min)
  # Each sortie's "window" runs to the next sortie (exclusive), or to end of
  # run for the last one — see seats_used in the roxygen above.
  window_ends <- c(sortie_times[-1], n_days * day_min)

  airframe <- resolve_ame_airframe(role4_params)

  #' Read a step function's value at a time from resource monitor rows
  #'
  #' @param df Time-sorted resource monitor subset for one resource.
  #' @param value_col Name of the column to read.
  #' @param t Time to read the value at, in minutes.
  #' @param before Logical: read strictly before `t` rather than at it.
  #' @return The last recorded value at or before `t`, 0 when `df` is empty.
  #' @details `before = TRUE` reads the state an instant ahead of a change
  #'   recorded at exactly `t`, which is how the capacity level a sortie's own
  #'   bump is about to change, or a server count immediately before its own
  #'   instantaneous drain, is recovered from a monitor that records both at
  #'   the same timestamp.
  step_at <- function(df, value_col, t, before = FALSE) {
    if (nrow(df) == 0) return(0)
    query <- if (before) t - 1e-6 else t
    idx <- findInterval(query, df$time)
    if (idx < 1) return(0)
    df[[value_col]][idx]
  }

  reps <- sort(unique(resources$replication))

  bind_rows(lapply(reps, function(rep_id) {
    rep_res    <- resources %>% filter(replication == rep_id) %>% arrange(time)
    ame_res    <- rep_res %>% filter(resource == "ame")
    crit_res   <- rep_res %>% filter(resource == "ame_critical")

    bind_rows(lapply(seq_along(sortie_times), function(i) {
      t           <- sortie_times[i]
      window_end  <- window_ends[i]
      std_added  <- step_at(ame_res,  "capacity", t) - step_at(ame_res,  "capacity", t, before = TRUE)
      crit_added <- step_at(crit_res, "capacity", t) - step_at(crit_res, "capacity", t, before = TRUE)

      outcome <- "Cancelled"
      if (std_added > 0 || crit_added > 0) {
        flew <- isTRUE(all.equal(airframe$critical_capacity, crit_added)) &&
          isTRUE(all.equal(airframe$standard_capacity, std_added))
        outcome <- if (flew) "Flown" else "Unknown"
      }

      data.frame(
        replication    = rep_id,
        sortie_day     = t / day_min,
        pool           = pool_levels,
        capacity_added = c(crit_added, std_added),
        seats_used     = c(step_at(crit_res, "server", window_end) - step_at(crit_res, "server", t, before = TRUE),
                           step_at(ame_res,  "server", window_end) - step_at(ame_res,  "server", t, before = TRUE)),
        outcome        = outcome
      )
    }))
  })) %>%
    mutate(pool = factor(pool, levels = pool_levels))
}

#' Computes the strategic AME backlog (casualties simultaneously awaiting
#' AME sortie capacity) over time, by pool, from per-casualty event
#' timestamps rather than the `"ame"`/`"ame_critical"` resource monitor's
#' `queue` column (see `@details`)
#'
#' @param attributes_log Tidy long-format attribute monitor data frame
#'   (`analyse_run()`'s `attributes`/`analyse_replications()`'s
#'   `attributes_raw`); must include name, replication, key, value, time
#' @param n_days Simulation duration in days — the backlog step function for
#'   each (replication, pool) is extended flat to this point so a group
#'   whose last event occurred before the end of the run doesn't visually
#'   truncate early; NULL skips this extension (kept optional so callers
#'   without a reliable `n_days` still get a valid, if visually truncated,
#'   result rather than an error)
#' @param day_min Minutes per simulation day (1440)
#' @return Tidy data frame with columns replication, time (minutes), pool,
#'   backlog (integer, cumulative arrivals-at-AME-stage minus boardings) —
#'   one row per event, suitable for `geom_step()`. Empty (zero rows) when
#'   no casualty ever reached the Strategic Evac AME-wait stage.
#'
#' @details A waiting casualty never registers in simmer's own queue
#'   tracking for `"ame"`/`"ame_critical"`: `r2e_ame_wait_and_board()`
#'   (R/trajectories.R) uses a manual `timeout()` + `rollback()` polling
#'   loop (`r2e_ame_dow_poll()`) rather than `select()`/`seize_selected()` or a
#'   blocking `seize()`, calling `seize(resource_name, 1)` only once
#'   capacity is already confirmed available — so the resource monitor's
#'   `queue` column for these two resources is structurally always 0,
#'   confirmed empirically against a real seed-42 run with 93 casualties
#'   genuinely still awaiting AME at run end (every one of that run's 48
#'   `"ame"`/`"ame_critical"` resource-monitor rows recorded `queue = 0`). A
#'   first-pass implementation of this plot read that column directly and
#'   always rendered a flat zero line regardless of the true backlog. The
#'   true backlog is instead reconstructed here from `r2e_departure_time`
#'   (when the Strategic Evac disposition is decided and the AME wait
#'   begins — a `+1` backlog event, one per pool via `ame_route`) and
#'   `ame_hold_end` (when the casualty stops waiting, NA while it still is —
#'   a `-1` event), cumulatively summed in event-time order per
#'   (replication, pool).
#'
#'   The wait ends at `ame_hold_end` rather than at `ame_departure_time`
#'   because a casualty can leave the queue without boarding: one that dies
#'   during `r2e_ame_dow_poll()` sets no departure time, and taking the
#'   departure as the only exit left it in the backlog for the rest of the
#'   campaign. `ame_hold_end` is set on both routes out of the wait, so the
#'   two exits are counted alike. A casualty still queued for the staging bed
#'   it waits in has neither attribute and is correctly carried as waiting.
compute_ame_backlog <- function(attributes_log, n_days = NULL, day_min = DAY_MIN) {
  route_labels <- c("1" = "Critical (ICU, CCATT/CCAST)", "2" = "Standard (Hold, CSU)")
  pool_levels  <- unname(route_labels)
  empty <- data.frame(replication = integer(0), time = numeric(0),
                      pool = character(0), backlog = integer(0))

  relevant_keys <- c("r2e_departure_time", "ame_hold_end", "ame_route")
  relevant <- attributes_log %>%
    filter(key %in% relevant_keys) %>%
    dplyr::select(name, replication, key, value) %>%
    pivot_wider(id_cols = c(name, replication), names_from = key, values_from = value,
                values_fn = ~ dplyr::first(.x))

  if (nrow(relevant) == 0 || !all(c("r2e_departure_time", "ame_route") %in% names(relevant))) {
    return(empty)
  }
  if (!"ame_hold_end" %in% names(relevant)) relevant$ame_hold_end <- NA_real_

  waiting <- relevant %>%
    filter(!is.na(r2e_departure_time), !is.na(ame_route)) %>%
    mutate(pool = route_labels[as.character(ame_route)])

  if (nrow(waiting) == 0) return(empty)

  events <- bind_rows(
    waiting %>% transmute(replication, pool, time = r2e_departure_time, delta = 1L),
    waiting %>% filter(!is.na(ame_hold_end)) %>%
      transmute(replication, pool, time = ame_hold_end, delta = -1L)
  ) %>%
    arrange(replication, pool, time) %>%
    group_by(replication, pool) %>%
    mutate(backlog = cumsum(delta)) %>%
    ungroup() %>%
    dplyr::select(replication, time, pool, backlog)

  if (!is.null(n_days)) {
    end_time <- n_days * day_min
    # The last row at that instant, not the first: events are accumulated in
    # the order this frame already carries, so only the last of several
    # sharing an instant holds the level after all of them. A campaign whose
    # closing arrival and departure coincide, which is the common case when a
    # sortie clears the queue it just received, otherwise carries the
    # intermediate level to the window's end and reads one casualty high.
    terminal <- events %>%
      group_by(replication, pool) %>%
      filter(time == max(time)) %>%
      slice(n()) %>%
      ungroup() %>%
      mutate(time = end_time) %>%
      filter(time > 0)
    events <- bind_rows(events, terminal)
  }

  events %>%
    mutate(pool = factor(pool, levels = pool_levels)) %>%
    arrange(replication, pool, time)
}

#' Builds the strategic AME queue-depth (backlog) plot: number
#' of casualties simultaneously awaiting AME sortie capacity over time, by
#' pool
#'
#' @param backlog_data Output of `compute_ame_backlog()`
#' @return A ggplot object, or NULL if `backlog_data` has no rows (no
#'   casualty ever reached the Strategic Evac AME-wait stage)
#'
#' @details Y-axis scaling is data-driven per facet (`scales = "free_y"`,
#'   the pipeline's convention) rather than a fixed range, since the two pools'
#'   backlogs differ by an order of magnitude in typical scenarios.
plot_ame_queue <- function(backlog_data) {
  if (nrow(backlog_data) == 0) return(NULL)

  facet_formula <- if (n_distinct(backlog_data$replication) > 1) {
    ~ pool + replication
  } else {
    ~ pool
  }

  ggplot(backlog_data, aes(x = time / DAY_MIN, y = backlog)) +
    geom_step(color = "#D62828") +
    facet_wrap(facet_formula, ncol = if (identical(facet_formula, ~ pool)) 1 else NULL,
              scales = "free_y") +
    labs(
      title    = "Strategic AME Backlog Over Time",
      subtitle = "Casualties awaiting AME sortie capacity (occupying an R2E ICU or Hold bed) by route",
      x = "Simulation Day", y = "Casualties Awaiting AME"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
}

#' Builds the strategic AME sortie timeline plot: each
#' scheduled sortie opportunity's outcome — flown or cancelled, and
#' seats used against capacity — for both AME pools
#'
#' @param sortie_data Output of `compute_ame_sorties()`
#' @param airframe_label Name of the configured airframe
#'   (`resolve_ame_airframe()$label`), reported in the subtitle so the
#'   capacity the bars are measured against is attributable
#' @return A ggplot object, or NULL if `sortie_data` has no rows (AME
#'   scheduling disabled)
#'
#' @details Values are averaged across replications at each scheduled
#'   `sortie_day` (a fixed, schedule-determined x-axis shared by every
#'   replication) before plotting — in single-run mode this mean is simply
#'   the observed value, so the same code path serves both Quick Run and
#'   Full Analysis without a branch. The grey bar is mean capacity added
#'   (seats the sortie brought, 0 for a cancelled sortie); the coloured bar
#'   is mean seats boarded before the next scheduled sortie (see
#'   `compute_ame_sorties()`'s `seats_used` roxygen), coloured by the modal
#'   outcome across contributing replications at that sortie_day. Because capacity is additive and never expires (MODEL
#'   ASSUMPTION — AME Capacity Banking), the coloured bar can exceed the
#'   grey bar at a given sortie_day — that sortie's window drew on capacity
#'   banked from an earlier, under-subscribed sortie, not solely its own
#'   contribution. Y-axis scaling is data-driven per facet.
plot_ame_sortie <- function(sortie_data, airframe_label = NULL) {
  if (nrow(sortie_data) == 0) return(NULL)

  sortie_summary <- sortie_data %>%
    group_by(sortie_day, pool) %>%
    summarise(
      mean_capacity_added = mean(capacity_added),
      mean_seats_used     = mean(seats_used),
      modal_outcome       = names(sort(table(outcome), decreasing = TRUE))[1],
      .groups = "drop"
    )

  subtitle <- "Grey bar = mean seats added (capacity) per scheduled sortie; coloured bar = mean seats boarded before the next sortie"
  if (!is.null(airframe_label)) {
    subtitle <- paste0(subtitle, "\nAirframe: ", airframe_label)
  }

  ggplot(sortie_summary, aes(x = sortie_day)) +
    geom_col(aes(y = mean_capacity_added), fill = "grey80", width = 0.6) +
    geom_col(aes(y = mean_seats_used, fill = modal_outcome), width = 0.6) +
    facet_wrap(~ pool, ncol = 1, scales = "free_y") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(
      values = c("Flown" = "#2A9D8F", "Cancelled" = "grey50", "Unknown" = "grey30"),
      name = "Modal Sortie Outcome"
    ) +
    labs(
      title    = "Strategic AME Sortie Timeline",
      subtitle = subtitle,
      x = "Simulation Day", y = "Seats"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"), legend.position = "bottom")
}
