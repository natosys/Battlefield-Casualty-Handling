##############################################
## R/role4_demand.R                         ##
## National support base bed demand          ##
##############################################
#
# The comparative, policy and airlift tables each carry one Role 4 figure, the
# peak of the daily census. A peak alone cannot separate a short spike from a
# plateau of the same height, which commit very different numbers of beds, and
# says nothing of what the census is made of. This module reduces each
# replication to the daily census itself, divided by ward phase and by the
# origin of the casualty, together with the operating theatre demand owed
# alongside it, so that its size, its timing and its composition can each be
# measured across replications.
#
# The model gives the national support base no capacity, queue or shortfall
# (tests/checks/check_role4_ward_phases.R asserts the absence), so what is reduced
# here is demand: the beds a campaign commits, never a shortfall against a
# stated bed count. Each replication is reduced inside the forked worker that
# produced it, on R/airlift.R's arrangement. Depends on R/replication.R,
# R/environment.R and R/analysis.R (assign_role4_los, role4_ward_phases,
# compute_role4_surgical_demand); source those before this file.

source("R/constants.R")

#' Replications the Role 4 demand measurement runs at
#'
#' @details Thirty, the sustained-operations protocol's count, so the figures
#'   sit beside those of the other replicated experiments.
ROLE4_DEMAND_REPLICATIONS <- 30L

#' Campaign length the measurement runs over, in days
ROLE4_DEMAND_DAYS <- 360L

#' Closing window the sustained demand is measured over, in days
#'
#' @details Ninety, the window every other sustained-horizon response of this
#'   project is measured over, so a census figure is comparable with the pool
#'   queues beside it.
ROLE4_DEMAND_WINDOW_DAYS <- 90L

#' Control seed the measurement runs under
ROLE4_DEMAND_SEED <- 42L

#' Scenario profiles the measurement runs under
ROLE4_DEMAND_SCENARIOS <- c("moderate_intensity", "high_intensity")

#' Origins a Role 4 casualty is divided between, in display order
#'
#' @details Mutually exclusive and exhaustive. A casualty owed the staged
#'   soft-tissue reconstruction sequence is the reconstruction cohort whatever
#'   their injury type, because that sequence, not the injury, is what sets the
#'   length of their intensive care phase; of the rest, disease and non-battle
#'   injury is separated from battle injury by the arrival's `injury_type`.
ROLE4_ORIGINS <- c(battle_injury = "Battle injury", dnbi = "Disease and non-battle injury",
                   reconstruction = "Reconstruction cohort")

#' Injury type code of disease and non-battle injury
#'
#' @details As assigned at arrival (R/trajectories.R): 1 is wounded in action,
#'   2 is disease and non-battle injury, 3 is killed in action.
ROLE4_INJURY_DNBI <- 2

#' Label the series of the census total, which every composition sums to
ROLE4_TOTAL <- "Total"

#' Classify each Role 4 casualty by origin
#'
#' @param assigned Frame returned by assign_role4_los(), carrying
#'   `injury_type` and, where the configuration draws it,
#'   `reconstruction_required`.
#' @return Character vector of `ROLE4_ORIGINS` keys, one per row.
role4_origin <- function(assigned) {
  recon <- if ("reconstruction_required" %in% names(assigned)) {
    !is.na(assigned$reconstruction_required) & assigned$reconstruction_required == 1
  } else {
    rep(FALSE, nrow(assigned))
  }
  dnbi <- !is.na(assigned$injury_type) & assigned$injury_type == ROLE4_INJURY_DNBI
  ifelse(recon, "reconstruction", ifelse(dnbi, "dnbi", "battle_injury"))
}

#' Daily Role 4 census of a set of ward phases, by ward and by origin
#'
#' @param phases One row per casualty-phase, as role4_ward_phases() returns it,
#'   with the `origin` label added: `phase_ward`, `origin`, `phase_start` and
#'   `phase_end`, the last two in campaign days.
#' @param wards Ward levels the census reports.
#' @param n_days Campaign length in days.
#' @return Data frame of day, subject and occupancy, one row per subject and
#'   day from 1 to `n_days`: the total, each ward, and each origin.
#'
#' @details A phase occupies a bed on every day from its start to its end,
#'   inclusive, clipped to the campaign window: a stay running past the horizon
#'   is counted to the horizon and no further, the campaign having ended.
#'   Each phase is expanded once and counted three ways, so the total, the
#'   wards and the origins cannot disagree about who was there; the wards sum
#'   to the total, and so do the origins.
role4_census_from_phases <- function(phases, wards, n_days) {
  subjects <- c(ROLE4_TOTAL, wards, unname(ROLE4_ORIGINS))
  zero <- expand.grid(day = seq_len(n_days), subject = subjects, stringsAsFactors = FALSE)
  zero$occupancy <- 0L
  if (nrow(phases) == 0) return(zero[order(zero$subject, zero$day), ])

  start <- pmax(1L, as.integer(phases$phase_start))
  end <- pmin(as.integer(n_days), as.integer(phases$phase_end))
  keep <- !is.na(start) & !is.na(end) & start <= end
  lens <- end[keep] - start[keep] + 1L
  expanded <- data.frame(
    day = sequence(lens, from = start[keep]),
    ward = rep(phases$phase_ward[keep], lens),
    origin = rep(phases$origin[keep], lens),
    stringsAsFactors = FALSE
  )

  #' Occupancy of one grouping of the expanded bed-days
  #'
  #' @param labels Subject label of each expanded bed-day.
  #' @return Data frame of day, subject and occupancy over the groups present.
  count_by <- function(labels) {
    if (length(labels) == 0) return(zero[0, ])
    t <- as.data.frame(table(day = expanded$day, subject = labels), stringsAsFactors = FALSE)
    t$day <- as.integer(as.character(t$day))
    names(t)[names(t) == "Freq"] <- "occupancy"
    t[t$occupancy > 0, ]
  }
  counted <- rbind(count_by(rep(ROLE4_TOTAL, nrow(expanded))),
                   count_by(expanded$ward), count_by(expanded$origin))
  merged <- merge(zero[, c("day", "subject")], counted, by = c("day", "subject"), all.x = TRUE)
  merged$occupancy[is.na(merged$occupancy)] <- 0L
  merged[order(merged$subject, merged$day), ]
}

#' Daily Role 4 census by ward phase and by origin
#'
#' @param arrivals_log Per-casualty arrivals and attributes, as
#'   compute_role4_census() takes.
#' @param r4_params `env_data$vars$role4` list.
#' @param n_days Campaign length in days.
#' @return The census role4_census_from_phases() returns for the stays of the
#'   casualties who reached the national support base.
#'
#' @details The same census as compute_role4_census(), drawn from the same
#'   stays: the total here equals that function's daily sum when both are drawn
#'   under one seed, which tests/checks/check_role4_demand_protocol.R asserts.
#'   Admissions cease at the horizon, so the census can only fall after it and
#'   the peak cannot lie beyond it.
role4_census_detail <- function(arrivals_log, r4_params, n_days) {
  assigned <- assign_role4_los(arrivals_log, r4_params)
  phases <- role4_ward_phases(assigned, r4_params)
  if (nrow(phases) > 0) phases$origin <- unname(ROLE4_ORIGINS[role4_origin(phases)])
  role4_census_from_phases(phases, role4_ward_levels(r4_params), n_days)
}

#' Measures of the Role 4 load left when the campaign ends, in display order
#'
#' @details `census_end` is the census on the campaign's last day; the three
#'   `days_to_*` are the days after it until the census, counting only casualties
#'   admitted during the campaign, first falls to half, to a tenth and to none of
#'   that census; `bed_days_after` is the beds those casualties occupy after the
#'   last day, summed over days.
ROLE4_TAIL_MEASURES <- c("census_end", "days_to_half", "days_to_tenth", "days_to_empty",
                         "bed_days_after")

#' The Role 4 load left when the campaign ends, from the phases of its stays
#'
#' @param phases Ward phases as role4_ward_phases() returns them, with
#'   `phase_start` and `phase_end` in campaign days.
#' @param n_days Campaign length in days.
#' @return Named numeric vector of ROLE4_TAIL_MEASURES.
#'
#' @details Counts a phase on every day from its start to its end inclusive, as
#'   role4_census_from_phases() does, but without clipping it to the campaign
#'   window, so the stays still running at the horizon are followed until they
#'   end. Admissions cease at the horizon, so after it the census can only fall;
#'   the casualties still in theatre awaiting evacuation at the horizon are added
#'   by role4_backlog_tail_detail().
role4_census_tail <- function(phases, n_days) {
  out <- stats::setNames(numeric(length(ROLE4_TAIL_MEASURES)), ROLE4_TAIL_MEASURES)
  if (nrow(phases) == 0) return(out)
  start <- pmax(1L, as.integer(phases$phase_start))
  end <- as.integer(phases$phase_end)
  ok <- !is.na(start) & !is.na(end) & start <= end
  start <- start[ok]
  end <- end[ok]
  census_end <- sum(start <= n_days & end >= n_days)
  after_start <- pmax(start, n_days + 1L)
  running <- end >= after_start
  lens <- end[running] - after_start[running] + 1L
  out[["census_end"]] <- census_end
  out[["bed_days_after"]] <- sum(lens)
  if (census_end == 0 || !any(running)) return(out)
  days <- sequence(lens, from = after_start[running]) - n_days
  occupancy <- tabulate(days, nbins = max(days))
  #' Days after the horizon until the census first falls to a share of its last day's
  #'
  #' @param share Share of the last day's census the census must fall to.
  #' @return The first day after the horizon on which it does, the day after the
  #'   last occupied day where it never does before.
  days_to <- function(share) {
    hit <- which(occupancy <= share * census_end)
    if (length(hit) == 0) length(occupancy) + 1L else hit[1]
  }
  out[["days_to_half"]] <- days_to(0.5)
  out[["days_to_tenth"]] <- days_to(0.1)
  out[["days_to_empty"]] <- length(occupancy) + 1L
  out
}

#' The Role 4 load left when the campaign ends, from the casualties' arrivals
#'
#' @param arrivals_log Per-casualty arrivals and attributes, as
#'   compute_role4_census() takes.
#' @param r4_params `env_data$vars$role4` list.
#' @param n_days Campaign length in days.
#' @return The measures role4_census_tail() returns.
#'
#' @details Draws the same lengths of stay as role4_census_detail() when called
#'   from the same seed position, so the tail continues the census the campaign
#'   reported rather than a fresh draw of it.
role4_census_tail_detail <- function(arrivals_log, r4_params, n_days) {
  assigned <- assign_role4_los(arrivals_log, r4_params)
  role4_census_tail(role4_ward_phases(assigned, r4_params), n_days)
}

#' Measures of the casualties still awaiting strategic airlift when the campaign ends
#'
#' @details `backlog_end` is the casualties queued for an airlift place on the
#'   campaign's last day; `backlog_boarded` and `backlog_died` are those of them
#'   who boarded a later sortie and who died first; `days_to_clear` is the days
#'   after the last day until the last of them boarded or died, missing where a
#'   route never flies again. The `_all` measures repeat the census tail with the
#'   boarded casualties admitted to Role 4 on their sortie's day, and
#'   `census_peak_after_all` is that census's highest day after the horizon.
ROLE4_BACKLOG_MEASURES <- c("backlog_end", "backlog_boarded", "backlog_died", "days_to_clear",
                            "census_peak_after_all", "days_to_half_all", "days_to_tenth_all",
                            "days_to_empty_all", "bed_days_after_all")

#' Fly the airlift backlog out on the sorties that follow the campaign
#'
#' @param route Airlift route of each casualty waiting, 1 critical and 2 standard.
#' @param queued_at Time each joined the queue, in minutes; sets the boarding order.
#' @param death_time Time each would die waiting, in minutes, or NA for none.
#' @param n_days Campaign length in days.
#' @param interval_days Days between sorties.
#' @param places Places per sortie, critical then standard.
#' @param failure_probability Probability a sortie is cancelled.
#' @return List of `board_time` (NA for a casualty who never boards), `died`
#'   (logical) and `clear_time`, the minute the last casualty boarded or died,
#'   NA where some casualty never does.
#'
#' @details Continues the sortie schedule the campaign flew, every
#'   `interval_days` from the first sortie after the horizon, each route boarding
#'   in queue order up to its places, as build_ame_sortie_trajectory() does. A
#'   casualty whose death time comes before the sortie that would carry them dies
#'   instead and frees the place, as the queued casualty reneges in
#'   r2e_ame_queue_and_board(). The death time is the one the model drew when the
#'   casualty was staged, so the wait continues the campaign's rather than a fresh
#'   draw; a death time already past at the horizon, possible for a casualty still
#'   in pre-flight intensive care, takes effect at the first sortie. A
#'   cancellation draw is made only where cancellation is possible.
drain_airlift_backlog <- function(route, queued_at, death_time, n_days, interval_days,
                                  places, failure_probability) {
  n <- length(route)
  board <- rep(NA_real_, n)
  died <- rep(FALSE, n)
  if (n == 0) return(list(board_time = board, died = died, clear_time = n_days * DAY_MIN))
  sortie <- floor(n_days / interval_days) + 1
  stranded <- FALSE
  repeat {
    pending <- which(is.na(board) & !died)
    if (length(pending) == 0) break
    t <- sortie * interval_days * DAY_MIN
    dead <- pending[!is.na(death_time[pending]) & death_time[pending] <= t]
    died[dead] <- TRUE
    pending <- setdiff(pending, dead)
    if (length(pending) == 0) break
    if (failure_probability >= 1 || all(places[route[pending]] <= 0)) {
      at_risk <- pending[!is.na(death_time[pending])]
      died[at_risk] <- TRUE
      stranded <- length(at_risk) < length(pending)
      break
    }
    flies <- failure_probability <= 0 || stats::runif(1) >= failure_probability
    if (flies) {
      for (r in 1:2) {
        q <- pending[route[pending] == r]
        q <- q[order(queued_at[q])]
        board[utils::head(q, places[r])] <- t
      }
    }
    sortie <- sortie + 1
  }
  ends <- c(board[!is.na(board)], pmax(death_time[died], n_days * DAY_MIN), n_days * DAY_MIN)
  list(board_time = board, died = died, clear_time = if (stranded) NA_real_ else max(ends))
}

#' The airlift backlog at the horizon and the Role 4 load once it is flown out
#'
#' @param arrivals_log Per-casualty arrivals and attributes, as
#'   compute_role4_census() takes.
#' @param r4_params `env_data$vars$role4` list.
#' @param n_days Campaign length in days.
#' @return Named numeric vector of ROLE4_BACKLOG_MEASURES.
#'
#' @details A casualty is in the backlog where their evacuation was decided and
#'   a route assigned but no sortie had carried them by the horizon and they had
#'   neither died nor returned to duty while waiting. Casualties still in
#'   surgery or in a Role 2 bed at the horizon, their evacuation not yet
#'   decided, are not among them.
role4_backlog_tail_detail <- function(arrivals_log, r4_params, n_days) {
  #' One attribute column of the arrivals log, missing throughout where absent
  #'
  #' @param name Attribute name.
  #' @return The column, or NA for every casualty where the run never set it.
  col <- function(name) {
    if (name %in% names(arrivals_log)) arrivals_log[[name]] else rep(NA_real_, nrow(arrivals_log))
  }
  dead <- !is.na(col("dow")) & col("dow") == 1
  returned <- !is.na(col("ame_rtd")) & col("ame_rtd") == 1
  waiting <- which(!is.na(col("ame_route")) & is.na(col("evacuation_day")) &
                     !is.na(col("r2e_evac")) & col("r2e_evac") == 1 & !dead & !returned)
  airframe <- resolve_ame_airframe(r4_params)
  places <- c(as.numeric(airframe$critical_capacity), as.numeric(airframe$standard_capacity))
  failure <- as.numeric(r4_params$ame$failure_probability)
  if (length(failure) == 0 || is.na(failure)) failure <- 0
  drained <- drain_airlift_backlog(
    route = col("ame_route")[waiting], queued_at = col("r2e_departure_time")[waiting],
    death_time = col("ame_death_time")[waiting], n_days = n_days,
    interval_days = as.numeric(r4_params$ame$schedule_interval_days), places = places,
    failure_probability = failure
  )
  boarded <- !is.na(drained$board_time)
  flown <- arrivals_log
  flown$evacuation_day[waiting[boarded]] <- floor(drained$board_time[boarded] / DAY_MIN) + 1
  phases <- role4_ward_phases(assign_role4_los(flown, r4_params), r4_params)
  all <- role4_census_tail(phases, n_days)
  out <- stats::setNames(numeric(length(ROLE4_BACKLOG_MEASURES)), ROLE4_BACKLOG_MEASURES)
  out[["backlog_end"]] <- length(waiting)
  out[["backlog_boarded"]] <- sum(boarded)
  out[["backlog_died"]] <- sum(drained$died)
  out[["days_to_clear"]] <- ceiling(drained$clear_time / DAY_MIN) - n_days
  out[["census_peak_after_all"]] <- role4_census_peak_after(phases, n_days)
  out[c("days_to_half_all", "days_to_tenth_all", "days_to_empty_all", "bed_days_after_all")] <-
    all[c("days_to_half", "days_to_tenth", "days_to_empty", "bed_days_after")]
  out
}

#' Measures of the convalescent evacuees who returned to duty awaiting airlift
#'
#' `convalescent_evacuees` counts the casualties evacuated for recovery beyond
#' the theatre evacuation policy (`evacuation_reason` 1);
#' `returned_awaiting_airlift` those of them whose convalescence ended before a
#' sortie carried them, who returned to duty from R2E (`ame_rtd`);
#' `returned_days_beyond_policy` the convalescence those returned served beyond
#' the policy, summed over them, in days; and `returned_days_held` the time from
#' their evacuation decision to their return, summed over them, in days.
ROLE4_RETURNED_MEASURES <- c("convalescent_evacuees", "returned_awaiting_airlift",
                             "returned_days_beyond_policy", "returned_days_held")

#' Count the convalescent evacuees who returned to duty while awaiting airlift
#'
#' @param arrivals_log Per-casualty arrivals and attributes of one replication.
#' @param policy_days The theatre evacuation policy, in days.
#' @return Named numeric vector of ROLE4_RETURNED_MEASURES.
#'
#' @details Each of these casualties is load the national support base never
#'   receives, so the measures size what the backlog removes from Role 4 rather
#'   than delays. The days beyond the policy are the drawn recovery less the
#'   policy, the convalescence the policy would have served at Role 4; the days
#'   held run from the evacuation decision (`r2e_departure_time`) to the return
#'   (`ame_hold_end`), the time spent holding a staging place while waiting.
#'   Draws no random number.
role4_returned_detail <- function(arrivals_log, policy_days) {
  #' One attribute column of the arrivals log, missing throughout where absent
  #'
  #' @param name Attribute name.
  #' @return The column, or NA for every casualty where the run never set it.
  col <- function(name) {
    if (name %in% names(arrivals_log)) arrivals_log[[name]] else rep(NA_real_, nrow(arrivals_log))
  }
  reason <- col("evacuation_reason")
  returned <- which(!is.na(col("ame_rtd")) & col("ame_rtd") == 1)
  out <- stats::setNames(numeric(length(ROLE4_RETURNED_MEASURES)), ROLE4_RETURNED_MEASURES)
  out[["convalescent_evacuees"]] <- sum(!is.na(reason) & reason == 1)
  out[["returned_awaiting_airlift"]] <- length(returned)
  out[["returned_days_beyond_policy"]] <- sum(col("recovery_to_duty_days")[returned] - policy_days)
  out[["returned_days_held"]] <-
    sum(col("ame_hold_end")[returned] - col("r2e_departure_time")[returned]) / DAY_MIN
  out
}

#' The highest Role 4 census on any day after the horizon
#'
#' @param phases Ward phases as role4_ward_phases() returns them.
#' @param n_days Campaign length in days.
#' @return The highest count of phases covering one day after `n_days`, 0 where
#'   none runs past it.
role4_census_peak_after <- function(phases, n_days) {
  if (nrow(phases) == 0) return(0)
  start <- pmax(as.integer(phases$phase_start), n_days + 1L)
  end <- as.integer(phases$phase_end)
  ok <- !is.na(start) & !is.na(end) & start <= end
  if (!any(ok)) return(0)
  days <- sequence(end[ok] - start[ok] + 1L, from = start[ok]) - n_days
  max(tabulate(days))
}

#' Sources of the operations owed at Role 4, in display order
#'
#' @details A definitive repair is the operation a casualty released with it
#'   outstanding carries rearward; a debridement is a return to theatre in the
#'   reconstruction sequence before the wound is closed; the reconstruction is
#'   the operation that ends that sequence.
ROLE4_OPERATION_SOURCES <- c(repair = "Definitive repair", debridement = "Debridement",
                             reconstruction = "Reconstruction")

#' Every operation owed at Role 4, one row each, with its source
#'
#' @param arrivals_log Per-casualty arrivals and attributes.
#' @param r4_params `env_data$vars$role4` list.
#' @return Data frame of day, source and theatre_minutes, one row per operation;
#'   zero rows where the configuration reports no theatre demand.
#'
#' @details Rebuilds the demand compute_role4_surgical_demand() reports from the
#'   same two draws in the same order, so the operations here are that
#'   function's own and merely carry the source it discards;
#'   tests/checks/check_role4_demand_protocol.R asserts that they sum to its total.
#'   The minutes are the conserved definitive repairs alone, no open-access
#'   source reporting how long a debridement or a flap takes.
role4_operation_events <- function(arrivals_log, r4_params) {
  empty <- data.frame(day = numeric(0), source = character(0), theatre_minutes = numeric(0))
  needed <- c("definitive_repair_outstanding", "definitive_repair_minutes")
  if (!role4_surgery_enabled(r4_params) || !all(needed %in% names(arrivals_log))) {
    return(empty)
  }
  assigned <- assign_role4_los(arrivals_log, r4_params)
  if (nrow(assigned) == 0) return(empty)

  released <- assigned[!is.na(assigned$definitive_repair_outstanding) &
                         assigned$definitive_repair_outstanding == 1, ]
  repairs <- data.frame(
    day = released$r4_admit_day, source = rep("repair", nrow(released)),
    theatre_minutes = ifelse(is.na(released$definitive_repair_minutes), 0,
                             released$definitive_repair_minutes),
    stringsAsFactors = FALSE
  )
  sequence <- with_preserved_rng(role4_reconstruction_sequence(assigned, r4_params))
  staged <- data.frame(day = sequence$day, source = sequence$procedure,
                       theatre_minutes = rep(0, nrow(sequence)), stringsAsFactors = FALSE)
  rbind(repairs, staged)
}

#' Role 4 operating theatre demand owed alongside the census
#'
#' @param arrivals_log Per-casualty arrivals and attributes.
#' @param r4_params `env_data$vars$role4` list.
#' @param n_days Campaign length in days.
#' @return List of `daily`, a data frame of day, source, operations and
#'   theatre_minutes with one row per source and day from 1 to `n_days` and zero
#'   on a day with nothing owed, and `after_horizon`, the operations owed after
#'   the last day.
#'
#' @details The reconstruction sequence of a casualty admitted late in the
#'   campaign runs on past its end, so the operations owed by the casualties a
#'   campaign admitted are not all owed inside it. They are counted separately
#'   rather than clamped into the last day or dropped, so that the total is the
#'   one R/policy_sweep.R reports and the daily series still ends on the
#'   campaign's last day. Every definitive repair is owed on the day of
#'   admission, so none falls beyond the horizon.
role4_operations_detail <- function(arrivals_log, r4_params, n_days) {
  events <- role4_operation_events(arrivals_log, r4_params)
  out <- expand.grid(day = seq_len(n_days), source = names(ROLE4_OPERATION_SOURCES),
                     stringsAsFactors = FALSE)
  out$operations <- 0
  out$theatre_minutes <- 0
  inside <- events[events$day >= 1 & events$day <= n_days, ]
  for (i in seq_len(nrow(inside))) {
    hit <- which(out$day == inside$day[i] & out$source == inside$source[i])
    out$operations[hit] <- out$operations[hit] + 1
    out$theatre_minutes[hit] <- out$theatre_minutes[hit] + inside$theatre_minutes[i]
  }
  list(daily = out, after_horizon = sum(events$day > n_days))
}

#' Reduce one replication's per-casualty data to the daily demand series
#'
#' @param wide Per-casualty arrivals and attributes of one replication, as
#'   reduce_airlift_replication() joins them.
#' @param r4_params `env_data$vars$role4` list.
#' @param n_days Campaign length in days.
#' @param seed The replication's seed, under which the length-of-stay draw is
#'   taken.
#' @param policy_days The theatre evacuation policy, in days, against which
#'   role4_returned_detail() measures the convalescence served in theatre.
#' @return Data frame of day, series, subject and value in long form: the
#'   `census` of each subject, the `operations` owed each day by source and in
#'   all, the `theatre_minutes` owed each day, and two single-row series on the last day: the
#'   `operations_after_horizon` owed after it and the `operations_admitted`
#'   owed in all by the casualties the campaign admitted, and the `census_tail`,
#'   one row per measure of role4_census_tail(), role4_backlog_tail_detail()
#'   and role4_returned_detail() on the last day.
#'
#' @details The census and the demand are each drawn under the replication's
#'   own seed, from the same position, so the two rest on one length-of-stay
#'   draw and the day an operation is owed on is a day the casualty is in a
#'   bed (R/policy_sweep.R does the same). The caller's stream is restored, so
#'   the reduction is a function of the seed and nothing else.
reduce_role4_demand <- function(wide, r4_params, n_days, seed, policy_days) {
  census <- with_preserved_rng({
    set.seed(seed)
    role4_census_detail(wide, r4_params, n_days)
  })
  ops <- with_preserved_rng({
    set.seed(seed)
    role4_operations_detail(wide, r4_params, n_days)
  })
  tail <- with_preserved_rng({
    set.seed(seed)
    role4_census_tail_detail(wide, r4_params, n_days)
  })
  backlog <- with_preserved_rng({
    set.seed(seed)
    role4_backlog_tail_detail(wide, r4_params, n_days)
  })
  returned <- role4_returned_detail(wide, policy_days)
  daily <- ops$daily
  label <- unname(ROLE4_OPERATION_SOURCES[daily$source])
  per_day <- aggregate(cbind(operations, theatre_minutes) ~ day, data = daily, FUN = sum)
  rbind(
    data.frame(day = census$day, series = "census", subject = census$subject,
               value = census$occupancy, stringsAsFactors = FALSE),
    data.frame(day = daily$day, series = "operations", subject = label,
               value = daily$operations, stringsAsFactors = FALSE),
    data.frame(day = per_day$day, series = "operations", subject = ROLE4_TOTAL,
               value = per_day$operations, stringsAsFactors = FALSE),
    data.frame(day = per_day$day, series = "theatre_minutes", subject = ROLE4_TOTAL,
               value = per_day$theatre_minutes, stringsAsFactors = FALSE),
    data.frame(day = n_days, series = "operations_after_horizon", subject = ROLE4_TOTAL,
               value = ops$after_horizon, stringsAsFactors = FALSE),
    data.frame(day = n_days, series = "operations_admitted", subject = ROLE4_TOTAL,
               value = sum(daily$operations) + ops$after_horizon, stringsAsFactors = FALSE),
    data.frame(day = n_days, series = "census_tail", subject = names(tail),
               value = unname(tail), stringsAsFactors = FALSE),
    data.frame(day = n_days, series = "census_tail", subject = names(backlog),
               value = unname(backlog), stringsAsFactors = FALSE),
    data.frame(day = n_days, series = "census_tail", subject = names(returned),
               value = unname(returned), stringsAsFactors = FALSE)
  )
}

#' Reduce one finished replication to the daily demand series
#'
#' @param env Wrapped simmer environment as returned by run_once().
#' @param n_days Campaign length in days.
#' @param seed The replication's seed.
#' @return The series reduce_role4_demand() returns.
#'
#' @details The same join reduce_airlift_replication() builds, duplicates of a
#'   casualty still inside a timeout when the run ended removed so that no
#'   casualty is counted twice.
reduce_role4_demand_replication <- function(env, n_days, seed) {
  arrivals <- simmer::get_mon_arrivals(env, ongoing = TRUE)
  attributes <- simmer::get_mon_attributes(env)
  arrivals <- arrivals[!duplicated(arrivals[, c("name", "replication")]), ]
  wide <- build_attributes_wide(attributes, arrivals) %>%
    dplyr::right_join(arrivals, by = c("name", "replication"),
                      suffix = c("", "_arrival"))
  reduce_role4_demand(wide, env_data$vars$role4, n_days, seed,
                      env_data$vars$r2eheavy$recovery$evacuation_policy_days)
}

#' Run the demand measurement across replications under the bound configuration
#'
#' @param n_iterations Replications to run (default ROLE4_DEMAND_REPLICATIONS).
#' @param n_days Campaign length in days (default ROLE4_DEMAND_DAYS).
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @param checkpoint_dir Directory each finished replication is saved to and
#'   read back from (checkpoint_replication()), or NULL for none.
#' @return Data frame of replication, day, series, subject and value.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed, and the caller's stream is restored on exit. Each
#'   replication is reduced inside the worker that produced it.
run_role4_demand <- function(n_iterations = ROLE4_DEMAND_REPLICATIONS,
                             n_days = ROLE4_DEMAND_DAYS, max_cores = NULL,
                             checkpoint_dir = NULL) {
  rng_state <- capture_rng_state()
  on.exit(restore_rng_state(rng_state), add = TRUE)

  rep_seeds <- sample.int(.Machine$integer.max, n_iterations)
  RNGkind("L'Ecuyer-CMRG")

  #' Run one replication and return its reduced series alone
  #'
  #' @param i Index of the replication, into `rep_seeds`.
  #' @return The replication's daily series, carrying its index.
  worker <- function(i) {
    checkpoint_replication(function() {
      env <- run_once(n_days, seed = rep_seeds[i], write_files = FALSE)
      out <- reduce_role4_demand_replication(env, n_days, seed = rep_seeds[i])
      out$replication <- i
      out
    }, checkpoint_dir, i, rep_seeds[i], n_days)
  }

  dispatched <- dispatch_replications(worker, n_iterations, max_cores)
  usable <- vapply(dispatched, is.data.frame, logical(1))
  if (!all(usable)) {
    stop(sprintf("%d of %d replications did not complete", sum(!usable), n_iterations),
         call. = FALSE)
  }
  do.call(rbind, dispatched)
}

#' Reduce the daily series to one row of responses per replication and subject
#'
#' @param series Daily series as returned by run_role4_demand(), carrying
#'   replication, day, series, subject and value, and a scenario column where
#'   more than one is bound.
#' @param n_days Campaign length in days.
#' @param window_days Closing window, in days.
#' @return Data frame of scenario, replication, series, subject, mean, peak,
#'   closing_mean and total. For the census the three are the mean, the
#'   maximum and the closing-window mean of the daily occupancy; for the
#'   operations series `total` is the sum over the campaign and `peak` the
#'   busiest day.
#'
#' @details The mean is taken over every day of the campaign, the opening days
#'   in which the census is still filling included, because a planner commits
#'   beds against the whole campaign; the closing-window mean is the figure
#'   that reads as sustained once the opening has passed.
role4_demand_responses <- function(series, n_days = ROLE4_DEMAND_DAYS,
                                   window_days = ROLE4_DEMAND_WINDOW_DAYS) {
  if (!"scenario" %in% names(series)) series$scenario <- "default"
  series$closing <- series$day > n_days - window_days
  keys <- c("scenario", "replication", "series", "subject")
  #' Mean of the daily value for each replication, series and subject
  #'
  #' @param d Daily series, or the closing window of it.
  #' @return Data frame of scenario, replication, series, subject and value.
  mean_of <- function(d) aggregate(value ~ scenario + replication + series + subject, data = d,
                                   FUN = mean)
  peak <- aggregate(value ~ scenario + replication + series + subject, data = series, FUN = max)
  total <- aggregate(value ~ scenario + replication + series + subject, data = series, FUN = sum)
  avg <- mean_of(series)
  closing <- mean_of(series[series$closing, ])
  out <- merge(merge(avg, peak, by = keys, suffixes = c("_mean", "_peak")),
               merge(closing, total, by = keys, suffixes = c("_closing", "_total")), by = keys)
  names(out) <- sub("^value_", "", names(out))
  names(out)[names(out) == "closing"] <- "closing_mean"
  out[order(out$scenario, out$series, out$subject, out$replication), ]
}

#' Mean and 95% interval of each response across replications
#'
#' @param responses Per-replication responses as returned by
#'   role4_demand_responses().
#' @return Data frame of scenario, series, subject, response, n_reps, mean,
#'   ci_lower and ci_upper, one row per scenario, series, subject and response.
#'
#' @details The Student t interval about the mean of the replications, the
#'   construction every other replicated experiment here uses.
summarise_role4_demand <- function(responses) {
  cols <- c("mean", "peak", "closing_mean", "total")
  groups <- unique(responses[, c("scenario", "series", "subject")])
  do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
    g <- groups[i, ]
    d <- responses[responses$scenario == g$scenario & responses$series == g$series &
                     responses$subject == g$subject, ]
    do.call(rbind, lapply(cols, function(cn) {
      x <- d[[cn]]
      n <- length(x)
      m <- mean(x)
      e <- if (n > 1) qt(0.975, df = n - 1) * sd(x) / sqrt(n) else 0
      data.frame(g, response = cn, n_reps = n, mean = m, ci_lower = m - e, ci_upper = m + e,
                 row.names = NULL)
    }))
  }))
}

#' Cross-replication mean of each subject's daily census, by scenario
#'
#' @param series Daily series as returned by run_role4_demand().
#' @return Data frame of scenario, subject, day, n_reps, mean, ci_lower and
#'   ci_upper, one row per census subject and day, for the figure of the census
#'   over time.
role4_census_daily <- function(series) {
  census <- series[series$series == "census", ]
  stats <- aggregate(value ~ scenario + subject + day, data = census, FUN = function(x) {
    n <- length(x)
    m <- mean(x)
    e <- if (n > 1) qt(0.975, df = n - 1) * sd(x) / sqrt(n) else 0
    c(n = n, mean = m, lower = m - e, upper = m + e)
  })
  out <- data.frame(stats[, c("scenario", "subject", "day")], stats$value)
  names(out) <- c("scenario", "subject", "day", "n_reps", "mean", "ci_lower", "ci_upper")
  out[order(out$scenario, out$subject, out$day), ]
}

#' Days in the block the operations owed are averaged over for the figure
#'
#' @details Seven, the interval between scheduled sorties, so that a block holds
#'   one whole cycle of the evacuation schedule and the figure does not trade
#'   the weekly rhythm of admissions for noise.
ROLE4_OPERATIONS_BLOCK_DAYS <- 7L

#' Cross-replication mean operations owed per day, by source and week
#'
#' @param series Daily series as returned by run_role4_demand(), with a
#'   `scenario` column.
#' @return Data frame of scenario, subject, block_start_day, n_reps, mean,
#'   ci_lower and ci_upper: the mean operations owed per day over each whole
#'   block of ROLE4_OPERATIONS_BLOCK_DAYS days, with its 95% interval across
#'   replications, for each source and for the total.
#'
#' @details A trailing partial block is dropped rather than averaged over fewer
#'   days, which would give it more weight than the days it holds.
role4_operations_weekly <- function(series) {
  ops <- series[series$series == "operations", ]
  ops$block <- (ops$day - 1L) %/% ROLE4_OPERATIONS_BLOCK_DAYS
  full <- tapply(ops$day, ops$block, function(d) length(unique(d)))
  ops <- ops[ops$block %in% as.integer(names(full)[full == ROLE4_OPERATIONS_BLOCK_DAYS]), ]
  per_rep <- aggregate(value ~ scenario + replication + subject + block, data = ops, FUN = mean)
  stats <- aggregate(value ~ scenario + subject + block, data = per_rep, FUN = function(x) {
    n <- length(x)
    m <- mean(x)
    e <- if (n > 1) qt(0.975, df = n - 1) * sd(x) / sqrt(n) else 0
    c(n = n, mean = m, lower = m - e, upper = m + e)
  })
  out <- data.frame(stats[, c("scenario", "subject")],
                    block_start_day = stats$block * ROLE4_OPERATIONS_BLOCK_DAYS + 1L,
                    stats$value)
  names(out) <- c("scenario", "subject", "block_start_day", "n_reps", "mean", "ci_lower",
                  "ci_upper")
  out[order(out$scenario, out$subject, out$block_start_day), ]
}
