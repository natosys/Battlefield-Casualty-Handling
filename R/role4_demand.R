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
# (scripts/check_role4_ward_phases.R asserts the absence), so what is reduced
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
#'   under one seed, which scripts/check_role4_demand_protocol.R asserts.
#'   Admissions cease at the horizon, so the census can only fall after it and
#'   the peak cannot lie beyond it.
role4_census_detail <- function(arrivals_log, r4_params, n_days) {
  assigned <- assign_role4_los(arrivals_log, r4_params)
  phases <- role4_ward_phases(assigned, r4_params)
  if (nrow(phases) > 0) phases$origin <- unname(ROLE4_ORIGINS[role4_origin(phases)])
  role4_census_from_phases(phases, role4_ward_levels(r4_params), n_days)
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
#'   scripts/check_role4_demand_protocol.R asserts that they sum to its total.
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
#' @return Data frame of day, series, subject and value in long form: the
#'   `census` of each subject, the `operations` owed each day by source and in
#'   all, the `theatre_minutes` owed each day, and two single-row series on the last day: the
#'   `operations_after_horizon` owed after it and the `operations_admitted`
#'   owed in all by the casualties the campaign admitted.
#'
#' @details The census and the demand are each drawn under the replication's
#'   own seed, from the same position, so the two rest on one length-of-stay
#'   draw and the day an operation is owed on is a day the casualty is in a
#'   bed (R/policy_sweep.R does the same). The caller's stream is restored, so
#'   the reduction is a function of the seed and nothing else.
reduce_role4_demand <- function(wide, r4_params, n_days, seed) {
  census <- with_preserved_rng({
    set.seed(seed)
    role4_census_detail(wide, r4_params, n_days)
  })
  ops <- with_preserved_rng({
    set.seed(seed)
    role4_operations_detail(wide, r4_params, n_days)
  })
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
               value = sum(daily$operations) + ops$after_horizon, stringsAsFactors = FALSE)
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
  reduce_role4_demand(wide, env_data$vars$role4, n_days, seed)
}

#' Run the demand measurement across replications under the bound configuration
#'
#' @param n_iterations Replications to run (default ROLE4_DEMAND_REPLICATIONS).
#' @param n_days Campaign length in days (default ROLE4_DEMAND_DAYS).
#' @param max_cores Cap on concurrent forks, or NULL for the machine's cores.
#' @return Data frame of replication, day, series, subject and value.
#'
#' @details Seeds are drawn as `run_replications()` draws them, from the
#'   caller's control seed, and the caller's stream is restored on exit. Each
#'   replication is reduced inside the worker that produced it.
run_role4_demand <- function(n_iterations = ROLE4_DEMAND_REPLICATIONS,
                             n_days = ROLE4_DEMAND_DAYS, max_cores = NULL) {
  rng_state <- capture_rng_state()
  on.exit(restore_rng_state(rng_state), add = TRUE)

  rep_seeds <- sample.int(.Machine$integer.max, n_iterations)
  RNGkind("L'Ecuyer-CMRG")

  #' Run one replication and return its reduced series alone
  #'
  #' @param i Index of the replication, into `rep_seeds`.
  #' @return The replication's daily series, carrying its index.
  worker <- function(i) {
    env <- run_once(n_days, seed = rep_seeds[i], write_files = FALSE)
    out <- reduce_role4_demand_replication(env, n_days, seed = rep_seeds[i])
    out$replication <- i
    out
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
