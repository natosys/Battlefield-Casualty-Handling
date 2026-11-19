##############################################################################
## R/r2b_theatre_sweep.R                                                    ##
## A second R2B operating theatre, crossed with the R2B surgical sections   ##
##############################################################################
#
# The surgical team sweep (R/surgical_team_sweep.R) adds a second R2B surgical
# section per unit at the shipped single theatre, which ends the off-shift
# diversions to R2E but roughly doubles the diversions because the theatre is
# busy. This module adds the theatre: one or two per unit
# (elms.r2b.beds.ot.qty), with one, two or four sections per unit
# (elms.r2b.sub_elms.surg.qty), R2E held at its shipped establishment.
#
# build_env() gives a unit's sections alternate shifts, so two sections per unit
# put one on each shift and a second theatre then gives a casualty a room but no
# second team: r2b_ot_check_path() admits a casualty who finds a free theatre
# while the section on shift is operating, and the casualty waits in that
# theatre for the section rather than being sent on to R2E. Four sections put
# two on each shift, at which a second theatre is a second concurrent
# operation. The arms separate the two, each adding one step to the one before:
# a second section (coverage of the other shift), a second theatre (a room to
# wait in), then two more sections (a second operation at once). Four sections
# at one theatre is not an arm: with one room the second section on shift has
# nowhere to operate, and a smoke run reproduced two sections at one theatre
# exactly.
#
# A theatre is a room the unit's surgery path selects from its theatre pool,
# whose capacity build_env() sets from the theatre count, so setting the count
# on the resolved configuration reaches the model with no trajectory change.
#
# Each replication is reduced inside the forked worker that produced it by
# reduce_ot_bed_replication() and the R2B theatres' occupancy over the closing
# window, so every response of the theatre sweep is reported alongside the
# forward one. No R2B queue is reported: a single section seizes its members as
# separate resources and several sections share a pool, so no one resource
# pattern reads the wait for a team at every arm, and the wait shows instead as
# the diversions it replaces. The arm at two sections
# and one theatre is the surgical team sweep's "R2E 3, R2B 2" configuration and
# the shipped arm its shipped one, under the same seed, so both reproduce that
# sweep's replications.
#
# Depends on R/analysis.R, R/policy_sweep.R, R/hold_window.R, R/long_horizon.R,
# R/surgical_team_sweep.R and R/ot_bed_sweep.R; source those first.

#' Replications per arm
R2B_THEATRE_SWEEP_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
R2B_THEATRE_SWEEP_DAYS <- 360L

#' Control seed every arm is set to before it runs
R2B_THEATRE_SWEEP_SEED <- 42L

#' Closing window the queue and occupancy responses are measured over, in days
R2B_THEATRE_SWEEP_WINDOW_DAYS <- 90L

#' Arm label of an R2B section and theatre establishment, per unit
#'
#' @param sections R2B surgical sections per unit.
#' @param theatres R2B operating theatres per unit.
#' @return The label, such as "R2B 2 sections, 2 theatres".
r2b_theatre_arm_label <- function(sections, theatres) {
  sprintf("R2B %d %s, %d %s", sections, if (sections == 1L) "section" else "sections",
          theatres, if (theatres == 1L) "theatre" else "theatres")
}

#' The arms swept, as an R2B section and theatre count per unit
#'
#' @details The shipped establishment first, then one step at a time, the
#'   order the results table reads them in. `on_shift` is the larger shift's
#'   section count, the one that meets the theatres.
R2B_THEATRE_SWEEP_ARMS <- local({
  sections <- c(1L, 2L, 2L, 4L)
  theatres <- c(1L, 1L, 2L, 2L)
  data.frame(arm = mapply(r2b_theatre_arm_label, sections, theatres),
             sections = sections, theatres = theatres,
             on_shift = as.integer(ceiling(sections / 2)), stringsAsFactors = FALSE)
})

#' Arm of the shipped establishment
R2B_THEATRE_SWEEP_BASELINE_ARM <- r2b_theatre_arm_label(1L, 1L)

#' The paired comparisons reported, as a from and to arm
#'
#' @details Every arm against the shipped establishment, which prices the whole
#'   change, and each step against the arm before it: the second theatre at two
#'   sections, which isolates the room, and four sections against two at two
#'   theatres, which isolates the second operation at once. Holm's adjustment is
#'   taken over every row of the file.
R2B_THEATRE_SWEEP_COMPARISONS <- rbind(
  data.frame(from = R2B_THEATRE_SWEEP_BASELINE_ARM,
             to = setdiff(R2B_THEATRE_SWEEP_ARMS$arm, R2B_THEATRE_SWEEP_BASELINE_ARM),
             stringsAsFactors = FALSE),
  data.frame(from = c(r2b_theatre_arm_label(2L, 1L), r2b_theatre_arm_label(2L, 2L)),
             to = c(r2b_theatre_arm_label(2L, 2L), r2b_theatre_arm_label(4L, 2L)),
             stringsAsFactors = FALSE)
)

#' Responses the paired comparisons report
#'
#' @details The off-shift diversions and the pre-open hold are not among them:
#'   both are zero by construction at two or more sections, a section being on
#'   shift at every hour, so a comparison between two such arms has no variance
#'   to form an interval from. The summary reports both at every arm.
R2B_THEATRE_SWEEP_PAIRED_RESPONSES <- c(
  "r2b_surgeries", "diverted_busy", "r2b_theatre_occupancy", "theatre_mean_queue",
  "r2e_first_surgeries", "released_unrepaired", "overflow_r2e_post_op", "total_dow",
  "total_rtd"
)

#' Half-width each paired response is sized against, in its own units
#'
#' @details As the theatre sweep chooses them: ten casualties in a count of
#'   operations, diversions or holds, a percentage point on an occupancy, one
#'   casualty in a queue or overflow census, one death and ten returns to duty.
R2B_THEATRE_SWEEP_HALF_WIDTHS <- c(
  r2b_surgeries = 10, diverted_busy = 10, r2b_theatre_occupancy = 0.01,
  theatre_mean_queue = 1, r2e_first_surgeries = 10, released_unrepaired = 10,
  overflow_r2e_post_op = 1, total_dow = 1, total_rtd = 10
)

#' Set the R2B operating theatre count per unit on a resolved configuration
#'
#' @param resolved Configuration as `resolve_scenario()` returns it.
#' @param theatres Number of operating theatres to establish at each R2B unit.
#' @return The configuration with that count set.
#'
#' @details Fails rather than returning the configuration unchanged where the
#'   R2B element or its theatre pool cannot be found, so a sweep cannot silently
#'   measure the shipped count at every arm. The element is located by name
#'   rather than by position, and the count applies to every R2B unit.
set_r2b_ot_establishment <- function(resolved, theatres) {
  if (!is.numeric(theatres) || length(theatres) != 1 || is.na(theatres) ||
        theatres < 1 || theatres != round(theatres)) {
    stop("theatres must be a single whole count of at least 1, found '",
         paste(format(theatres), collapse = ", "), "'", call. = FALSE)
  }
  found <- FALSE
  for (i in seq_along(resolved$elms)) {
    if (!identical(resolved$elms[[i]]$elm, "r2b")) next
    for (j in seq_along(resolved$elms[[i]]$beds)) {
      if (!identical(resolved$elms[[i]]$beds[[j]]$name, "ot")) next
      resolved$elms[[i]]$beds[[j]]$qty <- as.integer(theatres)
      found <- TRUE
    }
  }
  if (!found) {
    stop("no R2B operating theatre pool found in elms, so the theatre count ",
         "cannot be swept", call. = FALSE)
  }
  resolved
}

#' Enter a scenario's configuration with the R2B sections and theatres overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param arm One row of R2B_THEATRE_SWEEP_ARMS.
#' @return Invisibly, the described configuration that was applied.
#'
#' @details Both counts are structure in `elms`, so they are set on the resolved
#'   form before `build_environment()` constructs the resources from them. R2E
#'   keeps its shipped sections and theatres.
apply_r2b_theatre_setting <- function(json_data, scenario, arm) {
  resolved <- resolve_scenario(json_data, scenario)
  resolved <- set_surgical_sections(resolved, "r2b", arm$sections)
  resolved <- set_r2b_ot_establishment(resolved, arm$theatres)
  described <- build_environment(resolved)
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}

#' Reduce one replication to the theatre sweep's responses and the R2B theatres'
#'
#' @param env Simmer environment of one finished replication.
#' @param n_days Campaign length in days.
#' @param window_days Closing window for the queue and occupancy responses.
#' @return A list of `row`, reduce_ot_bed_replication()'s responses with the
#'   R2B theatres' occupancy, and `series`, its daily series.
#'
#' @details Occupancy is the theatres in use over those established across
#'   every unit, so a second theatre halves it at an unchanged workload, and a
#'   theatre a casualty waits in for the section on shift counts as in use.
reduce_r2b_theatre_replication <- function(env, n_days,
                                           window_days = R2B_THEATRE_SWEEP_WINDOW_DAYS) {
  out <- reduce_ot_bed_replication(env, n_days, window_days)
  beds <- LONG_HORIZON_POOLS[["R2B operating theatres"]]
  out$row$r2b_theatre_occupancy <- policy_pool_state(
    simmer::get_mon_resources(env), beds, n_days, window_days,
    pool_establishment(env_data$elms, beds)
  )[["occupancy"]]
  out
}
