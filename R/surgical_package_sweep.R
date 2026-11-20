##############################################################################
## R/surgical_package_sweep.R                                               ##
## R2B and R2E surgery with R2E intensive care beds, as one package         ##
##############################################################################
#
# Each piece of the surgical capacity the planning paper recommends was
# measured on its own: the R2E sections (R/surgical_team_sweep.R), the R2E
# theatres (R/ot_bed_sweep.R), the R2E intensive care beds behind added surgery
# (R/post_op_capacity_sweep.R) and the R2B sections and theatres
# (R/r2b_theatre_sweep.R). The pieces act on one casualty stream, so their
# effects need not add: an R2B unit that keeps its surgery forward leaves less
# for R2E to do. This module fields them together. Every arm past the shipped
# establishment carries the R2B package, two sections and two theatres per
# unit, and adds R2E sections, theatres and intensive care beds to it one step
# at a time, so each piece is priced against the package it would join.
#
# Every count is structure in `elms`, set on the resolved configuration before
# build_environment() constructs the resources, by the setter of the sweep that
# owns it. No trajectory changes. R2E holding beds keep the shipped 30, more
# holding beds having bought nothing behind added surgery.
#
# Each replication is reduced inside the forked worker that produced it by
# reduce_r2b_theatre_replication(), so every response of the theatre and R2B
# theatre sweeps is reported. The shipped arm and the R2B package alone are
# configurations the R2B second theatre sweep ran under the same seed and
# scenario, so both reproduce its replications.
#
# Depends on R/analysis.R, R/policy_sweep.R, R/hold_window.R, R/long_horizon.R,
# R/surgical_team_sweep.R, R/icu_bed_sweep.R, R/ot_bed_sweep.R and
# R/r2b_theatre_sweep.R; source those first.

#' Replications per arm
PACKAGE_SWEEP_REPLICATIONS <- 30L

#' Campaign length in days each replication runs for
PACKAGE_SWEEP_DAYS <- 360L

#' Control seed every arm is set to before it runs
PACKAGE_SWEEP_SEED <- 42L

#' Closing window the queue and occupancy responses are measured over, in days
PACKAGE_SWEEP_WINDOW_DAYS <- 90L

#' Scenario the protocol is run under
#'
#' @details High intensity alone: there every piece resolves a change in deaths
#'   of wounds on its own, so whether the pieces add is a question with an
#'   answer, while at moderate intensity no piece resolves one.
PACKAGE_SWEEP_SCENARIO <- "high_intensity"

#' The arms swept, as an R2B and R2E surgical and intensive care establishment
#'
#' @details The shipped establishment first, then the R2B package, then each R2E
#'   addition to it, the order the results table reads them in. Sections and
#'   theatres at R2B are per unit; R2E has one unit.
PACKAGE_SWEEP_ARMS <- data.frame(
  arm = c("Shipped", "R2B package", "R2B package, R2E 4 sections",
          "R2B package, R2E 4 sections, 12 ICU", "R2B package, R2E 6 sections, 3 theatres",
          "R2B package, R2E 6 sections, 3 theatres, 12 ICU"),
  r2b_sections = c(1L, 2L, 2L, 2L, 2L, 2L),
  r2b_theatres = c(1L, 2L, 2L, 2L, 2L, 2L),
  r2e_sections = c(3L, 3L, 4L, 4L, 6L, 6L),
  r2e_theatres = c(2L, 2L, 2L, 2L, 3L, 3L),
  icu_beds = c(4L, 4L, 4L, 12L, 4L, 12L),
  stringsAsFactors = FALSE
)

#' Arm of the shipped establishment
PACKAGE_SWEEP_BASELINE_ARM <- "Shipped"

#' Arm of the R2B package alone, R2E at its shipped establishment
PACKAGE_SWEEP_R2B_ARM <- "R2B package"

#' The paired comparisons reported, as a from and to arm
#'
#' @details Every arm against the shipped establishment, which prices the whole
#'   package; each R2E addition against the R2B package alone, which prices the
#'   addition once R2B keeps its surgery forward; the intensive care beds
#'   against the same surgery without them; and six sections and three theatres
#'   against four sections. Holm's adjustment is taken over every row of the
#'   file.
PACKAGE_SWEEP_COMPARISONS <- local({
  a <- PACKAGE_SWEEP_ARMS$arm
  rbind(
    data.frame(from = PACKAGE_SWEEP_BASELINE_ARM, to = a[-1], stringsAsFactors = FALSE),
    data.frame(from = PACKAGE_SWEEP_R2B_ARM, to = a[3:6], stringsAsFactors = FALSE),
    data.frame(from = c(a[3], a[5], a[3]), to = c(a[4], a[6], a[5]),
               stringsAsFactors = FALSE)
  )
})

#' Responses the paired comparisons report
#'
#' @details The theatre sweep's paired responses with the operations done at
#'   R2B, which the R2B package changes.
PACKAGE_SWEEP_PAIRED_RESPONSES <- c("r2b_surgeries", OT_SWEEP_PAIRED_RESPONSES)

#' Half-width each paired response is sized against, in its own units
#'
#' @details The theatre sweep's, with ten operations on the R2B surgeries as on
#'   every other count of operations.
PACKAGE_SWEEP_HALF_WIDTHS <- c(r2b_surgeries = 10, OT_SWEEP_HALF_WIDTHS)

#' Enter a scenario's configuration with the surgical package overridden
#'
#' @param json_data Parsed env_data.json.
#' @param scenario Scenario profile to resolve.
#' @param arm One row of PACKAGE_SWEEP_ARMS.
#' @return Invisibly, the described configuration that was applied.
#'
#' @details Each count is set by the setter of the sweep that owns it, each of
#'   which fails rather than returning the configuration unchanged, so an arm
#'   cannot silently measure the shipped establishment. R2E holding beds keep
#'   their shipped count.
apply_package_setting <- function(json_data, scenario, arm) {
  resolved <- resolve_scenario(json_data, scenario)
  resolved <- set_surgical_sections(resolved, "r2b", arm$r2b_sections)
  resolved <- set_r2b_ot_establishment(resolved, arm$r2b_theatres)
  resolved <- set_surgical_sections(resolved, "r2eheavy", arm$r2e_sections)
  resolved <- set_ot_establishment(resolved, arm$r2e_theatres)
  resolved <- set_icu_establishment(resolved, arm$icu_beds)
  described <- build_environment(resolved)
  assign("env_data", described, envir = globalenv())
  assign("day_min", DAY_MIN, envir = globalenv())
  assign("counts", sapply(described$elms, length), envir = globalenv())
  invisible(described)
}
