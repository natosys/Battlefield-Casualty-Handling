#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_casualty_flows.R                                      ##
## Regression check — every casualty takes exactly one flow, and the        ##
## tracked flows reconcile with the comparative scenario totals             ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_casualty_flows.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The casualty flow diagram of docs/Results.md divides
# every casualty of a campaign by type, furthest echelon reached and outcome. A
# rule that let a casualty fall between two outcomes, or into none, would still
# draw a plausible diagram, and a generator added without a known type would
# drop its casualties from the flows unseen. The flows are therefore asserted
# both on hand-built casualties, whose path is known, and on the tracked
# evidence, whose totals are known from another file:
#
#   1. On hand-built casualties, each classification rule gives the path
#      stated for it, death of wounds taking precedence over a return to duty,
#      a casualty that bypassed R2B counting as reaching R2E, and a casualty
#      with no attribute set remaining at R1 in care; a casualty of an unknown
#      type stops the classification.
#   2. The tracked flows carry both intensities at the protocol's replication
#      count, every label is one of the documented ones, and no flow is
#      impossible (a killed-in-action casualty is untreated and nothing else is;
#      a return to duty from R2B or R2E, an evacuation and a wait for airlift
#      each come from the echelon that gives them).
#   3. Each replication's flows sum to the casualties of the totals file: the
#      mean of the per-replication sums equals the published casualty total.
#   4. The summary is the reduction of the per-replication flows.
#   5. docs/Results.md shows the diagram and prints both flow tables as
#      generated spans.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

source("R/constants.R")
source("R/scenario_runner.R")

#' Directory of the tracked comparative scenario evidence set
EVIDENCE_DIR <- file.path("data", "scenarios")

#' The results paper the flows are published in
RESULTS_PATH <- file.path("docs", "Results.md")

#' Tolerance on a comparison of two computed reals
TOL <- 1e-6

state <- new.env(parent = emptyenv())
state$failures <- character(0)

#' Record a failure, deferring the non-zero exit to the end of the run
#'
#' @param ... sprintf() format string and its arguments.
#' @return Invisibly, the accumulated failure vector.
fail <- function(...) state$failures <- c(state$failures, sprintf(...))

#' Print one PASS or FAIL line, recording a failure
#'
#' @param ok TRUE when the assertion held.
#' @param fmt sprintf() format string describing the assertion.
#' @param ... Arguments to `fmt`.
#' @return Invisible NULL.
#'
#' @details Anything other than TRUE is a failure, NA included.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' Read one tracked file of the evidence set, or NULL when it is missing
#'
#' @param name File name under EVIDENCE_DIR.
#' @return The data frame, or NULL.
read_evidence <- function(name) {
  path <- file.path(EVIDENCE_DIR, name)
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE)
}

# ── 1. The classification rules on hand-built casualties ────────────────────

cat("-- each rule gives the path stated for it --\n")

na <- NA_real_
toy <- data.frame(
  name = c("kia_cbt1", "wia_cbt1", "wia_cbt2", "dnbi_spt1", "wia_spt1", "wia_cbt3",
           "wia_cbt4", "dnbi_cbt1", "wia_cbt5"),
  r2b = c(na, 1, -1, 2, 1, -1, 1, na, 2),
  r2e_arrival_time = c(na, 500, na, na, 900, na, 700, na, na),
  dow = c(na, 1, na, na, na, na, na, na, na),
  ame_rtd = c(na, na, 1, na, na, na, na, na, na),
  return_echelon = c(na, 3, 3, 2, na, na, na, na, na),
  ame_departure_time = c(na, na, na, na, 5000, na, na, na, na),
  evacuation_reason = c(na, 1, 1, na, 2, 1, 0, na, na),
  stringsAsFactors = FALSE
)
expected <- data.frame(
  type = c("Killed in action", "Wounded in action", "Wounded in action",
           "Disease and non-battle injury", "Wounded in action", "Wounded in action",
           "Wounded in action", "Disease and non-battle injury", "Wounded in action"),
  echelon = c("Not treated", "R2E", "R2E", "R2B", "R2E", "R2E", "R2E", "R1", "R2B"),
  outcome = c("Killed in action", "Died of wounds", "Returned to duty awaiting airlift",
              "Returned to duty from R2B", "Evacuated to Role 4",
              "Awaiting airlift on the last day", "In care on the last day",
              "In care on the last day", "In care on the last day"),
  stringsAsFactors = FALSE
)
got <- classify_casualty_flows(toy)
for (i in seq_len(nrow(toy))) {
  report(identical(unlist(got[i, ]), unlist(expected[i, ])),
         "%s takes %s / %s / %s (got %s)", toy$name[i], expected$type[i], expected$echelon[i],
         expected$outcome[i], paste(unlist(got[i, ]), collapse = " / "))
}
unknown <- tryCatch({
  classify_casualty_flows(transform(toy[1, ], name = "surgeon_1"))
  FALSE
}, error = function(e) TRUE)
report(unknown, "a casualty of an unknown type stops the classification")

# ── 2. The tracked flows carry the protocol and only possible paths ─────────

cat("\n-- the tracked flows are complete and possible --\n")

reps <- read_evidence("scenario_flow_replications.csv")
flows <- read_evidence("scenario_flows.csv")
totals <- read_evidence("scenario_comparison_totals.csv")
report(!is.null(reps) && !is.null(flows) && !is.null(totals),
       "the per-replication flows, their summary and the totals are tracked")

if (!is.null(reps) && !is.null(flows) && !is.null(totals)) {
  per_scenario <- tapply(reps$replication, reps$scenario, function(x) length(unique(x)))
  report(setequal(names(per_scenario), SCENARIO_PROTOCOL_PROFILES) &&
           all(per_scenario == SCENARIO_REPLICATIONS),
         "both intensities carry %d replications (%s)", SCENARIO_REPLICATIONS,
         paste(names(per_scenario), per_scenario, sep = "=", collapse = ", "))
  report(all(reps$type %in% SCENARIO_FLOW_TYPES) &&
           all(reps$echelon %in% SCENARIO_FLOW_ECHELONS) &&
           all(reps$outcome %in% SCENARIO_FLOW_OUTCOMES),
         "every type, echelon and outcome is a documented label")
  report(all(reps$casualties > 0 & reps$casualties == round(reps$casualties)),
         "every per-replication flow is a positive whole count")
  kia_type <- reps$type == "Killed in action"
  report(all((reps$echelon == "Not treated") == kia_type) &&
           all((reps$outcome == "Killed in action") == kia_type),
         "only killed-in-action casualties are untreated, and all of them are")
  #' Whether every flow to an outcome comes from one echelon
  #'
  #' @param outcome The outcome label.
  #' @param echelon The echelon label it must come from.
  #' @return TRUE when no flow to `outcome` comes from another echelon.
  from <- function(outcome, echelon) all(reps$echelon[reps$outcome == outcome] == echelon)
  report(from("Returned to duty from R2E", "R2E") && from("Returned to duty from R2B", "R2B") &&
           from("Returned to duty awaiting airlift", "R2E") &&
           from("Evacuated to Role 4", "R2E") && from("Awaiting airlift on the last day", "R2E"),
         "returns to duty, evacuations and airlift waits come from the echelon giving them")

  # ── 3. The flows reconcile with the published casualty totals ─────────────

  cat("\n-- the flows sum to the casualty totals --\n")
  for (sc in SCENARIO_PROTOCOL_PROFILES) {
    x <- reps[reps$scenario == sc, ]
    per_rep <- tapply(x$casualties, x$replication, sum)
    published <- totals$mean[totals$scenario == sc & totals$metric == "total_casualties"]
    report(length(published) == 1 && abs(mean(per_rep) - published) < TOL,
           "%s: the mean of the per-replication flow sums (%.3f) is the casualty total (%s)",
           sc, mean(per_rep), format(published))
  }

  # ── 4. The summary is the reduction of the replications ───────────────────

  cat("\n-- the summary is the reduction of the replications --\n")
  recomputed <- bind_rows(lapply(SCENARIO_PROTOCOL_PROFILES, function(sc) {
    cbind(scenario = sc, summarise_scenario_flows(reps[reps$scenario == sc, ]))
  }))
  #' The key identifying each flow of a summary
  #'
  #' @param d A flow summary.
  #' @return One string per row.
  key <- function(d) paste(d$scenario, d$type, d$echelon, d$outcome)
  m <- match(key(flows), key(recomputed))
  report(nrow(flows) == nrow(recomputed) && !anyNA(m) &&
           all(abs(flows$mean - recomputed$mean[m]) < TOL) &&
           all(abs(flows$ci_lower - recomputed$ci_lower[m]) < TOL) &&
           all(abs(flows$ci_upper - recomputed$ci_upper[m]) < TOL),
         "every flow's mean and interval reproduce from the replications (%d flows)",
         nrow(flows))
}

# ── 5. The results paper publishes the flows ──────────────────────────────

cat("\n-- the results paper shows the flows --\n")
paper <- if (file.exists(RESULTS_PATH)) readLines(RESULTS_PATH, warn = FALSE) else character(0)
report(any(grepl("](../images/casualty_flows.png)", paper, fixed = TRUE)),
       "docs/Results.md shows images/casualty_flows.png")
for (span in c("flow_outcomes", "flow_echelons")) {
  report(sum(paper == sprintf("<!-- GEN %s -->", span)) == 1,
         "docs/Results.md prints the %s table as one generated span", span)
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All casualty flow checks passed.\n")
quit(status = 0)
