#!/usr/bin/env Rscript
##############################################################################
## scripts/check_policy_sweep_protocol.R                                    ##
## Regression check — the evacuation policy sweep's parameters, its          ##
## responses and its published table agree                                  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_policy_sweep_protocol.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The shipped evacuation policy is a command decision the
# model informs rather than sets, and the sweep behind it costs 150
# replication-years, so nobody re-runs it to audit a change. Three things have
# to be checked without running it. Its parameters must be the ones the
# supplement documents. Its responses must mean what the paper says they mean,
# which for the two that also exist in the analysis pipeline (returns to duty
# and the in-theatre share) means agreeing with the pipeline rather than merely
# being computed: a sweep whose in-theatre share used a different definition
# from the one compared against the historical envelope would be comparing the
# wrong quantity. And the tracked evidence set must carry the table the paper
# prints.
#
# What this asserts:
#
#   1. Every sweep parameter in R/policy_sweep.R equals the value
#      docs/Multi_Run_Supplement.md documents in a marker comment, and the
#      shipped policy is one of the swept values.
#   2. The swept range spans the doctrinal 15 to 60 day decision range.
#   3. The reduction's returns to duty and in-theatre share agree with
#      analyse_run()'s, measured on one run's monitors.
#   4. The in-theatre share responds to the policy it is passed rather than to
#      the global, so an arm's share is measured against its own policy.
#   5. The closing-window pool state reads the window and not the campaign,
#      asserted on a constructed monitor whose answer is computable by hand.
#   6. The paired difference is taken within replication, and its replication
#      sizing follows the supplement's normal approximation.
#   7. The tracked data/policy/ summary carries the documented policies and
#      replication count, and matches the table docs/Multi_Run_Analysis.md
#      prints, row for row.
#
# Assertions 3 and 5 are what keep the rest from being circular: the others
# would hold for any response set the code happened to produce.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/scenario.R")
source("R/analysis.R")
source("R/policy_sweep.R")

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
#' @details Anything other than TRUE is a failure, NA included. A quantity
#'   compared here becomes NA when the code under test is wrong rather than when
#'   the assertion is inapplicable, and `if (ok)` on an NA raises an error that
#'   stops the run at the first such assertion instead of reporting it.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' Companion paper the published table is read from
PAPER_PATH <- file.path("docs", "Multi_Run_Analysis.md")

#' Supplement the sweep's parameters are read from
SUPPLEMENT_PATH <- file.path("docs", "Multi_Run_Supplement.md")

#' Tracked sweep summary the paper's table derives from
SUMMARY_PATH <- file.path("data", "policy", "policy_sweep.csv")

#' Campaign length the behavioural assertions run over, in days
CHECK_DAYS <- 30L

#' Control seed the behavioural assertions run under
CHECK_SEED <- 42L

#' Tolerance on a comparison of two computed reals
TOL <- 1e-8

#' Tolerance on a figure the paper prints rounded to one decimal place
PRINT_TOL <- 0.05

# ── 1-2. The code's parameters are the ones the supplement documents ─────────

cat("\n-- the sweep's parameters match the supplement --\n")

supplement <- paste(readLines(SUPPLEMENT_PATH, warn = FALSE), collapse = "\n")

#' Read one sweep parameter the supplement states in a marker comment
#'
#' @param name Marker name, as it appears after "POLICY ".
#' @return The marker's value as a character string, or NA where absent.
#'
#' @details The parameters are marked rather than parsed out of the prose,
#'   because a check that guesses which number in a paragraph is the replication
#'   count fails for reasons that have nothing to do with the sweep.
policy_marker <- function(name) {
  m <- regmatches(supplement,
                  regexpr(sprintf("<!-- POLICY %s=[^ ]+ -->", name), supplement))
  if (length(m) == 0) return(NA_character_)
  sub("^<!-- POLICY [^=]+=(.*) -->$", "\\1", m)
}

for (param in list(list("days", POLICY_DAYS_HORIZON),
                   list("replications", POLICY_REPLICATIONS),
                   list("window_days", POLICY_WINDOW_DAYS))) {
  stated <- suppressWarnings(as.numeric(policy_marker(param[[1]])))
  report(!is.na(stated) && stated == param[[2]],
         "the supplement states %s = %s and the code holds %s",
         param[[1]], format(stated), format(param[[2]]))
}

stated_policies <- policy_marker("policies")
parsed <- if (is.na(stated_policies)) {
  integer(0)
} else {
  suppressWarnings(as.integer(strsplit(stated_policies, ",")[[1]]))
}
report(length(parsed) == length(POLICY_DAYS) && !any(is.na(parsed)) &&
         all(parsed == POLICY_DAYS),
       "the supplement states policies %s and the code holds %s",
       paste(parsed, collapse = ","), paste(POLICY_DAYS, collapse = ","))

json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
shipped <- build_environment(json_data)$vars$r2eheavy$recovery$evacuation_policy_days
report(shipped %in% POLICY_DAYS,
       "the shipped policy (%s days) is one of the swept values", format(shipped))
report(min(POLICY_DAYS) <= 15L && max(POLICY_DAYS) >= 60L,
       "the sweep spans the doctrinal 15 to 60 day range (%d to %d)",
       min(POLICY_DAYS), max(POLICY_DAYS))
report(POLICY_WINDOW_DAYS < POLICY_DAYS_HORIZON,
       "the closing window (%d days) is shorter than the horizon (%d days)",
       POLICY_WINDOW_DAYS, POLICY_DAYS_HORIZON)

# ── 5. The closing-window pool state reads the window it is given ────────────

cat("\n-- the closing-window state reads the window, not the campaign --\n")

# Two beds over a ten-day campaign. Bed 1 is held for days 1 to 5 and free
# afterwards; bed 2 is free until day 8 and held from then to the end. Over a
# closing window of four days (days 7 to 10) the pool therefore serves 0 beds
# for two days and 1 bed for two days, so its occupancy is (2/4) / 2 = 0.25.
# Over the whole campaign it would be higher, so a function reading the campaign
# rather than the window disagrees here.
constructed <- data.frame(
  resource = c("b_x_1_t1", "b_x_1_t1", "b_x_2_t1", "b_x_2_t1"),
  time     = c(0, 5, 0, 8) * DAY_MIN,
  server   = c(1, 0, 0, 1),
  queue    = c(0, 0, 0, 0),
  capacity = c(1, 1, 1, 1),
  stringsAsFactors = FALSE
)
state_10 <- policy_pool_state(constructed, "^b_x_[0-9]+_t[0-9]+$",
                              n_days = 10, window_days = 4)
report(abs(state_10[["occupancy"]] - 0.25) < TOL,
       "the closing window's occupancy is %.4f, expected 0.2500",
       state_10[["occupancy"]])
report(abs(state_10[["mean_queue"]]) < TOL,
       "its mean queue is %.4f with nothing queued, expected 0",
       state_10[["mean_queue"]])

absent <- policy_pool_state(constructed, "^b_missing_", n_days = 10, window_days = 4)
report(is.na(absent[["occupancy"]]) && is.na(absent[["mean_queue"]]),
       "a pool the monitor carries no row for reads as absent rather than zero")

# ── 3-4. The responses agree with the analysis pipeline ──────────────────────

cat("\n-- the reduction agrees with the analysis pipeline --\n")

apply_policy_setting(json_data, "default", policy_days = NULL)
env <- run_once(CHECK_DAYS, seed = CHECK_SEED, write_files = FALSE)
shipped_policy <- env_data$vars$r2eheavy$recovery$evacuation_policy_days

reduced <- reduce_policy_replication(env, CHECK_DAYS, shipped_policy,
                                     seed = CHECK_SEED, window_days = CHECK_DAYS)

arrivals   <- simmer::get_mon_arrivals(env, ongoing = TRUE)
attributes <- simmer::get_mon_attributes(env)
wide <- build_attributes_wide(attributes, arrivals) %>%
  dplyr::right_join(arrivals, by = c("name", "replication"),
                    suffix = c("", "_arrival"))
pipeline <- summarise_outcomes_by_echelon(arrivals, wide)

report(reduced$total_rtd == pipeline$total_rtd,
       "returns to duty agree with the pipeline (%d against %d)",
       reduced$total_rtd, pipeline$total_rtd)
report(reduced$total_dow == pipeline$total_dow,
       "died of wounds agree with the pipeline (%d against %d)",
       reduced$total_dow, pipeline$total_dow)
report(!is.na(reduced$in_theatre_share) &&
         abs(reduced$in_theatre_share - pipeline$in_theatre_share) < TOL,
       "the in-theatre share agrees with the pipeline (%.6f against %.6f)",
       reduced$in_theatre_share, pipeline$in_theatre_share)
report(reduced$dispositions == pipeline$evacuation_policy_summary$decisions,
       "the disposition count agrees with the pipeline (%d against %d)",
       reduced$dispositions, pipeline$evacuation_policy_summary$decisions)

# The share is a function of the policy the reduction is passed, not of the
# global, so an arm measures its own policy. A longer policy retains at least as
# many casualties, the comparison being on one campaign's drawn recoveries.
longer <- reduce_policy_replication(env, CHECK_DAYS, shipped_policy + 20L,
                                    seed = CHECK_SEED, window_days = CHECK_DAYS)
report(!is.na(longer$in_theatre_share) &&
         longer$in_theatre_share > reduced$in_theatre_share,
       "a longer policy retains a larger share (%.4f against %.4f)",
       longer$in_theatre_share, reduced$in_theatre_share)
report(longer$total_rtd == reduced$total_rtd,
       "the campaign itself is unchanged by the policy the reduction is told")

# ── 6. The paired difference is within replication ───────────────────────────

cat("\n-- the paired difference is taken within replication --\n")

# Two arms over four replications, constructed so that the paired difference and
# the difference of means disagree: every pair rises by exactly 2, while the
# arms' spread is large enough that an unpaired interval would not exclude zero.
# Two arms over five replications, constructed so that the paired difference and
# the difference of means disagree: every pair rises by 2 or 3, a mean of 2.4,
# while each arm's own spread is an order of magnitude larger, so an unpaired
# interval on the same numbers would not exclude zero. The differences vary, so
# the sizing helper has a standard deviation to work from.
paired_rows <- data.frame(
  replication = rep(1:5, 2),
  policy_days = rep(c(21L, 30L), each = 5),
  total_dow   = c(10, 40, 70, 100, 130, 12, 43, 72, 103, 132)
)
differences <- c(2, 3, 2, 3, 2)
d <- policy_paired_difference(paired_rows, "total_dow", 21L, 30L)
report(d$n_pairs == 5 && abs(d$difference - mean(differences)) < TOL,
       "the paired difference is %.4f over %d pairs, expected %.1f over 5",
       d$difference, d$n_pairs, mean(differences))
report(!is.na(d$ci_lower) && d$ci_lower > 0,
       "its interval excludes zero (lower bound %.4f) where an unpaired one would not",
       d$ci_lower)
arm_sd <- sd(paired_rows$total_dow[paired_rows$policy_days == 21L])
report(arm_sd > 10 * sd(differences),
       "each arm's own spread (sd %.1f) dwarfs the paired differences' (sd %.2f)",
       arm_sd, sd(differences))

# The normal approximation the supplement uses, recomputed here rather than
# taken from the function, so the assertion has an answer of its own.
needed <- policy_replications_for(paired_rows, "total_dow", 21L, 30L, 1)
expected_needed <- ceiling((qnorm(0.975) * sd(differences) / 1)^2)
report(!is.na(needed) && needed == expected_needed,
       "the count for a half-width of one is %s, expected %s",
       format(needed), format(expected_needed))
report(is.na(policy_replications_for(paired_rows, "total_dow", 21L, 30L, 0)),
       "a non-positive half-width returns no count rather than an infinite one")

# ── 7. The tracked summary is the table the paper prints ─────────────────────

cat("\n-- the tracked summary matches the published table --\n")

if (!file.exists(SUMMARY_PATH)) {
  report(FALSE, "the tracked sweep summary %s exists", SUMMARY_PATH)
} else {
  tracked <- read.csv(SUMMARY_PATH, stringsAsFactors = FALSE)

  report(setequal(unique(tracked$policy_days), POLICY_DAYS),
         "the tracked summary carries the %d documented policies", length(POLICY_DAYS))
  report(all(tracked$n_reps[!is.na(tracked$mean)] == POLICY_REPLICATIONS),
         "every measured response carries %d replications", POLICY_REPLICATIONS)

  paper <- readLines(PAPER_PATH, warn = FALSE)
  marker <- grep("<!-- POLICY TABLE -->", paper, fixed = TRUE)
  if (length(marker) != 1) {
    report(FALSE, "the paper carries exactly one policy table marker (found %d)",
           length(marker))
  } else {
    rows <- paper[marker:length(paper)]
    rows <- rows[seq_len(which(!grepl("^\\|", rows) & seq_along(rows) > 2)[1] - 1)]
    rows <- rows[grepl("^\\|", rows)]
    header <- trimws(strsplit(sub("^\\|", "", sub("\\|$", "", rows[1])), "\\|")[[1]])
    swept <- suppressWarnings(as.integer(gsub("[^0-9]", "", header[-1])))
    report(length(swept) == length(POLICY_DAYS) && all(swept == POLICY_DAYS),
           "the paper's columns are the swept policies (%s)",
           paste(swept, collapse = ","))

    #' Check one printed table row against the tracked summary
    #'
    #' @param label Row label as the paper prints it.
    #' @param response Response name in the tracked summary.
    #' @param scale Multiplier the paper prints the response at.
    #' @return Invisible NULL.
    check_row <- function(label, response, scale) {
      row <- rows[grepl(paste0("^\\| ", label), rows)]
      if (length(row) != 1) {
        report(FALSE, "the paper prints one '%s' row (found %d)", label, length(row))
        return(invisible(NULL))
      }
      cells <- trimws(strsplit(sub("^\\|", "", sub("\\|$", "", row)), "\\|")[[1]])[-1]
      for (k in seq_along(POLICY_DAYS)) {
        printed <- suppressWarnings(as.numeric(sub(" .*$", "", cells[k])))
        arm <- tracked[tracked$policy_days == POLICY_DAYS[k] &
                         tracked$response == response, ]
        ok <- nrow(arm) == 1 && !is.na(printed) &&
          abs(printed - scale * arm$mean) < PRINT_TOL
        report(ok, "'%s' at %d days prints %s against the data's %s",
               label, POLICY_DAYS[k], format(printed),
               if (nrow(arm) == 1) format(round(scale * arm$mean, 2)) else "no row")
      }
      invisible(NULL)
    }

    check_row("R2E hold occupancy", "hold_occupancy", 100)
    check_row("Returns to duty", "total_rtd", 1)
    check_row("In-theatre share", "in_theatre_share", 100)
    check_row("Role 4 peak", "role4_peak", 1)
  }
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All policy sweep protocol checks passed.\n")
quit(status = 0)
