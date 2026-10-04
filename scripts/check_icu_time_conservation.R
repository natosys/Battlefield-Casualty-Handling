#!/usr/bin/env Rscript
##############################################################################
## scripts/check_icu_time_conservation.R                                    ##
## Regression check — post-operative ICU time is conserved across routes    ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_icu_time_conservation.R
#   Rscript scripts/check_icu_time_conservation.R --quick   # 10 days, fewer configurations
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step.
#
# Why this check exists: a casualty's intensive care requirement follows from
# the injury, so the total should not depend on which mix of echelons delivers
# it. The model once failed this badly and silently: R2B provided no
# post-operative intensive care at all, while R2E separately shortened its own
# episode for the very casualties R2B had operated on, so an R2B-operated
# casualty received about 28% of the ICU time an otherwise identical
# R2E-operated one did. Nothing in the run output said so.
#
# Forward holding is now decided per casualty, when an R2B operation ends,
# on stability (a window) and capacity (R2E intensive care saturated), for
# damage control and single-stage casualties alike. This check confirms, at
# each configuration of that rule:
#   1. Damage control: the stabilisation requirement is drawn once and split
#      between the echelons, so minutes served at R2B and R2E sum to it.
#   2. Single-stage: the post-definitive requirement is drawn at R2B where the
#      rule applies and the remainder served at R2E, so the two sum to it.
#   3. The forward minutes follow the rule: the stability window limited by
#      the requirement and the forward hold limit, extended only by the
#      capacity trigger and never beyond that limit.
#   4. Both triggers and both pathways are reachable in the configurations
#      asserted, so none of the above is vacuous, and a disabled rule holds
#      nobody forward and draws nothing at R2B for a single-stage casualty.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")

args       <- commandArgs(trailingOnly = TRUE)
quick      <- "--quick" %in% args
CHECK_DAYS <- if (quick) 10L else 30L
CHECK_SEED <- 42L

# One entry per configuration of the forward holding rule. `r2e_icu_beds`
# overrides the R2E intensive care establishment where it is not NULL, which is
# how the capacity trigger is made reachable in a short run: the shipped four
# beds are rarely saturated in thirty days.
CONFIGS <- list(
  off       = list(dcs = 0,   single = 0,   trigger = 0, r2e_icu_beds = NULL),
  stability = list(dcs = 240, single = 360, trigger = 0, r2e_icu_beds = NULL),
  capacity  = list(dcs = 0,   single = 0,   trigger = 1, r2e_icu_beds = 1L),
  both      = list(dcs = 240, single = 360, trigger = 1, r2e_icu_beds = 1L)
)
if (quick) CONFIGS <- CONFIGS[c("off", "both")]

failures <- character(0)

#' Record a failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated failures, invisibly; called for its side effect.
fail <- function(...) failures <<- c(failures, sprintf(...))

#' Print one PASS or FAIL line, recording a failure when the assertion fails
#'
#' @param ok Logical: whether the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Values interpolated into `fmt`.
#' @return The printed line, invisibly; called for its side effect.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

#' Build the model environment for one configuration of the forward rule
#'
#' @param cfg One element of `CONFIGS`.
#' @return The parsed configuration, ready to assign to the `env_data` global.
build_config <- function(cfg) {
  json <- fromJSON("env_data.json", simplifyVector = FALSE)
  if (!is.null(cfg$r2e_icu_beds)) {
    for (i in seq_along(json$elms)) {
      if (json$elms[[i]]$elm != "r2eheavy") next
      for (j in seq_along(json$elms[[i]]$beds)) {
        if (json$elms[[i]]$beds[[j]]$name == "icu") json$elms[[i]]$beds[[j]]$qty <- cfg$r2e_icu_beds
      }
    }
  }
  ed <- build_environment(json)
  ed$vars$r2b$post_op_icu$stability_window_dcs <- cfg$dcs
  ed$vars$r2b$post_op_icu$stability_window_single_stage <- cfg$single
  ed$vars$r2b$post_op_icu$capacity_trigger <- cfg$trigger
  ed
}

#' One casualty per row, with the post-operative minutes each echelon served
#'
#' @param attrs get_mon_attributes() output for a completed run
#' @return Data frame, one row per casualty who reached a surgical decision:
#'   name, dcs (1 damage control, 0 single-stage), the stabilisation and
#'   post-definitive requirements drawn (`stab_total`, `pd_total`, NA where
#'   none was), the minutes served forward (`r2b`), the minutes served at R2E
#'   (`r2e_stab`, `pd_min`), the `r2b_surgery` route marker and the
#'   `post_op_pathway` the R2E stabilisation took.
per_casualty <- function(attrs) {
  wanted <- c("dcs_pathway", "stabilisation_total", "post_definitive_total",
              "r2b_post_op_min", "r2e_post_op_min", "post_definitive_min",
              "r2b_surgery", "post_op_pathway", "r2b_post_op_pathway")
  wide <- attrs %>%
    filter(key %in% wanted) %>%
    group_by(name, key) %>%
    summarise(value = dplyr::last(value), .groups = "drop") %>%
    pivot_wider(names_from = key, values_from = value)
  for (col in wanted) if (!col %in% names(wide)) wide[[col]] <- NA_real_
  wide %>%
    filter(!is.na(dcs_pathway)) %>%
    transmute(
      name,
      dcs         = dcs_pathway,
      stab_total  = stabilisation_total,
      pd_total    = post_definitive_total,
      r2b         = ifelse(is.na(r2b_post_op_min), 0, r2b_post_op_min),
      r2e_stab    = ifelse(is.na(r2e_post_op_min), 0, r2e_post_op_min),
      pd_min      = post_definitive_min,
      r2b_surgery = ifelse(is.na(r2b_surgery), 0, r2b_surgery),
      pathway     = ifelse(is.na(post_op_pathway), 0, post_op_pathway),
      fwd_pathway = r2b_post_op_pathway
    )
}

# ── Setup ───────────────────────────────────────────────────────────────────

# Globals the model reads directly, mirroring run_bch()'s setup in run.R.
env_data <<- load_elms("env_data.json")
day_min  <<- DAY_MIN
counts   <<- sapply(env_data$elms, length)

shipped_rule <- env_data$vars$r2b$post_op_icu
cap <- shipped_rule$forward_hold_max

cat(sprintf("Post-operative ICU conservation: %d-day runs, seed %d, forward hold limit %g min\n",
            CHECK_DAYS, CHECK_SEED, cap))

cat("\n-- the shipped configuration holds nobody forward --\n")
report(shipped_rule$stability_window_dcs == 0 && shipped_rule$stability_window_single_stage == 0,
       "no stability window ships in force")
report(shipped_rule$capacity_trigger == 0, "the capacity trigger ships disabled")

results <- list()

for (name in names(CONFIGS)) {
  cfg <- CONFIGS[[name]]
  cat(sprintf("\n-- %s: stability %g / %g min (DCS / single-stage), capacity trigger %d, R2E ICU beds %s --\n",
              name, cfg$dcs, cfg$single, cfg$trigger,
              if (is.null(cfg$r2e_icu_beds)) "shipped" else cfg$r2e_icu_beds))

  ed <- build_config(cfg)
  assign("env_data", ed, envir = globalenv())
  assign("counts", sapply(ed$elms, length), envir = globalenv())

  invisible(capture.output(suppressWarnings(
    wrapped <- run_once(n_days = CHECK_DAYS, seed = CHECK_SEED)
  )))
  all_cas <- per_casualty(get_mon_attributes(wrapped))
  r2b_op <- all_cas %>% filter(r2b_surgery == 1)
  dcs    <- r2b_op %>% filter(dcs == 1, !is.na(stab_total))
  single <- r2b_op %>% filter(dcs == 0)

  report(nrow(dcs) > 0 && nrow(single) > 0,
         "%s: casualties operated at R2B on both pathways (%d damage control, %d single-stage)",
         name, nrow(dcs), nrow(single))

  # Check 1: damage control conservation. Binds on the nominal R2E pathway, a
  # casualty routed to the R2E post-operative holding bed substituting a
  # shorter stay by design, and a casualty whose journey ended between the
  # echelons never reaching the rear leg.
  nominal <- dcs %>% filter(pathway == 1)
  worst <- if (nrow(nominal)) max(abs(nominal$r2b + nominal$r2e_stab - nominal$stab_total)) else 0
  report(worst < 1e-6,
         "%s: %d damage control casualties served the stabilisation requirement drawn across R2B and R2E (worst gap %.2e min)",
         name, nrow(nominal), worst)

  # Check 2: single-stage conservation. A requirement drawn at R2B is served
  # across both echelons; one not drawn there is drawn at R2E whole.
  drawn_fwd <- single %>% filter(!is.na(pd_total), !is.na(pd_min))
  if (cfg$dcs == 0 && cfg$single == 0 && cfg$trigger == 0) {
    report(all(is.na(single$pd_total)) && all(single$r2b == 0),
           "%s: no single-stage casualty drew a requirement or held a forward bed with the rule disabled (%d casualties)",
           name, nrow(single))
  } else {
    worst_sc <- if (nrow(drawn_fwd)) max(abs(drawn_fwd$r2b + drawn_fwd$pd_min - drawn_fwd$pd_total)) else 0
    report(nrow(drawn_fwd) > 0 && worst_sc < 1e-6,
           "%s: %d single-stage casualties served the post-definitive requirement drawn across R2B and R2E (worst gap %.2e min)",
           name, nrow(drawn_fwd), worst_sc)
  }

  # Check 3: forward minutes follow the rule, per pathway. The lower bound is
  # the stability window limited by requirement and hold limit; the upper
  # bound is the requirement limited by the hold limit, which only the
  # capacity trigger can approach.
  check_rule <- function(df, window, total_col, label) {
    if (!nrow(df)) return(invisible(NULL))
    total <- df[[total_col]]
    total <- ifelse(is.na(total), 0, total)
    lower <- pmin(window, total, cap)
    upper <- pmin(total, cap)
    ok <- if (cfg$trigger == 1) {
      all(df$r2b >= lower - 1e-6 & df$r2b <= upper + 1e-6)
    } else {
      all(abs(df$r2b - lower) < 1e-6)
    }
    report(ok, "%s: %s forward minutes match the rule for all %d R2B-operated casualties",
           name, label, nrow(df))
    if (cfg$trigger == 1) {
      extended <- sum(df$r2b > lower + 1e-6)
      report(extended > 0,
             "%s: %d %s casualties held beyond the stability window on capacity grounds",
             name, extended, label)
    }
  }
  check_rule(dcs, cfg$dcs, "stab_total", "damage control")
  check_rule(single, cfg$single, "pd_total", "single-stage")

  # Check 4: a casualty not operated at R2B served nothing forward and, for
  # stabilisation, the whole requirement at R2E.
  not_r2b <- all_cas %>% filter(r2b_surgery != 1, dcs == 1, !is.na(stab_total), pathway == 1)
  if (nrow(not_r2b)) {
    report(all(not_r2b$r2b == 0) && all(abs(not_r2b$r2e_stab - not_r2b$stab_total) < 1e-6),
           "%s: %d casualties not operated at R2B served their whole requirement at R2E",
           name, nrow(not_r2b))
  }

  results[[name]] <- list(dcs = nrow(dcs), single = nrow(single),
                          held = sum(r2b_op$r2b > 0))
}

# Reachability: each rule held someone, so assertions above were not vacuous.
if ("stability" %in% names(results)) {
  report(results$stability$held > 0, "stability: %d casualties were held forward on stability grounds",
         results$stability$held)
}
if ("capacity" %in% names(results)) {
  report(results$capacity$held > 0, "capacity: %d casualties were held forward on capacity grounds",
         results$capacity$held)
}
report(results$off$held == 0, "off: no casualty was held forward with the rule disabled")

# ── A malformed rule is rejected by name ────────────────────────────────────

cat("\n-- a malformed field is rejected by name --\n")
base_ed <- load_elms("env_data.json")
for (field in c("stability_window_dcs", "stability_window_single_stage",
                "capacity_trigger", "capacity_poll_interval", "forward_hold_max")) {
  for (bad in list(-1, NA_real_, "x", c(1, 2))) {
    ed <- base_ed
    ed$vars$r2b$post_op_icu[[field]] <- bad
    assign("env_data", ed, envir = globalenv())
    err <- tryCatch({ invisible(forward_hold_rule()); NULL },
                    error = function(e) conditionMessage(e))
    report(!is.null(err) && grepl(field, err, fixed = TRUE),
           "%s of %s is rejected with a message naming the field", field,
           paste(format(bad), collapse = ", "))
  }
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All post-operative ICU time conservation checks passed.\n")
quit(status = 0)
