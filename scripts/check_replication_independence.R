#!/usr/bin/env Rscript
##############################################################################
## scripts/check_replication_independence.R                                 ##
## Regression check — replications are independent of one another           ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_replication_independence.R              # 3 x 60 reps, 30 days
#   Rscript scripts/check_replication_independence.R --quick      # 2 x 10 reps, 10 days
#   Rscript scripts/check_replication_independence.R --scenario moderate_intensity
#   Rscript scripts/check_replication_independence.R --measurements 4 --reps 60
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step. A full run executes 180 replications and takes
# roughly half an hour on four cores; --quick finishes in about a minute but
# has nowhere near the power to detect a dependence and says so in its output.
#
# Why this check exists: every confidence interval this project publishes is
# computed as qt(0.975, df = n - 1) * sd / sqrt(n) with n set to the number of
# replications, which is correct only if the replications are independent.
# They were not. run_replications() used to pair them, (2k-1, 2k) sharing a
# seed with the even member negating its arrival-generation uniforms. Because
# the negation reached the arrival generators alone, partners shared an
# unnegated trajectory stream and correlated *positively* on any response
# driven by treatment rather than by arrival counts: +0.38 measured on the
# died-of-wounds count. Positive correlation between observations narrows an
# interval that divides by n, and a too-narrow interval makes the model look
# more precisely calibrated than it is. The pairing was withdrawn (Issue #189,
# README — Multi-run Replication Framework); this check is what keeps it
# withdrawn.
#
# What it asserts. For each response it measures the lag-1 correlation across
# replications in run order, which is where a reintroduced pairing would show
# up, since partners were adjacent. The failure condition is one-sided: only
# positive correlation understates variance and so narrows the published
# intervals. A negative correlation makes them conservative instead, which is
# a variance-reduction scheme working rather than a defect, so it is reported
# and not failed on.
#
# The responses are chosen to sit on both sides of the defect. Total casualties
# is arrival-driven, which is the only thing the withdrawn negation ever
# reached; died-of-wounds count and R2E ICU mean queue are driven by treatment
# and transport draws taken inside simmer's event loop, which it never did.
# A scheme that helps the first and hurts the others is exactly the asymmetry
# that made withdrawal, rather than extension, the right correction.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")

args  <- commandArgs(trailingOnly = TRUE)
quick <- "--quick" %in% args

arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

SCENARIO   <- arg_value("--scenario", "default")
N_MEASURE  <- as.integer(arg_value("--measurements", if (quick) 2L else 3L))
N_REPS     <- as.integer(arg_value("--reps",         if (quick) 10L else 60L))
CHECK_DAYS <- as.integer(arg_value("--days",         if (quick) 10L else 30L))

# Fixed control seeds so a run is reproducible and two runs of this check on
# unchanged code agree exactly. Each seeds one independent measurement.
CONTROL_SEEDS <- c(42L, 777L, 20260808L, 13L, 20261L)

if (N_MEASURE > length(CONTROL_SEEDS)) {
  stop(sprintf("--measurements above %d needs more control seeds", length(CONTROL_SEEDS)))
}

failures <- character(0)
fail     <- function(...) failures <<- c(failures, sprintf(...))

report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}

# ── Measurement ─────────────────────────────────────────────────────────────

#' Per-replication responses for one replication set
#'
#' @param mon Monitoring list from run_replications()
#' @return Data frame in replication order, one row per replication, with
#'   total_casualties (arrival-driven), dow_count and icu_mean_queue (both
#'   driven by draws taken inside simmer's event loop)
responses <- function(mon) {
  total <- mon$arrivals %>% count(replication, name = "total_casualties")

  dow <- mon$attributes %>%
    filter(key == "dow", value == 1) %>%
    count(replication, name = "dow_count")

  # Time-weighted, matching summarise_replications()' unit of analysis, then
  # averaged over the four ICU beds so the response is one number per
  # replication.
  icu <- mon$resources %>%
    filter(grepl("^b_r2eheavy_icu_", resource)) %>%
    group_by(replication, resource) %>%
    arrange(time, .by_group = TRUE) %>%
    mutate(dt = lead(time, default = max(time)) - time) %>%
    summarise(mq = weighted.mean(queue, w = pmax(dt, 0), na.rm = TRUE), .groups = "drop") %>%
    group_by(replication) %>%
    summarise(icu_mean_queue = mean(mq), .groups = "drop")

  data.frame(replication = sort(unique(mon$arrivals$replication))) %>%
    left_join(total, by = "replication") %>%
    left_join(dow,   by = "replication") %>%
    left_join(icu,   by = "replication") %>%
    mutate(dow_count = coalesce(as.integer(dow_count), 0L)) %>%
    arrange(replication)
}

#' One independent measurement of a scenario at a given control seed
run_measurement <- function(scenario, seed) {
  json     <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
  env_data <<- build_environment(resolve_scenario(json, scenario))
  day_min  <<- 1440L
  counts   <<- sapply(env_data$elms, length)

  set.seed(seed)
  responses(run_replications(N_REPS, CHECK_DAYS))
}

# ── Checks ──────────────────────────────────────────────────────────────────

RESPONSES <- c(total_casualties = "total casualties (arrival-driven)",
               dow_count        = "died-of-wounds count (treatment-driven)",
               icu_mean_queue   = "R2E ICU mean queue (treatment-driven)")

cat(sprintf("Replication independence check: %s, %d measurement(s) x %d replications x %d days\n",
            SCENARIO, N_MEASURE, N_REPS, CHECK_DAYS))
cat("Failure condition: lag-1 correlation significantly positive (one-sided, 95%)\n\n")
if (quick) {
  cat("QUICK MODE — too few replications to detect a dependence. Wiring test only.\n\n")
}

# Lag-1 pairs are accumulated within each measurement and pooled across them.
# Pairing across a measurement boundary would compare replications from two
# different control seeds, which are independent by construction and would
# dilute the very correlation this check is looking for.
lagged <- setNames(lapply(names(RESPONSES), function(x) list(a = numeric(0), b = numeric(0))),
                   names(RESPONSES))

for (k in seq_len(N_MEASURE)) {
  d <- run_measurement(SCENARIO, CONTROL_SEEDS[k])
  for (col in names(RESPONSES)) {
    x <- d[[col]]
    lagged[[col]]$a <- c(lagged[[col]]$a, x[-length(x)])
    lagged[[col]]$b <- c(lagged[[col]]$b, x[-1])
  }
}

for (col in names(RESPONSES)) {
  a <- lagged[[col]]$a
  b <- lagged[[col]]$b

  if (sd(a) == 0 || sd(b) == 0) {
    cat(sprintf("[SKIP] %s — no variation across replications at this run length\n",
                RESPONSES[[col]]))
    next
  }

  ct <- suppressWarnings(cor.test(a, b))
  # One-sided: a lower bound above zero is a positive dependence, which is
  # what narrows the published intervals.
  ok <- ct$conf.int[1] <= 0

  cat(sprintf("\n%s\n", RESPONSES[[col]]))
  cat(sprintf("  lag-1 r = %+.3f  95%% CI [%+.3f, %+.3f]  n = %d pairs\n",
              ct$estimate, ct$conf.int[1], ct$conf.int[2], length(a)))

  if (!ok) {
    fail(paste0("%s: lag-1 correlation %+.3f (95%% CI [%+.3f, %+.3f]) is significantly ",
                "positive — replications are not independent, so every interval computed ",
                "over them is too narrow"),
         RESPONSES[[col]], ct$estimate, ct$conf.int[1], ct$conf.int[2])
  }
  report(ok, "%s shows no positive dependence between replications", RESPONSES[[col]])

  if (ct$conf.int[2] < 0) {
    cat("  [note] significantly negative — variance reduction, not a defect; the published\n")
    cat("         intervals are conservative rather than too narrow\n")
  }
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All replication independence checks passed.\n")
quit(status = 0)
