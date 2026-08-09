#!/usr/bin/env Rscript
##############################################################################
## scripts/check_replication_independence.R                                 ##
## Regression check — replications are independent of one another           ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_replication_independence.R              # structural checks
#   Rscript scripts/check_replication_independence.R --diagnostic # adds 3 x 60 reps
#   Rscript scripts/check_replication_independence.R --days 10    # shorter runs
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step. The structural checks run a handful of short
# simulations and finish in a couple of minutes. --diagnostic adds a further
# 180 replications, roughly half an hour on four cores, and gates nothing.
#
# Why this check exists: every confidence interval this project publishes is
# computed as qt(0.975, df = n - 1) * sd / sqrt(n) with n set to the number of
# replications, which is correct only if the replications are independent.
# They were not. run_replications() used to pair them, (2k-1, 2k) sharing a
# seed with the even member negating its arrival-generation uniforms, so the
# pair and not the replication was the unit the design supplied while every
# interval went on dividing by the replication count. The point estimates
# survived that, the mean of paired observations being unbiased; the widths did
# not. The pairing was withdrawn (Issue #189, README — Multi-run Replication
# Framework) and this check is what keeps it withdrawn.
#
# What it asserts, and why structurally rather than statistically. Two
# properties together make the replications independent, and each is checked
# directly:
#
#   1. run_once() is a pure function of its seed. Re-running a seed reproduces
#      its output exactly, even with a different replication run in between,
#      so there is no channel by which one replication could influence another.
#      The seed is the whole of what distinguishes two replications.
#
#   2. run_replications() gives every replication its own seed, drawn from the
#      parent stream. Sharing a seed between adjacent replications is precisely
#      the withdrawn pairing, and is what this would catch.
#
# Given (1), (2) is sufficient: independent seeds into a pure function give
# independent outputs. That is a stronger statement than any correlation test
# on a finite sample can make, and it is deterministic, so this check does not
# pass or fail on the luck of a particular seed set.
#
# The --diagnostic mode measures the lag-1 rank correlation across replications
# anyway, on an arrival-driven response and two treatment-driven ones, and
# reports it without gating on it. That is deliberate. Independence follows
# from the two properties above, so a correlation measured over 177 pairs adds
# no evidence about it, while treating it as a gate would fail the check
# roughly one run in twenty by chance. It would do exactly that here: at the
# shipped control seeds the R2E ICU mean queue shows a lag-1 rank correlation
# of +0.18 (p = 0.02), while the same series shows -0.16 (p = 0.03) at lag 3,
# which is the signature of a skewed, heavy-tailed response on a finite sample
# rather than of any dependence. That figure is identical whether replications
# are prescheduled into shared mclapply forks or run one fork per job, and
# identical to what the withdrawn pairing produced, both of which confirm it
# carries no information about the replication scheme.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
  library(dplyr)
})

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

SCENARIO   <- arg_value("--scenario", "default")
CHECK_DAYS <- as.integer(arg_value("--days", 10L))
DIAGNOSTIC <- "--diagnostic" %in% args
N_MEASURE  <- as.integer(arg_value("--measurements", 3L))
N_REPS     <- as.integer(arg_value("--reps", 60L))
DIAG_DAYS  <- as.integer(arg_value("--diagnostic-days", 30L))
N_CHECK_REPS <- 6L

CONTROL_SEEDS <- c(42L, 777L, 20260808L, 13L, 20261L)

failures <- character(0)
fail     <- function(...) failures <<- c(failures, sprintf(...))

report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}

json     <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
env_data <<- build_environment(resolve_scenario(json, SCENARIO))
day_min  <<- 1440L
counts   <<- sapply(env_data$elms, length)

#' A run's observable output, reduced to something comparable between runs
#'
#' @param seed Random seed passed to run_once()
#' @return Named list: `n` (arrival count) and `digest` (a string over every
#'   arrival's name and end time)
#'
#' @details Arrival identities and end times together cover both the arrival
#'   process and everything downstream of it that sets how long a casualty
#'   stays in the model, so two runs agreeing on this agree on the whole
#'   trajectory. run_once() writes a per-arrival trace to stdout that is not
#'   wanted here.
run_signature <- function(seed) {
  invisible(capture.output(wrapped <- run_once(n_days = CHECK_DAYS, seed = seed)))
  arr <- get_mon_arrivals(wrapped, ongoing = TRUE)
  arr <- arr[order(arr$name), ]
  list(n = nrow(arr),
       digest = paste(sprintf("%s:%.10f", arr$name, arr$end_time), collapse = "|"))
}

cat(sprintf("Replication independence check: %s, %d-day runs\n\n", SCENARIO, CHECK_DAYS))

# ── 1. run_once() is a pure function of its seed ────────────────────────────
#
# The interleaved run at a second seed is the point of this: if any state
# survived from one run into the next, running a different seed in between
# would disturb the repeat and the two runs of the first seed would diverge.

cat("-- run_once() is a pure function of its seed --\n")

a1 <- run_signature(CONTROL_SEEDS[1])
b1 <- run_signature(CONTROL_SEEDS[2])
a2 <- run_signature(CONTROL_SEEDS[1])

same_seed_reproduces <- identical(a1$digest, a2$digest)
if (!same_seed_reproduces) {
  fail(paste0("run_once() is not a pure function of its seed: seed %d gave %d arrivals and ",
              "then %d when re-run after an intervening run at seed %d. State is surviving ",
              "between runs, so replications sharing a worker process are not independent ",
              "whatever their seeds"),
       CONTROL_SEEDS[1], a1$n, a2$n, CONTROL_SEEDS[2])
}
report(same_seed_reproduces,
       "seed %d reproduces exactly across an intervening run at seed %d (%d arrivals)",
       CONTROL_SEEDS[1], CONTROL_SEEDS[2], a1$n)

different_seeds_differ <- !identical(a1$digest, b1$digest)
if (!different_seeds_differ) {
  fail("run_once() gave identical output at seeds %d and %d, so the seed is not reaching the model",
       CONTROL_SEEDS[1], CONTROL_SEEDS[2])
}
report(different_seeds_differ,
       "seeds %d and %d give different runs, so the seed reaches the model",
       CONTROL_SEEDS[1], CONTROL_SEEDS[2])

# ── 2. run_replications() gives every replication its own seed ──────────────
#
# Given (1), this is what makes the replications independent, and a repeated
# seed is what the withdrawn antithetic pairing looked like: partners (2k-1,
# 2k) shared one.

cat("\n-- run_replications() draws one distinct seed per replication --\n")

set.seed(CONTROL_SEEDS[1])
invisible(capture.output(mon <- run_replications(N_CHECK_REPS, CHECK_DAYS)))

seeds_reported <- !is.null(mon$seeds)
if (!seeds_reported) {
  fail("run_replications() no longer reports the seeds it used, so this check cannot verify them")
}
report(seeds_reported, "run_replications() reports its per-replication seeds")

if (seeds_reported) {
  n_seeds  <- length(mon$seeds)
  n_unique <- length(unique(mon$seeds))
  distinct <- n_unique == n_seeds && n_seeds == N_CHECK_REPS

  if (!distinct) {
    repeated <- unique(mon$seeds[duplicated(mon$seeds)])
    fail(paste0("run_replications() used %d seeds for %d replications, %d of them distinct. ",
                "A seed serving more than one replication makes the pair, not the replication, ",
                "the unit the design supplies, and every interval dividing by the replication ",
                "count is then too narrow. Repeated: %s"),
         n_seeds, N_CHECK_REPS, n_unique,
         if (length(repeated)) paste(repeated, collapse = ", ") else "none")
  }
  report(distinct, "%d replications ran under %d distinct seeds", N_CHECK_REPS, n_unique)
}

# ── 3. Diagnostic: lag-1 correlation across replications (gates nothing) ────

if (DIAGNOSTIC) {
  cat(sprintf("\n-- Diagnostic: lag-1 rank correlation, %d x %d replications x %d days --\n",
              N_MEASURE, N_REPS, DIAG_DAYS))
  cat("   Reported, not asserted: independence follows from the two checks above.\n")

  responses <- function(mon) {
    total <- mon$arrivals %>% count(replication, name = "total_casualties")
    dow   <- mon$attributes %>%
      filter(key == "dow", value == 1) %>%
      count(replication, name = "dow_count")
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

  RESPONSES <- c(total_casualties = "total casualties (arrival-driven)",
                 dow_count        = "died-of-wounds count (treatment-driven)",
                 icu_mean_queue   = "R2E ICU mean queue (treatment-driven)")

  series <- setNames(lapply(names(RESPONSES), function(x) list()), names(RESPONSES))
  for (k in seq_len(N_MEASURE)) {
    set.seed(CONTROL_SEEDS[k])
    invisible(capture.output(m <- run_replications(N_REPS, DIAG_DAYS)))
    d <- responses(m)
    for (col in names(RESPONSES)) series[[col]][[k]] <- d[[col]]
  }

  for (col in names(RESPONSES)) {
    # Each measurement contributes its own lag-1 pairs. Pairing across a
    # measurement boundary would compare replications from different control
    # seeds, which share nothing at all.
    a <- unlist(lapply(series[[col]], function(x) head(x, -1)))
    b <- unlist(lapply(series[[col]], function(x) tail(x, -1)))
    if (sd(a, na.rm = TRUE) == 0) {
      cat(sprintf("   %-42s no variation at this run length\n", RESPONSES[[col]]))
      next
    }
    sp <- suppressWarnings(cor.test(a, b, method = "spearman"))
    cat(sprintf("   %-42s rho = %+.3f (p = %.3f, n = %d pairs)\n",
                RESPONSES[[col]], sp$estimate, sp$p.value, length(a)))
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
