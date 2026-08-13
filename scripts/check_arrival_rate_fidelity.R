#!/usr/bin/env Rscript
##############################################################################
## scripts/check_arrival_rate_fidelity.R                                    ##
## Regression check — each stream realises the daily mean it is configured  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_arrival_rate_fidelity.R                # 1000-day draws
#   Rscript scripts/check_arrival_rate_fidelity.R --days 2000    # tighter band
#   Rscript scripts/check_arrival_rate_fidelity.R --seed 777     # other seed
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step.
#
# Why this check exists. Both arrival generators clamp each per-minute rate
# draw at `cap_multiplier × mean_daily`, which is what keeps the closure's
# minute-by-minute walk bounded when a heavy-tailed distribution returns an
# implausible draw. Clamping lowers a mean, so a stream parameterised straight
# from its configured `mean_daily` realises less than it: before Issue #203 the
# shipped streams realised between 78.7% and 99.2% of the rate their
# configuration named, and the shortfall was largest exactly where the model's
# casualty volumes matter most. The generators now solve for the location that
# makes the clamped draw average to the configured mean
# (solve_ln_location()/solve_exp_mean(), R/environment.R).
#
# Two properties follow from that solve, and this check asserts both:
#
#   1. Each shipped stream's realised long-run daily mean equals its configured
#      mean_daily. This is the planner-facing promise of the configuration
#      field, and it is invisible in any single run's output, a 30-day run
#      being far too short to separate a 20% shortfall from ordinary
#      variability.
#
#   2. The realised mean does not move when sd_daily alone is edited. The
#      clamped share of a lognormal's draws depends on its coefficient of
#      variation, so before the correction a planner who edited only the
#      standard deviation changed the stream's realised mean as a side effect.
#      That coupling between two independently editable fields is the symptom
#      a planner would actually meet, and the one most likely to be
#      reintroduced by a later edit to the parameterisation.
#
# Both are measured by exercising the shipped generator closures themselves,
# over a horizon long enough that the sampling band is a small fraction of the
# mean, rather than by re-deriving the truncated mean analytically. The
# analytic identity is checked too, but on its own it would only confirm that
# the solver solves the equation it was given, not that the generator draws
# from the distribution the solver parameterised.
#
# The check is calibrated to fail when the correction is removed rather than
# left to inspection: every stream is measured a second time with
# `bias_correct = FALSE`, and the check fails if any of those uncorrected
# measurements would have passed the band applied to the corrected ones.
#
# Run time is checked as well, on a stream parameterised well above any shipped
# mean. The cap's whole purpose is to bound the closure's iteration count, and
# a correction that raised the location without keeping the clamp would restore
# the unbounded behaviour the cap was introduced to stop.

suppressPackageStartupMessages({
  library(simmer)
  library(simmer.bricks)
  library(triangle)
})

source("R/environment.R")

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

DRAW_DAYS <- as.integer(arg_value("--days", 1000L))
SEED      <- as.integer(arg_value("--seed", 42L))

# Two-sided band on the realised mean, in standard errors of the mean of the
# clamped draws. At four the check fails by chance about once in 15,000
# measurements, which is negligible against the roughly 20% shortfall it is
# there to catch.
SIGMA_BAND <- 4

day_min <<- 1440L

failures <- character(0)
fail     <- function(...) failures <<- c(failures, sprintf(...))

report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}

# ── Streams under test ──────────────────────────────────────────────────────
#
# Every stream the project ships, across the base configuration and both
# scenario profiles, so a profile that overrides a stream's parameters or its
# distribution family is covered rather than assumed to behave like the base.

read_streams <- function(scenario) {
  json <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
  ed   <- build_environment(resolve_scenario(json, scenario))
  gens <- ed$vars$generators
  lapply(names(gens), function(nm) {
    g <- gens[[nm]]
    list(label        = sprintf("%s/%s", scenario, nm),
         mean_daily   = g$mean_daily,
         sd_daily     = g$sd_daily,
         distribution = if (!is.null(g$distribution)) g$distribution else "lognormal")
  })
}

streams <- c(read_streams("default"),
             read_streams("moderate_intensity"),
             read_streams("high_intensity"))

# Drop duplicates: the three profiles share most of their streams, and
# measuring an identical parameterisation three times adds nothing.
signature <- sapply(streams, function(s) {
  sprintf("%s|%.10g|%.10g", s$distribution, s$mean_daily,
          if (is.null(s$sd_daily)) NA_real_ else s$sd_daily)
})
streams <- streams[!duplicated(signature)]

#' Realised daily mean of a generator closure, measured by running it
#'
#' @param spec Stream description: mean_daily, sd_daily, distribution
#' @param n_days Horizon over which to exercise the closure
#' @param bias_correct Passed through to the generator constructor
#' @return Named list: `mean` (realised casualties per day), `se` (standard
#'   error of that estimate), `tol` (the band the check judges it against) and
#'   `arrivals` (count the closure emitted)
#'
#' @details The force-size global is held at 1,000 so a minute's rate is the
#'   drawn value divided by 1,440 and the realised daily mean is exactly the
#'   mean of the clamped draws, with no force-regeneration feedback in the way.
#'   The closure is driven to exhaustion rather than sampled, so what is
#'   measured is the shipped draw path, clamp included.
measure_stream <- function(spec, n_days, bias_correct = TRUE) {
  env <<- simmer("rate fidelity check") %>%
    add_global("effective_force_check", 1000)

  gen <- if (spec$distribution == "exponential") {
    make_exp_arrival_generator(spec$mean_daily, "effective_force_check", n_days,
                               bias_correct = bias_correct)
  } else {
    make_ln_arrival_generator(spec$mean_daily, spec$sd_daily, "effective_force_check",
                              n_days, bias_correct = bias_correct)
  }

  arrivals <- 0L
  repeat {
    if (gen() < 0) break
    arrivals <- arrivals + 1L
  }

  # The closure emits an arrival on each whole-casualty crossing of the
  # accumulated rate, so the count divided by the horizon is the realised
  # daily mean up to the fractional casualty in progress at the end.
  n_minutes <- day_min * n_days
  realised  <- arrivals / n_days

  # Tolerance has two parts. The sampling band is the standard error of a mean
  # over n_minutes clamped draws, taken at SIGMA_BAND. The second term is
  # discretisation: the closure emits whole casualties, so the fractional
  # casualty still accumulating when the horizon ends is never emitted and the
  # count is short by up to one over the whole run. That bias falls as 1/n_days
  # where the sampling band falls as its square root, so it stops mattering
  # first, but at any workable horizon it is the same size as the band and
  # would otherwise fail the check on the low-variability streams.
  sd_clamped <- clamped_sd(spec, bias_correct)
  list(mean     = realised,
       se       = sd_clamped / sqrt(n_minutes),
       tol      = SIGMA_BAND * sd_clamped / sqrt(n_minutes) + 1 / n_days,
       arrivals = arrivals)
}

#' Standard deviation of a stream's clamped per-minute draw, in daily units
#'
#' @param spec Stream description (see measure_stream())
#' @param bias_correct Whether the corrected parameterisation is in force
#' @return Standard deviation of $\min(X, cap)$
#'
#' @details Computed by quadrature over the distribution's own quantiles,
#'   which is exact enough for a sampling band and avoids a second closed form
#'   per distribution family.
clamped_sd <- function(spec, bias_correct = TRUE) {
  cap <- 3 * spec$mean_daily
  p   <- (seq_len(200000) - 0.5) / 200000
  x   <- if (spec$distribution == "exponential") {
    m <- if (bias_correct) solve_exp_mean(spec$mean_daily, cap) else spec$mean_daily
    qexp(p, rate = 1 / m)
  } else {
    sigma_log <- sqrt(log(1 + (spec$sd_daily^2 / spec$mean_daily^2)))
    mu_log <- if (bias_correct) {
      solve_ln_location(spec$mean_daily, sigma_log, cap)
    } else {
      log(spec$mean_daily^2 / sqrt(spec$sd_daily^2 + spec$mean_daily^2))
    }
    qlnorm(p, meanlog = mu_log, sdlog = sigma_log)
  }
  sd(pmin(x, cap))
}

cat(sprintf("Arrival rate fidelity check: %d-day draws, seed %d, %d streams\n\n",
            DRAW_DAYS, SEED, length(streams)))

# ── 1. The solver solves its own equation ───────────────────────────────────

cat("-- the corrected parameterisation's clamped mean equals the configured mean --\n")

for (s in streams) {
  cap <- 3 * s$mean_daily
  analytic <- if (s$distribution == "exponential") {
    capped_exp_mean(solve_exp_mean(s$mean_daily, cap), cap)
  } else {
    sigma_log <- sqrt(log(1 + (s$sd_daily^2 / s$mean_daily^2)))
    capped_lnorm_mean(solve_ln_location(s$mean_daily, sigma_log, cap), sigma_log, cap)
  }
  rel <- abs(analytic - s$mean_daily) / s$mean_daily
  ok  <- rel < 1e-6
  report(ok, "%-32s clamped mean %.6f against configured %.4f (%.2e relative)",
         s$label, analytic, s$mean_daily, rel)
  if (!ok) {
    fail("%s: the solved parameterisation's clamped mean is %.6f, not the configured %.4f",
         s$label, analytic, s$mean_daily)
  }
}

# ── 2. Each stream realises its configured mean when actually run ───────────

cat("\n-- each shipped stream's realised daily mean matches its configuration --\n")

set.seed(SEED)
corrected <- lapply(streams, measure_stream, n_days = DRAW_DAYS)

for (i in seq_along(streams)) {
  s <- streams[[i]]
  m <- corrected[[i]]
  band <- m$tol
  ok   <- abs(m$mean - s$mean_daily) <= band
  report(ok, "%-32s realised %.4f/day against configured %.4f (%.1f%%, band +/- %.4f)",
         s$label, m$mean, s$mean_daily, 100 * m$mean / s$mean_daily, band)
  if (!ok) {
    fail(paste0("%s: realised %.4f casualties/day against a configured mean_daily of %.4f ",
                "(%.1f%% of it), outside the sampling band of +/- %.4f"),
         s$label, m$mean, s$mean_daily, 100 * m$mean / s$mean_daily, band)
  }
}

# ── 3. The check fails when the correction is removed ───────────────────────
#
# Without this the check would pass against a generator that had quietly lost
# the correction, since a band wide enough to absorb sampling noise on a
# 400-day horizon is nowhere near wide enough to absorb a 20% shortfall but
# nothing here would have said so.

cat("\n-- the same measurement rejects the uncorrected parameterisation --\n")

set.seed(SEED)
uncorrected <- lapply(streams, measure_stream, n_days = DRAW_DAYS, bias_correct = FALSE)

for (i in seq_along(streams)) {
  s <- streams[[i]]
  u <- uncorrected[[i]]
  # The band the corrected measurement was judged against, applied to the
  # uncorrected one: that is what "the check catches its own removal" means.
  band <- corrected[[i]]$tol
  rejected <- abs(u$mean - s$mean_daily) > band
  report(rejected, "%-32s uncorrected realises %.4f/day (%.1f%% of configured), %s",
         s$label, u$mean, 100 * u$mean / s$mean_daily,
         if (rejected) "rejected" else "NOT rejected")
  if (!rejected) {
    fail(paste0("%s: the uncorrected parameterisation realises %.4f/day, within the band ",
                "this check applies, so the check would not catch the correction being ",
                "removed from this stream"),
         s$label, u$mean)
  }
}

# ── 4. The realised mean is invariant to sd_daily ───────────────────────────
#
# The planner-facing symptom: two fields that are independently editable in the
# Configure panel, one of which used to move the other.

cat("\n-- realised mean is invariant to sd_daily at fixed mean_daily --\n")

INVARIANCE_MEAN <- 1.77
INVARIANCE_SDS  <- c(0.5, 1.77, 3.56, 8.0)

set.seed(SEED)
inv <- lapply(INVARIANCE_SDS, function(sd_daily) {
  spec <- list(mean_daily = INVARIANCE_MEAN, sd_daily = sd_daily, distribution = "lognormal")
  list(sd = sd_daily,
       corrected   = measure_stream(spec, DRAW_DAYS),
       uncorrected = measure_stream(spec, DRAW_DAYS, bias_correct = FALSE))
})

for (r in inv) {
  band <- r$corrected$tol
  ok   <- abs(r$corrected$mean - INVARIANCE_MEAN) <= band
  report(ok, "sd_daily = %-5.2f (CV %.2f)          realised %.4f/day corrected, %.4f uncorrected",
         r$sd, r$sd / INVARIANCE_MEAN, r$corrected$mean, r$uncorrected$mean)
  if (!ok) {
    fail(paste0("sd_daily = %.2f at mean_daily = %.2f realises %.4f/day, outside the ",
                "band of +/- %.4f: the realised mean still depends on sd_daily"),
         r$sd, INVARIANCE_MEAN, r$corrected$mean, band)
  }
}

corrected_spread   <- diff(range(sapply(inv, function(r) r$corrected$mean)))
uncorrected_spread <- diff(range(sapply(inv, function(r) r$uncorrected$mean)))
cat(sprintf("   spread across sd_daily: %.4f/day corrected against %.4f/day uncorrected\n",
            corrected_spread, uncorrected_spread))
if (corrected_spread >= uncorrected_spread) {
  fail(paste0("the corrected realised mean varies with sd_daily by %.4f/day, no less than the ",
              "uncorrected %.4f/day, so the coupling between the two fields is not removed"),
       corrected_spread, uncorrected_spread)
}

# ── 5. Run time stays bounded well above the shipped means ──────────────────
#
# The correction raises the location, which raises the clamped share, so the
# clamp does more work than it did rather than less. This confirms the clamp is
# still there to do it: the closure's iteration count is the horizon in
# minutes whatever the parameterisation, so a stream two orders of magnitude
# above any shipped mean costs the same walk.

cat("\n-- generator run time stays bounded well above the shipped means --\n")

STRESS <- list(
  list(label = "lognormal mean 50/day, sd 200", mean_daily = 50, sd_daily = 200,
       distribution = "lognormal"),
  list(label = "exponential mean 50/day", mean_daily = 50, distribution = "exponential")
)

set.seed(SEED)
for (spec in STRESS) {
  elapsed <- system.time(m <- measure_stream(spec, 30L))["elapsed"]
  # An unbounded draw would emit arrivals without advancing the horizon; the
  # clamp holds the count at no more than cap * horizon.
  ceiling_count <- 3 * spec$mean_daily * 30
  ok <- m$arrivals <= ceiling_count
  report(ok, "%-32s %d arrivals over 30 days in %.1fs (ceiling %d)",
         spec$label, m$arrivals, elapsed, ceiling_count)
  if (!ok) {
    fail("%s emitted %d arrivals over 30 days, above the clamp's ceiling of %d",
         spec$label, m$arrivals, ceiling_count)
  }
}

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(failures)) {
  cat(sprintf("%d check(s) failed:\n", length(failures)))
  for (f in failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All arrival rate fidelity checks passed.\n")
quit(status = 0)
