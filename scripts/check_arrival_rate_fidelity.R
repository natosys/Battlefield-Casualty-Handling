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
# Why this check exists. Each stream's `mean_daily` is a planner-facing
# promise: the configuration names a daily casualty rate and the model is
# expected to generate it. Nothing in a run's output checks that promise, and a
# 30-day run is far too short to separate a 20% shortfall from ordinary
# variability, which is how the generators came to be running roughly a fifth
# below their configured WIA and KIA rates without it showing anywhere
# (Issue #203). Two properties are asserted here:
#
#   1. Each shipped stream's realised long-run daily mean equals its configured
#      mean_daily.
#
#   2. The realised mean does not move when sd_daily alone is edited. These are
#      independently editable configuration fields and one must not move the
#      other. Under the retired per-minute rate cap it did, because what the cap
#      clamped depended on the stream's coefficient of variation.
#
# Both are measured by exercising the shipped generator closures themselves
# over a long horizon, rather than by reasoning about the distributions the
# closures are parameterised from.
#
# A third property is checked because removing the cap made it the only
# clipping left in the generator. The closure emits at most one arrival per
# simulated minute: when the accumulated rate crosses more than one whole
# casualty within a minute it advances to the new floor and returns a single
# arrival, discarding the rest (R/environment.R). Nothing reports that when it
# happens. At the shipped parameterisations it is vanishingly rare, and this
# check measures how rare rather than assuming it, so a later re-parameterisation
# that pushes a stream close to the ceiling is caught here rather than silently
# under-generating.
#
# Run time is checked as well, on a stream parameterised well above any shipped
# mean. With no cap, the guarantee that a heavy-tailed draw cannot inflate run
# time rests entirely on the closure's structure: it iterates exactly
# n_minutes times across all calls and emits at most one arrival per minute
# whatever the draws. That is worth asserting rather than trusting.

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
# per-minute draws. At four the check fails by chance about once in 15,000
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
#' @return Named list: `mean` (realised casualties per day), `se` (standard
#'   error of that estimate), `tol` (the band the check judges it against) and
#'   `arrivals` (count the closure emitted)
#'
#' @details The force-size global is held at 1,000 so a minute's rate is the
#'   drawn value divided by 1,440 and the realised daily mean is exactly the
#'   mean of the per-minute draws, with no force-regeneration feedback in the
#'   way. The closure is driven to exhaustion rather than sampled, so what is
#'   measured is the shipped draw path itself.
measure_stream <- function(spec, n_days) {
  env <<- simmer("rate fidelity check") %>%
    add_global("effective_force_check", 1000)

  gen <- if (spec$distribution == "exponential") {
    make_exp_arrival_generator(spec$mean_daily, "effective_force_check", n_days)
  } else {
    make_ln_arrival_generator(spec$mean_daily, spec$sd_daily, "effective_force_check",
                              n_days)
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
  # over n_minutes draws, taken at SIGMA_BAND. The second term is
  # discretisation: the closure emits whole casualties, so the fractional
  # casualty still accumulating when the horizon ends is never emitted and the
  # count is short by up to one over the whole run. That bias falls as 1/n_days
  # where the sampling band falls as its square root, so it stops mattering
  # first, but at any workable horizon it is the same size as the band and
  # would otherwise fail the check on the low-variability streams.
  sd_draw <- draw_sd(spec)
  list(mean     = realised,
       se       = sd_draw / sqrt(n_minutes),
       tol      = SIGMA_BAND * sd_draw / sqrt(n_minutes) + 1 / n_days,
       arrivals = arrivals)
}

#' Standard deviation of a stream's per-minute draw, in daily units
#'
#' @param spec Stream description (see measure_stream())
#' @return Standard deviation of the drawn rate
#'
#' @details With no cap this is simply the configured standard deviation for a
#'   lognormal stream, and the mean for an exponential one, the exponential
#'   being a one-parameter family whose standard deviation equals its mean.
#'   That identity is itself worth stating: it is only true because nothing
#'   trims the draw.
draw_sd <- function(spec) {
  if (spec$distribution == "exponential") spec$mean_daily else spec$sd_daily
}

cat(sprintf("Arrival rate fidelity check: %d-day draws, seed %d, %d streams\n\n",
            DRAW_DAYS, SEED, length(streams)))

# ── 1. Each stream realises its configured mean when actually run ───────────

cat("-- each shipped stream's realised daily mean matches its configuration --\n")

set.seed(SEED)
measured <- lapply(streams, measure_stream, n_days = DRAW_DAYS)

for (i in seq_along(streams)) {
  s <- streams[[i]]
  m <- measured[[i]]
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

# ── 2. The realised mean is invariant to sd_daily ───────────────────────────
#
# The planner-facing symptom the rate cap used to produce: two fields that are
# independently editable in the Configure panel, one of which moved the other.

cat("\n-- realised mean is invariant to sd_daily at fixed mean_daily --\n")

INVARIANCE_MEAN <- 1.77
INVARIANCE_SDS  <- c(0.5, 1.77, 3.56, 8.0)

set.seed(SEED)
inv <- lapply(INVARIANCE_SDS, function(sd_daily) {
  spec <- list(mean_daily = INVARIANCE_MEAN, sd_daily = sd_daily, distribution = "lognormal")
  list(sd = sd_daily, m = measure_stream(spec, DRAW_DAYS))
})

for (r in inv) {
  band <- r$m$tol
  ok   <- abs(r$m$mean - INVARIANCE_MEAN) <= band
  report(ok, "sd_daily = %-5.2f (CV %.2f)          realised %.4f/day (band +/- %.4f)",
         r$sd, r$sd / INVARIANCE_MEAN, r$m$mean, band)
  if (!ok) {
    fail(paste0("sd_daily = %.2f at mean_daily = %.2f realises %.4f/day, outside the ",
                "band of +/- %.4f: the realised mean still depends on sd_daily"),
         r$sd, INVARIANCE_MEAN, r$m$mean, band)
  }
}

spread <- diff(range(sapply(inv, function(r) r$m$mean)))
cat(sprintf("   spread across sd_daily: %.4f/day over a %.1f-fold range of coefficient of variation\n",
            spread, max(INVARIANCE_SDS) / min(INVARIANCE_SDS)))

# ── 3. The one-arrival-per-minute ceiling stays out of reach ────────────────
#
# The closure advances to the new floor and returns a single arrival, so a
# minute whose accumulated rate crosses more than one whole casualty discards
# the remainder silently. This is the only clipping left in the generator now
# that the rate cap is gone. It is measured rather than assumed: the check
# reports the probability per minute for each shipped stream at full
# establishment strength, and fails if any stream is close enough to the
# ceiling for the loss to be material over a campaign.

cat("\n-- the one-arrival-per-minute ceiling stays out of reach --\n")

# Full establishment strength, the largest the force-size global ever reads,
# so this is the worst case for each stream.
FORCE <- list(cbt = 2500, spt = 1250)

# One casualty in one minute needs a drawn daily rate this large, per 1,000
# personnel at the pool's establishment strength.
ceiling_rate <- function(force) day_min * 1000 / force

# A stream losing more than this many casualties over a 30-day run would be
# materially under-generating; at the shipped parameterisations the expected
# loss is many orders of magnitude below it.
LOSS_BUDGET <- 0.01

for (s in streams) {
  force <- if (grepl("_spt$", s$label)) FORCE$spt else FORCE$cbt
  x_break <- ceiling_rate(force)
  p <- if (s$distribution == "exponential") {
    exp(-x_break / s$mean_daily)
  } else {
    sigma_log <- sqrt(log(1 + (s$sd_daily^2 / s$mean_daily^2)))
    mu_log    <- log(s$mean_daily^2 / sqrt(s$sd_daily^2 + s$mean_daily^2))
    pnorm((log(x_break) - mu_log) / sigma_log, lower.tail = FALSE)
  }
  expected_loss <- p * day_min * 30
  ok <- expected_loss < LOSS_BUDGET
  report(ok, "%-32s P(>1 arrival in a minute) = %.1e, expected loss %.2e per 30-day run",
         s$label, p, expected_loss)
  if (!ok) {
    fail(paste0("%s: a minute's draw crosses more than one whole casualty with probability ",
                "%.2e, so about %.3f casualties per 30-day run are discarded by the closure's ",
                "one-arrival-per-minute emission without being reported anywhere"),
         s$label, p, expected_loss)
  }
}

# ── 4. Run time stays bounded well above the shipped means ──────────────────
#
# With the cap gone this is the whole of the guarantee that a heavy-tailed
# draw cannot inflate run time, so it is asserted directly: the closure emits
# at most one arrival per simulated minute whatever the draws.

cat("\n-- generator run time stays bounded well above the shipped means --\n")

STRESS <- list(
  list(label = "lognormal mean 50/day, sd 200", mean_daily = 50, sd_daily = 200,
       distribution = "lognormal"),
  list(label = "lognormal mean 500/day, sd 5000", mean_daily = 500, sd_daily = 5000,
       distribution = "lognormal"),
  list(label = "exponential mean 500/day", mean_daily = 500, distribution = "exponential")
)

set.seed(SEED)
for (spec in STRESS) {
  elapsed <- system.time(m <- measure_stream(spec, 30L))["elapsed"]
  ceiling_count <- day_min * 30
  ok <- m$arrivals <= ceiling_count
  report(ok, "%-32s %d arrivals over 30 days in %.1fs (ceiling %d)",
         spec$label, m$arrivals, elapsed, ceiling_count)
  if (!ok) {
    fail("%s emitted %d arrivals over 30 days, above the closure's ceiling of %d",
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
