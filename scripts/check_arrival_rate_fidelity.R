#!/usr/bin/env Rscript
##############################################################################
## scripts/check_arrival_rate_fidelity.R                                    ##
## Regression check — each stream realises the daily mean and variance it   ##
## is configured for                                                        ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_arrival_rate_fidelity.R                # 20,000-day draws
#   Rscript scripts/check_arrival_rate_fidelity.R --days 50000   # tighter band
#   Rscript scripts/check_arrival_rate_fidelity.R --seed 777     # other seed
#
# Exits 0 when every check passes, 1 otherwise, so it can be wired into a
# pre-merge hook or CI step.
#
# Why this check exists. Each stream's `mean_daily` and `sd_daily` are
# planner-facing promises: the configuration names a daily casualty rate and a
# day-to-day variation around it, and the model is expected to generate both.
# Nothing in a run's output checks either promise, and a 30-day run is far too
# short to separate a shortfall from ordinary variability. That is how the
# generators came to be running roughly a fifth below their configured WIA and
# KIA rates without it showing anywhere (Issue #203), and how they came to be
# running at roughly a quarter of the day-to-day variation of even a plain
# Poisson process (Issue #206). Three properties are asserted here:
#
#   1. Each shipped stream's realised long-run daily mean equals its configured
#      mean_daily.
#
#   2. Each shipped stream's realised daily variance equals the variance its
#      construction targets. Arrivals are Poisson within a day whose rate is
#      itself drawn from the configured distribution, so by the law of total
#      variance the target at force size P is
#
#        Var[N] = mu * P / 1000 + (sigma * P / 1000)^2
#
#      the Poisson term plus the between-day term the configuration names. The
#      retired minute walk averaged 1,440 draws into each day and delivered
#      neither: the combat WIA stream realised a daily standard deviation of
#      0.50 against a Poisson 2.10 and a target here of 3.80.
#
#   3. The realised mean does not move when sd_daily alone is edited. These are
#      independently editable configuration fields and one must not move the
#      other. Under the retired per-minute rate cap it did, because what the cap
#      clamped depended on the stream's coefficient of variation.
#
# All three are measured by exercising the shipped generator closures
# themselves over a long horizon, rather than by reasoning about the
# distributions the closures are parameterised from.
#
# Run time is checked as well, on streams parameterised well above any shipped
# mean. Sampling arrival times directly makes the cost linear in the drawn
# rate, where the retired minute walk paid a fixed 1,440 iterations per day; a
# heavy-tailed draw therefore costs time in proportion to the casualties it
# generates, and that is worth asserting rather than trusting.

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

DRAW_DAYS <- as.integer(arg_value("--days", 20000L))
SEED      <- as.integer(arg_value("--seed", 42L))

# Two-sided band on the realised mean, in standard errors. At five the check
# fails by chance far less often than the roughly 20% shortfall it is there to
# catch would pass, with headroom for the skew a lognormal-mixed count carries
# into the sampling distribution of its own mean.
SIGMA_BAND <- 5

# Multiplicative band on the realised variance. A sample variance of a
# lognormal-mixed count is a heavy-tailed estimator — the shipped WIA
# parameterisation has a daily-count kurtosis near a thousand, so even a
# 20,000-day measurement carries a relative error around a fifth — and no
# additive band would be both stable and meaningful. A factor of two is several
# of those standard errors wide while still separating the target decisively
# from what the minute walk delivered, which was low by a factor of 58.
VAR_BAND <- 2

# The force size the streams are measured at. Holding it at 1,000 makes the
# population term exactly 1, so a stream's realised daily mean is its drawn
# rate and its target variance is mu + sigma^2 with no scaling in the way. It
# is also the establishment strength passed as the thinning bound, so the
# dominating rate is the true one and no candidate is rejected.
CHECK_FORCE <- 1000

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

#' Standard deviation of a stream's drawn daily rate
#'
#' @param spec Stream description: mean_daily, sd_daily, distribution
#' @return Standard deviation of the drawn rate
#'
#' @details Nothing trims the draw, so this is the configured standard
#'   deviation for a lognormal stream and the mean for an exponential one, the
#'   exponential being a one-parameter family whose standard deviation equals
#'   its mean. That identity is itself worth stating: it is only true because
#'   the draw is passed through untouched.
rate_sd <- function(spec) {
  if (spec$distribution == "exponential") spec$mean_daily else spec$sd_daily
}

#' Target variance of a stream's daily casualty count
#'
#' @param spec Stream description (see rate_sd())
#' @return Variance the construction targets, at a force size of CHECK_FORCE
#'
#' @details The law of total variance applied to a count that is Poisson
#'   conditional on a randomly drawn daily rate: the mean of the conditional
#'   variance (the rate itself) plus the variance of the conditional mean (the
#'   rate's own variance).
target_var <- function(spec) {
  p <- CHECK_FORCE / 1000
  spec$mean_daily * p + (rate_sd(spec) * p)^2
}

#' Daily casualty counts realised by a generator closure, measured by running it
#'
#' @param spec Stream description (see rate_sd())
#' @param n_days Horizon over which to exercise the closure
#' @return Named list: `counts` (integer vector of casualties per day),
#'   `arrivals` (total the closure emitted), `mean` and `var` (realised daily
#'   mean and variance)
#'
#' @details The force-size global is held at CHECK_FORCE, with no
#'   force-regeneration feedback in the way, so what is measured is the shipped
#'   draw path itself rather than a run's depletion of it. The closure is
#'   driven to exhaustion and its gaps accumulated back into arrival times,
#'   which is the only way to see the day a casualty actually lands in.
measure_stream <- function(spec, n_days) {
  env <<- simmer("rate fidelity check") %>%
    add_global("effective_force_check", CHECK_FORCE)

  gen <- if (spec$distribution == "exponential") {
    make_exp_arrival_generator(spec$mean_daily, "effective_force_check",
                               CHECK_FORCE, n_days)
  } else {
    make_ln_arrival_generator(spec$mean_daily, spec$sd_daily, "effective_force_check",
                              CHECK_FORCE, n_days)
  }

  times <- numeric(0)
  chunk <- numeric(1e5)
  n     <- 0L
  t     <- 0
  repeat {
    gap <- gen()
    if (gap < 0) break
    t <- t + gap
    n <- n + 1L
    if (n > length(chunk)) chunk <- c(chunk, numeric(length(chunk)))
    chunk[n] <- t
  }
  times <- chunk[seq_len(n)]

  # tabulate() over the 1-indexed day each arrival falls in, padded to the full
  # horizon so days with no casualties count as zeroes rather than dropping out
  # of the variance.
  counts <- tabulate(floor(times / day_min) + 1L, nbins = n_days)

  list(counts = counts, arrivals = n, mean = mean(counts), var = var(counts))
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
  band <- SIGMA_BAND * sqrt(target_var(s) / DRAW_DAYS)
  ok   <- abs(m$mean - s$mean_daily) <= band
  report(ok, "%-32s realised %.4f/day against configured %.4f (%.1f%%, band +/- %.4f)",
         s$label, m$mean, s$mean_daily, 100 * m$mean / s$mean_daily, band)
  if (!ok) {
    fail(paste0("%s: realised %.4f casualties/day against a configured mean_daily of %.4f ",
                "(%.1f%% of it), outside the sampling band of +/- %.4f"),
         s$label, m$mean, s$mean_daily, 100 * m$mean / s$mean_daily, band)
  }
}

# ── 2. Each stream realises the daily variance its construction targets ─────
#
# The property Issue #206 exists to restore. Reported against the Poisson
# variance as well as the target, since Poisson is the floor any arrival
# process should clear and the retired minute walk did not.

cat("\n-- each shipped stream's realised daily variance matches its target --\n")

for (i in seq_along(streams)) {
  s      <- streams[[i]]
  m      <- measured[[i]]
  target <- target_var(s)
  ratio  <- m$var / target
  ok     <- ratio >= 1 / VAR_BAND && ratio <= VAR_BAND
  report(ok, "%-32s realised sd %7.3f/day against target %7.3f (Poisson %6.3f, ratio %.2f)",
         s$label, sqrt(m$var), sqrt(target), sqrt(s$mean_daily * CHECK_FORCE / 1000), ratio)
  if (!ok) {
    fail(paste0("%s: realised a daily variance of %.3f against a target of %.3f ",
                "(a factor of %.2f), outside the band of %.0fx. The stream is not ",
                "delivering the day-to-day variation its sd_daily names"),
         s$label, m$var, target, ratio, VAR_BAND)
  }
}

# ── 3. The realised mean is invariant to sd_daily ───────────────────────────
#
# The planner-facing symptom the rate cap used to produce: two fields that are
# independently editable in the Configure panel, one of which moved the other.
# The variance is expected to move with sd_daily, and is reported alongside to
# show that it does.

cat("\n-- realised mean is invariant to sd_daily at fixed mean_daily --\n")

INVARIANCE_MEAN <- 1.77
INVARIANCE_SDS  <- c(0.5, 1.77, 3.56, 8.0)

set.seed(SEED)
inv <- lapply(INVARIANCE_SDS, function(sd_daily) {
  spec <- list(mean_daily = INVARIANCE_MEAN, sd_daily = sd_daily, distribution = "lognormal")
  list(spec = spec, m = measure_stream(spec, DRAW_DAYS))
})

for (r in inv) {
  band <- SIGMA_BAND * sqrt(target_var(r$spec) / DRAW_DAYS)
  ok   <- abs(r$m$mean - INVARIANCE_MEAN) <= band
  report(ok, "sd_daily = %-5.2f (CV %.2f)   realised %.4f/day (band +/- %.4f), daily sd %6.3f",
         r$spec$sd_daily, r$spec$sd_daily / INVARIANCE_MEAN, r$m$mean, band, sqrt(r$m$var))
  if (!ok) {
    fail(paste0("sd_daily = %.2f at mean_daily = %.2f realises %.4f/day, outside the ",
                "band of +/- %.4f: the realised mean still depends on sd_daily"),
         r$spec$sd_daily, INVARIANCE_MEAN, r$m$mean, band)
  }
}

spread <- diff(range(sapply(inv, function(r) r$m$mean)))
cat(sprintf("   spread across sd_daily: %.4f/day over a %.1f-fold range of coefficient of variation\n",
            spread, max(INVARIANCE_SDS) / min(INVARIANCE_SDS)))

# ── 4. Run time stays bounded well above the shipped means ──────────────────
#
# Sampling arrival times directly removes the fixed per-minute cost, so a day
# proposes candidates in proportion to the rate it drew rather than 1,440
# whatever it drew. Cost is therefore linear in the casualties generated, and
# what needs asserting is that the count itself stays at the rate configured
# and the wall time stays workable. This exercises streams two orders of
# magnitude above anything shipped.

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
  elapsed <- system.time(m <- measure_stream(spec, 200L))["elapsed"]
  # Expected arrivals at CHECK_FORCE, which the measurement holds the pool at.
  # A factor of two either side is loose enough not to fail on the sampling
  # variation a 200-day window carries at these coefficients of variation, and
  # tight enough to catch a sampler emitting orders of magnitude more than the
  # rate calls for.
  expected <- spec$mean_daily * 200 * CHECK_FORCE / 1000
  ok <- m$arrivals <= 2 * expected && m$arrivals >= expected / 2 && elapsed < 60
  report(ok, "%-34s %d arrivals over 200 days in %.1fs (expected ~%.0f)",
         spec$label, m$arrivals, elapsed, expected)
  if (!ok) {
    fail("%s emitted %d arrivals over 200 days in %.1fs against an expectation of %.0f",
         spec$label, m$arrivals, elapsed, expected)
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
