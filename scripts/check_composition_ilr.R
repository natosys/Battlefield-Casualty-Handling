#!/usr/bin/env Rscript
##############################################
## scripts/check_composition_ilr.R          ##
## Simplex invariant regression check       ##
##############################################
#
#   Rscript scripts/check_composition_ilr.R
#
# Asserts that every simplex-constrained composition group survives the round
# trip through its balance coordinates: that the transform is invertible at
# the baseline, that a whole Morris design produces valid compositions at
# every design point including the corners of the coordinate box, and that
# apply_params() writes those compositions back into the vars tree where the
# trajectory code reads them. Exits non-zero on failure.
#
# The check exists because the guarantee it tests is what allows these nine
# parameters to be screened at all: a design point whose composition had
# drifted off the simplex would be a run of a configuration nobody chose, and
# would do so silently, since sample() renormalises whatever probability
# vector it is handed.

suppressPackageStartupMessages({
  source("R/environment.R")
  source("R/sensitivity.R")
})

env_data <<- load_elms("env_data.json")
day_min  <<- DAY_MIN

tol      <- 1e-9
failures <- character(0)
#' Print one PASS or FAIL line, recording a failure
#'
#' @param ok Logical: whether the assertion held.
#' @param msg One-line description of the assertion.
#' @return Invisible NULL; called for its side effects.
check    <- function(ok, msg) {
  if (isTRUE(ok)) {
    message(sprintf("  PASS  %s", msg))
  } else {
    message(sprintf("  FAIL  %s", msg))
    failures <<- c(failures, msg)
  }
}

#' Whether a vector is a point on the three-part simplex
#'
#' @param x Numeric vector of the composition's parts.
#' @return TRUE when the vector holds three finite parts, each strictly
#'   inside (0, 1), summing to one within tolerance.
valid_composition <- function(x) {
  length(x) == 3L && all(is.finite(x)) && all(x > 0) && all(x < 1) &&
    abs(sum(x) - 1) < tol
}

# ── 1. The transform inverts at each group's baseline ─────────────────────────

message("\nRound trip at baseline")
for (nm in names(MORRIS_COMPOSITIONS)) {
  g   <- MORRIS_COMPOSITIONS[[nm]]
  b   <- g$baseline / sum(g$baseline)
  err <- max(abs(ilr3_inv(ilr3(b)) - b))
  check(err < tol, sprintf("%s recovers its baseline (max error %.2e)", nm, err))
}

# ── 2. The baseline in env_data.json is the baseline assumed here ─────────────

message("\nBaseline agreement with env_data.json")
observed <- list(
  triage = c(env_data$vars$r1$priority$one,
             env_data$vars$r1$priority$two,
             env_data$vars$r1$priority$three),
  dnbi = c(env_data$vars$r1$other$disease_pct,
           env_data$vars$r1$other$battle_fatigue_pct,
           env_data$vars$r1$other$nbi_pct),
  mass_casualty = c(env_data$vars$mass_casualty$priority$one,
                    env_data$vars$mass_casualty$priority$two,
                    env_data$vars$mass_casualty$priority$three)
)
for (nm in names(MORRIS_COMPOSITIONS)) {
  check(max(abs(observed[[nm]] - MORRIS_COMPOSITIONS[[nm]]$baseline)) < tol,
        sprintf("%s baseline matches env_data.json", nm))
  check(abs(sum(observed[[nm]]) - 1) < tol,
        sprintf("%s sums to 1.0 in env_data.json", nm))
}

# ── 3. Corners of the coordinate box ──────────────────────────────────────────

message("\nCoordinate box corners")
for (nm in names(MORRIS_COMPOSITIONS)) {
  g      <- MORRIS_COMPOSITIONS[[nm]]
  bounds <- morris_params[match(g$coords, morris_params$name), ]
  corners <- expand.grid(z1 = c(bounds$lower[1], bounds$upper[1]),
                         z2 = c(bounds$lower[2], bounds$upper[2]))
  parts <- t(apply(corners, 1, function(z) ilr3_inv(as.numeric(z))))
  check(all(apply(parts, 1, valid_composition)),
        sprintf("%s: all %d corners give a valid composition", nm, nrow(parts)))
  message(sprintf("        realised %s share across the box: %.3f to %.3f",
                  g$parts[1], min(parts[, 1]), max(parts[, 1])))
}

# ── 4. Every design point of a real Morris design ─────────────────────────────

message("\nMorris design points")
set.seed(42)
design <- morris(
  model   = NULL,
  factors = morris_params$name,
  r       = 10,
  design  = list(type = "oat", levels = 4, grid.jump = 2),
  binf    = morris_params$lower,
  bsup    = morris_params$upper,
  scale   = TRUE
)

env_data_base <<- env_data
n_bad <- 0L
for (i in seq_len(nrow(design$X))) {
  p  <- setNames(as.numeric(design$X[i, ]), morris_params$name)
  # apply_params() asserts the invariant itself, so an invalid point raises
  # here rather than reaching the simulation; this re-reads what it wrote to
  # confirm the composition lands where the trajectory code looks for it.
  ed <- tryCatch(apply_params(env_data_base, p), error = function(e) e)
  if (inherits(ed, "condition")) { n_bad <- n_bad + 1L; next }
  written <- list(
    triage = c(ed$vars$r1$priority$one, ed$vars$r1$priority$two,
               ed$vars$r1$priority$three),
    dnbi = c(ed$vars$r1$other$disease_pct, ed$vars$r1$other$battle_fatigue_pct,
             ed$vars$r1$other$nbi_pct),
    mass_casualty = c(ed$vars$mass_casualty$priority$one,
                      ed$vars$mass_casualty$priority$two,
                      ed$vars$mass_casualty$priority$three)
  )
  if (!all(vapply(written, valid_composition, logical(1)))) n_bad <- n_bad + 1L
}
check(n_bad == 0L,
      sprintf("all %d design points wrote three valid compositions (%d bad)",
              nrow(design$X), n_bad))

# ── 5. The baseline design point reproduces the shipped configuration ─────────

message("\nBaseline design point")
p_mode <- setNames(morris_params$mode, morris_params$name)
ed     <- apply_params(env_data_base, p_mode)
check(max(abs(c(ed$vars$r1$priority$one, ed$vars$r1$priority$two,
                ed$vars$r1$priority$three) - observed$triage)) < tol,
      "triage split at the coordinate modes equals the shipped split")
check(max(abs(c(ed$vars$r1$other$disease_pct, ed$vars$r1$other$battle_fatigue_pct,
                ed$vars$r1$other$nbi_pct) - observed$dnbi)) < tol,
      "DNBI composition at the coordinate modes equals the shipped composition")
check(max(abs(c(ed$vars$mass_casualty$priority$one,
                ed$vars$mass_casualty$priority$two,
                ed$vars$mass_casualty$priority$three) - observed$mass_casualty)) < tol,
      "mass casualty split at the coordinate modes equals the shipped split")

# ── 6. Dirichlet sampling stays on the simplex ────────────────────────────────

message("\nDirichlet sampling")
set.seed(42)
for (nm in names(MORRIS_COMPOSITIONS)) {
  g     <- MORRIS_COMPOSITIONS[[nm]]
  parts <- t(apply(rdirichlet_coords(2000, g), 1, ilr3_inv))
  check(all(apply(parts, 1, valid_composition)),
        sprintf("%s: 2000 Dirichlet draws all valid", nm))
  message(sprintf("        concentration %.1f; %s share mean %.3f, 5-95%% %.3f to %.3f",
                  composition_concentration(g), g$parts[1], mean(parts[, 1]),
                  quantile(parts[, 1], 0.05), quantile(parts[, 1], 0.95)))
}

if (length(failures) > 0) {
  message(sprintf("\n%d check(s) FAILED:", length(failures)))
  for (f in failures) message("  - ", f)
  quit(status = 1L)
}
message("\nAll composition checks passed.")
