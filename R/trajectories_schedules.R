##############################################
## R/trajectories_schedules.R             ##
## Reinforcement and airlift schedules    ##
##############################################
#
# The scheduled trajectories that are not casualties: force
# reinforcement and the strategic aeromedical evacuation sortie.
#
# Sourced by R/trajectories.R, which loads the packages and R/constants.R
# these functions use; source that file rather than this one alone.

# ── Force reinforcement ─────────────────────────────────────────────────────────

#' Builds the reinforcement demand/fulfillment trajectory
#'
#' @return A simmer trajectory representing one reinforcement demand cycle:
#'   at each firing (a "submission"), computes each pool's shortfall against
#'   its initial establishment strength, draws a fill fraction of that
#'   shortfall from a triangular distribution, waits
#'   fulfillment_lag_days, then credits the drawn amount to the pool.
#'
#' @details Driven by its own generator in run_once() (R/replication.R),
#'   scheduled every demand_interval_days (env_data$vars$force_regeneration$
#'   reinforcement) — this is the one genuinely periodic/daily-cycle
#'   mechanism in the force regeneration model (injury/RTD
#'   crediting is continuous — see debit_force_size()/credit_rtd() above).
#'
#'   Demand is assessed at submission time (shortfall = initial - current
#'   effective force, floored at 0 — a pool already at or above full
#'   strength submits no demand and fills nothing). The actual fill amount
#'   is also drawn at submission time (rather than at fulfillment), modelling
#'   the outcome of a reinforcement request being substantially known when
#'   submitted, with delivery simply delayed — not a second, independent
#'   source of uncertainty layered on top of delivery timing.
#'
#'   fill_min_frac/fill_mode_frac/fill_max_frac parameterise a
#'   Triangular(min, mode, max) distribution over the fraction of demand
#'   actually delivered: mode_frac close to (but below) 1 with min_frac far
#'   below it gives a long left tail (severe under-fill is more probable
#'   than a request being fully met), while max_frac only slightly above 1
#'   models a single, non-overlapping fulfillment slightly overshooting its
#'   own submission-time shortfall estimate. demand_interval_days <= 0 (the
#'   shipped default) disables reinforcement entirely — no generator is
#'   added in run_once() in that case, so no RNG draws are consumed,
#'   reproducing a constant-force baseline exactly.
#'
#'   A pool's global value only moves at credit time, so a
#'   naive re-read of the live shortfall on every cycle would let
#'   overlapping cycles (demand_interval_days < fulfillment_lag_days)
#'   independently re-claim the same shortfall an earlier, still-pending
#'   cycle already committed to filling — and, even in the single-cycle
#'   case, crediting the submission-time fill amount unconditionally
#'   (rather than re-checking it against the shortfall actually remaining
#'   fulfillment_lag_days later) could push the pool above its initial
#'   establishment strength. Two guards address this:
#'     - A per-pool "pending" global (reinf_*_pending, R/replication.R)
#'       tracks fill amounts already committed to an in-flight
#'       (submitted-but-not-yet-credited) cycle: incremented by the drawn
#'       fill at submission, decremented by that same amount at credit
#'       time. demand_fn() nets this out of the live shortfall, so a new
#'       cycle only claims shortfall no earlier cycle has already claimed.
#'       Because pending tracks the fill actually committed rather than
#'       the full demand, an under-filled cycle's uncovered remainder is
#'       never removed from view — it stays visible to the next demand
#'       computation instead of being silently written off.
#'     - The whole of a cycle's fill is credited on delivery, whatever the
#'       live shortfall has done during the fulfillment lag.
#'
#'   Reinforcement joins the population the moment it arrives.
#'   There is no formation-level reserve in this model to hold it in and
#'   nothing about a delivery is held back, so a pool that receives more
#'   than its remaining shortfall goes over establishment strength and
#'   stays there until casualties bring it back down. That is what a fill
#'   fraction above 1 means, and crediting it in full is what makes the
#'   parameter's realised effect equal to its configured one. Over-strength
#'   is a transient rather than a new equilibrium: demand is the shortfall
#'   floored at zero, so an over-strength pool requests nothing and no
#'   later cycle can build on the surplus.
#'
#'   The headroom this needs is not free. Both arrival generators sample by
#'   thinning against a dominating rate that fixes the force size at a
#'   bound, so the bound has to cover the largest over-strength a
#'   configuration can reach or the stream quietly under-generates above
#'   it. reinforcement_force_bound() (R/environment.R) computes that bound
#'   and run_once() passes it wherever reinforcement is enabled.

#' Rejects a reinforcement fill distribution rtriangle() could not draw from
#'
#' @return Invisibly TRUE when the configured distribution is drawable
#'
#' @details rtriangle() requires a <= c <= b and returns NA rather than
#'   erroring when that is violated, so an inverted configuration would
#'   otherwise surface as a missing fill much later, or as the NA cascade
#'   through every downstream KPI that a screening run once hit. No
#'   bound is placed on how large the fractions may be: a fill fraction above 1
#'   is a reinforcement package larger than the shortfall it was requested
#'   against, which the model delivers in full.
validate_fill_distribution <- function() {
  p <- env_data$vars$force_regeneration$reinforcement
  bounds <- c(fill_min_frac = p$fill_min_frac, fill_mode_frac = p$fill_mode_frac,
              fill_max_frac = p$fill_max_frac)

  if (!(bounds[["fill_min_frac"]] <= bounds[["fill_mode_frac"]] &&
        bounds[["fill_mode_frac"]] <= bounds[["fill_max_frac"]])) {
    stop(sprintf(paste0("force_regeneration.reinforcement: the fill distribution needs ",
                        "fill_min_frac <= fill_mode_frac <= fill_max_frac, but reads ",
                        "%.3f, %.3f, %.3f."),
                 bounds[["fill_min_frac"]], bounds[["fill_mode_frac"]],
                 bounds[["fill_max_frac"]]),
         call. = FALSE)
  }

  invisible(TRUE)
}

#' Build the force reinforcement trajectory
#'
#' @return A simmer trajectory that claims, delivers and credits one
#'   reinforcement package for each of the two populations.
#' @details A package is claimed against a pending global at request, held for
#'   the configured fulfilment lag, then credited to the population and
#'   released from pending, so a demand raised while an earlier package is in
#'   transit does not request the same shortfall twice.
build_reinforcement_trajectory <- function() {
  validate_fill_distribution()

  #' Build the closure computing one population's outstanding shortfall
  #'
  #' @param pool_global Name of the global holding the population's strength.
  #' @param pending_global Name of the global holding packages in transit.
  #' @param initial The population's establishment strength.
  #' @return A closure of no arguments returning the shortfall, floored at 0.
  #' @details Evaluated at run time by simmer, so the shortfall is the one
  #'   standing when the demand is raised rather than at trajectory build.
  demand_fn <- function(pool_global, pending_global, initial) {
    function() {
      max(0, initial - get_global(env, pool_global) - get_global(env, pending_global))
    }
  }

  #' Build the closure drawing one reinforcement package's size
  #'
  #' @param demand_attr Name of the attribute holding the shortfall demanded.
  #' @return A closure of no arguments returning the package size, in people.
  #' @details The fraction of the demand that is filled is drawn from the
  #'   configured triangular distribution, whose bounds
  #'   `validate_fill_distribution()` has already checked.
  fill_fn <- function(demand_attr) {
    function() {
      params <- env_data$vars$force_regeneration$reinforcement
      demand <- get_attribute(env, demand_attr)
      frac <- rtriangle(
        n = 1,
        a = params$fill_min_frac,
        b = params$fill_max_frac,
        c = params$fill_mode_frac
      )
      round(demand * frac)
    }
  }

  #' Build the closure claiming a package against the pending pool
  #'
  #' @param pending_global Name of the global holding packages in transit.
  #' @param fill_attr Name of the attribute holding the package size.
  #' @return A closure of no arguments returning the raised pending total.
  claim_fn <- function(pending_global, fill_attr) {
    function() get_global(env, pending_global) + get_attribute(env, fill_attr)
  }

  #' Build the closure releasing a delivered package from the pending pool
  #'
  #' @param pending_global Name of the global holding packages in transit.
  #' @param fill_attr Name of the attribute holding the package size.
  #' @return A closure of no arguments returning the lowered pending total.
  release_fn <- function(pending_global, fill_attr) {
    function() get_global(env, pending_global) - get_attribute(env, fill_attr)
  }

  #' Build the closure crediting a delivered package to the population
  #'
  #' @param pool_global Name of the global holding the population's strength.
  #' @param fill_attr Name of the attribute holding the package size.
  #' @return A closure of no arguments returning the raised strength.
  #' @details The whole delivery joins the population with no ceiling applied:
  #'   a pool receiving more than its remaining shortfall is over strength
  #'   until casualties bring it back down, which is the state a fill fraction
  #'   above one exists to produce.
  credit_fn <- function(pool_global, fill_attr) {
    function() get_global(env, pool_global) + get_attribute(env, fill_attr)
  }

  trajectory("Force Reinforcement") %>%
    set_attribute("reinf_combat_demand",
      demand_fn("effective_force_combat", "reinf_combat_pending",
                env_data$pops$combat)) %>%
    set_attribute("reinf_support_demand",
      demand_fn("effective_force_support", "reinf_support_pending",
                env_data$pops$support)) %>%
    set_attribute("reinf_combat_fill", fill_fn("reinf_combat_demand")) %>%
    set_attribute("reinf_support_fill", fill_fn("reinf_support_demand")) %>%
    set_global("reinf_combat_pending", claim_fn("reinf_combat_pending", "reinf_combat_fill")) %>%
    set_global("reinf_support_pending", claim_fn("reinf_support_pending", "reinf_support_fill")) %>%
    timeout(function() {
      env_data$vars$force_regeneration$reinforcement$fulfillment_lag_days * day_min
    }) %>%
    set_global("effective_force_combat",
      credit_fn("effective_force_combat", "reinf_combat_fill")) %>%
    set_global("effective_force_support",
      credit_fn("effective_force_support", "reinf_support_fill")) %>%
    set_global("reinf_combat_pending", release_fn("reinf_combat_pending", "reinf_combat_fill")) %>%
    set_global("reinf_support_pending", release_fn("reinf_support_pending", "reinf_support_fill"))
}

# ── Strategic AME (aeromedical evacuation) sortie schedule ────────────────────────
#
# Two capacity pools, "ame" for the standard seat and the smaller
# "ame_critical" for the critical care one, share a single sortie schedule.
# README Role 4 (National Support Base) Demand Modelling states the doctrinal
# basis for the split and which casualty each pool is for.
#
# What matters here is that a sortie fills both pools at once rather than
# trading one against the other, the configured airframe carrying both
# categories on the same flight. Capacity is the fitted patient capacity of
# the airframe the run is configured to fly, resolved by
# resolve_ame_airframe() (R/environment.R) from env_data$vars$role4, so
# changing the airframe changes both pools together.

#' Builds the AME sortie trajectory: one firing = one scheduled sortie
#' opportunity, which either flies — adding the configured airframe's
#' critical and standard capacity to the "ame" (standard) and
#' "ame_critical" (CCATT/CCAST-supported) resources — or is cancelled
#' (weather, tasking, airframe availability, etc.) and adds nothing to
#' either.
#'
#' @return A simmer trajectory
#'
#' @details Driven by its own generator in run_once() (R/replication.R),
#'   scheduled every schedule_interval_days
#'   (env_data$vars$role4$ame$schedule_interval_days). Capacity is additive
#'   (mod = "+"), never reset to an absolute value: casualties who seize
#'   "ame"/"ame_critical" (r2e_treat_wia()'s Strategic Evac branch, below)
#'   never release it, so a fixed/reset capacity value would permanently
#'   cap total-ever-admitted at that value the first time the server count
#'   catches up to it — the resource would never "reopen" for a later
#'   sortie. Additive capacity avoids this, at the cost of a simplification
#'   documented as MODEL ASSUMPTION — AME Capacity Banking in the README:
#'   unclaimed capacity from an under-subscribed sortie is not lost, and
#'   can be claimed by a later arrival even between scheduled sorties,
#'   rather than being wasted the way a real aircraft's empty seats would
#'   be once it departs.
build_ame_sortie_trajectory <- function() {
  airframe <- resolve_ame_airframe(env_data$vars$role4)

  trajectory("AME Sortie") %>%
    branch(
      option = function() {
        if (runif(1) < env_data$vars$role4$ame$failure_probability) return(2)
        return(1)
      },
      continue = TRUE,
      trajectory("Sortie Flies") %>%
        set_capacity("ame", value = airframe$standard_capacity, mod = "+") %>%
        set_capacity("ame_critical", value = airframe$critical_capacity, mod = "+"),
      trajectory("Sortie Cancelled")
    )
}
