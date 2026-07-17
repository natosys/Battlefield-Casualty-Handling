##############################################
## R/sensitivity.R                          ##
## Morris EE screening + Sobol follow-up   ##
##############################################

library(sensitivity)
library(dplyr)
library(tidyr)
library(ggplot2)

# ── Plotting helpers ──────────────────────────────────────────────────────────

#' Colour assignment for the Context/Capacity/Policy parameter category split
#'
#' @details Three-colour subset of the Okabe-Ito colourblind-safe palette
#'   (Okabe & Ito, 2008). Orange for Context (unchanged from the original
#'   two-way split); Design's two sub-categories get their own distinct
#'   hues rather than shades of one colour, since the Capacity/Policy
#'   distinction is the point of splitting them apart in the first place —
#'   a shade difference reads as "still basically Design" at a glance,
#'   which is exactly what Issue #112's second follow-up asked not to do.
MORRIS_CATEGORY_COLORS <- c(
  "Scenario / Casualty Context"        = "#E69F00",
  "Health System Design - Capacity"    = "#009E73",
  "Health System Design - Policy"      = "#0072B2"
)

#' Short morris_params$category code -> full plot-legend label
MORRIS_CATEGORY_LABELS <- c(
  "Context"  = "Scenario / Casualty Context",
  "Capacity" = "Health System Design - Capacity",
  "Policy"   = "Health System Design - Policy"
)

#' Render a Morris mu*/sigma scatter plot with overlap-avoiding, category-coloured labels
#'
#' @param obj A tell()-populated morris object (has a populated $ee matrix)
#' @param title Plot title
#' @return A ggplot2 object
#'
#' @details Base R's plot.morris() (the original implementation here) places
#'   every parameter's label with text() at a fixed offset with no collision
#'   avoidance — unreadable once the screen grew past roughly 15 parameters
#'   and became a dense, illegible cluster at p = 55 (Issue #112 follow-up).
#'   ggrepel::geom_text_repel() displaces overlapping labels and draws a
#'   thin leader line back to the point they belong to instead.
#'
#'   Points and labels are also coloured by `morris_params$category` (Issue
#'   #112 second follow-up) — three levels, not two: "Scenario / Casualty
#'   Context" (a fact about the operational environment or casualty
#'   population: generation rates, DOW calibration, clinical-need
#'   composition, treatment efficacy — not a planner's to choose); "Health
#'   System Design — Capacity" (a throughput/process time, changeable only
#'   through resourcing investment, not a standing-order decision); "Health
#'   System Design — Policy" (a threshold, cadence, or scheduling rule the
#'   health system's own standing orders set directly, no investment
#'   required). The original Context-vs-Design two-way split conflated the
#'   latter two — "a highly-ranked duration and a highly-ranked scheduling
#'   threshold are not equally actionable," per the issue that asked for
#'   this refinement — a planner can rewrite a threshold today, but cannot
#'   simply command a procedure to take less time. See the category
#'   column's own comment in `morris_params` for the assignment rule and
#'   its limits.
plot_morris_scatter <- function(obj, title) {
  ee      <- obj$ee
  mu_star <- apply(abs(ee), 2, mean, na.rm = TRUE)
  sigma   <- apply(ee, 2, sd, na.rm = TRUE)
  df <- data.frame(parameter = colnames(ee), mu_star = mu_star, sigma = sigma)
  df$category <- MORRIS_CATEGORY_LABELS[morris_params$category[match(df$parameter, morris_params$name)]]

  if (all(!is.finite(df$mu_star)) || all(!is.finite(df$sigma))) {
    stop("insufficient variation to plot")
  }

  ggplot(df, aes(x = mu_star, y = sigma, label = parameter, color = category)) +
    geom_point(size = 2) +
    ggrepel::geom_text_repel(
      size = 3, max.overlaps = Inf, segment.size = 0.25,
      show.legend = FALSE, min.segment.length = 0,
      box.padding = 0.3, point.padding = 0.15, seed = 42
    ) +
    scale_color_manual(values = MORRIS_CATEGORY_COLORS, name = NULL) +
    # expression(), not a literal "μ*"/"σ" string: ggplot2's Cairo/PNG device
    # in this project's containerised environments has repeatedly lacked a
    # font covering the Greek-letter Unicode codepoints (rendering as blank
    # tofu boxes), where base R's plotmath typesets the same symbols as
    # vector glyphs independent of font coverage — matching the axis labels
    # base R's plot.morris() (the function this replaced) always produced.
    labs(title = title,
         x = expression(mu * "* (importance)"),
         y = expression(sigma * " (nonlinearity / interaction)")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}

# ── Parameter definitions ─────────────────────────────────────────────────────

#' Parameter bounds for Morris Elementary Effects screening
#'
#' @format Data frame with columns: name, lower, upper, mode (current baseline value)
#'
#' @details Fifty-three parameters (Issue #112 full-coverage audit, expanded
#'   from the original eleven, then reduced from an intermediate fifty-five
#'   by a same-issue follow-up review — see the exclusion note below) span
#'   treatment durations (surgery, resuscitation, ICU, holding), DOW
#'   probability (both the full P1/P2 logistic curve and treatment-efficacy
#'   multipliers), evacuation transport times, surgical/evacuation decision
#'   probabilities, in-theatre recovery rate, OT shift availability, mass
#'   casualty event rate/size (Issue #9), force regeneration reinforcement
#'   timing (Issue #18), strategic AME sortie cadence (Issue #23), and
#'   casualty generation rates. Bounds are set to cover clinically plausible
#'   variation around the current baseline using one of two rules, applied
#'   per-parameter based on how well its baseline value is externally
#'   grounded (see the `source` citations in `R/app_params.R` where a
#'   matching field exists):
#'     Rule A (citation-anchored, moderate uncertainty): baseline +/-40%.
#'     Rule B (informed estimate / "not literature-derived" / no doctrinal
#'       source per its own citation): baseline x 0.5-2.0 (multiplicative),
#'       or baseline +/-0.15-0.25 for mid-range [0,1] probabilities.
#'   See README Sensitivity Analysis section for the full per-parameter
#'   derivation and the parameter-surface diff this expansion is based on.
#'
#'   `icu_defer_check_interval` (R2B/R2E OT-entry defer poll, Issue #43) and
#'   `ame_dow_check_interval` (strategic-AME-wait DOW poll, Issue #23 third
#'   follow-up) were screened in the initial Issue #112 pass but removed by
#'   a same-issue follow-up review: both are `timeout()`+`rollback()`
#'   polling-loop intervals — a numerical discretisation of what is
#'   conceptually continuous monitoring (ICU-bed availability, DOW risk
#'   while queued), not a real standing order a health system issues about
#'   "how often to check." Unlike `ame_schedule_interval_days` (a genuine
#'   sortie-cadence decision), labelling a poll interval "Policy" implies a
#'   planner can pull that lever in reality, which is misleading. See the
#'   README's "Parameters Excluded from Screening" note.
#'
#'   `fr_fill_mode_frac`'s upper bound is capped at 1.05, not the 1.4
#'   Rule-B-multiplicative bound Rule B would otherwise give (baseline
#'   0.85 x 2.0 = 1.7, clipped to the field's own registry max of 1.5)
#'   — env_data.json's `fill_min_frac`/`fill_max_frac` (0.2/1.1) are NOT
#'   screened here (only the triangular mode is, matching every other
#'   triangular parameter's convention), so they stay fixed at those
#'   values for every design point. `fill_fn()` (R/trajectories.R) calls
#'   `rtriangle(n=1, a=fill_min_frac, b=fill_max_frac, c=fill_mode_frac)`,
#'   which requires a <= c <= b; screening fill_mode_frac past 1.1 (with
#'   fill_max_frac fixed at 1.1) produces an invalid triangular
#'   distribution and rtriangle() silently returns NA — discovered via a
#'   real Issue #112 re-run where every parameter's sigma_ee came out NA,
#'   root-caused to this single out-of-envelope bound corrupting the
#'   simulation state (and therefore every downstream KPI) for the
#'   remainder of any OAT trajectory that perturbed this parameter above
#'   1.1. See README Limitation L18 follow-up note for the incident.
#'
#'   `post_op_hold_mode`'s lower bound is 380, not the Rule-B-multiplicative
#'   300 (baseline 600 x 0.5) — the same class of bug, one field over:
#'   `env_data.json`'s `r2eheavy.post_op_hold.min`/`.max` (360/1440) are
#'   fixed (unscreened, matching the min/max-not-screened convention), and
#'   `rtriangle(a=min, b=max, c=mode)` (R/trajectories.R) again requires
#'   a <= c <= b — 300 < 360 would have produced the identical NA-cascade
#'   failure. Found by auditing every newly-added triangular mode's bound
#'   against its own JSON min/max after the fill_mode_frac incident above,
#'   not from a second independent re-run failure.
#'
#'   Not every numeric leaf in env_data.json's `vars` tree is screened here;
#'   see the README's "Parameters Excluded from Screening" note for the
#'   full exclusion rationale (KIA/mortuary processing durations, simplex-
#'   constrained composition splits, discrete/categorical switches, and
#'   fixed establishment/capacity counts).
morris_params <- data.frame(
  name  = c(
    # ── Original eleven (Issue #3, #75, #9) ──────────────────────────────
    "surg_mode",      "long_resus_mode", "p1_p_max",
    "r1_transport",   "r2b_transport",   "long_icu_mode",
    "pri1_surg_prob", "in_theatre_rate", "ot_hours",
    "mass_casualty_rate",    "mass_casualty_max_cas",
    # ── R1/R2B/R2E durations ───────────────────────────────────────────────
    "short_resus_mode", "short_icu_mode", "r2b_hold_mode", "r2e_hold_mode",
    "post_op_hold_mode", "r1_recovery_mode", "r1_wia_treat_mode",
    # ── R1 surgical candidacy / evacuation probabilities ─────────────────
    "pri2_surg_prob", "pri3_dnbi_surg_prob", "pri3_other_surg_prob",
    "disease_surgery_pct", "pri1_evac_prob", "pri2_evac_prob",
    # ── DOW logistic curve (P1/P2 base, shape, P3 flat rate) ─────────────
    "p1_p_base", "p1_k", "p1_t_mid",
    "p2_p_base", "p2_p_max", "p2_k", "p2_t_mid", "p3_flat",
    # ── DOW treatment-efficacy multipliers ────────────────────────────────
    "r1_tccc_factor", "r2b_resus_factor", "r2b_dcs_factor",
    "r2e_resus_factor", "r2e_dcs1_factor", "r2e_dcs2_factor",
    "r2e_postop_hold_penalty",
    # ── Casualty generation rates (Issue #18 background generators) ──────
    "wia_cbt_mean", "kia_cbt_mean", "dnbi_cbt_mean",
    "wia_spt_mean", "kia_spt_mean", "dnbi_spt_mean",
    # ── Mass casualty, force regeneration, strategic AME ──────────────────
    "mass_casualty_min_cas",
    "fr_demand_interval_days", "fr_fulfillment_lag_days", "fr_fill_mode_frac",
    "ame_schedule_interval_days", "ame_failure_probability",
    # ── R2B/R2E routing thresholds ────────────────────────────────────────
    "post_surgery_prob", "r2b_hold_threshold"
  ),
  lower = c(
    90,    25,    0.0115, 15,   15,   770,   0.70,  0.05,  8,   0,    40,
    17,    36,    3600,   7800,   380,   1440,  12,
    0.55,  0.15,  0.35,  0.03,  0.70,  0.65,
    0.0005, 0.024, 72,   0.00025, 0.0095, 0.015, 108, 0.0005,
    0.68,  0.41,  0.17,  0.41,  0.10,  0.42,  1.5,
    1.06,  0.41,  1.22,  1.06,  0.41,  0.56,
    10,
    0,    4,    0.5,
    4,    0.08,
    0.55, 0.60
  ),
  upper = c(
    150,   70,    0.046,  45,   45,   2160,  0.98,  0.20,  16,  0.4,  80,
    39,    84,    14400,  18150,  1200,  5760,  28,
    0.95,  0.55,  0.75,  0.12,  0.99,  0.98,
    0.002, 0.056, 168,  0.001,  0.038,  0.035, 252, 0.002,
    0.98,  0.71,  0.47,  0.71,  0.40,  0.72,  6.0,
    2.48,  0.95,  2.86,  2.48,  0.95,  1.32,
    30,
    14,   14,   1.05,
    14,   0.30,
    0.95, 0.95
  ),
  mode  = c(
    120,   45,    0.023,  30,   30,   1440,  0.90,  0.10,  12,  0,    60,
    28,    60,    7200,   12960,  600,   2880,  20,
    0.80,  0.40,  0.60,  0.06,  0.95,  0.90,
    0.001, 0.04,  120,  0.0005, 0.019, 0.025, 180, 0.001,
    0.83,  0.56,  0.32,  0.56,  0.25,  0.57,  3.0,
    1.77,  0.68,  2.04,  1.77,  0.68,  0.94,
    20,
    0,    7,    0.85,
    7,    0.15,
    0.75, 0.80
  ),
  # "Context" = an assumption about the operational environment or the
  # casualty population itself (generation rates, DOW calibration,
  # clinical-need composition, treatment efficacy) — a planner does not
  # choose these, they describe what happens *to* the force. Inter-echelon
  # transport time (r1_transport, r2b_transport) lives here too, not under
  # Capacity: geography and terrain, not vehicle procurement, dominate how
  # long a given leg takes in a given scenario, unlike a treatment duration
  # a staffing/equipment investment can genuinely shorten.
  #
  # "Health System Design" splits further into two sub-categories with a
  # materially different practical implication for a planner deciding
  # whether to act on a highly-ranked parameter (Issue #112 second
  # follow-up — the original two-way split conflated them):
  #   "Capacity" = a treatment/holding throughput or process time (how
  #     long a procedure or stay inherently takes at current resourcing).
  #     Only changeable through investment — more staff, better equipment,
  #     training — not by a standing-order decision; r1_wia_treat_mode is
  #     a clinical process duration a planner cannot simply command to be
  #     shorter, unlike a genuine policy lever.
  #   "Policy" = a threshold, cadence, or scheduling rule the health
  #     system's own standing orders set directly (a shift roster length,
  #     a reroute threshold, a sortie interval) — a planner can change one
  #     of these by writing a new order, with no resourcing investment
  #     required to take effect.
  # Requested by the issue #112 follow-up so a planner reading the
  # screening plot can immediately tell which kind of lever a
  # highly-ranked parameter is — and how directly they can pull it —
  # before deciding whether to act on it. A few parameters sit close to
  # the line (see README note below the plot for the specific calls this
  # project made and why, including the transport-time call above); this
  # is an interpretive aid, not a claim of a clean, uncontested partition.
  #
  # post_surgery_prob moved from Policy to Context in a same-issue follow-
  # up review: it decides, for a casualty who already had R2B surgery,
  # whether they need only a short vs. full R2E ICU stay (R/trajectories.R
  # r2e_icu_recovery) — a clinical-severity fact about the casualty's
  # condition, not a threshold the health system sets. Its sibling
  # in_theatre_rate (in-theatre recovery vs. strategic evacuation) stays
  # Policy — it is a genuine disposition/triage decision, a different kind
  # of thing despite both living under the same recovery.* env_data block.
  category = c(
    "Capacity", "Capacity", "Context", "Context", "Context", "Capacity", "Context", "Policy", "Policy", "Context", "Context",
    "Capacity", "Capacity", "Capacity", "Capacity", "Capacity", "Capacity", "Capacity",
    "Context", "Context", "Context", "Context", "Context", "Context",
    "Context", "Context", "Context", "Context", "Context", "Context", "Context", "Context",
    "Context", "Context", "Context", "Context", "Context", "Context", "Context",
    "Context", "Context", "Context", "Context", "Context", "Context",
    "Context",
    "Policy", "Policy", "Policy",
    "Policy", "Context",
    "Context", "Policy"
  ),
  stringsAsFactors = FALSE
)

# ── Parameter application ─────────────────────────────────────────────────────

#' Apply a named parameter vector to a copy of env_data
#'
#' @param ed  A copy of the env_data list (not modified in place)
#' @param p   Named numeric vector — names must match morris_params$name.
#'   The ot_hours entry is excluded here; it is passed directly to
#'   run_replications() because it affects build_env() scheduling, not vars.
#' @return Modified env_data copy
#'
#' @details Issue #112 expanded this from eleven to fifty-five parameters,
#'   then a same-issue follow-up review reduced it to fifty-three by
#'   removing two polling-interval parameters from screening (see
#'   morris_params's own comment).
apply_params <- function(ed, p) {
  # ── Original eleven (Issue #3, #75, #9) ────────────────────────────────
  ed$vars$r2b$surgery$mode                  <- p[["surg_mode"]]
  ed$vars$r2eheavy$surgery$mode             <- p[["surg_mode"]]
  ed$vars$r2eheavy$long_resus$mode          <- p[["long_resus_mode"]]
  ed$vars$r2b$long_resus$mode               <- p[["long_resus_mode"]]
  ed$vars$dow$params$p1_p_max               <- p[["p1_p_max"]]
  ed$vars$r1$wia_transport$mode             <- p[["r1_transport"]]
  ed$vars$r2b$wia_transport$mode            <- p[["r2b_transport"]]
  ed$vars$r2eheavy$long_icu$mode            <- p[["long_icu_mode"]]
  ed$vars$r1$other$pri1_surgery             <- p[["pri1_surg_prob"]]
  ed$vars$r2eheavy$recovery$in_theatre_rate <- p[["in_theatre_rate"]]
  ed$vars$mass_casualty$event$rate_per_day  <- p[["mass_casualty_rate"]]
  ed$vars$mass_casualty$event$max_cas       <- p[["mass_casualty_max_cas"]]

  # ── R1/R2B/R2E durations (Issue #112) ───────────────────────────────────
  ed$vars$r2eheavy$short_resus$mode           <- p[["short_resus_mode"]]
  ed$vars$r2eheavy$short_icu$mode             <- p[["short_icu_mode"]]
  ed$vars$r2b$holding$mode                    <- p[["r2b_hold_mode"]]
  ed$vars$r2eheavy$holding$mode               <- p[["r2e_hold_mode"]]
  ed$vars$r2eheavy$post_op_hold$mode          <- p[["post_op_hold_mode"]]
  ed$vars$r1$recovery$mode                    <- p[["r1_recovery_mode"]]
  ed$vars$r1$wia_treat$mode                   <- p[["r1_wia_treat_mode"]]

  # ── R1 surgical candidacy / evacuation probabilities (Issue #112) ──────
  ed$vars$r1$other$pri2_surgery       <- p[["pri2_surg_prob"]]
  ed$vars$r1$other$pri3_dnbi_surgery  <- p[["pri3_dnbi_surg_prob"]]
  ed$vars$r1$other$pri3_other_surgery <- p[["pri3_other_surg_prob"]]
  ed$vars$r1$other$disease_surgery_pct <- p[["disease_surgery_pct"]]
  ed$vars$r1$other$pri1_evac          <- p[["pri1_evac_prob"]]
  ed$vars$r1$other$pri2_evac          <- p[["pri2_evac_prob"]]

  # ── DOW logistic curve (Issue #112) ─────────────────────────────────────
  ed$vars$dow$params$p1_p_base <- p[["p1_p_base"]]
  ed$vars$dow$params$p1_k      <- p[["p1_k"]]
  ed$vars$dow$params$p1_t_mid  <- p[["p1_t_mid"]]
  ed$vars$dow$params$p2_p_base <- p[["p2_p_base"]]
  ed$vars$dow$params$p2_p_max  <- p[["p2_p_max"]]
  ed$vars$dow$params$p2_k      <- p[["p2_k"]]
  ed$vars$dow$params$p2_t_mid  <- p[["p2_t_mid"]]
  ed$vars$dow$params$p3_flat   <- p[["p3_flat"]]

  # ── DOW treatment-efficacy multipliers (Issue #112) ─────────────────────
  ed$vars$dow$treatment_efficacy$r1_tccc_factor   <- p[["r1_tccc_factor"]]
  ed$vars$dow$treatment_efficacy$r2b_resus_factor <- p[["r2b_resus_factor"]]
  ed$vars$dow$treatment_efficacy$r2b_dcs_factor   <- p[["r2b_dcs_factor"]]
  ed$vars$dow$treatment_efficacy$r2e_resus_factor <- p[["r2e_resus_factor"]]
  ed$vars$dow$treatment_efficacy$r2e_dcs1_factor  <- p[["r2e_dcs1_factor"]]
  ed$vars$dow$treatment_efficacy$r2e_dcs2_factor  <- p[["r2e_dcs2_factor"]]
  ed$vars$dow$treatment_efficacy$r2e_postop_hold_penalty <- p[["r2e_postop_hold_penalty"]]

  # ── Casualty generation rates (Issue #112) ──────────────────────────────
  ed$vars$generators$wia_cbt$mean_daily  <- p[["wia_cbt_mean"]]
  ed$vars$generators$kia_cbt$mean_daily  <- p[["kia_cbt_mean"]]
  ed$vars$generators$dnbi_cbt$mean_daily <- p[["dnbi_cbt_mean"]]
  ed$vars$generators$wia_spt$mean_daily  <- p[["wia_spt_mean"]]
  ed$vars$generators$kia_spt$mean_daily  <- p[["kia_spt_mean"]]
  ed$vars$generators$dnbi_spt$mean_daily <- p[["dnbi_spt_mean"]]

  # ── Mass casualty, force regeneration, strategic AME (Issue #112) ──────
  ed$vars$mass_casualty$event$min_cas <- p[["mass_casualty_min_cas"]]

  ed$vars$force_regeneration$reinforcement$demand_interval_days  <- p[["fr_demand_interval_days"]]
  ed$vars$force_regeneration$reinforcement$fulfillment_lag_days  <- p[["fr_fulfillment_lag_days"]]
  ed$vars$force_regeneration$reinforcement$fill_mode_frac        <- p[["fr_fill_mode_frac"]]

  ed$vars$role4$ame$schedule_interval_days <- p[["ame_schedule_interval_days"]]
  ed$vars$role4$ame$failure_probability    <- p[["ame_failure_probability"]]

  # ── R2B/R2E routing thresholds (Issue #112) ─────────────────────────────
  ed$vars$r2eheavy$recovery$post_surgery <- p[["post_surgery_prob"]]
  ed$vars$r2b$holding$hold_threshold     <- p[["r2b_hold_threshold"]]

  ed
}

# ── KPI extraction ────────────────────────────────────────────────────────────

#' Time-weighted mean resource utilisation (fraction busy) matching a pattern
#'
#' @param mon Named list with 'resources' element as returned by run_replications()
#' @param pattern Regex pattern to match resource names
#' @return Mean utilisation (0-1), time-weighted per replication then averaged
#'   across replications and matching resources. Returns 0 if no resource matches.
#'
#' @details Complements summarise_replications() (queue-based) with a
#'   utilisation-based measure. Needed for resources such as pooled transport
#'   assets that rarely queue under current baseline demand (Issue #6) but
#'   whose busy-time is still directly affected by duration parameters —
#'   queue-based KPIs alone would show near-zero sensitivity in that case.
compute_utilisation <- function(mon, pattern) {
  util <- mon$resources %>%
    filter(grepl(pattern, resource)) %>%
    group_by(replication, resource) %>%
    arrange(time) %>%
    mutate(dt = lead(time, default = max(time)) - time) %>%
    summarise(
      rep_util = weighted.mean(server / pmax(capacity, 1), w = pmax(dt, 0), na.rm = TRUE),
      .groups  = "drop"
    )
  if (nrow(util) == 0) return(0)
  mean(util$rep_util, na.rm = TRUE)
}

#' Extract KPIs from a run_replications() monitoring list
#'
#' @param mon Named list with arrivals, attributes, resources
#' @return Named numeric vector: r2e_icu_q, r2b_ot_q, r2e_ot_q, system_ot_q,
#'   dow_count, transport_q, transport_util
#'
#' @details Uses summarise_replications() for queue KPIs. DOW count is taken
#'   directly from the attributes monitor to avoid dependency on warm-up
#'   filtering. transport_q and transport_util (Issue #6) cover the pooled
#'   PMV Ambulance and HX240M transport assets.
extract_kpis <- function(mon) {
  kpi <- summarise_replications(mon)

  safe_q <- function(pattern) {
    v <- kpi %>%
      filter(grepl(pattern, resource)) %>%
      summarise(v = mean(mean_q, na.rm = TRUE)) %>%
      pull(v)
    if (length(v) == 0 || is.na(v)) 0 else v
  }

  r2e_icu_q   <- safe_q("^b_r2eheavy_icu_")
  r2b_ot_q    <- safe_q("^b_r2b_ot_")
  r2e_ot_q    <- safe_q("^b_r2eheavy_ot_")
  system_ot_q <- r2b_ot_q + r2e_ot_q

  transport_q    <- safe_q("^t_PMVAmb_|^t_HX240M_")
  transport_util <- compute_utilisation(mon, "^t_PMVAmb_|^t_HX240M_")

  dow_count <- sum(
    mon$attributes$key == "dow" & mon$attributes$value == 1,
    na.rm = TRUE
  )

  c(
    r2e_icu_q      = r2e_icu_q,
    r2b_ot_q       = r2b_ot_q,
    r2e_ot_q       = r2e_ot_q,
    system_ot_q    = system_ot_q,
    dow_count      = as.numeric(dow_count),
    transport_q    = transport_q,
    transport_util = transport_util
  )
}

# ── Single-point evaluation ───────────────────────────────────────────────────

#' Run the simulation with a given parameter row and return KPIs
#'
#' @param params_row Numeric vector (length = nrow(morris_params)), in column order
#' @param n_rep      Replications per evaluation (5 recommended for Morris)
#' @param n_days     Simulation duration in days
#' @param max_cores  Optional integer cap on mclapply's mc.cores, passed
#'   through to run_replications() (see its own @param for why this
#'   matters for Shiny-triggered, locally-run screens). NULL preserves
#'   prior behaviour.
#' @return Named numeric vector: r2e_icu_q, r2b_ot_q, r2e_ot_q, system_ot_q,
#'   dow_count, transport_q, transport_util
#'
#' @details Modifies the global env_data via apply_params() then restores it.
#'   The ot_hours parameter is extracted separately and passed to run_replications()
#'   because it controls build_env() scheduling, not the vars structure.
eval_params <- function(params_row, n_rep, n_days, max_cores = NULL) {
  p    <- setNames(as.numeric(params_row), morris_params$name)
  ot_h <- p[["ot_hours"]]

  env_data <<- apply_params(env_data_base, p)
  mon      <- run_replications(n_rep, n_days, ot_hours = ot_h, max_cores = max_cores)
  extract_kpis(mon)
}

# ── Morris screening ──────────────────────────────────────────────────────────

#' Run Morris Elementary Effects screening
#'
#' @param n_days     Simulation duration per replication (default 30)
#' @param n_rep      Replications per Morris evaluation point (default 5)
#' @param r          Number of Morris trajectories (default 20)
#' @param levels     Number of grid levels (default 4)
#' @param output_dir Directory for CSV and PNG outputs (default "outputs")
#' @param progress_dir Optional directory path; when supplied, an empty
#'   marker file ("point_<i>.done") is written to it as each design point
#'   finishes evaluating, letting a caller on another process (e.g. the
#'   Shiny app's main session) observe real "point M of N" progress. NULL
#'   (default) disables this and preserves prior behaviour for existing
#'   callers (scripts/run_sensitivity.R).
#' @param max_cores Optional integer cap on mclapply's mc.cores at each
#'   design point, passed through to run_replications() via eval_params()
#'   (see run_replications()'s own @param for why this matters for
#'   Shiny-triggered, locally-run screens). NULL preserves prior behaviour.
#' @return Named list: morris_objs (per-KPI sensitivity objects), Y (KPI matrix),
#'   X (design matrix), ranking (data frame sorted descending by mu_star)
#'
#' @details Runs r*(p+1) model evaluations where p = nrow(morris_params).
#'   Saves per-KPI Morris plots (mu* vs sigma) to images/ and a parameter
#'   ranking CSV to output_dir. The global env_data is restored to env_data_base
#'   on exit regardless of errors.
run_morris <- function(n_days = 30, n_rep = 5, r = 20, levels = 4,
                       output_dir = "outputs", progress_dir = NULL, max_cores = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create("images",   recursive = TRUE, showWarnings = FALSE)

  n_eval <- r * (nrow(morris_params) + 1L)
  message(sprintf(
    "Morris screening: r=%d, p=%d, levels=%d → %d evaluations × %d reps each",
    r, nrow(morris_params), levels, n_eval, n_rep
  ))

  env_data_base <<- env_data

  sa <- morris(
    model   = NULL,
    factors = morris_params$name,
    r       = r,
    design  = list(type = "oat", levels = levels, grid.jump = 2),
    binf    = morris_params$lower,
    bsup    = morris_params$upper,
    scale   = TRUE
  )

  message(sprintf("Evaluating %d design points...", nrow(sa$X)))

  Y <- t(vapply(seq_len(nrow(sa$X)), function(i) {
    message(sprintf("  Point %d / %d", i, nrow(sa$X)))
    kpis <- tryCatch(
      eval_params(sa$X[i, ], n_rep, n_days, max_cores = max_cores),
      error = function(e) {
        warning(sprintf("Eval %d failed: %s", i, conditionMessage(e)))
        c(r2e_icu_q = NA_real_, r2b_ot_q = NA_real_, r2e_ot_q = NA_real_,
          system_ot_q = NA_real_, dow_count = NA_real_,
          transport_q = NA_real_, transport_util = NA_real_)
      }
    )
    if (!is.null(progress_dir)) {
      file.create(file.path(progress_dir, sprintf("point_%d.done", i)))
    }
    # A full production screen runs this loop hundreds of times in one long-
    # lived process (r=20 x (p+1) = 240 design points, each building and
    # discarding a full monitoring dataset via eval_params()/run_replications()).
    # R's own garbage collector is lazy about returning memory to the OS
    # between iterations of a tight loop like this one; left unforced, that
    # slow per-iteration accumulation was observed (Issue #15 follow-up) to
    # grow a local dev container's memory usage steadily over the course of
    # a multi-hour run until it started swapping/thrashing rather than
    # failing cleanly. Forcing a full collection after every point trades a
    # small amount of wall-clock time for keeping steady-state memory flat
    # across however many points the screen runs.
    gc(full = TRUE)
    kpis
  }, numeric(7)))

  env_data <<- env_data_base

  # Persisted so a plotting-only change (e.g. Issue #112's follow-up fixing
  # illegible overlapping labels at p=55) can re-render images/*.png from
  # the same design + responses without re-running the full simulation
  # sweep — a 94-minute cost at r=5/p=55 in this project's development
  # environment (see the README's reduced-r note) that a labelling tweak
  # alone should never require paying twice.
  saveRDS(list(X = sa$X, Y = Y, binf = sa$binf, bsup = sa$bsup),
          file.path(output_dir, "morris_design_and_responses.rds"))

  kpi_labels <- list(
    r2b_ot_q       = "Mean R2B OT Queue",
    r2e_ot_q       = "Mean R2E OT Queue",
    system_ot_q    = "System OT Queue (R2B + R2E)",
    r2e_icu_q      = "Mean R2E ICU Queue",
    dow_count      = "Total DOW Count",
    transport_q    = "Mean Transport Queue (PMV Amb + HX240M)",
    transport_util = "Mean Transport Utilisation (PMV Amb + HX240M)"
  )

  morris_objs <- lapply(names(kpi_labels), function(kpi) {
    obj <- sa
    tell(obj, Y[, kpi])

    plot_title <- sprintf("Morris Screening — %s", kpi_labels[[kpi]])
    p <- tryCatch(
      plot_morris_scatter(obj, plot_title),
      error = function(e) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste0(plot_title, "\n(insufficient variation to plot)")) +
          theme_void()
      }
    )
    # Sized well above the original 900x650/res=120 base-R default — a
    # dense, ggrepel-labelled 55-parameter scatter needs more canvas area
    # per label than the nine/ten/eleven-parameter screens this project's
    # image dimensions were originally tuned for.
    ggsave(file.path("images", sprintf("morris_%s.png", kpi)), plot = p,
           width = 12, height = 9, dpi = 130)

    obj
  })
  names(morris_objs) <- names(kpi_labels)

  message("Morris plots saved to images/")

  # Rank parameters by mu* on system OT queue (primary bottleneck KPI)
  primary  <- morris_objs$system_ot_q
  mu_star  <- setNames(
    as.numeric(apply(abs(primary$ee), 2, mean, na.rm = TRUE)),
    morris_params$name
  )
  sigma_ee <- setNames(
    as.numeric(apply(primary$ee, 2, sd, na.rm = TRUE)),
    morris_params$name
  )

  ranking <- data.frame(
    parameter = names(mu_star),
    mu_star   = mu_star,
    sigma_ee  = sigma_ee,
    row.names = NULL
  ) %>% arrange(desc(mu_star))

  write.csv(ranking, file.path(output_dir, "morris_ranking.csv"), row.names = FALSE)
  message("Parameter ranking written to outputs/morris_ranking.csv")
  message("\nTop parameters by mu* (system OT queue):")
  print(ranking, digits = 4)

  list(morris_objs = morris_objs, Y = Y, X = sa$X, ranking = ranking)
}

# ── Sobol variance decomposition ──────────────────────────────────────────────

#' Run Sobol variance decomposition on a selected parameter subset
#'
#' @param top_params  Character vector of parameter names from morris_params$name
#' @param n_days      Simulation duration per replication (default 30)
#' @param n_rep       Replications per Sobol evaluation point (default 5)
#' @param n_sobol     Sobol sample size N (default 200; total evals = N*(p+2))
#' @param output_dir  Directory for CSV outputs (default "outputs")
#' @param progress_dir Optional directory path; when supplied, an empty
#'   marker file ("point_<i>.done") is written to it as each design point
#'   finishes evaluating (see run_morris()'s equivalent parameter). NULL
#'   (default) disables this and preserves prior behaviour.
#' @param max_cores Optional integer cap on mclapply's mc.cores at each
#'   design point (see run_morris()'s equivalent parameter). NULL preserves
#'   prior behaviour.
#' @return Named list of sobol2007 objects: r2b_ot_q, r2e_ot_q, system_ot_q,
#'   transport_q, transport_util
#'
#' @details Applies sobol2007 (Saltelli et al. estimator) using a single design
#'   pass shared across all five KPIs, giving N*(p+2) total evaluations.
#'   Bootstrap CI uses nboot=100. Results written to output_dir as per-KPI CSVs.
run_sobol <- function(top_params, n_days = 30, n_rep = 5,
                      n_sobol = 200, output_dir = "outputs", progress_dir = NULL,
                      max_cores = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  p_idx <- which(morris_params$name %in% top_params)
  if (length(p_idx) == 0) stop("None of top_params found in morris_params$name")

  p_def   <- morris_params[p_idx, ]
  n_total <- n_sobol * (nrow(p_def) + 2L)
  message(sprintf(
    "Sobol: n=%d, p=%d → %d evaluations × %d reps (r2b_ot_q, r2e_ot_q, system_ot_q, transport_q, transport_util)",
    n_sobol, nrow(p_def), n_total, n_rep
  ))

  env_data_base <<- env_data

  X1 <- as.data.frame(mapply(function(lo, hi) runif(n_sobol, lo, hi),
                              p_def$lower, p_def$upper, SIMPLIFY = FALSE))
  X2 <- as.data.frame(mapply(function(lo, hi) runif(n_sobol, lo, hi),
                              p_def$lower, p_def$upper, SIMPLIFY = FALSE))
  names(X1) <- names(X2) <- p_def$name

  sb_r2b   <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)
  sb_r2e   <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)
  sb_sys   <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)
  sb_tq    <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)
  sb_tutil <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)

  full_params <- setNames(morris_params$mode, morris_params$name)

  Y_all <- t(vapply(seq_len(nrow(sb_r2b$X)), function(i) {
    message(sprintf("  Sobol point %d / %d", i, nrow(sb_r2b$X)))
    row <- full_params
    row[p_def$name] <- as.numeric(sb_r2b$X[i, ])
    res <- tryCatch(
      {
        kpis <- eval_params(row, n_rep, n_days, max_cores = max_cores)
        c(r2b_ot_q       = kpis[["r2b_ot_q"]],
          r2e_ot_q       = kpis[["r2e_ot_q"]],
          system_ot_q    = kpis[["system_ot_q"]],
          transport_q    = kpis[["transport_q"]],
          transport_util = kpis[["transport_util"]])
      },
      error = function(e) {
        warning(sprintf("Sobol eval %d failed: %s", i, conditionMessage(e)))
        c(r2b_ot_q = NA_real_, r2e_ot_q = NA_real_, system_ot_q = NA_real_,
          transport_q = NA_real_, transport_util = NA_real_)
      }
    )
    if (!is.null(progress_dir)) {
      file.create(file.path(progress_dir, sprintf("point_%d.done", i)))
    }
    # See run_morris()'s identical gc() call for why: this loop runs
    # n_sobol * (p + 2) iterations (200 * 7 = 1400 at the defaults) in one
    # long-lived process — forcing a collection after every point keeps
    # steady-state memory flat rather than creeping up across the run
    # (Issue #15 follow-up).
    gc(full = TRUE)
    res
  }, numeric(5)))

  env_data <<- env_data_base

  # tell() invokes boot::boot.ci() internally, which errors on a response
  # with (near-)zero variance across the design (e.g. transport_q when none
  # of top_params affect transport occupancy — see Issue #6 PR discussion).
  # Wrapped per-KPI so one degenerate response doesn't discard the rest.
  tell_safe <- function(sb, y, kpi_name) {
    tryCatch({
      tell(sb, y)
      TRUE
    }, error = function(e) {
      warning(sprintf(
        "Sobol tell() failed for %s (likely a near-zero-variance response — %s): %s",
        kpi_name, "top_params may not include a parameter that moves this KPI",
        conditionMessage(e)
      ))
      FALSE
    })
  }

  sobol_ok <- c(
    r2b_ot_q       = tell_safe(sb_r2b,   Y_all[, "r2b_ot_q"],       "r2b_ot_q"),
    r2e_ot_q       = tell_safe(sb_r2e,   Y_all[, "r2e_ot_q"],       "r2e_ot_q"),
    system_ot_q    = tell_safe(sb_sys,   Y_all[, "system_ot_q"],    "system_ot_q"),
    transport_q    = tell_safe(sb_tq,    Y_all[, "transport_q"],    "transport_q"),
    transport_util = tell_safe(sb_tutil, Y_all[, "transport_util"], "transport_util")
  )

  # Even when tell() does not throw, boot.ci() can silently fail for an
  # individual parameter within an otherwise-successful call (e.g. one
  # parameter's bootstrap distribution is degenerate while others are not),
  # leaving sb$S / sb$T columns shorter than p_def$name. Guard against that
  # here rather than relying on tell_safe() alone.
  save_sobol <- function(sb, kpi_name) {
    p <- nrow(p_def)
    lens <- c(length(sb$S$original), length(sb$S$`min. c.i.`), length(sb$S$`max. c.i.`),
              length(sb$T$original), length(sb$T$`min. c.i.`), length(sb$T$`max. c.i.`))
    if (any(lens != p)) {
      warning(sprintf(
        "Skipping Sobol output for %s: incomplete indices (expected %d parameters, got lengths %s) — likely a degenerate bootstrap for at least one parameter.",
        kpi_name, p, paste(lens, collapse = ",")
      ))
      return(invisible(NULL))
    }
    results <- data.frame(
      parameter = p_def$name,
      S1        = sb$S$original,
      S1_lower  = sb$S$`min. c.i.`,
      S1_upper  = sb$S$`max. c.i.`,
      ST        = sb$T$original,
      ST_lower  = sb$T$`min. c.i.`,
      ST_upper  = sb$T$`max. c.i.`
    )
    write.csv(results, file.path(output_dir, sprintf("sobol_%s.csv", kpi_name)),
              row.names = FALSE)
    message(sprintf("\nSobol indices for %s:", kpi_name))
    print(results, digits = 4)
    results
  }

  sb_objs <- list(r2b_ot_q = sb_r2b, r2e_ot_q = sb_r2e, system_ot_q = sb_sys,
                   transport_q = sb_tq, transport_util = sb_tutil)
  saved <- list()
  for (kpi_name in names(sb_objs)) {
    if (sobol_ok[[kpi_name]]) {
      res <- save_sobol(sb_objs[[kpi_name]], kpi_name)
      if (!is.null(res)) saved[[kpi_name]] <- sb_objs[[kpi_name]]
    }
  }

  message("\nSobol complete.")
  saved
}
