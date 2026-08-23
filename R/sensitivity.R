##############################################
## R/sensitivity.R                          ##
## Morris EE screening + Sobol follow-up   ##
##############################################

library(sensitivity)
library(dplyr)
library(tidyr)
library(stringr)
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

# ── Simplex-constrained compositions ──────────────────────────────────────────

#' Isometric log-ratio (ILR) transform of a three-part composition
#'
#' @param x Numeric vector of length 3, strictly positive. It need not sum to
#'   one; it is closed to the simplex first.
#' @return Numeric vector of length 2 — the two balance coordinates.
#'
#' @details A composition of k parts constrained to sum to one carries only
#'   k - 1 degrees of freedom, which is why a one-at-a-time design cannot
#'   perturb its members directly: moving one part while holding the others
#'   fixed leaves the simplex. The ILR transform (Pawlowsky-Glahn, Egozcue &
#'   Tolosana-Delgado, 2007) maps the composition onto k - 1 unconstrained
#'   real coordinates that can be varied independently and mapped back, so
#'   the sum-to-one constraint holds by construction rather than by
#'   renormalising after the fact — the renormalisation being the design
#'   decision that kept these parameters out of the screen until now.
#'
#'   The basis is the sequential binary partition separating part 1 from
#'   parts 2 and 3, then part 2 from part 3, so each coordinate reads as a
#'   balance: the first contrasts the leading part against the geometric mean
#'   of the other two, the second contrasts those two with each other. Each
#'   group in `MORRIS_COMPOSITIONS` orders its parts so the first coordinate
#'   is the contrast of planning interest.
#'
#'   Implemented here rather than taken from the `compositions` package: the
#'   three-part case is two lines each way, against a dependency carrying a
#'   large transitive tree into `renv.lock` for no other use in the project.
ilr3 <- function(x) {
  x <- x / sum(x)
  c(sqrt(2 / 3) * log(x[1] / sqrt(x[2] * x[3])),
    sqrt(1 / 2) * log(x[2] / x[3]))
}

#' Inverse ILR transform — balance coordinates back to a three-part composition
#'
#' @param z Numeric vector of length 2, the two balance coordinates.
#' @return Numeric vector of length 3, strictly positive and summing to one.
#'
#' @details The two defining relations, `u1 - (u2 + u3) / 2 = z1 / sqrt(2/3)`
#'   and `u2 - u3 = z2 / sqrt(1/2)` on the part logarithms, are satisfied by
#'   `u = (a1, a2/2, -a2/2)` up to the additive constant that closing to the
#'   simplex removes. Every real coordinate pair maps to a valid interior
#'   composition, which is what makes a corner of the screened coordinate box
#'   safe: no design point can produce a negative or above-one part.
ilr3_inv <- function(z) {
  a1 <- z[1] * sqrt(3 / 2)
  a2 <- z[2] * sqrt(2)
  u  <- c(a1, a2 / 2, -a2 / 2)
  # Shifted before exponentiating: a coordinate far from the baseline could
  # otherwise overflow exp() and return NaN in place of a composition.
  e <- exp(u - max(u))
  e / sum(e)
}

#' The three simplex-constrained composition groups, as screened coordinates
#'
#' @format Named list, one element per group, each carrying:
#'   `coords` (the two `morris_params` names), `parts` (plain-English part
#'   names in basis order), `baseline` (the `env_data.json` composition, in
#'   the same order), `lead_range` (the plausible range of the leading part's
#'   share, from which the first coordinate's bounds are derived), and
#'   `apply` (writes a composition back into an env_data copy).
#'
#' @details Nine parameters across three groups were previously excluded from
#'   screening for the reason `ilr3()` describes, leaving the two
#'   highest-ranked parameters in the screen — both conditional on a casualty
#'   being Priority 1 — with no companion evidence on the share of casualties
#'   that are Priority 1 at all. The nine become six coordinates here, added
#'   to the same Morris design at the same cost per trajectory.
#'
#'   Part order is chosen per group so the first balance is the contrast a
#'   planner cares about: the Priority 1 share for the two triage splits, and
#'   the disease share for the DNBI composition, disease being the part whose
#'   own onward surgical candidacy (`disease_surgery_pct`) is already
#'   screened while the share it applies to was not.
#'
#'   All three are Context. Each is a fact about the casualty population the
#'   force sustains, not a decision the health system makes: a planner
#'   chooses neither the severity mix of the wounded nor the split of
#'   non-battle admissions between psychological, medical, and injury causes.
MORRIS_COMPOSITIONS <- list(
  triage = list(
    coords     = c("triage_p1_balance", "triage_p2_p3_balance"),
    parts      = c("Priority 1", "Priority 2", "Priority 3"),
    baseline   = c(0.65, 0.20, 0.15),
    lead_range = c(0.45, 0.80),
    apply = function(ed, x) {
      ed$vars$r1$priority$one   <- x[1]
      ed$vars$r1$priority$two   <- x[2]
      ed$vars$r1$priority$three <- x[3]
      ed
    }
  ),
  dnbi = list(
    coords     = c("dnbi_disease_balance", "dnbi_bf_nbi_balance"),
    parts      = c("Disease", "Battle fatigue", "Non-battle injury"),
    baseline   = c(0.58, 0.25, 0.17),
    lead_range = c(0.40, 0.75),
    apply = function(ed, x) {
      ed$vars$r1$other$disease_pct        <- x[1]
      ed$vars$r1$other$battle_fatigue_pct <- x[2]
      ed$vars$r1$other$nbi_pct            <- x[3]
      ed
    }
  ),
  mass_casualty = list(
    coords     = c("mc_p1_balance", "mc_p2_p3_balance"),
    parts      = c("Priority 1", "Priority 2", "Priority 3"),
    baseline   = c(0.70, 0.20, 0.10),
    lead_range = c(0.55, 0.85),
    apply = function(ed, x) {
      ed$vars$mass_casualty$priority$one   <- x[1]
      ed$vars$mass_casualty$priority$two   <- x[2]
      ed$vars$mass_casualty$priority$three <- x[3]
      ed
    }
  )
)

#' Screening bounds for one composition group's two balance coordinates
#'
#' @param g One element of `MORRIS_COMPOSITIONS`
#' @return Data frame with the `morris_params` columns for the group's two
#'   coordinates
#'
#' @details A bound stated directly in coordinate space would have no
#'   intuitive meaning, so both are derived by transforming a compositional
#'   range. The first coordinate's bounds transform the group's `lead_range`,
#'   holding the ratio of the two trailing parts at its baseline, which is
#'   exactly the sub-composition the first balance leaves untouched. The
#'   second coordinate's bounds apply Rule B multiplicatively to that ratio
#'   (x0.5 to x2.0), which in coordinate space is the symmetric interval
#'   `baseline +/- sqrt(1/2) ln 2`, since a balance is a scaled log ratio.
#'
#'   A balance contrasts the leading part against the *geometric* mean of the
#'   other two, so the realised leading share at a corner where both
#'   coordinates sit at an extreme differs slightly from the nominal
#'   `lead_range` endpoint (for the triage split, 0.42 to 0.80 across the
#'   whole coordinate box against a nominal 0.45 to 0.80). The realised
#'   composition is always valid; only the endpoint is approximate.
composition_coord_bounds <- function(g) {
  sub  <- g$baseline[2:3] / sum(g$baseline[2:3])
  mode <- ilr3(g$baseline)
  lead <- vapply(g$lead_range, function(s) ilr3(c(s, (1 - s) * sub))[1], numeric(1))
  d2   <- sqrt(1 / 2) * log(2)

  data.frame(
    name     = g$coords,
    lower    = c(lead[1], mode[2] - d2),
    upper    = c(lead[2], mode[2] + d2),
    mode     = mode,
    category = c("Context", "Context"),
    stringsAsFactors = FALSE
  )
}

# ── Parameter definitions ─────────────────────────────────────────────────────

#' Parameter bounds for Morris Elementary Effects screening
#'
#' @format Data frame with columns: name, lower, upper, mode (current baseline value)
#'
#' @details The `mode` column carries the value the parameter holds in
#'   `env_data.json`, and carries it for two purposes. `run_sobol()` reads it
#'   as the held-fixed background for every parameter not selected into the
#'   decomposition, so a `mode` that has drifted from the shipped
#'   configuration silently measures variance contributions against a
#'   configuration nobody chose; the Shiny Sensitivity Calibration tab
#'   displays the same column to a planner under the heading `Baseline`.
#'   Neither use is exercised by an ordinary run, which is why two values
#'   were able to drift out of their own bounds and a third out of agreement
#'   with `env_data.json` before Issue #186 found them. Two guards now hold
#'   the column: the bounds assertion below, and
#'   `scripts/check_morris_baseline.R`, which asserts agreement with
#'   `env_data.json` itself.
#'
#' @details Sixty-four parameters (Issue #112 full-coverage audit, expanded
#'   from the original eleven, then reduced by a same-issue follow-up review
#'   — see the exclusion note below — and grown since by the parameters
#'   later issues introduced) span treatment durations (surgery,
#'   resuscitation, ICU, holding), DOW probability (both the full P1/P2
#'   logistic curve and treatment-efficacy multipliers), evacuation transport
#'   times, surgical/evacuation decision probabilities, the damage control
#'   versus single-stage surgical pathway split (Issue #173),
#'   in-theatre recovery rate, OT shift availability, mass
#'   casualty event rate/size (Issue #9), force regeneration reinforcement
#'   timing (Issue #18), strategic AME sortie cadence (Issue #23), and
#'   casualty generation rates. The last six rows are the balance coordinates
#'   of the three simplex-constrained composition groups, appended below from
#'   `MORRIS_COMPOSITIONS` rather than written out here because their bounds
#'   are derived by transforming a compositional range rather than stated
#'   directly (see `composition_coord_bounds()`).
#'   Bounds are otherwise set to cover clinically plausible
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
#'   `surg_mode` spans 57 to 133 minutes, Rule A around the shipped 95-minute
#'   mode. The bounds were previously 90 to 150, derived against a 120-minute
#'   mode that Issue #76 superseded, which left the baseline 0.083 of the way
#'   along its own range: every design point of the screen then ran a longer
#'   operation than the model ships, biasing every theatre-mediated
#'   parameter's rank rather than this row's alone (Issue #195). The lower
#'   endpoint was read against the source as well as derived from the rule:
#'   57 minutes lies inside
#'   the 41-to-210-minute operative-time range the mode's own source reports,
#'   above its fastest observed case, and both endpoints sit inside the fixed
#'   `r2b.surgery`/`r2eheavy.surgery` min and max, so no design point can
#'   produce the invalid triangular draw described in the two notes below.
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
#'   Issue #207 added validate_fill_distribution() (R/trajectories.R),
#'   which now raises this ordering violation as an error rather than
#'   leaving it to surface as an NA cascade; the bound stays where it is,
#'   an error being a worse outcome for a screening run than a bound that
#'   never trips.
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
#'   `ot_hours`'s screening bounds bracket the shift length configured at
#'   `vars.surgical_roster.shift.ot_hours` in env_data.json, which is where
#'   apply_params() writes the screened value and where build_env() reads it
#'   from — the same route every other parameter in this table takes.
#'
#'   Not every numeric leaf in env_data.json's `vars` tree is screened here;
#'   see the README's "Parameters Excluded from Screening" note for the
#'   full exclusion rationale (KIA/mortuary processing durations,
#'   discrete/categorical switches, and fixed establishment/capacity counts).
morris_params <- data.frame(
  name  = c(
    # ── Original eleven (Issue #3, #75, #9) ──────────────────────────────
    "surg_mode",      "long_resus_mode", "p1_p_max",
    "r1_transport",   "r2b_transport",   "stabilisation_icu_mode",
    "pri1_surg_prob", "evacuation_policy_days", "ot_hours",
    "mass_casualty_rate",    "mass_casualty_max_cas",
    # ── R1/R2B/R2E durations ───────────────────────────────────────────────
    "short_resus_mode", "r2b_hold_mode", "r2e_hold_mode",
    "post_op_hold_mode", "r1_recovery_mode", "r1_wia_treat_mode",
    "post_definitive_icu_mode",
    # ── R1 surgical candidacy / evacuation probabilities ─────────────────
    "pri2_surg_prob", "pri3_dnbi_surg_prob", "pri3_other_surg_prob",
    "disease_surgery_pct", "pri1_evac_prob", "pri2_evac_prob",
    # ── DOW logistic curve (P1/P2 base, shape, P3 flat rate) ─────────────
    "p1_p_base", "p1_k", "p1_t_mid",
    "p2_p_base", "p2_p_max", "p2_k", "p2_t_mid", "p3_flat",
    # ── DOW treatment-efficacy multipliers ────────────────────────────────
    "r1_tccc_factor", "r2b_resus_factor", "r2b_dcs_factor",
    "r2e_resus_factor", "r2e_dcs1_factor", "r2e_dcs2_factor",
    "r2e_postop_hold_penalty", "r2b_icu_penalty",
    # ── Casualty generation rates (Issue #18 background generators) ──────
    "wia_cbt_mean", "kia_cbt_mean", "dnbi_cbt_mean",
    "wia_spt_mean", "kia_spt_mean", "dnbi_spt_mean",
    # ── Mass casualty, force regeneration, strategic AME ──────────────────
    "mass_casualty_min_cas",
    "fr_demand_interval_days", "fr_fulfillment_lag_days", "fr_fill_mode_frac",
    "ame_schedule_interval_days", "ame_failure_probability",
    # ── R2B/R2E routing thresholds ────────────────────────────────────────
    "r2b_icu_share", "r2b_forward_hold_max", "r2b_hold_threshold",
    "r2b_pre_open_window",
    # ── Surgical pathway split (Issue #173) ───────────────────────────────
    "pri1_dcs_rate", "pri2_dcs_rate", "pri3_dcs_rate"
  ),
  lower = c(
    57,    25,    0.0100, 15,   15,   770,   0.70,  15,    8,   0,    40,
    17,    3600,   23400,  380,   1440,  12,   720,
    0.55,  0.15,  0.35,  0.03,  0.70,  0.65,
    0.0005, 0.024, 72,   0.00025, 0.0080, 0.015, 108, 0.0005,
    0.68,  0.41,  0.17,  0.41,  0.10,  0.42,  1.5,  1.09,
    1.06,  0.41,  1.22,  1.06,  0.41,  0.56,
    10,
    0,    4,    0.5,
    4,    0.08,
    0,    0,     0.60,
    0,
    0.30, 0.08, 0.00
  ),
  upper = c(
    133,   70,    0.040,  45,   45,   2160,  0.98,  60,    16,  0.4,  80,
    39,    14400,  54450,  1200,  5760,  28,   2880,
    0.95,  0.55,  0.75,  0.12,  0.99,  0.98,
    0.002, 0.056, 168,  0.001,  0.032,  0.035, 252, 0.002,
    0.98,  0.71,  0.47,  0.71,  0.40,  0.72,  6.0,  1.59,
    2.48,  0.95,  2.86,  2.48,  0.95,  1.32,
    30,
    14,   14,   1.05,
    14,   0.30,
    1,    2880,  0.95,
    360,
    0.80, 0.40, 0.20
  ),
  mode  = c(
    95,    45,    0.020,  30,   30,   1440,  0.90,  30,    12,  0,    60,
    28,    7200,   38880,  600,   2880,  20,   1440,
    0.80,  0.40,  0.60,  0.06,  0.95,  0.90,
    0.001, 0.04,  120,  0.0005, 0.016, 0.025, 180, 0.001,
    0.83,  0.56,  0.32,  0.56,  0.25,  0.57,  3.0,  1.31,
    1.77,  0.68,  2.04,  1.77,  0.68,  0.94,
    20,
    0,    7,    0.85,
    7,    0.15,
    0,    1440,  0.80,
    60,
    0.55, 0.20, 0.05
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
  # r2b_icu_share is Policy, and is the clearest case of the category in the
  # screen: holding a post-operative casualty forward at R2B against
  # evacuating them for rearward intensive care is a disposition a commander
  # decides by order, needing no resourcing change to take effect. The
  # penalty that prices it, r2b_icu_penalty, is Context — the mortality cost
  # of an ICU section without an intensivist is a fact about the
  # establishment, not something the same order can choose. The two
  # replaced post_surgery_prob and short_icu_mode, which are no longer
  # screened because the parameters themselves no longer exist: the R2E ICU
  # stay now follows from the requirement and the share (see
  # draw_post_op_icu_total(), R/trajectories.R) rather than from a
  # short-versus-full draw. Both were in the published ranking below, which
  # therefore predates this change.
  category = c(
    "Capacity", "Capacity", "Context", "Context", "Context", "Capacity", "Context", "Policy", "Policy", "Context", "Context",
    "Capacity", "Capacity", "Capacity", "Capacity", "Capacity", "Capacity",
    "Capacity",
    "Context", "Context", "Context", "Context", "Context", "Context",
    "Context", "Context", "Context", "Context", "Context", "Context", "Context", "Context",
    "Context", "Context", "Context", "Context", "Context", "Context", "Context", "Context",
    "Context", "Context", "Context", "Context", "Context", "Context",
    "Context",
    "Policy", "Policy", "Policy",
    "Policy", "Context",
    "Policy", "Policy", "Policy",
    "Policy",
    "Context", "Context", "Context"
  ),
  stringsAsFactors = FALSE
)

# The six balance coordinates are appended rather than written into the table
# above so that their bounds stay derived from the compositional ranges in
# MORRIS_COMPOSITIONS, where a reader can see what range was assumed, instead
# of appearing as six unexplained real numbers among the literals.
morris_params <- rbind(
  morris_params,
  do.call(rbind, lapply(MORRIS_COMPOSITIONS, composition_coord_bounds))
)
rownames(morris_params) <- NULL

# A row whose baseline sits outside its own screening range describes a
# parameter no design point can reach, and every Sobol run that does not
# select that parameter holds it there. The invariant is cheap and is checked
# at source time, where the offending row is named, rather than at design-point
# evaluation, where it is invisible.
local({
  bad <- which(!(morris_params$mode >= morris_params$lower &
                 morris_params$mode <= morris_params$upper))
  if (length(bad) > 0) {
    stop(sprintf(
      "morris_params: %d row(s) carry a mode outside their own bounds: %s",
      length(bad),
      paste(sprintf("%s (mode %g, bounds %g to %g)",
                    morris_params$name[bad], morris_params$mode[bad],
                    morris_params$lower[bad], morris_params$upper[bad]),
            collapse = "; ")
    ), call. = FALSE)
  }
})

#' Screened parameters whose `mode` cannot be compared against a single
#' `env_data.json` value, with the reason for each
#'
#' @format Named character vector — names are `morris_params$name`, values the
#'   reason the comparison does not apply.
#'
#' @details Read by `scripts/check_morris_baseline.R`. Held here, and stated
#'   as an explicit list rather than left to the check's own omissions, so that
#'   a parameter added to the screen without a corresponding entry fails the
#'   check instead of quietly escaping it.
MORRIS_MODE_CHECK_EXCLUSIONS <- local({
  coords <- unlist(lapply(MORRIS_COMPOSITIONS, `[[`, "coords"), use.names = FALSE)
  setNames(
    rep(paste("balance coordinate - writes a whole three-part composition rather",
              "than a scalar; agreement with env_data.json is asserted by",
              "scripts/check_composition_ilr.R"), length(coords)),
    coords
  )
})

# ── Parameter application ─────────────────────────────────────────────────────

#' Apply a named parameter vector to a copy of env_data
#'
#' @param ed  A copy of the env_data list (not modified in place)
#' @param p   Named numeric vector — names must match morris_params$name.
#' @return Modified env_data copy
#'
#' @details Issue #112 expanded this from eleven to fifty-five parameters,
#'   then a same-issue follow-up review reduced it to fifty-three by
#'   removing two polling-interval parameters from screening (see
#'   morris_params's own comment). Later issues have grown it to sixty-four,
#'   the last six being balance coordinates rather than direct writes: each
#'   pair is back-transformed to a whole composition at the end of this
#'   function.
apply_params <- function(ed, p) {
  # ── Original eleven (Issue #3, #75, #9) ────────────────────────────────
  ed$vars$r2b$surgery$mode                  <- p[["surg_mode"]]
  ed$vars$r2eheavy$surgery$mode             <- p[["surg_mode"]]
  ed$vars$r2eheavy$long_resus$mode          <- p[["long_resus_mode"]]
  ed$vars$r2b$long_resus$mode               <- p[["long_resus_mode"]]
  ed$vars$dow$params$p1_p_max               <- p[["p1_p_max"]]
  ed$vars$r1$wia_transport$mode             <- p[["r1_transport"]]
  ed$vars$r2b$wia_transport$mode            <- p[["r2b_transport"]]
  ed$vars$r2eheavy$stabilisation_icu$mode   <- p[["stabilisation_icu_mode"]]
  ed$vars$r1$other$pri1_surgery             <- p[["pri1_surg_prob"]]
  ed$vars$surgical_roster$shift$ot_hours    <- p[["ot_hours"]]
  ed$vars$r2eheavy$recovery$evacuation_policy_days <- p[["evacuation_policy_days"]]
  ed$vars$mass_casualty$event$rate_per_day  <- p[["mass_casualty_rate"]]
  ed$vars$mass_casualty$event$max_cas       <- p[["mass_casualty_max_cas"]]

  # ── R1/R2B/R2E durations (Issue #112) ───────────────────────────────────
  ed$vars$r2eheavy$short_resus$mode           <- p[["short_resus_mode"]]
  ed$vars$r2b$holding$mode                    <- p[["r2b_hold_mode"]]
  ed$vars$r2eheavy$holding$mode               <- p[["r2e_hold_mode"]]
  ed$vars$r2eheavy$post_op_hold$mode          <- p[["post_op_hold_mode"]]
  ed$vars$r1$recovery$mode                    <- p[["r1_recovery_mode"]]
  ed$vars$r1$wia_treat$mode                   <- p[["r1_wia_treat_mode"]]
  ed$vars$r2eheavy$post_definitive_icu$mode   <- p[["post_definitive_icu_mode"]]

  # ── R1 surgical candidacy / evacuation probabilities (Issue #112) ──────
  ed$vars$r1$other$pri2_surgery       <- p[["pri2_surg_prob"]]
  ed$vars$r1$other$pri3_dnbi_surgery  <- p[["pri3_dnbi_surg_prob"]]
  ed$vars$r1$other$pri3_other_surgery <- p[["pri3_other_surg_prob"]]
  ed$vars$r1$other$disease_surgery_pct <- p[["disease_surgery_pct"]]
  ed$vars$r1$other$pri1_evac          <- p[["pri1_evac_prob"]]
  ed$vars$r1$other$pri2_evac          <- p[["pri2_evac_prob"]]

  # ── Surgical pathway split (Issue #173) ─────────────────────────────────
  ed$vars$r1$other$pri1_dcs_rate <- p[["pri1_dcs_rate"]]
  ed$vars$r1$other$pri2_dcs_rate <- p[["pri2_dcs_rate"]]
  ed$vars$r1$other$pri3_dcs_rate <- p[["pri3_dcs_rate"]]

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
  ed$vars$dow$treatment_efficacy$r2b_icu_penalty  <- p[["r2b_icu_penalty"]]

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
  ed$vars$r2b$post_op_icu$share            <- p[["r2b_icu_share"]]
  ed$vars$r2b$post_op_icu$forward_hold_max <- p[["r2b_forward_hold_max"]]
  ed$vars$r2b$holding$hold_threshold <- p[["r2b_hold_threshold"]]
  ed$vars$r2b$surgery$pre_open_window_min <- p[["r2b_pre_open_window"]]

  # ── Simplex-constrained compositions (Issue #158) ───────────────────────
  # Each group's two balance coordinates are back-transformed to a whole
  # composition before the run. The assertion is the guarantee the screen
  # rests on: every design point, including the corners of the coordinate
  # box, must yield three strictly positive parts summing to one, since a
  # composition that had drifted off the simplex would make the design point
  # a run of a model configuration nobody chose.
  for (g in MORRIS_COMPOSITIONS) {
    x <- ilr3_inv(c(p[[g$coords[1]]], p[[g$coords[2]]]))
    stopifnot(
      length(x) == 3L,
      all(is.finite(x)),
      all(x > 0), all(x < 1),
      abs(sum(x) - 1) < 1e-9
    )
    ed <- g$apply(ed, x)
  }

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

# ── Response variable registry ────────────────────────────────────────────────

#' Morris response variables, their plain-English labels, their decision
#' domain, the selection criteria they satisfy, and the scalar reduction
#' applied where the underlying KPI is not itself a scalar
#'
#' @format Data frame with columns: name (the identifier used in
#'   `outputs/morris_ranking_<name>.csv` and `images/morris_<name>.png`),
#'   label, domain, criteria, reduction
#'
#' @details The response set is the Model Outputs KPI set (see README Model
#'   Outputs), not a separate selection. Each of the seventeen documented
#'   KPIs contributes one or more responses here; a KPI that is a vector, a
#'   distribution, or a time series contributes one response per element or
#'   one per named summary statistic, because Morris requires a single
#'   response value per design point. The `criteria` column records which of
#'   the five documented selection criteria (C1 doctrinal standard
#'   compliance, C2 planner decision relevance, C3 causal pathway position,
#'   C4 binding constraint identification, C5 health outcome attribution)
#'   the parent KPI satisfies, carried across unchanged from its Model
#'   Outputs entry, so the screen's response set is self-documenting in code
#'   rather than only in the README.
#'
#'   Two responses have no Model Outputs parent and are retained as derived
#'   aggregates rather than as KPIs in their own right: `system_ot_q`, the
#'   sum of the two theatre queue responses, which is the ranking reported in
#'   `outputs/morris_ranking.csv` and in the README's published table; and
#'   `transport_util`, which applies Domain 3's utilisation reduction to the
#'   transport fleet, whose queues stay near zero under baseline demand and
#'   would otherwise register no sensitivity at all (Issue #6). Both are
#'   marked as derived in the `domain` column.
#'
#'   Counts are reported as the per-replication mean rather than as a total
#'   across the replications evaluated at a design point, so a response keeps
#'   the same scale whether a screen runs at three replications per point or
#'   at five. Rates are already replication-count invariant, being divided by
#'   the arrival count over the same replications.
morris_kpis <- data.frame(
  name = c(
    # ── Domain 1 — Mortality and preventable death ──────────────────────
    "dow_count",
    "dow_rate_r1", "dow_rate_r2b", "dow_rate_r2e",
    "dow_rate_r2e_postop", "dow_rate_ame_wait",
    # ── Domain 2 — Time-to-care from R1 arrival ─────────────────────────
    "time_to_surgery_mean", "time_to_surgery_p90",
    "r2b_dwell_mean", "r2b_r2e_transit_mean", "r2e_dwell_mean",
    # ── Domain 3 — Surgical throughput ──────────────────────────────────
    "ot_util_r2b", "ot_util_r2e",
    "r2b_surgery_count", "r2e_surgery_count",
    # ── Domain 4 — Echelon load and capacity ────────────────────────────
    "r2b_ot_q", "r2e_ot_q", "r2e_icu_q", "transport_q",
    # ── Domain 5 — Flow and disposition ─────────────────────────────────
    "rtd_rate_r1", "rtd_rate_r2b", "rtd_rate_r2e", "r2b_bypass_rate",
    # ── Domain 6 — Combat power ─────────────────────────────────────────
    "total_rtd",
    # ── Domain 7 — Strategic evacuation and Role 4 demand ───────────────
    "role4_peak_occupancy", "role4_mean_occupancy",
    "ame_sortie_demand",
    "ame_wait_critical_mean", "ame_wait_standard_mean",
    "ame_backlog_critical_mean", "ame_backlog_critical_peak",
    "ame_backlog_standard_mean", "ame_backlog_standard_peak",
    "ame_sorties_flown",
    # ── Derived aggregates (no Model Outputs parent) ────────────────────
    "system_ot_q", "transport_util"
  ),
  label = c(
    "Total DOW Count",
    "DOW Rate — R1", "DOW Rate — R2B", "DOW Rate — R2E Arrival",
    "DOW Rate — R2E Post-Operative", "DOW Rate — Awaiting Strategic AME",
    "Mean Time to First Surgical Incision", "p90 Time to First Surgical Incision",
    "Mean R2B Dwell Time", "Mean R2B to R2E Transit Time", "Mean R2E Dwell Time",
    "R2B OT Utilisation", "R2E OT Utilisation",
    "R2B Surgeries per Run", "R2E Surgical Episodes per Run",
    "Mean R2B OT Queue", "Mean R2E OT Queue", "Mean R2E ICU Queue",
    "Mean Transport Queue (PMV Amb + HX240M)",
    "RTD Rate — R1", "RTD Rate — R2B", "RTD Rate — R2E", "R2B Bypass Rate",
    "Total RTD Count",
    "Role 4 Peak Bed Occupancy", "Role 4 Mean Bed Occupancy",
    "Unconstrained-Baseline AME Sortie Demand",
    "Mean Strategic AME Wait — Critical Route",
    "Mean Strategic AME Wait — Standard Route",
    "Mean Strategic AME Backlog — Critical Pool",
    "Peak Strategic AME Backlog — Critical Pool",
    "Mean Strategic AME Backlog — Standard Pool",
    "Peak Strategic AME Backlog — Standard Pool",
    "Strategic AME Sorties Flown",
    "System OT Queue (R2B + R2E)",
    "Mean Transport Utilisation (PMV Amb + HX240M)"
  ),
  domain = c(
    rep("1 — Mortality", 6),
    rep("2 — Time-to-care", 5),
    rep("3 — Surgical throughput", 4),
    rep("4 — Echelon load", 4),
    rep("5 — Flow and disposition", 4),
    "6 — Combat power",
    rep("7 — Strategic evacuation", 10),
    rep("Derived", 2)
  ),
  criteria = c(
    "C1, C2, C5",
    rep("C1, C2, C3, C5", 5),
    rep("C1, C2, C3, C5", 2),
    "C1, C3, C4", "C1, C3", "C1, C3, C4",
    "C3, C4", "C3, C4", "C2, C3, C4", "C2, C3, C4",
    rep("C3, C4", 4),
    rep("C1, C2, C5", 3), "C2, C3, C4",
    "C2, C5",
    "C2, C3, C5", "C2, C3, C5",
    "C2, C4, C5",
    "C2, C4, C5", "C2, C4, C5",
    rep("C3, C4, C5", 4),
    "C3, C4, C5",
    "C3, C4", "C3, C4"
  ),
  reduction = c(
    "scalar — per-replication mean count",
    rep("one response per echelon; deaths at that echelon over total arrivals", 5),
    "distribution — mean", "distribution — p90",
    "distribution — mean", "distribution — mean", "distribution — mean",
    "one response per echelon; time-weighted mean fraction of theatre capacity busy",
    "one response per echelon; time-weighted mean fraction of theatre capacity busy",
    "time series — per-replication mean total over the run",
    "time series — per-replication mean total over the run",
    rep("time series — time-weighted mean queue length", 4),
    rep("one response per echelon; returns at that echelon over total arrivals", 3),
    "scalar — bypassed casualties over WIA arrivals",
    "scalar — per-replication mean count",
    "time series — per-replication mean of the peak concurrent census",
    "time series — patient-days over the engagement window",
    "scalar — per-replication mean total sorties required",
    "distribution — mean, per route", "distribution — mean, per route",
    "time series — time-weighted mean, per pool",
    "time series — peak, per pool",
    "time series — time-weighted mean, per pool",
    "time series — peak, per pool",
    "event series — per-replication mean count of sorties flown",
    "time series — time-weighted mean queue length, summed across echelons",
    "time series — time-weighted mean fraction of fleet busy"
  ),
  stringsAsFactors = FALSE
)

#' Plain-English label per response, keyed by response name
#'
#' @details Retained as a named list for the per-response plotting and
#'   ranking loops in run_morris(); `morris_kpis` is the authority.
kpi_labels <- setNames(as.list(morris_kpis$label), morris_kpis$name)

# ── KPI extraction ────────────────────────────────────────────────────────────

#' Evaluate an expression against a fixed RNG stream, restoring the caller's
#' stream afterwards
#'
#' @param expr Expression to evaluate
#' @param seed Integer seed applied for the duration of the evaluation
#' @return The value of `expr`
#'
#' @details Two of the reused analysis derivations draw random numbers
#'   (`compute_role4_census()` draws each evacuee's Role 4 length of stay
#'   from a triangular distribution). Left unguarded, those draws would
#'   advance the screening process's own RNG stream between design points and
#'   so change the replication seeds every later design point runs under.
#'   Fixing the seed also makes the length-of-stay draws a common random
#'   number across design points, removing a source of noise from the Role 4
#'   responses that has nothing to do with the parameter being perturbed.
with_fixed_rng <- function(expr, seed = 20260729L) {
  has_seed <- exists(".Random.seed", envir = globalenv())
  if (has_seed) {
    old <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  } else {
    on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())), add = TRUE)
  }
  set.seed(seed)
  force(expr)
}

#' Extract the Morris response vector from a run_replications() monitoring list
#'
#' @param mon Named list with arrivals, attributes, resources
#' @return Named numeric vector, one element per row of `morris_kpis`, in
#'   that order. An element is NA when the design point produced no casualty
#'   in the cohort the response is measured over (for example a mean AME wait
#'   on a route nobody took); NA is returned rather than zero because zero
#'   would assert a measured value of zero minutes rather than the absence of
#'   a measurement, and run_morris() reports the count of such points
#'   alongside each ranking.
#'
#' @details Queue and utilisation responses come from
#'   `summarise_replications()` and `compute_utilisation()` over the resource
#'   monitor. Every other response is a per-casualty measure read from the
#'   arrivals and attributes monitors, reusing the derivations already
#'   present in `R/analysis.R` — `build_attributes_wide()` for the pivot to
#'   one row per casualty, and `compute_role4_census()`, `compute_ame_demand()`,
#'   `compute_ame_backlog()` and `compute_ame_sorties()` for the Role 4 and
#'   strategic evacuation measures — rather than restating them here.
extract_kpis <- function(mon) {
  kpi <- summarise_replications(mon)

  safe_q <- function(pattern) {
    v <- kpi %>%
      filter(grepl(pattern, resource)) %>%
      summarise(v = mean(mean_q, na.rm = TRUE)) %>%
      pull(v)
    if (length(v) == 0 || is.na(v)) 0 else v
  }

  # Empty cohorts are routine at an extreme design point, so every summary
  # below goes through these rather than through mean()/quantile()/max()
  # directly, all three of which return NaN, an error, or -Inf on no input.
  safe_mean <- function(x) { x <- x[is.finite(x)]; if (length(x) == 0) NA_real_ else mean(x) }
  safe_p90  <- function(x) { x <- x[is.finite(x)]; if (length(x) == 0) NA_real_ else unname(quantile(x, 0.90)) }
  safe_max  <- function(x) { x <- x[is.finite(x)]; if (length(x) == 0) NA_real_ else max(x) }

  arrivals <- mon$arrivals
  n_arrivals <- nrow(arrivals)
  n_reps     <- max(1L, dplyr::n_distinct(arrivals$replication))
  # The engagement window every "per day" and "over the run" reduction below
  # is measured against, derived the same way analyse_run() derives it.
  n_days <- if (n_arrivals == 0) 1 else max(1, ceiling(max(arrivals$start_time, na.rm = TRUE) / 1440))

  attributes_wide <- build_attributes_wide(mon$attributes, arrivals)
  combined <- arrivals %>%
    left_join(attributes_wide, by = c("name", "replication")) %>%
    mutate(casualty_type = str_extract(name, "^[^_]+"))

  # build_attributes_wide() guarantees the columns analyse_run() reads
  # directly; these are the remainder this function reads, absent from a run
  # in which no casualty ever reached the stage that sets them.
  for (nm in c("injury_type", "priority", "r2b_surgery_start", "r2e_surgery_1_start",
               "r2e_surgery_2_start", "r2b_treatment_start_time", "r2b_departure_time",
               "r2e_arrival_time", "r2e_departure_time", "return_day", "return_echelon",
               "dnbi_type", "r2b_treated", "r2e_treated")) {
    if (!nm %in% names(combined)) combined[[nm]] <- NA_real_
  }
  a <- function(nm) as.numeric(combined[[nm]])

  # ── Domain 4 — echelon load (and the two derived aggregates) ───────────
  r2e_icu_q   <- safe_q("^b_r2eheavy_icu_")
  r2b_ot_q    <- safe_q("^b_r2b_ot_")
  r2e_ot_q    <- safe_q("^b_r2eheavy_ot_")
  system_ot_q <- r2b_ot_q + r2e_ot_q

  transport_q    <- safe_q("^t_PMVAmb_|^t_HX240M_")
  transport_util <- compute_utilisation(mon, "^t_PMVAmb_|^t_HX240M_")

  # ── Domain 1 — mortality ───────────────────────────────────────────────
  # dow_echelon encoding: 1 = R1, 2 = R2B, 3 = R2E arrival, 4 = R2E
  # post-operative, 5 = awaiting strategic AME. The Model Outputs entry
  # names three echelons and predates the post-operative and AME-wait
  # checkpoints; all five the model can set are screened.
  dow     <- a("dow")
  dow_ech <- a("dow_echelon")
  died    <- !is.na(dow) & dow == 1
  dow_count <- sum(died) / n_reps
  dow_rate  <- function(k) {
    if (n_arrivals == 0) NA_real_ else sum(died & !is.na(dow_ech) & dow_ech == k) / n_arrivals
  }

  # ── Domain 2 — time-to-care ────────────────────────────────────────────
  first_surgery_start <- pmin(a("r2b_surgery_start"), a("r2e_surgery_1_start"), na.rm = TRUE)
  time_to_surgery <- first_surgery_start - combined$start_time
  time_to_surgery <- time_to_surgery[combined$casualty_type != "kia"]
  time_to_surgery <- time_to_surgery[is.finite(time_to_surgery) & time_to_surgery >= 0]

  r2b_dwell    <- a("r2b_departure_time") - a("r2b_treatment_start_time")
  transit      <- a("r2e_arrival_time")   - a("r2b_departure_time")
  r2e_dwell    <- a("r2e_departure_time") - a("r2e_arrival_time")
  non_negative <- function(x) x[is.finite(x) & x >= 0]

  # ── Domain 3 — surgical throughput ─────────────────────────────────────
  ot_util_r2b <- compute_utilisation(mon, "^b_r2b_ot_")
  ot_util_r2e <- compute_utilisation(mon, "^b_r2eheavy_ot_")
  r2b_surgery_count <- sum(!is.na(a("r2b_surgery_start"))) / n_reps
  # Theatre episodes, not casualties: a damage control casualty operated on
  # at R2E occupies theatre twice and is counted twice.
  r2e_surgery_count <- (sum(!is.na(a("r2e_surgery_1_start"))) +
                        sum(!is.na(a("r2e_surgery_2_start")))) / n_reps

  # ── Domain 5 / 6 — flow, disposition, combat power ─────────────────────
  returned  <- !is.na(a("return_day"))
  ret_ech   <- a("return_echelon")
  rtd_rate  <- function(k) {
    if (n_arrivals == 0) NA_real_ else sum(returned & !is.na(ret_ech) & ret_ech == k) / n_arrivals
  }
  total_rtd <- sum(returned) / n_reps

  n_wia <- sum(combined$casualty_type == "wia", na.rm = TRUE)
  r2b_bypass_rate <- if (n_wia == 0) NA_real_ else {
    sum(!is.na(a("r2e_treated")) & is.na(a("r2b_treated"))) / n_wia
  }

  # ── Domain 7 — strategic evacuation and Role 4 demand ──────────────────
  role4_params <- env_data$vars$role4
  role4_peak <- NA_real_
  role4_mean <- NA_real_
  ame_sortie_demand <- NA_real_
  if (!is.null(role4_params)) {
    census <- with_fixed_rng(compute_role4_census(combined, role4_params))
    if (nrow(census) > 0) {
      daily <- census %>%
        group_by(replication, day) %>%
        summarise(total = sum(occupancy), .groups = "drop")
      # Peak is taken over every censused day, including the tail of
      # convalescence past the end of the engagement, matching the peak
      # analyse_run() reports. The mean is instead patient-days accrued
      # within the engagement window over the length of that window, so it
      # is a concurrent-occupancy rate rather than a figure whose
      # denominator moves with how long the last evacuee stays.
      role4_peak <- safe_mean(
        (daily %>% group_by(replication) %>% summarise(pk = max(total), .groups = "drop"))$pk
      )
      role4_mean <- sum(daily$total[daily$day <= n_days]) / (n_reps * n_days)
    } else {
      role4_peak <- 0
      role4_mean <- 0
    }

    ame_capacity <- with(resolve_ame_airframe(role4_params),
                         critical_capacity + standard_capacity)
    demand <- compute_ame_demand(combined, ame_capacity)
    ame_sortie_demand <- if (nrow(demand) == 0) 0 else sum(demand$sorties_required) / n_reps
  }

  ame_wait  <- a("ame_wait_minutes")
  ame_route <- a("ame_route")
  ame_wait_route <- function(k) safe_mean(ame_wait[!is.na(ame_route) & ame_route == k])

  backlog <- compute_ame_backlog(mon$attributes, n_days)
  backlog_stat <- function(pool_label, stat) {
    sub <- backlog %>% filter(as.character(pool) == pool_label)
    if (nrow(sub) == 0) return(0)
    per_rep <- sub %>%
      group_by(replication) %>%
      arrange(time, .by_group = TRUE) %>%
      mutate(dt = pmax(lead(time, default = max(time)) - time, 0)) %>%
      summarise(
        # A replication whose backlog events all fall at one instant carries
        # no elapsed time to weight by, which would make weighted.mean() NaN.
        mean_backlog = if (sum(dt) > 0) weighted.mean(backlog, w = dt, na.rm = TRUE)
                       else mean(backlog, na.rm = TRUE),
        peak_backlog = max(backlog, na.rm = TRUE),
        .groups      = "drop"
      )
    safe_mean(per_rep[[if (stat == "mean") "mean_backlog" else "peak_backlog"]])
  }

  ame_sorties_flown <- 0
  if (!is.null(role4_params)) {
    sorties <- compute_ame_sorties(mon$resources, role4_params, n_days)
    # One row per (replication, sortie_day, pool) with the outcome repeated
    # across both pools, so the flown count is over distinct opportunities.
    ame_sorties_flown <- if (nrow(sorties) == 0) 0 else {
      nrow(distinct(sorties %>% filter(outcome == "Flown"), replication, sortie_day)) / n_reps
    }
  }

  out <- c(
    dow_count                 = dow_count,
    dow_rate_r1               = dow_rate(1),
    dow_rate_r2b              = dow_rate(2),
    dow_rate_r2e              = dow_rate(3),
    dow_rate_r2e_postop       = dow_rate(4),
    dow_rate_ame_wait         = dow_rate(5),
    time_to_surgery_mean      = safe_mean(time_to_surgery),
    time_to_surgery_p90       = safe_p90(time_to_surgery),
    r2b_dwell_mean            = safe_mean(non_negative(r2b_dwell)),
    r2b_r2e_transit_mean      = safe_mean(non_negative(transit)),
    r2e_dwell_mean            = safe_mean(non_negative(r2e_dwell)),
    ot_util_r2b               = ot_util_r2b,
    ot_util_r2e               = ot_util_r2e,
    r2b_surgery_count         = r2b_surgery_count,
    r2e_surgery_count         = r2e_surgery_count,
    r2b_ot_q                  = r2b_ot_q,
    r2e_ot_q                  = r2e_ot_q,
    r2e_icu_q                 = r2e_icu_q,
    transport_q               = transport_q,
    rtd_rate_r1               = rtd_rate(1),
    rtd_rate_r2b              = rtd_rate(2),
    rtd_rate_r2e              = rtd_rate(3),
    r2b_bypass_rate           = r2b_bypass_rate,
    total_rtd                 = total_rtd,
    role4_peak_occupancy      = role4_peak,
    role4_mean_occupancy      = role4_mean,
    ame_sortie_demand         = ame_sortie_demand,
    ame_wait_critical_mean    = ame_wait_route(1),
    ame_wait_standard_mean    = ame_wait_route(2),
    ame_backlog_critical_mean = backlog_stat("Critical (ICU, CCATT/CCAST)", "mean"),
    ame_backlog_critical_peak = backlog_stat("Critical (ICU, CCATT/CCAST)", "peak"),
    ame_backlog_standard_mean = backlog_stat("Standard (Hold, CSU)", "mean"),
    ame_backlog_standard_peak = backlog_stat("Standard (Hold, CSU)", "peak"),
    ame_sorties_flown         = ame_sorties_flown,
    system_ot_q               = system_ot_q,
    transport_util            = transport_util
  )

  # Order and completeness are a hard contract: run_morris() indexes the
  # response matrix by name and vapply()s against a fixed length.
  stopifnot(identical(names(out), morris_kpis$name))
  out
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
#' @return Named numeric vector, one element per row of `morris_kpis` — see
#'   extract_kpis()
#'
#' @details Modifies the global env_data via apply_params() then restores it.
#'   Every screened parameter, ot_hours included, reaches the model through
#'   the vars tree; build_env() reads the shift length from there, so no
#'   parameter needs extracting and threading separately.
eval_params <- function(params_row, n_rep, n_days, max_cores = NULL,
                        crn_seed = NULL, return_sd = FALSE) {
  p <- setNames(as.numeric(params_row), morris_params$name)

  env_data <<- apply_params(env_data_base, p)
  # Common random numbers, stated explicitly rather than relied on. Design
  # points already share their replication seeds without this, because
  # run_replications() snapshots the caller's RNG stream position and restores
  # it on exit (see its @details), so consecutive calls draw the same seed
  # vector. That is a property of a function written for measurement
  # reproducibility, not a guarantee this one asked for, and a screen that
  # depends on it should say so instead of inheriting it by side effect.
  # Setting crn_seed pins the behaviour here and keeps the screen correct if
  # that restoration ever stops. It is not a free switch, and an earlier
  # comment here wrongly said it was. The restoration makes every design point
  # share a seed vector; it does not fix which vector, that being whatever the
  # ambient stream had reached. Pinning fixes it, so a pinned screen and an
  # unpinned one share seeds within themselves but not with each other and
  # return different responses, which was measured rather than reasoned. NULL
  # is therefore the default: the shipped default has to reproduce the shipped
  # results, and the tracked caches under data/sensitivity/ were produced
  # unpinned.
  if (!is.null(crn_seed)) set.seed(crn_seed)
  mon <- run_replications(n_rep, n_days, max_cores = max_cores)
  out <- extract_kpis(mon)

  # Per-point spread across replications. The Sobol estimators treat each
  # point's response as if it were deterministic, so the replication noise
  # left in it inflates the total variance and pushes indices outside [0, 1].
  # Recording the spread costs no simulation and is what makes that nugget
  # measurable rather than inferred from index pathology after the fact.
  if (return_sd) {
    per_rep <- lapply(seq_len(n_rep), function(k) {
      sub <- mon
      for (nm in c("arrivals", "attributes", "resources")) {
        if (!is.null(sub[[nm]]) && "replication" %in% names(sub[[nm]])) {
          sub[[nm]] <- sub[[nm]][sub[[nm]]$replication == k, , drop = FALSE]
        }
      }
      tryCatch(extract_kpis(sub), error = function(e) setNames(rep(NA_real_, length(out)), names(out)))
    })
    m <- do.call(rbind, per_rep)
    attr(out, "sd") <- apply(m, 2, stats::sd, na.rm = TRUE)
  }
  out
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
#' @param crn_seed Seed pinned before each design point's replications, so
#'   every point runs the same noise realisation regardless of what the
#'   ambient stream had reached. Design points already share seeds via
#'   run_replications()' stream restoration, so this adds no property the
#'   screen lacked; what it adds is independence from everything that ran
#'   before, which the restoration does not give. It is therefore not a
#'   free switch: a pinned screen and an unpinned one draw different seed
#'   vectors and so produce different responses. NULL (default) leaves the
#'   stream untouched and reproduces the published ranking.
#' @param cache_dir Optional directory path; when supplied, each design
#'   point's responses are appended to points.csv there as it completes and
#'   read back on a later call, so an interrupted screen resumes instead of
#'   restarting. Clear it whenever the seed, r, the level count, the
#'   parameter bounds or crn_seed change, or the cache would be read against
#'   a design it does not belong to.
#' @param max_cores Optional integer cap on mclapply's mc.cores at each
#'   design point, passed through to run_replications() via eval_params()
#'   (see run_replications()'s own @param for why this matters for
#'   Shiny-triggered, locally-run screens). NULL preserves prior behaviour.
#' @param images_dir Directory path for saving the per-response PNG plots.
#'   Defaults to `file.path(output_dir, "images")`, which is gitignored, so
#'   an ordinary screening run cannot overwrite the tracked baseline plots in
#'   `images/` or scatter untracked ones alongside them unless the caller
#'   names that directory explicitly — the same contract analyse_run() has
#'   carried since Issue #154. A screen writes one plot per response rather
#'   than the seven this function once produced, so a default of `images/`
#'   would now leave twenty-nine untracked files in a tracked directory.
#' @return Named list: morris_objs (per-response sensitivity objects), Y
#'   (response matrix), X (design matrix), ranking (the primary system OT
#'   queue ranking, sorted descending by mu_star), rankings (the same data
#'   frame per response, named by response), kpis (`morris_kpis`)
#'
#' @details Runs r*(p+1) model evaluations where p = nrow(morris_params). The
#'   design is generated once and every response is told against the same
#'   response matrix, so the number of responses screened does not change the
#'   number of simulation runs — the marginal cost of a response is one
#'   `tell()`, one `ggsave()` and one `write.csv()`. Saves a Morris plot
#'   (mu* vs sigma) per response to images_dir and a ranking CSV per response
#'   to output_dir, plus `morris_ranking.csv` for the primary response. The
#'   global env_data is restored to env_data_base on exit regardless of
#'   errors.
run_morris <- function(n_days = 30, n_rep = 5, r = 20, levels = 4,
                       output_dir = "outputs", progress_dir = NULL, max_cores = NULL,
                       images_dir = file.path(output_dir, "images"), cache_dir = NULL,
                       crn_seed = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(images_dir, recursive = TRUE, showWarnings = FALSE)

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

  if (!is.null(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- if (!is.null(cache_dir)) file.path(cache_dir, "points.csv") else NULL

  Y <- t(vapply(seq_len(nrow(sa$X)), function(i) {
    # A production screen is r * (p + 1) design points in one long-lived
    # process: at r = 20 over the current parameter set that is 1,320 points
    # and some eleven hours, all of which a lost process previously discarded.
    # With cache_dir each point's responses are written as it completes and
    # read back on a later call, so an interrupted screen resumes. The design
    # follows from the seed, so a cached point belongs to the screen being
    # resumed only while the seed, r, the level count and the parameter bounds
    # are unchanged; clear the cache when any of those move.
    if (!is.null(cache_file) && file.exists(cache_file)) {
      cached <- cache_lookup(cache_file, i, morris_kpis$name)
      if (!is.null(cached)) {
        message(sprintf("  Point %d / %d (cached)", i, nrow(sa$X)))
        return(cached)
      }
    }
    message(sprintf("  Point %d / %d", i, nrow(sa$X)))
    kpis <- tryCatch(
      eval_params(sa$X[i, ], n_rep, n_days, max_cores = max_cores,
                  crn_seed = crn_seed),
      error = function(e) {
        warning(sprintf("Eval %d failed: %s", i, conditionMessage(e)))
        setNames(rep(NA_real_, nrow(morris_kpis)), morris_kpis$name)
      }
    )
    if (!is.null(cache_file) && !all(is.na(kpis))) cache_append(cache_file, i, kpis)
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
  }, numeric(nrow(morris_kpis))))

  env_data <<- env_data_base

  # Persisted so a plotting-only change (e.g. Issue #112's follow-up fixing
  # illegible overlapping labels at p=55) can re-render images/*.png from
  # the same design + responses without re-running the full simulation
  # sweep — a 94-minute cost at r=5/p=55 in this project's development
  # environment (see the README's reduced-r note) that a labelling tweak
  # alone should never require paying twice.
  saveRDS(list(X = sa$X, Y = Y, binf = sa$binf, bsup = sa$bsup),
          file.path(output_dir, "morris_design_and_responses.rds"))

  # A response carrying no variation across the design gives a mu* that is
  # arithmetically zero but carries no information, which reads identically
  # to a confident finding that no parameter influences it. The two are
  # distinguished here: a degenerate response's mu*/sigma are written as NA
  # with the reason recorded in the ranking's own `note` column, never as
  # zero. The threshold is relative to the response's own magnitude, so it
  # catches a constant response at any scale rather than only near zero.
  is_degenerate <- function(y) {
    y <- y[is.finite(y)]
    if (length(y) < 2) return(TRUE)
    sd(y) <= 1e-9 * max(1, abs(mean(y)))
  }

  #' Per-parameter mu*/sigma for one response, with the diagnostics needed to
  #' tell an uninformative response from an uninfluenced parameter.
  rank_response <- function(obj, kpi) {
    y          <- Y[, kpi]
    n_na       <- sum(!is.finite(y))
    degenerate <- is_degenerate(y)
    ee         <- obj$ee

    n_finite <- apply(ee, 2, function(v) sum(is.finite(v)))
    mu_star  <- apply(ee, 2, function(v) if (any(is.finite(v))) mean(abs(v), na.rm = TRUE) else NA_real_)
    sigma    <- apply(ee, 2, function(v) if (sum(is.finite(v)) > 1) sd(v, na.rm = TRUE) else NA_real_)

    note <- rep("", length(n_finite))
    note[n_finite < nrow(ee)] <- "some trajectories produced a non-finite elementary effect"
    note[n_finite == 0]       <- "no finite elementary effect at any trajectory"
    if (degenerate) {
      note[]    <- "degenerate response — insufficient variation across the design"
      mu_star[] <- NA_real_
      sigma[]   <- NA_real_
    }

    data.frame(
      parameter        = morris_params$name,
      mu_star          = as.numeric(mu_star),
      sigma_ee         = as.numeric(sigma),
      n_finite_ee      = as.integer(n_finite),
      kpi              = kpi,
      criteria         = morris_kpis$criteria[match(kpi, morris_kpis$name)],
      response_mean    = if (any(is.finite(y))) mean(y, na.rm = TRUE) else NA_real_,
      response_sd      = if (sum(is.finite(y)) > 1) sd(y, na.rm = TRUE) else NA_real_,
      response_na_pts  = as.integer(n_na),
      degenerate       = degenerate,
      note             = note,
      row.names        = NULL,
      stringsAsFactors = FALSE
    ) %>% arrange(desc(mu_star))
  }

  rankings    <- list()
  degenerates <- character(0)

  morris_objs <- lapply(names(kpi_labels), function(kpi) {
    obj <- sa
    tell(obj, Y[, kpi])

    ranking_kpi <- rank_response(obj, kpi)
    rankings[[kpi]] <<- ranking_kpi
    if (isTRUE(ranking_kpi$degenerate[1])) degenerates <<- c(degenerates, kpi)
    write.csv(ranking_kpi, file.path(output_dir, sprintf("morris_ranking_%s.csv", kpi)),
              row.names = FALSE)

    plot_title <- sprintf("Morris Screening — %s", kpi_labels[[kpi]])
    p <- tryCatch(
      if (isTRUE(ranking_kpi$degenerate[1])) stop("degenerate response")
      else plot_morris_scatter(obj, plot_title),
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
    ggsave(file.path(images_dir, sprintf("morris_%s.png", kpi)), plot = p,
           width = 12, height = 9, dpi = 130)

    obj
  })
  names(morris_objs) <- names(kpi_labels)

  message(sprintf("Morris plots saved to %s (%d responses)", images_dir, length(kpi_labels)))
  message(sprintf("Per-response rankings written to %s/morris_ranking_<response>.csv", output_dir))
  if (length(degenerates) > 0) {
    warning(sprintf(
      "%d response(s) carried no usable variation across the design and are flagged degenerate in their ranking CSV (mu*/sigma written as NA, not zero): %s",
      length(degenerates), paste(degenerates, collapse = ", ")
    ), call. = FALSE)
  }

  # The primary ranking remains system OT queue, the aggregate bottleneck
  # response the README's published table reports, written under its
  # historical filename as well as its per-response one.
  ranking <- rankings[["system_ot_q"]]
  write.csv(ranking, file.path(output_dir, "morris_ranking.csv"), row.names = FALSE)
  message("Primary parameter ranking written to outputs/morris_ranking.csv")
  message("\nTop parameters by mu* (system OT queue):")
  print(ranking, digits = 4)

  write_screen_metadata(output_dir, "morris", list(
    r                    = r,
    levels               = levels,
    grid_jump            = 2,
    n_params             = nrow(morris_params),
    n_design_points      = nrow(sa$X),
    n_rep                = n_rep,
    n_days               = n_days,
    cache_dir            = if (is.null(cache_dir)) "none" else cache_dir,
    crn_seed             = if (is.null(crn_seed)) "none" else crn_seed,
    degenerate_responses = if (length(degenerates) == 0) "none" else degenerates
  ))

  list(morris_objs = morris_objs, Y = Y, X = sa$X,
       ranking = ranking, rankings = rankings, kpis = morris_kpis)
}

# ── Sobol variance decomposition ──────────────────────────────────────────────

#' Concentration parameter for a composition group's Dirichlet sampler
#'
#' @param g One element of `MORRIS_COMPOSITIONS`
#' @return Numeric scalar concentration
#'
#' @details Chosen so the sampler spans the same planning range the group's
#'   Morris bounds span rather than an arbitrary spread. Under a Dirichlet
#'   with concentration $\kappa$ the leading part has standard deviation
#'   $\sqrt{p(1-p)/(\kappa+1)}$; setting two standard deviations equal to the
#'   half-width of the group's `lead_range` and solving for $\kappa$ gives
#'   roughly 29 for the triage split, 31 for the DNBI composition and 36 for
#'   the mass casualty split. A single figure would have been a spread nobody
#'   had argued for; this one is the spread already documented.
composition_concentration <- function(g) {
  p    <- g$baseline[1] / sum(g$baseline)
  half <- diff(g$lead_range) / 2
  p * (1 - p) / (half / 2)^2 - 1
}

#' Sample whole compositions from a Dirichlet centred on a group's baseline
#'
#' @param n Number of compositions to draw
#' @param g One element of `MORRIS_COMPOSITIONS`
#' @return Numeric matrix, n rows by 2 columns — the sampled compositions in
#'   balance coordinates, ready to substitute for the uniform draws
#'   `run_sobol()` would otherwise make on those columns.
#'
#' @details The confirmatory treatment the Morris screen cannot give. Morris
#'   varies each balance coordinate independently and reports a mu* per
#'   coordinate, which answers whether a contrast matters but not how much of
#'   the output variance the composition as a whole explains. Sampling whole
#'   compositions and decomposing the variance answers the second question,
#'   which is the form a planner can act on. Drawn as normalised gamma
#'   variates, the standard construction.
rdirichlet_coords <- function(n, g) {
  alpha <- g$baseline / sum(g$baseline) * composition_concentration(g)
  draws <- matrix(rgamma(n * length(alpha), shape = rep(alpha, each = n)),
                  nrow = n)
  draws <- draws / rowSums(draws)
  t(apply(draws, 1, ilr3))
}
#' Write a sidecar describing the run that produced a screen's outputs
#'
#' A results file carries no record of the design behind it, which has already
#' misled this project once: a Sobol decomposition was published whose selected
#' parameters came from a screen later shown to be noise-dominated, and nothing
#' in the output said so. This writes the design alongside the results, so a
#' file found later can be attributed without reconstructing how it was made.
#'
#' @param output_dir Directory the screen writes its results to.
#' @param screen Screen name, used in the filename ("morris" or "sobol").
#' @param fields Named list of design fields to record.
#' @return Invisibly the path written.
write_screen_metadata <- function(output_dir, screen, fields) {
  sha <- tryCatch(
    sub("\\s+$", "", system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)),
    error = function(e) NA_character_
  )
  meta <- c(
    list(screen = screen,
         run_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
         commit = if (length(sha) == 1L) sha else NA_character_,
         r_version = paste0(R.version$major, ".", R.version$minor)),
    lapply(fields, function(v) paste(v, collapse = "|"))
  )
  path <- file.path(output_dir, sprintf("%s_run_metadata.csv", screen))
  utils::write.csv(
    data.frame(field = names(meta), value = unlist(meta, use.names = FALSE),
               stringsAsFactors = FALSE),
    path, row.names = FALSE
  )
  message(sprintf("Run metadata written to %s", path))
  invisible(path)
}


# Both screens cache to one append-only CSV rather than a file per design
# point, so the whole cache is a single small artifact that can be checkpointed
# somewhere durable while a multi-hour sweep runs. The first column is the
# design point index; the rest are that screen's responses, named as the screen
# names them. Morris carries one column per entry in morris_kpis, Sobol the
# five it decomposes, so the width is read from the file rather than assumed.
# The five responses run_sobol() decomposes, in the order it assembles them.
# Named here because the cache now also carries a per-response sd_* column, so
# a reader cannot infer the response set from the file header alone.
SOBOL_RESPONSES <- c("r2b_ot_q", "r2e_ot_q", "system_ot_q",
                     "transport_q", "transport_util")


#' Read one design point's cached response vector
#'
#' @param path Cache CSV path.
#' @param i Design point index.
#' @param cols Optional response names to return, in order. Defaults to every
#'   column of the file except the index, which is the order the screen wrote.
#' @return Named numeric of the responses, or NULL when the point is absent,
#'   the file is unreadable, or the row is incomplete. A miss is always safe:
#'   the caller simply evaluates the point.
cache_lookup <- function(path, i, cols = NULL) {
  tab <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE),
                  error = function(e) NULL)
  if (is.null(tab) || !("i" %in% names(tab))) return(NULL)
  if (is.null(cols)) cols <- setdiff(names(tab), "i")
  if (!all(cols %in% names(tab))) return(NULL)
  hit <- tab[tab$i == i, , drop = FALSE]
  if (nrow(hit) == 0L) return(NULL)
  out <- as.numeric(hit[1L, cols])
  # An individual response can be legitimately NA -- a KPI undefined for a
  # design point that produced no casualties at that echelon, or a response
  # degenerate across the whole design. Only a row that is NA throughout is
  # treated as absent, which is also what an interrupted write leaves behind.
  if (all(is.na(out))) return(NULL)
  stats::setNames(out, cols)
}

#' Append one design point's response vector to the cache
#'
#' @param path Cache CSV path.
#' @param i Design point index.
#' @param res Named numeric of the responses.
#' @return Invisibly NULL; called for the write.
cache_append <- function(path, i, res) {
  row <- as.data.frame(c(list(i = i), as.list(res)))
  utils::write.table(row, path, sep = ",", row.names = FALSE,
                     col.names = !file.exists(path), append = file.exists(path))
  invisible(NULL)
}

#' Report how much of each response is replication noise rather than design signal
#'
#' A Sobol index is a share of the variance the design produces. Where a
#' response's variation between replications at a fixed design point is
#' comparable to its variation across the design, the estimator is resolving
#' noise and its indices will be small, wide and frequently outside [0, 1].
#' Nothing in the indices themselves says which case a reader is looking at,
#' so the comparison is reported here from the per-point standard deviations
#' the cache records.
#'
#' This is a diagnostic and nothing else: it is printed after the indices are
#' computed and never used to select, weight or exclude a response. Screening
#' on it would amount to choosing which results to publish after seeing them.
#'
#' @param cache_file Cache CSV path, or NULL when no cache was used.
#' @param responses Character vector of response names.
#' @param n_rep Replications per design point, used to scale the standard
#'   deviation to a standard error.
#' @return Invisibly a data frame of the per-response diagnostic, or NULL when
#'   the cache carries no standard deviations.
report_point_noise <- function(cache_file, responses, n_rep) {
  if (is.null(cache_file) || !file.exists(cache_file)) return(invisible(NULL))
  tab <- tryCatch(utils::read.csv(cache_file, stringsAsFactors = FALSE),
                  error = function(e) NULL)
  sd_cols <- paste0("sd_", responses)
  if (is.null(tab) || !all(responses %in% names(tab)) ||
      !any(sd_cols %in% names(tab))) {
    return(invisible(NULL))
  }

  rows <- lapply(seq_along(responses), function(k) {
    nm <- responses[k]
    if (!(sd_cols[k] %in% names(tab))) return(NULL)
    y  <- tab[[nm]]
    sdv <- tab[[sd_cols[k]]]
    keep <- is.finite(y) & is.finite(sdv)
    if (sum(keep) < 2) return(NULL)
    within  <- stats::median(sdv[keep]) / sqrt(n_rep)
    between <- stats::sd(y[keep])
    data.frame(
      response       = nm,
      n_points       = sum(keep),
      within_point_se = within,
      across_design_sd = between,
      noise_ratio    = if (between > 0) within / between else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) return(invisible(NULL))

  message("\nPer-response noise diagnostic (median within-point SE vs. across-design SD):")
  print(out, digits = 3, row.names = FALSE)
  message("A noise_ratio approaching 1 means the design moves the response no further than replication scatter does, and that response's indices should be read as unresolved rather than small.")
  invisible(out)
}

#' Run Sobol variance decomposition on a selected parameter subset
#'
#' @param top_params  Character vector of parameter names from morris_params$name
#' @param n_days      Simulation duration per replication (default 30)
#' @param n_rep       Replications per Sobol evaluation point (default 5)
#' @param n_sobol     Sobol sample size N (default 200; total evals = N*(p+2))
#' @param output_dir  Directory for CSV outputs (default "outputs")
#' @param dirichlet   Whether a composition group represented in `top_params`
#'   is sampled as whole compositions from a Dirichlet centred on its
#'   baseline (default TRUE) rather than by independent uniform draws on its
#'   balance coordinates. Both of a group's coordinates are added to the
#'   design whenever either is selected, since a first-order index on one
#'   coordinate of a composition sampled a coordinate at a time would not be
#'   an index on the composition.
#' @param progress_dir Optional directory path; when supplied, an empty
#'   marker file ("point_<i>.done") is written to it as each design point
#'   finishes evaluating (see run_morris()'s equivalent parameter). NULL
#'   (default) disables this and preserves prior behaviour.
#' @param cache_dir Optional directory path; when supplied, each design
#'   point's responses are appended to points.csv there as it completes and
#'   read back on a later call, so an interrupted production run resumes
#'   instead of restarting. Clear it whenever the seed, the selected
#'   parameters or their bounds change, or the cache would be read against
#'   a design it does not belong to.
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
                      max_cores = NULL, dirichlet = TRUE, cache_dir = NULL,
                      nboot = 1000, crn_seed = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # A composition group enters the decomposition whole or not at all.
  dirichlet_groups <- character(0)
  for (nm in names(MORRIS_COMPOSITIONS)) {
    g <- MORRIS_COMPOSITIONS[[nm]]
    if (any(g$coords %in% top_params)) {
      top_params <- union(top_params, g$coords)
      if (dirichlet) dirichlet_groups <- c(dirichlet_groups, nm)
    }
  }

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

  # A composition group's columns are overwritten with the coordinates of
  # Dirichlet-sampled whole compositions, so the group is varied as one
  # object over a plausible planning spread rather than as two coordinates
  # drawn independently over a box.
  for (nm in dirichlet_groups) {
    g <- MORRIS_COMPOSITIONS[[nm]]
    message(sprintf(
      "  %s composition sampled from a Dirichlet at concentration %.1f",
      nm, composition_concentration(g)
    ))
    X1[, g$coords] <- rdirichlet_coords(n_sobol, g)
    X2[, g$coords] <- rdirichlet_coords(n_sobol, g)
  }

  sb_r2b   <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = nboot)
  sb_r2e   <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = nboot)
  sb_sys   <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = nboot)
  sb_tq    <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = nboot)
  sb_tutil <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = nboot)

  full_params <- setNames(morris_params$mode, morris_params$name)

  if (!is.null(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- if (!is.null(cache_dir)) file.path(cache_dir, "points.csv") else NULL

  # Held in a local rather than read back off sb_r2b later: tell_safe()
  # returns NULL for a response whose bootstrap fails, and r2b_ot_q is
  # routinely that response, so a later read off sb_r2b would land on
  # NULL and the run metadata recorded an empty design size.
  n_points <- nrow(sb_r2b$X)

  Y_all <- t(vapply(seq_len(n_points), function(i) {
    # A production decomposition is n_sobol * (p + 2) design points in one
    # long-lived process, hours of compute that a lost process discards
    # entirely. With cache_dir each point's responses are written as it
    # completes and read back on a later call, so an interrupted run resumes.
    # The design is a deterministic function of the seed, so a cached point
    # belongs to the run being resumed only while the seed, the selected
    # parameters and their bounds are unchanged; clear the cache when any of
    # those move.
    if (!is.null(cache_file) && file.exists(cache_file)) {
      cached <- cache_lookup(cache_file, i, SOBOL_RESPONSES)
      if (!is.null(cached)) {
        message(sprintf("  Sobol point %d / %d (cached)", i, n_points))
        return(cached)
      }
    }
    message(sprintf("  Sobol point %d / %d", i, n_points))
    row <- full_params
    row[p_def$name] <- as.numeric(sb_r2b$X[i, ])
    res <- tryCatch(
      {
        kpis <- eval_params(row, n_rep, n_days, max_cores = max_cores,
                            crn_seed = crn_seed, return_sd = TRUE)
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
    if (!is.null(cache_file) && !all(is.na(res))) {
      sdv <- attr(res, "sd")
      row_out <- if (is.null(sdv)) res else
        c(res, stats::setNames(as.numeric(sdv), paste0("sd_", names(res))))
      cache_append(cache_file, i, row_out)
    }
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
  # sensitivity::tell() does not return the told object. It ends in
  # assign(id, x, parent.frame()), where id is deparse(substitute(x)), so it
  # writes the populated object back over the variable it was handed *in the
  # frame it was called from*. Called inside a wrapper that means the wrapper's
  # own local, leaving the caller's object untouched with S/T still empty —
  # which is why this must return `sb` after the call and the caller must
  # assign the result back, rather than relying on tell()'s side effect.
  tell_safe <- function(sb, y, kpi_name) {
    tryCatch({
      tell(sb, y)
      sb
    }, error = function(e) {
      warning(sprintf(
        "Sobol tell() failed for %s (likely a near-zero-variance response — %s): %s",
        kpi_name, "top_params may not include a parameter that moves this KPI",
        conditionMessage(e)
      ))
      NULL
    })
  }

  sb_r2b   <- tell_safe(sb_r2b,   Y_all[, "r2b_ot_q"],       "r2b_ot_q")
  sb_r2e   <- tell_safe(sb_r2e,   Y_all[, "r2e_ot_q"],       "r2e_ot_q")
  sb_sys   <- tell_safe(sb_sys,   Y_all[, "system_ot_q"],    "system_ot_q")
  sb_tq    <- tell_safe(sb_tq,    Y_all[, "transport_q"],    "transport_q")
  sb_tutil <- tell_safe(sb_tutil, Y_all[, "transport_util"], "transport_util")

  sobol_ok <- c(
    r2b_ot_q       = !is.null(sb_r2b),
    r2e_ot_q       = !is.null(sb_r2e),
    system_ot_q    = !is.null(sb_sys),
    transport_q    = !is.null(sb_tq),
    transport_util = !is.null(sb_tutil)
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
    # A Sobol index is a variance share and so lies in [0, 1] with ST >= S1.
    # The Monte Carlo estimators are unbiased but not range-constrained, so a
    # parameter whose true index sits near zero routinely returns a value
    # outside it. That is a statement about resolution, not about the model,
    # and it is flagged here so a reader of the CSV sees it without having to
    # check: an unflagged reader would take a negative S1 for a negative
    # variance share.
    results$flag <- vapply(seq_len(nrow(results)), function(k) {
      f <- character(0)
      if (isTRUE(results$ST[k] > 1))                 f <- c(f, "ST>1")
      if (isTRUE(results$S1[k] < 0))                 f <- c(f, "S1<0")
      if (isTRUE(results$S1[k] > results$ST[k]))     f <- c(f, "S1>ST")
      if (length(f) == 0L) "ok" else paste(f, collapse = ";")
    }, character(1))
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

  report_point_noise(cache_file, SOBOL_RESPONSES, n_rep)

  write_screen_metadata(output_dir, "sobol", list(
    n_sobol          = n_sobol,
    n_params         = nrow(p_def),
    n_design_points  = n_points,
    n_rep            = n_rep,
    n_days           = n_days,
    estimator        = "sobol2007",
    nboot            = nboot,
    crn_seed         = if (is.null(crn_seed)) "none" else crn_seed,
    dirichlet_groups = if (length(dirichlet_groups) == 0) "none" else dirichlet_groups,
    parameters       = p_def$name,
    cache_dir        = if (is.null(cache_dir)) "none" else cache_dir,
    responses_told   = names(sobol_ok)[sobol_ok]
  ))

  message("\nSobol complete.")
  saved
}
