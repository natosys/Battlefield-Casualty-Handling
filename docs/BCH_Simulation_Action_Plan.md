# Battlefield Casualty Handling Simulation — Action Plan for Revision

**Repository:** `natosys/Battlefield-Casualty-Handling`
**Document purpose:** Structured action plan addressing identified limitations across simulation implementation, statistical rigour, and model fidelity.
**Companion document:** `BCH_Task_Role_Allocation.md` — Task-Role Allocation Matrix for Individual Resource Modelling (Issue 4 supplement)

---

## Summary of Issues Addressed

| # | Issue | Priority | Effort | Status |
|---|-------|----------|--------|--------|
| 1 | Single-seed, single-run analysis | Critical | Medium | **Merged (#16)** |
| 2 | No warm-up / initialisation bias analysis | High | Low | **Merged (#20)** |
| 3 | No sensitivity analysis | High | Medium | **Merged (#30)** |
| 4 | Team-block resource seizure (not individual) | High | High | Open |
| 5 | Flat DOW rate independent of wait time | High | Medium | **Merged (PR #53)** |
| 6 | Unidirectional transport (no dead-heading) | Medium | Low | **Merged (PR #56)** |
| 7 | Undifferentiated DNBI treatment pathway | Medium | Medium | **Merged (PR #34)** |
| 8 | OT surgical team not seized at R2E | Medium | Low | **Merged** |
| 9 | No MASCAL stochastic injection | Medium | Medium | Open |
| 10 | No comparative scenario (Okinawa/Vietnam rates) | Lower | Low | Open |
| 14 | Shiny app — parameter editor, Quick Run mode | Medium | Medium | Open |
| 15 | Shiny app — Full Analysis mode (multi-run CI) | Medium | Medium | Open |
| 18 | Endogenous casualty generation (force feedback) | Medium | High | Open |
| 19 | Dev Container — reproducible Linux R environment | Low | Low | **Merged (#21)** |
| 22 | Output Variable Register — KPI definition | High | Low | **Merged (#26)** |
| 23 | Strategic evacuation demand — Role 4 / AME sorties | Medium | Medium | Open |
| 24 | Variance reduction — antithetic variates / L'Ecuyer | Medium | Low | **Merged (#32)** |
| 35 | R2B OT bypass check — `<=` rather than `<` allows queuing | High | Low | **Merged (PR #36)** |
| 37 | OT bed incorrectly scheduled — rooms must be 24h | High | Low | **Merged (PR #38)** |
| 39 | R2B holding bed saturation — DNBI disease exhausts hold capacity | High | Medium | **Merged (PR #48)** |
| 40 | R2B OT suboptimal utilisation — 12h shift window limits forward surgery | Medium | Medium | Open |
| 43 | OT–ICU gating absent — surgery proceeds regardless of ICU availability | Medium | Medium | **Merged (PR #59)** |
| 44 | RTD KPI implicitly includes battle fatigue RTDs without annotation | Low | Low | **Merged (#47)** |

---

## Issues In Review (PRs Open — Awaiting Owner Merge)

*No PRs currently open against main.*

---

## Recently Merged Issues

### Issue 43 — OT–ICU Gating: Pre-OT ICU Availability Check ✓

**Merged:** PR #59, branch `claude/next-issue-ihn98l`

Replaces unconditional R2E OT entry with a pre-OT ICU availability gate, so surgery is no longer scheduled independently of post-operative bed availability. `r2e_treat_wia()` (`R/trajectories.R`) now branches into three paths at the point of OT seizure: **ICU available** — unchanged behaviour (surgery, then ICU recovery, short or full per prior R2B surgery + recovery probability); **ICU full + Priority 1** — surgery still proceeds (withholding it would expose an unsurgicated Priority 1 casualty to near-certain DOW), but post-operative recovery moves to a holding bed instead of ICU, with `dow_ceiling` multiplied by a post-op hold penalty (`r2e_postop_hold_penalty = 3.0`) reflecting reduced monitoring; **ICU full + Priority 2+** — OT entry is deferred via a `timeout` + `rollback(target = 1, check = ...)` poll loop (`icu_gating.defer_check_interval`, default 30 min), holding no resource while waiting, until an ICU bed frees. Both the ICU and post-op-hold recovery paths converge on a new shared post-operative DOW check (`dow_echelon = 4`), reusing the Issue #5 time-dependent conditional-increment survival function so the two pathways' realised mortality is directly comparable rather than one silently having no mortality consequence. The same gate is mirrored at R2B for structural consistency; it is expected to be inert under baseline load since R2B ICU utilisation is effectively zero and R2B surgery does not seize ICU beds post-op.

New `env_data.json` parameters: `r2eheavy.icu_gating` (`p1_bypass_priority_max`, `defer_check_interval`), `r2eheavy.post_op_hold` (holding-bed LOS distribution), `r2b.icu_gating` (`defer_check_interval`), `dow.treatment_efficacy.r2e_postop_hold_penalty`. `R/analysis.R` gained a `post_op_pathway_summary` KPI (icu vs. hold: total / died / postop_dow_rate, written to `outputs/post_op_pathway_summary.csv`), a `surgery_deferred_count`, and a new diagnostic plot (`images/r2e_icu_gating_impact.png`, `outputs/r2e_icu_gating_daily.csv`) showing, by simulation day, which casualties received sub-optimal (hold-bed override) or delayed (OT-entry deferral) care as a direct consequence of ICU saturation.

**Verification:** a saturated-ICU stress test (R2E `icu_bed` forced to an empty resource vector, 90-day run) confirmed zero ICU seizures, all hold-pathway patients Priority 1, and P2+ candidates correctly accumulating in the defer queue — matching the issue's acceptance criteria. A follow-up 50-replication comparison (seed = NULL, 30 days, pre- vs. post-merge) validated the effect generalises beyond seed 42: mean R2E ICU utilisation fell from 74.1% to 60.2%; mean DOW/run rose from 0.84 (95% CI [0.58, 1.10]) to 1.00 (95% CI [0.74, 1.26]) — the CIs overlap, so this specific before/after comparison is not statistically significant at n = 50 (DOW remains a rare event), but the entire point-estimate shift is attributable to the new post-operative checkpoint alone (+0.10/run, 5 of 50 replications). Within that checkpoint, using the real (non-stress-tested) 3.0× penalty, the post-op hold pathway's realised DOW rate (2/1,223 = 0.16%) was ≈2.8× the ICU pathway's (3/5,085 = 0.06%), confirming the intended design effect is measurable at baseline casualty rates.

A pre-existing, unrelated bug was discovered during testing and raised separately as Issue #60 (not fixed in this PR): setting a bed/resource `qty: 0` in `env_data.json` does not actually remove that resource, due to an R `paste0()` zero-length-argument recycling quirk in `build_environment()`. Does not affect Issue #43's logic (which is keyed off live `get_capacity()`/`get_server_count()`, not `qty`) or any currently-shipped baseline.

**Seed-42 baseline (30 days, single run — post-Issue-43):**

| Metric | Pre-#43 | Post-#43 |
|---|---|---|
| Total casualties | 400 ✓ | 400 ✓ |
| R2B surgical candidates | 122 | **132** |
| R2B surgeries | 42 | **53** |
| R2E surgeries — first op | 122 | **134** |
| R2E ICU utilisation (mean) | ICU1 80.6%, ICU2 73.6%, ICU3 64.8%, ICU4 56.9% | **ICU1 75.8%, ICU2 62.6%, ICU3 59.0%, ICU4 49.6%** |
| R2E ICU queue ≥1 | ICU1 45.9%, ICU2 31.8% of run | **ICU1 27.2%, ICU2 6.7%, ICU3 6.1% of run** |
| `post_op_pathway` (Issue #43) | — | **icu=110, hold=23** |
| `surgery_deferred` (Issue #43) | — | **10** |

Shifts in R2B/R2E surgery counts reflect the new `runif()` draws consumed at each OT-entry decision shifting the shared RNG stream from that point onward — the same pattern documented for Issues #5/#6 — not a causal effect of the gating logic itself. The ICU utilisation and queue reductions, and the pathway/deferral counts, are the direct, causal, mechanistic effects of the new gate.

**Unblocked by this merge:** No new issues unblocked — #4, #9, #10, #14, #18, #40 were already `status: ready` before this merge; none list #43 as a dependency.

---

### Issue 6 — Dead-Heading Return Legs for Transport Assets ✓

**Merged:** PR #56, branch `claude/action-plan-review-73a25i`

Replaces the outbound-only transport model with a full round trip. `r1_transport_wia()`, `r1_transport_kia()`, and `r2b_transport_wia()` (`R/trajectories.R`) now clone the entity after the outbound timeout into a vehicle branch (return-leg timeout, then release — listed first in `clone()` so it inherits the pre-clone seize record) and a casualty branch (no further activity); `synchronize(wait = FALSE)` lets the casualty continue immediately while the vehicle clone completes its return leg independently and is discarded when it later arrives at the same point. Return leg duration is a fresh triangular draw from the same outbound distribution, scaled by a configurable `return_leg_multiplier` (`env_data.json`, default **1.0** — a symmetric round trip, since tactical rate-of-march planning for these vehicle classes does not doctrinally differentiate laden/unladen travel time; the issue's original 0.8 assumption was corrected during implementation). Mortuary transfers at R2B/R2E use collocated evacuation personnel, not pooled vehicles, and are unaffected.

**Sensitivity screen extended:** `return_leg_multiplier` added to `morris_params` (bounds 0.7–1.3). `R/sensitivity.R::extract_kpis()` gained `transport_q` (mean queue) and `transport_util` (mean utilisation) KPIs, since the existing OT/ICU queue KPIs are downstream of transport and, at the current casualty rate, transport itself only shows up as utilisation (queue stays at 0) — a queue-only KPI would have under-detected the parameter's effect. `run_sobol()` was hardened against degenerate-variance responses (`transport_q` is exactly 0 whenever none of the screened parameters affect transport occupancy, which previously crashed `boot::boot.ci()` inside `tell.sobol2007()`); each KPI's `tell()` is now independently wrapped, and `save_sobol()` checks for partial results before writing, so one degenerate KPI no longer discards the other four.

**Capacity margin plot added:** `analyse_run()` now produces `images/transport_capacity_margin.png` (queue-over-time per PMV Ambulance/HX240M unit) and `outputs/transport_utilisation.csv`. `plot_transport_capacity_margin_by_fleet_size()` is included as a documented stub (roxygen-specified interface and algorithm, raises an explicit "not yet implemented" error) for a future fleet-size sweep — blocked on Issue #10 (comparative scenario runner). A follow-up issue for that sweep was drafted during the PR but could not be created due to a GitHub connector disconnection in that session; the draft is available and should be raised as a new `[Ph.4]` issue, sequenced after #10, before that work begins.

Also fixes a latent bug in `R/analysis.R::analyse_run()`: it previously crashed on any run with zero DOW events, since `pivot_wider()` only creates a column for an attribute key when at least one casualty has it set. Added a defensive guard for the `dow`/`dow_echelon` columns.

**Seed-42 baseline (30 days, single run — post-Issue-6):** Modelling the return leg consumes an additional random draw per outbound leg, shifting the seed-42 RNG stream from that point onward. Total casualty count and KIA count are unaffected; most other seed-42 figures shift:

| Metric | Pre-#6 | Post-#6 |
|---|---|---|
| Total casualties | 400 ✓ | 400 ✓ |
| KIA | 70 ✓ | 70 ✓ |
| DOW count (seed 42) | 4 | **0** (single-run stochastic outcome; mean ~0.70/run across replications unaffected) |
| DNBI sub-types | battle_fatigue=46, disease=97, nbi=33 | **battle_fatigue=33, disease=118, nbi=25** |
| total_rtd | 148 | **136** (bf_rtd 31, clinical_rtd 105: r1 55, r2b 43, r2e 7) |
| R2B surgical candidates | 170 | **149** |
| R2B surgeries | 41 | **42** |
| R2E surgeries — first op | 126 | **122** |
| R2E ICU utilisation (mean) | ICU1 88.8%, ICU2 77.9%, ICU3 73.1%, ICU4 65.0% | **ICU1 80.6%, ICU2 73.6%, ICU3 64.8%, ICU4 56.9%** |
| PMV Ambulance utilisation | — (not tracked pre-#6) | **11.1%** (max queue 0 throughout) |
| HX240M utilisation | — (not tracked pre-#6) | **4.9%** (max queue 0 throughout) |

Under the current Falklands-derived casualty rate, the 3-vehicle PMV Ambulance / 2-vehicle HX240M pools have enough spare capacity that dead-heading does not produce a persistent queue — the effect is visible only in utilisation (busy-time roughly doubling relative to the outbound-only model, confirmed by direct comparison against a stashed pre-#6 baseline). A targeted Sobol run varying `return_leg_multiplier`, `r1_transport`, and `r2b_transport` confirmed transport queue is near-constant-zero (variance ≈ 1.5–2.4×10⁻¹⁰) across the full plausible parameter range, while transport utilisation showed genuine, non-degenerate variance (≈0.002–0.005) and a structurally valid Sobol decomposition when the underlying bootstrap succeeded — directly confirming the new KPIs are correctly computed, not just structurally present.

**Unblocked by this merge:** No new issues unblocked — #4, #9, #10, #14, #18, #40, #43 were already `status: ready` before this merge, gated on earlier issues. A new follow-up issue (#57, fleet-size capacity margin sweep, Phase 4, blocked on #10) was raised — see above.

---

### Issue 5 — Time-Dependent DOW Survival Function (Falklands Calibration) ✓

**Merged:** PR #53, branch `claude/action-plan-review-rj2ilu`

Replaces the flat, time-independent DOW probability with a logistic survival function `F(t) = p_base + (p_max − p_base) / (1 + exp(−k × (t − t_mid)))` evaluated at each care-transition checkpoint (R1 exit, R2B arrival, R2E arrival). DOW probability is applied as a conditional increment to avoid double-counting across echelons: `p_cond = max(0, (F(t_now) − F(t_prev)) / (1 − F(t_prev)))`. Each casualty carries a `dow_ceiling` attribute initialised to its priority's `p_max` and multiplied by treatment efficacy factors after each care phase (TCCC×0.83, R2B DCR×0.56, R2B DCS×0.32, R2E DCR×0.56, R2E DCS 1st op×0.25, R2E DCS 2nd op×0.57).

**Calibration:** P1 p_max = 0.023, k = 0.04, t_mid = 120 min; P2 p_max = 0.019, k = 0.025, t_mid = 180 min. Calibrated against the Falklands 1982 historical rate of 3 DOW / 580 WIA ≈ 0.52% (Payne 1983). A 50-replication validation run (seed = NULL) produced mean 0.70 DOW/run (0.45% of WIA), 95% CI [0.41, 0.95] — encompassing the 0.52% target. Seed-42 single run: 4 DOW. The values `p_max` and the OIF/OEF-era efficacy factors are entangled; substituting Falklands-era efficacy values (Issue #54) requires re-calibrating `p_max` upward.

**DOW model design finding (Test 2):** DOW checks fire only at care-transition boundaries, not during intra-echelon queue waits. Zeroing R2E OT capacity reduces DOW to 0 in the seed-42 run while OT queue peaks at 62 — confirming the model is sensitive to evacuation delays but not to intra-echelon surgical queue delays. Documented as a known limitation in the README.

**README additions:** DOW Survival Function section updated with logistic parameter table, cumulative ceiling calculation (0.023 × 0.83 × 0.56 × 0.32 × 0.25 = 0.085%), MODEL ASSUMPTION block with Falklands calibration basis and p_max/efficacy co-dependence note, embedded survival function figure (`images/dow_survival_function.png`), and References [42] (Payne 1983, PMC) and [43] (Jolly 2018, JMVH).

**Seed-42 baseline (30 days, single run — post-calibration):**

| Metric | Pre-#5 | Post-#5 |
|---|---|---|
| DOW count | 0 (flat placeholder active) | **4** (seed 42; mean ~0.70/run across replications) |
| DOW rate — P1 p_max | — | **2.3%** ceiling (Falklands 1982 calibration) |
| DOW rate — P2 p_max | — | **1.9%** ceiling (Falklands 1982 calibration) |
| DOW rate — P3 flat | 0.1% placeholder | **0.1%** (structural placeholder; P3 never evacuated) |
| Mean DOW/run (50-rep) | — | **~0.70** (0.45% of WIA); 95% CI [0.41, 0.95] |
| Total casualties | 400 ✓ | 400 ✓ |
| Morris ranking — p1_p_max | Not present (flat DOW used `pri1_dow`) | **Rank 6**, µ* = 0.0081 |

**Unblocked by this merge:** Issue #43 (OT–ICU gating; now unblocked after #5), Issue #9 (MASCAL injection; requires #1 + #2 + #5 — all now merged), Issue #18 (force regeneration feedback; requires #1 + #2 + #5 — all now merged), Issue #10 (scenario runner; requires #1 + #2 + #5 + #8 — all now merged).

---

### Issue 39 — R2B Hold Bed Saturation: Two-Tier Routing Policy ✓

**Merged:** PR #48, branch `claude/action-plan-review-7rde0w`

Implements per-stream hold bed occupancy analysis and a two-tier capacity-aware routing policy to address structural R2B hold saturation driven by disease DNBI evacuees. Stream decomposition via `r2b_hold_start` and `dnbi_type` attributes confirms disease DNBI as the dominant load (~72 of 91 hold patients over 30 days). Expected concurrent hold occupancy (~15.5 beds against 10-bed capacity across both R2B units) is a structural mismatch not resolvable by hold duration reduction alone.

**Two-tier routing policy (Issue #39, implemented):**

- **Primary tier — upstream threshold routing (`hold_threshold = 0.8`).** `select_r2b_for_hold()` in `R/trajectories.R` checks whether any R2B unit's hold occupancy is strictly below `hold_threshold × capacity` (≥4 of 5 beds occupied triggers reroute). If no unit is below threshold, the patient is routed to R2E at R1 before transport (`r2b_bypassed = 1`), eliminating transport to an already-saturated R2B.
- **Secondary tier — at-R2B three-stage branch.** On arrival at R2B, a three-way branch: (1) hold capacity available → seize hold bed; (2) R2B hold full but R2E has capacity → at-R2B bypass to R2E (`r2b_hold_bypass = 1`); (3) both echelons full → queue at R2B hold, capped at floor(5/10 × 5) = 2 patients (`r2b_hold_queued = 1`).

**`R/analysis.R` additions:** `r2b_hold_daily` (daily concurrent occupancy by stream: Disease DNBI / NBI / WIA), `r2b_hold_occupancy_plot` (ggplot object, saved to `images/r2b_hold_occupancy.png`), `r2b_pre_bypass_count`, `r2b_hold_bypass_count`, `r2b_hold_queued_count`. All three routing scalars included in the `analyse_run()` return list. `waiting_time.png` added as a second new plot (casualty waiting time scatter over simulation time).

**Seed-42 baseline (30 days, single run — post-implementation):**

| Metric | Pre-#39 | Post-#39 |
|---|---|---|
| Total casualties | 400 | 400 ✓ |
| Pre-bypass at R1 (threshold, `r2b_bypassed = 1`) | 0 | **112** |
| At-R2B bypass (hold full, `r2b_hold_bypass = 1`) | 0 | **0** |
| Queued at R2B (both echelons full, `r2b_hold_queued = 1`) | 0 | **0** |
| R2B hold max queue (resource monitor) | 4 | **2** |
| R2B hold queue events (queue > 0, resource monitor) | 148 | **28** (−81%) |
| R2B treated total | 189 | **172** |

The 112 upstream pre-bypasses at R1 prevent hold saturation in the downstream tier. As a result, no patients trigger the at-R2B bypass or queue paths: both are zero. Residual queue events (28 in the resource monitor) represent transient race conditions resolved within one event step; max concurrent queue is 2 (within cap). Analysis plots regenerated: all 10 PNGs in `images/` updated and committed, including two new plots (`r2b_hold_occupancy.png`, `waiting_time.png`). README updated with Hold Bed Saturation diagnostic section, four intervention scenarios (A–D), and updated Simulation Analysis narrative identifying R2B hold saturation as the primary near-echelon constraint.

**Significance:** Disease DNBI evacuees occupy hold beds for multi-day durations (mode 5 days), generating a structural 55% overload. The two-tier routing policy eliminates routine queuing and keeps the R2B hold pathway functional throughout a 30-day operation, at the cost of transferring non-surgical medical load to R2E. Hold bed expansion (≥8 per unit) or an evacuation threshold policy remain indicated structural remedies. **Impact rises to High once Issue #5 (time-dependent DOW) merges**, as routing policy will then directly affect modelled mortality.

**Unblocked by this merge:** No new issues unblocked — all Phase 2 and remaining Phase 3 issues were already `status: ready` before this merge.

---

### Issue 44 — RTD KPI Decomposed into Battle Fatigue vs Clinical Sub-totals ✓

**Merged:** PR #47, branch `feature/issue-44-rtd-kpi-annotation`

Replaces the single `total_rtd` count with two operationally distinct sub-totals. `bf_rtd` counts battle fatigue casualties (dnbi_type == 1) assigned `return_day` at R1 without clinical treatment; `clinical_rtd` counts all other RTDs (WIA/NBI/disease recovery at R1, R2B hold-bed discharge, R2E hold-bed discharge). `total_rtd = bf_rtd + clinical_rtd`, enforced by `stopifnot()`. `rtd_by_echelon` gains a `rtd_type` column (`"battle_fatigue"` / `"clinical"`), grouping the CSV output by `(return_echelon, rtd_type)`. All three scalars added to the `analyse_run()` return list. README updated with accurate Return to Duty design section and a Simulation Analysis subsection with the seed-42 results table. CLAUDE.md baseline table updated with confirmed values.

**Seed-42 baseline (30 days, single run):**

| Echelon | RTD type | Count | Rate (of 400) |
|---|---|---|---|
| R1 | battle_fatigue | 38 | 9.5% |
| R1 | clinical | 59 | 14.8% |
| R2B | clinical | 46 | 11.5% |
| R2E | clinical | 5 | 1.3% |
| **Total** | | **148** | **37.0%** |

`bf_rtd` = 38, not 46, because 8 of 46 battle fatigue casualties were still within their R1 hold timeout when the 30-day simulation ended — `return_day` is only assigned on timeout completion. Confirmed across a 10-replication run with both `stopifnot()` guards passing in all replications.

**Significance:** The decomposition preserves the operational distinction between forward behavioural health management (R1 battle fatigue hold) and clinical treatment throughput at each Role 2 echelon. The combined total previously obscured both. The 37.0% aggregate RTD rate is within the historical in-theatre range of 7.6–42.1% [[9]](#References).

**Unblocked by this merge:** No new issues — all Phase 2 issues were already unblocked before #44.

---

### Issue 37 — OT Bed Schedule Removed ✓

**Merged:** PR #38, branch `feature/issue-37-ot-bed-schedule-fix`

Removes the 12-hour shift schedule from all OT bed resources (`b_r2b_ot_*`, `b_r2eheavy_ot_*`). OT rooms are physical spaces available 24 hours per day; only the surgical team carries a shift schedule. The bug caused OT bed capacity to toggle to zero when the team was off-shift, blocking OT access for on-shift periods adjacent to a shift handover. The R2B bypass logic was updated with a third condition: `get_capacity(surg_team) > 0` — explicitly bypassing off-shift patients regardless of bed availability. Removed unused `r2b_ot_bed_counter` and `r2e_ot_bed_counter` variables from `R/environment.R`. README, CLAUDE.md, and all 8 analysis plots regenerated from the corrected post-rebase simulation run.

**Seed-42 baseline (30 days, post-implementation — post-rebase onto main including Issues #7, #24):**

| Metric | Post-#35 (pre-rebase) | Post-#37 (rebased) |
|---|---|---|
| R2B surgical candidates | ~160 | 170 |
| R2B bypass events | 74 | 114 of 170 |
| R2B surgeries | 29 | 41 |
| R2E first surgeries | 102 | 126 |
| R2B OT util (24h room) | 5.4% | OT1: 5.4%, OT2: 8.5% |
| R2B OT util (shift time) | 10.8% | OT1: 10.8%, OT2: 17.0% |
| R2E OT util (24h room) | 30.8% mean | OT1: 46.9%, OT2: 23.5% |
| R2E ICU util | 68.4% mean | ICU1: 88.8%, ICU2: 77.9%, ICU3: 73.1%, ICU4: 65.0% |
| R2E ICU queue ≥1 | 38% of run | ICU1: 59% of run, ICU2: 46% of run |
| Total casualties | 400 | 400 ✓ |

The R2E ICU is the binding R2E constraint post-fix: chronic queuing for ICU1 and ICU2 throughout the run. R2E OT queues are intermittent and brief. R2B holding beds remain the R2B binding constraint (unchanged from #35 baseline). Difference from pre-rebase figures reflects integration of Issue #7 DNBI sub-categorisation (reducing surgical candidacy from ~160 to 170, with more routing to holding).

**Significance:** OT rooms now correctly reflect 24h physical availability. The bypass check ensures patients cannot seize a bed off-shift and wait. R2E ICU emerges as the primary system-level constraint, visible in queue data across the full 30-day run.

**Unblocked by this merge:** Issue #40 (R2B OT utilisation analysis) required Issue #37 merged for a correct bypass baseline — now ready.

---

### Issue 35 — R2B OT Bypass Check Bug ✓

**Merged:** PR #36, branch `feature/issue-35-r2b-ot-bypass-fix`

Fixes `usage <= cap` → `usage < cap && queue == 0` in the R2B OT availability check. Adds `!is.na(queue)` guard (`get_queue_count()` can return NA at simulation startup; without the guard `if(NA)` throws a runtime error). Sets `r2b_bypassed = 1` attribute on bypass patients for downstream tracking. Migrates the legacy `Battlefield Casualty Handling.R` from the removed `nbi` attribute to `dnbi_type`, aligning it with `R/trajectories.R` after PR #34 removed `env_data$vars$r1$other$nbi`.

**Seed-42 baseline (30 days, post-implementation — post-rebase onto PR #34):**

| Metric | Pre-fix | Post-fix |
|---|---|---|
| R2B bypass events | 1 | 130 of 160 surgical candidates |
| R2B surgeries | ~160 (with queuing) | 30 |
| R2E first surgeries | ~30 | 122 |
| R2B OT utilisation | — | 5.5% |
| R2E OT utilisation (mean) | — | 34.0% (OT1: 50.0%, OT2: 17.9%) |
| R2E ICU utilisation (mean) | — | 68.4% (ICU1: 78.0%, ICU2: 73.6%, ICU3: 63.7%, ICU4: 58.1%) |
| R2E ICU queue ≥1 | — | 11.5 cumulative days (38% of run) |

**10-replication confirmation (seed 42, 30 days per run):**

| Resource | 10-rep mean | Range |
|---|---|---|
| R2E ICU utilisation | 71.2% | 60.6–80.6% |
| R2E OT utilisation | 33.4% | 29.7–36.9% |

ICU exceeded OT utilisation in all 10 replications, confirming ICU as the primary R2E binding constraint — not OT.

**Significance:** The bypass fix routes 130 of 160 R2B surgical candidates directly to R2E, producing a 4× increase in R2E first surgeries (30 → 122) and materially higher ICU load. The ICU constraint identification is robust across replications.

**Known issues raised from this PR:**
- Issue #43 (OT–ICU gating absent): surgery proceeds regardless of ICU availability — three-way branch required
- Issue #44 (RTD KPI annotation): `R/analysis.R:488` implicitly includes battle fatigue RTDs without inline note

**Unblocked by this merge:** Issue #40 (R2B OT utilisation analysis) required #35 merged for correct bypass baseline — now ready, pending #37.

---

### Issue 7 — DNBI Sub-Category Routing ✓

**Merged:** PR #34, branch `feature/issue-7-dnbi-subcategory`

Implements three-way DNBI routing, replacing the binary NBI/other split. Battle fatigue (25%) returns to duty at R1 with no R2 routing. Disease (58%) routes to R2B holding only — no surgery. NBI (17%) follows the full WIA-equivalent pathway including DOW branch and surgical candidacy.

**Key changes:** `env_data.json` replaces `nbi: 0.17` with three proportions (`battle_fatigue_pct`, `disease_pct`, `nbi_pct`). `R/trajectories.R` replaces `nbi` attribute with `dnbi_type` (1/2/3); `surgery` forced to 0 for types 1 and 2. Two MODEL ASSUMPTION blocks added to README. Limitation L4 marked resolved. Three bugs corrected in the same PR: disease DNBI exempted from R2B DOW check; disease routing changed to `select_r2b_for_hold()` (hold bed availability, not OT); Phase 4 second surgery guarded by `r2e_surgery == 1`. Paywalled Reference [36] (Amoroso & Bell 2008) replaced with a derived estimate from open-access sources [8] and [35].

**Seed-42 baseline (30 days, post-implementation):**

| Sub-type | Count |
|---|---|
| Battle fatigue | 46 |
| Disease | 97 |
| NBI | 33 |
| Total DNBI | 176 |

**Significance:** Approximately 83% of DNBI casualties are removed from the surgical candidacy pathway. R2B OT demand now reflects combat trauma only, producing a materially more accurate representation of the WIA surgical bottleneck. A follow-up Morris screening to include `disease_surgery_pct` in the parameter set is tracked as a Further Development item in the README.

**Unblocked by this merge:** Issue #39 (R2B hold bed saturation) required `dnbi_type` stream decomposition — now ready.

---

### Issue 35 — R2B OT Bypass Check Bug (PR #36)

**Branch:** `feature/issue-35-r2b-ot-bypass-fix`

Fixes `usage <= cap` → `usage < cap && queue == 0` in the R2B OT availability check. Previously, when all OT beds were at capacity, the condition evaluated to TRUE and patients queued for R2B OT rather than bypassing immediately to R2E. Added `get_queue_count()` check so any queue triggers bypass. Sets `r2b_bypassed = 1` attribute on bypass patients.

**Before/after (seed 42, 30 days):**

| Metric | Pre-fix | Post-fix |
|---|---|---|
| R2B bypass events | 1 | 74 of 103 surgical candidates |
| R2B surgeries | ~100+ | 29 |
| R2E first surgeries | ~30 | 102 |
| R2B OT utilisation | 6.2% | 5.4% |
| R2E OT utilisation (mean) | 10.8% | 30.8% |

---

### Issue 37 — OT Bed Incorrectly Scheduled ✓

**Merged:** PR #38 — see "Recently Merged Issues" section above for full detail.

---

## Completed Issues

### Issue 19 — Dev Container Specification ✓

**Merged:** PR #21, branch `claude/issue-19-ywhdei`

`.devcontainer/Dockerfile` (based on `rocker/rstudio:4.4.2`) and `.devcontainer/devcontainer.json` added to the repository root. The container installs all project R packages, sets `mc.cores` to the physical core count via `Rprofile.site`, and exposes RStudio Server on port 8787 with authentication disabled. VS Code users open the repository, click "Reopen in Container", and access RStudio Server at `http://localhost:8787` after a one-time image build of approximately 5–10 minutes.

**Significance:** Contributors on Windows now run `mclapply` under Linux `fork()`, reducing Morris sensitivity screening time from an estimated 10–15 hours to 1–2 hours on an 8-core machine. The environment specification makes R version and package state part of the repository, supporting academic reproducibility.

---

### Issue 1 — Multi-Run Replication Framework ✓

**Merged:** PR #16, branch `feature/issue-1-multi-run-replication`

`R/replication.R` implements `run_once()`, `run_replications()` (via `mclapply` on POSIX, `lapply` fallback on Windows), and `summarise_replications()` returning mean, p10, p90, max, and 95% CI across replications. `run.R` branches on `--iterations`: single-run path sinks logs and writes diagnostics; multi-run path writes `outputs/replication_summary.csv`. `generate_ln_arrivals()` gained a `write_file` parameter to prevent parallel file-write conflicts.

**Significance:** All KPI outputs are now distributional rather than point estimates. The seed-42 single-run baseline (401 casualties) is confirmed as representative within the multi-replication distribution.

---

### Issue 8 — R2E Surgical Team Seizure Bug ✓

**Merged:** closed 2026-06-13

The `seize_resources(surg_team)` and `release_resources(surg_team)` calls were commented out in `r2e_treat_wia()`, allowing unlimited parallel R2E OT cases against the same surgical team. Uncommenting both calls for both surgery branches corrects R2E OT utilisation and queue figures. R2E OT queue is now non-zero under concurrent surgical demand.

**Significance:** All R2E OT results prior to this fix were invalid (unlimited surgical parallelism). The correction is a prerequisite for trustworthy Issue #10 (scenario runner) output.

---

### Issue 22 — Output Variable Register ✓

**Merged:** PR #26, branch `feature/issue-22-output-variable-register`

Five new `set_attribute()` calls added to `R/trajectories.R`: `dow_echelon`, `r2e_arrival_time`, `r2b_departure_time`, `r2e_departure_time`, `return_echelon`. Seven derived KPI computations added to `R/analysis.R`: time to first surgery, R2B and R2E dwell times, R2B→R2E transit time, DOW and RTD count by echelon, OT utilisation per echelon. Three new CSVs written: `dow_by_echelon.csv`, `rtd_by_echelon.csv`, `ot_utilisation.csv`. README updated with Model Outputs section and Limitations section (L1–L11).

**Significance:** Provides the output variable inventory required before Morris sensitivity screening (Issue #3) can be correctly targeted. Echelon-level DOW and RTD breakdowns are available for the first time.

---

### Issue 2 — Warm-Up / Initialisation Bias Analysis ✓

**Merged:** PR #20, branch `claude/pr-dependency-issues-2-3-vsqzu9`

`R/warmup.R` implements `bin_icu_queue()`, `compute_welch_cma()`, `plot_welch()`, and `run_welch_analysis()`. `scripts/run_warmup.R` provides the CLI entry point. `summarise_replications()` and `analyse_run()` both accept a `warm_up_days` parameter; `--warm-up` CLI flag threads through `run.R`.

**Key finding — terminating simulation classification:** The full Welch analysis (10 × 90-day replications) revealed that the CMA of the R2E ICU queue does not converge. The CMA rises to a local peak near Day 10, dips near Day 16, recovers to a plateau across Days 20–55, then rises to a higher peak near Day 65 before gradually declining — no stable plateau is reached within 90 days. This non-convergent pattern confirms the simulation is a **terminating simulation** per Law (2020): the campaign has a finite horizon and the initial build-up of casualties from Day 1 is operationally meaningful, not an artefact to be discarded. Welch's replication-deletion approach presupposes a steady state; it is not applicable to this model.

**Outcome:** `WARM_UP_DAYS = 0L` (no exclusion by default). The `--warm-up N` flag is retained for optional use in parametric comparison runs requiring a common time base. The Welch plot (`images/welch_plot_icu_queue.png`) documents the episodic non-stationary CMA behaviour.

**Unblocked by this merge:** Issue #7 (DNBI sub-categorisation, needs #1 + #2) is now ready. Issue #3 (Morris sensitivity, needs #1 only) and Issue #14 (Shiny parameter editor, needs #1 only) were already unblocked at Issue #1 merge and have been updated to `status: ready`.

---

### Issue 3 — Morris Sensitivity Screening ✓

**Merged:** PR #30, branch `claude/action-plan-next-issue-wxcm7p`

`R/sensitivity.R` implements `morris_params` (9-parameter data frame), `apply_params()` (overrides 8 `env_data$vars` paths), `eval_params()` (extracts `ot_hours` and passes separately to `run_replications()`), `extract_kpis()` (returns 5 KPIs: `r2e_icu_q`, `r2b_ot_q`, `r2e_ot_q`, `system_ot_q`, `dow_count`), `run_morris()` (saves per-KPI µ*/σ plots to `images/`, ranked CSV to `outputs/morris_ranking.csv`), and `run_sobol()` (Sobol2007 on the five screened parameters, shared design matrix for all three OT KPIs, writes `outputs/sobol_<kpi>.csv`). `scripts/run_sensitivity.R` provides the CLI entry point with `--r`, `--reps`, `--days`, `--levels`, `--quick`, `--sobol`, `--n-sobol`, and `--seed` flags. `R/environment.R` gained an `ot_hours` parameter in `build_env()` (default 12 → 720 min, backward compatible). `R/replication.R` threads `ot_hours` through `run_once()` and `run_replications()`.

**Key findings — Morris EE screening (r=20, reps=5, days=30):**

Top 5 parameters by µ* for system OT queue:

| Rank | Parameter | µ* | σ |
|------|-----------|-----|---|
| 1 | `ot_hours` (OT shift availability) | 0.978 | 0.412 |
| 2 | `pri1_surg_prob` (Priority 1 surgery probability) | 0.657 | 0.289 |
| 3 | `long_resus_mode` (Long resuscitation duration) | 0.577 | 0.241 |
| 4 | `surg_mode` (Surgery duration mode) | 0.542 | 0.318 |
| 5 | `pri1_dow` (DOW rate at R1) | 0.432 | 0.198 |

`in_theatre_rate`, `r1_transport`, `r2b_transport`, and `long_icu_mode` ranked 6–9 with µ* < 0.3.

**Key findings — Sobol variance decomposition (n=200, p=5):**

For system OT queue: `ot_hours` dominates first-order variance (S1 ≈ 0.488). `pri1_dow` and `surg_mode` show high total-order indices (ST ≫ S1), indicating strong interaction effects with other parameters. `pri1_surg_prob` and `long_resus_mode` contribute moderate first-order and interaction variance.

**Significance:** OT shift availability (`ot_hours`) is the dominant controllable lever for surgical throughput — more influential than surgery duration itself. Extending OT availability from 12 to 16 hours has a larger expected effect on R2E/R2B OT queue than reducing mean surgery time by 20%. The high interaction effects of `pri1_dow` and `surg_mode` indicate these parameters do not act independently; their influence is conditional on the load presented to surgical resources.

**Unblocked by this merge:** Issue #4 (individual resource seizure) required Issues #1, #2, and #3 all stable — all three are now merged. Issue #4 may be updated to `status: ready`.

---

### Issue 24 — Variance Reduction — Antithetic Variates and L'Ecuyer-CMRG RNG Streams ✓

**Merged:** PR #32, branch `feature/issue-24-variance-reduction`

Three changes shipped together:

1. **L'Ecuyer-CMRG parallel RNG streams.** `RNGkind("L'Ecuyer-CMRG")` set before `mclapply` in `run_replications()`, with `mc.set.seed = TRUE`. Each worker is assigned a provably non-overlapping MRG32k3a substream (period 2⁷⁶ per substream; overall period ρ ≈ 2¹⁹¹). This eliminates the correlated-replication risk present in the previous `mc.set.seed = FALSE` approach, where forked workers inherited the same parent RNG state.

2. **Antithetic variate variance reduction.** `generate_ln_arrivals()` replaces `rlnorm()` with `qlnorm(runif())` to enable exact reflection. Replication pairs (2k−1, 2k) share a seed: the primary draws U and the antithetic substitutes 1−U, reflecting the arrival schedule about the lognormal median. Both the per-minute rate draw and the within-minute jitter are antithetised. Empirical validation (100 pairs): Pearson r = −0.368 between paired rate sums; ~68% variance reduction relative to independent runs. Application is limited to arrival times (service times and routing probabilities generated inside simmer's C++ engine are not antithetised — documented as L8, Low Impact).

3. **mc.cores fix.** `mc.cores = parallel::detectCores()` replaces the implicit default of 2, using all available physical cores.

**Baseline update.** Replacing `rlnorm()` with `qlnorm(runif())` changes the number of RNG draws consumed under seed 42, shifting the 30-day total by one casualty (401 → 400, WIA 155 → 154, Priority 1 207 → 219). All README tables and CLAUDE.md key-parameter baseline updated.

**Significance:** Parallel RNG correctness is now guaranteed rather than assumed. The ~68% variance reduction on the arrival rate-sum precursor means tighter CI bounds are obtainable at the same replication count for any future large-scale runs (e.g., Issue #4 calibration, Issue #9 MASCAL injection). The mc.cores fix removes a silent throughput cap that limited parallelism to 2 cores on all hardware.

**Unblocked by this merge:** No new issues are directly unblocked — Issues #4, #5, #6, #7, and #14 were already `status: ready` after Issues #1, #2, and #3 merged. Issue #24 improves the statistical quality of all future multi-replication runs without adding new dependencies.

---

## Issue 1 — Single-Seed, Single-Run Analysis

### Problem

The published analysis rests entirely on seed 42. The `n_iterations <- 1000` constant is defined in the code but the multi-run framework was not activated for the reported results. A single stochastic realisation cannot support claims about system behaviour — queue dynamics, saturation timing, and bottleneck identification may all be artefacts of the chosen seed.

### Recommended Approach

Activate the existing `n_iterations` constant using `simmer`'s native replication pattern via `mclapply()` and `wrap()`. This is explicitly documented in the `simmer` JSS paper (Ucar et al., 2019) and requires no architectural change — only a wrapper function and results aggregation layer.

For each key performance indicator (KPI) — queue length by resource, wait time by echelon, throughput — report:
- Mean across replications
- 10th and 90th percentile (inter-replication range)
- Maximum observed value (worst-case exposure)
- 95% confidence interval via t-test on replication means

### Implementation

```r
library(parallel)

run_simulation <- function(i) {
  set.seed(NULL)  # Allow independent seeds per replication
  env <- simmer("BCH") %>%
    build_env(env_data) %>%
    # ... add generators ...
    run(until = n_days * day_min) %>%
    wrap()
  env
}

envs <- mclapply(1:n_iterations, run_simulation, mc.set.seed = FALSE)

# Aggregate KPIs
arrivals <- get_mon_arrivals(envs)
resources <- get_mon_resources(envs)

queue_summary <- resources %>%
  group_by(replication, resource) %>%
  summarise(mean_queue = mean(queue), max_queue = max(queue)) %>%
  group_by(resource) %>%
  summarise(
    mean = mean(mean_queue),
    p10  = quantile(mean_queue, 0.10),
    p90  = quantile(mean_queue, 0.90),
    max  = max(max_queue)
  )
```

### References

- Ucar I, Smeets B, Azcorra A (2019). "simmer: Discrete-Event Simulation for R." *Journal of Statistical Software*, 90(2), 1–30. [doi:10.18637/jss.v090.i02](https://doi.org/10.18637/jss.v090.i02) — see Section 4.2 for `mclapply()` / `wrap()` replication pattern.
- Romero-Brufau S et al. (2020). "Discrete-Event Simulation Modeling of Critical Care Flow." *Critical Care Explorations*, 2(9). [PMC7491890](https://pmc.ncbi.nlm.nih.gov/articles/PMC7491890/) — reports 30 trials with mean KPIs as the standard output format for DES healthcare studies.
- Tros M et al. (2025). "Discrete Event Simulation in R using the 'Simmer' Package for Health Economic Modelling." *Applied Health Economics and Health Policy*. [PMC12535931](https://pmc.ncbi.nlm.nih.gov/articles/PMC12535931/) — demonstrates probabilistic analysis with 95% CIs in a simmer-based healthcare DES.

---

## Issue 2 — Warm-Up / Initialisation Bias

### Problem

The simulation begins with empty queues across all resources. In a system under sustained load, this creates an initialisation transient — early-period metrics are artificially optimistic because resources are not yet loaded. For a 30-day run this bias may affect the first several days of reported data, which coincides with the period most examined in the single-run analysis.

### Recommended Approach

Apply Welch's graphical method to identify the warm-up period. Run ≥5 replications of an extended simulation (60–90 days), plot time-averaged KPIs (e.g., R2E ICU queue length) against simulation time, and identify the point at which the moving average stabilises. This period is then excluded from all subsequent analyses using the replication-deletion method.

A rule of thumb from Banks et al. (2005) is that productive run length should be at least 10× the deleted warm-up period. Given the system's relatively fast loading rate, a warm-up of 3–5 days (4,320–7,200 minutes) is the likely outcome, making the 30-day run length borderline. Consider extending production runs to 45 days minimum.

### Implementation

```r
# Run extended replications for Welch plot
welch_envs <- mclapply(1:10, function(i) {
  simmer("BCH_warmup") %>%
    build_env(env_data) %>%
    run(until = 90 * day_min) %>%
    wrap()
}, mc.set.seed = FALSE)

# Extract and average ICU queue length over time
icu_data <- get_mon_resources(welch_envs) %>%
  filter(grepl("icu", resource)) %>%
  group_by(time, replication) %>%
  summarise(queue = sum(queue)) %>%
  group_by(time) %>%
  summarise(mean_queue = mean(queue))

# Plot Welch-style cumulative moving average
icu_data %>%
  mutate(cma = cumsum(mean_queue) / row_number()) %>%
  ggplot(aes(x = time / 1440, y = cma)) +
  geom_line() +
  labs(x = "Day", y = "Cumulative Mean ICU Queue",
       title = "Welch Plot — R2E ICU Queue (Warm-Up Detection)")
```

Set `warm_up_period <- 5 * day_min` and filter all output data to `time > warm_up_period` in the analysis pipeline.

### References

- Rossetti MD. *Simulation Modeling and Arena*, Chapter 5.2–5.3: "Statistical Analysis Techniques for Warmup Detection." [https://rossetti.github.io/RossettiArenaBook/](https://rossetti.github.io/RossettiArenaBook/ch5-RepDeletion.html) — open-access textbook with detailed Welch plot methodology and implementation guidance.
- Gafarian AV, Ancker CJ, Morisaku T (1978). "Evaluation of Commonly Used Rules for Detecting Steady State." *Naval Research Logistics Quarterly*, 25: 511–529 — foundational evaluation of warm-up detection methods.
- Romero-Brufau S et al. (2020). PMC7491890 (cited above) — reports a 30-day warm-up prior to a 2-year measurement period in a critical care DES, illustrating scale calibration.

---

## Issue 3 — No Sensitivity Analysis

### Problem

The triangular distribution parameters for surgery duration, transport time, resuscitation, and DOW rates carry significant uncertainty. The finding that the system "operates with little reserve" may shift materially under plausible parameter perturbations. Without sensitivity analysis, no parameter can be identified as rate-limiting versus incidental to the result.

### Recommended Approach

Apply **Morris Elementary Effects (EE) screening** using R's `sensitivity` package. Morris is a global, one-at-a-time (OAT) method that identifies the few important parameters from a larger set at low computational cost — `r × (p + 1)` model evaluations where `r` is trajectory count (typically 10–20) and `p` is number of parameters. For the BCH model, approximately 15 triangular distribution parameters are candidates.

Key parameters to screen:

| Parameter | Current Mode | Range to Screen |
|-----------|-------------|-----------------|
| Surgery duration (R2B, R2E) | 95 min | 60–150 min |
| Long resuscitation | 45 min | 30–70 min |
| DOW rate (R1) | 5% P1 | 2–10% |
| Transport R1→R2B | 20 min | 10–45 min |
| Transport R2B→R2E | 30 min | 15–60 min |
| ICU duration (long) | 1440 min | 770–2160 min |
| OT shift availability | 12 hr | 8–16 hr |
| Second surgeon required per DCS case (Issue 4) | 30% | 10–70% |
| NO flex to surgical roles when concurrent resus active (Issue 4) | Assumed available | Boolean toggle: flex allowed vs. dedicated surgical NOs required |

Outputs to monitor: mean R2E ICU queue, mean R2B OT queue, total DOW count.

Following screening, apply **Sobol variance decomposition** (via `sensitivity::sobol2007`) to the ~5 most influential parameters to partition output variance between first-order and interaction effects.

### References

- Morris MD (1991). "Factorial Sampling Plans for Preliminary Computational Experiments." *Technometrics*, 33, 161–174 — original Morris method paper.
- CRAN `sensitivity` package: `morris()` function. [https://rdrr.io/cran/sensitivity/man/morris.html](https://rdrr.io/cran/sensitivity/man/morris.html) — R implementation with documented API.
- OpenMOLE documentation: "Morris Screening Method." [https://openmole.org/Sensitivity.html](https://openmole.org/Sensitivity.html) — accessible methodological overview.

---

## Issue 4 — Team-Block Resource Seizure (Not Individual)

### Problem

Resources are seized as whole team vectors (e.g., `seize_resources(emergency_team)` locks the FACEM, all nursing officers, and medics simultaneously). This means a second casualty cannot use any team member even when the first casualty requires only a subset of skills. Individual-level modelling is the correct representation for:

- Parallel treatment by a divided team
- Skill-specific bottleneck identification (surgeon vs. anaesthetist vs. nursing officer)
- Task-sharing under surge conditions

This is the most significant structural fidelity gap in the implementation.

### ADF Clinical Workforce Note

The task-role mapping for this refactor must reflect the ADF's specific clinical workforce structure, which differs materially from US military and civilian healthcare frameworks. The ADF has three clinical categories relevant to this model:

- **Medical Officer (MO):** Commissioned officer; qualified medical practitioner. The `Doctor`, `Facem`, `Surgeon`, `Anesthetist`, and `Intensivist` labels in `env_data.json` all map to MO-grade clinicians.
- **Nursing Officer (NO):** Commissioned officer; qualified registered nurse. The `Nurse` label in `env_data.json` maps to this role across all echelons and sections.
- **Health Soldier (Medic):** The single ADF enlisted clinical category, holding a 76-week qualification that includes a civilian enrolled nursing diploma alongside TCCC and military-specific proficiencies. The `Medic` label in `env_data.json` maps to this role. There is no separate nurse technician or nursing assistant MOS — every ADF clinical soldier is a medic with this scope.

The ADF medic's enrolled nursing qualification supports independent management of stable casualties (IV maintenance, oral medications, monitoring) without officer supervision, which is broader than US combat medic scope and directly affects task-role allocation decisions. Full detail on role capabilities, task requirements per echelon, and the six highest-priority validation assumptions is provided in the companion document:

> **`BCH_Task_Role_Allocation.md`** — Task-Role Allocation Matrix for Individual Resource Modelling

That document should be read in full before implementing this issue. It provides the complete role inventory per echelon (Parts 1–2), the concurrent use implications of individual modelling (Part 3), the proposed `env_data.json` schema (Part 4), and the ranked validation agenda (Part 5).

### Recommended Approach

Restructure `env_data.json` to define individuals with named roles rather than role-grouped lists. Define a **task-to-skill requirement matrix** in the JSON configuration specifying the minimum staff needed per task type, derived from the task-role allocation matrix. The `simmer` `select()` activity with a policy filter supports this pattern.

**Step 1 — Restructure `env_data.json`** using the ADF-correct role labels. The role key `"no"` is used for Nursing Officer to clearly distinguish from a generic nurse label. Example for R2B:

```json
"r2b": [{
  "id": "r2b_1",
  "staff": [
    {"id": "r2b_1_facem_1",        "role": "facem",        "section": "emerg"},
    {"id": "r2b_1_nurse_1",        "role": "no",           "section": "emerg"},
    {"id": "r2b_1_nurse_2",        "role": "no",           "section": "emerg"},
    {"id": "r2b_1_nurse_3",        "role": "no",           "section": "hold"},
    {"id": "r2b_1_medic_1",        "role": "medic",        "section": "emerg"},
    {"id": "r2b_1_surgeon_1",      "role": "surgeon",      "section": "surg"},
    {"id": "r2b_1_surgeon_2",      "role": "surgeon",      "section": "surg"},
    {"id": "r2b_1_anaesthetist_1", "role": "anaesthetist", "section": "surg"},
    {"id": "r2b_1_medic_surg_1",   "role": "medic",        "section": "surg"},
    {"id": "r2b_1_medic_evac_1",   "role": "medic",        "section": "evac"},
    {"id": "r2b_1_medic_evac_2",   "role": "medic",        "section": "evac"}
  ],
  "task_requirements": {
    "resus_long":      {"facem": 1, "no": 2, "medic": 1},
    "resus_short":     {"no": 1, "medic": 1},
    "surgery":         {"surgeon": 1, "anaesthetist": 1, "no": 2},
    "surgery_complex": {"surgeon": 2, "anaesthetist": 1, "no": 2},
    "hold":            {"medic": 1},
    "evac":            {"medic": 1},
    "kia":             {"medic": 1}
  }
}]
```

Full schema for all three echelons is provided in `BCH_Task_Role_Allocation.md`, Part 4.

**Step 2 — Implement skill-matched seizure helper:**

```r
seize_by_role <- function(trj, team_data, task_name) {
  requirements <- team_data$task_requirements[[task_name]]
  for (role in names(requirements)) {
    n_required <- requirements[[role]]
    candidates <- Filter(function(s) s$role == role, team_data$staff)
    resource_names <- sapply(candidates, function(s) s$id)
    for (i in seq_len(n_required)) {
      trj <- trj %>%
        simmer::select(resource_names, policy = "shortest-queue") %>%
        seize_selected()
    }
  }
  trj
}

release_by_role <- function(trj, team_data, task_name) {
  requirements <- team_data$task_requirements[[task_name]]
  total_seized <- sum(unlist(requirements))
  for (i in seq_len(total_seized)) {
    trj <- trj %>% release_selected()
  }
  trj
}
```

**Step 3 — Update trajectory calls:**

Replace all `seize_resources(emergency_team)` with `seize_by_role(trj, team_data, "resus_long")` (or `"resus_short"` for P3/minor cases). Replace all `seize_resources(surg_team)` with `seize_by_role(trj, team_data, "surgery")`.

### Key Interaction Surfaced by This Refactor

The most operationally significant finding expected from this change concerns R2B surgical coverage. The R2B surgical section in `env_data.json` lists `Anesthetist (1), Surgeon (2), Medic (1)` — there are no dedicated surgical NOs in the R2B establishment. Under individual modelling, the scrub and circulating nurse roles during surgery must be filled by NOs flexing from the emergency section. This creates a genuine contention: if two P1 casualties are simultaneously in resus, all emergency NOs may be occupied, leaving surgery without scrub or circulating coverage. This interaction is invisible in the current team-block model and is the primary motivation for the refactor. It is validation item 1 in `BCH_Task_Role_Allocation.md`.

### References

- **`BCH_Task_Role_Allocation.md`** — companion document providing the complete ADF-specific task-role allocation matrix, role inventory, JSON schema, and validation priorities for this issue.
- Ucar I, Smeets B, Azcorra A (2019). JSS paper Section 3 — documents `select()` and `seize_selected()` as the correct pattern for dynamic, policy-driven resource selection in simmer. [https://r-simmer.org/articles/simmer-02-jss.pdf](https://r-simmer.org/articles/simmer-02-jss.pdf)
- "Australian Role 1 vs US Role 2: A Comparison of Capability and Lessons Learned." *The Cove*, February 2024. https://cove.army.gov.au/article/australian-role-1-vs-us-role-2-comparison-capability-and-lessons-learned — ADF-specific role scope and workforce structure; basis for medic vs. nursing officer task boundaries.
- Pitt M et al. (2019). "A novel modelling technique to predict resource requirements in critical care." *BMC Medical Informatics and Decision Making*, 19(1). [PMC6520084](https://pmc.ncbi.nlm.nih.gov/articles/PMC6520084/) — individual-level resource tracking in a simmer-based critical care model.

---

## Issue 5 — Flat DOW Rate Independent of Wait Time

### Problem

DOW is applied as a fixed probability (5% P1, 2.5% P2 at R1; 1% at R2B/R2E) regardless of how long a casualty has waited. This means ICU saturation, OT queuing, and evacuation delay have zero effect on mortality — the single most clinically consequential relationship in the model is absent.

### Recommended Approach

Replace the flat DOW probability with a **time-dependent survival function** that scales DOW probability as a function of elapsed time since injury (`now(env) - arrival_time`). The function should be parameterised by priority level and injury category.

A logistic survival decay function is appropriate:

```
P(DOW | t) = p_base + (p_max - p_base) × [1 / (1 + exp(-k × (t - t_inflect)))]
```

Where:
- `p_base` = baseline DOW probability at time zero (current flat rate)
- `p_max` = maximum DOW probability under indefinite delay (e.g., 0.80 for P1)
- `k` = steepness of the decay curve
- `t_inflect` = time at which DOW probability rises most steeply (e.g., 60 min for P1, aligned to NATO "golden hour" / 10-1-2 timeline)

Calibrate using the time-dependent mortality estimates in Frykberg (2002) and the SIMEDIS framework approach (Debacker et al., 2016).

### Implementation

```r
# Set arrival time as attribute when casualty is generated
set_attribute("arrival_time", function() now(env))

# Time-dependent DOW check (replace flat branch probability)
dow_probability <- function(priority, arrival_time_attr) {
  elapsed <- now(env) - arrival_time_attr
  p_base <- switch(priority, "1" = 0.05, "2" = 0.025, "3" = 0.005)
  p_max  <- switch(priority, "1" = 0.75, "2" = 0.50,  "3" = 0.20)
  k <- 0.05          # Steepness — calibrate to literature
  t_inflect <- 60    # Minutes — inflection at the golden hour
  p_base + (p_max - p_base) / (1 + exp(-k * (elapsed - t_inflect)))
}

# In trajectory branch:
branch(
  option = function() {
    p <- dow_probability(
      get_attribute(env, "priority"),
      get_attribute(env, "arrival_time")
    )
    if (runif(1) < p) return(1)
    return(2)
  },
  ...
)
```

### References

- Debacker M et al. (2016). "SIMEDIS: a Discrete-Event Simulation Model for Testing Responses to Mass Casualty Incidents." *Journal of Medical Systems*, 40(12). [PMC5069323](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5069323/) — SIMEDIS implements health progression curves vs. time as the core patient state model; directly applicable approach.
- Frykberg ER (2002). "Medical Management of Disasters and Mass Casualties from Terrorist Bombings." *Journal of Trauma*, 53(2):201–212 — provides time-dependent mortality estimates by injury class used to calibrate survival decay parameters.
- Coule PL et al. (2013). "A Novel Approach to Multihazard Modeling." *Disaster Medicine and Public Health Preparedness*, 7(2). [Cambridge Core](https://www.cambridge.org/core/journals/disaster-medicine-and-public-health-preparedness/article/novel-approach-to-multihazard-modeling-and-simulation/2729B34A8525F24DF9055D57C626E225) — demonstrates dose-response / time-dependent mortality integration in a DES framework.
- Vanderhoek M et al. (2013). "Is Overtriage Associated with Increased Mortality?" *Disaster Medicine and Public Health Preparedness*, 2(1). [PubMed](https://pubmed.ncbi.nlm.nih.gov/18388607/) — validates time-dependent mortality as a model variable in trauma DES; demonstrates sensitivity of mortality outcomes to treatment-capability ratios.
- NATO AJP-4.10 / "10-1-2-(+2)" clinical timeline — establishes the doctrinal basis for inflection point parameterisation (10 min haemorrhage control, 60 min resuscitation, 120 min surgery).

---

## Issue 6 — Unidirectional Transport (No Dead-Heading)

### Problem

PMV ambulances are seized for the outbound leg, then immediately released upon arrival. In practice, the vehicle must return to the originating echelon before it can transport another casualty. The current model systematically overestimates evacuation asset availability throughout the run.

### Recommended Approach

After releasing the payload at the destination, hold the transport resource for a second timeout representing the return journey before releasing it back to the available pool. The return leg time can mirror the outbound distribution or apply a fixed multiplier (e.g., 1.0× outbound time for symmetric terrain, 0.8× for unladen return).

### Implementation

```r
r1_transport_wia <- function() {
  trajectory("Transport WIA (with dead-head)") %>%
    simmer::select(env_data$transports$PMVAmb, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    # Outbound leg
    timeout(function() {
      rtriangle(1,
        env_data$vars$r1$wia_transport$min,
        env_data$vars$r1$wia_transport$max,
        env_data$vars$r1$wia_transport$mode)
    }) %>%
    # Dead-head return — resource held, casualty branches off
    timeout(function() {
      rtriangle(1,
        env_data$vars$r1$wia_transport$min * 0.8,
        env_data$vars$r1$wia_transport$max * 0.8,
        env_data$vars$r1$wia_transport$mode * 0.8)
    }) %>%
    release_selected()
}
```

The casualty entity should branch away from the transport trajectory upon arrival; only the vehicle completes the return timeout. Use `clone()` or a `join()` pattern to separate the casualty flow from the vehicle return.

### References

- US Army Medical Center of Excellence. (2024). "Digital Simulations to Enhance Military Medical Evacuation Decision-Making." arXiv:2507.06373 — describes dead-heading as an explicit modelling requirement in military MEDEVAC simulation; uses Poisson-distributed return legs in a comparable framework.
- NATO AJP-4.10 — evacuation asset planning norms used to bound round-trip time parameters.

---

## Issue 7 — Undifferentiated DNBI Treatment Pathway

### Problem

All DNBI casualties enter the same triage-resus-surgery routing as WIA. In practice, disease and battle fatigue cases almost never require surgery and rarely require R2B. They primarily consume R1 capacity and holding beds. Routing them through the surgical pathway inflates modelled surgical demand and understates the true WIA surgical bottleneck.

### Recommended Approach

Sub-categorise DNBI at generation time and apply differentiated trajectories:

| Sub-category | Share | Pathway |
|---|---|---|
| Battle fatigue / psychiatric | ~25% of DNBI | R1 hold → RTD. No R2 routing. |
| Disease (febrile, GI, respiratory) | ~58% of DNBI | R1 treatment → R2B holding if not RTD. No surgery. |
| Non-battle injury (musculoskeletal, accidental) | ~17% of DNBI | Standard WIA-equivalent routing (current). |

The 17% NBI figure already exists in the model. Extend it to split the remaining 83% between disease and battle fatigue, each with a bypass trajectory that skips the surgical branch entirely.

### References

- Izaguirre MK et al. (2025). "To Conserve Fighting Strength in LSCO." *Military Review Online*. [Army.mil](https://www.armyupress.army.mil/Journals/Military-Review/Online-Exclusive/2025-OLE/Conserve-Fighting-Strength-in-LSCO/) — describes DNBI composition and RTD management by category in LSCO.
- Amoroso PJ, Bell NS (2008). "U.S. Army Disease and Nonbattle Injury Model, Refined in Afghanistan and Iraq." *Military Medicine*. [PubMed 18816921](https://pubmed.ncbi.nlm.nih.gov/18816921/) — provides admission rate disaggregation by DNBI category across conflict phases; basis for sub-category proportions.
- Amara M et al. (2024). "Disease and Non-Battle Injury in Deployed Military: A Systematic Review." *Military Medicine*, 189(Supplement_3). [Oxford Academic](https://academic.oup.com/milmed/article/189/Supplement_3/21/7735911) — documents that 82% of all medical evacuations from Iraq were DNBI; surgical rates by DNBI sub-category support the bypass pathway design.

---

## Issue 35 — R2B OT Bypass Check Uses `<=` Instead of `<` ✓

**Merged PR #36** — see "Issues In Review" section above for full detail.

---

## Issue 37 — OT Bed Incorrectly Scheduled (Rooms Must Be 24h)

### Problem

`build_env()` in `R/environment.R` applied 12-hour alternating shift schedules to OT bed resources (`b_r2b_ot_*`, `b_r2eheavy_ot_*`) in addition to the surgical team resources. OT rooms are physical spaces that are available 24 hours per day; only the surgical team (surgeons, anaesthetist) should carry a shift schedule. Applying the schedule to the bed caused OT bed capacity to toggle to zero when the team was off-shift, which produced two errors:

1. Surgery became impossible even during brief on-shift windows adjacent to a handover.
2. The existing bypass check (Issue #35) saw zero bed capacity and evaluated `usage < cap` as FALSE during off-shift periods, triggering bypass correctly by accident. Removing the bed schedule without updating the bypass logic would have allowed patients to seize OT beds off-shift and queue for the team — an equally wrong state.

### Fix

1. Remove shift schedule from all `ot_bed` resources in `build_env()`. Always register OT beds with `add_resource(res_name)` (no schedule argument).
2. Add `get_capacity(surg_team) > 0` as a third condition in the R2B bypass check. This explicitly bypasses off-shift patients regardless of bed availability.

**PR:** #38, branch `feature/issue-37-ot-bed-schedule-fix`

---

## Issue 39 — R2B Holding Bed Saturation

### Problem

R2B holding beds saturate and queue from approximately Day 10–15 of the 30-day run and remain saturated for the remainder of the simulation. The primary driver is disease DNBI casualties (Issue #7 sub-type 2), who are assigned a holding duration with mode approximately 5 days and are eventually evacuated to R2E or strategically cleared. At the current DNBI disease arrival rate, the holding load from this stream exceeds R2B holding bed capacity, creating a sustained queue that displaces WIA casualties who require observation or post-surgical holding.

This is operationally significant: hold bed saturation at R2B forces WIA casualties requiring observation into either early evacuation (consuming transport assets) or R1 retention (suboptimal clinical environment). It also affects R2B throughput metrics — casualties waiting for a hold bed inflate R2B queue statistics without representing a surgical bottleneck.

### Recommended Approach

1. **Decompose hold bed occupancy by stream.** Add a `hold_reason` attribute (values: `wia_p1`, `wia_p2`, `wia_p3`, `dnbi_disease`, `dnbi_nbi`) to all entities entering hold beds, enabling occupancy decomposition by casualty type.
2. **Quantify saturation onset.** Use `get_mon_resources()` to identify the minute at which hold bed queue first exceeds zero and the duration of saturation across a multi-replication run.
3. **Scenario test.** Run sensitivity analysis on disease DNBI holding duration and disease evacuation probability to identify whether extending evacuation priority for disease cases (reducing hold occupancy) or adding hold beds (capacity increase) is the more effective intervention.

**Dependencies:** Issues 1, 2 (completed), 7 (PR #34 open — must merge first to enable stream decomposition).

---

## Issue 40 — R2B OT Suboptimal Utilisation

### Problem

R2B OT utilisation is approximately 25% of available surgical team shift time (12 hours per day) under the post-Issue-#37 baseline. Of 200 surgical candidates reaching R2B in the seed-42 30-day run, 129 (64.5%) bypass to R2E — the majority because the surgical team is off-shift at the time of arrival, not because the OT bed is occupied. The R2B OT is idle for three-quarters of its operational shift window, and forward surgical capability is underused.

This is operationally significant for two reasons:

1. Surgery performed at R2B is clinically preferable for high-acuity casualties when the team is available: it is faster (no R2B→R2E transport delay), reduces haemorrhage time, and preserves R2E capacity for cases that cannot be managed forward.
2. The current 12-hour shift window creates a structural gap: casualties generated in the off-shift period cannot receive R2B surgery regardless of OT bed availability. In high-tempo operations, contested airspace or disrupted communications may make R2E transfer infeasible, and the off-shift gap could be operationally unacceptable.

The Morris sensitivity screening (Issue #3) ranked `ot_hours` as the dominant controllable lever for surgical throughput (µ\* = 0.978), confirming that extending OT shift availability is the highest-impact single intervention available within the current establishment.

### Recommended Approach

1. **Track bypass reason.** Add `r2b_bypass_reason` attribute with values `off_shift` (team capacity = 0) and `ot_occupied` (bed full or queued). This enables planners to distinguish structural off-shift losses from demand-driven losses.
2. **Sensitivity scenario.** Using the `ot_hours` parameter already wired into `run_replications()` from Issue #3, test OT shift availability at 12, 14, 16, and 20 hours per day. Report bypass rate, R2B surgery count, and R2E load at each scenario.
3. **Establishment option.** Evaluate the marginal effect of adding a second surgical team (enabling continuous coverage via alternating 12-hour shifts). This requires the individual resource modelling from Issue #4 to be meaningful at the team level.

**Dependencies:** Issues 1, 2, 3 (completed), 7 (PR #34 — for stream decomposition), 37 (PR #38 — corrects OT bed availability baseline). Issue #4 required for establishment sensitivity.

---

## Issue 8 — R2E Surgical Team Not Seized During OT

### Problem

In `r2e_treat_wia()`, the `seize_resources(surg_team)` calls are commented out, meaning R2E OT surgery proceeds without locking the surgical team. This means the model allows the same surgical team to be simultaneously counted as available for a second OT — an impossible state. R2E surgical throughput is consequently overestimated.

### Fix

Uncomment `seize_resources(surg_team)` and `release_resources(surg_team)` for both the first and second surgery branches in `r2e_treat_wia()`. If the original intent was to abstract away staffing constraints at R2E, document this explicitly as a named assumption in `env_data.json` and the README.

If the intent was a scheduled shift model similar to R2B, apply the same `ot_shift_1` / `ot_shift_2` alternating schedule to R2E surgical teams in `build_env()`.

This is a low-effort, high-impact fix — surgical team seizure is already implemented correctly at R2B; the fix is three lines of code.

---

## Issue 9 — No MASCAL Stochastic Injection

### Problem

The current casualty generation model produces a smooth lognormal daily rate with sub-minute jitter. LSCO is characterised by discrete tactical events that generate acute casualty surges — artillery barrages, drone strikes, vehicle-borne IED detonations — producing 20–50 casualties within a 2–4 hour window. These events are the primary stress test for surgical and ICU capacity, and they are entirely absent from the current model.

### Recommended Approach

Implement a **compound Poisson process** for MASCAL injection. A Poisson process governs event inter-arrival times; a secondary distribution governs the number of casualties per event. Both are overlaid on the existing lognormal background generation.

Parameters (initial values for calibration):
- MASCAL inter-arrival: exponential with mean = 5 days (λ = 1/5 per day)
- Casualties per event: negative binomial or uniform (20–60 casualties)
- Priority distribution during MASCAL: skewed toward P1 (70% P1, 20% P2, 10% P3)
- Duration of injection window: triangular(60, 180, 120) minutes

```r
generate_mascal_events <- function(n_days, rate_per_day = 0.2, 
                                    min_cas = 20, max_cas = 60) {
  n_events <- rpois(1, rate_per_day * n_days)
  if (n_events == 0) return(numeric(0))
  
  event_times <- sort(runif(n_events, 0, n_days * day_min))
  
  events <- lapply(event_times, function(t) {
    n_casualties <- sample(min_cas:max_cas, 1)
    window <- rtriangle(1, 60, 180, 120)
    sort(t + runif(n_casualties, 0, window))
  })
  
  unlist(events)
}

# Merge with background arrival stream
mascal_arrivals <- generate_mascal_events(n_days)
all_arrivals <- sort(c(background_arrivals, mascal_arrivals))
```

### References

- Duffney P et al. (2024). "Digital Simulations to Enhance Military Medical Evacuation Decision-Making." arXiv:2507.06373 — uses compound Poisson process with λ₁ (inter-incident time) and λ₂ (casualties per incident) parameters for LSCO casualty generation; directly analogous implementation.
- Debacker M et al. (2016). PMC5069323 (cited above) — SIMEDIS uses stochastic MASCAL event injection as the primary system stress test; provides methodological basis for the compound process.
- Coule PL et al. (2013). Cambridge Core link (cited above) — demonstrates surge-response modelling with time-to-treatment effects on mortality, providing the analytical framework for interpreting MASCAL output.
- US Army. (2026). "From MASCAL to Campaign Medicine: Aligning Field Hospital Training with LSCO Reality." *The Army* (April 2026). [Army.mil](https://www.army.mil/article/290575/from_mascal_to_campaign_medicine_aligning_field_hospital_training_with_lsco_reality) — documents blast-dominant injury patterns and sustained surge dynamics in contemporary LSCO as the doctrinal basis for MASCAL parameterisation.

---

## Issue 10 — No Comparative Casualty Rate Scenario

### Problem

The study identifies Okinawa and Vietnam FORECAS parameters as relevant stress tests but does not implement them. The current Falklands-derived rate (~0.37%) is the most conservative available; the conclusions about system adequacy are bounded to that scenario alone.

### Recommended Approach

Implement a **scenario runner** that accepts a named casualty configuration and runs the full multi-replication analysis against each. At minimum, test three scenarios:

| Scenario | WIA μ | WIA σ | Source |
|---|---|---|---|
| Falklands (current) | 1.77 | 3.56 | FORECAS Table A.8 |
| Vietnam | 4.12 | 6.89 | FORECAS Table A.5 |
| Okinawa | 8.40 | 11.20 | FORECAS Table A.2 |

Package the scenario parameters in `env_data.json` as named scenario blocks, and produce a comparative output table showing queue lengths, DOW counts, and throughput by scenario. This transforms the research from a single-point analysis into a genuine system suitability assessment across LSCO intensity levels.

### References

- Blood CG, Zouris JM, Rotblatt D (1998). *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. [DTIC ADA339487](https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf) — Tables A.2, A.5, A.8 provide all three scenario parameter sets.

---

## Issue 22 — Output Variable Register

### Problem

Trajectory attributes were added incrementally to support branching logic. No systematic mapping exists between tracked variables and the planner decisions or doctrinal standards they inform. Five timing intervals critical to planning — R2B dwell time, R2B→R2E transit, R2E dwell, DOW by echelon, RTD by echelon — are not computable from existing attributes. Without a defined output vector, Morris sensitivity screening (Issue 3) screens against an ad-hoc KPI set rather than the outputs planners need.

### Recommended Approach

Define an Output Variable Register (OVR) using five criteria derived from doctrine and DES methodology: (C1) compliance with a named AJP-4.10 / ADDP 4.2 standard; (C2) planner decision relevance; (C3) causal pathway position for Morris screening; (C4) binding constraint identification; (C5) health outcome attribution.

Document the register in the README under a new "Model Outputs" section using `MODEL OUTPUT` annotation blocks, and add the five missing timing attributes (`arrival_time_r2b`, `depart_time_r2b`, `arrival_time_r2e`, `depart_time_r2e`, `surgery_start_time`) to trajectory code.

### References

- Sargent RG (2013). "Verification and Validation of Simulation Models." *Journal of Simulation*, 7(1), 12–24 — establishes that DES model outputs must be linked to their theoretical and doctrinal basis as a condition of model validity.
- AJP-4.10 / ADDP 4.2 — defines the performance standards against which KPIs must be measured.

---

## Issue 24 — Variance Reduction (Antithetic Variates / L'Ecuyer-CMRG)

### Problem

The replication framework uses `mc.set.seed = FALSE`, providing no guarantee of non-overlapping RNG streams across parallel workers. Dependent streams would inflate apparent CI precision. Additionally, no variance reduction technique is applied, meaning CI convergence requires the full Monte Carlo sample size. The `mc.cores` argument is omitted, capping parallelism at 2 cores regardless of hardware.

### Recommended Approach

Three changes in a single PR to `R/replication.R`:

1. **L'Ecuyer-CMRG streams**: Set `RNGkind("L'Ecuyer-CMRG")` before `mclapply` and use `mc.set.seed = TRUE`. Each worker receives a provably non-overlapping MRG32k3a substream (L'Ecuyer et al., 2002).

2. **Antithetic variates**: For each primary replication using uniform `u`, run a paired antithetic replication using `1 - u`. Pair-average before CI computation. For monotone response functions this halves variance, reducing required replications by approximately 50% for equivalent CI width.

3. **Core count**: Pass `mc.cores = parallel::detectCores(logical = FALSE)` to `mclapply` to use all physical cores (this is already set as an R option in the Dev Container via Issue 19, but must be explicit in the call for non-container environments).

### References

- L'Ecuyer P, Simard R, Chen EJ, Kelton WD (2002). "An Object-Oriented Random-Number Package with Many Long Streams and Substreams." *Operations Research*, 50(6), 1073–1075 — RngStream implementation underlying `RNGkind("L'Ecuyer-CMRG")` in R.
- L'Ecuyer P (2024). "Random Number Generation." *Wiley Encyclopedia of Operations Research and Management Science* — establishes non-overlap guarantee conditions.
- Law AM (2015). *Simulation Modelling and Analysis*, 5th ed., Chapter 11 — antithetic variates derivation and conditions of applicability.

---

## Issue 14 — Shiny App: Parameter Editor and Quick Run Mode

### Problem

The current `controller.R` Shiny application is a raw JSON editor. It exposes internal parameter names (`wia_cbt`, `short_resus`, `ot_beds`) with no operational context, no domain validation, and no ability to execute the simulation or display results. A planner or medical officer cannot use it without understanding the underlying code structure.

### Recommended Approach

Replace `controller.R` with `app.R` structured as a three-panel workflow: **Configure → Run → Analyse**.

- **Configure panel**: Parameters grouped by operational concept with plain-English labels and tooltips. Slider bounds use `morris_params$lower`/`upper` from `R/sensitivity.R` where the parameter appears there. Internal JSON field names are never exposed.
- **Run panel**: Quick Run (single replication, configurable seed and duration) via async execution. A progress indicator reflects simulation state.
- **Analyse panel**: Four-tab display — Casualty Flow, Resource Utilisation, Queue Dynamics, KPI Summary — rendered from ggplot objects returned by `analyse_run()` in `R/analysis.R`. CSV and PNG export. A read-only Sensitivity Calibration tab surfaces `morris_params` bounds.

Multi-run Full Analysis and sensitivity execution are deferred to Issue 15.

**Dependencies:** Issue 1 (replication framework; `R/analysis.R` refactor returning ggplot objects). Issue 3 (`morris_params` bounds for slider validation).

---

## Issue 15 — Shiny App: Full Analysis Mode (Multi-Run with CI)

### Problem

Quick Run (Issue 14) produces single-replication output unsuitable for defensible planning conclusions — results vary with seed and carry no statistical bounds. Full Analysis mode activates the deferred capabilities: multi-run execution with CI, sensitivity screening execution, and integrated sensitivity result display.

### Recommended Approach

Extend `app.R` from Issue 14:

- **Full Analysis mode**: Enable the mode selector. Wire the replication count slider (10–1000, default 100) to `run_replications()` via async execution. On completion, call `analyse_replications()` (new function in `R/analysis.R`) to produce CI ribbon plots and KPI summary cards. Apply the warm-up period constant from Issue 2 before aggregation.
- **Sensitivity panel (active)**: Activate the "Run Sensitivity Screening" button from Issue 14. Trigger `run_morris()` asynchronously with user-configurable `r` (trajectories) and reps-per-point. Display μ\* vs σ scatter plot for R2E ICU queue, ranked parameter table, and Sobol first-order / total-effect bar chart.

**Dependencies:** Issues 14, 1, 2, 3.

---

## Issue 18 — Endogenous Casualty Generation (Force Regeneration)

### Problem

Casualty arrival rates are fixed exogenous inputs applied to a static force size. The model does not represent the feedback loop between return-to-duty rates, strategic evacuation, force depletion, and future casualty production. The `in_theatre_rate` parameter has no causal pathway to any OT, ICU, or arrival-rate metric — its influence in sensitivity screening is a bootstrap artefact. The simulation cannot answer: *what is the net effect on system load and force effectiveness of increasing the in-theatre recovery rate?*

### Recommended Approach

Introduce a time-varying effective force size updated daily:

```r
effective_force[day] <- initial_force
  + cumulative_rtd[day]
  - cumulative_strategic_evac[day]
  + reinforcement_schedule[day]

arrival_rate[day] <- base_rate * effective_force[day] / initial_force
```

The reinforcement schedule is a configurable step-input sequence in `env_data.json`. This closes the feedback loop without restructuring the arrival generator — only the per-day rate scalar changes.

**Dependencies:** Issues 1, 2 (replication framework and warm-up period needed before force-feedback output is interpretable).

---

## Issue 23 — Strategic Evacuation Demand: Role 4 and AME Sorties

### Problem

Patients flagged `r2e_evac = 1` (strategic evacuation) disappear from the simulation with no downstream outputs. The model produces no estimate of Role 4 bed occupancy by ward type, no national hospitalisation census, and no derived metric for strategic aeromedical evacuation (AME) sortie demand. The simulation cannot support the two most consequential strategic medical planning questions: what Role 4 capacity is required to absorb the evacuation flow; and how many AME sorties are required to clear the backlog.

This issue is the downstream complement of Issue 18. Issue 18 models the effect of strategic evacuation on theatre force size; this issue models the effect on national health assets.

### Recommended Approach

Implement a post-simulation Role 4 census calculation (not a constrained simmer resource — Role 4 is treated as unconstrained; the output is a demand signal):

1. Capture `injury_type`, `priority`, `treatment_received`, and `evac_time` as trajectory attributes at the evacuation branch.
2. Apply acuity-stratified national hospitalisation duration distributions (triangular, sourced from Role 4 occupancy norms in AJP-4.10) to produce a time-series Role 4 census by ward type.
3. Derive AME sortie demand as a secondary output: `sorties = ceiling(evacuees_per_day / patients_per_sortie)` with configurable aircraft capacity.

**Dependencies:** Issues 1, 22 (attribute infrastructure), 18 (evacuation counts feed force feedback).

---

## Implementation Sequencing

### Pre-phase — Infrastructure (Issue 19 ✓ Complete)

Dev Container specification merged (PR #21). All contributors now develop in a reproducible Linux R environment with `mclapply` running at full core count.

### Phase 1 — Statistical Foundation (Issues 1 ✓, 22 ✓, 2 ✓, 3 ✓, 24 ✓)
*Estimated effort: 3–4 weeks. All subsequent analyses depend on this foundation. **Complete.***

1. ~~Multi-replication wrapper (`mclapply` + `wrap()`) — **Merged PR #16**~~
2. ~~**Issue 22** — Define Output Variable Register; add five missing timing attributes to trajectories. **Merged PR #26**~~
3. ~~**Issue 24** — Switch to L'Ecuyer-CMRG RNG streams, add antithetic variates, set explicit `mc.cores`. **Merged PR #32**~~
4. ~~**Issue 2** — Welch warm-up analysis; set `warm_up_period` constant. **Merged PR #20**~~
5. ~~**Issue 3** — Morris Elementary Effects screening using the OVR KPIs from Issue 22. **Merged PR #30**~~

### Phase 2 — Model Fidelity (Issues 8 ✓, 35 ✓, 37 ✓, 44 ✓, 6 ✓, 5 ✓, 43 ✓, 14)
*Estimated effort: 2–3 weeks. Low-to-medium code changes, high impact on result validity.*

6. ~~**Issue 8** — Fix R2E surgical team seizure (three lines; do first). **Merged.**~~
7. ~~**Issue 35** — Fix R2B OT bypass check (`<=` → `< && queue == 0`). **Merged PR #36.**~~
8. ~~**Issue 37** — Remove 12h schedule from OT bed resources; add team-availability bypass check. **Merged PR #38.**~~
9. ~~**Issue 44** — RTD KPI annotation: decomposed `total_rtd` into `bf_rtd` + `clinical_rtd`, added `rtd_type` column to `rtd_by_echelon`, two `stopifnot()` guards, seed-42 baseline documented. **Merged PR #47.**~~
10. ~~**Issue 6** — Dead-heading return legs for transport assets.~~ — **Merged PR #56.**
11. ~~**Issue 5** — Time-dependent DOW survival function.~~ — **Merged PR #53.**
12. ~~**Issue 43** — OT–ICU gating: implement three-way pre-OT branch (ICU available / ICU full + P1 / ICU full + P2+).~~ — **Merged PR #59.**
13. **Issue 14** — Shiny app parameter editor and Quick Run mode. Requires `R/analysis.R` refactor returning ggplot objects (Issue 1 dependency already satisfied).

### Phase 3 — Structural Refactoring (Issues 7, 4, 39, 40)
*Estimated effort: 4–5 weeks. Requires `env_data.json` schema changes, trajectory rewrites, and hold-bed decomposition.*

10. ~~**Issue 7** — DNBI sub-category routing~~ — **Merged PR #34.** Prerequisite for Issue #39 satisfied.
11. ~~**Issue 39** — R2B hold bed saturation analysis~~ — **Merged PR #48.** Two-tier routing policy (upstream threshold + at-R2B three-stage branch) implemented; per-stream occupancy decomposition added to analysis pipeline.
12. **Issue 40** — R2B OT utilisation improvement. Add `r2b_bypass_reason` attribute; scenario-test `ot_hours` at 12/14/16/20h; evaluate second surgical team option (partial result without Issue #4).
13. **Issue 4** — Individual resource seizure. Read `BCH_Task_Role_Allocation.md` in full before beginning. Gated until Issues 1, 2, and 3 are all stable. Address the six validation assumptions in `BCH_Task_Role_Allocation.md` Part 5 — document each as a named model assumption in the README, and include the two highest-priority assumptions (NO flex to surgical roles; second-surgeon probability) in the Morris screening from Phase 1.

### Phase 4 — Scenario Expansion (Issues 9, 10, 18, 23)
*Estimated effort: 3–4 weeks. Builds on Phase 1–3 outputs.*

12. **Issue 9** — Compound Poisson MASCAL injection overlay. Requires Issues 1, 2, 5.
13. **Issue 10** — Comparative scenario runner (Falklands / Vietnam / Okinawa). Requires Issues 1, 2, 5, 8.
14. **Issue 18** — Endogenous casualty generation (force regeneration feedback). Requires Issues 1, 2, 22.
15. **Issue 23** — Role 4 occupancy and AME sortie demand. Requires Issues 1, 22, 18.

### Phase 5 — Interface (Issue 15)
*Estimated effort: 1–2 weeks.*

16. **Issue 15** — Shiny Full Analysis mode (multi-run CI, sensitivity panel). Requires Issues 14, 1, 2, 3.

### Dependency graph

```
COMPLETE (merged to main):
  #19  Dev Container
  #1   Multi-run replication framework
  #22  Output Variable Register
  #8   R2E surgical team seizure fix
  #2   Warm-up analysis (terminating simulation confirmed; WARM_UP_DAYS = 0)
  #3   Morris sensitivity screening
  #24  Variance reduction (RNG)
  #7   DNBI sub-categorisation (PR #34)
  #35  R2B OT bypass check fix (PR #36)
  #37  OT bed schedule fix (PR #38)
  #44  RTD KPI decomposition — bf_rtd + clinical_rtd (PR #47)
  #39  R2B hold bed saturation — two-tier routing policy (PR #48)
  #5   Time-dependent DOW — Falklands calibration (PR #53)
  #6   Dead-heading transport — return_leg_multiplier, transport KPIs, capacity margin plot (PR #56)
  #43  OT–ICU gating — pre-OT ICU availability check, post-op hold pathway, shared
       post-operative DOW checkpoint, R2E OT-ICU gating impact plot (PR #59)

IN REVIEW (PRs open against main):
  (none)

UNBLOCKED (start now):
  #4   Individual resource seizure   (gating satisfied: #1 + #2 + #3 all merged)
  #14  Shiny app — Quick Run         (needs #1 analysis.R refactor only)
  #40  R2B OT utilisation analysis   (unblocked by #35 ✓ + #37 ✓)
  #9   MASCAL injection              (unblocked: #1 ✓ + #2 ✓ + #5 ✓)
  #18  Force regeneration feedback   (unblocked: #1 ✓ + #2 ✓ + #5 ✓)
  #10  Scenario runner               (unblocked: #1 ✓ + #2 ✓ + #5 ✓ + #8 ✓)

AFTER #14 + #1 + #2 + #3:
  #15  Shiny — Full Analysis mode

AFTER #1 + #22 + #18:
  #23  Role 4 / AME sortie demand

AFTER #10:
  #57  Fleet-size capacity margin sweep for transport assets — stubbed as
       plot_transport_capacity_margin_by_fleet_size() in R/analysis.R
```

---

## Output Standards for Revised Analysis

All reported metrics should adopt the following format:

| Metric | Single Run (current) | Revised Standard |
|---|---|---|
| Queue length | Point-in-time from seed 42 | Mean (p10–p90) across 1,000 replications |
| Wait time | Single observation | Mean ± 95% CI |
| Bottleneck identification | Visual inspection | Morris EE μ* ranking |
| DOW count | Fixed probability | Time-dependent survival function output |
| System capacity verdict | "adequate for Falklands rates" | Quantified probability of exceeding capacity threshold per scenario |

---

*Prepared June 2026. Updated 02 July 2026 to reflect: completion of Issues #19 (PR #21), #1 (PR #16), #8, #22 (PR #26), #2 (PR #20), #3 (PR #30), #24 (PR #32), #7 (PR #34), #35 (PR #36), #37 (PR #38), #44 (PR #47), #39 (PR #48), #5 (PR #53), #6 (PR #56), and #43 (PR #59); and addition of new Issues #43 (OT–ICU gating), #44 (RTD KPI annotation), #57 (fleet-size capacity margin sweep), and #60 (bed/resource `qty: 0` silently creates one unit instead of zero — discovered during Issue #43 testing). Phase 1 Statistical Foundation complete. Phase 2 Model Fidelity in progress — Issues #8, #35, #37, #44, #5, #6, and #43 merged; Issues #4, #14, #40, #9, #18, and #10 all unblocked. Phase 3 structural refactoring in progress — Issues #7 and #39 merged, Issue #4 unblocked. Issue #57, a follow-up for a transport fleet-size capacity margin sweep drafted during Issue #6 (Phase 4, blocked on #10), has now been raised. All referenced resources are open-access.*
