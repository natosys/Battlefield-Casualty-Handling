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
| 4 | Team-block resource seizure (not individual) | High | High | Backlog |
| 5 | Flat DOW rate independent of wait time | High | Medium | **Merged (PR #53)** |
| 6 | Unidirectional transport (no dead-heading) | Medium | Low | **Merged (PR #56)** |
| 7 | Undifferentiated DNBI treatment pathway | Medium | Medium | **Merged (PR #34)** |
| 8 | OT surgical team not seized at R2E | Medium | Low | **Merged** |
| 9 | No mass casualty stochastic injection | Medium | Medium | **Merged (PR #92)** |
| 10 | No comparative scenario (Okinawa/Vietnam rates) | Lower | Low | **Merged (PR #69)** |
| 14 | Shiny app — parameter editor, Quick Run mode | Medium | Medium | **Merged (PR #71)** |
| 15 | Shiny app — Full Analysis mode (multi-run CI) | Medium | Medium | **Merged (PR #97)** |
| 18 | Endogenous casualty generation (force feedback) | Medium | High | **Merged (PR #105)** |
| 19 | Dev Container — reproducible Linux R environment | Low | Low | **Merged (#21)** |
| 22 | Output Variable Register — KPI definition | High | Low | **Merged (#26)** |
| 23 | Strategic evacuation demand — Role 4 / AME sorties | Medium | Medium | **Merged (PR #107)** |
| 24 | Variance reduction — antithetic variates / L'Ecuyer | Medium | Low | **Merged (#32)** |
| 35 | R2B OT bypass check — `<=` rather than `<` allows queuing | High | Low | **Merged (PR #36)** |
| 37 | OT bed incorrectly scheduled — rooms must be 24h | High | Low | **Merged (PR #38)** |
| 39 | R2B holding bed saturation — DNBI disease exhausts hold capacity | High | Medium | **Merged (PR #48)** |
| 40 | R2B OT suboptimal utilisation — 12h shift window limits forward surgery | Medium | Medium | Bypass-reason diagnostic **Merged (PR #64)**; Scenario A/B **Backlog** |
| 43 | OT–ICU gating absent — surgery proceeds regardless of ICU availability | Medium | Medium | **Merged (PR #59)** |
| 44 | RTD KPI implicitly includes battle fatigue RTDs without annotation | Low | Low | **Merged (#47)** |
| 54 | Scenario-level parameter profiles for historical conflict calibration | High | Medium | **Merged (PR #67)** |
| 60 | `qty: 0` in env_data.json silently creates one unit instead of zero | Low | Low | **Merged (PR #62)** |
| 73 | R2B → R2E WIA dead-heading return leg configured but never applied | Medium | Medium | **Merged (PR #81)** |
| 74 | Remove the dead-heading return leg multiplier | Medium | Low | **Merged (PR #83)** |
| 75 | Stale Morris screening bounds for `p1_p_max` (predates Issue #5 recalibration) | High | Low | **Merged (PR #79)** |
| 76 | R2B/R2E surgery duration narrative diverges from shipped `env_data.json` | Low | Low | **Merged (PR #89)** |
| 85 | `check_env_data_summary.R` crashes on R1's integer `sub_elm` value | Low | Low | **Merged (PR #87)** |
| 72 | Adopt `renv` for reproducible R package dependency pinning | Medium | Medium | **Merged (PR #91)** |
| 93 | Dev Container build failure — missing `libuv` runtime + `renv.lock` R version mismatch | High | Low | **Merged (PR #94)** |
| 77 | Configure panel eager-render race — silent revert of typed/scenario-switch edits within ~15–20s of load | High | Medium | **Merged (PR #101)** |
| 57 | Fleet-size capacity margin sweep for transport assets | Medium | Medium | **Merged (PR #103)** |
| 110 | Queue-depth plots use fixed y-axis scale and clip data | Medium | Low | **Merged (PR #118)** |
| 111 | Bed Resource Usage Gantt chart rows overlap due to insufficient vertical space | Medium | Low | **Merged (PR #120)** |
| 121 | Graphs in Shiny app exceed available window space instead of scaling to fit | Medium | Medium | **Merged (PR #123)** |
| 109 | Add AME repatriation and queue visualisation to analysis tab | Medium | Medium | **Merged (PR #126)** |
| 124 | Force reinforcement can credit effective force size above initial establishment strength | Critical | Low | **Merged (PR #129)** |

---

## Issues In Review (PRs Open — Awaiting Owner Merge)

*No PRs currently open against main.*

---

## Recently Merged Issues

### Issue 124 — Force Reinforcement Can Credit Effective Force Size Above Initial Establishment Strength ✓

**Merged:** PR #129, branch `claude/friendly-hypatia-ftb4zt`

`build_reinforcement_trajectory()` (`R/trajectories.R`, Issue #18) could credit `effective_force_combat`/`effective_force_support` above their initial establishment strength via two independent mechanisms: `credit_fn()` added the submission-time fill amount to the pool unconditionally, `fulfillment_lag_days` later, with no re-check against the shortfall actually remaining and no ceiling at `initial`; and overlapping demand cycles (`demand_interval_days < fulfillment_lag_days`) each independently computed demand from the pool's live value, so a still-pending cycle's already-claimed shortfall could be re-claimed by a later cycle (this second root cause was identified in a follow-up comment on the issue after the initial report). `credit_fn()` now clamps the credited value to `min(initial, current + fill)`; a new per-pool "pending" global (`reinf_combat_pending`/`reinf_support_pending`, initialised in `run_once()`, `R/replication.R`, only when reinforcement is enabled) tracks fill amounts already committed to an in-flight cycle — incremented at submission, decremented at credit — and `demand_fn()` nets this out of the live shortfall so overlapping cycles can no longer double-claim. Because `pending` tracks the fill actually committed rather than the full demand, an under-filled cycle's uncovered remainder stays visible to the next demand computation rather than being silently written off, per the follow-up comment's requested treatment. The README's [Force Regeneration and the Endogenous Feedback Loop](../README.md#6-force-regeneration-and-the-endogenous-feedback-loop) mechanism description and its `REINFORCEMENT DEMAND, FULFILLMENT LAG, AND TRIANGULAR FILL` MODEL ASSUMPTION block were updated to document the netting/clamp and to state explicitly that `fill_max_frac > 1.0` is a deliberate, now-safe choice given the credit-time ceiling clamp.

Verified against the real `simmer`-based mechanism, not a design-only argument: R 4.3.3 and the four packages `R/environment.R`/`R/trajectories.R`/`R/replication.R` actually require (`jsonlite`, `triangle`, `simmer`, `simmer.bricks`) were installed ad hoc in-session (no Docker daemon available for the pinned Dev Container). `run_once()` was run under an aggressive overlapping-cycle stress configuration (`demand_interval_days=2` vs `fulfillment_lag_days=10`, `fill_max_frac=1.5`) on the fixed code — 0 ceiling violations across 2,220 recorded global-write events, both pools capped exactly at their initial establishment strength — and the identical configuration was then re-run against `R/trajectories.R`/`R/replication.R` checked out from pre-fix `main`, which reliably overshot (`effective_force_combat` max 2624 of initial 2500, 558 violation events; `effective_force_support` max 1313 of initial 1250, 646 violation events), confirming the stress test genuinely exercises the reported bug rather than passing vacuously.

**Seed-42 baseline (30 days, single run):** Unaffected — reinforcement is only added as a generator (and only then initialises the new `reinf_*_pending` globals) when `demand_interval_days > 0`; the shipped default is `0` (disabled). Confirmed via direct re-run of the shipped default config on the fixed code: total casualties 386, matching `CLAUDE.md`'s documented post-Issue-23 baseline exactly, with `reinf_combat_pending`/`reinf_support_pending` absent from the attribute monitor. `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue lists Issue #124 as a dependency.

### Issue 109 — Add AME Repatriation and Queue Visualisation to Analysis Tab ✓

**Merged:** PR #126, branch `claude/issue-109-ymf2ox`

Issue #23 introduced strategic AME evacuation modelling (Role 4 occupancy, sortie scheduling, critical/standard route wait times), but the Shiny app's Analyse tab had no visualisation of it — a planner could not see AME sortie timing, wait-queue depth, or the critical/standard split without inspecting raw arrival logs. `plot_ame_queue()` (factored out of `analyse_run()`'s pre-existing inline `ame_backlog_plot` block, unchanged in behaviour at extraction time) and new `compute_ame_sorties()`/`plot_ame_sortie()` (reconstructing every scheduled sortie opportunity's outcome — configuration selected, seats used vs. capacity — from the `"ame"`/`"ame_critical"` resource monitor against the schedule's own deterministic firing times, since `build_ame_sortie_trajectory()` keeps no sortie log of its own) are now wired into both `analyse_run()` and `analyse_replications()` and into a new "Strategic AME" Analyse tab in `app.R`, with PNG/PDF/CSV downloads.

Verified by actually executing the pipeline, not by code inspection alone: R 4.3.3 and every required package were installed from source ad hoc in-session (no Docker daemon available for the pinned Dev Container), and `run.R --seed 42 --days 30 --iterations 1` was run directly. This surfaced a real, pre-existing bug predating this issue (from the Issue #23 follow-up): `ame_wait_and_board()` (`R/trajectories.R`) uses a manual `timeout()`/`rollback()` polling loop rather than simmer's native queueing, so a waiting casualty never registers in the `"ame"`/`"ame_critical"` resources' own queue tracking — that column is structurally always 0, and the extracted `plot_ame_queue()` (and, it follows, the pre-existing `ame_backlog_plot` it came from) always rendered a flat zero line regardless of the true backlog. Fixed with a new `compute_ame_backlog()` that reconstructs the real backlog from per-casualty `r2e_departure_time`/`ame_departure_time`/`ame_route` event timestamps instead. `compute_ame_sorties()`'s `seats_used` had the same class of bug (an instantaneous pre-sortie queue snapshot, also always 0, since an arriving casualty seizes freed capacity immediately with no queueing step); fixed by measuring the resource's `server`-count change across each sortie's window (to the next scheduled sortie, or end of run for the last one) instead. The corrected backlog's peak values (critical pool 89, standard pool cyclical to 9) match figures already documented in the README's "Strategic Evacuation and Role 4 Demand" section, confirming the fix rather than introducing a new finding — that section's prose had always been derived correctly from `ame_wait_time_summary`, which is how the plot bug went unnoticed since Issue #23 merged.

**Seed-42 baseline (30 days, single run):** Unaffected — this PR only adds/corrects post-simulation analysis and visualisation functions in `R/analysis.R`; no `env_data.json` or trajectory-logic change. Confirmed via direct re-run: total casualties 386, matching `CLAUDE.md`'s documented post-Issue-23 baseline exactly. `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue currently carries `status: blocked`.

### Issue 121 — Graphs in Shiny App Exceed Available Window Space Instead of Scaling to Fit ✓

**Merged:** PR #123, branch `claude/121-0vhwjl`

Every Analyse-tab plot output (Casualty Flow, Queue Depths, Bed & Resource Utilisation, Waiting Times, Force Regeneration, plus the Sensitivity Calibration tab's Morris/Sobol/Transport Sweep plots) is now wrapped in a "shrink-to-fit" container (`shrink_to_fit_script()`/`bch_shrink_to_fit_css()`, `app.R`): a client-side script shrinks the container's CSS height to fit the current viewport minus a fixed chrome allowance, while `renderPlot()` keeps an explicit natural height (never `"auto"`) so Shiny never re-renders in response to the container's displayed size; a paired stylesheet makes the plot image track that height (`height: 100%`, `width: auto`, `margin: 0 auto`), so the browser scales the already-rendered, full-detail image down losslessly — like a photograph, not a redraw — and centres it. An "Expand to full size" link opens the same plot, unscaled, in a modal for dense plots. Two earlier designs were tried and dropped after live testing surfaced real defects, not just style preferences: leaving `renderPlot()` on auto-sizing so Shiny's own redraw filled a JS-shrunk container reintroduced label/row overlap once ggplot's fixed font sizes occupied a larger fraction of a heavily-shrunk canvas; a CSS `transform: scale()` on a full-size render caused Shiny's resize-sensing (which measures a container's on-screen, post-transform size) to eventually re-render at the already-shrunk width while the transform stayed active, compounding into a progressively narrower, uncentred image.

Two owner-directed follow-ups within the same PR extended scope beyond the original issue. First, Queue Depths (`r1_queues`/`r2b_bed_queues`/`r2e_bed_queues`) and Quick Run's Bed & Resource Utilisation (`r2b_treatment`/`r2b_gantt`/`r2e_surgery`/`r2e_gantt`) were each one combined `patchwork` image, meaning every constituent panel shrank to fit one shared budget rather than getting its own; both were split into separate plots, each with its own heading, PNG/PDF/CSV download buttons (CSVs filtered by the same per-echelon resource-name patterns `analyse_run()`/`analyse_replications()` use internally, so each panel's download matches its own image), and Expand link. A "group" mechanism apportioning one shared viewport budget across a split-out set of panels was implemented first, then reverted once it was established that a page scrolling *between* independently-sized panels is an acceptable trade a single panel exceeding the viewport is not — each panel now sizes against the full viewport budget independently. Full Analysis mode's single mean ± CI bar chart is unaffected, since it has no per-bed Gantt panels to split. Second, R2B Treatment — itself a 3-panel `patchwork` stack (casualties treated / surgeries started / casualties skipping R2B), the same shape as the Casualty Flow tab — was found sharing R2E Surgery's plain single-panel 400px height convention rather than Casualty Flow's 700px, leaving too little room per stacked panel and causing each sub-panel's "Casualties" y-axis title to overlap its neighbour's; fixed by giving R2B Treatment the same 700px as Casualty Flow.

Verified live rather than by code review alone, across all four commits: R 4.3.3 plus every package `app.R` and its sourced `R/*.R` files load were installed ad hoc in-session via `renv::restore()` against the project's own `renv.lock` (no Docker daemon available for the pinned `rocker/rstudio:4.4.2` Dev Container), and the app was driven end-to-end with Playwright/headless Chromium. Confirmed: no page-level scrolling needed to view any single plot's default state; correct shrink-and-centre behaviour at a narrow (700–900px) viewport, with plots rendering at full natural size (no shrinking, no distortion) at a tall (1400px) viewport; the Expand modal renders every plot at full natural size with no row/label overlap — the direct regression check for Issue #111 remaining intact under this issue's shrinking; the previously-selected Analyse sub-tab survives a window resize (verifying the deliberately client-side, non-reactive design); Full Analysis mode's single-plot tabs are unaffected by the Quick Run split; and each split panel's CSV download contains only that panel's own filtered resource subset. Screenshots taken during verification showed every R2B/R2E Gantt bed row fully legible at default size, without needing to Expand — a substantial legibility improvement over both the pre-#121 fixed-height container and the shared-budget "group" design tried mid-PR.

**Seed-42 baseline (30 days, single run):** Unaffected — this is a Shiny rendering-only fix; no `env_data.json` or trajectory-logic change. `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue currently carries `status: blocked`.

### Issue 111 — Bed Resource Usage Gantt Chart Rows Overlap Due to Insufficient Vertical Space ✓

**Merged:** PR #120, branch `claude/issue-111-7jaeib`

The Shiny app's "Bed & Resource Utilisation" tab (Quick Run mode) rendered the combined treatment/Gantt/surgery/Gantt composite plot inside a fixed `plotOutput(height = "1400px")`. As the number of individual bed resources in the R2B and R2E per-bed Gantt charts grew — the default `env_data.json` config alone has 40 R2E bed slots (ot=2, resus=4, icu=4, hold=30) across a single unfaceted panel — the fixed container height squeezed each Gantt row into less vertical space than `geom_segment(linewidth = 6)` needs, causing resource row labels and bars to overlap and become indistinguishable. A new `utilisation_plot_height()` reactive (`app.R`) computes the required height from the number of distinct resource rows in `res$r2b_gantt$data` (summed per R2B team facet, since `facet_wrap` gives each team equal panel height regardless of its own bed count) and `res$r2e_gantt$data` (not faceted), at 25px per row with a 150px floor per section; the two non-Gantt panels keep a fixed 400px allowance each. Full Analysis mode's single mean ± 95% CI bar chart (no per-bed Gantt) keeps a static 500px height, unaffected by the change. The same reactive drives both the `plotOutput` container height and the `renderPlot(..., height = function() ...)` call, so the rendered image and its container stay in sync.

Verified live rather than by code review alone: R 4.3.3 plus every package `app.R` and its sourced `R/*.R` files load were installed ad hoc in-session (no Docker daemon available for the pinned `rocker/rstudio:4.4.2` Dev Container), and the app was driven end-to-end with Playwright/headless Chromium. Quick Run at default `env_data.json` bed quantities (seed 42, 10-day run) computed a height of 1675px (vs. the old fixed 1400px), with the rendered `<img>`'s natural height matching exactly; a screenshot confirmed every R2B (2 teams × 10 bed rows) and R2E (18 of 40 possible bed rows occupied) Gantt row was distinct and legible. As a before/after regression check, the pre-fix `app.R` was run from a separate copy at the identical seed/config: the old code rendered at a fixed 1400px with row labels (e.g. "RESUS Bed 2/1", the "HOLD Bed" series) visibly compressed/overlapping — reproducing Issue #111's exact reported symptom, resolved by the new code at the same data. Full Analysis mode (10 replications, 5-day run) was also run end-to-end and confirmed unaffected, rendering its bar chart at the static 500px height.

**Seed-42 baseline (30 days, single run):** Unaffected — this is a Shiny rendering-only fix; no `env_data.json` or trajectory-logic change. `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue currently carries `status: blocked`.

### Issue 110 — Queue-Depth Plots Use Fixed Y-Axis Scale and Clip Data ✓

**Merged:** PR #118, branch `claude/gracious-rubin-e72h3z`

The R2E Heavy Bed Queue Length graph, and two other queue-depth plots in the Quick Run analysis path (`analyse_run()`, `R/analysis.R`), hardcoded `scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1), expand = c(0, 0))`, silently clipping any queue depth above 10 rather than showing the true peak. An audit of every queue-depth plot function found exactly three affected: `p_r1_queues` ("Queue Length Over Time by R1 Team"), `p_r2b_bed_queues` ("Queue Length Over Time by R2B"), and `p_r2e_bed_queues` ("R2E Heavy Bed Queue Length Over Time by Resource Type", the one originally reported). Each was replaced with `scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))`, deriving the upper bound from the plotted data with a 5% margin — the same `limits = c(0, NA)` pattern already used by the existing utilisation plot. The Full Analysis (multi-run, CI ribbon) equivalents (`p_r1_queues_ci`, `p_r2b_queues_ci`, `p_r2e_queues_ci`) never had a hardcoded limit in the first place and needed no change; `app.R`'s `tab_plot()` reads both modes' queue plots under the same list keys, so the fix applies uniformly regardless of run mode. Other fixed-scale plots in the same file (casualty/surgery/bypass daily-count bar charts) were audited and confirmed out of scope — they are not queue-depth plots.

Verified by actually executing the pipeline rather than by code inspection alone: R 4.3.3 plus the required packages (`simmer`, `simmer.bricks`, `ggplot2`, `dplyr`, etc.) were installed ad hoc in-session (no working Docker daemon available to build the pinned `rocker/rstudio:4.4.2` Dev Container), and `run.R --seed 42 --days 30 --iterations 1` was run directly. Console KPIs matched the documented seed-42 baseline exactly (see below), confirming no regression, and the rendered `images/r2eheavy_bed_queue_3_teams.png` showed the R2E ICU queue climbing past 20 by day 30 at this exact baseline — confirming the bug was clipping real, currently-documented output, not just a hypothetical stress-test scenario. The 10-replication Full Analysis path was also run directly and confirmed the CI-ribbon queue plots render correctly with no regression. All test-run artifacts (`data/arrivals_*.txt`, `images/*.png`, `logs/logs.txt`, `outputs/*`) were discarded after inspection so the merged PR carries only the `R/analysis.R` source change.

**Seed-42 baseline (30 days, single run):** Unaffected — this PR changes only three `scale_y_continuous()` calls; no `env_data.json` or trajectory-logic change, and no data preparation/filtering code was touched. Confirmed via direct re-run: total casualties 386, R2B pre-bypass 115, R2B OT bypass 72 (63 off-shift / 9 busy), post-op pathway icu=4/hold=104, Role 4 demand 40 reached via AME (peak occupancy 19.0, day 22) — an exact match to `CLAUDE.md`'s documented baseline. `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue currently carries `status: blocked`.

### Issue 23 — Strategic Evacuation Demand: Role 4 and AME Sorties ✓

**Merged:** PR #107, branch `claude/issue-23-tylbin`

Patients reaching the strategic evacuation decision (`r2e_evac = 1`) previously disappeared from the simulation with no downstream output. This closes the outbound half of the causal chain Issue #18 opened on the inbound side: `injury_type`/`evacuation_decision_day`/`treatment_received` are now captured at the evacuation branch, feeding `compute_role4_census()` (post-simulation, unconstrained Role 4 bed-occupancy-by-ward demand signal) and `compute_ame_demand()` (unconstrained-baseline AME sortie demand) in `R/analysis.R`, against new `vars.role4` length-of-stay parameters in `env_data.json`.

Four owner-directed follow-ups within the same PR substantially extended the original scope. First, strategic AME itself became a real constrained simmer resource — scheduled, capacity-limited, failure-prone, seized only from R2E, with casualties continuing to occupy R2E beds until actually evacuated (a fixed-capacity-per-sortie bug caught during verification — boarded casualties never release the resource, so a fixed rather than additive capacity setting permanently capped total-ever-admitted — was fixed with `set_capacity(..., mod = "+")`). Second, research into AJP-4.10(B) found direct doctrinal evidence that a single undifferentiated AME pool overstated ICU contention: a Casualty Staging Unit holds "already stabilized patients," with critical care an augmentation "if required," while AE crews are separately augmented by a CCATT/CCAST team "limited by capacity" — this became a two-pool split (`"ame"` standard / `"ame_critical"` critical), then a further redesign to two planner-named "aircraft configurations" (each a fixed critical/standard capacity pair, since a real sortie flies one loadout rather than filling both pools independently), with `select_ame_configuration()` flying whichever configuration minimises total unmet need at each scheduled opportunity. Third, the default sortie interval was changed from 3 to 7 days to match the cadence `force_regeneration.reinforcement` (Issue #18) is designed around. Fourth, and most significant structurally: casualties queued awaiting AME were the one population in the model facing zero DOW risk regardless of wait duration, since the R2E post-operative DOW check is the model's last fixed checkpoint and the AME wait itself is unbounded — `ame_dow_poll()` closes this using the same conditional-increment logistic formula as every other checkpoint, applied periodically via the same `timeout()`-then-`rollback()` polling pattern already used for R2E OT-ICU gating deferral (new `dow_echelon = 5`).

All four follow-ups are exposed in the Shiny Configure panel (a new "Strategic AME" subgroup: sortie interval, cancellation probability, two configurations' capacities, DOW poll interval) and on the Medevac Chain sidebar diagram (a new AME node with configuration-labelled legs). Two implementation bugs were caught and fixed during the DOW-poll follow-up specifically: new trajectory-building helper functions were initially inserted mid-way through an ongoing magrittr `%>%` chain, which silently mis-parsed as "pipe the result into an assignment" and produced an opaque `target of assignment expands to non-language object` error at trajectory-build time; and a `rollback()` loop target was initially a numeric offset (fragile — the correct value depends on the death branch's total activity count, not just the loop's own steps) before being switched to a named tag, verified safe for reuse across both AME pools and all R2E teams via isolated structural and functional tests before integration.

**Seed-42 baseline (30 days, single run, final PR state):** Not RNG-stream-neutral across any of the four follow-ups (AME wait duration changes R2E bed-release timing; the sortie generator and DOW poll each consume new `runif()` draws). Total casualties 386; 133 strategic evacuation decisions (97 critical-route, 36 standard-route); 40 boarded by day 30 (8 critical, 32 standard), 93 still queued; critical-pool mean wait 12.8 days (p10–p90 5.9–19.6), standard-pool mean wait 2.1 days (p10–p90 0.0–4.0); Role 4 peak occupancy 19.0 concurrent patients (day 22); 1 death recorded at the new `ame_wait` DOW checkpoint (standard route). `CLAUDE.md`'s Key Parameters table has been refreshed for this merge (see the Issue #23 provenance caveat there).

**Unblocked by this merge:** No new issues unblocked — no open issue currently carries `status: blocked`, and no remaining backlog item (`#4`, `#40`) lists Issue #23 as a dependency.

### Issue 18 — Endogenous Casualty Generation (Force Regeneration Feedback Loop) ✓

**Merged:** PR #105, branch `claude/issue-18-y6x96r`

Casualty arrival rate was a fixed exogenous input applied to a static force size; `in_theatre_rate` had no causal pathway to any arrival-rate or resource-load metric, its apparent Morris/Sobol influence (Issue #3) reflecting only an indirect effect on R2E holding-bed occupancy — a bootstrap artefact, not a genuine mechanism. This closes that loop: casualty arrival rate for all six background streams now scales against a live, time-varying `effective_force_combat`/`effective_force_support` simmer global, debited by 1 at every casualty's `injury_time` and credited by 1 at every RTD event (`debit_force_size()`/`credit_rtd()`, `R/trajectories.R`), giving `in_theatre_rate`'s existing in-theatre-recovery branch a real mechanical effect for the first time. Because the force-size global can only be known by actually running the simulation, this required replacing the previous batch/`at()` arrival generation (all 30 days' timestamps pre-computed before `run()` starts) with stateful, force-size-reactive generator closures (`make_ln_arrival_generator()`/`make_exp_arrival_generator()`, `R/environment.R`) that walk minute-by-minute and read the live global at each step — verified directly, including the trickiest part of the rewrite, the mass-casualty interleave into the `wia_cbt` stream (`wrap_with_mass_casualty()`), which stays exogenous/pre-computed since it represents an imposed shock rather than a population-scaled rate.

A reinforcement mechanism was added and then substantially redesigned during review. The initial implementation added a fixed periodic size (`interval_days`/`combat_size`/`support_size`); per owner feedback this was replaced with a demand/fulfillment model closer to a real reinforcement pipeline: a configurable `demand_interval_days` cycle, at which each pool submits a demand equal to its *actual current shortfall* against establishment strength (not a fixed size); a configurable `fulfillment_lag_days` delay; and a delivered amount drawn from a `Triangular(fill_min_frac, fill_mode_frac, fill_max_frac)` distribution over the fraction of demand actually met (shipped defaults 0.2/0.85/1.1 — a long left tail toward under-fill, ~15% chance of <50% fill, and a short right tail limiting over-supply to ~4.5% chance, capped at 1.1×). Because demand is the pool's real shortfall rather than a fixed size, the mechanism is self-limiting — a well-sustained pool automatically asks for less on its next cycle — which produces a materially different and more realistic demonstration result than the fixed-size version: at `high_intensity` rates, the redesigned mechanism reduces the no-reinforcement decline (slope −0.204/day, p=9.6×10⁻¹⁴) by an order of magnitude to a slope statistically indistinguishable from flat (−0.018/day, p=0.27), rather than overshooting into net growth as the fixed-size version had. Ships disabled by default (`demand_interval_days = 0`), confirmed byte-identical to the pre-redesign baseline with no RNG draws consumed when disabled.

`analyse_run()` and `analyse_replications()` (`R/analysis.R`) both gained a `force_regeneration_plot` — effective force size vs. simulation day, a mean ± 95% CI ribbon in the multi-run case — wired into a new "Force Regeneration" tab in the Shiny Analyse panel (PNG/PDF/CSV download, matching every other tab) and a five-field "Reinforcement Demand & Fulfillment" Configure subgroup (`R/app_params.R`); naming the three fill fields `fill_min`/`fill_mode`/`fill_max` happened to match the Configure panel's existing triangular-distribution auto-detection, rendering a live preview widget with no new UI code. All of this — Quick Run, Full Analysis, and the Configure fields — was verified end-to-end against a live running Shiny instance via headless-browser testing, not just reasoned about; this caught two real bugs before merge: `analyse_replications()` initially had no force-regeneration logic at all (Full Analysis showed a blank tab), and a key-naming mismatch (`force_regeneration` vs. `force_regeneration_plot`) left the multi-run plot blank with no error even after the logic was added, found by extracting and inspecting the actual base64 PNG Shiny was serving rather than trusting a blank screenshot.

README gained a new [Force Regeneration and the Endogenous Feedback Loop](../README.md#6-force-regeneration-and-the-endogenous-feedback-loop) subsection (mechanism, formula, five MODEL ASSUMPTION blocks covering the two-pool design, continuous vs. daily-poll crediting, no extra echelon-weighted RTD delay, mass-casualty exogeneity, and the demand/lag/triangular-fill design), a Simulation Analysis demonstration table (`moderate_intensity`/`high_intensity`, with/without reinforcement, OLS trend fit), and Limitation L10 marked resolved. `CLAUDE.md`'s Key Parameters table was refreshed for every RNG-stream-shifting row this issue touches, with an explicit unpinned-sandbox provenance caveat (no Docker access in the development environment) — several already-pending rows (per-room OT, per-bed ICU, per-vehicle transport, 50-rep multi-run figures) were left as last refreshed rather than guessed at, and per-room/per-bed/per-vehicle granularity was likewise not independently re-derived in this refresh.

**Seed-42 baseline (30 days, single run):** RNG-stream-shifting in an unusually structural way (unlike prior RNG-stream-shifting merges, which changed values drawn from an unchanged draw *order*, this one changes the draw order itself — arrival-rate draws are now interleaved with trajectory-duration draws in event time rather than 100% front-loaded before `run()` starts). Total casualties 400→386; WIA 154→149; KIA 70→67; DNBI 176→170; DOW count unchanged (0); DNBI sub-types battle_fatigue/disease/nbi 38/99/39→47/99/24; `total_rtd` 148→142; R2E first-op surgeries 142→124; R2E post-op pathway icu/hold 110/31→110/14. New: `effective_force_combat`/`effective_force_support` end the run at 2330/2500 and 1176/1250 with no reinforcement (the shipped default). Full detail, including rows explicitly flagged as not recomputed in this refresh, is in `CLAUDE.md`'s Key Parameters table.

**Unblocked by this merge:** Issue #23 (Role 4 occupancy and AME sortie demand) — its dependencies (#1, #22, #18) are now all merged; label updated `status: blocked` → `status: ready`.

### Issue 57 — Transport Fleet-Size Capacity Margin Sweep ✓

**Merged:** PR #103, branch `claude/issue-57-q3b1h2`

Replaces the `plot_transport_capacity_margin_by_fleet_size()` stub (scaffolded during the Issue #6 PR, #56, and blocked on Issue #10) with a working sweep: for each PMV Ambulance (1–5) and HX240M (1–4) fleet size, deep-copies the parsed `env_data.json`, overwrites that vehicle's `transports[[]]$qty` (the other vehicle type held at its current establishment size), rebuilds via `build_environment()`, and runs `run_replications()` (`R/replication.R`) — the same replication engine the comparative scenario runner (Issue #10) uses, reused directly rather than duplicated. A new `transport_rep_kpis()` helper extracts per-replication mean queue/utilisation, aggregated to a mean and t-distribution 95% CI at each sweep point. Results write to `outputs/transport_capacity_by_fleet_size.csv` and `images/transport_capacity_margin_by_fleet_size.png`; a new `scripts/run_transport_sweep.R` CLI entry point mirrors the `scripts/run_scenarios.R` convention. README's Transport Fleet Capacity Margin section replaces its STUB callout with the real results (a table plus interpretation), and Further Development now points at Vietnam/Okinawa-rate and mass-casualty re-sweeps as the next step, since this sweep only varies fleet size at the Falklands-derived casualty rate.

A follow-up commit, made at the owner's request within the same PR, integrates the sweep into the Shiny app's Sensitivity Calibration tab alongside the existing Morris/Sobol controls — beyond Issue #57's original task list, which only asked for the analysis function, CSV/PNG outputs, and README. `plot_transport_capacity_margin_by_fleet_size()` gained `progress_dir`/`max_cores` parameters (mirroring `run_morris()`) for real per-sweep-point progress polling; plot-building was factored into a new `render_transport_sweep_plot()` so the CLI/README path and the Shiny app render from the identical `ggplot2` specification rather than two maintained copies; `scripts/shiny_worker.R` gained a `transport_sweep` subprocess mode, dispatched the same way Full Analysis/Morris/Sobol already are (`run_replications()`'s `mclapply` forking is unsafe nested directly inside a `future()` body — see Issue #15's entry above); and `app.R` adds two range sliders, a run button, a live progress counter, the rendered plot with a current-establishment reference line read from the Configure panel, and a CSV download. Three further owner-requested UX passes refined the result: the panel's description text was rewritten from developer-facing roxygen-style prose to a plain statement of what question the sweep answers and why a planner would run it; the plot gained human-readable facet titles ("PMV Ambulance" rather than the raw `env_data.json` name "PMVAmb") and an explanatory legend for the ribbon/mean-line/reference-line, which previously relied on prose alone; the row (metric) labels were moved from `facet_grid`'s default right-hand placement to the left, where they read as a conventional y-axis title; and the "how to read this" interpretation text was moved to sit above the plot rather than below it, shortened to one succinct instruction. Verified against a live app instance (Playwright driving headless Chromium against the actual running app, not simulated): Quick Run → Analyse → Sensitivity Calibration → set sweep ranges → Run Transport Fleet Sweep completed with real progress updates (0 of 3 → 3 of 3) and a correctly rendered plot.

**Seed-42 baseline (30 days, single run):** Unaffected — no `env_data.json` or trajectory-logic change; the sweep function only reads a deep copy of the parsed JSON to vary `transports[[]]$qty` in-memory per sweep point, restoring the global `env_data`/`day_min`/`counts` to their pre-call values on completion. At fleet size 1, both PMV Ambulance (mean queue 0.0395, 95% CI [0.0339, 0.0452]) and HX240M (mean queue 0.0038, 95% CI [0.0020, 0.0056]) show a materially non-zero mean queue, confirming the sweep locates a genuine capacity boundary; queue collapses to a negligible fraction of a casualty by two vehicles for both platforms and stays there through the current three/four-vehicle establishment and beyond (seed 42, 10 replications x 30 days).

**Unblocked by this merge:** No new issues unblocked — no open issue lists Issue #57 as a dependency.

### Issue 77 — Configure Panel Eager-Render Race ✓

**Merged:** PR #101, branch `feature/issue-77-defer-configure-panel-render`

Every Configure accordion panel — plus its ~19 curve-preview plots and two sticky-sidebar diagrams — was forced to render eagerly (`suspendWhenHidden = FALSE`, an Issue #14 decision) regardless of which panel was actually open, so every field's value stayed capturable via `input` no matter which panels a planner had visited. This carried two costs, only the first of which the original issue anticipated: an ~18–20 second initial page load as all ~110 fields and 19 curve plots rendered and bound at once, and a race — dozens of widgets simultaneously reporting their own bind-time initial value to the server could arrive after, and silently overwrite, an edit made in the first several seconds after load or a Casualty Intensity Profile switch, with no error or visual indication. Reverted the `outputOptions()` overrides on the five `group_ui_*` panel outputs, the 19 curve previews, and the two diagrams back to Shiny's default (`suspendWhenHidden = TRUE`); a panel's fields now bind only once it is actually opened. `apply_registry_values()` already fell back to `scenario_json()`'s default for any field missing from `values`, so Quick Run/Full Analysis/Morris/Sobol/Save Configuration (all built from `current_json()`) were already safe under deferred rendering; `validate_config()`, however, runs on the flat `values` list *before* that fallback merge and had no null-guard on its Force Size checks, so a never-opened panel's fields (e.g. team counts, which live in "Health System Architecture", not the default-open "Force Size" panel) surfaced as a spurious validation error — closed by a new `fill_missing_defaults()` helper applied ahead of `validate_config()` at all three call sites.

Also closes Issue #98, raised independently against the same underlying mechanism but reached via a slider-paired field (`wire_slider_text_sync()`) rather than a plain numeric one; #98's own proposed fix (PR #100, routing the slider update through `session$sendCustomMessage()` instead of `updateSliderInput()`) was investigated, found not to address the actual defect (both delivery paths showed the identical revert-then-recover timing once tested against a live app under real WebSocket inspection), and closed as a duplicate once #77's true mechanism was confirmed as the shared root cause.

Verified live (R 4.3.3 + this repo's pinned `renv.lock` package versions, headless Chromium driving the actual app over its WebSocket connection — not simulated): startup quiescence dropped from ~18–20s/~196 frames to ~4.2s/~21 frames; the exact silent-revert race reproduced 2/2 times before the fix (typing into a Configure-panel slider immediately after page load showed the value apply, then spontaneously revert ~10s later, then get corrected by the edit's own delayed round-trip ~1s after that) did not reproduce in 2/2 runs after the fix; a regression the deferred-rendering approach itself introduced was caught in testing and fixed before merge — Quick Run with every non-default panel left closed failed validation on the closed panels' missing team-count fields until `fill_missing_defaults()` was added.

**Seed-42 baseline (30 days, single run):** Unaffected — this PR changes only Shiny reactive-output configuration (`outputOptions`) and adds one pure helper function used solely by the Shiny app's own validation path; no `env_data.json` or trajectory-logic change. CLAUDE.md's Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue lists Issue #77 as a dependency.

### Issue 15 — Shiny App: Full Analysis Mode and Sensitivity Screening ✓

**Merged:** PR #97, branch `claude/issue-15-iitarp`

Activates the two modes Issue #14 left as disabled placeholders. Full Analysis (`R/replication.R`'s `run_replications()`, `n_reps` slider 10–1000, default 100) reports mean ± 95% CI on four KPI summary cards and renders `analyse_replications()`'s ribbon/band variants of all four result tabs, following Williams et al. (2020)'s replication-count and CI-reporting guidance. Sensitivity Screening exposes Morris elementary-effects screening and, on completion, Sobol variance decomposition on the top 5 μ*-ranked parameters, both with real per-design-point progress counters, reusing `R/sensitivity.R`'s existing `run_morris()`/`run_sobol()`.

Making this work reliably under real deployment — rather than just passing a local smoke test — required substantially more than wiring the two modes into the UI, found through live testing on both a local Docker Desktop dev container and a GitHub Codespace: (1) `detect_safe_cores()` (`app.R`) bounds `mclapply`'s concurrency from the container's own real-time cgroup memory headroom rather than trusting `parallel::detectCores()`, recomputed fresh immediately before each run rather than cached at app startup; (2) `run_replications()`/`run_morris()`/`run_sobol()` gained `max_cores` caps, killed-worker detection, and (Morris/Sobol) an inter-iteration `gc()` to stop cumulative memory growth from swap-thrashing a long sequential screen; (3) a genuine, previously-latent bug — `simmer::select()` permanently masking `dplyr::select()` for the whole `app.R` session, because `library(dplyr)` at the top of `app.R` runs before `source("R/environment.R")` first attaches `simmer` — was found and fixed by qualifying every bare `select()`/`first()` in `R/analysis.R`, the reason `analyse_run()`'s CLI path (`run.R`, which attaches the two packages in the opposite order) never hit it; (4) most significantly, Full Analysis/Morris/Sobol no longer call `run_replications()`/`run_morris()`/`run_sobol()` in-process inside a `future()` body at all — both `future` backends proved unsafe for a future body that itself calls `mclapply()`: `multisession`'s worker is a separate process reached over a control socket that a grandchild forked from inside it can desynchronise (observed as "Future ... interrupted" with no OOM or R-level cause), and `multicore` forks the Shiny process directly, including its live `httpuv` event loop, risking a forked child permanently deadlocked on a lock held by a thread that does not exist in the fork (confirmed via `ps` — a forked worker accumulating 4 seconds of CPU time over several minutes of wall clock). All three now shell out via a new `run_shiny_worker()`/`scripts/shiny_worker.R` (`system2()` — a genuine fork-then-exec, replacing the child's memory image entirely rather than duplicating Shiny's process state) to the same code path `run.R`/`scripts/run_sensitivity.R` already run successfully with full `mclapply` parallelism from a plain process. `.devcontainer/devcontainer.json` also gained `hostRequirements` (sized to the account's actual available Codespaces machine tier) and a second forwarded port for the Shiny app alongside RStudio Server's.

**Unblocked by this merge:** No new issues unblocked — no open issue lists Issue #15 as a dependency.

### Issue 9 — Mass Casualty Stochastic Injection (Compound Poisson Process) ✓

**Merged:** PR #92, branch `feature/issue-9-mass-casualty-injection`

Casualty generation previously produced only a smooth lognormal background rate, with no representation of the discrete tactical events — artillery barrages, drone strikes, VBIED detonations — that generate acute casualty surges in LSCO. `generate_mass_casualty_events()` (`R/environment.R`) now overlays these as a compound Poisson process: Poisson-distributed event inter-arrival (default mean 5 days), Uniform(20, 60) casualties per event, Triangular(60, 120, 180)-minute injection window, with a blast-dominant 70/20/10 P1/P2/P3 triage split. Arrivals from either mode merge into the background `wia_cbt` stream and are tagged with a `mass_casualty_event` attribute for post-hoc analysis.

A second event-timing mode was added during development (owner feedback): `mass_casualty.event.mode = "scheduled"` lets a planner specify a fixed set of simulation days directly, each with its own independent occurrence probability, casualty-count bounds (min/max), and triage priority split — rather than only an inferred Poisson rate. The Configure panel exposes both modes in a new "Mass Casualty" accordion panel (`app.R`/`R/app_params.R`): a mode dropdown, conditionally-shown Poisson-rate vs. scheduled-day fields, a shared injection-window field with a live triangular curve, and — per further owner feedback — a dynamically-sized list of scheduled event cards (capped at 20 slots, `+`/`−` buttons show/hide rows) with each event independently configurable rather than sharing the Poisson-mode casualty-count/priority fields. Row visibility toggles client-side via a custom Shiny message rather than a server-triggered re-render, since re-rendering the whole panel group on every `+`/`−` click was discarding the user's live mode selection and other in-progress edits; a later follow-up fix also replaced a `layout_column_wrap()` grid (which reserved fixed row height for all 20 slots regardless of how many were visible, leaving a large dead-space gap) with a plain flexbox-wrap container that collapses to fit only the visible cards.

The "MASCAL" acronym used throughout the issue and initial implementation was renamed to "mass casualty" per owner feedback, retained only where it is a cited source's own article title (README reference [48]).

Adds a mass casualty event timeline plot and DOW-rate-by-origin analysis to `R/analysis.R`, wires the Poisson mode's event rate/size parameters into the Morris sensitivity screen (`R/sensitivity.R`), and documents both modes with two MODEL ASSUMPTION blocks and stress-test results in the README (Casualty Generation, Simulation Analysis, Limitations L7 resolved, References [47]/[48]).

**Seed-42 baseline (30 days, single run):** Unaffected — ships disabled by default (`mode = "poisson"`, `rate_per_day = 0`, empty schedule), confirmed identical to the documented `CLAUDE.md` baseline (400 total casualties, R2B pre-bypass 115, OT bypass 73/5, post-op pathway hold=31/icu=110) both before merge and re-verified against the merged `main` tip.

**Unblocked by this merge:** No new issues unblocked — no open issue lists Issue #9 as a dependency.

### Issue 93 — Dev Container Build Failure: Missing `libuv` Runtime + `renv.lock` R Version Mismatch ✓

**Merged:** PR #94, branch `claude/dev-container-startup-error-wona44`

A contributor's `devcontainer up` failed shortly after the Issue #72 merge. Initial investigation (before a full build log was available) found and corrected a real but non-fatal discrepancy: `renv.lock` recorded R `4.3.3` — the sandbox `r-base` version used to generate it during Issue #72, which had no Docker daemon available to test against the real Dev Container — rather than the Dockerfile's pinned `4.4.2`; `renv::snapshot()` under a real R 4.4.2 environment confirmed zero package version drift, so only the `R.Version` metadata field needed correcting. The reporter's fuller build log then surfaced the actual cause: `renv::restore()` aborted installing `fs`, whose prebuilt Posit Package Manager binary dynamically links `libuv.so.1` at runtime — a library `rocker/rstudio:4.4.2` (Ubuntu 24.04 "noble") does not ship by default and the Dockerfile never requested. Confirmed via `ldd` on the cached `fs.so` binary, identified `libuv1t64` as the package providing it on noble (plain `libuv1` has no installable candidate there), and swept every other `.so` in a full 116-package restore for unresolved shared-library dependencies (none found). `.devcontainer/Dockerfile` now installs `libuv1t64` and `curl` (the latter fixing a separate warning in the same log: "curl does not appear to be installed; downloads will fail").

**Seed-42 baseline (30 days, single run):** Unaffected — metadata/system-library changes only; no R package version pins changed and no `env_data.json` or trajectory-logic change.

**Unblocked by this merge:** No new issues unblocked — this restores the Dev Container to a working state (blocking all R-dependent verification per CLAUDE.md) rather than unblocking a specific downstream issue.

### Issue 72 — Adopt `renv` for Reproducible R Package Dependency Pinning ✓

**Merged:** PR #91, branch `claude/issue-72-cmfu9i`

R package versions were previously unpinned anywhere in the repository, risking silent version drift between contributors' machines and the committed baseline (concretely surfaced during Issue #14 as a missing `DT` package after a routine pull). `renv::init()` captured every package required by `run.R`, `app.R`, `R/*.R`, and `scripts/*.R` into a newly committed `renv.lock` (116 packages), replacing the Dockerfile's ad hoc `install.packages()` list with `renv::restore()` so the Dev Container and any host RStudio installation restore from the same lockfile. The Dockerfile now pre-warms `renv`'s global package cache from the committed lockfile during image build so `postCreateCommand`'s `renv::restore()` resolves from cache rather than rebuilding from source on every container start. The lockfile was generated in a sandbox without Docker access (recording R `4.3.3` instead of the Dockerfile's pinned `4.4.2`), a gap resolved in the Issue #93 follow-up above.

**Seed-42 baseline (30 days, single run):** Unaffected — dependency-management infrastructure only; no `env_data.json` or trajectory-logic change.

**Unblocked by this merge:** No new issues unblocked.

### Issue 76 — R2B/R2E Surgery Duration Narrative Diverges from Shipped `env_data.json` ✓

**Merged:** PR #89, branch `claude/issue-76-llpshj`

The README's R2B/R2E Heavy Trajectory narrative described the DAMCON surgery duration as a triangular distribution with `min=41, max=210, mode=95` minutes, citing DCS meta-analyses, while the shipped `env_data.json` configured `min=90, max=240, mode=120` minutes for both `r2b.surgery` and `r2eheavy.surgery` — an unsourced placeholder present since the project's initial commit and never reconciled with the later-researched narrative text (README Limitation L14, found during Issue #14). Investigation traced the narrative's 41/210/95 figures directly to first-look DCS operative-time data (median 96 minutes, range 41–210) reported for Sohn et al.'s (2018) cohort within Zizzo et al.'s (2020) systematic review, confirming the narrative — not the shipped config — was the correctly-sourced target. `env_data.json` was updated to match, the README citation was tightened to the specific Sohn/Zizzo data point, Limitation L14 was marked resolved, and the Shiny Configure panel tooltip's discrepancy flag (Issue #14) was removed. The seed-42 baseline was re-run in the project's actual pinned Dev Container (`rocker/rstudio:4.4.2`) — validated first against the pre-fix config, which reproduced every previously documented post-Issue-73 figure exactly — and `data/arrivals_*.txt`, `logs/logs.txt`, and `images/*.png` were regenerated and committed against the corrected config for the first time in that container (the Issue #73 follow-up refresh had skipped this, using an unpinned sandbox instead). A pre-existing, unrelated documentation inaccuracy was also corrected: `CLAUDE.md`'s "R2B bypass count ... (upstream R1 threshold: 50 + at-R2B OT-check bypass: 82)" parenthetical had presented two independent, non-summing counts as addends of a combined total.

**Seed-42 baseline (30 days, single run):** RNG-stream-shifting (same class as Issues #5, #6, #43, #73) — total/WIA/KIA/DNBI unchanged (400/154/70/176; casualty generation precedes all trajectory execution and is unaffected), but everything downstream of the new `rtriangle()` draws shifted: DOW count 3→0; DNBI sub-types battle_fatigue/disease/nbi 46/110/20→38/99/39; `total_rtd` 134→148; R2B surgical candidates 132→141; R2B surgeries 47→62; R2E first-op surgeries 132→142; R2B upstream bypass 132→115; R2B OT bypass (team off-shift/OT busy) 76/6→73/5; R2E post-op pathway icu/hold 108/24→110/31; R2E OT-ICU surgery deferred 10→13. Full per-resource OT/ICU/transport utilisation deltas are in `CLAUDE.md`'s Key Parameters table.

**Unblocked by this merge:** No new issues unblocked — no open issue lists #76 as a blocker.

### Issue 85 — `check_env_data_summary.R` Crashes on R1's Integer `sub_elm` Value ✓

**Merged:** PR #87, branch `claude/next-issue-hzwefy`

`scripts/check_env_data_summary.R` — the script that regenerates the ENV SUMMARY block in `README.md` and, per `CLAUDE.md`'s Post-Merge Checklist, is meant to be run whenever `env_data.json` changes — crashed under `purrr` releases where `map_chr()` treats mixing an integer with strings as a hard error rather than a deprecation warning, because `elms[[r1]].sub_elms[[1]].sub_elm` is the integer `1` while every other element's `sub_elm` (`r2b`, `r2eheavy`) is a string (`"surg"`, `"emerg"`, `"icu"`, `"evac"`). Discovered during the Issue #74 post-merge checklist (PR #84), where the regeneration step had to be skipped as a documented no-op.

Resolved per the issue's preferred Option 1 (script hardening, no `env_data.json` schema change): a new `resolve_sub_elm()` helper treats any non-character `sub_elm` — R1's placeholder integer included — the same as a missing one, since R1 has no named sub-team split in the trajectory code (it seizes technician/clinician resources directly rather than via a named sub-team). This also fixed a second defect the crash had been masking: with the prior `%||%` fallback bypassed by a non-`NULL` integer, R1's medical-resources column was rendering a bare `"1"` table header in README's ENV SUMMARY block instead of `"Base"` — confirmed via before/after diff to be the only change the fix produces.

R was not present in this session's base sandbox; `r-base-core`/`r-cran-jsonlite`/`r-cran-purrr`/`r-cran-stringr` were installed ad hoc via apt (R 4.3.3, purrr 1.0.2) to verify the fix. That `purrr` release only emits a deprecation warning for the integer/string mix rather than the hard error the issue reports, so the crash itself wasn't directly reproducible in-session — but the warning text is purrr's own recommended fix and matches what `resolve_sub_elm()` does, and the masked header defect was reproduced and fixed regardless. A Dev Container (`rocker/rstudio:4.4.2`) confirmation of the exact crash remains outstanding, noted as a known limitation on the PR.

**Seed-42 baseline (30 days, single run):** Unchanged — no `env_data.json`, trajectory, or simulation-logic changes; this PR touches only `scripts/check_env_data_summary.R` and the auto-generated ENV SUMMARY block in `README.md`. `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue lists #85 as a blocker.

### Issue 74 — Remove the Dead-Heading Return Leg Multiplier ✓

**Merged:** PR #83, branch `claude/issue-74-kpag0s`

The issue proposed removing `return_leg_multiplier` (`env_data.json`, `R/trajectories.R`, `R/sensitivity.R`, the Shiny Configure panel) on the grounds that it had no doctrinal basis for departing from its default of 1.0 and that Morris screening had found it inert. Between the issue being raised and implementation starting, PR #81 (Issue #73) extended the parameter's scope from two dead-heading legs to four and re-ran Morris screening, finding it had become the most influential of all ten screened parameters on mean transport utilisation and had the largest σ on total DOW count — the opposite of "inert." A comment was added to Issue #74 flagging that its original "no detectable effect" rationale no longer held.

This was raised with the repository owner before implementation, since removal now contradicted the project's own most recent sensitivity finding. Per owner direction, removal proceeded anyway on operational rather than sensitivity-based grounds: a dead-heading vehicle's rate of march is not doctrinally differentiated by payload (Fischer et al., 2025 [44]), so there is no real-world scenario the multiplier could legitimately represent by departing from 1.0, regardless of how influential it measures in screening. All four dead-heading return legs (R1↔R2B WIA/KIA, R2B↔R2E WIA/KIA-mortuary) now draw an unconditional fresh triangular sample from the same outbound distribution, with no configurable multiplier. `README.md` was updated throughout (Dead-Heading Return Legs narrative, removed MODEL ASSUMPTION block, Sensitivity Analysis reduced from ten to nine screened parameters with the removed row struck rather than silently deleted, Limitations L6/L15, a Further Development follow-up note, and the Shiny Medevac Chain diagram description) to document this reasoning explicitly rather than erasing the prior finding. The Shiny Configure-panel sliders for the four `return_leg_multiplier` fields were removed and verified visually (screenshotted against a live app instance) rather than by code inspection alone.

**Seed-42 baseline (30 days, single run):** Unchanged — confirmed RNG-stream-neutral by running seed-42/30-day baselines both immediately before and after the change (via a `git worktree`) in the same sandbox and diffing every `outputs/` file: byte-identical except `mon_arrivals.csv`, which is identical once sorted (only the row order of still-"ongoing" entities differs, a known `simmer` end-of-run monitoring artifact, not an RNG effect). `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — nothing in the dependency graph lists #74 as a blocker. Issue #73's remaining scope (R2B→R2E fleet-sharing design) is unaffected by this change.

### Issue 73 — R2B → R2E WIA Dead-Heading Return Leg Configured but Never Applied ✓

**Merged:** PR #81, branch `claude/next-issue-jundsp`

The issue documented a dead-code discrepancy found during Issue #14: `r2b_transport_wia()` read `r2b.wia_transport.return_leg_multiplier` and seized the shared PMVAmb fleet, but was never called from any trajectory — the R2B → R2E WIA evacuation code actually in use (all sub-paths of `r2b_treat_wia()`'s Step 5) seized each R2B team's own organic `evac` resource instead and modelled no return trip. The issue offered two resolutions without prejudging which: wire the dead function in (shared fleet), or formalise the organic-asset design and remove the dead code.

Resolution went through three stages in the same PR, the middle two driven by direct owner follow-up after the first stage shipped:

1. **Removed the dead code.** `r2b_transport_wia()` and the (at that point unread) `r2b.wia_transport.return_leg_multiplier` field were deleted, formalising R2B → R2E WIA transport as a deliberate organic-asset design distinct from the R1 ↔ R2B pooled fleet — Approach 2 from the issue.
2. **Reversed/extended per owner direction.** A dead-heading return leg was added to that *same* organic resource instead of leaving it absent (`r2b_evac_leg()`/`r2b_evac_return_leg()`, new helpers in `R/trajectories.R`), restoring `return_leg_multiplier` to `r2b.wia_transport`. A related gap was addressed in the same pass: R2B's KIA/DOW mortuary handling had modelled the mortuary as local to R2B with no travel time, but the mortuary is modelled as collocated with R2E, not R2B (new MODEL ASSUMPTION block, README) — R2B has no organic mortuary asset. `r2b_transport_kia()` now road-moves KIA/DOW casualties to R2E via the shared HX2 40M fleet (dead-heading return leg, mirroring `r1_transport_kia()`), handing off to a selected R2E team's own mortuary intake (new `r2e_mortuary_intake()` helper). The Medevac Chain diagram (`app.R`) was reworked to draw all four transport legs with working return-leg labels, mortuary marker rendering only at R2E.
3. **Verified rather than asserted the sensitivity impact.** The PR's own description initially claimed, without running it, that doubling `return_leg_multiplier`'s scope (two legs → four) wouldn't materially change its Morris ranking, reasoning from its low rank on system OT queue alone. Prompted to actually check this, a full Morris re-run (r=20, 5 reps, 30 days, seed 42) found the claim **wrong** for 2 of the screen's 7 tracked KPIs: `return_leg_multiplier` stays low-ranked (9th of 10) on system OT queue and the other queue-based KPIs, but is now the **most influential of all 10 parameters on mean transport utilisation** (µ*≈0.092, ~18% above the next-highest) and has the **largest σ of any parameter on total DOW count** (≈7.4, 2nd-highest µ*≈5.6) — DOW count being the model's core mortality outcome. README's Sensitivity Analysis section, ranking table, and Limitation L15 were corrected to reflect the verified finding rather than the wrong assertion; the regenerated Morris plots were committed. A comment was added to Issue #74 (which proposes removing `return_leg_multiplier` entirely, citing "no detectable effect on any tracked KPI") flagging that its central premise no longer holds given this finding.

**Seed-42 baseline (30 days, single run):** total casualties/WIA/KIA/DNBI unchanged (400/154/70/176) — casualty generation happens before any treatment-phase code this PR touches. Everything downstream of the new `rtriangle()` draws shifts (same RNG-stream-shift class as Issue #6): DOW count 0→3; `bf_rtd`/`clinical_rtd`/`total_rtd` 37/105/142→39/95/134; R2B surgeries 53→47; R2E first-op surgeries 134→132; R2B bypass 124→132 (upstream 47→50, at-R2B 77→82); post-op pathway hold=23/icu=110→hold=24/icu=108. See `CLAUDE.md`'s Key Parameters table for the full set.

**Environment caveat:** the refreshed baseline and Morris re-run were captured in an ad-hoc sandbox R 4.3.3 install without the project's `renv` lockfile (Issue #72) or Dev Container. A direct A/B check (re-running the already-merged pre-PR code in the same sandbox) confirmed individual arrival *times* diverge from the committed `data/arrivals_*.txt` baseline independent of this PR's changes — an environment artifact, not a code effect — so `data/arrivals_*.txt` and `logs/logs.txt` were deliberately **not** regenerated/committed, to avoid baking sandbox-specific noise into tracked artifacts. A Dev Container re-run to confirm exact figures remains outstanding.

**Unblocked by this merge:** No new issues unblocked — nothing in the dependency graph lists #73 as a blocker. Issue #74's premise was found to be partially outdated (see comment on #74); recommend re-scoping or closing it rather than implementing as originally written.

### Issue 75 — Stale Morris Screening Bounds for `p1_p_max` ✓

**Merged:** PR #79, branch `claude/next-issue-discussion-5ym7y9`

`morris_params` (`R/sensitivity.R`) screened `p1_p_max` (the Priority 1 DOW ceiling) over 0.25–0.75, predating Issue #5's Falklands recalibration of the shipped baseline to 0.023 — the screening bounds were never updated when the baseline moved by roughly an order of magnitude. Because `morris_params$mode` also supplies the fixed value held for `p1_p_max` while every *other* parameter is screened, the stale 60% mode had been silently biasing every parameter's µ*/σ ranking, not just `p1_p_max`'s own. Re-derived the bounds as a 0.5×–2× multiplicative range around the 0.023 baseline (1.15%–4.6%) — matching the treatment already used for `in_theatre_rate`, the screen's other small-magnitude probability parameter, since a "full clinically plausible range" treatment (the original derivation) doesn't make sense for a parameter this small — then re-ran the full Morris screen (r=20, 5 reps, 30 days, seed 42) against the current codebase (post Issues #7/#5/#6/#43) and published the corrected ranking in the README, replacing a screen that predated all four of those merges. `app.R`'s Configure panel slider for this field derives its range from `morris_params` automatically, so no app-side fix was needed beyond updating a stale code comment.

Two smaller gaps were also closed in the same PR while re-publishing the Sensitivity Analysis section: `images/morris_transport_q.png`/`morris_transport_util.png` — generated by `run_morris()` ever since Issue #6 added the two transport KPIs, but never committed or referenced in the README — are now embedded; and a stale `r*(p+1)` evaluation-count code comment (200, dating from before a tenth parameter was added) was corrected to 220.

A follow-on UI enhancement was requested during review of this fix: a live DOW survival-function curve (`render_dow_curve()`, drawing directly from `dow_prob()` in `R/trajectories.R`) was added to the Shiny Configure panel's Died of Wounds Ceilings fields, so the P1/P2 DOW Ceiling sliders show the actual shifted-logistic F(t) curve they parameterise rather than a bare percentage. The curve's fixed shape constants (`p_base`, `k`, `t_mid` — not Configure fields) are shown alongside as read-only boxes (`readonly_numeric()`), sourced live from the resolved scenario JSON.

**Seed-42 baseline (30 days, single run):** Unchanged — this PR touches only the Morris screening design table (`R/sensitivity.R`) and Shiny UI code, never `env_data.json` or trajectory logic; confirmed via an identical pre/post `Rscript run.R --seed 42` run (400 total arrivals, 154 WIA, 70 KIA, 176 DNBI, R2B OT bypass 67/10, post-op pathway hold=23/icu=110). CLAUDE.md's Key Parameters table does not require updating.

**Morris re-run (r=20, 5 reps, 30 days, seed 42):** `p1_p_max` now ranks 3rd by µ* (0.0179) on the system OT queue; all ten parameters fall within a 2.4× µ* range with no single dominant parameter, and every parameter's σ/µ* exceeds 0.9, consistent with the branching, resource-contingent routing this simulation models. `return_leg_multiplier` ranks last, corroborating the Sobol finding from the Issue #6 PR discussion that transport queue is near-constant-zero across the screened range under the current fleet size. See the updated table in README [Sensitivity Analysis](../README.md#sensitivity-analysis).

**Unblocked by this merge:** No new issues unblocked — nothing in the dependency graph lists #75 as a blocker.

### Issue 14 — Shiny App: Parameter Editor, Quick Run Mode, and Single-Run Output Display ✓

**Merged:** PR #71, branch `claude/next-issue-ydx5qt`

Replaces `controller.R`'s raw `env_data.json` field editor with `app.R`, a Configure → Run → Analyse Shiny console (`controller.R` retained as `controller_legacy.R`). The Configure panel groups all ~110 editable fields into five operational panels (Force Size, Health System Architecture, Medevac, Health Provision, Casualty Rates), each field rendered as a bordered card with a plain-English label, a source-citation tooltip, and a widget matched to its semantics — Morris-screened and other bounded probability/threshold fields as sliders (each paired with a typeable numeric box), the two sum-to-1 compositional splits (Triage Priority, DNBI Sub-Type) as two-handle range sliders with live recoloured/labelled segments, the R2E ICU-Full Priority Override Threshold as a three-option dropdown, and every triangular min/mode/max duration field as a live density-curve card with compact side-by-side inputs. Health System Architecture and Medevac each carry a sticky-sidebar SVG diagram — a force-structure node graph/bed table, and a Medevac Chain diagram tracing the actual PMVAmb/HX240M/R2B-evac-team transport legs modelled in `R/trajectories.R`, including the two distinct R2B-bypass mechanisms. The Run panel executes Quick Run (single replication) asynchronously via `future`/`promises`; Analyse renders `analyse_run()`'s four result tabs plus a read-only Sensitivity Calibration tab. `R/analysis.R::analyse_run()` was refactored to return its plots as named ggplot objects instead of printing them, the prerequisite that unblocks this issue and Issue #15; `run.R` reproduces the prior print order via a new `print_analysis_plots()` helper.

Three bugs were found and fixed during implementation: a lazy-argument-evaluation closure bug that silently corrupted 24 loop-built fields to all read/write the same (last-iteration) `env_data.json` path; a `future` worker-bootstrap race against `simmer`'s `run()` S3 method; and a pixel-rounding imprecision in the compositional split sliders' inline value labels. Three further pre-existing gaps were found (not introduced) and documented rather than fixed, each now with its own tracking issue: stale Morris screening bounds for `p1_p_max` (README L13, Issue #75), a divergence between the R2B/R2E surgery-duration narrative text and the shipped `env_data.json` configuration (README L14, Issue #76), and the R2B→R2E WIA dead-heading return leg being configured but never applied by any active code path (README L15, Issue #73). A fourth pre-existing gap — Configure-panel edits or scenario switches made within ~15–20s of load or a prior switch can be silently reverted by a late-arriving initial-value websocket message, a consequence of eagerly rendering every accordion panel so every field stays capturable regardless of which panel is open — is tracked as Issue #77.

**Seed-42 baseline (30 days, single run):** Unchanged — `R/analysis.R`'s refactor to return ggplot objects instead of printing them is verified byte-identical in CSV/PNG output at seed 42; no `env_data.json` or trajectory-logic change was made. CLAUDE.md's Key Parameters table does not require updating.

**Unblocked by this merge:** Issue #15 (Shiny Full Analysis mode) — its dependencies (#14, #1, #2, #3) are now all merged; label updated `status: blocked` → `status: ready`.

---

### Issue 10 — Comparative Scenario Runner ✓

**Merged:** PR #69, branch `claude/next-issue-gi4q2n`

Adds `run_scenario()` / `compare_scenarios()` (new `R/scenario_runner.R`) and a CLI entry point (`scripts/run_scenarios.R`) that execute the existing multi-replication framework (`run_replications()`/`summarise_replications()`, Issue #1) under a named `env_data.json` scenario profile (`load_scenario()`/`resolve_scenario()`, Issue #54), aggregating queue and mortality KPIs across replications in the project's standard mean (p10–p90), 95% CI format. `run_scenario()` also reports `wia_count` and `dow_rate` (DOW as a proportion of WIA, matching the existing "DOW/WIA rate" convention) alongside `total_casualties`. `compare_scenarios()` combines results across scenarios, writes `outputs/scenario_comparison_queues.csv` / `outputs/scenario_comparison_totals.csv`, and renders a faceted comparison plot (`images/scenario_comparison.png`) grouping queue KPIs by R2B OT, R2E OT, R2E ICU, and Transport.

**Scope revision:** the issue as originally raised cited Vietnam/Okinawa WIA/KIA figures that Issue #54 subsequently found do not exist in the FORECAS source document (Table A.5 is Vietnam DNBI only). This PR compares the two profiles Issue #54 actually shipped — `moderate_intensity` (Falklands 1982) and `high_intensity` (Okinawa, demonstration skeleton) — rather than fabricating a Vietnam profile with uncited numbers; this was flagged on the issue before work started.

A follow-up commit on the same PR added a "Comparative Scenario Runner" reference subsection under README's Codebase Structure (matching the pattern used for the replication/warm-up/sensitivity tooling), which had been missed in the initial commit — the results were documented under Simulation Analysis but the how-to-call-it reference was not.

**10-replication × 30-day comparison (seed 42):**

| Metric | `moderate_intensity` (Falklands) | `high_intensity` (Okinawa) | Ratio |
|---|---|---|---|
| Total casualties/run | 399.8 | 1082.1 | 2.71× |
| DOW/WIA rate | 0.260% | 1.078% | 4.15× |
| R2E OT mean queue | 0.049 | 37.82 | 773× |
| R2E ICU mean queue | 0.045 | 3.156 | 69× |
| R2B OT mean queue | 0.000 | 0.000 | — |

**Seed-42 baseline (30 days, single run):** Unchanged — no changes to `Battlefield Casualty Handling.R` or `env_data.json`; `moderate_intensity` total casualties (399.8 mean at 10 reps) match the documented default baseline (400) within 0.05%. CLAUDE.md's Key Parameters table does not require updating.

**Known limitations (documented in README L8/L12):** No Vietnam-intensity comparison exists — no genuinely FORECAS-sourced Vietnam combat-troop WIA/KIA table could be identified. `high_intensity` remains an unvalidated demonstration skeleton per Issue #54 (only casualty generation rate/distribution family sourced for Okinawa), so the queue/DOW increases reported are a lower bound on Okinawa-intensity system stress, not a fully validated clinical model.

**Unblocked by this merge:** Issue #57 (fleet-size capacity margin sweep) — its only hard blocker was #10; label updated `status: blocked` → `status: ready`. Issues #23 and #15 also reference #10-adjacent work in their surrounding context but list #1/#22/#18 and #14/#1/#2/#3 respectively as their actual blockers, not #10 — neither is unblocked by this merge.

### Issue 54 — Scenario-Level Parameter Profiles for Historical Conflict Calibration ✓

**Merged:** PR #67, branch `claude/next-issue-selection-43d2ju`

Introduces a named scenario profile overlay mechanism: a top-level `scenarios` block in `env_data.json`, `merge_scenario_vars()`/`resolve_scenario()` (new `R/scenario.R`), and `load_scenario(path, scenario)` (`R/environment.R`), which overlays only scenario-eligible `vars` (casualty generation, DOW ceiling/treatment efficacy, priority/DNBI/surgery/evac probabilities, transport times) onto the base configuration — structural config (`elms`, `transports`, `pops`) is never overridden. `scenario = "default"` (or omitting the argument, as every existing entry point does) is a verified no-op.

Implements two profiles, named for FORECAS's own battle-intensity framing rather than by conflict:
- **`moderate_intensity`** (Falklands 1982 exemplar) — disentangles the pre-existing co-dependence between the Falklands-calibrated DOW ceiling and the OIF/OEF-era treatment efficacy factors the base configuration had been using. Era-appropriate treatment efficacy factors are paired with an independently re-calibrated ceiling, validated by 50-replication Monte Carlo against the historical 0.52% DOW/WIA target (result: 0.480%, 95% CI [0.323%, 0.638%]).
- **`high_intensity`** (Okinawa exemplar, demonstration skeleton, not a fully validated second scenario) — required fetching and reading the actual FORECAS report (Blood, Zouris & Rotblatt, 1998) directly rather than assuming its methodology, which surfaced two corrections: (1) FORECAS fits an **exponential** distribution (not lognormal) to combat-troop WIA/KIA at high intensity, parameterised by its mean alone (`W ~ exponential(mean)`) — added `generate_exp_arrivals()` and a `generate_casualty_arrivals()` dispatcher keyed off a new `distribution` field per generator; (2) the paper's actual Appendix A tables gave different values than initially assumed (Table A.7 Okinawa combat WIA is `Expon(6.86)`, Table A.9 combat KIA is `Expon(1.63)`), and there is **no standalone Vietnam combat-troop WIA/KIA table** in the source document (Table A.5 is Vietnam DNBI only) — a `vietnam` profile was dropped rather than citing a table that doesn't contain those numbers. **This directly affects Issue #10's own body, which currently cites Vietnam FORECAS Table A.5 and specific WIA/KIA numbers that do not appear in the source document — see the note added to Issue #10.**

A secondary defect was found and fixed during this same PR: the per-minute rate cap (`cap = 5`, an undocumented engineering constant with no citation, present since the casualty generator's first commit) truncated a wildly uneven, mean-dependent share of draws once applied to the new exponential streams — ~48% for `high_intensity` WIA (mean 6.86, *above* the fixed cap) vs. ~1–7% for the existing lognormal streams. `generate_exp_arrivals()` now computes `cap = cap_multiplier × mean_daily` (default multiplier 3), which truncates a constant ~5% of draws regardless of intensity (since `P(Exponential(mean) > k·mean) = exp(-k)` is mean-invariant). `generate_ln_arrivals()` (used by `default`/`moderate_intensity`) is untouched.

`controller.R` gained a scenario selector for previewing effective parameters per scenario; saving is gated to `default` so the override mechanism can't be accidentally flattened into the base file through the generic editor.

**Seed-42 baseline (30 days, single run):** Unchanged — 400 total casualties, 154 WIA, 70 KIA, 176 DNBI, byte-for-byte identical to the documented baseline. `default` scenario output confirmed structurally identical to the pre-existing `load_elms()` path. No RNG-stream-affecting change to the base configuration; CLAUDE.md's Key Parameters table does not require updating.

**high_intensity (50-rep, 30 days):** mean WIA/run 732.9, mean KIA/run 173.4 (WIA+KIA ratio 4.05× `moderate_intensity`), mean DOW/run 7.040 (95% CI [6.296, 7.784]), DOW/WIA rate 0.961%.

**Known limitations (documented in README L12):** the `moderate_intensity` KIA:WIA ratio (0.452) differs from the published 255:777 (0.328) record — a pre-existing characteristic of the Issue #1 casualty generator calibration, not introduced or corrected here. `high_intensity` is an explicitly unvalidated skeleton (only casualty generation rate and distribution family are sourced). No Vietnam-calibrated profile exists — none was fabricated in place of a genuine source.

**Unblocked by this merge:** Issue #10 (comparative scenario runner) — its own body already listed #54 as a hard dependency (schema ownership) alongside #1/#2/#5/#8, all of which were already merged. Issue #10's `status: ready` label was already set (it had been set before #54's dependency was fully satisfied, an inconsistency this merge now resolves); no label change was required. No other open issue lists #54 as a blocker.

---

### Issue 40 (partial) — R2B OT Bypass Reason Diagnostic ✓

**Merged:** PR #64, branch `claude/next-issue-selection-v6kxjh`

Implements the diagnostic portion of Issue #40 only. `r2b_treat_wia()`'s OT-unavailable bypass branch (`R/trajectories.R`) now sets `r2b_bypass_reason` (1 = surgical team off-shift, 2 = OT bed busy or queued) and `r2b_bypass_time`, decomposing the previously undifferentiated at-R2B bypass count. `R/analysis.R` gained the matching decomposition (`r2b_ot_bypass_offshift_count`, `r2b_ot_bypass_busy_count`, `r2b_ot_bypass_count`) and a stacked bar chart of mean bypasses per simulation day by reason (`images/r2b_ot_bypass_reason.png`), following the `r2b_hold_daily` (Issue #39) replication-averaging convention.

**Scope decision:** Scenario A (extended `ot_hours`) and Scenario B (second surgical team per R2B unit) — the two intervention tests in the original issue — were deliberately **not** implemented. Extended-hours throughput gains can't be meaningfully assessed without a clinician fatigue/error-rate model, which the simulation doesn't represent; reporting them without that counterweight would overstate the intervention's net benefit. A second team is an establishment-size decision for planners, not a parameter to test unilaterally. Issue #40 remains open for this remaining scope.

**Seed-42 baseline (30 days, single run):** Of 77 at-R2B OT-check bypasses (subset of the existing combined 124 `r2b_bypassed` count — 47 upstream R1-threshold + 77 at-R2B), **67 (87%) are attributable to the surgical team's 12-hour shift window**, and only **10 (13%) to OT bed congestion** — confirming the shift window, not physical OT capacity, as the dominant constraint on forward surgical throughput at R2B. Total casualties (400), WIA (154), KIA (70), and DNBI (176) all unchanged; no RNG-stream-affecting change was made.

**A genuine bug was found and fixed during this PR's own review cycle:** the first version of the daily chart summed bypass counts across replications instead of averaging per replication first (matching `r2b_hold_daily`'s convention), which would have scaled the chart ~N× under multi-run mode (`--iterations > 1`) and silently mis-rendered rather than erroring. Caught by testing the multi-replication path before merge; fixed and re-verified (single-run baseline unaffected; multi-rep now correctly scaled) in the same PR.

**Unblocked by this merge:** No new issues unblocked — nothing in the dependency graph lists #40 as a blocker, and #40 itself remains `status: ready` for its remaining Scenario A/B scope.

---

### Issue 60 — `qty: 0` Silently Creates One Resource Instead of Zero ✓

**Merged:** PR #62, branch `claude/next-issue-4m3w6o`

`build_environment()` (`R/environment.R`) constructed bed and transport fleet resource IDs with `paste0("...", seq_len(x$qty), "...")`. R's `paste0()` default recycling rule treats a zero-length `seq_len(0)` argument as `""` rather than propagating zero length to the result, so a bed or transport `qty` of `0` in `env_data.json` silently produced **one** degenerate resource (e.g. `"b_r2eheavy_icu__t1"`) instead of the intended zero, with `add_resource()` then defaulting its capacity to 1. Both vectorised call sites (bed ID construction and transport fleet ID construction) now guard `qty > 0`, returning `character(0)` otherwise. The personnel/sub-element resource loops elsewhere in the file use `for (j in seq_len(res$qty))` — an actual for-loop, which already iterates zero times correctly for `qty: 0` — so no change was needed there; this was confirmed by audit rather than by symptom.

This was a latent defect with no currently-exercised code path: no entry in the shipped `env_data.json` sets a bed or resource `qty` to `0`, so it had no effect on any documented baseline result. It was discovered during Issue #43 testing (noted in that issue's merge entry below) when a script attempting to model a zero-capacity ICU for a stress test found the resource still present. Fixing it restores `qty: 0` as a reliable mechanism for modelling a capability gap (a degraded-establishment scenario, or a parameter-sweep boundary case for Issue #10's comparative scenario runner or Issue #57's fleet-size sweep).

**Verification:** ran `build_environment()` against the shipped `env_data.json` before and after the fix — identical 169 total resource IDs, confirming no behavioural change to the shipped baseline. Against a modified copy with `r2eheavy`'s `icu` bed and the `PMVAmb` transport both set to `qty: 0`, the fix correctly resolved both to `character(0)` (vs. one degenerate ID each pre-fix), while an unaffected `hold_bed` resource on the same instance was unchanged. A full seed-42, 30-day A/B run of `run.R` (pre-fix vs. post-fix) produced identical output — 400 total arrivals, identical R2B routing and R2E OT–ICU gating counts, and byte-identical `mon_arrivals.csv` content (row order differs only due to pre-existing, unrelated nondeterministic write ordering).

**Unblocked by this merge:** No new issues unblocked — Issue #57's only hard blocker is Issue #10, and Issues #54/#23 depend on #9/#10/#18, not #60.

---

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
| ~~Vietnam~~ | ~~4.12~~ | ~~6.89~~ | ~~FORECAS Table A.5~~ |
| Okinawa | 8.40 | 11.20 | FORECAS Table A.2 |

**Correction (Issue #54, PR #67):** the table above was the original plan, but Issue #54 fetched and read the actual FORECAS report to implement this schema and found neither the Vietnam nor the Okinawa row is accurate as stated. FORECAS's Appendix A has no standalone Vietnam combat-troop WIA/KIA distribution table at all — Table A.5 is Vietnam combat-troop **DNBI**, not WIA/KIA — so the Vietnam row cannot be sourced from this document and has been dropped rather than fabricated. Okinawa's real fitted values (Table A.7 WIA, Table A.9 KIA, both **exponential**, not lognormal) are `Expon(6.86)` and `Expon(1.63)` respectively — materially different from the 8.40/11.20 figures above. The schema Issue #54 built supports both lognormal and exponential distribution families (a `distribution` field per generator), which any future Vietnam-equivalent profile will need if a genuine source is found. See the `moderate_intensity`/`high_intensity` scenario profiles in `env_data.json` for the corrected implementation.

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

### Phase 1 — Statistical Foundation (Issues 1 ✓, 22 ✓, 2 ✓, 3 ✓, 24 ✓, 75 ✓)
*Estimated effort: 3–4 weeks. All subsequent analyses depend on this foundation. **Complete.***

1. ~~Multi-replication wrapper (`mclapply` + `wrap()`) — **Merged PR #16**~~
2. ~~**Issue 22** — Define Output Variable Register; add five missing timing attributes to trajectories. **Merged PR #26**~~
3. ~~**Issue 24** — Switch to L'Ecuyer-CMRG RNG streams, add antithetic variates, set explicit `mc.cores`. **Merged PR #32**~~
4. ~~**Issue 2** — Welch warm-up analysis; set `warm_up_period` constant. **Merged PR #20**~~
5. ~~**Issue 3** — Morris Elementary Effects screening using the OVR KPIs from Issue 22. **Merged PR #30**~~
6. ~~**Issue 75** — Re-derive stale `p1_p_max` Morris screening bounds (predated Issue #5's recalibration) and re-run the full Morris screen. **Merged PR #79**~~

### Phase 2 — Model Fidelity (Issues 8 ✓, 35 ✓, 37 ✓, 44 ✓, 6 ✓, 5 ✓, 43 ✓, 14 ✓)
*Estimated effort: 2–3 weeks. Low-to-medium code changes, high impact on result validity.*

6. ~~**Issue 8** — Fix R2E surgical team seizure (three lines; do first). **Merged.**~~
7. ~~**Issue 35** — Fix R2B OT bypass check (`<=` → `< && queue == 0`). **Merged PR #36.**~~
8. ~~**Issue 37** — Remove 12h schedule from OT bed resources; add team-availability bypass check. **Merged PR #38.**~~
9. ~~**Issue 44** — RTD KPI annotation: decomposed `total_rtd` into `bf_rtd` + `clinical_rtd`, added `rtd_type` column to `rtd_by_echelon`, two `stopifnot()` guards, seed-42 baseline documented. **Merged PR #47.**~~
10. ~~**Issue 6** — Dead-heading return legs for transport assets.~~ — **Merged PR #56.**
11. ~~**Issue 5** — Time-dependent DOW survival function.~~ — **Merged PR #53.**
12. ~~**Issue 43** — OT–ICU gating: implement three-way pre-OT branch (ICU available / ICU full + P1 / ICU full + P2+).~~ — **Merged PR #59.**
13. ~~**Issue 14** — Shiny app parameter editor and Quick Run mode. Requires `R/analysis.R` refactor returning ggplot objects (Issue 1 dependency already satisfied).~~ — **Merged PR #71.**

### Phase 3 — Structural Refactoring (Issues 7 ✓, 39 ✓, 60 ✓, 4 backlog, 40 partial ✓ / backlog)
*Estimated effort: 4–5 weeks. Requires `env_data.json` schema changes, trajectory rewrites, and hold-bed decomposition.*

10. ~~**Issue 7** — DNBI sub-category routing~~ — **Merged PR #34.** Prerequisite for Issue #39 satisfied.
11. ~~**Issue 39** — R2B hold bed saturation analysis~~ — **Merged PR #48.** Two-tier routing policy (upstream threshold + at-R2B three-stage branch) implemented; per-stream occupancy decomposition added to analysis pipeline.
12. ~~**Issue 60** — Guard zero-length `seq_len(qty)` in `build_environment()`'s bed and transport ID construction so `qty: 0` produces zero resources instead of one.~~ — **Merged PR #62.**
13. **Issue 40** — R2B OT utilisation improvement. ~~Add `r2b_bypass_reason` attribute~~ — **Merged PR #64** (67 off-shift, 10 OT busy/queued, of 77 at-R2B bypasses). Remaining scope: scenario-test `ot_hours` at 12/14/16/20h; evaluate second surgical team option (partial result without Issue #4) — both deferred pending a clinician fatigue model (Scenario A) and a directed establishment-size decision (Scenario B). **Backlog** — not currently planned; unblocked but on hold pending that design work.
14. **Issue 4** — Individual resource seizure. Read `BCH_Task_Role_Allocation.md` in full before beginning. Gated until Issues 1, 2, and 3 are all stable (satisfied). Address the six validation assumptions in `BCH_Task_Role_Allocation.md` Part 5 — document each as a named model assumption in the README, and include the two highest-priority assumptions (NO flex to surgical roles; second-surgeon probability) in the Morris screening from Phase 1. **Backlog** — unblocked but deprioritised given its size/risk; may not be resourced in the near term.

### Phase 4 — Scenario Expansion (Issues 9 ✓, 10 ✓, 57 ✓, 18 ✓, 23 ✓)
*Estimated effort: 3–4 weeks. Builds on Phase 1–3 outputs. **Complete.***

11a. ~~**Issue 54** — Scenario-level parameter profiles (schema, `load_scenario()`, `moderate_intensity`/`high_intensity` — prerequisite for Issue 10).~~ — **Merged PR #67.**
12. ~~**Issue 9** — Compound Poisson mass casualty injection overlay. Requires Issues 1, 2, 5.~~ — **Merged PR #92.**
13. ~~**Issue 10** — Comparative scenario runner (`run_scenario()`/`compare_scenarios()`, `R/scenario_runner.R`; scope revised to compare `moderate_intensity`/`high_intensity` rather than the uncited Vietnam/Okinawa figures originally in the issue body).~~ — **Merged PR #69.**
13a. ~~**Issue 57** — Fleet-size capacity margin sweep for transport assets (`plot_transport_capacity_margin_by_fleet_size()`; Shiny Sensitivity Calibration panel integration added at owner request within the same PR). Requires Issue 10.~~ — **Merged PR #103.**
14. ~~**Issue 18** — Endogenous casualty generation (force regeneration feedback). Requires Issues 1, 2, 22.~~ — **Merged PR #105.**
15. ~~**Issue 23** — Role 4 occupancy and AME sortie demand. Requires Issues 1, 22, 18.~~ — **Merged PR #107.**

### Phase 5 — Interface (Issues 15 ✓, 77 ✓, 110 ✓, 111 ✓, 121 ✓, 109 ✓)
*Estimated effort: 1–2 weeks.*

16. ~~**Issue 15** — Shiny Full Analysis mode (multi-run CI, sensitivity panel). Requires Issues 14, 1, 2, 3.~~ — **Merged PR #97.**
17. ~~**Issue 77** — Defer Configure panel rendering (`suspendWhenHidden = TRUE`) to eliminate the eager-render race that could silently revert edits made within ~15–20s of load or a scenario switch. Also closes the duplicate Issue #98.~~ — **Merged PR #101.**
18. ~~**Issue 110** — Replace fixed y-axis limits on queue-depth plots (R1, R2B, R2E Heavy Bed) with dynamic `scale_y_continuous(limits = c(0, NA), expand = expansion(...))` scaling so peak queue depth is never clipped.~~ — **Merged PR #118.**
19. ~~**Issue 111** — Scale the Bed & Resource Utilisation Gantt chart's rendered height to its distinct resource-row count so R2B/R2E bed rows no longer overlap at a fixed container height.~~ — **Merged PR #120.**
20. ~~**Issue 121** — Shrink-to-fit convention for every Analyse-tab plot output, with an Expand-to-full-size modal; split Queue Depths and Quick Run Bed & Resource Utilisation out of their combined patchwork images into individually-sized panels. Requires Issue 111.~~ — **Merged PR #123.**
21. ~~**Issue 109** — Add AME sortie timeline and queue-depth (backlog) visualisation to the Shiny Analyse tab, reconstructed from the `"ame"`/`"ame_critical"` resource monitor and per-casualty event timestamps since Issue #23 introduced the underlying mechanism with no accompanying graph.~~ — **Merged PR #126.**

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
  #60  qty:0 resource guard — build_environment() bed/transport ID construction
       fix (PR #62)
  #40  (partial) bypass-reason diagnostic — r2b_bypass_reason/r2b_bypass_time,
       daily chart (PR #64); Scenario A/B remain — see UNBLOCKED below
  #54  Scenario-level parameter profiles — scenarios schema, load_scenario(),
       moderate_intensity + high_intensity, generate_exp_arrivals() (PR #67)
  #10  Comparative scenario runner — run_scenario()/compare_scenarios()
       (R/scenario_runner.R), scripts/run_scenarios.R (PR #69)
  #14  Shiny app — parameter editor, Quick Run mode, single-run output
       display (R/analysis.R ggplot-object refactor, app.R) (PR #71)
  #75  Stale p1_p_max Morris screening bounds — re-derived bounds, full
       Morris re-run, DOW survival-curve Configure panel card (PR #79)
  #73  R2B <-> R2E dead-heading return leg (r2b_evac_leg()/
       r2b_evac_return_leg()) + real R2B->R2E road-move mortuary transport
       for KIA/DOW (r2b_transport_kia(), r2e_mortuary_intake()); verified
       Morris re-run found return_leg_multiplier now top-ranked on
       transport utilisation and near-top on DOW count (PR #81)
  #74  Removed return_leg_multiplier entirely (env_data.json,
       R/trajectories.R, R/sensitivity.R, Configure panel); RNG-neutral,
       removed on operational grounds despite #73's finding that it was
       influential in Morris screening (PR #83)
  #85  check_env_data_summary.R crash fix — resolve_sub_elm() hardening
       for R1's integer sub_elm; also fixed a masked "1" vs "Base"
       ENV SUMMARY header defect (PR #87)
  #76  R2B/R2E surgery duration reconciled to 41/210/95 (Sohn et al. 2018 /
       Zizzo et al. 2020), matching the README narrative; env_data.json
       previously shipped an unsourced 90/240/120 placeholder (PR #89)
  #72  Adopt renv for reproducible R package dependency pinning —
       renv.lock (116 packages), Dockerfile renv::restore() cache
       pre-warm (PR #91)
  #93  Dev Container build failure fix — renv.lock R version corrected
       4.3.3 -> 4.4.2; libuv1t64 + curl added to Dockerfile, fixing the
       real `fs` package dyn.load() failure (PR #94)
  #9   Mass casualty stochastic injection — compound Poisson process
       (generate_mass_casualty_events(), R/environment.R) plus a second
       scheduled-day mode with per-event casualty-count/priority fields;
       Configure panel "Mass Casualty" accordion panel (dynamic +/- event
       list, capped at 20 slots) (PR #92)
  #15  Shiny app — Full Analysis mode (multi-run CI) and Sensitivity
       Screening (Morris/Sobol); Full Analysis/Morris/Sobol dispatch to a
       subprocess (scripts/shiny_worker.R) rather than calling
       run_replications()/run_morris()/run_sobol() in-process inside a
       future() body, after live Codespaces testing found both future
       backends unsafe for a future body that itself calls mclapply()
       (PR #97)
  #77  Configure panel eager-render race — reverted group_ui_* panels, 19
       curve previews, and 2 sticky-sidebar diagrams to suspendWhenHidden
       = TRUE (default); added fill_missing_defaults() so validate_config()
       still sees correct defaults for never-opened panels; verified live
       via headless-browser WebSocket testing (startup ~18-20s -> ~4.2s;
       silent-revert race no longer reproduces). Also closes duplicate #98
       (PR #101)
  #57  Fleet-size capacity margin sweep for transport assets —
       plot_transport_capacity_margin_by_fleet_size() implemented
       (R/analysis.R), plus a Shiny Sensitivity Calibration panel
       integration added at owner request within the same PR
       (scripts/shiny_worker.R transport_sweep mode, app.R panel);
       verified live via Playwright against the running app (PR #103)
  #18  Endogenous casualty generation (force regeneration feedback) —
       effective_force_combat/effective_force_support live globals,
       debited at injury/credited at RTD (R/trajectories.R), giving
       in_theatre_rate a real mechanical effect; batch/at() arrival
       generation replaced with force-size-reactive generator closures
       (R/environment.R); reinforcement redesigned mid-PR from a fixed
       periodic size to a demand/fulfillment-lag/triangular-fill model
       (self-limiting, long under-fill tail); Shiny Configure/Analyse
       integration (Force Regeneration tab, Reinforcement Demand &
       Fulfillment fields); verified live via headless-browser testing,
       catching two real bugs (missing multi-run plot logic, a key-
       naming mismatch) before merge (PR #105)
  #23  Role 4 occupancy and AME sortie demand — compute_role4_census()/
       compute_ame_demand() (R/analysis.R); strategic AME redesigned
       mid-PR into a real two-pool, two-configuration, scheduled
       constrained simmer resource grounded in AJP-4.10(B) CSU/CCATT-
       CCAST doctrine, with select_ame_configuration() flying whichever
       configuration minimises total unmet need; sortie interval
       defaulted to 7 days to match the reinforcement cadence; a
       periodic wait-time DOW poll (ame_dow_poll(), dow_echelon = 5)
       closes the model's only previously-unbounded risk-free wait;
       Shiny Strategic AME Configure subgroup and Medevac Chain diagram
       AME node/legs added (PR #107)
  #110 Queue-depth plot fixed y-axis scale — replaced hardcoded
       scale_y_continuous(limits = c(0, 10)) with dynamic
       limits = c(0, NA) + expansion(mult = c(0, 0.05)) on
       p_r1_queues/p_r2b_bed_queues/p_r2e_bed_queues (Quick Run path);
       Full Analysis CI equivalents were already dynamic and needed no
       change; verified by direct execution (seed-42 KPIs matched the
       documented baseline exactly; R2E ICU queue confirmed clipped at
       that exact baseline before the fix) (PR #118)
  #111 Bed & Resource Utilisation Gantt chart row overlap — new
       utilisation_plot_height() reactive (app.R) scales the plot's
       rendered height to the distinct resource-row count in
       r2b_gantt/r2e_gantt (25px/row, 150px floor per section) in place
       of the old fixed 1400px container; Full Analysis mode's static
       500px bar-chart height is unaffected; verified live via
       Playwright/headless Chromium, including a before/after comparison
       against the pre-fix code reproducing the exact reported overlap
       (PR #120)
  #121 Shrink-to-fit convention for every Analyse-tab plot output
       (shrink_to_fit_script()/bch_shrink_to_fit_css(), app.R), with an
       Expand-to-full-size modal per plot; Queue Depths and Quick Run's
       Bed & Resource Utilisation split out of their combined patchwork
       images into individually-sized panels (each sizing against the
       full viewport budget independently, not a shared group budget);
       R2B Treatment given Casualty Flow's 700px height convention
       (it is also a 3-panel patchwork stack) in place of R2E Surgery's
       400px, fixing a y-axis-title overlap; verified live via
       Playwright/headless Chromium across all four commits, including
       the Issue #111 regression check (Expand modal shows every Gantt
       row with no overlap) (PR #123)
  #109 AME sortie timeline and queue-depth visualisation — plot_ame_queue()
       (factored out of analyse_run()'s pre-existing inline block) and new
       compute_ame_sorties()/plot_ame_sortie(), reconstructing every
       scheduled sortie's outcome from the "ame"/"ame_critical" resource
       monitor against the schedule's own deterministic firing times;
       wired into a new "Strategic AME" app.R Analyse tab. Live seed-42
       verification (no Docker daemon available) surfaced and fixed a
       pre-existing bug predating this issue (from Issue #23): a waiting
       casualty never registers in simmer's own queue tracking for these
       two resources (ame_wait_and_board() uses a manual timeout()/
       rollback() poll, not select()/seize_selected()), so the resource
       monitor's queue column is structurally always 0 — the extracted
       plot_ame_queue() always rendered a flat zero backlog regardless of
       the true state. Fixed with compute_ame_backlog(), reconstructing
       the backlog from r2e_departure_time/ame_departure_time/ame_route
       event timestamps instead; compute_ame_sorties()'s seats_used had
       the same class of bug, fixed by measuring the server-count change
       across each sortie's window rather than an instantaneous pre-
       sortie queue snapshot (PR #126)
  #124 Force reinforcement ceiling-clamp fix — credit_fn() now clamps
       to min(initial, current + fill); new reinf_*_pending globals
       (R/replication.R) let demand_fn() net out shortfall already
       claimed by an in-flight cycle, closing the overlapping-cycle
       (demand_interval_days < fulfillment_lag_days) double-claim
       identified in a follow-up comment on top of the original
       unconditional-credit report; verified against the live simmer
       mechanism under an adversarial overlapping-cycle stress
       configuration, including a before/after run confirming the
       identical config overshoots on pre-fix code (PR #129)

IN REVIEW (PRs open against main):
  (none)

UNBLOCKED (start now):
  (none)

BACKLOG (unblocked but deprioritised — not currently planned):
  #4   Individual resource seizure   (gating satisfied: #1 + #2 + #3 all merged;
       parked given its size/risk — largest structural change in the project)
  #40  R2B OT utilisation analysis   (remaining scope: Scenario A extended
       shift hours — deferred pending a clinician fatigue model; Scenario B
       second surgical team — deferred pending a directed establishment
       change. Bypass-reason diagnostic merged, PR #64.)
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

*Prepared June 2026. Updated 16 July 2026 to reflect: completion of Issue #124 (force reinforcement can credit effective force size above initial establishment strength, PR #129) — `build_reinforcement_trajectory()`'s `credit_fn()` (`R/trajectories.R`) now clamps the credited value to `min(initial, current + fill)` rather than adding the submission-time fill amount unconditionally, and a new per-pool "pending" global (`reinf_combat_pending`/`reinf_support_pending`, `R/replication.R`) lets `demand_fn()` net out shortfall already claimed by an in-flight (submitted, not-yet-credited) cycle, closing an overlapping-cycle (`demand_interval_days < fulfillment_lag_days`) double-claim identified in a follow-up comment on top of the original unconditional-credit report. Verified against the real `simmer`-based mechanism, not a design-only argument: R 4.3.3 plus the four packages the reinforcement code path actually requires (`jsonlite`, `triangle`, `simmer`, `simmer.bricks`) were installed ad hoc in-session (no Docker daemon available for the pinned Dev Container); an aggressive overlapping-cycle stress configuration (`demand_interval_days=2` vs `fulfillment_lag_days=10`, `fill_max_frac=1.5`) produced 0 ceiling violations on the fixed code across 2,220 recorded global-write events, and the identical configuration reliably overshot when re-run against pre-fix `main` (`effective_force_combat` max 2624 of initial 2500; `effective_force_support` max 1313 of initial 1250), confirming the stress test genuinely exercises the bug. No `env_data.json` change; seed-42 baseline unaffected (reinforcement only adds a generator, and now the pending globals, when `demand_interval_days > 0` — the shipped default is `0`; direct re-run confirmed total casualties 386, matching `CLAUDE.md`'s documented baseline exactly). No issue newly unblocked. Previously updated 16 July 2026 to reflect: completion of Issue #109 (add AME repatriation and queue visualisation to the analysis tab, PR #126) — `plot_ame_queue()` (factored out of `analyse_run()`'s pre-existing inline `ame_backlog_plot` block) and new `compute_ame_sorties()`/`plot_ame_sortie()` (reconstructing every scheduled AME sortie opportunity's outcome — configuration selected, seats used vs. capacity — from the `"ame"`/`"ame_critical"` resource monitor against the schedule's own deterministic firing times, since `build_ame_sortie_trajectory()` keeps no sortie log of its own) are wired into both `analyse_run()` and `analyse_replications()` and into a new "Strategic AME" Analyse tab in `app.R`. Verified by direct execution rather than code inspection alone: R 4.3.3 and every required package installed from source ad hoc in-session (no Docker daemon available for the pinned Dev Container), `run.R --seed 42 --days 30 --iterations 1` run directly. This surfaced a real, pre-existing bug predating this issue (from the Issue #23 follow-up): `ame_wait_and_board()` (`R/trajectories.R`) uses a manual `timeout()`/`rollback()` polling loop rather than simmer's native queueing, so a waiting casualty never registers in the `"ame"`/`"ame_critical"` resources' own queue tracking — the resource monitor's `queue` column is structurally always 0, and the extracted `plot_ame_queue()` always rendered a flat zero backlog line regardless of the true state. Fixed with a new `compute_ame_backlog()` reconstructing the true backlog from per-casualty `r2e_departure_time`/`ame_departure_time`/`ame_route` event timestamps instead; `compute_ame_sorties()`'s `seats_used` had the same class of bug (an instantaneous pre-sortie queue snapshot, also always 0), fixed by measuring the resource's `server`-count change across each sortie's window (to the next scheduled sortie, or end of run for the last one) instead. The corrected backlog's peak values (critical pool 89, standard pool cyclical to 9) match figures already documented in the README's "Strategic Evacuation and Role 4 Demand" section, confirming the fix — that prose had always been derived correctly from `ame_wait_time_summary`, which is how the plot bug went unnoticed since Issue #23 merged. No `env_data.json` or trajectory-logic change; seed-42 baseline unaffected (total casualties 386, matching the documented post-Issue-23 baseline exactly); no issue newly unblocked. Previously updated 16 July 2026 to reflect: completion of Issue #121 (graphs in Shiny app exceed available window space instead of scaling to fit, PR #123) — every Analyse-tab plot output is now wrapped in a "shrink-to-fit" container (`shrink_to_fit_script()`/`bch_shrink_to_fit_css()`, `app.R`): a client-side script shrinks the container's CSS height to fit the viewport minus a fixed chrome allowance, `renderPlot()` keeps an explicit natural height (never `"auto"`) so Shiny never re-renders in response to the container's displayed size, and a paired stylesheet makes the plot image track that height (`height: 100%`, `width: auto`, `margin: 0 auto`) so the browser scales the already-rendered, full-detail image down losslessly and centred; an "Expand to full size" modal link is provided per plot. Two earlier designs (auto-sized `renderPlot()` redrawing to fill a JS-shrunk container; a CSS `transform: scale()` on a full-size render) were tried and dropped after live testing found each reintroduced real defects — respectively, label/row overlap at small sizes, and a compounding-shrink bug from Shiny's resize-sensing measuring a transformed container's on-screen size. Two owner-directed follow-ups extended scope within the same PR: Queue Depths and Quick Run's Bed & Resource Utilisation, each previously one combined `patchwork` image, were split into individually-sized panels (a "group" shared-budget mechanism was tried first, then reverted once established that scrolling *between* panels is acceptable — only a single panel exceeding the viewport is not); and R2B Treatment (itself a 3-panel `patchwork` stack) was found sharing R2E Surgery's 400px height convention rather than Casualty Flow's 700px (the same 3-panel shape), causing y-axis-title overlap between its stacked sub-panels, fixed by giving it 700px. Verified live via Playwright/headless Chromium against an ad hoc R 4.3.3 sandbox (`renv::restore()` from the project's own `renv.lock`; no Docker daemon available for the pinned Dev Container) across all four commits, including the direct Issue #111 regression check (the Expand modal shows every Gantt row with no overlap). No `env_data.json` or trajectory-logic change; seed-42 baseline unaffected; no issue newly unblocked. Previously updated 15 July 2026 to reflect: completion of Issue #111 (Bed & Resource Utilisation Gantt chart rows overlap due to insufficient vertical space, PR #120) — the Quick Run "Bed & Resource Utilisation" tab's fixed `plotOutput(height = "1400px")` squeezed individual R2B/R2E bed rows past legibility as an echelon's bed count grew (the default `env_data.json` alone has 40 R2E bed slots in one unfaceted Gantt panel); a new `utilisation_plot_height()` reactive (`app.R`) instead computes the container/render height from the distinct resource-row count in `r2b_gantt$data` (summed per R2B team facet) and `r2e_gantt$data`, at 25px/row with a 150px floor per section, leaving Full Analysis mode's static 500px bar-chart height untouched. Verified live via Playwright/headless Chromium against a from-scratch R 4.3.3 environment (no Docker daemon available for the pinned Dev Container): Quick Run computed 1675px (vs. the old 1400px) with every bed row legible, and a before/after comparison against the pre-fix code at the identical seed/config reproduced the exact reported row-overlap symptom, confirming the fix. No `env_data.json` or trajectory-logic change; seed-42 baseline unaffected; no issue newly unblocked. Previously updated 15 July 2026 to reflect: completion of Issue #110 (queue-depth plots use fixed y-axis scale and clip data, PR #118) — audited every queue-depth plot function in `R/analysis.R` and found three affected in the Quick Run single-run path (`p_r1_queues`, `p_r2b_bed_queues`, `p_r2e_bed_queues`), each hardcoding `scale_y_continuous(limits = c(0, 10))`; replaced with `limits = c(0, NA)` + `expansion(mult = c(0, 0.05))`, matching the existing utilisation plot's pattern. The Full Analysis (multi-run CI ribbon) equivalents were already dynamic and needed no change. Verified by direct execution rather than code inspection alone: R 4.3.3 and required packages installed ad hoc in-session (no Docker daemon available for the pinned Dev Container), `run.R --seed 42 --days 30 --iterations 1` run directly, console KPIs matching the documented baseline exactly, and the rendered R2E Heavy Bed queue plot confirmed the ICU queue climbs past 20 by day 30 at that exact baseline — the fixed 0–10 scale was clipping real, currently-documented output, not just a hypothetical stress-test scenario. No `env_data.json` or trajectory-logic change; seed-42 baseline unaffected; no issue newly unblocked. Previously updated 14 July 2026 to reflect: completion of Issue #23 (strategic evacuation demand — Role 4 occupancy and AME sortie requirements, PR #107) — `compute_role4_census()`/`compute_ame_demand()` (`R/analysis.R`) close the outbound half of the causal chain Issue #18 opened on the inbound side. Four owner-directed follow-ups substantially extended scope within the same PR: strategic AME became a real scheduled, capacity-limited, failure-prone simmer resource (fixing a fixed-vs-additive capacity bug caught during verification); AJP-4.10(B) research grounded a two-pool critical/standard split, then a further redesign to two planner-named aircraft configurations selected each sortie by a backlog-minimising rule (`select_ame_configuration()`); the default sortie interval changed from 3 to 7 days to match the reinforcement mechanism's cadence; and a periodic wait-time DOW poll (`ame_dow_poll()`, new `dow_echelon = 5`) closed the model's only previously-unbounded, risk-free wait, using the same conditional-increment logistic formula and `timeout()`-then-`rollback()` polling pattern already established elsewhere. All four follow-ups are exposed in a new Shiny "Strategic AME" Configure subgroup and on the Medevac Chain sidebar diagram. Seed-42 baseline shifted (not RNG-stream-neutral — AME wait duration changes R2E bed-release timing, and the sortie generator and DOW poll each consume new `runif()` draws): total casualties 386; 133 strategic evacuation decisions, 40 boarded by day 30, critical-pool mean wait 12.8 days, Role 4 peak occupancy 19.0 (day 22); see the Key Parameters table in `CLAUDE.md` for full detail. No issue newly unblocked — no open issue currently carries `status: blocked`. Phase 4 Scenario Expansion now complete. Previously updated 13 July 2026 to reflect: completion of Issue #18 (endogenous casualty generation — force regeneration feedback loop, PR #105) — `effective_force_combat`/`effective_force_support` live simmer globals, debited at each casualty's `injury_time` and credited at each RTD event (`R/trajectories.R`), give `in_theatre_rate` a real mechanical effect on arrival rate for the first time, closing the gap identified in Issue #3's sensitivity screening; the six background casualty streams switch from pre-computed `at()` arrival vectors to force-size-reactive generator closures (`R/environment.R`). Reinforcement was redesigned mid-PR, per owner feedback, from a fixed periodic size to a demand/fulfillment-lag/triangular-fill model (`demand_interval_days`/`fulfillment_lag_days`/`fill_min_frac`/`fill_mode_frac`/`fill_max_frac`) — self-limiting since demand is the pool's actual shortfall rather than a fixed size, producing a materially more realistic demonstration (arrests decline to a statistically flat trend rather than overshooting into growth). Ships disabled by default, confirmed byte-identical to the pre-redesign baseline with zero RNG draws consumed when disabled. Adds a Shiny "Force Regeneration" Analyse tab and a "Reinforcement Demand & Fulfillment" Configure subgroup, verified end-to-end against a live running app via headless-browser testing — catching two real bugs (a missing multi-run plot path, a key-naming mismatch) before merge. Seed-42 baseline shifted (RNG-stream-shifting in a structurally larger way than prior such merges — the draw *order* changes, not just values drawn from an unchanged order): total casualties 400→386; see the Key Parameters table in `CLAUDE.md` for full detail, including several rows explicitly flagged as not yet re-derived in this refresh (unpinned-sandbox provenance caveat, no Docker access in the development environment). Unblocks Issue #23 (`status: blocked` → `status: ready`). Previously updated 13 July 2026 to reflect: completion of Issue #57 (transport fleet-size capacity margin sweep, PR #103) — implements `plot_transport_capacity_margin_by_fleet_size()` (`R/analysis.R`), replacing the Issue #6-era stub; sweeps PMV Ambulance (1–5) and HX240M (1–4) fleet size directly, reusing `run_replications()` (the same engine Issue #10's comparative scenario runner uses) rather than duplicating replication/aggregation logic; writes `outputs/transport_capacity_by_fleet_size.csv` and `images/transport_capacity_margin_by_fleet_size.png`; new `scripts/run_transport_sweep.R` CLI entry point. A follow-up commit within the same PR, made at owner request beyond the issue's original scope, integrates the sweep into the Shiny app's Sensitivity Calibration tab alongside Morris/Sobol, dispatched via the existing `scripts/shiny_worker.R` subprocess mechanism, followed by three further owner-requested UX passes — a plain-language panel description in place of developer-facing prose, human-readable facet titles plus a proper plot legend for the ribbon/mean-line/reference-line (previously explained only in prose), y-axis labels moved to their conventional left-hand position, and the "how to read this" interpretation text relocated above the plot and shortened to one instruction — verified live via Playwright driving headless Chromium against the actual running app. No `env_data.json` or trajectory-logic change; seed-42 baseline unaffected; no issue newly unblocked. Previously updated 12 July 2026 to reflect: completion of Issue #77 (Configure panel eager-render race, PR #101) — reverted the `group_ui_*` panel outputs, 19 curve-preview plots, and 2 sticky-sidebar diagrams from Issue #14's eager-render override back to Shiny's default `suspendWhenHidden = TRUE`, so a panel's fields bind only once actually opened, eliminating both the ~18-20s startup cost and a race where a widget's own late-arriving initial-value message could silently overwrite an edit made in the first several seconds after load or a scenario switch. Added `fill_missing_defaults()` so `validate_config()` still treats a never-opened panel's fields as their correct scenario default rather than an error-worthy NULL. Also closes the duplicate Issue #98 (a slider-specific report of the same underlying mechanism; its own proposed fix, PR #100, was investigated and found not to address the actual defect once tested live). Verified via headless-browser WebSocket testing against the real running app, not just reasoned about: the silent-revert race reproduced 2/2 times before the fix and 0/2 after; a regression the fix itself introduced (Quick Run failing validation with non-default panels closed) was caught in testing and fixed before merge. No `env_data.json` or trajectory-logic change; seed-42 baseline unaffected; no issue newly unblocked. Previously updated 11 July 2026 to reflect: completion of Issue #15 (Shiny app — Full Analysis mode and Sensitivity Screening, PR #97) — activates the two modes Issue #14 left as disabled placeholders (Full Analysis via `run_replications()`, mean ± 95% CI on four KPI cards and ribbon/band result tabs; Sensitivity Screening via `run_morris()`/`run_sobol()`, real per-design-point progress). Live testing on a local Docker Desktop dev container and a GitHub Codespace found and fixed several reliability gaps beyond the initial UI wiring: `detect_safe_cores()` bounds `mclapply` concurrency from real-time cgroup memory headroom, recomputed fresh before each run; `run_replications()`/`run_morris()`/`run_sobol()` gained `max_cores` caps, killed-worker detection, and an inter-iteration `gc()` against sequential-screen memory growth; a previously-latent `simmer::select()`/`dplyr::select()` masking bug in `R/analysis.R` (present since Issue #14, never triggered by the CLI path's different package-attach order) was fixed by qualifying every ambiguous call; and, most significantly, Full Analysis/Morris/Sobol no longer call these functions in-process inside a `future()` body — both `multisession` and `multicore` proved unsafe for a future body that itself calls `mclapply()` (a corrupted control socket and a real deadlock respectively, the latter confirmed via `ps`), so all three now shell out via `system2()` (`run_shiny_worker()`/`scripts/shiny_worker.R`) to the same code path `run.R` already runs successfully with full parallelism. `.devcontainer/devcontainer.json` also gained `hostRequirements` and a second forwarded port for the Shiny app. No `env_data.json` or trajectory-logic change; seed-42 baseline unaffected; no issue newly unblocked. Previously updated 11 July 2026 to reflect: completion of Issue #9 (mass casualty stochastic injection — compound Poisson process, PR #92) — `generate_mass_casualty_events()` (`R/environment.R`) overlays Poisson-distributed mass casualty surge events onto the background lognormal generation, plus a second scheduled-day mode (owner feedback) giving each planner-specified day its own occurrence probability, casualty-count bounds, and priority split; Configure panel exposure (owner feedback) added a "Mass Casualty" accordion panel including a dynamic `+`/`−` scheduled-event list (capped at 20 slots, later fixed to remove a dead-space layout bug); "MASCAL" acronym renamed to "mass casualty" throughout (owner feedback). Ships disabled by default (`rate_per_day = 0`, empty schedule); seed-42 baseline unaffected, re-verified against the merged `main` tip. No issue newly unblocked. Previously updated 11 July 2026 to reflect: completion of Issue #93 (Dev Container build failure — missing `libuv` runtime + `renv.lock` R version mismatch, PR #94) — corrected `renv.lock`'s `R.Version` field from `4.3.3` to `4.4.2` (verified via `renv::snapshot()` as package-version-neutral) and added `libuv1t64` + `curl` to `.devcontainer/Dockerfile`, fixing the actual build failure (`fs` package's prebuilt binary failing `dyn.load()` on a missing `libuv.so.1`). No `env_data.json` or trajectory-logic change; seed-42 baseline unaffected; no issue newly unblocked. Also backfills the previously-missing post-merge update for Issue #72 (adopt `renv` for reproducible R package dependency pinning, PR #91) — `renv.lock` (116 packages) committed, Dockerfile switched to `renv::restore()` with a pre-warmed cache; no `env_data.json` or trajectory-logic change; seed-42 baseline unaffected; no issue newly unblocked. Previously updated 10 July 2026 to reflect: completion of Issue #76 (R2B/R2E surgery duration narrative vs. shipped `env_data.json` divergence, PR #89) — reconciled to 41/210/95 minutes (Sohn et al. 2018 within Zizzo et al. 2020), matching the README narrative; `env_data.json` had shipped an unsourced 90/240/120 placeholder since the project's initial commit. Seed-42 baseline shifted (RNG-stream-shifting change, same class as Issues #5, #6, #43, #73) — see the Key Parameters table in `CLAUDE.md`. No issue newly unblocked. Previously updated 10 July 2026 to reflect: completion of Issue #85 (`check_env_data_summary.R` crash on R1's integer `sub_elm`, PR #87) — hardened via a new `resolve_sub_elm()` helper that treats any non-character `sub_elm` the same as a missing one, fixing both the `purrr::map_chr()` crash and a masked defect it had been hiding (R1's ENV SUMMARY column header rendering as a bare `"1"` instead of `"Base"`). No `env_data.json` schema change; no issue newly unblocked. Previously updated 10 July 2026 to reflect: completion of Issue #74 (remove the dead-heading return leg multiplier, PR #83) — `return_leg_multiplier` removed entirely from `env_data.json`, `R/trajectories.R`, `R/sensitivity.R`, and the Shiny Configure panel, despite Issue #73's Morris re-run having just found it highly influential on transport utilisation and DOW count; removal proceeded on owner-directed operational grounds (no doctrinal basis for departing from a symmetric round trip) rather than the issue's original since-invalidated "no detectable effect" rationale. RNG-stream-neutral (confirmed by a before/after worktree diff); no issue newly unblocked. Previously updated 10 July 2026 to reflect: completion of Issue #73 (R2B → R2E WIA dead-heading return leg configured but never applied, PR #81) — resolved in three stages within one PR: dead-code removal (formalising the R2B organic-asset design), a subsequent owner-directed reversal adding a working dead-heading return leg to that same organic resource plus real R2B→R2E road-move mortuary transport for KIA/DOW, and a verified (not merely asserted) Morris sensitivity re-run that found `return_leg_multiplier` — after its scope doubled from two legs to four — is now the most influential screened parameter on mean transport utilisation and near-top on total DOW count, correcting an initial unverified claim to the contrary. No issue newly unblocked; Issue #74's removal proposal was flagged (via comment) as resting on a now-outdated premise. Seed-42 baseline shifted (RNG-stream-shifting change, same class as Issue #6) — see the Key Parameters table in `CLAUDE.md`. Previously updated 09 July 2026 to reflect: completion of Issue #75 (stale `p1_p_max` Morris screening bounds — re-derived bounds, full Morris re-run, PR #79), plus a follow-on DOW survival-function curve added to the Shiny Configure panel during the same PR. Seed-42 baseline unaffected (Morris design table and Shiny UI code only; no `env_data.json` or trajectory-logic change). No issue was newly unblocked by this merge. Previously updated 09 July 2026 to reflect: completion of Issue #14 (Shiny app — parameter editor, Quick Run mode, single-run output display, PR #71), including the `R/analysis.R` ggplot-object refactor that was the sole gating dependency for Issue #15 — unblocking Issue #15 (`status: blocked` → `status: ready`). Three pre-existing gaps found (not introduced) during Issue #14 were raised as new issues: #75 (stale Morris screening bounds for `p1_p_max`), #76 (R2B/R2E surgery duration narrative vs. shipped `env_data.json` divergence), and #77 (Configure-panel eager-render/`suspendWhenHidden` race); a fourth (#73, R2B→R2E dead-heading return leg never applied) was already open, and a related owner-raised follow-up (#74, remove `return_leg_multiplier` entirely) is also open. Previously updated 07 July 2026 to reflect: completion of Issue #10 (comparative scenario runner, PR #69) — compares `moderate_intensity`/`high_intensity` (not the uncited Vietnam/Okinawa figures originally in the issue body), unblocking Issue #57 (`status: blocked` → `status: ready`). Previously updated 06 July 2026 to reflect: completion of Issues #19 (PR #21), #1 (PR #16), #8, #22 (PR #26), #2 (PR #20), #3 (PR #30), #24 (PR #32), #7 (PR #34), #35 (PR #36), #37 (PR #38), #44 (PR #47), #39 (PR #48), #5 (PR #53), #6 (PR #56), #43 (PR #59), #60 (PR #62), #54 (PR #67), and partial completion of #40 (bypass-reason diagnostic, PR #64); addition of new Issues #43 (OT–ICU gating), #44 (RTD KPI annotation), #57 (fleet-size capacity margin sweep), and #60 (bed/resource `qty: 0` silently creates one unit instead of zero — discovered during Issue #43 testing); and reclassification of Issues #4 and #40 (remaining Scenario A/B scope) as `status: backlog` — both are unblocked but deprioritised, not currently planned. Phase 1 Statistical Foundation complete. Phase 2 Model Fidelity complete except Issue #40's deferred Scenario A/B scope — Issues #8, #35, #37, #44, #5, #6, #43, and #14 all merged; Issue #9 unblocked; Issue #18 unblocked. Phase 4 Scenario Expansion in progress — Issues #54 and #10 merged; Issue #57 unblocked. Phase 5 Interface in progress — Issue #14 merged; Issue #15 unblocked.*
