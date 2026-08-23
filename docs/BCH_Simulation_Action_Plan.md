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
| 40 | R2B OT suboptimal utilisation — 12h shift window limits forward surgery | Medium | Medium | Bypass-reason diagnostic **Merged (PR #64)**; Scenario A/B **Closed (not planned, 13 Jul 26)** |
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
| 128 | Graph of hold beds used in absence of ICU not surfaced in Shiny app | Medium | Low | **Merged (PR #131)** |
| 117 | Audit and complete Shiny app Analysis tab coverage of simulation outcomes | Medium | High | **Merged (PR #133)** |
| 112 | Verify sensitivity screening covers all relevant model parameters | Medium | Medium | **Merged (PR #135, #137)** |
| 114 | Revise in-app help text for clarity and remove internal issue-number references | Medium | Medium | **Merged (PR #139)** |
| 116 | Repo cleanup — audit and resolve orphaned objects and file/folder structure | Low | Medium | **Merged (PR #141)** |
| 115 | Add an in-app Getting Started guide | Medium | Low | **Merged (PR #143)** |
| 161 | R2E surgery seizes no surgical team (OT throughput ignores the shift roster) | Critical | Low | **Merged (PR #162)** |
| 154 | `run.R` writes a different artifact set per run mode, letting the tracked baseline drift | High | Medium | **Merged (PR #165)** |
| 152 | DOW calibration target of 0.52% is not supported by its cited sources | Critical | Medium | **Merged (PR #167)** |
| 156 | R2E disposition drawn independently of severity; AME evacuees re-occupy ICU beds | High | High | **Merged (PR #169)** |
| 160 | AME sortie configurations understate C-17A capacity 9x overall and 18x on the critical pool | High | Medium | **Merged (PR #171)** |
| 159 | Total surgery and ICU time not conserved across treatment location; post-definitive ICU absent entirely | High | High | **Merged (PR #174)** |
| 173 | Every casualty requiring surgery is routed through the damage control sequence | High | Medium | **Merged (PR #176)** |
| 147 | `ot_hours` missing from `env_data.json` — hardcoded default duplicated across six files | Medium | Medium | **Merged (PR #179)** |
| 180 | `check_r2e_surgery_seizure.R` structural check inert — surgery blocks located by a stale trajectory name | High | Low | **Merged (PR #182)** |
| 157 | Morris screening evaluates seven KPIs selected by availability, not the seventeen criteria-selected model outputs | Medium | Medium | **Merged (PR #184)** |
| 158 | Nine simplex-constrained composition parameters unscreened, including the triage split the two top-ranked parameters are conditional on | Medium | Medium | **Merged (PR #187)** |
| 178 | Died-of-wounds ceilings overshoot the Ajax Bay calibration target in both shipped configurations | Critical | Medium | **Merged (PR #190)** |
| 189 | Replication confidence intervals treat antithetically paired runs as independent | High | Medium | **Merged (PR #192)** |
| 186 | Two screened parameters carry a baseline outside their own bounds, mis-anchoring every Sobol run | High | Low | **Merged (PR #194)** |
| 195 | `surg_mode` screened over a range sitting almost entirely above the baseline it should surround | High | Low | **Merged (PR #197)** |
| 153 | `scripts/run_scenarios.R` fails at the plotting step outside a UTF-8 locale | Medium | Low | **Merged (PR #199)** |
| 148 | Lognormal casualty generator uses a fixed absolute rate cap, not a mean-relative one | High | Medium | **Merged (PR #202)** |
| 146 | R2B surgical team under-utilisation — pre-open OT queue window ahead of shift start | Medium | Medium | **Merged (PR #204)** |
| 203 | Per-minute rate cap holds realised casualty generation below every stream's configured mean | High | Medium | **Merged (PR #209)** |
| 208 | `run_replications()` draws different replication seeds on its first call in a session | High | Low | **Merged (PR #211)** |
| 201 | Action plan phase sequence lists have gaps, and the document's anchor links are unverified | Medium | Low | **Merged (PR #213)** |
| 206 | Casualty arrivals are far less variable day to day than a real arrival process | Medium | High | **Merged (PR #215)** |
| 149 | Mass casualty events do not generate immediate KIA or DNBI casualties | Medium | Medium | **Merged (PR #219)** |
| 150 | DNBI sub-type surgical-candidacy statistics predate multiple RNG-stream-shifting merges | Medium | Low | **Merged (PR #217)** |
| 151 | Okinawa-specific DOW ceiling and treatment efficacy calibration for `high_intensity` | Medium | High | **Merged (PR #221)** |
| 207 | Two configured parameters have a realised effect that is clipped | Low | Low | **Merged (PR #224)** |
| 155 | Final canonical re-run and documentation refresh once all issues are closed | High | High | **Merged (PR #226)** |
| 227 | Delete the twelve `wip/*` checkpoint refs once the evidence set is on main | Low | Low | Open |
| 228 | Higher-resolution Sobol decomposition to separate the leading parameter pair | Medium | High | Open |
| 230 | Establish the verification baseline — execute the regression check suite in the pinned Dev Container | High | Medium | Open (PR raised) |
| 231 | Repair the README reference list — duplicated entries, misattributed authors, two paywalled sources | Critical | Low | Open |
| 232 | All ten README images are broken links — fix the paths and validate link targets | Critical | Low | Open |
| 233 | The analysis pipeline consumes RNG, so `analyse_run()` is not idempotent | Medium | Medium | Open |
| 234 | Rewrite `docs/STYLE_GUIDE.md` as an enforceable R code standard | High | Medium | Open |
| 236 | Global configuration save/restore is not exception-safe; `R/analysis.R` lacks input validation | Medium | Low | Open |
| 237 | Housekeeping — delete the ten `wip/*` branches, reconcile the Further Development scan table, close `CLAUDE.md` drift | Low | Low | Open |
| 235 | Add `lintr`, a single check-suite runner, and GitHub Actions CI so the regression checks become a gate | High | Medium | Open (blocked) |
| 241 | Apply the code standard — decompose the oversized functions behind `testServer` and Playwright verification | High | High | Open (blocked) |
| 238 | Re-cut the analysis papers by method — move the replicated experiments out of the single-run paper | High | Medium | Open |
| 239 | Bring the single-run analysis paper to publication standard | High | High | Open (blocked) |
| 240 | Bring the multi-run analysis paper to publication standard | High | High | Open (blocked) |

---

## Issues In Review (PRs Open — Awaiting Owner Merge)

### Issue 230 — Establish the Verification Baseline

**Branch:** `claude/issue-230-lq5ji3`

The repository's fifteen `scripts/check_*.R` regression checks had never been executed as a suite, so nothing recorded which of them passed. All fifteen were run at commit `edd6285` in the pinned Dev Container, built from `.devcontainer/Dockerfile` on base image `rocker/rstudio@sha256:6bfc87fb…`, the same digest the Issue #155 canonical refresh names, with `renv` resolving the lockfile from a binary mirror rather than compiling from source. All fifteen pass, and the tracked seed-42 evidence set reproduces byte for byte across `logs/logs.txt`, all seven `data/arrivals_*.txt` diagnostics and `data/mass_casualty_events.csv`, which confirms by execution the reproduction claim `CLAUDE.md`'s provenance note rests on. The results are recorded as a tracked table in a new `scripts/README.md`, registered in `scripts/check_markdown.R`'s `link_check_docs`.

The measurement was taken without repairing anything, so that the record describes the state it was taken from; no check failed, so no follow-up bug issue was raised. The runtime spread is the finding that governs Issue 235: the suite takes 56 minutes 49 seconds, of which `check_dow_calibration.R` alone accounts for 45 minutes, leaving the other fourteen at 11 minutes 47 seconds combined and eight of them under half a minute each. Two further observations were recorded for whoever wires the gate up: runs emit roughly one hundred `simmer` warnings per short run for casualties still holding an aeromedical evacuation seat when the window closes, which is ordinary end-of-run truncation but would bury a genuine message in a log; and `check_env_data_summary.R` and `check_markdown.R` rewrite tracked documents in place rather than only inspecting them, so a gate must read a resulting working-tree change as the failure signal. Both left the tree clean at this commit.

---

## Recently Merged Issues

### Issue 155 — Final Canonical Re-run and Documentation Refresh ✓

**Merged:** PR #226, branch `claude/issue-155-qit0ip`

The terminal refresh. Every figure, table and plot across `README.md`,
`docs/Single_Run_Analysis.md`, `docs/Multi_Run_Analysis.md` and `CLAUDE.md` is
rebuilt from one code state, commit `ed3c426`, in the pinned Dev Container
built from `.devcontainer/Dockerfile` on base image
`rocker/rstudio@sha256:6bfc87fb…`. This is the first refresh in the project's
history able to build that container, so the twenty-one accumulated per-issue
provenance caveats are retired rather than added to.

The headline result is that they retire as correct. The pinned run reproduces
the tracked seed-42 baseline **byte for byte**, in `logs/logs.txt` and in every
arrival diagnostic, and the 50-replication scenario comparison and the
450-replication died-of-wounds calibration both reproduce their published
figures exactly. Three independent generators agree, so the unpinned R 4.3.3
sandboxes the project relied on were faithful and no published seed-42 value
moves.

What does move are the measurements whose generators had not been re-run since
the arrival process was rebuilt around per-day rate draws: the transport fleet
sweep, the forward ICU share frontier, the mass casualty stress test, the force
regeneration trend table and nine Key Parameters rows. Each is identified in
place as a consequence of that model change rather than of the environment.

Two published claims did not survive re-measurement, and one code path was
found never to have worked. The R2B pre-open hold window's "the accounting
closes" finding is withdrawn: forward surgeries move +0.38 [−2.75, +3.51]
against 5.90 casualties held forward, the arms having diverged into different
realisations once the arrival process carried real variance. `run_sobol()`
could never return indices, `sensitivity::tell()` assigning its result into the
frame it is called from and so populating only a wrapper local.

The Morris screen is rebuilt at the full sixty-five parameters and at r = 20
trajectories, and answers the question Issue #158 was raised to ask against the
hypothesis that prompted it. `triage_p1_balance` ranks **nineteenth of
sixty-five** at µ* = 3.16, below both Priority 1 conditional rates defined on
the share it governs (`pri1_surg_prob` at 2, `pri1_evac_prob` at 13), so how
many casualties are classified Priority 1 matters less to surgical bottleneck
severity than what happens to one once classified. An interim screen at r = 5
put it first at µ* = 8.63, and that did not survive the quadrupled trajectory
count: it carried the largest σ in that table at 10.69, a standard error of
±4.78 on its own µ*, so the rank was never separable from noise. Four
paragraphs of ranking commentary that still described the r = 5,
fifty-three-parameter screen were corrected against the r = 20 table at the
same time.

The variance decomposition was then interrogated rather than published as it
stood, and three scripts were added to do it, each recomputing from responses
already cached and costing no further simulation.
`scripts/compare_sobol_estimators.R` recomputes the same design under the
Jansen and Martinez pick-freeze estimators alongside the reported Saltelli one,
which share an identical design; only the leading parameter holds its position
across all three, and the two alternatives' apparent tidiness is construction
rather than resolution, neither returning a total-order index at or below zero
anywhere in the design. `scripts/test_sobol_separation.R` tests which orderings
the sample supports by bootstrapping the design rather than the indices, so two
indices computed from the same evaluations keep their correlation; the
interval-overlap reading that preceded it demanded roughly three times the
sample the correct test does. `scripts/measure_noise_floor.R` measures how much
of the variance is replication noise, and finds 32.9% on the system OT queue at
four replications per point, 62.9% on transport utilisation and 89.1% on the
transport queue.

That measurement changed the conclusion. Noise enters the variance the indices
are shares of, so a larger design alone converges on indices roughly a third
too low; the bias is set by the replication count per point, not by the number
of points. The decomposition is therefore reported as establishing a leading
pair and a negligible tail rather than a ranking, which is independently what
the Morris screen concluded, with the second parameter separating from the
third at P = 0.992 and the leading pair not separating from each other. The
gap and its measured closure cost, roughly N = 800 at 8 to 12 replications,
are recorded as a new Further Development entry L29, L18 having covered both
screens' resolution in one entry despite their different causes.

Two pieces of durability tooling ship with the work, having been built to
survive the environment losses this refresh met repeatedly.
`scripts/screen_cache.sh` checkpoints a screen's point cache onto its own git
ref via plumbing, never touching the working tree, index or branch, and
`scripts/supervise_screen.sh` drives a long screen to completion across
failures, stopping on a caller-supplied completion marker rather than on exit
status. `scripts/check_screen_cache.R` asserts the cache round-trip invariants,
the write guard having once discarded any design point at which even one of
thirty-six responses was legitimately undefined, so an interrupted screen made
no progress across restarts and nothing in its output said so. Each screen now
also writes a `*_run_metadata.csv` sidecar recording the design, the commit and
the R version behind its results, and a Sobol result file carries a `flag`
column marking an index outside the theoretical range.

**Seed-42 baseline (30 days, single run):** Unchanged, and that is the finding rather than an absence of one. The pinned run reproduces `logs/logs.txt`, all seven `data/arrivals_*.txt` diagnostics and `data/mass_casualty_events.csv` byte for byte, and the 50-replication scenario comparison and the 450-replication died-of-wounds calibration reproduce their published figures exactly. No row of `CLAUDE.md`'s Key Parameters table moves. The fourteen refreshes made in unpinned R 4.3.3 sandboxes were therefore faithful, and their caveats retire as correct rather than corrected.

**Unblocked by this merge:** Issues #227 (delete the twelve `wip/*` checkpoint refs, now redundant since the evidence set is tracked at `data/sensitivity/`) and #228 (higher-resolution Sobol decomposition at N ~ 800 and 8 to 12 replications, closing Further Development L29). Both were raised from this work and were blocked on its merge; both move to `status: ready`. Issue #4 remains in backlog, and merging it later would invalidate this refresh and require a second canonical run, which was recorded as an explicit pre-merge decision on PR #226 and accepted.

### Issue 207 — Two Configured Parameters Have a Realised Effect That Is Clipped ✓

**Merged:** PR #224, branch `claude/issue-207-1h389q`

Two planner levers answered a slightly different question than the one they were asked, and in both cases nothing in a run's output said so. The reinforcement fill fraction is drawn from a triangular distribution whose maximum of 1.1 names a package larger than the shortfall it was requested against, and the credit was clamped at establishment strength, so roughly 4.4% of draws had their excess dropped at the moment of crediting. That clamp is removed: reinforcement joins the population on arrival, there being no formation-level reserve in the model to hold it in, so a pool receiving more than its remaining shortfall now goes over establishment strength and stays there until casualties bring it back down. Over-strength is a transient rather than a new equilibrium, demand being the shortfall floored at zero, so an over-strength pool requests nothing and no later cycle builds on the surplus. Removing the ceiling required work in the arrival generators, which sample by thinning against a dominating rate that fixes the population term at an upper bound on the force size and accept each candidate at `F(t)/P_max`: a force size above that bound saturates the acceptance probability at 1 and the stream would silently generate at the dominating rate instead of the intended one, under-generating exactly where the force is largest. `reinforcement_force_bound()` (`R/environment.R`) is new and widens the bound to `(1 + fill_max_frac)` times establishment strength, which the demands in flight at any moment provably cannot exceed, since each cycle nets out what earlier pending cycles have claimed and an over-strength pool claims nothing. It costs proposal draws in proportion and returns establishment strength unchanged wherever reinforcement is disabled, so the shipped configuration samples exactly as it did before. `validate_fill_distribution()` (`R/trajectories.R`) is new and rejects an inverted fill distribution, `rtriangle()` returning NA rather than erroring when its `a <= c <= b` requirement is violated, which is how the Issue #112 screening run lost every elementary effect to an NA cascade.

The second lever is the R2B holding evacuation threshold. A casualty whose drawn convalescence exceeded it was moved to R2E part-way through it, and R2E drew a fresh recovery duration from scratch, so enabling what `docs/Single_Run_Analysis.md` Intervention Scenario C presents as a routing lever changed total modelled convalescence by an unaccounted amount. The unserved remainder is now carried forward and served at R2E, in the manner `r2e_stabilisation_minutes()` already nets off what R2B served forward under Issue #159. Three helpers, `r2b_hold_threshold()`, `r2b_hold_minutes()` and `r2b_hold_residual_minutes()`, divide the single draw between the echelons, replacing threshold logic that had been inlined four times across the two hold branches, and `draw_recovery_to_duty()` serves the residual rather than redrawing. The threshold now settles where the convalescence is served without changing how much of it there is, so the R2E load the scenario produces is attributable to the routing decision alone. `scripts/check_lever_realisation.R` is new and asserts both properties, running the reinforcement mechanism in a purpose-built harness because `run_once()` adds its generator with `mon = 0` and moves the pool for other reasons at the same time; it was verified to fail on the pre-change code.

**Seed-42 baseline (30 days, single run):** Unaffected, and verified rather than assumed. Both features ship disabled, so the seed-42 console log reproduces `logs/logs.txt` byte for byte and `mon_attributes.csv`, `mon_resources.csv` and `replication_summary.csv` are byte-identical to the pre-change run; `mon_arrivals.csv` is identical as a set and differs only in the enumeration order of arrivals still in the system at the run's end, the artefact recorded for the Issue #146 zero-window verification. No row of `CLAUDE.md`'s Key Parameters table changes and no tracked artifact in `data/`, `logs/` or `images/` was regenerated. The realised mean fill fraction was measured over 490 delivered cycles per pool at 0.715 (standard error 0.009) for the combat pool and 0.709 (0.009) for the support pool, against the distribution's own mean of 0.7167, with peak force sizes 0.28% and 0.48% over establishment strength. Across five 90-day runs of the whole model at a 7-day demand cycle and 7-day fulfillment lag, neither pool ever rose above establishment: casualty production keeps both in shortfall throughout, so over-strength is representable but needs a lightly attrited pool, a long demand cycle relative to the losses, or a fill maximum well above 1. The Force Regeneration Feedback Loop trend table in `docs/Single_Run_Analysis.md` was not re-measured, its reinforced rows having been produced under the clamped credit; it already carries a currency note recording the larger staleness, that it predates the current arrival process.

**Unblocked by this merge:** No new issues unblocked. No open issue lists Issue #207 as a dependency; Issue #155 remains blocked by its own terms until every other issue is closed, and now clears once #4 does. No label changes were needed.

### Issue 151 — Okinawa-Specific DOW Ceiling and Treatment Efficacy Calibration for `high_intensity` ✓

**Merged:** PR #221, branch `claude/issue-151-j0s44g`

The `high_intensity` profile overrode only its casualty generation rates, inheriting both the died-of-wounds ceilings and the treatment efficacy factors from the base configuration. Those two are entangled by construction: the base ceiling was fitted to a Falklands 1982 mortality target jointly with OIF/OEF-era multipliers, so neither transfers on its own, and running an Okinawa casualty stream through the pair applied a mortality target and a standard of care drawn from two unrelated contexts, neither of them the profile's own. Any mortality finding under the profile therefore conflated casualty intensity with a borrowed calibration. The profile now overrides `dow.params` and `dow.treatment_efficacy` on the basis `moderate_intensity` established. The calibration target is Okinawa's own reported rate, 3.4% of casualties who reached a hospital alive dying there (Marble, 2025, *Joint Force Quarterly* 117), measured on the same treated cohort the model reports and bracketed by the war-wide figures of 3.5% (Holcomb et al., 2006, already README reference [11]) and 4.5% (Marble); unlike the Ajax Bay bound, which rests on an inexact "over 650" denominator, this is a reported rate against a stated denominator, so the model is calibrated to reach it rather than only to stay beneath it. The treatment efficacy factors are informed estimates for 1945 Pacific-theatre care, disclosed as such per Source Prioritisation level 5 and reasoned from the absence of TCCC, balanced damage control resuscitation and staged damage control surgery: R1 TCCC 1.0, resuscitation 0.95, damage control surgery and R2E first operation 0.70, second operation 0.90. The two within-era penalties (`r2e_postop_hold_penalty`, `r2b_icu_penalty`) are relative degradations rather than period-specific technology and stay inherited, as under `moderate_intensity`. Against those factors the ceilings re-calibrate to `p1_p_max` = 0.052 and `p2_p_max` = 0.042, up from the base 0.020 and 0.016; the ceilings rise here where `moderate_intensity`'s fell, because this profile weakens the same factors against a target roughly seven times higher. `scripts/check_dow_calibration.R` now holds a target per configuration rather than one shared bound and tests `high_intensity` two-sided at a tolerance of two percentage points, taking a full run from 300 replications to 450.

**Seed-42 baseline (30 days, single run):** Unaffected, and verified rather than assumed. The change touches no trajectory, no base-configuration parameter and no random draw outside the profile, so the seed-42 console log reproduces `logs/logs.txt` byte for byte and no row of `CLAUDE.md`'s Key Parameters table changes; a provenance note records why. No tracked artifact in `data/`, `logs/` or `images/` was regenerated apart from `images/scenario_comparison.png`, which the comparative scenario runner rewrites alongside the tables re-measured for it. Pooling three independent 50-replication measurements gives a treated-cohort died-of-wounds rate of 3.471% (95% CI [3.360%, 3.583%]) against individual measurements of 3.592%, 3.463% and 3.359%, an interval spanning the target; DOW/WIA measures 3.164% (95% CI [3.066%, 3.262%]). The 50-replication comparative tables in `docs/Multi_Run_Analysis.md` were re-measured, the profile's own rows moving as the calibration intends (DOW/run 5.80 to 23.58, DOW/WIA 0.88% to 3.43%) and its queue rows moving in two directions with them: the theatre and intensive care queues shorten, 39.8 to 38.2 and 0.618 to 0.564, because a casualty who dies of wounds leaves the queue for the resource that would have treated them, while the queues further from the point of death lengthen alongside a casualty count reading 1,021.0 against 992.3. The `moderate_intensity` arm reproduced every published figure exactly, each queue group to the precision published, which is the evidence that the two arms differ only where they are meant to. Two stale figures surfaced in the course of the work and are corrected with it: the document's abstract carried queue ratios predating the arrival-generator rebuild, and the README's 30-replication profile comparison table had held 151.1 WIA and 57.1 KIA per run for `moderate_intensity` since before the same rebuild, measuring 184.8 and 68.7 now.

**Unblocked by this merge:** No new issues unblocked. No open issue lists Issue #151 as a dependency; Issue #155 remains blocked by its own terms until every other issue is closed, and now clears once #4 and #207 do. No label changes were needed. Issue #40, named as one of #155's remaining blockers by the three preceding entries, was closed as not planned on 13 July 2026 and is dropped from that list here; the summary table and the dependency graph are corrected with it.

### Issue 149 — Mass Casualty Events Do Not Generate Immediate KIA or DNBI Casualties ✓

**Merged:** PR #219, branch `claude/issue-149-azx9wr`

A fired mass casualty event drew a casualty count and injected every one of those casualties onto the combat WIA stream, so a modelled incident produced survivors only: the mortuary section and the KIA transport fleet saw none of a surge's mortality load, and an event configured at 40 casualties delivered 40 wounded rather than 40 casualties of whom some share are killed, which is not how an incident's size is reported or planned against. The drawn count is now the incident's total, split by a single Binomial(n, `kia_fraction`) draw per event. The killed are overlaid on the background `kia_cbt` stream and the wounded on `wia_cbt`, so each takes the pathway its own background stream already takes, the killed reaching R1 mortuary handling and the KIA transport fleet and never being triaged. `wrap_with_mass_casualty()` now takes the name of the event-id sink it appends to, both combat streams being wrapped, and `build_casualty_trajectory()` reads whichever sink matches the entity's generator. Which of an event's casualties fall on each side costs no further draw, the injection-window offsets being exchangeable before they are sorted. `mass_casualty.event.kia_fraction` ships at 0.28 with a Configure-panel field of its own, rendering in both event-timing modes as the injection window does; no open-access source tabulates event-level killed-to-wounded ratios for a comparable campaign, so the default is documented as an informed estimate, being the killed share implied by the model's own FORECAS-derived combat streams (0.68 killed against 1.77 wounded per 1,000 per day). Disease and non-battle injury stay outside the mechanism on causal-link grounds, now stated alongside the fix so the two omissions are not read as one unmodelled thing: the missing killed were a modelling gap, while disease and non-battle injury are by definition not caused by a discrete tactical event. `scripts/check_mass_casualty_kia_split.R` is new and asserts conservation across the split at every share, the realised share against the configured one, the mortuary routing and the absence of triage, and that the parameter reaches nothing while injection is disabled. README Further Development L20 is deleted; no other document cited it.

**Seed-42 baseline (30 days, single run):** Unaffected, and verified rather than assumed. Mass casualty injection ships disabled (`mass_casualty.event.rate_per_day = 0`), so no event fires, the split draws no random number, and the run is bit-identical to the pre-change one: the seed-42 console log reproduces `logs/logs.txt` byte for byte, and a `--refresh-baseline` invocation returned every tracked artifact in `images/`, `logs/` and `data/` identical except the header of `data/mass_casualty_events.csv`, which gains the `n_wia` and `n_kia` columns the per-event table now carries. No row of `CLAUDE.md`'s Key Parameters table changes; a provenance note records why. Enabling injection is not RNG-stream-neutral, one Binomial draw being added per fired event, but no shipped configuration enables it.

**Unblocked by this merge:** No new issues unblocked. No open issue lists Issue #149 as a dependency; Issue #155 remains blocked by its own terms until every other issue is closed, and now clears once #4, #40, #151 and #207 do. No label changes were needed.

### Issue 150 — DNBI Sub-Type Surgical-Requirement Statistics Predate Multiple RNG-Stream-Shifting Merges ✓

**Merged:** PR #217, branch `claude/issue-150-4cfshc`

README's DNBI Sub-Type Split section reported a 100-replication surgical-requirement statistic that had stood unchanged since Issue #7 first measured it, and every RNG-stream-shifting merge recorded in `CLAUDE.md` landed between that measurement and this one, as did the three that moved casualty generation itself (#148, #203, #206) and the two that changed which replications a control seed selects (#189, which made replications independent rather than antithetically paired, and #208, which fixed the generator kind the per-replication seeds are drawn under). The section was presenting a pre-Issue-18 model as a current finding, which is the state this project's academic standard exists to prevent. The measurement was repeated at the same 100 replications of 30 days from control seed 42 against the current codebase, and the section and a `CLAUDE.md` provenance note were updated together. The qualitative conclusion the section draws is unchanged and now says so explicitly: WIA and NBI casualties drive operating theatre demand, disease adds a small load at its configured emergency rate, and battle fatigue adds none. The mean number of casualties requiring surgery per replication moves from 158.6 (SD 6.8; range 143 to 177) to 183.5 (SD 36.0; range 112 to 274), over a mean of 434.5 casualties generated per replication; by sub-type the rate is 81.1% for NBI (was 79.6%), 6.0% for disease (was 5.7%) and 0.0% for battle fatigue, the last being structural rather than drawn. The larger movement is dispersion rather than level: the per-replication standard deviation goes from roughly a twenty-third of the mean to roughly a fifth of it, which is Issue #206's restored between-day arrival variance reaching surgical demand, and the heaviest replication carries almost two and a half times the caseload of the lightest. The two drawn per-sub-type rates now sit within a percentage point of the probabilities that draw them, which the larger pooled casualty count buys.

Two things about the measurement are worth recording. A single unchunked `run_replications(100, 30)` call was OOM-killed in the sandbox and lost a whole worker's 25 replications, so the run was repeated in chunks of four, aggregating each chunk's monitor output and discarding it before the next; the chunked path reproduces `run_replications()` exactly, drawing the same seed vector from control seed 42 under the same generator kind and calling `run_once()` on each replication. And the statistic covers surgical requirement as assigned at R1, not surgeries actually performed under capacity constraint, which is reported separately in `CLAUDE.md`'s Key Parameters table and `docs/Single_Run_Analysis.md` and is not re-measured here.

**Seed-42 baseline (30 days, single run):** unchanged, and byte-identically so. No trajectory, `env_data.json` parameter or random draw is touched; the seed-42 run was reproduced as the standard validation step and its console log matches the tracked `logs/logs.txt` byte for byte, which is both the confirmation that no baseline row moves and the fidelity evidence for the sandbox. No tracked artifact in `images/`, `logs/` or `data/` was regenerated. The measurement was made in an unpinned R 4.3.3 sandbox under the same caveat as the Issue #18, #23, #161, #154, #156, #160, #159, #173, #178, #189, #148, #146, #203, #208 and #206 work.

**Unblocked by this merge:** No new issues unblocked. No open issue lists Issue #150 as a dependency; Issue #155 remains blocked by its own terms until every other issue is closed, and now clears once #4, #40, #149, #151 and #207 do. No label changes were needed.

### Issue 206 — Casualty Arrivals Are Far Less Variable Day to Day Than a Real Arrival Process ✓

**Merged:** PR #215, branch `claude/issue-206-heito1`

Each casualty stream drew a fresh rate for every simulated minute and emitted a casualty at each whole-casualty crossing of the running total, so a day's count was an average of 1,440 draws and the central limit theorem flattened the stream long before those draws could reach a daily total: the combat WIA stream realised a daily standard deviation of 0.50 against the 2.10 of a Poisson process at the same rate, and in 5,000 simulated days never produced a day worse than six casualties. Peak-day volume is what drives contention for theatres, intensive care beds and airlift, so the model understated every queue it exists to measure, in the direction that is not conservative for planning. The minute grid is replaced by direct arrival-time sampling on two recorded design decisions. The intensity is a Cox process whose rate is redrawn once per simulated day, the timescale FORECAS fitted `mean_daily` and `sd_daily` at, rather than once per minute; and `sd_daily` is honoured rather than discarded in favour of the Poisson value, arrivals within the day being Poisson so that by the law of total variance the stream realises the configured mean plus the configured between-day variance on top of the Poisson term. Placement uses thinning (Lewis & Shedler, 1979) against a dominating rate holding the pool at establishment strength, each candidate accepted at `F/P_max` for the live force size, which preserves the Issue #18 feedback loop; `P_max` bounds `F` for the whole run because every casualty debits its pool and `credit_fn()` clamps the reinforcement credit at establishment strength. The sub-minute jitter step is gone with the grid, arrival times being continuous, and generation cost is now linear in the drawn rate rather than fixed at 1,440 iterations per day. Measured over 30,000 days at the shipped combat WIA parameterisation the daily count has a mean of 4.41 and a standard deviation of 9.5 against the 9.1 the construction predicts, a median day of two, a 99th-percentile day of 38 and a busiest day of 564. `scripts/check_arrival_rate_fidelity.R` gains the realised-variance assertion alongside the mean it already checked, and its sections on the retired one-arrival-per-minute discard are removed with the grid. The mass casualty parameters were reconsidered against a background stream that now varies and deliberately left unchanged: the background does now reach into the event size band, delivering twenty or more casualties within a two-hour window about once in fifty 30-day runs, but only an injected event tags its casualties as a cohort that can be followed through the system. Neither died-of-wounds ceiling was re-fitted; the calibration was re-measured at 150 replications per shipped configuration and `scripts/check_dow_calibration.R` passes for both against the one-sided Ajax Bay bound. README Further Development L27 is deleted.

**Seed-42 baseline (30 days, single run):** this moves casualty generation itself rather than only the draw order downstream of it, and it moves in both directions, the configured means being untouched while only their dispersion changes. No row below should be read as the effect of the change: one 30-day run is now a draw from a far wider distribution, which is the point of it.

| Metric | Before (post-Issue-203) | After (post-Issue-206) |
|---|---|---|
| Total casualties | 437 | 530 |
| WIA / KIA / DNBI | 187 / 71 / 179 | 287 / 72 / 171 |
| R2B surgical decision point | 152 (74 operated, 78 bypassed) | 210 (69 operated, 141 bypassed) |
| R2E surgeries — first / second | 115 / 31 | 171 / 41 |
| R2E OT queue ≥1 (OT1 / OT2) | 3.0% / 0.6% of run | 46.3% / 34.6% |
| R2E ICU utilisation (4 beds) | 97.3 / 92.2 / 88.1 / 82.0% | 93.7 / 92.1 / 92.2 / 85.8% |
| `surgery_deferred` | 15 | 29 |
| Strategic AME mean wait | 1.1 days | 10.1 days (two of four sorties cancelled at this seed) |
| Treated-cohort DOW rate (150 reps) | 0.417% [0.354%, 0.480%] | 0.474% [0.412%, 0.536%] |

**Unblocked by this merge:** No new issues unblocked. No open issue lists Issue #206 as a dependency; #207 was already unblocked, and Issue #155 remains blocked by its own terms until every issue is closed.

### Issue 201 — Action Plan Phase Sequence Lists Have Gaps, and the Document's Anchor Links Are Unverified ✓

**Merged:** PR #213, branch `claude/issue-201-4ld6op`

The issue reported five merged issues missing from the phase sequence lists and from their phase heading rosters. The audit it asked for found thirteen: `#72`, `#73`, `#74`, `#76`, `#85`, `#93`, `#124` and `#208` were in the same state as `#153`, `#158`, `#159`, `#186` and `#189`. Each now carries a struck-through, letter-suffixed item at its position in merge order and an entry in its phase heading roster, the phase taken from the issue's own label in every case; that confirmed all five of the homes the issue proposed and placed the other eight as `#72`/`#93` in Phase 5, `#73`/`#74`/`#76`/`#85` in Phase 2, `#124` in Phase 4 and `#208` in Phase 1. Items following an insertion were re-lettered so each list still reads in merge order, which is what the lists exist to show; unlike the `L<n>` gap identifiers these letters are list markers rather than cited identifiers, so nothing points at them and no citation is redirected. Issue `#19` gained an item as well, the Pre-phase section having recorded it in prose with no numbered item. Every issue this document's summary table records as merged now has both an item and a roster entry, verified by parsing the table and the sequencing section and taking the set differences rather than by reading them.

`scripts/check_markdown.R` now runs its anchor link check over every tracked markdown document rather than the three that carry a table of contents block, in both directions: a document in the set has its own links checked, and a link from elsewhere in the set to one of its headings is resolved against the headings it actually offers. The table of contents and return-link maintenance stays scoped to the three documents that carry those blocks, so neither `CLAUDE.md` nor this document is given one. A new `strip_code()` blanks fenced blocks and inline code spans before the scan, GitHub rendering a link inside backticks as literal text rather than as a link: without it, three of `CLAUDE.md`'s statements of its own cross-reference and citation conventions, which name placeholder anchors by design, would be reported as broken. The check was verified to fail on a deliberate break introduced into each newly covered document in turn, and to pass when it was removed.

The Issue 44 citation resolved to the first of this document's eleven `### References` headings, roughly 270 lines below the entry and belonging to a different issue. It now points at README's References section rather than that entry's own, as the issue proposed, because the entry has none: the eleven blocks belong to the original per-issue plan sections and are unnumbered bullet lists in which `[9]` means nothing, while `[9]` is README's numbering for Izaguirre et al. (2025), the source of the 7.6-42.1% in-theatre range the sentence quotes. The other ten entries were audited with it; there are only four anchor links in the whole document and the other three resolve correctly. `CLAUDE.md`'s post-merge checklist stated the strikethrough obligation alone, which is the maintenance gap that let thirteen issues go unrecorded, and now states what to do when there is no item to strike and to update the phase heading roster at the same time.

**Unblocked by this merge:** No new issues unblocked. No open issue lists Issue #201 as a dependency, and Issue #155 (final canonical re-run and documentation refresh) remains blocked by its own terms with six issues still open. No label changes were needed.

---

### Issue 208 — `run_replications()` Draws Different Replication Seeds on Its First Call in a Session ✓

**Merged:** PR #211, branch `claude/issue-208-c9thw4`

`run_replications()` drew its per-replication seeds before setting `RNGkind("L'Ecuyer-CMRG")`, and the kind persists for the rest of the R session, so the first call in a session drew its seeds under Mersenne-Twister and every later call drew them under L'Ecuyer-CMRG. The seeds were good seeds either way, so no published point estimate was biased and no interval was wrong for the replications it was computed over; what did not hold was that a measurement could be reproduced from the control seed it is stated at, or compared with a measurement taken in a different position in the invocation. The caller's generator kind and stream position are now snapshotted on entry and restored on exit, so the function mutates no global random number state, and the kind is set on both dispatch paths rather than the parallel one alone, so a replication's output depends on its seed rather than on whether `mclapply` or `lapply` dispatched it. `scripts/check_measurement_reproducibility.R` is new and asserts the four properties this rests on; it was verified to fail on the pre-change code, reporting the same two seed vectors the issue documents.

The issue's recommended fix, moving `RNGkind()` above the seed draw, was not taken, and the reason is worth recording because it is not obvious. `RNGkind()` re-initialises `.Random.seed` from the system clock every time it is called, including when called with the kind already in effect, so drawing the seeds after it would have made them a function of the wall clock rather than of `set.seed()` and would have removed reproducibility altogether rather than restored it. The seeds are therefore still drawn first, under a kind the function no longer changes, and the intent behind the suggestion is met by making the kind stable across calls rather than by setting it earlier. Restoring the stream position as well as the kind is what satisfies the issue's second acceptance criterion: restoring only the kind would leave the seeds reproducible but the stream advancing, so a scenario measured third in a comparison would still disagree with the same scenario measured on its own. One consequence follows and was taken deliberately. Every scenario in a comparison, and every point in a sweep or Morris design, now runs on the same per-replication seeds, which makes the arms a comparison on common random numbers and estimates the difference between them more precisely; replications within an arm remain independent, so every published per-arm interval is still correctly specified.

The re-measurement cascade separates cleanly along first-call lines. The died-of-wounds calibration was re-measured at 150 replications per shipped configuration: `default` moves from 0.443% to 0.417% (95% CI [0.354%, 0.480%]), its first of three measurements unchanged at 0.524% and the two behind it moving, and `moderate_intensity` from 0.290% to 0.353% ([0.293%, 0.413%]), all three of its measurements moving because all three sat behind `default`. `moderate_intensity` at control seed 42 now returns 0.392%, the figure the issue reports for measuring that profile on its own, against the 0.248% the standard both-scenario invocation had returned for the same seed. Both configurations still pass the one-sided Ajax Bay bound, but their intervals now overlap, so the claim that 150 replications separate the profiles from each other on mortality is withdrawn while each remains separated from the bound. In the 50-replication comparative scenario tables the whole `moderate_intensity` arm is unchanged to the precision published, and `high_intensity` moves by the amount replication-to-replication variation produces at that count.

**Seed-42 baseline (30 days, single run):** unchanged, and byte-identically so. `run.R --iterations 1` calls `run_once()` directly rather than through `run_replications()`, and the run's console log reproduces the tracked `logs/logs.txt` byte for byte, which is both the confirmation that the single-run path does not move and the fidelity evidence for the sandbox. No tracked artifact in `images/`, `logs/` or `data/` was regenerated apart from `images/scenario_comparison.png`, which the comparative scenario runner rewrites alongside its tables. Measurements were made in an unpinned R 4.3.3 sandbox under the same caveat as the Issue #18, #23, #161, #154, #156, #160, #159, #173, #178, #189, #148, #146 and #203 refreshes.

**Unblocked by this merge:** No new issues unblocked. No open issue lists Issue #208 as a dependency; #206 and #207 were already unblocked, and Issue #155 remains blocked by its own terms until every issue is closed.

### Issue 203 — Per-Minute Rate Cap Holds Realised Generation Below the Configured Mean ✓

**Merged:** PR #209, branch `claude/issue-203-nvc17p`

Both arrival generators clamped each per-minute rate draw at three times the stream's own mean, which lowered the mean the stream realised: the WIA streams delivered 79.0% of the daily rate their configuration named, the KIA streams 78.7%, `dnbi_cbt` 95.2%, `dnbi_spt` 99.2% and any exponential stream 95.0%. The shortfall also tracked each stream's coefficient of variation, so editing `sd_daily` alone moved the realised mean of a stream whose mean had not been touched. The issue proposed either correcting for the cap or replacing the minute walk with thinning; the PR took a third route that measurement made available. The cap's justification was a run-time blow-up in an earlier, vectorised generator in which one extreme draw emitted an unbounded burst of entities, and the closure that replaced that generator performs exactly `n_minutes` iterations whatever the draws, so the cap was removed outright rather than corrected for. A bias-corrected parameterisation was built first and removed with the cap, the two having been measured as near-identical in distribution.

Removing the cap left one silent clipping behind, which the PR also fixed. The closure assigned the new cumulative floor rather than incrementing it, so a minute crossing several whole-casualty thresholds emitted one casualty and erased the rest. It now holds the remainder as a debt and discharges one per call, each taking its own offset within the minute. A minute owing a single casualty draws nothing extra and places it exactly where it did before, so every shipped parameterisation reproduces bit-for-bit. `scripts/check_arrival_rate_fidelity.R` is new and asserts the realised mean, its invariance to `sd_daily`, the run-time bound and the drain, the last on streams far above shipped rates where the ceiling actually binds.

The corrected generation rates carried the base configuration's treated-cohort mortality above the Ajax Bay bound, at 0.528% over 150 replications and 0.513% over 250, an overshoot that survived the larger pool rather than dissolving into it. `p1_p_max` and `p2_p_max` were re-fitted from 0.023/0.019 to 0.020/0.016, with the Morris screening bounds and mode vector rescaled to match; the re-fit is behaviourally inert at seed 42, every attribute and resource trace being identical apart from the recorded `dow_ceiling` value. Both shipped configurations pass the calibration check: `default` 0.443% (95% CI [0.385%, 0.500%]), `moderate_intensity` 0.290% ([0.240%, 0.340%]), the latter unmoved because it overrides both ceilings and so served as the control.

**Seed-42 baseline (30 days, single run):**

| Metric | Before | After |
|---|---|---|
| Total casualties | 382 | 437 |
| WIA (combat + support) | 151 | 187 |
| KIA (combat + support) | 57 | 71 |
| DNBI (combat + support) | 174 | 179 |
| R2B surgical decision point | 118 (49 operated / 69 bypassed) | 152 (74 / 78) |
| R2E first surgeries | 96 | 115 |
| R2E dispositions (in-theatre share) | 151 (24.5%) | 179 (26.8%) |
| Role 4 boarded / queued | 105 / 9 | 114 / 17 |

**Unblocked by this merge:** Issue #206, which the PR raised. Two further issues were raised from this work and are not gated on it: #208, that `run_replications()` draws different replication seeds on its first call in a session, found while investigating a control measurement that should have reproduced exactly and did not; and #207, two configured parameters whose realised effect is clipped.

### Issue 146 — R2B Surgical Section Under-Utilisation: Pre-Open Hold Window ✓

**Merged:** PR #204, branch `claude/issue-146-j6z6gp`

R2B fields one surgical section per team on a 12-hour shift against a theatre available around the clock, so for half of each day the theatre stood ready with nobody rostered to operate in it, and every casualty arriving in that half was diverted to R2E whether the section was due back in a minute or in eleven hours. `r2b.surgery.pre_open_window_min` now sets how long a casualty may instead be held forward for a section about to come on shift: one who finds the theatre free and the section closed for no longer than the window is received into the theatre and waits there, while one who finds the section closed for longer, or the theatre occupied, diverts as before. The OT-busy bypass path is untouched in both count and logic. The seizes do the waiting rather than a timer, a closed section carrying zero capacity, which removes the same-timestamp ordering hazard a timed wait would have carried against the roster's own capacity-change event; seizure order stays bed-then-team on both routes, so the mixed-order deadlock the file already guards against cannot appear. Requiring the bed condition on the hold route as well as the operate-now route bounds the mechanism to at most one held casualty per team at a time. The 60-minute default is an informed estimate anchored between the golden-hour standard above it and the 15 to 45 minute road move to R2E below it, recorded as README Further Development L28. `scripts/check_pre_open_window.R` is new and asserts that a zero window reproduces the instant-diversion model bit-for-bit, that `minutes_to_shift_open()` agrees with the roster at every minute of the day, and that every held casualty is operated on forward rather than diverted anyway.

The seed-42 row is deliberately not the acceptance evidence, and the issue's first acceptance criterion could not have been settled from it: at that seed forward surgeries **fall**, 54 to 49, while seven casualties are held. Across 50 replications at each setting they **rise**, 50.8 (95% CI [49.2, 52.4]) to 56.4 ([54.5, 58.3]), against 5.6 ([5.0, 6.3]) held per run, with off-shift bypasses falling 65.7 to 62.0 and forward theatre utilisation rising 7.4% to 8.3% of room time. The accounting closes, the held count and the surgery rise overlapping almost exactly. The concern the mechanism raises, that a held casualty occupying the single forward theatre displaces the cases behind it, does not show at this window: the theatre-busy bypass count is 7.56 ([6.75, 8.37]) against 7.10 ([6.18, 8.02]). Mortality falls with it, the treated-cohort rate moving 0.627% to 0.436%, both intervals at or below the approximately 0.46% Ajax Bay bound the one-sided check applies, so neither ceiling is adjusted. That comparison is written up in `docs/Multi_Run_Analysis.md`.

One item of the issue's scope was not implemented, and the PR says so rather than closing over it. The issue asks for `r2eheavy.surgery.pre_open_window_min` for parity, with the acceptance criterion that a non-zero value measurably change R2E bypass or utilisation figures. R2E has no surgical bypass for a window to modify: `r2e_treat_wia()` seizes a theatre and a section and waits, `select_r2e_surg_section()` picks an off-shift section only when none is on shift and the casualty then queues for the shift, and with three sections across two shifts one is always open, which `scripts/check_r2e_surgery_seizure.R` measures as 912 seizures with none off shift. R2E already waits indefinitely, the limiting case of an unbounded window, so the parameter would be a guaranteed no-op at every value and satisfying the criterion would first require introducing an R2E bypass that does not exist. Building that mechanism deliberately is follow-up work rather than a parity parameter.

**Seed-42 baseline (30 days, single run):**

| Metric | Post-Issue-148 | Post-Issue-146 |
|---|---|---|
| Total casualties (WIA / KIA / DNBI) | 382 (151 / 57 / 174) | unchanged |
| R2B surgeries | 54 | 49 |
| R2B pre-open hold | — | 7 held, all operated, mean 40.9 min, max 57.1 |
| R2B OT bypass (off shift / busy / total) | 59 / 5 / 64 | 53 / 16 / 69 |
| R2B OT utilisation, 24h room (T1 / T2) | 7.3% / 6.7% | 7.6% / 6.2% |
| R2B OT utilisation, rostered time (T1 / T2) | 14.5% / 13.4% | 13.9% / 11.9% |
| R2E surgeries (first / second) | 98 / 21 | 96 / 38 |
| Casualties operated on (damage control) | 115 (66) | 120 (66) |
| R2E theatre entry deferred | 17 | 7 |
| R2E ICU utilisation (four beds) | 91.7 / 92.7 / 88.4 / 85.4% | 90.0 / 86.0 / 82.1 / 78.9% |
| Realised in-theatre share | 36.7% of 139 | 24.5% of 151 |
| Role 4 boarded / queued / peak | 67 / 21 / 49.0 | 105 / 9 / 67.0 |
| DOW count | 0 | 1 |

The `R2B OT utilisation — rostered time` row changes meaning as well as value: it was computed as twice the 24-hour room figure, the section being rostered 12 hours of every 24, and that identity no longer holds now the theatre carries occupancy during a hold. It is measured directly against the roster from here.

**Unblocked by this merge:** No new issues unblocked. Issue #155 lists this issue as a prerequisite and remains blocked on the five substantive issues still open, being the terminal canonical re-run.

### Issue 148 — Lognormal Casualty Generator Used a Fixed Absolute Rate Cap ✓

**Merged:** PR #202, branch `claude/issue-148-iqwyi5`

`make_ln_arrival_generator()` capped each per-minute lognormal draw at a single fixed absolute value of 5, applied identically to every lognormal stream regardless of that stream's own mean, where `make_exp_arrival_generator()` had used the mean-relative `cap_multiplier × mean_daily` since Issue #54. The fixed cap is replaced with the mean-relative one at the same default multiplier of 3. Unlike the RNG-stream shifts recorded elsewhere in this document, this moves casualty generation itself rather than only the draw order downstream of it, and the direction differs by stream because the retired cap sat at a different multiple of each stream's own mean: nearly non-binding on the low-mean KIA streams at 7.4 times their mean, and binding hard on the higher-mean WIA streams at 2.8 times theirs. Equalising the multiple therefore tightens the KIA cap and loosens the WIA one. Truncation probability across the four shipped lognormal parameterisations moves from a spread of roughly 150-fold to under 6-fold; it does not become uniform, and the README records why, a lognormal's tail above a multiple of its mean depending also on its coefficient of variation, which genuinely differs between streams.

**Seed-42 baseline (30 days, single run):** total casualties 386 to 382, KIA 67 to 57, WIA 149 to 151, DNBI 170 to 174. The pooled died-of-wounds figures were re-measured over 150 replications at three control seeds, the treated-cohort rate rising to 0.436% (95% CI [0.361%, 0.511%]) as the treated cohort grew with the WIA count while the killed-in-action count fell, an interval that spans the approximately 0.46% Ajax Bay bound rather than sitting below it. The 50-replication comparative scenario figures were re-measured with it, both shipped profiles drawing at least one lognormal stream. Every affected row of `CLAUDE.md`'s Key Parameters table was updated at the time of the merge.

**Unblocked by this merge:** No new issues unblocked. Issue #203 was raised from this work, recording that the cap holds realised generation below every stream's configured mean whatever the multiplier, which needs the cap removed rather than retuned.

### Issue 153 — Comparative Scenario Runner Fails Outside a UTF-8 Locale ✓

**Merged:** PR #199, branch `claude/issue-153-g2ilqp`

`scripts/run_scenarios.R` aborted at its plotting stage in any locale that was not UTF-8, after the replications had finished and both comparative CSVs had been written. The scenario labels live in `env_data.json` and `jsonlite` flags what it parses as UTF-8, whereas a pattern written as an R source literal carries the session's native encoding; under a C locale R cannot translate a non-ASCII pattern to match against such a string, and `sub()` rejects it. `plot_scenario_comparison()` now derives each axis label from the scenario's own ASCII identifier through `scenario_short_label()`, which is the route that removes the dependency on the label's punctuation rather than merely working around the encoding. A second defect surfaced during the fix and is not in the issue: the plot title carried a literal em dash, so with the pattern repaired the run exited 0 but drew the title with three raw bytes where the dash belonged, a silent wrong output in place of a loud failure. The title is now a Unicode escape, which is UTF-8-flagged in any locale. `scripts/run_scenarios.R` requests a UTF-8 `LC_CTYPE` at startup and reports its absence there rather than after a full run, covering the one remaining locale-sensitive artifact, which is `write.csv()` escaping the em dash in the `scenario_label` column. The two structurally identical calls in `app.R` take `useBytes = TRUE`. `scripts/check_scenario_labels.R` is new and renders the comparison plot from a synthetic queue table in seconds, asserting that the C-locale PNG is byte-identical to the UTF-8 one; it was verified to fail on each of the two defects reintroduced separately.

The em-dash TOC anchor corruption recorded against the Issue #161 entry below, left unfixed there as the same class of defect, was fixed in the same PR, and proved to be the smaller half of a larger one. `scripts/check_markdown.R` built anchors by stripping `[[:punct:]]` and collapsing whitespace runs, which is locale-dependent as recorded, but also does not describe GitHub's algorithm in any locale: GitHub keeps the hyphen-minus that `[[:punct:]]` removes and replaces spaces one for one where the script collapsed them. Every hyphenated heading therefore carried a broken anchor whatever the locale, `#multirun-replication-framework` for a heading GitHub identifies as `multi-run-replication-framework`. `github_anchor()` now reproduces the algorithm by Unicode property, verified character for character against the ids GitHub itself generates, read from the rendered pages of all three documents; regenerating the tables of contents corrected 45 anchors, and all 137 now match. Fifteen body cross-references carrying the same defect were repaired. Separately, the project's citation format resolved to nothing, GitHub lower-casing the ids it generates and matching case-sensitively: all 216 occurrences were lower-cased and `CLAUDE.md`'s statement of the convention updated with them. The script gained a check that every anchor link resolves to a heading, so none of these classes can return unnoticed.

No model code, no `env_data.json` parameter and no random draw was touched, so no baseline value changes and no tracked artifact was regenerated. This was confirmed rather than assumed: the full 50-replication comparative run documented in `docs/Multi_Run_Analysis.md` was executed with no locale set and reproduced every published figure exactly, writing an `images/scenario_comparison.png` byte-identical to the committed one. The work was done in an unpinned R 4.3.3 sandbox, under the same caveat as the surrounding issues, though nothing here is stochastic.

**Unblocked by this merge:** No new issues unblocked. Issue #155 lists this issue as a prerequisite and its step 5 is now unblocked in isolation, but the issue as a whole remains blocked on the six substantive issues still open, since it is the terminal canonical re-run.

### Issue 195 — `surg_mode` Screened Over a Range Sitting Almost Entirely Above Its Baseline ✓

**Merged:** PR #197, branch `claude/issue-195-fo9gnk`

`surg_mode` was screened over 90 to 150 minutes against a shipped mode of 95, so the range that should have surrounded the baseline began five minutes below it, leaving the baseline 0.083 of the way along its own range. On a four-level grid three of the four sampled values were at least 15% longer than the operation the model performs, and because Morris draws each trajectory's base point from the whole parameter box, every design point ran an inflated theatre occupancy. The consequence therefore reached the rank of any parameter acting through theatre contention, not `surg_mode`'s alone, which is the same mechanism Issue #75 found in `p1_p_max` reached through the held-fixed baseline rather than the bounds. The bounds are now 57 to 133, Rule A around the 95-minute mode, putting the baseline at exactly 0.500.

Investigating the original derivation found it in the earliest README, in a model-assumption block later refactors dropped: duration parameters were bounded at "approximately ±25–50%" of baseline, and this row took the narrow end of that band around the then-shipped 120-minute mode. The Rule A label was applied later, by Issue #112's expansion, to numbers that had never followed it. A second finding explains why this row alone deviated: before Issue #76 the distribution minimum was 90, so Rule A around 120 would have given 72, below the fixed triangular minimum, and would have produced the same `NA` cascade `fr_fill_mode_frac` later caused. Rule A was not merely unused on this row, it was unavailable until Issue #76 dropped the minimum to 41.

The lower endpoint was read against the source rather than accepted as arithmetic. Fetching the systematic review's own tables showed it reports first-look operative time for six included studies rather than the one the parameter registry names, with central estimates of 85, 92, 95, 96, 96 and 100 minutes, so the 95-minute mode is better corroborated than the project had recorded. 57 minutes lies inside every reported range and above the fastest case any of them observed, and both endpoints sit inside the fixed 41 to 210 minute envelope, with 16 minutes of margin below and 77 above.

Two documentation defects found alongside it are corrected in the same PR. The bound-width narrative still described `surg_mode` as a 120-minute baseline spanning 90 to 150, a second surviving reference to the superseded mode; and the died-of-wounds table labelled `r2b_icu_penalty` Rule A while the prose directly beneath it lists that parameter among the three taking their bounds from neither rule. README Further Development L18 loses the off-centre-bound paragraph this issue closes, keeping the entry for the precision and coverage gaps, and the published ranking's staleness note now records that it was measured over the old range.

The audit of the remaining sixty-three rows the issue asked for found no other two-sided row with its baseline near an edge: the lowest is `pri3_dcs_rate` at 0.250 and the highest `pri1_evac_prob` at 0.862, with the three rows at 0.0 one-sided by construction. Twenty-eight rows do not follow their declared rule's width literally, in five patterns, none of them the same class of defect: Rule A rows using the multiplicative width, Rule A rows using the additive width on a probability or factor, Rule A durations at a width other than ±40%, rows clipped at a distribution envelope or a probability ceiling or zero or a neighbouring parameter's range, and the one genuine label error corrected here. The optional `rule` column and its assertion were deliberately not added: a machine-checkable column would need at least six rule values plus per-row clip annotations carrying their reason and limit, and roughly twenty rows would need reconciling before any assertion could pass, which is larger than the defect it would guard. What the README states instead is that the letter records provenance rather than fixing a width, and that the invariant two-sided rows share is a baseline sitting inside its own range.

A separate gap surfaced while verifying the baseline's source and is recorded as Further Development **L26**, not closed. One surgery duration distribution serves every casualty reaching a theatre, at both echelons and on both surgical pathways, while sampling of the United States Department of Defense Trauma Registry puts mean operative asset occupancy at a Role 2 facility between 93.9 minutes for the mildest injury severity band and 182.9 for the most critical, statistically indistinguishable from Role 3. The aggregate is about right, those band means weighted by the registry's own severity mix giving 123.6 minutes against the 115.3 minute mean of the shipped distribution, so the defect is in how theatre time is distributed across casualties rather than in how much of it there is. Closing it properly would need per-severity distributions the source cannot supply, reporting means and standard deviations without medians, ranges or shape; the entry records a severity-keyed multiplier on the single distribution as the lighter alternative. Hall et al. (2023) is added as README reference 64, its open-access status verified as a United States Government work in the public domain.

**Seed-42 baseline:** unchanged. `morris_params` is read only by the screen, and `run.R` never sources `R/sensitivity.R`. The seed-42 single run was executed as a regression check and reproduced every documented post-Issue-173 figure exactly (total casualties 386; R2B routing 114/0/0; OT bypass 55/8/63; 113 operated, 56 damage control; P1 86/50, P2 27/6; `surgery_deferred` 12; 139 dispositions at 32.4%; post-operative stabilisation icu=45 / hold=34; Role 4 75 boarded, 19 queued, peak 49.0, 28 sorties). No value in the CLAUDE.md Key Parameters table changes, and no tracked artifact in `images/`, `logs/` or `data/` was regenerated.

**Verification:** `scripts/check_morris_baseline.R` and `scripts/check_composition_ilr.R` both pass. The envelope was checked directly, the four-level grid over the new bounds being 57, 82.33, 107.67 and 133, all inside 41 to 210, with no non-finite draw in 40,000 `rtriangle()` samples. A Morris design over the corrected box at r = 2 across all sixty-four parameters completed 130 design points and produced no non-finite elementary effect attributable to an invalid triangular draw: the 266 non-finite responses observed are all means over a set empty at the three-day horizon the check ran at, 260 of them on two responses that are non-finite at every design point and therefore cannot depend on a parameter value, and every count and time-weighted-mean response, which has no empty-set failure mode, is finite at all 130 points. Worth recording from that check, a single non-finite point costs all sixty-four elementary effects in the trajectory containing it, because the elementary effects are derived by a linear transform over the trajectory's whole response vector; the outstanding canonical re-run should expect that on any response unable to accumulate observations at its chosen horizon.

**Not closed by this merge:** the published Morris ranking is not re-run. It still reflects the old bounds and now says so. This issue was sequenced ahead of the canonical re-run precisely so that the re-run is measured over the corrected range rather than paid for twice. A control run of the same design at the previous bounds was not executed, so the finding that the non-finite pattern above is unchanged by the bound rests on the diagnosis rather than on a measured comparison.

**Unblocked by this merge:** No new issues unblocked. No open issue lists Issue #195 as a dependency. Issue #155 (final canonical re-run and documentation refresh) remains blocked by its own terms until every issue is closed, and is the issue this work was sequenced to precede.

---

### Issue 186 — Two Screened Parameters Carry a Baseline Outside Their Own Bounds ✓

**Merged:** PR #194, branch `claude/issue-186-na6w6c`

Three rows of `morris_params` carried a `mode` that did not describe the model as shipped. `evacuation_policy_days` held 0.10, a probability left behind when the parameter it replaced was a rate, against a shipped 30 days; `r2e_hold_mode` held 12,960 minutes, the figure predating the threefold rebase of the base recovery-to-duty distribution, against a shipped 38,880. Both also lay outside the bounds on their own row. The third, `surg_mode`, was found by the audit the issue asked for: it held 120 minutes against the 95 both `r2b.surgery.mode` and `r2eheavy.surgery.mode` have carried since Issue #76 re-sourced the damage control surgery duration. That one sits inside its bounds, so a bounds assertion alone would never have seen it, which is the case that justified the second of the two guards. All three are corrected to the shipped values.

Morris reads only `binf`/`bsup` and never this column, which is why the drift survived. `run_sobol()` reads it as the held-fixed value for every parameter not selected into the decomposition, at each of the N x (p + 2) design points, and the Shiny Sensitivity Calibration tab displays it to a planner as `Baseline`. No Sobol result has been published, so nothing published rested on it.

Two guards ship with the values. `morris_params` now asserts `lower <= mode <= upper` on every row at source time, naming the offending rows, so a violation stops every caller of the screen rather than surfacing at design-point evaluation where it is invisible. `scripts/check_morris_baseline.R` asserts the stronger property the first cannot reach: that applying the whole `mode` vector through `apply_params()` leaves `env_data.json` unchanged, which is exactly the condition under which a Sobol run's held-fixed background is the shipped configuration whatever subset that run selects. Its parameter-to-path mapping is derived from `apply_params()` by perturbing each parameter and observing which of the vars tree's 174 leaves move, rather than restated as a second table free to drift from the code it describes; multi-path parameters fall out of that for free and are checked against every path they write. The six balance coordinates are excluded through `MORRIS_MODE_CHECK_EXCLUSIONS`, an explicit list held beside the table, so a parameter added to the screen without an entry fails rather than escapes.

The check was verified against each defect in turn: reverting either out-of-bounds value stops `source()` naming the row, reverting `surg_mode` passes the bounds section and fails the other two naming both paths it writes, and a screened parameter that `apply_params()` never writes is reported by name.

**Seed-42 baseline (30 days, single run):** unchanged, and bit-identically so. Nothing outside `run_sobol()` and the calibration display reads the column, so no RNG stream moves. The run's console log is identical to the tracked `logs/logs.txt` line for line: 386 casualties (149 WIA, 67 KIA, 170 DNBI); 114/0/0 R2B routing; 55/8/63 OT bypass; 113 operated / 56 damage control; P1 86/50, P2 27/6; `surgery_deferred` 12; 139 dispositions at 32.4%; post-op stabilisation icu=45 / hold=34; Role 4 75 boarded, 19 queued, peak 49.0, 28 sorties. No tracked artifact in `images/`, `logs/` or `data/` was regenerated. `check_composition_ilr.R` passes unchanged. Measurements were made in an unpinned R 4.3.3 sandbox under the same caveat as the Issue #18, #23, #161, #154, #156, #160, #159, #173, #178 and #189 refreshes, though the byte-identical log reproduction is stronger evidence of that sandbox's fidelity than earlier refreshes had.

**Not closed by this merge:** `run_sobol()` itself was not executed, the smallest useful decomposition being hours of wall clock in this sandbox; the assertion above is the property such a run depends on, so this is an inference rather than an observation. `surg_mode`'s screening bounds were deliberately left alone: they span 90 to 150 minutes around the corrected 95-minute mode, so the range sits almost entirely above the baseline it should surround, and re-deriving it changes the Morris design and therefore the published ranking, which correcting a held-fixed baseline does not. That is recorded under README Further Development L18 and raised as Issue #195, which should land before the canonical re-run rather than after it, so the screen is not paid for twice. An audit expressing each of the sixty-four baselines as its relative position within its own range puts `surg_mode` alone in this state at 0.083; the three rows at 0.0 are one-sided by construction and the rows above 0.75 are probabilities clipped near one.

**Unblocked by this merge:** No new issues unblocked. No open issue lists Issue #186 as a dependency. Issue #155 (final canonical re-run and documentation refresh) remains blocked by its own terms until every issue is closed, and Issue #195 raised from this work is one more that must close first.

### Issue 189 — Replication Confidence Intervals Treat Antithetically Paired Runs as Independent ✓

**Merged:** PR #192, branch `claude/issue-189-c6qadz`

`run_replications()` paired replications (2k-1, 2k) on a shared seed, the even member negating its arrival-generation uniforms, so the pair rather than the replication was the unit the design supplied while every interval in the project divided by the replication count. The point estimates were unaffected, the mean of paired observations being unbiased; the widths were not. Each replication now draws its own seed, which makes the same `qt(0.975, df = n - 1) * sd / sqrt(n)` arithmetic correctly specified at every one of the roughly thirty call sites without touching any of them.

The pairing was withdrawn rather than extended, on two grounds established by measurement. Extending the negation past the arrival generators is not available: simmer draws service times from the global stream inside its own event loop, in an order set by event timing that the negated arrivals have already changed, so partners have no corresponding draws to reflect. And the scheme bought nothing where it did reach. Over 75 pairs of the shipped base configuration the within-pair correlation on total casualties, the only response the negation touched, was -0.04 (95% CI [-0.27, +0.19]), worth about a 3% variance reduction and indistinguishable from none; on died-of-wounds count it was -0.01 and on R2E ICU mean queue +0.18. Antithetic variates need the response monotone in the input uniforms, and casualty arrivals are not, being produced by a threshold accumulator whose rate is scaled by an effective force size that trajectory outcomes themselves debit and credit. The +0.38 correlation the issue reported does not replicate; single 50-replication measurements of that quantity span -0.25 to +0.65, which is what a 25-pair estimate is worth. The defect fixed is the unit-of-analysis error, which is structural and holds whatever correlation a given seed set realises.

`scripts/check_replication_independence.R` is new, and its final form came out of a failure. A correlation-based version failed on R2E ICU mean queue at lag-1 Spearman +0.176 (p = 0.019). Running the same seeds one mclapply fork per job instead of prescheduled shared forks reproduced the series byte-identically at all three lags, which establishes `run_once()` as a pure function of its seed and so rules out any channel between replications; the same series reads -0.16 (p = 0.03) at lag 3, and the same +0.18 appears under the withdrawn pairing. Independence therefore holds by construction, and a correlation test over 177 pairs adds no evidence about it while failing roughly one run in twenty by chance, deterministically so given fixed control seeds. The check now asserts the two properties that do establish independence, `run_once()` reproducing a seed exactly across an intervening run at another seed and `run_replications()` drawing a distinct seed per replication, and was verified to fail when a shared-seed pairing is reintroduced. It runs in about a minute rather than half an hour, and still reports the lag-1 correlations under `--diagnostic` without gating on them. `run_replications()` now returns the seeds it used, which is what makes the second property checkable and is worth having for reproducibility regardless. `scripts/check_dow_calibration.R` drops its pair folding, and both shipped configurations pass its full 300-replication path, previously unexercised.

The replication count the issue asked to be established is documented in README Further Development L22: the per-replication standard deviation of the treated-cohort died-of-wounds rate is 0.0044, so a 95% half-width of 0.15 percentage points needs 33 replications, 0.10 points needs 73 and 0.05 points needs 292. At the 150 replications the project pools, the half-width is about 0.07 points, which supports two decimal places and no more, so the third decimal place these documents had been using is withdrawn. Gap L9 is deleted from Further Development, the scheme it described no longer existing, and the conflicting `moderate_intensity` died-of-wounds figures are reconciled by pointing `docs/Multi_Run_Analysis.md` at the README's pooled 150-replication figure.

**Seed-42 baseline (30 days, single run):** unchanged. Removing the antithetic machinery consumes no random draws, so the run reproduces bit-identically, verified by diff against a pre-change run: 386 casualties; 114/0/0 R2B routing; 55/8/63 OT bypass; 113 operated / 56 damage control; P1 86/50, P2 27/6; `surgery_deferred` 12; 139 dispositions at 32.4%; post-op stabilisation icu=45 / hold=34; Role 4 75 boarded, 19 queued, peak 49.0, 28 sorties. Every multi-replication figure was re-measured, since the seeds differ between the two designs:

| Metric (base configuration) | Superseded | Re-measured |
|---|---|---|
| Treated-cohort DOW rate (150 reps) | 0.416% (95% CI [0.346%, 0.485%]) | 0.38% (95% CI [0.31%, 0.45%]) |
| Mean DOW/run (150 reps) | 0.95 (95% CI [0.79, 1.10]) | 0.85 (95% CI [0.69, 1.01]) |
| DOW/WIA rate (150 reps) | 0.636% (95% CI [0.531%, 0.742%]) | 0.568% (95% CI [0.461%, 0.675%]) |
| Operated damage control share (50 reps) | 47.8% (95% CI [46.7%, 48.9%]) | 48.3% (95% CI [47.0%, 49.5%]) |
| Realised in-theatre share (50 reps) | 27.0% (95% CI [25.6%, 28.4%]) | 26.8% (95% CI [25.5%, 28.0%]) |
| Post-definitive care ICU share (50 reps) | 46.7% (95% CI [45.0%, 48.4%]) | 48.6% (95% CI [47.0%, 50.1%]) |
| `surgery_deferred` (50 reps) | 11.0 (95% CI [10.0, 12.0]) | 10.1 (95% CI [9.2, 11.1]) |
| R2E ICU four-bed utilisation (50 reps) | 84.6% (95% CI [83.6%, 85.6%]) | 79.7% (95% CI [78.4%, 81.0%]) |

Both shipped configurations now sit below the 0.46% Ajax Bay treated-cohort bound rather than the base spanning it. The bound is one-sided, so sitting below is agreement, and `p1_p_max`/`p2_p_max` are unchanged in both. The movement between the two columns is sampling variation between two sets of control-seed draws rather than an effect of the correction, which leaves every point estimate unbiased. The intervals overlap across most of their length in every row but the last; R2E ICU utilisation moves furthest and is flagged in `CLAUDE.md` as re-measured rather than changed in behaviour, and is now computed by the project's own `utilisation_per_replication()` so it is comparable with how `analyse_replications()` reports it. The comparative scenario table, the transport fleet-size sweep and the forward ICU share sweep were regenerated with `images/scenario_comparison.png`, `images/transport_capacity_margin_by_fleet_size.png` and `images/r2b_icu_share_frontier.png`; no other tracked artifact changed, the seed-42 run being identical. All five regression checks pass. Measurements were made in an unpinned R 4.3.3 sandbox under the same caveat as the Issue #18, #23, #161, #154, #156, #160, #159, #173 and #178 refreshes.

**Not closed by this merge:** the pre- versus post-Issue-43 comparison in `docs/Single_Run_Analysis.md` keeps its original intervals and says so in place, its "before" arm being a configuration no longer in the codebase; the paragraph's conclusion is only reinforced by intervals that should be wider. `high_intensity` died-of-wounds figures remain a single 50-replication measurement rather than pooled. The lag-1 correlation diagnostic was measured on the base configuration only.

**Unblocked by this merge:** No new issues unblocked. No open issue lists Issue #189 as a dependency, and the only issue carrying `status: blocked` is Issue #155 (final canonical re-run and documentation refresh), which is blocked by its own terms until every issue is closed and remains so with eight others open.

### Issue 178 — Died-of-Wounds Ceilings Overshoot the Ajax Bay Calibration Target ✓

**Merged:** PR #190, branch `claude/issue-178-r4a1vk`

The issue asked for `p1_p_max` and `p2_p_max` to be re-fitted to close an overshoot against the Ajax Bay treated-cohort bound, on a reported rate of 0.617% (95% CI [0.503%, 0.732%]) whose interval sat entirely above the ~0.46% target. The overshoot could not be reproduced and neither ceiling moved. Three independent 50-replication measurements of the unchanged base configuration returned 0.348%, 0.400% and 0.499%, pooling to 0.416% (95% CI [0.346%, 0.485%]) over 150 replications, an interval that spans the bound. `moderate_intensity` measures 0.274% (95% CI [0.216%, 0.331%]) over the same protocol, below the bound rather than spanning it, and is also left unchanged: the comparator is an upper bound, because "over 650" is inexact, so sitting beneath it agrees with the historical record and raising the ceilings to lift the interval onto it would add modelled deaths no source evidences. The issue's own acceptance criteria admit this outcome, which is why it closes here rather than deferring.

Two measurement faults produced the false signal, and both are now recorded rather than absorbed. Died of wounds averages about one death per replication, so a single 50-replication measurement does not resolve it; the spread across measurements of one unchanged configuration is 0.151 percentage points, wider than the interval any one of them reports. And `run_replications()` pairs replications on a shared seed while negating only the arrival-generation uniforms, so partners share an unnegated trajectory stream and correlate positively on mortality (+0.38 measured) against the negative correlation asserted at `R/replication.R:204`, while every interval in the project divides by the replication count as though the runs were independent. `scripts/check_dow_calibration.R` was added to assert the comparison that had previously only been made by hand at each recalibration: it pools independent measurements, treats the antithetic pair as the unit of analysis, and tests the bound one-sidedly so a configuration below it passes.

**Seed-42 baseline (30 days, single run):** unchanged. No model code, no `env_data.json` parameter and no RNG stream change, so no tracked artifact under `images/`, `logs/` or `data/` was regenerated and the Morris bounds for both ceilings stand, nothing having been re-fitted. The seed-42 run reproduces every documented post-Issue-173 figure exactly (386 casualties; 113 operated, 56 damage control; P1 86/50, P2 27/6; `surgery_deferred` 12; 139 dispositions at 32.4%; Role 4 75 boarded, 19 queued, peak 49.0, 28 sorties), and a 50-replication `moderate_intensity` measurement at seed 42 reproduces `docs/Multi_Run_Analysis.md`'s comparative figures to every digit including both confidence bounds (0.66 DOW/run, DOW/WIA 0.444%, 95% CI [0.286%, 0.601%]), which is what establishes that the discrepancy was not an artefact of the measurement environment. All three pre-existing regression checks pass. Measurements were made in an unpinned R 4.3.3 sandbox under the same caveat as the Issue #18, #23, #161, #154, #156, #160, #159 and #173 refreshes.

**Not measured by this merge:** the sensitivity screen the issue's first task asked for. Its purpose was choosing a lever for a re-fit that is not happening, and the treated-cohort rate is not among the thirty-six screened responses in any case. Two structural findings are recorded in its place and neither is actioned: `r2b_icu_penalty` is inert at the shipped `r2b.post_op_icu.share` of zero, and `r2e_postop_hold_penalty` compounds to 9x for a casualty missing an intensive care bed at both episodes (`R/trajectories.R:1565` and `:1657`), a squaring the 3.0 informed estimate was never validated for. `high_intensity`'s died-of-wounds figures were not re-measured and remain one draw, annotated as such. The full 300-replication path of the new check is itself unexercised, having been verified only in `--quick` mode.

**Unblocked by this merge:** No new issues unblocked. Issue #189 was raised from this work, recording the antithetic-pairing interval defect across `summarise_replications()`, `summarise_scenario_totals()` and the `ci_mean()`/`ci_by_group()` path, together with the two mutually inconsistent `moderate_intensity` died-of-wounds figures it left in `README.md` and `docs/Multi_Run_Analysis.md`.

### Issue 158 — Nine Simplex-Constrained Composition Parameters Unscreened ✓

**Merged:** PR #187, branch `claude/issue-158-ymxemk`

The R1 triage priority split, the DNBI sub-type composition and the mass casualty priority split are each constrained to sum to one, and were held out of Morris screening because a one-at-a-time design cannot vary a member of such a group without either leaving the simplex or applying a renormalisation that is itself a design decision affecting the result. Nine parameters therefore carried no µ\*, no σ and no ranking, and the gap was not neutral: the two highest-ranked parameters in the published screen, `pri1_evac_prob` and `pri1_surg_prob`, are both conditional on a casualty being Priority 1, while the parameter deciding what share of casualties are Priority 1 was unscreened. They are now screened through the isometric log-ratio transform, which maps each three-part composition onto two unconstrained balance coordinates and back, so the sum-to-one constraint holds by construction rather than by correction. The nine become six coordinates inside the existing Morris design at the same cost per trajectory, taking the screened set from fifty-eight parameters to sixty-four. The transform is implemented directly (`ilr3()`/`ilr3_inv()`) rather than by adding the `compositions` package, the three-part case being two lines each way against a dependency with a large transitive tree and no other use in the project.

Bounds are derived by transforming a compositional range rather than stated in coordinate space, where a number carries no intuitive meaning: the first coordinate of each group transforms a plausible range for the leading part's share, holding the ratio of the two trailing parts at baseline, and the second applies Rule B multiplicatively to that ratio. All three ranges are documented as informed estimates and are deliberately wider than either rule would give, the triage split resting on ADF planning norms with no open-access source and the disease share being a residual. `apply_params()` back-transforms each pair before every run and asserts three strictly positive parts summing to one; `scripts/check_composition_ilr.R` asserts the same across a whole Morris design, at each corner of each coordinate box, and over Dirichlet draws. `run_sobol()` gains the confirmatory path: a group represented in the selected parameters is sampled as whole compositions from a Dirichlet centred on its baseline, at a concentration matched to the same range the Morris bounds span, so the decomposition reports how much variance the composition explains rather than how much each contrast does.

**Seed-42 baseline (30 days, single run):** unchanged. No trajectory, environment or `env_data.json` change and no RNG stream shift; the diff reaches no file under `images/`, `logs/` or `data/`, and nothing in `R/sensitivity.R` outside the screening path executes during an ordinary `run.R` invocation. All three regression checks pass: `check_composition_ilr.R` (650 design points of an r = 10 design all writing valid compositions, coordinate modes reproducing the shipped configuration exactly), `check_r2e_surgery_seizure.R` (749 seizures, none off shift) and `check_icu_time_conservation.R`. Verification of the screen itself was by smoke run at r = 3, three replications, five days: 195 design points, zero failed outright, finite µ\* and σ for all six coordinates with an elementary effect from every trajectory, and all sixty-four parameters in one ranking with the six interleaved by µ\*.

**Not measured by this merge:** the production ranking, and with it three of the issue's acceptance criteria — the full sixty-four-row table, the explicit comparison of the triage split against `pri1_evac_prob` and `pri1_surg_prob`, and the conditional Dirichlet-sampled Sobol decomposition. A production screen is r × (p + 1) = 1,300 design points at five replications, and this issue's development environment had neither the pinned container nor the compute. This follows the issue's own sequencing note asking that the screening run be executed as part of the canonical re-run in #155 so one sweep produces a complete ranking rather than several partial ones; #155 was updated with an Impact of Issue #158 section and three implementation tasks recording it. The smoke ranks are deliberately published nowhere: the same run puts `pri1_evac_prob` at rank 50 against rank 1 in the published thirty-day ranking, which is direct evidence that r = 3 over five days cannot rank parameters.

**Unblocked by this merge:** No new issues unblocked. Issue #155 remains blocked on the closure of all remaining issues. Issue #186 was raised from this work, recording that `evacuation_policy_days` and `r2e_hold_mode` each carry a `morris_params$mode` outside their own bounds, residue from Issue #156; Morris never reads that column, but `run_sobol()` uses it as the held-fixed baseline for unselected parameters and the Shiny Calibration tab displays it as `Baseline`.

### Issue 157 — Morris Screening Evaluated Seven KPIs Selected by Availability ✓

**Merged:** PR #184, branch `claude/issue-157-ly2opk`

The Morris screen ranked fifty-eight parameters against seven response variables: five queue measures, a transport utilisation, and a death count. Those seven were the measures reachable from `summarise_replications()`'s resource-monitor output plus a count read off the attributes monitor, while `README.md`'s Model Outputs section separately defines seventeen KPIs across seven planner decision domains, each selected against five documented criteria (C1 doctrinal standard compliance through C5 health outcome attribution). Exactly one of the seven mapped cleanly onto a documented KPI and two had no counterpart at all, so parameter rankings were reported against a set chosen by what was convenient to compute rather than by argument. The practical consequence was that no time-to-care, return-to-duty, bypass or strategic evacuation response was screened: a parameter that delays time to first surgery without lengthening any queue, which is what happens when beds and theatres are adequate but transport cadence or triage thresholds are not, was invisible to the screen.

The response set is now the Model Outputs set. `extract_kpis()` reads the arrivals and attributes monitors alongside the resource monitor, reusing `build_attributes_wide()`, `compute_role4_census()`, `compute_ame_demand()`, `compute_ame_backlog()` and `compute_ame_sorties()` from `R/analysis.R` rather than restating their derivations, and returns thirty-six responses covering all seventeen KPIs, plus `system_ot_q` and `transport_util` retained and documented as derived aggregates. A new `morris_kpis` registry carries each response's label, decision domain, C1–C5 criteria mapping and scalar reduction, so the set is self-documenting in code; the reductions are a methodological choice and are recorded per response, with mean and p90 screened separately on time to first surgical incision because the doctrinal two-hour standard is a tail property. Counts are normalised to per-replication means so a response holds its scale across replication counts. A ranking CSV is now written per response rather than for the primary alone, closing the gap that forced findings on secondary responses to be read off saved plot images, and each file carries the diagnostics needed to tell an uninformative response from an uninfluenced parameter: a response with no usable variation across the design is marked degenerate with µ\* and σ written as `NA` rather than zero, alongside the response's mean and standard deviation, the count of design points at which it could not be measured, and the count of finite elementary effects per parameter. Two consequences of the expansion were fixed in the same PR: `run_morris()` wrote its plots to a hardcoded `images/`, which at one plot per response would have scattered twenty-nine untracked files through the tracked baseline directory, so `images_dir` now defaults to a gitignored location under `output_dir` with an `--images-dir` flag to opt into refreshing the tracked plots; and the app's Morris panel was headed with one response while the ranking table and CSV beneath it carried another.

**Seed-42 baseline (30 days, single run):** unchanged. No trajectory, environment or `env_data.json` change, and no RNG stream shift; `git diff` against `main` touches no file in `images/`, `logs/` or `data/`. Both regression scripts exit 0 (`check_r2e_surgery_seizure.R`: 749 surgical seizures, none off shift; `check_icu_time_conservation.R`: all conservation and route-agreement assertions across every forward ICU share). Verification of the screen itself was by smoke run at r = 3, 3 replications, 5 days: 177 design points in 1,755 s, 37 ranking CSVs and 36 plots written, every response either carrying finite µ\*/σ for all fifty-eight parameters or flagged degenerate for all fifty-eight, with none falling between. Seven were flagged, each for a reason a five-day run explains. The wider set costs 2.3 s per design point against a 28.4-second simulation at five replications over 30 days, 7.9%, confirming that the sweep is shared and response count is not a compute driver.

**Not measured by this merge:** the production screening run. Issue #157 sequences it with the canonical re-run in #155, and it would take roughly 2 to 2.5 hours at 295 design points. The published fifty-three-row ranking table is therefore left as it stands, with its existing staleness note extended to record that thirty-five of the thirty-six responses now screened have no ranking measured at production trajectory count. The smoke run does indicate the responses are not redundant, at a Spearman rank correlation of 0.55 between the system OT queue and mean time to first surgical incision, with `disease_surgery_pct` at 8th on queue depth against 55th on time to surgery; that is recorded in the README as an indication rather than a finding.

**Unblocked by this merge:** No new issues unblocked. Issue #155 remains blocked on the closure of all remaining issues, not on this one alone.

### Issue 180 — `check_r2e_surgery_seizure.R` Structural Check Inert ✓

**Merged:** PR #182, branch `claude/issue-180-8nhgcf`

The regression check guarding R2E surgical section seizure located its blocks in the printed trajectory by the literal `"R2E DAMCON Surgery"`, a label dropped in PR #176 when the surgical pathway split meant an R2E procedure was no longer necessarily damage control. The rename was correct; the check's pattern was not updated with it. The consequence went beyond one failing assertion: with no block matching, the structural half hit its empty-match guard and `next`-ed past the rest of the loop body, so neither the per-block seize/release assertions nor the `r2e_surgery_1_start` / `r2e_surgery_2_start` branch-coverage assertions had run since PR #176 merged, while the script reported a failure that read as though the model had lost its surgery blocks. A guard for a defect that has already occurred twice, reporting the wrong alarm and asserting nothing, trains reviewers to disregard its exit status.

The fix does not substitute the new literal, which would reproduce the same fragility. The name format now lives once, as `R2E_SURGERY_SECTION_FMT` in `R/trajectories.R`, used at the `sprintf()` call site in `build_r2e_surgery_block()`; the check derives its matching regex from that constant, escaping regex metacharacters in the literal parts and turning the `%d` into a capturing group so the section index can be read back off each matched header. A rename can therefore no longer desynchronise the two. Check-integrity failures are accumulated and reported separately from model failures, since "the check cannot see the model" and "the model is wrong" call for different responses, and a new assertion requires the matched blocks to cover every configured surgical section evenly, so a dropped branch or an uncovered section fails loudly rather than quietly shrinking what is checked. `scripts/check_icu_time_conservation.R` was audited for the same class of coupling and has none: it matches on simmer attribute keys and `env_data` paths, which are code identifiers rather than display prose.

**Seed-42 baseline (30 days, single run):** unchanged. The substituted constant produces the same string, so no RNG stream shifts; the run was fingerprinted before and after the change and is bit-identical (292 arrivals, sum of end times 6692161.49955743, sum of activity times 1222370.02189731, to fifteen significant figures). Both regression scripts now exit 0: the structural half reports the 12 section blocks (3 sections × 4 surgery sites) at 7 seizes and 7 releases each with both surgery-start attributes covered, and the behavioural half 749 surgical seizures with none off shift. Three negative tests were run and reverted, confirming that renaming the constant alone leaves the check passing, that a desynchronised name is reported as a check-integrity problem, and that detaching a surgery branch from its section fails with a message naming the affected blocks.

**Unblocked by this merge:** No new issues unblocked.

### Issue 147 — `ot_hours` Missing from `env_data.json` ✓

**Merged:** PR #179, branch `claude/issue-147-9n1u7u`

Every simulation parameter but one lived in `env_data.json` and reached the Shiny Configure panel through the `R/app_params.R` registry. `ot_hours`, the length of the first operating theatre shift, was the exception: a bare `= 12` function default repeated independently in six files, with no entry in the JSON and therefore none in the registry. The Configure panel could not display, edit or persist it, and `app.R`'s slider borrowed its bounds from the Morris screening table, an unrelated structure pressed into service as a workaround. Six copies of one value is a latent-drift risk in its own right, and the parameter is not a minor one: `ot_hours` tops the Morris screen at µ\* = 0.978, the most influential controllable lever the model has over surgical throughput.

The value now lives at `vars.surgical_roster.shift.ot_hours`, read by a single accessor, `get_ot_hours()` in `R/environment.R`. A dedicated `surgical_roster` element was added rather than folding the field into an existing echelon block, because one shift length rosters R2B's single surgical section and all three of R2E's, so it belongs to neither; keeping it on the standard `elm.acty.var` path is what lets `var_field()`, `apply_params()` and `get_var_value()` all reach it without special-casing. Every call site that carried its own literal now defaults to `NULL`, meaning "use the configured value", while still accepting an explicit argument that overrides for one call. `apply_params()` writes the screened value into the vars tree like every other Morris parameter, so `eval_params()` no longer extracts and threads it separately.

The Run tab's duplicate slider was removed rather than rewired. Keeping it would have left two controls for one parameter, which is the drift this issue set out to close, and the Run tab control was the more dangerous of the two: it was passed as an explicit argument, so it beat the configured value, but nothing wrote it back, meaning a run made at a non-default shift length saved a configuration that could not reproduce it. A shift length is a property of the health system being simulated rather than of one execution, so it now sits in Configure with every other model parameter. `scripts/shiny_worker.R`'s `--ot-hours` flag went with it, having lost its only caller.

**Seed-42 baseline (30 days, single run):** unchanged. This is a refactor and shifts no RNG stream; the run reproduces every documented post-Issue-173 figure exactly (386 casualties; R2B routing 114/0/0; OT bypass 55 off-shift / 8 busy / 63 total; 113 operated, 56 damage control; `surgery_deferred` 12; 32.4% of 139 dispositions retained in theatre; Role 4 75 boarded, peak 49.0). Verification that the configuration is genuinely authoritative came from changing the JSON alone: at 16 hours the run gives 384 casualties and 66 off-shift bypasses, at 20 hours 385 and 62. The Shiny app was driven in a real browser for this merge (Playwright and Chromium), confirming the Run tab offers no OT control, the Configure field renders bounded 8-16 at 12, *Save Configuration* captures an edit, and Quick Run at 12 and 16 hours reproduces the command-line figures of 386 and 384 exactly.

**Unblocked by this merge:** No new issues unblocked. Issue #180 was raised from this work, recording that `scripts/check_r2e_surgery_seizure.R` has been failing on `main` since PR #176 renamed the trajectory its structural half greps for; the check was confirmed to pass at the commit that introduced it, and the model invariant it guards still holds.

### Issue 173 — Every Casualty Requiring Surgery Routed Through the Damage Control Sequence ✓

**Merged:** PR #176, branch `claude/issue-173-6mbcko`

Every operated casualty took the full staged damage control sequence: an abbreviated operation, a stabilisation phase in intensive care, a definitive repair, and post-definitive intensive care. Real practice reserves that sequence for a minority. Across six United States Level 1 trauma centres 24% of 872 emergent laparotomies were damage control, with institutional rates of 16% to 34%, and no reported series approaches 100%. The model was therefore giving every casualty the most expensive pathway, which inflated exactly the capacity finding Issue #159 had just surfaced.

A `pri1_dcs_rate` / `pri2_dcs_rate` / `pri3_dcs_rate` family in `r1.other` (0.55 / 0.20 / 0.05) now assigns each casualty a pathway, drawn once in `build_casualty_trajectory()` where surgical candidacy itself is decided so both echelons read one value: a casualty's physiology does not change because a forward theatre happened to be free. A single-stage casualty takes one theatre episode and one intensive care episode, having no stabilisation phase between operations and no second procedure, and needs no R2E operation at all when their definitive repair was performed forward at R2B. Post-definitive care is unchanged and served on both pathways. The rates follow the established `pri*_surgery` pattern, are registered in `R/app_params.R` as sliders with a sourced tooltip, and join `morris_params` as Context, moving the screen from fifty-five parameters to fifty-eight.

The treatment efficacy question the split raises was resolved without a new parameter. A single-stage operation earns the product of the two multipliers the staged pathway earns across its two operations, both routes ending with the definitive repair complete; what separates them is elapsed time, which `dow_prob_conditional()` already prices at every checkpoint. Giving the single-stage operation only the abbreviated multiplier would have left a casualty who needed no staging at a higher residual mortality ceiling than one who did, inverting the indication and raising DOW as a bookkeeping artefact. `r2e_dcs2_factor` is unchanged, though the population it acts on shrinks to damage control casualties operated on at R2E.

A rate of exactly zero or one consumes no random draw, a degenerate Bernoulli trial having only one outcome, so setting all three to 1.0 reproduces the pre-change model bit-identically, including the sum of arrival end times to fifteen significant figures. The split is a generalisation of the previous model rather than a replacement for it, and that is demonstrable rather than merely asserted. `scripts/check_icu_time_conservation.R` is now pathway-aware: the conservation universe is the damage control cohort, single-stage casualties are asserted to draw no stabilisation requirement and never return to theatre, and a new check asserts every operated casualty reaching R2E disposition receives post-definitive care on either pathway.

Two findings move as a result. The forward ICU share frontier, re-swept at 20 replications per point, is now flat across the whole range with every confidence interval overlapping: only the damage control cohort has a stabilisation phase to move, so the monotonic gain the previous frontier showed was measured against a cohort roughly twice the real size. And the 50-replication treated-cohort DOW rate falls from 0.876% to 0.617% against the ~0.46% Ajax Bay target, recovering most of the overshoot Issue #159's post-definitive episode had introduced without reaching the target; `p1_p_max` and `p2_p_max` were left unchanged, since re-fitting ceilings to absorb a structural change hides the change rather than validating it, and the residual overshoot is recorded in Further Development L22. Further Development L25, from which this issue was raised, is deleted and its citations repaired.

**Seed-42 baseline (30 days, single run):**

| Metric | Post-Issue-159 | Post-Issue-173 |
|---|---|---|
| Total casualties | 385 | 386 |
| Surgical pathway split | not modelled (all staged) | 113 operated: 56 damage control (49.6%), 57 single-stage |
| R2B surgeries | 55 | 50 |
| R2E surgeries (first / second) | 122 / 58 | 87 / 22 |
| Surgeries deferred (ICU saturated, P2+) | 22 | 12 |
| R2E ICU utilisation (per bed) | 97.4 / 94.4 / 93.5 / 88.7% | 93.3 / 88.4 / 83.6 / 73.0% |
| Post-operative pathway (stabilisation) | icu=52, hold=70 | icu=45, hold=34 |
| Post-definitive care pathway | icu=37, hold=72 | icu=55, hold=45 |
| Realised in-theatre share | 24.5% of 143 | 32.4% of 139 |
| DOW count | 2 (r2b=1, r2e_postop=1) | 1 (r2b=1) |
| Mean DOW/run (50-rep) | 1.90 (95% CI [1.53, 2.27]) | 1.37 (95% CI [1.10, 1.63]) |
| Treated-cohort DOW rate (50-rep) | 0.876% [0.702%, 1.050%] | 0.617% [0.503%, 0.732%] |

**Unblocked by this merge:** No new issues unblocked.

### Issue 159 — Post-Operative ICU Time Not Conserved Across Treatment Location ✓

**Merged:** PR #174, branch `claude/issue-159-b1fzye`

A casualty's post-operative intensive care requirement follows from the injury, so the total should not depend on which echelons deliver it. The model conserved surgical time and not intensive care time: R2B delivered no post-operative intensive care at all, while R2E separately shortened its own episode for exactly the casualties R2B had operated on, discounting an episode that never occurred. An R2B-operated casualty received roughly 28% of the ICU time an otherwise identical R2E-operated one did. The requirement is now drawn once and divided between the echelons by a new `r2b.post_op_icu.share`, so the total is conserved by construction at every setting rather than by two parameters staying consistent with each other.

Fixing that exposed a larger gap. Damage control needs intensive care at two separate points, stabilisation between the abbreviated operation and the definitive repair, and post-definitive care after the final operation, and the model had only one episode. No casualty on any route was receiving intensive care after their definitive repair; the retired `short_icu` was in part intended to serve that purpose but was wired as a probabilistic shortened first stay and at 60 minutes was a recovery-room duration rather than an intensive care one. Both episodes are now modelled separately, `long_icu` becoming `stabilisation_icu` and `post_definitive_icu` being new, and the sequencing defect that followed is fixed with them: stabilisation was landing after both operations on the R2B route and is now served before the definitive procedure. Post-definitive care is never served forward, since R2B performs no definitive repair, which is what stops the forward-holding lever from hollowing it out as the share rises.

Three planner levers ship with it: the forward share, a `forward_hold_max` time cap (a commander sets forward holding in hours, not fractions, and the cap binds ahead of the share so zero disables forward holding outright), and the post-definitive duration. `r2b_icu_penalty` (1.31) prices the capability difference of an R2B section without an intensivist, sourced to the pooled ICU-mortality odds ratio for open-format against intensivist-led units. Validating the durations found the stabilisation minimum of 770 minutes traceable to no source; it is corrected to 360 per the WSES position paper's stated 6-72 hour range, with the mode and maximum both independently confirmed. Two new scripts ship: `check_icu_time_conservation.R`, a regression check asserting both invariants across all three routes at four shares, and `run_icu_share_sweep.R`, which reports the decision frontier. The Morris set moves from fifty-three parameters to fifty-five.

The headline finding is that four R2E intensive care beds cannot cover both episodes: only 37 of 109 casualties receive post-definitive care in an intensive care bed rather than the degraded holding-bed fallback. The forward-holding sweep shows the lever works, though not through the queue, which stays saturated at every setting: the share of casualties receiving post-definitive care in a real bed rises monotonically from 35.5% to 47.5% with non-overlapping confidence intervals at the ends. The mortality side of the trade remains unresolved at 20 replications, so the acceptance criterion that DOW rises with the share is reported as unmeasured rather than met, and the default ships at zero.

**Seed-42 baseline (30 days, single run):**

| Metric | Post-Issue-160 | Post-Issue-159 |
|---|---|---|
| Total casualties | 386 | 385 |
| R2E ICU utilisation (per bed) | 82.8 / 87.2 / 61.1 / 54.2% | 97.4 / 94.4 / 93.5 / 88.7% |
| Post-operative pathway (stabilisation) | icu=85, hold=38 | icu=52, hold=70 |
| Post-definitive care pathway | not modelled | icu=37, hold=72 |
| Surgeries deferred (ICU saturated, P2+) | 6 | 22 |
| DOW count | 1 (r2b=1) | 2 (r2b=1, r2e_postop=1) |
| R2B surgeries | 46 | 55 |
| R2E surgeries (first / second) | 123 / 73 | 122 / 58 |
| Realised in-theatre share | 27.1% of 166 | 24.5% of 143 |

**Unblocked by this merge:** #173 (every casualty requiring surgery is routed through the damage control sequence), raised from this issue's Further Development entry L25 and blocked on it.

### Issue 160 — AME Sortie Configurations Understate C-17A Capacity ✓

**Merged:** PR #171, branch `claude/issue-160-2phtt5`

The two strategic AME aircraft configurations shipped with capacities no source supported: 2 critical with 8 standard places, or 0 critical with 20 standard. The RAAF states that an AME-configured C-17A "can transport 54 ambulatory and 36 high dependency stretcher patients", two categories that map onto the model's critical and standard pools directly, so the shipped critical capacity was eighteen times below the airframe's stated fit. Because the critical pool governs the model's evacuation findings, the headline result that most evacuation-bound casualties remained in theatre at day 30 was a consequence of the assumed capacity rather than of anything the theatre did.

The two configurations, and `select_ame_configuration()`, were replaced by a single named airframe carrying its published fitted capacity. Each aircraft holds its own `role4.airframe_<id>` block and `role4.ame.airframe` names the one flown, so a scenario profile or the Shiny selector changes aircraft by naming one rather than overwriting sourced numbers; `resolve_ame_airframe()` (`R/environment.R`) resolves the selection and errors on an unknown name. Three aircraft ship: the C-17A Globemaster III at 36 critical and 54 standard, the C-130J-30 Hercules at 0 and 97, and the C-27J Spartan at 0 and 21. Only the C-17A's figures split by acuity in the source; the other two are given a single stretcher total, so both carry a critical capacity of zero as the literal reading. The C-130J's 97 places, higher than the larger C-17A's 90, was checked against the USAF C-130 fact sheet, which reports the same figure for the stretched C-130J-30 airframe the RAAF operates.

Retiring the two-configuration abstraction was recorded as a design decision rather than a side effect: the source describes one aircraft carrying both categories on the same sortie, Configuration B had no counterpart in any source, and at sourced capacity the selection rule stops discriminating between loadouts. The sortie timeline output reports flown or cancelled in place of the configuration selected. Limitation L17 was rewritten: R2E holding beds remain heavily loaded over a 90-day run at 88% occupancy, but the evacuation wait now accounts for about a third of that and in-theatre recovery for the rest, so the binding constraint is the bed count rather than airlift. The Welch warm-up analysis was re-run, the comparative scenario analysis re-run at 50 replications per scenario, and every 50-replication row in `CLAUDE.md` refreshed, clearing flags several of them had carried since Issue #73. The Morris screen was not re-run; the airframe capacities remain outside it, with the rationale recorded, and `images/scenario_comparison.png` is still blocked on Issue #153.

**Seed-42 baseline (30 days, single run):**

| Metric | Before (post-Issue-156) | After (post-Issue-160) |
|---|---|---|
| Total casualties | 385 | 386 |
| Strategic evacuation decisions | 83 | 121 |
| Boarded and reached Role 4 | 25 | 105 |
| Still queued at R2E on day 30 | 58 | 16 |
| Critical-pool mean wait | 6.0 days | 1.1 days |
| Role 4 peak occupancy | 17.0 (day 15) | 73.0 (day 21) |
| R2E ICU utilisation (4 beds) | 78.7 / 92.6 / 61.1 / 71.0% | 82.8 / 87.2 / 61.1 / 54.2% |
| R2E ICU queue ≥1 | 30.6 / 71.1 / 10.6 / 39.6% | 23.3 / 24.7 / 0 / 0% |
| Post-operative pathway (icu / hold) | 72 / 47 | 85 / 38 |
| `surgery_deferred` | 20 | 6 |
| R2B surgeries | 49 | 46 |
| R2E surgeries (first / second) | 119 / 48 | 123 / 73 |
| Realised in-theatre share | 30.3% of 119 dispositions | 27.1% of 166 |
| Treated-cohort DOW rate (50-rep) | 0.442% [0.316%, 0.568%] | 0.530% [0.382%, 0.678%] |
| Welch ICU-queue CMA | 1.18 | 0.303 |

**Unblocked by this merge:** No new issues unblocked. Issue #155 remains blocked pending closure of every other open issue.

---

### Issue 156 — R2E Disposition Drawn Independently of Severity; AME Evacuees Re-Occupy ICU Beds ✓

**Merged:** PR #169, branch `claude/issue-156-tz1umh`

Two coupled defects in the R2E Heavy trajectory's Phase 5 were fixed by one mechanism. Disposition decided in-theatre recovery against strategic evacuation with `sample(1:2, 1, prob = c(in_theatre_rate, 1 - in_theatre_rate))`, a draw containing no `get_attribute()` call at all, so a casualty who spent thirty minutes in ICU was exactly as likely to be evacuated as one who spent thirty-six hours. Separately, Priority 1 surgical evacuees seized an ICU bed a second time and held it for the whole evacuation wait, a mean of roughly 12.8 days, after the model had already recorded their clinical care as concluded. Together these made the project's headline finding, that R2E ICU is the binding constraint, substantially an artefact of bed accounting rather than of clinical demand.

`draw_recovery_to_duty()` now draws each casualty an expected recovery-to-duty duration at the close of R2E clinical care, scaled by a severity factor keyed to the same four categories that already set the Role 4 ward and length of stay, and the casualty is retained in theatre when that duration falls within `evacuation_policy_days`, shipped at the doctrinal 30 days. A retained casualty then holds its bed for exactly the duration that retained it. Casualties awaiting strategic AME stage in holding beds on both routes; only the ventilated share of the critical pool, shipped at 15%, holds an ICU bed, for a bounded pre-flight period that seizes the holding bed before releasing the ICU bed. In `env_data.json`, `in_theatre_rate` was replaced by `evacuation_policy_days`, `recovery_to_duty` and `critical_hold` were added, and `holding` was rebased by a factor of three to serve as the base recovery-to-duty distribution, with the Morris `r2e_hold_mode` bounds rescaled to match.

Three findings changed qualitatively rather than numerically. R2E ICU is no longer the binding constraint; the second-shift surgical section is, and the multi-run R2E OT queue rises 143-fold under Okinawa intensity because a casualty holds a theatre while waiting for staff. The post-operative pathway split reverses. And the evacuation policy becomes a usable planning lever, 15, 30 and 60-day policies giving in-theatre shares of 7.0%, 30.3% and 70.9%. Two limitations were recorded against the work itself: the four severity factors are informed estimates tuned so the realised share lands inside a wide historical range, which is a weak validation test (Further Development L23), and the residual rise in the Welch ICU queue is bed-blocking from a saturated holding pool rather than the original defect (L17, rewritten). The Morris screen was not re-run and its ranking table is flagged as naming two entries now stale in definition; `images/scenario_comparison.png` was not regenerated because the plotting stage fails outside a UTF-8 locale (Issue #153).

**Seed-42 baseline (30 days, single run):**

| Metric | Before (post-Issue-161) | After (post-Issue-156) |
|---|---|---|
| Total casualties | 387 | 385 |
| Priority split (P1/P2/P3/KIA) | 202 / 64 / 53 / 67 | 202 / 72 / 44 / 67 |
| DOW count | 2 (r2b=2) | 1 (r2b=1) |
| total_rtd | 148 | 140 |
| R2E first / second surgeries | 103 / 55 | 119 / 48 |
| R2E post-op pathway | icu=9, hold=94 | icu=72, hold=47 |
| `surgery_deferred` | 24 | 20 |
| R2E ICU utilisation | 100.0 / 100.0 / 98.8 / 99.8% | 78.7 / 92.6 / 61.1 / 71.0% |
| R2E ICU queue ≥1 | 95.5 / 95.0 / 94.3 / 95.5% | 30.6 / 71.1 / 10.6 / 39.6% |
| Realised in-theatre share | not an output | 30.3% (50-rep 28.9%, 95% CI [27.7%, 30.1%]) |
| Role 4 decisions / evacuated / queued | 116 / 40 / 76 | 83 / 25 / 58 |
| Welch ICU-queue CMA (10×90d) | 30.6, 0% decreasing | 1.18, 9.0% decreasing |
| Treated-cohort DOW rate (50-rep) | 0.633% [0.392%, 0.874%] | 0.442% [0.316%, 0.568%] |

Regenerated in an unpinned R 4.3.3 sandbox, no Docker being available to build the pinned Dev Container; the pre-change configuration was re-run first and reproduced every documented post-Issue-161 figure exactly as the validation step. Both DOW ceilings were left unchanged, the corrected interval still spanning the ~0.46% Ajax Bay target.

**Unblocked by this merge:** No new issues unblocked. Issue #155 remains blocked on Issue #153, and its scope was extended to absorb this change: the Morris re-run became mandatory rather than a provenance formality, and three new parameters need calibration review.

### Issue 152 — DOW Calibration Target Not Supported by Its Cited Sources ✓

**Merged:** PR #167, branch `claude/issue-152-thrj8s`

The DOW model's headline calibration target, "3 DOW / 580 WIA ≈ 0.52%", could not be traced to either source cited for it. All four defects in the issue were confirmed against the sources directly: neither reference reports 580; the cohort was described as British wounded in action where reference [14] states the casualties were "from both sides"; the quantity is a treated-cohort mortality rate rather than a DOW/WIA rate; and the "255 KIA : 777 WIA" campaign totals were attributed to a reference that does not report them. A fifth defect not raised in the issue was found in the same cluster: reference [13] was attributed to "Payne, R." when PMC2494365 is by Jackson, Batty, Ryan & McGregor. A sixth surfaced during validation, the README's own claim that the base configuration produces 0.70 DOW/run having gone stale at 1.34 DOW/run through the Issue #23, #43 and #161 shifts.

Resolution route (b) was selected and the reasoning recorded in the README. Route (a) is unavailable, since no open-access source reports a campaign DOW count against an exact WIA denominator, and deriving one from an Ajax Bay numerator over a campaign denominator would recreate the population mismatch the issue identifies; route (c) discards the Falklands anchor for no evidentiary gain. The target is now the Ajax Bay treated-cohort rate of three deaths among the "over 650" combat casualties who reached the Advanced Surgical Centre (≈0.46%, Westphalen 2018), compared against the model's own cohort of casualties reaching an R2B or R2E facility. The 255:777 campaign totals were sourced to the parliamentary record as new references [53] and [54], noting that the 777 figure counts injuries to Service personnel and civilians, including 109 cold injuries, and so is broader than a strict WIA count.

Neither ceiling changed. Both shipped configurations already span the corrected target at 50 replications: base 0.633% (95% CI [0.392%, 0.874%]) and `moderate_intensity` 0.491% (95% CI [0.310%, 0.673%]). The issue's tasks for re-deriving the ceilings, re-running the seed-42 baseline and refreshing the analysis-document figures therefore did not apply, and this was stated explicitly rather than silently omitted. The alternative treated cohort of Jackson et al. (three of 233 operated on, 1.29%) was rejected as the target on measured grounds rather than asserted ones: raising the ceiling shrinks the operated cohort, because casualties die before reaching surgery, so reaching 1.29% requires ceilings near `p1_p_max` = 0.08, which drive whole-of-WIA DOW to roughly 3.6% and 5.4 deaths per run. That figure is retained as corroborating evidence instead. Limitation L22 was added covering what the corrected target still cannot constrain: an inexact "over 650" denominator making 0.46% an upper bound, a cohort mixing British and Argentine casualties against a single-force model, and a whole-of-WIA mortality rate left unconstrained by any historical figure.

**Seed-42 baseline (30 days, single run):** unchanged. No model code, `env_data.json` parameter or RNG stream was touched, and the seed-42 run was reproduced exactly as a validation step before any measurement was taken. One multi-run row in `CLAUDE.md` was refreshed: the long-pending "Mean DOW/run (50-rep, seed=NULL)" figure moved from ~0.70 to 1.34 (95% CI [0.94, 1.74]), the accumulated effect of the earlier RNG-stream shifts finally being measured rather than an effect of this issue.

**Unblocked by this merge:** No new issues unblocked.

### Issue 154 — Tracked Seed-42 Baseline Could Drift Out of Sync ✓

**Merged:** PR #165, branch `claude/next-issue-8yp9wf`

`run_bch()` produced a different artifact set on each of its two paths, and the split followed no stated rule: `images/` was rewritten by any invocation at all, including a one-day smoke test, while `logs/logs.txt` and `data/arrivals_*.txt` were rewritten only by single runs. Since those three sets together constitute the project's seed-42 regression evidence, a run could refresh part of it and leave the remainder describing a different run, with nothing in the diff to signal it because PNG diffs are opaque. Approach (a) from the issue was adopted over (b), on the grounds that (b) prints a warning only after the damage is done. Every run now writes beneath the gitignored `outputs/` directory alone: CSVs and markdown tables to `outputs/`, plots to `outputs/images/`, the console log to `outputs/logs.txt`, and arrival diagnostics to `outputs/data/`. The tracked locations are reachable only through a new `--refresh-baseline` flag, which requires `--iterations 1` and errors otherwise, because the console log and the arrival diagnostics describe one run's event stream and have no multi-replication equivalent. A partial refresh is therefore not expressible, which is what makes the guarantee hold rather than merely making corruption unlikely. `analyse_run()` and `analyse_replications()` now default `images_dir` to `file.path(output_dir, "images")`, and `data_dir` was threaded through `run_once()`, `write_arrival_diagnostics()` and `generate_mass_casualty_events()`, all of which previously hardcoded `data/`. `replication_summary.csv` is emitted from both run modes rather than multi-run only, returning `NA` rather than `NaN` in its dispersion columns at n = 1, and the single-run `sink()` gained an `on.exit()` guard so a failed interactive run can no longer leave the RStudio console silently redirected to a file.

The drift the issue anticipated had already occurred. A `git log` audit per file found the tracked set traced to six commits spanning 2026-07-10 to 2026-07-16 and four model configurations: the bulk from Issue #76, plus `images/force_regeneration.png` from Issue #18, `images/role4_census.png` from Issue #23, `images/ame_backlog.png` and `images/ame_sortie_timeline.png` from Issue #109, and the two mass casualty files from Issue #9. The whole set also predated the Issue #18, #23 and #161 output shifts. It was regenerated from one `--refresh-baseline` run and committed together, so the three directories describe the same run for the first time. `images/mass_casualty_events.png` is deliberately excluded, being generated under a non-default `mass_casualty.event.rate_per_day` that a shipped-configuration run cannot reproduce.

A second commit refreshed `docs/Single_Run_Analysis.md`, which had been left self-contradictory: Issue #161's PR updated its R2E section but not the surrounding narrative, so the Conclusion asserted an ICU at 50 to 76% utilisation on the same page as an R2E section reporting 98.8 to 100.0%, while the casualty generation tables and Return to Duty breakdown still described the pre-Issue-18 model at 400 casualties. Regenerating the tracked plots made the mismatch visible rather than merely latent. All figures were re-derived from the same seed-42 run. The substantive correction is at R2B holding: the documented claim of 8 to 10 casualties queued per node described pre-Issue-39 behaviour, whereas the beds in fact run near-full (mean 8.4 of 10, all ten occupied on three days, nine or more on 19 of 30) with no queue deeper than one, because the capacity-aware routing policy diverts casualties upstream to R2E before transport, 109 times over the run. The structural shortfall is exported to R2E rather than absorbed, which inverts the reading of an absent R2B queue as headroom.

**Seed-42 baseline (30 days, single run):** Unchanged. This merge alters artifact destinations only, consumes no RNG draws and shifts no stream; the regenerated run reproduced every documented post-Issue-161 figure exactly (total casualties 387; priority split 203/64/53/67; DOW 2, `r2b`=2 / `ame_wait`=0; R2B routing 109/0/0; R2B OT bypass 67 off-shift / 6 busy / 73 total; post-op `icu`=9 / `hold`=94; `surgery_deferred`=24; Role 4 40 evacuated, 76 queued, peak 20.0, 29 sorties). No pending flag recorded in `CLAUDE.md`'s earlier provenance caveats is cleared by it; a provenance note recording the artifact refresh was added in the same PR.

**Unblocked by this merge:** No new issues unblocked.

### Issue 161 — R2E Surgery Seizes No Surgical Team ✓

**Merged:** PR #162, branch `claude/next-issue-v7kcop`

R2E surgery seized an operating theatre bed but no surgical team: `surg_team`/`surg_teams` were computed in `r2e_treat_wia()` and then never referenced, so R2E throughput was bounded only by its two theatre beds, the alternating day/night roster `build_env()` builds for R2E had no effect at any hour, and all three R2E surgical sections reported exactly 0% utilisation across a 30-day run. This is the same defect Issue #8 closed on 2026-06-13, reintroduced by a later refactor of the R2E trajectory: the calls are now absent rather than commented out, but the dead variables they used are still in place. Both R2E surgery branches (`r2e_ot_surgery` and the Phase 4 second-surgery branch) now seize a section as a block around the procedure, bed then team, released team then bed, matching `r2b_ot_check_path()` so the two echelons cannot deadlock against each other's ordering.

The implementation departs from the issue's recommended approach in one respect, flagged in the PR and accepted on merge. The issue proposed seizing the in-scope `surg_team`, but that variable is `select_subteam()` evaluated once at trajectory build time, so seizing it would have bound the whole echelon to one randomly chosen section of three, capping concurrency at one against two theatres and confining all R2E surgery to a single 12-hour shift. A new `select_r2e_surg_section()` instead picks a section per casualty: on-shift sections preferred, least loaded among them chosen, ties broken randomly, mirroring `select_r2e_team()` and the shortest-queue policy used for beds. Concurrency is therefore capped at two by day, when two sections are rostered on, and one by night. The singular `surg_team` is deleted rather than referenced; an audit for the same defect class found three further dead variables (`emergency_teams`, `evacuation_teams`, `icu_teams`), also removed.

A new regression check, `scripts/check_r2e_surgery_seizure.R`, guards against a third reintroduction. It fails both structurally, by reading the built trajectory back through simmer's verbose print (which recurses into branch forks and names the resource on each activity), and behaviourally, by running the model and failing on zero surgical utilisation, concurrency above one per section, or any seizure beginning while a section holds zero capacity. It was verified to fail on the pre-fix code on both halves before being relied on.

Documentation: Limitation L3 narrowed to team-block seizure granularity alone, since the R2E half it also described is now closed; the R2E Mermaid diagram's three surgery nodes now name both resources as R2B's do; the Schedules and Rosters and resource-model narrative rewritten to state that the rostered hours now bound throughput rather than describing it; `docs/Single_Run_Analysis.md` R2E and strategic-evacuation figures refreshed; `docs/BCH_Task_Role_Allocation.md`'s "current bug" row corrected.

**Seed-42 baseline (30 days, single run):** This is not an RNG-stream-neutral change, and unlike Issue #23 it shifts rows above R2E disposition as well, because Issue #18's force-regeneration loop couples arrival timing to casualty-event timing. Following the Issue #76 precedent, the pre-change configuration was run first as a control and reproduced every documented post-Issue-23 figure exactly, so the changes below are attributable to this merge rather than to environment drift. Produced in an unpinned R 4.3.3 sandbox (no Docker access in the development environment); `data/arrivals_*.txt`, `logs/logs.txt` and `images/*.png` were not regenerated, per the Issue #18 and #23 unpinned-sandbox precedent.

| Metric | Before | After |
|---|---|---|
| R2E surgical section utilisation | 0%, 0%, 0% | 19.9%, 35.4%, 19.1% of rostered time |
| R2E OT queue ≥1 | OT1 1.08% of run, OT2 0% | OT1 3.10%, OT2 0.08% (never deeper than 1) |
| R2E OT utilisation (24h room) | OT1 40.8%, OT2 16.9% | OT1 35.7%, OT2 15.0% |
| Total casualties | 386 | 387 |
| Priority split (P1/P2/P3) | 222 / 56 / 41 | 202 / 64 / 53 |
| DNBI sub-types (bf/disease/nbi) | 49 / 90 / 31 | 40 / 94 / 36 |
| Total RTD | 143 | 148 |
| R2B surgeries | 49 | 52 |
| R2E surgeries (first / second) | 108 / 64 | 103 / 55 |
| Surgeries deferred (ICU saturated) | 17 | 24 |
| R2B bypass (upstream R1 threshold) | 115 | 109 |
| Post-op pathway (icu / hold) | 4 / 104 | 9 / 94 |
| Strategic evacuation decisions | 133 | 116 |
| DOW echelon split | r2b 1, ame_wait 1 | r2b 2, ame_wait 0 |

Two findings surfaced during verification that this merge did not cause and did not address. R2E ICU reads roughly 99–100% utilisation with a queue for 94–96% of the run; the pre-change control run already read 99.7–100%, so this is the Issue #23 AME-wait effect finally being measured on rows that had carried a "not recomputed" flag since Issue #18, and it clears that flag. Separately, `scripts/check_markdown.R` corrupts em-dash TOC anchors when run outside a UTF-8 locale, rewriting `#domain-2-timetocare-...` to `#domain-2-—-timetocare-...`; this is the same class as Issue #153 and was reverted by hand rather than committed.

**Unblocked by this merge:** No new issues unblocked. Issue #155 (final canonical re-run and documentation refresh) remains `status: blocked` by its own terms, since it gates on all issues being closed; no other open issue carries `status: blocked` or lists Issue #161 as a dependency.

### Issue 115 — Add an In-App Getting Started Guide ✓

**Merged:** PR #143, branch `claude/issue-115-9k2v2x`

Adds `docs/Getting_Started.md`, a short, task-oriented onboarding document covering what the app does, the Configure → Run → Analyse workflow (including a fourth, explicitly optional "Sensitivity Screening" step added in a same-PR follow-up), which parameters are worth adjusting first, and a one-line-per-tab guide to reading each Analyse-tab graph — at roughly 70 lines, a small fraction of the README's length, aimed at a first-time non-developer user rather than duplicating the README's academic treatment. Surfaced inside the running app itself, not just linked as a GitHub file: `app.R` gained a new **Getting Started** tab, rendered via `shiny::includeMarkdown()` and made the first (default) tab so it acts as onboarding on load, plus a "See Getting Started for how to read each graph below" link at the top of the Analyse tab (`actionLink`/`updateNavbarPage`) for a user already mid-results who wants the same guidance. A same-PR follow-up, prompted by an owner review noting the guide's only mention of the Sensitivity Calibration tab was a single terse clause, added the fourth workflow subsection above and expanded the Analyse-tab bullet to actually explain what Morris μ*/σ and Sobol S1/ST mean and when a planner would bother running either. Issue #113 (README → System Reference/Single-Run/Multi-Run split) is still open, so the guide currently links to the single `README.md` as it exists today; it can be re-pointed once that split lands, per the issue's own note.

Verified live rather than by code review alone: no R runtime was available in-session, so a build of the pinned Dev Container was attempted first; the container image built, but `renv::restore()` inside it failed against CRAN over HTTPS (a container-networking/proxy-trust gap, not a package or lockfile defect), and a retry — after routing the container through the session's proxy — hung silently for several hours once the Docker daemon itself became unresponsive mid-build. Abandoned in favour of installing R 4.3.3 and the ~20 CRAN packages `app.R` actually imports (not the full 116-package `renv.lock`) directly in the host sandbox instead, which succeeded in minutes once one missing system header (`libuv1-dev`) was added. The real running app was then driven end-to-end with Playwright/headless Chromium: confirmed Getting Started renders as the default tab with every heading and a working relative README link, ran a real Quick Run to completion, confirmed the Analyse-tab cross-link is present and correctly switches the navbar back to Getting Started, and confirmed the follow-up's new sensitivity-screening content renders with no console errors.

**Seed-42 baseline (30 days, single run):** Unaffected — this PR touches only `app.R` UI/server wiring, a new `docs/Getting_Started.md`, and README prose; no `env_data.json` or trajectory-logic change. `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue lists Issue #115 as a dependency, and no issue in the repository currently carries `status: blocked`.

### Issue 116 — Repo Cleanup: Audit and Resolve Orphaned Objects and File/Folder Structure ✓

**Merged:** PR #141, branch `claude/issue-116-yx0ci0`

A full repository audit, across two passes, checked every tracked file for whether it was still sourced by an R script, read by the Shiny app, or linked from README/docs, recording an explicit keep/remove/relocate decision for each candidate rather than a report-only inventory. First pass removed 12 orphans: the pre-`R/`-refactor legacy monolith `Battlefield Casualty Handling.R` (zero references to `force_regeneration`/`mass_casualty`/`role4`/`ame_` — features from Issues #9/#18/#23 that only ever landed in the post-refactor `R/trajectories.R`/`R/environment.R`) and its two self-described "compatibility shim" wrappers (`data_import.R`, `single-run_analysis.R`); the standalone unreferenced `distribution_graphs.R` demo script; `Architecture.pptx` (unreferenced, superseded by the README's live Force Structure/Medevac SVG diagrams); three root-level `.md` tables and three `data/mon_*.csv` files that were stale duplicates of what `analyse_run()` now writes to the gitignored `outputs/` directory; and `README_inputs.md`, no longer written by anything since Issue #85 moved the environment-summary generation to write directly into README's `<!-- ENV SUMMARY -->` block (its dead, commented-out old code path was removed alongside it). `STYLE_GUIDE.md` was relocated to `docs/STYLE_GUIDE.md`. Removing the monolith also orphaned two direct package dependencies in `renv.lock` (`simmer.plot`, `truncnorm`); a `renv::snapshot()` run in the pinned Dev Container confirmed the full transitive closure (`DiagrammeR`, `readr`, and each of their own exclusive sub-dependencies) — 16 packages pruned in total, verified programmatically to add nothing and change no version of any package still in use. A second pass, prompted by an owner follow-up, removed `controller_legacy.R` (explicit owner sign-off, since CLAUDE.md had flagged it "retained for reference only") and a second-pass-found orphaned image (`images/r2eheavy_surgery_distribution.png`, the saved output of the already-removed `distribution_graphs.R`), and added `.vscode/` to `.gitignore`. `CLAUDE.md`'s stale Repository Structure table (which still listed the removed files and never mentioned the `R/` module split or several other directories) and README's Codebase Structure table were both rewritten to match the actual current layout.

**Seed-42 baseline (30 days, single run):** Unaffected — this PR is a pure file-inventory, documentation, and `renv.lock` change; no `env_data.json` or `R/*.R` trajectory/environment logic was touched. `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue lists Issue #116 as a dependency, and no issue in the repository currently carries `status: blocked`.

### Issue 114 — Revise In-App Help Text for Clarity and Remove Internal Issue-Number References ✓

**Merged:** PR #139, branch `claude/issue-114-5ytjhc`

Audited every hover tooltip, panel description, and diagram legend in `app.R` and `R/app_params.R` in three passes. First, every `(Issue #N)`-style internal reference was removed from user-facing strings — including one baked into the Mass Casualty Event Timeline plot's rendered subtitle (`R/analysis.R`) — while leaving developer-facing code comments untouched. Second, a concision pass addressed a live-testing report that the Casualty Intensity Profile selector's tooltip (~1,000 characters) overflowed the viewport: `field()`'s (`R/app_params.R`) auto-appended `"Source: <citation>"` clause was removed from the interactive tooltip entirely — provenance is now stored as `$source` metadata on each registry field but surfaced only in the README's numbered references, not the hover pop-out — dropping the median tooltip across the 269-field Configure panel registry from 361 to 106 characters (longest from 678 to under 200). Third, every plot in the Analyse tab was audited for a purpose/interpretation description: 17 plots had only a bare heading or nothing at all (Casualty Flow, queue-depth plots, Bed & Resource Utilisation, R2B Treatment, Gantt charts, R2E Surgery, Waiting Times, AME Backlog/Sortie Timeline, Mass Casualty Event Timeline, Morris μ*/σ scatter); all 24 Analyse-tab plots now carry a description stating not just what the plot shows but why a planner would look at it and how to act on what they see (e.g. "a queue that persists or grows suggests that team is under-resourced").

**Seed-42 baseline (30 days, single run):** Unaffected — this PR is a pure string/UI change; no simulation logic, `env_data.json`, or trajectory code was touched. Verified after every commit in an unpinned R 4.3.3 sandbox (Docker unavailable in-session): total casualties and every other documented baseline KPI reproduced exactly. `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue carries `status: blocked`.

### Issue 112 — Verify Sensitivity Screening Covers All Relevant Model Parameters ✓

**Merged:** PR #135, branch `claude/issue-112-3jvy9y`; follow-up correction PR #137, same branch

A full audit of `env_data.json`'s numeric parameter surface against the Morris screening set (`R/sensitivity.R`) — combining `R/app_params.R`'s 269-field Shiny parameter registry with a direct read of the DOW logistic/treatment-efficacy constants (not app-editable) — found 44 gaps against the 11 parameters screened since Issue #3/#75. All 44 were added (55 parameters total), spanning R1/R2B/R2E durations and probabilities, the DOW logistic curve and treatment-efficacy factors, casualty generation rates, and the force regeneration (Issue #18) and strategic AME (Issue #23) subsystems explicitly named in the issue as motivating examples; the remainder (triangular min/max bounds, simplex-constrained composition splits, discrete/categorical switches, fixed establishment counts, mass-casualty schedule slots, and secondary rate-shape parameters) were excluded with a documented, category-specific rationale in a new README "Parameters Excluded from Screening" subsection. The re-run (r=5, down from the project's prior r=20 convention, given compute cost scaling with the 5× parameter-count increase — disclosed as new README Limitation L18) took three attempts: the first two surfaced two genuine, previously-unexercised latent bugs — an out-of-envelope `rtriangle()` bound (`fr_fill_mode_frac`'s screened upper bound exceeded its fixed `fill_max_frac` companion, producing `NA` draws that cascaded through Morris's OAT trajectories) and a missing `n_days` guard on the reinforcement demand scheduler (`R/replication.R`, a `seq()` "wrong sign in 'by' argument" once a screened `demand_interval_days` exceeded a short test run) — both root-caused and fixed inline rather than deferred. The third attempt completed cleanly: valid µ*/σ for all 55 parameters, zero failed design points, 94 minutes on 4 cores. The 44 new parameters were wired into `R/app_params.R`'s Configure-panel registry (`morris_name=`) so the Shiny app's sliders automatically pick up the new screened bounds, and `app.R`'s `MORRIS_LABELS` was extended with plain-English labels for all 44.

Three same-PR follow-ups, made at owner request after reviewing the initial re-run's plots, extended scope: the Morris scatter plots' variable-name labels (previously placed by base R `plot.morris()`'s zero-collision-avoidance `text()`) were rebuilt with `ggrepel::geom_text_repel()` for both the CLI (`R/sensitivity.R::plot_morris_scatter()`) and Shiny (`app.R`'s Sensitivity Calibration panel) paths; the plot points and the Shiny calibration table were colour-coded by a new `category` column (`morris_params`) into a colourblind-safe (Okabe-Ito) three-way split — "Scenario / Casualty Context" (35 parameters) vs. "Health System Design — Capacity" (10) vs. "Health System Design — Policy" (10) — replacing an initial two-way Context/Health-System-Design split found too coarse once the owner noted some "health system design" parameters (e.g. treatment-mode probabilities) are far less operationally adjustable than others (e.g. bed counts); and, after an owner review flagged two probable miscategorisations, `r1_transport`/`r2b_transport` (evacuation transport time) were moved from Capacity to Context, since transport time is a scenario/terrain assumption rather than a health-system design lever. A final follow-up added a `Variable`-to-plain-English lookup table (`MORRIS_LABELS`/`morris_params$category`, 55 rows) to both the Shiny Sensitivity Calibration table and a new README "Parameter Name Reference" subsection, cross-referencing the short variable codes used in `outputs/morris_ranking.csv` and the plot axis labels against their full titles and category.

**Seed-42 baseline (30 days, single run):** Unaffected — this PR does not touch `env_data.json` or `Battlefield Casualty Handling.R`/`R/trajectories.R`; it only extends the Morris screening configuration, analysis/plotting code, and Shiny UI. The `R/replication.R` reinforcement-scheduling fix only changes behaviour when `demand_interval_days > n_days`, which the shipped baseline (`demand_interval_days = 0`, disabled) never triggers. `CLAUDE.md`'s Key Parameters table does not require updating.

**Follow-up correction (PR #137):** A subsequent user review of the three-way category split found two of the 44 newly-screened parameters were not genuine screening candidates at all — `icu_defer_check_interval` and `ame_dow_check_interval` are `timeout()`+`rollback()` polling-loop intervals (`R/trajectories.R`) approximating continuous monitoring (ICU-bed availability, DOW risk while queued for strategic evacuation) at a fixed simulation-clock interval, not a real standing order a health system could issue — both were removed from screening entirely, settling the screen at 53 parameters. The same review found `post_surgery_prob` miscategorised as Policy: it decides, for an already-operated casualty, whether they need only a short vs. full R2E ICU stay — a clinical-severity fact (Context), inconsistent with its sibling `in_theatre_rate` (a genuine disposition/triage decision) which correctly stayed Policy despite living under the same `recovery.*` env_data block. `fr_fulfillment_lag_days`/`fr_fill_mode_frac` (reinforcement pipeline responsiveness) were reviewed and kept as Policy, flagged as a closer call than the other assignments. Category counts moved to 36 Context / 10 Capacity / 7 Policy. Because `morris()`'s pseudorandom OAT trajectory generation depends on the factor count, dropping/reclassifying parameters changes the sampling for every *remaining* parameter too (the same effect already documented for the Issue #74 removal) — the screen was re-run in full (r=5, 270 design points × 5 reps = 1,350 simulation runs, ~108 minutes, zero NA/failed KPIs) rather than patched, and every downstream artifact (README ranking table, all seven `images/morris_*.png` plots) was regenerated from that re-run. Several parameters moved by dozens of rank positions between the two designs (e.g. `wia_spt_mean` 1st→8th, `pri1_evac_prob` 29th→1st), explicitly called out in the README as evidence the two tables are different designs, not a small perturbation of one.

**Seed-42 baseline (PR #137):** Unaffected — same rationale as above; this correction PR touches only `R/sensitivity.R`, `R/app_params.R`, `app.R`, and documentation.

**Unblocked by this merge:** No new issues unblocked — no open issue lists Issue #112 as a dependency, and no issue in the repository currently carries `status: blocked` (re-confirmed after PR #137).

### Issue 117 — Audit and Complete Shiny App Analysis Tab Coverage of Simulation Outcomes ✓

**Merged:** PR #133, branch `claude/issue-117-qvny10`

A systematic audit cross-checked every element `analyse_run()` (Quick Run) and `analyse_replications()` (Full Analysis) return in `R/analysis.R`, every metric described in the README's Simulation Analysis section, and every output actually wired in `app.R`'s Analyse tab, rather than continuing to find gaps one at a time as Issues #109–#111 and #128 had. Stage 1 closed 15+ Quick Run outputs with zero UI wiring — most already written to `images/`/`outputs/` for the CLI path but never surfaced: R2B hold bed occupancy and OT bypass reason (Issues #39/#40), R2E post-operative pathway and OT utilisation, the transport capacity margin plot and per-platform utilisation (Issue #6), time-to-treatment/dwell/transit KPIs, DOW/RTD-by-echelon breakdowns, Role 4 census and unconstrained AME sortie demand, actual AME wait time by route, and the mass casualty event stress-test reconstruction (Issue #9) — via a new Transport tab, a new Return to Duty & DOW tab, a new Mass Casualty Events tab, and extensions to the existing Bed & Resource Utilisation and Strategic AME tabs. A same-PR follow-up then extended `analyse_replications()` itself — two new shared helpers, `build_attributes_wide()` (the per-casualty attribute pivot, already replication-safe) and `ci_by_group()` (generalising the t-distribution CI construction already used ad hoc elsewhere in the pipeline) — to compute a mean ± 95% CI equivalent of every one of those outputs for Full Analysis mode, rather than leaving it with an explanatory placeholder. The five single-run Gantt/split-panel plots (R2B Treatment, R2B Gantt, R2E Surgery, R2E Gantt, R2E OT-ICU Gating) remain the sole Quick-Run-only exception, predating this issue.

Verified by actually executing the pipeline, not by static review alone: R 4.3.3 and roughly 30 required packages (`simmer`, `shiny`, `bslib`, `sensitivity`, etc.) were installed ad hoc in-session (no Docker daemon available for the pinned Dev Container). `analyse_run()`/`analyse_replications()` were run directly against a real simulation, producing correct, internally-consistent output across every new field in both modes. `shiny::testServer()` then drove the real `app.R` server function through a live Quick Run — reproducing production's actual package-attach order (unlike the standalone execution above) and catching a genuine pre-existing bug predating this issue, from Issue #23: `assign_role4_los()`'s unqualified `select()` resolved to `simmer::select()` rather than `dplyr::select()` under that ordering, silently breaking every Role 4 census computation whenever `simmer` attaches after `dplyr` (e.g. a multicore-forked Shiny worker inheriting `app.R`'s top-of-file `library(dplyr)`); fixed by qualifying it, matching every other `select()` call in the file. A second, smaller pre-existing bug was also fixed: Full Analysis mode's Bed & Resource Utilisation CSV download referenced `ot_utilisation`, a Quick-Run-only field, silently downloading an empty file — `analyse_replications()` now also returns `utilisation_summary` and the download branches on run mode.

**Seed-42 baseline (30 days, single run):** Unaffected — this PR adds no new RNG draws and does not touch `Battlefield Casualty Handling.R`, `R/trajectories.R`, or `env_data.json`; it only adds analysis/aggregation functions and Shiny rendering code. `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue lists Issue #117 as a dependency.

### Issue 128 — Graph of Hold Beds Used in Absence of ICU ✓

**Merged:** PR #131, branch `claude/issue-128-format-s0a9m5`

`R/analysis.R::analyse_run()` has computed `r2e_icu_gating_plot`/`r2e_icu_gating_daily` since Issue #43 — a daily stacked bar chart of R2E post-operative casualties split into Normal (ICU access), Sub-Optimal (Priority 1 recovered in a Hold bed because ICU was saturated at OT entry), and Delayed (Priority 2+ OT entry deferred pending ICU availability) — but `app.R` never rendered it; the app's only related output was a single scalar "R2E ICU Peak Queue" KPI card. Added a fifth, Quick-Run-only panel ("R2E OT-ICU Gating — Hold Bed Used in Lieu of ICU") to the Analyse tab's Bed & Resource Utilisation nav panel, immediately below the existing R2E Bed Resource Usage (Gantt) panel, following the exact conventions already established for every other panel in that tab: `shrink_to_fit_plot_ui()`/`new_shrink_to_fit_plot()` (Issue #121, shrink-to-fit sizing + Expand-to-full-size modal), a fixed 500px natural height consistent with the tab's other single-panel bar charts, and PNG/PDF/CSV download buttons wired the same way as the neighbouring R2E Gantt panel. No new computation was added — this is purely a UI wiring fix, gated to Quick Run only (matching the existing precedent for `r2b_treatment`/`r2b_gantt`/`r2e_surgery`/`r2e_gantt`, since `analyse_replications()` has no multi-run equivalent for this single-run breakdown).

Verified live rather than by code review alone: R 4.3.3 and every package `app.R` and its sourced `R/*.R` files load were installed ad hoc in-session (no Docker daemon available for the pinned `rocker/rstudio:4.4.2` Dev Container), and `run.R`'s underlying pipeline was run directly at seed 42/30 days, confirming `r2e_icu_gating_plot`/`r2e_icu_gating_daily` are populated (104 Sub-Optimal, 4 Normal, 0 Delayed) exactly matching the `post_op_pathway_summary` (hold=104, icu=4) already documented in `CLAUDE.md`'s Key Parameters baseline. The app was then driven end-to-end with Playwright/headless Chromium: a live Quick Run (seed 42, 30 days) was executed against a running `shiny::runApp()` instance, and the new panel was confirmed to render the correct stacked bar chart, its "Expand to full size" modal was confirmed to show the same plot at full size, and its CSV download button was confirmed to produce `r2e_icu_gating_daily.csv` with the correct `day`/`care_category`/`n` columns.

**Seed-42 baseline (30 days, single run):** Unaffected — this is a Shiny rendering-only fix; no `env_data.json` or trajectory-logic change. `CLAUDE.md`'s Key Parameters table does not require updating.

**Unblocked by this merge:** No new issues unblocked — no open issue lists Issue #128 as a dependency.

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
- **`moderate_intensity`** (Falklands 1982 exemplar) — disentangles the pre-existing co-dependence between the Falklands-calibrated DOW ceiling and the OIF/OEF-era treatment efficacy factors the base configuration had been using. Era-appropriate treatment efficacy factors are paired with an independently re-calibrated ceiling, validated by 50-replication Monte Carlo against what was then recorded as a 0.52% DOW/WIA target (result: 0.480%, 95% CI [0.323%, 0.638%]). Issue #152 replaced that target with the Ajax Bay treated-cohort rate (see Issue 5 above) and re-validated both configurations against it without changing any ceiling.
- **`high_intensity`** (Okinawa exemplar, demonstration skeleton, not a fully validated second scenario) — required fetching and reading the actual FORECAS report (Blood, Zouris & Rotblatt, 1998) directly rather than assuming its methodology, which surfaced two corrections: (1) FORECAS fits an **exponential** distribution (not lognormal) to combat-troop WIA/KIA at high intensity, parameterised by its mean alone (`W ~ exponential(mean)`) — added `generate_exp_arrivals()` and a `generate_casualty_arrivals()` dispatcher keyed off a new `distribution` field per generator; (2) the paper's actual Appendix A tables gave different values than initially assumed (Table A.7 Okinawa combat WIA is `Expon(6.86)`, Table A.9 combat KIA is `Expon(1.63)`), and there is **no standalone Vietnam combat-troop WIA/KIA table** in the source document (Table A.5 is Vietnam DNBI only) — a `vietnam` profile was dropped rather than citing a table that doesn't contain those numbers. **This directly affects Issue #10's own body, which currently cites Vietnam FORECAS Table A.5 and specific WIA/KIA numbers that do not appear in the source document — see the note added to Issue #10.**

A secondary defect was found and fixed during this same PR: the per-minute rate cap (`cap = 5`, an undocumented engineering constant with no citation, present since the casualty generator's first commit) truncated a wildly uneven, mean-dependent share of draws once applied to the new exponential streams — ~48% for `high_intensity` WIA (mean 6.86, *above* the fixed cap) vs. ~1–7% for the existing lognormal streams. `generate_exp_arrivals()` now computes `cap = cap_multiplier × mean_daily` (default multiplier 3), which truncates a constant ~5% of draws regardless of intensity (since `P(Exponential(mean) > k·mean) = exp(-k)` is mean-invariant). `generate_ln_arrivals()` (used by `default`/`moderate_intensity`) is untouched.

`controller.R` gained a scenario selector for previewing effective parameters per scenario; saving is gated to `default` so the override mechanism can't be accidentally flattened into the base file through the generic editor.

**Seed-42 baseline (30 days, single run):** Unchanged — 400 total casualties, 154 WIA, 70 KIA, 176 DNBI, byte-for-byte identical to the documented baseline. `default` scenario output confirmed structurally identical to the pre-existing `load_elms()` path. No RNG-stream-affecting change to the base configuration; CLAUDE.md's Key Parameters table does not require updating.

**high_intensity (50-rep, 30 days):** mean WIA/run 732.9, mean KIA/run 173.4 (WIA+KIA ratio 4.05× `moderate_intensity`), mean DOW/run 7.040 (95% CI [6.296, 7.784]), DOW/WIA rate 0.961%.

**Known limitations (documented in README L12):** the `moderate_intensity` KIA:WIA ratio (0.452) differs from the published 255:777 (0.328) record (Hansard, 18 Oct 1982 and 21 Dec 1982) — a pre-existing characteristic of the Issue #1 casualty generator calibration, not introduced or corrected here. `high_intensity` is an explicitly unvalidated skeleton (only casualty generation rate and distribution family are sourced). No Vietnam-calibrated profile exists — none was fabricated in place of a genuine source.

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

**Calibration:** P1 p_max = 0.023, k = 0.04, t_mid = 120 min; P2 p_max = 0.019, k = 0.025, t_mid = 180 min. Calibrated against a Falklands 1982 historical rate originally recorded as 3 DOW / 580 WIA ≈ 0.52% and attributed to Payne 1983. Issue #152 found that neither the 580 denominator nor the author attribution was supported by the cited sources, and replaced the target with the Ajax Bay treated-cohort rate of 3 deaths among over 650 casualties reaching forward surgery (≈0.46%, Westphalen 2018); the ceilings below were unchanged by that correction. The 50-replication validation run made at the time produced mean 0.70 DOW/run (0.45% of WIA), 95% CI [0.41, 0.95], against the then-stated 0.52% target; re-measured under Issue #152 with the same ceilings, the figure is 1.34 DOW/run, 95% CI [0.94, 1.74], and the treated-cohort rate that now carries the target is 0.633%, 95% CI [0.392%, 0.874%]. The movement is the accumulated effect of the Issue #73 follow-up, #76, #18, #23 and #161 RNG-stream shifts. Seed-42 single run at the time of this issue: 4 DOW. The values `p_max` and the OIF/OEF-era efficacy factors are entangled; substituting Falklands-era efficacy values (Issue #54) requires re-calibrating `p_max` upward.

**DOW model design finding (Test 2):** DOW checks fire only at care-transition boundaries, not during intra-echelon queue waits. Zeroing R2E OT capacity reduces DOW to 0 in the seed-42 run while OT queue peaks at 62 — confirming the model is sensitive to evacuation delays but not to intra-echelon surgical queue delays. Documented as a known limitation in the README.

**README additions:** DOW Survival Function section updated with logistic parameter table, cumulative ceiling calculation (0.023 × 0.83 × 0.56 × 0.32 × 0.25 = 0.085%), MODEL ASSUMPTION block with Falklands calibration basis and p_max/efficacy co-dependence note, embedded survival function figure (`images/dow_survival_function.png`), and References [42] (Jackson et al. 1983, PMC) and [43] (Westphalen 2018, JMVH). Both were recorded here under incorrect author attributions (Payne and Jolly respectively) until Issue #152 corrected them.

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

**Significance:** The decomposition preserves the operational distinction between forward behavioural health management (R1 battle fatigue hold) and clinical treatment throughput at each Role 2 echelon. The combined total previously obscured both. The 37.0% aggregate RTD rate is within the historical in-theatre range of 7.6–42.1% [[9]](../README.md#references).

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

1. ~~**Issue 19** — Dev Container specification. All contributors now develop in a reproducible Linux R environment with `mclapply` running at full core count.~~ — **Merged PR #21.**

### Phase 1 — Statistical Foundation (Issues 1 ✓, 22 ✓, 2 ✓, 3 ✓, 24 ✓, 75 ✓, 157 ✓, 158 ✓, 189 ✓, 186 ✓, 195 ✓, 208 ✓)
*Estimated effort: 3–4 weeks. All subsequent analyses depend on this foundation. **Complete.***

1. ~~Multi-replication wrapper (`mclapply` + `wrap()`) — **Merged PR #16**~~
2. ~~**Issue 22** — Define Output Variable Register; add five missing timing attributes to trajectories. **Merged PR #26**~~
3. ~~**Issue 24** — Switch to L'Ecuyer-CMRG RNG streams, add antithetic variates, set explicit `mc.cores`. **Merged PR #32**~~
4. ~~**Issue 2** — Welch warm-up analysis; set `warm_up_period` constant. **Merged PR #20**~~
5. ~~**Issue 3** — Morris Elementary Effects screening using the OVR KPIs from Issue 22. **Merged PR #30**~~
6. ~~**Issue 75** — Re-derive stale `p1_p_max` Morris screening bounds (predated Issue #5's recalibration) and re-run the full Morris screen. **Merged PR #79**~~
6a. ~~**Issue 157** — Replace the seven availability-selected Morris response variables with the seventeen criteria-selected Model Outputs KPIs, reduced to thirty-six scalar responses; add a `morris_kpis` registry carrying each response's C1–C5 criteria mapping and scalar reduction, emit a ranking CSV per response, and flag degenerate responses rather than reporting them as zero influence. Requires Issue 22 (the KPI definitions) and Issue 3 (the screen itself). **Merged PR #184**~~
6b. ~~**Issue 158** — Screen the nine simplex-constrained composition parameters (the R1 triage priority split, the DNBI sub-type composition and the mass casualty priority split) through the isometric log-ratio transform, which maps each three-part composition onto two unconstrained balance coordinates and back, so the sum-to-one constraint holds by construction rather than by a renormalisation that is itself a design decision. The nine become six coordinates and the screened set moves from fifty-eight parameters to sixty-four. Added `scripts/check_composition_ilr.R`; the production ranking was deferred to the canonical re-run. Requires Issue 3. **Merged PR #187**~~
6c. ~~**Issue 189** — Withdraw the antithetic pairing of replications. Pairing (2k-1, 2k) on a shared seed made the pair rather than the replication the unit the design supplied, while every interval in the project divided by the replication count; point estimates were unaffected and widths were not. Each replication now draws its own seed, which makes the same `qt(0.975, df = n - 1) * sd / sqrt(n)` arithmetic correctly specified at roughly thirty call sites without touching any of them. Added `scripts/check_replication_independence.R`. Requires Issue 24. **Merged PR #192**~~
6d. ~~**Issue 186** — Correct three `morris_params` baselines that did not describe the model as shipped, two of them outside the bounds on their own row. Morris never reads that column, but `run_sobol()` holds every unselected parameter fixed at it and the Shiny Calibration tab displays it to a planner. Added a source-time bounds assertion and `scripts/check_morris_baseline.R`, which asserts the stronger property that applying the whole baseline vector leaves `env_data.json` unchanged. Raised Issue #195 for the `surg_mode` bounds it deliberately left alone. Requires Issue 158. **Merged PR #194**~~
6e. ~~**Issue 195** — Re-derive `surg_mode`'s screening bounds from the shipped 95-minute mode, the 90 to 150 range having been set against the 120-minute mode Issue #76 superseded and leaving the baseline 0.083 of the way along its own range. Now 57 to 133, Rule A, baseline at 0.500; the lower endpoint checked against the source's own reported operative-time range and both endpoints against the fixed triangular envelope. Audit of the remaining sixty-three rows found none in the same state. Requires Issue 186 (which corrected the same row's baseline and deferred this half). **Merged PR #197**~~
6f. ~~**Issue 208** — Snapshot the caller's generator kind and stream position in `run_replications()` and restore them on exit. The function had drawn its per-replication seeds before setting `RNGkind("L'Ecuyer-CMRG")`, and the kind persists for the session, so the first call in a session drew its seeds under a different generator than every later one; the seeds were good either way, so no point estimate was biased, but a measurement could not be reproduced from the control seed it is stated at. Added `scripts/check_measurement_reproducibility.R`. Requires Issue 189. **Merged PR #211**~~

### Phase 2 — Model Fidelity (Issues 8 ✓, 35 ✓, 37 ✓, 44 ✓, 6 ✓, 5 ✓, 43 ✓, 14 ✓, 73 ✓, 74 ✓, 85 ✓, 76 ✓, 161 ✓, 156 ✓, 159 ✓, 173 ✓, 180 ✓, 178 ✓, 146 ✓)
*Estimated effort: 2–3 weeks. Low-to-medium code changes, high impact on result validity.*

6. ~~**Issue 8** — Fix R2E surgical team seizure (three lines; do first). **Merged.**~~
7. ~~**Issue 35** — Fix R2B OT bypass check (`<=` → `< && queue == 0`). **Merged PR #36.**~~
8. ~~**Issue 37** — Remove 12h schedule from OT bed resources; add team-availability bypass check. **Merged PR #38.**~~
9. ~~**Issue 44** — RTD KPI annotation: decomposed `total_rtd` into `bf_rtd` + `clinical_rtd`, added `rtd_type` column to `rtd_by_echelon`, two `stopifnot()` guards, seed-42 baseline documented. **Merged PR #47.**~~
10. ~~**Issue 6** — Dead-heading return legs for transport assets.~~ — **Merged PR #56.**
11. ~~**Issue 5** — Time-dependent DOW survival function.~~ — **Merged PR #53.**
12. ~~**Issue 43** — OT–ICU gating: implement three-way pre-OT branch (ICU available / ICU full + P1 / ICU full + P2+).~~ — **Merged PR #59.**
13. ~~**Issue 14** — Shiny app parameter editor and Quick Run mode. Requires `R/analysis.R` refactor returning ggplot objects (Issue 1 dependency already satisfied).~~ — **Merged PR #71.**
13a. ~~**Issue 73** — Wire in the R2B to R2E WIA dead-heading return leg that `env_data.json` configured but no trajectory called, on each R2B team's own organic evac resource rather than the shared fleet, and add the R2B to R2E road-move mortuary transport for KIA and DOW (`r2b_transport_kia()`, `r2e_mortuary_intake()`) that the model had been performing without a vehicle.~~ — **Merged PR #81.**
13b. ~~**Issue 74** — Remove `return_leg_multiplier` outright, a dead-heading vehicle's rate of march not being doctrinally differentiated by payload, so no scenario the parameter could represent departs from 1.0. Proceeded on owner direction despite Issue #73's re-run having just found it the most influential of the ten screened parameters on transport utilisation, the ground for removal being operational rather than sensitivity-based. Requires Issue 73.~~ — **Merged PR #83.**
13c. ~~**Issue 85** — Harden `scripts/check_env_data_summary.R` against R1's integer `sub_elm`, which crashed `map_chr()` under `purrr` releases treating a mixed-type vector as an error rather than a deprecation, and correct the defect the crash had been masking, R1's medical-resources column rendering a bare `"1"` header in place of `"Base"`.~~ — **Merged PR #87.**
13d. ~~**Issue 76** — Reconcile the shipped DAMCON surgery duration to the 41/210/95-minute triangular distribution the README narrative already cited (Sohn et al. 2018, reported within Zizzo et al. 2020), replacing the unsourced 90/240/120 placeholder carried since the initial commit. RNG-stream-shifting, and the first seed-42 baseline refresh made in the pinned Dev Container.~~ — **Merged PR #89.**
14. ~~**Issue 161** — Seize an R2E surgical section for every R2E procedure (regression of Issue 8; per-casualty section selection, plus a structural/behavioural regression check).~~ — **Merged PR #162.**
15. ~~**Issue 156** — Replace the unconditioned R2E Phase 5 disposition draw with a theatre evacuation policy compared against a severity-scaled recovery-to-duty duration; move casualties awaiting strategic AME off ICU beds onto holding beds, apart from a bounded pre-flight period for a ventilated minority.~~ — **Merged PR #169.**
15a. ~~**Issue 159** — Conserve the post-operative intensive care requirement across treatment location, drawing it once and dividing it between the echelons by a new `r2b.post_op_icu.share` rather than leaving R2B to deliver none of it while R2E discounted an episode that never occurred. Model separately the two episodes damage control requires, stabilisation between the abbreviated operation and the definitive repair and post-definitive care after it, the second of which no casualty on any route had been receiving. Added `scripts/check_icu_time_conservation.R`.~~ — **Merged PR #174.**
15b. ~~**Issue 173** — Split the surgical population between the staged damage control pathway and a single-stage definitive procedure, keyed to triage priority by a `pri*_dcs_rate` family; a single-stage casualty takes one theatre episode and one intensive care episode instead of two of each. Requires Issue 159.~~ — **Merged PR #176.**
15c. ~~**Issue 180** — `scripts/check_r2e_surgery_seizure.R` located its surgery blocks by a literal renamed in PR #176, so the structural half skipped every assertion while reporting a model defect: hold the section trajectory name once as `R2E_SURGERY_SECTION_FMT` in `R/trajectories.R` and derive the check's pattern from it, separate check-integrity failures from model failures, and assert that the matched blocks cover every configured surgical section. Requires Issue 161.~~ — **Merged PR #182.**
15d. ~~**Issue 178** — Re-measure the treated-cohort died-of-wounds rate against the Ajax Bay bound and decide whether the mortality ceilings need re-fitting. Closed with `p1_p_max` and `p2_p_max` unchanged in both shipped configurations: the reported 0.617% overshoot is one draw from a seed-to-seed spread of 0.151 percentage points and does not reproduce, the pooled 150-replication rate of 0.416% (95% CI [0.346%, 0.485%]) spanning the ~0.46% bound. Added `scripts/check_dow_calibration.R`. Requires Issue 159 and Issue 173.~~ — **Merged PR #190.**
15e. ~~**Issue 146** — Pre-open hold window at R2B: a casualty who finds the theatre free and the surgical section closed for no longer than `r2b.surgery.pre_open_window_min` is held forward for the section rather than diverted to R2E, the seizes doing the waiting. Shipped at 60 minutes, recorded as README Further Development L28. Added `scripts/check_pre_open_window.R`. The R2E parity parameter the issue also asks for was not added, R2E having no surgical bypass for a window to modify.~~ — **Merged PR #204.**

### Phase 3 — Structural Refactoring (Issues 7 ✓, 39 ✓, 60 ✓, 150 ✓, 4 backlog, 40 partial ✓ / backlog)
*Estimated effort: 4–5 weeks. Requires `env_data.json` schema changes, trajectory rewrites, and hold-bed decomposition.*

10. ~~**Issue 7** — DNBI sub-category routing~~ — **Merged PR #34.** Prerequisite for Issue #39 satisfied.
11. ~~**Issue 39** — R2B hold bed saturation analysis~~ — **Merged PR #48.** Two-tier routing policy (upstream threshold + at-R2B three-stage branch) implemented; per-stream occupancy decomposition added to analysis pipeline.
12. ~~**Issue 60** — Guard zero-length `seq_len(qty)` in `build_environment()`'s bed and transport ID construction so `qty: 0` produces zero resources instead of one.~~ — **Merged PR #62.**
12a. ~~**Issue 150** — Re-measure the 100-replication DNBI sub-type surgical-requirement statistics against the current codebase~~ — **Merged PR #217.** The Issue #7 figures predated every RNG-stream-shifting merge since; refreshed to 183.5 casualties requiring surgery per replication (SD 36.0) at 81.1% NBI / 6.0% disease / 0.0% battle fatigue, with the qualitative conclusion unchanged and a provenance note added to `CLAUDE.md`.
13. **Issue 40** — R2B OT utilisation improvement. ~~Add `r2b_bypass_reason` attribute~~ — **Merged PR #64** (67 off-shift, 10 OT busy/queued, of 77 at-R2B bypasses). Remaining scope: scenario-test `ot_hours` at 12/14/16/20h; evaluate second surgical team option (partial result without Issue #4) — both deferred pending a clinician fatigue model (Scenario A) and a directed establishment-size decision (Scenario B). **Backlog** — not currently planned; unblocked but on hold pending that design work.
14. **Issue 4** — Individual resource seizure. Read `BCH_Task_Role_Allocation.md` in full before beginning. Gated until Issues 1, 2, and 3 are all stable (satisfied). Address the six validation assumptions in `BCH_Task_Role_Allocation.md` Part 5 — document each as a named model assumption in the README, and include the two highest-priority assumptions (NO flex to surgical roles; second-surgeon probability) in the Morris screening from Phase 1. **Backlog** — unblocked but deprioritised given its size/risk; may not be resourced in the near term.

### Phase 4 — Scenario Expansion (Issues 54 ✓, 9 ✓, 10 ✓, 57 ✓, 18 ✓, 23 ✓, 124 ✓, 160 ✓, 153 ✓, 148 ✓, 203 ✓, 206 ✓, 149 ✓, 151 ✓, 207 ✓)
*Estimated effort: 3–4 weeks. Builds on Phase 1–3 outputs. **Complete.***

11a. ~~**Issue 54** — Scenario-level parameter profiles (schema, `load_scenario()`, `moderate_intensity`/`high_intensity` — prerequisite for Issue 10).~~ — **Merged PR #67.**
12. ~~**Issue 9** — Compound Poisson mass casualty injection overlay. Requires Issues 1, 2, 5.~~ — **Merged PR #92.**
13. ~~**Issue 10** — Comparative scenario runner (`run_scenario()`/`compare_scenarios()`, `R/scenario_runner.R`; scope revised to compare `moderate_intensity`/`high_intensity` rather than the uncited Vietnam/Okinawa figures originally in the issue body).~~ — **Merged PR #69.**
13a. ~~**Issue 57** — Fleet-size capacity margin sweep for transport assets (`plot_transport_capacity_margin_by_fleet_size()`; Shiny Sensitivity Calibration panel integration added at owner request within the same PR). Requires Issue 10.~~ — **Merged PR #103.**
14. ~~**Issue 18** — Endogenous casualty generation (force regeneration feedback). Requires Issues 1, 2, 22.~~ — **Merged PR #105.**
15. ~~**Issue 23** — Role 4 occupancy and AME sortie demand. Requires Issues 1, 22, 18.~~ — **Merged PR #107.**
15a. ~~**Issue 124** — Stop force reinforcement crediting `effective_force_combat`/`effective_force_support` above initial establishment strength. `credit_fn()` now clamps the credited value to `min(initial, current + fill)`, and a per-pool pending counter nets fill already committed to an in-flight cycle out of the live shortfall, so overlapping demand cycles can no longer each claim the same shortfall. Requires Issue 18.~~ — **Merged PR #129.**
15b. ~~**Issue 160** — Strategic AME sortie capacity sourced to the RAAF C-17A aeromedical evacuation fit (36 critical / 54 standard), replacing the two unsourced aircraft configurations and their selection rule with a single named airframe; C-130J-30 and C-27J added as selectable alternatives. Requires Issue 23.~~ — **Merged PR #171.**
15c. ~~**Issue 153** — Derive each comparative scenario's axis label from its own ASCII identifier through `scenario_short_label()`, so `scripts/run_scenarios.R` no longer aborts at its plotting stage in any locale that is not UTF-8, and escape the plot title's em dash, which had been drawn as three raw bytes once the pattern was repaired. `scripts/check_markdown.R`'s anchor generation was corrected in the same PR to reproduce GitHub's algorithm, repairing 45 table of contents anchors, 15 body cross-references and 216 citation links, and gained the link check this document is now inside. Added `scripts/check_scenario_labels.R`. Requires Issue 10.~~ — **Merged PR #199.**
15d. ~~**Issue 148** — Replace `make_ln_arrival_generator()`'s fixed absolute per-minute rate cap of 5 with the mean-relative `cap_multiplier × mean_daily` the exponential generator has used since Issue #54, narrowing the cross-stream truncation spread from roughly 150-fold to under 6-fold. Raised Issue #203 for the residual, that a cap of any multiplier holds realised generation below the configured mean.~~ — **Merged PR #202.**
15e. ~~**Issue 203** — Remove the per-minute rate cap outright rather than correct for it, the closure that replaced the vectorised generator having no run-time failure mode for the cap to prevent; every stream now realises the daily mean its configuration names. Emission also corrected so a minute accruing several casualties emits all of them. Died-of-wounds ceilings re-fitted 0.023/0.019 to 0.020/0.016 for the corrected rates. Raised Issues #206, #207 and #208.~~ — **Merged PR #209.**
15f. ~~**Issue 206** — Replace the per-minute rate walk with direct arrival-time sampling: the rate is drawn once per simulated day, the timescale FORECAS fitted it at, and arrivals are placed within the day by thinning, so the stream realises the between-day variance `sd_daily` names instead of averaging it away across 1,440 draws. Combat WIA daily standard deviation moves 0.50 to 9.5 against a Poisson 2.10, with a busiest day of 564 against six. Moves casualty generation itself; both died-of-wounds ceilings re-verified and left unchanged. README Further Development L27 deleted. Requires Issue 203.~~ — **Merged PR #215.**
15g. ~~**Issue 149** — Split a fired mass casualty event's drawn casualty count between the wounded and the casualties killed at or near the point of injury, by one Binomial draw per event, the killed overlaid on the `kia_cbt` stream and taking the mortuary pathway the background killed stream already takes. New `mass_casualty.event.kia_fraction`, shipping at 0.28 as an informed estimate anchored on the model's own combat stream means. Disease and non-battle injury stay out of the mechanism on causal-link grounds. Added `scripts/check_mass_casualty_kia_split.R`; README Further Development L20 deleted. Requires Issue 9.~~ — **Merged PR #219.**
15h. ~~**Issue 151** — Give `high_intensity` its own died-of-wounds ceilings and treatment efficacy factors in place of the Falklands-calibrated pair it inherited, so the profile's mortality model belongs to the campaign its casualty rates come from. Calibrated against Okinawa's own reported rate of 3.4% among casualties reaching a hospital alive (Marble, 2025), reaching 3.471% (95% CI [3.360%, 3.583%]) at `p1_p_max` = 0.052 and `p2_p_max` = 0.042. The efficacy factors are informed estimates for 1945 Pacific-theatre care; the two within-era penalties stay inherited. `scripts/check_dow_calibration.R` now holds a target per configuration and tests this one two-sided. Requires Issues 54 and 10.~~ — **Merged PR #221.**
15i. ~~**Issue 207** — Apply two configured planner levers in full. The reinforcement credit no longer clamps at establishment strength: reinforcement joins the population on arrival, so a fill fraction above 1 delivers more than the shortfall it was requested against and carries the pool over strength until casualties bring it back down, which required `reinforcement_force_bound()` to widen the arrival generators' thinning bound so a force size above establishment cannot saturate the acceptance probability. A casualty evacuated from R2B holding under `evac_threshold` now serves the remainder of the convalescence already drawn rather than a fresh draw. Added `scripts/check_lever_realisation.R` and `validate_fill_distribution()`. Both features ship disabled, so no baseline value moves. Requires Issues 18 and 124.~~ — **Merged PR #224.**

### Phase 5 — Interface (Issues 72 ✓, 93 ✓, 15 ✓, 77 ✓, 110 ✓, 111 ✓, 121 ✓, 109 ✓, 128 ✓, 117 ✓, 112 ✓, 114 ✓, 116 ✓, 115 ✓, 154 ✓, 152 ✓, 147 ✓, 201 ✓, 155 ✓)
*Estimated effort: 1–2 weeks.*

15a. ~~**Issue 72** — Adopt `renv` for reproducible R package dependency pinning: `renv.lock` captures the 116 packages `run.R`, `app.R`, `R/*.R` and `scripts/*.R` require, and the Dev Container restores from it rather than from the Dockerfile's ad hoc `install.packages()` list, with the image build pre-warming the cache so a container start resolves from it.~~ — **Merged PR #91.**
15b. ~~**Issue 93** — Repair the Dev Container build reported shortly after Issue #72 merged: install `libuv1t64`, which the prebuilt `fs` binary links at runtime and `rocker/rstudio:4.4.2` does not ship, add `curl`, and correct `renv.lock`'s recorded R version from the 4.3.3 of the Issue #72 sandbox to the Dockerfile's pinned 4.4.2, confirmed against a real 4.4.2 environment as zero package drift. Requires Issue 72.~~ — **Merged PR #94.**
16. ~~**Issue 15** — Shiny Full Analysis mode (multi-run CI, sensitivity panel). Requires Issues 14, 1, 2, 3.~~ — **Merged PR #97.**
17. ~~**Issue 77** — Defer Configure panel rendering (`suspendWhenHidden = TRUE`) to eliminate the eager-render race that could silently revert edits made within ~15–20s of load or a scenario switch. Also closes the duplicate Issue #98.~~ — **Merged PR #101.**
18. ~~**Issue 110** — Replace fixed y-axis limits on queue-depth plots (R1, R2B, R2E Heavy Bed) with dynamic `scale_y_continuous(limits = c(0, NA), expand = expansion(...))` scaling so peak queue depth is never clipped.~~ — **Merged PR #118.**
19. ~~**Issue 111** — Scale the Bed & Resource Utilisation Gantt chart's rendered height to its distinct resource-row count so R2B/R2E bed rows no longer overlap at a fixed container height.~~ — **Merged PR #120.**
20. ~~**Issue 121** — Shrink-to-fit convention for every Analyse-tab plot output, with an Expand-to-full-size modal; split Queue Depths and Quick Run Bed & Resource Utilisation out of their combined patchwork images into individually-sized panels. Requires Issue 111.~~ — **Merged PR #123.**
21. ~~**Issue 109** — Add AME sortie timeline and queue-depth (backlog) visualisation to the Shiny Analyse tab, reconstructed from the `"ame"`/`"ame_critical"` resource monitor and per-casualty event timestamps since Issue #23 introduced the underlying mechanism with no accompanying graph.~~ — **Merged PR #126.**
22. ~~**Issue 128** — Surface the existing `r2e_icu_gating_plot` (R2E hold-bed-in-lieu-of-ICU, Issue #43) in the Shiny Analyse tab's Bed & Resource Utilisation panel; the plot had been computed and saved to `images/r2e_icu_gating_impact.png` since Issue #43 but was never wired into `app.R`.~~ — **Merged PR #131.**
23. ~~**Issue 117** — Audit and complete Shiny app Analysis tab coverage of simulation outcomes: cross-check every `analyse_run()`/`analyse_replications()` output and README-documented metric against what `app.R` actually renders, in both Quick Run and Full Analysis modes, and close every gap found.~~ — **Merged PR #133.**
24. ~~**Issue 112** — Verify Morris sensitivity screening covers all relevant `env_data.json` parameters: audit the full parameter surface, add missing parameters with cited bounds, document exclusion rationale for the remainder, and re-run the screening.~~ — **Merged PR #135**; category-assignment correction and re-run at 53 parameters — **Merged PR #137.**
25. ~~**Issue 114** — Revise in-app help text for clarity and remove internal issue-number references: audit every `helpText()`/tooltip/panel description in `app.R`, remove GitHub issue-number references, and ensure every parameter control and graph has a concise, actionable description of purpose/interpretation.~~ — **Merged PR #139.**
26. ~~**Issue 116** — Repo cleanup: audit every tracked file for whether it is still sourced, read, or linked; record and execute an explicit keep/remove/relocate decision for each orphan found; sync CLAUDE.md/README structure tables to match.~~ — **Merged PR #141.**
27. ~~**Issue 115** — In-app Getting Started guide: author `docs/Getting_Started.md` (workflow, key parameters, graph-reading guidance, plus a same-PR follow-up covering sensitivity screening) and surface it inside `app.R` as a default-landing tab, cross-linked from the Analyse tab.~~ — **Merged PR #143.**
28. ~~**Issue 154** — Tracked seed-42 baseline drift: route every run's artifacts under the gitignored `outputs/` directory, gate the tracked `images/`, `logs/logs.txt` and `data/` set behind a `--refresh-baseline` flag requiring `--iterations 1`, guard the single-run `sink()` with `on.exit()`, and regenerate the (already drifted) tracked evidence set from one run.~~ — **Merged PR #165.**
29. ~~**Issue 152** — DOW calibration target unsupported by its cited sources: replace the untraceable "3 DOW / 580 WIA ≈ 0.52%" target with the Ajax Bay treated-cohort rate (≈0.46%, Westphalen 2018) measured against the model's matching cohort, correct the reference [13] author attribution and the 255:777 sourcing, and re-validate both shipped configurations against the corrected target.~~ — **Merged PR #167.**
30. ~~**Issue 147** — `ot_hours` absent from `env_data.json`, carried as a bare `= 12` default duplicated across six files and therefore missing from the Configure panel's parameter registry: add `vars.surgical_roster.shift.ot_hours` as the single source of truth, default every call site to `NULL` so it resolves from the configuration, register the field, and remove the Run tab's duplicate slider so the shift length is captured by the configuration a run is saved from.~~ — **Merged PR #179.**
30a. ~~**Issue 201** — Backfill the thirteen merged issues that had no item in any phase sequence list and no entry in their phase heading roster, five of them reported and eight found by the audit, each placed at its position in merge order under the letter-suffix convention. Widen `scripts/check_markdown.R`'s anchor link check from the three documents carrying a table of contents block to every tracked markdown document, excluding links inside backticks, which GitHub renders as literal text; the table of contents and return-link maintenance stays scoped to the three. Repair the Issue 44 citation, which resolved to another entry's References block. Requires Issue 153 (the anchor check itself).~~ — **Merged PR #213.**
30b. ~~**Issue 155** — The terminal canonical re-run. Rebuild every figure, table and plot across `README.md`, `docs/Single_Run_Analysis.md`, `docs/Multi_Run_Analysis.md` and `CLAUDE.md` from one code state, commit `ed3c426`, in the pinned Dev Container, and retire the twenty-one accumulated per-issue provenance caveats. The pinned run reproduces the tracked seed-42 baseline byte for byte, so the caveats retire as correct and no published seed-42 value moves. Rebuild the Morris ranking at sixty-five parameters and r = 20, which withdraws the r = 5 finding that `triage_p1_balance` ranks first. Fix `run_sobol()`, which could never return indices. Add three scripts that interrogate a completed decomposition from its cache at no simulation cost, and the durability tooling a multi-hour screen needs to survive a reclaimed filesystem. Track the sensitivity evidence set at `data/sensitivity/`. Requires every other issue by its own terms.~~ — **Merged PR #226.**

### Phase 6 — Code Quality and Verification (Issues 230, 231, 232, 233, 234, 236, 237, 235, 241)
*Estimated effort: 3–4 weeks. Establishes automated verification and applies a written code standard. The phase opens with the verification baseline, because a gate cannot be wired around checks of unknown status and a refactor cannot be shown behaviour-preserving against an unknown baseline.*

1. **Issue 230** — Execute the fifteen `scripts/check_*.R` regression checks and the seed-42 baseline reproduction in the pinned Dev Container, and record pass/fail, runtime and any failure output as a tracked table. A measurement rather than a repair: a check found failing becomes its own issue instead of being fixed in place, so the record stays an honest description of the state it was taken from. Blocks Issues 235, 241, 239 and 240. **In review, PR #243.**
2. **Issue 231** — Repair the README reference list: remove duplicated entries, correct misattributed authors, and replace the two sources that violate the open-access rule. Text only, so it depends on nothing in this phase.
3. **Issue 232** — Repair the ten broken README image links and extend `scripts/check_markdown.R` to validate link targets, so the same breakage cannot recur silently. Text and tooling only, and independent of the verification baseline.
4. **Issue 233** — Make `analyse_run()` stream-neutral, the analysis pipeline currently consuming random draws and so not being idempotent. Not a prerequisite for Issue 241, though landing it first removes a source of confusing byte-comparison failures there.
5. **Issue 234** — Rewrite `docs/STYLE_GUIDE.md` as an enforceable R code standard, including a commenting standard. Supplies the machine-checkable rules `.lintr` encodes in Issue 235 and the function length limit Issue 241 applies, so it gates both.
6. **Issue 236** — Make global configuration save and restore exception-safe, and add input validation to `R/analysis.R`.
7. **Issue 237** — Housekeeping: delete the ten `wip/*` branches, reconcile the Further Development scan table against its entries, and close the drift between `CLAUDE.md` and the repository it describes.
8. **Issue 235** — Add `lintr`, a glob-discovered `scripts/run_all_checks.R`, and a GitHub Actions workflow so the regression checks become a gate on every PR against `main`, with the slow checks scheduled or dispatched rather than run per PR. Add a PR template carrying the mandated test plan structure. Requires Issues 230 and 234.
9. **Issue 241** — Apply the code standard: decompose `server`, `analyse_run` and `analyse_replications` behind `testServer` and Playwright verification, verify the analysis pipeline byte for byte against the tracked seed-42 baseline, replace the ninety-three literal `1440`s with the named constant, and assess `r2e_treat_wia` for a provably stream-neutral decomposition. Requires Issues 234, 235 and 230.

### Phase 7 — Publication (Issues 238, 239, 240)
*Estimated effort: 2–3 weeks. Brings the two analysis documents to the standard of a published paper. Sequenced after Phase 6 because the figure-verification passes need a working environment and a known-good check suite.*

1. **Issue 238** — Re-cut the two analysis papers by method, moving the replicated experiments out of the single-run paper so each document reports one class of evidence. Settles what each of the other two issues is polishing, so it gates both.
2. **Issue 239** — Bring `docs/Single_Run_Analysis.md` to publication standard: remove the twenty-two issue-number references and three blockquoted maintainer notes, add a methods section and a limitations section cross-referencing the README's `L` entries, verify every numeric claim against the current baseline, and clear the presentation defects. Requires Issues 238 and 230.
3. **Issue 240** — Bring `docs/Multi_Run_Analysis.md` to publication standard: complete the statistical specification and remove the supersession narrative. Requires Issues 238 and 230.


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
  #128 R2E OT-ICU gating graph wired into Shiny app — new "R2E OT-ICU
       Gating" panel (app.R Bed & Resource Utilisation tab) renders the
       existing r2e_icu_gating_plot/r2e_icu_gating_daily (Issue #43),
       which had been computed and saved to
       images/r2e_icu_gating_impact.png since that issue but never
       wired into the app itself; UI-only, Quick Run only, no new
       computation; verified live via Playwright/headless Chromium
       against a running app instance (PR #131)
  #117 Analysis tab coverage audit — cross-checked every analyse_run()/
       analyse_replications() output and README-documented metric
       against app.R's actual rendering; closed 15+ Quick-Run-only gaps
       (R2B hold occupancy/bypass reason, R2E post-op pathway, transport
       capacity margin, dwell/transit KPIs, DOW/RTD by echelon, Role 4
       census, AME demand/wait time, mass casualty stress test) via a
       new Transport tab, Return to Duty & DOW tab, Mass Casualty
       Events tab, and Bed & Resource Utilisation/Strategic AME
       extensions; then extended analyse_replications() itself
       (build_attributes_wide()/ci_by_group() helpers) with a mean +/-
       95% CI equivalent of every one of those outputs for Full
       Analysis mode. Verified by direct pipeline execution plus
       shiny::testServer() driving the real app.R server through a live
       Quick Run, which caught and fixed a real pre-existing bug
       predating this issue (Issue #23's assign_role4_los() unqualified
       select(), masked by simmer::select() under production's package-
       attach order); also fixed a latent Full Analysis CSV-download
       bug (PR #133)
  #112 Sensitivity screening parameter-surface audit — cross-referenced
       R/app_params.R's 269-field Shiny registry plus the DOW logistic/
       treatment-efficacy constants against the 11 previously-screened
       morris_params rows; found and added 44 missing parameters (55
       total), spanning R1/R2B/R2E durations/probabilities, DOW logistic
       curve + treatment efficacy, casualty generation rates, force
       regeneration (#18), and strategic AME (#23); remainder excluded
       with documented category-specific rationale (README "Parameters
       Excluded from Screening"). Full Morris re-run (r=5, reduced from
       r=20 given the 5x parameter-count increase; README Limitation
       L18) surfaced and fixed two genuine latent bugs along the way:
       an out-of-envelope rtriangle() bound (fr_fill_mode_frac) and a
       missing n_days guard on the reinforcement demand scheduler
       (R/replication.R). Three owner-requested same-PR follow-ups:
       ggrepel::geom_text_repel() plot labels (fixing base R
       plot.morris()'s label overlap); a colourblind-safe three-way
       Context/Capacity/Policy category color split on both the CLI and
       Shiny scatter plots and the Shiny calibration table (owner-
       corrected mid-PR to move r1_transport/r2b_transport from
       Capacity to Context); and a Variable-to-plain-English lookup
       table added to both the Shiny app and a new README "Parameter
       Name Reference" subsection (PR #135). Follow-up category
       correction (PR #137): post_surgery_prob moved Policy -> Context
       (clinical-severity fact, not a threshold the health system
       sets); icu_defer_check_interval/ame_dow_check_interval removed
       from screening entirely (55 -> 53 parameters) as simulation-
       resolution polling intervals rather than genuine standing-order
       levers; fr_fulfillment_lag_days/fr_fill_mode_frac reviewed and
       kept Policy (flagged as a closer call). Full re-run at the
       corrected 53-parameter set (r=5, 1,350 sim runs, ~108 min, zero
       NA/failed KPIs) since morris()'s trajectory sampling depends on
       factor count; README ranking table and all seven per-KPI plots
       regenerated from real results, with the largest rank shifts
       between the two designs called out explicitly (PR #137)
  #114 In-app help text revision — removed every internal GitHub
       issue-number reference from app.R/R/app_params.R user-facing
       strings; field()'s auto-appended "Source: <citation>" tooltip
       clause dropped in favour of $source metadata (README remains
       the citation of record); median Configure-panel tooltip length
       cut from 361 to 106 characters after a live-testing report that
       the Casualty Intensity Profile selector's tooltip overflowed
       the viewport; all 24 Analyse-tab plots given a why/how-to-
       interpret description, closing 17 that previously had none
       (PR #139)
  #116 Repo cleanup — two-pass orphan audit removed 14 files (the
       pre-refactor legacy monolith and its two compatibility shims,
       Architecture.pptx, three stale root-level generated .md tables,
       three stale data/mon_*.csv duplicates, README_inputs.md,
       controller_legacy.R on explicit owner sign-off, and one
       second-pass-found orphaned image) plus 16 transitively-orphaned
       renv.lock packages (simmer.plot/truncnorm and their exclusive
       DiagrammeR/readr dependency subtree, verified via renv::snapshot()
       in the pinned Dev Container); relocated STYLE_GUIDE.md to docs/;
       CLAUDE.md's Repository Structure and README's Codebase Structure
       tables rewritten to match the resulting layout (PR #141)
  #115 In-app Getting Started guide — new docs/Getting_Started.md
       (workflow, key parameters, graph-reading guidance) rendered
       inside app.R as a default-landing tab (shiny::includeMarkdown())
       with a cross-link back to it from the Analyse tab; a same-PR
       follow-up added a fourth "Sensitivity Screening" workflow
       subsection and expanded the guide's Morris mu*/sigma and Sobol
       S1/ST explanation after an owner review found the original
       single-clause mention insufficient; verified live via
       Playwright/headless Chromium against R 4.3.3 installed directly
       in the host sandbox (a pinned-Dev-Container build got as far as
       renv::restore() before hitting a container-networking/proxy gap
       to CRAN, then hung for hours once the Docker daemon itself
       became unresponsive mid-retry — abandoned in favour of the host
       install) (PR #143)
  #161 R2E surgical section seizure — both R2E surgery branches now
       seize a section as a block around the procedure (bed then team,
       released team then bed, matching R2B), with per-casualty section
       selection via select_r2e_surg_section() so concurrency tracks the
       rostered establishment (two by day, one by night) rather than a
       build-time pick of one section in three; regression of the fix
       merged as Issue #8, so scripts/check_r2e_surgery_seizure.R was
       added to guard structurally and behaviourally against a third
       reintroduction (PR #162)
  #154 Tracked baseline drift — every run now writes only under the
       gitignored outputs/ tree; images/, logs/logs.txt and data/ are
       written solely under run.R --refresh-baseline, which requires
       --iterations 1 so a partial refresh cannot be expressed. sink()
       guarded with on.exit(); replication_summary.csv emitted from both
       run modes. The tracked set (found to span six commits and four
       model configurations) was regenerated from one run, and
       docs/Single_Run_Analysis.md refreshed to match (PR #165)
  #152 DOW calibration target replaced — the untraceable "3 DOW / 580
       WIA = 0.52%" gave way to the Ajax Bay treated-cohort rate (3
       deaths among over 650 reaching forward surgery, ~0.46%,
       Westphalen 2018) measured against the model's own reached-
       facility cohort. Reference [13] re-attributed to Jackson et al.;
       255:777 sourced to Hansard as [53]/[54]; Limitation L22 added.
       Both ceilings unchanged, both configurations already spanning
       the corrected target at 50 replications (PR #167)
  #156 R2E disposition rebuilt as a theatre evacuation policy — a
       severity-scaled recovery-to-duty duration compared against
       evacuation_policy_days replaces the unconditioned in_theatre_rate
       draw, and AME-awaiting casualties stage in holding beds rather
       than re-seizing ICU. Realised in-theatre share became an output
       (28.9%, 95% CI [27.7%, 30.1%]); ICU utilisation fell from ~100%
       to 61-93%; the Welch ICU-queue CMA fell from 30.6 to 1.18. The
       binding constraint moved to the second-shift surgical section
       (PR #169)
  #160 Strategic AME capacity sourced — the two unsourced aircraft
       configurations (2/8 and 0/20) and select_ame_configuration() gave
       way to one named airframe carrying its published fit, shipped as
       the C-17A Globemaster III at 36 critical / 54 standard, with the
       C-130J-30 and C-27J selectable via role4.ame.airframe. The day-30
       evacuation backlog fell from 58 queued to 16, Role 4 peak
       occupancy rose from 17 to 73, and L17 was rewritten: the R2E
       holding pool is bed-limited, not airlift-limited (PR #171)
  #159 Post-operative ICU time conservation, plus the two clinically
       distinct intensive care episodes damage control requires:
       stabilisation between the abbreviated and definitive operations,
       and post-definitive care after the final one, which the model had
       lacked entirely. Forward-holding policy levers added with it
       (PR #174)
  #173 Surgical population split between the staged damage control
       pathway and a single-stage definitive procedure, keyed to triage
       priority by a pri*_dcs_rate family. A single-stage casualty takes
       one theatre episode and one intensive care episode instead of two
       of each. R2E second surgeries fell 58 to 22, deferred surgeries
       22 to 12, and post-definitive care in an ICU bed rose from 34% to
       55%. The 50-replication treated-cohort DOW rate fell from 0.876%
       to 0.617% against the ~0.46% target. Setting every rate to 1.0
       reproduces the previous model bit-identically. L25 deleted
       (PR #176)
  #147 ot_hours moved into env_data.json (vars.surgical_roster.shift) as the
       single source of truth, replacing a bare = 12 default duplicated
       across six files; registered in the Configure panel and removed from
       the Run tab, so the shift length is captured by the configuration a
       run is saved from. Refactor — no baseline change (PR #179)
  #180 R2E surgical section trajectory name held once as
       R2E_SURGERY_SECTION_FMT and read by check_r2e_surgery_seizure.R, which
       had been matching a literal renamed in PR #176 and so skipping every
       structural assertion; check-integrity failures separated from model
       failures, section-coverage assertion added. Test harness — no
       baseline change (PR #182)
  #157 Morris response set replaced with the seventeen criteria-selected
       Model Outputs KPIs, reduced to 36 scalar responses (was 7 selected by
       what summarise_replications() exposed, of which one mapped cleanly to
       a documented KPI). morris_kpis registry carries each response's
       C1-C5 criteria and scalar reduction; extract_kpis() reads the
       arrivals/attributes monitors reusing R/analysis.R's derivations; a
       ranking CSV is written per response, with degenerate responses
       flagged rather than reported as zero influence. Screening plots
       moved off the tracked images/ directory. No model change — no
       baseline shift; production re-run deferred to #155 (PR #184)
  #158 Three simplex-constrained composition splits (R1 triage priority,
       DNBI sub-type, mass casualty priority) brought into the Morris
       screen through an isometric log-ratio transform, their nine
       parameters becoming six balance coordinates that vary
       independently while the sum-to-one constraint holds by
       construction; screened set 58 -> 64. apply_params() asserts the
       invariant per design point, check_composition_ilr.R asserts it
       across a whole design; run_sobol() gains a Dirichlet
       whole-composition path. No model change — no baseline shift;
       production ranking deferred to #155 (PR #187)
  #178 DOW ceilings investigated against the Ajax Bay treated-cohort
       bound and left unchanged: the reported 0.617% overshoot does not
       reproduce. Three independent 50-rep measurements of the unchanged
       base configuration give 0.348/0.400/0.499%, pooling to 0.416%
       (95% CI [0.346%, 0.485%]) over 150 reps, spanning the bound;
       moderate_intensity sits below it at 0.274% and is left there, the
       comparator being an upper bound. check_dow_calibration.R added to
       assert the comparison. No model change — no baseline shift;
       raised #189 (PR #190)
  #189 Antithetic pairing withdrawn from run_replications(); every
       replication now draws its own seed, so the replication is the
       unit of analysis the interval arithmetic already assumed.
       Withdrawn rather than extended: the negation cannot reach
       simmer's own event loop, and over 75 pairs it bought -0.04 on
       total casualties, the one response it did reach. The +0.38 on
       DOW count does not replicate (-0.005 over 75 pairs).
       check_replication_independence.R added, asserting independence
       structurally after a correlation-based version proved unable to.
       No model change — no baseline shift; every multi-replication
       interval re-measured (PR #192)
  #186 Three morris_params baselines corrected to the shipped configuration
       (evacuation_policy_days 0.10 to 30, r2e_hold_mode 12,960 to 38,880,
       surg_mode 120 to 95), two of them outside their own bounds. Morris
       never read the column; run_sobol() holds every unselected parameter
       there and the Shiny Calibration tab shows it as Baseline. Bounds now
       asserted at source time, and check_morris_baseline.R asserts the
       whole mode vector reproduces env_data.json, deriving its path
       mapping from apply_params(). No RNG shift — seed-42 run
       bit-identical (PR #194)
  #195 surg_mode re-derived from 90-150 to 57-133, Rule A around the shipped
       95-minute mode, moving the baseline from 0.083 of the way along its
       own range to 0.500. Original bounds traced to an "approximately
       ±25-50%" rule in the earliest README, applied to the old 120-minute
       mode; Rule A was unavailable on this row until Issue #76 dropped the
       distribution minimum from 90 to 41. Audit of the other 63 rows found
       none in the same state. Theatre occupancy severity invariance
       recorded as README Further Development L26, not closed. No RNG
       shift — morris_params reaches no ordinary run (PR #197)
  #153 Comparative scenario plotting made locale-independent: axis labels
       derived from the scenario identifier rather than by matching an
       em dash against a UTF-8-flagged label, plot title em dash written
       as an escape, UTF-8 locale requested at startup, app.R's two
       equivalent calls given useBytes. check_scenario_labels.R added.
       check_markdown.R's anchors rebuilt to GitHub's own algorithm,
       verified against the ids GitHub generates: 45 TOC anchors, 15 body
       cross-references and 216 citation links corrected, and an anchor
       link check added. No RNG shift — the 50-replication comparative
       run reproduces every published figure and a byte-identical
       scenario_comparison.png (PR #199)
  #148 Lognormal generator's fixed absolute per-minute rate cap of 5
       replaced with the mean-relative cap_multiplier x mean_daily the
       exponential generator has used since #54, at the same default
       multiplier of 3. Cross-stream truncation spread narrows from
       roughly 150-fold to under 6-fold, not to uniformity, a lognormal's
       tail above a multiple of its mean depending also on its coefficient
       of variation. Moves casualty generation itself, not only the draw
       order: seed-42 total 386 to 382, KIA 67 to 57, WIA 149 to 151.
       Pooled mortality re-measured over 150 replications; comparative
       scenario figures re-measured. Residual raised as #203 (PR #202)
  #146 R2B pre-open hold window — a casualty finding the theatre free and
       the surgical section closed for no longer than
       r2b.surgery.pre_open_window_min is held forward for the section
       rather than diverted, the seizes doing the waiting. Ships at 60
       min, recorded as README Further Development L28 and added to the
       Morris screen as a Policy lever (65 parameters).
       check_pre_open_window.R added. RNG-stream-shifting at a non-zero
       window, bit-for-bit identical at zero. Across 50 replications
       forward surgeries rise 50.8 to 56.4 against 5.6 held per run, with
       no detectable displacement of the cases behind them. R2E parity
       parameter not added — R2E has no surgical bypass to modify (PR #204)
  #203 Per-minute rate cap removed rather than retuned — the closure
       performs exactly n_minutes iterations whatever the draws, so the
       run-time failure mode the cap guarded against belongs to the
       vectorised generator it replaced. Every stream now realises its
       configured mean, against 78.7-99.2% before. Emission corrected so a
       minute accruing several casualties emits all of them, bit-for-bit
       identical at every shipped parameterisation.
       check_arrival_rate_fidelity.R added. Moves casualty generation
       itself: seed-42 total 382 to 437, WIA 151 to 187, KIA 57 to 71.
       DOW ceilings re-fitted 0.023/0.019 to 0.020/0.016 after the larger
       treated cohort overshot the Ajax Bay bound at 250 replications
       (PR #209)
  #208 run_replications() now snapshots and restores the caller's RNG
       kind and stream position, so a measurement is a function of its
       control seed alone rather than of its position in the invocation.
       The kind is deliberately still set after the seeds are drawn:
       RNGkind() re-initialises .Random.seed from the clock whenever it
       is called, so setting it first would have removed reproducibility
       rather than restored it. Both dispatch paths now run under one
       generator. check_measurement_reproducibility.R added. No model
       change — no seed-42 shift; DOW calibration re-measured to 0.417%
       (default) and 0.353% (moderate_intensity), comparative scenario
       tables re-measured (PR #211)
  #206 Per-minute rate walk replaced with direct arrival-time sampling:
       the rate is drawn once per simulated day, the timescale FORECAS
       fitted it at, and arrivals are placed within the day by thinning
       (Lewis & Shedler 1979) against a dominating rate at establishment
       strength, each candidate accepted at the live force size so the
       #18 feedback loop is preserved. The daily count is Poisson given
       the day's rate, so the stream realises the configured mean plus
       the configured between-day variance. Combat WIA daily sd 0.50 to
       9.5 against a Poisson 2.10, busiest day 6 to 564. Sub-minute
       jitter removed with the grid; cost now linear in the drawn rate.
       check_arrival_rate_fidelity.R gains the variance assertion. Moves
       casualty generation itself: seed-42 total 437 to 530, WIA 187 to
       287, DNBI 179 to 171. DOW ceilings re-verified at 150 reps and
       left unchanged. Mass casualty parameters reconsidered and left
       unchanged. L27 deleted (PR #215)
  #201 Thirteen merged issues backfilled into the phase sequence lists
       and their phase heading rosters, five reported and eight found by
       audit; every issue the summary table records as merged now has
       both. check_markdown.R's anchor link check widened from the three
       TOC-carrying documents to every tracked markdown document, links
       inside backticks excluded; TOC and return-link maintenance stays
       scoped to the three. Issue 44 citation repaired. Documentation
       only — no model change, no baseline shift (PR #213)
  #150 DNBI sub-type surgical-requirement statistics re-measured at 100
       replications of 30 days from control seed 42, the Issue #7 figures
       having predated every RNG-stream-shifting merge since. 158.6 (SD
       6.8) casualties requiring surgery per replication to 183.5 (SD
       36.0); 79.6% to 81.1% NBI, 5.7% to 6.0% disease, 0.0% battle
       fatigue unchanged. Qualitative conclusion unchanged; dispersion is
       the movement, at a fifth of the mean rather than a twenty-third.
       Provenance note added to CLAUDE.md. Documentation only — no model
       change, seed-42 log byte-identical (PR #217)
  #149 Mass casualty event casualty count split between the wounded and
       the immediately killed by one Binomial draw per event, the killed
       overlaid on kia_cbt and taking the mortuary pathway the background
       killed stream already takes. New mass_casualty.event.kia_fraction
       at 0.28, an informed estimate anchored on the model's own combat
       stream means. DNBI stays out on causal-link grounds.
       check_mass_casualty_kia_split.R added; L20 deleted. Injection
       ships disabled, so no baseline value moves and the seed-42 log is
       byte-identical (PR #219)
  #207 Reinforcement credit no longer clamped at establishment strength —
       a fill fraction above 1 delivers more than the shortfall it was
       requested against and carries the pool over strength until
       casualties bring it back down; reinforcement_force_bound() widens
       the arrival generators' thinning bound to match. A casualty
       evacuated from R2B holding under evac_threshold serves the
       remainder of the convalescence already drawn rather than a fresh
       draw. Added check_lever_realisation.R. Both features ship
       disabled; seed-42 log byte-identical (PR #224)

  #151 high_intensity given its own died-of-wounds ceilings and treatment
       efficacy factors in place of the Falklands-calibrated pair it
       inherited, calibrated against Okinawa's own reported 3.4% among
       casualties reaching a hospital alive (Marble, 2025). Reaches 3.471%
       (95% CI [3.360%, 3.583%]) at p1_p_max 0.052 / p2_p_max 0.042; the
       efficacy factors are informed estimates for 1945 Pacific-theatre
       care. check_dow_calibration.R now holds a target per configuration.
       Comparative scenario tables re-measured; base configuration
       untouched and the seed-42 log byte-identical (PR #221)

IN REVIEW (PRs open against main):
  #230 Verification baseline — all fifteen scripts/check_*.R checks and the
       seed-42 reproduction executed at commit edd6285 in the pinned Dev
       Container. All fifteen pass; the tracked seed-42 evidence set
       reproduces byte for byte. Recorded as a tracked table in a new
       scripts/README.md (PR #243)

UNBLOCKED (start now):
  #227 Delete the twelve wip/* checkpoint refs — the evidence set is on main
       under data/sensitivity/, so the refs are redundant; deletion needs the
       GitHub UI or a local clone, the session git proxy silently refusing it.
       Unblocked by the merge of #226.
  #228 Higher-resolution Sobol decomposition — N ~ 800 at 8 to 12 replications
       to separate the leading pair and bring the measured 32.9% replication
       noise share under 20%. Closes Further Development L29. Unblocked by the
       merge of #226.
  #231 Repair the README reference list — duplicated entries, misattributed
       authors, and two sources violating the open-access rule. Text only.
  #232 All ten README image links are broken — repair the paths and extend
       check_markdown.R to validate link targets. Text and tooling only.
  #233 The analysis pipeline consumes RNG, so analyse_run() is not
       idempotent — make it stream-neutral. Not a prerequisite for #241.
  #234 Rewrite docs/STYLE_GUIDE.md as an enforceable R code standard,
       including a commenting standard. Gates #235 and #241.
  #236 Global configuration save/restore is not exception-safe, and
       R/analysis.R has no input validation.
  #237 Housekeeping — delete the ten wip/* branches, reconcile the Further
       Development scan table, close CLAUDE.md drift.
  #238 Re-cut the analysis papers by method — move the replicated
       experiments out of the single-run paper. Gates #239 and #240.

BLOCKED (gated on other issues):
  #235 lintr, a glob-discovered scripts/run_all_checks.R, and GitHub Actions
       CI so the regression checks become a per-PR gate, with the slow checks
       scheduled rather than run per PR. Blocked on #230, which establishes
       which checks pass and how long each takes, and on #234, which supplies
       the rules .lintr encodes.
  #241 Apply the code standard — decompose server, analyse_run and
       analyse_replications behind testServer and Playwright verification,
       verified byte for byte against the tracked seed-42 baseline. Blocked
       on #234 (the length limit), #235 (the runner both suites plug into)
       and #230 (the checks passing before anything moves).
  #239 Single-run analysis paper to publication standard. Blocked on #238,
       which settles what the document contains, and #230, for the
       figure-verification pass.
  #240 Multi-run analysis paper to publication standard. Blocked on #238 and
       #230, for the same two reasons.

BACKLOG (unblocked but deprioritised — not currently planned):
  #4   Individual resource seizure   (gating satisfied: #1 + #2 + #3 all merged;
       parked given its size/risk — largest structural change in the project)
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

*Prepared June 2026. Updated 23 August 2026 to reflect: the merge of Issue #155 (PR #226), the terminal canonical re-run, which rebuilds every published figure from commit `ed3c426` in the pinned Dev Container, retires the twenty-one accumulated provenance caveats as correct, rebuilds the Morris ranking at sixty-five parameters, withdraws three published claims that did not survive re-measurement, and tracks the sensitivity evidence set at `data/sensitivity/`; and the two issues it raised, #227 (checkpoint ref cleanup) and #228 (higher-resolution decomposition). Further updated 23 August 2026 to scaffold two new phases, Phase 6 (code quality and verification, Issues #230 to #237 and #241) and Phase 7 (publication, Issues #238 to #240), and to record Issue #230 in review under PR #243; #227 and #228 move from BLOCKED to UNBLOCKED, the merge of #226 that gated them having landed.*
