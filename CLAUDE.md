# CLAUDE.md — Battlefield Casualty Handling Simulation

## Project Purpose

This is an **academic research project** producing a Discrete Event Simulation (DES) of deployed battlefield casualty handling. The simulation is written in R using the `simmer` package and is intended to provide evidence-based options to military planners for improving health outcomes in Large Scale Combat Operations (LSCO).

All work must meet academic research standards: reasoning must be explicit, sources must be cited, and limitations must be acknowledged. The project's academic output is split across four documents, each kept current with the code and written to the standard of a published academic paper: `README.md` (system reference — code structure, algorithms, trajectory logic, resource model, inline model assumptions, and Limitations), `docs/Results.md` (every measurement of the replicated experiments and the verification of one seed-42 campaign, generated from the tracked evidence, with no analysis or recommendation), `docs/Planning_Implications.md` (every lever read as a planning option, quoting `docs/Results.md` for every figure), and `docs/Methods.md` (the experimental design and statistical method behind those experiments). See [Document Maintenance](#document-maintenance) below for which PR types update which document.

---

## Repository Structure

The codebase is organised into a modular layout under `R/`, with `run.R` as the single CLI entry point. See the README's [Codebase Structure](README.md#codebase-structure) table for full detail on each `R/` module; this table covers the repository as a whole.

| File / Directory | Purpose |
|---|---|
| `run.R` | CLI entry point — validates arguments (via `R/cli.R`, before any simulation runs), orchestrates modules, and writes outputs. Takes `--scenario`, `--mode`, `--images-dir` and `--max-cores` alongside the run parameters |
| `R/constants.R` | Values shared across modules: `DAY_MIN` (minutes per simulated day) and `MODEL_ATTRIBUTE_KEYS`, every per-casualty attribute key the model can set, which `build_attributes_wide()` uses to give the wide pivot of the attributes monitor one shape whatever a run produced. Sourced by each module that needs one rather than by one module on every other's behalf, the modules under `R/` being otherwise independent |
| `R/cli.R` | Command-line argument validation shared by `run.R` and `scripts/run_warmup.R` — range and directory rules, the warm-up-against-run-length rule, execution mode resolution, and the two conditions guarding a baseline refresh. Base R only, so a regression check can source it alone and exercise every rule without loading simmer or running the model |
| `R/censoring.R` | Right-censored interval estimation: the Kaplan-Meier survival curve, its restricted mean and quantile, and `censored_interval_stats()`, which summarises one interval over every casualty who entered it rather than over those who also left it. Base R and independent of every other module, so `R/analysis.R` and `R/sensitivity.R` both source it and a KPI cannot drift from the screened response of the same name |
| `R/queue_series.R` | A resource pool's total queue as a step function of time, its time-weighted mean over a series of bins, and whether it ever cleared. The monitor records each bed separately, so the pool total is in none of its rows; it is recovered by differencing each bed's series into changes and accumulating them in time order. Base R and independent of every other module, so `R/analysis.R` and `R/long_horizon.R` both source it and the campaign figures and the long-horizon block series measure the same quantity by the same estimator |
| `R/long_horizon.R` | Sustained-operations replicated runner. Runs the model over the protocol's horizon and reduces each replication to a daily series inside the forked worker that produced it, so the parent holds the series rather than the monitoring data and peak memory is set by one replication rather than by their sum; `block_means()` collapses that series to the per-block means the protocol's stability statement is made from |
| `R/airlift.R` | Strategic evacuation and Role 4 demand across replications. Reduces each replication to one row of the response set inside the forked worker that produced it, sweeps a `role4.ame` field across values, and splits R2E holding bed occupancy into in-theatre recovery and evacuation wait. The split is exact rather than estimated: a casualty awaiting the standard airlift pool holds a holding bed for its whole wait, one awaiting the critical pool holds an intensive care bed instead and contributes nothing. The Role 4 census is drawn under the replication's own seed, that length of stay being drawn by the analysis rather than the simulation, so the peak is a function of the seed rather than of the caller's stream position |
| `R/policy_sweep.R` | The evacuation policy and holding establishment trade across replications at the sustained horizon. Reduces each replication to one row of the response set inside the forked worker that produced it, on `R/airlift.R`'s arrangement, and reports the trade a policy consists of: forward stability and post-operative intensive care access against returns to duty and demand on the national support base. Forward stability is measured over the campaign's closing days rather than its whole length, a system still filling being indistinguishable from a settled one in an average over both. Differences between policies are taken within replication, each arm's own spread across campaigns being large enough to hide them |
| `R/hold_window.R` | The R2B pre-open hold window across replications, comparing the shipped 60-minute window against a window of zero. Reduces each replication to one row of the response set inside the forked worker that produced it, on `R/policy_sweep.R`'s arrangement, every response being a casualty count read off a single arrival attribute. The two arms are not the same realisation: introducing the hold shifts `simmer`'s single global stream from the first hold onward, so the arms drift into different casualty streams and pairing on a common control seed removes none of the between-run variance; the paired difference is reported anyway as the more precise of the two comparisons available |
| `R/icu_gate.R` | The post-operative intensive care gate across replications, comparing the shipped rationing rule (`icu_gating.enabled`) against a configuration that reconstructs the model as it stood before the gate existed. Reduces each replication to one row of the response set inside the forked worker that produced it, on the arrangement `R/hold_window.R` uses for a paired two-arm comparison; the two modules stay separate rather than sharing a runner. Intensive care occupancy is recovered from the resource monitor over the whole campaign on `R/policy_sweep.R`'s pool-differencing convention, and the pathway death counts are carried as raw numerators and denominators so a rate can be pooled across the tracked evidence set rather than averaged per replication |
| `R/mass_casualty.R` | The mass casualty event stress test across replications, comparing an arm with events injected against a background-only arm. Reduces each replication to one row of the response set inside the forked worker that produced it, on `R/hold_window.R`'s arrangement, but the two arms are not paired on one control seed, each running under its own: every casualty carries a `mass_casualty_event` attribute of 0 or 1 regardless of whether injection is active (`R/trajectories.R`), so one reduction applies unchanged to both arms rather than a pairing removing variance an unpaired design still carries. The died-of-wounds rate is pooled across replications rather than averaged per replication, a handful of deaths per 30-replication arm leaving most replications with none, with an exact binomial interval rather than a normal approximation that could extend below zero |
| `R/results.R` | Result tables generated from the tracked evidence sets. Holds the builders behind every table and quoted figure of `docs/Results.md`, the cell reader that lets a sentence quote a table cell, `render_results()`, which regenerates each `GEN` span of the document, and `seed42_verification_rows()`, the verification measurements `run.R` writes to `data/seed42_verification.csv`. Base R only, so a regression check can source it without simmer |
| `R/environment.R` | Data import, arrival generation, and simmer environment construction |
| `R/trajectories.R` | All simmer `trajectory()` definitions — R1, R2B, R2E, and core casualty flow |
| `R/replication.R` | Multi-run replication framework (`run_once`, `run_replications`, `summarise_replications`) |
| `R/analysis.R` | Analysis and visualisation pipeline. `analyse_run()` and `analyse_replications()` are orchestrators over named single-purpose functions, one per stage (data preparation, per-domain summary, plotting, writing); a change to one stage belongs in that stage's function |
| `R/sensitivity.R` | Morris EE screening and Sobol variance decomposition |
| `R/warmup.R` | Welch warm-up analysis |
| `R/app_params.R` | Parameter registry for the Shiny Configure panel |
| `R/scenario.R` | Scenario overlay mechanism (`resolve_scenario`, `merge_scenario_vars`); the profiles themselves are defined in `env_data.json`'s `scenarios` block |
| `R/scenario_runner.R` | Comparative scenario runner — executes the replication framework under a named scenario profile |
| `app.R` | Shiny console. `server()` is an orchestrator over per-panel functions, one per tab and one per asynchronous run; a change to one panel belongs in that panel's function — Configure/Run/Analyse workflow for interactive `env_data.json` parameter editing, Quick Run, Full Analysis (multi-run with 95% CI), and Sensitivity Screening (Morris/Sobol) execution (Issues #14, #15) |
| `env_data.json` | All simulation parameters — populations, resources, distributions, schedules |
| `scripts/run_sensitivity.R` | CLI entry point for sensitivity analysis |
| `scripts/run_warmup.R` | CLI entry point for Welch warm-up analysis. Takes `--seed`, so a published warm-up figure is reproducible from the command line, and writes under `--output-dir`/`--images-dir`; the tracked `images/welch_plot_icu_queue.png` is written by `--refresh-baseline` alone |
| `scripts/run_long_horizon.R` | CLI entry point for the sustained-operations horizon. Runs the protocol `docs/Methods.md` documents at each casualty intensity and writes both the reduced daily series and the per-block means. Each block's mean is computed from that block's days alone, so one run yields twelve of them and no separate durations are run; it is not the same campaign a shorter run at the same seed would produce, the arrival stream being drawn over the horizon requested. `--refresh-baseline` is the only way to write the tracked `data/long_horizon/` |
| `scripts/render_long_horizon_warmup.R` | CLI entry point for the Welch cumulative-moving-average diagnostic at the sustained-operations horizon, computed for the two R2E bed pools the per-block classification finds closest to the boundary at moderate intensity. Renders from the tracked `data/long_horizon/long_horizon_series.csv.gz` alone and runs no simulation of its own. `--refresh-baseline` is the only way to write the tracked `images/welch_plot_long_horizon.png` and `data/long_horizon/long_horizon_welch_cma.csv` |
| `scripts/run_airlift_sweep.R` | CLI entry point for the replicated strategic evacuation and Role 4 measurement, and the sweeps over sortie reliability and the interval between sorties. The experiment `docs/Methods.md` recorded as the one a reader could not re-execute from a tracked command. `--refresh-baseline` is the only way to write the tracked `data/airlift/` |
| `scripts/run_airlift_collapse.R` | CLI entry point for the strategic airlift collapse experiment at the sustained horizon. Sweeps sortie cancellation across a replicated 360-day campaign and reduces each replication to a classification rather than a mean, the per-replication values being bimodal rather than spread: a campaign collapses where its R2E holding queue over the closing 90 days averages twenty casualties or more. The two halves of the experiment exist separately in `scripts/run_long_horizon.R` (the horizon, no sweep) and `scripts/run_airlift_sweep.R` (the sweep, a 30-day horizon, means); this is the two together with the classification the published result rests on. `--refresh-baseline` is the only way to write the tracked `data/airlift/` collapse copy |
| `scripts/run_policy_sweep.R` | CLI entry point for the evacuation policy and R2E holding establishment sweeps at the sustained horizon. Sweeps `r2eheavy.recovery.evacuation_policy_days` across the range its doctrinal source states is a command decision, and `--hold-beds` sweeps the establishment alongside it, the two being substitutes whose relative price is the planning input; the grid is their cross product. The arms are paired on one control seed each so a difference is measured within replication, and each response is reported with a 95% confidence interval alongside the paired difference against the shipped policy and the replication count each unresolved difference would need. An establishment sweep writes its own `establishment_sweep*` files rather than adding rows to the policy sweep's, that being a published result with a check reading it. Each arm is checkpointed as it completes and resumed rather than re-run, so an interruption costs the arm in flight rather than the whole measurement. `--refresh-baseline` is the only way to write the tracked `data/policy/` |
| `scripts/run_hold_window.R` | CLI entry point for the R2B pre-open hold window comparison, on `scripts/run_policy_sweep.R`'s arrangement for a paired two-arm comparison. Both arms run under one control seed, so replication $i$ of each shares a per-replication seed without sharing a casualty stream. `--refresh-baseline` is the only way to write the tracked `data/hold_window/`, and it runs the documented protocol (30 replications x 360 days x 2 arms at seed 42, migrated to the sustained-operations protocol under Issue #405) rather than whatever arguments accompany it |
| `scripts/run_icu_gate.R` | CLI entry point for the post-operative intensive care gate comparison, on `scripts/run_hold_window.R`'s arrangement for a paired two-arm comparison. Both arms run under one control seed, so replication $i$ of each shares a per-replication seed. `--refresh-baseline` is the only way to write the tracked `data/icu_gate/`, and it runs the documented protocol (30 replications x 360 days x 2 arms at seed 42, migrated to the sustained-operations protocol under Issue #405) rather than whatever arguments accompany it |
| `scripts/run_mass_casualty.R` | CLI entry point for the mass casualty event stress test, on `scripts/run_hold_window.R`'s arrangement for a two-arm comparison whose arms are not paired on one control seed, each replication reduced to one row of the response set inside the forked worker that produced it. Also writes the illustrative single run behind `images/mass_casualty_events.png`, the one tracked image `run.R --refresh-baseline` cannot produce because injection ships disabled. `--refresh-baseline` is the only way to write the tracked `data/mass_casualty/`, and it runs the documented protocol (30 replications x 360 days x 2 arms at seed 42) rather than whatever arguments accompany it |
| `scripts/run_saturation_sweep.R` | CLI entry point for the forward surgical saturation release sweep at the sustained horizon. Sweeps `r2eheavy.second_surgery.saturation_queue_threshold` across the range the R2E theatre queue reaches, past the point at which a threshold stops firing so that an inert value is identified by measurement rather than assumed, and reports the trade the lever consists of: a freed theatre slot forward against an intensive care bed held through the pre-flight wait, an operation owed at the national support base and a casualty who arrives there unrepaired. Runs on `R/policy_sweep.R`'s arrangement, each arm paired on one control seed and checkpointed as it completes, and writes its own `saturation_sweep*` files. `--refresh-baseline` is the only way to write the tracked `data/policy/` copy |
| `scripts/run_scenarios.R` | CLI entry point for the comparative scenario runner. `--refresh-baseline` is the only way to write the tracked `data/scenarios/` and `images/scenario_comparison.png`, and it runs the protocol `R/scenario_runner.R` holds rather than whichever arguments accompany it, so the tracked set and the design the supplement documents cannot diverge through a mistyped argument |
| `scripts/render_dow_survival.R` | Renders `images/dow_survival_function.png` from the `dow.params` block of `env_data.json`, for the base configuration or a `--scenario` profile, so a re-fitted `p_max` cannot leave the figure disagreeing with the calibration table beneath it; `--refresh-baseline` is the only way to write the tracked image |
| `scripts/render_paper_figures.R` | Renders the three result tables of `docs/Results.md` as figures, parsing the values out of the results paper's own markdown tables rather than holding a second copy of them, so a figure cannot disagree with the table it illustrates; fails rather than writing where a table has moved or no longer parses, and `--refresh-baseline` is the only way to write the tracked `images/paper_*.png` |
| `scripts/render_results_tables.R` | Regenerates every table and quoted figure of `docs/Results.md` and `docs/Planning_Implications.md` from the tracked evidence sets through `R/results.R`, replacing the content of each generated span and touching nothing outside one. An ordinary invocation writes under `outputs/`; `--refresh-baseline` is the only way to rewrite the tracked documents |
| `scripts/render_check_table.R` | Regenerates the per-check table of `docs/Continuous_Integration.md` from the checks themselves: each row's name from the runner's glob, its tier from the runner's `SLOW_CHECKS`, its runtime from `scripts/check_runtimes.csv` and its summary from the check's own banner, so a check cannot be added without appearing in the guide. An ordinary invocation writes `outputs/Continuous_Integration.md`; `--refresh-baseline` is the only way to rewrite the tracked guide |
| `scripts/render_time_series_figures.R` | Measures and renders the two campaign time series of `docs/Results.md`, queue length by resource pool and the degraded post-operative care rate, at both casualty intensities. `--run` executes the model and writes the aggregated series as CSV; an ordinary invocation renders from those CSVs and nothing else, so a figure is a function of tracked data rather than of a run nobody can repeat and re-rendering reproduces both images. `--refresh-baseline` is the only way to write the tracked `images/*_over_time.png` and `data/time_series/` |
| `scripts/render_morris_plots.R` | Re-renders a completed sensitivity screen's Morris scatter plots from its saved design and responses, without running the model again, checking each response's recomputed µ\* and σ against the tracked ranking CSV before writing so a plot cannot drift away from the table it illustrates; `--refresh-baseline` is the only way to write the tracked `images/morris_*.png` |
| `scripts/screen_cache.sh` | Checkpoints a sensitivity screen's point cache onto its own git ref and restores it, so a multi-hour screen survives an environment that reclaims its filesystem mid-run |
| `scripts/supervise_screen.sh` | Drives a long screen to completion across environment failures, restoring the cache before each attempt and checkpointing while the screen runs |
| `scripts/compare_sobol_estimators.R` | Recomputes a completed Sobol decomposition's cached responses under the Jansen and Martinez pick-freeze estimators alongside the reported Saltelli one, which share the same design and so cost no further simulation, and reports whether the ordering and the separations survive the change of estimator |
| `scripts/measure_noise_floor.R` | Measures how much of a completed Sobol decomposition's variance is replication noise rather than parameter effect, by evaluating a sample of design points at many more replications than the decomposition used; reports the factor the reported indices are deflated by and the replications per point that would make it negligible |
| `scripts/test_sobol_separation.R` | Tests whether a completed Sobol decomposition separates one parameter from the next, bootstrapping the design rather than the indices so that two indices estimated from the same evaluations keep their correlation, and reports the sample size each unestablished separation would require |
| `scripts/run_transport_sweep.R` | CLI entry point for the transport fleet-size sweep. `--refresh-baseline` is the only way to write this sweep's copy of the tracked `data/sweeps/`, and it fixes the swept range, replication count, horizon and seed rather than accepting the caller's; `--scenario` still applies, so the same design can be run under the shipped configuration or a named profile (e.g. `high_intensity`), each writing its own suffixed file |
| `scripts/run_icu_share_sweep.R` | CLI entry point for the forward ICU share (R2B post-operative stabilisation) sweep. `--refresh-baseline` is the only way to write this sweep's copy of the tracked `data/sweeps/`, on the same convention as the transport sweep beside it |
| `scripts/run_hold_threshold_sweep.R` | CLI entry point for the joint R2B holding capacity and evacuation threshold sweep. Sweeps `env_data.json`'s per-unit R2B holding bed establishment against `r2b.holding.evac_threshold` as a cross product, on `scripts/run_policy_sweep.R`'s establishment-sweep arrangement, and reports the forward R2B holding pool the two levers act on alongside the R2E holding and intensive care pools the transferred load lands on, plus returns to duty and died of wounds. `--refresh-baseline` is the only way to write this sweep's copy of the tracked `data/sweeps/` and the tracked `images/r2b_hold_threshold_sweep.png` |
| `scripts/shiny_worker.R` | Background worker sourced by `app.R` for async Quick Run / Full Analysis execution |
| `scripts/check_env_data_summary.R` | Regenerates the `<!-- ENV SUMMARY -->` block inside `README.md` from `env_data.json` |
| `scripts/check_markdown.R` | Maintains the TOC and "Return to Top" links across `README.md`, `docs/Results.md`, `docs/Planning_Implications.md` and `docs/Methods.md`, generating each anchor as GitHub does, and asserting that its own entry-heading match is byte-wise so the check does not depend on the session locale; exits non-zero if any anchor link points at no heading, if any local link or image target does not exist when resolved relative to the document containing it, if any image carries placeholder or empty alt text, or if a row of the README's Further Development scan table names a gap or an impact differently from the entry it points at. The link, target and alt-text checks run across every tracked markdown document, including this one and `docs/BCH_Simulation_Action_Plan.md` (which carry no TOC block and must not be given one); the scan table check applies to `README.md` alone. External URLs are out of scope |
| `scripts/check_references.R` | Regression check asserting that each of the four academic documents' reference lists is sound: every `[[n]]` citation resolves to an entry, every entry is cited at least once, the list is numbered from one in order of first appearance, no two entries share a URL, and every entry carries a URL and a retrieval date. Whether a URL is open access is a judgement the script cannot make and remains a manual step at the point a reference is added; exits non-zero on failure |
| `scripts/check_r2e_surgery_seizure.R` | Regression check asserting that every R2E surgery seizes a surgical section, structurally and behaviourally; exits non-zero on failure |
| `scripts/check_icu_gate_switch.R` | Regression check asserting that `icu_gating.enabled` reproduces the model as it stood before the pre-theatre intensive care gate existed: with the gate disabled no casualty is deferred and no operated casualty takes the degraded holding-bed recovery, with it in force both pathways are reachable so the first assertion is not vacuous, the shipped value is the enabled one, and a malformed value is rejected with a message naming the field. The R2B half is asserted at a non-zero forward intensive care share, that gate being close to inert at the shipped share of zero; exits non-zero on failure |
| `scripts/check_icu_gate_protocol.R` | Regression check asserting that the post-operative intensive care gate's parameters, its responses and its published section agree: every protocol parameter in `R/icu_gate.R` equals the value `docs/Methods.md` documents, the tracked evidence set carries both documented arms at the documented replication count and the response set the section reports, the tracked summary and paired differences are each the reduction of the tracked per-replication responses beside them, every figure the section prints matches the tracked measurement, and `summarise_icu_gate()` and `icu_gate_paired_difference()` are correct on inputs whose answers, including the interval construction, are computable by hand. Covers the magnitudes alongside `scripts/check_icu_gate_switch.R`, which covers the mechanism; exits non-zero on failure |
| `scripts/check_role4_surgical_demand.R` | Regression check asserting that the operating theatre demand a casualty carries to the national support base is faithful across both populations that generate it. For the casualty released with the definitive repair outstanding, that the requirement is conserved rather than re-estimated: the releasing theatre draws the operation and the casualty carries it rearward, so the reported theatre-minutes equal the minutes drawn and re-analysing one run reproduces the report exactly, the analysis taking no draw of its own. Also asserts that exactly one operation is owed per released casualty who reached Role 4 and none for a casualty who arrived repaired, that the operation falls on the day of admission ahead of the intensive care that follows it, that such a casualty is owed their whole post-operative intensive care requirement there, and, as an absence, that neither the analysis module nor `env_data.json` gives the echelon a theatre capacity, queue or shortfall, the model reporting the demand rather than simulating the echelon. For the reconstruction cohort, that the sequence is interval-driven rather than count-driven, so lengthening the return interval reduces the procedure count while the reconstruction itself survives; that each casualty receives exactly one reconstruction preceded by debridements; that reconstruction evacuates a casualty whatever their drawn recovery, changing the disposition at a 60-day policy rather than restating it, with the other two evacuation reasons still reachable; that a malformed return interval is rejected naming the field; and that the report is idempotent and leaves the caller's random number stream where it found it, the interval being drawn. Both levers ship disabled and a share of zero reproduces the campaign a configuration without the field produces; exits non-zero on failure |
| `scripts/check_role4_ward_phases.R` | Regression check asserting that a Role 4 stay split between an intensive care phase and a step-down ward conserves its length exactly: the shipped configuration reproduces the single-ward census it replaced and consumes no draw of its own, a split stay's phases account for the whole stay with no gap and no overlap, a requirement longer than the stay fills it rather than overrunning it, a casualty theatre never operated on is owed nothing, and a malformed ward mapping or continuation block is rejected naming the field. The split is taken as a difference rather than drawn, so conservation is structural; this is what keeps it so. Also asserts an absence: that neither the analysis module nor `env_data.json` gives the national support base a capacity, the model setting the demand a deployed trauma system places on that echelon rather than simulating it, so a bed count or a shortfall against one cannot be reintroduced silently; exits non-zero on failure |
| `scripts/check_icu_time_conservation.R` | Regression check asserting that a casualty's post-operative ICU requirement is conserved across all three routes and at every forward ICU share; exits non-zero on failure |
| `scripts/check_definitive_repair_release.R` | Regression check asserting that the release of a damage control casualty to strategic evacuation with the definitive repair outstanding is faithful to what it claims: the shipped threshold of zero disables it and reproduces the campaign a configuration without the field at all produces, so the lever costs the published model nothing; a malformed threshold is rejected naming the field; the release is reachable at a threshold in force, so the disabled assertion is not vacuous; and every released casualty is on the damage control pathway, was operated on at Role 2E, never had the second procedure, served no post-definitive episode and was evacuated rather than returned to duty. Exits non-zero on failure |
| `scripts/check_composition_ilr.R` | Regression check asserting that each simplex-constrained composition group stays on the simplex through its screened balance coordinates; exits non-zero on failure |
| `scripts/check_morris_baseline.R` | Regression check asserting that every screened parameter's baseline lies inside its own screening bounds and equals the value it holds in `env_data.json`; exits non-zero on failure |
| `scripts/check_dow_calibration.R` | Regression check asserting that each shipped configuration's treated-cohort died-of-wounds rate agrees with the historical anchor of the campaign it models, the Ajax Bay bound for the two Falklands-calibrated configurations and the reported Okinawa rate for `high_intensity`, pooling independent measurements at the 30-day horizon the anchors describe, and with `--write-csv` writing the pooled measurement the results paper reports at 360 days; exits non-zero on failure |
| `scripts/check_mass_casualty_kia_split.R` | Regression check asserting that a mass casualty event's casualty count is conserved across the wounded/killed split, that the realised killed share tracks the configured one, that an event's killed reach mortuary handling untriaged, and that the share reaches nothing while injection is disabled; exits non-zero on failure |
| `scripts/check_mass_casualty_protocol.R` | Regression check asserting that the mass casualty stress test's parameters, its responses and its published table agree: every protocol parameter in `R/mass_casualty.R` equals the value `docs/Methods.md` documents, the tracked evidence set carries both documented arms at the documented replication count and the response set the table prints, the count summary and the died-of-wounds summary are each the reduction of the tracked per-replication responses beside them, every figure the table prints matches the tracked measurement, and `summarise_mass_casualty_counts()` and `mass_casualty_dow_rate()` are correct on inputs whose answers, including the exact binomial interval, are computable by hand. Covers the magnitudes alongside `scripts/check_mass_casualty_kia_split.R`, which covers the injection mechanism; exits non-zero on failure |
| `scripts/check_results_tables.R` | Regression check asserting that `docs/Results.md` is what the tracked evidence says: re-rendering the document from `data/` reproduces it exactly, the table builders and the cell reader are correct on hand-computed inputs, the resolution and pathway builders are correct on hand-computed paired inputs, and no decimal figure, measured percentage or recommendation is typed outside a generated span; exits non-zero on failure |
| `scripts/check_planning_implications.R` | Regression check asserting that `docs/Planning_Implications.md` quotes `docs/Results.md` faithfully: re-rendering the document from `data/` reproduces it exactly, so a quoted figure cannot differ from the evidence it cites; no decimal figure or measured percentage is typed outside a generated span; every paragraph or table row carrying a quoted figure links a results section that exists; the lever table covers every lever section of the results paper and labels each with an evidence label; the half-widths behind the resolution table equal those the experiments' own scripts sized their replication counts against; and no design or provenance content (issue numbers, script invocations, file paths) remains. Fault-injected: a wrong figure inside a span and a typed decimal each fail it; exits non-zero on failure |
| `scripts/check_ci_check_table.R` | Regression check asserting that the per-check table of `docs/Continuous_Integration.md` lists every check: the guide's table equals the one `scripts/render_check_table.R` builds from the checks as they stand, every `scripts/check_*.R` the runner would discover has a row, no row names a check that no longer exists, and every check has a measured runtime in `scripts/check_runtimes.csv`; exits non-zero on failure |
| `scripts/check_lever_realisation.R` | Regression check asserting that two configured planner levers are applied in full: that every person of a reinforcement fill joins the population even where that carries a pool over establishment strength, and that a casualty evacuated from R2B holding under `evac_threshold` serves the remainder of the convalescence already drawn rather than a fresh draw; exits non-zero on failure |
| `scripts/check_convalescence_invariance.R` | Regression check asserting that a casualty's convalescence follows the casualty and not the facility: the duration a casualty carries equals the one drawn at R2B holding, a casualty evacuated part-way through serves the same total across the two echelons, casualties of one severity class draw the same mean duration whether the draw was taken at R2B or at R2E, the mean follows the configured severity factors and an operated Priority 1 casualty draws longer than an unoperated one, and a casualty whose duration exceeds the evacuation policy is never held forward while one within it is; exits non-zero on failure |
| `scripts/check_console_bindings.R` | Regression check asserting that no Shiny console panel function reads a name another panel binds locally, which parses and loads but renders an error on the panel that reads it, and that no panel function is left uncalled; exits non-zero on failure |
| `scripts/check_analysis_decomposition.R` | Regression check asserting that every stage of the analysis pipeline binds what it returns: that no stage returns a name bound only inside a conditional, which fails whenever that conditional does not fire, and that no stage returns a value nothing reads; exits non-zero on failure |
| `scripts/check_analysis_idempotence.R` | Regression check asserting that the analysis pipeline is idempotent: that two consecutive `analyse_run()` calls on one monitoring list return the same Role 4 census and write the same outputs, that `analyse_replications()` does the same including its jittered image, and that neither leaves the caller's random number stream advanced; exits non-zero on failure |
| `scripts/check_replication_independence.R` | Regression check asserting that `run_once()` is a pure function of its seed and that `run_replications()` draws a distinct seed per replication, the two properties that make replications independent; exits non-zero on failure |
| `scripts/check_config_restore.R` | Regression check asserting that an error raised inside a capacity sweep, a sensitivity screen or the scenario runner leaves `env_data`, `day_min` and `counts` at their pre-call values, and that `run_scenario()` restores them on its success path and leaves them unbound where they began unbound; exits non-zero on failure |
| `scripts/check_input_validation.R` | Regression check asserting that the analysis module's entry points (`analyse_run()`, `analyse_replications()` and the two capacity sweeps) and the Shiny console's configuration-loading boundary reject malformed input with a message naming the missing column or offending field, and accept what the model produces and ships; exits non-zero on failure |
| `scripts/check_screen_cache.R` | Regression check asserting that a sensitivity screen's design-point cache resumes what it recorded: a complete row round-trips, a partially-missing row reads as present with its gaps preserved, an all-missing row reads as absent, an uncached point or a foreign cache reads as absent, and a cache missing a response the caller is about to request is archived under a `.stale-` suffix rather than resumed with that response silently unfilled; exits non-zero on failure |
| `scripts/check_screen_order.R` | Regression check asserting that a sensitivity screen evaluates its design in index order and exactly once: that both drivers' designs and evaluation orders repeat at one control seed and differ at another, that each point's parameters match the design row it claims, that a resumed screen re-evaluates nothing, and that a response vector carries the names the drivers index it by; exits non-zero on failure |
| `scripts/check_sensitivity_protocol.R` | Regression check asserting that the sensitivity screens' documented design is the design their tracked run metadata records: every parameter `docs/Methods.md` states in a marker comment (Morris trajectories, parameters, design points and replications; Sobol sample size, parameters, design points and replications; the 30-day horizon) equals the value the corresponding `*_run_metadata.csv` holds, each design is the size its parameters imply (r(k+1) and N(k+2) points), and the result files behind each metadata file were kept. It defends the design alone; the published rankings and indices are not asserted. Runs in seconds without simmer; exits non-zero on failure |
| `scripts/check_measurement_reproducibility.R` | Regression check asserting that a multi-replication measurement is a function of its control seed alone: that it repeats at that seed, that it is unaffected by what preceded it in the session, that `run_replications()` restores the caller's generator kind and stream position, and that a replication reproduces from its seed on either dispatch path; exits non-zero on failure |
| `scripts/check_scenario_labels.R` | Regression check asserting that the comparative scenario plot renders in a C locale and is byte-identical to the same plot rendered under UTF-8; exits non-zero on failure |
| `scripts/check_arrival_rate_fidelity.R` | Regression check asserting that the generated arrival streams reproduce their configured rates and between-day variance, so a change to the arrival process cannot silently move the casualty count; exits non-zero on failure |
| `scripts/check_replication_memory.R` | Regression check asserting that peak memory does not grow with the replication count: that `dispatch_replications()` asks `mclapply` for one fork per job unconditionally, and that peak resident memory across the process tree is flat between four jobs and sixteen. Asserted against a worker retaining a known amount in a process global, so it measures the dispatch arrangement rather than the model and runs in seconds; exits non-zero on failure |
| `scripts/check_replication_loss_reporting.R` | Regression check asserting that a replication lost to its host is reported rather than dropped silently from the published count: a clean dispatch is unchanged, any loss stops the run under the shipped threshold of zero, a caller who raises the threshold gets a warning and the survivors within it, a loss beyond a raised threshold still stops, and the Welch figure's caption names the realised count it was given; exits non-zero on failure |
| `scripts/check_absent_attribute_columns.R` | Regression check asserting that an attribute no casualty set is an empty column rather than an absent one: `MODEL_ATTRIBUTE_KEYS` lists every key the model sets and no key it no longer sets, the wide pivot returns all of them from a monitor carrying one, and a run in which nobody returned to duty analyses end to end with the R2B holding summary empty rather than a series of zeroes; exits non-zero on failure |
| `scripts/check_bed_queue_coverage.R` | Regression check asserting that each echelon's bed queue figure covers every bed type that echelon fields, that each panel label counts the beds in the pool it names, and that the panels share one vertical scale rather than each filling its own; the coverage assertion is made against a bed type no configuration ships, injected into the monitor, so it defends the selection rule rather than today's establishment; exits non-zero on failure |
| `scripts/check_censored_interval_estimation.R` | Regression check asserting that an interval still open when the observation window closes is carried as the lower bound it is rather than dropped: the estimator degenerates to the arithmetic mean where nothing is censored, it recovers a known restricted mean from censoring injected into real stays, every casualty who entered an interval is counted in its `n`, the censored count and share are reported, no part of either dwell mean rests on the flat tail beyond the last observed completion, and a quantile the curve does not reach is reported as absent rather than as the same quantile of the casualties who finished; exits non-zero on failure |
| `scripts/check_hold_episode_reconstruction.R` | Regression check asserting that an R2B holding episode is bounded by the attribute its own exit route sets: an episode ended by return to duty ends at `return_day`, one ended by onward evacuation at the bed time served rather than at the `return_day` its casualty sets later at another echelon, and one still running when the window closes at the window's end rather than being dropped. The reconstructed bed-days are checked against the resource monitor, which counts bed occupancy directly, at the shipped configuration and under an evacuation threshold; exits non-zero on failure |
| `scripts/check_ci_apt_guards.R` | Regression check asserting that every CI job installing system libraries does so behind the same stall guards: each of the four `Install system libraries` steps carries a per-step `timeout-minutes`, each sets apt's retry count and both transport timeouts before it runs `apt-get update`, and the four step bodies stay identical, so three jobs hardened and one missed fails the gate rather than waiting to be found by a wedged mirror; exits non-zero on failure |
| `scripts/check_time_series_figures.R` | Regression check asserting that the tracked campaign time series covers every resource pool, pathway stage and casualty intensity the model defines at the replication count and horizon the paper names, that its clearance statistics are self-consistent, that every queue-clearance percentage the results paper prints matches the tracked measurement, and that the step-function estimators the series rest on are correct on inputs whose answers are computable by hand, so a series agreeing with the paper is not two copies of one error; exits non-zero on failure |
| `scripts/check_long_horizon_protocol.R` | Regression check asserting that the sustained-operations horizon's parameters, its reduction and its published series agree: every protocol parameter in `R/long_horizon.R` equals the value `docs/Methods.md` documents, the tracked series carries that duration, replication count, response set and scenario set, the daily reduction reproduces what the resource and arrival monitors report over the same window, each day's reduction uses that day's events alone so reducing one run's monitors over a shorter horizon reproduces the shorter horizon's days exactly (which is what makes twelve block means from one run legitimate), a long run and a short one at one seed are nonetheless different campaigns, and `block_means()` averages over the block rather than sampling it. The reduction is asserted against the monitors because a long run discards them, so an error in it could not be found later by re-reading them; exits non-zero on failure |
| `scripts/check_long_horizon_warmup.R` | Regression check asserting that the sustained-horizon Welch cumulative-moving-average diagnostic is correct and that the published reading rests on it: `compute_long_horizon_cma()` is correct on a constructed series whose answer is computable by hand and excludes an unrequested pool or a non-queue series from the reduction, the tracked `data/long_horizon/long_horizon_welch_cma.csv` reproduces exactly from the tracked `data/long_horizon/long_horizon_series.csv.gz`, and every day-30/90/180/360 figure `docs/Methods.md`'s Welch Diagnostic at Length paragraph states matches the tracked CMA; exits non-zero on failure |
| `scripts/check_holding_occupancy_split.R` | Regression check asserting that R2E holding bed occupancy splits into the four stays that consume it and that they account for the pool exactly: the components sum to the measured occupancy at the shipped configuration, under a cancellation rate that forms a backlog and under one that leaves casualties staged at the close, and every replication of the tracked evidence set closes to the arithmetic tolerance rather than a stated residual. The evacuation component is bounded by the pool and rises when sorties are lost, it counts both airlift routes (asserted against a run carrying both), it agrees with a casualty-by-casualty recount taken independently of the function under test, and a wait still running when the window closes is charged to the window's end rather than dropped. Each stay is bounded by the attribute its own exit route sets, so a casualty who died awaiting a sortie is charged to its death rather than to the window's close; that is asserted directly, and against the superseded departure-bounded estimator, which is shown still to over-count by the bed-days such a casualty did not serve; exits non-zero on failure |
| `scripts/check_ame_backlog_exit.R` | Regression check asserting that the strategic evacuation backlog is bounded by the attribute its own exit route sets. `compute_ame_backlog()` reconstructs the queue for the two airlift pools, the resource monitor's own queue column being structurally zero for them, and removing a casualty on its boarding alone left one who died waiting in the queue for the rest of the campaign. Asserted on a constructed monitor whose answer is computable by hand, carrying one casualty per exit (boards, dies waiting, still waiting at the close), against the superseded reconstruction, which is shown to disagree by exactly the casualty that died and from the instant it did, and on a real campaign at a cancellation rate that makes the exit reachable; exits non-zero on failure |
| `scripts/check_airlift_collapse_protocol.R` | Regression check asserting that the strategic airlift collapse experiment's parameters, its classifier and its published table agree: every protocol parameter in `R/airlift.R` equals the value `docs/Methods.md` documents, the classifier averages each replication over the closing window's days alone at an inclusive threshold and reads its own pool out of the series the long-horizon runner returns, the interval is the exact binomial one rather than a normal approximation that would report a zero count as exactly known, the tracked summary matches the table `docs/Results.md` prints row for row, and reclassifying the tracked daily series reproduces every published response, so the table is auditable without re-running 180 replication-years. The classifier is asserted against a constructed series whose answers are computable by hand, one replication peaking before the window and one exceeding the threshold on a single day inside it, so a classifier reading the whole campaign or the window's worst day would disagree; exits non-zero on failure |
| `scripts/check_airlift_protocol.R` | Regression check asserting that the strategic evacuation experiment's parameters, its interval construction and its published tables agree: every protocol parameter in `R/airlift.R` equals the value `docs/Methods.md` documents in a marker comment, the tracked per-replication responses carry that replication count and the documented thirteen configurations, and every figure the three tables of `docs/Results.md`'s national support base section print matches the tracked summary. A missing table, row or column fails rather than passing quietly, which is what stops the comparison being vacuous, and `summarise_airlift()`'s mean and interval are asserted against an input whose answer is computable by hand, so a table agreeing with the summary is not two copies of one error. Written because this experiment was the one replicated experiment with no such check, and its section had drifted from its own evidence set undetected; exits non-zero on failure |
| `scripts/check_scenario_protocol.R` | Regression check asserting that the comparative scenario analysis's parameters, its responses and its published tables agree: every protocol parameter in `R/scenario_runner.R` equals the value `docs/Methods.md` documents in a marker comment, the tracked evidence set carries those profiles and that replication count, the tracked summary is the reduction of the tracked per-replication series beside it rather than a second and independent claim, and every figure the two tables of `docs/Results.md`'s comparative section print matches the tracked measurement. A missing table, row or column fails rather than passing quietly, and the pool-queue estimator and its interval are asserted against inputs whose answers are computable by hand, so a table agreeing with the summary is not two copies of one error. Written because the paper's centrepiece experiment had no tracked evidence set at all, and its queue table had drifted from the abstract quoting it; exits non-zero on failure |
| `scripts/check_capacity_sweep_protocol.R` | Regression check asserting that the transport fleet-size sweep and the forward ICU share frontier agree with the tables `docs/Results.md` prints from them: every protocol parameter in `R/analysis.R` equals the value `docs/Methods.md` documents, each tracked sweep carries the swept values and response columns its table prints and contains the shipped establishment so that the sweep says what departing from it costs, every published figure matches the tracked measurement, and each tracked interval is the Student t one at 95% about its own mean where it is not clamped to the range its quantity can take. The two sweeps share one check because they are one shape; the constants are read out of `R/analysis.R` rather than sourced, so the check runs in seconds and without simmer; exits non-zero on failure |
| `scripts/check_policy_sweep_protocol.R` | Regression check asserting that the evacuation policy sweep's parameters, its responses and its published table agree: every sweep parameter in `R/policy_sweep.R` equals the value `docs/Methods.md` documents, the swept range spans the doctrinal 15 to 60 day decision range and contains the shipped policy, the reduction's returns to duty, died of wounds, disposition count and in-theatre share agree with the analysis pipeline's on one run's monitors, the in-theatre share responds to the policy each arm ran under rather than to the configuration global, the closing-window estimator is correct on a constructed monitor whose answer is computable by hand, the paired difference is taken within replication and its replication sizing follows the supplement's approximation, and the tracked summary matches the table `docs/Results.md` prints; exits non-zero on failure |
| `scripts/check_structure_tables.R` | Regression check asserting that the two tables claiming to index the codebase actually do: every `R/` module has a row in the README's Codebase Structure table and in this file's Repository Structure table, every `scripts/check_*.R`, every other `scripts/` entry point and every tracked `docs/` document has a row here, and neither table names a path that no longer exists. The file set comes from `git ls-files`, so an untracked scratch file cannot fail it; exits non-zero on failure |
| `scripts/check_testthat.R` | Regression check running the Shiny console's `testthat` suite under `tests/testthat`: unit coverage of the console's helpers, and `shiny::testServer()` coverage of its reactive state machine; exits non-zero on any failing test |
| `tests/testthat/` | The console's R test suite, discovered and run by `scripts/check_testthat.R` and therefore gated per PR |
| `tests/playwright/`, `playwright.config.js`, `package.json`, `package-lock.json` | The console's browser test suite and the Node toolchain it needs, kept out of `renv.lock` so that no browser automation package enters the R dependency set. Run with `npx playwright test`; it starts the app itself and uses whatever Chromium `PLAYWRIGHT_BROWSERS_PATH` provides |
| `scripts/run_all_checks.R` | Regression check suite runner — discovers every `scripts/check_*.R` by glob, reports a pass/fail line and a runtime for each, and exits non-zero if any fails; `--fast` omits the checks too slow for a per-PR gate, `--slow` runs those alone, `--list` prints the classification, and `--jobs <n>`/`--jobs auto` runs that many checks at once, longest first, dividing the machine's cores between them |
| `scripts/check_runtimes.csv` | Measured runtime per check, which the runner dispatches longest-first from under `--jobs`. A scheduling hint alone: a stale entry costs wall clock and cannot change a result. `--refresh-runtimes` is the only way to write it |
| `scripts/check_lint.R` | Regression check asserting that no `lintr` rule in `.lintr`, and neither of the two machine-checkable rules `lintr` has no linter for (function length, pictographic characters in source), reports more findings than the count tracked in `scripts/lint_baseline.csv`; exits non-zero on a rise. `--refresh-baseline` is the only way to write the tracked counts |
| `scripts/check_baseline_reproduction.R` | Regression check asserting that the tracked seed-42 evidence set reproduces byte for byte, running the model at seed 42 for 360 days into a temporary directory and comparing `logs/logs.txt` and every `data/arrivals_*.txt`, `data/mass_casualty_events.csv` and `data/seed42_verification.csv` against it; exits non-zero on any difference |
| `scripts/check_roxygen.R` | Regression check asserting that no documentation rule of `docs/STYLE_GUIDE.md` that a parser can decide reports more findings than the count tracked in `scripts/roxygen_baseline.csv`: every named function carries a roxygen header opening with a title, an `@param` for each argument and no more, and a `@return` (R1, R2), and every file-scope constant carries a header (R3). Exits non-zero on a rise; `--list` prints every finding with its file, line and function name, and `--refresh-baseline` is the only way to write the tracked counts |
| `scripts/roxygen_baseline.csv` | The per-rule finding counts the roxygen ratchet defends |
| `.lintr`, `scripts/lint_baseline.csv` | The lint configuration encoding the `[lint]`-tagged rules of `docs/STYLE_GUIDE.md`, and the per-rule finding counts the ratchet defends |
| `.github/` | Pull request template mirroring the test plan structure below, and the GitHub Actions workflow running the fast suite, the lint ratchet, the seed-42 reproduction and the console's browser suite on every PR against `main`, in the pinned container |
| `scripts/check_pre_open_window.R` | Regression check asserting that a zero R2B pre-open hold window reproduces the instant-diversion model bit-for-bit, that `minutes_to_shift_open()` agrees with the roster, and that every casualty held forward is operated on there; exits non-zero on failure |
| `scripts/check_hold_window_protocol.R` | Regression check asserting that the R2B pre-open hold window's parameters, its responses and its published table agree: every protocol parameter in `R/hold_window.R` equals the value `docs/Methods.md` documents, the tracked evidence set carries both documented arms at the documented replication count and the response set the table prints, the tracked summary and paired differences are each the reduction of the tracked per-replication responses beside them, every figure the paper's eight-row table prints matches the tracked measurement, and `summarise_hold_window()` and `hold_window_paired_difference()` are correct on inputs whose answers are computable by hand; exits non-zero on failure |
| `README.md` | System reference — introduction, literature review, methodology, codebase structure, trajectory logic, resource model, Mermaid diagrams, inline model assumptions, limitations, references. Does not contain simulation results. |
| `docs/Results.md` | Every measurement of every replicated experiment, and the verification of one 360-day seed-42 campaign in Annex A, with no analysis, interpretation or recommendation. Every table and every quoted figure is a generated span rebuilt from the tracked evidence under `data/` by `R/results.R` and `scripts/render_results_tables.R`, so a figure cannot differ from the evidence it reports; `scripts/check_results_tables.R` asserts that |
| `docs/Planning_Implications.md` | Every lever the model can evaluate read as a planning option: a diagnosis of where the system fails first, one lever table with an evidence label per lever, recommendations in priority order, the effects left unresolved (a generated table of paired differences and the replications each needs) and the research agenda. States no measurement of its own: every figure is a generated span quoting a cell of `docs/Results.md`, regenerated by `scripts/render_results_tables.R` and asserted by `scripts/check_planning_implications.R`, and every paragraph carrying one links the results section it comes from. Holds no experimental design or provenance |
| `docs/Methods.md` | A standalone academic paper companion to `docs/Planning_Implications.md` and `docs/Results.md`, written to the same standard and carrying its own abstract, introduction, limitations, conclusion and independently-numbered reference list: the full experimental design of every replicated experiment, the replication-independence argument, the withdrawn antithetic pairing and its measurement, the interval construction and replication-count derivation, the warm-up classification, the checks that defend each of those properties, and the provenance of the comparative figures. Holds no finding about the performance of the trauma system; every measured result of that kind belongs to the results paper. The one exception is the reinforcement comparison, which measures force generation rather than health system performance and is reported here in full. This paper is written to a ~5,000-word journal length and cites this document for the detail it no longer carries |
| `docs/BCH_Simulation_Action_Plan.md` | Issue tracker cross-reference — phase sequencing, dependency graph, merged-issue log |
| `docs/BCH_Task_Role_Allocation.md` | Task-role allocation design supplement for the not-yet-implemented individual resource modelling work (Issue #4) |
| `docs/Treatment_Flow_Configurability.md` | Design supplement evaluating how far the `env_data.json` parameter mechanism can be extended to let a user adapt the treatment flow itself, rather than only its magnitudes; sets out four options, their costs and their sequencing. Reports no simulation result and describes nothing yet implemented |
| `docs/Continuous_Integration.md` | Operating guide for the automated verification: what each GitHub Actions job runs and when, how to read a result, how to dispatch the slow suite, what each way the gate can fail calls for, and how a new check joins the suite |
| `docs/Getting_Started.md` | User guide for the Shiny console — the Configure/Run/Analyse workflow and how to read each output |
| `docs/archive/Project_Status_Review.md` | Repository-wide status review, a frozen snapshot kept for the record — the findings and remediation plan the Phase 6 code-quality issues derive from. Not maintained; the action plan is the tracker |
| `docs/archive/` | Frozen documents kept for the record and not maintained; the action plan is the tracker. The link, target and alt-text checks of `scripts/check_markdown.R` still cover them |
| `docs/STYLE_GUIDE.md` | The R code standard — every rule a reviewer checks a PR against, each tagged machine-checkable, reviewer-applied or preference; follow at all times |
| `data/` | Read-only input data (arrival schedules) plus the tracked seed-42 diagnostic, event and verification files (`arrivals_*.txt`, `mass_casualty_events.csv`, `seed42_verification.csv`) written by `R/environment.R` and `run.R`, rewritten only under `run.R --refresh-baseline` |
| `data/sensitivity/` | Tracked sensitivity evidence set — the Morris r=20 and Sobol N=800 design point caches, the per-response rankings, the decompositions, the noise floor measurement, and the estimator and separation re-analyses; the Morris cache alone is about fourteen hours of computation, kept because every published index and rank derives from it and it cannot be regenerated cheaply |
| `data/airlift/` | Tracked strategic evacuation evidence set — the per-replication responses and their summary behind the national support base section of `docs/Results.md`, at 30 replications over 360 days per arm across two baselines and two sweeps, plus the collapse classification behind the strategic airlift reliability sweep at 30 replications over 360 days per arm and the classified pool's daily series, which is retained so that a statement about how a campaign's opening relates to its outcome stays recomputable. Written by `scripts/run_airlift_sweep.R --refresh-baseline` and `scripts/run_airlift_collapse.R --refresh-baseline` alone, each owning its own files |
| `data/scenarios/` | Tracked comparative scenario evidence set — the casualty totals, the per-resource queue summaries, and the per-pool queue comparison both per replication and reduced, behind the centrepiece tables of `docs/Results.md`, at 30 replications over 360 days for each of the two casualty intensities, queues measured over the closing 90 days. The per-replication pool series is kept because a pool's interval cannot be recovered from per-bed summaries, the pool total being in none of the monitor's rows. Written by `scripts/run_scenarios.R --refresh-baseline` alone |
| `data/sweeps/` | Tracked capacity sweep evidence set — the per-point means and intervals behind the transport fleet-size table, the forward ICU share frontier table and the R2B holding capacity/evacuation threshold table of `docs/Results.md`, all three now at 30 replications per point over 360 days, the sustained-operations protocol every one of them migrated to under Issue #405 (transport from 10 over 30 days, the other two from their own counts over 30 days, migrated for cross-table consistency rather than an under-resolution finding of their own). Written by `scripts/run_transport_sweep.R --refresh-baseline`, `scripts/run_icu_share_sweep.R --refresh-baseline` and `scripts/run_hold_threshold_sweep.R --refresh-baseline` alone, each owning its own file; the transport sweep's `--scenario high_intensity` re-run writes its own `_high_intensity`-suffixed copy alongside the shipped configuration's |
| `data/long_horizon/` | Tracked sustained-operations evidence set — the reduced daily series and the per-block means behind every long-horizon statement in `docs/Methods.md`, at 30 replications over 360 days per casualty intensity, plus the Welch cumulative-moving-average reduction of that series behind the sustained-horizon warm-up diagnostic. The series and block means are written by `scripts/run_long_horizon.R --refresh-baseline`; the CMA is written separately by `scripts/render_long_horizon_warmup.R --refresh-baseline`, which reads the series rather than re-running the model |
| `data/policy/` | Tracked evacuation policy and holding establishment evidence set — the per-replication responses, their summary and the paired differences against the shipped policy behind the policy sweep of `docs/Results.md`, at 30 replications over 360 days for each of five policies, plus the per-arm checkpoints each was resumed from and the `establishment_sweep*` files of the holding establishment measurement. Written by `scripts/run_policy_sweep.R --refresh-baseline` alone |
| `data/hold_window/` | Tracked R2B pre-open hold window evidence set — both arms' per-replication responses, their summary and the paired differences behind the eight-row table of `docs/Results.md`'s hold window section, at 30 replications over 360 days per arm, the sustained-operations protocol. Written by `scripts/run_hold_window.R --refresh-baseline` alone |
| `data/icu_gate/` | Tracked post-operative intensive care gate evidence set — both arms' per-replication responses, their summary and the paired differences behind [The Post-Operative Intensive Care Gate](docs/Results.md#post-operative-intensive-care-gate), at 30 replications over 360 days per arm, the sustained-operations protocol. Written by `scripts/run_icu_gate.R --refresh-baseline` alone |
| `data/mass_casualty/` | Tracked mass casualty event stress test evidence set — both arms' per-replication responses, the total-casualty and event-count summary and the pooled died-of-wounds rate by casualty origin behind [Mass Casualty Events Degrade Care Without Revealing New Constraints](docs/Results.md#mass-casualty-events), at 30 replications over 360 days per arm, plus the illustrative single run behind `images/mass_casualty_events.png`. Written by `scripts/run_mass_casualty.R --refresh-baseline` alone |
| `data/time_series/` | Tracked campaign time series evidence set — the per-replication queue series, queue clearance statistics and degraded post-operative care rates behind the two time series figures of `docs/Results.md`, at 30 replications over 360 days per casualty intensity. Kept rather than regenerated per render so that a figure and the percentages the paper quotes from it derive from one measurement; written by `scripts/render_time_series_figures.R --run --refresh-baseline` alone |
| `data/calibration/` | Tracked treated-cohort died-of-wounds measurement at the sustained horizon, the pooled rate and interval of each configuration against its historical anchor, behind the mortality section of `docs/Results.md`. Written by `scripts/check_dow_calibration.R --days 360 --reps 10 --write-csv data/calibration/dow_calibration.csv` alone |
| `images/` | Tracked seed-42 baseline plots and reference diagrams, regenerated as part of baseline-affecting PRs via `run.R --refresh-baseline` |
| `logs/` | Tracked seed-42 baseline console log (`logs.txt`), regenerated as part of baseline-affecting PRs via `run.R --refresh-baseline` |
| `outputs/` | Gitignored destination for every ordinary run's artifacts: CSV/markdown outputs, plots (`outputs/images/`), console log, and arrival diagnostics (`outputs/data/`); tracked via `.gitkeep` only |
| `renv/`, `renv.lock`, `.Rprofile` | R package environment management |
| `.devcontainer/` | Pinned Dev Container definition (`rocker/rstudio:4.4.2`) used for canonical baseline runs |

---

## Development Workflow

### Branch Rules

- **All development happens on feature branches.** Never commit directly to `main`.
- **Only the repository owner can merge to `main`.** Do not merge to `main` directly. Always open a PR and await owner merge.
- **Always open a PR at the end of each issue.** Use the GitHub MCP tools (`mcp__github__create_pull_request`) to create the PR with a test plan in the description before handing over. Never ask the user to merge via git commands — they merge through GitHub.
- Branch naming: `feature/issue-<number>-<short-description>` (e.g., `feature/issue-1-multi-run-replication`).
- Each GitHub Issue corresponds to one feature branch and one PR.

### Sequence

1. Raise a GitHub Issue describing the work (see Issue Format below).
2. Create a feature branch from `main`.
3. Implement the changes.
4. Update the relevant document(s) — `README.md` and/or the `docs/` analysis documents — as part of the same PR (see Document Maintenance below).
5. Open a PR against `main` with a test plan (see Test Plans below).
6. Await owner merge — do not self-merge.

### Post-Merge Checklist

After the repository owner merges a PR to `main`, perform the following tasks on a new chore branch (`chore/post-pr<N>-action-plan-update`) and open a follow-up PR:

**1. Update `docs/BCH_Simulation_Action_Plan.md`**

| Location in document | What to do |
|---|---|
| Summary table | Change the issue's Status from `Open` → `**Merged (PR #N)**` |
| "Issues In Review" section | Remove the merged issue's entry; if the section is now empty, restore the placeholder: `*No PRs currently open against main.*` |
| "Recently Merged Issues" section | Add a new entry (see format below) above the previous most-recent entry |
| Phase sequence list | Strike through the item with `~~double tildes~~`. An issue raised after its phase's list was written has no item to strike, so add one at its position in merge order, numbered with a letter suffix on the item it follows (`6a`, `15b`); re-letter the items after it if merge order requires. Add the issue to the roster in the phase heading at the same time |
| Dependency graph | Move the issue node from UNBLOCKED to COMPLETE; move any newly unblocked issues from BLOCKED to UNBLOCKED |
| Footer | Update the "last updated" date |

Recently Merged Issues entry format:
```
### Issue N — <Title> ✓

**Merged:** PR #N, branch `<branch-name>`

<One paragraph describing what was implemented and how it works.>

**Seed-42 baseline (360 days, single run):** <Include a table of changed metrics if the merge altered simulation outputs. Omit this block for documentation-only changes.>

**Unblocked by this merge:** <List newly unblocked issues, or "No new issues unblocked.">
```

**2. Update GitHub issue labels**

For each issue newly unblocked by the merge: change its label from `status: blocked` to `status: ready` using the GitHub MCP tools.

**3. Refresh the seed-42 baseline (if simulation outputs changed)**

If the merged PR modified `R/trajectories.R`, `R/environment.R`, or `env_data.json` in a way that shifts the RNG stream or alters stochastic outputs, re-run the simulation at seed 42, regenerate `docs/Results.md` Annex A and `docs/Planning_Implications.md` with `scripts/render_results_tables.R --refresh-baseline`, and document the change, with the superseded values, in the action plan entry. `CLAUDE.md` carries no measured seed-42 figure to update.

The re-run must be invoked with the `--refresh-baseline` flag, which is the only way to write the tracked baseline evidence set (`images/`, `logs/logs.txt`, `data/arrivals_*.txt`, `data/mass_casualty_events.csv`, `data/seed42_verification.csv`):

```sh
Rscript run.R --seed 42 --days 360 --iterations 1 --refresh-baseline
```

Without the flag, every run writes to `outputs/` alone and leaves all tracked artifacts untouched, so an exploratory or smoke-test run cannot corrupt the baseline. The flag requires `--iterations 1` and errors otherwise, because the console log and the arrival diagnostics have no multi-replication equivalent; this is what guarantees the tracked sets always describe the same single run. Commit them together, as one commit, or not at all: a PR that regenerates only part of the set reintroduces the drift Issue #154 closed.

**4. Regenerate the README environment summary (if `env_data.json` changed)**

If the merged PR modified `env_data.json`, run `scripts/check_env_data_summary.R` to refresh the `<!-- ENV SUMMARY START/END -->` block inside `README.md` and include the updated `README.md` in the chore PR.

---

### Commit Messages

Commits should be clear and descriptive. Reference the issue number:

```
feat(issue-1): activate mclapply replication wrapper with wrap() aggregation

Replaces single-run execution with 1000-replication parallel framework.
All KPI outputs now report mean ± 95% CI across replications.

Closes #1
```

---

## Issue Format

Use the following hybrid format when raising GitHub Issues. It captures both the academic rationale and the engineering task list.

```markdown
## Problem Statement

<Describe what is wrong or missing in the current model. Include the clinical or operational consequence
of the gap — not just the code symptom. Cite literature where the basis for the problem is established.>

## Operational / Clinical Rationale

<Explain why this matters for health outcomes or planner decision-making. Reference doctrine,
historical data, or published evidence. Prioritise open-access sources.>

## Recommended Approach

<Describe the implementation approach at a conceptual level. Reference the method or algorithm chosen
and its basis in literature. Include any key design decisions.>

## Implementation Tasks

- [ ] Task 1
- [ ] Task 2
- [ ] ...

## Acceptance Criteria

- [ ] Criterion 1 (observable output change)
- [ ] Criterion 2
- [ ] ...

## References

- Author (Year). Title. Source. URL
```

---

## Issue Annotation System

All GitHub Issues use a consistent annotation system to make phase, type, and sequencing visible in the issue list without opening each issue.

### Title prefix format

Every issue title opens with a prefix in square brackets:

```
[Ph.N] Title of issue
[Ph.N · BUG] Title of bug issue
[HOTFIX · Ph.N] Title of pre-phase bug fix
```

| Prefix | When to use |
|---|---|
| `[Ph.1]` through `[Ph.5]` | Standard feature or analysis work in the named phase |
| `[Ph.N · BUG]` | A bug found within a phase that can wait for that phase |
| `[HOTFIX · Ph.N]` | A bug that must ship before its phase begins — no dependencies |

Do not include `READY` or `BLOCKED` in the title; those are maintained as labels (see below).

### Labels

All labels are applied on the repository. Use them as follows when raising new issues:

**Phase labels** — one per issue, matching the title prefix:
`phase/1 · statistical-foundation`, `phase/2 · model-fidelity`, `phase/3 · structural-refactor`, `phase/4 · scenario-expansion`, `phase/5 · interface`

**Type labels** — one per issue:
`bug` (defects in existing behaviour), `enhancement` (new capability or improvement)

**Status labels** — maintained as work progresses; update when dependencies are resolved:
`status: ready` (no blocking dependencies), `status: blocked` (has unresolved dependencies)

**Priority labels** — apply when the issue warrants it:
`priority: critical` (bug that invalidates current output), `priority: high` (blocks multiple other issues)

### Raising new issues

When a new issue is raised:
1. Assign the correct `[Ph.N]` prefix to the title.
2. Apply phase, type, status, and priority labels.
3. Set `status: ready` if it can be started immediately; `status: blocked` if it depends on open issues.
4. When a blocking issue merges, update the `status` label on all issues it unblocks.

---

## Test Plans

Every PR must include a **Documented Manual Test Plan** in the PR description, following the structure `.github/pull_request_template.md` prompts for.

Verification has two halves. The `scripts/check_*.R` regression checks are automated and gated: `Rscript scripts/run_all_checks.R --fast` runs every check a PR is gated on, and GitHub Actions runs the same suite, the lint ratchet, the seed-42 byte-for-byte reproduction and the Shiny console's browser suite on every PR against `main` in the pinned container (`.github/workflows/checks.yml`). The console's own coverage is split by what each half can see: `tests/testthat` drives the reactive state machine through `shiny::testServer()` and runs inside the fast suite, and `tests/playwright` drives a running app in headless Chromium and runs as its own job. A PR is not ready for review while that workflow is red; `docs/Continuous_Integration.md` is the operating guide for reading and acting on a result. Everything the checks do not assert, which is most of what a change to the model does, is verified by documented manual execution, which is what the test plan records. A behaviour worth protecting past the PR that introduces it belongs in a new `scripts/check_*.R`, which the runner discovers by glob and therefore gates from the moment it is committed.

Test plans must include:

1. **Setup** — seed, run duration, any parameter changes required to observe the behaviour under test.
2. **Steps** — numbered list of actions to execute.
3. **Expected outputs** — specific, observable values or patterns (e.g., "mean R2E ICU queue across replications should be non-zero and vary between replications").
4. **Regression checks** — confirm that outputs from unmodified pathways remain consistent with the seed-42 baseline: the measured figures in `docs/Results.md` Annex A, which are generated from the tracked `data/seed42_verification.csv`, and the byte-for-byte reproduction of the tracked set that `scripts/check_baseline_reproduction.R` asserts.
5. **Known limitations** — anything the test plan does not cover, and why.

Example entry:

```
### Test: Multi-replication output (Issue 1)
**Setup:** n_iterations = 10, n_days = 30, seed = NULL (independent per replication)
**Steps:**
1. Source `run.R`
2. Inspect `queue_summary` output object
3. Confirm 10 rows present in replication-level resource monitor output
**Expected:** `mean_queue` values differ across replications; p10 < mean < p90 for at least one resource
**Regression:** Total casualty count per replication should fall within ±15% of the seed-42 total in `docs/Results.md` Annex A
```

---

## Document Maintenance

The project's academic output is split across four documents, each kept current with the code and written to the standard of a published academic paper (see [Academic Standards](#academic-standards) and the Repository Structure table above):

- **`README.md`** (system reference) — code structure, algorithms, trajectory logic, resource model, Mermaid diagrams, inline model assumptions, the rationale for each default, how to install and run the model, and Further Development. Contains no measured result: where a default's rationale rests on a measurement, it states the rationale and cites the section of `docs/Results.md`.
- **`docs/Results.md`** — every measurement of the replicated experiments and the seed-42, 360-day verification of one campaign (Annex A), reported without interpretation. Its tables and quoted figures are generated spans, regenerated with `scripts/render_results_tables.R --refresh-baseline`; no recommendation and no hand-typed figure belongs in it.
- **`docs/Planning_Implications.md`** — the measurements read as options for a planner: where the system fails first, one lever table with an evidence label per lever, recommendations in priority order, the effects left unresolved and the research agenda. It states no measurement of its own: every figure is a generated span quoting a cell of `docs/Results.md`, and every paragraph carrying one links the results section it comes from.
- **`docs/Methods.md`** — the standalone companion paper to the other two: the full design of every replicated experiment, the replication framework and interval derivations they rest on, and the reinforcement comparison, which measures force generation rather than health system performance. No finding about the trauma system's performance belongs in it.

The boundary between the documents is what each states. What was measured goes in the results paper, what it means for a planner goes in the planning paper, how it was measured goes in the methods paper, and how the system works and why it is configured as it is goes in the README. A figure is stated once, in the results paper, and cited from the others. Cross-references between the documents (`[text](../README.md#anchor)`, `[text](docs/Results.md#anchor)`, `[text](docs/Planning_Implications.md#anchor)`, `[text](docs/Methods.md#anchor)`, as appropriate to the source document's location) must stay valid: re-run `scripts/check_markdown.R` after moving or renaming any heading referenced from another document.

All four are updated **as part of every PR that changes what they state**, not retrospectively. The generated documents regenerate with one command, so a measurement changing in a PR changes the figures in `docs/Results.md` and `docs/Planning_Implications.md` together.

### What to update per PR

| The PR changes | `README.md` | `docs/Results.md` | `docs/Planning_Implications.md` | `docs/Methods.md` |
|---|---|---|---|---|
| Trajectory logic, resource logic or a distribution | Simulation Design, the Mermaid diagram for the echelon, Further Development | Regenerate (a measurement may move) | Only if a lever's evidence label changes | Only if an experiment's design changes |
| A shipped default in `env_data.json` | The parameter and the rationale for it; run `scripts/check_env_data_summary.R` | Regenerate | Only if a lever's evidence label changes | No |
| An evidence set is re-run, at any replication count | No | Regenerate with `scripts/render_results_tables.R --refresh-baseline`; add prose only where a new experiment needs its own section | Regenerates on its own; revisit the recommendations the new measurement bears on | The experiment's replication count or protocol line, if changed |
| A new lever or experiment | Where the lever is configured | A new section and, where it has a table, a builder in `R/results.R` | A row in the lever table, with its evidence label | The experiment's design |
| An experiment's design, seed, parameter overrides or invocation | The CLI summary, if it changed | No | No | The experiment's design section |
| The replication framework, interval method or warm-up classification | The one-paragraph summary and its link | No | No | The Replication Framework section |
| The seed-42 baseline (`--refresh-baseline`) | No | Annex A regenerates | Regenerates | No |
| A gap is closed or newly identified | Further Development: delete the entry or add one with a new identifier | No | The research agenda, if it names the gap | No |
| A new source is cited | References | References | References | References |
| A new `R/` module or `scripts/` entry point or check | The Codebase Structure table (`R/` modules) | No | No | The checks paragraph, if the check defends a documented property; also this file's Repository Structure table |

Each document's References section lists only the sources that document itself cites, numbered in order of first appearance within that document, not a shared numbering scheme across all four. A source cited in more than one document is renumbered independently in each.

### Style

- Write in academic third-person prose. Avoid first person.
- **Write at a post-graduate research level that stays accessible to non-experts.** Use clear, plain prose and only standard dictionary words; do not coin non-standard terms (e.g. write "has not undergone surgery," not "unsurgicated").
- **Refer to people in the model as casualties, not "candidates."** "Candidate" is reserved for its other established uses in this project (a screened parameter, a scheduled day, a proposed intervention); a casualty being assessed or eligible for surgery is a "casualty requiring surgery" or "Priority N casualty," never a "surgical candidate" or "Priority N candidate."
- All parameters, probabilities, and distributions must be cited to their source.
- New methods introduced must reference the algorithm or statistical technique by name, with citation (e.g., "Morris Elementary Effects screening (Morris, 1991) was applied using R's `sensitivity` package").
- Tables and flowcharts must be kept synchronised with the code.
- **A figure in `docs/Results.md` or `docs/Planning_Implications.md` is a generated span, never typed.** A table or a quoted figure is a `GEN` span rebuilt from the tracked evidence by `scripts/render_results_tables.R`; a sentence quotes a cell of a table through a cell reference rather than copying it. `scripts/check_results_tables.R` and `scripts/check_planning_implications.R` fail on a figure typed outside a span.
- **Do not use em dashes** in new or edited prose across `README.md`, `docs/Results.md`, `docs/Planning_Implications.md`, and `docs/Methods.md`. Use commas, parentheses, or semicolons instead.
- **Simulation Design narrative sections describe only the current design.** Trajectory logic, algorithm, and resource-model sections state how the model works now, with supporting evidence (citations, code function names, computed figures), not how it used to work or which issue changed it (e.g. no "prior to Issue #N..." or "as of Issue #N..." framing, and no issue-number suffix on section/heading titles). This does not apply to the Limitations section or `docs/BCH_Simulation_Action_Plan.md`, which are required elsewhere in this document to track which issue addressed or introduced a given item.
- **Mathematical notation** uses LaTeX delimiters exclusively (`$...$` inline, `$$...$$` for display formulas), never a code fence or plain text, for a formula or a mathematical variable (e.g. `$p_{max}$`, not `p_max` or *p_max*). An actual code, attribute, or `env_data.json` identifier (e.g. `` `dow_ceiling` ``, `` `p1_p_max` ``) is set in backticks, not math notation, even where its name coincides with a formula's symbol.
- **Figure captions** are written as ordinary prose immediately following the image, not as a separate italicised "*Figure: ...*" note.
- **Avoid duplicating content** already documented elsewhere in the same document, or, per the cross-reference rule above, in one of the other documents; cross-reference the existing location instead of restating it. Every fact has exactly one home. The common failure is stating the same fact in two sections of the same document because both are about the thing it describes (for example a resource's concurrency limit appearing in both the roster section and the trajectory section). Put a fact where a reader would look for it first, and cross-reference from the other place.
- **Match the length of what surrounds the edit.** A new paragraph should be about as long as its neighbours in the same section; a new table row about as long as the other rows. Adding a paragraph that is twice the length of every other paragraph around it makes the document harder to read even when every sentence in it is accurate, and is a reliable sign that it is explaining something twice or explaining something the code already states. Check the actual lengths rather than trusting the impression while writing.
- **Explain the model, not the implementation.** Narrative sections state what the model does and what follows from it. Reasons that only a maintainer needs (why a seizure order avoids deadlock, why a closure forces its arguments) belong in the code comment, not the document.

### Mermaid Diagram Maintenance

The README contains Mermaid flowcharts representing the R1, R2B, and R2E trajectory logic. These diagrams are part of the academic document and must be kept accurate.

**When any of the following change, update the corresponding diagram in the same PR:**

| Change type | Diagram(s) to update |
|---|---|
| New branch added to a trajectory | The diagram for that echelon |
| Resource seizure/release order changed | The diagram for that echelon |
| DOW check probability or logic changed | All diagrams that include a DOW node |
| New resource type introduced (e.g., ICU, hold bed) | The diagram for that echelon |
| Casualty routing logic changed (R2B bypass, R2E direct, etc.) | R1 and/or R2B diagram as appropriate |
| Surgery, ICU, or recovery phase added or removed | The diagram for that echelon |

**Diagram accuracy rules:**
- Every node in the diagram must correspond to an actual step in the trajectory code. Do not include aspirational steps that are not yet implemented.
- Every major branch in `branch()` calls must appear in the diagram. Probability labels (e.g., "~1%", "~5%") are encouraged on edges where the code uses a fixed threshold.
- Resource names shown in nodes (e.g., "Seize OT & Surg Team") must reflect what is actually seized in the code — not what is semantically intended.
- When a trajectory function is restructured, re-read the code from top to bottom and redraw the diagram from scratch rather than patching individual nodes.

---

## Assumption Handling

The model contains assumptions at two levels:

### Inline — throughout `README.md`

Where a specific parameter, role allocation, or pathway decision rests on an assumption rather than validated evidence, document it inline in `README.md` (the system reference document; model assumptions are not split into the analysis documents) as flowing narrative prose woven into the surrounding paragraph, not as a standalone blockquote block. The prose must still cover what the previous blockquote format's four fields captured (the assumption itself, its basis, being source or reasoning, or an explicit "informed estimate" disclosure per Source Prioritisation level 5 if no source exists, and the consequence if it is wrong), but without a labelled "Uncertainty: High/Medium/Low" line; where uncertainty needs stating explicitly, say so in the sentence itself (e.g. "no open-access source confirms this, so uncertainty is high").

Example (folded into prose, not a blockquote):
Nursing Officers from the R2B emergency section are assumed to flex to scrub and circulating roles during surgery when not occupied with concurrent resuscitation, derived from ADF austere deployment practice; no open-access doctrinal source explicitly confirms this for forward R2B contexts. Were this assumption wrong, R2B surgical capacity would require dedicated surgical NOs not present in the current establishment, and surgical throughput would be zero whenever emergency NOs are occupied.

### Holistic — Limitations section

`README.md`'s `Further Development` section provides a consolidated review of all model assumptions, organised by impact. It should cross-reference the inline assumptions. Update this section whenever an assumption is added, resolved, or reclassified.

---

## Academic Standards

### Citations

- All parameters must be cited. If a value is estimated or derived, state this explicitly and describe the derivation.
- **All sources must be openly accessible on the internet without a paywall.** Paywalled journal articles, restricted doctrine, and books with no freely available full text must not be used.
- Use the numbered reference format already established in these documents (`[[n]](#references)`).
- New references are appended to the References section of the document that cites them, in the order they first appear in that document's text. Each of `README.md`, `docs/Results.md`, `docs/Planning_Implications.md`, and `docs/Methods.md` maintains its own independently-numbered References section (see Document Maintenance above) — a source cited in more than one document gets its own number in each.

### Reference List Rules

These rules apply to every entry in the References section of `README.md`, `docs/Results.md`, `docs/Planning_Implications.md`, and `docs/Methods.md`, and to references listed in GitHub Issues:

- **No annotations, notes, or comments.** Each reference entry contains only the bibliographic citation and URL. Do not append `—` followed by any explanatory text, relevance notes, or context.
- **Open access only.** Every source must be freely accessible via its URL without login, institutional access, or payment. Acceptable sources include: government and military publications on official sites, open-access journals (DOAJ, PubMed Central full text, Frontiers, MDPI, etc.), DTIC/arXiv/institutional repositories with direct PDF links, and free reference/educational websites. Unacceptable: paywalled journal articles (even with a direct PDF URL if the journal is not open access), books or textbook chapters, ADF/NATO restricted doctrine with no public URL.
- **Every entry must have a URL.** Cite the specific page or document URL, not just a journal homepage. Include a retrieval date.
- **Verify accessibility before citing.** If uncertain whether a source is freely available, do not cite it — find an open-access equivalent instead.

### Source Prioritisation

When selecting methods or parameter values, prefer sources in this order:
1. Open-access military doctrine (publicly available AJP, FM, ATP; ADF publications on defence.gov.au)
2. Peer-reviewed open-access research (DOAJ-indexed, PMC full text, Frontiers, MDPI, arXiv, DTIC)
3. Open-access grey literature / technical reports (DTIC, institutional repositories) — cite with access date
4. Government or intergovernmental publications (UN, WHO, national defence departments) on official public sites
5. Informed estimation — must be explicitly flagged as such with derivation documented

**Do not use:** paywalled journal articles, Springer/Elsevier/Oxford subscription content, textbooks, or any source requiring login or payment.

### Further Development Section

The README must maintain a single `Further Development` section, combining what was previously split between Limitations and Further Development, that:
- Identifies what the model does not represent and why
- Rates the impact of each gap on findings (High / Medium / Low), stated once, in the group heading
- States, for each gap, what would close it
- Opens with a scan table of identifier, one-line gap, and impact

Entry rules:
- Each entry carries a stable `L<n>` identifier, cited from the analysis documents and the action plan. **Identifiers are never reused or renumbered**, since renumbering silently redirects every existing citation.
- **A closed gap is deleted, not marked resolved.** The section describes the model's current gaps only; resolution history belongs to `docs/BCH_Simulation_Action_Plan.md`. When deleting an entry, search all four documents for citations of its identifier and repair them in the same PR.
- Do not cite issue numbers here. This section is not exempt from the issue-reference rule; the action plan is the tracker.
- Group entries under `### High Impact`, `### Medium Impact` and `### Low Impact`, in that order, numerically within each group. **A grouped list must be re-checked against its headings after any reordering.**
- The scan table at the head of the section is derived from the entries, so each row must repeat its entry's title and the impact group the entry sits under, exactly. `scripts/check_markdown.R` asserts this; reconcile the table to the entries, not the reverse.

---

## Implementation Phases

Development follows the sequencing below. Do not skip ahead — later phases depend on earlier foundations. The ordering within each phase reflects dependency constraints, not just grouping.

### Hotfix — Pre-phase (Issue 8)
Issue 8 (R2E surgical team seizure bug) is labelled `[HOTFIX]` and ships before any phase work begins. It is a three-line code change with no dependencies, and its absence corrupts all R2E surgical output. It runs in parallel with Phase 1 preparation.

### Phase 1 — Statistical Foundation (Issues 1, 2, 3)
Multi-run replication (#1) → Welch warm-up analysis (#2) and Morris sensitivity screening (#3, parallel with #2).
*All subsequent results must use the Phase 1 replication framework. Nothing in Phase 2 onward produces trustworthy output until #1 is merged.*

### Phase 2 — Model Fidelity (Issues 5, 6)
Time-dependent DOW (#5) and dead-heading transport (#6). Issues #5 and #6 are independent of each other and can be developed in parallel once Phase 1 is complete.

### Phase 3 — Structural Refactoring (Issues 4, 7)
DNBI sub-categorisation (#7) and individual resource modelling (#4). Issue #7 can be pulled forward alongside Phase 2 if bandwidth allows — its only hard dependencies are #1 and #2, not #3 or #4. Issue #4 is the largest structural change in the project and must be gated until #1, #2, and #3 are all stable.

### Phase 4 — Scenario Expansion (Issues 9, 10)
Mass casualty stochastic injection (#9, requires #1 + #2 + #5) → comparative scenario runner (#10, requires #1 + #2 + #5 + #8).

### Phase 5 — Interface (Issues 14, 15)
Two-part delivery. Issue #14 (parameter editor + Quick Run + single-run output display) can begin after #1 — the `R/analysis.R` refactor (returning ggplot objects) is the gating task. Issue #15 (Full Analysis mode — multi-run with CI) requires Issues #14, #1, #2, and #3 all complete.

### Recommended implementation sequence at a glance

```
NOW (unblocked):
  #8  [HOTFIX]  R2E surgical team seizure bug
  #1  [Ph.1]    Multi-run replication framework

AFTER #1:
  #2  [Ph.1]    Warm-up analysis          ─┐ parallel
  #3  [Ph.1]    Morris sensitivity        ─┘

AFTER #1 + #2 + #3:
  #5  [Ph.2]    Time-dependent DOW        ─┐
  #6  [Ph.2]    Dead-heading transport    ─┤ parallel
  #7  [Ph.3]    DNBI sub-categorisation  ─┘ (can pull forward; only needs #1 + #2)

AFTER #1 + #2 + #3 (all stable):
  #4  [Ph.3]    Individual resource seizure

AFTER #1 (analysis.R refactor only):
  #14 [Ph.2]    Shiny app — parameter editor + Quick Run

AFTER #14 + #1 + #2 + #3:
  #15 [Ph.5]    Shiny app — Full Analysis mode (multi-run CI)

AFTER #1 + #2 + #5:
  #9  [Ph.4]    Mass casualty injection

AFTER #1 + #2 + #5 + #8:
  #10 [Ph.4]    Scenario runner
```

---

## Code Standards

`docs/STYLE_GUIDE.md` is the code standard for every R source file in the repository, and it is authoritative: where this section and the standard could be read differently, the standard governs. Each of its rules is tagged `[lint]` (machine-checkable, and destined for the repository's lint configuration), `[review]` (applied by a reviewer without a judgement call) or `[preference]` (raised, but not blocking). Read it before writing R, and check a PR against it before opening one.

The rules below are the ones that come up in almost every PR. They are a pointer into the standard, not a substitute for it, and each names the rule that governs it.

- Every function carries a roxygen header, without exception, including one-line helpers and the `fail()`/`report()` helpers in a check script. Mandatory tags are a one-line title, `@param` for every argument, and `@return`; `@details` is required where the behaviour is not obvious from the title and the arguments (R1, R2).
- Branch logic carries a comment block describing the branch structure and the decision criterion for each arm before the `branch()` call (R4).
- Assign with `<-`, never `=`. Use the magrittr pipe `%>%`, never the native pipe. Keep lines inside 100 characters and function bodies inside 100 lines (F4, F5, F1, D1).
- Use `snake_case` for every variable and function name; `UPPER_SNAKE_CASE` for a file-scope constant. Resource variables follow `<type>_<echelon>` (e.g. `ot_beds`, `hold_beds`, `surg_team`), and trajectories take descriptive quoted names (e.g. `trajectory("R2B Surgery, DCS Phase 1")`) (N1, N2).
- Minutes per day is never the literal `1440`. `DAY_MIN` (`R/environment.R`) is its single definition, and the `day_min` global the execution model carries is assigned from it by each entry point; use `day_min` inside the model and the analysis pipeline, and `DAY_MIN` where no entry point has run yet, such as a regression check calling into a module directly, or in a parameter default that cannot name the global without resolving to itself. A parameter a planner might change belongs in `env_data.json` rather than in R source (C1, C3).
- `<<-` is permitted for closure state and, from an entry point only, for the four globals the execution model requires (`env`, `env_data`, `day_min`, `counts`). A function that mutates one of those and is expected to leave it as it found it restores it with `on.exit(..., add = TRUE)`, not by manual assignment at the foot of the function (G1 to G3).
- Anything read from outside the program (`env_data.json`, CLI arguments, a Shiny input) is validated before use and fails with a `stop()` message naming the field and the value found (E1, E2).
- A comment explains why; the academic documents explain the model. This is the reciprocal of the prose rule above: reasons only a maintainer needs go in the comment, and neither restates the other (R5, R6).

### Simmer-specific

- Use `select()` + `seize_selected()` for dynamic, policy-driven resource selection (not hardcoded resource names in `seize()`), and annotate the policy at the `select()` call (S1, S2).
- Resource monitoring: always use `get_mon_arrivals()` and `get_mon_resources()` on the wrapped environment list returned by the replication framework (S3).
- Never access `env` globals directly inside trajectory functions — use `get_attribute()` and `set_attribute()` for per-entity state (G5, S4).
- Where a trajectory's quoted name is constructed, hold the format string in a documented file-scope constant, so a rename cannot leave a regression check searching for a label the model no longer uses (S5).

### Regression check scripts

A new `scripts/check_*.R` follows the convention the standard sets out (K1 to K10): a shebang, a banner with a `# Usage:` block and a paragraph saying why the check exists, run parameters as file-scope constants, failures accumulated through `fail()` rather than stopping at the first, one `[PASS]`/`[FAIL]` line per assertion through `report()`, and an explicit `quit(status = 0)` or `quit(status = 1)`. A check signals failure by its exit status, never by `stop()`.

---

## Key Parameters (Shipped Configuration and the Seed-42 Baseline)

This section holds the shipped-configuration facts a change must not silently alter, and says where the measured seed-42 baseline lives. It carries no measured figure, since a figure copied here drifts the first time the baseline is regenerated.

> **Where the baseline is.** The seed-42 baseline is one 360-day run of the shipped default configuration (`Rscript run.R --seed 42 --days 360 --iterations 1 --refresh-baseline`), the horizon every replicated experiment adopted. Its measured figures (casualty generation by type, triage and damage control splits, the strategic evacuation timeline, the surgical load and the force trace) are in [Annex A of the results paper](docs/Results.md#annex-a-model-verification-of-one-seed-42-campaign), generated from the tracked `data/seed42_verification.csv`; the per-casualty event log is `logs/logs.txt`, and the arrival diagnostics are `data/arrivals_*.txt` and `data/mass_casualty_events.csv`. `scripts/check_baseline_reproduction.R` asserts that the whole tracked set reproduces byte for byte, so a regression test compares against Annex A and that check, never against a value restated here. A 360-day campaign draws a different arrival stream from a 30-day one, so a comparison between the two says nothing about the model; the superseded 30-day values are recorded in `docs/BCH_Simulation_Action_Plan.md`, this project's designated home for resolution history. The tracked set reproduces byte for byte in the project's unpinned R 4.3.3 sandbox; a maintainer re-run in `rocker/rstudio:4.4.2` remains the standard before the figures are treated as canonical.

| Fact | Shipped configuration |
|---|---|
| Force regeneration reinforcement mechanism | Enabled by default on a 7-day demand cycle (`demand_interval_days = 7`), matching the strategic aeromedical evacuation sortie interval; a planner-configured, not auto-balanced, demand/fulfillment-lag/triangular-fill model (not a fixed periodic size). See README [Force Regeneration and the Endogenous Feedback Loop](README.md#6-force-regeneration-and-the-endogenous-feedback-loop) for the mechanism and for why it ships enabled |
| DOW ceiling, Priority 1 (`p1_p_max`, logistic) | 2.0% (Falklands 1982 calibration) |
| DOW ceiling, Priority 2 (`p2_p_max`, logistic) | 1.6% (Falklands 1982 calibration) |
| DOW rate, Priority 3 (flat) | 0.1% (structural placeholder; Priority 3 is never evacuated) |
| Treated-cohort DOW rate | The ceilings were fitted to 30-day anchors, the Ajax Bay bound and the Okinawa rate, and `scripts/check_dow_calibration.R` still runs at 30 days. The rate measured at 360 days, pooled over independent measurements, is reported in [Treated-Cohort Mortality](docs/Results.md#treated-cohort-mortality); the shipped default sits above the Ajax Bay bound there, which the check's one-sided test would call an overshoot at that length. See README Further Development L22 |
| Replication count for mortality figures | The derivation, the counts each half-width requires and the resolution the 30-replication figures at the sustained horizon carry are stated in `docs/Methods.md`'s [Replication Count and Resolution](docs/Methods.md#replication-count-and-resolution) |
| All-damage-control equivalence | Setting `pri1_dcs_rate`, `pri2_dcs_rate` and `pri3_dcs_rate` to 1.0 reproduces the pre-Issue-173 model exactly. A degenerate rate of zero or one consumes no random draw, which is what makes the reproduction bit-identical rather than merely close. A property of the code, not re-measured at 360 days |
| R2B OT utilisation, shift time | Theatre occupancy divided by the time its surgical section is rostered. On an even two-shift day this is exactly twice the 24-hour room figure, the pre-open hold's off-roster occupancy being counted in the numerator of both |
| R2B evac team dead-heading | R2B→R2E WIA transport models a dead-heading return leg on the R2B team's own organic evac resource (`r2b_evac_leg()`/`r2b_evac_return_leg()`), matching the R1↔R2B legs; RNG-stream-shifting, not RNG-neutral |
| R2B→R2E mortuary transport | R2B KIA/DOW transported by road to the R2E-collocated mortuary via the shared HX2 40M fleet (`r2b_transport_kia()`, dead-heading return leg), then handed to a selected R2E team's mortuary intake (`r2e_mortuary_intake()`) |
| Strategic AME | C-17A Globemaster III at 36 critical / 54 standard places; `failure_probability` ships at 0; the wait-time DOW poll is `dow_echelon=5` at a daily interval (`role4.ame.dow_check_interval = 1440` min). See README [AME Wait Checkpoint](README.md#ame-wait-checkpoint) for why no single-run count should be read as evidence about the mechanism's magnitude |
| Sustained-horizon warm-up | `WARM_UP_DAYS` remains 0. The Welch cumulative-moving-average diagnostic at the sustained horizon is recorded in `docs/Methods.md`'s [The Welch Diagnostic at Length](docs/Methods.md#the-welch-diagnostic-at-length) |

---

## Out of Scope for Claude

- Merging to `main` — owner only.
- Changing the casualty rate baseline scenario without raising and discussing an issue first.
- Modifying `env_data.json` schema without a corresponding issue and PR.
- Removing or replacing existing references in `README.md`, `docs/Results.md`, `docs/Planning_Implications.md`, or `docs/Methods.md` without explicit instruction.
