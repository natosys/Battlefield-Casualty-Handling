# CLAUDE.md — Battlefield Casualty Handling Simulation

## Project Purpose

This is an **academic research project** producing a Discrete Event Simulation (DES) of deployed battlefield casualty handling. The simulation is written in R using the `simmer` package and is intended to provide evidence-based options to military planners for improving health outcomes in Large Scale Combat Operations (LSCO).

All work must meet academic research standards: reasoning must be explicit, sources must be cited, and limitations must be acknowledged. The project's academic output is split across four documents, each kept current with the code and written to the standard of a published academic paper: `README.md` (system reference — code structure, algorithms, trajectory logic, resource model, inline model assumptions, and Limitations), `docs/Results.md` (every measurement of the replicated experiments and the verification of one seed-42 campaign, generated from the tracked evidence, with no analysis or recommendation), `docs/Planning_Implications.md` (every lever read as a planning option, quoting `docs/Results.md` for every figure), and `docs/Methods.md` (the experimental design and statistical method behind those experiments). See [Document Maintenance](#document-maintenance) below for which PR types update which document.

---

## Repository Structure

The codebase is organised into a modular layout under `R/`, with `run.R` as the single CLI entry point. See the README's [Codebase Structure](README.md#codebase-structure) table for full detail on each `R/` module and the index of every `scripts/` entry point; this table covers the repository as a whole.

| File / Directory | Purpose |
|---|---|
| `run.R` | CLI entry point — validates arguments (via `R/cli.R`, before any simulation runs), orchestrates modules, and writes outputs. Takes `--config`, `--scenario`, `--mode`, `--images-dir` and `--max-cores` alongside the run parameters; `--config` (also on every `scripts/run_*.R`) names a configuration file other than the shipped `env_data.json`, validated before any run and rejected under `--refresh-baseline`, and a run outside the tracked evidence set writes a `run_manifest.json` beside its outputs |
| `R/constants.R` | Values shared across modules: `DAY_MIN` (minutes per simulated day) and `MODEL_ATTRIBUTE_KEYS`, every per-casualty attribute key the model can set, which `build_attributes_wide()` uses to give the wide pivot of the attributes monitor one shape whatever a run produced. Sourced by each module that needs one rather than by one module on every other's behalf, the modules under `R/` being otherwise independent |
| `R/cli.R` | Command-line argument validation shared by `run.R` and the `scripts/run_*.R` entry points — range and directory rules, the warm-up-against-run-length rule, execution mode resolution, the two conditions guarding a baseline refresh, and `resolve_config_path()` for the `--config` switch. Base R only, so a regression check can source it alone and exercise every rule without loading simmer or running the model |
| `R/run_manifest.R` | The record a run leaves of what it ran. `write_run_manifest()` writes `run_manifest.json` (the configuration file's SHA-256, the command, the seed, replication count and horizon, the git commit, the R and package versions), a byte copy of the configuration and its form under the scenario in force, beside the outputs of every entry point outside the tracked evidence set. Tracked evidence is exempt, a manifest carrying the commit changing with every commit and so breaking the byte-for-byte reproduction. Needs `R/scenario.R`, `R/environment.R` and the digest and jsonlite packages |
| `R/censoring.R` | Right-censored interval estimation: the Kaplan-Meier survival curve, its restricted mean and quantile, and `censored_interval_stats()`, which summarises one interval over every casualty who entered it rather than over those who also left it. Base R and independent of every other module, so `R/analysis.R` and `R/sensitivity.R` both source it and a KPI cannot drift from the screened response of the same name |
| `R/queue_series.R` | A resource pool's total queue as a step function of time, its time-weighted mean over a series of bins, and whether it ever cleared. The monitor records each bed separately, so the pool total is in none of its rows; it is recovered by differencing each bed's series into changes and accumulating them in time order. Base R and independent of every other module, so `R/analysis.R` and `R/long_horizon.R` both source it and the campaign figures and the long-horizon block series measure the same quantity by the same estimator |
| `R/long_horizon.R` | Sustained-operations replicated runner. Runs the model over the protocol's horizon and reduces each replication to a daily series inside the forked worker that produced it, so the parent holds the series rather than the monitoring data and peak memory is set by one replication rather than by their sum; `block_means()` collapses that series to the per-block means the protocol's stability statement is made from |
| `R/airlift.R` | Strategic evacuation and Role 4 demand across replications. Reduces each replication to one row of the response set inside the forked worker that produced it, sweeps a `role4.ame` field across values, and splits R2E holding bed occupancy into in-theatre recovery and evacuation wait. The split is exact rather than estimated: a casualty awaiting the standard airlift pool holds a holding bed for its whole wait, one awaiting the critical pool holds an intensive care bed instead and contributes nothing. The Role 4 census is drawn under the replication's own seed, that length of stay being drawn by the analysis rather than the simulation, so the peak is a function of the seed rather than of the caller's stream position |
| `R/role4_demand.R` | National support base bed demand across replications. Reduces each replication to a daily Role 4 census divided by ward phase (intensive care and step-down) and by origin (battle injury, DNBI, reconstruction cohort), with the operating theatre demand owed, inside the forked worker that produced it, on `R/airlift.R`'s arrangement. The census and the operations owed are each drawn under the replication's own seed, so the census total is the Role 4 peak `R/airlift.R` and `R/policy_sweep.R` report. Reports demand only: no capacity, queue or shortfall is given to the echelon, which `tests/checks/check_role4_ward_phases.R` asserts |
| `R/policy_sweep.R` | The evacuation policy and holding establishment trade across replications at the sustained horizon. Reduces each replication to one row of the response set inside the forked worker that produced it, on `R/airlift.R`'s arrangement, and reports the trade a policy consists of: forward stability and post-operative intensive care access against returns to duty and demand on the national support base. Forward stability is measured over the campaign's closing days rather than its whole length, a system still filling being indistinguishable from a settled one in an average over both. Differences between policies are taken within replication, each arm's own spread across campaigns being large enough to hide them |
| `R/hold_window.R` | The R2B pre-open hold window across replications, comparing the shipped 60-minute window against a window of zero. Reduces each replication to one row of the response set inside the forked worker that produced it, on `R/policy_sweep.R`'s arrangement, every response being a casualty count read off a single arrival attribute. The two arms are not the same realisation: introducing the hold shifts `simmer`'s single global stream from the first hold onward, so the arms drift into different casualty streams and pairing on a common control seed removes none of the between-run variance; the paired difference is reported anyway as the more precise of the two comparisons available |
| `R/surgical_team_sweep.R` | The surgical team establishment at R2B and R2E across replications, comparing five section counts (R2E 3, 4 and 6; R2B 1 or 2 per unit; both together) against the shipped establishment. Sets the count per echelon on the resolved configuration before the resources are built (`set_surgical_sections()` fails rather than returning the configuration unchanged), reduces each replication to one row of the response set and a daily R2E theatre queue inside the forked worker that produced it, on `R/hold_window.R`'s arrangement, and classifies each arm's queue converged or drifting by `R/long_horizon.R`'s late-half fit, reading its shortfall per day from the same slope. The arms are paired on one control seed each, though not on one casualty stream |
| `R/icu_bed_sweep.R` | The R2E intensive care bed establishment across replications, comparing the shipped four beds against six, eight, twelve and sixteen. Sets the count on the resolved configuration before the resources are built (`set_icu_establishment()` fails rather than returning the configuration unchanged), reduces each replication to one row of the response set and a daily R2E intensive care and holding queue inside the forked worker that produced it, on `R/surgical_team_sweep.R`'s arrangement, saving each finished replication so an interruption costs the replication in flight, and classifies each pool's queue converged or drifting through `surgical_stability()`. An R2E intensive care stay seizes the bed alone and not the intensive care section, so beds are swept without staff; the arms are paired on one control seed each, though not on one casualty stream |
| `R/icu_gate.R` | The post-operative intensive care gate across replications, comparing the shipped rationing rule (`icu_gating.enabled`) against a configuration that reconstructs the model as it stood before the gate existed. Reduces each replication to one row of the response set inside the forked worker that produced it, on the arrangement `R/hold_window.R` uses for a paired two-arm comparison; the two modules stay separate rather than sharing a runner. Intensive care occupancy is recovered from the resource monitor over the whole campaign on `R/policy_sweep.R`'s pool-differencing convention, and the pathway death counts are carried as raw numerators and denominators so a rate can be pooled across the tracked evidence set rather than averaged per replication |
| `R/casualty_surge.R` | The casualty surge event stress test across replications, comparing an arm with events injected against a background-only arm. Reduces each replication to one row of the response set inside the forked worker that produced it, on `R/hold_window.R`'s arrangement, but the two arms are not paired on one control seed, each running under its own: every casualty carries a `casualty_surge_event` attribute of 0 or 1 regardless of whether injection is active (`R/trajectories.R`), so one reduction applies unchanged to both arms rather than a pairing removing variance an unpaired design still carries. The died-of-wounds rate is pooled across replications rather than averaged per replication, a handful of deaths per 30-replication arm leaving most replications with none, with an exact binomial interval rather than a normal approximation that could extend below zero |
| `R/figures.R` | Result figures drawn from tracked data. Each function takes a data frame read from a tracked evidence set under `data/` and returns a ggplot object, among them the evacuation policy, R2E holding establishment, saturation release, post-operative intensive care gate, strategic airlift, sustained operations, Role 4 bed demand and casualty surge size figures, and the casualty surge timeline the analysis pipeline shares. ggplot2 and dplyr only and independent of every other module, so `scripts/render_sweep_figures.R` and a regression check can source it without simmer |
| `R/results.R` | Result tables generated from the tracked evidence sets. Holds the builders behind every table and quoted figure of `docs/Results.md`, the cell reader that lets a sentence quote a table cell, `render_results()`, which regenerates each `GEN` span of the document, and `seed42_verification_rows()`, the verification measurements `run.R` writes to `data/seed42/seed42_verification.csv`. Also holds the adequacy matrix, sizing and evacuation policy trade builders and the verdict criteria (`ADEQUACY_*`), which read the same tracked evidence for `docs/Planning_Implications.md` alone (`PLANNING_ONLY_TABLES`). Base R only, so a regression check can source it without simmer |
| `R/dow_calibration.R` | The treated-cohort died-of-wounds rate and the search for the ceilings that reproduce a target of it. Holds the historical anchors (`DOW_TARGETS`), the control seeds the pooled measurements are drawn under, `treated_cohort_rates()` and `measure_treated_cohort()`, which `tests/checks/check_dow_calibration.R` and `scripts/calibrate_dow.R` share so one measurement serves the check that asks whether a configuration agrees with its anchor and the fitter that asks what ceilings would make it agree, and `fit_dow_ceiling()`, an Illinois false position search on the pooled rate as a function of the Priority 1 ceiling, the Priority 2 ceiling following at a fixed ratio. The search takes the measurement as a function argument and is base R, so `tests/checks/check_dow_fitter.R` exercises it on a response whose answer is known; the measurement needs simmer and dplyr |
| `R/environment.R` | Data import, arrival generation, and simmer environment construction |
| `R/trajectories.R` | All simmer `trajectory()` definitions — R1, R2B, R2E, and core casualty flow |
| `R/replication.R` | Multi-run replication framework (`run_once`, `run_replications`, `summarise_replications`) |
| `R/analysis.R` | Analysis and visualisation pipeline. `analyse_run()` and `analyse_replications()` are orchestrators over named single-purpose functions, one per stage (data preparation, per-domain summary, plotting, writing); a change to one stage belongs in that stage's function |
| `R/sensitivity.R` | Morris EE screening and Sobol variance decomposition |
| `R/warmup.R` | Welch warm-up analysis |
| `R/app_params.R` | Parameter registry for the Shiny Configure panel |
| `R/scenario.R` | Scenario overlay mechanism (`resolve_scenario`, `merge_scenario_vars`); the profiles themselves are defined in `env_data.json`'s `scenarios` block |
| `R/scenario_runner.R` | Comparative scenario runner — executes the replication framework under a named scenario profile |
| `app.R` | Shiny console. `server()` is an orchestrator over per-panel functions, one per tab and one for the Quick Run; a change to one panel belongs in that panel's function. A configuration tool with a single-replication check: Configure/Run/Analyse workflow for interactive `env_data.json` parameter editing, Save and Load Configuration, Quick Run, and a *Run at Full Protocol* card that displays the `run.R --config` command for the saved file. It offers no replicated or sensitivity run, which belong to the command line where a run records its configuration, seed and commit (Issues #14, #505) |
| `env_data.json` | All simulation parameters — populations, resources, distributions, schedules |
| `scripts/` | The experiment entry points (`run_*.R`, each writing one tracked evidence set under `data/` and only under `--refresh-baseline`), the renderers (`render_*.R`, each regenerating a tracked document or figure from tracked data without running the model), and the sensitivity screen's tools (`calibrate_dow.R`, `compare_sobol_estimators.R`, `test_sobol_separation.R`, `screen_cache.sh`, `supervise_screen.sh`). Each is listed in the README's [Codebase Structure](README.md#codebase-structure) table and documented in its own banner, so none is described here; `tests/checks/check_structure_tables.R` asserts the README lists every one |
| `tests/checks/` | The regression checks, one `check_*.R` per asserted property, each opening with a banner saying what it asserts and why it exists; the per-check table of `docs/Continuous_Integration.md` is generated from those banners by `scripts/render_check_table.R`, and `tests/checks/check_ci_check_table.R` holds it complete, so no check is described here. Also holds the suite runner `run_all_checks.R`, which discovers every `check_*.R` by glob and exits non-zero if any fails (`--fast` omits the checks too slow for a per-PR gate, `--slow` runs those alone, `--list` prints the classification, `--jobs <n>`/`--jobs auto` runs that many at once, longest first); `check_runtimes.csv`, the measured runtime per check it dispatches from, written by `--refresh-runtimes` alone; and `lint_baseline.csv` and `roxygen_baseline.csv`, the per-rule finding counts the lint and roxygen ratchets defend, written by each check's `--refresh-baseline` alone |
| `tests/testthat/` | The console's R test suite, discovered and run by `tests/checks/check_testthat.R` and therefore gated per PR |
| `tests/playwright/`, `playwright.config.js`, `package.json`, `package-lock.json` | The console's browser test suite and the Node toolchain it needs, kept out of `renv.lock` so that no browser automation package enters the R dependency set. Run with `npx playwright test`; it starts the app itself and uses whatever Chromium `PLAYWRIGHT_BROWSERS_PATH` provides |
| `.lintr`, `tests/checks/lint_baseline.csv` | The lint configuration encoding the `[lint]`-tagged rules of `docs/STYLE_GUIDE.md`, and the per-rule finding counts the ratchet defends |
| `.github/` | Pull request template mirroring the test plan structure below, and the GitHub Actions workflow running the fast suite, the lint ratchet, the seed-42 reproduction and the console's browser suite on every PR against `main`, in the pinned container |
| `README.md` | System reference — introduction, literature review, methodology, codebase structure, trajectory logic, resource model, Mermaid diagrams, inline model assumptions, limitations, references. Does not contain simulation results. |
| `docs/Results.md` | Every measurement of every replicated experiment, and the verification of one 360-day seed-42 campaign in Annex A, with no analysis, interpretation or recommendation. Every table and every quoted figure is a generated span rebuilt from the tracked evidence under `data/` by `R/results.R` and `scripts/render_results_tables.R`, so a figure cannot differ from the evidence it reports; `tests/checks/check_results_tables.R` asserts that |
| `docs/Planning_Implications.md` | Every lever the model can evaluate read as a planning option: a diagnosis of where the system fails first, one lever table with an evidence label per lever, recommendations in priority order, the effects left unresolved (a generated table of paired differences and the replications each needs) and the research agenda. States no measurement of its own: every figure is a generated span quoting a cell of `docs/Results.md`, regenerated by `scripts/render_results_tables.R` and asserted by `tests/checks/check_planning_implications.R`, and every paragraph carrying one links the results section it comes from. Holds no experimental design or provenance |
| `docs/Methods.md` | A standalone academic paper companion to `docs/Planning_Implications.md` and `docs/Results.md`, written to the same standard and carrying its own abstract, introduction, limitations, conclusion and independently-numbered reference list: the full experimental design of every replicated experiment, the replication-independence argument, the withdrawn antithetic pairing and its measurement, the interval construction and replication-count derivation, the warm-up classification, the checks that defend each of those properties, and the provenance of the comparative figures. Holds no finding about the performance of the trauma system; every measured result of that kind belongs to the results paper. The one exception is the reinforcement comparison, which measures force generation rather than health system performance and is reported here in full. This paper is written to a ~5,000-word journal length and cites this document for the detail it no longer carries |
| `docs/Continuous_Integration.md` | Operating guide for the automated verification: what each GitHub Actions job runs and when, how to read a result, how to dispatch the slow suite, what each way the gate can fail calls for, and how a new check joins the suite |
| `docs/Getting_Started.md` | User guide for the Shiny console — the Configure/Run/Analyse workflow and how to read each output |
| `docs/STYLE_GUIDE.md` | The R code standard — every rule a reviewer checks a PR against, each tagged machine-checkable, reviewer-applied or preference; follow at all times |
| `data/` | Tracked evidence: one subdirectory per experiment, below, and the seed-42 baseline in `data/seed42/`. Nothing here is a model input; the configuration is `env_data.json` |
| `data/seed42/` | Tracked seed-42 baseline evidence set: the console log (`logs.txt`), the arrival diagnostics (`arrivals_*.txt`), the casualty surge event log (`casualty_surge_events.csv`) and the verification measurements behind Annex A (`seed42_verification.csv`), written by `R/environment.R` and `run.R` and rewritten only under `run.R --refresh-baseline` |
| `data/sensitivity/` | Tracked sensitivity evidence set — the 360-day Morris r=20 and Sobol N=96 design point caches (`morris_r20_d360/`, `sobol_n96_d360/`), their per-response rankings and decompositions and the estimator and separation re-analyses, behind [Sensitivity Screens](docs/Results.md#sensitivity-screens); the Morris cache alone is about 78 hours of computation on four cores, kept because every published index and rank derives from it and it cannot be regenerated cheaply. |
| `data/airlift/` | Tracked strategic evacuation evidence set — the per-replication responses and their summary behind the national support base section of `docs/Results.md`, at 30 replications over 360 days per arm across two baselines and two sweeps, plus the collapse classification behind the strategic airlift reliability sweep at 30 replications over 360 days per arm and the classified pool's daily series, which is retained so that a statement about how a campaign's opening relates to its outcome stays recomputable. Written by `scripts/run_airlift_sweep.R --refresh-baseline` and `scripts/run_airlift_collapse.R --refresh-baseline` alone, each owning its own files |
| `data/role4_demand/` | Tracked Role 4 bed demand evidence set — the daily census series by ward phase and origin with the theatre demand owed by source, the per-replication responses and their summary, the cross-replication daily mean, the weekly operations owed, the stationarity classification of the census, and the cancellation arm's responses and summary, behind [Role 4 Bed Demand](docs/Results.md#role-4-bed-demand), at 30 replications over 360 days per casualty intensity and per cancellation probability. Written by `scripts/run_role4_demand.R --refresh-baseline` alone |
| `data/scenarios/` | Tracked comparative scenario evidence set — the casualty totals, the per-resource queue summaries, and the per-pool queue comparison both per replication and reduced, behind the centrepiece tables of `docs/Results.md`, at 30 replications over 360 days for each of the two casualty intensities and for the high-intensity casualty load under the shipped treatment model (`high_intensity_modern_care`), queues measured over the closing 90 days. The per-replication pool series is kept because a pool's interval cannot be recovered from per-bed summaries, the pool total being in none of the monitor's rows. Also carries the queue and utilisation of the four transport holders (the two shared fleets and the evacuation crews integral to R2B and R2E), per replication and reduced. Written by `scripts/run_scenarios.R --refresh-baseline` alone |
| `data/sweeps/` | Tracked capacity sweep evidence set — the per-point means and intervals behind the transport fleet-size table, the forward holding frontier table and the R2B holding capacity/evacuation threshold table of `docs/Results.md`, all three now at 30 replications per point over 360 days, the sustained-operations protocol every one of them migrated to under Issue #405 (transport from 10 over 30 days, the other two from their own counts over 30 days, migrated for cross-table consistency rather than an under-resolution finding of their own). Written by `scripts/run_transport_sweep.R --refresh-baseline`, `scripts/run_forward_hold_sweep.R --refresh-baseline` and `scripts/run_hold_threshold_sweep.R --refresh-baseline` alone, each owning its own file; the transport sweep's `--scenario high_intensity` re-run writes its own `_high_intensity`-suffixed copy alongside the shipped configuration's |
| `data/long_horizon/` | Tracked sustained-operations evidence set — the reduced daily series and the per-block means behind every long-horizon statement in `docs/Methods.md`, at 30 replications over 360 days per casualty intensity, plus the Welch cumulative-moving-average reduction of that series behind the sustained-horizon warm-up diagnostic. The series and block means are written by `scripts/run_long_horizon.R --refresh-baseline`; the CMA is written separately by `scripts/render_long_horizon_warmup.R --refresh-baseline`, which reads the series rather than re-running the model |
| `data/policy/` | Tracked evacuation policy and holding establishment evidence set — the per-replication responses, their summary and the paired differences against the shipped policy behind the policy sweep of `docs/Results.md`, at 30 replications over 360 days for each of five policies, plus the `establishment_sweep*` files of the holding establishment measurement, and the `establishment_sweep_moderate_grid*` files of the moderate-intensity policy by establishment grid (four policies by three establishments), and the `interval_sweep_high_intensity*` files of the high-intensity sortie interval by policy by establishment grid (three intervals by two policies by two establishments). Written by `scripts/run_policy_sweep.R --refresh-baseline` alone |
| `data/hold_window/` | Tracked R2B pre-open hold window evidence set — both arms' per-replication responses, their summary and the paired differences behind the eight-row table of `docs/Results.md`'s hold window section, at 30 replications over 360 days per arm, the sustained-operations protocol. Written by `scripts/run_hold_window.R --refresh-baseline` alone |
| `data/surgical_teams/` | Tracked surgical team establishment evidence set: both intensities' per-replication responses, their summary, the paired differences against the shipped establishment, the R2E theatre queue classification, and each arm's daily theatre series, behind [Surgical Team Establishment](docs/Results.md#surgical-team-establishment), at 30 replications over 360 days per arm for each of five section counts. Written by `scripts/run_surgical_team_sweep.R --refresh-baseline` alone |
| `data/icu_beds/` | Tracked R2E intensive care bed establishment evidence set: both intensities' per-replication responses, their summary, the paired differences against the shipped four beds, the intensive care and holding queue classification, and each arm's daily series, behind [R2E Intensive Care Bed Establishment](docs/Results.md#r2e-intensive-care-bed-establishment), at 30 replications over 360 days per arm for each of five bed counts. Written by `scripts/run_icu_bed_sweep.R --refresh-baseline` alone |
| `data/icu_gate/` | Tracked post-operative intensive care gate evidence set — both arms' per-replication responses, their summary and the paired differences behind [The Post-Operative Intensive Care Gate](docs/Results.md#post-operative-intensive-care-gate), at 30 replications over 360 days per arm, the sustained-operations protocol. Written by `scripts/run_icu_gate.R --refresh-baseline` alone |
| `data/casualty_surge/` | Tracked casualty surge event stress test evidence set — both arms' per-replication responses, the total-casualty and event-count summary and the pooled died-of-wounds rate by casualty origin, together with the event size sweep's per-size responses and summary, behind [Casualty Surge Events Degrade Care Without Revealing New Constraints](docs/Results.md#casualty-surge-events), at 30 replications over 360 days per arm, plus the illustrative single run behind `images/casualty_surge_events.png`. Written by `scripts/run_casualty_surge.R --refresh-baseline` alone |
| `data/time_series/` | Tracked campaign time series evidence set — the per-replication queue series, queue clearance statistics and degraded post-operative care rates behind the two time series figures of `docs/Results.md`, at 30 replications over 360 days per casualty intensity. Kept rather than regenerated per render so that a figure and the percentages the paper quotes from it derive from one measurement; written by `scripts/render_time_series_figures.R --run --refresh-baseline` alone |
| `data/calibration/` | Tracked treated-cohort died-of-wounds measurement at the sustained horizon, the pooled rate and interval of each configuration against its historical anchor, behind the mortality section of `docs/Results.md`. Written by `tests/checks/check_dow_calibration.R --days 360 --reps 10 --write-csv data/calibration/dow_calibration.csv` alone |
| `images/` | Tracked seed-42 baseline plots and reference diagrams, regenerated as part of baseline-affecting PRs via `run.R --refresh-baseline` |
| `outputs/` | Gitignored destination for every ordinary run's artifacts: CSV/markdown outputs, plots (`outputs/images/`), console log, and arrival diagnostics (`outputs/data/`); tracked via `.gitkeep` only |
| `renv/`, `renv.lock`, `.Rprofile` | R package environment management |
| `.devcontainer/` | Pinned Dev Container definition (`rocker/rstudio:4.4.2`) used for canonical baseline runs |

---

## Development Workflow

### Branch Rules

- **All development happens on branches.** Never commit directly to `main`.
- **Only the repository owner can merge to `main`.** Do not merge to `main` directly. Always open a PR and await owner merge.
- **Always open a PR at the end of each issue.** Use the GitHub MCP tools (`mcp__github__create_pull_request`) to create the PR with a test plan in the description before handing over. Never ask the user to merge via git commands — they merge through GitHub.
- Branch naming: `<type>/issue-<number>-<short-description>`, where the type is `feature` for new capability, `fix` for a defect and `chore` for maintenance or post-merge work (e.g., `feature/issue-1-multi-run-replication`, `chore/post-pr541-baseline`). Where the environment assigns a branch, as a hosted session does, that branch is used as given.
- Each GitHub Issue corresponds to one branch and one PR.

### Sequence

1. Raise a GitHub Issue describing the work (see Issue Format below).
2. Create a branch from `main`, named as Branch Rules sets out.
3. Implement the changes.
4. Update the relevant document(s) — `README.md` and/or the `docs/` analysis documents — as part of the same PR (see Document Maintenance below).
5. Open a PR against `main` with a test plan (see Test Plans below).
6. Await owner merge — do not self-merge.

### Post-Merge Checklist

After the repository owner merges a PR to `main`, perform the following tasks. Where step 2 or 3 applies, do it on a new chore branch (`chore/post-pr<N>-baseline`) and open a follow-up PR. Resolution history lives in the issue tracker, the merged PR's description and the git history; no tracked document restates it.

**1. Update GitHub issue labels**

For each issue newly unblocked by the merge: change its label from `status: blocked` to `status: ready` using the GitHub MCP tools.

**2. Refresh the seed-42 baseline (if simulation outputs changed)**

If the merged PR modified `R/trajectories.R`, `R/environment.R`, or `env_data.json` in a way that shifts the RNG stream or alters stochastic outputs, re-run the simulation at seed 42, regenerate `docs/Results.md` Annex A and `docs/Planning_Implications.md` with `scripts/render_results_tables.R --refresh-baseline`, and document the change, with the superseded values, in the chore PR's description. `CLAUDE.md` carries no measured seed-42 figure to update.

The re-run must be invoked with the `--refresh-baseline` flag, which is the only way to write the tracked baseline evidence set (`images/`, `data/seed42/logs.txt`, `data/seed42/arrivals_*.txt`, `data/seed42/casualty_surge_events.csv`, `data/seed42/seed42_verification.csv`):

```sh
Rscript run.R --seed 42 --days 360 --iterations 1 --refresh-baseline
```

Without the flag, every run writes to `outputs/` alone and leaves all tracked artifacts untouched, so an exploratory or smoke-test run cannot corrupt the baseline. The flag requires `--iterations 1` and errors otherwise, because the console log and the arrival diagnostics have no multi-replication equivalent; this is what guarantees the tracked sets always describe the same single run. Commit them together, as one commit, or not at all: a PR that regenerates only part of the set reintroduces the drift Issue #154 closed.

**3. Regenerate the README environment summary (if `env_data.json` changed)**

If the merged PR modified `env_data.json`, run `tests/checks/check_env_data_summary.R --refresh-baseline` to refresh the `<!-- ENV SUMMARY START/END -->` block inside `README.md` and include the updated `README.md` in the chore PR.

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

Every issue carries labels that make its type, readiness and urgency visible in the issue list without opening it. The title carries no prefix: the project's five development phases are complete, and an issue raised now is filed by what it is rather than by a phase it does not belong to. Closed issues keep the `[Ph.N]` prefixes and `phase/*` labels they were raised with.

### Labels

**Type labels**, one per issue: `bug` (defects in existing behaviour), `enhancement` (new capability or improvement).

**Status labels**, maintained as work progresses: `status: ready` (no blocking dependencies), `status: blocked` (has unresolved dependencies).

**Priority labels**, applied when the issue warrants it: `priority: critical` (a bug that invalidates current output), `priority: high` (blocks other issues).

### Raising new issues

1. Apply a type label, a status label and, where warranted, a priority label.
2. Set `status: ready` if it can be started immediately; `status: blocked` if it depends on open issues, naming them in the body.
3. When a blocking issue merges, update the `status` label on every issue it unblocks.

---

## Test Plans

Every PR must include a **Documented Manual Test Plan** in the PR description, following the structure `.github/pull_request_template.md` prompts for.

Verification has two halves. The `tests/checks/check_*.R` regression checks are automated and gated: `Rscript tests/checks/run_all_checks.R --fast` runs every check a PR is gated on, and GitHub Actions runs the same suite, the lint ratchet, the seed-42 byte-for-byte reproduction and the Shiny console's browser suite on every PR against `main` in the pinned container (`.github/workflows/checks.yml`). The console's own coverage is split by what each half can see: `tests/testthat` drives the reactive state machine through `shiny::testServer()` and runs inside the fast suite, and `tests/playwright` drives a running app in headless Chromium and runs as its own job. A PR is not ready for review while that workflow is red; `docs/Continuous_Integration.md` is the operating guide for reading and acting on a result. Everything the checks do not assert, which is most of what a change to the model does, is verified by documented manual execution, which is what the test plan records. A behaviour worth protecting past the PR that introduces it belongs in a new `tests/checks/check_*.R`, which the runner discovers by glob and therefore gates from the moment it is committed.

Test plans must include:

1. **Setup** — seed, run duration, any parameter changes required to observe the behaviour under test.
2. **Steps** — numbered list of actions to execute.
3. **Expected outputs** — specific, observable values or patterns (e.g., "mean R2E ICU queue across replications should be non-zero and vary between replications").
4. **Regression checks** — confirm that outputs from unmodified pathways remain consistent with the seed-42 baseline: the measured figures in `docs/Results.md` Annex A, which are generated from the tracked `data/seed42/seed42_verification.csv`, and the byte-for-byte reproduction of the tracked set that `tests/checks/check_baseline_reproduction.R` asserts.
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

The boundary between the documents is what each states. What was measured goes in the results paper, what it means for a planner goes in the planning paper, how it was measured goes in the methods paper, and how the system works and why it is configured as it is goes in the README. A figure is stated once, in the results paper, and cited from the others. Cross-references between the documents (`[text](../README.md#anchor)`, `[text](docs/Results.md#anchor)`, `[text](docs/Planning_Implications.md#anchor)`, `[text](docs/Methods.md#anchor)`, as appropriate to the source document's location) must stay valid: re-run `tests/checks/check_markdown.R` after moving or renaming any heading referenced from another document.

All four are updated **as part of every PR that changes what they state**, not retrospectively. The generated documents regenerate with one command, so a measurement changing in a PR changes the figures in `docs/Results.md` and `docs/Planning_Implications.md` together.

### What to update per PR

| The PR changes | `README.md` | `docs/Results.md` | `docs/Planning_Implications.md` | `docs/Methods.md` |
|---|---|---|---|---|
| Trajectory logic, resource logic or a distribution | Simulation Design, the Mermaid diagram for the echelon, Further Development | Regenerate (a measurement may move) | Only if a lever's evidence label changes | Only if an experiment's design changes |
| A shipped default in `env_data.json` | The parameter and the rationale for it; run `tests/checks/check_env_data_summary.R --refresh-baseline` | Regenerate | Only if a lever's evidence label changes | No |
| An evidence set is re-run, at any replication count | No | Regenerate with `scripts/render_results_tables.R --refresh-baseline`; add prose only where a new experiment needs its own section | Regenerates on its own; revisit the recommendations the new measurement bears on | The experiment's replication count or protocol line, if changed |
| A new lever or experiment | Where the lever is configured | A new section and, where it has a table, a builder in `R/results.R` | A row in the lever table, with its evidence label | The experiment's design |
| An experiment's design, seed, parameter overrides or invocation | The CLI summary, if it changed | No | No | The experiment's design section |
| The replication framework, interval method or warm-up classification | The one-paragraph summary and its link | No | No | The Replication Framework section |
| The seed-42 baseline (`--refresh-baseline`) | No | Annex A regenerates | Regenerates | No |
| A gap is closed or newly identified | Further Development: delete the entry or add one with a new identifier | No | The research agenda, if it names the gap | No |
| A new source is cited | References | References | References | References |
| A new `R/` module, `scripts/` entry point or `tests/checks/` check | The Codebase Structure table (a module or entry point) | No | No | The checks paragraph, if the check defends a documented property; a module also needs a row in this file's Repository Structure table, and a check regenerates the table of `docs/Continuous_Integration.md` |

Each document's References section lists only the sources that document itself cites, numbered in order of first appearance within that document, not a shared numbering scheme across all four. A source cited in more than one document is renumbered independently in each.

### Style

- Write in academic third-person prose. Avoid first person.
- **Write at a post-graduate research level that stays accessible to non-experts.** Use clear, plain prose and only standard dictionary words; do not coin non-standard terms (e.g. write "has not undergone surgery," not "unsurgicated").
- **Refer to people in the model as casualties, not "candidates."** "Candidate" is reserved for its other established uses in this project (a screened parameter, a scheduled day, a proposed intervention); a casualty being assessed or eligible for surgery is a "casualty requiring surgery" or "Priority N casualty," never a "surgical candidate" or "Priority N candidate."
- All parameters, probabilities, and distributions must be cited to their source.
- New methods introduced must reference the algorithm or statistical technique by name, with citation (e.g., "Morris Elementary Effects screening (Morris, 1991) was applied using R's `sensitivity` package").
- Tables and flowcharts must be kept synchronised with the code.
- **A figure in `docs/Results.md` or `docs/Planning_Implications.md` is a generated span, never typed.** A table or a quoted figure is a `GEN` span rebuilt from the tracked evidence by `scripts/render_results_tables.R`; a sentence quotes a cell of a table through a cell reference rather than copying it. `tests/checks/check_results_tables.R` and `tests/checks/check_planning_implications.R` fail on a figure typed outside a span.
- **Do not use em dashes** in new or edited prose across `README.md`, `docs/Results.md`, `docs/Planning_Implications.md`, and `docs/Methods.md`. Use commas, parentheses, or semicolons instead.
- **Simulation Design narrative sections describe only the current design.** Trajectory logic, algorithm, and resource-model sections state how the model works now, with supporting evidence (citations, code function names, computed figures), not how it used to work or which issue changed it (e.g. no "prior to Issue #N..." or "as of Issue #N..." framing, and no issue-number suffix on section/heading titles). Which issue addressed or introduced an item is recorded in the issue tracker and the PR that closed it.
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
- Each entry carries a stable `L<n>` identifier, cited from the analysis documents and from issues and PRs. **Identifiers are never reused or renumbered**, since renumbering silently redirects every existing citation.
- **A closed gap is deleted, not marked resolved.** The section describes the model's current gaps only; resolution history belongs to the issue tracker and the PR that closed the gap. When deleting an entry, search all four documents for citations of its identifier and repair them in the same PR.
- Do not cite issue numbers here. This section is not exempt from the issue-reference rule; the issue tracker is the tracker.
- Group entries under `### High Impact`, `### Medium Impact` and `### Low Impact`, in that order, numerically within each group. **A grouped list must be re-checked against its headings after any reordering.**
- The scan table at the head of the section is derived from the entries, so each row must repeat its entry's title and the impact group the entry sits under, exactly. `tests/checks/check_markdown.R` asserts this; reconcile the table to the entries, not the reverse.

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

A new `tests/checks/check_*.R` follows the convention the standard sets out (K1 to K10): a shebang, a banner with a `# Usage:` block and a paragraph saying why the check exists, run parameters as file-scope constants, failures accumulated through `fail()` rather than stopping at the first, one `[PASS]`/`[FAIL]` line per assertion through `report()`, and an explicit `quit(status = 0)` or `quit(status = 1)`. A check signals failure by its exit status, never by `stop()`.

---

## Key Parameters (Shipped Configuration and the Seed-42 Baseline)

This section holds the shipped-configuration facts a change must not silently alter, and says where the measured seed-42 baseline lives. It carries no measured figure, since a figure copied here drifts the first time the baseline is regenerated.

> **Where the baseline is.** The seed-42 baseline is one 360-day run of the shipped default configuration (`Rscript run.R --seed 42 --days 360 --iterations 1 --refresh-baseline`), the horizon every replicated experiment adopted. Its measured figures (casualty generation by type, triage and damage control splits, the strategic evacuation timeline, the surgical load and the force trace) are in [Annex A of the results paper](docs/Results.md#annex-a-model-verification-of-one-seed-42-campaign), generated from the tracked `data/seed42/seed42_verification.csv`; the per-casualty event log is `data/seed42/logs.txt`, and the arrival diagnostics are `data/seed42/arrivals_*.txt` and `data/seed42/casualty_surge_events.csv`. `tests/checks/check_baseline_reproduction.R` asserts that the whole tracked set reproduces byte for byte, so a regression test compares against Annex A and that check, never against a value restated here. A 360-day campaign draws a different arrival stream from a 30-day one, so a comparison between the two says nothing about the model; the superseded 30-day values are recorded in the git history of `docs/Results.md` Annex A. The tracked set reproduces byte for byte in the project's unpinned R 4.3.3 sandbox; a maintainer re-run in `rocker/rstudio:4.4.2` remains the standard before the figures are treated as canonical.

| Fact | Shipped configuration |
|---|---|
| Force regeneration reinforcement mechanism | Enabled by default on a 7-day demand cycle (`demand_interval_days = 7`), matching the strategic aeromedical evacuation sortie interval; a planner-configured, not auto-balanced, demand/fulfillment-lag/triangular-fill model (not a fixed periodic size). See README [Force Regeneration and the Endogenous Feedback Loop](README.md#6-force-regeneration-and-the-endogenous-feedback-loop) for the mechanism and for why it ships enabled |
| DOW ceiling, Priority 1 (`p1_p_max`, logistic) | 0.89% (Falklands 1982 calibration, paired with 1982-era treatment efficacy factors) |
| DOW ceiling, Priority 2 (`p2_p_max`, logistic) | 0.74% (Falklands 1982 calibration, paired with 1982-era treatment efficacy factors) |
| DOW rate, Priority 3 (flat) | 0.1% (structural placeholder; Priority 3 is never evacuated) |
| Treated-cohort DOW rate | The default ceilings are held beneath the 30-day Ajax Bay bound by design rather than fitted to its value, the `high_intensity` pair is fitted to the Okinawa rate, and `tests/checks/check_dow_calibration.R` still runs at 30 days. The rate measured at 360 days, pooled over independent measurements, is reported in [Treated-Cohort Mortality](docs/Results.md#treated-cohort-mortality); the shipped default, which is the moderate-intensity calibration, sits within the Ajax Bay bound there. See README Further Development L22 |
| Replication count for mortality figures | The derivation, the counts each half-width requires and the resolution the 30-replication figures at the sustained horizon carry are stated in `docs/Methods.md`'s [Replication Count and Resolution](docs/Methods.md#replication-count-and-resolution) |
| All-damage-control equivalence | Setting `pri1_dcs_rate`, `pri2_dcs_rate` and `pri3_dcs_rate` to 1.0 reproduces the pre-Issue-173 model exactly. A degenerate rate of zero or one consumes no random draw, which is what makes the reproduction bit-identical rather than merely close. A property of the code, not re-measured at 360 days |
| R2B OT utilisation, shift time | Theatre occupancy divided by the time its surgical section is rostered. On an even two-shift day this is exactly twice the 24-hour room figure, the pre-open hold's off-roster occupancy being counted in the numerator of both |
| R2B evac team dead-heading | R2B→R2E WIA transport models a dead-heading return leg on the R2B team's own organic evac resource (`r2b_evac_leg()`/`r2b_evac_return_leg()`), matching the R1↔R2B legs; RNG-stream-shifting, not RNG-neutral |
| R2B→R2E mortuary transport | R2B KIA/DOW transported by road to the R2E-collocated mortuary via the shared HX2 40M fleet (`r2b_transport_kia()`, dead-heading return leg), then handed to a selected R2E team's mortuary intake (`r2e_mortuary_intake()`) |
| Shipped default configuration | The moderate-intensity Falklands calibration: Falklands casualty generation, DOW ceilings and 1982-era treatment efficacy factors together. `moderate_intensity` is an empty overlay kept as an alias of it, and `high_intensity` is the one overlay that changes the configuration. The Shiny dropdown offers "Moderate Intensity" and "High Intensity" |
| Strategic AME | C-17A Globemaster III at 36 critical / 54 standard places; `failure_probability` ships at 0; the wait-time DOW poll is `dow_echelon=5` at a daily interval (`role4.ame.dow_check_interval = 1440` min). See README [AME Wait Checkpoint](README.md#ame-wait-checkpoint) for why no single-run count should be read as evidence about the mechanism's magnitude |
| Sustained-horizon warm-up | `WARM_UP_DAYS` remains 0. The Welch cumulative-moving-average diagnostic at the sustained horizon is recorded in `docs/Methods.md`'s [The Welch Diagnostic at Length](docs/Methods.md#the-welch-diagnostic-at-length) |

---

## Out of Scope for Claude

- Merging to `main` — owner only.
- Changing the casualty rate baseline scenario without raising and discussing an issue first.
- Modifying `env_data.json` schema without a corresponding issue and PR.
- Removing or replacing existing references in `README.md`, `docs/Results.md`, `docs/Planning_Implications.md`, or `docs/Methods.md` without explicit instruction.
