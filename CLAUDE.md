# CLAUDE.md — Battlefield Casualty Handling Simulation

## Project Purpose

This is an **academic research project** producing a Discrete Event Simulation (DES) of deployed battlefield casualty handling. The simulation is written in R using the `simmer` package and is intended to provide evidence-based options to military planners for improving health outcomes in Large Scale Combat Operations (LSCO).

All work must meet academic research standards: reasoning must be explicit, sources must be cited, and limitations must be acknowledged. The project's academic output is split across four documents, each kept current with the code and written to the standard of a published academic paper: `README.md` (system reference — code structure, algorithms, trajectory logic, resource model, inline model assumptions, and Limitations), `docs/Single_Run_Analysis.md` (the seed-42 verification and behavioural walk-through of one campaign), `docs/Multi_Run_Analysis.md` (every replicated experiment, the n≥30/95% CI scenario comparison among them), and `docs/Multi_Run_Supplement.md` (the experimental design and statistical method behind those experiments). See [README Maintenance](#readme-maintenance) below for which PR types update which document.

---

## Repository Structure

The codebase is organised into a modular layout under `R/`, with `run.R` as the single CLI entry point. See the README's [Codebase Structure](README.md#codebase-structure) table for full detail on each `R/` module; this table covers the repository as a whole.

| File / Directory | Purpose |
|---|---|
| `run.R` | CLI entry point — validates arguments (via `R/cli.R`, before any simulation runs), orchestrates modules, and writes outputs. Takes `--scenario`, `--mode`, `--images-dir` and `--max-cores` alongside the run parameters |
| `R/constants.R` | Values shared across modules: `DAY_MIN` (minutes per simulated day) and `MODEL_ATTRIBUTE_KEYS`, every per-casualty attribute key the model can set, which `build_attributes_wide()` uses to give the wide pivot of the attributes monitor one shape whatever a run produced. Sourced by each module that needs one rather than by one module on every other's behalf, the modules under `R/` being otherwise independent |
| `R/cli.R` | Command-line argument validation shared by `run.R` and `scripts/run_warmup.R` — range and directory rules, the warm-up-against-run-length rule, execution mode resolution, and the two conditions guarding a baseline refresh. Base R only, so a regression check can source it alone and exercise every rule without loading simmer or running the model |
| `R/censoring.R` | Right-censored interval estimation: the Kaplan-Meier survival curve, its restricted mean and quantile, and `censored_interval_stats()`, which summarises one interval over every casualty who entered it rather than over those who also left it. Base R and independent of every other module, so `R/analysis.R` and `R/sensitivity.R` both source it and a KPI cannot drift from the screened response of the same name |
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
| `scripts/run_scenarios.R` | CLI entry point for the comparative scenario runner |
| `scripts/render_dow_survival.R` | Renders `images/dow_survival_function.png` from the `dow.params` block of `env_data.json`, for the base configuration or a `--scenario` profile, so a re-fitted `p_max` cannot leave the figure disagreeing with the calibration table beneath it; `--refresh-baseline` is the only way to write the tracked image |
| `scripts/render_paper_figures.R` | Renders the three result tables of `docs/Multi_Run_Analysis.md` as figures, parsing the values out of the paper's own markdown tables rather than holding a second copy of them, so a figure cannot disagree with the table it illustrates; fails rather than writing where a table has moved or no longer parses, and `--refresh-baseline` is the only way to write the tracked `images/paper_*.png` |
| `scripts/render_morris_plots.R` | Re-renders a completed sensitivity screen's Morris scatter plots from its saved design and responses, without running the model again, checking each response's recomputed µ\* and σ against the tracked ranking CSV before writing so a plot cannot drift away from the table it illustrates; `--refresh-baseline` is the only way to write the tracked `images/morris_*.png` |
| `scripts/screen_cache.sh` | Checkpoints a sensitivity screen's point cache onto its own git ref and restores it, so a multi-hour screen survives an environment that reclaims its filesystem mid-run |
| `scripts/supervise_screen.sh` | Drives a long screen to completion across environment failures, restoring the cache before each attempt and checkpointing while the screen runs |
| `scripts/compare_sobol_estimators.R` | Recomputes a completed Sobol decomposition's cached responses under the Jansen and Martinez pick-freeze estimators alongside the reported Saltelli one, which share the same design and so cost no further simulation, and reports whether the ordering and the separations survive the change of estimator |
| `scripts/measure_noise_floor.R` | Measures how much of a completed Sobol decomposition's variance is replication noise rather than parameter effect, by evaluating a sample of design points at many more replications than the decomposition used; reports the factor the reported indices are deflated by and the replications per point that would make it negligible |
| `scripts/test_sobol_separation.R` | Tests whether a completed Sobol decomposition separates one parameter from the next, bootstrapping the design rather than the indices so that two indices estimated from the same evaluations keep their correlation, and reports the sample size each unestablished separation would require |
| `scripts/run_transport_sweep.R` | CLI entry point for the transport fleet-size sweep |
| `scripts/run_icu_share_sweep.R` | CLI entry point for the forward ICU share (R2B post-operative stabilisation) sweep |
| `scripts/shiny_worker.R` | Background worker sourced by `app.R` for async Quick Run / Full Analysis execution |
| `scripts/check_env_data_summary.R` | Regenerates the `<!-- ENV SUMMARY -->` block inside `README.md` from `env_data.json` |
| `scripts/check_markdown.R` | Maintains the TOC and "Return to Top" links across `README.md`, `docs/Single_Run_Analysis.md`, `docs/Multi_Run_Analysis.md` and `docs/Multi_Run_Supplement.md`, generating each anchor as GitHub does, and asserting that its own entry-heading match is byte-wise so the check does not depend on the session locale; exits non-zero if any anchor link points at no heading, if any local link or image target does not exist when resolved relative to the document containing it, if any image carries placeholder or empty alt text, or if a row of the README's Further Development scan table names a gap or an impact differently from the entry it points at. The link, target and alt-text checks run across every tracked markdown document, including this one and `docs/BCH_Simulation_Action_Plan.md` (which carry no TOC block and must not be given one); the scan table check applies to `README.md` alone. External URLs are out of scope |
| `scripts/check_references.R` | Regression check asserting that each of the four academic documents' reference lists is sound: every `[[n]]` citation resolves to an entry, every entry is cited at least once, the list is numbered from one in order of first appearance, no two entries share a URL, and every entry carries a URL and a retrieval date. Whether a URL is open access is a judgement the script cannot make and remains a manual step at the point a reference is added; exits non-zero on failure |
| `scripts/check_r2e_surgery_seizure.R` | Regression check asserting that every R2E surgery seizes a surgical section, structurally and behaviourally; exits non-zero on failure |
| `scripts/check_icu_time_conservation.R` | Regression check asserting that a casualty's post-operative ICU requirement is conserved across all three routes and at every forward ICU share; exits non-zero on failure |
| `scripts/check_composition_ilr.R` | Regression check asserting that each simplex-constrained composition group stays on the simplex through its screened balance coordinates; exits non-zero on failure |
| `scripts/check_morris_baseline.R` | Regression check asserting that every screened parameter's baseline lies inside its own screening bounds and equals the value it holds in `env_data.json`; exits non-zero on failure |
| `scripts/check_dow_calibration.R` | Regression check asserting that each shipped configuration's treated-cohort died-of-wounds rate agrees with the historical anchor of the campaign it models, the Ajax Bay bound for the two Falklands-calibrated configurations and the reported Okinawa rate for `high_intensity`, pooling independent measurements; exits non-zero on failure |
| `scripts/check_mass_casualty_kia_split.R` | Regression check asserting that a mass casualty event's casualty count is conserved across the wounded/killed split, that the realised killed share tracks the configured one, that an event's killed reach mortuary handling untriaged, and that the share reaches nothing while injection is disabled; exits non-zero on failure |
| `scripts/check_lever_realisation.R` | Regression check asserting that two configured planner levers are applied in full: that every person of a reinforcement fill joins the population even where that carries a pool over establishment strength, and that a casualty evacuated from R2B holding under `evac_threshold` serves the remainder of the convalescence already drawn rather than a fresh draw; exits non-zero on failure |
| `scripts/check_console_bindings.R` | Regression check asserting that no Shiny console panel function reads a name another panel binds locally, which parses and loads but renders an error on the panel that reads it, and that no panel function is left uncalled; exits non-zero on failure |
| `scripts/check_analysis_decomposition.R` | Regression check asserting that every stage of the analysis pipeline binds what it returns: that no stage returns a name bound only inside a conditional, which fails whenever that conditional does not fire, and that no stage returns a value nothing reads; exits non-zero on failure |
| `scripts/check_analysis_idempotence.R` | Regression check asserting that the analysis pipeline is idempotent: that two consecutive `analyse_run()` calls on one monitoring list return the same Role 4 census and write the same outputs, that `analyse_replications()` does the same including its jittered image, and that neither leaves the caller's random number stream advanced; exits non-zero on failure |
| `scripts/check_replication_independence.R` | Regression check asserting that `run_once()` is a pure function of its seed and that `run_replications()` draws a distinct seed per replication, the two properties that make replications independent; exits non-zero on failure |
| `scripts/check_config_restore.R` | Regression check asserting that an error raised inside a capacity sweep, a sensitivity screen or the scenario runner leaves `env_data`, `day_min` and `counts` at their pre-call values, and that `run_scenario()` restores them on its success path and leaves them unbound where they began unbound; exits non-zero on failure |
| `scripts/check_input_validation.R` | Regression check asserting that the analysis module's entry points (`analyse_run()`, `analyse_replications()` and the two capacity sweeps) and the Shiny console's configuration-loading boundary reject malformed input with a message naming the missing column or offending field, and accept what the model produces and ships; exits non-zero on failure |
| `scripts/check_screen_cache.R` | Regression check asserting that a sensitivity screen's design-point cache resumes what it recorded: a complete row round-trips, a partially-missing row reads as present with its gaps preserved, an all-missing row reads as absent, and an uncached point or a foreign cache reads as absent; exits non-zero on failure |
| `scripts/check_screen_order.R` | Regression check asserting that a sensitivity screen evaluates its design in index order and exactly once: that both drivers' designs and evaluation orders repeat at one control seed and differ at another, that each point's parameters match the design row it claims, that a resumed screen re-evaluates nothing, and that a response vector carries the names the drivers index it by; exits non-zero on failure |
| `scripts/check_measurement_reproducibility.R` | Regression check asserting that a multi-replication measurement is a function of its control seed alone: that it repeats at that seed, that it is unaffected by what preceded it in the session, that `run_replications()` restores the caller's generator kind and stream position, and that a replication reproduces from its seed on either dispatch path; exits non-zero on failure |
| `scripts/check_scenario_labels.R` | Regression check asserting that the comparative scenario plot renders in a C locale and is byte-identical to the same plot rendered under UTF-8; exits non-zero on failure |
| `scripts/check_arrival_rate_fidelity.R` | Regression check asserting that the generated arrival streams reproduce their configured rates and between-day variance, so a change to the arrival process cannot silently move the casualty count; exits non-zero on failure |
| `scripts/check_replication_loss_reporting.R` | Regression check asserting that a replication lost to its host is reported rather than dropped silently from the published count: a clean dispatch is unchanged, any loss stops the run under the shipped threshold of zero, a caller who raises the threshold gets a warning and the survivors within it, a loss beyond a raised threshold still stops, and the Welch figure's caption names the realised count it was given; exits non-zero on failure |
| `scripts/check_absent_attribute_columns.R` | Regression check asserting that an attribute no casualty set is an empty column rather than an absent one: `MODEL_ATTRIBUTE_KEYS` lists every key the model sets and no key it no longer sets, the wide pivot returns all of them from a monitor carrying one, and a run in which nobody returned to duty analyses end to end with the R2B holding summary empty rather than a series of zeroes; exits non-zero on failure |
| `scripts/check_bed_queue_coverage.R` | Regression check asserting that each echelon's bed queue figure covers every bed type that echelon fields, that each panel label counts the beds in the pool it names, and that the panels share one vertical scale rather than each filling its own; the coverage assertion is made against a bed type no configuration ships, injected into the monitor, so it defends the selection rule rather than today's establishment; exits non-zero on failure |
| `scripts/check_censored_interval_estimation.R` | Regression check asserting that an interval still open when the observation window closes is carried as the lower bound it is rather than dropped: the estimator degenerates to the arithmetic mean where nothing is censored, it recovers a known restricted mean from censoring injected into real stays, every casualty who entered an interval is counted in its `n`, the censored count and share are reported, no part of either dwell mean rests on the flat tail beyond the last observed completion, and a quantile the curve does not reach is reported as absent rather than as the same quantile of the casualties who finished; exits non-zero on failure |
| `scripts/check_hold_episode_reconstruction.R` | Regression check asserting that an R2B holding episode is bounded by the attribute its own exit route sets: an episode ended by return to duty ends at `return_day`, one ended by onward evacuation at the bed time served rather than at the `return_day` its casualty sets later at another echelon, and one still running when the window closes at the window's end rather than being dropped. The reconstructed bed-days are checked against the resource monitor, which counts bed occupancy directly, at the shipped configuration and under an evacuation threshold; exits non-zero on failure |
| `scripts/check_structure_tables.R` | Regression check asserting that the two tables claiming to index the codebase actually do: every `R/` module has a row in the README's Codebase Structure table and in this file's Repository Structure table, every `scripts/check_*.R` and every other `scripts/` entry point has a row here, and neither table names a path that no longer exists. The file set comes from `git ls-files`, so an untracked scratch file cannot fail it; exits non-zero on failure |
| `scripts/check_testthat.R` | Regression check running the Shiny console's `testthat` suite under `tests/testthat`: unit coverage of the console's helpers, and `shiny::testServer()` coverage of its reactive state machine; exits non-zero on any failing test |
| `tests/testthat/` | The console's R test suite, discovered and run by `scripts/check_testthat.R` and therefore gated per PR |
| `tests/playwright/`, `playwright.config.js`, `package.json`, `package-lock.json` | The console's browser test suite and the Node toolchain it needs, kept out of `renv.lock` so that no browser automation package enters the R dependency set. Run with `npx playwright test`; it starts the app itself and uses whatever Chromium `PLAYWRIGHT_BROWSERS_PATH` provides |
| `scripts/run_all_checks.R` | Regression check suite runner — discovers every `scripts/check_*.R` by glob, reports a pass/fail line and a runtime for each, and exits non-zero if any fails; `--fast` omits the checks too slow for a per-PR gate, `--slow` runs those alone, `--list` prints the classification, and `--jobs <n>`/`--jobs auto` runs that many checks at once, longest first, dividing the machine's cores between them |
| `scripts/check_runtimes.csv` | Measured runtime per check, which the runner dispatches longest-first from under `--jobs`. A scheduling hint alone: a stale entry costs wall clock and cannot change a result. `--refresh-runtimes` is the only way to write it |
| `scripts/check_lint.R` | Regression check asserting that no `lintr` rule in `.lintr`, and neither of the two machine-checkable rules `lintr` has no linter for (function length, pictographic characters in source), reports more findings than the count tracked in `scripts/lint_baseline.csv`; exits non-zero on a rise. `--refresh-baseline` is the only way to write the tracked counts |
| `scripts/check_baseline_reproduction.R` | Regression check asserting that the tracked seed-42 evidence set reproduces byte for byte, running the model at seed 42 for 30 days into a temporary directory and comparing `logs/logs.txt` and every `data/arrivals_*.txt` and `data/mass_casualty_events.csv` against it; exits non-zero on any difference |
| `scripts/check_roxygen.R` | Regression check asserting that no documentation rule of `docs/STYLE_GUIDE.md` that a parser can decide reports more findings than the count tracked in `scripts/roxygen_baseline.csv`: every named function carries a roxygen header opening with a title, an `@param` for each argument and no more, and a `@return` (R1, R2), and every file-scope constant carries a header (R3). Exits non-zero on a rise; `--list` prints every finding with its file, line and function name, and `--refresh-baseline` is the only way to write the tracked counts |
| `scripts/roxygen_baseline.csv` | The per-rule finding counts the roxygen ratchet defends |
| `.lintr`, `scripts/lint_baseline.csv` | The lint configuration encoding the `[lint]`-tagged rules of `docs/STYLE_GUIDE.md`, and the per-rule finding counts the ratchet defends |
| `.github/` | Pull request template mirroring the test plan structure below, and the GitHub Actions workflow running the fast suite, the lint ratchet, the seed-42 reproduction and the console's browser suite on every PR against `main`, in the pinned container |
| `scripts/README.md` | Verification baseline for the regression check suite — the result, runtime and observed behaviour of every `check_*.R` script, measured together in the pinned Dev Container |
| `scripts/check_pre_open_window.R` | Regression check asserting that a zero R2B pre-open hold window reproduces the instant-diversion model bit-for-bit, that `minutes_to_shift_open()` agrees with the roster, and that every casualty held forward is operated on there; exits non-zero on failure |
| `README.md` | System reference — introduction, literature review, methodology, codebase structure, trajectory logic, resource model, Mermaid diagrams, inline model assumptions, limitations, references. Does not contain simulation results. |
| `docs/Single_Run_Analysis.md` | Verification and behavioural walk-through of one seed-42, 30-day campaign under the Falklands-modified baseline, per echelon. Reports one run only: no confidence interval, replication count or non-default configuration belongs here |
| `docs/Multi_Run_Analysis.md` | Every replicated experiment, and the replication and interval methodology they rest on: the 50-replication/95% CI Falklands-modified vs. Okinawa-intensity scenario comparison, plus the R2B pre-open hold window, the post-operative intensive care gate, the forward ICU share frontier, the transport fleet-size sweep and the mass casualty stress test. The reinforcement comparison measures force generation rather than health system performance and is reported in `docs/Multi_Run_Supplement.md` instead. Organised by the planning decision each informs rather than by experiment: a diagnosis section locating the binding constraint, planning options in priority order, what resourcing alone cannot fix, the effects the replication counts leave unresolved, and a research agenda for the options the model cannot yet evaluate. Every option carries an evidence label (measured, direction only, unresolved, untested) and every experiment is retained beneath the decision it informs, stating its own design |
| `docs/Multi_Run_Supplement.md` | A standalone academic paper companion to `docs/Multi_Run_Analysis.md`, written to the same standard and carrying its own abstract, introduction, limitations, conclusion and independently-numbered reference list: the full experimental design of every replicated experiment, the replication-independence argument, the withdrawn antithetic pairing and its measurement, the interval construction and replication-count derivation, the warm-up classification, the checks that defend each of those properties, and the provenance of the comparative figures. Holds no finding about the performance of the trauma system; every measured result of that kind belongs to the paper. The one exception is the reinforcement comparison, which measures force generation rather than health system performance and is reported here in full. The paper is written to a ~5,000-word journal length and cites this document for the detail it no longer carries |
| `docs/BCH_Simulation_Action_Plan.md` | Issue tracker cross-reference — phase sequencing, dependency graph, merged-issue log |
| `docs/BCH_Task_Role_Allocation.md` | Task-role allocation design supplement for the not-yet-implemented individual resource modelling work (Issue #4) |
| `docs/Treatment_Flow_Configurability.md` | Design supplement evaluating how far the `env_data.json` parameter mechanism can be extended to let a user adapt the treatment flow itself, rather than only its magnitudes; sets out four options, their costs and their sequencing. Reports no simulation result and describes nothing yet implemented |
| `docs/Continuous_Integration.md` | Operating guide for the automated verification: what each GitHub Actions job runs and when, how to read a result, how to dispatch the slow suite, what each way the gate can fail calls for, and how a new check joins the suite |
| `docs/Getting_Started.md` | User guide for the Shiny console — the Configure/Run/Analyse workflow and how to read each output |
| `docs/Project_Status_Review.md` | Repository-wide status review — the findings and remediation plan the Phase 6 code-quality issues derive from |
| `docs/STYLE_GUIDE.md` | The R code standard — every rule a reviewer checks a PR against, each tagged machine-checkable, reviewer-applied or preference; follow at all times |
| `data/` | Read-only input data (arrival schedules) plus the tracked seed-42 diagnostic/event files (`arrivals_*.txt`, `mass_casualty_events.csv`) written by `R/environment.R`, rewritten only under `run.R --refresh-baseline` |
| `data/sensitivity/` | Tracked sensitivity evidence set — the Morris r=20 and Sobol N=200 design point caches, the per-response rankings, the decompositions, the noise floor measurement, and the estimator and separation re-analyses; roughly nineteen hours of computation, kept because every published index and rank derives from it and it cannot be regenerated cheaply |
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
4. Update the relevant document(s) — `README.md` and/or the `docs/` analysis documents — as part of the same PR (see README Maintenance below).
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

**Seed-42 baseline (30 days, single run):** <Include a table of changed metrics if the merge altered simulation outputs. Omit this block for documentation-only changes.>

**Unblocked by this merge:** <List newly unblocked issues, or "No new issues unblocked.">
```

**2. Update GitHub issue labels**

For each issue newly unblocked by the merge: change its label from `status: blocked` to `status: ready` using the GitHub MCP tools.

**3. Update `CLAUDE.md` baseline table (if simulation outputs changed)**

If the merged PR modified `R/trajectories.R`, `R/environment.R`, or `env_data.json` in a way that shifts the RNG stream or alters stochastic outputs, re-run the simulation at seed 42 and update the Key Parameters table at the bottom of this file. Document the change in the action plan entry.

The re-run must be invoked with the `--refresh-baseline` flag, which is the only way to write the tracked baseline evidence set (`images/`, `logs/logs.txt`, `data/arrivals_*.txt`, `data/mass_casualty_events.csv`):

```sh
Rscript run.R --seed 42 --days 30 --iterations 1 --refresh-baseline
```

Without the flag, every run writes to `outputs/` alone and leaves all tracked artifacts untouched, so an exploratory or smoke-test run cannot corrupt the baseline. The flag requires `--iterations 1` and errors otherwise, because the console log and the arrival diagnostics have no multi-replication equivalent; this is what guarantees the three tracked sets always describe the same single run. Commit them together, as one commit, or not at all: a PR that regenerates only part of the set reintroduces the drift Issue #154 closed.

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
4. **Regression checks** — confirm that outputs from unmodified pathways remain consistent with the baseline single-run (seed 42) values documented in `docs/Single_Run_Analysis.md` and this file's Key Parameters table.
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
**Regression:** Total casualty count per replication should fall within ±15% of seed 42 baseline (401 casualties)
```

---

## README Maintenance

The project's academic output is split across four documents (see [Academic Standards](#academic-standards) intro and the Repository Structure table above):

- **`README.md`** (system reference) — code structure, algorithms, trajectory logic, resource model, Mermaid diagrams, inline model assumptions, and Further Development. Contains no simulation results.
- **`docs/Single_Run_Analysis.md`** — the seed-42, 30-day verification and behavioural walk-through of one campaign under the Falklands-modified baseline. One run only: no confidence interval, replication count or non-default configuration belongs in it.
- **`docs/Multi_Run_Analysis.md`** — every replicated experiment, and the replication and interval methodology they rest on. The 50-replication/95% CI scenario comparison (Falklands-modified vs. Okinawa-intensity) is its centrepiece; the policy-lever sweeps, the mass casualty stress test and the reinforcement comparison sit alongside it, each stating its own design.
- **`docs/Multi_Run_Supplement.md`** — the standalone companion paper to the above: the full design of every replicated experiment, the replication framework and interval derivations they rest on, and the reinforcement comparison, which measures force generation rather than health system performance. No finding about the trauma system's performance belongs in it.

All four must be updated **as part of every PR that touches the section(s) they own** — not retrospectively. A PR that only changes code structure or trajectory logic updates `README.md` alone; a PR that changes seed-42 single-run findings updates `docs/Single_Run_Analysis.md`; a PR that changes any replicated finding, whether the scenario comparison or one of the sweeps and stress tests, updates `docs/Multi_Run_Analysis.md`, and a PR that changes the design of a replicated experiment, its replication count or the statistical basis of its intervals updates `docs/Multi_Run_Supplement.md`. The boundary between the two analysis documents is the unit of analysis, not the subject: a result from one run goes in the first, a result from many goes in the second, and a section that reports both belongs in the second with a cross-reference from the first. Cross-references between the four documents (`[text](../README.md#anchor)`, `[text](docs/Single_Run_Analysis.md#anchor)`, `[text](docs/Multi_Run_Analysis.md#anchor)` as appropriate to the source document's location) must stay valid — re-run `scripts/check_markdown.R` after moving or renaming any heading referenced from another document.

### What to update per PR

| Document | Section | Update trigger |
|---|---|---|
| `README.md` | Abstract | When the scope of the codebase or system reference changes materially |
| `README.md` | Simulation Design | When trajectories, resource logic, or distributions are changed |
| `README.md` | Further Development | When a gap is closed (delete the entry) or a new one is identified (add one, with a new identifier) |
| `README.md` | References | Add any new sources used in the implementation that `README.md` itself cites |
| `docs/Single_Run_Analysis.md` | Relevant echelon/domain section | When new seed-42 single-run results are generated (replace or supplement existing analysis) |
| `docs/Multi_Run_Analysis.md` | Relevant experiment section | When a sweep, stress test or before/after comparison is re-run at any replication count |
| `docs/Single_Run_Analysis.md` | References | Add any new sources this document itself cites |
| `docs/Multi_Run_Analysis.md` | Comparative Scenario Analysis | When new scenario-comparison results are generated (replace or supplement existing analysis) |
| `docs/Multi_Run_Analysis.md` | References | Add any new sources this document itself cites |
| `docs/Multi_Run_Supplement.md` | Relevant experiment design | When an experiment's replication count, seed, parameter overrides or invocation change |
| `docs/Multi_Run_Supplement.md` | Replication Framework | When the replication framework, the interval method or the warm-up classification changes |
| `docs/Multi_Run_Supplement.md` | References | Add any new sources this document itself cites |

Each document's References section lists only the sources that document itself cites, numbered in order of first appearance within that document — not a shared numbering scheme across all four. A source cited in more than one document is renumbered independently in each.

### Style

- Write in academic third-person prose. Avoid first person.
- **Write at a post-graduate research level that stays accessible to non-experts.** Use clear, plain prose and only standard dictionary words; do not coin non-standard terms (e.g. write "has not undergone surgery," not "unsurgicated").
- **Refer to people in the model as casualties, not "candidates."** "Candidate" is reserved for its other established uses in this project (a screened parameter, a scheduled day, a proposed intervention); a casualty being assessed or eligible for surgery is a "casualty requiring surgery" or "Priority N casualty," never a "surgical candidate" or "Priority N candidate."
- All parameters, probabilities, and distributions must be cited to their source.
- New methods introduced must reference the algorithm or statistical technique by name, with citation (e.g., "Morris Elementary Effects screening (Morris, 1991) was applied using R's `sensitivity` package").
- Tables and flowcharts must be kept synchronised with the code.
- **Do not use em dashes** in new or edited prose across `README.md`, `docs/Single_Run_Analysis.md`, `docs/Multi_Run_Analysis.md`, and `docs/Multi_Run_Supplement.md`. Use commas, parentheses, or semicolons instead.
- **Simulation Design narrative sections describe only the current design.** Trajectory logic, algorithm, and resource-model sections state how the model works now, with supporting evidence (citations, code function names, computed figures), not how it used to work or which issue changed it (e.g. no "prior to Issue #N..." or "as of Issue #N..." framing, and no issue-number suffix on section/heading titles). This does not apply to the Limitations section or `docs/BCH_Simulation_Action_Plan.md`, which are required elsewhere in this document to track which issue addressed or introduced a given item.
- **Mathematical notation** uses LaTeX delimiters exclusively (`$...$` inline, `$$...$$` for display formulas), never a code fence or plain text, for a formula or a mathematical variable (e.g. `$p_{max}$`, not `p_max` or *p_max*). An actual code, attribute, or `env_data.json` identifier (e.g. `` `dow_ceiling` ``, `` `p1_p_max` ``) is set in backticks, not math notation, even where its name coincides with a formula's symbol.
- **Figure captions** are written as ordinary prose immediately following the image, not as a separate italicised "*Figure: ...*" note.
- **Avoid duplicating content** already documented elsewhere in the same document, or, per the cross-reference rule above, in one of the other two documents; cross-reference the existing location instead of restating it. Every fact has exactly one home. The common failure is stating the same fact in two sections of the same document because both are about the thing it describes (for example a resource's concurrency limit appearing in both the roster section and the trajectory section). Put a fact where a reader would look for it first, and cross-reference from the other place.
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
- New references are appended to the References section of the document that cites them, in the order they first appear in that document's text. Each of `README.md`, `docs/Single_Run_Analysis.md`, `docs/Multi_Run_Analysis.md`, and `docs/Multi_Run_Supplement.md` maintains its own independently-numbered References section (see README Maintenance above) — a source cited in more than one document gets its own number in each.

### Reference List Rules

These rules apply to every entry in the References section of `README.md`, `docs/Single_Run_Analysis.md`, `docs/Multi_Run_Analysis.md`, and `docs/Multi_Run_Supplement.md`, and to references listed in GitHub Issues:

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

## Key Parameters (Current Baseline — Seed 42, 30-day run)

These are the validated baseline values from the current single-run analysis. Regression tests must check against these.

> **Provenance (sustained-campaign defaults, Issue #312).** Every seed-42 figure below was re-measured after three shipped defaults changed: `evacuation_policy_days` from 30 to 21, `force_regeneration.reinforcement.demand_interval_days` from 0 to 7, and `role4.ame.failure_probability` from 0.15 to 0. The third is the reason the strategic evacuation backlog no longer forms: all four scheduled sorties fly, all 193 evacuation decisions board, and none is left queued. README [Role 4 (National Support Base) Demand Modelling](README.md#role-4-national-support-base-demand-modelling) records why sortie reliability is set rather than simulated, and `docs/Multi_Run_Analysis.md`'s [Strategic Airlift Reliability Is Assumed, and the Assumption Is Load-Bearing](docs/Multi_Run_Analysis.md#strategic-airlift-reliability-is-assumed-and-the-assumption-is-load-bearing) measures what that assumption buys.
>
> **Three rows below are not yet re-measured at the new default and say so where they appear:** the pooled died-of-wounds calibrations, the Welch warm-up figures, and every 50-replication figure quoted alongside a seed-42 one. They were measured before `failure_probability` changed and are retained only because the direction of the change is known (less queueing, not more); none should be quoted as current until re-run. README [Return to Duty](README.md#return-to-duty) and [Force Regeneration and the Endogenous Feedback Loop](README.md#6-force-regeneration-and-the-endogenous-feedback-loop) record why each was changed.
>
> The per-row chains of superseded values that this table carried previously have been reset rather than extended. Those chains recorded how a figure moved as the code changed with the configuration held fixed, which made them a series a reader could follow. This refresh breaks that premise: every earlier figure was measured under different defaults, so placing it beside the current one in the same cell invites a comparison that means nothing about the model. The superseded values are not lost; `docs/BCH_Simulation_Action_Plan.md` is this project's designated home for resolution history and records them.
>
> The measurements were made in an unpinned R 4.3.3 sandbox rather than the pinned Dev Container. Before the defaults changed, `scripts/check_baseline_reproduction.R` reproduced the tracked evidence set byte for byte in this environment, so the sandbox is faithful for this model at this seed; a maintainer re-run in `rocker/rstudio:4.4.2` remains the standard before these figures are treated as canonical. All three died-of-wounds calibrations pass at the new defaults: `default` 0.464% (95% CI [0.397%, 0.531%]) and `moderate_intensity` 0.353% ([0.294%, 0.411%]) against the Ajax Bay bound of 0.46%, and `high_intensity` 3.498% ([3.378%, 3.618%]) against the Okinawa target of 3.40%, so no ceiling was re-fitted.

> **Provenance (canonical refresh, Issue #155).** This note records the canonical refresh that established the project's reproducibility, and the figures it reports have since been re-measured under the defaults the note above describes. Every figure it produced derived from one code state: the model code and configuration at commit `ed3c426`, which every run in this refresh was made from and which no later commit on this branch alters, the remainder of the branch touching documentation and regenerated artifacts only. All of it was produced in the project's pinned Dev Container, built from `.devcontainer/Dockerfile` on base image `rocker/rstudio@sha256:6bfc87fb66d0072e28d88d684a1f7b3e42a1c20360ee5eca5b43168a4eba3945`, so no row here carries a sandbox caveat and none is flagged as awaiting recomputation.
>
> This note replaces the twenty-one per-issue provenance caveats that stood here previously, covering Issues #76, #18, #23, #161, #154, #152, #156, #160, #159, #173, #178, #189, #153, #148, #146, #203, #208, #206, #150, #149 and #151. Those caveats recorded a drift the project had deliberately chosen to document rather than chase, each refresh having been made in an unpinned R 4.3.3 sandbox because no Docker was available to build the pinned container, and each accordingly stating that a maintainer re-run in `rocker/rstudio:4.4.2` was needed before its figures could be considered authoritative. That re-run is this one, and it resolves them in the affirmative: the seed-42 run in the pinned container reproduces the tracked baseline **byte for byte**, in `logs/logs.txt` and in all seven `data/arrivals_*.txt` diagnostics and `data/mass_casualty_events.csv` alike. The sandboxes were faithful, so no published seed-42 value moves in this refresh and the caveats are retired as correct rather than corrected. What this establishes is reproducibility across R 4.3.3 and R 4.4.2 for this model at this seed; it is evidence about the environments actually used, not a proof that no environment could diverge.
>
> The comparative scenario tables stand at 50 replications per profile: `moderate_intensity` 432.2 total casualties, 184.5 WIA, 1.06 DOW, 0.55% DOW/WIA, and `high_intensity` 1,050.4, 707.2, 23.80 and 3.35%, re-measured at the three defaults the note above describes. At the defaults in force for the Issue #155 refresh these read 437.8, 188.7, 0.78 and 0.42%, and 1,021.0, 684.3, 23.58 and 3.43%, and reproduced exactly in the pinned container, which is the reproducibility result this note was written to record. The measurements that do move are those whose generators had not been re-run since the arrival process was rebuilt, and they move because of that model change rather than because of the environment; each is identified where it appears.
>
> Two accommodations were needed to build the container in this session, both transport-only, and neither reaches any package version: the Ubuntu archive is addressed over TLS because the session's egress proxy refuses plain HTTP, and `renv` resolves the lockfile from the Posit Package Manager binary mirror for the same Ubuntu release rather than compiling each package from source. `renv.lock` remains the sole authority on versions, and the R version, the package versions and the system libraries are those the Dockerfile specifies. The tracked `.devcontainer/Dockerfile` is unmodified.

| Metric | Baseline value |
|---|---|
| Total casualties (30 days) | 495 |
| WIA (combat + support) | 212 (combat 116 / support 96) |
| KIA (combat + support) | 64 (combat 34 / support 30) |
| DNBI (combat + support) | 219 (combat 179 / support 40) |
| Effective force size, end of run (seed 42) | combat: 2407 of 2500 initial (−3.7%); support: 1208 of 1250 initial (−3.4%), under the shipped 7-day reinforcement cycle. The pool is held far nearer establishment than an unreinforced run leaves it, which is the mechanism's purpose |
| Force regeneration reinforcement mechanism | Enabled by default on a 7-day demand cycle (`demand_interval_days = 7`), matching the strategic aeromedical evacuation sortie interval — a planner-configured, not auto-balanced, demand/fulfillment-lag/triangular-fill model (not a fixed periodic size); see README [Force Regeneration and the Endogenous Feedback Loop](README.md#6-force-regeneration-and-the-endogenous-feedback-loop) for the mechanism and for why it ships enabled |
| Priority 1 share (seed 42) | 292 of 495 classified (59.0%) — P1 292 / P2 80 / P3 59 / KIA 64. Every generated casualty carries an assigned priority in this run |
| DOW count (seed 42) | 1 total (r2b=1). A single-run count at n≤4 carries no statistical weight; see the pooled multi-replication row below for the figure that does |
| DOW rate — P1 p_max (logistic) | 2.0% ceiling (Falklands 1982 calibration; re-fitted from 2.3% under Issue #203) |
| DOW rate — P2 p_max (logistic) | 1.6% ceiling (Falklands 1982 calibration; re-fitted from 1.9% under Issue #203) |
| DOW rate — P3 flat | 0.1% (structural placeholder; P3 never evacuated) |
| Mean DOW/run (150 reps pooled, NOT re-measured at `failure_probability` 0) | Treated-cohort DOW rate (casualties reaching R2B or R2E) 0.464% (95% CI [0.397%, 0.531%]) for the shipped `default` configuration, pooled across three independent 50-replication measurements returning 0.403%, 0.483% and 0.507%. This is the quantity the historical Ajax Bay bound of ~0.46% applies to, and the interval spans it. `moderate_intensity` measures 0.353% ([0.294%, 0.411%]) across 0.377%, 0.315% and 0.366%, and `high_intensity` 3.498% ([3.378%, 3.618%]) against the Okinawa target of 3.40%. `scripts/check_dow_calibration.R` passes for all three, so no ceiling was re-fitted for the defaults change. See README Further Development L22 |
| Replication count for mortality figures | The derivation, the counts each half-width requires and the resolution the 50-replication figures in this table carry are stated in `docs/Multi_Run_Analysis.md`'s [Replication Count and Resolution](docs/Multi_Run_Analysis.md#replication-count-and-resolution) |
| DNBI sub-types (seed 42) | battle_fatigue=48, disease=123, nbi=48 |
| bf_rtd (seed 42) | 44 |
| clinical_rtd (seed 42) | 128 (r1: 71, r2b: 41, r2e: 16) |
| total_rtd (seed 42) | 172, which is 34.7% of arrivals |
| Realised in-theatre share (seed 42) | 14.2% of 225 R2E dispositions retained in theatre under the shipped 21-day `evacuation_policy_days`. An output of the policy, not an input. The 50-replication figure is 11.6% (95% CI [10.9%, 12.2%]) over 179.4 dispositions per run, inside the 7.6%-42.1% historical range cited in README Return to Duty |
| Evacuation is severity-conditioned (seed 42) | Sorting the 225 dispositions into quartiles by drawn `recovery_to_duty_days` gives evacuation rates of 43.9% / 100% / 100% / 100% from shortest to longest recovery. An unconditioned draw would give the same rate in every quartile. The shorter 21-day policy lifts the first quartile's rate, more of the shortest-recovery casualties now falling beyond the threshold |
| Pre-flight critical ICU hold (seed 42) | 17 critical-route evacuees drew the ventilated pre-flight path (`critical_hold.ventilated_share = 0.15`), holding intensive care a mean of 24.9 h, median 22.2 h, p90 30.2 h. Pooled across 50 replications the hold measures 36.1 h mean, 24.6 h median, 38.6 h p90 over 631 ventilated holds. The mean sits above the median because a hold stretches only when the holding pool is full, which the 21-day policy makes rarer (README Further Development L17) |
| Welch ICU-queue CMA (10 reps × 90 days, NOT re-measured at `failure_probability` 0) | Final cross-replication CMA 0.342 with 14.2% of 2,159 increments decreasing, instantaneous mean queue peaking at 1.0. The queue roughly halves against the previous defaults, the 21-day evacuation policy freeing the holding beds that were blocking intensive care step-down. `WARM_UP_DAYS` remains 0; the basis for that classification at a sustained-campaign horizon is open work under Issue #312 |
| Surgical pathway split (seed 42) | 181 casualties operated on: 98 damage control (54.1%), 83 single-stage. By priority, Priority 1 147 operated / 88 damage control (59.9%, configured rate 55%), Priority 2 34 operated / 10 damage control (29.4%, configured 20%); no Priority 3 casualty reached theatre in this run. The 50-replication damage control share is 43.1% (95% CI [42.1%, 44.0%]) |
| All-damage-control equivalence (seed 42, post-Issue-173) | Setting `pri1_dcs_rate`, `pri2_dcs_rate` and `pri3_dcs_rate` to 1.0 reproduces the pre-Issue-173 model exactly, including the sum of arrival end times to fifteen significant figures. A degenerate rate of zero or one consumes no random draw, which is what makes the reproduction bit-identical rather than merely close |
| R2B surgical decision point (seed 42) | 253 casualties reached it (77 operated at R2B, 176 bypassed to R2E) |
| R2B surgeries (seed 42) | 77 |
| R2E surgeries — first op (seed 42) | 137 |
| R2E surgeries — second op (seed 42) | 55. Only a damage control casualty whose abbreviated operation was performed at R2E returns for a second procedure |
| R2B pre-transport bypass (seed 42) | at-R2B hold-full bypass: 2 — an independent count from the R2B surgical decision point above |
| R2B OT bypass reason (seed 42) | at-R2B subset: team off-shift 93, OT busy/queued 17 |
| R2B pre-open hold (seed 42) | 10 casualties held forward for a section about to reopen, mean hold 24.1 minutes, longest 44.8, against the shipped 60-minute `r2b.surgery.pre_open_window_min`. The 50-replication figure is 6.0 held per run (95% CI [5.3, 6.6]) |
| OT utilisation — echelon aggregate (seed 42) | R2B: 5.4%, R2E: 18.9% |
| R2B OT utilisation — 24h room (seed 42) | The 50-replication figures are T1 8.0% (95% CI [7.5%, 8.4%]) and T2 8.2% ([7.6%, 8.7%]) |
| R2B OT utilisation — shift time (seed 42) | Theatre occupancy divided by the time its surgical section is rostered. On an even two-shift day this is exactly twice the 24-hour room figure above, the pre-open hold's off-roster occupancy being counted in the numerator of both |
| Seed-42 per-resource utilisation window | Every per-resource utilisation and queue-share figure below is a fraction of the 30-day campaign window (43,200 minutes), which is also the convention `docs/Single_Run_Analysis.md` uses, so the two agree row for row. The echelon aggregates written to `outputs/ot_utilisation.csv` divide by the full observation window instead, which runs past day 30 while the Role 4 census decays, and are correspondingly lower |
| R2E OT utilisation — 24h room (seed 42) | OT1: 53.7%, OT2: 37.3%. A theatre is seized before its surgical section, so a room reads as occupied while its casualty is still queued for staff; much of this is that wait, not operating time |
| R2E OT queue ≥1 (seed 42) | OT1: 27.2% of run, OT2: 20.2% |
| R2E surgical section utilisation (seed 42) | Section 1: 26.8%, Section 2: 50.3%, Section 3: 24.4% of the time their own rosters had them open, and queued for 13.7%, 41.9% and 12.7% of that same open time. Section 2 remains the busiest on both measures, being the section rostered to the second shift and so carrying the whole night-time surgical load alone |
| R2E ICU utilisation — mean (seed 42) | ICU1: 93.7%, ICU2: 93.0%, ICU3: 90.5%, ICU4: 84.0%, the four-bed pool holding a mean of 3.61 of its 4 beds. The 50-replication four-bed mean is 86.3% (95% CI [84.9%, 87.6%]) |
| R2E ICU queue ≥1 (seed 42) | ICU1: 35.1% of run, ICU2: 7.7%, ICU3: 3.5%, ICU4: 0% |
| Transport utilisation — platform aggregate (seed 42) | HX240M: 4.7%, PMVAmb: 14.3% |
| PMV Ambulance utilisation (seed 42) | 14.3%; per-vehicle 27.6% / 11.9% / 3.5%, queued for 0.04% of the run on the first vehicle |
| HX240M utilisation (seed 42) | 3.8%; per-vehicle 9.0% / 3.0% / 1.8% / 1.2%, queued for 0.70% of the run on the first vehicle — carries R2B→R2E mortuary road-move traffic in addition to R1→mortuary KIA |
| R2B evac team dead-heading (seed 42, Issue #73 follow-up) | R2B→R2E WIA transport models a dead-heading return leg on the R2B team's own organic evac resource (`r2b_evac_leg()`/`r2b_evac_return_leg()`), matching the R1↔R2B legs; RNG-stream-shifting, not RNG-neutral |
| R2B→R2E mortuary transport (seed 42, Issue #73 follow-up) | R2B KIA/DOW transported by road to the R2E-collocated mortuary via the shared HX2 40M fleet (`r2b_transport_kia()`, dead-heading return leg), then handed to a selected R2E team's mortuary intake (`r2e_mortuary_intake()`) |
| R2E post-op pathway — stabilisation (seed 42) | icu=74, hold=57, so 56% of the stabilisation cohort reaches an intensive care bed. The 50-replication stabilisation ICU share is 60.2% (95% CI [57.7%, 62.7%]). `surgery_deferred` = 17; the 50-replication figure is 15.5 per run (95% CI [12.9, 18.1]) |
| R2E post-definitive care pathway (seed 42) | icu=56, hold=114 — 33% of operated casualties receive post-definitive care in an ICU bed, the remainder taking the degraded holding-bed fallback. The 50-replication ICU share is 39.3% (95% CI [36.3%, 42.3%]) |
| R2E post-op DOW rate — icu vs hold (seed 42) | 0/74 vs 0/57 (single-run). The saturated-ICU 90-day stress test from Issue #43, which confirmed the mechanism fires with the hold pathway riskier than the icu pathway, has not been re-run since |
| Role 4 demand (seed 42) | 193 strategic evacuation decisions (125 critical route, 68 standard); all 193 boarded and reached Role 4 by day 30, none left queued at R2E; Role 4 peak occupancy 120.0 concurrent patients on day 30, the campaign's last day |
| Strategic AME actual performance (seed 42) | C-17A Globemaster III at 36 critical / 54 standard places; all four scheduled sorties flew, `failure_probability` shipping at 0, and 193 evacuated at a mean wait of 1.0 days (p10-p90 0.0-4.0). Nothing is left queued, so the strategic evacuation backlog that earlier baselines carried does not form at all |
| AME wait-time DOW poll (seed 42) | `dow_echelon=5`, daily poll interval (`role4.ame.dow_check_interval = 1440` min); 0 deaths observed, as in every run since the poll was added; see README [AME Wait Checkpoint](README.md#ame-wait-checkpoint) for why no single-run count should be read as evidence about the mechanism's magnitude |

---

## Out of Scope for Claude

- Merging to `main` — owner only.
- Changing the casualty rate baseline scenario without raising and discussing an issue first.
- Modifying `env_data.json` schema without a corresponding issue and PR.
- Removing or replacing existing references in `README.md`, `docs/Single_Run_Analysis.md`, `docs/Multi_Run_Analysis.md`, or `docs/Multi_Run_Supplement.md` without explicit instruction.
