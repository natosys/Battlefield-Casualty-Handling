# CLAUDE.md — Battlefield Casualty Handling Simulation

## Project Purpose

This is an **academic research project** producing a Discrete Event Simulation (DES) of deployed battlefield casualty handling. The simulation is written in R using the `simmer` package and is intended to provide evidence-based options to military planners for improving health outcomes in Large Scale Combat Operations (LSCO).

All work must meet academic research standards: reasoning must be explicit, sources must be cited, and limitations must be acknowledged. The project's academic output is split across three documents, each kept current with the code and written to the standard of a published academic paper: `README.md` (system reference — code structure, algorithms, trajectory logic, resource model, inline model assumptions, and Limitations), `docs/Single_Run_Analysis.md` (the seed-42 verification and behavioural walk-through of one campaign), and `docs/Multi_Run_Analysis.md` (every replicated experiment, the n≥30/95% CI scenario comparison among them). See [README Maintenance](#readme-maintenance) below for which PR types update which document.

---

## Repository Structure

The codebase is organised into a modular layout under `R/`, with `run.R` as the single CLI entry point. See the README's [Codebase Structure](README.md#codebase-structure) table for full detail on each `R/` module; this table covers the repository as a whole.

| File / Directory | Purpose |
|---|---|
| `run.R` | CLI entry point — parses arguments, orchestrates modules, and writes outputs |
| `R/constants.R` | Values shared across modules, `DAY_MIN` (minutes per simulated day) among them. Sourced by each module that needs one rather than by one module on every other's behalf, the modules under `R/` being otherwise independent |
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
| `scripts/run_warmup.R` | CLI entry point for Welch warm-up analysis |
| `scripts/run_scenarios.R` | CLI entry point for the comparative scenario runner |
| `scripts/render_dow_survival.R` | Renders `images/dow_survival_function.png` from the `dow.params` block of `env_data.json`, for the base configuration or a `--scenario` profile, so a re-fitted `p_max` cannot leave the figure disagreeing with the calibration table beneath it; `--refresh-baseline` is the only way to write the tracked image |
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
| `scripts/check_markdown.R` | Maintains the TOC and "Return to Top" links across `README.md`, `docs/Single_Run_Analysis.md`, and `docs/Multi_Run_Analysis.md`, generating each anchor as GitHub does, and asserting that its own entry-heading match is byte-wise so the check does not depend on the session locale; exits non-zero if any anchor link points at no heading, if any local link or image target does not exist when resolved relative to the document containing it, if any image carries placeholder or empty alt text, or if a row of the README's Further Development scan table names a gap or an impact differently from the entry it points at. The link, target and alt-text checks run across every tracked markdown document, including this one and `docs/BCH_Simulation_Action_Plan.md` (which carry no TOC block and must not be given one); the scan table check applies to `README.md` alone. External URLs are out of scope |
| `scripts/check_references.R` | Regression check asserting that each of the three academic documents' reference lists is sound: every `[[n]]` citation resolves to an entry, every entry is cited at least once, the list is numbered from one in order of first appearance, no two entries share a URL, and every entry carries a URL and a retrieval date. Whether a URL is open access is a judgement the script cannot make and remains a manual step at the point a reference is added; exits non-zero on failure |
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
| `docs/Multi_Run_Analysis.md` | Every replicated experiment, and the replication and interval methodology they rest on: the 50-replication/95% CI Falklands-modified vs. Okinawa-intensity scenario comparison, plus the R2B pre-open hold window, the post-operative intensive care gate, the forward ICU share frontier, the transport fleet-size sweep, the reinforcement comparison and the mass casualty stress test. Organised by the planning decision each informs rather than by experiment: a diagnosis section locating the binding constraint, planning options in priority order, what resourcing alone cannot fix, the effects the replication counts leave unresolved, and a research agenda for the options the model cannot yet evaluate. Every option carries an evidence label (measured, direction only, unresolved, untested) and every experiment is retained beneath the decision it informs, stating its own design |
| `docs/BCH_Simulation_Action_Plan.md` | Issue tracker cross-reference — phase sequencing, dependency graph, merged-issue log |
| `docs/BCH_Task_Role_Allocation.md` | Task-role allocation design supplement for the not-yet-implemented individual resource modelling work (Issue #4) |
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

The project's academic output is split across three documents (see [Academic Standards](#academic-standards) intro and the Repository Structure table above):

- **`README.md`** (system reference) — code structure, algorithms, trajectory logic, resource model, Mermaid diagrams, inline model assumptions, and Further Development. Contains no simulation results.
- **`docs/Single_Run_Analysis.md`** — the seed-42, 30-day verification and behavioural walk-through of one campaign under the Falklands-modified baseline. One run only: no confidence interval, replication count or non-default configuration belongs in it.
- **`docs/Multi_Run_Analysis.md`** — every replicated experiment, and the replication and interval methodology they rest on. The 50-replication/95% CI scenario comparison (Falklands-modified vs. Okinawa-intensity) is its centrepiece; the policy-lever sweeps, the mass casualty stress test and the reinforcement comparison sit alongside it, each stating its own design.

All three must be updated **as part of every PR that touches the section(s) they own** — not retrospectively. A PR that only changes code structure or trajectory logic updates `README.md` alone; a PR that changes seed-42 single-run findings updates `docs/Single_Run_Analysis.md`; a PR that changes any replicated finding, whether the scenario comparison or one of the sweeps and stress tests, updates `docs/Multi_Run_Analysis.md`. The boundary between the two analysis documents is the unit of analysis, not the subject: a result from one run goes in the first, a result from many goes in the second, and a section that reports both belongs in the second with a cross-reference from the first. Cross-references between the three documents (`[text](../README.md#anchor)`, `[text](docs/Single_Run_Analysis.md#anchor)`, `[text](docs/Multi_Run_Analysis.md#anchor)` as appropriate to the source document's location) must stay valid — re-run `scripts/check_markdown.R` after moving or renaming any heading referenced from another document.

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

Each document's References section lists only the sources that document itself cites, numbered in order of first appearance within that document — not a shared numbering scheme across all three. A source cited in more than one document is renumbered independently in each.

### Style

- Write in academic third-person prose. Avoid first person.
- **Write at a post-graduate research level that stays accessible to non-experts.** Use clear, plain prose and only standard dictionary words; do not coin non-standard terms (e.g. write "has not undergone surgery," not "unsurgicated").
- **Refer to people in the model as casualties, not "candidates."** "Candidate" is reserved for its other established uses in this project (a screened parameter, a scheduled day, a proposed intervention); a casualty being assessed or eligible for surgery is a "casualty requiring surgery" or "Priority N casualty," never a "surgical candidate" or "Priority N candidate."
- All parameters, probabilities, and distributions must be cited to their source.
- New methods introduced must reference the algorithm or statistical technique by name, with citation (e.g., "Morris Elementary Effects screening (Morris, 1991) was applied using R's `sensitivity` package").
- Tables and flowcharts must be kept synchronised with the code.
- **Do not use em dashes** in new or edited prose across `README.md`, `docs/Single_Run_Analysis.md`, and `docs/Multi_Run_Analysis.md`. Use commas, parentheses, or semicolons instead.
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
- New references are appended to the References section of the document that cites them, in the order they first appear in that document's text. Each of `README.md`, `docs/Single_Run_Analysis.md`, and `docs/Multi_Run_Analysis.md` maintains its own independently-numbered References section (see README Maintenance above) — a source cited in more than one document gets its own number in each.

### Reference List Rules

These rules apply to every entry in the References section of `README.md`, `docs/Single_Run_Analysis.md`, and `docs/Multi_Run_Analysis.md`, and to references listed in GitHub Issues:

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

> **Provenance (canonical refresh, Issue #155).** Every figure in the table below, and every figure, table and plot across `README.md`, `docs/Single_Run_Analysis.md` and `docs/Multi_Run_Analysis.md`, derives from one code state: the model code and configuration at commit `ed3c426`, which every run in this refresh was made from and which no later commit on this branch alters, the remainder of the branch touching documentation and regenerated artifacts only. All of it was produced in the project's pinned Dev Container, built from `.devcontainer/Dockerfile` on base image `rocker/rstudio@sha256:6bfc87fb66d0072e28d88d684a1f7b3e42a1c20360ee5eca5b43168a4eba3945`, so no row here carries a sandbox caveat and none is flagged as awaiting recomputation.
>
> This note replaces the twenty-one per-issue provenance caveats that stood here previously, covering Issues #76, #18, #23, #161, #154, #152, #156, #160, #159, #173, #178, #189, #153, #148, #146, #203, #208, #206, #150, #149 and #151. Those caveats recorded a drift the project had deliberately chosen to document rather than chase, each refresh having been made in an unpinned R 4.3.3 sandbox because no Docker was available to build the pinned container, and each accordingly stating that a maintainer re-run in `rocker/rstudio:4.4.2` was needed before its figures could be considered authoritative. That re-run is this one, and it resolves them in the affirmative: the seed-42 run in the pinned container reproduces the tracked baseline **byte for byte**, in `logs/logs.txt` and in all seven `data/arrivals_*.txt` diagnostics and `data/mass_casualty_events.csv` alike. The sandboxes were faithful, so no published seed-42 value moves in this refresh and the caveats are retired as correct rather than corrected. What this establishes is reproducibility across R 4.3.3 and R 4.4.2 for this model at this seed; it is evidence about the environments actually used, not a proof that no environment could diverge.
>
> The comparative scenario tables reproduce exactly as well, at 50 replications per profile: `moderate_intensity` 437.8 total casualties, 188.7 WIA, 0.78 DOW, 0.42% DOW/WIA, and `high_intensity` 1,021.0, 684.3, 23.58 and 3.43%, with every queue group matching to the precision published. The measurements that do move are those whose generators had not been re-run since the arrival process was rebuilt, and they move because of that model change rather than because of the environment; each is identified where it appears.
>
> Two accommodations were needed to build the container in this session, both transport-only, and neither reaches any package version: the Ubuntu archive is addressed over TLS because the session's egress proxy refuses plain HTTP, and `renv` resolves the lockfile from the Posit Package Manager binary mirror for the same Ubuntu release rather than compiling each package from source. `renv.lock` remains the sole authority on versions, and the R version, the package versions and the system libraries are those the Dockerfile specifies. The tracked `.devcontainer/Dockerfile` is unmodified.

| Metric | Baseline value |
|---|---|
| Total casualties (30 days) | 530 (post-Issue-206; was 437 post-Issue-203, 382 post-Issue-148 and post-Issue-146, 386 post-Issue-173, 385 post-Issue-159, 386 post-Issue-160, 387 post-Issue-161, 400 pre-Issue-18). The configured means are unchanged, so this is one draw from a distribution that is now far wider rather than a shift in the rate |
| WIA (combat + support) | 287 (post-Issue-206; combat 218 / support 69; was 187 post-Issue-203, 151 post-Issue-148 and post-Issue-146, 149 post-Issue-173, 148 post-Issue-159, 149 post-Issue-160, 150 post-Issue-161, 154 pre-Issue-18). The WIA streams carry the highest coefficient of variation of any shipped stream, at 2.01, so they are the streams the restored between-day variance moves furthest in either direction |
| KIA (combat + support) | 72 (post-Issue-206; combat 50 / support 22; was 71 post-Issue-203, 57 post-Issue-148 and post-Issue-146, 67 post-Issue-173, post-Issue-159, post-Issue-160 and post-Issue-161, 70 pre-Issue-18) |
| DNBI (combat + support) | 171 (post-Issue-206; combat 138 / support 33; was 179 post-Issue-203, 174 post-Issue-148 and post-Issue-146, 170 post-Issue-173, post-Issue-159, post-Issue-160 and post-Issue-161, 176 pre-Issue-18). Falls at this seed while the others rise, which is what an unchanged mean and a wider spread produce |
| Effective force size, end of run (seed 42, post-Issue-206) | combat: 2225 of 2500 initial (−11.0%); support: 1162 of 1250 initial (−7.0%); no reinforcement (`force_regeneration.reinforcement.demand_interval_days = 0`, the shipped default). Was combat 2300 / support 1162 post-Issue-203 |
| Force regeneration reinforcement mechanism | Disabled by default (`demand_interval_days = 0`) — a planner-configured, not auto-balanced, demand/fulfillment-lag/triangular-fill model (not a fixed periodic size); see README [Force Regeneration and the Endogenous Feedback Loop](README.md#6-force-regeneration-and-the-endogenous-feedback-loop) for a `high_intensity`-scenario demonstration of the mechanism under both no-reinforcement and reinforcement-enabled configurations |
| Priority 1 share (seed 42, post-Issue-206) | 280 of 530 classified (52.8%) — P1 280 / P2 110 / P3 68 / KIA 72; was P1 229 / P2 85 / P3 52 / KIA 71 post-Issue-203. Every generated casualty carries an assigned priority in this run |
| DOW count (seed 42, post-Issue-206) | 4 total (r2b=3, r2e=1); was 1 total (r2b=1) post-Issue-203. At n≤4 a change of three carries no statistical weight; see the pooled multi-replication row below for the figure that does |
| DOW rate — P1 p_max (logistic) | 2.0% ceiling (Falklands 1982 calibration; re-fitted from 2.3% under Issue #203) |
| DOW rate — P2 p_max (logistic) | 1.6% ceiling (Falklands 1982 calibration; re-fitted from 1.9% under Issue #203) |
| DOW rate — P3 flat | 0.1% (structural placeholder; P3 never evacuated) |
| Mean DOW/run (150 reps pooled, post-Issue-206) | Treated-cohort DOW rate (casualties reaching R2B or R2E) 0.474% (95% CI [0.412%, 0.536%]) at the Issue #203 ceilings, pooled across three independent 50-replication measurements; the three returned 0.387%, 0.519% and 0.516% individually. This is the quantity the historical Ajax Bay bound of ~0.46% applies to, and the interval spans it where it previously sat below it. `moderate_intensity` measures 0.368% (95% CI [0.310%, 0.426%]) across 0.292%, 0.382% and 0.430%. `scripts/check_dow_calibration.R` passes for both against the one-sided bound, so neither ceiling was re-fitted for this issue. Was 0.417% ([0.354%, 0.480%]) and 0.353% ([0.293%, 0.413%]) post-Issue-208. The two intervals overlap, so 150 replications separate each profile from the bound but not the profiles from each other. DOW/WIA rate is not pooled at 150 replications, the calibration check not reporting it; see the 50-replication comparative measurement in `docs/Multi_Run_Analysis.md`. See README Further Development L22 |
| Replication count for mortality figures (post-Issue-206) | Per-replication sd of the treated-cohort DOW rate is 0.0039 (base, 150 reps); the derivation, the counts each half-width requires and the resolution the 50-replication figures in this table carry are stated in `docs/Multi_Run_Analysis.md`'s [Replication Count and Resolution](docs/Multi_Run_Analysis.md#replication-count-and-resolution) |
| DNBI sub-types (seed 42, post-Issue-206) | battle_fatigue=46, disease=93, nbi=32 (post-Issue-206; was battle_fatigue=42, disease=108, nbi=29 post-Issue-203) |
| bf_rtd (seed 42, post-Issue-206) | 44 (post-Issue-206; was 41 post-Issue-203), tracking the rise in the battle fatigue sub-type count above |
| clinical_rtd (seed 42, post-Issue-206) | 123 (r1: 78, r2b: 42, r2e: 3) (post-Issue-206; was 108, split 52 / 41 / 15, post-Issue-203). The R2E component collapses as the larger operated cohort's recoveries extend past the run's end |
| total_rtd (seed 42, post-Issue-206) | 167 (post-Issue-206; was 149 post-Issue-203). As a share of arrivals this falls, 34.1% to 31.5%, the larger casualty count including more casualties whose recovery extends past the 30-day window |
| Realised in-theatre share (seed 42, post-Issue-206) | 23.3% of 176 R2E dispositions retained in theatre under the shipped 30-day `evacuation_policy_days`. An output of the policy, not an input. Was 26.8% of 179 dispositions post-Issue-203. The 50-replication figure is 27.6% (95% CI [26.1%, 29.1%]) over 158.4 dispositions per run, inside the 7.6%-42.1% historical range cited in README Return to Duty |
| Evacuation is severity-conditioned (seed 42, post-Issue-206) | Sorting the 176 dispositions into quartiles by drawn `recovery_to_duty_days` gives evacuation rates of 6.8% / 100% / 100% / 100% from shortest to longest recovery; was 0% / 93.3% / 100% / 100% post-Issue-203. An unconditioned draw would give the same rate in every quartile. The 50-replication figures are 0.0% / 90.9% / 100% / 100% |
| Pre-flight critical ICU hold (seed 42, post-Issue-206) | 8 critical-route evacuees drew the ventilated pre-flight path (`critical_hold.ventilated_share = 0.15`); 4 completed within the run at a mean of 109.4 h, median 102.8 h, p90 147 h. Was 12 with 11 completed at a mean of 28.0 h post-Issue-203. The hold stretches because a ventilated casualty cannot step down while the holding pool is full, which the two cancelled sorties at this seed made worse (README Further Development L17). Pooled across 50 replications the hold measures 50.5 h mean, 26.2 h median, 104.6 h p90 over 439 ventilated holds, the mean sitting well above the median because a hold stretches only when the holding pool is full |
| Welch ICU-queue CMA (10 reps × 90 days, post-Issue-155) | Final cross-replication CMA 0.567 with 14.7% of 2,159 increments decreasing, instantaneous mean queue peaking at 2.0; was 0.291 with 24.8% decreasing and a 0.90 peak post-Issue-173. Re-measured in the pinned Dev Container, this row having previously predated the mean-relative lognormal cap and the arrival-process rebuild alike; the queue roughly doubles because a heavy day now forms an intensive care queue that a flattened arrival process never produced |
| Surgical pathway split (seed 42, post-Issue-206) | 210 casualties operated on: 93 damage control (44.3%), 117 single-stage. By priority, Priority 1 154 operated / 82 damage control (53.2%, configured rate 55%), Priority 2 56 operated / 11 damage control (19.6%, configured 20%); no Priority 3 casualty reached theatre in this run. Was 155 operated, 72 damage control (46.5%) post-Issue-203. The 50-replication share is 43.3% (95% CI [42.0%, 44.5%]) |
| All-damage-control equivalence (seed 42, post-Issue-173) | Setting `pri1_dcs_rate`, `pri2_dcs_rate` and `pri3_dcs_rate` to 1.0 reproduces the pre-Issue-173 model exactly, including the sum of arrival end times to fifteen significant figures. A degenerate rate of zero or one consumes no random draw, which is what makes the reproduction bit-identical rather than merely close |
| R2B surgical decision point (seed 42, post-Issue-206) | 210 casualties reached it (69 operated at R2B, 141 bypassed to R2E); was 152 (74 operated, 78 bypassed) post-Issue-203. Forward surgeries fall while the caseload rises by half, the single forward theatre and its one rostered section being saturated at the peaks the restored variance produces |
| R2B surgeries (seed 42, post-Issue-206) | 69 (post-Issue-206; was 74 post-Issue-203) |
| R2E surgeries — first op (seed 42, post-Issue-206) | 171 (post-Issue-206; was 115 post-Issue-203) |
| R2E surgeries — second op (seed 42, post-Issue-206) | 41 (post-Issue-206; was 31 post-Issue-203). Only a damage control casualty whose abbreviated operation was performed at R2E returns for a second procedure, so this stays at roughly the damage control share of casualties not operated on forward |
| R2B pre-transport bypass (seed 42, post-Issue-206) | upstream R1-threshold bypass: 179 (was 135 post-Issue-203); at-R2B hold-full bypass: 3 (was 1); R2B hold queue (both full): 1 (was 0) — these are three independent, non-summing counts |
| R2B OT bypass reason (seed 42, post-Issue-206) | at-R2B subset, 141 total (was 78 post-Issue-203): team off-shift 100 (was 58), OT busy/queued 41 (was 20). The off-shift share falls from 74% to 71% as the peaks press harder on the single forward theatre |
| R2B pre-open hold (seed 42, post-Issue-206) | 7 casualties held forward for a section about to reopen, all 7 operated on within the run, mean hold 35.2 minutes, longest 58.9, against the shipped 60-minute `r2b.surgery.pre_open_window_min`. Was 10 held at a mean of 22.4 minutes post-Issue-203. The 50-replication figure is 5.9 held per run (95% CI [5.2, 6.6]) |
| OT utilisation — echelon aggregate (seed 42, post-Issue-206) | R2B: 5.2%, R2E: 24.2% (was R2B: 6.1%, R2E: 11.4% post-Issue-203) |
| R2B OT utilisation — 24h room (seed 42, post-Issue-206) | T1: 9.7%, T2: 9.6% (was T1: 8.3%, T2: 11.3% post-Issue-203). The 50-replication figures are T1 7.4% (95% CI [7.0%, 7.8%]) and T2 7.1% ([6.6%, 7.5%]) |
| R2B OT utilisation — shift time (seed 42, post-Issue-206) | T1: 19.3%, T2: 19.1% — theatre occupancy divided by the time its surgical section is rostered. Was T1: 16.6%, T2: 22.5% post-Issue-203. On an even two-shift day this is exactly twice the 24-hour room figure above, the pre-open hold's off-roster occupancy being counted in the numerator of both |
| R2E OT utilisation — 24h room (seed 42, post-Issue-206) | OT1: 66.6%, OT2: 52.8% (was OT1: 30.9%, OT2: 15.4% post-Issue-203). A theatre is seized before its surgical section, so a room reads as occupied while its casualty is still queued for staff; much of this is that wait, not operating time |
| R2E OT queue ≥1 (seed 42, post-Issue-206) | OT1: 46.3% of run, OT2: 34.6% (was OT1: 3.0%, OT2: 0.6% post-Issue-203). The largest single movement in this refresh, and the clearest illustration of what the retired generator was suppressing: theatre contention is driven by peak-day volume rather than by mean volume |
| R2E surgical section utilisation (seed 42, post-Issue-206) | Section 1: 30.8%, Section 2: 53.6%, Section 3: 30.8% of the time their own rosters had them open, queued for 0.67%, 2.45% and 0.60% of it; was 18.2/38.9/19.0 queued 0/1.04/0 post-Issue-203. Section 2 remains the busiest, being the section rostered to the second shift and so carrying the whole night-time surgical load alone |
| R2E ICU utilisation — mean (seed 42, post-Issue-206) | ICU1: 93.7%, ICU2: 92.1%, ICU3: 92.2%, ICU4: 85.8% (was ICU1: 97.3%, ICU2: 92.2%, ICU3: 88.1%, ICU4: 82.0% post-Issue-203). The load spreads across the four beds rather than rising, the first bed's occupancy falling as the later ones take more. The 50-replication four-bed mean is 85.8% (95% CI [84.3%, 87.4%]) |
| R2E ICU queue ≥1 (seed 42, post-Issue-206) | ICU1: 26.5% of run, ICU2: 13.1%, ICU3: 13.5%, ICU4: 7.8% (was ICU1: 60.9%, ICU2: 8.1%, ICU3: 0%, ICU4: 0% post-Issue-203). The queue spreads across all four beds where it previously concentrated behind the first |
| Transport utilisation — platform aggregate (seed 42, post-Issue-206) | HX240M: 5.1%, PMVAmb: 14.9% (was HX240M: 5.0%, PMVAmb: 12.3% post-Issue-203) |
| PMV Ambulance utilisation (seed 42, post-Issue-206) | 14.9%; max queue 2, queued for 1.4% of the run on the first vehicle; per-vehicle 28.5% / 12.1% / 4.1% (was 28.0% / 7.9% / 1.0% at max queue 0 post-Issue-203). The first transport queue the model has produced at the shipped fleet size, which is what the fleet-size sweep should now be re-run against (README Further Development L19) |
| HX240M utilisation (seed 42, post-Issue-206) | 5.1%; max queue 0 throughout run; per-vehicle 9.0% / 1.1%, the remaining two vehicles idle (was 9.0% / 1.0% / 0.0% post-Issue-203) — carries R2B→R2E mortuary road-move traffic in addition to R1→mortuary KIA |
| R2B evac team dead-heading (seed 42, Issue #73 follow-up) | R2B→R2E WIA transport models a dead-heading return leg on the R2B team's own organic evac resource (`r2b_evac_leg()`/`r2b_evac_return_leg()`), matching the R1↔R2B legs; RNG-stream-shifting, not RNG-neutral |
| R2B→R2E mortuary transport (seed 42, Issue #73 follow-up) | R2B KIA/DOW transported by road to the R2E-collocated mortuary via the shared HX2 40M fleet (`r2b_transport_kia()`, dead-heading return leg), then handed to a selected R2E team's mortuary intake (`r2e_mortuary_intake()`) |
| R2E post-op pathway — stabilisation (seed 42, post-Issue-206) | icu=79, hold=58 (post-Issue-206; was icu=47, hold=48 post-Issue-203). The degraded share falls from 51% to 42%, against a 50-replication stabilisation ICU share of 53.1% (95% CI [50.3%, 55.9%]). `surgery_deferred` = 29 (was 15 post-Issue-203); the 50-replication figure is 15.6 per run (95% CI [13.7, 17.6]) |
| R2E post-definitive care pathway (seed 42, post-Issue-206) | icu=54, hold=130 — 29% of operated casualties receive post-definitive care in an ICU bed, down from 38% post-Issue-203, the remainder taking the degraded holding-bed fallback. The 50-replication ICU share is 36.5% (95% CI [33.1%, 39.9%]). Four ICU beds cover a smaller share of the requirement as peak-day volume rises |
| R2E post-op DOW rate — icu vs hold (seed 42, post-Issue-206) | 0/79 vs 0/58 (single-run; was 0/47 vs 0/48 post-Issue-203). The saturated-ICU 90-day stress test from Issue #43, which confirmed the mechanism fires with the hold pathway riskier than the icu pathway, has not been re-run since |
| Role 4 demand (seed 42, post-Issue-206) | 135 strategic evacuation decisions (85 critical route, 50 standard); 99 boarded and reached Role 4 by day 30, 36 still queued at R2E; Role 4 peak occupancy 90.0 concurrent patients (day 30, the campaign's last day, after which the census turns over and decays to near zero by day 69 as the length-of-stay draws run out); unconstrained-baseline demand would need 27 sorties. Was 131 decisions, 114 boarded, 17 queued, peak 76.0 on day 21 post-Issue-203 |
| Strategic AME actual performance (seed 42, post-Issue-206) | C-17A Globemaster III at 36 critical / 54 standard places; 99 evacuated at a mean wait of 10.1 days (p10-p90 1.7-18.0). Two of the four scheduled sorties drew a cancellation (`role4.ame.failure_probability`), so the first sortie to fly departed on day 21 and boarded 36 critical and 33 standard, the second on day 28 boarding 25 and 5. Was 99 evacuated at a mean wait of 1.1 days across four flown sorties post-Issue-203; the wait is the cancellations and the larger backlog together, not the generator alone |
| AME wait-time DOW poll (seed 42, post-Issue-206) | `dow_echelon=5`, daily poll interval (`role4.ame.dow_check_interval = 1440` min); 0 deaths observed, as in every run since the poll was added, including this one with its 10-day mean wait; see README [AME Wait Checkpoint](README.md#ame-wait-checkpoint) for why no single-run count should be read as evidence about the mechanism's magnitude |

---

## Out of Scope for Claude

- Merging to `main` — owner only.
- Changing the casualty rate baseline scenario without raising and discussing an issue first.
- Modifying `env_data.json` schema without a corresponding issue and PR.
- Removing or replacing existing references in `README.md`, `docs/Single_Run_Analysis.md`, or `docs/Multi_Run_Analysis.md` without explicit instruction.
