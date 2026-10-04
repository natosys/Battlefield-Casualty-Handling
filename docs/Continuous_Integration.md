# Continuous Integration and the Check Suite

This document is the operating guide for the repository's automated
verification: what runs, when it runs, how to read a result, and what to do
about each way it can fail. It is written for a maintainer working on a pull
request. The rules the lint gate enforces are in
[docs/STYLE_GUIDE.md](STYLE_GUIDE.md), the measured runtime and behaviour of
each individual check are in [The Checks](#the-checks) below, and the
design of the suite is described in the README's
[Verification and Continuous Integration](../README.md#verification-and-continuous-integration)
section. This document does not repeat any of those.

## What runs, and when

The workflow is `.github/workflows/checks.yml`. It defines five jobs, and
every one of them runs in the pinned container the project's baseline figures
are produced in, so that a difference it reports is a difference in the code
rather than in the environment.

| Job | Runs on | What it does | Typical cost |
|---|---|---|---|
| Classify the change | Every pull request | Compares the branch against its base and decides whether the change can move a model output, a lint count or the tracked baseline | Seconds |
| Fast suite and lint ratchet | Every pull request against `main`, and every push to `main` | `scripts/run_all_checks.R --fast --jobs auto`, which is every check except the calibration check, including the lint ratchet and the seed-42 reproduction, run several at a time | 7 min 27 s on the four-core runner, measured 27 August |
| Seed-42 baseline reproduction | The same events | `scripts/check_baseline_reproduction.R` alone, so that the property every published figure rests on reports as its own status check rather than as a line inside another job's log | Thirty-five seconds plus the restore |
| Shiny console browser suite | The same events | `npx playwright test`, which starts the console and drives it in a headless Chromium | Two to three minutes plus the restore and the toolchain install |
| Slow suite | Weekly, at 02:00 UTC on Sunday, and on demand | `scripts/run_all_checks.R --slow`, which is `check_dow_calibration.R` and its 450 replications | Forty-five minutes to an hour |

The figures above are the check time alone. Restoring the project library
costs a further minute or so on top, and less again once the cache keyed on the
hash of `renv.lock` is warm. The fast suite is what a pull request waits on:
every other job reports inside three minutes.

## What a documentation-only change runs

A pull request that touches no code is not worth eleven minutes of the same
checks. The first job classifies the change by comparing the branch against its
base, and the other two narrow what they run accordingly.

A change counts as reaching code when it touches `R/`, `scripts/`, `tests/`,
`run.R`, `app.R`, `env_data.json`, `renv.lock`, `.Rprofile`, `.lintr`,
`package.json`, `package-lock.json`, `playwright.config.js`, `.devcontainer/`
or `.github/workflows/`, or the tracked baseline evidence under `data/`,
`images/` or `logs/`. Anything else, which in practice means the markdown
documents, is a documentation-only change. A push to `main`, the weekly
schedule and a manual dispatch are never narrowed: each wants the whole gate
rather than a subset inferred from one diff.

| | Reaches code | Documentation only |
|---|---|---|
| Fast suite | Every fast check, and the lint ratchet | `check_markdown.R`, `check_references.R` and `check_env_data_summary.R` alone, in about twelve seconds |
| Seed-42 reproduction | Runs | Reports as not applicable |
| Browser suite | Runs | Reports as not applicable |

Two things follow from how this is arranged, and both are deliberate. The jobs
narrow what they run rather than being skipped by a path filter, because a
required status check that never reports leaves a pull request waiting on it
indefinitely; every job reports on every pull request. And the checks a
documentation-only change does run are the three that read the tracked
documents, which is where a prose edit can actually break something: a moved
heading, a stale anchor, a citation with no entry, an environment summary that
no longer matches `env_data.json`.

A post-merge chore pull request, which updates the action plan and little else,
therefore reports in about a minute rather than in eleven.

## Reading a result on a pull request

The Checks tab of the pull request lists each job by name. A green tick means
every check the job ran exited zero. A failure names the step that failed, and
the suite's log holds one line per check:

```
  [PASS] check_arrival_rate_fidelity.R                13 s
  [FAIL] check_icu_time_conservation.R              1 min 39 s  exit status 1
      | 3 check(s) FAILED:
      | - the ICU requirement is conserved on the deferred-surgery route
```

Only a failing check's output is printed, and only its last forty lines, with
the `simmer` end-of-run warnings filtered out and a line stating how many
earlier lines were omitted. The full output of every check,
passing or failing, is attached to the run as the `fast-check-logs` artifact,
downloadable from the run's summary page.

Two checks regenerate a tracked document rather than only inspecting it, and
for those the runner treats a modified working tree as the failure signal. A
failure reported as `modified tracked files` means the document had drifted
from what generates it, not that the check itself broke.

## Running the same thing locally

Every job runs a command that can be run by hand from the repository root, and
running it before pushing is faster than waiting for the gate:

```bash
# What the pull request is gated on, one check per core
Rscript scripts/run_all_checks.R --fast --jobs auto

# The same thing one check at a time
Rscript scripts/run_all_checks.R --fast

# One check, by name or by pattern
Rscript scripts/run_all_checks.R --only check_icu_time_conservation

# What the weekly job runs
Rscript scripts/run_all_checks.R --slow

# Which checks are classified fast and which slow, without running any
Rscript scripts/run_all_checks.R --list
```

The runner writes each check's output to `outputs/checks/`, which is
gitignored, and prints a summary line and a non-zero exit status if any check
failed. Add `--no-tree-check` when the working tree is already dirty for
unrelated reasons, which otherwise makes the two document-regenerating checks
report a failure that is not theirs.

## Running several checks at once

`--jobs <n>`, or `--jobs auto` for one check per logical core, runs several
checks concurrently. Each check is a separate `Rscript` process reading the
repository and writing its own log and its own temporary directory, so nothing
about a check depends on being alone in the repository, with one exception:
the two checks that regenerate a tracked document are recognised by a
repository-wide `git status` comparison, which cannot say which of two
concurrent writers touched a file, so the runner takes those first and on
their own before the pool starts.

Two consequences are worth knowing. The pool divides the machine between the
checks it has in flight, giving each child an `MC_CORES` of the detected core
count divided by the job count, so a check that runs replications forks fewer
workers than it would on its own; what it costs changes and what it concludes
does not, a measurement being a function of its control seed rather than of the
core count it was taken on. And results are printed as each check finishes
rather than in alphabetical order, so the summary line reports the elapsed time
with the summed check time beside it:

```
24 of 24 checks passed in 7 min 27 s (29 min 06 s of check time)
```

That line is the 27 August measurement of the fast suite on the four-core
runner, against 16 min 51 s for the same twenty-four checks one after another.
The summed figure exceeds the serial one because concurrent checks contend:
what each check costs rises while what the suite costs falls. The suite is now
bounded by the machine rather than by the ordering, 29 minutes of check time
across four cores being a little over seven minutes whatever the schedule, so
the remaining levers are a larger runner or less work rather than a better
arrangement of the same work. The longest single check,
`check_measurement_reproducibility.R`, accounts for 6 min 11 s of the seven.

Checks are dispatched longest first, from the runtimes recorded in
`scripts/check_runtimes.csv`, because the suite cannot finish before its
longest check does and a long check started last strands the pool waiting on
it. That file is a scheduling hint and nothing else: a missing or stale entry
costs a little wall clock and cannot change a result. Refresh it from a full
run's own measurements with

```bash
Rscript scripts/run_all_checks.R --fast --refresh-runtimes
```

which is the only way the tracked file is written. Refresh it from a serial
run rather than a concurrent one, the concurrent runtimes being a function of
how many cores each check was left with.

Running the suite locally needs the project library restored
(`renv::restore()`) and `lintr` installed. The Dev Container installs both, so
the simplest way to reproduce a continuous integration result exactly is to
open the repository in it.

## Triggering the slow suite on demand

The slow suite does not run on a pull request. To run it before a merge that
could move the calibration, or to re-run it after a failure, dispatch it from
the Actions tab: open **Actions**, select the **Checks** workflow, choose **Run
workflow**, set **Run the slow suite** to true, and select the branch. Leaving
that input false dispatches the fast jobs alone, which is a way to re-run them
without pushing a commit.

The same dispatch from the command line, for a maintainer with the GitHub CLI
authenticated against this repository:

```bash
gh workflow run checks.yml --ref <branch> -f run_slow_suite=true
```

A change that alters mortality, the arrival process, or any parameter the
died-of-wounds curve is fitted against should have the slow suite dispatched on
its branch before it is merged. The weekly run catches drift, but it catches it
after the fact.

## When the gate is red

### A regression check fails

Read the check's own output first, in the log or in the artifact. Each check
states the property it asserts and prints one line per assertion, so the
failing assertion names what broke. The checks are not style gates: a failure
means a property the model is meant to hold no longer holds, and the fix is in
the code rather than in the check. Changing a check to accommodate a failure is
appropriate only when the property itself was wrong, and then the change is
argued in the pull request rather than made quietly.

### The lint ratchet fails

The failure names the rule and both counts:

```
  [FAIL] line_length_linter                 726 (baseline   725)  RISEN
```

The pull request added a finding. Locate it with
`Rscript -e 'print(lintr::lint_dir("."))'`, which lists every finding with its
file and line, and repair the new one. The baseline is not raised to
accommodate new findings; that is what makes it a ratchet.

The opposite case is not a failure. When a pull request removes findings, the
check reports the improvement and passes, and a maintainer tightens the ratchet
by refreshing the baseline:

```bash
Rscript scripts/check_lint.R --refresh-baseline
```

That rewrites `scripts/lint_baseline.csv`, which is committed with the change
that earned it. Refreshing the baseline is also the correct response to a rise
that follows a deliberate `lintr` version change, since a new version can add
or refine a linter; the version is pinned in `.devcontainer/Dockerfile` and in
the workflow's `LINTR_VERSION`, and both move together.

### The roxygen ratchet fails

The failure names the rule and both counts, in the same shape the lint
ratchet uses:

```
  [FAIL] missing_header               1 (baseline     0)  RISEN
```

The pull request added a function without a roxygen header, an argument
without an `@param`, or a function without a `@return`. List the findings with
their file, line and function name:

```bash
Rscript scripts/check_roxygen.R --list
```

Repair the new one rather than raising the baseline. As with lint, a pull
request that removes findings passes and reports the improvement, and a
maintainer tightens the ratchet with
`Rscript scripts/check_roxygen.R --refresh-baseline`, committing
`scripts/roxygen_baseline.csv` with the change that earned it.

### The seed-42 reproduction fails

The check names the first artifact that differs and the first line at which it
differs. There are two cases, and they are told apart by whether the change was
meant to move the model.

If the change was meant to alter the model, or to alter anything that consumes
random draws, the tracked baseline is now stale and is regenerated deliberately:

```bash
Rscript run.R --seed 42 --days 360 --iterations 1 --refresh-baseline
```

That rewrites `images/`, `logs/logs.txt` and the `data/` diagnostics, including `data/seed42_verification.csv`, together,
and they are committed in one commit. Every published seed-42 figure in
`CLAUDE.md`, `docs/Results.md` and `docs/Planning_Implications.md` then
needs revisiting in the same pull request, which is the work the check exists to
make visible rather than to prevent.

If the change was not meant to alter the model, the reproduction failing is the
defect. A change that only reorders code can still shift the random number
stream, by consuming a draw that was not consumed before or by consuming draws
in a different order, and that is a real change to every result the project
publishes even when the model's logic is untouched.

### The console test suite fails

The console carries two suites, split by what each can see. `testthat` covers
the reactive state machine and the helpers around it, and runs inside the fast
suite as `check_testthat.R`. Playwright covers the rendered app, and runs as
its own job against a console it starts.

A `testthat` failure names the file, the line and the expectation, and reads
like any other check: the console's behaviour has changed, and the fix is in
`app.R` unless the expectation itself was wrong.

A Playwright failure is read from the report the job uploads as an artifact,
which carries a screenshot of the page at the moment of the failure and a trace
of everything that led to it. Open the trace with
`npx playwright show-trace <path>`. Two failures are worth telling apart before
reaching for the app. A test that times out waiting for a Quick Run to finish
may be reporting a slow runner rather than a broken console; the run itself is
a real simulation. And a test that fails to find an element by its accessible
name is reporting that the markup moved, which is a real change to the app but
not necessarily a defect in it, so the expectation moves with it.

What neither suite covers is appearance. Playwright asserts that a plot
rendered and has real dimensions, never what it looks like. A layout regression
that breaks no assertion passes both suites. That is an accepted tradeoff
against the maintenance cost of pixel snapshots over `ggplot` output, which
fail on a font substitution and tell a reader nothing; it is worth revisiting
only if a visual regression actually reaches `main`.

### A job fails before any check runs

A failure in the container's system libraries, in restoring the project
library, or in installing `lintr` is an environment failure rather than a
finding. Re-running the job is reasonable once, since a transient failure to
reach a package mirror looks the same. A second identical failure is a real
problem with the workflow or the lockfile, and the fix belongs in the same pull
request only if the pull request caused it.

### The system library install times out

`Install system libraries` reports that it exceeded its 15 minute limit. The
Ubuntu archive mirror the runner was routed to stalled, and the step was cut
off rather than being allowed to hang until the job's own limit, which is 60
minutes for three of these jobs and 180 for the slow suite. This is an
environment failure and says nothing about the pull request.

Re-run the job. A second timeout on the same pull request is worth raising as
a workflow issue rather than re-running a third time, because the step already
retries a failed fetch five times and bounds a silent connection at 30 seconds,
so reaching the limit twice means the mirror is not merely slow.

The step is duplicated in all four jobs that need system libraries, and the
guards are applied inline in each rather than through a composite action,
because `timeout-minutes` is not a step property a composite action supports.
`scripts/check_ci_apt_guards.R` asserts that the four copies stay identical and
that each sets the retries and both transport timeouts before it runs
`apt-get update`, so three jobs hardened and one missed fails the gate rather
than waiting to be discovered by a wedge.

## The Checks

Every regression check under `scripts/`, with its tier, its measured runtime and the
summary it states about itself in its own banner. The table is generated by
`scripts/render_check_table.R` from the checks, the runner's `SLOW_CHECKS` constant
and `scripts/check_runtimes.csv`, and `scripts/check_ci_check_table.R` fails when it
is stale, so a check cannot join the suite without appearing here. A runtime of "not
yet measured" means `scripts/run_all_checks.R --refresh-runtimes` has not been run
since the check was added; the runner schedules such a check as if it cost thirty
seconds.

<!-- CHECK TABLE START -->
| Check | Tier | Measured runtime | What it asserts |
|---|---|---|---|
| `check_absent_attribute_columns.R` | fast | 37 s | an attribute nobody set is an empty column, not none |
| `check_airlift_collapse_protocol.R` | fast | 5 s | the airlift collapse experiment's parameters, its classifier and its published table agree |
| `check_airlift_protocol.R` | fast | 2 s | the strategic evacuation experiment's parameters, its interval construction and its published tables agree |
| `check_ame_backlog_exit.R` | fast | 23 s | the strategic evacuation backlog is bounded by the attribute its own exit route sets |
| `check_analysis_decomposition.R` | fast | 6 s | every analysis stage binds what it returns |
| `check_analysis_idempotence.R` | fast | 74 s | the analysis pipeline is idempotent and RNG-neutral |
| `check_arrival_rate_fidelity.R` | fast | 12 s | each stream realises the daily mean and variance it is configured for |
| `check_baseline_reproduction.R` | fast | 37 s | Seed-42 tracked evidence set reproduction |
| `check_bed_queue_coverage.R` | fast | 27 s | a bed queue figure covers every bed type its echelon fields |
| `check_capacity_sweep_protocol.R` | fast | 7 s | the transport fleet-size sweep and the forward ICU share frontier agree with their published tables |
| `check_censored_interval_estimation.R` | fast | 22 s | an interval still open when the window closes is carried as the lower bound it is, not dropped |
| `check_ci_apt_guards.R` | fast | 2 s | every CI system-library install carries the same stall guards |
| `check_ci_check_table.R` | fast | 2 s | the CI guide's check table lists every check |
| `check_composition_ilr.R` | fast | 8 s | Simplex invariant regression check |
| `check_config_restore.R` | fast | 18 s | a failed sweep or screen restores the configuration |
| `check_console_bindings.R` | fast | 20 s | no console panel reads another panel's local |
| `check_convalescence_invariance.R` | fast | 49 s | convalescence follows the casualty, not the facility |
| `check_definitive_repair_release.R` | fast | 150 s | release to strategic evacuation with the definitive repair outstanding |
| `check_dow_calibration.R` | slow | 2702 s | died-of-wounds rate against its campaign's anchor |
| `check_env_data_summary.R` | fast | 6 s | README environment summary regeneration |
| `check_forward_hold_switch.R` | fast | 60 s | disabled forward holding is the model as it stood |
| `check_hold_episode_reconstruction.R` | fast | 43 s | an R2B holding episode is bounded by the attribute its own exit route sets |
| `check_hold_window_protocol.R` | fast | 7 s | the R2B pre-open hold window's parameters, its responses and its published table agree |
| `check_holding_occupancy_split.R` | fast | 45 s | R2E holding occupancy splits into recovery and evacuation wait, and the two account for the pool within a stated bound |
| `check_icu_gate_protocol.R` | fast | 7 s | the post-operative intensive care gate's parameters, its responses and its published table agree |
| `check_icu_gate_switch.R` | fast | 120 s | the intensive care gate disable switch reproduces the pre-gate model |
| `check_icu_time_conservation.R` | fast | 120 s | post-operative ICU time is conserved across routes |
| `check_input_validation.R` | fast | 16 s | entry points reject malformed input by name |
| `check_lever_realisation.R` | fast | 28 s | two planner levers realise the value configured |
| `check_lint.R` | fast | 92 s | Lint ratchet against the code standard |
| `check_long_horizon_protocol.R` | fast | 40 s | the long-horizon protocol's parameters, its reduction and its published series agree with one another |
| `check_long_horizon_warmup.R` | fast | 8 s | the sustained-horizon Welch diagnostic is correct and the tracked plot and CSV reproduce it |
| `check_markdown.R` | fast | 6 s | Markdown TOC, link and table checks |
| `check_mass_casualty_kia_split.R` | fast | 41 s | a mass casualty event's casualty count is a total, split between the wounded and the immediately killed |
| `check_mass_casualty_protocol.R` | fast | 7 s | the mass casualty stress test's parameters, its responses and its published table agree |
| `check_measurement_reproducibility.R` | fast | 175 s | a measurement is a function of its control seed |
| `check_morris_baseline.R` | fast | 8 s | Screened-parameter baseline agreement |
| `check_planning_implications.R` | fast | 8 s | the planning paper quotes the results paper faithfully |
| `check_policy_sweep_protocol.R` | fast | 22 s | the evacuation policy sweep's parameters, its responses and its published table agree |
| `check_pre_open_window.R` | fast | 39 s | the R2B pre-open hold window behaves at its bounds |
| `check_r2e_surgery_seizure.R` | fast | 16 s | R2E surgery seizes a surgical section |
| `check_references.R` | fast | 5 s | Reference list structural checks Regression check for the reference lists of the four academic documents. The checks below are structural, not bibliographic: they assert the properties a reader relies on when following a citation, and they are the properties a hand renumber silently breaks. Whether a URL is open access is a judgement no script can make, so it stays a manual step at the point a reference is added; what the script can guarantee is that the same source is not listed twice, that every entry is reachable from the text and every |
| `check_replication_independence.R` | fast | 68 s | replications are independent of one another |
| `check_replication_loss_reporting.R` | fast | 19 s | a lost replication is reported, not silently dropped |
| `check_replication_memory.R` | fast | 25 s | peak memory does not grow with the replication count |
| `check_results_tables.R` | fast | 9 s | docs/Results.md is what the tracked evidence says |
| `check_role4_surgical_demand.R` | fast | 169 s | the operating theatre requirement a released casualty carries to the national support base |
| `check_role4_ward_phases.R` | fast | 9 s | the Role 4 ward split conserves the length of stay |
| `check_roxygen.R` | fast | 5 s | Roxygen ratchet against R1 and R2 |
| `check_scenario_labels.R` | fast | 7 s | comparative scenario plotting is independent of the session's character locale Terminal / Claude Code cloud: |
| `check_scenario_protocol.R` | fast | 8 s | the comparative scenario analysis's parameters, its responses and its published tables agree |
| `check_screen_cache.R` | fast | 7 s | a screen's point cache resumes what it recorded |
| `check_screen_order.R` | fast | 159 s | a screen evaluates its design in order, exactly once |
| `check_sensitivity_protocol.R` | fast | 3 s | the sensitivity screens' documented design is the design their tracked run metadata records |
| `check_structure_tables.R` | fast | 5 s | the two structure tables list every module and script they claim to |
| `check_testthat.R` | fast | 96 s | the Shiny console's unit and testServer suites |
| `check_time_series_figures.R` | fast | 6 s | the campaign time series, and the claims the paper makes from them, agree with the tracked measurement |
<!-- CHECK TABLE END -->

## Adding a check

A new `scripts/check_*.R` is picked up by the runner automatically, and is
therefore gated from the moment it is committed, with no edit to the runner or
to the workflow. It is classified fast unless it is named in the runner's
`SLOW_CHECKS` constant, which is deliberate: a check nobody classified is one
that runs on every pull request. Add a check to `SLOW_CHECKS` only on the
evidence of a measured runtime, and record that measurement with
`scripts/run_all_checks.R --refresh-runtimes`; `scripts/render_check_table.R
--refresh-baseline` then adds the check to [The Checks](#the-checks).

The shape a check follows, its exit contract, and its use of the `fail()` and
`report()` helpers are set out in `docs/STYLE_GUIDE.md` under Regression check
scripts.

## Adding a console test

A test of the console's reactive behaviour is a file under `tests/testthat`,
picked up by `check_testthat.R` and therefore by the fast suite, with no edit
to anything else. It needs no browser: `shiny::testServer()` advances the
reactive graph in process, and the helper in `tests/testthat/helper-load-app.R`
has already loaded the console and made its paths absolute. Assert reactive
state, not markup.

A test of what the console renders is a file under `tests/playwright`, picked
up by the browser job on the same terms. Target behaviour: that a control
round-trips a value, that a run completes, that a tab renders something rather
than nothing. Assert against a control's accessible name rather than a
generated element id, since Shiny generates the ids for a `navset_tab` afresh.
The shared waits and the plot assertion are in `tests/playwright/helpers.js`;
prefer them to a bare `waitForTimeout`, which passes on a fast machine and
fails on a slow one.

Both suites are held to `docs/STYLE_GUIDE.md` where it applies: the R files are
linted by the ratchet along with everything else under `R/` and `scripts/`.

The browser suite uses whatever Chromium `PLAYWRIGHT_BROWSERS_PATH` already
provides, which is how it runs in a development container that ships one. A
runner with none, which is what continuous integration is, downloads exactly
the Chromium the pinned `@playwright/test` version expects. Nothing about the
browser enters `renv.lock`; that separation is why the Node toolchain is here
at all.
