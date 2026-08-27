# Continuous Integration and the Check Suite

This document is the operating guide for the repository's automated
verification: what runs, when it runs, how to read a result, and what to do
about each way it can fail. It is written for a maintainer working on a pull
request. The rules the lint gate enforces are in
[docs/STYLE_GUIDE.md](STYLE_GUIDE.md), the measured runtime and behaviour of
each individual check are in [scripts/README.md](../scripts/README.md), and the
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
| Fast suite and lint ratchet | Every pull request against `main`, and every push to `main` | `scripts/run_all_checks.R --fast --jobs auto`, which is every check except the calibration check, including the lint ratchet and the seed-42 reproduction, run several at a time | 16 min 51 s of check time as at 27 August, spread across the runner's cores |
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
with the summed check time beside it, the second figure being what the same
checks would have cost one after another.

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

### The seed-42 reproduction fails

The check names the first artifact that differs and the first line at which it
differs. There are two cases, and they are told apart by whether the change was
meant to move the model.

If the change was meant to alter the model, or to alter anything that consumes
random draws, the tracked baseline is now stale and is regenerated deliberately:

```bash
Rscript run.R --seed 42 --days 30 --iterations 1 --refresh-baseline
```

That rewrites `images/`, `logs/logs.txt` and the `data/` diagnostics together,
and they are committed in one commit. Every published seed-42 figure in
`CLAUDE.md`, `docs/Single_Run_Analysis.md` and `docs/Multi_Run_Analysis.md` then
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

## Adding a check

A new `scripts/check_*.R` is picked up by the runner automatically, and is
therefore gated from the moment it is committed, with no edit to the runner or
to the workflow. It is classified fast unless it is named in the runner's
`SLOW_CHECKS` constant, which is deliberate: a check nobody classified is one
that runs on every pull request. Add a check to `SLOW_CHECKS` only on the
evidence of a measured runtime, and record that measurement in
[scripts/README.md](../scripts/README.md) alongside the others.

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
