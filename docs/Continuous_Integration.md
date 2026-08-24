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

The workflow is `.github/workflows/checks.yml`. It defines three jobs, and
every one of them runs in the pinned container the project's baseline figures
are produced in, so that a difference it reports is a difference in the code
rather than in the environment.

| Job | Runs on | What it does | Typical cost |
|---|---|---|---|
| Fast suite and lint ratchet | Every pull request against `main`, and every push to `main` | `scripts/run_all_checks.R --fast`, which is every check except the calibration check, including the lint ratchet and the seed-42 reproduction | Twelve to fifteen minutes once the package cache is warm |
| Seed-42 baseline reproduction | The same events | `scripts/check_baseline_reproduction.R` alone, so that the property every published figure rests on reports as its own status check rather than as a line inside another job's log | Two to three minutes plus the restore |
| Slow suite | Weekly, at 02:00 UTC on Sunday, and on demand | `scripts/run_all_checks.R --slow`, which is `check_dow_calibration.R` and its 450 replications | Forty-five minutes to an hour |

The first run of any of these on a branch whose `renv.lock` differs from what
the cache holds pays for restoring the project library, which takes several
minutes on top of the figures above. Subsequent runs restore from the cache
keyed on the hash of `renv.lock`.

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
the `simmer` end-of-run warnings filtered out. The full output of every check,
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
# What the pull request is gated on
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
