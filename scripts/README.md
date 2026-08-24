# Regression Check Suite

The repository carries sixteen regression checks under `scripts/`, each named
`check_*.R`. Every one of them exits 0 when its assertions hold and non-zero
otherwise. `scripts/run_all_checks.R` runs them as a suite, and
`.github/workflows/checks.yml` runs the fast selection on every pull request
against `main`, so a check's result now gates a merge rather than waiting for
a maintainer to think of it. `docs/Continuous_Integration.md` is the operating
guide for that workflow.

This document records the verification baseline: the result, the runtime and
the observed behaviour of every check, measured together in one sitting in the
project's pinned Dev Container. It is a measurement record rather than a
maintenance guide. A check that fails is recorded here as failing and raised as
its own issue; it is not repaired in the course of taking the measurement,
because a baseline that has been repaired while being measured is no longer an
honest description of the state it was taken from.

## Measurement environment

| Property | Value |
|---|---|
| Commit measured | `edd6285` (head of `main` at the time of measurement) |
| Base image | `rocker/rstudio:4.4.2` |
| Image digest | `sha256:6bfc87fb66d0072e28d88d684a1f7b3e42a1c20360ee5eca5b43168a4eba3945` |
| R version | 4.4.2 (2024-10-31), "Pile of Leaves" |
| `renv` version | 1.2.3 |
| Package resolution | Posit Package Manager **binary** mirror for Ubuntu noble, not compiled from source |
| Packages restored | 100 into the project library, from `renv.lock` unmodified |
| Host resources | 4 CPUs, 15.7 GiB memory |

The image digest is the one `CLAUDE.md`'s Key Parameters provenance note names
for the Issue #155 canonical refresh, so this baseline was measured in the same
container that note derives its published figures from.

Two accommodations were needed to build the container in the measurement
session, both concerning transport only, and neither reaching any package
version. The Ubuntu archive was addressed over TLS because the session's egress
proxy refuses plain HTTP, and `renv` resolved the lockfile from the Posit
Package Manager binary mirror rather than compiling each package from source.
These are the same two accommodations `CLAUDE.md` records for the Issue #155
refresh. `renv.lock` remains the sole authority on package versions, and the
tracked `.devcontainer/Dockerfile` was not modified; the build used a copy that
differs from it in those two lines alone.

## Results

All fifteen checks pass. The checks were executed one after another in a single
container, in the alphabetical order shown, and each runtime below is the
interval between that check's completion and the previous one's, so each figure
carries roughly a second of scheduling overhead.

| Check | Result | Runtime | What it asserts |
|---|---|---|---|
| `check_arrival_rate_fidelity.R` | Pass | 13 s | Each shipped arrival stream realises its configured daily mean and variance, and the realised mean is invariant to the configured standard deviation |
| `check_composition_ilr.R` | Pass | 11 s | Each simplex-constrained composition group stays on the simplex through its screened balance coordinates, across a whole Morris design |
| `check_dow_calibration.R` | Pass | 45 min 02 s | Each configuration's treated-cohort died-of-wounds rate agrees with its campaign's historical anchor, pooled over three independent 50-replication measurements per profile |
| `check_env_data_summary.R` | Pass | 7 s | The `<!-- ENV SUMMARY -->` block in `README.md` agrees with `env_data.json` |
| `check_icu_time_conservation.R` | Pass | 1 min 39 s | A casualty's post-operative intensive care requirement is conserved across all three routes and at every forward ICU share |
| `check_lever_realisation.R` | Pass | 42 s | The reinforcement fill and the R2B holding evacuation threshold are each applied in full |
| `check_markdown.R` | Pass | 7 s | Every anchor link across the tracked markdown set resolves to a heading, every local link and image target exists when resolved relative to its own document, no image carries placeholder alt text, and no heading carries an emoji |
| `check_mass_casualty_kia_split.R` | Pass | 1 min 06 s | A mass casualty event's casualty count is conserved across the wounded and killed split, and the killed reach mortuary handling untriaged |
| `check_measurement_reproducibility.R` | Pass | 4 min 04 s | A multi-replication measurement is a function of its control seed alone, and the caller's generator kind and stream position are restored |
| `check_morris_baseline.R` | Pass | 10 s | Every screened parameter's baseline lies inside its own screening bounds and equals the value it holds in `env_data.json` |
| `check_pre_open_window.R` | Pass | 1 min 01 s | A zero R2B pre-open hold window reproduces the instant-diversion model bit for bit, and every casualty held forward is operated on there |
| `check_r2e_surgery_seizure.R` | Pass | 25 s | Every R2E surgery seizes a surgical section, structurally and behaviourally |
| `check_replication_independence.R` | Pass | 1 min 43 s | `run_once()` is a pure function of its seed, and `run_replications()` draws a distinct seed per replication |
| `check_scenario_labels.R` | Pass | 10 s | The comparative scenario plot renders identically in a C locale and under UTF-8 |
| `check_screen_cache.R` | Pass | 9 s | A sensitivity screen's design point cache resumes exactly what it recorded |
| **Whole suite** | **15 of 15 pass** | **56 min 49 s** | |

No check produced non-zero exit output, so the column the measurement reserved
for failure output is empty throughout.

A sixteenth check, `check_references.R`, was added after this measurement and so
carries no row above. It reads the three academic documents and exits without
running the model, so it belongs with the eight sub-half-minute checks rather
than with the calibration check, but its runtime in this container has not been
measured.

Three checks corroborate published figures independently of the baseline
reproduction below. `check_dow_calibration.R` returns a pooled treated-cohort
died-of-wounds rate of 0.474% (95% CI [0.412%, 0.536%]) for `default` and
0.368% (95% CI [0.310%, 0.426%]) for `moderate_intensity`, matching the Key
Parameters table digit for digit, and adds 3.471% (95% CI [3.360%, 3.583%])
for `high_intensity` against its 3.40% Okinawa target. `check_pre_open_window.R`
reproduces the seed-42 figures of seven casualties held forward and a longest
hold of 58.9 minutes. `check_mass_casualty_kia_split.R` reports 530 arrivals in
its degenerate-case run, the seed-42 total casualty count.

## Seed-42 baseline reproduction

The tracked seed-42 evidence set reproduces **byte for byte**. The run was made
with `Rscript run.R --seed 42 --days 30 --iterations 1`, which writes to
`outputs/` alone and leaves every tracked artifact untouched, and its output was
then compared byte for byte against the tracked files.

| Artifact | Result |
|---|---|
| `logs/logs.txt` | Identical |
| `data/arrivals_wia_cbt.txt` | Identical |
| `data/arrivals_wia_spt.txt` | Identical |
| `data/arrivals_kia_cbt.txt` | Identical |
| `data/arrivals_kia_spt.txt` | Identical |
| `data/arrivals_dnbi_cbt.txt` | Identical |
| `data/arrivals_dnbi_spt.txt` | Identical |
| `data/arrivals_mass_casualty.txt` | Identical |
| `data/mass_casualty_events.csv` | Identical |

The run itself completed in 51.7 seconds. This confirms the reproduction claim
`CLAUDE.md`'s provenance note rests on, at the commit and in the container that
note names.

## Observations for a continuous integration gate

The runtimes divide the suite cleanly. Fourteen checks together take 11 minutes
47 seconds, and eight of those finish in under half a minute each. The
fifteenth, `check_dow_calibration.R`, takes 45 minutes on four cores by itself,
because at its defaults it executes 450 replications across three scenario
profiles. That one check accounts for four fifths of the suite's wall-clock
time, so a per-PR gate can afford the other fourteen comfortably and cannot
afford it. It offers a `--quick` mode, but that mode's own output describes
itself as a wiring test rather than a calibration test, so it is not a
substitute in a gate that is meant to defend the calibration. Running the
calibration check on a schedule, or on demand before a release, fits its cost
better than running it on every pull request.

Two further points bear on wiring the suite up. Several checks emit a large
volume of `simmer` warnings of the form `'wia_cbt42': leaving without releasing
'ame_critical'`, roughly one hundred in a three-day run, raised for casualties
still holding an aeromedical evacuation seat when the run window closes. This
is ordinary end-of-run truncation rather than a defect, and it does not affect
any exit status, but the volume would bury a genuine message in a gate's log and
warrants filtering or suppressing before the output is read automatically.
Separately, `check_env_data_summary.R` and `check_markdown.R` rewrite tracked
documents in place rather than only inspecting them, so a gate must treat a
resulting change in the working tree as the failure signal. Both left the tree
clean at this commit, `check_markdown.R` regenerating all three tables of
contents byte-identically, which is itself evidence that neither the environment
summary nor the anchor set has drifted.


## What was wired up from this baseline

The measurements above decided the shape of the gate rather than merely
describing the suite, and the wiring added under Issue #235 follows them
directly.

`scripts/run_all_checks.R` runs the suite. It discovers checks by glob, so a
check added after this document was written is run without the runner being
edited, and it reports one pass or fail line and a runtime per check before
exiting non-zero if any failed. Its `--fast` selection is every check except
`check_dow_calibration.R`, which is the division the runtimes above establish:
the fast selection costs roughly twelve minutes and the calibration check
alone costs forty-five. `--slow` runs the calibration check by itself, weekly
and on demand rather than on a pull request. The runner also acts on the two
observations at the foot of the results section. Each check's output goes to a
log file and only a failing check's tail is printed, so the hundred `simmer`
end-of-run warnings a model-running check emits cannot bury the line a reader
needs; and for `check_env_data_summary.R` and `check_markdown.R`, which
regenerate a tracked document rather than only inspecting it, the runner
compares the working tree before and after and treats a modification as the
failure signal.

Three checks were added at the same time, and none carries a row in the
results table above because none existed when it was measured.
`check_baseline_reproduction.R` performs the seed-42 byte-for-byte comparison
this document reports under its own heading, as a check rather than as a
manual procedure. It takes 44 seconds, of which the run recorded under that
heading is almost all, measured in an unpinned R 4.3.3 sandbox where it also
reports the tracked set reproducing byte for byte. `check_lint.R` runs `lintr` under `.lintr` and ratchets the
finding count per rule against `scripts/lint_baseline.csv`; it takes 24
seconds, measured in an unpinned R 4.3.3 sandbox with `lintr` 3.4.0 rather
than in the container above, since `lintr` is not part of the pinned library.
`check_references.R`, added between the measurement and this wiring, had never
passed, which running it as part of a suite is what exposed. It matched a URL
with the bracket expression `[^ )\]]` under R's default regular expression
engine, which reads the backslash inside a bracket expression as a literal
character: the class closed at the first bracket and the second became a
literal the URL would have to be followed by, so the pattern matched nothing
and all 68 entries across the three documents read as carrying no URL. The
expression is now evaluated in Perl mode, and the check passes, reporting 63,
3 and 2 correctly sourced entries. The reference lists themselves were never
at fault.

The lint baseline itself was taken in the same R 4.3.3 sandbox: 1,229 findings
across 11 rules, of which 725 are over-long lines and 160 are indentation.
Because the counts are compared rather than required to be zero, a difference
between that environment and the pinned one would surface as a rise on the
first continuous integration run rather than as a silent weakening, and would
be resolved by refreshing the baseline in the container.
