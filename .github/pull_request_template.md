## Summary

<!-- What this PR changes, and the issue it closes. One paragraph. -->

Closes #

## Documented Manual Test Plan

`CLAUDE.md` requires this section in every PR. Fill in all five parts; delete
none of the headings.

### Setup

<!-- Seed, run duration, scenario profile, and any parameter change needed to
     observe the behaviour under test. -->

### Steps

1.
2.
3.

### Expected outputs

<!-- Specific observable values or patterns, not "it works". -->

### Regression checks

<!-- Which of the seed-42 baseline values in CLAUDE.md's Key Parameters table
     and docs/Single_Run_Analysis.md this PR was checked against, and whether
     any moved. State the result of:

       Rscript scripts/run_all_checks.R --fast

     and, where the change could affect the calibration, of the slow suite,
     which is dispatched from the Actions tab rather than run on a PR. See
     docs/Continuous_Integration.md. -->

### Known limitations

<!-- What this test plan does not cover, and why. -->

## Documents updated

<!-- Per CLAUDE.md's README Maintenance section, tick what this PR updates and
     delete what does not apply. -->

- [ ] `README.md` (system reference: structure, trajectories, resources, assumptions, Further Development)
- [ ] `docs/Single_Run_Analysis.md` (seed-42 single-run findings)
- [ ] `docs/Multi_Run_Analysis.md` (every replicated finding: the scenario comparison, the policy-lever sweeps and the stress tests)
- [ ] None: this PR changes no section any of the three documents owns
