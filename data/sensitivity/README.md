# Sensitivity Analysis Evidence Set

The measured evidence behind the sensitivity findings reported in
[README.md](../../README.md#sensitivity-analysis) and in Further Development
entries L18 and L29. It is tracked here because it cannot be regenerated
cheaply: the two design point caches together represent roughly nineteen hours
of computation on four cores, and every published index, rank and separation in
the project derives from them.

All of it was produced from one code state, commit `ed3c426`, in the pinned Dev
Container described in the [Development
Environment](../../README.md#development-environment) section. Each screen's
`*_run_metadata.csv` records the design behind its own results.

## Contents

| Path | What it holds |
|---|---|
| `morris_r20/points.csv` | The Morris design point cache: 1,320 points, being 20 trajectories over 65 parameters plus one, at 4 replications and 30 days each. One row per design point, one column per screened response |
| `morris_r20/morris_ranking_<response>.csv` | Per-parameter µ\* and σ for each of the 36 screened responses, with that response's criteria mapping and degeneracy diagnostics |
| `morris_r20/morris_ranking.csv` | The primary system OT queue ranking, repeated under its historical filename. This is the file the published ranking table is built from |
| `morris_r20/morris_design_and_responses.rds` | The design matrix and response matrix as R objects, for re-analysis without re-running the screen |
| `sobol_n200/points.csv` | The Sobol design point cache: 1,400 points, being N = 200 over the five leading parameters plus two, at 4 replications and 30 days each |
| `sobol_n200/sobol_<response>.csv` | First-order and total-order indices with 95% bootstrap intervals, per response. A `flag` column marks an index outside the theoretical [0, 1] range with ST ≥ S1 |
| `noise_floor/points.csv` | Within-point standard deviations at 20 design points evaluated at 20 replications each, the measurement of replication noise |
| `noise_floor/sobol_noise_floor.csv` | The noise share per response, with the deflation factor on the reported indices and the replication count that would make it negligible |
| `sobol_estimator_comparison.csv` | The same cached responses recomputed under the Jansen and Martinez pick-freeze estimators alongside the reported Saltelli one |
| `sobol_separation.csv` | Which orderings the sample establishes, from a bootstrap over the design rather than over the indices |

## Re-analysis without re-running the model

Four scripts read these files and cost no simulation, so a reader can check
the reported conclusions rather than take them.

The Morris scatter plots `README.md` embeds are rendered from
`morris_r20/morris_design_and_responses.rds` rather than written by the screen
that produced it, so the plots and the published ranking table cannot describe
different screens. They did once: the tracked plots were left at the r = 5
screen of Issue #155 while the tracked rankings moved to the r = 20 screen that
superseded it, and every published plot disagreed with the table printed above
it until Issue #232. The renderer recomputes each response's µ\* and σ from the
saved design and refuses to write a plot that does not match the ranking CSV it
accompanies.

The tracked plots were last rendered outside the pinned container, in an R
4.3.3 sandbox carrying `ggplot2`, `ggrepel` and `sensitivity` at the exact
versions `renv.lock` names. Nothing measured moves with the renderer: every
value a plot shows comes from the tracked design and responses, and the check
above confirmed each response reproduces its tracked ranking to within 5e-15
relative. What a different R version could move is the rendering, so a
maintainer re-render in `rocker/rstudio:4.4.2` would establish that the images
are byte-identical as well as numerically identical.

```sh
Rscript scripts/render_morris_plots.R                       # to outputs/images
Rscript scripts/render_morris_plots.R --refresh-baseline    # to images/
```

The three Sobol re-analyses read the decomposition rather than the screen:

```sh
P=pri1_surg_prob,mass_casualty_rate,mass_casualty_max_cas,mass_casualty_min_cas,pri1_dcs_rate

Rscript scripts/compare_sobol_estimators.R \
  --cache data/sensitivity/sobol_n200/points.csv --params "$P"

Rscript scripts/test_sobol_separation.R \
  --cache data/sensitivity/sobol_n200/points.csv --params "$P"
```

`scripts/measure_noise_floor.R` does run the model, but resumes from
`noise_floor/points.csv` when pointed at it with `--point-cache`, so it
reproduces the reported table without re-simulating:

```sh
Rscript scripts/measure_noise_floor.R --params "$P" \
  --cache data/sensitivity/sobol_n200/points.csv \
  --point-cache data/sensitivity/noise_floor/points.csv \
  --points 20 --reps 20
```

## What the caches are and are not

Both caches here were written with the replication seed unpinned, which is the
shipped default (`--crn-seed` absent). A pinned screen draws a different seed
vector, so it produces different responses and must not resume either of these
caches. That is why the default is unpinned: a shipped default that did not
reproduce the shipped data would make every figure in this set unverifiable.

A cache belongs to the design that produced it. The design follows from the
seed, the parameter set and their bounds, so a cache read against a screen
whose seed, trajectory count, level count or bounds have moved would silently
supply responses from a different design. Clear a cache whenever any of those
change rather than resuming across the change. `scripts/check_screen_cache.R`
asserts the invariants the resume path depends on.

The Sobol cache does not record the generator state that produced its design
matrix, only the responses in design point order. That is sufficient for every
pick-freeze estimator, each of which is a formula over the response vector and
the fixed row layout, and is why the two re-analysis scripts above need no
design values. It is not sufficient to re-evaluate a specific design point,
which is why the noise floor measurement samples fresh points from the same
bounds rather than repeating the decomposition's own.
