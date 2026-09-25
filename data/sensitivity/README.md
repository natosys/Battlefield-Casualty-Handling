# Sensitivity Analysis Evidence Set

The measured evidence behind the sensitivity findings reported in
[README.md](../../README.md#sensitivity-analysis) and in Further Development
entries L18 and L29. It is tracked here because it cannot be regenerated
cheaply: the Morris design point cache alone represents about fourteen hours of
computation on four cores, the Sobol cache many more, and every published index,
rank and separation in the project derives from them.

The Sobol decomposition, the noise floor measurement and the two re-analyses
were produced from one code state, commit `ed3c426`, in the pinned Dev
Container described in the [Development
Environment](../../README.md#development-environment) section. The Morris
screen was re-run under Issue #339 at commit `a3dc41a`, in an unpinned R 4.3.3
environment on four cores, over about fourteen hours; the host restarted once
during the run and the screen resumed from its design point cache, which
`scripts/check_screen_order.R` asserts re-evaluates nothing. Each screen's
`*_run_metadata.csv` records the design behind its own results.

**The Sobol decomposition therefore predates the Morris re-screen, and its
parameter selection no longer matches it.** Its five selected parameters were
chosen as the five leading ones on an earlier Morris ranking. On the current
ranking three of them still lead (`mass_casualty_rate` 1st,
`mass_casualty_max_cas` 2nd, `pri1_surg_prob` 4th), but `mass_casualty_min_cas`
has fallen to 12th and `pri1_dcs_rate` to 25th, and their places in the top five
are taken by `pri1_evac_prob` and `mass_casualty_kia_fraction`. At twenty
trajectories no leading parameter is separated from the one below it, so this is
a change of membership within an unresolved group rather than a firm reordering;
but no index in the decomposition should be quoted as describing the current
parameter set, and re-running it is roughly fourteen hours of computation.

## The Issue #339 re-screen and the decisions behind it

The re-screen was made because `r2b_dwell_mean` and `r2e_dwell_mean` changed
definition under Issue #331, from the arithmetic mean of the stays that closed
inside the run to the Kaplan-Meier restricted mean over every casualty who
entered the echelon. The earlier definition dropped a stay still running when
the window closed, which discounted the effect of exactly the parameters that
lengthen stays, and the tracked cache could not re-derive the changed responses
because it holds scalar responses rather than the monitoring data behind them.
Four decisions were settled before it ran.

**All thirty-six responses were re-screened, not only the two.** The design is
shared, so evaluating it evaluates every response at no extra simulation cost,
and a single run leaves one internally consistent evidence set at one commit.
The other thirty-four rankings move because the parameter set grew, not
because their definitions changed.

**The restriction horizon is not screened.** `INTERVAL_RESTRICTION_MIN` fixes
the Kaplan-Meier restricted mean at seven days. It is a reporting choice about
how a censored interval is summarised, not a property of the trauma system a
planner could change or that carries epistemic uncertainty about a true value,
so screening it would rank the estimator rather than the model.

**Thirteen parameters were added, taking the set from 65 to 78.** An audit of
every numeric leaf in `env_data.json` against `morris_params` and the README's
exclusion list found thirteen screened by neither: the Role 4 reconstruction
share, return interval and post-reconstruction return rate; the four Role 4
length-of-stay modes; the Role 4 intensive care continuation duration; the R2E
pre-flight critical hold share and duration; the forward theatre saturation
release threshold; the R2B holding evacuation threshold; and the mass casualty
wounded/killed split. Adding them to this run cost about a fifth more design
points; adding them later would have cost a further full screen.

**The R2B holding evacuation threshold is screened, with its ranks
annotated.** `docs/Multi_Run_Supplement.md` sets out why a screen cannot rank
it: it ships disabled at zero, so its first grid step measures switching it on
rather than the size of the threshold. The screen bears that out, ranking it
first on R2B dwell, both forward return-to-duty rates and the p90 time to first
surgery. It is kept in the design so that every other parameter is screened
across configurations with the threshold in force as well as disabled, and its
own ranks are annotated in the README as the on/off transition rather than a
planning influence.

Three of the added parameters, `role4_return_interval_mode`,
`role4_post_reconstruction_return_rate` and `role4_icu_continuation_mode`,
measure µ\* of exactly zero on every response. None of the thirty-six measures
what they change, and they are kept so that a response added later can rank
them without a new design. The four Role 4 length-of-stay modes are zero on
every in-theatre response by construction, the census being computed after the
simulation, and register only on the two Role 4 occupancy responses.

Neither response set carries a holding bed queue response at either echelon,
and neither parameter set carries the R2E holding or intensive care bed counts
as a screened parameter; `R/sensitivity.R`'s own comment on `morris_params`
records why the fixed establishment counts are excluded. Issue #348 raised both
gaps. `cache_check_schema()`, asserted by `scripts/check_screen_cache.R`,
closes the separable defect found alongside them, that `points.csv` could not
detect a response added to an existing cache.

## Contents

| Path | What it holds |
|---|---|
| `morris_r20/points.csv` | The Morris design point cache: 1,580 points, being 20 trajectories over 78 parameters plus one, at 5 replications and 30 days each. One row per design point, one column per screened response |
| `morris_r20/morris_ranking_<response>.csv` | Per-parameter µ\* and σ for each of the 36 screened responses, with that response's criteria mapping and degeneracy diagnostics |
| `morris_r20/morris_ranking.csv` | The primary system OT queue ranking, repeated under its historical filename. This is the file the published ranking table is built from |
| `morris_r20/morris_design_and_responses.rds` | The design matrix and response matrix as R objects, for re-analysis without re-running the screen |
| `morris_r20/morris_run_metadata.csv` | The design behind the Morris results: trajectory count, levels, grid jump, replications, run length, commit and the responses flagged degenerate |
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
