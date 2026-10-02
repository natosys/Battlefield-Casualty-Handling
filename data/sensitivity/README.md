# Sensitivity Analysis Evidence Set

The measured evidence behind the sensitivity findings reported in
[README.md](../../README.md#sensitivity-analysis) and in Further Development
entries L18 and L29. It is tracked here because it cannot be regenerated
cheaply: the Morris design point cache alone represents about fifteen and a
half hours of computation on four cores, the Sobol cache many more, and every
published index, rank and separation in the project derives from them.

The Morris screen was run under Issue #410 at commit `5ee47ee`, in an unpinned R
4.3.3 environment on four cores, over about fifteen and a half hours; the host
restarted once during the run and the screen resumed from its design point
cache, which `scripts/check_screen_order.R` asserts re-evaluates nothing. The
Sobol decomposition, the noise floor measurement and the two re-analyses were
produced under Issues #408 and #228 at commit `b6093a8`, in the same unpinned
R 4.3.3 environment. The decomposition ran at roughly 1.3 design points per minute on four cores, a
hundred hours of compute, across repeated host restarts each resumed from the
design point cache; the 8,000 responses are one design evaluated once, with no
point repeated or discarded. Each screen's
`*_run_metadata.csv` records the design behind its own results.

**The Sobol decomposition follows the current Morris ranking by the rule
below.** It decomposes the unresolved leading cluster on the system OT queue
ranking of the Issue #410 screen, ranks 1 to 7 (`mass_casualty_rate`,
`pri1_evac_prob`, `pri1_surg_prob`, `pri1_dcs_rate`, `mc_p1_balance`,
`mass_casualty_kia_fraction` and `mass_casualty_max_cas`, with µ\* from 13.04
down to 3.42), and carries a composition group whole whenever one of its
coordinates falls inside the cluster. `mc_p1_balance` does, so the mass casualty
composition enters as a single object, sampled from a Dirichlet distribution,
and its second coordinate `mc_p2_p3_balance` joins as the eighth column.
`dnbi_disease_balance`, at rank 9, sits just outside the cluster and was not
carried. The cut at rank 7 is not arbitrary at its edge in the way a top five
would be: at r = 20 no parameter in ranks 2 to 7 separates from its neighbour,
so the cluster is the unit the screen resolves, and ranks 1 to 7 is that unit
with the leader included.

The decomposition runs at N = 800 with 8 replications per design point, over
30 days, under the shipped configuration (`ame_failure_probability` at zero)
with the replication seed unpinned. It carries three responses: the system OT
queue, the R2E OT queue (identical to it, since the R2B theatre queue is
constant across the design) and transport utilisation. The transport queue and
the R2B theatre queue are dropped, the first being almost entirely noise and
the second constant. Transport utilisation is retained, but at 49.3% noise
share it does not meet the 20% target the other two meet, and it is reported
rather than interpreted; see the noise floor row below.

**Results.** On the system OT queue, `mass_casualty_rate` carries a total-order
index of 0.79 (95% CI [0.66, 0.92]), `mass_casualty_max_cas` 0.27, `pri1_surg_prob`
0.20, `pri1_evac_prob` 0.14, `pri1_dcs_rate` 0.10, `mass_casualty_kia_fraction`
0.10, `mc_p2_p3_balance` 0.06 and `mc_p1_balance` 0.04. The leader separates from
the second at a difference of 0.525 ([0.366, 0.687], P > 0.999); the second does
not separate from the third (difference 0.067, P = 0.81, needing N of about
4,260). Three of the six separations the reading requires hold. Replication
noise is 11.3% of the system queue's variance at 8 replications (95% CI
[9.7%, 12.9%]), against 16.5% projected from the earlier four-replication
measurement, so the realised share is below the projection. The reported
indices are uncorrected; dividing by the deflation factor of 0.887 gives the
upper end of the bracket the README states under L29. The Jansen and Martinez
estimators agree that `mass_casualty_rate` leads and disagree about the order
beneath it, and neither returns a total-order index at or below zero anywhere
in the design, which is construction and not resolution.

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
annotated.** `docs/Methods.md` sets out why a screen cannot rank
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

## The Issue #410 re-screen and the decisions behind it

Issue #348 closed by fixing the cache-schema defect alone (PR #398), leaving
its two coverage gaps, an R2B/R2E holding queue response and the R2E
establishment bed counts as screened parameters, orphaned and untracked. This
re-screen closes both, taking the design from 78 to 80 parameters and from
1,580 to 1,620 points, and re-screens all thirty-six responses against the
larger design at no extra simulation cost, on the same rationale the Issue
#339 re-screen gives above.

**Two responses were added: `r2b_hold_q` and `r2e_hold_q`.** Each is the
time-weighted mean queue length of its echelon's holding bed pool, on the same
`safe_q()` regex-match convention every other queue response in the set uses
(`^b_r2b_hold_` and `^b_r2eheavy_hold_`), so a holding bed queue is measured by
the same estimator as the theatre and intensive care queues beside it.

**Two parameters were added: `r2e_icu_beds` and `r2e_hold_beds`.** Screening
the R2E intensive care and holding bed establishment counts needed a new
`apply_bed_establishment_params()` function rather than a write into the
`vars` tree every other parameter uses, because `ed$elms` at the point
`apply_params()` receives it is not the raw `env_data.json` configuration but
`build_element_resources()`'s already-expanded resource identifier vectors:
each R2E team instance carries an `icu_bed` and a `hold_bed` character vector,
one identifier per bed, and `build_env()` (`R/environment.R`) registers one
`simmer` resource per identifier the vector carries. There is no separate
count field to write, so the function regenerates each vector at the rounded
length Morris draws, on the exact naming convention
`build_element_resources()` used to create it. Morris moves a screened
parameter continuously across its bounds, and a bed count is discrete, so each
draw is rounded to the nearest whole bed before the vector is built:
round-to-nearest over a continuous one-at-a-time step, rather than a fixed
integer grid, so a design point's bounds stay the literal bed-count bounds a
reader expects. This is the discretisation decision Issue #348 asked to have
recorded rather than left to whatever the code happened to do.

Both additions land where the audit that added them expected: `r2e_icu_beds`
leads its own queue response (`morris_ranking_r2e_icu_q.csv`) at µ\* = 1.51,
the only response in the tracked set where a fixed establishment count
outranks every casualty-load or clinical-probability parameter, and
`r2e_hold_beds` ranks 21st on the primary system queue ranking, ahead of the
median parameter. Screening the establishment alongside the demand placed on
it is what lets a ranking distinguish a queue driven by arrivals from one
driven by capacity, which neither screen could do while the bed counts were
fixed.

## Contents

| Path | What it holds |
|---|---|
| `morris_r20/points.csv` | The Morris design point cache: 1,620 points, being 20 trajectories over 80 parameters plus one, at 5 replications and 30 days each. One row per design point, one column per screened response |
| `morris_r20/morris_ranking_<response>.csv` | Per-parameter µ\* and σ for each of the 36 screened responses, with that response's criteria mapping and degeneracy diagnostics |
| `morris_r20/morris_ranking.csv` | The primary system OT queue ranking, repeated under its historical filename. This is the file the published ranking table is built from |
| `morris_r20/morris_design_and_responses.rds` | The design matrix and response matrix as R objects, for re-analysis without re-running the screen |
| `morris_r20/morris_run_metadata.csv` | The design behind the Morris results: trajectory count, levels, grid jump, replications, run length, commit and the responses flagged degenerate |
| `sobol_n800/points.csv` | The Sobol design point cache: 8,000 points, being N = 800 over the eight decomposed coordinates plus two, at 8 replications and 30 days each. One row per design point, one column per decomposed response |
| `sobol_n800/sobol_run_metadata.csv` | The design behind the decomposition: sample size, estimator, bootstrap resamples, replications, run length, the eight parameters in design order, the Dirichlet group and the commit |
| `sobol_n800/sobol_<response>.csv` | First-order and total-order indices with 95% bootstrap intervals, per response. A `flag` column marks an index outside the theoretical [0, 1] range with ST ≥ S1 |
| `noise_floor/points.csv` | Within-point standard deviations at 20 design points evaluated at 20 replications each, the measurement of replication noise |
| `noise_floor/noise_floor_run_metadata.csv` | The parameters, point and replication counts, seed and commit behind the noise floor measurement |
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
P=pri1_surg_prob,mass_casualty_rate,mass_casualty_max_cas,pri1_evac_prob,pri1_dcs_rate,mass_casualty_kia_fraction,mc_p1_balance,mc_p2_p3_balance

Rscript scripts/compare_sobol_estimators.R \
  --cache data/sensitivity/sobol_n800/points.csv --params "$P"

Rscript scripts/test_sobol_separation.R \
  --cache data/sensitivity/sobol_n800/points.csv --params "$P"
```

`scripts/measure_noise_floor.R` does run the model, but resumes from
`noise_floor/points.csv` when pointed at it with `--point-cache`, so it
reproduces the reported table without re-simulating:

```sh
Rscript scripts/measure_noise_floor.R --params "$P" \
  --cache data/sensitivity/sobol_n800/points.csv \
  --point-cache data/sensitivity/noise_floor/points.csv \
  --points 20 --reps 20 --design-reps 8
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
