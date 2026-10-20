# Sensitivity Analysis Evidence Set

The measured evidence behind the sensitivity findings reported in
[README.md](../../README.md#sensitivity-analysis), in
[Sensitivity Screens](../../docs/Results.md#sensitivity-screens) of the results
paper and in Further Development entries L18 and L29. It is tracked here
because it cannot be regenerated cheaply: the 360-day Morris cache represents
about 78 hours of wall clock on four cores and the 360-day Sobol cache about
70, and every published index, rank and separation derives from them.

## The 360-day screens

Both screens were re-run under Issue #454 at the 360-day horizon every other
experiment uses, under the shipped configuration and the current 79-parameter
design, in an unpinned R 4.3.3 environment on four cores. Each evaluated four
design points at once, each point running its replications in turn in one
worker seeded from the point's index (`evaluate_design_concurrently()` in
`R/sensitivity.R`), so a point's responses depend on the design and its index
alone and the screens reproduce from their seed whatever the number of workers
or the resumes they took. Both survived repeated environment restarts through
`scripts/supervise_screen.sh`, which checkpointed each cache onto its own git
ref every five minutes; no point was repeated or discarded. Four Morris design
points failed outright on the first pass and were evaluated on a second, so
every one of the 1,600 points is in the cache.

**Morris** (`morris_r20_d360/`): 20 trajectories over 79 parameters, 1,600
design points, 4 replications of 360 days each, seed 42. It ranks the
surgical delivery share, `surgery_delivery_rate`, beside the R2E OT queue. The
screen was run on a branch whose delivery share was taken over the arrivals
monitor's rows rather than over casualties, which counts a casualty still in
the system at the close twice; the shipped definition takes one row per
casualty. Every other response is computed identically, which was confirmed by
evaluating the same design points under both code bases. The thirteen points evaluated first, before
the screen moved to its seeded concurrent evaluation, were re-evaluated on the
shipped code so that the whole cache follows from one seeding rule, and four
points already in the cache (14, 200, 777 and 1,599) were re-evaluated beside
them: every response other than the delivery share reproduced exactly, and the
delivery share differed by at most 2e-4, which moves no µ\* by more than
about 6e-4, inside the screen's own standard error of a µ\* at r = 20. Those
seventeen points carry the shipped definition and the rest the row-based one. Invoked as
`Rscript scripts/run_sensitivity.R --point-workers 4 --cache-dir outputs/cache/morris_r20_d360`.

**Sobol** (`sobol_n96_d360/`): a Saltelli design at N = 96 over nine
coordinates, 1,056 design points, 4 replications of 360 days each, the Sobol
2007 estimator and 1,000 bootstrap resamples. The coordinates are the seven
parameters ranked first to seventh on the 360-day R2E OT queue ranking, the
second coordinate of the triage composition (carried whole through the
Dirichlet option because `triage_p1_balance` falls inside the cluster), and
`pri1_evac_prob`, which leads the surgical delivery ranking and stands tenth
on the queue. It decomposes the queue, the delivery share and transport
utilisation. Invoked as `Rscript scripts/run_sobol_decomposition.R`.

**What the decomposition does and does not support.** N = 96 was the largest
sample the compute allowed and was not sized to separate the indices, and it
does not: `sobol_separation.csv` establishes almost none of the orderings a
reading would need, `sobol_estimator_comparison.csv` shows the order beneath
the leader changing with the estimator, and the delivery share returns indices
outside the unit interval. Replication noise is not the cause. The cache
records each point's standard deviation across its replications, and the
ratio of the median within-point standard error to the across-design standard
deviation is below a tenth for each decomposed response, as `run_sobol()`
reports on completion. The Morris ranking carries the reading at this horizon.

| Path | What it holds |
|---|---|
| `morris_r20_d360/points.csv` | The 360-day Morris design point cache: one row per design point, one column per screened response |
| `morris_r20_d360/morris_ranking_<response>.csv` | Per-parameter µ\* and σ for each of the 38 screened responses, with that response's criteria mapping and degeneracy diagnostics |
| `morris_r20_d360/morris_ranking.csv` | The primary R2E OT queue ranking, repeated under its historical filename |
| `morris_r20_d360/morris_design_and_responses.rds` | The design matrix and response matrix as R objects, for re-analysis without re-running the screen |
| `morris_r20_d360/morris_run_metadata.csv` | The design behind the Morris results |
| `sobol_n96_d360/points.csv` | The 360-day Sobol design point cache, with each response's per-point standard deviation across replications |
| `sobol_n96_d360/sobol_<response>.csv` | First-order and total-order indices with 95% bootstrap intervals, per response, with the out-of-range `flag` |
| `sobol_n96_d360/sobol_run_metadata.csv` | The design behind the decomposition, the nine parameters in design order |
| `sobol_n96_d360/sobol_estimator_comparison.csv` | The cached responses recomputed under the Jansen and Martinez estimators |
| `sobol_n96_d360/sobol_separation.csv` | Which orderings the sample establishes, and the N each would need |

The two re-analyses read the decomposition from its cache without running the model:

```sh
P=pri1_surg_prob,casualty_surge_rate,pri1_evac_prob,r2e_dcs2_factor,wia_cbt_mean,wia_spt_mean,pri1_dcs_rate,triage_p1_balance,triage_p2_p3_balance

Rscript scripts/compare_sobol_estimators.R \
  --cache data/sensitivity/sobol_n96_d360/points.csv --params "$P"

Rscript scripts/test_sobol_separation.R \
  --cache data/sensitivity/sobol_n96_d360/points.csv --params "$P"
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
design values. It is not sufficient to re-evaluate a specific design point.
