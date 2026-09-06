# Supplementary Material: Experimental Designs and Statistical Methods for the Replicated Trauma System Experiments

## Purpose

<small>[Return to Top](#contents)</small>

This document holds the full experimental designs, the statistical derivations and the implementation detail behind the results reported in `docs/Multi_Run_Analysis.md`. The main paper states the conclusions of the arguments set out here and reports each experiment's finding; this supplement records how each experiment was configured and run, so a reader wishing to reproduce or audit a result has the specification in front of them.

Nothing in this document is a finding. Every measured result belongs to the main paper.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Purpose](#purpose)
- [Contents](#contents)
- [Replication Framework](#replication-framework)
  - [The Independence Argument](#the-independence-argument)
  - [The Withdrawn Antithetic Pairing](#the-withdrawn-antithetic-pairing)
  - [Interval Construction](#interval-construction)
  - [Replication Count Derivation](#replication-count-derivation)
  - [Warm-up Classification](#warm-up-classification)
- [Experiment Designs](#experiment-designs)
  - [Comparative Scenario Analysis](#comparative-scenario-analysis)
  - [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)
  - [The Post-Operative Intensive Care Gate](#the-post-operative-intensive-care-gate)
  - [Forward ICU Share Decision Frontier](#forward-icu-share-decision-frontier)
  - [Transport Fleet-Size Sweep](#transport-fleet-size-sweep)
  - [Force Regeneration Under Reinforcement](#force-regeneration-under-reinforcement)
  - [Mass Casualty Event Stress Test](#mass-casualty-event-stress-test)
- [Provenance](#provenance)
<!-- TOC END -->

---

## Replication Framework

<small>[Return to Top](#contents)</small>

Every experiment runs through the project's multi-run replication framework (`run_replications()`, `R/replication.R`), which executes a requested number of independent replications and returns their monitoring data with a replication index attached. The unit of analysis is the replication throughout, and every response is reduced to one number per replication before any statistic is taken across replications.

### The Independence Argument

Independence follows from how a replication is built rather than being inferred from the output. Two facts establish it.

First, `run_once()` (`R/replication.R`) is a pure function of its seed: re-running a seed reproduces its output exactly, whether or not another run happens in between, so the seed is all that distinguishes two replications and no other channel connects them. Second, `run_replications()` draws a distinct seed for each replication. Independent seeds into a pure function give independent replications, which is a deterministic argument rather than one a finite sample of correlations could support. `scripts/check_replication_independence.R` asserts both facts and runs on every pull request.

Running replications in parallel preserves both facts. They run under `RNGkind("L'Ecuyer-CMRG")` with `mc.set.seed = TRUE`, which gives each worker its own substream of the MRG32k3a generator, and the substream spacing of $2^{76}$ makes overlap impossible at any simulation budget used here. Both dispatch paths use the one generator, so a replication's output depends on its seed and not on the path or the core count that produced it.

### The Withdrawn Antithetic Pairing

No variance reduction scheme sits on top of that framework. Antithetic pairing was used earlier in the project and then withdrawn, because neither of the conditions it needs holds for this model.

Its reach stops at the arrival generators, since `simmer` draws service times and routing probabilities from the global stream inside its own event loop, in an order set by event timing rather than by entity. The technique also requires the response to move consistently in one direction with the input uniforms, and casualty arrivals do not, their rate being scaled by an effective force size that trajectory outcomes themselves add to and subtract from. Measurement agrees: over 75 pairs the within-pair correlation on total casualties is $-0.04$ (95% CI $[-0.27, +0.19]$), worth a variance reduction of about 3% and indistinguishable from none.

One consequence reaches the main paper. The intervals reported for the post-operative intensive care gate were computed while the pairing was still in force, over replications that were not independent, and are narrower than those runs entitle them to be. They are not recomputed, because the comparison is against a configuration that no longer exists in the codebase and the earlier arm cannot be re-run. Intervals that should be wider only strengthen that section's conclusion, which is that the two overlap.

### Interval Construction

Every confidence interval is a Student $t$ interval on the mean across replications,

$$\bar{x} \pm t_{0.975,\;n-1}\,\frac{s}{\sqrt{n}}$$

where $n$ is the replication count, $\bar{x}$ the mean across replications of the per-replication response and $s$ its sample standard deviation. No normal approximation and no bootstrap appears anywhere in the main paper or here. The same expression is evaluated in `summarise_replications()` (`R/replication.R`), in the comparative scenario runner (`R/scenario_runner.R`) and everywhere in the analysis pipeline that reports an interval, so an interval quoted in the paper, one shown in the Shiny console and one written to a CSV output are the same quantity computed the same way.

Where a comparison sets two configurations run at the same control seed against each other, the interval is on the mean of the per-replication paired difference rather than on the difference of the two means.

### Replication Count Derivation

The best-determined spread available for this model is on the treated-cohort died-of-wounds rate, whose per-replication standard deviation is 0.0039 under the shipped configuration, measured over 150 replications. At that spread, a 95% half-width of 0.15 percentage points needs 29 replications, one of 0.10 pp needs 62, and one of 0.05 pp needs 237. The 50-replication mortality figures therefore carry roughly $\pm 0.11$ pp.

Single 50-replication measurements of one unchanged configuration span 0.132 pp across control seeds, which is why `scripts/check_dow_calibration.R` pools three independent measurements rather than reporting one.

### Warm-up Classification

No warm-up period is discarded from any observation window. The model is a terminating simulation with a fixed campaign length and a genuinely empty start, so the opening period is part of the quantity of interest rather than a transient to be removed. The README's Warm-up Period Analysis reports the Welch graphical diagnostic, run over ten 90-day replications, which supports that classification.

---

## Experiment Designs

<small>[Return to Top](#contents)</small>

### Comparative Scenario Analysis

50 replications of 30 simulated days per profile at control seed 42, under the shipped default establishment, the only overrides being those the scenario profile itself applies. Invoked as:

```
Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity --iterations 50 --days 30 --seed 42
```

A scenario profile is a named set of overrides applied on top of the shipped default `env_data.json` parameters, resolved by `resolve_scenario()` (`R/scenario.R`). Both profiles are defined in the `scenarios` block of `env_data.json`. Element, bed and transport fleet counts are structural configuration a scenario cannot override, so the two profiles differ in their casualty-generation parameters alone.

A third, Vietnam-calibrated profile is missing for want of sources: FORECAS's Appendix A has no standalone Vietnam combat-troop WIA/KIA distribution table, Table A.5 covering Vietnam DNBI only, so there are no genuinely FORECAS-sourced Vietnam parameters to build one from.

### The R2B Pre-Open Hold Window

50 replications of 30 simulated days per arm at control seed 42, under the shipped default configuration with one override, `r2b.surgery.pre_open_window_min` set to 0 in one arm against its shipped 60 in the other.

The two arms are not the same realisation. A zero-window run and a 60-minute run at the same control seed share their per-replication seeds, but the first hold shifts simmer's single global stream, and the force-regeneration loop feeds arrival timing back from casualty event timing, so the two arms drift into different casualty streams. Not one of the 50 replication pairs generated the same number of casualties in both arms, and the paired difference in total casualties spans −32.00 to +12.72. Pairing on the seed therefore removes none of the between-run variance, and an effect of about six operations disappears into the noise of a response whose paired difference has a standard deviation of 11.

Those standard deviations set the replication count the comparison would need. Resolving the forward-surgery effect to a half-width of two operations takes about 120 replications per arm. The bypass and R2E surgery rows, whose paired differences carry standard deviations of 27 and 32, would take several hundred to a few thousand.

Two further limits apply to the design. The comparison was run at the shipped default configuration only, so it says nothing about the window under surge. And 60 minutes is a single point on a range the screening bounds take from zero to six hours.

### The Post-Operative Intensive Care Gate

50 replications of 30 simulated days at an independent seed per replication (`seed = NULL`), under the shipped default configuration, run before and after the gate was introduced. The intervals in this experiment alone were computed over antithetically paired replications while the interval still divided by the replication count (see The Withdrawn Antithetic Pairing above).

The mortality mechanism was confirmed separately by a stress test that forced intensive care capacity to zero over a 90-day run. The degraded route then carries most casualties and produces measurable post-operative deaths, which establishes that the checkpoint fires as designed without establishing that the effect is quantitatively resolved at Falklands-calibrated rates.

### Forward ICU Share Decision Frontier

20 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: `r2b_icu_share` set to 0, 0.25, 0.5, 0.75 and 1.0 in turn. Point 0 is the shipped default. Run via:

```
Rscript scripts/run_icu_share_sweep.R --iterations 20 --days 30
```

Forward intensive care utilisation is poorly determined at this replication count: it reads 22.4% at a zero share, where the beds serve the evacuation wait alone, then moves between 14.1% and 22.7% in no particular order once forward holding is enabled. Too few events per replication go into that column for it to be well determined.

### Transport Fleet-Size Sweep

10 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: the PMV Ambulance fleet swept across 1 to 5 vehicles and the HX2 40M fleet across 1 to 4, each with the other fleet held at its shipped establishment size.

`plot_transport_capacity_margin_by_fleet_size()` (`R/analysis.R`) rebuilds the environment at each sweep point via `build_environment()` and runs the same replication engine the comparative scenario runner uses. Run via `Rscript scripts/run_transport_sweep.R`. `outputs/transport_capacity_by_fleet_size.csv` holds the full per-point results, including the interval bounds omitted from the paper's table.

Mean utilisation across the swept range runs the wrong way on both platforms, rising with fleet size where a fixed demand spread over more vehicles should lower it, and the interval on HX2 40M utilisation at three vehicles spans 2.3% to 19.9%. So few transport events occur per replication that the busy-time estimate at each sweep point is barely pinned down.

### Force Regeneration Under Reinforcement

15 replications per row at `moderate_intensity` and 12 at `high_intensity`, each of 30 simulated days, with daily casualty volume averaged across replications and fitted with an ordinary least-squares trend against simulation day. The unreinforced rows use the shipped default (`force_regeneration.reinforcement.demand_interval_days = 0`, which disables the mechanism). The reinforced rows override it with a 7-day demand submission cycle, a 7-day fulfillment lag and the shipped default triangular fill distribution (`fill_min_frac = 0.2`, `fill_mode_frac = 0.85`, `fill_max_frac = 1.1`).

| Scenario | Reinforcement | Daily volume slope | p-value | First-week mean | Last-week mean |
|---|---|---|---|---|---|
| `moderate_intensity` (15 reps) | None | −0.018/day | 0.75 | 14.9 | 14.4 |
| `moderate_intensity` (15 reps) | 7-day demand cycle, 7-day lag | −0.103/day | 0.17 | 16.6 | 14.1 |
| `high_intensity` (12 reps) | None | −0.349/day | 0.0027 | 39.8 | 29.5 |
| `high_intensity` (12 reps) | 7-day demand cycle, 7-day lag | +0.030/day | 0.80 | 35.8 | 36.1 |

At `high_intensity` casualty rates daily volume falls significantly without reinforcement, declining 26% from the first week to the last, and the demand-cycle configuration removes that decline entirely, leaving a slope indistinguishable from flat. At `moderate_intensity` neither configuration shows a slope distinguishable from flat, that profile's casualty volume being too low for 30 days of attrition to deplete the force measurably.

This experiment is recorded here rather than in the paper. It measures the force generation mechanism that drives casualty arrivals rather than the performance of the health system, so it informs the simulation's construction rather than a planning decision about the trauma system.

`force_regeneration.reinforcement` is entirely for the planner to set, and this project makes no attempt to balance it automatically against a scenario's attrition rate. The 7-day/7-day configuration illustrates the mechanism; it is not a recommended operational setting.

### Mass Casualty Event Stress Test

10 replications of 30 simulated days at control seed 42, under the shipped default configuration with one override: `mass_casualty.event.rate_per_day` set to 0.2 events per day, a mean of five days between events, against a background-only arm at the shipped value of 0.

Injection ships disabled, so everything in this experiment needs that override, the illustrative single run and `images/mass_casualty_events.png` included, and none of it can be reproduced by a shipped-configuration run. That makes this figure the one tracked image `run.R --refresh-baseline` cannot write; it is copied into place from the run's own output directory.

The casualty count drawn for an event is its total, not the number of survivors. A configured share, `mass_casualty.event.kia_fraction`, arrives killed at or near the point of injury and goes to the mortuary pathway rather than through triage. At the shipped share of 0.28, a 30-day run at this rate and seed produces 78 event-derived casualties, 27 killed outright and 51 wounded.

The comparison sorts casualties by origin rather than by a strict time window around each event (see the assumption note in `R/analysis.R`). One seed-42 run under the same override produced 537 total casualties, 459 from the background streams and 78 from two events, one of 33 casualties on day 14 and one of 45 on day 27. The gap-based reconstruction the analysis pipeline applies recovers both exactly; its known failure mode, reading two closely spaced events as one, needs a busier event schedule than this seed produced.

---

## Provenance

<small>[Return to Top](#contents)</small>

The comparative scenario figures, and `images/scenario_comparison.png` with them, were produced in the project's pinned development container (`rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`). The seed-42 evidence set the companion verification paper reports reproduces byte for byte in that same container, and `scripts/check_baseline_reproduction.R` re-checks the reproduction whenever the model changes.

The `moderate_intensity` profile gives 437.8 total casualties per run against the 530 of the documented seed-42 single run. That single run sits inside the profile's own 10th-to-90th-percentile range of 362.7 to 528.0 rather than near its mean, which is what one draw from a wide distribution does; the replicated and single-campaign results agree. Every casualty, mortality and queue figure in the paper's comparison tables reproduces exactly when the comparison is re-run at this seed, inside the pinned container and outside it alike.
