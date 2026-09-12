# Experimental Design and Statistical Method for a Replicated Simulation of the Land-Based Trauma System

## Abstract

<small>[Return to Top](#contents)</small>

This document is the design record for the replicated experiments reported in the companion paper on the land-based trauma system [[1]](#references). For each of seven experiments it states the replication count, the campaign horizon, the control seed, the parameters overridden against the shipped default configuration, and the command that invokes it. For the replication framework beneath them it states how independence between replications is established, how confidence intervals are constructed, how many replications a given resolution requires, and why no warm-up period is discarded. Replications are independent by construction rather than by measurement, each being a pure function of a distinct seed, and every property the framework rests on is asserted by a regression check that runs whenever the model changes. Antithetic pairing was trialled as a variance reduction scheme and withdrawn. No finding about the performance of the trauma system appears here, with one exception: the force regeneration comparison measures the mechanism generating casualty arrivals rather than the system treating them, and is reported in full. A reader can reproduce any result the companion paper states, or cost the further runs an unresolved one would need.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Introduction](#introduction)
- [The Replication Framework](#the-replication-framework)
  - [Independence of Replications](#independence-of-replications)
  - [The Withdrawn Antithetic Pairing](#the-withdrawn-antithetic-pairing)
  - [Interval Construction](#interval-construction)
  - [Replication Count and Resolution](#replication-count-and-resolution)
  - [Warm-up Classification](#warm-up-classification)
- [The Sustained-Operations Horizon](#the-sustained-operations-horizon)
  - [The Protocol](#the-protocol)
  - [Stability, and What It Means for Warm-up](#stability-and-what-it-means-for-warm-up)
  - [Which Findings Are Horizon-Limited](#which-findings-are-horizon-limited)
  - [The Checks That Defend These Properties](#the-checks-that-defend-these-properties)
  - [A Replication Lost to Its Host](#a-replication-lost-to-its-host)
- [Experimental Designs](#experimental-designs)
  - [Comparative Scenario Analysis](#comparative-scenario-analysis)
  - [Campaign Time Series of Queue Length and Degraded Care](#campaign-time-series-of-queue-length-and-degraded-care)
  - [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)
  - [The Post-Operative Intensive Care Gate](#the-post-operative-intensive-care-gate)
  - [Forward ICU Share Decision Frontier](#forward-icu-share-decision-frontier)
  - [Transport Fleet-Size Sweep](#transport-fleet-size-sweep)
  - [National Support Base Demand and the Airlift Schedule](#national-support-base-demand-and-the-airlift-schedule)
  - [Strategic Airlift Reliability Sweep](#strategic-airlift-reliability-sweep)
  - [Mass Casualty Event Stress Test](#mass-casualty-event-stress-test)
- [Force Regeneration Under Reinforcement](#force-regeneration-under-reinforcement)
- [Provenance](#provenance)
- [Limitations of the Designs Recorded Here](#limitations-of-the-designs-recorded-here)
- [Conclusion](#conclusion)
- [References](#references)
<!-- TOC END -->

---

## Introduction

<small>[Return to Top](#contents)</small>

This project reports its work across four documents. The first traces a single campaign end to end and verifies that the model behaves as its specification describes [[2]](#references). The second reports the replicated experiments and the planning options that follow from them [[1]](#references). Behind both sits a system reference describing how the simulation is built, what each trajectory does and which assumptions it rests on [[3]](#references). This document is the fourth, and it serves the second: it holds the experimental designs and the statistical derivations that the companion paper states the conclusions of.

The division is one of audience rather than of subject. A planner reading the companion paper needs to know that three ambulances are sufficient and that the evidence for it is a queue that collapses between one and two vehicles. A reader auditing that recommendation needs to know that it rests on ten replications per fleet size at one control seed and one casualty intensity, that the utilisation column of the same sweep is too poorly determined to read, and which script produces both. The first reader is served by omitting that detail and the second by recording it, so it is recorded here.

Every measured result about the performance of the trauma system belongs to the companion paper, and the one experiment reported here in full is the exception the abstract names: force regeneration under reinforcement informs how the simulation is constructed rather than any planning decision about the health system.

The document is presented in four parts. The first describes the replication framework every experiment runs through and the statistical basis of every interval reported from it. The second gives the design of each experiment in turn. The third reports the force regeneration experiment. The fourth records the provenance of the figures and the limitations the designs carry.

---

## The Replication Framework

<small>[Return to Top](#contents)</small>

Every experiment runs through the project's multi-run replication framework (`run_replications()`, `R/replication.R`), which executes a requested number of independent replications of the model and returns their monitoring data with a replication index attached. The model itself is built on the `simmer` discrete event simulation package for R [[4]](#references).

The unit of analysis is the replication throughout. Every response is reduced to one number per replication, such as the mean queue at a resource across the 30 days or the count of casualties taking a given pathway, before any statistic is taken across replications. Observations within one campaign are not independent of one another, so no interval in this project is ever taken across casualties or across days.

### Independence of Replications

Independence follows from how a replication is built rather than being inferred from the output. Two facts establish it.

First, `run_once()` (`R/replication.R`) is a pure function of its seed: re-running a seed reproduces its output exactly, whether or not another run happens in between, so the seed is all that distinguishes two replications and no other channel connects them. Second, `run_replications()` draws a distinct seed for each replication, taking them in one call from the caller's control seed before any replication begins. Independent seeds into a pure function give independent replications, which is a deterministic argument rather than one a finite sample of correlations could support.

Running replications in parallel preserves both facts. They are dispatched through `mclapply()` under `RNGkind("L'Ecuyer-CMRG")` with `mc.set.seed = TRUE`, which gives each worker its own substream of the MRG32k3a generator [[5]](#references). The substream spacing of $2^{76}$ [[6]](#references) makes overlap impossible at any simulation budget used here. Both dispatch paths, the parallel one and the serial fallback used at a single replication and on platforms without forking, run under the one generator, so a replication's output depends on its seed and not on the path or the core count that produced it.

The framework also captures the caller's generator kind and stream position on entry and restores both on exit. Without that, the first measurement taken in a session would draw its seeds under a different generator from the second, and a measurement would depend on what preceded it rather than on its control seed alone.

### The Withdrawn Antithetic Pairing

No variance reduction scheme sits on top of that framework. Antithetic pairing, in which each replication is paired with a second run driven by the complements of its uniform random numbers so that the two errors partly cancel [[7]](#references), was used earlier in the project and then withdrawn, because neither of the conditions it needs holds for this model.

Its reach stops at the arrival generators, since `simmer` draws service times and routing probabilities from the global stream inside its own event loop, in an order set by event timing rather than by entity. The technique also requires the response to move consistently in one direction with the input uniforms, and casualty arrivals do not, their rate being scaled by an effective force size that trajectory outcomes themselves add to and subtract from. Measurement agrees: over 75 pairs the within-pair correlation on total casualties is $-0.04$ (95% confidence interval $[-0.27, +0.19]$), worth a variance reduction of about 3% and indistinguishable from none.

One consequence reached the companion paper and has since been discharged. The intervals reported for the post-operative intensive care gate were computed while the pairing was still in force, over replications that were not independent, and were narrower than those runs entitled them to be. They were carried forward rather than recomputed because the comparison is against a configuration that no longer existed in the codebase. It does now: `icu_gating.enabled` reconstructs the earlier arm as a supported configuration, and the experiment has been re-run over independent replications. The pairing leaves no uncorrected interval in either document.

### Interval Construction

Every confidence interval reported by this project is a Student $t$ interval on the mean across replications [[8]](#references),

$$\bar{x} \pm t_{0.975,\;n-1}\,\frac{s}{\sqrt{n}}$$

where $n$ is the replication count, $\bar{x}$ the mean across replications of the per-replication response and $s$ its sample standard deviation. No normal approximation and no bootstrap appears anywhere in the companion paper or here.

The same expression is evaluated in `summarise_replications()` (`R/replication.R`), in the comparative scenario runner (`R/scenario_runner.R`) and everywhere in the analysis pipeline that reports an interval, so an interval quoted in the paper, one shown in the Shiny console and one written to a CSV output are the same quantity computed the same way. Where a comparison sets two configurations run at the same control seed against each other, the interval is on the mean of the per-replication paired difference rather than on the difference of the two means.

Several tables in the companion paper also carry a 10th-to-90th-percentile range. That is the empirical spread across replications rather than an interval on the mean, and it is not derived from the expression above.

### Replication Count and Resolution

How many replications an experiment needs depends on the spread of its response, and the responses this model reports differ by orders of magnitude in how many events they accumulate. A mean queue integrates over every arrival and departure at a resource across 30 days. A died-of-wounds count rests on a handful of events per campaign.

The best determined spread available for this model is on the treated-cohort died-of-wounds rate, the rate among casualties reaching R2B or R2E, whose per-replication standard deviation is 0.0039 under the shipped configuration, measured over 150 replications. Setting the half-width of the interval above equal to a target and solving for $n$ gives the counts a given resolution requires.

| Target 95% half-width | Replications required |
|---|---|
| 0.15 percentage points | 29 |
| 0.10 percentage points | 62 |
| 0.05 percentage points | 237 |

The 50-replication mortality figures reported in the companion paper therefore carry roughly $\pm 0.11$ percentage points. That is enough to separate two casualty intensities whose rates differ eightfold and not enough to separate two treatment pathways within one intensity, which is why the intensive care gate's mortality comparison is reported as unresolved. Measured on the paired difference between that experiment's two arms, deaths of wounds carry a standard deviation of 1.46 per replication against a difference of 0.060, so an interval whose half-width matched the observed effect would require roughly 2,283 replications.

Single 50-replication measurements of one unchanged configuration span 0.132 percentage points across control seeds, which is a direct illustration of the same arithmetic and is why `scripts/check_dow_calibration.R` pools three independent measurements rather than reporting one.

### Warm-up Classification

No warm-up period is discarded from any observation window. Discarding one removes the settling-in behaviour a model shows before it reaches steady state, which is worth doing only where steady state is the quantity of interest [[9]](#references). This is a terminating simulation with a fixed campaign length and a genuinely empty start, so the opening period is part of the quantity of interest rather than a transient to be removed: a deploying health system really does start empty, and how it copes while filling is a planning question in its own right.

That classification is supported rather than assumed. The Welch graphical diagnostic, run over ten 90-day replications and reported in the system reference [[3]](#references), shows the behaviour a terminating model is expected to show, and `scripts/run_warmup.R` re-runs it on demand. It is also re-derived at a horizon twelve times the experiments' own, which establishes both that a steady state exists for every response at moderate intensity and that it does not for four of them at high; see [Stability, and What It Means for Warm-up](#stability-and-what-it-means-for-warm-up).

## The Sustained-Operations Horizon

<small>[Return to Top](#contents)</small>

Every experiment reported in the companion paper runs for 30 days, a window inherited from the campaign the baseline models rather than chosen by measurement. A window that short cannot distinguish a system in equilibrium at a given load from one thirty days into a divergence, and the model has responses of both kinds. This section defines a second horizon alongside the first and states which question belongs to which.

### The Protocol

<!-- PROTOCOL days=360 -->
<!-- PROTOCOL replications=30 -->
<!-- PROTOCOL block_days=30 -->
Thirty replications of a 360-day campaign at control seed 42, at each of the two casualty intensities, under the shipped default establishment and the shipped default configuration. Results are reported as means over twelve consecutive 30-day blocks. Invoked as:

```
Rscript scripts/run_long_horizon.R --refresh-baseline
```

**Force regeneration is part of the protocol, not a default it inherits.** With reinforcement disabled, arrival rate is proportional to effective force size, so a long run loses combat power and casualty arrivals decay with it; that measures a force being consumed rather than a system under sustained load, and the two are different questions that the same duration answers differently. The protocol runs at the shipped 7-day reinforcement cycle, which holds the force near establishment. The measurement confirms the intent rather than assuming it: arrivals per day are 14.20 in block one and 14.91 in block twelve at moderate intensity, and 34.89 against 35.17 at high, so the casualty stream is stationary and any movement in a downstream response is the system's and not the stream's.

**One long run answers the question for every shorter horizon, in distribution rather than run for run.** Each block's mean is computed from that block's days alone, so twelve block means come out of one run at no extra cost and running separate durations would cost several times as much to say the same thing. What the reduction does not give, and what an earlier draft of this protocol wrongly claimed, is that block one of a long run reproduces a standalone 30-day run at the same seed. It does not. The arrival streams are force-size-reactive closures sampled by thinning, as set out under [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window), over the horizon the run requests, so changing the horizon changes the draws and two runs of different length at one control seed diverge from their first arrival: at a four-day and a two-day run of the same seed, 25 arrivals fall in the first two days against 34. Block one is therefore a valid sample of the 30-day window and not the same campaign as a 30-day experiment, which is why the protocol compares block means across thirty replications rather than one run against another.

`scripts/check_long_horizon_protocol.R` asserts both halves of that: that reducing one run's monitors over a shorter horizon reproduces the shorter horizon's days exactly, which is the property the block means rest on, and that a long run and a short one at one seed are different campaigns, so the stronger claim cannot be reinstated from the shape of the block means.

**Each replication is reduced before the next begins.** `run_long_horizon()` (`R/long_horizon.R`) reduces a finished replication to a daily series inside the forked worker that produced it and discards the monitoring data there, so the parent process holds the series rather than the monitors. Peak memory is then set by one replication rather than by their sum: measured across the process tree, a full protocol run holds at about 2.7 GB. The reduction is not an optimisation of a working arrangement but the thing that makes the run possible, the unreduced monitoring set at this horizon and replication count being some two orders of magnitude larger than any analysis of it needs.

The responses the protocol covers are the queue and the occupancy of each of five bed pools, and three system quantities: casualty arrivals, deaths of wounds, and the strategic evacuation backlog, this last being the number of casualties whose evacuation had been decided and who had not yet departed. The tracked evidence set is `data/long_horizon/`.

### Stability, and What It Means for Warm-up

A response's stability is classified from its block means rather than read off a figure. The trend is fitted over the **second half** of the horizon, not over all of it, because a response decaying from an opening transient to a steady level and one growing without bound both have a non-zero slope across the whole horizon and are the two cases the classification exists to separate. A run starts from an empty system, so a decaying opening is expected and is not evidence against convergence; what distinguishes the two is whether the response is still moving once that opening has passed. A response is reported as converged where the late slope's interval spans zero, or where it does not but the drift is under one percent of the response's own late level per block; as drifting otherwise; and as degenerate where it never moved at all, which carries no information about convergence and would otherwise be reported as a perfectly determined zero trend.

**At moderate intensity every response converges. At high intensity four do not.**

| Response | Moderate intensity | High intensity |
|---|---|---|
| R2E operating theatre queue | converged, +1.2%/block | **drifting, +11.3%/block**, 85.2 to 1,725.9 |
| R2E holding bed queue | converged, -10.2%/block | **drifting, +12.3%/block**, 33.3 to 816.5 |
| R2E intensive care queue | converged, -3.5%/block | **drifting, +16.8%/block**, 2.5 to 228.2 |
| Strategic evacuation backlog | converged, -4.6%/block | **drifting, +13.8%/block**, 24.1 to 548.7 |
| R2B holding bed queue | converged, -0.1%/block | converged, +0.5%/block |
| Casualty arrivals per day | converged, +0.5%/block | converged, -0.2%/block |
| Deaths of wounds per day | converged, -3.0%/block | converged, -1.5%/block |

The drift percentages are per 30-day block and are taken over the second half of the horizon, so the four drifting responses are still growing at those rates in the twelfth block rather than decelerating towards a level. Occupancy explains why: at high intensity the three R2E pools reach 1.00 by block two and stay there, so the queues grow because the pools have nothing left to give.

This changes what a warm-up period can mean here, and it changes it differently for the two classes.

For a converged response the classification also reports the first block from which the response never again leaves a band around its late level, the band being two standard deviations of its own variation across the late blocks. Scaling the band to the response's own variation rather than to its level is what makes the statement comparable across a queue averaging half a casualty and one averaging fifty, and it is the form the question takes: whether the opening blocks are distinguishable from the ordinary variation the response shows once settled. **Every converged response settles by block seven, and most by block one or two**, so no converged response carries an opening transient that reaches far into the horizon.

For a drifting response no warm-up period exists to be measured. Welch's procedure presumes the response converges to a steady state whose initial transient is being removed, and a response with no steady state does not satisfy that premise: a period chosen for it would not be a transient being discarded but an arbitrary truncation of a trend. The four drifting responses are therefore reported as having no steady state rather than as having a warm-up period of zero, which is a different claim.

`WARM_UP_DAYS` remains 0, and the basis for it is unchanged and is not the one this measurement bears on. Nothing is discarded because the companion paper's experiments are terminating simulations with a fixed campaign length and a genuinely empty start, so the opening period is part of the quantity of interest rather than a transient to be removed. What this measurement adds is that the classification is now correct for the right reason at both intensities: at moderate intensity a steady state exists and the opening reaches it quickly, so discarding nothing costs little; at high intensity no steady state exists for the R2E pools, so there is nothing a warm-up period could be measured against, and discarding an opening period would remove real campaign behaviour without removing a transient.

### Which Findings Are Horizon-Limited

Every experiment in the companion paper runs at 30 days, which is block one of this protocol, and the audit of what that costs follows from the classification above.

**At moderate intensity, no published finding is horizon-limited.** Every response converges and most settle within the first two blocks, so block one is a representative window and the 30-day figures describe the system rather than a moment in its history.

**At high intensity, every figure resting on an R2E queue or on the evacuation backlog is a point on a trend rather than a level.** The companion paper's mean R2E theatre queue of 43.13 at high intensity is block one's value; block twelve's is 1,725.9. That does not make the published figure wrong, and it does not change the paper's conclusions, which rest on the comparison between intensities and on the fact that the queue does not clear. It does mean no high-intensity queue figure should be read as a level the establishment could be sized against, because there is no such level within a simulated year. The companion paper states this in its Limitations.

The sensitivity screens carry the same qualification and it is already recorded: the published Morris and Sobol rankings are 30-day rankings, labelled as such, and whether a parameter's influence differs at length is unresolved. Re-screening at the sustained horizon is a separate piece of work whose cost, roughly nineteen hours at 30 days, would rise with the horizon.

### The Checks That Defend These Properties

Each property above is asserted by a regression check that runs on every pull request, so a change to the framework that broke one would fail the gate rather than silently altering every interval the project reports.

| Property | Check |
|---|---|
| A replication is a pure function of its seed, and each replication is given a distinct seed | `scripts/check_replication_independence.R` |
| A measurement depends on its control seed alone, repeats at that seed, and leaves the caller's generator as it found it | `scripts/check_measurement_reproducibility.R` |
| A configuration error inside a sweep, a screen or the scenario runner leaves the global configuration at its pre-call values | `scripts/check_config_restore.R` |
| The analysis pipeline is idempotent and does not advance the caller's random number stream | `scripts/check_analysis_idempotence.R` |
| A run losing any replication stops rather than reporting the survivors, and a count that is reported is the count that contributed | `scripts/check_replication_loss_reporting.R` |

### A Replication Lost to Its Host

A replication whose worker process is killed outright, which on a memory-constrained host is the way one fails, cannot be recovered. **A run that loses any replication stops rather than reporting the survivors.**

The alternative is tempting and was rejected on inspection. Losing two of fifty looks like a smaller sample rather than a spoiled one, the survivors remaining independent draws, and on that reading the cost is precision alone and the interval reports it honestly. That reasoning assumes the replications that die are a random subset of those dispatched, and nothing here establishes it. A worker is killed because the host exhausted its memory, the operating system takes the largest process, and a replication generating more casualties carries more monitoring data than one generating fewer. The losses therefore skew toward the heavier campaigns, and the survivors are biased low on queue depth, occupancy and mortality, which are the responses this model exists to report. No interval computed from the survivors reveals that, an interval describing only the spread of what survived.

How large the bias would be has not been measured, and it is bounded: the response-dependent part of a replication's footprint is a few megabytes of monitoring data against a fixed cost per call of roughly 175 MB, so most of what the killer weighs is the same for every replication. That consideration points to a small effect. A second one, that losses arrive in batches mixing heavy campaigns with light ones, no longer holds: the dispatcher now gives each replication its own process rather than pre-dividing them among workers, so a loss is exactly one replication and carries whatever that replication's size implied. The case for a small bias is therefore narrower than it was, resting on the fixed cost alone, which is the more robust of the two arguments but is now unaccompanied. Neither makes the bias zero, and a published interval should not rest on an unmeasured assumption pointing in the unfavourable direction.

The threshold is a parameter rather than a prohibition. A caller who would rather lose one design point than lose a four-day sensitivity screen can raise it at the call site, which makes that an explicit choice on a run whose cost justifies it rather than a silent default on every run.

Every count this document and its companion paper carry is the count that contributed, not the count requested. The two are the same on a run that loses nothing, which is now the only run that reports at all unless a caller has deliberately accepted otherwise. `run_replications()` returns both so that a caller cannot report one while computing from the other, which is the form the original defect took: the metrics came from the survivors and the label came from the request.

---

## Experimental Designs

<small>[Return to Top](#contents)</small>

Each design below states its replication count, its campaign horizon, its control seed and the parameters it overrides. Three conventions hold throughout unless a design says otherwise. Every run is a complete 30-day campaign from an empty system. Every run uses the shipped default establishment, the element, bed, team and transport fleet counts defined in the `elms` structure of `env_data.json`, which no scenario profile can override. And every arm of a comparison differs from the shipped default configuration in the named parameters alone, so that the difference between two arms is attributable to those parameters.

A control seed is the seed given to the framework, from which the per-replication seeds are drawn. Two arms run at the same control seed therefore share their per-replication seeds without sharing their realised casualty streams, for the reason set out under [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window) below.

### Comparative Scenario Analysis

50 replications of 30 simulated days per profile at control seed 42, under the shipped default establishment, the only overrides being those the scenario profile itself applies. Invoked as:

```
Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity --iterations 50 --days 30 --seed 42
```

A scenario profile is a named set of overrides applied on top of the shipped default `env_data.json` parameters, resolved by `resolve_scenario()` (`R/scenario.R`). Both profiles are defined in the `scenarios` block of `env_data.json`. Element, bed and transport fleet counts are structural configuration a scenario cannot override, so the two profiles differ in their casualty-generation parameters alone.

Both profiles draw their casualty rates from the FORECAS projection study [[10]](#references), which selects a distribution family by battle intensity rather than by named conflict: lognormal incidence at moderate intensity, exponential at high. The `moderate_intensity` profile overrides the died-of-wounds ceilings and treatment efficacy factors alone, the base configuration's casualty generation parameters already being Falklands-sourced, while the base values for those two groups are drawn from the era of tactical combat casualty care and damage control resuscitation and would misattribute modern technique to a 1982 campaign. The `high_intensity` profile overrides casualty generation and the died-of-wounds model together, the latter calibrated to reported Okinawa experience [[11]](#references), and inherits its triage priority split, disease composition and transport times from the base configuration. That inheritance is recorded in the profile's own `notes` field and is the reason the profile is described as partially calibrated.

A third, Vietnam-calibrated profile is missing for want of sources: FORECAS's Appendix A has no standalone Vietnam combat-troop wounded-in-action or killed-in-action distribution table, Table A.5 covering Vietnam disease and non-battle injury only, so there are no genuinely FORECAS-sourced Vietnam parameters to build one from.

### Campaign Time Series of Queue Length and Degraded Care

50 replications of 30 simulated days per casualty intensity at control seed 42, under the shipped default establishment and the same two scenario profiles as the comparative analysis above. The seed is set once before each intensity rather than once for the pair, so replication $i$ of one intensity draws the same per-replication seed as replication $i$ of the other. Invoked as:

```
Rscript scripts/render_time_series_figures.R --run --refresh-baseline --iterations 50 --days 30
```

Measurement and rendering are separated. The `--run` half executes the model and writes three aggregated series to `data/time_series/`: queue length per resource pool, replication and four-hour bin; the two clearance statistics per pool and replication; and the degraded-care rate per stage, replication and day. An invocation without `--run` reads those files and renders the figures from them alone, so both images are a function of tracked data rather than of a run that cannot be repeated, and re-rendering reproduces them byte for byte. Only the aggregated series is kept; the monitoring data behind it runs to hundreds of megabytes and nothing published derives from a single replication of it.

A pool's queue is not recorded by the simulation and cannot be read off any single monitor row, each bed's queue being monitored separately. It is recovered by differencing each bed's own series into changes and accumulating those changes in time order, which gives the exact pool total after every event rather than an interpolation of it. The binned value is then the time-weighted mean of that step function over the bin, computed from its cumulative integral, so a peak falling entirely inside a bin is carried by the bin's value; sampling the step function at the bin edges instead would report whatever the queue happened to be at one instant every four hours and would miss such a peak entirely.

The two statistics the companion paper quotes in prose, the share of the campaign a pool's queue stood empty and the longest unbroken run it did not, are computed from the unbinned step function, so neither depends on the bin width the figure is drawn at. The figures report the median across replications with the interquartile range around it rather than a mean with a confidence interval. The question they are drawn to answer is what a campaign looks like, and an interval on the mean narrows as replications are added, which would imply an agreement between campaigns that the replications do not show; the quartile band does not move with the replication count and is the spread itself.

`scripts/check_time_series_figures.R` defends the arrangement. It asserts that the tracked series covers every resource pool, pathway stage and casualty intensity the model defines at the stated replication count and horizon, that the clearance statistics are internally consistent, and that every clearance percentage the companion paper states in prose matches the tracked measurement. Those three compare two derived artifacts against one another and would pass on two copies of the same error, so the check also exercises the step-function estimators on inputs whose answers are computable by hand, which is the assertion that keeps the others from being circular.

### The R2B Pre-Open Hold Window

50 replications of 30 simulated days per arm at control seed 42, under the shipped default configuration with one override, `r2b.surgery.pre_open_window_min` set to 0 in one arm against its shipped 60 in the other.

The two arms are not the same realisation. A zero-window run and a 60-minute run at the same control seed share their per-replication seeds, but the first hold shifts `simmer`'s single global stream, and the arrival streams are force-size-reactive closures sampled by thinning [[12]](#references) whose rate the force regeneration loop feeds back from casualty event timing, so the two arms drift into different casualty streams from the first hold onward. Not one of the 50 replication pairs generated the same number of casualties in both arms, and the paired difference in total casualties spans $-32.00$ to $+12.72$. Pairing on the control seed therefore removes none of the between-run variance, and an effect of about six operations disappears into the noise of a response whose paired difference has a standard deviation of 11.

Those standard deviations set the replication count the comparison would need. Resolving the forward-surgery effect to a half-width of two operations takes about 120 replications per arm. The bypass and R2E surgery rows, whose paired differences carry standard deviations of 27 and 32, would take several hundred to a few thousand.

Two further limits apply to the design. The comparison was run at the shipped default configuration only, so it says nothing about the window under surge. And 60 minutes is a single point on a range the screening bounds take from zero to six hours.

### The Post-Operative Intensive Care Gate

50 replications of 30 simulated days under the shipped default configuration, in two arms: the gate in force, and the gate disabled through `r2b.icu_gating.enabled` and `r2eheavy.icu_gating.enabled`, both set to zero. Disabling it reproduces the model as it stood before the gate existed, in that theatre entry does not depend on an intensive care bed being free and a casualty needing stabilisation is admitted to intensive care whether or not one is, queueing if none is. Reconstructing the earlier arm as a configuration rather than as a code state is what makes the experiment repeatable after a later model change; `scripts/check_icu_gate_switch.R` asserts that the disabled arm carries no deferral and no diverted recovery, and that both pathways are reachable when the gate is in force, so the first assertion is not vacuous.

Both arms run under one control seed, so each draws the same 50 per-replication seeds and replication $i$ of one is paired with replication $i$ of the other. Replications within an arm remain independent of one another, each taking its own seed, so each arm's own interval is unaffected by the pairing across arms; the paired difference between arms is reported as well, and is the more precise of the two comparisons for the same reason common random numbers are used in the scenario comparison. This supersedes an earlier measurement whose intervals were computed over antithetically paired replications while the interval still divided by the replication count, and which was therefore narrower than its runs entitled it to be.

The mortality mechanism was confirmed separately by a stress test that forced intensive care capacity to zero over a 90-day run. The degraded route then carries most casualties and produces measurable post-operative deaths, which establishes that the checkpoint fires as designed without establishing that the effect is quantitatively resolved at the died-of-wounds rates the moderate intensity profile is calibrated to.

### Forward ICU Share Decision Frontier

20 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: `r2b_icu_share` set to 0, 0.25, 0.5, 0.75 and 1.0 in turn. Point 0 is the shipped default. Run via:

```
Rscript scripts/run_icu_share_sweep.R --iterations 20 --days 30
```

Forward intensive care utilisation is poorly determined at this replication count: it reads 22.4% at a zero share, where the beds serve the evacuation wait alone, then moves between 14.1% and 22.7% in no particular order once forward holding is enabled. Too few events per replication go into that column for it to be well determined, and it should not be read as a trend.

### Transport Fleet-Size Sweep

10 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: the PMV Ambulance fleet swept across 1 to 5 vehicles and the HX2 40M fleet across 1 to 4, each with the other fleet held at its shipped establishment size.

`plot_transport_capacity_margin_by_fleet_size()` (`R/analysis.R`) rebuilds the environment at each sweep point via `build_environment()` and runs the same replication engine the comparative scenario runner uses. Run via `Rscript scripts/run_transport_sweep.R`. `outputs/transport_capacity_by_fleet_size.csv` holds the full per-point results, including the interval bounds omitted from the companion paper's table.

Mean utilisation across the swept range runs the wrong way on both platforms, rising with fleet size where a fixed demand spread over more vehicles should lower it, and the interval on HX2 40M utilisation at three vehicles spans 2.3% to 19.9%. So few transport events occur per replication that the busy-time estimate at each sweep point is barely pinned down, which is why the companion paper reads the queue column and not this one.

### National Support Base Demand and the Airlift Schedule

50 replications of a 30-day campaign at control seed 42 in each of thirteen configurations: the shipped configuration under each casualty intensity, six values of `role4.ame.failure_probability` from 0 to 0.40, and five values of `role4.ame.schedule_interval_days` from 3 to 14. The seed is set once before each configuration, so replication $i$ of every arm draws the same per-replication seed. Invoked as:

```
Rscript scripts/run_airlift_sweep.R --refresh-baseline
```

This is a different experiment from [Strategic Airlift Reliability Sweep](#strategic-airlift-reliability-sweep) below, which asks at a 360-day horizon whether a campaign collapses and reports a probability. This one asks what the schedule costs within the campaign the rest of the companion paper measures, and reports means with intervals. Each replication is reduced to one row of responses inside the forked worker that produced it, as the sustained-operations protocol reduces to a daily series, so thirteen configurations at fifty replications hold the responses rather than the monitoring data behind them. The tracked evidence set is `data/airlift/`.

Two properties of the measurement needed establishing before any of it could be reported.

**The Role 4 peak is drawn under the replication's own seed.** Each evacuated casualty's length of stay at the national support base is drawn by the analysis rather than by the simulation (`README.md`'s Further Development L30), so the census and the peak taken from it otherwise depend on how many draws preceded them in the session rather than on the campaign alone: the same finished campaign gives a peak of 117, 119 or 121 from three different stream positions. Drawing it under the replication's seed makes every response a function of that seed, which is what an interval across replications requires, and the caller's stream is restored so the measurement stays stream-neutral.

**A sortie scheduled at exactly the horizon is censored, not cancelled.** A sortie's outcome is reconstructed from the capacity it adds and the seats taken afterwards, and one scheduled at the instant the run ends never resolves. Counting it as a cancellation reported rates of 10%, 17% and 33% at a configured probability of zero in the sortie interval sweep, purely because 30 divides by 3, 5 and 10; the intervals that do not divide it correctly reported zero. Such a sortie is now dropped from both the scheduled and the flown count, after which the whole interval sweep reports zero and the reliability sweep tracks its configured values, measuring 6%, 10%, 17%, 25% and 41% against a configured 5%, 10%, 15%, 25% and 40%.

**The split of R2E holding occupancy is exact rather than estimated.** A casualty awaiting the standard airlift pool seizes a holding bed on reaching the evacuation decision and releases it on boarding, so its whole wait is holding occupancy; a casualty awaiting the critical pool holds an intensive care bed already seized upstream and contributes nothing to that pool. In-theatre recovery is then the remainder of the pool's measured occupancy rather than a second reconstruction, which makes the two components sum to the total by construction. What needs defending is the evacuation component, since an error there moves the same quantity out of recovery and the sum still holds, and `scripts/check_holding_occupancy_split.R` defends it: that it counts the standard route and not the critical one, asserted against a run carrying both; that it agrees with a casualty-by-casualty recount taken independently of the function under test; and that a wait still running when the window closes is charged to the window's end rather than dropped.

### Strategic Airlift Reliability Sweep

<!-- COLLAPSE days=360 -->
<!-- COLLAPSE replications=30 -->
<!-- COLLAPSE window_days=90 -->
<!-- COLLAPSE threshold=20 -->
<!-- COLLAPSE probabilities=0,0.05,0.1,0.15,0.2,0.25 -->
30 replications of 360 simulated days per arm at control seed 42, under the shipped default configuration with one override per arm: `role4.ame.failure_probability` at 0, 0.05, 0.10, 0.15, 0.20 and 0.25. The six arms draw from one seed vector, so replication $k$ of every arm runs the same parent stream and the arms are paired.

This is the longest experiment in the project, at 180 replication-years, and it is long by necessity rather than by preference. The response is a state a campaign reaches and does not leave, and the horizon has to exceed the time that state takes to develop: the effect is invisible at 30 days and only partly formed at 180. Sizing was governed by the replication count rather than the horizon, the quantity of interest being a probability rather than a mean, which needs roughly 217 replications for a half-width of five percentage points and returns only the wide intervals in the companion paper's table at 30.

The response is not the mean of anything. A campaign is classified collapsed where the R2E holding queue over its closing 90 days averages 20 casualties or more, and the reported quantity is the share of replications that collapse. A threshold classifier is used because the per-replication values are bimodal rather than spread: the highest clear run reaches 17.9 and the lowest collapsed run 84, so any threshold inside that gap returns the same count and the classifier's exact value is not a tuning choice. Reporting a mean over a bimodal population would describe no campaign in it.

Two properties of this experiment bear on how far its table can be read, and both are stated in the companion paper. Collapse is a property of a whole campaign rather than of any month within it, and a logistic fit of the outcome on the closing holding queue of the first 30 days does not distinguish the runs that go on to collapse ($p = 0.36$). And the median holding queue stays below 2 in five of the six arms, so the entire effect sits in a tail that a summary of typical performance does not show.

Invoked as:

```
Rscript scripts/run_airlift_collapse.R --refresh-baseline
```

Each replication is reduced to a daily series inside the forked worker that produced it and the series alone is returned, since holding thirty 360-day monitoring sets in memory is what would otherwise bound the experiment. The two capabilities the design needs exist separately in the other entry points, `scripts/run_long_horizon.R` running a reducing 360-day replicated campaign and `scripts/run_airlift_sweep.R` sweeping `role4.ame.failure_probability` at a 30-day horizon; this command is the two together with the collapse classification, which neither of them reports. `scripts/check_airlift_collapse_protocol.R` asserts that the parameters above are the ones the code holds, that the classifier averages each replication over the closing window's days at an inclusive threshold rather than reading the window's worst day, and that the tracked summary is the table the companion paper prints.

### Mass Casualty Event Stress Test

10 replications of 30 simulated days at control seed 42, under the shipped default configuration with one override: `mass_casualty.event.rate_per_day` set to 0.2 events per day, a mean of five days between events, against a background-only arm at the shipped value of 0.

Injection ships disabled, so everything in this experiment needs that override, the illustrative single run and `images/mass_casualty_events.png` included, and none of it can be reproduced by a shipped-configuration run. That makes this figure the one tracked image `run.R --refresh-baseline` cannot write; it is copied into place from the run's own output directory.

The casualty count drawn for an event is its total, not the number of survivors. A configured share, `mass_casualty.event.kia_fraction`, arrives killed at or near the point of injury and goes to the mortuary pathway rather than through triage. At the shipped share of 0.28, a 30-day run at this rate and seed produces 78 event-derived casualties, 27 killed outright and 51 wounded.

The comparison sorts casualties by origin rather than by a strict time window around each event (see the assumption note in `R/analysis.R`). One seed-42 run under the same override produced 537 total casualties, 459 from the background streams and 78 from two events, one of 33 casualties on day 14 and one of 45 on day 27. The gap-based reconstruction the analysis pipeline applies recovers both exactly; its known failure mode, reading two closely spaced events as one, needs a busier event schedule than this seed produced.

---

## Force Regeneration Under Reinforcement

<small>[Return to Top](#contents)</small>

This experiment is reported here rather than in the companion paper. It measures the force generation mechanism that drives casualty arrivals rather than the performance of the health system, so it informs the simulation's construction rather than a planning decision about the trauma system.

**Design.** 15 replications per row at `moderate_intensity` and 12 at `high_intensity`, each of 30 simulated days, with daily casualty volume averaged across replications and fitted with an ordinary least-squares trend against simulation day. The unreinforced rows set `force_regeneration.reinforcement.demand_interval_days` to 0, which disables the mechanism. The reinforced rows use a 7-day demand submission cycle, a 7-day fulfillment lag and the triangular fill distribution (`fill_min_frac = 0.2`, `fill_mode_frac = 0.85`, `fill_max_frac = 1.1`).

| Scenario | Reinforcement | Daily volume slope | p-value | First-week mean | Last-week mean |
|---|---|---|---|---|---|
| `moderate_intensity` (15 reps) | None | −0.018/day | 0.75 | 14.9 | 14.4 |
| `moderate_intensity` (15 reps) | 7-day demand cycle, 7-day lag | −0.103/day | 0.17 | 16.6 | 14.1 |
| `high_intensity` (12 reps) | None | −0.349/day | 0.0027 | 39.8 | 29.5 |
| `high_intensity` (12 reps) | 7-day demand cycle, 7-day lag | +0.030/day | 0.80 | 35.8 | 36.1 |

At `high_intensity` casualty rates daily volume falls significantly without reinforcement, declining 26% from the first week to the last, and the demand-cycle configuration removes that decline entirely, leaving a slope indistinguishable from flat. At `moderate_intensity` neither configuration shows a slope distinguishable from flat, that profile's casualty volume being too low for 30 days of attrition to deplete the force measurably.

The result matters to the designs above in one respect. Casualty volume at high intensity is not stationary across a 30-day campaign under the shipped configuration, so a response averaged over the campaign carries a declining force behind it. Every experiment in this document is run at the shipped default, which is to say without reinforcement, and the companion paper's comparison of the two intensities is a comparison of campaigns fought under that same condition.

`force_regeneration.reinforcement` is entirely for the planner to set, and this project makes no attempt to balance it automatically against a scenario's attrition rate. The 7-day demand cycle with a 7-day lag illustrates the mechanism; it is not a recommended operational setting.

---

## Provenance

<small>[Return to Top](#contents)</small>

The comparative scenario figures, and `images/scenario_comparison.png` with them, were produced in the project's pinned development container (`rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`). The seed-42 evidence set the companion verification paper reports [[2]](#references) reproduces byte for byte in that same container, and `scripts/check_baseline_reproduction.R` re-checks the reproduction whenever the model changes.

The `moderate_intensity` profile gives 437.8 total casualties per run against the 530 of the documented seed-42 single run. That single run sits inside the profile's own 10th-to-90th-percentile range of 362.7 to 528.0 rather than near its mean, which is what one draw from a wide distribution does; the replicated and single-campaign results agree. Every casualty, mortality and queue figure in the companion paper's comparison tables reproduces exactly when the comparison is re-run at this seed, inside the pinned container and outside it alike.

The three result figures of the companion paper are rendered from the values in its own markdown tables by `scripts/render_paper_figures.R` rather than from a second copy of the data, so a figure cannot disagree with the table it illustrates.

The two campaign time series figures are rendered from `data/time_series/`, the tracked aggregated series the measurement wrote, by `scripts/render_time_series_figures.R` with no further model execution, so the figures and the percentages the companion paper quotes from them derive from one measurement. The measurement itself was made in an unpinned R 4.3.3 sandbox rather than the pinned container; the figures it produced are reproducible from the tracked series in either environment, and it is the series rather than the images that a re-run in the pinned container would need to confirm.

---

## Limitations of the Designs Recorded Here

<small>[Return to Top](#contents)</small>

Four limitations are properties of the designs rather than of the model, and each bounds what the companion paper can claim from the experiment it applies to.

**A comparison of two configurations is not a controlled comparison.** Changing any setting alters the sequence of random draws, so the two arms generate different casualty streams and cannot be matched campaign for campaign. The consequence is a loss of precision rather than a bias: the means remain correct and the intervals are wider than a matched design would give. The scenario comparison is unaffected, its arms differing by design rather than by a small perturbation.

**Every design runs at one control seed.** A control seed determines the whole set of per-replication seeds, so a measurement at 50 replications is one draw from the distribution of 50-replication measurements. The 0.132 percentage point spread across control seeds recorded above is the size of that effect on the best determined response the model reports, and it is the reason the calibration check pools three independent measurements.

**Utilisation columns at 10 and 20 replications are not determined.** Both the transport sweep and the forward intensive care share sweep report utilisation figures that move without order across their swept range. Too few busy-time events accumulate per replication at those counts for the column to be read, and neither sweep's conclusion rests on it.

**The sweeps were run at moderate intensity only.** The transport fleet sweep, the forward intensive care share frontier and the pre-open hold window comparison all use the shipped default configuration, so none of them establishes that its result survives at the higher casualty intensity. Re-running them at high intensity is listed among the further development items of the companion paper [[1]](#references).

Beyond these, the simulation is verified rather than validated: it behaves as its specification describes, which is a separate question from whether that specification represents the real trauma system well [[13]](#references). No design recorded here addresses that question, and none can.

---

## Conclusion

<small>[Return to Top](#contents)</small>

This document records the experimental design of every replicated experiment this project has run and the statistical basis of every interval reported from them. Independence between replications is established by construction, a replication being a pure function of a distinct seed, and each property the framework depends on is asserted by a regression check that runs on every pull request. Intervals are Student $t$ intervals on the mean across replications throughout, and no warm-up period is discarded from any of them, the model being a terminating simulation of a campaign that genuinely starts empty.

The replication counts the experiments use follow from the spread of the responses they measure. A queue is well determined at 50 replications; a died-of-wounds count at the moderate casualty intensity is not, and the counts a given resolution would require are stated above so that any experiment reported as unresolved can be costed before it is re-run. Two further constraints bound what the designs support: no comparison of two configurations is perfectly controlled, and each measurement is taken at a single control seed.

A reader who wants to reproduce a result in the companion paper has the replication count, the horizon, the control seed, the parameter overrides and the invocation for it here. A reader who wants to extend one has the reason the present design stops where it does.

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Battlefield Casualty Handling project. (2026). *Surgical Hours, Not Operating Theatres: Sizing the Land-Based Trauma System for Large Scale Combat Operations*. Retrieved 07 Sep 26, from https://github.com/natosys/Battlefield-Casualty-Handling/blob/main/docs/Multi_Run_Analysis.md

[2] Battlefield Casualty Handling project. (2026). *Applying Discrete Event Simulation to the Land-Based Trauma System: Baseline Performance and System Constraints in a Single Campaign*. Retrieved 07 Sep 26, from https://github.com/natosys/Battlefield-Casualty-Handling/blob/main/docs/Single_Run_Analysis.md

[3] Battlefield Casualty Handling project. (2026). *Battlefield Casualty Handling*. Retrieved 07 Sep 26, from https://github.com/natosys/Battlefield-Casualty-Handling/blob/main/README.md

[4] Ucar, I., Smeets, B., & Azcorra, A. (2019). simmer: Discrete-Event Simulation for R. *Journal of Statistical Software*, *90*(2), 1–30. Retrieved 27 Aug 26, from https://doi.org/10.18637/jss.v090.i02

[5] Karl, A., Eubank, R., Milovanovic, J., Reiser, M., & Young, D. (2014). Using RngStreams for parallel random number generation in C++ and R. *Computational Statistics*, *29*(5), 1301–1320. Open-access preprint retrieved 26 Jun 26, from https://arxiv.org/abs/1403.7645

[6] R Core Team. (2024). *RNGstreams: L'Ecuyer's RngStreams for parallel random number generation*. R Documentation, parallel package. Retrieved 26 Jun 26, from https://stat.ethz.ch/R-manual/R-patched/library/parallel/html/RngStream.html

[7] Rossetti, M. D. (2023). *Simulation Modeling using the Kotlin Simulation Library (KSL)*, §9.2: Variance Reduction Techniques. Retrieved 26 Jun 26, from https://rossetti.github.io/KSLBook/

[8] Law, A. M. (2020). Statistical analysis of simulation output data: the practical state of the art. In *Proceedings of the 2020 Winter Simulation Conference* (pp. 1117–1127). INFORMS Simulation Society. Retrieved 27 Aug 26, from https://informs-sim.org/wsc20papers/134.pdf

[9] Rossetti, M. D. *Simulation Modeling and Arena*, Chapter 5: Statistical Analysis for Infinite Horizon Simulation Models. Retrieved 27 Aug 26, from https://rossetti.github.io/RossettiArenaBook/05-Chapter5.html

[10] Blood, C. G., Zouris, J. M., & Rotblatt, D. (1998). *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

[11] Marble, S. (2025). Both joint and not: Medical support at Okinawa, 1945. *Joint Force Quarterly*, *117*(2), article 11. National Defense University Press. Retrieved 17 Aug 26, from https://digitalcommons.ndu.edu/joint-force-quarterly/vol117/iss2/11/

[12] Lewis, P. A. W., & Shedler, G. S. (1979). Simulation of nonhomogeneous Poisson processes by thinning. *Naval Research Logistics Quarterly*, *26*(3), 403–413. Naval Postgraduate School Calhoun repository. Retrieved 13 Aug 26, from https://calhoun.nps.edu/handle/10945/63159

[13] Sargent, R. G. (2010). Verification and validation of simulation models. In *Proceedings of the 2010 Winter Simulation Conference* (pp. 166–183). IEEE. Retrieved 27 Aug 26, from https://www.informs-sim.org/wsc10papers/016.pdf

<!-- REFERENCES END -->
