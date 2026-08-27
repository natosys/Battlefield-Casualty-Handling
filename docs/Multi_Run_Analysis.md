# Battlefield Casualty Handling: Replicated Experiments and Comparative Scenario Analysis

## Abstract

<small>[Return to Top](#contents)</small>

This document reports every experiment the Battlefield Casualty Handling discrete event simulation has been subjected to under replication, together with the replication and confidence interval methodology they rest on. Its centrepiece is a comparative analysis of two named casualty-rate scenario profiles at 50 replications each: `moderate_intensity` (a Falklands 1982-modified baseline, the casualty rate underlying the single-run verification in `docs/Single_Run_Analysis.md`) and `high_intensity` (an Okinawa exemplar, its casualty rates calibrated from FORECAS Tables A.7/A.9 [[1]](#references) and its died-of-wounds model from the rate the US Army reported on Okinawa [[2]](#references)). Where the companion document establishes what the modelled deployed health system does under one seed and one casualty-rate assumption, this document establishes which of those findings survive replication and how the same system responds when casualty production is scaled to a materially higher intensity. Six further sections apply the same framework to individual design questions a single run can raise but not settle: the R2B pre-open hold window, the post-operative intensive care gate, the forward intensive care share policy lever, the transport fleet-size margin, the reinforcement demand cycle, and an acute mass casualty surge. Replication counts are sized to the response each experiment measures and range from 10 to 50; every section states its own.

Across 50 replications of each scenario (30 simulated days, control seed 42), the comparison finds that the current establishment's adequacy conclusion does not extrapolate from Falklands to Okinawa intensity. Mean total casualties per run rise 2.33-fold; the R2E operating theatre mean queue rises approximately 36-fold, the R2E intensive care queue approximately 4.3-fold from a low base, the R2E holding bed queue approximately 4.5-fold from a base already materially non-zero, and the R2B holding bed queue approximately 5.5-fold. R2B theatre queue remains at zero in both profiles, not because R2B absorbs any of the surge but because the model's bypass routing diverts all surgical overflow onto an already saturated R2E. Died-of-wounds rate as a proportion of WIA rises from 0.42% to 3.43%, a row that compares two campaigns' standards of care as well as two casualty volumes, each profile carrying the mortality model of the conflict its casualty rates come from. Transport (PMV Ambulance and HX240M) is the one echelon with genuine headroom at both intensities.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Introduction](#introduction)
- [Methods](#methods)
  - [Design and Unit of Analysis](#design-and-unit-of-analysis)
  - [Replication Independence](#replication-independence)
  - [Confidence Intervals](#confidence-intervals)
  - [Replication Count and Resolution](#replication-count-and-resolution)
  - [Scenario Profiles](#scenario-profiles)
  - [Run Configuration and Provenance](#run-configuration-and-provenance)
- [Comparative Scenario Analysis](#comparative-scenario-analysis)
  - [Casualty and Mortality Totals](#casualty-and-mortality-totals)
  - [Resource Queue Comparison (mean of per-resource mean queue, by group)](#resource-queue-comparison-mean-of-per-resource-mean-queue-by-group)
  - [Interpretation](#interpretation)
- [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)
- [The Post-Operative Intensive Care Gate](#the-post-operative-intensive-care-gate)
- [Forward ICU Share Decision Frontier](#forward-icu-share-decision-frontier)
- [Transport Fleet-Size Sweep](#transport-fleet-size-sweep)
- [Force Regeneration Under Reinforcement](#force-regeneration-under-reinforcement)
- [Mass Casualty Event Stress Test](#mass-casualty-event-stress-test)
- [Limitations](#limitations)
- [Conclusion](#conclusion)
- [References](#references)
<!-- TOC END -->

---

## Introduction

<small>[Return to Top](#contents)</small>

A deployed health system is sized before the campaign it has to serve, and the planner sizing it needs two things a single simulated campaign cannot supply. The first is a statement of how much of what one run shows is the system and how much is the draw. The second is a statement of how far a conclusion reached at one casualty intensity carries to another, because the casualty volumes anticipated in large scale combat operations exceed those the deployed health systems of the past two decades were built around [[3]](#references), and the medical planning assumptions carried forward from those operations have been argued to understate both the volume and the acuity a peer fight would produce [[4]](#references).

This document accordingly asks two questions. Does the shipped establishment's apparent adequacy under a Falklands-derived casualty rate survive both replication and a change of casualty intensity? And which of the design levers a planner can actually reach, being the parameters the model exposes rather than the structures it fixes, move an outcome by enough to be measured at an affordable replication count?

The [Comparative Scenario Analysis](#comparative-scenario-analysis) answers the first, by running the same establishment under two calibrated casualty-rate profiles and reporting the difference with intervals. The six experiment sections that follow it answer the second, one lever at a time: the R2B pre-open hold window, the post-operative intensive care gate, the forward intensive care share, the transport fleet size, the reinforcement demand cycle, and the injection of acute mass casualty events on top of the sustained background tempo. The answer to the second question is more negative than the first, and the negative result is itself reported: three of the six experiments measure a lever whose effect the replication count cannot separate from noise, and each says so and states what count would be needed.

What this document does not do is validate the model against the real system. Establishing that a model behaves as its specification describes, and establishing that the specification adequately represents the system, are distinct activities resting on distinct evidence [[5]](#references); the first is the subject of `docs/Single_Run_Analysis.md`, and the second, so far as the available historical anchors allow, of the README's [Parameter Calibration](../README.md#parameter-calibration) section. The experiments below are conducted on the model. Their conclusions are conditional on it, and the [Limitations](#limitations) section states the conditions that bear hardest.

---

## Methods

<small>[Return to Top](#contents)</small>

### Design and Unit of Analysis

The simulation is a discrete event model built on the `simmer` package for R [[6]](#references), in which each casualty is an arriving entity that seizes and releases clinical staff, beds, operating theatres and transport as it moves rearward through the echelons of allied medical support doctrine [[7]](#references). Every experiment reported here executes it through the project's multi-run replication framework (`run_replications()`, `R/replication.R`), which runs a requested number of independent replications and returns their monitoring data with a replication index attached.

The replication is the unit of analysis throughout, and neither the casualty nor the simulated day is [[8]](#references). Each replication is one complete campaign run from an empty and idle system over the horizon its section states, and every response is reduced to a single number per replication before any statistic is taken across replications. For a resource this is the time-weighted mean queue length over that replication's observation window; for a count, the count within it. Taking statistics over casualties or over days instead would treat observations generated inside one campaign as though they were independent campaigns, and would produce intervals far narrower than the runs support.

No warm-up period is deleted from any observation window. Warm-up deletion removes the initialisation transient of an infinite-horizon model approaching steady state and is appropriate only where that steady state is the quantity of interest [[9]](#references); this model is a terminating simulation with a finite campaign horizon and a genuinely empty starting condition, so the transient is part of what a planner needs to see. The README's [Warm-up Period Analysis](../README.md#warm-up-period-analysis) sets out that classification and reports the Welch graphical diagnostic run against the model over ten 90-day replications, which supports it.

### Replication Independence

Every interval in this document divides a sample standard deviation by the square root of the replication count, which is correct only where replications are independent of one another. That independence is a property of how a replication is constructed here, rather than an inference drawn from the output.

Two facts establish it. `run_once()` (`R/replication.R`) is a pure function of its seed: re-running a seed reproduces its output exactly, whether or not another run is interposed, so the seed is the whole of what distinguishes two replications and no other channel exists between them. And `run_replications()` draws a distinct seed for each replication. Independent seeds into a pure function give independent replications, which is a deterministic statement rather than one a finite sample of correlations could support. `scripts/check_replication_independence.R` asserts both facts, and runs on every pull request.

The parallel dispatch path preserves them. Replications run under `RNGkind("L'Ecuyer-CMRG")` with `mc.set.seed = TRUE`, which assigns each worker its own substream of the MRG32k3a generator; the substream spacing of $2^{76}$ makes overlap impossible at any simulation budget used in this study [[10]](#references)[[11]](#references). Both dispatch paths run under the one generator, so a replication's output is a function of its seed and not of the path or the core count that produced it, and a figure quoted at a control seed reproduces at that seed whatever else the session has already measured.

No variance reduction scheme is applied on top of that. Antithetic pairing was used earlier in the project and was withdrawn, because neither of its preconditions holds for this model. Its reach is confined to the arrival generators, `simmer` drawing service times and routing probabilities from the global stream inside its own event loop in an order set by event timing rather than by entity; and the technique requires the response to be monotone in the input uniforms [[12]](#references), which casualty arrivals are not, their rate being scaled by an effective force size that trajectory outcomes themselves debit and credit. Measurement bears that out: over 75 pairs the within-pair correlation on total casualties is $-0.04$ (95% CI $[-0.27, +0.19]$), worth a variance reduction of about 3% and indistinguishable from none. The README's [Multi-run Replication Framework](../README.md#multi-run-replication-framework) gives the construction and that measurement in full. One consequence of the withdrawn pairing survives into this document and is flagged at the point it appears: the intervals in [The Post-Operative Intensive Care Gate](#the-post-operative-intensive-care-gate) were computed while the pairing was in force, over replications that were not independent, and are narrower than those runs entitle them to be.

### Confidence Intervals

Every confidence interval in this document is a Student $t$ interval on the mean across replications,

$$\bar{x} \pm t_{0.975,\;n-1}\,\frac{s}{\sqrt{n}}$$

where $n$ is the replication count, $\bar{x}$ the mean across replications of the per-replication response and $s$ its sample standard deviation. No normal approximation and no bootstrap is used anywhere in this document. The same expression is evaluated in `summarise_replications()` (`R/replication.R`), in the comparative scenario runner (`R/scenario_runner.R`) and at every point in the analysis pipeline that reports an interval, so an interval quoted here, an interval shown in the Shiny console and an interval in the written CSV outputs are the same quantity computed the same way. Where the response cannot be negative, which is the case for every queue length and every count, a lower bound below zero is truncated at zero; the truncation is a reporting convention and does not change the arithmetic behind it.

Where a comparison is between two configurations run at the same control seed, the interval is on the mean of the per-replication paired difference rather than on the difference of the two means, and the tables say so in their column headings. Pairing on the seed is worth doing only where it removes variance, and one section below reports that it does not (see [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)).

A 10th-to-90th-percentile range is not a confidence interval and is never used as one here. Where a table reports both, the interval states how precisely the mean is determined and the range states how widely a single campaign varies about it. The second exceeds the first by a large factor in every table below, and it is the second that should be in mind when reading any figure from a single run.

### Replication Count and Resolution

Replication counts differ between the experiments below because the responses differ in how many events each replication supplies. A count of deaths, of which a Falklands-rate campaign produces about one, is determined by a handful of events per replication; a time-weighted mean queue is determined by every arrival and departure at that resource over 30 days.

The best-determined dispersion available for this model is on the treated-cohort died-of-wounds rate, whose per-replication standard deviation is 0.0039 under the shipped configuration, measured over 150 replications. At that dispersion a 95% half-width of 0.15 percentage points requires 29 replications, one of 0.10 pp requires 62, and one of 0.05 pp requires 237. The 50-replication mortality figures below accordingly carry roughly $\pm 0.11$ pp on this response, and single 50-replication measurements of one unchanged configuration span 0.132 pp across control seeds, which is why `scripts/check_dow_calibration.R` pools three independent measurements rather than reporting one. `CLAUDE.md`'s Key Parameters table cross-references this section for that derivation.

Queue and occupancy responses are far better resolved at the same replication count, being time-weighted over orders of magnitude more events per replication; a comparable critical care discrete event simulation reports its occupancy and queue results at 95% intervals over replication counts of this order for the same reason [[13]](#references). The pattern runs through every section below. The queue rows of the comparison separate the two casualty-rate profiles decisively at 50 replications; the mortality rows separate them only because the effect between them is large. A rare-event response measured over a cohort of a few dozen casualties, which is what the post-operative mortality of each intensive care pathway is, is not resolved at any replication count this project has run, and the sections reporting such responses state that rather than reading a point estimate as a finding.

### Scenario Profiles

A scenario profile is a named set of overrides applied on top of the shipped default `env_data.json` parameters, resolved by `resolve_scenario()` (`R/scenario.R`). Two are compared here, `moderate_intensity` (Falklands 1982 exemplar) and `high_intensity` (Okinawa exemplar), both defined in the `scenarios` block of `env_data.json`. A third, Vietnam-calibrated profile is not included: FORECAS's Appendix A has no standalone Vietnam combat-troop WIA/KIA distribution table, Table A.5 being Vietnam DNBI only [[1]](#references), so no genuinely FORECAS-sourced Vietnam parameters exist to build one from.

Each profile's casualty-generation parameters, being its arrival-rate distributions, its priority-severity mix and its died-of-wounds calibration, are calibrated against a named historical exemplar, and the extent of that calibration differs by profile and by parameter. Each profile's died-of-wounds ceilings are fitted against a treated-cohort mortality rate reported for its own campaign, measured over casualties who reached a treatment facility alive rather than over all wounded (see the README's [Parameter Calibration](../README.md#parameter-calibration)): `moderate_intensity` against the Falklands figure, `high_intensity` against the rate the US Army reported on Okinawa [[2]](#references). What `high_intensity` still inherits from the Falklands-calibrated base is its priority split, its DNBI composition and its transport times.

Both profiles use the same health system establishment, being the simulation's shipped default configuration: a representative combat brigade served by three Role 1 treatment teams, two Role 2 Basic (R2B) facilities and one Role 2 Enhanced Heavy (R2E Heavy) hospital. Element, bed and transport fleet counts are structural configuration and are not scenario-eligible parameters, so only the casualty-generation parameters differ between the two.

### Run Configuration and Provenance

Every section below opens with its own design statement, naming its replication count, its control seed and any parameter set away from the shipped default. The comparative scenario analysis is invoked as:

```
Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity --iterations 50 --days 30 --seed 42
```

The figures in that comparison, and `images/scenario_comparison.png` alongside them, were produced in the project's pinned development container (`rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`), so they carry no unpinned-environment caveat. The seed-42 evidence set the single-run document reports reproduces byte for byte in the same container, and `scripts/check_baseline_reproduction.R` asserts that reproduction on every change to the model.

One property of the comparison is worth stating because it is easily mistaken for a defect. `moderate_intensity` total casualties measure 437.8 per run, while the documented seed-42 single run produces 530 (`CLAUDE.md` Key Parameters). The single run sits inside this profile's own 10th-to-90th-percentile range of 362.7 to 528.0 rather than near its mean, which is what a single draw from a wide distribution does. That the comparative runner reproduces the base configuration, and so honours the scenario mechanism's no-op guarantee, is established directly instead: `moderate_intensity` overrides only casualty-generation parameters, and every casualty, mortality and queue figure in the tables below reproduces exactly when the comparison is re-run at this seed, in the pinned container and outside it alike.

---

## Comparative Scenario Analysis

<small>[Return to Top](#contents)</small>

**Design.** 50 replications of 30 simulated days per profile at control seed 42, under the shipped default establishment, the only overrides being those the scenario profile itself applies (see [Scenario Profiles](#scenario-profiles) and [Run Configuration and Provenance](#run-configuration-and-provenance)).

### Casualty and Mortality Totals

| Metric | `moderate_intensity` (Falklands) | `high_intensity` (Okinawa) | Ratio |
|---|---|---|---|
| Total casualties/run | 437.8, 95% CI [421.0, 454.7] (p10–p90: 362.7–528.0) | 1,021.0, 95% CI [993.9, 1,048.1] (p10–p90: 906.5–1,138.5) | 2.33× |
| WIA/run | 188.7, 95% CI [175.4, 202.0] (p10–p90: 137.6–251.5) | 684.3, 95% CI [658.3, 710.3] (p10–p90: 586.2–792.5) | 3.63× |
| DOW/run | 0.78, 95% CI [0.55, 1.01] (p10–p90: 0–2.0) | 23.58, 95% CI [21.88, 25.28] (p10–p90: 18.0–32.1) | 30.2× |
| DOW/WIA rate | 0.42%, 95% CI [0.29%, 0.54%] (p10–p90: 0–1.02%) | 3.43%, 95% CI [3.24%, 3.61%] (p10–p90: 2.63%–4.22%) | 8.24× |

The casualty rows carry a wide p10 to p90 spread because each arrival stream draws its rate once per simulated day from the distribution its configuration names, then places that day's arrivals within the day by thinning [[14]](#references), so the between-day standard deviation the source reports reaches the output rather than being averaged away (see the README's [Casualty Generation](../README.md#casualty-generation)). Total casualties per `moderate_intensity` run span 362.7 to 528.0 between those percentiles against a mean of 437.8, a spread of roughly a third of the mean. Every mean in this document should be read against a distribution wide enough that a single campaign is a poor guide to it.

The died-of-wounds rows measure something wider than the health system. Each profile carries the mortality model of the campaign its casualty rates come from, `high_intensity` having its own Okinawa calibration rather than one inherited from the Falklands-calibrated base (see [Scenario Profiles](#scenario-profiles)), so the 30-fold ratio between the profiles is a comparison of two campaigns' mortality experience as well as of two casualty volumes, and the two effects cannot be separated within this table.

The two rows are also resolved very differently. The `moderate_intensity` figure is a single 50-replication measurement of a response averaging well under one death per replication, which 50 replications resolve only to roughly ±0.13 percentage points (see [Replication Count and Resolution](#replication-count-and-resolution)); it moves between control seeds by about as much as its own interval spans, so the figure to quote for that profile is the 150-replication pooled one in the README's [Scenario Profiles](../README.md#scenario-profiles), 0.368% (95% CI [0.310%, 0.426%]) on the treated cohort. The `high_intensity` figure rests on some 24 deaths per replication and is correspondingly well resolved, its 3.43% sitting inside the 3.471% (95% CI [3.360%, 3.583%]) that the same 150-replication pooling gives. The intervals in both cases are correctly specified, the replications behind them being independent of one another (see [Replication Independence](#replication-independence)). The queue rows below are better resolved than either at the same replication count, being time-weighted occupancy measures with far more events behind each replication.

### Resource Queue Comparison (mean of per-resource mean queue, by group)

| Resource group | `moderate_intensity` mean queue (95% CI) | `high_intensity` mean queue (95% CI) | Ratio |
|---|---|---|---|
| R2B OT | 0.000 [0.000, 0.000] | 0.000 [0.000, 0.000] | not applicable |
| R2B Hold | 0.593 [0.501, 0.685] | 3.228 [3.005, 3.452] | 5.45× |
| R2E OT | 1.063 [0.691, 1.435] | 38.17 [34.01, 42.33] | 35.9× |
| R2E ICU | 0.131 [0.104, 0.159] | 0.564 [0.464, 0.664] | 4.29× |
| R2E Hold | 0.598 [0.437, 0.758] | 2.694 [2.449, 2.938] | 4.51× |
| Transport (PMV Ambulance / HX240M) | 0.0038 [0.0000, 0.0078] | 0.0278 [0.0196, 0.0361] | 7.25× (small in both) |

Each cell is the mean across the 50 replications of that replication's mean queue over the group's resources, with the Student $t$ interval of [Confidence Intervals](#confidence-intervals) on that mean; a resource idle throughout a replication contributes a zero rather than dropping out of that replication's average, so the denominator is the group's full establishment in every replication. The R2B theatre interval is degenerate because every replication of both profiles returns exactly zero, which is a property of the bypass routing rather than of the estimate (see [Interpretation](#interpretation)). Transport is the one group whose lower bound truncates at zero.

![Four-panel bar chart of mean queue length by resource group, R2B OT, R2E OT, R2E ICU and transport, each panel comparing the high intensity and moderate intensity profiles with error bars, on four different vertical scales](../images/scenario_comparison.png)

Each panel carries its own vertical scale, so the panels compare profiles rather than resources: the R2E theatre panel runs to 60 casualties while the transport panel runs to 0.07. Both bars in the R2B theatre panel sit exactly at zero. The error bars are the mean of the per-resource p10 to p90 range across replications rather than a confidence interval, and every high intensity error bar is wide enough to show that the surge queues vary substantially from replication to replication.

### Interpretation

The comparison exposes a structural fragility that the single-run baseline could not surface on its own, and it locates that fragility in the operating theatres. Mean R2E OT queue rises from 1.06 casualties at Falklands-equivalent load to 38.2 under `high_intensity`, a factor of roughly 36, which is by a wide margin the largest movement anywhere in the model. The mechanism is the surgical roster: a casualty seizes a theatre before it seizes one of the three surgical sections that staff them, so a room reads as queued while its occupant waits for staff, and at Okinawa-intensity arrival rates that wait dominates. The Falklands-load figure is itself materially above zero, which governs how the surge figure should be read: an arrival process that delivers genuine heavy days produces theatre contention at Falklands rates too, so theatre contention is a baseline property of this establishment that peer-conflict intensity makes acute rather than a fragility confined to peer conflict. R2E ICU rises from 0.131 to 0.564, a factor of 4.3, and is the flattest of the three R2E groups under surge: with only the damage control cohort taking a stabilisation episode, intensive care carries one episode for half the surgical population and two for the other half rather than two for everyone. R2E holding beds rise from 0.598 to 2.69, a factor of 4.5, and absorb what intensive care does not, since the holding bed is where a casualty goes when no intensive care bed is free and where those awaiting strategic evacuation stage.

R2B OT queue remains at 0 in both scenarios, and not because R2B absorbs any of the surge: the OT-bypass routing diverts casualties requiring surgery to R2E whenever the theatre is busy or queued, or the surgical section is closed for longer than the pre-open hold window, rather than letting them wait; under `high_intensity`, this shunts the entire surge onto an R2E that has limited further capacity to absorb it. The hold window bounds how long a casualty may wait for a section about to reopen and so cannot produce a standing queue, which is why the queue reads zero even with the window open (see [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)). R2B Hold bed queue, already identified as a Falklands-rate bottleneck (see the single-run analysis's [R2B Hold Bed Saturation](Single_Run_Analysis.md#r2b-hold-bed-saturation-stream-decomposition-and-intervention-analysis) finding), increases roughly 5-fold (0.593 to 3.23), driven by the proportional increase in non-surgical WIA volume rather than any change to DNBI generation, since DNBI generation rate is not one of the parameters a scenario profile overrides.

Transport remains the one echelon with genuine headroom: mean queue stays a small fraction of a casualty even at 2.3× total casualty volume, consistent with the single-run analysis's [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin) finding that the PMV Ambulance/HX240M pool is not the binding constraint at the Falklands-derived rate. The margin is wide but it is not untouched: a queue forms on the peak days both profiles produce, which is why the figures are small rather than exactly zero, and why re-running the dedicated fleet-size sweep against the current arrival process is outstanding work in its own right (see the README's Further Development entry L19).

DOW/WIA rate rises from 0.42% to 3.43%, a factor of roughly 8, on intervals that do not overlap. What that factor measures needs stating explicitly, because it is the one row here that is not a statement about the modelled health system alone. Each profile carries the mortality model of the campaign its casualty rates come from, so the gap between the rows combines three things: Okinawa's heavier casualty volume, the treatment queues that volume produces, and a standard of surgical and resuscitative care four decades older than the Falklands profile's. The first two belong to this comparison; the third is a property of the calibration and would be present at equal casualty volumes. A planner reading the ratio as the cost of surge alone would overstate it substantially. What survives unambiguously is the mechanism the queue rows establish: at Okinawa intensity the deaths occur against treatment queues deep enough that removing an intensive care episode and a theatre episode from half the operated population, which is what the surgical pathway split does, does not relieve them.

---

## The R2B Pre-Open Hold Window

<small>[Return to Top](#contents)</small>

R2B fields one surgical section per team on a 12-hour shift against a theatre available around the clock, so for half of each day the theatre stands ready with nobody rostered to operate in it. `r2b.surgery.pre_open_window_min` sets how long a casualty who arrives in that half may be held forward for the section rather than being diverted to R2E (see the README's [R2B Trajectory](../README.md#r2b-trajectory)). The window ships at 60 minutes and has no source (see the README's Further Development entry L28), so what it does is a question for measurement rather than for argument. A single run cannot answer it either: turning the window on shifts simmer's single global random stream, so a zero-window run and a 60-minute run at the same seed are different realisations rather than a controlled comparison.

**Design.** 50 replications of 30 simulated days per arm at control seed 42, under the shipped default configuration with one override, `r2b.surgery.pre_open_window_min` set to 0 in one arm against its shipped 60 in the other, run in the project's pinned Dev Container. The first two columns are per-replication means; the third is the mean of the per-replication paired difference with its 95% confidence interval, which is the quantity the comparison turns on.

| Measure | Window 0 (instant diversion) | Window 60 min (shipped) | Difference (95% CI) |
| --- | --- | --- | --- |
| Casualties held forward | 0 | 5.90 | +5.90 [+5.18, +6.62] |
| R2B surgeries | 51.82 | 52.20 | +0.38 [−2.75, +3.51] |
| Bypassed, section off shift | 84.94 | 75.24 | −9.70 [−17.25, −2.15] |
| Bypassed, theatre busy or queued | 19.76 | 17.08 | −2.68 [−6.95, +1.59] |
| R2E first surgeries | 125.16 | 117.96 | −7.20 [−16.33, +1.93] |
| R2E theatre entry deferred (ICU full) | 18.94 | 15.62 | −3.32 [−6.56, −0.08] |
| Died of wounds per run | 1.02 | 1.02 | +0.00 [−0.38, +0.38] |
| R2B OT utilisation (24-hour room) | 7.0% | 7.2% | not applicable |
| Total casualties | 442.82 | 433.18 | −9.64 [−32.00, +12.72] |

The window does what it was added to do, and on this the measurement is decisive: 5.90 casualties are held forward per run (95% CI [5.18, 6.62]), where a zero window holds none by construction, and the off-shift bypass count falls by 9.70 ([−17.25, −2.15]). Both intervals exclude zero. Casualties that would have been diverted are instead retained forward.

What the measurement cannot establish is the accounting behind that retention, which is that forward surgeries rise by the number held forward and by nothing else. Forward surgeries move by +0.38 ([−2.75, +3.51]), an interval comfortably spanning zero and equally comfortably spanning the +5.90 the holds would predict. The measurement cannot distinguish those two hypotheses, and it should not be read as evidence for either.

That is a limit on what this design can resolve rather than a finding about the window, and the reason is that the two arms are not the same realisation. A zero-window run and a 60-minute run at the same control seed share their per-replication seeds, but the first hold shifts simmer's single global stream, and the force-regeneration loop couples arrival timing back to casualty event timing (see the README's [Force Regeneration and the Endogenous Feedback Loop](../README.md#6-force-regeneration-and-the-endogenous-feedback-loop)), so the arms diverge into different casualty streams. Not one of the 50 replication pairs generated the same number of casualties in both arms, and the paired difference in total casualties spans −32.00 to +12.72. Pairing on the seed therefore removes none of the between-run variance, and an effect of about six operations sits inside the noise of a response whose paired difference has a standard deviation of 11.

The replication count this comparison would need follows from those standard deviations. Resolving the forward-surgery effect to a half-width of two operations requires about 120 replications per arm; the bypass and R2E surgery rows, whose paired differences carry standard deviations of 27 and 32, require several hundred to low thousands. Neither is run here, so the forward-surgery, bypass-composition and R2E-surgery rows of the table above are reported as unresolved rather than as effects. Re-running at those counts, or adopting a variance reduction design that survives a stream shift, is the outstanding work on this section.

Two rows are worth reading despite this. R2E theatre entry deferred for a full intensive care unit falls by 3.32 ([−6.56, −0.08]), an interval that excludes zero and points the way the mechanism predicts, earlier surgery for the casualties the window reaches relieving rearward pressure a little. And mortality is flat: deaths of wounds per run differ by 0.00 ([−0.38, +0.38]), with the treated-cohort rate at 0.37% and 0.39%, both below the approximately 0.46% Ajax Bay bound the project's one-sided calibration check applies. That row is a null result at this replication count rather than a demonstration that the window is mortality-neutral, deaths of wounds being the rarest response in the table.

Two further limits apply to the design itself. The comparison was run at the shipped default configuration only, so it says nothing about how the window behaves under surge, where the forward theatre is contended and displacement would be likelier to bite. And 60 minutes is one point on a range the screening bounds run from zero to six hours, so this establishes that the shipped value drives the mechanism it was added for, not that it is the value that pays best.

---

## The Post-Operative Intensive Care Gate

<small>[Return to Top](#contents)</small>

A damage control casualty leaving theatre needs a stabilisation episode, and the model gates theatre entry on whether an intensive care bed is available to provide it: a Priority 1 casualty is operated on regardless and recovers in a holding bed at elevated risk when no bed is free, while a Priority 2 or lower casualty has theatre entry deferred until one is (see the README's [Post-Operative Stabilisation](../README.md#post-operative-stabilisation)). The seed-42 walk-through shows which casualties took the degraded route and on which day (see [R2E Heavy Handling](Single_Run_Analysis.md#r2e-heavy-handling)); whether the gate's two effects, relieved intensive care load and elevated mortality on the degraded route, survive beyond that one draw is a question for replication.

**Design.** 50 replications of 30 simulated days at an independent seed per replication (`seed = NULL`), under the shipped default configuration, run before and after the gate was introduced. The intervals in this section alone were computed over replications that were antithetically paired while the interval divided by the replication count, which makes them narrower than those runs entitle them to be (see [Replication Independence](#replication-independence)). They are not recomputed, the comparison being against a configuration that no longer exists in the codebase and the earlier arm being unable to be re-run; the section's conclusion, that the two intervals overlap and the comparison does not reach significance at 50 replications, is only reinforced by intervals that should be wider. Every other interval in this document is computed over independent replications.

Mean R2E ICU utilisation fell from **74.1% to 60.2%**, a substantial and consistently observed reduction in intensive care load rather than a seed-42 artefact. Mean DOW/run rose from **0.84 (95% CI [0.58, 1.10]) to 1.00 (95% CI [0.74, 1.26])**; the two intervals overlap substantially, so this comparison does not reach conventional statistical significance at 50 replications, deaths of wounds remaining a rare event at this casualty rate. The increase is nonetheless fully attributable to the new post-operative checkpoint, which contributed a mean of 0.10 DOW/run on its own, in 5 of 50 replications, accounting for essentially the entire point-estimate shift.

Within that checkpoint the qualitative design intent held under the shipped parameters rather than only under a stress test: the post-operative holding pathway's realised DOW rate, 2 deaths in 1,223 casualties or 0.16%, was roughly **2.8× the ICU pathway's rate** of 3 in 5,085 or 0.06%. The elevated-risk pathway is measurably, not just theoretically, riskier at baseline casualty rates, though the small absolute counts mean the ratio itself carries wide uncertainty and should be read as direction rather than as magnitude.

The mortality mechanism itself was confirmed separately, by a stress test that forced intensive care capacity to zero over a 90-day run: the degraded route then dominates and produces measurable post-operative deaths, establishing that the checkpoint fires as designed rather than that it is quantitatively resolved at Falklands-calibrated rates.

---

## Forward ICU Share Decision Frontier

<small>[Return to Top](#contents)</small>

A casualty's stabilisation requirement is a single quantity divided between R2B and R2E by the forward-holding policy, and the post-definitive care that follows their definitive repair is a separate episode served only at R2E (see the README's [Post-Operative Stabilisation](../README.md#post-operative-stabilisation)). Because the stabilisation total is conserved at every setting, sweeping the policy moves load between the echelons without changing how much care is delivered, which makes it a genuine planning lever rather than a way of quietly reducing treatment. Only the damage control cohort has a stabilisation phase, so the population the lever acts on is roughly half of operated casualties rather than all of them (see the README's [Surgical Pathway](../README.md#surgical-pathway)).

**Design.** 20 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: `r2b_icu_share` set to 0, 0.25, 0.5, 0.75 and 1.0 in turn. Point 0 is the shipped default. Run via `Rscript scripts/run_icu_share_sweep.R --iterations 20 --days 30`.

![Five stacked line plots against the share of post-operative intensive care delivered forward at R2B, from 0% to 100%, showing R2E ICU mean queue, R2B ICU utilisation, R2E ICU utilisation, the share of post-definitive care delivered in ICU, and DOW count, each with a 95% confidence ribbon and a dashed line at the shipped default of 0%](../images/r2b_icu_share_frontier.png)

Every panel moves little across the full sweep and every confidence ribbon spans the whole movement, so at this replication count the lever is not resolved: the frontier shows where the trade-off would appear rather than establishing its size.

| Forward ICU share | R2E ICU mean queue (95% CI) | R2B ICU utilisation | R2E ICU utilisation | Post-definitive care in ICU (95% CI) | Mean DOW per run (95% CI) |
|---|---|---|---|---|---|
| 0% (shipped) | 0.108 (0.066–0.149) | 22.4% | 87.7% | 35.5% (28.4–42.6) | 0.80 (0.35–1.25) |
| 25% | 0.080 (0.042–0.119) | 22.1% | 84.9% | 38.7% (30.4–46.9) | 1.00 (0.52–1.48) |
| 50% | 0.078 (0.028–0.129) | 14.1% | 83.4% | 41.6% (34.5–48.8) | 1.00 (0.52–1.48) |
| 75% | 0.079 (0.036–0.121) | 20.2% | 83.8% | 42.2% (31.4–52.9) | 1.10 (0.47–1.73) |
| 100% | 0.125 (0.033–0.218) | 22.7% | 83.9% | 42.0% (32.4–51.6) | 1.00 (0.25–1.75) |

Once only the damage control cohort has a stabilisation phase to move, the lever stops earning its keep. Every quantity in the table above is flat across the swept range. The R2E intensive care queue moves between 0.078 and 0.125 casualties with overlapping intervals and no trend, its highest value falling at the 100% share where the lever is supposed to help most; R2E utilisation sits between 83.4% and 87.7% at every setting, and is likewise highest at the shipped zero share.

The share of casualties receiving post-definitive care in an intensive care bed is the one column with any apparent direction, rising from 35.5% at a zero share to 42.0% at a full one. It should not be read as a gain. Every interval on that column overlaps every other, spanning some fourteen percentage points against a movement of seven, and the ordering is not monotonic; twenty replications cannot separate a trend of this size from noise on this response.

The reason the lever does so little is the size of the population it acts on. Roughly half of operated casualties take the single-stage pathway, and they have no stabilisation phase to move; of the remainder, only those operated on forward at R2B can have any of it served forward. What is left is a small enough cohort that shifting all of their stabilisation forward does not measurably relieve a unit running above 83% occupancy. R2B intensive care utilisation confirms the mechanism only weakly, reading 22.4% at a zero share where the beds serve only the evacuation wait and then moving without order between 14.1% and 22.7% once forward holding is enabled. That column is estimated from few enough events per replication to be poorly determined, and it should be read as showing that load moves rather than as measuring how much.

The mortality column remains unresolved. Mean deaths of wounds per run move 0.80, 1.00, 1.00, 1.10 and 1.00 across the five points with every confidence interval overlapping every other. Deaths of wounds are rare at this casualty rate, and the capability penalty applies only to the fraction of a fraction that is operated on forward and holds there, so twenty replications cannot separate an effect of this size from noise.

The shipped default therefore stays at zero. What this frontier shows is a lever with a real mechanism and no measurable benefit at Falklands-equivalent load, the cohort it acts on being roughly half of operated casualties and, within that, only those operated on forward. It may still matter at higher casualty rates, where R2E intensive care is contended by a wider margin, and that is the experiment worth running next.

---

## Transport Fleet-Size Sweep

<small>[Return to Top](#contents)</small>

The seed-42 walk-through establishes that the shipped three-vehicle PMV Ambulance and four-vehicle HX240M pools carry substantial headroom, the PMV Ambulance pool queueing only briefly and the HX240M pool not at all (see [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin)). What that run cannot say is where the headroom ends. This sweep varies vehicle count directly, rather than casualty rate or transport duration, and so locates the fleet size at which transport becomes the binding constraint.

**Design.** 10 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: the PMV Ambulance fleet swept across 1 to 5 vehicles and the HX240M fleet across 1 to 4, each with the other fleet held at its shipped establishment size.

`plot_transport_capacity_margin_by_fleet_size()` (`R/analysis.R`) rebuilds the environment at each sweep point via `build_environment()` and runs the same replication engine (`run_replications()`, `R/replication.R`) the comparative scenario runner uses. The sweep was run via `Rscript scripts/run_transport_sweep.R`:

![Four-panel line plot of mean queue and mean utilisation against fleet size for the PMV Ambulance and HX240M fleets, each line with a 95% confidence ribbon and a dashed vertical line marking the current establishment size](../images/transport_capacity_margin_by_fleet_size.png)

The PMV Ambulance queue collapses between one and two vehicles and is flat thereafter, so the shipped establishment of three sits on the flat part of the curve rather than at its knee. Utilisation rises again beyond three vehicles because the measure is a per-vehicle average over a pool that is seldom fully engaged.

| Fleet size | PMV Ambulance mean queue (95% CI) | PMV Ambulance mean utilisation | HX240M mean queue (95% CI) | HX240M mean utilisation |
|---|---|---|---|---|
| 1 | 2.1060 (0.2270–3.9850) | 38.7% | 0.0442 (0.0000–0.1021) | 10.7% |
| 2 | 0.0487 (0.0000–0.0974) | 18.5% | 0.0011 (0.0000–0.0022) | 5.5% |
| 3 (current PMV) | 0.0068 (0.0000–0.0155) | 12.5% | 0.0001 (0.0000–0.0002) | 11.1% |
| 4 (current HX240M) | 0.0006 (0.0000–0.0012) | 14.6% | 0.0000 | 14.6% |
| 5 | 0.0001 (0.0000–0.0001) | 17.8% | not swept | not swept |

At a single vehicle the PMV Ambulance fleet queues heavily, at a mean of 2.11 casualties, and the sweep locates the capacity boundary sharply rather than merely confirming the current always-zero finding. Queue falls by roughly a factor of forty at two vehicles and by a further factor of seven at the current three, where it is small but not exactly zero at 0.0068 casualties. The margin is wide, and the fleet carries more headroom than one additional vehicle would supply, but it is a margin rather than an absence of demand, and the same holds one row down: a reduction from three PMV Ambulances to two would raise the mean queue from 0.0068 to 0.0487, both small, neither zero. HX240M behaves the same way an order of magnitude lower, reaching zero only at its current four vehicles. What produces a queue at all is the day-to-day variation in casualty volume rather than the mean volume, a transport queue forming on peak days and on no others (see the README's [Casualty Generation](../README.md#casualty-generation)); the seed-42 walk-through shows the same thing in a single campaign, its PMV Ambulance pool queueing briefly (see [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin)).

Mean utilisation across the swept range remains too weakly determined to read. It runs the wrong way on both platforms, rising with fleet size where a fixed demand spread over more vehicles should lower it, and the interval on HX240M utilisation at three vehicles spans 2.3% to 19.9%. So few transport events occur per replication that the busy-time estimate at each sweep point is barely determined, which the wide 95% CI ribbons on the utilisation panels of the plot above show in the same way. The queue column is the column to read. `outputs/transport_capacity_by_fleet_size.csv` provides the full per-point results, including CI bounds omitted from the table above.

---

## Force Regeneration Under Reinforcement

<small>[Return to Top](#contents)</small>

Casualty production in this model is coupled to a live, time-varying effective force size rather than to a fixed roll strength, so sustained attrition should depress daily casualty volume as the force depletes, and an active reinforcement demand cycle should counteract that decline (see the README's [Force Regeneration and the Endogenous Feedback Loop](../README.md#6-force-regeneration-and-the-endogenous-feedback-loop)). The seed-42 walk-through shows the depletion curve for one campaign (see [Force Regeneration Feedback Loop](Single_Run_Analysis.md#force-regeneration-feedback-loop)); a daily volume slope is a regression on a noisy series and needs replication before it can be read.

**Design.** 15 replications per row at `moderate_intensity` and 12 at `high_intensity`, each of 30 simulated days, with daily casualty volume averaged across replications and fitted with an ordinary least-squares trend against simulation day. The unreinforced rows use the shipped default (`force_regeneration.reinforcement.demand_interval_days = 0`, which disables the mechanism). The reinforced rows override it with a 7-day demand submission cycle, a 7-day fulfillment lag and the shipped default triangular fill distribution (`fill_min_frac = 0.2`, `fill_mode_frac = 0.85`, `fill_max_frac = 1.1`).

| Scenario | Reinforcement | Daily volume slope | p-value | First-week mean | Last-week mean |
|---|---|---|---|---|---|
| `moderate_intensity` (15 reps) | None | −0.018/day | 0.75 | 14.9 | 14.4 |
| `moderate_intensity` (15 reps) | 7-day demand cycle, 7-day lag | −0.103/day | 0.17 | 16.6 | 14.1 |
| `high_intensity` (12 reps) | None | −0.349/day | 0.0027 | 39.8 | 29.5 |
| `high_intensity` (12 reps) | 7-day demand cycle, 7-day lag | +0.030/day | 0.80 | 35.8 | 36.1 |

All four rows were measured in the pinned Dev Container, the `high_intensity` rows including that profile's own died-of-wounds calibration.

At `high_intensity` casualty rates the mechanism is clear: daily volume falls significantly with no reinforcement, a 26% first-to-last-week decline at a slope of −0.349/day (p = 0.0027), and the demand-cycle reinforcement configuration removes that decline entirely, leaving a slope statistically indistinguishable from flat (+0.030/day, p = 0.80, under 1% first-to-last-week change). Reinforcement arrests depletion without overshooting into net growth. This is a direct consequence of the demand-based design: because each cycle's demand is the pool's actual current shortfall rather than a fixed size, a well-sustained pool automatically asks for less on its next cycle. The daily counts the regression is fitted to are themselves noisy, each carrying the between-day variation the arrival process delivers, so the trend is established at moderate rather than overwhelming confidence even where the slope is steep. At `moderate_intensity` neither configuration shows a slope distinguishable from flat, the profile's casualty volume being too low for 30 days of attrition to deplete the force measurably.

`force_regeneration.reinforcement` (`env_data.json`) remains a fully planner-tunable input, being the demand cycle, the fulfillment lag and all three triangular fill parameters, and this project does not attempt to auto-balance it against a scenario's attrition rate; the 7-day/7-day configuration above is illustrative, not a recommended operational setting.

The table demonstrates the mechanism's direction and statistical behaviour across replications. It is not a substitute for the seed-42 baseline figures, which are reported in the [Force Regeneration Feedback Loop](Single_Run_Analysis.md#force-regeneration-feedback-loop) walk-through and in `CLAUDE.md`.

---

## Mass Casualty Event Stress Test

<small>[Return to Top](#contents)</small>

The preceding sections analyse sustained casualty tempo, the background lognormal and exponential streams at either Falklands or Okinawa intensity. This section tests a qualitatively different scenario: an acute, discrete casualty surge layered on top of the Falklands-calibrated background tempo, using the compound Poisson mass casualty injection mechanism, under which discrete mass-casualty events of stochastic size are injected on top of the sustained background arrival streams.

An event's drawn casualty count is its total rather than its survivor count: a configured share of it, `mass_casualty.event.kia_fraction`, arrives killed at or near the point of injury and is handled by the mortuary pathway rather than triaged (see the README's [Mass Casualty Event Injection](../README.md#5-mass-casualty-event-injection)). At the shipped share of 0.28, a 30-day run at this rate and seed produces 78 event-derived casualties, of which 27 are immediate killed and 51 wounded. An event of a given nominal size therefore places a smaller load on the surgical echelons than its drawn casualty count suggests, and places a load on mortuary handling and killed-casualty transport alongside it that the surgical figures do not capture.

**Design.** 10 replications of 30 simulated days at control seed 42, under the shipped default configuration with one override: `mass_casualty.event.rate_per_day` set to 0.2 events per day (a mean 5-day inter-event interval), against a background-only arm at the shipped value of 0. Mass casualty injection ships disabled, so every figure in this section, the single illustrative run that follows the table and `images/mass_casualty_events.png` alike, requires that override and none of them can be reproduced by a shipped-configuration run. The figure is accordingly the one tracked image that `run.R --refresh-baseline` cannot write, and is copied into place from the run's own output directory.

| Metric | Background-only baseline | With mass casualty injection |
|---|---|---|
| Mean total casualties/run | 444.6 | 682.1 |
| Mean mass casualty events/run | 0 | 5.40 (range 3–8) |
| DOW rate, background-origin casualties | 0.18% (8/4,446) | 0.28% (13/4,577) |
| DOW rate, mass-casualty-origin casualties | not applicable | 0.58% (13/2,244) |

The mean 5.40 events per 30-day run sits a little below the configured 0.2/day event rate (theoretical expectation: 30 × 0.2 = 6), which ten replications cannot separate from it; event count varies from 3 to 8 across those replications, confirming the Poisson process is genuinely stochastic rather than deterministic. At a mean 41.6 casualties per event the drawn sizes sit close to the midpoint of the configured 20 to 60 range, as a uniform draw should. Mass-casualty-origin casualties show a died-of-wounds rate 2.1 times the background-origin rate, 0.58% against 0.28%, which is consistent with the intended stress-test effect of a blast-dominant priority mix arriving faster than steady-state capacity. Three qualifications apply. It is a per-casualty-origin comparison rather than a strict temporal-window comparison (see the assumption note in the analysis code, `R/analysis.R`); deaths of wounds remain rare at this sample size, 13 in each arm, so the ratio is illustrative of direction rather than precise; and the background column is not a quiet baseline, the background stream itself delivering heavy days, which is why its own died-of-wounds rate is non-zero at 0.18% and why the contrast between the two arms is narrower than the injected volume alone would suggest.

A single seed-42 run under the same override, without replication averaging, illustrates the mechanism directly: 537 total casualties, 459 from the background streams and 78 injected by two events, one of 33 casualties on day 14 and one of 45 on day 27. Both are recovered exactly by the gap-based reconstruction the analysis pipeline applies, which at two well-separated events has nothing to merge; the heuristic's known failure mode, two closely spaced events read as one, is a property of a busier event schedule than this seed produced.

The effect on the R2E theatre and intensive care gate is the clearest single-run signal. Post-operative stabilisation splits `hold=85` against `icu=37` under injection, against `hold=58` and `icu=79` in the shipped background-only run at the same seed (see the [R2E Heavy Handling](Single_Run_Analysis.md#r2e-heavy-handling) walk-through): the majority pathway inverts, and a cohort that mostly recovered in an intensive care bed mostly recovers in a holding bed instead. That is the degraded-care substitution the gate exists to expose, and it persists across the whole 30-day run rather than only during the two event windows.

The theatre and bypass measures barely move, and that is itself the finding. OT-entry deferrals for ICU-saturated Priority 2 and lower casualties read 25 under injection against 29 without it, and upstream pre-bypass from R1 reads 177 against 179; R2E theatre utilisation rises only from 24.2% to 25.7%, against R2B's 5.2% to 4.9%. A background tempo that delivers its own heavy days has already consumed the forward echelons' spare capacity, so an injected event adds to a system that is intermittently saturated regardless. The surge still degrades the care delivered, as the pathway inversion shows; what it does not do is reveal a constraint the background tempo was hiding.

![Stem plot of the two mass casualty events reconstructed from the run, each drawn as a vertical line at its simulation day with a point at its casualty count: 33 casualties midway through day 13 and 45 midway through day 26](../images/mass_casualty_events.png)

Two events, thirteen days apart, is a thin draw from a process configured to deliver a mean of six over the run, and it is why this seed illustrates the injection mechanism rather than measures its effect. The replicated table above carries the measurement.

---

## Limitations

<small>[Return to Top](#contents)</small>

Four classes of limitation bear on how the findings above should be read.

The first is resolution. Three of the six experiments report a lever whose effect the replication count cannot separate from noise: the forward-surgery, bypass-composition and R2E-surgery rows of the pre-open hold window comparison, the whole of the forward intensive care share frontier, and the mortality arm of the post-operative intensive care gate. Each states the count it would need, and none of those counts has been run. A reader should treat those rows as bounds on an effect rather than as estimates of one, and should not read a point estimate whose interval spans zero as a small effect (see [Replication Count and Resolution](#replication-count-and-resolution)).

The second is that several comparisons are not controlled in the way a paired design assumes. Any parameter change that alters the sequence of events shifts `simmer`'s single global random stream, and the force-regeneration loop then couples that shift back into arrival timing, so two arms run at one control seed diverge into different casualty streams. Pairing on the seed removes no variance under those conditions, and the effect is quantified where it bites hardest (see [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)). Only the scenario comparison, whose arms differ by design rather than by a small perturbation, is unaffected by this.

The third is that a scenario comparison confounds what it varies. Each profile carries the mortality model of its own campaign as well as its casualty rates, so no mortality ratio between the profiles is attributable to casualty volume alone, and the two effects cannot be separated within the tables reported here. Separating them would need a profile pairing one campaign's casualty rates with the other's mortality model, which is not among the configurations this project ships.

The fourth is the model's own representational gaps. Those are catalogued once, in the README's [Further Development](../README.md#further-development) section, and are not restated here; the entries bearing most directly on this document are listed below by identifier.

| Entry | Bearing on this document |
|---|---|
| L3 | Clinical teams are seized whole, so the surgical section utilisation and theatre queue figures overstate scarcity where a procedure needs only part of a section and understate it where staff are in practice shared. |
| L11 | The theatre and intensive care gating parameters are informed estimates, so the post-operative intensive care gate measures the mechanism the parameters encode rather than a mortality effect a planner should size against. |
| L12 | Scenario calibration is incomplete: `high_intensity` inherits its priority split, DNBI composition and transport times from the Falklands-calibrated base, so only its casualty rates and its mortality model are Okinawa-derived. |
| L17 | R2E holding beds carry in-theatre recovery and the strategic evacuation wait on one pool, so the R2E holding queue rows conflate two distinct demands. |
| L19 | The transport fleet-size sweep was run at one casualty rate, so the headroom it locates is Falklands-rate headroom and the comparison's surge figures are not a substitute for sweeping the fleet at surge. |
| L22 | The died-of-wounds calibration target is a bounded treated-cohort rate, so a mortality figure agreeing with it is consistent with the anchor rather than validated against it. |
| L26 | One surgery duration distribution serves every casualty regardless of severity, so theatre contention is understated on exactly the heavy, high-acuity days the surge queue figures are drawn from. |
| L28 | The pre-open hold window has no source, so its comparison establishes what the shipped value does rather than that the shipped value is the right one. |

---

## Conclusion

<small>[Return to Top](#contents)</small>

The system's resilience to surge is directly quantified rather than inferred, and the answer to this document's first question is negative. Neither R2B nor R2E can absorb Okinawa-intensity casualty rates without fundamental redesign: R2E theatre mean queue rises approximately 36-fold and R2E holding queue approximately 4.5-fold relative to the Falklands-modified baseline, R2B holding queue rises approximately 5-fold, and DOW/WIA rate rises by a factor of roughly 8, of which only part is attributable to surge (see [Interpretation](#interpretation)), all while R2B theatre queue remains at zero only because the bypass routing shunts surgical overflow onto an already saturated R2E rather than R2B absorbing any of it. Effective medical support at that intensity would require scalable holding capacity at the forward echelons, a deeper surgical roster at R2E, and dynamic load-balancing between R2B and R2E, capabilities the current static establishment does not provide.

Two qualifications travel with that conclusion. The theatre contention it identifies is not a peer-conflict phenomenon: the Falklands-load theatre queue is materially above zero, so the surge makes acute a constraint the shipped establishment already carries on its own heavy days. And the mortality ratio between the profiles is not a measure of surge alone, each profile carrying the standard of care of the campaign its casualty rates come from.

The answer to the second question, which of the levers a planner can reach move an outcome measurably, is more restrictive still. Of the four policy levers replicated here, two are resolved: the R2B pre-open hold window demonstrably retains casualties forward, and the transport fleet carries margin down to two PMV Ambulances. The forward intensive care share frontier is flat across its whole domain, and the post-operative intensive care gate's mortality effect is directional but unresolved at 50 replications; both are unresolved for the same reason, which is that the responses they move are rare events measured over cohorts of a few dozen. Reinforcement is the one lever that is both resolved and effective: at Okinawa intensity a 7-day demand cycle removes the depletion trend entirely. The mass casualty stress test shows what a surge costs where the levers do not reach, inverting the post-operative pathway split from an intensive care majority to a holding bed majority for the whole of the run rather than for the event windows alone.

A comparable Vietnam-intensity comparison remains unavailable pending a genuine FORECAS-sourced Vietnam combat-troop WIA/KIA table (see [Scenario Profiles](#scenario-profiles)).

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Blood, C. G., Zouris, J. M., & Rotblatt, D. (1998). *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

[2] Marble, S. (2025). Both joint and not: Medical support at Okinawa, 1945. *Joint Force Quarterly*, *117*(2), article 11. National Defense University Press. Retrieved 17 Aug 26, from https://digitalcommons.ndu.edu/joint-force-quarterly/vol117/iss2/11/

[3] Remondelli, M. H., Remick, K. N., Shackelford, S. A., Gurney, J. M., Pamplin, J. C., Polk, T. M., Potter, B. K., & Holt, D. B. (2023). Casualty care implications of large-scale combat operations. *Journal of Trauma and Acute Care Surgery*, *95*(2S), S180-S184. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC10389308/

[4] Fandre, M. (2020). Medical changes needed for large-scale combat operations: observations from Mission Command Training Program warfighter exercises. *Military Review*. Retrieved 27 Aug 26, from https://www.armyupress.army.mil/Journals/Military-Review/English-Edition-Archives/May-June-2020/Fandre-Medical-Changes/

[5] Sargent, R. G. (2010). Verification and validation of simulation models. In *Proceedings of the 2010 Winter Simulation Conference* (pp. 166-183). IEEE. Retrieved 27 Aug 26, from https://www.informs-sim.org/wsc10papers/016.pdf

[6] Ucar, I., Smeets, B., & Azcorra, A. (2019). simmer: Discrete-Event Simulation for R. *Journal of Statistical Software*, *90*(2), 1-30. Retrieved 27 Aug 26, from https://doi.org/10.18637/jss.v090.i02

[7] NATO Standardization Office. (2019). *AJP-4.10 Allied Joint Doctrine for Medical Support* (Edition C, Version 1). NATO Standardization Office. Retrieved 27 Aug 26, from https://www.coemed.org/files/stanags/01_AJP/AJP-4.10_EDC_V1_E_2228.pdf

[8] Law, A. M. (2020). Statistical analysis of simulation output data: the practical state of the art. In *Proceedings of the 2020 Winter Simulation Conference* (pp. 1117-1127). INFORMS Simulation Society. Retrieved 27 Aug 26, from https://informs-sim.org/wsc20papers/134.pdf

[9] Rossetti, M. D. *Simulation Modeling and Arena*, Chapter 5: Statistical Analysis for Infinite Horizon Simulation Models. Retrieved 27 Aug 26, from https://rossetti.github.io/RossettiArenaBook/05-Chapter5.html

[10] Karl, A., Eubank, R., Milovanovic, J., Reiser, M., & Young, D. (2014). Using RngStreams for parallel random number generation in C++ and R. *Computational Statistics*, *29*(5), 1301-1320. Open-access preprint retrieved 26 Jun 26, from https://arxiv.org/abs/1403.7645

[11] R Core Team. (2024). *RNGstreams: L'Ecuyer's RngStreams for parallel random number generation*. R Documentation, parallel package. Retrieved 26 Jun 26, from https://stat.ethz.ch/R-manual/R-patched/library/parallel/html/RngStream.html

[12] Rossetti, M. D. (2023). *Simulation Modeling using the Kotlin Simulation Library (KSL)*, including section 9.2, Variance Reduction Techniques. Retrieved 26 Jun 26, from https://rossetti.github.io/KSLBook/

[13] Williams, E., Szakmany, T., Spernaes, I., Muthuswamy, B., & Holborn, P. (2020). Discrete-event simulation modeling of critical care flow: new hospital, old challenges. *Critical Care Explorations*, *2*(9), e0174. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC7491890/

[14] Lewis, P. A. W., & Shedler, G. S. (1979). Simulation of nonhomogeneous Poisson processes by thinning. *Naval Research Logistics Quarterly*, *26*(3), 403-413. Naval Postgraduate School Calhoun repository. Retrieved 13 Aug 26, from https://calhoun.nps.edu/handle/10945/63159

<!-- REFERENCES END -->
