# Applying Discrete Event Simulation to the Land-Based Trauma System: Replicated Experiments and Comparative Scenario Analysis

## Abstract

<small>[Return to Top](#contents)</small>

This document reports every experiment the Battlefield Casualty Handling discrete event simulation has been run under replication, together with the replication and confidence interval methods behind them. At its centre is a comparison of two casualty-rate scenario profiles at 50 replications each: `moderate_intensity`, a Falklands 1982-modified baseline and the casualty rate behind the single-run verification in `docs/Single_Run_Analysis.md`, and `high_intensity`, an Okinawa exemplar whose casualty rates come from FORECAS Tables A.7/A.9 [[1]](#references) and whose died-of-wounds model is fitted to the rate the US Army reported on Okinawa [[2]](#references). The companion document shows what the modelled health system does under one seed and one casualty-rate assumption; this one shows which of those findings survive replication, and how the same system copes when casualty production is scaled to a far higher intensity. Six further sections put the same framework to design questions a single run can raise but not settle: the R2B pre-open hold window, the post-operative intensive care gate, the forward intensive care share, the transport fleet-size margin, the reinforcement demand cycle, and an acute mass casualty surge. Replication counts run from 10 to 50, matched to the response each experiment measures, and every section states its own.

Across 50 replications of each scenario (30 simulated days, control seed 42), the comparison finds that the establishment's adequacy at Falklands rates does not carry over to Okinawa intensity. Mean total casualties per run rise 2.33-fold. The R2E operating theatre mean queue rises about 36-fold, the R2E intensive care queue about 4.3-fold from a low base, the R2E holding bed queue about 4.5-fold from a base already well above zero, and the R2B holding bed queue about 5.5-fold. The R2B theatre queue stays at zero in both profiles, not because R2B absorbs any of the surge but because the bypass routing pushes all surgical overflow onto an R2E that is already saturated. Died-of-wounds as a proportion of WIA rises from 0.42% to 3.43%, a row that compares two campaigns' standards of care as well as two casualty volumes, since each profile carries the mortality model of the conflict its casualty rates come from. Transport, the PMV Ambulance and HX240M pools, is the one echelon with real headroom at both intensities.

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

A deployed health system has to be sized before the campaign it will serve, and the planner sizing it needs two things that one simulated campaign cannot give. The first is how much of what a single run shows is the system and how much is chance. The second is how far a conclusion reached at one casualty intensity carries to another. That second question presses hard, because the casualty volumes expected in large scale combat operations are greater than those the deployed health systems of the past two decades were built around [[3]](#references), and the planning assumptions carried forward from those operations have been argued to understate both the volume and the acuity a peer fight would produce [[4]](#references).

This document therefore asks two questions. Does the shipped establishment's apparent adequacy at a Falklands-derived casualty rate survive replication and a change of casualty intensity? And which of the levers a planner can actually reach, meaning the parameters the model exposes rather than the structures it fixes, move an outcome enough to be measured at a replication count the project can afford?

The [Comparative Scenario Analysis](#comparative-scenario-analysis) answers the first, running the same establishment under two calibrated casualty-rate profiles and reporting the difference with intervals. The six experiment sections after it answer the second, one lever at a time: the R2B pre-open hold window, the post-operative intensive care gate, the forward intensive care share, the transport fleet size, the reinforcement demand cycle, and acute mass casualty events injected on top of the sustained background tempo. The answer to the second question is the more negative of the two, and that negative answer is reported rather than passed over: three of the six experiments measure a lever whose effect the replication count cannot separate from noise, and each says so and states the count it would take.

What this document does not do is validate the model against the real system. Showing that a model behaves as its specification describes, and showing that the specification represents the system well, are separate tasks resting on separate evidence [[5]](#references). The first is the subject of `docs/Single_Run_Analysis.md`; the second, as far as the available historical anchors allow, of the README's [Parameter Calibration](../README.md#parameter-calibration) section. The experiments below are run on the model, and their conclusions hold only as far as the model does. The [Limitations](#limitations) section states the conditions that bear hardest on them.

---

## Methods

<small>[Return to Top](#contents)</small>

### Design and Unit of Analysis

The simulation is a discrete event model built on the `simmer` package for R [[6]](#references). Each casualty is an entity that arrives, then claims and releases clinical staff, beds, operating theatres and transport as it moves rearward through the echelons of allied medical support doctrine [[7]](#references). Every experiment here runs it through the project's multi-run replication framework (`run_replications()`, `R/replication.R`), which executes a requested number of independent replications and returns their monitoring data with a replication index attached.

The replication is the unit of analysis throughout; neither the casualty nor the simulated day is [[8]](#references). Each replication is one complete campaign, run from an empty and idle system over the horizon its section states, and every response is reduced to a single number per replication before any statistic is taken across replications. For a resource that number is the time-weighted mean queue length over the replication's observation window; for a count, the count within it. Taking statistics over casualties or over days instead would treat observations from inside one campaign as though they were separate campaigns, and would produce intervals far narrower than the runs support.

No warm-up period is discarded from any observation window. Discarding one removes the settling-in behaviour a model shows before reaching steady state, which is worth doing only when steady state is the quantity of interest [[9]](#references). This model is a terminating simulation with a fixed campaign length and a genuinely empty start, so that opening period is part of what a planner needs to see. The README's [Warm-up Period Analysis](../README.md#warm-up-period-analysis) sets out that classification and reports the Welch graphical diagnostic, run over ten 90-day replications, which supports it.

### Replication Independence

Every interval in this document divides a sample standard deviation by the square root of the replication count, which is valid only if the replications are independent of one another. Here that independence follows from how a replication is built, rather than being inferred from the output.

Two facts establish it. First, `run_once()` (`R/replication.R`) is a pure function of its seed: re-running a seed reproduces its output exactly, whether or not another run happens in between, so the seed is all that distinguishes two replications and no other channel connects them. Second, `run_replications()` draws a distinct seed for each replication. Independent seeds into a pure function give independent replications, and that is a deterministic argument rather than one a finite sample of correlations could support. `scripts/check_replication_independence.R` asserts both facts and runs on every pull request.

Running replications in parallel preserves both facts. They run under `RNGkind("L'Ecuyer-CMRG")` with `mc.set.seed = TRUE`, which gives each worker its own substream of the MRG32k3a generator, and the substream spacing of $2^{76}$ makes overlap impossible at any simulation budget used here [[10]](#references)[[11]](#references). Both dispatch paths use the one generator, so a replication's output depends on its seed and not on the path or the core count that produced it. A figure quoted at a control seed reproduces at that seed whatever else the session has measured first.

No variance reduction scheme sits on top of that. Antithetic pairing was used earlier in the project and then withdrawn, because neither of the conditions it needs holds for this model. Its reach stops at the arrival generators, since `simmer` draws service times and routing probabilities from the global stream inside its own event loop, in an order set by event timing rather than by entity. The technique also requires the response to move consistently in one direction with the input uniforms [[12]](#references), and casualty arrivals do not, their rate being scaled by an effective force size that trajectory outcomes themselves add to and subtract from. Measurement agrees: over 75 pairs the within-pair correlation on total casualties is $-0.04$ (95% CI $[-0.27, +0.19]$), worth a variance reduction of about 3% and indistinguishable from none. The README's [Multi-run Replication Framework](../README.md#multi-run-replication-framework) gives the construction and that measurement in full. One consequence of the withdrawn pairing reaches this document and is flagged where it appears: the intervals in [The Post-Operative Intensive Care Gate](#the-post-operative-intensive-care-gate) were computed while the pairing was still in force, over replications that were not independent, and are narrower than those runs entitle them to be.

### Confidence Intervals

Every confidence interval in this document is a Student $t$ interval on the mean across replications,

$$\bar{x} \pm t_{0.975,\;n-1}\,\frac{s}{\sqrt{n}}$$

where $n$ is the replication count, $\bar{x}$ the mean across replications of the per-replication response and $s$ its sample standard deviation. No normal approximation and no bootstrap appears anywhere in this document. The same expression is evaluated in `summarise_replications()` (`R/replication.R`), in the comparative scenario runner (`R/scenario_runner.R`) and everywhere in the analysis pipeline that reports an interval, so an interval quoted here, one shown in the Shiny console and one written to a CSV output are the same quantity computed the same way. Where the response cannot go below zero, which is true of every queue length and every count, a negative lower bound is truncated at zero. That truncation is a reporting convention and does not change the arithmetic behind it.

Where a comparison sets two configurations run at the same control seed against each other, the interval is on the mean of the per-replication paired difference rather than on the difference of the two means, and the tables say so in their column headings. Pairing on the seed is worth doing only where it removes variance, and one section below reports that it does not (see [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)).

A 10th-to-90th-percentile range is not a confidence interval and is never used as one here. Where a table reports both, the interval says how precisely the mean is known and the range says how widely a single campaign varies around it. The range is many times the wider in every table below, and it is the range to keep in mind when reading any figure from a single run.

### Replication Count and Resolution

Replication counts differ between the experiments below because the responses differ in how many events each replication supplies. A death count, of which a Falklands-rate campaign produces about one, rests on a handful of events per replication; a time-weighted mean queue rests on every arrival and departure at that resource over 30 days.

The best-determined spread available for this model is on the treated-cohort died-of-wounds rate, whose per-replication standard deviation is 0.0039 under the shipped configuration, measured over 150 replications. At that spread, a 95% half-width of 0.15 percentage points needs 29 replications, one of 0.10 pp needs 62, and one of 0.05 pp needs 237. The 50-replication mortality figures below therefore carry roughly $\pm 0.11$ pp on this response. Single 50-replication measurements of one unchanged configuration span 0.132 pp across control seeds, which is why `scripts/check_dow_calibration.R` pools three independent measurements rather than reporting one. `CLAUDE.md`'s Key Parameters table cross-references this section for that derivation.

Queue and occupancy responses are far better resolved at the same replication count, being time-weighted over orders of magnitude more events per replication. A comparable critical care discrete event simulation reports its occupancy and queue results at 95% intervals over replication counts of this order for the same reason [[13]](#references). The pattern runs through every section below. At 50 replications the queue rows of the comparison separate the two casualty-rate profiles decisively; the mortality rows separate them only because the gap between them is large. A rare-event response measured over a cohort of a few dozen casualties, which is what the post-operative mortality of each intensive care pathway amounts to, is not resolved at any replication count this project has run, and the sections reporting such responses say so rather than reading a point estimate as a finding.

### Scenario Profiles

A scenario profile is a named set of overrides applied on top of the shipped default `env_data.json` parameters, resolved by `resolve_scenario()` (`R/scenario.R`). Two are compared here, `moderate_intensity` (Falklands 1982 exemplar) and `high_intensity` (Okinawa exemplar), both defined in the `scenarios` block of `env_data.json`. A third, Vietnam-calibrated profile is missing for want of sources: FORECAS's Appendix A has no standalone Vietnam combat-troop WIA/KIA distribution table, Table A.5 covering Vietnam DNBI only [[1]](#references), so there are no genuinely FORECAS-sourced Vietnam parameters to build one from.

Each profile's casualty-generation parameters, meaning its arrival-rate distributions, its priority-severity mix and its died-of-wounds calibration, are fitted to a named historical campaign, though how completely varies by profile and by parameter. The died-of-wounds ceilings are fitted to a treated-cohort mortality rate reported for the profile's own campaign, measured over casualties who reached a treatment facility alive rather than over all wounded (see the README's [Parameter Calibration](../README.md#parameter-calibration)): `moderate_intensity` to the Falklands figure, `high_intensity` to the rate the US Army reported on Okinawa [[2]](#references). What `high_intensity` still takes from the Falklands-calibrated base is its priority split, its DNBI composition and its transport times.

Both profiles use the same health system establishment, the simulation's shipped default: a representative combat brigade served by three Role 1 treatment teams, two Role 2 Basic (R2B) facilities and one Role 2 Enhanced Heavy (R2E Heavy) hospital. Element, bed and transport fleet counts are structural configuration and cannot be overridden by a scenario, so the two profiles differ in their casualty-generation parameters alone.

### Run Configuration and Provenance

Every section below opens with its own design statement, naming its replication count, its control seed and any parameter set away from the shipped default. The comparative scenario analysis is invoked as:

```
Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity --iterations 50 --days 30 --seed 42
```

The figures in that comparison, and `images/scenario_comparison.png` with them, were produced in the project's pinned development container (`rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`), so none carries a caveat about the environment it was produced in. The seed-42 evidence set the single-run document reports reproduces byte for byte in that same container, and `scripts/check_baseline_reproduction.R` re-checks the reproduction whenever the model changes.

One feature of the comparison is worth stating, because it is easily mistaken for a defect. `moderate_intensity` gives 437.8 total casualties per run, while the documented seed-42 single run produces 530 (`CLAUDE.md` Key Parameters). The single run sits inside this profile's own 10th-to-90th-percentile range of 362.7 to 528.0 rather than near its mean, which is what one draw from a wide distribution does. That the comparative runner does reproduce the base configuration, and so honours the scenario mechanism's guarantee of changing nothing it does not name, is established directly instead: `moderate_intensity` overrides casualty-generation parameters only, and every casualty, mortality and queue figure in the tables below reproduces exactly when the comparison is re-run at this seed, inside the pinned container and outside it alike.

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

The casualty rows carry a wide p10 to p90 spread because each arrival stream draws its rate once per simulated day from the distribution its configuration names, then places that day's arrivals within the day by thinning [[14]](#references). The between-day standard deviation the source reports therefore reaches the output rather than being averaged away (see the README's [Casualty Generation](../README.md#casualty-generation)). Total casualties per `moderate_intensity` run span 362.7 to 528.0 between those percentiles against a mean of 437.8, a spread of roughly a third of the mean. Every mean in this document should be read against a distribution wide enough that one campaign is a poor guide to it.

The died-of-wounds rows measure more than the health system. Each profile carries the mortality model of the campaign its casualty rates come from, `high_intensity` having its own Okinawa calibration rather than one inherited from the Falklands-calibrated base (see [Scenario Profiles](#scenario-profiles)). The 30-fold ratio between the profiles therefore compares two campaigns' mortality experience as well as two casualty volumes, and this table cannot separate the two.

The two rows are also resolved very differently. The `moderate_intensity` figure is one 50-replication measurement of a response averaging well under a single death per replication, which 50 replications pin down only to roughly ±0.13 percentage points (see [Replication Count and Resolution](#replication-count-and-resolution)). It moves between control seeds by about as much as its own interval spans, so the figure to quote for that profile is the 150-replication pooled one in the README's [Scenario Profiles](../README.md#scenario-profiles), 0.368% (95% CI [0.310%, 0.426%]) on the treated cohort. The `high_intensity` figure rests on some 24 deaths per replication and is correspondingly well resolved, its 3.43% sitting inside the 3.471% (95% CI [3.360%, 3.583%]) the same pooling gives. Both intervals are correctly specified, the replications behind them being independent of one another (see [Replication Independence](#replication-independence)). The queue rows below are better resolved than either at the same replication count, being time-weighted occupancy measures with far more events behind each replication.

### Resource Queue Comparison (mean of per-resource mean queue, by group)

| Resource group | `moderate_intensity` mean queue (95% CI) | `high_intensity` mean queue (95% CI) | Ratio |
|---|---|---|---|
| R2B OT | 0.000 [0.000, 0.000] | 0.000 [0.000, 0.000] | not applicable |
| R2B Hold | 0.593 [0.501, 0.685] | 3.228 [3.005, 3.452] | 5.45× |
| R2E OT | 1.063 [0.691, 1.435] | 38.17 [34.01, 42.33] | 35.9× |
| R2E ICU | 0.131 [0.104, 0.159] | 0.564 [0.464, 0.664] | 4.29× |
| R2E Hold | 0.598 [0.437, 0.758] | 2.694 [2.449, 2.938] | 4.51× |
| Transport (PMV Ambulance / HX240M) | 0.0038 [0.0000, 0.0078] | 0.0278 [0.0196, 0.0361] | 7.25× (small in both) |

Each cell is the mean across the 50 replications of that replication's mean queue over the group's resources, with the Student $t$ interval of [Confidence Intervals](#confidence-intervals) on that mean. A resource idle throughout a replication contributes a zero rather than dropping out of that replication's average, so the denominator is the group's full establishment in every replication. The R2B theatre interval collapses to a point because every replication of both profiles returns exactly zero, which is a property of the bypass routing rather than of the estimate (see [Interpretation](#interpretation)). Transport is the one group whose lower bound truncates at zero.

![Four-panel bar chart of mean queue length by resource group, R2B OT, R2E OT, R2E ICU and transport, each panel comparing the high intensity and moderate intensity profiles with error bars, on four different vertical scales](../images/scenario_comparison.png)

Each panel carries its own vertical scale, so the panels compare profiles rather than resources: the R2E theatre panel runs to 60 casualties while the transport panel runs to 0.07. Both bars in the R2B theatre panel sit exactly at zero. The error bars show the mean of the per-resource p10 to p90 range across replications, not a confidence interval, and every high intensity bar is wide enough to show that the surge queues vary a great deal from replication to replication.

### Interpretation

The comparison exposes a structural weakness the single-run baseline could not surface on its own, and it locates that weakness in the operating theatres. The mean R2E theatre queue rises from 1.06 casualties at Falklands-equivalent load to 38.2 under `high_intensity`, a factor of roughly 36 and by a wide margin the largest movement anywhere in the model. The surgical roster is the mechanism: a casualty takes a theatre before taking one of the three surgical sections that staff them, so a room reads as queued while its occupant waits for people, and at Okinawa-intensity arrival rates that wait dominates. How to read the surge figure depends on the Falklands-load figure being well above zero already. An arrival process that delivers genuine heavy days produces theatre contention at Falklands rates too, so contention is a standing property of this establishment that peer-conflict intensity makes acute, not a weakness confined to peer conflict. R2E intensive care rises from 0.131 to 0.564, a factor of 4.3, and is the flattest of the three R2E groups under surge: only the damage control cohort takes a stabilisation episode, so intensive care carries one episode for half the surgical population and two for the other half rather than two for everyone. R2E holding beds rise from 0.598 to 2.69, a factor of 4.5, absorbing what intensive care does not, since a holding bed is where a casualty goes when no intensive care bed is free and where those awaiting strategic evacuation wait.

The R2B theatre queue stays at 0 in both scenarios, and not because R2B absorbs any of the surge. The bypass routing sends a casualty requiring surgery to R2E whenever the theatre is busy or queued, or the surgical section has been closed for longer than the pre-open hold window, rather than letting them wait. Under `high_intensity` that pushes the entire surge onto an R2E with little spare capacity to take it. The hold window caps how long a casualty may wait for a section about to reopen and so cannot produce a standing queue, which is why the queue reads zero even with the window open (see [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)). The R2B holding bed queue, already a bottleneck at Falklands rates (see the single-run analysis's [R2B Hold Bed Saturation](Single_Run_Analysis.md#r2b-hold-bed-saturation-stream-decomposition-and-intervention-analysis) finding), rises roughly 5-fold, from 0.593 to 3.23. What drives it is the proportional rise in non-surgical WIA volume rather than any change to DNBI, whose generation rate a scenario profile does not override.

Transport stays the one echelon with real headroom: its mean queue remains a small fraction of a casualty even at 2.3 times the casualty volume, which agrees with the single-run analysis's [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin) finding that the PMV Ambulance and HX240M pool is not the binding constraint at Falklands-derived rates. The margin is wide but not untouched: a queue forms on the peak days both profiles produce, which is why these figures are small rather than exactly zero, and why re-running the dedicated fleet-size sweep against the current arrival process is outstanding work in its own right (see the README's Further Development entry L19).

The died-of-wounds rate as a share of WIA rises from 0.42% to 3.43%, a factor of roughly 8, on intervals that do not overlap. What that factor measures needs saying plainly, because this is the one row here that is not about the modelled health system alone. Each profile carries the mortality model of the campaign its casualty rates come from, so the gap between the rows combines three things: Okinawa's heavier casualty volume, the treatment queues that volume produces, and a standard of surgical and resuscitative care four decades older than the Falklands profile's. The first two belong to this comparison. The third belongs to the calibration and would show up even at equal casualty volumes, so a planner reading the ratio as the cost of surge alone would overstate it substantially. What survives without qualification is the mechanism the queue rows establish: at Okinawa intensity, deaths occur against treatment queues deep enough that removing an intensive care episode and a theatre episode from half the operated population, which is what the surgical pathway split does, does not relieve them.

---

## The R2B Pre-Open Hold Window

<small>[Return to Top](#contents)</small>

R2B fields one surgical section per team on a 12-hour shift against a theatre available around the clock, so for half of each day the theatre stands ready with nobody rostered to operate in it. `r2b.surgery.pre_open_window_min` sets how long a casualty arriving in that half may be held forward for the section instead of being sent to R2E (see the README's [R2B Trajectory](../README.md#r2b-trajectory)). The window ships at 60 minutes and has no source behind it (see the README's Further Development entry L28), so what it achieves is a question to be measured rather than argued. One run cannot answer it either: turning the window on shifts simmer's single global random stream, so a zero-window run and a 60-minute run at the same seed are two different realisations rather than a controlled comparison.

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

On what the window was added to do, the measurement is decisive. It holds 5.90 casualties forward per run (95% CI [5.18, 6.62]), where a zero window holds none by construction, and the off-shift bypass count falls by 9.70 ([−17.25, −2.15]). Neither interval includes zero. Casualties that would have been sent rearward are kept forward instead.

What the measurement cannot establish is the accounting behind that: whether forward surgeries rise by the number held forward and by nothing else. Forward surgeries move by +0.38 ([−2.75, +3.51]), an interval that comfortably contains zero and just as comfortably contains the +5.90 the holds would predict. It cannot tell those two possibilities apart, and should not be read as evidence for either.

That is a limit on what this design can resolve, not a finding about the window, and the reason is that the two arms are not the same realisation. A zero-window run and a 60-minute run at the same control seed share their per-replication seeds, but the first hold shifts simmer's single global stream, and the force-regeneration loop feeds arrival timing back from casualty event timing (see the README's [Force Regeneration and the Endogenous Feedback Loop](../README.md#6-force-regeneration-and-the-endogenous-feedback-loop)), so the two arms drift into different casualty streams. Not one of the 50 replication pairs generated the same number of casualties in both arms, and the paired difference in total casualties spans −32.00 to +12.72. Pairing on the seed therefore removes none of the between-run variance, and an effect of about six operations disappears into the noise of a response whose paired difference has a standard deviation of 11.

Those standard deviations set the replication count this comparison would need. Resolving the forward-surgery effect to a half-width of two operations takes about 120 replications per arm. The bypass and R2E surgery rows, whose paired differences carry standard deviations of 27 and 32, would take several hundred to a few thousand. Neither count has been run, so the forward-surgery, bypass-composition and R2E-surgery rows above are reported as unresolved rather than as effects. Re-running at those counts, or finding a variance reduction design that survives a stream shift, is the outstanding work here.

Two rows are still worth reading. Theatre entry at R2E deferred for a full intensive care unit falls by 3.32 ([−6.56, −0.08]), an interval that excludes zero and points the way the mechanism predicts: operating earlier on the casualties the window reaches relieves a little rearward pressure. And mortality is flat, deaths of wounds per run differing by 0.00 ([−0.38, +0.38]), with the treated-cohort rate at 0.37% and 0.39%, both under the roughly 0.46% Ajax Bay bound the project's one-sided calibration check applies. That is a null result at this replication count rather than a demonstration that the window costs no lives, deaths of wounds being the rarest response in the table.

Two further limits apply to the design itself. The comparison was run at the shipped default configuration only, so it says nothing about the window under surge, where the forward theatre is contended and displacing one casualty with another would be likelier to bite. And 60 minutes is a single point on a range the screening bounds take from zero to six hours, so this shows that the shipped value drives the mechanism it was added for, not that it is the value that pays best.

---

## The Post-Operative Intensive Care Gate

<small>[Return to Top](#contents)</small>

A damage control casualty leaving theatre needs a period of stabilisation, and the model makes entry to theatre depend on an intensive care bed being free to provide it. A Priority 1 casualty is operated on regardless and recovers in a holding bed at raised risk when no bed is free; a Priority 2 or lower casualty waits to enter theatre until one comes free (see the README's [Post-Operative Stabilisation](../README.md#post-operative-stabilisation)). The seed-42 walk-through shows which casualties took the degraded route and on which day (see [R2E Heavy Handling](Single_Run_Analysis.md#r2e-heavy-handling)). Whether the gate's two effects, lighter intensive care load and higher mortality on the degraded route, survive beyond that one draw is a question for replication.

**Design.** 50 replications of 30 simulated days at an independent seed per replication (`seed = NULL`), under the shipped default configuration, run before and after the gate was introduced. The intervals in this section alone were computed over replications that were antithetically paired while the interval still divided by the replication count, which makes them narrower than those runs entitle them to be (see [Replication Independence](#replication-independence)). They are not recomputed, because the comparison is against a configuration that no longer exists in the codebase and the earlier arm cannot be re-run. Intervals that should be wider only strengthen this section's conclusion, which is that the two overlap and the comparison does not reach significance at 50 replications. Every other interval in this document is computed over independent replications.

Mean R2E intensive care utilisation fell from **74.1% to 60.2%**, a large reduction in load seen consistently across replications rather than an artefact of one seed. Mean deaths of wounds per run rose from **0.84 (95% CI [0.58, 1.10]) to 1.00 (95% CI [0.74, 1.26])**. Those intervals overlap heavily, so the comparison does not reach conventional significance at 50 replications, deaths of wounds being a rare event at this casualty rate. The rise is nonetheless attributable to the new post-operative checkpoint, which contributed a mean of 0.10 deaths per run on its own, in 5 replications of 50, accounting for almost the whole movement in the point estimate.

Inside the checkpoint the design behaved as intended under the shipped parameters, not only under a stress test. The holding pathway's realised death rate, 2 in 1,223 casualties or 0.16%, was roughly **2.8 times the intensive care pathway's rate** of 3 in 5,085 or 0.06%. The higher-risk pathway is measurably riskier at baseline casualty rates, not just riskier in principle, though the small counts leave the ratio itself very uncertain and it should be read for its direction rather than its size.

The mortality mechanism was confirmed separately by a stress test that forced intensive care capacity to zero over a 90-day run. The degraded route then carries most casualties and produces measurable post-operative deaths, which establishes that the checkpoint fires as designed. It does not establish that the effect is quantitatively resolved at Falklands-calibrated rates.

---

## Forward ICU Share Decision Frontier

<small>[Return to Top](#contents)</small>

A casualty's need for stabilisation is a single quantity that the forward-holding policy divides between R2B and R2E, while the post-definitive care following their definitive repair is a separate episode delivered only at R2E (see the README's [Post-Operative Stabilisation](../README.md#post-operative-stabilisation)). The stabilisation total stays the same at every setting, so sweeping the policy moves load between echelons without changing how much care is given. That is what makes it a real planning lever rather than a quiet reduction in treatment. Only damage control casualties have a stabilisation phase, so the lever acts on roughly half of operated casualties rather than all of them (see the README's [Surgical Pathway](../README.md#surgical-pathway)).

**Design.** 20 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: `r2b_icu_share` set to 0, 0.25, 0.5, 0.75 and 1.0 in turn. Point 0 is the shipped default. Run via `Rscript scripts/run_icu_share_sweep.R --iterations 20 --days 30`.

![Five stacked line plots against the share of post-operative intensive care delivered forward at R2B, from 0% to 100%, showing R2E ICU mean queue, R2B ICU utilisation, R2E ICU utilisation, the share of post-definitive care delivered in ICU, and DOW count, each with a 95% confidence ribbon and a dashed line at the shipped default of 0%](../images/r2b_icu_share_frontier.png)

Every panel moves little across the full sweep, and every confidence ribbon is wide enough to cover the whole movement, so at this replication count the lever is not resolved. The frontier shows where the trade-off would appear rather than how large it is.

| Forward ICU share | R2E ICU mean queue (95% CI) | R2B ICU utilisation | R2E ICU utilisation | Post-definitive care in ICU (95% CI) | Mean DOW per run (95% CI) |
|---|---|---|---|---|---|
| 0% (shipped) | 0.108 (0.066–0.149) | 22.4% | 87.7% | 35.5% (28.4–42.6) | 0.80 (0.35–1.25) |
| 25% | 0.080 (0.042–0.119) | 22.1% | 84.9% | 38.7% (30.4–46.9) | 1.00 (0.52–1.48) |
| 50% | 0.078 (0.028–0.129) | 14.1% | 83.4% | 41.6% (34.5–48.8) | 1.00 (0.52–1.48) |
| 75% | 0.079 (0.036–0.121) | 20.2% | 83.8% | 42.2% (31.4–52.9) | 1.10 (0.47–1.73) |
| 100% | 0.125 (0.033–0.218) | 22.7% | 83.9% | 42.0% (32.4–51.6) | 1.00 (0.25–1.75) |

With only the damage control cohort having a stabilisation phase to move, the lever stops earning its keep. Every quantity in the table is flat across the swept range. The R2E intensive care queue moves between 0.078 and 0.125 casualties with overlapping intervals and no trend, and its highest value falls at the 100% share, where the lever is meant to help most. R2E utilisation sits between 83.4% and 87.7% at every setting, and is likewise highest at the shipped zero share.

The share of casualties receiving post-definitive care in an intensive care bed is the one column that appears to move in a direction, rising from 35.5% at a zero share to 42.0% at a full one. It should not be read as a gain. Every interval in that column overlaps every other, each spanning some fourteen percentage points against a movement of seven, and the values do not rise in order. Twenty replications cannot separate a trend this small from noise on this response.

The lever does so little because the population it acts on is small. Roughly half of operated casualties take the single-stage pathway and have no stabilisation phase to move at all; of the rest, only those operated on forward at R2B can have any of it served forward. What remains is a cohort small enough that moving all of their stabilisation forward does not measurably relieve a unit running above 83% occupancy. R2B intensive care utilisation supports the mechanism only weakly: it reads 22.4% at a zero share, where the beds serve the evacuation wait alone, then moves between 14.1% and 22.7% in no particular order once forward holding is enabled. Too few events per replication go into that column for it to be well determined, and it should be read as showing that load moves rather than as measuring how much.

The mortality column stays unresolved. Mean deaths of wounds per run read 0.80, 1.00, 1.00, 1.10 and 1.00 across the five points, every confidence interval overlapping every other. Deaths of wounds are rare at this casualty rate, and the penalty for reduced capability applies only to the fraction of a fraction operated on forward and held there, so twenty replications cannot separate an effect this small from noise.

The shipped default therefore stays at zero. The frontier shows a lever with a real mechanism and no measurable benefit at Falklands-equivalent load, acting as it does on roughly half of operated casualties and, within that half, only on those operated on forward. It may still matter at higher casualty rates, where R2E intensive care is contended by a wider margin, and that is the experiment worth running next.

---

## Transport Fleet-Size Sweep

<small>[Return to Top](#contents)</small>

The seed-42 walk-through shows that the shipped three-vehicle PMV Ambulance and four-vehicle HX240M pools carry plenty of headroom, the PMV Ambulance pool queueing only briefly and the HX240M pool not at all (see [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin)). What one run cannot say is where that headroom ends. This sweep varies the number of vehicles directly, rather than the casualty rate or the transport duration, and so finds the fleet size at which transport becomes the binding constraint.

**Design.** 10 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: the PMV Ambulance fleet swept across 1 to 5 vehicles and the HX240M fleet across 1 to 4, each with the other fleet held at its shipped establishment size.

`plot_transport_capacity_margin_by_fleet_size()` (`R/analysis.R`) rebuilds the environment at each sweep point via `build_environment()` and runs the same replication engine (`run_replications()`, `R/replication.R`) the comparative scenario runner uses. The sweep was run via `Rscript scripts/run_transport_sweep.R`:

![Four-panel line plot of mean queue and mean utilisation against fleet size for the PMV Ambulance and HX240M fleets, each line with a 95% confidence ribbon and a dashed vertical line marking the current establishment size](../images/transport_capacity_margin_by_fleet_size.png)

The PMV Ambulance queue collapses between one and two vehicles and is flat after that, so the shipped establishment of three sits on the flat part of the curve rather than at its bend. Utilisation rises again beyond three vehicles because the measure averages across vehicles in a pool that is rarely fully engaged.

| Fleet size | PMV Ambulance mean queue (95% CI) | PMV Ambulance mean utilisation | HX240M mean queue (95% CI) | HX240M mean utilisation |
|---|---|---|---|---|
| 1 | 2.1060 (0.2270–3.9850) | 38.7% | 0.0442 (0.0000–0.1021) | 10.7% |
| 2 | 0.0487 (0.0000–0.0974) | 18.5% | 0.0011 (0.0000–0.0022) | 5.5% |
| 3 (current PMV) | 0.0068 (0.0000–0.0155) | 12.5% | 0.0001 (0.0000–0.0002) | 11.1% |
| 4 (current HX240M) | 0.0006 (0.0000–0.0012) | 14.6% | 0.0000 | 14.6% |
| 5 | 0.0001 (0.0000–0.0001) | 17.8% | not swept | not swept |

At a single vehicle the PMV Ambulance fleet queues heavily, at a mean of 2.11 casualties, so the sweep locates the capacity boundary sharply rather than merely confirming the always-zero finding at the current size. The queue falls by roughly a factor of forty at two vehicles and by a further factor of seven at the current three, where it is small but not exactly zero, at 0.0068 casualties. The margin is wide, and the fleet carries more headroom than one extra vehicle would supply, but it is a margin rather than an absence of demand. The row below says the same: cutting from three PMV Ambulances to two would raise the mean queue from 0.0068 to 0.0487, both small, neither zero. The HX240M pool behaves the same way an order of magnitude lower, reaching zero only at its current four vehicles. What produces a queue at all is the day-to-day variation in casualty volume rather than the average volume, a transport queue forming on peak days and on no others (see the README's [Casualty Generation](../README.md#casualty-generation)). The seed-42 walk-through shows the same thing within one campaign, its PMV Ambulance pool queueing briefly (see [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin)).

Mean utilisation across the swept range is too poorly determined to read. It runs the wrong way on both platforms, rising with fleet size where a fixed demand spread over more vehicles should lower it, and the interval on HX240M utilisation at three vehicles spans 2.3% to 19.9%. So few transport events occur per replication that the busy-time estimate at each sweep point is barely pinned down, which is what the wide 95% confidence ribbons on the utilisation panels above show. The queue column is the one to read. `outputs/transport_capacity_by_fleet_size.csv` holds the full per-point results, including the interval bounds left out of the table.

---

## Force Regeneration Under Reinforcement

<small>[Return to Top](#contents)</small>

Casualties in this model are generated against the force actually present at the time rather than against a fixed roll strength, so sustained losses should reduce daily casualty volume as the force depletes, and an active reinforcement cycle should offset that decline (see the README's [Force Regeneration and the Endogenous Feedback Loop](../README.md#6-force-regeneration-and-the-endogenous-feedback-loop)). The seed-42 walk-through shows the depletion curve for one campaign (see [Force Regeneration Feedback Loop](Single_Run_Analysis.md#force-regeneration-feedback-loop)). A slope fitted to daily volume is a regression on a noisy series, and needs replication before it can be read.

**Design.** 15 replications per row at `moderate_intensity` and 12 at `high_intensity`, each of 30 simulated days, with daily casualty volume averaged across replications and fitted with an ordinary least-squares trend against simulation day. The unreinforced rows use the shipped default (`force_regeneration.reinforcement.demand_interval_days = 0`, which disables the mechanism). The reinforced rows override it with a 7-day demand submission cycle, a 7-day fulfillment lag and the shipped default triangular fill distribution (`fill_min_frac = 0.2`, `fill_mode_frac = 0.85`, `fill_max_frac = 1.1`).

| Scenario | Reinforcement | Daily volume slope | p-value | First-week mean | Last-week mean |
|---|---|---|---|---|---|
| `moderate_intensity` (15 reps) | None | −0.018/day | 0.75 | 14.9 | 14.4 |
| `moderate_intensity` (15 reps) | 7-day demand cycle, 7-day lag | −0.103/day | 0.17 | 16.6 | 14.1 |
| `high_intensity` (12 reps) | None | −0.349/day | 0.0027 | 39.8 | 29.5 |
| `high_intensity` (12 reps) | 7-day demand cycle, 7-day lag | +0.030/day | 0.80 | 35.8 | 36.1 |

All four rows were measured in the pinned Dev Container, the `high_intensity` rows including that profile's own died-of-wounds calibration.

At `high_intensity` casualty rates the mechanism is clear. Daily volume falls significantly without reinforcement, declining 26% from the first week to the last at a slope of −0.349/day (p = 0.0027), and the demand-cycle configuration removes that decline entirely, leaving a slope indistinguishable from flat (+0.030/day, p = 0.80, under 1% change from first week to last). Reinforcement halts the depletion without overshooting into growth, which follows directly from the demand-based design: each cycle asks for the pool's actual current shortfall rather than a fixed number, so a well-sustained pool asks for less next time. The daily counts the regression is fitted to are themselves noisy, each carrying the between-day variation the arrival process delivers, so even a steep slope is established at moderate rather than overwhelming confidence. At `moderate_intensity` neither configuration shows a slope distinguishable from flat, that profile's casualty volume being too low for 30 days of attrition to deplete the force measurably.

`force_regeneration.reinforcement` (`env_data.json`) is entirely for the planner to set, covering the demand cycle, the fulfillment lag and all three triangular fill parameters, and this project makes no attempt to balance it automatically against a scenario's attrition rate. The 7-day/7-day configuration above illustrates the mechanism; it is not a recommended operational setting.

The table shows the mechanism's direction and its statistical behaviour across replications. It does not replace the seed-42 baseline figures, which are reported in the [Force Regeneration Feedback Loop](Single_Run_Analysis.md#force-regeneration-feedback-loop) walk-through and in `CLAUDE.md`.

---

## Mass Casualty Event Stress Test

<small>[Return to Top](#contents)</small>

The sections before this one examine sustained casualty tempo, the background lognormal and exponential streams at either Falklands or Okinawa intensity. This section tests something different in kind: a sudden casualty surge laid on top of the Falklands-calibrated background tempo. The compound Poisson injection mechanism delivers it, adding discrete mass casualty events of random size to the sustained background streams.

The casualty count drawn for an event is its total, not the number of survivors. A configured share of it, `mass_casualty.event.kia_fraction`, arrives killed at or near the point of injury and goes to the mortuary pathway rather than through triage (see the README's [Mass Casualty Event Injection](../README.md#5-mass-casualty-event-injection)). At the shipped share of 0.28, a 30-day run at this rate and seed produces 78 event-derived casualties, 27 of them killed outright and 51 wounded. An event of a given nominal size therefore loads the surgical echelons less than its drawn count suggests, while loading mortuary handling and the transport of the dead in a way the surgical figures do not show.

**Design.** 10 replications of 30 simulated days at control seed 42, under the shipped default configuration with one override: `mass_casualty.event.rate_per_day` set to 0.2 events per day, a mean of five days between events, against a background-only arm at the shipped value of 0. Injection ships disabled, so everything in this section needs that override, the illustrative single run and `images/mass_casualty_events.png` included, and none of it can be reproduced by a shipped-configuration run. That makes this figure the one tracked image `run.R --refresh-baseline` cannot write; it is copied into place from the run's own output directory.

| Metric | Background-only baseline | With mass casualty injection |
|---|---|---|
| Mean total casualties/run | 444.6 | 682.1 |
| Mean mass casualty events/run | 0 | 5.40 (range 3–8) |
| DOW rate, background-origin casualties | 0.18% (8/4,446) | 0.28% (13/4,577) |
| DOW rate, mass-casualty-origin casualties | not applicable | 0.58% (13/2,244) |

The mean of 5.40 events per 30-day run sits a little below the 6 the configured 0.2/day rate implies, a difference ten replications cannot separate from zero. Event counts range from 3 to 8 across those replications, which confirms the Poisson process is genuinely random rather than fixed. At a mean of 41.6 casualties per event, the drawn sizes sit near the middle of the configured 20 to 60 range, as a uniform draw should. Casualties from mass casualty events die of wounds at 2.1 times the background rate, 0.58% against 0.28%, consistent with the stress test's intent: a blast-dominant priority mix arriving faster than steady-state capacity can absorb. Three qualifications apply. The comparison sorts casualties by origin rather than by a strict time window around each event (see the assumption note in `R/analysis.R`). Deaths of wounds are rare at this sample size, 13 in each arm, so the ratio shows a direction rather than a precise figure. And the background column is not a quiet baseline, the background stream delivering heavy days of its own, which is why its died-of-wounds rate is 0.18% rather than zero and why the gap between the arms is narrower than the injected volume alone would suggest.

One seed-42 run under the same override, with no averaging across replications, shows the mechanism directly: 537 total casualties, 459 from the background streams and 78 from two events, one of 33 casualties on day 14 and one of 45 on day 27. The gap-based reconstruction the analysis pipeline applies recovers both exactly, having nothing to merge when two events are this far apart. Its known failure mode, reading two closely spaced events as one, needs a busier event schedule than this seed produced.

The clearest signal in that single run is what happens at the R2E intensive care gate. Post-operative stabilisation splits `hold=85` against `icu=37` under injection, where the shipped background-only run at the same seed gives `hold=58` and `icu=79` (see the [R2E Heavy Handling](Single_Run_Analysis.md#r2e-heavy-handling) walk-through). The majority pathway flips: a cohort that mostly recovered in an intensive care bed now mostly recovers in a holding bed. That is the substitution of degraded care the gate exists to expose, and it lasts the whole 30 days rather than only the two event windows.

The theatre and bypass measures barely move, and that is the finding. Deferred theatre entry for Priority 2 and lower casualties with intensive care full reads 25 under injection against 29 without it, and upstream bypass from R1 reads 177 against 179. R2E theatre utilisation rises only from 24.2% to 25.7%, while R2B's falls from 5.2% to 4.9%. A background tempo that delivers heavy days of its own has already used up the forward echelons' spare capacity, so an injected event lands on a system that is intermittently saturated anyway. The surge still degrades the care delivered, as the flipped pathway split shows. What it does not do is reveal a constraint the background tempo was hiding.

![Stem plot of the two mass casualty events reconstructed from the run, each drawn as a vertical line at its simulation day with a point at its casualty count: 33 casualties midway through day 13 and 45 midway through day 26](../images/mass_casualty_events.png)

Two events thirteen days apart is a thin draw from a process configured to deliver an average of six across the run, which is why this seed illustrates the injection mechanism rather than measuring its effect. The replicated table above carries the measurement.

---

## Limitations

<small>[Return to Top](#contents)</small>

Four kinds of limitation bear on how the findings above should be read.

The first is resolution. Three of the six experiments report a lever whose effect the replication count cannot separate from noise: the forward-surgery, bypass-composition and R2E-surgery rows of the pre-open hold window comparison, the whole of the forward intensive care share frontier, and the mortality arm of the post-operative intensive care gate. Each states the count it would need, and none of those counts has been run. Treat those rows as bounds on an effect rather than estimates of one, and do not read a point estimate whose interval spans zero as evidence of a small effect (see [Replication Count and Resolution](#replication-count-and-resolution)).

The second is that several comparisons are not controlled in the way a paired design assumes. Any parameter change that alters the order of events shifts `simmer`'s single global random stream, and the force-regeneration loop then feeds that shift back into arrival timing, so two arms run at one control seed drift into different casualty streams. Under those conditions pairing on the seed removes no variance at all, and the size of the problem is quantified where it bites hardest (see [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)). Only the scenario comparison escapes it, its two arms differing by design rather than by a small perturbation.

The third is that the scenario comparison varies two things at once. Each profile carries the mortality model of its own campaign as well as its casualty rates, so no mortality ratio between the profiles can be put down to casualty volume alone, and the tables here cannot separate the two. Separating them would need a profile combining one campaign's casualty rates with the other's mortality model, which is not among the configurations this project ships.

The fourth is what the model does not represent. Those gaps are catalogued once, in the README's [Further Development](../README.md#further-development) section, and are not repeated here. The entries bearing most directly on this document are listed below by identifier.

| Entry | Bearing on this document |
|---|---|
| L3 | Clinical teams are taken whole, so the surgical section utilisation and theatre queue figures overstate scarcity where a procedure needs only part of a section, and understate it where staff would in practice be shared. |
| L11 | The theatre and intensive care gating parameters are informed estimates, so the post-operative gate measures the mechanism those parameters encode rather than a mortality effect a planner should size against. |
| L12 | Scenario calibration is incomplete. `high_intensity` takes its priority split, DNBI composition and transport times from the Falklands-calibrated base, so only its casualty rates and its mortality model come from Okinawa. |
| L17 | One pool of R2E holding beds carries both in-theatre recovery and the strategic evacuation wait, so the R2E holding queue rows combine two separate demands. |
| L19 | The transport fleet-size sweep was run at one casualty rate, so the headroom it finds is Falklands-rate headroom, and the comparison's surge figures do not substitute for sweeping the fleet under surge. |
| L22 | The died-of-wounds calibration target is a bounded treated-cohort rate, so a mortality figure that agrees with it is consistent with the anchor rather than validated against it. |
| L26 | A single surgery duration distribution serves every casualty whatever their severity, so theatre contention is understated on exactly the heavy, high-acuity days the surge queue figures come from. |
| L28 | The pre-open hold window has no source behind it, so its comparison shows what the shipped value does rather than that the shipped value is the right one. |

---

## Conclusion

<small>[Return to Top](#contents)</small>

The system's ability to absorb a surge is measured here rather than inferred, and the answer to this document's first question is no. Neither R2B nor R2E can take Okinawa-intensity casualty rates without fundamental redesign. Against the Falklands-modified baseline, the R2E theatre mean queue rises about 36-fold and the R2E holding queue about 4.5-fold, the R2B holding queue rises about 5-fold, and the died-of-wounds rate as a share of WIA rises by a factor of roughly 8, only part of which is attributable to the surge (see [Interpretation](#interpretation)). Through all of it the R2B theatre queue stays at zero, and only because the bypass routing pushes surgical overflow onto an already saturated R2E rather than R2B taking any of it. Effective medical support at that intensity would need holding capacity that can scale at the forward echelons, a deeper surgical roster at R2E, and the ability to balance load between R2B and R2E as it changes, none of which the current static establishment provides.

Two qualifications travel with that conclusion. The theatre contention it identifies is not peculiar to peer conflict: the theatre queue at Falklands load is already well above zero, so the surge makes acute a constraint the shipped establishment carries on its own heavy days. And the mortality ratio between the profiles does not measure surge alone, each profile carrying the standard of care of the campaign its casualty rates come from.

The answer to the second question, which levers a planner can reach move an outcome measurably, is narrower still. Of the four policy levers replicated here, two are resolved: the R2B pre-open hold window demonstrably keeps casualties forward, and the transport fleet holds margin down to two PMV Ambulances. The forward intensive care share frontier is flat across its whole range, and the post-operative gate's mortality effect points in the expected direction but is unresolved at 50 replications. Both are unresolved for the same reason, which is that the responses they move are rare events measured over cohorts of a few dozen casualties. Reinforcement is the one lever that is both resolved and effective: at Okinawa intensity a 7-day demand cycle removes the depletion trend entirely. The mass casualty stress test shows what a surge costs where no lever reaches, flipping the post-operative split from a majority in intensive care to a majority in holding beds, and holding it there for the whole run rather than for the event windows alone.

A comparable Vietnam-intensity comparison has to wait for a genuine FORECAS-sourced Vietnam combat-troop WIA/KIA table (see [Scenario Profiles](#scenario-profiles)).

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
