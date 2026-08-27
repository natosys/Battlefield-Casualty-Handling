# Battlefield Casualty Handling — Multi-Run Comparative Analysis

## Abstract

<small>[Return to Top](#contents)</small>

This document reports every replicated experiment the Battlefield Casualty Handling discrete event simulation has been subjected to, and the replication and confidence-interval methodology they rest on. Its centrepiece is a multi-run (n≥30 replications, 95% confidence intervals) comparative analysis under two named casualty-rate scenario profiles: `moderate_intensity` (a Falklands 1982-modified baseline, the same casualty rate underlying the illustrative single-run analysis in `docs/Single_Run_Analysis.md`) and `high_intensity` (an Okinawa exemplar, its casualty rates calibrated from FORECAS Tables A.7/A.9 [[1]](#references) and its died-of-wounds model from the rate the US Army reported on Okinawa [[2]](#references)). Where the companion document `docs/Single_Run_Analysis.md` establishes what the modelled deployed health system does under one seed and one casualty-rate assumption, this document establishes whether those findings are an artefact of that single draw and how the same system responds when casualty production is scaled to a materially higher intensity, using the project's multi-run replication framework, which executes independent stochastic replications of the discrete event simulation and aggregates outcomes as mean, 95% confidence interval, and p10–p90 range across runs. Six further sections apply the same framework to individual design questions the walk-through can raise but not settle: the R2B pre-open hold window, the post-operative intensive care gate, the forward ICU share policy lever, the transport fleet-size margin, the reinforcement demand cycle, and an acute mass casualty surge.

Across 50 replications of each scenario (30 simulated days, seed 42), the comparison confirms that the current establishment's adequacy conclusion does not extrapolate from Falklands to Okinawa intensity: mean total casualties per run rise 2.33-fold, the R2E Operating Theatre mean queue rises approximately 36-fold, the R2E Intensive Care Unit mean queue rises approximately 4.3-fold from a low base, the R2E Holding bed mean queue rises approximately 4.3-fold from a base already materially non-zero, and the R2B Holding bed mean queue rises approximately 5.5-fold, while R2B OT queue remains at zero in both scenarios — not because R2B absorbs any of the surge, but because the model's existing bypass routing diverts all surgical overflow to an already-saturated R2E. Died-of-wounds rate as a proportion of WIA rises from 0.42% to 3.43%, though that row alone compares two campaigns' standards of care as well as two casualty volumes, each profile carrying the mortality model of the conflict its casualty rates come from. Transport (PMV Ambulance / HX240M) remains the one echelon with genuine headroom at both intensities.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Methodology](#methodology)
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
- [Conclusion](#conclusion)
- [References](#references)
<!-- TOC END -->

---

## Methodology

<small>[Return to Top](#contents)</small>

This analysis uses the project's comparative scenario runner (`run_scenario()` / `compare_scenarios()`, `R/scenario_runner.R`), which executes the multi-replication framework (`run_replications()`, `R/replication.R`) under a named scenario profile and aggregates queue and mortality KPIs across replications in the same mean (p10–p90), 95% CI format used throughout this project. A scenario profile is a named set of overrides applied on top of the shipped default `env_data.json` parameters; each profile's casualty-generation parameters (arrival-rate distributions, priority-severity mix, and died-of-wounds calibration) are calibrated against a named historical exemplar. The extent of that validation differs by profile and by parameter. Each profile's died-of-wounds ceilings are calibrated against a treated-cohort mortality rate reported for its own campaign, measured over casualties who reached a treatment facility alive rather than over all wounded (see the README's [Parameter Calibration](../README.md#parameter-calibration) and Further Development entry L22): `moderate_intensity` against the Falklands figure, `high_intensity` against the rate the US Army reported on Okinawa. What `high_intensity` still inherits from the Falklands-calibrated base is its priority split, its DNBI composition and its transport times, as the following paragraph sets out.

Both scenarios use the same health system establishment — the simulation's shipped default configuration, a representative combat brigade served by three Role 1 (R1) treatment teams, two Role 2 Basic (R2B) facilities, and one Role 2 Enhanced Heavy (R2E Heavy) hospital — since element, bed, and transport fleet counts are structural configuration, not scenario-eligible parameters. Only the casualty-generation parameters differ between `moderate_intensity` and `high_intensity`.

Two scenarios are compared — `moderate_intensity` (Falklands 1982 exemplar) and `high_intensity` (Okinawa exemplar, calibrated in its casualty generation and its died-of-wounds model but not in its triage, sub-type and transport assumptions) — both defined in `env_data.json` (Issue #54). A third, Vietnam-calibrated profile is not included: FORECAS's Appendix A has no standalone Vietnam combat-troop WIA/KIA distribution table (Table A.5 is Vietnam DNBI only) [[1]](#references), so no genuinely FORECAS-sourced Vietnam parameters exist to build one from.

The replication counts, seeds and configurations differ between the experiments this document reports, because each is sized to the response it measures and several require a parameter override the model does not ship. Every section below therefore opens with its own design statement naming its replication count, its seed, and any parameter set away from the shipped default; the run configuration for the scenario comparison itself follows.

**Run configuration:** 50 replications × 30 days (seed 42) were run for each scenario via:

```
Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity --iterations 50 --days 30 --seed 42
```

`moderate_intensity` total casualties measure 437.8 per run (95% CI [420.99, 454.69]), and the documented seed-42 single run produces 530 (`CLAUDE.md` Key Parameters). The single run sits inside this profile's own 10th-to-90th-percentile range of 362.7 to 528.0 rather than near its mean, which is what a single draw from a wide distribution does and is no longer evidence about the runner either way. The check that the comparative runner reproduces the base configuration, and so honours the scenario mechanism's no-op guarantee for `default`, is instead made directly: `moderate_intensity` overrides only casualty-generation parameters, and every figure in the tables below reproduced exactly when this comparison was re-run in the project's pinned Dev Container.

---

## Comparative Scenario Analysis

<small>[Return to Top](#contents)</small>

### Casualty and Mortality Totals

| Metric | `moderate_intensity` (Falklands) | `high_intensity` (Okinawa) | Ratio |
|---|---|---|---|
| Total casualties/run | 437.8 (p10–p90: 362.7–528.0) | 1,021.0 (p10–p90: 906.5–1,138.5) | 2.33× |
| WIA/run | 188.7 (137.6–251.5) | 684.3 (586.2–792.5) | 3.63× |
| DOW/run | 0.78 (0–2.0) | 23.58 (18.0–32.1) | 30.2× |
| DOW/WIA rate | 0.42% (95% CI [0.29%, 0.54%]) | 3.43% (95% CI [3.24%, 3.61%]) | 8.24× |

The p10 to p90 spread on the casualty rows is the headline change from the previous measurement of this table, and it is the arrival process rather than the model around it. Both profiles now draw each stream's rate once per simulated day from the distribution its configuration names, so the between-day standard deviation the source reports reaches the output instead of being averaged away across 1,440 per-minute draws (see the README's [Casualty Generation](../README.md#casualty-generation)). Total casualties per `moderate_intensity` run previously spanned 439.0 to 444.1 between the 10th and 90th percentiles, a range of five casualties across a whole campaign; they now span 362.7 to 528.0. The means are essentially unmoved, 437.8 against 441.6, which is what an unchanged configured mean should produce. Every mean in this document should now be read against a distribution wide enough that a single run is a poor guide to it.

The died-of-wounds rows are the other change from the previous measurement, and unlike the spread they are not a discovery about the system: `high_intensity` now carries a mortality model calibrated against Okinawa rather than one inherited from the Falklands-calibrated base, so its died-of-wounds figures moved from a Falklands ceiling paired with modern treatment technique to a 1945 pairing (see the README's [Scenario Profiles](../README.md#scenario-profiles)). The 30-fold ratio between the profiles is therefore a comparison of two campaigns' mortality experience as well as of two casualty volumes, and the two effects cannot be separated within this table.

The two rows are also resolved very differently. The `moderate_intensity` figure is a single 50-replication measurement of a response averaging well under one death per replication, which 50 replications resolve only to roughly ±0.13 percentage points (see the README's [Further Development](../README.md#further-development) entry L22 for where that figure comes from); it moves between control seeds by about as much as its own interval spans, so the figure to quote for that profile is the 150-replication pooled one in the README's [Scenario Profiles](../README.md#scenario-profiles), 0.368% (95% CI [0.310%, 0.426%]) on the treated cohort. The `high_intensity` figure rests on some 24 deaths per replication and is correspondingly well resolved, its 3.43% sitting inside the 3.471% (95% CI [3.360%, 3.583%]) that the same 150-replication pooling gives. The intervals in both cases are correctly specified, the replications behind them being independent of one another (see the README's [Multi-run Replication Framework](../README.md#multi-run-replication-framework)). The queue rows below are better resolved than either at the same replication count, being time-weighted occupancy measures with far more events behind each replication.

### Resource Queue Comparison (mean of per-resource mean queue, by group)

| Resource group | `moderate_intensity` mean queue | `high_intensity` mean queue | Ratio |
|---|---|---|---|
| R2B OT | 0.000 | 0.000 | — |
| R2B Hold | 0.593 | 3.228 | 5.45× |
| R2E OT | 1.063 | 38.17 | 35.9× |
| R2E ICU | 0.131 | 0.564 | 4.29× |
| R2E Hold | 0.622 | 2.694 | 4.33× |
| Transport (PMV Ambulance / HX240M) | 0.00386 | 0.0278 | 7.22× (small in both) |

![Four-panel bar chart of mean queue length by resource group, R2B OT, R2E OT, R2E ICU and transport, each panel comparing the high intensity and moderate intensity profiles with error bars, on four different vertical scales](../images/scenario_comparison.png)

Each panel carries its own vertical scale, so the panels compare profiles rather than resources: the R2E theatre panel runs to 60 casualties while the transport panel runs to 0.07. Both bars in the R2B theatre panel sit exactly at zero, and every high intensity error bar is wide enough to show that the surge queues vary substantially from replication to replication.

### Interpretation

The comparison exposes a structural fragility that the single-run baseline could not surface on its own, and it locates that fragility in the operating theatres. Mean R2E OT queue rises from 1.06 casualties at Falklands-equivalent load to 38.2 under `high_intensity`, a factor of roughly 36, which is by a wide margin the largest movement anywhere in the model. The mechanism is the surgical roster: a casualty seizes a theatre before it seizes one of the three surgical sections that staff them, so a room reads as queued while its occupant waits for staff, and at Okinawa-intensity arrival rates that wait dominates. The ratio has fallen sharply from the roughly 180-fold previously reported, and the reason is instructive rather than reassuring: the Falklands-load figure rose almost fivefold, from 0.227 to 1.06, while the surge figure barely moved. An arrival process that can deliver a heavy day produces theatre contention at Falklands rates too, so what the previous measurement read as a fragility confined to peer conflict is better read as one that peer conflict makes acute. R2E ICU rises from 0.131 to 0.564, a factor of 4.3, and is the flattest of the three R2E groups under surge: with only the damage control cohort taking a stabilisation episode, intensive care carries one episode for half the surgical population and two for the other half rather than two for everyone. R2E holding beds rise from 0.622 to 2.69, a factor of 4.3, and absorb what intensive care does not, since the holding bed is where a casualty goes when no intensive care bed is free and where those awaiting strategic evacuation stage. Both R2E theatre and R2E intensive care queues read slightly shorter than the previous measurement of this table, at 38.2 against 39.8 and 0.564 against 0.618, while every other queue reads longer; the profile's calibrated mortality is what separates them, a casualty who dies of wounds leaving the queue for the resource that would otherwise have treated them.

R2B OT queue remains at 0 in both scenarios — not because R2B absorbs any of the surge, but because the OT-bypass routing diverts casualties requiring surgery to R2E whenever the theatre is busy or queued, or the surgical section is closed for longer than the pre-open hold window, rather than letting them wait; under `high_intensity`, this shunts the entire surge onto an R2E that has limited further capacity to absorb it. The hold window bounds how long a casualty may wait for a section about to reopen and so cannot produce a standing queue, which is why the queue reads zero even with the window open (see [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)). R2B Hold bed queue, already identified as a Falklands-rate bottleneck (see the single-run analysis's [R2B Hold Bed Saturation](Single_Run_Analysis.md#r2b-hold-bed-saturation--stream-decomposition-and-intervention-analysis) finding), increases roughly 5-fold (0.593 to 3.23), driven by the proportional increase in non-surgical WIA volume rather than any change to DNBI generation, since DNBI generation rate is not one of the parameters a scenario profile overrides.

Transport remains the one echelon with genuine headroom: mean queue stays a small fraction of a casualty even at 2.3× total casualty volume, consistent with the single-run analysis's [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin) finding that the PMV Ambulance/HX240M pool is not the binding constraint at the Falklands-derived rate. The absolute figures are nonetheless some 300 times larger than they were before the arrival process was rebuilt around per-day rate draws, at 0.0039 and 0.028 casualties against 0.0000113 and 0.000131, because a queue forms on peak days where a flattened arrival process produced none at all. The margin is wide and it is no longer untouched, which is what makes a re-run of the dedicated fleet-size sweep against the current arrival process the outstanding work rather than a re-run at higher intensity alone (see the README's Further Development entry L19).

DOW/WIA rate rises from 0.42% to 3.43%, a factor of roughly 8, on intervals that do not overlap. What that factor measures has changed with this table, and it is worth being explicit about, because it is the one row here that is no longer a statement about the modelled health system alone. Each profile now carries the mortality model of the campaign its casualty rates come from, so the gap between the rows is the combined effect of three things: Okinawa's heavier casualty volume, the treatment queues that volume produces, and a standard of surgical and resuscitative care four decades older than the Falklands profile's. The first two belong to this comparison; the third is a property of the calibration and would be present at equal casualty volumes. A planner reading the ratio as the cost of surge alone would overstate it substantially. What survives unambiguously is the mechanism the queue rows establish: at Okinawa intensity the deaths are driven by treatment queues deep enough that removing an intensive care episode and a theatre episode from half the operated population, which is what the surgical pathway split does, does not relieve them.

---

## The R2B Pre-Open Hold Window

<small>[Return to Top](#contents)</small>

R2B fields one surgical section per team on a 12-hour shift against a theatre available around the clock, so for half of each day the theatre stands ready with nobody rostered to operate in it. `r2b.surgery.pre_open_window_min` sets how long a casualty who arrives in that half may be held forward for the section, rather than being diverted to R2E as every off-shift arrival previously was (see the README's [R2B Trajectory](../README.md#r2b-trajectory)). The window ships at 60 minutes and has no source, so what it does is a question for measurement rather than for argument.

A single run cannot answer it. Turning the window on shifts simmer's single global random stream, so a zero-window run and a 60-minute run at the same seed are different realisations rather than a controlled comparison. The table below is therefore 50 replications at each setting, run at control seed 42 under the shipped default configuration in the project's pinned Dev Container. The first two columns are per-replication means; the third is the mean of the per-replication paired difference with its 95% confidence interval, which is the quantity the comparison turns on.

| Measure | Window 0 (instant diversion) | Window 60 min (shipped) | Difference (95% CI) |
| --- | --- | --- | --- |
| Casualties held forward | 0 | 5.90 | +5.90 [+5.18, +6.62] |
| R2B surgeries | 51.82 | 52.20 | +0.38 [−2.75, +3.51] |
| Bypassed, section off shift | 84.94 | 75.24 | −9.70 [−17.25, −2.15] |
| Bypassed, theatre busy or queued | 19.76 | 17.08 | −2.68 [−6.95, +1.59] |
| R2E first surgeries | 125.16 | 117.96 | −7.20 [−16.33, +1.93] |
| R2E theatre entry deferred (ICU full) | 18.94 | 15.62 | −3.32 [−6.56, −0.08] |
| Died of wounds per run | 1.02 | 1.02 | +0.00 [−0.38, +0.38] |
| R2B OT utilisation (24-hour room) | 7.0% | 7.2% | — |
| Total casualties | 442.82 | 433.18 | −9.64 [−32.00, +12.72] |

The window does what it was added to do, and the mechanism is unambiguous: 5.90 casualties are held forward per run (95% CI [5.18, 6.62]), where a zero window holds none by construction, and the off-shift bypass count falls by 9.70 ([−17.25, −2.15]). Both intervals exclude zero. Casualties that would have been diverted are instead retained forward, which is what the mechanism was added to do.

What this measurement no longer supports is the stronger claim previously made from it, that the accounting closes: that forward surgeries rise by the number held forward and by nothing else. Forward surgeries move by +0.38 ([−2.75, +3.51]), an interval comfortably spanning zero and equally comfortably spanning the +5.90 the holds would predict. The measurement cannot distinguish those two hypotheses, and it should not be read as evidence for either.

The reason is that the two arms are no longer the same realisation. A zero-window run and a 60-minute run at the same control seed share their per-replication seeds, but the first hold shifts simmer's single global stream, and Issue #18's force-regeneration loop couples arrival timing back to casualty event timing, so the arms diverge into different casualty streams: not one of the 50 replication pairs generated the same number of casualties in both arms, and the paired difference in total casualties spans −32.00 to +12.72. Pairing on the seed therefore removes none of the between-run variance, and an effect of about six operations sits inside the noise of a response whose paired difference has a standard deviation of 11.

This is a change in what the comparison can resolve rather than in the model. The previous measurement of this table put total casualties at 381.18 and 381.22 in the two arms, differing by 0.04 across a whole campaign, because the arrival process then in use flattened each stream's day-to-day variation almost to nothing; the arms were near-identical realisations and a six-operation effect stood clear of the residual noise. Restoring the configured between-day variance (see the README's [Casualty Generation](../README.md#casualty-generation)) restored the sampling variability that comparison had been borrowing against. Resolving the forward-surgery effect to a half-width of two operations would need about 120 replications per arm, and the bypass and R2E surgery rows, whose paired differences carry standard deviations of 27 and 32, would need several hundred to low thousands. Re-running at that count, or adopting a variance reduction design that survives a stream shift, is the outstanding work on this section.

Two rows are worth reading despite this. R2E theatre entry deferred for a full intensive care unit falls by 3.32 ([−6.56, −0.08]), an interval that excludes zero and points the way the mechanism predicts, earlier surgery for the casualties the window reaches relieving rearward pressure a little. And mortality is flat: deaths of wounds per run differ by 0.00 ([−0.38, +0.38]), with the treated-cohort rate at 0.37% and 0.39%, both below the approximately 0.46% Ajax Bay bound the project's one-sided calibration check applies. The earlier reading, that mortality falls from 1.10 to 0.78 deaths per run, is withdrawn: it was never resolved at this replication count, and the current measurement puts the difference at zero.

Two further limits are unchanged. The comparison was run at the shipped default configuration only, so it says nothing about how the window behaves under surge, where the forward theatre is contended and displacement would be likelier to bite. And 60 minutes is one point on a range the screening bounds run from zero to six hours, so this establishes that the shipped value drives the mechanism it was added for, not that it is the value that pays best.

---

## The Post-Operative Intensive Care Gate

<small>[Return to Top](#contents)</small>

A damage control casualty leaving theatre needs a stabilisation episode, and the model gates theatre entry on whether an intensive care bed is available to provide it: a Priority 1 casualty is operated on regardless and recovers in a holding bed at elevated risk when no bed is free, while a Priority 2 or lower casualty has theatre entry deferred until one is (see the README's [Post-Operative Stabilisation](../README.md#post-operative-stabilisation)). The seed-42 walk-through shows which casualties took the degraded route and on which day (see [R2E Heavy Handling](Single_Run_Analysis.md#r2e-heavy-handling)); whether the gate's two effects, relieved intensive care load and elevated mortality on the degraded route, survive beyond that one draw is a question for replication.

**Design.** 50 replications of 30 simulated days at an independent seed per replication (`seed = NULL`), under the shipped default configuration, run before and after the gate was introduced.

Comparing 50 independent replications made before and after the gate was introduced: mean R2E ICU utilisation fell from **74.1% to 60.2%** — a substantial, consistently-observed reduction in ICU load, not a seed-42 artefact. Mean DOW/run rose from **0.84 (95% CI [0.58, 1.10]) to 1.00 (95% CI [0.74, 1.26])** — the two confidence intervals overlap substantially, so this specific comparison does not reach conventional statistical significance at n = 50 (DOW remains a rare event; a properly powered before/after comparison would need a considerably larger replication count). The increase is, however, fully attributable to the new post-operative checkpoint: it contributed a mean of 0.10 DOW/run on its own (5 of 50 replications), accounting for essentially the entire point-estimate shift. Within that checkpoint, the qualitative design intent held using the real (non-stress-tested) parameters: the post-op hold pathway's realised DOW rate (2 deaths / 1,223 patients = 0.16%) was roughly **2.8× the ICU pathway's rate** (3 deaths / 5,085 patients = 0.06%) — the elevated-risk pathway is measurably, not just theoretically, riskier at baseline casualty rates, though the small absolute counts mean this ratio itself carries wide uncertainty. The intervals in this paragraph alone are as originally computed, over replications that were antithetically paired while the interval divided by the replication count, which makes them narrower than the runs entitle them to be. They are not recomputed because the comparison is against a configuration that no longer exists in the codebase, so the "before" arm cannot be re-run; the paragraph's own conclusion, that the two intervals overlap and the comparison does not reach significance at n = 50, is only reinforced by intervals that should be wider. Every other interval in this document is computed over independent replications.

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

The reason the lever does so little is the size of the population it acts on. Roughly half of operated casualties take the single-stage pathway, and they have no stabilisation phase to move; of the remainder, only those operated on forward at R2B can have any of it served forward. What is left is a small enough cohort that shifting all of their stabilisation forward does not measurably relieve a unit running above 83% occupancy. R2B intensive care utilisation no longer confirms the mechanism as cleanly as it did, reading 22.4% at a zero share where the beds serve only the evacuation wait and then moving without order between 14.1% and 22.7% once forward holding is enabled. That column is estimated from few enough events per replication to be poorly determined, and it should be read as showing that load moves rather than as measuring how much.

The mortality column remains unresolved. Mean deaths of wounds per run move 0.80, 1.00, 1.00, 1.10 and 1.00 across the five points with every confidence interval overlapping every other. Deaths of wounds are rare at this casualty rate, and the capability penalty applies only to the fraction of a fraction that is operated on forward and holds there, so twenty replications cannot separate an effect of this size from noise.

The shipped default therefore stays at zero, and the case for changing it is weaker rather than merely unproven. An earlier frontier appeared to show a monotonic gain in post-definitive intensive care access, but it was measured against a model that routed every operated casualty through the staged pathway, so the gain it showed was in proportion to a cohort roughly twice the size of the real one. What this frontier shows is a lever with a real mechanism and no measurable benefit at Falklands-equivalent load. It may still matter at higher casualty rates, where R2E intensive care is contended by a wider margin, and that is the experiment worth running next.

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
| 5 | 0.0001 (0.0000–0.0001) | 17.8% | — | — |

At a single vehicle the PMV Ambulance fleet queues heavily, at a mean of 2.11 casualties, and the sweep locates the capacity boundary sharply rather than merely confirming the current always-zero finding. Queue falls by roughly a factor of forty at two vehicles and by a further factor of seven at the current three, where it is small but no longer exactly zero at 0.0068 casualties. That last point is the substantive change from the previous measurement of this sweep, which put every fleet size from two vehicles upward at an exact zero. The margin is still wide, and the fleet still carries more headroom than one additional vehicle would supply, but it is now a margin rather than an absence of demand, and the same is true one row down: a reduction from three PMV Ambulances to two would raise the mean queue from 0.0068 to 0.0487, both small, neither zero. HX240M behaves the same way an order of magnitude lower, reaching zero only at its current four vehicles.

The reason the whole column moved is the arrival process rather than the transport model, which is unchanged. This sweep had not been re-run since the casualty generators were rebuilt around per-day rate draws, so it previously measured a demand stream with almost none of the day-to-day variation its configuration names (see the README's [Casualty Generation](../README.md#casualty-generation)). A transport queue forms on peak days and on no others, which is precisely what a flattened arrival process could not produce, and it is why the single-vehicle figure rises by a factor of fifty-four while the establishment-size figures rise from exact zeros. The seed-42 walk-through shows the same thing directly, its PMV Ambulance pool queueing for the first time (see [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin)).

Mean utilisation across the swept range remains too weakly determined to read. It runs the wrong way on both platforms, rising with fleet size where a fixed demand spread over more vehicles should lower it, and the interval on HX240M utilisation at three vehicles spans 2.3% to 19.9%. So few transport events occur per replication that the busy-time estimate at each sweep point is barely determined, which the wide 95% CI ribbons on the utilisation panels of the plot above show in the same way. The queue column now carries the weight it previously shared with those exact zeros, and it is the column to read. `outputs/transport_capacity_by_fleet_size.csv` provides the full per-point results, including CI bounds omitted from the table above.

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

All four rows were measured in the pinned Dev Container against the current model, the `high_intensity` rows including that profile's own died-of-wounds calibration.

At `high_intensity` casualty rates the mechanism is clear: daily volume falls significantly with no reinforcement, a 26% first-to-last-week decline at a slope of −0.349/day (p = 0.0027), and the demand-cycle reinforcement configuration removes that decline entirely, leaving a slope statistically indistinguishable from flat (+0.030/day, p = 0.80, under 1% first-to-last-week change). Reinforcement arrests depletion without overshooting into net growth. This is a direct consequence of the demand-based design: because each cycle's demand is the pool's actual current shortfall rather than a fixed size, a well-sustained pool automatically asks for less on its next cycle. The evidence for the decline is weaker than it was, the p-value moving from 9.6×10⁻¹⁴ to 0.0027 even as the slope steepened, and that is the restored day-to-day arrival variation rather than a weaker mechanism: the daily counts the regression is fitted to are now far noisier, so the same trend is established at less confidence. At `moderate_intensity` neither configuration shows a slope distinguishable from flat, which is the same conclusion as before.

`force_regeneration.reinforcement` (`env_data.json`) remains a fully planner-tunable input — the demand cycle, fulfillment lag, and all three triangular fill parameters — and this project does not attempt to auto-balance it against a scenario's attrition rate; the 7-day/7-day configuration above is illustrative, not a recommended operational setting.

The table demonstrates the mechanism's direction and statistical behaviour across replications. It is not a substitute for the seed-42 baseline figures, which are reported in the [Force Regeneration Feedback Loop](Single_Run_Analysis.md#force-regeneration-feedback-loop) walk-through and in `CLAUDE.md`.

---

## Mass Casualty Event Stress Test

<small>[Return to Top](#contents)</small>

The preceding sections analyse sustained casualty tempo, the background lognormal and exponential streams at either Falklands or Okinawa intensity. This section tests a qualitatively different scenario: an acute, discrete casualty surge layered on top of the Falklands-calibrated background tempo, using the compound Poisson mass casualty injection mechanism, under which discrete mass-casualty events of stochastic size are injected on top of the sustained background arrival streams.

An event's drawn casualty count is now its total rather than its survivor count: a configured share of it, `mass_casualty.event.kia_fraction`, arrives killed at or near the point of injury and is handled by the mortuary pathway rather than triaged (see the README's [Mass Casualty Event Injection](../README.md#5-mass-casualty-event-injection)). At the shipped share of 0.28, a 30-day run at this rate and seed produces 78 event-derived casualties, of which 27 are immediate killed and 51 wounded, where every one of the 78 would previously have been wounded. The wounded load an event places on the surgical echelons is therefore smaller than the figures below record for the same nominal event size, and a load on mortuary handling and killed-casualty transport that those figures do not include at all now accompanies it.

**Design.** 10 replications of 30 simulated days at control seed 42, under the shipped default configuration with one override: `mass_casualty.event.rate_per_day` set to 0.2 events per day (a mean 5-day inter-event interval), against a background-only arm at the shipped value of 0. Mass casualty injection ships disabled, so every figure in this section, the single illustrative run that follows the table and `images/mass_casualty_events.png` alike, requires that override and none of them can be reproduced by a shipped-configuration run. The figure is accordingly the one tracked image that `run.R --refresh-baseline` cannot write, and is copied into place from the run's own output directory.

| Metric | Background-only baseline | With mass casualty injection |
|---|---|---|
| Mean total casualties/run | 444.6 | 682.1 |
| Mean mass casualty events/run | 0 | 5.40 (range 3–8) |
| DOW rate — background-origin casualties | 0.18% (8/4,446) | 0.28% (13/4,577) |
| DOW rate — mass-casualty-origin casualties | — | 0.58% (13/2,244) |

The mean 5.40 events per 30-day run sits a little below the configured 0.2/day event rate (theoretical expectation: 30 × 0.2 = 6), which ten replications cannot separate from it; event count varies from 3 to 8 across those replications, confirming the Poisson process is genuinely stochastic rather than deterministic. At a mean 41.6 casualties per event the drawn sizes sit close to the midpoint of the configured 20 to 60 range, as a uniform draw should. Mass-casualty-origin casualties show a died-of-wounds rate 2.1 times the background-origin rate, 0.58% against 0.28%, which is consistent with the intended stress-test effect of a blast-dominant priority mix arriving faster than steady-state capacity. Three qualifications apply. It is a per-casualty-origin comparison rather than a strict temporal-window comparison (see the assumption note in the analysis code, `R/analysis.R`); deaths of wounds remain rare at this sample size, 13 in each arm, so the ratio is illustrative of direction rather than precise; and the background column is no longer the quiet baseline it once was, since the background stream can now itself deliver a heavy day, which is why its own died-of-wounds rate is non-zero at 0.18% and why the contrast between the two arms is narrower than previously published.

A single seed-42 run under the same override, without replication averaging, illustrates the mechanism directly: 537 total casualties, 459 from the background streams and 78 injected by two events, one of 33 casualties on day 14 and one of 45 on day 27. Both are recovered exactly by the gap-based reconstruction the analysis pipeline applies, which at two well-separated events has nothing to merge; the heuristic's known failure mode, two closely spaced events read as one, is a property of a busier event schedule than this seed produced.

The effect on the R2E theatre and intensive care gate is the clearest single-run signal. Post-operative stabilisation splits `hold=85` against `icu=37` under injection, against `hold=58` and `icu=79` in the shipped background-only run at the same seed (see the [R2E Heavy Handling](Single_Run_Analysis.md#r2e-heavy-handling) walk-through): the majority pathway inverts, and a cohort that mostly recovered in an intensive care bed mostly recovers in a holding bed instead. That is the degraded-care substitution the gate exists to expose, and it persists across the whole 30-day run rather than only during the two event windows.

The measures that once moved most now barely move at all, and the reason is instructive. OT-entry deferrals for ICU-saturated Priority 2 and lower casualties read 25 under injection against 29 without it, and upstream pre-bypass from R1 reads 177 against 179; R2E theatre utilisation rises only from 24.2% to 25.7% (against R2B's 5.2% to 4.9%). Under the previous, flattened arrival process these were the section's headline movements, because the background load left the forward echelons with spare capacity that a surge could visibly consume. An arrival process that delivers its own heavy days has already consumed it, so an injected event adds to a system that is intermittently saturated regardless. The surge still degrades the care delivered, as the pathway inversion shows; what it no longer does is reveal a constraint that the background tempo was hiding.

![Stem plot of the two mass casualty events reconstructed from the run, each drawn as a vertical line at its simulation day with a point at its casualty count: 33 casualties midway through day 13 and 45 midway through day 26](../images/mass_casualty_events.png)

Two events, thirteen days apart, is a thin draw from a process configured to deliver a mean of six over the run, and it is why this seed illustrates the injection mechanism rather than measures its effect. The replicated table above carries the measurement.

---

## Conclusion

<small>[Return to Top](#contents)</small>

The system's resilience to surge is directly quantified rather than inferred. This multi-run comparison confirms that neither R2B nor R2E can absorb Okinawa-intensity casualty rates without fundamental redesign: R2E OT mean queue rises approximately 36-fold and R2E Hold mean queue approximately 4-fold relative to the Falklands-modified baseline, R2B Hold queue rises approximately 5-fold, and DOW/WIA rate rises by a factor of roughly 8, of which only part is attributable to surge (see the interpretation above), all while R2B OT queue remains at zero only because the existing bypass routing shunts all surgical overflow onto an already-saturated R2E rather than R2B absorbing any of the surge itself. Effective LSCO medical support at Okinawa intensity would require scalable holding capacity at forward echelons, a deeper surgical roster at R2E, and dynamic load-balancing between R2B and R2E, capabilities the current static establishment does not provide.

These figures were regenerated at 50 replications per scenario after the arrival generators were rebuilt around per-day rate draws and thinning, and they supersede the comparison made while a rate redrawn every simulated minute was flattening each stream's day-to-day variation to roughly a quarter of a plain Poisson process's (see the README's [Casualty Generation](../README.md#casualty-generation)). The means barely move: `moderate_intensity` total casualties from 441.6 to 437.8 per run and `high_intensity` from 1,015.6 to 992.3, which is what an unchanged configured mean should produce. What moves is the spread and everything that depends on it. The 10th to 90th percentile range on `moderate_intensity` total casualties widens from 439.0-444.1 to 362.7-528.0, and the queue rows widen with it.

The direction of that movement is what makes this the most consequential of the generator changes for the findings above. Every ratio in the tables compresses, because the Falklands-load arm rises far more than the surge arm: the R2E theatre mean queue at `moderate_intensity` rises from 0.227 to 1.06 while the `high_intensity` figure falls slightly, from 40.8 to 39.8 as then measured, and the headline ratio falls from roughly 180 to roughly 37. A ratio compressing is easy to misread as a system found more robust than it was. It is the opposite. The surge arm is close to unmoved because it was already generating contention on most days; the baseline arm rises because it now has bad days at all. What the previous arrival process supported was the claim that theatre contention is a peer-conflict problem, and that claim does not survive the correction. Transport shows the same pattern in a smaller register, its Falklands-load mean queue rising from around one ten-thousandth of a casualty to roughly four thousandths, small in absolute terms and no longer indistinguishable from zero.

The tables were then re-measured again when `high_intensity` gained a died-of-wounds model calibrated to Okinawa in place of the one it had inherited from the Falklands-calibrated base, which is the version shown above and which supersedes the `high_intensity` figures quoted in the two paragraphs before this one. The profile's own rows move as that calibration intends, DOW/WIA from 0.88% to 3.43%, and its queue rows move a little with them: the theatre and intensive care queues shorten, 39.8 to 38.2 and 0.618 to 0.564, because a casualty who dies of wounds leaves the queue for the resource that would have treated them, while the queues further from the point of death lengthen alongside a casualty count that reads 1,021.0 against 992.3. The `moderate_intensity` arm is untouched by that change and reproduced every figure above exactly, which is the evidence that the two arms differ only where they are meant to.

The qualitative conclusion is unchanged and the quantitative one is now measured against an arrival process that reproduces both the daily rate and the day-to-day variation the configuration names. The run was made in the project's pinned Dev Container (`rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`), and every figure in the tables above reproduced exactly the values that had been measured for them in an unpinned sandbox, to the precision published and including each queue group. The tables therefore carry no environment caveat, and the agreement is itself part of the evidence that the sandbox measurements this project relied on while no pinned container could be built were faithful (see `CLAUDE.md`'s Key Parameters provenance note). `images/scenario_comparison.png` was regenerated alongside the tables by the command given above. A comparable Vietnam-intensity comparison remains unavailable pending a genuine FORECAS-sourced Vietnam combat-troop WIA/KIA table.

The six experiments reported alongside the comparison qualify what a planner may do about the constraints it exposes, and the qualification is consistent across them. Of the four policy levers replicated here, only two are resolved: the R2B pre-open hold window demonstrably retains casualties forward, and the transport fleet carries margin down to two PMV Ambulances. The forward ICU share frontier is flat across its whole domain, the post-operative intensive care gate's mortality effect is directional but unresolved at 50 replications, and both are unresolved for the same reason, which is that the responses they move are rare events measured over cohorts of a few dozen. Reinforcement, by contrast, is resolved and works: at Okinawa intensity a 7-day demand cycle removes the depletion trend entirely. The mass casualty stress test shows what a surge costs where the levers do not reach, inverting the post-operative pathway split from an intensive care majority to a holding bed majority for the whole of the run rather than for the event windows alone.

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Blood, CG; Zouris, JM; Rotblatt, D; (1998) *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

[2] Marble, S. (2025). Both joint and not: Medical support at Okinawa, 1945. *Joint Force Quarterly*, *117*(2), article 11. National Defense University Press. Retrieved 17 Aug 26, from https://digitalcommons.ndu.edu/joint-force-quarterly/vol117/iss2/11/

<!-- REFERENCES END -->
