# Battlefield Casualty Handling — Multi-Run Comparative Analysis

## Abstract

<small>[Return to Top](#contents)</small>

This document presents a multi-run (n≥30 replications, 95% confidence intervals) comparative analysis of the Battlefield Casualty Handling discrete event simulation under two named casualty-rate scenario profiles: `moderate_intensity` (a Falklands 1982-modified baseline, the same casualty rate underlying the illustrative single-run analysis in `docs/Single_Run_Analysis.md`) and `high_intensity` (an Okinawa exemplar, its casualty rates calibrated from FORECAS Tables A.7/A.9 [[1]](#references) and its died-of-wounds model from the rate the US Army reported on Okinawa [[2]](#references)). Where the single-run document establishes what the modelled deployed health system does under one seed and one casualty-rate assumption, this document establishes whether those findings are an artefact of that single draw and how the same system responds when casualty production is scaled to a materially higher intensity, using the project's multi-run replication framework, which executes independent stochastic replications of the discrete event simulation and aggregates outcomes as mean, 95% confidence interval, and p10–p90 range across runs.

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
- [Conclusion](#conclusion)
- [References](#references)
<!-- TOC END -->

---

## Methodology

<small>[Return to Top](#contents)</small>

This analysis uses the project's comparative scenario runner (`run_scenario()` / `compare_scenarios()`, `R/scenario_runner.R`), which executes the multi-replication framework (`run_replications()`, `R/replication.R`) under a named scenario profile and aggregates queue and mortality KPIs across replications in the same mean (p10–p90), 95% CI format used throughout this project. A scenario profile is a named set of overrides applied on top of the shipped default `env_data.json` parameters; each profile's casualty-generation parameters (arrival-rate distributions, priority-severity mix, and died-of-wounds calibration) are calibrated against a named historical exemplar. The extent of that validation differs by profile and by parameter. Each profile's died-of-wounds ceilings are calibrated against a treated-cohort mortality rate reported for its own campaign, measured over casualties who reached a treatment facility alive rather than over all wounded (see the README's [Parameter Calibration](../README.md#parameter-calibration) and Further Development entry L22): `moderate_intensity` against the Falklands figure, `high_intensity` against the rate the US Army reported on Okinawa. What `high_intensity` still inherits from the Falklands-calibrated base is its priority split, its DNBI composition and its transport times, as the following paragraph sets out.

Both scenarios use the same health system establishment — the simulation's shipped default configuration, a representative combat brigade served by three Role 1 (R1) treatment teams, two Role 2 Basic (R2B) facilities, and one Role 2 Enhanced Heavy (R2E Heavy) hospital — since element, bed, and transport fleet counts are structural configuration, not scenario-eligible parameters. Only the casualty-generation parameters differ between `moderate_intensity` and `high_intensity`.

Two scenarios are compared — `moderate_intensity` (Falklands 1982 exemplar) and `high_intensity` (Okinawa exemplar, calibrated in its casualty generation and its died-of-wounds model but not in its triage, sub-type and transport assumptions) — both defined in `env_data.json` (Issue #54). A third, Vietnam-calibrated profile is not included: FORECAS's Appendix A has no standalone Vietnam combat-troop WIA/KIA distribution table (Table A.5 is Vietnam DNBI only) [[1]](#references), so no genuinely FORECAS-sourced Vietnam parameters exist to build one from.

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

## Conclusion

<small>[Return to Top](#contents)</small>

The system's resilience to surge is directly quantified rather than inferred. This multi-run comparison confirms that neither R2B nor R2E can absorb Okinawa-intensity casualty rates without fundamental redesign: R2E OT mean queue rises approximately 36-fold and R2E Hold mean queue approximately 4-fold relative to the Falklands-modified baseline, R2B Hold queue rises approximately 5-fold, and DOW/WIA rate rises by a factor of roughly 8, of which only part is attributable to surge (see the interpretation above), all while R2B OT queue remains at zero only because the existing bypass routing shunts all surgical overflow onto an already-saturated R2E rather than R2B absorbing any of the surge itself. Effective LSCO medical support at Okinawa intensity would require scalable holding capacity at forward echelons, a deeper surgical roster at R2E, and dynamic load-balancing between R2B and R2E, capabilities the current static establishment does not provide.

These figures were regenerated at 50 replications per scenario after the arrival generators were rebuilt around per-day rate draws and thinning, and they supersede the comparison made while a rate redrawn every simulated minute was flattening each stream's day-to-day variation to roughly a quarter of a plain Poisson process's (see the README's [Casualty Generation](../README.md#casualty-generation)). The means barely move: `moderate_intensity` total casualties from 441.6 to 437.8 per run and `high_intensity` from 1,015.6 to 992.3, which is what an unchanged configured mean should produce. What moves is the spread and everything that depends on it. The 10th to 90th percentile range on `moderate_intensity` total casualties widens from 439.0-444.1 to 362.7-528.0, and the queue rows widen with it.

The direction of that movement is what makes this the most consequential of the generator changes for the findings above. Every ratio in the tables compresses, because the Falklands-load arm rises far more than the surge arm: the R2E theatre mean queue at `moderate_intensity` rises from 0.227 to 1.06 while the `high_intensity` figure falls slightly, from 40.8 to 39.8 as then measured, and the headline ratio falls from roughly 180 to roughly 37. A ratio compressing is easy to misread as a system found more robust than it was. It is the opposite. The surge arm is close to unmoved because it was already generating contention on most days; the baseline arm rises because it now has bad days at all. What the previous arrival process supported was the claim that theatre contention is a peer-conflict problem, and that claim does not survive the correction. Transport shows the same pattern in a smaller register, its Falklands-load mean queue rising from around one ten-thousandth of a casualty to roughly four thousandths, small in absolute terms and no longer indistinguishable from zero.

The tables were then re-measured again when `high_intensity` gained a died-of-wounds model calibrated to Okinawa in place of the one it had inherited from the Falklands-calibrated base, which is the version shown above and which supersedes the `high_intensity` figures quoted in the two paragraphs before this one. The profile's own rows move as that calibration intends, DOW/WIA from 0.88% to 3.43%, and its queue rows move a little with them: the theatre and intensive care queues shorten, 39.8 to 38.2 and 0.618 to 0.564, because a casualty who dies of wounds leaves the queue for the resource that would have treated them, while the queues further from the point of death lengthen alongside a casualty count that reads 1,021.0 against 992.3. The `moderate_intensity` arm is untouched by that change and reproduced every figure above exactly, which is the evidence that the two arms differ only where they are meant to.

The qualitative conclusion is unchanged and the quantitative one is now measured against an arrival process that reproduces both the daily rate and the day-to-day variation the configuration names. The run was made in the project's pinned Dev Container (`rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`), and every figure in the tables above reproduced exactly the values that had been measured for them in an unpinned sandbox, to the precision published and including each queue group. The tables therefore carry no environment caveat, and the agreement is itself part of the evidence that the sandbox measurements this project relied on while no pinned container could be built were faithful (see `CLAUDE.md`'s Key Parameters provenance note). `images/scenario_comparison.png` was regenerated alongside the tables by the command given above. A comparable Vietnam-intensity comparison remains unavailable pending a genuine FORECAS-sourced Vietnam combat-troop WIA/KIA table.

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Blood, CG; Zouris, JM; Rotblatt, D; (1998) *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

[2] Marble, S. (2025). Both joint and not: Medical support at Okinawa, 1945. *Joint Force Quarterly*, *117*(2), article 11. National Defense University Press. Retrieved 17 Aug 26, from https://digitalcommons.ndu.edu/joint-force-quarterly/vol117/iss2/11/

<!-- REFERENCES END -->
