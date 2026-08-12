# Battlefield Casualty Handling — Multi-Run Comparative Analysis

## Abstract

<small>[Return to Top](#contents)</small>

This document presents a multi-run (n≥30 replications, 95% confidence intervals) comparative analysis of the Battlefield Casualty Handling discrete event simulation under two named casualty-rate scenario profiles: `moderate_intensity` (a Falklands 1982-modified baseline, the same casualty rate underlying the illustrative single-run analysis in `docs/Single_Run_Analysis.md`) and `high_intensity` (an Okinawa exemplar, calibrated from FORECAS Tables A.7/A.9 [[1]](#references)). Where the single-run document establishes what the modelled deployed health system does under one seed and one casualty-rate assumption, this document establishes whether those findings are an artefact of that single draw and how the same system responds when casualty production is scaled to a materially higher intensity, using the project's multi-run replication framework, which executes independent stochastic replications of the discrete event simulation and aggregates outcomes as mean, 95% confidence interval, and p10–p90 range across runs.

Across 50 replications of each scenario (30 simulated days, seed 42), the comparison confirms that the current establishment's adequacy conclusion does not extrapolate from Falklands to Okinawa intensity: mean total casualties per run rise 2.51-fold, the R2E Operating Theatre mean queue rises approximately 143-fold, the R2E Intensive Care Unit mean queue rises approximately 12.8-fold from a low base, the R2E Holding bed mean queue roughly doubles from a base already materially non-zero, and the R2B Holding bed mean queue rises approximately 10.8-fold, while R2B OT queue remains at zero in both scenarios — not because R2B absorbs any of the surge, but because the model's existing bypass routing diverts all surgical overflow to an already-saturated R2E. Died-of-wounds rate as a proportion of WIA rises from 0.54% to 0.86%. Transport (PMV Ambulance / HX240M) remains the one echelon with genuine headroom at both intensities.

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

This analysis uses the project's comparative scenario runner (`run_scenario()` / `compare_scenarios()`, `R/scenario_runner.R`), which executes the multi-replication framework (`run_replications()`, `R/replication.R`) under a named scenario profile and aggregates queue and mortality KPIs across replications in the same mean (p10–p90), 95% CI format used throughout this project. A scenario profile is a named set of overrides applied on top of the shipped default `env_data.json` parameters; each profile's casualty-generation parameters (arrival-rate distributions, priority-severity mix, and died-of-wounds calibration) are calibrated against a named historical exemplar. The extent of that validation differs by profile and by parameter: the `moderate_intensity` died-of-wounds ceilings are validated against a treated-cohort mortality rate reported for casualties reaching forward surgical care, not against a whole-of-wounded rate (see the README's [Parameter Calibration](../README.md#parameter-calibration) and Further Development entry L22), while `high_intensity` has only its generation rates and distribution family sourced, as the following paragraph sets out.

Both scenarios use the same health system establishment — the simulation's shipped default configuration, a representative combat brigade served by three Role 1 (R1) treatment teams, two Role 2 Basic (R2B) facilities, and one Role 2 Enhanced Heavy (R2E Heavy) hospital — since element, bed, and transport fleet counts are structural configuration, not scenario-eligible parameters. Only the casualty-generation parameters differ between `moderate_intensity` and `high_intensity`.

Two scenarios are compared — `moderate_intensity` (Falklands 1982 exemplar) and `high_intensity` (Okinawa exemplar, an explicitly unvalidated demonstration skeleton) — both defined in `env_data.json` (Issue #54). A third, Vietnam-calibrated profile is not included: FORECAS's Appendix A has no standalone Vietnam combat-troop WIA/KIA distribution table (Table A.5 is Vietnam DNBI only) [[1]](#references), so no genuinely FORECAS-sourced Vietnam parameters exist to build one from.

**Run configuration:** 50 replications × 30 days (seed 42) were run for each scenario via:

```
Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity --iterations 50 --days 30 --seed 42
```

`moderate_intensity` total casualties (381.1, 95% CI [380.8, 381.5]) match the documented seed-42 single-run baseline (382, `CLAUDE.md` Key Parameters) within 0.3%, confirming that the comparative runner reproduces the single-run baseline under the Falklands profile it is scenario-explicit about, consistent with the scenario mechanism's no-op guarantee for `default`.

---

## Comparative Scenario Analysis

<small>[Return to Top](#contents)</small>

### Casualty and Mortality Totals

| Metric | `moderate_intensity` (Falklands) | `high_intensity` (Okinawa) | Ratio |
|---|---|---|---|
| Total casualties/run | 381.1 (p10–p90: 380.0–383.0) | 972.5 (p10–p90: 969.0–976.0) | 2.55× |
| WIA/run | 151.1 (150.0–152.0) | 656.5 (653.9–659.0) | 4.34× |
| DOW/run | 0.68 (0–2.0) | 6.04 (3.0–8.0) | 8.88× |
| DOW/WIA rate | 0.45% (95% CI [0.30%, 0.60%]) | 0.92% (95% CI [0.83%, 1.01%]) | 2.05× |

The two died-of-wounds rows are each a single 50-replication measurement of a response averaging well under one death per replication at Falklands load, and 50 replications resolve a died-of-wounds rate only to roughly ±0.13 percentage points (see the README's [Further Development](../README.md#further-development) entry L22 for where that figure comes from). The intervals shown are correctly specified, the replications behind them being independent of one another (see the README's [Multi-run Replication Framework](../README.md#multi-run-replication-framework)); they are simply wide, and the `moderate_intensity` figure moves between control seeds by about as much as the interval spans. The figure to quote for that profile is therefore the 150-replication pooled one in the README's [Scenario Profiles](../README.md#scenario-profiles), 0.40% (95% CI [0.31%, 0.49%]), which this table's single measurement is one draw around, and the ratio between the two profiles should be read as approximate for the same reason. The queue rows below are better resolved at the same replication count: they are time-weighted occupancy measures with far more events behind each replication.

### Resource Queue Comparison (mean of per-resource mean queue, by group)

| Resource group | `moderate_intensity` mean queue | `high_intensity` mean queue | Ratio |
|---|---|---|---|
| R2B OT | 0.000 | 0.000 | — |
| R2B Hold | 0.309 | 3.170 | 10.26× |
| R2E OT | 0.092 | 39.26 | 427.3× |
| R2E ICU | 0.087 | 0.571 | 6.54× |
| R2E Hold | 0.260 | 2.326 | 8.93× |
| Transport (PMV Ambulance / HX240M) | 0.0000019 | 0.0000666 | 34.9× (negligible in both) |

![Comparative Scenario Analysis](../images/scenario_comparison.png)

### Interpretation

The comparison exposes a structural fragility that the single-run baseline could not surface on its own, and it locates that fragility in the operating theatres. Mean R2E OT queue rises from 0.092 casualties at Falklands-equivalent load to 39.3 under `high_intensity`, a factor of roughly 430, which is by a wide margin the largest movement anywhere in the model. The mechanism is the surgical roster: a casualty seizes a theatre before it seizes one of the three surgical sections that staff them, so a room reads as queued while its occupant waits for staff, and at Okinawa-intensity arrival rates that wait dominates. R2E ICU rises from 0.087 to 0.57, a factor of 6.5, and is the flattest of the three R2E groups under surge: with only the damage control cohort taking a stabilisation episode, intensive care carries one episode for half the surgical population and two for the other half rather than two for everyone. R2E holding beds rise from 0.26 to 2.33, a factor of 8.9, and absorb what intensive care does not, since the holding bed is where a casualty goes when no intensive care bed is free and where those awaiting strategic evacuation stage.

R2B OT queue remains at 0 in both scenarios — not because R2B absorbs any of the surge, but because the OT-bypass routing diverts casualties requiring surgery to R2E whenever the theatre is busy or queued, or the surgical section is closed for longer than the pre-open hold window, rather than letting them wait; under `high_intensity`, this shunts the entire surge onto an R2E that has limited further capacity to absorb it. The hold window bounds how long a casualty may wait for a section about to reopen and so cannot produce a standing queue, which is why the queue reads zero even with the window open (see [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)). R2B Hold bed queue — already identified as a Falklands-rate bottleneck (see the single-run analysis's [R2B Hold Bed Saturation](Single_Run_Analysis.md#r2b-hold-bed-saturation--stream-decomposition-and-intervention-analysis) finding) — increases roughly 10-fold (0.31 to 3.17), driven by the proportional increase in non-surgical WIA volume rather than any change to DNBI generation, since DNBI generation rate is not one of the parameters a scenario profile overrides.

Transport remains the one echelon with genuine headroom: mean queue stays a small fraction of a casualty even at 2.5× total casualty volume, consistent with the single-run analysis's [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin) finding that the PMV Ambulance/HX240M pool is not the binding constraint at the Falklands-derived rate — though the roughly 35-fold rise in mean queue (still negligible in absolute terms, at well under one ten-thousandth of a casualty) indicates this margin is not unlimited, and a Vietnam/Okinawa-intensity re-run of the dedicated fleet-size sweep would be needed to establish exactly where it is exhausted.

DOW/WIA rate rises from 0.45% to 0.92%, a factor of roughly 2, and the confidence intervals do not overlap. The direction is the defensible finding rather than the factor itself, for the reasons given under the table: the Falklands-load figure moves across control seeds by about as much as its own interval spans, which is enough to shift the ratio between measurements without disturbing the conclusion, since the surge figure rests on roughly nine times as many deaths and is correspondingly better resolved. The mechanism is that at Okinawa intensity the deaths are driven by treatment queues deep enough that removing an intensive care episode and a theatre episode from half the operated population, which is what the surgical pathway split does, does not relieve them.

---

## The R2B Pre-Open Hold Window

<small>[Return to Top](#contents)</small>

R2B fields one surgical section per team on a 12-hour shift against a theatre available around the clock, so for half of each day the theatre stands ready with nobody rostered to operate in it. `r2b.surgery.pre_open_window_min` sets how long a casualty who arrives in that half may be held forward for the section, rather than being diverted to R2E as every off-shift arrival previously was (see the README's [R2B Trajectory](../README.md#r2b-trajectory)). The window ships at 60 minutes and has no source, so what it does is a question for measurement rather than for argument.

A single run cannot answer it. Turning the window on shifts simmer's single global random stream, so a zero-window run and a 60-minute run at the same seed are different realisations rather than a controlled comparison, and at seed 42 the forward surgery count moves the opposite way to the population mean. The table below is therefore 50 replications at each setting, run at control seed 42 under the shipped default configuration. Every figure is a per-replication mean with a 95% confidence interval.

| Measure | Window 0 (instant diversion) | Window 60 min (shipped) |
| --- | --- | --- |
| Casualties held forward | 0 | 5.64 [5.03, 6.25] |
| R2B surgeries | 50.78 [49.20, 52.36] | 56.42 [54.53, 58.31] |
| Bypassed, section off shift | 65.66 [63.99, 67.33] | 62.02 [60.00, 64.04] |
| Bypassed, theatre busy or queued | 7.56 [6.75, 8.37] | 7.10 [6.18, 8.02] |
| R2B OT utilisation (24-hour room) | 7.4% [7.2%, 7.6%] | 8.3% [8.0%, 8.6%] |
| R2B surgical section utilisation | 6.9% [6.7%, 7.1%] | 7.6% [7.4%, 7.9%] |
| R2E first surgeries | 99.88 [97.82, 101.94] | 98.90 [96.99, 100.81] |
| R2E theatre entry deferred (ICU full) | 12.40 [11.45, 13.35] | 11.20 [10.18, 12.22] |
| Died of wounds per run | 1.10 [0.79, 1.41] | 0.78 [0.57, 0.99] |
| Treated-cohort died-of-wounds rate | 0.63% [0.45%, 0.81%] | 0.44% [0.32%, 0.55%] |
| Total casualties | 381.18 [380.79, 381.57] | 381.22 [380.89, 381.55] |

The window does what it was added to do, and the accounting closes. Roughly 5.6 casualties are held forward per run, forward surgeries rise by 5.6, and the off-shift bypass count falls by 3.6; the intervals on the first two overlap almost exactly, which is what a mechanism that converts a diversion into a forward operation and nothing else should produce. Forward theatre utilisation rises by about a fifth in relative terms, from 7.4% to 8.3% of room time, and section utilisation with it.

The concern the mechanism raises is that a held casualty occupies the single forward theatre for the whole hold and then for the operation, so each hold might displace the cases arriving behind it. The measurement says it does not, at this window: the theatre-busy bypass count is 7.56 against 7.10, an interval overlap wide enough that no displacement is detectable. What bounds the effect is the same thing that bounds the hold, since a 60-minute window reaches only the last hour of a 720-minute closed period and can hold at most one casualty per team at a time.

The rearward effect is small but consistent in direction. R2E performs about one fewer first operation per run and defers theatre entry about one fewer time, both movements with intervals that overlap zero, which is what shifting five or six operations forward out of about a hundred should look like against replication-to-replication variation. Mortality falls, from 1.10 deaths per run to 0.78, and the treated-cohort rate with it, from 0.63% to 0.44%. Both intervals sit at or below the approximately 0.46% Ajax Bay bound the project's one-sided calibration check applies (see the README's Further Development entry L22), so neither ceiling is adjusted; the movement is in the direction the mechanism predicts, earlier surgery for the casualties it reaches, but a response averaging about one death per replication is not resolved to a factor by 50 replications and the direction is the defensible reading rather than the size.

Two limits apply to this comparison. It was run at the shipped default configuration only, so it says nothing about how the window behaves under surge, where the forward theatre is contended and displacement would be likelier to bite; the comparative scenario figures above were not re-measured for it. And 60 minutes is one point on a range the screening bounds run from zero to six hours, so this establishes that the shipped value pays for itself, not that it is the value that pays best.

---

## Conclusion

<small>[Return to Top](#contents)</small>

The system's resilience to surge is directly quantified rather than inferred. This multi-run comparison confirms that neither R2B nor R2E can absorb Okinawa-intensity casualty rates without fundamental redesign: R2E OT mean queue rises approximately 430-fold and R2E Hold mean queue approximately 9-fold relative to the Falklands-modified baseline, R2B Hold queue rises approximately 10-fold, and DOW/WIA rate roughly doubles — all while R2B OT queue remains at zero only because the existing bypass routing shunts all surgical overflow onto an already-saturated R2E rather than R2B absorbing any of the surge itself. Effective LSCO medical support at Okinawa intensity would require scalable holding capacity at forward echelons, a deeper surgical roster at R2E, and dynamic load-balancing between R2B and R2E — capabilities the current static establishment does not provide.

These figures were regenerated at 50 replications per scenario after the lognormal casualty generator's per-minute rate cap was made relative to each stream's own mean, and they supersede the comparison made under the previous fixed absolute cap. Both profiles draw at least one lognormal stream, so both moved: `moderate_intensity` draws all six that way, and `high_intensity` overrides only its WIA and KIA streams to exponential and leaves DNBI lognormal. The movement is a real change in what the model generates, not sampling variation, and it is not uniform across streams, the retired cap having sat at a different multiple of each stream's mean (see the README's [Casualty Generation](../README.md#casualty-generation)). Under `moderate_intensity` that raises WIA from 148.8 to 151.1 per run and lowers total casualties from 385.7 to 381.1, the killed-in-action streams falling furthest. The `high_intensity` totals move much less, its two overridden streams being unaffected. The run was made in an unpinned R 4.3.3 sandbox rather than the project's pinned Dev Container, under the same caveat as the other figures refreshed alongside it (see `CLAUDE.md`'s Key Parameters provenance caveats). `images/scenario_comparison.png` was regenerated alongside the tables by the command given above. A comparable Vietnam-intensity comparison remains unavailable pending a genuine FORECAS-sourced Vietnam combat-troop WIA/KIA table.

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Blood, CG; Zouris, JM; Rotblatt, D; (1998) *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

<!-- REFERENCES END -->
