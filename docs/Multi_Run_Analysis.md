# Battlefield Casualty Handling — Multi-Run Comparative Analysis

## Abstract

<small>[Return to Top](#contents)</small>

This document presents a multi-run (n≥30 replications, 95% confidence intervals) comparative analysis of the Battlefield Casualty Handling discrete event simulation under two named casualty-rate scenario profiles: `moderate_intensity` (a Falklands 1982-modified baseline, the same casualty rate underlying the illustrative single-run analysis in `docs/Single_Run_Analysis.md`) and `high_intensity` (an Okinawa exemplar, calibrated from FORECAS Tables A.7/A.9 [[1]](#References)). Where the single-run document establishes what the modelled deployed health system does under one seed and one casualty-rate assumption, this document establishes whether those findings are an artefact of that single draw and how the same system responds when casualty production is scaled to a materially higher intensity, using the project's multi-run replication framework, which executes independent stochastic replications of the discrete event simulation and aggregates outcomes as mean, 95% confidence interval, and p10–p90 range across runs.

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
- [Conclusion](#conclusion)
- [References](#references)
<!-- TOC END -->

---

## Methodology

<small>[Return to Top](#contents)</small>

This analysis uses the project's comparative scenario runner (`run_scenario()` / `compare_scenarios()`, `R/scenario_runner.R`), which executes the multi-replication framework (`run_replications()`, `R/replication.R`) under a named scenario profile and aggregates queue and mortality KPIs across replications in the same mean (p10–p90), 95% CI format used throughout this project. A scenario profile is a named set of overrides applied on top of the shipped default `env_data.json` parameters; each profile's casualty-generation parameters (arrival-rate distributions, priority-severity mix, and died-of-wounds calibration) are calibrated against a named historical exemplar. The extent of that validation differs by profile and by parameter: the `moderate_intensity` died-of-wounds ceilings are validated against a treated-cohort mortality rate reported for casualties reaching forward surgical care, not against a whole-of-wounded rate (see the README's [Parameter Calibration](../README.md#parameter-calibration) and Further Development entry L22), while `high_intensity` has only its generation rates and distribution family sourced, as the following paragraph sets out.

Both scenarios use the same health system establishment — the simulation's shipped default configuration, a representative combat brigade served by three Role 1 (R1) treatment teams, two Role 2 Basic (R2B) facilities, and one Role 2 Enhanced Heavy (R2E Heavy) hospital — since element, bed, and transport fleet counts are structural configuration, not scenario-eligible parameters. Only the casualty-generation parameters differ between `moderate_intensity` and `high_intensity`.

Two scenarios are compared — `moderate_intensity` (Falklands 1982 exemplar) and `high_intensity` (Okinawa exemplar, an explicitly unvalidated demonstration skeleton) — both defined in `env_data.json` (Issue #54). A third, Vietnam-calibrated profile is not included: FORECAS's Appendix A has no standalone Vietnam combat-troop WIA/KIA distribution table (Table A.5 is Vietnam DNBI only) [[1]](#References), so no genuinely FORECAS-sourced Vietnam parameters exist to build one from.

**Run configuration:** 50 replications × 30 days (seed 42) were run for each scenario via:

```
Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity --iterations 50 --days 30 --seed 42
```

`moderate_intensity` total casualties (385.7, 95% CI [385.3, 386.2]) match the documented seed-42 single-run baseline (386, `CLAUDE.md` Key Parameters) within 0.1%, confirming that the comparative runner reproduces the single-run baseline under the Falklands profile it is scenario-explicit about, consistent with the scenario mechanism's no-op guarantee for `default`.

---

## Comparative Scenario Analysis

<small>[Return to Top](#contents)</small>

### Casualty and Mortality Totals

| Metric | `moderate_intensity` (Falklands) | `high_intensity` (Okinawa) | Ratio |
|---|---|---|---|
| Total casualties/run | 385.7 (p10–p90: 384.0–388.0) | 969.4 (p10–p90: 967.0–972.0) | 2.51× |
| WIA/run | 148.8 (148.0–150.0) | 656.7 (655.0–659.0) | 4.41× |
| DOW/run | 0.72 (0–2.0) | 5.04 (3.0–7.0) | 7.00× |
| DOW/WIA rate | 0.48% (95% CI [0.30%, 0.67%]) | 0.77% (95% CI [0.68%, 0.85%]) | 1.59× |

The two died-of-wounds rows are each a single 50-replication measurement of a response averaging well under one death per replication at Falklands load, and 50 replications resolve a died-of-wounds rate only to roughly ±0.12 percentage points (see the README's [Further Development](../README.md#further-development) entry L22 for where that figure comes from). The intervals shown are correctly specified, the replications behind them being independent of one another (see the README's [Multi-run Replication Framework](../README.md#multi-run-replication-framework)); they are simply wide, and the `moderate_intensity` figure moves between control seeds by about as much as the interval spans. The figure to quote for that profile is therefore the 150-replication pooled one in the README's [Scenario Profiles](../README.md#scenario-profiles), 0.43% (95% CI [0.34%, 0.51%]), which this table's single measurement is one draw around, and the ratio between the two profiles should be read as approximate for the same reason. The queue rows below are better resolved at the same replication count: they are time-weighted occupancy measures with far more events behind each replication.

### Resource Queue Comparison (mean of per-resource mean queue, by group)

| Resource group | `moderate_intensity` mean queue | `high_intensity` mean queue | Ratio |
|---|---|---|---|
| R2B OT | 0.000 | 0.000 | — |
| R2B Hold | 0.320 | 3.105 | 9.70× |
| R2E OT | 0.052 | 38.81 | 749.4× |
| R2E ICU | 0.055 | 0.577 | 10.53× |
| R2E Hold | 0.172 | 2.429 | 14.15× |
| Transport (PMV Ambulance / HX240M) | 0.0000071 | 0.000103 | 14.5× (negligible in both) |

![Comparative Scenario Analysis](../images/scenario_comparison.png)

### Interpretation

The comparison exposes a structural fragility that the single-run baseline could not surface on its own, and it locates that fragility in the operating theatres. Mean R2E OT queue rises from 0.052 casualties at Falklands-equivalent load to 38.8 under `high_intensity`, a factor of roughly 750, which is by a wide margin the largest movement anywhere in the model. Both ends of that ratio fall once roughly half of operated casualties take a single-stage procedure rather than two staged ones, but the Falklands-load figure falls much the further, which is why the ratio widens rather than narrowing. The mechanism is the surgical roster: a casualty seizes a theatre before it seizes one of the three surgical sections that staff them, so a room reads as queued while its occupant waits for staff, and at Okinawa-intensity arrival rates that wait dominates. R2E ICU rises from 0.055 to 0.58, a factor of 11, and is now the flattest of the three R2E groups under surge: with only the damage control cohort taking a stabilisation episode, intensive care carries one episode for half the surgical population and two for the other half rather than two for everyone. R2E holding beds rise from 0.17 to 2.43, a factor of 14, and absorb what intensive care does not, since the holding bed is where a casualty goes when no intensive care bed is free and where those awaiting strategic evacuation stage.

R2B OT queue remains at 0 in both scenarios — not because R2B absorbs any of the surge, but because the existing OT-bypass routing diverts casualties requiring surgery to R2E whenever R2B is off-shift, busy, or queued rather than allowing them to wait; under `high_intensity`, this shunts the entire surge onto an R2E that has limited further capacity to absorb it. R2B Hold bed queue — already identified as a Falklands-rate bottleneck (see the single-run analysis's [R2B Hold Bed Saturation](Single_Run_Analysis.md#r2b-hold-bed-saturation--stream-decomposition-and-intervention-analysis) finding) — increases roughly 9.7-fold (0.32 to 3.11), driven by the proportional increase in non-surgical WIA volume rather than any change to DNBI generation, since DNBI generation rate is not one of the parameters a scenario profile overrides.

Transport remains the one echelon with genuine headroom: mean queue stays a small fraction of a casualty even at 2.5× total casualty volume, consistent with the single-run analysis's [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin) finding that the PMV Ambulance/HX240M pool is not the binding constraint at the Falklands-derived rate — though the roughly 14-fold rise in mean queue (still negligible in absolute terms, at around one ten-thousandth of a casualty) indicates this margin is not unlimited, and a Vietnam/Okinawa-intensity re-run of the dedicated fleet-size sweep would be needed to establish exactly where it is exhausted.

DOW/WIA rate rises from 0.48% to 0.77%, a factor of roughly 1.6, and the confidence intervals do not overlap. The direction is the defensible finding rather than the factor itself, for the reasons given under the table: the Falklands-load figure moves across control seeds by about as much as its own interval spans, which is enough to shift the ratio between measurements without disturbing the conclusion, since the surge figure rests on roughly seven times as many deaths and is correspondingly better resolved. The mechanism is that at Okinawa intensity the deaths are driven by treatment queues deep enough that removing an intensive care episode and a theatre episode from half the operated population, which is what the surgical pathway split does, does not relieve them.

---

## Conclusion

<small>[Return to Top](#contents)</small>

The system's resilience to surge is directly quantified rather than inferred. This multi-run comparison confirms that neither R2B nor R2E can absorb Okinawa-intensity casualty rates without fundamental redesign: R2E OT mean queue rises approximately 750-fold and R2E Hold mean queue approximately 14-fold relative to the Falklands-modified baseline, R2B Hold queue rises approximately 10-fold, and DOW/WIA rate rises by roughly half again — all while R2B OT queue remains at zero only because the existing bypass routing shunts all surgical overflow onto an already-saturated R2E rather than R2B absorbing any of the surge itself. Effective LSCO medical support at Okinawa intensity would require scalable holding capacity at forward echelons, a deeper surgical roster at R2E, and dynamic load-balancing between R2B and R2E — capabilities the current static establishment does not provide.

These figures were regenerated at 50 replications per scenario after replications were made independent of one another, and they supersede the comparison made when replications were antithetically paired. No model behaviour changed with that correction, so the movement between the two sets of figures is sampling variation between two sets of seed draws rather than a change in what the model does; the ratios most affected are the ones resting on the fewest events per replication, the died-of-wounds rows and the holding-bed queues, which is the same reason those rows carry the caveats given above. What the correction does change is that these intervals are now correctly specified, where previously they divided by a replication count that the pairing did not entitle them to. The run was made in an unpinned R 4.3.3 sandbox rather than the project's pinned Dev Container, under the same caveat as the other figures refreshed alongside it (see `CLAUDE.md`'s Key Parameters provenance caveats). `images/scenario_comparison.png` was regenerated alongside the tables, and has since been regenerated again by the command given above with no locale set at all, reproducing the figures in the tables and the image itself; the plotting stage no longer depends on the session's locale. A comparable Vietnam-intensity comparison remains unavailable pending a genuine FORECAS-sourced Vietnam combat-troop WIA/KIA table.

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Blood, CG; Zouris, JM; Rotblatt, D; (1998) *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

<!-- REFERENCES END -->
