# Battlefield Casualty Handling — Multi-Run Comparative Analysis

## Abstract

<small>[Return to Top](#contents)</small>

This document presents a multi-run (n≥30 replications, 95% confidence intervals) comparative analysis of the Battlefield Casualty Handling discrete event simulation under two named casualty-rate scenario profiles: `moderate_intensity` (a Falklands 1982-modified baseline, the same casualty rate underlying the illustrative single-run analysis in `docs/Single_Run_Analysis.md`) and `high_intensity` (an Okinawa exemplar, calibrated from FORECAS Tables A.7/A.9 [[1]](#References)). Where the single-run document establishes what the modelled deployed health system does under one seed and one casualty-rate assumption, this document establishes whether those findings are an artefact of that single draw and how the same system responds when casualty production is scaled to a materially higher intensity, using the statistical replication framework described in the [README](../README.md#simulation-design).

Across 30 replications of each scenario (30 simulated days, seed 42), the comparison confirms that the current establishment's adequacy conclusion does not extrapolate from Falklands to Okinawa intensity: mean total casualties per run rise 2.51-fold, the R2E Operating Theatre mean queue rises approximately 185-fold, the R2E Intensive Care Unit mean queue rises approximately 2.9-fold, and the R2B Holding bed mean queue rises approximately 11.7-fold, while R2B OT queue remains at zero in both scenarios — not because R2B absorbs any of the surge, but because the model's existing bypass routing diverts all surgical overflow to an already-saturated R2E. Died-of-wounds rate as a proportion of WIA rises from 0.67% to 0.89%. Transport (PMV Ambulance / HX240M) remains the one echelon with genuine headroom at both intensities.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Methodology](#methodology)
- [Comparative Scenario Analysis](#comparative-scenario-analysis)
  - [Casualty and Mortality Totals](#casualty-and-mortality-totals)
  - [Resource Queue Comparison (mean of per-resource mean queue, by group)](#resource-queue-comparison-mean-of-perresource-mean-queue-by-group)
  - [Interpretation](#interpretation)
- [Conclusion](#conclusion)
- [References](#references)
<!-- TOC END -->

---

## Methodology

<small>[Return to Top](#contents)</small>

This analysis uses the project's comparative scenario runner (`run_scenario()` / `compare_scenarios()`, `R/scenario_runner.R`), which executes the multi-replication framework (`run_replications()`, `R/replication.R` — see the [README's Multi-run Replication Framework](../README.md#multirun-replication-framework)) under a named scenario profile and aggregates queue and mortality KPIs across replications in the same mean (p10–p90), 95% CI format used throughout this project. Full detail on the scenario-profile overlay mechanism, the parameters each profile does and does not override, and the calibration validation confirming each profile reproduces its intended historical target, is documented in the [README's Scenario Profiles](../README.md#scenario-profiles) section and is not repeated here.

Both scenarios use the same health system establishment — the simulation's shipped default configuration, a representative combat brigade served by three Role 1 (R1) treatment teams, two Role 2 Basic (R2B) facilities, and one Role 2 Enhanced Heavy (R2E Heavy) hospital (see the [README's Scenario Context](../README.md#scenario-context)) — since element, bed, and transport fleet counts are structural configuration, not scenario-eligible parameters (see [Scenario Profiles — Parameter classification](../README.md#parameter-classification)). Only the casualty-generation parameters differ between `moderate_intensity` and `high_intensity`.

Two scenarios are compared — `moderate_intensity` (Falklands 1982 exemplar) and `high_intensity` (Okinawa exemplar, an explicitly unvalidated demonstration skeleton per the README's Scenario Profiles documentation) — both defined in `env_data.json` (Issue #54). A third, Vietnam-calibrated profile is not included: FORECAS's Appendix A has no standalone Vietnam combat-troop WIA/KIA distribution table (Table A.5 is Vietnam DNBI only) [[1]](#References), so no genuinely FORECAS-sourced Vietnam parameters exist to build one from (see the README's Limitations, L12).

**Run configuration:** 30 replications × 30 days (seed 42) were run for each scenario via:

```
Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity --iterations 30 --days 30 --seed 42
```

`moderate_intensity` total casualties (386.0, 95% CI [385.6, 386.4]) match the documented seed-42 single-run baseline (386, `CLAUDE.md` Key Parameters, post-Issue-23) within 0.01% — confirming the comparative runner reproduces the existing single-run baseline under the Falklands profile it is scenario-explicit about, consistent with the scenario mechanism's no-op guarantee for `default`.

---

## Comparative Scenario Analysis

<small>[Return to Top](#contents)</small>

### Casualty and Mortality Totals

| Metric | `moderate_intensity` (Falklands) | `high_intensity` (Okinawa) | Ratio |
|---|---|---|---|
| Total casualties/run | 386.0 (p10–p90: 384.9–387.0) | 969.4 (p10–p90: 966.0–972.0) | 2.51× |
| WIA/run | 148.9 (148.0–150.0) | 656.6 (654.9–659.0) | 4.41× |
| DOW/run | 1.0 (0–2) | 5.8 (2.9–8.1) | 5.83× |
| DOW/WIA rate | 0.671% (95% CI [0.426%, 0.916%]) | 0.888% (95% CI [0.755%, 1.022%]) | 1.32× |

### Resource Queue Comparison (mean of per-resource mean queue, by group)

| Resource group | `moderate_intensity` mean queue | `high_intensity` mean queue | Ratio |
|---|---|---|---|
| R2B OT | 0.000 | 0.000 | — |
| R2B Hold | 0.272 | 3.174 | 11.68× |
| R2E OT | 0.0079 | 1.458 | 184.6× |
| R2E ICU | 8.818 | 25.62 | 2.91× |
| Transport (PMV Ambulance / HX240M) | 0.0000179 | 0.000139 | 7.78× (negligible in both) |

![Comparative Scenario Analysis](../images/scenario_comparison.png)

### Interpretation

The comparison exposes a structural fragility that the single-run baseline could not surface on its own. Under `high_intensity` casualty rates, R2E OT and ICU — already the binding constraints at Falklands-equivalent load (see the single-run analysis's [R2E Heavy Handling](Single_Run_Analysis.md#r2e-heavy-handling) finding) — become severely saturated: mean R2E OT queue rises from 0.008 to 1.46 casualties (≈185-fold), and R2E ICU queue rises from 8.8 to 25.6 (≈2.9-fold) — the ICU is already under sustained load at the Falklands-modified rate under the current codebase (see the [Role 4 (National Support Base) Demand Modelling](../README.md#role-4-national-support-base-demand-modelling) mechanism, whose critical-pool AME wait competes for the same ICU bed pool — README Limitation L17), and Okinawa-intensity casualty production pushes that same finite pool further into sustained saturation rather than introducing a new bottleneck from a clear baseline.

R2B OT queue remains at 0 in both scenarios — not because R2B absorbs any of the surge, but because the existing OT-bypass routing (see the [R2B Trajectory](../README.md#r2b-trajectory)) diverts surgical candidates to R2E whenever R2B is off-shift, busy, or queued rather than allowing them to wait; under `high_intensity`, this shunts the entire surge onto an R2E that has limited further capacity to absorb it. R2B Hold bed queue — already identified as a Falklands-rate bottleneck (see the single-run analysis's [R2B Hold Bed Saturation](Single_Run_Analysis.md#r2b-hold-bed-saturation-stream-decomposition-and-intervention-analysis) finding) — increases roughly 11.7-fold (0.27 to 3.17), driven by the proportional increase in non-surgical WIA volume rather than any change to DNBI generation (DNBI is not scenario-eligible; see the [README's Scenario Profiles — Parameter classification](../README.md#parameter-classification)).

Transport remains the one echelon with genuine headroom: mean queue stays a small fraction of a casualty even at 2.5× total casualty volume, consistent with the single-run analysis's [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin) finding that the PMV Ambulance/HX240M pool is not the binding constraint at the Falklands-derived rate — though the roughly 7.8-fold rise in mean queue (still negligible in absolute terms at this casualty rate) indicates this margin is not unlimited, and a Vietnam/Okinawa-intensity re-run of the dedicated fleet-size sweep (see the README's Further Development) would be needed to establish exactly where it is exhausted.

DOW/WIA rate rises from 0.67% to 0.89% — a smaller proportional increase than the queue-length findings above, consistent with DOW remaining a comparatively rare event even at Okinawa-intensity casualty production over a 30-day window; a longer run or a larger replication count would sharpen this estimate further.

---

## Conclusion

<small>[Return to Top](#contents)</small>

The system's resilience to surge is now directly quantified rather than inferred. This multi-run comparison confirms that neither R2B nor R2E can absorb Okinawa-intensity casualty rates without fundamental redesign: R2E OT mean queue rises approximately 185-fold and R2E ICU mean queue approximately 2.9-fold relative to the Falklands-modified baseline, R2B Hold queue rises approximately 11.7-fold, and DOW/WIA rate rises from 0.67% to 0.89% — all while R2B OT queue remains at zero only because the existing bypass routing shunts all surgical overflow onto an already-saturated R2E rather than R2B absorbing any of the surge itself. Effective LSCO medical support at Okinawa intensity would require scalable holding capacity at forward echelons, adaptable evacuation architecture, and dynamic load-balancing between R2B and R2E — capabilities the current static establishment does not provide.

These figures supersede the earlier 10-replication comparison previously reported in this project's single combined README: the current codebase includes several RNG-stream-shifting merges made since that comparison was last produced (see `CLAUDE.md`'s Key Parameters provenance caveats for Issues #18, #23, and #76), and the queue magnitudes reported here reflect the current model, not the earlier one. A comparable Vietnam-intensity comparison remains unavailable pending a genuine FORECAS-sourced Vietnam combat-troop WIA/KIA table (see the README's Limitations, L12).

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Blood, CG; Zouris, JM; Rotblatt, D; (1998) *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

<!-- REFERENCES END -->
