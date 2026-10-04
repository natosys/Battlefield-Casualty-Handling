# Measured Results of a Replicated Simulation of the Land-Based Trauma System

## Abstract

<small>[Return to Top](#contents)</small>

**Background**

A discrete event simulation of an Australian brigade-sized force and its land-based trauma system produces measurements that planners can read without the model's own interpretation attached. This document reports those measurements and nothing else: every replicated experiment, the verification of one seed-42 campaign, and the sensitivity screens.

**Methods**

Each replicated experiment ran 30 independent replications of a 360-day campaign under the shipped configuration or a named variation, and measured queues and occupancy as whole-pool totals over each campaign's closing 90 days. The sensitivity screens ran at 30 days. Intervals are 95% Student $t$ intervals about the mean of per-replication values unless a section states otherwise. The designs are in [Methods](Methods.md).

**Results**

At moderate casualty intensity a campaign produced <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|mean -->5,322.5<!-- /GEN --> casualties and at high intensity <!-- GEN cell:scenario_totals|Total casualties/run|High intensity|mean -->12,479.7<!-- /GEN -->. Over the closing 90 days the mean R2E operating theatre queue was <!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|mean -->2.324<!-- /GEN --> casualties at moderate intensity and <!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|mean -->556.126<!-- /GEN --> at high. At high intensity that queue stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|High: queue empty|full -->1%<!-- /GEN --> of the campaign and was still growing at day 360, while at moderate intensity it stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: queue empty|full -->68%<!-- /GEN -->. The treated-cohort died-of-wounds rate of the shipped default configuration was <!-- GEN cell:dow_calibration|Shipped default|Treated-cohort died-of-wounds rate|full -->0.544% [0.494%, 0.593%]<!-- /GEN --> at 360 days.

**Conclusion**

The measurements record where queues form at each intensity and which responses each tested lever moved. Their reading as planning options is in [Planning Implications](Planning_Implications.md).

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Scope and Conventions](#scope-and-conventions)
- [Comparative Scenario Analysis](#comparative-scenario-analysis)
- [Queue Behaviour over the Campaign](#queue-behaviour-over-the-campaign)
- [Sustained Operations](#sustained-operations)
- [Treated-Cohort Mortality](#treated-cohort-mortality)
- [Forward Holding and Evacuation Levers](#forward-holding-and-evacuation-levers)
  - [R2B Pre-Open Hold Window](#r2b-pre-open-hold-window)
  - [R2B Holding Capacity and Evacuation Threshold](#r2b-holding-capacity-and-evacuation-threshold)
  - [Transport Fleet Size](#transport-fleet-size)
  - [Forward Holding of Post-Operative Intensive Care](#forward-holding-of-post-operative-intensive-care)
  - [Post-Operative Intensive Care Gate](#post-operative-intensive-care-gate)
  - [Evacuation Policy](#evacuation-policy)
  - [R2E Holding Establishment](#r2e-holding-establishment)
  - [Forward Surgical Saturation Release](#forward-surgical-saturation-release)
- [National Support Base and Strategic Airlift](#national-support-base-and-strategic-airlift)
- [Mass Casualty Events](#mass-casualty-events)
- [Resolution of Paired Differences](#resolution-of-paired-differences)
- [Sensitivity Screens](#sensitivity-screens)
- [Annex A. Model Verification of One Seed-42 Campaign](#annex-a-model-verification-of-one-seed-42-campaign)
- [References](#references)
<!-- TOC END -->

---

## Scope and Conventions

<small>[Return to Top](#contents)</small>

This document states what the model measured. It makes no recommendation, ranks no option and draws no planning conclusion; each section gives the question the experiment asked, a protocol tag, the measurement and a neutral statement of what the measurement shows with its interval and resolution. The experimental designs, the interval constructions and the checks that defend them are in [Methods](Methods.md), and the reading of these results as planning options is in [Planning Implications](Planning_Implications.md).

A protocol tag has the form `[configuration · horizon · replications · measurement]`. A replication is one complete campaign from an empty system under its own random number stream. A pool's queue is the total number of casualties waiting across every bed of one type at one facility, and its occupancy is the time-weighted number of beds in use over the beds established, both measured over a campaign's closing 90 days unless a section states otherwise. Responses that accumulate over a campaign, such as returns to duty and deaths of wounds, are campaign totals. Ratios divide the high-intensity figure by the moderate-intensity one. Intervals are 95% Student $t$ intervals about the mean of per-replication values; a bound below zero for a quantity that cannot be negative is clamped at zero where a section says so, and an interval for a proportion of campaigns is an exact binomial interval.

Every table and every quoted figure is regenerated from the tracked evidence under `data/` by `scripts/render_results_tables.R`, and `scripts/check_results_tables.R` fails when the document disagrees with that evidence. A figure written in a sentence is a copy of a cell of the table beside it.

---

## Comparative Scenario Analysis

<small>[Return to Top](#contents)</small>

**Question.** How do casualty volume, mortality and the load on each resource pool differ between the moderate-intensity (Falklands 1982) and high-intensity (Okinawa 1945) casualty profiles, whose rates derive from the FORECAS projection study [[1]](#references), when the health system is identical? `[moderate_intensity and high_intensity · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN scenario_totals -->
| Metric | Moderate intensity | High intensity | Ratio |
|---|---|---|---|
| Total casualties/run | 5,322.5 [5,255.6, 5,389.3] (p10–p90: 5,108.4–5,515.7) | 12,479.7 [12,356.9, 12,602.4] (p10–p90: 12,115.7–12,915.6) | 2.34× |
| Wounded in action/run | 2,267.9 [2,202.1, 2,333.7] (p10–p90: 2,053.2–2,420.6) | 8,409.0 [8,290.1, 8,527.9] (p10–p90: 7,987.5–8,790.8) | 3.71× |
| Died of wounds/run | 11.77 [10.52, 13.02] (p10–p90: 7.0–16.0) | 303.73 [296.26, 311.21] (p10–p90: 280.8–329.3) | 25.8× |
| Died of wounds, as share of wounded | 0.52% [0.47%, 0.57%] | 3.61% [3.53%, 3.70%] | 6.97× |
<!-- /GEN -->

![Four panels of total casualties, wounded in action, deaths of wounds and deaths as a share of wounded at the two casualty intensities](../images/paper_casualty_totals.png)

Each panel plots the moderate and high casualty intensities as a point with a narrow 95% confidence interval bar and a wide band for the 10th-to-90th-percentile spread across campaigns. The bar shows how precisely the average is known and the band shows how much one campaign varies.

Moderate intensity produced <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|mean -->5,322.5<!-- /GEN --> casualties per run (95% interval <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|ci -->[5,255.6, 5,389.3]<!-- /GEN -->) and high intensity <!-- GEN cell:scenario_totals|Total casualties/run|High intensity|mean -->12,479.7<!-- /GEN --> (<!-- GEN cell:scenario_totals|Total casualties/run|High intensity|ci -->[12,356.9, 12,602.4]<!-- /GEN -->), a ratio of <!-- GEN cell:scenario_totals|Total casualties/run|Ratio|full -->2.34×<!-- /GEN -->. Wounded in action rose by <!-- GEN cell:scenario_totals|Wounded in action/run|Ratio|full -->3.71×<!-- /GEN -->, died of wounds per run by <!-- GEN cell:scenario_totals|Died of wounds/run|Ratio|full -->25.8×<!-- /GEN --> and died of wounds as a share of wounded by <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|Ratio|full -->6.97×<!-- /GEN -->, from <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|Moderate intensity|mean -->0.52%<!-- /GEN --> to <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|High intensity|mean -->3.61%<!-- /GEN -->.

<!-- GEN scenario_queue -->
| Resource group | Moderate intensity mean queue | High intensity mean queue | Ratio |
|---|---|---|---|
| R2B operating theatre | 0.000 [0.000, 0.000] | 0.000 [0.000, 0.000] | not applicable |
| R2B holding beds | 6.648 [5.981, 7.316] | 35.427 [33.946, 36.908] | 5.33× |
| R2E operating theatre | 2.324 [1.650, 2.999] | 556.126 [501.727, 610.525] | 239.26× |
| R2E intensive care | 1.522 [0.419, 2.624] | 1,474.941 [1,459.287, 1,490.596] | 969.24× |
| R2E holding beds | 0.756 [−0.262, 1.774] | 465.425 [445.357, 485.492] | 615.43× |
| Ambulance and truck fleets | 0.058 [−0.015, 0.131] | 0.219 [0.161, 0.277] | 3.78× |
<!-- /GEN -->

![Horizontal plot of queue growth factor on a logarithmic scale for five resource groups with a dashed reference line at the growth in casualty volume](../images/paper_queue_growth.png)

Queue growth factor from moderate to high intensity on a logarithmic scale, with a dashed reference line at the growth in casualty volume. Each point is sized by its absolute queue at high intensity.

![Four-panel bar chart of mean queue length by resource group at the two casualty intensities](../images/scenario_comparison.png)

Mean queue length by resource group at each casualty intensity. Each panel has its own vertical scale, and the error bars show the spread across campaigns rather than a confidence interval.

The R2B operating theatre queue was zero at both intensities. At moderate intensity the R2B holding beds carried a mean queue of <!-- GEN cell:scenario_queue|R2B holding beds|Moderate intensity mean queue|mean -->6.648<!-- /GEN --> casualties (<!-- GEN cell:scenario_queue|R2B holding beds|Moderate intensity mean queue|ci -->[5.981, 7.316]<!-- /GEN -->), against <!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|mean -->2.324<!-- /GEN --> at the R2E operating theatres (<!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|ci -->[1.650, 2.999]<!-- /GEN -->). At high intensity the R2E operating theatre queue was <!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|mean -->556.126<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|ci -->[501.727, 610.525]<!-- /GEN -->), the R2E intensive care queue <!-- GEN cell:scenario_queue|R2E intensive care|High intensity mean queue|mean -->1,474.941<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E intensive care|High intensity mean queue|ci -->[1,459.287, 1,490.596]<!-- /GEN -->) and the R2E holding queue <!-- GEN cell:scenario_queue|R2E holding beds|High intensity mean queue|mean -->465.425<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E holding beds|High intensity mean queue|ci -->[445.357, 485.492]<!-- /GEN -->); the ratios to moderate intensity were <!-- GEN cell:scenario_queue|R2E operating theatre|Ratio|full -->239.26×<!-- /GEN -->, <!-- GEN cell:scenario_queue|R2E intensive care|Ratio|full -->969.24×<!-- /GEN --> and <!-- GEN cell:scenario_queue|R2E holding beds|Ratio|full -->615.43×<!-- /GEN -->. The ambulance and truck fleets queued <!-- GEN cell:scenario_queue|Ambulance and truck fleets|Moderate intensity mean queue|mean -->0.058<!-- /GEN --> and <!-- GEN cell:scenario_queue|Ambulance and truck fleets|High intensity mean queue|mean -->0.219<!-- /GEN --> casualties at the two intensities. The high-intensity R2E figures are means over a window in which the queues were still growing, as the sections that follow record, so they describe the closing 90 days and not a level.

---

## Queue Behaviour over the Campaign

<small>[Return to Top](#contents)</small>

**Question.** Does a pool's queue clear between its busy spells, or does it stand for the whole campaign, and how does the share of post-operative care delivered in a holding bed change with time? `[moderate_intensity and high_intensity · 360 d · 30 replications · pool totals, unbinned series]`

<!-- GEN queue_clearance -->
| Resource pool | Moderate: queue empty | Moderate: longest run above zero (days) | High: queue empty | High: longest run above zero (days) |
|---|---|---|---|---|
| R2B holding beds | 15% | 53.3 (41.0–61.4) | 1% | 356.5 (273.3–358.1) |
| R2E operating theatres | 68% | 13.4 (11.1–15.5) | 1% | 354.9 (352.5–358.4) |
| R2E intensive care | 65% | 19.0 (12.0–25.8) | 1% | 355.8 (352.5–357.9) |
| R2E holding beds | 88% | 21.6 (10.6–37.2) | 1% | 355.8 (355.4–356.5) |
<!-- /GEN -->

![Four stacked panels of queue length against campaign day, one per resource pool, at the two casualty intensities](../images/queue_length_over_time.png)

Queue length against campaign day for each pool, as the median across replications with an interquartile band, at each casualty intensity. Each panel is annotated with the share of the campaign the queue stood empty.

The table reports, for each pool and intensity, the median across replications of the share of the campaign the pool's queue stood empty and of the longest unbroken spell it did not, with the interquartile range of the spell in parentheses. At high intensity every pool listed stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|High: queue empty|full -->1%<!-- /GEN --> of the campaign or less, and the median longest spell above zero in the R2E operating theatres was <!-- GEN cell:queue_clearance|R2E operating theatres|High: longest run above zero (days)|full -->354.9 (352.5–358.4)<!-- /GEN --> of the 360 days. At moderate intensity the same pool stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: queue empty|full -->68%<!-- /GEN --> of the campaign with a median longest spell of <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: longest run above zero (days)|full -->13.4 (11.1–15.5)<!-- /GEN --> days.

<!-- GEN degraded_care -->
| Intensity and stage | First day the median daily rate reaches 100% | First ten days | Last ten days | Whole campaign |
|---|---|---|---|---|
| Moderate intensity: Stabilisation | not reached | 41.5% | 48.3% | 48.6% |
| Moderate intensity: Post-definitive care | not reached | 58.4% | 65.9% | 66.7% |
| High intensity: Stabilisation | 8 | 64.4% | 100.0% | 99.1% |
| High intensity: Post-definitive care | 5 | 86.3% | 100.0% | 99.6% |
<!-- /GEN -->

![Two stacked panels of the share of casualties recovering in a holding bed against campaign day](../images/degraded_care_rate_over_time.png)

The share of casualties taking the holding-bed recovery against campaign day, stabilisation above and post-definitive care below, as a daily rate inside an interquartile band and as a running cumulative rate.

The degraded post-operative care rate is the share of casualties who took the holding-bed recovery in place of an intensive care bed at the point the decision was taken. At high intensity the median daily rate for post-definitive care first reached 100% on day <!-- GEN cell:degraded_care|High intensity: Post-definitive care|First day the median daily rate reaches 100%|full -->5<!-- /GEN --> and for stabilisation on day <!-- GEN cell:degraded_care|High intensity: Stabilisation|First day the median daily rate reaches 100%|full -->8<!-- /GEN -->; over the whole campaign <!-- GEN cell:degraded_care|High intensity: Post-definitive care|Whole campaign|full -->99.6%<!-- /GEN --> of post-definitive care and <!-- GEN cell:degraded_care|High intensity: Stabilisation|Whole campaign|full -->99.1%<!-- /GEN --> of stabilisation took the holding bed. At moderate intensity the whole-campaign rates were <!-- GEN cell:degraded_care|Moderate intensity: Post-definitive care|Whole campaign|full -->66.7%<!-- /GEN --> and <!-- GEN cell:degraded_care|Moderate intensity: Stabilisation|Whole campaign|full -->48.6%<!-- /GEN -->, and the median daily rate did not reach 100%.

---

## Sustained Operations

<small>[Return to Top](#contents)</small>

**Question.** Does each response settle to a level over a 360-day campaign, or does it keep moving? `[moderate_intensity and high_intensity · 360 d · 30 replications · twelve 30-day block means]`

Each response was reduced to twelve consecutive 30-day block means and classified from the trend over the late half of the campaign; the rule is in [Methods](Methods.md#classifying-stability).

<!-- GEN long_horizon_stability -->
| Response | Moderate intensity | High intensity |
|---|---|---|
| R2E operating theatre queue | converged at 2.21 | **drifting, +11.1%/block**, 40.5 to 605.9 |
| R2E holding bed queue | converged at 0.57 | **drifting, +9.4%/block**, 38.8 to 502.3 |
| R2E intensive care queue | converged at 1.20 | **drifting, +13.1%/block**, 30.1 to 1,630.8 |
| Strategic evacuation backlog | converged at 2.08 | **drifting, +12.6%/block**, 56.7 to 1,828.2 |
| R2B holding bed queue | converged at 6.45 | converged at 35.2 |
| Casualty arrivals per day | converged at 14.8 | converged at 34.7 |
| Deaths of wounds per day | converged at 0.03 | converged at 0.85 |
<!-- /GEN -->

Four responses at high intensity were classified as drifting: the R2E operating theatre queue (<!-- GEN cell:long_horizon_stability|R2E operating theatre queue|High intensity|full -->**drifting, +11.1%/block**, 40.5 to 605.9<!-- /GEN -->), the R2E holding bed queue (<!-- GEN cell:long_horizon_stability|R2E holding bed queue|High intensity|full -->**drifting, +9.4%/block**, 38.8 to 502.3<!-- /GEN -->), the R2E intensive care queue (<!-- GEN cell:long_horizon_stability|R2E intensive care queue|High intensity|full -->**drifting, +13.1%/block**, 30.1 to 1,630.8<!-- /GEN -->) and the strategic evacuation backlog (<!-- GEN cell:long_horizon_stability|Strategic evacuation backlog|High intensity|full -->**drifting, +12.6%/block**, 56.7 to 1,828.2<!-- /GEN -->). The R2B holding bed queue at high intensity was classified as converged, at <!-- GEN cell:long_horizon_stability|R2B holding bed queue|High intensity|full -->converged at 35.2<!-- /GEN -->. No response at moderate intensity was classified as drifting; the R2E operating theatre queue was <!-- GEN cell:long_horizon_stability|R2E operating theatre queue|Moderate intensity|full -->converged at 2.21<!-- /GEN --> and the R2B holding bed queue <!-- GEN cell:long_horizon_stability|R2B holding bed queue|Moderate intensity|full -->converged at 6.45<!-- /GEN -->.

---

## Treated-Cohort Mortality

<small>[Return to Top](#contents)</small>

**Question.** What fraction of casualties who reach an R2B or R2E facility die of wounds, measured against the historical anchor of the campaign each configuration models? `[default, moderate_intensity and high_intensity · 360 d · 3 measurements of 10 replications · pooled, Student t]`

<!-- GEN dow_calibration -->
| Configuration | Historical anchor | Treated-cohort died-of-wounds rate | Replications |
|---|---|---|---|
| Shipped default | at or below, 0.46% (Ajax Bay) | 0.544% [0.494%, 0.593%] | 30 |
| Moderate intensity | at or below, 0.46% (Ajax Bay) | 0.365% [0.322%, 0.407%] | 30 |
| High intensity | reported, 3.40% (Okinawa) | 3.780% [3.698%, 3.863%] | 30 |
<!-- /GEN -->

The anchors are the Ajax Bay Advanced Surgical Centre's three deaths among over 650 casualties who reached forward surgical care alive [[2]](#references), an upper bound for the two Falklands-calibrated configurations, and the rate of casualties who reached a hospital alive and died there on Okinawa [[3]](#references), which the high-intensity row records as <!-- GEN cell:dow_calibration|High intensity|Historical anchor|full -->reported, 3.40% (Okinawa)<!-- /GEN -->. The shipped default's pooled rate over 360-day campaigns was <!-- GEN cell:dow_calibration|Shipped default|Treated-cohort died-of-wounds rate|full -->0.544% [0.494%, 0.593%]<!-- /GEN -->, moderate intensity's <!-- GEN cell:dow_calibration|Moderate intensity|Treated-cohort died-of-wounds rate|full -->0.365% [0.322%, 0.407%]<!-- /GEN --> and high intensity's <!-- GEN cell:dow_calibration|High intensity|Treated-cohort died-of-wounds rate|full -->3.780% [3.698%, 3.863%]<!-- /GEN -->. The calibration check `scripts/check_dow_calibration.R` runs at 30 days, the horizon at which the ceilings were fitted.

---

## Forward Holding and Evacuation Levers

<small>[Return to Top](#contents)</small>

Each subsection varies one setting of the shipped configuration and reports the responses it moved. Experiments with several arms draw every arm from one control seed unless the subsection says otherwise, and the paired difference is reported where the arms are paired.

### R2B Pre-Open Hold Window

**Question.** What changes when a casualty needing surgery at R2B is held forward for a surgical section about to reopen rather than moved rearward at once? `[default, hold window 0 and 60 min · 360 d · 30 replications · campaign totals, paired difference]`

<!-- GEN hold_window -->
| Measure | Window 0 | Window 60 min | Difference |
| --- | --- | --- | --- |
| Casualties held at R2B | 0.00 | 80.03 | +80.03 [+76.45, +83.62] |
| R2B surgeries | 632.83 | 669.57 | +36.73 [+19.19, +54.28] |
| Diverted, team off shift | 1008.37 | 947.43 | −60.93 [−91.91, −29.96] |
| Diverted, theatre busy | 230.13 | 245.47 | +15.33 [−7.88, +38.55] |
| R2E first surgeries | 1573.83 | 1558.83 | −15.00 [−74.83, +44.83] |
| R2E theatre entry deferred | 214.33 | 212.70 | −1.63 [−17.85, +14.58] |
| Died of wounds per run | 15.83 | 15.40 | −0.43 [−2.75, +1.88] |
| Total casualties | 5386.03 | 5359.90 | −26.13 [−139.02, +86.75] |
<!-- /GEN -->

![Forest plot of the paired difference in each of eight measures with its confidence interval](../images/paper_hold_window_effects.png)

The paired difference per campaign for each of the eight measures with its 95% confidence interval against a vertical line at zero.

A 60-minute window held <!-- GEN cell:hold_window|Casualties held at R2B|Window 60 min|full -->80.03<!-- /GEN --> casualties per campaign at R2B against none with a zero window. R2B surgeries differed by <!-- GEN cell:hold_window|R2B surgeries|Difference|full -->+36.73 [+19.19, +54.28]<!-- /GEN --> and diversions for an off-shift section by <!-- GEN cell:hold_window|Diverted, team off shift|Difference|full -->−60.93 [−91.91, −29.96]<!-- /GEN -->. The differences in diversions for a busy theatre were <!-- GEN cell:hold_window|Diverted, theatre busy|Difference|full -->+15.33 [−7.88, +38.55]<!-- /GEN -->, in R2E first surgeries <!-- GEN cell:hold_window|R2E first surgeries|Difference|full -->−15.00 [−74.83, +44.83]<!-- /GEN -->, in R2E theatre entry deferrals <!-- GEN cell:hold_window|R2E theatre entry deferred|Difference|full -->−1.63 [−17.85, +14.58]<!-- /GEN -->, in died of wounds <!-- GEN cell:hold_window|Died of wounds per run|Difference|full -->−0.43 [−2.75, +1.88]<!-- /GEN --> and in total casualties <!-- GEN cell:hold_window|Total casualties|Difference|full -->−26.13 [−139.02, +86.75]<!-- /GEN -->.

### R2B Holding Capacity and Evacuation Threshold

**Question.** How do the R2B holding queue and the pools behind it respond to the R2B holding bed establishment and to the threshold at which convalescent casualties are evacuated early? `[default, 5 to 10 holding beds per unit crossed with a 0 to 7 day threshold · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN hold_threshold_beds -->
| R2B holding beds per unit | R2B hold mean queue | R2B hold utilisation | R2E hold mean queue | R2E ICU mean queue | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|
| 5 (shipped) | 4.90 [3.68, 6.12] | 66.8% [65.2, 68.3] | 0.320 [0.010, 0.630] | 1.578 [0.736, 2.420] | 1687 [1663, 1711] | 11.7 [10.2, 13.1] |
| 7 | 1.84 [1.18, 2.50] | 61.5% [58.7, 64.2] | 0.019 [0.000, 0.044] | 1.406 [0.497, 2.315] | 1659 [1631, 1687] | 10.9 [9.5, 12.3] |
| 10 | 0.26 [0.09, 0.42] | 44.9% [42.1, 47.7] | 0.002 [0.000, 0.006] | 0.904 [0.644, 1.163] | 1678 [1649, 1707] | 10.3 [8.8, 11.9] |
<!-- /GEN -->

With the threshold disabled, raising the establishment from five to ten holding beds per unit moved the R2B holding queue from <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2B hold mean queue|full -->4.90 [3.68, 6.12]<!-- /GEN --> to <!-- GEN cell:hold_threshold_beds|10|R2B hold mean queue|full -->0.26 [0.09, 0.42]<!-- /GEN --> and R2B holding utilisation from <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2B hold utilisation|full -->66.8% [65.2, 68.3]<!-- /GEN --> to <!-- GEN cell:hold_threshold_beds|10|R2B hold utilisation|full -->44.9% [42.1, 47.7]<!-- /GEN -->. The R2E holding queue was <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2E hold mean queue|full -->0.320 [0.010, 0.630]<!-- /GEN --> at five beds and <!-- GEN cell:hold_threshold_beds|10|R2E hold mean queue|full -->0.002 [0.000, 0.006]<!-- /GEN --> at ten, and the R2E intensive care queue <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2E ICU mean queue|full -->1.578 [0.736, 2.420]<!-- /GEN --> and <!-- GEN cell:hold_threshold_beds|10|R2E ICU mean queue|full -->0.904 [0.644, 1.163]<!-- /GEN --> Returns to duty were <!-- GEN cell:hold_threshold_beds|5 (shipped)|Returns to duty|full -->1687 [1663, 1711]<!-- /GEN --> at five beds and <!-- GEN cell:hold_threshold_beds|10|Returns to duty|full -->1678 [1649, 1707]<!-- /GEN --> at ten, and deaths of wounds <!-- GEN cell:hold_threshold_beds|5 (shipped)|Deaths of wounds|full -->11.7 [10.2, 13.1]<!-- /GEN --> and <!-- GEN cell:hold_threshold_beds|10|Deaths of wounds|full -->10.3 [8.8, 11.9]<!-- /GEN -->.

<!-- GEN hold_threshold_threshold -->
| Evacuation threshold | R2B hold mean queue | R2B hold utilisation | R2E hold mean queue | R2E ICU mean queue | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|
| Disabled (shipped) | 4.90 [3.68, 6.12] | 66.8% [65.2, 68.3] | 0.320 [0.010, 0.630] | 1.578 [0.736, 2.420] | 1687 [1663, 1711] | 11.7 [10.2, 13.1] |
| 1 day | 0.07 [0.01, 0.12] | 7.3% [6.7, 7.8] | 2.217 [1.370, 3.064] | 1.625 [0.773, 2.477] | 1665 [1630, 1700] | 10.5 [9.5, 11.6] |
| 3 days | 0.13 [0.02, 0.24] | 19.8% [18.8, 20.8] | 1.108 [0.626, 1.589] | 1.357 [0.881, 1.834] | 1673 [1644, 1702] | 11.2 [9.8, 12.6] |
| 5 days (mode) | 0.17 [0.10, 0.25] | 33.0% [31.4, 34.7] | 0.837 [0.275, 1.399] | 0.868 [0.622, 1.113] | 1649 [1624, 1673] | 10.0 [8.7, 11.3] |
| 7 days | 0.47 [0.29, 0.66] | 41.6% [39.7, 43.5] | 0.388 [0.117, 0.659] | 1.306 [0.354, 2.258] | 1683 [1655, 1712] | 11.4 [10.0, 12.8] |
<!-- /GEN -->

![Eight panels of R2B and R2E queue and utilisation, returns to duty and died of wounds against the evacuation threshold](../images/r2b_hold_threshold_sweep.png)

R2B and R2E queue and utilisation, returns to duty and died of wounds against the evacuation threshold in days, one line per swept bed count, with a 95% confidence ribbon.

With five beds per unit, a one-day threshold moved the R2B holding queue to <!-- GEN cell:hold_threshold_threshold|1 day|R2B hold mean queue|full -->0.07 [0.01, 0.12]<!-- /GEN --> and the R2E holding queue to <!-- GEN cell:hold_threshold_threshold|1 day|R2E hold mean queue|full -->2.217 [1.370, 3.064]<!-- /GEN -->, and the R2E intensive care queue to <!-- GEN cell:hold_threshold_threshold|1 day|R2E ICU mean queue|full -->1.625 [0.773, 2.477]<!-- /GEN -->. A three-day threshold gave <!-- GEN cell:hold_threshold_threshold|3 days|R2B hold mean queue|full -->0.13 [0.02, 0.24]<!-- /GEN -->, <!-- GEN cell:hold_threshold_threshold|3 days|R2E hold mean queue|full -->1.108 [0.626, 1.589]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|3 days|R2E ICU mean queue|full -->1.357 [0.881, 1.834]<!-- /GEN --> on the same three responses. At five and seven days the R2B holding queue was <!-- GEN cell:hold_threshold_threshold|5 days (mode)|R2B hold mean queue|full -->0.17 [0.10, 0.25]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|7 days|R2B hold mean queue|full -->0.47 [0.29, 0.66]<!-- /GEN -->. Returns to duty were <!-- GEN cell:hold_threshold_threshold|Disabled (shipped)|Returns to duty|full -->1687 [1663, 1711]<!-- /GEN --> with the threshold disabled and <!-- GEN cell:hold_threshold_threshold|1 day|Returns to duty|full -->1665 [1630, 1700]<!-- /GEN --> at one day, and deaths of wounds <!-- GEN cell:hold_threshold_threshold|Disabled (shipped)|Deaths of wounds|full -->11.7 [10.2, 13.1]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|1 day|Deaths of wounds|full -->10.5 [9.5, 11.6]<!-- /GEN -->. The full grid, including returns to duty and deaths of wounds at every point, is in `data/sweeps/r2b_hold_threshold_sweep.csv`.

### Transport Fleet Size

**Question.** At what fleet size does the transport queue form, at each casualty intensity? `[default and high_intensity · 360 d · 30 replications · pool totals, closing 90 d]`

The first table is the shipped moderate-intensity configuration and the second the high-intensity profile.

<!-- GEN transport -->
| Fleet size | Ambulance mean queue | Truck mean queue |
|---|---|---|
| 1 | 1.4397 [0.2895, 2.5898] | 0.4053 [0.0000, 1.0906] |
| 2 | 0.0866 [0.0398, 0.1333] | 0.0099 [0.0005, 0.0193] |
| 3 (current ambulance) | 0.0171 [0.0057, 0.0286] | 0.0007 [0.0000, 0.0015] |
| 4 (current truck) | 0.0102 [0.0000, 0.0212] | 0.0035 [0.0000, 0.0103] |
| 5 | 0.0513 [0.0000, 0.1272] | not swept |
<!-- /GEN -->

![Four panels of mean queue and utilisation against fleet size for the ambulance and truck fleets](../images/transport_capacity_margin_by_fleet_size.png)

Mean queue and mean utilisation against fleet size for the ambulance and truck fleets under the shipped configuration, each with a 95% confidence ribbon and a dashed vertical line at the current establishment size.

<!-- GEN transport_high -->
| Fleet size | Ambulance mean queue | Truck mean queue |
|---|---|---|
| 1 | 46.7132 [37.7039, 55.7225] | 0.1202 [0.1006, 0.1398] |
| 2 | 1.1259 [0.8840, 1.3678] | 0.0090 [0.0075, 0.0104] |
| 3 (current ambulance) | 0.2188 [0.1604, 0.2771] | 0.0013 [0.0009, 0.0018] |
| 4 (current truck) | 0.0644 [0.0366, 0.0921] | 0.0002 [0.0001, 0.0004] |
| 5 | 0.0122 [0.0088, 0.0155] | not swept |
<!-- /GEN -->

![Four panels of mean queue and utilisation against fleet size under the high intensity profile](../images/transport_capacity_margin_by_fleet_size_high_intensity.png)

The same measures under the `high_intensity` profile, each with a 95% confidence ribbon and a dashed vertical line at the current establishment size.

Under the shipped configuration a single ambulance queued <!-- GEN cell:transport|1|Ambulance mean queue|mean -->1.4397<!-- /GEN --> casualties (interval <!-- GEN cell:transport|1|Ambulance mean queue|ci -->[0.2895, 2.5898]<!-- /GEN -->), two queued <!-- GEN cell:transport|2|Ambulance mean queue|mean -->0.0866<!-- /GEN --> and the shipped three <!-- GEN cell:transport|3 (current ambulance)|Ambulance mean queue|mean -->0.0171<!-- /GEN -->. Under the high-intensity profile the corresponding ambulance queues were <!-- GEN cell:transport_high|1|Ambulance mean queue|mean -->46.7132<!-- /GEN -->, <!-- GEN cell:transport_high|2|Ambulance mean queue|mean -->1.1259<!-- /GEN --> and <!-- GEN cell:transport_high|3 (current ambulance)|Ambulance mean queue|mean -->0.2188<!-- /GEN -->, and the shipped four trucks queued <!-- GEN cell:transport_high|4 (current truck)|Truck mean queue|mean -->0.0002<!-- /GEN -->.

### Forward Holding of Post-Operative Intensive Care

**Question.** What changes when a casualty operated on at R2B is held there for part of their post-operative intensive care, for a stability window or while R2E intensive care is saturated? `[default, seven forward holding arms · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN forward_hold -->
| Forward holding rule | R2E ICU mean queue | R2B ICU utilisation | R2E ICU utilisation | Post-definitive care in ICU | Died of wounds per run |
|---|---|---|---|---|---|
| Off (current) | 1.578 [0.736, 2.420] | 1.5% | 87.6% | 31.4 [30.3, 32.5] | 11.67 [10.24, 13.09] |
| 120 min | 1.012 [0.616, 1.409] | 5.3% | 85.9% | 33.9 [32.5, 35.2] | 12.03 [10.73, 13.34] |
| 240 min | 0.771 [0.582, 0.961] | 9.2% | 86.3% | 35.6 [34.4, 36.8] | 11.23 [9.70, 12.76] |
| 480 min | 0.861 [0.622, 1.099] | 17.2% | 85.9% | 35.4 [33.4, 37.4] | 12.73 [11.17, 14.30] |
| 1,440 min | 0.819 [0.468, 1.171] | 38.9% | 83.2% | 41.1 [39.0, 43.3] | 13.43 [12.03, 14.84] |
| Capacity only | 0.923 [0.535, 1.310] | 18.1% | 88.1% | 35.6 [34.0, 37.1] | 12.70 [10.85, 14.55] |
| 240 min + capacity | 2.045 [0.000, 4.791] | 23.9% | 87.9% | 37.0 [34.9, 39.1] | 12.83 [11.30, 14.37] |
<!-- /GEN -->

![Five stacked panels against the forward holding rule](../images/r2b_forward_hold_frontier.png)

R2E intensive care mean queue, R2B and R2E intensive care utilisation, the share of post-definitive care delivered in intensive care and the died-of-wounds count under each forward holding arm, each with a 95% confidence ribbon. The windows apply to damage control and single-stage casualties alike.

The R2E intensive care queue was <!-- GEN cell:forward_hold|Off (current)|R2E ICU mean queue|full -->1.578 [0.736, 2.420]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold|240 min|R2E ICU mean queue|full -->0.771 [0.582, 0.961]<!-- /GEN --> with a 240-minute window. R2B intensive care utilisation rose from <!-- GEN cell:forward_hold|Off (current)|R2B ICU utilisation|full -->1.5%<!-- /GEN --> with no forward holding to <!-- GEN cell:forward_hold|1,440 min|R2B ICU utilisation|full -->38.9%<!-- /GEN --> at a 1,440-minute window while R2E intensive care utilisation moved from <!-- GEN cell:forward_hold|Off (current)|R2E ICU utilisation|full -->87.6%<!-- /GEN --> to <!-- GEN cell:forward_hold|1,440 min|R2E ICU utilisation|full -->83.2%<!-- /GEN -->. Post-definitive care in an intensive care bed rose from <!-- GEN cell:forward_hold|Off (current)|Post-definitive care in ICU|full -->31.4 [30.3, 32.5]<!-- /GEN --> to <!-- GEN cell:forward_hold|1,440 min|Post-definitive care in ICU|full -->41.1 [39.0, 43.3]<!-- /GEN -->, and died of wounds per run was <!-- GEN cell:forward_hold|Off (current)|Died of wounds per run|full -->11.67 [10.24, 13.09]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold|1,440 min|Died of wounds per run|full -->13.43 [12.03, 14.84]<!-- /GEN --> at 1,440 minutes. Holding on capacity grounds alone gave an R2E queue of <!-- GEN cell:forward_hold|Capacity only|R2E ICU mean queue|full -->0.923 [0.535, 1.310]<!-- /GEN -->, and a 240-minute window with the capacity trigger gave <!-- GEN cell:forward_hold|240 min + capacity|R2E ICU mean queue|full -->2.045 [0.000, 4.791]<!-- /GEN -->.

### Post-Operative Intensive Care Gate

**Question.** What does the rationing rule that defers lower-priority surgery when intensive care is saturated change? `[default, gate on and off · 360 d · 30 replications · paired, closing 90 d]`

<!-- GEN icu_gate -->
| Measure | Without the rule | With the rule | Paired difference |
|---|---|---|---|
| R2E ICU utilisation (%) | 94.8 [93.8, 95.8] | 87.1 [86.0, 88.2] | −7.7 [−9.4, −6.0] |
| Died of wounds per run | 14.50 [12.83, 16.17] | 15.40 [14.14, 16.66] | +0.90 [−1.17, +2.97] |
| Total casualties | 5,405.2 [5,331.1, 5,479.4] | 5,359.9 [5,282.7, 5,437.1] | −45.3 [−141.5, +50.9] |
<!-- /GEN -->

R2E intensive care utilisation was <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|Without the rule|full -->94.8 [93.8, 95.8]<!-- /GEN --> without the rule and <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|With the rule|full -->87.1 [86.0, 88.2]<!-- /GEN --> with it, a paired difference of <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|Paired difference|full -->−7.7 [−9.4, −6.0]<!-- /GEN --> percentage points. Died of wounds per run were <!-- GEN cell:icu_gate|Died of wounds per run|Without the rule|full -->14.50 [12.83, 16.17]<!-- /GEN --> and <!-- GEN cell:icu_gate|Died of wounds per run|With the rule|full -->15.40 [14.14, 16.66]<!-- /GEN -->, a paired difference of <!-- GEN cell:icu_gate|Died of wounds per run|Paired difference|full -->+0.90 [−1.17, +2.97]<!-- /GEN -->.

<!-- GEN icu_gate_pathways -->
| Recovery pathway | Casualty-replications | Died of wounds | Rate |
|---|---|---|---|
| Intensive care bed | 18,440 | 3 | 0.02% |
| Holding bed | 18,579 | 20 | 0.11% |
<!-- /GEN -->

Where the rule was in force, casualties recovering in a holding bed died of wounds at <!-- GEN cell:icu_gate_pathways|Holding bed|Rate|full -->0.11%<!-- /GEN --> and those recovering in an intensive care bed at <!-- GEN cell:icu_gate_pathways|Intensive care bed|Rate|full -->0.02%<!-- /GEN -->, pooled over the replications of that arm.

### Evacuation Policy

**Question.** What happens to returns to duty, mortality and the R2E pools when the days a casualty may recover in theatre before evacuation is set across its doctrinal range? `[default, evacuation policy 15 to 60 days · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN policy -->
| Response | 15 d | 21 d (shipped) | 30 d | 45 d | 60 d |
|---|---|---|---|---|---|
| R2E hold occupancy (%) | 21.1 [19.5, 22.7] | 44.8 [42.1, 47.4] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] |
| R2E hold mean queue | 0.00 [0.00, 0.00] | 0.32 [0.01, 0.63] | 733.98 [689.13, 778.83] | 1812.94 [1767.87, 1858.00] | 1932.34 [1887.68, 1977.01] |
| R2E ICU occupancy (%) | 87.9 [86.7, 89.1] | 87.6 [86.4, 88.7] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] |
| R2E ICU mean queue | 1.00 [0.72, 1.29] | 1.58 [0.74, 2.42] | 108.10 [97.89, 118.32] | 40.43 [35.77, 45.09] | 22.36 [19.14, 25.58] |
| Post-definitive ICU access (%) | 32.4 [30.8, 34.1] | 31.4 [30.3, 32.5] | 3.3 [2.7, 3.9] | 4.3 [3.7, 4.8] | 5.6 [4.6, 6.6] |
| In-theatre share (%) | 5.2 [5.0, 5.5] | 12.1 [11.8, 12.3] | 29.9 [29.3, 30.5] | 66.5 [65.8, 67.3] | 87.2 [86.6, 87.8] |
| Returns to duty | 1492.0 [1470.9, 1513.0] | 1686.8 [1662.8, 1710.7] | 1872.7 [1851.7, 1893.7] | 1793.9 [1774.2, 1813.6] | 1735.7 [1703.6, 1767.7] |
| Died of wounds | 10.93 [10.04, 11.83] | 11.67 [10.24, 13.09] | 16.17 [14.54, 17.79] | 21.43 [19.63, 23.24] | 24.07 [22.24, 25.89] |
| Never evacuated by horizon | 0.9 [0.4, 1.5] | 2.7 [1.0, 4.3] | 422.2 [401.8, 442.7] | 366.6 [355.7, 377.5] | 156.9 [146.7, 167.0] |
| Mean evacuation wait (d) | 0.44 [0.20, 0.69] | 0.39 [0.32, 0.47] | 33.35 [31.29, 35.40] | 95.73 [92.23, 99.23] | 100.73 [94.52, 106.94] |
| Role 4 peak beds | 206.5 [199.0, 214.0] | 206.1 [198.4, 213.8] | 105.4 [99.3, 111.5] | 29.6 [26.0, 33.3] | 15.0 [12.1, 18.0] |
<!-- /GEN -->

At the shipped 21-day policy the R2E holding queue was <!-- GEN cell:policy|R2E hold mean queue|21 d (shipped)|full -->0.32 [0.01, 0.63]<!-- /GEN --> and its occupancy <!-- GEN cell:policy|R2E hold occupancy (%)|21 d (shipped)|full -->44.8 [42.1, 47.4]<!-- /GEN -->. At 30 days the holding occupancy was <!-- GEN cell:policy|R2E hold occupancy (%)|30 d|full -->100.0 [100.0, 100.0]<!-- /GEN --> and the queue <!-- GEN cell:policy|R2E hold mean queue|30 d|full -->733.98 [689.13, 778.83]<!-- /GEN -->; at 45 and 60 days the occupancy was <!-- GEN cell:policy|R2E hold occupancy (%)|45 d|full -->100.0 [100.0, 100.0]<!-- /GEN --> and <!-- GEN cell:policy|R2E hold occupancy (%)|60 d|full -->100.0 [100.0, 100.0]<!-- /GEN -->. Returns to duty per campaign were <!-- GEN cell:policy|Returns to duty|15 d|full -->1492.0 [1470.9, 1513.0]<!-- /GEN --> at 15 days, <!-- GEN cell:policy|Returns to duty|21 d (shipped)|full -->1686.8 [1662.8, 1710.7]<!-- /GEN --> at 21, <!-- GEN cell:policy|Returns to duty|30 d|full -->1872.7 [1851.7, 1893.7]<!-- /GEN --> at 30, <!-- GEN cell:policy|Returns to duty|45 d|full -->1793.9 [1774.2, 1813.6]<!-- /GEN --> at 45 and <!-- GEN cell:policy|Returns to duty|60 d|full -->1735.7 [1703.6, 1767.7]<!-- /GEN --> at 60. Died of wounds were <!-- GEN cell:policy|Died of wounds|15 d|full -->10.93 [10.04, 11.83]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|21 d (shipped)|full -->11.67 [10.24, 13.09]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|30 d|full -->16.17 [14.54, 17.79]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|45 d|full -->21.43 [19.63, 23.24]<!-- /GEN --> and <!-- GEN cell:policy|Died of wounds|60 d|full -->24.07 [22.24, 25.89]<!-- /GEN --> across the five policies. The realised in-theatre share was <!-- GEN cell:policy|In-theatre share (%)|15 d|full -->5.2 [5.0, 5.5]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|21 d (shipped)|full -->12.1 [11.8, 12.3]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|30 d|full -->29.9 [29.3, 30.5]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|45 d|full -->66.5 [65.8, 67.3]<!-- /GEN --> and <!-- GEN cell:policy|In-theatre share (%)|60 d|full -->87.2 [86.6, 87.8]<!-- /GEN --> percent. Paired differences against the shipped policy are in `data/policy/policy_sweep_paired.csv`.

### R2E Holding Establishment

**Question.** What does enlarging the R2E holding establishment change at the shipped evacuation policy? `[default, 21-day policy, 30 to 90 holding beds · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN establishment -->
| Response | 30 beds (shipped) | 45 beds | 60 beds | 90 beds |
|---|---|---|---|---|
| R2E hold occupancy (%) | 44.8 [42.1, 47.4] | 29.9 [27.8, 31.9] | 21.3 [20.0, 22.6] | 13.9 [13.1, 14.8] |
| R2E hold mean queue | 0.32 [0.01, 0.63] | 0.00 [0.00, 0.00] | 0.00 [0.00, 0.00] | 0.00 [0.00, 0.00] |
| R2E ICU mean queue | 1.58 [0.74, 2.42] | 1.05 [0.56, 1.53] | 0.99 [0.54, 1.44] | 0.91 [0.45, 1.36] |
| Post-definitive ICU access (%) | 31.4 [30.3, 32.5] | 32.9 [31.4, 34.5] | 33.2 [32.2, 34.2] | 33.6 [32.7, 34.5] |
| In-theatre share (%) | 12.1 [11.8, 12.3] | 12.4 [12.2, 12.7] | 12.2 [12.0, 12.5] | 12.2 [12.0, 12.5] |
| Returns to duty | 1686.8 [1662.8, 1710.7] | 1676.0 [1650.1, 1701.9] | 1678.2 [1652.9, 1703.6] | 1668.3 [1644.1, 1692.5] |
| Died of wounds | 11.7 [10.2, 13.1] | 11.4 [10.0, 12.8] | 11.9 [10.5, 13.2] | 11.9 [10.7, 13.1] |
| Never evacuated by horizon | 2.7 [1.0, 4.3] | 1.4 [0.6, 2.1] | 1.6 [0.7, 2.5] | 1.4 [0.6, 2.3] |
| Role 4 peak beds | 206.1 [198.4, 213.8] | 204.0 [195.2, 212.9] | 198.6 [192.0, 205.1] | 199.0 [191.5, 206.5] |
<!-- /GEN -->

Holding occupancy was <!-- GEN cell:establishment|R2E hold occupancy (%)|30 beds (shipped)|full -->44.8 [42.1, 47.4]<!-- /GEN --> at the shipped 30 beds, <!-- GEN cell:establishment|R2E hold occupancy (%)|45 beds|full -->29.9 [27.8, 31.9]<!-- /GEN --> at 45, <!-- GEN cell:establishment|R2E hold occupancy (%)|60 beds|full -->21.3 [20.0, 22.6]<!-- /GEN --> at 60 and <!-- GEN cell:establishment|R2E hold occupancy (%)|90 beds|full -->13.9 [13.1, 14.8]<!-- /GEN --> at 90 percent. The R2E holding queue was <!-- GEN cell:establishment|R2E hold mean queue|30 beds (shipped)|full -->0.32 [0.01, 0.63]<!-- /GEN --> at 30 beds and <!-- GEN cell:establishment|R2E hold mean queue|45 beds|full -->0.00 [0.00, 0.00]<!-- /GEN --> at 45. Returns to duty per campaign were <!-- GEN cell:establishment|Returns to duty|30 beds (shipped)|full -->1686.8 [1662.8, 1710.7]<!-- /GEN --> at 30 beds and <!-- GEN cell:establishment|Returns to duty|90 beds|full -->1668.3 [1644.1, 1692.5]<!-- /GEN --> at 90, and died of wounds <!-- GEN cell:establishment|Died of wounds|30 beds (shipped)|full -->11.7 [10.2, 13.1]<!-- /GEN --> and <!-- GEN cell:establishment|Died of wounds|90 beds|full -->11.9 [10.7, 13.1]<!-- /GEN -->.

### Forward Surgical Saturation Release

**Question.** What changes when casualties are released to strategic evacuation with the definitive repair outstanding once the R2E theatre queue reaches a threshold? `[default, threshold 0 to 24 casualties · 360 d · 30 replications · paired, closing 90 d]`

<!-- GEN saturation -->
| Response | 0 (disabled) | 1 | 2 | 3 | 5 | 8 (shipped) | 12 | 16 | 24 |
|---|---|---|---|---|---|---|---|---|---|
| Theatre mean queue | 5.77 [3.82, 7.71] | 1.90 [1.10, 2.69] | 1.82 [1.40, 2.24] | 2.26 [1.47, 3.04] | 2.57 [1.88, 3.26] | 3.95 [2.71, 5.19] | 3.24 [2.44, 4.04] | 3.19 [2.71, 3.68] | 5.50 [3.97, 7.03] |
| Released with repair outstanding | 0.0 [0.0, 0.0] | 276.0 [259.7, 292.3] | 228.4 [209.4, 247.4] | 199.0 [181.7, 216.3] | 189.6 [165.3, 213.9] | 146.9 [132.7, 161.1] | 95.8 [82.8, 108.8] | 62.2 [48.6, 75.7] | 54.6 [37.4, 71.8] |
| Role 4 operations owed | 963.4 [934.0, 992.7] | 1252.2 [1204.5, 1300.0] | 1195.0 [1147.0, 1242.9] | 1141.7 [1094.8, 1188.7] | 1154.7 [1101.9, 1207.5] | 1127.1 [1089.6, 1164.6] | 1035.8 [993.8, 1077.8] | 1005.2 [964.5, 1045.9] | 998.5 [944.9, 1052.2] |
| Post-definitive ICU access (%) | 35.1 [34.2, 36.0] | 30.1 [28.9, 31.4] | 30.8 [28.8, 32.9] | 32.0 [30.7, 33.4] | 30.9 [29.2, 32.6] | 31.4 [30.3, 32.5] | 33.1 [32.0, 34.3] | 34.1 [33.0, 35.2] | 33.9 [32.4, 35.4] |
| Died of wounds | 11.8 [10.4, 13.1] | 11.9 [10.6, 13.1] | 11.0 [9.1, 12.9] | 11.1 [9.8, 12.5] | 12.9 [11.4, 14.3] | 11.7 [10.2, 13.1] | 12.7 [11.0, 14.4] | 10.8 [9.6, 12.1] | 12.3 [10.8, 13.9] |
| Returns to duty | 1668.1 [1647.5, 1688.8] | 1648.7 [1622.9, 1674.5] | 1649.8 [1621.3, 1678.2] | 1650.8 [1625.4, 1676.1] | 1681.9 [1651.3, 1712.5] | 1686.8 [1662.8, 1710.7] | 1655.7 [1629.4, 1682.0] | 1661.2 [1637.2, 1685.3] | 1657.1 [1630.6, 1683.6] |
<!-- /GEN -->

With the release disabled the closing-window theatre queue was <!-- GEN cell:saturation|Theatre mean queue|0 (disabled)|full -->5.77 [3.82, 7.71]<!-- /GEN --> casualties and no casualty was released with the repair outstanding. At the shipped threshold of eight the queue was <!-- GEN cell:saturation|Theatre mean queue|8 (shipped)|full -->3.95 [2.71, 5.19]<!-- /GEN --> and <!-- GEN cell:saturation|Released with repair outstanding|8 (shipped)|full -->146.9 [132.7, 161.1]<!-- /GEN --> casualties per campaign were released with the repair outstanding, with <!-- GEN cell:saturation|Role 4 operations owed|8 (shipped)|full -->1127.1 [1089.6, 1164.6]<!-- /GEN --> operations owed at the national support base against <!-- GEN cell:saturation|Role 4 operations owed|0 (disabled)|full -->963.4 [934.0, 992.7]<!-- /GEN --> with the release disabled. Returns to duty and died of wounds per campaign were <!-- GEN cell:saturation|Returns to duty|8 (shipped)|full -->1686.8 [1662.8, 1710.7]<!-- /GEN --> and <!-- GEN cell:saturation|Died of wounds|8 (shipped)|full -->11.7 [10.2, 13.1]<!-- /GEN --> at the shipped threshold and <!-- GEN cell:saturation|Returns to duty|0 (disabled)|full -->1668.1 [1647.5, 1688.8]<!-- /GEN --> and <!-- GEN cell:saturation|Died of wounds|0 (disabled)|full -->11.8 [10.4, 13.1]<!-- /GEN --> with the release disabled.

---

## National Support Base and Strategic Airlift

<small>[Return to Top](#contents)</small>

**Question.** How does strategic evacuation demand and its wait respond to the shipped airlift schedule, to the interval between sorties and to sortie cancellation, and what census does the national support base carry? `[default, both intensities and swept values · 360 d · 30 replications · per-replication reductions]`

<!-- GEN airlift_baseline -->
| Response at the shipped schedule | Moderate intensity | High intensity |
|---|---|---|
| Casualties boarded | 2275.33 [2225.02, 2325.64] | 2893.27 [2877.31, 2909.22] |
| Still waiting at the close | 1.73 [0.75, 2.71] | 1905.30 [1887.18, 1923.42] |
| Mean wait (days) | 0.39 [0.28, 0.50] | 17.64 [17.31, 17.98] |
| Share of R2E holding beds held by the evacuation wait | 2% [1%, 3%] | 27% [26%, 29%] |
| Role 4 peak occupancy (concurrent patients) | 181.67 [174.05, 189.28] | 198.03 [195.62, 200.45] |
| Days the peak falls before the campaign ends | 141.63 [106.63, 176.63] | 168.70 [130.99, 206.41] |
<!-- /GEN -->

At the shipped schedule <!-- GEN cell:airlift_baseline|Casualties boarded|Moderate intensity|mean -->2275.33<!-- /GEN --> casualties boarded at moderate intensity and <!-- GEN cell:airlift_baseline|Casualties boarded|High intensity|mean -->2893.27<!-- /GEN --> at high, with <!-- GEN cell:airlift_baseline|Still waiting at the close|Moderate intensity|mean -->1.73<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Still waiting at the close|High intensity|mean -->1905.30<!-- /GEN --> still waiting at the close and mean waits of <!-- GEN cell:airlift_baseline|Mean wait (days)|Moderate intensity|mean -->0.39<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Mean wait (days)|High intensity|mean -->17.64<!-- /GEN --> days. The evacuation wait held <!-- GEN cell:airlift_baseline|Share of R2E holding beds held by the evacuation wait|Moderate intensity|full -->2% [1%, 3%]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Share of R2E holding beds held by the evacuation wait|High intensity|full -->27% [26%, 29%]<!-- /GEN --> of the R2E holding beds. Role 4 peak occupancy was <!-- GEN cell:airlift_baseline|Role 4 peak occupancy (concurrent patients)|Moderate intensity|full -->181.67 [174.05, 189.28]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Role 4 peak occupancy (concurrent patients)|High intensity|full -->198.03 [195.62, 200.45]<!-- /GEN --> concurrent patients, and the peak fell <!-- GEN cell:airlift_baseline|Days the peak falls before the campaign ends|Moderate intensity|full -->141.63 [106.63, 176.63]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Days the peak falls before the campaign ends|High intensity|full -->168.70 [130.99, 206.41]<!-- /GEN --> days before the campaign ended.

<!-- GEN airlift_interval -->
| Response by interval between sorties | 3 days | 5 days | 7 days (shipped) | 10 days | 14 days |
|---|---|---|---|---|---|
| Sorties flown | 119.00 | 71.00 | 51.00 | 35.00 | 25.00 |
| Mean wait (days) | 0.30 [0.23, 0.37] | 0.32 [0.27, 0.36] | 0.39 [0.28, 0.50] | 8.44 [6.33, 10.54] | 27.35 [25.85, 28.86] |
| Share of R2E holding beds held by the evacuation wait | 0% [0%, 0%] | 1% [0%, 1%] | 2% [1%, 3%] | 41% [36%, 45%] | 57% [56%, 58%] |
| Ventilated pre-flight intensive care hold (hours) | 25.10 [24.67, 25.53] | 25.16 [24.62, 25.71] | 26.28 [25.36, 27.19] | 127.66 [103.19, 152.14] | 423.66 [397.04, 450.28] |
<!-- /GEN -->

Shortening the interval between sorties below the shipped seven days left the mean wait at <!-- GEN cell:airlift_interval|Mean wait (days)|3 days|mean -->0.30<!-- /GEN --> days at three days and <!-- GEN cell:airlift_interval|Mean wait (days)|5 days|mean -->0.32<!-- /GEN --> at five against <!-- GEN cell:airlift_interval|Mean wait (days)|7 days (shipped)|mean -->0.39<!-- /GEN --> at seven. Lengthening it gave <!-- GEN cell:airlift_interval|Mean wait (days)|10 days|full -->8.44 [6.33, 10.54]<!-- /GEN --> days at ten days with <!-- GEN cell:airlift_interval|Sorties flown|10 days|full -->35.00<!-- /GEN --> sorties flown and <!-- GEN cell:airlift_interval|Mean wait (days)|14 days|full -->27.35 [25.85, 28.86]<!-- /GEN --> days at fourteen with <!-- GEN cell:airlift_interval|Sorties flown|14 days|full -->25.00<!-- /GEN -->.

<!-- GEN airlift_reliability -->
| Response by configured cancellation probability | 0% | 5% | 10% | 15% | 25% | 40% |
|---|---|---|---|---|---|---|
| Sorties flown | 51.00 | 48.37 | 45.80 | 42.57 | 38.33 | 30.93 |
| Realised cancellation rate | 0% | 5% | 10% | 17% | 25% | 39% |
| Mean wait (days) | 0.39 [0.28, 0.50] | 0.81 [0.23, 1.39] | 1.67 [0.80, 2.55] | 3.14 [1.76, 4.52] | 6.66 [4.04, 9.27] | 16.66 [13.70, 19.61] |
| Share of R2E holding beds held by the evacuation wait | 2% [1%, 3%] | 6% [3%, 9%] | 12% [7%, 16%] | 19% [13%, 26%] | 29% [22%, 35%] | 48% [46%, 51%] |
<!-- /GEN -->

The mean wait rose from <!-- GEN cell:airlift_reliability|Mean wait (days)|0%|mean -->0.39<!-- /GEN --> days with no cancellation to <!-- GEN cell:airlift_reliability|Mean wait (days)|10%|mean -->1.67<!-- /GEN --> at 10%, <!-- GEN cell:airlift_reliability|Mean wait (days)|25%|mean -->6.66<!-- /GEN --> at 25% and <!-- GEN cell:airlift_reliability|Mean wait (days)|40%|mean -->16.66<!-- /GEN --> at 40%, as the sorties flown fell from <!-- GEN cell:airlift_reliability|Sorties flown|0%|full -->51.00<!-- /GEN --> to <!-- GEN cell:airlift_reliability|Sorties flown|40%|full -->30.93<!-- /GEN -->.

**Collapse classification.** A campaign was classified as collapsed where its R2E holding queue over the closing 90 days averaged twenty casualties or more. `[default, sortie cancellation 0 to 25% · 360 d · 30 replications · exact binomial]`

<!-- GEN airlift_collapse -->
| Sortie cancellation | Collapsed | Collapse rate (exact 95% interval) | Median closing-window queue | Worst closing-window queue |
|---|---|---|---|---|
| 0% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.00 | 1.48 |
| 5% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.08 | 5.66 |
| 10% | 3 of 30 | 10.0% [2.1%, 26.5%] | 0.03 | 56.27 |
| 15% | 7 of 30 | 23.3% [9.9%, 42.3%] | 0.21 | 136.16 |
| 20% | 12 of 30 | 40.0% [22.7%, 59.4%] | 0.79 | 218.03 |
| 25% | 16 of 30 | 53.3% [34.3%, 71.7%] | 26.51 | 273.03 |
<!-- /GEN -->

No campaign collapsed at <!-- GEN cell:airlift_collapse|0%|Sortie cancellation|full -->0%<!-- /GEN --> or 5% cancellation. <!-- GEN cell:airlift_collapse|10%|Collapsed|full -->3 of 30<!-- /GEN --> collapsed at 10% cancellation, <!-- GEN cell:airlift_collapse|15%|Collapsed|full -->7 of 30<!-- /GEN --> at 15%, <!-- GEN cell:airlift_collapse|20%|Collapsed|full -->12 of 30<!-- /GEN --> at 20% and <!-- GEN cell:airlift_collapse|25%|Collapsed|full -->16 of 30<!-- /GEN --> at 25%.

---

## Mass Casualty Events

<small>[Return to Top](#contents)</small>

**Question.** What changes when mass casualty events are injected into a campaign? `[default, injection off and on at 0.2 events per day · 360 d · 30 replications per arm · independent seeds, pooled exact binomial]`

<!-- GEN mass_casualty -->
| Metric | No events injected | Events injected |
|---|---|---|
| Average total casualties/run | 5359.9 | 8187.2 |
| Average events/run | 0 | 69.83 (range 57–83) |
| Died-of-wounds rate, ordinary casualties | 0.29% [0.26%, 0.31%] | 0.37% [0.34%, 0.40%] |
| Died-of-wounds rate, event casualties | not applicable | 0.77% [0.72%, 0.84%] |
<!-- /GEN -->

![Stem plot of two mass casualty events reconstructed from one campaign](../images/mass_casualty_events.png)

Two mass casualty events reconstructed from one seed-42 campaign, each drawn as a vertical line at its simulation day with a point at its casualty count.

Events added <!-- GEN cell:mass_casualty|Average events/run|Events injected|full -->69.83 (range 57–83)<!-- /GEN --> events per campaign and raised the mean campaign total from <!-- GEN cell:mass_casualty|Average total casualties/run|No events injected|full -->5359.9<!-- /GEN --> to <!-- GEN cell:mass_casualty|Average total casualties/run|Events injected|full -->8187.2<!-- /GEN --> casualties. The died-of-wounds rate among event casualties was <!-- GEN cell:mass_casualty|Died-of-wounds rate, event casualties|Events injected|full -->0.77% [0.72%, 0.84%]<!-- /GEN -->. Among ordinary casualties it was <!-- GEN cell:mass_casualty|Died-of-wounds rate, ordinary casualties|No events injected|full -->0.29% [0.26%, 0.31%]<!-- /GEN --> without injection and <!-- GEN cell:mass_casualty|Died-of-wounds rate, ordinary casualties|Events injected|full -->0.37% [0.34%, 0.40%]<!-- /GEN --> with it.

---

## Resolution of Paired Differences

<small>[Return to Top](#contents)</small>

**Question.** How many replications would each paired difference left open by the experiments above need before its interval narrowed to a stated half-width? `[as each experiment above · 360 d · 30 replications · paired, normal approximation to the interval half-width]`

<!-- GEN resolution -->
| Comparison | Paired difference | Half-width sought | Replications needed |
|---|---|---|---|
| Hold window, R2E first surgeries | −15.00 [−74.83, +44.83] | 2.0 | 24,655 |
| Hold window, R2E theatre entry deferred | −1.63 [−17.85, +14.58] | 1.0 | 7,246 |
| Hold window, diverted for a busy theatre | +15.33 [−7.88, +38.55] | 2.0 | 3,712 |
| Hold window, died of wounds | −0.43 [−2.75, +1.88] | 0.5 | 591 |
| Intensive care gate, died of wounds | +0.90 [−1.17, +2.97] | 0.5 | 471 |
| Policy 15 days against 21, died of wounds | −0.73 [−2.46, +0.99] | 1.0 | 82 |
| Policy 45 days against 21, died of wounds | +9.77 [+7.21, +12.32] | 1.0 | 181 |
| Policy 60 days against 21, died of wounds | +12.40 [+10.18, +14.62] | 1.0 | 137 |
| Saturation release at 8, died of wounds | −0.10 [−1.55, +1.35] | 1.0 | 58 |
| Saturation release at 8, returns to duty | +18.63 [−17.54, +54.81] | 10.0 | 361 |
<!-- /GEN -->

The count for each row is $\lceil (z_{0.975}\, s_d / h)^2 \rceil$, where $s_d$ is the standard deviation of the within-replication differences observed at 30 replications and $h$ the half-width sought, chosen in the response's own units by the script that ran the experiment. The hold window's R2E first-surgery difference was <!-- GEN cell:resolution|Hold window, R2E first surgeries|Paired difference|full -->−15.00 [−74.83, +44.83]<!-- /GEN --> against a half-width sought of <!-- GEN cell:resolution|Hold window, R2E first surgeries|Half-width sought|full -->2.0<!-- /GEN --> operations, which needs <!-- GEN cell:resolution|Hold window, R2E first surgeries|Replications needed|full -->24,655<!-- /GEN --> replications. The intensive care gate's died-of-wounds difference was <!-- GEN cell:resolution|Intensive care gate, died of wounds|Paired difference|full -->+0.90 [−1.17, +2.97]<!-- /GEN --> and needs <!-- GEN cell:resolution|Intensive care gate, died of wounds|Replications needed|full -->471<!-- /GEN -->. Against a half-width sought of <!-- GEN cell:resolution|Policy 15 days against 21, died of wounds|Half-width sought|full -->1.0<!-- /GEN --> death per campaign, the three policy comparisons against the shipped 21 days need <!-- GEN cell:resolution|Policy 15 days against 21, died of wounds|Replications needed|full -->82<!-- /GEN -->, <!-- GEN cell:resolution|Policy 45 days against 21, died of wounds|Replications needed|full -->181<!-- /GEN --> and <!-- GEN cell:resolution|Policy 60 days against 21, died of wounds|Replications needed|full -->137<!-- /GEN --> replications. The saturation release's died-of-wounds difference at the shipped threshold of eight needs <!-- GEN cell:resolution|Saturation release at 8, died of wounds|Replications needed|full -->58<!-- /GEN -->.

---

## Sensitivity Screens

<small>[Return to Top](#contents)</small>

**Question.** Which of the eighty screened parameters most influence the system operating theatre queue, and how is that influence divided among the leading eight? `[default · 30 d · Morris r = 20 with 5 replications per point; Sobol N = 800 with 8 replications per point]`

These screens are tagged 30 days because they were not re-measured at the sustained horizon; their rankings describe a month of campaign.

<!-- GEN morris_top -->
| Rank | Parameter | µ* | σ |
|---|---|---|---|
| 1 | `mass_casualty_rate` | 13.04 | 12.81 |
| 2 | `pri1_evac_prob` | 4.72 | 5.98 |
| 3 | `pri1_surg_prob` | 4.35 | 4.68 |
| 4 | `pri1_dcs_rate` | 4.21 | 6.06 |
| 5 | `mc_p1_balance` | 4.20 | 7.14 |
| 6 | `mass_casualty_kia_fraction` | 3.49 | 6.14 |
| 7 | `mass_casualty_max_cas` | 3.42 | 5.97 |
| 8 | `wia_cbt_mean` | 2.64 | 3.63 |
| 9 | `dnbi_disease_balance` | 2.45 | 3.08 |
| 10 | `surg_mode` | 2.33 | 3.33 |
| 11 | `saturation_queue_threshold` | 2.10 | 4.02 |
| 12 | `long_resus_mode` | 2.08 | 3.49 |
| 13 | `p1_p_max` | 2.03 | 3.56 |
| 14 | `pri2_surg_prob` | 2.01 | 2.86 |
| 15 | `r2b_pre_open_window` | 2.00 | 3.73 |
| 16 | `mass_casualty_min_cas` | 1.98 | 3.74 |
| 17 | `r2e_hold_mode` | 1.94 | 3.35 |
| 18 | `pri2_evac_prob` | 1.92 | 3.08 |
| 19 | `triage_p2_p3_balance` | 1.89 | 3.04 |
| 20 | `kia_cbt_mean` | 1.84 | 3.21 |
<!-- /GEN -->

The table lists the twenty parameters with the largest Morris $\mu^*$ on the system operating theatre queue, of eighty screened. `mass_casualty_rate` ranked first at $\mu^*$ = <!-- GEN cell:morris_top|1|µ*|full -->13.04<!-- /GEN -->, and the next six parameters ranked within $\mu^*$ of <!-- GEN cell:morris_top|2|µ*|full -->4.72<!-- /GEN --> to <!-- GEN cell:morris_top|7|µ*|full -->3.42<!-- /GEN -->.

Scatter plots of the screen for seven responses follow. Each plots every screened parameter at its mean absolute elementary effect on the horizontal axis against the standard deviation of its elementary effects on the vertical axis, coloured by the parameter's category: scenario context, health system capacity or health system policy.

![Morris screening scatter plot of the mean system operating theatre queue across R2B and R2E](../images/morris_system_ot_q.png)

Screening of the mean system operating theatre queue across R2B and R2E.

![Placeholder panel for the Morris screening of the mean R2B operating theatre queue](../images/morris_r2b_ot_q.png)

The response is degenerate, the forward theatre queue standing at zero at every design point, so no parameter produced an elementary effect and none is ranked.

![Morris screening scatter plot of the mean R2E operating theatre queue](../images/morris_r2e_ot_q.png)

Screening of the mean R2E operating theatre queue.

![Morris screening scatter plot of the mean R2E intensive care queue](../images/morris_r2e_icu_q.png)

Screening of the mean R2E intensive care queue.

![Morris screening scatter plot of the total died-of-wounds count](../images/morris_dow_count.png)

Screening of the total died-of-wounds count.

![Morris screening scatter plot of the mean transport queue across the PMV Ambulance and HX240M fleets](../images/morris_transport_q.png)

Screening of the mean transport queue across the PMV Ambulance and HX240M fleets.

![Morris screening scatter plot of the mean transport utilisation across the PMV Ambulance and HX240M fleets](../images/morris_transport_util.png)

Screening of the mean transport utilisation across the PMV Ambulance and HX240M fleets.

<!-- GEN sobol -->
| Parameter | Total-order index | First-order index |
|---|---|---|
| `mass_casualty_rate` | 0.79 [0.66, 0.92] | 0.34 [0.17, 0.47] |
| `mass_casualty_max_cas` | 0.27 [0.17, 0.37] | −0.06 [−0.09, −0.02] |
| `pri1_surg_prob` | 0.20 [0.07, 0.35] | 0.06 [−0.01, 0.11] |
| `pri1_evac_prob` | 0.14 [0.04, 0.24] | −0.00 [−0.04, 0.04] |
| `pri1_dcs_rate` | 0.10 [0.04, 0.17] | −0.00 [−0.04, 0.03] |
| `mass_casualty_kia_fraction` | 0.10 [−0.00, 0.21] | 0.04 [−0.01, 0.09] |
| `mc_p2_p3_balance` | 0.06 [0.01, 0.12] | −0.02 [−0.04, 0.02] |
| `mc_p1_balance` | 0.04 [−0.07, 0.13] | −0.01 [−0.05, 0.03] |
<!-- /GEN -->

The Sobol decomposition of the eight leading parameters on the same response gave `mass_casualty_rate` a total-order index of <!-- GEN cell:sobol|`mass_casualty_rate`|Total-order index|full -->0.79 [0.66, 0.92]<!-- /GEN -->. The next index was <!-- GEN cell:sobol|`mass_casualty_max_cas`|Total-order index|full -->0.27 [0.17, 0.37]<!-- /GEN --> for `mass_casualty_max_cas`.

---

## Annex A. Model Verification of One Seed-42 Campaign

<small>[Return to Top](#contents)</small>

This annex records the measurements of one 360-day campaign at seed 42 that verify mechanisms a replicated experiment cannot show: that each arrival stream realises its configured rate, that the triage and damage control splits realise their configured shares, that the strategic evacuation timeline closes, the surgical sections carry the load their rosters imply and the force regeneration cycle holds the pool near establishment. It reports one run, carries no interval and measures no performance; the replicated sections above do that. The measurements are tracked in `data/seed42_verification.csv`, written with the baseline by `Rscript run.R --seed 42 --days 360 --iterations 1 --refresh-baseline`.

**Casualty generation.** A stream's configured expectation is its configured daily mean per thousand personnel times its population over a thousand times the days.

<!-- GEN annex_generation -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Combat wounded in action | 1,547 | 1,593 |
| Support wounded in action | 779 | 796 |
| Combat killed in action | 548 | 612 |
| Support killed in action | 314 | 306 |
| Combat disease and non-battle injury | 1,713 | 1,836 |
| Support disease and non-battle injury | 423 | 423 |
<!-- /GEN -->

**Triage.** The expectation is the configured share of the casualties who received a priority.

<!-- GEN annex_triage -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Priority 1 | 2,841 | 2,900 |
| Priority 2 | 932 | 892 |
| Priority 3 | 689 | 669 |
| Killed in action | 862 | not applicable |
<!-- /GEN -->

**Damage control.** The expectation is the configured damage control rate times the casualties operated on at that priority.

<!-- GEN annex_damage_control -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Priority 1 operated | 1,476 | not applicable |
| Priority 1 damage control | 799 | 812 |
| Priority 2 operated | 431 | not applicable |
| Priority 2 damage control | 92 | 86 |
<!-- /GEN -->

**Strategic evacuation.**

<!-- GEN annex_evacuation -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Strategic evacuation decisions | 2,736 | not applicable |
| Boarded | 2,736 | not applicable |
| Still waiting at the close | 0 | not applicable |
<!-- /GEN -->

**Surgical load.** Utilisation of open time is the time-weighted share of a section's rostered time during which its first surgeon was in use, and the queued share is the share of that open time with one or more casualties waiting for any role in the section.

<!-- GEN annex_surgical_load -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Diverted from R2B, surgical team off shift | 969 | not applicable |
| Diverted from R2B, theatre busy | 210 | not applicable |
| R2B holding beds in use, both facilities (mean) | 6.55 | not applicable |
| R2E section 1 utilisation of open time (%) | 21.8 | not applicable |
| R2E section 2 utilisation of open time (%) | 44.4 | not applicable |
| R2E section 3 utilisation of open time (%) | 21.5 | not applicable |
| R2E section 1 queued share of open time (%) | 2.2 | not applicable |
| R2E section 2 queued share of open time (%) | 18.2 | not applicable |
| R2E section 3 queued share of open time (%) | 2.9 | not applicable |
<!-- /GEN -->

**Force regeneration.** The effective force is the combat or support pool at the day shown, under the shipped seven-day reinforcement cycle.

<!-- GEN annex_force -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Combat force, day 0 | 2,500 | not applicable |
| Combat force, day 180 | 2,408 | not applicable |
| Combat force, day 360 | 2,412 | not applicable |
| Support force, day 0 | 1,250 | not applicable |
| Support force, day 180 | 1,212 | not applicable |
| Support force, day 360 | 1,211 | not applicable |
<!-- /GEN -->

The conservation of a casualty's intensive care requirement across the three post-operative routes is asserted by `scripts/check_icu_time_conservation.R` and the generation rates by `scripts/check_arrival_rate_fidelity.R`.

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Blood, C. G., Zouris, J. M., & Rotblatt, D. (1998). *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

[2] Westphalen, N. (2018). Surgeon Captain Richard Tadeusz 'Rick' Jolly OBE RN Rtd. *Journal of Military and Veterans' Health*, *26*(1). Retrieved 26 Jul 26, from https://jmvh.org/article/surgeon-captain-richard-tadeusz-rick-jolly-obe-rn-rtd/

[3] Marble, S. (2025). Both joint and not: Medical support at Okinawa, 1945. *Joint Force Quarterly*, *117*(2), article 11. National Defense University Press. Retrieved 17 Aug 26, from https://digitalcommons.ndu.edu/joint-force-quarterly/vol117/iss2/11/

<!-- REFERENCES END -->
