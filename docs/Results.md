# Measured Results of a Replicated Simulation of the Land-Based Trauma System

## Abstract

<small>[Return to Top](#contents)</small>

**Background**

A discrete event simulation of an Australian brigade-sized force and its land-based trauma system produces measurements that planners can read without the model's own interpretation attached. This document reports those measurements and nothing else: every replicated experiment, the verification of one seed-42 campaign, and the sensitivity screens.

**Methods**

Each replicated experiment ran 30 independent replications of a 360-day campaign under the shipped configuration or a named variation, and measured queues and occupancy as whole-pool totals over each campaign's closing 90 days. The sensitivity screens ran at 30 days. Intervals are 95% Student $t$ intervals about the mean of per-replication values unless a section states otherwise. The designs are in [Methods](Methods.md).

**Results**

At moderate casualty intensity a campaign produced <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|mean -->5,402.9<!-- /GEN --> casualties and at high intensity <!-- GEN cell:scenario_totals|Total casualties/run|High intensity|mean -->12,409.5<!-- /GEN -->. Over the closing 90 days the mean R2E operating theatre queue was <!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|mean -->4.384<!-- /GEN --> casualties at moderate intensity and <!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|mean -->519.388<!-- /GEN --> at high. At high intensity that queue stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|High: queue empty|full -->0%<!-- /GEN --> of the campaign and was still growing at day 360, while at moderate intensity it stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: queue empty|full -->64%<!-- /GEN -->. Over the closing 90 days the national support base carried a mean census of <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|Moderate intensity|mean -->136.89<!-- /GEN --> beds at moderate intensity and <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|High intensity|mean -->179.53<!-- /GEN --> at high. The treated-cohort died-of-wounds rate of the shipped default configuration was <!-- GEN cell:dow_calibration|Shipped default|Treated-cohort died-of-wounds rate|full -->0.319% [0.284%, 0.354%]<!-- /GEN --> at 360 days.

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
    - [Utilisation of Shared Fleets and Integral Evacuation Elements](#utilisation-of-shared-fleets-and-integral-evacuation-elements)
  - [Forward Holding of Post-Operative Intensive Care](#forward-holding-of-post-operative-intensive-care)
  - [Post-Operative Intensive Care Gate](#post-operative-intensive-care-gate)
  - [Evacuation Policy](#evacuation-policy)
  - [R2E Holding Establishment](#r2e-holding-establishment)
  - [Forward Surgical Saturation Release](#forward-surgical-saturation-release)
- [National Support Base and Strategic Airlift](#national-support-base-and-strategic-airlift)
- [Role 4 Bed Demand](#role-4-bed-demand)
- [Casualty Surge Events](#casualty-surge-events)
- [Casualty Surge Event Size](#casualty-surge-event-size)
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
| Total casualties/run | 5,402.9 [5,315.9, 5,489.8] (p10–p90: 5,127.9–5,668.3) | 12,409.5 [12,291.1, 12,527.9] (p10–p90: 12,093.0–12,795.4) | 2.30× |
| Wounded in action/run | 2,345.5 [2,275.9, 2,415.2] (p10–p90: 2,100.8–2,569.2) | 8,365.9 [8,238.8, 8,493.0] (p10–p90: 7,972.9–8,796.5) | 3.57× |
| Died of wounds/run | 10.67 [9.33, 12.00] (p10–p90: 6.0–15.0) | 277.60 [272.10, 283.10] (p10–p90: 259.9–295.1) | 26.0× |
| Died of wounds, as share of wounded | 0.45% [0.40%, 0.51%] | 3.32% [3.27%, 3.36%] | 7.30× |
<!-- /GEN -->

![Four panels of total casualties, wounded in action, deaths of wounds and deaths as a share of wounded at the two casualty intensities](../images/paper_casualty_totals.png)

Each panel plots the moderate and high casualty intensities as a point with a narrow 95% confidence interval bar and a wide band for the 10th-to-90th-percentile spread across campaigns. The bar shows how precisely the average is known and the band shows how much one campaign varies.

Moderate intensity produced <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|mean -->5,402.9<!-- /GEN --> casualties per run (95% interval <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|ci -->[5,315.9, 5,489.8]<!-- /GEN -->) and high intensity <!-- GEN cell:scenario_totals|Total casualties/run|High intensity|mean -->12,409.5<!-- /GEN --> (<!-- GEN cell:scenario_totals|Total casualties/run|High intensity|ci -->[12,291.1, 12,527.9]<!-- /GEN -->), a ratio of <!-- GEN cell:scenario_totals|Total casualties/run|Ratio|full -->2.30×<!-- /GEN -->. Wounded in action rose by <!-- GEN cell:scenario_totals|Wounded in action/run|Ratio|full -->3.57×<!-- /GEN -->, died of wounds per run by <!-- GEN cell:scenario_totals|Died of wounds/run|Ratio|full -->26.0×<!-- /GEN --> and died of wounds as a share of wounded by <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|Ratio|full -->7.30×<!-- /GEN -->, from <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|Moderate intensity|mean -->0.45%<!-- /GEN --> to <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|High intensity|mean -->3.32%<!-- /GEN -->.

<!-- GEN scenario_queue -->
| Resource group | Moderate intensity mean queue | High intensity mean queue | Ratio |
|---|---|---|---|
| R2B operating theatre | 0.000 [0.000, 0.000] | 0.000 [0.000, 0.000] | not applicable |
| R2B holding beds | 4.836 [3.566, 6.106] | 30.585 [25.220, 35.950] | 6.32× |
| R2E operating theatre | 4.384 [1.434, 7.335] | 519.388 [464.786, 573.991] | 118.46× |
| R2E intensive care | 1.355 [0.554, 2.156] | 1,438.305 [1,423.919, 1,452.690] | 1061.56× |
| R2E holding beds | 0.078 [−0.034, 0.190] | 556.080 [528.497, 583.663] | 7107.95× |
| Ambulance and truck fleets | 0.428 [−0.337, 1.193] | 0.217 [0.175, 0.260] | 0.51× |
<!-- /GEN -->

![Horizontal plot of queue growth factor on a logarithmic scale for five resource groups with a dashed reference line at the growth in casualty volume](../images/paper_queue_growth.png)

Queue growth factor from moderate to high intensity on a logarithmic scale, with a dashed reference line at the growth in casualty volume. Each point is sized by its absolute queue at high intensity.

![Four-panel bar chart of mean queue length by resource group at the two casualty intensities](../images/scenario_comparison.png)

Mean queue length by resource group at each casualty intensity. Each panel has its own vertical scale, and the error bars show the spread across campaigns rather than a confidence interval.

The R2B operating theatre queue was zero at both intensities. At moderate intensity the R2B holding beds carried a mean queue of <!-- GEN cell:scenario_queue|R2B holding beds|Moderate intensity mean queue|mean -->4.836<!-- /GEN --> casualties (<!-- GEN cell:scenario_queue|R2B holding beds|Moderate intensity mean queue|ci -->[3.566, 6.106]<!-- /GEN -->), against <!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|mean -->4.384<!-- /GEN --> at the R2E operating theatres (<!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|ci -->[1.434, 7.335]<!-- /GEN -->). At high intensity the R2E operating theatre queue was <!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|mean -->519.388<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|ci -->[464.786, 573.991]<!-- /GEN -->), the R2E intensive care queue <!-- GEN cell:scenario_queue|R2E intensive care|High intensity mean queue|mean -->1,438.305<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E intensive care|High intensity mean queue|ci -->[1,423.919, 1,452.690]<!-- /GEN -->) and the R2E holding queue <!-- GEN cell:scenario_queue|R2E holding beds|High intensity mean queue|mean -->556.080<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E holding beds|High intensity mean queue|ci -->[528.497, 583.663]<!-- /GEN -->); the ratios to moderate intensity were <!-- GEN cell:scenario_queue|R2E operating theatre|Ratio|full -->118.46×<!-- /GEN -->, <!-- GEN cell:scenario_queue|R2E intensive care|Ratio|full -->1061.56×<!-- /GEN --> and <!-- GEN cell:scenario_queue|R2E holding beds|Ratio|full -->7107.95×<!-- /GEN -->. The ambulance and truck fleets queued <!-- GEN cell:scenario_queue|Ambulance and truck fleets|Moderate intensity mean queue|mean -->0.428<!-- /GEN --> and <!-- GEN cell:scenario_queue|Ambulance and truck fleets|High intensity mean queue|mean -->0.217<!-- /GEN --> casualties at the two intensities. The high-intensity R2E figures are means over a window in which the queues were still growing, as the sections that follow record, so they describe the closing 90 days and not a level.

---

## Queue Behaviour over the Campaign

<small>[Return to Top](#contents)</small>

**Question.** Does a pool's queue clear between its busy spells, or does it stand for the whole campaign, and how does the share of post-operative care delivered in a holding bed change with time? `[moderate_intensity and high_intensity · 360 d · 30 replications · pool totals, unbinned series]`

<!-- GEN queue_clearance -->
| Resource pool | Moderate: queue empty | Moderate: longest run above zero (days) | High: queue empty | High: longest run above zero (days) |
|---|---|---|---|---|
| R2B holding beds | 46% | 45.1 (38.8–50.3) | 5% | 208.0 (155.9–335.9) |
| R2E operating theatres | 64% | 13.1 (11.2–15.5) | 0% | 356.7 (345.4–358.9) |
| R2E intensive care | 63% | 13.2 (8.5–20.4) | 0% | 357.9 (355.7–358.6) |
| R2E holding beds | 93% | 19.3 (3.8–24.0) | 1% | 357.2 (356.9–357.7) |
<!-- /GEN -->

![Four stacked panels of queue length against campaign day, one per resource pool, at the two casualty intensities](../images/queue_length_over_time.png)

Queue length against campaign day for each pool, as the median across replications with an interquartile band, at each casualty intensity. Each panel is annotated with the share of the campaign the queue stood empty.

The table reports, for each pool and intensity, the median across replications of the share of the campaign the pool's queue stood empty and of the longest unbroken spell it did not, with the interquartile range of the spell in parentheses. At high intensity every pool listed stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|High: queue empty|full -->0%<!-- /GEN --> of the campaign or less, and the median longest spell above zero in the R2E operating theatres was <!-- GEN cell:queue_clearance|R2E operating theatres|High: longest run above zero (days)|full -->356.7 (345.4–358.9)<!-- /GEN --> of the 360 days. At moderate intensity the same pool stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: queue empty|full -->64%<!-- /GEN --> of the campaign with a median longest spell of <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: longest run above zero (days)|full -->13.1 (11.2–15.5)<!-- /GEN --> days.

<!-- GEN degraded_care -->
| Intensity and stage | First day the median daily rate reaches 100% | First ten days | Last ten days | Whole campaign |
|---|---|---|---|---|
| Moderate intensity: Stabilisation | not reached | 47.4% | 49.2% | 50.6% |
| Moderate intensity: Post-definitive care | not reached | 61.6% | 68.9% | 68.3% |
| High intensity: Stabilisation | 7 | 71.5% | 100.0% | 99.2% |
| High intensity: Post-definitive care | 3 | 92.0% | 100.0% | 99.8% |
<!-- /GEN -->

![Two stacked panels of the share of casualties recovering in a holding bed against campaign day](../images/degraded_care_rate_over_time.png)

The share of casualties taking the holding-bed recovery against campaign day, stabilisation above and post-definitive care below, as a daily rate inside an interquartile band and as a running cumulative rate.

The degraded post-operative care rate is the share of casualties who took the holding-bed recovery in place of an intensive care bed at the point the decision was taken. At high intensity the median daily rate for post-definitive care first reached 100% on day <!-- GEN cell:degraded_care|High intensity: Post-definitive care|First day the median daily rate reaches 100%|full -->3<!-- /GEN --> and for stabilisation on day <!-- GEN cell:degraded_care|High intensity: Stabilisation|First day the median daily rate reaches 100%|full -->7<!-- /GEN -->; over the whole campaign <!-- GEN cell:degraded_care|High intensity: Post-definitive care|Whole campaign|full -->99.8%<!-- /GEN --> of post-definitive care and <!-- GEN cell:degraded_care|High intensity: Stabilisation|Whole campaign|full -->99.2%<!-- /GEN --> of stabilisation took the holding bed. At moderate intensity the whole-campaign rates were <!-- GEN cell:degraded_care|Moderate intensity: Post-definitive care|Whole campaign|full -->68.3%<!-- /GEN --> and <!-- GEN cell:degraded_care|Moderate intensity: Stabilisation|Whole campaign|full -->50.6%<!-- /GEN -->, and the median daily rate did not reach 100%.

---

## Sustained Operations

<small>[Return to Top](#contents)</small>

**Question.** Does each response settle to a level over a 360-day campaign, or does it keep moving? `[moderate_intensity and high_intensity · 360 d · 30 replications · twelve 30-day block means]`

Each response was reduced to twelve consecutive 30-day block means and classified from the trend over the late half of the campaign; the rule is in [Methods](Methods.md#classifying-stability).

<!-- GEN long_horizon_stability -->
| Response | Moderate intensity | High intensity |
|---|---|---|
| R2E operating theatre queue | converged at 3.27 | **drifting, +11.7%/block**, 36.9 to 573.6 |
| R2E holding bed queue | converged at 0.09 | **drifting, +9.3%/block**, 48.6 to 595.9 |
| R2E intensive care queue | converged at 1.05 | **drifting, +13.0%/block**, 32.5 to 1,590.1 |
| Strategic evacuation backlog | converged at 1.89 | **drifting, +12.4%/block**, 68.8 to 1,850.7 |
| R2B holding bed queue | converged at 4.73 | converged at 32.6 |
| Casualty arrivals per day | converged at 15.0 | converged at 34.6 |
| Deaths of wounds per day | converged at 0.03 | converged at 0.77 |
<!-- /GEN -->

![Panels of 30-day block means against campaign day for every response at the moderate and high casualty intensities](../images/long_horizon_blocks.png)

Block means of every queue, occupancy, arrival, evacuation backlog and mortality response over the campaign at the two casualty intensities, each panel headed by its stability classification at each intensity.

Four responses at high intensity were classified as drifting: the R2E operating theatre queue (<!-- GEN cell:long_horizon_stability|R2E operating theatre queue|High intensity|full -->**drifting, +11.7%/block**, 36.9 to 573.6<!-- /GEN -->), the R2E holding bed queue (<!-- GEN cell:long_horizon_stability|R2E holding bed queue|High intensity|full -->**drifting, +9.3%/block**, 48.6 to 595.9<!-- /GEN -->), the R2E intensive care queue (<!-- GEN cell:long_horizon_stability|R2E intensive care queue|High intensity|full -->**drifting, +13.0%/block**, 32.5 to 1,590.1<!-- /GEN -->) and the strategic evacuation backlog (<!-- GEN cell:long_horizon_stability|Strategic evacuation backlog|High intensity|full -->**drifting, +12.4%/block**, 68.8 to 1,850.7<!-- /GEN -->). The R2B holding bed queue at high intensity was classified as converged, at <!-- GEN cell:long_horizon_stability|R2B holding bed queue|High intensity|full -->converged at 32.6<!-- /GEN -->. No response at moderate intensity was classified as drifting; the R2E operating theatre queue was <!-- GEN cell:long_horizon_stability|R2E operating theatre queue|Moderate intensity|full -->converged at 3.27<!-- /GEN --> and the R2B holding bed queue <!-- GEN cell:long_horizon_stability|R2B holding bed queue|Moderate intensity|full -->converged at 4.73<!-- /GEN -->.

---

## Treated-Cohort Mortality

<small>[Return to Top](#contents)</small>

**Question.** What fraction of casualties who reach an R2B or R2E facility die of wounds, measured against the historical anchor of the campaign each configuration models? `[default, moderate_intensity and high_intensity · 360 d · 3 measurements of 10 replications · pooled, Student t]`

<!-- GEN dow_calibration -->
| Configuration | Historical anchor | Treated-cohort died-of-wounds rate | Replications |
|---|---|---|---|
| Shipped default | at or below, 0.46% (Ajax Bay) | 0.319% [0.284%, 0.354%] | 30 |
| Moderate intensity | at or below, 0.46% (Ajax Bay) | 0.319% [0.284%, 0.354%] | 30 |
| High intensity | reported, 3.40% (Okinawa) | 3.504% [3.432%, 3.576%] | 30 |
<!-- /GEN -->

The anchors are the Ajax Bay Advanced Surgical Centre's three deaths among over 650 casualties who reached forward surgical care alive [[2]](#references), an upper bound for the two Falklands-calibrated configurations, and the rate of casualties who reached a hospital alive and died there on Okinawa [[3]](#references), which the high-intensity row records as <!-- GEN cell:dow_calibration|High intensity|Historical anchor|full -->reported, 3.40% (Okinawa)<!-- /GEN -->. The shipped default's pooled rate over 360-day campaigns was <!-- GEN cell:dow_calibration|Shipped default|Treated-cohort died-of-wounds rate|full -->0.319% [0.284%, 0.354%]<!-- /GEN -->, moderate intensity's <!-- GEN cell:dow_calibration|Moderate intensity|Treated-cohort died-of-wounds rate|full -->0.319% [0.284%, 0.354%]<!-- /GEN --> and high intensity's <!-- GEN cell:dow_calibration|High intensity|Treated-cohort died-of-wounds rate|full -->3.504% [3.432%, 3.576%]<!-- /GEN -->. The calibration check `scripts/check_dow_calibration.R` runs at 30 days, the horizon at which the ceilings were fitted.

---

## Forward Holding and Evacuation Levers

<small>[Return to Top](#contents)</small>

Each subsection varies one setting of the shipped configuration and reports the responses it moved. Experiments with several arms draw every arm from one control seed unless the subsection says otherwise, and the paired difference is reported where the arms are paired.

### R2B Pre-Open Hold Window

**Question.** What changes when a casualty needing surgery at R2B is held forward for a surgical section about to reopen rather than moved rearward at once? `[default, hold window 0 and 60 min · 360 d · 30 replications · campaign totals, paired difference]`

<!-- GEN hold_window -->
| Measure | Window 0 | Window 60 min | Difference |
| --- | --- | --- | --- |
| Casualties held at R2B | 0.00 | 88.87 | +88.87 [+85.42, +92.31] |
| R2B surgeries | 664.63 | 718.30 | +53.67 [+31.17, +76.16] |
| Diverted, team off shift | 1061.73 | 997.73 | −64.00 [−105.06, −22.94] |
| Diverted, theatre busy | 187.67 | 201.60 | +13.93 [−4.25, +32.11] |
| R2E first surgeries | 1584.37 | 1573.47 | −10.90 [−72.35, +50.55] |
| R2E theatre entry deferred | 223.13 | 220.23 | −2.90 [−20.63, +14.83] |
| Died of wounds per run | 10.73 | 10.67 | −0.07 [−2.25, +2.11] |
| Total casualties | 5423.97 | 5402.87 | −21.10 [−161.67, +119.47] |
<!-- /GEN -->

![Forest plot of the paired difference in each of eight measures with its confidence interval](../images/paper_hold_window_effects.png)

The paired difference per campaign for each of the eight measures with its 95% confidence interval against a vertical line at zero.

A 60-minute window held <!-- GEN cell:hold_window|Casualties held at R2B|Window 60 min|full -->88.87<!-- /GEN --> casualties per campaign at R2B against none with a zero window. R2B surgeries differed by <!-- GEN cell:hold_window|R2B surgeries|Difference|full -->+53.67 [+31.17, +76.16]<!-- /GEN --> and diversions for an off-shift section by <!-- GEN cell:hold_window|Diverted, team off shift|Difference|full -->−64.00 [−105.06, −22.94]<!-- /GEN -->. The differences in diversions for a busy theatre were <!-- GEN cell:hold_window|Diverted, theatre busy|Difference|full -->+13.93 [−4.25, +32.11]<!-- /GEN -->, in R2E first surgeries <!-- GEN cell:hold_window|R2E first surgeries|Difference|full -->−10.90 [−72.35, +50.55]<!-- /GEN -->, in R2E theatre entry deferrals <!-- GEN cell:hold_window|R2E theatre entry deferred|Difference|full -->−2.90 [−20.63, +14.83]<!-- /GEN -->, in died of wounds <!-- GEN cell:hold_window|Died of wounds per run|Difference|full -->−0.07 [−2.25, +2.11]<!-- /GEN --> and in total casualties <!-- GEN cell:hold_window|Total casualties|Difference|full -->−21.10 [−161.67, +119.47]<!-- /GEN -->.

### R2B Holding Capacity and Evacuation Threshold

**Question.** How do the R2B holding queue and the pools behind it respond to the R2B holding bed establishment and to the threshold at which convalescent casualties are evacuated early? `[default, 5 to 10 holding beds per unit crossed with a 0 to 7 day threshold · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN hold_threshold_beds -->
| R2B holding beds per unit | R2B hold mean queue | R2B hold utilisation | R2E hold mean queue | R2E ICU mean queue | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|
| 5 (shipped) | 4.84 [3.57, 6.11] | 66.6% [65.1, 68.0] | 0.078 [0.000, 0.190] | 1.355 [0.554, 2.156] | 1673 [1647, 1700] | 10.7 [9.3, 12.0] |
| 7 | 2.09 [1.38, 2.80] | 61.6% [59.4, 63.9] | 0.009 [0.000, 0.021] | 0.700 [0.543, 0.856] | 1660 [1629, 1692] | 10.8 [9.6, 12.0] |
| 10 | 0.32 [0.10, 0.53] | 46.9% [44.8, 49.0] | 0.000 [0.000, 0.000] | 0.886 [0.576, 1.196] | 1670 [1648, 1692] | 9.5 [8.6, 10.5] |
<!-- /GEN -->

With the threshold disabled, raising the establishment from five to ten holding beds per unit moved the R2B holding queue from <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2B hold mean queue|full -->4.84 [3.57, 6.11]<!-- /GEN --> to <!-- GEN cell:hold_threshold_beds|10|R2B hold mean queue|full -->0.32 [0.10, 0.53]<!-- /GEN --> and R2B holding utilisation from <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2B hold utilisation|full -->66.6% [65.1, 68.0]<!-- /GEN --> to <!-- GEN cell:hold_threshold_beds|10|R2B hold utilisation|full -->46.9% [44.8, 49.0]<!-- /GEN -->. The R2E holding queue was <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2E hold mean queue|full -->0.078 [0.000, 0.190]<!-- /GEN --> at five beds and <!-- GEN cell:hold_threshold_beds|10|R2E hold mean queue|full -->0.000 [0.000, 0.000]<!-- /GEN --> at ten, and the R2E intensive care queue <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2E ICU mean queue|full -->1.355 [0.554, 2.156]<!-- /GEN --> and <!-- GEN cell:hold_threshold_beds|10|R2E ICU mean queue|full -->0.886 [0.576, 1.196]<!-- /GEN --> Returns to duty were <!-- GEN cell:hold_threshold_beds|5 (shipped)|Returns to duty|full -->1673 [1647, 1700]<!-- /GEN --> at five beds and <!-- GEN cell:hold_threshold_beds|10|Returns to duty|full -->1670 [1648, 1692]<!-- /GEN --> at ten, and deaths of wounds <!-- GEN cell:hold_threshold_beds|5 (shipped)|Deaths of wounds|full -->10.7 [9.3, 12.0]<!-- /GEN --> and <!-- GEN cell:hold_threshold_beds|10|Deaths of wounds|full -->9.5 [8.6, 10.5]<!-- /GEN -->.

<!-- GEN hold_threshold_threshold -->
| Evacuation threshold | R2B hold mean queue | R2B hold utilisation | R2E hold mean queue | R2E ICU mean queue | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|
| Disabled (shipped) | 4.84 [3.57, 6.11] | 66.6% [65.1, 68.0] | 0.078 [0.000, 0.190] | 1.355 [0.554, 2.156] | 1673 [1647, 1700] | 10.7 [9.3, 12.0] |
| 1 day | 0.01 [0.00, 0.04] | 7.1% [6.7, 7.5] | 1.655 [0.829, 2.481] | 1.274 [0.773, 1.775] | 1666 [1646, 1687] | 10.8 [9.3, 12.3] |
| 3 days | 0.04 [0.00, 0.09] | 20.4% [19.1, 21.6] | 1.047 [0.489, 1.606] | 1.099 [0.533, 1.666] | 1671 [1643, 1699] | 10.4 [9.2, 11.5] |
| 5 days (mode) | 0.15 [0.06, 0.25] | 31.0% [29.5, 32.6] | 1.018 [0.361, 1.675] | 2.051 [0.000, 4.558] | 1669 [1637, 1701] | 10.1 [8.8, 11.3] |
| 7 days | 0.49 [0.33, 0.66] | 43.0% [40.9, 45.1] | 0.219 [0.041, 0.396] | 0.662 [0.528, 0.795] | 1684 [1667, 1701] | 10.4 [9.2, 11.6] |
<!-- /GEN -->

![Eight panels of R2B and R2E queue and utilisation, returns to duty and died of wounds against the evacuation threshold](../images/r2b_hold_threshold_sweep.png)

R2B and R2E queue and utilisation, returns to duty and died of wounds against the evacuation threshold in days, one line per swept bed count, with a 95% confidence ribbon.

With five beds per unit, a one-day threshold moved the R2B holding queue to <!-- GEN cell:hold_threshold_threshold|1 day|R2B hold mean queue|full -->0.01 [0.00, 0.04]<!-- /GEN --> and the R2E holding queue to <!-- GEN cell:hold_threshold_threshold|1 day|R2E hold mean queue|full -->1.655 [0.829, 2.481]<!-- /GEN -->, and the R2E intensive care queue to <!-- GEN cell:hold_threshold_threshold|1 day|R2E ICU mean queue|full -->1.274 [0.773, 1.775]<!-- /GEN -->. A three-day threshold gave <!-- GEN cell:hold_threshold_threshold|3 days|R2B hold mean queue|full -->0.04 [0.00, 0.09]<!-- /GEN -->, <!-- GEN cell:hold_threshold_threshold|3 days|R2E hold mean queue|full -->1.047 [0.489, 1.606]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|3 days|R2E ICU mean queue|full -->1.099 [0.533, 1.666]<!-- /GEN --> on the same three responses. At five and seven days the R2B holding queue was <!-- GEN cell:hold_threshold_threshold|5 days (mode)|R2B hold mean queue|full -->0.15 [0.06, 0.25]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|7 days|R2B hold mean queue|full -->0.49 [0.33, 0.66]<!-- /GEN -->. Returns to duty were <!-- GEN cell:hold_threshold_threshold|Disabled (shipped)|Returns to duty|full -->1673 [1647, 1700]<!-- /GEN --> with the threshold disabled and <!-- GEN cell:hold_threshold_threshold|1 day|Returns to duty|full -->1666 [1646, 1687]<!-- /GEN --> at one day, and deaths of wounds <!-- GEN cell:hold_threshold_threshold|Disabled (shipped)|Deaths of wounds|full -->10.7 [9.3, 12.0]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|1 day|Deaths of wounds|full -->10.8 [9.3, 12.3]<!-- /GEN -->. The full grid, including returns to duty and deaths of wounds at every point, is in `data/sweeps/r2b_hold_threshold_sweep.csv`.

### Transport Fleet Size

**Question.** At what fleet size does the transport queue form, at each casualty intensity? `[default and high_intensity · 360 d · 30 replications · pool totals, closing 90 d]`

The first table is the shipped moderate-intensity configuration and the second the high-intensity profile.

<!-- GEN transport -->
| Fleet size | Ambulance mean queue | Truck mean queue |
|---|---|---|
| 1 | 1.2509 [0.4597, 2.0422] | 0.1133 [0.0055, 0.2210] |
| 2 | 0.1427 [0.0342, 0.2513] | 0.0369 [0.0000, 0.0816] |
| 3 (current ambulance) | 0.4278 [0.0000, 1.1927] | 0.0011 [0.0005, 0.0018] |
| 4 (current truck) | 0.0511 [0.0000, 0.1204] | 0.0001 [0.0000, 0.0002] |
| 5 | 0.0067 [0.0000, 0.0166] | not swept |
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

Under the shipped configuration a single ambulance queued <!-- GEN cell:transport|1|Ambulance mean queue|mean -->1.2509<!-- /GEN --> casualties (interval <!-- GEN cell:transport|1|Ambulance mean queue|ci -->[0.4597, 2.0422]<!-- /GEN -->), two queued <!-- GEN cell:transport|2|Ambulance mean queue|mean -->0.1427<!-- /GEN --> and the shipped three <!-- GEN cell:transport|3 (current ambulance)|Ambulance mean queue|mean -->0.4278<!-- /GEN -->. Under the high-intensity profile the corresponding ambulance queues were <!-- GEN cell:transport_high|1|Ambulance mean queue|mean -->46.7132<!-- /GEN -->, <!-- GEN cell:transport_high|2|Ambulance mean queue|mean -->1.1259<!-- /GEN --> and <!-- GEN cell:transport_high|3 (current ambulance)|Ambulance mean queue|mean -->0.2188<!-- /GEN -->, and the shipped four trucks queued <!-- GEN cell:transport_high|4 (current truck)|Truck mean queue|mean -->0.0002<!-- /GEN -->.

#### Utilisation of Shared Fleets and Integral Evacuation Elements

**Question.** How heavily is each transport holder used, the shared brigade fleets and the evacuation elements integral to R2B and R2E? `[default and high_intensity · 360 d · 30 replications · pool totals, closing 90 d, shipped establishment, not swept]`

Utilisation is the time-weighted share of the established units in use over the closing 90 days. An evacuation crew is two medics seized together, so a crew is measured on its lead medic.

<!-- GEN transport_holders -->
| Holder | Asset | Moderate intensity mean queue | Moderate intensity utilisation | High intensity mean queue | High intensity utilisation |
|---|---|---|---|---|---|
| PMV Ambulance fleet | Shared | 0.428 [0.000, 1.193] | 12.5% [12.0%, 13.0%] | 0.217 [0.174, 0.260] | 30.4% [29.7%, 31.1%] |
| HX2 40M fleet | Shared | 0.000 [0.000, 0.000] | 2.5% [2.4%, 2.6%] | 0.000 [0.000, 0.000] | 6.2% [6.0%, 6.3%] |
| R2B evacuation crews | Integral | 0.062 [0.045, 0.080] | 15.7% [14.9%, 16.6%] | 0.421 [0.384, 0.458] | 41.0% [39.9%, 42.1%] |
| R2E evacuation sections | Integral | 0.000 [0.000, 0.000] | 0.0% [0.0%, 0.0%] | 0.000 [0.000, 0.000] | 0.4% [0.4%, 0.4%] |
<!-- /GEN -->

At moderate intensity the R2B evacuation crews were in use for <!-- GEN cell:transport_holders|R2B evacuation crews|Moderate intensity utilisation|mean -->15.7%<!-- /GEN --> of the window and the ambulance fleet for <!-- GEN cell:transport_holders|PMV Ambulance fleet|Moderate intensity utilisation|mean -->12.5%<!-- /GEN -->. At high intensity the crews were in use for <!-- GEN cell:transport_holders|R2B evacuation crews|High intensity utilisation|mean -->41.0%<!-- /GEN --> (interval <!-- GEN cell:transport_holders|R2B evacuation crews|High intensity utilisation|ci -->[39.9%, 42.1%]<!-- /GEN -->) and queued <!-- GEN cell:transport_holders|R2B evacuation crews|High intensity mean queue|mean -->0.421<!-- /GEN --> casualties, the ambulance fleet was in use for <!-- GEN cell:transport_holders|PMV Ambulance fleet|High intensity utilisation|mean -->30.4%<!-- /GEN --> and the truck fleet for <!-- GEN cell:transport_holders|HX2 40M fleet|High intensity utilisation|mean -->6.2%<!-- /GEN -->. The R2E evacuation sections were in use for <!-- GEN cell:transport_holders|R2E evacuation sections|High intensity utilisation|mean -->0.4%<!-- /GEN --> at high intensity.

### Forward Holding of Post-Operative Intensive Care

**Question.** What changes when a casualty operated on at R2B is held there for part of their post-operative intensive care, for a stability window or while R2E intensive care is saturated? `[default, seven forward holding arms · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN forward_hold -->
| Forward holding rule | R2E ICU mean queue | R2B ICU utilisation | R2E ICU utilisation | Post-definitive care in ICU | Died of wounds per run |
|---|---|---|---|---|---|
| Off (current) | 1.355 [0.554, 2.156] | 1.6% | 87.8% | 31.7 [30.6, 32.8] | 10.67 [9.33, 12.00] |
| 120 min | 2.305 [0.000, 4.787] | 5.3% | 87.6% | 32.7 [30.9, 34.5] | 11.83 [10.44, 13.23] |
| 240 min | 0.911 [0.674, 1.149] | 9.3% | 86.7% | 34.4 [32.8, 36.0] | 11.40 [10.01, 12.79] |
| 480 min | 0.803 [0.368, 1.239] | 17.1% | 85.7% | 36.1 [34.6, 37.7] | 11.07 [9.83, 12.30] |
| 1,440 min | 1.020 [0.436, 1.603] | 36.8% | 82.9% | 41.4 [39.7, 43.1] | 12.23 [10.59, 13.87] |
| Capacity only | 0.953 [0.468, 1.437] | 16.8% | 88.4% | 37.0 [35.6, 38.3] | 12.40 [11.02, 13.78] |
| 240 min + capacity | 0.666 [0.546, 0.786] | 22.8% | 87.4% | 37.8 [36.2, 39.5] | 12.53 [11.41, 13.66] |
<!-- /GEN -->

![Five stacked panels against the forward holding rule](../images/r2b_forward_hold_frontier.png)

R2E intensive care mean queue, R2B and R2E intensive care utilisation, the share of post-definitive care delivered in intensive care and the died-of-wounds count under each forward holding arm, each with a 95% confidence ribbon. The windows apply to damage control and single-stage casualties alike.

The R2E intensive care queue was <!-- GEN cell:forward_hold|Off (current)|R2E ICU mean queue|full -->1.355 [0.554, 2.156]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold|240 min|R2E ICU mean queue|full -->0.911 [0.674, 1.149]<!-- /GEN --> with a 240-minute window. R2B intensive care utilisation rose from <!-- GEN cell:forward_hold|Off (current)|R2B ICU utilisation|full -->1.6%<!-- /GEN --> with no forward holding to <!-- GEN cell:forward_hold|1,440 min|R2B ICU utilisation|full -->36.8%<!-- /GEN --> at a 1,440-minute window while R2E intensive care utilisation moved from <!-- GEN cell:forward_hold|Off (current)|R2E ICU utilisation|full -->87.8%<!-- /GEN --> to <!-- GEN cell:forward_hold|1,440 min|R2E ICU utilisation|full -->82.9%<!-- /GEN -->. Post-definitive care in an intensive care bed rose from <!-- GEN cell:forward_hold|Off (current)|Post-definitive care in ICU|full -->31.7 [30.6, 32.8]<!-- /GEN --> to <!-- GEN cell:forward_hold|1,440 min|Post-definitive care in ICU|full -->41.4 [39.7, 43.1]<!-- /GEN -->, and died of wounds per run was <!-- GEN cell:forward_hold|Off (current)|Died of wounds per run|full -->10.67 [9.33, 12.00]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold|1,440 min|Died of wounds per run|full -->12.23 [10.59, 13.87]<!-- /GEN --> at 1,440 minutes. Holding on capacity grounds alone gave an R2E queue of <!-- GEN cell:forward_hold|Capacity only|R2E ICU mean queue|full -->0.953 [0.468, 1.437]<!-- /GEN -->, and a 240-minute window with the capacity trigger gave <!-- GEN cell:forward_hold|240 min + capacity|R2E ICU mean queue|full -->0.666 [0.546, 0.786]<!-- /GEN -->.

### Post-Operative Intensive Care Gate

**Question.** What does the rationing rule that defers lower-priority surgery when intensive care is saturated change? `[default, gate on and off · 360 d · 30 replications · paired, closing 90 d]`

<!-- GEN icu_gate -->
| Measure | Without the rule | With the rule | Paired difference |
|---|---|---|---|
| R2E ICU utilisation (%) | 93.0 [91.8, 94.2] | 87.8 [86.8, 88.9] | −5.1 [−6.7, −3.5] |
| Died of wounds per run | 10.57 [9.67, 11.46] | 10.67 [9.33, 12.00] | +0.10 [−1.71, +1.91] |
| Total casualties | 5,350.7 [5,290.0, 5,411.3] | 5,402.9 [5,315.9, 5,489.8] | +52.2 [−65.2, +169.6] |
<!-- /GEN -->

![Four panels comparing the post-operative intensive care gate disabled and enabled](../images/icu_gate.png)

Intensive care occupancy, deaths of wounds and the casualties taking each post-operative pathway with the gate disabled and enabled, each point with its 95% confidence interval.

R2E intensive care utilisation was <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|Without the rule|full -->93.0 [91.8, 94.2]<!-- /GEN --> without the rule and <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|With the rule|full -->87.8 [86.8, 88.9]<!-- /GEN --> with it, a paired difference of <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|Paired difference|full -->−5.1 [−6.7, −3.5]<!-- /GEN --> percentage points. Died of wounds per run were <!-- GEN cell:icu_gate|Died of wounds per run|Without the rule|full -->10.57 [9.67, 11.46]<!-- /GEN --> and <!-- GEN cell:icu_gate|Died of wounds per run|With the rule|full -->10.67 [9.33, 12.00]<!-- /GEN -->, a paired difference of <!-- GEN cell:icu_gate|Died of wounds per run|Paired difference|full -->+0.10 [−1.71, +1.91]<!-- /GEN -->.

<!-- GEN icu_gate_pathways -->
| Recovery pathway | Casualty-replications | Died of wounds | Rate |
|---|---|---|---|
| Intensive care bed | 18,455 | 15 | 0.08% |
| Holding bed | 18,731 | 34 | 0.18% |
<!-- /GEN -->

Where the rule was in force, casualties recovering in a holding bed died of wounds at <!-- GEN cell:icu_gate_pathways|Holding bed|Rate|full -->0.18%<!-- /GEN --> and those recovering in an intensive care bed at <!-- GEN cell:icu_gate_pathways|Intensive care bed|Rate|full -->0.08%<!-- /GEN -->, pooled over the replications of that arm.

### Evacuation Policy

**Question.** What happens to returns to duty, mortality and the R2E pools when the days a casualty may recover in theatre before evacuation is set across its doctrinal range? `[default, evacuation policy 15 to 60 days · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN policy -->
| Response | 15 d | 21 d (shipped) | 30 d | 45 d | 60 d |
|---|---|---|---|---|---|
| R2E hold occupancy (%) | 20.3 [19.0, 21.6] | 44.8 [42.6, 46.9] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] |
| R2E hold mean queue | 0.00 [0.00, 0.00] | 0.08 [0.00, 0.19] | 759.26 [706.25, 812.27] | 1833.35 [1776.97, 1889.73] | 1965.53 [1927.78, 2003.28] |
| R2E ICU occupancy (%) | 86.9 [85.7, 88.0] | 87.8 [86.8, 88.9] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] |
| R2E ICU mean queue | 0.82 [0.62, 1.01] | 1.35 [0.55, 2.16] | 113.29 [102.97, 123.62] | 44.09 [35.73, 52.44] | 24.23 [20.48, 27.98] |
| Post-definitive ICU access (%) | 33.5 [31.7, 35.3] | 31.7 [30.6, 32.8] | 3.4 [2.8, 4.0] | 4.5 [3.8, 5.2] | 6.0 [4.8, 7.2] |
| In-theatre share (%) | 5.2 [5.0, 5.4] | 12.2 [11.9, 12.5] | 29.8 [29.2, 30.4] | 66.7 [65.9, 67.6] | 87.1 [86.4, 87.7] |
| Returns to duty | 1476.0 [1452.3, 1499.8] | 1673.3 [1647.0, 1699.6] | 1875.3 [1855.2, 1895.4] | 1814.4 [1788.8, 1840.0] | 1724.5 [1698.2, 1750.8] |
| Died of wounds | 9.87 [8.47, 11.26] | 10.67 [9.33, 12.00] | 12.10 [10.73, 13.47] | 11.83 [10.64, 13.03] | 13.43 [11.91, 14.96] |
| Never evacuated by horizon | 2.9 [1.4, 4.5] | 1.5 [0.9, 2.1] | 440.8 [417.8, 463.8] | 367.8 [348.7, 387.0] | 157.9 [147.4, 168.4] |
| Mean evacuation wait (d) | 0.39 [0.24, 0.55] | 0.33 [0.28, 0.38] | 34.62 [32.17, 37.06] | 95.56 [91.73, 99.40] | 98.69 [92.01, 105.37] |
| Role 4 peak beds | 198.4 [190.8, 205.9] | 202.6 [193.6, 211.7] | 101.7 [96.6, 106.7] | 30.2 [26.5, 33.8] | 15.2 [12.3, 18.1] |
<!-- /GEN -->

![Six panels of returns to duty, deaths of wounds, in-theatre share and R2E queues against evacuation policy days](../images/policy_sweep.png)

Six responses against the evacuation policy in days, each with its 95% confidence interval and the shipped policy marked by a dashed line.

At the shipped 21-day policy the R2E holding queue was <!-- GEN cell:policy|R2E hold mean queue|21 d (shipped)|full -->0.08 [0.00, 0.19]<!-- /GEN --> and its occupancy <!-- GEN cell:policy|R2E hold occupancy (%)|21 d (shipped)|full -->44.8 [42.6, 46.9]<!-- /GEN -->. At 30 days the holding occupancy was <!-- GEN cell:policy|R2E hold occupancy (%)|30 d|full -->100.0 [100.0, 100.0]<!-- /GEN --> and the queue <!-- GEN cell:policy|R2E hold mean queue|30 d|full -->759.26 [706.25, 812.27]<!-- /GEN -->; at 45 and 60 days the occupancy was <!-- GEN cell:policy|R2E hold occupancy (%)|45 d|full -->100.0 [100.0, 100.0]<!-- /GEN --> and <!-- GEN cell:policy|R2E hold occupancy (%)|60 d|full -->100.0 [100.0, 100.0]<!-- /GEN -->. Returns to duty per campaign were <!-- GEN cell:policy|Returns to duty|15 d|full -->1476.0 [1452.3, 1499.8]<!-- /GEN --> at 15 days, <!-- GEN cell:policy|Returns to duty|21 d (shipped)|full -->1673.3 [1647.0, 1699.6]<!-- /GEN --> at 21, <!-- GEN cell:policy|Returns to duty|30 d|full -->1875.3 [1855.2, 1895.4]<!-- /GEN --> at 30, <!-- GEN cell:policy|Returns to duty|45 d|full -->1814.4 [1788.8, 1840.0]<!-- /GEN --> at 45 and <!-- GEN cell:policy|Returns to duty|60 d|full -->1724.5 [1698.2, 1750.8]<!-- /GEN --> at 60. Died of wounds were <!-- GEN cell:policy|Died of wounds|15 d|full -->9.87 [8.47, 11.26]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|21 d (shipped)|full -->10.67 [9.33, 12.00]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|30 d|full -->12.10 [10.73, 13.47]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|45 d|full -->11.83 [10.64, 13.03]<!-- /GEN --> and <!-- GEN cell:policy|Died of wounds|60 d|full -->13.43 [11.91, 14.96]<!-- /GEN --> across the five policies. The realised in-theatre share was <!-- GEN cell:policy|In-theatre share (%)|15 d|full -->5.2 [5.0, 5.4]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|21 d (shipped)|full -->12.2 [11.9, 12.5]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|30 d|full -->29.8 [29.2, 30.4]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|45 d|full -->66.7 [65.9, 67.6]<!-- /GEN --> and <!-- GEN cell:policy|In-theatre share (%)|60 d|full -->87.1 [86.4, 87.7]<!-- /GEN --> percent. Paired differences against the shipped policy are in `data/policy/policy_sweep_paired.csv`.

### R2E Holding Establishment

**Question.** What does enlarging the R2E holding establishment change at the shipped evacuation policy? `[default, 21-day policy, 30 to 90 holding beds · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN establishment -->
| Response | 30 beds (shipped) | 45 beds | 60 beds | 90 beds |
|---|---|---|---|---|
| R2E hold occupancy (%) | 44.8 [42.6, 46.9] | 29.7 [27.6, 31.7] | 24.1 [19.8, 28.3] | 14.6 [13.8, 15.4] |
| R2E hold mean queue | 0.08 [0.00, 0.19] | 0.07 [0.00, 0.20] | 0.56 [0.00, 1.70] | 0.00 [0.00, 0.00] |
| R2E ICU mean queue | 1.35 [0.55, 2.16] | 0.73 [0.52, 0.93] | 1.98 [0.00, 4.16] | 0.87 [0.62, 1.11] |
| Post-definitive ICU access (%) | 31.7 [30.6, 32.8] | 32.1 [30.0, 34.1] | 31.0 [28.7, 33.2] | 32.0 [30.8, 33.3] |
| In-theatre share (%) | 12.2 [11.9, 12.5] | 12.2 [11.9, 12.4] | 12.1 [11.9, 12.3] | 12.1 [11.8, 12.3] |
| Returns to duty | 1673.3 [1647.0, 1699.6] | 1692.8 [1662.3, 1723.3] | 1693.0 [1665.1, 1720.9] | 1685.8 [1655.3, 1716.3] |
| Died of wounds | 10.7 [9.3, 12.0] | 11.2 [9.6, 12.7] | 11.3 [10.1, 12.6] | 11.0 [9.9, 12.1] |
| Never evacuated by horizon | 1.5 [0.9, 2.1] | 1.1 [0.5, 1.6] | 2.6 [0.3, 4.9] | 1.6 [0.7, 2.5] |
| Role 4 peak beds | 202.6 [193.6, 211.7] | 202.9 [193.8, 212.0] | 208.2 [201.5, 214.8] | 206.5 [199.8, 213.2] |
<!-- /GEN -->

![Six panels of campaign responses against R2E holding beds](../images/establishment_sweep.png)

The same six responses against the R2E holding bed establishment at the shipped evacuation policy, with the shipped establishment marked by a dashed line.

Holding occupancy was <!-- GEN cell:establishment|R2E hold occupancy (%)|30 beds (shipped)|full -->44.8 [42.6, 46.9]<!-- /GEN --> at the shipped 30 beds, <!-- GEN cell:establishment|R2E hold occupancy (%)|45 beds|full -->29.7 [27.6, 31.7]<!-- /GEN --> at 45, <!-- GEN cell:establishment|R2E hold occupancy (%)|60 beds|full -->24.1 [19.8, 28.3]<!-- /GEN --> at 60 and <!-- GEN cell:establishment|R2E hold occupancy (%)|90 beds|full -->14.6 [13.8, 15.4]<!-- /GEN --> at 90 percent. The R2E holding queue was <!-- GEN cell:establishment|R2E hold mean queue|30 beds (shipped)|full -->0.08 [0.00, 0.19]<!-- /GEN --> at 30 beds and <!-- GEN cell:establishment|R2E hold mean queue|45 beds|full -->0.07 [0.00, 0.20]<!-- /GEN --> at 45. Returns to duty per campaign were <!-- GEN cell:establishment|Returns to duty|30 beds (shipped)|full -->1673.3 [1647.0, 1699.6]<!-- /GEN --> at 30 beds and <!-- GEN cell:establishment|Returns to duty|90 beds|full -->1685.8 [1655.3, 1716.3]<!-- /GEN --> at 90, and died of wounds <!-- GEN cell:establishment|Died of wounds|30 beds (shipped)|full -->10.7 [9.3, 12.0]<!-- /GEN --> and <!-- GEN cell:establishment|Died of wounds|90 beds|full -->11.0 [9.9, 12.1]<!-- /GEN -->.

### Forward Surgical Saturation Release

**Question.** What changes when casualties are released to strategic evacuation with the definitive repair outstanding once the R2E theatre queue reaches a threshold? `[default, threshold 0 to 24 casualties · 360 d · 30 replications · paired, closing 90 d]`

<!-- GEN saturation -->
| Response | 0 (disabled) | 1 | 2 | 3 | 5 | 8 (shipped) | 12 | 16 | 24 |
|---|---|---|---|---|---|---|---|---|---|
| Theatre mean queue | 8.32 [5.23, 11.42] | 2.28 [1.40, 3.16] | 2.30 [1.34, 3.26] | 2.62 [1.14, 4.09] | 4.84 [1.09, 8.60] | 4.92 [1.93, 7.91] | 2.77 [2.09, 3.44] | 3.66 [2.79, 4.53] | 6.90 [3.71, 10.08] |
| Released with repair outstanding | 0.0 [0.0, 0.0] | 296.0 [272.0, 319.9] | 228.5 [206.6, 250.5] | 216.0 [193.6, 238.4] | 175.8 [156.8, 194.8] | 146.0 [125.8, 166.3] | 93.0 [77.0, 108.9] | 77.7 [61.1, 94.3] | 55.8 [40.2, 71.5] |
| Role 4 operations owed | 931.4 [898.0, 964.8] | 1298.6 [1234.3, 1362.8] | 1181.5 [1131.2, 1231.7] | 1207.9 [1150.4, 1265.5] | 1110.3 [1058.6, 1162.0] | 1141.1 [1088.4, 1193.8] | 1025.3 [978.2, 1072.5] | 1036.1 [990.1, 1082.1] | 995.4 [956.4, 1034.3] |
| Post-definitive ICU access (%) | 34.8 [33.8, 35.7] | 28.8 [27.2, 30.3] | 30.4 [28.0, 32.8] | 30.9 [29.6, 32.2] | 31.8 [30.3, 33.3] | 31.7 [30.6, 32.8] | 33.2 [31.7, 34.8] | 32.6 [31.4, 33.9] | 33.5 [32.2, 34.8] |
| Died of wounds | 10.7 [9.5, 11.9] | 11.2 [9.6, 12.8] | 10.6 [9.1, 12.1] | 11.0 [9.5, 12.4] | 11.8 [10.5, 13.1] | 10.7 [9.3, 12.0] | 10.9 [9.7, 12.0] | 12.1 [10.7, 13.5] | 10.5 [9.3, 11.7] |
| Returns to duty | 1670.9 [1646.1, 1695.8] | 1670.5 [1644.8, 1696.3] | 1672.7 [1644.1, 1701.2] | 1668.2 [1641.7, 1694.6] | 1674.5 [1650.2, 1698.8] | 1673.3 [1647.0, 1699.6] | 1664.1 [1637.7, 1690.6] | 1697.7 [1671.2, 1724.2] | 1674.8 [1645.4, 1704.2] |
<!-- /GEN -->

![Six panels of campaign responses against the forward surgical saturation release threshold](../images/saturation_sweep.png)

The same six responses against the R2E theatre queue length at which a casualty is released, with the shipped threshold marked by a dashed line.

With the release disabled the closing-window theatre queue was <!-- GEN cell:saturation|Theatre mean queue|0 (disabled)|full -->8.32 [5.23, 11.42]<!-- /GEN --> casualties and no casualty was released with the repair outstanding. At the shipped threshold of eight the queue was <!-- GEN cell:saturation|Theatre mean queue|8 (shipped)|full -->4.92 [1.93, 7.91]<!-- /GEN --> and <!-- GEN cell:saturation|Released with repair outstanding|8 (shipped)|full -->146.0 [125.8, 166.3]<!-- /GEN --> casualties per campaign were released with the repair outstanding, with <!-- GEN cell:saturation|Role 4 operations owed|8 (shipped)|full -->1141.1 [1088.4, 1193.8]<!-- /GEN --> operations owed at the national support base against <!-- GEN cell:saturation|Role 4 operations owed|0 (disabled)|full -->931.4 [898.0, 964.8]<!-- /GEN --> with the release disabled. Returns to duty and died of wounds per campaign were <!-- GEN cell:saturation|Returns to duty|8 (shipped)|full -->1673.3 [1647.0, 1699.6]<!-- /GEN --> and <!-- GEN cell:saturation|Died of wounds|8 (shipped)|full -->10.7 [9.3, 12.0]<!-- /GEN --> at the shipped threshold and <!-- GEN cell:saturation|Returns to duty|0 (disabled)|full -->1670.9 [1646.1, 1695.8]<!-- /GEN --> and <!-- GEN cell:saturation|Died of wounds|0 (disabled)|full -->10.7 [9.5, 11.9]<!-- /GEN --> with the release disabled.

---

## National Support Base and Strategic Airlift

<small>[Return to Top](#contents)</small>

**Question.** How does strategic evacuation demand and its wait respond to the shipped airlift schedule, to the interval between sorties and to sortie cancellation, and what census does the national support base carry? `[default, both intensities and swept values · 360 d · 30 replications · per-replication reductions]`

<!-- GEN airlift_baseline -->
| Response at the shipped schedule | Moderate intensity | High intensity |
|---|---|---|
| Casualties boarded | 2786.90 [2735.91, 2837.89] | 3366.73 [3344.71, 3388.76] |
| Still waiting at the close | 1.50 [0.89, 2.11] | 1924.43 [1904.07, 1944.80] |
| Mean wait (days) | 0.33 [0.28, 0.38] | 18.57 [18.04, 19.10] |
| Share of R2E holding beds held by the evacuation wait | 4% [3%, 5%] | 38% [37%, 39%] |
| Role 4 peak occupancy (concurrent patients) | 202.63 [193.59, 211.68] | 215.77 [214.00, 217.53] |
| Days the peak falls before the campaign ends | 133.80 [97.30, 170.30] | 159.90 [119.69, 200.11] |
<!-- /GEN -->

At the shipped schedule <!-- GEN cell:airlift_baseline|Casualties boarded|Moderate intensity|mean -->2786.90<!-- /GEN --> casualties boarded at moderate intensity and <!-- GEN cell:airlift_baseline|Casualties boarded|High intensity|mean -->3366.73<!-- /GEN --> at high, with <!-- GEN cell:airlift_baseline|Still waiting at the close|Moderate intensity|mean -->1.50<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Still waiting at the close|High intensity|mean -->1924.43<!-- /GEN --> still waiting at the close and mean waits of <!-- GEN cell:airlift_baseline|Mean wait (days)|Moderate intensity|mean -->0.33<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Mean wait (days)|High intensity|mean -->18.57<!-- /GEN --> days. The evacuation wait held <!-- GEN cell:airlift_baseline|Share of R2E holding beds held by the evacuation wait|Moderate intensity|full -->4% [3%, 5%]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Share of R2E holding beds held by the evacuation wait|High intensity|full -->38% [37%, 39%]<!-- /GEN --> of the R2E holding beds. Role 4 peak occupancy was <!-- GEN cell:airlift_baseline|Role 4 peak occupancy (concurrent patients)|Moderate intensity|full -->202.63 [193.59, 211.68]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Role 4 peak occupancy (concurrent patients)|High intensity|full -->215.77 [214.00, 217.53]<!-- /GEN --> concurrent patients, and the peak fell <!-- GEN cell:airlift_baseline|Days the peak falls before the campaign ends|Moderate intensity|full -->133.80 [97.30, 170.30]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Days the peak falls before the campaign ends|High intensity|full -->159.90 [119.69, 200.11]<!-- /GEN --> days before the campaign ended.

<!-- GEN airlift_interval -->
| Response by interval between sorties | 3 days | 5 days | 7 days (shipped) | 10 days | 14 days |
|---|---|---|---|---|---|
| Sorties flown | 119.00 | 71.00 | 51.00 | 35.00 | 25.00 |
| Mean wait (days) | 0.23 [0.20, 0.26] | 0.24 [0.21, 0.27] | 0.33 [0.28, 0.38] | 9.30 [7.77, 10.83] | 26.84 [25.42, 28.25] |
| Share of R2E holding beds held by the evacuation wait | 0% [0%, 1%] | 2% [1%, 2%] | 4% [3%, 5%] | 53% [49%, 57%] | 64% [64%, 65%] |
| Ventilated pre-flight intensive care hold (hours) | 24.15 [23.91, 24.39] | 24.40 [24.14, 24.66] | 25.17 [24.50, 25.83] | 135.73 [117.75, 153.72] | 423.31 [395.55, 451.07] |
<!-- /GEN -->

Shortening the interval between sorties below the shipped seven days left the mean wait at <!-- GEN cell:airlift_interval|Mean wait (days)|3 days|mean -->0.23<!-- /GEN --> days at three days and <!-- GEN cell:airlift_interval|Mean wait (days)|5 days|mean -->0.24<!-- /GEN --> at five against <!-- GEN cell:airlift_interval|Mean wait (days)|7 days (shipped)|mean -->0.33<!-- /GEN --> at seven. Lengthening it gave <!-- GEN cell:airlift_interval|Mean wait (days)|10 days|full -->9.30 [7.77, 10.83]<!-- /GEN --> days at ten days with <!-- GEN cell:airlift_interval|Sorties flown|10 days|full -->35.00<!-- /GEN --> sorties flown and <!-- GEN cell:airlift_interval|Mean wait (days)|14 days|full -->26.84 [25.42, 28.25]<!-- /GEN --> days at fourteen with <!-- GEN cell:airlift_interval|Sorties flown|14 days|full -->25.00<!-- /GEN -->.

<!-- GEN airlift_reliability -->
| Response by configured cancellation probability | 0% | 5% | 10% | 15% | 25% | 40% |
|---|---|---|---|---|---|---|
| Sorties flown | 51.00 | 48.40 | 45.03 | 43.80 | 37.87 | 31.43 |
| Realised cancellation rate | 0% | 5% | 12% | 14% | 26% | 38% |
| Mean wait (days) | 0.33 [0.28, 0.38] | 0.83 [0.22, 1.44] | 1.46 [0.36, 2.56] | 2.30 [0.85, 3.76] | 7.78 [5.43, 10.14] | 15.81 [12.91, 18.70] |
| Share of R2E holding beds held by the evacuation wait | 4% [3%, 5%] | 8% [4%, 12%] | 13% [8%, 19%] | 20% [13%, 26%] | 43% [36%, 50%] | 57% [54%, 60%] |
<!-- /GEN -->

![Eight panels of strategic evacuation responses against sortie cancellation probability and the interval between sorties](../images/airlift_sweeps.png)

Wait for a sortie, casualties still waiting at the horizon, Role 4 peak beds and the share of holding bed-days spent awaiting a sortie against sortie cancellation probability and the interval between sorties, with the shipped value of each marked by a dashed line.

The mean wait rose from <!-- GEN cell:airlift_reliability|Mean wait (days)|0%|mean -->0.33<!-- /GEN --> days with no cancellation to <!-- GEN cell:airlift_reliability|Mean wait (days)|10%|mean -->1.46<!-- /GEN --> at 10%, <!-- GEN cell:airlift_reliability|Mean wait (days)|25%|mean -->7.78<!-- /GEN --> at 25% and <!-- GEN cell:airlift_reliability|Mean wait (days)|40%|mean -->15.81<!-- /GEN --> at 40%, as the sorties flown fell from <!-- GEN cell:airlift_reliability|Sorties flown|0%|full -->51.00<!-- /GEN --> to <!-- GEN cell:airlift_reliability|Sorties flown|40%|full -->31.43<!-- /GEN -->.

**Collapse classification.** A campaign was classified as collapsed where its R2E holding queue over the closing 90 days averaged twenty casualties or more. `[default, sortie cancellation 0 to 25% · 360 d · 30 replications · exact binomial]`

<!-- GEN airlift_collapse -->
| Sortie cancellation | Collapsed | Collapse rate (exact 95% interval) | Median closing-window queue | Worst closing-window queue |
|---|---|---|---|---|
| 0% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.00 | 1.48 |
| 5% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.00 | 8.32 |
| 10% | 2 of 30 | 6.7% [0.8%, 22.1%] | 0.00 | 32.50 |
| 15% | 4 of 30 | 13.3% [3.8%, 30.7%] | 0.00 | 86.28 |
| 20% | 11 of 30 | 36.7% [19.9%, 56.1%] | 12.61 | 297.37 |
| 25% | 22 of 30 | 73.3% [54.1%, 87.7%] | 64.90 | 346.99 |
<!-- /GEN -->

![Line plot of the share of campaigns collapsing against sortie cancellation probability](../images/airlift_collapse.png)

The share of campaigns classified as collapsed against sortie cancellation probability, with exact binomial 95% confidence intervals.

No campaign collapsed at <!-- GEN cell:airlift_collapse|0%|Sortie cancellation|full -->0%<!-- /GEN --> or 5% cancellation. <!-- GEN cell:airlift_collapse|10%|Collapsed|full -->2 of 30<!-- /GEN --> collapsed at 10% cancellation, <!-- GEN cell:airlift_collapse|15%|Collapsed|full -->4 of 30<!-- /GEN --> at 15%, <!-- GEN cell:airlift_collapse|20%|Collapsed|full -->11 of 30<!-- /GEN --> at 20% and <!-- GEN cell:airlift_collapse|25%|Collapsed|full -->22 of 30<!-- /GEN --> at 25%.

---

## Role 4 Bed Demand

<small>[Return to Top](#contents)</small>

**Question.** How many national support base beds does a campaign commit, what is the demand made of, has it settled by the closing window, and how do the forward evacuation levers move it? `[default, both intensities, the evacuation policy, R2E holding establishment, saturation release and sortie cancellation sweeps · 360 d · 30 replications · daily census, mean, peak and closing 90 d]`

The model gives the national support base no capacity, queue or shortfall, so every figure here is demand: the casualties in a Role 4 bed on a day, counted as the other sections count the Role 4 peak. The mean is taken over the whole campaign, the peak is the busiest day of one campaign, and the closing mean is the mean over the last 90 days. A casualty is counted by the ward phase it is in, intensive care or step-down, and by its origin, battle injury, disease and non-battle injury or the reconstruction cohort.

<!-- GEN role4_census_ward -->
| Census by ward phase | Moderate intensity | High intensity |
|---|---|---|
| Total, mean beds | 127.86 [124.86, 130.87] | 171.47 [170.58, 172.36] |
| Total, peak beds | 202.63 [193.59, 211.68] | 215.77 [214.00, 217.53] |
| Total, closing 90-day mean beds | 136.89 [130.59, 143.20] | 179.53 [177.80, 181.25] |
| Intensive care phase, mean beds | 28.86 [27.95, 29.76] | 39.27 [39.03, 39.50] |
| Intensive care phase, peak beds | 64.30 [61.30, 67.30] | 61.83 [61.17, 62.49] |
| Intensive care phase, closing 90-day mean beds | 30.71 [28.59, 32.83] | 39.96 [39.59, 40.33] |
| Step-down ward phase, mean beds | 99.01 [96.87, 101.14] | 132.21 [131.35, 133.06] |
| Step-down ward phase, peak beds | 158.37 [151.82, 164.92] | 167.80 [165.79, 169.81] |
| Step-down ward phase, closing 90-day mean beds | 106.18 [101.90, 110.46] | 139.56 [137.89, 141.23] |
<!-- /GEN -->

At moderate intensity the census averaged <!-- GEN cell:role4_census_ward|Total, mean beds|Moderate intensity|full -->127.86 [124.86, 130.87]<!-- /GEN --> beds over the campaign, peaked at <!-- GEN cell:role4_census_ward|Total, peak beds|Moderate intensity|full -->202.63 [193.59, 211.68]<!-- /GEN --> and averaged <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|Moderate intensity|full -->136.89 [130.59, 143.20]<!-- /GEN --> over the closing 90 days. At high intensity the three figures were <!-- GEN cell:role4_census_ward|Total, mean beds|High intensity|full -->171.47 [170.58, 172.36]<!-- /GEN -->, <!-- GEN cell:role4_census_ward|Total, peak beds|High intensity|full -->215.77 [214.00, 217.53]<!-- /GEN --> and <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|High intensity|full -->179.53 [177.80, 181.25]<!-- /GEN -->. The closing mean exceeded the campaign mean at both intensities. The step-down ward phase carried <!-- GEN cell:role4_census_ward|Step-down ward phase, mean beds|Moderate intensity|mean -->99.01<!-- /GEN --> of the <!-- GEN cell:role4_census_ward|Total, mean beds|Moderate intensity|mean -->127.86<!-- /GEN --> mean beds at moderate intensity and <!-- GEN cell:role4_census_ward|Step-down ward phase, mean beds|High intensity|mean -->132.21<!-- /GEN --> of the <!-- GEN cell:role4_census_ward|Total, mean beds|High intensity|mean -->171.47<!-- /GEN --> at high, and the intensive care phase the remainder.

![Daily Role 4 census over a 360-day campaign at each casualty intensity, divided by ward phase and by origin](../images/role4_demand_census.png)

The mean daily census across replications with its 95% interval, at each casualty intensity, divided by ward phase (upper row) and by origin (lower row), each beside the total it sums to. At high intensity the census rises and falls in a regular cycle about its mean.

<!-- GEN role4_census_origin -->
| Census by origin | Moderate intensity | High intensity |
|---|---|---|
| Total, mean beds | 127.86 [124.86, 130.87] | 171.47 [170.58, 172.36] |
| Total, peak beds | 202.63 [193.59, 211.68] | 215.77 [214.00, 217.53] |
| Total, closing 90-day mean beds | 136.89 [130.59, 143.20] | 179.53 [177.80, 181.25] |
| Battle injury, mean beds | 88.44 [85.87, 91.00] | 130.94 [129.99, 131.89] |
| Battle injury, peak beds | 148.00 [140.72, 155.28] | 169.83 [167.82, 171.84] |
| Battle injury, closing 90-day mean beds | 95.84 [90.15, 101.52] | 137.18 [135.36, 138.99] |
| Disease and non-battle injury, mean beds | 20.35 [19.94, 20.75] | 15.29 [14.94, 15.65] |
| Disease and non-battle injury, peak beds | 42.27 [40.35, 44.18] | 30.27 [29.16, 31.37] |
| Disease and non-battle injury, closing 90-day mean beds | 20.35 [19.62, 21.09] | 15.31 [14.67, 15.95] |
| Reconstruction cohort, mean beds | 19.08 [18.43, 19.73] | 25.24 [24.77, 25.71] |
| Reconstruction cohort, peak beds | 35.00 [33.21, 36.79] | 39.33 [38.20, 40.47] |
| Reconstruction cohort, closing 90-day mean beds | 20.70 [19.64, 21.76] | 27.04 [26.24, 27.84] |
<!-- /GEN -->

Battle injury accounted for <!-- GEN cell:role4_census_origin|Battle injury, mean beds|Moderate intensity|mean -->88.44<!-- /GEN --> of the mean beds at moderate intensity and <!-- GEN cell:role4_census_origin|Battle injury, mean beds|High intensity|mean -->130.94<!-- /GEN --> at high. Disease and non-battle injury accounted for <!-- GEN cell:role4_census_origin|Disease and non-battle injury, mean beds|Moderate intensity|mean -->20.35<!-- /GEN --> and <!-- GEN cell:role4_census_origin|Disease and non-battle injury, mean beds|High intensity|mean -->15.29<!-- /GEN -->, and the reconstruction cohort for <!-- GEN cell:role4_census_origin|Reconstruction cohort, mean beds|Moderate intensity|mean -->19.08<!-- /GEN --> and <!-- GEN cell:role4_census_origin|Reconstruction cohort, mean beds|High intensity|mean -->25.24<!-- /GEN -->.

<!-- GEN role4_operations -->
| Demand owed alongside the census | Moderate intensity | High intensity |
|---|---|---|
| Operations owed within the 360 days | 1,123 [1,073, 1,174] | 1,373 [1,347, 1,398] |
| Definitive repairs owed within the 360 days | 145 [125, 166] | 99 [94, 104] |
| Debridements owed within the 360 days | 672 [644, 699] | 905 [885, 925] |
| Reconstructions owed within the 360 days | 306 [297, 315] | 368 [363, 374] |
| Operations owed after day 360 | 18 [13, 22] | 22 [19, 24] |
| Operations owed by casualties admitted during the campaign | 1,141 [1,088, 1,194] | 1,394 [1,368, 1,420] |
| Operations owed on the busiest day | 11.30 [10.67, 11.93] | 13.47 [12.96, 13.97] |
| Closing 90-day mean operations owed per day | 3.36 [3.05, 3.67] | 3.77 [3.62, 3.91] |
| Theatre minutes owed for definitive repairs | 16,793 [14,494, 19,093] | 11,400 [10,827, 11,973] |
<!-- /GEN -->

The operations owed alongside the census were counted three ways. <!-- GEN cell:role4_operations|Operations owed within the 360 days|Moderate intensity|mean -->1,123<!-- /GEN --> fell inside the campaign at moderate intensity and <!-- GEN cell:role4_operations|Operations owed within the 360 days|High intensity|mean -->1,373<!-- /GEN --> at high, a further <!-- GEN cell:role4_operations|Operations owed after day 360|Moderate intensity|mean -->18<!-- /GEN --> and <!-- GEN cell:role4_operations|Operations owed after day 360|High intensity|mean -->22<!-- /GEN --> fell after the campaign ended, in the reconstruction sequences of casualties admitted late, and <!-- GEN cell:role4_operations|Operations owed by casualties admitted during the campaign|Moderate intensity|mean -->1,141<!-- /GEN --> and <!-- GEN cell:role4_operations|Operations owed by casualties admitted during the campaign|High intensity|mean -->1,394<!-- /GEN --> were owed in all, the figure the other sections report as operations owed. By source, the operations inside the campaign were <!-- GEN cell:role4_operations|Definitive repairs owed within the 360 days|Moderate intensity|mean -->145<!-- /GEN --> definitive repairs, <!-- GEN cell:role4_operations|Debridements owed within the 360 days|Moderate intensity|mean -->672<!-- /GEN --> debridements and <!-- GEN cell:role4_operations|Reconstructions owed within the 360 days|Moderate intensity|mean -->306<!-- /GEN --> reconstructions at moderate intensity, and <!-- GEN cell:role4_operations|Definitive repairs owed within the 360 days|High intensity|mean -->99<!-- /GEN -->, <!-- GEN cell:role4_operations|Debridements owed within the 360 days|High intensity|mean -->905<!-- /GEN --> and <!-- GEN cell:role4_operations|Reconstructions owed within the 360 days|High intensity|mean -->368<!-- /GEN --> at high. The busiest day owed <!-- GEN cell:role4_operations|Operations owed on the busiest day|Moderate intensity|mean -->11.30<!-- /GEN --> operations at moderate intensity and <!-- GEN cell:role4_operations|Operations owed on the busiest day|High intensity|mean -->13.47<!-- /GEN --> at high. The theatre minutes cover the definitive repairs only, no source reporting the time a debridement or a flap takes.

![Mean operations owed per day at Role 4 over each week of a 360-day campaign at each casualty intensity, by source and in all](../images/role4_demand_operations.png)

The mean operations owed per day over each week of the campaign, with the 95% interval across replications, at each casualty intensity, for the definitive repairs, the debridements and the reconstructions and for their total.

<!-- GEN role4_stability -->
| Census | Moderate intensity classification | Moderate intensity settles by block | Moderate intensity late mean beds | High intensity classification | High intensity settles by block | High intensity late mean beds |
|---|---|---|---|---|---|---|
| Total | converged | 2 | 134.12 | converged | 5 | 179.19 |
| Intensive care phase | converged | 2 | 29.71 | converged | 2 | 40.07 |
| Step-down ward phase | converged | 2 | 104.41 | converged | 5 | 139.12 |
| Battle injury | converged | 2 | 93.49 | converged | 5 | 137.13 |
| Disease and non-battle injury | converged | 2 | 20.50 | converged | 2 | 15.62 |
| Reconstruction cohort | converged | 2 | 20.12 | drifting | none | 26.44 |
<!-- /GEN -->

<!-- GEN role4_cma -->
| Cumulative moving average of the mean total census (beds) | Moderate intensity | High intensity |
|---|---|---|
| Day 30 | 68.97 | 91.54 |
| Day 90 | 111.76 | 149.00 |
| Day 180 | 122.25 | 163.72 |
| Day 360 | 127.86 | 171.47 |
<!-- /GEN -->

The 30-day block means of the total census were classified <!-- GEN cell:role4_stability|Total|Moderate intensity classification|full -->converged<!-- /GEN --> at moderate intensity, settling by block <!-- GEN cell:role4_stability|Total|Moderate intensity settles by block|full -->2<!-- /GEN -->, and <!-- GEN cell:role4_stability|Total|High intensity classification|full -->converged<!-- /GEN --> at high, settling by block <!-- GEN cell:role4_stability|Total|High intensity settles by block|full -->5<!-- /GEN -->. The reconstruction cohort was classified <!-- GEN cell:role4_stability|Reconstruction cohort|Moderate intensity classification|full -->converged<!-- /GEN --> at moderate intensity and <!-- GEN cell:role4_stability|Reconstruction cohort|High intensity classification|full -->drifting<!-- /GEN --> at high. The cumulative moving average of the mean total census was <!-- GEN cell:role4_cma|Day 90|Moderate intensity|full -->111.76<!-- /GEN --> beds at day 90 and <!-- GEN cell:role4_cma|Day 360|Moderate intensity|full -->127.86<!-- /GEN --> at day 360 at moderate intensity, and <!-- GEN cell:role4_cma|Day 90|High intensity|full -->149.00<!-- /GEN --> and <!-- GEN cell:role4_cma|Day 360|High intensity|full -->171.47<!-- /GEN --> at high.

**Response to the forward levers.** The three sweeps below carry the Role 4 peak, the closing 90-day mean and the operations owed by the casualties admitted in their own evidence sets, and are read from them without a further run; the sortie cancellation sweep was measured for this section, the strategic airlift evidence set carrying the peak alone. Each table's shipped column is the configuration of the census tables above at moderate intensity.

<!-- GEN role4_levers_policy -->
| Response | 15 d | 21 d (shipped) | 30 d | 45 d | 60 d |
|---|---|---|---|---|---|
| Role 4 peak beds | 198.4 [190.8, 205.9] | 202.6 [193.6, 211.7] | 101.7 [96.6, 106.7] | 30.2 [26.5, 33.8] | 15.2 [12.3, 18.1] |
| Role 4 closing 90-day mean beds | 106.5 [101.9, 111.0] | 109.0 [104.0, 114.1] | 50.9 [48.1, 53.7] | 8.2 [7.3, 9.0] | 2.7 [2.4, 3.0] |
| Role 4 operations owed | 1047 [995, 1098] | 1141 [1088, 1194] | 502 [471, 533] | 92 [81, 103] | 48 [42, 55] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|15 d|mean -->106.5<!-- /GEN --> beds at 15 days, <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|21 d (shipped)|mean -->109.0<!-- /GEN --> at the shipped 21 days, <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|30 d|mean -->50.9<!-- /GEN --> at 30, <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|45 d|mean -->8.2<!-- /GEN --> at 45 and <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|60 d|mean -->2.7<!-- /GEN --> at 60. The peak was <!-- GEN cell:role4_levers_policy|Role 4 peak beds|21 d (shipped)|mean -->202.6<!-- /GEN --> at 21 days and <!-- GEN cell:role4_levers_policy|Role 4 peak beds|30 d|mean -->101.7<!-- /GEN -->, <!-- GEN cell:role4_levers_policy|Role 4 peak beds|45 d|mean -->30.2<!-- /GEN --> and <!-- GEN cell:role4_levers_policy|Role 4 peak beds|60 d|mean -->15.2<!-- /GEN --> at 30, 45 and 60. The operations owed were <!-- GEN cell:role4_levers_policy|Role 4 operations owed|21 d (shipped)|mean -->1141<!-- /GEN --> at 21 days and <!-- GEN cell:role4_levers_policy|Role 4 operations owed|60 d|mean -->48<!-- /GEN --> at 60.

<!-- GEN role4_levers_establishment -->
| Response | 30 beds (shipped) | 45 beds | 60 beds | 90 beds |
|---|---|---|---|---|
| Role 4 peak beds | 202.6 [193.6, 211.7] | 202.9 [193.8, 212.0] | 208.2 [201.5, 214.8] | 206.5 [199.8, 213.2] |
| Role 4 closing 90-day mean beds | 109.0 [104.0, 114.1] | 102.3 [97.5, 107.1] | 106.7 [101.8, 111.7] | 105.9 [101.8, 110.1] |
| Role 4 operations owed | 1141 [1088, 1194] | 1103 [1050, 1156] | 1138 [1085, 1192] | 1123 [1077, 1169] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|30 beds (shipped)|mean -->109.0<!-- /GEN --> beds at the shipped 30 R2E holding beds and <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|45 beds|mean -->102.3<!-- /GEN -->, <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|60 beds|mean -->106.7<!-- /GEN --> and <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|90 beds|mean -->105.9<!-- /GEN --> at 45, 60 and 90; the four intervals overlap. The peak was <!-- GEN cell:role4_levers_establishment|Role 4 peak beds|30 beds (shipped)|mean -->202.6<!-- /GEN --> at 30 beds and <!-- GEN cell:role4_levers_establishment|Role 4 peak beds|90 beds|mean -->206.5<!-- /GEN --> at 90.

<!-- GEN role4_levers_saturation -->
| Response | 0 (disabled) | 1 | 2 | 3 | 5 | 8 (shipped) | 12 | 16 | 24 |
|---|---|---|---|---|---|---|---|---|---|
| Role 4 peak beds | 191.9 [185.6, 198.1] | 205.9 [199.3, 212.4] | 198.2 [190.0, 206.4] | 199.7 [191.3, 208.1] | 199.1 [192.1, 206.1] | 202.6 [193.6, 211.7] | 193.8 [185.5, 202.0] | 199.6 [191.9, 207.3] | 205.6 [195.8, 215.3] |
| Role 4 closing 90-day mean beds | 105.9 [100.8, 110.9] | 107.1 [101.4, 112.8] | 105.1 [100.6, 109.5] | 107.0 [101.6, 112.4] | 105.6 [100.3, 111.0] | 109.0 [104.0, 114.1] | 101.2 [97.2, 105.3] | 103.2 [98.7, 107.8] | 106.2 [99.9, 112.4] |
| Role 4 operations owed | 931 [898, 965] | 1299 [1234, 1363] | 1181 [1131, 1232] | 1208 [1150, 1265] | 1110 [1059, 1162] | 1141 [1088, 1194] | 1025 [978, 1072] | 1036 [990, 1082] | 995 [956, 1034] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_saturation|Role 4 closing 90-day mean beds|0 (disabled)|mean -->105.9<!-- /GEN --> beds with the release disabled and <!-- GEN cell:role4_levers_saturation|Role 4 closing 90-day mean beds|8 (shipped)|mean -->109.0<!-- /GEN --> at the shipped threshold of 8, and ranged across the nine thresholds without a monotone course. The operations owed were <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|0 (disabled)|mean -->931<!-- /GEN --> with the release disabled, <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|1|mean -->1299<!-- /GEN --> at a threshold of 1, <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|8 (shipped)|mean -->1141<!-- /GEN --> at 8 and <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|24|mean -->995<!-- /GEN --> at 24.

<!-- GEN role4_levers_cancellation -->
| Response | 0% (shipped) | 5% | 10% | 15% | 25% | 40% |
|---|---|---|---|---|---|---|
| Role 4 peak beds | 202.6 [193.6, 211.7] | 202.3 [194.0, 210.5] | 189.9 [182.4, 197.4] | 191.7 [187.1, 196.3] | 188.5 [182.1, 194.9] | 177.1 [173.6, 180.6] |
| Role 4 closing 90-day mean beds | 136.9 [130.6, 143.2] | 136.2 [129.0, 143.3] | 129.1 [123.2, 135.0] | 132.6 [128.1, 137.0] | 119.1 [112.4, 125.8] | 100.6 [94.1, 107.2] |
| Role 4 operations owed | 1141 [1088, 1194] | 1128 [1070, 1186] | 1079 [1030, 1129] | 1092 [1050, 1134] | 984 [940, 1029] | 790 [756, 823] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|0% (shipped)|mean -->136.9<!-- /GEN --> beds with no sortie cancelled and <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|10%|mean -->129.1<!-- /GEN -->, <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|25%|mean -->119.1<!-- /GEN --> and <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|40%|mean -->100.6<!-- /GEN --> at cancellation probabilities of 10%, 25% and 40%. The peak was <!-- GEN cell:role4_levers_cancellation|Role 4 peak beds|0% (shipped)|mean -->202.6<!-- /GEN --> with no cancellation and <!-- GEN cell:role4_levers_cancellation|Role 4 peak beds|40%|mean -->177.1<!-- /GEN --> at 40%, and the operations owed were <!-- GEN cell:role4_levers_cancellation|Role 4 operations owed|0% (shipped)|mean -->1141<!-- /GEN --> and <!-- GEN cell:role4_levers_cancellation|Role 4 operations owed|40%|mean -->790<!-- /GEN -->.

![Role 4 peak beds, closing 90-day mean beds and operations owed against the evacuation policy, R2E holding establishment, saturation release threshold and sortie cancellation probability](../images/role4_demand_levers.png)

The Role 4 peak beds (upper row), closing 90-day mean beds (middle row) and operations owed (lower row), with 95% intervals across replications, against each lever, with the shipped value of each marked by a dashed line.

---

## Casualty Surge Events

<small>[Return to Top](#contents)</small>

**Question.** What changes when casualty surge events are injected into a campaign? `[default, injection off and on at 0.2 events per day · 360 d · 30 replications per arm · independent seeds, pooled exact binomial]`

<!-- GEN casualty_surge -->
| Metric | No events injected | Events injected |
|---|---|---|
| Average total casualties/run | 5402.9 | 8160.5 |
| Average events/run | 0 | 72.47 (range 59–91) |
| Died-of-wounds rate, ordinary casualties | 0.20% [0.18%, 0.22%] | 0.21% [0.19%, 0.23%] |
| Died-of-wounds rate, event casualties | not applicable | 0.43% [0.39%, 0.48%] |
<!-- /GEN -->

![Stem plot of casualty surge events reconstructed from one campaign](../images/casualty_surge_events.png)

Casualty surge events reconstructed from one seed-42 campaign, each drawn as a vertical line at its simulation day with a point at its casualty count.

Events added <!-- GEN cell:casualty_surge|Average events/run|Events injected|full -->72.47 (range 59–91)<!-- /GEN --> events per campaign and raised the mean campaign total from <!-- GEN cell:casualty_surge|Average total casualties/run|No events injected|full -->5402.9<!-- /GEN --> to <!-- GEN cell:casualty_surge|Average total casualties/run|Events injected|full -->8160.5<!-- /GEN --> casualties. The died-of-wounds rate among event casualties was <!-- GEN cell:casualty_surge|Died-of-wounds rate, event casualties|Events injected|full -->0.43% [0.39%, 0.48%]<!-- /GEN -->. Among ordinary casualties it was <!-- GEN cell:casualty_surge|Died-of-wounds rate, ordinary casualties|No events injected|full -->0.20% [0.18%, 0.22%]<!-- /GEN --> without injection and <!-- GEN cell:casualty_surge|Died-of-wounds rate, ordinary casualties|Events injected|full -->0.21% [0.19%, 0.23%]<!-- /GEN --> with it.

---

## Casualty Surge Event Size

<small>[Return to Top](#contents)</small>

**Question.** At what size do casualty surge events begin to degrade care? `[default, every event fixed at one size · 0.2 events per day · 360 d · 30 replications per size · independent seeds, pooled exact binomial and Student t]`

<!-- GEN casualty_surge_size -->
| Event size | Events/run | Died of wounds, event casualties | Died of wounds, ordinary casualties | Peak R2B holding queue | Peak R2E theatre queue | Peak R2E intensive care queue | Peak R2E holding queue |
|---|---|---|---|---|---|---|---|
| None | 0.0 | not applicable | 0.20% [0.18%, 0.22%] | 48.7 ± 9.2 | 44.1 ± 11.9 | 14.4 ± 3.1 | 19.9 ± 5.0 |
| 10 | 72.5 | 0.32% [0.25%, 0.40%] | 0.20% [0.18%, 0.23%] | 50.2 ± 8.0 | 47.2 ± 9.2 | 33.4 ± 12.8 | 54.6 ± 12.4 |
| 20 | 72.5 | 0.43% [0.37%, 0.49%] | 0.21% [0.19%, 0.24%] | 59.5 ± 7.0 | 44.3 ± 4.2 | 143.6 ± 25.4 | 126.7 ± 10.0 |
| 40 | 72.5 | 0.45% [0.40%, 0.49%] | 0.21% [0.19%, 0.24%] | 90.8 ± 12.2 | 77.2 ± 10.5 | 654.3 ± 49.3 | 240.9 ± 15.5 |
| 60 | 72.5 | 0.44% [0.41%, 0.48%] | 0.22% [0.20%, 0.24%] | 125.0 ± 16.6 | 142.4 ± 14.8 | 1110.0 ± 57.6 | 366.3 ± 25.7 |
| 90 | 72.5 | 0.47% [0.44%, 0.50%] | 0.22% [0.20%, 0.24%] | 151.3 ± 17.5 | 416.9 ± 63.0 | 1584.6 ± 47.3 | 503.3 ± 32.7 |
| 120 | 72.5 | 0.46% [0.43%, 0.48%] | 0.24% [0.21%, 0.26%] | 194.5 ± 23.4 | 1051.6 ± 120.2 | 1665.4 ± 35.5 | 660.0 ± 34.4 |
| 180 | 72.5 | 0.48% [0.46%, 0.51%] | 0.25% [0.22%, 0.27%] | 251.1 ± 33.4 | 2556.0 ± 178.4 | 1739.7 ± 27.5 | 898.7 ± 43.6 |
<!-- /GEN -->

![Line plot of the died-of-wounds rate of event and ordinary casualties against casualty surge event size](../images/casualty_surge_size_sweep.png)

The died-of-wounds rate of event and ordinary casualties against the number of casualties per event, with pooled exact binomial 95% confidence intervals.

Peak queues are the largest four-hour mean queue of each pool over the campaign, with a 95% half-width across replications. With every event fixed at 10 casualties, the died-of-wounds rate among event casualties was <!-- GEN cell:casualty_surge_size|10|Died of wounds, event casualties|full -->0.32% [0.25%, 0.40%]<!-- /GEN --> and the peak R2E intensive care queue <!-- GEN cell:casualty_surge_size|10|Peak R2E intensive care queue|full -->33.4 ± 12.8<!-- /GEN -->, against <!-- GEN cell:casualty_surge_size|None|Peak R2E intensive care queue|full -->14.4 ± 3.1<!-- /GEN --> with no events injected. At 20 casualties per event the intensive care queue peaked at <!-- GEN cell:casualty_surge_size|20|Peak R2E intensive care queue|full -->143.6 ± 25.4<!-- /GEN -->, and at 40 at <!-- GEN cell:casualty_surge_size|40|Peak R2E intensive care queue|full -->654.3 ± 49.3<!-- /GEN -->. The R2E theatre queue peaked at <!-- GEN cell:casualty_surge_size|None|Peak R2E theatre queue|full -->44.1 ± 11.9<!-- /GEN --> with no events, <!-- GEN cell:casualty_surge_size|60|Peak R2E theatre queue|full -->142.4 ± 14.8<!-- /GEN --> at 60 casualties per event, <!-- GEN cell:casualty_surge_size|90|Peak R2E theatre queue|full -->416.9 ± 63.0<!-- /GEN --> at 90 and <!-- GEN cell:casualty_surge_size|180|Peak R2E theatre queue|full -->2556.0 ± 178.4<!-- /GEN --> at 180. The event casualty died-of-wounds rate was <!-- GEN cell:casualty_surge_size|60|Died of wounds, event casualties|full -->0.44% [0.41%, 0.48%]<!-- /GEN --> at 60 casualties and <!-- GEN cell:casualty_surge_size|180|Died of wounds, event casualties|full -->0.48% [0.46%, 0.51%]<!-- /GEN --> at 180, and the rate among ordinary casualties was <!-- GEN cell:casualty_surge_size|None|Died of wounds, ordinary casualties|full -->0.20% [0.18%, 0.22%]<!-- /GEN --> with no events and <!-- GEN cell:casualty_surge_size|180|Died of wounds, ordinary casualties|full -->0.25% [0.22%, 0.27%]<!-- /GEN --> at 180.

---

## Resolution of Paired Differences

<small>[Return to Top](#contents)</small>

**Question.** How many replications would each paired difference left open by the experiments above need before its interval narrowed to a stated half-width? `[as each experiment above · 360 d · 30 replications · paired, normal approximation to the interval half-width]`

<!-- GEN resolution -->
| Comparison | Paired difference | Half-width sought | Replications needed |
|---|---|---|---|
| Hold window, R2E first surgeries | −10.90 [−72.35, +50.55] | 2.0 | 26,008 |
| Hold window, R2E theatre entry deferred | −2.90 [−20.63, +14.83] | 1.0 | 8,665 |
| Hold window, diverted for a busy theatre | +13.93 [−4.25, +32.11] | 2.0 | 2,277 |
| Hold window, died of wounds | −0.07 [−2.25, +2.11] | 0.5 | 524 |
| Intensive care gate, died of wounds | +0.10 [−1.71, +1.91] | 0.5 | 362 |
| Policy 15 days against 21, died of wounds | −0.80 [−2.54, +0.94] | 1.0 | 84 |
| Policy 45 days against 21, died of wounds | +1.17 [−0.81, +3.14] | 1.0 | 108 |
| Policy 60 days against 21, died of wounds | +2.77 [+0.66, +4.87] | 1.0 | 123 |
| Saturation release at 8, died of wounds | −0.07 [−1.72, +1.58] | 1.0 | 75 |
| Saturation release at 8, returns to duty | +2.37 [−35.97, +40.70] | 10.0 | 405 |
<!-- /GEN -->

The count for each row is $\lceil (z_{0.975}\, s_d / h)^2 \rceil$, where $s_d$ is the standard deviation of the within-replication differences observed at 30 replications and $h$ the half-width sought, chosen in the response's own units by the script that ran the experiment. The hold window's R2E first-surgery difference was <!-- GEN cell:resolution|Hold window, R2E first surgeries|Paired difference|full -->−10.90 [−72.35, +50.55]<!-- /GEN --> against a half-width sought of <!-- GEN cell:resolution|Hold window, R2E first surgeries|Half-width sought|full -->2.0<!-- /GEN --> operations, which needs <!-- GEN cell:resolution|Hold window, R2E first surgeries|Replications needed|full -->26,008<!-- /GEN --> replications. The intensive care gate's died-of-wounds difference was <!-- GEN cell:resolution|Intensive care gate, died of wounds|Paired difference|full -->+0.10 [−1.71, +1.91]<!-- /GEN --> and needs <!-- GEN cell:resolution|Intensive care gate, died of wounds|Replications needed|full -->362<!-- /GEN -->. Against a half-width sought of <!-- GEN cell:resolution|Policy 15 days against 21, died of wounds|Half-width sought|full -->1.0<!-- /GEN --> death per campaign, the three policy comparisons against the shipped 21 days need <!-- GEN cell:resolution|Policy 15 days against 21, died of wounds|Replications needed|full -->84<!-- /GEN -->, <!-- GEN cell:resolution|Policy 45 days against 21, died of wounds|Replications needed|full -->108<!-- /GEN --> and <!-- GEN cell:resolution|Policy 60 days against 21, died of wounds|Replications needed|full -->123<!-- /GEN --> replications. The saturation release's died-of-wounds difference at the shipped threshold of eight needs <!-- GEN cell:resolution|Saturation release at 8, died of wounds|Replications needed|full -->75<!-- /GEN -->.

---

## Sensitivity Screens

<small>[Return to Top](#contents)</small>

**Question.** Which of the eighty screened parameters most influence the system operating theatre queue, and how is that influence divided among the leading eight? `[default · 30 d · Morris r = 20 with 5 replications per point; Sobol N = 800 with 8 replications per point]`

These screens are tagged 30 days because they were not re-measured at the sustained horizon; their rankings describe a month of campaign.

<!-- GEN morris_top -->
| Rank | Parameter | µ* | σ |
|---|---|---|---|
| 1 | `casualty_surge_rate` | 13.04 | 12.81 |
| 2 | `pri1_evac_prob` | 4.72 | 5.98 |
| 3 | `pri1_surg_prob` | 4.35 | 4.68 |
| 4 | `pri1_dcs_rate` | 4.21 | 6.06 |
| 5 | `mc_p1_balance` | 4.20 | 7.14 |
| 6 | `casualty_surge_kia_fraction` | 3.49 | 6.14 |
| 7 | `casualty_surge_max_cas` | 3.42 | 5.97 |
| 8 | `wia_cbt_mean` | 2.64 | 3.63 |
| 9 | `dnbi_disease_balance` | 2.45 | 3.08 |
| 10 | `surg_mode` | 2.33 | 3.33 |
| 11 | `saturation_queue_threshold` | 2.10 | 4.02 |
| 12 | `long_resus_mode` | 2.08 | 3.49 |
| 13 | `p1_p_max` | 2.03 | 3.56 |
| 14 | `pri2_surg_prob` | 2.01 | 2.86 |
| 15 | `r2b_pre_open_window` | 2.00 | 3.73 |
| 16 | `casualty_surge_min_cas` | 1.98 | 3.74 |
| 17 | `r2e_hold_mode` | 1.94 | 3.35 |
| 18 | `pri2_evac_prob` | 1.92 | 3.08 |
| 19 | `triage_p2_p3_balance` | 1.89 | 3.04 |
| 20 | `kia_cbt_mean` | 1.84 | 3.21 |
<!-- /GEN -->

The table lists the twenty parameters with the largest Morris $\mu^*$ on the system operating theatre queue, of eighty screened. `casualty_surge_rate` ranked first at $\mu^*$ = <!-- GEN cell:morris_top|1|µ*|full -->13.04<!-- /GEN -->, and the next six parameters ranked within $\mu^*$ of <!-- GEN cell:morris_top|2|µ*|full -->4.72<!-- /GEN --> to <!-- GEN cell:morris_top|7|µ*|full -->3.42<!-- /GEN -->.

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
| `casualty_surge_rate` | 0.79 [0.66, 0.92] | 0.34 [0.17, 0.47] |
| `casualty_surge_max_cas` | 0.27 [0.17, 0.37] | −0.06 [−0.09, −0.02] |
| `pri1_surg_prob` | 0.20 [0.07, 0.35] | 0.06 [−0.01, 0.11] |
| `pri1_evac_prob` | 0.14 [0.04, 0.24] | −0.00 [−0.04, 0.04] |
| `pri1_dcs_rate` | 0.10 [0.04, 0.17] | −0.00 [−0.04, 0.03] |
| `casualty_surge_kia_fraction` | 0.10 [−0.00, 0.21] | 0.04 [−0.01, 0.09] |
| `mc_p2_p3_balance` | 0.06 [0.01, 0.12] | −0.02 [−0.04, 0.02] |
| `mc_p1_balance` | 0.04 [−0.07, 0.13] | −0.01 [−0.05, 0.03] |
<!-- /GEN -->

The Sobol decomposition of the eight leading parameters on the same response gave `casualty_surge_rate` a total-order index of <!-- GEN cell:sobol|`casualty_surge_rate`|Total-order index|full -->0.79 [0.66, 0.92]<!-- /GEN -->. The next index was <!-- GEN cell:sobol|`casualty_surge_max_cas`|Total-order index|full -->0.27 [0.17, 0.37]<!-- /GEN --> for `casualty_surge_max_cas`.

---

## Annex A. Model Verification of One Seed-42 Campaign

<small>[Return to Top](#contents)</small>

This annex records the measurements of one 360-day campaign at seed 42 that verify mechanisms a replicated experiment cannot show: that each arrival stream realises its configured rate, that the triage and damage control splits realise their configured shares, that the strategic evacuation timeline closes, the surgical sections carry the load their rosters imply and the force regeneration cycle holds the pool near establishment. It reports one run, carries no interval and measures no performance; the replicated sections above do that. The measurements are tracked in `data/seed42_verification.csv`, written with the baseline by `Rscript run.R --seed 42 --days 360 --iterations 1 --refresh-baseline`.

**Casualty generation.** A stream's configured expectation is its configured daily mean per thousand personnel times its population over a thousand times the days.

<!-- GEN annex_generation -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Combat wounded in action | 1,592 | 1,593 |
| Support wounded in action | 721 | 796 |
| Combat killed in action | 626 | 612 |
| Support killed in action | 279 | 306 |
| Combat disease and non-battle injury | 1,842 | 1,836 |
| Support disease and non-battle injury | 449 | 423 |
<!-- /GEN -->

**Triage.** The expectation is the configured share of the casualties who received a priority.

<!-- GEN annex_triage -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Priority 1 | 3,034 | 2,993 |
| Priority 2 | 912 | 921 |
| Priority 3 | 658 | 691 |
| Killed in action | 905 | not applicable |
<!-- /GEN -->

**Damage control.** The expectation is the configured damage control rate times the casualties operated on at that priority.

<!-- GEN annex_damage_control -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Priority 1 operated | 1,536 | not applicable |
| Priority 1 damage control | 819 | 845 |
| Priority 2 operated | 409 | not applicable |
| Priority 2 damage control | 80 | 82 |
<!-- /GEN -->

**Strategic evacuation.**

<!-- GEN annex_evacuation -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Strategic evacuation decisions | 2,838 | not applicable |
| Boarded | 2,836 | not applicable |
| Still waiting at the close | 2 | not applicable |
<!-- /GEN -->

**Surgical load.** Utilisation of open time is the time-weighted share of a section's rostered time during which its first surgeon was in use, and the queued share is the share of that open time with one or more casualties waiting for any role in the section.

<!-- GEN annex_surgical_load -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Diverted from R2B, surgical team off shift | 969 | not applicable |
| Diverted from R2B, theatre busy | 205 | not applicable |
| R2B holding beds in use, both facilities (mean) | 6.68 | not applicable |
| R2E section 1 utilisation of open time (%) | 20.7 | not applicable |
| R2E section 2 utilisation of open time (%) | 46.8 | not applicable |
| R2E section 3 utilisation of open time (%) | 20.6 | not applicable |
| R2E section 1 queued share of open time (%) | 2.7 | not applicable |
| R2E section 2 queued share of open time (%) | 18.9 | not applicable |
| R2E section 3 queued share of open time (%) | 2.4 | not applicable |
<!-- /GEN -->

**Force regeneration.** The effective force is the combat or support pool at the day shown, under the shipped seven-day reinforcement cycle.

<!-- GEN annex_force -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Combat force, day 0 | 2,500 | not applicable |
| Combat force, day 180 | 2,398 | not applicable |
| Combat force, day 360 | 2,408 | not applicable |
| Support force, day 0 | 1,250 | not applicable |
| Support force, day 180 | 1,202 | not applicable |
| Support force, day 360 | 1,225 | not applicable |
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
