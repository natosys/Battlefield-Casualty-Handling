# Measured Results of a Replicated Simulation of the Land-Based Trauma System

## Abstract

<small>[Return to Top](#contents)</small>

**Background**

A discrete event simulation of an Australian brigade-sized force and its land-based trauma system produces measurements that planners can read without the model's own interpretation attached. This document reports those measurements and nothing else: every replicated experiment and the verification of one seed-42 campaign, with a statement of why the sensitivity screens are not reported.

**Methods**

Each replicated experiment ran 30 independent replications of a 360-day campaign under the shipped configuration or a named variation, and measured queues and occupancy as whole-pool totals over each campaign's closing 90 days. Intervals are 95% Student $t$ intervals about the mean of per-replication values unless a section states otherwise. The designs are in [Methods](Methods.md).

**Results**

At moderate casualty intensity a campaign produced <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|mean -->5,386.9<!-- /GEN --> casualties and at high intensity <!-- GEN cell:scenario_totals|Total casualties/run|High intensity|mean -->12,379.7<!-- /GEN -->. Over the closing 90 days the mean R2E operating theatre queue was <!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|mean -->0.967<!-- /GEN --> casualties at moderate intensity and <!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|mean -->72.616<!-- /GEN --> at high. At high intensity that queue stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|High: queue empty|full -->7%<!-- /GEN --> of the campaign, its late-half level classified as <!-- GEN cell:long_horizon_stability|R2E operating theatre queue|High intensity|full -->converged at 71.0<!-- /GEN -->, while at moderate intensity it stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: queue empty|full -->85%<!-- /GEN -->. Over the closing 90 days the national support base carried a mean census of <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|Moderate intensity|mean -->150.38<!-- /GEN --> beds at moderate intensity and <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|High intensity|mean -->219.86<!-- /GEN --> at high. The treated-cohort died-of-wounds rate of the shipped default configuration was <!-- GEN cell:dow_calibration|Shipped default|Rate at the anchor's horizon|full -->0.421% [0.350%, 0.492%]<!-- /GEN --> at the 30 days of its Ajax Bay anchor and <!-- GEN cell:dow_calibration|Shipped default|Rate at 360 days|full -->0.541% [0.453%, 0.629%]<!-- /GEN --> at 360 days.

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
  - [Demand, Utilisation and Shortfall per Day](#demand-utilisation-and-shortfall-per-day)
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
  - [High-Intensity Evacuation Policy and Holding Establishment](#high-intensity-evacuation-policy-and-holding-establishment)
  - [High-Intensity Strategic Sortie Interval by Policy and Holding Establishment](#high-intensity-strategic-sortie-interval-by-policy-and-holding-establishment)
  - [Moderate-Intensity Evacuation Policy and Holding Establishment](#moderate-intensity-evacuation-policy-and-holding-establishment)
  - [Forward Surgical Saturation Release](#forward-surgical-saturation-release)
  - [Surgical Team Establishment](#surgical-team-establishment)
  - [R2E Operating Theatre Establishment](#r2e-operating-theatre-establishment)
  - [R2E Intensive Care Bed Establishment](#r2e-intensive-care-bed-establishment)
  - [Priority 1 Triage Share](#priority-1-triage-share)
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

A protocol tag has the form `[configuration · horizon · replications · measurement]`. A replication is one complete campaign from an empty system under its own random number stream. A pool's queue is the total number of casualties waiting across every bed of one type at one facility, and its occupancy is the time-weighted number of beds in use over the beds established, both measured over a campaign's closing 90 days unless a section states otherwise. Responses that accumulate over a campaign, such as returns to duty and deaths of wounds, are campaign totals. Ratios divide the high-intensity figure by the moderate-intensity one. Intervals are 95% Student $t$ intervals about the mean of per-replication values; a bound below zero for a quantity that cannot be negative is clamped at zero where a section says so, and an interval for a proportion of campaigns is an exact binomial interval. A paired difference marked † has an interval that excludes zero but does not survive Holm's adjustment across the comparisons of its experiment ([Methods: Interval Methods](Methods.md#interval-methods)), and is not resolved.

Every table and every quoted figure is regenerated from the tracked evidence under `data/` by `scripts/render_results_tables.R`, and `tests/checks/check_results_tables.R` fails when the document disagrees with that evidence. A figure written in a sentence is a copy of a cell of the table beside it.

---

## Comparative Scenario Analysis

<small>[Return to Top](#contents)</small>

**Question.** How do casualty volume, mortality and the load on each resource pool differ between the moderate-intensity (Falklands 1982) and high-intensity (Okinawa 1945) casualty profiles, whose rates derive from the FORECAS projection study [[1]](#references), when the health system is identical? `[moderate_intensity and high_intensity · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN scenario_totals -->
| Metric | Moderate intensity | High intensity | Ratio |
|---|---|---|---|
| Total casualties/run | 5,386.9 [5,321.5, 5,452.4] (p10–p90: 5,157.3–5,622.2) | 12,379.7 [12,227.9, 12,531.5] (p10–p90: 11,964.1–12,894.3) | 2.30× |
| Wounded in action/run | 2,304.0 [2,246.6, 2,361.4] (p10–p90: 2,058.8–2,473.9) | 8,315.5 [8,175.6, 8,455.5] (p10–p90: 7,883.3–8,771.4) | 3.61× |
| Died of wounds/run | 21.17 [17.55, 24.79] (p10–p90: 12.9–30.3) | 568.60 [500.55, 636.65] (p10–p90: 349.6–772.7) | 26.9× |
| Died of wounds, as share of wounded | 0.91% [0.77%, 1.05%] | 6.77% [6.06%, 7.48%] | 7.45× |
<!-- /GEN -->

![Four panels of total casualties, wounded in action, deaths of wounds and deaths as a share of wounded at the two casualty intensities](../images/paper_casualty_totals.png)

Each panel plots the moderate and high casualty intensities as a point with a narrow 95% confidence interval bar and a wide band for the 10th-to-90th-percentile spread across campaigns. The bar shows how precisely the average is known and the band shows how much one campaign varies.

Moderate intensity produced <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|mean -->5,386.9<!-- /GEN --> casualties per run (95% interval <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|ci -->[5,321.5, 5,452.4]<!-- /GEN -->) and high intensity <!-- GEN cell:scenario_totals|Total casualties/run|High intensity|mean -->12,379.7<!-- /GEN --> (<!-- GEN cell:scenario_totals|Total casualties/run|High intensity|ci -->[12,227.9, 12,531.5]<!-- /GEN -->), a ratio of <!-- GEN cell:scenario_totals|Total casualties/run|Ratio|full -->2.30×<!-- /GEN -->. Wounded in action rose by <!-- GEN cell:scenario_totals|Wounded in action/run|Ratio|full -->3.61×<!-- /GEN -->, died of wounds per run by <!-- GEN cell:scenario_totals|Died of wounds/run|Ratio|full -->26.9×<!-- /GEN --> and died of wounds as a share of wounded by <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|Ratio|full -->7.45×<!-- /GEN -->, from <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|Moderate intensity|mean -->0.91%<!-- /GEN --> to <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|High intensity|mean -->6.77%<!-- /GEN -->.

<!-- GEN scenario_queue -->
| Resource group | Moderate intensity mean queue | High intensity mean queue | Ratio |
|---|---|---|---|
| R2B operating theatre | 0.000 [0.000, 0.000] | 0.000 [0.000, 0.000] | not applicable |
| R2B holding beds | 0.090 [0.050, 0.131] | 0.335 [0.276, 0.395] | 3.71× |
| R2B reception overflow census | 0.065 [0.024, 0.106] | 0.278 [0.219, 0.337] | 4.26× |
| R2E operating theatre | 0.967 [0.627, 1.308] | 72.616 [51.404, 93.829] | 75.09× |
| R2E holding beds | 24.376 [22.591, 26.160] | 54.064 [52.007, 56.122] | 2.22× |
| R2E post-operative overflow census | 5.724 [5.359, 6.088] | 19.794 [19.473, 20.115] | 3.46× |
| R2E staging overflow census | 32.163 [26.869, 37.457] | 2,515.747 [2,474.068, 2,557.425] | 78.22× |
| Ambulance and truck fleets | 0.462 [0.225, 0.699] | 1.760 [1.435, 2.085] | 3.81× |
<!-- /GEN -->

![Horizontal plot of queue growth factor on a logarithmic scale for five resource groups with a dashed reference line at the growth in casualty volume](../images/paper_queue_growth.png)

Queue growth factor from moderate to high intensity on a logarithmic scale, with a dashed reference line at the growth in casualty volume. Each point is sized by its absolute queue at high intensity.

![Four-panel bar chart of mean queue length by resource group at the two casualty intensities](../images/scenario_comparison.png)

Mean queue length by resource group at each casualty intensity. Each panel has its own vertical scale, and the error bars show the spread across campaigns rather than a confidence interval.

The R2B operating theatre queue was zero at both intensities. At moderate intensity the R2E holding beds carried a mean queue of <!-- GEN cell:scenario_queue|R2E holding beds|Moderate intensity mean queue|mean -->24.376<!-- /GEN --> casualties (<!-- GEN cell:scenario_queue|R2E holding beds|Moderate intensity mean queue|ci -->[22.591, 26.160]<!-- /GEN -->), against <!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|mean -->0.967<!-- /GEN --> at the R2E operating theatres (<!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|ci -->[0.627, 1.308]<!-- /GEN -->); a mean of <!-- GEN cell:scenario_queue|R2E post-operative overflow census|Moderate intensity mean queue|mean -->5.724<!-- /GEN --> operated casualties were cared for in post-operative overflow and <!-- GEN cell:scenario_queue|R2E staging overflow census|Moderate intensity mean queue|mean -->32.163<!-- /GEN --> awaited a flight in staging overflow. At high intensity the R2E operating theatre queue was <!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|mean -->72.616<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|ci -->[51.404, 93.829]<!-- /GEN -->), the R2E holding queue <!-- GEN cell:scenario_queue|R2E holding beds|High intensity mean queue|mean -->54.064<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E holding beds|High intensity mean queue|ci -->[52.007, 56.122]<!-- /GEN -->), the post-operative overflow census <!-- GEN cell:scenario_queue|R2E post-operative overflow census|High intensity mean queue|mean -->19.794<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E post-operative overflow census|High intensity mean queue|ci -->[19.473, 20.115]<!-- /GEN -->) and the staging overflow census <!-- GEN cell:scenario_queue|R2E staging overflow census|High intensity mean queue|mean -->2,515.747<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E staging overflow census|High intensity mean queue|ci -->[2,474.068, 2,557.425]<!-- /GEN -->); the ratios to moderate intensity were <!-- GEN cell:scenario_queue|R2E operating theatre|Ratio|full -->75.09×<!-- /GEN -->, <!-- GEN cell:scenario_queue|R2E holding beds|Ratio|full -->2.22×<!-- /GEN -->, <!-- GEN cell:scenario_queue|R2E post-operative overflow census|Ratio|full -->3.46×<!-- /GEN --> and <!-- GEN cell:scenario_queue|R2E staging overflow census|Ratio|full -->78.22×<!-- /GEN -->. The ambulance and truck fleets queued <!-- GEN cell:scenario_queue|Ambulance and truck fleets|Moderate intensity mean queue|mean -->0.462<!-- /GEN --> and <!-- GEN cell:scenario_queue|Ambulance and truck fleets|High intensity mean queue|mean -->1.760<!-- /GEN --> casualties at the two intensities. An overflow census counts casualties cared for without a bed rather than casualties waiting for one, a casualty finding no intensive care or holding bed being placed in overflow instead of queued, so the intensive care pool carries no queue. The high-intensity staging figure is a mean over a window in which that quantity was still growing, as the sections that follow record, so it describes the closing 90 days and not a level.

---

## Queue Behaviour over the Campaign

<small>[Return to Top](#contents)</small>

**Question.** Does a pool's queue clear between its busy spells, or does it stand for the whole campaign, and how does the share of post-operative care delivered in a holding bed change with time? `[moderate_intensity and high_intensity · 360 d · 30 replications · pool totals, unbinned series]`

<!-- GEN queue_clearance -->
| Resource pool | Moderate: queue empty | Moderate: longest run above zero (days) | High: queue empty | High: longest run above zero (days) |
|---|---|---|---|---|
| R2B reception overflow | 99% | 0.9 (0.5–1.8) | 90% | 2.1 (1.8–2.5) |
| R2B holding beds | 94% | 9.9 (7.2–12.5) | 75% | 17.1 (13.9–19.4) |
| R2E operating theatres | 85% | 4.3 (3.4–5.9) | 7% | 132.4 (65.8–167.8) |
| R2E post-operative overflow | 14% | 23.6 (19.7–26.7) | 1% | 357.6 (357.2–358.3) |
| R2E holding beds | 1% | 355.9 (354.0–356.7) | 1% | 357.6 (357.4–358.1) |
| R2E staging overflow | 5% | 55.5 (36.6–81.1) | 1% | 357.8 (357.4–358.2) |
<!-- /GEN -->

![Four stacked panels of queue length against campaign day, one per resource pool, at the two casualty intensities](../images/queue_length_over_time.png)

Queue length against campaign day for each pool, as the median across replications with an interquartile band, at each casualty intensity. Each panel is annotated with the share of the campaign the queue stood empty.

The table reports, for each pool and intensity, the median across replications of the share of the campaign the pool's queue or overflow census stood empty and of the longest unbroken spell it did not, with the interquartile range of the spell in parentheses. At high intensity the R2E operating theatre queue stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|High: queue empty|full -->7%<!-- /GEN --> of the campaign, with a median longest spell above zero of <!-- GEN cell:queue_clearance|R2E operating theatres|High: longest run above zero (days)|full -->132.4 (65.8–167.8)<!-- /GEN --> days, and the R2E holding queue and both R2E overflow censuses stood empty for <!-- GEN cell:queue_clearance|R2E staging overflow|High: queue empty|full -->1%<!-- /GEN --> or less. At moderate intensity the theatre queue stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: queue empty|full -->85%<!-- /GEN --> of the campaign with a median longest spell of <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: longest run above zero (days)|full -->4.3 (3.4–5.9)<!-- /GEN --> days, while the R2E holding queue stood empty for <!-- GEN cell:queue_clearance|R2E holding beds|Moderate: queue empty|full -->1%<!-- /GEN -->.

<!-- GEN degraded_care -->
| Intensity and stage | First day the median daily rate reaches 100% | First ten days | Last ten days | Whole campaign |
|---|---|---|---|---|
| Moderate intensity: Stabilisation | not reached | 34.8% | 35.3% | 36.5% |
| Moderate intensity: Post-definitive care | not reached | 59.6% | 63.0% | 65.0% |
| High intensity: Stabilisation | not reached | 41.5% | 44.8% | 43.4% |
| High intensity: Post-definitive care | not reached | 85.4% | 88.9% | 88.8% |
<!-- /GEN -->

![Two stacked panels of the share of casualties recovering in a holding bed against campaign day](../images/degraded_care_rate_over_time.png)

The share of casualties taking the holding-bed recovery against campaign day, stabilisation above and post-definitive care below, as a daily rate inside an interquartile band and as a running cumulative rate.

The degraded post-operative care rate is the share of casualties who took the holding-bed recovery in place of an intensive care bed at the point the decision was taken. At high intensity the median daily rate for post-definitive care first reached 100% on day <!-- GEN cell:degraded_care|High intensity: Post-definitive care|First day the median daily rate reaches 100%|full -->not reached<!-- /GEN --> and for stabilisation on day <!-- GEN cell:degraded_care|High intensity: Stabilisation|First day the median daily rate reaches 100%|full -->not reached<!-- /GEN -->; over the whole campaign <!-- GEN cell:degraded_care|High intensity: Post-definitive care|Whole campaign|full -->88.8%<!-- /GEN --> of post-definitive care and <!-- GEN cell:degraded_care|High intensity: Stabilisation|Whole campaign|full -->43.4%<!-- /GEN --> of stabilisation took the holding bed. At moderate intensity the whole-campaign rates were <!-- GEN cell:degraded_care|Moderate intensity: Post-definitive care|Whole campaign|full -->65.0%<!-- /GEN --> and <!-- GEN cell:degraded_care|Moderate intensity: Stabilisation|Whole campaign|full -->36.5%<!-- /GEN -->, and the median daily rate did not reach 100%.

---

## Sustained Operations

<small>[Return to Top](#contents)</small>

**Question.** Does each response settle to a level over a 360-day campaign, or does it keep moving? `[moderate_intensity and high_intensity · 360 d · 30 replications · twelve 30-day block means]`

Each response was reduced to twelve consecutive 30-day block means and classified from the trend over the late half of the campaign, one slope per replication with an interval across replications; the rule is in [Methods](Methods.md#classifying-stability).

<!-- GEN long_horizon_stability -->
| Response | Moderate intensity | High intensity |
|---|---|---|
| R2E operating theatre queue | converged at 0.90 | converged at 71.0 |
| R2E holding bed queue | converged at 23.8 | converged at 54.1 |
| R2E post-operative overflow census | converged at 5.54 | converged at 19.8 |
| R2E staging overflow census | converged at 30.1 | **drifting, +11.2%/block**, 117.7 to 2,739.6 |
| Strategic evacuation backlog | **drifting, +4.3%/block**, 26.1 to 44.6 | **drifting, +11.6%/block**, 137.5 to 2,927.6 |
| R2B reception overflow census | converged at 0.10 | converged at 0.29 |
| R2B holding bed queue | converged at 0.08 | converged at 0.33 |
| Casualty arrivals per day | converged at 14.9 | converged at 34.3 |
| Deaths of wounds per day | converged at 0.07 | **drifting, +5.1%/block**, 0.5 to 2.2 |
<!-- /GEN -->

![Panels of 30-day block means against campaign day for every response at the moderate and high casualty intensities](../images/long_horizon_blocks.png)

Block means of every queue, occupancy, arrival, evacuation backlog and mortality response over the campaign at the two casualty intensities, each panel headed by its stability classification at each intensity.

Three responses at high intensity were classified as drifting: the R2E staging overflow census (<!-- GEN cell:long_horizon_stability|R2E staging overflow census|High intensity|full -->**drifting, +11.2%/block**, 117.7 to 2,739.6<!-- /GEN -->), the strategic evacuation backlog (<!-- GEN cell:long_horizon_stability|Strategic evacuation backlog|High intensity|full -->**drifting, +11.6%/block**, 137.5 to 2,927.6<!-- /GEN -->) and the deaths of wounds per day (<!-- GEN cell:long_horizon_stability|Deaths of wounds per day|High intensity|full -->**drifting, +5.1%/block**, 0.5 to 2.2<!-- /GEN -->). The R2E operating theatre queue at high intensity was <!-- GEN cell:long_horizon_stability|R2E operating theatre queue|High intensity|full -->converged at 71.0<!-- /GEN -->, as were the R2E holding bed queue, <!-- GEN cell:long_horizon_stability|R2E holding bed queue|High intensity|full -->converged at 54.1<!-- /GEN -->, and the post-operative overflow census, <!-- GEN cell:long_horizon_stability|R2E post-operative overflow census|High intensity|full -->converged at 19.8<!-- /GEN -->. At moderate intensity one response was classified as drifting, the strategic evacuation backlog (<!-- GEN cell:long_horizon_stability|Strategic evacuation backlog|Moderate intensity|full -->**drifting, +4.3%/block**, 26.1 to 44.6<!-- /GEN -->); the R2E operating theatre queue was <!-- GEN cell:long_horizon_stability|R2E operating theatre queue|Moderate intensity|full -->converged at 0.90<!-- /GEN -->, the R2E holding bed queue <!-- GEN cell:long_horizon_stability|R2E holding bed queue|Moderate intensity|full -->converged at 23.8<!-- /GEN -->, the post-operative overflow census <!-- GEN cell:long_horizon_stability|R2E post-operative overflow census|Moderate intensity|full -->converged at 5.54<!-- /GEN --> and the staging overflow census <!-- GEN cell:long_horizon_stability|R2E staging overflow census|Moderate intensity|full -->converged at 30.1<!-- /GEN -->.

### Demand, Utilisation and Shortfall per Day

**Question.** How much more demand does the high-intensity profile place on the system than the moderate-intensity one, how fully is each R2E pool used over the closing 90 days, and at what rate does a pool fall behind where it has not settled? `[moderate_intensity and high_intensity · 360 d · 30 replications · closing 90 d and the late half of the campaign]`

<!-- GEN load_demand -->
| Demand measure | Moderate intensity | High intensity | Ratio |
|---|---|---|---|
| Wounded in action/run | 2,304.0 [2,246.6, 2,361.4] | 8,315.5 [8,175.6, 8,455.5] | 3.61× |
| R2B and R2E first surgeries/run | 2,640.8 [2,590.7, 2,690.9] | 7,954.7 [7,838.0, 8,071.4] | 3.01× |
| Post-definitive recovery dispositions/run | 3,913.8 [3,850.3, 3,977.4] | 9,363.3 [9,271.3, 9,455.3] | 2.39× |
<!-- /GEN -->

The wounded in action rose <!-- GEN cell:load_demand|Wounded in action/run|Ratio|full -->3.61×<!-- /GEN -->, the first surgeries at R2B and R2E <!-- GEN cell:load_demand|R2B and R2E first surgeries/run|Ratio|full -->3.01×<!-- /GEN --> and the post-definitive recovery dispositions <!-- GEN cell:load_demand|Post-definitive recovery dispositions/run|Ratio|full -->2.39×<!-- /GEN --> from moderate to high intensity. The surgeries and dispositions are per-campaign counts from the shipped configuration's arms of the [R2B pre-open hold window](#r2b-pre-open-hold-window) and [evacuation policy](#evacuation-policy) experiments, and the total casualties are in the [comparative scenario analysis](#comparative-scenario-analysis).

<!-- GEN load_shortfall -->
| Pool | Moderate: shortfall per day | Moderate: utilisation | High: shortfall per day | High: utilisation |
|---|---|---|---|---|
| R2E operating theatres | 0.000 [−0.004, 0.004] | 28.1% [26.8%, 29.5%] | 0.014 [−0.151, 0.179] | 75.9% [74.7%, 77.1%] |
| R2E holding beds | 0.007 [−0.012, 0.026] | 99.6% [99.4%, 99.7%] | 0.002 [−0.022, 0.027] | 100.0% [100.0%, 100.0%] |
| R2E intensive care | no queue forms | 86.8% [86.0%, 87.6%] | no queue forms | 97.5% [97.4%, 97.7%] |
| R2E post-operative overflow | 0.003 [−0.001, 0.007] | not applicable | −0.001 [−0.004, 0.002] | not applicable |
| R2E staging overflow | 0.044 [−0.006, 0.093] | not applicable | 7.692 [7.514, 7.871] | not applicable |
| Strategic evacuation backlog | 0.057 [0.006, 0.107] | not applicable | 8.410 [8.227, 8.593] | not applicable |
<!-- /GEN -->

The shortfall per day is the mean per-replication slope of the 30-day block means over the late half of the campaign, the estimate the stability classification uses, divided by the 30 days of a block; the estimator is defined in [Methods](Methods.md#classifying-stability). At moderate intensity the intervals of the R2E theatre queue (<!-- GEN cell:load_shortfall|R2E operating theatres|Moderate: shortfall per day|full -->0.000 [−0.004, 0.004]<!-- /GEN -->), the holding bed queue (<!-- GEN cell:load_shortfall|R2E holding beds|Moderate: shortfall per day|full -->0.007 [−0.012, 0.026]<!-- /GEN -->), the post-operative overflow census (<!-- GEN cell:load_shortfall|R2E post-operative overflow|Moderate: shortfall per day|full -->0.003 [−0.001, 0.007]<!-- /GEN -->) and the staging overflow census (<!-- GEN cell:load_shortfall|R2E staging overflow|Moderate: shortfall per day|full -->0.044 [−0.006, 0.093]<!-- /GEN -->) contain zero, while the strategic evacuation backlog grew by <!-- GEN cell:load_shortfall|Strategic evacuation backlog|Moderate: shortfall per day|full -->0.057 [0.006, 0.107]<!-- /GEN --> casualties a day. At high intensity the staging overflow census grew by <!-- GEN cell:load_shortfall|R2E staging overflow|High: shortfall per day|full -->7.692 [7.514, 7.871]<!-- /GEN --> and the strategic evacuation backlog by <!-- GEN cell:load_shortfall|Strategic evacuation backlog|High: shortfall per day|full -->8.410 [8.227, 8.593]<!-- /GEN -->, while the intervals of the R2E theatre queue (<!-- GEN cell:load_shortfall|R2E operating theatres|High: shortfall per day|full -->0.014 [−0.151, 0.179]<!-- /GEN -->), the holding bed queue (<!-- GEN cell:load_shortfall|R2E holding beds|High: shortfall per day|full -->0.002 [−0.022, 0.027]<!-- /GEN -->) and the post-operative overflow census (<!-- GEN cell:load_shortfall|R2E post-operative overflow|High: shortfall per day|full -->−0.001 [−0.004, 0.002]<!-- /GEN -->) contain zero. Over the closing 90 days the R2E theatres were in use <!-- GEN cell:load_shortfall|R2E operating theatres|Moderate: utilisation|full -->28.1% [26.8%, 29.5%]<!-- /GEN --> of the time at moderate intensity and <!-- GEN cell:load_shortfall|R2E operating theatres|High: utilisation|full -->75.9% [74.7%, 77.1%]<!-- /GEN --> at high, the holding beds <!-- GEN cell:load_shortfall|R2E holding beds|Moderate: utilisation|full -->99.6% [99.4%, 99.7%]<!-- /GEN --> and <!-- GEN cell:load_shortfall|R2E holding beds|High: utilisation|full -->100.0% [100.0%, 100.0%]<!-- /GEN -->, and the intensive care beds <!-- GEN cell:load_shortfall|R2E intensive care|Moderate: utilisation|full -->86.8% [86.0%, 87.6%]<!-- /GEN --> and <!-- GEN cell:load_shortfall|R2E intensive care|High: utilisation|full -->97.5% [97.4%, 97.7%]<!-- /GEN -->. The strategic evacuation backlog and the overflow censuses count casualties without a bed and have no establishment, so they carry no utilisation, and the intensive care beds carry no shortfall, no queue forming for them.

---

## Treated-Cohort Mortality

<small>[Return to Top](#contents)</small>

**Question.** What fraction of casualties reaching an R2B or R2E facility die of wounds over a 360-day campaign, and how does that compare with the rate at the length of the historical campaign each configuration models? `[default and high_intensity · 360 d and each anchor's horizon (30 d, 82 d) · 3 measurements of 10 and of 50 replications · pooled, Student t]`

<!-- GEN dow_calibration -->
| Configuration | Rate at 360 days | Post-operative share at 360 days | Replications at 360 days | Historical anchor | Rate at the anchor's horizon | Post-operative share at the anchor's horizon | Replications at the anchor's horizon |
|---|---|---|---|---|---|---|---|
| Shipped default | 0.541% [0.453%, 0.629%] | 24% | 30 | fitted to 0.46% (Ajax Bay, 30 days) | 0.421% [0.350%, 0.492%] | 21% | 150 |
| High intensity | 5.692% [5.088%, 6.295%] | 19% | 30 | independent bound, at or below 3.40% (Okinawa, 82 days) | 2.905% [2.681%, 3.128%] | 8% | 150 |
<!-- /GEN -->

At 360 days the shipped default's pooled rate was <!-- GEN cell:dow_calibration|Shipped default|Rate at 360 days|full -->0.541% [0.453%, 0.629%]<!-- /GEN -->, with post-operative care carrying <!-- GEN cell:dow_calibration|Shipped default|Post-operative share at 360 days|full -->24%<!-- /GEN --> of its expected deaths, and high intensity's was <!-- GEN cell:dow_calibration|High intensity|Rate at 360 days|full -->5.692% [5.088%, 6.295%]<!-- /GEN -->, with post-operative care carrying <!-- GEN cell:dow_calibration|High intensity|Post-operative share at 360 days|full -->19%<!-- /GEN -->. Each configuration was also measured at the length of the historical campaign it models, where the two anchors play different roles. The Falklands anchor is the three deaths of wounds among over 650 combat casualties treated by the Ajax Bay field hospital and its forward stations at Teal Inlet and Fitzroy [[2]](#references), a 30-day campaign; the shipped default's base rates and post-operative multiplier are fitted to it, so its rate at that horizon, <!-- GEN cell:dow_calibration|Shipped default|Rate at the anchor's horizon|full -->0.421% [0.350%, 0.492%]<!-- /GEN -->, shows that the fit was achieved rather than measuring an outcome. The Okinawa anchor is the share of casualties reaching a hospital alive who died of wounds over the 82 days of that campaign [[3]](#references); high intensity is not fitted to it, so its rate at that horizon, <!-- GEN cell:dow_calibration|High intensity|Rate at the anchor's horizon|full -->2.905% [2.681%, 3.128%]<!-- /GEN -->, is checked against it as an independent upper bound. The calibration check `tests/checks/check_dow_calibration.R` asserts each anchor at the length of the campaign it describes.

---

## Forward Holding and Evacuation Levers

<small>[Return to Top](#contents)</small>

Each subsection varies one setting of the shipped configuration and reports the responses it moved. Experiments with several arms draw every arm from one control seed unless the subsection says otherwise, and the paired difference is reported where the arms are paired.

### R2B Pre-Open Hold Window

**Question.** What changes when a casualty needing surgery at R2B is held forward for a surgical section about to reopen rather than moved rearward at once? `[default, hold window 0 and 60 min · 360 d · 30 replications · campaign totals, paired difference]`

<!-- GEN hold_window -->
| Measure | Window 0 | Window 60 min | Difference |
| --- | --- | --- | --- |
| Casualties held at R2B | 0.00 | 106.57 | +106.57 [+102.36, +110.78] |
| R2B surgeries | 775.27 | 824.83 | +49.57 [+32.67, +66.47] |
| Diverted, team off shift | 1286.57 | 1201.53 | −85.03 [−120.67, −49.40] |
| Diverted, theatre busy | 214.43 | 236.83 | +22.40 [+1.70, +43.10]† |
| R2E first surgeries | 1833.97 | 1815.97 | −18.00 [−75.22, +39.22] |
| R2E theatre entry deferred | 377.10 | 375.13 | −1.97 [−17.60, +13.66] |
| Died of wounds per run | 19.27 | 21.17 | +1.90 [−2.11, +5.91] |
| Total casualties | 5344.57 | 5386.93 | +42.37 [−51.34, +136.07] |
<!-- /GEN -->

![Forest plot of the paired difference in each of eight measures with its confidence interval](../images/paper_hold_window_effects.png)

The paired difference per campaign for each of the eight measures with its 95% confidence interval against a vertical line at zero.

A 60-minute window held <!-- GEN cell:hold_window|Casualties held at R2B|Window 60 min|full -->106.57<!-- /GEN --> casualties per campaign at R2B against none with a zero window. R2B surgeries differed by <!-- GEN cell:hold_window|R2B surgeries|Difference|full -->+49.57 [+32.67, +66.47]<!-- /GEN --> and diversions for an off-shift section by <!-- GEN cell:hold_window|Diverted, team off shift|Difference|full -->−85.03 [−120.67, −49.40]<!-- /GEN -->. The differences in diversions for a busy theatre were <!-- GEN cell:hold_window|Diverted, theatre busy|Difference|full -->+22.40 [+1.70, +43.10]†<!-- /GEN -->, in R2E first surgeries <!-- GEN cell:hold_window|R2E first surgeries|Difference|full -->−18.00 [−75.22, +39.22]<!-- /GEN -->, in R2E theatre entry deferrals <!-- GEN cell:hold_window|R2E theatre entry deferred|Difference|full -->−1.97 [−17.60, +13.66]<!-- /GEN -->, in died of wounds <!-- GEN cell:hold_window|Died of wounds per run|Difference|full -->+1.90 [−2.11, +5.91]<!-- /GEN --> and in total casualties <!-- GEN cell:hold_window|Total casualties|Difference|full -->+42.37 [−51.34, +136.07]<!-- /GEN -->.

**High-intensity profile.** The same comparison under `high_intensity` `[high_intensity, hold window 0 and 60 min · 360 d · 30 replications · campaign totals, paired differences]`:

<!-- GEN hold_window_high -->
| Measure | Window 0 | Window 60 min | Difference |
| --- | --- | --- | --- |
| Casualties held at R2B | 0.00 | 340.63 | +340.63 [+334.65, +346.61] |
| R2B surgeries | 1742.87 | 1795.40 | +52.53 [+28.00, +77.07] |
| Diverted, team off shift | 4170.03 | 3825.83 | −344.20 [−403.16, −285.24] |
| Diverted, theatre busy | 1342.77 | 1375.33 | +32.57 [−15.39, +80.52] |
| R2E first surgeries | 6250.23 | 6159.30 | −90.93 [−196.94, +15.07] |
| R2E theatre entry deferred | 1635.73 | 1587.77 | −47.97 [−85.24, −10.69]† |
| Died of wounds per run | 656.13 | 568.60 | −87.53 [−183.07, +8.01] |
| Total casualties | 12511.50 | 12379.70 | −131.80 [−301.08, +37.48] |
<!-- /GEN -->

The 60-minute window held <!-- GEN cell:hold_window_high|Casualties held at R2B|Window 60 min|full -->340.63<!-- /GEN --> casualties forward and changed diversions for a team off shift by <!-- GEN cell:hold_window_high|Diverted, team off shift|Difference|full -->−344.20 [−403.16, −285.24]<!-- /GEN -->.

### R2B Holding Capacity and Evacuation Threshold

**Question.** How do the R2B holding queue and the pools behind it respond to the R2B holding bed establishment and to the threshold at which convalescent casualties are evacuated early? `[default, 5 to 10 holding beds per unit crossed with a 0 to 7 day threshold · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN hold_threshold_beds -->
| R2B holding beds per unit | R2B hold mean queue | R2B hold utilisation | R2B reception overflow census | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|---|
| 5 (shipped) | 0.09 [0.05, 0.13] | 79.1% [78.6, 79.6] | 0.065 [0.024, 0.106] | 24.376 [22.591, 26.160] | 5.72 [5.36, 6.09] | 2062 [2032, 2093] | 21.2 [17.5, 24.8] |
| 7 | 0.17 [0.08, 0.25] | 84.0% [83.7, 84.3] | 0.085 [0.010, 0.160] | 21.752 [20.229, 23.275] | 5.56 [5.12, 6.01] | 2061 [2029, 2093] | 21.0 [17.6, 24.4] |
| 10 | 0.06 [0.00, 0.11] | 77.6% [77.3, 78.0] | 0.006 [0.003, 0.009] | 17.595 [16.366, 18.825] | 4.83 [4.49, 5.16] | 2043 [2014, 2073] | 22.2 [19.5, 25.0] |
<!-- /GEN -->

With the threshold disabled, raising the establishment from five to ten holding beds per unit moved the R2B holding queue from <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2B hold mean queue|full -->0.09 [0.05, 0.13]<!-- /GEN --> to <!-- GEN cell:hold_threshold_beds|10|R2B hold mean queue|full -->0.06 [0.00, 0.11]<!-- /GEN --> and R2B holding utilisation from <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2B hold utilisation|full -->79.1% [78.6, 79.6]<!-- /GEN --> to <!-- GEN cell:hold_threshold_beds|10|R2B hold utilisation|full -->77.6% [77.3, 78.0]<!-- /GEN -->. The R2E holding queue was <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2E hold mean queue|full -->24.376 [22.591, 26.160]<!-- /GEN --> at five beds and <!-- GEN cell:hold_threshold_beds|10|R2E hold mean queue|full -->17.595 [16.366, 18.825]<!-- /GEN --> at ten, and the R2E post-operative overflow census <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2E post-operative overflow census|full -->5.72 [5.36, 6.09]<!-- /GEN --> and <!-- GEN cell:hold_threshold_beds|10|R2E post-operative overflow census|full -->4.83 [4.49, 5.16]<!-- /GEN --> Returns to duty were <!-- GEN cell:hold_threshold_beds|5 (shipped)|Returns to duty|full -->2062 [2032, 2093]<!-- /GEN --> at five beds and <!-- GEN cell:hold_threshold_beds|10|Returns to duty|full -->2043 [2014, 2073]<!-- /GEN --> at ten, and deaths of wounds <!-- GEN cell:hold_threshold_beds|5 (shipped)|Deaths of wounds|full -->21.2 [17.5, 24.8]<!-- /GEN --> and <!-- GEN cell:hold_threshold_beds|10|Deaths of wounds|full -->22.2 [19.5, 25.0]<!-- /GEN -->.

<!-- GEN hold_threshold_threshold -->
| Evacuation threshold | R2B hold mean queue | R2B hold utilisation | R2B reception overflow census | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|---|
| Disabled (shipped) | 0.09 [0.05, 0.13] | 79.1% [78.6, 79.6] | 0.065 [0.024, 0.106] | 24.376 [22.591, 26.160] | 5.72 [5.36, 6.09] | 2062 [2032, 2093] | 21.2 [17.5, 24.8] |
| 1 day | 0.05 [0.03, 0.06] | 53.8% [52.7, 54.9] | 0.182 [0.000, 0.489] | 26.460 [25.083, 27.836] | 5.84 [5.33, 6.35] | 2055 [2022, 2088] | 25.4 [16.5, 34.4] |
| 3 days | 0.05 [0.03, 0.07] | 68.5% [67.9, 69.1] | 0.040 [0.007, 0.072] | 24.521 [23.131, 25.911] | 5.53 [5.19, 5.88] | 2065 [2033, 2097] | 24.1 [18.0, 30.2] |
| 5 days (mode) | 0.05 [0.03, 0.07] | 74.1% [73.6, 74.6] | 0.035 [0.010, 0.061] | 23.375 [22.264, 24.485] | 5.50 [5.23, 5.76] | 2033 [2002, 2064] | 21.7 [19.0, 24.5] |
| 7 days | 0.05 [0.03, 0.08] | 76.6% [76.2, 77.1] | 0.049 [0.009, 0.090] | 24.529 [23.186, 25.871] | 5.59 [5.23, 5.96] | 2063 [2032, 2094] | 24.0 [20.2, 27.8] |
<!-- /GEN -->

![Eight panels of R2B and R2E queue and utilisation, returns to duty and died of wounds against the evacuation threshold](../images/r2b_hold_threshold_sweep.png)

R2B and R2E queue and utilisation, returns to duty and died of wounds against the evacuation threshold in days, one line per swept bed count, with a 95% confidence ribbon.

With five beds per unit, a one-day threshold moved the R2B holding queue to <!-- GEN cell:hold_threshold_threshold|1 day|R2B hold mean queue|full -->0.05 [0.03, 0.06]<!-- /GEN --> and the R2E holding queue to <!-- GEN cell:hold_threshold_threshold|1 day|R2E hold mean queue|full -->26.460 [25.083, 27.836]<!-- /GEN -->, and the R2E post-operative overflow census to <!-- GEN cell:hold_threshold_threshold|1 day|R2E post-operative overflow census|full -->5.84 [5.33, 6.35]<!-- /GEN -->. A three-day threshold gave <!-- GEN cell:hold_threshold_threshold|3 days|R2B hold mean queue|full -->0.05 [0.03, 0.07]<!-- /GEN -->, <!-- GEN cell:hold_threshold_threshold|3 days|R2E hold mean queue|full -->24.521 [23.131, 25.911]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|3 days|R2E post-operative overflow census|full -->5.53 [5.19, 5.88]<!-- /GEN --> on the same three responses. At five and seven days the R2B holding queue was <!-- GEN cell:hold_threshold_threshold|5 days (mode)|R2B hold mean queue|full -->0.05 [0.03, 0.07]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|7 days|R2B hold mean queue|full -->0.05 [0.03, 0.08]<!-- /GEN -->. Returns to duty were <!-- GEN cell:hold_threshold_threshold|Disabled (shipped)|Returns to duty|full -->2062 [2032, 2093]<!-- /GEN --> with the threshold disabled and <!-- GEN cell:hold_threshold_threshold|1 day|Returns to duty|full -->2055 [2022, 2088]<!-- /GEN --> at one day, and deaths of wounds <!-- GEN cell:hold_threshold_threshold|Disabled (shipped)|Deaths of wounds|full -->21.2 [17.5, 24.8]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|1 day|Deaths of wounds|full -->25.4 [16.5, 34.4]<!-- /GEN -->. The full grid, including returns to duty and deaths of wounds at every point, is in `data/sweeps/r2b_hold_threshold_sweep.csv`.

**High-intensity profile.** The same grid under `high_intensity` `[high_intensity, 5 to 10 beds per unit and 0 to 7 days · 360 d · 30 replications · pool totals, closing 90 d]`:

<!-- GEN hold_threshold_beds_high -->
| R2B holding beds per unit | R2B hold mean queue | R2B hold utilisation | R2B reception overflow census | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|---|
| 5 (shipped) | 0.34 [0.28, 0.40] | 81.3% [81.0, 81.6] | 0.278 [0.219, 0.337] | 54.064 [52.007, 56.122] | 19.79 [19.47, 20.12] | 2862 [2824, 2900] | 568.6 [500.5, 636.7] |
| 7 | 0.49 [0.40, 0.58] | 85.9% [85.7, 86.1] | 0.299 [0.232, 0.366] | 51.388 [49.791, 52.985] | 19.80 [19.55, 20.06] | 2842 [2809, 2875] | 598.1 [526.8, 669.4] |
| 10 | 0.41 [0.31, 0.51] | 80.5% [80.3, 80.8] | 0.292 [0.237, 0.348] | 49.845 [47.875, 51.816] | 19.87 [19.65, 20.10] | 2839 [2807, 2871] | 553.1 [485.4, 620.8] |
<!-- /GEN -->

<!-- GEN hold_threshold_threshold_high -->
| Evacuation threshold | R2B hold mean queue | R2B hold utilisation | R2B reception overflow census | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|---|
| Disabled (shipped) | 0.34 [0.28, 0.40] | 81.3% [81.0, 81.6] | 0.278 [0.219, 0.337] | 54.064 [52.007, 56.122] | 19.79 [19.47, 20.12] | 2862 [2824, 2900] | 568.6 [500.5, 636.7] |
| 1 day | 0.14 [0.12, 0.17] | 59.4% [58.4, 60.5] | 0.334 [0.269, 0.399] | 56.158 [54.247, 58.069] | 19.88 [19.67, 20.09] | 2856 [2825, 2887] | 513.9 [471.4, 556.5] |
| 3 days | 0.22 [0.19, 0.25] | 73.7% [73.3, 74.2] | 0.374 [0.279, 0.469] | 54.676 [53.368, 55.985] | 19.94 [19.71, 20.16] | 2848 [2819, 2876] | 604.1 [522.8, 685.4] |
| 5 days (mode) | 0.25 [0.20, 0.29] | 77.5% [76.9, 78.0] | 0.314 [0.252, 0.376] | 53.921 [52.180, 55.663] | 19.83 [19.61, 20.05] | 2833 [2808, 2857] | 544.8 [481.9, 607.8] |
| 7 days | 0.30 [0.25, 0.35] | 79.9% [79.5, 80.2] | 0.332 [0.267, 0.397] | 55.056 [53.127, 56.985] | 19.88 [19.59, 20.16] | 2879 [2852, 2906] | 651.6 [569.2, 733.9] |
<!-- /GEN -->

At 10 holding beds per unit the R2B holding queue was <!-- GEN cell:hold_threshold_beds_high|10|R2B hold mean queue|full -->0.41 [0.31, 0.51]<!-- /GEN --> against <!-- GEN cell:hold_threshold_beds_high|5 (shipped)|R2B hold mean queue|full -->0.34 [0.28, 0.40]<!-- /GEN --> at the shipped five. A one-day evacuation threshold left the R2E holding queue at <!-- GEN cell:hold_threshold_threshold_high|1 day|R2E hold mean queue|full -->56.158 [54.247, 58.069]<!-- /GEN --> against <!-- GEN cell:hold_threshold_threshold_high|Disabled (shipped)|R2E hold mean queue|full -->54.064 [52.007, 56.122]<!-- /GEN --> with the threshold disabled.

![Eight panels of R2B and R2E queue and utilisation, returns to duty and died of wounds against the evacuation threshold at high intensity](../images/r2b_hold_threshold_sweep_high_intensity.png)

The panels of the first figure of this section under the high-intensity profile.

### Transport Fleet Size

**Question.** At what fleet size does the transport queue form, at each casualty intensity? `[default and high_intensity · 360 d · 30 replications · pool totals, closing 90 d]`

The first table is the shipped moderate-intensity configuration and the second the high-intensity profile.

<!-- GEN transport -->
| Fleet size | Ambulance mean queue | Truck mean queue |
|---|---|---|
| 1 | 7.6600 [4.1208, 11.1993] | 0.0606 [0.0202, 0.1011] |
| 2 | 1.5242 [0.1249, 2.9236] | 0.0103 [0.0000, 0.0222] |
| 3 (current ambulance) | 0.4613 [0.2240, 0.6986] | 0.0003 [0.0001, 0.0005] |
| 4 (current truck) | 0.0448 [0.0199, 0.0697] | 0.0008 [0.0000, 0.0018] |
| 5 | 0.1087 [0.0000, 0.2579] | not swept |
<!-- /GEN -->

![Four panels of mean queue and utilisation against fleet size for the ambulance and truck fleets](../images/transport_capacity_margin_by_fleet_size.png)

Mean queue and mean utilisation against fleet size for the ambulance and truck fleets under the shipped configuration, each with a 95% confidence ribbon and a dashed vertical line at the current establishment size.

<!-- GEN transport_high -->
| Fleet size | Ambulance mean queue | Truck mean queue |
|---|---|---|
| 1 | 2080.6801 [1946.9363, 2214.4240] | 0.0854 [0.0733, 0.0975] |
| 2 | 11.8888 [10.0922, 13.6854] | 0.0084 [0.0070, 0.0098] |
| 3 (current ambulance) | 1.7600 [1.4353, 2.0847] | 0.0013 [0.0010, 0.0017] |
| 4 (current truck) | 0.5873 [0.4363, 0.7383] | 0.0002 [0.0001, 0.0002] |
| 5 | 0.1618 [0.1270, 0.1966] | not swept |
<!-- /GEN -->

![Four panels of mean queue and utilisation against fleet size under the high intensity profile](../images/transport_capacity_margin_by_fleet_size_high_intensity.png)

The same measures under the `high_intensity` profile, each with a 95% confidence ribbon and a dashed vertical line at the current establishment size.

Under the shipped configuration a single ambulance queued <!-- GEN cell:transport|1|Ambulance mean queue|mean -->7.6600<!-- /GEN --> casualties (interval <!-- GEN cell:transport|1|Ambulance mean queue|ci -->[4.1208, 11.1993]<!-- /GEN -->), two queued <!-- GEN cell:transport|2|Ambulance mean queue|mean -->1.5242<!-- /GEN --> and the shipped three <!-- GEN cell:transport|3 (current ambulance)|Ambulance mean queue|mean -->0.4613<!-- /GEN -->. Under the high-intensity profile the corresponding ambulance queues were <!-- GEN cell:transport_high|1|Ambulance mean queue|mean -->2080.6801<!-- /GEN -->, <!-- GEN cell:transport_high|2|Ambulance mean queue|mean -->11.8888<!-- /GEN --> and <!-- GEN cell:transport_high|3 (current ambulance)|Ambulance mean queue|mean -->1.7600<!-- /GEN -->, and the shipped four trucks queued <!-- GEN cell:transport_high|4 (current truck)|Truck mean queue|mean -->0.0002<!-- /GEN -->.

#### Utilisation of Shared Fleets and Integral Evacuation Elements

**Question.** How heavily is each transport holder used, the shared brigade fleets and the evacuation elements integral to R2B and R2E? `[default and high_intensity · 360 d · 30 replications · pool totals, closing 90 d, shipped establishment, not swept]`

Utilisation is the time-weighted share of the established units in use over the closing 90 days. An evacuation crew is two medics seized together, so a crew is measured on its lead medic.

<!-- GEN transport_holders -->
| Holder | Asset | Moderate intensity mean queue | Moderate intensity utilisation | High intensity mean queue | High intensity utilisation |
|---|---|---|---|---|---|
| PMV Ambulance fleet | Shared | 0.461 [0.224, 0.699] | 24.6% [23.8%, 25.5%] | 1.760 [1.435, 2.085] | 51.1% [49.8%, 52.4%] |
| HX2 40M fleet | Shared | 0.001 [0.000, 0.002] | 2.5% [2.4%, 2.7%] | 0.000 [0.000, 0.000] | 5.7% [5.6%, 5.9%] |
| R2B evacuation crews | Integral | 0.121 [0.066, 0.176] | 13.9% [13.3%, 14.5%] | 0.622 [0.545, 0.700] | 41.6% [40.4%, 42.8%] |
| R2E evacuation sections | Integral | 0.000 [0.000, 0.000] | 0.0% [0.0%, 0.0%] | 0.000 [0.000, 0.000] | 1.1% [0.9%, 1.2%] |
<!-- /GEN -->

At moderate intensity the R2B evacuation crews were in use for <!-- GEN cell:transport_holders|R2B evacuation crews|Moderate intensity utilisation|mean -->13.9%<!-- /GEN --> of the window and the ambulance fleet for <!-- GEN cell:transport_holders|PMV Ambulance fleet|Moderate intensity utilisation|mean -->24.6%<!-- /GEN -->. At high intensity the crews were in use for <!-- GEN cell:transport_holders|R2B evacuation crews|High intensity utilisation|mean -->41.6%<!-- /GEN --> (interval <!-- GEN cell:transport_holders|R2B evacuation crews|High intensity utilisation|ci -->[40.4%, 42.8%]<!-- /GEN -->) and queued <!-- GEN cell:transport_holders|R2B evacuation crews|High intensity mean queue|mean -->0.622<!-- /GEN --> casualties, the ambulance fleet was in use for <!-- GEN cell:transport_holders|PMV Ambulance fleet|High intensity utilisation|mean -->51.1%<!-- /GEN --> and the truck fleet for <!-- GEN cell:transport_holders|HX2 40M fleet|High intensity utilisation|mean -->5.7%<!-- /GEN -->. The R2E evacuation sections were in use for <!-- GEN cell:transport_holders|R2E evacuation sections|High intensity utilisation|mean -->1.1%<!-- /GEN --> at high intensity.

### Forward Holding of Post-Operative Intensive Care

**Question.** What changes when a casualty operated on at R2B is held there for part of their post-operative intensive care, for a stability window or while R2E intensive care is saturated? `[default, seven forward holding arms · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN forward_hold -->
| Forward holding rule | R2E post-operative overflow census | R2B ICU utilisation | R2E ICU utilisation | Post-definitive care in ICU | Died of wounds per run |
|---|---|---|---|---|---|
| Off (current) | 5.72 [5.36, 6.09] | 1.4% | 86.8% | 35.2 [34.6, 35.9] | 21.17 [17.55, 24.79] |
| 120 min | 5.34 [4.96, 5.72] | 5.8% | 85.7% | 35.9 [35.2, 36.7] | 21.40 [18.22, 24.58] |
| 240 min | 5.10 [4.81, 5.39] | 10.5% | 85.7% | 36.7 [35.9, 37.4] | 21.37 [17.03, 25.71] |
| 480 min | 4.99 [4.61, 5.38] | 20.0% | 85.4% | 37.8 [37.0, 38.5] | 19.73 [17.88, 21.59] |
| 1,440 min | 3.94 [3.61, 4.26] | 44.6% | 82.8% | 41.9 [41.1, 42.8] | 26.83 [18.99, 34.68] |
| Capacity only | 5.01 [4.71, 5.31] | 10.9% | 87.6% | 37.1 [36.4, 37.7] | 21.10 [18.24, 23.96] |
| 240 min + capacity | 4.98 [4.67, 5.28] | 20.0% | 87.3% | 38.2 [37.5, 39.0] | 27.10 [18.19, 36.01] |
<!-- /GEN -->

![Five stacked panels against the forward holding rule](../images/r2b_forward_hold_frontier.png)

R2E post-operative overflow census, R2B and R2E intensive care utilisation, the share of post-definitive care delivered in intensive care and the died-of-wounds count under each forward holding arm, each with a 95% confidence ribbon. The windows apply to damage control and single-stage casualties alike.

The R2E post-operative overflow census was <!-- GEN cell:forward_hold|Off (current)|R2E post-operative overflow census|full -->5.72 [5.36, 6.09]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold|240 min|R2E post-operative overflow census|full -->5.10 [4.81, 5.39]<!-- /GEN --> with a 240-minute window. R2B intensive care utilisation rose from <!-- GEN cell:forward_hold|Off (current)|R2B ICU utilisation|full -->1.4%<!-- /GEN --> with no forward holding to <!-- GEN cell:forward_hold|1,440 min|R2B ICU utilisation|full -->44.6%<!-- /GEN --> at a 1,440-minute window while R2E intensive care utilisation moved from <!-- GEN cell:forward_hold|Off (current)|R2E ICU utilisation|full -->86.8%<!-- /GEN --> to <!-- GEN cell:forward_hold|1,440 min|R2E ICU utilisation|full -->82.8%<!-- /GEN -->. Post-definitive care in an intensive care bed rose from <!-- GEN cell:forward_hold|Off (current)|Post-definitive care in ICU|full -->35.2 [34.6, 35.9]<!-- /GEN --> to <!-- GEN cell:forward_hold|1,440 min|Post-definitive care in ICU|full -->41.9 [41.1, 42.8]<!-- /GEN -->, and died of wounds per run was <!-- GEN cell:forward_hold|Off (current)|Died of wounds per run|full -->21.17 [17.55, 24.79]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold|1,440 min|Died of wounds per run|full -->26.83 [18.99, 34.68]<!-- /GEN --> at 1,440 minutes. Holding on capacity grounds alone gave a post-operative overflow census of <!-- GEN cell:forward_hold|Capacity only|R2E post-operative overflow census|full -->5.01 [4.71, 5.31]<!-- /GEN -->, and a 240-minute window with the capacity trigger gave <!-- GEN cell:forward_hold|240 min + capacity|R2E post-operative overflow census|full -->4.98 [4.67, 5.28]<!-- /GEN -->.

**High-intensity profile.** The same seven arms under `high_intensity` `[high_intensity, seven forward holding arms · 360 d · 30 replications · pool totals, closing 90 d]`:

<!-- GEN forward_hold_high -->
| Forward holding rule | R2E post-operative overflow census | R2B ICU utilisation | R2E ICU utilisation | Post-definitive care in ICU | Died of wounds per run |
|---|---|---|---|---|---|
| Off (current) | 19.79 [19.47, 20.12] | 9.6% | 97.5% | 11.3 [11.1, 11.5] | 568.60 [500.55, 636.65] |
| 120 min | 19.61 [19.41, 19.82] | 19.6% | 97.5% | 11.3 [11.1, 11.5] | 530.53 [463.62, 597.45] |
| 240 min | 18.87 [18.67, 19.07] | 29.5% | 97.4% | 11.6 [11.4, 11.7] | 573.00 [493.77, 652.23] |
| 480 min | 18.12 [17.85, 18.40] | 45.0% | 97.3% | 11.9 [11.7, 12.2] | 533.97 [485.74, 582.20] |
| 1,440 min | 15.65 [15.41, 15.89] | 72.0% | 97.0% | 13.4 [13.2, 13.6] | 573.03 [504.20, 641.86] |
| Capacity only | 18.51 [18.24, 18.78] | 41.9% | 97.7% | 12.4 [12.2, 12.6] | 614.50 [565.71, 663.29] |
| 240 min + capacity | 17.43 [17.13, 17.74] | 51.9% | 97.5% | 12.9 [12.8, 13.1] | 615.83 [540.59, 691.08] |
<!-- /GEN -->

![Five stacked panels against the forward holding rule at high intensity](../images/r2b_forward_hold_frontier_high_intensity.png)

The panels of the figure above under the high-intensity profile.

The R2E post-operative overflow census was <!-- GEN cell:forward_hold_high|Off (current)|R2E post-operative overflow census|full -->19.79 [19.47, 20.12]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold_high|240 min|R2E post-operative overflow census|full -->18.87 [18.67, 19.07]<!-- /GEN --> with a 240-minute window. R2E intensive care utilisation was <!-- GEN cell:forward_hold_high|Off (current)|R2E ICU utilisation|full -->97.5%<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold_high|1,440 min|R2E ICU utilisation|full -->97.0%<!-- /GEN --> at a 1,440-minute window, while R2B intensive care utilisation rose from <!-- GEN cell:forward_hold_high|Off (current)|R2B ICU utilisation|full -->9.6%<!-- /GEN --> to <!-- GEN cell:forward_hold_high|1,440 min|R2B ICU utilisation|full -->72.0%<!-- /GEN -->. Died of wounds per run was <!-- GEN cell:forward_hold_high|Off (current)|Died of wounds per run|full -->568.60 [500.55, 636.65]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold_high|Capacity only|Died of wounds per run|full -->614.50 [565.71, 663.29]<!-- /GEN --> on the capacity trigger alone.

### Post-Operative Intensive Care Gate

**Question.** What does the rationing rule that defers lower-priority surgery when intensive care is saturated change? `[default, gate on and off · 360 d · 30 replications · paired, closing 90 d]`

<!-- GEN icu_gate -->
| Measure | Without the rule | With the rule | Paired difference |
|---|---|---|---|
| R2E ICU utilisation (%) | 85.9 [85.1, 86.7] | 86.8 [86.0, 87.6] | +0.9 [−0.1, +2.0] |
| Died of wounds per run | 21.37 [17.61, 25.13] | 21.17 [17.55, 24.79] | −0.20 [−5.03, +4.63] |
| Total casualties | 5,403.6 [5,317.8, 5,489.3] | 5,386.9 [5,321.5, 5,452.4] | −16.6 [−117.9, +84.7] |
<!-- /GEN -->

![Four panels comparing the post-operative intensive care gate disabled and enabled](../images/icu_gate.png)

Intensive care occupancy, deaths of wounds and the casualties taking each post-operative pathway with the gate disabled and enabled, each point with its 95% confidence interval.

R2E intensive care utilisation was <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|Without the rule|full -->85.9 [85.1, 86.7]<!-- /GEN --> without the rule and <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|With the rule|full -->86.8 [86.0, 87.6]<!-- /GEN --> with it, a paired difference of <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|Paired difference|full -->+0.9 [−0.1, +2.0]<!-- /GEN --> percentage points. Died of wounds per run were <!-- GEN cell:icu_gate|Died of wounds per run|Without the rule|full -->21.37 [17.61, 25.13]<!-- /GEN --> and <!-- GEN cell:icu_gate|Died of wounds per run|With the rule|full -->21.17 [17.55, 24.79]<!-- /GEN -->, a paired difference of <!-- GEN cell:icu_gate|Died of wounds per run|Paired difference|full -->−0.20 [−5.03, +4.63]<!-- /GEN -->.

<!-- GEN icu_gate_pathways -->
| Recovery pathway | Casualty-replications | Died of wounds | Rate |
|---|---|---|---|
| Intensive care bed | 34,579 | 157 | 0.45% |
| Holding bed | 1,363 | 5 | 0.37% |
<!-- /GEN -->

Where the rule was in force, casualties recovering in a holding bed died of wounds at <!-- GEN cell:icu_gate_pathways|Holding bed|Rate|full -->0.37%<!-- /GEN --> and those recovering in an intensive care bed at <!-- GEN cell:icu_gate_pathways|Intensive care bed|Rate|full -->0.45%<!-- /GEN -->, pooled over the replications of that arm.

**High-intensity profile.** The same comparison under `high_intensity` `[high_intensity, gate disabled and enabled · 360 d · 30 replications · campaign totals, paired differences]`:

<!-- GEN icu_gate_high -->
| Measure | Without the rule | With the rule | Paired difference |
|---|---|---|---|
| R2E ICU utilisation (%) | 97.4 [97.3, 97.5] | 97.5 [97.4, 97.7] | +0.1 [−0.0, +0.2] |
| Died of wounds per run | 597.93 [532.47, 663.40] | 568.60 [500.55, 636.65] | −29.33 [−138.46, +79.79] |
| Total casualties | 12,466.8 [12,387.6, 12,546.1] | 12,379.7 [12,227.9, 12,531.5] | −87.1 [−257.8, +83.5] |
<!-- /GEN -->

<!-- GEN icu_gate_pathways_high -->
| Recovery pathway | Casualty-replications | Died of wounds | Rate |
|---|---|---|---|
| Intensive care bed | 104,807 | 5,924 | 5.65% |
| Holding bed | 354 | 2 | 0.56% |
<!-- /GEN -->

The paired difference in deaths of wounds per run with the rule in force was <!-- GEN cell:icu_gate_high|Died of wounds per run|Paired difference|full -->−29.33 [−138.46, +79.79]<!-- /GEN -->. Casualties recovering in a holding bed died of wounds at <!-- GEN cell:icu_gate_pathways_high|Holding bed|Rate|full -->0.56%<!-- /GEN --> and those recovering in an intensive care bed at <!-- GEN cell:icu_gate_pathways_high|Intensive care bed|Rate|full -->5.65%<!-- /GEN -->.

### Evacuation Policy

**Question.** What happens to returns to duty, mortality and the R2E pools when the days a casualty may recover in theatre before evacuation is set across its doctrinal range? `[default, evacuation policy 15 to 60 days · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN policy -->
| Response | 15 d | 21 d (shipped) | 30 d | 45 d | 60 d |
|---|---|---|---|---|---|
| R2E hold occupancy (%) | 96.5 [96.0, 97.1] | 99.6 [99.4, 99.7] | 99.9 [99.9, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] |
| R2E hold mean queue | 9.54 [8.74, 10.34] | 24.38 [22.59, 26.16] | 43.32 [41.45, 45.19] | 78.38 [75.21, 81.55] | 139.79 [133.72, 145.85] |
| R2E ICU occupancy (%) | 86.6 [86.1, 87.1] | 86.8 [86.0, 87.6] | 87.1 [85.9, 88.2] | 86.4 [85.6, 87.2] | 86.6 [85.8, 87.3] |
| R2E post-operative overflow census | 4.82 [4.47, 5.16] | 5.72 [5.36, 6.09] | 6.10 [5.70, 6.50] | 5.72 [5.35, 6.10] | 5.97 [5.60, 6.34] |
| R2E staging overflow census | 30.0 [21.7, 38.4] | 32.2 [26.9, 37.5] | 33.6 [22.9, 44.3] | 20.5 [17.3, 23.6] | 16.4 [13.6, 19.1] |
| Post-definitive ICU access (%) | 35.5 [34.7, 36.2] | 35.2 [34.6, 35.9] | 35.6 [34.7, 36.5] | 35.8 [35.1, 36.5] | 35.8 [35.0, 36.5] |
| In-theatre share (%) | 27.1 [26.6, 27.5] | 40.2 [39.8, 40.7] | 49.0 [48.4, 49.5] | 59.3 [58.8, 59.8] | 71.1 [70.7, 71.5] |
| Returns to duty | 1560.3 [1535.6, 1585.1] | 2062.4 [2031.8, 2093.0] | 2360.5 [2318.5, 2402.5] | 2673.6 [2642.7, 2704.6] | 3039.8 [2997.9, 3081.7] |
| Died of wounds | 21.93 [19.39, 24.47] | 21.17 [17.55, 24.79] | 22.03 [18.14, 25.93] | 19.47 [17.17, 21.76] | 19.97 [17.28, 22.65] |
| Never evacuated by horizon | 41.8 [30.6, 53.0] | 36.3 [27.0, 45.6] | 39.1 [29.2, 49.0] | 19.6 [14.9, 24.4] | 13.2 [10.0, 16.4] |
| Mean evacuation wait (d) | 5.08 [4.60, 5.56] | 4.90 [4.55, 5.24] | 5.17 [4.37, 5.97] | 4.54 [4.24, 4.85] | 4.36 [4.13, 4.59] |
| Role 4 peak beds | 229.9 [224.9, 234.9] | 209.2 [205.2, 213.2] | 194.6 [190.2, 199.0] | 176.4 [172.0, 180.8] | 155.1 [149.4, 160.8] |
<!-- /GEN -->

![Six panels of returns to duty, deaths of wounds, in-theatre share and R2E queues against evacuation policy days](../images/policy_sweep.png)

Six responses against the evacuation policy in days, each with its 95% confidence interval and the shipped policy marked by a dashed line.

At the shipped 21-day policy the R2E holding queue was <!-- GEN cell:policy|R2E hold mean queue|21 d (shipped)|full -->24.38 [22.59, 26.16]<!-- /GEN --> and its occupancy <!-- GEN cell:policy|R2E hold occupancy (%)|21 d (shipped)|full -->99.6 [99.4, 99.7]<!-- /GEN -->. At 30 days the holding occupancy was <!-- GEN cell:policy|R2E hold occupancy (%)|30 d|full -->99.9 [99.9, 100.0]<!-- /GEN --> and the queue <!-- GEN cell:policy|R2E hold mean queue|30 d|full -->43.32 [41.45, 45.19]<!-- /GEN -->; at 45 and 60 days the occupancy was <!-- GEN cell:policy|R2E hold occupancy (%)|45 d|full -->100.0 [100.0, 100.0]<!-- /GEN --> and <!-- GEN cell:policy|R2E hold occupancy (%)|60 d|full -->100.0 [100.0, 100.0]<!-- /GEN -->. Returns to duty per campaign were <!-- GEN cell:policy|Returns to duty|15 d|full -->1560.3 [1535.6, 1585.1]<!-- /GEN --> at 15 days, <!-- GEN cell:policy|Returns to duty|21 d (shipped)|full -->2062.4 [2031.8, 2093.0]<!-- /GEN --> at 21, <!-- GEN cell:policy|Returns to duty|30 d|full -->2360.5 [2318.5, 2402.5]<!-- /GEN --> at 30, <!-- GEN cell:policy|Returns to duty|45 d|full -->2673.6 [2642.7, 2704.6]<!-- /GEN --> at 45 and <!-- GEN cell:policy|Returns to duty|60 d|full -->3039.8 [2997.9, 3081.7]<!-- /GEN --> at 60. Died of wounds were <!-- GEN cell:policy|Died of wounds|15 d|full -->21.93 [19.39, 24.47]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|21 d (shipped)|full -->21.17 [17.55, 24.79]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|30 d|full -->22.03 [18.14, 25.93]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|45 d|full -->19.47 [17.17, 21.76]<!-- /GEN --> and <!-- GEN cell:policy|Died of wounds|60 d|full -->19.97 [17.28, 22.65]<!-- /GEN --> across the five policies. The realised in-theatre share was <!-- GEN cell:policy|In-theatre share (%)|15 d|full -->27.1 [26.6, 27.5]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|21 d (shipped)|full -->40.2 [39.8, 40.7]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|30 d|full -->49.0 [48.4, 49.5]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|45 d|full -->59.3 [58.8, 59.8]<!-- /GEN --> and <!-- GEN cell:policy|In-theatre share (%)|60 d|full -->71.1 [70.7, 71.5]<!-- /GEN --> percent. Paired differences against the shipped policy are in `data/policy/policy_sweep_paired.csv`.

### R2E Holding Establishment

**Question.** What does enlarging the R2E holding establishment change at the shipped evacuation policy? `[default, 21-day policy, 30 to 90 holding beds · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN establishment -->
| Response | 30 beds (shipped) | 45 beds | 60 beds | 90 beds |
|---|---|---|---|---|
| R2E hold occupancy (%) | 99.6 [99.4, 99.7] | 97.5 [97.0, 97.9] | 93.3 [92.0, 94.7] | 82.6 [80.8, 84.5] |
| R2E hold mean queue | 24.38 [22.59, 26.16] | 18.71 [17.21, 20.20] | 13.15 [11.56, 14.75] | 5.24 [4.18, 6.29] |
| R2E post-operative overflow census | 5.72 [5.36, 6.09] | 4.83 [4.35, 5.31] | 3.73 [3.27, 4.19] | 1.84 [1.45, 2.23] |
| R2E staging overflow census | 32.2 [26.9, 37.5] | 29.9 [18.7, 41.0] | 20.7 [14.0, 27.4] | 7.6 [4.5, 10.6] |
| Post-definitive ICU access (%) | 35.2 [34.6, 35.9] | 35.3 [34.4, 36.1] | 35.8 [34.8, 36.9] | 35.6 [34.8, 36.4] |
| In-theatre share (%) | 40.2 [39.8, 40.7] | 40.1 [39.6, 40.7] | 40.1 [39.4, 40.7] | 40.4 [39.9, 40.9] |
| Returns to duty | 2062.4 [2031.8, 2093.0] | 2055.3 [2026.0, 2084.6] | 2023.4 [1983.1, 2063.7] | 2044.1 [2013.6, 2074.6] |
| Died of wounds | 21.2 [17.5, 24.8] | 23.9 [20.5, 27.3] | 21.4 [18.0, 24.7] | 19.1 [16.3, 21.8] |
| Never evacuated by horizon | 36.3 [27.0, 45.6] | 41.5 [27.9, 55.2] | 32.2 [24.7, 39.8] | 41.2 [31.1, 51.3] |
| Role 4 peak beds | 209.2 [205.2, 213.2] | 212.8 [208.4, 217.3] | 209.1 [204.1, 214.0] | 207.7 [202.9, 212.5] |
<!-- /GEN -->

![Six panels of campaign responses against R2E holding beds](../images/establishment_sweep.png)

The same six responses against the R2E holding bed establishment at the shipped evacuation policy, with the shipped establishment marked by a dashed line.

Holding occupancy was <!-- GEN cell:establishment|R2E hold occupancy (%)|30 beds (shipped)|full -->99.6 [99.4, 99.7]<!-- /GEN --> at the shipped 30 beds, <!-- GEN cell:establishment|R2E hold occupancy (%)|45 beds|full -->97.5 [97.0, 97.9]<!-- /GEN --> at 45, <!-- GEN cell:establishment|R2E hold occupancy (%)|60 beds|full -->93.3 [92.0, 94.7]<!-- /GEN --> at 60 and <!-- GEN cell:establishment|R2E hold occupancy (%)|90 beds|full -->82.6 [80.8, 84.5]<!-- /GEN --> at 90 percent. The R2E holding queue was <!-- GEN cell:establishment|R2E hold mean queue|30 beds (shipped)|full -->24.38 [22.59, 26.16]<!-- /GEN --> at 30 beds and <!-- GEN cell:establishment|R2E hold mean queue|45 beds|full -->18.71 [17.21, 20.20]<!-- /GEN --> at 45. Returns to duty per campaign were <!-- GEN cell:establishment|Returns to duty|30 beds (shipped)|full -->2062.4 [2031.8, 2093.0]<!-- /GEN --> at 30 beds and <!-- GEN cell:establishment|Returns to duty|90 beds|full -->2044.1 [2013.6, 2074.6]<!-- /GEN --> at 90, and died of wounds <!-- GEN cell:establishment|Died of wounds|30 beds (shipped)|full -->21.2 [17.5, 24.8]<!-- /GEN --> and <!-- GEN cell:establishment|Died of wounds|90 beds|full -->19.1 [16.3, 21.8]<!-- /GEN -->.

### High-Intensity Evacuation Policy and Holding Establishment

**Question.** What happens to the R2E pools, returns to duty and mortality under the high-intensity profile when the evacuation policy and the R2E holding establishment are set across their ranges together? `[high_intensity, evacuation policy 15 to 60 days, 30 to 90 holding beds · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN policy_establishment_high -->
| Policy and holding beds | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Died of wounds | Never evacuated by horizon | Role 4 peak beds |
|---|---|---|---|---|---|---|
| 15 d, 30 beds | 30.7 [29.7, 31.7] | 19.8 [19.5, 20.0] | 2,065.7 [2,039.5, 2,091.8] | 552.1 [497.2, 607.0] | 3,169.2 [3,110.7, 3,227.6] | 287.4 [286.1, 288.7] |
| 15 d, 45 beds | 31.0 [30.0, 32.1] | 19.7 [19.4, 20.1] | 2,050.2 [2,026.4, 2,074.0] | 538.2 [481.8, 594.5] | 3,186.1 [3,136.4, 3,235.8] | 288.2 [286.9, 289.4] |
| 15 d, 60 beds | 33.2 [32.1, 34.4] | 19.8 [19.5, 20.0] | 2,089.7 [2,065.0, 2,114.4] | 524.9 [471.3, 578.5] | 3,222.8 [3,168.1, 3,277.5] | 287.4 [285.9, 288.9] |
| 15 d, 90 beds | 31.6 [30.6, 32.6] | 19.7 [19.5, 19.9] | 2,055.6 [2,025.1, 2,086.0] | 566.3 [521.0, 611.6] | 3,213.6 [3,160.5, 3,266.6] | 287.5 [285.9, 289.0] |
| 21 d, 30 beds | 54.1 [52.0, 56.1] | 19.8 [19.5, 20.1] | 2,861.9 [2,824.1, 2,899.7] | 568.6 [500.5, 636.7] | 3,041.1 [2,993.0, 3,089.1] | 281.3 [279.0, 283.6] |
| 21 d, 45 beds | 53.1 [51.0, 55.2] | 19.5 [19.2, 19.8] | 2,848.9 [2,815.7, 2,882.1] | 628.1 [526.2, 729.9] | 3,021.8 [2,960.8, 3,082.8] | 282.8 [280.1, 285.5] |
| 21 d, 60 beds | 56.5 [55.0, 58.0] | 19.9 [19.7, 20.1] | 2,859.2 [2,831.3, 2,887.1] | 561.1 [494.1, 628.1] | 3,044.8 [3,006.7, 3,082.9] | 280.5 [277.7, 283.2] |
| 21 d, 90 beds | 56.6 [54.2, 59.1] | 19.5 [19.3, 19.8] | 2,845.5 [2,822.5, 2,868.5] | 574.9 [487.8, 662.1] | 3,012.7 [2,951.2, 3,074.2] | 282.4 [280.1, 284.7] |
| 30 d, 30 beds | 92.5 [90.2, 94.8] | 19.9 [19.6, 20.2] | 3,414.7 [3,386.4, 3,443.0] | 578.3 [519.9, 636.6] | 2,967.0 [2,926.5, 3,007.5] | 263.3 [260.3, 266.4] |
| 30 d, 45 beds | 80.9 [78.0, 83.8] | 19.9 [19.6, 20.2] | 3,440.0 [3,397.5, 3,482.6] | 613.6 [547.8, 679.4] | 2,959.7 [2,914.7, 3,004.7] | 262.4 [259.6, 265.1] |
| 30 d, 60 beds | 75.6 [73.2, 78.1] | 19.9 [19.7, 20.2] | 3,404.5 [3,369.5, 3,439.5] | 518.8 [474.2, 563.5] | 2,966.4 [2,925.8, 3,007.0] | 262.3 [260.1, 264.5] |
| 30 d, 90 beds | 79.4 [77.7, 81.2] | 19.7 [19.5, 20.0] | 3,439.4 [3,402.0, 3,476.8] | 573.8 [515.8, 631.9] | 2,928.0 [2,885.0, 2,971.0] | 261.8 [259.3, 264.4] |
| 45 d, 30 beds | 190.0 [186.1, 194.0] | 20.0 [19.8, 20.2] | 4,294.0 [4,248.7, 4,339.3] | 647.9 [560.3, 735.5] | 2,674.7 [2,622.6, 2,726.7] | 226.0 [224.4, 227.7] |
| 45 d, 45 beds | 175.3 [171.3, 179.4] | 19.8 [19.6, 20.1] | 4,294.1 [4,261.5, 4,326.7] | 612.9 [535.6, 690.2] | 2,656.2 [2,606.8, 2,705.6] | 225.8 [223.8, 227.8] |
| 45 d, 60 beds | 163.6 [159.8, 167.4] | 19.9 [19.7, 20.2] | 4,272.6 [4,227.6, 4,317.5] | 639.1 [545.8, 732.4] | 2,659.8 [2,599.4, 2,720.3] | 225.1 [223.3, 226.8] |
| 45 d, 90 beds | 133.0 [129.1, 136.8] | 19.8 [19.6, 20.1] | 4,270.4 [4,223.5, 4,317.3] | 607.1 [522.4, 691.9] | 2,662.9 [2,613.3, 2,712.4] | 224.9 [223.0, 226.8] |
| 60 d, 30 beds | 326.4 [319.8, 333.0] | 19.9 [19.6, 20.2] | 5,119.6 [5,071.4, 5,167.8] | 718.6 [629.8, 807.4] | 2,263.7 [2,213.0, 2,314.5] | 195.9 [195.0, 196.7] |
| 60 d, 45 beds | 314.7 [310.0, 319.3] | 19.9 [19.7, 20.2] | 5,106.4 [5,072.2, 5,140.6] | 624.1 [558.6, 689.5] | 2,271.4 [2,226.4, 2,316.4] | 196.3 [194.8, 197.8] |
| 60 d, 60 beds | 293.3 [287.4, 299.2] | 19.9 [19.6, 20.1] | 5,064.5 [5,020.8, 5,108.2] | 600.8 [516.7, 684.9] | 2,263.8 [2,218.6, 2,308.9] | 196.5 [195.0, 197.9] |
| 60 d, 90 beds | 263.7 [258.1, 269.3] | 19.7 [19.4, 19.9] | 5,055.1 [5,014.4, 5,095.8] | 724.6 [612.1, 837.1] | 2,200.0 [2,145.1, 2,254.9] | 195.4 [194.3, 196.5] |
<!-- /GEN -->

At the shipped 21-day policy and 30 holding beds the closing-window R2E holding queue was <!-- GEN cell:policy_establishment_high|21 d, 30 beds|R2E hold mean queue|full -->54.1 [52.0, 56.1]<!-- /GEN --> casualties and the post-operative overflow census <!-- GEN cell:policy_establishment_high|21 d, 30 beds|R2E post-operative overflow census|full -->19.8 [19.5, 20.1]<!-- /GEN -->. The holding queue was <!-- GEN cell:policy_establishment_high|15 d, 30 beds|R2E hold mean queue|full -->30.7 [29.7, 31.7]<!-- /GEN --> at 15 days and 30 beds and <!-- GEN cell:policy_establishment_high|60 d, 30 beds|R2E hold mean queue|full -->326.4 [319.8, 333.0]<!-- /GEN --> at 60 days and 30 beds, and <!-- GEN cell:policy_establishment_high|60 d, 90 beds|R2E hold mean queue|full -->263.7 [258.1, 269.3]<!-- /GEN --> at 60 days and 90 beds. The post-operative overflow census lay between <!-- GEN cell:policy_establishment_high|60 d, 60 beds|R2E post-operative overflow census|full -->19.9 [19.6, 20.1]<!-- /GEN --> at 60 days and 60 beds and <!-- GEN cell:policy_establishment_high|21 d, 30 beds|R2E post-operative overflow census|full -->19.8 [19.5, 20.1]<!-- /GEN --> at the shipped setting.

<!-- GEN policy_establishment_high_stability -->
| Policy and holding beds | Holding queue not growing | Post-operative overflow not growing | Both not growing | Staging overflow not growing | R2E pools across the cell |
|---|---|---|---|---|---|
| 15 d, 30 beds | 24 of 30 | 29 of 30 | 23 of 30 | 0 of 30 | at least one growing |
| 15 d, 45 beds | 28 of 30 | 27 of 30 | 26 of 30 | 0 of 30 | at least one growing |
| 15 d, 60 beds | 27 of 30 | 30 of 30 | 27 of 30 | 0 of 30 | at least one growing |
| 15 d, 90 beds | 28 of 30 | 29 of 30 | 27 of 30 | 0 of 30 | at least one growing |
| 21 d, 30 beds | 28 of 30 | 29 of 30 | 27 of 30 | 0 of 30 | at least one growing |
| 21 d, 45 beds | 27 of 30 | 30 of 30 | 27 of 30 | 0 of 30 | at least one growing |
| 21 d, 60 beds | 29 of 30 | 29 of 30 | 28 of 30 | 0 of 30 | at least one growing |
| 21 d, 90 beds | 28 of 30 | 28 of 30 | 26 of 30 | 0 of 30 | at least one growing |
| 30 d, 30 beds | 29 of 30 | 30 of 30 | 29 of 30 | 0 of 30 | at least one growing |
| 30 d, 45 beds | 30 of 30 | 29 of 30 | 29 of 30 | 0 of 30 | at least one growing |
| 30 d, 60 beds | 29 of 30 | 30 of 30 | 29 of 30 | 0 of 30 | at least one growing |
| 30 d, 90 beds | 25 of 30 | 30 of 30 | 25 of 30 | 0 of 30 | at least one growing |
| 45 d, 30 beds | 28 of 30 | 30 of 30 | 28 of 30 | 0 of 30 | at least one growing |
| 45 d, 45 beds | 30 of 30 | 30 of 30 | 30 of 30 | 0 of 30 | at least one growing |
| 45 d, 60 beds | 30 of 30 | 30 of 30 | 30 of 30 | 0 of 30 | at least one growing |
| 45 d, 90 beds | 26 of 30 | 30 of 30 | 26 of 30 | 0 of 30 | at least one growing |
| 60 d, 30 beds | 27 of 30 | 29 of 30 | 26 of 30 | 0 of 30 | at least one growing |
| 60 d, 45 beds | 29 of 30 | 30 of 30 | 29 of 30 | 0 of 30 | at least one growing |
| 60 d, 60 beds | 30 of 30 | 30 of 30 | 30 of 30 | 0 of 30 | at least one growing |
| 60 d, 90 beds | 29 of 30 | 29 of 30 | 28 of 30 | 0 of 30 | at least one growing |
<!-- /GEN -->

The table counts, for each cell, the replications in which each R2E pool was not still rising over the late half of the campaign, and classifies the cell as a whole: a pool is growing across the cell where the mean of its replications' late-half slopes is positive with a 95% interval excluding zero and a drift of at least one percent of its level per 30-day block, the rule of [Classifying Stability](Methods.md#classifying-stability), and the cell holds where no pool is growing. The staging overflow census was not growing in <!-- GEN cell:policy_establishment_high_stability|21 d, 30 beds|Staging overflow not growing|full -->0 of 30<!-- /GEN --> replications at the shipped cell and in <!-- GEN cell:policy_establishment_high_stability|60 d, 90 beds|Staging overflow not growing|full -->0 of 30<!-- /GEN --> at 60 days and 90 beds, and the shipped cell was classified <!-- GEN cell:policy_establishment_high_stability|21 d, 30 beds|R2E pools across the cell|full -->at least one growing<!-- /GEN -->.

<!-- GEN policy_establishment_high_paired -->
| Policy and holding beds | Returns to duty | Died of wounds | Role 4 peak beds |
|---|---|---|---|
| 15 d, 30 beds | −796.2 [−844.4, −748.0] | −16.5 [−115.1, +82.1] | +6.1 [+3.1, +9.1] |
| 15 d, 45 beds | −811.7 [−854.9, −768.5] | −30.4 [−115.9, +55.0] | +6.9 [+3.9, +9.8] |
| 15 d, 60 beds | −772.2 [−811.8, −732.5] | −43.7 [−132.8, +45.4] | +6.1 [+2.9, +9.3] |
| 15 d, 90 beds | −806.3 [−851.3, −761.4] | −2.3 [−83.4, +78.8] | +6.2 [+3.1, +9.2] |
| 21 d, 45 beds | −13.0 [−58.9, +32.9] | +59.5 [−56.2, +175.1] | +1.5 [−1.8, +4.9] |
| 21 d, 60 beds | −2.7 [−41.1, +35.7] | −7.5 [−95.8, +80.8] | −0.8 [−4.4, +2.7] |
| 21 d, 90 beds | −16.4 [−60.5, +27.7] | +6.3 [−102.9, +115.6] | +1.1 [−1.8, +4.0] |
| 30 d, 30 beds | +552.8 [+502.9, +602.7] | +9.7 [−80.9, +100.2] | −18.0 [−21.4, −14.5] |
| 30 d, 45 beds | +578.1 [+521.9, +634.3] | +45.0 [−58.3, +148.3] | −18.9 [−22.5, −15.4] |
| 30 d, 60 beds | +542.6 [+486.3, +598.9] | −49.8 [−138.0, +38.4] | −19.0 [−22.6, −15.4] |
| 30 d, 90 beds | +577.5 [+525.7, +629.3] | +5.2 [−79.7, +90.2] | −19.5 [−22.8, −16.1] |
| 45 d, 30 beds | +1,432.1 [+1,383.3, +1,480.9] | +79.3 [−30.8, +189.3] | −55.3 [−57.6, −53.0] |
| 45 d, 45 beds | +1,432.2 [+1,377.7, +1,486.7] | +44.3 [−54.5, +143.1] | −55.5 [−58.7, −52.3] |
| 45 d, 60 beds | +1,410.7 [+1,357.8, +1,463.5] | +70.5 [−37.1, +178.1] | −56.2 [−59.1, −53.4] |
| 45 d, 90 beds | +1,408.5 [+1,344.7, +1,472.4] | +38.5 [−66.6, +143.6] | −56.4 [−59.6, −53.2] |
| 60 d, 30 beds | +2,257.7 [+2,203.1, +2,312.3] | +150.0 [+35.6, +264.4]† | −85.4 [−87.9, −82.9] |
| 60 d, 45 beds | +2,244.5 [+2,188.7, +2,300.3] | +55.5 [−42.1, +153.0] | −85.0 [−88.1, −81.8] |
| 60 d, 60 beds | +2,202.6 [+2,140.3, +2,264.9] | +32.2 [−85.7, +150.1] | −84.8 [−87.6, −82.1] |
| 60 d, 90 beds | +2,193.2 [+2,137.1, +2,249.3] | +156.0 [+44.6, +267.4]† | −85.9 [−88.9, −82.9] |
<!-- /GEN -->

Each cell was paired by replication against the shipped 21-day policy at 30 holding beds. At 30 days and 90 beds returns to duty differed by <!-- GEN cell:policy_establishment_high_paired|30 d, 90 beds|Returns to duty|full -->+577.5 [+525.7, +629.3]<!-- /GEN --> and Role 4 peak beds by <!-- GEN cell:policy_establishment_high_paired|30 d, 90 beds|Role 4 peak beds|full -->−19.5 [−22.8, −16.1]<!-- /GEN -->.

For each holding establishment the largest policy at which the cell held the R2E pools, by the rule of the preceding table, is given below; where none did, the cell reads as such.

<!-- GEN policy_grid_largest_high -->
| Holding beds | Largest policy holding the R2E pools | Returns to duty there | Paired gain over 21 d and 30 beds | Gain of the added beds at that policy |
|---|---|---|---|---|
| 30 | none in the grid | not applicable | not applicable | not applicable |
| 45 | none in the grid | not applicable | not applicable | not applicable |
| 60 | none in the grid | not applicable | not applicable | not applicable |
| 90 | none in the grid | not applicable | not applicable | not applicable |
<!-- /GEN -->

### High-Intensity Strategic Sortie Interval by Policy and Holding Establishment

**Question.** Does a shorter interval between strategic sorties, together with a longer evacuation policy or more R2E holding beds, keep the R2E pools below the collapse threshold under the high-intensity profile? `[high_intensity, sortie interval 3 to 7 days, evacuation policy 21 and 30 days, 30 and 60 holding beds · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN interval_grid_high -->
| Interval, policy and holding beds | R2E hold mean queue | R2E post-operative overflow census | Pools |
|---|---|---|---|
| 3 d interval, 21 d policy, 30 beds | 51.8 [50.2, 53.5] | 19.8 [19.5, 20.0] | at least one growing |
| 3 d interval, 21 d policy, 60 beds | 42.6 [41.3, 44.0] | 19.3 [19.0, 19.6] | at least one growing |
| 3 d interval, 30 d policy, 30 beds | 90.1 [87.5, 92.6] | 20.0 [19.8, 20.2] | at least one growing |
| 3 d interval, 30 d policy, 60 beds | 67.3 [65.3, 69.3] | 19.7 [19.5, 20.0] | at least one growing |
| 5 d interval, 21 d policy, 30 beds | 53.1 [51.4, 54.8] | 19.6 [19.3, 19.9] | at least one growing |
| 5 d interval, 21 d policy, 60 beds | 54.7 [53.2, 56.1] | 19.9 [19.6, 20.2] | at least one growing |
| 5 d interval, 30 d policy, 30 beds | 93.3 [91.1, 95.5] | 19.9 [19.6, 20.1] | at least one growing |
| 5 d interval, 30 d policy, 60 beds | 73.0 [71.0, 75.1] | 19.7 [19.4, 20.0] | at least one growing |
| 7 d interval, 21 d policy, 30 beds | 54.1 [52.0, 56.1] | 19.8 [19.5, 20.1] | at least one growing |
| 7 d interval, 21 d policy, 60 beds | 56.5 [55.0, 58.0] | 19.9 [19.7, 20.1] | at least one growing |
| 7 d interval, 30 d policy, 30 beds | 92.5 [90.2, 94.8] | 19.9 [19.6, 20.2] | at least one growing |
| 7 d interval, 30 d policy, 60 beds | 75.6 [73.2, 78.1] | 19.9 [19.7, 20.2] | at least one growing |
<!-- /GEN -->

The last column classifies each cell by the rule of the preceding grid. At the shipped cell, the seven-day interval with the 21-day policy and 30 beds, the closing-window R2E holding queue was <!-- GEN cell:interval_grid_high|7 d interval, 21 d policy, 30 beds|R2E hold mean queue|full -->54.1 [52.0, 56.1]<!-- /GEN --> casualties and the post-operative overflow census <!-- GEN cell:interval_grid_high|7 d interval, 21 d policy, 30 beds|R2E post-operative overflow census|full -->19.8 [19.5, 20.1]<!-- /GEN -->. At the three-day interval with the same policy and establishment they were <!-- GEN cell:interval_grid_high|3 d interval, 21 d policy, 30 beds|R2E hold mean queue|full -->51.8 [50.2, 53.5]<!-- /GEN --> and <!-- GEN cell:interval_grid_high|3 d interval, 21 d policy, 30 beds|R2E post-operative overflow census|full -->19.8 [19.5, 20.0]<!-- /GEN -->, and with 60 beds <!-- GEN cell:interval_grid_high|3 d interval, 21 d policy, 60 beds|R2E hold mean queue|full -->42.6 [41.3, 44.0]<!-- /GEN --> and <!-- GEN cell:interval_grid_high|3 d interval, 21 d policy, 60 beds|R2E post-operative overflow census|full -->19.3 [19.0, 19.6]<!-- /GEN -->. The three-day, 21-day, 60-bed cell was classified <!-- GEN cell:interval_grid_high|3 d interval, 21 d policy, 60 beds|Pools|full -->at least one growing<!-- /GEN -->, as was the three-day, 30-day, 60-bed cell (<!-- GEN cell:interval_grid_high|3 d interval, 30 d policy, 60 beds|Pools|full -->at least one growing<!-- /GEN -->).

<!-- GEN interval_grid_high_wait -->
| Interval, policy and holding beds | Mean wait, critical channel (days) | Mean wait, standard channel (days) | Still waiting, critical channel | Still waiting, standard channel |
|---|---|---|---|---|
| 3 d interval, 21 d policy, 30 beds | 22.44 [20.84, 24.04] | 1.51 [1.50, 1.52] | 608.0 [559.8, 656.2] | 17.5 [15.1, 19.9] |
| 3 d interval, 21 d policy, 60 beds | 21.52 [19.82, 23.23] | 1.50 [1.49, 1.51] | 606.7 [557.5, 655.9] | 18.6 [16.7, 20.6] |
| 3 d interval, 30 d policy, 30 beds | 19.72 [18.31, 21.14] | 1.50 [1.49, 1.51] | 550.7 [509.7, 591.7] | 13.8 [12.1, 15.4] |
| 3 d interval, 30 d policy, 60 beds | 19.64 [18.04, 21.24] | 1.50 [1.49, 1.51] | 525.0 [473.2, 576.8] | 13.0 [11.7, 14.4] |
| 5 d interval, 21 d policy, 30 beds | 82.44 [80.92, 83.96] | 2.50 [2.49, 2.52] | 2,298.9 [2,241.4, 2,356.4] | 28.9 [26.1, 31.7] |
| 5 d interval, 21 d policy, 60 beds | 82.43 [81.14, 83.71] | 2.50 [2.49, 2.52] | 2,321.2 [2,278.8, 2,363.6] | 30.8 [27.7, 34.0] |
| 5 d interval, 30 d policy, 30 beds | 80.12 [78.71, 81.54] | 2.51 [2.50, 2.52] | 2,230.4 [2,193.7, 2,267.2] | 21.6 [19.2, 24.1] |
| 5 d interval, 30 d policy, 60 beds | 80.93 [79.44, 82.42] | 2.50 [2.49, 2.51] | 2,199.5 [2,130.3, 2,268.8] | 22.7 [20.7, 24.8] |
| 7 d interval, 21 d policy, 30 beds | 109.67 [108.40, 110.95] | 3.56 [3.53, 3.58] | 3,022.2 [2,974.9, 3,069.4] | 18.9 [16.5, 21.3] |
| 7 d interval, 21 d policy, 60 beds | 109.57 [108.23, 110.92] | 3.54 [3.52, 3.56] | 3,026.4 [2,988.9, 3,064.0] | 18.4 [16.2, 20.6] |
| 7 d interval, 30 d policy, 30 beds | 109.38 [107.98, 110.79] | 3.51 [3.48, 3.54] | 2,952.5 [2,911.7, 2,993.4] | 14.5 [12.9, 16.1] |
| 7 d interval, 30 d policy, 60 beds | 108.83 [107.56, 110.10] | 3.48 [3.47, 3.50] | 2,953.8 [2,912.9, 2,994.7] | 12.6 [11.1, 14.2] |
<!-- /GEN -->

The mean wait is reported separately for the casualties who boarded on the critical (intensive care) channel and on the standard channel, with the casualties of each channel decided for evacuation who had not boarded at the close. At the three-day interval with the 21-day policy and 30 beds the mean wait was <!-- GEN cell:interval_grid_high_wait|3 d interval, 21 d policy, 30 beds|Mean wait, critical channel (days)|full -->22.44 [20.84, 24.04]<!-- /GEN --> days on the critical channel against <!-- GEN cell:interval_grid_high_wait|3 d interval, 21 d policy, 30 beds|Mean wait, standard channel (days)|full -->1.51 [1.50, 1.52]<!-- /GEN --> days on the standard channel, and <!-- GEN cell:interval_grid_high_wait|3 d interval, 21 d policy, 30 beds|Still waiting, critical channel|full -->608.0 [559.8, 656.2]<!-- /GEN --> casualties were still waiting on the critical channel at the close against <!-- GEN cell:interval_grid_high_wait|3 d interval, 21 d policy, 30 beds|Still waiting, standard channel|full -->17.5 [15.1, 19.9]<!-- /GEN --> on the standard channel. At the shipped seven-day interval the standard channel waited <!-- GEN cell:interval_grid_high_wait|7 d interval, 21 d policy, 30 beds|Mean wait, standard channel (days)|full -->3.56 [3.53, 3.58]<!-- /GEN --> days.

<!-- GEN interval_grid_high_outcomes -->
| Interval, policy and holding beds | Returns to duty | Died of wounds | Role 4 peak beds | Role 4 closing mean beds |
|---|---|---|---|---|
| 3 d interval, 21 d policy, 30 beds | 2,853.7 [2,822.5, 2,884.9] | 478.6 [399.4, 557.7] | 465.6 [463.4, 467.7] | 327.2 [325.0, 329.5] |
| 3 d interval, 21 d policy, 60 beds | 2,825.0 [2,790.8, 2,859.2] | 429.5 [351.6, 507.4] | 464.1 [461.7, 466.4] | 326.3 [323.8, 328.8] |
| 3 d interval, 30 d policy, 30 beds | 3,416.2 [3,377.9, 3,454.5] | 423.7 [363.2, 484.3] | 445.3 [442.8, 447.7] | 315.3 [313.5, 317.1] |
| 3 d interval, 30 d policy, 60 beds | 3,406.4 [3,364.4, 3,448.4] | 510.6 [424.7, 596.5] | 448.7 [445.5, 451.8] | 318.4 [316.2, 320.6] |
| 5 d interval, 21 d policy, 30 beds | 2,864.2 [2,826.3, 2,902.1] | 556.7 [476.2, 637.2] | 334.7 [332.1, 337.3] | 221.3 [219.3, 223.3] |
| 5 d interval, 21 d policy, 60 beds | 2,870.2 [2,841.2, 2,899.3] | 532.4 [478.2, 586.7] | 337.4 [334.9, 340.0] | 222.9 [221.1, 224.7] |
| 5 d interval, 30 d policy, 30 beds | 3,440.4 [3,406.2, 3,474.7] | 499.9 [454.9, 544.9] | 315.7 [313.8, 317.7] | 211.6 [209.7, 213.6] |
| 5 d interval, 30 d policy, 60 beds | 3,418.3 [3,381.4, 3,455.3] | 646.4 [536.4, 756.4] | 317.6 [315.3, 319.8] | 211.3 [209.3, 213.2] |
| 7 d interval, 21 d policy, 30 beds | 2,861.9 [2,824.1, 2,899.7] | 568.6 [500.5, 636.7] | 281.3 [279.0, 283.6] | 174.8 [172.8, 176.8] |
| 7 d interval, 21 d policy, 60 beds | 2,859.2 [2,831.3, 2,887.1] | 561.1 [494.1, 628.1] | 280.5 [277.7, 283.2] | 176.4 [174.9, 177.9] |
| 7 d interval, 30 d policy, 30 beds | 3,414.7 [3,386.4, 3,443.0] | 578.3 [519.9, 636.6] | 263.3 [260.3, 266.4] | 165.3 [163.8, 166.9] |
| 7 d interval, 30 d policy, 60 beds | 3,404.5 [3,369.5, 3,439.5] | 518.8 [474.2, 563.5] | 262.3 [260.1, 264.5] | 164.3 [162.9, 165.8] |
<!-- /GEN -->

Returns to duty per campaign were <!-- GEN cell:interval_grid_high_outcomes|7 d interval, 21 d policy, 30 beds|Returns to duty|full -->2,861.9 [2,824.1, 2,899.7]<!-- /GEN --> at the shipped cell and <!-- GEN cell:interval_grid_high_outcomes|3 d interval, 30 d policy, 60 beds|Returns to duty|full -->3,406.4 [3,364.4, 3,448.4]<!-- /GEN --> at the three-day interval with the 30-day policy and 60 beds, where the Role 4 peak was <!-- GEN cell:interval_grid_high_outcomes|3 d interval, 30 d policy, 60 beds|Role 4 peak beds|full -->448.7 [445.5, 451.8]<!-- /GEN --> beds.

<!-- GEN interval_grid_high_paired -->
| Interval, policy and holding beds | Returns to duty | Died of wounds | Role 4 peak beds |
|---|---|---|---|
| 3 d interval, 21 d policy, 30 beds | −8.2 [−50.3, +34.0] | −90.0 [−184.4, +4.3] | +184.3 [+180.8, +187.7] |
| 3 d interval, 21 d policy, 60 beds | −36.9 [−84.6, +10.7] | −139.1 [−230.5, −47.8]† | +182.8 [+179.5, +186.0] |
| 3 d interval, 30 d policy, 30 beds | +554.3 [+515.4, +593.1] | −144.9 [−246.1, −43.7]† | +164.0 [+160.3, +167.6] |
| 3 d interval, 30 d policy, 60 beds | +544.5 [+490.7, +598.3] | −58.0 [−147.4, +31.5] | +167.4 [+163.8, +170.9] |
| 5 d interval, 21 d policy, 30 beds | +2.3 [−52.8, +57.3] | −11.9 [−123.4, +99.6] | +53.4 [+50.2, +56.5] |
| 5 d interval, 21 d policy, 60 beds | +8.3 [−31.8, +48.5] | −36.2 [−121.8, +49.5] | +56.1 [+52.7, +59.6] |
| 5 d interval, 30 d policy, 30 beds | +578.5 [+519.4, +637.6] | −68.7 [−161.0, +23.6] | +34.4 [+31.1, +37.8] |
| 5 d interval, 30 d policy, 60 beds | +556.4 [+500.6, +612.3] | +77.8 [−47.1, +202.6] | +36.3 [+33.1, +39.4] |
| 7 d interval, 21 d policy, 60 beds | −2.7 [−41.1, +35.7] | −7.5 [−95.8, +80.8] | −0.8 [−4.4, +2.7] |
| 7 d interval, 30 d policy, 30 beds | +552.8 [+502.9, +602.7] | +9.7 [−80.9, +100.2] | −18.0 [−21.4, −14.5] |
| 7 d interval, 30 d policy, 60 beds | +542.6 [+486.3, +598.9] | −49.8 [−138.0, +38.4] | −19.0 [−22.6, −15.4] |
<!-- /GEN -->

Each cell was paired by replication against the shipped cell. At the three-day interval with the 30-day policy and 60 beds returns to duty differed by <!-- GEN cell:interval_grid_high_paired|3 d interval, 30 d policy, 60 beds|Returns to duty|full -->+544.5 [+490.7, +598.3]<!-- /GEN --> and died of wounds by <!-- GEN cell:interval_grid_high_paired|3 d interval, 30 d policy, 60 beds|Died of wounds|full -->−58.0 [−147.4, +31.5]<!-- /GEN -->; at the three-day interval with the 21-day policy and 30 beds returns to duty differed by <!-- GEN cell:interval_grid_high_paired|3 d interval, 21 d policy, 30 beds|Returns to duty|full -->−8.2 [−50.3, +34.0]<!-- /GEN --> and the Role 4 peak by <!-- GEN cell:interval_grid_high_paired|3 d interval, 21 d policy, 30 beds|Role 4 peak beds|full -->+184.3 [+180.8, +187.7]<!-- /GEN -->.

### Moderate-Intensity Evacuation Policy and Holding Establishment

**Question.** What happens to the R2E pools, returns to duty and mortality under the shipped configuration when the evacuation policy is set across the 21 to 30 day gap and the R2E holding establishment across 30 to 60 beds together? `[default, evacuation policy 21 to 30 days, 30 to 60 holding beds · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN policy_establishment_moderate -->
| Policy and holding beds | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Died of wounds | Never evacuated by horizon | Role 4 peak beds |
|---|---|---|---|---|---|---|
| 21 d, 30 beds | 24.4 [22.6, 26.2] | 5.7 [5.4, 6.1] | 2,062.4 [2,031.8, 2,093.0] | 21.2 [17.5, 24.8] | 36.3 [27.0, 45.6] | 209.2 [205.2, 213.2] |
| 21 d, 45 beds | 18.7 [17.2, 20.2] | 4.8 [4.4, 5.3] | 2,055.3 [2,026.0, 2,084.6] | 23.9 [20.5, 27.3] | 41.5 [27.9, 55.2] | 212.8 [208.4, 217.3] |
| 21 d, 60 beds | 13.2 [11.6, 14.7] | 3.7 [3.3, 4.2] | 2,023.4 [1,983.1, 2,063.7] | 21.4 [18.0, 24.7] | 32.2 [24.7, 39.8] | 209.1 [204.1, 214.0] |
| 24 d, 30 beds | 32.9 [30.9, 34.9] | 5.9 [5.5, 6.4] | 2,183.9 [2,158.2, 2,209.5] | 22.8 [19.1, 26.5] | 35.7 [21.5, 50.0] | 204.6 [199.6, 209.6] |
| 24 d, 45 beds | 24.5 [22.5, 26.4] | 5.0 [4.6, 5.5] | 2,177.9 [2,142.0, 2,213.9] | 21.5 [18.5, 24.5] | 45.3 [29.3, 61.3] | 201.7 [196.2, 207.2] |
| 24 d, 60 beds | 17.8 [15.9, 19.7] | 4.2 [3.7, 4.8] | 2,161.4 [2,127.3, 2,195.6] | 24.8 [19.7, 29.8] | 40.9 [26.6, 55.2] | 207.2 [202.6, 211.8] |
| 27 d, 30 beds | 36.3 [33.8, 38.8] | 5.8 [5.4, 6.2] | 2,280.8 [2,240.4, 2,321.2] | 22.8 [19.2, 26.3] | 34.8 [23.1, 46.6] | 200.4 [196.4, 204.4] |
| 27 d, 45 beds | 27.6 [25.8, 29.3] | 5.3 [4.8, 5.7] | 2,280.0 [2,250.6, 2,309.4] | 21.5 [18.0, 25.1] | 36.9 [26.9, 46.9] | 202.0 [198.2, 205.9] |
| 27 d, 60 beds | 22.0 [20.3, 23.8] | 4.5 [4.0, 4.9] | 2,279.7 [2,253.4, 2,306.0] | 18.8 [16.5, 21.1] | 34.2 [24.1, 44.3] | 199.6 [195.6, 203.7] |
| 30 d, 30 beds | 43.3 [41.5, 45.2] | 6.1 [5.7, 6.5] | 2,360.5 [2,318.5, 2,402.5] | 22.0 [18.1, 25.9] | 39.1 [29.2, 49.0] | 194.6 [190.2, 199.0] |
| 30 d, 45 beds | 31.1 [29.0, 33.2] | 5.4 [5.0, 5.8] | 2,346.7 [2,314.9, 2,378.4] | 20.9 [17.6, 24.3] | 39.9 [27.3, 52.6] | 196.1 [191.0, 201.1] |
| 30 d, 60 beds | 24.1 [21.9, 26.2] | 4.7 [4.3, 5.1] | 2,328.2 [2,297.6, 2,358.8] | 21.1 [17.6, 24.6] | 32.3 [22.6, 42.1] | 200.6 [196.5, 204.7] |
<!-- /GEN -->

At the shipped 21-day policy and 30 holding beds the closing-window R2E holding queue was <!-- GEN cell:policy_establishment_moderate|21 d, 30 beds|R2E hold mean queue|full -->24.4 [22.6, 26.2]<!-- /GEN --> casualties and the post-operative overflow census <!-- GEN cell:policy_establishment_moderate|21 d, 30 beds|R2E post-operative overflow census|full -->5.7 [5.4, 6.1]<!-- /GEN -->. At 30 days and 30 beds they were <!-- GEN cell:policy_establishment_moderate|30 d, 30 beds|R2E hold mean queue|full -->43.3 [41.5, 45.2]<!-- /GEN --> and <!-- GEN cell:policy_establishment_moderate|30 d, 30 beds|R2E post-operative overflow census|full -->6.1 [5.7, 6.5]<!-- /GEN -->, and at 30 days and 60 beds <!-- GEN cell:policy_establishment_moderate|30 d, 60 beds|R2E hold mean queue|full -->24.1 [21.9, 26.2]<!-- /GEN --> and <!-- GEN cell:policy_establishment_moderate|30 d, 60 beds|R2E post-operative overflow census|full -->4.7 [4.3, 5.1]<!-- /GEN -->. Returns to duty per campaign rose from <!-- GEN cell:policy_establishment_moderate|21 d, 30 beds|Returns to duty|full -->2,062.4 [2,031.8, 2,093.0]<!-- /GEN --> at the shipped setting to <!-- GEN cell:policy_establishment_moderate|30 d, 60 beds|Returns to duty|full -->2,328.2 [2,297.6, 2,358.8]<!-- /GEN --> at 30 days and 60 beds.

<!-- GEN policy_establishment_moderate_stability -->
| Policy and holding beds | Holding queue not growing | Post-operative overflow not growing | Both not growing | Staging overflow not growing | R2E pools across the cell |
|---|---|---|---|---|---|
| 21 d, 30 beds | 26 of 30 | 28 of 30 | 24 of 30 | 27 of 30 | all hold |
| 21 d, 45 beds | 29 of 30 | 30 of 30 | 29 of 30 | 27 of 30 | all hold |
| 21 d, 60 beds | 27 of 30 | 29 of 30 | 27 of 30 | 29 of 30 | all hold |
| 24 d, 30 beds | 26 of 30 | 27 of 30 | 25 of 30 | 27 of 30 | all hold |
| 24 d, 45 beds | 29 of 30 | 30 of 30 | 29 of 30 | 29 of 30 | all hold |
| 24 d, 60 beds | 29 of 30 | 29 of 30 | 28 of 30 | 27 of 30 | all hold |
| 27 d, 30 beds | 27 of 30 | 30 of 30 | 27 of 30 | 29 of 30 | all hold |
| 27 d, 45 beds | 29 of 30 | 30 of 30 | 29 of 30 | 26 of 30 | all hold |
| 27 d, 60 beds | 29 of 30 | 30 of 30 | 29 of 30 | 29 of 30 | all hold |
| 30 d, 30 beds | 28 of 30 | 28 of 30 | 26 of 30 | 28 of 30 | all hold |
| 30 d, 45 beds | 30 of 30 | 29 of 30 | 29 of 30 | 27 of 30 | all hold |
| 30 d, 60 beds | 30 of 30 | 28 of 30 | 28 of 30 | 28 of 30 | all hold |
<!-- /GEN -->

The table counts and classifies as the high-intensity grid's does. At 27 days the replications with neither the holding queue nor the post-operative overflow growing numbered <!-- GEN cell:policy_establishment_moderate_stability|27 d, 30 beds|Both not growing|full -->27 of 30<!-- /GEN --> at 30 beds, <!-- GEN cell:policy_establishment_moderate_stability|27 d, 45 beds|Both not growing|full -->29 of 30<!-- /GEN --> at 45 and <!-- GEN cell:policy_establishment_moderate_stability|27 d, 60 beds|Both not growing|full -->29 of 30<!-- /GEN --> at 60; at 30 days they numbered <!-- GEN cell:policy_establishment_moderate_stability|30 d, 30 beds|Both not growing|full -->26 of 30<!-- /GEN -->, <!-- GEN cell:policy_establishment_moderate_stability|30 d, 45 beds|Both not growing|full -->29 of 30<!-- /GEN --> and <!-- GEN cell:policy_establishment_moderate_stability|30 d, 60 beds|Both not growing|full -->28 of 30<!-- /GEN -->. The 30-day, 30-bed cell was classified <!-- GEN cell:policy_establishment_moderate_stability|30 d, 30 beds|R2E pools across the cell|full -->all hold<!-- /GEN -->.

<!-- GEN policy_establishment_moderate_paired -->
| Policy and holding beds | Returns to duty | Died of wounds | Role 4 peak beds |
|---|---|---|---|
| 21 d, 45 beds | −7.1 [−42.1, +27.9] | +2.7 [−2.2, +7.7] | +3.6 [−2.4, +9.6] |
| 21 d, 60 beds | −39.0 [−87.2, +9.2] | +0.2 [−5.3, +5.7] | −0.2 [−7.1, +6.7] |
| 24 d, 30 beds | +121.5 [+92.9, +150.1] | +1.6 [−2.2, +5.5] | −4.6 [−10.8, +1.5] |
| 24 d, 45 beds | +115.5 [+63.3, +167.8] | +0.4 [−4.7, +5.5] | −7.5 [−14.3, −0.7]† |
| 24 d, 60 beds | +99.0 [+53.6, +144.5] | +3.6 [−2.8, +10.0] | −2.0 [−9.1, +5.0] |
| 27 d, 30 beds | +218.4 [+170.2, +266.6] | +1.6 [−3.4, +6.6] | −8.8 [−13.4, −4.2] |
| 27 d, 45 beds | +217.6 [+172.1, +263.1] | +0.4 [−4.3, +5.1] | −7.2 [−12.8, −1.6]† |
| 27 d, 60 beds | +217.3 [+176.9, +257.6] | −2.4 [−6.7, +2.0] | −9.6 [−15.5, −3.7]† |
| 30 d, 30 beds | +298.1 [+248.7, +347.5] | +0.9 [−4.3, +6.0] | −14.6 [−19.6, −9.6] |
| 30 d, 45 beds | +284.3 [+249.1, +319.5] | −0.2 [−4.8, +4.3] | −13.2 [−19.3, −7.0] |
| 30 d, 60 beds | +265.8 [+226.3, +305.3] | −0.1 [−5.5, +5.4] | −8.7 [−14.7, −2.6]† |
<!-- /GEN -->

Each cell was paired by replication against the shipped 21-day policy at 30 holding beds. At 27 days and 45 beds returns to duty differed by <!-- GEN cell:policy_establishment_moderate_paired|27 d, 45 beds|Returns to duty|full -->+217.6 [+172.1, +263.1]<!-- /GEN --> and Role 4 peak beds by <!-- GEN cell:policy_establishment_moderate_paired|27 d, 45 beds|Role 4 peak beds|full -->−7.2 [−12.8, −1.6]†<!-- /GEN -->; at 30 days and 60 beds the differences were <!-- GEN cell:policy_establishment_moderate_paired|30 d, 60 beds|Returns to duty|full -->+265.8 [+226.3, +305.3]<!-- /GEN --> and <!-- GEN cell:policy_establishment_moderate_paired|30 d, 60 beds|Role 4 peak beds|full -->−8.7 [−14.7, −2.6]†<!-- /GEN -->.

<!-- GEN policy_grid_largest_moderate -->
| Holding beds | Largest policy holding the R2E pools | Returns to duty there | Paired gain over 21 d and 30 beds | Gain of the added beds at that policy |
|---|---|---|---|---|
| 30 | 30 d | 2,360.5 [2,318.5, 2,402.5] | +298.1 [+248.7, +347.5] | not applicable |
| 45 | 30 d | 2,346.7 [2,314.9, 2,378.4] | +284.3 [+249.1, +319.5] | −13.8 [−66.7, +39.1] |
| 60 | 30 d | 2,328.2 [2,297.6, 2,358.8] | +265.8 [+226.3, +305.3] | −32.3 [−90.5, +25.9] |
<!-- /GEN -->

The largest policy at which the cell held the R2E pools was <!-- GEN cell:policy_grid_largest_moderate|30|Largest policy holding the R2E pools|full -->30 d<!-- /GEN --> at 30 beds, <!-- GEN cell:policy_grid_largest_moderate|45|Largest policy holding the R2E pools|full -->30 d<!-- /GEN --> at 45 and <!-- GEN cell:policy_grid_largest_moderate|60|Largest policy holding the R2E pools|full -->30 d<!-- /GEN --> at 60. At that 30-day policy, the beds added beyond 30 changed returns to duty by <!-- GEN cell:policy_grid_largest_moderate|45|Gain of the added beds at that policy|full -->−13.8 [−66.7, +39.1]<!-- /GEN --> at 45 beds and <!-- GEN cell:policy_grid_largest_moderate|60|Gain of the added beds at that policy|full -->−32.3 [−90.5, +25.9]<!-- /GEN --> at 60, each paired by replication against the same policy at 30 beds.

### Forward Surgical Saturation Release

**Question.** What changes when casualties are released to strategic evacuation with the definitive repair outstanding once the R2E theatre queue reaches a threshold? `[default, threshold 0 to 24 casualties · 360 d · 30 replications · paired, closing 90 d]`

<!-- GEN saturation -->
| Response | 0 (disabled) | 1 | 2 (shipped) | 3 | 5 | 8 | 12 | 16 | 24 |
|---|---|---|---|---|---|---|---|---|---|
| Theatre mean queue | 1.58 [0.97, 2.19] | 0.81 [0.34, 1.27] | 0.97 [0.63, 1.31] | 0.65 [0.41, 0.89] | 0.77 [0.53, 1.01] | 0.77 [0.55, 1.00] | 1.05 [0.64, 1.45] | 1.04 [0.80, 1.28] | 0.82 [0.56, 1.09] |
| Released with repair outstanding | 0.0 [0.0, 0.0] | 168.9 [152.2, 185.6] | 131.0 [115.9, 146.0] | 108.9 [94.2, 123.6] | 93.6 [77.1, 110.2] | 66.0 [53.5, 78.5] | 56.4 [45.8, 67.0] | 43.2 [30.9, 55.5] | 14.5 [6.6, 22.4] |
| Role 4 operations owed | 1077.2 [1040.3, 1114.2] | 1270.0 [1217.9, 1322.0] | 1216.3 [1176.7, 1255.8] | 1174.2 [1134.9, 1213.5] | 1178.3 [1141.8, 1214.7] | 1143.3 [1102.7, 1184.0] | 1139.8 [1107.6, 1172.0] | 1135.5 [1092.0, 1179.1] | 1070.8 [1022.9, 1118.7] |
| Post-definitive ICU access (%) | 34.7 [33.9, 35.4] | 35.3 [34.5, 36.0] | 35.2 [34.6, 35.9] | 35.6 [35.1, 36.2] | 35.0 [34.4, 35.7] | 34.9 [34.1, 35.6] | 34.4 [33.7, 35.1] | 34.2 [33.5, 35.0] | 34.9 [34.0, 35.8] |
| Died of wounds | 23.5 [16.7, 30.3] | 20.0 [17.2, 22.9] | 21.2 [17.5, 24.8] | 18.8 [16.1, 21.5] | 24.2 [20.8, 27.5] | 21.6 [18.8, 24.4] | 22.3 [19.2, 25.4] | 23.0 [19.2, 26.8] | 18.9 [16.0, 21.9] |
| Returns to duty | 2048.7 [2016.1, 2081.4] | 2038.7 [2011.6, 2065.8] | 2062.4 [2031.8, 2093.0] | 2050.9 [2017.5, 2084.3] | 2039.0 [2002.5, 2075.5] | 2055.8 [2027.6, 2084.0] | 2048.9 [2018.7, 2079.2] | 2055.2 [2026.5, 2084.0] | 2048.4 [2003.9, 2093.0] |
<!-- /GEN -->

![Six panels of campaign responses against the forward surgical saturation release threshold](../images/saturation_sweep.png)

The same six responses against the R2E theatre queue length at which a casualty is released, with the shipped threshold marked by a dashed line.

With the release disabled the closing-window theatre queue was <!-- GEN cell:saturation|Theatre mean queue|0 (disabled)|full -->1.58 [0.97, 2.19]<!-- /GEN --> casualties and no casualty was released with the repair outstanding. At the shipped threshold of two the queue was <!-- GEN cell:saturation|Theatre mean queue|2 (shipped)|full -->0.97 [0.63, 1.31]<!-- /GEN --> and <!-- GEN cell:saturation|Released with repair outstanding|2 (shipped)|full -->131.0 [115.9, 146.0]<!-- /GEN --> casualties per campaign were released with the repair outstanding, with <!-- GEN cell:saturation|Role 4 operations owed|2 (shipped)|full -->1216.3 [1176.7, 1255.8]<!-- /GEN --> operations owed at the national support base against <!-- GEN cell:saturation|Role 4 operations owed|0 (disabled)|full -->1077.2 [1040.3, 1114.2]<!-- /GEN --> with the release disabled. Returns to duty and died of wounds per campaign were <!-- GEN cell:saturation|Returns to duty|2 (shipped)|full -->2062.4 [2031.8, 2093.0]<!-- /GEN --> and <!-- GEN cell:saturation|Died of wounds|2 (shipped)|full -->21.2 [17.5, 24.8]<!-- /GEN --> at the shipped threshold and <!-- GEN cell:saturation|Returns to duty|0 (disabled)|full -->2048.7 [2016.1, 2081.4]<!-- /GEN --> and <!-- GEN cell:saturation|Died of wounds|0 (disabled)|full -->23.5 [16.7, 30.3]<!-- /GEN --> with the release disabled.

**High-intensity profile.** The same sweep under `high_intensity` `[high_intensity, threshold 0 to 24 casualties · 360 d · 30 replications · paired, closing 90 d]`:

<!-- GEN saturation_high -->
| Response | 0 (disabled) | 1 | 2 (shipped) | 3 | 5 | 8 | 12 | 16 | 24 |
|---|---|---|---|---|---|---|---|---|---|
| Theatre mean queue | 902.55 [845.56, 959.53] | 105.83 [73.18, 138.48] | 72.62 [51.40, 93.83] | 70.55 [55.33, 85.77] | 82.10 [56.63, 107.57] | 96.27 [66.05, 126.50] | 103.66 [70.72, 136.60] | 72.00 [53.18, 90.81] | 75.02 [56.98, 93.06] |
| Released with repair outstanding | 0.0 [0.0, 0.0] | 2026.2 [1991.4, 2061.0] | 1985.5 [1933.0, 2038.0] | 1971.5 [1922.0, 2021.0] | 1948.0 [1897.3, 1998.7] | 1909.0 [1860.4, 1957.6] | 1905.3 [1853.9, 1956.8] | 1815.1 [1747.3, 1882.9] | 1729.0 [1652.2, 1805.8] |
| Role 4 operations owed | 1308.8 [1282.8, 1334.8] | 2075.1 [2040.1, 2110.1] | 2049.9 [2020.6, 2079.2] | 2042.7 [2014.4, 2070.9] | 2050.8 [2016.9, 2084.6] | 2031.8 [2003.2, 2060.4] | 1994.9 [1958.7, 2031.0] | 1952.9 [1914.9, 1990.9] | 1961.5 [1915.9, 2007.0] |
| Post-definitive ICU access (%) | 15.9 [15.7, 16.1] | 11.0 [10.8, 11.2] | 11.3 [11.1, 11.5] | 11.2 [11.0, 11.4] | 11.3 [11.0, 11.5] | 11.1 [10.9, 11.3] | 11.2 [11.0, 11.4] | 11.3 [11.1, 11.5] | 11.4 [11.1, 11.6] |
| Died of wounds | 1771.3 [1687.6, 1855.0] | 652.4 [571.0, 733.7] | 568.6 [500.5, 636.7] | 529.5 [480.1, 578.9] | 581.8 [527.9, 635.7] | 694.4 [583.4, 805.5] | 643.3 [561.3, 725.2] | 578.9 [519.3, 638.6] | 618.7 [561.6, 675.9] |
| Returns to duty | 2790.3 [2762.6, 2818.0] | 2840.5 [2804.5, 2876.5] | 2861.9 [2824.1, 2899.7] | 2840.3 [2808.9, 2871.7] | 2849.1 [2817.1, 2881.1] | 2863.9 [2830.3, 2897.4] | 2840.7 [2805.1, 2876.3] | 2859.7 [2824.9, 2894.6] | 2852.5 [2818.1, 2887.0] |
<!-- /GEN -->

The R2E theatre mean queue was <!-- GEN cell:saturation_high|Theatre mean queue|0 (disabled)|full -->902.55 [845.56, 959.53]<!-- /GEN --> with the release disabled and <!-- GEN cell:saturation_high|Theatre mean queue|2 (shipped)|full -->72.62 [51.40, 93.83]<!-- /GEN --> at the shipped threshold of two.


### Surgical Team Establishment

**Question.** What changes when the surgical sections fielded at R2E, at R2B or at both are increased, and does the R2E theatre queue converge at the larger establishment? `[default, R2E 3 to 6 and R2B 1 to 2 sections · 360 d · 30 replications · paired, closing 90 d]`

Each arm is a count of surgical sections: three, four or six at R2E, and one or two per R2B unit. R2E fields two operating theatres in every arm. A second R2B section takes the shift the first does not, and an R2E fourth section makes two sections on each shift. The shipped establishment is three sections at R2E and one per R2B unit.

<!-- GEN surgical_team -->
| Response | R2E 3, R2B 1 (shipped) | R2E 4, R2B 1 | R2E 6, R2B 1 | R2E 3, R2B 2 | R2E 4, R2B 2 |
|---|---|---|---|---|---|
| R2E theatre mean queue | 0.97 [0.63, 1.31] | 0.59 [0.15, 1.02] | 0.31 [0.23, 0.38] | 0.59 [0.32, 0.86] | 0.26 [0.09, 0.43] |
| R2E theatre queue, late-half classification | converged at 0.90 | converged at 0.40 | converged at 0.27 | converged at 0.56 | converged at 0.18 |
| R2E theatre queue shortfall per day | 0.000 [−0.004, 0.004] | 0.003 [−0.002, 0.007] | 0.000 [−0.001, 0.001] | 0.000 [−0.006, 0.005] | 0.001 [−0.001, 0.003] |
| Diverted, team off shift | 1201.5 [1178.8, 1224.3] | 1202.2 [1162.7, 1241.6] | 1184.2 [1162.1, 1206.4] | 0.0 [0.0, 0.0] | 0.0 [0.0, 0.0] |
| Diverted, theatre busy | 236.8 [220.9, 252.8] | 243.1 [221.1, 265.1] | 233.6 [222.7, 244.5] | 510.3 [486.3, 534.4] | 502.9 [481.3, 524.5] |
| R2B surgeries | 824.8 [813.4, 836.2] | 823.2 [807.6, 838.8] | 815.8 [806.1, 825.5] | 1574.5 [1558.6, 1590.4] | 1580.1 [1562.0, 1598.1] |
| R2E first surgeries | 1816.0 [1774.7, 1857.3] | 1821.2 [1750.7, 1891.8] | 1792.4 [1758.1, 1826.7] | 1390.5 [1346.2, 1434.8] | 1379.4 [1339.8, 1418.9] |
| R2E theatre entry deferred | 375.1 [361.6, 388.7] | 375.5 [353.1, 397.9] | 367.4 [355.7, 379.1] | 232.0 [219.5, 244.5] | 230.7 [218.9, 242.6] |
| Released with repair outstanding | 131.0 [115.9, 146.0] | 97.0 [71.1, 122.9] | 78.3 [66.9, 89.8] | 74.1 [61.3, 86.8] | 45.3 [34.5, 56.1] |
| Died of wounds | 21.17 [17.55, 24.79] | 25.80 [15.83, 35.77] | 17.27 [14.71, 19.82] | 19.77 [16.96, 22.58] | 16.20 [12.74, 19.66] |
| Returns to duty | 2062.4 [2031.8, 2093.0] | 2035.4 [2002.5, 2068.3] | 2046.5 [2020.3, 2072.7] | 2034.9 [2004.3, 2065.5] | 2041.2 [2012.3, 2070.1] |
| Post-definitive ICU access (%) | 35.2 [34.6, 35.9] | 35.3 [34.3, 36.2] | 35.7 [35.1, 36.3] | 33.9 [33.2, 34.6] | 33.8 [33.1, 34.5] |
<!-- /GEN -->

The paired differences against the shipped establishment are in the next table, each taken within replication.

<!-- GEN surgical_team_paired -->
| Paired difference against the shipped establishment | R2E 4, R2B 1 | R2E 6, R2B 1 | R2E 3, R2B 2 | R2E 4, R2B 2 |
|---|---|---|---|---|
| R2E theatre mean queue | −0.38 [−0.97, +0.21] | −0.66 [−1.03, −0.29] | −0.38 [−0.83, +0.07] | −0.71 [−1.04, −0.37] |
| Diverted, team off shift | +0.6 [−45.0, +46.3] | −17.3 [−47.3, +12.7] | −1201.5 [−1224.3, −1178.8] | −1201.5 [−1224.3, −1178.8] |
| Diverted, theatre busy | +6.3 [−21.6, +34.1] | −3.2 [−24.3, +17.8] | +273.5 [+251.5, +295.5] | +266.1 [+236.8, +295.3] |
| R2B surgeries | −1.7 [−19.7, +16.4] | −9.0 [−24.8, +6.7] | +749.7 [+730.9, +768.4] | +755.2 [+732.2, +778.3] |
| R2E first surgeries | +5.3 [−76.9, +87.5] | −23.6 [−77.5, +30.4] | −425.5 [−467.2, −383.7] | −436.6 [−497.0, −376.2] |
| R2E theatre entry deferred | +0.4 [−25.5, +26.2] | −7.7 [−27.4, +11.9] | −143.1 [−155.9, −130.3] | −144.4 [−164.3, −124.5] |
| Released with repair outstanding | −33.9 [−64.3, −3.5]† | −52.6 [−72.2, −33.0] | −56.9 [−70.8, −43.0] | −85.7 [−104.2, −67.2] |
| Died of wounds | +4.63 [−6.22, +15.48] | −3.90 [−8.71, +0.91] | −1.40 [−5.46, +2.66] | −4.97 [−10.28, +0.34] |
| Returns to duty | −27.0 [−70.9, +16.9] | −15.9 [−57.4, +25.6] | −27.5 [−72.8, +17.8] | −21.2 [−64.0, +21.6] |
| Post-definitive ICU access (%) | +0.0 [−1.2, +1.2] | +0.5 [−0.5, +1.4] | −1.3 [−2.0, −0.7] | −1.4 [−2.4, −0.4]† |
<!-- /GEN -->

At the shipped establishment the R2E theatre queue averaged <!-- GEN cell:surgical_team|R2E theatre mean queue|R2E 3, R2B 1 (shipped)|full -->0.97 [0.63, 1.31]<!-- /GEN --> over the closing 90 days and was classified <!-- GEN cell:surgical_team|R2E theatre queue, late-half classification|R2E 3, R2B 1 (shipped)|full -->converged at 0.90<!-- /GEN -->. A fourth R2E section changed it by <!-- GEN cell:surgical_team_paired|R2E theatre mean queue|R2E 4, R2B 1|full -->−0.38 [−0.97, +0.21]<!-- /GEN --> and a sixth by <!-- GEN cell:surgical_team_paired|R2E theatre mean queue|R2E 6, R2B 1|full -->−0.66 [−1.03, −0.29]<!-- /GEN -->, with the queue classified <!-- GEN cell:surgical_team|R2E theatre queue, late-half classification|R2E 4, R2B 1|full -->converged at 0.40<!-- /GEN --> and <!-- GEN cell:surgical_team|R2E theatre queue, late-half classification|R2E 6, R2B 1|full -->converged at 0.27<!-- /GEN -->. The paired differences in deaths of wounds for the fourth and sixth R2E sections were <!-- GEN cell:surgical_team_paired|Died of wounds|R2E 4, R2B 1|full -->+4.63 [−6.22, +15.48]<!-- /GEN --> and <!-- GEN cell:surgical_team_paired|Died of wounds|R2E 6, R2B 1|full -->−3.90 [−8.71, +0.91]<!-- /GEN -->, and in returns to duty <!-- GEN cell:surgical_team_paired|Returns to duty|R2E 4, R2B 1|full -->−27.0 [−70.9, +16.9]<!-- /GEN --> and <!-- GEN cell:surgical_team_paired|Returns to duty|R2E 6, R2B 1|full -->−15.9 [−57.4, +25.6]<!-- /GEN -->.

A second R2B section removed the cases diverted for a closed section, which fell from <!-- GEN cell:surgical_team|Diverted, team off shift|R2E 3, R2B 1 (shipped)|full -->1201.5 [1178.8, 1224.3]<!-- /GEN --> to <!-- GEN cell:surgical_team|Diverted, team off shift|R2E 3, R2B 2|full -->0.0 [0.0, 0.0]<!-- /GEN -->, and raised those diverted for a busy theatre by <!-- GEN cell:surgical_team_paired|Diverted, theatre busy|R2E 3, R2B 2|full -->+273.5 [+251.5, +295.5]<!-- /GEN -->. R2B surgeries rose by <!-- GEN cell:surgical_team_paired|R2B surgeries|R2E 3, R2B 2|full -->+749.7 [+730.9, +768.4]<!-- /GEN -->, R2E first surgeries fell by <!-- GEN cell:surgical_team_paired|R2E first surgeries|R2E 3, R2B 2|full -->−425.5 [−467.2, −383.7]<!-- /GEN --> and the casualties released with the repair outstanding fell by <!-- GEN cell:surgical_team_paired|Released with repair outstanding|R2E 3, R2B 2|full -->−56.9 [−70.8, −43.0]<!-- /GEN -->. Deaths of wounds differed by <!-- GEN cell:surgical_team_paired|Died of wounds|R2E 3, R2B 2|full -->−1.40 [−5.46, +2.66]<!-- /GEN --> per campaign, and returns to duty by <!-- GEN cell:surgical_team_paired|Returns to duty|R2E 3, R2B 2|full -->−27.5 [−72.8, +17.8]<!-- /GEN -->. With both levers together the differences in deaths of wounds and in returns to duty were <!-- GEN cell:surgical_team_paired|Died of wounds|R2E 4, R2B 2|full -->−4.97 [−10.28, +0.34]<!-- /GEN --> and <!-- GEN cell:surgical_team_paired|Returns to duty|R2E 4, R2B 2|full -->−21.2 [−64.0, +21.6]<!-- /GEN -->.


At high intensity the same five arms were run under the `high_intensity` profile (30 replications, 360 days, seed 42).

<!-- GEN surgical_team_high -->
| Response | R2E 3, R2B 1 (shipped) | R2E 4, R2B 1 | R2E 6, R2B 1 | R2E 3, R2B 2 | R2E 4, R2B 2 |
|---|---|---|---|---|---|
| R2E theatre mean queue | 72.62 [51.40, 93.83] | 9.82 [7.70, 11.95] | 8.68 [6.88, 10.48] | 21.31 [14.29, 28.33] | 4.01 [3.51, 4.51] |
| R2E theatre queue, late-half classification | converged at 71.0 | converged at 9.37 | converged at 8.66 | converged at 20.6 | converged at 4.45 |
| R2E theatre queue shortfall per day | 0.014 [−0.151, 0.179] | 0.003 [−0.016, 0.022] | 0.006 [−0.009, 0.022] | 0.017 [−0.053, 0.088] | −0.007 [−0.017, 0.003] |
| Diverted, team off shift | 3825.8 [3770.0, 3881.7] | 3806.2 [3754.7, 3857.7] | 3830.0 [3784.3, 3875.7] | 0.0 [0.0, 0.0] | 0.0 [0.0, 0.0] |
| Diverted, theatre busy | 1375.3 [1337.9, 1412.8] | 1358.9 [1322.1, 1395.6] | 1357.0 [1319.7, 1394.4] | 2551.9 [2502.0, 2601.8] | 2511.5 [2458.6, 2564.4] |
| R2B surgeries | 1795.4 [1776.0, 1814.8] | 1787.3 [1767.0, 1807.6] | 1792.6 [1775.7, 1809.5] | 3270.7 [3243.9, 3297.5] | 3256.9 [3234.2, 3279.6] |
| R2E first surgeries | 6159.3 [6060.2, 6258.4] | 6181.5 [6086.0, 6277.1] | 6197.7 [6113.1, 6282.3] | 5371.8 [5289.9, 5453.6] | 5337.0 [5238.3, 5435.6] |
| R2E theatre entry deferred | 1587.8 [1554.3, 1621.2] | 1586.4 [1560.0, 1612.9] | 1579.8 [1554.0, 1605.6] | 1247.9 [1217.3, 1278.5] | 1235.7 [1208.4, 1262.9] |
| Released with repair outstanding | 1985.5 [1933.0, 2038.0] | 1529.7 [1468.9, 1590.4] | 1544.0 [1491.4, 1596.5] | 1346.7 [1302.6, 1390.7] | 964.6 [914.1, 1015.1] |
| Died of wounds | 568.60 [500.55, 636.65] | 308.70 [294.86, 322.54] | 307.60 [294.88, 320.32] | 346.30 [327.68, 364.92] | 272.57 [262.27, 282.86] |
| Returns to duty | 2861.9 [2824.1, 2899.7] | 2882.0 [2845.7, 2918.3] | 2899.2 [2859.7, 2938.7] | 2895.8 [2860.2, 2931.4] | 2882.0 [2841.8, 2922.2] |
| Post-definitive ICU access (%) | 11.3 [11.1, 11.5] | 11.5 [11.3, 11.7] | 11.4 [11.2, 11.7] | 10.8 [10.6, 11.0] | 11.0 [10.8, 11.3] |
<!-- /GEN -->

<!-- GEN surgical_team_paired_high -->
| Paired difference against the shipped establishment | R2E 4, R2B 1 | R2E 6, R2B 1 | R2E 3, R2B 2 | R2E 4, R2B 2 |
|---|---|---|---|---|
| R2E theatre mean queue | −62.79 [−83.73, −41.86] | −63.94 [−84.95, −42.93] | −51.31 [−72.80, −29.82] | −68.61 [−89.90, −47.31] |
| Diverted, team off shift | −19.7 [−97.6, +58.3] | +4.1 [−67.5, +75.8] | −3825.8 [−3881.7, −3770.0] | −3825.8 [−3881.7, −3770.0] |
| Diverted, theatre busy | −16.5 [−67.6, +34.6] | −18.3 [−73.0, +36.4] | +1176.6 [+1109.4, +1243.8] | +1136.2 [+1068.0, +1204.3] |
| R2B surgeries | −8.1 [−36.4, +20.2] | −2.8 [−28.0, +22.4] | +1475.3 [+1441.1, +1509.5] | +1461.5 [+1432.9, +1490.1] |
| R2E first surgeries | +22.2 [−113.9, +158.3] | +38.4 [−94.0, +170.8] | −787.5 [−915.9, −659.2] | −822.3 [−959.9, −684.8] |
| R2E theatre entry deferred | −1.3 [−44.6, +42.0] | −8.0 [−52.5, +36.6] | −339.9 [−390.3, −289.4] | −352.1 [−396.2, −308.0] |
| Released with repair outstanding | −455.8 [−530.5, −381.1] | −441.5 [−517.5, −365.6] | −638.8 [−707.1, −570.6] | −1020.9 [−1094.9, −946.8] |
| Died of wounds | −259.90 [−330.91, −188.89] | −261.00 [−330.32, −191.68] | −222.30 [−290.65, −153.95] | −296.03 [−365.00, −227.06] |
| Returns to duty | +20.1 [−31.9, +72.2] | +37.3 [−21.8, +96.5] | +33.9 [−21.7, +89.5] | +20.1 [−39.4, +79.6] |
| Post-definitive ICU access (%) | +0.2 [−0.1, +0.6] | +0.2 [−0.2, +0.5] | −0.5 [−0.8, −0.2]† | −0.2 [−0.5, +0.1] |
<!-- /GEN -->

At the shipped establishment the high-intensity R2E theatre queue averaged <!-- GEN cell:surgical_team_high|R2E theatre mean queue|R2E 3, R2B 1 (shipped)|full -->72.62 [51.40, 93.83]<!-- /GEN --> over the closing 90 days and was classified <!-- GEN cell:surgical_team_high|R2E theatre queue, late-half classification|R2E 3, R2B 1 (shipped)|full -->converged at 71.0<!-- /GEN -->, a shortfall of <!-- GEN cell:surgical_team_high|R2E theatre queue shortfall per day|R2E 3, R2B 1 (shipped)|full -->0.014 [−0.151, 0.179]<!-- /GEN --> casualties per day. A fourth R2E section gave <!-- GEN cell:surgical_team_high|R2E theatre mean queue|R2E 4, R2B 1|full -->9.82 [7.70, 11.95]<!-- /GEN --> and <!-- GEN cell:surgical_team_high|R2E theatre queue, late-half classification|R2E 4, R2B 1|full -->converged at 9.37<!-- /GEN -->, with a shortfall of <!-- GEN cell:surgical_team_high|R2E theatre queue shortfall per day|R2E 4, R2B 1|full -->0.003 [−0.016, 0.022]<!-- /GEN --> per day; a sixth gave <!-- GEN cell:surgical_team_high|R2E theatre queue, late-half classification|R2E 6, R2B 1|full -->converged at 8.66<!-- /GEN --> and <!-- GEN cell:surgical_team_high|R2E theatre queue shortfall per day|R2E 6, R2B 1|full -->0.006 [−0.009, 0.022]<!-- /GEN --> per day. A second R2B section alone gave <!-- GEN cell:surgical_team_high|R2E theatre queue, late-half classification|R2E 3, R2B 2|full -->converged at 20.6<!-- /GEN --> and <!-- GEN cell:surgical_team_high|R2E theatre queue shortfall per day|R2E 3, R2B 2|full -->0.017 [−0.053, 0.088]<!-- /GEN --> per day, and both levers together <!-- GEN cell:surgical_team_high|R2E theatre queue, late-half classification|R2E 4, R2B 2|full -->converged at 4.45<!-- /GEN --> and <!-- GEN cell:surgical_team_high|R2E theatre queue shortfall per day|R2E 4, R2B 2|full -->−0.007 [−0.017, 0.003]<!-- /GEN --> per day. The paired difference in deaths of wounds was <!-- GEN cell:surgical_team_paired_high|Died of wounds|R2E 4, R2B 1|full -->−259.90 [−330.91, −188.89]<!-- /GEN --> for the fourth R2E section and <!-- GEN cell:surgical_team_paired_high|Died of wounds|R2E 4, R2B 2|full -->−296.03 [−365.00, −227.06]<!-- /GEN --> for both levers, and in returns to duty <!-- GEN cell:surgical_team_paired_high|Returns to duty|R2E 4, R2B 1|full -->+20.1 [−31.9, +72.2]<!-- /GEN --> and <!-- GEN cell:surgical_team_paired_high|Returns to duty|R2E 4, R2B 2|full -->+20.1 [−39.4, +79.6]<!-- /GEN -->.

![Two columns of panels of campaign responses against the surgical section establishment at moderate and high intensity](../images/surgical_team_sweep.png)

The R2E theatre queue and the diversions, deaths of wounds and returns to duty of each establishment, at moderate intensity (left) and high intensity (right), with the shipped establishment marked.


### R2E Operating Theatre Establishment

**Question.** Does the number of R2E operating theatres or the number of surgical sections limit R2E surgery, and what does an added theatre buy when more sections share a shift than there are theatres? `[default and high_intensity, R2E 2 to 4 theatres crossed with 3 to 8 sections · 360 d · 30 replications · paired, closing 90 d]`

Each arm is an R2E theatre count crossed with a surgical section count, and the first row of each table states the sections on the larger shift against the theatres, the ratio the arms were chosen on ([Methods](Methods.md#r2e-operating-theatre-establishment-sweep)). Theatres are open around the clock, so theatre occupancy is the share of all theatre time in use. Every arm is compared with the shipped establishment, and three theatres at six sections with two theatres at six sections; Holm's adjustment is taken across all of those comparisons.

<!-- GEN ot_bed -->
| Response | 2 theatres, 3 sections (shipped) | 3 theatres, 3 sections | 2 theatres, 6 sections | 3 theatres, 6 sections | 4 theatres, 8 sections |
|---|---|---|---|---|---|
| Sections on the larger shift against theatres | 2 against 2 | 2 against 3 | 3 against 2 | 3 against 3 | 4 against 4 |
| R2E theatre mean queue | 0.97 [0.63, 1.31] | 0.54 [0.34, 0.74] | 0.31 [0.23, 0.38] | 0.08 [0.05, 0.10] | 0.03 [0.02, 0.04] |
| R2E theatre queue, late-half classification | converged at 0.90 | converged at 0.65 | converged at 0.27 | converged at 0.07 | converged at 0.03 |
| R2E theatre queue shortfall per day | 0.000 [−0.004, 0.004] | −0.002 [−0.005, 0.002] | 0.000 [−0.001, 0.001] | 0.000 [0.000, 0.000] | 0.000 [0.000, 0.000] |
| R2E theatre occupancy (%) | 28.1 [26.8, 29.5] | 17.7 [17.0, 18.4] | 28.1 [26.9, 29.4] | 18.9 [17.8, 20.0] | 15.1 [14.2, 16.0] |
| R2E first surgeries | 1816.0 [1774.7, 1857.3] | 1809.5 [1755.6, 1863.4] | 1792.4 [1758.1, 1826.7] | 1851.1 [1800.7, 1901.5] | 1816.5 [1763.9, 1869.2] |
| R2E theatre entry deferred | 375.1 [361.6, 388.7] | 369.4 [352.6, 386.2] | 367.4 [355.7, 379.1] | 381.9 [366.5, 397.3] | 372.6 [355.6, 389.5] |
| Released with definitive repair outstanding | 131.0 [115.9, 146.0] | 124.1 [107.7, 140.4] | 78.3 [66.9, 89.8] | 39.8 [32.6, 47.1] | 17.0 [12.6, 21.4] |
| Diverted from R2B, theatre busy | 236.8 [220.9, 252.8] | 237.2 [220.6, 253.7] | 233.6 [222.7, 244.5] | 251.4 [234.5, 268.3] | 239.7 [224.7, 254.7] |
| R2E ICU occupancy (%) | 86.8 [86.0, 87.6] | 86.3 [85.4, 87.2] | 86.9 [86.1, 87.6] | 86.5 [85.6, 87.4] | 86.3 [85.5, 87.1] |
| R2E post-operative overflow census | 5.72 [5.36, 6.09] | 5.32 [5.00, 5.63] | 5.63 [5.29, 5.97] | 5.82 [5.35, 6.29] | 6.13 [5.58, 6.68] |
| R2E post-operative overflow, late-half classification | converged at 5.54 | **drifting, -2.0%/block**, 4.3 to 5.5 | converged at 5.60 | converged at 5.78 | converged at 5.89 |
| R2E holding mean queue | 24.38 [22.59, 26.16] | 23.63 [21.98, 25.29] | 23.95 [23.04, 24.87] | 24.07 [22.50, 25.65] | 24.53 [22.58, 26.48] |
| R2E holding queue, late-half classification | converged at 23.8 | converged at 24.0 | converged at 23.9 | converged at 24.3 | converged at 24.2 |
| R2E staging overflow, late-half classification | converged at 30.1 | converged at 31.8 | converged at 27.9 | converged at 31.7 | converged at 30.1 |
| Post-definitive ICU access (%) | 35.2 [34.6, 35.9] | 35.6 [34.7, 36.4] | 35.7 [35.1, 36.3] | 34.1 [33.3, 34.8] | 34.4 [33.5, 35.2] |
| Died of wounds | 21.17 [17.55, 24.79] | 22.23 [18.07, 26.40] | 17.27 [14.71, 19.82] | 18.07 [16.00, 20.13] | 17.37 [15.49, 19.24] |
| Returns to duty | 2062.4 [2031.8, 2093.0] | 2064.0 [2037.0, 2091.1] | 2046.5 [2020.3, 2072.7] | 2066.8 [2033.0, 2100.6] | 2044.1 [2010.1, 2078.1] |
<!-- /GEN -->

The paired differences, each taken within replication, are in the next table.

<!-- GEN ot_bed_paired -->
| Paired difference | 3 theatres, 3 sections | 2 theatres, 6 sections | 3 theatres, 6 sections | 4 theatres, 8 sections | 3 theatres, 6 sections against 2 theatres, 6 sections |
|---|---|---|---|---|---|
| R2E theatre mean queue | −0.43 [−0.79, −0.06]† | −0.66 [−1.03, −0.29] | −0.89 [−1.24, −0.54] | −0.94 [−1.28, −0.59] | −0.23 [−0.31, −0.15] |
| R2E theatre occupancy (%) | −10.4 [−11.9, −8.9] | +0.0 [−1.9, +2.0] | −9.2 [−11.2, −7.2] | −13.0 [−14.7, −11.4] | −9.2 [−10.9, −7.5] |
| R2E first surgeries | −6.4 [−75.9, +63.0] | −23.6 [−77.5, +30.4] | +35.1 [−34.4, +104.6] | +0.6 [−72.7, +73.8] | +58.7 [−8.6, +126.0] |
| R2E theatre entry deferred | −5.7 [−27.9, +16.4] | −7.7 [−27.4, +11.9] | +6.8 [−16.6, +30.1] | −2.6 [−25.7, +20.5] | +14.5 [−5.4, +34.4] |
| Released with definitive repair outstanding | −6.9 [−30.5, +16.7] | −52.6 [−72.2, −33.0] | −91.1 [−107.4, −74.9] | −114.0 [−130.8, −97.2] | −38.5 [−52.9, −24.1] |
| Diverted from R2B, theatre busy | +0.3 [−23.5, +24.2] | −3.2 [−24.3, +17.8] | +14.6 [−9.9, +39.1] | +2.9 [−22.7, +28.5] | +17.8 [−4.7, +40.3] |
| R2E ICU occupancy (%) | −0.5 [−1.6, +0.6] | +0.1 [−1.0, +1.1] | −0.3 [−1.6, +1.0] | −0.5 [−1.7, +0.6] | −0.3 [−1.5, +0.8] |
| R2E post-operative overflow census | −0.41 [−0.88, +0.06] | −0.09 [−0.61, +0.43] | +0.10 [−0.59, +0.79] | +0.41 [−0.24, +1.05] | +0.19 [−0.42, +0.80] |
| R2E holding mean queue | −0.74 [−3.29, +1.80] | −0.42 [−2.26, +1.42] | −0.30 [−2.84, +2.23] | +0.15 [−2.69, +2.99] | +0.12 [−1.91, +2.15] |
| Post-definitive ICU access (%) | +0.3 [−0.8, +1.4] | +0.5 [−0.5, +1.4] | −1.2 [−2.2, −0.2]† | −0.9 [−2.0, +0.2] | −1.6 [−2.7, −0.6]† |
| Died of wounds | +1.07 [−5.00, +7.13] | −3.90 [−8.71, +0.91] | −3.10 [−7.25, +1.05] | −3.80 [−8.33, +0.73] | +0.80 [−2.15, +3.75] |
| Returns to duty | +1.6 [−35.2, +38.4] | −15.9 [−57.4, +25.6] | +4.4 [−48.3, +57.0] | −18.3 [−68.2, +31.6] | +20.3 [−22.9, +63.4] |
<!-- /GEN -->

At the shipped establishment the R2E theatre queue averaged <!-- GEN cell:ot_bed|R2E theatre mean queue|2 theatres, 3 sections (shipped)|full -->0.97 [0.63, 1.31]<!-- /GEN --> over the closing 90 days and was classified <!-- GEN cell:ot_bed|R2E theatre queue, late-half classification|2 theatres, 3 sections (shipped)|full -->converged at 0.90<!-- /GEN -->, with a theatre occupancy of <!-- GEN cell:ot_bed|R2E theatre occupancy (%)|2 theatres, 3 sections (shipped)|full -->28.1 [26.8, 29.5]<!-- /GEN --> per cent. A third theatre at three sections changed the queue by <!-- GEN cell:ot_bed_paired|R2E theatre mean queue|3 theatres, 3 sections|full -->−0.43 [−0.79, −0.06]†<!-- /GEN --> and deaths of wounds by <!-- GEN cell:ot_bed_paired|Died of wounds|3 theatres, 3 sections|full -->+1.07 [−5.00, +7.13]<!-- /GEN -->. Six sections at two theatres changed the queue by <!-- GEN cell:ot_bed_paired|R2E theatre mean queue|2 theatres, 6 sections|full -->−0.66 [−1.03, −0.29]<!-- /GEN --> and deaths of wounds by <!-- GEN cell:ot_bed_paired|Died of wounds|2 theatres, 6 sections|full -->−3.90 [−8.71, +0.91]<!-- /GEN -->, with a theatre occupancy of <!-- GEN cell:ot_bed|R2E theatre occupancy (%)|2 theatres, 6 sections|full -->28.1 [26.9, 29.4]<!-- /GEN --> per cent. Against that arm, a third theatre at six sections changed the queue by <!-- GEN cell:ot_bed_paired|R2E theatre mean queue|3 theatres, 6 sections against 2 theatres, 6 sections|full -->−0.23 [−0.31, −0.15]<!-- /GEN -->, deaths of wounds by <!-- GEN cell:ot_bed_paired|Died of wounds|3 theatres, 6 sections against 2 theatres, 6 sections|full -->+0.80 [−2.15, +3.75]<!-- /GEN -->, returns to duty by <!-- GEN cell:ot_bed_paired|Returns to duty|3 theatres, 6 sections against 2 theatres, 6 sections|full -->+20.3 [−22.9, +63.4]<!-- /GEN --> and the post-operative overflow census by <!-- GEN cell:ot_bed_paired|R2E post-operative overflow census|3 theatres, 6 sections against 2 theatres, 6 sections|full -->+0.19 [−0.42, +0.80]<!-- /GEN -->. Four theatres and eight sections changed deaths of wounds by <!-- GEN cell:ot_bed_paired|Died of wounds|4 theatres, 8 sections|full -->−3.80 [−8.33, +0.73]<!-- /GEN --> and returns to duty by <!-- GEN cell:ot_bed_paired|Returns to duty|4 theatres, 8 sections|full -->−18.3 [−68.2, +31.6]<!-- /GEN --> against the shipped establishment, and the queue was classified <!-- GEN cell:ot_bed|R2E theatre queue, late-half classification|4 theatres, 8 sections|full -->converged at 0.03<!-- /GEN -->.

At high intensity the same five arms were run under the `high_intensity` profile (30 replications, 360 days, seed 42).

<!-- GEN ot_bed_high -->
| Response | 2 theatres, 3 sections (shipped) | 3 theatres, 3 sections | 2 theatres, 6 sections | 3 theatres, 6 sections | 4 theatres, 8 sections |
|---|---|---|---|---|---|
| Sections on the larger shift against theatres | 2 against 2 | 2 against 3 | 3 against 2 | 3 against 3 | 4 against 4 |
| R2E theatre mean queue | 72.62 [51.40, 93.83] | 48.90 [31.03, 66.77] | 8.68 [6.88, 10.48] | 1.22 [1.08, 1.36] | 0.44 [0.40, 0.49] |
| R2E theatre queue, late-half classification | converged at 71.0 | converged at 49.5 | converged at 8.66 | converged at 1.29 | converged at 0.44 |
| R2E theatre queue shortfall per day | 0.014 [−0.151, 0.179] | −0.006 [−0.201, 0.190] | 0.006 [−0.009, 0.022] | 0.000 [−0.002, 0.002] | 0.000 [0.000, 0.001] |
| R2E theatre occupancy (%) | 75.9 [74.7, 77.1] | 51.4 [50.4, 52.5] | 83.3 [81.8, 84.7] | 62.0 [61.0, 63.1] | 49.9 [48.7, 51.1] |
| R2E first surgeries | 6159.3 [6060.2, 6258.4] | 6239.0 [6167.3, 6310.6] | 6197.7 [6113.1, 6282.3] | 6307.6 [6223.6, 6391.7] | 6365.3 [6258.8, 6471.8] |
| R2E theatre entry deferred | 1587.8 [1554.3, 1621.2] | 1608.2 [1579.9, 1636.4] | 1579.8 [1554.0, 1605.6] | 1606.1 [1575.3, 1636.9] | 1617.6 [1587.7, 1647.5] |
| Released with definitive repair outstanding | 1985.5 [1933.0, 2038.0] | 1966.7 [1921.6, 2011.7] | 1544.0 [1491.4, 1596.5] | 748.4 [712.3, 784.6] | 358.3 [333.8, 382.8] |
| Diverted from R2B, theatre busy | 1375.3 [1337.9, 1412.8] | 1404.4 [1370.2, 1438.5] | 1357.0 [1319.7, 1394.4] | 1387.7 [1348.1, 1427.2] | 1413.4 [1368.3, 1458.4] |
| R2E ICU occupancy (%) | 97.5 [97.4, 97.7] | 97.4 [97.3, 97.6] | 97.2 [97.1, 97.3] | 96.8 [96.7, 96.9] | 96.8 [96.7, 96.9] |
| R2E post-operative overflow census | 19.79 [19.47, 20.12] | 20.30 [19.91, 20.69] | 22.00 [21.59, 22.42] | 24.20 [23.77, 24.63] | 25.88 [25.24, 26.52] |
| R2E post-operative overflow, late-half classification | converged at 19.8 | converged at 20.4 | converged at 21.9 | converged at 24.6 | converged at 25.7 |
| R2E holding mean queue | 54.06 [52.01, 56.12] | 54.91 [52.96, 56.86] | 56.12 [54.50, 57.74] | 56.22 [54.27, 58.18] | 59.61 [57.87, 61.35] |
| R2E holding queue, late-half classification | converged at 54.1 | converged at 54.7 | **drifting, +1.0%/block**, 36.9 to 56.9 | converged at 57.5 | **drifting, +1.4%/block**, 38.7 to 63.3 |
| R2E staging overflow, late-half classification | **drifting, +11.2%/block**, 117.7 to 2,739.6 | **drifting, +11.1%/block**, 136.2 to 2,853.3 | **drifting, +11.4%/block**, 135.2 to 2,901.2 | **drifting, +11.3%/block**, 147.3 to 2,910.0 | **drifting, +10.9%/block**, 141.8 to 2,908.1 |
| Post-definitive ICU access (%) | 11.3 [11.1, 11.5] | 11.0 [10.8, 11.3] | 11.4 [11.2, 11.7] | 11.2 [11.0, 11.4] | 10.9 [10.7, 11.1] |
| Died of wounds | 568.60 [500.55, 636.65] | 489.53 [454.01, 525.05] | 307.60 [294.88, 320.32] | 270.00 [261.93, 278.07] | 264.97 [256.29, 273.64] |
| Returns to duty | 2861.9 [2824.1, 2899.7] | 2883.8 [2851.8, 2915.8] | 2899.2 [2859.7, 2938.7] | 2952.1 [2906.6, 2997.7] | 2966.1 [2928.8, 3003.3] |
<!-- /GEN -->

<!-- GEN ot_bed_paired_high -->
| Paired difference | 3 theatres, 3 sections | 2 theatres, 6 sections | 3 theatres, 6 sections | 4 theatres, 8 sections | 3 theatres, 6 sections against 2 theatres, 6 sections |
|---|---|---|---|---|---|
| R2E theatre mean queue | −23.72 [−48.04, +0.60] | −63.94 [−84.95, −42.93] | −71.40 [−92.55, −50.24] | −72.17 [−93.39, −50.96] | −7.46 [−9.23, −5.68] |
| R2E theatre occupancy (%) | −24.5 [−26.2, −22.8] | +7.4 [+5.2, +9.5] | −13.9 [−15.3, −12.5] | −26.0 [−27.8, −24.2] | −21.3 [−23.0, −19.6] |
| R2E first surgeries | +79.7 [−38.2, +197.5] | +38.4 [−94.0, +170.8] | +148.3 [+13.3, +283.3]† | +206.0 [+53.4, +358.6]† | +109.9 [−10.9, +230.8] |
| R2E theatre entry deferred | +20.4 [−25.0, +65.8] | −8.0 [−52.5, +36.6] | +18.3 [−25.3, +61.9] | +29.9 [−17.7, +77.4] | +26.3 [−11.3, +63.8] |
| Released with definitive repair outstanding | −18.8 [−88.0, +50.3] | −441.5 [−517.5, −365.6] | −1237.1 [−1298.7, −1175.5] | −1627.2 [−1687.7, −1566.7] | −795.5 [−860.6, −730.5] |
| Diverted from R2B, theatre busy | +29.0 [−16.8, +74.9] | −18.3 [−73.0, +36.4] | +12.3 [−45.6, +70.2] | +38.0 [−27.3, +103.3] | +30.6 [−25.4, +86.6] |
| R2E ICU occupancy (%) | −0.1 [−0.3, +0.1] | −0.3 [−0.5, −0.1]† | −0.7 [−0.9, −0.5] | −0.7 [−0.9, −0.5] | −0.4 [−0.6, −0.2] |
| R2E post-operative overflow census | +0.51 [−0.07, +1.08] | +2.21 [+1.61, +2.80] | +4.41 [+3.91, +4.90] | +6.08 [+5.34, +6.82] | +2.20 [+1.63, +2.76] |
| R2E holding mean queue | +0.85 [−2.44, +4.13] | +2.05 [−0.77, +4.88] | +2.16 [−0.26, +4.58] | +5.55 [+2.55, +8.54] | +0.11 [−2.55, +2.77] |
| Post-definitive ICU access (%) | −0.2 [−0.6, +0.1] | +0.2 [−0.2, +0.5] | −0.1 [−0.3, +0.2] | −0.4 [−0.7, −0.1]† | −0.2 [−0.5, +0.1] |
| Died of wounds | −79.07 [−135.37, −22.76]† | −261.00 [−330.32, −191.68] | −298.60 [−367.85, −229.35] | −303.63 [−374.40, −232.87] | −37.60 [−52.42, −22.78] |
| Returns to duty | +21.9 [−32.9, +76.7] | +37.3 [−21.8, +96.5] | +90.2 [+24.3, +156.2]† | +104.2 [+51.1, +157.3] | +52.9 [−4.8, +110.6] |
<!-- /GEN -->

At the shipped establishment at high intensity the R2E theatre queue averaged <!-- GEN cell:ot_bed_high|R2E theatre mean queue|2 theatres, 3 sections (shipped)|full -->72.62 [51.40, 93.83]<!-- /GEN --> over the closing 90 days and was classified <!-- GEN cell:ot_bed_high|R2E theatre queue, late-half classification|2 theatres, 3 sections (shipped)|full -->converged at 71.0<!-- /GEN -->, with a theatre occupancy of <!-- GEN cell:ot_bed_high|R2E theatre occupancy (%)|2 theatres, 3 sections (shipped)|full -->75.9 [74.7, 77.1]<!-- /GEN --> per cent. A third theatre at three sections changed the queue by <!-- GEN cell:ot_bed_paired_high|R2E theatre mean queue|3 theatres, 3 sections|full -->−23.72 [−48.04, +0.60]<!-- /GEN --> and deaths of wounds by <!-- GEN cell:ot_bed_paired_high|Died of wounds|3 theatres, 3 sections|full -->−79.07 [−135.37, −22.76]†<!-- /GEN -->. Six sections at two theatres changed the queue by <!-- GEN cell:ot_bed_paired_high|R2E theatre mean queue|2 theatres, 6 sections|full -->−63.94 [−84.95, −42.93]<!-- /GEN --> and deaths of wounds by <!-- GEN cell:ot_bed_paired_high|Died of wounds|2 theatres, 6 sections|full -->−261.00 [−330.32, −191.68]<!-- /GEN -->, with a theatre occupancy of <!-- GEN cell:ot_bed_high|R2E theatre occupancy (%)|2 theatres, 6 sections|full -->83.3 [81.8, 84.7]<!-- /GEN --> per cent. Against that arm, a third theatre at six sections changed the queue by <!-- GEN cell:ot_bed_paired_high|R2E theatre mean queue|3 theatres, 6 sections against 2 theatres, 6 sections|full -->−7.46 [−9.23, −5.68]<!-- /GEN -->, deaths of wounds by <!-- GEN cell:ot_bed_paired_high|Died of wounds|3 theatres, 6 sections against 2 theatres, 6 sections|full -->−37.60 [−52.42, −22.78]<!-- /GEN -->, returns to duty by <!-- GEN cell:ot_bed_paired_high|Returns to duty|3 theatres, 6 sections against 2 theatres, 6 sections|full -->+52.9 [−4.8, +110.6]<!-- /GEN --> and the post-operative overflow census by <!-- GEN cell:ot_bed_paired_high|R2E post-operative overflow census|3 theatres, 6 sections against 2 theatres, 6 sections|full -->+2.20 [+1.63, +2.76]<!-- /GEN -->. Four theatres and eight sections changed deaths of wounds by <!-- GEN cell:ot_bed_paired_high|Died of wounds|4 theatres, 8 sections|full -->−303.63 [−374.40, −232.87]<!-- /GEN --> and returns to duty by <!-- GEN cell:ot_bed_paired_high|Returns to duty|4 theatres, 8 sections|full -->+104.2 [+51.1, +157.3]<!-- /GEN --> against the shipped establishment, and the queue was classified <!-- GEN cell:ot_bed_high|R2E theatre queue, late-half classification|4 theatres, 8 sections|full -->converged at 0.44<!-- /GEN -->.

### R2E Intensive Care Bed Establishment

**Question.** What changes when the R2E intensive care establishment is raised from the shipped four beds, and does the post-operative overflow census, where an operated casualty finding no intensive care bed is placed, stop growing at any bed count swept? `[default and high_intensity, R2E ICU 4 to 16 beds · 360 d · 30 replications · paired, closing 90 d]`

Each arm is a bed count for the R2E intensive care pool: four (shipped), six, eight, twelve and sixteen. No stay at R2E seizes an intensive care section, so the beds are varied without staff and the nurses per bed row is the shipped three sections of four nurses over the bed count, a ratio the model does not charge. R2B intensive care beds are not varied.

<!-- GEN icu_bed -->
| Response | 4 beds (shipped) | 6 beds | 8 beds | 12 beds | 16 beds |
|---|---|---|---|---|---|
| Nurses per ICU bed at three sections | 3.00 | 2.00 | 1.50 | 1.00 | 0.75 |
| R2E post-operative overflow census | 5.72 [5.36, 6.09] | 4.13 [3.78, 4.48] | 2.93 [2.63, 3.23] | 1.94 [1.60, 2.28] | 1.23 [0.98, 1.48] |
| R2E post-operative overflow, late-half classification | converged at 5.54 | converged at 4.21 | converged at 3.22 | converged at 1.94 | converged at 1.12 |
| R2E post-operative overflow growth per day | 0.003 [−0.001, 0.007] | 0.000 [−0.004, 0.003] | −0.003 [−0.006, 0.000] | 0.000 [−0.002, 0.003] | 0.001 [−0.002, 0.003] |
| R2E ICU occupancy (%) | 86.8 [86.0, 87.6] | 81.6 [80.4, 82.8] | 75.7 [74.5, 76.9] | 64.0 [62.3, 65.7] | 54.4 [52.5, 56.3] |
| R2E holding mean queue | 24.38 [22.59, 26.16] | 24.22 [22.70, 25.73] | 23.67 [22.55, 24.79] | 22.53 [20.84, 24.23] | 24.23 [22.36, 26.10] |
| R2E holding queue, late-half classification | converged at 23.8 | converged at 23.8 | converged at 23.8 | converged at 23.1 | converged at 23.2 |
| R2E holding queue shortfall per day | 0.007 [−0.012, 0.026] | 0.006 [−0.010, 0.021] | 0.003 [−0.011, 0.017] | −0.009 [−0.026, 0.008] | 0.009 [−0.008, 0.027] |
| R2E staging overflow, late-half classification | converged at 30.1 | converged at 30.9 | converged at 32.1 | converged at 32.3 | converged at 31.6 |
| Post-operative care in a holding bed (%) | 36.5 [36.1, 36.9] | 29.3 [28.7, 29.9] | 22.6 [21.9, 23.3] | 13.6 [13.0, 14.2] | 8.1 [7.4, 8.7] |
| Post-definitive ICU access (%) | 35.2 [34.6, 35.9] | 50.5 [49.6, 51.4] | 63.2 [62.1, 64.3] | 78.6 [77.6, 79.7] | 88.1 [87.3, 89.0] |
| R2E theatre entry deferred | 375.1 [361.6, 388.7] | 303.1 [291.1, 315.0] | 228.0 [212.4, 243.6] | 141.8 [129.1, 154.4] | 83.2 [76.0, 90.5] |
| Died of wounds | 21.17 [17.55, 24.79] | 19.37 [17.06, 21.68] | 20.13 [16.99, 23.28] | 20.50 [17.81, 23.19] | 21.00 [18.14, 23.86] |
| Returns to duty | 2062.4 [2031.8, 2093.0] | 2050.2 [2025.5, 2075.0] | 2059.4 [2027.3, 2091.5] | 2045.1 [2015.1, 2075.2] | 2038.3 [2005.6, 2071.1] |
| Critical-route evacuation wait (days) | 5.65 [5.12, 6.18] | 6.08 [5.39, 6.76] | 6.19 [5.15, 7.23] | 6.17 [5.22, 7.11] | 6.18 [5.54, 6.81] |
| Role 4 peak census | 209.2 [205.2, 213.2] | 211.9 [207.1, 216.7] | 211.2 [206.0, 216.5] | 209.9 [205.5, 214.3] | 211.6 [207.7, 215.5] |
| Role 4 closing mean census | 121.8 [118.1, 125.5] | 118.1 [113.3, 123.0] | 116.2 [111.5, 120.9] | 117.9 [112.5, 123.4] | 119.5 [114.2, 124.7] |
<!-- /GEN -->

<!-- GEN icu_bed_paired -->
| Paired difference against the shipped establishment | 6 beds | 8 beds | 12 beds | 16 beds |
|---|---|---|---|---|
| R2E post-operative overflow census | −1.59 [−2.04, −1.15] | −2.79 [−3.27, −2.31] | −3.78 [−4.28, −3.29] | −4.49 [−4.91, −4.07] |
| R2E ICU occupancy (%) | −5.2 [−6.6, −3.8] | −11.1 [−12.5, −9.7] | −22.8 [−24.7, −20.9] | −32.4 [−34.4, −30.5] |
| R2E holding mean queue | −0.16 [−2.52, +2.20] | −0.71 [−2.65, +1.24] | −1.84 [−4.63, +0.94] | −0.14 [−2.78, +2.49] |
| Post-operative care in a holding bed (%) | −7.2 [−7.9, −6.5] | −13.9 [−14.6, −13.2] | −22.9 [−23.6, −22.2] | −28.5 [−29.2, −27.7] |
| Post-definitive ICU access (%) | +15.3 [+14.0, +16.5] | +28.0 [+26.9, +29.0] | +43.4 [+42.2, +44.7] | +52.9 [+51.8, +53.9] |
| R2E theatre entry deferred | −72.1 [−91.3, −52.9] | −147.1 [−163.6, −130.6] | −233.4 [−251.0, −215.7] | −291.9 [−306.3, −277.5] |
| Died of wounds | −1.80 [−6.44, +2.84] | −1.03 [−5.71, +3.65] | −0.67 [−4.95, +3.62] | −0.17 [−4.23, +3.90] |
| Returns to duty | −12.2 [−54.4, +30.1] | −3.0 [−55.8, +49.8] | −17.3 [−60.1, +25.6] | −24.1 [−69.5, +21.4] |
| Critical-route evacuation wait (days) | +0.42 [−0.46, +1.31] | +0.54 [−0.38, +1.46] | +0.52 [−0.59, +1.62] | +0.52 [−0.28, +1.33] |
| Role 4 peak census | +2.6 [−4.5, +9.7] | +2.0 [−4.3, +8.3] | +0.6 [−5.4, +6.6] | +2.3 [−2.6, +7.3] |
| Role 4 closing mean census | −3.6 [−8.9, +1.6] | −5.6 [−11.2, +0.0] | −3.8 [−10.6, +2.9] | −2.3 [−8.0, +3.4] |
<!-- /GEN -->

At the shipped establishment the R2E post-operative overflow census over the closing 90 days averaged <!-- GEN cell:icu_bed|R2E post-operative overflow census|4 beds (shipped)|full -->5.72 [5.36, 6.09]<!-- /GEN --> and was classified <!-- GEN cell:icu_bed|R2E post-operative overflow, late-half classification|4 beds (shipped)|full -->converged at 5.54<!-- /GEN -->, with <!-- GEN cell:icu_bed|R2E ICU occupancy (%)|4 beds (shipped)|full -->86.8 [86.0, 87.6]<!-- /GEN --> percent occupancy of the intensive care beds; <!-- GEN cell:icu_bed|Post-operative care in a holding bed (%)|4 beds (shipped)|full -->36.5 [36.1, 36.9]<!-- /GEN --> percent of post-operative care was taken in a holding bed and <!-- GEN cell:icu_bed|Post-definitive ICU access (%)|4 beds (shipped)|full -->35.2 [34.6, 35.9]<!-- /GEN --> percent of post-definitive care in an intensive care bed. At sixteen beds the census averaged <!-- GEN cell:icu_bed|R2E post-operative overflow census|16 beds|full -->1.23 [0.98, 1.48]<!-- /GEN -->. Twelve beds changed the share of post-operative care taken in a holding bed by <!-- GEN cell:icu_bed_paired|Post-operative care in a holding bed (%)|12 beds|full -->−22.9 [−23.6, −22.2]<!-- /GEN --> percentage points and the post-definitive access by <!-- GEN cell:icu_bed_paired|Post-definitive ICU access (%)|12 beds|full -->+43.4 [+42.2, +44.7]<!-- /GEN --> percentage points. The paired differences in deaths of wounds at six, eight, twelve and sixteen beds were <!-- GEN cell:icu_bed_paired|Died of wounds|6 beds|full -->−1.80 [−6.44, +2.84]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired|Died of wounds|8 beds|full -->−1.03 [−5.71, +3.65]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired|Died of wounds|12 beds|full -->−0.67 [−4.95, +3.62]<!-- /GEN --> and <!-- GEN cell:icu_bed_paired|Died of wounds|16 beds|full -->−0.17 [−4.23, +3.90]<!-- /GEN -->, and in returns to duty <!-- GEN cell:icu_bed_paired|Returns to duty|6 beds|full -->−12.2 [−54.4, +30.1]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired|Returns to duty|8 beds|full -->−3.0 [−55.8, +49.8]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired|Returns to duty|12 beds|full -->−17.3 [−60.1, +25.6]<!-- /GEN --> and <!-- GEN cell:icu_bed_paired|Returns to duty|16 beds|full -->−24.1 [−69.5, +21.4]<!-- /GEN -->. The R2E holding queue was classified <!-- GEN cell:icu_bed|R2E holding queue, late-half classification|4 beds (shipped)|full -->converged at 23.8<!-- /GEN --> at the shipped establishment and <!-- GEN cell:icu_bed|R2E holding queue, late-half classification|16 beds|full -->converged at 23.2<!-- /GEN --> at sixteen beds.

At high intensity the same five arms were run under the `high_intensity` profile (30 replications, 360 days, seed 42).

<!-- GEN icu_bed_high -->
| Response | 4 beds (shipped) | 6 beds | 8 beds | 12 beds | 16 beds |
|---|---|---|---|---|---|
| Nurses per ICU bed at three sections | 3.00 | 2.00 | 1.50 | 1.00 | 0.75 |
| R2E post-operative overflow census | 19.79 [19.47, 20.12] | 18.51 [18.27, 18.76] | 17.02 [16.78, 17.25] | 14.62 [14.36, 14.87] | 11.66 [11.39, 11.93] |
| R2E post-operative overflow, late-half classification | converged at 19.8 | converged at 18.5 | converged at 17.1 | converged at 14.6 | converged at 11.8 |
| R2E post-operative overflow growth per day | −0.001 [−0.004, 0.002] | 0.000 [−0.002, 0.003] | −0.001 [−0.004, 0.002] | −0.001 [−0.003, 0.002] | −0.002 [−0.005, 0.001] |
| R2E ICU occupancy (%) | 97.5 [97.4, 97.7] | 97.3 [97.2, 97.4] | 97.0 [96.9, 97.1] | 96.4 [96.2, 96.7] | 95.3 [95.0, 95.6] |
| R2E holding mean queue | 54.06 [52.01, 56.12] | 54.39 [52.71, 56.07] | 53.39 [52.08, 54.70] | 54.75 [53.24, 56.27] | 54.23 [52.53, 55.92] |
| R2E holding queue, late-half classification | converged at 54.1 | **drifting, +1.2%/block**, 33.3 to 55.3 | converged at 53.0 | converged at 54.4 | converged at 53.7 |
| R2E holding queue shortfall per day | 0.002 [−0.022, 0.027] | 0.021 [0.001, 0.042] | 0.003 [−0.016, 0.021] | 0.005 [−0.013, 0.023] | 0.011 [−0.008, 0.029] |
| R2E staging overflow, late-half classification | **drifting, +11.2%/block**, 117.7 to 2,739.6 | **drifting, +11.3%/block**, 117.9 to 2,705.4 | **drifting, +11.1%/block**, 124.2 to 2,733.6 | **drifting, +11.2%/block**, 125.8 to 2,746.1 | **drifting, +11.0%/block**, 124.3 to 2,746.7 |
| Post-operative care in a holding bed (%) | 43.4 [43.1, 43.6] | 40.4 [40.1, 40.6] | 37.7 [37.4, 38.0] | 32.0 [31.7, 32.2] | 26.0 [25.8, 26.3] |
| Post-definitive ICU access (%) | 11.3 [11.1, 11.5] | 17.1 [16.9, 17.4] | 23.0 [22.6, 23.3] | 35.2 [34.7, 35.8] | 47.2 [46.7, 47.7] |
| R2E theatre entry deferred | 1587.8 [1554.3, 1621.2] | 1522.9 [1492.1, 1553.8] | 1428.8 [1400.9, 1456.8] | 1263.3 [1237.3, 1289.2] | 1061.7 [1038.1, 1085.4] |
| Died of wounds | 568.60 [500.55, 636.65] | 592.40 [523.63, 661.17] | 634.23 [534.69, 733.77] | 564.73 [491.97, 637.50] | 617.77 [544.27, 691.26] |
| Returns to duty | 2861.9 [2824.1, 2899.7] | 2842.7 [2809.2, 2876.2] | 2850.9 [2814.9, 2886.9] | 2854.1 [2822.1, 2886.2] | 2825.4 [2786.9, 2863.9] |
| Critical-route evacuation wait (days) | 109.67 [108.40, 110.95] | 109.90 [108.71, 111.08] | 110.83 [109.70, 111.97] | 110.73 [109.68, 111.77] | 111.11 [109.99, 112.23] |
| Role 4 peak census | 281.3 [279.0, 283.6] | 280.8 [279.2, 282.4] | 280.2 [278.1, 282.4] | 281.5 [279.7, 283.4] | 283.6 [281.3, 286.0] |
| Role 4 closing mean census | 174.8 [172.8, 176.8] | 176.4 [174.9, 177.9] | 175.2 [173.4, 176.9] | 175.5 [174.0, 177.0] | 176.2 [174.7, 177.8] |
<!-- /GEN -->

<!-- GEN icu_bed_paired_high -->
| Paired difference against the shipped establishment | 6 beds | 8 beds | 12 beds | 16 beds |
|---|---|---|---|---|
| R2E post-operative overflow census | −1.28 [−1.67, −0.89] | −2.78 [−3.18, −2.38] | −5.18 [−5.58, −4.78] | −8.13 [−8.56, −7.70] |
| R2E ICU occupancy (%) | −0.2 [−0.4, +0.0] | −0.5 [−0.7, −0.3] | −1.1 [−1.3, −0.8] | −2.2 [−2.6, −1.8] |
| R2E holding mean queue | +0.33 [−2.08, +2.74] | −0.67 [−3.14, +1.79] | +0.69 [−1.91, +3.29] | +0.16 [−2.38, +2.71] |
| Post-operative care in a holding bed (%) | −3.0 [−3.4, −2.7] | −5.7 [−5.9, −5.4] | −11.4 [−11.8, −11.0] | −17.3 [−17.7, −17.0] |
| Post-definitive ICU access (%) | +5.9 [+5.6, +6.1] | +11.7 [+11.3, +12.1] | +24.0 [+23.3, +24.6] | +35.9 [+35.4, +36.5] |
| R2E theatre entry deferred | −64.8 [−104.6, −25.1]† | −158.9 [−205.0, −112.9] | −324.5 [−371.4, −277.6] | −526.0 [−568.7, −483.4] |
| Died of wounds | +23.80 [−67.86, +115.46] | +65.63 [−50.55, +181.81] | −3.87 [−108.47, +100.74] | +49.17 [−57.08, +155.41] |
| Returns to duty | −19.2 [−67.4, +29.0] | −11.0 [−60.4, +38.4] | −7.8 [−61.2, +45.7] | −36.5 [−86.6, +13.6] |
| Critical-route evacuation wait (days) | +0.22 [−1.41, +1.86] | +1.16 [−0.38, +2.70] | +1.05 [−0.43, +2.54] | +1.43 [−0.05, +2.91] |
| Role 4 peak census | −0.5 [−3.3, +2.3] | −1.1 [−4.3, +2.2] | +0.2 [−2.9, +3.4] | +2.3 [−0.7, +5.3] |
| Role 4 closing mean census | +1.6 [−0.8, +4.1] | +0.4 [−2.0, +2.8] | +0.7 [−1.9, +3.3] | +1.4 [−0.9, +3.8] |
<!-- /GEN -->

At the shipped establishment the high-intensity post-operative overflow census averaged <!-- GEN cell:icu_bed_high|R2E post-operative overflow census|4 beds (shipped)|full -->19.79 [19.47, 20.12]<!-- /GEN --> and was classified <!-- GEN cell:icu_bed_high|R2E post-operative overflow, late-half classification|4 beds (shipped)|full -->converged at 19.8<!-- /GEN -->, with <!-- GEN cell:icu_bed_high|R2E ICU occupancy (%)|4 beds (shipped)|full -->97.5 [97.4, 97.7]<!-- /GEN --> percent occupancy. At sixteen beds, four times the shipped establishment, the census averaged <!-- GEN cell:icu_bed_high|R2E post-operative overflow census|16 beds|full -->11.66 [11.39, 11.93]<!-- /GEN --> and was classified <!-- GEN cell:icu_bed_high|R2E post-operative overflow, late-half classification|16 beds|full -->converged at 11.8<!-- /GEN -->, and post-definitive intensive care access changed by <!-- GEN cell:icu_bed_paired_high|Post-definitive ICU access (%)|16 beds|full -->+35.9 [+35.4, +36.5]<!-- /GEN --> percentage points. The R2E holding queue was classified <!-- GEN cell:icu_bed_high|R2E holding queue, late-half classification|4 beds (shipped)|full -->converged at 54.1<!-- /GEN --> at the shipped establishment and <!-- GEN cell:icu_bed_high|R2E holding queue, late-half classification|16 beds|full -->converged at 53.7<!-- /GEN --> at sixteen, and the staging overflow census <!-- GEN cell:icu_bed_high|R2E staging overflow, late-half classification|4 beds (shipped)|full -->**drifting, +11.2%/block**, 117.7 to 2,739.6<!-- /GEN --> and <!-- GEN cell:icu_bed_high|R2E staging overflow, late-half classification|16 beds|full -->**drifting, +11.0%/block**, 124.3 to 2,746.7<!-- /GEN -->. The mean evacuation wait of the critical route was <!-- GEN cell:icu_bed_high|Critical-route evacuation wait (days)|4 beds (shipped)|full -->109.67 [108.40, 110.95]<!-- /GEN --> days at the shipped establishment and <!-- GEN cell:icu_bed_high|Critical-route evacuation wait (days)|16 beds|full -->111.11 [109.99, 112.23]<!-- /GEN --> days at sixteen beds. The paired differences in deaths of wounds at six, eight, twelve and sixteen beds were <!-- GEN cell:icu_bed_paired_high|Died of wounds|6 beds|full -->+23.80 [−67.86, +115.46]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired_high|Died of wounds|8 beds|full -->+65.63 [−50.55, +181.81]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired_high|Died of wounds|12 beds|full -->−3.87 [−108.47, +100.74]<!-- /GEN --> and <!-- GEN cell:icu_bed_paired_high|Died of wounds|16 beds|full -->+49.17 [−57.08, +155.41]<!-- /GEN -->, and in returns to duty at sixteen beds <!-- GEN cell:icu_bed_paired_high|Returns to duty|16 beds|full -->−36.5 [−86.6, +13.6]<!-- /GEN -->.

![Two columns of panels of campaign responses against the R2E intensive care bed count at moderate and high intensity](../images/icu_bed_sweep.png)

The R2E post-operative overflow census, intensive care occupancy, holding queue, post-operative and post-definitive care and the critical-route evacuation wait of each establishment, with deaths of wounds and returns to duty, at moderate intensity (left) and high intensity (right), with the shipped establishment marked.

### Priority 1 Triage Share

**Question.** Do the capacity, flow and mortality responses change when the Priority 1 share of the background triage split is lowered from the shipped sixty-five per cent towards the comparator range, and does the treated-cohort died-of-wounds rate move with it? `[default and high_intensity, Priority 1 share fifteen to sixty-five per cent · 360 d · 30 replications · paired, closing 90 d]`

Each arm is a Priority 1 share of the background split, the remainder divided between Priority 2 and Priority 3 in the shipped ratio of 20 to 15. The died-of-wounds parameters, the surgical need rates and the damage control rates are held at their shipped values, and the treated-cohort rate is the mean of each arm's per-replication rates with a Student $t$ interval.

<!-- GEN triage_split -->
| Response | P1 15% | P1 30% | P1 45% | P1 65% (shipped) |
|---|---|---|---|---|
| Priority split (%, P1 / P2 / P3) | 15 / 49 / 36 | 30 / 40 / 30 | 45 / 31 / 24 | 65 / 20 / 15 |
| Wounded in action | 2328.8 [2252.5, 2405.2] | 2348.0 [2271.8, 2424.3] | 2276.8 [2218.2, 2335.3] | 2303.9 [2246.5, 2361.3] |
| Died of wounds | 17.23 [12.16, 22.30] | 15.83 [13.48, 18.19] | 17.47 [15.33, 19.60] | 21.17 [17.55, 24.79] |
| Died of wounds per wounded in action (%) | 0.72 [0.54, 0.89] | 0.67 [0.58, 0.75] | 0.76 [0.68, 0.85] | 0.91 [0.77, 1.05] |
| Treated-cohort died-of-wounds rate | 0.407% [0.297%, 0.517%] | 0.381% [0.331%, 0.432%] | 0.431% [0.380%, 0.482%] | 0.514% [0.428%, 0.600%] |
| Expected deaths accrued after surgery (%) | 15% | 19% | 20% | 24% |
| R2B surgeries | 756.8 [740.5, 773.0] | 789.1 [772.3, 805.9] | 790.8 [779.1, 802.4] | 824.8 [813.4, 836.2] |
| Diverted from R2B, team off shift | 1064.9 [1037.9, 1091.9] | 1110.8 [1078.3, 1143.3] | 1128.4 [1102.4, 1154.4] | 1201.5 [1178.8, 1224.3] |
| Diverted from R2B, theatre busy | 204.4 [188.8, 220.0] | 211.3 [195.7, 227.0] | 218.2 [206.0, 230.5] | 236.8 [220.9, 252.8] |
| R2E first surgeries | 1459.3 [1415.6, 1503.0] | 1571.7 [1519.5, 1624.0] | 1643.6 [1602.6, 1684.5] | 1816.0 [1774.7, 1857.3] |
| Time to first incision (hours) | 9.08 [6.74, 11.43] | 7.31 [6.65, 7.96] | 7.44 [6.55, 8.33] | 7.39 [6.03, 8.76] |
| Casualties requiring surgery operated on (%) | 99.7 [99.6, 99.8] | 99.4 [98.7, 100.1] | 99.7 [99.6, 99.8] | 99.7 [99.6, 99.8] |
| R2E theatre queue, late-half classification | converged at 0.76 | converged at 0.54 | converged at 0.76 | converged at 0.90 |
| R2E post-operative overflow, late-half classification | converged at 3.98 | converged at 4.64 | converged at 5.02 | converged at 5.54 |
| R2E holding queue, late-half classification | converged at 41.0 | converged at 36.5 | converged at 29.8 | converged at 23.8 |
| R2E staging overflow, late-half classification | converged at 17.9 | converged at 19.1 | converged at 21.0 | converged at 30.1 |
| R2E theatre mean queue | 0.56 [0.41, 0.71] | 0.54 [0.41, 0.67] | 0.86 [0.51, 1.22] | 0.97 [0.63, 1.31] |
| R2E post-operative overflow census | 4.05 [3.79, 4.32] | 4.65 [4.30, 4.99] | 5.12 [4.79, 5.44] | 5.72 [5.36, 6.09] |
| R2E holding mean queue | 41.18 [39.18, 43.19] | 37.77 [35.69, 39.84] | 30.42 [28.83, 32.00] | 24.38 [22.59, 26.16] |
| R2E staging overflow census | 17.72 [16.83, 18.62] | 19.42 [18.36, 20.49] | 21.70 [19.28, 24.12] | 32.16 [26.87, 37.46] |
| R2E ICU occupancy (%) | 81.1 [80.0, 82.2] | 84.0 [83.2, 84.8] | 85.3 [84.8, 85.9] | 86.8 [86.0, 87.6] |
| Returns to duty | 2627.8 [2587.2, 2668.4] | 2466.6 [2431.5, 2501.6] | 2263.2 [2236.5, 2289.9] | 2062.4 [2031.8, 2093.0] |
| Strategic evacuation decisions | 1814.9 [1771.0, 1858.9] | 1986.8 [1933.7, 2039.8] | 2098.3 [2054.9, 2141.7] | 2353.2 [2305.7, 2400.7] |
| Critical airlift backlog at the close | 4.5 [3.0, 6.1] | 6.9 [5.4, 8.5] | 13.6 [9.5, 17.7] | 30.0 [20.8, 39.3] |
| Role 4 peak ICU census | 29.2 [26.3, 32.1] | 47.8 [44.8, 50.8] | 58.3 [56.7, 59.9] | 63.8 [62.8, 64.8] |
<!-- /GEN -->

<!-- GEN triage_split_paired -->
| Paired difference against the shipped split | P1 15% | P1 30% | P1 45% |
|---|---|---|---|
| Died of wounds | −3.93 [−10.45, +2.58] | −5.33 [−10.09, −0.58]† | −3.70 [−8.21, +0.81] |
| Died of wounds per wounded in action (%) | −0.19 [−0.42, +0.04] | −0.24 [−0.42, −0.06]† | −0.14 [−0.32, +0.03] |
| Diverted from R2B, team off shift | −136.6 [−174.2, −99.1] | −90.7 [−133.4, −48.1] | −73.1 [−109.2, −37.0] |
| Diverted from R2B, theatre busy | −32.4 [−58.4, −6.4]† | −25.5 [−47.3, −3.7]† | −18.6 [−40.9, +3.7] |
| Time to first incision (hours) | +1.69 [−1.17, +4.56] | −0.09 [−1.63, +1.46] | +0.04 [−1.69, +1.78] |
| Casualties requiring surgery operated on (%) | −0.0 [−0.1, +0.1] | −0.3 [−1.0, +0.4] | +0.0 [−0.1, +0.1] |
| R2E theatre mean queue | −0.41 [−0.81, −0.00]† | −0.43 [−0.78, −0.07]† | −0.10 [−0.61, +0.40] |
| R2E post-operative overflow census | −1.67 [−2.13, −1.21] | −1.08 [−1.55, −0.61] | −0.61 [−1.13, −0.08]† |
| R2E holding mean queue | +16.81 [+14.19, +19.43] | +13.39 [+11.22, +15.56] | +6.04 [+3.73, +8.35] |
| R2E staging overflow census | −14.44 [−19.87, −9.01] | −12.74 [−17.86, −7.61] | −10.46 [−16.08, −4.84] |
| R2E ICU occupancy (%) | −5.7 [−7.1, −4.3] | −2.8 [−3.9, −1.7] | −1.5 [−2.5, −0.4]† |
| Returns to duty | +565.4 [+510.9, +619.8] | +404.2 [+368.9, +439.4] | +200.8 [+161.9, +239.8] |
| Strategic evacuation decisions | −538.3 [−606.1, −470.4] | −366.4 [−439.3, −293.5] | −254.9 [−325.0, −184.8] |
| Critical airlift backlog at the close | −25.5 [−35.0, −16.0] | −23.1 [−32.6, −13.6] | −16.4 [−26.6, −6.2]† |
| Role 4 peak ICU census | −34.6 [−37.9, −31.3] | −16.0 [−19.1, −12.8] | −5.5 [−7.4, −3.6] |
<!-- /GEN -->

At the shipped share the treated-cohort died-of-wounds rate was <!-- GEN cell:triage_split|Treated-cohort died-of-wounds rate|P1 65% (shipped)|full -->0.514% [0.428%, 0.600%]<!-- /GEN -->, with <!-- GEN cell:triage_split|Expected deaths accrued after surgery (%)|P1 65% (shipped)|full -->24%<!-- /GEN --> of the cohort's expected deaths accrued after surgery, and at a share of fifteen per cent it was <!-- GEN cell:triage_split|Treated-cohort died-of-wounds rate|P1 15%|full -->0.407% [0.297%, 0.517%]<!-- /GEN -->, with <!-- GEN cell:triage_split|Expected deaths accrued after surgery (%)|P1 15%|full -->15%<!-- /GEN -->; the base rates were fitted at the shipped share, to the anchor the calibration table states: <!-- GEN cell:dow_calibration|Shipped default|Historical anchor|full -->fitted to 0.46% (Ajax Bay, 30 days)<!-- /GEN -->. Lowering the share from sixty-five per cent to fifteen per cent changed R2E first surgeries from <!-- GEN cell:triage_split|R2E first surgeries|P1 65% (shipped)|full -->1816.0 [1774.7, 1857.3]<!-- /GEN --> to <!-- GEN cell:triage_split|R2E first surgeries|P1 15%|full -->1459.3 [1415.6, 1503.0]<!-- /GEN -->, the post-operative overflow census by <!-- GEN cell:triage_split_paired|R2E post-operative overflow census|P1 15%|full -->−1.67 [−2.13, −1.21]<!-- /GEN -->, the staging overflow census by <!-- GEN cell:triage_split_paired|R2E staging overflow census|P1 15%|full -->−14.44 [−19.87, −9.01]<!-- /GEN -->, the critical airlift backlog at the close by <!-- GEN cell:triage_split_paired|Critical airlift backlog at the close|P1 15%|full -->−25.5 [−35.0, −16.0]<!-- /GEN --> and the Role 4 peak intensive care census by <!-- GEN cell:triage_split_paired|Role 4 peak ICU census|P1 15%|full -->−34.6 [−37.9, −31.3]<!-- /GEN -->, and changed the R2E holding queue by <!-- GEN cell:triage_split_paired|R2E holding mean queue|P1 15%|full -->+16.81 [+14.19, +19.43]<!-- /GEN --> and returns to duty by <!-- GEN cell:triage_split_paired|Returns to duty|P1 15%|full -->+565.4 [+510.9, +619.8]<!-- /GEN -->. The R2E theatre queue was classified <!-- GEN cell:triage_split|R2E theatre queue, late-half classification|P1 65% (shipped)|full -->converged at 0.90<!-- /GEN --> at the shipped share and <!-- GEN cell:triage_split|R2E theatre queue, late-half classification|P1 15%|full -->converged at 0.76<!-- /GEN --> at fifteen per cent, the post-operative overflow <!-- GEN cell:triage_split|R2E post-operative overflow, late-half classification|P1 65% (shipped)|full -->converged at 5.54<!-- /GEN --> and <!-- GEN cell:triage_split|R2E post-operative overflow, late-half classification|P1 15%|full -->converged at 3.98<!-- /GEN -->, and the staging overflow <!-- GEN cell:triage_split|R2E staging overflow, late-half classification|P1 65% (shipped)|full -->converged at 30.1<!-- /GEN --> and <!-- GEN cell:triage_split|R2E staging overflow, late-half classification|P1 15%|full -->converged at 17.9<!-- /GEN -->. The paired differences in deaths of wounds at fifteen, thirty and forty-five per cent were <!-- GEN cell:triage_split_paired|Died of wounds|P1 15%|full -->−3.93 [−10.45, +2.58]<!-- /GEN -->, <!-- GEN cell:triage_split_paired|Died of wounds|P1 30%|full -->−5.33 [−10.09, −0.58]†<!-- /GEN --> and <!-- GEN cell:triage_split_paired|Died of wounds|P1 45%|full -->−3.70 [−8.21, +0.81]<!-- /GEN -->.

At high intensity the same four arms were run under the `high_intensity` profile (30 replications, 360 days, seed 42).

<!-- GEN triage_split_high -->
| Response | P1 15% | P1 30% | P1 45% | P1 65% (shipped) |
|---|---|---|---|---|
| Priority split (%, P1 / P2 / P3) | 15 / 49 / 36 | 30 / 40 / 30 | 45 / 31 / 24 | 65 / 20 / 15 |
| Wounded in action | 8517.8 [8380.7, 8654.8] | 8460.0 [8375.9, 8544.0] | 8496.7 [8392.8, 8600.6] | 8314.4 [8174.6, 8454.2] |
| Died of wounds | 167.57 [151.58, 183.55] | 231.23 [215.37, 247.09] | 362.23 [333.09, 391.38] | 568.60 [500.55, 636.65] |
| Died of wounds per wounded in action (%) | 1.95 [1.79, 2.11] | 2.72 [2.56, 2.89] | 4.25 [3.95, 4.55] | 6.77 [6.06, 7.48] |
| Treated-cohort died-of-wounds rate | 1.635% [1.497%, 1.773%] | 2.281% [2.140%, 2.423%] | 3.582% [3.321%, 3.842%] | 5.696% [5.080%, 6.311%] |
| Expected deaths accrued after surgery (%) | 24% | 28% | 23% | 18% |
| R2B surgeries | 1717.2 [1699.7, 1734.8] | 1743.4 [1731.7, 1755.2] | 1777.2 [1760.0, 1794.4] | 1795.4 [1776.0, 1814.8] |
| Diverted from R2B, team off shift | 3458.0 [3406.7, 3509.3] | 3560.7 [3524.8, 3596.6] | 3727.1 [3685.9, 3768.3] | 3825.8 [3770.0, 3881.7] |
| Diverted from R2B, theatre busy | 1223.9 [1187.0, 1260.8] | 1285.6 [1259.6, 1311.6] | 1338.5 [1307.2, 1369.7] | 1375.3 [1337.9, 1412.8] |
| R2E first surgeries | 5237.7 [5148.6, 5326.8] | 5537.4 [5479.6, 5595.1] | 5893.8 [5818.6, 5969.0] | 6159.3 [6060.2, 6258.4] |
| Time to first incision (hours) | 28.14 [25.40, 30.87] | 33.43 [30.67, 36.20] | 47.16 [41.13, 53.19] | 71.06 [57.93, 84.19] |
| Casualties requiring surgery operated on (%) | 99.3 [99.2, 99.5] | 99.1 [98.9, 99.4] | 99.0 [98.8, 99.3] | 98.6 [98.3, 98.8] |
| R2E theatre queue, late-half classification | converged at 16.0 | converged at 22.3 | converged at 39.9 | converged at 71.0 |
| R2E post-operative overflow, late-half classification | converged at 17.6 | converged at 18.5 | converged at 19.3 | converged at 19.8 |
| R2E holding queue, late-half classification | converged at 118.9 | converged at 98.0 | converged at 77.2 | converged at 54.1 |
| R2E staging overflow, late-half classification | **drifting, +11.6%/block**, 68.2 to 1,163.5 | **drifting, +10.8%/block**, 76.4 to 1,328.0 | **drifting, +11.0%/block**, 92.4 to 1,790.8 | **drifting, +11.2%/block**, 117.7 to 2,739.6 |
| R2E theatre mean queue | 15.31 [11.84, 18.79] | 24.49 [18.73, 30.25] | 48.82 [32.59, 65.05] | 72.62 [51.40, 93.83] |
| R2E post-operative overflow census | 17.50 [17.08, 17.91] | 18.77 [18.36, 19.18] | 19.41 [19.06, 19.76] | 19.79 [19.47, 20.12] |
| R2E holding mean queue | 117.62 [114.28, 120.97] | 99.68 [96.43, 102.92] | 76.81 [74.85, 78.77] | 54.06 [52.01, 56.12] |
| R2E staging overflow census | 1072.12 [1023.71, 1120.53] | 1216.10 [1163.58, 1268.62] | 1644.88 [1593.76, 1696.01] | 2515.75 [2474.07, 2557.42] |
| R2E ICU occupancy (%) | 96.6 [96.4, 96.7] | 97.1 [96.9, 97.2] | 97.3 [97.2, 97.4] | 97.5 [97.4, 97.7] |
| Returns to duty | 4700.1 [4633.5, 4766.7] | 4146.1 [4107.6, 4184.6] | 3603.4 [3561.4, 3645.5] | 2861.9 [2824.1, 2899.7] |
| Strategic evacuation decisions | 5559.7 [5477.1, 5642.2] | 6036.8 [5983.6, 6090.0] | 6533.1 [6465.7, 6600.6] | 6927.6 [6858.9, 6996.2] |
| Critical airlift backlog at the close | 26.9 [22.2, 31.6] | 808.0 [773.1, 842.9] | 1863.3 [1814.8, 1911.8] | 3022.2 [2974.9, 3069.4] |
| Role 4 peak ICU census | 66.3 [65.4, 67.3] | 67.4 [66.7, 68.2] | 67.6 [66.8, 68.4] | 67.6 [66.9, 68.3] |
<!-- /GEN -->

<!-- GEN triage_split_paired_high -->
| Paired difference against the shipped split | P1 15% | P1 30% | P1 45% |
|---|---|---|---|
| Died of wounds | −401.03 [−469.36, −332.70] | −337.37 [−406.14, −268.59] | −206.37 [−270.40, −142.33] |
| Died of wounds per wounded in action (%) | −4.82 [−5.53, −4.10] | −4.05 [−4.76, −3.33] | −2.52 [−3.21, −1.84] |
| Diverted from R2B, team off shift | −367.9 [−434.8, −301.0] | −265.1 [−328.4, −201.9] | −98.8 [−159.5, −38.1] |
| Diverted from R2B, theatre busy | −151.4 [−200.6, −102.2] | −89.8 [−132.7, −46.8] | −36.9 [−80.0, +6.3] |
| Time to first incision (hours) | −42.92 [−55.95, −29.90] | −37.63 [−51.11, −24.14] | −23.90 [−35.72, −12.09] |
| Casualties requiring surgery operated on (%) | +0.8 [+0.5, +1.0] | +0.6 [+0.2, +1.0]† | +0.5 [+0.1, +0.8] |
| R2E theatre mean queue | −57.30 [−78.90, −35.70] | −48.12 [−71.01, −25.24] | −23.80 [−48.29, +0.70] |
| R2E post-operative overflow census | −2.30 [−2.81, −1.79] | −1.02 [−1.47, −0.57] | −0.38 [−0.81, +0.05] |
| R2E holding mean queue | +63.56 [+59.80, +67.32] | +45.61 [+41.52, +49.70] | +22.75 [+19.86, +25.63] |
| R2E staging overflow census | −1443.63 [−1497.81, −1389.44] | −1299.64 [−1371.52, −1227.77] | −870.86 [−944.30, −797.42] |
| R2E ICU occupancy (%) | −1.0 [−1.2, −0.7] | −0.4 [−0.6, −0.3] | −0.2 [−0.4, −0.1] |
| Returns to duty | +1838.2 [+1771.7, +1904.7] | +1284.2 [+1239.5, +1328.9] | +741.5 [+691.1, +792.0] |
| Strategic evacuation decisions | −1367.9 [−1455.1, −1280.7] | −890.7 [−982.6, −798.9] | −394.4 [−496.4, −292.4] |
| Critical airlift backlog at the close | −2995.2 [−3042.1, −2948.3] | −2214.2 [−2272.0, −2156.3] | −1158.9 [−1231.9, −1085.9] |
| Role 4 peak ICU census | −1.2 [−2.4, −0.1]† | −0.1 [−1.2, +0.9] | +0.0 [−0.9, +1.0] |
<!-- /GEN -->

At the shipped share the high-intensity treated-cohort died-of-wounds rate was <!-- GEN cell:triage_split_high|Treated-cohort died-of-wounds rate|P1 65% (shipped)|full -->5.696% [5.080%, 6.311%]<!-- /GEN --> and at fifteen per cent it was <!-- GEN cell:triage_split_high|Treated-cohort died-of-wounds rate|P1 15%|full -->1.635% [1.497%, 1.773%]<!-- /GEN -->. The R2E theatre queue was classified <!-- GEN cell:triage_split_high|R2E theatre queue, late-half classification|P1 65% (shipped)|full -->converged at 71.0<!-- /GEN --> at the shipped share and <!-- GEN cell:triage_split_high|R2E theatre queue, late-half classification|P1 15%|full -->converged at 16.0<!-- /GEN --> at fifteen per cent, the post-operative overflow <!-- GEN cell:triage_split_high|R2E post-operative overflow, late-half classification|P1 65% (shipped)|full -->converged at 19.8<!-- /GEN --> and <!-- GEN cell:triage_split_high|R2E post-operative overflow, late-half classification|P1 15%|full -->converged at 17.6<!-- /GEN -->, and the staging overflow <!-- GEN cell:triage_split_high|R2E staging overflow, late-half classification|P1 65% (shipped)|full -->**drifting, +11.2%/block**, 117.7 to 2,739.6<!-- /GEN --> and <!-- GEN cell:triage_split_high|R2E staging overflow, late-half classification|P1 15%|full -->**drifting, +11.6%/block**, 68.2 to 1,163.5<!-- /GEN -->. Lowering the share from sixty-five per cent to fifteen per cent changed the R2E theatre queue by <!-- GEN cell:triage_split_paired_high|R2E theatre mean queue|P1 15%|full -->−57.30 [−78.90, −35.70]<!-- /GEN -->, the critical airlift backlog at the close by <!-- GEN cell:triage_split_paired_high|Critical airlift backlog at the close|P1 15%|full -->−2995.2 [−3042.1, −2948.3]<!-- /GEN -->, the R2E holding queue by <!-- GEN cell:triage_split_paired_high|R2E holding mean queue|P1 15%|full -->+63.56 [+59.80, +67.32]<!-- /GEN -->, deaths of wounds by <!-- GEN cell:triage_split_paired_high|Died of wounds|P1 15%|full -->−401.03 [−469.36, −332.70]<!-- /GEN --> and returns to duty by <!-- GEN cell:triage_split_paired_high|Returns to duty|P1 15%|full -->+1838.2 [+1771.7, +1904.7]<!-- /GEN -->.

---

## National Support Base and Strategic Airlift

<small>[Return to Top](#contents)</small>

**Question.** How does strategic evacuation demand and its wait respond to the shipped airlift schedule, to the interval between sorties and to sortie cancellation, and what census does the national support base carry? `[default, both intensities and swept values · 360 d · 30 replications · per-replication reductions]`

<!-- GEN airlift_baseline -->
| Response at the shipped schedule | Moderate intensity | High intensity |
|---|---|---|
| Casualties boarded | 2316.93 [2272.58, 2361.28] | 3886.50 [3861.47, 3911.53] |
| Still waiting at the close | 36.27 [26.95, 45.58] | 3041.00 [2992.95, 3089.05] |
| Mean wait (days) | 4.90 [4.55, 5.24] | 53.69 [53.15, 54.22] |
| Share of R2E holding beds held by the evacuation wait | 10% [10%, 11%] | 8% [7%, 10%] |
| Role 4 peak occupancy (concurrent patients) | 209.23 [205.24, 213.23] | 281.30 [278.98, 283.62] |
| Days the peak falls before the campaign ends | 151.57 [112.10, 191.03] | 164.87 [130.57, 199.17] |
<!-- /GEN -->

At the shipped schedule <!-- GEN cell:airlift_baseline|Casualties boarded|Moderate intensity|mean -->2316.93<!-- /GEN --> casualties boarded at moderate intensity and <!-- GEN cell:airlift_baseline|Casualties boarded|High intensity|mean -->3886.50<!-- /GEN --> at high, with <!-- GEN cell:airlift_baseline|Still waiting at the close|Moderate intensity|mean -->36.27<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Still waiting at the close|High intensity|mean -->3041.00<!-- /GEN --> still waiting at the close and mean waits of <!-- GEN cell:airlift_baseline|Mean wait (days)|Moderate intensity|mean -->4.90<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Mean wait (days)|High intensity|mean -->53.69<!-- /GEN --> days. The evacuation wait held <!-- GEN cell:airlift_baseline|Share of R2E holding beds held by the evacuation wait|Moderate intensity|full -->10% [10%, 11%]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Share of R2E holding beds held by the evacuation wait|High intensity|full -->8% [7%, 10%]<!-- /GEN --> of the R2E holding beds. Role 4 peak occupancy was <!-- GEN cell:airlift_baseline|Role 4 peak occupancy (concurrent patients)|Moderate intensity|full -->209.23 [205.24, 213.23]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Role 4 peak occupancy (concurrent patients)|High intensity|full -->281.30 [278.98, 283.62]<!-- /GEN --> concurrent patients, and the peak fell <!-- GEN cell:airlift_baseline|Days the peak falls before the campaign ends|Moderate intensity|full -->151.57 [112.10, 191.03]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Days the peak falls before the campaign ends|High intensity|full -->164.87 [130.57, 199.17]<!-- /GEN --> days before the campaign ended.

<!-- GEN airlift_interval -->
| Response by interval between sorties | 3 days | 5 days | 7 days (shipped) | 10 days | 14 days |
|---|---|---|---|---|---|
| Sorties flown | 119.00 | 71.00 | 51.00 | 35.00 | 25.00 |
| Mean wait (days) | 1.66 [1.65, 1.67] | 2.89 [2.77, 3.01] | 4.90 [4.55, 5.24] | 20.35 [18.09, 22.61] | 41.63 [39.84, 43.43] |
| Share of R2E holding beds held by the evacuation wait | 6% [6%, 6%] | 8% [8%, 9%] | 10% [10%, 11%] | 22% [20%, 23%] | 34% [32%, 35%] |
| Ventilated pre-flight intensive care hold (hours) | 23.99 [23.85, 24.13] | 24.07 [23.91, 24.23] | 24.08 [23.90, 24.26] | 24.01 [23.86, 24.16] | 23.93 [23.76, 24.10] |
<!-- /GEN -->

Shortening the interval between sorties below the shipped seven days left the mean wait at <!-- GEN cell:airlift_interval|Mean wait (days)|3 days|mean -->1.66<!-- /GEN --> days at three days and <!-- GEN cell:airlift_interval|Mean wait (days)|5 days|mean -->2.89<!-- /GEN --> at five against <!-- GEN cell:airlift_interval|Mean wait (days)|7 days (shipped)|mean -->4.90<!-- /GEN --> at seven. Lengthening it gave <!-- GEN cell:airlift_interval|Mean wait (days)|10 days|full -->20.35 [18.09, 22.61]<!-- /GEN --> days at ten days with <!-- GEN cell:airlift_interval|Sorties flown|10 days|full -->35.00<!-- /GEN --> sorties flown and <!-- GEN cell:airlift_interval|Mean wait (days)|14 days|full -->41.63 [39.84, 43.43]<!-- /GEN --> days at fourteen with <!-- GEN cell:airlift_interval|Sorties flown|14 days|full -->25.00<!-- /GEN -->.

<!-- GEN airlift_reliability -->
| Response by configured cancellation probability | 0% | 5% | 10% | 15% | 25% | 40% |
|---|---|---|---|---|---|---|
| Sorties flown | 51.00 | 48.77 | 45.97 | 43.13 | 39.10 | 31.50 |
| Realised cancellation rate | 0% | 4% | 10% | 15% | 23% | 38% |
| Mean wait (days) | 4.90 [4.55, 5.24] | 7.01 [5.87, 8.15] | 8.35 [7.15, 9.56] | 11.24 [9.66, 12.82] | 18.74 [15.48, 22.00] | 31.96 [27.54, 36.38] |
| Share of R2E holding beds held by the evacuation wait | 10% [10%, 11%] | 12% [11%, 13%] | 13% [12%, 14%] | 16% [15%, 18%] | 21% [19%, 23%] | 28% [25%, 30%] |
<!-- /GEN -->

![Eight panels of strategic evacuation responses against sortie cancellation probability and the interval between sorties](../images/airlift_sweeps.png)

Wait for a sortie, casualties still waiting at the horizon, Role 4 peak beds and the share of holding bed-days spent awaiting a sortie against sortie cancellation probability and the interval between sorties, with the shipped value of each marked by a dashed line.

The mean wait rose from <!-- GEN cell:airlift_reliability|Mean wait (days)|0%|mean -->4.90<!-- /GEN --> days with no cancellation to <!-- GEN cell:airlift_reliability|Mean wait (days)|10%|mean -->8.35<!-- /GEN --> at 10%, <!-- GEN cell:airlift_reliability|Mean wait (days)|25%|mean -->18.74<!-- /GEN --> at 25% and <!-- GEN cell:airlift_reliability|Mean wait (days)|40%|mean -->31.96<!-- /GEN --> at 40%, as the sorties flown fell from <!-- GEN cell:airlift_reliability|Sorties flown|0%|full -->51.00<!-- /GEN --> to <!-- GEN cell:airlift_reliability|Sorties flown|40%|full -->31.50<!-- /GEN -->.

**High-intensity profile.** The same two sweeps under `high_intensity` `[high_intensity, interval 3 to 14 days and cancellation 0 to 40% · 360 d · 30 replications · campaign means]`:

<!-- GEN airlift_interval_high -->
| Response by interval between sorties | 3 days | 5 days | 7 days (shipped) | 10 days | 14 days |
|---|---|---|---|---|---|
| Sorties flown | 119.00 | 71.00 | 51.00 | 35.00 | 25.00 |
| Mean wait (days) | 15.55 [14.49, 16.60] | 46.54 [45.75, 47.33] | 53.69 [53.15, 54.22] | 63.31 [62.04, 64.59] | 96.89 [95.72, 98.06] |
| Share of R2E holding beds held by the evacuation wait | 2% [2%, 3%] | 6% [5%, 6%] | 8% [7%, 10%] | 9% [8%, 11%] | 12% [11%, 14%] |
| Ventilated pre-flight intensive care hold (hours) | 23.97 [23.88, 24.07] | 23.97 [23.88, 24.07] | 24.02 [23.94, 24.11] | 23.99 [23.91, 24.07] | 23.94 [23.86, 24.02] |
<!-- /GEN -->

At the shipped seven-day interval the mean wait was <!-- GEN cell:airlift_interval_high|Mean wait (days)|7 days (shipped)|mean -->53.69<!-- /GEN --> days. Shortening the interval to five days gave <!-- GEN cell:airlift_interval_high|Mean wait (days)|5 days|mean -->46.54<!-- /GEN --> days and to three days <!-- GEN cell:airlift_interval_high|Mean wait (days)|3 days|mean -->15.55<!-- /GEN --> days with <!-- GEN cell:airlift_interval_high|Sorties flown|3 days|mean -->119.00<!-- /GEN --> sorties flown, while lengthening it to ten and fourteen days gave <!-- GEN cell:airlift_interval_high|Mean wait (days)|10 days|mean -->63.31<!-- /GEN --> and <!-- GEN cell:airlift_interval_high|Mean wait (days)|14 days|mean -->96.89<!-- /GEN --> days.

<!-- GEN airlift_reliability_high -->
| Response by configured cancellation probability | 0% | 5% | 10% | 15% | 25% | 40% |
|---|---|---|---|---|---|---|
| Sorties flown | 51.00 | 48.43 | 45.77 | 43.30 | 38.07 | 30.57 |
| Realised cancellation rate | 0% | 5% | 10% | 15% | 25% | 40% |
| Mean wait (days) | 53.69 [53.15, 54.22] | 54.14 [53.45, 54.84] | 54.86 [54.02, 55.70] | 56.58 [55.01, 58.15] | 62.88 [59.55, 66.22] | 80.98 [76.80, 85.16] |
| Share of R2E holding beds held by the evacuation wait | 8% [7%, 10%] | 9% [8%, 10%] | 9% [8%, 10%] | 9% [8%, 10%] | 10% [9%, 12%] | 11% [9%, 13%] |
<!-- /GEN -->

The mean wait rose from <!-- GEN cell:airlift_reliability_high|Mean wait (days)|0%|mean -->53.69<!-- /GEN --> days with no cancellation to <!-- GEN cell:airlift_reliability_high|Mean wait (days)|10%|mean -->54.86<!-- /GEN --> at 10%, <!-- GEN cell:airlift_reliability_high|Mean wait (days)|25%|mean -->62.88<!-- /GEN --> at 25% and <!-- GEN cell:airlift_reliability_high|Mean wait (days)|40%|mean -->80.98<!-- /GEN --> at 40%, as the sorties flown fell from <!-- GEN cell:airlift_reliability_high|Sorties flown|0%|mean -->51.00<!-- /GEN --> to <!-- GEN cell:airlift_reliability_high|Sorties flown|40%|mean -->30.57<!-- /GEN -->.

![Eight panels of strategic evacuation responses against sortie cancellation probability and the interval between sorties at high intensity](../images/airlift_sweeps_high_intensity.png)

The panels of the figure above under the high-intensity profile, with the shipped value of each sweep marked by a dashed line.

**Collapse classification.** Each airlift route was classified separately, the critical and standard seat pools filling independently. A route was classified as collapsed in a campaign where the backlog of casualties awaiting a place on it was still rising over the late half of the campaign, by the late-half trend rule of [Classifying Stability](Methods.md#classifying-stability), and its late-half mean exceeded the places one sortie opens on the route; a casualty left the backlog on boarding or on dying. Utilisation is the route's demand over the campaign divided by the places its flown sorties opened. `[default, sortie cancellation 0 to 25% · 360 d · 30 replications · exact binomial]`

<!-- GEN airlift_collapse -->
| Route and sortie cancellation | Collapsed | Collapse rate (exact 95% interval) | Mean utilisation | Demand at or above places flown | Median closing backlog | Worst closing backlog |
|---|---|---|---|---|---|---|
| Critical, 0% | 1 of 30 | 3.3% [0.1%, 17.2%] | 0.83 | 0 of 30 | 21.3 | 71.8 |
| Critical, 5% | 1 of 30 | 3.3% [0.1%, 17.2%] | 0.89 | 2 of 30 | 22.5 | 169.8 |
| Critical, 10% | 2 of 30 | 6.7% [0.8%, 22.1%] | 0.92 | 2 of 30 | 27.9 | 217.9 |
| Critical, 15% | 7 of 30 | 23.3% [9.9%, 42.3%] | 0.99 | 11 of 30 | 76.2 | 384.3 |
| Critical, 20% | 13 of 30 | 43.3% [25.5%, 62.6%] | 1.07 | 22 of 30 | 126.8 | 379.2 |
| Critical, 25% | 12 of 30 | 40.0% [22.7%, 59.4%] | 1.09 | 24 of 30 | 151.2 | 379.1 |
| Standard, 0% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.30 | 0 of 30 | 7.2 | 8.2 |
| Standard, 5% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.32 | 0 of 30 | 7.4 | 9.5 |
| Standard, 10% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.33 | 0 of 30 | 8.0 | 13.2 |
| Standard, 15% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.35 | 0 of 30 | 9.8 | 16.7 |
| Standard, 20% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.39 | 0 of 30 | 11.0 | 33.7 |
| Standard, 25% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.39 | 0 of 30 | 10.0 | 20.6 |
<!-- /GEN -->

![Line plot of the share of campaigns collapsing against sortie cancellation probability](../images/airlift_collapse.png)

The share of campaigns in which each route was classified as collapsed against sortie cancellation probability, with exact binomial 95% confidence intervals.

The critical route collapsed in <!-- GEN cell:airlift_collapse|Critical, 0%|Collapsed|full -->1 of 30<!-- /GEN --> campaigns with no cancellation, <!-- GEN cell:airlift_collapse|Critical, 15%|Collapsed|full -->7 of 30<!-- /GEN --> at 15% and <!-- GEN cell:airlift_collapse|Critical, 25%|Collapsed|full -->12 of 30<!-- /GEN --> at 25%, its mean utilisation rising from <!-- GEN cell:airlift_collapse|Critical, 0%|Mean utilisation|full -->0.83<!-- /GEN --> to <!-- GEN cell:airlift_collapse|Critical, 25%|Mean utilisation|full -->1.09<!-- /GEN -->. Demand met or exceeded the critical places flown in <!-- GEN cell:airlift_collapse|Critical, 15%|Demand at or above places flown|full -->11 of 30<!-- /GEN --> campaigns at 15% cancellation. The standard route collapsed in <!-- GEN cell:airlift_collapse|Standard, 25%|Collapsed|full -->0 of 30<!-- /GEN --> campaigns at 25%, at a mean utilisation of <!-- GEN cell:airlift_collapse|Standard, 25%|Mean utilisation|full -->0.39<!-- /GEN -->.

**High-intensity profile.** The same classification under `high_intensity` `[high_intensity, sortie cancellation 0 to 25% · 360 d · 30 replications per arm · exact binomial interval]`:

<!-- GEN airlift_collapse_high -->
| Route and sortie cancellation | Collapsed | Collapse rate (exact 95% interval) | Mean utilisation | Demand at or above places flown | Median closing backlog | Worst closing backlog |
|---|---|---|---|---|---|---|
| Critical, 0% | 30 of 30 | 100.0% [88.4%, 100.0%] | 2.65 | 30 of 30 | 2,502.1 | 2,668.9 |
| Critical, 5% | 30 of 30 | 100.0% [88.4%, 100.0%] | 2.76 | 30 of 30 | 2,585.8 | 2,749.2 |
| Critical, 10% | 30 of 30 | 100.0% [88.4%, 100.0%] | 2.95 | 30 of 30 | 2,648.6 | 2,956.6 |
| Critical, 15% | 30 of 30 | 100.0% [88.4%, 100.0%] | 3.12 | 30 of 30 | 2,733.7 | 3,036.4 |
| Critical, 20% | 30 of 30 | 100.0% [88.4%, 100.0%] | 3.30 | 30 of 30 | 2,780.4 | 3,033.4 |
| Critical, 25% | 30 of 30 | 100.0% [88.4%, 100.0%] | 3.56 | 30 of 30 | 2,854.7 | 3,330.8 |
| Standard, 0% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.75 | 0 of 30 | 17.6 | 22.1 |
| Standard, 5% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.79 | 0 of 30 | 19.3 | 38.1 |
| Standard, 10% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.85 | 0 of 30 | 27.8 | 75.2 |
| Standard, 15% | 1 of 30 | 3.3% [0.1%, 17.2%] | 0.90 | 2 of 30 | 37.4 | 151.8 |
| Standard, 20% | 6 of 30 | 20.0% [7.7%, 38.6%] | 0.96 | 6 of 30 | 80.5 | 227.7 |
| Standard, 25% | 12 of 30 | 40.0% [22.7%, 59.4%] | 1.02 | 15 of 30 | 111.5 | 424.7 |
<!-- /GEN -->

Under `high_intensity` the critical route collapsed in <!-- GEN cell:airlift_collapse_high|Critical, 0%|Collapsed|full -->30 of 30<!-- /GEN --> campaigns with no cancellation, at a mean utilisation of <!-- GEN cell:airlift_collapse_high|Critical, 0%|Mean utilisation|full -->2.65<!-- /GEN -->, and the standard route in <!-- GEN cell:airlift_collapse_high|Standard, 0%|Collapsed|full -->0 of 30<!-- /GEN -->, at <!-- GEN cell:airlift_collapse_high|Standard, 0%|Mean utilisation|full -->0.75<!-- /GEN -->.

---

## Critical-Care Places per Sortie

<small>[Return to Top](#contents)</small>

**Question.** What do more critical-care places per sortie buy, with the airframe's total held at 90, and how does reconfiguring the aircraft compare with flying it more often? `[default and high_intensity, 36 to 72 critical places of 90 crossed with a 7-day and a 3-day interval · 360 d · 30 replications · paired, closing 90 d]`

Each arm splits the C-17A's 90 places between the critical-care and standard pools and flies at the shipped weekly interval or every three days ([Methods](Methods.md#critical-care-places-per-sortie-sweep)). A route's collapse row counts the campaigns whose backlog on that route was still rising over the second half of the year and exceeded the places that arm's sorties open on it, with the exact binomial interval on the rate. Every arm is compared with the shipped split at the weekly interval, and Holm's adjustment is taken across all of those comparisons.

<!-- GEN airlift_places -->
| Response | 36/54, 7 days (shipped) | 45/45, 7 days | 54/36, 7 days | 72/18, 7 days | 36/54, 3 days | 45/45, 3 days | 54/36, 3 days | 72/18, 3 days |
|---|---|---|---|---|---|---|---|---|
| Critical airlift closing backlog | 25.5 [20.3, 30.6] | 16.3 [13.9, 18.7] | 15.7 [12.1, 19.3] | 13.9 [13.1, 14.8] | 5.3 [5.1, 5.6] | 5.4 [5.0, 5.7] | 5.2 [4.9, 5.5] | 5.4 [5.0, 5.7] |
| Critical airlift campaigns collapsed | 1 of 30 [0%, 17%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] |
| Standard airlift closing backlog | 7.0 [6.7, 7.2] | 7.1 [6.7, 7.5] | 6.7 [6.3, 7.0] | 17.7 [13.1, 22.3] | 2.3 [2.2, 2.4] | 2.4 [2.3, 2.5] | 2.4 [2.3, 2.5] | 2.4 [2.3, 2.5] |
| Standard airlift campaigns collapsed | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 1 of 30 [0%, 17%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] |
| Critical airlift mean wait (days) | 5.65 [5.12, 6.18] | 4.22 [4.01, 4.44] | 4.04 [3.77, 4.30] | 3.71 [3.68, 3.75] | 1.75 [1.73, 1.77] | 1.73 [1.71, 1.74] | 1.72 [1.70, 1.73] | 1.72 [1.70, 1.73] |
| Standard airlift mean wait (days) | 3.50 [3.47, 3.53] | 3.50 [3.47, 3.54] | 3.54 [3.49, 3.58] | 7.30 [6.15, 8.44] | 1.50 [1.49, 1.51] | 1.50 [1.49, 1.51] | 1.50 [1.49, 1.51] | 1.52 [1.50, 1.53] |
| Critical places departing empty per sortie | 6.6 [6.0, 7.2] | 15.9 [15.2, 16.6] | 24.5 [23.6, 25.4] | 42.1 [41.2, 43.0] | 23.5 [23.1, 23.8] | 32.3 [32.0, 32.6] | 41.5 [41.1, 41.8] | 59.4 [59.1, 59.8] |
| Standard places departing empty per sortie | 38.0 [37.6, 38.4] | 29.1 [28.8, 29.5] | 20.1 [19.7, 20.4] | 2.0 [1.7, 2.4] | 47.3 [47.1, 47.5] | 38.2 [38.1, 38.4] | 29.3 [29.1, 29.5] | 11.2 [11.0, 11.4] |
| R2E ICU occupancy (%) | 86.8 [86.0, 87.6] | 86.3 [85.5, 87.2] | 86.3 [85.3, 87.2] | 86.6 [85.9, 87.2] | 86.3 [85.6, 87.0] | 86.1 [85.4, 86.9] | 86.0 [85.2, 86.8] | 86.3 [85.4, 87.3] |
| R2E holding occupancy (%) | 99.6 [99.4, 99.7] | 99.4 [99.2, 99.6] | 99.4 [99.3, 99.5] | 99.5 [99.4, 99.5] | 99.0 [98.8, 99.2] | 99.0 [98.8, 99.2] | 99.0 [98.8, 99.2] | 99.0 [98.7, 99.2] |
| Died of wounds awaiting airlift | 3.80 [2.84, 4.76] | 2.03 [1.52, 2.55] | 2.90 [2.37, 3.43] | 2.30 [1.73, 2.87] | 1.07 [0.66, 1.47] | 0.93 [0.54, 1.32] | 0.93 [0.58, 1.29] | 0.93 [0.57, 1.30] |
| Died of wounds | 21.17 [17.55, 24.79] | 20.97 [18.29, 23.65] | 22.47 [15.87, 29.07] | 20.07 [17.04, 23.09] | 20.03 [17.42, 22.65] | 19.47 [17.05, 21.88] | 18.43 [16.20, 20.66] | 17.37 [15.44, 19.29] |
| Role 4 peak census | 209.2 [205.2, 213.2] | 226.8 [220.0, 233.6] | 236.4 [224.4, 248.4] | 236.5 [225.9, 247.2] | 236.8 [222.8, 250.8] | 239.7 [225.9, 253.4] | 236.3 [222.6, 249.9] | 231.1 [221.1, 241.1] |
<!-- /GEN -->

<!-- GEN airlift_places_paired -->
| Paired difference against the shipped split | 45/45, 7 days | 54/36, 7 days | 72/18, 7 days | 36/54, 3 days | 45/45, 3 days | 54/36, 3 days | 72/18, 3 days |
|---|---|---|---|---|---|---|---|
| Critical airlift closing backlog | −9.2 [−14.8, −3.5]† | −9.7 [−14.1, −5.4] | −11.5 [−16.5, −6.5] | −20.1 [−25.3, −14.9] | −20.1 [−25.3, −14.9] | −20.3 [−25.4, −15.1] | −20.1 [−25.3, −14.9] |
| Standard airlift closing backlog | +0.2 [−0.3, +0.6] | −0.3 [−0.8, +0.2] | +10.7 [+6.2, +15.3] | −4.7 [−4.9, −4.4] | −4.6 [−4.8, −4.3] | −4.6 [−4.8, −4.4] | −4.6 [−4.8, −4.3] |
| Critical airlift mean wait (days) | −1.43 [−2.00, −0.86] | −1.62 [−2.15, −1.09] | −1.94 [−2.47, −1.41] | −3.90 [−4.44, −3.36] | −3.93 [−4.46, −3.39] | −3.93 [−4.47, −3.40] | −3.93 [−4.47, −3.40] |
| Standard airlift mean wait (days) | +0.00 [−0.04, +0.05] | +0.04 [−0.02, +0.09] | +3.79 [+2.64, +4.95] | −2.00 [−2.03, −1.97] | −2.00 [−2.03, −1.97] | −2.00 [−2.03, −1.97] | −1.99 [−2.02, −1.95] |
| Critical places departing empty per sortie | +9.4 [+8.6, +10.1] | +17.9 [+16.9, +19.0] | +35.5 [+34.7, +36.4] | +16.9 [+16.3, +17.6] | +25.7 [+25.1, +26.4] | +34.9 [+34.2, +35.6] | +52.9 [+52.2, +53.6] |
| Standard places departing empty per sortie | −8.9 [−9.3, −8.4] | −17.9 [−18.4, −17.4] | −36.0 [−36.4, −35.5] | +9.3 [+8.9, +9.7] | +0.2 [−0.1, +0.6] | −8.7 [−9.1, −8.4] | −26.8 [−27.2, −26.3] |
| R2E ICU occupancy (%) | −0.5 [−1.6, +0.6] | −0.5 [−1.9, +0.8] | −0.2 [−1.4, +0.9] | −0.5 [−1.6, +0.6] | −0.7 [−1.7, +0.4] | −0.8 [−2.0, +0.4] | −0.5 [−1.9, +0.9] |
| R2E holding occupancy (%) | −0.2 [−0.3, +0.0] | −0.1 [−0.3, +0.1] | −0.1 [−0.2, +0.0] | −0.5 [−0.8, −0.3] | −0.6 [−0.8, −0.4] | −0.5 [−0.8, −0.3] | −0.6 [−0.8, −0.4] |
| Died of wounds awaiting airlift | −1.77 [−2.75, −0.79] | −0.90 [−2.06, +0.26] | −1.50 [−2.52, −0.48]† | −2.73 [−3.73, −1.74] | −2.87 [−3.91, −1.83] | −2.87 [−3.89, −1.84] | −2.87 [−3.91, −1.82] |
| Died of wounds | −0.20 [−4.52, +4.12] | +1.30 [−6.24, +8.84] | −1.10 [−5.65, +3.45] | −1.13 [−5.30, +3.04] | −1.70 [−6.35, +2.95] | −2.73 [−7.19, +1.72] | −3.80 [−8.02, +0.42] |
| Role 4 peak census | +17.5 [+8.9, +26.2] | +27.2 [+15.4, +39.0] | +27.3 [+15.8, +38.8] | +27.6 [+12.7, +42.4] | +30.4 [+16.0, +44.9] | +27.0 [+12.8, +41.3] | +21.8 [+10.7, +33.0] |
<!-- /GEN -->

At the shipped split and interval the critical airlift's closing backlog was <!-- GEN cell:airlift_places|Critical airlift closing backlog|36/54, 7 days (shipped)|full -->25.5 [20.3, 30.6]<!-- /GEN -->, with <!-- GEN cell:airlift_places|Critical airlift campaigns collapsed|36/54, 7 days (shipped)|full -->1 of 30 [0%, 17%]<!-- /GEN --> campaigns collapsed on the critical route. At the weekly interval, 72 critical places of 90 changed the critical backlog by <!-- GEN cell:airlift_places_paired|Critical airlift closing backlog|72/18, 7 days|full -->−11.5 [−16.5, −6.5]<!-- /GEN --> and the standard backlog by <!-- GEN cell:airlift_places_paired|Standard airlift closing backlog|72/18, 7 days|full -->+10.7 [+6.2, +15.3]<!-- /GEN -->, with <!-- GEN cell:airlift_places|Standard airlift campaigns collapsed|72/18, 7 days|full -->1 of 30 [0%, 17%]<!-- /GEN --> campaigns collapsed on the standard route. Flying every three days at the shipped split changed the critical backlog by <!-- GEN cell:airlift_places_paired|Critical airlift closing backlog|36/54, 3 days|full -->−20.1 [−25.3, −14.9]<!-- /GEN -->, with <!-- GEN cell:airlift_places|Critical airlift campaigns collapsed|36/54, 3 days|full -->0 of 30 [0%, 12%]<!-- /GEN --> campaigns collapsed on the critical route, and at 45 critical places by <!-- GEN cell:airlift_places_paired|Critical airlift closing backlog|45/45, 3 days|full -->−20.1 [−25.3, −14.9]<!-- /GEN -->, with <!-- GEN cell:airlift_places|Critical airlift campaigns collapsed|45/45, 3 days|full -->0 of 30 [0%, 12%]<!-- /GEN -->. Deaths of wounds among casualties awaiting airlift changed by <!-- GEN cell:airlift_places_paired|Died of wounds awaiting airlift|45/45, 3 days|full -->−2.87 [−3.91, −1.83]<!-- /GEN --> at 45 places every three days, deaths of wounds in total by <!-- GEN cell:airlift_places_paired|Died of wounds|45/45, 3 days|full -->−1.70 [−6.35, +2.95]<!-- /GEN -->, and the Role 4 peak census by <!-- GEN cell:airlift_places_paired|Role 4 peak census|45/45, 3 days|full -->+30.4 [+16.0, +44.9]<!-- /GEN -->.

At high intensity the same eight arms were run under the `high_intensity` profile (30 replications, 360 days, seed 42).

<!-- GEN airlift_places_high -->
| Response | 36/54, 7 days (shipped) | 45/45, 7 days | 54/36, 7 days | 72/18, 7 days | 36/54, 3 days | 45/45, 3 days | 54/36, 3 days | 72/18, 3 days |
|---|---|---|---|---|---|---|---|---|
| Critical airlift closing backlog | 2506.7 [2465.2, 2548.2] | 2144.3 [2107.7, 2180.9] | 1771.3 [1730.7, 1811.9] | 1004.8 [964.0, 1045.6] | 493.6 [454.6, 532.5] | 23.0 [21.9, 24.0] | 20.4 [20.0, 20.9] | 20.5 [20.0, 20.9] |
| Critical airlift campaigns collapsed | 30 of 30 [88%, 100%] | 30 of 30 [88%, 100%] | 30 of 30 [88%, 100%] | 30 of 30 [88%, 100%] | 29 of 30 [83%, 100%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] |
| Standard airlift closing backlog | 17.6 [17.0, 18.3] | 22.6 [20.5, 24.7] | 231.9 [213.3, 250.4] | 997.6 [980.4, 1014.8] | 6.1 [5.9, 6.2] | 6.1 [5.9, 6.3] | 6.0 [5.8, 6.1] | 25.7 [18.5, 32.9] |
| Standard airlift campaigns collapsed | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 30 of 30 [88%, 100%] | 30 of 30 [88%, 100%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 0 of 30 [0%, 12%] | 3 of 30 [2%, 27%] |
| Critical airlift mean wait (days) | 109.67 [108.40, 110.95] | 94.05 [93.06, 95.04] | 77.32 [75.95, 78.70] | 44.47 [42.80, 46.15] | 22.44 [20.84, 24.04] | 2.17 [2.14, 2.19] | 2.00 [1.99, 2.00] | 2.00 [1.99, 2.00] |
| Standard airlift mean wait (days) | 3.56 [3.53, 3.58] | 4.21 [4.08, 4.35] | 24.49 [22.86, 26.11] | 100.15 [98.71, 101.59] | 1.51 [1.50, 1.52] | 1.51 [1.50, 1.51] | 1.50 [1.50, 1.51] | 4.52 [3.73, 5.32] |
| Critical places departing empty per sortie | 0.0 [0.0, 0.0] | 0.0 [0.0, 0.0] | 0.1 [0.0, 0.1] | 0.3 [0.2, 0.4] | 0.3 [0.2, 0.4] | 4.7 [4.3, 5.2] | 13.9 [13.4, 14.3] | 31.8 [31.4, 32.2] |
| Standard places departing empty per sortie | 13.8 [13.3, 14.3] | 4.3 [3.8, 4.7] | 0.2 [0.1, 0.3] | 0.0 [0.0, 0.0] | 36.5 [36.3, 36.7] | 27.4 [27.2, 27.7] | 18.6 [18.4, 18.8] | 0.8 [0.6, 1.0] |
| R2E ICU occupancy (%) | 97.5 [97.4, 97.7] | 97.5 [97.4, 97.6] | 97.5 [97.4, 97.6] | 97.5 [97.4, 97.6] | 97.5 [97.4, 97.7] | 97.5 [97.5, 97.6] | 97.5 [97.4, 97.6] | 97.5 [97.4, 97.6] |
| R2E holding occupancy (%) | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] |
| Died of wounds awaiting airlift | 182.37 [176.57, 188.16] | 160.67 [156.33, 165.01] | 141.63 [136.09, 147.18] | 105.83 [99.63, 112.04] | 41.50 [37.90, 45.10] | 3.73 [3.03, 4.44] | 3.43 [2.64, 4.22] | 5.23 [4.43, 6.04] |
| Died of wounds | 568.60 [500.55, 636.65] | 528.17 [475.27, 581.06] | 540.00 [477.87, 602.13] | 495.83 [419.76, 571.91] | 478.57 [399.45, 557.69] | 392.83 [305.46, 480.21] | 420.27 [324.09, 516.44] | 456.27 [375.43, 537.10] |
| Role 4 peak census | 281.3 [279.0, 283.6] | 315.0 [313.2, 316.7] | 336.6 [335.4, 337.8] | 376.4 [375.3, 377.5] | 465.6 [463.4, 467.7] | 533.9 [530.5, 537.4] | 536.1 [531.2, 541.0] | 528.5 [523.7, 533.3] |
<!-- /GEN -->

<!-- GEN airlift_places_paired_high -->
| Paired difference against the shipped split | 45/45, 7 days | 54/36, 7 days | 72/18, 7 days | 36/54, 3 days | 45/45, 3 days | 54/36, 3 days | 72/18, 3 days |
|---|---|---|---|---|---|---|---|
| Critical airlift closing backlog | −362.4 [−418.4, −306.4] | −735.4 [−788.3, −682.5] | −1501.9 [−1555.4, −1448.4] | −2013.2 [−2074.1, −1952.2] | −2483.7 [−2525.0, −2442.5] | −2486.3 [−2527.7, −2444.8] | −2486.2 [−2527.9, −2444.6] |
| Standard airlift closing backlog | +5.0 [+2.8, +7.2] | +214.2 [+195.5, +233.0] | +980.0 [+962.9, +997.1] | −11.5 [−12.2, −10.9] | −11.5 [−12.2, −10.8] | −11.7 [−12.3, −11.0] | +8.1 [+0.7, +15.5]† |
| Critical airlift mean wait (days) | −15.62 [−17.09, −14.15] | −32.35 [−33.78, −30.93] | −65.20 [−67.41, −62.98] | −87.23 [−89.48, −84.99] | −107.51 [−108.78, −106.23] | −107.68 [−108.95, −106.40] | −107.68 [−108.96, −106.40] |
| Standard airlift mean wait (days) | +0.66 [+0.52, +0.80] | +20.93 [+19.30, +22.56] | +96.59 [+95.16, +98.03] | −2.05 [−2.08, −2.02] | −2.05 [−2.08, −2.02] | −2.05 [−2.08, −2.03] | +0.97 [+0.18, +1.76]† |
| Critical places departing empty per sortie | +0.0 [+0.0, +0.0]† | +0.1 [+0.0, +0.1] | +0.3 [+0.2, +0.4] | +0.3 [+0.2, +0.4] | +4.7 [+4.3, +5.2] | +13.9 [+13.4, +14.3] | +31.8 [+31.4, +32.2] |
| Standard places departing empty per sortie | −9.5 [−10.2, −8.9] | −13.6 [−14.1, −13.1] | −13.8 [−14.3, −13.3] | +22.7 [+22.1, +23.3] | +13.7 [+13.1, +14.2] | +4.8 [+4.3, +5.4] | −13.0 [−13.6, −12.5] |
| R2E ICU occupancy (%) | +0.0 [−0.1, +0.2] | +0.0 [−0.2, +0.2] | +0.0 [−0.1, +0.2] | +0.0 [−0.1, +0.2] | +0.0 [−0.1, +0.2] | +0.0 [−0.1, +0.2] | −0.0 [−0.2, +0.1] |
| R2E holding occupancy (%) | −0.0 [−0.0, +0.0] | −0.0 [−0.0, −0.0]† | −0.0 [−0.0, +0.0] | −0.0 [−0.0, −0.0]† | −0.0 [−0.0, −0.0]† | −0.0 [−0.0, −0.0] | −0.0 [−0.0, −0.0]† |
| Died of wounds awaiting airlift | −21.70 [−29.07, −14.33] | −40.73 [−48.15, −33.32] | −76.53 [−85.13, −67.94] | −140.87 [−148.25, −133.49] | −178.63 [−184.41, −172.86] | −178.93 [−184.72, −173.14] | −177.13 [−183.05, −171.22] |
| Died of wounds | −40.43 [−134.02, +53.15] | −28.60 [−121.68, +64.48] | −72.77 [−170.89, +25.36] | −90.03 [−184.41, +4.35] | −175.77 [−275.12, −76.41] | −148.33 [−264.79, −31.88]† | −112.33 [−222.38, −2.28]† |
| Role 4 peak census | +33.7 [+30.2, +37.2] | +55.3 [+52.7, +57.9] | +95.1 [+92.5, +97.8] | +184.3 [+180.8, +187.7] | +252.6 [+248.7, +256.5] | +254.8 [+249.4, +260.2] | +247.2 [+242.2, +252.3] |
<!-- /GEN -->

At the shipped split and interval at high intensity the critical airlift's closing backlog was <!-- GEN cell:airlift_places_high|Critical airlift closing backlog|36/54, 7 days (shipped)|full -->2506.7 [2465.2, 2548.2]<!-- /GEN -->, with <!-- GEN cell:airlift_places_high|Critical airlift campaigns collapsed|36/54, 7 days (shipped)|full -->30 of 30 [88%, 100%]<!-- /GEN --> campaigns collapsed on the critical route. At the weekly interval, 72 critical places of 90 changed the critical backlog by <!-- GEN cell:airlift_places_paired_high|Critical airlift closing backlog|72/18, 7 days|full -->−1501.9 [−1555.4, −1448.4]<!-- /GEN --> and the standard backlog by <!-- GEN cell:airlift_places_paired_high|Standard airlift closing backlog|72/18, 7 days|full -->+980.0 [+962.9, +997.1]<!-- /GEN -->, with <!-- GEN cell:airlift_places_high|Standard airlift campaigns collapsed|72/18, 7 days|full -->30 of 30 [88%, 100%]<!-- /GEN --> campaigns collapsed on the standard route. Flying every three days at the shipped split changed the critical backlog by <!-- GEN cell:airlift_places_paired_high|Critical airlift closing backlog|36/54, 3 days|full -->−2013.2 [−2074.1, −1952.2]<!-- /GEN -->, with <!-- GEN cell:airlift_places_high|Critical airlift campaigns collapsed|36/54, 3 days|full -->29 of 30 [83%, 100%]<!-- /GEN --> campaigns collapsed on the critical route, and at 45 critical places by <!-- GEN cell:airlift_places_paired_high|Critical airlift closing backlog|45/45, 3 days|full -->−2483.7 [−2525.0, −2442.5]<!-- /GEN -->, with <!-- GEN cell:airlift_places_high|Critical airlift campaigns collapsed|45/45, 3 days|full -->0 of 30 [0%, 12%]<!-- /GEN -->. Deaths of wounds among casualties awaiting airlift changed by <!-- GEN cell:airlift_places_paired_high|Died of wounds awaiting airlift|45/45, 3 days|full -->−178.63 [−184.41, −172.86]<!-- /GEN --> at 45 places every three days, deaths of wounds in total by <!-- GEN cell:airlift_places_paired_high|Died of wounds|45/45, 3 days|full -->−175.77 [−275.12, −76.41]<!-- /GEN -->, and the Role 4 peak census by <!-- GEN cell:airlift_places_paired_high|Role 4 peak census|45/45, 3 days|full -->+252.6 [+248.7, +256.5]<!-- /GEN -->.

---

## Role 4 Bed Demand

<small>[Return to Top](#contents)</small>

**Question.** How many national support base beds does a campaign commit, what is the demand made of, has it settled by the closing window, and how do the forward evacuation levers move it? `[default, both intensities, the evacuation policy, R2E holding establishment, saturation release and sortie cancellation sweeps · 360 d · 30 replications · daily census, mean, peak and closing 90 d]`

The model gives the national support base no capacity, queue or shortfall, so every figure here is demand: the casualties in a Role 4 bed on a day, counted as the other sections count the Role 4 peak. The mean is taken over the whole campaign, the peak is the busiest day of one campaign, and the closing mean is the mean over the last 90 days. A casualty is counted by the ward phase it is in, intensive care or step-down, and by its origin, battle injury, disease and non-battle injury or the reconstruction cohort.

<!-- GEN role4_census_ward -->
| Census by ward phase | Moderate intensity | High intensity |
|---|---|---|
| Total, mean beds | 139.02 [136.18, 141.85] | 210.23 [209.23, 211.23] |
| Total, peak beds | 209.23 [205.24, 213.23] | 281.30 [278.98, 283.62] |
| Total, closing 90-day mean beds | 150.38 [145.47, 155.29] | 219.86 [217.41, 222.31] |
| Intensive care phase, mean beds | 33.59 [32.85, 34.33] | 42.77 [42.57, 42.97] |
| Intensive care phase, peak beds | 63.80 [62.77, 64.83] | 67.57 [66.87, 68.26] |
| Intensive care phase, closing 90-day mean beds | 35.74 [34.43, 37.06] | 43.72 [43.32, 44.13] |
| Step-down ward phase, mean beds | 105.43 [103.26, 107.59] | 167.46 [166.45, 168.47] |
| Step-down ward phase, peak beds | 154.17 [150.14, 158.19] | 223.53 [220.98, 226.08] |
| Step-down ward phase, closing 90-day mean beds | 114.64 [110.88, 118.40] | 176.14 [173.72, 178.55] |
<!-- /GEN -->

At moderate intensity the census averaged <!-- GEN cell:role4_census_ward|Total, mean beds|Moderate intensity|full -->139.02 [136.18, 141.85]<!-- /GEN --> beds over the campaign, peaked at <!-- GEN cell:role4_census_ward|Total, peak beds|Moderate intensity|full -->209.23 [205.24, 213.23]<!-- /GEN --> and averaged <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|Moderate intensity|full -->150.38 [145.47, 155.29]<!-- /GEN --> over the closing 90 days. At high intensity the three figures were <!-- GEN cell:role4_census_ward|Total, mean beds|High intensity|full -->210.23 [209.23, 211.23]<!-- /GEN -->, <!-- GEN cell:role4_census_ward|Total, peak beds|High intensity|full -->281.30 [278.98, 283.62]<!-- /GEN --> and <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|High intensity|full -->219.86 [217.41, 222.31]<!-- /GEN -->. The closing mean exceeded the campaign mean at both intensities. The step-down ward phase carried <!-- GEN cell:role4_census_ward|Step-down ward phase, mean beds|Moderate intensity|mean -->105.43<!-- /GEN --> of the <!-- GEN cell:role4_census_ward|Total, mean beds|Moderate intensity|mean -->139.02<!-- /GEN --> mean beds at moderate intensity and <!-- GEN cell:role4_census_ward|Step-down ward phase, mean beds|High intensity|mean -->167.46<!-- /GEN --> of the <!-- GEN cell:role4_census_ward|Total, mean beds|High intensity|mean -->210.23<!-- /GEN --> at high, and the intensive care phase the remainder.

![Daily Role 4 census over a 360-day campaign at each casualty intensity, divided by ward phase and by origin](../images/role4_demand_census.png)

The mean daily census across replications with its 95% interval, at each casualty intensity, divided by ward phase (upper row) and by origin (lower row), each beside the total it sums to. At high intensity the census rises and falls in a regular cycle about its mean.

<!-- GEN role4_census_origin -->
| Census by origin | Moderate intensity | High intensity |
|---|---|---|
| Total, mean beds | 139.02 [136.18, 141.85] | 210.23 [209.23, 211.23] |
| Total, peak beds | 209.23 [205.24, 213.23] | 281.30 [278.98, 283.62] |
| Total, closing 90-day mean beds | 150.38 [145.47, 155.29] | 219.86 [217.41, 222.31] |
| Battle injury, mean beds | 97.77 [95.26, 100.28] | 173.07 [171.83, 174.31] |
| Battle injury, peak beds | 158.67 [154.50, 162.83] | 238.70 [236.08, 241.32] |
| Battle injury, closing 90-day mean beds | 106.81 [102.69, 110.93] | 180.96 [178.29, 183.64] |
| Disease and non-battle injury, mean beds | 18.69 [18.28, 19.10] | 10.58 [10.25, 10.92] |
| Disease and non-battle injury, peak beds | 36.07 [34.09, 38.04] | 22.13 [21.10, 23.17] |
| Disease and non-battle injury, closing 90-day mean beds | 19.31 [18.37, 20.25] | 11.03 [10.46, 11.60] |
| Reconstruction cohort, mean beds | 22.56 [22.05, 23.07] | 26.58 [26.08, 27.08] |
| Reconstruction cohort, peak beds | 38.63 [36.96, 40.30] | 41.23 [39.98, 42.49] |
| Reconstruction cohort, closing 90-day mean beds | 24.26 [22.99, 25.54] | 27.87 [26.83, 28.91] |
<!-- /GEN -->

Battle injury accounted for <!-- GEN cell:role4_census_origin|Battle injury, mean beds|Moderate intensity|mean -->97.77<!-- /GEN --> of the mean beds at moderate intensity and <!-- GEN cell:role4_census_origin|Battle injury, mean beds|High intensity|mean -->173.07<!-- /GEN --> at high. Disease and non-battle injury accounted for <!-- GEN cell:role4_census_origin|Disease and non-battle injury, mean beds|Moderate intensity|mean -->18.69<!-- /GEN --> and <!-- GEN cell:role4_census_origin|Disease and non-battle injury, mean beds|High intensity|mean -->10.58<!-- /GEN -->, and the reconstruction cohort for <!-- GEN cell:role4_census_origin|Reconstruction cohort, mean beds|Moderate intensity|mean -->22.56<!-- /GEN --> and <!-- GEN cell:role4_census_origin|Reconstruction cohort, mean beds|High intensity|mean -->26.58<!-- /GEN -->.

<!-- GEN role4_operations -->
| Demand owed alongside the census | Moderate intensity | High intensity |
|---|---|---|
| Operations owed within the 360 days | 1,195 [1,156, 1,234] | 2,025 [1,997, 2,053] |
| Definitive repairs owed within the 360 days | 129 [114, 143] | 736 [715, 756] |
| Debridements owed within the 360 days | 772 [747, 796] | 944 [927, 962] |
| Reconstructions owed within the 360 days | 295 [288, 302] | 345 [338, 351] |
| Operations owed after day 360 | 21 [18, 24] | 25 [22, 28] |
| Operations owed by casualties admitted during the campaign | 1,216 [1,177, 1,256] | 2,050 [2,021, 2,079] |
| Operations owed on the busiest day | 20.73 [19.30, 22.17] | 27.93 [27.14, 28.73] |
| Closing 90-day mean operations owed per day | 3.59 [3.33, 3.85] | 5.86 [5.69, 6.03] |
| Theatre minutes owed for definitive repairs | 15,845 [14,039, 17,650] | 91,014 [88,405, 93,624] |
<!-- /GEN -->

The operations owed alongside the census were counted three ways. <!-- GEN cell:role4_operations|Operations owed within the 360 days|Moderate intensity|mean -->1,195<!-- /GEN --> fell inside the campaign at moderate intensity and <!-- GEN cell:role4_operations|Operations owed within the 360 days|High intensity|mean -->2,025<!-- /GEN --> at high, a further <!-- GEN cell:role4_operations|Operations owed after day 360|Moderate intensity|mean -->21<!-- /GEN --> and <!-- GEN cell:role4_operations|Operations owed after day 360|High intensity|mean -->25<!-- /GEN --> fell after the campaign ended, in the reconstruction sequences of casualties admitted late, and <!-- GEN cell:role4_operations|Operations owed by casualties admitted during the campaign|Moderate intensity|mean -->1,216<!-- /GEN --> and <!-- GEN cell:role4_operations|Operations owed by casualties admitted during the campaign|High intensity|mean -->2,050<!-- /GEN --> were owed in all, the figure the other sections report as operations owed. By source, the operations inside the campaign were <!-- GEN cell:role4_operations|Definitive repairs owed within the 360 days|Moderate intensity|mean -->129<!-- /GEN --> definitive repairs, <!-- GEN cell:role4_operations|Debridements owed within the 360 days|Moderate intensity|mean -->772<!-- /GEN --> debridements and <!-- GEN cell:role4_operations|Reconstructions owed within the 360 days|Moderate intensity|mean -->295<!-- /GEN --> reconstructions at moderate intensity, and <!-- GEN cell:role4_operations|Definitive repairs owed within the 360 days|High intensity|mean -->736<!-- /GEN -->, <!-- GEN cell:role4_operations|Debridements owed within the 360 days|High intensity|mean -->944<!-- /GEN --> and <!-- GEN cell:role4_operations|Reconstructions owed within the 360 days|High intensity|mean -->345<!-- /GEN --> at high. The busiest day owed <!-- GEN cell:role4_operations|Operations owed on the busiest day|Moderate intensity|mean -->20.73<!-- /GEN --> operations at moderate intensity and <!-- GEN cell:role4_operations|Operations owed on the busiest day|High intensity|mean -->27.93<!-- /GEN --> at high. The theatre minutes cover the definitive repairs only, no source reporting the time a debridement or a flap takes.

![Mean operations owed per day at Role 4 over each week of a 360-day campaign at each casualty intensity, by source and in all](../images/role4_demand_operations.png)

The mean operations owed per day over each week of the campaign, with the 95% interval across replications, at each casualty intensity, for the definitive repairs, the debridements and the reconstructions and for their total.

<!-- GEN role4_stability -->
| Census | Moderate intensity classification | Moderate intensity settles by block | Moderate intensity late mean beds | High intensity classification | High intensity settles by block | High intensity late mean beds |
|---|---|---|---|---|---|---|
| Total | converged | 2 | 146.96 | converged | 2 | 219.81 |
| Intensive care phase | converged | 2 | 34.77 | converged | 2 | 43.79 |
| Step-down ward phase | converged | 2 | 112.19 | converged | 2 | 176.02 |
| Battle injury | converged | 2 | 103.59 | converged | 2 | 180.92 |
| Disease and non-battle injury | converged | 2 | 19.46 | converged | 2 | 10.92 |
| Reconstruction cohort | converged | 2 | 23.91 | converged | 2 | 27.97 |
<!-- /GEN -->

<!-- GEN role4_cma -->
| Cumulative moving average of the mean total census (beds) | Moderate intensity | High intensity |
|---|---|---|
| Day 30 | 55.70 | 103.32 |
| Day 90 | 116.35 | 180.21 |
| Day 180 | 131.15 | 200.97 |
| Day 360 | 139.02 | 210.23 |
<!-- /GEN -->

The 30-day block means of the total census were classified <!-- GEN cell:role4_stability|Total|Moderate intensity classification|full -->converged<!-- /GEN --> at moderate intensity, settling by block <!-- GEN cell:role4_stability|Total|Moderate intensity settles by block|full -->2<!-- /GEN -->, and <!-- GEN cell:role4_stability|Total|High intensity classification|full -->converged<!-- /GEN --> at high, settling by block <!-- GEN cell:role4_stability|Total|High intensity settles by block|full -->2<!-- /GEN -->. The reconstruction cohort was classified <!-- GEN cell:role4_stability|Reconstruction cohort|Moderate intensity classification|full -->converged<!-- /GEN --> at moderate intensity and <!-- GEN cell:role4_stability|Reconstruction cohort|High intensity classification|full -->converged<!-- /GEN --> at high. The cumulative moving average of the mean total census was <!-- GEN cell:role4_cma|Day 90|Moderate intensity|full -->116.35<!-- /GEN --> beds at day 90 and <!-- GEN cell:role4_cma|Day 360|Moderate intensity|full -->139.02<!-- /GEN --> at day 360 at moderate intensity, and <!-- GEN cell:role4_cma|Day 90|High intensity|full -->180.21<!-- /GEN --> and <!-- GEN cell:role4_cma|Day 360|High intensity|full -->210.23<!-- /GEN --> at high.

**Response to the forward levers.** The three sweeps below carry the Role 4 peak, the closing 90-day mean and the operations owed by the casualties admitted in their own evidence sets, and are read from them without a further run; the sortie cancellation sweep was measured for this section, the strategic airlift evidence set carrying the peak alone. Each table's shipped column is the configuration of the census tables above at moderate intensity.

<!-- GEN role4_levers_policy -->
| Response | 15 d | 21 d (shipped) | 30 d | 45 d | 60 d |
|---|---|---|---|---|---|
| Role 4 peak beds | 229.9 [224.9, 234.9] | 209.2 [205.2, 213.2] | 194.6 [190.2, 199.0] | 176.4 [172.0, 180.8] | 155.1 [149.4, 160.8] |
| Role 4 closing 90-day mean beds | 128.1 [123.2, 133.1] | 121.8 [118.1, 125.5] | 113.1 [107.8, 118.3] | 97.2 [92.6, 101.8] | 81.9 [77.4, 86.4] |
| Role 4 operations owed | 1228 [1182, 1273] | 1216 [1177, 1256] | 1204 [1156, 1252] | 1226 [1177, 1275] | 1263 [1205, 1321] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|15 d|mean -->128.1<!-- /GEN --> beds at 15 days, <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|21 d (shipped)|mean -->121.8<!-- /GEN --> at the shipped 21 days, <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|30 d|mean -->113.1<!-- /GEN --> at 30, <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|45 d|mean -->97.2<!-- /GEN --> at 45 and <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|60 d|mean -->81.9<!-- /GEN --> at 60. The peak was <!-- GEN cell:role4_levers_policy|Role 4 peak beds|21 d (shipped)|mean -->209.2<!-- /GEN --> at 21 days and <!-- GEN cell:role4_levers_policy|Role 4 peak beds|30 d|mean -->194.6<!-- /GEN -->, <!-- GEN cell:role4_levers_policy|Role 4 peak beds|45 d|mean -->176.4<!-- /GEN --> and <!-- GEN cell:role4_levers_policy|Role 4 peak beds|60 d|mean -->155.1<!-- /GEN --> at 30, 45 and 60. The operations owed were <!-- GEN cell:role4_levers_policy|Role 4 operations owed|21 d (shipped)|mean -->1216<!-- /GEN --> at 21 days and <!-- GEN cell:role4_levers_policy|Role 4 operations owed|60 d|mean -->1263<!-- /GEN --> at 60.

<!-- GEN role4_levers_establishment -->
| Response | 30 beds (shipped) | 45 beds | 60 beds | 90 beds |
|---|---|---|---|---|
| Role 4 peak beds | 209.2 [205.2, 213.2] | 212.8 [208.4, 217.3] | 209.1 [204.1, 214.0] | 207.7 [202.9, 212.5] |
| Role 4 closing 90-day mean beds | 121.8 [118.1, 125.5] | 120.0 [114.4, 125.5] | 119.6 [114.2, 125.1] | 116.5 [111.9, 121.2] |
| Role 4 operations owed | 1216 [1177, 1256] | 1212 [1163, 1261] | 1209 [1150, 1269] | 1187 [1138, 1236] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|30 beds (shipped)|mean -->121.8<!-- /GEN --> beds at the shipped 30 R2E holding beds and <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|45 beds|mean -->120.0<!-- /GEN -->, <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|60 beds|mean -->119.6<!-- /GEN --> and <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|90 beds|mean -->116.5<!-- /GEN --> at 45, 60 and 90; the four intervals overlap. The peak was <!-- GEN cell:role4_levers_establishment|Role 4 peak beds|30 beds (shipped)|mean -->209.2<!-- /GEN --> at 30 beds and <!-- GEN cell:role4_levers_establishment|Role 4 peak beds|90 beds|mean -->207.7<!-- /GEN --> at 90.

<!-- GEN role4_levers_saturation -->
| Response | 0 (disabled) | 1 | 2 (shipped) | 3 | 5 | 8 | 12 | 16 | 24 |
|---|---|---|---|---|---|---|---|---|---|
| Role 4 peak beds | 209.7 [204.5, 214.9] | 210.6 [205.6, 215.6] | 209.2 [205.2, 213.2] | 207.1 [203.2, 211.0] | 209.1 [204.1, 214.2] | 211.0 [207.0, 215.0] | 210.5 [206.0, 215.1] | 212.4 [208.3, 216.4] | 208.2 [203.1, 213.3] |
| Role 4 closing 90-day mean beds | 119.7 [114.7, 124.7] | 119.4 [115.1, 123.7] | 121.8 [118.1, 125.5] | 116.8 [112.1, 121.5] | 119.1 [114.7, 123.5] | 117.6 [112.8, 122.4] | 117.8 [113.5, 122.1] | 119.0 [115.0, 123.0] | 115.4 [109.7, 121.1] |
| Role 4 operations owed | 1077 [1040, 1114] | 1270 [1218, 1322] | 1216 [1177, 1256] | 1174 [1135, 1213] | 1178 [1142, 1215] | 1143 [1103, 1184] | 1140 [1108, 1172] | 1136 [1092, 1179] | 1071 [1023, 1119] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_saturation|Role 4 closing 90-day mean beds|0 (disabled)|mean -->119.7<!-- /GEN --> beds with the release disabled and <!-- GEN cell:role4_levers_saturation|Role 4 closing 90-day mean beds|2 (shipped)|mean -->121.8<!-- /GEN --> at the shipped threshold of 2, and ranged across the nine thresholds without a monotone course. The operations owed were <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|0 (disabled)|mean -->1077<!-- /GEN --> with the release disabled, <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|1|mean -->1270<!-- /GEN --> at a threshold of 1, <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|2 (shipped)|mean -->1216<!-- /GEN --> at 2 and <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|24|mean -->1071<!-- /GEN --> at 24.

<!-- GEN role4_levers_cancellation -->
| Response | 0% (shipped) | 5% | 10% | 15% | 25% | 40% |
|---|---|---|---|---|---|---|
| Role 4 peak beds | 209.2 [205.2, 213.2] | 214.1 [208.6, 219.5] | 213.9 [208.4, 219.4] | 212.9 [209.3, 216.5] | 203.0 [199.7, 206.4] | 201.0 [196.8, 205.2] |
| Role 4 closing 90-day mean beds | 150.4 [145.5, 155.3] | 148.7 [143.2, 154.2] | 144.6 [139.0, 150.3] | 140.6 [134.7, 146.5] | 139.6 [133.9, 145.4] | 117.2 [109.7, 124.8] |
| Role 4 operations owed | 1216 [1177, 1256] | 1237 [1189, 1286] | 1173 [1138, 1208] | 1163 [1119, 1206] | 1078 [1045, 1111] | 883 [836, 931] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|0% (shipped)|mean -->150.4<!-- /GEN --> beds with no sortie cancelled and <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|10%|mean -->144.6<!-- /GEN -->, <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|25%|mean -->139.6<!-- /GEN --> and <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|40%|mean -->117.2<!-- /GEN --> at cancellation probabilities of 10%, 25% and 40%. The peak was <!-- GEN cell:role4_levers_cancellation|Role 4 peak beds|0% (shipped)|mean -->209.2<!-- /GEN --> with no cancellation and <!-- GEN cell:role4_levers_cancellation|Role 4 peak beds|40%|mean -->201.0<!-- /GEN --> at 40%, and the operations owed were <!-- GEN cell:role4_levers_cancellation|Role 4 operations owed|0% (shipped)|mean -->1216<!-- /GEN --> and <!-- GEN cell:role4_levers_cancellation|Role 4 operations owed|40%|mean -->883<!-- /GEN -->.

![Role 4 peak beds, closing 90-day mean beds and operations owed against the evacuation policy, R2E holding establishment, saturation release threshold and sortie cancellation probability](../images/role4_demand_levers.png)

The Role 4 peak beds (upper row), closing 90-day mean beds (middle row) and operations owed (lower row), with 95% intervals across replications, against each lever, with the shipped value of each marked by a dashed line.

---

## Casualty Surge Events

<small>[Return to Top](#contents)</small>

**Question.** What changes when casualty surge events are injected into a campaign? `[default, injection off and on at 0.2 events per day · 360 d · 30 replications per arm · independent seeds, Student t]`

<!-- GEN casualty_surge -->
| Metric | No events injected | Events injected |
|---|---|---|
| Average total casualties/run | 5386.9 | 8286.2 |
| Average events/run | 0 | 75.20 (range 58–88) |
| Died-of-wounds rate, ordinary casualties | 0.39% [0.33%, 0.46%] | 1.31% [1.20%, 1.41%] |
| Died-of-wounds rate, event casualties | not applicable | 2.41% [2.24%, 2.59%] |
<!-- /GEN -->

![Stem plot of casualty surge events reconstructed from one campaign](../images/casualty_surge_events.png)

Casualty surge events reconstructed from one seed-42 campaign, each drawn as a vertical line at its simulation day with a point at its casualty count.

Events added <!-- GEN cell:casualty_surge|Average events/run|Events injected|full -->75.20 (range 58–88)<!-- /GEN --> events per campaign and raised the mean campaign total from <!-- GEN cell:casualty_surge|Average total casualties/run|No events injected|full -->5386.9<!-- /GEN --> to <!-- GEN cell:casualty_surge|Average total casualties/run|Events injected|full -->8286.2<!-- /GEN --> casualties. The died-of-wounds rate among event casualties was <!-- GEN cell:casualty_surge|Died-of-wounds rate, event casualties|Events injected|full -->2.41% [2.24%, 2.59%]<!-- /GEN -->. Among ordinary casualties it was <!-- GEN cell:casualty_surge|Died-of-wounds rate, ordinary casualties|No events injected|full -->0.39% [0.33%, 0.46%]<!-- /GEN --> without injection and <!-- GEN cell:casualty_surge|Died-of-wounds rate, ordinary casualties|Events injected|full -->1.31% [1.20%, 1.41%]<!-- /GEN --> with it.

**High-intensity profile.** The same stress test under `high_intensity` `[high_intensity, 0 and 0.2 events per day · 360 d · 30 replications · campaign totals]`:

<!-- GEN casualty_surge_high -->
| Metric | No events injected | Events injected |
|---|---|---|
| Average total casualties/run | 12379.7 | 15063.6 |
| Average events/run | 0 | 75.20 (range 58–88) |
| Died-of-wounds rate, ordinary casualties | 4.56% [4.06%, 5.07%] | 14.23% [13.49%, 14.98%] |
| Died-of-wounds rate, event casualties | not applicable | 16.12% [15.19%, 17.05%] |
<!-- /GEN -->

Event casualties died of wounds at <!-- GEN cell:casualty_surge_high|Died-of-wounds rate, event casualties|Events injected|full -->16.12% [15.19%, 17.05%]<!-- /GEN --> against <!-- GEN cell:casualty_surge_high|Died-of-wounds rate, ordinary casualties|Events injected|full -->14.23% [13.49%, 14.98%]<!-- /GEN --> for ordinary casualties in the same arm.

---

## Casualty Surge Event Size

<small>[Return to Top](#contents)</small>

**Question.** At what size do casualty surge events begin to degrade care? `[default, every event fixed at one size · 0.2 events per day · 360 d · 30 replications per size · independent seeds, Student t]`

<!-- GEN casualty_surge_size -->
| Event size | Events/run | Died of wounds, event casualties | Died of wounds, ordinary casualties | Peak R2B holding queue | Peak R2E theatre queue | Peak R2E post-operative overflow census | Peak R2E holding queue | Peak R2E staging overflow census |
|---|---|---|---|---|---|---|---|---|
| None | 0.0 | not applicable | 0.39% [0.33%, 0.46%] | 2.1 ± 0.7 | 33.7 ± 6.3 | 27.1 ± 0.8 | 59.4 ± 4.1 | 112.8 ± 10.4 |
| 10 | 75.2 | 0.81% [0.69%, 0.94%] | 0.51% [0.41%, 0.61%] | 2.3 ± 0.5 | 35.7 ± 10.7 | 27.5 ± 0.6 | 63.2 ± 3.1 | 170.7 ± 22.2 |
| 20 | 75.2 | 1.26% [1.11%, 1.42%] | 0.74% [0.65%, 0.82%] | 2.5 ± 0.4 | 39.7 ± 9.8 | 28.6 ± 0.7 | 64.9 ± 4.9 | 411.1 ± 44.0 |
| 40 | 75.2 | 2.28% [2.17%, 2.40%] | 1.17% [1.07%, 1.27%] | 3.0 ± 0.3 | 57.9 ± 6.1 | 30.6 ± 0.5 | 68.8 ± 3.5 | 992.6 ± 59.2 |
| 60 | 75.2 | 3.58% [3.37%, 3.79%] | 1.71% [1.56%, 1.86%] | 3.9 ± 0.7 | 105.8 ± 12.4 | 31.3 ± 0.5 | 80.0 ± 3.7 | 1584.3 ± 77.4 |
| 90 | 75.2 | 6.45% [6.05%, 6.85%] | 2.89% [2.68%, 3.10%] | 5.2 ± 0.6 | 239.6 ± 24.7 | 32.8 ± 0.5 | 81.5 ± 4.5 | 2303.2 ± 85.1 |
| 120 | 75.2 | 12.19% [11.08%, 13.31%] | 6.27% [5.58%, 6.96%] | 5.8 ± 0.5 | 562.0 ± 77.0 | 31.4 ± 0.5 | 78.9 ± 2.5 | 2433.9 ± 81.0 |
| 180 | 75.2 | 21.18% [20.48%, 21.87%] | 11.55% [11.04%, 12.05%] | 8.4 ± 1.1 | 2134.1 ± 217.2 | 32.0 ± 0.7 | 87.3 ± 2.4 | 1559.2 ± 99.3 |
<!-- /GEN -->

![Line plot of the died-of-wounds rate of event and ordinary casualties against casualty surge event size](../images/casualty_surge_size_sweep.png)

The died-of-wounds rate of event and ordinary casualties against the number of casualties per event, with Student $t$ 95% confidence intervals across replications.

Peak queues are the largest four-hour mean queue, or census for an overflow point, of each pool over the campaign, with a 95% half-width across replications. With every event fixed at 10 casualties, the died-of-wounds rate among event casualties was <!-- GEN cell:casualty_surge_size|10|Died of wounds, event casualties|full -->0.81% [0.69%, 0.94%]<!-- /GEN --> and the peak R2E post-operative overflow census <!-- GEN cell:casualty_surge_size|10|Peak R2E post-operative overflow census|full -->27.5 ± 0.6<!-- /GEN -->, against <!-- GEN cell:casualty_surge_size|None|Peak R2E post-operative overflow census|full -->27.1 ± 0.8<!-- /GEN --> with no events injected. At 20 casualties per event the post-operative overflow census peaked at <!-- GEN cell:casualty_surge_size|20|Peak R2E post-operative overflow census|full -->28.6 ± 0.7<!-- /GEN -->, and at 40 at <!-- GEN cell:casualty_surge_size|40|Peak R2E post-operative overflow census|full -->30.6 ± 0.5<!-- /GEN -->. The R2E theatre queue peaked at <!-- GEN cell:casualty_surge_size|None|Peak R2E theatre queue|full -->33.7 ± 6.3<!-- /GEN --> with no events, <!-- GEN cell:casualty_surge_size|60|Peak R2E theatre queue|full -->105.8 ± 12.4<!-- /GEN --> at 60 casualties per event, <!-- GEN cell:casualty_surge_size|90|Peak R2E theatre queue|full -->239.6 ± 24.7<!-- /GEN --> at 90 and <!-- GEN cell:casualty_surge_size|180|Peak R2E theatre queue|full -->2134.1 ± 217.2<!-- /GEN --> at 180. The event casualty died-of-wounds rate was <!-- GEN cell:casualty_surge_size|60|Died of wounds, event casualties|full -->3.58% [3.37%, 3.79%]<!-- /GEN --> at 60 casualties and <!-- GEN cell:casualty_surge_size|180|Died of wounds, event casualties|full -->21.18% [20.48%, 21.87%]<!-- /GEN --> at 180, and the rate among ordinary casualties was <!-- GEN cell:casualty_surge_size|None|Died of wounds, ordinary casualties|full -->0.39% [0.33%, 0.46%]<!-- /GEN --> with no events and <!-- GEN cell:casualty_surge_size|180|Died of wounds, ordinary casualties|full -->11.55% [11.04%, 12.05%]<!-- /GEN --> at 180.

**High-intensity profile.** The same sweep under `high_intensity` `[high_intensity, every event fixed at one size · 0.2 events per day · 360 d · 30 replications per size · independent seeds]`:

<!-- GEN casualty_surge_size_high -->
| Event size | Events/run | Died of wounds, event casualties | Died of wounds, ordinary casualties | Peak R2B holding queue | Peak R2E theatre queue | Peak R2E post-operative overflow census | Peak R2E holding queue | Peak R2E staging overflow census |
|---|---|---|---|---|---|---|---|---|
| None | 0.0 | not applicable | 4.56% [4.06%, 5.07%] | 3.4 ± 0.5 | 185.2 ± 27.5 | 31.9 ± 0.5 | 90.1 ± 2.6 | 2867.4 ± 45.1 |
| 10 | 75.2 | 7.96% [6.70%, 9.22%] | 6.98% [5.94%, 8.03%] | 4.2 ± 1.1 | 345.0 ± 64.1 | 31.4 ± 0.3 | 91.6 ± 3.3 | 2831.9 ± 96.6 |
| 20 | 75.2 | 11.50% [10.52%, 12.48%] | 10.31% [9.38%, 11.24%] | 3.7 ± 0.5 | 592.8 ± 85.2 | 31.5 ± 0.6 | 88.8 ± 3.1 | 2508.7 ± 99.4 |
| 40 | 75.2 | 16.60% [15.69%, 17.51%] | 14.21% [13.61%, 14.81%] | 3.9 ± 0.6 | 1182.7 ± 90.2 | 31.2 ± 0.5 | 85.4 ± 2.4 | 2008.6 ± 80.7 |
| 60 | 75.2 | 18.82% [18.20%, 19.44%] | 15.85% [15.42%, 16.28%] | 5.4 ± 0.8 | 1893.9 ± 108.4 | 31.1 ± 0.5 | 88.3 ± 3.2 | 1686.7 ± 59.2 |
| 90 | 75.2 | 20.18% [19.72%, 20.63%] | 16.94% [16.63%, 17.25%] | 5.8 ± 0.7 | 2958.8 ± 151.6 | 30.7 ± 0.4 | 93.0 ± 3.7 | 1367.2 ± 50.1 |
| 120 | 75.2 | 20.73% [20.31%, 21.14%] | 17.41% [17.05%, 17.76%] | 7.1 ± 0.7 | 3853.5 ± 150.8 | 31.0 ± 0.5 | 88.9 ± 2.7 | 1224.1 ± 45.9 |
| 180 | 75.2 | 21.99% [21.52%, 22.46%] | 18.25% [17.85%, 18.66%] | 8.6 ± 0.9 | 5343.5 ± 156.7 | 31.4 ± 0.8 | 99.4 ± 5.2 | 1120.4 ± 33.6 |
<!-- /GEN -->

At 20 casualties per event the peak R2E post-operative overflow census was <!-- GEN cell:casualty_surge_size_high|20|Peak R2E post-operative overflow census|full -->31.5 ± 0.6<!-- /GEN -->, against <!-- GEN cell:casualty_surge_size_high|None|Peak R2E post-operative overflow census|full -->31.9 ± 0.5<!-- /GEN --> with no events injected.

---

## Resolution of Paired Differences

<small>[Return to Top](#contents)</small>

**Question.** Which of the paired differences the experiments above sized for resolution were resolved at 30 replications, their interval excluding zero and their difference surviving Holm's adjustment within the experiment, and how many replications would each of the others need before its interval narrowed to a stated half-width? `[as each experiment above · 360 d · 30 replications · paired, Holm-adjusted, normal approximation to the interval half-width]`

<!-- GEN resolution -->
| Comparison | Paired difference | Holm-adjusted p | Half-width sought | Replications needed |
|---|---|---|---|---|
| Hold window, R2E first surgeries | −18.00 [−75.22, +39.22] | 1.000 | 2.0 | 22,554 |
| Hold window, R2E theatre entry deferred | −1.97 [−17.60, +13.66] | 1.000 | 1.0 | 6,732 |
| Hold window, diverted for a busy theatre | +22.40 [+1.70, +43.10] | 0.175 | 2.0 | 2,953 |
| Hold window, died of wounds | +1.90 [−2.11, +5.91] | 1.000 | 0.5 | 1,771 |
| Intensive care gate, died of wounds | −0.20 [−5.03, +4.63] | 1.000 | 0.5 | 2,570 |
| Policy 15 days against 21, died of wounds | +0.77 [−3.65, +5.18] | 1.000 | 1.0 | 537 |
| Policy 45 days against 21, died of wounds | −1.70 [−6.29, +2.89] | 1.000 | 1.0 | 581 |
| Policy 60 days against 21, died of wounds | −1.20 [−6.07, +3.67] | 1.000 | 1.0 | 653 |
| Saturation release at 2, died of wounds | −2.33 [−9.73, +5.06] | 1.000 | 1.0 | 1,506 |
| Saturation release at 2, returns to duty | +13.67 [−34.40, +61.74] | 1.000 | 10.0 | 637 |
<!-- /GEN -->

A comparison whose 95% interval excludes zero and whose Holm-adjusted $p$-value is within the family-wise level is listed in the second table rather than the first, the split being taken from the evidence each time the document is generated; the second table is empty where no comparison is resolved.

<!-- GEN resolved -->
| Comparison | Paired difference | Holm-adjusted p |
|---|---|---|
<!-- /GEN -->

The count for each row is $\lceil (z_{0.975}\, s_d / h)^2 \rceil$, where $s_d$ is the standard deviation of the within-replication differences observed at 30 replications and $h$ the half-width sought, chosen in the response's own units by the script that ran the experiment. The hold window's R2E first-surgery difference was <!-- GEN cell:resolution|Hold window, R2E first surgeries|Paired difference|full -->−18.00 [−75.22, +39.22]<!-- /GEN --> against a half-width sought of <!-- GEN cell:resolution|Hold window, R2E first surgeries|Half-width sought|full -->2.0<!-- /GEN --> operations, which needs <!-- GEN cell:resolution|Hold window, R2E first surgeries|Replications needed|full -->22,554<!-- /GEN --> replications. The intensive care gate's died-of-wounds difference was <!-- GEN cell:resolution|Intensive care gate, died of wounds|Paired difference|full -->−0.20 [−5.03, +4.63]<!-- /GEN --> and needs <!-- GEN cell:resolution|Intensive care gate, died of wounds|Replications needed|full -->2,570<!-- /GEN -->. Against a half-width sought of <!-- GEN cell:resolution|Policy 15 days against 21, died of wounds|Half-width sought|full -->1.0<!-- /GEN --> death per campaign, the 15-day and 45-day policy comparisons against the shipped 21 days need <!-- GEN cell:resolution|Policy 15 days against 21, died of wounds|Replications needed|full -->537<!-- /GEN --> and <!-- GEN cell:resolution|Policy 45 days against 21, died of wounds|Replications needed|full -->581<!-- /GEN --> replications. The 60-day policy's died-of-wounds difference against 21 days was <!-- GEN cell:resolution|Policy 60 days against 21, died of wounds|Paired difference|full -->−1.20 [−6.07, +3.67]<!-- /GEN --> and needs <!-- GEN cell:resolution|Policy 60 days against 21, died of wounds|Replications needed|full -->653<!-- /GEN -->. The saturation release's died-of-wounds difference at the shipped threshold of two needs <!-- GEN cell:resolution|Saturation release at 2, died of wounds|Replications needed|full -->1,506<!-- /GEN -->.

---

## Sensitivity Screens

<small>[Return to Top](#contents)</small>

A Morris elementary effects screen and a Sobol variance decomposition were run at the sustained horizon, and neither is reported here as evidence. Parameters that cannot affect a response inside the simulation, among them the Role 4 length-of-stay modes, which are drawn after the run, ranked among the active parameters on the R2E operating theatre queue, so the screen did not separate most parameters from replication noise, and the decomposition did not establish the orderings it would need. Parameter influence is measured instead by the replicated sweeps reported above, each of which varies one lever across a range of settings. The design of both screens and the reason they were set aside are recorded in [Sensitivity Screens](Methods.md#sensitivity-screens), and the tracked caches remain under `data/sensitivity/` as their provenance.

---

## Annex A. Model Verification of One Seed-42 Campaign

<small>[Return to Top](#contents)</small>

This annex records the measurements of one 360-day campaign at seed 42 that verify mechanisms a replicated experiment cannot show: that each arrival stream realises its configured rate, that the triage and damage control splits realise their configured shares, that the strategic evacuation timeline closes, the surgical sections carry the load their rosters imply and the force regeneration cycle holds the pool near establishment. It reports one run, carries no interval and measures no performance; the replicated sections above do that. The measurements are tracked in `data/seed42/seed42_verification.csv`, written with the baseline by `Rscript run.R --seed 42 --days 360 --iterations 1 --refresh-baseline`.

**Casualty generation.** A stream's configured expectation is its configured daily mean per thousand personnel times its population over a thousand times the days.

<!-- GEN annex_generation -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Combat wounded in action | 1,339 | 1,593 |
| Support wounded in action | 790 | 796 |
| Combat killed in action | 484 | 612 |
| Support killed in action | 357 | 306 |
| Combat disease and non-battle injury | 1,836 | 1,836 |
| Support disease and non-battle injury | 406 | 423 |
<!-- /GEN -->

**Triage.** The trauma split applies to wounded and non-battle injury casualties, and its expectation is the configured share of them. A disease casualty is Priority 1 at the configured medical emergency share, and otherwise Priority 2 if it needs emergency surgery and Priority 3 if not, so the expectations for disease are the medical emergency share of disease casualties and the disease surgical share of the rest; battle fatigue is Priority 3.

<!-- GEN annex_triage -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Priority 1, wounded and non-battle injury | 1,644 | 1,633 |
| Priority 2, wounded and non-battle injury | 476 | 503 |
| Priority 3, wounded and non-battle injury | 393 | 377 |
| Priority 1, disease (medical emergency) | 55 | 64 |
| Priority 2, disease (emergency surgery) | 65 | 74 |
| Priority 3, disease | 1,166 | not applicable |
| Priority 3, battle fatigue | 572 | 572 |
| Killed in action | 841 | not applicable |
<!-- /GEN -->

**Damage control.** The expectation is the configured damage control rate times the casualties operated on at that priority.

<!-- GEN annex_damage_control -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Priority 1 operated | 1,463 | not applicable |
| Priority 1 damage control | 790 | 805 |
| Priority 2 operated | 449 | not applicable |
| Priority 2 damage control | 87 | 90 |
<!-- /GEN -->

**Strategic evacuation.** A sortie carries the casualties queued on each seat pool when it departs, and the places nobody takes depart with it; the expectation of sorties flown is the scheduled sorties before the horizon times the probability that a sortie flies.

<!-- GEN annex_evacuation -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Strategic evacuation decisions | 2,222 | not applicable |
| Boarded | 2,198 | not applicable |
| Still waiting at the close | 24 | not applicable |
| Mean wait of the casualties boarded (days) | 4.09 | not applicable |
| Sorties flown | 51 | 51 |
| Critical places departing empty per sortie (mean) | 8.3 | not applicable |
| Standard places departing empty per sortie (mean) | 38.6 | not applicable |
<!-- /GEN -->

**Admission at Role 2.** Minor casualties are Priority 3 wounded and non-battle injury casualties and disease casualties below Priority 1, who share one convalescence factor; the expectation is the share of that factor's convalescence draws exceeding the theatre evacuation policy. A casualty returned to duty while queued is one whose convalescence or restoration elapsed before a holding bed came free.

<!-- GEN annex_admission -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Minor casualties evacuated strategically (% of those with a disposition) | 11.2 | 9.3 |
| Returned to duty while queued for a holding bed | 588 | not applicable |
| Battle fatigue returned to duty from Role 2 | 568 | not applicable |
<!-- /GEN -->

**Placement at Role 2.** A casualty whose care calls for a bed takes the best one free when that care begins: the bed the care calls for, otherwise a holding bed, otherwise overflow, with no bed. A stabilisation is counted where a stay was served, and the overflow census is the time-weighted mean number of casualties cared for without a bed at each point over the campaign.

<!-- GEN annex_placement -->
| Measure | Realised | Configured expectation |
|---|---|---|
| R2B reception, straight to a resuscitation bed | 2,073 | not applicable |
| R2B reception, waiting in a holding bed | 71 | not applicable |
| R2B reception, waiting in overflow | 34 | not applicable |
| R2B wait for the evacuation vehicle in intensive care | 427 | not applicable |
| R2B wait for the evacuation vehicle in a holding bed | 9 | not applicable |
| R2B wait for the evacuation vehicle in overflow | 8 | not applicable |
| R2E stabilisation in intensive care | 304 | not applicable |
| R2E stabilisation in a holding bed | 47 | not applicable |
| R2E stabilisation in overflow | 539 | not applicable |
| R2E post-definitive care in intensive care | 754 | not applicable |
| R2E post-definitive care in a holding bed | 94 | not applicable |
| R2E post-definitive care in overflow | 1,180 | not applicable |
| Evacuees staged in a holding bed | 232 | not applicable |
| Evacuees staged in overflow | 1,990 | not applicable |
| Mean overflow census, R2B reception | 0.00 | not applicable |
| Mean overflow census, R2B wait for the evacuation vehicle | 0.00 | not applicable |
| Mean overflow census, R2E post-operative stays | 4.89 | not applicable |
| Mean overflow census, R2E pre-flight care | 0.44 | not applicable |
| Mean overflow census, R2E staging | 21.36 | not applicable |
<!-- /GEN -->

**Surgical load.** Utilisation of open time is the time-weighted share of a section's rostered time during which its first surgeon was in use, and the queued share is the share of that open time with one or more casualties waiting for any role in the section.

<!-- GEN annex_surgical_load -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Diverted from R2B, surgical team off shift | 1,105 | not applicable |
| Diverted from R2B, theatre busy | 201 | not applicable |
| R2B holding beds in use, both facilities (mean) | 7.91 | not applicable |
| R2E section 1 utilisation of open time (%) | 28.0 | not applicable |
| R2E section 2 utilisation of open time (%) | 43.7 | not applicable |
| R2E section 3 utilisation of open time (%) | 33.3 | not applicable |
| R2E section 1 open time with casualties waiting for a section (%) | 8.8 | not applicable |
| R2E section 2 open time with casualties waiting for a section (%) | 16.9 | not applicable |
| R2E section 3 open time with casualties waiting for a section (%) | 8.8 | not applicable |
<!-- /GEN -->

**Force regeneration.** The effective force is the combat or support pool at the day shown, under the shipped seven-day reinforcement cycle.

<!-- GEN annex_force -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Combat force, day 0 | 2,500 | not applicable |
| Combat force, day 180 | 2,406 | not applicable |
| Combat force, day 360 | 2,406 | not applicable |
| Support force, day 0 | 1,250 | not applicable |
| Support force, day 180 | 1,185 | not applicable |
| Support force, day 360 | 1,213 | not applicable |
<!-- /GEN -->

The conservation of a casualty's intensive care requirement across the three post-operative routes is asserted by `tests/checks/check_icu_time_conservation.R` and the generation rates by `tests/checks/check_arrival_rate_fidelity.R`.

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Blood, C. G., Zouris, J. M., & Rotblatt, D. (1997). *Using the Ground Forces Casualty Forecasting System (FORECAS) to Project Casualty Sustainment* (NHRC Report No. 97-39). Naval Health Research Center. Retrieved 21 Oct 26, from https://archive.org/details/DTIC_ADA339487

[2] Westphalen, N. (2018). Surgeon Captain Richard Tadeusz 'Rick' Jolly OBE RN Rtd. *Journal of Military and Veterans' Health*, *26*(1). Retrieved 26 Jul 26, from https://jmvh.org/article/surgeon-captain-richard-tadeusz-rick-jolly-obe-rn-rtd/

[3] Marble, S. (2025). Both joint and not: Medical support at Okinawa, 1945. *Joint Force Quarterly*, *117*, 73–80. National Defense University Press. Retrieved 30 Oct 26, from https://digitalcommons.ndu.edu/joint-force-quarterly/vol117/iss2/11/

<!-- REFERENCES END -->
