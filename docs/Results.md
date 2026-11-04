# Measured Results of a Replicated Simulation of the Land-Based Trauma System

## Abstract

<small>[Return to Top](#contents)</small>

**Background**

A discrete event simulation of an Australian brigade-sized force and its land-based trauma system produces measurements that planners can read without the model's own interpretation attached. This document reports those measurements and nothing else: every replicated experiment and the verification of one seed-42 campaign, with a statement of why the sensitivity screens are not reported.

**Methods**

Each replicated experiment ran 30 independent replications of a 360-day campaign under the shipped configuration or a named variation, and measured queues and occupancy as whole-pool totals over each campaign's closing 90 days. Intervals are 95% Student $t$ intervals about the mean of per-replication values unless a section states otherwise. The designs are in [Methods](Methods.md).

**Results**

At moderate casualty intensity a campaign produced <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|mean -->5,350.9<!-- /GEN --> casualties and at high intensity <!-- GEN cell:scenario_totals|Total casualties/run|High intensity|mean -->12,356.9<!-- /GEN -->. Over the closing 90 days the mean R2E operating theatre queue was <!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|mean -->0.515<!-- /GEN --> casualties at moderate intensity and <!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|mean -->56.794<!-- /GEN --> at high. At high intensity that queue stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|High: queue empty|full -->7%<!-- /GEN --> of the campaign and was still growing at day 360, while at moderate intensity it stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: queue empty|full -->85%<!-- /GEN -->. Over the closing 90 days the national support base carried a mean census of <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|Moderate intensity|mean -->144.43<!-- /GEN --> beds at moderate intensity and <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|High intensity|mean -->221.51<!-- /GEN --> at high. The treated-cohort died-of-wounds rate of the shipped default configuration was <!-- GEN cell:dow_calibration|Shipped default|Treated-cohort died-of-wounds rate|full -->0.554% [0.468%, 0.640%]<!-- /GEN --> at 360 days.

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
  - [R2E Intensive Care Bed Establishment](#r2e-intensive-care-bed-establishment)
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

Every table and every quoted figure is regenerated from the tracked evidence under `data/` by `scripts/render_results_tables.R`, and `tests/checks/check_results_tables.R` fails when the document disagrees with that evidence. A figure written in a sentence is a copy of a cell of the table beside it.

---

## Comparative Scenario Analysis

<small>[Return to Top](#contents)</small>

**Question.** How do casualty volume, mortality and the load on each resource pool differ between the moderate-intensity (Falklands 1982) and high-intensity (Okinawa 1945) casualty profiles, whose rates derive from the FORECAS projection study [[1]](#references), when the health system is identical? `[moderate_intensity and high_intensity · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN scenario_totals -->
| Metric | Moderate intensity | High intensity | Ratio |
|---|---|---|---|
| Total casualties/run | 5,350.9 [5,287.1, 5,414.8] (p10–p90: 5,177.2–5,549.5) | 12,356.9 [12,252.4, 12,461.3] (p10–p90: 11,980.1–12,695.8) | 2.31× |
| Wounded in action/run | 2,283.8 [2,227.5, 2,340.2] (p10–p90: 2,095.7–2,474.0) | 8,291.4 [8,202.9, 8,380.0] (p10–p90: 8,014.7–8,601.6) | 3.63× |
| Died of wounds/run | 20.17 [17.72, 22.61] (p10–p90: 13.7–26.3) | 509.33 [464.71, 553.96] (p10–p90: 392.2–635.9) | 25.3× |
| Died of wounds, as share of wounded | 0.88% [0.78%, 0.97%] | 6.12% [5.64%, 6.60%] | 6.98× |
<!-- /GEN -->

![Four panels of total casualties, wounded in action, deaths of wounds and deaths as a share of wounded at the two casualty intensities](../images/paper_casualty_totals.png)

Each panel plots the moderate and high casualty intensities as a point with a narrow 95% confidence interval bar and a wide band for the 10th-to-90th-percentile spread across campaigns. The bar shows how precisely the average is known and the band shows how much one campaign varies.

Moderate intensity produced <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|mean -->5,350.9<!-- /GEN --> casualties per run (95% interval <!-- GEN cell:scenario_totals|Total casualties/run|Moderate intensity|ci -->[5,287.1, 5,414.8]<!-- /GEN -->) and high intensity <!-- GEN cell:scenario_totals|Total casualties/run|High intensity|mean -->12,356.9<!-- /GEN --> (<!-- GEN cell:scenario_totals|Total casualties/run|High intensity|ci -->[12,252.4, 12,461.3]<!-- /GEN -->), a ratio of <!-- GEN cell:scenario_totals|Total casualties/run|Ratio|full -->2.31×<!-- /GEN -->. Wounded in action rose by <!-- GEN cell:scenario_totals|Wounded in action/run|Ratio|full -->3.63×<!-- /GEN -->, died of wounds per run by <!-- GEN cell:scenario_totals|Died of wounds/run|Ratio|full -->25.3×<!-- /GEN --> and died of wounds as a share of wounded by <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|Ratio|full -->6.98×<!-- /GEN -->, from <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|Moderate intensity|mean -->0.88%<!-- /GEN --> to <!-- GEN cell:scenario_totals|Died of wounds, as share of wounded|High intensity|mean -->6.12%<!-- /GEN -->.

<!-- GEN scenario_queue -->
| Resource group | Moderate intensity mean queue | High intensity mean queue | Ratio |
|---|---|---|---|
| R2B operating theatre | 0.000 [0.000, 0.000] | 0.000 [0.000, 0.000] | not applicable |
| R2B holding beds | 0.055 [0.030, 0.081] | 0.357 [0.279, 0.435] | 6.45× |
| R2B reception overflow census | 0.023 [0.004, 0.043] | 0.249 [0.210, 0.288] | 10.68× |
| R2E operating theatre | 0.515 [0.338, 0.692] | 56.794 [42.954, 70.635] | 110.23× |
| R2E holding beds | 22.715 [21.224, 24.206] | 55.398 [53.466, 57.331] | 2.44× |
| R2E post-operative overflow census | 5.247 [4.822, 5.672] | 20.158 [19.956, 20.361] | 3.84× |
| R2E staging overflow census | 26.191 [21.998, 30.385] | 2,534.573 [2,506.907, 2,562.240] | 96.77× |
| Ambulance and truck fleets | 0.147 [0.085, 0.209] | 1.919 [1.544, 2.295] | 13.05× |
<!-- /GEN -->

![Horizontal plot of queue growth factor on a logarithmic scale for five resource groups with a dashed reference line at the growth in casualty volume](../images/paper_queue_growth.png)

Queue growth factor from moderate to high intensity on a logarithmic scale, with a dashed reference line at the growth in casualty volume. Each point is sized by its absolute queue at high intensity.

![Four-panel bar chart of mean queue length by resource group at the two casualty intensities](../images/scenario_comparison.png)

Mean queue length by resource group at each casualty intensity. Each panel has its own vertical scale, and the error bars show the spread across campaigns rather than a confidence interval.

The R2B operating theatre queue was zero at both intensities. At moderate intensity the R2E holding beds carried a mean queue of <!-- GEN cell:scenario_queue|R2E holding beds|Moderate intensity mean queue|mean -->22.715<!-- /GEN --> casualties (<!-- GEN cell:scenario_queue|R2E holding beds|Moderate intensity mean queue|ci -->[21.224, 24.206]<!-- /GEN -->), against <!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|mean -->0.515<!-- /GEN --> at the R2E operating theatres (<!-- GEN cell:scenario_queue|R2E operating theatre|Moderate intensity mean queue|ci -->[0.338, 0.692]<!-- /GEN -->); a mean of <!-- GEN cell:scenario_queue|R2E post-operative overflow census|Moderate intensity mean queue|mean -->5.247<!-- /GEN --> operated casualties were cared for in post-operative overflow and <!-- GEN cell:scenario_queue|R2E staging overflow census|Moderate intensity mean queue|mean -->26.191<!-- /GEN --> awaited a flight in staging overflow. At high intensity the R2E operating theatre queue was <!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|mean -->56.794<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E operating theatre|High intensity mean queue|ci -->[42.954, 70.635]<!-- /GEN -->), the R2E holding queue <!-- GEN cell:scenario_queue|R2E holding beds|High intensity mean queue|mean -->55.398<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E holding beds|High intensity mean queue|ci -->[53.466, 57.331]<!-- /GEN -->), the post-operative overflow census <!-- GEN cell:scenario_queue|R2E post-operative overflow census|High intensity mean queue|mean -->20.158<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E post-operative overflow census|High intensity mean queue|ci -->[19.956, 20.361]<!-- /GEN -->) and the staging overflow census <!-- GEN cell:scenario_queue|R2E staging overflow census|High intensity mean queue|mean -->2,534.573<!-- /GEN --> (<!-- GEN cell:scenario_queue|R2E staging overflow census|High intensity mean queue|ci -->[2,506.907, 2,562.240]<!-- /GEN -->); the ratios to moderate intensity were <!-- GEN cell:scenario_queue|R2E operating theatre|Ratio|full -->110.23×<!-- /GEN -->, <!-- GEN cell:scenario_queue|R2E holding beds|Ratio|full -->2.44×<!-- /GEN -->, <!-- GEN cell:scenario_queue|R2E post-operative overflow census|Ratio|full -->3.84×<!-- /GEN --> and <!-- GEN cell:scenario_queue|R2E staging overflow census|Ratio|full -->96.77×<!-- /GEN -->. The ambulance and truck fleets queued <!-- GEN cell:scenario_queue|Ambulance and truck fleets|Moderate intensity mean queue|mean -->0.147<!-- /GEN --> and <!-- GEN cell:scenario_queue|Ambulance and truck fleets|High intensity mean queue|mean -->1.919<!-- /GEN --> casualties at the two intensities. An overflow census counts casualties cared for without a bed rather than casualties waiting for one, a casualty finding no intensive care or holding bed being placed in overflow instead of queued, so the intensive care pool carries no queue. The high-intensity theatre and staging figures are means over a window in which those quantities were still growing, as the sections that follow record, so they describe the closing 90 days and not a level.

---

## Queue Behaviour over the Campaign

<small>[Return to Top](#contents)</small>

**Question.** Does a pool's queue clear between its busy spells, or does it stand for the whole campaign, and how does the share of post-operative care delivered in a holding bed change with time? `[moderate_intensity and high_intensity · 360 d · 30 replications · pool totals, unbinned series]`

<!-- GEN queue_clearance -->
| Resource pool | Moderate: queue empty | Moderate: longest run above zero (days) | High: queue empty | High: longest run above zero (days) |
|---|---|---|---|---|
| R2B reception overflow | 99% | 0.8 (0.5–1.5) | 91% | 1.8 (1.5–2.1) |
| R2B holding beds | 95% | 8.3 (5.5–12.2) | 73% | 15.1 (14.4–20.4) |
| R2E operating theatres | 85% | 4.7 (3.1–5.9) | 7% | 113.7 (77.2–131.4) |
| R2E post-operative overflow | 14% | 25.6 (23.0–29.8) | 1% | 357.7 (357.4–358.4) |
| R2E holding beds | 1% | 355.9 (354.0–356.7) | 1% | 357.6 (357.4–358.1) |
| R2E staging overflow | 5% | 59.3 (48.8–90.4) | 1% | 357.9 (357.4–358.3) |
<!-- /GEN -->

![Four stacked panels of queue length against campaign day, one per resource pool, at the two casualty intensities](../images/queue_length_over_time.png)

Queue length against campaign day for each pool, as the median across replications with an interquartile band, at each casualty intensity. Each panel is annotated with the share of the campaign the queue stood empty.

The table reports, for each pool and intensity, the median across replications of the share of the campaign the pool's queue or overflow census stood empty and of the longest unbroken spell it did not, with the interquartile range of the spell in parentheses. At high intensity the R2E operating theatre queue stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|High: queue empty|full -->7%<!-- /GEN --> of the campaign, with a median longest spell above zero of <!-- GEN cell:queue_clearance|R2E operating theatres|High: longest run above zero (days)|full -->113.7 (77.2–131.4)<!-- /GEN --> days, and the R2E holding queue and both R2E overflow censuses stood empty for <!-- GEN cell:queue_clearance|R2E staging overflow|High: queue empty|full -->1%<!-- /GEN --> or less. At moderate intensity the theatre queue stood empty for <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: queue empty|full -->85%<!-- /GEN --> of the campaign with a median longest spell of <!-- GEN cell:queue_clearance|R2E operating theatres|Moderate: longest run above zero (days)|full -->4.7 (3.1–5.9)<!-- /GEN --> days, while the R2E holding queue stood empty for <!-- GEN cell:queue_clearance|R2E holding beds|Moderate: queue empty|full -->1%<!-- /GEN -->.

<!-- GEN degraded_care -->
| Intensity and stage | First day the median daily rate reaches 100% | First ten days | Last ten days | Whole campaign |
|---|---|---|---|---|
| Moderate intensity: Stabilisation | not reached | 34.8% | 34.6% | 36.7% |
| Moderate intensity: Post-definitive care | not reached | 59.7% | 65.8% | 64.6% |
| High intensity: Stabilisation | not reached | 41.1% | 44.6% | 43.4% |
| High intensity: Post-definitive care | not reached | 85.9% | 89.3% | 89.0% |
<!-- /GEN -->

![Two stacked panels of the share of casualties recovering in a holding bed against campaign day](../images/degraded_care_rate_over_time.png)

The share of casualties taking the holding-bed recovery against campaign day, stabilisation above and post-definitive care below, as a daily rate inside an interquartile band and as a running cumulative rate.

The degraded post-operative care rate is the share of casualties who took the holding-bed recovery in place of an intensive care bed at the point the decision was taken. At high intensity the median daily rate for post-definitive care first reached 100% on day <!-- GEN cell:degraded_care|High intensity: Post-definitive care|First day the median daily rate reaches 100%|full -->not reached<!-- /GEN --> and for stabilisation on day <!-- GEN cell:degraded_care|High intensity: Stabilisation|First day the median daily rate reaches 100%|full -->not reached<!-- /GEN -->; over the whole campaign <!-- GEN cell:degraded_care|High intensity: Post-definitive care|Whole campaign|full -->89.0%<!-- /GEN --> of post-definitive care and <!-- GEN cell:degraded_care|High intensity: Stabilisation|Whole campaign|full -->43.4%<!-- /GEN --> of stabilisation took the holding bed. At moderate intensity the whole-campaign rates were <!-- GEN cell:degraded_care|Moderate intensity: Post-definitive care|Whole campaign|full -->64.6%<!-- /GEN --> and <!-- GEN cell:degraded_care|Moderate intensity: Stabilisation|Whole campaign|full -->36.7%<!-- /GEN -->, and the median daily rate did not reach 100%.

---

## Sustained Operations

<small>[Return to Top](#contents)</small>

**Question.** Does each response settle to a level over a 360-day campaign, or does it keep moving? `[moderate_intensity and high_intensity · 360 d · 30 replications · twelve 30-day block means]`

Each response was reduced to twelve consecutive 30-day block means and classified from the trend over the late half of the campaign; the rule is in [Methods](Methods.md#classifying-stability).

<!-- GEN long_horizon_stability -->
| Response | Moderate intensity | High intensity |
|---|---|---|
| R2E operating theatre queue | converged at 0.60 | **drifting, +4.5%/block**, 17.2 to 60.7 |
| R2E holding bed queue | converged at 23.1 | converged at 53.7 |
| R2E post-operative overflow census | converged at 5.44 | converged at 20.1 |
| R2E staging overflow census | converged at 29.5 | **drifting, +11.4%/block**, 116.8 to 2,770.7 |
| Strategic evacuation backlog | converged at 39.0 | **drifting, +11.9%/block**, 136.0 to 2,969.0 |
| R2B reception overflow census | converged at 0.02 | converged at 0.26 |
| R2B holding bed queue | converged at 0.06 | converged at 0.37 |
| Casualty arrivals per day | converged at 14.8 | converged at 34.4 |
| Deaths of wounds per day | converged at 0.06 | **drifting, +6.7%/block**, 0.4 to 2.1 |
<!-- /GEN -->

![Panels of 30-day block means against campaign day for every response at the moderate and high casualty intensities](../images/long_horizon_blocks.png)

Block means of every queue, occupancy, arrival, evacuation backlog and mortality response over the campaign at the two casualty intensities, each panel headed by its stability classification at each intensity.

Four responses at high intensity were classified as drifting: the R2E operating theatre queue (<!-- GEN cell:long_horizon_stability|R2E operating theatre queue|High intensity|full -->**drifting, +4.5%/block**, 17.2 to 60.7<!-- /GEN -->), the R2E staging overflow census (<!-- GEN cell:long_horizon_stability|R2E staging overflow census|High intensity|full -->**drifting, +11.4%/block**, 116.8 to 2,770.7<!-- /GEN -->), the strategic evacuation backlog (<!-- GEN cell:long_horizon_stability|Strategic evacuation backlog|High intensity|full -->**drifting, +11.9%/block**, 136.0 to 2,969.0<!-- /GEN -->) and the deaths of wounds per day (<!-- GEN cell:long_horizon_stability|Deaths of wounds per day|High intensity|full -->**drifting, +6.7%/block**, 0.4 to 2.1<!-- /GEN -->). The R2E holding bed queue at high intensity was classified as converged, at <!-- GEN cell:long_horizon_stability|R2E holding bed queue|High intensity|full -->converged at 53.7<!-- /GEN -->, and the post-operative overflow census <!-- GEN cell:long_horizon_stability|R2E post-operative overflow census|High intensity|full -->converged at 20.1<!-- /GEN -->. No response at moderate intensity was classified as drifting; the R2E operating theatre queue was <!-- GEN cell:long_horizon_stability|R2E operating theatre queue|Moderate intensity|full -->converged at 0.60<!-- /GEN -->, the R2E holding bed queue <!-- GEN cell:long_horizon_stability|R2E holding bed queue|Moderate intensity|full -->converged at 23.1<!-- /GEN --> and the staging overflow census <!-- GEN cell:long_horizon_stability|R2E staging overflow census|Moderate intensity|full -->converged at 29.5<!-- /GEN -->.

### Demand, Utilisation and Shortfall per Day

**Question.** How much more demand does the high-intensity profile place on the system than the moderate-intensity one, how fully is each R2E pool used over the closing 90 days, and at what rate does a pool fall behind where it has not settled? `[moderate_intensity and high_intensity · 360 d · 30 replications · closing 90 d and the late half of the campaign]`

<!-- GEN load_demand -->
| Demand measure | Moderate intensity | High intensity | Ratio |
|---|---|---|---|
| Wounded in action/run | 2,283.8 [2,227.5, 2,340.2] | 8,291.4 [8,202.9, 8,380.0] | 3.63× |
| R2B and R2E first surgeries/run | 2,623.9 [2,574.5, 2,673.3] | 7,945.0 [7,873.6, 8,016.4] | 3.03× |
| Post-definitive recovery dispositions/run | 3,901.1 [3,842.6, 3,959.5] | 9,431.3 [9,364.3, 9,498.3] | 2.42× |
<!-- /GEN -->

The wounded in action rose <!-- GEN cell:load_demand|Wounded in action/run|Ratio|full -->3.63×<!-- /GEN -->, the first surgeries at R2B and R2E <!-- GEN cell:load_demand|R2B and R2E first surgeries/run|Ratio|full -->3.03×<!-- /GEN --> and the post-definitive recovery dispositions <!-- GEN cell:load_demand|Post-definitive recovery dispositions/run|Ratio|full -->2.42×<!-- /GEN --> from moderate to high intensity. The surgeries and dispositions are per-campaign counts from the shipped configuration's arms of the [R2B pre-open hold window](#r2b-pre-open-hold-window) and [evacuation policy](#evacuation-policy) experiments, and the total casualties are in the [comparative scenario analysis](#comparative-scenario-analysis).

<!-- GEN load_shortfall -->
| Pool | Moderate: shortfall per day | Moderate: utilisation | High: shortfall per day | High: utilisation |
|---|---|---|---|---|
| R2E operating theatres | 0.000 [−0.004, 0.005] | 26.5% [25.1%, 27.9%] | 0.079 [0.061, 0.097] | 76.6% [76.0%, 77.2%] |
| R2E holding beds | −0.002 [−0.019, 0.016] | 99.4% [99.2%, 99.5%] | 0.015 [−0.015, 0.044] | 100.0% [100.0%, 100.0%] |
| R2E intensive care | no queue forms | 86.4% [85.6%, 87.2%] | no queue forms | 97.6% [97.6%, 97.7%] |
| R2E post-operative overflow | −0.001 [−0.008, 0.007] | not applicable | 0.001 [−0.002, 0.004] | not applicable |
| R2E staging overflow | −0.029 [−0.061, 0.002] | not applicable | 7.861 [7.814, 7.908] | not applicable |
| Strategic evacuation backlog | −0.020 [−0.052, 0.013] | not applicable | 8.639 [8.558, 8.719] | not applicable |
<!-- /GEN -->

The shortfall per day is the slope of the 30-day block means over the late half of the campaign, the fit the stability classification uses, divided by the 30 days of a block; the estimator is defined in [Methods](Methods.md#classifying-stability). At moderate intensity every shortfall interval contains zero. At high intensity the R2E theatre queue grew by <!-- GEN cell:load_shortfall|R2E operating theatres|High: shortfall per day|full -->0.079 [0.061, 0.097]<!-- /GEN --> casualties a day, the staging overflow census by <!-- GEN cell:load_shortfall|R2E staging overflow|High: shortfall per day|full -->7.861 [7.814, 7.908]<!-- /GEN --> and the strategic evacuation backlog by <!-- GEN cell:load_shortfall|Strategic evacuation backlog|High: shortfall per day|full -->8.639 [8.558, 8.719]<!-- /GEN -->, while the intervals of the holding bed queue (<!-- GEN cell:load_shortfall|R2E holding beds|High: shortfall per day|full -->0.015 [−0.015, 0.044]<!-- /GEN -->) and the post-operative overflow census (<!-- GEN cell:load_shortfall|R2E post-operative overflow|High: shortfall per day|full -->0.001 [−0.002, 0.004]<!-- /GEN -->) contain zero. Over the closing 90 days the R2E theatres were in use <!-- GEN cell:load_shortfall|R2E operating theatres|Moderate: utilisation|full -->26.5% [25.1%, 27.9%]<!-- /GEN --> of the time at moderate intensity and <!-- GEN cell:load_shortfall|R2E operating theatres|High: utilisation|full -->76.6% [76.0%, 77.2%]<!-- /GEN --> at high, the holding beds <!-- GEN cell:load_shortfall|R2E holding beds|Moderate: utilisation|full -->99.4% [99.2%, 99.5%]<!-- /GEN --> and <!-- GEN cell:load_shortfall|R2E holding beds|High: utilisation|full -->100.0% [100.0%, 100.0%]<!-- /GEN -->, and the intensive care beds <!-- GEN cell:load_shortfall|R2E intensive care|Moderate: utilisation|full -->86.4% [85.6%, 87.2%]<!-- /GEN --> and <!-- GEN cell:load_shortfall|R2E intensive care|High: utilisation|full -->97.6% [97.6%, 97.7%]<!-- /GEN -->. The strategic evacuation backlog and the overflow censuses count casualties without a bed and have no establishment, so they carry no utilisation, and the intensive care beds carry no shortfall, no queue forming for them.

---

## Treated-Cohort Mortality

<small>[Return to Top](#contents)</small>

**Question.** What fraction of casualties who reach an R2B or R2E facility die of wounds, measured against the historical anchor of the campaign each configuration models? `[default, moderate_intensity and high_intensity · 360 d · 3 measurements of 10 replications · pooled, Student t]`

<!-- GEN dow_calibration -->
| Configuration | Historical anchor | Treated-cohort died-of-wounds rate | Post-operative share of risk | Replications |
|---|---|---|---|---|
| Shipped default | fitted to 0.46% (Ajax Bay, 30 days) | 0.554% [0.468%, 0.640%] | 25% | 30 |
| Moderate intensity | fitted to 0.46% (Ajax Bay, 30 days) | 0.557% [0.470%, 0.643%] | 24% | 30 |
| High intensity | at or below 3.40% (Okinawa, 82 days) | 5.437% [5.024%, 5.850%] | 20% | 30 |
<!-- /GEN -->

The Falklands anchor is the three deaths of wounds among over 650 combat casualties treated by the Ajax Bay field hospital and its forward stations at Teal Inlet and Fitzroy [[2]](#references), a 30-day campaign, to which the two Falklands-calibrated configurations are fitted. The Okinawa anchor is the 3.4% of casualties reaching a hospital alive who died of wounds over the 82 days of that campaign [[3]](#references), an upper bound for the high-intensity profile, which is not fitted. Each anchor describes a campaign of its own length, and the measurement here is over 360 days. The shipped default's pooled rate was <!-- GEN cell:dow_calibration|Shipped default|Treated-cohort died-of-wounds rate|full -->0.554% [0.468%, 0.640%]<!-- /GEN --> and moderate intensity's <!-- GEN cell:dow_calibration|Moderate intensity|Treated-cohort died-of-wounds rate|full -->0.557% [0.470%, 0.643%]<!-- /GEN -->, with post-operative care carrying <!-- GEN cell:dow_calibration|Shipped default|Post-operative share of risk|full -->25%<!-- /GEN --> of the shipped default's expected deaths; high intensity's was <!-- GEN cell:dow_calibration|High intensity|Treated-cohort died-of-wounds rate|full -->5.437% [5.024%, 5.850%]<!-- /GEN -->. The calibration check `tests/checks/check_dow_calibration.R` asserts each anchor at the length of the campaign it describes, 30 days for the Falklands configurations and 82 for high intensity.

---

## Forward Holding and Evacuation Levers

<small>[Return to Top](#contents)</small>

Each subsection varies one setting of the shipped configuration and reports the responses it moved. Experiments with several arms draw every arm from one control seed unless the subsection says otherwise, and the paired difference is reported where the arms are paired.

### R2B Pre-Open Hold Window

**Question.** What changes when a casualty needing surgery at R2B is held forward for a surgical section about to reopen rather than moved rearward at once? `[default, hold window 0 and 60 min · 360 d · 30 replications · campaign totals, paired difference]`

<!-- GEN hold_window -->
| Measure | Window 0 | Window 60 min | Difference |
| --- | --- | --- | --- |
| Casualties held at R2B | 0.00 | 107.07 | +107.07 [+104.04, +110.09] |
| R2B surgeries | 771.60 | 822.87 | +51.27 [+31.92, +70.62] |
| Diverted, team off shift | 1287.20 | 1193.50 | −93.70 [−131.51, −55.89] |
| Diverted, theatre busy | 220.33 | 231.43 | +11.10 [−8.72, +30.92] |
| R2E first surgeries | 1842.07 | 1801.07 | −41.00 [−105.33, +23.33] |
| R2E theatre entry deferred | 384.07 | 368.57 | −15.50 [−34.50, +3.50] |
| Died of wounds per run | 22.83 | 20.37 | −2.47 [−6.47, +1.53] |
| Total casualties | 5345.60 | 5359.40 | +13.80 [−99.85, +127.45] |
<!-- /GEN -->

![Forest plot of the paired difference in each of eight measures with its confidence interval](../images/paper_hold_window_effects.png)

The paired difference per campaign for each of the eight measures with its 95% confidence interval against a vertical line at zero.

A 60-minute window held <!-- GEN cell:hold_window|Casualties held at R2B|Window 60 min|full -->107.07<!-- /GEN --> casualties per campaign at R2B against none with a zero window. R2B surgeries differed by <!-- GEN cell:hold_window|R2B surgeries|Difference|full -->+51.27 [+31.92, +70.62]<!-- /GEN --> and diversions for an off-shift section by <!-- GEN cell:hold_window|Diverted, team off shift|Difference|full -->−93.70 [−131.51, −55.89]<!-- /GEN -->. The differences in diversions for a busy theatre were <!-- GEN cell:hold_window|Diverted, theatre busy|Difference|full -->+11.10 [−8.72, +30.92]<!-- /GEN -->, in R2E first surgeries <!-- GEN cell:hold_window|R2E first surgeries|Difference|full -->−41.00 [−105.33, +23.33]<!-- /GEN -->, in R2E theatre entry deferrals <!-- GEN cell:hold_window|R2E theatre entry deferred|Difference|full -->−15.50 [−34.50, +3.50]<!-- /GEN -->, in died of wounds <!-- GEN cell:hold_window|Died of wounds per run|Difference|full -->−2.47 [−6.47, +1.53]<!-- /GEN --> and in total casualties <!-- GEN cell:hold_window|Total casualties|Difference|full -->+13.80 [−99.85, +127.45]<!-- /GEN -->.

**High-intensity profile.** The same comparison under `high_intensity` `[high_intensity, hold window 0 and 60 min · 360 d · 30 replications · campaign totals, paired differences]`:

<!-- GEN hold_window_high -->
| Measure | Window 0 | Window 60 min | Difference |
| --- | --- | --- | --- |
| Casualties held at R2B | 0.00 | 338.47 | +338.47 [+334.36, +342.57] |
| R2B surgeries | 1745.00 | 1791.73 | +46.73 [+23.82, +69.65] |
| Diverted, team off shift | 4168.83 | 3830.60 | −338.23 [−399.33, −277.14] |
| Diverted, theatre busy | 1340.40 | 1358.57 | +18.17 [−23.49, +59.83] |
| R2E first surgeries | 6213.73 | 6153.27 | −60.47 [−147.11, +26.17] |
| R2E theatre entry deferred | 1629.47 | 1600.87 | −28.60 [−67.25, +10.05] |
| Died of wounds per run | 649.37 | 509.33 | −140.03 [−232.70, −47.37] |
| Total casualties | 12463.60 | 12356.87 | −106.73 [−256.08, +42.62] |
<!-- /GEN -->

The 60-minute window held <!-- GEN cell:hold_window_high|Casualties held at R2B|Window 60 min|full -->338.47<!-- /GEN --> casualties forward and changed diversions for a team off shift by <!-- GEN cell:hold_window_high|Diverted, team off shift|Difference|full -->−338.23 [−399.33, −277.14]<!-- /GEN -->.

### R2B Holding Capacity and Evacuation Threshold

**Question.** How do the R2B holding queue and the pools behind it respond to the R2B holding bed establishment and to the threshold at which convalescent casualties are evacuated early? `[default, 5 to 10 holding beds per unit crossed with a 0 to 7 day threshold · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN hold_threshold_beds -->
| R2B holding beds per unit | R2B hold mean queue | R2B hold utilisation | R2B reception overflow census | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|---|
| 5 (shipped) | 0.06 [0.03, 0.08] | 79.4% [79.1, 79.8] | 0.023 [0.004, 0.043] | 22.872 [21.384, 24.359] | 5.26 [4.84, 5.68] | 2050 [2024, 2075] | 20.4 [17.9, 22.9] |
| 7 | 0.12 [0.07, 0.16] | 83.7% [83.4, 83.9] | 0.047 [0.000, 0.096] | 20.666 [19.594, 21.738] | 5.27 [4.85, 5.69] | 2054 [2025, 2084] | 20.3 [17.8, 22.7] |
| 10 | 0.07 [0.01, 0.12] | 77.9% [77.6, 78.2] | 0.022 [0.010, 0.034] | 19.338 [18.025, 20.651] | 5.77 [5.45, 6.10] | 2068 [2041, 2096] | 24.5 [18.1, 30.8] |
<!-- /GEN -->

With the threshold disabled, raising the establishment from five to ten holding beds per unit moved the R2B holding queue from <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2B hold mean queue|full -->0.06 [0.03, 0.08]<!-- /GEN --> to <!-- GEN cell:hold_threshold_beds|10|R2B hold mean queue|full -->0.07 [0.01, 0.12]<!-- /GEN --> and R2B holding utilisation from <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2B hold utilisation|full -->79.4% [79.1, 79.8]<!-- /GEN --> to <!-- GEN cell:hold_threshold_beds|10|R2B hold utilisation|full -->77.9% [77.6, 78.2]<!-- /GEN -->. The R2E holding queue was <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2E hold mean queue|full -->22.872 [21.384, 24.359]<!-- /GEN --> at five beds and <!-- GEN cell:hold_threshold_beds|10|R2E hold mean queue|full -->19.338 [18.025, 20.651]<!-- /GEN --> at ten, and the R2E post-operative overflow census <!-- GEN cell:hold_threshold_beds|5 (shipped)|R2E post-operative overflow census|full -->5.26 [4.84, 5.68]<!-- /GEN --> and <!-- GEN cell:hold_threshold_beds|10|R2E post-operative overflow census|full -->5.77 [5.45, 6.10]<!-- /GEN --> Returns to duty were <!-- GEN cell:hold_threshold_beds|5 (shipped)|Returns to duty|full -->2050 [2024, 2075]<!-- /GEN --> at five beds and <!-- GEN cell:hold_threshold_beds|10|Returns to duty|full -->2068 [2041, 2096]<!-- /GEN --> at ten, and deaths of wounds <!-- GEN cell:hold_threshold_beds|5 (shipped)|Deaths of wounds|full -->20.4 [17.9, 22.9]<!-- /GEN --> and <!-- GEN cell:hold_threshold_beds|10|Deaths of wounds|full -->24.5 [18.1, 30.8]<!-- /GEN -->.

<!-- GEN hold_threshold_threshold -->
| Evacuation threshold | R2B hold mean queue | R2B hold utilisation | R2B reception overflow census | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|---|
| Disabled (shipped) | 0.06 [0.03, 0.08] | 79.4% [79.1, 79.8] | 0.023 [0.004, 0.043] | 22.872 [21.384, 24.359] | 5.26 [4.84, 5.68] | 2050 [2024, 2075] | 20.4 [17.9, 22.9] |
| 1 day | 0.03 [0.02, 0.03] | 52.7% [51.4, 54.1] | 0.054 [0.012, 0.095] | 23.672 [22.471, 24.873] | 5.56 [5.25, 5.87] | 2033 [2001, 2064] | 20.8 [18.4, 23.2] |
| 3 days | 0.05 [0.03, 0.07] | 69.3% [68.7, 70.0] | 0.087 [0.000, 0.191] | 23.709 [22.435, 24.984] | 5.60 [5.19, 6.01] | 2046 [2018, 2073] | 23.0 [18.4, 27.7] |
| 5 days (mode) | 0.07 [0.05, 0.10] | 74.5% [74.0, 75.0] | 0.055 [0.019, 0.092] | 24.325 [22.961, 25.689] | 5.63 [5.22, 6.04] | 2035 [2005, 2065] | 24.9 [17.9, 32.0] |
| 7 days | 0.08 [0.06, 0.10] | 76.3% [75.8, 76.8] | 0.014 [0.005, 0.023] | 22.652 [21.324, 23.980] | 5.33 [4.96, 5.70] | 2043 [2011, 2075] | 20.6 [18.3, 22.9] |
<!-- /GEN -->

![Eight panels of R2B and R2E queue and utilisation, returns to duty and died of wounds against the evacuation threshold](../images/r2b_hold_threshold_sweep.png)

R2B and R2E queue and utilisation, returns to duty and died of wounds against the evacuation threshold in days, one line per swept bed count, with a 95% confidence ribbon.

With five beds per unit, a one-day threshold moved the R2B holding queue to <!-- GEN cell:hold_threshold_threshold|1 day|R2B hold mean queue|full -->0.03 [0.02, 0.03]<!-- /GEN --> and the R2E holding queue to <!-- GEN cell:hold_threshold_threshold|1 day|R2E hold mean queue|full -->23.672 [22.471, 24.873]<!-- /GEN -->, and the R2E post-operative overflow census to <!-- GEN cell:hold_threshold_threshold|1 day|R2E post-operative overflow census|full -->5.56 [5.25, 5.87]<!-- /GEN -->. A three-day threshold gave <!-- GEN cell:hold_threshold_threshold|3 days|R2B hold mean queue|full -->0.05 [0.03, 0.07]<!-- /GEN -->, <!-- GEN cell:hold_threshold_threshold|3 days|R2E hold mean queue|full -->23.709 [22.435, 24.984]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|3 days|R2E post-operative overflow census|full -->5.60 [5.19, 6.01]<!-- /GEN --> on the same three responses. At five and seven days the R2B holding queue was <!-- GEN cell:hold_threshold_threshold|5 days (mode)|R2B hold mean queue|full -->0.07 [0.05, 0.10]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|7 days|R2B hold mean queue|full -->0.08 [0.06, 0.10]<!-- /GEN -->. Returns to duty were <!-- GEN cell:hold_threshold_threshold|Disabled (shipped)|Returns to duty|full -->2050 [2024, 2075]<!-- /GEN --> with the threshold disabled and <!-- GEN cell:hold_threshold_threshold|1 day|Returns to duty|full -->2033 [2001, 2064]<!-- /GEN --> at one day, and deaths of wounds <!-- GEN cell:hold_threshold_threshold|Disabled (shipped)|Deaths of wounds|full -->20.4 [17.9, 22.9]<!-- /GEN --> and <!-- GEN cell:hold_threshold_threshold|1 day|Deaths of wounds|full -->20.8 [18.4, 23.2]<!-- /GEN -->. The full grid, including returns to duty and deaths of wounds at every point, is in `data/sweeps/r2b_hold_threshold_sweep.csv`.

**High-intensity profile.** The same grid under `high_intensity` `[high_intensity, 5 to 10 beds per unit and 0 to 7 days · 360 d · 30 replications · pool totals, closing 90 d]`:

<!-- GEN hold_threshold_beds_high -->
| R2B holding beds per unit | R2B hold mean queue | R2B hold utilisation | R2B reception overflow census | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|---|
| 5 (shipped) | 0.36 [0.28, 0.43] | 81.8% [81.3, 82.2] | 0.251 [0.212, 0.289] | 54.952 [53.023, 56.881] | 20.17 [19.97, 20.38] | 2859 [2826, 2892] | 506.2 [461.5, 550.8] |
| 7 | 0.50 [0.42, 0.57] | 85.8% [85.6, 86.0] | 0.316 [0.262, 0.371] | 51.644 [50.036, 53.252] | 19.74 [19.44, 20.05] | 2845 [2812, 2878] | 607.5 [524.4, 690.6] |
| 10 | 0.36 [0.26, 0.46] | 80.3% [80.0, 80.6] | 0.267 [0.209, 0.326] | 49.064 [47.341, 50.787] | 19.86 [19.62, 20.10] | 2852 [2819, 2886] | 567.1 [497.0, 637.2] |
<!-- /GEN -->

<!-- GEN hold_threshold_threshold_high -->
| Evacuation threshold | R2B hold mean queue | R2B hold utilisation | R2B reception overflow census | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Deaths of wounds |
|---|---|---|---|---|---|---|---|
| Disabled (shipped) | 0.36 [0.28, 0.43] | 81.8% [81.3, 82.2] | 0.251 [0.212, 0.289] | 54.952 [53.023, 56.881] | 20.17 [19.97, 20.38] | 2859 [2826, 2892] | 506.2 [461.5, 550.8] |
| 1 day | 0.14 [0.12, 0.16] | 60.1% [59.2, 61.0] | 0.323 [0.272, 0.375] | 57.961 [56.178, 59.745] | 19.73 [19.53, 19.93] | 2847 [2821, 2874] | 587.6 [500.7, 674.4] |
| 3 days | 0.22 [0.19, 0.25] | 74.0% [73.3, 74.6] | 0.326 [0.264, 0.387] | 55.848 [54.232, 57.464] | 19.68 [19.34, 20.02] | 2871 [2839, 2903] | 624.1 [514.9, 733.2] |
| 5 days (mode) | 0.26 [0.22, 0.29] | 77.4% [76.9, 77.9] | 0.332 [0.261, 0.403] | 54.519 [52.539, 56.499] | 19.97 [19.72, 20.21] | 2869 [2837, 2901] | 573.9 [512.5, 635.2] |
| 7 days | 0.29 [0.23, 0.34] | 79.3% [79.0, 79.7] | 0.345 [0.271, 0.419] | 54.004 [51.825, 56.182] | 19.71 [19.43, 19.99] | 2852 [2827, 2878] | 592.3 [520.5, 664.1] |
<!-- /GEN -->

At 10 holding beds per unit the R2B holding queue was <!-- GEN cell:hold_threshold_beds_high|10|R2B hold mean queue|full -->0.36 [0.26, 0.46]<!-- /GEN --> against <!-- GEN cell:hold_threshold_beds_high|5 (shipped)|R2B hold mean queue|full -->0.36 [0.28, 0.43]<!-- /GEN --> at the shipped five. A one-day evacuation threshold left the R2E holding queue at <!-- GEN cell:hold_threshold_threshold_high|1 day|R2E hold mean queue|full -->57.961 [56.178, 59.745]<!-- /GEN --> against <!-- GEN cell:hold_threshold_threshold_high|Disabled (shipped)|R2E hold mean queue|full -->54.952 [53.023, 56.881]<!-- /GEN --> with the threshold disabled.

![Eight panels of R2B and R2E queue and utilisation, returns to duty and died of wounds against the evacuation threshold at high intensity](../images/r2b_hold_threshold_sweep_high_intensity.png)

The panels of the first figure of this section under the high-intensity profile.

### Transport Fleet Size

**Question.** At what fleet size does the transport queue form, at each casualty intensity? `[default and high_intensity · 360 d · 30 replications · pool totals, closing 90 d]`

The first table is the shipped moderate-intensity configuration and the second the high-intensity profile.

<!-- GEN transport -->
| Fleet size | Ambulance mean queue | Truck mean queue |
|---|---|---|
| 1 | 7.6600 [4.1208, 11.1993] | 0.0599 [0.0330, 0.0868] |
| 2 | 1.1869 [0.7669, 1.6069] | 0.0055 [0.0020, 0.0089] |
| 3 (current ambulance) | 0.1471 [0.0849, 0.2093] | 0.0154 [0.0000, 0.0369] |
| 4 (current truck) | 0.1214 [0.0000, 0.2904] | 0.0006 [0.0000, 0.0014] |
| 5 | 0.0200 [0.0098, 0.0302] | not swept |
<!-- /GEN -->

![Four panels of mean queue and utilisation against fleet size for the ambulance and truck fleets](../images/transport_capacity_margin_by_fleet_size.png)

Mean queue and mean utilisation against fleet size for the ambulance and truck fleets under the shipped configuration, each with a 95% confidence ribbon and a dashed vertical line at the current establishment size.

<!-- GEN transport_high -->
| Fleet size | Ambulance mean queue | Truck mean queue |
|---|---|---|
| 1 | 2080.6801 [1946.9363, 2214.4240] | 0.1164 [0.0914, 0.1413] |
| 2 | 9.8669 [8.2616, 11.4722] | 0.0067 [0.0055, 0.0078] |
| 3 (current ambulance) | 1.9345 [1.5620, 2.3070] | 0.0010 [0.0007, 0.0013] |
| 4 (current truck) | 0.7159 [0.5711, 0.8607] | 0.0001 [0.0000, 0.0002] |
| 5 | 0.2132 [0.1261, 0.3004] | not swept |
<!-- /GEN -->

![Four panels of mean queue and utilisation against fleet size under the high intensity profile](../images/transport_capacity_margin_by_fleet_size_high_intensity.png)

The same measures under the `high_intensity` profile, each with a 95% confidence ribbon and a dashed vertical line at the current establishment size.

Under the shipped configuration a single ambulance queued <!-- GEN cell:transport|1|Ambulance mean queue|mean -->7.6600<!-- /GEN --> casualties (interval <!-- GEN cell:transport|1|Ambulance mean queue|ci -->[4.1208, 11.1993]<!-- /GEN -->), two queued <!-- GEN cell:transport|2|Ambulance mean queue|mean -->1.1869<!-- /GEN --> and the shipped three <!-- GEN cell:transport|3 (current ambulance)|Ambulance mean queue|mean -->0.1471<!-- /GEN -->. Under the high-intensity profile the corresponding ambulance queues were <!-- GEN cell:transport_high|1|Ambulance mean queue|mean -->2080.6801<!-- /GEN -->, <!-- GEN cell:transport_high|2|Ambulance mean queue|mean -->9.8669<!-- /GEN --> and <!-- GEN cell:transport_high|3 (current ambulance)|Ambulance mean queue|mean -->1.9345<!-- /GEN -->, and the shipped four trucks queued <!-- GEN cell:transport_high|4 (current truck)|Truck mean queue|mean -->0.0001<!-- /GEN -->.

#### Utilisation of Shared Fleets and Integral Evacuation Elements

**Question.** How heavily is each transport holder used, the shared brigade fleets and the evacuation elements integral to R2B and R2E? `[default and high_intensity · 360 d · 30 replications · pool totals, closing 90 d, shipped establishment, not swept]`

Utilisation is the time-weighted share of the established units in use over the closing 90 days. An evacuation crew is two medics seized together, so a crew is measured on its lead medic.

<!-- GEN transport_holders -->
| Holder | Asset | Moderate intensity mean queue | Moderate intensity utilisation | High intensity mean queue | High intensity utilisation |
|---|---|---|---|---|---|
| PMV Ambulance fleet | Shared | 0.146 [0.084, 0.209] | 23.8% [23.1%, 24.6%] | 1.919 [1.543, 2.295] | 51.9% [50.9%, 53.0%] |
| HX2 40M fleet | Shared | 0.001 [0.000, 0.001] | 2.6% [2.5%, 2.8%] | 0.000 [0.000, 0.000] | 5.8% [5.6%, 5.9%] |
| R2B evacuation crews | Integral | 0.052 [0.030, 0.073] | 13.1% [12.4%, 13.8%] | 0.611 [0.546, 0.676] | 42.2% [41.3%, 43.0%] |
| R2E evacuation sections | Integral | 0.000 [0.000, 0.000] | 0.0% [0.0%, 0.0%] | 0.000 [0.000, 0.000] | 1.0% [0.9%, 1.1%] |
<!-- /GEN -->

At moderate intensity the R2B evacuation crews were in use for <!-- GEN cell:transport_holders|R2B evacuation crews|Moderate intensity utilisation|mean -->13.1%<!-- /GEN --> of the window and the ambulance fleet for <!-- GEN cell:transport_holders|PMV Ambulance fleet|Moderate intensity utilisation|mean -->23.8%<!-- /GEN -->. At high intensity the crews were in use for <!-- GEN cell:transport_holders|R2B evacuation crews|High intensity utilisation|mean -->42.2%<!-- /GEN --> (interval <!-- GEN cell:transport_holders|R2B evacuation crews|High intensity utilisation|ci -->[41.3%, 43.0%]<!-- /GEN -->) and queued <!-- GEN cell:transport_holders|R2B evacuation crews|High intensity mean queue|mean -->0.611<!-- /GEN --> casualties, the ambulance fleet was in use for <!-- GEN cell:transport_holders|PMV Ambulance fleet|High intensity utilisation|mean -->51.9%<!-- /GEN --> and the truck fleet for <!-- GEN cell:transport_holders|HX2 40M fleet|High intensity utilisation|mean -->5.8%<!-- /GEN -->. The R2E evacuation sections were in use for <!-- GEN cell:transport_holders|R2E evacuation sections|High intensity utilisation|mean -->1.0%<!-- /GEN --> at high intensity.

### Forward Holding of Post-Operative Intensive Care

**Question.** What changes when a casualty operated on at R2B is held there for part of their post-operative intensive care, for a stability window or while R2E intensive care is saturated? `[default, seven forward holding arms · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN forward_hold -->
| Forward holding rule | R2E post-operative overflow census | R2B ICU utilisation | R2E ICU utilisation | Post-definitive care in ICU | Died of wounds per run |
|---|---|---|---|---|---|
| Off (current) | 5.26 [4.84, 5.68] | 0.9% | 86.3% | 35.5 [34.8, 36.1] | 20.37 [17.86, 22.88] |
| 120 min | 5.23 [4.82, 5.63] | 5.8% | 85.3% | 35.6 [34.9, 36.2] | 22.13 [19.32, 24.94] |
| 240 min | 5.18 [4.82, 5.53] | 10.6% | 86.1% | 36.3 [35.5, 37.1] | 22.83 [18.56, 27.11] |
| 480 min | 4.95 [4.62, 5.29] | 19.5% | 85.3% | 36.7 [36.0, 37.5] | 22.03 [19.39, 24.68] |
| 1,440 min | 4.09 [3.71, 4.47] | 44.1% | 82.7% | 41.6 [40.8, 42.4] | 26.00 [19.78, 32.22] |
| Capacity only | 5.33 [4.96, 5.70] | 11.4% | 88.1% | 36.8 [36.1, 37.5] | 20.97 [18.09, 23.84] |
| 240 min + capacity | 4.65 [4.32, 4.97] | 19.4% | 86.9% | 39.1 [38.2, 40.0] | 21.43 [17.56, 25.30] |
<!-- /GEN -->

![Five stacked panels against the forward holding rule](../images/r2b_forward_hold_frontier.png)

R2E post-operative overflow census, R2B and R2E intensive care utilisation, the share of post-definitive care delivered in intensive care and the died-of-wounds count under each forward holding arm, each with a 95% confidence ribbon. The windows apply to damage control and single-stage casualties alike.

The R2E post-operative overflow census was <!-- GEN cell:forward_hold|Off (current)|R2E post-operative overflow census|full -->5.26 [4.84, 5.68]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold|240 min|R2E post-operative overflow census|full -->5.18 [4.82, 5.53]<!-- /GEN --> with a 240-minute window. R2B intensive care utilisation rose from <!-- GEN cell:forward_hold|Off (current)|R2B ICU utilisation|full -->0.9%<!-- /GEN --> with no forward holding to <!-- GEN cell:forward_hold|1,440 min|R2B ICU utilisation|full -->44.1%<!-- /GEN --> at a 1,440-minute window while R2E intensive care utilisation moved from <!-- GEN cell:forward_hold|Off (current)|R2E ICU utilisation|full -->86.3%<!-- /GEN --> to <!-- GEN cell:forward_hold|1,440 min|R2E ICU utilisation|full -->82.7%<!-- /GEN -->. Post-definitive care in an intensive care bed rose from <!-- GEN cell:forward_hold|Off (current)|Post-definitive care in ICU|full -->35.5 [34.8, 36.1]<!-- /GEN --> to <!-- GEN cell:forward_hold|1,440 min|Post-definitive care in ICU|full -->41.6 [40.8, 42.4]<!-- /GEN -->, and died of wounds per run was <!-- GEN cell:forward_hold|Off (current)|Died of wounds per run|full -->20.37 [17.86, 22.88]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold|1,440 min|Died of wounds per run|full -->26.00 [19.78, 32.22]<!-- /GEN --> at 1,440 minutes. Holding on capacity grounds alone gave a post-operative overflow census of <!-- GEN cell:forward_hold|Capacity only|R2E post-operative overflow census|full -->5.33 [4.96, 5.70]<!-- /GEN -->, and a 240-minute window with the capacity trigger gave <!-- GEN cell:forward_hold|240 min + capacity|R2E post-operative overflow census|full -->4.65 [4.32, 4.97]<!-- /GEN -->.

**High-intensity profile.** The same seven arms under `high_intensity` `[high_intensity, seven forward holding arms · 360 d · 30 replications · pool totals, closing 90 d]`:

<!-- GEN forward_hold_high -->
| Forward holding rule | R2E post-operative overflow census | R2B ICU utilisation | R2E ICU utilisation | Post-definitive care in ICU | Died of wounds per run |
|---|---|---|---|---|---|
| Off (current) | 20.16 [19.96, 20.36] | 9.8% | 97.6% | 11.0 [10.9, 11.2] | 509.33 [464.71, 553.96] |
| 120 min | 19.31 [18.96, 19.65] | 19.6% | 97.4% | 11.4 [11.2, 11.5] | 576.27 [495.55, 656.98] |
| 240 min | 19.02 [18.79, 19.26] | 29.8% | 97.5% | 11.4 [11.2, 11.6] | 597.77 [520.75, 674.78] |
| 480 min | 18.32 [18.12, 18.52] | 44.7% | 97.4% | 11.6 [11.4, 11.8] | 584.80 [524.45, 645.15] |
| 1,440 min | 15.45 [15.18, 15.73] | 72.3% | 97.0% | 13.2 [13.0, 13.5] | 644.13 [548.22, 740.05] |
| Capacity only | 18.48 [18.25, 18.71] | 41.7% | 97.6% | 12.2 [12.1, 12.3] | 632.43 [544.42, 720.45] |
| 240 min + capacity | 17.56 [17.35, 17.78] | 52.6% | 97.5% | 12.9 [12.7, 13.1] | 583.27 [520.23, 646.30] |
<!-- /GEN -->

![Five stacked panels against the forward holding rule at high intensity](../images/r2b_forward_hold_frontier_high_intensity.png)

The panels of the figure above under the high-intensity profile.

The R2E post-operative overflow census was <!-- GEN cell:forward_hold_high|Off (current)|R2E post-operative overflow census|full -->20.16 [19.96, 20.36]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold_high|240 min|R2E post-operative overflow census|full -->19.02 [18.79, 19.26]<!-- /GEN --> with a 240-minute window. R2E intensive care utilisation was <!-- GEN cell:forward_hold_high|Off (current)|R2E ICU utilisation|full -->97.6%<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold_high|1,440 min|R2E ICU utilisation|full -->97.0%<!-- /GEN --> at a 1,440-minute window, while R2B intensive care utilisation rose from <!-- GEN cell:forward_hold_high|Off (current)|R2B ICU utilisation|full -->9.8%<!-- /GEN --> to <!-- GEN cell:forward_hold_high|1,440 min|R2B ICU utilisation|full -->72.3%<!-- /GEN -->. Died of wounds per run was <!-- GEN cell:forward_hold_high|Off (current)|Died of wounds per run|full -->509.33 [464.71, 553.96]<!-- /GEN --> with no forward holding and <!-- GEN cell:forward_hold_high|Capacity only|Died of wounds per run|full -->632.43 [544.42, 720.45]<!-- /GEN --> on the capacity trigger alone.

### Post-Operative Intensive Care Gate

**Question.** What does the rationing rule that defers lower-priority surgery when intensive care is saturated change? `[default, gate on and off · 360 d · 30 replications · paired, closing 90 d]`

<!-- GEN icu_gate -->
| Measure | Without the rule | With the rule | Paired difference |
|---|---|---|---|
| R2E ICU utilisation (%) | 85.6 [84.8, 86.4] | 86.4 [85.6, 87.2] | +0.8 [−0.2, +1.8] |
| Died of wounds per run | 20.13 [17.47, 22.79] | 20.73 [17.94, 23.52] | +0.60 [−2.67, +3.87] |
| Total casualties | 5,351.3 [5,290.5, 5,412.1] | 5,362.3 [5,296.2, 5,428.3] | +11.0 [−67.8, +89.7] |
<!-- /GEN -->

![Four panels comparing the post-operative intensive care gate disabled and enabled](../images/icu_gate.png)

Intensive care occupancy, deaths of wounds and the casualties taking each post-operative pathway with the gate disabled and enabled, each point with its 95% confidence interval.

R2E intensive care utilisation was <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|Without the rule|full -->85.6 [84.8, 86.4]<!-- /GEN --> without the rule and <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|With the rule|full -->86.4 [85.6, 87.2]<!-- /GEN --> with it, a paired difference of <!-- GEN cell:icu_gate|R2E ICU utilisation (%)|Paired difference|full -->+0.8 [−0.2, +1.8]<!-- /GEN --> percentage points. Died of wounds per run were <!-- GEN cell:icu_gate|Died of wounds per run|Without the rule|full -->20.13 [17.47, 22.79]<!-- /GEN --> and <!-- GEN cell:icu_gate|Died of wounds per run|With the rule|full -->20.73 [17.94, 23.52]<!-- /GEN -->, a paired difference of <!-- GEN cell:icu_gate|Died of wounds per run|Paired difference|full -->+0.60 [−2.67, +3.87]<!-- /GEN -->.

<!-- GEN icu_gate_pathways -->
| Recovery pathway | Casualty-replications | Died of wounds | Rate |
|---|---|---|---|
| Intensive care bed | 34,316 | 170 | 0.50% |
| Holding bed | 1,319 | 8 | 0.61% |
<!-- /GEN -->

Where the rule was in force, casualties recovering in a holding bed died of wounds at <!-- GEN cell:icu_gate_pathways|Holding bed|Rate|full -->0.61%<!-- /GEN --> and those recovering in an intensive care bed at <!-- GEN cell:icu_gate_pathways|Intensive care bed|Rate|full -->0.50%<!-- /GEN -->, pooled over the replications of that arm.

**High-intensity profile.** The same comparison under `high_intensity` `[high_intensity, gate disabled and enabled · 360 d · 30 replications · campaign totals, paired differences]`:

<!-- GEN icu_gate_high -->
| Measure | Without the rule | With the rule | Paired difference |
|---|---|---|---|
| R2E ICU utilisation (%) | 97.4 [97.3, 97.6] | 97.6 [97.5, 97.7] | +0.2 [+0.0, +0.4] |
| Died of wounds per run | 561.63 [487.33, 635.94] | 509.33 [464.71, 553.96] | −52.30 [−139.07, +34.47] |
| Total casualties | 12,311.3 [12,178.6, 12,444.0] | 12,356.9 [12,252.4, 12,461.3] | +45.6 [−147.9, +239.0] |
<!-- /GEN -->

<!-- GEN icu_gate_pathways_high -->
| Recovery pathway | Casualty-replications | Died of wounds | Rate |
|---|---|---|---|
| Intensive care bed | 104,827 | 4,759 | 4.54% |
| Holding bed | 364 | 4 | 1.10% |
<!-- /GEN -->

The paired difference in deaths of wounds per run with the rule in force was <!-- GEN cell:icu_gate_high|Died of wounds per run|Paired difference|full -->−52.30 [−139.07, +34.47]<!-- /GEN -->. Casualties recovering in a holding bed died of wounds at <!-- GEN cell:icu_gate_pathways_high|Holding bed|Rate|full -->1.10%<!-- /GEN --> and those recovering in an intensive care bed at <!-- GEN cell:icu_gate_pathways_high|Intensive care bed|Rate|full -->4.54%<!-- /GEN -->.

### Evacuation Policy

**Question.** What happens to returns to duty, mortality and the R2E pools when the days a casualty may recover in theatre before evacuation is set across its doctrinal range? `[default, evacuation policy 15 to 60 days · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN policy -->
| Response | 15 d | 21 d (shipped) | 30 d | 45 d | 60 d |
|---|---|---|---|---|---|
| R2E hold occupancy (%) | 95.9 [95.3, 96.4] | 99.4 [99.2, 99.5] | 99.9 [99.9, 100.0] | 100.0 [100.0, 100.0] | 100.0 [100.0, 100.0] |
| R2E hold mean queue | 9.03 [8.30, 9.75] | 22.80 [21.32, 24.28] | 41.87 [39.50, 44.25] | 81.51 [77.83, 85.18] | 144.20 [138.67, 149.73] |
| R2E ICU occupancy (%) | 86.6 [86.0, 87.2] | 86.4 [85.6, 87.2] | 87.3 [86.4, 88.2] | 86.5 [85.7, 87.2] | 86.8 [85.9, 87.6] |
| R2E post-operative overflow census | 4.70 [4.37, 5.03] | 5.30 [4.87, 5.73] | 6.05 [5.63, 6.48] | 5.91 [5.58, 6.23] | 6.01 [5.69, 6.34] |
| R2E staging overflow census | 26.6 [22.5, 30.8] | 27.4 [22.8, 31.9] | 29.8 [24.7, 34.8] | 21.5 [18.2, 24.9] | 14.9 [13.1, 16.6] |
| Post-definitive ICU access (%) | 35.3 [34.6, 36.1] | 35.4 [34.7, 36.1] | 35.4 [34.6, 36.1] | 35.5 [34.7, 36.3] | 35.7 [35.0, 36.4] |
| In-theatre share (%) | 27.1 [26.7, 27.5] | 40.4 [39.8, 40.9] | 48.5 [47.8, 49.1] | 59.4 [58.8, 60.0] | 71.2 [70.8, 71.6] |
| Returns to duty | 1575.0 [1548.5, 1601.5] | 2049.6 [2024.4, 2074.7] | 2346.7 [2307.6, 2385.8] | 2688.1 [2649.8, 2726.3] | 3039.0 [2989.5, 3088.6] |
| Died of wounds | 22.00 [19.14, 24.86] | 20.73 [17.94, 23.52] | 20.47 [18.10, 22.83] | 19.93 [17.02, 22.85] | 19.70 [16.91, 22.49] |
| Never evacuated by horizon | 45.0 [33.4, 56.7] | 33.3 [25.4, 41.1] | 33.2 [24.2, 42.1] | 23.8 [17.9, 29.8] | 16.7 [12.4, 21.0] |
| Mean evacuation wait (d) | 4.86 [4.53, 5.19] | 5.04 [4.50, 5.58] | 5.05 [4.67, 5.43] | 4.61 [4.30, 4.93] | 4.18 [4.06, 4.30] |
| Role 4 peak beds | 228.5 [222.9, 234.1] | 211.2 [207.6, 214.7] | 197.5 [193.1, 201.9] | 178.3 [174.3, 182.3] | 153.1 [148.9, 157.2] |
<!-- /GEN -->

![Six panels of returns to duty, deaths of wounds, in-theatre share and R2E queues against evacuation policy days](../images/policy_sweep.png)

Six responses against the evacuation policy in days, each with its 95% confidence interval and the shipped policy marked by a dashed line.

At the shipped 21-day policy the R2E holding queue was <!-- GEN cell:policy|R2E hold mean queue|21 d (shipped)|full -->22.80 [21.32, 24.28]<!-- /GEN --> and its occupancy <!-- GEN cell:policy|R2E hold occupancy (%)|21 d (shipped)|full -->99.4 [99.2, 99.5]<!-- /GEN -->. At 30 days the holding occupancy was <!-- GEN cell:policy|R2E hold occupancy (%)|30 d|full -->99.9 [99.9, 100.0]<!-- /GEN --> and the queue <!-- GEN cell:policy|R2E hold mean queue|30 d|full -->41.87 [39.50, 44.25]<!-- /GEN -->; at 45 and 60 days the occupancy was <!-- GEN cell:policy|R2E hold occupancy (%)|45 d|full -->100.0 [100.0, 100.0]<!-- /GEN --> and <!-- GEN cell:policy|R2E hold occupancy (%)|60 d|full -->100.0 [100.0, 100.0]<!-- /GEN -->. Returns to duty per campaign were <!-- GEN cell:policy|Returns to duty|15 d|full -->1575.0 [1548.5, 1601.5]<!-- /GEN --> at 15 days, <!-- GEN cell:policy|Returns to duty|21 d (shipped)|full -->2049.6 [2024.4, 2074.7]<!-- /GEN --> at 21, <!-- GEN cell:policy|Returns to duty|30 d|full -->2346.7 [2307.6, 2385.8]<!-- /GEN --> at 30, <!-- GEN cell:policy|Returns to duty|45 d|full -->2688.1 [2649.8, 2726.3]<!-- /GEN --> at 45 and <!-- GEN cell:policy|Returns to duty|60 d|full -->3039.0 [2989.5, 3088.6]<!-- /GEN --> at 60. Died of wounds were <!-- GEN cell:policy|Died of wounds|15 d|full -->22.00 [19.14, 24.86]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|21 d (shipped)|full -->20.73 [17.94, 23.52]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|30 d|full -->20.47 [18.10, 22.83]<!-- /GEN -->, <!-- GEN cell:policy|Died of wounds|45 d|full -->19.93 [17.02, 22.85]<!-- /GEN --> and <!-- GEN cell:policy|Died of wounds|60 d|full -->19.70 [16.91, 22.49]<!-- /GEN --> across the five policies. The realised in-theatre share was <!-- GEN cell:policy|In-theatre share (%)|15 d|full -->27.1 [26.7, 27.5]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|21 d (shipped)|full -->40.4 [39.8, 40.9]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|30 d|full -->48.5 [47.8, 49.1]<!-- /GEN -->, <!-- GEN cell:policy|In-theatre share (%)|45 d|full -->59.4 [58.8, 60.0]<!-- /GEN --> and <!-- GEN cell:policy|In-theatre share (%)|60 d|full -->71.2 [70.8, 71.6]<!-- /GEN --> percent. Paired differences against the shipped policy are in `data/policy/policy_sweep_paired.csv`.

### R2E Holding Establishment

**Question.** What does enlarging the R2E holding establishment change at the shipped evacuation policy? `[default, 21-day policy, 30 to 90 holding beds · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN establishment -->
| Response | 30 beds (shipped) | 45 beds | 60 beds | 90 beds |
|---|---|---|---|---|
| R2E hold occupancy (%) | 99.4 [99.2, 99.5] | 97.0 [96.5, 97.6] | 94.1 [93.2, 94.9] | 82.7 [80.5, 84.9] |
| R2E hold mean queue | 22.80 [21.32, 24.28] | 17.24 [16.05, 18.43] | 13.48 [12.13, 14.82] | 5.58 [4.26, 6.91] |
| R2E post-operative overflow census | 5.30 [4.87, 5.73] | 4.59 [4.12, 5.07] | 4.01 [3.53, 4.49] | 1.89 [1.44, 2.34] |
| R2E staging overflow census | 27.4 [22.8, 31.9] | 25.0 [16.2, 33.7] | 23.3 [14.8, 31.8] | 8.4 [5.3, 11.6] |
| Post-definitive ICU access (%) | 35.4 [34.7, 36.1] | 35.8 [35.0, 36.6] | 35.5 [34.8, 36.2] | 35.4 [34.8, 36.1] |
| In-theatre share (%) | 40.4 [39.8, 40.9] | 40.4 [39.9, 40.9] | 39.7 [39.2, 40.2] | 40.4 [40.0, 40.9] |
| Returns to duty | 2049.6 [2024.4, 2074.7] | 2043.1 [2013.9, 2072.4] | 2017.4 [1991.9, 2042.8] | 2069.1 [2045.6, 2092.5] |
| Died of wounds | 20.7 [17.9, 23.5] | 21.4 [18.2, 24.6] | 23.3 [19.8, 26.8] | 22.0 [17.5, 26.4] |
| Never evacuated by horizon | 33.3 [25.4, 41.1] | 40.2 [25.5, 54.9] | 40.6 [30.4, 50.9] | 36.5 [29.0, 44.0] |
| Role 4 peak beds | 211.2 [207.6, 214.7] | 207.9 [203.4, 212.5] | 211.3 [206.7, 215.9] | 211.2 [206.3, 216.0] |
<!-- /GEN -->

![Six panels of campaign responses against R2E holding beds](../images/establishment_sweep.png)

The same six responses against the R2E holding bed establishment at the shipped evacuation policy, with the shipped establishment marked by a dashed line.

Holding occupancy was <!-- GEN cell:establishment|R2E hold occupancy (%)|30 beds (shipped)|full -->99.4 [99.2, 99.5]<!-- /GEN --> at the shipped 30 beds, <!-- GEN cell:establishment|R2E hold occupancy (%)|45 beds|full -->97.0 [96.5, 97.6]<!-- /GEN --> at 45, <!-- GEN cell:establishment|R2E hold occupancy (%)|60 beds|full -->94.1 [93.2, 94.9]<!-- /GEN --> at 60 and <!-- GEN cell:establishment|R2E hold occupancy (%)|90 beds|full -->82.7 [80.5, 84.9]<!-- /GEN --> at 90 percent. The R2E holding queue was <!-- GEN cell:establishment|R2E hold mean queue|30 beds (shipped)|full -->22.80 [21.32, 24.28]<!-- /GEN --> at 30 beds and <!-- GEN cell:establishment|R2E hold mean queue|45 beds|full -->17.24 [16.05, 18.43]<!-- /GEN --> at 45. Returns to duty per campaign were <!-- GEN cell:establishment|Returns to duty|30 beds (shipped)|full -->2049.6 [2024.4, 2074.7]<!-- /GEN --> at 30 beds and <!-- GEN cell:establishment|Returns to duty|90 beds|full -->2069.1 [2045.6, 2092.5]<!-- /GEN --> at 90, and died of wounds <!-- GEN cell:establishment|Died of wounds|30 beds (shipped)|full -->20.7 [17.9, 23.5]<!-- /GEN --> and <!-- GEN cell:establishment|Died of wounds|90 beds|full -->22.0 [17.5, 26.4]<!-- /GEN -->.

### High-Intensity Evacuation Policy and Holding Establishment

**Question.** What happens to the R2E pools, returns to duty and mortality under the high-intensity profile when the evacuation policy and the R2E holding establishment are set across their ranges together? `[high_intensity, evacuation policy 15 to 60 days, 30 to 90 holding beds · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN policy_establishment_high -->
| Policy and holding beds | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Died of wounds | Never evacuated by horizon | Role 4 peak beds |
|---|---|---|---|---|---|---|
| 15 d, 30 beds | 30.4 [29.5, 31.3] | 19.8 [19.5, 20.0] | 2,064.7 [2,039.8, 2,089.6] | 623.9 [555.1, 692.8] | 3,216.5 [3,159.7, 3,273.3] | 287.4 [286.0, 288.8] |
| 15 d, 45 beds | 31.4 [30.3, 32.4] | 19.7 [19.5, 20.0] | 2,072.5 [2,042.4, 2,102.6] | 609.2 [541.6, 676.7] | 3,213.8 [3,156.3, 3,271.4] | 287.7 [286.5, 288.8] |
| 15 d, 60 beds | 32.5 [31.4, 33.6] | 19.8 [19.5, 20.1] | 2,063.2 [2,030.5, 2,095.9] | 562.3 [495.0, 629.6] | 3,204.6 [3,144.8, 3,264.4] | 287.5 [286.1, 288.8] |
| 15 d, 90 beds | 31.4 [30.4, 32.3] | 19.5 [19.2, 19.8] | 2,069.9 [2,046.6, 2,093.2] | 514.2 [474.8, 553.7] | 3,193.1 [3,140.4, 3,245.9] | 288.1 [286.6, 289.6] |
| 21 d, 30 beds | 55.4 [53.5, 57.3] | 20.2 [20.0, 20.4] | 2,864.3 [2,832.8, 2,895.8] | 509.3 [464.7, 554.0] | 3,083.1 [3,051.3, 3,114.9] | 281.3 [279.1, 283.5] |
| 21 d, 45 beds | 54.9 [53.0, 56.9] | 19.9 [19.6, 20.1] | 2,865.9 [2,830.0, 2,901.8] | 603.7 [540.6, 666.7] | 3,052.5 [3,009.1, 3,095.9] | 281.6 [279.5, 283.7] |
| 21 d, 60 beds | 56.1 [54.6, 57.7] | 19.8 [19.6, 20.0] | 2,845.0 [2,809.7, 2,880.4] | 601.0 [526.8, 675.2] | 3,032.2 [2,982.5, 3,081.9] | 282.1 [279.9, 284.3] |
| 21 d, 90 beds | 55.4 [53.3, 57.5] | 19.6 [19.3, 19.9] | 2,819.8 [2,781.5, 2,858.1] | 621.8 [525.5, 718.2] | 3,008.7 [2,947.1, 3,070.2] | 279.8 [277.7, 281.8] |
| 30 d, 30 beds | 93.5 [90.7, 96.2] | 19.9 [19.7, 20.2] | 3,413.5 [3,375.2, 3,451.8] | 603.1 [531.5, 674.7] | 2,901.5 [2,856.7, 2,946.4] | 260.0 [257.9, 262.1] |
| 30 d, 45 beds | 79.1 [76.6, 81.6] | 19.8 [19.6, 20.1] | 3,427.5 [3,393.7, 3,461.2] | 627.3 [551.9, 702.7] | 2,934.8 [2,891.8, 2,977.7] | 263.6 [260.2, 266.9] |
| 30 d, 60 beds | 76.1 [74.1, 78.1] | 19.6 [19.4, 19.9] | 3,461.5 [3,422.0, 3,501.1] | 605.8 [517.8, 693.9] | 2,918.4 [2,866.3, 2,970.4] | 261.8 [259.7, 263.9] |
| 30 d, 90 beds | 78.5 [76.7, 80.2] | 19.7 [19.4, 20.0] | 3,403.0 [3,361.3, 3,444.7] | 534.2 [475.8, 592.7] | 2,907.7 [2,863.1, 2,952.4] | 261.2 [259.2, 263.1] |
| 45 d, 30 beds | 191.9 [188.7, 195.2] | 20.1 [19.8, 20.3] | 4,276.0 [4,235.8, 4,316.2] | 586.1 [505.1, 667.2] | 2,672.5 [2,627.1, 2,717.9] | 227.1 [224.8, 229.4] |
| 45 d, 45 beds | 175.4 [172.3, 178.4] | 19.8 [19.5, 20.0] | 4,263.9 [4,228.7, 4,299.1] | 709.9 [598.4, 821.5] | 2,631.7 [2,575.8, 2,687.6] | 225.4 [223.8, 227.1] |
| 45 d, 60 beds | 163.2 [159.5, 166.8] | 20.0 [19.6, 20.3] | 4,282.9 [4,245.8, 4,320.0] | 526.8 [467.5, 586.1] | 2,690.3 [2,650.3, 2,730.3] | 225.5 [223.7, 227.3] |
| 45 d, 90 beds | 133.8 [130.3, 137.3] | 20.1 [19.8, 20.3] | 4,282.8 [4,235.2, 4,330.5] | 640.1 [551.3, 728.9] | 2,669.4 [2,621.6, 2,717.1] | 227.5 [225.6, 229.4] |
| 60 d, 30 beds | 327.9 [323.3, 332.4] | 20.0 [19.8, 20.2] | 5,063.9 [5,024.9, 5,102.9] | 583.9 [485.8, 682.1] | 2,263.3 [2,206.2, 2,320.4] | 195.8 [194.2, 197.4] |
| 60 d, 45 beds | 308.9 [302.0, 315.9] | 19.7 [19.4, 20.1] | 5,110.0 [5,055.0, 5,165.0] | 751.6 [637.6, 865.6] | 2,242.7 [2,183.8, 2,301.5] | 197.9 [196.4, 199.4] |
| 60 d, 60 beds | 291.3 [285.6, 297.0] | 19.4 [19.1, 19.7] | 5,102.3 [5,062.1, 5,142.6] | 698.4 [592.6, 804.1] | 2,174.9 [2,115.9, 2,233.9] | 195.9 [194.5, 197.4] |
| 60 d, 90 beds | 269.8 [264.2, 275.3] | 20.0 [19.7, 20.2] | 5,139.9 [5,105.4, 5,174.5] | 611.3 [546.1, 676.4] | 2,304.2 [2,259.0, 2,349.3] | 196.5 [195.3, 197.7] |
<!-- /GEN -->

At the shipped 21-day policy and 30 holding beds the closing-window R2E holding queue was <!-- GEN cell:policy_establishment_high|21 d, 30 beds|R2E hold mean queue|full -->55.4 [53.5, 57.3]<!-- /GEN --> casualties and the post-operative overflow census <!-- GEN cell:policy_establishment_high|21 d, 30 beds|R2E post-operative overflow census|full -->20.2 [20.0, 20.4]<!-- /GEN -->. The holding queue was <!-- GEN cell:policy_establishment_high|15 d, 30 beds|R2E hold mean queue|full -->30.4 [29.5, 31.3]<!-- /GEN --> at 15 days and 30 beds and <!-- GEN cell:policy_establishment_high|60 d, 30 beds|R2E hold mean queue|full -->327.9 [323.3, 332.4]<!-- /GEN --> at 60 days and 30 beds, and <!-- GEN cell:policy_establishment_high|60 d, 90 beds|R2E hold mean queue|full -->269.8 [264.2, 275.3]<!-- /GEN --> at 60 days and 90 beds. The post-operative overflow census lay between <!-- GEN cell:policy_establishment_high|60 d, 60 beds|R2E post-operative overflow census|full -->19.4 [19.1, 19.7]<!-- /GEN --> at 60 days and 60 beds and <!-- GEN cell:policy_establishment_high|21 d, 30 beds|R2E post-operative overflow census|full -->20.2 [20.0, 20.4]<!-- /GEN --> at the shipped setting.

<!-- GEN policy_establishment_high_stability -->
| Policy and holding beds | Holding queue not growing | Post-operative overflow not growing | Both not growing | Staging overflow not growing | R2E pools across the cell |
|---|---|---|---|---|---|
| 15 d, 30 beds | 28 of 30 | 28 of 30 | 26 of 30 | 0 of 30 | at least one growing |
| 15 d, 45 beds | 28 of 30 | 30 of 30 | 28 of 30 | 0 of 30 | at least one growing |
| 15 d, 60 beds | 25 of 30 | 28 of 30 | 24 of 30 | 0 of 30 | at least one growing |
| 15 d, 90 beds | 29 of 30 | 29 of 30 | 28 of 30 | 0 of 30 | at least one growing |
| 21 d, 30 beds | 29 of 30 | 29 of 30 | 28 of 30 | 0 of 30 | at least one growing |
| 21 d, 45 beds | 27 of 30 | 30 of 30 | 27 of 30 | 0 of 30 | at least one growing |
| 21 d, 60 beds | 28 of 30 | 28 of 30 | 27 of 30 | 0 of 30 | at least one growing |
| 21 d, 90 beds | 27 of 30 | 29 of 30 | 26 of 30 | 0 of 30 | at least one growing |
| 30 d, 30 beds | 29 of 30 | 29 of 30 | 28 of 30 | 0 of 30 | at least one growing |
| 30 d, 45 beds | 29 of 30 | 29 of 30 | 28 of 30 | 0 of 30 | at least one growing |
| 30 d, 60 beds | 29 of 30 | 30 of 30 | 29 of 30 | 0 of 30 | at least one growing |
| 30 d, 90 beds | 28 of 30 | 30 of 30 | 28 of 30 | 0 of 30 | at least one growing |
| 45 d, 30 beds | 29 of 30 | 30 of 30 | 29 of 30 | 0 of 30 | at least one growing |
| 45 d, 45 beds | 30 of 30 | 29 of 30 | 29 of 30 | 0 of 30 | at least one growing |
| 45 d, 60 beds | 30 of 30 | 29 of 30 | 29 of 30 | 0 of 30 | at least one growing |
| 45 d, 90 beds | 30 of 30 | 29 of 30 | 29 of 30 | 0 of 30 | at least one growing |
| 60 d, 30 beds | 26 of 30 | 29 of 30 | 26 of 30 | 0 of 30 | at least one growing |
| 60 d, 45 beds | 28 of 30 | 27 of 30 | 26 of 30 | 0 of 30 | at least one growing |
| 60 d, 60 beds | 29 of 30 | 30 of 30 | 29 of 30 | 0 of 30 | at least one growing |
| 60 d, 90 beds | 28 of 30 | 29 of 30 | 28 of 30 | 0 of 30 | at least one growing |
<!-- /GEN -->

The table counts, for each cell, the replications in which each R2E pool was not still rising over the second half of the campaign, and classifies the cell as a whole: a pool is growing across the cell where the mean of its replications' late-half slopes is positive with a 95% interval excluding zero and a drift of at least one percent of its level per 30-day block, the rule of [Classifying Stability](Methods.md#classifying-stability), and the cell holds where no pool is growing. The staging overflow census was not growing in <!-- GEN cell:policy_establishment_high_stability|21 d, 30 beds|Staging overflow not growing|full -->0 of 30<!-- /GEN --> replications at the shipped cell and in <!-- GEN cell:policy_establishment_high_stability|60 d, 90 beds|Staging overflow not growing|full -->0 of 30<!-- /GEN --> at 60 days and 90 beds, and the shipped cell was classified <!-- GEN cell:policy_establishment_high_stability|21 d, 30 beds|R2E pools across the cell|full -->at least one growing<!-- /GEN -->.

<!-- GEN policy_establishment_high_paired -->
| Policy and holding beds | Returns to duty | Died of wounds | Role 4 peak beds |
|---|---|---|---|
| 15 d, 30 beds | −799.6 [−838.1, −761.1] | +114.6 [+38.5, +190.7] | +6.1 [+3.8, +8.4] |
| 15 d, 45 beds | −791.8 [−837.0, −746.7] | +99.8 [+22.5, +177.2] | +6.4 [+3.9, +8.9] |
| 15 d, 60 beds | −801.1 [−846.8, −755.3] | +53.0 [−38.0, +143.9] | +6.2 [+3.8, +8.5] |
| 15 d, 90 beds | −794.4 [−831.5, −757.4] | +4.9 [−49.8, +59.6] | +6.8 [+4.2, +9.4] |
| 21 d, 45 beds | +1.6 [−47.1, +50.3] | +94.3 [+28.0, +160.7] | +0.3 [−2.9, +3.5] |
| 21 d, 60 beds | −19.3 [−59.5, +21.0] | +91.6 [+6.6, +176.7] | +0.8 [−1.9, +3.5] |
| 21 d, 90 beds | −44.5 [−96.2, +7.2] | +112.5 [−6.8, +231.8] | −1.5 [−4.4, +1.3] |
| 30 d, 30 beds | +549.2 [+505.2, +593.1] | +93.8 [+9.3, +178.2] | −21.3 [−24.1, −18.5] |
| 30 d, 45 beds | +563.2 [+518.8, +607.6] | +118.0 [+33.3, +202.6] | −17.7 [−21.8, −13.6] |
| 30 d, 60 beds | +597.2 [+552.4, +642.0] | +96.5 [+1.8, +191.2] | −19.5 [−22.9, −16.1] |
| 30 d, 90 beds | +538.7 [+490.1, +587.4] | +24.9 [−49.0, +98.8] | −20.1 [−23.2, −17.1] |
| 45 d, 30 beds | +1,411.7 [+1,355.8, +1,467.7] | +76.8 [−7.5, +161.1] | −54.2 [−57.1, −51.3] |
| 45 d, 45 beds | +1,399.6 [+1,355.9, +1,443.3] | +200.6 [+81.6, +319.6] | −55.9 [−58.7, −53.0] |
| 45 d, 60 beds | +1,418.6 [+1,369.4, +1,467.8] | +17.5 [−54.8, +89.8] | −55.8 [−59.2, −52.5] |
| 45 d, 90 beds | +1,418.5 [+1,357.9, +1,479.2] | +130.7 [+23.4, +238.1] | −53.8 [−56.7, −50.9] |
| 60 d, 30 beds | +2,199.6 [+2,156.3, +2,242.8] | +74.6 [−33.6, +182.8] | −85.5 [−88.0, −83.0] |
| 60 d, 45 beds | +2,245.7 [+2,190.2, +2,301.2] | +242.3 [+121.7, +362.8] | −83.4 [−86.2, −80.6] |
| 60 d, 60 beds | +2,238.0 [+2,189.6, +2,286.5] | +189.0 [+83.7, +294.3] | −85.4 [−87.8, −83.0] |
| 60 d, 90 beds | +2,275.6 [+2,229.9, +2,321.3] | +101.9 [+17.7, +186.1] | −84.8 [−87.3, −82.3] |
<!-- /GEN -->

Each cell was paired by replication against the shipped 21-day policy at 30 holding beds. At 30 days and 90 beds returns to duty differed by <!-- GEN cell:policy_establishment_high_paired|30 d, 90 beds|Returns to duty|full -->+538.7 [+490.1, +587.4]<!-- /GEN --> and Role 4 peak beds by <!-- GEN cell:policy_establishment_high_paired|30 d, 90 beds|Role 4 peak beds|full -->−20.1 [−23.2, −17.1]<!-- /GEN -->.

For each holding establishment the largest policy at which the cell held the R2E pools, by the rule of the preceding table, is given below; where none did, the cell reads as such.

<!-- GEN policy_grid_largest_high -->
| Holding beds | Largest policy holding the R2E pools | Returns to duty there | Paired gain over 21 d and 30 beds | Gain per added bed |
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
| 3 d interval, 21 d policy, 30 beds | 50.6 [48.8, 52.3] | 19.8 [19.6, 20.1] | at least one growing |
| 3 d interval, 21 d policy, 60 beds | 41.3 [39.8, 42.8] | 19.0 [18.6, 19.5] | at least one growing |
| 3 d interval, 30 d policy, 30 beds | 93.5 [91.0, 95.9] | 19.7 [19.4, 20.0] | at least one growing |
| 3 d interval, 30 d policy, 60 beds | 68.1 [66.0, 70.2] | 19.9 [19.6, 20.1] | at least one growing |
| 5 d interval, 21 d policy, 30 beds | 53.6 [51.7, 55.6] | 19.8 [19.6, 20.1] | at least one growing |
| 5 d interval, 21 d policy, 60 beds | 53.2 [51.7, 54.8] | 19.7 [19.4, 20.0] | at least one growing |
| 5 d interval, 30 d policy, 30 beds | 93.2 [90.7, 95.8] | 19.9 [19.6, 20.2] | at least one growing |
| 5 d interval, 30 d policy, 60 beds | 77.4 [74.7, 80.1] | 19.7 [19.5, 20.0] | at least one growing |
| 7 d interval, 21 d policy, 30 beds | 55.0 [53.0, 56.9] | 20.2 [20.0, 20.4] | at least one growing |
| 7 d interval, 21 d policy, 60 beds | 56.1 [54.6, 57.7] | 19.8 [19.6, 20.0] | at least one growing |
| 7 d interval, 30 d policy, 30 beds | 93.6 [90.8, 96.3] | 19.9 [19.7, 20.2] | at least one growing |
| 7 d interval, 30 d policy, 60 beds | 76.4 [74.2, 78.6] | 19.6 [19.4, 19.9] | at least one growing |
<!-- /GEN -->

The last column classifies each cell by the rule of the preceding grid. At the shipped cell, the seven-day interval with the 21-day policy and 30 beds, the closing-window R2E holding queue was <!-- GEN cell:interval_grid_high|7 d interval, 21 d policy, 30 beds|R2E hold mean queue|full -->55.0 [53.0, 56.9]<!-- /GEN --> casualties and the post-operative overflow census <!-- GEN cell:interval_grid_high|7 d interval, 21 d policy, 30 beds|R2E post-operative overflow census|full -->20.2 [20.0, 20.4]<!-- /GEN -->. At the three-day interval with the same policy and establishment they were <!-- GEN cell:interval_grid_high|3 d interval, 21 d policy, 30 beds|R2E hold mean queue|full -->50.6 [48.8, 52.3]<!-- /GEN --> and <!-- GEN cell:interval_grid_high|3 d interval, 21 d policy, 30 beds|R2E post-operative overflow census|full -->19.8 [19.6, 20.1]<!-- /GEN -->, and with 60 beds <!-- GEN cell:interval_grid_high|3 d interval, 21 d policy, 60 beds|R2E hold mean queue|full -->41.3 [39.8, 42.8]<!-- /GEN --> and <!-- GEN cell:interval_grid_high|3 d interval, 21 d policy, 60 beds|R2E post-operative overflow census|full -->19.0 [18.6, 19.5]<!-- /GEN -->. The three-day, 21-day, 60-bed cell was classified <!-- GEN cell:interval_grid_high|3 d interval, 21 d policy, 60 beds|Pools|full -->at least one growing<!-- /GEN -->, as was the three-day, 30-day, 60-bed cell (<!-- GEN cell:interval_grid_high|3 d interval, 30 d policy, 60 beds|Pools|full -->at least one growing<!-- /GEN -->).

<!-- GEN interval_grid_high_wait -->
| Interval, policy and holding beds | Mean wait, critical channel (days) | Mean wait, standard channel (days) | Still waiting, critical channel | Still waiting, standard channel |
|---|---|---|---|---|
| 3 d interval, 21 d policy, 30 beds | 22.45 [20.78, 24.13] | 1.50 [1.49, 1.51] | 631.0 [587.6, 674.4] | 17.5 [15.7, 19.3] |
| 3 d interval, 21 d policy, 60 beds | 20.58 [18.81, 22.36] | 1.50 [1.49, 1.51] | 547.2 [477.4, 617.0] | 17.3 [15.2, 19.4] |
| 3 d interval, 30 d policy, 30 beds | 20.22 [18.71, 21.73] | 1.50 [1.49, 1.51] | 514.0 [460.7, 567.3] | 12.4 [11.1, 13.7] |
| 3 d interval, 30 d policy, 60 beds | 20.54 [18.95, 22.14] | 1.49 [1.49, 1.50] | 567.2 [524.9, 609.4] | 14.8 [13.3, 16.3] |
| 5 d interval, 21 d policy, 30 beds | 82.84 [81.48, 84.20] | 2.52 [2.51, 2.54] | 2,326.1 [2,281.2, 2,370.9] | 28.1 [25.6, 30.5] |
| 5 d interval, 21 d policy, 60 beds | 82.47 [81.09, 83.84] | 2.49 [2.48, 2.51] | 2,290.9 [2,236.0, 2,345.7] | 30.8 [28.2, 33.4] |
| 5 d interval, 30 d policy, 30 beds | 80.13 [78.94, 81.32] | 2.49 [2.47, 2.51] | 2,203.1 [2,148.0, 2,258.2] | 22.6 [20.0, 25.2] |
| 5 d interval, 30 d policy, 60 beds | 80.75 [79.55, 81.96] | 2.49 [2.47, 2.50] | 2,223.4 [2,173.2, 2,273.7] | 22.9 [20.5, 25.3] |
| 7 d interval, 21 d policy, 30 beds | 109.52 [108.04, 111.00] | 3.54 [3.52, 3.56] | 3,065.9 [3,034.2, 3,097.7] | 18.8 [16.8, 20.8] |
| 7 d interval, 21 d policy, 60 beds | 109.16 [107.93, 110.38] | 3.56 [3.54, 3.58] | 3,013.3 [2,963.7, 3,062.9] | 18.9 [17.2, 20.6] |
| 7 d interval, 30 d policy, 30 beds | 107.40 [106.35, 108.45] | 3.50 [3.47, 3.52] | 2,885.2 [2,839.8, 2,930.6] | 14.7 [13.0, 16.4] |
| 7 d interval, 30 d policy, 60 beds | 108.28 [107.04, 109.52] | 3.49 [3.47, 3.51] | 2,904.3 [2,851.9, 2,956.7] | 13.3 [11.6, 15.0] |
<!-- /GEN -->

The mean wait is reported separately for the casualties who boarded on the critical (intensive care) channel and on the standard channel, with the casualties of each channel decided for evacuation who had not boarded at the close. At the three-day interval with the 21-day policy and 30 beds the mean wait was <!-- GEN cell:interval_grid_high_wait|3 d interval, 21 d policy, 30 beds|Mean wait, critical channel (days)|full -->22.45 [20.78, 24.13]<!-- /GEN --> days on the critical channel against <!-- GEN cell:interval_grid_high_wait|3 d interval, 21 d policy, 30 beds|Mean wait, standard channel (days)|full -->1.50 [1.49, 1.51]<!-- /GEN --> days on the standard channel, and <!-- GEN cell:interval_grid_high_wait|3 d interval, 21 d policy, 30 beds|Still waiting, critical channel|full -->631.0 [587.6, 674.4]<!-- /GEN --> casualties were still waiting on the critical channel at the close against <!-- GEN cell:interval_grid_high_wait|3 d interval, 21 d policy, 30 beds|Still waiting, standard channel|full -->17.5 [15.7, 19.3]<!-- /GEN --> on the standard channel. At the shipped seven-day interval the standard channel waited <!-- GEN cell:interval_grid_high_wait|7 d interval, 21 d policy, 30 beds|Mean wait, standard channel (days)|full -->3.54 [3.52, 3.56]<!-- /GEN --> days.

<!-- GEN interval_grid_high_outcomes -->
| Interval, policy and holding beds | Returns to duty | Died of wounds | Role 4 peak beds | Role 4 closing mean beds |
|---|---|---|---|---|
| 3 d interval, 21 d policy, 30 beds | 2,828.2 [2,796.0, 2,860.5] | 436.2 [376.4, 496.0] | 465.4 [462.9, 467.9] | 328.8 [326.7, 331.0] |
| 3 d interval, 21 d policy, 60 beds | 2,824.5 [2,798.9, 2,850.1] | 532.9 [405.7, 660.0] | 465.6 [463.1, 468.1] | 327.4 [324.8, 330.1] |
| 3 d interval, 30 d policy, 30 beds | 3,464.5 [3,427.8, 3,501.1] | 497.6 [405.9, 589.2] | 446.0 [443.6, 448.3] | 316.1 [314.4, 317.8] |
| 3 d interval, 30 d policy, 60 beds | 3,406.0 [3,369.5, 3,442.5] | 380.9 [335.4, 426.3] | 447.3 [444.7, 449.9] | 319.2 [317.2, 321.2] |
| 5 d interval, 21 d policy, 30 beds | 2,864.3 [2,833.3, 2,895.3] | 590.7 [502.9, 678.5] | 338.0 [335.0, 340.9] | 222.3 [220.0, 224.7] |
| 5 d interval, 21 d policy, 60 beds | 2,853.6 [2,822.4, 2,884.7] | 578.6 [491.3, 665.8] | 334.4 [332.1, 336.7] | 220.8 [219.0, 222.7] |
| 5 d interval, 30 d policy, 30 beds | 3,404.6 [3,370.4, 3,438.9] | 544.7 [462.8, 626.6] | 317.7 [314.8, 320.6] | 209.9 [207.8, 211.9] |
| 5 d interval, 30 d policy, 60 beds | 3,424.1 [3,387.4, 3,460.8] | 535.2 [458.2, 612.2] | 314.7 [312.6, 316.8] | 209.9 [208.3, 211.5] |
| 7 d interval, 21 d policy, 30 beds | 2,859.0 [2,825.7, 2,892.4] | 506.2 [461.5, 550.8] | 281.3 [279.1, 283.5] | 176.1 [174.4, 177.7] |
| 7 d interval, 21 d policy, 60 beds | 2,845.0 [2,809.7, 2,880.4] | 601.0 [526.8, 675.2] | 282.1 [279.9, 284.3] | 176.0 [174.5, 177.5] |
| 7 d interval, 30 d policy, 30 beds | 3,414.1 [3,375.9, 3,452.3] | 602.1 [530.2, 674.0] | 260.0 [257.8, 262.1] | 164.7 [163.1, 166.3] |
| 7 d interval, 30 d policy, 60 beds | 3,461.6 [3,422.0, 3,501.2] | 609.7 [521.4, 697.9] | 262.0 [260.0, 264.1] | 164.5 [163.1, 165.9] |
<!-- /GEN -->

Returns to duty per campaign were <!-- GEN cell:interval_grid_high_outcomes|7 d interval, 21 d policy, 30 beds|Returns to duty|full -->2,859.0 [2,825.7, 2,892.4]<!-- /GEN --> at the shipped cell and <!-- GEN cell:interval_grid_high_outcomes|3 d interval, 30 d policy, 60 beds|Returns to duty|full -->3,406.0 [3,369.5, 3,442.5]<!-- /GEN --> at the three-day interval with the 30-day policy and 60 beds, where the Role 4 peak was <!-- GEN cell:interval_grid_high_outcomes|3 d interval, 30 d policy, 60 beds|Role 4 peak beds|full -->447.3 [444.7, 449.9]<!-- /GEN --> beds.

<!-- GEN interval_grid_high_paired -->
| Interval, policy and holding beds | Returns to duty | Died of wounds | Role 4 peak beds |
|---|---|---|---|
| 3 d interval, 21 d policy, 30 beds | −30.8 [−74.0, +12.4] | −70.0 [−140.3, +0.4] | +184.1 [+181.2, +187.0] |
| 3 d interval, 21 d policy, 60 beds | −34.6 [−73.3, +4.2] | +26.7 [−114.6, +168.0] | +184.3 [+180.6, +187.9] |
| 3 d interval, 30 d policy, 30 beds | +605.4 [+551.9, +658.9] | −8.6 [−99.9, +82.7] | +164.7 [+160.9, +168.4] |
| 3 d interval, 30 d policy, 60 beds | +547.0 [+494.5, +599.5] | −125.3 [−184.6, −66.0] | +166.0 [+162.8, +169.1] |
| 5 d interval, 21 d policy, 30 beds | +5.2 [−45.9, +56.4] | +84.5 [−22.7, +191.7] | +56.7 [+53.3, +60.0] |
| 5 d interval, 21 d policy, 60 beds | −5.5 [−42.4, +31.5] | +72.4 [−31.5, +176.3] | +53.1 [+50.2, +56.1] |
| 5 d interval, 30 d policy, 30 beds | +545.6 [+493.3, +597.9] | +38.5 [−57.3, +134.3] | +36.4 [+32.8, +40.0] |
| 5 d interval, 30 d policy, 60 beds | +565.0 [+510.2, +619.9] | +29.1 [−60.4, +118.6] | +33.4 [+30.4, +36.4] |
| 7 d interval, 21 d policy, 60 beds | −14.0 [−53.4, +25.4] | +94.8 [+10.8, +178.8] | +0.8 [−1.9, +3.5] |
| 7 d interval, 30 d policy, 30 beds | +555.1 [+509.9, +600.2] | +95.9 [+11.1, +180.8] | −21.3 [−24.1, −18.6] |
| 7 d interval, 30 d policy, 60 beds | +602.5 [+553.7, +651.3] | +103.5 [+5.2, +201.8] | −19.3 [−22.6, −16.0] |
<!-- /GEN -->

Each cell was paired by replication against the shipped cell. At the three-day interval with the 30-day policy and 60 beds returns to duty differed by <!-- GEN cell:interval_grid_high_paired|3 d interval, 30 d policy, 60 beds|Returns to duty|full -->+547.0 [+494.5, +599.5]<!-- /GEN --> and died of wounds by <!-- GEN cell:interval_grid_high_paired|3 d interval, 30 d policy, 60 beds|Died of wounds|full -->−125.3 [−184.6, −66.0]<!-- /GEN -->; at the three-day interval with the 21-day policy and 30 beds returns to duty differed by <!-- GEN cell:interval_grid_high_paired|3 d interval, 21 d policy, 30 beds|Returns to duty|full -->−30.8 [−74.0, +12.4]<!-- /GEN --> and the Role 4 peak by <!-- GEN cell:interval_grid_high_paired|3 d interval, 21 d policy, 30 beds|Role 4 peak beds|full -->+184.1 [+181.2, +187.0]<!-- /GEN -->.

### Moderate-Intensity Evacuation Policy and Holding Establishment

**Question.** What happens to the R2E pools, returns to duty and mortality under the shipped configuration when the evacuation policy is set across the 21 to 30 day gap and the R2E holding establishment across 30 to 60 beds together? `[default, evacuation policy 21 to 30 days, 30 to 60 holding beds · 360 d · 30 replications · pool totals, closing 90 d]`

<!-- GEN policy_establishment_moderate -->
| Policy and holding beds | R2E hold mean queue | R2E post-operative overflow census | Returns to duty | Died of wounds | Never evacuated by horizon | Role 4 peak beds |
|---|---|---|---|---|---|---|
| 21 d, 30 beds | 22.8 [21.3, 24.3] | 5.3 [4.9, 5.7] | 2,049.6 [2,024.4, 2,074.7] | 20.7 [17.9, 23.5] | 33.3 [25.4, 41.1] | 211.2 [207.6, 214.7] |
| 21 d, 45 beds | 17.2 [16.1, 18.4] | 4.6 [4.1, 5.1] | 2,043.1 [2,013.9, 2,072.4] | 21.4 [18.2, 24.6] | 40.2 [25.5, 54.9] | 207.9 [203.4, 212.5] |
| 21 d, 60 beds | 13.5 [12.1, 14.8] | 4.0 [3.5, 4.5] | 2,017.4 [1,991.9, 2,042.8] | 23.3 [19.8, 26.8] | 40.6 [30.4, 50.9] | 211.3 [206.7, 215.9] |
| 24 d, 30 beds | 30.0 [28.0, 32.0] | 5.5 [5.2, 5.8] | 2,175.8 [2,147.5, 2,204.1] | 18.3 [16.4, 20.3] | 29.9 [22.4, 37.3] | 199.8 [196.9, 202.8] |
| 24 d, 45 beds | 22.9 [21.1, 24.7] | 4.9 [4.4, 5.3] | 2,189.4 [2,156.7, 2,222.0] | 18.4 [16.1, 20.7] | 28.9 [21.3, 36.6] | 200.4 [196.1, 204.8] |
| 24 d, 60 beds | 19.5 [17.9, 21.2] | 4.5 [4.1, 5.0] | 2,203.9 [2,172.6, 2,235.2] | 23.3 [19.9, 26.7] | 37.1 [28.2, 46.0] | 206.5 [202.3, 210.7] |
| 27 d, 30 beds | 36.2 [34.1, 38.3] | 5.5 [5.1, 5.9] | 2,262.7 [2,232.8, 2,292.6] | 18.8 [15.8, 21.7] | 26.4 [21.1, 31.7] | 196.9 [192.7, 201.2] |
| 27 d, 45 beds | 26.7 [25.0, 28.4] | 5.0 [4.6, 5.3] | 2,277.2 [2,243.6, 2,310.8] | 20.0 [17.7, 22.2] | 24.3 [20.0, 28.6] | 198.5 [194.6, 202.4] |
| 27 d, 60 beds | 21.1 [19.4, 22.8] | 4.3 [3.9, 4.7] | 2,264.3 [2,231.2, 2,297.4] | 20.8 [18.7, 22.9] | 33.8 [23.9, 43.7] | 199.1 [195.0, 203.2] |
| 30 d, 30 beds | 42.0 [39.6, 44.3] | 6.0 [5.6, 6.4] | 2,348.0 [2,308.6, 2,387.3] | 20.0 [17.6, 22.3] | 32.8 [23.9, 41.8] | 197.2 [192.8, 201.6] |
| 30 d, 45 beds | 32.4 [30.5, 34.2] | 5.8 [5.3, 6.3] | 2,345.0 [2,308.3, 2,381.7] | 21.7 [17.1, 26.4] | 42.2 [30.2, 54.2] | 197.5 [192.7, 202.3] |
| 30 d, 60 beds | 23.9 [22.3, 25.5] | 4.4 [4.0, 4.7] | 2,331.6 [2,307.3, 2,355.8] | 22.0 [16.2, 27.9] | 28.3 [21.4, 35.2] | 195.6 [191.6, 199.6] |
<!-- /GEN -->

At the shipped 21-day policy and 30 holding beds the closing-window R2E holding queue was <!-- GEN cell:policy_establishment_moderate|21 d, 30 beds|R2E hold mean queue|full -->22.8 [21.3, 24.3]<!-- /GEN --> casualties and the post-operative overflow census <!-- GEN cell:policy_establishment_moderate|21 d, 30 beds|R2E post-operative overflow census|full -->5.3 [4.9, 5.7]<!-- /GEN -->. At 30 days and 30 beds they were <!-- GEN cell:policy_establishment_moderate|30 d, 30 beds|R2E hold mean queue|full -->42.0 [39.6, 44.3]<!-- /GEN --> and <!-- GEN cell:policy_establishment_moderate|30 d, 30 beds|R2E post-operative overflow census|full -->6.0 [5.6, 6.4]<!-- /GEN -->, and at 30 days and 60 beds <!-- GEN cell:policy_establishment_moderate|30 d, 60 beds|R2E hold mean queue|full -->23.9 [22.3, 25.5]<!-- /GEN --> and <!-- GEN cell:policy_establishment_moderate|30 d, 60 beds|R2E post-operative overflow census|full -->4.4 [4.0, 4.7]<!-- /GEN -->. Returns to duty per campaign rose from <!-- GEN cell:policy_establishment_moderate|21 d, 30 beds|Returns to duty|full -->2,049.6 [2,024.4, 2,074.7]<!-- /GEN --> at the shipped setting to <!-- GEN cell:policy_establishment_moderate|30 d, 60 beds|Returns to duty|full -->2,331.6 [2,307.3, 2,355.8]<!-- /GEN --> at 30 days and 60 beds.

<!-- GEN policy_establishment_moderate_stability -->
| Policy and holding beds | Holding queue not growing | Post-operative overflow not growing | Both not growing | Staging overflow not growing | R2E pools across the cell |
|---|---|---|---|---|---|
| 21 d, 30 beds | 28 of 30 | 30 of 30 | 28 of 30 | 28 of 30 | all hold |
| 21 d, 45 beds | 30 of 30 | 30 of 30 | 30 of 30 | 29 of 30 | all hold |
| 21 d, 60 beds | 28 of 30 | 27 of 30 | 27 of 30 | 26 of 30 | all hold |
| 24 d, 30 beds | 28 of 30 | 30 of 30 | 28 of 30 | 28 of 30 | all hold |
| 24 d, 45 beds | 30 of 30 | 30 of 30 | 30 of 30 | 30 of 30 | all hold |
| 24 d, 60 beds | 27 of 30 | 28 of 30 | 26 of 30 | 26 of 30 | all hold |
| 27 d, 30 beds | 28 of 30 | 29 of 30 | 27 of 30 | 29 of 30 | all hold |
| 27 d, 45 beds | 30 of 30 | 30 of 30 | 30 of 30 | 30 of 30 | all hold |
| 27 d, 60 beds | 26 of 30 | 29 of 30 | 26 of 30 | 27 of 30 | all hold |
| 30 d, 30 beds | 28 of 30 | 27 of 30 | 27 of 30 | 27 of 30 | all hold |
| 30 d, 45 beds | 30 of 30 | 28 of 30 | 28 of 30 | 23 of 30 | all hold |
| 30 d, 60 beds | 29 of 30 | 30 of 30 | 29 of 30 | 29 of 30 | all hold |
<!-- /GEN -->

The table counts and classifies as the high-intensity grid's does. At 27 days the replications with neither the holding queue nor the post-operative overflow growing numbered <!-- GEN cell:policy_establishment_moderate_stability|27 d, 30 beds|Both not growing|full -->27 of 30<!-- /GEN --> at 30 beds, <!-- GEN cell:policy_establishment_moderate_stability|27 d, 45 beds|Both not growing|full -->30 of 30<!-- /GEN --> at 45 and <!-- GEN cell:policy_establishment_moderate_stability|27 d, 60 beds|Both not growing|full -->26 of 30<!-- /GEN --> at 60; at 30 days they numbered <!-- GEN cell:policy_establishment_moderate_stability|30 d, 30 beds|Both not growing|full -->27 of 30<!-- /GEN -->, <!-- GEN cell:policy_establishment_moderate_stability|30 d, 45 beds|Both not growing|full -->28 of 30<!-- /GEN --> and <!-- GEN cell:policy_establishment_moderate_stability|30 d, 60 beds|Both not growing|full -->29 of 30<!-- /GEN -->. The 30-day, 30-bed cell was classified <!-- GEN cell:policy_establishment_moderate_stability|30 d, 30 beds|R2E pools across the cell|full -->all hold<!-- /GEN -->.

<!-- GEN policy_establishment_moderate_paired -->
| Policy and holding beds | Returns to duty | Died of wounds | Role 4 peak beds |
|---|---|---|---|
| 21 d, 45 beds | −6.4 [−42.0, +29.1] | +0.7 [−3.5, +4.8] | −3.2 [−8.6, +2.1] |
| 21 d, 60 beds | −32.2 [−71.9, +7.5] | +2.6 [−1.4, +6.6] | +0.1 [−6.0, +6.3] |
| 24 d, 30 beds | +126.2 [+88.5, +164.0] | −2.4 [−5.7, +0.9] | −11.3 [−16.4, −6.3] |
| 24 d, 45 beds | +139.8 [+94.8, +184.8] | −2.3 [−5.9, +1.2] | −10.7 [−16.1, −5.3] |
| 24 d, 60 beds | +154.3 [+113.8, +194.9] | +2.5 [−2.0, +7.1] | −4.7 [−10.4, +1.0] |
| 27 d, 30 beds | +213.2 [+168.5, +257.8] | −2.0 [−6.5, +2.6] | −14.2 [−19.2, −9.3] |
| 27 d, 45 beds | +227.6 [+181.9, +273.3] | −0.8 [−4.2, +2.7] | −12.6 [−18.9, −6.3] |
| 27 d, 60 beds | +214.7 [+170.4, +259.0] | +0.1 [−3.3, +3.4] | −12.1 [−17.6, −6.5] |
| 30 d, 30 beds | +298.4 [+247.1, +349.7] | −0.8 [−4.1, +2.6] | −14.0 [−19.8, −8.1] |
| 30 d, 45 beds | +295.4 [+247.6, +343.3] | +1.0 [−4.1, +6.1] | −13.7 [−19.6, −7.8] |
| 30 d, 60 beds | +282.0 [+247.6, +316.4] | +1.3 [−5.4, +8.0] | −15.6 [−21.3, −9.8] |
<!-- /GEN -->

Each cell was paired by replication against the shipped 21-day policy at 30 holding beds. At 27 days and 45 beds returns to duty differed by <!-- GEN cell:policy_establishment_moderate_paired|27 d, 45 beds|Returns to duty|full -->+227.6 [+181.9, +273.3]<!-- /GEN --> and Role 4 peak beds by <!-- GEN cell:policy_establishment_moderate_paired|27 d, 45 beds|Role 4 peak beds|full -->−12.6 [−18.9, −6.3]<!-- /GEN -->; at 30 days and 60 beds the differences were <!-- GEN cell:policy_establishment_moderate_paired|30 d, 60 beds|Returns to duty|full -->+282.0 [+247.6, +316.4]<!-- /GEN --> and <!-- GEN cell:policy_establishment_moderate_paired|30 d, 60 beds|Role 4 peak beds|full -->−15.6 [−21.3, −9.8]<!-- /GEN -->.

<!-- GEN policy_grid_largest_moderate -->
| Holding beds | Largest policy holding the R2E pools | Returns to duty there | Paired gain over 21 d and 30 beds | Gain per added bed |
|---|---|---|---|---|
| 30 | 30 d | 2,348.0 [2,308.6, 2,387.3] | +298.4 [+247.1, +349.7] | not applicable |
| 45 | 30 d | 2,345.0 [2,308.3, 2,381.7] | +295.4 [+247.6, +343.3] | +19.7 |
| 60 | 30 d | 2,331.6 [2,307.3, 2,355.8] | +282.0 [+247.6, +316.4] | +9.4 |
<!-- /GEN -->

The largest policy at which the cell held the R2E pools was <!-- GEN cell:policy_grid_largest_moderate|30|Largest policy holding the R2E pools|full -->30 d<!-- /GEN --> at 30 beds, <!-- GEN cell:policy_grid_largest_moderate|45|Largest policy holding the R2E pools|full -->30 d<!-- /GEN --> at 45 and <!-- GEN cell:policy_grid_largest_moderate|60|Largest policy holding the R2E pools|full -->30 d<!-- /GEN --> at 60. The paired gain per added bed was <!-- GEN cell:policy_grid_largest_moderate|45|Gain per added bed|full -->+19.7<!-- /GEN --> returns to duty at 45 beds and <!-- GEN cell:policy_grid_largest_moderate|60|Gain per added bed|full -->+9.4<!-- /GEN --> at 60.

### Forward Surgical Saturation Release

**Question.** What changes when casualties are released to strategic evacuation with the definitive repair outstanding once the R2E theatre queue reaches a threshold? `[default, threshold 0 to 24 casualties · 360 d · 30 replications · paired, closing 90 d]`

<!-- GEN saturation -->
| Response | 0 (disabled) | 1 | 2 (shipped) | 3 | 5 | 8 | 12 | 16 | 24 |
|---|---|---|---|---|---|---|---|---|---|
| Theatre mean queue | 1.93 [0.79, 3.07] | 0.70 [0.39, 1.01] | 0.53 [0.35, 0.71] | 0.65 [0.49, 0.81] | 0.87 [0.49, 1.25] | 0.90 [0.67, 1.13] | 0.99 [0.55, 1.43] | 0.89 [0.63, 1.15] | 0.93 [0.68, 1.17] |
| Released with repair outstanding | 0.0 [0.0, 0.0] | 173.0 [153.9, 192.0] | 129.3 [113.1, 145.5] | 99.9 [85.9, 113.8] | 97.8 [82.1, 113.5] | 71.6 [60.1, 83.1] | 47.3 [34.9, 59.6] | 47.2 [35.6, 58.8] | 25.8 [16.3, 35.2] |
| Role 4 operations owed | 1063.4 [1031.9, 1094.8] | 1255.3 [1198.8, 1311.8] | 1207.4 [1162.0, 1252.9] | 1164.4 [1113.7, 1215.0] | 1173.7 [1130.1, 1217.2] | 1148.2 [1107.5, 1188.9] | 1126.0 [1097.2, 1154.7] | 1139.1 [1096.7, 1181.4] | 1104.7 [1072.0, 1137.3] |
| Post-definitive ICU access (%) | 34.4 [33.6, 35.2] | 35.7 [34.8, 36.6] | 35.4 [34.7, 36.1] | 35.9 [35.2, 36.7] | 34.8 [34.1, 35.5] | 34.7 [34.0, 35.4] | 35.3 [34.7, 35.9] | 34.4 [33.8, 34.9] | 34.6 [33.8, 35.4] |
| Died of wounds | 21.9 [18.6, 25.2] | 22.7 [18.8, 26.6] | 20.7 [17.9, 23.5] | 19.0 [16.9, 21.1] | 23.0 [19.6, 26.3] | 21.4 [19.0, 23.8] | 22.0 [19.4, 24.6] | 22.6 [19.7, 25.5] | 23.6 [20.5, 26.8] |
| Returns to duty | 2037.3 [2000.8, 2073.8] | 2022.7 [1997.4, 2048.1] | 2049.6 [2024.4, 2074.7] | 2049.7 [2026.7, 2072.7] | 2047.7 [2012.7, 2082.6] | 2040.9 [2005.9, 2075.8] | 2042.3 [2013.1, 2071.6] | 2052.5 [2026.2, 2078.7] | 2066.8 [2028.6, 2105.0] |
<!-- /GEN -->

![Six panels of campaign responses against the forward surgical saturation release threshold](../images/saturation_sweep.png)

The same six responses against the R2E theatre queue length at which a casualty is released, with the shipped threshold marked by a dashed line.

With the release disabled the closing-window theatre queue was <!-- GEN cell:saturation|Theatre mean queue|0 (disabled)|full -->1.93 [0.79, 3.07]<!-- /GEN --> casualties and no casualty was released with the repair outstanding. At the shipped threshold of two the queue was <!-- GEN cell:saturation|Theatre mean queue|2 (shipped)|full -->0.53 [0.35, 0.71]<!-- /GEN --> and <!-- GEN cell:saturation|Released with repair outstanding|2 (shipped)|full -->129.3 [113.1, 145.5]<!-- /GEN --> casualties per campaign were released with the repair outstanding, with <!-- GEN cell:saturation|Role 4 operations owed|2 (shipped)|full -->1207.4 [1162.0, 1252.9]<!-- /GEN --> operations owed at the national support base against <!-- GEN cell:saturation|Role 4 operations owed|0 (disabled)|full -->1063.4 [1031.9, 1094.8]<!-- /GEN --> with the release disabled. Returns to duty and died of wounds per campaign were <!-- GEN cell:saturation|Returns to duty|2 (shipped)|full -->2049.6 [2024.4, 2074.7]<!-- /GEN --> and <!-- GEN cell:saturation|Died of wounds|2 (shipped)|full -->20.7 [17.9, 23.5]<!-- /GEN --> at the shipped threshold and <!-- GEN cell:saturation|Returns to duty|0 (disabled)|full -->2037.3 [2000.8, 2073.8]<!-- /GEN --> and <!-- GEN cell:saturation|Died of wounds|0 (disabled)|full -->21.9 [18.6, 25.2]<!-- /GEN --> with the release disabled.

**High-intensity profile.** The same sweep under `high_intensity` `[high_intensity, threshold 0 to 24 casualties · 360 d · 30 replications · paired, closing 90 d]`:

<!-- GEN saturation_high -->
| Response | 0 (disabled) | 1 | 2 (shipped) | 3 | 5 | 8 | 12 | 16 | 24 |
|---|---|---|---|---|---|---|---|---|---|
| Theatre mean queue | 937.46 [875.99, 998.94] | 79.21 [50.81, 107.61] | 56.79 [42.95, 70.64] | 99.99 [68.27, 131.71] | 84.72 [54.62, 114.82] | 78.20 [54.96, 101.43] | 88.88 [69.03, 108.74] | 80.90 [61.97, 99.82] | 98.38 [65.53, 131.23] |
| Released with repair outstanding | 0.0 [0.0, 0.0] | 2019.1 [1984.6, 2053.6] | 2004.2 [1966.7, 2041.7] | 1970.9 [1926.3, 2015.6] | 1914.1 [1854.5, 1973.6] | 1922.3 [1870.2, 1974.3] | 1911.6 [1861.2, 1962.0] | 1868.6 [1810.6, 1926.7] | 1800.7 [1737.5, 1864.0] |
| Role 4 operations owed | 1331.6 [1311.4, 1351.8] | 2065.1 [2037.6, 2092.6] | 2023.2 [1994.3, 2052.0] | 2045.1 [2019.3, 2070.9] | 2046.3 [2003.6, 2089.0] | 2021.4 [1994.8, 2048.0] | 2029.8 [1991.5, 2068.0] | 1998.9 [1964.4, 2033.4] | 2001.0 [1966.3, 2035.7] |
| Post-definitive ICU access (%) | 15.8 [15.7, 16.0] | 11.2 [11.0, 11.4] | 11.0 [10.9, 11.2] | 11.1 [10.9, 11.2] | 11.2 [11.0, 11.5] | 11.2 [11.0, 11.4] | 11.2 [11.0, 11.3] | 11.2 [10.9, 11.4] | 11.4 [11.2, 11.6] |
| Died of wounds | 1748.8 [1679.6, 1818.0] | 529.6 [476.7, 582.4] | 509.3 [464.7, 554.0] | 585.8 [523.6, 647.9] | 581.0 [496.9, 665.1] | 609.9 [532.4, 687.5] | 671.1 [602.0, 740.2] | 646.4 [581.0, 711.8] | 748.0 [669.7, 826.3] |
| Returns to duty | 2777.5 [2755.4, 2799.7] | 2836.3 [2801.4, 2871.3] | 2864.3 [2832.8, 2895.8] | 2878.3 [2836.3, 2920.4] | 2871.6 [2838.6, 2904.7] | 2863.7 [2827.7, 2899.7] | 2907.1 [2876.9, 2937.3] | 2861.0 [2825.9, 2896.2] | 2842.3 [2808.1, 2876.5] |
<!-- /GEN -->

The R2E theatre mean queue was <!-- GEN cell:saturation_high|Theatre mean queue|0 (disabled)|full -->937.46 [875.99, 998.94]<!-- /GEN --> with the release disabled and <!-- GEN cell:saturation_high|Theatre mean queue|2 (shipped)|full -->56.79 [42.95, 70.64]<!-- /GEN --> at the shipped threshold of two.


### Surgical Team Establishment

**Question.** What changes when the surgical sections fielded at R2E, at R2B or at both are increased, and does the R2E theatre queue converge at the larger establishment? `[default, R2E 3 to 6 and R2B 1 to 2 sections · 360 d · 30 replications · paired, closing 90 d]`

Each arm is a count of surgical sections: three, four or six at R2E, and one or two per R2B unit. R2E fields two operating theatres in every arm. A second R2B section takes the shift the first does not, and an R2E fourth section makes two sections on each shift. The shipped establishment is three sections at R2E and one per R2B unit.

<!-- GEN surgical_team -->
| Response | R2E 3, R2B 1 (shipped) | R2E 4, R2B 1 | R2E 6, R2B 1 | R2E 3, R2B 2 | R2E 4, R2B 2 |
|---|---|---|---|---|---|
| R2E theatre mean queue | 0.53 [0.35, 0.71] | 0.27 [0.18, 0.35] | 0.27 [0.17, 0.38] | 0.58 [0.29, 0.86] | 0.21 [0.14, 0.27] |
| R2E theatre queue, late-half classification | converged at 0.60 | converged at 0.33 | converged at 0.32 | converged at 0.46 | converged at 0.25 |
| R2E theatre queue shortfall per day | 0.000 [−0.004, 0.005] | 0.000 [−0.003, 0.003] | −0.001 [−0.005, 0.003] | 0.002 [−0.002, 0.006] | 0.000 [−0.002, 0.001] |
| Diverted, team off shift | 1198.4 [1172.5, 1224.4] | 1214.4 [1177.0, 1251.9] | 1184.3 [1165.3, 1203.3] | 0.0 [0.0, 0.0] | 0.0 [0.0, 0.0] |
| Diverted, theatre busy | 233.0 [217.7, 248.2] | 247.4 [228.4, 266.4] | 234.4 [223.0, 245.8] | 497.2 [467.0, 527.5] | 518.2 [493.3, 543.2] |
| R2B surgeries | 824.8 [811.6, 837.9] | 830.1 [814.5, 845.8] | 820.8 [810.9, 830.6] | 1563.1 [1536.7, 1589.6] | 1580.9 [1558.8, 1602.9] |
| R2E first surgeries | 1807.4 [1762.1, 1852.8] | 1837.1 [1775.6, 1898.6] | 1798.8 [1764.9, 1832.7] | 1368.2 [1316.6, 1419.8] | 1397.3 [1351.8, 1442.8] |
| R2E theatre entry deferred | 368.8 [358.3, 379.2] | 376.1 [357.6, 394.7] | 365.7 [354.7, 376.6] | 226.5 [212.1, 240.9] | 235.9 [221.6, 250.2] |
| Released with repair outstanding | 129.3 [113.1, 145.5] | 92.2 [75.0, 109.5] | 78.8 [68.1, 89.5] | 68.7 [55.1, 82.3] | 52.8 [39.9, 65.7] |
| Died of wounds | 20.73 [17.94, 23.52] | 23.30 [13.59, 33.01] | 18.80 [16.30, 21.30] | 18.17 [15.50, 20.83] | 17.17 [14.57, 19.76] |
| Returns to duty | 2049.6 [2024.4, 2074.7] | 2039.0 [2012.2, 2065.8] | 2052.3 [2025.4, 2079.1] | 2067.6 [2029.3, 2105.8] | 2056.9 [2024.1, 2089.7] |
| Post-definitive ICU access (%) | 35.4 [34.7, 36.1] | 34.8 [34.0, 35.7] | 35.3 [34.8, 35.9] | 34.4 [33.4, 35.3] | 33.5 [32.7, 34.2] |
<!-- /GEN -->

The paired differences against the shipped establishment are in the next table, each taken within replication.

<!-- GEN surgical_team_paired -->
| Paired difference against the shipped establishment | R2E 4, R2B 1 | R2E 6, R2B 1 | R2E 3, R2B 2 | R2E 4, R2B 2 |
|---|---|---|---|---|
| R2E theatre mean queue | −0.26 [−0.48, −0.05] | −0.26 [−0.46, −0.05] | +0.05 [−0.29, +0.38] | −0.32 [−0.49, −0.16] |
| Diverted, team off shift | +16.0 [−27.4, +59.4] | −14.1 [−44.3, +16.1] | −1198.4 [−1224.4, −1172.5] | −1198.4 [−1224.4, −1172.5] |
| Diverted, theatre busy | +14.4 [−8.6, +37.4] | +1.4 [−13.8, +16.6] | +264.3 [+232.1, +296.5] | +285.3 [+257.1, +313.4] |
| R2B surgeries | +5.4 [−12.9, +23.7] | −4.0 [−19.9, +11.9] | +738.4 [+708.3, +768.4] | +756.1 [+730.1, +782.1] |
| R2E first surgeries | +29.7 [−42.2, +101.6] | −8.6 [−59.1, +41.9] | −439.2 [−503.1, −375.4] | −410.1 [−473.5, −346.7] |
| R2E theatre entry deferred | +7.4 [−13.1, +27.8] | −3.1 [−17.9, +11.7] | −142.3 [−160.4, −124.1] | −132.9 [−149.9, −115.8] |
| Released with repair outstanding | −37.0 [−60.0, −14.0] | −50.5 [−68.4, −32.6] | −60.6 [−79.9, −41.2] | −76.5 [−96.2, −56.8] |
| Died of wounds | +2.57 [−7.19, +12.33] | −1.93 [−6.06, +2.20] | −2.57 [−6.34, +1.21] | −3.57 [−7.43, +0.30] |
| Returns to duty | −10.6 [−41.5, +20.3] | +2.7 [−33.7, +39.1] | +18.0 [−26.9, +62.9] | +7.3 [−27.8, +42.4] |
| Post-definitive ICU access (%) | −0.6 [−1.6, +0.4] | −0.0 [−0.8, +0.8] | −1.0 [−2.2, +0.1] | −1.9 [−3.0, −0.9] |
<!-- /GEN -->

At the shipped establishment the R2E theatre queue averaged <!-- GEN cell:surgical_team|R2E theatre mean queue|R2E 3, R2B 1 (shipped)|full -->0.53 [0.35, 0.71]<!-- /GEN --> over the closing 90 days and was classified <!-- GEN cell:surgical_team|R2E theatre queue, late-half classification|R2E 3, R2B 1 (shipped)|full -->converged at 0.60<!-- /GEN -->. A fourth R2E section changed it by <!-- GEN cell:surgical_team_paired|R2E theatre mean queue|R2E 4, R2B 1|full -->−0.26 [−0.48, −0.05]<!-- /GEN --> and a sixth by <!-- GEN cell:surgical_team_paired|R2E theatre mean queue|R2E 6, R2B 1|full -->−0.26 [−0.46, −0.05]<!-- /GEN -->, with the queue classified <!-- GEN cell:surgical_team|R2E theatre queue, late-half classification|R2E 4, R2B 1|full -->converged at 0.33<!-- /GEN --> and <!-- GEN cell:surgical_team|R2E theatre queue, late-half classification|R2E 6, R2B 1|full -->converged at 0.32<!-- /GEN -->. The paired differences in deaths of wounds for the fourth and sixth R2E sections were <!-- GEN cell:surgical_team_paired|Died of wounds|R2E 4, R2B 1|full -->+2.57 [−7.19, +12.33]<!-- /GEN --> and <!-- GEN cell:surgical_team_paired|Died of wounds|R2E 6, R2B 1|full -->−1.93 [−6.06, +2.20]<!-- /GEN -->, and in returns to duty <!-- GEN cell:surgical_team_paired|Returns to duty|R2E 4, R2B 1|full -->−10.6 [−41.5, +20.3]<!-- /GEN --> and <!-- GEN cell:surgical_team_paired|Returns to duty|R2E 6, R2B 1|full -->+2.7 [−33.7, +39.1]<!-- /GEN -->.

A second R2B section removed the cases diverted for a closed section, which fell from <!-- GEN cell:surgical_team|Diverted, team off shift|R2E 3, R2B 1 (shipped)|full -->1198.4 [1172.5, 1224.4]<!-- /GEN --> to <!-- GEN cell:surgical_team|Diverted, team off shift|R2E 3, R2B 2|full -->0.0 [0.0, 0.0]<!-- /GEN -->, and raised those diverted for a busy theatre by <!-- GEN cell:surgical_team_paired|Diverted, theatre busy|R2E 3, R2B 2|full -->+264.3 [+232.1, +296.5]<!-- /GEN -->. R2B surgeries rose by <!-- GEN cell:surgical_team_paired|R2B surgeries|R2E 3, R2B 2|full -->+738.4 [+708.3, +768.4]<!-- /GEN -->, R2E first surgeries fell by <!-- GEN cell:surgical_team_paired|R2E first surgeries|R2E 3, R2B 2|full -->−439.2 [−503.1, −375.4]<!-- /GEN --> and the casualties released with the repair outstanding fell by <!-- GEN cell:surgical_team_paired|Released with repair outstanding|R2E 3, R2B 2|full -->−60.6 [−79.9, −41.2]<!-- /GEN -->. Deaths of wounds differed by <!-- GEN cell:surgical_team_paired|Died of wounds|R2E 3, R2B 2|full -->−2.57 [−6.34, +1.21]<!-- /GEN --> per campaign, and returns to duty by <!-- GEN cell:surgical_team_paired|Returns to duty|R2E 3, R2B 2|full -->+18.0 [−26.9, +62.9]<!-- /GEN -->. With both levers together the differences in deaths of wounds and in returns to duty were <!-- GEN cell:surgical_team_paired|Died of wounds|R2E 4, R2B 2|full -->−3.57 [−7.43, +0.30]<!-- /GEN --> and <!-- GEN cell:surgical_team_paired|Returns to duty|R2E 4, R2B 2|full -->+7.3 [−27.8, +42.4]<!-- /GEN -->.


At high intensity the same five arms were run under the `high_intensity` profile (30 replications, 360 days, seed 42).

<!-- GEN surgical_team_high -->
| Response | R2E 3, R2B 1 (shipped) | R2E 4, R2B 1 | R2E 6, R2B 1 | R2E 3, R2B 2 | R2E 4, R2B 2 |
|---|---|---|---|---|---|
| R2E theatre mean queue | 56.79 [42.95, 70.64] | 8.91 [7.84, 9.98] | 10.51 [7.91, 13.11] | 21.44 [15.27, 27.62] | 4.78 [3.87, 5.68] |
| R2E theatre queue, late-half classification | **drifting, +4.3%/block**, 17.4 to 60.5 | converged at 8.97 | converged at 10.2 | converged at 21.0 | converged at 4.93 |
| R2E theatre queue shortfall per day | 0.074 [0.050, 0.099] | −0.001 [−0.028, 0.025] | 0.009 [−0.011, 0.030] | 0.001 [−0.029, 0.031] | −0.002 [−0.008, 0.004] |
| Diverted, team off shift | 3830.6 [3794.8, 3866.4] | 3839.1 [3794.0, 3884.1] | 3868.2 [3821.6, 3914.7] | 0.0 [0.0, 0.0] | 0.0 [0.0, 0.0] |
| Diverted, theatre busy | 1358.6 [1326.8, 1390.3] | 1388.7 [1358.0, 1419.5] | 1413.2 [1374.6, 1451.8] | 2585.8 [2534.7, 2637.0] | 2564.9 [2513.6, 2616.2] |
| R2B surgeries | 1791.7 [1779.3, 1804.2] | 1799.7 [1783.1, 1816.3] | 1811.9 [1793.2, 1830.7] | 3287.5 [3259.6, 3315.5] | 3286.4 [3260.4, 3312.5] |
| R2E first surgeries | 6153.3 [6090.4, 6216.1] | 6251.6 [6172.5, 6330.7] | 6316.7 [6224.0, 6409.4] | 5454.2 [5367.2, 5541.2] | 5418.8 [5324.2, 5513.5] |
| R2E theatre entry deferred | 1600.9 [1577.9, 1623.8] | 1602.2 [1576.0, 1628.5] | 1621.9 [1593.6, 1650.3] | 1275.0 [1249.0, 1301.0] | 1269.5 [1241.2, 1297.9] |
| Released with repair outstanding | 2004.2 [1966.7, 2041.7] | 1574.7 [1522.8, 1626.5] | 1619.0 [1561.7, 1676.4] | 1385.8 [1336.8, 1434.9] | 985.5 [936.2, 1034.8] |
| Died of wounds | 509.33 [464.71, 553.96] | 318.67 [305.17, 332.16] | 326.53 [307.26, 345.80] | 362.50 [343.66, 381.34] | 283.17 [274.19, 292.14] |
| Returns to duty | 2864.3 [2832.8, 2895.8] | 2906.4 [2871.1, 2941.7] | 2909.5 [2874.0, 2945.0] | 2898.8 [2861.6, 2936.0] | 2914.2 [2876.6, 2951.8] |
| Post-definitive ICU access (%) | 11.0 [10.9, 11.2] | 11.4 [11.2, 11.5] | 11.3 [11.1, 11.5] | 10.7 [10.5, 10.8] | 10.9 [10.8, 11.1] |
<!-- /GEN -->

<!-- GEN surgical_team_paired_high -->
| Paired difference against the shipped establishment | R2E 4, R2B 1 | R2E 6, R2B 1 | R2E 3, R2B 2 | R2E 4, R2B 2 |
|---|---|---|---|---|
| R2E theatre mean queue | −47.89 [−61.78, −34.00] | −46.29 [−60.68, −31.89] | −35.35 [−49.68, −21.01] | −52.02 [−66.03, −38.01] |
| Diverted, team off shift | +8.5 [−42.9, +59.8] | +37.6 [−15.4, +90.5] | −3830.6 [−3866.4, −3794.8] | −3830.6 [−3866.4, −3794.8] |
| Diverted, theatre busy | +30.2 [−10.6, +71.0] | +54.6 [−1.0, +110.3] | +1227.3 [+1172.1, +1282.4] | +1206.4 [+1146.4, +1266.4] |
| R2B surgeries | +8.0 [−13.4, +29.4] | +20.2 [−2.8, +43.2] | +1495.8 [+1462.4, +1529.2] | +1494.7 [+1466.0, +1523.4] |
| R2E first surgeries | +98.4 [+17.0, +179.7] | +163.4 [+46.4, +280.5] | −699.1 [−792.1, −606.0] | −734.4 [−834.5, −634.4] |
| R2E theatre entry deferred | +1.4 [−30.9, +33.7] | +21.1 [−14.8, +56.9] | −325.8 [−360.4, −291.2] | −331.3 [−365.5, −297.2] |
| Released with repair outstanding | −429.6 [−486.0, −373.2] | −385.2 [−451.0, −319.4] | −618.4 [−668.6, −568.2] | −1018.7 [−1068.7, −968.8] |
| Died of wounds | −190.67 [−232.21, −149.13] | −182.80 [−233.49, −132.11] | −146.83 [−189.01, −104.66] | −226.17 [−270.43, −181.90] |
| Returns to duty | +42.1 [−1.0, +85.3] | +45.2 [+1.1, +89.3] | +34.5 [−19.8, +88.8] | +49.9 [+3.5, +96.3] |
| Post-definitive ICU access (%) | +0.3 [+0.1, +0.6] | +0.2 [−0.0, +0.5] | −0.4 [−0.6, −0.1] | −0.1 [−0.3, +0.2] |
<!-- /GEN -->

At the shipped establishment the high-intensity R2E theatre queue averaged <!-- GEN cell:surgical_team_high|R2E theatre mean queue|R2E 3, R2B 1 (shipped)|full -->56.79 [42.95, 70.64]<!-- /GEN --> over the closing 90 days and was classified <!-- GEN cell:surgical_team_high|R2E theatre queue, late-half classification|R2E 3, R2B 1 (shipped)|full -->**drifting, +4.3%/block**, 17.4 to 60.5<!-- /GEN -->, a shortfall of <!-- GEN cell:surgical_team_high|R2E theatre queue shortfall per day|R2E 3, R2B 1 (shipped)|full -->0.074 [0.050, 0.099]<!-- /GEN --> casualties per day. A fourth R2E section gave <!-- GEN cell:surgical_team_high|R2E theatre mean queue|R2E 4, R2B 1|full -->8.91 [7.84, 9.98]<!-- /GEN --> and <!-- GEN cell:surgical_team_high|R2E theatre queue, late-half classification|R2E 4, R2B 1|full -->converged at 8.97<!-- /GEN -->, with a shortfall of <!-- GEN cell:surgical_team_high|R2E theatre queue shortfall per day|R2E 4, R2B 1|full -->−0.001 [−0.028, 0.025]<!-- /GEN --> per day; a sixth gave <!-- GEN cell:surgical_team_high|R2E theatre queue, late-half classification|R2E 6, R2B 1|full -->converged at 10.2<!-- /GEN --> and <!-- GEN cell:surgical_team_high|R2E theatre queue shortfall per day|R2E 6, R2B 1|full -->0.009 [−0.011, 0.030]<!-- /GEN --> per day. A second R2B section alone gave <!-- GEN cell:surgical_team_high|R2E theatre queue, late-half classification|R2E 3, R2B 2|full -->converged at 21.0<!-- /GEN --> and <!-- GEN cell:surgical_team_high|R2E theatre queue shortfall per day|R2E 3, R2B 2|full -->0.001 [−0.029, 0.031]<!-- /GEN --> per day, and both levers together <!-- GEN cell:surgical_team_high|R2E theatre queue, late-half classification|R2E 4, R2B 2|full -->converged at 4.93<!-- /GEN --> and <!-- GEN cell:surgical_team_high|R2E theatre queue shortfall per day|R2E 4, R2B 2|full -->−0.002 [−0.008, 0.004]<!-- /GEN --> per day. The paired difference in deaths of wounds was <!-- GEN cell:surgical_team_paired_high|Died of wounds|R2E 4, R2B 1|full -->−190.67 [−232.21, −149.13]<!-- /GEN --> for the fourth R2E section and <!-- GEN cell:surgical_team_paired_high|Died of wounds|R2E 4, R2B 2|full -->−226.17 [−270.43, −181.90]<!-- /GEN --> for both levers, and in returns to duty <!-- GEN cell:surgical_team_paired_high|Returns to duty|R2E 4, R2B 1|full -->+42.1 [−1.0, +85.3]<!-- /GEN --> and <!-- GEN cell:surgical_team_paired_high|Returns to duty|R2E 4, R2B 2|full -->+49.9 [+3.5, +96.3]<!-- /GEN -->.

![Two columns of panels of campaign responses against the surgical section establishment at moderate and high intensity](../images/surgical_team_sweep.png)

The R2E theatre queue and the diversions, deaths of wounds and returns to duty of each establishment, at moderate intensity (left) and high intensity (right), with the shipped establishment marked.


### R2E Intensive Care Bed Establishment

**Question.** What changes when the R2E intensive care establishment is raised from the shipped four beds, and does the post-operative overflow census, where an operated casualty finding no intensive care bed is placed, stop growing at any bed count swept? `[default and high_intensity, R2E ICU 4 to 16 beds · 360 d · 30 replications · paired, closing 90 d]`

Each arm is a bed count for the R2E intensive care pool: four (shipped), six, eight, twelve and sixteen. No stay at R2E seizes an intensive care section, so the beds are varied without staff and the nurses per bed row is the shipped three sections of four nurses over the bed count, a ratio the model does not charge. R2B intensive care beds are not varied.

<!-- GEN icu_bed -->
| Response | 4 beds (shipped) | 6 beds | 8 beds | 12 beds | 16 beds |
|---|---|---|---|---|---|
| Nurses per ICU bed at three sections | 3.00 | 2.00 | 1.50 | 1.00 | 0.75 |
| R2E post-operative overflow census | 5.30 [4.87, 5.73] | 4.12 [3.68, 4.56] | 3.36 [3.09, 3.63] | 2.00 [1.63, 2.37] | 0.93 [0.76, 1.09] |
| R2E post-operative overflow, late-half classification | converged at 5.44 | converged at 4.19 | converged at 3.30 | converged at 1.97 | converged at 1.03 |
| R2E post-operative overflow growth per day | −0.001 [−0.008, 0.007] | −0.002 [−0.005, 0.001] | 0.001 [−0.005, 0.006] | 0.000 [−0.002, 0.003] | −0.001 [−0.003, 0.001] |
| R2E ICU occupancy (%) | 86.4 [85.6, 87.2] | 81.0 [80.0, 82.1] | 75.8 [74.7, 76.9] | 65.7 [63.8, 67.6] | 54.2 [52.8, 55.5] |
| R2E holding mean queue | 22.80 [21.32, 24.28] | 23.40 [21.99, 24.82] | 24.42 [23.25, 25.60] | 23.93 [22.53, 25.34] | 22.46 [21.15, 23.76] |
| R2E holding queue, late-half classification | converged at 23.1 | converged at 23.5 | converged at 23.7 | converged at 23.8 | converged at 23.1 |
| R2E holding queue shortfall per day | −0.002 [−0.019, 0.016] | −0.004 [−0.015, 0.007] | 0.011 [−0.004, 0.027] | −0.003 [−0.019, 0.013] | −0.014 [−0.029, 0.000] |
| R2E staging overflow, late-half classification | converged at 29.5 | **drifting, -5.2%/block**, 18.5 to 25.0 | converged at 29.9 | converged at 34.9 | **drifting, -6.6%/block**, 16.2 to 26.4 |
| Post-operative care in a holding bed (%) | 36.7 [36.3, 37.1] | 29.1 [28.5, 29.7] | 23.0 [22.3, 23.7] | 13.8 [13.2, 14.3] | 7.8 [7.0, 8.5] |
| Post-definitive ICU access (%) | 35.4 [34.7, 36.1] | 50.5 [49.6, 51.4] | 62.5 [61.5, 63.4] | 78.7 [77.7, 79.7] | 88.8 [87.8, 89.8] |
| R2E theatre entry deferred | 368.8 [358.3, 379.2] | 299.1 [284.1, 314.1] | 233.8 [221.9, 245.7] | 150.2 [139.7, 160.7] | 77.8 [68.2, 87.4] |
| Died of wounds | 20.73 [17.94, 23.52] | 20.40 [18.06, 22.74] | 17.97 [15.80, 20.14] | 21.27 [18.85, 23.68] | 20.37 [18.03, 22.70] |
| Returns to duty | 2049.6 [2024.4, 2074.7] | 2066.2 [2035.1, 2097.3] | 2051.1 [2012.2, 2090.1] | 2039.9 [2012.5, 2067.3] | 2015.4 [1982.6, 2048.3] |
| Critical-route evacuation wait (days) | 5.85 [5.05, 6.64] | 5.68 [5.16, 6.20] | 5.74 [5.21, 6.27] | 6.36 [5.53, 7.18] | 6.12 [5.20, 7.03] |
| Role 4 peak census | 211.2 [207.6, 214.7] | 211.7 [206.8, 216.6] | 207.9 [203.3, 212.5] | 216.4 [212.4, 220.4] | 207.8 [203.3, 212.3] |
| Role 4 closing mean census | 116.9 [112.1, 121.6] | 115.1 [109.7, 120.6] | 119.5 [115.2, 123.7] | 119.7 [114.0, 125.5] | 116.9 [112.7, 121.1] |
<!-- /GEN -->

<!-- GEN icu_bed_paired -->
| Paired difference against the shipped establishment | 6 beds | 8 beds | 12 beds | 16 beds |
|---|---|---|---|---|
| R2E post-operative overflow census | −1.18 [−1.85, −0.52] | −1.94 [−2.45, −1.43] | −3.30 [−3.92, −2.68] | −4.38 [−4.86, −3.90] |
| R2E ICU occupancy (%) | −5.4 [−6.9, −3.8] | −10.6 [−12.0, −9.2] | −20.7 [−22.7, −18.7] | −32.2 [−33.7, −30.7] |
| R2E holding mean queue | +0.61 [−1.61, +2.83] | +1.63 [−0.22, +3.47] | +1.13 [−1.30, +3.57] | −0.34 [−2.38, +1.70] |
| Post-operative care in a holding bed (%) | −7.6 [−8.3, −6.9] | −13.7 [−14.5, −12.9] | −22.9 [−23.7, −22.2] | −28.9 [−29.8, −28.1] |
| Post-definitive ICU access (%) | +15.1 [+14.0, +16.2] | +27.1 [+25.8, +28.3] | +43.3 [+41.9, +44.8] | +53.4 [+52.2, +54.7] |
| R2E theatre entry deferred | −69.7 [−89.7, −49.7] | −135.0 [−149.9, −120.1] | −218.6 [−235.7, −201.5] | −291.0 [−304.9, −277.1] |
| Died of wounds | −0.33 [−4.02, +3.35] | −2.77 [−6.00, +0.47] | +0.53 [−3.43, +4.50] | −0.37 [−4.11, +3.38] |
| Returns to duty | +16.6 [−22.2, +55.4] | +1.6 [−38.3, +41.4] | −9.7 [−51.6, +32.3] | −34.1 [−78.7, +10.5] |
| Critical-route evacuation wait (days) | −0.16 [−1.08, +0.75] | −0.10 [−0.97, +0.77] | +0.51 [−0.69, +1.71] | +0.27 [−1.03, +1.58] |
| Role 4 peak census | +0.5 [−5.5, +6.6] | −3.3 [−8.1, +1.5] | +5.3 [−0.7, +11.3] | −3.4 [−10.2, +3.4] |
| Role 4 closing mean census | −1.8 [−9.7, +6.1] | +2.6 [−4.3, +9.5] | +2.9 [−4.5, +10.2] | +0.0 [−6.1, +6.2] |
<!-- /GEN -->

At the shipped establishment the R2E post-operative overflow census over the closing 90 days averaged <!-- GEN cell:icu_bed|R2E post-operative overflow census|4 beds (shipped)|full -->5.30 [4.87, 5.73]<!-- /GEN --> and was classified <!-- GEN cell:icu_bed|R2E post-operative overflow, late-half classification|4 beds (shipped)|full -->converged at 5.44<!-- /GEN -->, with <!-- GEN cell:icu_bed|R2E ICU occupancy (%)|4 beds (shipped)|full -->86.4 [85.6, 87.2]<!-- /GEN --> percent occupancy of the intensive care beds; <!-- GEN cell:icu_bed|Post-operative care in a holding bed (%)|4 beds (shipped)|full -->36.7 [36.3, 37.1]<!-- /GEN --> percent of post-operative care was taken in a holding bed and <!-- GEN cell:icu_bed|Post-definitive ICU access (%)|4 beds (shipped)|full -->35.4 [34.7, 36.1]<!-- /GEN --> percent of post-definitive care in an intensive care bed. At sixteen beds the census averaged <!-- GEN cell:icu_bed|R2E post-operative overflow census|16 beds|full -->0.93 [0.76, 1.09]<!-- /GEN -->. Twelve beds changed the share of post-operative care taken in a holding bed by <!-- GEN cell:icu_bed_paired|Post-operative care in a holding bed (%)|12 beds|full -->−22.9 [−23.7, −22.2]<!-- /GEN --> percentage points and the post-definitive access by <!-- GEN cell:icu_bed_paired|Post-definitive ICU access (%)|12 beds|full -->+43.3 [+41.9, +44.8]<!-- /GEN --> percentage points. The paired differences in deaths of wounds at six, eight, twelve and sixteen beds were <!-- GEN cell:icu_bed_paired|Died of wounds|6 beds|full -->−0.33 [−4.02, +3.35]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired|Died of wounds|8 beds|full -->−2.77 [−6.00, +0.47]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired|Died of wounds|12 beds|full -->+0.53 [−3.43, +4.50]<!-- /GEN --> and <!-- GEN cell:icu_bed_paired|Died of wounds|16 beds|full -->−0.37 [−4.11, +3.38]<!-- /GEN -->, and in returns to duty <!-- GEN cell:icu_bed_paired|Returns to duty|6 beds|full -->+16.6 [−22.2, +55.4]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired|Returns to duty|8 beds|full -->+1.6 [−38.3, +41.4]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired|Returns to duty|12 beds|full -->−9.7 [−51.6, +32.3]<!-- /GEN --> and <!-- GEN cell:icu_bed_paired|Returns to duty|16 beds|full -->−34.1 [−78.7, +10.5]<!-- /GEN -->. The R2E holding queue was classified <!-- GEN cell:icu_bed|R2E holding queue, late-half classification|4 beds (shipped)|full -->converged at 23.1<!-- /GEN --> at the shipped establishment and <!-- GEN cell:icu_bed|R2E holding queue, late-half classification|16 beds|full -->converged at 23.1<!-- /GEN --> at sixteen beds.

At high intensity the same five arms were run under the `high_intensity` profile (30 replications, 360 days, seed 42).

<!-- GEN icu_bed_high -->
| Response | 4 beds (shipped) | 6 beds | 8 beds | 12 beds | 16 beds |
|---|---|---|---|---|---|
| Nurses per ICU bed at three sections | 3.00 | 2.00 | 1.50 | 1.00 | 0.75 |
| R2E post-operative overflow census | 20.16 [19.96, 20.36] | 18.32 [18.03, 18.62] | 17.27 [17.01, 17.54] | 14.64 [14.40, 14.89] | 12.02 [11.75, 12.29] |
| R2E post-operative overflow, late-half classification | converged at 20.1 | converged at 18.4 | converged at 17.2 | converged at 14.6 | converged at 11.9 |
| R2E post-operative overflow growth per day | 0.001 [−0.002, 0.004] | −0.001 [−0.004, 0.002] | 0.002 [0.000, 0.004] | −0.001 [−0.003, 0.002] | 0.002 [−0.001, 0.005] |
| R2E ICU occupancy (%) | 97.6 [97.5, 97.7] | 97.3 [97.2, 97.4] | 97.0 [96.9, 97.2] | 96.5 [96.4, 96.7] | 95.6 [95.3, 95.9] |
| R2E holding mean queue | 55.40 [53.47, 57.33] | 52.99 [51.48, 54.51] | 54.22 [52.36, 56.08] | 53.32 [51.28, 55.36] | 54.60 [52.84, 56.36] |
| R2E holding queue, late-half classification | converged at 54.1 | converged at 53.9 | converged at 53.4 | converged at 53.7 | converged at 53.6 |
| R2E holding queue shortfall per day | 0.016 [−0.014, 0.047] | −0.014 [−0.032, 0.005] | 0.016 [−0.005, 0.037] | −0.002 [−0.020, 0.015] | 0.012 [−0.006, 0.029] |
| R2E staging overflow, late-half classification | **drifting, +11.4%/block**, 116.9 to 2,768.7 | **drifting, +11.1%/block**, 124.3 to 2,746.2 | **drifting, +11.2%/block**, 122.5 to 2,736.3 | **drifting, +11.1%/block**, 122.9 to 2,769.3 | **drifting, +11.3%/block**, 119.3 to 2,747.0 |
| Post-operative care in a holding bed (%) | 43.3 [43.1, 43.5] | 40.5 [40.3, 40.8] | 37.7 [37.4, 37.9] | 32.1 [31.9, 32.3] | 26.1 [25.8, 26.3] |
| Post-definitive ICU access (%) | 11.0 [10.9, 11.2] | 17.2 [16.9, 17.4] | 23.1 [22.8, 23.4] | 34.9 [34.5, 35.3] | 47.3 [46.9, 47.7] |
| R2E theatre entry deferred | 1600.9 [1577.9, 1623.8] | 1530.9 [1503.2, 1558.5] | 1433.6 [1406.7, 1460.5] | 1266.9 [1243.4, 1290.5] | 1055.5 [1031.5, 1079.4] |
| Died of wounds | 509.33 [464.71, 553.96] | 601.43 [527.16, 675.70] | 587.93 [519.80, 656.06] | 602.10 [532.93, 671.27] | 562.67 [498.14, 627.19] |
| Returns to duty | 2864.3 [2832.8, 2895.8] | 2857.5 [2822.1, 2893.0] | 2849.7 [2813.9, 2885.5] | 2839.7 [2804.3, 2875.2] | 2859.8 [2830.1, 2889.4] |
| Critical-route evacuation wait (days) | 109.60 [108.15, 111.04] | 111.43 [110.52, 112.33] | 110.83 [109.91, 111.76] | 111.17 [110.08, 112.25] | 110.27 [109.26, 111.29] |
| Role 4 peak census | 281.2 [279.0, 283.3] | 281.1 [278.8, 283.5] | 281.4 [278.8, 284.1] | 282.6 [280.1, 285.1] | 282.2 [279.7, 284.7] |
| Role 4 closing mean census | 176.2 [174.6, 177.9] | 176.6 [174.7, 178.6] | 175.8 [174.1, 177.5] | 175.9 [174.6, 177.1] | 176.4 [175.3, 177.5] |
<!-- /GEN -->

<!-- GEN icu_bed_paired_high -->
| Paired difference against the shipped establishment | 6 beds | 8 beds | 12 beds | 16 beds |
|---|---|---|---|---|
| R2E post-operative overflow census | −1.83 [−2.26, −1.41] | −2.88 [−3.19, −2.58] | −5.51 [−5.88, −5.15] | −8.13 [−8.49, −7.77] |
| R2E ICU occupancy (%) | −0.4 [−0.5, −0.2] | −0.6 [−0.8, −0.4] | −1.1 [−1.3, −0.9] | −2.0 [−2.3, −1.8] |
| R2E holding mean queue | −2.40 [−5.07, +0.26] | −1.18 [−4.35, +1.99] | −2.08 [−4.66, +0.51] | −0.80 [−3.58, +1.98] |
| Post-operative care in a holding bed (%) | −2.7 [−3.0, −2.5] | −5.6 [−6.0, −5.3] | −11.2 [−11.5, −10.9] | −17.2 [−17.6, −16.9] |
| Post-definitive ICU access (%) | +6.1 [+5.7, +6.5] | +12.1 [+11.7, +12.5] | +23.9 [+23.4, +24.3] | +36.3 [+35.8, +36.8] |
| R2E theatre entry deferred | −70.0 [−106.2, −33.8] | −167.2 [−204.3, −130.1] | −333.9 [−368.7, −299.2] | −545.4 [−577.9, −512.9] |
| Died of wounds | +92.10 [+7.00, +177.20] | +78.60 [−3.96, +161.16] | +92.77 [+19.32, +166.22] | +53.33 [−30.93, +137.59] |
| Returns to duty | −6.8 [−57.0, +43.4] | −14.6 [−72.9, +43.6] | −24.6 [−71.8, +22.6] | −4.5 [−47.5, +38.5] |
| Critical-route evacuation wait (days) | +1.83 [+0.22, +3.44] | +1.23 [−0.34, +2.81] | +1.57 [−0.33, +3.47] | +0.67 [−0.98, +2.33] |
| Role 4 peak census | −0.0 [−3.0, +2.9] | +0.3 [−3.3, +3.9] | +1.4 [−1.7, +4.6] | +1.0 [−2.8, +4.9] |
| Role 4 closing mean census | +0.4 [−2.4, +3.2] | −0.4 [−2.7, +1.8] | −0.3 [−2.4, +1.8] | +0.2 [−1.9, +2.2] |
<!-- /GEN -->

At the shipped establishment the high-intensity post-operative overflow census averaged <!-- GEN cell:icu_bed_high|R2E post-operative overflow census|4 beds (shipped)|full -->20.16 [19.96, 20.36]<!-- /GEN --> and was classified <!-- GEN cell:icu_bed_high|R2E post-operative overflow, late-half classification|4 beds (shipped)|full -->converged at 20.1<!-- /GEN -->, with <!-- GEN cell:icu_bed_high|R2E ICU occupancy (%)|4 beds (shipped)|full -->97.6 [97.5, 97.7]<!-- /GEN --> percent occupancy. At sixteen beds, four times the shipped establishment, the census averaged <!-- GEN cell:icu_bed_high|R2E post-operative overflow census|16 beds|full -->12.02 [11.75, 12.29]<!-- /GEN --> and was classified <!-- GEN cell:icu_bed_high|R2E post-operative overflow, late-half classification|16 beds|full -->converged at 11.9<!-- /GEN -->, and post-definitive intensive care access changed by <!-- GEN cell:icu_bed_paired_high|Post-definitive ICU access (%)|16 beds|full -->+36.3 [+35.8, +36.8]<!-- /GEN --> percentage points. The R2E holding queue was classified <!-- GEN cell:icu_bed_high|R2E holding queue, late-half classification|4 beds (shipped)|full -->converged at 54.1<!-- /GEN --> at the shipped establishment and <!-- GEN cell:icu_bed_high|R2E holding queue, late-half classification|16 beds|full -->converged at 53.6<!-- /GEN --> at sixteen, and the staging overflow census <!-- GEN cell:icu_bed_high|R2E staging overflow, late-half classification|4 beds (shipped)|full -->**drifting, +11.4%/block**, 116.9 to 2,768.7<!-- /GEN --> and <!-- GEN cell:icu_bed_high|R2E staging overflow, late-half classification|16 beds|full -->**drifting, +11.3%/block**, 119.3 to 2,747.0<!-- /GEN -->. The mean evacuation wait of the critical route was <!-- GEN cell:icu_bed_high|Critical-route evacuation wait (days)|4 beds (shipped)|full -->109.60 [108.15, 111.04]<!-- /GEN --> days at the shipped establishment and <!-- GEN cell:icu_bed_high|Critical-route evacuation wait (days)|16 beds|full -->110.27 [109.26, 111.29]<!-- /GEN --> days at sixteen beds. The paired differences in deaths of wounds at six, eight, twelve and sixteen beds were <!-- GEN cell:icu_bed_paired_high|Died of wounds|6 beds|full -->+92.10 [+7.00, +177.20]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired_high|Died of wounds|8 beds|full -->+78.60 [−3.96, +161.16]<!-- /GEN -->, <!-- GEN cell:icu_bed_paired_high|Died of wounds|12 beds|full -->+92.77 [+19.32, +166.22]<!-- /GEN --> and <!-- GEN cell:icu_bed_paired_high|Died of wounds|16 beds|full -->+53.33 [−30.93, +137.59]<!-- /GEN -->, and in returns to duty at sixteen beds <!-- GEN cell:icu_bed_paired_high|Returns to duty|16 beds|full -->−4.5 [−47.5, +38.5]<!-- /GEN -->.

![Two columns of panels of campaign responses against the R2E intensive care bed count at moderate and high intensity](../images/icu_bed_sweep.png)

The R2E post-operative overflow census, intensive care occupancy, holding queue, post-operative and post-definitive care and the critical-route evacuation wait of each establishment, with deaths of wounds and returns to duty, at moderate intensity (left) and high intensity (right), with the shipped establishment marked.

---

## National Support Base and Strategic Airlift

<small>[Return to Top](#contents)</small>

**Question.** How does strategic evacuation demand and its wait respond to the shipped airlift schedule, to the interval between sorties and to sortie cancellation, and what census does the national support base carry? `[default, both intensities and swept values · 360 d · 30 replications · per-replication reductions]`

<!-- GEN airlift_baseline -->
| Response at the shipped schedule | Moderate intensity | High intensity |
|---|---|---|
| Casualties boarded | 2307.80 [2256.82, 2358.78] | 3907.33 [3885.61, 3929.06] |
| Still waiting at the close | 33.23 [25.41, 41.06] | 3083.07 [3051.26, 3114.88] |
| Mean wait (days) | 5.04 [4.50, 5.58] | 53.37 [52.79, 53.94] |
| Share of R2E holding beds held by the evacuation wait | 10% [10%, 11%] | 8% [7%, 9%] |
| Role 4 peak occupancy (concurrent patients) | 211.17 [207.59, 214.75] | 281.17 [279.01, 283.33] |
| Days the peak falls before the campaign ends | 149.00 [112.79, 185.21] | 175.37 [138.44, 212.30] |
<!-- /GEN -->

At the shipped schedule <!-- GEN cell:airlift_baseline|Casualties boarded|Moderate intensity|mean -->2307.80<!-- /GEN --> casualties boarded at moderate intensity and <!-- GEN cell:airlift_baseline|Casualties boarded|High intensity|mean -->3907.33<!-- /GEN --> at high, with <!-- GEN cell:airlift_baseline|Still waiting at the close|Moderate intensity|mean -->33.23<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Still waiting at the close|High intensity|mean -->3083.07<!-- /GEN --> still waiting at the close and mean waits of <!-- GEN cell:airlift_baseline|Mean wait (days)|Moderate intensity|mean -->5.04<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Mean wait (days)|High intensity|mean -->53.37<!-- /GEN --> days. The evacuation wait held <!-- GEN cell:airlift_baseline|Share of R2E holding beds held by the evacuation wait|Moderate intensity|full -->10% [10%, 11%]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Share of R2E holding beds held by the evacuation wait|High intensity|full -->8% [7%, 9%]<!-- /GEN --> of the R2E holding beds. Role 4 peak occupancy was <!-- GEN cell:airlift_baseline|Role 4 peak occupancy (concurrent patients)|Moderate intensity|full -->211.17 [207.59, 214.75]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Role 4 peak occupancy (concurrent patients)|High intensity|full -->281.17 [279.01, 283.33]<!-- /GEN --> concurrent patients, and the peak fell <!-- GEN cell:airlift_baseline|Days the peak falls before the campaign ends|Moderate intensity|full -->149.00 [112.79, 185.21]<!-- /GEN --> and <!-- GEN cell:airlift_baseline|Days the peak falls before the campaign ends|High intensity|full -->175.37 [138.44, 212.30]<!-- /GEN --> days before the campaign ended.

<!-- GEN airlift_interval -->
| Response by interval between sorties | 3 days | 5 days | 7 days (shipped) | 10 days | 14 days |
|---|---|---|---|---|---|
| Sorties flown | 119.00 | 71.00 | 51.00 | 35.00 | 25.00 |
| Mean wait (days) | 1.66 [1.65, 1.68] | 2.92 [2.78, 3.05] | 5.04 [4.50, 5.58] | 20.88 [18.82, 22.93] | 42.48 [40.91, 44.04] |
| Share of R2E holding beds held by the evacuation wait | 6% [5%, 6%] | 8% [7%, 9%] | 10% [10%, 11%] | 22% [21%, 24%] | 35% [33%, 36%] |
| Ventilated pre-flight intensive care hold (hours) | 24.07 [23.95, 24.19] | 23.87 [23.73, 24.02] | 24.05 [23.88, 24.22] | 23.93 [23.79, 24.07] | 24.01 [23.85, 24.18] |
<!-- /GEN -->

Shortening the interval between sorties below the shipped seven days left the mean wait at <!-- GEN cell:airlift_interval|Mean wait (days)|3 days|mean -->1.66<!-- /GEN --> days at three days and <!-- GEN cell:airlift_interval|Mean wait (days)|5 days|mean -->2.92<!-- /GEN --> at five against <!-- GEN cell:airlift_interval|Mean wait (days)|7 days (shipped)|mean -->5.04<!-- /GEN --> at seven. Lengthening it gave <!-- GEN cell:airlift_interval|Mean wait (days)|10 days|full -->20.88 [18.82, 22.93]<!-- /GEN --> days at ten days with <!-- GEN cell:airlift_interval|Sorties flown|10 days|full -->35.00<!-- /GEN --> sorties flown and <!-- GEN cell:airlift_interval|Mean wait (days)|14 days|full -->42.48 [40.91, 44.04]<!-- /GEN --> days at fourteen with <!-- GEN cell:airlift_interval|Sorties flown|14 days|full -->25.00<!-- /GEN -->.

<!-- GEN airlift_reliability -->
| Response by configured cancellation probability | 0% | 5% | 10% | 15% | 25% | 40% |
|---|---|---|---|---|---|---|
| Sorties flown | 51.00 | 48.60 | 46.47 | 43.00 | 38.77 | 29.50 |
| Realised cancellation rate | 0% | 5% | 9% | 16% | 24% | 42% |
| Mean wait (days) | 4.90 [4.47, 5.32] | 6.30 [5.42, 7.18] | 8.26 [6.66, 9.85] | 12.31 [10.20, 14.43] | 18.88 [15.80, 21.95] | 38.24 [33.85, 42.63] |
| Share of R2E holding beds held by the evacuation wait | 10% [10%, 11%] | 12% [11%, 13%] | 14% [12%, 15%] | 17% [15%, 18%] | 21% [18%, 23%] | 31% [29%, 33%] |
<!-- /GEN -->

![Eight panels of strategic evacuation responses against sortie cancellation probability and the interval between sorties](../images/airlift_sweeps.png)

Wait for a sortie, casualties still waiting at the horizon, Role 4 peak beds and the share of holding bed-days spent awaiting a sortie against sortie cancellation probability and the interval between sorties, with the shipped value of each marked by a dashed line.

The mean wait rose from <!-- GEN cell:airlift_reliability|Mean wait (days)|0%|mean -->4.90<!-- /GEN --> days with no cancellation to <!-- GEN cell:airlift_reliability|Mean wait (days)|10%|mean -->8.26<!-- /GEN --> at 10%, <!-- GEN cell:airlift_reliability|Mean wait (days)|25%|mean -->18.88<!-- /GEN --> at 25% and <!-- GEN cell:airlift_reliability|Mean wait (days)|40%|mean -->38.24<!-- /GEN --> at 40%, as the sorties flown fell from <!-- GEN cell:airlift_reliability|Sorties flown|0%|full -->51.00<!-- /GEN --> to <!-- GEN cell:airlift_reliability|Sorties flown|40%|full -->29.50<!-- /GEN -->.

**High-intensity profile.** The same two sweeps under `high_intensity` `[high_intensity, interval 3 to 14 days and cancellation 0 to 40% · 360 d · 30 replications · campaign means]`:

<!-- GEN airlift_interval_high -->
| Response by interval between sorties | 3 days | 5 days | 7 days (shipped) | 10 days | 14 days |
|---|---|---|---|---|---|
| Sorties flown | 119.00 | 71.00 | 51.00 | 35.00 | 25.00 |
| Mean wait (days) | 15.57 [14.45, 16.68] | 46.65 [45.95, 47.35] | 53.33 [52.74, 53.93] | 63.01 [61.66, 64.36] | 97.22 [95.63, 98.81] |
| Share of R2E holding beds held by the evacuation wait | 3% [2%, 3%] | 6% [5%, 7%] | 8% [7%, 9%] | 10% [9%, 11%] | 10% [9%, 11%] |
| Ventilated pre-flight intensive care hold (hours) | 23.94 [23.86, 24.02] | 24.01 [23.93, 24.10] | 24.02 [23.93, 24.11] | 23.95 [23.87, 24.03] | 23.98 [23.91, 24.06] |
<!-- /GEN -->

At the shipped seven-day interval the mean wait was <!-- GEN cell:airlift_interval_high|Mean wait (days)|7 days (shipped)|mean -->53.33<!-- /GEN --> days. Shortening the interval to five days gave <!-- GEN cell:airlift_interval_high|Mean wait (days)|5 days|mean -->46.65<!-- /GEN --> days and to three days <!-- GEN cell:airlift_interval_high|Mean wait (days)|3 days|mean -->15.57<!-- /GEN --> days with <!-- GEN cell:airlift_interval_high|Sorties flown|3 days|mean -->119.00<!-- /GEN --> sorties flown, while lengthening it to ten and fourteen days gave <!-- GEN cell:airlift_interval_high|Mean wait (days)|10 days|mean -->63.01<!-- /GEN --> and <!-- GEN cell:airlift_interval_high|Mean wait (days)|14 days|mean -->97.22<!-- /GEN --> days.

<!-- GEN airlift_reliability_high -->
| Response by configured cancellation probability | 0% | 5% | 10% | 15% | 25% | 40% |
|---|---|---|---|---|---|---|
| Sorties flown | 51.00 | 48.53 | 45.27 | 43.07 | 37.57 | 30.73 |
| Realised cancellation rate | 0% | 5% | 11% | 16% | 26% | 40% |
| Mean wait (days) | 53.37 [52.79, 53.94] | 54.22 [53.56, 54.88] | 55.89 [54.87, 56.92] | 56.95 [54.99, 58.92] | 65.01 [61.47, 68.55] | 80.87 [75.32, 86.42] |
| Share of R2E holding beds held by the evacuation wait | 8% [7%, 9%] | 8% [7%, 10%] | 8% [7%, 10%] | 8% [7%, 10%] | 10% [9%, 12%] | 10% [8%, 11%] |
<!-- /GEN -->

The mean wait rose from <!-- GEN cell:airlift_reliability_high|Mean wait (days)|0%|mean -->53.37<!-- /GEN --> days with no cancellation to <!-- GEN cell:airlift_reliability_high|Mean wait (days)|10%|mean -->55.89<!-- /GEN --> at 10%, <!-- GEN cell:airlift_reliability_high|Mean wait (days)|25%|mean -->65.01<!-- /GEN --> at 25% and <!-- GEN cell:airlift_reliability_high|Mean wait (days)|40%|mean -->80.87<!-- /GEN --> at 40%, as the sorties flown fell from <!-- GEN cell:airlift_reliability_high|Sorties flown|0%|mean -->51.00<!-- /GEN --> to <!-- GEN cell:airlift_reliability_high|Sorties flown|40%|mean -->30.73<!-- /GEN -->.

![Eight panels of strategic evacuation responses against sortie cancellation probability and the interval between sorties at high intensity](../images/airlift_sweeps_high_intensity.png)

The panels of the figure above under the high-intensity profile, with the shipped value of each sweep marked by a dashed line.

**Collapse classification.** Each airlift route was classified separately, the critical and standard seat pools filling independently. A route was classified as collapsed in a campaign where the backlog of casualties awaiting a place on it was still rising over the second half of the campaign, by the late-half trend rule of [Classifying Stability](Methods.md#classifying-stability); a casualty left the backlog on boarding or on dying. Utilisation is the route's demand over the campaign divided by the places its flown sorties opened. `[default, sortie cancellation 0 to 25% · 360 d · 30 replications · exact binomial]`

<!-- GEN airlift_collapse -->
| Route and sortie cancellation | Collapsed | Collapse rate (exact 95% interval) | Mean utilisation | Demand at or above places flown | Median closing backlog | Worst closing backlog |
|---|---|---|---|---|---|---|
| Critical, 0% | 3 of 30 | 10.0% [2.1%, 26.5%] | 0.83 | 0 of 30 | 16.8 | 46.1 |
| Critical, 5% | 2 of 30 | 6.7% [0.8%, 22.1%] | 0.87 | 0 of 30 | 22.8 | 101.2 |
| Critical, 10% | 3 of 30 | 10.0% [2.1%, 26.5%] | 0.91 | 2 of 30 | 27.2 | 219.1 |
| Critical, 15% | 10 of 30 | 33.3% [17.3%, 52.8%] | 1.01 | 15 of 30 | 82.8 | 363.4 |
| Critical, 20% | 13 of 30 | 43.3% [25.5%, 62.6%] | 1.07 | 26 of 30 | 115.2 | 379.0 |
| Critical, 25% | 16 of 30 | 53.3% [34.3%, 71.7%] | 1.13 | 26 of 30 | 152.9 | 439.2 |
| Standard, 0% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.30 | 0 of 30 | 6.5 | 8.5 |
| Standard, 5% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.31 | 0 of 30 | 7.2 | 9.6 |
| Standard, 10% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.33 | 0 of 30 | 7.5 | 14.1 |
| Standard, 15% | 1 of 30 | 3.3% [0.1%, 17.2%] | 0.36 | 0 of 30 | 10.1 | 20.7 |
| Standard, 20% | 2 of 30 | 6.7% [0.8%, 22.1%] | 0.38 | 0 of 30 | 11.0 | 35.3 |
| Standard, 25% | 0 of 30 | 0.0% [0.0%, 11.6%] | 0.40 | 0 of 30 | 12.8 | 46.4 |
<!-- /GEN -->

![Line plot of the share of campaigns collapsing against sortie cancellation probability](../images/airlift_collapse.png)

The share of campaigns in which each route was classified as collapsed against sortie cancellation probability, with exact binomial 95% confidence intervals.

The critical route collapsed in <!-- GEN cell:airlift_collapse|Critical, 0%|Collapsed|full -->3 of 30<!-- /GEN --> campaigns with no cancellation, <!-- GEN cell:airlift_collapse|Critical, 15%|Collapsed|full -->10 of 30<!-- /GEN --> at 15% and <!-- GEN cell:airlift_collapse|Critical, 25%|Collapsed|full -->16 of 30<!-- /GEN --> at 25%, its mean utilisation rising from <!-- GEN cell:airlift_collapse|Critical, 0%|Mean utilisation|full -->0.83<!-- /GEN --> to <!-- GEN cell:airlift_collapse|Critical, 25%|Mean utilisation|full -->1.13<!-- /GEN -->. Demand met or exceeded the critical places flown in <!-- GEN cell:airlift_collapse|Critical, 15%|Demand at or above places flown|full -->15 of 30<!-- /GEN --> campaigns at 15% cancellation. The standard route collapsed in <!-- GEN cell:airlift_collapse|Standard, 25%|Collapsed|full -->0 of 30<!-- /GEN --> campaigns at 25%, at a mean utilisation of <!-- GEN cell:airlift_collapse|Standard, 25%|Mean utilisation|full -->0.40<!-- /GEN -->.

**High-intensity profile.** The same classification under `high_intensity` `[high_intensity, sortie cancellation 0 to 25% · 360 d · 30 replications per arm · exact binomial interval]`:

<!-- GEN airlift_collapse_high -->
| Route and sortie cancellation | Collapsed | Collapse rate (exact 95% interval) | Mean utilisation | Demand at or above places flown | Median closing backlog | Worst closing backlog |
|---|---|---|---|---|---|---|
| Critical, 0% | 30 of 30 | 100.0% [88.4%, 100.0%] | 2.67 | 30 of 30 | 2,528.7 | 2,646.9 |
| Critical, 5% | 30 of 30 | 100.0% [88.4%, 100.0%] | 2.78 | 30 of 30 | 2,569.5 | 2,755.6 |
| Critical, 10% | 30 of 30 | 100.0% [88.4%, 100.0%] | 3.00 | 30 of 30 | 2,735.6 | 3,009.2 |
| Critical, 15% | 30 of 30 | 100.0% [88.4%, 100.0%] | 3.14 | 30 of 30 | 2,777.6 | 2,986.7 |
| Critical, 20% | 30 of 30 | 100.0% [88.4%, 100.0%] | 3.30 | 30 of 30 | 2,796.7 | 3,052.4 |
| Critical, 25% | 30 of 30 | 100.0% [88.4%, 100.0%] | 3.60 | 30 of 30 | 2,886.1 | 3,208.9 |
| Standard, 0% | 1 of 30 | 3.3% [0.1%, 17.2%] | 0.76 | 0 of 30 | 17.5 | 22.0 |
| Standard, 5% | 1 of 30 | 3.3% [0.1%, 17.2%] | 0.80 | 0 of 30 | 19.9 | 49.8 |
| Standard, 10% | 2 of 30 | 6.7% [0.8%, 22.1%] | 0.86 | 0 of 30 | 31.8 | 121.3 |
| Standard, 15% | 5 of 30 | 16.7% [5.6%, 34.7%] | 0.90 | 1 of 30 | 40.3 | 198.7 |
| Standard, 20% | 11 of 30 | 36.7% [19.9%, 56.1%] | 0.95 | 5 of 30 | 76.1 | 187.4 |
| Standard, 25% | 11 of 30 | 36.7% [19.9%, 56.1%] | 1.03 | 14 of 30 | 143.4 | 473.0 |
<!-- /GEN -->

Under `high_intensity` the critical route collapsed in <!-- GEN cell:airlift_collapse_high|Critical, 0%|Collapsed|full -->30 of 30<!-- /GEN --> campaigns with no cancellation, at a mean utilisation of <!-- GEN cell:airlift_collapse_high|Critical, 0%|Mean utilisation|full -->2.67<!-- /GEN -->, and the standard route in <!-- GEN cell:airlift_collapse_high|Standard, 0%|Collapsed|full -->1 of 30<!-- /GEN -->, at <!-- GEN cell:airlift_collapse_high|Standard, 0%|Mean utilisation|full -->0.76<!-- /GEN -->.

---

## Role 4 Bed Demand

<small>[Return to Top](#contents)</small>

**Question.** How many national support base beds does a campaign commit, what is the demand made of, has it settled by the closing window, and how do the forward evacuation levers move it? `[default, both intensities, the evacuation policy, R2E holding establishment, saturation release and sortie cancellation sweeps · 360 d · 30 replications · daily census, mean, peak and closing 90 d]`

The model gives the national support base no capacity, queue or shortfall, so every figure here is demand: the casualties in a Role 4 bed on a day, counted as the other sections count the Role 4 peak. The mean is taken over the whole campaign, the peak is the busiest day of one campaign, and the closing mean is the mean over the last 90 days. A casualty is counted by the ward phase it is in, intensive care or step-down, and by its origin, battle injury, disease and non-battle injury or the reconstruction cohort.

<!-- GEN role4_census_ward -->
| Census by ward phase | Moderate intensity | High intensity |
|---|---|---|
| Total, mean beds | 138.85 [135.44, 142.26] | 210.72 [209.81, 211.64] |
| Total, peak beds | 211.17 [207.59, 214.75] | 281.17 [279.01, 283.33] |
| Total, closing 90-day mean beds | 144.43 [138.39, 150.47] | 221.51 [220.06, 222.95] |
| Intensive care phase, mean beds | 33.52 [32.60, 34.45] | 42.76 [42.54, 42.97] |
| Intensive care phase, peak beds | 64.00 [63.26, 64.74] | 67.20 [66.51, 67.89] |
| Intensive care phase, closing 90-day mean beds | 33.87 [32.32, 35.43] | 43.93 [43.55, 44.32] |
| Step-down ward phase, mean beds | 105.33 [102.80, 107.85] | 167.97 [167.08, 168.85] |
| Step-down ward phase, peak beds | 155.40 [152.15, 158.65] | 224.30 [221.93, 226.67] |
| Step-down ward phase, closing 90-day mean beds | 110.56 [105.97, 115.15] | 177.57 [176.06, 179.09] |
<!-- /GEN -->

At moderate intensity the census averaged <!-- GEN cell:role4_census_ward|Total, mean beds|Moderate intensity|full -->138.85 [135.44, 142.26]<!-- /GEN --> beds over the campaign, peaked at <!-- GEN cell:role4_census_ward|Total, peak beds|Moderate intensity|full -->211.17 [207.59, 214.75]<!-- /GEN --> and averaged <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|Moderate intensity|full -->144.43 [138.39, 150.47]<!-- /GEN --> over the closing 90 days. At high intensity the three figures were <!-- GEN cell:role4_census_ward|Total, mean beds|High intensity|full -->210.72 [209.81, 211.64]<!-- /GEN -->, <!-- GEN cell:role4_census_ward|Total, peak beds|High intensity|full -->281.17 [279.01, 283.33]<!-- /GEN --> and <!-- GEN cell:role4_census_ward|Total, closing 90-day mean beds|High intensity|full -->221.51 [220.06, 222.95]<!-- /GEN -->. The closing mean exceeded the campaign mean at both intensities. The step-down ward phase carried <!-- GEN cell:role4_census_ward|Step-down ward phase, mean beds|Moderate intensity|mean -->105.33<!-- /GEN --> of the <!-- GEN cell:role4_census_ward|Total, mean beds|Moderate intensity|mean -->138.85<!-- /GEN --> mean beds at moderate intensity and <!-- GEN cell:role4_census_ward|Step-down ward phase, mean beds|High intensity|mean -->167.97<!-- /GEN --> of the <!-- GEN cell:role4_census_ward|Total, mean beds|High intensity|mean -->210.72<!-- /GEN --> at high, and the intensive care phase the remainder.

![Daily Role 4 census over a 360-day campaign at each casualty intensity, divided by ward phase and by origin](../images/role4_demand_census.png)

The mean daily census across replications with its 95% interval, at each casualty intensity, divided by ward phase (upper row) and by origin (lower row), each beside the total it sums to. At high intensity the census rises and falls in a regular cycle about its mean.

<!-- GEN role4_census_origin -->
| Census by origin | Moderate intensity | High intensity |
|---|---|---|
| Total, mean beds | 138.85 [135.44, 142.26] | 210.72 [209.81, 211.64] |
| Total, peak beds | 211.17 [207.59, 214.75] | 281.17 [279.01, 283.33] |
| Total, closing 90-day mean beds | 144.43 [138.39, 150.47] | 221.51 [220.06, 222.95] |
| Battle injury, mean beds | 97.30 [94.46, 100.14] | 173.67 [172.62, 174.71] |
| Battle injury, peak beds | 157.63 [153.59, 161.68] | 237.53 [235.17, 239.90] |
| Battle injury, closing 90-day mean beds | 100.64 [95.48, 105.80] | 182.93 [180.97, 184.89] |
| Disease and non-battle injury, mean beds | 18.80 [18.31, 19.30] | 10.76 [10.45, 11.06] |
| Disease and non-battle injury, peak beds | 36.27 [35.00, 37.53] | 23.40 [22.62, 24.18] |
| Disease and non-battle injury, closing 90-day mean beds | 19.49 [18.61, 20.37] | 11.16 [10.42, 11.90] |
| Reconstruction cohort, mean beds | 22.75 [22.14, 23.35] | 26.30 [25.92, 26.68] |
| Reconstruction cohort, peak beds | 38.57 [37.22, 39.91] | 41.27 [40.22, 42.31] |
| Reconstruction cohort, closing 90-day mean beds | 24.31 [22.99, 25.62] | 27.42 [26.37, 28.46] |
<!-- /GEN -->

Battle injury accounted for <!-- GEN cell:role4_census_origin|Battle injury, mean beds|Moderate intensity|mean -->97.30<!-- /GEN --> of the mean beds at moderate intensity and <!-- GEN cell:role4_census_origin|Battle injury, mean beds|High intensity|mean -->173.67<!-- /GEN --> at high. Disease and non-battle injury accounted for <!-- GEN cell:role4_census_origin|Disease and non-battle injury, mean beds|Moderate intensity|mean -->18.80<!-- /GEN --> and <!-- GEN cell:role4_census_origin|Disease and non-battle injury, mean beds|High intensity|mean -->10.76<!-- /GEN -->, and the reconstruction cohort for <!-- GEN cell:role4_census_origin|Reconstruction cohort, mean beds|Moderate intensity|mean -->22.75<!-- /GEN --> and <!-- GEN cell:role4_census_origin|Reconstruction cohort, mean beds|High intensity|mean -->26.30<!-- /GEN -->.

<!-- GEN role4_operations -->
| Demand owed alongside the census | Moderate intensity | High intensity |
|---|---|---|
| Operations owed within the 360 days | 1,188 [1,142, 1,233] | 1,998 [1,971, 2,025] |
| Definitive repairs owed within the 360 days | 126 [110, 141] | 732 [719, 746] |
| Debridements owed within the 360 days | 767 [740, 793] | 924 [905, 943] |
| Reconstructions owed within the 360 days | 295 [287, 303] | 342 [337, 346] |
| Operations owed after day 360 | 20 [17, 22] | 26 [22, 29] |
| Operations owed by casualties admitted during the campaign | 1,207 [1,162, 1,253] | 2,024 [1,995, 2,053] |
| Operations owed on the busiest day | 20.97 [19.63, 22.30] | 27.20 [26.44, 27.96] |
| Closing 90-day mean operations owed per day | 3.40 [3.19, 3.60] | 5.82 [5.67, 5.97] |
| Theatre minutes owed for definitive repairs | 15,546 [13,682, 17,409] | 90,775 [89,116, 92,433] |
<!-- /GEN -->

The operations owed alongside the census were counted three ways. <!-- GEN cell:role4_operations|Operations owed within the 360 days|Moderate intensity|mean -->1,188<!-- /GEN --> fell inside the campaign at moderate intensity and <!-- GEN cell:role4_operations|Operations owed within the 360 days|High intensity|mean -->1,998<!-- /GEN --> at high, a further <!-- GEN cell:role4_operations|Operations owed after day 360|Moderate intensity|mean -->20<!-- /GEN --> and <!-- GEN cell:role4_operations|Operations owed after day 360|High intensity|mean -->26<!-- /GEN --> fell after the campaign ended, in the reconstruction sequences of casualties admitted late, and <!-- GEN cell:role4_operations|Operations owed by casualties admitted during the campaign|Moderate intensity|mean -->1,207<!-- /GEN --> and <!-- GEN cell:role4_operations|Operations owed by casualties admitted during the campaign|High intensity|mean -->2,024<!-- /GEN --> were owed in all, the figure the other sections report as operations owed. By source, the operations inside the campaign were <!-- GEN cell:role4_operations|Definitive repairs owed within the 360 days|Moderate intensity|mean -->126<!-- /GEN --> definitive repairs, <!-- GEN cell:role4_operations|Debridements owed within the 360 days|Moderate intensity|mean -->767<!-- /GEN --> debridements and <!-- GEN cell:role4_operations|Reconstructions owed within the 360 days|Moderate intensity|mean -->295<!-- /GEN --> reconstructions at moderate intensity, and <!-- GEN cell:role4_operations|Definitive repairs owed within the 360 days|High intensity|mean -->732<!-- /GEN -->, <!-- GEN cell:role4_operations|Debridements owed within the 360 days|High intensity|mean -->924<!-- /GEN --> and <!-- GEN cell:role4_operations|Reconstructions owed within the 360 days|High intensity|mean -->342<!-- /GEN --> at high. The busiest day owed <!-- GEN cell:role4_operations|Operations owed on the busiest day|Moderate intensity|mean -->20.97<!-- /GEN --> operations at moderate intensity and <!-- GEN cell:role4_operations|Operations owed on the busiest day|High intensity|mean -->27.20<!-- /GEN --> at high. The theatre minutes cover the definitive repairs only, no source reporting the time a debridement or a flap takes.

![Mean operations owed per day at Role 4 over each week of a 360-day campaign at each casualty intensity, by source and in all](../images/role4_demand_operations.png)

The mean operations owed per day over each week of the campaign, with the 95% interval across replications, at each casualty intensity, for the definitive repairs, the debridements and the reconstructions and for their total.

<!-- GEN role4_stability -->
| Census | Moderate intensity classification | Moderate intensity settles by block | Moderate intensity late mean beds | High intensity classification | High intensity settles by block | High intensity late mean beds |
|---|---|---|---|---|---|---|
| Total | converged | 2 | 146.31 | converged | 4 | 220.71 |
| Intensive care phase | converged | 6 | 34.35 | converged | 2 | 43.89 |
| Step-down ward phase | converged | 3 | 111.96 | converged | 5 | 176.82 |
| Battle injury | drifting | none | 102.44 | converged | 4 | 182.20 |
| Disease and non-battle injury | converged | 3 | 19.70 | converged | 5 | 11.13 |
| Reconstruction cohort | converged | 2 | 24.17 | converged | 2 | 27.38 |
<!-- /GEN -->

<!-- GEN role4_cma -->
| Cumulative moving average of the mean total census (beds) | Moderate intensity | High intensity |
|---|---|---|
| Day 30 | 56.19 | 103.33 |
| Day 90 | 115.33 | 179.51 |
| Day 180 | 132.23 | 200.73 |
| Day 360 | 138.85 | 210.72 |
<!-- /GEN -->

The 30-day block means of the total census were classified <!-- GEN cell:role4_stability|Total|Moderate intensity classification|full -->converged<!-- /GEN --> at moderate intensity, settling by block <!-- GEN cell:role4_stability|Total|Moderate intensity settles by block|full -->2<!-- /GEN -->, and <!-- GEN cell:role4_stability|Total|High intensity classification|full -->converged<!-- /GEN --> at high, settling by block <!-- GEN cell:role4_stability|Total|High intensity settles by block|full -->4<!-- /GEN -->. The reconstruction cohort was classified <!-- GEN cell:role4_stability|Reconstruction cohort|Moderate intensity classification|full -->converged<!-- /GEN --> at moderate intensity and <!-- GEN cell:role4_stability|Reconstruction cohort|High intensity classification|full -->converged<!-- /GEN --> at high. The cumulative moving average of the mean total census was <!-- GEN cell:role4_cma|Day 90|Moderate intensity|full -->115.33<!-- /GEN --> beds at day 90 and <!-- GEN cell:role4_cma|Day 360|Moderate intensity|full -->138.85<!-- /GEN --> at day 360 at moderate intensity, and <!-- GEN cell:role4_cma|Day 90|High intensity|full -->179.51<!-- /GEN --> and <!-- GEN cell:role4_cma|Day 360|High intensity|full -->210.72<!-- /GEN --> at high.

**Response to the forward levers.** The three sweeps below carry the Role 4 peak, the closing 90-day mean and the operations owed by the casualties admitted in their own evidence sets, and are read from them without a further run; the sortie cancellation sweep was measured for this section, the strategic airlift evidence set carrying the peak alone. Each table's shipped column is the configuration of the census tables above at moderate intensity.

<!-- GEN role4_levers_policy -->
| Response | 15 d | 21 d (shipped) | 30 d | 45 d | 60 d |
|---|---|---|---|---|---|
| Role 4 peak beds | 228.5 [222.9, 234.1] | 211.2 [207.6, 214.7] | 197.5 [193.1, 201.9] | 178.3 [174.3, 182.3] | 153.1 [148.9, 157.2] |
| Role 4 closing 90-day mean beds | 125.1 [121.0, 129.2] | 116.9 [112.1, 121.6] | 111.7 [106.5, 116.9] | 96.7 [92.5, 101.0] | 80.2 [76.9, 83.5] |
| Role 4 operations owed | 1222 [1169, 1275] | 1207 [1162, 1253] | 1235 [1186, 1284] | 1217 [1167, 1266] | 1218 [1175, 1261] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|15 d|mean -->125.1<!-- /GEN --> beds at 15 days, <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|21 d (shipped)|mean -->116.9<!-- /GEN --> at the shipped 21 days, <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|30 d|mean -->111.7<!-- /GEN --> at 30, <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|45 d|mean -->96.7<!-- /GEN --> at 45 and <!-- GEN cell:role4_levers_policy|Role 4 closing 90-day mean beds|60 d|mean -->80.2<!-- /GEN --> at 60. The peak was <!-- GEN cell:role4_levers_policy|Role 4 peak beds|21 d (shipped)|mean -->211.2<!-- /GEN --> at 21 days and <!-- GEN cell:role4_levers_policy|Role 4 peak beds|30 d|mean -->197.5<!-- /GEN -->, <!-- GEN cell:role4_levers_policy|Role 4 peak beds|45 d|mean -->178.3<!-- /GEN --> and <!-- GEN cell:role4_levers_policy|Role 4 peak beds|60 d|mean -->153.1<!-- /GEN --> at 30, 45 and 60. The operations owed were <!-- GEN cell:role4_levers_policy|Role 4 operations owed|21 d (shipped)|mean -->1207<!-- /GEN --> at 21 days and <!-- GEN cell:role4_levers_policy|Role 4 operations owed|60 d|mean -->1218<!-- /GEN --> at 60.

<!-- GEN role4_levers_establishment -->
| Response | 30 beds (shipped) | 45 beds | 60 beds | 90 beds |
|---|---|---|---|---|
| Role 4 peak beds | 211.2 [207.6, 214.7] | 207.9 [203.4, 212.5] | 211.3 [206.7, 215.9] | 211.2 [206.3, 216.0] |
| Role 4 closing 90-day mean beds | 116.9 [112.1, 121.6] | 117.7 [113.3, 122.2] | 122.0 [118.3, 125.8] | 117.6 [112.6, 122.6] |
| Role 4 operations owed | 1207 [1162, 1253] | 1183 [1134, 1233] | 1223 [1175, 1271] | 1201 [1140, 1262] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|30 beds (shipped)|mean -->116.9<!-- /GEN --> beds at the shipped 30 R2E holding beds and <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|45 beds|mean -->117.7<!-- /GEN -->, <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|60 beds|mean -->122.0<!-- /GEN --> and <!-- GEN cell:role4_levers_establishment|Role 4 closing 90-day mean beds|90 beds|mean -->117.6<!-- /GEN --> at 45, 60 and 90; the four intervals overlap. The peak was <!-- GEN cell:role4_levers_establishment|Role 4 peak beds|30 beds (shipped)|mean -->211.2<!-- /GEN --> at 30 beds and <!-- GEN cell:role4_levers_establishment|Role 4 peak beds|90 beds|mean -->211.2<!-- /GEN --> at 90.

<!-- GEN role4_levers_saturation -->
| Response | 0 (disabled) | 1 | 2 (shipped) | 3 | 5 | 8 | 12 | 16 | 24 |
|---|---|---|---|---|---|---|---|---|---|
| Role 4 peak beds | 211.3 [206.2, 216.5] | 208.6 [203.1, 214.0] | 211.2 [207.6, 214.7] | 209.6 [205.0, 214.2] | 214.9 [210.6, 219.2] | 211.2 [206.0, 216.4] | 209.6 [204.4, 214.8] | 211.6 [206.4, 216.7] | 214.1 [209.6, 218.7] |
| Role 4 closing 90-day mean beds | 120.5 [115.6, 125.3] | 117.2 [111.9, 122.5] | 116.9 [112.1, 121.6] | 116.5 [112.2, 120.9] | 120.5 [115.6, 125.4] | 119.7 [114.3, 125.1] | 115.7 [111.2, 120.2] | 118.0 [113.2, 122.9] | 116.9 [112.4, 121.3] |
| Role 4 operations owed | 1063 [1032, 1095] | 1255 [1199, 1312] | 1207 [1162, 1253] | 1164 [1114, 1215] | 1174 [1130, 1217] | 1148 [1108, 1189] | 1126 [1097, 1155] | 1139 [1097, 1181] | 1105 [1072, 1137] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_saturation|Role 4 closing 90-day mean beds|0 (disabled)|mean -->120.5<!-- /GEN --> beds with the release disabled and <!-- GEN cell:role4_levers_saturation|Role 4 closing 90-day mean beds|2 (shipped)|mean -->116.9<!-- /GEN --> at the shipped threshold of 2, and ranged across the nine thresholds without a monotone course. The operations owed were <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|0 (disabled)|mean -->1063<!-- /GEN --> with the release disabled, <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|1|mean -->1255<!-- /GEN --> at a threshold of 1, <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|2 (shipped)|mean -->1207<!-- /GEN --> at 2 and <!-- GEN cell:role4_levers_saturation|Role 4 operations owed|24|mean -->1105<!-- /GEN --> at 24.

<!-- GEN role4_levers_cancellation -->
| Response | 0% (shipped) | 5% | 10% | 15% | 25% | 40% |
|---|---|---|---|---|---|---|
| Role 4 peak beds | 211.2 [207.6, 214.7] | 212.0 [208.1, 215.9] | 210.8 [207.0, 214.6] | 212.8 [207.8, 217.8] | 211.2 [206.3, 216.1] | 200.5 [195.5, 205.6] |
| Role 4 closing 90-day mean beds | 144.4 [138.4, 150.5] | 146.9 [140.4, 153.4] | 144.7 [138.7, 150.6] | 143.3 [137.6, 149.0] | 135.0 [129.0, 140.9] | 115.1 [108.2, 122.0] |
| Role 4 operations owed | 1207 [1162, 1253] | 1200 [1158, 1242] | 1185 [1141, 1230] | 1172 [1136, 1208] | 1100 [1059, 1141] | 836 [790, 881] |
<!-- /GEN -->

The closing mean was <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|0% (shipped)|mean -->144.4<!-- /GEN --> beds with no sortie cancelled and <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|10%|mean -->144.7<!-- /GEN -->, <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|25%|mean -->135.0<!-- /GEN --> and <!-- GEN cell:role4_levers_cancellation|Role 4 closing 90-day mean beds|40%|mean -->115.1<!-- /GEN --> at cancellation probabilities of 10%, 25% and 40%. The peak was <!-- GEN cell:role4_levers_cancellation|Role 4 peak beds|0% (shipped)|mean -->211.2<!-- /GEN --> with no cancellation and <!-- GEN cell:role4_levers_cancellation|Role 4 peak beds|40%|mean -->200.5<!-- /GEN --> at 40%, and the operations owed were <!-- GEN cell:role4_levers_cancellation|Role 4 operations owed|0% (shipped)|mean -->1207<!-- /GEN --> and <!-- GEN cell:role4_levers_cancellation|Role 4 operations owed|40%|mean -->836<!-- /GEN -->.

![Role 4 peak beds, closing 90-day mean beds and operations owed against the evacuation policy, R2E holding establishment, saturation release threshold and sortie cancellation probability](../images/role4_demand_levers.png)

The Role 4 peak beds (upper row), closing 90-day mean beds (middle row) and operations owed (lower row), with 95% intervals across replications, against each lever, with the shipped value of each marked by a dashed line.

---

## Casualty Surge Events

<small>[Return to Top](#contents)</small>

**Question.** What changes when casualty surge events are injected into a campaign? `[default, injection off and on at 0.2 events per day · 360 d · 30 replications per arm · independent seeds, pooled exact binomial]`

<!-- GEN casualty_surge -->
| Metric | No events injected | Events injected |
|---|---|---|
| Average total casualties/run | 5362.3 | 8157.7 |
| Average events/run | 0 | 75.20 (range 58–88) |
| Died-of-wounds rate, ordinary casualties | 0.39% [0.36%, 0.42%] | 1.14% [1.09%, 1.20%] |
| Died-of-wounds rate, event casualties | not applicable | 2.41% [2.31%, 2.52%] |
<!-- /GEN -->

![Stem plot of casualty surge events reconstructed from one campaign](../images/casualty_surge_events.png)

Casualty surge events reconstructed from one seed-42 campaign, each drawn as a vertical line at its simulation day with a point at its casualty count.

Events added <!-- GEN cell:casualty_surge|Average events/run|Events injected|full -->75.20 (range 58–88)<!-- /GEN --> events per campaign and raised the mean campaign total from <!-- GEN cell:casualty_surge|Average total casualties/run|No events injected|full -->5362.3<!-- /GEN --> to <!-- GEN cell:casualty_surge|Average total casualties/run|Events injected|full -->8157.7<!-- /GEN --> casualties. The died-of-wounds rate among event casualties was <!-- GEN cell:casualty_surge|Died-of-wounds rate, event casualties|Events injected|full -->2.41% [2.31%, 2.52%]<!-- /GEN -->. Among ordinary casualties it was <!-- GEN cell:casualty_surge|Died-of-wounds rate, ordinary casualties|No events injected|full -->0.39% [0.36%, 0.42%]<!-- /GEN --> without injection and <!-- GEN cell:casualty_surge|Died-of-wounds rate, ordinary casualties|Events injected|full -->1.14% [1.09%, 1.20%]<!-- /GEN --> with it.

**High-intensity profile.** The same stress test under `high_intensity` `[high_intensity, 0 and 0.2 events per day · 360 d · 30 replications · campaign totals]`:

<!-- GEN casualty_surge_high -->
| Metric | No events injected | Events injected |
|---|---|---|
| Average total casualties/run | 12351.9 | 15079.9 |
| Average events/run | 0 | 75.20 (range 58–88) |
| Died-of-wounds rate, ordinary casualties | 4.10% [4.03%, 4.16%] | 14.60% [14.48%, 14.71%] |
| Died-of-wounds rate, event casualties | not applicable | 16.66% [16.42%, 16.91%] |
<!-- /GEN -->

Event casualties died of wounds at <!-- GEN cell:casualty_surge_high|Died-of-wounds rate, event casualties|Events injected|full -->16.66% [16.42%, 16.91%]<!-- /GEN --> against <!-- GEN cell:casualty_surge_high|Died-of-wounds rate, ordinary casualties|Events injected|full -->14.60% [14.48%, 14.71%]<!-- /GEN --> for ordinary casualties in the same arm.

---

## Casualty Surge Event Size

<small>[Return to Top](#contents)</small>

**Question.** At what size do casualty surge events begin to degrade care? `[default, every event fixed at one size · 0.2 events per day · 360 d · 30 replications per size · independent seeds, pooled exact binomial and Student t]`

<!-- GEN casualty_surge_size -->
| Event size | Events/run | Died of wounds, event casualties | Died of wounds, ordinary casualties | Peak R2B holding queue | Peak R2E theatre queue | Peak R2E post-operative overflow census | Peak R2E holding queue | Peak R2E staging overflow census |
|---|---|---|---|---|---|---|---|---|
| None | 0.0 | not applicable | 0.39% [0.36%, 0.42%] | 1.8 ± 0.4 | 29.9 ± 4.4 | 27.4 ± 0.9 | 56.8 ± 2.7 | 115.3 ± 9.7 |
| 10 | 75.2 | 0.76% [0.65%, 0.88%] | 0.44% [0.41%, 0.47%] | 2.0 ± 0.4 | 29.4 ± 4.4 | 27.4 ± 0.7 | 59.8 ± 3.0 | 163.4 ± 18.9 |
| 20 | 75.2 | 1.41% [1.30%, 1.52%] | 0.74% [0.70%, 0.78%] | 2.7 ± 0.4 | 40.0 ± 7.6 | 28.1 ± 0.6 | 70.3 ± 4.4 | 387.5 ± 40.9 |
| 40 | 75.2 | 2.37% [2.27%, 2.47%] | 1.11% [1.06%, 1.17%] | 3.1 ± 0.5 | 57.0 ± 7.6 | 30.6 ± 0.6 | 73.8 ± 4.2 | 951.7 ± 57.3 |
| 60 | 75.2 | 3.73% [3.63%, 3.83%] | 1.61% [1.55%, 1.68%] | 3.5 ± 0.4 | 104.8 ± 11.9 | 31.3 ± 0.5 | 77.2 ± 4.0 | 1587.4 ± 80.7 |
| 90 | 75.2 | 6.55% [6.45%, 6.66%] | 3.01% [2.93%, 3.10%] | 5.3 ± 0.6 | 249.7 ± 28.5 | 32.0 ± 0.5 | 80.2 ± 3.5 | 2319.6 ± 89.4 |
| 120 | 75.2 | 12.82% [12.69%, 12.94%] | 6.52% [6.39%, 6.64%] | 5.8 ± 0.8 | 574.6 ± 74.0 | 32.0 ± 0.5 | 82.2 ± 3.6 | 2410.0 ± 77.9 |
| 180 | 75.2 | 21.34% [21.21%, 21.47%] | 11.47% [11.31%, 11.64%] | 8.0 ± 0.7 | 2114.4 ± 197.9 | 31.0 ± 0.4 | 85.5 ± 3.0 | 1553.5 ± 97.6 |
<!-- /GEN -->

![Line plot of the died-of-wounds rate of event and ordinary casualties against casualty surge event size](../images/casualty_surge_size_sweep.png)

The died-of-wounds rate of event and ordinary casualties against the number of casualties per event, with pooled exact binomial 95% confidence intervals.

Peak queues are the largest four-hour mean queue, or census for an overflow point, of each pool over the campaign, with a 95% half-width across replications. With every event fixed at 10 casualties, the died-of-wounds rate among event casualties was <!-- GEN cell:casualty_surge_size|10|Died of wounds, event casualties|full -->0.76% [0.65%, 0.88%]<!-- /GEN --> and the peak R2E post-operative overflow census <!-- GEN cell:casualty_surge_size|10|Peak R2E post-operative overflow census|full -->27.4 ± 0.7<!-- /GEN -->, against <!-- GEN cell:casualty_surge_size|None|Peak R2E post-operative overflow census|full -->27.4 ± 0.9<!-- /GEN --> with no events injected. At 20 casualties per event the post-operative overflow census peaked at <!-- GEN cell:casualty_surge_size|20|Peak R2E post-operative overflow census|full -->28.1 ± 0.6<!-- /GEN -->, and at 40 at <!-- GEN cell:casualty_surge_size|40|Peak R2E post-operative overflow census|full -->30.6 ± 0.6<!-- /GEN -->. The R2E theatre queue peaked at <!-- GEN cell:casualty_surge_size|None|Peak R2E theatre queue|full -->29.9 ± 4.4<!-- /GEN --> with no events, <!-- GEN cell:casualty_surge_size|60|Peak R2E theatre queue|full -->104.8 ± 11.9<!-- /GEN --> at 60 casualties per event, <!-- GEN cell:casualty_surge_size|90|Peak R2E theatre queue|full -->249.7 ± 28.5<!-- /GEN --> at 90 and <!-- GEN cell:casualty_surge_size|180|Peak R2E theatre queue|full -->2114.4 ± 197.9<!-- /GEN --> at 180. The event casualty died-of-wounds rate was <!-- GEN cell:casualty_surge_size|60|Died of wounds, event casualties|full -->3.73% [3.63%, 3.83%]<!-- /GEN --> at 60 casualties and <!-- GEN cell:casualty_surge_size|180|Died of wounds, event casualties|full -->21.34% [21.21%, 21.47%]<!-- /GEN --> at 180, and the rate among ordinary casualties was <!-- GEN cell:casualty_surge_size|None|Died of wounds, ordinary casualties|full -->0.39% [0.36%, 0.42%]<!-- /GEN --> with no events and <!-- GEN cell:casualty_surge_size|180|Died of wounds, ordinary casualties|full -->11.47% [11.31%, 11.64%]<!-- /GEN --> at 180.

**High-intensity profile.** The same sweep under `high_intensity` `[high_intensity, every event fixed at one size · 0.2 events per day · 360 d · 30 replications per size · independent seeds]`:

<!-- GEN casualty_surge_size_high -->
| Event size | Events/run | Died of wounds, event casualties | Died of wounds, ordinary casualties | Peak R2B holding queue | Peak R2E theatre queue | Peak R2E post-operative overflow census | Peak R2E holding queue | Peak R2E staging overflow census |
|---|---|---|---|---|---|---|---|---|
| None | 0.0 | not applicable | 4.12% [4.06%, 4.19%] | 3.7 ± 0.4 | 163.0 ± 21.3 | 32.0 ± 0.5 | 89.6 ± 2.5 | 2897.3 ± 28.2 |
| 10 | 75.2 | 7.76% [7.42%, 8.12%] | 6.98% [6.90%, 7.06%] | 3.7 ± 0.6 | 290.6 ± 36.3 | 32.0 ± 0.6 | 88.3 ± 3.5 | 2845.7 ± 64.4 |
| 20 | 75.2 | 11.83% [11.53%, 12.13%] | 10.51% [10.42%, 10.61%] | 3.6 ± 0.5 | 587.2 ± 76.8 | 31.0 ± 0.5 | 90.1 ± 3.6 | 2492.1 ± 96.1 |
| 40 | 75.2 | 16.75% [16.51%, 16.99%] | 14.58% [14.46%, 14.69%] | 4.3 ± 0.5 | 1210.0 ± 96.6 | 31.3 ± 0.5 | 87.2 ± 2.7 | 1964.5 ± 73.2 |
| 60 | 75.2 | 18.94% [18.73%, 19.15%] | 15.96% [15.84%, 16.08%] | 4.9 ± 0.6 | 1899.8 ± 114.7 | 31.2 ± 0.6 | 87.1 ± 4.5 | 1666.4 ± 69.5 |
| 90 | 75.2 | 20.38% [20.20%, 20.55%] | 17.10% [16.98%, 17.23%] | 6.1 ± 0.8 | 2908.8 ± 143.4 | 31.4 ± 0.7 | 88.9 ± 3.8 | 1375.6 ± 53.6 |
| 120 | 75.2 | 20.55% [20.40%, 20.71%] | 17.46% [17.33%, 17.59%] | 6.5 ± 0.5 | 3898.2 ± 181.1 | 31.6 ± 0.5 | 90.9 ± 3.0 | 1225.4 ± 46.7 |
| 180 | 75.2 | 22.06% [21.93%, 22.19%] | 18.37% [18.24%, 18.51%] | 8.8 ± 1.1 | 5271.0 ± 162.8 | 31.8 ± 0.7 | 100.5 ± 4.7 | 1120.8 ± 34.8 |
<!-- /GEN -->

At 20 casualties per event the peak R2E post-operative overflow census was <!-- GEN cell:casualty_surge_size_high|20|Peak R2E post-operative overflow census|full -->31.0 ± 0.5<!-- /GEN -->, against <!-- GEN cell:casualty_surge_size_high|None|Peak R2E post-operative overflow census|full -->32.0 ± 0.5<!-- /GEN --> with no events injected.

---

## Resolution of Paired Differences

<small>[Return to Top](#contents)</small>

**Question.** Which of the paired differences the experiments above sized for resolution excluded zero at 30 replications, and how many replications would each of the others need before its interval narrowed to a stated half-width? `[as each experiment above · 360 d · 30 replications · paired, normal approximation to the interval half-width]`

<!-- GEN resolution -->
| Comparison | Paired difference | Half-width sought | Replications needed |
|---|---|---|---|
| Hold window, R2E first surgeries | −41.00 [−105.33, +23.33] | 2.0 | 28,507 |
| Hold window, R2E theatre entry deferred | −15.50 [−34.50, +3.50] | 1.0 | 9,947 |
| Hold window, diverted for a busy theatre | +11.10 [−8.72, +30.92] | 2.0 | 2,705 |
| Hold window, died of wounds | −2.47 [−6.47, +1.53] | 0.5 | 1,763 |
| Intensive care gate, died of wounds | +0.60 [−2.67, +3.87] | 0.5 | 1,178 |
| Policy 15 days against 21, died of wounds | +1.27 [−3.17, +5.70] | 1.0 | 542 |
| Policy 45 days against 21, died of wounds | −0.80 [−4.45, +2.85] | 1.0 | 367 |
| Policy 60 days against 21, died of wounds | −1.03 [−5.46, +3.39] | 1.0 | 540 |
| Saturation release at 2, died of wounds | −1.17 [−5.77, +3.44] | 1.0 | 585 |
| Saturation release at 2, returns to duty | +12.27 [−32.16, +56.69] | 10.0 | 544 |
<!-- /GEN -->

A comparison whose 95% interval excludes zero is listed in the second table rather than the first, the split being taken from the evidence each time the document is generated; the second table is empty where no interval excludes zero.

<!-- GEN resolved -->
| Comparison | Paired difference |
|---|---|
<!-- /GEN -->

The count for each row is $\lceil (z_{0.975}\, s_d / h)^2 \rceil$, where $s_d$ is the standard deviation of the within-replication differences observed at 30 replications and $h$ the half-width sought, chosen in the response's own units by the script that ran the experiment. The hold window's R2E first-surgery difference was <!-- GEN cell:resolution|Hold window, R2E first surgeries|Paired difference|full -->−41.00 [−105.33, +23.33]<!-- /GEN --> against a half-width sought of <!-- GEN cell:resolution|Hold window, R2E first surgeries|Half-width sought|full -->2.0<!-- /GEN --> operations, which needs <!-- GEN cell:resolution|Hold window, R2E first surgeries|Replications needed|full -->28,507<!-- /GEN --> replications. The intensive care gate's died-of-wounds difference was <!-- GEN cell:resolution|Intensive care gate, died of wounds|Paired difference|full -->+0.60 [−2.67, +3.87]<!-- /GEN --> and needs <!-- GEN cell:resolution|Intensive care gate, died of wounds|Replications needed|full -->1,178<!-- /GEN -->. Against a half-width sought of <!-- GEN cell:resolution|Policy 15 days against 21, died of wounds|Half-width sought|full -->1.0<!-- /GEN --> death per campaign, the 15-day and 45-day policy comparisons against the shipped 21 days need <!-- GEN cell:resolution|Policy 15 days against 21, died of wounds|Replications needed|full -->542<!-- /GEN --> and <!-- GEN cell:resolution|Policy 45 days against 21, died of wounds|Replications needed|full -->367<!-- /GEN --> replications. The 60-day policy's died-of-wounds difference against 21 days was <!-- GEN cell:resolution|Policy 60 days against 21, died of wounds|Paired difference|full -->−1.03 [−5.46, +3.39]<!-- /GEN --> and needs <!-- GEN cell:resolution|Policy 60 days against 21, died of wounds|Replications needed|full -->540<!-- /GEN -->. The saturation release's died-of-wounds difference at the shipped threshold of two needs <!-- GEN cell:resolution|Saturation release at 2, died of wounds|Replications needed|full -->585<!-- /GEN -->.

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
| Combat wounded in action | 1,412 | 1,593 |
| Support wounded in action | 756 | 796 |
| Combat killed in action | 599 | 612 |
| Support killed in action | 336 | 306 |
| Combat disease and non-battle injury | 1,782 | 1,836 |
| Support disease and non-battle injury | 409 | 423 |
<!-- /GEN -->

**Triage.** The trauma split applies to wounded and non-battle injury casualties, and its expectation is the configured share of them. A disease casualty is Priority 1 at the configured medical emergency share, and otherwise Priority 2 if it needs emergency surgery and Priority 3 if not, so the expectations for disease are the medical emergency share of disease casualties and the disease surgical share of the rest; battle fatigue is Priority 3.

<!-- GEN annex_triage -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Priority 1, wounded and non-battle injury | 1,691 | 1,663 |
| Priority 2, wounded and non-battle injury | 485 | 512 |
| Priority 3, wounded and non-battle injury | 382 | 384 |
| Priority 1, disease (medical emergency) | 52 | 63 |
| Priority 2, disease (emergency surgery) | 63 | 73 |
| Priority 3, disease | 1,150 | not applicable |
| Priority 3, battle fatigue | 536 | 536 |
| Killed in action | 935 | not applicable |
<!-- /GEN -->

**Damage control.** The expectation is the configured damage control rate times the casualties operated on at that priority.

<!-- GEN annex_damage_control -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Priority 1 operated | 1,512 | not applicable |
| Priority 1 damage control | 808 | 832 |
| Priority 2 operated | 447 | not applicable |
| Priority 2 damage control | 86 | 89 |
<!-- /GEN -->

**Strategic evacuation.** A sortie carries the casualties queued on each seat pool when it departs, and the places nobody takes depart with it; the expectation of sorties flown is the scheduled sorties before the horizon times the probability that a sortie flies.

<!-- GEN annex_evacuation -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Strategic evacuation decisions | 2,259 | not applicable |
| Boarded | 2,239 | not applicable |
| Still waiting at the close | 20 | not applicable |
| Mean wait of the casualties boarded (days) | 4.33 | not applicable |
| Sorties flown | 51 | 51 |
| Critical places departing empty per sortie (mean) | 7.1 | not applicable |
| Standard places departing empty per sortie (mean) | 39.0 | not applicable |
<!-- /GEN -->

**Admission at Role 2.** Minor casualties are Priority 3 wounded and non-battle injury casualties and disease casualties below Priority 1, who share one convalescence factor; the expectation is the share of that factor's convalescence draws exceeding the theatre evacuation policy. A casualty returned to duty while queued is one whose convalescence or restoration elapsed before a holding bed came free.

<!-- GEN annex_admission -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Minor casualties evacuated strategically (% of those with a disposition) | 10.6 | 9.3 |
| Returned to duty while queued for a holding bed | 543 | not applicable |
| Battle fatigue returned to duty from Role 2 | 534 | not applicable |
<!-- /GEN -->

**Placement at Role 2.** A casualty whose care calls for a bed takes the best one free when that care begins: the bed the care calls for, otherwise a holding bed, otherwise overflow, with no bed. A stabilisation is counted where a stay was served, and the overflow census is the time-weighted mean number of casualties cared for without a bed at each point over the campaign.

<!-- GEN annex_placement -->
| Measure | Realised | Configured expectation |
|---|---|---|
| R2B reception, straight to a resuscitation bed | 2,069 | not applicable |
| R2B reception, waiting in a holding bed | 126 | not applicable |
| R2B reception, waiting in overflow | 41 | not applicable |
| R2B wait for the evacuation vehicle in intensive care | 460 | not applicable |
| R2B wait for the evacuation vehicle in a holding bed | 13 | not applicable |
| R2B wait for the evacuation vehicle in overflow | 10 | not applicable |
| R2E stabilisation in intensive care | 319 | not applicable |
| R2E stabilisation in a holding bed | 42 | not applicable |
| R2E stabilisation in overflow | 544 | not applicable |
| R2E post-definitive care in intensive care | 747 | not applicable |
| R2E post-definitive care in a holding bed | 107 | not applicable |
| R2E post-definitive care in overflow | 1,227 | not applicable |
| Evacuees staged in a holding bed | 229 | not applicable |
| Evacuees staged in overflow | 2,030 | not applicable |
| Mean overflow census, R2B reception | 0.01 | not applicable |
| Mean overflow census, R2B wait for the evacuation vehicle | 0.00 | not applicable |
| Mean overflow census, R2E post-operative stays | 5.10 | not applicable |
| Mean overflow census, R2E pre-flight care | 0.43 | not applicable |
| Mean overflow census, R2E staging | 23.27 | not applicable |
<!-- /GEN -->

**Surgical load.** Utilisation of open time is the time-weighted share of a section's rostered time during which its first surgeon was in use, and the queued share is the share of that open time with one or more casualties waiting for any role in the section.

<!-- GEN annex_surgical_load -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Diverted from R2B, surgical team off shift | 1,153 | not applicable |
| Diverted from R2B, theatre busy | 209 | not applicable |
| R2B holding beds in use, both facilities (mean) | 7.89 | not applicable |
| R2E section 1 utilisation of open time (%) | 29.1 | not applicable |
| R2E section 2 utilisation of open time (%) | 43.8 | not applicable |
| R2E section 3 utilisation of open time (%) | 34.7 | not applicable |
| R2E section 1 open time with casualties waiting for a section (%) | 8.6 | not applicable |
| R2E section 2 open time with casualties waiting for a section (%) | 16.9 | not applicable |
| R2E section 3 open time with casualties waiting for a section (%) | 8.6 | not applicable |
<!-- /GEN -->

**Force regeneration.** The effective force is the combat or support pool at the day shown, under the shipped seven-day reinforcement cycle.

<!-- GEN annex_force -->
| Measure | Realised | Configured expectation |
|---|---|---|
| Combat force, day 0 | 2,500 | not applicable |
| Combat force, day 180 | 2,355 | not applicable |
| Combat force, day 360 | 2,403 | not applicable |
| Support force, day 0 | 1,250 | not applicable |
| Support force, day 180 | 1,215 | not applicable |
| Support force, day 360 | 1,203 | not applicable |
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
