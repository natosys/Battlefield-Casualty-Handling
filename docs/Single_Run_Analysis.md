# Battlefield Casualty Handling — Model Verification and Baseline Behaviour

## Abstract

<small>[Return to Top](#contents)</small>

This document verifies the Battlefield Casualty Handling discrete event simulation by walking through a single simulated campaign, echelon by echelon, under the `moderate_intensity` (Falklands 1982-modified) casualty rate baseline. Every figure reported here comes from one run of the shipped default configuration at seed 42 over 30 simulated days, so the document's unit of analysis is one campaign throughout. Its purpose is to establish that the model behaves as designed and to identify the system constraints a reader should then look for at statistical scale. Every replicated experiment this project reports, and the replication and confidence-interval methodology those experiments rest on, sits in the companion document `docs/Multi_Run_Analysis.md`; a figure carrying a confidence interval or a replication count will be found there rather than here.

Findings demonstrate that the current system design is capable of managing moderate casualty volumes, historically represented by the Falklands conflict. Two system constraints are identified. At R2B, holding bed capacity saturates progressively over a 30-day operation, driven by disease DNBI evacuees occupying hold beds for extended durations; stream decomposition confirms a structural 55% overload (expected 15.5 concurrent hold beds against 10-bed capacity); a two-tier routing policy — an upstream occupancy threshold and an at-R2B three-stage policy — manages this, with hold bed expansion or an evacuation threshold as the indicated structural remedies. At R2E Heavy, the second-shift surgical section is the primary binding constraint, with theatres reading as occupied largely because casualties hold a room while waiting for staff; ICU is busy but no longer saturated, and R2B OT capacity is not saturated. Whether these single-run findings generalise across independent replications, and how the system responds under a materially higher casualty rate, is addressed in `docs/Multi_Run_Analysis.md`.

This analysis uses the simulation's shipped default health system configuration: a representative combat brigade served by three Role 1 (R1) treatment teams, two Role 2 Basic (R2B) facilities, and one Role 2 Enhanced Heavy (R2E Heavy) hospital. This establishment is a configurable input to the simulation, not a fixed property of the model — the number of elements, and each element's internal team and bed composition, are defined in `env_data.json`'s `elms` structure and editable directly or via the Shiny Configure panel.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Simulation Casualty Generation](#simulation-casualty-generation)
- [R1 Handling](#r1-handling)
- [R2B Handling](#r2b-handling)
  - [R2B Hold Bed Saturation — Stream Decomposition and Intervention Analysis](#r2b-hold-bed-saturation--stream-decomposition-and-intervention-analysis)
- [R2E Heavy Handling](#r2e-heavy-handling)
- [Casualty Waiting Time](#casualty-waiting-time)
- [Transport Fleet Capacity Margin](#transport-fleet-capacity-margin)
- [Return to Duty](#return-to-duty)
- [Force Regeneration Feedback Loop](#force-regeneration-feedback-loop)
- [Strategic Evacuation and Role 4 Demand](#strategic-evacuation-and-role-4-demand)
- [Conclusion](#conclusion)
- [References](#references)
<!-- TOC END -->

---

## Simulation Casualty Generation

This section presents a detailed breakdown of casualty source data captured from a single simulation run using seed 42, spanning a 30-day operational duration. The data is analyzed through the lens of deployed health system design, highlighting implications for medical resource allocation, evacuation planning, and treatment capacity across Role 1 and Role 2 facilities.

> **Note on warm-up exclusion:** No warm-up exclusion is applied. The simulation is classified as a terminating simulation — it runs for a fixed, finite campaign horizon rather than approximating an indefinite steady state — so the full observation window, including campaign start-up, is retained in all outputs (`WARM_UP_DAYS = 0L`).

![Three stacked bar charts of daily casualty arrivals over the 30-day run, the same daily totals decomposed first by casualty type (WIA, KIA, DNBI), then by population source (combat, support), then by triage priority (Priority 1 to 3 and KIA)](../images/casualty_summary.png)

The three panels decompose one arrival series three ways, so each panel's daily totals match. Arrivals are strongly uneven: the heaviest day delivers 46 casualties and the lightest four, and combat-source casualties supply the great majority of every peak.

|casualty_type |population_source |  1|  2|  3|  4|  5|  6|  7|  8|  9| 10| 11| 12| 13| 14| 15| 16| 17| 18| 19| 20| 21| 22| 23| 24| 25| 26| 27| 28| 29| 30| total|
|:-------------|:-----------------|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|-----:|
|dnbi          |cbt               |  2|  1|  8|  4|  3|  5| 11|  7|  6|  3| 12|  1| 15| 20|  6|  3|  3|  0|  1|  0|  7|  0|  1|  4|  0|  3|  3|  1|  2|  6|   138|
|dnbi          |spt               |  0|  1|  0|  0|  2|  0|  1|  2|  2|  0|  0|  3|  2|  4|  0|  1|  3|  1|  1|  0|  0|  0|  0|  1|  3|  2|  1|  0|  3|  0|    33|
|kia           |cbt               |  0|  3|  0|  0|  2|  0|  3|  2|  5|  0|  5|  6|  4|  2|  1|  1|  0|  2|  0|  9|  0|  0|  0|  0|  1|  1|  0|  1|  0|  2|    50|
|kia           |spt               |  1|  0|  3|  0|  2|  0|  1|  0|  1|  1|  0|  0|  0|  0|  0|  0|  2|  0|  1|  1|  0|  3|  0|  0|  2|  2|  1|  0|  0|  1|    22|
|wia           |cbt               | 12|  0|  6| 34| 21|  1|  1|  6| 15|  2|  3|  0|  1|  1|  3| 23|  6| 10|  1|  3|  0|  1|  1| 16|  0|  7|  3|  3| 35|  3|   218|
|wia           |spt               |  2|  2|  0|  8|  1|  4|  2|  2|  1|  0|  5| 15|  0|  2|  3|  0|  4|  9|  0|  0|  0|  1|  2|  1|  2|  1|  0|  1|  1|  0|    69|
|Total         |                  | 17|  7| 17| 46| 31| 10| 19| 19| 30|  6| 25| 25| 22| 29| 13| 28| 18| 22|  4| 13|  7|  5|  4| 22|  8| 16|  8|  6| 41| 12|   530|

The table above presents a summary of the simulated casualty data generated across three primary categories Wounded in Action (WIA), Killed in Action (KIA), and Disease and Non-Battle Injury (DNBI), with their source population: combat forces and support forces. A total of 530 casualties were recorded, with combat elements accounting for the majority (406), reflecting their higher exposure to operational risk. WIA cases totalled 287, with a notable skew toward combat personnel (218 vs. 69) as a result of the force ratios present within the simulation. These casualties typically require multi-echelon care, including resuscitation, surgical intervention, and post-operative holding, placing sustained demand on Role 1 and Role 2 facilities. DNBI accounted for 171 cases, underscoring the persistent burden of non-combat medical conditions even in high-intensity environments; this aligns with historical data indicating that DNBI can rival or exceed battle injuries in terms of lost duty days and medical resource consumption. KIA figures were lower (72 total).

What this run shows above all is that the tempo is not steady. Daily totals run from 4 casualties to 46, with a mean of 17.7 and a standard deviation of 10.9, and the combat WIA stream alone contributes 35 casualties on day 29 and none at all on four separate days. That spread is the arrival process behaving as its configuration describes rather than a peculiarity of this seed: each stream draws its rate once per simulated day from a distribution whose standard deviation is sourced alongside its mean, and places arrivals within the day as a Poisson process (see the README's [Casualty Generation](../README.md#casualty-generation)). A planner reading this table should take the peak days rather than the mean as the sizing case, and should read a single 30-day run as one draw from a wide distribution: the multi-replication figures in [Multi-Run Analysis](Multi_Run_Analysis.md) are where the central behaviour is resolved.

From a health system planning perspective, this data implies a need for scalable treatment capacity, robust DNBI mitigation strategies, and distributed surgical capability, sized against a demand that arrives in bursts rather than at a constant rate.

|population_source |  1|  2|  3|  4|  5|  6|  7|  8|  9| 10| 11| 12| 13| 14| 15| 16| 17| 18| 19| 20| 21| 22| 23| 24| 25| 26| 27| 28| 29| 30| total|
|:-----------------|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|-----:|
|cbt               | 14|  4| 14| 38| 26|  6| 15| 15| 26|  5| 20|  7| 20| 23| 10| 27|  9| 12|  2| 12|  7|  1|  2| 20|  1| 11|  6|  5| 37| 11|   406|
|spt               |  3|  3|  3|  8|  5|  4|  4|  4|  4|  1|  5| 18|  2|  6|  3|  1|  9| 10|  2|  1|  0|  4|  2|  2|  7|  5|  2|  1|  4|  1|   124|
|Total             | 17|  7| 17| 46| 31| 10| 19| 19| 30|  6| 25| 25| 22| 29| 13| 28| 18| 22|  4| 13|  7|  5|  4| 22|  8| 16|  8|  6| 41| 12|   530|

The second table provides a breakdown of the casualty population by source: combat forces (cbt) and support forces (spt). Of the 530 total casualties generated, 406 (approximately 77%) originated from combat elements, while 124 (23%) were drawn from support units. This distribution reflects the total population breakdown of the organisation together with the higher rates the combat streams are configured at. The consistent presence of support force casualties across all periods underscores the vulnerability of rear-area personnel in LSCO environments, particularly under conditions of indirect fire, degraded situational awareness, and disrupted medical evacuation. The two populations peak independently: the support stream's heaviest day is day 12, at 18 casualties against a combat count of 7, while the combat stream's heaviest days are 4 and 29. A surge in one is therefore no guide to the other, and rear-area medical coverage cannot be planned as a fixed fraction of forward demand.

From a health system perspective, this data reinforces the need for distributed medical coverage that includes both forward and rear-area assets. Role 1 treatment teams must be positioned to respond rapidly to combat casualties, while Role 2 facilities must be capable of absorbing and triaging support force casualties who may present with different injury profiles, including DNBI and delayed trauma. The casualty burden across both populations highlights the importance of scalable capacity, flexible evacuation pathways, and robust command and control to ensure timely treatment and prevent bottlenecks in casualty flow.

|priority_group |  1|  2|  3|  4|  5|  6|  7|  8|  9| 10| 11| 12| 13| 14| 15| 16| 17| 18| 19| 20| 21| 22| 23| 24| 25| 26| 27| 28| 29| 30| total|
|:--------------|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|-----:|
|Priority 1     |  9|  4|  9| 23| 17|  5|  7|  7| 18|  4|  9| 12| 13| 17|  9| 19|  9| 16|  1|  1|  4|  2|  3| 13|  5| 10|  3|  3| 23|  5|   280|
|Priority 2     |  4|  0|  3| 18|  5|  2|  6|  4|  4|  0|  7|  6|  3|  8|  2|  4|  3|  1|  2|  2|  3|  0|  1|  5|  0|  2|  3|  0| 11|  1|   110|
|Priority 3     |  3|  0|  2|  5|  5|  3|  2|  6|  2|  1|  4|  1|  2|  2|  1|  4|  4|  3|  0|  0|  0|  0|  0|  4|  0|  1|  1|  2|  7|  3|    68|
|KIA            |  1|  3|  3|  0|  4|  0|  4|  2|  6|  1|  5|  6|  4|  2|  1|  1|  2|  2|  1| 10|  0|  3|  0|  0|  3|  3|  1|  1|  0|  3|    72|
|Total          | 17|  7| 17| 46| 31| 10| 19| 19| 30|  6| 25| 25| 22| 29| 13| 28| 18| 22|  4| 13|  7|  5|  4| 22|  8| 16|  8|  6| 41| 12|   530|

Of the total casualties, 280 (52.8%) were classified as Priority 1, representing patients requiring immediate life-saving intervention. This dominant category underscores the doctrinal necessity of forward-positioned Role 1 assets capable of rapid triage and stabilization. Priority 1 arrivals reach 23 on each of two days against a 30-day mean of 9.3, so the high-acuity burden is sustained in aggregate and highly uneven in incidence, which is the pattern that sizes resuscitation and surgical throughput.

Priority 2 and Priority 3 casualties accounted for 110 (20.8%) and 68 (12.8%) cases respectively. These patients typically require delayed or routine care. The simulation also generated 72 KIA cases (13.6%). While these cases do not contribute to medical workload substantially, their operational implications are significant.

From a systems design perspective, the acuity profile derived from this simulation reinforces several key imperatives:

- Role 1 facilities must be optimized for high-throughput triage and stabilization, with emphasis on rapid evacuation of Priority 1 cases.
- Role 2 facilities requires flexible bed space and surgical capability to absorb cases, especially during sustained operations.
- Evacuation architecture must support continuous movement of mixed-acuity casualties, with prioritization protocols and redundancy to ensure resilience.

## R1 Handling

Role 1 facilities consistently demonstrated the ability to process casualties without delay, with all patients receiving immediate triage and treatment on arrival. The absence of queuing reflects both adequate staffing and appropriately scaled treatment capacity relative to the casualty inflow modelled. Rapid handling times ensured that Priority 1 cases could be stabilised and evacuated without degradation in clinical status, while lower‑priority cases were managed and prepared for movement in line with requirements. However, the model does not currently fully represent the limitations in availability of evacuation assets, as a result, throughput at the Role 1 was not constrained by evacuation availability, allowing continuous casualty flow to higher‑echelon care and preventing downstream bottlenecks in the system which may bear out with the introduction of more detailed modelling of evacuation. Despite this, the performance underscores the critical function of Role 1 as an agile, forward medical capability able to maintain momentum under sustained operational tempo.

![Step plots of queue length over the 30-day run for each clinical role at each of the three R1 teams, flat at zero in every panel except two brief single-casualty spikes at R1 2 on day 3](../images/r1_queues.png)

Every R1 role holds a queue of zero for the whole run apart from two moments on day 3, when one casualty waited briefly for the nursing role at R1 2. The vertical scales differ between panels, so the two R1 1 and R1 3 panels are flat at zero across their full range.

## R2B Handling

The plot below outlines a summary of casualty handling at R2B. Following DNBI sub-categorisation (Issue #7), OT-bypass routing (Issue #35), and correction of OT bed scheduling (Issue #37), the R2B picture is substantially revised from earlier model iterations.

![Three stacked bar charts per simulation day at R2B: casualties treated at each of the two R2B stations, surgeries started at each station, and casualties bypassing R2B altogether](../images/r2b_handling.png)

Treated casualties and surgeries are shared between the two stations on most days, with neither station carrying the load alone for long. The bypass panel sits at zero or one on most days and rises sharply on a handful, day 14 and day 22 in particular, so bypassing is an episodic response to congestion rather than a steady share of the flow.

OT rooms are modelled as physical spaces available 24 hours per day. The surgical section operates on a 12-hour shift schedule and is the operative constraint on surgical access. Under seed 42 (30 days), **210 casualties reached the R2B surgical decision point**; **69 surgeries** were performed at R2B, and **141 were bypassed to R2E**. R2B OT utilisation was **9.7% (T1) and 9.6% (T2) against 24-hour room time**, and **19.3% and 19.1% against the section's own rostered time**. The OT queue remained flat at zero throughout the run, confirming the bypass logic is functioning as designed. Forward surgeries fall while the caseload arriving at the decision point rises by a third, which is what a single theatre and a single rostered section do when demand arrives in bursts: the peak cannot be spread.

**Bypass reason decomposition.** The undifferentiated bypass count above conflates two distinct causes: the surgical section being off shift, and the OT bed itself being busy or queued. `r2b_bypass_reason` (set at the point of bypass in `r2b_treat_wia()`, `R/trajectories.R`) distinguishes them: of the 141 bypasses, **100 (71%) were because the surgical section was off shift**, and **41 (29%) because the OT bed was busy or a queue existed**. This confirms the 12-hour shift window, not physical OT capacity, as the dominant constraint on forward surgical throughput at R2B: for half of each 24-hour cycle, a casualty arriving at either R2B unit cannot receive surgery there regardless of bed availability, and is routed to R2E instead.

**The pre-open hold.** Not every off-shift arrival is diverted. A casualty who finds the theatre free and the section closed for no longer than `r2b.surgery.pre_open_window_min`, shipped at 60 minutes, is received into the theatre and waits there for the section to open (see [R2B Trajectory](../README.md#r2b-trajectory)). Seven casualties were held that way in this run and all seven were operated on forward, waiting a mean of 35.2 minutes and at most 58.9. That is a small share of a 30-day run, which is what a 60-minute window against a 720-minute off-shift period should produce: the hold reaches the last hour of the closed period and no more of it. The reason-1 count above is what is left after those seven, and the replicated measurement of how far the window moves it is reported in [Multi-Run Analysis](Multi_Run_Analysis.md#the-r2b-pre-open-hold-window), a single run being too coarse to separate the movement from sampling variation.

![R2B OT Bypass Reason per Simulation Day](../images/r2b_ot_bypass_reason.png)

Off-shift bypasses (blue) dominate on nearly every day of the run, while OT-busy/queued bypasses (green) appear only intermittently and cluster on the heaviest arrival days. The shift-window gap is therefore a persistent, day-to-day constraint, and theatre congestion an intermittent one that appears when the arrival process delivers a peak.

Two candidate interventions to close the remaining gap — extending the existing section's shift hours, or fielding a second surgical section per R2B unit on the complementary shift — are not evaluated in this analysis. Extending shift hours cannot be meaningfully assessed without a model of clinician fatigue and associated error/complication risk, which the simulation does not represent; reporting throughput gains from longer shifts without that counterweight would overstate the intervention's net benefit. Fielding a second team is an establishment-size decision — a resourcing question for planners, not a parameter the simulation should default to testing as if cost-free. Both remain candidate follow-up scenario tests once a fatigue model exists or a second-team establishment change is directed.

**Holding bed capacity at R2B is the primary identified system constraint, and the ten beds run close to full for the whole run.** Concurrent hold occupancy rises from 2 beds on Day 1 to 9 or more within the first four days and stays high, averaging 7.6 of the 10 available beds across the run, reaching ten or more on 10 separate days and nine or more on 13 of the 30, and exceeding the establishment at 11 on its heaviest day. This load is driven by disease DNBI evacuees occupying hold beds for multi-day durations (mode 5 days), not by post-surgical patients.

The run now does show a queue, where it previously did not: the queue on the busiest hold beds peaks at five casualties. That is the capacity-aware routing policy reaching its limits on the peak days rather than evidence of spare capacity on the others. The upstream threshold check diverts a casualty to R2E before transport whenever no R2B unit is below 80% hold occupancy, and it did so 179 times over the run, with a further three diverted on arrival and one queued with both echelons full. The structural shortfall analysed in the next section is therefore real but largely exported to R2E, where it arrives as additional medical hold and ICU load, rather than accumulating as a visible queue at R2B.

![Step plots of queue length over the run for every bed at each of the two R2B units, showing queues of up to four casualties at R2B 1 and five at R2B 2 on the holding beds, and brief single-casualty queues on the resuscitation beds](../images/r2b_bed_queues.png)

Only the holding and resuscitation beds ever queue. The holding-bed queues are long-lived, persisting for days at a time around days 4 to 8 and again from day 14, while the resuscitation queues are brief spikes of one or two casualties.

![Gantt chart of bed occupancy at each of the two R2B units over the run, with one horizontal band per bed coloured by bed type, the five holding beds almost continuously occupied and the operating theatre, intensive care and resuscitation beds showing short scattered episodes](../images/r2b_gantt.png)

The contrast between bed types is the point: holding beds carry multi-day occupancies that run together into near-continuous bands, whereas theatre, intensive care and resuscitation occupancies are short enough to appear as isolated marks.

### R2B Hold Bed Saturation — Stream Decomposition and Intervention Analysis

Issue #39 adds per-stream decomposition of R2B hold bed occupancy. A `r2b_hold_start` attribute is now recorded for each patient entering the long-duration hold pathway, enabling daily concurrent occupancy to be decomposed by patient stream (disease DNBI, NBI DNBI, WIA) in the analysis pipeline. The `r2b_hold_drawn` attribute stores the drawn hold duration at the time of bed seizure, supporting optional evac-threshold logic described below.

**Battle fatigue verification.** Code inspection confirms that battle fatigue casualties (dnbi_type == 1) exit the trajectory at R1 via the "Battle Fatigue R1 Hold" branch and never reach R2B hold beds. This is enforced by a `stopifnot` assertion in the analysis pipeline.

**Structural load calculation.** Under the baseline seed 42 parameters (171 DNBI total; 93 disease, 32 NBI, 46 battle fatigue):

- Disease DNBI reaching R2B hold: ~74 evacuated (P1: 93 × 0.65 × 0.95 ≈ 57; P2: 93 × 0.20 × 0.90 ≈ 17), minus ~6% surgical candidacy ≈ **70 entering hold-bed recovery** over 30 days (≈ 2.3 per day)
- Non-surgical WIA and NBI reaching R2B hold: ~20 over 30 days (≈ 0.7 per day)
- **Total hold entry rate: ≈ 3.0 patients per day**
- Expected hold duration (triangular min=0.5d, mode=5d, max=10d): mean = (0.5 + 5 + 10) / 3 = **5.17 days**
- **Expected concurrent hold occupancy: 3.0 × 5.17 ≈ 15.5 beds** against 10 available (5 per R2B unit × 2 units)

This is a **structural 55% overload**. That is a long-run expectation over the configured means; a single 30-day run's realised occupancy sits below it, at 7.6 beds on average, because the routing policy exports the excess to R2E rather than letting it accumulate. The saturation cannot be resolved by changes to surgical throughput; it requires an intervention at the holding pathway itself.

![R2B Hold Bed Daily Occupancy by Patient Stream](../images/r2b_hold_occupancy.png)

Disease DNBI, in green, is the largest stream on all but a handful of days and is what carries occupancy above the ten-bed establishment. Wounded in action, in purple, holds a steady two to four beds throughout, so relieving the hold-bed constraint is a question about disease management rather than about surgical throughput.

**Intervention Scenario A — Hold duration reduction** (`vars.r2b.holding.mode` in `env_data.json`). Reducing the hold mode from 5 days (7,200 min) to 3 days (4,320 min) reduces expected mean duration from 5.17 to (0.5 + 3 + 10) / 3 = 4.5 days. Expected concurrent occupancy falls from 15.5 to 3.0 × 4.5 = **13.5 beds**, still 35% above the 10-bed capacity. A clinically implausible mode of ≤ 1.6 days would be required to bring expected occupancy within capacity. Hold duration reduction alone is insufficient to resolve saturation. To test: change `{"var": "mode", "val": 7200}` to `{"var": "mode", "val": 4320}` in the `vars.r2b.holding` activity and re-run 10+ replications.

**Intervention Scenario B — Hold bed expansion** (`elms.r2b.beds.hold.qty` in `env_data.json`). Increasing hold beds from 5 to 10 per R2B unit provides 20 total beds against expected steady-state demand of ~15.5, yielding headroom to absorb the day-to-day variation the arrival process carries. Eight beds per unit (16 total) sits barely above expected demand and would leave none. To test: change `{"name": "hold", "qty": 5}` to `{"name": "hold", "qty": 10}` in the `elms.r2b.beds` array and re-run 10+ replications.

**Intervention Scenario C — Evacuation threshold** (`vars.r2b.holding.evac_threshold` in `env_data.json`). The trajectory now supports an optional evac threshold (minutes): when `evac_threshold` is set and a patient's drawn hold duration exceeds it, the patient is forwarded to R2E rather than waiting for full recovery at R2B. At a threshold of 3 days (4,320 min): the triangular CDF gives P(draw > 4,320) = 1 − (4,320 − 720)² / ((14,400 − 720) × (7,200 − 720)) ≈ **85% of hold patients forwarded to R2E early**, effectively eliminating R2B hold saturation. This reduces R2B hold bed occupancy substantially but transfers a non-surgical medical load to the R2E hold and ICU pathway. What transfers is the unserved remainder of each forwarded casualty's own convalescence rather than a fresh duration drawn on arrival at R2E (see README [R2B Trajectory](../README.md#r2b-trajectory)), so the scenario redistributes a fixed quantity of bed time between the echelons and the R2E load it produces is attributable to the routing change alone. To test: add `{"var": "evac_threshold", "val": 4320}` to the `vars.r2b.holding` activity vals array and re-run 10+ replications.

**Intervention Scenario D — Capacity-aware hold routing (Issue #39, implemented).** A two-tier routing policy manages hold bed allocation. The primary tier operates at R1 before transport begins; the secondary tier operates at R2B on arrival.

**Primary tier — upstream threshold routing (`vars.r2b.holding.hold_threshold`, default 0.8).** `select_r2b_for_hold()` now checks whether a R2B unit's hold occupancy is strictly below `hold_threshold × capacity` before routing a patient there. With 5 beds per unit and threshold 0.8, a unit is only selected if fewer than 4 beds (80%) are occupied, keeping at least 1 bed reserved for incoming Step 1 staging patients. If no R2B unit is below threshold, the patient is routed directly to R2E from R1 (`r2b_bypassed = 1`) without incurring transport to R2B at all. When `hold_threshold` is absent the function falls back to routing whenever any bed is free (original behaviour). This eliminates the cascade where long-duration Step 4 holders starve new Step 1 arrivals: the routing decision is made before transport, not after the patient has already consumed a hold bed. To test: set `{"var": "hold_threshold", "val": 0.6}` for more aggressive upstream routing, or remove the parameter to restore original behaviour.

**Secondary tier — at-R2B three-stage policy.** For patients who arrive at R2B (either because the upstream check passed, or a race condition occurred between routing decision and arrival):

1. **Hold capacity available** — patient seizes a hold bed immediately (Step 4 No Surgery branch).
2. **Hold full, R2E has capacity** — patient bypasses to R2E via evacuation-team transport (`r2b_hold_bypass = 1`); also the fallback when queue cap is exceeded.
3. **Both echelons full, queue within cap** — patient joins the R2B hold queue (`r2b_hold_queued = 1`). Queue cap = floor(R2B\_beds / (R2B\_beds + R2E\_beds) × R2B\_beds) = **2 patients**; above cap, fallback to stage 2.

The analysis pipeline reports all three routing outcomes: `r2b_pre_bypass_count` (upstream, at R1), `r2b_hold_bypass_count` (at R2B Step 4), and `r2b_hold_queued_count` (queued at R2B when both echelons saturated).

> **MODEL ASSUMPTION — R2B Hold Bed Structural Overload:** Five hold beds per R2B unit are insufficient to absorb the demand generated by the 64% disease DNBI proportion observed over a 30-day operation. The overload is structural (expected demand 15.5 beds vs. 10 available) and is not resolved by hold duration reduction alone. With no-queue bypass active (Scenario D), overflowing patients transfer to R2E rather than accumulating at R2B, preserving system throughput at the cost of increased R2E medical hold load.
> **Basis:** Derived from model parameters: hold entry rate ≈ 3.0 patients/day × mean hold 5.17 days = 15.5 concurrent beds. No empirical doctrinal standard for forward medical holding capacity in LSCO contexts has been identified in open-access literature.
> **Uncertainty:** Medium — conditioned on the disease DNBI proportion assumption (itself High uncertainty; see [DNBI Sub-Type Split](../README.md#dnbi-sub-type-split)). If true disease proportion is lower, the overload reduces proportionally.
> **Consequence if wrong:** If disease DNBI proportion is substantially lower (e.g., 30%), expected concurrent hold occupancy falls to ~9 beds, within the 10-bed capacity. The saturation finding is sensitive to this assumption.

## R2E Heavy Handling

The R2E Heavy is the primary surgical node for the deployed health system, receiving both casualties bypassed directly from R1 and those bypassed onward from an R2B whose theatre was off-shift, occupied, or ICU-saturated. Under seed 42 over 30 days, the R2E performed **171 first surgeries** and **41 second surgeries**. The second-procedure count is now a minority of the first, where it was once most of it: only a damage control casualty whose abbreviated operation was performed here returns to theatre for a definitive repair, and a single-stage casualty operated on forward at R2B needs no procedure here at all (see README [Surgical Pathway](../README.md#surgical-pathway)).

![Step plots of R2E Heavy queue length over the run, one panel for the four intensive care beds and one for the two operating theatre beds, the intensive care queue never exceeding one and the theatre queue peaking at eight around day 9](../images/r2eheavy_bed_queue_3_teams.png)

The two panels share a vertical scale, which makes the difference between them plain: the theatre queue rises and falls in waves that reach eight casualties on day 9 and six on day 24, while the intensive care queue never exceeds one.

**R2E surgical throughput is bounded by rostered surgical sections as well as by theatre space.** A procedure seizes both an operating theatre and one of the three surgical sections that staff them, and a section carries a 12-hour roster while a theatre is available continuously, so the number of concurrent operations is capped at two during the first shift, when two sections are rostered on, and one during the second, when one is. Utilisation across the three sections was **30.8%, 53.6% and 30.8%** of the time their own rosters had them open; the middle figure is higher because that section is the one rostered to the second shift, and so absorbs the whole of the night-time surgical load on its own. Against 24-hour room time the two theatres ran at **66.6%** and **52.8%**.

**The second-shift surgical section, not the theatres, sets the pace at R2E.** OT 1 carried a queue for **46.3%** of the run and OT 2 for **34.6%**, and the second-shift section was queued for **2.5%** of its open time against **0.7%** and **0.6%** for the two first-shift sections. Because a casualty seizes a theatre before it seizes a section, a theatre reads as occupied while its casualty is still waiting for staff, so most of the theatre queue figure is casualties waiting on staff rather than on rooms. This is the largest single movement anywhere in the model between the previous generator and the current one, the theatre queue having stood at 3.0% and 0.6%, and it is the clearest illustration of what a flattened arrival process was concealing: theatre contention is set by peak-day volume, not by mean volume. The second-shift section remains the specific constraint, carrying roughly three quarters again the load of either first-shift section and a queue several times longer than either.

**R2E ICU is busy but no longer saturated.** Per-bed utilisation across the four ICU beds is **93.7%, 92.1%, 92.2% and 85.8%** (seed 42, 30 days), with a queue present for **26.5%** of the run on the first bed, **13.1%** on the second, **13.5%** on the third and **7.8%** on the fourth. The queue is spread across all four beds where it previously sat almost entirely behind the first. That is a materially looser picture than the effectively full ICU the model reported while every critical-route evacuee held a bed for the whole of its wait for an aircraft. Casualties awaiting strategic evacuation stage in holding beds, and only the ventilated minority of the critical pool holds an ICU bed at all, for a bounded pre-flight period: four such holds completed within this run, at a mean of **109.4 hours** and a 90th percentile of 147 hours. That is well above the roughly 28 hours the same measurement returned under the previous generator, and it is a bed-blocking effect rather than a clinical one: two of this run's four scheduled evacuation sorties drew a cancellation, so the holding pool a ventilated casualty must step down into stayed full for weeks at a time. The residual pressure on ICU is clinical demand plus a bed-blocking effect when the holding pool is full (see [Strategic Evacuation and Role 4 Demand](#strategic-evacuation-and-role-4-demand), and README Limitation L17).

Relieving the ICU constraint moves casualties through the pre-OT gate rather than around it. The stabilisation phase belongs to the damage control pathway alone, so the cohort passing through the gate's post-operative branch is the damage control casualties rather than every operated casualty: **79 recovered in ICU** (`post_op_pathway = 1`) and **58 Priority 1 casualties recovered in a holding bed** (`post_op_pathway = 2`) because ICU was full at the moment of theatre entry; a further **29 Priority 2 and lower casualties had theatre entry deferred** (`surgery_deferred = 1`) until a bed freed. Post-definitive care, which both pathways receive, went to an ICU bed for **54** casualties and to the degraded holding-bed fallback for **130**. Neither pathway produced a post-operative death of wounds in this single run, consistent with the small per-patient probabilities applied at that checkpoint and the small absolute counts characteristic of the Falklands-calibrated baseline; the replicated measurement of this checkpoint, and the stress test confirming that it fires, are reported in [The Post-Operative Intensive Care Gate](Multi_Run_Analysis.md#the-post-operative-intensive-care-gate). The single-run result should not be read as evidence that the holding-bed route is clinically safe, only that 58 casualties on it is too small a sample to resolve a sub-percent mortality difference.

`analyse_run()` now visualises exactly which casualties, and on which simulation day, received degraded care as a direct consequence of ICU saturation:

![R2E OT-ICU Gating Impact](../images/r2e_icu_gating_impact.png)

Sub-optimal care (red — surgery proceeded despite ICU saturation, Priority 1 override to holding-bed recovery) and delayed care (orange — OT entry deferred pending ICU availability, Priority 2+) cluster on the higher-arrival days later in the run, consistent with cumulative ICU demand outstripping the four-bed establishment as surgical volume accumulates. `outputs/r2e_icu_gating_daily.csv` and `outputs/post_op_pathway_summary.csv` provide the underlying daily and pathway-level counts.

![Gantt chart of R2E Heavy bed occupancy over the run, one horizontal band per bed coloured by bed type, the thirty holding beds filling one after another over the first five days and staying occupied to the end, above them four intensive care beds heavily but intermittently occupied and two theatres and three resuscitation beds in short episodes](../images/r2eheavy_gantt.png)

The holding beds fill in sequence and none is released again before the run ends, which is what makes the strategic evacuation backlog visible as a resource state rather than only as a count.

![Bar chart of R2E Heavy surgeries completed on each simulation day, varying between four and twenty-five with two days carrying none](../images/r2eheavy_surgeries.png)

Daily surgical output varies by a factor of six across the run and reaches 25 on day 25, so theatre demand at R2E arrives in bursts rather than at a steady rate.

> **Provenance (canonical refresh, Issue #155).** Every seed-42 figure in this document, and every figure it embeds, was produced from one code state in the project's pinned Dev Container (`rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`). It therefore carries no sandbox caveat and supersedes the per-issue currency notes that stood here previously, which recorded the successive model changes that had moved these figures and the unpinned R 4.3.3 sandboxes they were measured in.
>
> The pinned run reproduces the tracked baseline byte for byte, in `logs/logs.txt` and in every arrival diagnostic under `data/`, so no figure in this document moved because of the environment and the sandbox measurements it previously reported were faithful.
>
> One caution survives the refresh and is a property of the model rather than of its provenance. A single 30-day run is a draw from a wide distribution, because each arrival stream now carries the day-to-day variation its configuration names, so the figures below illustrate mechanisms and the multi-replication figures in [Multi-Run Analysis](Multi_Run_Analysis.md) carry the central behaviour.

When examined in system context, the combined OT capacity of two R2B elements and one R2E Heavy is adequate for a single combat brigade under Falklands-equivalent casualty rates [[1]](#references). However, if this system were applied to a deployed division, surgical and holding capacity would be grossly insufficient even if only one brigade was assumed to be in contact at any time. The modelled scenario also does not account for mass-casualty events or the elevated casualty production rates reported in FORECAS modelling of campaigns such as Okinawa or Vietnam, both of which would expose this deficit [[1]](#references).

## Casualty Waiting Time

![Scatter plot of each casualty's total waiting time in minutes against the simulation day of arrival, with a fitted trend line, most points lying on the zero line and a scattered upper band reaching 33,000 minutes that thins out after day 20](../images/waiting_time.png)

The distribution is sharply bimodal. Most casualties wait essentially no time at all, and a minority wait days to weeks, those being the casualties queued for a strategic evacuation sortie rather than for clinical care. The upper band falls away after day 20 because a casualty arriving late in the run has less time in which to accumulate a wait before the run ends, so the apparent improvement is an artefact of the finite horizon rather than a recovery.

## Transport Fleet Capacity Margin

![Transport Fleet Capacity Margin — Queue Over Time](../images/transport_capacity_margin.png)

Under seed 42 (30 days), the HX240M queue remains at 0 throughout the run and the PMV Ambulance pool queues for the first time, reaching two casualties and standing at one or more for 1.4% of the run on its busiest vehicle. The three-vehicle PMV Ambulance and four-vehicle HX240M pools remain far from binding at the Falklands-derived casualty rate, even with the full round-trip dead-heading model applied (each vehicle is held for an unladen return leg back to its originating echelon after casualty drop-off, rather than becoming available for the next pickup immediately). Mean utilisation (`outputs/transport_utilisation.csv`) is 14.9% for PMV Ambulance and 5.1% for HX240M, so substantial headroom remains on average. What has changed is that the margin is no longer untouched: a queue appearing at all, at 15% mean utilisation, is a property of demand arriving in bursts, and it is the reason the fleet-size sweep should be re-run against the current arrival process (see the README's Further Development entry L19). This plot shows the current single-run margin only; where the margin ends is a question a single run cannot answer, and is addressed by the replicated fleet-size sweep in [Transport Fleet-Size Sweep](Multi_Run_Analysis.md#transport-fleet-size-sweep), which varies vehicle count directly rather than casualty rate or transport duration.

**Seed-42 baseline (30 days, single run):** Under the current Falklands-derived casualty rate, the three-vehicle PMV Ambulance pool has sufficient spare capacity that dead-heading does not produce a persistent evacuation queue (max queue = 0, both with and without dead-heading). The effect is visible in asset utilisation instead: total PMV Ambulance busy-time rises from 6,816 to 14,376 minutes (+111%, consistent with an approximately symmetric round trip) across the 30-day run once the return leg is modelled, and a third vehicle is drawn into service that was never required under the outbound-only model. This baseline predates the Issue #73 follow-up (R2B↔R2E dead-heading) and the Issue #74 removal of the (by then unjustified) `return_leg_multiplier` parameter; see the `CLAUDE.md` Key Parameters table for the current post-#74 seed-42 figures.

## Return to Duty

Under seed 42 (30 days), **167 casualties** were assigned a `return_day` attribute, decomposed as follows:

| Echelon | RTD type | Count | Rate (of 530 arrivals) |
|---|---|---|---|
| R1 | battle_fatigue | 44 | 8.3% |
| R1 | clinical | 78 | 14.7% |
| R2B | clinical | 42 | 7.9% |
| R2E | clinical | 3 | 0.6% |
| **Total** | | **167** | **31.5%** |

`bf_rtd` is 44, not 46 (the total battle fatigue casualties generated), because 2 battle fatigue entities were still within their R1 hold timeout when the 30-day simulation ended and were not assigned `return_day`. Battle fatigue RTDs are exclusively at R1, consistent with the no-R2-routing design. The majority of clinical RTDs occur at R1 (Priority 3 WIA and NBI completing R1 recovery) and R2B (disease cases discharged from hold beds). R2E clinical RTDs are very low (3) because R2E hold-bed discharge is contingent on completing the drawn recovery-to-duty period, which for most retained casualties extends beyond the 30-day window; the count falls further as the operated cohort grows, since more of it is still recovering when the run ends. The aggregate RTD rate of 31.5% is within the historical range for in-theatre MTF admissions (7.6–42.1% [[2]](#references)), though direct comparison requires accounting for the simulation's 30-day boundary effect.

The share of R2E casualties retained in theatre is a separate quantity from this aggregate, and is now an output of the theatre evacuation policy rather than a fixed input (see README [R2E Heavy Trajectory](../README.md#r2e-heavy-trajectory)). Of the **176 casualties reaching R2E disposition** under seed 42, **23.3%** drew an expected recovery within the shipped 30-day policy and were retained, inside the historical range. Because the same drawn duration decides both the disposition and, for those retained, the holding-bed occupancy that follows it, evacuation is strictly a function of severity: sorting the 176 dispositions into quartiles by drawn recovery duration gives evacuation rates of **6.8%, 100%, 100% and 100%** from shortest to longest, where an unconditioned draw would have given the same rate in every quartile.

## Force Regeneration Feedback Loop

This section shows the force regeneration feedback loop mechanism (Issue #18) as one campaign realises it. Casualty production is coupled to a live, time-varying effective force size rather than to a fixed roll strength, so sustained attrition should draw the effective force down over the run. Reinforcement ships disabled (`force_regeneration.reinforcement.demand_interval_days = 0`), so the run below is the pure depletion case. Whether the depletion registers as a falling daily casualty volume, and whether an active reinforcement demand cycle arrests it, are questions about a trend in a noisy series and are answered across replications in [Force Regeneration Under Reinforcement](Multi_Run_Analysis.md#force-regeneration-under-reinforcement).

`analyse_run()` (`R/analysis.R`) now always produces a `force_regeneration_plot` — `effective_force_combat`/`effective_force_support` plotted against simulation day, faceted by replication when more than one is present — written to `images/force_regeneration.png`. The seed-42 baseline (no reinforcement, the shipped default) is shown below:

![Effective Force Size Over Time](../images/force_regeneration.png)

Both pools decline smoothly and monotonically-in-trend (net depletion outweighing RTD regeneration for most of the run), ending the 30-day run at 2,225 of 2,500 initial combat strength (−11.0%) and 1,162 of 1,250 initial support strength (−7.0%), modest in absolute terms at Falklands-calibrated rates. The combat curve is visibly stepped rather than smooth, each step a heavy arrival day.

## Strategic Evacuation and Role 4 Demand

This section presents the seed-42 30-day single-run Role 4 (national support base) and strategic AME outputs, with the wait-time died-of-wounds poll active. Of the 530 total casualties generated, 135 reached the strategic evacuation decision (`r2e_evac = 1`); of those, 99 had boarded an AME sortie and reached Role 4 by the end of the 30-day run, with 36 still queued and occupying an R2E holding bed. The sortie carries the RAAF's published C-17A aeromedical evacuation fit of 36 high dependency and 54 ambulatory places (see README [Role 4 (National Support Base) Demand Modelling](../README.md#role-4-national-support-base-demand-modelling)), so the residual 36 is set by which sorties actually flew rather than by seats.

![Role 4 (National Support Base) Daily Bed Occupancy by Ward](../images/role4_census.png)

Daily Role 4 bed occupancy rises through the engagement window, reaching a peak of 90.0 concurrent patients (all wards combined) on day 30, still rising at the run's end and not decaying to zero within the window shown, since the length-of-stay distributions extend well past the campaign horizon. The peak tracks the number of evacuation decisions rather than any change in how Role 4 itself is modelled, and it rises with them as each stream now realises the daily rate its configuration names (see README [Casualty Generation](../README.md#casualty-generation)).

![Two step plots of the number of casualties awaiting a strategic evacuation sortie over the run, one for the critical route and one for the standard route, both rising steadily to a peak around day 21 and falling sharply when a sortie flies before climbing again](../images/ame_backlog.png)

Both routes accumulate for three weeks before the first sortie flies, and neither is cleared by the two sorties that do fly: the critical backlog is higher at the end of the run than it was before the first departure. Every casualty counted here is holding an R2E bed while waiting.

> **Provenance note (Issue #109):** this image was regenerated as part of Issue #109 fixing a bug in the backlog computation itself — see the Domain 7 MODEL OUTPUT — Strategic AME Backlog Over Time (by Pool) block above for what was wrong and how it was fixed. The figures in the prose below were already correct (derived from `ame_wait_time_summary`, not the broken plot), which is how the bug went unnoticed until this issue's verification. The image is now regenerated in the pinned Dev Container with the rest of this document.

The two-pool split, separating critical (Priority 1 surgical) demand from standard demand, still earns its place, because the two pools clear at different rates even when neither is starved: **at the sourced airframe capacity both pools clear within days rather than accumulating across the run.** Of the 135 evacuation decisions, 85 route to the critical pool and 50 to the standard pool. Two of the four scheduled opportunities drew the 15% cancellation in this run, so the first sortie to fly departed on day 21. Of the critical pool, 61 of 85 decisions had boarded by day 30 (24 still waiting) at a mean wait of 10.1 days; of the standard pool, 38 of 50 had boarded (12 still waiting) at the same mean wait. The waits here are set by the cancellations rather than by seats or by the sortie interval, and they are the single clearest illustration in this run of why a schedule with idle capacity on average is not the same as a schedule that clears its backlog: two consecutive cancellations against a fortnightly effective interval left three weeks of disposition demand accumulating in R2E holding beds.

What the sortie timeline shows in this run is a schedule that cannot recover from a cancellation. The two sorties that flew boarded 36 and 25 casualties on the critical pool and 33 and 5 on the standard pool against 36 and 54 places each: the day-21 sortie fills the critical cabin exactly and still leaves a queue behind it. The unconstrained theoretical baseline, same-day and uncapped at the airframe's 90-seat total, would have needed 27 sorties across the run; the real schedule flew 2 and moved 99 of the 135 casualties who needed moving. At the sourced airframe capacity the binding quantity is therefore the number of sorties that actually depart, not the seats each one carries, and a single run's cancellation draws move the evacuation figures more than anything else in this section.

Where the evacuation backlog lands has changed again, and with it what the model reports as the constrained resource. Casualties awaiting a sortie hold R2E holding beds, but they now hold them for a day or two rather than for weeks, so the holding pool is no longer an evacuation queue in disguise: across a 90-day run it averages 88% occupancy with at least one casualty queued for 60% of the run, of which the evacuation wait accounts for roughly a third and in-theatre recovery to duty for the rest. The post-operative stabilisation split now reads `icu=79`, `hold=58` over the damage control cohort, and deferred surgeries stand at 29. The residual coupling into intensive care is correspondingly weaker: a ventilated critical-route casualty still cannot step down while the holding pool is full, but over 90 days that stretches its ICU occupancy from a median of 28 hours to a mean of 94 hours rather than the twelve-day tail the previous configuration produced. Limitation L17, rewritten for this change, records the systemic consequence.

The wait-time died-of-wounds poll, a periodic mortality check applied to casualties queued awaiting strategic evacuation, polls correctly against this backlog but recorded no death while awaiting AME in this seed-42 run (`outputs/dow_by_echelon.csv`), as in every run made since the poll was added. The per-poll death probability is deliberately small even for long waits, and the waits are now short, so the zero should be read as consistent with the mechanism rather than as evidence about its magnitude: a single seed-42 run cannot resolve a rare event in either direction.

Because disposition is now set by an evacuation policy rather than by a fixed rate, that policy can be swept as a planning lever, which is a question the previous formulation could not pose. Re-running the same seed-42 30-day configuration at the 15-day and 60-day policies the source cites as realistic alternatives [[3]](#references) gives:

| Policy | In-theatre share | Evacuation decisions | Reached Role 4 | Peak Role 4 occupancy |
|---|---|---|---|---|
| 15 days | 3.0% | 191 | 182 | 122.0 |
| 30 days (shipped) | 23.3% | 135 | 99 | 90.0 |
| 60 days | 73.8% | 34 | 13 | 13.0 |

Lengthening the policy moves casualties off the strategic airlift and onto R2E holding beds, and shortens the national support base's queue at the cost of theatre bed-days; shortening it does the reverse. The response is steep, and a planner reading it should note that the in-theatre share at the 15-day policy falls to 3.0%, below the 7.6% lower bound of the historical range, so the model is being pushed outside its validated envelope at that end. At the 15-day policy 182 of 191 casualties released by the policy reach Role 4 within the run, against 99 of 135 at the shipped policy: the difference is not the policy but which sorties flew, two of four having been cancelled in the shipped-policy run, which is a reminder that a single run's cancellation draws confound this comparison more than the policy does. These comparison runs are illustrative and do not alter the shipped 30-day baseline.

> **Reproducibility note:** the figures above were produced in the project's pinned Dev Container alongside the rest of this document.
>
> Splitting the surgical population between the damage control and single-stage pathways **is not RNG-stream-neutral**. Roughly half of operated casualties now take one theatre episode and one intensive care episode where they previously took two of each, so beds and theatres free sooner, which changes when every later casualty seizes a resource and therefore the order in which every subsequent draw is consumed from simmer's single global stream. Because Issue #18's force-regeneration loop couples arrival timing to casualty-event timing, even total casualty count moves (385 to 386). Every seed-42 figure in this document therefore differs from the previously documented baseline, which is the same pattern documented for prior merges (Issue #43, #73, #76, #18, #156, #160). Setting every damage control rate to 1.0 reproduces the pre-change run bit-identically, because a degenerate Bernoulli rate consumes no draw. A maintainer re-run in the pinned container is needed before these figures are fully authoritative — see the Post-Merge Checklist in `CLAUDE.md`.

## Conclusion

This walk-through, viewed in its entirety, demonstrates that the modelled deployed health system is capable of sustaining a steady operational tempo for a single brigade under baseline casualty assumptions derived from the Falklands conflict. Role 1 elements show sufficient responsiveness and throughput, and the dual-node R2B configuration absorbs surgical demand effectively through a combination of forward surgery and bypass routing to R2E.

Following correction of DNBI sub-categorisation (Issue #7), OT-bypass routing (Issues #35 and #37), and structural analysis of R2B holding capacity (Issue #39), two system constraints are identified. At R2B, holding bed capacity is the binding constraint: the ten beds average 7.6 concurrent occupants and reach or exceed full capacity on 10 days of the run, driven by disease DNBI evacuees occupying hold beds for multi-day durations. Stream decomposition analysis (Issue #39) confirms disease DNBI as the dominant load: expected concurrent hold occupancy of ~15.5 beds exceeds 10-bed capacity by 55%, a structural mismatch not addressable through surgical throughput adjustment. That the shortfall appears only intermittently as an R2B queue is a consequence of the capacity-aware routing policy, which diverted 179 casualties upstream to R2E before transport; the demand is displaced rather than absorbed. Hold bed expansion (≥10 beds per unit) or an evacuation threshold policy are the indicated interventions. OT is not a constraint at R2B, which operates at 9.6–9.7% against 24-hour room time (19.1–19.3% against shift time).

**The primary binding constraint at R2E is the surgical roster, with intensive care a close second.** The four ICU beds operate at 85.8–93.7% utilisation and each carries a queue for between 8% and 27% of the run; the three surgical sections run at 30.8%, 53.6% and 30.8% of their rostered time, and the second-shift section carries the night-time load alone. The R2E Heavy performs 171 first surgeries and 41 second surgeries in the baseline run, compared to 69 at R2B, and its two theatres carry a queue for 46.3% and 34.6% of the run, almost all of it casualties holding a room while waiting for staff. Strategic airlift is constrained in this run by sortie cancellations rather than by seats: two of four scheduled sorties did not fly, and 36 casualties were still queued at the run's end. Three system levers are indicated: R2B holding bed expansion or a higher evacuation threshold from R2B holding, a second-shift surgical section at R2E, and R2E holding beds, which carry in-theatre recovery and the residual evacuation wait on one pool (see README Limitation L17). The OT–ICU gate (Issue #43) does not add capacity; it makes the consequence of the existing shortfall explicit in the model's mortality output rather than absorbing it silently into ICU queue time.

This walk-through characterises one campaign under the Falklands-modified baseline. Whether its findings generalise, and how the system responds under a materially higher casualty rate, is addressed directly and quantitatively in [Comparative Scenario Analysis](Multi_Run_Analysis.md#comparative-scenario-analysis) (`docs/Multi_Run_Analysis.md`), which compares this baseline against an Okinawa-intensity profile across n≥30 replications with 95% confidence intervals. That document is also where the model's replicated policy experiments are reported: the post-operative intensive care gate, the forward ICU share frontier, the transport fleet-size sweep, the reinforcement comparison and the mass casualty stress test.

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Blood, CG; Zouris, JM; Rotblatt, D; (1998) *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

[2] Izaguirre, MK; Cox, D; Lodi, PC; Giraud, RS; Murray, CK; Teyhen, DS; Capaldi, VF; Kelly, KM; Taylor, JF; Holland, JC; Laragione, VJ. (March 2025) *To Conserve Fighting Strength in Large Scale Combat Operations*. Military Review Online. Retrieved 20 Jul 25, from https://www.armyupress.army.mil/Journals/Military-Review/Online-Exclusive/2025-OLE/Conserve-Fighting-Strength-in-LSCO/

[3] U.S. Army Medical Department Center and School. *Health Service Support in a Theater of Operations*, Subcourse MD0002, Evacuation Policy. Retrieved 02 Aug 26, from http://armymedical.tpub.com/MD0002/Evacuation-Policy-Health-Service-Support-in-a-Theater-of-Operations-88.htm

<!-- REFERENCES END -->
