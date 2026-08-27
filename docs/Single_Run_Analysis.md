# Battlefield Casualty Handling: Model Verification and Baseline Behaviour

## Abstract

<small>[Return to Top](#contents)</small>

This document verifies the Battlefield Casualty Handling discrete event simulation by walking through a single simulated campaign, echelon by echelon, under the `moderate_intensity` (Falklands 1982-modified) casualty rate baseline. Every figure reported here comes from one run of the shipped default configuration at seed 42 over 30 simulated days, so the document's unit of analysis is one campaign throughout. Its purpose is to establish that the model behaves as designed and to identify the system constraints a reader should then look for at statistical scale. Every replicated experiment this project reports, and the replication and confidence-interval methodology those experiments rest on, sits in the companion document `docs/Multi_Run_Analysis.md`; a figure carrying a confidence interval or a replication count will be found there rather than here.

Findings demonstrate that the current system design is capable of managing moderate casualty volumes, historically represented by the Falklands conflict. Two system constraints are identified. At R2B, holding bed capacity saturates progressively over a 30-day operation, driven by disease DNBI evacuees occupying hold beds for extended durations; stream decomposition confirms a structural 55% overload (expected 15.5 concurrent hold beds against 10-bed capacity). A two-tier routing policy, combining an upstream occupancy threshold with an at-R2B three-stage policy, manages this, with hold bed expansion or an evacuation threshold as the indicated structural remedies. At R2E Heavy, the second-shift surgical section is the primary binding constraint, with theatres reading as occupied largely because casualties hold a room while waiting for staff; intensive care is busy but not saturated, and R2B operating theatre capacity is not saturated. Whether these single-run findings generalise across independent replications, and how the system responds under a materially higher casualty rate, is addressed in `docs/Multi_Run_Analysis.md`.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Methods](#methods)
  - [Model and Run Configuration](#model-and-run-configuration)
  - [Warm-Up Treatment](#warm-up-treatment)
  - [Scope of a Single Run](#scope-of-a-single-run)
  - [Evidence Set and Provenance](#evidence-set-and-provenance)
- [Simulation Casualty Generation](#simulation-casualty-generation)
- [R1 Handling](#r1-handling)
- [R2B Handling](#r2b-handling)
  - [R2B Hold Bed Saturation: Stream Decomposition and Intervention Analysis](#r2b-hold-bed-saturation-stream-decomposition-and-intervention-analysis)
- [R2E Heavy Handling](#r2e-heavy-handling)
- [Casualty Waiting Time](#casualty-waiting-time)
- [Transport Fleet Capacity Margin](#transport-fleet-capacity-margin)
- [Return to Duty](#return-to-duty)
- [Force Regeneration Feedback Loop](#force-regeneration-feedback-loop)
- [Strategic Evacuation and Role 4 Demand](#strategic-evacuation-and-role-4-demand)
- [Limitations](#limitations)
- [Conclusion](#conclusion)
- [References](#references)
<!-- TOC END -->

---

## Methods

<small>[Return to Top](#contents)</small>

### Model and Run Configuration

The simulation is a discrete event model built on the `simmer` package for R [[1]](#references), in which each casualty is an arriving entity that seizes and releases clinical staff, beds, operating theatres and transport as it moves rearward through the echelons of care. The echelon structure follows allied medical support doctrine [[2]](#references): Role 1 for primary care and resuscitation forward, Role 2 Basic (R2B) for damage control surgery and short-term holding, and Role 2 Enhanced Heavy (R2E Heavy) for definitive surgery, intensive care and in-theatre recovery, with strategic aeromedical evacuation to a Role 4 national support base beyond it.

The analysis uses the simulation's shipped default health system configuration: a representative combat brigade served by three Role 1 treatment teams, two R2B facilities and one R2E Heavy hospital. This establishment is a configurable input to the simulation rather than a fixed property of the model. The number of elements, and each element's internal team and bed composition, are defined in `env_data.json`'s `elms` structure and are editable directly or through the Shiny Configure panel.

Every figure reported below, except where the text states otherwise, comes from one execution of that configuration under the settings in the table.

| Setting | Value |
|---|---|
| Scenario profile | `moderate_intensity` (Falklands 1982-modified) |
| Random seed | 42 |
| Campaign horizon | 30 simulated days |
| Replications | 1 |
| Warm-up exclusion | None (`WARM_UP_DAYS = 0L`) |
| Reinforcement | Disabled (`force_regeneration.reinforcement.demand_interval_days = 0`) |
| Mass casualty injection | Disabled (`mass_casualty.event.rate_per_day = 0`) |
| Theatre evacuation policy | 30 days |
| Invocation | `Rscript run.R --seed 42 --days 30 --iterations 1` |

### Warm-Up Treatment

No warm-up period is deleted from the observation window. Warm-up deletion exists to remove the initialisation transient by which an infinite-horizon model approaches steady state, and it is appropriate only where the quantity of interest is that steady state [[3]](#references). This model is a terminating simulation: it runs for a fixed, finite campaign horizon from an empty and idle system, and the start-up transient is part of what a planner needs to see, because a deploying health system genuinely does begin empty. The full observation window is therefore retained in every output (`WARM_UP_DAYS = 0L`, `R/warmup.R`). A Welch graphical procedure is nonetheless run against the model as a diagnostic, and is reported in `docs/Multi_Run_Analysis.md`.

### Scope of a Single Run

This document is a verification exercise rather than an experiment. Verification asks whether the implemented model behaves as its specification describes; validation asks whether that specification is an adequate representation of the real system, and the two are distinct activities resting on distinct evidence [[4]](#references). What follows is the first. It traces one campaign end to end and checks that casualties are generated at the configured rates, routed by the documented rules, held by the documented resources and dispositioned by the documented policies, and it identifies where the resulting load concentrates.

A single run cannot do more than that. Each arrival stream draws its daily rate from a distribution before placing arrivals within the day, so a 30-day realisation is one draw from a wide distribution and its point estimates carry no interval [[5]](#references). Every figure below should therefore be read as an instance of a mechanism rather than as an estimate of a mean, and no comparison between two figures in this document is a test of any hypothesis. Every quantity this project reports with a confidence interval is in `docs/Multi_Run_Analysis.md`, and the sections below cross-reference it wherever a replicated measurement of the same quantity exists.

### Evidence Set and Provenance

Every seed-42 figure in this document, and every figure it embeds, was produced from one code state in the project's pinned development container (`rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`), so no figure carries an unpinned-environment caveat. That run reproduces the repository's tracked baseline evidence set byte for byte, in the console log `logs/logs.txt` and in every arrival diagnostic under `data/`, and `scripts/check_baseline_reproduction.R` asserts the reproduction on every subsequent change to the model. Figures drawn from a configuration other than the shipped default appear only in the theatre evacuation policy comparison in [Strategic Evacuation and Role 4 Demand](#strategic-evacuation-and-role-4-demand), and are identified as such where they appear.

---

## Simulation Casualty Generation

<small>[Return to Top](#contents)</small>

This section presents a detailed breakdown of casualty source data captured from the run, analysed through the lens of deployed health system design and highlighting implications for medical resource allocation, evacuation planning, and treatment capacity across Role 1 and Role 2 facilities. The daily rates each stream draws from are derived from the Falklands 1982 campaign as tabulated in the FORECAS casualty projection study [[6]](#references), modified as the README's [Parameter Calibration](../README.md#parameter-calibration) section records.

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

The table above summarises the simulated casualty data across three primary categories, Wounded in Action (WIA), Killed in Action (KIA), and Disease and Non-Battle Injury (DNBI), with their source population of combat or support forces. A total of 530 casualties were recorded, with combat elements accounting for the majority (406), reflecting their higher exposure to operational risk. WIA cases totalled 287, with a notable skew toward combat personnel (218 against 69) as a result of the force ratios present within the simulation. These casualties typically require multi-echelon care, including resuscitation, surgical intervention, and post-operative holding, placing sustained demand on Role 1 and Role 2 facilities. DNBI accounted for 171 cases, underscoring the persistent burden of non-combat medical conditions even in high-intensity environments; this aligns with the historical pattern in which disease and non-battle injury rivals or exceeds battle injury in lost duty days and medical resource consumption [[7]](#references). KIA figures were lower, at 72 in total.

What this run shows above all is that the tempo is not steady. Daily totals run from 4 casualties to 46, with a mean of 17.7 and a standard deviation of 10.9, and the combat WIA stream alone contributes 35 casualties on day 29 and none at all on four separate days. That spread is the arrival process behaving as its configuration describes rather than a peculiarity of this seed: each stream draws its rate once per simulated day from a distribution whose standard deviation is sourced alongside its mean, and places arrivals within the day as a Poisson process (see the README's [Casualty Generation](../README.md#casualty-generation)). A planner reading this table should take the peak days rather than the mean as the sizing case, and should read a single 30-day run as one draw from a wide distribution: the multi-replication figures in [Multi-Run Analysis](Multi_Run_Analysis.md) are where the central behaviour is resolved.

From a health system planning perspective, this data implies a need for scalable treatment capacity, robust DNBI mitigation strategies, and distributed surgical capability, sized against a demand that arrives in bursts rather than at a constant rate.

|population_source |  1|  2|  3|  4|  5|  6|  7|  8|  9| 10| 11| 12| 13| 14| 15| 16| 17| 18| 19| 20| 21| 22| 23| 24| 25| 26| 27| 28| 29| 30| total|
|:-----------------|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|-----:|
|cbt               | 14|  4| 14| 38| 26|  6| 15| 15| 26|  5| 20|  7| 20| 23| 10| 27|  9| 12|  2| 12|  7|  1|  2| 20|  1| 11|  6|  5| 37| 11|   406|
|spt               |  3|  3|  3|  8|  5|  4|  4|  4|  4|  1|  5| 18|  2|  6|  3|  1|  9| 10|  2|  1|  0|  4|  2|  2|  7|  5|  2|  1|  4|  1|   124|
|Total             | 17|  7| 17| 46| 31| 10| 19| 19| 30|  6| 25| 25| 22| 29| 13| 28| 18| 22|  4| 13|  7|  5|  4| 22|  8| 16|  8|  6| 41| 12|   530|

The second table breaks the casualty population down by source: combat forces (cbt) and support forces (spt). Of the 530 total casualties generated, 406 (approximately 77%) originated from combat elements, while 124 (23%) were drawn from support units. This distribution reflects the total population breakdown of the organisation together with the higher rates the combat streams are configured at. The consistent presence of support force casualties across all periods underscores the vulnerability of rear-area personnel in large scale combat operations, particularly under conditions of indirect fire, degraded situational awareness, and disrupted medical evacuation [[8]](#references). The two populations peak independently: the support stream's heaviest day is day 12, at 18 casualties against a combat count of 7, while the combat stream's heaviest days are 4 and 29. A surge in one is therefore no guide to the other, and rear-area medical coverage cannot be planned as a fixed fraction of forward demand.

From a health system perspective, this data reinforces the need for distributed medical coverage that includes both forward and rear-area assets. Role 1 treatment teams must be positioned to respond rapidly to combat casualties, while Role 2 facilities must be capable of absorbing and triaging support force casualties who may present with different injury profiles, including DNBI and delayed trauma. The casualty burden across both populations highlights the importance of scalable capacity, flexible evacuation pathways, and robust command and control to ensure timely treatment and prevent bottlenecks in casualty flow.

|priority_group |  1|  2|  3|  4|  5|  6|  7|  8|  9| 10| 11| 12| 13| 14| 15| 16| 17| 18| 19| 20| 21| 22| 23| 24| 25| 26| 27| 28| 29| 30| total|
|:--------------|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|-----:|
|Priority 1     |  9|  4|  9| 23| 17|  5|  7|  7| 18|  4|  9| 12| 13| 17|  9| 19|  9| 16|  1|  1|  4|  2|  3| 13|  5| 10|  3|  3| 23|  5|   280|
|Priority 2     |  4|  0|  3| 18|  5|  2|  6|  4|  4|  0|  7|  6|  3|  8|  2|  4|  3|  1|  2|  2|  3|  0|  1|  5|  0|  2|  3|  0| 11|  1|   110|
|Priority 3     |  3|  0|  2|  5|  5|  3|  2|  6|  2|  1|  4|  1|  2|  2|  1|  4|  4|  3|  0|  0|  0|  0|  0|  4|  0|  1|  1|  2|  7|  3|    68|
|KIA            |  1|  3|  3|  0|  4|  0|  4|  2|  6|  1|  5|  6|  4|  2|  1|  1|  2|  2|  1| 10|  0|  3|  0|  0|  3|  3|  1|  1|  0|  3|    72|
|Total          | 17|  7| 17| 46| 31| 10| 19| 19| 30|  6| 25| 25| 22| 29| 13| 28| 18| 22|  4| 13|  7|  5|  4| 22|  8| 16|  8|  6| 41| 12|   530|

Of the total casualties, 280 (52.8%) were classified as Priority 1, representing patients requiring immediate life-saving intervention. This dominant category underscores the doctrinal necessity of forward-positioned Role 1 assets capable of rapid triage and stabilisation. Priority 1 arrivals reach 23 on each of two days against a 30-day mean of 9.3, so the high-acuity burden is sustained in aggregate and highly uneven in incidence, which is the pattern that sizes resuscitation and surgical throughput.

Priority 2 and Priority 3 casualties accounted for 110 (20.8%) and 68 (12.8%) cases respectively. These patients typically require delayed or routine care. The simulation also generated 72 KIA cases (13.6%). While these cases do not contribute substantially to medical workload, their operational implications are significant.

From a systems design perspective, the acuity profile derived from this simulation reinforces several key imperatives:

- Role 1 facilities must be optimised for high-throughput triage and stabilisation, with emphasis on rapid evacuation of Priority 1 cases.
- Role 2 facilities require flexible bed space and surgical capability to absorb cases, especially during sustained operations.
- Evacuation architecture must support continuous movement of mixed-acuity casualties, with prioritisation protocols and redundancy to ensure resilience.

## R1 Handling

<small>[Return to Top](#contents)</small>

Role 1 facilities consistently processed casualties without delay, with all patients receiving immediate triage and treatment on arrival. The absence of queuing reflects both adequate staffing and appropriately scaled treatment capacity relative to the casualty inflow modelled. Rapid handling times allowed Priority 1 cases to be stabilised and evacuated without degradation in clinical status, while lower-priority cases were managed and prepared for movement in line with requirements. The model does not, however, fully represent the limitations in availability of evacuation assets, so throughput at Role 1 was not constrained by evacuation availability, allowing continuous casualty flow to higher-echelon care and preventing downstream bottlenecks that a more detailed model of evacuation may yet expose. Read with that caveat, the result underscores the critical function of Role 1 as an agile, forward medical capability able to maintain momentum under sustained operational tempo.

![Step plots of queue length over the 30-day run for each clinical role at each of the three R1 teams, flat at zero in every panel except two brief single-casualty spikes at R1 2 on day 3](../images/r1_queues.png)

Every R1 role holds a queue of zero for the whole run apart from two moments on day 3, when one casualty waited briefly for the nursing role at R1 2. The vertical scales differ between panels, so the two R1 1 and R1 3 panels are flat at zero across their full range.

## R2B Handling

<small>[Return to Top](#contents)</small>

The plot below summarises casualty handling at R2B.

![Three stacked bar charts per simulation day at R2B: casualties treated at each of the two R2B stations, surgeries started at each station, and casualties bypassing R2B altogether](../images/r2b_handling.png)

Treated casualties and surgeries are shared between the two stations on most days, with neither station carrying the load alone for long. The bypass panel sits at zero or one on most days and rises sharply on a handful, day 14 and day 22 in particular, so bypassing is an episodic response to congestion rather than a steady share of the flow.

Operating theatre rooms are modelled as physical spaces available 24 hours per day. The surgical section operates on a 12-hour shift schedule and is the operative constraint on surgical access. Under seed 42 over 30 days, **210 casualties reached the R2B surgical decision point**; **69 surgeries** were performed at R2B, and **141 were bypassed to R2E**. R2B theatre utilisation was **9.7% (T1) and 9.6% (T2) against 24-hour room time**, and **19.3% and 19.1% against the section's own rostered time**. The theatre queue remained flat at zero throughout the run, confirming that the bypass logic functions as designed. Forward surgical volume is bounded not by the caseload arriving at the decision point but by a single theatre and a single rostered section per unit, which cannot spread a peak.

**Bypass reason decomposition.** The undifferentiated bypass count above conflates two distinct causes: the surgical section being off shift, and the theatre itself being busy or queued. The `r2b_bypass_reason` attribute, set at the point of bypass in `r2b_treat_wia()` (`R/trajectories.R`), distinguishes them: of the 141 bypasses, **100 (71%) were because the surgical section was off shift**, and **41 (29%) because the theatre was busy or a queue existed**. This confirms the 12-hour shift window, not physical theatre capacity, as the dominant constraint on forward surgical throughput at R2B: for half of each 24-hour cycle, a casualty arriving at either R2B unit cannot receive surgery there regardless of bed availability, and is routed to R2E instead. Because time to surgical intervention is among the strongest determinants of survival after severe battlefield injury [[9]](#references), a constraint that removes forward surgery for half of every day is a clinically material one and not merely a utilisation artefact.

**The pre-open hold.** Not every off-shift arrival is diverted. A casualty who finds the theatre free and the section closed for no longer than `r2b.surgery.pre_open_window_min`, shipped at 60 minutes, is received into the theatre and waits there for the section to open (see [R2B Trajectory](../README.md#r2b-trajectory)). Seven casualties were held that way in this run and all seven were operated on forward, waiting a mean of 35.2 minutes and at most 58.9. That is a small share of a 30-day run, which is what a 60-minute window against a 720-minute off-shift period should produce: the hold reaches the last hour of the closed period and no more of it. The off-shift bypass count above is what is left after those seven, and the replicated measurement of how far the window moves it is reported in [Multi-Run Analysis](Multi_Run_Analysis.md#the-r2b-pre-open-hold-window), a single run being too coarse to separate the movement from sampling variation.

![Two-colour stacked bar chart of R2B operating theatre bypasses on each simulation day, separating bypasses caused by the surgical section being off shift from those caused by the theatre being busy or queued](../images/r2b_ot_bypass_reason.png)

Off-shift bypasses carry most days of the run, and theatre-busy bypasses appear on roughly half the days and are the whole of the count on day 5 alone. The shift-window gap is therefore a persistent, day-to-day constraint, and theatre congestion an intermittent one that rides on top of it: no day is free of the first, and the second never accounts for a bypass count on its own except once.

Two candidate interventions to close the remaining gap, extending the existing section's shift hours or fielding a second surgical section per R2B unit on the complementary shift, are not evaluated in this analysis. Extending shift hours cannot be meaningfully assessed without a model of clinician fatigue and associated error and complication risk, which the simulation does not represent; reporting throughput gains from longer shifts without that counterweight would overstate the intervention's net benefit. Fielding a second team is an establishment-size decision, a resourcing question for planners rather than a parameter the simulation should default to testing as if cost-free. Both remain candidate follow-up scenario tests once a fatigue model exists or a second-team establishment change is directed.

**Holding bed capacity at R2B is the primary identified system constraint, and the ten beds run close to full for the whole run.** Concurrent hold occupancy rises from 2 beds on Day 1 to 9 or more within the first four days and stays high, averaging 7.6 of the 10 available beds across the run, reaching ten or more on 10 separate days and nine or more on 13 of the 30, and exceeding the establishment at 11 on each of its two heaviest days. This load is driven by disease DNBI evacuees occupying hold beds for multi-day durations (mode 5 days), not by post-surgical patients.

The run does show a queue on the busiest holding beds, peaking at five casualties. That is the capacity-aware routing policy reaching its limits on the peak days rather than evidence of spare capacity on the others. The upstream threshold check diverts a casualty to R2E before transport whenever no R2B unit is below 80% hold occupancy, and it did so 179 times over the run, with a further three diverted on arrival and one queued with both echelons full. The structural shortfall analysed in the next section is therefore real but largely exported to R2E, where it arrives as additional medical hold and intensive care load, rather than accumulating as a visible queue at R2B.

![Step plots of queue length over the run for every bed at each of the two R2B units, showing queues of up to four casualties at R2B 1 and five at R2B 2 on the holding beds, and brief single-casualty queues on the resuscitation beds](../images/r2b_bed_queues.png)

Only the holding and resuscitation beds ever queue. The holding-bed queues are long-lived, persisting for days at a time around days 4 to 8 and again from day 14, while the resuscitation queues are brief spikes of one or two casualties.

![Gantt chart of bed occupancy at each of the two R2B units over the run, with one horizontal band per bed coloured by bed type, the five holding beds almost continuously occupied and the operating theatre, intensive care and resuscitation beds showing short scattered episodes](../images/r2b_gantt.png)

The contrast between bed types is the point: holding beds carry multi-day occupancies that run together into near-continuous bands, whereas theatre, intensive care and resuscitation occupancies are short enough to appear as isolated marks.

### R2B Hold Bed Saturation: Stream Decomposition and Intervention Analysis

The model records an `r2b_hold_start` attribute for each patient entering the long-duration hold pathway, which allows daily concurrent occupancy to be decomposed by patient stream (disease DNBI, NBI DNBI, WIA) in the analysis pipeline. A companion `r2b_hold_drawn` attribute stores the drawn hold duration at the time of bed seizure, supporting the optional evacuation-threshold logic described below.

**Battle fatigue verification.** Code inspection confirms that battle fatigue casualties (`dnbi_type == 1`) exit the trajectory at R1 through the "Battle Fatigue R1 Hold" branch and never reach R2B hold beds. A `stopifnot` assertion in the analysis pipeline enforces this.

**Structural load calculation.** Under the baseline parameters (171 DNBI in total; 93 disease, 32 NBI, 46 battle fatigue):

- Disease DNBI reaching R2B hold: approximately 74 evacuated (P1: 93 × 0.65 × 0.95 ≈ 57; P2: 93 × 0.20 × 0.90 ≈ 17), less roughly 6% requiring surgery, giving about **70 entering hold-bed recovery** over 30 days (≈ 2.3 per day)
- Non-surgical WIA and NBI reaching R2B hold: approximately 20 over 30 days (≈ 0.7 per day)
- **Total hold entry rate: ≈ 3.0 patients per day**
- Expected hold duration (triangular min = 0.5 d, mode = 5 d, max = 10 d): mean = (0.5 + 5 + 10) / 3 = **5.17 days**
- **Expected concurrent hold occupancy: 3.0 × 5.17 ≈ 15.5 beds** against 10 available (5 per R2B unit × 2 units)

This is a **structural 55% overload**. That is a long-run expectation over the configured means; this run's realised occupancy sits below it, at 7.6 beds on average, because the routing policy exports the excess to R2E rather than letting it accumulate. The saturation cannot be resolved by changes to surgical throughput; it requires an intervention at the holding pathway itself.

![Stacked bar chart of mean concurrent R2B hold bed occupancy on each simulation day, each bar decomposed into the disease DNBI, non-battle injury and wounded in action streams, with a dashed reference line at the five-bed per-unit capacity and bars reaching eleven on two days](../images/r2b_hold_occupancy.png)

Disease DNBI is the largest stream on all but a handful of days and is what carries occupancy above the ten-bed establishment; the dashed line sits at the five-bed capacity of a single R2B unit, so a bar above it is a load neither unit could hold alone. Wounded in action runs between one and five beds and falls away entirely over the closing days, so relieving the hold-bed constraint is a question about disease management rather than about surgical throughput. That disease should dominate a forward holding pathway is consistent with the campaign record, in which non-battle sickness has repeatedly generated admission loads comparable to or larger than those from wounding [[10]](#references).

**Intervention Scenario A: hold duration reduction** (`vars.r2b.holding.mode` in `env_data.json`). Reducing the hold mode from 5 days (7,200 min) to 3 days (4,320 min) reduces expected mean duration from 5.17 to (0.5 + 3 + 10) / 3 = 4.5 days. Expected concurrent occupancy falls from 15.5 to 3.0 × 4.5 = **13.5 beds**, still 35% above the 10-bed capacity. A clinically implausible mode of 1.6 days or less would be required to bring expected occupancy within capacity. Hold duration reduction alone is insufficient to resolve saturation. To test: change `{"var": "mode", "val": 7200}` to `{"var": "mode", "val": 4320}` in the `vars.r2b.holding` activity and re-run 10 or more replications.

**Intervention Scenario B: hold bed expansion** (`elms.r2b.beds.hold.qty` in `env_data.json`). Increasing hold beds from 5 to 10 per R2B unit provides 20 total beds against expected steady-state demand of about 15.5, yielding headroom to absorb the day-to-day variation the arrival process carries. Eight beds per unit (16 total) sits barely above expected demand and would leave none. To test: change `{"name": "hold", "qty": 5}` to `{"name": "hold", "qty": 10}` in the `elms.r2b.beds` array and re-run 10 or more replications.

**Intervention Scenario C: evacuation threshold** (`vars.r2b.holding.evac_threshold` in `env_data.json`). The trajectory supports an optional evacuation threshold in minutes: when `evac_threshold` is set and a patient's drawn hold duration exceeds it, the patient is forwarded to R2E rather than waiting for full recovery at R2B. At a threshold of 3 days (4,320 min) the triangular cumulative distribution gives P(draw > 4,320) = 1 − (4,320 − 720)² / ((14,400 − 720) × (7,200 − 720)) ≈ **85% of hold patients forwarded to R2E early**, effectively eliminating R2B hold saturation. This reduces R2B hold bed occupancy substantially but transfers a non-surgical medical load to the R2E hold and intensive care pathway. What transfers is the unserved remainder of each forwarded casualty's own convalescence rather than a fresh duration drawn on arrival at R2E (see README [R2B Trajectory](../README.md#r2b-trajectory)), so the scenario redistributes a fixed quantity of bed time between the echelons and the R2E load it produces is attributable to the routing change alone. To test: add `{"var": "evac_threshold", "val": 4320}` to the `vars.r2b.holding` activity vals array and re-run 10 or more replications.

**Intervention Scenario D: capacity-aware hold routing (implemented in the shipped model).** A two-tier routing policy manages hold bed allocation. The primary tier operates at R1 before transport begins; the secondary tier operates at R2B on arrival.

**Primary tier, upstream threshold routing (`vars.r2b.holding.hold_threshold`, default 0.8).** `select_r2b_for_hold()` checks whether an R2B unit's hold occupancy is strictly below `hold_threshold × capacity` before routing a patient there. With 5 beds per unit and a threshold of 0.8, a unit is selected only if fewer than 4 beds (80%) are occupied, keeping at least one bed reserved for incoming staging patients. If no R2B unit is below threshold, the patient is routed directly to R2E from R1 (`r2b_bypassed = 1`) without incurring transport to R2B at all. Where `hold_threshold` is absent the function falls back to routing whenever any bed is free. This removes the cascade in which long-duration holders starve new arrivals, because the routing decision is made before transport rather than after the patient has already consumed a hold bed. To test: set `{"var": "hold_threshold", "val": 0.6}` for more aggressive upstream routing, or remove the parameter to restore unconditional routing.

**Secondary tier, at-R2B three-stage policy.** For patients who arrive at R2B, either because the upstream check passed or because a race condition occurred between the routing decision and arrival:

1. **Hold capacity available.** The patient seizes a hold bed immediately (Step 4 No Surgery branch).
2. **Hold full, R2E has capacity.** The patient bypasses to R2E by evacuation-team transport (`r2b_hold_bypass = 1`); this is also the fallback when the queue cap is exceeded.
3. **Both echelons full, queue within cap.** The patient joins the R2B hold queue (`r2b_hold_queued = 1`). The queue cap is floor(R2B\_beds / (R2B\_beds + R2E\_beds) × R2B\_beds) = **2 patients**; above the cap, the policy falls back to stage 2.

The analysis pipeline reports all three routing outcomes: `r2b_pre_bypass_count` (upstream, at R1), `r2b_hold_bypass_count` (at R2B Step 4), and `r2b_hold_queued_count` (queued at R2B when both echelons are saturated).

The overload this policy manages is a property of the shipped establishment rather than of this run. Five hold beds per R2B unit are insufficient to absorb the demand generated by the disease-dominated DNBI mix, and the shortfall is structural: an expected 15.5 concurrent beds against 10 available, derived from an entry rate of about 3.0 patients per day and a mean hold of 5.17 days. No open-access source gives a doctrinal standard for forward medical holding capacity in large scale combat operations, so the establishment itself cannot be checked against an external benchmark, and the finding is conditioned on the disease DNBI proportion, which is itself an informed estimate (see README [DNBI Sub-Type Split](../README.md#dnbi-sub-type-split)). Were the true disease proportion substantially lower, at 30% rather than the modelled share, expected concurrent occupancy would fall to roughly 9 beds and sit within capacity, so the saturation finding is sensitive to that assumption in proportion to it. With capacity-aware routing active, the consequence of the overload is a transfer of load to R2E rather than an accumulation at R2B, which preserves system throughput at the cost of increased R2E medical hold demand. README Further Development entry L4 carries this gap in the model's standing catalogue.

## R2E Heavy Handling

<small>[Return to Top](#contents)</small>

The R2E Heavy is the primary surgical node for the deployed health system, receiving both casualties bypassed directly from R1 and those bypassed onward from an R2B whose theatre was off shift, occupied, or intensive-care saturated. Under seed 42 over 30 days, the R2E performed **171 first surgeries** and **41 second surgeries**. Only a damage control casualty whose abbreviated first operation was performed here returns to theatre for the definitive repair that completes the staged sequence [[11]](#references), which is what keeps the second count a minority of the first: a single-stage casualty needs no second procedure, and a damage control casualty operated on forward at R2B has already had its first stage elsewhere (see README [Surgical Pathway](../README.md#surgical-pathway)).

![Step plots of R2E Heavy queue length over the run, one panel for the four intensive care beds and one for the two operating theatre beds, the intensive care queue never exceeding one and the theatre queue peaking at eight around day 9](../images/r2eheavy_bed_queue_3_teams.png)

The two panels share a vertical scale, which makes the difference between them plain: the theatre queue rises and falls in waves that reach eight casualties on day 9 and six on day 24, while the intensive care queue never exceeds one.

**R2E surgical throughput is bounded by rostered surgical sections as well as by theatre space.** A procedure seizes both an operating theatre and one of the three surgical sections that staff them, and a section carries a 12-hour roster while a theatre is available continuously, so the number of concurrent operations is capped at two during the first shift, when two sections are rostered on, and one during the second, when one is. Utilisation across the three sections was **30.8%, 53.6% and 30.8%** of the time their own rosters had them open; the middle figure is higher because that section is the one rostered to the second shift, and so absorbs the whole of the night-time surgical load on its own. Against 24-hour room time the two theatres ran at **66.6%** and **52.8%**. Operating cannot account for that: the 212 procedures performed here, at the 123.6 minutes of operative asset occupancy per case that registry data for deployed Role 2 and Role 3 facilities gives once its band means are weighted by its own severity mix [[12]](#references), consume about 30% of the two theatres' combined 24-hour availability, roughly half of what the rooms report. The room figure is therefore measuring something other than operating.

**The second-shift surgical section, not the theatres, sets the pace at R2E.** Theatre 1 carried a queue for **46.3%** of the run and Theatre 2 for **34.6%**, and the second-shift section was queued for **2.5%** of its open time against **0.7%** and **0.6%** for the two first-shift sections. Because a casualty seizes a theatre before it seizes a section, a theatre reads as occupied while its casualty is still waiting for staff, so most of the theatre queue figure is casualties waiting on staff rather than on rooms. The second-shift section is the specific constraint, carrying roughly three quarters again the load of either first-shift section and a queue several times longer than either, and theatre contention is set by peak-day volume rather than by mean volume.

**R2E intensive care is busy but not saturated.** Per-bed utilisation across the four intensive care beds is **93.7%, 92.1%, 92.2% and 85.8%**, with a queue present for **26.5%** of the run on the first bed, **13.1%** on the second, **13.5%** on the third and **7.8%** on the fourth. The queue is spread across all four beds rather than standing behind the first, which is the signature of a pool under sustained load rather than one in overflow; discrete event studies of critical care flow report the same progression as demand approaches, without exceeding, bed capacity [[13]](#references). Casualties awaiting strategic evacuation stage in holding beds, and only the ventilated minority of the critical pool holds an intensive care bed at all, for a bounded pre-flight period: **eight** critical-route evacuees drew that path and **four** completed within the run, at a mean of **109.4 hours**, a median of **102.8** and a 90th percentile of **147**. Those holds are long because a ventilated casualty cannot step down while the holding pool is full, and two of this run's four scheduled evacuation sorties drew a cancellation, so the pool stayed full for weeks at a time. The residual pressure on intensive care is therefore clinical demand plus a bed-blocking effect (see [Strategic Evacuation and Role 4 Demand](#strategic-evacuation-and-role-4-demand), and README Further Development entry L17).

Relieving the intensive care constraint moves casualties through the pre-theatre gate rather than around it. The stabilisation phase belongs to the damage control pathway alone, so the cohort passing through the gate's post-operative branch is the damage control casualties rather than every operated casualty: **79 recovered in intensive care** (`post_op_pathway = 1`) and **58 Priority 1 casualties recovered in a holding bed** (`post_op_pathway = 2`) because intensive care was full at the moment of theatre entry; a further **29 Priority 2 and lower casualties had theatre entry deferred** (`surgery_deferred = 1`) until a bed freed. Post-definitive care, which both pathways receive, went to an intensive care bed for **54** casualties and to the degraded holding-bed fallback for **130**. Neither pathway produced a post-operative death of wounds in this run, consistent with the small per-patient probabilities applied at that checkpoint and the small absolute counts characteristic of the Falklands-calibrated baseline; the replicated measurement of this checkpoint, and the stress test confirming that it fires, are reported in [The Post-Operative Intensive Care Gate](Multi_Run_Analysis.md#the-post-operative-intensive-care-gate). The single-run result should not be read as evidence that the holding-bed route is clinically safe, only that 58 casualties on it is too small a sample to resolve a sub-percent mortality difference.

`analyse_run()` (`R/analysis.R`) visualises which casualties, and on which simulation day, received degraded care as a direct consequence of intensive care saturation:

![Stacked bar chart of R2E surgical casualties per simulation day split across three care pathways, normal intensive care access, sub-optimal holding-bed recovery under a Priority 1 override, and theatre entry delayed pending an intensive care bed, with degraded care present on most days from the first onward](../images/r2e_icu_gating_impact.png)

Sub-optimal care, meaning surgery that proceeded despite intensive care saturation with a Priority 1 override to holding-bed recovery, and delayed care, meaning theatre entry deferred pending intensive care availability for Priority 2 and lower casualties, together account for 87 of the 137 R2E surgical casualties and appear on most days from the first onward rather than building up late in the run. Intensive care saturation is therefore a standing condition at the four-bed establishment, not a state the campaign gradually arrives at, and the heaviest degraded days track the heaviest surgical days. `outputs/r2e_icu_gating_daily.csv` and `outputs/post_op_pathway_summary.csv` provide the underlying daily and pathway-level counts.

![Gantt chart of R2E Heavy bed occupancy over the run, one horizontal band per bed coloured by bed type, the thirty holding beds filling one after another over the first five days and staying occupied to the end, above them four intensive care beds heavily but intermittently occupied, two operating theatres showing near-continuous occupancy through the first fortnight, and three resuscitation beds marked only by short scattered episodes](../images/r2eheavy_gantt.png)

The holding beds fill in sequence and are barely released again before the run ends, which is what makes the strategic evacuation backlog visible as a resource state rather than only as a count. The theatre bands are the second thing to note: they run nearly unbroken through the first fortnight, which is the occupancy the 66.6% and 52.8% room figures above measure, and most of it is a casualty holding a room while waiting for a surgical section.

![Bar chart of R2E Heavy surgeries completed on each simulation day, varying between four and twenty-five with two days carrying none](../images/r2eheavy_surgeries.png)

Daily surgical output varies by a factor of six across the run and reaches 25 on day 25, so theatre demand at R2E arrives in bursts rather than at a steady rate.

When examined in system context, the combined theatre capacity of two R2B elements and one R2E Heavy is adequate for a single combat brigade under Falklands-equivalent casualty rates [[6]](#references). If the same system were applied to a deployed division, however, surgical and holding capacity would be grossly insufficient even if only one brigade were assumed to be in contact at any time. The configuration analysed here also carries mass casualty injection disabled and generates casualties at the moderate-intensity rates, so it represents neither a mass casualty event nor the elevated production rates reported for campaigns such as Okinawa or Vietnam [[6]](#references), both of which would expose that deficit; the replicated experiments covering both sit in `docs/Multi_Run_Analysis.md`.

## Casualty Waiting Time

<small>[Return to Top](#contents)</small>

![Scatter plot of each casualty's total waiting time in minutes against the simulation day of arrival, with a fitted trend line, most points lying on the zero line and a scattered upper band reaching 33,000 minutes that thins out after day 20](../images/waiting_time.png)

The distribution is sharply bimodal. Most casualties wait essentially no time at all, and a minority wait days to weeks, those being the casualties queued for a strategic evacuation sortie rather than for clinical care. The upper band falls away after day 20 because a casualty arriving late in the run has less time in which to accumulate a wait before the run ends, so the apparent improvement is an artefact of the finite horizon rather than a recovery.

## Transport Fleet Capacity Margin

<small>[Return to Top](#contents)</small>

![Step plots of transport queue length over the run, one panel per pool, the HX240M panel flat at zero throughout and the PMV Ambulance panel showing brief single-casualty spikes on a handful of days with one spike reaching two around day 4](../images/transport_capacity_margin.png)

Under seed 42 over 30 days, the HX240M queue remains at zero throughout the run and the PMV Ambulance pool queues, reaching two casualties once and standing at one or more for 1.4% of the run on its busiest vehicle. The three-vehicle PMV Ambulance and four-vehicle HX240M pools remain far from binding at the Falklands-derived casualty rate, even with the full round-trip dead-heading model applied, in which each vehicle is held for an unladen return leg back to its originating echelon after casualty drop-off rather than becoming available for the next pickup immediately. Mean utilisation (`outputs/transport_utilisation.csv`) is 14.9% for the PMV Ambulance and 5.1% for the HX240M, so substantial headroom remains on average. The margin is nonetheless not untouched: a queue appearing at all, at 15% mean utilisation, is a property of demand arriving in bursts, and it is the reason the fleet-size sweep should be re-run against the current arrival process (see the README's Further Development entry L19). This plot shows the current single-run margin only; where the margin ends is a question a single run cannot answer, and is addressed by the replicated fleet-size sweep in [Transport Fleet-Size Sweep](Multi_Run_Analysis.md#transport-fleet-size-sweep), which varies vehicle count directly rather than casualty rate or transport duration.

## Return to Duty

<small>[Return to Top](#contents)</small>

Under seed 42 over 30 days, **167 casualties** were assigned a `return_day` attribute, decomposed as follows:

| Echelon | RTD type | Count | Rate (of 530 arrivals) |
|---|---|---|---|
| R1 | battle_fatigue | 44 | 8.3% |
| R1 | clinical | 78 | 14.7% |
| R2B | clinical | 42 | 7.9% |
| R2E | clinical | 3 | 0.6% |
| **Total** | | **167** | **31.5%** |

`bf_rtd` is 44, not the 46 battle fatigue casualties generated, because 2 battle fatigue entities were still within their R1 hold timeout when the 30-day simulation ended and were not assigned a `return_day`. Battle fatigue returns to duty occur exclusively at R1, consistent with the no-R2-routing design. The majority of clinical returns occur at R1 (Priority 3 WIA and NBI completing R1 recovery) and R2B (disease cases discharged from hold beds). R2E clinical returns are very low, at 3, because R2E hold-bed discharge is contingent on completing the drawn recovery-to-duty period, which for most retained casualties extends beyond the 30-day window. The aggregate return-to-duty rate of 31.5% is within the historical range for in-theatre medical treatment facility admissions, 7.6% to 42.1% [[14]](#references), though direct comparison requires accounting for the simulation's 30-day boundary effect.

The share of R2E casualties retained in theatre is a separate quantity from this aggregate, and is an output of the theatre evacuation policy rather than a fixed input (see README [R2E Heavy Trajectory](../README.md#r2e-heavy-trajectory)). Of the **176 casualties reaching R2E disposition** under seed 42, **23.3%** drew an expected recovery within the shipped 30-day policy and were retained, inside the historical range. Because the same drawn duration decides both the disposition and, for those retained, the holding-bed occupancy that follows it, evacuation is strictly a function of severity: sorting the 176 dispositions into quartiles by drawn recovery duration gives evacuation rates of **6.8%, 100%, 100% and 100%** from shortest to longest, where an unconditioned draw would have given the same rate in every quartile.

## Force Regeneration Feedback Loop

<small>[Return to Top](#contents)</small>

Casualty production is coupled to a live, time-varying effective force size rather than to a fixed roll strength, so sustained attrition draws the effective force down over the run. Reinforcement ships disabled (`force_regeneration.reinforcement.demand_interval_days = 0`), so the run below is the pure depletion case. Whether the depletion registers as a falling daily casualty volume, and whether an active reinforcement demand cycle arrests it, are questions about a trend in a noisy series and are answered across replications in [Force Regeneration Under Reinforcement](Multi_Run_Analysis.md#force-regeneration-under-reinforcement).

`analyse_run()` (`R/analysis.R`) always produces a `force_regeneration_plot`, holding `effective_force_combat` and `effective_force_support` against simulation day and faceted by replication where more than one is present, written to `images/force_regeneration.png`. The seed-42 baseline, without reinforcement, is shown below.

![Line chart of effective combat and support force size against simulation day, each pool's solid curve declining across the 30 days below a dashed line marking its initial establishment strength, the combat curve visibly stepped at heavy arrival days and ending near 2,225 of 2,500, the support curve shallower and ending near 1,162 of 1,250](../images/force_regeneration.png)

Both pools decline, with net depletion outweighing return-to-duty regeneration for most of the run, ending the 30-day run at 2,225 of 2,500 initial combat strength (−11.0%) and 1,162 of 1,250 initial support strength (−7.0%), modest in absolute terms at Falklands-calibrated rates. The combat curve is visibly stepped rather than smooth, each step a heavy arrival day.

## Strategic Evacuation and Role 4 Demand

<small>[Return to Top](#contents)</small>

This section presents the Role 4 (national support base) and strategic aeromedical evacuation outputs, with the wait-time died-of-wounds poll active. Of the 530 total casualties generated, 135 reached the strategic evacuation decision (`r2e_evac = 1`); of those, 99 had boarded a sortie and reached Role 4 by the end of the 30-day run, with 36 still queued and occupying an R2E holding bed. The sortie carries the Royal Australian Air Force's published C-17A aeromedical evacuation fit of 36 high dependency and 54 ambulatory places [[15]](#references) (see README [Role 4 (National Support Base) Demand Modelling](../README.md#role-4-national-support-base-demand-modelling)), so the residual 36 is set by which sorties actually flew rather than by seats.

![Stacked bar chart of daily Role 4 bed occupancy split across intensive care, surgical and general wards, flat at zero until day 21, rising to about ninety concurrent patients on days 29 and 30, then decaying to near zero by day 69, with a dotted line marking the end of the 30-day engagement window](../images/role4_census.png)

Occupancy is zero until day 21, because no sortie flew before then, and it then rises to a peak of 90.0 concurrent patients across all wards on day 30, the last day of the campaign. The plot runs on past the engagement window, marked by the dotted line, because the length-of-stay distributions extend well beyond the campaign horizon: the census decays back to near zero only by about day 69. The whole of that tail is demand the national support base carries after the theatre has stopped generating casualties, and the peak tracks the number of evacuation decisions rather than any property of Role 4 itself, which is modelled as unconstrained demand rather than as a capacity.

![Two step plots of the number of casualties awaiting a strategic evacuation sortie over the run, one for the critical route and one for the standard route, both rising steadily to a peak around day 21 and falling sharply when a sortie flies before climbing again](../images/ame_backlog.png)

Both routes accumulate for three weeks before the first sortie flies, and neither is cleared by the two sorties that do fly. The critical backlog stands at about 35 before the day-21 departure, drops to 12, climbs back above 40 by day 28 and ends the run at 24; the standard route follows the same shape and ends at 12. Two sorties therefore cut each backlog without clearing it, and every casualty counted here is holding an R2E bed while waiting.

The two-pool split, separating critical (Priority 1 surgical) demand from standard demand, earns its place because the two pools draw on separate cabin allocations and fill at different rates. Of the 135 evacuation decisions, 85 route to the critical pool and 50 to the standard pool. Two of the four scheduled opportunities drew the 15% cancellation in this run, so the first sortie to fly departed on day 21. Of the critical pool, 61 of 85 decisions had boarded by day 30, leaving 24 waiting, at a mean wait of 10.1 days; of the standard pool, 38 of 50 had boarded, leaving 12 waiting, at the same mean wait. The binding quantity is neither seats nor the nominal sortie interval but the cancellations, and this run is the clearest illustration in the document of why a schedule with idle capacity on average is not the same as a schedule that clears its backlog: two cancellations against a fortnightly effective interval left three weeks of disposition demand accumulating in R2E holding beds.

What the sortie timeline shows is a schedule that cannot recover from a cancellation. The two sorties that flew boarded 36 and 25 casualties from the critical pool and 33 and 5 from the standard pool, against 36 and 54 places each: the day-21 sortie fills the critical cabin exactly and still leaves a queue behind it. An unconstrained theoretical baseline, same-day and uncapped at the airframe's 90-seat total, would have needed 27 sorties across the run; the real schedule flew 2 and moved 99 of the 135 casualties who needed moving. At the sourced airframe capacity the binding quantity is therefore the number of sorties that actually depart, not the seats each one carries, and a single run's cancellation draws move the evacuation figures more than anything else in this section.

Casualties awaiting a sortie hold R2E holding beds for the whole of the wait, so the 36 still queued at the run's end are occupying clinical capacity as well as appearing in a backlog count. That coupling runs in both directions at the shipped establishment: a ventilated critical-route casualty cannot step down from its bounded pre-flight intensive care period while the holding pool is full, which is why the ventilated holds reported in [R2E Heavy Handling](#r2e-heavy-handling) stretched as far as they did in this run. A single pool therefore carries in-theatre recovery and the strategic evacuation wait together, and no intensive care or theatre finding in this document should be read without it. README Further Development entry L17 records the systemic consequence and the replicated occupancy measurements behind it.

The wait-time died-of-wounds poll, a periodic mortality check applied to casualties queued awaiting strategic evacuation, polled correctly against this backlog but recorded no death while awaiting evacuation in this run (`outputs/dow_by_echelon.csv`). The per-poll death probability is deliberately small, so the zero should be read as consistent with the mechanism rather than as evidence about its magnitude: a single run cannot resolve a rare event in either direction.

Because disposition is set by an evacuation policy rather than by a fixed rate, that policy can be swept as a planning lever. Re-running the same 30-day configuration at seed 42 under the 15-day and 60-day policies the source cites as realistic alternatives [[16]](#references) gives the comparison below. The 15-day and 60-day rows come from configurations other than the shipped default and are reported here for contrast only; they do not form part of the baseline evidence set described in [Methods](#evidence-set-and-provenance).

| Policy | In-theatre share | Evacuation decisions | Reached Role 4 | Peak Role 4 occupancy |
|---|---|---|---|---|
| 15 days | 3.0% | 191 | 182 | 122.0 |
| 30 days (shipped) | 23.3% | 135 | 99 | 90.0 |
| 60 days | 73.8% | 34 | 13 | 13.0 |

Lengthening the policy moves casualties off the strategic airlift and onto R2E holding beds, shortening the national support base's queue at the cost of theatre bed-days; shortening it does the reverse. The response is steep, and a planner reading it should note that the in-theatre share at the 15-day policy falls to 3.0%, below the 7.6% lower bound of the historical range, so the model is being pushed outside its validated envelope at that end. At the 15-day policy 182 of the 191 casualties released by the policy reach Role 4 within the run, against 99 of 135 at the shipped policy; that difference is driven not by the policy but by which sorties flew, two of four having been cancelled in the shipped-policy run, which is a reminder that a single run's cancellation draws confound this comparison more than the policy does.

## Limitations

<small>[Return to Top](#contents)</small>

Three classes of limitation bear on how the findings above should be read.

The first is the unit of analysis, set out in [Scope of a Single Run](#scope-of-a-single-run). One run illustrates a mechanism and cannot estimate a mean, so no figure in this document supports a claim about the size of an effect, and the several places where two figures are set beside one another are descriptions of one realisation rather than comparisons of two populations.

The second is the model's own representational gaps. Those are catalogued once, in the README's [Further Development](../README.md#further-development) section, and are not restated here; the entries bearing most directly on this document are listed below by identifier.

| Entry | Bearing on this document |
|---|---|
| L1 | Casualties enter the model at Role 1, so every waiting time reported here excludes the interval from wounding to first contact with the health system, and none is comparable against a doctrinal time-to-surgery standard. |
| L3 | Clinical teams are seized whole, so the surgical section utilisation figures overstate scarcity where a procedure needs only part of a section and understate it where staff are in practice shared. |
| L4 | The R2B holding shortfall is a structural property of the shipped establishment rather than a finding of this run, and it is conditioned on an uncertain disease DNBI proportion. |
| L11 | The theatre and intensive care gating parameters are informed estimates, so the post-operative pathway counts illustrate the mechanism rather than predicting mortality. |
| L16 | Role 4 is a demand calculation without capacity, so its census is a demand signal and not a claim that the base can absorb that demand. |
| L17 | R2E holding beds carry in-theatre recovery and the strategic evacuation wait on one pool, so no intensive care or theatre finding here should be read in isolation from holding-pool occupancy. |
| L23 | The severity factors setting recovery duration are uncalibrated, so the evacuation policy comparison is of uncertain steepness. |
| L26 | One surgery duration distribution serves every casualty regardless of severity, so theatre contention is understated on exactly the heavy, high-acuity days the queue figures are drawn from. |

The third is specific to the verification exercise itself. Establishing that the figures published here are what the current code produces says nothing about whether the model is calibrated to the campaign it represents. That question is tracked separately under Further Development entries L12 and L22, and is addressed, so far as the available historical anchors allow, in the README's [Parameter Calibration](../README.md#parameter-calibration) section.

## Conclusion

<small>[Return to Top](#contents)</small>

This walk-through, viewed in its entirety, demonstrates that the modelled deployed health system is capable of sustaining a steady operational tempo for a single brigade under baseline casualty assumptions derived from the Falklands conflict. Role 1 elements show sufficient responsiveness and throughput, and the dual-node R2B configuration absorbs surgical demand effectively through a combination of forward surgery and bypass routing to R2E.

Two system constraints are identified. At R2B, holding bed capacity is the binding constraint: the ten beds average 7.6 concurrent occupants and reach or exceed full capacity on 10 days of the run, driven by disease DNBI evacuees occupying hold beds for multi-day durations. Stream decomposition confirms disease DNBI as the dominant load, with an expected concurrent hold occupancy of about 15.5 beds against a 10-bed capacity, a structural mismatch of 55% that surgical throughput adjustment cannot address. That the shortfall appears only intermittently as an R2B queue is a consequence of the capacity-aware routing policy, which diverted 179 casualties upstream to R2E before transport; the demand is displaced rather than absorbed. Hold bed expansion, to at least 10 beds per unit, or an evacuation threshold policy are the indicated interventions. Operating theatre capacity is not a constraint at R2B, which runs at 9.6% to 9.7% against 24-hour room time and 19.1% to 19.3% against shift time.

**The primary binding constraint at R2E is the surgical roster, with intensive care a close second.** The four intensive care beds operate at 85.8% to 93.7% utilisation and each carries a queue for between 8% and 27% of the run; the three surgical sections run at 30.8%, 53.6% and 30.8% of their rostered time, and the second-shift section carries the night-time load alone. The R2E Heavy performs 171 first surgeries and 41 second surgeries in this run, against 69 at R2B, and its two theatres carry a queue for 46.3% and 34.6% of the run, almost all of it casualties holding a room while waiting for staff. Strategic airlift is constrained in this run by sortie cancellations rather than by seats: two of four scheduled sorties did not fly, and 36 casualties were still queued at the run's end. Three system levers are indicated: R2B holding bed expansion or a higher evacuation threshold from R2B holding, a second-shift surgical section at R2E, and additional R2E holding beds, which currently carry in-theatre recovery and the residual evacuation wait on one pool. The pre-theatre intensive care gate adds no capacity; what it does is make the consequence of the existing shortfall explicit in the model's mortality output rather than absorbing it silently into intensive care queue time.

This walk-through characterises one campaign under the Falklands-modified baseline. Whether its findings generalise, and how the system responds under a materially higher casualty rate, is addressed directly and quantitatively in [Comparative Scenario Analysis](Multi_Run_Analysis.md#comparative-scenario-analysis) (`docs/Multi_Run_Analysis.md`), which compares this baseline against an Okinawa-intensity profile across at least 30 replications with 95% confidence intervals. That document is also where the model's replicated policy experiments are reported: the post-operative intensive care gate, the forward intensive care share frontier, the transport fleet-size sweep, the reinforcement comparison and the mass casualty stress test.

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Ucar, I., Smeets, B., & Azcorra, A. (2019). simmer: Discrete-Event Simulation for R. *Journal of Statistical Software*, *90*(2), 1–30. Retrieved 27 Aug 26, from https://doi.org/10.18637/jss.v090.i02

[2] NATO Standardization Office. (2019). *AJP-4.10 Allied Joint Doctrine for Medical Support* (Edition C, Version 1). NATO Standardization Office. Retrieved 27 Aug 26, from https://www.coemed.org/files/stanags/01_AJP/AJP-4.10_EDC_V1_E_2228.pdf

[3] Rossetti, M. D. *Simulation Modeling and Arena*, Chapter 5: Statistical Analysis for Infinite Horizon Simulation Models. Retrieved 27 Aug 26, from https://rossetti.github.io/RossettiArenaBook/05-Chapter5.html

[4] Sargent, R. G. (2010). Verification and validation of simulation models. In *Proceedings of the 2010 Winter Simulation Conference* (pp. 166–183). IEEE. Retrieved 27 Aug 26, from https://www.informs-sim.org/wsc10papers/016.pdf

[5] Law, A. M. (2020). Statistical analysis of simulation output data: the practical state of the art. In *Proceedings of the 2020 Winter Simulation Conference* (pp. 1117–1127). INFORMS Simulation Society. Retrieved 27 Aug 26, from https://informs-sim.org/wsc20papers/134.pdf

[6] Blood, C. G., Zouris, J. M., & Rotblatt, D. (1998). *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

[7] Remondelli, M. H., Remick, K. N., Shackelford, S. A., Gurney, J. M., Pamplin, J. C., Polk, T. M., Potter, B. K., & Holt, D. B. (2023). Casualty care implications of large-scale combat operations. *Journal of Trauma and Acute Care Surgery*, *95*(2S), S180–S184. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC10389308/

[8] Fandre, M. (2020). Medical changes needed for large-scale combat operations: observations from Mission Command Training Program warfighter exercises. *Military Review*. Retrieved 27 Aug 26, from https://www.armyupress.army.mil/Journals/Military-Review/English-Edition-Archives/May-June-2020/Fandre-Medical-Changes/

[9] Kotwal, R. S., Montgomery, H. R., Kotwal, B. M., Champion, H. R., Butler, F. K., Mabry, R. L., Cain, J. S., Blackbourne, L. H., Mechler, K. K., & Holcomb, J. B. (2011). Eliminating preventable death on the battlefield. *Archives of Surgery*, *146*(12), 1350–1358. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC5832013/

[10] Black, J. (2002). Acute appendicitis in Japanese soldiers in Burma: support for the "fibre" theory. *Gut*, *51*(2), 297. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC1773321/

[11] Beldowicz, B. C. (2018). The evolution of damage control in concept and practice. *Clinics in Colon and Rectal Surgery*, *31*(1), 30–35. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC5787400/

[12] Hall, A., Graham, B., Hanson, M., & Stern, C. (2023). Surgical capability utilization time for military casualties at role 2 and role 3 facilities. *Military Medicine*, *188*(11–12), e3368–e3370. Retrieved 27 Aug 26, from https://academic.oup.com/milmed/article/188/11-12/e3368/6961509

[13] Williams, E., Szakmany, T., Spernaes, I., Muthuswamy, B., & Holborn, P. (2020). Discrete-event simulation modeling of critical care flow: new hospital, old challenges. *Critical Care Explorations*, *2*(9), e0174. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC7491890/

[14] Izaguirre, M. K., Cox, D., Lodi, P. C., Giraud, R. S., Murray, C. K., Teyhen, D. S., Capaldi, V. F., Kelly, K. M., Taylor, J. F., Holland, J. C., & Laragione, V. J. (2025). To conserve fighting strength in large scale combat operations. *Military Review Online*. Retrieved 20 Jul 25, from https://www.armyupress.army.mil/Journals/Military-Review/Online-Exclusive/2025-OLE/Conserve-Fighting-Strength-in-LSCO/

[15] Royal Australian Air Force. *Aeromedical evacuation*. Australian Government, Department of Defence. Retrieved 27 Aug 26, from https://www.airforce.gov.au/our-work/humanitarian-support/aeromedical-evacuation

[16] U.S. Army Medical Department Center and School. *Health Service Support in a Theater of Operations*, Subcourse MD0002, Evacuation Policy. Retrieved 02 Aug 26, from http://armymedical.tpub.com/MD0002/Evacuation-Policy-Health-Service-Support-in-a-Theater-of-Operations-88.htm

<!-- REFERENCES END -->
