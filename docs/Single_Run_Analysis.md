# Applying Discrete Event Simulation to the Land-Based Trauma System: Baseline Performance and System Constraints in a Single Campaign

## Abstract

<small>[Return to Top](#contents)</small>

**Background**

High-intensity Large Scale Combat Operations (LSCO) overload forward medical infrastructure, threatening survival from severe combat trauma. Discrete event simulation provides a quantitative method to evaluate system performance and structural interventions across battlefield trauma treatment systems.

**Objective**

To model a land-based trauma system and evaluate its performance in handling historical moderate-intensity warfare casualty rates (based on the 1982 Falklands conflict, modified), enabling planners to identify opportunities for system optimisation.

**Methods**

A discrete event simulation was constructed reflecting an Australian brigade-sized force supported by three Role 1 treatment teams, two Role 2 Basic (R2B) facilities and one Role 2 Enhanced Heavy (R2E Heavy) hospital. The simulation generated casualties over a 30-day period based on distribution rates recorded from the Falklands 1982 conflict, with modified outcomes for modern casualty treatment efficacy, and their handling within the land-based trauma system.

**Results**

The simulation generated 495 casualties across 30 days. Two constraints emerged within the system. Holding beds at the two R2B were heavily utilised and reached capacity on 13 of 30 days, filled by disease casualties. At the R2E Heavy, holding beds carry both in-theatre recovery and the wait for evacuation, though in this run every scheduled sortie flew and nothing waited. Significant queuing was observed for access to surgery in periods with modest casualty spikes, arising because casualties held an operating theatre while waiting for a rostered surgical section rather than because theatre space was scarce, suggesting that surgical capacity would quickly become a constraint at rates above those used in this simulation.

**Conclusion**

The modelled system sustains a single brigade at Falklands-equivalent casualty rates. The single run simulation suggests capacity could be improved with additional surgical time rostered. Additionally, increases to holding capacity or different approaches to holding policies than those simulated here may improve holding bed availability for more critical casualties. Whether these constraints hold across replications, and how the system responds at higher casualty rates, is addressed in `docs/Multi_Run_Analysis.md`.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Methods](#methods)
  - [Model and Run Configuration](#model-and-run-configuration)
  - [Scope of a Single Run](#scope-of-a-single-run)
  - [Environment](#environment)
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

The simulation is a discrete event model built on the `simmer` package for R [[1]](#references). Each casualty is an entity that arrives, then claims and releases clinical staff, beds, operating theatres and transport as it moves rearward through the echelons of care. Those echelons follow allied medical support doctrine [[2]](#references): Role 1 for primary care and resuscitation forward, Role 2 Basic (R2B) for damage control surgery and short-term holding, and Role 2 Enhanced Heavy (R2E Heavy) for definitive surgery, intensive care and in-theatre recovery. Beyond them sits strategic aeromedical evacuation to a Role 4 national support base.

The analysis uses the simulation's shipped default health system: a representative combat brigade served by three Role 1 treatment teams, two R2B facilities and one R2E Heavy hospital. That establishment is an input a planner sets, not a fixed property of the model. The number of elements, and the teams and beds inside each one, are defined in the `elms` structure of `env_data.json` and can be edited there directly or through the Shiny Configure panel.

No warm-up period is discarded. Discarding one removes the settling-in behaviour a model shows before it reaches steady state, which is worth doing only where steady state is the quantity of interest [[3]](#references); this is a terminating simulation of a fixed campaign length from an empty system, and that opening period is what a planner needs to see, because a deploying health system really does start empty.

Unless the text says otherwise, every figure below comes from one run of that configuration under the settings in the table.

| Setting                   | Value                                                                  |
| ------------------------- | ---------------------------------------------------------------------- |
| Scenario profile          | `moderate_intensity` (Falklands 1982-modified)                         |
| Random seed               | 42                                                                     |
| Campaign horizon          | 30 simulated days                                                      |
| Replications              | 1                                                                      |
| Warm-up exclusion         | None (`WARM_UP_DAYS = 0L`)                                             |
| Reinforcement             | Enabled (`force_regeneration.reinforcement.demand_interval_days = 7`)  |
| Sortie cancellation       | Disabled (`role4.ame.failure_probability = 0`)                         |
| Mass casualty injection   | Disabled (`mass_casualty.event.rate_per_day = 0`)                      |
| Theatre evacuation policy | 21 days                                                                |
| Invocation                | `Rscript run.R --seed 42 --days 30 --iterations 1`                     |

### Scope of a Single Run

This document reports a single 30-day run, undertaken to identify where a representative land-based trauma system might be improved. It traces one campaign end to end, with casualties generated at rates modelled on those observed in the Falklands 1982 conflict and routed through the system by documented handling rules representing operational health policy. In doing so it verifies that the model behaves as its specification describes, which is a separate question from whether that specification fairly represents the real system [[4]](#references), and it shows where load gathers and which parts of the system warrant closer investigation.

One run can do that but no more. Each arrival stream draws its daily rate from a distribution before placing arrivals within the day, so a 30-day run is a single draw from a wide distribution and carries no interval [[5]](#references). Every figure below should be read as an illustration of how a mechanism works rather than as an estimate of an average, and no comparison between two figures here is a test of a hypothesis. The horizon bounds the findings as firmly as the replication count does: 30 days is long enough to show which resources come under load and how the handling rules respond, and too short to establish whether a load that is present throughout is a standing property of the establishment or the opening phase of an accumulation, so no claim below is made about how the system behaves over a longer campaign. Anything this project reports with a confidence interval is in `docs/Multi_Run_Analysis.md`, and the sections below point to it wherever a replicated measurement of the same quantity exists.

### Environment

The run was made in the development container defined in the Battlefield-Casualty-Handling repository, `rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`, so no figure below carries a caveat about the environment that produced it. That run reproduces the repository's tracked baseline evidence set byte for byte, both the console log and every arrival diagnostic, and the reproduction is re-checked whenever the model changes.

---

## Simulation Casualty Generation

<small>[Return to Top](#contents)</small>

**The defining feature of this run was that casualty demand arrived in bursts rather than at a steady rate, and every constraint reported in the sections that follow is a consequence of that.** The run generated 495 casualties over 30 days, ranging from 4 on the lightest day to 38 on the heaviest, so no echelon below can be sized against the average alone. The daily rates each stream draws from come from the Falklands 1982 campaign as tabulated in the FORECAS casualty projection study [[6]](#references), modified as the README's [Parameter Calibration](../README.md#parameter-calibration) section records.

![Three stacked bar charts of daily casualty arrivals over the 30-day run, the same daily totals decomposed first by casualty type (WIA, KIA, DNBI), then by population source (combat, support), then by triage priority (Priority 1 to 3 and KIA)](../images/casualty_summary.png)

The three panels split one arrival series three ways, so their daily totals match. Combat casualties made up most of every peak.

| casualty_type | population_source | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | total |
| :-------------- | :------------------ | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ------: |
| dnbi | cbt | 4 | 2 | 21 | 3 | 3 | 2 | 4 | 15 | 4 | 4 | 11 | 5 | 9 | 2 | 9 | 4 | 3 | 5 | 0 | 7 | 1 | 3 | 5 | 29 | 2 | 6 | 2 | 5 | 2 | 7 | 179 |
| dnbi | spt | 1 | 0 | 2 | 1 | 0 | 0 | 2 | 0 | 3 | 1 | 0 | 0 | 1 | 0 | 1 | 1 | 0 | 1 | 0 | 5 | 0 | 4 | 4 | 0 | 6 | 2 | 0 | 2 | 1 | 2 | 40 |
| kia | cbt | 0 | 0 | 3 | 2 | 0 | 0 | 1 | 1 | 0 | 2 | 0 | 0 | 0 | 4 | 0 | 0 | 5 | 0 | 0 | 2 | 0 | 1 | 1 | 0 | 4 | 0 | 0 | 1 | 7 | 0 | 34 |
| kia | spt | 0 | 1 | 1 | 5 | 3 | 0 | 0 | 3 | 0 | 0 | 0 | 0 | 2 | 0 | 1 | 3 | 0 | 0 | 2 | 3 | 0 | 0 | 0 | 0 | 0 | 4 | 0 | 2 | 0 | 0 | 30 |
| wia | cbt | 14 | 3 | 0 | 11 | 0 | 2 | 0 | 5 | 0 | 9 | 2 | 0 | 7 | 3 | 1 | 1 | 2 | 5 | 6 | 8 | 0 | 1 | 2 | 0 | 0 | 6 | 20 | 0 | 2 | 6 | 116 |
| wia | spt | 0 | 0 | 6 | 2 | 0 | 0 | 7 | 7 | 2 | 1 | 4 | 1 | 0 | 0 | 4 | 0 | 4 | 8 | 4 | 0 | 9 | 1 | 1 | 2 | 11 | 5 | 16 | 0 | 0 | 1 | 96 |
| Total |  | 19 | 6 | 33 | 24 | 6 | 4 | 14 | 31 | 9 | 17 | 17 | 6 | 19 | 9 | 16 | 9 | 14 | 19 | 12 | 25 | 10 | 10 | 13 | 31 | 23 | 23 | 38 | 10 | 12 | 16 | 495 |

The table sorts the run's casualties into three categories, Wounded in Action (WIA), Killed in Action (KIA), and Disease and Non-Battle Injury (DNBI), and by whether they came from combat or support forces. The run produced 495 casualties in all. Combat elements accounted for most of them (329), as their greater exposure to risk would suggest. WIA cases totalled 212 and were split far more evenly than the force ratios alone would suggest, 116 on combat personnel against 96 on support. These casualties usually need care at more than one echelon, including resuscitation, surgery and post-operative holding, and so place steady demand on Role 1 and Role 2. DNBI accounted for 219 cases, the largest of the three categories, a reminder that sickness and non-combat injury persist even in high-intensity operations; historically they rival or exceed battle injury in lost duty days and medical resources consumed [[7]](#references). KIA numbered 64.

That spread was the arrival process working as configured rather than a quirk of this seed. Daily totals averaged 16.5 with a standard deviation of 8.8, and the combat WIA stream alone delivered 20 casualties on day 27 and none at all on nine other days. The heaviest day is day 27 at 38 casualties, a little over twice the mean. Each stream draws its rate once per simulated day from a distribution whose standard deviation is sourced alongside its mean, then places the arrivals within the day as a Poisson process (see the README's [Casualty Generation](../README.md#casualty-generation)), so the spread between the lightest and heaviest days is a property of the configured distribution rather than of any scripted event; the typical behaviour across many such draws is pinned down in [Multi-Run Analysis](Multi_Run_Analysis.md).

For a planner this points to treatment capacity that can scale, surgical capability spread across more than one location, and serious attention to preventing disease and non-battle injury, all sized against the peak days rather than the mean.

| population_source | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | total |
|:----------------- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | -----:|
| cbt               | 18 | 5 | 24 | 16 | 3 | 4 | 5 | 21 | 4 | 15 | 13 | 5 | 16 | 9 | 10 | 5 | 10 | 10 | 6 | 17 | 1 | 5 | 8 | 29 | 6 | 12 | 22 | 6 | 11 | 13 | 329 |
| spt               | 1 | 1 | 9 | 8 | 3 | 0 | 9 | 10 | 5 | 2 | 4 | 1 | 3 | 0 | 6 | 4 | 4 | 9 | 6 | 8 | 9 | 5 | 5 | 2 | 17 | 11 | 16 | 4 | 1 | 3 | 166 |
| Total             | 19 | 6 | 33 | 24 | 6 | 4 | 14 | 31 | 9 | 17 | 17 | 6 | 19 | 9 | 16 | 9 | 14 | 19 | 12 | 25 | 10 | 10 | 13 | 31 | 23 | 23 | 38 | 10 | 12 | 16 | 495 |

The second table splits the casualties by source: combat forces (cbt) and support forces (spt). Of the 495 generated, 329 (about 66%) came from combat elements and 166 (34%) from support units, which follows from the size of each population and the higher rates the combat streams are configured at. Support casualties appear in every period of the run, a reminder that rear-area personnel are exposed too in large scale combat operations, especially under indirect fire, poor situational awareness and disrupted evacuation [[8]](#references). The two populations peak at different times: the support stream's worst day is day 25, at 17 casualties against a combat count of 6, while the combat stream's worst day is 24, at 29 against a support count of 2. A surge in one is therefore no guide to the other, and rear-area medical coverage cannot be planned as a fixed fraction of forward demand.

Medical coverage therefore has to reach both forward and rear areas. Role 1 teams need to be positioned to reach combat casualties quickly, while Role 2 facilities have to absorb and triage support casualties, who often present differently, with disease, non-battle injury and trauma that has taken time to reach care. Carrying both populations calls for capacity that can scale, evacuation pathways that can be re-routed, and command and control firm enough to keep casualties moving.

| priority_group | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | total |
| :--------------- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ------: |
| Priority 1 | 12 | 2 | 24 | 14 | 2 | 2 | 10 | 18 | 5 | 8 | 13 | 5 | 11 | 3 | 11 | 5 | 5 | 10 | 6 | 16 | 6 | 5 | 8 | 20 | 14 | 15 | 22 | 4 | 4 | 12 | 292 |
| Priority 2 | 7 | 2 | 3 | 3 | 0 | 2 | 3 | 2 | 3 | 5 | 3 | 1 | 3 | 1 | 2 | 1 | 3 | 5 | 1 | 2 | 0 | 3 | 0 | 5 | 2 | 2 | 12 | 2 | 1 | 1 | 80 |
| Priority 3 | 0 | 1 | 2 | 0 | 1 | 0 | 0 | 7 | 1 | 2 | 1 | 0 | 3 | 1 | 2 | 0 | 1 | 4 | 3 | 2 | 4 | 1 | 4 | 6 | 3 | 2 | 4 | 1 | 0 | 3 | 59 |
| KIA | 0 | 1 | 4 | 7 | 3 | 0 | 1 | 4 | 0 | 2 | 0 | 0 | 2 | 4 | 1 | 3 | 5 | 0 | 2 | 5 | 0 | 1 | 1 | 0 | 4 | 4 | 0 | 3 | 7 | 0 | 64 |
| Total | 19 | 6 | 33 | 24 | 6 | 4 | 14 | 31 | 9 | 17 | 17 | 6 | 19 | 9 | 16 | 9 | 14 | 19 | 12 | 25 | 10 | 10 | 13 | 31 | 23 | 23 | 38 | 10 | 12 | 16 | 495 |

Priority 1 casualties, those needing immediate life-saving treatment, account for 292 of the total (59.0%). That such casualties are the largest group is the reason doctrine places Role 1 assets well forward, able to triage and stabilise quickly. Priority 1 arrivals reach 24 on day 3 against a 30-day average of 9.7, so the high-acuity load is heavy overall and very uneven day to day, which is the pattern that sizes resuscitation and surgical throughput.

Priority 2 and Priority 3 casualties, who need delayed or routine care, accounted for 80 cases (16.2%) and 59 (11.9%). The run also generated 64 KIA (12.9%), who add little to the medical workload but carry considerable operational weight.

This acuity profile carries three implications for how the system is designed:

- Role 1 facilities need to triage and stabilise at volume, and to move Priority 1 casualties rearward quickly.
- Role 2 facilities need bed space and surgical capability they can re-allocate, particularly during sustained operations.
- The evacuation system needs to keep casualties of mixed acuity moving continuously, with clear prioritisation and enough spare capacity to absorb disruption.

## R1 Handling

<small>[Return to Top](#contents)</small>

**Role 1 was not a constraint in this run: every casualty was triaged and treated on arrival, and nothing queued.** Priority 1 casualties were stabilised and moved rearward before their condition could deteriorate, and lower-priority casualties were treated and prepared for movement as required. One caveat bears on that result: the model does not fully represent shortages of evacuation assets, so nothing at Role 1 was held up waiting for transport, and a more detailed evacuation model may yet expose delays this run does not show. Read with that caveat, the three Role 1 teams are the one part of the establishment this run gives a planner no reason to change.

![Step plots of queue length over the 30-day run for each clinical role at each of the three R1 teams, every panel flat at zero across the whole run](../images/r1_queues.png)

No R1 role held a queue at any point in the run, including on day 27, when 38 casualties arrived. Every panel is flat at zero across its whole range, which is the clearest single statement this run makes about Role 1: the constraint the campaign found lies behind it, not at it.

## R2B Handling

<small>[Return to Top](#contents)</small>

**R2B's binding constraint was holding beds rather than surgery, and what limited its forward surgical capacity was the roster rather than the theatres.** Both findings point at the establishment a planner sets rather than at anything the run did on the day.

![Three stacked bar charts per simulation day at R2B: casualties treated at each of the two R2B stations, surgeries started at each station, and casualties bypassing R2B altogether](../images/r2b_handling.png)

The two stations shared the treatment and surgical load on most days, and neither carried it alone for long. Treatment volume tracks the arrival series directly, reaching 30 casualties on day 24 and 21 on day 29 against three or fewer on nine other days, while surgeries held between zero and five throughout. The bypass panel is the one that moves independently of both, jumping on days 1, 24 and 29, so bypassing was a response to congestion and to the shift clock rather than a steady share of the flow.

Operating theatres are modelled as rooms available around the clock, while the surgical section that staffs them works a 12-hour shift. In this run 253 casualties reached the R2B surgical decision point: 77 were operated on at R2B and 176 were bypassed to R2E. The theatres were in use 10.1% (T1) and 10.5% (T2) of the 24-hour day, and 19.9% and 20.2% of the time their section was rostered on. No queue for a theatre formed at any point, which is the bypass logic working as designed. What limited forward surgery was not the number of casualties reaching the decision point but the single theatre and single rostered section at each unit, which have no way to spread a peak.

**Why casualties bypassed.** A single bypass count hides two different causes, the surgical section being off shift and the theatre being busy or already queued, and the model records which applied to each casualty. That record covers the 110 casualties who reached an R2B unit and were turned away there; the remaining 66 of the 176 were routed past R2B upstream, at Role 1, and never faced the theatre decision at all. Of the 110, 93 (85%) were bypassed because the surgical section was off shift and 17 (15%) because the theatre was busy or queued. The 12-hour shift window, not the physical theatre, is therefore what limits forward surgery: for half of every day, a casualty arriving at either R2B unit cannot be operated on there no matter how many beds are free, and goes to R2E instead. Time to surgery is one of the strongest determinants of survival after severe battlefield injury [[9]](#references), so a planner should read this as the loss of forward surgery for half of each day rather than as a low utilisation figure.

**The pre-open hold.** Not every casualty arriving off shift is sent away. One who finds the theatre free and the section due to open within the pre-open window, shipped at 60 minutes, is taken into the theatre and waits there for the section to arrive (see [R2B Trajectory](../README.md#r2b-trajectory)). Ten casualties were held this way, all ten were operated on forward, and they waited 24.1 minutes on average and 44.8 at most. Ten over 30 days is the order a 60-minute window should catch out of a 720-minute closed period: the hold reaches back into the last hour before opening and no further. The off-shift bypass count above is what remains after those ten. How much the window moves that count is measured across replications in [Multi-Run Analysis](Multi_Run_Analysis.md#the-r2b-pre-open-hold-window), one run being too coarse to tell the movement apart from sampling variation.

![Two-colour stacked bar chart of R2B operating theatre bypasses on each simulation day, separating bypasses caused by the surgical section being off shift from those caused by the theatre being busy or queued](../images/r2b_ot_bypass_reason.png)

Off-shift bypasses accounted for most of the count on most days, appearing on 21 of the 30 and reaching 13 on day 29 and 10 on day 1. Theatre-busy bypasses appear on 14 days, never more than four at a time, and make up the whole count on only three of them. The shift window is therefore the constant, day-to-day constraint and theatre congestion an occasional one sitting on top of it.

Two ways of closing that gap suggest themselves, extending the existing section's shift or fielding a second surgical section at each R2B unit to cover the other half of the day, and neither is evaluated here. Longer shifts cannot be assessed fairly without a model of clinician fatigue and the errors and complications that come with it, which the simulation does not have; reporting the extra throughput without that counterweight would overstate what the change is worth. A second team is an establishment decision and a resourcing question for planners, not something the simulation should test as though it were free. Both are worth a follow-up scenario test once a fatigue model exists or a change to the establishment is directed.

**Holding beds were the main constraint the run identified, and the ten of them ran close to full throughout.** Occupancy climbed from 2 beds on day 1 to 9 within three days and stayed there, averaging 7.9 of the 10 beds across the run. It reached ten or more on 13 days, nine or more on 18 of the 30, and went past the establishment on 4 days, peaking at 11 on day 11. What filled these beds was disease DNBI casualties staying for days at a time, most commonly 5, rather than patients recovering from surgery.

A queue did form on the busiest holding beds, reaching three casualties at its peak. That was the routing policy running out of room on the peak days rather than a sign of spare capacity on the others. Before a casualty is moved at all, an upstream check sends them to R2E whenever no R2B unit has hold occupancy below 80%, and it did so 176 times in this run, with two more diverted on arrival and none queued while both echelons were full. The shortfall analysed in the next section is therefore real, but it was mostly exported to R2E as extra medical holding and intensive care load rather than piling up as a queue anyone would see at R2B.

![Step plots of queue length over the run for every bed at each of the two R2B units, showing queues of up to three casualties on the holding beds and brief single-casualty queues on the resuscitation beds](../images/r2b_bed_queues.png)

Only holding and resuscitation beds queued at all, with the holding beds carrying a queue for between 15% and 52% of the run. No holding bed's queue exceeded three casualties, and the resuscitation queues at both units were brief single-casualty spikes.

![Gantt chart of bed occupancy at each of the two R2B units over the run, with one horizontal band per bed coloured by bed type, the five holding beds almost continuously occupied and the operating theatre, intensive care and resuscitation beds showing short scattered episodes](../images/r2b_gantt.png)

The contrast between bed types is what to look at: holding beds carried stays of several days that run together into almost unbroken bands, while theatre, intensive care and resuscitation stays were short enough to appear as isolated marks.

### R2B Hold Bed Saturation: Stream Decomposition and Intervention Analysis

**The hold bed shortfall belongs to the shipped establishment rather than to this run: the disease stream alone implies about 16 beds in use against the 10 fielded, a structural overload of 60% that no change to surgical throughput can close.** The run averaged 7.9 beds, less than that, only because the routing policy pushed the excess to R2E instead of letting it build.

The daily occupancy behind that figure can be split by stream, because the model records when each casualty enters the long-stay hold pathway and which stream they came from. Battle fatigue casualties do not appear in it at all: they leave the model at Role 1 and never occupy an R2B hold bed, which the analysis pipeline asserts rather than assumes.

The derivation, under the baseline parameters and this run's 219 DNBI casualties (123 disease, 48 NBI, 48 battle fatigue), runs as follows:

- Disease DNBI reaching R2B hold: approximately 98 evacuated (P1: 123 × 0.65 × 0.95 ≈ 76; P2: 123 × 0.20 × 0.90 ≈ 22), less roughly 6% requiring surgery, giving about **92 entering hold-bed recovery** over 30 days (≈ 3.1 per day)
- Expected hold duration (triangular min = 0.5 d, mode = 5 d, max = 10 d): mean = (0.5 + 5 + 10) / 3 = **5.17 days**
- **Expected concurrent occupancy from the disease stream alone: 3.1 × 5.17 ≈ 16.0 beds** against 10 available (5 per R2B unit × 2 units)
- The non-surgical wounded in action and non-battle injury streams sit on top of that. They cannot be derived the same way, because the run's own figure for them is already suppressed by the routing policy, but even after 176 casualties had been diverted upstream they contributed a further 4.1 beds on average, so the true demand is higher again than the disease term alone.

![Stacked bar chart of mean concurrent R2B hold bed occupancy on each simulation day, each bar decomposed into the disease DNBI, non-battle injury and wounded in action streams, with a dashed reference line at the five-bed per-unit capacity and bars reaching eleven on day 11](../images/r2b_hold_occupancy.png)

Disease DNBI was the largest stream over the opening week and again over the closing third of the run, averaging 3.8 beds against 2.9 for wounded in action and 1.2 for non-battle injury, and it is the stream that both opens and closes the campaign's occupancy. The dashed line marks the five beds a single R2B unit holds, so any bar above it is a load neither unit could carry alone. The wounded in action stream peaked at 7 beds in the middle of the run and had cleared entirely by the closing days, so relieving the hold-bed constraint over a sustained campaign is a question of managing disease rather than of surgical throughput. Disease dominating a forward holding pathway matches the campaign record, in which sickness has repeatedly produced admission loads as large as or larger than wounding [[10]](#references).

**Three remedies are available to a planner, and the arithmetic above rules one of them out.** Shortening the hold does not work at all: moving the most common stay from 5 days to 3 brings the expected duration down to 4.5 days and the disease stream's expected occupancy to 14.0 beds, still 40% over the 10 available, and even a most-common stay of half a day, the shortest the distribution permits, leaves 11.3 beds and does not fit inside capacity. Adding beds works but is expensive: 10 per unit gives 20 against a disease demand of about 16.0 plus the wounded in action and non-battle injury load on top of it, which is barely enough headroom to absorb the day-to-day swings the arrival process produces, whereas 8 per unit sits at or below expected demand and leaves no margin at all. An evacuation threshold is the cheapest and the most consequential: sending on any casualty whose drawn recovery exceeds three days moves roughly 85% of hold patients forward to R2E and all but removes the R2B saturation, at the price of transferring a non-surgical medical load onto R2E holding and intensive care. What transfers is the unserved remainder of each casualty's own recovery rather than a fresh duration drawn on arrival (see README [R2B Trajectory](../README.md#r2b-trajectory)), so the change moves a fixed quantity of bed time between echelons rather than creating more of it. Each of the three is a configuration change a planner can make and test across replications; none is evaluated here, one run being too coarse to separate the effect from sampling variation.

**A fourth response is already running in the shipped model, which is why the shortfall shows up as transferred load rather than as a backlog.** Hold beds are allocated by a two-tier capacity-aware policy. The first tier acts at Role 1, before transport begins: a casualty is sent to an R2B unit only while that unit's hold occupancy is below 80% of capacity, so with 5 beds per unit at least one is kept free for casualties staging through, and where neither unit qualifies the casualty goes straight from R1 to R2E and is never transported to R2B at all. Deciding before transport rather than after the casualty has taken a bed is what stops long-stay patients from crowding out new arrivals.

The second tier acts at R2B itself, on casualties who arrive because the upstream check passed or because occupancy changed in the meantime. A free hold bed is taken at once. Where hold is full and R2E has room, the casualty moves on to R2E by evacuation-team transport. Where both echelons are full, the casualty joins a short R2B hold queue, capped at two, beyond which they move on to R2E instead. This run recorded 176 upstream diversions at R1 and two diversions on arrival at R2B, and no casualty reached the last of those states.

Two caveats bound the finding. No open-access source sets a doctrinal standard for forward holding capacity in large scale combat operations, so the establishment cannot be measured against an external benchmark. And the result depends on the share of DNBI that is disease, which is an informed estimate rather than a measurement (see README [DNBI Sub-Type Split](../README.md#dnbi-sub-type-split)): were that share 30% rather than the modelled 58%, the disease stream's expected occupancy would fall to roughly 8.3 beds and sit inside capacity, so the saturation finding is sensitive to the assumption in direct proportion. README Further Development entry L4 tracks this gap.

## R2E Heavy Handling

<small>[Return to Top](#contents)</small>

**What set the pace at R2E was the second-shift surgical section, not theatre space, and intensive care ran close enough to saturation that a majority of operated casualties received a degraded post-operative pathway.** R2E Heavy is the modelled system's main surgical node, receiving casualties sent straight from R1 and casualties passed on from an R2B whose theatre was off shift, occupied, or short of intensive care. In this run it performed 137 first surgeries and 55 second surgeries. Only a damage control casualty whose abbreviated first operation happened here comes back to theatre for the definitive repair that completes the staged sequence [[11]](#references), which is why the second count is so much smaller: a single-stage casualty needs no second procedure, and a damage control casualty operated on forward at R2B had its first stage elsewhere (see README [Surgical Pathway](../README.md#surgical-pathway)).

![Step plots of R2E Heavy queue length over the run, one panel for the four intensive care beds and one for the two operating theatre beds, the intensive care queue never exceeding one on any single bed and the theatre queue peaking at fourteen on day 28](../images/r2eheavy_bed_queue_3_teams.png)

The two panels share a vertical scale, which makes the difference plain: the theatre queue rose and fell in waves that reached fourteen casualties on day 28 and twelve on day 27, while no single intensive care bed ever held a queue of more than one.

**Surgical throughput was limited by rostered teams as much as by theatre space.** An operation needs both a theatre and one of the three surgical sections that staff them. Theatres are available around the clock, but each section works a 12-hour roster, so at most two operations can run at once during the first shift, when two sections are on, and one during the second, when a single section is on. The three sections were busy for 26.8%, 50.3% and 24.4% of the time their own rosters had them open. The middle figure is the highest because that section covers the second shift and carries the whole night-time surgical load by itself. Measured against the 24-hour day, the two theatres were occupied 53.7% and 37.3% of the time. Operating does not account for that. The 192 procedures performed here, at 123.6 minutes of theatre time per case (registry data for deployed Role 2 and Role 3 facilities, with its band means weighted by its own severity mix [[12]](#references)), use about 27% of the two theatres' combined availability, well under half what the rooms report. The room figure is therefore measuring something other than surgery.

Theatre 1 had a queue for 27.2% of the run and Theatre 2 for 20.2%, while the second-shift section was queued for 41.9% of its open time against 13.7% and 12.7% for the two first-shift sections. A casualty takes a theatre before taking a section, so a room reads as occupied while the casualty in it is still waiting for staff, and most of that theatre queue was therefore a wait for people rather than for rooms. The second-shift section is the specific constraint, carrying about twice the load of either first-shift section and a queue roughly three times longer. For a planner the implication is that a second section rostered to the night shift would buy more throughput than a third theatre.

**Intensive care ran at the edge of saturation.** The four beds were occupied 93.7%, 93.0%, 90.5% and 84.0% of the run, with a queue present 35.1% of the time on the first bed, 7.7% on the second, 3.5% on the third and not at all on the fourth. The pool held a mean of 3.61 of its 4 beds across the run and stood at all four on 29 of the 30 days. The queue sat behind three of the four beds rather than only the first, which is how a pool under sustained load behaves rather than one in overflow; discrete event studies of critical care flow show the same pattern as demand approaches bed capacity without exceeding it [[13]](#references). Casualties waiting for strategic evacuation wait in holding beds, and only the ventilated few in the critical pool occupy an intensive care bed at all, for a limited pre-flight period. Seventeen critical-route evacuees took that path, holding intensive care for 24.9 hours on average and 22.2 at the median, with the longest tenth running to 30.2 hours. Mean and median sit close together because no hold was stretched by a full holding pool: with every scheduled sortie flying, a ventilated casualty steps down as soon as the pre-flight period ends. The pressure on intensive care was therefore clinical demand plus blocked beds (see [Strategic Evacuation and Role 4 Demand](#strategic-evacuation-and-role-4-demand), and README Further Development entry L17).

**The cost of that occupancy fell on the post-operative pathway.** The stabilisation phase belongs to the damage control pathway alone, so it is damage control casualties, not every operated casualty, who pass through the gate before theatre. Seventy-four recovered in intensive care and 57 Priority 1 casualties recovered in a holding bed instead, because intensive care was full when they entered theatre; a further 17 Priority 2 and lower casualties had theatre entry deferred until a bed came free. Care after the definitive operation, which both pathways receive, went to an intensive care bed for 56 casualties and to the holding-bed fallback for 114. Neither pathway produced a post-operative death of wounds in this run, which is what the small per-patient probabilities at that checkpoint and the low counts of the Falklands baseline would lead one to expect; the run's single death of wounds occurred at R2B. The replicated measurement, and the stress test confirming the checkpoint fires, are in [The Post-Operative Intensive Care Gate](Multi_Run_Analysis.md#the-post-operative-intensive-care-gate). This run is not evidence that the holding-bed route is clinically safe; 57 casualties is simply too few to detect a difference in mortality of well under one percent.

The chart below shows which casualties, and on which simulation day, received degraded care as a direct consequence of intensive care saturation.

![Stacked bar chart of R2E surgical casualties per simulation day split across three care pathways, normal intensive care access, sub-optimal holding-bed recovery under a Priority 1 override, and theatre entry delayed pending an intensive care bed, with degraded care present on most days from the first onward](../images/r2e_icu_gating_impact.png)

Two kinds of degraded care appear here. Sub-optimal care means surgery went ahead with intensive care full, a Priority 1 casualty being sent to a holding bed to recover instead. Delayed care means a Priority 2 or lower casualty waited to enter theatre until an intensive care bed came free. Together they accounted for 74 of the 121 gate decisions falling inside the 30-day window, and they appear on most days from the first onward rather than building up late. Within this window, degraded care therefore tracks the surgical load of the day rather than an accumulating backlog, and the worst days for it were the busiest surgical days. Whether that continues to hold over a longer campaign is a separate question this run cannot answer, a 30-day window being too short to distinguish a standing shortfall from the opening phase of a slow accumulation, and no replicated measurement in this project yet settles it. What the replicated evidence does establish about the rationing rule itself, and about why the intensive care queue reads low while its beds run near capacity, is in [Intensive Care Access Is Rationed by Design](Multi_Run_Analysis.md#intensive-care-access-is-rationed-by-design). A planner reading this should note that the gate adds no capacity of its own: it makes the cost of the existing shortfall visible as a clinical pathway rather than hiding it in queue time.

![Gantt chart of R2E Heavy bed occupancy over the run, one horizontal band per bed coloured by bed type, the thirty holding beds filling one after another over the first five days and staying occupied to the end, above them four intensive care beds heavily but intermittently occupied, two operating theatres showing near-continuous occupancy through the first fortnight, and three resuscitation beds marked only by short scattered episodes](../images/r2eheavy_gantt.png)

The holding beds filled one after another and were barely released before the run ended, which shows the strategic evacuation backlog as occupied capacity rather than only as a count: the thirty beds held a mean of 21.1 occupants across the run and stood full on 6 of the 30 days. The theatre bands are the second thing to notice: they run almost unbroken through the first fortnight. That is the occupancy behind the 53.7% and 37.3% room figures above, and most of it was a casualty holding a room while waiting for a surgical section.

![Bar chart of R2E Heavy surgeries completed on each simulation day, varying between one and sixteen with no day carrying none](../images/r2eheavy_surgeries.png)

Daily surgical output varied sixteen-fold across the run, reaching 16 on day 28 and falling to 1 on day 12, so theatre demand at R2E arrived in the same bursts as the casualties generating it. No day passed without an operation.

Taken as a whole, the theatre capacity of two R2B elements and one R2E Heavy was adequate for a single combat brigade at Falklands-equivalent casualty rates [[6]](#references). Applied to a deployed division it would fall well short on both surgery and holding, even assuming only one brigade in contact at a time. This configuration also has mass casualty injection switched off and generates casualties at moderate-intensity rates, so it represents neither a mass casualty event nor the far higher rates recorded for campaigns such as Okinawa or Vietnam [[6]](#references). Either would expose that shortfall, and the replicated experiments covering both are in `docs/Multi_Run_Analysis.md`.

## Casualty Waiting Time

<small>[Return to Top](#contents)</small>

**Waiting time in this run was almost entirely a question of strategic airlift rather than of clinical care.** Waits fell into two distinct groups: most casualties waited essentially no time at all, while a minority waited days or weeks, and that second group was waiting for an evacuation sortie rather than for treatment.

Two thirds of the run's casualties waited essentially no time at all, while 62 waited more than a day and the longest wait reached 15,574 minutes, a little under 11 days.

![Scatter plot of each casualty's total waiting time in minutes against the simulation day of arrival, with a fitted trend line, most points lying on the zero line and a scattered upper band reaching 15,000 minutes that thins out after day 20](../images/waiting_time.png)

The upper band thins out after day 20 simply because a casualty arriving late in the run has less time to accumulate a wait before the run ends, so the apparent improvement is an effect of the 30-day boundary rather than a real recovery. A planner should read the long waits as a movement problem, not a treatment one.

## Transport Fleet Capacity Margin

<small>[Return to Top](#contents)</small>

**Transport was the one echelon with real headroom, but the margin was not untouched.** Average utilisation ran at 14.3% for the PMV Ambulance pool and 4.7% for the HX240M pool, and only the PMV Ambulance pool queued at all.

![Step plots of transport queue length over the run, one panel per pool, the HX240M panel flat at zero throughout and the PMV Ambulance panel showing brief single-casualty spikes on a handful of days](../images/transport_capacity_margin.png)

Neither pool ever held more than one casualty waiting, and the HX240M pool never queued at all. The PMV Ambulance pool stood at one for 0.04% of the run on its busiest vehicle. The load within each pool was uneven, the first PMV Ambulance running at 27.6% against 3.5% for the third, because the selection policy takes the first available vehicle rather than balancing across the fleet. Neither the three-vehicle PMV Ambulance pool nor the four-vehicle HX240M pool came close to binding at Falklands-derived casualty rates, even with the full round-trip model applied, in which a vehicle is held for an empty return leg to its own echelon after dropping a casualty rather than becoming available immediately. That a queue formed at all at 14% average utilisation is what bursty demand does, and it is why the fleet-size sweep should be re-run against the current arrival process (see the README's Further Development entry L19). Where the margin actually ends is a question one run cannot answer, and is taken up by the replicated [Transport Fleet-Size Sweep](Multi_Run_Analysis.md#transport-fleet-size-sweep), which varies the number of vehicles directly rather than the casualty rate or the transport duration.

## Return to Duty

<small>[Return to Top](#contents)</small>

**The modelled system returned 172 casualties to duty over the 30 days, 34.7% of its arrivals, and where that happened says more about the establishment than the rate does.** The returns decompose as follows.

| Echelon   | RTD type       | Count   | Rate (of 495 arrivals) |
| --------- | -------------- | ------- | ---------------------- |
| R1        | battle_fatigue | 44      | 8.9%                   |
| R1        | clinical       | 71      | 14.3%                  |
| R2B       | clinical       | 41      | 8.3%                   |
| R2E       | clinical       | 16      | 3.2%                   |
| **Total** |                | **172** | **34.7%**              |

The battle fatigue count is 44 rather than the 48 generated because 4 casualties were still inside their R1 hold when the run ended. Battle fatigue casualties return to duty only at R1, as the design intends, since they are never routed to Role 2. Most clinical returns happened at R1, where Priority 3 wounded and non-battle injuries finish their recovery, and at R2B, where disease cases are discharged from hold beds. R2E returned 16, because a casualty leaves an R2E hold bed only after completing the recovery period drawn for them, and for most retained casualties that period ran past the 30-day window. The overall rate of 34.7% sits inside the historical range for in-theatre medical facility admissions, 7.6% to 42.1% [[14]](#references), though any direct comparison has to allow for the run's 30-day boundary.

The share of R2E casualties kept in theatre is a different quantity from that overall rate, and it is an output of the theatre evacuation policy rather than a number set in advance (see README [R2E Heavy Trajectory](../README.md#r2e-heavy-trajectory)). Of the 225 casualties reaching an R2E disposition, 14.2% drew an expected recovery short enough to fall inside the shipped 21-day policy and were kept, which is within the historical range. The same drawn duration decides both the disposition and, for those kept, how long they then occupy a holding bed, so evacuation follows severity directly: sorting the 225 dispositions into quartiles by drawn recovery duration gives evacuation rates of 43.9%, 100%, 100% and 100% from shortest to longest, where a draw unrelated to severity would give the same rate in every quartile. The shorter 21-day threshold is what lifts the first quartile's rate above zero: more of even the shortest-recovery casualties now fall beyond it. Because the policy rather than a fixed rate decides who stays, it is a lever a planner can move, and its effect is measured in [Strategic Evacuation and Role 4 Demand](#strategic-evacuation-and-role-4-demand).

## Force Regeneration Feedback Loop

<small>[Return to Top](#contents)</small>

**Both force pools depleted across the run, modestly, and at Falklands-calibrated rates neither reached a level that would change how the health system is sized.** Casualties are generated against the force actually present at the time rather than against a fixed roll strength, so sustained losses pull the effective force down as the campaign proceeds. Reinforcement is enabled in the shipped configuration on a 7-day demand cycle, so the run below is depletion partly offset by fill rather than pure depletion; eight demand cycles for each pool fell inside the 30 days. Whether that depletion shows up as falling daily casualty numbers, and how much of it reinforcement recovers, are questions about a trend in a noisy series and are answered across replications in [Force Regeneration Under Reinforcement](Multi_Run_Supplement.md#force-regeneration-under-reinforcement).

![Line chart of effective combat and support force size against simulation day, each pool's solid curve declining across the 30 days below a dashed line marking its initial establishment strength, the combat curve visibly stepped at heavy arrival days and ending near 2,407 of 2,500, the support curve ending near 1,208 of 1,250](../images/force_regeneration.png)

Both pools shrank, losses outrunning the sum of returns to duty and reinforcement for most of the run, but only slightly. The combat force ended at 2,407 of its initial 2,500 (−3.7%) and the support force at 1,208 of 1,250 (−3.4%). The combat curve is stepped rather than smooth, each step a heavy arrival day, which is the same burst structure that drives every constraint reported above. A planner should read the shallowness of these curves as the reinforcement cycle working rather than as evidence of light casualties: the pool is held near establishment, which is what keeps the population at risk large and the casualty stream flowing at the rates reported above.

## Strategic Evacuation and Role 4 Demand

<small>[Return to Top](#contents)</small>

**Strategic evacuation cleared completely in this run, and the R2E holding beds it would otherwise have occupied stayed free for clinical use.** Of the 495 casualties generated, 193 reached the strategic evacuation decision, and all 193 boarded a sortie and reached Role 4 within the 30 days, none left waiting when the run ended. Each sortie carries the Royal Australian Air Force's published C-17A aeromedical evacuation fit of 36 high dependency and 54 ambulatory places [[15]](#references) (see README [Role 4 (National Support Base) Demand Modelling](../README.md#role-4-national-support-base-demand-modelling)). The shipped sortie cancellation probability is zero, so all four scheduled sorties flew; that setting reflects the model's scope, which is to set the demand the land-based trauma system places on strategic evacuation rather than to simulate the reliability of the airlift meeting it, and what the assumption is worth is measured across replications in [Strategic Airlift Reliability Is Assumed, and the Assumption Is Load-Bearing](Multi_Run_Analysis.md#strategic-airlift-reliability-is-assumed-and-the-assumption-is-load-bearing).

![Stacked bar chart of daily Role 4 bed occupancy split across intensive care, surgical and general wards, flat at zero until day 8, rising to about a hundred and twenty concurrent patients on day 30, then decaying to near zero by day 68, with a dotted line marking the end of the 30-day engagement window](../images/role4_census.png)

Occupancy was zero until day 8, because the first sortie flies on day 7. It then climbed to a peak of 120.0 patients across all wards on day 30, the campaign's last day. The plot continues past the engagement window, marked by the dotted line, because patients stay well beyond the campaign horizon: the census falls back below one only around day 68. That whole tail is demand the national support base carries after the theatre has stopped producing casualties, and a planner sizing the base should note that it peaks after the campaign ends rather than during it. The peak follows the number of evacuation decisions rather than anything about Role 4 itself, which the model treats as unlimited demand rather than as a capacity.

![Two step plots of the number of casualties awaiting a strategic evacuation sortie over the run, one for the critical route and one for the standard route, both rising between sorties and falling to zero each time one flies](../images/ame_backlog.png)

Neither route accumulated across the run. Each builds between sorties and clears when one flies, so the backlog series is a sawtooth returning to zero rather than a rising line.

Splitting the queue into two pools, critical (Priority 1 surgical) and standard, is worth doing because they draw on separate parts of the cabin and fill at different rates. Of the 193 evacuation decisions, 125 went to the critical pool and 68 to the standard pool. Casualties in the critical pool waited 1.03 days on average and those in the standard pool 0.90, the tenth percentile being immediate in both and the ninetieth four days for the critical route and five for the standard.

The sortie timeline shows a schedule comfortably ahead of demand. The four sorties boarded 36, 36, 36 and 17 casualties from the critical pool and 35, 11, 18 and 4 from the standard pool, against 36 and 54 places on each aircraft. The critical cabin filled exactly on the first three, which is the tighter of the two constraints and worth a planner's attention: the critical route ran at capacity on three sorties out of four while the standard cabin never rose above 35 of its 54 places. A theoretical schedule with no constraints, flying same-day and limited only by the airframe's 90 seats, would have needed 29 sorties across the run; the real schedule flew 4 and moved every casualty needing movement.

Because nothing queued, no casualty held an R2E holding bed waiting for a sortie in this run. That is the mechanism behind several figures above: the ventilated pre-flight intensive care holds stayed short, and the R2E holding pool averaged 21.1 of its 30 beds rather than running near capacity. One pool still carries both in-theatre recovery and the strategic evacuation wait, so the finding is that the wait was short here rather than that the interaction has gone; README Further Development entry L17 records it, and [Strategic Airlift Reliability Is Assumed, and the Assumption Is Load-Bearing](Multi_Run_Analysis.md#strategic-airlift-reliability-is-assumed-and-the-assumption-is-load-bearing) shows what happens to the same pool when sorties do not fly.

The wait-time died-of-wounds poll, a periodic mortality check applied to casualties queued for strategic evacuation, recorded no deaths. With every sortie flying, few casualties were exposed to it for long. The chance of death at each poll is deliberately small, so a zero is consistent with the mechanism working rather than evidence about how large its effect is; one run cannot settle a rare event in either direction.

Re-running the same 30-day configuration at seed 42 under the 15-day and 60-day evacuation policies the source gives as realistic alternatives [[16]](#references) shows how steeply the system responds to that lever. The 15-day and 60-day rows come from configurations other than the shipped default and appear here for contrast only; they are not part of the baseline evidence set described in [Methods](#model-and-run-configuration).

| Policy            | In-theatre share | Evacuation decisions | Reached Role 4 | Peak Role 4 occupancy |
| ----------------- | ---------------- | -------------------- | -------------- | --------------------- |
| 15 days           | 8.4%             | 175                  | 174            | 108.0                 |
| 21 days (shipped) | 14.2%            | 193                  | 193            | 120.0                 |
| 60 days           | 79.4%            | 28                   | 11             | 10.0                  |

A longer policy moves casualties off the strategic airlift and onto R2E holding beds, shortening the national support base's queue at the cost of theatre bed-days; a shorter one does the reverse. The response is steep and it is not symmetric about the shipped setting: moving from 21 days to 15 changes the in-theatre share by about half, while moving from 21 to 60 changes it more than fivefold, because the recovery durations the model draws are concentrated well below 60 days. A planner should note that the in-theatre share at the 15-day policy falls to 8.4%, only just inside the 7.6% lower bound of the historical range, while the 60-day setting sits far above its 42.1% upper bound; the shipped 21-day setting sits comfortably within it. With sortie cancellation shipping at zero, all four scheduled sorties fly in every one of the three runs, so the number reaching Role 4 now tracks the number released rather than differing draws, and 174 of 175 and 193 of 193 reach it under the two shorter policies. Under the 60-day policy only 11 of 28 arrive, the releases falling late enough in the campaign to miss the last sortie. One dimension the table does not report is the R2E holding pool, which is where a longer policy's cost is actually paid: the in-theatre share it retains is held in the same beds that carry in-theatre recovery and the evacuation wait. This run's occupancy for that pool is in [R2E Heavy Handling](#r2e-heavy-handling), measured under the shipped policy alone; what a longer policy does to it over a sustained campaign is not measured anywhere in this project, and the table should be read as comparing three policies on airlift and national support base demand rather than on the whole of the trade.

## Limitations

<small>[Return to Top](#contents)</small>

Three kinds of limitation bear on how the findings above should be read.

The first is the unit of analysis, set out in [Scope of a Single Run](#scope-of-a-single-run). One run shows how a mechanism works but cannot estimate an average, so no figure here supports a claim about the size of an effect. Where two figures are set side by side, they describe one run rather than compare two populations.

The second is what the model does not represent. Those gaps are catalogued once, in the README's [Further Development](../README.md#further-development) section, and are not repeated here. The entries bearing most directly on this document are listed below by identifier.

| Entry | Bearing on this document                                                                                                                                                                                                   |
| ----- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| L1    | Casualties enter the model at Role 1, so every waiting time here leaves out the interval between wounding and first contact with the health system, and none can be compared against a doctrinal time-to-surgery standard. |
| L3    | Clinical teams are taken whole, so the surgical section utilisation figures overstate scarcity where a procedure needs only part of a section, and understate it where staff would in practice be shared.                  |
| L4    | The R2B holding shortfall belongs to the shipped establishment rather than to this run, and it depends on a disease DNBI share that is itself uncertain.                                                                   |
| L11   | The theatre and intensive care gating parameters are informed estimates, so the post-operative pathway counts show how the mechanism behaves rather than predicting mortality.                                             |
| L16   | Role 4 is modelled as demand without capacity, so its census signals what would arrive, not that the base could absorb it.                                                                                                 |
| L17   | One pool of R2E holding beds carries both in-theatre recovery and the strategic evacuation wait, so no intensive care or theatre finding here should be read apart from holding-pool occupancy.                            |
| L23   | The severity factors that set recovery duration are uncalibrated, so how steeply the system responds to a change of evacuation policy is uncertain.                                                                        |
| L26   | A single surgery duration distribution serves every casualty whatever their severity, so theatre contention is understated on exactly the heavy, high-acuity days the queue figures come from.                             |

The third belongs to verification itself. Showing that the figures published here are what the current code produces says nothing about whether the model is calibrated to the campaign it represents. That question is tracked separately under Further Development entries L12 and L22, and is addressed, as far as the available historical anchors allow, in the README's [Parameter Calibration](../README.md#parameter-calibration) section.

## Conclusion

<small>[Return to Top](#contents)</small>

**The modelled system sustained a single brigade at Falklands-equivalent casualty rates, and the two constraints it revealed are both matters of establishment rather than of policy.** Role 1 responded quickly and handled its volume, and the two R2B units absorbed surgical demand between them by combining forward surgery with bypass routing to R2E.

**The first constraint is holding bed capacity at R2B.** The ten beds averaged 7.9 occupants and were full or over on 13 days of the run, filled by disease DNBI casualties staying for days at a time. Splitting the load by stream confirms disease DNBI as the dominant one, with about 16.0 beds expected in use from that stream alone against 10 available, a structural gap of 60% that no adjustment to surgical throughput can close. The shortfall showed up only intermittently as a queue because the capacity-aware routing policy sent 176 casualties on to R2E before transport, which displaces the demand rather than absorbing it. The remedies open to a planner are more hold beds, at least 10 per unit, or an evacuation threshold from R2B holding. Theatre capacity was not a constraint at R2B, running at 10.1% to 10.5% of the 24-hour day and 19.9% to 20.2% of rostered shift time.

**The second constraint is the surgical roster at R2E, with intensive care close behind.** The four intensive care beds ran at 84.0% to 93.7% occupancy, held a mean of 3.61 of their 4 beds, and three of the four carried a queue for between 3% and 35% of the run. The three surgical sections were busy for 26.8%, 50.3% and 24.4% of their rostered time, the middle one covering the night-time load alone and queued for 41.9% of it. R2E Heavy performed 137 first surgeries and 55 second surgeries against 77 at R2B, and its two theatres carried a queue for 27.2% and 20.2% of the run, nearly all of it casualties holding a room while they waited for staff. Strategic airlift was not a constraint at all in this run: all four scheduled sorties flew and all 193 casualties needing movement reached Role 4 within the campaign.

Three levers follow for a planner: more R2B hold beds or an evacuation threshold from R2B holding; a second surgical section rostered to the night shift at R2E; and more R2E holding beds, since one pool carries both in-theatre recovery and the evacuation wait, a combination this run leaves untested because nothing waited.

This walk-through describes one campaign under the Falklands-modified baseline, and none of its figures carries an interval. Whether these findings hold more widely, and how the system copes with a much higher casualty rate, is answered in [Comparative Scenario Analysis](Multi_Run_Analysis.md#comparative-scenario-analysis) (`docs/Multi_Run_Analysis.md`), which sets this baseline against an Okinawa-intensity profile across 50 replications with 95% confidence intervals. That document also reports the model's replicated policy experiments: the post-operative intensive care gate, the forward intensive care share frontier, the transport fleet-size sweep and the mass casualty stress test. The reinforcement comparison, which measures force generation rather than health system performance, is reported in `docs/Multi_Run_Supplement.md`.

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
