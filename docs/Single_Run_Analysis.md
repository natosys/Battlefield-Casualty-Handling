# Applying Discrete Event Simulation to the Land-Based Trauma System: Model Verification and Baseline Behaviour

## Abstract

<small>[Return to Top](#contents)</small>

**Background**

High-intensity Large Scale Combat Operations (LSCO) overload forward medical infrastructure, threatening survival from severe combat trauma. Discrete event simulation provides a quantitative method to evaluate system performance and structural interventions across battlefield trauma treatment systems.

**Objective**

To model a land-based trauma system and evaluate its performance in handling historical moderate-intensity warfare casualty rates (based on the 1982 Falklands conflict, modified), enabling planners to identify opportunities for system optimisation.

**Methods**

A discrete event simulation was constructed reflecting an Australian brigade-sized force supported by three Role 1 treatment teams, two Role 2 Basic (R2B) facilities and one Role 2 Enhanced Heavy (R2E Heavy) hospital. The simulation generated casualties over a 30-day period based on distribution rates recorded from the Falklands 1982 conflict, with modified outcomes for modern casualty treatment efficacy, and their handling within the land-based trauma system.

**Results**

The simulation generated 530 casualties across 30 days. Two constraints emerged within the system. Holding beds at the two R2B were heavily utilised and reached capacity on 10 of 30 days, filled by disease casualties. At the R2E Heavy, holding beds were in competition for use by patients waiting for evacuation and those recovering in theatre. Significant queuing was observed for access to surgery in periods with modest casualty spikes, arising because casualties held an operating theatre while waiting for a rostered surgical section rather than because theatre space was scarce, suggesting that surgical capacity would quickly become a constraint at rates above those used in this simulation.

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
| Reinforcement             | Disabled (`force_regeneration.reinforcement.demand_interval_days = 0`) |
| Mass casualty injection   | Disabled (`mass_casualty.event.rate_per_day = 0`)                      |
| Theatre evacuation policy | 30 days                                                                |
| Invocation                | `Rscript run.R --seed 42 --days 30 --iterations 1`                     |

### Scope of a Single Run

This document reports a single 30-day run, undertaken to identify where a representative land-based trauma system might be improved. It traces one campaign end to end, with casualties generated at rates modelled on those observed in the Falklands 1982 conflict and routed through the system by documented handling rules representing operational health policy. In doing so it verifies that the model behaves as its specification describes, which is a separate question from whether that specification fairly represents the real system [[4]](#references), and it shows where load gathers and which parts of the system warrant closer investigation.

One run can do that but no more. Each arrival stream draws its daily rate from a distribution before placing arrivals within the day, so a 30-day run is a single draw from a wide distribution and carries no interval [[5]](#references). Every figure below should be read as an illustration of how a mechanism works rather than as an estimate of an average, and no comparison between two figures here is a test of a hypothesis. Anything this project reports with a confidence interval is in `docs/Multi_Run_Analysis.md`, and the sections below point to it wherever a replicated measurement of the same quantity exists.

### Environment

The run was made in the development container defined in the Battlefield-Casualty-Handling repository, `rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`, so no figure below carries a caveat about the environment that produced it. That run reproduces the repository's tracked baseline evidence set byte for byte, both the console log and every arrival diagnostic, and the reproduction is re-checked whenever the model changes.

---

## Simulation Casualty Generation

<small>[Return to Top](#contents)</small>

**The defining feature of this run was that casualty demand arrived in bursts rather than at a steady rate, and every constraint reported in the sections that follow is a consequence of that.** The run generated 530 casualties over 30 days, ranging from 4 on the lightest day to 46 on the heaviest, so no echelon below can be sized against the average alone. The daily rates each stream draws from come from the Falklands 1982 campaign as tabulated in the FORECAS casualty projection study [[6]](#references), modified as the README's [Parameter Calibration](../README.md#parameter-calibration) section records.

![Three stacked bar charts of daily casualty arrivals over the 30-day run, the same daily totals decomposed first by casualty type (WIA, KIA, DNBI), then by population source (combat, support), then by triage priority (Priority 1 to 3 and KIA)](../images/casualty_summary.png)

The three panels split one arrival series three ways, so their daily totals match. Combat casualties made up most of every peak.

| casualty_type | population_source | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   | 9   | 10  | 11  | 12  | 13  | 14  | 15  | 16  | 17  | 18  | 19  | 20  | 21  | 22  | 23  | 24  | 25  | 26  | 27  | 28  | 29  | 30  | total |
|:------------- |:----------------- | ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| -----:|
| dnbi          | cbt               | 2   | 1   | 8   | 4   | 3   | 5   | 11  | 7   | 6   | 3   | 12  | 1   | 15  | 20  | 6   | 3   | 3   | 0   | 1   | 0   | 7   | 0   | 1   | 4   | 0   | 3   | 3   | 1   | 2   | 6   | 138   |
| dnbi          | spt               | 0   | 1   | 0   | 0   | 2   | 0   | 1   | 2   | 2   | 0   | 0   | 3   | 2   | 4   | 0   | 1   | 3   | 1   | 1   | 0   | 0   | 0   | 0   | 1   | 3   | 2   | 1   | 0   | 3   | 0   | 33    |
| kia           | cbt               | 0   | 3   | 0   | 0   | 2   | 0   | 3   | 2   | 5   | 0   | 5   | 6   | 4   | 2   | 1   | 1   | 0   | 2   | 0   | 9   | 0   | 0   | 0   | 0   | 1   | 1   | 0   | 1   | 0   | 2   | 50    |
| kia           | spt               | 1   | 0   | 3   | 0   | 2   | 0   | 1   | 0   | 1   | 1   | 0   | 0   | 0   | 0   | 0   | 0   | 2   | 0   | 1   | 1   | 0   | 3   | 0   | 0   | 2   | 2   | 1   | 0   | 0   | 1   | 22    |
| wia           | cbt               | 12  | 0   | 6   | 34  | 21  | 1   | 1   | 6   | 15  | 2   | 3   | 0   | 1   | 1   | 3   | 23  | 6   | 10  | 1   | 3   | 0   | 1   | 1   | 16  | 0   | 7   | 3   | 3   | 35  | 3   | 218   |
| wia           | spt               | 2   | 2   | 0   | 8   | 1   | 4   | 2   | 2   | 1   | 0   | 5   | 15  | 0   | 2   | 3   | 0   | 4   | 9   | 0   | 0   | 0   | 1   | 2   | 1   | 2   | 1   | 0   | 1   | 1   | 0   | 69    |
| Total         |                   | 17  | 7   | 17  | 46  | 31  | 10  | 19  | 19  | 30  | 6   | 25  | 25  | 22  | 29  | 13  | 28  | 18  | 22  | 4   | 13  | 7   | 5   | 4   | 22  | 8   | 16  | 8   | 6   | 41  | 12  | 530   |

The table sorts the run's casualties into three categories, Wounded in Action (WIA), Killed in Action (KIA), and Disease and Non-Battle Injury (DNBI), and by whether they came from combat or support forces. The run produced 530 casualties in all. Combat elements accounted for most of them (406), as their greater exposure to risk would suggest. WIA cases totalled 287 and fell mostly on combat personnel (218 against 69), following the force ratios the simulation uses. These casualties usually need care at more than one echelon, including resuscitation, surgery and post-operative holding, and so place steady demand on Role 1 and Role 2. DNBI accounted for 171 cases, a reminder that sickness and non-combat injury persist even in high-intensity operations; historically they rival or exceed battle injury in lost duty days and medical resources consumed [[7]](#references). KIA were fewer, at 72.

That spread was the arrival process working as configured rather than a quirk of this seed. Daily totals averaged 17.7 with a standard deviation of 10.9, and the combat WIA stream alone delivered 35 casualties on day 29 and none at all on four other days. Each stream draws its rate once per simulated day from a distribution whose standard deviation is sourced alongside its mean, then places the arrivals within the day as a Poisson process (see the README's [Casualty Generation](../README.md#casualty-generation)); the typical behaviour across many such draws is pinned down in [Multi-Run Analysis](Multi_Run_Analysis.md).

For a planner this points to treatment capacity that can scale, surgical capability spread across more than one location, and serious attention to preventing disease and non-battle injury, all sized against the peak days rather than the mean.

| population_source | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   | 9   | 10  | 11  | 12  | 13  | 14  | 15  | 16  | 17  | 18  | 19  | 20  | 21  | 22  | 23  | 24  | 25  | 26  | 27  | 28  | 29  | 30  | total |
|:----------------- | ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| -----:|
| cbt               | 14  | 4   | 14  | 38  | 26  | 6   | 15  | 15  | 26  | 5   | 20  | 7   | 20  | 23  | 10  | 27  | 9   | 12  | 2   | 12  | 7   | 1   | 2   | 20  | 1   | 11  | 6   | 5   | 37  | 11  | 406   |
| spt               | 3   | 3   | 3   | 8   | 5   | 4   | 4   | 4   | 4   | 1   | 5   | 18  | 2   | 6   | 3   | 1   | 9   | 10  | 2   | 1   | 0   | 4   | 2   | 2   | 7   | 5   | 2   | 1   | 4   | 1   | 124   |
| Total             | 17  | 7   | 17  | 46  | 31  | 10  | 19  | 19  | 30  | 6   | 25  | 25  | 22  | 29  | 13  | 28  | 18  | 22  | 4   | 13  | 7   | 5   | 4   | 22  | 8   | 16  | 8   | 6   | 41  | 12  | 530   |

The second table splits the casualties by source: combat forces (cbt) and support forces (spt). Of the 530 generated, 406 (about 77%) came from combat elements and 124 (23%) from support units, which follows from the size of each population and the higher rates the combat streams are configured at. Support casualties appear in every period of the run, a reminder that rear-area personnel are exposed too in large scale combat operations, especially under indirect fire, poor situational awareness and disrupted evacuation [[8]](#references). The two populations peak at different times: the support stream's worst day is day 12, at 18 casualties against a combat count of 7, while the combat stream's worst days are 4 and 29. A surge in one is therefore no guide to the other, and rear-area medical coverage cannot be planned as a fixed fraction of forward demand.

Medical coverage therefore has to reach both forward and rear areas. Role 1 teams need to be positioned to reach combat casualties quickly, while Role 2 facilities have to absorb and triage support casualties, who often present differently, with disease, non-battle injury and trauma that has taken time to reach care. Carrying both populations calls for capacity that can scale, evacuation pathways that can be re-routed, and command and control firm enough to keep casualties moving.

| priority_group | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   | 9   | 10  | 11  | 12  | 13  | 14  | 15  | 16  | 17  | 18  | 19  | 20  | 21  | 22  | 23  | 24  | 25  | 26  | 27  | 28  | 29  | 30  | total |
|:-------------- | ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| -----:|
| Priority 1     | 9   | 4   | 9   | 23  | 17  | 5   | 7   | 7   | 18  | 4   | 9   | 12  | 13  | 17  | 9   | 19  | 9   | 16  | 1   | 1   | 4   | 2   | 3   | 13  | 5   | 10  | 3   | 3   | 23  | 5   | 280   |
| Priority 2     | 4   | 0   | 3   | 18  | 5   | 2   | 6   | 4   | 4   | 0   | 7   | 6   | 3   | 8   | 2   | 4   | 3   | 1   | 2   | 2   | 3   | 0   | 1   | 5   | 0   | 2   | 3   | 0   | 11  | 1   | 110   |
| Priority 3     | 3   | 0   | 2   | 5   | 5   | 3   | 2   | 6   | 2   | 1   | 4   | 1   | 2   | 2   | 1   | 4   | 4   | 3   | 0   | 0   | 0   | 0   | 0   | 4   | 0   | 1   | 1   | 2   | 7   | 3   | 68    |
| KIA            | 1   | 3   | 3   | 0   | 4   | 0   | 4   | 2   | 6   | 1   | 5   | 6   | 4   | 2   | 1   | 1   | 2   | 2   | 1   | 10  | 0   | 3   | 0   | 0   | 3   | 3   | 1   | 1   | 0   | 3   | 72    |
| Total          | 17  | 7   | 17  | 46  | 31  | 10  | 19  | 19  | 30  | 6   | 25  | 25  | 22  | 29  | 13  | 28  | 18  | 22  | 4   | 13  | 7   | 5   | 4   | 22  | 8   | 16  | 8   | 6   | 41  | 12  | 530   |

Priority 1 casualties, those needing immediate life-saving treatment, account for 280 of the total (52.8%). That such casualties are the largest group is the reason doctrine places Role 1 assets well forward, able to triage and stabilise quickly. Priority 1 arrivals reach 23 on each of two days against a 30-day average of 9.3, so the high-acuity load is heavy overall and very uneven day to day, which is the pattern that sizes resuscitation and surgical throughput.

Priority 2 and Priority 3 casualties, who need delayed or routine care, accounted for 110 cases (20.8%) and 68 (12.8%). The run also generated 72 KIA (13.6%), who add little to the medical workload but carry considerable operational weight.

This acuity profile carries three implications for how the system is designed:

- Role 1 facilities need to triage and stabilise at volume, and to move Priority 1 casualties rearward quickly.
- Role 2 facilities need bed space and surgical capability they can re-allocate, particularly during sustained operations.
- The evacuation system needs to keep casualties of mixed acuity moving continuously, with clear prioritisation and enough spare capacity to absorb disruption.

## R1 Handling

<small>[Return to Top](#contents)</small>

**Role 1 was not a constraint in this run: every casualty was triaged and treated on arrival, and nothing queued.** Priority 1 casualties were stabilised and moved rearward before their condition could deteriorate, and lower-priority casualties were treated and prepared for movement as required. One caveat bears on that result: the model does not fully represent shortages of evacuation assets, so nothing at Role 1 was held up waiting for transport, and a more detailed evacuation model may yet expose delays this run does not show. Read with that caveat, the three Role 1 teams are the one part of the establishment this run gives a planner no reason to change.

![Step plots of queue length over the 30-day run for each clinical role at each of the three R1 teams, flat at zero in every panel except two brief single-casualty spikes at R1 2 on day 3](../images/r1_queues.png)

No R1 role held a queue at any point in the run except twice on day 3, when one casualty waited briefly for the nursing role at R1 2. The panels use different vertical scales, so those for R1 1 and R1 3 are flat at zero across their whole range.

## R2B Handling

<small>[Return to Top](#contents)</small>

**R2B's binding constraint was holding beds rather than surgery, and what limited its forward surgical capacity was the roster rather than the theatres.** Both findings point at the establishment a planner sets rather than at anything the run did on the day.

![Three stacked bar charts per simulation day at R2B: casualties treated at each of the two R2B stations, surgeries started at each station, and casualties bypassing R2B altogether](../images/r2b_handling.png)

The two stations shared the treatment and surgical load on most days, and neither carried it alone for long. The bypass panel sits at zero or one on most days and jumps on a handful, day 14 and day 22 above all, so bypassing was an occasional response to congestion rather than a steady share of the flow.

Operating theatres are modelled as rooms available around the clock, while the surgical section that staffs them works a 12-hour shift. In this run 210 casualties reached the R2B surgical decision point: 69 were operated on at R2B and 141 were bypassed to R2E. The theatres were in use 9.7% (T1) and 9.6% (T2) of the 24-hour day, and 19.3% and 19.1% of the time their section was rostered on. No queue for a theatre formed at any point, which is the bypass logic working as designed. What limited forward surgery was not the number of casualties reaching the decision point but the single theatre and single rostered section at each unit, which have no way to spread a peak.

**Why casualties bypassed.** A single bypass count hides two different causes, the surgical section being off shift and the theatre being busy or already queued, and the model records which applied to each casualty. Of the 141 bypasses, 100 (71%) happened because the surgical section was off shift and 41 (29%) because the theatre was busy or queued. The 12-hour shift window, not the physical theatre, is therefore what limits forward surgery: for half of every day, a casualty arriving at either R2B unit cannot be operated on there no matter how many beds are free, and goes to R2E instead. Time to surgery is one of the strongest determinants of survival after severe battlefield injury [[9]](#references), so a planner should read this as the loss of forward surgery for half of each day rather than as a low utilisation figure.

**The pre-open hold.** Not every casualty arriving off shift is sent away. One who finds the theatre free and the section due to open within the pre-open window, shipped at 60 minutes, is taken into the theatre and waits there for the section to arrive (see [R2B Trajectory](../README.md#r2b-trajectory)). Seven casualties were held this way, all seven were operated on forward, and they waited 35.2 minutes on average and 58.9 at most. Seven over 30 days is what a 60-minute window should catch out of a 720-minute closed period: the hold reaches back into the last hour before opening and no further. The off-shift bypass count above is what remains after those seven. How much the window moves that count is measured across replications in [Multi-Run Analysis](Multi_Run_Analysis.md#the-r2b-pre-open-hold-window), one run being too coarse to tell the movement apart from sampling variation.

![Two-colour stacked bar chart of R2B operating theatre bypasses on each simulation day, separating bypasses caused by the surgical section being off shift from those caused by the theatre being busy or queued](../images/r2b_ot_bypass_reason.png)

Off-shift bypasses accounted for most of the count on most days. Theatre-busy bypasses appear on about half the days and make up the whole count only on day 5. The shift window is therefore a constant, day-to-day constraint and theatre congestion an occasional one sitting on top of it: no day escaped the first, and the second explained a day's bypasses on its own just once.

Two ways of closing that gap suggest themselves, extending the existing section's shift or fielding a second surgical section at each R2B unit to cover the other half of the day, and neither is evaluated here. Longer shifts cannot be assessed fairly without a model of clinician fatigue and the errors and complications that come with it, which the simulation does not have; reporting the extra throughput without that counterweight would overstate what the change is worth. A second team is an establishment decision and a resourcing question for planners, not something the simulation should test as though it were free. Both are worth a follow-up scenario test once a fatigue model exists or a change to the establishment is directed.

**Holding beds were the main constraint the run identified, and the ten of them ran close to full throughout.** Occupancy climbed from 2 beds on day 1 to 9 or more within four days and stayed there, averaging 7.6 of the 10 beds across the run. It reached ten or more on 10 days, nine or more on 13 of the 30, and went past the establishment to 11 on each of the two heaviest days. What filled these beds was disease DNBI casualties staying for days at a time, most commonly 5, rather than patients recovering from surgery.

A queue did form on the busiest holding beds, reaching five casualties at its peak. That was the routing policy running out of room on the peak days rather than a sign of spare capacity on the others. Before a casualty is moved at all, an upstream check sends them to R2E whenever no R2B unit has hold occupancy below 80%, and it did so 179 times in this run, with three more diverted on arrival and one queued while both echelons were full. The shortfall analysed in the next section is therefore real, but it was mostly exported to R2E as extra medical holding and intensive care load rather than piling up as a queue anyone would see at R2B.

![Step plots of queue length over the run for every bed at each of the two R2B units, showing queues of up to four casualties at R2B 1 and five at R2B 2 on the holding beds, and brief single-casualty queues on the resuscitation beds](../images/r2b_bed_queues.png)

Only holding and resuscitation beds queued at all. The holding-bed queues lasted days at a time, around days 4 to 8 and again from day 14, while the resuscitation queues were brief spikes of one or two casualties.

![Gantt chart of bed occupancy at each of the two R2B units over the run, with one horizontal band per bed coloured by bed type, the five holding beds almost continuously occupied and the operating theatre, intensive care and resuscitation beds showing short scattered episodes](../images/r2b_gantt.png)

The contrast between bed types is what to look at: holding beds carried stays of several days that run together into almost unbroken bands, while theatre, intensive care and resuscitation stays were short enough to appear as isolated marks.

### R2B Hold Bed Saturation: Stream Decomposition and Intervention Analysis

**The hold bed shortfall belongs to the shipped establishment rather than to this run: the configured means imply about 15.5 beds in use against the 10 fielded, a structural overload of 55% that no change to surgical throughput can close.** The run averaged less than that, 7.6 beds, only because the routing policy pushed the excess to R2E instead of letting it build.

The daily occupancy behind that figure can be split by stream, because the model records when each casualty enters the long-stay hold pathway and which stream they came from. Battle fatigue casualties do not appear in it at all: they leave the model at Role 1 and never occupy an R2B hold bed, which the analysis pipeline asserts rather than assumes.

The derivation, under the baseline parameters of 171 DNBI in total (93 disease, 32 NBI, 46 battle fatigue), runs as follows:

- Disease DNBI reaching R2B hold: approximately 74 evacuated (P1: 93 × 0.65 × 0.95 ≈ 57; P2: 93 × 0.20 × 0.90 ≈ 17), less roughly 6% requiring surgery, giving about **70 entering hold-bed recovery** over 30 days (≈ 2.3 per day)
- Non-surgical WIA and NBI reaching R2B hold: approximately 20 over 30 days (≈ 0.7 per day)
- **Total hold entry rate: ≈ 3.0 patients per day**
- Expected hold duration (triangular min = 0.5 d, mode = 5 d, max = 10 d): mean = (0.5 + 5 + 10) / 3 = **5.17 days**
- **Expected concurrent hold occupancy: 3.0 × 5.17 ≈ 15.5 beds** against 10 available (5 per R2B unit × 2 units)

![Stacked bar chart of mean concurrent R2B hold bed occupancy on each simulation day, each bar decomposed into the disease DNBI, non-battle injury and wounded in action streams, with a dashed reference line at the five-bed per-unit capacity and bars reaching eleven on two days](../images/r2b_hold_occupancy.png)

Disease DNBI was the largest stream on all but a few days and is what pushed occupancy past the ten-bed establishment. The dashed line marks the five beds a single R2B unit holds, so any bar above it is a load neither unit could carry alone. The wounded in action stream ran between one and five beds and disappeared over the closing days, so relieving the hold-bed constraint is a question of managing disease rather than of surgical throughput. Disease dominating a forward holding pathway matches the campaign record, in which sickness has repeatedly produced admission loads as large as or larger than wounding [[10]](#references).

**Three remedies are available to a planner, and the arithmetic above rules one of them out.** Shortening the hold is the weakest: moving the most common stay from 5 days to 3 brings the expected duration down to 4.5 days and expected occupancy to 13.5 beds, still 35% over the 10 available, and fitting inside capacity would need a most-common stay of 1.6 days or less, which is not clinically plausible. Adding beds works but is expensive: 10 per unit gives 20 against expected demand of about 15.5, enough headroom to absorb the day-to-day swings the arrival process produces, whereas 8 per unit sits barely above expected demand and leaves no margin at all. An evacuation threshold is the cheapest and the most consequential: sending on any casualty whose drawn recovery exceeds three days moves roughly 85% of hold patients forward to R2E and all but removes the R2B saturation, at the price of transferring a non-surgical medical load onto R2E holding and intensive care. What transfers is the unserved remainder of each casualty's own recovery rather than a fresh duration drawn on arrival (see README [R2B Trajectory](../README.md#r2b-trajectory)), so the change moves a fixed quantity of bed time between echelons rather than creating more of it. Each of the three is a configuration change a planner can make and test across replications; none is evaluated here, one run being too coarse to separate the effect from sampling variation.

**A fourth response is already running in the shipped model, which is why the shortfall shows up as transferred load rather than as a backlog.** Hold beds are allocated by a two-tier capacity-aware policy. The first tier acts at Role 1, before transport begins: a casualty is sent to an R2B unit only while that unit's hold occupancy is below 80% of capacity, so with 5 beds per unit at least one is kept free for casualties staging through, and where neither unit qualifies the casualty goes straight from R1 to R2E and is never transported to R2B at all. Deciding before transport rather than after the casualty has taken a bed is what stops long-stay patients from crowding out new arrivals.

The second tier acts at R2B itself, on casualties who arrive because the upstream check passed or because occupancy changed in the meantime. A free hold bed is taken at once. Where hold is full and R2E has room, the casualty moves on to R2E by evacuation-team transport. Where both echelons are full, the casualty joins a short R2B hold queue, capped at two, beyond which they move on to R2E instead. This run recorded 179 upstream diversions at R1, three diversions on arrival at R2B, and one casualty queued while both echelons were full.

Two caveats bound the finding. No open-access source sets a doctrinal standard for forward holding capacity in large scale combat operations, so the establishment cannot be measured against an external benchmark. And the result depends on the share of DNBI that is disease, which is an informed estimate rather than a measurement (see README [DNBI Sub-Type Split](../README.md#dnbi-sub-type-split)): were that share 30% rather than the modelled figure, expected occupancy would fall to roughly 9 beds and sit inside capacity, so the saturation finding is sensitive to the assumption in direct proportion. README Further Development entry L4 tracks this gap.

## R2E Heavy Handling

<small>[Return to Top](#contents)</small>

**What set the pace at R2E was the second-shift surgical section, not theatre space, and intensive care ran close enough to saturation that a majority of operated casualties received a degraded post-operative pathway.** R2E Heavy is the modelled system's main surgical node, receiving casualties sent straight from R1 and casualties passed on from an R2B whose theatre was off shift, occupied, or short of intensive care. In this run it performed 171 first surgeries and 41 second surgeries. Only a damage control casualty whose abbreviated first operation happened here comes back to theatre for the definitive repair that completes the staged sequence [[11]](#references), which is why the second count is so much smaller: a single-stage casualty needs no second procedure, and a damage control casualty operated on forward at R2B had its first stage elsewhere (see README [Surgical Pathway](../README.md#surgical-pathway)).

![Step plots of R2E Heavy queue length over the run, one panel for the four intensive care beds and one for the two operating theatre beds, the intensive care queue never exceeding one and the theatre queue peaking at eight around day 9](../images/r2eheavy_bed_queue_3_teams.png)

The two panels share a vertical scale, which makes the difference plain: the theatre queue rose and fell in waves that reached eight casualties on day 9 and six on day 24, while the intensive care queue never went above one.

**Surgical throughput was limited by rostered teams as much as by theatre space.** An operation needs both a theatre and one of the three surgical sections that staff them. Theatres are available around the clock, but each section works a 12-hour roster, so at most two operations can run at once during the first shift, when two sections are on, and one during the second, when a single section is on. The three sections were busy for 30.8%, 53.6% and 30.8% of the time their own rosters had them open. The middle figure is the highest because that section covers the second shift and carries the whole night-time surgical load by itself. Measured against the 24-hour day, the two theatres were occupied 66.6% and 52.8% of the time. Operating does not account for that. The 212 procedures performed here, at 123.6 minutes of theatre time per case (registry data for deployed Role 2 and Role 3 facilities, with its band means weighted by its own severity mix [[12]](#references)), use about 30% of the two theatres' combined availability, roughly half what the rooms report. The room figure is therefore measuring something other than surgery.

Theatre 1 had a queue for 46.3% of the run and Theatre 2 for 34.6%, while the second-shift section was queued for 2.5% of its open time against 0.7% and 0.6% for the two first-shift sections. A casualty takes a theatre before taking a section, so a room reads as occupied while the casualty in it is still waiting for staff, and most of that theatre queue was therefore a wait for people rather than for rooms. The second-shift section is the specific constraint, carrying about three quarters again the load of either first-shift section and a queue several times longer. For a planner the implication is that a second section rostered to the night shift would buy more throughput than a third theatre.

**Intensive care was busy but not saturated.** The four beds were occupied 93.7%, 92.1%, 92.2% and 85.8% of the run, with a queue present 26.5% of the time on the first bed, 13.1% on the second, 13.5% on the third and 7.8% on the fourth. The queue was spread across all four beds rather than sitting behind the first, which is how a pool under sustained load behaves rather than one in overflow; discrete event studies of critical care flow show the same pattern as demand approaches bed capacity without exceeding it [[13]](#references). Casualties waiting for strategic evacuation wait in holding beds, and only the ventilated few in the critical pool occupy an intensive care bed at all, for a limited pre-flight period. Eight critical-route evacuees took that path and four finished it within the run, waiting 109.4 hours on average, 102.8 at the median and 147 at the 90th percentile. Those holds ran long because a ventilated casualty cannot step down while the holding pool is full, and two of the run's four scheduled sorties were cancelled, so the pool stayed full for weeks at a time. The pressure on intensive care was therefore clinical demand plus blocked beds (see [Strategic Evacuation and Role 4 Demand](#strategic-evacuation-and-role-4-demand), and README Further Development entry L17).

**The cost of that occupancy fell on the post-operative pathway.** The stabilisation phase belongs to the damage control pathway alone, so it is damage control casualties, not every operated casualty, who pass through the gate before theatre. Seventy-nine recovered in intensive care and 58 Priority 1 casualties recovered in a holding bed instead, because intensive care was full when they entered theatre; a further 29 Priority 2 and lower casualties had theatre entry deferred until a bed came free. Care after the definitive operation, which both pathways receive, went to an intensive care bed for 54 casualties and to the holding-bed fallback for 130. Neither pathway produced a post-operative death of wounds in this run, which is what the small per-patient probabilities at that checkpoint and the low counts of the Falklands baseline would lead one to expect. The replicated measurement, and the stress test confirming the checkpoint fires, are in [The Post-Operative Intensive Care Gate](Multi_Run_Analysis.md#the-post-operative-intensive-care-gate). This run is not evidence that the holding-bed route is clinically safe; 58 casualties is simply too few to detect a difference in mortality of well under one percent.

The chart below shows which casualties, and on which simulation day, received degraded care as a direct consequence of intensive care saturation.

![Stacked bar chart of R2E surgical casualties per simulation day split across three care pathways, normal intensive care access, sub-optimal holding-bed recovery under a Priority 1 override, and theatre entry delayed pending an intensive care bed, with degraded care present on most days from the first onward](../images/r2e_icu_gating_impact.png)

Two kinds of degraded care appear here. Sub-optimal care means surgery went ahead with intensive care full, a Priority 1 casualty being sent to a holding bed to recover instead. Delayed care means a Priority 2 or lower casualty waited to enter theatre until an intensive care bed came free. Together they accounted for 87 of the 137 R2E surgical casualties, and they appear on most days from the first onward rather than building up late. Intensive care saturation is therefore a standing condition of the four-bed establishment rather than something the campaign works its way into, and the worst days for degraded care were the busiest surgical days. A planner reading this should note that the gate adds no capacity of its own: it makes the cost of the existing shortfall visible as a clinical pathway rather than hiding it in queue time.

![Gantt chart of R2E Heavy bed occupancy over the run, one horizontal band per bed coloured by bed type, the thirty holding beds filling one after another over the first five days and staying occupied to the end, above them four intensive care beds heavily but intermittently occupied, two operating theatres showing near-continuous occupancy through the first fortnight, and three resuscitation beds marked only by short scattered episodes](../images/r2eheavy_gantt.png)

The holding beds filled one after another and were barely released before the run ended, which shows the strategic evacuation backlog as occupied capacity rather than only as a count. The theatre bands are the second thing to notice: they run almost unbroken through the first fortnight. That is the occupancy behind the 66.6% and 52.8% room figures above, and most of it was a casualty holding a room while waiting for a surgical section.

![Bar chart of R2E Heavy surgeries completed on each simulation day, varying between four and twenty-five with two days carrying none](../images/r2eheavy_surgeries.png)

Daily surgical output varied six-fold across the run and reached 25 on day 25, so theatre demand at R2E arrived in the same bursts as the casualties generating it.

Taken as a whole, the theatre capacity of two R2B elements and one R2E Heavy was adequate for a single combat brigade at Falklands-equivalent casualty rates [[6]](#references). Applied to a deployed division it would fall well short on both surgery and holding, even assuming only one brigade in contact at a time. This configuration also has mass casualty injection switched off and generates casualties at moderate-intensity rates, so it represents neither a mass casualty event nor the far higher rates recorded for campaigns such as Okinawa or Vietnam [[6]](#references). Either would expose that shortfall, and the replicated experiments covering both are in `docs/Multi_Run_Analysis.md`.

## Casualty Waiting Time

<small>[Return to Top](#contents)</small>

**Waiting time in this run was almost entirely a question of strategic airlift rather than of clinical care.** Waits fell into two distinct groups: most casualties waited essentially no time at all, while a minority waited days or weeks, and that second group was waiting for an evacuation sortie rather than for treatment.

![Scatter plot of each casualty's total waiting time in minutes against the simulation day of arrival, with a fitted trend line, most points lying on the zero line and a scattered upper band reaching 33,000 minutes that thins out after day 20](../images/waiting_time.png)

The upper band thins out after day 20 simply because a casualty arriving late in the run has less time to accumulate a wait before the run ends, so the apparent improvement is an effect of the 30-day boundary rather than a real recovery. A planner should read the long waits as a movement problem, not a treatment one.

## Transport Fleet Capacity Margin

<small>[Return to Top](#contents)</small>

**Transport was the one echelon with real headroom, but the margin was not untouched.** Average utilisation ran at 14.9% for the PMV Ambulance pool and 5.1% for the HX240M pool, yet the PMV Ambulance pool still queued on the peak days.

![Step plots of transport queue length over the run, one panel per pool, the HX240M panel flat at zero throughout and the PMV Ambulance panel showing brief single-casualty spikes on a handful of days with one spike reaching two around day 4](../images/transport_capacity_margin.png)

The HX240M queue stayed at zero all run. The PMV Ambulance pool reached two casualties once and stood at one or more for 1.4% of the run on its busiest vehicle. Neither the three-vehicle PMV Ambulance pool nor the four-vehicle HX240M pool came close to binding at Falklands-derived casualty rates, even with the full round-trip model applied, in which a vehicle is held for an empty return leg to its own echelon after dropping a casualty rather than becoming available immediately. That a queue formed at all at 15% average utilisation is what bursty demand does, and it is why the fleet-size sweep should be re-run against the current arrival process (see the README's Further Development entry L19). Where the margin actually ends is a question one run cannot answer, and is taken up by the replicated [Transport Fleet-Size Sweep](Multi_Run_Analysis.md#transport-fleet-size-sweep), which varies the number of vehicles directly rather than the casualty rate or the transport duration.

## Return to Duty

<small>[Return to Top](#contents)</small>

**The modelled system returned 167 casualties to duty over the 30 days, 31.5% of its arrivals, and where that happened says more about the establishment than the rate does.** The returns decompose as follows.

| Echelon   | RTD type       | Count   | Rate (of 530 arrivals) |
| --------- | -------------- | ------- | ---------------------- |
| R1        | battle_fatigue | 44      | 8.3%                   |
| R1        | clinical       | 78      | 14.7%                  |
| R2B       | clinical       | 42      | 7.9%                   |
| R2E       | clinical       | 3       | 0.6%                   |
| **Total** |                | **167** | **31.5%**              |

The battle fatigue count is 44 rather than the 46 generated because 2 casualties were still inside their R1 hold when the run ended. Battle fatigue casualties return to duty only at R1, as the design intends, since they are never routed to Role 2. Most clinical returns happened at R1, where Priority 3 wounded and non-battle injuries finish their recovery, and at R2B, where disease cases are discharged from hold beds. R2E returned just 3, because a casualty leaves an R2E hold bed only after completing the recovery period drawn for them, and for most retained casualties that period ran past the 30-day window. The overall rate of 31.5% sits inside the historical range for in-theatre medical facility admissions, 7.6% to 42.1% [[14]](#references), though any direct comparison has to allow for the run's 30-day boundary.

The share of R2E casualties kept in theatre is a different quantity from that overall rate, and it is an output of the theatre evacuation policy rather than a number set in advance (see README [R2E Heavy Trajectory](../README.md#r2e-heavy-trajectory)). Of the 176 casualties reaching an R2E disposition, 23.3% drew an expected recovery short enough to fall inside the shipped 30-day policy and were kept, which is within the historical range. The same drawn duration decides both the disposition and, for those kept, how long they then occupy a holding bed, so evacuation follows severity directly: sorting the 176 dispositions into quartiles by drawn recovery duration gives evacuation rates of 6.8%, 100%, 100% and 100% from shortest to longest, where a draw unrelated to severity would give the same rate in every quartile. Because the policy rather than a fixed rate decides who stays, it is a lever a planner can move, and its effect is measured in [Strategic Evacuation and Role 4 Demand](#strategic-evacuation-and-role-4-demand).

## Force Regeneration Feedback Loop

<small>[Return to Top](#contents)</small>

**Both force pools depleted across the run, modestly, and at Falklands-calibrated rates neither reached a level that would change how the health system is sized.** Casualties are generated against the force actually present at the time rather than against a fixed roll strength, so sustained losses pull the effective force down as the campaign proceeds. Reinforcement ships switched off in the shipped configuration, making the run below a case of pure depletion. Whether that depletion shows up as falling daily casualty numbers, and whether reinforcement halts it, are questions about a trend in a noisy series and are answered across replications in [Force Regeneration Under Reinforcement](Multi_Run_Analysis.md#force-regeneration-under-reinforcement).

![Line chart of effective combat and support force size against simulation day, each pool's solid curve declining across the 30 days below a dashed line marking its initial establishment strength, the combat curve visibly stepped at heavy arrival days and ending near 2,225 of 2,500, the support curve shallower and ending near 1,162 of 1,250](../images/force_regeneration.png)

Both pools shrank, losses outrunning returns to duty for most of the run. The combat force ended at 2,225 of its initial 2,500 (−11.0%) and the support force at 1,162 of 1,250 (−7.0%). The combat curve is stepped rather than smooth, each step a heavy arrival day, which is the same burst structure that drives every constraint reported above.

## Strategic Evacuation and Role 4 Demand

<small>[Return to Top](#contents)</small>

**What bound strategic evacuation in this run was how many sorties actually departed, not how many seats each carried, and every casualty left waiting held an R2E bed while they waited.** Of the 530 casualties generated, 135 reached the strategic evacuation decision. Of those, 99 boarded a sortie and reached Role 4 within the 30 days, while 36 were still waiting, and still occupying an R2E holding bed, when the run ended. Each sortie carries the Royal Australian Air Force's published C-17A aeromedical evacuation fit of 36 high dependency and 54 ambulatory places [[15]](#references) (see README [Role 4 (National Support Base) Demand Modelling](../README.md#role-4-national-support-base-demand-modelling)), so what left those 36 behind was which sorties flew, not how many seats each carried.

![Stacked bar chart of daily Role 4 bed occupancy split across intensive care, surgical and general wards, flat at zero until day 21, rising to about ninety concurrent patients on days 29 and 30, then decaying to near zero by day 69, with a dotted line marking the end of the 30-day engagement window](../images/role4_census.png)

Occupancy was zero until day 21, because no sortie flew before then. It then climbed to a peak of 90.0 patients across all wards on day 30, the campaign's last day. The plot continues past the engagement window, marked by the dotted line, because patients stay well beyond the campaign horizon: the census falls back to near zero only around day 69. That whole tail is demand the national support base carries after the theatre has stopped producing casualties, and a planner sizing the base should note that it peaks after the campaign ends rather than during it. The peak follows the number of evacuation decisions rather than anything about Role 4 itself, which the model treats as unlimited demand rather than as a capacity.

![Two step plots of the number of casualties awaiting a strategic evacuation sortie over the run, one for the critical route and one for the standard route, both rising steadily to a peak around day 21 and falling sharply when a sortie flies before climbing again](../images/ame_backlog.png)

Both routes built up for three weeks before the first sortie flew, and the two sorties that did fly cleared neither. The critical backlog reached about 35 before the day-21 departure, dropped to 12, climbed back above 40 by day 28 and ended the run at 24. The standard route followed the same shape and ended at 12.

Splitting the queue into two pools, critical (Priority 1 surgical) and standard, is worth doing because they draw on separate parts of the cabin and fill at different rates. Of the 135 evacuation decisions, 85 went to the critical pool and 50 to the standard pool. Two of the four scheduled opportunities drew a cancellation, so the first sortie to fly left on day 21. In the critical pool, 61 of 85 had boarded by day 30, leaving 24 waiting an average of 10.1 days; in the standard pool, 38 of 50 had boarded, leaving 12 waiting the same average time. This run is the document's clearest illustration of why a schedule with spare capacity on average is not the same as a schedule that clears its backlog: two cancellations against an effective fortnightly interval left three weeks of demand sitting in R2E holding beds.

The sortie timeline shows a schedule with no way to recover from a cancellation. The two sorties that flew boarded 36 and 25 casualties from the critical pool and 33 and 5 from the standard pool, against 36 and 54 places on each aircraft. The day-21 sortie filled the critical cabin exactly and still left a queue behind. A theoretical schedule with no constraints, flying same-day and limited only by the airframe's 90 seats, would have needed 27 sorties across the run; the real schedule flew 2 and moved 99 of the 135 casualties needing movement. The lever a planner holds here is therefore schedule resilience, a reserve airframe or a shorter nominal interval, rather than cabin fit.

A casualty waiting for a sortie holds an R2E holding bed for the whole wait, so the 36 still queued at the run's end were occupying clinical capacity as well as sitting in a backlog count. The effect runs both ways at the shipped establishment: a ventilated critical-route casualty cannot step down from pre-flight intensive care while the holding pool is full, which is why the ventilated holds reported in [R2E Heavy Handling](#r2e-heavy-handling) ran as long as they did. One pool therefore carries in-theatre recovery and the strategic evacuation wait at the same time, and no intensive care or theatre finding in this document should be read apart from it. README Further Development entry L17 records the consequence and the replicated occupancy measurements behind it.

The wait-time died-of-wounds poll, a periodic mortality check applied to casualties queued for strategic evacuation, ran correctly against this backlog but recorded no deaths. The chance of death at each poll is deliberately small, so a zero is consistent with the mechanism working rather than evidence about how large its effect is; one run cannot settle a rare event in either direction.

Re-running the same 30-day configuration at seed 42 under the 15-day and 60-day evacuation policies the source gives as realistic alternatives [[16]](#references) shows how steeply the system responds to that lever. The 15-day and 60-day rows come from configurations other than the shipped default and appear here for contrast only; they are not part of the baseline evidence set described in [Methods](#model-and-run-configuration).

| Policy            | In-theatre share | Evacuation decisions | Reached Role 4 | Peak Role 4 occupancy |
| ----------------- | ---------------- | -------------------- | -------------- | --------------------- |
| 15 days           | 3.0%             | 191                  | 182            | 122.0                 |
| 30 days (shipped) | 23.3%            | 135                  | 99             | 90.0                  |
| 60 days           | 73.8%            | 34                   | 13             | 13.0                  |

A longer policy moves casualties off the strategic airlift and onto R2E holding beds, shortening the national support base's queue at the cost of theatre bed-days; a shorter one does the reverse. The response is steep, and a planner should note that the in-theatre share at the 15-day policy falls to 3.0%, below the 7.6% lower bound of the historical range, which puts the model outside its validated envelope at that end. Under the 15-day policy 182 of the 191 casualties it releases reached Role 4 within the run, against 99 of 135 under the shipped policy. That gap comes not from the policy but from which sorties flew, two of four having been cancelled in the shipped-policy run, and it is a reminder that one run's cancellation draws affect this comparison more than the policy does.

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

**The first constraint is holding bed capacity at R2B.** The ten beds averaged 7.6 occupants and were full or over on 10 days of the run, filled by disease DNBI casualties staying for days at a time. Splitting the load by stream confirms disease DNBI as the dominant one, with about 15.5 beds expected in use against 10 available, a structural gap of 55% that no adjustment to surgical throughput can close. The shortfall showed up only intermittently as a queue because the capacity-aware routing policy sent 179 casualties on to R2E before transport, which displaces the demand rather than absorbing it. The remedies open to a planner are more hold beds, at least 10 per unit, or an evacuation threshold from R2B holding. Theatre capacity was not a constraint at R2B, running at 9.6% to 9.7% of the 24-hour day and 19.1% to 19.3% of rostered shift time.

**The second constraint is the surgical roster at R2E, with intensive care close behind.** The four intensive care beds ran at 85.8% to 93.7% occupancy and each carried a queue for between 8% and 27% of the run. The three surgical sections were busy for 30.8%, 53.6% and 30.8% of their rostered time, the middle one covering the night-time load alone. R2E Heavy performed 171 first surgeries and 41 second surgeries against 69 at R2B, and its two theatres carried a queue for 46.3% and 34.6% of the run, nearly all of it casualties holding a room while they waited for staff. Strategic airlift was limited by cancelled sorties rather than by seats: two of four scheduled sorties did not fly, and 36 casualties were still waiting when the run ended.

Three levers follow for a planner: more R2B hold beds or an evacuation threshold from R2B holding; a second surgical section rostered to the night shift at R2E; and more R2E holding beds, since one pool currently carries both in-theatre recovery and the evacuation wait.

This walk-through describes one campaign under the Falklands-modified baseline, and none of its figures carries an interval. Whether these findings hold more widely, and how the system copes with a much higher casualty rate, is answered in [Comparative Scenario Analysis](Multi_Run_Analysis.md#comparative-scenario-analysis) (`docs/Multi_Run_Analysis.md`), which sets this baseline against an Okinawa-intensity profile across 50 replications with 95% confidence intervals. That document also reports the model's replicated policy experiments: the post-operative intensive care gate, the forward intensive care share frontier, the transport fleet-size sweep, the reinforcement comparison and the mass casualty stress test.

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
