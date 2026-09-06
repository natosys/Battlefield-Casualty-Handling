# Applying Discrete Event Simulation to the Land-Based Trauma System: Planning Options for Surge Capacity and System Transformation

## Abstract

<small>[Return to Top](#contents)</small>

**Background**

Casualty estimation is critical to the planning and design of the land-based trauma system deployed to support operations. Recent wars and conflicts of choice have produced dramatically lower casualty rates than those previously observed, or those expected in future large scale combat operations [[1]](#references), so a health system sized against recent experience may not carry into a peer fight [[2]](#references).

**Objective**

To identify options to improve the land-based trauma system, and to establish where that system fails first as casualty intensity rises.

**Methods**

A discrete event simulation of a brigade-level trauma system was run at two casualty intensities, 50 replications of a 30-day campaign each. Both take their casualty rates from the FORECAS projection study [[3]](#references), a moderate profile from the Falklands 1982 campaign and a high profile from Okinawa 1945, each carrying the died-of-wounds experience of its own campaign [[4]](#references). The health system is identical across the two, isolating casualty intensity. Six further experiments test individual design levers at 10 to 50 replications, and every option reported carries a label stating what the evidence establishes about it.

**Results**

Casualty volume rises 2.33-fold between the two intensities, while the operating theatre queue at the Role 2 Enhanced hospital rises about 36-fold, forward holding beds about 5.5-fold, rearward holding beds about 4.5-fold and intensive care about 4.3-fold. The system's response to load is not proportional to the load placed on it. Surgical team scheduling, not theatre space, is the major constraint: a casualty occupies a theatre while waiting for a rostered surgical team. The forward theatre queue reads zero at both intensities only because casualties are diverted rearward instead of held, concealing the forward shortfall from any measure based on waiting. Transport holds margin at both intensities.

**Conclusion**

Surgical team coverage is the change the evidence points at most directly. Forward facilities lose surgery for half of each day to a 12-hour roster, and the rearward hospital's night shift carries the whole surgical load with a single team. Both would be answered by extended hours or additional teams giving 24-hour coverage, and neither can be costed until the model represents clinician fatigue. A reinforcement demand cycle is the one lever both measured and effective, forward holding capacity needs relief at either intensity, and moving post-operative intensive care forward pays nothing measurable.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Introduction](#introduction)
- [Methods](#methods)
  - [The Model](#the-model)
  - [Design and Unit of Analysis](#design-and-unit-of-analysis)
  - [Replication Independence](#replication-independence)
  - [Confidence Intervals](#confidence-intervals)
  - [Replication Count and Resolution](#replication-count-and-resolution)
  - [Reading the Evidence Labels](#reading-the-evidence-labels)
  - [Casualty Intensities Compared](#casualty-intensities-compared)
- [Where the Trauma System Fails First](#where-the-trauma-system-fails-first)
  - [Comparative Scenario Analysis](#comparative-scenario-analysis)
  - [The Binding Constraint: Surgical Team Scheduling](#the-binding-constraint-surgical-team-scheduling)
- [Planning Options in Priority Order](#planning-options-in-priority-order)
  - [Option 1. Extend Surgical Team Coverage](#option-1-extend-surgical-team-coverage)
  - [Option 2. Relieve Forward Holding Capacity](#option-2-relieve-forward-holding-capacity)
  - [Option 3. Hold Casualties Forward for a Reopening Team](#option-3-hold-casualties-forward-for-a-reopening-team)
    - [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)
  - [Option 4. Sustain the Force with a Reinforcement Demand Cycle](#option-4-sustain-the-force-with-a-reinforcement-demand-cycle)
    - [Force Regeneration Under Reinforcement](#force-regeneration-under-reinforcement)
  - [Option 5. Take Risk on the Transport Fleet](#option-5-take-risk-on-the-transport-fleet)
    - [Transport Fleet-Size Sweep](#transport-fleet-size-sweep)
  - [An Option That Does Not Pay: Moving Post-Operative Intensive Care Forward](#an-option-that-does-not-pay-moving-post-operative-intensive-care-forward)
    - [Forward ICU Share Decision Frontier](#forward-icu-share-decision-frontier)
- [What Resourcing Alone Cannot Fix](#what-resourcing-alone-cannot-fix)
  - [Rearward Diversion Hides the Forward Shortfall](#rearward-diversion-hides-the-forward-shortfall)
  - [The Post-Operative Intensive Care Gate](#the-post-operative-intensive-care-gate)
  - [Mass Casualty Event Stress Test](#mass-casualty-event-stress-test)
- [Demand on the National Support Base](#demand-on-the-national-support-base)
- [Evidence That Does Not Yet Support a Decision](#evidence-that-does-not-yet-support-a-decision)
- [Research and Development Agenda](#research-and-development-agenda)
- [Limitations](#limitations)
- [Conclusion](#conclusion)
- [References](#references)
<!-- TOC END -->

---

## Introduction

<small>[Return to Top](#contents)</small>

Casualty estimation drives the design of a deployed health system, and the system has to be sized before the campaign it will serve. A planner sizing one makes three decisions. Whether the establishment as it stands carries the casualty load expected of it, and if not, where it gives way first. Which of the changes available buys the most improvement for the force. And which candidate changes the available evidence cannot yet separate from chance.

The third decision is usually left implicit, and this paper makes it explicit, because the casualty volumes expected in large scale combat operations exceed those the deployed health systems of the past two decades were built around [[1]](#references). Planning assumptions carried forward from those operations understate both the volume and the acuity a peer fight would produce [[2]](#references). A planner working from the last campaign's establishment is working from a baseline that may not transfer.

This paper is presented in five parts. The first locates where the modelled trauma system fails as casualty intensity rises. The second sets out five planning options in priority order, each labelled by what the evidence establishes about it. The third covers behaviour that persists whatever the establishment is set to. The fourth states the effects the evidence cannot yet resolve and what it would take to settle them. The fifth sets a research agenda for the options the model cannot presently evaluate. A companion paper reports the verification of this model and the detailed behaviour of a single campaign [[5]](#references); a supplementary document holds the full experimental designs and the statistical derivations summarised here [[6]](#references).

---

## Methods

<small>[Return to Top](#contents)</small>

### The Model

The simulation is a discrete event model in which each casualty is an entity that arrives, then claims and releases clinical staff, beds, operating theatres and transport as it moves rearward through the echelons of allied medical support doctrine [[7]](#references). Those echelons are Role 1 for primary care and resuscitation forward, Role 2 Basic (R2B) for damage control surgery and short-term holding, and Role 2 Enhanced Heavy (R2E) for definitive surgery, intensive care and in-theatre recovery, with strategic aeromedical evacuation to a Role 4 national support base beyond them. The modelled establishment is a representative combat brigade served by three Role 1 treatment teams, two R2B facilities and one R2E hospital. The model is built on the `simmer` package for R [[8]](#references) and all results were produced under R 4.4.2.

### Design and Unit of Analysis

The replication is the unit of analysis [[9]](#references). Each replication is one complete 30-day campaign run from an empty system, and every response is reduced to a single number for that campaign before any statistic is taken across campaigns: for a resource, the time-weighted mean queue length; for a count, the count. Treating casualties or simulated days as the unit instead would take observations from inside one campaign as though they were separate campaigns, and would report intervals several times narrower than the evidence supports.

Each campaign is analysed in full, from its first day. Removing an opening warm-up period is worth doing only when steady-state behaviour is the quantity of interest [[10]](#references), and a deployed health system genuinely starts empty, so the opening period is part of what a planner needs to see.

### Replication Independence

Independence between replications matters because every confidence interval reported here divides a standard deviation by the square root of the replication count, which is valid only for independent replications. Two properties of the framework establish it. Each campaign is reproducible from its seed alone, and each campaign is given a distinct seed, so nothing but the model connects any two of them. Replications run in parallel draw on separate random number substreams, which preserves both properties [[11]](#references)[[12]](#references). One exception is flagged where it appears, in the post-operative intensive care gate, whose intervals were computed before an earlier pairing scheme was withdrawn and are narrower than those runs entitle them to be.

### Confidence Intervals

Every interval reported is a Student $t$ interval on the mean across replications, with a negative lower bound truncated at zero where the response cannot go below zero, as queue lengths and counts cannot.

Confidence intervals and percentile ranges answer different questions, and several tables below report both. The interval says how precisely the mean is known. The 10th-to-90th-percentile range says how widely one campaign varies around that mean. The range is many times the wider throughout, and it is the range to keep in mind when reading any figure taken from a single campaign.

### Replication Count and Resolution

Replication counts differ between experiments because responses differ in how many events each campaign supplies. A queue measure is time-weighted over every arrival and departure at that resource across 30 days. A death count rests on a handful of events, a Falklands-rate campaign producing about one death of wounds.

That difference decides which findings below are stated as measured and which as unresolved. On the treated-cohort died-of-wounds rate, a 95% half-width of 0.10 percentage points needs 62 replications and one of 0.05 points needs 237. The 50-replication mortality figures below therefore carry about $\pm 0.11$ points, which is enough to separate two casualty intensities whose rates differ eightfold, and not enough to separate two treatment pathways within one intensity. Queue and occupancy measures are far better resolved at the same count, which is why comparable critical care simulations report occupancy and queue results at replication counts of this order [[13]](#references).

### Reading the Evidence Labels

Each option below carries one of four labels, so a planner can see at a glance what the evidence behind it will bear. The labels describe the strength of the evidence, not the size or desirability of the change. A label attaches to a specific claim: a diagnosis can be measured while the remedy for it is untested, and several options below carry more than one.

| Label | Meaning |
|---|---|
| **Measured** | Estimated with a 95% confidence interval that excludes no effect. Direction and approximate size are both supported. |
| **Direction only** | The mechanism is confirmed to fire and the estimate moves as predicted, but the interval admits no effect. The sign is supported; the size is not. |
| **Unresolved** | The replication count cannot separate the effect from noise. Figures are bounds on an effect, not estimates of one. |
| **Untested** | The model cannot evaluate the option, because the structure it would act on is not represented. |

### Casualty Intensities Compared

Two casualty intensities are compared, both taking their arrival rates from the FORECAS casualty projection study [[3]](#references). The moderate intensity is calibrated to the Falklands 1982 campaign and the high intensity to Okinawa 1945. Each carries the died-of-wounds experience of its own campaign, fitted to the mortality rate reported among casualties who reached a treatment facility alive, the Okinawa figure taken from the US Army's own reporting [[4]](#references). What the high intensity still takes from the Falklands-calibrated base is its triage priority split, its disease composition and its transport times.

The health system is identical under both intensities, ensuring the comparison isolates casualty intensity rather than confounding it with a change of establishment. Element, bed and transport fleet sizes are parameters a planner using the simulation can set, but a casualty intensity profile does not adjust them.

---
## Where the Trauma System Fails First

<small>[Return to Top](#contents)</small>

**The establishment that copes at Falklands-equivalent casualty rates does not cope at Okinawa intensity, and it gives way at the rearward operating theatres before anywhere else.** Casualty volume rises by a factor of 2.33 between the two intensities while the R2E theatre queue rises by a factor of about 36. Sizing the system by the casualty ratio alone would therefore under-provide surgery by more than an order of magnitude.

### Comparative Scenario Analysis

**Design.** 50 replications of 30 simulated days at each casualty intensity, control seed 42, under the same establishment throughout.

| Metric | Moderate (Falklands) | High (Okinawa) | Ratio |
|---|---|---|---|
| Total casualties/run | 437.8, 95% CI [421.0, 454.7] (p10–p90: 362.7–528.0) | 1,021.0, 95% CI [993.9, 1,048.1] (p10–p90: 906.5–1,138.5) | 2.33× |
| WIA/run | 188.7, 95% CI [175.4, 202.0] (p10–p90: 137.6–251.5) | 684.3, 95% CI [658.3, 710.3] (p10–p90: 586.2–792.5) | 3.63× |
| DOW/run | 0.78, 95% CI [0.55, 1.01] (p10–p90: 0–2.0) | 23.58, 95% CI [21.88, 25.28] (p10–p90: 18.0–32.1) | 30.2× |
| DOW/WIA rate | 0.42%, 95% CI [0.29%, 0.54%] (p10–p90: 0–1.02%) | 3.43%, 95% CI [3.24%, 3.61%] (p10–p90: 2.63%–4.22%) | 8.24× |

Casualty counts vary widely from campaign to campaign because each arrival stream draws its daily rate from a distribution before placing that day's arrivals within the day [[14]](#references), so the between-day variation the historical source reports reaches the output rather than being averaged away. Total casualties at moderate intensity span 362.7 to 528.0 between the 10th and 90th percentiles against a mean of 437.8. A planner sizing against the mean of either intensity is sizing against a day the campaign will frequently exceed.

The died-of-wounds rows measure more than the health system, because each intensity carries the mortality experience of its own campaign. The eightfold difference in deaths as a share of wounded combines Okinawa's heavier casualty volume, the treatment queues that volume produces, and a standard of surgical and resuscitative care four decades older. Only the first two belong to this comparison, so no planning case should rest on that ratio as a measure of what surge alone costs.

| Resource group | Moderate mean queue (95% CI) | High mean queue (95% CI) | Ratio |
|---|---|---|---|
| R2B operating theatre | 0.000 [0.000, 0.000] | 0.000 [0.000, 0.000] | not applicable |
| R2B holding beds | 0.593 [0.501, 0.685] | 3.228 [3.005, 3.452] | 5.45× |
| R2E operating theatre | 1.063 [0.691, 1.435] | 38.17 [34.01, 42.33] | 35.9× |
| R2E intensive care | 0.131 [0.104, 0.159] | 0.564 [0.464, 0.664] | 4.29× |
| R2E holding beds | 0.598 [0.437, 0.758] | 2.694 [2.449, 2.938] | 4.51× |
| Transport (ambulance / truck) | 0.0038 [0.0000, 0.0078] | 0.0278 [0.0196, 0.0361] | 7.25× |

Each cell is the mean across 50 campaigns of that campaign's mean queue over the group's resources. A resource idle throughout a campaign contributes a zero rather than dropping out, so the denominator is the full establishment every time.

![Four-panel bar chart of mean queue length by resource group, R2B operating theatre, R2E operating theatre, R2E intensive care and transport, each panel comparing the high and moderate casualty intensities with error bars, on four different vertical scales](../images/scenario_comparison.png)

Each panel carries its own vertical scale, so the panels compare intensities rather than resources: the R2E theatre panel runs to 60 casualties while the transport panel runs to 0.07. Both bars in the R2B theatre panel sit at zero. The error bars show the spread across campaigns rather than a confidence interval, and every high-intensity bar is wide enough to show that surge queues vary a great deal between campaigns.

### The Binding Constraint: Surgical Team Scheduling

**Surgical team scheduling, not theatre space, is the major system constraint.** A casualty takes an operating theatre before taking one of the three surgical teams that staff them, so a theatre reads as occupied while the casualty inside it waits for people. Operating accounts for only about 30% of the two R2E theatres' combined availability against room occupancy figures of 67% and 53%, so most of what the theatres report is a wait for staff [[5]](#references). **Evidence: measured.** A planner reading the theatre queue as a demand for rooms will buy the wrong thing.

Theatre contention is a standing property of this establishment rather than a peer-conflict phenomenon. The queue at Falklands-equivalent load is 1.06 casualties, not zero, so an arrival process delivering genuine heavy days produces contention at moderate rates too. Okinawa intensity makes acute a constraint the establishment already carries on its own heavy days, which moves the remedy from a contingency measure to a standing one.

The rest of the rearward hospital follows the theatres rather than leading them. Intensive care rises 4.3-fold, the flattest of the three rearward groups, because only the damage control cohort takes a stabilisation episode. Holding beds rise 4.5-fold, absorbing what intensive care does not, since a holding bed is where a casualty goes when no intensive care bed is free and where those awaiting strategic evacuation wait.

---

## Planning Options in Priority Order

<small>[Return to Top](#contents)</small>

**Five options follow, ordered by how directly each bears on the health outcome of the force and, where that is comparable, by the strength of the evidence behind it.** The ordering principle matters as much as the order. An option addressing the binding constraint ranks above one that is better measured but acts where the system is not failing, and a planner who inverts that buys precision instead of capability.

| Priority | Option | What the evidence establishes |
|---|---|---|
| 1 | Extend surgical team coverage at R2B and R2E | Diagnosis **measured**; remedy **untested**, needing a clinician fatigue model to cost |
| 2 | Relieve forward holding capacity at R2B | Diagnosis **measured** at both intensities; the three remedies **untested** under replication |
| 3 | Hold casualties forward for a reopening team | **Measured** on casualties held and diversions avoided; **unresolved** on forward surgeries gained |
| 4 | Sustain the force with a reinforcement demand cycle | **Measured** and effective at high intensity; no detectable effect at moderate intensity |
| 5 | Take risk on the transport fleet | **Measured**: margin holds to two of three ambulances, collapses at one |
| Not recommended | Move post-operative intensive care forward to R2B | **Unresolved** across the whole sweep, with no benefit visible at any setting |

### Option 1. Extend Surgical Team Coverage

**Surgical team time is the capacity to add, at both echelons, and it is the change this model cannot yet cost.** The diagnosis is measured and the mechanism is the same forward and rearward: theatres stand available around the clock while the teams that staff them work 12-hour rosters. **Evidence for the diagnosis: measured. Evidence for the remedy: untested.**

Forward, the two R2B facilities lose surgery for half of each day. Of the casualties reaching the forward surgical decision point in a single verified campaign, 71% of those diverted rearward were diverted because the surgical team was off shift, against 29% because the theatre was busy [[5]](#references). Extending the forward team's hours, or fielding a second team to cover the other half of the day, would recover surgery for casualties who presently travel further to reach it. Time to surgery is among the strongest determinants of survival after severe battlefield injury [[15]](#references), so the loss is clinical rather than merely a low utilisation figure.

Rearward, three teams cover the day on 12-hour rosters, which puts two on during the first shift and one on during the second. The single second-shift team is busy for 53.6% of its open time against 30.8% for each first-shift team, and queued for 2.45% against 0.67% and 0.60% [[5]](#references). A fourth team rostered to the night shift would buy more throughput than a third theatre.

Neither form can be costed here. Establishment sizes are fixed structure in the present model, so what a fourth team buys cannot be swept. Extending shifts cannot be assessed fairly without a model of clinician fatigue and the errors and complications that accompany it, and reporting the extra throughput without that counterweight would overstate the change. What a planner can take now is the ranking: surgical capacity should be added as staff time, not as theatre space, and the case does not depend on assuming a peer-intensity fight.

### Option 2. Relieve Forward Holding Capacity

**Forward holding is the second constraint, and it is saturated by the establishment's own arithmetic rather than only by surge.** The forward holding queue rises 5.45-fold between the intensities, the second largest movement in the queue table, driven by the proportional rise in non-surgical wounded rather than by any change in disease. **Evidence: measured.**

Ten forward holding beds are fielded against an expected occupancy of about 15.5, an overload of 55% that no change in surgical throughput can close, and disease is the stream that fills them [[5]](#references). Three remedies are open. Shortening the length of stay cannot reach inside capacity at any clinically plausible figure. Adding beds works, at ten per facility rather than the five fielded. An evacuation threshold moving long-stay patients rearward is cheapest, at the price of transferring a non-surgical medical load onto the echelon Option 1 has already identified as binding. **Evidence for the three remedies: untested under replication.** Sweeping holding capacity jointly against the evacuation threshold needs no new model structure and is the most tractable outstanding work in this paper.

### Option 3. Hold Casualties Forward for a Reopening Team

**The forward hold window keeps about six casualties per campaign at the forward facility instead of sending them rearward, and this design cannot show what those holds buy in surgery.** Both halves matter: the mechanism is demonstrably live, and its value is not yet measured.

#### The R2B Pre-Open Hold Window

A casualty arriving while the forward surgical team is off shift may be held for a team about to return rather than diverted rearward. The window ships at 60 minutes and has no source behind it, so what it achieves is a question to be measured.

**Design.** 50 replications of 30 simulated days per arm at control seed 42, the window set to 0 in one arm against 60 minutes in the other. The third column is the mean of the per-campaign paired difference.

| Measure | Window 0 | Window 60 min | Difference (95% CI) |
| --- | --- | --- | --- |
| Casualties held forward | 0 | 5.90 | +5.90 [+5.18, +6.62] |
| R2B surgeries | 51.82 | 52.20 | +0.38 [−2.75, +3.51] |
| Diverted, team off shift | 84.94 | 75.24 | −9.70 [−17.25, −2.15] |
| Diverted, theatre busy or queued | 19.76 | 17.08 | −2.68 [−6.95, +1.59] |
| R2E first surgeries | 125.16 | 117.96 | −7.20 [−16.33, +1.93] |
| R2E theatre entry deferred | 18.94 | 15.62 | −3.32 [−6.56, −0.08] |
| Died of wounds per run | 1.02 | 1.02 | +0.00 [−0.38, +0.38] |
| Total casualties | 442.82 | 433.18 | −9.64 [−32.00, +12.72] |

On what the window was added to do, the measurement is decisive: 5.90 casualties held forward per campaign, and off-shift diversions down by 9.70, neither interval including zero. **Evidence: measured.**

What the measurement cannot establish is whether forward surgeries rise by the number held. Forward surgeries move by +0.38, an interval containing both zero and the +5.90 the holds would predict, so it cannot tell those possibilities apart. **Evidence: unresolved.** The cause is that the two arms are not the same realisation: introducing a hold shifts the sequence of random draws, and the force-regeneration loop feeds that shift back into arrival timing, so the arms drift into different casualty streams. No pair of the 50 generated the same casualty count in both arms. Resolving the effect to a half-width of two operations takes about 120 replications per arm, which has not been run.

Two rows still read. Deferred theatre entry rearward falls by 3.32, an interval excluding zero, which points the way the mechanism predicts: operating earlier forward relieves a little pressure on the binding echelon. Mortality is flat, which at this replication count is a null result rather than a demonstration that the window costs no lives.

### Option 4. Sustain the Force with a Reinforcement Demand Cycle

**Reinforcement is the one lever that is both measured and effective, and it acts on the force rather than on the health system.**

#### Force Regeneration Under Reinforcement

Casualties are generated against the force actually present rather than a fixed roll strength, so sustained losses reduce daily casualty volume as the force depletes, and reinforcement should offset that decline.

**Design.** 15 replications per row at moderate intensity and 12 at high intensity, each of 30 simulated days, daily volume fitted with an ordinary least-squares trend. Reinforced rows use a 7-day demand cycle with a 7-day fulfillment lag.

| Casualty intensity | Reinforcement | Daily volume slope | p-value | First-week mean | Last-week mean |
|---|---|---|---|---|---|
| Moderate (15 reps) | None | −0.018/day | 0.75 | 14.9 | 14.4 |
| Moderate (15 reps) | 7-day demand cycle | −0.103/day | 0.17 | 16.6 | 14.1 |
| High (12 reps) | None | −0.349/day | 0.0027 | 39.8 | 29.5 |
| High (12 reps) | 7-day demand cycle | +0.030/day | 0.80 | 35.8 | 36.1 |

At high intensity the mechanism is clear. Daily volume falls 26% from the first week to the last without reinforcement, and the demand cycle removes that decline entirely, leaving a slope indistinguishable from flat. **Evidence: measured.** Reinforcement halts depletion without overshooting into growth, because each cycle asks for the pool's actual current shortfall rather than a fixed number. At moderate intensity neither configuration shows a slope distinguishable from flat, 30 days of attrition at that volume being too little to deplete the force measurably.

One planning implication runs against the obvious one. A reinforced force sustains its casualty production, so the health system serving it faces a load that does not taper as the campaign proceeds. Reinforcement raises the medical requirement at the same time as it sustains combat power, and the two decisions belong together.

### Option 5. Take Risk on the Transport Fleet

**Transport is where a planner can release resource rather than spend it.** The fleet holds its margin down to two ambulances and loses it at one, so the fielded three sit on the flat part of the curve rather than at its bend. **Evidence: measured.**

#### Transport Fleet-Size Sweep

**Design.** 10 replications of 30 simulated days per sweep point at control seed 42, the ambulance fleet swept across 1 to 5 vehicles and the truck fleet across 1 to 4, each with the other held at its fielded size.

![Four-panel line plot of mean queue and mean utilisation against fleet size for the ambulance and truck fleets, each line with a 95% confidence ribbon and a dashed vertical line marking the current establishment size](../images/transport_capacity_margin_by_fleet_size.png)

The ambulance queue collapses between one and two vehicles and is flat thereafter. Utilisation rises again beyond three vehicles because the measure averages across a pool that is rarely fully engaged.

| Fleet size | Ambulance mean queue (95% CI) | Ambulance utilisation | Truck mean queue (95% CI) | Truck utilisation |
|---|---|---|---|---|
| 1 | 2.1060 (0.2270–3.9850) | 38.7% | 0.0442 (0.0000–0.1021) | 10.7% |
| 2 | 0.0487 (0.0000–0.0974) | 18.5% | 0.0011 (0.0000–0.0022) | 5.5% |
| 3 (current ambulance) | 0.0068 (0.0000–0.0155) | 12.5% | 0.0001 (0.0000–0.0002) | 11.1% |
| 4 (current truck) | 0.0006 (0.0000–0.0012) | 14.6% | 0.0000 | 14.6% |
| 5 | 0.0001 (0.0000–0.0001) | 17.8% | not swept | not swept |

At a single vehicle the ambulance fleet queues heavily, at a mean of 2.11 casualties, so the sweep locates the capacity boundary sharply rather than merely confirming adequacy at the current size. The queue falls roughly fortyfold at two vehicles and sevenfold again at three, where it is small but not zero. What produces a queue at all is the day-to-day variation in casualty volume rather than its average, a transport queue forming on peak days and no others.

Utilisation across the swept range is too poorly determined to read, running the wrong way on both platforms and carrying intervals spanning most of its range. The queue column is the one to read. One qualification bounds the licence this option gives: the sweep was run at moderate intensity only, and the surge comparison puts the transport queue up 7.25-fold at high intensity.

### An Option That Does Not Pay: Moving Post-Operative Intensive Care Forward

**Moving post-operative stabilisation forward changes nothing this sweep can measure, because the cohort the lever reaches is too small to relieve a rearward unit running above 83% occupancy.** **Evidence: unresolved across the whole swept range**, which for a planning decision differs from a measured absence of effect: the sweep licenses leaving the setting alone, not a claim that forward intensive care could never help.

#### Forward ICU Share Decision Frontier

A casualty's need for stabilisation is a single quantity the forward-holding policy divides between the two echelons, so sweeping the policy moves load without changing how much care is given. Only damage control casualties have a stabilisation phase, so the lever acts on roughly half of operated casualties.

**Design.** 20 replications of 30 simulated days per sweep point at control seed 42, the forward share set to 0, 25, 50, 75 and 100% in turn.

![Five stacked line plots against the share of post-operative intensive care delivered forward, from 0% to 100%, showing rearward intensive care mean queue, forward and rearward intensive care utilisation, the share of post-definitive care delivered in intensive care, and died-of-wounds count, each with a 95% confidence ribbon](../images/r2b_icu_share_frontier.png)

Every panel moves little across the full sweep and every confidence ribbon covers the whole movement.

| Forward share | R2E ICU mean queue (95% CI) | R2B ICU utilisation | R2E ICU utilisation | Post-definitive care in ICU (95% CI) | Mean DOW per run (95% CI) |
|---|---|---|---|---|---|
| 0% (current) | 0.108 (0.066–0.149) | 22.4% | 87.7% | 35.5% (28.4–42.6) | 0.80 (0.35–1.25) |
| 25% | 0.080 (0.042–0.119) | 22.1% | 84.9% | 38.7% (30.4–46.9) | 1.00 (0.52–1.48) |
| 50% | 0.078 (0.028–0.129) | 14.1% | 83.4% | 41.6% (34.5–48.8) | 1.00 (0.52–1.48) |
| 75% | 0.079 (0.036–0.121) | 20.2% | 83.8% | 42.2% (31.4–52.9) | 1.10 (0.47–1.73) |
| 100% | 0.125 (0.033–0.218) | 22.7% | 83.9% | 42.0% (32.4–51.6) | 1.00 (0.25–1.75) |

The lever does so little because the population it acts on is small. About half of operated casualties take the single-stage pathway and have no stabilisation phase to move; of the rest, only those operated on forward can have any of it served forward. What remains is too small a cohort to relieve a unit above 83% occupancy. The rearward intensive care queue moves between 0.078 and 0.125 casualties with overlapping intervals and no trend, and its highest value falls at the full forward share, where the lever should help most.

The setting therefore stays where it is. The lever may still matter at higher casualty rates, where rearward intensive care is contended by a wider margin, and that is the experiment worth running next.

---

## What Resourcing Alone Cannot Fix

<small>[Return to Top](#contents)</small>

**Three behaviours persist whatever the establishment is set to, and each changes how a planner should read what the system reports.** None is a lever; they are the reasons a resourcing decision taken on the face of the output can go wrong.

### Rearward Diversion Hides the Forward Shortfall

**The forward theatre queue reads zero at both casualty intensities, and not because the forward facilities absorb any of the surge.** Casualties requiring surgery are sent rearward whenever the forward theatre is busy or the surgical team has been off shift beyond the hold window, rather than being allowed to wait. At high intensity that pushes the entire surge onto a rearward hospital with little spare capacity to take it.

A zero queue at a forward echelon is therefore the signature of a shortfall being exported, not of capacity being adequate. The same displacement appears in holding, where a capacity-aware routing policy diverted 179 casualties rearward before transport in a single verified campaign [[5]](#references). A planner auditing this system by queue lengths alone would conclude that the forward facilities need nothing and the rearward hospital needs everything, when part of what the rearward hospital needs is what the forward facilities could not provide. Any measure of forward adequacy has to count what was sent rearward, not what waited.

### The Post-Operative Intensive Care Gate

**The intensive care gate moved a large amount of load off intensive care and left its mortality effect unresolved.** A damage control casualty leaving theatre needs stabilisation, and the model makes entry to theatre depend on an intensive care bed being free to provide it. A Priority 1 casualty is operated on regardless and recovers in a holding bed at raised risk when no bed is free; a Priority 2 or lower casualty waits for a bed.

**Design.** 50 replications of 30 simulated days at an independent seed per replication, run before and after the gate was introduced. These intervals alone were computed under a pairing scheme since withdrawn and are narrower than the runs entitle them to be, which only strengthens the conclusion that they overlap.

Mean rearward intensive care utilisation fell from 74.1% to 60.2%. Mean deaths of wounds per campaign rose from 0.84 (95% CI [0.58, 1.10]) to 1.00 (95% CI [0.74, 1.26]), intervals that overlap heavily. **Evidence: direction only.** Inside the checkpoint the design behaved as intended: the holding pathway's death rate, 2 in 1,223 casualties, was roughly 2.8 times the intensive care pathway's 3 in 5,085. The small counts leave that ratio very uncertain and it should be read for direction, not size.

For a planner the gate is a measuring instrument rather than a lever. It adds no capacity; it converts an intensive care shortfall into a count of casualties who received a degraded pathway, which is a more useful quantity to plan against than a queue length, because it names who bore the cost. The remedy it points at is intensive care capacity at the rearward hospital, competing for the same resource as Option 1.

### Mass Casualty Event Stress Test

**An injected surge degrades the care delivered without revealing a constraint the background tempo was hiding.** A background casualty tempo that produces heavy days of its own has already consumed the forward echelons' spare capacity, so there is no reserve to be found by looking harder at the peaks: the peaks are already the design case.

**Design.** 10 replications of 30 simulated days at control seed 42, mass casualty events injected at 0.2 events per day against a background-only arm.

| Metric | Background only | With mass casualty injection |
|---|---|---|
| Mean total casualties/run | 444.6 | 682.1 |
| Mean mass casualty events/run | 0 | 5.40 (range 3–8) |
| DOW rate, background-origin casualties | 0.18% (8/4,446) | 0.28% (13/4,577) |
| DOW rate, event-origin casualties | not applicable | 0.58% (13/2,244) |

Casualties from mass casualty events die of wounds at 2.1 times the background rate, consistent with a blast-dominant priority mix arriving faster than steady-state capacity can absorb. **Evidence: direction only**, 13 deaths in each arm being too few for a precise ratio. The background arm is not a quiet baseline, which is why its rate is 0.18% rather than zero and why the gap between the arms is narrower than the injected volume alone would suggest.

The clearest signal is at the intensive care gate. Post-operative stabilisation splits 85 in holding against 37 in intensive care under injection, where the same campaign without injection gives 58 and 79 [[5]](#references). The majority pathway flips: a cohort that mostly recovered in an intensive care bed now mostly recovers in a holding bed, and it stays flipped for the whole campaign rather than only the event windows. Theatre and diversion measures barely move, which is the finding rather than an absence of one.

![Stem plot of two mass casualty events reconstructed from one campaign, each drawn as a vertical line at its simulation day with a point at its casualty count: 33 casualties midway through day 13 and 45 midway through day 26](../images/mass_casualty_events.png)

Two events thirteen days apart is a thin draw from a process configured to deliver an average of six across the campaign, so this campaign illustrates the injection mechanism while the replicated table above carries the measurement.

---

## Demand on the National Support Base

<small>[Return to Top](#contents)</small>

**Strategic evacuation is bound by how many sorties depart rather than by how many places each carries, and every casualty left waiting holds a rearward hospital bed while waiting.** The findings in this section come from one verified campaign rather than from replication, and are labelled accordingly. **Evidence: direction only, from a single campaign.**

Of 135 casualties reaching a strategic evacuation decision in that campaign, 99 boarded and reached the national support base within 30 days while 36 were still waiting, and still occupying a rearward holding bed, when the campaign ended [[5]](#references). Two of four scheduled sorties were cancelled, so the first to fly departed on day 21 and the mean wait reached 10.1 days. Each aircraft offers 36 high-dependency and 54 ambulatory places, and the sortie that flew on day 21 filled its high-dependency cabin exactly and still left a queue behind. The lever a planner holds is therefore schedule resilience, a reserve airframe or a shorter nominal interval, rather than cabin fit.

Two consequences follow for the national support base. Its demand peaks after the campaign ends rather than during it: occupancy reached 90 concurrent patients on the campaign's last day and decayed to near zero only around day 69, so the base carries a load well beyond the period that generates it. And the evacuation backlog is a clinical constraint forward, not only an administrative one, because one pool of rearward holding beds carries both in-theatre recovery and the evacuation wait. No intensive care or theatre finding in this paper should be read apart from that.

These figures rest on one campaign and one set of sortie cancellation draws, so they establish the mechanism rather than its size. A replicated analysis of national support base demand is the first item on the research agenda below.

---

## Evidence That Does Not Yet Support a Decision

<small>[Return to Top](#contents)</small>

**Three effects above are unresolved at the replication counts run, and reporting them as unresolved rather than passing over them is itself a planning input.** A point estimate whose interval spans zero is not a small effect but an unmeasured one, and the distinction decides whether a planner is entitled to act on it.

| Unresolved effect | What it would take |
|---|---|
| Forward surgeries gained from the hold window | About 120 replications per arm for a half-width of two operations |
| Diversion composition and rearward surgery counts under the window | Several hundred to a few thousand replications per arm |
| Every response in the forward intensive care share sweep | More than 20 replications per point, and a surge intensity where rearward intensive care is contended by a wider margin |
| Mortality effect of the post-operative intensive care gate | A rare-event response over cohorts of a few dozen casualties; not resolved at any count this project has run |

Two of those are affordable and one is not. The first two need larger runs of the existing framework. The gate's mortality effect differs in kind: deaths of wounds at Falklands-calibrated rates are rare enough that no replication count would resolve a difference between two pathway cohorts of a few dozen casualties each, so settling it needs a different response variable rather than more computation.

---

## Research and Development Agenda

<small>[Return to Top](#contents)</small>

**The option this paper ranks first is one it cannot evaluate, which sets the agenda.** Each item names the decision it would unblock.

| Priority | What to build | Which decision it unblocks |
|---|---|---|
| 1 | A replicated analysis of national support base demand and strategic evacuation reliability | Whether sortie resilience or cabin capacity binds evacuation, and how large the post-campaign demand peak is |
| 2 | A configurable establishment, with team and element counts a scenario can vary | Option 1: what a fourth surgical team rostered to the night shift buys against a third theatre |
| 3 | A clinician fatigue model representing the errors and complications of extended shifts | Option 1 in its cheaper form: whether extending existing teams' hours is worth its clinical cost |
| 4 | A joint sweep of forward holding capacity against the evacuation threshold | Option 2: which remedy to buy, given that the threshold transfers load onto the binding echelon |
| 5 | Severity-conditioned surgery durations, replacing the single distribution serving every casualty | Whether the theatre contention measured here is understated on exactly the heavy days it comes from |
| 6 | A casualty intensity combining one campaign's rates with another's mortality model | Whether any part of the eightfold mortality ratio is attributable to surge rather than to era of care |

Items 2 and 3 gate the change this paper ranks first and can otherwise only name. Item 4 needs no new model structure and is the most tractable work outstanding.

---

## Limitations

<small>[Return to Top](#contents)</small>

Four kinds of limitation bear on how the options above should be read.

The first is verification against validation. This paper reports experiments on a model that has been verified to behave as its specification describes, which is a separate question from whether that specification represents the real system well [[16]](#references). The options are options inside the model, and hold only as far as the model does.

The second is resolution. Three effects cannot be separated from noise at the replication counts run, and are collected above with the counts they would need. A point estimate whose interval spans zero is a bound, not an estimate.

The third is that several comparisons are not controlled in the way a paired design assumes. Any parameter change that alters the order of events shifts the sequence of random draws, and the force-regeneration loop feeds that shift back into arrival timing, so two arms run at one control seed drift into different casualty streams. Only the casualty intensity comparison escapes this, its arms differing by design rather than by a small perturbation.

The fourth is what the model does not represent. Clinical teams are taken whole, so team utilisation overstates scarcity where a procedure needs only part of a team; this is also what prevents Option 1 from being evaluated. A single surgery duration serves every casualty whatever their severity, so theatre contention is understated on exactly the heavy days the surge figures come from. One pool of rearward holding beds carries both in-theatre recovery and the evacuation wait, so those queue figures combine two demands. The high casualty intensity takes its priority split, disease composition and transport times from the Falklands-calibrated base, so only its rates and mortality model come from Okinawa. And the mortality calibration target is a bounded treated-cohort rate, so agreement with it is consistency with the anchor rather than validation against it.

---

## Conclusion

<small>[Return to Top](#contents)</small>

**The establishment does not scale to Okinawa intensity, and the change the evidence points at most directly is the one this model cannot yet cost.** The rearward theatre queue rises about 36-fold against a 2.33-fold rise in casualty volume, forward holding about 5-fold and rearward holding about 4.5-fold, while the forward theatre queue reads zero throughout only because casualties are diverted rearward. Surgical team scheduling is the constraint, so the capacity to add is staff time rather than theatre space. That contention is present at Falklands-equivalent rates too, which moves the case for adding it from a contingency measure to a standing one.

**Among the options the model can evaluate, the ranking is clear and the margins are not.** A reinforcement demand cycle is the one lever both measured and effective, though a sustained force also sustains the medical load it generates and the two decisions belong together. Forward holding needs relief at either intensity, and which of its three remedies to buy is the most tractable open question here. The forward hold window keeps about six casualties per campaign at the forward facility, while what those holds buy in surgery stays unresolved. Transport is where a planner can take risk. Moving post-operative intensive care forward pays nothing measurable.

**Three effects remain unresolved, and one will not be settled by more computation.** Reporting them as unmeasured rather than as small is the difference between a planner declining to act and a planner acting on noise. Until a configurable establishment and a clinician fatigue model exist, extending surgical team coverage can be argued from the mechanism measured here but not costed against the alternatives.

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Remondelli, M. H., Remick, K. N., Shackelford, S. A., Gurney, J. M., Pamplin, J. C., Polk, T. M., Potter, B. K., & Holt, D. B. (2023). Casualty care implications of large-scale combat operations. *Journal of Trauma and Acute Care Surgery*, *95*(2S), S180–S184. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC10389308/

[2] Fandre, M. (2020). Medical changes needed for large-scale combat operations: observations from Mission Command Training Program warfighter exercises. *Military Review*. Retrieved 27 Aug 26, from https://www.armyupress.army.mil/Journals/Military-Review/English-Edition-Archives/May-June-2020/Fandre-Medical-Changes/

[3] Blood, C. G., Zouris, J. M., & Rotblatt, D. (1998). *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

[4] Marble, S. (2025). Both joint and not: Medical support at Okinawa, 1945. *Joint Force Quarterly*, *117*(2), article 11. National Defense University Press. Retrieved 17 Aug 26, from https://digitalcommons.ndu.edu/joint-force-quarterly/vol117/iss2/11/

[5] Battlefield Casualty Handling project. (2026). *Applying Discrete Event Simulation to the Land-Based Trauma System: Model Verification and Baseline Behaviour*. Retrieved 06 Sep 26, from https://github.com/natosys/Battlefield-Casualty-Handling/blob/main/docs/Single_Run_Analysis.md

[6] Battlefield Casualty Handling project. (2026). *Supplementary Material: Experimental Designs and Statistical Methods for the Replicated Trauma System Experiments*. Retrieved 06 Sep 26, from https://github.com/natosys/Battlefield-Casualty-Handling/blob/main/docs/Multi_Run_Supplement.md

[7] NATO Standardization Office. (2019). *AJP-4.10 Allied Joint Doctrine for Medical Support* (Edition C, Version 1). NATO Standardization Office. Retrieved 27 Aug 26, from https://www.coemed.org/files/stanags/01_AJP/AJP-4.10_EDC_V1_E_2228.pdf

[8] Ucar, I., Smeets, B., & Azcorra, A. (2019). simmer: Discrete-Event Simulation for R. *Journal of Statistical Software*, *90*(2), 1–30. Retrieved 27 Aug 26, from https://doi.org/10.18637/jss.v090.i02

[9] Law, A. M. (2020). Statistical analysis of simulation output data: the practical state of the art. In *Proceedings of the 2020 Winter Simulation Conference* (pp. 1117–1127). INFORMS Simulation Society. Retrieved 27 Aug 26, from https://informs-sim.org/wsc20papers/134.pdf

[10] Rossetti, M. D. *Simulation Modeling and Arena*, Chapter 5: Statistical Analysis for Infinite Horizon Simulation Models. Retrieved 27 Aug 26, from https://rossetti.github.io/RossettiArenaBook/05-Chapter5.html

[11] Karl, A., Eubank, R., Milovanovic, J., Reiser, M., & Young, D. (2014). Using RngStreams for parallel random number generation in C++ and R. *Computational Statistics*, *29*(5), 1301–1320. Open-access preprint retrieved 26 Jun 26, from https://arxiv.org/abs/1403.7645

[12] R Core Team. (2024). *RNGstreams: L'Ecuyer's RngStreams for parallel random number generation*. R Documentation, parallel package. Retrieved 26 Jun 26, from https://stat.ethz.ch/R-manual/R-patched/library/parallel/html/RngStream.html

[13] Williams, E., Szakmany, T., Spernaes, I., Muthuswamy, B., & Holborn, P. (2020). Discrete-event simulation modeling of critical care flow: new hospital, old challenges. *Critical Care Explorations*, *2*(9), e0174. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC7491890/

[14] Lewis, P. A. W., & Shedler, G. S. (1979). Simulation of nonhomogeneous Poisson processes by thinning. *Naval Research Logistics Quarterly*, *26*(3), 403–413. Naval Postgraduate School Calhoun repository. Retrieved 13 Aug 26, from https://calhoun.nps.edu/handle/10945/63159

[15] Kotwal, R. S., Montgomery, H. R., Kotwal, B. M., Champion, H. R., Butler, F. K., Mabry, R. L., Cain, J. S., Blackbourne, L. H., Mechler, K. K., & Holcomb, J. B. (2011). Eliminating preventable death on the battlefield. *Archives of Surgery*, *146*(12), 1350–1358. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC5832013/

[16] Sargent, R. G. (2010). Verification and validation of simulation models. In *Proceedings of the 2010 Winter Simulation Conference* (pp. 166–183). IEEE. Retrieved 27 Aug 26, from https://www.informs-sim.org/wsc10papers/016.pdf

<!-- REFERENCES END -->
