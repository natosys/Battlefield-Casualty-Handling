# Applying Discrete Event Simulation to the Land-Based Trauma System: Planning Options for Surge Capacity and System Transformation

## Abstract

<small>[Return to Top](#contents)</small>

**Background**

A deployed health system has to be sized before the campaign it will serve. The casualty volumes expected in large scale combat operations exceed those the deployed health systems of the past two decades were built around, so a planner needs to know not only whether the present establishment copes, but which of the changes available to them buys the most improvement in health outcomes for the force.

**Objective**

To establish where the modelled land-based trauma system fails first as casualty intensity rises, to set out the changes a planner can make in order of what the evidence supports, and to state plainly which candidate transformations the present model cannot yet evaluate.

**Methods**

Every experiment the Battlefield Casualty Handling discrete event simulation has been run under replication is reported here, together with the replication and confidence interval methods behind them. At the centre is a comparison of two casualty-rate scenario profiles at 50 replications each: `moderate_intensity`, a Falklands 1982-modified baseline and the casualty rate behind the single-run verification in `docs/Single_Run_Analysis.md`, and `high_intensity`, an Okinawa exemplar whose casualty rates come from FORECAS Tables A.7/A.9 [[1]](#references) and whose died-of-wounds model is fitted to the rate the US Army reported on Okinawa [[2]](#references). Six further experiments test individual design levers: the R2B pre-open hold window, the post-operative intensive care gate, the forward intensive care share, the transport fleet-size margin, the reinforcement demand cycle, and an acute mass casualty surge. Replication counts run from 10 to 50, matched to the response each experiment measures, and every experiment states its own design. Each planning option below carries an explicit label for what the evidence establishes about it.

**Results**

The establishment's adequacy at Falklands-equivalent rates does not carry over to Okinawa intensity. Across 50 replications of each profile (30 simulated days, control seed 42), mean total casualties per run rise 2.33-fold while the R2E operating theatre mean queue rises about 36-fold, the R2B holding bed queue about 5.5-fold, the R2E holding bed queue about 4.5-fold and the R2E intensive care queue about 4.3-fold. The system's response to load is therefore not proportional to the load put on it, and the disproportion is concentrated in one place. The mechanism behind it is the surgical roster rather than theatre space: a casualty takes an operating theatre before taking one of the three surgical sections that staff them, so a room reads as occupied while its occupant waits for people. The R2B theatre queue stays at zero in both profiles, not because R2B absorbs any of the surge but because bypass routing pushes all forward surgical overflow onto an R2E that is already saturated. Transport is the one echelon holding real margin at both intensities, retaining it down to two of its three PMV Ambulances.

**Conclusion**

Deepening the R2E surgical roster is the change the evidence points at most directly, and it is a change this model cannot yet cost, because evaluating a longer shift needs a model of clinician fatigue the simulation does not have. Of the levers the model can evaluate, a reinforcement demand cycle is the one that is both measured and effective, removing the force-depletion trend entirely at Okinawa intensity. Forward holding capacity at R2B needs relief on the evidence of both intensities. Transport is where a planner can take risk. Moving post-operative intensive care forward to R2B pays nothing measurable and should stay switched off. Three further effects remain unresolved at the replication counts run, and each is reported as a bound on an effect rather than an estimate of one.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Introduction](#introduction)
- [Methods](#methods)
  - [Design and Unit of Analysis](#design-and-unit-of-analysis)
  - [Replication Independence](#replication-independence)
  - [Confidence Intervals](#confidence-intervals)
  - [Replication Count and Resolution](#replication-count-and-resolution)
  - [Reading the Evidence Labels](#reading-the-evidence-labels)
  - [Scenario Profiles](#scenario-profiles)
  - [Run Configuration and Provenance](#run-configuration-and-provenance)
- [Where the System Fails First](#where-the-system-fails-first)
  - [Comparative Scenario Analysis](#comparative-scenario-analysis)
    - [Casualty and Mortality Totals](#casualty-and-mortality-totals)
    - [Resource Queue Comparison (mean of per-resource mean queue, by group)](#resource-queue-comparison-mean-of-per-resource-mean-queue-by-group)
  - [The Binding Constraint: the R2E Surgical Roster](#the-binding-constraint-the-r2e-surgical-roster)
- [Planning Options in Priority Order](#planning-options-in-priority-order)
  - [Option 1. Deepen the R2E Surgical Roster](#option-1-deepen-the-r2e-surgical-roster)
  - [Option 2. Relieve Forward Holding Capacity at R2B](#option-2-relieve-forward-holding-capacity-at-r2b)
  - [Option 3. Hold Casualties Forward for a Reopening Section](#option-3-hold-casualties-forward-for-a-reopening-section)
    - [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)
  - [Option 4. Sustain the Force with a Reinforcement Demand Cycle](#option-4-sustain-the-force-with-a-reinforcement-demand-cycle)
    - [Force Regeneration Under Reinforcement](#force-regeneration-under-reinforcement)
  - [Option 5. Take Risk on the Transport Fleet](#option-5-take-risk-on-the-transport-fleet)
    - [Transport Fleet-Size Sweep](#transport-fleet-size-sweep)
  - [An Option That Does Not Pay: Moving Post-Operative Intensive Care Forward](#an-option-that-does-not-pay-moving-post-operative-intensive-care-forward)
    - [Forward ICU Share Decision Frontier](#forward-icu-share-decision-frontier)
- [What Resourcing Alone Cannot Fix](#what-resourcing-alone-cannot-fix)
  - [Bypass Routing Hides the Forward Shortfall](#bypass-routing-hides-the-forward-shortfall)
  - [The Post-Operative Intensive Care Gate](#the-post-operative-intensive-care-gate)
  - [Mass Casualty Event Stress Test](#mass-casualty-event-stress-test)
- [Evidence That Does Not Yet Support a Decision](#evidence-that-does-not-yet-support-a-decision)
- [Research and Development Agenda](#research-and-development-agenda)
- [Limitations](#limitations)
- [Conclusion](#conclusion)
- [References](#references)
<!-- TOC END -->

---

## Introduction

<small>[Return to Top](#contents)</small>

A deployed health system has to be sized before the campaign it will serve, and the planner sizing it makes three decisions this document is built to inform. The first is whether the establishment as it stands carries the casualty load expected of it, and if not, where it gives way first. The second is which of the changes available buys the most improvement for the force. The third, and the one most often left implicit, is which candidate changes the available evidence cannot yet separate from chance, and what it would take to settle them.

Those decisions press harder than recent experience suggests, because the casualty volumes expected in large scale combat operations are greater than those the deployed health systems of the past two decades were built around [[3]](#references), and the planning assumptions carried forward from those operations have been argued to understate both the volume and the acuity a peer fight would produce [[4]](#references). A planner working from the last campaign's establishment is therefore working from a baseline that may not transfer.

This document is organised by those decisions rather than by the experiments that inform them. [Where the System Fails First](#where-the-system-fails-first) diagnoses the establishment under two calibrated casualty intensities and locates the binding constraint. [Planning Options in Priority Order](#planning-options-in-priority-order) sets out what a planner can change, most consequential first, each labelled by what the evidence establishes. [What Resourcing Alone Cannot Fix](#what-resourcing-alone-cannot-fix) covers the behaviour that persists whatever the establishment is set to. [Evidence That Does Not Yet Support a Decision](#evidence-that-does-not-yet-support-a-decision) states the three effects the replication counts cannot resolve and the counts they would need, and [Research and Development Agenda](#research-and-development-agenda) sets out what would have to be built to evaluate the options this model cannot reach. The experiments themselves are retained in full beneath the decisions they inform, each with its own design statement, so the evidential record stays auditable.

What this document does not do is validate the model against the real system. Showing that a model behaves as its specification describes, and showing that the specification represents the system well, are separate tasks resting on separate evidence [[5]](#references). The first is the subject of `docs/Single_Run_Analysis.md`; the second, as far as the available historical anchors allow, of the README's [Parameter Calibration](../README.md#parameter-calibration) section. Every option below is an option inside the model, and holds only as far as the model does. The [Limitations](#limitations) section states the conditions bearing hardest on that.

---

## Methods

<small>[Return to Top](#contents)</small>

### Design and Unit of Analysis

The simulation is a discrete event model built on the `simmer` package for R [[6]](#references). Each casualty is an entity that arrives, then claims and releases clinical staff, beds, operating theatres and transport as it moves rearward through the echelons of allied medical support doctrine [[7]](#references). Every experiment here runs it through the project's multi-run replication framework (`run_replications()`, `R/replication.R`), which executes a requested number of independent replications and returns their monitoring data with a replication index attached.

The replication is the unit of analysis throughout; neither the casualty nor the simulated day is [[8]](#references). Each replication is one complete campaign, run from an empty and idle system over the horizon its section states, and every response is reduced to a single number per replication before any statistic is taken across replications. For a resource that number is the time-weighted mean queue length over the replication's observation window; for a count, the count within it. Taking statistics over casualties or over days instead would treat observations from inside one campaign as though they were separate campaigns, and would produce intervals far narrower than the runs support.

No warm-up period is discarded from any observation window. Discarding one removes the settling-in behaviour a model shows before reaching steady state, which is worth doing only when steady state is the quantity of interest [[9]](#references). This model is a terminating simulation with a fixed campaign length and a genuinely empty start, so that opening period is part of what a planner needs to see. The README's [Warm-up Period Analysis](../README.md#warm-up-period-analysis) sets out that classification and reports the Welch graphical diagnostic, run over ten 90-day replications, which supports it.

### Replication Independence

Every interval in this document divides a sample standard deviation by the square root of the replication count, which is valid only if the replications are independent of one another. Here that independence follows from how a replication is built, rather than being inferred from the output.

Two facts establish it. First, `run_once()` (`R/replication.R`) is a pure function of its seed: re-running a seed reproduces its output exactly, whether or not another run happens in between, so the seed is all that distinguishes two replications and no other channel connects them. Second, `run_replications()` draws a distinct seed for each replication. Independent seeds into a pure function give independent replications, and that is a deterministic argument rather than one a finite sample of correlations could support. `scripts/check_replication_independence.R` asserts both facts and runs on every pull request.

Running replications in parallel preserves both facts. They run under `RNGkind("L'Ecuyer-CMRG")` with `mc.set.seed = TRUE`, which gives each worker its own substream of the MRG32k3a generator, and the substream spacing of $2^{76}$ makes overlap impossible at any simulation budget used here [[10]](#references)[[11]](#references). Both dispatch paths use the one generator, so a replication's output depends on its seed and not on the path or the core count that produced it. A figure quoted at a control seed reproduces at that seed whatever else the session has measured first.

No variance reduction scheme sits on top of that. Antithetic pairing was used earlier in the project and then withdrawn, because neither of the conditions it needs holds for this model. Its reach stops at the arrival generators, since `simmer` draws service times and routing probabilities from the global stream inside its own event loop, in an order set by event timing rather than by entity. The technique also requires the response to move consistently in one direction with the input uniforms [[12]](#references), and casualty arrivals do not, their rate being scaled by an effective force size that trajectory outcomes themselves add to and subtract from. Measurement agrees: over 75 pairs the within-pair correlation on total casualties is $-0.04$ (95% CI $[-0.27, +0.19]$), worth a variance reduction of about 3% and indistinguishable from none. The README's [Multi-run Replication Framework](../README.md#multi-run-replication-framework) gives the construction and that measurement in full. One consequence of the withdrawn pairing reaches this document and is flagged where it appears: the intervals in [The Post-Operative Intensive Care Gate](#the-post-operative-intensive-care-gate) were computed while the pairing was still in force, over replications that were not independent, and are narrower than those runs entitle them to be.

### Confidence Intervals

Every confidence interval in this document is a Student $t$ interval on the mean across replications,

$$\bar{x} \pm t_{0.975,\;n-1}\,\frac{s}{\sqrt{n}}$$

where $n$ is the replication count, $\bar{x}$ the mean across replications of the per-replication response and $s$ its sample standard deviation. No normal approximation and no bootstrap appears anywhere in this document. The same expression is evaluated in `summarise_replications()` (`R/replication.R`), in the comparative scenario runner (`R/scenario_runner.R`) and everywhere in the analysis pipeline that reports an interval, so an interval quoted here, one shown in the Shiny console and one written to a CSV output are the same quantity computed the same way. Where the response cannot go below zero, which is true of every queue length and every count, a negative lower bound is truncated at zero. That truncation is a reporting convention and does not change the arithmetic behind it.

Where a comparison sets two configurations run at the same control seed against each other, the interval is on the mean of the per-replication paired difference rather than on the difference of the two means, and the tables say so in their column headings. Pairing on the seed is worth doing only where it removes variance, and one experiment below reports that it does not (see [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)).

A 10th-to-90th-percentile range is not a confidence interval and is never used as one here. Where a table reports both, the interval says how precisely the mean is known and the range says how widely a single campaign varies around it. The range is many times the wider in every table below, and it is the range to keep in mind when reading any figure from a single run.

### Replication Count and Resolution

Replication counts differ between the experiments below because the responses differ in how many events each replication supplies. A death count, of which a Falklands-rate campaign produces about one, rests on a handful of events per replication; a time-weighted mean queue rests on every arrival and departure at that resource over 30 days.

The best-determined spread available for this model is on the treated-cohort died-of-wounds rate, whose per-replication standard deviation is 0.0039 under the shipped configuration, measured over 150 replications. At that spread, a 95% half-width of 0.15 percentage points needs 29 replications, one of 0.10 pp needs 62, and one of 0.05 pp needs 237. The 50-replication mortality figures below therefore carry roughly $\pm 0.11$ pp on this response. Single 50-replication measurements of one unchanged configuration span 0.132 pp across control seeds, which is why `scripts/check_dow_calibration.R` pools three independent measurements rather than reporting one. `CLAUDE.md`'s Key Parameters table cross-references this section for that derivation.

Queue and occupancy responses are far better resolved at the same replication count, being time-weighted over orders of magnitude more events per replication. A comparable critical care discrete event simulation reports its occupancy and queue results at 95% intervals over replication counts of this order for the same reason [[13]](#references). The pattern runs through every section below. At 50 replications the queue rows of the comparison separate the two casualty-rate profiles decisively; the mortality rows separate them only because the gap between them is large. A rare-event response measured over a cohort of a few dozen casualties, which is what the post-operative mortality of each intensive care pathway amounts to, is not resolved at any replication count this project has run, and the sections reporting such responses say so rather than reading a point estimate as a finding.

### Reading the Evidence Labels

Each planning option below carries one of four labels, so that a planner can see at a glance what the evidence behind it will bear. The labels describe the strength of the evidence, not the size or the desirability of the change.

| Label | Meaning |
|---|---|
| **Measured** | The effect is estimated with a 95% confidence interval that excludes no effect at the replication count run. The direction and the approximate size are both supported. |
| **Direction only** | The point estimate moves as the mechanism predicts and the mechanism is confirmed to fire, but the interval admits no effect. The sign is supported; the size is not. |
| **Unresolved** | The replication count cannot separate the effect from noise. The option states the count that would, and the figures are reported as bounds on an effect rather than estimates of one. |
| **Untested** | The model cannot currently evaluate the option at all, because the structure or the mechanism it would act on is not represented. The option states what would have to be built, and the [Research and Development Agenda](#research-and-development-agenda) carries it forward. |

A label attaches to a specific claim rather than to a whole option, and several options below carry more than one: a diagnosis may be measured while the remedy for it is untested, and a lever may be measured on the thing it directly does while its downstream benefit is unresolved. Where that is so, the option says which label applies to which claim.

### Scenario Profiles

A scenario profile is a named set of overrides applied on top of the shipped default `env_data.json` parameters, resolved by `resolve_scenario()` (`R/scenario.R`). Two are compared here, `moderate_intensity` (Falklands 1982 exemplar) and `high_intensity` (Okinawa exemplar), both defined in the `scenarios` block of `env_data.json`. A third, Vietnam-calibrated profile is missing for want of sources: FORECAS's Appendix A has no standalone Vietnam combat-troop WIA/KIA distribution table, Table A.5 covering Vietnam DNBI only [[1]](#references), so there are no genuinely FORECAS-sourced Vietnam parameters to build one from.

Each profile's casualty-generation parameters, meaning its arrival-rate distributions, its priority-severity mix and its died-of-wounds calibration, are fitted to a named historical campaign, though how completely varies by profile and by parameter. The died-of-wounds ceilings are fitted to a treated-cohort mortality rate reported for the profile's own campaign, measured over casualties who reached a treatment facility alive rather than over all wounded (see the README's [Parameter Calibration](../README.md#parameter-calibration)): `moderate_intensity` to the Falklands figure, `high_intensity` to the rate the US Army reported on Okinawa [[2]](#references). What `high_intensity` still takes from the Falklands-calibrated base is its priority split, its DNBI composition and its transport times.

Both profiles use the same health system establishment, the simulation's shipped default: a representative combat brigade served by three Role 1 treatment teams, two Role 2 Basic (R2B) facilities and one Role 2 Enhanced Heavy (R2E Heavy) hospital. Element, bed and transport fleet counts are structural configuration and cannot be overridden by a scenario, so the two profiles differ in their casualty-generation parameters alone. That is what makes the comparison a test of one establishment under two casualty intensities rather than a comparison of two health systems.

### Run Configuration and Provenance

Every experiment below opens with its own design statement, naming its replication count, its control seed and any parameter set away from the shipped default. The comparative scenario analysis is invoked as:

```
Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity --iterations 50 --days 30 --seed 42
```

The figures in that comparison, and `images/scenario_comparison.png` with them, were produced in the project's pinned development container (`rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`), so none carries a caveat about the environment it was produced in. The seed-42 evidence set the single-run document reports reproduces byte for byte in that same container, and `scripts/check_baseline_reproduction.R` re-checks the reproduction whenever the model changes.

One feature of the comparison is worth stating, because it is easily mistaken for a defect. `moderate_intensity` gives 437.8 total casualties per run, while the documented seed-42 single run produces 530 (`CLAUDE.md` Key Parameters). The single run sits inside this profile's own 10th-to-90th-percentile range of 362.7 to 528.0 rather than near its mean, which is what one draw from a wide distribution does. That the comparative runner does reproduce the base configuration, and so honours the scenario mechanism's guarantee of changing nothing it does not name, is established directly instead: `moderate_intensity` overrides casualty-generation parameters only, and every casualty, mortality and queue figure in the tables below reproduces exactly when the comparison is re-run at this seed, inside the pinned container and outside it alike.

---
## Where the System Fails First

<small>[Return to Top](#contents)</small>

**The establishment that copes at Falklands-equivalent casualty rates does not cope at Okinawa intensity, and it gives way at the R2E operating theatres before anywhere else.** Casualty volume between the two profiles rises by a factor of 2.33 while the R2E theatre queue rises by a factor of about 36, so what the system does under load is not proportional to the load put on it. A planner reading only the casualty ratio would size the system by a factor that understates the surgical requirement fifteen-fold.

### Comparative Scenario Analysis

**Design.** 50 replications of 30 simulated days per profile at control seed 42, under the shipped default establishment, the only overrides being those the scenario profile itself applies (see [Scenario Profiles](#scenario-profiles) and [Run Configuration and Provenance](#run-configuration-and-provenance)).

#### Casualty and Mortality Totals

| Metric | `moderate_intensity` (Falklands) | `high_intensity` (Okinawa) | Ratio |
|---|---|---|---|
| Total casualties/run | 437.8, 95% CI [421.0, 454.7] (p10–p90: 362.7–528.0) | 1,021.0, 95% CI [993.9, 1,048.1] (p10–p90: 906.5–1,138.5) | 2.33× |
| WIA/run | 188.7, 95% CI [175.4, 202.0] (p10–p90: 137.6–251.5) | 684.3, 95% CI [658.3, 710.3] (p10–p90: 586.2–792.5) | 3.63× |
| DOW/run | 0.78, 95% CI [0.55, 1.01] (p10–p90: 0–2.0) | 23.58, 95% CI [21.88, 25.28] (p10–p90: 18.0–32.1) | 30.2× |
| DOW/WIA rate | 0.42%, 95% CI [0.29%, 0.54%] (p10–p90: 0–1.02%) | 3.43%, 95% CI [3.24%, 3.61%] (p10–p90: 2.63%–4.22%) | 8.24× |

The casualty rows carry a wide p10 to p90 spread because each arrival stream draws its rate once per simulated day from the distribution its configuration names, then places that day's arrivals within the day by thinning [[14]](#references). The between-day standard deviation the source reports therefore reaches the output rather than being averaged away (see the README's [Casualty Generation](../README.md#casualty-generation)). Total casualties per `moderate_intensity` run span 362.7 to 528.0 between those percentiles against a mean of 437.8, a spread of roughly a third of the mean. A planner sizing against the mean of either profile is sizing against a day the campaign will frequently exceed.

The died-of-wounds rows measure more than the health system. Each profile carries the mortality model of the campaign its casualty rates come from, `high_intensity` having its own Okinawa calibration rather than one inherited from the Falklands-calibrated base (see [Scenario Profiles](#scenario-profiles)). The 30-fold ratio between the profiles therefore compares two campaigns' mortality experience as well as two casualty volumes, and this table cannot separate the two. No planning case should rest on that ratio as a measure of what surge alone costs.

The two rows are also resolved very differently. The `moderate_intensity` figure is one 50-replication measurement of a response averaging well under a single death per replication, which 50 replications pin down only to roughly ±0.13 percentage points (see [Replication Count and Resolution](#replication-count-and-resolution)). It moves between control seeds by about as much as its own interval spans, so the figure to quote for that profile is the 150-replication pooled one in the README's [Scenario Profiles](../README.md#scenario-profiles), 0.368% (95% CI [0.310%, 0.426%]) on the treated cohort. The `high_intensity` figure rests on some 24 deaths per replication and is correspondingly well resolved, its 3.43% sitting inside the 3.471% (95% CI [3.360%, 3.583%]) the same pooling gives. Both intervals are correctly specified, the replications behind them being independent of one another (see [Replication Independence](#replication-independence)). The queue rows below are better resolved than either at the same replication count, being time-weighted occupancy measures with far more events behind each replication.

#### Resource Queue Comparison (mean of per-resource mean queue, by group)

| Resource group | `moderate_intensity` mean queue (95% CI) | `high_intensity` mean queue (95% CI) | Ratio |
|---|---|---|---|
| R2B OT | 0.000 [0.000, 0.000] | 0.000 [0.000, 0.000] | not applicable |
| R2B Hold | 0.593 [0.501, 0.685] | 3.228 [3.005, 3.452] | 5.45× |
| R2E OT | 1.063 [0.691, 1.435] | 38.17 [34.01, 42.33] | 35.9× |
| R2E ICU | 0.131 [0.104, 0.159] | 0.564 [0.464, 0.664] | 4.29× |
| R2E Hold | 0.598 [0.437, 0.758] | 2.694 [2.449, 2.938] | 4.51× |
| Transport (PMV Ambulance / HX240M) | 0.0038 [0.0000, 0.0078] | 0.0278 [0.0196, 0.0361] | 7.25× (small in both) |

Each cell is the mean across the 50 replications of that replication's mean queue over the group's resources, with the Student $t$ interval of [Confidence Intervals](#confidence-intervals) on that mean. A resource idle throughout a replication contributes a zero rather than dropping out of that replication's average, so the denominator is the group's full establishment in every replication. The R2B theatre interval collapses to a point because every replication of both profiles returns exactly zero, which is a property of the bypass routing rather than of the estimate (see [Bypass Routing Hides the Forward Shortfall](#bypass-routing-hides-the-forward-shortfall)). Transport is the one group whose lower bound truncates at zero.

![Four-panel bar chart of mean queue length by resource group, R2B OT, R2E OT, R2E ICU and transport, each panel comparing the high intensity and moderate intensity profiles with error bars, on four different vertical scales](../images/scenario_comparison.png)

Each panel carries its own vertical scale, so the panels compare profiles rather than resources: the R2E theatre panel runs to 60 casualties while the transport panel runs to 0.07. Both bars in the R2B theatre panel sit exactly at zero. The error bars show the mean of the per-resource p10 to p90 range across replications, not a confidence interval, and every high intensity bar is wide enough to show that the surge queues vary a great deal from replication to replication.

### The Binding Constraint: the R2E Surgical Roster

**The comparison exposes a structural weakness the single-run baseline could not surface on its own, and it locates that weakness in the operating theatres.** The mean R2E theatre queue rises from 1.06 casualties at Falklands-equivalent load to 38.2 under `high_intensity`, a factor of roughly 36 and by a wide margin the largest movement anywhere in the model. **Evidence: measured.**

**The mechanism is the surgical roster rather than theatre space, and that distinction decides which remedy works.** A casualty takes a theatre before taking one of the three surgical sections that staff them, so a room reads as queued while its occupant waits for people, and at Okinawa-intensity arrival rates that wait dominates. The single-run walk-through separates the two directly at Falklands load: the 212 procedures performed there consume about 30% of the two theatres' combined availability against room occupancy figures of 66.6% and 52.8%, so most of what the rooms report is a casualty holding a theatre while waiting for staff (see [R2E Heavy Handling](Single_Run_Analysis.md#r2e-heavy-handling)). A planner who reads the theatre queue as a demand for rooms will buy the wrong thing.

**Contention at the theatres is a standing property of this establishment, not a peer-conflict phenomenon.** The Falklands-load figure is 1.06 casualties rather than zero, so an arrival process delivering genuine heavy days produces theatre contention at moderate rates too. Okinawa intensity makes acute a constraint the establishment already carries on its own heavy days, which matters to a planner because it moves the remedy from a contingency measure to a standing one.

The rest of R2E follows the theatres rather than leading them. R2E intensive care rises from 0.131 to 0.564, a factor of 4.3, and is the flattest of the three R2E groups under surge: only the damage control cohort takes a stabilisation episode, so intensive care carries one episode for half the surgical population and two for the other half rather than two for everyone. R2E holding beds rise from 0.598 to 2.69, a factor of 4.5, absorbing what intensive care does not, since a holding bed is where a casualty goes when no intensive care bed is free and where those awaiting strategic evacuation wait.

**The died-of-wounds rate as a share of WIA rises from 0.42% to 3.43%, a factor of roughly 8, on intervals that do not overlap.** What that factor measures needs saying plainly, because this is the one row here that is not about the modelled health system alone. Each profile carries the mortality model of the campaign its casualty rates come from, so the gap between the rows combines three things: Okinawa's heavier casualty volume, the treatment queues that volume produces, and a standard of surgical and resuscitative care four decades older than the Falklands profile's. The first two belong to this comparison. The third belongs to the calibration and would show up even at equal casualty volumes, so a planner reading the ratio as the cost of surge alone would overstate it substantially. What survives without qualification is the mechanism the queue rows establish: at Okinawa intensity, deaths occur against treatment queues deep enough that removing an intensive care episode and a theatre episode from half the operated population, which is what the surgical pathway split does, does not relieve them.

---
## Planning Options in Priority Order

<small>[Return to Top](#contents)</small>

**Five options are set out below, ordered by how directly each bears on the health outcome of the force and, where that is comparable, by the strength of the evidence behind it.** The ordering principle matters as much as the order: an option that addresses the binding constraint ranks above one that is better measured but acts somewhere the system is not failing. A planner who inverts that, spending where the evidence is cleanest rather than where the system gives way, buys precision instead of capability. Each option states its evidence label against a specific claim, as [Reading the Evidence Labels](#reading-the-evidence-labels) sets out, and the summary below is the whole set at a glance.

| Priority | Option | What the evidence establishes |
|---|---|---|
| 1 | Deepen the R2E surgical roster | Diagnosis **measured**; remedy **untested**, needing a clinician fatigue model to cost |
| 2 | Relieve forward holding capacity at R2B | Diagnosis **measured** at both intensities; the three remedies **untested** under replication |
| 3 | Hold casualties forward for a reopening section | **Measured** on casualties held and bypasses avoided; **unresolved** on forward surgeries gained |
| 4 | Sustain the force with a reinforcement demand cycle | **Measured** and effective at `high_intensity`; no detectable effect at `moderate_intensity` |
| 5 | Take risk on the transport fleet | **Measured**: margin holds to two of three PMV Ambulances, collapses at one |
| Not recommended | Move post-operative intensive care forward to R2B | **Unresolved** across the whole sweep, with no benefit visible at any setting |

### Option 1. Deepen the R2E Surgical Roster

**This is the change the evidence points at most directly, and the one this model cannot yet cost.** The diagnosis is measured: the R2E theatre queue rises about 36-fold between the profiles, and the mechanism is a casualty holding a room while waiting for staff rather than a shortage of rooms (see [The Binding Constraint](#the-binding-constraint-the-r2e-surgical-roster)). The remedy that follows is more surgical section time at R2E, and it can take either of two forms, neither of which the model can presently evaluate.

**A second section rostered to the night shift** is the form the single-run evidence points at. Three sections cover a 24-hour day on 12-hour rosters, so two are on during the first shift and one during the second, and it is the second-shift section that carries the whole night-time load alone: it is busy for 53.6% of its open time against 30.8% for each first-shift section, and queued for 2.45% of it against 0.67% and 0.60% (see [R2E Heavy Handling](Single_Run_Analysis.md#r2e-heavy-handling)). A fourth section rostered to the night shift would buy more throughput than a third operating theatre, because rooms are not what is scarce. **Evidence: untested.** The model fixes element and team counts as structural configuration that a scenario profile cannot override (see [Scenario Profiles](#scenario-profiles)), so establishing what a fourth section buys needs the establishment itself made configurable, which the individual resource modelling work would deliver (README Further Development entry L3).

**Extending the existing sections' shifts** is the cheaper form and the one this project declines to evaluate on principle. Longer shifts cannot be assessed fairly without a model of clinician fatigue and the errors and complications that come with it, and reporting the extra throughput without that counterweight would overstate what the change is worth. **Evidence: untested**, and untestable until a fatigue model exists (see [Research and Development Agenda](#research-and-development-agenda)).

What a planner can take from this now is the ranking rather than the size: whatever surgical capacity is added at R2E should be added as staff time, not as theatre space, and the case for it does not depend on assuming a peer-intensity fight, the contention being present at Falklands rates already.

### Option 2. Relieve Forward Holding Capacity at R2B

**Forward holding is the second constraint, and unlike the theatres it is saturated by the establishment's own arithmetic rather than only by surge.** Under replication the R2B holding queue rises 5.45-fold between the profiles, from 0.593 to 3.228, the second largest movement in the queue table. **Evidence: measured.** What drives the rise is the proportional increase in non-surgical WIA volume rather than any change to disease and non-battle injury, whose generation rate a scenario profile does not override.

The structural case is made in the single-run document and is not repeated here: the configured means imply about 15.5 hold beds in use against the 10 fielded, an overload of 55% that no change to surgical throughput can close, and disease is the stream that fills them (see [R2B Hold Bed Saturation](Single_Run_Analysis.md#r2b-hold-bed-saturation-stream-decomposition-and-intervention-analysis)). The replicated queue figure above is the same shortfall seen at a second casualty intensity, which is what establishes it as a property of the establishment rather than of one campaign's draw.

Three remedies are open to a planner, and the single-run arithmetic rules one of them out and ranks the other two. Shortening the hold cannot reach inside capacity at any clinically plausible stay length. Adding beds works, at 10 per unit rather than the 5 fielded. An evacuation threshold from R2B holding is the cheapest, moving roughly 85% of hold patients rearward at the price of transferring a non-surgical medical load onto R2E holding and intensive care, which is a transfer onto the echelon Option 1 has already identified as the binding one. **Evidence: untested under replication.** Each is a configuration change the model can make, none has been swept at any replication count, and the third interacts with Option 1 in a way only a joint sweep would expose. That sweep is the single most tractable piece of outstanding work in this document, needing no new model structure at all.

### Option 3. Hold Casualties Forward for a Reopening Section

**The window keeps about six casualties forward per run, which is what it was added to do, and this design cannot show what those six holds buy in forward surgery.** Both halves matter to a planner: the mechanism is demonstrably live, and its value is not yet measured. Time to surgery is among the strongest determinants of survival after severe battlefield injury [[15]](#references), so a casualty kept forward rather than moved rearward is the kind of change that should pay; whether it does at this scale is exactly what the replication count cannot say.

#### The R2B Pre-Open Hold Window

R2B fields one surgical section per team on a 12-hour shift against a theatre available around the clock, so for half of each day the theatre stands ready with nobody rostered to operate in it. `r2b.surgery.pre_open_window_min` sets how long a casualty arriving in that half may be held forward for the section instead of being sent to R2E (see the README's [R2B Trajectory](../README.md#r2b-trajectory)). The window ships at 60 minutes and has no source behind it (see the README's Further Development entry L28), so what it achieves is a question to be measured rather than argued. One run cannot answer it either: turning the window on shifts simmer's single global random stream, so a zero-window run and a 60-minute run at the same seed are two different realisations rather than a controlled comparison.

**Design.** 50 replications of 30 simulated days per arm at control seed 42, under the shipped default configuration with one override, `r2b.surgery.pre_open_window_min` set to 0 in one arm against its shipped 60 in the other, run in the project's pinned Dev Container. The first two columns are per-replication means; the third is the mean of the per-replication paired difference with its 95% confidence interval, which is the quantity the comparison turns on.

| Measure | Window 0 (instant diversion) | Window 60 min (shipped) | Difference (95% CI) |
| --- | --- | --- | --- |
| Casualties held forward | 0 | 5.90 | +5.90 [+5.18, +6.62] |
| R2B surgeries | 51.82 | 52.20 | +0.38 [−2.75, +3.51] |
| Bypassed, section off shift | 84.94 | 75.24 | −9.70 [−17.25, −2.15] |
| Bypassed, theatre busy or queued | 19.76 | 17.08 | −2.68 [−6.95, +1.59] |
| R2E first surgeries | 125.16 | 117.96 | −7.20 [−16.33, +1.93] |
| R2E theatre entry deferred (ICU full) | 18.94 | 15.62 | −3.32 [−6.56, −0.08] |
| Died of wounds per run | 1.02 | 1.02 | +0.00 [−0.38, +0.38] |
| R2B OT utilisation (24-hour room) | 7.0% | 7.2% | not applicable |
| Total casualties | 442.82 | 433.18 | −9.64 [−32.00, +12.72] |

**On what the window was added to do, the measurement is decisive.** It holds 5.90 casualties forward per run (95% CI [5.18, 6.62]), where a zero window holds none by construction, and the off-shift bypass count falls by 9.70 ([−17.25, −2.15]). Neither interval includes zero. Casualties that would have been sent rearward are kept forward instead. **Evidence: measured.**

**What the measurement cannot establish is the accounting behind that:** whether forward surgeries rise by the number held forward and by nothing else. Forward surgeries move by +0.38 ([−2.75, +3.51]), an interval that comfortably contains zero and just as comfortably contains the +5.90 the holds would predict. It cannot tell those two possibilities apart, and should not be read as evidence for either. **Evidence: unresolved.**

That is a limit on what this design can resolve, not a finding about the window, and the reason is that the two arms are not the same realisation. A zero-window run and a 60-minute run at the same control seed share their per-replication seeds, but the first hold shifts simmer's single global stream, and the force-regeneration loop feeds arrival timing back from casualty event timing (see the README's [Force Regeneration and the Endogenous Feedback Loop](../README.md#6-force-regeneration-and-the-endogenous-feedback-loop)), so the two arms drift into different casualty streams. Not one of the 50 replication pairs generated the same number of casualties in both arms, and the paired difference in total casualties spans −32.00 to +12.72. Pairing on the seed therefore removes none of the between-run variance, and an effect of about six operations disappears into the noise of a response whose paired difference has a standard deviation of 11.

Those standard deviations set the replication count this comparison would need. Resolving the forward-surgery effect to a half-width of two operations takes about 120 replications per arm. The bypass and R2E surgery rows, whose paired differences carry standard deviations of 27 and 32, would take several hundred to a few thousand. Neither count has been run, so the forward-surgery, bypass-composition and R2E-surgery rows above are reported as unresolved rather than as effects. Re-running at those counts, or finding a variance reduction design that survives a stream shift, is the outstanding work here.

**Two rows are still worth reading.** Theatre entry at R2E deferred for a full intensive care unit falls by 3.32 ([−6.56, −0.08]), an interval that excludes zero and points the way the mechanism predicts: operating earlier on the casualties the window reaches relieves a little rearward pressure, which is pressure on the echelon Option 1 identifies as binding. And mortality is flat, deaths of wounds per run differing by 0.00 ([−0.38, +0.38]), with the treated-cohort rate at 0.37% and 0.39%, both under the roughly 0.46% Ajax Bay bound the project's one-sided calibration check applies. That is a null result at this replication count rather than a demonstration that the window costs no lives, deaths of wounds being the rarest response in the table.

Two further limits apply to the design itself. The comparison was run at the shipped default configuration only, so it says nothing about the window under surge, where the forward theatre is contended and displacing one casualty with another would be likelier to bite. And 60 minutes is a single point on a range the screening bounds take from zero to six hours, so this shows that the shipped value drives the mechanism it was added for, not that it is the value that pays best. Sweeping the window across that range is the second piece of tractable outstanding work here.

### Option 4. Sustain the Force with a Reinforcement Demand Cycle

**This is the one lever below that is both measured and effective, and it acts on the force rather than on the health system.** Depletion is measurable only at Okinawa intensity, where a 7-day reinforcement demand cycle removes it entirely; 30 days of Falklands-rate attrition does not deplete the force enough to show a trend at all. A planner should read it as a force-generation decision that the health system's output depends on, rather than as a medical capability.

#### Force Regeneration Under Reinforcement

Casualties in this model are generated against the force actually present at the time rather than against a fixed roll strength, so sustained losses should reduce daily casualty volume as the force depletes, and an active reinforcement cycle should offset that decline (see the README's [Force Regeneration and the Endogenous Feedback Loop](../README.md#6-force-regeneration-and-the-endogenous-feedback-loop)). The seed-42 walk-through shows the depletion curve for one campaign (see [Force Regeneration Feedback Loop](Single_Run_Analysis.md#force-regeneration-feedback-loop)). A slope fitted to daily volume is a regression on a noisy series, and needs replication before it can be read.

**Design.** 15 replications per row at `moderate_intensity` and 12 at `high_intensity`, each of 30 simulated days, with daily casualty volume averaged across replications and fitted with an ordinary least-squares trend against simulation day. The unreinforced rows use the shipped default (`force_regeneration.reinforcement.demand_interval_days = 0`, which disables the mechanism). The reinforced rows override it with a 7-day demand submission cycle, a 7-day fulfillment lag and the shipped default triangular fill distribution (`fill_min_frac = 0.2`, `fill_mode_frac = 0.85`, `fill_max_frac = 1.1`).

| Scenario | Reinforcement | Daily volume slope | p-value | First-week mean | Last-week mean |
|---|---|---|---|---|---|
| `moderate_intensity` (15 reps) | None | −0.018/day | 0.75 | 14.9 | 14.4 |
| `moderate_intensity` (15 reps) | 7-day demand cycle, 7-day lag | −0.103/day | 0.17 | 16.6 | 14.1 |
| `high_intensity` (12 reps) | None | −0.349/day | 0.0027 | 39.8 | 29.5 |
| `high_intensity` (12 reps) | 7-day demand cycle, 7-day lag | +0.030/day | 0.80 | 35.8 | 36.1 |

All four rows were measured in the pinned Dev Container, the `high_intensity` rows including that profile's own died-of-wounds calibration.

**At `high_intensity` casualty rates the mechanism is clear.** Daily volume falls significantly without reinforcement, declining 26% from the first week to the last at a slope of −0.349/day (p = 0.0027), and the demand-cycle configuration removes that decline entirely, leaving a slope indistinguishable from flat (+0.030/day, p = 0.80, under 1% change from first week to last). **Evidence: measured.** Reinforcement halts the depletion without overshooting into growth, which follows directly from the demand-based design: each cycle asks for the pool's actual current shortfall rather than a fixed number, so a well-sustained pool asks for less next time. The daily counts the regression is fitted to are themselves noisy, each carrying the between-day variation the arrival process delivers, so even a steep slope is established at moderate rather than overwhelming confidence. At `moderate_intensity` neither configuration shows a slope distinguishable from flat, that profile's casualty volume being too low for 30 days of attrition to deplete the force measurably.

One planning implication runs the other way from the obvious one. A reinforced force sustains its casualty *production*, so the health system serving it faces a load that does not taper as the campaign proceeds. Reinforcement is therefore a decision that raises the medical requirement at the same time as it sustains combat power, and the two should be planned together rather than in sequence.

`force_regeneration.reinforcement` (`env_data.json`) is entirely for the planner to set, covering the demand cycle, the fulfillment lag and all three triangular fill parameters, and this project makes no attempt to balance it automatically against a scenario's attrition rate. The 7-day/7-day configuration above illustrates the mechanism; it is not a recommended operational setting.

The table shows the mechanism's direction and its statistical behaviour across replications. It does not replace the seed-42 baseline figures, which are reported in the [Force Regeneration Feedback Loop](Single_Run_Analysis.md#force-regeneration-feedback-loop) walk-through and in `CLAUDE.md`.

### Option 5. Take Risk on the Transport Fleet

**Transport is where a planner can release resource rather than spend it.** The fleet holds its margin down to two PMV Ambulances and loses it at one, so the shipped three-vehicle establishment sits on the flat part of the curve rather than at its bend. **Evidence: measured.** Against every other echelon in this document, which is either constrained or saturated, transport is the one place a planner can take lift away to pay for capability elsewhere, provided they accept that the margin is a margin rather than an absence of demand.

#### Transport Fleet-Size Sweep

The seed-42 walk-through shows that the shipped three-vehicle PMV Ambulance and four-vehicle HX240M pools carry plenty of headroom, the PMV Ambulance pool queueing only briefly and the HX240M pool not at all (see [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin)). What one run cannot say is where that headroom ends. This sweep varies the number of vehicles directly, rather than the casualty rate or the transport duration, and so finds the fleet size at which transport becomes the binding constraint.

**Design.** 10 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: the PMV Ambulance fleet swept across 1 to 5 vehicles and the HX240M fleet across 1 to 4, each with the other fleet held at its shipped establishment size.

`plot_transport_capacity_margin_by_fleet_size()` (`R/analysis.R`) rebuilds the environment at each sweep point via `build_environment()` and runs the same replication engine (`run_replications()`, `R/replication.R`) the comparative scenario runner uses. The sweep was run via `Rscript scripts/run_transport_sweep.R`:

![Four-panel line plot of mean queue and mean utilisation against fleet size for the PMV Ambulance and HX240M fleets, each line with a 95% confidence ribbon and a dashed vertical line marking the current establishment size](../images/transport_capacity_margin_by_fleet_size.png)

The PMV Ambulance queue collapses between one and two vehicles and is flat after that, so the shipped establishment of three sits on the flat part of the curve rather than at its bend. Utilisation rises again beyond three vehicles because the measure averages across vehicles in a pool that is rarely fully engaged.

| Fleet size | PMV Ambulance mean queue (95% CI) | PMV Ambulance mean utilisation | HX240M mean queue (95% CI) | HX240M mean utilisation |
|---|---|---|---|---|
| 1 | 2.1060 (0.2270–3.9850) | 38.7% | 0.0442 (0.0000–0.1021) | 10.7% |
| 2 | 0.0487 (0.0000–0.0974) | 18.5% | 0.0011 (0.0000–0.0022) | 5.5% |
| 3 (current PMV) | 0.0068 (0.0000–0.0155) | 12.5% | 0.0001 (0.0000–0.0002) | 11.1% |
| 4 (current HX240M) | 0.0006 (0.0000–0.0012) | 14.6% | 0.0000 | 14.6% |
| 5 | 0.0001 (0.0000–0.0001) | 17.8% | not swept | not swept |

**At a single vehicle the PMV Ambulance fleet queues heavily,** at a mean of 2.11 casualties, so the sweep locates the capacity boundary sharply rather than merely confirming the always-zero finding at the current size. The queue falls by roughly a factor of forty at two vehicles and by a further factor of seven at the current three, where it is small but not exactly zero, at 0.0068 casualties. The margin is wide, and the fleet carries more headroom than one extra vehicle would supply, but it is a margin rather than an absence of demand. The row below says the same: cutting from three PMV Ambulances to two would raise the mean queue from 0.0068 to 0.0487, both small, neither zero. The HX240M pool behaves the same way an order of magnitude lower, reaching zero only at its current four vehicles. What produces a queue at all is the day-to-day variation in casualty volume rather than the average volume, a transport queue forming on peak days and on no others (see the README's [Casualty Generation](../README.md#casualty-generation)). The seed-42 walk-through shows the same thing within one campaign, its PMV Ambulance pool queueing briefly (see [Transport Fleet Capacity Margin](Single_Run_Analysis.md#transport-fleet-capacity-margin)).

**Mean utilisation across the swept range is too poorly determined to read.** It runs the wrong way on both platforms, rising with fleet size where a fixed demand spread over more vehicles should lower it, and the interval on HX240M utilisation at three vehicles spans 2.3% to 19.9%. So few transport events occur per replication that the busy-time estimate at each sweep point is barely pinned down, which is what the wide 95% confidence ribbons on the utilisation panels above show. The queue column is the one to read. `outputs/transport_capacity_by_fleet_size.csv` holds the full per-point results, including the interval bounds left out of the table.

One qualification bounds how far this licence extends. The sweep was run at `moderate_intensity` only, and the comparison's surge figures, though small in absolute terms, put the transport queue up 7.25-fold under `high_intensity`. A planner taking lift away on this evidence is taking it away on Falklands-rate evidence, and re-running the sweep under surge is outstanding work in its own right (README Further Development entry L19).

### An Option That Does Not Pay: Moving Post-Operative Intensive Care Forward

**Moving post-operative stabilisation forward to R2B changes nothing this sweep can measure at Falklands-equivalent load, because the cohort the lever reaches is too small to relieve a rearward unit running above 83% occupancy.** The shipped default therefore stays at zero, and the frontier below shows where a trade-off would appear rather than how large it is. **Evidence: unresolved across the whole swept range**, which for a planning decision is a different thing from a measured absence of effect: the sweep licenses leaving the setting alone, not a claim that forward intensive care could never help.

#### Forward ICU Share Decision Frontier

A casualty's need for stabilisation is a single quantity that the forward-holding policy divides between R2B and R2E, while the post-definitive care following their definitive repair is a separate episode delivered only at R2E (see the README's [Post-Operative Stabilisation](../README.md#post-operative-stabilisation)). The stabilisation total stays the same at every setting, so sweeping the policy moves load between echelons without changing how much care is given. That is what makes it a real planning lever rather than a quiet reduction in treatment. Only damage control casualties have a stabilisation phase, so the lever acts on roughly half of operated casualties rather than all of them (see the README's [Surgical Pathway](../README.md#surgical-pathway)).

**Design.** 20 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: `r2b_icu_share` set to 0, 0.25, 0.5, 0.75 and 1.0 in turn. Point 0 is the shipped default. Run via `Rscript scripts/run_icu_share_sweep.R --iterations 20 --days 30`.

![Five stacked line plots against the share of post-operative intensive care delivered forward at R2B, from 0% to 100%, showing R2E ICU mean queue, R2B ICU utilisation, R2E ICU utilisation, the share of post-definitive care delivered in ICU, and DOW count, each with a 95% confidence ribbon and a dashed line at the shipped default of 0%](../images/r2b_icu_share_frontier.png)

Every panel moves little across the full sweep, and every confidence ribbon is wide enough to cover the whole movement, so at this replication count the lever is not resolved. The frontier shows where the trade-off would appear rather than how large it is.

| Forward ICU share | R2E ICU mean queue (95% CI) | R2B ICU utilisation | R2E ICU utilisation | Post-definitive care in ICU (95% CI) | Mean DOW per run (95% CI) |
|---|---|---|---|---|---|
| 0% (shipped) | 0.108 (0.066–0.149) | 22.4% | 87.7% | 35.5% (28.4–42.6) | 0.80 (0.35–1.25) |
| 25% | 0.080 (0.042–0.119) | 22.1% | 84.9% | 38.7% (30.4–46.9) | 1.00 (0.52–1.48) |
| 50% | 0.078 (0.028–0.129) | 14.1% | 83.4% | 41.6% (34.5–48.8) | 1.00 (0.52–1.48) |
| 75% | 0.079 (0.036–0.121) | 20.2% | 83.8% | 42.2% (31.4–52.9) | 1.10 (0.47–1.73) |
| 100% | 0.125 (0.033–0.218) | 22.7% | 83.9% | 42.0% (32.4–51.6) | 1.00 (0.25–1.75) |

With only the damage control cohort having a stabilisation phase to move, the lever stops earning its keep. Every quantity in the table is flat across the swept range. The R2E intensive care queue moves between 0.078 and 0.125 casualties with overlapping intervals and no trend, and its highest value falls at the 100% share, where the lever is meant to help most. R2E utilisation sits between 83.4% and 87.7% at every setting, and is likewise highest at the shipped zero share.

The share of casualties receiving post-definitive care in an intensive care bed is the one column that appears to move in a direction, rising from 35.5% at a zero share to 42.0% at a full one. It should not be read as a gain. Every interval in that column overlaps every other, each spanning some fourteen percentage points against a movement of seven, and the values do not rise in order. Twenty replications cannot separate a trend this small from noise on this response.

**The lever does so little because the population it acts on is small.** Roughly half of operated casualties take the single-stage pathway and have no stabilisation phase to move at all; of the rest, only those operated on forward at R2B can have any of it served forward. What remains is a cohort small enough that moving all of their stabilisation forward does not measurably relieve a unit running above 83% occupancy. R2B intensive care utilisation supports the mechanism only weakly: it reads 22.4% at a zero share, where the beds serve the evacuation wait alone, then moves between 14.1% and 22.7% in no particular order once forward holding is enabled. Too few events per replication go into that column for it to be well determined, and it should be read as showing that load moves rather than as measuring how much.

The mortality column stays unresolved. Mean deaths of wounds per run read 0.80, 1.00, 1.00, 1.10 and 1.00 across the five points, every confidence interval overlapping every other. Deaths of wounds are rare at this casualty rate, and the penalty for reduced capability applies only to the fraction of a fraction operated on forward and held there, so twenty replications cannot separate an effect this small from noise.

The shipped default therefore stays at zero. The frontier shows a lever with a real mechanism and no measurable benefit at Falklands-equivalent load, acting as it does on roughly half of operated casualties and, within that half, only on those operated on forward. It may still matter at higher casualty rates, where R2E intensive care is contended by a wider margin, and that is the experiment worth running next.

---
## What Resourcing Alone Cannot Fix

<small>[Return to Top](#contents)</small>

**Three behaviours in this model persist whatever the establishment is set to, and each changes how a planner should read the numbers the system reports.** None is a lever. They are the reasons a resourcing decision taken on the face of the output can go wrong.

### Bypass Routing Hides the Forward Shortfall

**The R2B theatre queue stays at 0 in both scenarios, and not because R2B absorbs any of the surge.** The bypass routing sends a casualty requiring surgery to R2E whenever the theatre is busy or queued, or the surgical section has been closed for longer than the pre-open hold window, rather than letting them wait. Under `high_intensity` that pushes the entire surge onto an R2E with little spare capacity to take it. The hold window caps how long a casualty may wait for a section about to reopen and so cannot produce a standing queue, which is why the queue reads zero even with the window open (see [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)).

The planning consequence is a reporting trap. A zero queue at a forward echelon is the signature of a shortfall being exported rather than of capacity being adequate, and the same displacement appears in the holding pathway, where the capacity-aware routing policy diverted 179 casualties to R2E before transport in the seed-42 campaign (see [R2B Hold Bed Saturation](Single_Run_Analysis.md#r2b-hold-bed-saturation-stream-decomposition-and-intervention-analysis)). A planner auditing this system by its queue lengths alone would conclude that R2B needs nothing and R2E needs everything, when part of what R2E needs is what R2B could not provide. Any measure of forward adequacy has to count what was sent rearward, not what waited.

### The Post-Operative Intensive Care Gate

**The gate moved a large amount of load off intensive care, and left its mortality effect unresolved at 50 replications.** The direction of that effect is nonetheless the one the design predicts, and the pathway carrying the higher risk is measurably the riskier of the two. What the gate does not do is add capacity: it makes the cost of an existing shortfall visible as a clinical pathway rather than hiding it in queue time, which is a reason to trust the output and not a reason to expect the output to improve.

A damage control casualty leaving theatre needs a period of stabilisation, and the model makes entry to theatre depend on an intensive care bed being free to provide it. A Priority 1 casualty is operated on regardless and recovers in a holding bed at raised risk when no bed is free; a Priority 2 or lower casualty waits to enter theatre until one comes free (see the README's [Post-Operative Stabilisation](../README.md#post-operative-stabilisation)). The seed-42 walk-through shows which casualties took the degraded route and on which day (see [R2E Heavy Handling](Single_Run_Analysis.md#r2e-heavy-handling)). Whether the gate's two effects, lighter intensive care load and higher mortality on the degraded route, survive beyond that one draw is a question for replication.

**Design.** 50 replications of 30 simulated days at an independent seed per replication (`seed = NULL`), under the shipped default configuration, run before and after the gate was introduced. The intervals in this section alone were computed over replications that were antithetically paired while the interval still divided by the replication count, which makes them narrower than those runs entitle them to be (see [Replication Independence](#replication-independence)). They are not recomputed, because the comparison is against a configuration that no longer exists in the codebase and the earlier arm cannot be re-run. Intervals that should be wider only strengthen this section's conclusion, which is that the two overlap and the comparison does not reach significance at 50 replications. Every other interval in this document is computed over independent replications.

Mean R2E intensive care utilisation fell from **74.1% to 60.2%**, a large reduction in load seen consistently across replications rather than an artefact of one seed. Mean deaths of wounds per run rose from **0.84 (95% CI [0.58, 1.10]) to 1.00 (95% CI [0.74, 1.26])**. Those intervals overlap heavily, so the comparison does not reach conventional significance at 50 replications, deaths of wounds being a rare event at this casualty rate. The rise is nonetheless attributable to the new post-operative checkpoint, which contributed a mean of 0.10 deaths per run on its own, in 5 replications of 50, accounting for almost the whole movement in the point estimate. **Evidence: direction only.**

**Inside the checkpoint the design behaved as intended under the shipped parameters, not only under a stress test.** The holding pathway's realised death rate, 2 in 1,223 casualties or 0.16%, was roughly **2.8 times the intensive care pathway's rate** of 3 in 5,085 or 0.06%. The higher-risk pathway is measurably riskier at baseline casualty rates, not just riskier in principle, though the small counts leave the ratio itself very uncertain and it should be read for its direction rather than its size.

The mortality mechanism was confirmed separately by a stress test that forced intensive care capacity to zero over a 90-day run. The degraded route then carries most casualties and produces measurable post-operative deaths, which establishes that the checkpoint fires as designed. It does not establish that the effect is quantitatively resolved at Falklands-calibrated rates.

For a planner the gate is a measuring instrument rather than a lever. It converts an intensive care bed shortfall into a count of casualties who received the degraded pathway, which is a more useful quantity to plan against than a queue length, because it names who bore the cost. The remedy it points at is intensive care capacity at R2E, which is the same echelon Option 1 identifies and competes for the same resource.

### Mass Casualty Event Stress Test

**An injected surge degrades the care the system delivers without revealing a constraint the background tempo was hiding, because a tempo that produces heavy days of its own has already used the forward echelons' spare capacity.** The degradation shows up in the post-operative pathway rather than in theatre or bypass counts, and it lasts the whole run rather than the event windows alone. For a planner that is the more uncomfortable of the two possible findings: there is no reserve to be found by looking harder at the peaks, because the peaks are already the design case.

The sections before this one examine sustained casualty tempo, the background lognormal and exponential streams at either Falklands or Okinawa intensity. This section tests something different in kind: a sudden casualty surge laid on top of the Falklands-calibrated background tempo. The compound Poisson injection mechanism delivers it, adding discrete mass casualty events of random size to the sustained background streams.

The casualty count drawn for an event is its total, not the number of survivors. A configured share of it, `mass_casualty.event.kia_fraction`, arrives killed at or near the point of injury and goes to the mortuary pathway rather than through triage (see the README's [Mass Casualty Event Injection](../README.md#5-mass-casualty-event-injection)). At the shipped share of 0.28, a 30-day run at this rate and seed produces 78 event-derived casualties, 27 of them killed outright and 51 wounded. An event of a given nominal size therefore loads the surgical echelons less than its drawn count suggests, while loading mortuary handling and the transport of the dead in a way the surgical figures do not show.

**Design.** 10 replications of 30 simulated days at control seed 42, under the shipped default configuration with one override: `mass_casualty.event.rate_per_day` set to 0.2 events per day, a mean of five days between events, against a background-only arm at the shipped value of 0. Injection ships disabled, so everything in this section needs that override, the illustrative single run and `images/mass_casualty_events.png` included, and none of it can be reproduced by a shipped-configuration run. That makes this figure the one tracked image `run.R --refresh-baseline` cannot write; it is copied into place from the run's own output directory.

| Metric | Background-only baseline | With mass casualty injection |
|---|---|---|
| Mean total casualties/run | 444.6 | 682.1 |
| Mean mass casualty events/run | 0 | 5.40 (range 3–8) |
| DOW rate, background-origin casualties | 0.18% (8/4,446) | 0.28% (13/4,577) |
| DOW rate, mass-casualty-origin casualties | not applicable | 0.58% (13/2,244) |

The mean of 5.40 events per 30-day run sits a little below the 6 the configured 0.2/day rate implies, a difference ten replications cannot separate from zero. Event counts range from 3 to 8 across those replications, which confirms the Poisson process is genuinely random rather than fixed. At a mean of 41.6 casualties per event, the drawn sizes sit near the middle of the configured 20 to 60 range, as a uniform draw should. Casualties from mass casualty events die of wounds at 2.1 times the background rate, 0.58% against 0.28%, consistent with the stress test's intent: a blast-dominant priority mix arriving faster than steady-state capacity can absorb. **Evidence: direction only.** Three qualifications apply. The comparison sorts casualties by origin rather than by a strict time window around each event (see the assumption note in `R/analysis.R`). Deaths of wounds are rare at this sample size, 13 in each arm, so the ratio shows a direction rather than a precise figure. And the background column is not a quiet baseline, the background stream delivering heavy days of its own, which is why its died-of-wounds rate is 0.18% rather than zero and why the gap between the arms is narrower than the injected volume alone would suggest.

One seed-42 run under the same override, with no averaging across replications, shows the mechanism directly: 537 total casualties, 459 from the background streams and 78 from two events, one of 33 casualties on day 14 and one of 45 on day 27. The gap-based reconstruction the analysis pipeline applies recovers both exactly, having nothing to merge when two events are this far apart. Its known failure mode, reading two closely spaced events as one, needs a busier event schedule than this seed produced.

**The clearest signal in that single run is what happens at the R2E intensive care gate.** Post-operative stabilisation splits `hold=85` against `icu=37` under injection, where the shipped background-only run at the same seed gives `hold=58` and `icu=79` (see the [R2E Heavy Handling](Single_Run_Analysis.md#r2e-heavy-handling) walk-through). The majority pathway flips: a cohort that mostly recovered in an intensive care bed now mostly recovers in a holding bed. That is the substitution of degraded care the gate exists to expose, and it lasts the whole 30 days rather than only the two event windows.

**The theatre and bypass measures barely move, and that is the finding.** Deferred theatre entry for Priority 2 and lower casualties with intensive care full reads 25 under injection against 29 without it, and upstream bypass from R1 reads 177 against 179. R2E theatre utilisation rises only from 24.2% to 25.7%, while R2B's falls from 5.2% to 4.9%. A background tempo that delivers heavy days of its own has already used up the forward echelons' spare capacity, so an injected event lands on a system that is intermittently saturated anyway. The surge still degrades the care delivered, as the flipped pathway split shows. What it does not do is reveal a constraint the background tempo was hiding.

![Stem plot of the two mass casualty events reconstructed from the run, each drawn as a vertical line at its simulation day with a point at its casualty count: 33 casualties midway through day 13 and 45 midway through day 26](../images/mass_casualty_events.png)

Two events thirteen days apart is a thin draw from a process configured to deliver an average of six across the run, which is why this seed illustrates the injection mechanism rather than measuring its effect. The replicated table above carries the measurement.

---

## Evidence That Does Not Yet Support a Decision

<small>[Return to Top](#contents)</small>

**Three effects above are unresolved at the replication counts run, and reporting them as unresolved rather than passing over them is itself a planning input.** A point estimate whose interval spans zero is not a small effect; it is an unmeasured one, and the distinction decides whether a planner is entitled to act on it. Each row below states what would settle it.

| Unresolved effect | Where | What it would take |
|---|---|---|
| Forward surgeries gained from the pre-open hold window | [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window) | About 120 replications per arm for a half-width of two operations |
| Bypass composition and R2E surgery counts under the window | [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window) | Several hundred to a few thousand replications per arm, or a variance reduction design surviving a stream shift |
| Every response in the forward intensive care share sweep | [Forward ICU Share Decision Frontier](#forward-icu-share-decision-frontier) | More than 20 replications per point, and a surge profile where R2E intensive care is contended by a wider margin |
| Mortality effect of the post-operative intensive care gate | [The Post-Operative Intensive Care Gate](#the-post-operative-intensive-care-gate) | A rare-event response over a cohort of a few dozen casualties, not resolved at any count this project has run |

Two of those are cheap and one is not. The pre-open window's forward-surgery row needs about 120 replications per arm, which is within reach of the existing framework and a machine the project already uses. The intensive care share sweep needs more replications and a second casualty intensity, which is a larger but still ordinary run. The gate's mortality effect is different in kind: deaths of wounds at Falklands-calibrated rates are rare enough that no replication count this project has contemplated would resolve a difference between two pathway cohorts of a few dozen casualties each, so settling it needs a different response variable rather than more computation.

---

## Research and Development Agenda

<small>[Return to Top](#contents)</small>

**The option this document ranks first is the one it cannot evaluate, which sets the agenda.** The items below are ordered by what each unblocks, and each names the decision it would let a planner take.

| Priority | What to build | Which decision it unblocks |
|---|---|---|
| 1 | A configurable R2E establishment, meaning element and team counts a scenario can override rather than structural constants (README Further Development entry L3) | Option 1: what a fourth surgical section rostered to the night shift buys, against a third theatre |
| 2 | A clinician fatigue model, representing the errors and complications that accompany extended shifts | Option 1 in its cheaper form: whether extending the existing sections' shifts is worth its clinical cost |
| 3 | A joint sweep of R2B holding capacity against the R2B evacuation threshold | Option 2: which of the two remedies to buy, given that the threshold transfers load onto the binding echelon |
| 4 | Severity-conditioned surgery durations, replacing the single distribution serving every casualty (entry L26) | Whether the theatre contention this document measures is understated on exactly the heavy days it comes from |
| 5 | A scenario profile combining one campaign's casualty rates with another's mortality model (entry L12) | Whether any part of the 8-fold died-of-wounds ratio is attributable to surge rather than to era of care |
| 6 | A transport fleet-size sweep under `high_intensity` (entry L19) | Option 5: whether the margin a planner would spend survives surge |
| 7 | A pre-open hold window sweep across its zero-to-six-hour range, at about 120 replications per point | Option 3: whether 60 minutes is the value that pays best |

Items 1 and 2 are the ones that matter most, because between them they gate the change this document ranks first and can otherwise only name. Item 3 needs no new model structure at all and is the most tractable piece of outstanding work here. The remainder refine measurements this document already reports rather than opening new decisions.

---

## Limitations

<small>[Return to Top](#contents)</small>

Four kinds of limitation bear on how the options above should be read.

The first is resolution. Three of the effects reported here cannot be separated from noise at the replication counts run, and they are collected in [Evidence That Does Not Yet Support a Decision](#evidence-that-does-not-yet-support-a-decision) with the counts they would need. Treat those rows as bounds on an effect rather than estimates of one, and do not read a point estimate whose interval spans zero as evidence of a small effect (see [Replication Count and Resolution](#replication-count-and-resolution)).

The second is that several comparisons are not controlled in the way a paired design assumes. Any parameter change that alters the order of events shifts `simmer`'s single global random stream, and the force-regeneration loop then feeds that shift back into arrival timing, so two arms run at one control seed drift into different casualty streams. Under those conditions pairing on the seed removes no variance at all, and the size of the problem is quantified where it bites hardest (see [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)). Only the scenario comparison escapes it, its two arms differing by design rather than by a small perturbation.

The third is that the scenario comparison varies two things at once. Each profile carries the mortality model of its own campaign as well as its casualty rates, so no mortality ratio between the profiles can be put down to casualty volume alone, and the tables here cannot separate the two. Separating them would need a profile combining one campaign's casualty rates with the other's mortality model, which is not among the configurations this project ships and which item 5 of the [Research and Development Agenda](#research-and-development-agenda) would supply.

The fourth is what the model does not represent. Those gaps are catalogued once, in the README's [Further Development](../README.md#further-development) section, and are not repeated here. The entries bearing most directly on this document are listed below by identifier.

| Entry | Bearing on this document |
|---|---|
| L3 | Clinical teams are taken whole, so the surgical section utilisation and theatre queue figures overstate scarcity where a procedure needs only part of a section, and understate it where staff would in practice be shared. It is also what prevents Option 1 from being evaluated. |
| L11 | The theatre and intensive care gating parameters are informed estimates, so the post-operative gate measures the mechanism those parameters encode rather than a mortality effect a planner should size against. |
| L12 | Scenario calibration is incomplete. `high_intensity` takes its priority split, DNBI composition and transport times from the Falklands-calibrated base, so only its casualty rates and its mortality model come from Okinawa. |
| L17 | One pool of R2E holding beds carries both in-theatre recovery and the strategic evacuation wait, so the R2E holding queue rows combine two separate demands. |
| L19 | The transport fleet-size sweep was run at one casualty rate, so the headroom Option 5 would spend is Falklands-rate headroom, and the comparison's surge figures do not substitute for sweeping the fleet under surge. |
| L22 | The died-of-wounds calibration target is a bounded treated-cohort rate, so a mortality figure that agrees with it is consistent with the anchor rather than validated against it. |
| L26 | A single surgery duration distribution serves every casualty whatever their severity, so theatre contention is understated on exactly the heavy, high-acuity days the surge queue figures come from. |
| L28 | The pre-open hold window has no source behind it, so Option 3 shows what the shipped value does rather than that the shipped value is the right one. |

Above all of these sits the limitation the [Introduction](#introduction) states: these are options inside a model that has been verified against its specification but not validated against the real system. They are a way of ordering a planner's judgement, not a substitute for it.

---

## Conclusion

<small>[Return to Top](#contents)</small>

**The establishment does not scale to Okinawa intensity, and the single change the evidence points at most directly is the one this model cannot yet cost.** The R2E operating theatre queue rises about 36-fold against a 2.33-fold rise in casualty volume, the R2B holding queue about 5-fold and the R2E holding queue about 4.5-fold, while the R2B theatre queue stays at zero throughout only because bypass routing pushes forward surgical overflow onto an already saturated R2E. The mechanism behind the theatre queue is the surgical roster rather than theatre space, so the capacity to add at R2E is staff time. That contention is present at Falklands rates too, which moves the case for adding it from a contingency measure to a standing one.

**Of the options the model can evaluate, the ranking is clear and the margins are not.** A reinforcement demand cycle is the one lever that is both measured and effective, removing the force-depletion trend entirely at Okinawa intensity, though a sustained force also sustains the medical load it generates and the two decisions belong together. Forward holding at R2B needs relief on the evidence of both intensities, and which of its three remedies to buy is the most tractable open question here, needing no new model structure. The pre-open hold window demonstrably keeps about six casualties forward per run, while what those holds buy in forward surgery stays unresolved. Transport is where a planner can take risk, holding its margin down to two of three PMV Ambulances at Falklands rates. Moving post-operative intensive care forward to R2B pays nothing measurable and should stay switched off.

**Three effects remain unresolved, and one of them will not be settled by more computation.** The pre-open window's surgical benefit and the forward intensive care share both need larger runs that the existing framework could deliver. The post-operative gate's mortality effect is a rare event measured over cohorts of a few dozen casualties, and no replication count this project has contemplated would resolve it, so it needs a different response variable instead. Reporting these as unmeasured rather than as small is the difference between a planner declining to act and a planner acting on noise.

What the model cannot reach at all is set out in the [Research and Development Agenda](#research-and-development-agenda), and its first two items gate the option ranked first. Until a configurable establishment and a clinician fatigue model exist, deepening the R2E surgical roster can be argued from the mechanism this document measures but not costed against the alternatives. A comparable Vietnam-intensity comparison waits on a genuine FORECAS-sourced Vietnam combat-troop WIA/KIA table (see [Scenario Profiles](#scenario-profiles)).

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Blood, C. G., Zouris, J. M., & Rotblatt, D. (1998). *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

[2] Marble, S. (2025). Both joint and not: Medical support at Okinawa, 1945. *Joint Force Quarterly*, *117*(2), article 11. National Defense University Press. Retrieved 17 Aug 26, from https://digitalcommons.ndu.edu/joint-force-quarterly/vol117/iss2/11/

[3] Remondelli, M. H., Remick, K. N., Shackelford, S. A., Gurney, J. M., Pamplin, J. C., Polk, T. M., Potter, B. K., & Holt, D. B. (2023). Casualty care implications of large-scale combat operations. *Journal of Trauma and Acute Care Surgery*, *95*(2S), S180–S184. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC10389308/

[4] Fandre, M. (2020). Medical changes needed for large-scale combat operations: observations from Mission Command Training Program warfighter exercises. *Military Review*. Retrieved 27 Aug 26, from https://www.armyupress.army.mil/Journals/Military-Review/English-Edition-Archives/May-June-2020/Fandre-Medical-Changes/

[5] Sargent, R. G. (2010). Verification and validation of simulation models. In *Proceedings of the 2010 Winter Simulation Conference* (pp. 166–183). IEEE. Retrieved 27 Aug 26, from https://www.informs-sim.org/wsc10papers/016.pdf

[6] Ucar, I., Smeets, B., & Azcorra, A. (2019). simmer: Discrete-Event Simulation for R. *Journal of Statistical Software*, *90*(2), 1–30. Retrieved 27 Aug 26, from https://doi.org/10.18637/jss.v090.i02

[7] NATO Standardization Office. (2019). *AJP-4.10 Allied Joint Doctrine for Medical Support* (Edition C, Version 1). NATO Standardization Office. Retrieved 27 Aug 26, from https://www.coemed.org/files/stanags/01_AJP/AJP-4.10_EDC_V1_E_2228.pdf

[8] Law, A. M. (2020). Statistical analysis of simulation output data: the practical state of the art. In *Proceedings of the 2020 Winter Simulation Conference* (pp. 1117–1127). INFORMS Simulation Society. Retrieved 27 Aug 26, from https://informs-sim.org/wsc20papers/134.pdf

[9] Rossetti, M. D. *Simulation Modeling and Arena*, Chapter 5: Statistical Analysis for Infinite Horizon Simulation Models. Retrieved 27 Aug 26, from https://rossetti.github.io/RossettiArenaBook/05-Chapter5.html

[10] Karl, A., Eubank, R., Milovanovic, J., Reiser, M., & Young, D. (2014). Using RngStreams for parallel random number generation in C++ and R. *Computational Statistics*, *29*(5), 1301–1320. Open-access preprint retrieved 26 Jun 26, from https://arxiv.org/abs/1403.7645

[11] R Core Team. (2024). *RNGstreams: L'Ecuyer's RngStreams for parallel random number generation*. R Documentation, parallel package. Retrieved 26 Jun 26, from https://stat.ethz.ch/R-manual/R-patched/library/parallel/html/RngStream.html

[12] Rossetti, M. D. (2023). *Simulation Modeling using the Kotlin Simulation Library (KSL)*, including section 9.2, Variance Reduction Techniques. Retrieved 26 Jun 26, from https://rossetti.github.io/KSLBook/

[13] Williams, E., Szakmany, T., Spernaes, I., Muthuswamy, B., & Holborn, P. (2020). Discrete-event simulation modeling of critical care flow: new hospital, old challenges. *Critical Care Explorations*, *2*(9), e0174. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC7491890/

[14] Lewis, P. A. W., & Shedler, G. S. (1979). Simulation of nonhomogeneous Poisson processes by thinning. *Naval Research Logistics Quarterly*, *26*(3), 403–413. Naval Postgraduate School Calhoun repository. Retrieved 13 Aug 26, from https://calhoun.nps.edu/handle/10945/63159

[15] Kotwal, R. S., Montgomery, H. R., Kotwal, B. M., Champion, H. R., Butler, F. K., Mabry, R. L., Cain, J. S., Blackbourne, L. H., Mechler, K. K., & Holcomb, J. B. (2011). Eliminating preventable death on the battlefield. *Archives of Surgery*, *146*(12), 1350–1358. Retrieved 27 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC5832013/

<!-- REFERENCES END -->
