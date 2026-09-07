# Experimental Design and Statistical Method for a Replicated Simulation of the Land-Based Trauma System

## Abstract

<small>[Return to Top](#contents)</small>

**Background**

A simulation result is only as good as the design that produced it. The replicated experiments reported for this trauma system model [[1]](#references) each rest on a run count, a control seed, a set of parameter overrides and a choice of interval, and a reader who wants to reproduce or audit a result needs all four in front of them. Reporting that detail alongside each finding would obscure the findings themselves.

**Objective**

To record the full experimental design of every replicated experiment this project has run, together with the statistical basis of the intervals reported from them, so that each result in the companion paper can be reproduced, audited or extended.

**Methods**

Each experiment is described by its replication count, campaign horizon, control seed, the parameters overridden against the shipped default configuration and the command that invokes it. The replication framework beneath them is described in the same terms: how independence between replications is established, how confidence intervals are constructed, how many replications a given resolution requires, and why no warm-up period is discarded. Each property is tied to the automated check that defends it.

**Results**

Replication independence follows from construction rather than from measurement: a replication is a pure function of its seed, and each replication is given a distinct seed. Antithetic pairing was trialled as a variance reduction scheme and withdrawn, its within-pair correlation on total casualties measuring $-0.04$ (95% confidence interval $[-0.27, +0.19]$) over 75 pairs. The best determined spread available for the model, on the treated-cohort died-of-wounds rate, has a per-replication standard deviation of 0.0039, from which the replication counts required for a given half-width follow directly. The force regeneration experiment, which measures the mechanism driving casualty arrivals rather than the performance of the health system, is reported here in full.

**Conclusion**

The designs recorded here support the companion paper's findings at the precision it states and no further. Where a design cannot support a stronger claim, the reason is a run count, an uncontrolled comparison or a single control seed, and each is identified against the experiment it limits.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Introduction](#introduction)
- [The Replication Framework](#the-replication-framework)
  - [Independence of Replications](#independence-of-replications)
  - [The Withdrawn Antithetic Pairing](#the-withdrawn-antithetic-pairing)
  - [Interval Construction](#interval-construction)
  - [Replication Count and Resolution](#replication-count-and-resolution)
  - [Warm-up Classification](#warm-up-classification)
  - [The Checks That Defend These Properties](#the-checks-that-defend-these-properties)
- [Experimental Designs](#experimental-designs)
  - [Comparative Scenario Analysis](#comparative-scenario-analysis)
  - [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window)
  - [The Post-Operative Intensive Care Gate](#the-post-operative-intensive-care-gate)
  - [Forward ICU Share Decision Frontier](#forward-icu-share-decision-frontier)
  - [Transport Fleet-Size Sweep](#transport-fleet-size-sweep)
  - [Mass Casualty Event Stress Test](#mass-casualty-event-stress-test)
- [Force Regeneration Under Reinforcement](#force-regeneration-under-reinforcement)
- [Provenance](#provenance)
- [Limitations of the Designs Recorded Here](#limitations-of-the-designs-recorded-here)
- [Conclusion](#conclusion)
- [References](#references)
<!-- TOC END -->

---

## Introduction

<small>[Return to Top](#contents)</small>

This project reports its work across four documents. The first traces a single campaign end to end and verifies that the model behaves as its specification describes [[2]](#references). The second reports the replicated experiments and the planning options that follow from them [[1]](#references). Behind both sits a system reference describing how the simulation is built, what each trajectory does and which assumptions it rests on [[3]](#references). This document is the fourth, and it serves the second: it holds the experimental designs and the statistical derivations that the companion paper states the conclusions of.

The division is one of audience rather than of subject. A planner reading the companion paper needs to know that three ambulances are sufficient and that the evidence for it is a queue that collapses between one and two vehicles. A reader auditing that recommendation needs to know that it rests on ten replications per fleet size at one control seed and one casualty intensity, that the utilisation column of the same sweep is too poorly determined to read, and which script produces both. The first reader is served by omitting that detail and the second by recording it, so it is recorded here.

Nothing in this document is a finding about the performance of the trauma system. Every measured result of that kind belongs to the companion paper. The one experiment reported here in full, force regeneration under reinforcement, measures the mechanism that generates casualty arrivals rather than the system that treats them, and so informs how the simulation is constructed rather than any planning decision about the health system.

The document is presented in four parts. The first describes the replication framework every experiment runs through and the statistical basis of every interval reported from it. The second gives the design of each experiment in turn. The third reports the force regeneration experiment. The fourth records the provenance of the figures and the limitations the designs carry.

---

## The Replication Framework

<small>[Return to Top](#contents)</small>

Every experiment runs through the project's multi-run replication framework (`run_replications()`, `R/replication.R`), which executes a requested number of independent replications of the model and returns their monitoring data with a replication index attached. The model itself is built on the `simmer` discrete event simulation package for R [[4]](#references).

The unit of analysis is the replication throughout. Every response is reduced to one number per replication, such as the mean queue at a resource across the 30 days or the count of casualties taking a given pathway, before any statistic is taken across replications. Observations within one campaign are not independent of one another, so no interval in this project is ever taken across casualties or across days.

### Independence of Replications

Independence follows from how a replication is built rather than being inferred from the output. Two facts establish it.

First, `run_once()` (`R/replication.R`) is a pure function of its seed: re-running a seed reproduces its output exactly, whether or not another run happens in between, so the seed is all that distinguishes two replications and no other channel connects them. Second, `run_replications()` draws a distinct seed for each replication, taking them in one call from the caller's control seed before any replication begins. Independent seeds into a pure function give independent replications, which is a deterministic argument rather than one a finite sample of correlations could support.

Running replications in parallel preserves both facts. They are dispatched through `mclapply()` under `RNGkind("L'Ecuyer-CMRG")` with `mc.set.seed = TRUE`, which gives each worker its own substream of the MRG32k3a generator [[5]](#references). The substream spacing of $2^{76}$ [[6]](#references) makes overlap impossible at any simulation budget used here. Both dispatch paths, the parallel one and the serial fallback used at a single replication and on platforms without forking, run under the one generator, so a replication's output depends on its seed and not on the path or the core count that produced it.

The framework also captures the caller's generator kind and stream position on entry and restores both on exit. Without that, the first measurement taken in a session would draw its seeds under a different generator from the second, and a measurement would depend on what preceded it rather than on its control seed alone.

### The Withdrawn Antithetic Pairing

No variance reduction scheme sits on top of that framework. Antithetic pairing, in which each replication is paired with a second run driven by the complements of its uniform random numbers so that the two errors partly cancel [[7]](#references), was used earlier in the project and then withdrawn, because neither of the conditions it needs holds for this model.

Its reach stops at the arrival generators, since `simmer` draws service times and routing probabilities from the global stream inside its own event loop, in an order set by event timing rather than by entity. The technique also requires the response to move consistently in one direction with the input uniforms, and casualty arrivals do not, their rate being scaled by an effective force size that trajectory outcomes themselves add to and subtract from. Measurement agrees: over 75 pairs the within-pair correlation on total casualties is $-0.04$ (95% confidence interval $[-0.27, +0.19]$), worth a variance reduction of about 3% and indistinguishable from none.

One consequence reaches the companion paper. The intervals reported for the post-operative intensive care gate were computed while the pairing was still in force, over replications that were not independent, and are narrower than those runs entitle them to be. They are not recomputed, because the comparison is against a configuration that no longer exists in the codebase and the earlier arm cannot be re-run. Intervals that should be wider only strengthen that section's conclusion, which is that the two overlap.

### Interval Construction

Every confidence interval reported by this project is a Student $t$ interval on the mean across replications [[8]](#references),

$$\bar{x} \pm t_{0.975,\;n-1}\,\frac{s}{\sqrt{n}}$$

where $n$ is the replication count, $\bar{x}$ the mean across replications of the per-replication response and $s$ its sample standard deviation. No normal approximation and no bootstrap appears anywhere in the companion paper or here.

The same expression is evaluated in `summarise_replications()` (`R/replication.R`), in the comparative scenario runner (`R/scenario_runner.R`) and everywhere in the analysis pipeline that reports an interval, so an interval quoted in the paper, one shown in the Shiny console and one written to a CSV output are the same quantity computed the same way. Where a comparison sets two configurations run at the same control seed against each other, the interval is on the mean of the per-replication paired difference rather than on the difference of the two means.

Several tables in the companion paper also carry a 10th-to-90th-percentile range. That is the empirical spread across replications rather than an interval on the mean, and it is not derived from the expression above.

### Replication Count and Resolution

How many replications an experiment needs depends on the spread of its response, and the responses this model reports differ by orders of magnitude in how many events they accumulate. A mean queue integrates over every arrival and departure at a resource across 30 days. A died-of-wounds count rests on a handful of events per campaign.

The best determined spread available for this model is on the treated-cohort died-of-wounds rate, the rate among casualties reaching R2B or R2E, whose per-replication standard deviation is 0.0039 under the shipped configuration, measured over 150 replications. Setting the half-width of the interval above equal to a target and solving for $n$ gives the counts a given resolution requires.

| Target 95% half-width | Replications required |
|---|---|
| 0.15 percentage points | 29 |
| 0.10 percentage points | 62 |
| 0.05 percentage points | 237 |

The 50-replication mortality figures reported in the companion paper therefore carry roughly $\pm 0.11$ percentage points. That is enough to separate two casualty intensities whose rates differ eightfold and not enough to separate two treatment pathways within one intensity, which is why the intensive care gate comparison is reported as unresolved.

Single 50-replication measurements of one unchanged configuration span 0.132 percentage points across control seeds, which is a direct illustration of the same arithmetic and is why `scripts/check_dow_calibration.R` pools three independent measurements rather than reporting one.

### Warm-up Classification

No warm-up period is discarded from any observation window. Discarding one removes the settling-in behaviour a model shows before it reaches steady state, which is worth doing only where steady state is the quantity of interest [[9]](#references). This is a terminating simulation with a fixed campaign length and a genuinely empty start, so the opening period is part of the quantity of interest rather than a transient to be removed: a deploying health system really does start empty, and how it copes while filling is a planning question in its own right.

That classification is supported rather than assumed. The Welch graphical diagnostic, run over ten 90-day replications and reported in the system reference [[3]](#references), shows the behaviour a terminating model is expected to show, and `scripts/run_warmup.R` re-runs it on demand.

### The Checks That Defend These Properties

Each property above is asserted by a regression check that runs on every pull request, so a change to the framework that broke one would fail the gate rather than silently altering every interval the project reports.

| Property | Check |
|---|---|
| A replication is a pure function of its seed, and each replication is given a distinct seed | `scripts/check_replication_independence.R` |
| A measurement depends on its control seed alone, repeats at that seed, and leaves the caller's generator as it found it | `scripts/check_measurement_reproducibility.R` |
| A configuration error inside a sweep, a screen or the scenario runner leaves the global configuration at its pre-call values | `scripts/check_config_restore.R` |
| The analysis pipeline is idempotent and does not advance the caller's random number stream | `scripts/check_analysis_idempotence.R` |

---

## Experimental Designs

<small>[Return to Top](#contents)</small>

Each design below states its replication count, its campaign horizon, its control seed and the parameters it overrides. Three conventions hold throughout unless a design says otherwise. Every run is a complete 30-day campaign from an empty system. Every run uses the shipped default establishment, the element, bed, team and transport fleet counts defined in the `elms` structure of `env_data.json`, which no scenario profile can override. And every arm of a comparison differs from the shipped default configuration in the named parameters alone, so that the difference between two arms is attributable to those parameters.

A control seed is the seed given to the framework, from which the per-replication seeds are drawn. Two arms run at the same control seed therefore share their per-replication seeds without sharing their realised casualty streams, for the reason set out under [The R2B Pre-Open Hold Window](#the-r2b-pre-open-hold-window) below.

### Comparative Scenario Analysis

50 replications of 30 simulated days per profile at control seed 42, under the shipped default establishment, the only overrides being those the scenario profile itself applies. Invoked as:

```
Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity --iterations 50 --days 30 --seed 42
```

A scenario profile is a named set of overrides applied on top of the shipped default `env_data.json` parameters, resolved by `resolve_scenario()` (`R/scenario.R`). Both profiles are defined in the `scenarios` block of `env_data.json`. Element, bed and transport fleet counts are structural configuration a scenario cannot override, so the two profiles differ in their casualty-generation parameters alone.

Both profiles draw their casualty rates from the FORECAS projection study [[10]](#references), which selects a distribution family by battle intensity rather than by named conflict: lognormal incidence at moderate intensity, exponential at high. The `moderate_intensity` profile overrides the died-of-wounds ceilings and treatment efficacy factors alone, the base configuration's casualty generation parameters already being Falklands-sourced, while the base values for those two groups are drawn from the era of tactical combat casualty care and damage control resuscitation and would misattribute modern technique to a 1982 campaign. The `high_intensity` profile overrides casualty generation and the died-of-wounds model together, the latter calibrated to reported Okinawa experience [[11]](#references), and inherits its triage priority split, disease composition and transport times from the base configuration. That inheritance is recorded in the profile's own `notes` field and is the reason the profile is described as partially calibrated.

A third, Vietnam-calibrated profile is missing for want of sources: FORECAS's Appendix A has no standalone Vietnam combat-troop wounded-in-action or killed-in-action distribution table, Table A.5 covering Vietnam disease and non-battle injury only, so there are no genuinely FORECAS-sourced Vietnam parameters to build one from.

### The R2B Pre-Open Hold Window

50 replications of 30 simulated days per arm at control seed 42, under the shipped default configuration with one override, `r2b.surgery.pre_open_window_min` set to 0 in one arm against its shipped 60 in the other.

The two arms are not the same realisation. A zero-window run and a 60-minute run at the same control seed share their per-replication seeds, but the first hold shifts `simmer`'s single global stream, and the arrival streams are force-size-reactive closures sampled by thinning [[12]](#references) whose rate the force regeneration loop feeds back from casualty event timing, so the two arms drift into different casualty streams from the first hold onward. Not one of the 50 replication pairs generated the same number of casualties in both arms, and the paired difference in total casualties spans $-32.00$ to $+12.72$. Pairing on the control seed therefore removes none of the between-run variance, and an effect of about six operations disappears into the noise of a response whose paired difference has a standard deviation of 11.

Those standard deviations set the replication count the comparison would need. Resolving the forward-surgery effect to a half-width of two operations takes about 120 replications per arm. The bypass and R2E surgery rows, whose paired differences carry standard deviations of 27 and 32, would take several hundred to a few thousand.

Two further limits apply to the design. The comparison was run at the shipped default configuration only, so it says nothing about the window under surge. And 60 minutes is a single point on a range the screening bounds take from zero to six hours.

### The Post-Operative Intensive Care Gate

50 replications of 30 simulated days at an independent seed per replication (`seed = NULL`), under the shipped default configuration, run before and after the gate was introduced. The intervals in this experiment alone were computed over antithetically paired replications while the interval still divided by the replication count, which is why they are narrower than the runs entitle them to be (see [The Withdrawn Antithetic Pairing](#the-withdrawn-antithetic-pairing) above).

The mortality mechanism was confirmed separately by a stress test that forced intensive care capacity to zero over a 90-day run. The degraded route then carries most casualties and produces measurable post-operative deaths, which establishes that the checkpoint fires as designed without establishing that the effect is quantitatively resolved at the died-of-wounds rates the moderate intensity profile is calibrated to.

### Forward ICU Share Decision Frontier

20 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: `r2b_icu_share` set to 0, 0.25, 0.5, 0.75 and 1.0 in turn. Point 0 is the shipped default. Run via:

```
Rscript scripts/run_icu_share_sweep.R --iterations 20 --days 30
```

Forward intensive care utilisation is poorly determined at this replication count: it reads 22.4% at a zero share, where the beds serve the evacuation wait alone, then moves between 14.1% and 22.7% in no particular order once forward holding is enabled. Too few events per replication go into that column for it to be well determined, and it should not be read as a trend.

### Transport Fleet-Size Sweep

10 replications of 30 simulated days per sweep point at control seed 42, under the shipped default configuration with one override per point: the PMV Ambulance fleet swept across 1 to 5 vehicles and the HX2 40M fleet across 1 to 4, each with the other fleet held at its shipped establishment size.

`plot_transport_capacity_margin_by_fleet_size()` (`R/analysis.R`) rebuilds the environment at each sweep point via `build_environment()` and runs the same replication engine the comparative scenario runner uses. Run via `Rscript scripts/run_transport_sweep.R`. `outputs/transport_capacity_by_fleet_size.csv` holds the full per-point results, including the interval bounds omitted from the companion paper's table.

Mean utilisation across the swept range runs the wrong way on both platforms, rising with fleet size where a fixed demand spread over more vehicles should lower it, and the interval on HX2 40M utilisation at three vehicles spans 2.3% to 19.9%. So few transport events occur per replication that the busy-time estimate at each sweep point is barely pinned down, which is why the companion paper reads the queue column and not this one.

### Mass Casualty Event Stress Test

10 replications of 30 simulated days at control seed 42, under the shipped default configuration with one override: `mass_casualty.event.rate_per_day` set to 0.2 events per day, a mean of five days between events, against a background-only arm at the shipped value of 0.

Injection ships disabled, so everything in this experiment needs that override, the illustrative single run and `images/mass_casualty_events.png` included, and none of it can be reproduced by a shipped-configuration run. That makes this figure the one tracked image `run.R --refresh-baseline` cannot write; it is copied into place from the run's own output directory.

The casualty count drawn for an event is its total, not the number of survivors. A configured share, `mass_casualty.event.kia_fraction`, arrives killed at or near the point of injury and goes to the mortuary pathway rather than through triage. At the shipped share of 0.28, a 30-day run at this rate and seed produces 78 event-derived casualties, 27 killed outright and 51 wounded.

The comparison sorts casualties by origin rather than by a strict time window around each event (see the assumption note in `R/analysis.R`). One seed-42 run under the same override produced 537 total casualties, 459 from the background streams and 78 from two events, one of 33 casualties on day 14 and one of 45 on day 27. The gap-based reconstruction the analysis pipeline applies recovers both exactly; its known failure mode, reading two closely spaced events as one, needs a busier event schedule than this seed produced.

---

## Force Regeneration Under Reinforcement

<small>[Return to Top](#contents)</small>

This experiment is reported here rather than in the companion paper. It measures the force generation mechanism that drives casualty arrivals rather than the performance of the health system, so it informs the simulation's construction rather than a planning decision about the trauma system.

**Design.** 15 replications per row at `moderate_intensity` and 12 at `high_intensity`, each of 30 simulated days, with daily casualty volume averaged across replications and fitted with an ordinary least-squares trend against simulation day. The unreinforced rows use the shipped default (`force_regeneration.reinforcement.demand_interval_days = 0`, which disables the mechanism). The reinforced rows override it with a 7-day demand submission cycle, a 7-day fulfillment lag and the shipped default triangular fill distribution (`fill_min_frac = 0.2`, `fill_mode_frac = 0.85`, `fill_max_frac = 1.1`).

| Scenario | Reinforcement | Daily volume slope | p-value | First-week mean | Last-week mean |
|---|---|---|---|---|---|
| `moderate_intensity` (15 reps) | None | −0.018/day | 0.75 | 14.9 | 14.4 |
| `moderate_intensity` (15 reps) | 7-day demand cycle, 7-day lag | −0.103/day | 0.17 | 16.6 | 14.1 |
| `high_intensity` (12 reps) | None | −0.349/day | 0.0027 | 39.8 | 29.5 |
| `high_intensity` (12 reps) | 7-day demand cycle, 7-day lag | +0.030/day | 0.80 | 35.8 | 36.1 |

At `high_intensity` casualty rates daily volume falls significantly without reinforcement, declining 26% from the first week to the last, and the demand-cycle configuration removes that decline entirely, leaving a slope indistinguishable from flat. At `moderate_intensity` neither configuration shows a slope distinguishable from flat, that profile's casualty volume being too low for 30 days of attrition to deplete the force measurably.

The result matters to the designs above in one respect. Casualty volume at high intensity is not stationary across a 30-day campaign under the shipped configuration, so a response averaged over the campaign carries a declining force behind it. Every experiment in this document is run at the shipped default, which is to say without reinforcement, and the companion paper's comparison of the two intensities is a comparison of campaigns fought under that same condition.

`force_regeneration.reinforcement` is entirely for the planner to set, and this project makes no attempt to balance it automatically against a scenario's attrition rate. The 7-day demand cycle with a 7-day lag illustrates the mechanism; it is not a recommended operational setting.

---

## Provenance

<small>[Return to Top](#contents)</small>

The comparative scenario figures, and `images/scenario_comparison.png` with them, were produced in the project's pinned development container (`rocker/rstudio:4.4.2`, built from `.devcontainer/Dockerfile`). The seed-42 evidence set the companion verification paper reports [[2]](#references) reproduces byte for byte in that same container, and `scripts/check_baseline_reproduction.R` re-checks the reproduction whenever the model changes.

The `moderate_intensity` profile gives 437.8 total casualties per run against the 530 of the documented seed-42 single run. That single run sits inside the profile's own 10th-to-90th-percentile range of 362.7 to 528.0 rather than near its mean, which is what one draw from a wide distribution does; the replicated and single-campaign results agree. Every casualty, mortality and queue figure in the companion paper's comparison tables reproduces exactly when the comparison is re-run at this seed, inside the pinned container and outside it alike.

The three result figures of the companion paper are rendered from the values in its own markdown tables by `scripts/render_paper_figures.R` rather than from a second copy of the data, so a figure cannot disagree with the table it illustrates.

---

## Limitations of the Designs Recorded Here

<small>[Return to Top](#contents)</small>

Four limitations are properties of the designs rather than of the model, and each bounds what the companion paper can claim from the experiment it applies to.

**A comparison of two configurations is not a controlled comparison.** Changing any setting alters the sequence of random draws, so the two arms generate different casualty streams and cannot be matched campaign for campaign. The consequence is a loss of precision rather than a bias: the means remain correct and the intervals are wider than a matched design would give. The scenario comparison is unaffected, its arms differing by design rather than by a small perturbation.

**Every design runs at one control seed.** A control seed determines the whole set of per-replication seeds, so a measurement at 50 replications is one draw from the distribution of 50-replication measurements. The 0.132 percentage point spread across control seeds recorded above is the size of that effect on the best determined response the model reports, and it is the reason the calibration check pools three independent measurements.

**Utilisation columns at 10 and 20 replications are not determined.** Both the transport sweep and the forward intensive care share sweep report utilisation figures that move without order across their swept range. Too few busy-time events accumulate per replication at those counts for the column to be read, and neither sweep's conclusion rests on it.

**The sweeps were run at moderate intensity only.** The transport fleet sweep, the forward intensive care share frontier and the pre-open hold window comparison all use the shipped default configuration, so none of them establishes that its result survives at the higher casualty intensity. Re-running them at high intensity is listed among the further development items of the companion paper [[1]](#references).

Beyond these, the simulation is verified rather than validated: it behaves as its specification describes, which is a separate question from whether that specification represents the real trauma system well [[13]](#references). No design recorded here addresses that question, and none can.

---

## Conclusion

<small>[Return to Top](#contents)</small>

This document records the experimental design of every replicated experiment this project has run and the statistical basis of every interval reported from them. Independence between replications is established by construction, a replication being a pure function of a distinct seed, and each property the framework depends on is asserted by a regression check that runs on every pull request. Intervals are Student $t$ intervals on the mean across replications throughout, and no warm-up period is discarded from any of them, the model being a terminating simulation of a campaign that genuinely starts empty.

The replication counts the experiments use follow from the spread of the responses they measure. A queue is well determined at 50 replications; a died-of-wounds count at the moderate casualty intensity is not, and the counts a given resolution would require are stated above so that any experiment reported as unresolved can be costed before it is re-run. Two further constraints bound what the designs support: no comparison of two configurations is perfectly controlled, and each measurement is taken at a single control seed.

A reader who wants to reproduce a result in the companion paper has the replication count, the horizon, the control seed, the parameter overrides and the invocation for it here. A reader who wants to extend one has the reason the present design stops where it does.

---

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] Battlefield Casualty Handling project. (2026). *Surgical Hours, Not Operating Theatres: Sizing the Land-Based Trauma System for Large Scale Combat Operations*. Retrieved 07 Sep 26, from https://github.com/natosys/Battlefield-Casualty-Handling/blob/main/docs/Multi_Run_Analysis.md

[2] Battlefield Casualty Handling project. (2026). *Applying Discrete Event Simulation to the Land-Based Trauma System: Baseline Performance and System Constraints in a Single Campaign*. Retrieved 07 Sep 26, from https://github.com/natosys/Battlefield-Casualty-Handling/blob/main/docs/Single_Run_Analysis.md

[3] Battlefield Casualty Handling project. (2026). *Battlefield Casualty Handling*. Retrieved 07 Sep 26, from https://github.com/natosys/Battlefield-Casualty-Handling/blob/main/README.md

[4] Ucar, I., Smeets, B., & Azcorra, A. (2019). simmer: Discrete-Event Simulation for R. *Journal of Statistical Software*, *90*(2), 1–30. Retrieved 27 Aug 26, from https://doi.org/10.18637/jss.v090.i02

[5] Karl, A., Eubank, R., Milovanovic, J., Reiser, M., & Young, D. (2014). Using RngStreams for parallel random number generation in C++ and R. *Computational Statistics*, *29*(5), 1301–1320. Open-access preprint retrieved 26 Jun 26, from https://arxiv.org/abs/1403.7645

[6] R Core Team. (2024). *RNGstreams: L'Ecuyer's RngStreams for parallel random number generation*. R Documentation, parallel package. Retrieved 26 Jun 26, from https://stat.ethz.ch/R-manual/R-patched/library/parallel/html/RngStream.html

[7] Rossetti, M. D. (2023). *Simulation Modeling using the Kotlin Simulation Library (KSL)*, §9.2: Variance Reduction Techniques. Retrieved 26 Jun 26, from https://rossetti.github.io/KSLBook/

[8] Law, A. M. (2020). Statistical analysis of simulation output data: the practical state of the art. In *Proceedings of the 2020 Winter Simulation Conference* (pp. 1117–1127). INFORMS Simulation Society. Retrieved 27 Aug 26, from https://informs-sim.org/wsc20papers/134.pdf

[9] Rossetti, M. D. *Simulation Modeling and Arena*, Chapter 5: Statistical Analysis for Infinite Horizon Simulation Models. Retrieved 27 Aug 26, from https://rossetti.github.io/RossettiArenaBook/05-Chapter5.html

[10] Blood, C. G., Zouris, J. M., & Rotblatt, D. (1998). *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

[11] Marble, S. (2025). Both joint and not: Medical support at Okinawa, 1945. *Joint Force Quarterly*, *117*(2), article 11. National Defense University Press. Retrieved 17 Aug 26, from https://digitalcommons.ndu.edu/joint-force-quarterly/vol117/iss2/11/

[12] Lewis, P. A. W., & Shedler, G. S. (1979). Simulation of nonhomogeneous Poisson processes by thinning. *Naval Research Logistics Quarterly*, *26*(3), 403–413. Naval Postgraduate School Calhoun repository. Retrieved 13 Aug 26, from https://calhoun.nps.edu/handle/10945/63159

[13] Sargent, R. G. (2010). Verification and validation of simulation models. In *Proceedings of the 2010 Winter Simulation Conference* (pp. 166–183). IEEE. Retrieved 27 Aug 26, from https://www.informs-sim.org/wsc10papers/016.pdf

<!-- REFERENCES END -->
