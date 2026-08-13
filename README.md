# Battlefield Casualty Handling

## Abstract

This repository contains a Discrete Event Simulation (DES) framework, written in R using the `simmer` package, developed to evaluate resource utilisation and casualty processing within a deployed battlefield medical system under Large Scale Combat Operations (LSCO). Providing baseline parameterised inputs derived from open-access literature, the simulation models per-minute casualty arrivals, triage, and surgical throughput across Role 1 (R1), Role 2 Basic (R2B), and Role 2 Enhanced – Heavy (R2E Heavy) treatment nodes, embedding a three-stage damage control surgery model to reflect treatment pathways and operational constraints.

This document is the project's **system reference**: it describes the codebase structure, the literature and doctrinal basis for every modelled algorithm and parameter, the resource and trajectory model (including the R1/R2B/R2E Heavy trajectory flowcharts), the model's known limitations, and the development environment. It does not present simulation results — those are published as two companion analysis documents:

- **[docs/Single_Run_Analysis.md](docs/Single_Run_Analysis.md)** — the illustrative single-run (seed 42, 30-day) analysis under the Falklands-modified casualty rate baseline, verifying model behaviour and identifying candidate system constraints echelon by echelon.
- **[docs/Multi_Run_Analysis.md](docs/Multi_Run_Analysis.md)** — a multi-run (n≥30 replications, 95% CI) comparative analysis confirming which of those constraints hold at statistical scale, and how the system responds under both the Falklands-modified and an Okinawa-intensity casualty rate.

Readers wanting to run the simulation rather than read about it should start at [Development Environment](#development-environment), which covers the Dev Container setup, dependency restoration, and how to launch a run.

This tool supports iterative refinement and stakeholder engagement, offering a transparent, modular platform for testing health system resilience. With further development and testing against high-intensity casualty models, the simulation can inform doctrinal updates and guide medical planning for future operational environments.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [Introduction](#introduction)
- [Literature Review](#literature-review)
  - [Methodology](#methodology)
  - [Findings](#findings)
    - [Battlefield Casualty Rates and Estimation Models](#battlefield-casualty-rates-and-estimation-models)
    - [Casualty Simulation and DES](#casualty-simulation-and-des)
    - [Statistical Distributions and Modelling Algorithms](#statistical-distributions-and-modelling-algorithms)
    - [Military Doctrine and Operational Health Support Policy](#military-doctrine-and-operational-health-support-policy)
    - [Damage Control Surgery and Post-Operative Critical Care](#damage-control-surgery-and-post-operative-critical-care)
    - [Preventable Death and Time-Dependent Mortality](#preventable-death-and-time-dependent-mortality)
    - [Strategic Aeromedical Evacuation (AME) and Role 4 Doctrine](#strategic-aeromedical-evacuation-ame-and-role-4-doctrine)
    - [Mass Casualty Event Simulation](#mass-casualty-event-simulation)
    - [Statistical Methods for Simulation Verification, Replication, and Sensitivity Analysis](#statistical-methods-for-simulation-verification-replication-and-sensitivity-analysis)
    - [Disease and Non-Battle Injury Evidence](#disease-and-non-battle-injury-evidence)
- [Scenario Context](#scenario-context)
- [Simulation Framework](#simulation-framework)
- [Resource Descriptions](#resource-descriptions)
  - [Health Teams](#health-teams)
    - [Role 1 (R1) Treatment Team](#role-1-r1-treatment-team)
    - [Role 2 Basic (R2B)](#role-2-basic-r2b)
    - [Role 2 Enhanced Heavy (R2E Heavy)](#role-2-enhanced-heavy-r2e-heavy)
  - [Bed Types](#bed-types)
    - [Operating Theatre (OT)](#operating-theatre-ot)
    - [Resuscitation (Resus) (alternatively Emergency)](#resuscitation-resus-alternatively-emergency)
    - [Intensive Care Unit (ICU)](#intensive-care-unit-icu)
    - [Holding (Hold)](#holding-hold)
  - [Transport Assets](#transport-assets)
    - [Protected Mobility Vehicle Ambulance (PMV Ambulance)](#protected-mobility-vehicle-ambulance-pmv-ambulance)
    - [HX2 40M](#hx2-40m)
    - [Dead-Heading Return Legs](#dead-heading-return-legs)
- [Model Parameters and Model Configuration](#model-parameters-and-model-configuration)
  - [Force Size](#force-size)
    - [Population](#population)
    - [Reinforcement Demand & Fulfillment](#reinforcement-demand--fulfillment)
  - [Health System Architecture](#health-system-architecture)
    - [Surgical Shift Roster](#surgical-shift-roster)
  - [Medevac — Transport Fleet](#medevac--transport-fleet)
  - [Schedules and Rosters](#schedules-and-rosters)
  - [Casualty Generation](#casualty-generation)
    - [1. Distribution Parameterisation](#1-distribution-parameterisation)
    - [2. Per-Minute Rate Sampling and Scaling](#2-per-minute-rate-sampling-and-scaling)
    - [3. Arrival Detection via Cumulative Sum](#3-arrival-detection-via-cumulative-sum)
    - [4. Temporal Randomisation](#4-temporal-randomisation)
    - [5. Mass Casualty Event Injection](#5-mass-casualty-event-injection)
    - [6. Force Regeneration and the Endogenous Feedback Loop](#6-force-regeneration-and-the-endogenous-feedback-loop)
  - [Casualty Generation Rates](#casualty-generation-rates)
    - [WIA — Combat](#wia--combat)
    - [KIA — Combat](#kia--combat)
    - [DNBI — Combat](#dnbi--combat)
    - [WIA — Support](#wia--support)
    - [KIA — Support](#kia--support)
    - [DNBI — Support](#dnbi--support)
  - [DNBI Sub-Type Split](#dnbi-sub-type-split)
  - [Parameters Not Scenario-Eligible](#parameters-not-scenario-eligible)
- [Casualty Priorities](#casualty-priorities)
- [Return to Duty](#return-to-duty)
- [Died of Wounds](#died-of-wounds)
  - [Survival Function](#survival-function)
  - [Parameter Calibration](#parameter-calibration)
  - [Multi-Echelon Check and Conditional Increment](#multi-echelon-check-and-conditional-increment)
  - [Treatment Efficacy Modifiers](#treatment-efficacy-modifiers)
  - [Post-Operative Checkpoint](#post-operative-checkpoint)
  - [AME Wait Checkpoint](#ame-wait-checkpoint)
- [Scenario Profiles](#scenario-profiles)
  - [Mechanism](#mechanism)
  - [Parameter classification](#parameter-classification)
  - [Moderate Intensity profile (Falklands 1982 exemplar)](#moderate-intensity-profile-falklands-1982-exemplar)
  - [High Intensity profile (Okinawa exemplar)](#high-intensity-profile-okinawa-exemplar)
- [Development Environment](#development-environment)
  - [Prerequisites](#prerequisites)
  - [First-time setup](#first-time-setup)
  - [Restoring dependencies](#restoring-dependencies)
  - [RStudio Server configuration](#rstudio-server-configuration)
  - [Running the simulation with full parallelism](#running-the-simulation-with-full-parallelism)
  - [Git workflow](#git-workflow)
- [Simulation Design](#simulation-design)
  - [Codebase Structure](#codebase-structure)
    - [Running the simulation](#running-the-simulation)
    - [Multi-run Replication Framework](#multi-run-replication-framework)
    - [Warm-up Period Analysis](#warm-up-period-analysis)
    - [Sensitivity Analysis](#sensitivity-analysis)
      - [Simplex-Constrained Compositions](#simplex-constrained-compositions)
    - [Parameters Excluded from Screening](#parameters-excluded-from-screening)
    - [Screening Response Set](#screening-response-set)
    - [Parameter Name Reference](#parameter-name-reference)
    - [Comparative Scenario Runner](#comparative-scenario-runner)
    - [Shiny Application](#shiny-application)
      - [Full Analysis Mode](#full-analysis-mode)
      - [Sensitivity Panel](#sensitivity-panel)
  - [Simulation Environment Setup](#simulation-environment-setup)
  - [Core Trajectory](#core-trajectory)
    - [Surgical Pathway](#surgical-pathway)
  - [R2B Trajectory](#r2b-trajectory)
    - [Post-Operative Stabilisation](#post-operative-stabilisation)
  - [R2E Heavy Trajectory](#r2e-heavy-trajectory)
  - [Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling)
- [Model Outputs](#model-outputs)
  - [Domain 1 — Mortality and Preventable Death](#domain-1--mortality-and-preventable-death)
  - [Domain 2 — Time-to-Care from R1 Arrival](#domain-2--time-to-care-from-r1-arrival)
  - [Domain 3 — Surgical Throughput](#domain-3--surgical-throughput)
  - [Domain 4 — Echelon Load and Capacity](#domain-4--echelon-load-and-capacity)
  - [Domain 5 — Flow and Disposition](#domain-5--flow-and-disposition)
  - [Domain 6 — Combat Power](#domain-6--combat-power)
  - [Domain 7 — Strategic Evacuation and National Support Base Demand](#domain-7--strategic-evacuation-and-national-support-base-demand)
  - [Output Variable Register cross-reference](#output-variable-register-cross-reference)
- [Further Development](#further-development)
  - [High Impact](#high-impact)
  - [Medium Impact](#medium-impact)
- [Conclusion](#conclusion)
- [References](#references)
  <!-- TOC END -->

---

## Introduction

<small>[Return to Top](#contents)</small>

Large‑scale combat operations (LSCO) represent the most demanding form of conventional warfare, characterised by high‑tempo, multi‑domain action against peer or near‑peer adversaries. LSCO require the orchestration of manoeuvre, fires, logistics, intelligence, and command across vast, often nonlinear battlespaces [[1]](#references), [[2]](#references). Unlike the western experience in Middle-East conflicts, forces in LSCO will not have guaranteed air superiority, uncontested lines of communication, and predictable casualty flows. LSCO is expected to unfold in contested, degraded, and dynamic operational conditions [[1]](#references), [[2]](#references), [[3]](#references). Historical and contemporary case studies - from the industrial battlefields of the Second World War to recent fighting in Ukraine - demonstrate that such operations generate high casualty densities, impose unprecedented logistical demands, and challenge even the most sophisticated forces’ ability to sustain tempo over time [[2]](#references), [[4]](#references).

The medical implications of LSCO are profound. Estimates consistently project casualty high casualty rates, recent literature suggests that a significant amount of those potentially could and should be able to return to duty (potentially without evacuation from theatre) if treated effectively and without delay [[5]](#references). The deployed health system is therefore not a peripheral service but a critical combat enabler. Its capacity to preserve fighting strength underpins the force’s ability to seize, retain, and exploit the initiative. In LSCO, battlefield clearance must be achieved despite contested airspace and disrupted communications, injury patterns will reflect the lethality of modern munitions, and medical logistics must adapt to disrupted supply chains and fluid front lines.

Yet, existing medical doctrine and force design have evolved largely from counterinsurgency campaigns where operational conditions were comparatively permissive [[5]](#references). In a peer‑conflict LSCO scenario, planners must expect prolonged field care, delays in evacuation, and the need for smaller, more mobile surgical teams positioned closer to the fight [[3]](#references), [[5]](#references). A resilient and agile, deployed health system serves not only to save lives, but to sustain operational momentum and, ultimately, to enable the successful prosecution of the campaign.

This research looks to explore the performance of the deployed health system through simulation with an eye to understanding its implications for participation in LSCO. The simulation approach allows for the exploration of scenarios that are difficult to replicate in live exercises, offering evidence‑based insights to refine doctrine, optimise medical force posture, and ensure that health support is adequate for LSCO. In doing so, it contributes to the broader imperative of preparing the force for the realities of high‑intensity warfare in an era of renewed great‑power competition [[6]](#references), [[7]](#references).

---

## Literature Review

### Methodology

To inform the design and implementation of the battlefield casualty simulation, a structured literature review was conducted in two phases.

The initial phase used a multi-pronged methodology. First, open-access academic literature and publicly available internet-based resources on battlefield casualty modelling, discrete event simulation (DES), and casualty rate estimation were surveyed. This was complemented by a snowballing technique, recursively exploring the references cited within key papers to identify additional relevant sources. Large language models (LLMs) were then engaged to identify supplementary resources. This surfaced publications, technical reports, and grey literature that conventional search techniques did not. Finally, this academic and technical review was supplemented by a survey of publicly available military publications, providing context for force structure, casualty flow assumptions, and the operational constraints that shape operational medical planning. This initial phase established the foundational casualty-rate, DES, distributional, and doctrinal basis.

In the second phase the simulation was extended feature by feature, each addition triggered its own deliberate, LLM-assisted search for open-access sources addressing that specific enhancement. These features included the three-stage damage control surgery (DCS) model and its treatment-duration and post-operative recovery parameters, the time-dependent died-of-wounds survival function, the strategic aeromedical evacuation (AME) and Role 4 demand model, the compound Poisson mass casualty event mechanism, and the statistical framework underpinning multi-run replication, warm-up analysis, and Morris/Sobol sensitivity screening. This iterative extension is summarised in the further thematic domains that follow the initial four, each tied to the design or parameterisation requirement of a specific subsequent Issue.

### Findings

In total, 54 resources have been reviewed and incorporated into the simulation framework as of the current codebase, spanning peer-reviewed journals, technical reports, doctrinal publications, and internet publications. The review covers ten thematic domains.

#### Battlefield Casualty Rates and Estimation Models

Historical and predictive models of casualty rates were central to the review. The FORECAS system [[8]](#references) provided a statistically grounded approach to projecting wounded-in-action (WIA), killed-in-action (KIA), and disease/non-battle injury (DNBI) rates using empirical data from past conflicts. Complementary studies [[9]](#references), [[10]](#references), [[11]](#references) and [[12]](#references) highlighted the operational implications of casualty rates in LSCO, emphasizing the disproportionate impact of DNBI on lost duty days and the need for robust force health protection (FHP) strategies. The Falklands War 1982 (Operation CORPORATE) serves as this project's principal historical validation exemplar for a moderate-intensity casualty rate. Field-surgical mortality outcomes are reported from two overlapping vantage points: four Army Field Surgical Teams operated on 233 casualties across the Ajax Bay Advanced Surgical Centre and the forward stations at Teal Inlet and Fitzroy, recording three deaths [[13]](#references), while an account of the Ajax Bay facility records that over 650 combat casualties from both sides passed through it, of whom three died of wounds [[14]](#references). Campaign-level totals of 255 killed and 777 injured are recorded in the parliamentary record [[53]](#references), [[54]](#references). These are the figures the died-of-wounds calibration is validated against (see [Parameter Calibration](#parameter-calibration)).

#### Casualty Simulation and DES

Simulation methodologies were explored through both military-specific and general DES literature. The simmer package for R [[15]](#references) was identified as a suitable framework for implementing modular, auditable, and event-driven logic. Studies such as [[16]](#references) demonstrated DES applications in healthcare contexts, while [[17]](#references) and [[4]](#references) provided high-level casualty rate numbers that allowed the evaluation of the performance of DES models and [[2]](#references) provided insights into trauma system design under combat conditions. These sources informed the architectural decisions for the simulation engine, including event scheduling, resource constraints, and patient flow logic.

#### Statistical Distributions and Modelling Algorithms

The review examined statistical distributions suitable for modelling casualty arrival rates and treatment durations. FORECAS [[8]](#references) employed lognormal and exponential distributions based on battle intensity and troop type, validated through historical data. Additional studies [[18]](#references), [[11]](#references) and [[16]](#references) provided statistics, explored distribution models measures, and described other approaches using DES in medical care contexts.
These findings guided the selection of distribution functions for stochastic modelling, ensuring that simulated outputs reflect doctrinally plausible variability and temporal clustering.

#### Military Doctrine and Operational Health Support Policy

Doctrinal and policy publications such as [[1]](#references), [[3]](#references), [[6]](#references) and [[7]](#references) established the current geostrategic context and outlined the imperitive for militaries to be able to provide force options for LSCO. 
Recent doctrinal analyses [[9]](#references), [[2]](#references) and [[5]](#references) emphasized the shift toward prolonged casualty care, contested evacuation, and the need for distributed medical assets. A related analysis [[19]](#references) frames return-to-duty as a direct force-sustainment lever rather than a purely clinical outcome and is implemented in the model directly ([Force Regeneration and the Endogenous Feedback Loop](#6-force-regeneration-and-the-endogenous-feedback-loop)), coupling casualty production to a live, time-varying effective force size. These insights were incorporated into the simulation design to ensure alignment with contemporary operational realities.

#### Damage Control Surgery and Post-Operative Critical Care

A systematic review [[20]](#references) and an account of damage control technique in abdominal surgery [[21]](#references) establish the three-phase structure that this project's R2B and R2E Heavy trajectories implement. Operative-time data reported for a damage control cohort within [[20]](#references) directly sourced the DAMCON surgery duration distribution, corroborated by the rapid-closure operative-window principle central to damage control technique [[22]](#references) and by outcomes literature from an austere-setting DCS series [[23]](#references) and abdominal-trauma DCS practice [[24]](#references). Post-operative critical care requirements are established by [[25]](#references), [[22]](#references), [[24]](#references) and [[26]](#references); descriptions of post-DCS stabilisation timeframes [[20]](#references), [[27]](#references), [[24]](#references) informed the ICU length-of-stay parameters at both echelons. Task-time estimation for the R2B/R2E resuscitation phase, where no single published source tabulates an end-to-end duration, drew on the treatment-process optimisation methodology of [[28]](#references), constructing an estimate from collated task-duration components rather than a single reported figure.

#### Preventable Death and Time-Dependent Mortality

An analysis of 4,596 battlefield deaths during Operations Enduring Freedom and Iraqi Freedom [[29]](#references) found that 87.3% resulted from haemorrhage, predominantly within 30–90 minutes of injury; a study of a "golden hour" surgical-capability policy [[30]](#references) found it reduced preventable prehospital death from 32% to 3.5% in a Special Operations context. Together these establish the direct empirical link between time-to-care and survivability that motivates modelling died-of-wounds probability as a function of elapsed time since injury; the logistic curve's shape parameters are anchored to the mortality time-windows these two studies report. The treatment-efficacy multipliers that further modify the died-of-wounds ceiling by care received at each phase are similarly evidence-based rather than estimated outright: a 41% relative mortality reduction from balanced-component damage control resuscitation is reported in [[31]](#references), and the PROMMTT study [[32]](#references) provides the haemorrhage-specific mortality basis used to derive the surgical efficacy factor.

#### Strategic Aeromedical Evacuation (AME) and Role 4 Doctrine

The strategic AME and Role 4 demand model draws on [[33]](#references), which establishes the triage framework, echelon functions, and the Casualty Staging Unit/CCATT-CCAST acuity concepts underpinning the model's critical/standard AME pool split. Role 4 ward assignment and length-of-stay parameters follow the general injury-severity length-of-stay gradient described in [[34]](#references), applied by informed extension since no open-access source tabulates Role 4 ward assignment by this project's exact category scheme.

#### Mass Casualty Event Simulation

The compound Poisson mass casualty event mechanism follows the general approach to LSCO casualty-surge simulation described in [[35]](#references), and complements the discrete-event mass-casualty-incident stress-testing precedent of SIMEDIS [[36]](#references), which injects a fixed, deterministic victim count rather than a stochastically varying one. An account of aligning field hospital training with LSCO reality [[37]](#references) informed the blast-dominant triage priority distribution applied to mass-casualty-derived casualties.

#### Statistical Methods for Simulation Verification, Replication, and Sensitivity Analysis

Moving from a single illustrative run to a defensible multi-run analytical framework required its own literature base. Verification and validation followed the framework in [[38]](#references) and the general DES methodology in [[39]](#references). Warm-up classification used Welch's graphical procedure [[40]](#references) and the treatment of terminating-versus-steady-state simulation in [[41]](#references), with [[42]](#references) establishing that warm-up detection methods presuppose a steady state that does not apply to a campaign with a defined finite horizon. Multi-run replication drew on a demonstration of L'Ecuyer-CMRG parallel RNG streams in R [[43]](#references) and the corresponding parallel-package documentation [[44]](#references), with the conditions a variance reduction scheme must meet taken from a treatment of antithetic variates [[45]](#references) and the choice of unit of analysis and replication count from a review of simulation output analysis [[63]](#references). Global sensitivity screening used Morris Elementary Effects [[46]](#references), implemented via R's `sensitivity` package [[47]](#references) and the OpenMOLE documentation [[48]](#references), and Sobol variance-based decomposition [[49]](#references). A critical-care DES study [[50]](#references) informed the Shiny application's default replication count and 95% CI reporting.

#### Disease and Non-Battle Injury Evidence

Disease DNBI sub-categorisation and its emergency surgical candidacy rate draw on documented evidence of acute surgical disease conditions occurring in deployed populations. Acute appendicitis incidence among deployed Japanese soldiers in Burma is reported in [[51]](#references), and humanitarian surgical care delivered at US military treatment facilities in Afghanistan is documented in [[52]](#references), together supporting the inclusion of conditions such as appendicitis, cholecystitis, and perforated peptic ulcer within the disease DNBI pathway's surgical candidacy branch, distinct from the battle fatigue and NBI sub-types that do not carry the same surgical risk profile.

Because no open-access source was identified that tabulates the specific quantity required, several parameters in this project remain informed estimates rather than directly sourced values, described in detail in the [Further Development](#further-development) section.

This literature review enabled the design of a model suitable to support the assessment of the deployed health system capacity within LSCO.

---

## Scenario Context

<small>[Return to Top](#contents)</small>

The health system establishment — the number of Role 1 (R1) treatment teams, Role 2 Basic (R2B) facilities, and Role 2 Enhanced Heavy (R2E Heavy) hospitals, and each element's internal team and bed composition — is a configurable input to the simulation, defined in `env_data.json`'s `elms` structure and editable directly or via the Shiny Configure panel (`app.R`).

The shipped default configuration models the land combat health system anticipated for a representative combat brigade: three battlegroups, each allocated one R1 treatment team tasked with providing immediate clinical intervention and stabilisation in proximity to combat operations (three R1 teams in total); two R2B facilities conducting damage control resuscitation and surgery before onward evacuation to higher-level care; and one R2E Heavy hospital sited to the rear for complex surgical procedures, extended post-operative care, and advanced diagnostic support. This default configuration underpins the diagram below and the illustrative single-run and multi-run analyses documented in [docs/Single_Run_Analysis.md](docs/Single_Run_Analysis.md) and [docs/Multi_Run_Analysis.md](docs/Multi_Run_Analysis.md).

![Alt text](../images/tactical_diagram.png)

An [Interactive Diagram](https://www.map.army/?ShareID=1041883&UserType=RO-xOMjf7j6) allows further exploration of the default model configuration being simulated.

---

## Simulation Framework

<small>[Return to Top](#contents)</small>

The simulation models casualty handling across echelons of care in a battlefield environment, structured around modular trajectories and dynamic resource availability. It operates within a discrete-event simulation framework using `simmer`, and is driven by probabilistic rates, conditional branching, and resource interactions across Role 1 (R1), Role 2 Basic (R2B), and Role 2 Enhanced Heavy (R2E) facilities.

The simulation was designed around the general functions of each role of health element as outlined in the diagram below. Where roles overlap they are able to provide the same functions to varying degree. The diagram below provides an outline of the role and function design applied for this simulation using the three-stage Damage Control Surgery (DCS) model of care described in [[20]](#references) and [[21]](#references).

```mermaid
block-beta
  columns 13

  a["Casualty Care"]:13
  b["R1"]:3
  c["R2B"]:4 space:9
  e["R2E"]:7 space:6
  f["R4"]:10

  g["Triage"]
  h["POI Care"]
  i["MEDIVAC"]
  j["Resus/Emergency"]
  k["Abbreviated Surgery"]
  l["ICU Stabilisation"]
  m["MEDIVAC"]
  n["Definitive Surgery"]
  o["Recovery"]
  p["MEDIVAC"]
  q["Reconstructive Surgery"]
  r["Rehabilitation"]
  s["Long Term Recovery"]

  %% Role 1 — Deep Navy
  style b fill:#336699,stroke:#003366,color:#ffffff
  style g fill:#336699,stroke:#003366,color:#ffffff
  style h fill:#336699,stroke:#003366,color:#ffffff
  style i fill:#336699,stroke:#003366,color:#ffffff

  %% Role 2B — Dark Sea Green
  style c fill:#2e8b57,stroke:#14532d,color:#ffffff
  style j fill:#2e8b57,stroke:#14532d,color:#ffffff
  style k fill:#2e8b57,stroke:#14532d,color:#ffffff
  style l fill:#2e8b57,stroke:#14532d,color:#ffffff
  style m fill:#2e8b57,stroke:#14532d,color:#ffffff

  %% Role 2E — Olive Gold
  style e fill:#b5a900,stroke:#665c00,color:#ffffff
  style n fill:#b5a900,stroke:#665c00,color:#ffffff
  style o fill:#b5a900,stroke:#665c00,color:#ffffff
  style p fill:#b5a900,stroke:#665c00,color:#ffffff

  %% Role 4 — Burnt Amber
  style f fill:#cc6600,stroke:#663300,color:#ffffff
  style q fill:#cc6600,stroke:#663300,color:#ffffff
  style r fill:#cc6600,stroke:#663300,color:#ffffff
  style s fill:#cc6600,stroke:#663300,color:#ffffff
```

---

## Resource Descriptions

<small>[Return to Top](#contents)</small>

### Health Teams

#### Role 1 (R1) Treatment Team

A role 1 treatment team provides the first line of medical care. It is designed to deliver immediate lifesaving measures, perform triage and stabilization, and manage minor injuries and illnesses close to the point of injury or unit location. These teams also prepare casualties for evacuation to higher levels of care if needed.

#### Role 2 Basic (R2B)

A Role 2 Basic (R2B) medical treatment facility provides forward surgical and resuscitative care close to the battlefield. Its purpose is to deliver damage-control surgery, critical care, and short-term patient holding in austere environments where rapid intervention can save lives.

It’s designed to be mobile, logistically lean, and capable of stabilizing casualties before evacuation to higher-level care. With ICU beds, low-dependency holding, and a surgical team, R2B bridges the gap between frontline treatment and more comprehensive facilities like Role 2 Enhanced or Role 3.

#### Role 2 Enhanced Heavy (R2E Heavy)

A R2E Heavy facility delivers advanced surgical and critical care capabilities in forward-deployed military operations. Its purpose is to provide damage-control surgery, intensive care, inpatient services, and scalable resuscitation for casualties who require more than basic stabilization but are not yet ready for strategic evacuation.

The R2E Heavy is a static field hospital designed to handle complex trauma, prolonged care, and high casualty volumes.

### Bed Types

#### Operating Theatre (OT)

OT beds are specialized surgical stations designed to support damage-control surgery and life-saving interventions. These beds are part of a sterile operating suite and are equipped to handle:

- Emergency trauma procedures.
- Advanced surgical care including orthopedic, abdominal, and thoracic operations.
- Integrated anesthesia and monitoring systems for patient stability.
- Rapid turnover and sterilization protocols to manage high casualty volumes.

#### Resuscitation (Resus) (alternatively Emergency)

Emergency beds are designed for rapid stabilization and life-saving interventions immediately after casualty arrival. These beds support:

- Advanced trauma management including airway control, hemorrhage control, and shock treatment.
- Critical monitoring and resuscitation equipment such as defibrillators, oxygen delivery systems, and IV access.
- Quick turnover and accessibility to facilitate high casualty throughput during mass casualty events.
- Integration with surgical and evacuation pathways, ensuring seamless transition to operating theatres or higher echelons of care.

These beds serve as the first stop for severely injured personnel.

#### Intensive Care Unit (ICU)

Intensive Care Unit (ICU) beds are designed to deliver advanced life-support and continuous monitoring for critically injured or ill personnel. These beds serve as the backbone of forward-deployed critical care, enabling:

- Resuscitation and stabilization of casualties with multi-system trauma.
- Mechanical ventilation, invasive monitoring, and medication infusions.
- Postoperative care following damage-control surgery.
- Support for prolonged field care when evacuation is delayed.

#### Holding (Hold)

Holding beds are designated for short-term patient care and observation, typically for those who are awaiting evacuation, recovering from minor procedures, or expected to return to duty soon. These beds serve as a transitional space between acute treatment zones (like ICU or OT) and final disposition, whether that’s evacuation to higher care or reintegration into the force.

They’re often used for:

- Postoperative recovery after damage-control surgery.
- Monitoring stable patients who don’t require intensive care.
- Staging casualties for medical evacuation.
- Low-dependency care such as hydration, pain management, or wound dressing.

Holding beds help to maintain patient flow and prevent bottlenecks in critical care areas.

### Transport Assets

#### Protected Mobility Vehicle Ambulance (PMV Ambulance)

The PMV Ambulance (Protected Mobility Vehicle – Ambulance) is a blast-resistant, armored medical transport designed to safely evacuate casualties from combat zones. Based on the Bushmaster, it combines mobility, protection, and medical capability, allowing medics to deliver care en route while shielding patients from small arms fire, IEDs, and mines.

#### HX2 40M

The HX2 40M is a 4×4 tactical military truck developed by Rheinmetall MAN Military Vehicles (RMMV) as part of the HX2 series. Designed for high mobility and rugged performance, it serves as a versatile logistics platform for transporting troops, equipment, and supplies in demanding operational environments. In this simulation the HX2 40M is used for the transport of KIA and casualties that have DOW.

#### Dead-Heading Return Legs

PMV Ambulance and HX2 40M transport assets are held for a return leg after casualty drop-off, rather than becoming available for the next pickup immediately. This reflects the real-world requirement for a vehicle to travel back to the originating echelon before it can be tasked again.

Dead-heading is implemented for all four WIA/KIA transport legs using simmer's `clone()`/`synchronize()` activities. After the outbound timeout, the entity is cloned into two parallel branches: a vehicle (or, for the R2B→R2E WIA leg, escort-team) branch that runs an unladen return-leg timeout before releasing the asset, and a casualty branch with no further activity. `synchronize(wait = FALSE)` then lets the casualty branch continue immediately into the rest of the trajectory (it is always the first of the two clones to reach that point), while the vehicle/escort branch is discarded once it later arrives there having completed the return leg and released the resource. The resource therefore remains occupied for the full round trip, while the casualty's own care pathway is unaffected by the return travel time. The two pooled, brigade-level PMV Ambulance/HX2 40M fleets (R1→R2B WIA, R1→mortuary KIA) and the R2B→R2E mortuary road move (HX2 40M) share fleet capacity; the R2B→R2E WIA leg draws on a separate, R2B-team-organic resource not shared with any pooled fleet. R2E's own local mortuary transfer (`r2e_transport_kia()`) is the one remaining leg with no vehicle asset and no return leg, based on the design assumption that the mortuary is collocated with R2E itself. Return leg duration for all four legs is modelled as a fresh, unconditional triangular draw from the same outbound distribution.

---

## Model Parameters and Model Configuration

<small>[Return to Top](#contents)</small>

The population sizes, health system establishment, transport fleet, and casualty generation rates described in this section are the simulation's shipped defaults. Each is derived from the open-access research and analysis cited alongside it; each value is a configurable input, editable directly in `env_data.json` or via the Shiny Configure panel (`app.R`).

The simulation heavily uses triangular distributions to model the duration of activities undertaken in the model (treatment, transport and other handling tasks). Triangular distributions were employed as they are generally used when the underlying distribution is unknown, but a minimal value, some maximal value, and a most likely value are available [[18]](#references). This approach is similar to other applications of DES in clinical settings, as shown in [[16]](#references).

<!-- ENV SUMMARY START -->
<!-- This section is auto-generated. Do not edit manually. -->

### Force Size

#### Population

The following population groups are defined in the simulation environment:

| Population | Count |
|------------|-------|
| Combat | 2500 |
| Support | 1250 |

#### Reinforcement Demand & Fulfillment

A demand submission cycle of 0 days disables reinforcement (the shipped default); the fulfillment lag and fill distribution parameters are then unused.

| Parameter | Variable | Value |
|-----------|----------|-------|
| Demand Submission Cycle (days) | — | 0 |
| Fulfillment Lag (days) | — | 7 |
| Fill Distribution — Minimum (fraction of demand) | a | 0.2 |
| Fill Distribution — Mode (fraction of demand) | c | 0.85 |
| Fill Distribution — Maximum (fraction of demand) | b | 1.1 |

Each reinforcement cycle computes a pool's demand as its shortfall against initial establishment strength, net of any shortfall an earlier, still-pending cycle has already claimed (`initial − current − pending`, floored at 0) — this prevents overlapping cycles from independently re-claiming the same shortfall when the demand submission cycle is shorter than the fulfillment lag. The amount actually delivered is drawn, at submission time rather than at fulfillment, as a fraction of that demand from a Triangular(*a*, *b*, *c*) distribution parameterised by the three fill values above, then credited to the pool once the fulfillment lag elapses, clamped so a pool can never be credited above its initial establishment strength. The model has no sortie-failure rate or binary success/failure roll for reinforcement.

The fraction of demand *x* actually delivered in a single cycle is drawn from the following probability density function:

$$
f(x) =
\begin{cases}
\dfrac{2(x-a)}{(b-a)(c-a)} & a \le x < c \\[4pt]
\dfrac{2}{b-a} & x = c \\[4pt]
\dfrac{2(b-x)}{(b-a)(b-c)} & c < x \le b
\end{cases}
$$

Where *a*, *b*, and *c* are the Fill Distribution Minimum, Maximum, and Mode values in the table above respectively.

### Health System Architecture

The following table summarises the medical elements configured in `env_data.json`, including team types, personnel, and beds. `Quantity` (team counts) and `Beds` (bed counts per team) are editable directly in `env_data.json` or via the Shiny Configure panel's Health System Architecture group (`app.R`); the personnel/team composition columns (`Base`/`Surg`/`Emerg`/`Icu`/`Evac`) are a fixed part of the establishment definition and are not independently configurable.

| Element | Quantity | Beds | Base | Surg | Emerg | Icu | Evac |
| --- | --- | --- | --- | --- | --- | --- | --- |
| R1 | 3 | NA | Medic (3), Nurse (1), Doctor (1) | NA | NA | NA | NA |
| R2B | 2 | OT (1); Resus (2); ICU (2); Hold (5) | NA | Anesthetist (1), Surgeon (2), Medic (1) | Facem (1), Nurse (3), Medic (1) | Nurse (2), Medic (2) | Medic (2) |
| R2EHEAVY | 1 | OT (2); Resus (4); ICU (4); Hold (30) | NA | Anesthetist (1), Surgeon (2), Nurse (4) | Facem (1), Nurse (3), Medic (1) | Intensivist (1), Nurse (4) | Medic (2) |

#### Surgical Shift Roster

One shift length rosters every surgical section in the theatre, at R2B and R2E alike. It sets the first shift's length; the second covers the remainder of the 24-hour day. See [Schedules and Rosters](#schedules-and-rosters) for how the sections alternate across the two shifts.

| Parameter | Value |
|-----------|-------|
| OT Shift Length (hours per shift) | 12 |

### Medevac — Transport Fleet

These are the available transport platforms and their characteristics:

| Platform | Quantity | Capacity |
|----------|----------|----------|
| PMVAMB | 3 | 4 |
| HX240M | 4 | 50 |

<!-- ENV SUMMARY END -->

### Schedules and Rosters

Some resources carry a roster. Surgical sections work 12-hour shifts, alternating across successive sections so cover is staggered rather than simultaneous. Each R2B facility has one section, giving 12 hours of cover a day. R2E has three, two on the first shift and one on the second, giving 36 section-hours a day across its two theatres. Because a procedure seizes the section as well as the theatre, these hours cap surgical throughput: two concurrent operations at R2E by day and one by night.

| Resource | Roster applied | Configurable variable | Default | Where configured |
|---|---|---|---|---|
| R1 Treatment Team | No — no shift schedule; available continuously | — | — | — |
| R2B Surgical Section | Yes — alternating two-shift roster across successive R2B surgical sections (`build_env()`, `R/environment.R`) | `ot_hours` | 12 (hours) | `env_data.json` (`vars.surgical_roster.shift.ot_hours`), or the Shiny Configure panel's Health System Architecture group |
| R2E Surgical Section | Yes — alternating two-shift roster across R2E's three surgical sections, two on the first shift and one on the second (`build_env()`, `R/environment.R`) | `ot_hours` | 12 (hours) | `env_data.json` (`vars.surgical_roster.shift.ot_hours`), or the Shiny Configure panel's Health System Architecture group |
| R2B / R2E Operating Theatre beds | No — the physical OT bed is available 24 hours per day; only the surgical section carries the shift schedule | — | — | — |

`ot_hours` is a single shared parameter: it sets the first shift's length (the second shift covers the remainder of the 24-hour day) identically at both R2B and R2E, not independently per echelon. Its configured value lives at `vars.surgical_roster.shift.ot_hours` in `env_data.json` and is read from there by `get_ot_hours()` (`R/environment.R`), so a change to that one field reaches every entry point at once: `build_env()`, `run_once()`, `run_replications()`, the scenario runner, the Shiny console and the sensitivity screen. Each of those also accepts an explicit `ot_hours` argument overriding the configured value for that call alone, which programmatic callers use to sweep the shift length without rewriting the configuration; Morris screening (see [Sensitivity Analysis](#sensitivity-analysis)) instead varies it through the same configuration path as every other screened parameter. The Shiny console offers no separate run-time control for it, since a shift length is a property of the health system being simulated rather than of one execution, and a control outside the configuration would govern a run without appearing in the configuration that run is saved and reproduced from. The 12-hour default is a rostering assumption rather than a sourced quantity, no open-access source prescribing a deployed surgical shift length; a two-shift day splitting 24 hours evenly is the simplest arrangement consistent with the section counts above, and were it wrong in either direction surgical throughput would scale with it, which is why the parameter is screened.

### Casualty Generation

Casualties are generated based on rates outlined in [[8]](#references) and refined with analysis provided in [[10]](#references) and supported by [[17]](#references), with the implementation outlined below.

The simulation supports three selectable casualty-rate profiles, each applied as a named scenario-profile overlay on the base configuration (see [Scenario Profiles](#scenario-profiles)) and selectable live via the Shiny app's Casualty Intensity Profile dropdown: the shipped default ("Falklands — Modified"), an explicit `moderate_intensity` profile ("Falklands — Unmodified"), and a `high_intensity` profile ("Okinawa — Casualty Rates"). The default and `moderate_intensity` share identical Falklands-calibrated casualty generation rates, differing only in their downstream died-of-wounds treatment-efficacy calibration (see [Scenario Profiles](#scenario-profiles)); `high_intensity` instead applies Okinawa-calibrated rates with an exponential rather than lognormal distribution family (see [Casualty Generation Rates](#casualty-generation-rates)). Every individual stream's mean and standard deviation is independently re-parameterisable via the Configure panel or directly in `env_data.json`, regardless of which profile is active.

US historical analysis of the Battle of Okinawa [[8]](#references) gives WIA and KIA rates producing approximately 30 casualties per day for a force size of 3,750 — a casualty rate of ~0.8%. By comparison, Russia’s estimated 700-1,100 daily casualties from a committed force of 450,000–600,000 in Ukraine imply a lower casualty rate of ~0.2% [[4]](#references). Historical data from the Falklands War suggests a casualty rate of ~0.37% [[8]](#references), and was selected as the shipped default over the Okinawa rate on the basis that the Okinawa-derived ~0.8% sits well above the rates currently observed for Russia in publicly available data in the Ukraine war. This selection also recognises that there is likely under-reporting in Russian casualty estimates, particularly of non-critical wounded personnel, and that casualty rates will have varied over time in the Ukraine war based on combat intensity and seasonal fluctuations in operational tempo.

Arrival times for the casualty streams are driven by a stateful generator closure (`make_ln_arrival_generator()`/`make_exp_arrival_generator()`, `R/environment.R`) passed directly to simmer's `add_generator()`, called once per arrival during the run itself (this excludes mass casualty event timing, which remains pre-computed and exogenous — see [Mass Casualty Event Injection](#5-mass-casualty-event-injection)). Each call models continuous per-minute intensity and converts it to a discrete arrival event via cumulative-threshold crossing, reading the live effective force size at each step so arrival timing can react to the replication's own in-run events (see [Force Regeneration and the Endogenous Feedback Loop](#6-force-regeneration-and-the-endogenous-feedback-loop)). The general process is outlined below.

[[8]](#references) fits casualty incidence to one of two distribution families, selected by battle intensity and troop type rather than a single distribution applying universally: a lognormal model for moderate/light-intensity combat troops and for support troops at all intensities, and an exponential model for combat troops in high-intensity battles. `generate_casualty_arrivals()` (`R/environment.R`) dispatches each casualty stream to `make_ln_arrival_generator()` or `make_exp_arrival_generator()` based on an explicit `distribution` field read from `env_data$vars$generators`. Both models share the same per-minute sampling, cumulative-sum arrival detection, and jitter mechanics (steps 2–4 below).

#### 1. Distribution Parameterisation

**Lognormal** (`make_ln_arrival_generator()`) converts the daily standard deviation into a log-space scale parameter, preserving the shape of the empirical distribution:

$$
\sigma_{\log} = \sqrt{\ln\left(1 + \frac{\sigma^2}{\mu^2}\right)}
$$

The log-space location is not the matching closed form $\ln(\mu^2 / \sqrt{\sigma^2 + \mu^2})$, which would give a distribution whose *unclamped* mean is $\mu$. Every draw is clamped at the cap described in Step 2 below, and clamping lowers a mean, so a stream parameterised that way realises less than the daily mean its configuration names. `solve_ln_location()` (`R/environment.R`) instead solves for the location at which the clamped draw averages to $\mu$:

$$
E[\min(X, c)] = e^{\mu_{\log} + \sigma_{\log}^2 / 2}\,\Phi\!\left(\frac{\ln c - \mu_{\log} - \sigma_{\log}^2}{\sigma_{\log}}\right) + c\,\Phi\!\left(-\frac{\ln c - \mu_{\log}}{\sigma_{\log}}\right) = \mu
$$

Where:

- $\mu$ = expected number of casualties per day, as configured
- $\sigma$ = daily standard deviation, as configured
- $c$ = the per-minute rate cap (Step 2)
- $\Phi$ = the standard normal cumulative distribution function

The clamped mean is strictly increasing in $\mu_{\log}$, is at most $\mu$ at the closed form above, and tends to $c$ from below, so a unique solution exists whenever the cap exceeds the configured mean. It is found by a one-dimensional root search once, when the generator is constructed, so the per-draw path costs no more than it did. Only the location moves, leaving $\sigma_{\log}$ and therefore the coefficient of variation as configured, which is what makes the correction a pure shift in log space rather than a reshaping of the distribution FORECAS fitted.

**Exponential** (`make_exp_arrival_generator()`) is single-parameter — the rate is fully determined by one mean, with no separate shape parameter, following FORECAS's own formula $W \sim \text{exponential}(\mu)$. The same correction applies, `solve_exp_mean()` solving the exponential's own clamped mean for the underlying mean $\mu'$ that realises the configured one:

$$
E[\min(X, c)] = \mu'\left(1 - e^{-c / \mu'}\right) = \mu, \qquad \lambda = \frac{1}{\mu'}
$$

Where:

- $\lambda$ = exponential rate parameter passed to the per-minute draw (no $\sigma$ term — a reported standard deviation for an exponential-fitted stream is retained in `env_data.json` for citation only and plays no role in generation)

Because the exponential is a scale family and the cap is set as a multiple of the configured mean, $\mu' / \mu$ is the same for every exponential stream whatever its mean: 1.0633 at the shipped multiplier of three.

`scripts/check_arrival_rate_fidelity.R` holds both families to the property, exercising the shipped generator closures over a 1,000-day horizon and comparing each stream's realised daily mean against its configured `mean_daily`. It measures every stream a second time with the correction disabled and fails if any of those measurements would have passed the same band, so the check registers its own removal rather than quietly continuing to pass.

#### 2. Per-Minute Rate Sampling and Scaling

Draws samples from the stream's selected distribution representing per-minute casualty rates, capped at a threshold to prevent extreme outliers, then scaled according to population size and temporal resolution (per minute per 1000 personnel).

For each simulation minute $i \in \{1, 2, \dots, n_{\text{minutes}}\}$, the per-minute casualty rate is computed as:

$$
r_i = \min\left(x_i, \text{cap}\right) \times \frac{P}{1000 \times 1440}
$$

Where:

- $x_i \sim \text{LogNormal}(\mu_{\log}, \sigma_{\log}^2)$ (lognormal streams) or $x_i \sim \text{Exponential}(\lambda)$ (exponential streams)
- $P$ = population size (support or combat) — not a fixed constant, but a live, time-varying effective force size read at each minute; see [Force Regeneration and the Endogenous Feedback Loop](#6-force-regeneration-and-the-endogenous-feedback-loop) below
- $r_i$ = scaled and capped casualty rate for minute i

The cap exists primarily to keep the per-minute generator's execution time practically bounded, not merely to trim outliers cosmetically: because both the lognormal and exponential distributions have an unbounded right tail, an uncapped draw can occasionally return an implausibly large per-minute rate, and since `make_ln_arrival_generator()`/`make_exp_arrival_generator()` walk the simulation minute-by-minute inside a stateful closure rather than sampling arrival times directly, a rare extreme draw of this kind was previously found to substantially inflate run time for the lognormal generator before a cap was introduced. Bounding the draw keeps each minute's rate, and therefore the closure's iteration count, within a predictable range regardless of how extreme a single stochastic draw happens to be.

Both generators set the cap relative to the stream's own mean rather than at a fixed absolute value: `cap = cap_multiplier × mean_daily`, with `cap_multiplier` defaulting to 3 in `make_ln_arrival_generator()` and `make_exp_arrival_generator()` alike. A mean-relative cap is what keeps the cap meaningful when a stream is re-parameterised. Because every stream's mean is independently editable, through the Configure panel or `env_data.json` directly, a cap fixed in absolute terms binds harder the higher the mean a planner enters, until it stops bounding outliers and starts setting the rate: a lognormal stream entered at a mean of 6.86 per day, the value FORECAS reports for high-intensity combat troop WIA, would have half its draws truncated by a fixed cap of 5 and would realise only 57% of the mean asked for.

How uniformly the multiplier binds across streams differs between the two families. For an exponential distribution the share of draws above $k \times \mu'$ depends on $k$ alone, so a cap at three times the mean clamps 6.0% of draws whatever the mean; the property is exact. A lognormal's tail above the cap also depends on its coefficient of variation $\sigma / \mu$, which genuinely differs between streams, so the same multiplier clamps 10.7% of the WIA streams' draws ($\sigma/\mu \approx 2.0$), 10.9% of the KIA streams', 4.3% of `dnbi_cbt`'s ($\approx 0.93$) and 1.2% of `dnbi_spt`'s ($\approx 0.60$). The mean-relative cap therefore narrows the spread across the shipped lognormal streams rather than removing it, from a factor of roughly 150 under a fixed cap of 5 to a factor of under 9. It is the raw scale of the mean, not the coefficient of variation, that produces the large disparity, so removing the first is what matters; a quantile-based cap would equalise the clamped share exactly but would bind harder on the low-variability streams, which a fixed share of draws does not distinguish from the high-variability ones.

What that remaining spread no longer reaches is the realised mean. The clamped share differs between streams because their variability genuinely differs, but the location solved for in Step 1 absorbs whatever share the cap clamps, so each stream still averages the daily rate its configuration names and editing a stream's standard deviation alone leaves its realised mean where it was. The cost is paid in the distribution's shape rather than its mean: a clamped draw is returned at the cap rather than at the value drawn, so roughly a tenth of the WIA and KIA streams' minutes sit at exactly three times the mean, day-to-day variability is understated, and the peak-day casualty volume that drives contention is understated with it (see [Further Development](#further-development), L27).

#### 3. Arrival Detection via Cumulative Sum

Accumulates per-minute rates and detects new arrivals based on when the cumulative total crosses each whole casualty threshold.

Let R = \{r_1, r_2, \dots, r_N\} be the per-minute rates. Then the cumulative sum is:

$$
C_i = \sum_{j=1}^{i} r_j
$$

An arrival is triggered at time i if:

$$
\lfloor C_i \rfloor > \lfloor C_{i-1} \rfloor
$$

This captures each increment in the expected arrival count.

#### 4. Temporal Randomisation

Introduces sub-minute jitter — a Uniform(0, 1) offset added to each detected arrival's whole-minute timestamp (`arrival_time <- minute_ptr + jitter`), so arrivals are not tied to the exact minute boundary the discrete per-minute computation grid produces them on. Without this offset, every arrival from every one of the six independent casualty-generation streams would fall on a whole-minute timestamp, and two streams (e.g. combat WIA and combat KIA) could easily register an arrival in the same simulated minute, producing simultaneous, tied event times with no principled ordering between them. The jitter spreads each stream's arrivals continuously within their detected minute, giving simmer's event queue a well-ordered sequence of distinct timestamps to schedule against rather than a discretised, tick-aligned one. Each call to the generator closure returns the gap, in simulation minutes, between this arrival and the previous one — the value simmer's `add_generator()` `distribution` argument itself expects — rather than a pre-computed, already-sorted vector of arrival times; because each stream's own arrivals are produced in strictly increasing time order by construction, no separate sorting step is needed.

#### 5. Mass Casualty Event Injection

The background streams above model a continuous casualty rate and cannot represent the acute, discrete casualty surges. These form a distinct stress test for surgical and ICU capacity, separate from the sustained background tempo the lognormal/exponential streams already represent. `generate_mass_casualty_events()` (`R/environment.R`) overlays mass casualty events on the background `wia_cbt` combat-WIA stream via one of two selectable event-timing modes (`mass_casualty.event.mode`), sharing an identical per-event casualty-count/injection-window mechanism (`mass_casualty_event_casualties()`) regardless of which mode generated the event's start time. This complements the discrete-event mass-casualty-incident stress-testing precedent of SIMEDIS [[36]](#references), which, unlike either mode here, injects a fixed, deterministic victim count per scenario rather than a stochastically varying one.

The feature ships disabled by default (`mode = "poisson"`, `mass_casualty.event.rate_per_day = 0` in `env_data.json`).

**Mode 1 — `"poisson"` (default).** A compound Poisson process, informed by the compound Poisson parameterisation of Fischer et al. [[35]](#references) and the blast-dominant injury context reported for contemporary LSCO [[37]](#references). Event start times are drawn from a Poisson process with rate `mass_casualty.event.rate_per_day` via the standard exponential inter-arrival construction (`mass_casualty_event_starts_poisson()`):

$$
t_{k+1} = t_k - \frac{\ln(1 - U)}{\lambda_{\text{min}}}, \quad U \sim \text{Uniform}(0, 1)
$$

where $\lambda_{\text{min}} = \text{rate\_per\_day} / 1440$ is the per-minute event rate. `rate_per_day = 0` returns an empty arrival stream with no RNG draws consumed, reproducing the background-only baseline exactly.

**Mode 2 — `"scheduled"`.** Rather than an inferred rate, a planner specifies a fixed set of candidate simulation days directly — `mass_casualty.schedule.days` (e.g. `[5, 12, 20]`) — with an independent per-day occurrence probability, `mass_casualty.schedule.probabilities` (e.g. `[1.0, 0.8, 0.5]`; omitted or empty defaults every listed day to probability 1, i.e. always fires). `mass_casualty_event_starts_scheduled()` draws a Bernoulli(probability) outcome for each configured day independently, so replication-to-replication variation is still possible for any day given a probability below 1, while a day at probability 1 fires identically in every replication. A fired day's exact start minute is drawn Uniform(0, 1440) within that day, so intra-day timing remains stochastic even when the day itself is planner-specified. An empty `days` list (the shipped default) produces no scheduled events regardless of `mode`.

**Event size** (both modes). Each fired event injects a number of casualties drawn from $\text{Uniform}(\text{min\_cas}, \text{max\_cas})$ (default 20–60), rounded to the nearest integer.

**Injection window** (both modes). Casualties from a single event are not injected simultaneously. Each event's injection window duration is drawn from $\text{Triangular}(\text{window\_min}, \text{window\_mode}, \text{window\_max})$ minutes (default 60/120/180, i.e. 1–3 hours, mode 2 hours); individual casualty offsets within that window are drawn from $\text{Uniform}(0, \text{window})$ and sorted.

**Triage priority** (both modes). Mass-casualty-derived casualties draw triage priority from an independently set distribution. The default distribution is intended to reflect the higher proportion of immediately life-threatening injuries in blast/fragmentation trauma relative to the mixed injury pattern of the background stream, consistent with the blast/fragmentation injury share reported in [[37]](#references).

**Stream merge and tagging.** Mass casualty arrival times, from whichever mode is active, are merged into the background `wia_cbt` arrival vector and the combined vector is sorted before being passed to simmer's `at()` generator, so mass casualty and background casualties are dispatched through the same trajectory. Each casualty is tagged with a `mass_casualty_event` attribute (1 = mass-casualty-derived, 0 = background) at the point of triage in `build_casualty_trajectory()`, enabling the post-hoc stress-test analysis in [Mass Casualty Event Stress Test](docs/Single_Run_Analysis.md#mass-casualty-event-stress-test).

Because mass casualty events overlay only the combat WIA stream, immediate KIA and DNBI are not generated by a mass casualty event.

#### 6. Force Regeneration and the Endogenous Feedback Loop

The population term $P$ in the per-minute rate formula (Step 2 above) is a live, time-varying effective force size, read fresh at every simulated minute from a simmer global (`effective_force_combat`/`effective_force_support`) that the running simulation updates as casualties occur and return to duty or where reinforcements are introduced. Casualty production is thereby a function of the effective force, consistent with [[8]](#references) and [[19]](#references).

$$
r_i = \min(x_i, \text{cap}) \times \frac{F(t_i)}{1000 \times 1440}
$$

Where $F(t_i)$ is the effective force size (combat or support pool, matching the stream) at simulated minute $t_i$.

Combat and support effective force size are tracked as two independent globals rather than a single combined scalar: combat and support casualties are already separate generated streams drawn against separate population sizes ([Casualty Generation Rates](#casualty-generation-rates)).

**Mechanism.** Each pool starts at its full establishment strength (`env_data$pops$combat`/`support`) and is updated at three points, all driven by events the simulation itself generates for that specific replication, at the instant each occurs:

1. **Debited by 1** the instant any casualty (WIA, KIA, or DNBI, either pool) is generated — `debit_force_size()`, applied at `build_casualty_trajectory()`'s `injury_time` assignment (`R/trajectories.R`). Every casualty is removed from effective fighting strength from the moment they occur.
2. **Credited by 1** the instant a casualty reaches return-to-duty — `credit_rtd()`, applied at each of R1/R2B/R2E's existing `return_day` assignment points (`R/trajectories.R`). `return_day` is set to `now(env)` at the actual completion of each echelon's own timeout-governed recovery/holding period.
3. **Reinforcement** — every `force_regeneration.reinforcement.demand_interval_days` (`env_data.json`), each pool independently submits a demand equal to its current shortfall against establishment strength, net of any shortfall already claimed by an earlier, still-pending cycle (`initial − current − pending`, floored at 0), then, after a configurable `fulfillment_lag_days`, is credited with a fraction of that demand drawn from a Triangular(`fill_min_frac`, `fill_mode_frac`, `fill_max_frac`) distribution, clamped so the credit can never carry the pool above `initial` (`build_reinforcement_trajectory()`, `R/trajectories.R`). This is the one genuinely periodic mechanism in the model; it ships disabled (`demand_interval_days = 0`) so no generator is added and no RNG draws are consumed.

KIA and strategic-evac (`r2e_evac = 1`) casualties never reach a `return_day` site, so they remain a permanent loss to the pool without a separate subtraction term.

The reinforcement demand/fulfillment cycle's shipped defaults (`fill_min_frac = 0.2`, `fill_mode_frac = 0.85`, `fill_max_frac = 1.1`, `fulfillment_lag_days = 7`) give a fill-fraction distribution with a long left tail toward severe under-fill (5th percentile ≈ 0.37 of demand) and a short right tail bounding over-supply (99th percentile ≈ 1.05) — full or over-fulfillment is the *less* likely outcome even though it is the distribution's mode, because the long lower tail pulls the mean well below the mode (≈0.72 vs. 0.85 at the shipped defaults). Demand and the fill fraction are both resolved at submission time, not at fulfillment.

The reinforcement demand cycle, fulfillment lag, and fill distribution are the key modelling levers a planner controls: a baseline run with no reinforcement isolates the pure depletion effect of sustained casualty production against RTD-only regeneration; a run with a short demand cycle, short lag, and a fill distribution weighted toward full delivery approximates well-sustained LSCO reinforcement and keeps daily casualty volume closer to constant. Both are demonstrated in [Force Regeneration Feedback Loop](docs/Single_Run_Analysis.md#force-regeneration-feedback-loop).

### Casualty Generation Rates

The shipped default and `moderate_intensity` share identical lognormal parameters; `high_intensity` overrides the WIA and KIA streams with an exponential distribution, while DNBI remains lognormal and inherited from the base configuration unchanged across all three profiles.

#### WIA — Combat

Combat WIA casualty generation has been based on Falklands combat troop WIA rates for the default and `moderate_intensity` profiles, and Okinawa combat troop WIA rates for `high_intensity` ([[8]](#references), tables A.8 p32 and A.7 respectively).

| Profile | Distribution | Parameters |
|---|---|---|
| default / `moderate_intensity` | Lognormal | $\mu = 1.77$, $\sigma = 3.56$ |
| `high_intensity` | Exponential | $\mu = 6.86$ |

#### KIA — Combat

Combat KIA casualty generation has been based on Falklands combat troop KIA rates for the default and `moderate_intensity` profiles, and Okinawa combat troop KIA rates for `high_intensity` ([[8]](#references), tables A.8 p32 and A.9 respectively).

| Profile | Distribution | Parameters |
|---|---|---|
| default / `moderate_intensity` | Lognormal | $\mu = 0.68$, $\sigma = 1.39$ |
| `high_intensity` | Exponential | $\mu = 1.63$ |

#### DNBI — Combat

Combat DNBI casualty generation has been based on Vietnam combat troop DNBI rates ([[8]](#references), table A.5 p31). This stream is inherited from the base configuration unchanged across all three profiles.

$$
\mu = 2.04, \quad \sigma = 1.89
$$

#### WIA — Support

Support WIA casualties employ the same casualty generation outlined above for combat WIA (except using the support population estimate of 1250 instead of the combat population of 2500), including the same `high_intensity` exponential override applied to the support population (see [Scenario Profiles](#scenario-profiles) for the rationale). This is on the basis that most historical modelling of force casualties include support elements at or below division in division and below casualty estimation due to their integral nature to combat operations and close proximity to the Forward Edge of the Battle Area (FEBA) (see [[17]](#references) and [[10]](#references) p 2-4).

#### KIA — Support

Similar to WIA, support casualty KIA employ the same casualty generation outlined above for combat KIA (except using the support population estimate of 1250 instead of the combat population of 2500), including the same `high_intensity` exponential override applied to the support population (see [[17]](#references) and [[10]](#references) p 2-4).

#### DNBI — Support

Support DNBI casualty generation has been based on Okinawa support troop DNBI rates ([[8]](#references), table A.2 p29). This stream is inherited from the base configuration unchanged across all three profiles.

$$
\mu = 0.94, \sigma = 0.56
$$

### DNBI Sub-Type Split

DNBI casualties are sub-categorised at generation time into three distinct clinical groups, each assigned a differentiated treatment pathway that reflects the substantially different resource demands of each sub-type.

| Sub-category                                    | Proportion | Pathway                                                                                                                                                               |
| ----------------------------------------------- | ---------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Battle fatigue / psychiatric                    | 25%        | R1 hold → RTD. No R2 routing, no surgery candidacy, no DOW check.                                                                                                     |
| Disease (febrile, GI, respiratory)              | 58%        | R1 treatment → R2B holding if evacuation threshold met. 6% surgical candidacy for emergency conditions (appendicitis, cholecystitis, perforated ulcer). No DOW check. |
| Non-battle injury (musculoskeletal, accidental) | 17%        | Standard WIA-equivalent routing, including DOW check and surgical candidacy.                                                                                          |

The 17% NBI proportion is drawn from FORECAS empirical data ([[8]](#references), pp 22–23). The remaining split between battle fatigue and disease is derived from historical LSCO data: approximately 25–30% of total DNBI evacuations across conflict periods are documented as psychiatric and battle fatigue cases [[19]](#references); 25% is used as a central estimate, with an over-estimate artificially reducing R2B/R2E load and an under-estimate over-loading the surgical pathway with non-surgical cases. With NBI fixed at 17% from [[8]](#references), disease is the residual category, representing approximately 53–58% of total DNBI — rounded to 58% as the central estimate for the model, since no open-access source directly measures the deployed disease DNBI proportion; because this proportion determines the fraction of DNBI routed to R2B holding rather than to surgery, a higher disease proportion would increase holding bed demand without affecting OT throughput.

The disease sub-type's 6% emergency surgical candidacy rate is an informed estimate derived from population-level surgical incidence in military-age males: appendicitis alone occurs at approximately 35–50 per 10,000 per year in this demographic [[51]](#references), with acute cholecystitis, perforated peptic ulcer, and complicated soft tissue infections adding further surgical demand, and against approximately 100 disease DNBI presentations per month in the modelled force these conditions yield approximately 3–6 surgical cases — consistent with emergency surgical care for disease conditions being documented as a significant component of deployed hospital workload [[52]](#references).

Only the NBI sub-type (17% of DNBI) enters the routine surgical pathway on the same terms as WIA; disease contributes a small additional load through its 6% emergency surgical candidacy, and battle fatigue contributes none. OT demand is therefore driven primarily by WIA and NBI casualties. Across 100 replications (30 days, seed 42), the mean number of casualties requiring surgery per replication was 158.6 (SD 6.8; range 143–177). Of DNBI sub-types, NBI cases generated surgical candidacy at a rate of 79.6% (consistent with the WIA-equivalent trajectory), disease cases at 5.7% (reflecting the 6% emergency surgical rate assumption), and battle fatigue cases at 0.0% (by design).

### Parameters Not Scenario-Eligible

The scenario profiles ([Scenario Profiles](#scenario-profiles)) override only casualty generation, DOW, priority, DNBI, and transport-time parameters. Several other `env_data.json` parameter groups sit outside that framework entirely, either because neither shipped profile has yet been sourced to override them, or because they describe something other than a historically-variable casualty-rate or mortality fact:

- **Structural configuration** (element, bed, and team counts in `elms`; transport fleet sizes in `transports`; and population sizes in `pops`) describes the deployed force structure being tested against a scenario rather than the scenario itself, so it is never overridden by a scenario profile.
- **Mass casualty event parameters** (`mass_casualty.event`, `mass_casualty.priority`, `mass_casualty.schedule`; see [Mass Casualty Event Injection](#5-mass-casualty-event-injection)) are plausibly scenario-relevant, since event rate, size, and the blast-dominant priority mix could differ by battle intensity, but neither shipped profile currently overrides them; sourcing era-specific mass casualty parameters is the same class of gap already noted for `high_intensity`'s DOW ceiling and treatment efficacy.
- **Force regeneration reinforcement parameters** (`force_regeneration.reinforcement`; see [Force Regeneration and the Endogenous Feedback Loop](#6-force-regeneration-and-the-endogenous-feedback-loop)) are a planner-configured logistics lever, the reinforcement demand cadence, fulfillment lag, and fill distribution, rather than a fact about a historical casualty rate, so they are not treated as scenario-eligible.
- **Role 4 and strategic AME parameters** (`role4.*`; see [Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling)) are inherited unchanged by both shipped profiles. Strategic fixed-wing aeromedical evacuation as modelled here is a distinctly modern capability, so applying it unchanged to `high_intensity`'s Okinawa-era casualty stream is the same kind of era mismatch already acknowledged for that profile's DOW ceiling and treatment efficacy factors.
- **Per-echelon treatment and process parameters** (`r1.recovery`/`kia_treat`/`wia_treat`; `r2b.surgery`/`long_resus`/`holding`/`kia_treat`/`icu_gating`; `r2eheavy.surgery`/`short_resus`/`long_resus`/`long_icu`/`short_icu`/`holding`/`recovery`/`kia_treat`/`icu_gating`/`post_op_hold`; see [Core Trajectory](#core-trajectory), [R2B Trajectory](#r2b-trajectory), and [R2E Heavy Trajectory](#r2e-heavy-trajectory)) describe how long a clinical task takes and when a gating threshold triggers, not how effective that task is. Only the treatment efficacy factors that modify DOW risk ([Treatment Efficacy Modifiers](#treatment-efficacy-modifiers)) are treated as era-specific; the task durations themselves are held constant across profiles.

---

## Casualty Priorities

<small>[Return to Top](#contents)</small>

The following casualty priority rates were used with the rates requiring surgery:

- **Priority 1**. 65% of casualties with 90% requiring surgery.

- **Priority 2**. 20% of casualties with 80% requiring surgery.

- **Priority 3**. 15% of casualties with:
  
  - 40% of DNBI requiring surgery.
  
  - 60% of other priority 3 casualties requiring surgery. 

The values reflect planning assumptions for a brigade-level deployment rather than empirical data from a named conflict.

---

## Return to Duty

<small>[Return to Top](#contents)</small>

Return to duty (RTD) is modelled at three echelons and decomposed into two operationally distinct sub-types:

- **Battle fatigue RTD (R1 only):** Battle fatigue casualties (DNBI sub-type 1, 25% of DNBI) are held at R1 and returned to duty without R2 routing or clinical treatment. An entity receives a `return_day` attribute when it completes the R1 hold timeout. Because the 30-day simulation may end before all battle fatigue entities complete their hold, `bf_rtd` is bounded by simulation end and will be less than the total number of battle fatigue casualties generated.

- **Clinical RTD (R1, R2B, R2E):** All other casualties assigned `return_day` constitute clinical RTDs — Priority 3 WIA and NBI cases completing R1 recovery, disease cases discharged from R2B holding beds, and post-surgical cases discharged from R2E holding beds. `clinical_rtd` is assigned at the echelon where the hold-bed discharge occurs.

`total_rtd = bf_rtd + clinical_rtd`. The decomposition preserves the operational distinction between forward behavioural health management (which conserves R2 capacity) and clinical treatment efficacy at each Role 2 echelon.

Per [[9]](#references), historical in-theatre return-to-duty rates for those admitted to MTFs ranged from 7.6% (U.S. Indo-Pacific Command) to 42.1% (Republic of Vietnam) and 33.4% (CONUS). These figures are used as external validity comparators.

---

## Died of Wounds

<small>[Return to Top](#contents)</small>

The simulation implements a time-dependent Died of Wounds (DOW) probability model calibrated from combat casualty survival literature, so that DOW risk is sensitive to queue saturation and evacuation delay rather than reflecting a flat, time-invariant rate.

This allows the incorporation of the well-documented relationship between time from injury and preventable death. An analysis of 4,596 battlefield deaths during Operations Enduring Freedom and Iraqi Freedom [[29]](#references) found that 87.3% resulted from haemorrhage, with the majority occurring within 30–90 minutes of injury. A study of a "golden hour" policy mandating surgical capability within 60 minutes of injury [[30]](#references) found it reduced preventable prehospital death rates from 32% to 3.5% in a Special Operations context, establishing a direct empirical link between time-to-care and survivability.

### Survival Function

DOW probability for each priority cohort is modelled as a shifted logistic function of elapsed time since injury:

$$
F(t) = p_{base} + \frac{p_{max} - p_{base}}{1 + e^{-k(t - t_{mid})}}
$$

where $t$ is elapsed minutes since injury, $p_{base}$ is the irreducible DOW probability at $t = 0$ (representing immediately non-survivable injury independent of care speed), $p_{max}$ is the asymptotic ceiling (representing the fraction of casualties that will die without timely definitive care), $k$ controls the steepness of the rise, and $t_{mid}$ is the inflection point in minutes.

![DOW Survival Function](../images/dow_survival_function.png)

The plotted curves show DOW probability $F(t)$ for the P1 (urgent, red) and P2 (priority, blue) cohorts. Both curves are near-flat before 60 minutes — the window in which most simulated casualties reach R1 treatment — before rising through the critical window (shaded, 60–180 min), the period of greatest time-sensitivity. The dashed horizontal lines show the $p_{max}$ asymptotes; the curves approach but never exceed these ceilings.

### Parameter Calibration

| Priority        | p_base | p_max | k (min⁻¹) | t_mid (min) |
| --------------- | ------ | ----- | --------- | ----------- |
| P1 (urgent)     | 0.001  | 0.023 | 0.04      | 120         |
| P2 (priority)   | 0.0005 | 0.019 | 0.025     | 180         |
| P3 (non-urgent) | —      | —     | flat      | 0.001       |

The logistic shape parameters ($k$, $t_{mid}$) are anchored to the haemorrhagic shock critical window. The majority of potentially survivable haemorrhagic deaths occur within 60–180 minutes post-injury [[29]](#references). The inflection point $t_{mid} = 120$ minutes centres the logistic rise within this window; the P2 inflection is set to 180 minutes, reflecting the lower urgency of the Priority 2 cohort.

The ceiling $p_{max}$ and floor $p_{base}$ values, and the shape parameters, are calibrated to the Falklands War 1982 (Operation CORPORATE) historical DOW outcome rather than fitted to a per-minute individual-level survival curve, which no published dataset provides. The historical target is a **treated-cohort** mortality rate, and the population it covers is stated here explicitly because the model must be compared against the matching cohort. An account of the Ajax Bay Advanced Surgical Centre records that over 650 combat casualties from both sides passed through the facility, of whom three died of wounds [[14]](#references), giving a rate of approximately 0.46% among casualties who reached forward surgical care alive. The model's corresponding cohort is the set of casualties that reach an R2B or R2E facility. At the shipped ceilings (`p1_p_max` = 0.023, `p2_p_max` = 0.019) that cohort's mortality is 0.44%, with a 95% confidence interval of [0.36%, 0.51%] that spans the 0.46% target, which is agreement because the target is an upper bound (see [Further Development](#further-development), L22). That figure pools three independent 50-replication measurements (150 replications in total), which is the precision this quantity requires: died of wounds averages under one death per replication, and the three measurements behind it returned 0.485%, 0.402% and 0.422% individually. The shape parameters are anchored to the aggregate mortality time-window analysis in [[29]](#references) and [[30]](#references); the logistic form itself is a standard S-shaped approximation for time-dependent failure processes [[41]](#references).

Two properties of this target govern how it may legitimately be used. First, the denominator counts casualties who survived long enough to reach a surgical facility, so the rate is not a whole-of-WIA DOW rate and must not be compared against one: applied to the entire wounded population it would understate mortality, because casualties who died before reaching care are excluded from the historical denominator by construction. Second, the denominator is reported as "over 650" rather than as an exact count, so 0.46% is an upper bound on the true treated-cohort rate rather than a point estimate, and the confidence interval above is the meaningful test rather than the agreement of the central value.

A second published cohort from the same campaign is not used as the calibration target, and the reason is recorded here because the two figures differ by a factor of roughly three. Four Army Field Surgical Teams operated on 233 casualties across Ajax Bay, Teal Inlet and Fitzroy, with three deaths [[13]](#references), a rate of 1.29% among casualties selected for surgery. That cohort is a more severely injured subset than the model's operated cohort, which comprises roughly 110 of some 318 casualties per run, so the two populations are not comparable. Forcing the model's operated cohort to 1.29% requires ceilings near `p1_p_max` = 0.08, which drive whole-of-WIA DOW to approximately 3.6% and 5.4 deaths per run, far above anything the campaign record supports. The 1.29% figure is therefore retained as corroborating evidence that 1982 forward surgery was highly effective, not as a target.

A lower $p_{max}$ caps how high DOW probability can rise even under severe queue saturation or evacuation delay, so the total DOW count becomes less sensitive to those conditions. A later $t_{mid}$ delays the point at which DOW probability starts climbing steeply, so the model becomes less sensitive specifically to delays that occur early, at R1.

$p_{max}$ and the treatment efficacy factors ([Treatment Efficacy Modifiers](#treatment-efficacy-modifiers), below) were calibrated together: $p_{max} = 0.023$ reproduces the treated-cohort target specifically in combination with the OIF/OEF-era multipliers detailed there, so the two are entangled. The `moderate_intensity` scenario profile ([Scenario Profiles](#scenario-profiles)) resolves this by pairing era-appropriate (weaker) treatment efficacy factors with an independently re-calibrated, lower ceiling, approaching the same historical bound through a mechanistically consistent route and settling below it; the `default` scenario runs this base configuration, retaining the OIF/OEF-era factors.

### Multi-Echelon Check and Conditional Increment

DOW checks are performed at four points in the trajectory: on completion of R1 treatment, on arrival at R2B (after hold bed seizure), on arrival at R2E, and on completion of post-operative recovery at R2E (ICU or holding bed — see [Post-Operative Checkpoint](#post-operative-checkpoint) below). To avoid double-counting mortality across echelons, the probability applied at each check after the first is a conditional increment — the additional mortality risk accumulated since the previous check — rather than the cumulative probability:

$$
p_{conditional} = \max\left(0, \frac{F(t_{now}) - F(t_{prev})}{1 - F(t_{prev})}\right)
$$

where $t_{prev}$ is the elapsed time at the previous DOW check and $t_{now}$ is the elapsed time at the current check. A `last_dow_t` attribute records the absolute simulation time of the most recent check for each entity. A casualty who survives the R1 DOW check (at $t \approx 20$ minutes) and arrives at R2B after a 30-minute transport ($t \approx 50$ minutes) has the conditional increment $\frac{F(50) - F(20)}{1 - F(20)}$ applied, not the full $F(50)$. This ensures that system-wide mortality integrates correctly across echelons.

Disease DNBI and battle fatigue DNBI are exempt from DOW checks at all echelons, consistent with their non-traumatic injury mechanisms. NBI and WIA follow the full time-dependent DOW pathway.

### Treatment Efficacy Modifiers

The logistic function $F(t)$ characterises the mortality trajectory of a casualty receiving no further care — the probability of death given indefinite delay from the current state. Without modification, this ceiling ($p_{max}$) would apply equally at each subsequent DOW check regardless of care received: a P1 casualty who has undergone R2B damage control resuscitation and surgery would face the same asymptotic mortality ceiling (0.023) on arrival at R2E as a casualty who received no treatment, which overstates residual mortality risk for the treated population and removes the incentive structure by which the model should reward timely definitive care.

To address this, the model introduces a per-entity `dow_ceiling` attribute, initialised to the priority-appropriate $p_{max}$ at casualty entry. After each care phase completes, `dow_ceiling` is multiplied by a treatment efficacy factor, reducing the effective ceiling applied at the next DOW check:

$$
\text{dow\_ceiling} \leftarrow \text{dow\_ceiling} \times \text{treatment\_efficacy\_factor}
$$

The $p_{base}$ term is held fixed throughout: it represents non-survivable injuries (non-compressible truncal and junctional haemorrhage, unsurvivable CNS trauma) for which no care can alter the outcome. Only the treatable fraction of the ceiling is reduced.

| Care phase                 | Factor | Rationale                                                                                                                                                                                                                                                                                                                                                      |
| -------------------------- | ------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| R1 TCCC                    | 0.83   | Non-compressible haemorrhage (truncal, junctional) is identified as the mechanism in 90% of potentially preventable battlefield deaths [[29]](#references) — injuries beyond the scope of TCCC intervention. TCCC skills (tourniquet, wound packing, airway management) address the remaining 10%, yielding a modest 17% ceiling reduction. |
| R2B DCR (resus)            | 0.56   | Damage control resuscitation with balanced haemostatic products reduces laparotomy mortality from 22% to 13% [[31]](#references) — a 41% relative reduction — reflecting the haemostatic benefit of early plasma and platelet administration.                                                                              |
| R2B DCS (surgery)          | 0.32   | The PROMMTT study [[32]](#references) reported a 40% overall mortality rate in massively transfused surgical patients, with exsanguination accounting for 33.3% of deaths — approximately 13% haemorrhage-specific post-DCS mortality. This implies a 68% relative reduction from the pre-DCS ceiling, applied as a factor of 0.32.                |
| R2E DCR (resus)            | 0.56   | Same factor as R2B DCR [[31]](#references); applied only when full resuscitation occurs at R2E (i.e., the casualty bypassed R2B). Casualties pre-resuscitated at R2B receive a short resus at R2E; this factor is not re-applied, avoiding double-counting the DCR effect.                                                                                     |
| R2E DCS 1st op             | 0.25   | Post-operative mortality in optimally resuscitated DCS patients is approximately 3–5% at 30 days — a 75% relative reduction from the pre-first-DCS ceiling [[31]](#references).                                                                                                                                                                                |
| R2E DCS 2nd op             | 0.57   | Informed estimate. The second definitive procedure addresses residual injury load after initial damage control; mortality reduction is smaller than the first operation. Applied only to casualties without prior R2B DCS.                                                                                                                                     |
| R2E post-op hold (penalty) | 3.0    | Informed estimate. Applied instead of a reduction when post-operative recovery occurs in a holding bed rather than ICU, partially reversing the R2E DCS 1st op reduction to reflect the absence of continuous critical-care monitoring. See [Post-Operative Checkpoint](#post-operative-checkpoint) below.                                                                       |
| R2B forward ICU (penalty)  | 1.31   | Applied instead of a reduction to post-operative intensive care served forward at R2B rather than at R2E (see [Post-Operative Stabilisation](#post-operative-stabilisation)). A meta-analysis of open-format against closed-format intensive care units, the difference being whether a trained intensivist holds responsibility for the patient, reports a pooled odds ratio for ICU mortality of 1.31 (95% CI 1.09 to 1.59) [[59]](#references). An R2B section fields two nurses and two medics with no intensivist; an R2E section fields an intensivist and four nurses. |

The cumulative effect on a P1 casualty (initial ceiling = 0.023) who receives the full care pathway (TCCC → R2B DCR → R2B DCS → R2E DCS first op) is:

$$
0.023 \times 0.83 \times 0.56 \times 0.32 \times 0.25 = 0.00085
$$

This residual ceiling of 0.085% represents the fraction of optimally treated P1 casualties expected to die of wounds despite receiving definitive care at every echelon — consistent with the Falklands 1982 historical outcome of effectively zero post-operative deaths in patients who survived to definitive surgical care at Ajax Bay.

The multiplicative reduction factors are derived from aggregate post-care survival rates found in academic literature rather than fitted to individual-level combat casualty data, and have not been validated against a specifically comparable conflict dataset. Overestimating a factor would reduce modelled DOW sensitivity to system overload for treated casualties, while underestimating one would inflate DOW for patients who received definitive care; the relative ordering (DCS reduces the ceiling more than DCR, DCR more than TCCC) reflects clinical consensus and is unlikely to reverse under parameter uncertainty.

### Post-Operative Checkpoint

The R2E surgical trajectory performs a pre-OT ICU availability check before seizing an OT bed, since damage control surgery is established doctrine specifically because post-operative critical care is expected to follow [[25]](#references), post-operative ICU or high-dependency care is the guideline-recommended standard after major trauma surgery [[26]](#references), and bed capacity is an explicitly named constraint at deployed damage-control facilities in LSCO [[2]](#references):

1. **ICU available** — surgery proceeds unchanged; post-operative recovery is in ICU (short or full duration).
2. **ICU full, Priority 1** — surgery still proceeds (withholding it would expose a Priority 1 casualty who has not undergone surgery to near-certain DOW), but post-operative recovery is in a holding bed instead of ICU. `dow_ceiling` is multiplied by the post-op hold penalty (3.0 — Treatment Efficacy Modifiers table above) rather than a further reduction, reflecting reduced monitoring.
3. **ICU full, Priority 2+** — OT entry is deferred. The casualty polls ICU availability every `icu_gating.defer_check_interval` minutes (30, by default) without holding any resource while waiting, and proceeds as path 1 once a bed frees.

Both the ICU and post-op-hold pathways lead into the same post-operative DOW check afterward, using the same conditional-increment mechanism as the three earlier arrival-time checkpoints, each evaluated against its own `dow_ceiling`. Because both pathways share this check, their resulting mortality is directly comparable in the output (`outputs/post_op_pathway_summary.csv`; `post_op_pathway` attribute: 1 = ICU, 2 = post-op hold).

The post-definitive care that follows the definitive repair takes the same two-way split, recorded separately as `post_definitive_pathway`, and for the same reason: a casualty who has already been operated on cannot be made to wait indefinitely for a bed, so when intensive care is saturated they recover in a holding bed at the elevated ceiling instead. This is where the model's intensive care constraint now shows most clearly. At the shipped establishment of four beds, most casualties reaching this point take the holding-bed route, because the same four beds are also serving the stabilisation episode.

R2B has the same pre-OT ICU check, and at R2B only the Priority 2+ deferral rule matters, since no Priority 1 override applies there. What the check constrains depends on the forward ICU share (see [Post-Operative Stabilisation](#post-operative-stabilisation)): at the shipped share of zero the two beds per team hold only casualties waiting on an evacuation asset, and the deferral fires rarely, while at a non-zero share the same beds also carry post-operative recovery and the check becomes a real limit on how many casualties R2B can operate on at once.

Priority 1 casualties are always committed to surgery, even when no post-operative ICU bed is available, accepting elevated post-operative mortality risk in preference to withholding surgery, which would leave them facing near-certain DOW. The clinical trade-off is described in [[25]](#references) and [[2]](#references), and the standard of post-operative ICU/HDU care against which the "hold" pathway is a departure is set out in [[26]](#references); the default 3.0× penalty multiplier is an informed estimate, chosen to produce a materially higher, but not overwhelming, realised DOW rate for the hold pathway relative to ICU.

### AME Wait Checkpoint

Once a casualty is queued awaiting strategic AME (see [Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling)), the wait for evacuation capacity can be unbounded (see [Strategic Evacuation and Role 4 Demand](docs/Single_Run_Analysis.md#strategic-evacuation-and-role-4-demand)). `ame_dow_poll()` (`R/trajectories.R`) checks DOW risk periodically while the casualty waits, rather than once as at the other checkpoints, using the same conditional-increment logistic mechanism (`dow_prob_conditional()`, the same priority-based parameters and `dow_ceiling`):

1. **Capacity already available** — the casualty boards immediately, with no poll and no artificial minimum wait.
2. **Capacity unavailable** — the casualty enters a poll loop: wait `role4.ame.dow_check_interval` minutes, roll DOW (conditional on elapsed time since injury, same as every other checkpoint), then re-check AME capacity and repeat if still unavailable. A casualty who dies during this poll releases the R2E bed they were holding and is routed to KIA processing exactly as at every other DOW checkpoint. `dow_echelon = 5` distinguishes this checkpoint in `outputs/dow_by_echelon.csv` (`"ame_wait"`).

This uses the same `timeout()`-then-`rollback()` polling pattern already used for R2E OT–ICU gating deferral (`icu_gating.defer_check_interval`, [Post-Operative Checkpoint](#post-operative-checkpoint) above).

The shipped default polls every `dow_check_interval = 1440` minutes (once daily). No open-access source specifies a periodic mortality-reassessment cadence for this wait, so daily polling is an informed estimate, chosen to manage simulation cost without materially affecting outcomes. A shorter interval increases the number of conditional-increment rolls per unit time without changing the model's asymptotic DOW ceiling.

---

## Scenario Profiles

<small>[Return to Top](#contents)</small>

Casualty generation rates ([Casualty Generation](#casualty-generation)) and the DOW ceiling ([Parameter Calibration](#parameter-calibration)) are, by default, calibrated to the Falklands War 1982 (Operation CORPORATE), while the treatment efficacy factors that modify that ceiling ([Treatment Efficacy Modifiers](#treatment-efficacy-modifiers)) model modern combat casualty care [[29]](#references), [[31]](#references), [[32]](#references). This allows the user to leverage the Falklands casualty pattern, with a model of modern combat casualty care. A **named scenario profile** overlays a discrete, internally consistent parameter set onto that base configuration, which the Shiny console lists as "Falklands — Modified". Two profiles ship: `moderate_intensity`, shown as "Falklands — Unmodified", and `high_intensity`, shown as "Okinawa — Casualty Rates". The identifiers follow FORECAS's battle-intensity framing [[8]](#references); the console's labels instead name the conflict each profile is calibrated against (see [Shiny Application](#shiny-application)).

### Mechanism

Scenario profiles are defined under a top-level `scenarios` key in `env_data.json`. Each profile carries a `label`, a `source`, `notes`, and a partial `vars` override in the same shape as the base `vars` block: a list of element (`elm`) blocks, each holding `actys` entries, each of which holds `vals` entries of `var`/`val` pairs. `merge_scenario_vars()` (`R/scenario.R`) overlays a profile's `vars` onto the base at the individual variable level, so only the variables the profile names are replaced, every other variable retains its base value, and names present in a profile but absent from the base are appended. `resolve_scenario()` (`R/scenario.R`) selects a named profile, raising an explicit error listing the available profiles if the requested name does not exist, and records the selection in `active_scenario`/`active_scenario_label` so output metadata identifies which profile produced a given run. `load_scenario(path, scenario)` (`R/environment.R`) composes this with the JSON parsing and environment-building pipeline (`build_environment()`).

```r
env_data <- load_scenario("env_data.json", "moderate_intensity")   # scenario-explicit
env_data <- load_scenario("env_data.json", "default")              # base configuration
env_data <- load_elms("env_data.json")                              # equivalent to the line above
```

Every entry point that does not name a profile (`run.R`, `scripts/run_warmup.R`, `scripts/run_sensitivity.R`) calls `load_elms()` directly, so it runs the base configuration exactly as it would were the scenario mechanism absent. Profiles are selectable interactively through the Shiny console's Casualty Intensity Profile dropdown, which lists both alongside the base configuration under plainer, conflict-based labels (see [Shiny Application](#shiny-application)), and from the command line through the comparative scenario runner ([Comparative Scenario Runner](#comparative-scenario-runner)).

A distribution family is itself a scenario-specific choice, not just a distribution's parameters: FORECAS fits casualty incidence to either a lognormal or an exponential distribution depending on battle intensity, so `generators.*` entries carry an explicit `distribution` field (`"lognormal"` or `"exponential"`) alongside `mean_daily`/`sd_daily`. `generate_casualty_arrivals()` (`R/environment.R`) dispatches to `make_ln_arrival_generator()` or `make_exp_arrival_generator()` on this field, defaulting to lognormal where the field is absent. `make_exp_arrival_generator()` draws the per-minute rate via `qexp(u, rate = 1 / mean_daily)`, a single-parameter distribution.

### Parameter classification

Only variables that genuinely differ by battle intensity or historical context are scenario-eligible. See [Parameters Not Scenario-Eligible](#parameters-not-scenario-eligible) for the parameter groups this excludes.

| Parameter group                                                     | `moderate_intensity` profile                                                                     |
| ------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| Casualty generation rates and distribution family (`generators.*`)  | Inherited from base (already Falklands-sourced: FORECAS Table A.8 [[8]](#references), lognormal) |
| DOW ceiling and shape (`dow.params`)                                | **Overridden**, re-calibrated (see below)                                                        |
| DOW treatment efficacy (`dow.treatment_efficacy`)                   | **Overridden**, era-appropriate factors (see below)                                              |
| Priority distribution (`r1.priority`)                               | Inherited from base (no Falklands-specific triage data identified)                               |
| DNBI composition, surgery/evacuation probabilities (`r1.other`)     | Inherited from base (already Falklands/FORECAS-sourced where cited)                              |
| Transport time distributions (`*.wia_transport`, `*.kia_transport`) | Inherited from base (no Falklands-specific transport-time source identified)                     |

### Moderate Intensity profile (Falklands 1982 exemplar)

The `moderate_intensity` profile overrides `dow.params` and `dow.treatment_efficacy` to separate the DOW ceiling from the treatment efficacy factors it was jointly calibrated with (see [Parameter Calibration](#parameter-calibration)); the base value each factor modifies is given in [Treatment Efficacy Modifiers](#treatment-efficacy-modifiers).

| Factor                   | `moderate_intensity` | Rationale                                                                                                                                                                                                                                                                                                                                                         |
| ------------------------ | -------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| R1 TCCC                  | 1.0                  | TCCC is a post-1990s doctrine [[29]](#references); no equivalent tourniquet-forward or haemostatic-dressing prehospital doctrine is documented for 1982 British forces in the available sources, so no ceiling reduction is attributed to this checkpoint.                                                                                                        |
| R2B / R2E resuscitation  | 0.90                 | The base factor from [[31]](#references) is specific to balanced-component damage control resuscitation. A modest benefit from whole-blood/crystalloid resuscitation (available in 1982) is retained; the specific balanced-ratio benefit is not.                                                                                                |
| R2B DCS / R2E DCS 1st op | 0.55                 | Low post-operative mortality is recorded among the 233 casualties operated on by the Army Field Surgical Teams, three of whom died [[13]](#references), so definitive surgical intervention itself is retained as materially protective; the more aggressive modern base factors reflect additional staged damage-control and haemostatic-adjunct technique not available in 1982. |
| R2E DCS 2nd op           | 0.80                 | Era-appropriate weakening of the (already informed-estimate) second-operation factor, consistent with the reasoning applied to the first operation.                                                                                                                                                                                                               |
| R2E post-op hold penalty | 3.0 (unchanged)      | A within-era relative degradation factor (ICU versus non-ICU recovery) rather than a period-specific treatment technology, and therefore not scenario-eligible.                                                                                                                                                                                                   |

These treatment efficacy factors are informed estimates. They were constructed by reasoning from the absence of the specific modern techniques (TCCC, balanced DCR, staged DCS) documented in [[29]](#references), [[31]](#references), and [[32]](#references), while preserving the evidence in [[13]](#references) and [[14]](#references) that 1982 field surgery was highly effective for casualties who reached it. The paired ceiling re-calibration below absorbs the magnitude of the estimate, so the aggregate DOW rate stays close to the historical target regardless of the precise factor values; what different values would change is the distribution of mortality risk across care phases, not the aggregate rate.

With these weaker factors, `dow.params` was re-calibrated by the same iterative Monte Carlo procedure used for the base configuration, reaching the historical target at `p1_p_max` = 0.0089 and `p2_p_max` = 0.0074, down from the base 0.023 and 0.019. A lower ceiling is required to compensate for the weaker factors' smaller ceiling reduction. The floors (`p1_p_base`, `p2_p_base`), the shape parameters ($k$, $t_{mid}$ for both priorities), and the P3 flat rate (`p3_flat`) are unchanged from base under this profile; only the two ceilings are re-calibrated. Each of these terms is defined by the shifted logistic curve given in [Survival Function](#survival-function). Pooling three independent 50-replication measurements of `moderate_intensity` (30 days, 150 replications in total) produced:

| Metric                  | `moderate_intensity` (150-rep)     | Historical comparator                                              |
| ----------------------- | ---------------------------------- | ------------------------------------------------------------------ |
| Treated-cohort DOW rate | 0.27% (95% CI [0.21%, 0.33%])      | ≤0.46% (3 deaths among over 650 reaching forward surgery [[14]](#references)) |
| Mean DOW/run            | 0.61 (95% CI [0.47, 0.74])         | No directly comparable published figure                            |
| DOW/WIA rate            | 0.40% (95% CI [0.31%, 0.49%])      | Not historically constrained (see Further Development, L22)        |
| KIA:WIA ratio           | 0.377                              | 0.328 (255 killed : 777 injured [[53]](#references), [[54]](#references)) |

The treated-cohort rate, measured over the casualties that reach an R2B or R2E facility, sits below the historical comparator rather than spanning it. Because that comparator is an upper bound rather than a point estimate, sitting beneath it is consistent with the historical record, and the ceilings are left as they are: raising them to bring the interval up onto the bound would add modelled deaths that no source evidences. The base configuration reaches 0.44% (95% CI [0.36%, 0.51%]) by the different mechanistic route described in [Parameter Calibration](#parameter-calibration), an interval spanning the bound rather than sitting below it. Both figures are pooled over 150 replications rather than measured once, for the reason given there. At that precision the two intervals no longer overlap, so the profiles are separated on mortality where earlier measurements left them indistinguishable. See [Further Development](#further-development), L22. The KIA:WIA ratio still does not match, and the comparison is now a clean one: each stream realises the daily mean its configuration names (see [Casualty Generation](#casualty-generation)), so the realised ratio is the configured ratio of the two means rather than that ratio modified by whatever share the rate cap clamped from each stream, and the disagreement is attributable to the sourced rates alone. What remains is a characteristic of the base casualty generation rates that both profiles share rather than something this profile introduced, since the profile overrides only the DOW ceiling and treatment efficacy factors (see Further Development).

### High Intensity profile (Okinawa exemplar)

The `high_intensity` profile implements exponential casualty generation in line with the casualty models published in FORECAS [[8]](#references). It is not a fully validated second scenario (see Further Development).

FORECAS reports that INFANTRY (direct combat) troop WIA and KIA incidence in high-intensity battles is best approximated by a single-parameter exponential distribution, $W \sim \text{exponential}(\mu)$, rather than the lognormal distribution used at moderate and light intensity [[8]](#references). The fitted Okinawa WIA and KIA means are given alongside the base lognormal parameters in [Casualty Generation Rates](#casualty-generation-rates), and `generators.wia_cbt`/`kia_cbt` are overridden with `distribution = "exponential"` using them.

FORECAS further distinguishes three troop categories with different casualty-rate treatments: INFANTRY (ground combat troops, exponential at high intensity), SUPPORT (intra-divisional combat support such as tank, artillery, light-armoured infantry, and combat engineer, lognormal at all intensities), and SERVICE SUPPORT (extra-divisional sustainment such as Force Service Support Group and Surveillance Reconnaissance Intelligence Group, lognormal and without autocorrelation at all intensities) [[8]](#references). This simulation models a single brigade (division and below), so it has no extra-divisional service support population, and its `support` group represents an organic brigade element exposed to the same battle risk as the `combat` group rather than FORECAS's rear-area category. Both `generators.wia_cbt`/`kia_cbt` **and** `generators.wia_spt`/`kia_spt` are therefore overridden with `distribution = "exponential"` using the same Table A.7 and A.9 means. This is a considered reclassification rather than a literature-derived value, following FORECAS's own category definitions and this project's documented force structure ([Scenario Context](#scenario-context)); were the `support` group better treated as FORECAS's lognormal-always SUPPORT category, `generators.wia_spt`/`kia_spt` would revert to `distribution = "lognormal"` under this profile, somewhat reducing realised support-troop casualty counts without affecting `combat` output.

DOW ceiling, treatment efficacy factors, priority distribution, DNBI composition, and transport time distributions are not sourced for Okinawa and are inherited unchanged from the Falklands-calibrated base (see Further Development).

A 30-replication run (30 days, seed 42) of each profile produced:

| Metric                                 | `moderate_intensity` (30-rep) | `high_intensity` (30-rep) |
| -------------------------------------- | ----------------------------- | ------------------------- |
| Mean WIA/run                           | 151.1                         | 655.7                     |
| Mean KIA/run                           | 57.1                          | 155.3                     |
| WIA+KIA ratio vs. `moderate_intensity` | 1.00×                         | 3.90×                     |

Both profiles cap their draws at three times the stream's own mean (see [Casualty Generation](#casualty-generation)). Under `high_intensity` the overridden WIA and KIA streams are exponential, so that cap clamps exactly 6.0% of their draws; under `moderate_intensity` they are lognormal, so it clamps 10.7% and 10.9% respectively. DNBI is not overridden by either profile and stays lognormal under both. What is clamped no longer changes what is realised: every stream under every profile averages the daily rate its configuration names.

Mean DNBI per run falls under `high_intensity`, from 173.1 to 160.6, even though the profile leaves DNBI generation untouched. Casualty rates are set per 1,000 troops and scaled by the live force size (see [Force Regeneration and the Endogenous Feedback Loop](#6-force-regeneration-and-the-endogenous-feedback-loop)), so heavier battle attrition leaves fewer troops in theatre to fall sick.

---

## Development Environment

<small>[Return to Top](#contents)</small>

The simulation uses `parallel::mclapply` for multi-replication parallelism, which relies on `fork()`, a POSIX primitive unavailable on Windows. On a Windows RStudio installation, `mclapply` silently falls back to sequential execution rather than reporting an error, so a multi-replication run or a sensitivity screen takes roughly as many times longer as the host has physical cores (see [Sensitivity Analysis](#sensitivity-analysis) for the screening configuration and its run time).

A Dev Container specification in `.devcontainer/` defines a reproducible Linux R environment (R 4.4.2, all project packages) that can be launched from VS Code with a single command. It provides a Linux `fork()` context, RStudio Server on `http://localhost:8787`, and automatic core-count configuration, so contributors on any host OS get full parallelism and a consistent package environment without manual dependency resolution. Two ports are forwarded: 8787 for RStudio Server, and 3838 for the Shiny console (see [Shiny Application](#shiny-application)).

### Prerequisites

| Requirement                                                                                                        | Notes                                                                                                 |
| ------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------- |
| [Docker Desktop](https://www.docker.com/products/docker-desktop/)                                                  | Provides the container runtime. Enable "Use the WSL 2 based engine" on Windows.                       |
| [VS Code](https://code.visualstudio.com/)                                                                          | Host IDE used to manage the container lifecycle.                                                      |
| [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) | VS Code extension (`ms-vscode-remote.remote-containers`) that adds the "Reopen in Container" command. |
| Host capacity                                                                                                      | The container declares `hostRequirements` of 4 CPUs and 16 GB of memory (`.devcontainer/devcontainer.json`). Multi-replication runs scale with physical core count, so a host below this will still work but more slowly. |

### First-time setup

1. Clone the repository to the local machine:
   
   ```sh
   git clone https://github.com/natosys/Battlefield-Casualty-Handling.git
   cd Battlefield-Casualty-Handling
   ```
2. Open the repository folder in VS Code: **File → Open Folder**.
3. VS Code will detect `.devcontainer/devcontainer.json` and display a notification: *"Folder contains a Dev Container configuration file. Reopen folder to develop in a container."* Click **Reopen in Container**. Alternatively, open the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`) and select **Dev Containers: Reopen in Container**.
4. VS Code builds the Docker image and starts the container. The initial build downloads the base image, pre-warms the `renv` package cache from `renv.lock`, and installs all R packages; this typically takes 5–10 minutes. Subsequent starts use the cached image and complete in seconds.
5. Once the container starts, open a browser and navigate to `http://localhost:8787` to access RStudio Server. No login credentials are required (`DISABLE_AUTH=true`). `postCreateCommand` runs `renv::restore()` against the bind-mounted workspace automatically, so the RStudio session is ready to run the simulation without any manual package installation.

### Restoring dependencies

Package versions are pinned via `renv`: `renv.lock` at the repository root records the exact CRAN version of every package required by `run.R`, `app.R`, `R/*.R`, and `scripts/*.R`. The Dev Container restores from this lockfile automatically (`postCreateCommand`, above); it is the same lockfile the `.devcontainer/Dockerfile` image build uses to pre-warm its package cache, so the container and any host RStudio installation share a single source of truth for package versions.

Contributors running R directly on the host (outside the Dev Container), or after pulling a branch that updates `renv.lock`, should restore the pinned versions with:

```r
install.packages("renv")  # first-time only, if not already installed
renv::restore()
```

`renv::status()` can be used afterwards to confirm the project library matches `renv.lock` exactly.

### RStudio Server configuration

After connecting to RStudio Server at `http://localhost:8787`:

1. Set the working directory to the workspace mount point:
   
   ```r
   setwd("/home/rstudio/workspace")
   ```
   
   This can be made permanent via **Tools → Global Options → General → Default working directory**.
2. Verify the parallel core configuration:
   
   ```r
   parallel::detectCores(logical = FALSE)  # should return > 2 on a multi-core host
   getOption("mc.cores")                   # should match the above
   ```
   
   Both values are set automatically by `Rprofile.site` during the image build; no per-session configuration is required.

### Running the simulation with full parallelism

From the RStudio Server terminal or console, all `Rscript` invocations work identically to the host workflow:

```sh
# Standard single run (seed 42, 30 days, 1 iteration)
Rscript run.R --seed 42 --days 30 --iterations 1

# Multi-run replication (100 iterations, parallel via mclapply)
Rscript run.R --days 30 --iterations 100

# Quick smoke test (5 days, 5 iterations)
Rscript run.R --quick
```

`mclapply` will use all physical cores reported by `parallel::detectCores(logical = FALSE)`, providing linear scaling up to the host core count. `--seed` takes an integer and defaults to 42; it is not given a null value to obtain independent replications, since `run_replications()` already draws a separate L'Ecuyer-CMRG stream for each worker.

### Git workflow

All files are bind-mounted from the host filesystem into `/home/rstudio/workspace` inside the container (`workspaceMount`, `.devcontainer/devcontainer.json`). Git commits and pushes can be made from either location, since changes are immediately visible on both sides.

---

## Simulation Design

<small>[Return to Top](#contents)</small>

The simulation is built as a Discrete Event Simulation (DES), it is written in R  using the simmer package [[15]](#references). DES has been used as a proven way to simulate healthcare systems and support healthcare decision-making (as shown in [[16]](#references)).

### Codebase Structure

The codebase is organised into a modular layout under an `R/` directory, with a single CLI entry point (`run.R`). The split allows each module to be tested and extended independently, and provides a clear separation between data loading, simulation logic, execution, and analysis.

| File / Directory                   | Purpose |
| ---------------------------------- | ------- |
| `run.R`                            | CLI entry point; parses arguments, orchestrates modules, and writes outputs |
| `env_data.json`                    | All simulation parameters: populations, health system establishment, transport fleet, distributions, schedules, and the named scenario profiles under its `scenarios` block (see [Model Parameters and Model Configuration](#model-parameters-and-model-configuration)) |
| `R/environment.R`                  | Data import (`load_elms`, `load_scenario`, `build_environment`), arrival generation (`generate_casualty_arrivals`, `make_ln_arrival_generator`, `make_exp_arrival_generator`), and simmer environment construction (`build_env`) |
| `R/trajectories.R`                 | All simmer `trajectory()` definitions: R1, R2B, R2E, and core casualty flow |
| `R/replication.R`                  | Multi-run replication framework: `run_once` (single replication with `wrap()`), `run_replications` (parallel `mclapply` over *n* replications), and `summarise_replications` (time-weighted KPI summary with 95% CI) |
| `R/analysis.R`                     | Analysis and visualisation pipeline (`analyse_run`); accepts monitoring data objects rather than reading from hardcoded CSV paths |
| `R/sensitivity.R`                  | Morris EE screening (`run_morris`) and Sobol variance decomposition (`run_sobol`); holds the `morris_params` bounds table, `apply_params` for env_data override, and `eval_params` for single design-point evaluation |
| `R/warmup.R`                       | Welch warm-up analysis: `compute_welch_cma`, `plot_welch`, `run_welch_analysis`, and the `WARM_UP_DAYS` constant |
| `R/app_params.R`                   | Parameter registry for the Shiny Configure panel: plain-English labels, tooltips, and get/set accessors for every editable `env_data.json` field, keyed to Morris screening bounds where applicable |
| `R/scenario.R`                     | Scenario overlay mechanism: `resolve_scenario()` applies a named profile's overrides, and `merge_scenario_vars()` merges them variable by variable onto the base configuration. The profiles themselves are defined in `env_data.json`, not here |
| `R/scenario_runner.R`              | Comparative scenario runner; `run_scenario()`/`compare_scenarios()` execute the multi-run replication framework under a named scenario profile |
| `app.R`                            | Shiny console with Getting Started, Configure, Run, and Analyse tabs (see [Shiny Application](#shiny-application) below) |
| `scripts/run_sensitivity.R`        | CLI entry point for sensitivity analysis, with `--quick`, `--sobol`, `--r`, `--reps`, `--days`, `--n-sobol`, and `--no-dirichlet` flags |
| `scripts/run_warmup.R`             | CLI entry point for Welch warm-up analysis |
| `scripts/run_scenarios.R`          | CLI entry point for the comparative scenario runner (see [Comparative Scenario Runner](#comparative-scenario-runner)) |
| `scripts/run_transport_sweep.R`    | CLI entry point for the transport fleet-size sweep (see [Transport Fleet Capacity Margin](docs/Single_Run_Analysis.md#transport-fleet-capacity-margin)) |
| `scripts/run_icu_share_sweep.R`    | CLI entry point for the forward ICU share sweep (see [Post-Operative Stabilisation](#post-operative-stabilisation)) |
| `scripts/shiny_worker.R`           | Background worker script sourced by `app.R` to run Quick Run / Full Analysis asynchronously without blocking the Shiny session |
| `scripts/check_env_data_summary.R` | Regenerates the `<!-- ENV SUMMARY START/END -->` block within this README directly from `env_data.json` |
| `scripts/check_markdown.R`         | Maintains the table of contents and "Return to Top" links across this README and the two analysis documents, reproducing the anchor GitHub itself generates for each heading; rejects any heading containing emoji, and reports any anchor link pointing at no heading |
| `scripts/check_r2e_surgery_seizure.R` | Checks that every R2E surgery seizes a surgical section, by reading the built trajectory and by running the model; exits non-zero on failure |
| `scripts/check_icu_time_conservation.R` | Checks that a casualty's post-operative ICU requirement is conserved across all three routes and at every forward ICU share; exits non-zero on failure |
| `scripts/check_composition_ilr.R`  | Checks that each simplex-constrained composition survives the round trip through its balance coordinates, at every design point of a Morris design and at each corner of each coordinate box; exits non-zero on failure |
| `scripts/check_morris_baseline.R`  | Checks that every screened parameter's baseline lies inside its own screening bounds and equals the value it holds in `env_data.json`, deriving the parameter-to-path mapping from `apply_params()` rather than restating it; exits non-zero on failure |
| `scripts/check_dow_calibration.R`  | Checks that neither shipped configuration's treated-cohort died-of-wounds rate overshoots the Ajax Bay historical bound, pooling independent measurements because one does not resolve a response averaging about one death per replication; exits non-zero on failure |
| `scripts/check_replication_independence.R` | Checks that `run_once()` is a pure function of its seed and that `run_replications()` draws a distinct seed per replication, which together make the replications independent; exits non-zero on failure |
| `scripts/check_scenario_labels.R`  | Checks that the comparative scenario plot renders in a C locale and byte-for-byte matches the same plot rendered under UTF-8, reaching the plotting stage from a synthetic queue table rather than from a full replication run; exits non-zero on failure |
| `scripts/check_pre_open_window.R`  | Checks the R2B pre-open hold window at its bounds: that a window of zero reproduces the instant-diversion model bit-for-bit, that `minutes_to_shift_open()` agrees with the roster at every minute of the day, and that every casualty held forward is operated on there rather than diverted anyway; exits non-zero on failure |
| `renv.lock`, `.Rprofile`, `renv/`  | Pinned package versions and the `renv` project library (see [Restoring dependencies](#restoring-dependencies)) |
| `.devcontainer/`                   | Dev Container definition pinning the reproducible R 4.4.2 Linux environment (see [Development Environment](#development-environment)) |
| `outputs/`                         | Generated outputs directory; every run writes its CSVs, markdown tables, plots (`outputs/images/`), console log and arrival diagnostics (`outputs/data/`) here. Tracked via `.gitkeep` and otherwise gitignored |
| `data/`                            | Read-only input data plus a small set of diagnostic/event files (`arrivals_*.txt` per-casualty-type diagnostics, `mass_casualty_events.csv`) forming part of the tracked seed-42 baseline, rewritten only under `--refresh-baseline` |
| `images/`                          | Tracked seed-42 baseline plots and reference diagrams, rewritten only under `--refresh-baseline` as part of PRs that shift the RNG stream or simulation outputs |
| `logs/`                            | Tracked seed-42 baseline console log (`logs.txt`), rewritten only under `--refresh-baseline` |
| `docs/`                            | Project documentation: the two companion analysis documents, the action plan, the task-role allocation supplement, the R code style guide, and the in-app Getting Started guide (`Getting_Started.md`, also rendered inside `app.R`'s Getting Started tab) |

#### Running the simulation

```sh
# Standard single run (seed 42, 30 days, 1 iteration)
Rscript run.R --seed 42 --days 30 --iterations 1

# Custom run
Rscript run.R --seed 99 --days 14 --iterations 10

# Quick smoke test (5 days, 5 iterations, seed 42)
Rscript run.R --quick

# Exclude a warm-up period from the analysis window
Rscript run.R --days 30 --iterations 10 --warm-up 5

# Regenerate the tracked seed-42 baseline evidence set
Rscript run.R --seed 42 --days 30 --iterations 1 --refresh-baseline
```

`--seed` takes an integer and defaults to 42, `--days` defaults to 30, and `--iterations` defaults to 1. `--warm-up` sets the number of days excluded from the start of the analysis window, defaulting to the `WARM_UP_DAYS` constant in `R/warmup.R`, which currently ships at 0 (see [Warm-up Period Analysis](#warm-up-period-analysis) below for why).

Artifacts fall into two categories, distinguished by whether they are a disposable record of one particular run or the repository's tracked regression evidence. Every run writes only the first category, all of it beneath `outputs/`, which is gitignored apart from its `.gitkeep`. The tracked baseline set is written only when `--refresh-baseline` is passed, and then every part of it is written together from that one run, so no invocation can leave `images/`, `logs/logs.txt` and `data/` describing a mixture of different runs:

| Artifact | Default destination | Under `--refresh-baseline` | Single run | Multi-run |
|---|---|---|---|---|
| Monitoring CSVs and markdown tables | `outputs/` | `outputs/` | Yes | Yes |
| KPI summary (`replication_summary.csv`) | `outputs/` | `outputs/` | Yes | Yes |
| Plots | `outputs/images/` | `images/` (tracked) | Yes | Yes |
| Console log | `outputs/logs.txt` | `logs/logs.txt` (tracked) | Yes | No |
| Arrival diagnostics (`arrivals_*.txt`, `mass_casualty_events.csv`) | `outputs/data/` | `data/` (tracked) | Yes | No |

The console log and the arrival diagnostics record one specific run's event stream and have no multi-replication equivalent: the parallel replication workers cannot write concurrently to a single path, and an aggregate of thirty arrival streams would not be a diagnostic of any of them. `--refresh-baseline` therefore requires `--iterations 1` and stops with an explanatory error otherwise, since a multi-run refresh could only ever produce part of the set. The KPI summary is emitted in both modes, computed by `summarise_replications()` on the monitoring structure both paths produce; at one iteration its dispersion and confidence-interval columns are `NA`, there being no second observation from which to estimate spread.

Not every file in `images/` belongs to the baseline set. The directory also holds reference diagrams that no run produces, figures written by the other entry points in `scripts/` (Morris screening, the Welch warm-up plot, the scenario comparison, the transport fleet sweep), and figures deliberately generated under a non-default configuration, such as the mass casualty event timeline, which requires `mass_casualty.event.rate_per_day` to be set above its shipped value of zero. `--refresh-baseline` rewrites only the figures a seed-42 run produces under the shipped configuration and leaves the rest untouched, so those categories must be regenerated by whichever command produced them.

Package versions are pinned via a committed `renv.lock`; see [Restoring dependencies](#restoring-dependencies) for the `renv::restore()` workflow.

#### Multi-run Replication Framework

The simulation supports Monte Carlo replication via `run_replications(n_iterations, n_days)` in `R/replication.R`. When `--iterations` is greater than 1, each replication:

1. Builds a fresh `simmer` environment from `env_data.json`, with arrival streams drawn from whichever generator the active configuration selects for each stream, lognormal or exponential (see [Casualty Generation](#casualty-generation)).
2. Runs to completion and snapshots monitoring state with `wrap(env)`, which captures arrivals, attributes, and resource utilisation without holding the live environment in memory.
3. Returns all replication data aggregated by `get_mon_arrivals(envs)` / `get_mon_resources(envs)` / `get_mon_attributes(envs)`, which append a `replication` index column ($1 \ldots n$) to each row.

Replications are dispatched in parallel via `mclapply` whenever the platform is not Windows and more than one replication is requested; any other case runs them sequentially through `lapply`. The parallel path sets `RNGkind("L'Ecuyer-CMRG")` before the call and passes `mc.set.seed = TRUE`, which assigns each worker a distinct substream of the underlying MRG32k3a generator. That generator has period $\rho \approx 2^{191}$ with substream spacing $2^{76}$, so stream overlap is impossible within any realistic simulation workload; the practical application of this mechanism in R via the `parallel` and `rstream` packages is demonstrated in [[43]](#references). The worker count is taken from the `mc.cores` option where set, falling back to `parallel::detectCores(logical = FALSE)`, the physical rather than logical core count.

Non-overlapping streams are assumed rather than measured here, on the basis that the R `parallel` package documentation states the mechanism is designed to "use a separate stream for each of the parallel computations (which ensures that the random numbers generated never get into sync)" [[44]](#references), and that the non-overlap property follows mathematically from the substream period given any simulation budget used in this study. Were the assumption wrong, replications would be correlated, understating variance and producing confidence intervals that are too narrow.

**Replications are independent of one another**, each drawing its own seed from the parent stream. The replication is therefore the unit of analysis, and every interval this project reports is correctly specified in dividing by the replication count [[63]](#references). No variance reduction scheme is applied on top of that, because none of practical benefit is available to this model. Antithetic pairing, in which paired replications draw $U$ and $1 - U$ so that $\mathrm{Cor}(X, X') < 0$ reduces the variance of their mean [[45]](#references), fails on both of its preconditions here. Its reach is limited to the arrival generators, since simmer draws service times and routing probabilities from the global stream inside its own event loop in an order set by event timing rather than by entity, so paired replications would share an unnegated treatment stream and any treatment-driven response would take the pairing's cost, a halving of the count of independent observations, without its benefit. And the technique needs the response to be monotone in the input uniforms, which casualty arrivals are not: they are produced by a threshold accumulator whose rate is scaled by an effective force size that trajectory outcomes themselves debit and credit (see [Force Regeneration and the Endogenous Feedback Loop](#6-force-regeneration-and-the-endogenous-feedback-loop)), so reflecting the arrival uniforms does not reflect the arrival count.

Measurement bears that out. Over 75 pairs of the shipped configuration the within-pair correlation on total casualties, the response the reflection actually reaches, is $-0.04$ (95% CI $[-0.27, +0.19]$), worth a variance reduction of about 3% and indistinguishable from none; on the died-of-wounds count it is $-0.01$, and on R2E ICU mean queue $+0.18$. Individual 50-replication measurements of those same quantities range from $-0.25$ to $+0.65$, which is what a 25-pair correlation estimate is worth and the reason the figures above pool three of them.

Independence is a property of the construction rather than something inferred from those correlations, and `scripts/check_replication_independence.R` asserts it as one. Two facts establish it. `run_once()` is a pure function of its seed, re-running a seed reproducing its output exactly even with a different run in between, so the seed is the whole of what distinguishes two replications and no other channel exists between them; and `run_replications()` draws a distinct seed per replication, a repeated seed being exactly what the withdrawn pairing did. Independent seeds into a pure function give independent replications, which is a stronger statement than a correlation measured on any finite sample supports, and a deterministic one. The check also reports the lag-1 rank correlation across replications without gating on it, for the reason a gate would be misleading: at the shipped control seeds the R2E ICU mean queue reads $+0.18$ ($p = 0.02$) at lag 1 and $-0.16$ ($p = 0.03$) at lag 3, the signature of a skewed response on a finite sample rather than of a dependence, and the same figure appears whether replications share an `mclapply` fork or run one fork each.

A key-performance-indicator summary is computed by `summarise_replications(mon)` using the time-weighted mean queue per replication as the unit of analysis. The across-replication summary reports mean, p10, p90, max queue, and a 95% confidence interval ($t$-distribution, $\mathit{df} = n - 1$) for each resource, sorted descending by mean queue. `run.R` writes it to `outputs/replication_summary.csv` in both run modes; where $n = 1$ the standard deviation and both interval bounds are reported as `NA`, since a single replication supplies no estimate of dispersion.

#### Warm-up Period Analysis

Discrete event simulations are classified as either **terminating** or **steady-state** based on the nature of the system being modelled [[41]](#references). A terminating simulation has a natural, finite end state (for example, an operational campaign concluding after a defined horizon); the run begins under well-defined initial conditions, and behaviour across the entire horizon, including the start-up period, is of direct interest. A steady-state simulation models a perpetual system in which the long-run equilibrium is the quantity of interest; here, the initialisation transient is an artefact that must be discarded before meaningful statistics can be collected. The choice of classification governs whether warm-up exclusion is appropriate.

Welch's graphical method [[40]](#references) was applied to characterise the simulation's time-varying behaviour and determine which classification applies. The method involves: (1) running at least 10 independent replications of an extended simulation (90 days); (2) computing the cross-replication cumulative moving average (CMA) of a sensitive KPI at each time point; and (3) determining whether the CMA converges to a stable level. The R2E ICU queue was selected as the KPI, being the most congestion-sensitive resource in the model.

The analysis is implemented in `R/warmup.R` and can be executed from the repository root:

```bash
# Full analysis: 10 replications, 90 days
Rscript scripts/run_warmup.R

# Reduced run for testing
Rscript scripts/run_warmup.R --reps 5 --days 60
```

The resulting Welch plot shows the cross-replication CMA of the R2E ICU queue across 90 days.

![Welch plot of the R2E ICU queue CMA across 90 days](../images/welch_plot_icu_queue.png)

The CMA rises from 0 at Day 0 to 0.30 at Day 89 and is still climbing gently when the run ends, with 22.5% of the 2,159 hourly increments a decrease. The instantaneous cross-replication mean queue peaks at 1.1 casualties waiting on a pool of four R2E ICU beds. This is a far milder picture than the model produced while every critical-route evacuee held an ICU bed for the whole of its wait for an aircraft, when the same measurement reached 30.6 with no decreasing increment at all: the queue no longer accumulates without bound. What it does not do is settle. The curve is still rising at the end of a 90-day horizon, which is what a campaign-long build-up of clinical demand looks like rather than a start-up transient overlaid on a steady state.

This non-settling CMA is consistent with the battlefield casualty handling simulation being a **terminating simulation** [[41]](#references), which is what its structure independently establishes. The campaign has a defined finite horizon; the ICU queue trajectory represents the operational reality of that campaign, including the initial build-up of casualties from Day 1. The empty-start initial condition, no casualties in care on Day 0, is the correct operational initial condition for a force beginning operations rather than a modelling artefact to be excluded. [[42]](#references) establishes that warm-up detection methods, including graphical approaches, presuppose the existence of a steady state; they are not applicable to terminating simulations. Uncertainty on this classification is low, since it follows from the finite campaign model structure rather than from a parameter subject to calibration. Were it wrong, and early data discarded as a transient, the reported KPIs would describe mid-campaign equilibrium rather than the campaign-wide casualty burden from Day 1, understating both total system demand and the severity of early-period queues for a planner who must account for casualty load from the onset of operations.

Warm-up exclusion is therefore **not applied** as the default. The `WARM_UP_DAYS` constant in `R/warmup.R` is set to `0L`. All KPI summaries and analysis outputs use the full observation window.

The `--warm-up` CLI flag remains available for **parametric comparison runs**, such as sensitivity screening and scenario analysis, where a researcher wishes to study mid-campaign behaviour net of start-up effects, or where two scenarios differ in their initialisation characteristics and the comparison requires a common time base:

```bash
# Optional: exclude first 10 days for parametric comparison runs only
Rscript run.R --iterations 50 --days 60 --warm-up 10
```

#### Sensitivity Analysis

The triangular distribution parameters carry significant epistemic uncertainty. The conclusion that a particular resource is the primary system constraint may shift under plausible parameter perturbations. Without sensitivity analysis, no parameter can be identified as rate-limiting versus incidental to the result, and findings cannot be used to prioritise capability investments.

**Morris Elementary Effects (EE) screening** [[47]](#references) was applied using R's `sensitivity` package [[48]](#references). Morris EE is a global, one-at-a-time (OAT) method that identifies the few influential parameters from a larger set at low computational cost, requiring r × (p + 1) model evaluations, where r is the trajectory count and p is the number of parameters. It produces two statistics per parameter: µ\* (the mean absolute Elementary Effect, indicating overall influence) and σ (the standard deviation of Elementary Effects, indicating nonlinearity and interaction). Parameters with large µ\* and small σ have large, approximately linear effects; large µ\* and large σ indicate nonlinear or interaction-dominated effects.

**Parameter coverage.** The screened set is derived from the full parameter surface rather than by expert selection. Every numeric leaf in `env_data.json`'s `vars` tree is enumerated by combining `R/app_params.R`'s `build_param_registry()` (286 fields, the same registry the Shiny Configure panel renders from, each carrying a `path` into the vars tree and, where established, a `source` citation) with a direct read of `env_data.json` for the parameters that are calibrated constants rather than user-editable fields, namely the DOW logistic curve's shape and base terms and the treatment efficacy multipliers (see [Died of Wounds](#died-of-wounds)).

Two classes of parameter are then held out of the screen. Polling-loop intervals are excluded because they discretise continuous monitoring rather than represent a decision a planner could make, and the categories listed under [Parameters Excluded from Screening](#parameters-excluded-from-screening) are excluded for the reasons given there. The three casualty composition splits, previously held out as a third class, are now screened through a transform that satisfies their sum-to-one constraint by construction ([Simplex-Constrained Compositions](#simplex-constrained-compositions)).

Each screened parameter carries a baseline alongside its bounds, and the baseline does more work than the tables below suggest. Morris moves every parameter across its own range and never reads the baseline at all. A Sobol decomposition instead selects a subset and holds every parameter outside it fixed, at its baseline, at each of the $N \times (p+2)$ design points, so a baseline that has drifted from the shipped configuration decomposes the variance of a system nobody described. The Shiny Sensitivity Calibration tab displays the same column to a planner. Because neither use is exercised by an ordinary run, `scripts/check_morris_baseline.R` asserts the agreement rather than leaving it to inspection: that every baseline lies inside its own bounds, and that applying the whole baseline vector through `apply_params()` leaves `env_data.json` unchanged, which is precisely the condition under which a Sobol run's held-fixed background is the shipped configuration whatever subset it selects. The parameter-to-path mapping the second assertion needs is derived by perturbing each parameter in turn and observing which values move, so the check cannot itself drift from the code it describes.

Sixty-four parameters are screened, spanning the main uncertain inputs across all three echelons plus the casualty generation, force regeneration, and strategic evacuation subsystems. Fifty-eight are ordinary scalars whose bounds are set using one of two rules, described below: **Rule A** (citation-anchored, moderate uncertainty) spans approximately baseline ±40%; **Rule B** (informed estimate, no literature anchor) spans baseline ×0.5–×2.0 (duration/rate parameters) or approximately baseline ±0.15–0.25 (probabilities), clipped to a clinically sensible range. The remaining six are the balance coordinates of the three casualty composition splits, which take their bounds by a transformation of a compositional range rather than from either rule (see [Simplex-Constrained Compositions](#simplex-constrained-compositions)).

**R1 — Forward Aid Post**

| Parameter                       | Variable               | Baseline | Lower | Upper | Rule |
| ------------------------------- | ---------------------- | -------- | ----- | ----- | ---- |
| R1→R2B transport time           | `r1_transport`         | 30 min   | 15    | 45    | A    |
| WIA treatment time              | `r1_wia_treat_mode`    | 20 min   | 12    | 28    | A    |
| Battle fatigue hold duration    | `r1_recovery_mode`     | 2880 min | 1440  | 5760  | B    |
| P1 surgical candidacy           | `pri1_surg_prob`       | 90%      | 70%   | 98%   | A    |
| P1 damage control rate          | `pri1_dcs_rate`        | 55%      | 30%   | 80%   | B    |
| P2 damage control rate          | `pri2_dcs_rate`        | 20%      | 8%    | 40%   | B    |
| P3 damage control rate          | `pri3_dcs_rate`        | 5%       | 0%    | 20%   | B    |
| P2 surgical candidacy           | `pri2_surg_prob`       | 80%      | 55%   | 95%   | B    |
| P3 DNBI surgical candidacy      | `pri3_dnbi_surg_prob`  | 40%      | 15%   | 55%   | B    |
| P3 other surgical candidacy     | `pri3_other_surg_prob` | 60%      | 35%   | 75%   | B    |
| Disease DNBI surgical candidacy | `disease_surgery_pct`  | 6%       | 3%    | 12%   | B    |
| P1 strategic evacuation rate    | `pri1_evac_prob`       | 95%      | 70%   | 99%   | B    |
| P2 strategic evacuation rate    | `pri2_evac_prob`       | 90%      | 65%   | 98%   | B    |

**R2B — Battalion Aid Post**

| Parameter                            | Variable             | Baseline | Lower | Upper | Rule |
| ------------------------------------ | -------------------- | -------- | ----- | ----- | ---- |
| Surgery duration (shared R2B/R2E)    | `surg_mode`          | 95 min   | 57    | 133   | A    |
| Long resuscitation duration (shared) | `long_resus_mode`    | 45 min   | 25    | 70    | A    |
| R2B→R2E transport time               | `r2b_transport`      | 30 min   | 15    | 45    | A    |
| Holding bed duration                 | `r2b_hold_mode`      | 7200 min | 3600  | 14400 | B    |
| Hold-bed reroute threshold           | `r2b_hold_threshold` | 80%      | 60%   | 95%   | B    |
| Pre-open hold window                 | `r2b_pre_open_window` | 60 min  | 0     | 360   | —    |

**R2E — Field Hospital**

| Parameter                       | Variable            | Baseline  | Lower | Upper | Rule |
| ------------------------------- | ------------------- | --------- | ----- | ----- | ---- |
| Stabilisation ICU requirement   | `stabilisation_icu_mode` | 1440 min | 770 | 2160 | A    |
| Post-definitive ICU duration    | `post_definitive_icu_mode` | 1440 min | 720 | 2880 | B    |
| Short resuscitation duration    | `short_resus_mode`  | 28 min    | 17    | 39    | A    |
| Base recovery-to-duty duration  | `r2e_hold_mode`     | 38880 min | 23400 | 54450 | A    |
| Post-op holding-bed duration    | `post_op_hold_mode` | 600 min   | 380   | 1200  | B    |
| Theatre evacuation policy       | `evacuation_policy_days` | 30 days | 15 | 60 | A    |
| Forward ICU share               | `r2b_icu_share`     | 0%        | 0%    | 100%  | —    |
| Forward hold time limit         | `r2b_forward_hold_max` | 1440 min | 0  | 2880  | —    |
| OT shift duration               | `ot_hours`          | 12 hr     | 8     | 16    | A    |

**Died of Wounds — logistic curve and treatment efficacy**

| Parameter                           | Variable                                | Baseline | Lower  | Upper | Rule |
| ----------------------------------- | --------------------------------------- | -------- | ------ | ----- | ---- |
| P1 DOW max probability              | `p1_p_max`                              | 2.3%     | 1.15%  | 4.6%  | A    |
| P1 DOW base probability             | `p1_p_base`                             | 0.1%     | 0.05%  | 0.2%  | A    |
| P1 logistic steepness               | `p1_k`                                  | 0.04     | 0.024  | 0.056 | A    |
| P1 logistic midpoint                | `p1_t_mid`                              | 120 min  | 72     | 168   | A    |
| P2 DOW max probability              | `p2_p_max`                              | 1.9%     | 0.95%  | 3.8%  | A    |
| P2 DOW base probability             | `p2_p_base`                             | 0.05%    | 0.025% | 0.1%  | A    |
| P2 logistic steepness               | `p2_k`                                  | 0.025    | 0.015  | 0.035 | A    |
| P2 logistic midpoint                | `p2_t_mid`                              | 180 min  | 108    | 252   | A    |
| P3 flat DOW probability             | `p3_flat`                               | 0.1%     | 0.05%  | 0.2%  | B    |
| R1 TCCC efficacy factor             | `r1_tccc_factor`                        | 0.83     | 0.68   | 0.98  | A    |
| R2B/R2E DCR (resus) efficacy factor | `r2b_resus_factor` / `r2e_resus_factor` | 0.56     | 0.41   | 0.71  | A    |
| R2B DCS efficacy factor             | `r2b_dcs_factor`                        | 0.32     | 0.17   | 0.47  | A    |
| R2E DCS 1st-op efficacy factor      | `r2e_dcs1_factor`                       | 0.25     | 0.10   | 0.40  | A    |
| R2E DCS 2nd-op efficacy factor      | `r2e_dcs2_factor`                       | 0.57     | 0.42   | 0.72  | B    |
| R2E post-op hold penalty            | `r2e_postop_hold_penalty`               | 3.0×     | 1.5    | 6.0   | B    |
| R2B forward ICU penalty             | `r2b_icu_penalty`                       | 1.31×    | 1.09   | 1.59  | —    |

**Casualty Generation Rates**

| Parameter                       | Variable        | Baseline | Lower | Upper | Rule |
| ------------------------------- | --------------- | -------- | ----- | ----- | ---- |
| WIA — combat, mean daily rate   | `wia_cbt_mean`  | 1.77     | 1.06  | 2.48  | A    |
| KIA — combat, mean daily rate   | `kia_cbt_mean`  | 0.68     | 0.41  | 0.95  | A    |
| DNBI — combat, mean daily rate  | `dnbi_cbt_mean` | 2.04     | 1.22  | 2.86  | A    |
| WIA — support, mean daily rate  | `wia_spt_mean`  | 1.77     | 1.06  | 2.48  | A    |
| KIA — support, mean daily rate  | `kia_spt_mean`  | 0.68     | 0.41  | 0.95  | A    |
| DNBI — support, mean daily rate | `dnbi_spt_mean` | 0.94     | 0.56  | 1.32  | A    |

**Mass Casualty, Force Regeneration, Strategic AME**

| Parameter                            | Variable                     | Baseline     | Lower | Upper | Rule |
| ------------------------------------ | ---------------------------- | ------------ | ----- | ----- | ---- |
| Mass casualty event rate             | `mass_casualty_rate`         | 0/day        | 0     | 0.4   | B    |
| Mass casualty size — maximum         | `mass_casualty_max_cas`      | 60           | 40    | 80    | B    |
| Mass casualty size — minimum         | `mass_casualty_min_cas`      | 20           | 10    | 30    | B    |
| Reinforcement demand cycle           | `fr_demand_interval_days`    | 0 (disabled) | 0     | 14    | B    |
| Reinforcement fulfillment lag        | `fr_fulfillment_lag_days`    | 7 days       | 4     | 14    | B    |
| Reinforcement fill distribution mode | `fr_fill_mode_frac`          | 0.85         | 0.5   | 1.05  | B    |
| AME sortie interval                  | `ame_schedule_interval_days` | 7 days       | 4     | 14    | B    |
| AME sortie cancellation probability  | `ame_failure_probability`    | 15%          | 8%    | 30%   | B    |

##### Simplex-Constrained Compositions

Three groups of parameters describe a composition rather than a level: the R1 triage priority split (`pri_one`/`pri_two`/`pri_three`), the DNBI sub-type composition (`battle_fatigue_pct`/`disease_pct`/`nbi_pct`), and the mass casualty priority split (`mass_casualty.priority.one`/`two`/`three`). Each is constrained to sum to one, which is why a one-at-a-time design cannot perturb its members directly: a composition of $k$ parts carries only $k-1$ degrees of freedom, so moving one part while holding the others fixed leaves the simplex, and renormalising afterwards is a design decision that shapes the answer.

These nine parameters are screened through the **isometric log-ratio (ILR) transform** [[62]](#references), which maps a composition onto $k-1$ unconstrained real coordinates that can be varied independently and mapped back, so the constraint holds by construction. With $k = 3$ in each group, the nine become six coordinates, screened inside the existing Morris design at the same cost per trajectory. The transform and its inverse are implemented directly in `ilr3()` and `ilr3_inv()` (`R/sensitivity.R`), the three-part case being two lines each way.

The basis used is the sequential binary partition that separates the leading part from the other two, then those two from each other, so each coordinate reads as a **balance** between named groups of parts:

$$z_1 = \sqrt{\tfrac{2}{3}} \ln \frac{x_1}{\sqrt{x_2 x_3}}, \qquad z_2 = \sqrt{\tfrac{1}{2}} \ln \frac{x_2}{x_3}$$

A µ\* therefore attaches to a contrast, not to a single named share. Each group orders its parts so the first balance is the contrast of planning interest, and the six carry the labels below.

| Coordinate             | Contrast                                                | Baseline | Lower  | Upper |
| ---------------------- | ------------------------------------------------------- | -------- | ------ | ----- |
| `triage_p1_balance`    | Priority 1 against Priority 2 and 3                     | 1.080    | 0.411  | 1.706 |
| `triage_p2_p3_balance` | Priority 2 against Priority 3                           | 0.203    | −0.287 | 0.694 |
| `dnbi_disease_balance` | Disease against battle fatigue and non-battle injury    | 0.845    | 0.250  | 1.478 |
| `dnbi_bf_nbi_balance`  | Battle fatigue against non-battle injury                | 0.273    | −0.217 | 0.763 |
| `mc_p1_balance`        | Mass casualty Priority 1 against Priority 2 and 3       | 1.306    | 0.778  | 2.030 |
| `mc_p2_p3_balance`     | Mass casualty Priority 2 against Priority 3             | 0.490    | 0.000  | 0.980 |

A high µ\* on a first balance tells a planner that how severe the casualty mix is drives the response, independently of how the less urgent casualties divide among themselves; a high µ\* on a second balance says the opposite, that the split within the remainder is what matters. For the triage group specifically, a high µ\* on `triage_p1_balance` would mean the Priority 1 share is itself a leading uncertainty, which is the comparison these coordinates were brought into the screen to make: the two highest-ranked parameters in the published ranking, `pri1_evac_prob` and `pri1_surg_prob`, are both conditional on a casualty being Priority 1, and until now the share they are conditional on carried no evidence at all.

Bounds are derived by transforming a compositional range rather than stated in coordinate space, where a number would have no intuitive meaning. The first coordinate's bounds transform a plausible range for the leading part's share, holding the ratio of the two trailing parts at baseline, which is the sub-composition the first balance leaves untouched: Priority 1 spanning 0.45 to 0.80 for the triage split, disease spanning 0.40 to 0.75 for the DNBI composition, and mass casualty Priority 1 spanning 0.55 to 0.85. All three ranges are informed estimates rather than measured spreads, and are deliberately wider than either rule would give, because the baselines they surround are among the least well grounded in the model: the triage split rests on ADF planning norms with no open-access source, and the disease share is a residual left after the battle fatigue and non-battle injury shares are taken out, the first sourced to a historical evacuation proportion and the second to FORECAS [[8]](#references). A composition assumed rather than measured warrants the wider bound, and if the screen finds one of them influential, the width is itself part of the finding. The second coordinate applies Rule B multiplicatively to the ratio of the two trailing parts (×0.5 to ×2.0), which in coordinate space is the symmetric interval $\pm\sqrt{1/2}\ln 2$ around the baseline, a balance being a scaled log ratio.

One approximation follows from the geometry. A balance contrasts the leading part against the *geometric* mean of the other two, so at a corner where both coordinates sit at an extreme the realised leading share differs slightly from the nominal endpoint: across the whole triage coordinate box the Priority 1 share spans 0.42 to 0.80 against a nominal 0.45 to 0.80. The composition is valid everywhere; only the endpoint is approximate. `scripts/check_composition_ilr.R` asserts the invariant rather than leaving it to inspection, confirming that every design point of a real Morris design, and each corner of each coordinate box, yields three strictly positive parts summing to one, and that the coordinate modes reproduce the shipped configuration exactly.

For any group that screens as influential, a confirmatory decomposition treats the composition as a single object rather than as two separate contrasts. `run_sobol()` samples whole compositions from a **Dirichlet** distribution centred on the group's baseline and computes first-order and total-order Sobol indices [[49]](#references) for them alongside the other selected parameters, reporting how much of the output variance the whole composition explains rather than how much each contrast does, which is the form a planner can act on.

The Dirichlet is used because it is the natural distribution over the simplex: every draw from it is already a valid composition, three positive parts summing to one, so a plausible casualty mix is sampled directly and nothing has to be corrected afterwards to keep it valid. It carries a single concentration parameter setting how tightly draws cluster around the baseline, which is fixed per group so that two standard deviations of the leading part match the half-width of the compositional range the Morris bounds use, giving 28.7, 30.8 and 36.3, rather than a spread chosen for no stated reason. Both coordinates of a group enter the decomposition together whenever either is selected, since an index on one coordinate of a composition varied a coordinate at a time would not be an index on the composition.

#### Parameters Excluded from Screening

Not every numeric leaf in `env_data.json`'s `vars` tree is a candidate for Morris OAT screening. The following categories are deliberately excluded, each for a reason specific to the category rather than a blanket omission. The three composition splits are no longer among them: they are screened through the transform described under [Simplex-Constrained Compositions](#simplex-constrained-compositions).

**Triangular-distribution minimum/maximum bounds.** For every duration modelled as a triangular distribution (surgery, resuscitation, transport, ICU, holding, and Role 4 length of stay), only the *mode* is screened. The mode carries the primary epistemic uncertainty; the min/max define the distribution's outer envelope and are treated as fixed shape parameters. This excludes 36 fields.

**KIA/mortuary processing durations.** `kia_treat` and `kia_transport` at all three echelons (`r1_kia_treat`, `r1_kia_transport`, `r2b_kia_treat`, `r2b_kia_transport`, which carries the KIA/mortuary road move to the R2E-collocated mortuary, `r2e_kia_treat`, and `r2e_kia_transport`) govern only deceased-casualty logistics, the time to process and transport a body already confirmed dead. They do not affect any live-casualty health outcome or any response in the screened set ([Screening Response Set](#screening-response-set)), whose resource contention is dominated by live-patient throughput. Screening them would consume design points without informing a health-outcome-relevant finding.

**Discrete/categorical switches.** `r2e_icu_p1_bypass` (`icu_gating.p1_bypass_priority_max`) takes only the meaningful values 1–3 (a priority-level threshold) rather than varying continuously; `mass_casualty.event.mode` selects between `"poisson"` and `"scheduled"` timing, a categorical choice with no numeric ordering. Neither is amenable to continuous OAT interpolation.

**Fixed establishment/capacity counts.** Population sizes, team/bed/vehicle counts (`pop_combat`, `r1_team_count`, `r2b_bed_ot`, `transport_PMVAmb_qty`, etc.) and each selectable AME airframe's fitted critical/standard capacity pair (`airframe_c17a`, `airframe_c130j`, `airframe_c27j`, six fields) represent discrete establishment or hardware decisions a planner sets explicitly, not continuous parameters carrying epistemic uncertainty about a true underlying value. The airframe capacities have the stronger claim to exclusion of the two, being a published property of a named aircraft rather than an establishment choice: perturbing them over a range would screen the sensitivity of the model to an aircraft that does not exist. The airframe selector (`role4.ame.airframe`) is likewise unscreened, being categorical.

**Mass casualty schedule slots and injection-window timing.** The 20-slot deliberate event schedule (`mass_casualty.schedule.*`, 140 fields) is a planner-populated list that ships empty by default, each slot a specific scenario input rather than a parameter carrying epistemic uncertainty around a baseline value. The injection window (`window_min`/`window_mode`/`window_max`, which spreads a fired event's casualties over time) is excluded because its influence is expected to be second-order relative to whether an event fires and how large it is, both of which are screened.

**Secondary casualty-rate shape parameter.** `sd_daily` (second of the three fields per generator stream, six fields total) governs day-to-day variability in the already heavy-tailed lognormal arrival process, and is unused by exponential streams, which are single-parameter; `mean_daily` (screened above) captures each stream's primary influence on total casualty load.

**Polling-loop intervals.** The OT-entry defer poll (`r2b_icu_defer_interval`, `r2e_icu_defer_interval`) and the strategic-AME-wait DOW poll (`ame_dow_check_interval`) are not screened. These are a different kind of parameter from a genuine scheduling policy such as `ame_schedule_interval_days` (a real sortie-cadence decision, still screened): in reality, clinical staff and evacuation coordinators monitor these conditions continuously, not on a fixed poll interval.

#### Screening Response Set

Morris ranks parameters by their influence on a response variable, so the response set decides what the ranking is a ranking of. The screen uses the KPI set defined in [Model Outputs](#model-outputs), not a separate selection: every KPI documented there contributes at least one response here, and each response inherits the C1–C5 selection criteria of the KPI it comes from. `morris_kpis` (`R/sensitivity.R`) carries the name, label, domain, criteria and scalar reduction of each response, and is the authority this section reproduces.

Morris requires exactly one response value per design point, but most of the documented KPIs are not scalars. A vector KPI, such as DOW rate by echelon, contributes one response per element. A distribution, such as time to first surgical incision, contributes one response per summary statistic taken from it. A time series, such as the AME backlog, contributes one response per statistic reduced over time. Each reduction changes what µ\* means for that response and is therefore recorded below rather than left to the code.

| Model Output KPI | Criteria | Responses | Scalar reduction |
| --- | --- | --- | --- |
| Total DOW count | C1, C2, C5 | `dow_count` | Per-replication mean count |
| DOW rate by echelon | C1, C2, C3, C5 | `dow_rate_r1`, `dow_rate_r2b`, `dow_rate_r2e`, `dow_rate_r2e_postop`, `dow_rate_ame_wait` | One response per echelon; deaths at that echelon over total arrivals |
| Time from R1 arrival to first surgical incision | C1, C2, C3, C5 | `time_to_surgery_mean`, `time_to_surgery_p90` | Mean and p90 of the distribution, screened separately |
| R2B dwell time | C1, C3, C4 | `r2b_dwell_mean` | Mean of the distribution |
| R2B→R2E transit time | C1, C3 | `r2b_r2e_transit_mean` | Mean of the distribution |
| R2E dwell time | C1, C3, C4 | `r2e_dwell_mean` | Mean of the distribution |
| OT utilisation by echelon | C3, C4 | `ot_util_r2b`, `ot_util_r2e` | One response per echelon; time-weighted mean fraction of theatre capacity busy |
| R2B and R2E surgery counts per day | C2, C3, C4 | `r2b_surgery_count`, `r2e_surgery_count` | Per-replication mean total over the run |
| Resource queue length over time | C3, C4 | `r2b_ot_q`, `r2e_ot_q`, `r2e_icu_q`, `transport_q` | Time-weighted mean queue length |
| RTD rate by echelon | C1, C2, C5 | `rtd_rate_r1`, `rtd_rate_r2b`, `rtd_rate_r2e` | One response per echelon; returns at that echelon over total arrivals |
| R2B bypass rate | C2, C3, C4 | `r2b_bypass_rate` | Bypassed casualties over WIA arrivals |
| Total RTD count | C2, C5 | `total_rtd` | Per-replication mean count |
| Role 4 daily bed occupancy by ward | C2, C3, C5 | `role4_peak_occupancy`, `role4_mean_occupancy` | Per-replication mean of the peak concurrent census, and patient-days over the engagement window |
| Unconstrained-baseline AME sortie demand | C2, C4, C5 | `ame_sortie_demand` | Per-replication mean total sorties required |
| Strategic AME wait time by route | C2, C4, C5 | `ame_wait_critical_mean`, `ame_wait_standard_mean` | Mean of the distribution, per route |
| Strategic AME backlog over time by pool | C3, C4, C5 | `ame_backlog_critical_mean`, `ame_backlog_critical_peak`, `ame_backlog_standard_mean`, `ame_backlog_standard_peak` | Time-weighted mean and peak, per pool |
| Strategic AME sortie timeline | C3, C4, C5 | `ame_sorties_flown` | Count of sortie opportunities flown, as a per-replication mean |

Two further responses are screened that have no counterpart in Model Outputs, and are kept as derived aggregates rather than promoted to KPIs. `system_ot_q` is the sum of the two theatre queue responses and is the primary ranking, reported in `outputs/morris_ranking.csv` and in the published table below. `transport_util` applies Domain 3's utilisation reduction to the transport fleet, whose queues sit near zero under baseline demand and would register no sensitivity at all if only queue depth were screened.

Three conventions apply across the set. Counts are reported as the per-replication mean rather than as a total across the replications evaluated at a design point, so a response keeps the same scale whether a screen runs at three replications per point or five; rates are already invariant to that count, being divided by arrivals over the same replications. The DOW echelon breakdown screens five echelons rather than the three its Model Outputs entry names, that entry predating the post-operative and AME-wait mortality checkpoints. `r2e_surgery_count` counts theatre episodes rather than casualties, so a damage control casualty operated on twice at R2E contributes two.

Expanding the response set costs no additional simulation. The Morris design is generated once and every response is told against the same response matrix, so the sweep is shared: the marginal cost of a response is one `tell()`, one plot and one ranking file, none of which scale with the design point count. What it does add is the cost of reading the wider set off each design point's monitors, measured at 2.3 seconds against a 28.4-second simulation at five replications over 30 days, or 7.9% of a design point. Wall-clock time therefore remains set by the parameter count and the replication count rather than by how many responses are read.

**Whether the wider set changes what the screen says.** The question the expansion is meant to answer is whether a parameter that matters for time to treatment can be invisible in a ranking on queue depth. A smoke screen at three trajectories over five days, which is far too coarse to rank parameters but adequate to show that two responses disagree, puts the rank correlation between the system OT queue and mean time to first surgical incision at 0.55 across the fifty-eight parameters. Individual parameters move a long way: Disease DNBI Surgical Candidacy (`disease_surgery_pct`) ranks 8th on queue depth and 55th on time to surgery, while Theatre Evacuation Policy (`evacuation_policy_days`) ranks 47th on queue depth and 18th on the mean time to surgery, 7th on its p90. The p90 pairing earns its place on the same evidence: P2 Logistic Midpoint (`p2_t_mid`) ranks 1st on the tail and 4th on the mean. This is an indication that the responses are not redundant, not a finding about the model; the ranking a planner should read comes from the outstanding production re-run.

**Degenerate responses.** Some responses are near-constant across the design, either because the mechanism they measure barely fires under any screened parameter combination or because a short screening run leaves the cohort empty. µ\* is then arithmetically near zero while carrying no information, which reads identically to a confident finding that nothing influences the response. The two are distinguished in the output. A response whose variation across the design falls below a threshold relative to its own magnitude is marked `degenerate = TRUE` in its ranking file, with µ\* and σ written as `NA` rather than zero, and the screen warns on completion listing every response so marked. Each ranking file also carries `response_mean`, `response_sd` and `response_na_pts`, the last counting design points at which the response could not be measured at all, and `n_finite_ee` per parameter, so a parameter whose elementary effects were partly lost to a failed design point is visible rather than silently averaged over what survived.

#### Parameter Name Reference

The grouped tables above and the ranking table below identify each parameter by its `morris_params$name`, the same identifier used in `outputs/morris_ranking.csv`, in `apply_params()` (`R/sensitivity.R`), and on every `images/morris_*.png` axis. The table below maps all sixty-five to a plain-English title and category, sorted alphabetically by variable. Titles come from `MORRIS_LABELS` (`app.R`) and categories from `morris_params$category` (`R/sensitivity.R`); this table reproduces both rather than deriving from them, so it must be updated whenever a parameter is added, removed, retitled, or recategorised. The Shiny app's Sensitivity Calibration tab presents the same mapping alongside each parameter's screened bounds, with a CSV download ([Shiny Application](#shiny-application)).

| Variable                     | Title                                     | Category                        |
| ---------------------------- | ----------------------------------------- | ------------------------------- |
| `ame_failure_probability`    | AME Sortie Cancellation Probability       | Scenario / Casualty Context     |
| `ame_schedule_interval_days` | AME Sortie Interval (Days)                | Health System Design - Policy   |
| `disease_surgery_pct`        | Disease Surgical Candidacy                | Scenario / Casualty Context     |
| `dnbi_bf_nbi_balance`        | DNBI Balance — Battle Fatigue against Non-Battle Injury | Scenario / Casualty Context |
| `dnbi_disease_balance`       | DNBI Balance — Disease against Battle Fatigue and NBI | Scenario / Casualty Context |
| `dnbi_cbt_mean`              | DNBI — Combat Mean Daily Rate             | Scenario / Casualty Context     |
| `dnbi_spt_mean`              | DNBI — Support Mean Daily Rate            | Scenario / Casualty Context     |
| `fr_demand_interval_days`    | Reinforcement Demand Cycle (Days)         | Health System Design - Policy   |
| `fr_fill_mode_frac`          | Reinforcement Fill Distribution (Mode)    | Health System Design - Policy   |
| `fr_fulfillment_lag_days`    | Reinforcement Fulfillment Lag (Days)      | Health System Design - Policy   |
| `evacuation_policy_days`     | Theatre Evacuation Policy (Days)          | Health System Design - Policy   |
| `kia_cbt_mean`               | KIA — Combat Mean Daily Rate              | Scenario / Casualty Context     |
| `kia_spt_mean`               | KIA — Support Mean Daily Rate             | Scenario / Casualty Context     |
| `long_resus_mode`            | Long Resuscitation Duration (Mode)        | Health System Design - Capacity |
| `mass_casualty_max_cas`      | Mass Casualty Event Size (Maximum)        | Scenario / Casualty Context     |
| `mass_casualty_min_cas`      | Mass Casualty Event Size (Minimum)        | Scenario / Casualty Context     |
| `mass_casualty_rate`         | Mass Casualty Event Rate (per day)        | Scenario / Casualty Context     |
| `mc_p1_balance`              | Mass Casualty Triage Balance — Priority 1 against Priority 2 and 3 | Scenario / Casualty Context |
| `mc_p2_p3_balance`           | Mass Casualty Triage Balance — Priority 2 against Priority 3 | Scenario / Casualty Context |
| `ot_hours`                   | OT Shift Length (Hours per Shift)         | Health System Design - Policy   |
| `p1_k`                       | Priority 1 DOW Logistic Steepness         | Scenario / Casualty Context     |
| `p1_p_base`                  | Priority 1 DOW Base Probability           | Scenario / Casualty Context     |
| `p1_p_max`                   | Priority 1 DOW Ceiling                    | Scenario / Casualty Context     |
| `p1_t_mid`                   | Priority 1 DOW Logistic Midpoint          | Scenario / Casualty Context     |
| `p2_k`                       | Priority 2 DOW Logistic Steepness         | Scenario / Casualty Context     |
| `p2_p_base`                  | Priority 2 DOW Base Probability           | Scenario / Casualty Context     |
| `p2_p_max`                   | Priority 2 DOW Ceiling                    | Scenario / Casualty Context     |
| `p2_t_mid`                   | Priority 2 DOW Logistic Midpoint          | Scenario / Casualty Context     |
| `p3_flat`                    | Priority 3 Flat DOW Probability           | Scenario / Casualty Context     |
| `post_definitive_icu_mode`   | R2E Post-Definitive ICU Duration (Mode)   | Health System Design - Capacity |
| `post_op_hold_mode`          | R2E Post-Op Holding-Bed Duration (Mode)   | Health System Design - Capacity |
| `pri1_evac_prob`             | Priority 1 Strategic Evacuation Rate      | Scenario / Casualty Context     |
| `pri1_dcs_rate`              | Priority 1 Damage Control Rate            | Scenario / Casualty Context     |
| `pri1_surg_prob`             | Priority 1 Surgical Candidacy             | Scenario / Casualty Context     |
| `pri2_evac_prob`             | Priority 2 Strategic Evacuation Rate      | Scenario / Casualty Context     |
| `pri2_dcs_rate`              | Priority 2 Damage Control Rate            | Scenario / Casualty Context     |
| `pri2_surg_prob`             | Priority 2 Surgical Candidacy             | Scenario / Casualty Context     |
| `pri3_dcs_rate`              | Priority 3 Damage Control Rate            | Scenario / Casualty Context     |
| `pri3_dnbi_surg_prob`        | Priority 3 DNBI Surgical Candidacy        | Scenario / Casualty Context     |
| `pri3_other_surg_prob`       | Priority 3 Other Surgical Candidacy       | Scenario / Casualty Context     |
| `r1_recovery_mode`           | R1 Battle Fatigue Hold Duration (Mode)    | Health System Design - Capacity |
| `r1_tccc_factor`             | R1 TCCC Efficacy Factor                   | Scenario / Casualty Context     |
| `r1_transport`               | R1 Transport Time (Mode)                  | Scenario / Casualty Context     |
| `r1_wia_treat_mode`          | R1 WIA Treatment Time (Mode)              | Health System Design - Capacity |
| `r2b_dcs_factor`             | R2B DCS Efficacy Factor                   | Scenario / Casualty Context     |
| `r2b_hold_mode`              | R2B Holding Bed Duration (Mode)           | Health System Design - Capacity |
| `r2b_hold_threshold`         | R2B Hold-Bed Reroute Threshold            | Health System Design - Policy   |
| `r2b_pre_open_window`        | R2B Pre-Open Hold Window (Minutes)        | Health System Design - Policy   |
| `r2b_icu_penalty`            | R2B Forward ICU DOW Penalty (Multiplier)  | Scenario / Casualty Context     |
| `r2b_forward_hold_max`       | R2B Forward Hold Time Limit               | Health System Design - Policy   |
| `r2b_icu_share`              | R2B Forward ICU Share                     | Health System Design - Policy   |
| `r2b_resus_factor`           | R2B/R2E DCR (Resus) Efficacy Factor       | Scenario / Casualty Context     |
| `r2b_transport`              | R2B Transport Time (Mode)                 | Scenario / Casualty Context     |
| `r2e_dcs1_factor`            | R2E DCS 1st-Op Efficacy Factor            | Scenario / Casualty Context     |
| `r2e_dcs2_factor`            | R2E DCS 2nd-Op Efficacy Factor            | Scenario / Casualty Context     |
| `r2e_hold_mode`              | R2E Base Recovery-to-Duty Duration (Mode) | Health System Design - Capacity |
| `r2e_postop_hold_penalty`    | R2E Post-Op Hold DOW Penalty (Multiplier) | Scenario / Casualty Context     |
| `r2e_resus_factor`           | R2E DCR (Resus) Efficacy Factor           | Scenario / Casualty Context     |
| `short_resus_mode`           | R2E Short Resuscitation Duration (Mode)   | Health System Design - Capacity |
| `stabilisation_icu_mode`     | R2E Stabilisation ICU Requirement (Mode)  | Health System Design - Capacity |
| `surg_mode`                  | Surgery Duration (Mode)                   | Health System Design - Capacity |
| `triage_p1_balance`          | Triage Balance — Priority 1 against Priority 2 and 3 | Scenario / Casualty Context |
| `triage_p2_p3_balance`       | Triage Balance — Priority 2 against Priority 3 | Scenario / Casualty Context |
| `wia_cbt_mean`               | WIA — Combat Mean Daily Rate              | Scenario / Casualty Context     |
| `wia_spt_mean`               | WIA — Support Mean Daily Rate             | Scenario / Casualty Context     |

Seven response variables are computed at each design point. Five are time-weighted mean queue depths: R2B OT, R2E OT, their sum as the system OT queue, R2E ICU, and transport pooled across the PMV Ambulance and HX240M fleets. A sixth, mean transport utilisation over the same fleets, is tracked because transport assets rarely queue at the baseline casualty rate: availability tightens before a queue forms, so a queue-only response would under-detect transport parameters. All six are averaged across a design point's replications. The seventh, total DOW count, is summed across them, so its µ\* values are not on the same scale as the rest. The system OT queue is the ranking response: `run_morris()` builds `outputs/morris_ranking.csv` from it and the table below is sorted on it, identifying the inputs most responsible for surgical bottleneck severity. The Shiny Sensitivity panel instead displays the R2E ICU queue scatter ([Shiny Application](#shiny-application)).

Screening bounds cover clinically plausible variation around each baseline, under two rules that record where the baseline came from. **Rule A** covers parameters traceable to an open-access source cited in this document, including the DOW logistic shape parameters [[29]](#references), [[30]](#references), the treatment efficacy factors [[31]](#references), [[32]](#references), and the casualty generation rates [[8]](#references). **Rule B** covers parameters whose entry in `R/app_params.R` states the value is an informed estimate with no published source.

Bound width depends on what kind of quantity a parameter is. Durations and rates are scaled: `r1_recovery_mode`, a 2880-minute hold, spans half to twice that at 1440 to 5760, while `surg_mode`, at 95 minutes, spans the narrower 57 to 133 that Rule A gives, a cited source constraining it. Its lower endpoint is also read against that source rather than accepted as arithmetic: 57 minutes falls inside the 41 to 210 minute operative-time range reported for the damage control cohort the mode itself is drawn from [[20]](#references), and above the fastest operation that cohort recorded, so it describes a short abbreviated procedure the data supports rather than one quicker than anything observed. Probabilities and efficacy factors instead move by a fixed amount, usually 0.15 to 0.25: `r1_tccc_factor` spans 0.68 to 0.98 around a baseline of 0.83. Where that margin would carry a value past a clinical limit it is clipped, which is what makes some bounds lopsided: `pri1_evac_prob` runs from 70% to 99% around a 95% baseline, since 95% plus 25 points would exceed certainty.

The letter a row carries records where its baseline came from rather than fixing a width exactly, so the widths above are typical rather than universal: an endpoint is clipped wherever a rule applied literally would carry it past a probability of one, below zero, or outside a fixed distribution envelope. What every two-sided row does share is that its baseline sits inside its range rather than at an edge of it, which is what makes one row's µ\* comparable with another's.

Four parameters take their bounds from neither rule. `r2b_icu_share` is swept across the whole of its domain, 0 to 1, because it is a policy lever with no baseline to vary around: the shipped value of zero is one end of the range a planner may choose from rather than an estimate of anything. `r2b_forward_hold_max` is swept from zero, which disables forward holding outright, to 2,880 minutes, which exceeds the longest stabilisation requirement the model can draw and so leaves the share acting alone. `r2b_pre_open_window` is swept on the same basis, from zero, which restores instant diversion, to 360 minutes, half a 12-hour off-shift period and long enough that the hold is plainly no longer a bridge to an imminent shift; its 60-minute baseline is an informed estimate rather than a measured quantity (see Further Development L28), which is the second reason a proportional margin around it would claim more than the value supports. `r2b_icu_penalty` takes the 95% confidence interval its own source reports, 1.09 to 1.59, in preference to a fixed proportional margin around the point estimate, which is both better grounded and narrower than Rule A would give.

One further constraint applies whatever the width. A screened triangular mode must stay inside its own distribution's fixed minimum and maximum, because `rtriangle()` requires $a \leq c \leq b$ and returns `NA` otherwise. `fr_fill_mode_frac` and `post_op_hold_mode` are bounded to respect this, with the reason recorded in `R/sensitivity.R`.

These bounds are estimates, so confidence in them is moderate overall and lower for Rule B parameters. Bounds set too narrow understate a parameter's influence; bounds set too wide mix realistic values with unrealistic ones. Because the model responds non-linearly, the ranking can shift with the bounds chosen, though widening every bound would raise µ\* without reordering parameters if responses were monotonic.

The screen runs at r = 5 Morris trajectories rather than the `--r` default of 20, giving 5 × 65 = 325 design points at five replications each. At r = 20 it would need 1,300, four times the compute. A lower r makes each µ\*/σ estimate noisier without biasing it, since the Morris method [[46]](#references) is unbiased at any number of trajectories and only gains precision as more are added. The ranking below should therefore be read as indicating relative influence rather than an exact order.




The sensitivity analysis is implemented in `R/sensitivity.R` and executed via:

```bash
# Full Morris screening: r=20 trajectories × (64 + 1) = 1,300 design points, 5 reps each
Rscript scripts/run_sensitivity.R

# Smoke test: r=3, reps=3, days=5 (completes in <5 minutes)
Rscript scripts/run_sensitivity.R --quick

# Morris then Sobol variance decomposition on top 5 parameters
Rscript scripts/run_sensitivity.R --sobol
```

Outputs are written to `outputs/morris_ranking_<response>.csv`, one ranking per response in the [Screening Response Set](#screening-response-set), each carrying the response's criteria mapping and its degeneracy diagnostics alongside the per-parameter µ\* and σ. `outputs/morris_ranking.csv` repeats the primary system OT queue ranking under its historical filename. Scatter plots are written per response to `outputs/images/morris_<response>.png`, which is gitignored; `--images-dir images` redirects them to the tracked baseline location, which is a deliberate act rather than a default, since a screen writes one plot per response and would otherwise scatter untracked files through a tracked directory. When `--sobol` is specified, first-order (S1) and total-order (ST) indices for the top-ranked parameters are written to `outputs/sobol_<kpi>.csv`.

**Current ranking.** The table below is `outputs/morris_ranking.csv` for the then-shipped fifty-three-parameter set, run at r = 5 with 5 replications over 30 days at seed 42, ranked by µ\* on the system OT queue. Wall-clock time was 108 minutes on 4 cores. It also predates the expansion of the response set from seven measures to the thirty-six documented in [Screening Response Set](#screening-response-set), so it is a ranking on one of those responses with no companion ranking on the other thirty-five; in particular no time-to-care, return-to-duty or strategic evacuation response has yet been ranked at production r, and whether any parameter ranks materially differently on a time-to-care response than on the queue responses is therefore an open question the outstanding re-run is expected to answer. The screen has not been re-run since the R2E disposition mechanism was rebuilt around the theatre evacuation policy, nor since post-operative intensive care was split into its two clinical episodes, nor since the surgical population was split between the damage control and single-stage pathways, nor since the lognormal generator's rate cap was made relative to each stream's own mean, nor since the generators were reparameterised so that each stream realises the daily mean it is configured with, so five entries name the parameters as they stood when it was measured. The last two of those change the realised rate every design point of the six casualty-generation parameters produces, so their ranks in particular were measured under a generator the model no longer runs; under the correction a `*_mean` design point now delivers the rate it names, where before it delivered between 79% and 99% of it depending on the stream. `in_theatre_rate` at rank 25 is the parameter `evacuation_policy_days` replaced, and `r2e_hold_mode` was screened over its earlier, unscaled range. `long_icu_mode` at rank 46 is now `stabilisation_icu_mode`, unchanged in value and bounds but narrower in meaning, since it no longer has to stand for post-definitive care as well. `short_icu_mode` at rank 36 and `post_surgery_prob` at rank 41 no longer exist at all: the R2E stay now follows from the casualty's requirement and the forward-holding policy rather than from a short-against-full draw. Thirteen parameters new to the screened set have no measured rank yet — `r2b_icu_share`, `r2b_forward_hold_max`, `r2b_icu_penalty`, `post_definitive_icu_mode`, `pri1_dcs_rate`, `pri2_dcs_rate`, `pri3_dcs_rate`, and the six composition balance coordinates — which is also why the table below lists fifty-three rows against the screen's current sixty-five. Every µ\* in it was measured against the earlier disposition and post-operative logic, and a re-run is outstanding. It was also measured over `surg_mode`'s earlier 90 to 150 minute range, which sat almost entirely above the shipped mode, so every design point behind the table ran a longer operation than the model performs, and the resulting theatre occupancy bears on the rank of any parameter acting through theatre contention rather than on `surg_mode`'s alone. That range is corrected above, ahead of the re-run rather than after it. The comparison the balance coordinates were added to make, between the Priority 1 share and the two Priority 1 conditional rates that top this table, therefore awaits that re-run: `scripts/run_sensitivity.R` reports each coordinate's rank alongside the rank of `pri1_evac_prob` and `pri1_surg_prob` at the end of every screen, so the comparison is read off the run rather than assembled by hand.

| Rank | Parameter                 | µ\*    | σ       | Rank | Parameter                    | µ\*    | σ      |
| ---- | ------------------------- | ------ | ------- | ---- | ---------------------------- | ------ | ------ |
| 1    | `pri1_evac_prob`          | 9.6285 | 11.7767 | 28   | `surg_mode`                  | 0.8556 | 0.8395 |
| 2    | `pri1_surg_prob`          | 7.4479 | 13.1315 | 29   | `ame_schedule_interval_days` | 0.8553 | 1.0327 |
| 3    | `r2e_dcs2_factor`         | 6.9901 | 11.4911 | 30   | `ame_failure_probability`    | 0.7820 | 0.9800 |
| 4    | `fr_demand_interval_days` | 6.2837 | 9.8554  | 31   | `pri2_evac_prob`             | 0.7298 | 0.9768 |
| 5    | `r2b_hold_mode`           | 3.5434 | 7.0305  | 32   | `p1_p_max`                   | 0.7233 | 1.1000 |
| 6    | `pri3_other_surg_prob`    | 3.3681 | 7.1993  | 33   | `long_resus_mode`            | 0.6831 | 1.0911 |
| 7    | `short_resus_mode`        | 3.3210 | 6.9503  | 34   | `r2e_resus_factor`           | 0.6591 | 1.1066 |
| 8    | `wia_spt_mean`            | 3.1055 | 6.1896  | 35   | `dnbi_cbt_mean`              | 0.5494 | 0.7270 |
| 9    | `pri3_dnbi_surg_prob`     | 2.9054 | 6.2861  | 36   | `short_icu_mode`             | 0.5058 | 0.6696 |
| 10   | `mass_casualty_rate`      | 2.8699 | 2.0662  | 37   | `r2b_resus_factor`           | 0.4769 | 0.6727 |
| 11   | `kia_spt_mean`            | 2.6471 | 4.5539  | 38   | `r2e_dcs1_factor`            | 0.4599 | 0.7370 |
| 12   | `mass_casualty_max_cas`   | 2.6190 | 4.9154  | 39   | `dnbi_spt_mean`              | 0.4346 | 0.8547 |
| 13   | `kia_cbt_mean`            | 2.5651 | 3.6500  | 40   | `r2b_transport`              | 0.3752 | 0.6840 |
| 14   | `p2_k`                    | 2.2704 | 3.4687  | 41   | `post_surgery_prob`          | 0.3662 | 0.4883 |
| 15   | `r1_transport`            | 2.2094 | 3.5133  | 42   | `disease_surgery_pct`        | 0.3184 | 0.5905 |
| 16   | `p1_p_base`               | 2.1857 | 3.3285  | 43   | `r2e_hold_mode`              | 0.3072 | 0.5612 |
| 17   | `r1_recovery_mode`        | 2.1738 | 4.4717  | 44   | `r2b_dcs_factor`             | 0.2900 | 0.4154 |
| 18   | `p3_flat`                 | 2.0918 | 3.7868  | 45   | `r2b_hold_threshold`         | 0.2546 | 0.3494 |
| 19   | `r1_wia_treat_mode`       | 2.0859 | 3.0575  | 46   | `long_icu_mode`              | 0.2443 | 0.3731 |
| 20   | `fr_fill_mode_frac`       | 2.0262 | 3.6954  | 47   | `p1_t_mid`                   | 0.2425 | 0.4887 |
| 21   | `wia_cbt_mean`            | 1.8676 | 3.4164  | 48   | `r1_tccc_factor`             | 0.2080 | 0.3922 |
| 22   | `p1_k`                    | 1.7930 | 3.2622  | 49   | `post_op_hold_mode`          | 0.1786 | 0.2838 |
| 23   | `p2_p_base`               | 1.6900 | 2.7566  | 50   | `r2e_postop_hold_penalty`    | 0.1784 | 0.2839 |
| 24   | `p2_p_max`                | 1.5494 | 3.0226  | 51   | `p2_t_mid`                   | 0.1595 | 0.2713 |
| 25   | `in_theatre_rate`         | 1.2782 | 2.4311  | 52   | `fr_fulfillment_lag_days`    | 0.1330 | 0.2019 |
| 26   | `pri2_surg_prob`          | 1.0394 | 1.5161  | 53   | `ot_hours`                   | 0.1203 | 0.1742 |
| 27   | `mass_casualty_min_cas`   | 0.8707 | 1.4419  |      |                              |        |        |

**What the ranking tells you.** Casualty disposition dominates. Priority 1 Strategic Evacuation Rate (`pri1_evac_prob`) and Priority 1 Surgical Candidacy (`pri1_surg_prob`) rank first and second, at µ\* 9.63 and 7.45 against a median of 0.87 across the set. Before relying on a run, check that both match the scenario being modelled: an error in either shifts surgical queue depth more than an error anywhere else.

Seven of the top ten are Scenario / Casualty Context parameters, so results depend most on assumptions about the casualty load rather than on decisions the health system can make. Two are Capacity, R2B Holding Bed Duration (`r2b_hold_mode`) and R2E Short Resuscitation Duration (`short_resus_mode`), and one is Policy, the Reinforcement Demand Cycle (`fr_demand_interval_days`). A parameter's category tells you what a high rank means in practice and matters more than its exact position; see "Reading the plots: colour coding" below.

**Effects are not independent.** σ exceeds µ\* for 51 of the 53 parameters, meaning most parameters' influence depends on where the others are set. A parameter that looks inert at the baseline may matter once other values move, so conclusions drawn by changing one input at a time will understate what the model can do. Vary related parameters together, or compare whole scenario profiles, rather than tuning singly.

**Two results to read carefully.** The Reinforcement Demand Cycle ranks 4th, but reinforcement ships disabled (`demand_interval_days = 0`). Its rank comes from the screen varying it across 0 to 14 days, and shows that reinforcement would matter if switched on, not that it drives the shipped baseline. Separately, ranks toward the bottom sit very close together: the ten lowest span only 0.29 to 0.12, and ranks 49 and 50 differ by 0.0002. Treat the lower half of the table as a band rather than an order.

**Influence varies widely inside a single mechanism.** Reinforcement is the clearest case: the demand cycle ranks 4th while Reinforcement Fulfillment Lag (`fr_fulfillment_lag_days`) ranks 52nd of 53 and Reinforcement Fill Distribution (`fr_fill_mode_frac`) 20th, so cycle length is the value to get right when configuring it. Mass casualty sizing shows the same pattern more mildly, with Event Rate (`mass_casualty_rate`) 10th and Event Size Maximum (`mass_casualty_max_cas`) 12th against Event Size Minimum (`mass_casualty_min_cas`) at the median.

**Reading the plots: colour coding.** Each point is coloured by category, which tells you what can be done about a high-ranking parameter.

- **Orange, Scenario / Casualty Context (45 parameters).** Facts about the operating environment or the casualty population: casualty generation rates, the DOW curve, clinical-need composition, treatment efficacy, and transport times between echelons. Nobody chooses these. A high rank means the conclusion depends on how severe the scenario turns out to be.
- **Green, Health System Design - Capacity (10 parameters).** How long a treatment or holding step takes at current resourcing, such as Surgery Duration (`surg_mode`) and R1 WIA Treatment Time (`r1_wia_treat_mode`). Shortening these needs investment in staff, equipment or training. A high rank shows where capacity investment would have the largest effect.
- **Blue, Health System Design - Policy (10 parameters).** Thresholds, cadences and scheduling rules set by standing order: R2B Hold-Bed Reroute Threshold (`r2b_hold_threshold`), R2B Pre-Open Hold Window (`r2b_pre_open_window`), OT Shift Length (`ot_hours`), AME Sortie Interval (`ame_schedule_interval_days`), the reinforcement demand cycle, and Theatre Evacuation Policy (`evacuation_policy_days`). These can be changed by decision, without new resources, so a high rank here is the most immediately actionable result the screen produces.

Four assignments are judgement calls, and each affects how a result should be read. Transport times (`r1_transport`, `r2b_transport`) are Context rather than Capacity, because terrain and distance dominate them rather than vehicle numbers. AME Sortie Cancellation Probability (`ame_failure_probability`) is Context despite sitting among Policy AME settings, since weather, tasking and airframe availability drive it. The Forward ICU DOW Penalty (`r2b_icu_penalty`) is Context while the Forward ICU Share it prices (`r2b_icu_share`) is Policy: the mortality cost of an intensive care section without an intensivist is a fact about the establishment, while how much care to deliver forward is a disposition a commander orders. Reinforcement Fulfillment Lag and Fill Distribution are Policy on the view that commanders influence both through how they prioritise requests, which is the least clear-cut call in the set. The `category` field in `R/sensitivity.R` records the rule applied.

A scatter plot is written for every response in the set. The seven reproduced below are the ones the tracked baseline carries, all of them from the seven-response screen that predates the expansion; a screening run writes all thirty-six to its own output directory, and refreshes these seven in place only when explicitly pointed at the tracked location.

![Morris EE: System OT queue](../images/morris_system_ot_q.png)

![Morris EE: R2B OT queue](../images/morris_r2b_ot_q.png)

![Morris EE: R2E OT queue](../images/morris_r2e_ot_q.png)

![Morris EE: R2E ICU queue](../images/morris_r2e_icu_q.png)

![Morris EE: DOW count](../images/morris_dow_count.png)

![Morris EE: Transport queue](../images/morris_transport_q.png)

![Morris EE: Transport utilisation](../images/morris_transport_util.png)

#### Comparative Scenario Runner

The comparative scenario runner (`R/scenario_runner.R`) runs the replication framework described above under a named scenario profile instead of the base configuration, then aggregates queue and mortality results across replications so scenarios can be compared side by side.

Two profiles ship in `env_data.json`, `moderate_intensity` and `high_intensity` (see [Scenario Profiles](#scenario-profiles)). The name `default` is also accepted and means the unmodified baseline. Any other name fails immediately, with a message listing what is available.

`run_scenario(scenario, n_iterations, n_days)` runs a single profile. It returns the raw monitoring data plus two summary tables, both in the project's mean (p10 to p90) with 95% CI format:

- `queue_kpi`, per-resource queue depths
- `totals`, holding `total_casualties`, `wia_count`, `dow_count`, and `dow_rate` (deaths of wounds as a proportion of wounded in action)

`compare_scenarios(scenarios, n_iterations, n_days)` runs each profile in turn and combines the results, labelled by scenario. It writes three files:

- `outputs/scenario_comparison_queues.csv`
- `outputs/scenario_comparison_totals.csv`
- `images/scenario_comparison.png`, mean queue by scenario, faceted across R2B OT, R2E OT, R2E ICU and Transport

The plot labels each scenario by a title-cased form of its identifier (`moderate_intensity` becomes "Moderate Intensity") rather than by trimming its long display label, and the entry point requests a UTF-8 character locale at startup, reporting the fact when none can be set. The long labels contain characters a C locale cannot represent, and these two measures together make the three files identical whatever locale the session runs in.

```bash
# Default comparison: moderate_intensity vs high_intensity, 10 reps x 30 days
Rscript scripts/run_scenarios.R

# Explicit scenario list, custom replication count
Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity,default --iterations 30 --days 30

# Smoke test: 3 reps, 5 days
Rscript scripts/run_scenarios.R --quick
```

```r
# RStudio console
source("R/environment.R"); source("R/trajectories.R"); source("R/replication.R")
source("R/analysis.R"); source("R/scenario_runner.R")
cmp <- compare_scenarios(c("moderate_intensity", "high_intensity"), n_iterations = 10, n_days = 30)
```

Results and interpretation are presented in [Comparative Scenario Analysis](docs/Multi_Run_Analysis.md#comparative-scenario-analysis) in `docs/Multi_Run_Analysis.md`.

#### Shiny Application

`app.R` is a Shiny console that lets users explore the parameter space without reading source code, through a Configure, Run, Analyse workflow.

```r
# Terminal (recommended for Full Analysis / Sensitivity Screening):
#   Rscript -e "shiny::runApp(port = 3838, host = '0.0.0.0')"
#   (port 8787 is already bound by RStudio Server in the dev container —
#   3838, the conventional Shiny Server port, is forwarded in
#   .devcontainer/devcontainer.json)
# RStudio console (Quick Run only — see note below):
shiny::runApp("app.R")
```

Full Analysis, Sensitivity Screening, and the Transport Fleet Capacity Margin Sweep all run replications in parallel and should be launched from a terminal rather than the RStudio console, which does not support the parallel backend they use. Quick Run works in either.

**Configure** presents every editable parameter across six panels: Force Size, Health System Architecture, Medevac, Health Provision, Casualty Rates, and Mass Casualty. Fields carry plain-English labels and a hover tooltip giving the value's provenance, either a citation matching this document's references or an explicit statement that the value is an informed estimate with no published source, so the evidence behind a number is visible while adjusting it.

A **Casualty Intensity Profile** selector above the panels switches between the base configuration and the profiles defined in `env_data.json` (see [Scenario Profiles](#scenario-profiles)): "Falklands — Modified" (the base), "Falklands — Unmodified", and "Okinawa — Casualty Rates". Selecting one refreshes every affected field's default and tooltip in place, flags each overridden field, and names the paths the profile changes. Force size, team counts, bed counts and fleet sizes are never touched by a profile.

Input widgets follow the kind of value:

- Parameters in the Morris screen render as sliders bounded by the screened range, with that range shown in the tooltip ([Sensitivity Analysis](#sensitivity-analysis)).
- Every slider is paired with a numeric box, so a value can be dragged or typed.
- The R2E ICU-Full Priority Override Threshold is a dropdown, since it takes one of three triage priority levels rather than a quantity.
- The Triage Priority Split and the DNBI Sub-Type Split use a single two-handle slider each, so the three shares always sum to 1 and cannot be set to an invalid combination.

Several field groups carry a live preview that redraws as values change, computed from the same functions the simulation itself uses:

- The six casualty-generation streams show a density curve, because a mean and standard deviation say little about a distribution's shape and a long tail plans very differently from a narrow peak. Where a profile makes a stream exponential, its standard deviation field is removed, since an exponential is described by its mean alone.
- All 21 triangular duration groups show a triangular curve above their minimum, most likely and maximum inputs.
- The Priority 1 and Priority 2 DOW Ceilings show the full survival curve ([Died of Wounds](#died-of-wounds)), turning the ceiling percentage into a visible statement about how an untreated casualty's risk accumulates over time.

*Save Configuration* downloads the edited parameters as a timestamped `env_data.json`, and *Load Configuration* accepts a file saved earlier. Neither writes to the copy of `env_data.json` on disk, so exploring parameters cannot alter the repository's tracked configuration.

**Run** offers two modes, both executed in the background so the interface stays responsive. Quick Run executes a single replication, taking roughly 20 seconds for a 30-day run. Full Analysis executes between 10 and 1,000 independent replications, 100 by default, and reports progress as each completes (see [Full Analysis Mode](#full-analysis-mode)). Before either starts, the configuration is checked for force sizes and team counts above zero, triangular minimum, most likely and maximum values in a valid order, and transport capacity of at least one wherever a fleet size is above zero. Any problems are reported together in a single dialog.

**Analyse** presents results across ten tabs: Casualty Flow, Queue Depths, Bed & Resource Utilisation, Transport, Waiting Times, Return to Duty & DOW, Force Regeneration, Strategic AME, Mass Casualty Events, and Sensitivity Calibration. After a Quick Run these show single-run results. After a Full Analysis they show mean values with 95% confidence intervals instead, and four summary cards appear above the tabs giving total casualties, DOW count, R2E ICU peak queue and R2B OT peak queue in the same form.

Every plot can be downloaded as PNG, PDF or CSV, and a *Download All* button zips the three raw monitoring files (`mon_arrivals.csv`, `mon_attributes.csv`, `mon_resources.csv`) from the most recent run. Plots are scaled to fit the browser window, so none needs scrolling within itself to see; where a plot is dense, such as a bed-occupancy chart with many rows, an *Expand to full size* link opens it unscaled in a dialog.

##### Full Analysis Mode

Full Analysis runs the model many times and reports results as averages with 95% confidence intervals, so a finding can be told apart from the variation between runs.

Plots change form accordingly. Queue-depth and Casualty Flow plots put each replication's trace on a shared 240-minute grid, draw a faint line per replication so individual runs stay visible, and overlay a confidence ribbon around the mean. Waiting Times shows a p10 to p90 band with a median line, pooling every replication's casualties per arrival day, since a scatter of individual runs does not aggregate. Bed & Resource Utilisation replaces the single-run Gantt charts, which describe one run's bed-by-bed occupancy and do not generalise across many, with a bar chart of mean utilisation and confidence intervals across four groups: R2B OT, R2E OT, R2E ICU, and the pooled PMV Ambulance and HX240M fleet. The two Strategic AME plots are identical in both modes, since both already handle multiple replications.

Every breakdown and summary available after a Quick Run has a multi-run form. Two are pooled across replications rather than averaged: actual AME wait time by route, and the mass casualty DOW rate comparison. Both report one combined figure, because the counts in any single replication are usually too small for a rate to mean anything.

Four summary cards sit above the tabs: total casualties, DOW count, R2E ICU peak queue and R2B OT peak queue. The two peak-queue cards take each replication's highest summed queue and then average those, so the figure is a typical worst moment rather than the worst moment seen anywhere. Lower bounds are held at zero, since all four are counts.

The replication slider runs from 10, for a quick exploratory check, to 1,000 for a final planning figure, and defaults to 100. Reporting confidence intervals rather than a single seeded run follows published practice for defensible discrete event simulation output in healthcare [[50]](#references). A figure intended for planning use should use a count high enough that raising it further narrows the interval without moving the mean.

Warm-up exclusion is applied identically in both modes and ships at zero (see [Warm-up Period Analysis](#warm-up-period-analysis)).

##### Sensitivity Panel

The Sensitivity Calibration tab's *Run Sensitivity Screening* button runs the Morris screen from within the app. Two controls set the design: trajectories (`r`, default 20, range 3 to 50) and replications per design point (default 5, range 3 to 20). Duration is shared with the Run panel rather than set separately. The screen runs in the background, reporting progress as "evaluating design point M of N".

On completion the µ\* against σ scatter for the R2E ICU queue is drawn in the app, read directly from the Morris result rather than recomputed. See [Sensitivity Analysis](#sensitivity-analysis) for what µ\* and σ measure and which response the ranking is built on. The ranked parameter table uses the same plain-English labels as the Configure panel, and highlights its top five rows.

A *Run Sobol Decomposition* button becomes available once Morris finishes, pre-selecting the top five parameters by µ\* in a checkbox group that can still be adjusted. It reuses the replications-per-point and duration values already set, and reports progress the same way. Results are drawn as a grouped bar chart with 95% bootstrap confidence intervals, following [[49]](#references): S1 is the share of output variance a parameter explains acting alone, and ST the share it explains including every interaction it takes part in. A parameter with high ST but low S1 matters only in combination with others, and a one-parameter-at-a-time check would miss it.

A **Transport Fleet Capacity Margin Sweep** panel sits below Sobol Decomposition and runs independently of either, so neither needs to have run first. It re-runs the model across a range of fleet sizes for a chosen vehicle type and plots the resulting queue and utilisation against a dashed line marking the current establishment, showing how much margin the fleet holds before queues form (see [Transport Fleet Capacity Margin](docs/Single_Run_Analysis.md#transport-fleet-capacity-margin)).

Run time scales with r × (p + 1) × reps, where p is the number of screened parameters. At 53 parameters, the shipped default of r = 20 with 5 replications is a long job; the r = 5 configuration behind the current published ranking took 94 minutes on 4 cores, roughly 20 seconds per design point. Actual times are hardware-dependent. A deliberately small design runs quickly but produces high-uncertainty estimates, with some parameters showing near-zero effect purely through sampling noise, and is useful for checking the pipeline runs rather than for interpreting results.

### Simulation Environment Setup

Before any casualty is generated, the declarative configuration in `env_data.json` is expanded into a populated `simmer` environment. Two stages build the environment itself, and a third populates it at run time. `build_environment()` (`R/environment.R`) converts the parsed JSON into a structured list of populations, resource identifiers, transport identifiers, and parameter values, without touching `simmer` at all. `build_env()` (`R/environment.R`) then takes that list and registers every identifier as an actual `simmer` resource, applying shift schedules where they apply. Two entry points reach the first stage: `load_elms()` reads and parses a file directly, while `load_scenario()` additionally applies a named profile's overrides through `resolve_scenario()` before building (see [Scenario Profiles](#scenario-profiles)). `load_elms()` takes only a path and has no scenario argument.

The expansion step is what turns a compact configuration into an explicit inventory. Each element block in `env_data.json` carries a `qty`, and `build_environment()` repeats that block's full resource set once per instance, numbering each copy. A configuration declaring three R1 teams therefore yields three independent sets of R1 resources rather than one shared set with a capacity of three, which is what allows a casualty to be routed to a specific team and to queue for that team's resources alone.

Every resource carries a structured identifier recording where it sits in that inventory, and these identifiers are what appear in resource monitor output and in the [Model Outputs](#model-outputs) computations. Four forms are produced:

| Form | Pattern | Example |
| --- | --- | --- |
| Personnel in an unsectioned element | `c_<element>_<type>_<role>_<n>_t<instance>` | `c_r1_clinician_doctor_1_t1` |
| Personnel in a sectioned element | `c_<element>_<section>_<section n>_<role>_<n>_t<instance>` | `c_r2b_emerg_1_nurse_3_t2` |
| Beds | `b_<element>_<bed type>_<n>_t<instance>` | `b_r2b_hold_3_t1` |
| Transport vehicles | `t_<vehicle>_<n>` | `t_PMVAmb_1` |

The trailing `t<instance>` counter identifies which team of that element type the resource belongs to, so `b_r2b_hold_3_t1` is the third holding bed of the first R2B team. Transport identifiers carry no such counter because the fleet is pooled across the theatre rather than held by any one element (see [Transport Assets](#transport-assets)).

`build_env()` registers these with `add_resource()`. Most are added with no schedule and are continuously available. The exception is the surgical sections at R2B and R2E, which are assigned alternating day and night shifts across successive teams so that surgical cover is staggered rather than simultaneous; operating theatre rooms themselves are registered without a schedule, since a room is a physical space available at any hour while the team staffing it is not (see [Schedules and Rosters](#schedules-and-rosters) for the shift boundary and its parameter). A section is registered as a vector of individually named resources and seized as a block, which is what makes the roster binding: an off-shift section holds zero capacity, so no procedure can begin against it.

Two strategic aeromedical evacuation resources, `ame` and `ame_critical`, are added when a run is set up (`run_once()`, `R/replication.R`) rather than during environment construction, since they represent a theatre-level airlift capability rather than anything held by a deployed element. Both are created with zero capacity and always exist, because any casualty reaching the strategic evacuation disposition attempts to seize one of them. Capacity is added only when a sortie arrives, so a configuration that schedules no sorties leaves both permanently closed and every strategic evacuee queued (see [Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling)).

Run setup then adds the globals that couple the model to its own outputs, `effective_force_combat` and `effective_force_support`, and attaches the six casualty arrival generators. Each generator is bound to whichever of those two globals matches its population pool, which is what makes arrival rates respond to losses already sustained (see [Force Regeneration and the Endogenous Feedback Loop](#6-force-regeneration-and-the-endogenous-feedback-loop)). The combat WIA generator is wrapped by `wrap_with_mass_casualty()`, which interleaves pre-computed mass casualty arrivals into the background stream in chronological order (see [Casualty Generation](#casualty-generation) and [Mass Casualty Event Injection](#5-mass-casualty-event-injection)).

### Core Trajectory

Every casualty enters the model at R1 and is routed by classification: wounded in action (WIA), disease or non-battle injury (DNBI), or killed in action (KIA). On arrival each casualty is assigned to an R1 team, given a triage priority of 1 to 3 by weighted draw if WIA or DNBI, and assessed for whether it will need surgery. DNBI casualties also receive a sub-type: battle fatigue (25%), disease (58%), or non-battle injury (17%). Battle fatigue carries no surgical need. Disease carries a 6% chance of an emergency surgical condition such as appendicitis, applied regardless of priority.

WIA and non-battle-injury casualties are then checked for died of wounds, using the time-dependent survival function described under [Died of Wounds](#died-of-wounds). The check is evaluated at elapsed time since injury, so at a typical R1 treatment time of about 20 minutes the Priority 1 probability is roughly 0.1%, approaching its 2.3% ceiling only after many hours without treatment. Battle fatigue and disease cases are exempt, since neither has a traumatic injury mechanism. A casualty flagged as died of wounds is reclassified and follows KIA handling.

Survivors are then dispositioned. Around 95% of Priority 1 and 90% of Priority 2 casualties are evacuated to R2B, or directly to R2E if no R2B team is available. Those not meeting the evacuation criteria, mostly Priority 3 and DNBI cases, recover at R1 over 0.5 to 5 days, most often 2, and return to duty.

Durations are drawn from triangular distributions. WIA and DNBI treatment at R1 takes 10 to 30 minutes, most often 20 [[28]](#references). KIA processing takes 10 to 20 minutes, most often 15, followed by transport to the mortuary of 15 to 45 minutes, most often 30.

#### Surgical Pathway

A casualty assessed as needing surgery is also assigned the kind of operation they will receive: staged damage control, or a single-stage definitive procedure. The two differ in what they consume. Damage control is an abbreviated operation to control haemorrhage and contamination, a period of intensive care to correct the casualty's physiology, a return to theatre for the definitive repair, and intensive care after it. A single-stage casualty receives their definitive repair in one operation, so they take one theatre episode instead of two and one episode of intensive care instead of two, with no stabilisation phase between.

Which pathway a casualty takes is decided by physiology rather than by the site of the injury. Damage control is chosen for a casualty exhausted by hypothermia, coagulopathy and acidosis, who would not survive a prolonged definitive procedure; the abbreviated operation buys time to correct that physiology first [[60]](#references), [[22]](#references). A casualty stable on the table is repaired there and then. The model represents physiological derangement through triage priority alone, so the rate is keyed to priority: 55% of Priority 1 casualties, 20% of Priority 2, and 5% of Priority 3. These are informed estimates rather than sourced quantities. They are anchored on the largest reported series, in which 24% of 872 emergent laparotomies across six United States Level 1 trauma centres were damage control, with institutional rates ranging from 16% to 34% [[61]](#references), and uplifted from it because a combat casualty population is more penetrating and blast-dominated than a civilian one. No open-access source reports the split for a deployed military cohort, and none reports it by triage priority, so uncertainty is high. At the shipped rates the realised share across operated casualties runs at roughly half, about twice the civilian figure; were the true combat share materially lower, R2E intensive care and theatre demand would both be overstated, since the staged pathway is the more expensive of the two in each.

The draw is made once, where surgical candidacy itself is decided, and both echelons read the same value. Deciding it forward rather than at each theatre is what keeps the two consistent: a casualty's physiology does not change because a forward operating theatre happened to be free.

```mermaid
flowchart TD
    A(["Start"]) --> B["Set Attributes: <br> priority, dnbi_type, surgery, <br> dcs_pathway (statistically assigned)"]
    B --> C["Assign R1"]
    C --> D{"KIA?"}
    D -- WIA/DNBI --> E["treat casualty"]
    E --> F{"DOW?"}
    F -- DOW --> G["treat KIA"]
    F -- WIA/DNBI --> H{"Evac?"}
    D -- KIA --> G
    H -- Yes --> I{"R2B Ready?"}
    I -- Yes --> J["Transfer to R2B"]
    I -- No --> K["Transfer to R2E"]
    K --> L(["End"])
    J --> L
    H -- No --> M["Recover at R1"]
    M --> N["Return to Duty"]
    N --> L
    G --> O["Transfer KIA"]
    O --> L
```

### R2B Trajectory

Casualties arriving at R2B take a holding bed until a resuscitation bed frees up.

They are then checked for died of wounds, using the same time-dependent survival function as at R1 (see [Died of Wounds](#died-of-wounds)), applied to elapsed time since injury and conditional on having survived the R1 check. Under normal conditions the added risk is very small: a Priority 1 casualty arriving 60 minutes after injury carries an incremental probability of roughly 0.1%. Under evacuation delay or queue pressure, the accumulated elapsed time raises it substantially. A casualty who dies of wounds is processed over 10 to 20 minutes, most often 15, then transported to the mortuary over 15 to 45 minutes, most often 30.

Resuscitation takes 25 to 70 minutes, most often 45. No published duration for the resuscitation phase at a Role 2 facility could be found, so the distribution was built from the tasks the phase involves, with an estimated duration for each, constrained so that all of them complete within the 90 minutes indicated by [[23]](#references):

| Long Resuscitation       |           |            |           |
| ------------------------ | --------- | ---------- | --------- |
| Step                     | Min (min) | Mode (min) | Max (min) |
| Hemorrhage Control       | 2         | 5          | 10        |
| IV/IO Access             | 2         | 5          | 10        |
| TXA Administration       | 10        | 10         | 15        |
| Fluid Resuscitation      | 5         | 10         | 20        |
| Airway/Breathing Support | 3         | 5          | 10        |
| TBI Monitoring & Warming | 2         | 5          | 10        |
| Documentation/Prep       | 2         | 3          | 5         |
| **TOTAL**                | 25        | 45         | 70        |

Casualties not needing surgery need a holding bed, and where that bed is found depends on capacity. A team is only chosen if its holding beds are less than 80% occupied, which reserves headroom for new arrivals rather than letting long-stay patients fill the unit. If a team has room the casualty recovers at R2B over 0.5 to 10 days, most often 5, and returns to duty. If no R2B team is below the threshold but R2E has holding capacity, the casualty is sent to R2E instead. If both are full, the casualty queues for an R2B bed, subject to a cap on queue length.

Surgical candidacy is assessed next, behind an ICU availability gate. Priority 1 casualties proceed regardless of ICU status; Priority 2 and below defer entry to the operating theatre while every ICU bed is occupied, polling on a timer and holding no resource in the meantime. How much work the gate does depends on the forward ICU share described below: at the shipped share of zero the two ICU beds per team serve only the evacuation-wait fallback and the gate is close to inert, while at a non-zero share every casualty operated on here also recovers here, and the gate becomes a real constraint on forward surgical throughput.

Once the gate clears, operating theatre availability decides the route. If a theatre is free, the casualty is operated on over 41 to 210 minutes, most often 95. Surgical durations vary too widely for a single reliable figure; these bounds are the first-look operative-time range reported for a damage control cohort in a systematic review [[20]](#references), whose median of 96 minutes the mode approximates, and they sit within the rapid-closure window of about 90 minutes that damage control technique aims for [[22]](#references). The same distribution serves both surgical pathways, since no open-access source separates single-stage from abbreviated operative times for this population. If no theatre is free, the casualty is evacuated to R2E instead.

A theatre alone is not enough: R2B fields one surgical section per team on a 12-hour shift against a theatre available around the clock (see [Schedules and Rosters](#schedules-and-rosters)), so for half of each day the theatre stands ready with nobody rostered to operate in it. Rather than divert every casualty who arrives in that half, `r2b.surgery.pre_open_window_min` sets how long a casualty may be held forward for a section about to come on shift. A casualty who finds the theatre free and the section closed, but closed for no longer than the window, is received into the theatre and waits there for the section to open; anyone who finds the section closed for longer, or the theatre already occupied, is evacuated to R2E as before. The window ships at 60 minutes, which is an informed estimate rather than a sourced figure: no open-access source states how far ahead of a shift a forward facility should hold a casualty. It is anchored at each end by a quantity the model already carries. Sixty minutes is the interval within which the "golden hour" standard expects a casualty to reach surgical care [[30]](#references), so a hold inside it does not by itself put the casualty outside that standard; and it is long relative to the 15 to 45 minute road move to R2E, which is what makes holding a decision with something at stake rather than an obvious one. Uncertainty is high in both directions. Too long a window holds casualties who would have been better served by moving, and too short a window reproduces the diversion of every off-shift arrival that the parameter exists to relieve.

Holding is not free, and the model shows why. R2B has one theatre per team, and a held casualty occupies it for the wait and then for the operation, so each hold displaces the cases arriving behind it. Whether that trade favours holding is a question for the parameter sweep rather than for the default, and [Multi-Run Analysis](docs/Multi_Run_Analysis.md#the-r2b-pre-open-hold-window) reports what it costs and returns at the shipped value. Setting the window to zero restores the instant diversion exactly, which `scripts/check_pre_open_window.R` asserts along with the properties of the hold itself.

What the operation was determines what follows it. For a casualty on the damage control pathway the forward procedure is the abbreviated one, so a stabilisation phase follows and the definitive repair waits at R2E. For a single-stage casualty the forward procedure is their definitive repair: no stabilisation phase follows, and they need no further operation, so they move on to R2E for the post-definitive intensive care that only that echelon provides. Casualties evacuated to R2E take 15 to 45 minutes to move, most often 30; if no evacuation asset is free, they hold an ICU bed until one becomes available.

#### Post-Operative Stabilisation

Damage control is a staged sequence, and a casualty who goes through it needs intensive care at two separate points for two different reasons [[20]](#references), [[21]](#references). The first is stabilisation: after the abbreviated operation that controls haemorrhage and contamination, intensive care corrects the hypothermia, coagulopathy and acidosis that would make definitive repair unsurvivable. Only then is the casualty returned to theatre. The second comes after that definitive repair, and covers weaning from ventilation, organ support and watching for complications. The model draws the two separately, because they answer to different things. Stabilisation can be delivered at either echelon, and is what forward holding moves; post-definitive care necessarily follows the definitive operation, so it is always served at R2E, the only echelon that performs one. See [R2E Heavy Trajectory](#r2e-heavy-trajectory) for both durations.

Only the damage control pathway has a stabilisation phase, since the phase exists to bridge the interval between two operations and a single-stage casualty has no such interval (see [Surgical Pathway](#surgical-pathway)). Forward holding therefore acts on that cohort alone. Post-definitive care is served on both pathways, every operated casualty having a definitive repair for it to follow.

How much stabilisation a casualty needs follows from the injury rather than from the facility holding them, so the model draws the whole requirement once and divides that single draw between the echelons. The total is therefore the same on either route and at every setting of the policy, because one quantity is being divided rather than two being drawn and kept consistent with each other. Keeping the two episodes distinct is what stops forward holding from hollowing out the care that has to come afterwards: no setting of the forward policy can reduce post-definitive care, because R2B performs no definitive repair for it to follow. Both properties are properties of the design rather than observations about any particular run, and `scripts/check_icu_time_conservation.R` asserts them across all three routes a casualty requiring surgery can take.

Two parameters set the division, because a commander sets forward holding in two different terms. `r2b.post_op_icu.share` is the intent: what fraction of the stabilisation phase to attempt forward at all. `r2b.post_op_icu.forward_hold_max` is the operational limit: how long one casualty may occupy a scarce forward intensive care bed before being moved on regardless of what is outstanding, shipped at 24 hours. The cap binds first, so setting it to zero disables forward holding whatever the share says, and setting it above the longest drawn requirement leaves the share acting alone. Whatever the two allow is served at R2B; the remainder is served at R2E before the definitive operation, which is where the resuscitation phase belongs on that route.

Forward holding is a command lever rather than a clinical fact, and it is not free. An R2B intensive care section fields two nurses and two medics against an R2E section's intensivist and four nurses, so time served forward is served without a resident intensivist, and `r2b_icu_penalty` prices that difference into the casualty's risk of dying of wounds (see [Treatment Efficacy Modifiers](#treatment-efficacy-modifiers)). Where every R2B intensive care bed is already occupied, the stay happens in a holding bed instead, for the same duration and at the further elevated risk the equivalent R2E pathway carries.

The share ships at zero, placing all stabilisation at R2E. That is the conservative setting rather than a recommendation, since it leaves forward bed occupancy where it was. Choosing any other value is a question for evidence rather than for a default, and `scripts/run_icu_share_sweep.R` produces that evidence: it sweeps the share and reports how far the R2E intensive care queue falls, how far forward occupancy rises, and what the movement costs in deaths.

```mermaid
flowchart TD
    A(["Start"]) --> B["Seize Hold Bed"]
    B --> C{"DOW?"}
    C -- Yes --> D["Treat KIA"]
    D --> E["Release Hold Bed"]
    E --> F["Transfer KIA"]
    F --> Z(["End"])
    C -- No --> G["Seize Resus Bed <br> Seize Emerg Team <br> Release Hold Bed"]
    G --> H["Resus"]
    H --> I["Release Resources"]
    I --> J{"Surgery?"}
    J -- Yes --> K0{"ICU Full and<br>Priority 2+?"}
    K0 -- "No (P1, or ICU free)" --> K{"OT Free and<br>Surg Team On Shift?"}
    K0 -- Yes --> KD["Defer: Poll ICU on Timer"]
    KD --> K0
    K -- "Both available" --> L["Seize OT & Surg Team"]
    K -- "OT free, team back<br>within pre-open window" --> KW["Seize OT <br> Hold for Surg Team"]
    KW --> L
    L --> M["Surgery"]
    M --> N["Release Resources"]
    N --> ND{"Damage Control<br>Pathway?"}
    ND -- "No (single-stage,<br>definitive repair done)" --> O
    ND -- Yes --> NS{"Forward ICU<br>Share > 0?"}
    NS -- Yes --> NI{"R2B ICU<br>Bed Free?"}
    NI -- Yes --> NI1["Seize ICU Bed <br> Post-Op Stabilisation"]
    NI -- No --> NI2["Seize Hold Bed <br> Post-Op Stabilisation <br> (elevated DOW risk)"]
    NI1 --> NI3["Release Bed"]
    NI2 --> NI3
    K -- "OT busy, or team back<br>beyond the window" --> O{"Evac Ready?"}
    J -- No --> P0{"R2B Hold < 80%?"}
    P0 -- Yes --> P["Seize Hold Bed"]
    P --> Q["Recover at R2B"]
    Q --> R["Release Hold Bed"]
    R --> S["Return to Duty"]
    S --> Z
    P0 -- "No, R2E has room" --> PB["Bypass to R2E"]
    PB --> Z
    P0 -- "No, both full" --> PC["Queue for R2B Hold Bed"]
    PC --> P
    NS -- No --> O
    NI3 --> O
    O -- Yes --> T["Select R2E"]
    T --> U["Transfer to R2E"]
    U --> V["Release Evac Team"]
    V --> Z
    O -- No --> W["Seize ICU"]
    W --> X["Wait for Evac"]
    X --> Y["Release ICU"]
    Y --> T
```

### R2E Heavy Trajectory

Casualties are checked for died of wounds on arrival (see [Died of Wounds](#died-of-wounds)) and those who have died are handled as KIA. Elapsed time since injury is typically 60 to 180 minutes by this point, a window over which the Priority 1 risk rises steeply, so reception matters most for casualties who have not yet had surgery. Survivors queue for a resuscitation bay. A casualty already resuscitated at R2B receives a short resuscitation of 13 to 55 minutes, most often 28, built from task estimates in the same way as the R2B distribution; anyone else receives the full resuscitation, using the R2B distribution of 25 to 70 minutes, most often 45.

| Short Resuscitation      |           |            |           |
| ------------------------ | --------- | ---------- | --------- |
| Step                     | Min (min) | Mode (min) | Max (min) |
| Hemorrhage Control       | 2         | 5          | 10        |
| IV/IO Access             | 2         | 5          | 10        |
| Fluid Resuscitation      | 5         | 10         | 20        |
| TBI Monitoring & Warming | 2         | 5          | 10        |
| Documentation/Prep       | 2         | 3          | 5         |
| **TOTAL**                | 13        | 28         | 55        |

A casualty who already had their definitive repair at R2B, which is what the single-stage pathway delivers forward, needs no operation here. They pass the post-operative check for died of wounds described below and go straight to post-definitive intensive care, so that reaching R2E already repaired does not let them skip the checkpoint every other operated casualty faces.

For everyone else, surgical candidacy is assessed next, behind an ICU availability gate that is checked before theatre entry rather than at the point of post-operative admission. If an ICU bed is free, surgery proceeds and ICU recovery follows. If ICU is full and the casualty is Priority 1, surgery still proceeds, because withholding it would leave an unoperated Priority 1 casualty at near-certain risk of dying of wounds, but recovery is in a holding bed instead, at elevated risk. If ICU is full and the casualty is Priority 2 or lower, theatre entry is deferred until a bed frees. Surgery takes 41 to 210 minutes, most often 95, drawn from the same operative-time data as R2B [[20]](#references).

A procedure needs both a theatre and the staff for it. R2E has three surgical sections and two theatres, so `select_r2e_surg_section()` (`R/trajectories.R`) picks a section for each casualty rather than fixing one in advance: sections on shift are preferred, and the least busy of those is chosen. The section is held for the whole operation and released before the theatre. Since a section is rostered and a theatre is not (see [Schedules and Rosters](#schedules-and-rosters)), whichever is scarcer at that hour sets how many operations can run at once. A casualty who finds no section free waits, and an operation already under way is not interrupted when the shift ends.

Post-operative care depends on which route the gate sent the casualty down. The stabilisation requirement runs 360 to 2,160 minutes, most often 1,440. Each bound is separately sourced. A position paper on abdominal damage control states that return to theatre "can vary from 6-72 hours from the time of the primary procedure", which sets the 6-hour lower bound, and reports that the majority of surveyed trauma surgeons wait approximately 24 hours, which sets the mode [[60]](#references). A Cochrane review describes the definitive phase as "usually" taking place "within 24 to 36 hours" of the first operation, which sets the 36-hour upper bound [[24]](#references), consistent with the wider damage control literature [[20]](#references), [[27]](#references). The position paper's 72-hour outer limit is deliberately not used as the maximum: a triangular distribution reaching it would place around two casualties in five beyond the window both sources describe as usual. Where it falls in the sequence depends on which operation is this casualty's definitive repair. A casualty operated on only at R2E has their abbreviated procedure here and their definitive one at the second operation below, so stabilisation sits between the two. A casualty already operated on at R2B had their abbreviated procedure forward, which makes the R2E procedure their definitive repair, so whatever stabilisation the forward echelon did not serve is served here before it (see [Post-Operative Stabilisation](#post-operative-stabilisation)). A single-stage casualty has no stabilisation phase at all and takes neither the draw nor a bed for one. On the saturated Priority 1 route, recovery is in a holding bed for 360 to 1,440 minutes, most often 600: shorter than a full ICU stay, but carrying an elevated risk of dying of wounds, and again only for the damage control cohort, the holding-bed stay being the degraded form of the stabilisation phase rather than a separate episode. All routes then meet at a shared post-operative check for died of wounds.

A second procedure follows for a damage control casualty whose abbreviated operation was performed here, that operation being their definitive repair. A casualty who had their abbreviated procedure at R2B has already received their definitive repair on arrival here, and a single-stage casualty never had a staged sequence to complete, so neither returns to theatre.

After the definitive repair, on either route, the casualty receives post-definitive intensive care of 360 to 2,880 minutes, most often 1,440. This is an informed estimate rather than a sourced quantity. The mode is anchored on the deployed norm that coalition casualties admitted to a forward intensive care unit are usually evacuated out of theatre within 24 hours [[56]](#references), but that is a figure for the whole intensive care cohort rather than for the post-definitive phase specifically, and no open-access source reports a post-definitive-repair intensive care duration for a deployed facility. The spread around the mode is not sourced at all. Uncertainty is correspondingly high, and because this episode applies to every casualty who has an operation, on either surgical pathway, it is a direct multiplier on R2E intensive care demand. The episode is bounded in this way because it is the theatre-level portion only: a casualty evacuated strategically continues critical care at Role 4, which this model represents as unconstrained demand rather than as a resource (see [Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling)). When every intensive care bed is occupied the episode is served in a holding bed instead, at the same elevated risk the saturated Priority 1 route carries, since a casualty who has already been operated on cannot be made to wait indefinitely for a bed.

After post-operative recovery a casualty either stays in theatre or is evacuated, and the model decides which by representing the theatre evacuation policy rather than by drawing a fixed share. Doctrine defines the policy as a duration threshold: "a theater that evacuates out of the theater all patients requiring 30 or more days of hospitalization is said to have a '30-day evacuation policy'", and the threshold itself is a command decision, so that "a theater may have an evacuation policy of 15 days whereas another theater may have one of 60 days" [[55]](#references). `draw_recovery_to_duty()` (`R/trajectories.R`) therefore draws each casualty an expected recovery duration at the close of clinical care, and the disposition follows from comparing it against `evacuation_policy_days`, shipped at the doctrinal 30 days and exposed as a planning lever in the Configure panel. The source states the threshold in days of hospitalisation; the model treats that as the casualty's expected time to being fit for duty, which is the same quantity only where a casualty is held until fit, so the two diverge for anyone who would convalesce outside a hospital bed and the model retains such casualties slightly too readily. A casualty retained in theatre then occupies a holding bed for exactly the duration that retained it, so its bed-days and its prognosis cannot disagree.

The recovery duration is a base convalescence distribution of 3 to 63 days, most often 27, scaled by a severity factor keyed to the same four categories that set a casualty's Role 4 ward and length of stay (see [Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling)): 2.33 for an operated Priority 1 casualty, 1.67 for an unoperated one, 1.33 for Priority 2, and 1.0 for Priority 3 and DNBI. Severity therefore drives the prognosis, the ward and the evacuation route from one classification instead of from independent draws, and evacuees emerge as the long-recovery tail of the distribution rather than as an arbitrary sample. The factors themselves are informed estimates: no open-access source tabulates recovery-to-duty durations by triage priority for this population, so they were anchored to the severity gradient already present in the Role 4 length-of-stay values [[34]](#references) and then chosen so that the realised in-theatre share sits inside the historical range discussed in [Return to Duty](#return-to-duty). Uncertainty is correspondingly high, and because the factors set both the retention share and the holding-bed load, an error in them moves R2E bed demand and strategic airlift demand together and in opposite directions.

The share retained in theatre is an output rather than an input. At the shipped configuration it runs at 26.9% across 50 replications, with a 95% confidence interval of 25.8% to 27.9%, inside the 7.6% to 42.1% range of historical in-theatre return-to-duty rates recorded in [[9]](#references).

```mermaid
flowchart TD
    A(["Start"]) --> B{"DOW?"}
    B -- Yes --> C["Treat KIA"]
    C --> D["Transfer"]
    D --> Z(["End"])
    B -- No --> E["Seize Resus Bed <br> Seize Emerg Team"]
    E --> F{"Prev Resus?"}
    F -- Yes --> G["Short Resus"]
    F -- No --> H["Long Resus"]
    G --> I["Release Emerg Team & Resus"]
    H --> I
    I --> J{"Surgery?"}
    J -- No --> P{"Damage Control,<br>R2E Surgery,<br>No Prior R2B Surg?"}
    J -- Yes --> JD{"Single-Stage &<br>Operated at R2B?"}
    JD -- Yes --> PD{"Post-Op DOW?"}
    JD -- No --> K{"ICU Available?"}
    K -- "Yes" --> KP{"Damage Control &<br>Operated at R2B?"}
    KP -- "Yes, stabilisation outstanding" --> KP1["Seize ICU Bed <br> Remaining Stabilisation"]
    KP1 --> KP2["Release ICU"]
    KP2 --> L
    KP -- "No, or none outstanding" --> L["Select Surg Section <br> Seize OT & Surg Section"]
    L --> M["Surgery (First)"]
    M --> N["Release Surg Section & OT"]
    N --> O{"Damage Control &<br>Not Operated at R2B?"}
    O -- No --> PD
    O -- Yes --> Olo["Seize ICU Bed <br> Stabilisation"]
    Olo --> O2["Release ICU"]
    O2 --> PD
    K -- "Full, Priority 1" --> L2["Select Surg Section <br> Seize OT & Surg Section"]
    L2 --> M2["Surgery (First)"]
    M2 --> N2["Release Surg Section & OT"]
    N2 --> O3{"Damage Control?"}
    O3 -- Yes --> O3a["Seize Hold Bed (Post-Op)"]
    O3a --> O4["Release Hold Bed"]
    O4 --> PD
    O3 -- No --> PD
    K -- "Full, Priority 2+" --> KD["Defer: Poll ICU on Timer"]
    KD --> K
    PD -- Yes --> C
    PD -- No --> P
    P -- Yes --> Q["Select Surg Section <br> Seize OT & Surg Section"]
    Q --> R["Surgery (Second)"]
    R --> S["Release Surg Section & OT"]
    S --> PDC{"Had Surgery?"}
    P -- No --> PDC
    PDC -- "Yes, ICU free" --> PDC1["Seize ICU Bed <br> Post-Definitive Care"]
    PDC -- "Yes, ICU full" --> PDC2["Seize Hold Bed <br> Post-Definitive Care <br> (elevated DOW risk)"]
    PDC1 --> PDC3["Release Bed"]
    PDC2 --> PDC3
    PDC3 --> T0["Draw Recovery-to-Duty Days<br>(severity-scaled)"]
    PDC -- No --> T0
    T0 --> T{"Recovery Within<br>Evacuation Policy?"}
    T -- Yes --> U["Seize Hold Bed"]
    U --> V["Recover at R2E<br>(for the drawn duration)"]
    V --> W["Release Hold Bed"]
    W --> X["Return to Duty"]
    X --> Z
    T -- No --> Y{"Priority 1 &<br>Surgical?"}
    Y -- Yes --> YV{"Ventilated?"}
    YV -- Yes --> YV1["Seize ICU Bed<br>Pre-Flight Critical Care"]
    YV1 --> YV2["Seize Hold Bed<br>Release ICU Bed"]
    YV -- No --> YV3["Seize Hold Bed"]
    YV2 --> YW{"DOW While<br>Awaiting AME?"}
    YV3 --> YW
    YW -- Yes --> C
    YW -- No --> Y1a["Seize ame_critical<br>(CCATT/CCAST, small capacity)"]
    Y -- No --> Y2["Seize Hold Bed"]
    Y2 --> YW2{"DOW While<br>Awaiting AME?"}
    YW2 -- Yes --> C
    YW2 -- No --> Y2a["Seize ame<br>(standard, CSU, larger capacity)"]
    Y1a --> Y4["Release Hold Bed"]
    Y2a --> Y4
    Y4 --> Z
```

---

### Role 4 (National Support Base) Demand Modelling

Strategic evacuation is modelled in two layers, because the two halves of it constrain a theatre differently.

**Role 4**, the national support base hospital, is modelled as unconstrained demand. `compute_role4_census()` (`R/analysis.R`) works out bed occupancy after the simulation finishes, from the log of evacuation events. The model estimates what a theatre asks of the national health system; it does not plan that system's capacity, and Further Development records what follows from that.

**Strategic aeromedical evacuation**, the transport that carries a casualty from R2E to Role 4, is a constrained resource inside the simulation: two theatre-wide pools sharing one sortie schedule, seized only at the R2E strategic evacuation disposition. A casualty waiting for a sortie continues to hold a real R2E bed, so evacuation delay competes for beds with clinical care rather than being a bookkeeping entry.

When a casualty is assigned to strategic evacuation, the model records the day the decision was made and whether damage control surgery was performed at either echelon; priority and injury type were already captured at R1. Departure time, departure day and total wait are recorded only once a casualty actually boards. Clinical dwell time at R2E keeps its original meaning of care concluded and disposition decided, so the evacuation wait is reported separately rather than folded into it.

Each evacuated casualty is assigned one of four length-of-stay categories, each a triangular distribution in `env_data.json`. The category sets the Role 4 ward, which R2E bed the casualty occupies while waiting, and which AME pool carries them:

| LoS category    | Assignment criteria                                         | Role 4 ward   | R2E bed while awaiting AME | AME pool       | `env_data.json` key  |
| --------------- | ----------------------------------------------------------- | ------------- | -------------------------- | -------------- | -------------------- |
| P1 Surgical     | Priority 1, `treatment_received = 1`                        | ICU           | Hold bed, after a bounded pre-flight ICU period for the ventilated share | `ame_critical` | `los_p1_surgical`    |
| P1 Non-Surgical | Priority 1, `treatment_received = 0`                        | Surgical Ward | Hold bed                   | `ame`          | `los_p1_nonsurgical` |
| P2              | Priority 2 (any `treatment_received`)                       | Surgical Ward | Hold bed                   | `ame`          | `los_p2`             |
| P3 / DNBI       | Priority 3 WIA, or any DNBI casualty regardless of priority | General Ward  | Hold bed                   | `ame`          | `los_p3_dnbi`        |

Both routes stage in a holding bed, the Casualty Staging Unit equivalent, because every casualty reaching this point has by construction already completed post-operative recovery; the critical and standard split is a distinction in airlift seat type, not in bed type. The exception is the ventilated minority of the critical pool, shipped at 15% and configurable as `critical_hold.ventilated_share`, who genuinely need intensive care up to the point of flight. They hold an ICU bed for 12 to 36 hours, most often 24, and then step down to a holding bed. The bound comes from a deployed intensive care study at Camp Bastion which records that coalition soldiers admitted there "are usually evacuated within 24 h of admission" [[56]](#references); the share itself is an informed estimate, since no open-access source reports what fraction of strategic evacuees require continuing critical care, so uncertainty is high. Were it much larger, R2E ICU would again be consumed by evacuation rather than by treatment. The step-down seizes the holding bed before releasing the ICU bed, so a ventilated casualty is never moved out of intensive care before somewhere exists to move it to; the cost of that ordering is that a saturated holding pool blocks the ICU bed, which is what `ame_icu_hold_minutes` measures.


Two parts of this mapping are informed estimates rather than sourced rules. DNBI casualties take the P3/DNBI category and general ward whatever their in-theatre priority, treating disease, non-battle injury and battle fatigue as lower-acuity for national length-of-stay purposes, consistent with how the model already handles DNBI sub-types elsewhere. Priority 2 casualties take the surgical ward whether or not they were operated on in theatre, on the assumption they need continuing surgical-specialty management, which follows the severity gradient described in [[34]](#references). No open-access source tabulates ward assignment by this scheme, so uncertainty is high. A different mapping would move occupancy between the surgical and general wards without changing total Role 4 bed-days, and would change which R2E bed type evacuees hold while waiting; ICU occupancy is the least affected, since only Priority 1 surgical casualties reach it.

The four length-of-stay distributions, in days, are 10/21/45, 7/14/30, 5/10/21 and 2/5/14 for minimum, most likely and maximum. They are informed estimates reflecting a severity gradient, longest for Priority 1 surgical casualties and shortest for DNBI, directionally consistent with the length-of-stay patterns in [[34]](#references) but not extracted from its tables, so uncertainty is high. Peak Role 4 occupancy and total bed-days scale roughly linearly with these values, so an error biases the figure reported to national planners, but nothing in theatre is affected, since the census is computed downstream of all in-theatre logic.

**Sorties and the airframe.** Evacuation is available only at scheduled opportunities, and each one is cancelled with a set probability. A sortie that flies carries the fitted patient capacity of one named aircraft, filling both pools on the same flight. `resolve_ame_airframe()` (`R/environment.R`) reads the aircraft named by `role4.ame.airframe` and returns its capacity pair; `build_ame_sortie_trajectory()` (`R/trajectories.R`) adds that pair to the two pools whenever a sortie flies. Because each aircraft's capacities are held in their own `role4.airframe_<id>` block, changing aircraft is a matter of naming a different one, in `env_data.json`, through the Shiny selector, or as a scenario override, rather than overwriting two numbers whose provenance would then be lost.

Three aircraft ship, all from the RAAF's account of its aeromedical evacuation capability [[57]](#references), which reports that an AME-configured C-17A "can transport 54 ambulatory and 36 high dependency stretcher patients" and that "the C-130J and C-27J can carry 97 and 21 stretcher patients respectively":

| Airframe | `env_data.json` key | Critical capacity | Standard capacity |
| -------- | ------------------- | ----------------- | ----------------- |
| C-17A Globemaster III | `airframe_c17a`  | 36 | 54 |
| C-130J-30 Hercules    | `airframe_c130j` | 0  | 97 |
| C-27J Spartan         | `airframe_c27j`  | 0  | 21 |

The C-17A is the shipped default, and it is the only one of the three whose figures map onto the model's two pools without interpretation: high dependency stretcher onto the critical pool and ambulatory onto the standard pool, matching the acuity distinction the pools already encode. The source gives the other two aircraft a single stretcher total with no high dependency component, so both ship with a critical capacity of zero and their whole stretcher figure on the standard pool. That is the literal reading of the source rather than a claim that neither aircraft can carry a critical care team; a planner modelling critical care augmentation on either must enter a figure themselves, and none is available to cite. The C-130J's 97 places, higher than the larger C-17A's 90, is the stretched C-130J-30 airframe the RAAF operates, and the same figure appears in the USAF C-130J fact sheet [[58]](#references), so the two sources agree despite how the comparison reads.

Two sortie parameters remain unsourced: the shipped cadence of one sortie every 7 days and the 15% cancellation probability. AJP-4.10(B) [[33]](#references) establishes strategic evacuation, casualty staging and critical care augmentation as planning functions without prescribing either, both being theatre and campaign specific, so they are informed estimates and should be set to the lift cycle of the operation being modelled before any result is relied on. Together with the airframe's capacity they set the scale of both backlogs.

**Which pool a casualty uses** follows AJP-4.10(B) [[33]](#references) rather than an arbitrary split. A casualty reaching strategic evacuation has already completed post-operative recovery, so the default is a holding bed and the standard pool: the doctrine defines a casualty staging unit as holding *already stabilised* patients and describes critical care as an augmentation added only if required, and one explicitly limited by capacity. Priority 1 surgical evacuees are the exception, modelled as still needing in-transit critical care and routed to an ICU bed and the smaller critical pool. The doctrinal distinction is well sourced; which of this model's categories counts as already stabilised is an informed judgement, since the doctrine does not map it to triage priorities. If in reality fewer casualties need in-transit critical care, the model overstates both ICU contention with post-operative recovery and critical-pool backlog; if more do, it understates both.

Two mechanisms are simplifications made for tractability. Unclaimed capacity from an under-subscribed sortie persists on its pool and can be taken by a later arrival, rather than departing with the aircraft as empty seats would. This is an engineering necessity rather than a doctrinal claim: casualties who board never release the resource, matching one-way evacuation, so capacity has to accumulate for a pool to reopen at the next sortie. Its practical effect differs sharply by pool, since the standard pool's capacity comfortably exceeds demand while the critical pool stays saturated regardless. Within a pool, casualties board in the order they reached the disposition, with no further prioritisation beyond the critical and standard split itself. Any finer ordering would redistribute waiting time within a pool without changing its throughput or backlog.

**Dying while waiting.** Casualties queued in either pool are re-assessed for died-of-wounds risk at intervals, so an unbounded wait carries mortality risk rather than none. See [AME Wait Checkpoint](#ame-wait-checkpoint) for the mechanism.

**Unconstrained comparison.** `compute_ame_demand()` (`R/analysis.R`) separately reports how many sorties same-day, uncapped, best-case evacuation would need to clear each day's decisions, dividing each day's evacuation count by the configured airframe's combined capacity and grouping by the day the decision was made rather than the day of departure. Comparing that against what the constrained pools actually achieved is what makes each pool's adequacy visible, including cases where the aggregate looks reasonable while one pool is badly saturated.

Across multiple replications, peak Role 4 occupancy and the unconstrained sortie total are also reported as means with 95% confidence intervals.

---

## Model Outputs

The simulation produces a defined set of Key Performance Indicators (KPIs) organised by planner decision domain. Each KPI is selected against five criteria derived from military medical doctrine and discrete event simulation methodology [[38]](#references):

- **C1 — Doctrinal Standard Compliance:** Variable measures compliance with a named standard in AJP-4.10 [[33]](#references).
- **C2 — Planner Decision Relevance:** Variable value would change a force structure, positioning, or evacuation policy decision.
- **C3 — Causal Pathway Position:** Variable lies on the causal path between input parameters and health outcomes, required for meaningful Morris sensitivity screening.
- **C4 — Binding Constraint Identification:** Variable identifies when a resource or process becomes the active bottleneck.
- **C5 — Health Outcome Attribution:** Variable connects to a measurable health outcome (mortality, RTD, time-to-care).

This same set is the response set the Morris screen ranks parameters against; [Screening Response Set](#screening-response-set) records which responses each KPI below contributes and the scalar reduction applied where a KPI is a vector, a distribution, or a time series.

**Point-of-injury time.** The simulation generates casualties as entities entering at Role 1. There is no pre-R1 phase modelled. Simmer's `start_time` in the arrivals monitor equals R1 arrival time, not point of injury. All time-to-care KPIs are therefore measured from R1 arrival, not point of injury. The POI-to-R1 transit falls outside the model's scope and cannot be derived from the current simulation structure. See Further Development for the impact assessment.

---

### Domain 1 — Mortality and Preventable Death

**Total DOW Count.** Count of casualties assigned `dow = 1` across all replications. Includes all echelons.

- **Doctrinal basis:** AJP-4.10 §3: evacuation timeliness standards are designed to minimise preventable death.
- **Criteria:** C1, C2, C5
- **Computation:** `sum(attributes_wide$dow == 1, na.rm = TRUE)` per replication.
- **Note:** DOW probability is time-dependent, so DOW count increases under queue saturation and evacuation delay relative to non-congested baseline values, making this metric sensitive to system load.

**DOW Rate by Echelon.** Count and proportion of DOW deaths occurring at each echelon (R1, R2B, R2E), derived from the `dow_echelon` attribute. Attribute encoding: 1 = R1, 2 = R2B, 3 = R2E (simmer supports only numeric attribute values).

- **Doctrinal basis:** AJP-4.10 §5: echelon-specific mortality is the primary indicator for role-appropriate capability allocation.
- **Criteria:** C1, C2, C3, C5
- **Computation:** Filter `attributes_wide` where `dow == 1`; decode `dow_echelon` (1→"r1", 2→"r2b", 3→"r2e"); count by decoded echelon label; divide by total arrivals for rate. Consistency check: echelon subtotals must sum to total DOW count.
- **Note:** Echelon DOW rates are sensitive to system load. Elevated R2B or R2E DOW rates indicate that transport or admission delays are accumulating mortality risk in the corresponding phase of care.

---

### Domain 2 — Time-to-Care from R1 Arrival

**Time from R1 Arrival to First Surgical Incision.** Elapsed time (minutes) from R1 arrival (`start_time`) to first surgical incision (`min(r2b_surgery_start, r2e_surgery_1_start)`), per casualty requiring surgery.

- **Doctrinal basis:** AJP-4.10 §5 and the NATO 10-1-2 timeline specify surgical intervention within 2 hours of point of injury. This KPI measures the within-system component of that standard.
- **Criteria:** C1, C2, C3, C5
- **Computation:** `pmin(r2b_surgery_start, r2e_surgery_1_start, na.rm = TRUE) - start_time`; exclude KIA and DOW cases where death preceded any surgery; report mean, p10, p90.
- **Limitation:** Measured from R1 arrival, not point of injury. The POI-to-R1 component (evacuation from point of wounding to R1) is outside the model's scope and must be added separately to compare against the doctrinal 2-hour standard.

**R2B Dwell Time.** Time (minutes) a casualty spends at R2B from treatment start (`r2b_treatment_start_time`) to departure towards R2E (`r2b_departure_time`).

- **Doctrinal basis:** AJP-4.10 §5 specifies that R2B (Role 2 Basic) dwell should not exceed the damage control surgery window; extended dwell indicates holding capacity pressure.
- **Criteria:** C1, C3, C4
- **Computation:** `r2b_departure_time - r2b_treatment_start_time`; report mean and p90.

**R2B→R2E Transit Time.** Time (minutes) between R2B departure (`r2b_departure_time`) and R2E arrival (`r2e_arrival_time`).

- **Doctrinal basis:** AJP-4.10 §5 evacuation time norms for second-echelon to third-echelon transfer.
- **Criteria:** C1, C3
- **Computation:** `r2e_arrival_time - r2b_departure_time`; report mean and p90.

**R2E Dwell Time.** Time (minutes) a casualty spends at R2E from arrival (`r2e_arrival_time`) to disposition (`r2e_departure_time`), covering resuscitation, surgery, ICU, and holding.

- **Doctrinal basis:** R2E (Role 2 Enhanced) dwell is the primary determinant of ICU and OT bed occupancy; AJP-4.10 §5 capacity planning norms are calibrated to expected dwell distributions.
- **Criteria:** C1, C3, C4
- **Computation:** `r2e_departure_time - r2e_arrival_time`; report mean and p90.

---

### Domain 3 — Surgical Throughput

**OT Utilisation Rate by Echelon.** Server time as a proportion of available capacity-minutes within the observation window, for R2B and R2E operating theatres.

- **Doctrinal basis:** AJP-4.10 §5 bed and OT planning ratios; sustained utilisation above 85% indicates saturation risk.
- **Criteria:** C3, C4
- **Computation:** `sum(server × duration) / (sum(capacity) × observation_window)` per echelon, derived from resource monitor for `b_r2b_ot_*` and `b_r2eheavy_ot_*` resources.

**R2B and R2E Surgery Counts per Day.** Count of surgical cases started per simulation day at each echelon, derived from `r2b_surgery_start` and `r2e_surgery_1_start` / `r2e_surgery_2_start` attributes.

- **Doctrinal basis:** AJP-4.10 §5 OT throughput norms; daily surgical volume is the primary operational throughput indicator for surgical teams.
- **Criteria:** C2, C3, C4
- **Computation:** Floor of surgery start time divided by 1440; count by day and echelon.

---

### Domain 4 — Echelon Load and Capacity

**Resource Queue Length Over Time.** Queue length time-series for each bed type (hold, resus, OT, ICU) at R1, R2B, and R2E, derived from the simmer resource monitor.

- **Doctrinal basis:** AJP-4.10 §5 bed ratios and queue saturation thresholds; sustained non-zero queues indicate structural capacity shortfall.
- **Criteria:** C3, C4
- **Computation:** `queue` column from `get_mon_resources()` filtered by resource name pattern per echelon.

---

### Domain 5 — Flow and Disposition

**RTD Rate by Echelon.** Count and proportion of casualties returning to duty at each echelon (R1, R2B, R2E), decomposed by RTD type (`battle_fatigue` / `clinical`). Derived from the `return_echelon`, `return_day`, and `dnbi_type` attributes. Attribute encoding: `return_echelon` 1 = R1, 2 = R2B, 3 = R2E; `dnbi_type` 1 = battle fatigue.

- **Doctrinal basis:** AJP-4.10 §5 [[33]](#references): in-theatre return-to-duty rate is the primary combat power conservation metric; echelon-level RTD indicates where treatment is most efficient. The `battle_fatigue` sub-type reflects forward behavioural health management capacity (R1 hold, no R2 routing); the `clinical` sub-type reflects Role 2 treatment throughput and efficacy.
- **Criteria:** C1, C2, C5
- **Computation:** Filter `attributes_wide` where `return_day` is not NA; decode `return_echelon` (1→"r1", 2→"r2b", 3→"r2e"); assign `rtd_type` = "battle_fatigue" where `dnbi_type == 1`, else "clinical"; count by `(return_echelon, rtd_type)`; divide by total WIA + DNBI arrivals for rate. Consistency check: echelon × type subtotals must sum to `total_rtd`.

**R2B Bypass Rate.** Proportion of WIA casualties routed directly from R1 to R2E without R2B treatment, identifiable where `r2e_treated` is not NA and `r2b_treated` is NA.

- **Doctrinal basis:** AJP-4.10 §5: bypass indicates either R2B overload or deliberate acuity-based routing policy; elevated bypass rates reduce R2B workload while increasing R2E demand.
- **Criteria:** C2, C3, C4
- **Computation:** Count of `combined` where `!is.na(r2e_treated) & is.na(r2b_treated)`, divided by total WIA arrivals.

---

### Domain 6 — Combat Power

**Total RTD Count (bf_rtd + clinical_rtd).** Total count of casualties assigned `return_day`, decomposed into two operationally distinct sub-totals: `bf_rtd` (battle fatigue casualties returned at R1 without clinical treatment) and `clinical_rtd` (all other RTDs following R1 recovery, R2B hold-bed discharge, or R2E hold-bed discharge). `total_rtd = bf_rtd + clinical_rtd`.

- **Doctrinal basis:** AJP-4.10 §5 and ADDP 4.2: return-to-duty throughput directly determines the rate at which combat power is regenerated from the medical system. `bf_rtd` measures forward behavioural health management; `clinical_rtd` measures Role 2 treatment efficacy. Reporting a combined total without this decomposition overstates clinical RTD output.
- **Criteria:** C2, C5
- **Computation:** `bf_rtd = sum(!is.na(return_day) & dnbi_type == 1)`; `clinical_rtd = sum(!is.na(return_day) & (is.na(dnbi_type) | dnbi_type != 1))`; consistency check: `bf_rtd + clinical_rtd == sum(!is.na(return_day))`.

---

### Domain 7 — Strategic Evacuation and National Support Base Demand

**Role 4 Daily Bed Occupancy by Ward.** Mean concurrent Role 4 (national support base) patients by ward category (ICU, Surgical Ward, General Ward) per simulation day, derived from strategically evacuated casualties' assigned length-of-stay, admitted from the day of *actual* AME departure (`evacuation_day`), not the day the evacuation decision was made (see [Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling)).

- **Doctrinal basis:** AJP-4.10 [[33]](#references) mandates that Role 4 requirements be derived from theatre casualty estimates.
- **Criteria:** C2, C3, C5
- **Computation:** `compute_role4_census()` equivalent logic in `analyse_run()`: assign `los_category`/`ward` from `injury_type`, `priority`, `treatment_received` for casualties with a completed AME departure (`!is.na(evacuation_day)`); draw `los_days` from the matching triangular distribution (`env_data$vars$role4`); expand each casualty into one row per occupied day between `evacuation_day` and `evacuation_day + ceiling(los_days) - 1`; average concurrent occupancy per `(day, ward)` across replications.
- **Note:** Role 4 itself remains an unconstrained demand signal, not a capacity-gated queuing outcome (see Further Development), but its *input* (which casualties have reached Role 4, and when) is now gated by the real constrained AME resource below, not merely the evacuation decision.

**Unconstrained-Baseline AME Sortie Demand.** Daily and cumulative strategic aeromedical evacuation sortie requirements *if* AME had same-day, uncapped capacity: a theoretical comparison baseline, not a prediction of actual throughput (see the real constrained-resource outputs below).

- **Doctrinal basis:** AJP-4.10 [[33]](#references) strategic evacuation planning function.
- **Criteria:** C2, C4, C5
- **Computation:** `sorties_required = ceiling(daily_evacuation_count / ame_capacity)` grouped by `evacuation_decision_day`, where `ame_capacity` is the configured airframe's combined standard + critical throughput (`resolve_ame_airframe()`, `R/environment.R`); `cumulative_sorties = cumsum(sorties_required)`.
- **Note:** A derived planning metric, not a simulated resource constraint.

**Strategic AME Wait Time (by Route).** Elapsed time (minutes) from evacuation decision (`r2e_departure_time`) to actual AME boarding (`ame_departure_time`), decomposed by route (critical/ICU/CCATT-CCAST vs standard/Hold/CSU, see [Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling)) as well as reported overall, for casualties who have completed evacuation by the end of the run; also reports the count still queued (`n_awaiting`) per route at end of run.

- **Doctrinal basis:** AJP-4.10 [[33]](#references) strategic evacuation timeliness planning; the CSU/CCATT-CCAST distinction (see [Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling)) is exactly what the route decomposition is designed to make visible.
- **Criteria:** C2, C4, C5
- **Computation:** `ame_wait_minutes = ame_departure_time - r2e_departure_time`, computed in the R2E Heavy Trajectory's Strategic Evac branch at the moment `seize("ame", 1)` or `seize("ame_critical", 1)` succeeds (`ame_route` records which); `analyse_run()` reports `n_evacuated`, `n_awaiting`, and mean/p10/p90 `ame_wait_minutes` for "Overall" and each route separately (`ame_wait_time_summary`).
- **Note:** No further acuity-based boarding priority beyond the critical/standard route split itself is modelled; see [Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling).

**Strategic AME Backlog Over Time (by Pool).** Count of casualties simultaneously awaiting AME sortie capacity, by simulation time, for each of the two AME pools separately (critical-pool casualties each occupy an R2E ICU bed; standard-pool casualties each occupy an R2E Hold bed).

- **Doctrinal basis:** AJP-4.10 [[33]](#references). Backlog size is the direct visible consequence of a schedule/capacity combination inadequate to theatre demand; reporting the two pools separately is necessary because, as the seed-42 results show, one pool can be saturated while the other clears completely.
- **Criteria:** C3, C4, C5
- **Computation:** `compute_ame_backlog()`/`plot_ame_queue()` (`R/analysis.R`) reconstruct the backlog from per-casualty event timestamps: `r2e_departure_time` (a `+1` event, when the Strategic Evac disposition is decided and the AME wait begins; `ame_route` selects the pool) and `ame_departure_time` (a `-1` event, NA while still waiting), cumulatively summed in event-time order per (replication, pool), rather than from the `"ame"`/`"ame_critical"` resource monitor's own `queue` column. This is a correction, not a stylistic choice: `ame_wait_and_board()` (R/trajectories.R) uses a manual `timeout()`/`rollback()` polling loop (`ame_dow_poll()`) rather than `select()`/`seize_selected()` or a blocking `seize()`, calling `seize(resource_name, 1)` only once capacity is already confirmed available, so a waiting casualty never registers in simmer's own queue tracking for these two resources, and the `queue` column is structurally always 0 regardless of the true backlog. An initial implementation of this plot read that column directly and, verified against a real seed-42 run with 93 casualties genuinely still awaiting AME at run end, rendered a flat zero line for the entire run on both pools; the event-based reconstruction instead reproduces the peak backlog figures reported in [Strategic Evacuation and Role 4 Demand](docs/Single_Run_Analysis.md#strategic-evacuation-and-role-4-demand) below. Faceted by pool (and by replication when more than one is present), with independent y-axis scales given the pools' very different capacity magnitudes.
- **Note:** Because critical-pool-awaiting casualties occupy a real R2E ICU bed, a sustained critical-pool backlog also directly increases contention on that same bed pool for unrelated post-operative recovery casualties; see Further Development.

**Strategic AME Sortie Timeline.** The outcome of every scheduled AME sortie opportunity across the run: whether it flew or was cancelled (the `failure_probability` roll), how many seats each pool's added capacity brought, and how many of those seats were boarded before the next scheduled sortie.

- **Doctrinal basis:** AJP-4.10 [[33]](#references) strategic evacuation planning function. The sortie schedule (see [Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling)) is only visible as a schedule/capacity/backlog time series, not from the aggregate wait-time or backlog outputs alone; a planner sizing lift needs to see which opportunities actually flew and how much of each one's capacity was taken up.
- **Criteria:** C3, C4, C5
- **Computation:** `compute_ame_sorties()` (`R/analysis.R`) reconstructs every scheduled opportunity from the `"ame"`/`"ame_critical"` resource monitor rather than from a dedicated sortie log (`build_ame_sortie_trajectory()` keeps none): the schedule itself is deterministic (fixed `at(seq(...))` times), so each opportunity's outcome is read as the capacity delta at that exact time (0/0 = cancelled; matched against the configured airframe's capacity pair otherwise). Seats used is the change in the resource's `server` count (a boarded casualty is never released, as `build_ame_sortie_trajectory()`'s roxygen records, so `server` is monotonically non-decreasing) between this sortie and the next scheduled sortie exclusive (or end of run for the last one), not the backlog waiting at the sortie's own instant: an earlier implementation used that instantaneous reading and was verified, against a real seed-42 run, to always read 0, because `ame_wait_and_board()` (R/trajectories.R) lets an arriving casualty seize freed capacity immediately with no queueing step, so a sortie's seats are typically claimed by arrivals in the days *following* it rather than by anyone already queued at its own moment (see the backlog output above for the same underlying mechanism). `plot_ame_sortie()` averages capacity added and seats used across replications at each scheduled day (a fixed, schedule-determined x-axis every replication shares) and colours by the modal outcome, flown or cancelled, so the same function serves both Quick Run (mean = the single observed value) and Full Analysis without a branch.
- **Note:** A cancelled sortie (both pools' capacity delta zero) is indistinguishable from a flown sortie of a hypothetical zero-capacity airframe; since no selectable airframe has zero capacity on both pools simultaneously, this is not a practical ambiguity. Because capacity is additive and never expires ([Role 4 (National Support Base) Demand Modelling](#role-4-national-support-base-demand-modelling)), a sortie's "seats used" can exceed its own "capacity added", since its window drew on capacity banked from an earlier, under-subscribed sortie, not solely its own contribution.

---

### Output Variable Register cross-reference

| KPI                                      | Domain                | Attributes Required                                                           | Criteria   | Analysis Function                                                                   |
| ---------------------------------------- | --------------------- | ----------------------------------------------------------------------------- | ---------- | ----------------------------------------------------------------------------------- |
| Total DOW count                          | Mortality             | `dow`                                                                         | C1, C2, C5 | `sum(dow == 1)`                                                                     |
| DOW rate by echelon                      | Mortality             | `dow`, `dow_echelon`                                                          | C1–C3, C5  | `dow_by_echelon`                                                                    |
| Time to first surgery                    | Time-to-care          | `r2b_surgery_start`, `r2e_surgery_1_start`, `start_time`                      | C1–C3, C5  | `time_to_first_surgery`                                                             |
| R2B dwell time                           | Time-to-care          | `r2b_treatment_start_time`, `r2b_departure_time`                              | C1, C3, C4 | `r2b_dwell_time`                                                                    |
| R2B→R2E transit                          | Time-to-care          | `r2b_departure_time`, `r2e_arrival_time`                                      | C1, C3     | `r2b_r2e_transit_time`                                                              |
| R2E dwell time                           | Time-to-care          | `r2e_arrival_time`, `r2e_departure_time`                                      | C1, C3, C4 | `r2e_dwell_time`                                                                    |
| OT utilisation                           | Surgical              | resource monitor                                                              | C3, C4     | `ot_utilisation`                                                                    |
| Surgery counts/day                       | Surgical              | `r2b_surgery_start`, `r2e_surgery_*`                                          | C2–C4      | `r2b_summary`, `r2e_summary`                                                        |
| Queue length over time                   | Echelon load          | resource monitor                                                              | C3, C4     | resource plots                                                                      |
| RTD rate by echelon × type               | Flow/disposition      | `return_day`, `return_echelon`, `dnbi_type`                                   | C1, C2, C5 | `rtd_by_echelon` (columns: `return_echelon`, `rtd_type`, `rtd_count`, `rtd_rate`)   |
| R2B bypass rate                          | Flow/disposition      | `r2b_treated`, `r2e_treated`                                                  | C2–C4      | derived in `combined`                                                               |
| Total RTD count (bf + clinical)          | Combat power          | `return_day`, `dnbi_type`                                                     | C2, C5     | `bf_rtd`, `clinical_rtd`, `total_rtd`                                               |
| Role 4 bed occupancy by ward             | Strategic evac/Role 4 | `r2e_evac`, `injury_type`, `priority`, `treatment_received`, `evacuation_day` | C2, C3, C5 | `role4_census_daily`, `role4_summary`, `role4_replication_summary`                  |
| Unconstrained-baseline AME sortie demand | Strategic evac/Role 4 | `r2e_evac`, `evacuation_decision_day`                                         | C2, C4, C5 | `ame_demand_daily`, `ame_summary`, `ame_replication_summary`                        |
| Strategic AME wait time by route         | Strategic evac/Role 4 | `r2e_departure_time`, `ame_departure_time`, `ame_wait_minutes`, `ame_route`   | C2, C4, C5 | `ame_wait_time_summary`                                                             |
| Strategic AME backlog over time by pool  | Strategic evac/Role 4 | resource monitor (`"ame"`, `"ame_critical"`)                                  | C3–C5      | `plot_ame_queue()` (`ame_backlog_plot`)                                             |
| Strategic AME sortie timeline            | Strategic evac/Role 4 | resource monitor (`"ame"`, `"ame_critical"`)                                  | C3–C5      | `compute_ame_sorties()`, `plot_ame_sortie()` (`ame_sortie_data`, `ame_sortie_plot`) |

---

## Further Development

This section records what the model does not represent, how much each gap matters, and what would close it. Entries carry a stable identifier so they can be cited from the analysis documents and the action plan; identifiers are never reused, so gaps in the sequence are entries that have been closed and removed. Sequencing and status are tracked in `docs/BCH_Simulation_Action_Plan.md`, not here.

| | Gap | Impact |
|---|---|---|
| L3 | Resource seizure granularity | High |
| L17 | Strategic evacuation backlog blocks R2E beds | High |
| L1 | Point of injury to R1 transit not modelled | Medium |
| L4 | R2B holding capacity below expected occupancy | Medium |
| L11 | OT and ICU gating parameters are informed estimates | Medium |
| L12 | Scenario calibration incomplete | Medium |
| L16 | Role 4 modelled as unconstrained demand | Medium |
| L18 | Screening precision and coverage | Medium |
| L19 | Transport capacity margin tested at one casualty rate | Medium |
| L20 | Mass casualty events generate wounded only | Medium |
| L21 | R2B surgical throughput options cannot be tested | Medium |
| L22 | DOW calibration target is a bounded treated-cohort rate | Medium |
| L23 | Recovery-to-duty severity factors are uncalibrated | Medium |
| L24 | Saturated-ICU recovery does not conserve the post-operative requirement | Medium |
| L26 | Theatre occupancy does not vary with casualty severity | Medium |
| L27 | The rate cap flattens the top of every stream's distribution | Medium |
| L28 | The R2B pre-open hold window has no source | Medium |

### High Impact

**L3 — Resource seizure granularity.** Resources are taken as whole team vectors, so a second casualty cannot use any member of a team even when the first needs only a subset of its skills. A surgical section is held complete for the duration of a procedure, which means its four nursing staff are unavailable to any other task for as long as the anaesthetist and surgeons are operating, and an operation cannot begin because one member is committed elsewhere even where the remaining members would suffice. Skill-specific bottlenecks between surgeon, anaesthetist and nursing staff are therefore invisible, as is task sharing under surge. The direction of the resulting error is not uniform: whole-team seizure overstates scarcity where a procedure needs only part of a section, and understates it where staff are in practice shared across concurrent cases. Closing the gap requires moving from team-block to individual resource seizure, a structural refactor of every trajectory that seizes a clinical team.

**L17 — R2E holding beds carry recovery and evacuation on one pool.** A casualty awaiting a sortie holds a real R2E holding bed for the whole of its wait, and so does a casualty recovering to duty in theatre, so a single pool of 30 beds serves two unrelated demands. At the shipped airframe capacity the evacuation wait is short, a mean of 2.7 days across a 90-day run, and accounts for roughly a third of the pool's occupancy; the remainder is in-theatre recovery. The pool is nonetheless heavily loaded, averaging 88% occupancy with at least one casualty queued for 60% of that run, so the binding constraint is the bed count itself rather than airlift. What that does is push back into intensive care. A ventilated casualty on the critical route cannot step down from its bounded pre-flight ICU period while the holding pool is full, so its ICU occupancy stretches: the median is 28 hours, consistent with the documented norm, but the mean is 94 hours and the 90th percentile roughly thirteen days. Any R2E ICU or theatre-gating capacity finding must therefore be read alongside the holding pool's occupancy rather than in isolation. Separating the staging pool from the clinical holding beds, or expanding the pool, would each reduce the coupling; the defaults were deliberately not tuned to hide it.

### Medium Impact

**L1 — Point of injury to R1 transit not modelled.** Casualties enter the model at R1, so tourniquet application, self and buddy aid, and tactical field care all sit outside its scope. Every time-to-care measure runs from R1 arrival rather than from wounding, which means the time to first surgical incision covers only the within-system delay and cannot be compared directly against the doctrinal two-hour standard. Closing it means either modelling the pre-R1 phase or carrying an explicit offset that a planner can add.

**L4 — R2B holding capacity below expected occupancy.** Expected concurrent occupancy is around 15.5 beds against ten across both R2B units. Two-tier routing bounds the consequences: a team is only selected while its holding beds are below 80% occupied, and on arrival a casualty takes a bed if one is free, diverts to R2E if not, or queues within a cap when both echelons are saturated. Casualties are always dispositioned in finite time, but the routing shifts load onto R2E. Expanding to eight or ten beds per unit resolves it structurally; an evacuation threshold routing long-stay holders forward earlier is the lighter alternative.

**L11 — OT and ICU gating parameters are informed estimates.** The Priority 1 override threshold, the post-operative hold mortality multiplier and the post-operative hold length of stay are all informed estimates: no open-access source gives a ward-against-ICU mortality ratio for post-damage-control trauma patients, or a typical recovery length of stay outside ICU in an austere setting. Priority 2 and below casualties deferring theatre entry while ICU is saturated also have no escape route, so under sustained saturation one could in principle wait indefinitely rather than being triaged to non-operative management. The direction of the findings should be robust; the absolute post-operative mortality rates should be read as illustrative pending clinical review or a calibration target.

**L12 — Scenario calibration incomplete.** The moderate intensity profile reproduces a killed-to-wounded ratio of 0.452 against a published campaign record of 0.328 (255 killed against 777 injured [[53]](#references), [[54]](#references); the parliamentary total counts injuries to Service personnel and civilians, including 109 cold injuries, so it is broader than a strict wounded-in-action count and the ratio derived from it is correspondingly approximate). This follows from the underlying casualty generation rates combined with the capped lognormal mechanism rather than from the profile itself. The high intensity profile is an explicitly unvalidated skeleton: only its generation rates and distribution family are sourced, while its mortality ceiling, treatment efficacy, priority split, sub-type composition and transport times are all inherited from the Falklands-calibrated base. A third, Vietnam-intensity profile cannot be added from the current source, whose appendix carries no standalone Vietnam combat-troop distribution table. Each part needs different work: recalibrating the generator, completing the high intensity profile from Okinawa-specific sources, and finding a source that tabulates Vietnam rates.

**L16 — Role 4 modelled as unconstrained demand.** The national support base is a post-simulation calculation over the evacuation log rather than a resource with finite capacity, so its occupancy can exceed any real bed count without producing a queue or a deferral. The output is a demand signal for national planners, not a claim that the base can absorb that demand. Strategic evacuation itself is constrained and scheduled, but four narrower gaps remain: boarding within a pool is strictly first come, first served once the critical and standard split has applied; the interval at which waiting casualties are re-assessed for mortality risk is an informed estimate and has been observed to fire only once, so its magnitude is unvalidated; unclaimed sortie capacity carries forward rather than departing with the aircraft, which has no real-world analogue; and one airframe flies the whole campaign, with no mixed fleet and no surge tasking of a second aircraft type.

**L18 — Screening precision and coverage.** The Morris screen covers sixty-five parameters, derived by auditing every numeric parameter in the configuration rather than by expert selection. It runs at five trajectories rather than the default twenty, because the full design at twenty would need 6,500 simulation runs. The method is unbiased at any trajectory count, so the estimates are not skewed, but they are noisier, and parameters close together in influence should be read with more caution than the same gap would warrant at twenty. Separately, the published ranking predates the expansion of the response set, so thirty-five of the thirty-six responses now screened have no ranking measured at production trajectory count; the outstanding wider re-run would close both that and the precision gap together.

**L19 — Transport capacity margin tested at one casualty rate.** The fleet-size sweep runs at the Falklands-derived rate only, so the margin it reports is untested under the conditions most likely to consume it. Re-running it at high intensity and under mass casualty injection would establish whether the finding holds. The sweep function accepts a path to a pre-configured environment file but not a scenario name, so a small interface change is needed first.

**L20 — Mass casualty events generate wounded only.** A mass casualty event injects wounded combat casualties with no immediate deaths and no disease or non-battle injury, which understates the load such an event places on mortuary handling and on R1 transport specifically.

**L21 — R2B surgical throughput options cannot be tested.** Two ways of raising forward surgical throughput are deliberately out of reach. Extending shift hours needs a clinician fatigue and error-rate model the simulation does not have, without which longer hours would appear free. Adding a second surgical team per unit is an establishment decision for planners rather than something the model should assume. The shift-length parameter already threads through to environment construction for the first; the second needs the R2B surgical sub-element at a quantity of two and a rework of the shift-alternation counter, which alternates across units rather than within one.

**L22 — The died-of-wounds calibration target is a bounded treated-cohort rate.** The historical anchor for the mortality ceilings is three deaths among the "over 650" casualties who reached the Ajax Bay Advanced Surgical Centre, a cohort drawn from both sides of the conflict and reported with an inexact denominator. Three consequences follow. The rate of approximately 0.46% is an upper bound rather than a point estimate, so the validation test this project has applied is whether the model's confidence interval spans it, rather than whether the central values agree. The cohort mixes British and Argentine casualties, whose prior treatment and evacuation timelines differ, while the model represents a single force. And because the target constrains only casualties who survived to reach surgical care, the model's whole-of-wounded mortality rate is unconstrained by any historical figure, which is the quantity a planner is most likely to read off the output. Closing this would need a source reporting a campaign died-of-wounds count against an exact wounded-in-action denominator for one force; no open-access source doing so was identified.

Against that bound the base configuration measures 0.44%, with a 95% confidence interval of 0.36% to 0.51% that spans it, and the ceilings are deliberately left as they are. Reaching that conclusion required abandoning the single 50-replication measurement this project had used as its calibration test. Died of wounds averages under one death per replication, and independent 50-replication measurements of the same shipped configuration have returned rates spanning 0.08 percentage points or more across control seeds, a spread comparable to the confidence interval any one of them reports, which is why the figure above pools three measurements over 150 replications. A single measurement of 0.617% was once read as an overshoot requiring the ceilings to be re-fitted; it is one draw from this spread and lies outside every pooled interval since.

How many replications a mortality figure needs follows from the same measurement. The per-replication standard deviation of the treated-cohort rate is 0.0046, so a 95% half-width of 0.15 percentage points needs 39 replications, 0.10 points needs 84, and 0.05 points needs 328. At the 150 replications this project pools, the half-width is about 0.07 points, which resolves the rate to two decimal places and no further; a third decimal place would need of the order of ten thousand replications and is not reported anywhere. Figures at 50 replications, which is what the resource-queue and utilisation measures use, carry a half-width around 0.13 points on this response and should be read as approximate. That guidance applies to mortality specifically. The queue and occupancy measures are time-weighted over far more events per replication and are correspondingly better resolved at the same replication count.

Two further consequences follow for how the bound is used. A test asking whether the model's interval spans the target is stricter than an upper bound supports, since it fails a model for sitting comfortably below a ceiling. The base configuration's interval spans the bound at 0.44%, and `moderate_intensity` sits below it at 0.27% (95% confidence interval 0.21% to 0.33%); neither has its ceilings adjusted, since the one-sided test both satisfy is the only one the source supports, and lifting modelled mortality to meet a bound would add deaths the historical record does not evidence. And the ceilings remain entangled with the treatment efficacy factors, so agreement with the bound confirms the pair jointly and not either alone.

**L23 — Recovery-to-duty severity factors are uncalibrated.** The theatre evacuation policy compares each casualty's drawn recovery-to-duty duration against a configurable threshold, which makes disposition a function of severity, but the four severity factors that scale the base convalescence distribution are informed estimates. No open-access source tabulates time to fitness for duty by triage priority for a battlefield trauma population, so the factors were anchored to the severity gradient in the Role 4 length-of-stay values and then set so that the realised in-theatre share falls inside the historical range. That range spans 7.6% to 42.1%, which is wide enough to admit many factor sets, so agreement with it is a weak test: the mechanism is defensible and the ordering between categories is not in doubt, but the specific values are not calibrated. Because the same factors set both the retention share and the holding-bed occupancy of everyone retained, an error moves R2E bed demand and strategic airlift demand together in opposite directions, and the policy sweep reported in the single-run analysis will be correspondingly too steep or too shallow. Closing the gap needs a source giving recovery-to-duty durations by severity, or a calibration target sharper than the in-theatre share.

**L24 — Saturated-ICU recovery does not conserve the post-operative requirement.** A casualty's post-operative intensive care requirement is drawn once and divided between the echelons, so the total is the same on every ordinary route. One route is outside that guarantee. When R2E intensive care is saturated and a Priority 1 casualty recovers in a holding bed instead, the stay is drawn from its own shorter distribution rather than from what remains of the requirement, so a casualty who served part of it forward at R2B loses the rest. That pathway predates the conservation rule and its holding-bed duration is itself an informed estimate (see L11), which is why it was not simply rebased onto the requirement: doing so would silently lengthen the degraded pathway by a factor of roughly two and change the mortality comparison the pathway exists to expose. At the shipped forward share of zero the gap cannot arise, since nothing is served forward to be lost; at an intermediate share it affected 12 of 93 casualties at 0.25 and 15 of 108 at 0.50 in a 30-day run. Closing it needs either a sourced holding-bed recovery duration to rebase onto, or an explicit decision that degraded recovery is shorter by intent rather than by inheritance.

**L26 — Theatre occupancy does not vary with casualty severity.** One surgery duration distribution serves every casualty who reaches an operating theatre, at both echelons and on both surgical pathways, so a Priority 3 casualty occupies a theatre for as long as a Priority 1 one. Registry data does not support that. Sampling the United States Department of Defense Trauma Registry across two decades of operations, mean operative asset occupancy at a Role 2 facility runs from 93.9 minutes for the mildest injury severity band to 182.9 minutes for the most critical, roughly a twofold spread, and is statistically indistinguishable between Role 2 and Role 3 [[64]](#references). The total load the model places on theatre is nonetheless about right: weighting those band means by the registry's own severity mix gives 123.6 minutes against the 115.3 minute mean of the distribution the model draws from, so the defect is in how theatre time is distributed across casualties rather than in how much of it there is. What follows is that peak contention is understated whenever the casualty mix is more severe than average, which is exactly the condition a mass casualty event creates, and every finding resting on the operating theatre queues should be read with that in mind. Closing it properly would need per-severity distributions the registry source cannot supply, since it reports means and standard deviations without medians, ranges or distribution shape; the lighter alternative is a severity-keyed multiplier on the single distribution, taking the ratios between the published band means and assuming the shape is otherwise unchanged, which the roughly constant coefficient of variation across bands supports.

**L27 — The rate cap flattens the top of every stream's distribution.** The per-minute rate cap that keeps the generator's iteration count bounded (see [Casualty Generation](#casualty-generation)) returns a clamped draw at the cap rather than at the value drawn. The location the generator is parameterised at absorbs that clamping, so each stream averages the daily rate its configuration names, but the shape it averages to is not the shape FORECAS fitted: about 11% of the WIA and KIA streams' minutes sit at exactly three times the mean, against 4% of `dnbi_cbt`'s, 1% of `dnbi_spt`'s and 6% of any exponential stream's. What follows is that day-to-day variability is understated, and with it the peak-day casualty volume that drives contention for theatres, intensive care beds and airlift, so every queue and utilisation figure in the analysis documents should be read as a lower bound on its own peak rather than on its mean. Raising the multiplier moves probability mass back out of the spike but lengthens the generator's walk on the rare extreme draw, the two properties trading directly against one another; the mean is unaffected either way, since the parameterisation is solved against whatever cap is in force. Closing the gap needs the cap removed rather than retuned, which in turn needs a generator that samples arrival times directly instead of walking the run minute by minute, since it is the minute-by-minute walk that makes an extreme draw expensive rather than merely implausible. Simulating the arrival process by thinning [[65]](#references) is the established way to do that, generating candidate times under a dominating rate and rejecting a controlled fraction, and is compatible with the live force-size dependence because the intensity would be read at the candidate time rather than at every minute.

**L28 — The R2B pre-open hold window has no source.** How long a forward facility should hold a casualty for a surgical section about to come on shift, rather than moving them rearward, is a standing-order decision no open-access source quantifies. The shipped 60 minutes is an informed estimate anchored between two quantities the model already carries, the golden-hour standard above it and the road move to R2E below it (see [R2B Trajectory](#r2b-trajectory)), which bounds it plausibly without calibrating it. Two things follow. The window's own value is uncertain in both directions, and because the held casualty occupies the single forward theatre for the whole hold, the parameter trades forward operations against one another rather than simply adding them, so the sign of its effect on forward throughput is a property of the configuration rather than of the mechanism. The screening bounds run from zero to six hours, which is wide enough to admit the whole of that trade. Closing the gap needs either a doctrinal statement of the holding decision or a calibration target on forward surgical throughput; short of either, the parameter is best read as a lever to sweep rather than a default to trust.

Beyond these, model fidelity would benefit from structured expert consultation. Treatment durations, triage logic and evacuation thresholds would all be improved by review from clinicians, medical planners and operational commanders, and several parameters recorded above as informed estimates have no other realistic route to a sourced value.

## Conclusion

<small>[Return to Top](#contents)</small>

This project models deployed health system performance by combining casualty generation calibrated on historical conflict data with a discrete event simulation capable of brigade-level, multi-week campaign runs. Published casualty models were identified through systematic literature review, then restructured to support per-minute granularity, so medical infrastructure can be evaluated across 30-day and longer campaigns.

The modelled system covers triage, evacuation and definitive care, including a phased model of damage control surgery drawn from the clinical literature. A statistical framework of multi-run replication, warm-up analysis, and Morris and Sobol sensitivity screening allows each reported finding to carry its uncertainty rather than rest on a single run. Results are published separately, in [Single-Run Analysis](docs/Single_Run_Analysis.md) and [Multi-Run Analysis](docs/Multi_Run_Analysis.md).

The repository is a foundation for further work rather than a finished decision-support tool. Planners, clinicians and commanders can use it to interrogate system performance and identify where it fails; what would need closing before it could support a planning decision is set out in [Further Development](#further-development).

## References

<small>[Return to Top](#contents)</small>

<!-- REFERENCES START -->

[1] U.S. Army Combined Arms Center. (2025). *ADP 3-0: Operations*. Headquarters, Department of the Army. Retrieved 17 Aug 25, from https://armypubs.army.mil/epubs/DR*pubs/DR*a/ARN30041-ADP_3-0-000-WEB-1.pdf

[2] Remondelli, M. H., Remick, K. N., Shackelford, S. A., Gurney, J. M., Pamplin, J. C., Polk, T. M., ... & Holt, D. B. (2023). Casualty care implications of large-scale combat operations. *Journal of Trauma and Acute Care Surgery*, *95*(2S), S180-S184. Retrieved 17 Aug 25, from https://pmc.ncbi.nlm.nih.gov/articles/PMC10389308/

[3] U.S. Army Combined Arms Center. (2025). *FM 3-0: Operations*. Headquarters, Department of the Army. Retrieved 17 Aug 25, from https://armypubs.army.mil/epubs/DR_pubs/DR_a/ARN43326-FM_3-0-000-WEB-1.pdf

[4] The Economist. (2025, July 9). *Russia’s summer Ukraine offensive looks like its deadliest yet*. Retrieved 26 Jul 25, from https://www.economist.com/interactive/graphic-detail/2025/07/09/russias-summer-ukraine-offensive-looks-like-its-deadliest-so-far

[5] Fandre M. Medical Changes Needed for Large-Scale Combat Operations: Observations from Mission Command Training Program Warfighter Exercises. Military Review. 2020. Retrieved 17 Aug 25, from https://www.armyupress.army.mil/Journals/Military-Review/English-Edition-Archives/May-June-2020/Fandre-Medical-Changes/

[6] Department of Defence. (2023). *National Defence: Defence Strategic Review*, Commonwealth of Australia. Retrieved 17 Aug 25, from https://www.defence.gov.au/about/reviews-inquiries/defence-strategic-review

[7] Department of Defence. (2024). *National Defence Strategy 2024*, Commonwealth of Australia. Retrieved 17 Aug 25, from https://www.defence.gov.au/about/strategic-planning/2024-national-defence-strategy-2024-integrated-investment-program

[8] Blood, CG; Zouris, JM; Rotblatt, D; (1998) *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. Retrieved 20 Jul 25, from https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf

[9] Izaguirre, MK; Cox, D; Lodi, PC; Giraud, RS; Murray, CK; Teyhen, DS; Capaldi, VF; Kelly, KM; Taylor, JF; Holland, JC; Laragione, VJ. (March 2025) *To Conserve Fighting Strength in Large Scale Combat Operations*. Military Review Online. Retrieved 20 Jul 25, from https://www.armyupress.army.mil/Journals/Military-Review/Online-Exclusive/2025-OLE/Conserve-Fighting-Strength-in-LSCO/

[10] Gibson, D (2003). *Casualty estimation in modern warfare*. The Free Library. Retrieved 20 Jul 25, from https://www.thefreelibrary.com/Casualty%2Bestimation%2Bin%2Bmodern%2Bwarfare.-a0110459243

[11] Holcomb, J. B., Stansbury, L. G., Champion, H. R., Wade, C., & Bellamy, R. F. (2006). *Understanding combat casualty care statistics*. U.S. Army Institute of Surgical Research. Retrieved 20 Jul 25, from https://apps.dtic.mil/sti/pdfs/ADA480496.pdf

[12] Howard, J. T., Kotwal, R. S., Stern, C. A., Janak, J. C., Mazuchowski, E. L., Butler, F. K., ... & Smith, D. J. (2019). Use of combat casualty care data to assess the US military trauma system during the Afghanistan and Iraq conflicts, 2001-2017. *JAMA surgery*, *154*(7), 600-608. Retrieved 01 Aug 25, from https://jamanetwork.com/journals/jamasurgery/articlepdf/2729451/jamasurgery_howard_2019_oi_190007.pdf

[13] Jackson, D. S., Batty, C. G., Ryan, J. M., & McGregor, W. S. (1983). The Falklands war: Army field surgical experience. *Annals of the Royal College of Surgeons of England*, *65*(5), 281–285. Retrieved 02 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC2494365/

[14] Westphalen, N. (2018). Surgeon Captain Richard Tadeusz 'Rick' Jolly OBE RN Rtd. *Journal of Military and Veterans' Health*, *26*(1). Retrieved 26 Jul 26, from https://jmvh.org/article/surgeon-captain-richard-tadeusz-rick-jolly-obe-rn-rtd/

[15] Ucar I, Smeets B, Azcorra A (2019). “simmer: Discrete-Event Simulation for R.” *Journal of Statistical Software*, 90(2), 1–30. [doi:10.18637/jss.v090.i02](https://doi.org/10.18637/jss.v090.i02).

[16] Maddeh, M., Ayouni, S., Al-Otaibi, S., Alazzam, M. B., Alturki, N. M., & Hajjej, F. (2023). Discrete-Event Simulation Model for Monitoring Elderly and Patient’s Smart Beds. *Journal of Disability Research*, *2*(3), 1-9. DOI: 10.57197/JDR-2023-0026. Retrieved 26 Jul 25, from https://www.scienceopen.com/hosted-document?doi=10.57197/JDR-2023-0026

[17] Kemple, W. G., & Lacy, L. W. (1995). *Modeling command and control: The design and implementation of the C2 model*. Defense Technical Information Center. Retrieved 20 Jul 25, from https://apps.dtic.mil/sti/html/tr/ADA304910/)

[18] Wang, Y., & Pinsky, E. (2023). Geometry of deviation measures for triangular distributions. *Frontiers in Applied Mathematics and Statistics*, *9*, 1274787. Retrieved 26 Jul 25, from https://doi.org/10.3389/fams.2023.1274787

[19] Izaguirre, M. K., Lopez, J. A., & Smith, T. R. (2025). To conserve fighting strength in large scale combat operations. *Military Review Online*. Retrieved 26 Jun 26, from https://www.armyupress.army.mil/Journals/Military-Review/Online-Exclusive/2025-OLE/Conserve-Fighting-Strength-in-LSCO/

[20] Zizzo, M., Ruiz, C. C., Zanelli, M., Bassi, M. C., Sanguedolce, F., Ascani, S., & Annessi, V. (2020). Damage control surgery for the treatment of perforated acute colonic diverticulitis: a systematic review. *Medicine*, *99*(48), e23323. Retrieved 26 Jul 25, from https://journals.lww.com/md-journal/fulltext/2020/11250/damage_control_surgery_for_the_treatment_of.43.aspx

[21] Karamarković, A. Damage Control in Abdominal Surgery. *Clin Surg. 2016; 1*, *1118*. Retrieved 02 Aug 25, from https://www.clinicsinsurgery.com/open-access/damage-control-in-abdominal-surgery-2563.pdf

[22] Beldowicz, B.C. (2018). The evolution of damage control in concept and practice. *Clinics in Colon and Rectal Surgery*, *31*(1), 30–35. Retrieved 25 Jun 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC5787400/

[23] Abri, M. A., Snani, S. A., Almayahi, J., Sharqi, A. A., & Qadhi, H. A. The Outcome of Damage Control Surgery at Sultan Qaboos University Hospital. *World J Surg Surgical Res. 2022; 5*, *1428*. Retrieved 26 Jul 25, from https://www.surgeryresearchjournal.com/open-access/the-outcome-of-damage-control-surgery-at-sultan-qaboos-university-9532.pdf

[24] Chaudhry, R., Tiwari, G.L., & Singh, Y. (2006). Damage control surgery for abdominal trauma. *Medical Journal, Armed Forces India*, *62*(3), 259–262. Retrieved 25 Jun 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC4922877/

[25] Turner, J., & Wilson, A. (2024). Backed into a corner: damage control surgery in the rural or austere setting. *Trauma Surgery & Acute Care Open*, *9*(Suppl 2), e001391. Retrieved 02 Jul 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC11029234/

[26] Hardcastle, T. C., Gaarder, C., Balogh, Z., et al. (2025). Guidelines for Enhanced Recovery After Trauma and Intensive Care (ERATIC): ERAS Society and IATSIC Recommendations: Paper 1: Initial Care — Pre and Intraoperative Care Until ICU, Including Non-Operative Management. *World Journal of Surgery*, *49*(8), 1997–2028. Retrieved 02 Jul 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC12338446/

[27] Nickson, C. (2020, November 3). *Damage Control Resuscitation*. Life in the Fastlane. Retrieved 27 July, 2025, from https://litfl.com/damage-control-resuscitation/

[28] Hodický, J., Procházka, D., Jersák, R., Stodola, P., & Drozd, J. (2020). Optimization of the casualties' treatment process: Blended military experiment. *Entropy*, *22*(6), 706. Retrieved 25 Jun 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC7517244/

[29] Eastridge, B. J., Mabry, R. L., Seguin, P., Cantrell, J., Tops, T., Uribe, P., ... & Blackbourne, L. H. (2012). Death on the battlefield (2001–2011): implications for the future of combat casualty care. *Journal of Trauma and Acute Care Surgery*, *73*(6 Suppl 5), S431–S437. Retrieved 29 Jun 26, from https://apps.dtic.mil/sti/pdfs/ADA609611.pdf

[30] Kotwal, R. S., Montgomery, H. R., Kotwal, B. M., Champion, H. R., Butler Jr, F. K., Mabry, R. L., ... & Holcomb, J. B. (2011). Eliminating preventable death on the battlefield. *Archives of Surgery*, *146*(12), 1350–1358. Retrieved 29 Jun 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC5832013/

[31] Braverman, M. A., Smith, A., Arshad, M. I., Cannon, J. W., Borgman, M. A., Holcomb, J. B., Etchill, E. W., DuBose, J. J., Rasmussen, T. E., Edwards, J., Epley, E., Glaser, J. J., Redfield, C. S., Schreiber, M. A., & Morrison, J. J. (2021). Damage control resuscitation in patients undergoing emergency laparotomy: outcomes and implications. *Journal of Trauma and Acute Care Surgery*, *92*(2), 321–328. Retrieved 01 Jul 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC8600903/

[32] Holcomb, J. B., Del Junco, D. J., Fox, E. E., Wade, C. E., Cohen, M. J., Schreiber, M. A., Alarcon, L. H., Bai, Y., Brasel, K. J., Bulger, E. M., Cotton, B. A., Matijevic, N., Muskat, P., Myers, J. G., Phelan, H. A., White, C. E., Zhang, J., Rahbar, M. H., & PROMMTT Study Group. (2013). The prospective, observational, multicenter, major trauma transfusion (PROMMTT) study: comparative effectiveness of a time-varying treatment with competing risks. *JAMA Surgery*, *148*(2), 127–136. Retrieved 01 Jul 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC3773975/

[33] NATO Standardization Office. (2019). *AJP-4.10 allied joint doctrine for medical support* (Edition C, Version 1). NATO Standardization Office. Retrieved 25 Jun 26, from https://www.coemed.org/files/stanags/01_AJP/AJP-4.10_EDC_V1_E_2228.pdf

[34] Nessen, S. C., Lounsbury, D. E., & Hetz, S. P. (Eds.). (2008). *War Surgery in Afghanistan and Iraq: A Series of Cases, 2003–2007*. Borden Institute, Office of The Surgeon General, US Army. Retrieved 13 Jul 26, from https://medcoe.army.mil/borden-tb-war-surgery-afg-iraq/

[35] Fischer, J., Al-Husseini, M., Krishnamoorthy, R., Kumar, V., & Kochenderfer, M. J. (2025). Digital simulations to enhance military medical evacuation decision-making. Open-access preprint retrieved 02 Jul 26, from https://arxiv.org/abs/2507.06373

[36] Debacker, M., Van Utterbeeck, F., Ullrich, C., Dhondt, E., & Hubloue, I. (2016). SIMEDIS: a discrete-event simulation model for testing responses to mass casualty incidents. *Journal of Medical Systems*, *40*(12), 273. Retrieved 10 Jul 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC5069323/

[37] Dilday, T. (2026, April 20). *From MASCAL to Campaign Medicine: Aligning Field Hospital Training with LSCO Reality*. U.S. Army. Retrieved 10 Jul 26, from https://www.army.mil/article/290575

[38] Sargent, R. G. (2010). Verification and validation of simulation models. In *Proceedings of the 2010 Winter Simulation Conference* (pp. 166–183). IEEE. Retrieved 25 Jun 26, from https://www.informs-sim.org/wsc10papers/016.pdf

[39] Banks, J., Carson, J. S., Nelson, B. L., & Nicol, D. M. (2005). *Discrete-Event System Simulation* (4th ed.). Pearson Prentice-Hall.

[40] Rossetti, M. D. *Simulation Modeling and Arena*, Chapter 5.2–5.3: Replication-Deletion Method and Welch's Graphical Procedure. Retrieved 25 Jun 26, from https://rossetti.github.io/RossettiArenaBook/05-Chapter5.html

[41] Law, A.M. (2020). Statistical analysis of simulation output data: the practical state of the art. In *Proceedings of the 2020 Winter Simulation Conference* (pp. 1117–1127). INFORMS Simulation Society. Retrieved 25 Jun 26, from https://informs-sim.org/wsc20papers/134.pdf

[42] Gafarian, A. V., Ancker, C. J., & Morisaku, T. (1978). Evaluation of Commonly Used Rules for Detecting Steady State. *Naval Research Logistics Quarterly*, 25, 511–529.

[43] Karl, A., Eubank, R., Milovanovic, J., Reiser, M., & Young, D. (2014). Using RngStreams for parallel random number generation in C++ and R. *Computational Statistics*, 29(5), 1301–1320. Open-access preprint retrieved 26 Jun 26, from https://arxiv.org/abs/1403.7645

[44] R Core Team. (2024). *RNGstreams: L'Ecuyer's RngStreams for parallel random number generation*. R Documentation, parallel package. Retrieved 26 Jun 26, from https://stat.ethz.ch/R-manual/R-patched/library/parallel/html/RngStream.html

[45] Rossetti, M. D. (2023). *Simulation Modeling using the Kotlin Simulation Library (KSL)* (open-access, CC BY-NC-ND 4.0), §9.2 Variance Reduction Techniques. Retrieved 26 Jun 26, from https://rossetti.github.io/KSLBook/09-Chapter9.html

[46] Morris, M. D. (1991). Factorial sampling plans for preliminary computational experiments. *Technometrics*, *33*(2), 161–174. Retrieved 11 Jul 26, from https://www.stat.cmu.edu/technometrics/90-00/vol-33-02/v3302161.pdf

[47] Pujol, G., Iooss, B., Janon, A., Gilquin, L., Le Gratiet, L., Lemaitre, P., Marrel, A., Meynaoui, A., Nelson, B. L., Monod, H., Fruth, J., Ratto, M., Touati, T., & Weber, F. (2024). *sensitivity: Global Sensitivity Analysis of Model Outputs and Related Quantities*. R package version 1.30.1. Retrieved 25 Jun 26, from https://cran.r-project.org/package=sensitivity

[48] OpenMOLE Community. (2024). *Sensitivity Analysis: Morris Screening Method*. OpenMOLE Documentation. Retrieved 25 Jun 26, from https://openmole.org/Sensitivity.html

[49] Saltelli, A., Annoni, P., Azzini, I., Campolongo, F., Ratto, M., & Tarantola, S. (2010). Variance based sensitivity analysis of model output. Design and estimator for the total sensitivity index. *Computer Physics Communications*, *181*(2), 259–270. Retrieved 11 Jul 26, from https://www.andreasaltelli.eu/file/repository/PUBLISHED_PAPER.pdf

[50] Williams, E., Szakmany, T., Spernaes, I., Muthuswamy, B., & Holborn, P. (2020). Discrete-event simulation modeling of critical care flow: New hospital, old challenges. *Critical Care Explorations*, *2*(9), e0174. Retrieved 11 Jul 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC7491890/

[51] Black, J. (2002). Acute appendicitis in Japanese soldiers in Burma: support for the "fibre" theory. *Gut*, *51*(2), 297. Retrieved 26 Jun 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC1773321/

[52] Weeks, S. R., Oh, J. S., Elster, E. A., & Learn, P. A. (2017). Humanitarian surgical care in the US military treatment facilities in Afghanistan from 2002 to 2013. *JAMA Surgery*, *153*(1), 84–86. Retrieved 26 Jun 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC5833623/

[53] Blaker, P. (1982, October 18). Falkland Islands (Casualties). *Parliamentary Debates, Commons*, written answers. Retrieved 02 Aug 26, from https://api.parliament.uk/historic-hansard/written-answers/1982/oct/18/falkland-islands-casualties

[54] Silkin, J. (1982, December 21). Falklands Campaign. *Parliamentary Debates, Commons*. Retrieved 02 Aug 26, from https://api.parliament.uk/historic-hansard/commons/1982/dec/21/falklands-campaign

[55] U.S. Army Medical Department Center and School. *Health Service Support in a Theater of Operations*, Subcourse MD0002, Evacuation Policy. Retrieved 02 Aug 26, from http://armymedical.tpub.com/MD0002/Evacuation-Policy-Health-Service-Support-in-a-Theater-of-Operations-88.htm

[56] Inwald, D. P., Arul, G. S., Montgomery, M., Henning, J., McNicholas, J., & Bree, S. (2013). Management of children in the deployed intensive care unit at Camp Bastion, Afghanistan. *Journal of the Royal Army Medical Corps*, *160*(3), 236–240. Retrieved 02 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC4154587/

[57] Royal Australian Air Force. *Aeromedical evacuation*. Australian Government, Department of Defence. Retrieved 02 Aug 26, from https://www.airforce.gov.au/our-work/humanitarian-support/aeromedical-evacuation

[58] U.S. Air Force. *C-130 Hercules*. Air Force Fact Sheet. Retrieved 02 Aug 26, from https://www.af.mil/About-Us/Fact-Sheets/Display/Article/1555054/c-130-hercules

[59] Yang, Q., Du, J. L., & Shao, F. (2019). Mortality rate and other clinical features observed in open vs closed format intensive care units: A systematic review and meta-analysis. *Medicine*, *98*(27), e16261. Retrieved 03 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC6635169/

[60] Godat, L., Kobayashi, L., Costantini, T., & Coimbra, R. (2013). Abdominal damage control surgery and reconstruction: world society of emergency surgery position paper. *World Journal of Emergency Surgery*, *8*, 53. Retrieved 03 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC3878509/

[61] Harvin, J. A., Kao, L. S., Liang, M. K., Adams, S. D., McNutt, M. K., Love, J. D., Moore, L. J., Wade, C. E., Tyson, J. E., & Holcomb, J. B. (2019). Better understanding the utilization of damage control laparotomy: a multi-institutional quality improvement project. *Journal of Trauma and Acute Care Surgery*, *87*(1), 27-34. Retrieved 03 Aug 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC6771434/

[62] Pawlowsky-Glahn, V., Egozcue, J. J., & Tolosana-Delgado, R. (2007). *Lecture notes on compositional data analysis*. Universitat de Girona. Retrieved 07 Aug 26, from https://dugi-doc.udg.edu/bitstream/handle/10256/297/CoDa-book.pdf

[63] Law, A. M. (2020). Statistical analysis of simulation output data: the practical state of the art. In *Proceedings of the 2020 Winter Simulation Conference* (pp. 1117-1127). INFORMS Simulation Society. Retrieved 08 Aug 26, from https://informs-sim.org/wsc20papers/134.pdf

[64] Hall, A., Graham, B., Hanson, M., & Stern, C. (2023). Surgical capability utilization time for military casualties at role 2 and role 3 facilities. *Military Medicine*, *188*(11-12), e3368-e3370. Retrieved 10 Aug 26, from https://academic.oup.com/milmed/article/188/11-12/e3368/6961509

[65] Lewis, P. A. W., & Shedler, G. S. (1979). Simulation of nonhomogeneous Poisson processes by thinning. *Naval Research Logistics Quarterly*, *26*(3), 403-413. Naval Postgraduate School Calhoun repository. Retrieved 13 Aug 26, from https://calhoun.nps.edu/handle/10945/63159

<!-- REFERENCES END -->
