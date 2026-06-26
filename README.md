# Battlefield Casualty Handling

## Abstract

This study presents a Discrete Event Simulation (DES) framework developed to evaluate resource utilization and casualty processing within a deployed battlefield medical system under Large Scale Combat Operations (LSCO). Leveraging parameterized inputs published in open-access literature, the simulation models per-minute casualty arrivals, triage, and surgical throughput across Role 1 (R1), Role 2 Basic (R2B), and Role 2 Enhanced – Heavy (R2E Heavy) treatment nodes.

The model enables extended-duration analysis at varying scales, capturing key metrics such as queue lengths, wait times, and resource saturation to identify temporal bottlenecks and assess system suitability. A three-stage damage control surgery model is embedded within the simulation to reflect treatment pathways and operational constraints.

Findings demonstrate that the current system design is capable of managing moderate casualty volumes, historically represented by the Falklands conflict. However, the simulation reveals critical vulnerabilities in surgical capacity and evacuation throughput that would likely be exacerbated under LSCO conditions with greater casualty rates. The study identifies areas of risk, recommends targeted system refinements, and proposes enhancements to the simulation architecture—including dynamic DOW modelling, pulsed strategic evacuation, and stochastic mass-casualty events.

This tool supports iterative refinement and stakeholder engagement, offering a transparent, modular platform for testing health system resilience. With further development and testing against high-intensity casualty models, the simulation can inform doctrinal updates and guide medical planning for future operational environments.

## Contents

<small>[Return to Top](#contents)</small>

<!-- TOC START -->
- [Abstract](#abstract)
- [Contents](#contents)
- [📘 Introduction](#-introduction)
- [📚 Literature Review](#-literature-review)
  - [Methodology](#methodology)
  - [Findings](#findings)
    - [Battlefield Casualty Rates and Estimation Models](#battlefield-casualty-rates-and-estimation-models)
    - [Casualty Simulation and DES](#casualty-simulation-and-des)
    - [Statistical Distributions and Modelling Algorithms](#statistical-distributions-and-modelling-algorithms)
    - [Military Doctrine and Operational Health Support Policy](#military-doctrine-and-operational-health-support-policy)
- [🌍 Scenario Context](#-scenario-context)
- [🧰 Resource Descriptions](#-resource-descriptions)
  - [🏥Health Teams](#health-teams)
    - [Role 1 (R1) Treatment Team](#role-1-r1-treatment-team)
    - [Role 2 Basic (R2B)](#role-2-basic-r2b)
    - [Role 2 Enhanced Heavy (R2E Heavy)](#role-2-enhanced-heavy-r2e-heavy)
  - [🛏️ Bed Types](#-bed-types)
    - [Operating Theatre (OT)](#operating-theatre-ot)
    - [Resuscitation (Resus) (alternatively Emergency)](#resuscitation-resus-alternatively-emergency)
    - [Intensive Care Unit (ICU)](#intensive-care-unit-icu)
    - [Holding (Hold)](#holding-hold)
  - [🚑 Transport Assets](#-transport-assets)
    - [Protected Mobility Vehicle Ambulance (PMV Ambulance)](#protected-mobility-vehicle-ambulance-pmv-ambulance)
    - [HX2 40M](#hx2-40m)
- [📊 Environment Data Summary](#-environment-data-summary)
  - [👥 Population Groups](#-population-groups)
  - [🚑 Transport Resources](#-transport-resources)
  - [🏥 Medical Resources](#-medical-resources)
  - [Schedules and Rosters](#schedules-and-rosters)
- [🤕 Casualties](#-casualties)
  - [Casualty Generation](#casualty-generation)
    - [1. Lognormal Parameterisation](#1-lognormal-parameterisation)
    - [2. Per-Minute Rate Sampling and Scaling](#2-perminute-rate-sampling-and-scaling)
    - [3. Arrival Detection via Cumulative Sum](#3-arrival-detection-via-cumulative-sum)
    - [4. Temporal Randomisation](#4-temporal-randomisation)
  - [Wounded In Action (WIA)](#wounded-in-action-wia)
    - [Combat Casualties](#combat-casualties)
    - [Support Casualties](#support-casualties)
  - [Killed In Action (KIA)](#killed-in-action-kia)
    - [Combat Casualties](#combat-casualties)
    - [Support Casualties](#support-casualties)
  - [Disease and Non-Battle Injury (DNBI)](#disease-and-nonbattle-injury-dnbi)
    - [Combat Casualties](#combat-casualties)
    - [Support Casualties](#support-casualties)
    - [DNBI Sub-Categorisation](#dnbi-subcategorisation)
- [Casualty Priorities](#casualty-priorities)
- [Return to Duty](#return-to-duty)
- [Died of Wounds](#died-of-wounds)
- [Development Environment](#development-environment)
- [Simulation Design](#simulation-design)
  - [Codebase Structure](#codebase-structure)
  - [Warm-up Period Analysis](#warm-up-period-analysis)
  - [Sensitivity Analysis](#sensitivity-analysis)
  - [🔧Simulation Environment Setup](#simulation-environment-setup)
  - [Core Trajectory](#core-trajectory)
  - [R2B Trajectory](#r2b-trajectory)
  - [R2E Heavy Trajectory](#r2e-heavy-trajectory)
- [Model Outputs](#model-outputs)
  - [Domain 1 — Mortality and Preventable Death](#domain-1--mortality-and-preventable-death)
  - [Domain 2 — Time-to-Care from R1 Arrival](#domain-2--time-to-care-from-r1-arrival)
  - [Domain 3 — Surgical Throughput](#domain-3--surgical-throughput)
  - [Domain 4 — Echelon Load and Capacity](#domain-4--echelon-load-and-capacity)
  - [Domain 5 — Flow and Disposition](#domain-5--flow-and-disposition)
  - [Domain 6 — Combat Power](#domain-6--combat-power)
  - [Output Variable Register cross-reference](#output-variable-register-cross-reference)
- [Simulation Analysis](#simulation-analysis)
  - [Simulation Casualty Generation](#simulation-casualty-generation)
  - [R1 Handling](#r1-handling)
  - [R2B Handling](#r2b-handling)
  - [R2E Heavy Handling](#r2e-heavy-handling)
  - [Conclusion](#conclusion)
- [Limitations](#limitations)
- [Further Development](#further-development)
- [Conclusion](#conclusion)
- [References](#references)
  <!-- TOC END -->

---

## 📘 Introduction

<small>[Return to Top](#contents)</small>

Large‑scale combat operations (LSCO) represent the most demanding form of conventional warfare, characterised by high‑tempo, multi‑domain action against peer or near‑peer adversaries. Defined in contemporary doctrine such as extensive joint combat operations conducted at division (and above) scale to achieve strategic objectives, LSCO require the orchestration of manoeuvre, fires, logistics, intelligence, and command across vast, often nonlinear battlespaces [[1]](#References), [[2]](#References). Unlike the western experience in Middle-East conflicts, forces in LSCO will not have guaranteed air superiority, uncontested lines of communication, and predictable casualty flows. LSCO is expected to unfold in contested, degraded, and dynamic operational conditions [[1]](#References), [[2]](#References), [[3]](#References). Historical and contemporary case studies - from the industrial battlefields of the Second World War to recent fighting in Ukraine - demonstrate that such operations generate high casualty densities, impose unprecedented logistical demands, and challenge even the most sophisticated forces’ ability to sustain tempo over time [[2]](#References), [[4]](#References).

The medical implications of LSCO are profound. Estimates consistently project casualty volumes in the order of 50,000 to 60,000 per 100,000 personnel over a matter of weeks, with thousands of those potentially able to return to duty if treated effectively and without delay [[5]](#References). The deployed health system — encompassing point‑of‑injury care, medical evacuation, forward surgical capabilities, hospitalisation, force health protection, and medical command and control — is therefore not a peripheral service but a critical combat enabler. Its capacity to preserve fighting strength underpins the force’s ability to seize, retain, and exploit the initiative. In LSCO, battlefield clearance must be achieved despite contested airspace and disrupted communications, injury patterns will reflect the lethality of modern munitions, and medical logistics must adapt to disrupted supply chains and fluid front lines.

Yet, existing medical doctrine and force design have evolved largely from counterinsurgency campaigns where operational conditions were comparatively permissive [[5]](#References). In a peer‑conflict LSCO scenario, planners must expect prolonged field care, delays in evacuation, and the need for smaller, more mobile surgical teams positioned closer to the fight [[3]](#References), [[5]](#References). A resilient and agile, deployed health system serves not only to save lives, but to sustain operational momentum and, ultimately, to enable the successful prosecution of the campaign.

This thesis looks to explore the performance of the deployed health system within the through simulation with an eye to understanding its implications for participation in LSCO. By modelling casualty flows, evacuation timelines, treatment capacities, and return‑to‑duty rates, the study seeks to identify key vulnerabilities, and resource requirements. The simulation approach allows for the exploration of scenarios that are difficult to replicate in live exercises, offering evidence‑based insights to refine doctrine, optimise medical force posture, and ensure that health support is adequate for LSCO. In doing so, it contributes to the broader imperative of preparing the force for the realities of high‑intensity warfare in an era of renewed great‑power competition [[6]](#References), [[7]](#References).

## 📚 Literature Review

### Methodology

To inform the design and implementation of the battlefield casualty simulation, a structured literature review was conducted using a multi-pronged methodology. The initial phase involved surveying open-access academic literature and publicly available internet-based resources related to battlefield casualty modelling, discrete event simulation (DES), and casualty rate estimation. This was complemented by a snowballing technique, where references cited within key papers were recursively explored to identify additional relevant sources.

To further expand the scope and ensure coverage of emerging and less-indexed materials, large language models (LLMs) were engaged iteratively to identify supplementary resources. These included algorithmic approaches to casualty distribution modelling, comparative analyses of simulation paradigms, and operational insights into trauma system design under combat conditions. The use of LLMs enabled the discovery of niche publications, technical reports, and grey literature that may not have surfaced through conventional search techniques.

This academic and technical review was supplemented by a survey of publicly available military publications, including field manuals, operational health service support doctrine, and policy documents from allied defence organisations. These sources provided critical context for force structure, casualty flow assumptions, and the operational constraints that shape real-world medical planning.

### Findings

In total, 26 resources were reviewed to develop the simulation framework. These spanned peer-reviewed journals, technical reports, doctrinal publications, and internet publications. The review focused on four thematic domains:

#### Battlefield Casualty Rates and Estimation Models

Historical and predictive models of casualty rates were central to the review. The FORECAS system [[8]](#References) provided a statistically grounded approach to projecting wounded-in-action (WIA), killed-in-action (KIA), and disease/non-battle injury (DNBI) rates using empirical data from past conflicts. Complementary studies [[9]](#References), [[10]](#References), [[11]](#References) and [[12]](#References) highlighted the operational implications of casualty rates in LSCO, emphasizing the disproportionate impact of DNBI on lost duty days and the need for robust force health protection (FHP) strategies.

#### Casualty Simulation and DES

Simulation methodologies were explored through both military-specific and general DES literature. The simmer package for R [[13]](#References) was identified as a suitable framework for implementing modular, auditable, and event-driven logic. Studies such as [[14]](#References) demonstrated DES applications in healthcare contexts, while [[15]](#References) and [[4]](#References) provided high-level casualty rate numbers that allowed the evaluation of the performance of DES models and [[2]](#References) provided insights into trauma system design under combat conditions.
These sources informed the architectural decisions for the simulation engine, including event scheduling, resource constraints, and patient flow logic.

#### Statistical Distributions and Modelling Algorithms

The review examined statistical distributions suitable for modelling casualty arrival rates and treatment durations. FORECAS [[8]](#References) employed lognormal and exponential distributions based on battle intensity and troop type, validated through historical data. Additional studies [[16]](#References), [[11]](#References) and [[14]](#References) provided statistics, explored distribution models measures, and described other approaches using DES in medical care contexts.
These findings guided the selection of distribution functions for stochastic modelling, ensuring that simulated outputs reflect doctrinally plausible variability and temporal clustering.

#### Military Doctrine and Operational Health Support Policy

Doctrinal and policy publications such as [[1]](#References), [[3]](#References), [[6]](#References) and [[7]](#References) established the current geostrategic context and outlined the imperitive for militaries to be able to provide force options for LSCO. 
Recent doctrinal analyses [[9]](#References), [[2]](#References) and [[5]](#References) emphasized the shift toward prolonged casualty care, contested evacuation, and the need for distributed medical assets. These insights were incorporated into the simulation design to ensure alignment with contemporary operational realities.

This literature review enabled the design of a model suitable to support the assessment of the deployed health system capacity within LSCO.

## 🌍 Scenario Context

<small>[Return to Top](#contents)</small>

The simulation scenario models a deployed combat brigade's assigned health support capabilities. The design reflects the doctrinal organisation of deployed elements adapted to the operational requirements of LSCO. The simulated brigade is composed of three battlegroups, each commanded by one of the resident combat manoeuvre units. To ensure close health support at the point of need, each battlegroup is allocated `1` R1 treatment teams tasked with providing immediate clinical intervention and stabilisation in proximity to combat operations (for a total of `3` R1 treatment teams). These forward elements represent the first echelon of the deployed health system, capable of delivering life‑saving care under combat conditions.

In the second medical echelon, `2` Role 2 - Basic (R2B) facilities are established. Their function is to conduct damage control resuscitation and surgery, stabilising casualties before onward evacuation to higher‑level medical treatment facilities. Positioned to balance proximity to the fight with survivability, the R2B facilities serve as critical nodes in the casualty evacuation chain, extending the reach of forward treatment teams.

To provide robust surgical capability and greater depth of care, `1` Role 2 – Enhanced Heavy (R2E Heavy) hospital is sited to the rear. This facility is equipped to conduct complex surgical procedures, extended post‑operative care, and advanced diagnostic support. Its location optimises both the survivability of the asset and its accessibility to casualties moved from forward positions, thereby anchoring the brigade’s deployed medical architecture.

The organisational structure described above underpins the simulation’s logic for casualty generation, treatment timelines, and evacuation flows. A representative diagram illustrates the spatial and functional relationships between the brigade’s manoeuvre elements and its health support system, serving as the baseline model for subsequent scenario analysis.

![Alt text](images/tactical_diagram.png)

An [Interactive Diagram](https://www.map.army/?ShareID=1041883&UserType=RO-xOMjf7j6) allows further exploration of the model being simulated.

---

## 🧰 Resource Descriptions

<small>[Return to Top](#contents)</small>

### 🏥Health Teams

#### Role 1 (R1) Treatment Team

A role 1 treatment team provides the first line of medical care. It is designed to deliver immediate lifesaving measures, perform triage and stabilization, and manage minor injuries and illnesses close to the point of injury or unit location. These teams also prepare casualties for evacuation to higher levels of care if needed.

#### Role 2 Basic (R2B)

A Role 2 Basic (R2B) medical treatment facility provides forward surgical and resuscitative care close to the battlefield. Its purpose is to deliver damage-control surgery, critical care, and short-term patient holding in austere environments where rapid intervention can save lives.

It’s designed to be mobile, logistically lean, and capable of stabilizing casualties before evacuation to higher-level care. With ICU beds, low-dependency holding, and a surgical team, R2B bridges the gap between frontline treatment and more comprehensive facilities like Role 2 Enhanced or Role 3.

#### Role 2 Enhanced Heavy (R2E Heavy)

A R2E Heavy facility delivers advanced surgical and critical care capabilities in forward-deployed military operations. Its purpose is to provide damage-control surgery, intensive care, inpatient services, and scalable resuscitation for casualties who require more than basic stabilization but are not yet ready for strategic evacuation.

The R2E Heavy is a static field hospital with designed to handle complex trauma, prolonged care, and high casualty volumes—bridging the gap between battlefield stabilization and full hospital-level treatment.

### 🛏️ Bed Types

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

ICU beds are equipped with portable monitors, ventilators, infusion pumps, and access to diagnostics like labs and imaging.

#### Holding (Hold)

Holding beds are designated for short-term patient care and observation, typically for those who are awaiting evacuation, recovering from minor procedures, or expected to return to duty soon. These beds serve as a transitional space between acute treatment zones (like ICU or OT) and final disposition—whether that’s evacuation to higher care or reintegration into the force.

They’re often used for:

- Postoperative recovery after damage-control surgery.
- Monitoring stable patients who don’t require intensive care.
- Staging casualties for medical evacuation.
- Low-dependency care such as hydration, pain management, or wound dressing.

Holding beds help to maintain patient flow and prevent bottlenecks in critical care areas.

### 🚑 Transport Assets

#### Protected Mobility Vehicle Ambulance (PMV Ambulance)

The PMV Ambulance (Protected Mobility Vehicle – Ambulance) is a blast-resistant, armored medical transport designed to safely evacuate casualties from combat zones. Based on the Bushmaster, it combines mobility, protection, and medical capability, allowing medics to deliver care en route while shielding patients from small arms fire, IEDs, and mines.

#### HX2 40M

The HX2 40M is a 4×4 tactical military truck developed by Rheinmetall MAN Military Vehicles (RMMV) as part of the HX2 series. Designed for high mobility and rugged performance, it serves as a versatile logistics platform for transporting troops, equipment, and supplies in demanding operational environments. In this simulation the HX2 40M is used for the transport of KIA and casualties that have DOW.

---

## 📊 Environment Data Summary

<small>[Return to Top](#contents)</small>

<!-- ENV SUMMARY START -->
<!-- This section is auto-generated. Do not edit manually. -->

### 👥 Population Groups

The following population groups are defined in the simulation environment:

| Population | Count |
|------------|-------|
| Combat | 2500 |
| Support | 1250 |

### 🚑 Transport Resources

These are the available transport platforms and their characteristics:

| Platform | Quantity | Capacity |
|----------|----------|----------|
| PMVAMB | 3 | 4 |
| HX240M | 4 | 50 |

### 🏥 Medical Resources

The following table summarises the medical elements configured in `env_data.json`, including team types, personnel, and beds:

| Element | Quantity | Beds | 1 | Surg | Emerg | Icu | Evac |
| --- | --- | --- | --- | --- | --- | --- | --- |
| R1 | 3 | NA | Medic (3), Nurse (1), Doctor (1) | NA | NA | NA | NA |
| R2B | 2 | OT (1); Resus (2); ICU (2); Hold (5) | NA | Anesthetist (1), Surgeon (2), Medic (1) | Facem (1), Nurse (3), Medic (1) | Nurse (2), Medic (2) | Medic (2) |
| R2EHEAVY | 1 | OT (2); Resus (4); ICU (4); Hold (30) | NA | Anesthetist (1), Surgeon (2), Nurse (4) | Facem (1), Nurse (3), Medic (1) | Intensivist (1), Nurse (4) | Medic (2) |

<!-- ENV SUMMARY END -->

### Schedules and Rosters

Some resource teams have rosters/schedules. Due to the limited size and structure, surgical teams are rostered as available for12 hour shifts. This results in there being 12 hours of time available for surgery at the R2B in every 24 hours and 36 hours of surgery time available in every 24 hours across two OT in the R2E Heavy. 

---

## 🤕 Casualties

<small>[Return to Top](#contents)</small>

Casualties are generated based on rates outlined in [[8]](#References) and refined with analysis provided in [[10]](#References) and supported by [[15]](#References), with the implementation outlined below.

Initially, WIA and KIA rates from US historical analysis of the Battle of Okinawa were used [[8]](#References), producing approximately 30 casualties per day for a force size of 3,750, yielding a casualty rate of ~0.8%. By comparison, Russia’s estimated 700-1,100 daily casualties from a committed force of 450,000–600,000 in Ukraine imply a lower casualty rate of ~0.2% [[4]](#References).

Given this discrepancy, a planning baseline was re-evaluated using historical data from the Falklands War, which suggests a casualty rate of ~0.37% [[8]](#References). This adjustment accounts for both the likely under-reporting in Russian casualty estimates, particularly of non-critical wounded personnel and over three years of varied combat intensity in Ukraine, with seasonal fluctuations in operational tempo (source TBD).

Based on this reasoning, a daily casualty rate of ~0.37% is considered a suitable estimate for operational planning.

### Casualty Generation

For simulation efficiency, arrival times for cases were pre-computed and then introduced deterministically to the simulation environment for processing. The function simulates the timing of casualty arrivals using a lognormal distribution to reflect daily variability, transformed into randomized, minute-level arrival times. Rather than sampling explicit arrival times, the function models continuous per-minute intensity and converts this to discrete arrival events using cumulative thresholds. The general process is outlined below.

#### 1. Lognormal Parameterisation

Converts daily mean and standard deviation into log-space parameters, preserving the shape of the empirical distribution.

Mean (log-space):

$$
\mu_{\log} = \ln\left(\frac{\mu^2}{\sqrt{\sigma^2 + \mu^2}}\right)
$$

Standard deviation (log-space):

$$
\sigma_{\log} = \sqrt{\ln\left(1 + \frac{\sigma^2}{\mu^2}\right)}
$$

Where:

- \mu = expected number of DNBI casualties per day
- \sigma = daily standard deviation

#### 2. Per-Minute Rate Sampling and Scaling

Draws lognormally distributed samples representing per-minute DNBI rates, capped at a specified threshold to prevent extreme outliers. The sample is scaled according to population size and temporal resolution (per minute per 1000 personnel).

For each simulation minute $i \in \{1, 2, \dots, n_{\text{minutes}}\}$, the per-minute DNBI rate is computed as:

$$
r_i = \min\left(x_i, \text{cap}\right) \times \frac{P}{1000 \times 1440}
$$

Where:

- $x_i \sim \text{LogNormal}(\mu_{\log}, \sigma_{\log}^2)$
- $\mu_{\log} = \ln\left(\frac{\mu^2}{\sqrt{\sigma^2 + \mu^2}}\right)$
- $\sigma_{\log} = \sqrt{\ln\left(1 + \frac{\sigma^2}{\mu^2}\right)}$
- $\mu, \sigma$ = daily mean and standard deviation
- $\text{cap}$ = upper bound (e.g., 5) to prevent extreme values
- $P$ = population size (support or combat)
- $r_i$ = scaled and capped casualty rate for minute i

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

Introduces sub-minute jitter to avoid clustering arrivals on discrete time ticks and returns a sorted list of event timestamps.

### Wounded In Action (WIA)

#### Combat Casualties

Combat WIA casualty generation has been based on Falklands combat troop WIA rates ([[a8]](#References), table A.8 p32).

$$
\mu = 1.77, \quad \sigma = 3.56
$$

#### Support Casualties

Support casualties employ the same casualty generation outlined above (except using the support population estimate of 1250 instead of the combatt population of 2250). This is on the basis that most historical modelling of force casualties include support elements at or below division in division and below casualty estimation due to their integral nature to combat operations and close proximity to the Forward Edge of the Battle Area (FEBA) (see [[15]](#References) and [[10]](#References) p 2-4).

### Killed In Action (KIA)

#### Combat Casualties

Combat WIA casualty generation has been based on Falklands combat troop WIA rates ([[8]](#References), table A.8 p32).

$$
\mu = 0.68, \quad \sigma = 1.39
$$

#### Support Casualties

Similar to WIA, support casualty KIA employ the same casualty generation outlined above (except using the support population estimate of 1250 instead of the combatt population of 2250) (see [[15]](#References) and [[10]](#References) p 2-4).

### Disease and Non-Battle Injury (DNBI)

#### Combat Casualties

Combat DNBI casualty generation has been based on Vietnam combat troop DNBI rates ([[8]](#References), table A.5 p31).

$$
\mu = 2.04, \quad \sigma = 1.89
$$

#### Support Casualties

Support DNBI casualty generation has been based on Okinawa support troop DNBI rates ([[8]](#References), table A.2 p29).

$$
\mu = 0.94, \sigma = 0.56
$$

#### DNBI Sub-Categorisation

DNBI cases were further sub-categorised as either NBI or disease/battle fatigue with 17% of DNBI cases being allocated as NBI and the remainder disease or battle fatigue (per [[8]](#References), pp 22-23).

## Casualty Priorities

<small>[Return to Top](#contents)</small>

The following casualty priority rates were used with the rates requiring surgery:

- **Priority 1**. 65% of casualties with 90% requiring surgery.

- **Priority 2**. 20% of casualties with 80% requiring surgery.

- **Priority 3**. 15% of casualties with:
  
  - 40% of DNBI requiring surgery.
  
  - 60% of other priority 3 casualties requiring surgery. 

> **MODEL ASSUMPTION — Casualty Priority Distribution:** The priority distribution (65% Priority 1, 20% Priority 2, 15% Priority 3) and associated surgical requirement rates (90%, 80%, and 40–60% respectively) are derived from ADF operational planning guidance that is not publicly accessible. NATO doctrine (AJP-4.10 [[21]](#References)) establishes the triage framework and echelon functions but does not prescribe specific priority distribution percentages for simulation planning.
> **Basis:** ADF internal operational planning norms; no open-access source of equivalent specificity exists. The values reflect planning assumptions for a brigade-level ADF deployment rather than empirical data from a named conflict.
> **Uncertainty:** High
> **Consequence if wrong:** If the true priority distribution differs materially from that assumed, surgical resource utilisation at R2B and R2E shifts correspondingly. Over-representation of P1 increases OT and ICU demand; over-representation of P3 increases R1 holding time and reduces forward surgical load.

## Return to Duty

<small>[Return to Top](#contents)</small>

- Priority 3 are returned to duty from R1. Priority 1 and 2 that do not require surgery are RTF post recovery from emergency treatment at r2b.

- Per [[9]](#References), of those admitted to MTFs, the distribution for return to duty was 42.1 percent in Republic of Vietnam, 7.6 percent in the U.S. Indo-Pacific Command, and 33.4 percent in the CONUS.

## Died of Wounds

<small>[Return to Top](#contents)</small>

5% of Priority 1 and 2.5% of P2 at R1, 1% of arrivals at R2B, 1% of arrivals ar R2E Heavy

---

## Development Environment

<small>[Return to Top](#contents)</small>

The simulation uses `parallel::mclapply` for multi-replication parallelism, which relies on `fork()` — a POSIX primitive unavailable on Windows. On a Windows RStudio installation, `mclapply` silently falls back to sequential execution, meaning the full Morris sensitivity screening (r = 20, reps = 5, n_sobol = 200, ~8 000 simulation runs) takes an estimated 10–15 hours rather than 1–2 hours on equivalent hardware.

A Dev Container specification in `.devcontainer/` defines a reproducible Linux R environment (R 4.4.2, all project packages) that can be launched from VS Code with a single command. It provides a Linux `fork()` context, RStudio Server on `http://localhost:8787`, and automatic core-count configuration — so contributors on any host OS get full parallelism and a consistent package environment without manual dependency resolution.

### Prerequisites

| Requirement | Notes |
|---|---|
| [Docker Desktop](https://www.docker.com/products/docker-desktop/) | Provides the container runtime. Enable "Use the WSL 2 based engine" on Windows. |
| [VS Code](https://code.visualstudio.com/) | Host IDE used to manage the container lifecycle. |
| [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) | VS Code extension (`ms-vscode-remote.remote-containers`) that adds the "Reopen in Container" command. |

### First-time setup

1. Clone the repository to the local machine:
   ```sh
   git clone https://github.com/natosys/Battlefield-Casualty-Handling.git
   cd Battlefield-Casualty-Handling
   ```
2. Open the repository folder in VS Code: **File → Open Folder**.
3. VS Code will detect `.devcontainer/devcontainer.json` and display a notification: *"Folder contains a Dev Container configuration file. Reopen folder to develop in a container."* Click **Reopen in Container**. Alternatively, open the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`) and select **Dev Containers: Reopen in Container**.
4. VS Code builds the Docker image and starts the container. The initial build downloads the base image and installs all R packages; this typically takes 5–10 minutes. Subsequent starts use the cached image and complete in seconds.
5. Once the container starts, open a browser and navigate to `http://localhost:8787` to access RStudio Server. No login credentials are required (`DISABLE_AUTH=true`).

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
Rscript run.R --seed NULL --days 30 --iterations 100

# Quick smoke test (5 days, 5 iterations)
Rscript run.R --quick
```

`mclapply` will use all physical cores reported by `parallel::detectCores(logical = FALSE)`, providing linear scaling up to the host core count.

### Git workflow

All files are bind-mounted from the host filesystem into `/home/rstudio/workspace` inside the container. Git commits and pushes can be made from either location — changes are immediately visible on both sides. SSH keys placed in the host `~/.ssh/` directory are available inside the container via the default Docker bind mount for SSH agent forwarding.

---

## Simulation Design

<small>[Return to Top](#contents)</small>

The simulation is built as a Discrete Event Simulation (DES), it is written in R  using the simmer package [[13]](#References). DES has been used as a proven way to simulate healthcare systems and support healthcare decision-making (as shown in [[14]](#References)).

### Codebase Structure

The codebase is organised into a modular layout under an `R/` directory, with a single CLI entry point (`run.R`). The split allows each module to be tested and extended independently, and provides a clear separation between data loading, simulation logic, execution, and analysis.

| File / Directory | Purpose |
|---|---|
| `run.R` | CLI entry point — parses arguments, orchestrates modules, and writes outputs |
| `R/environment.R` | Data import (`load_elms`, `build_environment`), arrival generation (`generate_ln_arrivals`), and simmer environment construction (`build_env`) |
| `R/trajectories.R` | All simmer `trajectory()` definitions — R1, R2B, R2E, and core casualty flow |
| `R/replication.R` | Multi-run replication framework — `run_once` (single replication with `wrap()`), `run_replications` (parallel `mclapply` over *n* replications), `summarise_replications` (time-weighted KPI summary with 95% CI), and `run_single` backwards-compat shim |
| `R/analysis.R` | Analysis and visualisation pipeline (`analyse_run`) — accepts monitoring data objects rather than reading from hardcoded CSV paths |
| `R/sensitivity.R` | Morris EE screening (`run_morris`) and Sobol variance decomposition (`run_sobol`) — parameter bounds table, `apply_params` for env_data override, `eval_params` for single design-point evaluation |
| `R/warmup.R` | Welch warm-up analysis — `compute_welch_cma`, `plot_welch`, `run_welch_analysis`; `WARM_UP_DAYS` constant |
| `scripts/run_sensitivity.R` | CLI entry point for sensitivity analysis — `--quick`, `--sobol`, `--r`, `--reps`, `--days`, `--n-sobol` flags |
| `scripts/run_warmup.R` | CLI entry point for Welch warm-up analysis |
| `data_import.R` | Compatibility shim — sources `R/environment.R` so existing code continues to work |
| `outputs/` | Generated outputs directory — CSVs and markdown tables are written here; tracked via `.gitkeep` |
| `data/` | Read-only input data — arrival schedules and environment data |
| `docs/` | Project documentation — action plans and role allocation tables |

#### Running the simulation

```sh
# Standard single run (seed 42, 30 days, 1 iteration)
Rscript run.R --seed 42 --days 30 --iterations 1

# Custom run
Rscript run.R --seed 99 --days 14 --iterations 10

# Quick smoke test (5 days, 5 iterations, seed 42)
Rscript run.R --quick
```

Single-run outputs (monitoring CSVs and markdown tables) are written to `outputs/`. Simulation logs are written to `logs/logs.txt`. Multi-run mode additionally writes `outputs/replication_summary.csv` containing the KPI table (see [Multi-run Replication Framework](#multi-run-replication-framework) below).

> **Note — dependency pinning:** `renv` lockfile initialisation is planned but not yet implemented. Package versions are therefore not pinned. This is a known limitation that will be addressed in a future issue.

#### Multi-run Replication Framework

The simulation supports independent Monte Carlo replication via `run_replications(n_iterations, n_days)` in `R/replication.R`. When `--iterations` is greater than 1, each replication:

1. Builds a fresh `simmer` environment from `env_data.json` with independent arrival streams drawn from the lognormal generator (`seed = NULL` per worker).
2. Runs to completion and snapshots monitoring state with `wrap(env)`, which captures arrivals, attributes, and resource utilisation without holding the live environment in memory.
3. Returns all replication data aggregated by `get_mon_arrivals(envs)` / `get_mon_resources(envs)` / `get_mon_attributes(envs)`, which append a `replication` index column (1…*n*) to each row.

On POSIX systems (Linux, macOS), replications are dispatched via `mclapply` with `mc.set.seed = TRUE` and `RNGkind("L'Ecuyer-CMRG")` set before the call. This assigns each worker a provably non-overlapping substream of the underlying MRG32k3a generator, with overall period ρ ≈ 2¹⁹¹ and substream spacing ρ₂ = 2⁷⁶ — stream overlap is impossible within any realistic simulation workload. Karl et al. (2014) [[32]](#References) demonstrate the practical application of this mechanism in R via the `parallel` and `rstream` packages. The physical core count is used via `mc.cores = parallel::detectCores()`. On Windows, the framework falls back to sequential `lapply`.

**Antithetic variate variance reduction** is applied to arrival generation. Replication pairs (2*k*−1, 2*k*) share a seed: both workers call `run_once()` with the same `seed` value, so their RNG streams start from an identical state. The primary replication (odd index) draws U ~ Uniform(0,1) and computes X = qlnorm(U); the antithetic replication (even index) substitutes U′ = 1 − U, computing X′ = qlnorm(1 − U). Because both use the *same* initial uniform sequence, the reflection is exact: Cor(X, X′) < 0 and the estimator variance Var[Ȳ] is reduced without increasing replication count (Rossetti, 2023 [[33]](#References)). Independence *across* pairs is ensured by distinct pair seeds drawn from the parent RNG. The within-minute arrival jitter is also antithetised. Antithetic application is limited to arrival times; service times and routing probabilities generated internally by simmer cannot be antithetised without deep trajectory instrumentation (see L8, Limitations).

A key-performance-indicator summary is computed by `summarise_replications(mon)` using the time-weighted mean queue per replication as the unit of analysis. The across-replication summary reports mean, p10, p90, max queue, and a 95% confidence interval (t-distribution, *df* = *n* − 1) for each resource, sorted descending by mean queue. Results are written to `outputs/replication_summary.csv`.

> **MODEL ASSUMPTION — L'Ecuyer-CMRG Non-overlapping RNG Streams:** Non-overlapping parallel RNG streams are guaranteed via `RNGkind("L'Ecuyer-CMRG")` with `mc.set.seed = TRUE` in `mclapply`. Each worker is assigned a distinct MRG32k3a substream with a period of 2⁷⁶ draws, making overlap negligible for any simulation budget used in this study. The R `parallel` package documentation confirms this assignment: "use a separate stream for each of the parallel computations (which ensures that the random numbers generated never get into sync)" [[41]](#References).
> **Basis:** Karl et al. (2014) [[32]](#References) demonstrates practical use of L'Ecuyer-CMRG via R's `parallel` package; R Core Team (2024) [[34]](#References) documents the `mc.set.seed = TRUE` mechanism and stream period properties.
> **Uncertainty:** Low (the non-overlap property is mathematically proven given the substream period).
> **Consequence if wrong:** Correlated replications would understate variance and produce CI bounds that are overly narrow; this risk is negligible given the substream design [[34]](#References).

#### Warm-up Period Analysis

Discrete event simulations are classified as either **terminating** or **steady-state** based on the nature of the system being modelled [[26]](#References). A terminating simulation has a natural, finite end state (e.g., an operational campaign concluding after a defined horizon); the run begins under well-defined initial conditions, and behaviour across the entire horizon — including the start-up period — is of direct interest. A steady-state simulation models a perpetual system in which the long-run equilibrium is the quantity of interest; here, the initialisation transient is an artefact that must be discarded before meaningful statistics can be collected. The choice of classification governs whether warm-up exclusion is appropriate.

Welch's graphical method [[27]](#References) was applied to characterise the simulation's time-varying behaviour and determine which classification applies. The method involves: (1) running ≥10 independent replications of an extended simulation (90 days); (2) computing the cross-replication cumulative moving average (CMA) of a sensitive KPI at each time point; and (3) determining whether the CMA converges to a stable level. The R2E ICU queue was selected as the KPI, being the most congestion-sensitive resource in the model.

The analysis is implemented in `R/warmup.R` and can be executed from the repository root:

```bash
# Full analysis: 10 reps × 90 days
Rscript scripts/run_warmup.R

# Reduced run for testing
Rscript scripts/run_warmup.R --reps 5 --days 60
```

The resulting Welch plot (`images/welch_plot_icu_queue.png`) shows the cross-replication CMA of the R2E ICU queue across 90 days. Rather than converging to a stable plateau, the CMA displays episodic, non-stationary behaviour: a rise to a local peak near Day 13 (the first wave of R2E ICU admissions propagating from early combat), a decline to a trough near Day 25, then a second rise to a higher peak near Day 38 as cumulative casualty load continues to build. No convergence to a steady state is observed within the 90-day horizon. This pattern is consistent with the lognormal arrival process generating episodic surges; the ICU queue is driven by campaign dynamics rather than a stationary queue process, and the CMA continues to shift across the full run length.

This non-convergent CMA confirms that the battlefield casualty handling simulation is a **terminating simulation** per Law (2020) [[26]](#References). The campaign has a defined finite horizon; the ICU queue trajectory represents the operational reality of that campaign, including the initial build-up of casualties from Day 1. The empty-start initial condition — no casualties in care on Day 0 — is the correct operational initial condition for a force beginning operations. It is not a modelling artefact to be excluded. Gafarian, Ancker and Morisaku (1978) [[28]](#References) establish that warm-up detection methods, including graphical approaches, presuppose the existence of a steady state; they are not applicable to terminating simulations.

Warm-up exclusion is therefore **not applied** as the default. The `WARM_UP_DAYS` constant in `R/warmup.R` is set to `0L`. All KPI summaries and analysis outputs use the full observation window.

The `--warm-up` CLI flag remains available for **parametric comparison runs** — sensitivity screening and scenario analysis — where a researcher wishes to study mid-campaign behaviour net of start-up effects, or where two scenarios differ in their initialisation characteristics and the comparison requires a common time base:

```bash
# Optional: exclude first 10 days for parametric comparison runs only
Rscript run.R --iterations 50 --days 60 --warm-up 10
```

> **MODEL ASSUMPTION — TERMINATING SIMULATION (NO WARM-UP EXCLUSION BY DEFAULT):** The simulation is classified as a terminating simulation. The empty-start initial condition is the correct operational initial condition; no warm-up exclusion is applied by default (`WARM_UP_DAYS = 0L`).
> **Basis:** Law (2020) [[26]](#References) distinguishes terminating simulations (finite-horizon, natural end state) from steady-state simulations (infinite-horizon, seeking long-run equilibrium). The battlefield casualty handling simulation models a finite operational campaign; the full run — including the initial build-up — represents the campaign truth. Welch's graphical method [[27]](#References) was applied across 10 × 90-day replications; the CMA of the R2E ICU queue exhibited episodic non-stationary behaviour (peaks at Days 13 and 38) with no convergence, confirming the absence of a steady state. Gafarian, Ancker and Morisaku (1978) [[28]](#References) establish that graphical warm-up detection methods presuppose a steady state; they are not applicable to terminating simulations.
> **Uncertainty:** Low — the terminating classification is an inherent property of the finite campaign model structure, not a parameter subject to calibration.
> **Consequence if wrong:** If the simulation were treated as steady-state and early data discarded, KPIs would represent mid-campaign equilibrium rather than the campaign-wide casualty burden from Day 1. For operational planning — which must account for casualty load from the onset of operations — this would understate total system demand and the severity of early-period queues.

#### Sensitivity Analysis

The triangular distribution parameters governing surgery duration, resuscitation time, transport delay, DOW probability, ICU length of stay, and OT availability carry significant epistemic uncertainty. The conclusion that the system "operates with little reserve" [[22]](#References) may shift materially under plausible parameter perturbations. Without sensitivity analysis, no parameter can be identified as rate-limiting versus incidental to the result, and findings cannot be used to prioritise capability investments.

**Morris Elementary Effects (EE) screening** [[30]](#References) was applied using R's `sensitivity` package [[31]](#References). Morris EE is a global, one-at-a-time (OAT) method that identifies the few influential parameters from a larger set at low computational cost — r × (p + 1) model evaluations, where r is the trajectory count and p is the number of parameters. It produces two statistics per parameter: µ\* (the mean absolute Elementary Effect, indicating overall influence) and σ (the standard deviation of Elementary Effects, indicating nonlinearity and interaction). Parameters with large µ\* and small σ have large, approximately linear effects; large µ\* and large σ indicate nonlinear or interaction-dominated effects.

Nine parameters were selected for screening, spanning the main uncertain inputs across all three echelons:

| Parameter | Variable | Baseline | Lower | Upper |
|-----------|----------|----------|-------|-------|
| Surgery duration (R2B and R2E) | `surg_mode` | 120 min | 90 | 150 |
| Long resuscitation duration | `long_resus_mode` | 45 min | 25 | 70 |
| P1 DOW probability at R1 | `pri1_dow` | 5% | 2% | 10% |
| R1→R2B transport time | `r1_transport` | 30 min | 15 | 45 |
| R2B→R2E transport time | `r2b_transport` | 30 min | 15 | 45 |
| Long ICU duration | `long_icu_mode` | 1440 min | 770 | 2160 |
| P1 surgery probability | `pri1_surg_prob` | 90% | 70% | 98% |
| In-theatre recovery rate | `in_theatre_rate` | 10% | 5% | 20% |
| OT shift duration | `ot_hours` | 12 hr | 8 | 16 |

Three primary KPI outputs were monitored across all replications at each design point: mean R2B OT queue, mean R2E OT queue (combined as system OT queue), and mean R2E ICU queue. Total DOW count is tracked as a secondary output. Parameters ranked by µ\* on the system OT queue identify the inputs most responsible for surgical bottleneck severity.

> **MODEL ASSUMPTION — SENSITIVITY PARAMETER BOUNDS:** The bounds in the Morris screening table are set to cover clinically plausible variation around the current baseline values, derived from expert judgement and the literature reviewed in the Simulation Design section.
> **Basis:** Lower and upper bounds were set to span approximately ±25–50% of the baseline value for duration parameters (surgery, resuscitation, transport, ICU) and the full clinically plausible range for probability parameters (DOW, surgery probability, in-theatre recovery). OT shift availability (8–16 hours) reflects the range from a single extended session to full 16-hour dual-shift coverage.
> **Uncertainty:** Medium — bounds represent informed clinical judgement rather than empirically derived uncertainty intervals. Wider bounds would increase µ\* values without changing parameter ranking if the model is monotone.
> **Consequence if wrong:** Narrow bounds will understate the influence of parameters whose true range is wider; wide bounds will conflate plausible and implausible parameter regions. Parameter ranking is sensitive to bound specification in nonlinear models.

The sensitivity analysis is implemented in `R/sensitivity.R` and executed via:

```bash
# Full Morris screening: r=20 trajectories × 10 (p+1) = 200 evaluations, 5 reps each
Rscript scripts/run_sensitivity.R

# Smoke test: r=3, reps=3, days=5 (completes in <5 minutes)
Rscript scripts/run_sensitivity.R --quick

# Morris then Sobol variance decomposition on top 5 parameters
Rscript scripts/run_sensitivity.R --sobol
```

Outputs are written to `outputs/morris_ranking.csv` (parameter ranking by µ\* for system OT queue) and per-KPI scatter plots to `images/morris_<kpi>.png`. When `--sobol` is specified, first-order (S1) and total-order (ST) indices for the top-ranked parameters are written to `outputs/sobol_<kpi>.csv`.

### 🔧Simulation Environment Setup

The simulation models casualty handling across echelons of care in a battlefield environment, structured around modular trajectories and dynamic resource availability. It operates within a discrete-event simulation framework using `simmer`, and is driven by probabilistic rates, conditional branching, and resource interactions across Role 1 (R1), Role 2 Basic (R2B), and Role 2 Enhanced Heavy (R2E) facilities.

The simulation was designed around the general functions of each role of health element as outlined in the diagram below. Where roles overlap they are able to provide the same functions to varying degree. The diagram below provides an outline of the role and function design applied for this simulation using the three-stage Damage Control Surgery (DCS) model of care described in [[17]](#References) and [[18]](#References).

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

The simulation heavily uses triangular distributions to model the duration of activities undertaken in the model (treatment, transport and other handling tasks). A triangular distributions was employed as they are generally used when the underlying distribution is unknown, but a minimal value, some maximal value, and a most likely value are available [[16]](#References). This approach is similar to other applications of DES in clinical settings, as shown in [[14]](#References). 

---

### Core Trajectory

The casualty processing trajectory at R1 care establishes a dynamic and doctrinally aligned framework for routing battlefield casualties based on classification—wounded in action (WIA), disease/non-battle injury (DNBI), or killed in action (KIA). Each casualty is initialized with key attributes: assignment to a R1 team (via random selection), triage priority for WIA/DNBI based on weighted probabilities (priority 1-3), NBI status flagged with a ``17%`` probability [[8]](#References), and a probabilistic determination of surgical need based on casualty type and severity. WIA and DNBI casualties are determined if they have died-of-wounds (DOW) with probabilistic allocation of DOW for Priority 1 and Priority 2 cases ``5%`` DOW chance for Priority 1 and ``2.5%`` for Priority 2 casualties (estimate based on reporting in [[12]](#References)). Those flagged as DOW are reclassified and routed through KIA processing. 

Survivors are dispositioned based on urgency: evacuation decisions for Priority 1 and Priority 2 cases result in approximately ``95%`` of Priority 1 and ``90%`` of Priority 2 casualties advancing (based on estimates of casualty surgical requirement) to R2B, or bypassing to R2E if R2B teams are unavailable. Lower-priority or DNBI casualties not meeting evacuation criteria are retained for local recovery at the R1, with a recovery duration modeled using triangular distribution with ``min = 0.5``, ``max = 5``, and ``mode = 2`` (days), based on field estimates of minor injury convalescence. WIA and DNBI casualties receiving immediate treatment at R1 are assigned a treatment duration drawn from a triangular distribution with ``min = 10``, ``max = 30``, and ``mode = 20`` (minutes) [[23]](#References). KIA casualties bypass clinical treatment and are processed and transported, each having a processing duration with a triangular distribution: ``min = 15``, ``max = 45``, and ``mode = 30`` (minutes).

```mermaid
flowchart TD
    A(["Start"]) --> B["Set Attributes: <br> priority, nbi, surgery (statistically assigned)"]
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

Casualties routed to the R2B undergo surgical care, stabilization, and further dispositioning. Casualties are initially assigned a holding bed, until they can be transferred to a resuscitation bed.

Similar to the R1, where DOW occurs (~1% of arrivals at the R2B, again based on [[12]](#References)), casualties undergo a short treatment and are transported to the mortuary. Treatment and transport both use a duration with a triangular distribution: `min = 15`, `max = 45`, and `mode = 30` (minutes).

Resuscitation is modeled using a triangular distribution with ``min = 25``, ``max = 70``, and ``mode = 45`` (min). This distribution was developed based on estimates as there were no clear durations that could be identified in literature for the duration to be used for the resuscitation/emergency phase of treatment in R2 facilities. Instead, the likely/anticipated tasks required to be undertaken in this phase were collated with task duration estimates collated to produce estimates for use in the simulation (demonstrated in the table below). The durations were developed recognising the need for all activities to be completed within 90 min as indicated by [[19]](#References).

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

Once resuscitation/emergency treatment has been completed, casualties not requiring surgery are transferred to a holding bed for recovery. Recovery follows a triangular distribution with ``min = 0.5``, ``max = 10``, and ``mode = 5`` (days).

Next, surgical candidacy is assessed based on operating theatre (OT) bed availability. If capacity permits, patients requiring surgery are transferred to an operating theatre for damage control (DAMCON) surgery. The DAMCON surgery treatment duration is modeled using a triangular distribution with ``min = 41``, ``max = 210``, and ``mode = 95`` (minutes). Due to the variability of potential requirements for surgery it was difficult to identify reliable durations for surgery time. This distribution was developed based on the interpretation of several meta studies ([[17]](#References), [[24]](#References)). Where there is not OT capacity, casualties are evacuated to the R2E for handling. 

Casualties requiring further care (surgery following the DCS model described in [[17]](#References) and [[18]](#References)) are evacuated to the R2E. The duration for evacuation to the R2E follows a triangular distribution with ``min = 15``, ``max = 45``, and ``mode = 30``. Where evacuation resources are not available, the patient is transferred to the ICU until evacuation resources are available to facilitate transfer.

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
    J -- Yes --> K{"OT Ready?"}
    K -- Available --> L["Seize OT"]
    L --> M["Surgery"]
    M --> N["Release Resources"]
    K -- Not Available --> O{"Evac Ready?"}
    J -- No --> P["Seize Hold Bed"]
    P --> Q["Recover at R2B"]
    Q --> R["Release Hold Bed"]
    R --> S["Return to Duty"]
    S --> Z
    N --> O
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

The R2E facility serves as a critical node for advanced casualty management, including resuscitation, surgery, intensive care, holding and pathways to strategic evacuation. 

Upon arrival, casualties are triaged, those identified as DOW (~1%, based on [[12]](#References)) are transferred for mortuary handling. Surviving casualties are allocated to an initial holding bed until a resuscitation bay is available, where they undergo a resuscitation phase. Where previous resuscitation has not been completed (at the R2B) a long duration resuscitation is completed, otherwise a short resuscitation is completed. The R2E long duration resuscitation follows the triangular distribution estimated for R2B resuscitations (`min = 25`, `max = 70`, and `mode = 45` (min)). The short duration resuscitation is modelled based on task estimate durations. These times are outlined in the table below. The duration uses a triangular distribution with ``min = 13``, ``max = 55``, and ``mode = 28``.

| Short Reuscitation       |           |            |           |
| ------------------------ | --------- | ---------- | --------- |
| Step                     | Min (min) | Mode (min) | Max (min) |
| Hemorrhage Control       | 2         | 5          | 10        |
| IV/IO Access             | 2         | 5          | 10        |
| Fluid Resuscitation      | 5         | 10         | 20        |
| TBI Monitoring & Warming | 2         | 5          | 10        |
| Documentation/Prep       | 2         | 3          | 5         |
| **TOTAL**                | 13        | 28         | 55        |

On completion of resuscitation, surgical candidacy is then assessed: if the casualty is flagged for damage control surgery and operating theatre resources are available, procedures follow the same triangular distribution for DAMCON surgeries at the R2B (``min = 41``, ``max = 210``, and ``mode = 95``), derived from meta-analyses and other academic studies ([[17]](#References), [[24]](#References)). 

Post-operative care involves admission to the ICU, where durations vary by surgical phase: the first ICU period ranges from ``min = 770`` to ``max = 2160`` minutes (``mode =  1440``) based on descriptions of post- DCS-I stabilization requirements (described as 24-36 h in most DCS research [[17]](#References), [[20]](#References), [[25]](#References)), while the secondary ICU phase (following second surgery) ranges from ``min = 30`` to ``mode = 90``, with ``mode = 60`` (min) to allow for post surgery monitoring and stabilisation prior to transfer to holding. Casualties who arrive at the R2E requiring surgery, but not having received any prior to arrival are queued to complete a second round of surgery after ICU time. 

After completing surgery and ICU monitoring, patients are either transferred to holding for recovery or undertake strategic evacuation. ~10% of casualties undertake recovery at the R2E following a triangular distribution for recovery time with ``min = 1``, ``max = 21``, and ``mode = 9`` (days) this distribution was selected on the basis that casualties with shorter recovery times and a likelihood for capacity to return to duty following recovery would be retained in theatre. The remaining ~90% are transferred for strategy evacuation. Based on [[9]](#References) Vietnam data that indicated 31% return to duty with 42% in theatre providing about 13% recovery in theatre at R2E

```mermaid
flowchart TD
    A(["Start"]) --> B{"DOW?"}
    B -- Yes --> C["Treat KIA"]
    C --> D["Transfer"]
    D --> E(["End"])
    B -- No --> F["Seize Hold Bed"]
    F --> G["Seize Resus Bed <br> Release Hold Bed"]
    G --> H{"Prev Resus?"}
    H -- No --> I["Long Resus"]
    H -- Yes --> J["Short Resus"]
    I --> K{"Surgery?"}
    J --> K
    K -- Yes --> L["Seize OT"]
    L --> M["Surgery"]
    M --> N["Release Resources"]
    K -- No --> O{"Evac?"}
    N --> O
    O -- No --> P["Seize Hold Bed"]
    P --> Q["Recover at R2E"]
    Q --> R["Release Hold Bed"]
    R --> S["Return to Duty"]
    O -- Yes --> T["Strategic Evacuation"]
    S --> E
    T --> E
```

---

## Model Outputs

The simulation produces a defined set of Key Performance Indicators (KPIs) organised by planner decision domain. Each KPI is selected against five criteria derived from military medical doctrine and discrete event simulation methodology [[22]](#References):

- **C1 — Doctrinal Standard Compliance:** Variable measures compliance with a named standard in AJP-4.10 [[21]](#References).
- **C2 — Planner Decision Relevance:** Variable value would change a force structure, positioning, or evacuation policy decision.
- **C3 — Causal Pathway Position:** Variable lies on the causal path between input parameters and health outcomes, required for meaningful Morris sensitivity screening.
- **C4 — Binding Constraint Identification:** Variable identifies when a resource or process becomes the active bottleneck.
- **C5 — Health Outcome Attribution:** Variable connects to a measurable health outcome (mortality, RTD, time-to-care).

> **Note on Point-of-Injury Time:** The simulation generates casualties as entities entering at Role 1. There is no pre-R1 phase modelled. Simmer's `start_time` in the arrivals monitor equals R1 arrival time, not point of injury. All time-to-care KPIs are therefore measured from R1 arrival, not point of injury. The POI-to-R1 transit falls outside the model's scope and cannot be derived from the current simulation structure. See the Limitations section for impact assessment.

---

### Domain 1 — Mortality and Preventable Death

> **MODEL OUTPUT — Total DOW Count:**
> Count of casualties assigned `dow = 1` across all replications. Includes all echelons.
> **Doctrinal basis:** AJP-4.10 §3: evacuation timeliness standards are designed to minimise preventable death.
> **Criteria:** C1, C2, C5
> **Computation:** `sum(attributes_wide$dow == 1, na.rm = TRUE)` per replication.
> **Limitation:** Flat DOW probability (5% P1, 2.5% P2 at R1; 1% at R2B/R2E) is independent of wait time. Time-dependent DOW (Issue #5) is required before this metric reflects the clinical consequence of queue saturation.

> **MODEL OUTPUT — DOW Rate by Echelon:**
> Count and proportion of DOW deaths occurring at each echelon (R1, R2B, R2E), derived from the `dow_echelon` attribute. Attribute encoding: 1 = R1, 2 = R2B, 3 = R2E (simmer supports only numeric attribute values).
> **Doctrinal basis:** AJP-4.10 §5: echelon-specific mortality is the primary indicator for role-appropriate capability allocation.
> **Criteria:** C1, C2, C3, C5
> **Computation:** Filter `attributes_wide` where `dow == 1`; decode `dow_echelon` (1→"r1", 2→"r2b", 3→"r2e"); count by decoded echelon label; divide by total arrivals for rate. Consistency check: echelon subtotals must sum to total DOW count.
> **Limitation:** Until Issue #5 is implemented, echelon DOW rates are not sensitive to system load and cannot be used to assess the effect of queue saturation on mortality.

---

### Domain 2 — Time-to-Care from R1 Arrival

> **MODEL OUTPUT — Time from R1 Arrival to First Surgical Incision:**
> Elapsed time (minutes) from R1 arrival (`start_time`) to first surgical incision (`min(r2b_surgery_start, r2e_surgery_1_start)`), per casualty requiring surgery.
> **Doctrinal basis:** AJP-4.10 §5 and the NATO 10-1-2 timeline specify surgical intervention within 2 hours of point of injury. This KPI measures the within-system component of that standard.
> **Criteria:** C1, C2, C3, C5
> **Computation:** `pmin(r2b_surgery_start, r2e_surgery_1_start, na.rm = TRUE) - start_time`; exclude KIA and DOW cases where death preceded any surgery; report mean, p10, p90.
> **Limitation:** Measured from R1 arrival, not point of injury. The POI-to-R1 component (evacuation from point of wounding to R1) is outside the model's scope and must be added separately to compare against the doctrinal 2-hour standard.

> **MODEL OUTPUT — R2B Dwell Time:**
> Time (minutes) a casualty spends at R2B from treatment start (`r2b_treatment_start_time`) to departure towards R2E (`r2b_departure_time`).
> **Doctrinal basis:** AJP-4.10 §5 specifies that R2B (Role 2 Basic) dwell should not exceed the damage control surgery window; extended dwell indicates holding capacity pressure.
> **Criteria:** C1, C3, C4
> **Computation:** `r2b_departure_time - r2b_treatment_start_time`; report mean and p90.

> **MODEL OUTPUT — R2B→R2E Transit Time:**
> Time (minutes) between R2B departure (`r2b_departure_time`) and R2E arrival (`r2e_arrival_time`).
> **Doctrinal basis:** AJP-4.10 §5 evacuation time norms for second-echelon to third-echelon transfer.
> **Criteria:** C1, C3
> **Computation:** `r2e_arrival_time - r2b_departure_time`; report mean and p90.

> **MODEL OUTPUT — R2E Dwell Time:**
> Time (minutes) a casualty spends at R2E from arrival (`r2e_arrival_time`) to disposition (`r2e_departure_time`), covering resuscitation, surgery, ICU, and holding.
> **Doctrinal basis:** R2E (Role 2 Enhanced) dwell is the primary determinant of ICU and OT bed occupancy; AJP-4.10 §5 capacity planning norms are calibrated to expected dwell distributions.
> **Criteria:** C1, C3, C4
> **Computation:** `r2e_departure_time - r2e_arrival_time`; report mean and p90.

---

### Domain 3 — Surgical Throughput

> **MODEL OUTPUT — OT Utilisation Rate by Echelon:**
> Server time as a proportion of available capacity-minutes within the observation window, for R2B and R2E operating theatres.
> **Doctrinal basis:** AJP-4.10 §5 bed and OT planning ratios; sustained utilisation above 85% indicates saturation risk.
> **Criteria:** C3, C4
> **Computation:** `sum(server × duration) / (sum(capacity) × observation_window)` per echelon, derived from resource monitor for `b_r2b_ot_*` and `b_r2eheavy_ot_*` resources.

> **MODEL OUTPUT — R2B and R2E Surgery Counts per Day:**
> Count of surgical cases started per simulation day at each echelon, derived from `r2b_surgery_start` and `r2e_surgery_1_start` / `r2e_surgery_2_start` attributes.
> **Doctrinal basis:** AJP-4.10 §5 OT throughput norms; daily surgical volume is the primary operational throughput indicator for surgical teams.
> **Criteria:** C2, C3, C4
> **Computation:** Floor of surgery start time divided by 1440; count by day and echelon.

---

### Domain 4 — Echelon Load and Capacity

> **MODEL OUTPUT — Resource Queue Length Over Time:**
> Queue length time-series for each bed type (hold, resus, OT, ICU) at R1, R2B, and R2E, derived from the simmer resource monitor.
> **Doctrinal basis:** AJP-4.10 §5 bed ratios and queue saturation thresholds; sustained non-zero queues indicate structural capacity shortfall.
> **Criteria:** C3, C4
> **Computation:** `queue` column from `get_mon_resources()` filtered by resource name pattern per echelon.

---

### Domain 5 — Flow and Disposition

> **MODEL OUTPUT — RTD Rate by Echelon:**
> Count and proportion of casualties returning to duty at each echelon (R1, R2B, R2E), derived from the `return_echelon` attribute alongside `return_day` assignments. Attribute encoding: 1 = R1, 2 = R2B, 3 = R2E.
> **Doctrinal basis:** AJP-4.10 §5 [[21]](#References): in-theatre return-to-duty rate is the primary combat power conservation metric; echelon-level RTD indicates where treatment is most efficient.
> **Criteria:** C1, C2, C5
> **Computation:** Filter `attributes_wide` where `return_day` is not NA; decode `return_echelon` (1→"r1", 2→"r2b", 3→"r2e"); count by decoded echelon label; divide by WIA + DNBI arrivals for rate. Consistency check: echelon subtotals must sum to total RTD count.

> **MODEL OUTPUT — R2B Bypass Rate:**
> Proportion of WIA casualties routed directly from R1 to R2E without R2B treatment, identifiable where `r2e_treated` is not NA and `r2b_treated` is NA.
> **Doctrinal basis:** AJP-4.10 §5: bypass indicates either R2B overload or deliberate acuity-based routing policy; elevated bypass rates reduce R2B workload while increasing R2E demand.
> **Criteria:** C2, C3, C4
> **Computation:** Count of `combined` where `!is.na(r2e_treated) & is.na(r2b_treated)`, divided by total WIA arrivals.

---

### Domain 6 — Combat Power

> **MODEL OUTPUT — Total RTD Count:**
> Count of casualties assigned `return_day` across all replications.
> **Doctrinal basis:** AJP-4.10 §5 and ADDP 4.2: return-to-duty throughput directly determines the rate at which combat power is regenerated from the medical system.
> **Criteria:** C2, C5
> **Computation:** `sum(!is.na(attributes_wide$return_day))` per replication.

---

### Output Variable Register cross-reference

| KPI | Domain | Attributes Required | Criteria | Analysis Function |
|---|---|---|---|---|
| Total DOW count | Mortality | `dow` | C1, C2, C5 | `sum(dow == 1)` |
| DOW rate by echelon | Mortality | `dow`, `dow_echelon` | C1–C3, C5 | `dow_by_echelon` |
| Time to first surgery | Time-to-care | `r2b_surgery_start`, `r2e_surgery_1_start`, `start_time` | C1–C3, C5 | `time_to_first_surgery` |
| R2B dwell time | Time-to-care | `r2b_treatment_start_time`, `r2b_departure_time` | C1, C3, C4 | `r2b_dwell_time` |
| R2B→R2E transit | Time-to-care | `r2b_departure_time`, `r2e_arrival_time` | C1, C3 | `r2b_r2e_transit_time` |
| R2E dwell time | Time-to-care | `r2e_arrival_time`, `r2e_departure_time` | C1, C3, C4 | `r2e_dwell_time` |
| OT utilisation | Surgical | resource monitor | C3, C4 | `ot_utilisation` |
| Surgery counts/day | Surgical | `r2b_surgery_start`, `r2e_surgery_*` | C2–C4 | `r2b_summary`, `r2e_summary` |
| Queue length over time | Echelon load | resource monitor | C3, C4 | resource plots |
| RTD rate by echelon | Flow/disposition | `return_day`, `return_echelon` | C1, C2, C5 | `rtd_by_echelon` |
| R2B bypass rate | Flow/disposition | `r2b_treated`, `r2e_treated` | C2–C4 | derived in `combined` |
| Total RTD count | Combat power | `return_day` | C2, C5 | `sum(!is.na(return_day))` |

---

## Simulation Analysis

### Simulation Casualty Generation

This section presents a detailed breakdown of casualty source data captured from a single simulation run using seed 42, spanning a 30-day operational duration. The data is analyzed through the lens of deployed health system design, highlighting implications for medical resource allocation, evacuation planning, and treatment capacity across Role 1 and Role 2 facilities.

> **Note on warm-up exclusion:** No warm-up exclusion is applied. The simulation is classified as a terminating simulation; the full observation window, including campaign start-up, is retained in all outputs (`WARM_UP_DAYS = 0L`). See the [Warm-up Period Analysis](#warm-up-period-analysis) section for the methodological basis.

![Alt text](images/casualty_summary.png)

| Casualty Type | Population Source | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   | 9   | 10  | 11  | 12  | 13  | 14  | 15  | 16  | 17  | 18  | 19  | 20  | 21  | 22  | 23  | 24  | 25  | 26  | 27  | 28  | 29  | 30  | total |
|:------------- |:----------------- | ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| -----:|
| dnbi          | cbt               | 4   | 5   | 5   | 4   | 5   | 4   | 5   | 5   | 5   | 4   | 5   | 5   | 4   | 5   | 5   | 5   | 5   | 4   | 5   | 5   | 4   | 5   | 5   | 5   | 4   | 5   | 4   | 5   | 5   | 5   | 141   |
| dnbi          | spt               | 1   | 1   | 1   | 1   | 1   | 2   | 1   | 1   | 1   | 1   | 1   | 2   | 1   | 1   | 1   | 1   | 2   | 1   | 1   | 1   | 1   | 1   | 2   | 1   | 1   | 1   | 1   | 1   | 1   | 2   | 35    |
| kia           | cbt               | 1   | 2   | 1   | 2   | 1   | 2   | 2   | 1   | 2   | 2   | 1   | 2   | 1   | 2   | 2   | 1   | 2   | 1   | 2   | 1   | 2   | 2   | 1   | 2   | 1   | 2   | 1   | 2   | 1   | 2   | 47    |
| kia           | spt               | 0   | 1   | 1   | 1   | 0   | 1   | 1   | 1   | 1   | 0   | 1   | 1   | 1   | 1   | 0   | 1   | 1   | 1   | 1   | 0   | 1   | 1   | 1   | 1   | 0   | 1   | 1   | 1   | 1   | 0   | 23    |
| wia           | cbt               | 3   | 4   | 3   | 3   | 4   | 3   | 4   | 3   | 4   | 3   | 4   | 3   | 4   | 3   | 3   | 4   | 4   | 3   | 4   | 3   | 4   | 3   | 3   | 4   | 3   | 4   | 3   | 4   | 3   | 4   | 104   |
| wia           | spt               | 1   | 2   | 2   | 1   | 2   | 2   | 2   | 1   | 2   | 2   | 1   | 2   | 2   | 1   | 2   | 2   | 2   | 1   | 2   | 2   | 1   | 2   | 2   | 2   | 1   | 2   | 2   | 1   | 2   | 2   | 51    |
| Total         |                   | 10  | 15  | 13  | 12  | 13  | 14  | 15  | 12  | 15  | 12  | 13  | 15  | 13  | 13  | 13  | 14  | 16  | 11  | 15  | 12  | 13  | 14  | 14  | 15  | 10  | 15  | 12  | 14  | 13  | 15  | 401   |

The table above presents a summary of the simulated casualty data generated across three primary categories Wounded in Action (WIA), Killed in Action (KIA), and Disease and Non-Battle Injury (DNBI), with their source population: combat forces and support forces. A total of 401 casualties were recorded, with combat elements accounting for the majority (292), reflecting their higher exposure to operational risk. DNBI emerged as the most frequent casualty type (176 cases), underscoring the persistent burden of non-combat medical conditions even in high-intensity environments. This aligns with historical data indicating that DNBI can rival or exceed battle injuries in terms of lost duty days and medical resource consumption.

WIA cases totaled 155, with a notable skew toward combat personnel (104 vs. 51) as a result of the force ratios present within the simulation. These casualties typically require multi-echelon care, including resuscitation, surgical intervention, and post-operative holding, placing sustained demand on Role 1 and Role 2 facilities. KIA figures were lower (70 total).

From a health system planning perspective, this data implies a need for scalable treatment capacity, robust DNBI mitigation strategies, and distributed surgical capability. The consistent casualty generation across periods suggests a steady operational tempo, requiring continuous staffing, replenishment of medical supplies, and resilient evacuation pathways.

| Population Source | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   | 9   | 10  | 11  | 12  | 13  | 14  | 15  | 16  | 17  | 18  | 19  | 20  | 21  | 22  | 23  | 24  | 25  | 26  | 27  | 28  | 29  | 30  | total |
|:----------------- | ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| -----:|
| cbt               | 8   | 11  | 9   | 9   | 10  | 9   | 11  | 9   | 11  | 9   | 10  | 10  | 9   | 10  | 10  | 10  | 11  | 8   | 11  | 9   | 10  | 10  | 9   | 11  | 8   | 11  | 8   | 11  | 9   | 11  | 292   |
| spt               | 2   | 4   | 4   | 3   | 3   | 5   | 4   | 3   | 4   | 3   | 3   | 5   | 4   | 3   | 3   | 4   | 5   | 3   | 4   | 3   | 3   | 4   | 5   | 4   | 2   | 4   | 4   | 3   | 4   | 4   | 109   |
| Total             | 10  | 15  | 13  | 12  | 13  | 14  | 15  | 12  | 15  | 12  | 13  | 15  | 13  | 13  | 13  | 14  | 16  | 11  | 15  | 12  | 13  | 14  | 14  | 15  | 10  | 15  | 12  | 14  | 13  | 15  | 401   |

The second table provides a breakdown of the casualty population by source: combat forces (cbt) and support forces (spt). Of the 401 total casualties generated, 292 (approximately 73%) originated from combat elements, while 109 (27%) were drawn from support units. This distribution reflects the total population breakdown of the organisation. The consistent presence of support force casualties across all periods underscores the vulnerability of rear-area personnel in LSCO environments, particularly under conditions of indirect fire, degraded situational awareness, and disrupted medical evacuation. The temporal spread of casualties shows a relatively stable operational tempo, with total casualties per period ranging from 10 to 16. 

From a health system perspective, this data reinforces the need for distributed medical coverage that includes both forward and rear-area assets. Role 1 treatment teams must be positioned to respond rapidly to combat casualties, while Role 2 facilities must be capable of absorbing and triaging support force casualties who may present with different injury profiles, including DNBI and delayed trauma. The consistent casualty burden across both populations highlights the importance of scalable capacity, flexible evacuation pathways, and robust command and control to ensure timely treatment and prevent bottlenecks in casualty flow.

| Priority | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   | 9   | 10  | 11  | 12  | 13  | 14  | 15  | 16  | 17  | 18  | 19  | 20  | 21  | 22  | 23  | 24  | 25  | 26  | 27  | 28  | 29  | 30  | total |
|:-------- | ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| ---:| -----:|
| Pri 1    | 7   | 9   | 5   | 6   | 6   | 6   | 9   | 6   | 10  | 7   | 8   | 8   | 5   | 8   | 7   | 8   | 8   | 6   | 6   | 6   | 5   | 8   | 6   | 10  | 7   | 7   | 6   | 4   | 7   | 6   | 207   |
| Pri 2    | 1   | 2   | 4   | 1   | 4   | 4   | 2   | 1   | 1   | 1   | 1   | 3   | 6   | 1   | 3   | 2   | 2   | 2   | 4   | 3   | 3   | 1   | 5   | 2   | 1   | 4   | 2   | 4   | 1   | 5   | 76    |
| Pri 3    | 1   | 1   | 2   | 2   | 2   | 1   | 1   | 3   | 1   | 2   | 2   | 1   | 0   | 1   | 1   | 2   | 3   | 1   | 2   | 2   | 2   | 2   | 1   | 0   | 1   | 1   | 2   | 3   | 3   | 2   | 48    |
| KIA      | 1   | 3   | 2   | 3   | 1   | 3   | 3   | 2   | 3   | 2   | 2   | 3   | 2   | 3   | 2   | 2   | 3   | 2   | 3   | 1   | 3   | 3   | 2   | 3   | 1   | 3   | 2   | 3   | 2   | 2   | 70    |
| Total    | 10  | 15  | 13  | 12  | 13  | 14  | 15  | 12  | 15  | 12  | 13  | 15  | 13  | 13  | 13  | 14  | 16  | 11  | 15  | 12  | 13  | 14  | 14  | 15  | 10  | 15  | 12  | 14  | 13  | 15  | 401   |

Of the total casualties, 207 (51.6%) were classified as Priority 1, representing patients requiring immediate life-saving intervention. This dominant category underscores the doctrinal necessity of forward-positioned Role 1 assets capable of rapid triage and stabilization. The consistent presence of Priority 1 cases across all 30 days suggests a sustained high-acuity burden, reinforcing the need for scalable throughput 

Priority 2 and Priority 3 casualties accounted for 76 (19.0%) and 48 (12.0%) cases respectively. These patients typically require delayed or routine care. The simulation also generated 70 KIA cases (17.5%), distributed evenly across the operational timeline. While these cases do not contribute to medical workload substantially, their operational implications are significant.

From a systems design perspective, the acuity profile derived from this simulation reinforces several key imperatives:

- Role 1 facilities must be optimized for high-throughput triage and stabilization, with emphasis on rapid evacuation of Priority 1 cases.
- Role 2 facilities requires flexible bed space and surgical capability to absorb cases, especially during sustained operations.
- Evacuation architecture must support continuous movement of mixed-acuity casualties, with prioritization protocols and redundancy to ensure resilience.

### R1 Handling

Role 1 facilities consistently demonstrated the ability to process casualties without delay, with all patients receiving immediate triage and treatment on arrival. The absence of queuing reflects both adequate staffing and appropriately scaled treatment capacity relative to the casualty inflow modelled. Rapid handling times ensured that Priority 1 cases could be stabilised and evacuated without degradation in clinical status, while lower‑priority cases were managed and prepared for movement in line with requirements. However, the model does not currently fully represent the limitations in availability of evacuation assets, as a result, throughput at the Role 1 was not constrained by evacuation availability, allowing continuous casualty flow to higher‑echelon care and preventing downstream bottlenecks in the system which may bear out with the introduction of more detailed modelling of evacuation. Despite this, the performance underscores the critical function of Role 1 as an agile, forward medical capability able to maintain momentum under sustained operational tempo.

![Alt text](images/r1_queues.png)

### R2B Handling

The plot below outlines a summary of the casualty handling at the R2B. The data indicates a system that is generally well-balanced, handling a substantial number of the casualties generated within the simulation every day. 

![Alt text](images/r2b_handling.png)

Daily casualty treatments at R2B 1 and R2B 2 show saw-tooth cycles in queuing for OT (shown in the plots below) returning to zero in the vast majority of days. Of note, the 12-hour availability window for the single surgical team in each R2B is likely to substantially impact this saw-toothing. The development of models to support the ongoing availability of the R2B OT is likely to see substantial improvement in overall system capacity with relatively modest resource investment. The majority of casualties processed through the deployed health system receive treatment at the R2B, confirming that the two-node configuration is effective for the medical demand without excessive reliance on direct bypass to a higher echelon of health care.

![Alt text](images/r2b_bed_queues.png)

### R2E Heavy Handling

The R2E Heavy reveals a complex interaction between critical care saturation, surgical throughput, and the utilisation of downstream holding capacity. ICU queues displayed sustained and system‑wide congestion during several periods, most notably between Days 11–13 and Days 26–29, where as many a number of patients were awaiting critical care. Such peaks are consistent with high‑acuity casualty streams in LSCO and prolonged ICU lengths of stay, both of which constrain turnover rates. Under these conditions, surgical throughput is at risk, as post‑operative patients may be delayed in theatre or recovery pending ICU availability, potentially increasing morbidity and mortality. This dynamic underscores the vulnerability of the R2E Heavy’s ICU provision to operational surges and the need for either expanded critical care capacity or well‑rehearsed transfer protocols to higher‑echelon or other health facilities.

![Alt text](images/r2eheavy_bed_queue_3_teams.png)

Operating Theatre (OT) queues at the R2E Heavy were shorter and less frequent than ICU queues, rarely exceeding two patients per theatre; however, they were almost continuously present throughout the simulation. This pattern suggests the surgical component was operating at or near full capacity, with minimal slack to absorb unplanned casualty spikes. When examined in system context, the combined OT capacity of two Role 2 Basic (R2B) elements and one R2E Heavy appears sufficient to support a single combat brigade under conditions modelled on the Falklands casualty rates [[8]](#References). Yet, if this deployed health system was applied to a deployed division it would be grossly insufficient, even if only one brigade was assumed to be in contact at any time. With no other changes to casualty rates with an increased force, DNBI rates for a the further deployed force of a division would exceed the limited excess capacity that is observed in this system. Furthermore, the modelled scenario does not account for mass‑casualty events or the elevated casualty production rates reported in FORECAS modelling of campaigns such as Okinawa, or Vietnam both of which would further expose this deficit [[8]](#References).

![Alt text](images/r2eheavy_gantt.png)

Analysis of other treatment capacities revealed significant underutilisation in certain areas. Two resuscitation beds were unused throughout the simulation, while sixteen holding beds saw no occupancy at all. This imbalance offers an opportunity to reallocate resources—either through expansion of OT capacity at the R2E Heavy or by modifying holding policy to increase the proportion of in‑theatre recoveries beyond the ten percent modelled in the simulation. A higher return‑to‑duty rate within theatre could alleviate the burden on higher‑echelon facilities and improve the overall force operational viability period.

![Alt text](images/r2eheavy_surgeries.png)

Taken together, the simulation outputs indicate that the R2E Heavy’s primary limiting factor is OT availability, with ICU capacity representing a secondary but significant constraint. Both could be mitigated through deliberate rebalancing of underused bed spaces and by modelling scenarios that better reflect LSCO surge conditions. Forward Role 2 Basic nodes remain a critical component in modulating the demand placed upon the R2E Heavy, highlighting the importance of integrated patient flow monitoring, robust surge protocols, and synchronised scheduling across all surgical and critical care assets. Without such measures, even sustained baseline performance offers little resilience to the shock of high‑intensity operational realities.

### Conclusion

The single run analysis, viewed in its entirety, demonstrates that while the modelled deployed health system is capable of sustaining a steady operational tempo for a single brigade under baseline casualty assumptions (using casualty models derived from the Falklands war), it operates with little reserve and exhibits critical vulnerabilities under higher‑demand conditions. Role 1 elements show sufficient responsiveness and throughput, and the dual‑node R2B configuration effectively absorbs the majority of cases without systemic reliance on bypass to higher care. However, the R2E Heavy emerges as a structural bottleneck, with surgical and ICU capacity constraints that would be rapidly overwhelmed in the event of divisional‑level operations, mass‑casualty incidents, or casualty rates on par with historical LSCO campaigns with more substantial casualty rates such as Okinawa or Vietnam.

Underutilisation of resuscitation and holding beds indicates scope for resource reallocation or policy changes to increase in‑theatre recovery and return‑to‑duty rates, thereby reducing pressure on constrained surgical and critical care assets. Without such adjustments, the system’s ability to absorb prolonged surges, manage high‑acuity case‑mixes, and maintain operational tempo would be severely limited.

Ultimately, the findings reinforce that effective LSCO medical support cannot rely solely on baseline performance metrics. Instead, it must be built on a foundation of scalable capacity, adaptable evacuation architecture, and dynamically balanced resource distribution between Roles 1, 2B, and 2E Heavy. By integrating these design principles into future modelling and force development, the deployed health system will be better positioned to sustain combat effectiveness across the full spectrum of operational intensity.

---

## Limitations

This section consolidates known model limitations, organised by impact on findings. Each limitation is cross-referenced to the inline assumption blocks or output annotation blocks where applicable, and to the action plan issue addressing it where one exists.

### High Impact

**L1 — Point-of-Injury to R1 Transit Not Modelled (Medium Impact on Time-to-Care KPIs)**
The simulation generates casualties as entities entering at Role 1 (R1). The transit from point of injury (POI) to R1 — covering application of tourniquet, self-aid, buddy-aid, and tactical field care — is outside the model's scope. All time-to-care KPIs are therefore measured from R1 arrival, not POI. This means the "time to first surgical incision" KPI represents only the within-system delay and cannot be directly compared to the doctrinal AJP-4.10 2-hour surgical standard without adding an external POI-to-R1 estimate. The within-system delay component remains planner-controllable; the POI-to-R1 component is determined by tactical factors outside the health system. **Impact: Medium.** Rated Medium rather than High because the within-system delay is the component planners can act on; however, any comparison to the doctrinal 2-hour standard must account for this gap explicitly.

**L2 — Flat DOW Rate Independent of Wait Time (High Impact on Mortality KPIs)**
Died of Wounds (DOW) probability is applied as a fixed value (5% P1, 2.5% P2 at R1; 1% at R2B/R2E) regardless of how long a casualty has waited for care. This means ICU saturation, OT queuing, and evacuation delay have zero effect on modelled mortality — the most clinically consequential relationship in the model is absent. DOW rate by echelon (KPI Domain 1) and time-to-care KPIs (Domain 2) are currently disconnected. **Impact: High.** Addressed in Issue #5 (time-dependent DOW survival function). **Until Issue #5 is merged, DOW count and rate outputs must not be used to assess the mortality consequence of queue saturation.**

**L3 — Team-Block Resource Seizure (High Impact on Bottleneck Identification)**
Resources are seized as whole team vectors. A second casualty cannot use any team member even when the first casualty requires only a subset of skills. Skill-specific bottlenecks (surgeon vs. anaesthetist vs. nursing officer) and task-sharing under surge conditions are invisible. OT utilisation and queue length KPIs understate contention. **Impact: High.** Addressed in Issue #4 (individual resource seizure refactor).

### Medium Impact

**L4 — Undifferentiated DNBI Treatment Pathway (Medium Impact on Surgical Demand)**
All DNBI casualties enter the same triage-resus-surgery routing as WIA. In practice, disease and battle fatigue cases almost never require surgery. Routing them through the surgical pathway inflates modelled surgical demand and understates the true WIA surgical bottleneck. **Impact: Medium.** Addressed in Issue #7 (DNBI sub-category routing).

**L5 — Unidirectional Transport (Medium Impact on Asset Availability)**
PMV ambulances are seized for the outbound leg only. Vehicles do not return to the originating echelon before becoming available again. Transport asset availability is systematically overestimated. **Impact: Medium.** Addressed in Issue #6 (dead-heading return legs).

**L6 — No MASCAL Stochastic Injection (Medium Impact on Surge Capacity Assessment)**
The casualty generation model produces a smooth lognormal daily rate. Discrete tactical events generating 20–50 casualties within a 2–4 hour window — the primary stress test for surgical and ICU capacity in LSCO — are entirely absent. **Impact: Medium.** Addressed in Issue #9 (compound Poisson MASCAL injection).

**L7 — Single Baseline Casualty Rate Scenario (Medium Impact on Generalisability)**
The current analysis uses Falklands-derived casualty rates (~0.37% daily rate), the most conservative available benchmark. System adequacy conclusions are bounded to this scenario and cannot be extrapolated to Vietnam- or Okinawa-intensity LSCO without scenario expansion. **Impact: Medium.** Addressed in Issue #10 (comparative scenario runner).

**L8 — Partial Antithetisation (Low Impact on CI Precision)**
Antithetic variate variance reduction is applied to arrival time generation only. Service times and routing probabilities are generated internally by simmer's C++ engine from R's global RNG and cannot be antithetised without deep trajectory instrumentation. The CI-narrowing benefit of antithetic pairing is therefore partial: it reduces arrival-driven variance but leaves service-time variance unreduced. **Impact: Low** — the dominant source of between-replication variance is arrival schedule variation (lognormal), which is fully antithetised; residual variance from service draws is secondary.

### Low Impact

**L9 — No Endogenous Force Feedback (Low Impact on Arrival Rates)**
Casualty arrival rates are fixed exogenous inputs applied to a static force size. The feedback loop between return-to-duty rates, strategic evacuation, force depletion, and future casualty production is not represented. **Impact: Low** for 30-day runs; increases with campaign duration. Addressed in Issue #18 (endogenous casualty generation).

---

## Further Development

The single run analysis has demonstrated that while the current simulation framework offers a credible baseline for evaluating deployed health system performance under brigade-level LSCO conditions, several areas warrant further development to improve the accuracy of the model and enhance the analysis from it.

One immediate opportunity lies in expanding the modelling of transport resources to include return journeys—commonly referred to as "dead-heading". The current simulation assumes unidirectional casualty movement, which underestimates the logistical burden and resource consumption associated with evacuation cycles. Incorporating return legs would allow for more accurate scheduling, asset availability tracking, and fuel or crew fatigue modelling, all of which are critical in contested or extended operational environments.

Another refinement involves pulsing strategic medical evacuation availability to simulate its temporal constraints. Rather than assuming continuous access to strategic lift, future iterations should model episodic availability windows, reflecting real-world limitations such as airframe tasking, weather delays, or air superiority conditions. This would allow for more realistic bottleneck formation and better inform prioritisation protocols for high-acuity casualties.

Model fidelity can also be improved through structured expert consultation. Engaging clinicians, medical planners, and operational commanders would support refinement of treatment durations, triage logic, and evacuation thresholds. This would ensure that the simulation reflects not only doctrinal intent but also clinical realities and operational constraints.

A critical enhancement involves introducing dynamic modelling of Died of Wounds (DOW) outcomes based on wait times. The current simulation does not penalise delayed treatment, which may understate the clinical consequences of ICU or OT saturation. By linking DOW probability to time-to-treatment metrics, future models can better capture the operational cost of medical delay and support more aggressive capacity planning.

To reflect the unpredictability of LSCO, future simulations should also incorporate rare but high-impact mass-casualty events. These could be triggered stochastically and used to evaluate system shock absorption, surge protocols, and triage degradation under extreme conditions. Such modelling would be particularly valuable in validating the robustness of the R2E Heavy and its ability to maintain throughput under duress.

Finally, comparative analysis against other casualty generation models—particularly those published in [[8]](#References), such as Okinawa and Vietnam—would provide a more rigorous test of system scalability. These historical scenarios project casualty rates far beyond those modelled in the current run and would likely expose critical shortfalls in surgical capacity, evacuation throughput, and ICU resilience. Such stress-testing is essential to determine whether the current health system design is supportable under true LSCO conditions or whether significant redesign is required.

Despite these refinements, the recommendations from the single run analysis remain relevant. Rebalancing underutilised bed spaces (e.g., resuscitation and holding beds), expanding in-theatre recovery rates to improve return-to-duty throughput, and exploring the operational impact of increasing surgical team availability at R2B nodes are all worth investigating. The model refinements will support the development of a more responsive, and scalable deployed health system capable of sustaining combat power under the full spectrum of LSCO demands.

---

## Conclusion

This research has advanced the modelling of deployed health system performance by using casualty generation modelled on historical conflict casualty data with a discrete event simulation framework capable to build a brigade-level simulation that can be run over extended operational durations. Through a systematic review of existing literature, previously published casualty models were identified, restructured, and adapted to support per-minute simulation granularity; enabling evaluation of medical infrastructure across 30+ day campaigns.

A representative deployed health system was constructed, encompassing triage, evacuation, and definitive care, and incorporating a three-stage model of damage control surgery derived from academic resources. The simulation was executed under moderate casualty conditions, and its outputs were analysed to identify systemic risks, capacity shortfalls, and design inefficiencies. These findings informed targeted recommendations for system refinement, including adjustments to bed allocation, surgical team distribution, and recovery throughput.

The simulation demonstrates that the current system design is capable of managing casualty rates comparable to those experienced during the Falklands conflict. However, this historical benchmark may not reflect the operational realities anticipated in future large-scale combat operations (LSCO), where casualty volumes, evacuation delays, and treatment bottlenecks may be significantly more severe. The current model, while robust under moderate stress, has not yet been fully tested against LSCO-scale casualty projections.

To address this gap, further development is required. Enhancements such as dynamic DOW modelling, pulsed strategic evacuation availability, and stochastic mass-casualty event triggers will improve realism and operational relevance. Comparative analysis against high-intensity casualty models—such as those published in [[8]](#References) for Okinawa and Vietnam—will be essential to evaluate whether the current health system architecture is supportable under LSCO conditions or requires fundamental redesign.

Ultimately, this research provides a transparent, modular, and extensible foundation for future simulation efforts. It enables planners, clinicians, and commanders to interrogate system performance, anticipate failure points, and iteratively refine medical support doctrine. With continued development and rigorous testing, this framework can evolve into a decision-support tool capable of guiding health system design for the most demanding operational environments.

---

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

[13] Ucar I, Smeets B, Azcorra A (2019). “simmer: Discrete-Event Simulation for R.” *Journal of Statistical Software*, 90(2), 1–30. [doi:10.18637/jss.v090.i02](https://doi.org/10.18637/jss.v090.i02).

[14] Maddeh, M., Ayouni, S., Al-Otaibi, S., Alazzam, M. B., Alturki, N. M., & Hajjej, F. (2023). Discrete-Event Simulation Model for Monitoring Elderly and Patient’s Smart Beds. *Journal of Disability Research*, *2*(3), 1-9. DOI: 10.57197/JDR-2023-0026. Retrieved 26 Jul 25, from https://www.scienceopen.com/hosted-document?doi=10.57197/JDR-2023-0026

[15] Kemple, W. G., & Lacy, L. W. (1995). *Modeling command and control: The design and implementation of the C2 model*. Defense Technical Information Center. Retrieved 20 Jul 25, from https://apps.dtic.mil/sti/html/tr/ADA304910/)

[16] Wang, Y., & Pinsky, E. (2023). Geometry of deviation measures for triangular distributions. *Frontiers in Applied Mathematics and Statistics*, *9*, 1274787. Retrieved 26 Jul 25, from https://doi.org/10.3389/fams.2023.1274787

[17] Zizzo, M., Ruiz, C. C., Zanelli, M., Bassi, M. C., Sanguedolce, F., Ascani, S., & Annessi, V. (2020). Damage control surgery for the treatment of perforated acute colonic diverticulitis: a systematic review. *Medicine*, *99*(48), e23323. Retrieved 26 Jul 25, from https://journals.lww.com/md-journal/fulltext/2020/11250/damage_control_surgery_for_the_treatment_of.43.aspx

[18] Karamarković, A. Damage Control in Abdominal Surgery. *Clin Surg. 2016; 1*, *1118*. Retrieved 02 Aug 25, from https://www.clinicsinsurgery.com/open-access/damage-control-in-abdominal-surgery-2563.pdf

[19] Abri, M. A., Snani, S. A., Almayahi, J., Sharqi, A. A., & Qadhi, H. A. The Outcome of Damage Control Surgery at Sultan Qaboos University Hospital. *World J Surg Surgical Res. 2022; 5*, *1428*. Retrieved 26 Jul 25, from https://www.surgeryresearchjournal.com/open-access/the-outcome-of-damage-control-surgery-at-sultan-qaboos-university-9532.pdf

[20] Nickson, C. (2020, November 3). *Damage Control Resuscitation*. Life in the Fastlane. Retrieved 27 July, 2025, from https://litfl.com/damage-control-resuscitation/

[21] NATO Standardization Office. (2019). *AJP-4.10 allied joint doctrine for medical support* (Edition C, Version 1). NATO Standardization Office. Retrieved 25 Jun 26, from https://www.coemed.org/files/stanags/01_AJP/AJP-4.10_EDC_V1_E_2228.pdf

[22] Sargent, R. G. (2010). Verification and validation of simulation models. In *Proceedings of the 2010 Winter Simulation Conference* (pp. 166–183). IEEE. Retrieved 25 Jun 26, from https://www.informs-sim.org/wsc10papers/016.pdf

[23] Hodický, J., Procházka, D., Jersák, R., Stodola, P., & Drozd, J. (2020). Optimization of the casualties' treatment process: Blended military experiment. *Entropy*, *22*(6), 706. Retrieved 25 Jun 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC7517244/

[24] Beldowicz, B.C. (2018). The evolution of damage control in concept and practice. *Clinics in Colon and Rectal Surgery*, *31*(1), 30–35. Retrieved 25 Jun 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC5787400/

[25] Chaudhry, R., Tiwari, G.L., & Singh, Y. (2006). Damage control surgery for abdominal trauma. *Medical Journal, Armed Forces India*, *62*(3), 259–262. Retrieved 25 Jun 26, from https://pmc.ncbi.nlm.nih.gov/articles/PMC4922877/

[26] Law, A.M. (2020). Statistical analysis of simulation output data: the practical state of the art. In *Proceedings of the 2020 Winter Simulation Conference* (pp. 1117–1127). INFORMS Simulation Society. Retrieved 25 Jun 26, from https://informs-sim.org/wsc20papers/134.pdf

[27] Rossetti, M. D. *Simulation Modeling and Arena*, Chapter 5.2–5.3: Replication-Deletion Method and Welch's Graphical Procedure. Retrieved 25 Jun 26, from https://rossetti.github.io/RossettiArenaBook/ch5-RepDeletion.html

[28] Gafarian, A. V., Ancker, C. J., & Morisaku, T. (1978). Evaluation of Commonly Used Rules for Detecting Steady State. *Naval Research Logistics Quarterly*, 25, 511–529.

[29] Banks, J., Carson, J. S., Nelson, B. L., & Nicol, D. M. (2005). *Discrete-Event System Simulation* (4th ed.). Pearson Prentice-Hall.

[30] Pujol, G., Iooss, B., Janon, A., Gilquin, L., Le Gratiet, L., Lemaitre, P., Marrel, A., Meynaoui, A., Nelson, B. L., Monod, H., Fruth, J., Ratto, M., Touati, T., & Weber, F. (2024). *sensitivity: Global Sensitivity Analysis of Model Outputs and Related Quantities*. R package version 1.30.1. Retrieved 25 Jun 26, from https://cran.r-project.org/package=sensitivity

[31] OpenMOLE Community. (2024). *Sensitivity Analysis: Morris Screening Method*. OpenMOLE Documentation. Retrieved 25 Jun 26, from https://openmole.org/Sensitivity.html

[32] Karl, A., Eubank, R., Milovanovic, J., Reiser, M., & Young, D. (2014). Using RngStreams for parallel random number generation in C++ and R. *Computational Statistics*, 29(5), 1301–1320. Open-access preprint retrieved 26 Jun 26, from https://arxiv.org/abs/1403.7645

[33] Rossetti, M. D. (2023). *Simulation Modeling using the Kotlin Simulation Library (KSL)* (open-access, CC BY-NC-ND 4.0), §9.2 Variance Reduction Techniques. Retrieved 26 Jun 26, from https://rossetti.github.io/KSLBook/ch9VRTs.html

[34] R Core Team. (2024). *RNGstreams: L'Ecuyer's RngStreams for parallel random number generation*. R Documentation, parallel package. Retrieved 26 Jun 26, from https://stat.ethz.ch/R-manual/R-patched/library/parallel/html/RngStream.html

---

<!-- REFERENCES END -->
