# Getting Started

A short guide to using the Battlefield Casualty Handling simulation console. This page covers what you need to run a scenario and read its results — for the full methodology, citations, and model design, see the [README](../README.md).

## What This Tool Does

This app runs a discrete event simulation of casualties moving through a deployed health system — from point of injury, through Role 1 (forward aid post) and Role 2 (battalion aid post / field hospital), to return-to-duty, strategic evacuation, or death. You configure the force size, health system, casualty rates, and evacuation chain; the simulation then generates casualties and routes them through that system, producing wait times, bed/transport utilisation, and outcome counts you can use to judge whether the configured system is adequate.

## The Workflow: Configure → Run → Analyse

### 1. Configure

Every editable parameter lives in one of six panels: **Force Size**, **Health System Architecture** (team counts, bed capacity), **Medevac** (transport times and fleet size), **Health Provision** (treatment/surgery/recovery durations and thresholds), **Casualty Rates** (arrival distributions, triage split, DOW ceilings), and **Mass Casualty** (surge event injection). Hover the ⓘ icon beside any field for a plain-English explanation and its evidence source.

A **Casualty Intensity Profile** dropdown above the panels switches between the shipped scenario profiles (e.g. Falklands — Modified, Falklands — Unmodified, Okinawa — Casualty Rates) without needing to edit fields by hand. *Save Configuration* downloads your edited parameters as a JSON file; *Load Configuration* re-imports one. Neither writes to the app's on-disk configuration, so experimenting here is always safe to undo — just reload the page or re-upload a saved file.

### 2. Run

Set the **Simulation Duration** (days) and **Random Seed** (leave blank for a different draw each run), then choose a mode:

- **Quick Run** — one replication, ready in well under a minute for a typical 30-day run. Good for a fast first look, or for tuning parameters interactively.
- **Full Analysis** — runs many independent replications (slider, 10–1000) and reports mean values with 95% confidence intervals. Use this before drawing any conclusion you intend to act on — a single Quick Run reflects one random draw and can be misleading on its own.

Both run in the background so the app stays responsive; a progress indicator tracks real completion for Full Analysis.

### 3. Analyse

Results appear as a set of tabs (see below). Full Analysis mode adds four KPI summary cards above the tabs (total casualties, DOW count, R2E ICU peak queue, R2B OT peak queue, each with a 95% CI) and shows every plot as a mean ± CI band rather than a single trace. Every plot and table has PNG/PDF/CSV download buttons, and a *Download All* button zips the raw per-casualty and per-resource monitoring data from the most recent run for offline analysis.

## Key Parameters Worth Adjusting First

Before exploring the full field set, these are the parameters most likely to change your results materially:

| Parameter | Where | Why it matters |
|---|---|---|
| Casualty Intensity Profile | Configure (top) | Switches the whole casualty-rate and DOW-ceiling basis (e.g. historical exemplar) in one action |
| Simulation Duration (days) | Run | Longer runs reveal steady-state congestion that a short run may not show |
| Random Seed | Run | Blank seed varies each run — use this to see how much outcomes vary by chance before trusting a single result |
| Replication Count | Run | More replications narrow the 95% CI on Full Analysis output — 100 is a reasonable default; use more before a planning-grade report |
| Team counts and bed capacity | Configure → Health System Architecture | Directly sets treatment capacity at each echelon — the most direct lever for relieving a queue you see in Analyse |
| Transport fleet size | Configure → Medevac | Relieves a sustained transport queue (see the Transport tab) |

Fields also used in the project's Morris sensitivity screening render as sliders bounded by the range found to matter most — a narrower slider range is a signal that field has been checked against a wider what-if range already.

## Reading the Output Graphs

Each tab's own on-screen text explains what it shows and why it matters; this is a quick reference for what to look for in each.

- **Casualty Flow** — daily casualty count by type/priority. Check this first: it confirms the casualty-generation rates you configured produced the volume and mix you expected, since this demand drives every other tab.
- **Queue Depths** — queue length over time for R1 treatment slots and R2B/R2E beds. A queue that persists or grows identifies an under-resourced team or bed type.
- **Bed & Resource Utilisation** — busy-time fraction per resource; near 100% signals a binding constraint. Also includes R2B hold-bed occupancy by patient stream, why casualties bypassed R2B surgery, and the R2E post-operative ICU-vs-hold-bed pathway.
- **Transport** — evacuation fleet queue and utilisation; a sustained queue means the fleet, not treatment capacity, is the bottleneck.
- **Waiting Times** — per-casualty total wait for treatment against arrival time, plus time-to-first-surgery and echelon dwell/transit KPIs. A rising trend signals growing system-wide congestion.
- **Return to Duty & DOW** — how much of the casualty stream recovered (battle-fatigue vs clinical return-to-duty) versus died of wounds, broken down by echelon.
- **Force Regeneration** — effective force size over time, debited by injury and credited by return-to-duty/reinforcement. A persistently declining line means losses are outpacing recovery and reinforcement.
- **Strategic AME** — aeromedical evacuation backlog and sortie performance, Role 4 (national support base) census and unconstrained bed demand, and actual wait time by route versus the theoretical best case.
- **Mass Casualty Events** — the reconstructed timeline of any configured surge events and whether they carry a higher died-of-wounds rate than background casualties.
- **Sensitivity Calibration** — the Morris-screened parameter list and, if you run one, live Morris/Sobol sensitivity screening and transport fleet sweep results.

## Where to Go for More Detail

This guide covers the workflow; it deliberately leaves out the model's citations, algorithms, and full results narrative. For that, see the [README](../README.md) — in particular the Simulation Design, Model Outputs, and Simulation Analysis sections, and the Limitations section for what the model does not represent.
