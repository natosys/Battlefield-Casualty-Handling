# Getting Started

A short guide to using the Battlefield Casualty Handling simulation console. This page covers what you need to run a scenario and read its results — for the full methodology, citations, and model design, see the [README](../README.md).

## What This Tool Does

This app runs a discrete event simulation of casualties moving through a deployed health system — from point of injury, through Role 1 (unit or regimental aid post and its treatment teams) and Role 2 (the forward surgical R2B and the R2E Heavy hospital), to return-to-duty, strategic evacuation, or death. You configure the force size, health system, casualty rates, and evacuation chain; the simulation then generates casualties and routes them through that system, producing wait times, bed/transport utilisation, and outcome counts you can use to judge whether the configured system is adequate.

## The Workflow: Configure → Run → Analyse → Save and Run at Full Protocol

### 1. Configure

Every editable parameter lives in one of six panels: **Force Size**, **Health System Architecture** (team counts, bed capacity), **Medevac** (transport times and fleet size), **Health Provision** (treatment/surgery/recovery durations and thresholds), **Casualty Rates** (arrival distributions, triage split, DOW ceilings), and **Casualty Surge** (surge event injection). Hover the ⓘ icon beside any field for a plain-English explanation and its evidence source.

A **Casualty Intensity Profile** dropdown above the panels switches between the shipped scenario profiles (Moderate Intensity, the shipped default, and High Intensity) without needing to edit fields by hand. *Save Configuration* downloads your edited parameters as a JSON file; *Load Configuration* re-imports one. Neither writes to the app's on-disk configuration, so experimenting here is always safe to undo — just reload the page or re-upload a saved file.

### 2. Run

Set the **Simulation Duration** (days, up to 180 in the console) and **Random Seed** (ships as 42, so every run repeats the same draw; clear the field for a different draw each run), then click **Quick Run**. It runs one replication in the background, ready in well under a minute for a typical 30-day run. Use it as a check that the configuration behaves sensibly. A single replication reflects one random draw and the console records no configuration, seed or commit with it, so do not draw a conclusion from it that you intend to act on or cite.

### 3. Analyse

Results appear as a set of tabs (see below), each showing the Quick Run's single replication. Most plots have PNG, PDF and CSV download buttons, while some outputs (post-operative pathway, transport utilisation, dwell times, DOW and RTD by echelon, AME wait time) offer CSV only, and a *Download All* button zips the raw per-casualty and per-resource monitoring data from the most recent run for offline analysis.

### 4. Save, then run at full protocol

Evidence you can act on or cite comes from the command line, at the replication count and horizon the published experiments use (30 replications of 360 days). The **Run at Full Protocol** card on the Run tab shows the exact command for your configuration:

1. On the Configure tab, type a **Configuration file name** and click *Save Configuration*. The file downloads under that name.
2. Move the file to the repository root, or give its path in the command.
3. Copy the command from the Run tab and run it in a terminal from the repository root, for example `Rscript run.R --config my_config.json --seed 42 --iterations 30 --days 360`.

The saved file already carries the casualty intensity profile you chose, so the command needs no `--scenario`. The run writes its outputs and a `run_manifest.json` (the configuration's hash, the command, the seed, the commit and the package versions) under `outputs/`, so the result can be cited and repeated. The README's [Reproducing the Published Results](../README.md#reproducing-the-published-results) maps each published experiment to its own script.

### 5. (Optional) Sensitivity Screening

The last Analyse tab, **Sensitivity Screening**, is a reference rather than a run. It lists every screened parameter with its plausible range (the same ranges the Configure sliders use), which tells you which parameters are worth the effort of getting right in the first place. Screens themselves run from a terminal with `scripts/run_sensitivity.R` (Morris, and Sobol with `--sobol`), because the default design takes hours and a browser session is the wrong place to hold it. See the README's [Sensitivity Analysis](../README.md#sensitivity-analysis) section for the design and run time.

## Key Parameters Worth Adjusting First

Before exploring the full field set, these are the parameters most likely to change your results materially:

| Parameter | Where | Why it matters |
|---|---|---|
| Casualty Intensity Profile | Configure (top) | Switches the whole casualty-rate and DOW-ceiling basis (e.g. historical exemplar) in one action |
| Simulation Duration (days) | Run | Longer runs reveal steady-state congestion that a short run may not show; the console caps the Quick Run at 180, and the command line runs 360 |
| Random Seed | Run | The field ships as 42 and repeats the same draw; clear it to vary each run, and use that to see how much outcomes vary by chance before trusting a single result |
| Team counts and bed capacity | Configure → Health System Architecture | Directly sets treatment capacity at each echelon — the most direct lever for relieving a queue you see in Analyse |
| Transport fleet size | Configure → Medevac | Relieves a sustained transport queue (see the Transport tab) |

Fields also used in the project's Morris sensitivity screening render as sliders bounded by the range found to matter most — a narrower slider range is a signal that field has been checked against a wider what-if range already.

## Reading the Output Graphs

Each tab's own on-screen text explains what it shows and why it matters; this is a quick reference for what to look for in each.

- **Casualty Flow** — daily casualty count by type/priority. Check this first: it confirms the casualty-generation rates you configured produced the volume and mix you expected, since this demand drives every other tab.
- **Queue Depths** — queue length over time for R1 treatment slots and R2B/R2E beds. A queue that persists or grows identifies an under-resourced team or bed type.
- **Bed & Resource Utilisation** — busy-time fraction per resource; near 100% signals a binding constraint. Also includes R2B hold-bed occupancy by patient stream, why casualties bypassed R2B surgery, and the R2E post-operative ICU-vs-hold-bed pathway.
- **Transport** — evacuation fleet queue and utilisation; a sustained queue means the fleet, not treatment capacity, is the bottleneck.
- **Waiting Times** — per-casualty total wait for treatment against arrival time, plus time-to-first-surgery and echelon dwell/transit KPIs. A rising trend signals growing system-wide congestion. The dwell and transit figures carry a censored percentage: the share of casualties still at that echelon when the run ended, whose stays the mean counts only as far as the run saw them. A high percentage means the run was too short to see those stays out, so read the mean as a floor and lengthen the run.
- **Return to Duty & DOW** — how much of the casualty stream recovered (battle-fatigue vs clinical return-to-duty) versus died of wounds, broken down by echelon.
- **Force Regeneration** — effective force size over time, debited by injury and credited by return-to-duty/reinforcement. A persistently declining line means losses are outpacing recovery and reinforcement.
- **Strategic AME** — aeromedical evacuation backlog and sortie performance, Role 4 (national support base) census and unconstrained bed demand, and actual wait time by route versus the theoretical best case.
- **Casualty Surge Events** — the reconstructed timeline of any configured surge events and whether they carry a higher died-of-wounds rate than background casualties.
- **Sensitivity Screening** — the full list of screened parameters and their plausible ranges (the same ranges the Configure sliders use), and the raw code each is known by in the screening output files. If you run a Morris screen from the command line, look at μ\* (how much a parameter moves the result overall) and σ (how much that effect depends on other parameters' values — high σ means the parameter interacts with others rather than acting alone). If you run a Sobol decomposition, S1 is the share of variance a parameter causes acting alone, ST is its share including every interaction — a parameter with high ST but low S1 is easy to miss with a simpler one-at-a-time check.

## Where to Go for More Detail

This guide covers the workflow; it deliberately leaves out the model's citations, algorithms, and full results narrative. For that, see the [README](../README.md) — in particular the Simulation Design, Model Outputs and Further Development sections for how the model works and what it does not represent — and the companion documents, [Results.md](Results.md) for the measurements and [Planning_Implications.md](Planning_Implications.md) for their reading as planning options.
