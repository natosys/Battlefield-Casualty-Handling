# Getting Started

A short guide to using the Battlefield Casualty Handling simulation console. This page covers how to build a configuration and run it; for the full methodology, citations, and model design, see the [README](../README.md).

## What This Tool Does

The model is a discrete event simulation of casualties moving through a deployed health system: from point of injury, through Role 1 (unit or regimental aid post and its treatment teams) and Role 2 (the forward surgical R2B and the R2E Heavy hospital), to return-to-duty, strategic evacuation, or death. This console is where you configure it: the force size, health system, casualty rates, and evacuation chain. It runs nothing itself. A run belongs on the command line, where it records the configuration, seed and commit behind its results so they can be cited and repeated.

## The Workflow: Configure → Save → Run from the Command Line

### 1. Configure

Every editable parameter lives in one of six panels: **Force Size**, **Health System Architecture** (team counts, bed capacity), **Medevac** (transport times and fleet size), **Health Provision** (treatment/surgery/recovery durations and thresholds), **Casualty Rates** (arrival distributions, triage split, DOW ceilings), and **Casualty Surge** (surge event injection). Hover the ⓘ icon beside any field for a plain-English explanation and its evidence source.

A **Casualty Intensity Profile** dropdown above the panels switches between the shipped scenario profiles (Moderate Intensity, the shipped default, and High Intensity) without needing to edit fields by hand. *Load Configuration* re-imports a saved file. Nothing here writes to the app's on-disk configuration, so experimenting is always safe to undo: reload the page or re-upload a saved file.

### 2. Save

On the Configure tab, type a **Configuration file name** and click *Save Configuration*. The file downloads under that name and already carries the casualty intensity profile you chose.

### 3. Run from the command line

The **Run** tab shows the exact command for your configuration, at the replication count and horizon the published experiments use (30 replications of 360 days). Set the **Random Seed** there (it ships as 42), then:

1. Move the saved file to the repository root, or give its path in the command.
2. Copy the command from the Run tab and run it in a terminal from the repository root, for example `Rscript run.R --config my_config.json --seed 42 --iterations 30 --days 360`.

The run writes its outputs and a `run_manifest.json` (the configuration's hash, the command, the seed, the commit and the package versions) under `outputs/`. The README's [Model Outputs](../README.md#model-outputs) section describes each output, and [Reproducing the Published Results](../README.md#reproducing-the-published-results) maps each published experiment to its own script.

## Key Parameters Worth Adjusting First

Before exploring the full field set, these are the parameters most likely to change your results materially:

| Parameter | Where | Why it matters |
|---|---|---|
| Casualty Intensity Profile | Configure (top) | Switches the whole casualty-rate and DOW-ceiling basis (e.g. historical exemplar) in one action |
| Random Seed | Run | Sets the seed in the displayed command; the replications draw their own seeds from it, so the same seed repeats the same campaign set |
| Team counts and bed capacity | Configure → Health System Architecture | Directly sets treatment capacity at each echelon, the most direct lever for relieving a queue a run reports |
| Transport fleet size | Configure → Medevac | Relieves a sustained transport queue |

Fields with a plausible range render as sliders bounded by that range, widened to admit any loaded value outside it.

## Where to Go for More Detail

This guide covers the workflow; it deliberately leaves out the model's citations, algorithms, and results. For that, see the [README](../README.md), in particular the Simulation Design, Model Outputs and Further Development sections for how the model works and what it does not represent, and the companion documents, [Results.md](Results.md) for the measurements and [Planning_Implications.md](Planning_Implications.md) for their reading as planning options.
