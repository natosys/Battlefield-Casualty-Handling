# Making the Treatment Flow User-Adaptable
## Battlefield Casualty Handling Simulation — Design Supplement

**Status:** Design evaluation. Nothing described here is implemented; the options are presented for sequencing against the open issue set.
**Prepared:** September 2026
**Relates to:** `README.md` Further Development L31, Action Plan Issue 4

---

## Purpose

The simulation already allows a planner to change how much of everything there is and how long everything takes, without editing R. What it does not allow is a change to what happens to a casualty and in what order. The echelon sequence, the decision points, the number of arms at each branch, the placement of the died-of-wounds checkpoints and the fallback taken when a bed is full are all expressed as R closures in [`R/trajectories.R`](../R/trajectories.R).

This document evaluates whether the existing configuration mechanism can be extended to cover that second class of change, identifies the seams in the current code that would carry it, and sets out four options with their costs. It reaches no conclusion about the model's clinical behaviour and reports no simulation result.

---

## The mechanism as it stands

Four components make a parameter user-adjustable today.

`env_data.json` holds every configured value. Its `vars` block is a three-level tree of element, activity and variable, carrying 176 leaf values across nine elements. `build_variable_tree()` ([`R/environment.R:315`](../R/environment.R)) flattens it into `env_data$vars$<elm>$<acty>$<var>` for the model to read.

[`R/app_params.R`](../R/app_params.R) is a field registry mapping each editable value to a Configure-panel widget, its bounds, its tooltip and its provenance citation. `build_param_registry()` assembles roughly two hundred field specifications; `apply_registry_values()` writes an edited set back into the parsed tree.

[`R/scenario.R`](../R/scenario.R) overlays a named profile onto `vars` alone. `merge_scenario_vars()` replaces a leaf where the name matches and appends the whole element, activity or value where it does not, so a profile can add structure as well as retune it. The `elms`, `transports` and `pops` blocks are deliberately outside its scope.

[`R/sensitivity.R`](../R/sensitivity.R) screens the continuous subset of those values under Morris elementary effects and Sobol variance decomposition, which is what makes a parameter's influence on the reported outcomes measurable rather than asserted.

---

## Why the mechanism extends further than it appears to

Three properties of the current code, none of them designed for this purpose, together make a configurable treatment flow tractable.

**Leaf values are passed through uninterpreted.** `build_variable_tree()` applies no coercion to a value it reads, and the shipped file already carries arrays (the mass casualty schedule) and strings (`"lognormal"`, `"c17a"`) alongside its numbers. A specification of arbitrary shape can therefore live inside the existing `vars` tree without any schema change, and inherits the scenario overlay, the console's Save and Load Configuration handlers and the structural validator without further work. Anything placed outside `vars` inherits none of those.

**The trajectory file is already decomposed into named single-purpose builders.** Roughly eighty of them exist, one per clinical step or decision, each taking its resources as arguments rather than resolving them itself. The two composition roots, `r2b_treat_wia()` ([`R/trajectories.R:1433`](../R/trajectories.R)) and `r2e_treat_wia()` (`:2483`), are the only places that turn an establishment into resource vectors, which makes them the natural point at which a specification would be consumed. Half of a step registry is therefore already present, and `r2e_surgery_block()` (`:1573`), which takes its section, selection identifier, attribute names, efficacy closure and resources as parameters, is a working example of what a fully parameterised step looks like.

**A structural switch already exists in configuration.** `r2eheavy.icu_gating.p1_bypass_priority_max` is read at `R/trajectories.R:2260` to decide which arm of the R2E surgical branch a casualty takes, and is exposed to the planner as a dropdown. The `high_intensity` profile goes further and changes a distribution family, not merely a distribution's parameters. Selecting behaviour rather than magnitude from configuration is therefore established practice in this codebase rather than a departure from it.

The limit of the mechanism is equally clear. Trajectories are built once per run, from closures that read the configuration globals, so any decision that can be made at build time costs nothing at run time. A change requiring a step type the code does not contain, or an echelon the code does not know about, is a different order of work.

---

## Option 1: widen the parameter surface

The first option moves the clinical values still expressed as literals in R into `vars`, and registers each as a Configure-panel field. Seven substantive cases were identified.

| Literal | Location in `R/trajectories.R` | Candidate parameter |
|---|---|---|
| Priority 1 always proceeds through the R2B surgery gate | `:1091` | An R2B bypass priority threshold, mirroring the R2E field that already exists |
| Intensive care availability tested as an instantaneous count against capacity, never as a reserve | `:889`, `:1092`, `:1841`, `:2254` | An occupancy fraction, as `select_r2b_for_hold()` (`:163`) already applies to holding beds |
| R2B holding queue cap derived from a fixed bed-share formula | `:1268` | A configured queue cap share |
| No bound on how long entry to theatre may be deferred pending a bed | `:1102`, `:1889` | A maximum defer period |
| Vehicle type fixed per movement leg, by literal key into the transport block | `:517`, `:591`, `:661` | A transport key per leg |
| Two different selection policies applied to the same R2B holding pool | `:1126` against `:1194` | One configured policy, the divergence appearing unintentional |
| Critical strategic evacuation restricted to treated Priority 1 casualties | `:2380` | A priority threshold |

Each is small, independent and suited to its own issue, and each new field is continuous and therefore screenable alongside the existing parameter set. The option changes no topology: it widens the space of configurations a planner can express without adding any new shape to it.

---

## Option 2: structural policy switches

The second option introduces a `pathway` activity for each element, whose leaf values are enumerated policy choices read at build time. Because the chosen arm is composed and the alternatives are never constructed, a switch costs nothing at run time and consumes no random draw. Five candidates follow directly from the branch inventory.

A Role 1 forward routing switch would let Role 1 evacuate directly to Role 2 Enhanced, representing a laydown with no Role 2 Basic. A Role 2 Basic surgical switch would reduce that echelon to a non-surgical holding and evacuation function. An intensive-care-full policy switch would choose between the holding-bed fallback, the deferral poll and forward diversion, where the code currently decides between the first two by priority alone. A died-of-wounds checkpoint list would name which of the five checkpoints are live, replacing five hardcoded sites and their literal echelon codes. A roster pattern parameter would generalise the two-shift alternation in `build_env()` ([`R/environment.R:1144`](../R/environment.R)), which is currently a counter taken modulo two, into a shift count with a per-section assignment.

Two consequences follow and should be accepted deliberately. A structural switch is not a continuous quantity, so it cannot be screened under Morris or Sobol, and evidence for its effect has to come from replicated comparison runs of the kind the scenario runner already performs. Each switch also adds an arm to the trajectory diagrams in `README.md`, which the project requires to correspond exactly to the code.

The vocabulary this option offers is curated rather than open, which is its principal virtue: every configuration a planner can express is one that a clinician can be asked to review.

---

## Option 3: a declarative care pathway specification

The third option places a pathway specification in `vars`: named nodes referencing step types drawn from a registry built by wrapping the existing builders, decision nodes carrying guards drawn from a restricted predicate vocabulary of attribute comparisons, resource availability tests and Bernoulli draws, and a compiler in a new module turning the specification into simmer trajectories. It is the only option under which a user can add, remove or reorder an echelon.

It is also the largest structural change the project has attempted, and its cost is roughly double what the trajectory file alone suggests, for three reasons.

The analysis pipeline is keyed to the current clinical vocabulary. [`R/analysis.R`](../R/analysis.R) carries 54 references to the fixed attribute names by which a casualty's treatment history is read, so an echelon a user adds would produce no output at all until those keys were themselves made data-driven.

The simmer selection identifiers are allocated by convention rather than by code. Ten of them appear as bare integers at roughly thirty call sites, with no constant and no table. A compiler must allocate them, which means the convention has to be replaced before the compiler can exist.

The configuration globals recorded as Further Development entry L31 become load-bearing. A compiler resolving its specification from the global environment rather than receiving it as an argument would be difficult to test in isolation, so threading the configuration through as an argument stops being a tidiness improvement and becomes a precondition.

One by-product is worth recording. Given a specification, the trajectory diagrams in `README.md` could be generated from it rather than maintained by hand, which would close the standing risk that a diagram and the code it describes disagree.

---

## Option 4: an expert escape hatch

The fourth option adds a command-line argument sourcing a user-supplied R file after `R/trajectories.R`, overriding builders by name. It is perhaps twenty lines of work and gives a researcher complete freedom immediately.

It is not a substitute for the options above. It requires the user to write R, it cannot be offered through the Shiny console because it executes arbitrary code supplied to a web application, and a run it produces is not reproducible from a configuration file alone, which places it outside the provenance discipline the rest of the project observes. It is worth keeping as a documented research facility and not as the answer to the requirement.

---

## Recommended sequence

Options 1, 2 and 3 are stages of one progression rather than alternatives, and the recommended order is Option 1, then Option 2, with Option 3 gated behind Further Development entry L31 and Action Plan Issue 4. Issue 4 rewrites the same trajectory internals that a pathway compiler would rewrite, so undertaking the two separately means undertaking the harder of them twice. Option 4 may be added at any point, independently of the others.

Two constraints carry over from the project's own precedent and apply to every stage.

Each new structure must reproduce the seed-42 baseline byte for byte at its default setting. The established technique is a default that consumes no random draw, demonstrated by the all-damage-control equivalence under Issue 173 and by the zero-length pre-open window that `scripts/check_pre_open_window.R` asserts. `scripts/check_baseline_reproduction.R` is the gate.

Each switch requires its own regression check asserting that its non-default settings reach the model, on the pattern of `scripts/check_lever_realisation.R`. The check suite is discovered by glob, so a committed check gates from the moment it is committed.

---

## Files a first-stage implementation would touch

`env_data.json` gains values under existing activities and, for Option 2, new `pathway` activities. No schema change is required.

`R/trajectories.R` loses the literal sites listed above; a switch is read at the two composition roots, which are already the only functions resolving resources.

`R/environment.R` gains semantic validation of the enumerated values in `validate_env_data_json()` (`:96`), on the pattern of `resolve_ame_airframe()` (`:410`). One defect is worth repairing at the same time: that validator is reached only from the Shiny console, and the command-line entry points parse the configuration file without calling it.

`R/app_params.R` gains one field per new parameter. Any generated field set must be built with `lapply` rather than a loop, and must force its path arguments, for the reason recorded at `R/app_params.R:428`.

`README.md` requires its Simulation Design narrative and its three trajectory diagrams updated, and `docs/Getting_Started.md` its Configure panel field list. Each new regression check joins `scripts/` and the runtime table the suite runner schedules from.

---

## What this document does not settle

The clinical validity of any pathway a user might configure is outside its scope. Option 2 constrains the space to configurations that can be reviewed, but neither it nor Option 3 provides any assurance that a configured pathway is doctrinally sound, and Option 3 in particular would allow a planner to describe a health system that no force could field. Whether that freedom should be bounded, and by what, is a question for the model's intended users rather than for its implementation.

Nor does this document estimate effort in hours. The relative ordering of the four options is well supported by the code as it stands; the absolute cost of Option 3 in particular depends on decisions about the analysis pipeline that have not been taken.
