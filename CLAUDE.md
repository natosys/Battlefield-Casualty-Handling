# CLAUDE.md — Battlefield Casualty Handling Simulation

## Project Purpose

This is an **academic research project** producing a Discrete Event Simulation (DES) of deployed battlefield casualty handling. The simulation is written in R using the `simmer` package and is intended to provide evidence-based options to military planners for improving health outcomes in Large Scale Combat Operations (LSCO).

All work must meet academic research standards: reasoning must be explicit, sources must be cited, and limitations must be acknowledged. The project's academic output is split across three documents, each kept current with the code and written to the standard of a published academic paper: `README.md` (system reference — code structure, algorithms, trajectory logic, resource model, inline model assumptions, and Limitations), `docs/Single_Run_Analysis.md` (the illustrative single-run, seed-42 results narrative), and `docs/Multi_Run_Analysis.md` (the multi-run, n≥30/95% CI comparative results narrative). See [README Maintenance](#readme-maintenance) below for which PR types update which document.

---

## Repository Structure

The codebase is organised into a modular layout under `R/`, with `run.R` as the single CLI entry point. See the README's [Codebase Structure](README.md#codebase-structure) table for full detail on each `R/` module; this table covers the repository as a whole.

| File / Directory | Purpose |
|---|---|
| `run.R` | CLI entry point — parses arguments, orchestrates modules, and writes outputs |
| `R/environment.R` | Data import, arrival generation, and simmer environment construction |
| `R/trajectories.R` | All simmer `trajectory()` definitions — R1, R2B, R2E, and core casualty flow |
| `R/replication.R` | Multi-run replication framework (`run_once`, `run_replications`, `summarise_replications`) |
| `R/analysis.R` | Analysis and visualisation pipeline (`analyse_run`) |
| `R/sensitivity.R` | Morris EE screening and Sobol variance decomposition |
| `R/warmup.R` | Welch warm-up analysis |
| `R/app_params.R` | Parameter registry for the Shiny Configure panel |
| `R/scenario.R` | Scenario overlay mechanism (`resolve_scenario`, `merge_scenario_vars`); the profiles themselves are defined in `env_data.json`'s `scenarios` block |
| `R/scenario_runner.R` | Comparative scenario runner — executes the replication framework under a named scenario profile |
| `app.R` | Shiny console — Configure/Run/Analyse workflow for interactive `env_data.json` parameter editing, Quick Run, Full Analysis (multi-run with 95% CI), and Sensitivity Screening (Morris/Sobol) execution (Issues #14, #15) |
| `env_data.json` | All simulation parameters — populations, resources, distributions, schedules |
| `scripts/run_sensitivity.R` | CLI entry point for sensitivity analysis |
| `scripts/run_warmup.R` | CLI entry point for Welch warm-up analysis |
| `scripts/run_scenarios.R` | CLI entry point for the comparative scenario runner |
| `scripts/screen_cache.sh` | Checkpoints a sensitivity screen's point cache onto its own git ref and restores it, so a multi-hour screen survives an environment that reclaims its filesystem mid-run |
| `scripts/supervise_screen.sh` | Drives a long screen to completion across environment failures, restoring the cache before each attempt and checkpointing while the screen runs |
| `scripts/run_transport_sweep.R` | CLI entry point for the transport fleet-size sweep |
| `scripts/run_icu_share_sweep.R` | CLI entry point for the forward ICU share (R2B post-operative stabilisation) sweep |
| `scripts/shiny_worker.R` | Background worker sourced by `app.R` for async Quick Run / Full Analysis execution |
| `scripts/check_env_data_summary.R` | Regenerates the `<!-- ENV SUMMARY -->` block inside `README.md` from `env_data.json` |
| `scripts/check_markdown.R` | Maintains the TOC and "Return to Top" links across `README.md`, `docs/Single_Run_Analysis.md`, and `docs/Multi_Run_Analysis.md`, generating each anchor as GitHub does; exits non-zero if any anchor link points at no heading, checked across every tracked markdown document including this one and `docs/BCH_Simulation_Action_Plan.md` (which carry no TOC block and must not be given one) |
| `scripts/check_r2e_surgery_seizure.R` | Regression check asserting that every R2E surgery seizes a surgical section, structurally and behaviourally; exits non-zero on failure |
| `scripts/check_icu_time_conservation.R` | Regression check asserting that a casualty's post-operative ICU requirement is conserved across all three routes and at every forward ICU share; exits non-zero on failure |
| `scripts/check_composition_ilr.R` | Regression check asserting that each simplex-constrained composition group stays on the simplex through its screened balance coordinates; exits non-zero on failure |
| `scripts/check_morris_baseline.R` | Regression check asserting that every screened parameter's baseline lies inside its own screening bounds and equals the value it holds in `env_data.json`; exits non-zero on failure |
| `scripts/check_dow_calibration.R` | Regression check asserting that each shipped configuration's treated-cohort died-of-wounds rate agrees with the historical anchor of the campaign it models, the Ajax Bay bound for the two Falklands-calibrated configurations and the reported Okinawa rate for `high_intensity`, pooling independent measurements; exits non-zero on failure |
| `scripts/check_mass_casualty_kia_split.R` | Regression check asserting that a mass casualty event's casualty count is conserved across the wounded/killed split, that the realised killed share tracks the configured one, that an event's killed reach mortuary handling untriaged, and that the share reaches nothing while injection is disabled; exits non-zero on failure |
| `scripts/check_lever_realisation.R` | Regression check asserting that two configured planner levers are applied in full: that every person of a reinforcement fill joins the population even where that carries a pool over establishment strength, and that a casualty evacuated from R2B holding under `evac_threshold` serves the remainder of the convalescence already drawn rather than a fresh draw; exits non-zero on failure |
| `scripts/check_replication_independence.R` | Regression check asserting that `run_once()` is a pure function of its seed and that `run_replications()` draws a distinct seed per replication, the two properties that make replications independent; exits non-zero on failure |
| `scripts/check_screen_cache.R` | Regression check asserting that a sensitivity screen's design-point cache resumes what it recorded: a complete row round-trips, a partially-missing row reads as present with its gaps preserved, an all-missing row reads as absent, and an uncached point or a foreign cache reads as absent; exits non-zero on failure |
| `scripts/check_measurement_reproducibility.R` | Regression check asserting that a multi-replication measurement is a function of its control seed alone: that it repeats at that seed, that it is unaffected by what preceded it in the session, that `run_replications()` restores the caller's generator kind and stream position, and that a replication reproduces from its seed on either dispatch path; exits non-zero on failure |
| `scripts/check_scenario_labels.R` | Regression check asserting that the comparative scenario plot renders in a C locale and is byte-identical to the same plot rendered under UTF-8; exits non-zero on failure |
| `scripts/check_pre_open_window.R` | Regression check asserting that a zero R2B pre-open hold window reproduces the instant-diversion model bit-for-bit, that `minutes_to_shift_open()` agrees with the roster, and that every casualty held forward is operated on there; exits non-zero on failure |
| `README.md` | System reference — introduction, literature review, methodology, codebase structure, trajectory logic, resource model, Mermaid diagrams, inline model assumptions, limitations, references. Does not contain simulation results. |
| `docs/Single_Run_Analysis.md` | Illustrative single-run (seed 42, 30-day) results narrative under the Falklands-modified baseline — the project's original per-echelon results walk-through |
| `docs/Multi_Run_Analysis.md` | Multi-run (n≥30 replications, 95% CI) comparative results narrative — Falklands-modified vs. Okinawa-intensity scenario comparison |
| `docs/BCH_Simulation_Action_Plan.md` | Issue tracker cross-reference — phase sequencing, dependency graph, merged-issue log |
| `docs/BCH_Task_Role_Allocation.md` | Task-role allocation design supplement for the not-yet-implemented individual resource modelling work (Issue #4) |
| `docs/STYLE_GUIDE.md` | R code style conventions — follow at all times |
| `data/` | Read-only input data (arrival schedules) plus the tracked seed-42 diagnostic/event files (`arrivals_*.txt`, `mass_casualty_events.csv`) written by `R/environment.R`, rewritten only under `run.R --refresh-baseline` |
| `images/` | Tracked seed-42 baseline plots and reference diagrams, regenerated as part of baseline-affecting PRs via `run.R --refresh-baseline` |
| `logs/` | Tracked seed-42 baseline console log (`logs.txt`), regenerated as part of baseline-affecting PRs via `run.R --refresh-baseline` |
| `outputs/` | Gitignored destination for every ordinary run's artifacts: CSV/markdown outputs, plots (`outputs/images/`), console log, and arrival diagnostics (`outputs/data/`); tracked via `.gitkeep` only |
| `renv/`, `renv.lock`, `.Rprofile` | R package environment management |
| `.devcontainer/` | Pinned Dev Container definition (`rocker/rstudio:4.4.2`) used for canonical baseline runs |

---

## Development Workflow

### Branch Rules

- **All development happens on feature branches.** Never commit directly to `main`.
- **Only the repository owner can merge to `main`.** Do not merge to `main` directly. Always open a PR and await owner merge.
- **Always open a PR at the end of each issue.** Use the GitHub MCP tools (`mcp__github__create_pull_request`) to create the PR with a test plan in the description before handing over. Never ask the user to merge via git commands — they merge through GitHub.
- Branch naming: `feature/issue-<number>-<short-description>` (e.g., `feature/issue-1-multi-run-replication`).
- Each GitHub Issue corresponds to one feature branch and one PR.

### Sequence

1. Raise a GitHub Issue describing the work (see Issue Format below).
2. Create a feature branch from `main`.
3. Implement the changes.
4. Update the relevant document(s) — `README.md` and/or the `docs/` analysis documents — as part of the same PR (see README Maintenance below).
5. Open a PR against `main` with a test plan (see Test Plans below).
6. Await owner merge — do not self-merge.

### Post-Merge Checklist

After the repository owner merges a PR to `main`, perform the following tasks on a new chore branch (`chore/post-pr<N>-action-plan-update`) and open a follow-up PR:

**1. Update `docs/BCH_Simulation_Action_Plan.md`**

| Location in document | What to do |
|---|---|
| Summary table | Change the issue's Status from `Open` → `**Merged (PR #N)**` |
| "Issues In Review" section | Remove the merged issue's entry; if the section is now empty, restore the placeholder: `*No PRs currently open against main.*` |
| "Recently Merged Issues" section | Add a new entry (see format below) above the previous most-recent entry |
| Phase sequence list | Strike through the item with `~~double tildes~~`. An issue raised after its phase's list was written has no item to strike, so add one at its position in merge order, numbered with a letter suffix on the item it follows (`6a`, `15b`); re-letter the items after it if merge order requires. Add the issue to the roster in the phase heading at the same time |
| Dependency graph | Move the issue node from UNBLOCKED to COMPLETE; move any newly unblocked issues from BLOCKED to UNBLOCKED |
| Footer | Update the "last updated" date |

Recently Merged Issues entry format:
```
### Issue N — <Title> ✓

**Merged:** PR #N, branch `<branch-name>`

<One paragraph describing what was implemented and how it works.>

**Seed-42 baseline (30 days, single run):** <Include a table of changed metrics if the merge altered simulation outputs. Omit this block for documentation-only changes.>

**Unblocked by this merge:** <List newly unblocked issues, or "No new issues unblocked.">
```

**2. Update GitHub issue labels**

For each issue newly unblocked by the merge: change its label from `status: blocked` to `status: ready` using the GitHub MCP tools.

**3. Update `CLAUDE.md` baseline table (if simulation outputs changed)**

If the merged PR modified `R/trajectories.R`, `R/environment.R`, or `env_data.json` in a way that shifts the RNG stream or alters stochastic outputs, re-run the simulation at seed 42 and update the Key Parameters table at the bottom of this file. Document the change in the action plan entry.

The re-run must be invoked with the `--refresh-baseline` flag, which is the only way to write the tracked baseline evidence set (`images/`, `logs/logs.txt`, `data/arrivals_*.txt`, `data/mass_casualty_events.csv`):

```sh
Rscript run.R --seed 42 --days 30 --iterations 1 --refresh-baseline
```

Without the flag, every run writes to `outputs/` alone and leaves all tracked artifacts untouched, so an exploratory or smoke-test run cannot corrupt the baseline. The flag requires `--iterations 1` and errors otherwise, because the console log and the arrival diagnostics have no multi-replication equivalent; this is what guarantees the three tracked sets always describe the same single run. Commit them together, as one commit, or not at all: a PR that regenerates only part of the set reintroduces the drift Issue #154 closed.

**4. Regenerate the README environment summary (if `env_data.json` changed)**

If the merged PR modified `env_data.json`, run `scripts/check_env_data_summary.R` to refresh the `<!-- ENV SUMMARY START/END -->` block inside `README.md` and include the updated `README.md` in the chore PR.

---

### Commit Messages

Commits should be clear and descriptive. Reference the issue number:

```
feat(issue-1): activate mclapply replication wrapper with wrap() aggregation

Replaces single-run execution with 1000-replication parallel framework.
All KPI outputs now report mean ± 95% CI across replications.

Closes #1
```

---

## Issue Format

Use the following hybrid format when raising GitHub Issues. It captures both the academic rationale and the engineering task list.

```markdown
## Problem Statement

<Describe what is wrong or missing in the current model. Include the clinical or operational consequence
of the gap — not just the code symptom. Cite literature where the basis for the problem is established.>

## Operational / Clinical Rationale

<Explain why this matters for health outcomes or planner decision-making. Reference doctrine,
historical data, or published evidence. Prioritise open-access sources.>

## Recommended Approach

<Describe the implementation approach at a conceptual level. Reference the method or algorithm chosen
and its basis in literature. Include any key design decisions.>

## Implementation Tasks

- [ ] Task 1
- [ ] Task 2
- [ ] ...

## Acceptance Criteria

- [ ] Criterion 1 (observable output change)
- [ ] Criterion 2
- [ ] ...

## References

- Author (Year). Title. Source. URL
```

---

## Issue Annotation System

All GitHub Issues use a consistent annotation system to make phase, type, and sequencing visible in the issue list without opening each issue.

### Title prefix format

Every issue title opens with a prefix in square brackets:

```
[Ph.N] Title of issue
[Ph.N · BUG] Title of bug issue
[HOTFIX · Ph.N] Title of pre-phase bug fix
```

| Prefix | When to use |
|---|---|
| `[Ph.1]` through `[Ph.5]` | Standard feature or analysis work in the named phase |
| `[Ph.N · BUG]` | A bug found within a phase that can wait for that phase |
| `[HOTFIX · Ph.N]` | A bug that must ship before its phase begins — no dependencies |

Do not include `READY` or `BLOCKED` in the title; those are maintained as labels (see below).

### Labels

All labels are applied on the repository. Use them as follows when raising new issues:

**Phase labels** — one per issue, matching the title prefix:
`phase/1 · statistical-foundation`, `phase/2 · model-fidelity`, `phase/3 · structural-refactor`, `phase/4 · scenario-expansion`, `phase/5 · interface`

**Type labels** — one per issue:
`bug` (defects in existing behaviour), `enhancement` (new capability or improvement)

**Status labels** — maintained as work progresses; update when dependencies are resolved:
`status: ready` (no blocking dependencies), `status: blocked` (has unresolved dependencies)

**Priority labels** — apply when the issue warrants it:
`priority: critical` (bug that invalidates current output), `priority: high` (blocks multiple other issues)

### Raising new issues

When a new issue is raised:
1. Assign the correct `[Ph.N]` prefix to the title.
2. Apply phase, type, status, and priority labels.
3. Set `status: ready` if it can be started immediately; `status: blocked` if it depends on open issues.
4. When a blocking issue merges, update the `status` label on all issues it unblocks.

---

## Test Plans

Every PR must include a **Documented Manual Test Plan** in the PR description. There is no automated test framework; verification is by documented manual execution.

Test plans must include:

1. **Setup** — seed, run duration, any parameter changes required to observe the behaviour under test.
2. **Steps** — numbered list of actions to execute.
3. **Expected outputs** — specific, observable values or patterns (e.g., "mean R2E ICU queue across replications should be non-zero and vary between replications").
4. **Regression checks** — confirm that outputs from unmodified pathways remain consistent with the baseline single-run (seed 42) values documented in `docs/Single_Run_Analysis.md` and this file's Key Parameters table.
5. **Known limitations** — anything the test plan does not cover, and why.

Example entry:

```
### Test: Multi-replication output (Issue 1)
**Setup:** n_iterations = 10, n_days = 30, seed = NULL (independent per replication)
**Steps:**
1. Source `run.R`
2. Inspect `queue_summary` output object
3. Confirm 10 rows present in replication-level resource monitor output
**Expected:** `mean_queue` values differ across replications; p10 < mean < p90 for at least one resource
**Regression:** Total casualty count per replication should fall within ±15% of seed 42 baseline (401 casualties)
```

---

## README Maintenance

The project's academic output is split across three documents (see [Academic Standards](#academic-standards) intro and the Repository Structure table above):

- **`README.md`** (system reference) — code structure, algorithms, trajectory logic, resource model, Mermaid diagrams, inline model assumptions, and Further Development. Contains no simulation results.
- **`docs/Single_Run_Analysis.md`** — the illustrative single-run (seed 42, 30-day) results narrative under the Falklands-modified baseline.
- **`docs/Multi_Run_Analysis.md`** — the multi-run (n≥30 replications, 95% CI) comparative results narrative (Falklands-modified vs. Okinawa-intensity).

All three must be updated **as part of every PR that touches the section(s) they own** — not retrospectively. A PR that only changes code structure or trajectory logic updates `README.md` alone; a PR that changes seed-42 single-run findings updates `docs/Single_Run_Analysis.md`; a PR that changes multi-run/scenario-comparison findings updates `docs/Multi_Run_Analysis.md`. Cross-references between the three documents (`[text](../README.md#anchor)`, `[text](docs/Single_Run_Analysis.md#anchor)`, `[text](docs/Multi_Run_Analysis.md#anchor)` as appropriate to the source document's location) must stay valid — re-run `scripts/check_markdown.R` after moving or renaming any heading referenced from another document.

### What to update per PR

| Document | Section | Update trigger |
|---|---|---|
| `README.md` | Abstract | When the scope of the codebase or system reference changes materially |
| `README.md` | Simulation Design | When trajectories, resource logic, or distributions are changed |
| `README.md` | Further Development | When a gap is closed (delete the entry) or a new one is identified (add one, with a new identifier) |
| `README.md` | References | Add any new sources used in the implementation that `README.md` itself cites |
| `docs/Single_Run_Analysis.md` | Relevant echelon/domain section | When new seed-42 single-run results are generated (replace or supplement existing analysis) |
| `docs/Single_Run_Analysis.md` | References | Add any new sources this document itself cites |
| `docs/Multi_Run_Analysis.md` | Comparative Scenario Analysis | When new multi-run/scenario-comparison results are generated (replace or supplement existing analysis) |
| `docs/Multi_Run_Analysis.md` | References | Add any new sources this document itself cites |

Each document's References section lists only the sources that document itself cites, numbered in order of first appearance within that document — not a shared numbering scheme across all three. A source cited in more than one document is renumbered independently in each.

### Style

- Write in academic third-person prose. Avoid first person.
- **Write at a post-graduate research level that stays accessible to non-experts.** Use clear, plain prose and only standard dictionary words; do not coin non-standard terms (e.g. write "has not undergone surgery," not "unsurgicated").
- **Refer to people in the model as casualties, not "candidates."** "Candidate" is reserved for its other established uses in this project (a screened parameter, a scheduled day, a proposed intervention); a casualty being assessed or eligible for surgery is a "casualty requiring surgery" or "Priority N casualty," never a "surgical candidate" or "Priority N candidate."
- All parameters, probabilities, and distributions must be cited to their source.
- New methods introduced must reference the algorithm or statistical technique by name, with citation (e.g., "Morris Elementary Effects screening (Morris, 1991) was applied using R's `sensitivity` package").
- Tables and flowcharts must be kept synchronised with the code.
- **Do not use em dashes** in new or edited prose across `README.md`, `docs/Single_Run_Analysis.md`, and `docs/Multi_Run_Analysis.md`. Use commas, parentheses, or semicolons instead.
- **Simulation Design narrative sections describe only the current design.** Trajectory logic, algorithm, and resource-model sections state how the model works now, with supporting evidence (citations, code function names, computed figures), not how it used to work or which issue changed it (e.g. no "prior to Issue #N..." or "as of Issue #N..." framing, and no issue-number suffix on section/heading titles). This does not apply to the Limitations section or `docs/BCH_Simulation_Action_Plan.md`, which are required elsewhere in this document to track which issue addressed or introduced a given item.
- **Mathematical notation** uses LaTeX delimiters exclusively (`$...$` inline, `$$...$$` for display formulas), never a code fence or plain text, for a formula or a mathematical variable (e.g. `$p_{max}$`, not `p_max` or *p_max*). An actual code, attribute, or `env_data.json` identifier (e.g. `` `dow_ceiling` ``, `` `p1_p_max` ``) is set in backticks, not math notation, even where its name coincides with a formula's symbol.
- **Figure captions** are written as ordinary prose immediately following the image, not as a separate italicised "*Figure: ...*" note.
- **Avoid duplicating content** already documented elsewhere in the same document, or, per the cross-reference rule above, in one of the other two documents; cross-reference the existing location instead of restating it. Every fact has exactly one home. The common failure is stating the same fact in two sections of the same document because both are about the thing it describes (for example a resource's concurrency limit appearing in both the roster section and the trajectory section). Put a fact where a reader would look for it first, and cross-reference from the other place.
- **Match the length of what surrounds the edit.** A new paragraph should be about as long as its neighbours in the same section; a new table row about as long as the other rows. Adding a paragraph that is twice the length of every other paragraph around it makes the document harder to read even when every sentence in it is accurate, and is a reliable sign that it is explaining something twice or explaining something the code already states. Check the actual lengths rather than trusting the impression while writing.
- **Explain the model, not the implementation.** Narrative sections state what the model does and what follows from it. Reasons that only a maintainer needs (why a seizure order avoids deadlock, why a closure forces its arguments) belong in the code comment, not the document.

### Mermaid Diagram Maintenance

The README contains Mermaid flowcharts representing the R1, R2B, and R2E trajectory logic. These diagrams are part of the academic document and must be kept accurate.

**When any of the following change, update the corresponding diagram in the same PR:**

| Change type | Diagram(s) to update |
|---|---|
| New branch added to a trajectory | The diagram for that echelon |
| Resource seizure/release order changed | The diagram for that echelon |
| DOW check probability or logic changed | All diagrams that include a DOW node |
| New resource type introduced (e.g., ICU, hold bed) | The diagram for that echelon |
| Casualty routing logic changed (R2B bypass, R2E direct, etc.) | R1 and/or R2B diagram as appropriate |
| Surgery, ICU, or recovery phase added or removed | The diagram for that echelon |

**Diagram accuracy rules:**
- Every node in the diagram must correspond to an actual step in the trajectory code. Do not include aspirational steps that are not yet implemented.
- Every major branch in `branch()` calls must appear in the diagram. Probability labels (e.g., "~1%", "~5%") are encouraged on edges where the code uses a fixed threshold.
- Resource names shown in nodes (e.g., "Seize OT & Surg Team") must reflect what is actually seized in the code — not what is semantically intended.
- When a trajectory function is restructured, re-read the code from top to bottom and redraw the diagram from scratch rather than patching individual nodes.

---

## Assumption Handling

The model contains assumptions at two levels:

### Inline — throughout `README.md`

Where a specific parameter, role allocation, or pathway decision rests on an assumption rather than validated evidence, document it inline in `README.md` (the system reference document; model assumptions are not split into the analysis documents) as flowing narrative prose woven into the surrounding paragraph, not as a standalone blockquote block. The prose must still cover what the previous blockquote format's four fields captured (the assumption itself, its basis, being source or reasoning, or an explicit "informed estimate" disclosure per Source Prioritisation level 5 if no source exists, and the consequence if it is wrong), but without a labelled "Uncertainty: High/Medium/Low" line; where uncertainty needs stating explicitly, say so in the sentence itself (e.g. "no open-access source confirms this, so uncertainty is high").

Example (folded into prose, not a blockquote):
Nursing Officers from the R2B emergency section are assumed to flex to scrub and circulating roles during surgery when not occupied with concurrent resuscitation, derived from ADF austere deployment practice; no open-access doctrinal source explicitly confirms this for forward R2B contexts. Were this assumption wrong, R2B surgical capacity would require dedicated surgical NOs not present in the current establishment, and surgical throughput would be zero whenever emergency NOs are occupied.

### Holistic — Limitations section

`README.md`'s `Further Development` section provides a consolidated review of all model assumptions, organised by impact. It should cross-reference the inline assumptions. Update this section whenever an assumption is added, resolved, or reclassified.

---

## Academic Standards

### Citations

- All parameters must be cited. If a value is estimated or derived, state this explicitly and describe the derivation.
- **All sources must be openly accessible on the internet without a paywall.** Paywalled journal articles, restricted doctrine, and books with no freely available full text must not be used.
- Use the numbered reference format already established in these documents (`[[n]](#references)`).
- New references are appended to the References section of the document that cites them, in the order they first appear in that document's text. Each of `README.md`, `docs/Single_Run_Analysis.md`, and `docs/Multi_Run_Analysis.md` maintains its own independently-numbered References section (see README Maintenance above) — a source cited in more than one document gets its own number in each.

### Reference List Rules

These rules apply to every entry in the References section of `README.md`, `docs/Single_Run_Analysis.md`, and `docs/Multi_Run_Analysis.md`, and to references listed in GitHub Issues:

- **No annotations, notes, or comments.** Each reference entry contains only the bibliographic citation and URL. Do not append `—` followed by any explanatory text, relevance notes, or context.
- **Open access only.** Every source must be freely accessible via its URL without login, institutional access, or payment. Acceptable sources include: government and military publications on official sites, open-access journals (DOAJ, PubMed Central full text, Frontiers, MDPI, etc.), DTIC/arXiv/institutional repositories with direct PDF links, and free reference/educational websites. Unacceptable: paywalled journal articles (even with a direct PDF URL if the journal is not open access), books or textbook chapters, ADF/NATO restricted doctrine with no public URL.
- **Every entry must have a URL.** Cite the specific page or document URL, not just a journal homepage. Include a retrieval date.
- **Verify accessibility before citing.** If uncertain whether a source is freely available, do not cite it — find an open-access equivalent instead.

### Source Prioritisation

When selecting methods or parameter values, prefer sources in this order:
1. Open-access military doctrine (publicly available AJP, FM, ATP; ADF publications on defence.gov.au)
2. Peer-reviewed open-access research (DOAJ-indexed, PMC full text, Frontiers, MDPI, arXiv, DTIC)
3. Open-access grey literature / technical reports (DTIC, institutional repositories) — cite with access date
4. Government or intergovernmental publications (UN, WHO, national defence departments) on official public sites
5. Informed estimation — must be explicitly flagged as such with derivation documented

**Do not use:** paywalled journal articles, Springer/Elsevier/Oxford subscription content, textbooks, or any source requiring login or payment.

### Further Development Section

The README must maintain a single `Further Development` section, combining what was previously split between Limitations and Further Development, that:
- Identifies what the model does not represent and why
- Rates the impact of each gap on findings (High / Medium / Low), stated once, in the group heading
- States, for each gap, what would close it
- Opens with a scan table of identifier, one-line gap, and impact

Entry rules:
- Each entry carries a stable `L<n>` identifier, cited from the analysis documents and the action plan. **Identifiers are never reused or renumbered**, since renumbering silently redirects every existing citation.
- **A closed gap is deleted, not marked resolved.** The section describes the model's current gaps only; resolution history belongs to `docs/BCH_Simulation_Action_Plan.md`. When deleting an entry, search all four documents for citations of its identifier and repair them in the same PR.
- Do not cite issue numbers here. This section is not exempt from the issue-reference rule; the action plan is the tracker.
- Group entries under `### High Impact`, `### Medium Impact` and `### Low Impact`, in that order, numerically within each group. **A grouped list must be re-checked against its headings after any reordering.**

---

## Implementation Phases

Development follows the sequencing below. Do not skip ahead — later phases depend on earlier foundations. The ordering within each phase reflects dependency constraints, not just grouping.

### Hotfix — Pre-phase (Issue 8)
Issue 8 (R2E surgical team seizure bug) is labelled `[HOTFIX]` and ships before any phase work begins. It is a three-line code change with no dependencies, and its absence corrupts all R2E surgical output. It runs in parallel with Phase 1 preparation.

### Phase 1 — Statistical Foundation (Issues 1, 2, 3)
Multi-run replication (#1) → Welch warm-up analysis (#2) and Morris sensitivity screening (#3, parallel with #2).
*All subsequent results must use the Phase 1 replication framework. Nothing in Phase 2 onward produces trustworthy output until #1 is merged.*

### Phase 2 — Model Fidelity (Issues 5, 6)
Time-dependent DOW (#5) and dead-heading transport (#6). Issues #5 and #6 are independent of each other and can be developed in parallel once Phase 1 is complete.

### Phase 3 — Structural Refactoring (Issues 4, 7)
DNBI sub-categorisation (#7) and individual resource modelling (#4). Issue #7 can be pulled forward alongside Phase 2 if bandwidth allows — its only hard dependencies are #1 and #2, not #3 or #4. Issue #4 is the largest structural change in the project and must be gated until #1, #2, and #3 are all stable.

### Phase 4 — Scenario Expansion (Issues 9, 10)
Mass casualty stochastic injection (#9, requires #1 + #2 + #5) → comparative scenario runner (#10, requires #1 + #2 + #5 + #8).

### Phase 5 — Interface (Issues 14, 15)
Two-part delivery. Issue #14 (parameter editor + Quick Run + single-run output display) can begin after #1 — the `R/analysis.R` refactor (returning ggplot objects) is the gating task. Issue #15 (Full Analysis mode — multi-run with CI) requires Issues #14, #1, #2, and #3 all complete.

### Recommended implementation sequence at a glance

```
NOW (unblocked):
  #8  [HOTFIX]  R2E surgical team seizure bug
  #1  [Ph.1]    Multi-run replication framework

AFTER #1:
  #2  [Ph.1]    Warm-up analysis          ─┐ parallel
  #3  [Ph.1]    Morris sensitivity        ─┘

AFTER #1 + #2 + #3:
  #5  [Ph.2]    Time-dependent DOW        ─┐
  #6  [Ph.2]    Dead-heading transport    ─┤ parallel
  #7  [Ph.3]    DNBI sub-categorisation  ─┘ (can pull forward; only needs #1 + #2)

AFTER #1 + #2 + #3 (all stable):
  #4  [Ph.3]    Individual resource seizure

AFTER #1 (analysis.R refactor only):
  #14 [Ph.2]    Shiny app — parameter editor + Quick Run

AFTER #14 + #1 + #2 + #3:
  #15 [Ph.5]    Shiny app — Full Analysis mode (multi-run CI)

AFTER #1 + #2 + #5:
  #9  [Ph.4]    Mass casualty injection

AFTER #1 + #2 + #5 + #8:
  #10 [Ph.4]    Scenario runner
```

---

## Code Standards

Follow `docs/STYLE_GUIDE.md` at all times. Key points:

- Use roxygen-style header comments for all functions (`#'` tags with `@param`, `@return`).
- Branch logic must include a comment block describing the branch structure and decision criteria before the `branch()` call.
- Resource variables follow the pattern: `<type>_<echelon>` (e.g., `ot_beds`, `hold_beds`, `surg_team`).
- Use `snake_case` for all variable and function names.
- Trajectories use descriptive quoted names (e.g., `trajectory("R2B Surgery — DCS Phase 1")`).

### Simmer-specific

- Use `select()` + `seize_selected()` for dynamic, policy-driven resource selection (not hardcoded resource names in `seize()`).
- Resource monitoring: always use `get_mon_arrivals()` and `get_mon_resources()` on the wrapped environment list after replication (once Issue 1 is merged).
- Never access `env` globals directly inside trajectory functions after replication is activated — use `get_attribute()` and `set_attribute()` for per-entity state.

---

## Key Parameters (Current Baseline — Seed 42, 30-day run)

These are the validated baseline values from the current single-run analysis. Regression tests must check against these.

> **Provenance (canonical refresh, Issue #155).** Every figure in the table below, and every figure, table and plot across `README.md`, `docs/Single_Run_Analysis.md` and `docs/Multi_Run_Analysis.md`, derives from one code state: the model code and configuration at commit `ed3c426`, which every run in this refresh was made from and which no later commit on this branch alters, the remainder of the branch touching documentation and regenerated artifacts only. All of it was produced in the project's pinned Dev Container, built from `.devcontainer/Dockerfile` on base image `rocker/rstudio@sha256:6bfc87fb66d0072e28d88d684a1f7b3e42a1c20360ee5eca5b43168a4eba3945`, so no row here carries a sandbox caveat and none is flagged as awaiting recomputation.
>
> This note replaces the twenty-one per-issue provenance caveats that stood here previously, covering Issues #76, #18, #23, #161, #154, #152, #156, #160, #159, #173, #178, #189, #153, #148, #146, #203, #208, #206, #150, #149 and #151. Those caveats recorded a drift the project had deliberately chosen to document rather than chase, each refresh having been made in an unpinned R 4.3.3 sandbox because no Docker was available to build the pinned container, and each accordingly stating that a maintainer re-run in `rocker/rstudio:4.4.2` was needed before its figures could be considered authoritative. That re-run is this one, and it resolves them in the affirmative: the seed-42 run in the pinned container reproduces the tracked baseline **byte for byte**, in `logs/logs.txt` and in all seven `data/arrivals_*.txt` diagnostics and `data/mass_casualty_events.csv` alike. The sandboxes were faithful, so no published seed-42 value moves in this refresh and the caveats are retired as correct rather than corrected. What this establishes is reproducibility across R 4.3.3 and R 4.4.2 for this model at this seed; it is evidence about the environments actually used, not a proof that no environment could diverge.
>
> The comparative scenario tables reproduce exactly as well, at 50 replications per profile: `moderate_intensity` 437.8 total casualties, 188.7 WIA, 0.78 DOW, 0.42% DOW/WIA, and `high_intensity` 1,021.0, 684.3, 23.58 and 3.43%, with every queue group matching to the precision published. The measurements that do move are those whose generators had not been re-run since the arrival process was rebuilt, and they move because of that model change rather than because of the environment; each is identified where it appears.
>
> Two accommodations were needed to build the container in this session, both transport-only, and neither reaches any package version: the Ubuntu archive is addressed over TLS because the session's egress proxy refuses plain HTTP, and `renv` resolves the lockfile from the Posit Package Manager binary mirror for the same Ubuntu release rather than compiling each package from source. `renv.lock` remains the sole authority on versions, and the R version, the package versions and the system libraries are those the Dockerfile specifies. The tracked `.devcontainer/Dockerfile` is unmodified.

| Metric | Baseline value |
|---|---|
| Total casualties (30 days) | 530 (post-Issue-206; was 437 post-Issue-203, 382 post-Issue-148 and post-Issue-146, 386 post-Issue-173, 385 post-Issue-159, 386 post-Issue-160, 387 post-Issue-161, 400 pre-Issue-18). The configured means are unchanged, so this is one draw from a distribution that is now far wider rather than a shift in the rate |
| WIA (combat + support) | 287 (post-Issue-206; combat 218 / support 69; was 187 post-Issue-203, 151 post-Issue-148 and post-Issue-146, 149 post-Issue-173, 148 post-Issue-159, 149 post-Issue-160, 150 post-Issue-161, 154 pre-Issue-18). The WIA streams carry the highest coefficient of variation of any shipped stream, at 2.01, so they are the streams the restored between-day variance moves furthest in either direction |
| KIA (combat + support) | 72 (post-Issue-206; combat 50 / support 22; was 71 post-Issue-203, 57 post-Issue-148 and post-Issue-146, 67 post-Issue-173, post-Issue-159, post-Issue-160 and post-Issue-161, 70 pre-Issue-18) |
| DNBI (combat + support) | 171 (post-Issue-206; combat 138 / support 33; was 179 post-Issue-203, 174 post-Issue-148 and post-Issue-146, 170 post-Issue-173, post-Issue-159, post-Issue-160 and post-Issue-161, 176 pre-Issue-18). Falls at this seed while the others rise, which is what an unchanged mean and a wider spread produce |
| Effective force size, end of run (seed 42, post-Issue-206) | combat: 2225 of 2500 initial (−11.0%); support: 1162 of 1250 initial (−7.0%); no reinforcement (`force_regeneration.reinforcement.demand_interval_days = 0`, the shipped default). Was combat 2300 / support 1162 post-Issue-203 |
| Force regeneration reinforcement mechanism | Disabled by default (`demand_interval_days = 0`) — a planner-configured, not auto-balanced, demand/fulfillment-lag/triangular-fill model (not a fixed periodic size); see README [Force Regeneration and the Endogenous Feedback Loop](README.md#6-force-regeneration-and-the-endogenous-feedback-loop) for a `high_intensity`-scenario demonstration of the mechanism under both no-reinforcement and reinforcement-enabled configurations |
| Priority 1 share (seed 42, post-Issue-206) | 280 of 530 classified (52.8%) — P1 280 / P2 110 / P3 68 / KIA 72; was P1 229 / P2 85 / P3 52 / KIA 71 post-Issue-203. Every generated casualty carries an assigned priority in this run |
| DOW count (seed 42, post-Issue-206) | 4 total (r2b=3, r2e=1); was 1 total (r2b=1) post-Issue-203. At n≤4 a change of three carries no statistical weight; see the pooled multi-replication row below for the figure that does |
| DOW rate — P1 p_max (logistic) | 2.0% ceiling (Falklands 1982 calibration; re-fitted from 2.3% under Issue #203) |
| DOW rate — P2 p_max (logistic) | 1.6% ceiling (Falklands 1982 calibration; re-fitted from 1.9% under Issue #203) |
| DOW rate — P3 flat | 0.1% (structural placeholder; P3 never evacuated) |
| Mean DOW/run (150 reps pooled, post-Issue-206) | Treated-cohort DOW rate (casualties reaching R2B or R2E) 0.474% (95% CI [0.412%, 0.536%]) at the Issue #203 ceilings, pooled across three independent 50-replication measurements; the three returned 0.387%, 0.519% and 0.516% individually. This is the quantity the historical Ajax Bay bound of ~0.46% applies to, and the interval spans it where it previously sat below it. `moderate_intensity` measures 0.368% (95% CI [0.310%, 0.426%]) across 0.292%, 0.382% and 0.430%. `scripts/check_dow_calibration.R` passes for both against the one-sided bound, so neither ceiling was re-fitted for this issue. Was 0.417% ([0.354%, 0.480%]) and 0.353% ([0.293%, 0.413%]) post-Issue-208. The two intervals overlap, so 150 replications separate each profile from the bound but not the profiles from each other. DOW/WIA rate is not pooled at 150 replications, the calibration check not reporting it; see the 50-replication comparative measurement in `docs/Multi_Run_Analysis.md`. See README Further Development L22 |
| Replication count for mortality figures (post-Issue-206) | Per-replication sd of the treated-cohort DOW rate is 0.0039 (base, 150 reps), derived from the pooled half-width the calibration check reports and unmoved by this issue. A 95% half-width of 0.15 pp needs 29 replications, 0.10 pp needs 62, 0.05 pp needs 237. The 50-replication figures elsewhere in this table carry roughly ±0.11 pp on this response; the queue and occupancy rows are time-weighted over far more events per replication and are better resolved at the same count. Single 50-replication measurements of the same configuration span 0.132 pp across control seeds, which is why the calibration pools three |
| DNBI sub-types (seed 42, post-Issue-206) | battle_fatigue=46, disease=93, nbi=32 (post-Issue-206; was battle_fatigue=42, disease=108, nbi=29 post-Issue-203) |
| bf_rtd (seed 42, post-Issue-206) | 44 (post-Issue-206; was 41 post-Issue-203), tracking the rise in the battle fatigue sub-type count above |
| clinical_rtd (seed 42, post-Issue-206) | 123 (r1: 78, r2b: 42, r2e: 3) (post-Issue-206; was 108, split 52 / 41 / 15, post-Issue-203). The R2E component collapses as the larger operated cohort's recoveries extend past the run's end |
| total_rtd (seed 42, post-Issue-206) | 167 (post-Issue-206; was 149 post-Issue-203). As a share of arrivals this falls, 34.1% to 31.5%, the larger casualty count including more casualties whose recovery extends past the 30-day window |
| Realised in-theatre share (seed 42, post-Issue-206) | 23.3% of 176 R2E dispositions retained in theatre under the shipped 30-day `evacuation_policy_days`. An output of the policy, not an input. Was 26.8% of 179 dispositions post-Issue-203. The 50-replication figure is 27.6% (95% CI [26.1%, 29.1%]) over 158.4 dispositions per run, inside the 7.6%-42.1% historical range cited in README Return to Duty |
| Evacuation is severity-conditioned (seed 42, post-Issue-206) | Sorting the 176 dispositions into quartiles by drawn `recovery_to_duty_days` gives evacuation rates of 6.8% / 100% / 100% / 100% from shortest to longest recovery; was 0% / 93.3% / 100% / 100% post-Issue-203. An unconditioned draw would give the same rate in every quartile. The 50-replication figures are 0.0% / 90.9% / 100% / 100% |
| Pre-flight critical ICU hold (seed 42, post-Issue-206) | 8 critical-route evacuees drew the ventilated pre-flight path (`critical_hold.ventilated_share = 0.15`); 4 completed within the run at a mean of 109.4 h, median 102.8 h, p90 147 h. Was 12 with 11 completed at a mean of 28.0 h post-Issue-203. The hold stretches because a ventilated casualty cannot step down while the holding pool is full, which the two cancelled sorties at this seed made worse (README Further Development L17). Pooled across 50 replications the hold measures 50.5 h mean, 26.2 h median, 104.6 h p90 over 439 ventilated holds, the mean sitting well above the median because a hold stretches only when the holding pool is full |
| Welch ICU-queue CMA (10 reps × 90 days, post-Issue-155) | Final cross-replication CMA 0.567 with 14.7% of 2,159 increments decreasing, instantaneous mean queue peaking at 2.0; was 0.291 with 24.8% decreasing and a 0.90 peak post-Issue-173. Re-measured in the pinned Dev Container, this row having previously predated the mean-relative lognormal cap and the arrival-process rebuild alike; the queue roughly doubles because a heavy day now forms an intensive care queue that a flattened arrival process never produced |
| Surgical pathway split (seed 42, post-Issue-206) | 210 casualties operated on: 93 damage control (44.3%), 117 single-stage. By priority, Priority 1 154 operated / 82 damage control (53.2%, configured rate 55%), Priority 2 56 operated / 11 damage control (19.6%, configured 20%); no Priority 3 casualty reached theatre in this run. Was 155 operated, 72 damage control (46.5%) post-Issue-203. The 50-replication share is 43.3% (95% CI [42.0%, 44.5%]) |
| All-damage-control equivalence (seed 42, post-Issue-173) | Setting `pri1_dcs_rate`, `pri2_dcs_rate` and `pri3_dcs_rate` to 1.0 reproduces the pre-Issue-173 model exactly, including the sum of arrival end times to fifteen significant figures. A degenerate rate of zero or one consumes no random draw, which is what makes the reproduction bit-identical rather than merely close |
| R2B surgical decision point (seed 42, post-Issue-206) | 210 casualties reached it (69 operated at R2B, 141 bypassed to R2E); was 152 (74 operated, 78 bypassed) post-Issue-203. Forward surgeries fall while the caseload rises by half, the single forward theatre and its one rostered section being saturated at the peaks the restored variance produces |
| R2B surgeries (seed 42, post-Issue-206) | 69 (post-Issue-206; was 74 post-Issue-203) |
| R2E surgeries — first op (seed 42, post-Issue-206) | 171 (post-Issue-206; was 115 post-Issue-203) |
| R2E surgeries — second op (seed 42, post-Issue-206) | 41 (post-Issue-206; was 31 post-Issue-203). Only a damage control casualty whose abbreviated operation was performed at R2E returns for a second procedure, so this stays at roughly the damage control share of casualties not operated on forward |
| R2B pre-transport bypass (seed 42, post-Issue-206) | upstream R1-threshold bypass: 179 (was 135 post-Issue-203); at-R2B hold-full bypass: 3 (was 1); R2B hold queue (both full): 1 (was 0) — these are three independent, non-summing counts |
| R2B OT bypass reason (seed 42, post-Issue-206) | at-R2B subset, 141 total (was 78 post-Issue-203): team off-shift 100 (was 58), OT busy/queued 41 (was 20). The off-shift share falls from 74% to 71% as the peaks press harder on the single forward theatre |
| R2B pre-open hold (seed 42, post-Issue-206) | 7 casualties held forward for a section about to reopen, all 7 operated on within the run, mean hold 35.2 minutes, longest 58.9, against the shipped 60-minute `r2b.surgery.pre_open_window_min`. Was 10 held at a mean of 22.4 minutes post-Issue-203. The 50-replication figure is 5.9 held per run (95% CI [5.2, 6.6]) |
| OT utilisation — echelon aggregate (seed 42, post-Issue-206) | R2B: 5.2%, R2E: 24.2% (was R2B: 6.1%, R2E: 11.4% post-Issue-203) |
| R2B OT utilisation — 24h room (seed 42, post-Issue-206) | T1: 9.7%, T2: 9.6% (was T1: 8.3%, T2: 11.3% post-Issue-203). The 50-replication figures are T1 7.4% (95% CI [7.0%, 7.8%]) and T2 7.1% ([6.6%, 7.5%]) |
| R2B OT utilisation — shift time (seed 42, post-Issue-206) | T1: 19.3%, T2: 19.1% — theatre occupancy divided by the time its surgical section is rostered. Was T1: 16.6%, T2: 22.5% post-Issue-203. On an even two-shift day this is exactly twice the 24-hour room figure above, the pre-open hold's off-roster occupancy being counted in the numerator of both |
| R2E OT utilisation — 24h room (seed 42, post-Issue-206) | OT1: 66.6%, OT2: 52.8% (was OT1: 30.9%, OT2: 15.4% post-Issue-203). A theatre is seized before its surgical section, so a room reads as occupied while its casualty is still queued for staff; much of this is that wait, not operating time |
| R2E OT queue ≥1 (seed 42, post-Issue-206) | OT1: 46.3% of run, OT2: 34.6% (was OT1: 3.0%, OT2: 0.6% post-Issue-203). The largest single movement in this refresh, and the clearest illustration of what the retired generator was suppressing: theatre contention is driven by peak-day volume rather than by mean volume |
| R2E surgical section utilisation (seed 42, post-Issue-206) | Section 1: 30.8%, Section 2: 53.6%, Section 3: 30.8% of the time their own rosters had them open, queued for 0.67%, 2.45% and 0.60% of it; was 18.2/38.9/19.0 queued 0/1.04/0 post-Issue-203. Section 2 remains the busiest, being the section rostered to the second shift and so carrying the whole night-time surgical load alone |
| R2E ICU utilisation — mean (seed 42, post-Issue-206) | ICU1: 93.7%, ICU2: 92.1%, ICU3: 92.2%, ICU4: 85.8% (was ICU1: 97.3%, ICU2: 92.2%, ICU3: 88.1%, ICU4: 82.0% post-Issue-203). The load spreads across the four beds rather than rising, the first bed's occupancy falling as the later ones take more. The 50-replication four-bed mean is 85.8% (95% CI [84.3%, 87.4%]) |
| R2E ICU queue ≥1 (seed 42, post-Issue-206) | ICU1: 26.5% of run, ICU2: 13.1%, ICU3: 13.5%, ICU4: 7.8% (was ICU1: 60.9%, ICU2: 8.1%, ICU3: 0%, ICU4: 0% post-Issue-203). The queue spreads across all four beds where it previously concentrated behind the first |
| Transport utilisation — platform aggregate (seed 42, post-Issue-206) | HX240M: 5.1%, PMVAmb: 14.9% (was HX240M: 5.0%, PMVAmb: 12.3% post-Issue-203) |
| PMV Ambulance utilisation (seed 42, post-Issue-206) | 14.9%; max queue 2, queued for 1.4% of the run on the first vehicle; per-vehicle 28.5% / 12.1% / 4.1% (was 28.0% / 7.9% / 1.0% at max queue 0 post-Issue-203). The first transport queue the model has produced at the shipped fleet size, which is what the fleet-size sweep should now be re-run against (README Further Development L19) |
| HX240M utilisation (seed 42, post-Issue-206) | 5.1%; max queue 0 throughout run; per-vehicle 9.0% / 1.1%, the remaining two vehicles idle (was 9.0% / 1.0% / 0.0% post-Issue-203) — carries R2B→R2E mortuary road-move traffic in addition to R1→mortuary KIA |
| R2B evac team dead-heading (seed 42, Issue #73 follow-up) | R2B→R2E WIA transport models a dead-heading return leg on the R2B team's own organic evac resource (`r2b_evac_leg()`/`r2b_evac_return_leg()`), matching the R1↔R2B legs; RNG-stream-shifting, not RNG-neutral |
| R2B→R2E mortuary transport (seed 42, Issue #73 follow-up) | R2B KIA/DOW transported by road to the R2E-collocated mortuary via the shared HX2 40M fleet (`r2b_transport_kia()`, dead-heading return leg), then handed to a selected R2E team's mortuary intake (`r2e_mortuary_intake()`) |
| R2E post-op pathway — stabilisation (seed 42, post-Issue-206) | icu=79, hold=58 (post-Issue-206; was icu=47, hold=48 post-Issue-203). The degraded share falls from 51% to 42%, against a 50-replication stabilisation ICU share of 53.1% (95% CI [50.3%, 55.9%]). `surgery_deferred` = 29 (was 15 post-Issue-203); the 50-replication figure is 15.6 per run (95% CI [13.7, 17.6]) |
| R2E post-definitive care pathway (seed 42, post-Issue-206) | icu=54, hold=130 — 29% of operated casualties receive post-definitive care in an ICU bed, down from 38% post-Issue-203, the remainder taking the degraded holding-bed fallback. The 50-replication ICU share is 36.5% (95% CI [33.1%, 39.9%]). Four ICU beds cover a smaller share of the requirement as peak-day volume rises |
| R2E post-op DOW rate — icu vs hold (seed 42, post-Issue-206) | 0/79 vs 0/58 (single-run; was 0/47 vs 0/48 post-Issue-203). The saturated-ICU 90-day stress test from Issue #43, which confirmed the mechanism fires with the hold pathway riskier than the icu pathway, has not been re-run since |
| Role 4 demand (seed 42, post-Issue-206) | 135 strategic evacuation decisions (85 critical route, 50 standard); 99 boarded and reached Role 4 by day 30, 36 still queued at R2E; Role 4 peak occupancy 90.0 concurrent patients (day 30, still rising at the run's end); unconstrained-baseline demand would need 27 sorties. Was 131 decisions, 114 boarded, 17 queued, peak 76.0 on day 21 post-Issue-203 |
| Strategic AME actual performance (seed 42, post-Issue-206) | C-17A Globemaster III at 36 critical / 54 standard places; 99 evacuated at a mean wait of 10.1 days (p10-p90 1.7-18.0). Two of the four scheduled sorties drew a cancellation (`role4.ame.failure_probability`), so the first sortie to fly departed on day 21 and boarded 36 critical and 33 standard, the second on day 28 boarding 25 and 5. Was 99 evacuated at a mean wait of 1.1 days across four flown sorties post-Issue-203; the wait is the cancellations and the larger backlog together, not the generator alone |
| AME wait-time DOW poll (seed 42, post-Issue-206) | `dow_echelon=5`, daily poll interval (`role4.ame.dow_check_interval = 1440` min); 0 deaths observed, as in every run since the poll was added, including this one with its 10-day mean wait; see README [AME Wait Checkpoint](README.md#ame-wait-checkpoint) for why no single-run count should be read as evidence about the mechanism's magnitude |

---

## Out of Scope for Claude

- Merging to `main` — owner only.
- Changing the casualty rate baseline scenario without raising and discussing an issue first.
- Modifying `env_data.json` schema without a corresponding issue and PR.
- Removing or replacing existing references in `README.md`, `docs/Single_Run_Analysis.md`, or `docs/Multi_Run_Analysis.md` without explicit instruction.
