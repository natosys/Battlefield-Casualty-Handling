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
| `scripts/run_transport_sweep.R` | CLI entry point for the transport fleet-size sweep |
| `scripts/shiny_worker.R` | Background worker sourced by `app.R` for async Quick Run / Full Analysis execution |
| `scripts/check_env_data_summary.R` | Regenerates the `<!-- ENV SUMMARY -->` block inside `README.md` from `env_data.json` |
| `scripts/check_markdown.R` | Maintains the TOC and "Return to Top" links across `README.md`, `docs/Single_Run_Analysis.md`, and `docs/Multi_Run_Analysis.md` |
| `scripts/check_r2e_surgery_seizure.R` | Regression check asserting that every R2E surgery seizes a surgical section, structurally and behaviourally; exits non-zero on failure |
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
| Phase sequence list | Strike through the item with `~~double tildes~~` |
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
- Use the numbered reference format already established in these documents (`[[n]](#References)`).
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

> **Provenance caveat (Issue #76 refresh):** the seed-42 single-run rows below were regenerated in the project's actual pinned Dev Container (`rocker/rstudio:4.4.2`, built directly from `.devcontainer/Dockerfile`), superseding the prior Issue #73 follow-up refresh (which used an unpinned R 4.3.3 sandbox and carried a corresponding accuracy caveat). As a validation step, the pre-Issue-76 configuration (`env_data.json` `r2b.surgery`/`r2eheavy.surgery` = 90/240/120) was re-run first in this pinned container and reproduced every one of the previously documented post-Issue-73 figures exactly (total/WIA/KIA/DNBI counts, DOW count, DNBI sub-types, RTD breakdown, R2B/R2E surgical counts, bypass counts, per-resource OT/ICU/transport utilisation, post-op pathway) — confirming the pinned container is a faithful, reproducible environment. Issue #76 then updated `r2b.surgery`/`r2eheavy.surgery` to 41/210/95 (see Issue #76 for the citation basis) and the seed-42 baseline was re-run again; the rows below reflect that post-Issue-76 configuration. This is an RNG-stream-shifting change: because `simmer` draws from a single global RNG stream shared across all interleaved entity trajectories in event-time order, a shorter/different DAMCON surgery-duration distribution shifts the RNG position for every subsequent draw by any entity for the remainder of the run — so, as with prior RNG-stream-shifting merges (Issue #43, #73), every seed-42 row *below* raw casualty generation (which precedes all trajectory execution and is therefore unaffected) changed. The `R2B OT utilisation — shift time` row and all 50-replication multi-run rows were **not** recomputed as part of this refresh (out of scope for Issue #76's single-run acceptance criterion) and remain flagged pending. Unlike the Issue #73 follow-up refresh, `data/arrivals_*.txt`, `logs/logs.txt`, and `images/*.png` **were** regenerated and committed as part of this refresh, since this run was produced in the genuine pinned container rather than an unpinned sandbox — these tracked artifacts now finally reflect a canonical-environment run for the first time since that caveat was introduced. In the course of this refresh, the previously documented "R2B bypass count ... (upstream R1 threshold: 50 + at-R2B OT-check bypass: 82)" parenthetical was found to be a pre-existing inaccuracy unrelated to Issue #76 — the current codebase's `r2b_pre_bypass_count` (upstream R1-threshold routing) and `r2b_ot_bypass_count` (at-R2B OT-check bypass) are two independent, non-summing counts, not addends of a combined total; the row below corrects this.

> **Provenance caveat (Issue #18 refresh):** the seed-42 single-run rows below marked "post-Issue-18" were regenerated in an **unpinned R 4.3.3 sandbox** (this issue's development environment had no Docker access to build the project's pinned `rocker/rstudio:4.4.2` Dev Container), carrying the same accuracy caveat as the Issue #73 follow-up refresh — a maintainer re-run in the pinned container is needed before these figures are fully authoritative. Issue #18 replaces the previous batch/`at()` casualty arrival generation with live, force-size-reactive generator closures (see README [Force Regeneration and the Endogenous Feedback Loop](README.md#6-force-regeneration-and-the-endogenous-feedback-loop)) — the RNG consumption *order* itself changes (arrival-rate draws are now interleaved with trajectory-duration draws in event time, rather than 100% front-loaded before `run()` starts), which is a materially larger RNG-stream shift than prior merges in this category (Issue #43/#73/#76 changed values drawn from an unchanged draw order; this one changes the order). `force_regeneration.reinforcement` ships disabled (`interval_days = 0`) by default, so this refresh reflects the constant-reinforcement-off baseline. Only the rows below marked "post-Issue-18" were recomputed; every other row (including several already-pending rows carried forward from Issue #76) remains as last refreshed and is **not** re-verified here. Two new rows are added for the effective force size mechanism itself. `data/arrivals_*.txt`, `logs/logs.txt`, and `images/*.png` were **not** regenerated or committed as part of this refresh, consistent with the Issue #73 follow-up's unpinned-sandbox precedent (only a genuine pinned-container run updates those tracked artifacts).

> **Provenance caveat (Issue #23 refresh):** the seed-42 single-run rows below marked "post-Issue-23" were regenerated in an **unpinned R sandbox** (no Docker access in this session's development environment to build the project's pinned `rocker/rstudio:4.4.2` Dev Container), carrying the same accuracy caveat as the Issue #18 refresh — a maintainer re-run in the pinned container is needed before these figures are fully authoritative. Issue #23 and its four in-PR follow-ups (strategic AME as a real scheduled/capacity-constrained/two-configuration simmer resource; the sortie interval default changed from 3 to 7 days; a periodic wait-time DOW poll) are **not RNG-stream-neutral**: casualties now hold R2E ICU/Hold beds for a variable, often multi-day-to-multi-week AME wait rather than departing the model instantly, the AME sortie generator and the new wait-time DOW poll each consume additional `runif()` draws, and — per the same single-global-RNG-stream mechanism already documented for Issue #18/#76/#73/#43 — this shifts the draw position for every subsequent draw by any entity for the remainder of the run, including entities whose own trajectory logic is otherwise unchanged (e.g. DNBI sub-type assignment, an early-stage draw). Total casualty count, WIA/KIA/DNBI counts, and priority split are unaffected in this particular case, because Issue #23's changes occur strictly downstream of R2E disposition and neither strategically-evacuated nor in-theatre-recovering casualties feed an RTD credit back into the live force-size-reactive arrival generator differently than before (see [Force Regeneration and the Endogenous Feedback Loop](README.md#6-force-regeneration-and-the-endogenous-feedback-loop)) — every row from DOW count downward *is* affected, since it depends on trajectory-internal draw timing rather than arrival generation. Only the rows below marked "post-Issue-23" were recomputed; every other row (including several already-pending rows carried forward from Issue #18) remains as last refreshed and is **not** re-verified here. New rows are added for the Role 4/AME/wait-time-DOW-poll outputs Issue #23 introduces. `data/arrivals_*.txt`, `logs/logs.txt`, and `images/*.png` were **not** regenerated or committed as part of this refresh, consistent with the Issue #18 unpinned-sandbox precedent.

> **Provenance caveat (Issue #161 refresh):** every seed-42 single-run row below marked "post-Issue-161" was regenerated in an **unpinned R 4.3.3 sandbox** (no Docker access in this session's development environment to build the project's pinned `rocker/rstudio:4.4.2` Dev Container), carrying the same accuracy caveat as the Issue #18 and Issue #23 refreshes — a maintainer re-run in the pinned container is needed before these figures are fully authoritative. As a validation step, and following the precedent set by the Issue #76 refresh, the pre-Issue-161 configuration was re-run first in this sandbox and reproduced every documented post-Issue-23 figure **exactly** (total/WIA/KIA/DNBI counts, priority split, DOW count and echelon split, DNBI sub-types, RTD breakdown, R2B surgeries, R2E first surgeries, bypass counts, post-op pathway split, deferred surgeries, and the Role 4/AME figures), which is the strongest available evidence that this unpinned sandbox is a faithful stand-in for the pinned container on this model. Issue #161 binds the R2E surgical section to theatre entry, so R2E surgery now queues for staff where it previously ran against theatre beds alone; this is **not** RNG-stream-neutral, and unlike Issue #23 it shifts rows *above* R2E disposition as well, because Issue #18's force-regeneration feedback loop couples arrival timing to casualty-event timing and so propagates a downstream timing change back into casualty generation (total casualties 386 to 387). Rows recomputed in this refresh clear the long-standing "not recomputed post-Issue-18 or post-Issue-23" pending flags on the per-room OT, per-bed ICU and per-vehicle transport breakdowns. The `R2B OT utilisation — shift time` row and all 50-replication multi-run rows were **not** recomputed and remain flagged pending. `data/arrivals_*.txt`, `logs/logs.txt`, and `images/*.png` were **not** regenerated or committed as part of this refresh, consistent with the Issue #18 and Issue #23 unpinned-sandbox precedent.

> **Provenance note (Issue #154 artifact refresh):** Issue #154 changes no model code and shifts no RNG stream, so **no baseline value in the table below changes** and none of the pending flags recorded in the caveats above are cleared by it. What it does change is the tracked artifact set. An audit of `git log` per file found that `images/`, `logs/logs.txt` and `data/` had never been mutually consistent: their contents traced to six different commits spanning 2026-07-10 to 2026-07-16 and four different model configurations (the bulk from Issue #76, plus `images/force_regeneration.png` from Issue #18, `images/role4_census.png` from Issue #23, `images/ame_backlog.png` and `images/ame_sortie_timeline.png` from Issue #109, and the two mass casualty files from Issue #9), and the whole set predated the Issue #18, #23 and #161 output shifts. All of it was regenerated together from one `Rscript run.R --seed 42 --days 30 --iterations 1 --refresh-baseline` invocation and committed as a single set, so the three directories are traceable to one run for the first time. The run was made in an **unpinned R 4.3.3 sandbox** under the same constraint and caveat as the Issue #161 refresh (no Docker access to build `rocker/rstudio:4.4.2`); departing from the Issue #18/#23/#161 precedent of not committing tracked artifacts from an unpinned sandbox is deliberate here, since leaving them mixed is the defect being closed, and the run reproduced every documented post-Issue-161 figure exactly (total casualties 387; priority split 202/64/53; DOW 2, `r2b`=2 / `ame_wait`=0; R2B routing 109/0/0; R2B OT bypass 67 off-shift / 6 busy / 73 total; post-op `icu`=9 / `hold`=94; `surgery_deferred`=24; Role 4 40 evacuated, 76 queued, peak 20.0, 29 sorties), which is the same validation standard the Issue #161 refresh applied. One tracked figure, `images/mass_casualty_events.png`, is deliberately **not** part of this set and was left untouched: it is generated under `mass_casualty.event.rate_per_day = 0.2` rather than the shipped default of zero, as the surrounding narrative in `docs/Single_Run_Analysis.md` states, so a shipped-configuration run cannot reproduce it.

> **Provenance note (Issue #152 calibration-target correction):** Issue #152 changes no model code, no `env_data.json` parameter and no RNG stream, so **no seed-42 single-run value in the table below changes**. One multi-run row does change: the long-pending "Mean DOW/run (50-rep, seed=NULL)" figure of ~0.70 was re-measured during this issue's validation work and is now 1.34 (95% CI [0.94, 1.74]), with the treated-cohort rate added alongside it. That movement is the accumulated effect of the Issue #73 follow-up, #76, #18, #23 and #161 RNG-stream shifts finally being measured, not an effect of Issue #152, which left both ceilings untouched. The remaining 50-replication rows below stay flagged pending. What changes is the historical target the existing DOW ceilings are documented as reproducing. The previous target, "3 DOW / 580 WIA ≈ 0.52%", could not be traced to either cited source: neither reference reports 580, the cohort was mischaracterised as British wounded in action when the source describes casualties from both sides, and the quantity was a treated-cohort mortality rate rather than a DOW/WIA rate. Reference [13] was additionally attributed to the wrong author (Payne rather than Jackson, Batty, Ryan & McGregor). The target is now the Ajax Bay treated-cohort rate of three deaths among the "over 650" casualties who reached forward surgical care (≈0.46%, Westphalen 2018), compared against the model's own cohort of casualties reaching an R2B or R2E facility. Both shipped configurations were re-validated against it at 50 replications in an **unpinned R 4.3.3 sandbox** (no Docker access in this session to build `rocker/rstudio:4.4.2`), under the same caveat as the Issue #18, #23, #161 and #154 refreshes: base 0.633% (95% CI [0.392%, 0.874%]), `moderate_intensity` 0.491% (95% CI [0.310%, 0.673%]), both spanning the target, so `p1_p_max` and `p2_p_max` were left unchanged in both. As a validation step the seed-42 single run was reproduced first in this sandbox and matched every documented post-Issue-161 figure exactly (total casualties 387; R2B routing 109/0/0; R2B OT bypass 67/6/73; post-op `icu`=9 / `hold`=94; `surgery_deferred`=24; Role 4 40 evacuated, 76 queued, peak 20.0, 29 sorties). No tracked artifact in `images/`, `logs/` or `data/` was regenerated, since nothing about the run changed.

> **Provenance caveat (Issue #156 refresh):** every seed-42 single-run row below marked "post-Issue-156" was regenerated in an **unpinned R 4.3.3 sandbox** (no Docker access in this session's development environment to build the project's pinned `rocker/rstudio:4.4.2` Dev Container), carrying the same accuracy caveat as the Issue #18, #23, #161 and #154 refreshes — a maintainer re-run in the pinned container is needed before these figures are fully authoritative. As the validation step this project now applies as standard, the pre-Issue-156 configuration was re-run first in this sandbox and reproduced every documented post-Issue-161 figure **exactly** (total casualties 387; priority split 202/64/53/67; R2B routing 109/0/0; R2B OT bypass 67/6/73; post-op `icu`=9 / `hold`=94; `surgery_deferred`=24; Role 4 116 decisions, 40 evacuated, 76 queued, peak 20.0, 29 sorties). Issue #156 replaces the unconditioned `in_theatre_rate` disposition draw with a theatre evacuation policy compared against a severity-scaled recovery-to-duty duration, and moves casualties awaiting strategic AME off ICU beds onto holding beds apart from a bounded pre-flight period for a ventilated minority. This is **not** RNG-stream-neutral and, like Issue #161, shifts rows *above* R2E disposition as well, because Issue #18's force-regeneration feedback loop couples arrival timing to casualty-event timing (total casualties 387 to 385). Three parameters change identity in `env_data.json`: `r2eheavy.recovery.in_theatre_rate` is removed in favour of `evacuation_policy_days`, `r2eheavy.holding` is rebased by a factor of three to serve as the base recovery-to-duty distribution (min/mode/max 4320/38880/90720 minutes), and `r2eheavy.recovery_to_duty` and `r2eheavy.critical_hold` are new. The Morris `r2e_hold_mode` screening bounds were rescaled to match; the published Morris ranking itself has **not** been re-run and is flagged as such in README. The `R2B OT utilisation — shift time` row and the pre-Issue-43 comparison rows were **not** recomputed and remain flagged pending. `data/arrivals_*.txt`, `logs/logs.txt` and `images/*.png` **were** regenerated and committed as one set from a single `--refresh-baseline` invocation, following the Issue #154 precedent, since leaving them mixed is the defect that issue closed; `images/welch_plot_icu_queue.png` was regenerated separately by `scripts/run_warmup.R` at the shipped configuration.

> **Provenance caveat (Issue #160 refresh):** every seed-42 single-run row below marked "post-Issue-160" was regenerated in an **unpinned R 4.3.3 sandbox** (no Docker access in this session's development environment to build the project's pinned `rocker/rstudio:4.4.2` Dev Container), carrying the same accuracy caveat as the Issue #18, #23, #161, #154 and #156 refreshes — a maintainer re-run in the pinned container is needed before these figures are fully authoritative. As the validation step this project now applies as standard, the pre-Issue-160 configuration was re-run first in this sandbox and reproduced every documented post-Issue-156 figure **exactly** (total casualties 385; R2B routing 121/0/0; R2B OT bypass 62/12/74; `surgery_deferred`=20; post-op `icu`=72 / `hold`=47; 119 dispositions at a 30.3% in-theatre share; Role 4 25 evacuated, 58 queued, peak 17.0). Issue #160 replaces the two unsourced AME aircraft configurations, and the rule that selected between them, with a single named airframe carrying its published fitted capacity: the shipped C-17A Globemaster III at 36 critical and 54 standard places per sortie, against the 2 and 8 the selected configuration previously offered. The C-130J-30 and C-27J are added as selectable alternatives. This is **not** RNG-stream-neutral, although it draws no new random numbers: a casualty who boards the first available sortie releases its R2E bed weeks earlier than one queued behind a 2-seat critical pool, which reorders every subsequent draw from simmer's single global stream, and Issue #18's force-regeneration loop carries that back into arrival timing (total casualties 385 to 386). Every row below was recomputed except where the row itself says otherwise, including the 50-replication rows and the `R2B OT utilisation — shift time` row, both long flagged pending. Four rows were removed rather than refreshed: the three pre- versus post-Issue-43 comparisons, whose quantities have been re-measured three times since, and the "Priority 1 share (of 400 total)" row, which predates four RNG-stream shifts and is no longer comparable to anything. `data/arrivals_*.txt`, `logs/logs.txt` and `images/*.png` **were** regenerated and committed as one set from a single `--refresh-baseline` invocation, following the Issue #154 precedent; `images/welch_plot_icu_queue.png` was regenerated separately by `scripts/run_warmup.R` at the shipped configuration.

| Metric | Baseline value |
|---|---|
| Total casualties (30 days) | 386 (post-Issue-160; was 385 post-Issue-156, 387 post-Issue-161, 400 pre-Issue-18) |
| WIA (combat + support) | 149 (post-Issue-160; was 148 post-Issue-156, 150 post-Issue-161, 154 pre-Issue-18) |
| KIA (combat + support) | 67 (post-Issue-160, unchanged from post-Issue-156 and post-Issue-161; was 70 pre-Issue-18) |
| DNBI (combat + support) | 170 (post-Issue-160, unchanged from post-Issue-156 and post-Issue-161; was 176 pre-Issue-18) |
| Effective force size, end of run (seed 42, post-Issue-160) | combat: 2321 of 2500 initial (−7.2%); support: 1175 of 1250 initial (−6.0%); no reinforcement (`force_regeneration.reinforcement.demand_interval_days = 0`, the shipped default). Was combat 2330 / support 1176 when last measured, post-Issue-18 |
| Force regeneration reinforcement mechanism | Disabled by default (`demand_interval_days = 0`) — a planner-configured, not auto-balanced, demand/fulfillment-lag/triangular-fill model (not a fixed periodic size); see README [Force Regeneration and the Endogenous Feedback Loop](README.md#6-force-regeneration-and-the-endogenous-feedback-loop) for a `high_intensity`-scenario demonstration of the mechanism under both no-reinforcement and reinforcement-enabled configurations |
| Priority 1 share (seed 42, post-Issue-160) | 208 of 386 classified (53.9%) — P1 208 / P2 66 / P3 45 / KIA 67; was P1 202 / P2 72 / P3 44 / KIA 67 post-Issue-156. Every generated casualty carries an assigned priority in this run |
| DOW count (seed 42, post-Issue-160) | 1 total: r2b=1, ame_wait=0; unchanged from post-Issue-156. At n≤2 a change of one carries no statistical weight; see the 50-replication row below for the figure that does |
| DOW rate — P1 p_max (logistic) | 2.3% ceiling (Falklands 1982 calibration) |
| DOW rate — P2 p_max (logistic) | 1.9% ceiling (Falklands 1982 calibration) |
| DOW rate — P3 flat | 0.1% (structural placeholder; P3 never evacuated) |
| Mean DOW/run (50-rep, seed=NULL, post-Issue-160) | 0.80; 95% CI [0.58, 1.02]. DOW/WIA rate 0.538% (95% CI [0.387%, 0.689%]); treated-cohort DOW rate (casualties reaching R2B or R2E) 0.530% (95% CI [0.382%, 0.678%]), the quantity the historical Ajax Bay target of ~0.46% applies to — the interval spans the target, so `p1_p_max` and `p2_p_max` were left unchanged. Was 0.98, 95% CI [0.70, 1.26], treated-cohort 0.442% (95% CI [0.316%, 0.568%]) post-Issue-156 |
| DNBI sub-types (seed 42, post-Issue-160) | battle_fatigue=31, disease=109, nbi=30 (post-Issue-160; was battle_fatigue=43, disease=106, nbi=21 post-Issue-156) — RNG-stream-shifted despite DNBI sub-type assignment itself being unrelated code, per the single-global-RNG-stream mechanism (see provenance caveat above) |
| bf_rtd (seed 42, post-Issue-160) | 28 (post-Issue-160; was 39 post-Issue-156) |
| clinical_rtd (seed 42, post-Issue-160) | 104 (r1: 52, r2b: 41, r2e: 11) (post-Issue-160; was 101 — r1: 51, r2b: 39, r2e: 11 — post-Issue-156) |
| total_rtd (seed 42, post-Issue-160) | 132 (post-Issue-160; was 140 post-Issue-156) |
| Realised in-theatre share (seed 42, post-Issue-160) | 27.1% of 166 R2E dispositions retained in theatre under the shipped 30-day `evacuation_policy_days`. An output of the policy, not an input. Across 50 replications 26.9% (95% CI [25.8%, 27.9%]), inside the 7.6%-42.1% historical range cited in README Return to Duty. Was 30.3% of 119 dispositions (50-rep 28.9%) post-Issue-156; the disposition count rises because casualties clear R2E instead of queueing for a sortie |
| Evacuation is severity-conditioned (seed 42, post-Issue-160) | Sorting the 166 dispositions into quartiles by drawn `recovery_to_duty_days` gives evacuation rates of 0% / 100% / 100% / 100% from shortest to longest recovery (50-rep: 0% / 92.8% / 100% / 100%). An unconditioned draw would give the same rate in every quartile |
| Pre-flight critical ICU hold (seed 42, post-Issue-160) | 15 of 82 critical-route evacuees drew the ventilated pre-flight path (`critical_hold.ventilated_share = 0.15`); 13 completed within the run at a mean of 25.2 h, median 26.2 h, p90 33 h. Across 50 replications the mean is 31.9 h, median 24.3 h, p90 32 h — the ~12-day p90 tail the previous configuration produced is gone, because the holding pool that blocked step-down now clears (README Limitation L17). Was 6 of 42, mean 26.4 h single-run and mean 80.1 h across 50 replications, post-Issue-156 |
| Welch ICU-queue CMA (10 reps × 90 days, post-Issue-160) | Final cross-replication CMA 0.303 with 22.5% of increments decreasing, instantaneous mean queue peaking at 1.10; was 1.18 with 9.0% decreasing and a 3.10 peak post-Issue-156, and 30.6 with 0% decreasing before that. The `ventilated_share = 0` control run from the Issue #156 refresh was not repeated |
| R2B surgical decision point (seed 42, post-Issue-160) | 126 casualties reached it (46 operated at R2B, 80 bypassed to R2E). Supersedes the "R2B surgical candidates = 141" row carried since Issue #76, which counted a different quantity and had not been recomputed since |
| R2B surgeries (seed 42, post-Issue-160) | 46 (post-Issue-160; was 49 post-Issue-156) |
| R2E surgeries — first op (seed 42, post-Issue-160) | 123 (post-Issue-160; was 119 post-Issue-156) |
| R2E surgeries — second op (seed 42, post-Issue-160) | 73 (post-Issue-160; was 48 post-Issue-156) — more casualties survive to a second procedure once ICU and holding beds turn over faster |
| R2B pre-transport bypass (seed 42, post-Issue-160) | upstream R1-threshold bypass: 134 (was 121 post-Issue-156); at-R2B hold-full bypass: 0 (unchanged); R2B hold queue (both full): 0 (unchanged) — these are three independent, non-summing counts |
| R2B OT bypass reason (seed 42, post-Issue-160) | at-R2B subset, 80 total (was 74 post-Issue-156): team off-shift 73 (was 62), OT busy/queued 7 (was 12) |
| OT utilisation — echelon aggregate (seed 42, post-Issue-160) | R2B: 3.7%, R2E: 23.4% (was R2B: 4.2%, R2E: 13.6% post-Issue-161) |
| R2B OT utilisation — 24h room (seed 42, post-Issue-160) | T1: 5.1%, T2: 7.3% (was T1: 7.8%, T2: 5.9% post-Issue-161) |
| R2B OT utilisation — shift time (seed 42, post-Issue-160) | T1: 10.2%, T2: 14.6% — twice the 24-hour room figure above, the team being rostered 12 hours of every 24. Clears the pending flag this row has carried since Issue #73, when it read T1: 14.4%, T2: 13.9% |
| R2E OT utilisation — 24h room (seed 42, post-Issue-160) | OT1: 59.6%, OT2: 44.0% (was OT1: 62.6%, OT2: 48.3% post-Issue-156). A theatre is seized before its surgical section, so a room reads as occupied while its casualty is still queued for staff; much of this is that wait, not operating time |
| R2E OT queue ≥1 (seed 42, post-Issue-160) | OT1: 31.1% of run, OT2: 22.5% (was OT1: 30.2%, OT2: 23.9% post-Issue-156) |
| R2E surgical section utilisation (seed 42, post-Issue-160) | Section 1: 24.8%, Section 2: 53.1%, Section 3: 19.7% of rostered time; queued ≥1 for 3.14%, 23.20% and 1.81% of rostered time respectively (was 21.4/47.0/21.9% and 1.38/15.65/4.61% post-Issue-156). Section 2 is higher because it is the section rostered to the second shift and so carries the whole night-time surgical load alone |
| R2E ICU utilisation — mean (seed 42, post-Issue-160) | ICU1: 82.8%, ICU2: 87.2%, ICU3: 61.1%, ICU4: 54.2% (was ICU1: 78.7%, ICU2: 92.6%, ICU3: 61.1%, ICU4: 71.0% post-Issue-156). Across 50 replications the four-bed mean is 68.9% |
| R2E ICU queue ≥1 (seed 42, post-Issue-160) | ICU1: 23.3% of run, ICU2: 24.7%, ICU3: 0%, ICU4: 0% (was ICU1: 30.6%, ICU2: 71.1%, ICU3: 10.6%, ICU4: 39.6% post-Issue-156) |
| Transport utilisation — platform aggregate (seed 42, post-Issue-160) | HX240M: 4.7%, PMVAmb: 10.7% (was HX240M: 4.8%, PMVAmb: 10.0% post-Issue-161) |
| PMV Ambulance utilisation (seed 42, post-Issue-160) | 10.7%; max queue 0 throughout run; per-vehicle 25.1% / 6.0% / 1.0% (was 23.8% / 5.8% / 1.4% post-Issue-161) |
| HX240M utilisation (seed 42, post-Issue-160) | 4.7%; max queue 0 throughout run; per-vehicle 8.8% / 0.7% (was 9.7% / 2.3% post-Issue-161) — carries R2B→R2E mortuary road-move traffic in addition to R1→mortuary KIA |
| R2B evac team dead-heading (seed 42, Issue #73 follow-up) | R2B→R2E WIA transport models a dead-heading return leg on the R2B team's own organic evac resource (`r2b_evac_leg()`/`r2b_evac_return_leg()`), matching the R1↔R2B legs; RNG-stream-shifting, not RNG-neutral |
| R2B→R2E mortuary transport (seed 42, Issue #73 follow-up) | R2B KIA/DOW transported by road to the R2E-collocated mortuary via the shared HX2 40M fleet (`r2b_transport_kia()`, dead-heading return leg), then handed to a selected R2E team's mortuary intake (`r2e_mortuary_intake()`) |
| R2E post-op pathway (seed 42, post-Issue-160) | icu=85, hold=38 (post-Issue-160; was icu=72, hold=47 post-Issue-156) — the degraded-care share falls again as evacuees clear R2E beds sooner; see README Limitation L17. `surgery_deferred` = 6 (was 20 post-Issue-156) |
| R2E post-op DOW rate — icu vs hold (seed 42, post-Issue-160) | 0/85 vs 0/38 (single-run; was 0/72 vs 0/47 post-Issue-156). The saturated-ICU 90-day stress test from Issue #43, which confirmed the mechanism fires with the hold pathway riskier than the icu pathway, has not been re-run since |
| Role 4 demand (seed 42, post-Issue-160) | 121 strategic evacuation decisions (82 critical-route, 39 standard-route); 105 boarded and reached Role 4 by day 30 (73 critical, 32 standard), 16 still queued at R2E (9 critical, 7 standard); Role 4 peak occupancy 73.0 concurrent patients (day 21); unconstrained-baseline demand would need 29 sorties at same-day/uncapped/best-case (90/sortie) capacity. Was 83 decisions, 25 boarded, 58 queued, peak 17.0, 27 sorties post-Issue-156 |
| Strategic AME actual performance (seed 42, post-Issue-160) | C-17A Globemaster III at 36 critical / 54 standard places, all 4 of 4 scheduled sorties flown (7-day interval, 15% cancellation probability, none drawn); seats boarded per sortie window 31/27/15/0 critical and 20/10/2/0 standard; critical-pool mean wait 1.1 days (p10–p90 0.0–4.0), standard-pool mean wait 0.9 days (p10–p90 0.0–3.9); peak backlog 13 critical and 8 standard. Was 6.0 and 5.2 days at the previous 2/8 configuration. The residual wait is set by the 7-day interval, not by seats |
| AME wait-time DOW poll (seed 42, post-Issue-160) | `dow_echelon=5`, daily poll interval (`role4.ame.dow_check_interval = 1440` min); 0 deaths observed, as in every run since the poll was added; see README [AME Wait Checkpoint](README.md#ame-wait-checkpoint) for why no single-run count should be read as evidence about the mechanism's magnitude |

---

## Out of Scope for Claude

- Merging to `main` — owner only.
- Changing the casualty rate baseline scenario without raising and discussing an issue first.
- Modifying `env_data.json` schema without a corresponding issue and PR.
- Removing or replacing existing references in `README.md`, `docs/Single_Run_Analysis.md`, or `docs/Multi_Run_Analysis.md` without explicit instruction.
