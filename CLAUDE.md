# CLAUDE.md — Battlefield Casualty Handling Simulation

## Project Purpose

This is an **academic research project** producing a Discrete Event Simulation (DES) of deployed battlefield casualty handling. The simulation is written in R using the `simmer` package and is intended to provide evidence-based options to military planners for improving health outcomes in Large Scale Combat Operations (LSCO).

All work must meet academic research standards: reasoning must be explicit, sources must be cited, and limitations must be acknowledged. The README functions as the thesis document — it is kept current with the code and written to the standard of a published academic paper.

---

## Repository Structure

| File / Directory | Purpose |
|---|---|
| `Battlefield Casualty Handling.R` | Main simulation engine — trajectories, resource seizure, casualty routing |
| `data_import.R` | Reads `env_data.json` and builds the simmer environment |
| `single-run_analysis.R` | Analysis and visualisation pipeline (currently single-run; being extended) |
| `app.R` | Shiny console — Configure/Run/Analyse workflow for interactive `env_data.json` parameter editing, Quick Run, Full Analysis (multi-run with 95% CI), and Sensitivity Screening (Morris/Sobol) execution (Issues #14, #15) |
| `controller_legacy.R` | Superseded by `app.R`; retained for reference only |
| `env_data.json` | All simulation parameters — populations, resources, distributions, schedules |
| `distribution_graphs.R` | Triangle distribution demonstration |
| `README.md` | Primary academic document — introduction, literature review, methodology, results, limitations, references |
| `README_inputs.md` | Auto-generated environment summary (do not edit manually) |
| `STYLE_GUIDE.md` | R code style conventions — follow at all times |
| `scripts/check_env_data_summary.R` | Regenerates the `README_inputs.md` environment summary |
| `scripts/check_markdown.R` | Maintains README TOC and reference links |

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
4. Update the README as part of the same PR (see README Maintenance below).
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

If the merged PR modified `Battlefield Casualty Handling.R` or `env_data.json` in a way that shifts the RNG stream or alters stochastic outputs, re-run the simulation at seed 42 and update the Key Parameters table at the bottom of this file. Document the change in the action plan entry.

**4. Regenerate `README_inputs.md` (if `env_data.json` changed)**

If the merged PR modified `env_data.json`, run `scripts/check_env_data_summary.R` to regenerate `README_inputs.md` and include the updated file in the chore PR.

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
4. **Regression checks** — confirm that outputs from unmodified pathways remain consistent with the baseline single-run (seed 42) values documented in the README.
5. **Known limitations** — anything the test plan does not cover, and why.

Example entry:

```
### Test: Multi-replication output (Issue 1)
**Setup:** n_iterations = 10, n_days = 30, seed = NULL (independent per replication)
**Steps:**
1. Source `Battlefield Casualty Handling.R`
2. Inspect `queue_summary` output object
3. Confirm 10 rows present in replication-level resource monitor output
**Expected:** `mean_queue` values differ across replications; p10 < mean < p90 for at least one resource
**Regression:** Total casualty count per replication should fall within ±15% of seed 42 baseline (401 casualties)
```

---

## README Maintenance

The README is the primary academic output of this project. It must be updated **as part of every PR** — not retrospectively.

### What to update per PR

| Section | Update trigger |
|---|---|
| Abstract | When the scope or findings of the simulation change materially |
| Simulation Design | When trajectories, resource logic, or distributions are changed |
| Simulation Analysis | When new results are generated (replace or supplement existing analysis) |
| Limitations | When a known gap is addressed (remove or update) or a new one is identified |
| Further Development | Remove completed items; add newly identified items |
| References | Add any new sources used in the implementation |

### Style

- Write in academic third-person prose. Avoid first person.
- All parameters, probabilities, and distributions must be cited to their source.
- New methods introduced must reference the algorithm or statistical technique by name, with citation (e.g., "Morris Elementary Effects screening (Morris, 1991) was applied using R's `sensitivity` package").
- Tables and flowcharts must be kept synchronised with the code.

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

### Inline — throughout the README

Where a specific parameter, role allocation, or pathway decision rests on an assumption rather than validated evidence, document it inline using a named assumption block:

```markdown
> **MODEL ASSUMPTION — [Short Name]:** <Description of the assumption.>
> **Basis:** <Source or reasoning used to derive the assumption.>
> **Uncertainty:** High / Medium / Low
> **Consequence if wrong:** <What changes in the model output if this assumption is incorrect.>
```

Example:
> **MODEL ASSUMPTION — NO Flex to Surgical Roles at R2B:** Nursing Officers from the R2B emergency section are assumed to flex to scrub and circulating roles during surgery when not occupied with concurrent resuscitation.
> **Basis:** Derived from ADF austere deployment practice; no open-access doctrinal source explicitly confirms this for forward R2B contexts.
> **Uncertainty:** High
> **Consequence if wrong:** R2B surgical capacity requires dedicated surgical NOs not present in the current establishment; surgical throughput would be zero whenever emergency NOs are occupied.

### Holistic — Limitations section

The README `Limitations` section (to be added if not present, or maintained if it exists) provides a consolidated review of all model assumptions, organised by impact. It should cross-reference the inline blocks. Update this section whenever an assumption is added, resolved, or reclassified.

---

## Academic Standards

### Citations

- All parameters must be cited. If a value is estimated or derived, state this explicitly and describe the derivation.
- **All sources must be openly accessible on the internet without a paywall.** Paywalled journal articles, restricted doctrine, and books with no freely available full text must not be used.
- Use the numbered reference format already established in the README (`[[n]](#References)`).
- New references are appended to the References section in the order they first appear in the text.

### Reference List Rules

These rules apply to every entry in the README References section and to references listed in GitHub Issues:

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

### Limitations Section

The README must maintain a Limitations section that:
- Identifies what the model does not represent and why
- Rates the impact of each limitation on findings (High / Medium / Low)
- Notes whether the limitation is addressed in the action plan and under which issue

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

Follow `STYLE_GUIDE.md` at all times. Key points:

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

| Metric | Baseline value |
|---|---|
| Total casualties (30 days) | 400 |
| WIA (combat + support) | 154 |
| KIA (combat + support) | 70 |
| DNBI (combat + support) | 176 |
| Priority 1 share (of 400 total) | ~53.3% (pre-dates the Issue #73 follow-up RNG-stream shift; not yet re-derived against a current-code baseline — post-Issue-76 seed-42 priority split is P1 202/P2 74/P3 54/KIA 70, i.e. 50.5% P1, but this reflects both the Issue #73 follow-up and Issue #76 shifts combined and should not be treated as isolating either) |
| DOW count (seed 42) | 0, down from 3 (all previously at R2B) pre-Issue-76 — RNG-stream-shifted by the surgery-duration change, not a change in DOW logic itself; mean ~0.70/run (50-rep, seed=NULL) figure predates both the Issue #73 follow-up and Issue #76 RNG-stream shifts and has not been refreshed |
| DOW rate — P1 p_max (logistic) | 2.3% ceiling (Falklands 1982 calibration) |
| DOW rate — P2 p_max (logistic) | 1.9% ceiling (Falklands 1982 calibration) |
| DOW rate — P3 flat | 0.1% (structural placeholder; P3 never evacuated) |
| Mean DOW/run (50-rep, seed=NULL) | ~0.70 (0.45% of WIA); 95% CI [0.41, 0.95] — predates the Issue #73 follow-up and Issue #76 RNG-stream shifts; not yet refreshed |
| DNBI sub-types (seed 42) | battle_fatigue=38, disease=99, nbi=39 (was battle_fatigue=46, disease=110, nbi=20 pre-Issue-76) |
| bf_rtd (seed 42) | 35 (was 39 pre-Issue-76) |
| clinical_rtd (seed 42) | 113 (r1: 65, r2b: 39, r2e: 9) (was 95 — r1: 48, r2b: 42, r2e: 5 — pre-Issue-76) |
| total_rtd (seed 42) | 148 (was 134 pre-Issue-76) |
| R2B surgical candidates (seed 42, post-Issue-76) | 141 (was 132 pre-Issue-76) |
| R2B surgeries (seed 42, post-Issue-76) | 62 (was 47 pre-Issue-76) |
| R2E surgeries — first op (seed 42, post-Issue-76) | 142 (was 132 pre-Issue-76) |
| R2B pre-transport bypass (seed 42, post-Issue-76) | upstream R1-threshold bypass: 115 (was 132); at-R2B hold-full bypass: 0 (was 1); R2B hold queue (both full): 0 (unchanged) — these are three independent, non-summing counts (see provenance caveat above for a correction to how this row was previously presented) |
| R2B OT bypass reason (seed 42, post-Issue-76) | at-R2B subset, 78 total (was 82): team off-shift 73 (was 76), OT busy/queued 5 (was 6) |
| R2B OT utilisation — 24h room (seed 42, post-Issue-76) | T1: 6.7%, T2: 9.3% (was T1: 8.5%, T2: 7.4% pre-Issue-76) |
| R2B OT utilisation — shift time (seed 42, post-Issue-73) | T1: 14.4%, T2: 13.9% — pre-dates Issue #76; not recomputed in this refresh |
| R2E OT utilisation — 24h room (seed 42, post-Issue-76) | OT1: 40.8%, OT2: 16.9% (was OT1: 47.9%, OT2: 24.4% pre-Issue-76) |
| R2E ICU utilisation — mean (seed 42, post-Issue-76) | ICU1: 76.6%, ICU2: 62.1%, ICU3: 59.6%, ICU4: 47.0% (was ICU1: 76.8%, ICU2: 74.8%, ICU3: 57.8%, ICU4: 52.8% pre-Issue-76) |
| R2E ICU queue ≥1 (seed 42, post-Issue-76) | ICU1: 11.2% of run, ICU2: 2.2% of run, ICU3: 0% of run, ICU4: 0% of run (was ICU1: 15.3%, ICU2: 2.3%, ICU3: 0%, ICU4: 0% pre-Issue-76) |
| PMV Ambulance utilisation (seed 42, post-Issue-76) | 10.4% (was 10.8%); max queue 0 throughout run; per-vehicle 25.5% / 5.3% / 0.6% (was 26.2% / 5.2% / 0.9%) |
| HX240M utilisation (seed 42, post-Issue-76) | 4.8% (was 5.2%); max queue 0 throughout run; per-vehicle 8.6% / 1.0% (was 9.1% / 1.3%) — carries R2B→R2E mortuary road-move traffic in addition to R1→mortuary KIA |
| R2B evac team dead-heading (seed 42, Issue #73 follow-up) | R2B→R2E WIA transport models a dead-heading return leg on the R2B team's own organic evac resource (`r2b_evac_leg()`/`r2b_evac_return_leg()`), matching the R1↔R2B legs; RNG-stream-shifting, not RNG-neutral |
| R2B→R2E mortuary transport (seed 42, Issue #73 follow-up) | R2B KIA/DOW (70 KIA + 0 DOW = 70 of 400 casualties post-Issue-76, was 70 KIA + 3 DOW = 73 pre-Issue-76) transported by road to the R2E-collocated mortuary via the shared HX2 40M fleet (`r2b_transport_kia()`, dead-heading return leg), then handed to a selected R2E team's mortuary intake (`r2e_mortuary_intake()`) |
| R2E post-op pathway (seed 42, post-Issue-76) | icu=110, hold=31 (ICU saturated, P1 override); surgery_deferred=13 (ICU saturated, P2+) (was icu=108, hold=24, surgery_deferred=10 pre-Issue-76) |
| R2E post-op DOW rate — icu vs hold (seed 42, post-Issue-76) | 0/110 vs 0/31 (single-run; was 0/108 vs 0/24 pre-Issue-76; saturated-ICU 90-day stress test from Issue #43 confirms mechanism fires, hold pathway > icu pathway — stress test predates this refresh and has not been re-run) |
| Mean DOW/run — pre- vs post-Issue-43 (50-rep, seed=NULL) | 0.84 (95% CI [0.58, 1.10]) → 1.00 (95% CI [0.74, 1.26]); CIs overlap (not significant at n=50); +0.10/run attributable to the new post-op checkpoint alone (5/50 reps) — predates the Issue #73 follow-up and Issue #76 RNG-stream shifts; not yet refreshed |
| R2E post-op DOW rate — icu vs hold (50-rep, Issue #43) | icu: 3/5,085 (0.06%); hold: 2/1,223 (0.16%) — hold ≈2.8× icu, consistent with intended design at real (non-stress-tested) parameters — predates the Issue #73 follow-up and Issue #76 RNG-stream shifts; not yet refreshed |
| R2E ICU utilisation — mean (50-rep, pre- vs post-Issue-43) | 74.1% → 60.2% — predates the Issue #73 follow-up and Issue #76 RNG-stream shifts; not yet refreshed |

---

## Out of Scope for Claude

- Merging to `main` — owner only.
- Changing the casualty rate baseline scenario without raising and discussing an issue first.
- Modifying `env_data.json` schema without a corresponding issue and PR.
- Removing or replacing existing references in the README without explicit instruction.
