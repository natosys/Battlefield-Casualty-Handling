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
| `controller.R` | Shiny GUI for interactive `env_data.json` parameter editing |
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
- **Only the repository owner can merge to `main`.** Do not merge, do not raise PRs against `main` without being asked.
- Branch naming: `feature/issue-<number>-<short-description>` (e.g., `feature/issue-1-multi-run-replication`).
- Each GitHub Issue corresponds to one feature branch and one PR.

### Sequence

1. Raise a GitHub Issue describing the work (see Issue Format below).
2. Create a feature branch from `main`.
3. Implement the changes.
4. Update the README as part of the same PR (see README Maintenance below).
5. Open a PR against `main` with a test plan (see Test Plans below).
6. Await owner merge — do not self-merge.

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

- Author (Year). Title. Source. URL — <annotation explaining relevance>
```

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
- Prefer open-access sources. Where a paywalled source is used, note it and provide the closest open-access equivalent if one exists.
- Use the numbered reference format already established in the README (`[[n]](#References)`).
- New references are appended to the References section in the order they first appear in the text.

### Source Prioritisation

When selecting methods or parameter values, prefer sources in this order:
1. ADF doctrine (ADDP, LWP, etc.)
2. NATO/allied doctrine (AJP, FM, ATP)
3. Peer-reviewed open-access research
4. Peer-reviewed paywalled research (note and provide alternative where possible)
5. Grey literature / technical reports (DTIC, arXiv) — cite with access date
6. Informed estimation — must be explicitly flagged as such with derivation documented

### Limitations Section

The README must maintain a Limitations section that:
- Identifies what the model does not represent and why
- Rates the impact of each limitation on findings (High / Medium / Low)
- Notes whether the limitation is addressed in the action plan and under which issue

---

## Implementation Phases

Development follows the sequencing defined in `BCH_Simulation_Action_Plan.md`. Do not skip ahead — later phases depend on earlier foundations.

### Phase 1 — Statistical Foundation (Issues 1, 2, 3)
Multi-run replication → Welch warm-up analysis → Morris sensitivity screening.
*All subsequent results must use the Phase 1 replication framework.*

### Phase 2 — Model Fidelity (Issues 5, 6, 8)
R2E surgical team seizure fix (Issue 8 first — bug fix) → dead-heading transport → time-dependent DOW.

### Phase 3 — Structural Refactoring (Issues 4, 7)
Individual resource modelling (requires `BCH_Task_Role_Allocation.md`) → DNBI sub-categorisation.

### Phase 4 — Scenario Expansion (Issues 9, 10)
MASCAL stochastic injection → comparative scenario runner (Falklands / Vietnam / Okinawa).

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

| Metric | Baseline value |
|---|---|
| Total casualties (30 days) | 401 |
| WIA (combat + support) | 155 |
| KIA (combat + support) | 70 |
| DNBI (combat + support) | 176 |
| Priority 1 share | ~51.6% |
| DOW rate (P1 at R1) | 5% (flat) |
| DOW rate (P2 at R1) | 2.5% (flat) |
| DOW rate (R2B/R2E) | 1% (flat) |

---

## Out of Scope for Claude

- Merging to `main` — owner only.
- Changing the casualty rate baseline scenario without raising and discussing an issue first.
- Modifying `env_data.json` schema without a corresponding issue and PR.
- Removing or replacing existing references in the README without explicit instruction.
