# Implementation Sequencing — Battlefield Casualty Handling Simulation

**Prepared:** 2026-06-21  
**Status:** Live — update as PRs merge and issues close

---

## 1. Current Completion State

| Issue | Title | Status |
|---|---|---|
| #12 | Codebase restructure and entry point refactor | **MERGED** |
| #8 | [HOTFIX] R2E surgical team seizure bug | **MERGED** |
| #1 | [Ph.1] Multi-run replication framework | **MERGED** |

All three pre-phase and Phase 1 foundation tasks are complete. The codebase has a working CLI entry point (`run.R`), a modular `R/` structure, and an active `mclapply`-based replication framework.

---

## 2. Open Pull Requests

Three PRs are currently open and awaiting owner merge. **Do not run the test plans for PRs #20 or #17 yet** — see the note on Issue #24 below first.

| PR | Issue | Title | Conflict? | Notes |
|---|---|---|---|---|
| #21 | #19 | Dev Container specification | None | Independent of all simulation code — merge any time |
| #20 | #2 | Warm-up period analysis | With PR #17 | **Hold test run until #24 is merged** (see §3) |
| #17 | #3 | Morris EE sensitivity + Sobol | With PR #20 | **Hold test run until #24 is merged** (see §3); label should be updated from `status: blocked` |

**Recommended merge order:**

1. **PR #21** (dev container) — merge immediately, no dependencies
2. **Issue #24** — implement and merge before testing #20/#17 (see §3)
3. **PR #20** (warm-up, Issue #2) — rebase onto #24, then run test plan and merge
4. **PR #17** (Morris, Issue #3) — rebase onto merged #20; resolve the additive `R/replication.R` conflict, then run test plan and merge

> **Conflict note for step 4:** When rebasing PR #17 after #20 merges, `R/replication.R` will show a conflict. The resolution is additive: `summarise_replications()` and `run_once()` need both `warm_up_days` (from #20) and `ot_hours` (from #17) in their signatures. Combine both parameters — do not discard either.

---

## 3. Issues Ready to Implement (No Open PR)

Two Phase 1 issues have no open PR but are unblocked. **Issue #24 must be implemented and merged before the test plans for PRs #20 and #17 are run.**

### Issue #24 — Variance Reduction and L'Ecuyer-CMRG RNG Streams `[Ph.1]` `status: ready` ⚠️ implement first

**Why this must precede PR testing:** PRs #20 and #17 are validated by running the simulation and inspecting multi-run outputs — Welch plots, Morris rankings, CI values. Those test runs execute in `R/replication.R`. The current framework uses `mc.set.seed = FALSE`, which provides no guarantee of non-overlapping RNG streams across parallel workers, meaning replications may be correlated and CI estimates may be artificially tight. If the test plans for #2 and #3 are run and approved against the broken RNG framework, the evidence base for those PRs is unreliable by the same logic that motivates #24.

Implementing #24 first means every test run — including the acceptance criteria for #2 and #3 — is executed against the correct statistical infrastructure. The test results are then trustworthy.

**Dependency:** Issue #1 (MERGED). No other dependencies.

**Recommended action:** Implement Issue #24, raise a PR, and merge it before running the test plans for PRs #20 and #17. Both of those PRs will then need a rebase onto #24's changes to `R/replication.R` before their test runs proceed.

---

### Issue #22 — Output Variable Register (KPI gap remediation) `[Ph.1]` `status: ready`

**Why now:** The Morris screening (Issue #3) computes Elementary Effects against a KPI vector. Without a formally defined output variable register, that KPI vector is ad-hoc and incomplete. Five timing intervals critical to planning — R2B dwell time, R2B→R2E transit time, R2E dwell time, DOW by echelon, RTD by echelon — are not computable from existing trajectory attributes. Issue #22 adds the missing `set_attribute()` calls and defines the canonical KPI set.

**Dependencies:** Issue #1 (MERGED). Logically precedes finalisation of Issue #3 analysis.

**Impact if deferred:** Morris screening results (Issue #3) will be computed against an incomplete KPI set, limiting their planning validity. Issues #5, #14, #15, and #18 all consume the KPI definitions established by #22.

**Recommended action:** Implement Issue #22 in parallel with Issue #24. If PR #17 has already merged, this PR adds trajectory attribute calls and does not conflict with the sensitivity analysis infrastructure.

---

## 4. Full Dependency-Ordered Backlog

The table below integrates the original CLAUDE.md sequence with the three new issues (#22, #23, #24).

```
COMPLETED:
  #12  [Pre-phase]  Codebase restructure                          MERGED
  #8   [HOTFIX]     R2E surgical team seizure bug                  MERGED
  #1   [Ph.1]       Multi-run replication framework                MERGED

IN REVIEW (awaiting owner merge):
  #19  [Ph.5]       Dev Container — PR #21                        ─┐
  #2   [Ph.1]       Warm-up period analysis — PR #20              ─┤ merge in order: #21, #20, #17
  #3   [Ph.1]       Morris EE sensitivity — PR #17                ─┘ (resolve replication.R conflict on #17)

IMPLEMENT BEFORE TESTING OPEN PRs:
  #24  [Ph.1]       Variance reduction / L'Ecuyer-CMRG            ← merge before running test plans for #2 / #3

IMPLEMENT IN PARALLEL WITH #24 (before Phase 2):
  #22  [Ph.1]       Output Variable Register

AFTER #1 + #2 + #3 + #22 + #24 (Phase 1 complete):
  #7   [Ph.3]       DNBI sub-categorisation                       (needs #1 + #2 only; can pull forward)
  #5   [Ph.2]       Time-dependent DOW                            ─┐
  #6   [Ph.2]       Dead-heading transport                        ─┤ parallel
  #14  [Ph.2]       Shiny app — parameter editor + Quick Run      ─┘ (needs #1 + analysis.R refactor)

AFTER #1 + #2 + #3 (all stable):
  #4   [Ph.3]       Individual resource seizure                   (largest structural change; gate until Ph.1 stable)

AFTER #14 + #1 + #2 + #3:
  #15  [Ph.5]       Shiny app — Full Analysis mode (multi-run CI)

AFTER #1 + #2 + #5:
  #9   [Ph.4]       MASCAL stochastic injection
  #18  [Ph.4]       Endogenous casualty generation / RTD loop     (needs #1 + #2 at minimum)
  #23  [Ph.4]       Strategic evacuation demand / Role 4 load     (needs #1 + #2; downstream complement of #18)

AFTER #1 + #2 + #5 + #8 (HOTFIX already merged):
  #10  [Ph.4]       Comparative scenario runner
```

---

## 5. Phase 1 Completion Gate

Phase 2 and 3 work should not begin until all five Phase 1 issues are merged and stable. The gate criteria are:

| Issue | Criterion |
|---|---|
| #1 | Replication framework active; `get_mon_resources()` returns `replication` column ✓ DONE |
| #2 | Warm-up filter applied; `WARM_UP_DAYS` constant in use; Welch plot generated |
| #3 | Morris ranking CSV produced; μ\*/σ plots written; Sobol follow-up available |
| #22 | All five timing attributes traceable in arrival monitor; KPI register documented |
| #24 | `RNGkind("L'Ecuyer-CMRG")` active; `mc.cores` set to physical core count; antithetic variates applied |

Until all five are green, KPI point estimates and CIs from multi-run analyses are provisional.

---

## 6. Issue-Label Housekeeping

The following label states are outdated and should be updated when the corresponding PR merges:

| Issue | Current label | Correct label after action |
|---|---|---|
| #3 | `status: blocked` | `status: ready` — PR #17 is complete; label predates the PR being opened |
| #2 | `status: ready` | Close on merge of PR #20 |
| #19 | `status: ready` | Close on merge of PR #21 |
| #5, #6, #7 | `status: blocked` | Update to `status: ready` once Phase 1 gate is passed (#7 can move to ready after #1 + #2) |
| #14 | `status: blocked` | Update to `status: ready` once `R/analysis.R` refactor (part of Issue #1/restructure) is confirmed stable |
| #4 | `status: blocked` | Remains blocked until #1 + #2 + #3 all stable |

---

## 7. Notes on New Issues

### #24 vs open PRs #20 / #17

Issue #24 changes `run_replications()` in `R/replication.R` — the same file that both PR #20 and PR #17 modify. By merging #24 first, PRs #20 and #17 rebase onto a corrected RNG foundation and their test runs produce trustworthy output. The rebases are additive: `warm_up_days` (from #20) and `ot_hours` (from #17) are new parameters alongside the L'Ecuyer and `mc.cores` changes from #24 — none of the changes overlap semantically.

### #23 relationship to #18

Issues #18 (force regeneration / RTD feedback loop) and #23 (Role 4 occupancy / AME sortie demand) are described in #23 as the "two halves" of the strategic evacuation causal chain. They share the same dependency profile and can be developed in parallel once Phase 1 is complete and Issue #5 (time-dependent DOW) is merged. Neither should proceed before #5, since the DOW survival function directly determines the casualty mix reaching the strategic evacuation decision point.

### #22 and #3 interaction

Issue #22 adds trajectory `set_attribute()` calls that define the KPI outputs. Issue #3 (Morris) defines its KPI extraction layer in `R/sensitivity.R`. If #3 merges before #22, the sensitivity analysis will compute Elementary Effects against the original three ad-hoc KPIs (R2E ICU queue, R2B OT queue, DOW count) rather than the doctrine-aligned KPI set from #22. The owner should decide whether to accept this limitation or treat #22 as a blocker for finalising the Morris analysis interpretation.
