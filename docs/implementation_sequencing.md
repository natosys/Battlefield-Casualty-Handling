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

Three PRs are currently open and awaiting owner merge. **Important: PRs #17 and #20 have an additive conflict on `R/replication.R` — the second to merge will need a straightforward manual resolution (combine `ot_hours` param from #17 with `warm_up_days` param from #20).**

| PR | Issue | Title | Conflict? | Notes |
|---|---|---|---|---|
| #21 | #19 | Dev Container specification | None | Independent of all simulation code — merge any time |
| #20 | #2 | Warm-up period analysis (Issue #2) | With PR #17 | Merge before or after #17; whoever goes second resolves conflict |
| #17 | #3 | Morris EE sensitivity + Sobol (Issue #3) | With PR #20 | Issue #3 is labelled `status: blocked` but PR is complete — label should be updated |

**Recommended merge order for open PRs:**

1. **PR #21** (dev container) — no dependencies, merge immediately
2. **PR #20** (warm-up, Issue #2) — merge second; no unresolved code conflicts with dev container
3. **PR #17** (Morris, Issue #3) — merge third; will require resolving the `R/replication.R` conflict with PR #20 before merge

> **Note for owner:** When merging PR #17 after PR #20, `R/replication.R` will show a conflict. The resolution is additive: `summarise_replications()` and `run_once()` need both `warm_up_days` (from #20) and `ot_hours` (from #17) in their signatures. Combine both parameters — do not discard either.

---

## 3. Issues Ready to Implement (No Open PR)

Two Phase 1 issues have no open PR but are unblocked and should be implemented next. **Both should be implemented and PRed before Phase 2 work begins.**

### Issue #24 — Variance Reduction and L'Ecuyer-CMRG RNG Streams `[Ph.1]` `status: ready`

**Why now:** The current replication framework uses `mc.set.seed = FALSE`, which provides no guarantee of non-overlapping RNG streams across parallel workers. This means replications may be correlated, which would inflate the apparent precision of every CI produced by Issues #2 and #3. Issue #24 replaces this with `RNGkind("L'Ecuyer-CMRG")` + `mc.set.seed = TRUE`, and adds antithetic variates and explicit `mc.cores` configuration.

**Dependency:** Issue #1 (MERGED). No other dependencies.

**Impact if deferred:** All multi-run KPIs from Issues #2 and #3 are computed against a potentially mis-specified RNG framework. The CIs may be incorrect. Issue #24 should be merged before the Phase 1 analyses are treated as validated results.

**Recommended action:** Implement Issue #24 and raise a PR immediately. If PRs #20 and #17 have already merged, this PR will need to rebase onto the merged state of `R/replication.R` and carry forward all parameters (`warm_up_days`, `ot_hours`, and the new RNG changes).

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

READY (implement next, before Phase 2):
  #24  [Ph.1]       Variance reduction / L'Ecuyer-CMRG            ─┐ parallel
  #22  [Ph.1]       Output Variable Register                      ─┘

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

Issue #24 changes `run_replications()` in `R/replication.R` — the same file that both PR #20 and PR #17 modify. If #24 is implemented after the existing PRs merge, the conflict is additive and straightforward: incorporate the L'Ecuyer RNG setup and `mc.cores` configuration alongside the `warm_up_days` and `ot_hours` parameters already merged.

### #23 relationship to #18

Issues #18 (force regeneration / RTD feedback loop) and #23 (Role 4 occupancy / AME sortie demand) are described in #23 as the "two halves" of the strategic evacuation causal chain. They share the same dependency profile and can be developed in parallel once Phase 1 is complete and Issue #5 (time-dependent DOW) is merged. Neither should proceed before #5, since the DOW survival function directly determines the casualty mix reaching the strategic evacuation decision point.

### #22 and #3 interaction

Issue #22 adds trajectory `set_attribute()` calls that define the KPI outputs. Issue #3 (Morris) defines its KPI extraction layer in `R/sensitivity.R`. If #3 merges before #22, the sensitivity analysis will compute Elementary Effects against the original three ad-hoc KPIs (R2E ICU queue, R2B OT queue, DOW count) rather than the doctrine-aligned KPI set from #22. The owner should decide whether to accept this limitation or treat #22 as a blocker for finalising the Morris analysis interpretation.
