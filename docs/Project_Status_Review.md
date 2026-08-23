# Battlefield Casualty Handling — Project Status Review

*Prepared 23 August 2026, against `main` at commit `edd6285` (post-merge of PR #229).*

## Purpose and Method

This document records a repository-wide status review conducted after the merge of
Issue #155, the terminal canonical re-run that rebuilt every published figure from
one code state in the project's pinned Dev Container. With the planned five phases
substantially complete and three issues open, the review establishes what the project
has achieved, where it falls short of the standards `CLAUDE.md` sets for it, and what
sequence of work would bring it to publication.

The review is **static**. R is not installed in the environment it was conducted in,
so no simulation, regression check or analysis pipeline was executed. Every finding
below is drawn from reading the source, the configuration and the three academic
documents, and from mechanical checks over their text. Findings that would require
execution to confirm are marked as such, and re-running the fifteen regression check
scripts is the first task of the plan in [Phase 6](#phase-6--code-review-and-standards).

---

## Overall Assessment

The project is in good health and closer to publication than most work of its kind.
The simulation code is modular, the parameterisation is externalised and cited, the
statistical apparatus (replication framework, Welch warm-up, Morris screening, Sobol
decomposition with a measured noise floor) is unusually complete for a single-author
model, and the documentation discipline is real rather than nominal: the stable `L<n>`
gap identifiers resolve without a single dangling citation across all three academic
documents, and the action plan is current to the day.

The weaknesses are concentrated in three places, and none of them is in the simulation
logic itself:

1. **The academic documents are written in a changelog voice.** Both analysis papers
   spend substantial space narrating how their own figures have changed across issues
   and which earlier claims are withdrawn. That is excellent internal record-keeping
   and it is not what a published paper looks like.
2. **The single-run / multi-run split is by document title, not by method.** Roughly a
   third of the "single-run" paper reports multi-replication results with confidence
   intervals.
3. **There is no automated verification of anything.** Fifteen regression check scripts
   exist and nothing runs them. There is no `.github/` directory at all.

Against these, a set of small, concrete defects has accumulated in the documents that
would each be caught in peer review. They are listed below and are cheap to fix.

---

## Findings

Severity reflects consequence for the project's stated goal of publishable academic
output, not engineering risk in isolation.

### Critical

**C1 — Every image in `README.md` is a broken link.** All ten image references in
`README.md` use a `../images/` prefix, but `README.md` sits at the repository root, so
the path resolves outside the repository. None of them render on GitHub. The twenty
references in `docs/` are correct, because `../images/` is right from `docs/`; the
README's paths appear to have been copied from a document that lived in `docs/` and
never re-based. This affects the project's most-read document, including the tactical
diagram (`README.md:221`), the DOW survival function (`:737`), the Welch plot (`:1158`)
and all seven Morris plots (`:1554`-`:1566`). Fix: strip `../` from those ten lines, and
extend `scripts/check_markdown.R` to validate link and image targets, which it currently
does not do (it checks heading anchors only).

**C2 — Two references are duplicated, and one duplicate pair carries conflicting
author lists.** `README.md` reference `[9]` and reference `[19]` are the same *Military
Review* article, but `[9]` gives the full eleven-author list while `[19]` gives
"Izaguirre, M. K., Lopez, J. A., & Smith, T. R." Those authors are not the article's
authors. References `[41]` and `[63]` are likewise the same Law (2020) Winter Simulation
Conference paper listed twice. A misattributed reference is the single most damaging
defect a reviewer can find; this needs correcting before anything else in the reference
list is touched.

### High

**H1 — Two references violate the project's own open-access rule.** `CLAUDE.md` states
that textbooks and paywalled journal articles must not be used and that every entry must
carry a URL and a retrieval date. `README.md:2338` cites Banks, Carson, Nelson & Nicol,
*Discrete-Event System Simulation* (Pearson, 4th ed.), a textbook with no URL.
`README.md:2344` cites Gafarian, Ancker & Morisaku (1978) in *Naval Research Logistics
Quarterly*, a paywalled Wiley journal, with no URL. Both need open-access replacements
or removal, and the claims resting on them need re-sourcing. Reference `[15]` (the
simmer JSS paper) is open access but lacks a retrieval date.

**H2 — Reference `[67]` is never cited.** Puy, Becker, Lo Piano & Saltelli (2022) on
total-order estimators appears in the reference list but no `[[67]]` citation exists in
the text. Either cite it (it is directly relevant to
`scripts/compare_sobol_estimators.R`) or remove it.

**H3 — The single-run paper reports multi-replication results.** `docs/Single_Run_Analysis.md`
is scoped in its own abstract as "the illustrative single-run (seed 42, 30 simulated
days) analysis", but at least four of its sections report replicated experiments with
confidence intervals: the 50-replication ICU-pathway validation (line 183), the transport
fleet-size sweep (lines 209-225), the forward ICU share decision frontier (lines 229-233),
and the mass casualty stress test at 10 replications under a non-default configuration
(line 334). The confidence-interval machinery those sections depend on is described in
the *other* paper. A reader of either document alone gets an incoherent methods picture.

**H4 — Both papers are written as changelogs.** `docs/Multi_Run_Analysis.md` devotes
large parts of its Interpretation and Conclusion to comparing current figures against
superseded ones ("the headline ratio falls from roughly 180 to roughly 37", "the earlier
reading ... is withdrawn", "supersedes the `high_intensity` figures quoted in the two
paragraphs before this one"). `docs/Single_Run_Analysis.md` carries twenty-two `Issue #N`
references, including in its Conclusion, plus three blockquoted maintainer notes
("Provenance note (Issue #109)", "Reproducibility note", "Note on configuration").
`CLAUDE.md` already bans this framing in the README's design sections; the same principle
has not been applied to the analysis documents. A published paper states current findings
and confines methodological history to a methods or limitations statement. The revision
history belongs in `docs/BCH_Simulation_Action_Plan.md`, which already holds it.

**H5 — No automated verification exists.** The repository contains fifteen
`scripts/check_*.R` regression checks, several of them guarding properties that are
genuinely subtle (replication independence, ICU time conservation, measurement
reproducibility, DOW calibration against historical anchors). Nothing runs them. There is
no `.github/` directory, no workflow, no pre-commit hook, and no single command that
executes the suite. Their present value is therefore only as documentation of intent,
and there is no evidence in the repository that they currently pass.

### Medium

**M1 — Hand-rolled global save/restore is not exception-safe.** `env_data`, `day_min`
and `counts` are mutated with `<<-` and restored manually at
`R/analysis.R:3100-3128` and `:3309-3352`, `R/sensitivity.R:1240/1301` and `:1679/1771`,
and `R/scenario_runner.R:119-121`. None of these restores is wrapped in `on.exit()`, so
any error inside a sweep or screen leaves the session's global configuration silently
clobbered for every subsequent run. The blast radius is limited in the Shiny app, which
correctly shells out to `scripts/shiny_worker.R` in a fresh process, but it is real in an
interactive RStudio session and in any script that runs two sweeps in sequence. Fix:
wrap each save/restore in `on.exit()`, or better, thread configuration through as an
argument rather than a global.

**M2 — Three functions are too large to review.** `server` in `app.R:1635` is 2,285
lines; `analyse_run` in `R/analysis.R:564` is 1,410; `analyse_replications` in
`R/analysis.R:2085` is 816; `r2e_treat_wia` in `R/trajectories.R:1495` is 808. These are
not badly written — the comment density is good and the roxygen is present — but a
1,400-line function that performs data preparation, twenty plots, fifteen CSV writes and
KPI computation cannot be reasoned about as a unit, and it is the main obstacle to the
detailed code review the next phase calls for.

**M3 — `R/analysis.R` has essentially no error handling.** The project's largest module
(3,364 lines) contains zero `stop()` calls, zero `tryCatch`, and four `stopifnot`. It
consumes monitoring data frames while assuming column presence throughout. `app.R`, at
3,920 lines and taking arbitrary user parameter edits, has one `stop()` and two
`tryCatch`. A malformed configuration surfaces as an R subscript error rather than a
diagnosable message.

**M4 — `docs/STYLE_GUIDE.md` does not cover what the code review needs.** The guide is
109 lines, emoji-headed, and repeats its "Function & Trajectory Documentation" heading
twice with the second occurrence empty. It specifies roxygen headers, branch comments,
resource naming and snake_case, and nothing else. It is silent on line length, function
length, the assignment operator, pipe style, magic numbers, global state, error handling,
file organisation and the standard for regression check scripts. Since the first item of
the proposed next phase is "establishment and implementation of code standards", this
document is the deliverable that has to come first.

**M5 — The Further Development scan table disagrees with three of its own entries.**
The table at `README.md:2183` lists L17 as "Strategic evacuation backlog blocks R2E beds"
where the entry reads "R2E holding beds carry recovery and evacuation on one pool"; L18
as "Morris screening precision and response coverage" against "Screening resolution and
response coverage"; and L22 as "DOW calibration target is a bounded treated-cohort rate"
against "The died-of-wounds calibration target is a bounded treated-cohort rate". The
table's first column also has an empty header. Separately, entry L22 runs to four
paragraphs where every neighbouring entry is one, which `CLAUDE.md`'s own length rule
flags as a sign that something is being explained twice.

**M6 — Ten `wip/*` branches remain on the remote.** Issue #227 records this and is
unactioned: `wip/issue-155-morris-r20-results`, `wip/issue-155-sobol-cache`,
`wip/issue-155-sobol-cache-n200`, `wip/issue-155-sobol-cache-n64`,
`wip/issue-155-sobol-results-n64`, `wip/issue-155-sobol-results-r20top5`,
`wip/screen-cache-fake-test`, `wip/screen-cache-issue-155-noise-floor`,
`wip/screen-cache-morris-r20`, `wip/screen-cache-morris-r20-rep4`. The evidence set they
checkpointed is now tracked under `data/sensitivity/`, so they are safe to delete once
that is confirmed.

### Low

**L-a — Nine figures carry the placeholder alt text `![Alt text]`.** Eight in
`docs/Single_Run_Analysis.md` (lines 44, 94, 100, 118, 120, 167, 185, 187) and one in
`README.md` (line 221). Several of those figures also have no prose caption following
them, which `CLAUDE.md` requires.

**L-b — Spelling is inconsistent between British and US forms.** The documents use
"utilisation" and "stabilisation" throughout but also contain "stabilization" (four in
`README.md`, two in `docs/Single_Run_Analysis.md`), "utilization", "analyzed",
"optimized", "prioritization", "modeling" and "stabilizing". Pick one and enforce it.

**L-c — 204 em dashes remain across the three documents**, against a `CLAUDE.md` rule
banning them (150 in `README.md`, 45 in `docs/Single_Run_Analysis.md`, 9 in
`docs/Multi_Run_Analysis.md`). The rule is scoped to "new or edited prose", so these are
grandfathered, but a publication pass is the natural point to clear them.

**L-d — `1440` appears as a literal 93 times** across the R sources despite the
`day_min` global existing for exactly that purpose, concentrated in `R/analysis.R` (66
occurrences).

**L-e — `CLAUDE.md` has minor documentation drift.** Its Repository Structure table
omits `scripts/check_arrival_rate_fidelity.R` and `docs/Getting_Started.md`, both of
which exist and are referenced elsewhere. `docs/BCH_Simulation_Action_Plan.md:1852`
points the reader to the "Issues In Review" section for detail, but that section is now
correctly empty.

**L-f — Dead assignments.** `R/analysis.R:583-586` contains `attributes <- attributes_raw`
and `resources <- resources_raw`, aliases that serve no purpose.

---

## What the Review Found Working Well

These are recorded so that the next phase does not disturb them.

- **The `L<n>` gap identifier discipline holds exactly.** Every identifier cited in the
  three academic documents resolves to a defined entry, with no dangling references and
  no reuse, across sixteen live entries and eleven retired ones.
- **The provenance discipline is exemplary.** `CLAUDE.md`'s Key Parameters note records
  precisely which commit and which container every published figure derives from, and
  resolves twenty-one earlier per-issue caveats with a byte-for-byte reproduction result
  rather than an assertion.
- **The artifact separation introduced by Issue #154 is sound.** `run.R` cannot write a
  tracked baseline file without `--refresh-baseline`, and that flag refuses
  `--iterations > 1`, which is what makes the tracked evidence set structurally incapable
  of describing a mixture of runs.
- **The Shiny app's concurrency design is correct and well justified.** Shelling out to
  `scripts/shiny_worker.R` via `system2()` rather than nesting `mclapply` inside a
  `future()` avoids two distinct real failure modes, both documented at the call site.
- **The sensitivity work goes past the usual stopping point.** Measuring the replication
  noise floor, testing the decomposition under three pick-freeze estimators, and
  bootstrapping the design rather than the indices to test parameter separation are all
  steps most published sensitivity analyses omit.

---

## Recommended Plan

The three items proposed are the right ones and are already in dependency order: the
code standards work must precede the paper work, because the papers cite function names
and file paths that the refactor will move. What follows adds a short preparatory phase,
because two of the critical findings above are cheap and should not wait behind a
refactor, and because there is currently no evidence that the regression suite passes.

### Phase 6 — Code Review and Standards

*Goal: the codebase is verifiably correct, mechanically checked, and governed by a style
guide detailed enough to review against.*

**6.0 — Verification baseline (do first, blocks everything).** Run all fifteen
`scripts/check_*.R` regression checks in the pinned Dev Container and record the result.
Nothing else in this plan is trustworthy until it is known which of them pass. Any
failure becomes a `[Ph.6 · BUG]` issue ahead of the rest of the phase.

**6.1 — Repair the reference list (C2, H1, H2).** Correct the misattributed authors on
`[19]`, merge the two duplicate pairs and renumber, replace or remove the textbook and
the paywalled article, cite or drop `[67]`, add the missing retrieval date. Renumbering
touches every `[[n]]` citation in `README.md`, so this is done as one PR and verified
mechanically.

**6.2 — Fix the broken image paths and extend the markdown checker (C1, L-a).** Strip
`../` from the ten `README.md` image references. Extend `scripts/check_markdown.R` to
resolve every link and image target against the filesystem and exit non-zero on a miss,
so the class of defect cannot recur. Replace the nine placeholder alt texts and add prose
captions where they are missing.

**6.3 — Write the code standard.** Replace `docs/STYLE_GUIDE.md` with a document that
covers what a reviewer actually needs to check: line length, maximum function length,
assignment and pipe style, roxygen completeness (every exported function, every `@param`,
`@return` and `@details`), the rule on magic numbers, the rule on global state, error
handling and input validation expectations, the structure of a regression check script,
and the commenting standard for `branch()` blocks that the current guide gestures at.
Keep the existing naming conventions table; drop the emoji and the duplicated heading.

**6.4 — Mechanise the standard.** Add `lintr` with a configuration expressing the rules
from 6.3 that a linter can express, and a `scripts/run_all_checks.R` that executes the
regression suite and the linter and returns a single exit code. Add a GitHub Actions
workflow running it on every PR against `main`. This closes H5 and is the single
highest-leverage item in the plan: it converts fifteen scripts of documented intent into
an actual gate.

**6.5 — Apply the standard.** Decompose the four oversized functions (M2) along the seams
they already have — `analyse_run` splits cleanly into data preparation, per-domain
summary, plotting and writing; `server` splits by tab. Convert the manual global
save/restore sites to `on.exit()` or to argument threading (M1). Add input validation at
the module boundaries of `R/analysis.R` and `app.R` (M3). Replace the literal `1440`s
(L-d) and remove the dead aliases (L-f). Every step here is behaviour-preserving and must
be demonstrated so: the seed-42 baseline must reproduce byte for byte, which 6.0 and 6.4
make checkable.

**6.6 — Housekeeping.** Delete the ten `wip/*` branches (M6, Issue #227), correct the
three Further Development table titles and the empty column header, trim L22 to the
length of its neighbours (M5), and close the `CLAUDE.md` drift (L-e).

### Phase 7 — Single-Run Analysis to Publication Standard

*Goal: `docs/Single_Run_Analysis.md` reads as a paper, and every figure in it is current.*

**7.1 — Re-cut the scope (H3).** Decide the boundary by method rather than by title. The
recommendation is to move the transport fleet-size sweep, the forward ICU share frontier,
the 50-replication ICU-pathway validation and the mass casualty stress test into the
multi-run paper, where the replication and confidence-interval methodology is already
established, and to retitle this document to reflect what then remains: a verification
and behavioural walk-through of one campaign. If instead the sweeps are to stay, this
document needs its own methods section describing the replication framework, and its
title and abstract must stop calling it a single-run analysis.

**7.2 — Strip the changelog voice (H4).** Remove all twenty-two `Issue #N` references and
the three blockquoted maintainer notes. Where a note carries information a reader needs
(the mass casualty section's non-default configuration, the pinned-container provenance),
promote it into a proper methods or limitations statement. Where it records history, it
is already in the action plan and can simply go.

**7.3 — Add the missing apparatus.** The document currently has no methods section, no
limitations section of its own, and three references. A results paper needs all three.
Limitations should cross-reference the README's `L<n>` entries rather than restate them,
per the no-duplication rule.

**7.4 — Verify every figure against the current baseline.** Each numeric claim is checked
against `CLAUDE.md`'s Key Parameters table and the regenerated outputs, and any figure
that predates the arrival-process rebuild is either re-run or explicitly dated. This is
the "findings are accurate" half of the task and it needs R, so it runs in the pinned
container.

**7.5 — Language pass.** Fix the grammatical errors ("Role 2 facilities requires"),
settle the spelling to one convention (L-b), clear the em dashes (L-c), and check every
figure has a caption.

### Phase 8 — Multi-Run Analysis to Publication Standard

*Goal: `docs/Multi_Run_Analysis.md` reads as a paper and its statistics are fully
specified.*

**8.1 — Absorb what Phase 7 moves in.** Integrate the sweeps and the stress test as
proper experimental sections, each with its design stated.

**8.2 — Strip the changelog voice (H4).** This paper's Interpretation and Conclusion are
where the problem is worst: several paragraphs exist only to compare current figures with
superseded ones. Current findings stay; the supersession record moves to the action plan.
Where a withdrawn claim matters to a reader — the forward-surgery effect that 50
replications cannot resolve is a genuine and honest limitation — it is restated as a
limitation rather than as a retraction narrative.

**8.3 — Complete the statistical specification.** The paper reports confidence intervals
throughout without ever stating how they are computed (normal approximation, *t*, or
bootstrap) or stating the independence argument in its own text. It also reports the
queue comparison table as bare point estimates while the casualty table carries intervals
and percentile ranges, which is inconsistent in a paper whose entire premise is interval
reporting. Add intervals to the queue table or state why they are omitted. Reconcile the
abstract's "n≥30" with the actual n=50.

**8.4 — Add the missing apparatus.** An introduction stating the research question, a
limitations section, and a reference list larger than two entries. The power analysis
already present in `CLAUDE.md` (the replication counts needed for a given half-width) is
strong material and belongs in this paper's methods.

**8.5 — Language pass.** As 7.5.

### Sequencing

```
Phase 6 (blocking)
  6.0 verification baseline  ──┐
  6.1 references             ──┤ 6.1-6.2 can run in parallel with 6.3-6.4
  6.2 links + checker        ──┤
  6.3 style guide            ──┤
  6.4 lint + CI              ──┘
  6.5 apply standard         ── requires 6.0, 6.3, 6.4
  6.6 housekeeping           ── any time

Phase 7 ── requires 6.5 (function names and paths cited in the papers move)
  7.1 scope decision         ── gates 8.1
  7.2 → 7.5 sequential

Phase 8 ── requires 7.1
  8.1 → 8.5 sequential
```

The one genuine ordering constraint is that **7.1 gates 8.1**: the scope decision
determines what each paper contains, so it must be made before either is rewritten.
Everything in 6.1, 6.2 and 6.6 is independent and can be shipped immediately.

---

## Issues Raised

Every finding and plan task above is tracked. Issues were raised 23 August 2026 under two new
labels, `phase/6 · code-quality` and `phase/7 · publication`.

| Issue | Title | Findings | Status |
|---|---|---|---|
| #230 | Establish the verification baseline | H5 (6.0) | ready — blocks #235, #239, #240, #241 |
| #231 | Repair the README reference list | C2, H1, H2 | ready |
| #232 | Fix the ten broken README images, extend the markdown checker | C1, L-a | ready |
| #233 | Role 4 length-of-stay drawn from an unseeded stream | new finding | ready — blocks #241 |
| #234 | Rewrite `docs/STYLE_GUIDE.md` as an enforceable standard | M4 | ready |
| #235 | Add lintr, a check-suite runner, and CI | H5 | blocked by #230, #234 |
| #236 | Exception-safe global save/restore, and input validation | M1, M3 | ready |
| #237 | Housekeeping: wip branches, scan table, CLAUDE.md drift | M5, M6, L-e | ready |
| #238 | Re-cut the analysis papers by method | H3 | ready — blocks #239, #240 |
| #239 | Single-run paper to publication standard | H4, L-a, L-b, L-c | blocked by #238, #230 |
| #240 | Multi-run paper to publication standard | H4 | blocked by #238, #230 |
| #241 | Apply the code standard: decompose the oversized functions | M2, L-d, L-f | blocked by #233, #234, #235, #230 |

Three decisions were taken while raising these and are recorded here because they changed the
plan above.

**The refactor covers both `analyse_run` and `server` (#241).** The review implied `server`
could not be verified after a refactor. That was wrong. Its reactive state machine (forty
`reactiveVal`, ten `reactive`, ten `observeEvent`) is testable with `shiny::testServer()`,
which ships with shiny and needs no new dependency, and its rendered output is testable with
Playwright against the Chromium already present in the development environment. `server` is
untested, not untestable, and the risky part of it is cheap to cover. Playwright was preferred
over `shinytest2` because it keeps browser automation out of `renv.lock` and is less brittle
than snapshot testing; the cost is a Node toolchain alongside `renv.lock`.

**The refactor does not gate the papers.** The review's sequencing argument was that the papers
cite function names the refactor would move. They cite eight function names between them, of
which two are in `R/analysis.R`: `analyse_run()`, whose name survives decomposition, and
`plot_transport_capacity_margin_by_fleet_size()`. Phase 7 can therefore proceed in parallel
with #241 rather than behind it. The dependency that does bind is #233 before #241, because
`R/analysis.R` draws random numbers at analysis time and reordering the pipeline would move a
published figure.

**The replicated experiments move to the multi-run paper (#238).** The single-run document is
retitled to reflect what remains.

One discrepancy worth noting: Issue #227 refers to twelve `wip/*` checkpoint refs; `git
ls-remote` currently returns ten. #237 covers the deletion and either supersedes #227 or is
dropped in its favour.

---

## Open Questions for the Owner

1. **Scope boundary (7.1).** Move the parameter sweeps into the multi-run paper, or keep
   them in the single-run paper and give it a methods section? This is the one decision
   that changes the shape of both papers.
2. **Publication target.** The two papers are currently sized very differently (373 lines
   against 154) and neither is formatted to any journal's requirements. Knowing the target
   venue would settle length, structure and reference style before the rewrite rather than
   after.
3. **Refactor appetite (6.5).** Decomposing `server` and `analyse_run` is the largest
   piece of work in the plan and carries the most regression risk. It is worth doing
   before the papers cite the new structure, but it could equally be deferred if the
   papers matter more than the code's reviewability.
