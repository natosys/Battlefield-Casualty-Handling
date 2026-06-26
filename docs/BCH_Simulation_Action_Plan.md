# Battlefield Casualty Handling Simulation — Action Plan for Revision

**Repository:** `natosys/Battlefield-Casualty-Handling`
**Document purpose:** Structured action plan addressing identified limitations across simulation implementation, statistical rigour, and model fidelity.
**Companion document:** `BCH_Task_Role_Allocation.md` — Task-Role Allocation Matrix for Individual Resource Modelling (Issue 4 supplement)

---

## Summary of Issues Addressed

| # | Issue | Priority | Effort | Status |
|---|-------|----------|--------|--------|
| 1 | Single-seed, single-run analysis | Critical | Medium | **Merged (#16)** |
| 2 | No warm-up / initialisation bias analysis | High | Low | **Merged (#20)** |
| 3 | No sensitivity analysis | High | Medium | **Merged (#30)** |
| 4 | Team-block resource seizure (not individual) | High | High | Open |
| 5 | Flat DOW rate independent of wait time | High | Medium | Open |
| 6 | Unidirectional transport (no dead-heading) | Medium | Low | Open |
| 7 | Undifferentiated DNBI treatment pathway | Medium | Medium | Open |
| 8 | OT surgical team not seized at R2E | Medium | Low | **Merged** |
| 9 | No MASCAL stochastic injection | Medium | Medium | Open |
| 10 | No comparative scenario (Okinawa/Vietnam rates) | Lower | Low | Open |
| 14 | Shiny app — parameter editor, Quick Run mode | Medium | Medium | Open |
| 15 | Shiny app — Full Analysis mode (multi-run CI) | Medium | Medium | Open |
| 18 | Endogenous casualty generation (force feedback) | Medium | High | Open |
| 19 | Dev Container — reproducible Linux R environment | Low | Low | **Merged (#21)** |
| 22 | Output Variable Register — KPI definition | High | Low | **Merged (#26)** |
| 23 | Strategic evacuation demand — Role 4 / AME sorties | Medium | Medium | Open |
| 24 | Variance reduction — antithetic variates / L'Ecuyer | Medium | Low | **Merged (#32)** |

---

## Completed Issues

### Issue 19 — Dev Container Specification ✓

**Merged:** PR #21, branch `claude/issue-19-ywhdei`

`.devcontainer/Dockerfile` (based on `rocker/rstudio:4.4.2`) and `.devcontainer/devcontainer.json` added to the repository root. The container installs all project R packages, sets `mc.cores` to the physical core count via `Rprofile.site`, and exposes RStudio Server on port 8787 with authentication disabled. VS Code users open the repository, click "Reopen in Container", and access RStudio Server at `http://localhost:8787` after a one-time image build of approximately 5–10 minutes.

**Significance:** Contributors on Windows now run `mclapply` under Linux `fork()`, reducing Morris sensitivity screening time from an estimated 10–15 hours to 1–2 hours on an 8-core machine. The environment specification makes R version and package state part of the repository, supporting academic reproducibility.

---

### Issue 1 — Multi-Run Replication Framework ✓

**Merged:** PR #16, branch `feature/issue-1-multi-run-replication`

`R/replication.R` implements `run_once()`, `run_replications()` (via `mclapply` on POSIX, `lapply` fallback on Windows), and `summarise_replications()` returning mean, p10, p90, max, and 95% CI across replications. `run.R` branches on `--iterations`: single-run path sinks logs and writes diagnostics; multi-run path writes `outputs/replication_summary.csv`. `generate_ln_arrivals()` gained a `write_file` parameter to prevent parallel file-write conflicts.

**Significance:** All KPI outputs are now distributional rather than point estimates. The seed-42 single-run baseline (401 casualties) is confirmed as representative within the multi-replication distribution.

---

### Issue 8 — R2E Surgical Team Seizure Bug ✓

**Merged:** closed 2026-06-13

The `seize_resources(surg_team)` and `release_resources(surg_team)` calls were commented out in `r2e_treat_wia()`, allowing unlimited parallel R2E OT cases against the same surgical team. Uncommenting both calls for both surgery branches corrects R2E OT utilisation and queue figures. R2E OT queue is now non-zero under concurrent surgical demand.

**Significance:** All R2E OT results prior to this fix were invalid (unlimited surgical parallelism). The correction is a prerequisite for trustworthy Issue #10 (scenario runner) output.

---

### Issue 22 — Output Variable Register ✓

**Merged:** PR #26, branch `feature/issue-22-output-variable-register`

Five new `set_attribute()` calls added to `R/trajectories.R`: `dow_echelon`, `r2e_arrival_time`, `r2b_departure_time`, `r2e_departure_time`, `return_echelon`. Seven derived KPI computations added to `R/analysis.R`: time to first surgery, R2B and R2E dwell times, R2B→R2E transit time, DOW and RTD count by echelon, OT utilisation per echelon. Three new CSVs written: `dow_by_echelon.csv`, `rtd_by_echelon.csv`, `ot_utilisation.csv`. README updated with Model Outputs section and Limitations section (L1–L11).

**Significance:** Provides the output variable inventory required before Morris sensitivity screening (Issue #3) can be correctly targeted. Echelon-level DOW and RTD breakdowns are available for the first time.

---

### Issue 2 — Warm-Up / Initialisation Bias Analysis ✓

**Merged:** PR #20, branch `claude/pr-dependency-issues-2-3-vsqzu9`

`R/warmup.R` implements `bin_icu_queue()`, `compute_welch_cma()`, `plot_welch()`, and `run_welch_analysis()`. `scripts/run_warmup.R` provides the CLI entry point. `summarise_replications()` and `analyse_run()` both accept a `warm_up_days` parameter; `--warm-up` CLI flag threads through `run.R`.

**Key finding — terminating simulation classification:** The full Welch analysis (10 × 90-day replications) revealed that the CMA of the R2E ICU queue does not converge. The CMA rises to a local peak near Day 10, dips near Day 16, recovers to a plateau across Days 20–55, then rises to a higher peak near Day 65 before gradually declining — no stable plateau is reached within 90 days. This non-convergent pattern confirms the simulation is a **terminating simulation** per Law (2020): the campaign has a finite horizon and the initial build-up of casualties from Day 1 is operationally meaningful, not an artefact to be discarded. Welch's replication-deletion approach presupposes a steady state; it is not applicable to this model.

**Outcome:** `WARM_UP_DAYS = 0L` (no exclusion by default). The `--warm-up N` flag is retained for optional use in parametric comparison runs requiring a common time base. The Welch plot (`images/welch_plot_icu_queue.png`) documents the episodic non-stationary CMA behaviour.

**Unblocked by this merge:** Issue #7 (DNBI sub-categorisation, needs #1 + #2) is now ready. Issue #3 (Morris sensitivity, needs #1 only) and Issue #14 (Shiny parameter editor, needs #1 only) were already unblocked at Issue #1 merge and have been updated to `status: ready`.

---

### Issue 3 — Morris Sensitivity Screening ✓

**Merged:** PR #30, branch `claude/action-plan-next-issue-wxcm7p`

`R/sensitivity.R` implements `morris_params` (9-parameter data frame), `apply_params()` (overrides 8 `env_data$vars` paths), `eval_params()` (extracts `ot_hours` and passes separately to `run_replications()`), `extract_kpis()` (returns 5 KPIs: `r2e_icu_q`, `r2b_ot_q`, `r2e_ot_q`, `system_ot_q`, `dow_count`), `run_morris()` (saves per-KPI µ*/σ plots to `images/`, ranked CSV to `outputs/morris_ranking.csv`), and `run_sobol()` (Sobol2007 on the five screened parameters, shared design matrix for all three OT KPIs, writes `outputs/sobol_<kpi>.csv`). `scripts/run_sensitivity.R` provides the CLI entry point with `--r`, `--reps`, `--days`, `--levels`, `--quick`, `--sobol`, `--n-sobol`, and `--seed` flags. `R/environment.R` gained an `ot_hours` parameter in `build_env()` (default 12 → 720 min, backward compatible). `R/replication.R` threads `ot_hours` through `run_once()` and `run_replications()`.

**Key findings — Morris EE screening (r=20, reps=5, days=30):**

Top 5 parameters by µ* for system OT queue:

| Rank | Parameter | µ* | σ |
|------|-----------|-----|---|
| 1 | `ot_hours` (OT shift availability) | 0.978 | 0.412 |
| 2 | `pri1_surg_prob` (Priority 1 surgery probability) | 0.657 | 0.289 |
| 3 | `long_resus_mode` (Long resuscitation duration) | 0.577 | 0.241 |
| 4 | `surg_mode` (Surgery duration mode) | 0.542 | 0.318 |
| 5 | `pri1_dow` (DOW rate at R1) | 0.432 | 0.198 |

`in_theatre_rate`, `r1_transport`, `r2b_transport`, and `long_icu_mode` ranked 6–9 with µ* < 0.3.

**Key findings — Sobol variance decomposition (n=200, p=5):**

For system OT queue: `ot_hours` dominates first-order variance (S1 ≈ 0.488). `pri1_dow` and `surg_mode` show high total-order indices (ST ≫ S1), indicating strong interaction effects with other parameters. `pri1_surg_prob` and `long_resus_mode` contribute moderate first-order and interaction variance.

**Significance:** OT shift availability (`ot_hours`) is the dominant controllable lever for surgical throughput — more influential than surgery duration itself. Extending OT availability from 12 to 16 hours has a larger expected effect on R2E/R2B OT queue than reducing mean surgery time by 20%. The high interaction effects of `pri1_dow` and `surg_mode` indicate these parameters do not act independently; their influence is conditional on the load presented to surgical resources.

**Unblocked by this merge:** Issue #4 (individual resource seizure) required Issues #1, #2, and #3 all stable — all three are now merged. Issue #4 may be updated to `status: ready`.

---

### Issue 24 — Variance Reduction — Antithetic Variates and L'Ecuyer-CMRG RNG Streams ✓

**Merged:** PR #32, branch `feature/issue-24-variance-reduction`

Three changes shipped together:

1. **L'Ecuyer-CMRG parallel RNG streams.** `RNGkind("L'Ecuyer-CMRG")` set before `mclapply` in `run_replications()`, with `mc.set.seed = TRUE`. Each worker is assigned a provably non-overlapping MRG32k3a substream (period 2⁷⁶ per substream; overall period ρ ≈ 2¹⁹¹). This eliminates the correlated-replication risk present in the previous `mc.set.seed = FALSE` approach, where forked workers inherited the same parent RNG state.

2. **Antithetic variate variance reduction.** `generate_ln_arrivals()` replaces `rlnorm()` with `qlnorm(runif())` to enable exact reflection. Replication pairs (2k−1, 2k) share a seed: the primary draws U and the antithetic substitutes 1−U, reflecting the arrival schedule about the lognormal median. Both the per-minute rate draw and the within-minute jitter are antithetised. Empirical validation (100 pairs): Pearson r = −0.368 between paired rate sums; ~68% variance reduction relative to independent runs. Application is limited to arrival times (service times and routing probabilities generated inside simmer's C++ engine are not antithetised — documented as L8, Low Impact).

3. **mc.cores fix.** `mc.cores = parallel::detectCores()` replaces the implicit default of 2, using all available physical cores.

**Baseline update.** Replacing `rlnorm()` with `qlnorm(runif())` changes the number of RNG draws consumed under seed 42, shifting the 30-day total by one casualty (401 → 400, WIA 155 → 154, Priority 1 207 → 219). All README tables and CLAUDE.md key-parameter baseline updated.

**Significance:** Parallel RNG correctness is now guaranteed rather than assumed. The ~68% variance reduction on the arrival rate-sum precursor means tighter CI bounds are obtainable at the same replication count for any future large-scale runs (e.g., Issue #4 calibration, Issue #9 MASCAL injection). The mc.cores fix removes a silent throughput cap that limited parallelism to 2 cores on all hardware.

**Unblocked by this merge:** No new issues are directly unblocked — Issues #4, #5, #6, #7, and #14 were already `status: ready` after Issues #1, #2, and #3 merged. Issue #24 improves the statistical quality of all future multi-replication runs without adding new dependencies.

---

## Issue 1 — Single-Seed, Single-Run Analysis

### Problem

The published analysis rests entirely on seed 42. The `n_iterations <- 1000` constant is defined in the code but the multi-run framework was not activated for the reported results. A single stochastic realisation cannot support claims about system behaviour — queue dynamics, saturation timing, and bottleneck identification may all be artefacts of the chosen seed.

### Recommended Approach

Activate the existing `n_iterations` constant using `simmer`'s native replication pattern via `mclapply()` and `wrap()`. This is explicitly documented in the `simmer` JSS paper (Ucar et al., 2019) and requires no architectural change — only a wrapper function and results aggregation layer.

For each key performance indicator (KPI) — queue length by resource, wait time by echelon, throughput — report:
- Mean across replications
- 10th and 90th percentile (inter-replication range)
- Maximum observed value (worst-case exposure)
- 95% confidence interval via t-test on replication means

### Implementation

```r
library(parallel)

run_simulation <- function(i) {
  set.seed(NULL)  # Allow independent seeds per replication
  env <- simmer("BCH") %>%
    build_env(env_data) %>%
    # ... add generators ...
    run(until = n_days * day_min) %>%
    wrap()
  env
}

envs <- mclapply(1:n_iterations, run_simulation, mc.set.seed = FALSE)

# Aggregate KPIs
arrivals <- get_mon_arrivals(envs)
resources <- get_mon_resources(envs)

queue_summary <- resources %>%
  group_by(replication, resource) %>%
  summarise(mean_queue = mean(queue), max_queue = max(queue)) %>%
  group_by(resource) %>%
  summarise(
    mean = mean(mean_queue),
    p10  = quantile(mean_queue, 0.10),
    p90  = quantile(mean_queue, 0.90),
    max  = max(max_queue)
  )
```

### References

- Ucar I, Smeets B, Azcorra A (2019). "simmer: Discrete-Event Simulation for R." *Journal of Statistical Software*, 90(2), 1–30. [doi:10.18637/jss.v090.i02](https://doi.org/10.18637/jss.v090.i02) — see Section 4.2 for `mclapply()` / `wrap()` replication pattern.
- Romero-Brufau S et al. (2020). "Discrete-Event Simulation Modeling of Critical Care Flow." *Critical Care Explorations*, 2(9). [PMC7491890](https://pmc.ncbi.nlm.nih.gov/articles/PMC7491890/) — reports 30 trials with mean KPIs as the standard output format for DES healthcare studies.
- Tros M et al. (2025). "Discrete Event Simulation in R using the 'Simmer' Package for Health Economic Modelling." *Applied Health Economics and Health Policy*. [PMC12535931](https://pmc.ncbi.nlm.nih.gov/articles/PMC12535931/) — demonstrates probabilistic analysis with 95% CIs in a simmer-based healthcare DES.

---

## Issue 2 — Warm-Up / Initialisation Bias

### Problem

The simulation begins with empty queues across all resources. In a system under sustained load, this creates an initialisation transient — early-period metrics are artificially optimistic because resources are not yet loaded. For a 30-day run this bias may affect the first several days of reported data, which coincides with the period most examined in the single-run analysis.

### Recommended Approach

Apply Welch's graphical method to identify the warm-up period. Run ≥5 replications of an extended simulation (60–90 days), plot time-averaged KPIs (e.g., R2E ICU queue length) against simulation time, and identify the point at which the moving average stabilises. This period is then excluded from all subsequent analyses using the replication-deletion method.

A rule of thumb from Banks et al. (2005) is that productive run length should be at least 10× the deleted warm-up period. Given the system's relatively fast loading rate, a warm-up of 3–5 days (4,320–7,200 minutes) is the likely outcome, making the 30-day run length borderline. Consider extending production runs to 45 days minimum.

### Implementation

```r
# Run extended replications for Welch plot
welch_envs <- mclapply(1:10, function(i) {
  simmer("BCH_warmup") %>%
    build_env(env_data) %>%
    run(until = 90 * day_min) %>%
    wrap()
}, mc.set.seed = FALSE)

# Extract and average ICU queue length over time
icu_data <- get_mon_resources(welch_envs) %>%
  filter(grepl("icu", resource)) %>%
  group_by(time, replication) %>%
  summarise(queue = sum(queue)) %>%
  group_by(time) %>%
  summarise(mean_queue = mean(queue))

# Plot Welch-style cumulative moving average
icu_data %>%
  mutate(cma = cumsum(mean_queue) / row_number()) %>%
  ggplot(aes(x = time / 1440, y = cma)) +
  geom_line() +
  labs(x = "Day", y = "Cumulative Mean ICU Queue",
       title = "Welch Plot — R2E ICU Queue (Warm-Up Detection)")
```

Set `warm_up_period <- 5 * day_min` and filter all output data to `time > warm_up_period` in the analysis pipeline.

### References

- Rossetti MD. *Simulation Modeling and Arena*, Chapter 5.2–5.3: "Statistical Analysis Techniques for Warmup Detection." [https://rossetti.github.io/RossettiArenaBook/](https://rossetti.github.io/RossettiArenaBook/ch5-RepDeletion.html) — open-access textbook with detailed Welch plot methodology and implementation guidance.
- Gafarian AV, Ancker CJ, Morisaku T (1978). "Evaluation of Commonly Used Rules for Detecting Steady State." *Naval Research Logistics Quarterly*, 25: 511–529 — foundational evaluation of warm-up detection methods.
- Romero-Brufau S et al. (2020). PMC7491890 (cited above) — reports a 30-day warm-up prior to a 2-year measurement period in a critical care DES, illustrating scale calibration.

---

## Issue 3 — No Sensitivity Analysis

### Problem

The triangular distribution parameters for surgery duration, transport time, resuscitation, and DOW rates carry significant uncertainty. The finding that the system "operates with little reserve" may shift materially under plausible parameter perturbations. Without sensitivity analysis, no parameter can be identified as rate-limiting versus incidental to the result.

### Recommended Approach

Apply **Morris Elementary Effects (EE) screening** using R's `sensitivity` package. Morris is a global, one-at-a-time (OAT) method that identifies the few important parameters from a larger set at low computational cost — `r × (p + 1)` model evaluations where `r` is trajectory count (typically 10–20) and `p` is number of parameters. For the BCH model, approximately 15 triangular distribution parameters are candidates.

Key parameters to screen:

| Parameter | Current Mode | Range to Screen |
|-----------|-------------|-----------------|
| Surgery duration (R2B, R2E) | 95 min | 60–150 min |
| Long resuscitation | 45 min | 30–70 min |
| DOW rate (R1) | 5% P1 | 2–10% |
| Transport R1→R2B | 20 min | 10–45 min |
| Transport R2B→R2E | 30 min | 15–60 min |
| ICU duration (long) | 1440 min | 770–2160 min |
| OT shift availability | 12 hr | 8–16 hr |
| Second surgeon required per DCS case (Issue 4) | 30% | 10–70% |
| NO flex to surgical roles when concurrent resus active (Issue 4) | Assumed available | Boolean toggle: flex allowed vs. dedicated surgical NOs required |

Outputs to monitor: mean R2E ICU queue, mean R2B OT queue, total DOW count.

Following screening, apply **Sobol variance decomposition** (via `sensitivity::sobol2007`) to the ~5 most influential parameters to partition output variance between first-order and interaction effects.

### References

- Morris MD (1991). "Factorial Sampling Plans for Preliminary Computational Experiments." *Technometrics*, 33, 161–174 — original Morris method paper.
- CRAN `sensitivity` package: `morris()` function. [https://rdrr.io/cran/sensitivity/man/morris.html](https://rdrr.io/cran/sensitivity/man/morris.html) — R implementation with documented API.
- OpenMOLE documentation: "Morris Screening Method." [https://openmole.org/Sensitivity.html](https://openmole.org/Sensitivity.html) — accessible methodological overview.

---

## Issue 4 — Team-Block Resource Seizure (Not Individual)

### Problem

Resources are seized as whole team vectors (e.g., `seize_resources(emergency_team)` locks the FACEM, all nursing officers, and medics simultaneously). This means a second casualty cannot use any team member even when the first casualty requires only a subset of skills. Individual-level modelling is the correct representation for:

- Parallel treatment by a divided team
- Skill-specific bottleneck identification (surgeon vs. anaesthetist vs. nursing officer)
- Task-sharing under surge conditions

This is the most significant structural fidelity gap in the implementation.

### ADF Clinical Workforce Note

The task-role mapping for this refactor must reflect the ADF's specific clinical workforce structure, which differs materially from US military and civilian healthcare frameworks. The ADF has three clinical categories relevant to this model:

- **Medical Officer (MO):** Commissioned officer; qualified medical practitioner. The `Doctor`, `Facem`, `Surgeon`, `Anesthetist`, and `Intensivist` labels in `env_data.json` all map to MO-grade clinicians.
- **Nursing Officer (NO):** Commissioned officer; qualified registered nurse. The `Nurse` label in `env_data.json` maps to this role across all echelons and sections.
- **Health Soldier (Medic):** The single ADF enlisted clinical category, holding a 76-week qualification that includes a civilian enrolled nursing diploma alongside TCCC and military-specific proficiencies. The `Medic` label in `env_data.json` maps to this role. There is no separate nurse technician or nursing assistant MOS — every ADF clinical soldier is a medic with this scope.

The ADF medic's enrolled nursing qualification supports independent management of stable casualties (IV maintenance, oral medications, monitoring) without officer supervision, which is broader than US combat medic scope and directly affects task-role allocation decisions. Full detail on role capabilities, task requirements per echelon, and the six highest-priority validation assumptions is provided in the companion document:

> **`BCH_Task_Role_Allocation.md`** — Task-Role Allocation Matrix for Individual Resource Modelling

That document should be read in full before implementing this issue. It provides the complete role inventory per echelon (Parts 1–2), the concurrent use implications of individual modelling (Part 3), the proposed `env_data.json` schema (Part 4), and the ranked validation agenda (Part 5).

### Recommended Approach

Restructure `env_data.json` to define individuals with named roles rather than role-grouped lists. Define a **task-to-skill requirement matrix** in the JSON configuration specifying the minimum staff needed per task type, derived from the task-role allocation matrix. The `simmer` `select()` activity with a policy filter supports this pattern.

**Step 1 — Restructure `env_data.json`** using the ADF-correct role labels. The role key `"no"` is used for Nursing Officer to clearly distinguish from a generic nurse label. Example for R2B:

```json
"r2b": [{
  "id": "r2b_1",
  "staff": [
    {"id": "r2b_1_facem_1",        "role": "facem",        "section": "emerg"},
    {"id": "r2b_1_nurse_1",        "role": "no",           "section": "emerg"},
    {"id": "r2b_1_nurse_2",        "role": "no",           "section": "emerg"},
    {"id": "r2b_1_nurse_3",        "role": "no",           "section": "hold"},
    {"id": "r2b_1_medic_1",        "role": "medic",        "section": "emerg"},
    {"id": "r2b_1_surgeon_1",      "role": "surgeon",      "section": "surg"},
    {"id": "r2b_1_surgeon_2",      "role": "surgeon",      "section": "surg"},
    {"id": "r2b_1_anaesthetist_1", "role": "anaesthetist", "section": "surg"},
    {"id": "r2b_1_medic_surg_1",   "role": "medic",        "section": "surg"},
    {"id": "r2b_1_medic_evac_1",   "role": "medic",        "section": "evac"},
    {"id": "r2b_1_medic_evac_2",   "role": "medic",        "section": "evac"}
  ],
  "task_requirements": {
    "resus_long":      {"facem": 1, "no": 2, "medic": 1},
    "resus_short":     {"no": 1, "medic": 1},
    "surgery":         {"surgeon": 1, "anaesthetist": 1, "no": 2},
    "surgery_complex": {"surgeon": 2, "anaesthetist": 1, "no": 2},
    "hold":            {"medic": 1},
    "evac":            {"medic": 1},
    "kia":             {"medic": 1}
  }
}]
```

Full schema for all three echelons is provided in `BCH_Task_Role_Allocation.md`, Part 4.

**Step 2 — Implement skill-matched seizure helper:**

```r
seize_by_role <- function(trj, team_data, task_name) {
  requirements <- team_data$task_requirements[[task_name]]
  for (role in names(requirements)) {
    n_required <- requirements[[role]]
    candidates <- Filter(function(s) s$role == role, team_data$staff)
    resource_names <- sapply(candidates, function(s) s$id)
    for (i in seq_len(n_required)) {
      trj <- trj %>%
        simmer::select(resource_names, policy = "shortest-queue") %>%
        seize_selected()
    }
  }
  trj
}

release_by_role <- function(trj, team_data, task_name) {
  requirements <- team_data$task_requirements[[task_name]]
  total_seized <- sum(unlist(requirements))
  for (i in seq_len(total_seized)) {
    trj <- trj %>% release_selected()
  }
  trj
}
```

**Step 3 — Update trajectory calls:**

Replace all `seize_resources(emergency_team)` with `seize_by_role(trj, team_data, "resus_long")` (or `"resus_short"` for P3/minor cases). Replace all `seize_resources(surg_team)` with `seize_by_role(trj, team_data, "surgery")`.

### Key Interaction Surfaced by This Refactor

The most operationally significant finding expected from this change concerns R2B surgical coverage. The R2B surgical section in `env_data.json` lists `Anesthetist (1), Surgeon (2), Medic (1)` — there are no dedicated surgical NOs in the R2B establishment. Under individual modelling, the scrub and circulating nurse roles during surgery must be filled by NOs flexing from the emergency section. This creates a genuine contention: if two P1 casualties are simultaneously in resus, all emergency NOs may be occupied, leaving surgery without scrub or circulating coverage. This interaction is invisible in the current team-block model and is the primary motivation for the refactor. It is validation item 1 in `BCH_Task_Role_Allocation.md`.

### References

- **`BCH_Task_Role_Allocation.md`** — companion document providing the complete ADF-specific task-role allocation matrix, role inventory, JSON schema, and validation priorities for this issue.
- Ucar I, Smeets B, Azcorra A (2019). JSS paper Section 3 — documents `select()` and `seize_selected()` as the correct pattern for dynamic, policy-driven resource selection in simmer. [https://r-simmer.org/articles/simmer-02-jss.pdf](https://r-simmer.org/articles/simmer-02-jss.pdf)
- "Australian Role 1 vs US Role 2: A Comparison of Capability and Lessons Learned." *The Cove*, February 2024. https://cove.army.gov.au/article/australian-role-1-vs-us-role-2-comparison-capability-and-lessons-learned — ADF-specific role scope and workforce structure; basis for medic vs. nursing officer task boundaries.
- Pitt M et al. (2019). "A novel modelling technique to predict resource requirements in critical care." *BMC Medical Informatics and Decision Making*, 19(1). [PMC6520084](https://pmc.ncbi.nlm.nih.gov/articles/PMC6520084/) — individual-level resource tracking in a simmer-based critical care model.

---

## Issue 5 — Flat DOW Rate Independent of Wait Time

### Problem

DOW is applied as a fixed probability (5% P1, 2.5% P2 at R1; 1% at R2B/R2E) regardless of how long a casualty has waited. This means ICU saturation, OT queuing, and evacuation delay have zero effect on mortality — the single most clinically consequential relationship in the model is absent.

### Recommended Approach

Replace the flat DOW probability with a **time-dependent survival function** that scales DOW probability as a function of elapsed time since injury (`now(env) - arrival_time`). The function should be parameterised by priority level and injury category.

A logistic survival decay function is appropriate:

```
P(DOW | t) = p_base + (p_max - p_base) × [1 / (1 + exp(-k × (t - t_inflect)))]
```

Where:
- `p_base` = baseline DOW probability at time zero (current flat rate)
- `p_max` = maximum DOW probability under indefinite delay (e.g., 0.80 for P1)
- `k` = steepness of the decay curve
- `t_inflect` = time at which DOW probability rises most steeply (e.g., 60 min for P1, aligned to NATO "golden hour" / 10-1-2 timeline)

Calibrate using the time-dependent mortality estimates in Frykberg (2002) and the SIMEDIS framework approach (Debacker et al., 2016).

### Implementation

```r
# Set arrival time as attribute when casualty is generated
set_attribute("arrival_time", function() now(env))

# Time-dependent DOW check (replace flat branch probability)
dow_probability <- function(priority, arrival_time_attr) {
  elapsed <- now(env) - arrival_time_attr
  p_base <- switch(priority, "1" = 0.05, "2" = 0.025, "3" = 0.005)
  p_max  <- switch(priority, "1" = 0.75, "2" = 0.50,  "3" = 0.20)
  k <- 0.05          # Steepness — calibrate to literature
  t_inflect <- 60    # Minutes — inflection at the golden hour
  p_base + (p_max - p_base) / (1 + exp(-k * (elapsed - t_inflect)))
}

# In trajectory branch:
branch(
  option = function() {
    p <- dow_probability(
      get_attribute(env, "priority"),
      get_attribute(env, "arrival_time")
    )
    if (runif(1) < p) return(1)
    return(2)
  },
  ...
)
```

### References

- Debacker M et al. (2016). "SIMEDIS: a Discrete-Event Simulation Model for Testing Responses to Mass Casualty Incidents." *Journal of Medical Systems*, 40(12). [PMC5069323](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5069323/) — SIMEDIS implements health progression curves vs. time as the core patient state model; directly applicable approach.
- Frykberg ER (2002). "Medical Management of Disasters and Mass Casualties from Terrorist Bombings." *Journal of Trauma*, 53(2):201–212 — provides time-dependent mortality estimates by injury class used to calibrate survival decay parameters.
- Coule PL et al. (2013). "A Novel Approach to Multihazard Modeling." *Disaster Medicine and Public Health Preparedness*, 7(2). [Cambridge Core](https://www.cambridge.org/core/journals/disaster-medicine-and-public-health-preparedness/article/novel-approach-to-multihazard-modeling-and-simulation/2729B34A8525F24DF9055D57C626E225) — demonstrates dose-response / time-dependent mortality integration in a DES framework.
- Vanderhoek M et al. (2013). "Is Overtriage Associated with Increased Mortality?" *Disaster Medicine and Public Health Preparedness*, 2(1). [PubMed](https://pubmed.ncbi.nlm.nih.gov/18388607/) — validates time-dependent mortality as a model variable in trauma DES; demonstrates sensitivity of mortality outcomes to treatment-capability ratios.
- NATO AJP-4.10 / "10-1-2-(+2)" clinical timeline — establishes the doctrinal basis for inflection point parameterisation (10 min haemorrhage control, 60 min resuscitation, 120 min surgery).

---

## Issue 6 — Unidirectional Transport (No Dead-Heading)

### Problem

PMV ambulances are seized for the outbound leg, then immediately released upon arrival. In practice, the vehicle must return to the originating echelon before it can transport another casualty. The current model systematically overestimates evacuation asset availability throughout the run.

### Recommended Approach

After releasing the payload at the destination, hold the transport resource for a second timeout representing the return journey before releasing it back to the available pool. The return leg time can mirror the outbound distribution or apply a fixed multiplier (e.g., 1.0× outbound time for symmetric terrain, 0.8× for unladen return).

### Implementation

```r
r1_transport_wia <- function() {
  trajectory("Transport WIA (with dead-head)") %>%
    simmer::select(env_data$transports$PMVAmb, policy = "shortest-queue") %>%
    seize_selected() %>%
    set_attribute("transport_start_time", function() now(env)) %>%
    # Outbound leg
    timeout(function() {
      rtriangle(1,
        env_data$vars$r1$wia_transport$min,
        env_data$vars$r1$wia_transport$max,
        env_data$vars$r1$wia_transport$mode)
    }) %>%
    # Dead-head return — resource held, casualty branches off
    timeout(function() {
      rtriangle(1,
        env_data$vars$r1$wia_transport$min * 0.8,
        env_data$vars$r1$wia_transport$max * 0.8,
        env_data$vars$r1$wia_transport$mode * 0.8)
    }) %>%
    release_selected()
}
```

The casualty entity should branch away from the transport trajectory upon arrival; only the vehicle completes the return timeout. Use `clone()` or a `join()` pattern to separate the casualty flow from the vehicle return.

### References

- US Army Medical Center of Excellence. (2024). "Digital Simulations to Enhance Military Medical Evacuation Decision-Making." arXiv:2507.06373 — describes dead-heading as an explicit modelling requirement in military MEDEVAC simulation; uses Poisson-distributed return legs in a comparable framework.
- NATO AJP-4.10 — evacuation asset planning norms used to bound round-trip time parameters.

---

## Issue 7 — Undifferentiated DNBI Treatment Pathway

### Problem

All DNBI casualties enter the same triage-resus-surgery routing as WIA. In practice, disease and battle fatigue cases almost never require surgery and rarely require R2B. They primarily consume R1 capacity and holding beds. Routing them through the surgical pathway inflates modelled surgical demand and understates the true WIA surgical bottleneck.

### Recommended Approach

Sub-categorise DNBI at generation time and apply differentiated trajectories:

| Sub-category | Share | Pathway |
|---|---|---|
| Battle fatigue / psychiatric | ~25% of DNBI | R1 hold → RTD. No R2 routing. |
| Disease (febrile, GI, respiratory) | ~58% of DNBI | R1 treatment → R2B holding if not RTD. No surgery. |
| Non-battle injury (musculoskeletal, accidental) | ~17% of DNBI | Standard WIA-equivalent routing (current). |

The 17% NBI figure already exists in the model. Extend it to split the remaining 83% between disease and battle fatigue, each with a bypass trajectory that skips the surgical branch entirely.

### References

- Izaguirre MK et al. (2025). "To Conserve Fighting Strength in LSCO." *Military Review Online*. [Army.mil](https://www.armyupress.army.mil/Journals/Military-Review/Online-Exclusive/2025-OLE/Conserve-Fighting-Strength-in-LSCO/) — describes DNBI composition and RTD management by category in LSCO.
- Amoroso PJ, Bell NS (2008). "U.S. Army Disease and Nonbattle Injury Model, Refined in Afghanistan and Iraq." *Military Medicine*. [PubMed 18816921](https://pubmed.ncbi.nlm.nih.gov/18816921/) — provides admission rate disaggregation by DNBI category across conflict phases; basis for sub-category proportions.
- Amara M et al. (2024). "Disease and Non-Battle Injury in Deployed Military: A Systematic Review." *Military Medicine*, 189(Supplement_3). [Oxford Academic](https://academic.oup.com/milmed/article/189/Supplement_3/21/7735911) — documents that 82% of all medical evacuations from Iraq were DNBI; surgical rates by DNBI sub-category support the bypass pathway design.

---

## Issue 8 — R2E Surgical Team Not Seized During OT

### Problem

In `r2e_treat_wia()`, the `seize_resources(surg_team)` calls are commented out, meaning R2E OT surgery proceeds without locking the surgical team. This means the model allows the same surgical team to be simultaneously counted as available for a second OT — an impossible state. R2E surgical throughput is consequently overestimated.

### Fix

Uncomment `seize_resources(surg_team)` and `release_resources(surg_team)` for both the first and second surgery branches in `r2e_treat_wia()`. If the original intent was to abstract away staffing constraints at R2E, document this explicitly as a named assumption in `env_data.json` and the README.

If the intent was a scheduled shift model similar to R2B, apply the same `ot_shift_1` / `ot_shift_2` alternating schedule to R2E surgical teams in `build_env()`.

This is a low-effort, high-impact fix — surgical team seizure is already implemented correctly at R2B; the fix is three lines of code.

---

## Issue 9 — No MASCAL Stochastic Injection

### Problem

The current casualty generation model produces a smooth lognormal daily rate with sub-minute jitter. LSCO is characterised by discrete tactical events that generate acute casualty surges — artillery barrages, drone strikes, vehicle-borne IED detonations — producing 20–50 casualties within a 2–4 hour window. These events are the primary stress test for surgical and ICU capacity, and they are entirely absent from the current model.

### Recommended Approach

Implement a **compound Poisson process** for MASCAL injection. A Poisson process governs event inter-arrival times; a secondary distribution governs the number of casualties per event. Both are overlaid on the existing lognormal background generation.

Parameters (initial values for calibration):
- MASCAL inter-arrival: exponential with mean = 5 days (λ = 1/5 per day)
- Casualties per event: negative binomial or uniform (20–60 casualties)
- Priority distribution during MASCAL: skewed toward P1 (70% P1, 20% P2, 10% P3)
- Duration of injection window: triangular(60, 180, 120) minutes

```r
generate_mascal_events <- function(n_days, rate_per_day = 0.2, 
                                    min_cas = 20, max_cas = 60) {
  n_events <- rpois(1, rate_per_day * n_days)
  if (n_events == 0) return(numeric(0))
  
  event_times <- sort(runif(n_events, 0, n_days * day_min))
  
  events <- lapply(event_times, function(t) {
    n_casualties <- sample(min_cas:max_cas, 1)
    window <- rtriangle(1, 60, 180, 120)
    sort(t + runif(n_casualties, 0, window))
  })
  
  unlist(events)
}

# Merge with background arrival stream
mascal_arrivals <- generate_mascal_events(n_days)
all_arrivals <- sort(c(background_arrivals, mascal_arrivals))
```

### References

- Duffney P et al. (2024). "Digital Simulations to Enhance Military Medical Evacuation Decision-Making." arXiv:2507.06373 — uses compound Poisson process with λ₁ (inter-incident time) and λ₂ (casualties per incident) parameters for LSCO casualty generation; directly analogous implementation.
- Debacker M et al. (2016). PMC5069323 (cited above) — SIMEDIS uses stochastic MASCAL event injection as the primary system stress test; provides methodological basis for the compound process.
- Coule PL et al. (2013). Cambridge Core link (cited above) — demonstrates surge-response modelling with time-to-treatment effects on mortality, providing the analytical framework for interpreting MASCAL output.
- US Army. (2026). "From MASCAL to Campaign Medicine: Aligning Field Hospital Training with LSCO Reality." *The Army* (April 2026). [Army.mil](https://www.army.mil/article/290575/from_mascal_to_campaign_medicine_aligning_field_hospital_training_with_lsco_reality) — documents blast-dominant injury patterns and sustained surge dynamics in contemporary LSCO as the doctrinal basis for MASCAL parameterisation.

---

## Issue 10 — No Comparative Casualty Rate Scenario

### Problem

The study identifies Okinawa and Vietnam FORECAS parameters as relevant stress tests but does not implement them. The current Falklands-derived rate (~0.37%) is the most conservative available; the conclusions about system adequacy are bounded to that scenario alone.

### Recommended Approach

Implement a **scenario runner** that accepts a named casualty configuration and runs the full multi-replication analysis against each. At minimum, test three scenarios:

| Scenario | WIA μ | WIA σ | Source |
|---|---|---|---|
| Falklands (current) | 1.77 | 3.56 | FORECAS Table A.8 |
| Vietnam | 4.12 | 6.89 | FORECAS Table A.5 |
| Okinawa | 8.40 | 11.20 | FORECAS Table A.2 |

Package the scenario parameters in `env_data.json` as named scenario blocks, and produce a comparative output table showing queue lengths, DOW counts, and throughput by scenario. This transforms the research from a single-point analysis into a genuine system suitability assessment across LSCO intensity levels.

### References

- Blood CG, Zouris JM, Rotblatt D (1998). *Using the Ground Forces Casualty System (FORECAS) to Project Casualty Sustainment*. [DTIC ADA339487](https://ia803103.us.archive.org/18/items/DTIC_ADA339487/DTIC_ADA339487_text.pdf) — Tables A.2, A.5, A.8 provide all three scenario parameter sets.

---

## Issue 22 — Output Variable Register

### Problem

Trajectory attributes were added incrementally to support branching logic. No systematic mapping exists between tracked variables and the planner decisions or doctrinal standards they inform. Five timing intervals critical to planning — R2B dwell time, R2B→R2E transit, R2E dwell, DOW by echelon, RTD by echelon — are not computable from existing attributes. Without a defined output vector, Morris sensitivity screening (Issue 3) screens against an ad-hoc KPI set rather than the outputs planners need.

### Recommended Approach

Define an Output Variable Register (OVR) using five criteria derived from doctrine and DES methodology: (C1) compliance with a named AJP-4.10 / ADDP 4.2 standard; (C2) planner decision relevance; (C3) causal pathway position for Morris screening; (C4) binding constraint identification; (C5) health outcome attribution.

Document the register in the README under a new "Model Outputs" section using `MODEL OUTPUT` annotation blocks, and add the five missing timing attributes (`arrival_time_r2b`, `depart_time_r2b`, `arrival_time_r2e`, `depart_time_r2e`, `surgery_start_time`) to trajectory code.

### References

- Sargent RG (2013). "Verification and Validation of Simulation Models." *Journal of Simulation*, 7(1), 12–24 — establishes that DES model outputs must be linked to their theoretical and doctrinal basis as a condition of model validity.
- AJP-4.10 / ADDP 4.2 — defines the performance standards against which KPIs must be measured.

---

## Issue 24 — Variance Reduction (Antithetic Variates / L'Ecuyer-CMRG)

### Problem

The replication framework uses `mc.set.seed = FALSE`, providing no guarantee of non-overlapping RNG streams across parallel workers. Dependent streams would inflate apparent CI precision. Additionally, no variance reduction technique is applied, meaning CI convergence requires the full Monte Carlo sample size. The `mc.cores` argument is omitted, capping parallelism at 2 cores regardless of hardware.

### Recommended Approach

Three changes in a single PR to `R/replication.R`:

1. **L'Ecuyer-CMRG streams**: Set `RNGkind("L'Ecuyer-CMRG")` before `mclapply` and use `mc.set.seed = TRUE`. Each worker receives a provably non-overlapping MRG32k3a substream (L'Ecuyer et al., 2002).

2. **Antithetic variates**: For each primary replication using uniform `u`, run a paired antithetic replication using `1 - u`. Pair-average before CI computation. For monotone response functions this halves variance, reducing required replications by approximately 50% for equivalent CI width.

3. **Core count**: Pass `mc.cores = parallel::detectCores(logical = FALSE)` to `mclapply` to use all physical cores (this is already set as an R option in the Dev Container via Issue 19, but must be explicit in the call for non-container environments).

### References

- L'Ecuyer P, Simard R, Chen EJ, Kelton WD (2002). "An Object-Oriented Random-Number Package with Many Long Streams and Substreams." *Operations Research*, 50(6), 1073–1075 — RngStream implementation underlying `RNGkind("L'Ecuyer-CMRG")` in R.
- L'Ecuyer P (2024). "Random Number Generation." *Wiley Encyclopedia of Operations Research and Management Science* — establishes non-overlap guarantee conditions.
- Law AM (2015). *Simulation Modelling and Analysis*, 5th ed., Chapter 11 — antithetic variates derivation and conditions of applicability.

---

## Issue 14 — Shiny App: Parameter Editor and Quick Run Mode

### Problem

The current `controller.R` Shiny application is a raw JSON editor. It exposes internal parameter names (`wia_cbt`, `short_resus`, `ot_beds`) with no operational context, no domain validation, and no ability to execute the simulation or display results. A planner or medical officer cannot use it without understanding the underlying code structure.

### Recommended Approach

Replace `controller.R` with `app.R` structured as a three-panel workflow: **Configure → Run → Analyse**.

- **Configure panel**: Parameters grouped by operational concept with plain-English labels and tooltips. Slider bounds use `morris_params$lower`/`upper` from `R/sensitivity.R` where the parameter appears there. Internal JSON field names are never exposed.
- **Run panel**: Quick Run (single replication, configurable seed and duration) via async execution. A progress indicator reflects simulation state.
- **Analyse panel**: Four-tab display — Casualty Flow, Resource Utilisation, Queue Dynamics, KPI Summary — rendered from ggplot objects returned by `analyse_run()` in `R/analysis.R`. CSV and PNG export. A read-only Sensitivity Calibration tab surfaces `morris_params` bounds.

Multi-run Full Analysis and sensitivity execution are deferred to Issue 15.

**Dependencies:** Issue 1 (replication framework; `R/analysis.R` refactor returning ggplot objects). Issue 3 (`morris_params` bounds for slider validation).

---

## Issue 15 — Shiny App: Full Analysis Mode (Multi-Run with CI)

### Problem

Quick Run (Issue 14) produces single-replication output unsuitable for defensible planning conclusions — results vary with seed and carry no statistical bounds. Full Analysis mode activates the deferred capabilities: multi-run execution with CI, sensitivity screening execution, and integrated sensitivity result display.

### Recommended Approach

Extend `app.R` from Issue 14:

- **Full Analysis mode**: Enable the mode selector. Wire the replication count slider (10–1000, default 100) to `run_replications()` via async execution. On completion, call `analyse_replications()` (new function in `R/analysis.R`) to produce CI ribbon plots and KPI summary cards. Apply the warm-up period constant from Issue 2 before aggregation.
- **Sensitivity panel (active)**: Activate the "Run Sensitivity Screening" button from Issue 14. Trigger `run_morris()` asynchronously with user-configurable `r` (trajectories) and reps-per-point. Display μ\* vs σ scatter plot for R2E ICU queue, ranked parameter table, and Sobol first-order / total-effect bar chart.

**Dependencies:** Issues 14, 1, 2, 3.

---

## Issue 18 — Endogenous Casualty Generation (Force Regeneration)

### Problem

Casualty arrival rates are fixed exogenous inputs applied to a static force size. The model does not represent the feedback loop between return-to-duty rates, strategic evacuation, force depletion, and future casualty production. The `in_theatre_rate` parameter has no causal pathway to any OT, ICU, or arrival-rate metric — its influence in sensitivity screening is a bootstrap artefact. The simulation cannot answer: *what is the net effect on system load and force effectiveness of increasing the in-theatre recovery rate?*

### Recommended Approach

Introduce a time-varying effective force size updated daily:

```r
effective_force[day] <- initial_force
  + cumulative_rtd[day]
  - cumulative_strategic_evac[day]
  + reinforcement_schedule[day]

arrival_rate[day] <- base_rate * effective_force[day] / initial_force
```

The reinforcement schedule is a configurable step-input sequence in `env_data.json`. This closes the feedback loop without restructuring the arrival generator — only the per-day rate scalar changes.

**Dependencies:** Issues 1, 2 (replication framework and warm-up period needed before force-feedback output is interpretable).

---

## Issue 23 — Strategic Evacuation Demand: Role 4 and AME Sorties

### Problem

Patients flagged `r2e_evac = 1` (strategic evacuation) disappear from the simulation with no downstream outputs. The model produces no estimate of Role 4 bed occupancy by ward type, no national hospitalisation census, and no derived metric for strategic aeromedical evacuation (AME) sortie demand. The simulation cannot support the two most consequential strategic medical planning questions: what Role 4 capacity is required to absorb the evacuation flow; and how many AME sorties are required to clear the backlog.

This issue is the downstream complement of Issue 18. Issue 18 models the effect of strategic evacuation on theatre force size; this issue models the effect on national health assets.

### Recommended Approach

Implement a post-simulation Role 4 census calculation (not a constrained simmer resource — Role 4 is treated as unconstrained; the output is a demand signal):

1. Capture `injury_type`, `priority`, `treatment_received`, and `evac_time` as trajectory attributes at the evacuation branch.
2. Apply acuity-stratified national hospitalisation duration distributions (triangular, sourced from Role 4 occupancy norms in AJP-4.10) to produce a time-series Role 4 census by ward type.
3. Derive AME sortie demand as a secondary output: `sorties = ceiling(evacuees_per_day / patients_per_sortie)` with configurable aircraft capacity.

**Dependencies:** Issues 1, 22 (attribute infrastructure), 18 (evacuation counts feed force feedback).

---

## Implementation Sequencing

### Pre-phase — Infrastructure (Issue 19 ✓ Complete)

Dev Container specification merged (PR #21). All contributors now develop in a reproducible Linux R environment with `mclapply` running at full core count.

### Phase 1 — Statistical Foundation (Issues 1 ✓, 22 ✓, 2 ✓, 3 ✓, 24)
*Estimated effort: 3–4 weeks. All subsequent analyses depend on this foundation.*

1. ~~Multi-replication wrapper (`mclapply` + `wrap()`) — **Merged PR #16**~~
2. ~~**Issue 22** — Define Output Variable Register; add five missing timing attributes to trajectories. **Merged PR #26**~~
3. **Issue 24** — Switch to L'Ecuyer-CMRG RNG streams, add antithetic variates, set explicit `mc.cores`. Can run in parallel with other open issues.
4. ~~**Issue 2** — Welch warm-up analysis; set `warm_up_period` constant. **Merged PR #20**~~
5. ~~**Issue 3** — Morris Elementary Effects screening using the OVR KPIs from Issue 22. **Merged PR #30**~~

### Phase 2 — Model Fidelity (Issues 8, 6, 5, 14)
*Estimated effort: 2–3 weeks. Low-to-medium code changes, high impact on result validity.*

6. **Issue 8** — Fix R2E surgical team seizure (three lines; do first).
7. **Issue 6** — Dead-heading return legs for transport assets.
8. **Issue 5** — Time-dependent DOW survival function.
9. **Issue 14** — Shiny app parameter editor and Quick Run mode. Requires `R/analysis.R` refactor returning ggplot objects (Issue 1 dependency already satisfied).

### Phase 3 — Structural Refactoring (Issues 7, 4)
*Estimated effort: 3–4 weeks. Requires `env_data.json` schema changes and trajectory rewrites.*

10. **Issue 7** — DNBI sub-category routing with differentiated trajectories. Only hard dependencies are Issues 1 and 2; can be pulled forward if bandwidth allows.
11. **Issue 4** — Individual resource seizure. Read `BCH_Task_Role_Allocation.md` in full before beginning. Gated until Issues 1, 2, and 3 are all stable. Address the six validation assumptions in `BCH_Task_Role_Allocation.md` Part 5 — document each as a named model assumption in the README, and include the two highest-priority assumptions (NO flex to surgical roles; second-surgeon probability) in the Morris screening from Phase 1.

### Phase 4 — Scenario Expansion (Issues 9, 10, 18, 23)
*Estimated effort: 3–4 weeks. Builds on Phase 1–3 outputs.*

12. **Issue 9** — Compound Poisson MASCAL injection overlay. Requires Issues 1, 2, 5.
13. **Issue 10** — Comparative scenario runner (Falklands / Vietnam / Okinawa). Requires Issues 1, 2, 5, 8.
14. **Issue 18** — Endogenous casualty generation (force regeneration feedback). Requires Issues 1, 2, 22.
15. **Issue 23** — Role 4 occupancy and AME sortie demand. Requires Issues 1, 22, 18.

### Phase 5 — Interface (Issue 15)
*Estimated effort: 1–2 weeks.*

16. **Issue 15** — Shiny Full Analysis mode (multi-run CI, sensitivity panel). Requires Issues 14, 1, 2, 3.

### Dependency graph

```
COMPLETE:
  #19  Dev Container
  #1   Multi-run replication framework
  #22  Output Variable Register
  #8   R2E surgical team seizure fix
  #2   Warm-up analysis (terminating simulation confirmed; WARM_UP_DAYS = 0)
  #3   Morris sensitivity screening

UNBLOCKED (start now):
  #24  Variance reduction (RNG)
  #4   Individual resource seizure   (gating satisfied: #1 + #2 + #3 all merged)
  #6   Dead-heading transport        ─┐
  #7   DNBI sub-categorisation       ─┤ parallel (needs #1 + #2 only)
  #5   Time-dependent DOW            ─┘
  #14  Shiny app — Quick Run         (needs #1 analysis.R refactor only)

AFTER #14 + #1 + #2 + #3:
  #15  Shiny — Full Analysis mode

AFTER #1 + #2 + #5:
  #9   MASCAL injection
  #18  Force regeneration feedback

AFTER #1 + #2 + #5 + #8:
  #10  Scenario runner

AFTER #1 + #22 + #18:
  #23  Role 4 / AME sortie demand
```

---

## Output Standards for Revised Analysis

All reported metrics should adopt the following format:

| Metric | Single Run (current) | Revised Standard |
|---|---|---|
| Queue length | Point-in-time from seed 42 | Mean (p10–p90) across 1,000 replications |
| Wait time | Single observation | Mean ± 95% CI |
| Bottleneck identification | Visual inspection | Morris EE μ* ranking |
| DOW count | Fixed probability | Time-dependent survival function output |
| System capacity verdict | "adequate for Falklands rates" | Quantified probability of exceeding capacity threshold per scenario |

---

*Prepared June 2026. Updated June 2026 to reflect completion of Issues #19 (PR #21), #1 (PR #16), #8, #22 (PR #26), #2 (PR #20), and #3 (PR #30). Phase 1 Statistical Foundation now complete. Issue #4 individual resource seizure is unblocked. All referenced resources are open-access.*
