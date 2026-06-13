# Task-Role Allocation Matrix for Individual Resource Modelling
## Battlefield Casualty Handling Simulation — Issue 4 Supplement

**Status:** Derived assumption set — requires SME validation before use in published analysis.
**Prepared:** June 2026
**Relates to:** Action Plan Issue 4 — Team-Block Resource Seizure

---

## ADF Clinical Workforce Structure — Correction Note

The ADF deployed health workforce has three categories of clinical personnel relevant to this simulation. This structure differs materially from US military and civilian healthcare frameworks and must be correctly represented in the model.

**Medical Officer (MO):** Commissioned officer; qualified medical practitioner. Clinical decision-maker, prescriber, team leader for treatment at all echelons.

**Nursing Officer (NO):** Commissioned officer; qualified registered nurse. Advanced nursing capability, independent clinical assessment, IV therapy, medication administration, resus support. In the model's `env_data.json`, the `Nurse` resource label refers to this role.

**Health Soldier (Medic):** The single ADF enlisted clinical category. Completes a 76-week training programme producing a concurrent civilian enrolled nursing qualification alongside military-specific proficiencies including TCCC, IV access, airway support, haemorrhage control, and schedule 8 medication administration under authority. In the model's `env_data.json`, the `Medic` resource label refers to this role.

There is no separate nurse technician, nursing assistant, or secondary enlisted clinical MOS in the ADF. Every clinical soldier is a medic with the scope of practice described above. This is a materially broader scope than US combat medics (16-week training, EMT-level certification) and means ADF medics can independently manage tasks that would require a registered nurse in US military or comparable civilian frameworks — including IV therapy, medication administration, and monitoring of stable casualties without officer supervision.

The task-role matrix below reflects this structure throughout. Where the previous version of this document introduced a `nurse` enlisted role or a `medical technician (MT)` category, those labels have been corrected.

---

## Derivation Methodology

This matrix was constructed from three converging open sources in the absence of available ADF clinical SME input. No single document provides a complete task-to-role allocation table for forward-deployed ADF medical facilities; the matrix is a reasoned synthesis and is explicitly flagged as an interim assumption requiring validation.

**Source layer 1 — ADF doctrinal personnel structure**
ADDP 1.2 (Health Support to Operations, 2016) establishes the role composition of ADF treatment teams. The Cove article "Australian Role 1 vs US Role 2" (2024) provides the definitive open-access description of ADF R1 team structure (one MO, one NO, three medics) and the scope of practice boundaries between roles, including the ADF medic's 76-week programme and enrolled nursing qualification. LWP 0-5-2 provides casualty priority and treatment function references. US ATP 4-02.6 (2022) is included only for its description of US squad function; it is not used to derive ADF role boundaries given the structural differences between the two forces.

**Source layer 2 — Clinical task taxonomy**
ATLS (10th edition, 2018) defines the standard task set during trauma resuscitation and the minimum skill level required for each task. StatPearls Trauma Care Principles (Pennardt, 2023) and the ScienceDirect trauma team overview elaborate interprofessional task distribution in multi-provider resus. For surgical staffing, the AORN perioperative position statements define the evidence-based minimum OR complement (surgeon, anaesthetist, scrub person, circulating nurse).

**Source layer 3 — Staffing norms**
AACN critical care staffing standards establish the 1:2 nursing officer-to-patient ratio as the ICU baseline, with 1:1 for highest acuity. These norms are applied to the NO (not medic) role in ICU contexts, as ICU-level critical care decisions and ventilator management require registered nurse or above qualification.

---

## Part 1 — Role Inventory by Echelon

The resource labels used here align directly with the labels in `env_data.json`. The ADF role mapping is provided for clarity.

### Role 1 Treatment Team

Three R1 teams in the model. `N` = team number (1, 2, 3).

| Resource ID (proposed) | env_data.json label | ADF Role | Key Capabilities |
|---|---|---|---|
| `r1_N_doctor_1` | Doctor | Medical Officer (MO) | Team lead, all clinical decisions, advanced airway, prescribing, P1/P2 assessment |
| `r1_N_nurse_1` | Nurse | Nursing Officer (NO) | Independent nursing assessment, IV therapy, medication admin, advanced resus support |
| `r1_N_medic_1` | Medic | Health Soldier (Medic) | TCCC, IV access, haemorrhage control, airway assist, medication admin under authority, stable patient monitoring |
| `r1_N_medic_2` | Medic | Health Soldier (Medic) | As above |
| `r1_N_medic_3` | Medic | Health Soldier (Medic) | As above |

### Role 2 Basic

Two R2B elements. `N` = element number (1, 2).

| Resource ID (proposed) | env_data.json label | ADF Role | Section | Key Capabilities |
|---|---|---|---|---|
| `r2b_N_facem_1` | Facem | Emergency Physician (MO) | Emergency | Lead clinician, clinical decisions, advanced procedures, resus leadership |
| `r2b_N_nurse_1` | Nurse | Nursing Officer (NO) | Emergency | Resus bay nursing, IV therapy, monitoring, medication admin |
| `r2b_N_nurse_2` | Nurse | Nursing Officer (NO) | Emergency | As above |
| `r2b_N_nurse_3` | Nurse | Nursing Officer (NO) | Holding / flex | Holding care, patient prep for evacuation, flex to emergency |
| `r2b_N_medic_1` | Medic | Health Soldier (Medic) | Emergency | Haemorrhage control, IV access, airway assist, patient handling |
| `r2b_N_surgeon_1` | Surgeon | Surgeon (MO) | Surgical | Operative lead, DCS procedures |
| `r2b_N_surgeon_2` | Surgeon | Surgeon (MO) | Surgical | Operative assist / second surgeon for complex cases |
| `r2b_N_anaesthetist_1` | Anesthetist | Anaesthetist (MO) | Surgical | General anaesthesia, airway management in OT, pre-op assessment |
| `r2b_N_medic_evac_1` | Medic | Health Soldier (Medic) | Evacuation | In-transit care, casualty escort, mortuary handling |
| `r2b_N_medic_evac_2` | Medic | Health Soldier (Medic) | Evacuation | As above |

**Note on R2B nursing officer roles:** The three NOs at R2B cover emergency resus support and holding. In a forward austere setting, NOs may also perform scrub and circulating functions during surgery where a dedicated surgical nurse is not part of the established team. This is a key ADF-specific assumption — the surgical team at R2B (two surgeons, one anaesthetist, one medic) relies on NOs from the emergency section flexing to scrub/circ roles when not occupied with concurrent resus. This assumption carries moderate uncertainty and is listed in Part 5 as a validation priority.

### Role 2 Enhanced Heavy

One R2E element.

| Resource ID (proposed) | env_data.json label | ADF Role | Section | Key Capabilities |
|---|---|---|---|---|
| `r2e_facem_1` | Facem | Emergency Physician (MO) | Emergency | Lead clinician resus, advanced procedures |
| `r2e_nurse_1` | Nurse | Nursing Officer (NO) | Emergency | Resus bay nursing |
| `r2e_nurse_2` | Nurse | Nursing Officer (NO) | Emergency | Resus bay nursing |
| `r2e_nurse_3` | Nurse | Nursing Officer (NO) | Emergency / flex | Resus support, flex to surgical or holding |
| `r2e_medic_1` | Medic | Health Soldier (Medic) | Emergency | Haemorrhage control, IV access, patient handling |
| `r2e_surgeon_1` | Surgeon | Surgeon (MO) | Surgical | Operative lead |
| `r2e_surgeon_2` | Surgeon | Surgeon (MO) | Surgical | Operative assist / second surgeon |
| `r2e_nurse_surg_1` | Nurse | Nursing Officer (NO) | Surgical | Scrub nurse — sterile field, instrument handling |
| `r2e_nurse_surg_2` | Nurse | Nursing Officer (NO) | Surgical | Circulating nurse — non-sterile coordination |
| `r2e_nurse_surg_3` | Nurse | Nursing Officer (NO) | Surgical | Second scrub / flex for concurrent OT or complex cases |
| `r2e_nurse_surg_4` | Nurse | Nursing Officer (NO) | Surgical | Second circulating / flex |
| `r2e_anaesthetist_1` | Anesthetist | Anaesthetist (MO) | Surgical | General anaesthesia and airway management |
| `r2e_intensivist_1` | Intensivist | Intensivist (MO) | ICU | ICU clinical lead, ventilator management, complex care decisions |
| `r2e_nurse_icu_1` | Nurse | Nursing Officer (NO) | ICU | ICU bedside nursing — registered nurse level required for ICU monitoring and interventions |
| `r2e_nurse_icu_2` | Nurse | Nursing Officer (NO) | ICU | As above |
| `r2e_nurse_icu_3` | Nurse | Nursing Officer (NO) | ICU | As above |
| `r2e_nurse_icu_4` | Nurse | Nursing Officer (NO) | ICU | As above |
| `r2e_medic_evac_1` | Medic | Health Soldier (Medic) | Evacuation | In-transit care, casualty escort, mortuary handling |
| `r2e_medic_evac_2` | Medic | Health Soldier (Medic) | Evacuation | As above |

**Note on ICU nursing:** ICU nursing is assigned to NOs (not medics) in this model. The management of ventilated, multi-system trauma patients in a critical care environment requires registered nurse-level competency for independent clinical monitoring and intervention. ADF medics hold an enrolled nursing diploma, which does not encompass independent registered nurse scope. They may assist in ICU tasks under NO supervision, but the bedside ICU monitoring role is attributed to NOs. This is an assumption requiring validation — if ADF medics are routinely assigned independent ICU monitoring duties on deployment, the ICU staffing picture changes substantially.

---

## Part 2 — Task-Role Minimum Staffing Matrix

Resources not required for a given task remain available for concurrent use by other casualties or tasks. This is the primary fidelity gain over team-block seizure.

### R1 Tasks

| Task | Required Roles | Min Count | Basis |
|---|---|---|---|
| **Triage** | Doctor or Nurse (NO) | 1 | ADDP 1.2: MO-led resuscitation and triage; NO can lead triage of P3/DNBI. Both are interchangeable for initial sorting. |
| **P1/P2 treatment** | Doctor + Nurse (NO) + Medic | 1 + 1 + 1 | ATLS team: MO leads clinical assessment and decisions, NO provides advanced nursing support (IV, medications, monitoring), medic manages haemorrhage control and patient handling |
| **P3/DNBI treatment** | Nurse (NO) + Medic | 1 + 1 | Simpler presentations do not require MO presence throughout; NO leads with medic assist. Consistent with ADF medic scope and enrolled nursing qualification |
| **RTD recovery monitoring** | Medic | 1 | Stable P3 and RTD: observation, oral medications, wound checks. Within ADF medic scope under standing orders without requiring NO or MO co-presence |
| **KIA processing** | Medic | 1 | Administrative and handling task; no clinical decision-making required |
| **KIA transport** | Medic | 1 | Escort and handling only |

**Assumption note (R1):** Freeing the MO for P1/P2 cases while medics manage stable P3 and RTD independently is supported by the Cove article's description of ADF medic scope and the ADF's Authority to Carry endorsement for schedule 8 medications. The MO and NO are the scarce resources at R1 under this model; the three medics provide the bulk of patient throughput for stable cases.

### R2B Tasks

| Task | Required Roles | Min Count | Basis |
|---|---|---|---|
| **Hold bed monitoring** | Medic | 1 | Stable holding pre-resus; within ADF medic scope for basic monitoring and IV maintenance |
| **Resuscitation — long (P1/P2)** | Facem + Nurse (NO) + Nurse (NO) + Medic | 1 + 1 + 1 + 1 | ATLS full team: FACEM leads, one NO on airway/monitoring, one NO on IV/medications, medic on haemorrhage control and positioning. Basis: ScienceDirect trauma team composition; StatPearls ATLS role distribution |
| **Resuscitation — short (P3/minor)** | Nurse (NO) + Medic | 1 + 1 | Minor presentations: NO-led with medic assist; FACEM not required |
| **DAMCON surgery** | Surgeon + Anaesthetist + Nurse (NO, scrub) + Nurse (NO, circ) | 1 + 1 + 1 + 1 | AORN minimum OR standard. NOs fill scrub and circulating roles — this is the ADF-specific assumption replacing a dedicated surgical nurse MOS. NOs flex from emergency section when not in concurrent resus |
| **DAMCON surgery — complex** | Surgeon + Surgeon + Anaesthetist + Nurse (NO, scrub) + Nurse (NO, circ) | 2 + 1 + 1 + 1 | Second surgeon required for approximately 30% of DCS cases (estimate; high uncertainty — see Part 5) |
| **Post-op hold / recovery** | Medic | 1 | Post-operative stable monitoring; within ADF medic scope under standing orders |
| **KIA treatment** | Medic (evac) | 1 | Mortuary preparation; administrative task |
| **KIA transport** | Medic (evac) | 1 | Escort only |
| **Evacuation escort** | Medic (evac) | 1 | In-transit care to R2E; within medic scope |

**Critical assumption — R2B surgical nursing:** The R2B surgical team in `env_data.json` is `Anesthetist (1), Surgeon (2), Medic (1)`. The medic listed in the surgical section is interpreted here as providing instrument runner and non-sterile support functions, not as scrub or circulating nurse. The scrub and circulating roles are assumed to be filled by NOs flexing from the emergency section. This means R2B surgical capacity is constrained not only by OT bed and surgeon availability, but also by NO availability — if two P1 casualties are simultaneously in resus, the emergency NOs may not be free to support surgery. This interaction is invisible in the current team-block model and is one of the primary gains from individual resource modelling.

### R2E Heavy Tasks

| Task | Required Roles | Min Count | Basis |
|---|---|---|---|
| **DOW processing** | Medic (evac) | 1 | Administrative; as R2B |
| **DOW transport** | Medic (evac) | 1 | Escort; as R2B |
| **Hold bed monitoring (pre-resus)** | Medic | 1 | Brief stable holding; medic scope |
| **Resuscitation — short (prior R2B resus)** | Facem + Nurse (NO) | 1 + 1 | Abbreviated reassessment; FACEM and one NO adequate for review and adjustment of established resus |
| **Resuscitation — long (no prior resus)** | Facem + Nurse (NO) + Nurse (NO) + Medic | 1 + 1 + 1 + 1 | Full ATLS team as per R2B long resuscitation |
| **DAMCON surgery — first pass** | Surgeon + Anaesthetist + Nurse (NO, scrub) + Nurse (NO, circ) | 1 + 1 + 1 + 1 | AORN minimum OR standard; NOs from surgical section |
| **DAMCON surgery — second pass** | Surgeon + Anaesthetist + Nurse (NO, scrub) + Nurse (NO, circ) | 1 + 1 + 1 + 1 | Second OT at R2E enables concurrent cases; each OT requires independent surgical team |
| **ICU — long (active critical care)** | Intensivist + Nurse (NO, ICU) | 1 + 1 | Active critical phase: intensivist for clinical decisions and complex interventions, ICU NO for continuous monitoring and nursing care. AACN 1:1 standard for highest-acuity patients |
| **ICU — short (post-surgery monitoring)** | Nurse (NO, ICU) | 1 | Stable post-operative monitoring; intensivist available for rounds but not continuously seized. AACN 1:2 standard supports one NO per two recovering patients |
| **Hold / recovery** | Medic | 1 | As R2B; stable post-op and awaiting evacuation |
| **Evacuation escort** | Medic (evac) | 1 | As R2B |

---

## Part 3 — Concurrent Use Implications

| Scenario | Current behaviour | Individual model behaviour |
|---|---|---|
| Two P1 arrivals at R2B simultaneously | Entire emergency team locked; second casualty waits for all resources | Second casualty can seize whichever NOs and medics are free; FACEM may be the only contended resource |
| R2B casualty in resus, surgery required | FACEM counted as occupied if in resus team block | FACEM releases from resus as patient moves to OT; available for next arrival |
| R2B surgery and new P3 arrival simultaneously | FACEM locked in surgical team block | FACEM not required for surgery; available immediately for P3 triage and treatment |
| R2B: two concurrent resus with one NO in surgery | Invisible — team block prevents this state | NO scarcity surfaces correctly: simultaneous resus and surgery may not have enough NOs to fully staff both |
| R2E two concurrent surgeries | Surgical team not seized (current bug); effectively unlimited throughput | Each OT correctly seizes surgeon + anaesthetist + 2 NOs; concurrent cases contend for the four surgical NOs |
| R2E long ICU + new surgical case | Intensivist locked in ICU team block | Intensivist released when active critical phase ends; available for pre-op assessment |
| R2E ICU at capacity + concurrent resus | ICU NOs locked in team block | ICU NOs held only in 1:1 ratio; remaining ICU NOs potentially available for resus support if census allows |
| R2B post-op recovery | Full hold team seized per patient | One medic seized per recovering patient; NOs and FACEM available for new arrivals |

---

## Part 4 — `env_data.json` Schema Change

### Current schema (team-block, matching existing env_data.json structure)

```json
"r2b": [{
  "emerg": [["r2b_1_facem_1", "r2b_1_nurse_1", "r2b_1_nurse_2", "r2b_1_nurse_3", "r2b_1_medic_1"]],
  "surg":  [["r2b_1_surgeon_1", "r2b_1_surgeon_2", "r2b_1_anaesthetist_1", "r2b_1_medic_surg_1"]],
  "evac":  [["r2b_1_medic_evac_1", "r2b_1_medic_evac_2"]]
}]
```

### Proposed schema (individual with ADF role tags)

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
    "resus_long":    {"facem": 1, "no": 2, "medic": 1},
    "resus_short":   {"no": 1, "medic": 1},
    "surgery":       {"surgeon": 1, "anaesthetist": 1, "no": 2},
    "surgery_complex": {"surgeon": 2, "anaesthetist": 1, "no": 2},
    "hold":          {"medic": 1},
    "evac":          {"medic": 1},
    "kia":           {"medic": 1}
  }
}]
```

Note the role key `"no"` is used for Nursing Officer to clearly distinguish from a generic `"nurse"` label, avoiding confusion with civilian nursing categories.

### R helper functions (unchanged from Action Plan)

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

---

## Part 5 — Validation Priorities

Ranked by estimated impact on simulation output if the assumption is wrong:

| # | Assumption | Uncertainty | Consequence if wrong |
|---|---|---|---|
| 1 | NOs flex from emergency to scrub/circ roles at R2B when not in concurrent resus | High — ADF-specific practice, not documented in open sources | If NOs cannot flex, R2B surgery requires dedicated surgical NOs not present in the establishment; surgical capacity is zero when NOs are occupied in resus |
| 2 | Second surgeon required for ~30% of DCS cases | High — proportion is an estimate with no direct source | If higher (e.g. 60%), surgeon availability becomes a more significant bottleneck; if lower, R2B throughput improves |
| 3 | ADF medic can independently monitor stable ICU-recovery patients | Medium — enrolled nursing diploma supports monitoring but ICU scope is uncertain | If ICU NO must be present for all patient monitoring, ICU NO pool is always fully committed and unavailable for surgical flex |
| 4 | Intensivist seized only during active/long ICU phase | Medium — intensivist may conduct rounds on all patients simultaneously | If intensivist is effectively always occupied, pre-op assessment for surgical cases is blocked whenever ICU census is non-zero |
| 5 | ADF medic can independently manage stable P3/RTD at R1 without NO co-presence | Low-Medium — supported by Authority to Carry and enrolled nursing scope | If MO or NO must co-sign all patient dispositions, MO/NO become R1 bottlenecks even for low-acuity cases |
| 6 | One scrub + one circ NO per active OT at R2E | Low — consistent with AORN standard | If two scrub NOs are required per OT, R2E's four surgical NOs support only one OT at a time rather than two |

These six assumptions constitute the minimum validation agenda. Items 1 and 2 have the greatest potential to change the character of the model's bottleneck findings and should be treated as the highest priority if SME access becomes available.

---

## References

- ADDP 1.2 Health Support to Operations (2016), Chapter 1B — ADF treatment team composition, MO-led resuscitation function
- LWP 0-5-2 Staff Officers Aide-Memoir (2018) — ADF casualty priorities and treatment team references
- AJP-4.10 Allied Joint Doctrine for Medical Support (2019), NATO — Role 2 resuscitative spectrum definition
- "Australian Role 1 vs US Role 2: A Comparison of Capability and Lessons Learned." *The Cove*, February 2024. https://cove.army.gov.au/article/australian-role-1-vs-us-role-2-comparison-capability-and-lessons-learned — Definitive open-access description of ADF R1 team structure, ADF medic 76-week training and enrolled nursing qualification, Authority to Carry endorsement, and scope of practice comparison with US combat medic
- ATP 4-02.6 The Medical Company (Role 2) (2022), US Army — referenced only for US squad function descriptions; not used to derive ADF role boundaries
- Pennardt AM. "Trauma Care Principles." *StatPearls*, May 2023. https://www.ncbi.nlm.nih.gov/books/NBK547757/ — ATLS team roles and interprofessional task distribution
- Gärtner M et al. (2022). "Variation of in-hospital trauma team staffing: new resuscitation, new team." *BMC Emergency Medicine*, 22(1). https://pmc.ncbi.nlm.nih.gov/articles/PMC9479395/ — empirical trauma team composition; confirms surgeon and ER physician as the least-variable (most essential) roles
- AORN. "Position Statement on Perioperative Safe Staffing." (2021). https://www.aorn.org/docs/default-source/guidelines-resources/position-statements/personnel-staffing/posstat-staffingoncall-0721.pdf — minimum OR staffing: one scrub person + one circulating nurse per active theatre; used to establish NO surgical role allocation
- AACN. "Standards for Appropriate Staffing in Adult Critical Care." (2022). https://www.aacn.org/newsroom/standards-published-for-critical-care-nurse-staffing — 1:2 NO-to-patient ratio as ICU standard; 1:1 for highest acuity active critical care
- Hall A et al. (2023). "Surgical Capability Utilization Time for Military Casualties at Role 2 and Role 3 Facilities." *Military Medicine*, 188(11–12). https://doi.org/10.1093/milmed/usac414 — DCS team composition and OT utilisation in forward military settings; basis for second-surgeon probability estimate
