# 🎯 Simulation Style Guide

A purpose-driven guide for documenting simulation workflows, especially in R, with branching logic, resource synchronization, and modular trajectories.

---

## 🧩 Function & Trajectory Documentation

Use header-style comments or roxygen-style tags:

```r
#' Assigns surgery pathway based on casualty attribute and OT bed availability
#' 
#' @param env simulation environment containing global attributes and resources
#' @return trajectory object directing casualty flow through surgery or observation

```

💡 Include inputs, side effects, and decision criteria.

---

## 🧩 Function & Trajectory Documentation

Use header-style comments or roxygen-style tags:

## 🧠 Branch Logic Comments

Explicitly describe the structure and purpose of branching:

```
# Step 4: Surgery
# Branches based on attribute "surgery":
# - If surgery required → check OT availability
#     - If available → Surgery path
#     - If not → Skip surgery
# - If not required → Recover in holding bed and exit trajectory
```

> 📌 Use verbs like “Branches,” “Assigns,” “Routes,” etc.

## 🛠️ Resource Reference Patterns

Refer to resources clearly:

- Variable names like `ot_beds`, `surg_team`, `evacuation_team`

- Annotate usage policies: `# Select OT bed using shortest-queue policy`

> ⚙️ Optionally, summarize resources at script/module start.

## 📊 Global Variable Hygiene

Standardize your pattern for managing globals:

- Initialization:

```
env <- env %>% add_global("evac_wait_count", 0)
```

- Increment and annotate:

```
set_global("evac_wait_count", function() {
  current <- get_global(env, "evac_wait_count")
  updated <- current + 1
  cat("evac_wait_count:", updated, "\n")
  return(updated)
})
```

> 🧼 Consider a tracker to summarize global usage across modules.

## 📦 Visualization Annotations

Describe labeling and layout logic in charts:

- Label cleaning: `# Remove "_treated" suffix, standardize "Return to Force" labels`

- Node layout: `# Nodes grouped by echelon for readability`

## 🪢 Naming Conventions

| Element      | Style Example               | Notes                                 |
| ------------ | --------------------------- | ------------------------------------- |
| Variables    | `r2b_surgery`, `arrival_df` | Use snake_case, descriptive + concise |
| Trajectories | `"Immediate Evac"`          | Descriptive name in quotes            |
| Globals      | `evac_wait_count`           | Singular, action-oriented             |
| Resources    | `ot_beds`, `hold_beds`      | Prefix with type when helpful         |

> 🧬 Consistency improves scalability and debugging.

## 🧪 Visualization Example: Roxygen + Inline Comments

r

```
#' Selects R2E heavy team based on availability
#'
#' @return Integer index of selected team (1 to N)
#' @details Uses a weighted random selector prioritizing lowest usage.
#'          Called during immediate evacuation branching.
select_r2e_team <- function() {
  ...
}
```


