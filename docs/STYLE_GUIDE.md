# R Code Standard

This document is the code standard for every R source file in the repository:
the modules under `R/`, the scripts under `scripts/`, the test suite under
`tests/`, and the two root entry points `app.R` and `run.R`. `CLAUDE.md`'s Code Standards section defers to it,
and a reviewer checking a pull request checks it against this document.

It is written to be applied rather than admired. Every rule below is tagged
with how it is enforced:

| Tag | Meaning |
|---|---|
| `[lint]` | Machine-checkable. Expressible as a `lintr` rule, and belongs in the repository's lint configuration |
| `[review]` | Applied by a reviewer, and phrased so that applying it needs no judgement call |
| `[preference]` | A stated preference, not a requirement. A reviewer may raise it; a pull request is not blocked on it |

A rule that is none of the three is not a standard and does not belong here.

The `[lint]` rules are enforced by `.lintr` and `scripts/check_lint.R`, which
run in the fast check suite and in continuous integration on every pull
request. Because the existing code carries the findings recorded under
[Current conformance](#current-conformance), the gate is a ratchet: the
finding count per rule is compared against `scripts/lint_baseline.csv` and a
pull request fails only where a count has risen. A rule tagged `[review]` or
`[preference]` is deliberately absent from `.lintr`, a linter enforcing
something this document does not require being a second and undocumented
standard. Two `[lint]` rules have no `lintr` linter, D1 and R9, and
`scripts/check_lint.R` computes both itself.

The standard documents conventions the codebase already follows wherever it has
one. Where the codebase is inconsistent, the rule states the convention chosen
and the section headed "Current conformance" records how far the existing code
sits from it. Bringing existing code into conformance is separate work; this
document does not require it, and a rule is not waived because some file
predates it.

## Contents

- [Formatting and layout](#formatting-and-layout)
- [Naming conventions](#naming-conventions)
- [Function design](#function-design)
- [Constants and magic numbers](#constants-and-magic-numbers)
- [Global state](#global-state)
- [Error handling and input validation](#error-handling-and-input-validation)
- [File organisation](#file-organisation)
- [Commenting standard](#commenting-standard)
- [Simmer conventions](#simmer-conventions)
- [Regression check scripts](#regression-check-scripts)
- [Enforcement summary](#enforcement-summary)
- [Current conformance](#current-conformance)

## Formatting and layout

**F1 `[lint]` No line exceeds 100 characters.** This includes comment and
roxygen lines. A line that cannot be brought under the limit by breaking after
a comma or after `%>%` is a sign the expression is doing too much and should be
split into named intermediates.

**F2 `[preference]` Prefer 80 characters.** Most of the codebase already sits
inside 80, and prose comments read better at that width. This is a preference:
a line between 80 and 100 characters is not a defect.

**F3 `[lint]` Indent with two spaces. Never a tab.** Continuation lines are
indented one further level (two spaces) from the line they continue.

**F4 `[lint]` Assign with `<-`, never with `=`.** `=` is reserved for named
arguments in a call and for named elements in a `list()`. Superassignment is
governed by [Global state](#global-state) below.

**F5 `[lint]` Use the magrittr pipe `%>%`. Do not use the native pipe `|>`.**
The codebase is uniformly magrittr, `simmer` trajectories are built with it,
and mixing the two in one file makes a chain harder to scan.

**F6 `[lint]` Write `TRUE` and `FALSE` in full.** `T` and `F` are ordinary
variables in R and can be reassigned.

**F7 `[lint]` Quote strings with double quotes,** unless the string itself
contains a double quote, in which case single quotes avoid the escape.

**F8 `[lint]` No trailing whitespace, and every file ends with exactly one
newline.**

**F9 `[review]` One statement per line. No semicolons as statement
separators.**

**F10 `[review]` Put a space after every comma, a space on each side of every
infix operator (`<-`, `+`, `==`, `%>%`), and no space immediately inside
parentheses or brackets.** Aligning the `<-` of consecutive short assignments
into a column is permitted; the codebase does this in several places and it
reads well.

**F11 `[review]` Break a pipe chain after `%>%`, one call per line,** with the
continuation indented two spaces from the head of the chain. A chain short
enough to fit on one line inside F1 may stay on one line.

## Naming conventions

**N1 `[lint]` Every variable and function name is `snake_case`.** No dots as
word separators, no camelCase.

**N2 `[review]` Names follow the table below.**

| Element | Style example | Notes |
|---|---|---|
| Variables | `r2b_surgery`, `arrival_df` | snake_case, descriptive and concise |
| Functions | `build_casualty_trajectory()`, `run_replications()` | Verb led: the name says what the call does |
| Predicate functions | `is_open()`, `has_capacity()` | `is_` or `has_` prefix, returning a single logical |
| File-scope constants | `CHECK_SEED`, `R2E_SURGERY_SECTION_FMT` | `UPPER_SNAKE_CASE`, defined near the top of the file |
| Resources | `ot_beds`, `hold_beds`, `surg_team` | `<type>_<echelon>` where an echelon distinguishes them |
| Trajectories | `trajectory("R2B Surgery, DCS Phase 1")` | Descriptive name in quotes, title case |
| Module files | `R/trajectories.R`, `R/replication.R` | Plural noun naming the concern the module owns |
| Check scripts | `scripts/check_icu_time_conservation.R` | `check_` plus the property asserted |
| Runner scripts | `scripts/run_transport_sweep.R` | `run_` plus what is run |
| Render scripts | `scripts/render_dow_survival.R` | `render_` plus the artifact produced |

**N3 `[review]` A name that appears in an output file, a plot label or a
published table matches the name used in the code that produces it.** A reader
tracing a published figure back to its source should not have to translate.

**N4 `[review]` Do not abbreviate beyond the abbreviations the project already
uses.** R1, R2B, R2E, DCS, DOW, WIA, KIA, DNBI, AME, ICU, OT and RTD are
established and need no expansion in a name. Anything else is spelled out.

## Function design

**D1 `[lint]` No function body exceeds 100 lines.** Four existing functions
do; they are listed under [Current conformance](#current-conformance), with the
reason each is left as it stands. The limit applies in full to every new function
and to any listed function that a pull request restructures.

The number is a proxy and is worth reading as one. What the rule defends is that
a function be testable on its own, that a published figure be traceable to a
named function rather than to a position inside a long one, and that what a
function depends on be visible in its signature where a check can reason about
it. `scripts/check_analysis_decomposition.R` and `scripts/check_console_bindings.R`
exist only because there are boundaries for them to inspect. Length correlates
with all of that and measures none of it, so two consequences follow. A short
function that does several things is still wrong under D3, and passing D1 does
not excuse it. And a long function that already has those properties, the clear
case being an orchestrator whose body is a sequence of named calls, is recorded
under Current conformance with the argument rather than split to satisfy the
count; splitting one into sub-orchestrators trades an explicit dependency list
for a smaller number and makes the code harder to read, not easier.

**D2 `[preference]` Aim for 50 lines.** The median function in the codebase is
twelve lines, so this is descriptive of the code's own habit rather than an
imposition.

**D3 `[review]` A function does one thing, and its name says which thing.** A
function that computes a result and also writes it to disk is two functions. A
function whose roxygen `@details` needs the word "and then" to describe what it
does is a candidate for splitting.

**D4 `[review]` A function takes its inputs as arguments and returns its
result.** Reading configuration out of the global environment instead of
receiving it as an argument is permitted only where
[Global state](#global-state) permits it.

**D5 `[review]` Give an argument a default only where one value is genuinely
the ordinary case.** A default that exists to save typing hides which callers
depend on it.

**D6 `[review]` A function returns one type.** A function that returns a data
frame on success and `NULL` on failure forces every caller to branch; raise a
condition instead, per
[Error handling and input validation](#error-handling-and-input-validation).

## Constants and magic numbers

**C1 `[review]` Minutes per day is never the literal `1440`.** `DAY_MIN`, at the
head of `R/environment.R`, is its single definition, and the one permitted
occurrence of the literal. The `day_min` global that the execution model carries
is assigned from `DAY_MIN` by each entry point. Which of the two a given line
should name follows from when it runs: use `day_min` inside the model and the
analysis pipeline, where an entry point has already set it; use `DAY_MIN` where
one has not, as in a regression check calling into a module directly, and in a
parameter default, which cannot name a global of its own name without resolving
to the parameter itself.

**C2 `[review]` A numeric or string literal used more than once in a file is
given a named `UPPER_SNAKE_CASE` constant at the top of that file.** A literal
used once, in a context that makes its meaning obvious, needs no name.

**C3 `[review]` A value that a planner might reasonably want to change belongs
in `env_data.json`, not in R source.** Population sizes, resource counts,
distribution parameters, probabilities, schedules and thresholds are
configuration. Structural constants of the model's implementation, such as a
trajectory block's name format or a check's seed, are code.

**C4 `[review]` Write an integer literal with an `L` suffix where the value is
a count, an index or a seed.** `30L` days, `42L` seed, `1440L` minutes.

**C5 `[review]` A constant whose value comes from a source carries the citation
in its roxygen header,** in the form the academic documents use, so the code
and the document cite the same thing.

## Global state

The codebase uses `<<-` for two different purposes. One is sound and the
standard permits it; the other is a hazard and the standard constrains it.

**G1 `[review]` `<<-` is permitted for closure state:** where the name being
assigned is bound in an enclosing function and the assignment advances the
state of a generator or accumulator. The arrival generators in
`R/environment.R` are the model case, and the `fail()` accumulators in the
regression check scripts are the same pattern at a smaller scale. No
restriction beyond the ordinary ones applies here, and in particular G3's
restoration requirement does not: a generator's state belongs to the closure
that holds it, is invisible outside it, and is meant to persist across calls.
These sites are not to be converted to anything else. The distinction that
decides which rule applies is where the assigned name is bound, not the
operator: a name bound in an enclosing function is closure state, a name bound
in the global environment is configuration.

**G2 `[review]` `<<-` to the global environment is permitted only for the four
names the simulation's execution model requires there,** which are `env`,
`env_data`, `day_min` and `counts`, and only from an entry point: `run.R`,
`R/replication.R`, `R/scenario_runner.R`, a `scripts/` entry point, or a
sweep or screening driver in `R/analysis.R` or `R/sensitivity.R`. A trajectory
function, a plotting function or a helper does not write to them.

**G3 `[review]` A function that mutates one of those four names and is expected
to leave it as it found it restores it with `on.exit(..., add = TRUE)`,
registered immediately after the mutation.** Manual save-and-restore at the
foot of a function is not sufficient: an error between the two leaves the
global corrupted for every subsequent call in the session, which for a sweep or
a screen means silently wrong results rather than a visible failure. The
configuration globals have a pair of helpers for this in `R/environment.R`,
`capture_config_globals()` and `restore_config_globals()`, which also handle
the case of a name that was not bound when the snapshot was taken; the RNG
save-and-restore in `R/replication.R` and `R/sensitivity.R` is the same pattern
applied to the random number stream.

**G4 `[lint]` No other assignment to the global environment,** by `<<-`,
`assign(envir = globalenv())` or otherwise.

**G5 `[review]` A trajectory function never reads a global.** Per-entity state
is carried with `get_attribute()` and `set_attribute()`; anything a trajectory
needs from configuration is captured when the trajectory is built.

## Error handling and input validation

**E1 `[review]` A function that receives input from outside the program
validates it before use, and fails with `stop()` naming the offending value.**
Outside the program means `env_data.json`, command line arguments, a Shiny
input, or a file read from disk. A malformed configuration must surface as a
message a planner can act on, not as a subscript-out-of-bounds error from three
frames down.

**E2 `[review]` An error message names the thing, the expectation and the value
found,** and is written as a sentence. `stop(sprintf("env_data.json: r2b.surgery.pre_open_window_min must be a non-negative number, found %s", value))`, not `stop("bad input")`.

**E3 `[review]` Use `stopifnot()` for an internal invariant and `stop()` for
anything a user can cause.** An invariant is a condition that only a
programming error can violate; its failure message is for a maintainer. A
`stop()` message is for whoever ran the command.

**E4 `[review]` A module that consumes a monitoring data frame checks the
columns it depends on are present before using them,** once at the point of
entry, rather than relying on each downstream expression to fail informatively.

**E5 `[review]` `tryCatch` is used only where the failure has a defined
recovery, and the handler either recovers or re-raises.** A handler that
returns `NULL`, or that swallows the condition without a comment saying which
condition and why, is not permitted: it converts a diagnosable failure into a
wrong answer.

**E6 `[review]` `suppressWarnings()` and `suppressMessages()` carry an adjacent
comment naming the specific warning suppressed and why it is expected.** The
exception is `suppressPackageStartupMessages()` around a `library()` block,
which needs no comment.

**E7 `[review]` A script signals failure by its exit status, not by `stop()`.**
See [Regression check scripts](#regression-check-scripts).

## File organisation

**O1 `[review]` A new function goes in the module that owns its concern.**

| Concern | File |
|---|---|
| Reading `env_data.json`, generating arrivals, building the simmer environment | `R/environment.R` |
| Any `trajectory()` definition and its helpers | `R/trajectories.R` |
| Running one replication, running many, aggregating across them | `R/replication.R` |
| Computing a KPI, writing a CSV, producing a plot | `R/analysis.R` |
| Morris screening, Sobol decomposition, and their design and response handling | `R/sensitivity.R` |
| Welch warm-up analysis | `R/warmup.R` |
| Scenario overlay resolution and merging | `R/scenario.R` |
| Running the replication framework under a named scenario | `R/scenario_runner.R` |
| The Shiny Configure panel's parameter registry | `R/app_params.R` |
| Shiny UI and server logic | `app.R` |
| Command line parsing and orchestration for the main entry point | `run.R` |

A function that fits none of these needs a new module and a note in the
Repository Structure table in `CLAUDE.md` and the Codebase Structure table in
`README.md`, both updated in the same pull request.

**O2 `[review]` A file is ordered: banner comment, `library()` calls, `source()`
calls, file-scope constants, helper functions, public functions, then any
top-level execution.** A script's top-level execution is its last section.

**O3 `[review]` Sections within a file are separated by a rule comment** in the
form the codebase already uses, a `#` followed by a space, a run of box-drawing
characters, the section name, and a further run out to the line's end:

```r
# ── Helper functions ────────────────────────────────────────────────────────
```

**O4 `[review]` Every `library()` call sits at the top of the file, inside a
single `suppressPackageStartupMessages({ ... })` block where the file is a
script.** Loading a package part way down a file hides the dependency.

**O5 `[review]` A `source()` path is relative to the repository root,** because
every entry point is run from there.

## Commenting standard

The codebase's comment quality is its strongest documentation asset, running
from 21% to 55% of non-blank lines by file. These rules record the habit that
produced it so it does not depend on one author.

**R1 `[review]` Every function has a roxygen header. Without exception.** This
covers every function in `R/`, every function in `scripts/`, every function in
`app.R` and `run.R`, including a one-line helper, a `fail()` accumulator and a
function defined inside another function. A function short enough that its
header is longer than its body is still a function whose contract a reader
needs stated.

**R2 `[review]` A roxygen header carries, in order:**

- A one-line title, in the third person, saying what the function does
  ("Draws the daily arrival rate for one stream"). No trailing full stop.
- `@param` for **every** argument, naming its type and its meaning.
- `@return`, naming the type and what it represents. A function called only
  for its side effect documents that: `@return Invisibly NULL; called for the
  CSV it writes`.
- `@details`, **where the behaviour is not obvious from the title and the
  arguments**. Non-obvious means: the function has a side effect, it mutates
  global state, it makes a modelling choice a reader might question, its result
  depends on the RNG stream, or its correctness rests on an assumption stated
  elsewhere. Where none of those hold, `@details` is omitted rather than padded.

**R3 `[review]` A file-scope constant carries a roxygen header** stating what
it is, where its value comes from, and what would break if it changed.
`R2E_SURGERY_SECTION_FMT` in `R/trajectories.R` is the model.

**R4 `[review]` Every `branch()` call is preceded by a comment block describing
the branch structure and the decision criterion for each arm,** written as an
indented outline with a verb leading each arm:

```r
# Step 4: Surgery
# Branches on attribute "surgery":
# - If surgery required, check OT availability
#     - If available, route to the surgery path
#     - If not, skip surgery and route to holding
# - If not required, recover in a holding bed and exit the trajectory
```

Use verbs such as "Branches", "Assigns", "Routes", "Seizes", "Releases". Every
arm of the `branch()` appears in the block, and the block is updated in the
same commit as the branch.

**R5 `[review]` The comment explains why; the code already says what.** A
comment restating the line beneath it is noise. A comment explaining why a
seizure happens in that order, why a closure forces its arguments, or why a
tolerance is set where it is, is the reason the comment exists.

**R6 `[review]` The boundary between a comment and an academic document is the
reciprocal of the rule `CLAUDE.md` states for prose.** `README.md` and the two
analysis documents explain the model and what follows from it. A code comment
explains the implementation: reasons only a maintainer needs. Neither restates
the other. Where a comment needs to establish the model's basis, it
cross-references the document section rather than repeating it, and where a
comment records a parameter's source it cites it in the same form the documents
use.

**R7 `[review]` No commented-out code.** Version control holds the previous
version. An alternative approach worth recording is recorded in prose, saying
why it was not taken.

**R8 `[review]` Every file opens with a banner comment** naming the file and
stating in one line what it holds:

```r
##############################################
## R/trajectories.R                         ##
## All simmer trajectory definitions        ##
##############################################
```

**R9 `[lint]` No emoji or symbol characters in R source,** in code, comments or
strings, other than the box-drawing characters of a section rule (O3) and the
banner (R8). This matches the rule `scripts/check_markdown.R` enforces on
document headings, and for the same reason: a symbol's treatment depends on the
session locale.

**R10 `[review]` A comment that will date is not written.** "As of Issue #N" and
"previously this did X" belong in `docs/BCH_Simulation_Action_Plan.md`. A
comment says how the code works now. The exception is a comment recording why a
defect cannot recur, which necessarily refers to the defect.

## Simmer conventions

**S1 `[review]` Use `select()` with `seize_selected()` for policy-driven
resource selection.** Do not hardcode a resource name in `seize()` where the
choice among equivalent resources is a policy.

**S2 `[review]` Annotate a selection policy at the `select()` call:**
`# Select OT bed using shortest-queue policy`.

**S3 `[review]` Read the resource monitor with `get_mon_arrivals()` and
`get_mon_resources()` on the wrapped environment list returned by the
replication framework,** never on a bare environment held in a global.

**S4 `[review]` Per-entity state is carried in attributes,** set with
`set_attribute()` and read with `get_attribute()`. See G5.

**S5 `[review]` A trajectory's quoted name is stable and is the name the
analysis and the regression checks look for.** Where a name is constructed,
hold the format string in a documented file-scope constant so a rename cannot
leave a check searching for a label the model no longer uses.

**S6 `[review]` Seize and release in matched pairs within one function where
possible,** and where a seizure is released in a different function, say so in
both roxygen headers.

## Regression check scripts

The repository carries sixteen `scripts/check_*.R` scripts. Ten of them
already share one shape; this section makes that shape the standard.

**K1 `[review]` Name the file `check_<property>.R`,** naming the property
asserted rather than the code exercised.

**K2 `[review]` The first line is `#!/usr/bin/env Rscript`.**

**K3 `[review]` A banner (R8) is followed by a `# Usage:` block giving the
exact command, a statement of the exit contract, and a "why this check exists"
paragraph** naming the defect the check guards against and why that defect
would survive ordinary inspection of run output. A check whose reason for
existing is not written down is deleted the first time it becomes inconvenient.

**K4 `[review]` Run parameters are file-scope constants,** `CHECK_DAYS`,
`CHECK_SEED` and any others, defined together after the `source()` block.

**K5 `[review]` Failures accumulate; the script does not stop at the first
one.** The convention is a `failures <- character(0)` vector with

```r
fail <- function(...) failures <<- c(failures, sprintf(...))
```

so that one run reports every violation.

**K6 `[review]` Each assertion prints one line through a `report()` helper** in
the established form, so the output is scannable and greppable:

```r
report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
}
```

**K7 `[review]` The script ends by printing the accumulated failures and
calling `quit(status = 1)`, or a single success line and `quit(status = 0)`.**
Both calls are explicit. A check never signals failure with `stop()`: the exit
status is the contract, and a traceback is not one.

**K8 `[review]` Where a check can fail because it has lost sight of the model,
that failure is accumulated and reported separately from a model defect.**
`scripts/check_r2e_surgery_seizure.R` is the model: a pattern that matches
nothing means the check can no longer read the model, which calls for a
different response from the model being wrong, and reporting it as a model
defect sends the next reader to the wrong place.

**K9 `[review]` Command line flags are read through the established
`arg_value(flag, default)` helper,** which takes the value following the flag
and falls back to the default.

**K10 `[review]` Every helper in the script carries a roxygen header (R1),**
including `fail()`, `report()` and `arg_value()`.

### Assessment of the existing scripts

Conformance measured at the time of writing. Conforming the non-conforming
scripts is separate work and is not required by this document.

| Script | K2 shebang | K3 banner and usage | K5 and K6 helpers | K7 exit contract | K10 roxygen on helpers |
|---|---|---|---|---|---|
| `check_arrival_rate_fidelity.R` | Yes | Yes | Yes | Yes | 3 of 7 |
| `check_composition_ilr.R` | Yes | Banner only | No | Exit 1 only | 0 of 2 |
| `check_dow_calibration.R` | Yes | Yes | Yes | Yes | 2 of 5 |
| `check_env_data_summary.R` | No | No | No | Exit 1 only | 0 of 7 |
| `check_icu_time_conservation.R` | Yes | Yes | Yes | Yes | 1 of 4 |
| `check_lever_realisation.R` | Yes | Yes | Yes | Yes | 4 of 8 |
| `check_markdown.R` | No | No | No | Exit 1 only | 9 of 12 |
| `check_mass_casualty_kia_split.R` | Yes | Yes | Yes | Yes | 2 of 5 |
| `check_measurement_reproducibility.R` | Yes | Yes | Yes | Yes | 2 of 6 |
| `check_morris_baseline.R` | Yes | Banner only | No | Exit 1 only | 1 of 6 |
| `check_pre_open_window.R` | Yes | Yes | Yes | Yes | 1 of 4 |
| `check_r2e_surgery_seizure.R` | Yes | Yes | Yes | Yes | 1 of 4 |
| `check_references.R` | Yes | No | No | Exit 1 only | 4 of 4 |
| `check_replication_independence.R` | Yes | Yes | Yes | Yes | 1 of 4 |
| `check_scenario_labels.R` | Yes | Banner only | No | Exit 1 only | 2 of 3 |
| `check_screen_cache.R` | Yes | Yes | Yes | Yes | 0 of 2 |

Two of the sixteen, `check_env_data_summary.R` and `check_markdown.R`, are
maintenance scripts that rewrite the documents they check rather than pure
assertions over the model, which is why neither carries the `fail()` and
`report()` pair. They are still `check_*.R` scripts run for their exit status,
so K2, K3, K7 and K10 apply to them unchanged; only K4 to K6, K8 and K9 do not.

The most common single gap is K10: only 33 of the 83 helper functions across
the sixteen scripts carry a roxygen header, against 119 of 128 in `R/`.

## Enforcement summary

| Rule | Subject | Machine-checkable |
|---|---|---|
| F1 | Line length 100 | Yes, `line_length_linter(100)` |
| F2 | 80 preferred | No, preference |
| F3 | Two-space indent, no tabs | Yes, `indentation_linter`, `whitespace_linter` |
| F4 | `<-` not `=` | Yes, `assignment_linter(operator = "<-")` |
| F5 | magrittr pipe only | Yes, `pipe_consistency_linter(pipe = "%>%")` |
| F6 | `TRUE`/`FALSE` in full | Yes, `T_and_F_symbol_linter` |
| F7 | Double quotes | Yes, `quotes_linter` |
| F8 | Trailing whitespace, final newline | Yes, `trailing_whitespace_linter`, `trailing_blank_lines_linter` |
| F9 to F11 | Statement and spacing style | Partly, `infix_spaces_linter`, `commas_linter`, `semicolon_linter`, `spaces_inside_linter` |
| N1 | snake_case | Yes, `object_name_linter` |
| N2 to N4 | Naming table, output names, abbreviations | No, reviewer |
| D1 | Function length 100 | Yes, counted from the parse data by `scripts/check_lint.R`; `lintr` ships no linter for it |
| D2 to D6 | Function design | No, reviewer |
| C1 to C5 | Constants and magic numbers | Partly: a `1440` literal is greppable, and outside `DAY_MIN`'s definition there are none left, so the rule now reads as a gate on zero rather than as a direction of travel; the rest is reviewer |
| G1 to G3, G5 | Permitted `<<-` and restoration | No, reviewer |
| G4 | No other global assignment | Yes, `assignment_linter(operator = "<-")`, which reports every `<<-`. The permitted sites of G1 to G3 are among the counts the ratchet holds, so a new `<<-` raises a count and is reviewed on its merits |
| E1 to E7 | Error handling | No, reviewer |
| O1 to O5 | File organisation | No, reviewer |
| R1 | Roxygen on every function | Yes, a script over the source is straightforward, as the audit for this document showed |
| R2 | Mandatory tags | Partly: presence of `@param` per argument and of `@return` is checkable; whether `@details` is warranted is not |
| R3 to R8, R10 | Comment content and structure | No, reviewer |
| R9 | No emoji in source | Yes, a codepoint scan in `scripts/check_lint.R`, covering arrows, enclosed alphanumerics, geometric shapes through dingbats, the symbols supplement and the emoji planes, and permitting the box drawing of O3 and R8 |
| S1 to S6 | Simmer conventions | No, reviewer |
| K1, K2, K7 | Check script name, shebang, exit contract | Yes |
| K3 to K6, K8 to K10 | Check script structure | Partly: presence of the `fail()` and `report()` helpers is greppable, their correct use is not |

## Current conformance

Measured across the 21,129 lines of R in `R/`, `scripts/`, `app.R` and `run.R`.
These figures are the starting point the standard is applied from, not a
requirement of it.

**Already uniform.** Assignment is `<-` everywhere, with no `=` statement
assignment anywhere in the codebase (F4). The magrittr pipe is used throughout
and the native pipe nowhere (F5). `T` and `F` appear four times against
thousands of `TRUE` and `FALSE` (F6). Every module in `R/` opens with a banner
(R8) and uses the section rule (O3). Roxygen coverage in `R/` is 119 of 128
functions, and eight of the nine gaps are the paired accessors at the head of
`R/app_params.R` (R1).

**Where the code sits outside the standard.**

| Rule | Gap |
|---|---|
| F1 | 919 lines exceed 100 characters: 261 in `app.R`, 238 in `R/app_params.R`, 186 in `R/analysis.R`, 51 in `R/sensitivity.R`, the rest scattered across every file. The longest line is 974 characters |
| D1 | Four functions exceed 100 lines, in two groups. **Orchestrators, whose bodies are a sequence of named calls and which already have the properties D1 defends, recorded rather than split:** `analyse_run` (398), `analyse_replications` (229), `server` (195). **Not yet reduced:** `extract_kpis` (126). Its body is one reduction per response domain over a shared set of frames, and the two pieces that could be lifted out whole have been, `prepare_kpi_frames()` and `extract_role4_kpis()`; what remains would have to thread roughly twenty-five intermediate values back into the response assembly, which moves the coupling into a parameter list rather than removing it. The five simulation-logic functions that stood here have been split, one per pull request, each verified against the seed-42 byte-for-byte reproduction: `run_once` (147), `build_casualty_trajectory` (334), `r2b_treat_wia` (560), `r2e_treat_wia` (766) and `run_replications` (116). Trajectory construction consumes no random draws, the draws sitting inside the arguments simmer evaluates at run time, so lifting a builder out moves none of them; a phase appended to the chain (`trj %>% ...`) adds to the same trajectory object, and only `join()` copies, which the splits introduced nowhere the undivided bodies did not already do |
| E1, E4 | `R/analysis.R` validates the monitoring data and the arguments its four entry points receive, but its interior helpers assume well-formed input rather than checking it, which is the intended division and is recorded here because a helper called directly from a new caller is unchecked |
| R1 | Twelve functions in `R/` and `app.R` lack a roxygen header, and 50 of the 83 helpers across the `check_*.R` scripts do |
| K3 to K10 | Six of the sixteen check scripts sit outside the common shape; see the assessment table above |
| G4 | One `<<-` per check script, in its `fail()` accumulator, which G1 permits; these are counted rather than exempted, so adding a check script raises the count by one |

Each of these is tracked as its own item in
`docs/BCH_Simulation_Action_Plan.md`. This document does not require any of them
to be repaired; it establishes what "repaired" means.

The counts the ratchet holds are the machine's own, taken by
`scripts/check_lint.R` and tracked in `scripts/lint_baseline.csv`. They do not
all agree with the figures above, which were measured for this document by
scanning the source directly, and the difference is a matter of definition
rather than of disagreement. F1 reads 725 rather than 919 because `lintr`
measures a line in characters where the scan measured it in bytes, and a
comment carrying an em dash is three bytes but one character. D1 reads 18
rather than 17 because the parse data counts a function defined inside another
in its own right. R9 reads 130 findings, dominated by the arrow used in
comments rather than by the emoji the rule was written for. A count moves only
when a maintainer refreshes the baseline with
`Rscript scripts/check_lint.R --refresh-baseline`, which is how the ratchet
tightens after a pull request removes findings.
