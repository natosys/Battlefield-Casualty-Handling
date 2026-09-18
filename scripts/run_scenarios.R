#!/usr/bin/env Rscript
##############################################
## scripts/run_scenarios.R                 ##
## Comparative scenario runner entry point ##
##############################################
#
# Terminal / Claude Code cloud:
#   Rscript scripts/run_scenarios.R --refresh-baseline       # write the tracked data/scenarios/
#   Rscript scripts/run_scenarios.R                          # default: moderate_intensity + high_intensity, 10 reps x 30 days
#   Rscript scripts/run_scenarios.R --scenarios moderate_intensity,high_intensity,default
#   Rscript scripts/run_scenarios.R --iterations 30 --days 30
#   Rscript scripts/run_scenarios.R --quick                  # smoke test (3 reps, 5 days)
#
# --refresh-baseline is the only way to write the tracked data/scenarios/ and
# the tracked images/scenario_comparison.png. Without it every invocation
# writes under outputs/ alone, so an exploratory run cannot move the evidence
# set the companion paper's centrepiece table is checked against. The flag
# fixes the protocol's replication count, horizon, seed and scenario set rather
# than accepting whichever the caller passed, because an evidence set measured
# at some other design would not be the experiment
# docs/Multi_Run_Supplement.md documents.
#
# RStudio Console (interactive):
#   source("R/environment.R"); source("R/trajectories.R"); source("R/replication.R")
#   source("R/analysis.R"); source("R/scenario_runner.R")
#   cmp <- compare_scenarios(c("moderate_intensity", "high_intensity"), n_iterations = 10, n_days = 30)
#
# The script runs to completion in any locale. Where a UTF-8 LC_CTYPE cannot be
# obtained, the only difference is that the em dash inside the scenario_label
# column of the two CSVs is escaped as <U+2014> by write.csv(); every figure,
# every plot label and the saved PNG are byte-identical either way.

# ── Locale guard ──────────────────────────────────────────────────────────────
# The scenario labels in env_data.json contain an em dash, and jsonlite flags
# them UTF-8 regardless of the session's locale. A C locale cannot represent
# that character natively, so any handling of these strings is locale-sensitive
# (Issue #153). The code itself no longer depends on the locale, but a UTF-8
# LC_CTYPE keeps the written CSVs identical to a UTF-8 session's, so one is
# requested here and its absence is reported at startup rather than after a
# full run has completed.
if (!grepl("UTF-8|utf8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) {
  for (loc in c("C.UTF-8", "en_US.UTF-8", "en_AU.UTF-8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
  }
  if (!grepl("UTF-8|utf8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) {
    message("Note: no UTF-8 locale available (LC_CTYPE is '", Sys.getlocale("LC_CTYPE"),
            "'). The run will complete, but the em dash in the scenario_label ",
            "CSV column will be written escaped as <U+2014>. Set ",
            "LANG=en_US.UTF-8 to avoid this.")
  }
}

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/scenario_runner.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--scenarios",  type = "character", default = "moderate_intensity,high_intensity",
              help = "Comma-separated scenario names to compare [default: %default]"),
  make_option("--iterations", type = "integer", default = 10L,
              help = "Replications per scenario [default: %default]"),
  make_option("--days",       type = "integer", default = 30L,
              help = "Simulation duration in days [default: %default]"),
  make_option("--seed",       type = "integer", default = 42L,
              help = "Random seed [default: %default]"),
  make_option("--quick",      action = "store_true", default = FALSE,
              help = "Smoke test: 3 iterations, 5 days"),
  make_option("--output-dir", type = "character", default = "outputs",
              help = "Directory for CSV outputs [default: %default]"),
  make_option("--images-dir", type = "character", default = NULL,
              help = paste("Directory for the comparison plot [default:",
                           "outputs/images, or images under --refresh-baseline]")),
  make_option("--refresh-baseline", action = "store_true", default = FALSE,
              help = "Write the tracked data/scenarios/ and images/ copies")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (opt$quick && isTRUE(opt$`refresh-baseline`)) {
  stop("--quick and --refresh-baseline are incompatible: a smoke test's three ",
       "replications over five days are not the experiment the tracked ",
       "evidence set records.", call. = FALSE)
}

if (opt$quick) {
  opt$iterations <- 3L
  opt$days       <- 5L
  message("Quick mode: iterations=3, days=5")
}

# A baseline refresh runs the protocol R/scenario_runner.R holds rather than
# whatever the caller passed, so the tracked set and the design the supplement
# documents cannot diverge through a mistyped argument.
if (isTRUE(opt$`refresh-baseline`)) {
  opt$scenarios  <- paste(SCENARIO_PROTOCOL_PROFILES, collapse = ",")
  opt$iterations <- SCENARIO_REPLICATIONS
  opt$days       <- SCENARIO_DAYS
  opt$seed       <- SCENARIO_SEED
  message("Baseline refresh: running the documented protocol, ",
          sprintf("%d replications x %d days x %d profiles at seed %d",
                  SCENARIO_REPLICATIONS, SCENARIO_DAYS,
                  length(SCENARIO_PROTOCOL_PROFILES), SCENARIO_SEED))
}

output_dir <- if (isTRUE(opt$`refresh-baseline`)) {
  file.path("data", "scenarios")
} else {
  opt[["output-dir"]]
}

images_dir <- if (!is.null(opt[["images-dir"]])) {
  opt[["images-dir"]]
} else if (isTRUE(opt$`refresh-baseline`)) {
  "images"
} else {
  file.path("outputs", "images")
}

scenario_names <- trimws(strsplit(opt$scenarios, ",")[[1]])

message(sprintf("Scenario comparison config: scenarios=%s, iterations=%d, days=%d, seed=%d",
                paste(scenario_names, collapse = ", "), opt$iterations, opt$days, opt$seed))

set.seed(opt$seed)
cmp <- compare_scenarios(
  scenarios    = scenario_names,
  n_iterations = opt$iterations,
  n_days       = opt$days,
  output_dir   = output_dir,
  images_dir   = images_dir
)

message("\nScenario comparison complete.")
print(cmp$totals_table)
