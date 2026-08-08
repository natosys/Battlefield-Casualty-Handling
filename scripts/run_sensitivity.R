#!/usr/bin/env Rscript
##############################################
## scripts/run_sensitivity.R               ##
## Morris EE sensitivity analysis runner   ##
##############################################
#
# Terminal / Claude Code cloud:
#   Rscript scripts/run_sensitivity.R                    # full Morris (r=20, reps=5)
#   Rscript scripts/run_sensitivity.R --quick            # smoke test (r=3, reps=3, days=5)
#   Rscript scripts/run_sensitivity.R --sobol            # Morris then Sobol on top 5
#   Rscript scripts/run_sensitivity.R --r 5              # reduced-r full-coverage run (see below)
#
# RStudio Console (interactive):
#   source("R/sensitivity.R")                            # loads helpers only
#   mr <- run_morris(r=3, n_rep=3, n_days=5)            # smoke test
#   run_sobol(mr$ranking$parameter[1:5])                 # Sobol on top 5
#
# morris_params (R/sensitivity.R) covers 64 parameters; r=20 at this
# parameter count is r*(p+1) = 1,300 design points x 5 reps = 6,500
# simulation runs, impractical outside a long-lived compute session. --r 5
# was used for the Issue #112 re-run documented in the README (280 design
# points x 5 reps = 1,400 runs, ~20s/run on 4 cores); scale --r up when a
# longer session is available.
#
# The last six of the 64 are the balance coordinates of the three
# simplex-constrained composition groups (Issue #158). --sobol samples any
# selected group's composition from a Dirichlet centred on its baseline
# rather than drawing its coordinates independently; --no-dirichlet reverts
# to uniform coordinate draws.
#
# Every response in morris_kpis (R/sensitivity.R) is ranked against the same
# design, so the response count does not change how long the sweep takes.
# Each writes outputs/morris_ranking_<response>.csv and
# images/morris_<response>.png; outputs/morris_ranking.csv repeats the
# primary system OT queue ranking.

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/sensitivity.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--days",       type = "integer", default = 30L,
              help = "Simulation days per evaluation [default: %default]"),
  make_option("--reps",       type = "integer", default = 5L,
              help = "Replications per Morris evaluation point [default: %default]"),
  make_option("--r",          type = "integer", default = 20L,
              help = "Number of Morris trajectories [default: %default]"),
  make_option("--levels",     type = "integer", default = 4L,
              help = "Morris grid levels [default: %default]"),
  make_option("--quick",      action = "store_true", default = FALSE,
              help = "Smoke test: r=3, reps=3, days=5"),
  make_option("--sobol",      action = "store_true", default = FALSE,
              help = "Run Sobol decomposition on top 5 parameters after Morris"),
  make_option("--n-sobol",    type = "integer", default = 200L,
              help = "Sobol sample size N [default: %default]"),
  make_option("--no-dirichlet", action = "store_true", default = FALSE,
              help = paste(
                "Draw a selected composition group's balance coordinates",
                "independently over their screened bounds instead of",
                "sampling whole compositions from a Dirichlet centred on the",
                "baseline. Only affects --sobol runs."
              )),
  make_option("--seed",       type = "integer", default = 42L,
              help = "Random seed for reproducibility [default: %default]"),
  make_option("--output-dir", type = "character", default = "outputs",
              help = "Directory for CSV outputs [default: %default]"),
  make_option("--images-dir", type = "character", default = NULL,
              help = paste(
                "Directory for the per-response PNG plots [default:",
                "<output-dir>/images, which is gitignored]. Pass 'images' to",
                "refresh the tracked baseline plots, which is a deliberate act:",
                "a screen writes one plot per response, so the default keeps an",
                "ordinary run from scattering untracked files through the",
                "tracked images/ directory (Issue #154's contract)."
              )),
  make_option("--max-cores",  type = "integer", default = NULL,
              help = paste(
                "Cap mclapply's mc.cores per design-point evaluation. A random",
                "OAT trajectory start can land on several elevated",
                "casualty-generation-rate parameters simultaneously (Issue #112),",
                "producing a 30-day run with far more casualties/events than the",
                "baseline; at full core parallelism this can exceed the",
                "container's memory and get a worker OOM-killed (see",
                "run_replications()'s own comment on this, R/replication.R) —",
                "a partial-replication warning if some workers survive, or a",
                "hard error (caught by run_morris()'s tryCatch as an NA design",
                "point) if all do. NULL (default) preserves prior behaviour",
                "(uses all detected cores)."
              ))
)

opt <- parse_args(OptionParser(option_list = option_list))

if (opt$quick) {
  opt$r    <- 3L
  opt$reps <- 3L
  opt$days <- 5L
  message("Quick mode: r=3, reps=3, days=5")
}

message(sprintf(
  "Sensitivity config: r=%d, reps=%d, days=%d, levels=%d, seed=%d, max_cores=%s",
  opt$r, opt$reps, opt$days, opt$levels, opt$seed,
  if (is.null(opt[["max-cores"]])) "all" else opt[["max-cores"]]
))

set.seed(opt$seed)
env_data <<- load_elms("env_data.json")
day_min  <<- 1440L
counts   <<- sapply(env_data$elms, length)

morris_result <- run_morris(
  n_days     = opt$days,
  n_rep      = opt$reps,
  r          = opt$r,
  levels     = opt$levels,
  output_dir = opt[["output-dir"]],
  images_dir = if (is.null(opt[["images-dir"]])) file.path(opt[["output-dir"]], "images")
               else opt[["images-dir"]],
  max_cores  = opt[["max-cores"]]
)

# A design point at which *every* response is NA is a failed evaluation. A
# design point at which only some responses are NA is ordinary: a response
# measured over a cohort nobody entered (a mean AME wait in a run too short
# for a sortie to be scheduled, say) has no value to report, and
# extract_kpis() returns NA rather than a fictitious zero. The two are
# reported separately, since only the first indicates something went wrong.
n_failed <- sum(apply(morris_result$Y, 1, function(r) all(!is.finite(r))))
if (n_failed > 0) {
  warning(sprintf(
    paste0(
      "%d of %d design points failed outright — every response is NA (see the ",
      "'X of Y replications did not complete' warning from run_replications(), ",
      "R/replication.R, likely an OOM-killed mclapply worker at this ",
      "parameter count — try a lower --max-cores). Elementary effects for ",
      "any parameter whose trajectory passes through an affected design ",
      "point will be NA — inspect the per-response ranking CSVs' n_finite_ee ",
      "column before treating this run as authoritative."
    ),
    n_failed, nrow(morris_result$Y)
  ), call. = FALSE)
}

partial <- colSums(!is.finite(morris_result$Y))
partial <- partial[partial > 0]
if (length(partial) > 0) {
  message(sprintf(
    "\n%d response(s) could not be measured at every design point (response_na_pts in each ranking CSV): %s",
    length(partial),
    paste(sprintf("%s (%d/%d)", names(partial), partial, nrow(morris_result$Y)), collapse = ", ")
  ))
}

# Where each composition group's coordinates landed in the primary ranking,
# reported whether or not --sobol follows: the comparison against the two
# Priority 1 conditional rates is the specific question the compositions were
# brought into the screen to answer (Issue #158).
comp_rank <- match(unlist(lapply(MORRIS_COMPOSITIONS, `[[`, "coords")),
                   morris_result$ranking$parameter)
names(comp_rank) <- unlist(lapply(MORRIS_COMPOSITIONS, `[[`, "coords"))
message("\nComposition balance coordinates in the primary ranking (of ",
        nrow(morris_result$ranking), "):")
for (nm in names(comp_rank)) {
  message(sprintf("  %-22s rank %s", nm, comp_rank[[nm]]))
}
for (ref in c("pri1_evac_prob", "pri1_surg_prob")) {
  message(sprintf("  %-22s rank %s (comparison)", ref,
                  match(ref, morris_result$ranking$parameter)))
}

if (opt$sobol) {
  top5 <- head(morris_result$ranking$parameter, 5)
  message(sprintf("\nRunning Sobol on top 5 parameters: %s", paste(top5, collapse = ", ")))
  run_sobol(
    top_params  = top5,
    n_days      = opt$days,
    n_rep       = opt$reps,
    n_sobol     = opt[["n-sobol"]],
    output_dir  = opt[["output-dir"]],
    dirichlet   = !opt[["no-dirichlet"]]
  )
}

message("\nSensitivity analysis complete.")
