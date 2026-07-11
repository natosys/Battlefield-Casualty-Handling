#!/usr/bin/env Rscript
##############################################
## scripts/shiny_worker.R                   ##
## Subprocess worker for app.R's Full       ##
## Analysis / Sensitivity Screening modes   ##
##############################################
#
# Runs run_replications()/run_morris()/run_sobol() in a fresh Rscript
# process launched via system2() (real OS fork+exec, replacing the child's
# memory image entirely) rather than in-process inside a future() body.
# mclapply's own fork()-without-exec is safe here because this process has
# none of the state that made it unsafe nested one level up: no Shiny/httpuv
# event loop to inherit mid-request (multicore's failure mode — a forked
# child permanently deadlocked on a lock held by a thread that does not
# exist in the fork), and no PSOCK control socket back to a coordinating
# process for a forked grandchild to corrupt (multisession's failure mode —
# "Future ... interrupted" with no OOM or R-level cause). Both were observed
# under real Shiny usage (Issue #15 follow-up); this subprocess design
# reuses exactly the code path scripts/run_sensitivity.R and run.R already
# exercise successfully with full mclapply parallelism from a plain process.
#
# Not intended for direct interactive use — invoked by app.R via system2().

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")
source("R/sensitivity.R")
source("R/warmup.R")

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--mode",         type = "character", default = ""),
  make_option("--json",         type = "character", default = ""),
  make_option("--days",         type = "integer",   default = 30L),
  make_option("--ot-hours",     type = "double",    default = 12),
  make_option("--n-reps",       type = "integer",   default = 100L),
  make_option("--r",            type = "integer",   default = 20L),
  make_option("--n-rep",        type = "integer",   default = 5L),
  make_option("--n-sobol",      type = "integer",   default = 200L),
  make_option("--top-params",   type = "character", default = ""),
  make_option("--max-cores",    type = "character", default = ""),
  make_option("--progress-dir", type = "character", default = ""),
  make_option("--work-dir",     type = "character", default = ""),
  make_option("--output-rds",   type = "character", default = "")
)

opt <- parse_args(OptionParser(option_list = option_list))

stopifnot(nzchar(opt$mode), nzchar(opt$json), nzchar(opt[["output-rds"]]))

max_cores    <- if (nzchar(opt[["max-cores"]]))    as.integer(opt[["max-cores"]]) else NULL
progress_dir <- if (nzchar(opt[["progress-dir"]])) opt[["progress-dir"]]          else NULL

env_data <<- load_elms(opt$json)
day_min  <<- 1440L
counts   <<- sapply(env_data$elms, length)

result <- switch(opt$mode,

  "full" = {
    mon <- run_replications(opt[["n-reps"]], opt$days, ot_hours = opt[["ot-hours"]],
                            progress_dir = progress_dir, max_cores = max_cores)

    out_dir <- tempfile("bch_full_outputs_")
    img_dir <- tempfile("bch_full_images_")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)

    results <- analyse_replications(mon, output_dir = out_dir, images_dir = img_dir,
                                    warm_up_period = WARM_UP_DAYS)
    list(mon = mon, results = results)
  },

  "morris" = {
    stopifnot(nzchar(opt[["work-dir"]]))
    setwd(opt[["work-dir"]])

    res <- run_morris(n_days = opt$days, n_rep = opt[["n-rep"]], r = opt$r,
                      output_dir = "outputs", progress_dir = progress_dir,
                      max_cores = max_cores)

    png_files <- list.files("images", pattern = "^morris_.*\\.png$", full.names = TRUE)
    png_bytes <- setNames(
      lapply(png_files, function(f) readBin(f, "raw", file.info(f)$size)),
      basename(png_files)
    )
    list(res = res, png_bytes = png_bytes)
  },

  "sobol" = {
    stopifnot(nzchar(opt[["work-dir"]]), nzchar(opt[["top-params"]]))
    setwd(opt[["work-dir"]])
    top_params <- strsplit(opt[["top-params"]], ",", fixed = TRUE)[[1]]

    res <- run_sobol(top_params, n_days = opt$days, n_rep = opt[["n-rep"]],
                     output_dir = "outputs", progress_dir = progress_dir,
                     max_cores = max_cores)

    csv_files <- list.files("outputs", pattern = "^sobol_.*\\.csv$", full.names = TRUE)
    csv_bytes <- setNames(
      lapply(csv_files, function(f) readBin(f, "raw", file.info(f)$size)),
      basename(csv_files)
    )
    list(res = res, csv_bytes = csv_bytes, top_params = top_params)
  },

  stop("Unknown --mode: ", opt$mode)
)

saveRDS(result, opt[["output-rds"]])
message("shiny_worker.R complete.")
