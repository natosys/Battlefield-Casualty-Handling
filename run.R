#!/usr/bin/env Rscript
##############################################
## run.R — Simulation entry point           ##
## Battlefield Casualty Handling Simulation ##
##############################################
#
# Terminal / Claude Code cloud:
#   Rscript run.R --seed 42 --days 30 --iterations 1
#   Rscript run.R --quick
#
# RStudio Console:
#   source("run.R")          # loads run_bch() function
#   run_bch()                # default run: seed 42, 30 days, 1 iteration
#   run_bch(quick = TRUE)    # smoke test
#   run_bch(seed = 99, days = 10, iterations = 5)

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")

# ── Main function ─────────────────────────────────────────────────────────────

#' Run the BCH simulation
#'
#' @param seed    Random seed (default 42)
#' @param days    Simulation duration in days (default 30)
#' @param iterations Number of replications (default 1)
#' @param quick   Smoke-test mode: seed 42, 5 days, 5 iterations
#' @param output_dir Directory for output files (default "outputs")
#' @return Invisibly returns the monitoring data list
run_bch <- function(seed = 42L, days = 30L, iterations = 1L,
                    quick = FALSE, output_dir = "outputs") {
  if (quick) {
    seed <- 42L; days <- 5L; iterations <- 5L
    message("Quick mode: iterations=5, days=5, seed=42")
  }

  message(sprintf("Run configuration: iterations=%d, days=%d, seed=%d",
                  iterations, days, seed))

  set.seed(seed)

  env_data         <<- load_elms("env_data.json")
  total_population <- env_data$pops$combat + env_data$pops$support
  day_min          <<- 1440L
  counts           <<- sapply(env_data$elms, length)

  # env must be global so trajectory closures in R/trajectories.R can reference it
  env      <<- simmer("Battlefield Casualty Handling")
  env      <<- build_env(env, env_data)
  casualty <- build_casualty_trajectory()

  env %>%
    add_generator("wia_cbt",  casualty,
                  distribution = at(generate_ln_arrivals(
                    "wia_cbt",
                    env_data$vars$generators$wia_cbt$mean_daily,
                    env_data$vars$generators$wia_cbt$sd_daily,
                    env_data$pops$combat, days)), mon = 2) %>%
    add_generator("kia_cbt",  casualty,
                  distribution = at(generate_ln_arrivals(
                    "kia_cbt",
                    env_data$vars$generators$kia_cbt$mean_daily,
                    env_data$vars$generators$kia_cbt$sd_daily,
                    env_data$pops$combat, days)), mon = 2) %>%
    add_generator("dnbi_cbt", casualty,
                  distribution = at(generate_ln_arrivals(
                    "dnbi_cbt",
                    env_data$vars$generators$dnbi_cbt$mean_daily,
                    env_data$vars$generators$dnbi_cbt$sd_daily,
                    env_data$pops$combat, days)), mon = 2) %>%
    add_generator("wia_spt",  casualty,
                  distribution = at(generate_ln_arrivals(
                    "wia_spt",
                    env_data$vars$generators$wia_spt$mean_daily,
                    env_data$vars$generators$wia_spt$sd_daily,
                    env_data$pops$support, days)), mon = 2) %>%
    add_generator("kia_spt",  casualty,
                  distribution = at(generate_ln_arrivals(
                    "kia_spt",
                    env_data$vars$generators$kia_spt$mean_daily,
                    env_data$vars$generators$kia_spt$sd_daily,
                    env_data$pops$support, days)), mon = 2) %>%
    add_generator("dnbi_spt", casualty,
                  distribution = at(generate_ln_arrivals(
                    "dnbi_spt",
                    env_data$vars$generators$dnbi_spt$mean_daily,
                    env_data$vars$generators$dnbi_spt$sd_daily,
                    env_data$pops$support, days)), mon = 2) %>%
    add_global("evac_wait_count", 0)

  sink(file.path("logs", "logs.txt"))
  mon <- run_single(env, days)
  sink()

  message(sprintf("Simulation complete. Total arrivals: %d", nrow(mon$arrivals)))

  analyse_run(mon, output_dir = output_dir)

  message(sprintf("Analysis complete. Outputs written to %s/", output_dir))

  invisible(mon)
}

# ── CLI entry point (Rscript only) ───────────────────────────────────────────

if (!interactive()) {
  suppressPackageStartupMessages(library(optparse))

  option_list <- list(
    make_option("--iterations", type = "integer", default = 1L,
                help = "Number of replications [default: %default]"),
    make_option("--days",       type = "integer", default = 30L,
                help = "Simulation duration in days [default: %default]"),
    make_option("--seed",       type = "integer", default = 42L,
                help = "Random seed [default: %default]"),
    make_option("--quick",      action = "store_true", default = FALSE,
                help = "Smoke-test mode: 5 iterations, 5 days, seed 42")
  )

  opt <- parse_args(OptionParser(option_list = option_list))

  run_bch(seed = opt$seed, days = opt$days,
          iterations = opt$iterations, quick = opt$quick)
}
