#!/usr/bin/env Rscript
##############################################
## run.R — CLI entry point                  ##
## Battlefield Casualty Handling Simulation ##
##############################################
#
# Usage:
#   Rscript run.R [options]
#
# Options:
#   --iterations  Number of replications to run (default: 1)
#   --days        Simulation duration in days    (default: 30)
#   --seed        Random seed                    (default: 42)
#   --quick       Smoke-test mode: 5 iterations, 5 days, seed 42
#
# Example:
#   Rscript run.R --seed 42 --days 30 --iterations 1
#   Rscript run.R --quick

suppressPackageStartupMessages(library(optparse))

# ── Argument parsing ──────────────────────────────────────────────────────────

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

if (opt$quick) {
  opt$iterations <- 5L
  opt$days       <- 5L
  opt$seed       <- 42L
  message("Quick mode: iterations=5, days=5, seed=42")
}

n_iterations <- opt$iterations
n_days       <- opt$days
seed_val     <- opt$seed

message(sprintf("Run configuration: iterations=%d, days=%d, seed=%d", n_iterations, n_days, seed_val))

# ── Load modules ──────────────────────────────────────────────────────────────

source("R/environment.R")
source("R/trajectories.R")
source("R/replication.R")
source("R/analysis.R")

# ── Constants ─────────────────────────────────────────────────────────────────

day_min <- 1440L

# ── Initialise ────────────────────────────────────────────────────────────────

set.seed(seed_val)

env_data        <- load_elms("env_data.json")
counts          <- sapply(env_data$elms, length)
total_population <- env_data$pops$combat + env_data$pops$support
cie_threshold   <- (2 / 3) * total_population

# ── Build simmer environment ──────────────────────────────────────────────────

env <- simmer("Battlefield Casualty Handling")
env <- build_env(env, env_data)

# ── Build casualty trajectory ─────────────────────────────────────────────────

casualty <- build_casualty_trajectory()

# ── Add generators ────────────────────────────────────────────────────────────

env %>%
  add_generator("wia_cbt",
                casualty,
                distribution = at(generate_ln_arrivals(
                  type       = "wia_cbt",
                  mean_daily = env_data$vars$generators$wia_cbt$mean_daily,
                  sd_daily   = env_data$vars$generators$wia_cbt$sd_daily,
                  pop        = env_data$pops$combat,
                  n_days     = n_days)), mon = 2) %>%
  add_generator("kia_cbt",
                casualty,
                distribution = at(generate_ln_arrivals(
                  type       = "kia_cbt",
                  mean_daily = env_data$vars$generators$kia_cbt$mean_daily,
                  sd_daily   = env_data$vars$generators$kia_cbt$sd_daily,
                  pop        = env_data$pops$combat,
                  n_days     = n_days)), mon = 2) %>%
  add_generator("dnbi_cbt",
                casualty,
                distribution = at(generate_ln_arrivals(
                  type       = "dnbi_cbt",
                  mean_daily = env_data$vars$generators$dnbi_cbt$mean_daily,
                  sd_daily   = env_data$vars$generators$dnbi_cbt$sd_daily,
                  pop        = env_data$pops$combat,
                  n_days     = n_days)), mon = 2) %>%
  add_generator("wia_spt",
                casualty,
                distribution = at(generate_ln_arrivals(
                  type       = "wia_spt",
                  mean_daily = env_data$vars$generators$wia_spt$mean_daily,
                  sd_daily   = env_data$vars$generators$wia_spt$sd_daily,
                  pop        = env_data$pops$support,
                  n_days     = n_days)), mon = 2) %>%
  add_generator("kia_spt",
                casualty,
                distribution = at(generate_ln_arrivals(
                  type       = "kia_spt",
                  mean_daily = env_data$vars$generators$kia_spt$mean_daily,
                  sd_daily   = env_data$vars$generators$kia_spt$sd_daily,
                  pop        = env_data$pops$support,
                  n_days     = n_days)), mon = 2) %>%
  add_generator("dnbi_spt",
                casualty,
                distribution = at(generate_ln_arrivals(
                  type       = "dnbi_spt",
                  mean_daily = env_data$vars$generators$dnbi_spt$mean_daily,
                  sd_daily   = env_data$vars$generators$dnbi_spt$sd_daily,
                  pop        = env_data$pops$support,
                  n_days     = n_days)), mon = 2) %>%
  add_global("evac_wait_count", 0)

# ── Run simulation ────────────────────────────────────────────────────────────

sink(file.path("logs", "logs.txt"))
mon <- run_single(env, n_days)
sink()

message(sprintf("Simulation complete. Total arrivals: %d", nrow(mon$arrivals)))

# ── Analyse and save outputs ──────────────────────────────────────────────────

analyse_run(mon, output_dir = "outputs")

message("Analysis complete. Outputs written to outputs/")
