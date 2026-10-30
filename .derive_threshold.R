suppressPackageStartupMessages({library(simmer);library(simmer.bricks);library(triangle);library(dplyr);library(ggplot2)})
source("R/environment.R");source("R/trajectories.R");source("R/replication.R");source("R/scenario.R");source("R/analysis.R");source("R/airlift.R");source("R/policy_sweep.R")
json_data <- jsonlite::fromJSON("env_data.json", simplifyVector = FALSE)
apply_policy_setting(json_data, "default", saturation_threshold = 0)
policy <- env_data$vars$r2eheavy$recovery$evacuation_policy_days
set.seed(42)
rows <- run_policy_measurement(policy, n_iterations = 30, n_days = 360)
q <- rows$theatre_mean_queue
cat(sprintf("RESULT closing-90-day R2E theatre queue, release disabled: mean %.3f, 95%% CI [%.3f, %.3f], n %d\n",
            mean(q), mean(q) - qt(.975, length(q)-1)*sd(q)/sqrt(length(q)), mean(q) + qt(.975, length(q)-1)*sd(q)/sqrt(length(q)), length(q)))
print(summary(q))
