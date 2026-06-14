##############################################
## R/sensitivity.R                          ##
## Morris EE screening + Sobol follow-up   ##
##############################################

library(sensitivity)
library(dplyr)
library(tidyr)
library(ggplot2)

# ── Parameter definitions ─────────────────────────────────────────────────────

#' Parameter bounds for Morris and Sobol screening
#'
#' @format Data frame with columns: name, lower, upper, mode (current value)
#'
#' @details Parameters span treatment durations (surgery, resuscitation, ICU),
#'   DOW probability, evacuation transport time, surgical decision probabilities,
#'   in-theatre recovery rate, and OT shift availability.
morris_params <- data.frame(
  name  = c("surg_mode",      "long_resus_mode", "pri1_dow",
            "r1_transport",   "r2b_transport",   "long_icu_mode",
            "pri1_surg_prob", "in_theatre_rate", "ot_hours"),
  lower = c(90,    25,    0.02,  15,   15,   770,   0.70,  0.05,  8),
  upper = c(150,   70,    0.10,  45,   45,   2160,  0.98,  0.20,  16),
  mode  = c(120,   45,    0.05,  30,   30,   1440,  0.90,  0.10,  12),
  stringsAsFactors = FALSE
)

# ── Parameter application ─────────────────────────────────────────────────────

#' Apply a named parameter vector to a copy of env_data
#'
#' @param ed  A copy of the env_data list (not modified in place)
#' @param p   Named numeric vector — names must match morris_params$name
#' @return Modified env_data copy
apply_params <- function(ed, p) {
  ed$vars$r2b$surgery$mode                  <- p[["surg_mode"]]
  ed$vars$r2eheavy$surgery$mode             <- p[["surg_mode"]]
  ed$vars$r2eheavy$long_resus$mode          <- p[["long_resus_mode"]]
  ed$vars$r2b$long_resus$mode               <- p[["long_resus_mode"]]
  ed$vars$r1$other$pri1_dow                 <- p[["pri1_dow"]]
  ed$vars$r1$wia_transport$mode             <- p[["r1_transport"]]
  ed$vars$r2b$wia_transport$mode            <- p[["r2b_transport"]]
  ed$vars$r2eheavy$long_icu$mode            <- p[["long_icu_mode"]]
  ed$vars$r1$other$pri1_surgery             <- p[["pri1_surg_prob"]]
  ed$vars$r2eheavy$recovery$in_theatre_rate <- p[["in_theatre_rate"]]
  ed
}

# ── KPI extraction ────────────────────────────────────────────────────────────

#' Extract KPIs from a run_replications() monitoring list
#'
#' @param mon Named list with arrivals, attributes, resources
#' @return Named numeric vector: r2e_icu_q, r2b_ot_q, r2e_ot_q, system_ot_q, dow_count
extract_kpis <- function(mon) {
  kpi <- summarise_replications(mon)

  safe_q <- function(pattern) {
    v <- kpi %>%
      filter(grepl(pattern, resource)) %>%
      summarise(v = mean(mean_q, na.rm = TRUE)) %>%
      pull(v)
    if (length(v) == 0 || is.na(v)) 0 else v
  }

  r2e_icu_q   <- safe_q("^b_r2eheavy_icu_")
  r2b_ot_q    <- safe_q("^b_r2b_ot_")
  r2e_ot_q    <- safe_q("^b_r2eheavy_ot_")
  system_ot_q <- r2b_ot_q + r2e_ot_q

  dow_count <- sum(
    mon$attributes$key == "dow" & mon$attributes$value == 1,
    na.rm = TRUE
  )

  c(
    r2e_icu_q   = r2e_icu_q,
    r2b_ot_q    = r2b_ot_q,
    r2e_ot_q    = r2e_ot_q,
    system_ot_q = system_ot_q,
    dow_count   = as.numeric(dow_count)
  )
}

# ── Single-point evaluation ───────────────────────────────────────────────────

#' Run the simulation with a given parameter row and return KPIs
#'
#' @param params_row Numeric vector (length = nrow(morris_params)), in column order
#' @param n_rep      Replications per evaluation (5 recommended for Morris)
#' @param n_days     Simulation duration in days
#' @return Named numeric vector: r2e_icu_q, r2b_ot_q, r2e_ot_q, system_ot_q, dow_count
eval_params <- function(params_row, n_rep, n_days) {
  p      <- setNames(as.numeric(params_row), morris_params$name)
  ot_h   <- p[["ot_hours"]]

  env_data <<- apply_params(env_data_base, p)
  mon      <- run_replications(n_rep, n_days, ot_hours = ot_h)
  extract_kpis(mon)
}

# ── Morris screening ──────────────────────────────────────────────────────────

#' Run Morris Elementary Effects screening
#'
#' @param n_days     Simulation duration per replication (default 30)
#' @param n_rep      Replications per Morris evaluation point (default 5)
#' @param r          Number of Morris trajectories (default 20)
#' @param levels     Number of grid levels (default 4)
#' @param output_dir Directory for CSV and PNG outputs (default "outputs")
#' @return Named list: morris_obj (sensitivity object), Y (KPI matrix),
#'   ranking (data frame sorted by mu_star for system_ot_q)
#'
#' @details Saves per-KPI Morris plots to images/ and a parameter ranking CSV
#'   to output_dir. The global env_data is restored to env_data_base on exit.
run_morris <- function(n_days = 30, n_rep = 5, r = 20, levels = 4,
                       output_dir = "outputs") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create("images",   recursive = TRUE, showWarnings = FALSE)

  n_eval <- r * (nrow(morris_params) + 1L)
  message(sprintf(
    "Morris screening: r=%d, p=%d, levels=%d → %d evaluations × %d reps",
    r, nrow(morris_params), levels, n_eval, n_rep
  ))

  env_data_base <<- env_data

  sa <- morris(
    model  = NULL,
    factors = morris_params$name,
    r      = r,
    design = list(type = "oat", levels = levels, grid.jump = 2),
    binf   = morris_params$lower,
    bsup   = morris_params$upper,
    scale  = TRUE
  )

  message(sprintf("Evaluating %d design points...", nrow(sa$X)))

  Y <- t(vapply(seq_len(nrow(sa$X)), function(i) {
    message(sprintf("  Point %d / %d", i, nrow(sa$X)))
    tryCatch(
      eval_params(sa$X[i, ], n_rep, n_days),
      error = function(e) {
        warning(sprintf("Eval %d failed: %s", i, conditionMessage(e)))
        c(r2e_icu_q = NA_real_, r2b_ot_q = NA_real_, r2e_ot_q = NA_real_,
          system_ot_q = NA_real_, dow_count = NA_real_)
      }
    )
  }, numeric(5)))

  env_data <<- env_data_base

  kpi_labels <- list(
    r2b_ot_q    = "Mean R2B OT Queue",
    r2e_ot_q    = "Mean R2E OT Queue",
    system_ot_q = "System OT Queue (R2B + R2E)",
    r2e_icu_q   = "Mean R2E ICU Queue",
    dow_count   = "Total DOW Count"
  )

  morris_objs <- lapply(names(kpi_labels), function(kpi) {
    obj <- sa
    tell(obj, Y[, kpi])

    png(file.path("images", sprintf("morris_%s.png", kpi)),
        width = 900, height = 650, res = 120)
    tryCatch(
      plot(obj, main = sprintf("Morris Screening — %s", kpi_labels[[kpi]])),
      error = function(e) {
        plot.new()
        title(main = sprintf("Morris Screening — %s\n(insufficient variation to plot)", kpi_labels[[kpi]]))
      }
    )
    dev.off()

    obj
  })
  names(morris_objs) <- names(kpi_labels)

  message("Morris plots saved to images/")

  # Rank by mu* on primary KPI (system OT queue)
  primary <- morris_objs$system_ot_q
  mu_star <- setNames(
    as.numeric(apply(abs(primary$ee), 2, mean, na.rm = TRUE)),
    morris_params$name
  )
  sigma_ee <- setNames(
    as.numeric(apply(primary$ee, 2, sd, na.rm = TRUE)),
    morris_params$name
  )

  ranking <- data.frame(
    parameter = names(mu_star),
    mu_star   = mu_star,
    sigma_ee  = sigma_ee,
    row.names = NULL
  ) %>% arrange(desc(mu_star))

  write.csv(ranking, file.path(output_dir, "morris_ranking.csv"), row.names = FALSE)
  message("Parameter ranking written to outputs/morris_ranking.csv")
  message("\nTop parameters by mu* (system OT queue):")
  print(ranking, digits = 4)

  list(morris_objs = morris_objs, Y = Y, X = sa$X, ranking = ranking)
}

# ── Sobol variance decomposition ──────────────────────────────────────────────

#' Run Sobol variance decomposition on top parameters for three OT KPIs
#'
#' @param top_params  Character vector of parameter names from morris_params$name
#' @param n_days      Simulation duration per replication (default 30)
#' @param n_rep       Replications per Sobol evaluation point (default 5)
#' @param n_sobol     Sobol sample size N (default 200; total evals = N*(p+2))
#' @param output_dir  Directory for CSV outputs (default "outputs")
#' @return Named list of sobol2007 objects: r2b_ot_q, r2e_ot_q, system_ot_q
#'
#' @details Uses sobol2007 (Saltelli et al., 2010). A single design pass extracts
#'   all three OT KPIs simultaneously; total evaluations = N*(p+2), not 3*N*(p+2).
run_sobol <- function(top_params, n_days = 30, n_rep = 5,
                      n_sobol = 200, output_dir = "outputs") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  p_idx <- which(morris_params$name %in% top_params)
  if (length(p_idx) == 0) stop("None of top_params found in morris_params$name")

  p_def   <- morris_params[p_idx, ]
  n_total <- n_sobol * (nrow(p_def) + 2)
  message(sprintf(
    "Sobol: n=%d, p=%d → %d evaluations × %d reps (r2b_ot_q, r2e_ot_q, system_ot_q)",
    n_sobol, nrow(p_def), n_total, n_rep
  ))

  env_data_base <<- env_data

  X1 <- as.data.frame(mapply(function(lo, hi) runif(n_sobol, lo, hi),
                              p_def$lower, p_def$upper, SIMPLIFY = FALSE))
  X2 <- as.data.frame(mapply(function(lo, hi) runif(n_sobol, lo, hi),
                              p_def$lower, p_def$upper, SIMPLIFY = FALSE))
  names(X1) <- names(X2) <- p_def$name

  sb_r2b <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)
  sb_r2e <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)
  sb_sys <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)

  full_params <- setNames(morris_params$mode, morris_params$name)

  Y_all <- t(vapply(seq_len(nrow(sb_r2b$X)), function(i) {
    message(sprintf("  Sobol point %d / %d", i, nrow(sb_r2b$X)))
    row <- full_params
    row[p_def$name] <- as.numeric(sb_r2b$X[i, ])
    tryCatch(
      {
        kpis <- eval_params(row, n_rep, n_days)
        c(r2b_ot_q    = kpis[["r2b_ot_q"]],
          r2e_ot_q    = kpis[["r2e_ot_q"]],
          system_ot_q = kpis[["system_ot_q"]])
      },
      error = function(e) {
        warning(sprintf("Sobol eval %d failed: %s", i, conditionMessage(e)))
        c(r2b_ot_q = NA_real_, r2e_ot_q = NA_real_, system_ot_q = NA_real_)
      }
    )
  }, numeric(3)))

  env_data <<- env_data_base

  tell(sb_r2b, Y_all[, "r2b_ot_q"])
  tell(sb_r2e, Y_all[, "r2e_ot_q"])
  tell(sb_sys, Y_all[, "system_ot_q"])

  save_sobol <- function(sb, kpi_name) {
    results <- data.frame(
      parameter = p_def$name,
      S1        = sb$S$original,
      S1_lower  = sb$S$`min. c.i.`,
      S1_upper  = sb$S$`max. c.i.`,
      ST        = sb$T$original,
      ST_lower  = sb$T$`min. c.i.`,
      ST_upper  = sb$T$`max. c.i.`
    )
    write.csv(results, file.path(output_dir, sprintf("sobol_%s.csv", kpi_name)),
              row.names = FALSE)
    message(sprintf("\nSobol indices for %s:", kpi_name))
    print(results, digits = 4)
    results
  }

  save_sobol(sb_r2b, "r2b_ot_q")
  save_sobol(sb_r2e, "r2e_ot_q")
  save_sobol(sb_sys, "system_ot_q")

  message("\nSobol complete.")
  list(r2b_ot_q = sb_r2b, r2e_ot_q = sb_r2e, system_ot_q = sb_sys)
}
