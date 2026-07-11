##############################################
## R/sensitivity.R                          ##
## Morris EE screening + Sobol follow-up   ##
##############################################

library(sensitivity)
library(dplyr)
library(tidyr)
library(ggplot2)

# ── Parameter definitions ─────────────────────────────────────────────────────

#' Parameter bounds for Morris Elementary Effects screening
#'
#' @format Data frame with columns: name, lower, upper, mode (current baseline value)
#'
#' @details Eleven parameters span treatment durations (surgery, resuscitation,
#'   ICU), DOW probability, evacuation transport times, surgical decision
#'   probabilities, in-theatre recovery rate, OT shift availability, and
#'   mass casualty event rate/size (Issue #9). Bounds are set to
#'   cover clinically plausible variation around the current baseline; see
#'   README Sensitivity Analysis section for derivation.
morris_params <- data.frame(
  name  = c("surg_mode",      "long_resus_mode", "p1_p_max",
            "r1_transport",   "r2b_transport",   "long_icu_mode",
            "pri1_surg_prob", "in_theatre_rate", "ot_hours",
            "mass_casualty_rate",    "mass_casualty_max_cas"),
  lower = c(90,    25,    0.0115, 15,   15,   770,   0.70,  0.05,  8,   0,    40),
  upper = c(150,   70,    0.046,  45,   45,   2160,  0.98,  0.20,  16,  0.4,  80),
  mode  = c(120,   45,    0.023,  30,   30,   1440,  0.90,  0.10,  12,  0,    60),
  stringsAsFactors = FALSE
)

# ── Parameter application ─────────────────────────────────────────────────────

#' Apply a named parameter vector to a copy of env_data
#'
#' @param ed  A copy of the env_data list (not modified in place)
#' @param p   Named numeric vector — names must match morris_params$name.
#'   The ot_hours entry is excluded here; it is passed directly to
#'   run_replications() because it affects build_env() scheduling, not vars.
#' @return Modified env_data copy
apply_params <- function(ed, p) {
  ed$vars$r2b$surgery$mode                  <- p[["surg_mode"]]
  ed$vars$r2eheavy$surgery$mode             <- p[["surg_mode"]]
  ed$vars$r2eheavy$long_resus$mode          <- p[["long_resus_mode"]]
  ed$vars$r2b$long_resus$mode               <- p[["long_resus_mode"]]
  ed$vars$dow$params$p1_p_max               <- p[["p1_p_max"]]
  ed$vars$r1$wia_transport$mode             <- p[["r1_transport"]]
  ed$vars$r2b$wia_transport$mode            <- p[["r2b_transport"]]
  ed$vars$r2eheavy$long_icu$mode            <- p[["long_icu_mode"]]
  ed$vars$r1$other$pri1_surgery             <- p[["pri1_surg_prob"]]
  ed$vars$r2eheavy$recovery$in_theatre_rate <- p[["in_theatre_rate"]]
  ed$vars$mass_casualty$event$rate_per_day  <- p[["mass_casualty_rate"]]
  ed$vars$mass_casualty$event$max_cas       <- p[["mass_casualty_max_cas"]]
  ed
}

# ── KPI extraction ────────────────────────────────────────────────────────────

#' Time-weighted mean resource utilisation (fraction busy) matching a pattern
#'
#' @param mon Named list with 'resources' element as returned by run_replications()
#' @param pattern Regex pattern to match resource names
#' @return Mean utilisation (0-1), time-weighted per replication then averaged
#'   across replications and matching resources. Returns 0 if no resource matches.
#'
#' @details Complements summarise_replications() (queue-based) with a
#'   utilisation-based measure. Needed for resources such as pooled transport
#'   assets that rarely queue under current baseline demand (Issue #6) but
#'   whose busy-time is still directly affected by duration parameters —
#'   queue-based KPIs alone would show near-zero sensitivity in that case.
compute_utilisation <- function(mon, pattern) {
  util <- mon$resources %>%
    filter(grepl(pattern, resource)) %>%
    group_by(replication, resource) %>%
    arrange(time) %>%
    mutate(dt = lead(time, default = max(time)) - time) %>%
    summarise(
      rep_util = weighted.mean(server / pmax(capacity, 1), w = pmax(dt, 0), na.rm = TRUE),
      .groups  = "drop"
    )
  if (nrow(util) == 0) return(0)
  mean(util$rep_util, na.rm = TRUE)
}

#' Extract KPIs from a run_replications() monitoring list
#'
#' @param mon Named list with arrivals, attributes, resources
#' @return Named numeric vector: r2e_icu_q, r2b_ot_q, r2e_ot_q, system_ot_q,
#'   dow_count, transport_q, transport_util
#'
#' @details Uses summarise_replications() for queue KPIs. DOW count is taken
#'   directly from the attributes monitor to avoid dependency on warm-up
#'   filtering. transport_q and transport_util (Issue #6) cover the pooled
#'   PMV Ambulance and HX240M transport assets.
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

  transport_q    <- safe_q("^t_PMVAmb_|^t_HX240M_")
  transport_util <- compute_utilisation(mon, "^t_PMVAmb_|^t_HX240M_")

  dow_count <- sum(
    mon$attributes$key == "dow" & mon$attributes$value == 1,
    na.rm = TRUE
  )

  c(
    r2e_icu_q      = r2e_icu_q,
    r2b_ot_q       = r2b_ot_q,
    r2e_ot_q       = r2e_ot_q,
    system_ot_q    = system_ot_q,
    dow_count      = as.numeric(dow_count),
    transport_q    = transport_q,
    transport_util = transport_util
  )
}

# ── Single-point evaluation ───────────────────────────────────────────────────

#' Run the simulation with a given parameter row and return KPIs
#'
#' @param params_row Numeric vector (length = nrow(morris_params)), in column order
#' @param n_rep      Replications per evaluation (5 recommended for Morris)
#' @param n_days     Simulation duration in days
#' @param max_cores  Optional integer cap on mclapply's mc.cores, passed
#'   through to run_replications() (see its own @param for why this
#'   matters for Shiny-triggered, locally-run screens). NULL preserves
#'   prior behaviour.
#' @return Named numeric vector: r2e_icu_q, r2b_ot_q, r2e_ot_q, system_ot_q,
#'   dow_count, transport_q, transport_util
#'
#' @details Modifies the global env_data via apply_params() then restores it.
#'   The ot_hours parameter is extracted separately and passed to run_replications()
#'   because it controls build_env() scheduling, not the vars structure.
eval_params <- function(params_row, n_rep, n_days, max_cores = NULL) {
  p    <- setNames(as.numeric(params_row), morris_params$name)
  ot_h <- p[["ot_hours"]]

  env_data <<- apply_params(env_data_base, p)
  mon      <- run_replications(n_rep, n_days, ot_hours = ot_h, max_cores = max_cores)
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
#' @param progress_dir Optional directory path; when supplied, an empty
#'   marker file ("point_<i>.done") is written to it as each design point
#'   finishes evaluating, letting a caller on another process (e.g. the
#'   Shiny app's main session) observe real "point M of N" progress. NULL
#'   (default) disables this and preserves prior behaviour for existing
#'   callers (scripts/run_sensitivity.R).
#' @param max_cores Optional integer cap on mclapply's mc.cores at each
#'   design point, passed through to run_replications() via eval_params()
#'   (see run_replications()'s own @param for why this matters for
#'   Shiny-triggered, locally-run screens). NULL preserves prior behaviour.
#' @return Named list: morris_objs (per-KPI sensitivity objects), Y (KPI matrix),
#'   X (design matrix), ranking (data frame sorted descending by mu_star)
#'
#' @details Runs r*(p+1) model evaluations where p = nrow(morris_params).
#'   Saves per-KPI Morris plots (mu* vs sigma) to images/ and a parameter
#'   ranking CSV to output_dir. The global env_data is restored to env_data_base
#'   on exit regardless of errors.
run_morris <- function(n_days = 30, n_rep = 5, r = 20, levels = 4,
                       output_dir = "outputs", progress_dir = NULL, max_cores = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create("images",   recursive = TRUE, showWarnings = FALSE)

  n_eval <- r * (nrow(morris_params) + 1L)
  message(sprintf(
    "Morris screening: r=%d, p=%d, levels=%d → %d evaluations × %d reps each",
    r, nrow(morris_params), levels, n_eval, n_rep
  ))

  env_data_base <<- env_data

  sa <- morris(
    model   = NULL,
    factors = morris_params$name,
    r       = r,
    design  = list(type = "oat", levels = levels, grid.jump = 2),
    binf    = morris_params$lower,
    bsup    = morris_params$upper,
    scale   = TRUE
  )

  message(sprintf("Evaluating %d design points...", nrow(sa$X)))

  Y <- t(vapply(seq_len(nrow(sa$X)), function(i) {
    message(sprintf("  Point %d / %d", i, nrow(sa$X)))
    kpis <- tryCatch(
      eval_params(sa$X[i, ], n_rep, n_days, max_cores = max_cores),
      error = function(e) {
        warning(sprintf("Eval %d failed: %s", i, conditionMessage(e)))
        c(r2e_icu_q = NA_real_, r2b_ot_q = NA_real_, r2e_ot_q = NA_real_,
          system_ot_q = NA_real_, dow_count = NA_real_,
          transport_q = NA_real_, transport_util = NA_real_)
      }
    )
    if (!is.null(progress_dir)) {
      file.create(file.path(progress_dir, sprintf("point_%d.done", i)))
    }
    # A full production screen runs this loop hundreds of times in one long-
    # lived process (r=20 x (p+1) = 240 design points, each building and
    # discarding a full monitoring dataset via eval_params()/run_replications()).
    # R's own garbage collector is lazy about returning memory to the OS
    # between iterations of a tight loop like this one; left unforced, that
    # slow per-iteration accumulation was observed (Issue #15 follow-up) to
    # grow a local dev container's memory usage steadily over the course of
    # a multi-hour run until it started swapping/thrashing rather than
    # failing cleanly. Forcing a full collection after every point trades a
    # small amount of wall-clock time for keeping steady-state memory flat
    # across however many points the screen runs.
    gc(full = TRUE)
    kpis
  }, numeric(7)))

  env_data <<- env_data_base

  kpi_labels <- list(
    r2b_ot_q       = "Mean R2B OT Queue",
    r2e_ot_q       = "Mean R2E OT Queue",
    system_ot_q    = "System OT Queue (R2B + R2E)",
    r2e_icu_q      = "Mean R2E ICU Queue",
    dow_count      = "Total DOW Count",
    transport_q    = "Mean Transport Queue (PMV Amb + HX240M)",
    transport_util = "Mean Transport Utilisation (PMV Amb + HX240M)"
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
        title(main = sprintf("Morris Screening — %s\n(insufficient variation to plot)",
                             kpi_labels[[kpi]]))
      }
    )
    dev.off()

    obj
  })
  names(morris_objs) <- names(kpi_labels)

  message("Morris plots saved to images/")

  # Rank parameters by mu* on system OT queue (primary bottleneck KPI)
  primary  <- morris_objs$system_ot_q
  mu_star  <- setNames(
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

#' Run Sobol variance decomposition on a selected parameter subset
#'
#' @param top_params  Character vector of parameter names from morris_params$name
#' @param n_days      Simulation duration per replication (default 30)
#' @param n_rep       Replications per Sobol evaluation point (default 5)
#' @param n_sobol     Sobol sample size N (default 200; total evals = N*(p+2))
#' @param output_dir  Directory for CSV outputs (default "outputs")
#' @param progress_dir Optional directory path; when supplied, an empty
#'   marker file ("point_<i>.done") is written to it as each design point
#'   finishes evaluating (see run_morris()'s equivalent parameter). NULL
#'   (default) disables this and preserves prior behaviour.
#' @param max_cores Optional integer cap on mclapply's mc.cores at each
#'   design point (see run_morris()'s equivalent parameter). NULL preserves
#'   prior behaviour.
#' @return Named list of sobol2007 objects: r2b_ot_q, r2e_ot_q, system_ot_q,
#'   transport_q, transport_util
#'
#' @details Applies sobol2007 (Saltelli et al. estimator) using a single design
#'   pass shared across all five KPIs, giving N*(p+2) total evaluations.
#'   Bootstrap CI uses nboot=100. Results written to output_dir as per-KPI CSVs.
run_sobol <- function(top_params, n_days = 30, n_rep = 5,
                      n_sobol = 200, output_dir = "outputs", progress_dir = NULL,
                      max_cores = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  p_idx <- which(morris_params$name %in% top_params)
  if (length(p_idx) == 0) stop("None of top_params found in morris_params$name")

  p_def   <- morris_params[p_idx, ]
  n_total <- n_sobol * (nrow(p_def) + 2L)
  message(sprintf(
    "Sobol: n=%d, p=%d → %d evaluations × %d reps (r2b_ot_q, r2e_ot_q, system_ot_q, transport_q, transport_util)",
    n_sobol, nrow(p_def), n_total, n_rep
  ))

  env_data_base <<- env_data

  X1 <- as.data.frame(mapply(function(lo, hi) runif(n_sobol, lo, hi),
                              p_def$lower, p_def$upper, SIMPLIFY = FALSE))
  X2 <- as.data.frame(mapply(function(lo, hi) runif(n_sobol, lo, hi),
                              p_def$lower, p_def$upper, SIMPLIFY = FALSE))
  names(X1) <- names(X2) <- p_def$name

  sb_r2b   <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)
  sb_r2e   <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)
  sb_sys   <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)
  sb_tq    <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)
  sb_tutil <- sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = 100)

  full_params <- setNames(morris_params$mode, morris_params$name)

  Y_all <- t(vapply(seq_len(nrow(sb_r2b$X)), function(i) {
    message(sprintf("  Sobol point %d / %d", i, nrow(sb_r2b$X)))
    row <- full_params
    row[p_def$name] <- as.numeric(sb_r2b$X[i, ])
    res <- tryCatch(
      {
        kpis <- eval_params(row, n_rep, n_days, max_cores = max_cores)
        c(r2b_ot_q       = kpis[["r2b_ot_q"]],
          r2e_ot_q       = kpis[["r2e_ot_q"]],
          system_ot_q    = kpis[["system_ot_q"]],
          transport_q    = kpis[["transport_q"]],
          transport_util = kpis[["transport_util"]])
      },
      error = function(e) {
        warning(sprintf("Sobol eval %d failed: %s", i, conditionMessage(e)))
        c(r2b_ot_q = NA_real_, r2e_ot_q = NA_real_, system_ot_q = NA_real_,
          transport_q = NA_real_, transport_util = NA_real_)
      }
    )
    if (!is.null(progress_dir)) {
      file.create(file.path(progress_dir, sprintf("point_%d.done", i)))
    }
    # See run_morris()'s identical gc() call for why: this loop runs
    # n_sobol * (p + 2) iterations (200 * 7 = 1400 at the defaults) in one
    # long-lived process — forcing a collection after every point keeps
    # steady-state memory flat rather than creeping up across the run
    # (Issue #15 follow-up).
    gc(full = TRUE)
    res
  }, numeric(5)))

  env_data <<- env_data_base

  # tell() invokes boot::boot.ci() internally, which errors on a response
  # with (near-)zero variance across the design (e.g. transport_q when none
  # of top_params affect transport occupancy — see Issue #6 PR discussion).
  # Wrapped per-KPI so one degenerate response doesn't discard the rest.
  tell_safe <- function(sb, y, kpi_name) {
    tryCatch({
      tell(sb, y)
      TRUE
    }, error = function(e) {
      warning(sprintf(
        "Sobol tell() failed for %s (likely a near-zero-variance response — %s): %s",
        kpi_name, "top_params may not include a parameter that moves this KPI",
        conditionMessage(e)
      ))
      FALSE
    })
  }

  sobol_ok <- c(
    r2b_ot_q       = tell_safe(sb_r2b,   Y_all[, "r2b_ot_q"],       "r2b_ot_q"),
    r2e_ot_q       = tell_safe(sb_r2e,   Y_all[, "r2e_ot_q"],       "r2e_ot_q"),
    system_ot_q    = tell_safe(sb_sys,   Y_all[, "system_ot_q"],    "system_ot_q"),
    transport_q    = tell_safe(sb_tq,    Y_all[, "transport_q"],    "transport_q"),
    transport_util = tell_safe(sb_tutil, Y_all[, "transport_util"], "transport_util")
  )

  # Even when tell() does not throw, boot.ci() can silently fail for an
  # individual parameter within an otherwise-successful call (e.g. one
  # parameter's bootstrap distribution is degenerate while others are not),
  # leaving sb$S / sb$T columns shorter than p_def$name. Guard against that
  # here rather than relying on tell_safe() alone.
  save_sobol <- function(sb, kpi_name) {
    p <- nrow(p_def)
    lens <- c(length(sb$S$original), length(sb$S$`min. c.i.`), length(sb$S$`max. c.i.`),
              length(sb$T$original), length(sb$T$`min. c.i.`), length(sb$T$`max. c.i.`))
    if (any(lens != p)) {
      warning(sprintf(
        "Skipping Sobol output for %s: incomplete indices (expected %d parameters, got lengths %s) — likely a degenerate bootstrap for at least one parameter.",
        kpi_name, p, paste(lens, collapse = ",")
      ))
      return(invisible(NULL))
    }
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

  sb_objs <- list(r2b_ot_q = sb_r2b, r2e_ot_q = sb_r2e, system_ot_q = sb_sys,
                   transport_q = sb_tq, transport_util = sb_tutil)
  saved <- list()
  for (kpi_name in names(sb_objs)) {
    if (sobol_ok[[kpi_name]]) {
      res <- save_sobol(sb_objs[[kpi_name]], kpi_name)
      if (!is.null(res)) saved[[kpi_name]] <- sb_objs[[kpi_name]]
    }
  }

  message("\nSobol complete.")
  saved
}
