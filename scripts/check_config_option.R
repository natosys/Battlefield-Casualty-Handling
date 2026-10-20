#!/usr/bin/env Rscript
##############################################################################
## scripts/check_config_option.R                                            ##
## Regression check — every entry point runs against a named configuration  ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_config_option.R            # all checks
#   Rscript scripts/check_config_option.R --days 2   # shorter live run
#
# Exits 0 when every check passes, 1 otherwise. The rule checks are
# instantaneous; the live check runs run.R twice over a few days.
#
# Why this check exists: a configuration saved from the Shiny console could be
# run from the command line only by overwriting the tracked, shipped
# env_data.json, and no run recorded which configuration produced it. Every
# entry point now takes `--config`, validates the file before any simulation,
# and writes a manifest beside its outputs from which the run can be repeated.
# That is only worth having if the file really is the one that runs, a
# malformed one is refused naming the field, the tracked set cannot be written
# from any other file, and the manifest describes what was run rather than what
# was asked for.
#
# What it asserts. The path rules of `resolve_config_path()` on the cases that
# distinguish them; that `read_config()` names the file and the field of a
# malformed or unparseable file; that a manifest's hash equals the file's
# SHA-256 computed independently of the function under test, that its copy is
# byte-identical and its resolved copy carries the scenario overlay; and, live,
# that a file written exactly as the console's Save Configuration writes it runs
# through `run.R --config`, changes the run it describes, leaves the tracked
# env_data.json untouched, and is the configuration its manifest records. The
# byte-for-byte reproduction of the seed-42 baseline through `--config
# env_data.json` is asserted by scripts/check_baseline_reproduction.R.

suppressPackageStartupMessages({
  library(jsonlite)
})

source("R/environment.R")
source("R/cli.R")
source("R/run_manifest.R")

args <- commandArgs(trailingOnly = TRUE)

#' Read a flag's value from the command line
#'
#' @param flag Flag name, e.g. "--days"
#' @param default Value returned when the flag is absent or has no argument
#' @return The argument following the flag, or `default`
arg_value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

#' Days each live run simulates
LIVE_DAYS <- as.integer(arg_value("--days", 2L))

state <- new.env(parent = emptyenv())
state$failures <- character(0)

#' Record a failure
#'
#' @param ... sprintf() format string and its arguments
#' @return Invisible NULL; called for its side effect on the failure list
fail <- function(...) {
  state$failures <- c(state$failures, sprintf(...))
  invisible(NULL)
}

#' Print one PASS/FAIL line
#'
#' @param ok TRUE for a passing assertion
#' @param fmt sprintf() format string describing the assertion
#' @param ... Arguments to `fmt`
#' @return Invisible NULL; called for its output
report <- function(ok, fmt, ...) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", sprintf(fmt, ...)))
  if (!ok) fail(fmt, ...)
  invisible(NULL)
}

#' The message a call raises, or NA where it raises none
#'
#' @param expr Expression to evaluate.
#' @return The error message, or NA_character_.
message_of <- function(expr) {
  tryCatch({
    force(expr)
    NA_character_
  }, error = function(e) conditionMessage(e))
}

work_dir <- file.path(tempdir(), "config_option")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

# ── 1. The path rules ───────────────────────────────────────────────────────

cat("\n-- resolve_config_path --\n")

report(identical(resolve_config_path(), SHIPPED_CONFIG),
       "no switch resolves to the shipped %s", SHIPPED_CONFIG)

copy_path <- file.path(work_dir, "copy.json")
invisible(file.copy(SHIPPED_CONFIG, copy_path, overwrite = TRUE))
report(identical(resolve_config_path(copy_path), copy_path),
       "a named existing file resolves to itself")

missing_msg <- message_of(resolve_config_path(file.path(work_dir, "absent.json")))
report(grepl("--config", missing_msg, fixed = TRUE) && grepl("absent.json", missing_msg,
                                                             fixed = TRUE),
       "a missing file is refused naming the switch and the value")

dir_msg <- message_of(resolve_config_path(work_dir))
report(!is.na(dir_msg) && grepl("--config", dir_msg, fixed = TRUE),
       "a directory is refused as a configuration file")

empty_msg <- message_of(resolve_config_path(""))
report(!is.na(empty_msg) && grepl("empty", empty_msg, fixed = TRUE),
       "an empty value is refused")

report(!"alias" %in% names(formals(resolve_config_path)),
       "the retired --path alias is no longer accepted")

refresh_other <- message_of(resolve_config_path(copy_path, refresh_baseline = TRUE))
other_path <- file.path(work_dir, "other.json")
other <- fromJSON(SHIPPED_CONFIG, simplifyVector = FALSE)
other$pops[[1]]$count <- other$pops[[1]]$count + 1L
write_json(other, other_path, pretty = TRUE, auto_unbox = TRUE)
refresh_changed <- message_of(resolve_config_path(other_path, refresh_baseline = TRUE))
report(is.na(refresh_other),
       "a refresh accepts a file whose bytes are the shipped file's")
report(!is.na(refresh_changed) && grepl("--refresh-baseline", refresh_changed, fixed = TRUE),
       "a refresh rejects any other configuration, naming the switch")
report(identical(resolve_config_path(SHIPPED_CONFIG, refresh_baseline = TRUE), SHIPPED_CONFIG),
       "a refresh accepts the shipped file")

# ── 2. Validation at the boundary ───────────────────────────────────────────

cat("\n-- read_config --\n")

bad_path <- file.path(work_dir, "bad.json")
writeLines('{"pops": []}', bad_path)
bad_msg <- message_of(read_config(bad_path))
report(!is.na(bad_msg) && grepl("bad.json", bad_msg, fixed = TRUE) &&
         grepl("'elms'", bad_msg, fixed = TRUE),
       "a configuration missing a block is refused naming the file and the block")

junk_path <- file.path(work_dir, "junk.json")
writeLines("not json at all", junk_path)
junk_msg <- message_of(read_config(junk_path))
report(!is.na(junk_msg) && grepl("junk.json", junk_msg, fixed = TRUE) &&
         grepl("not valid JSON", junk_msg, fixed = TRUE),
       "an unparseable file is refused naming the file")

report(is.list(read_config(SHIPPED_CONFIG)) && is.list(read_config(other_path)),
       "the shipped file and a console-style file both load")

# ── 3. The manifest ─────────────────────────────────────────────────────────

cat("\n-- write_run_manifest --\n")

#' SHA-256 of a file computed with the system's tool, independently of the function under test
#'
#' @param path File to hash.
#' @return The hash as a lower-case hex string.
sha_independent <- function(path) {
  tool <- Sys.which("sha256sum")
  out <- if (nzchar(tool)) {
    system2(tool, shQuote(path), stdout = TRUE)
  } else {
    system2(Sys.which("shasum"), c("-a", "256", shQuote(path)), stdout = TRUE)
  }
  strsplit(out[1], " ", fixed = TRUE)[[1]][1]
}

manifest_dir <- file.path(work_dir, "manifest")
manifest_path <- write_run_manifest(
  manifest_dir, other_path,
  list(seed = 7L, days = 3L, iterations = 2L, scenario = "high_intensity"),
  command = "Rscript run.R --config other.json"
)
manifest <- fromJSON(manifest_path, simplifyVector = FALSE)

report(identical(manifest$config_sha256, sha_independent(other_path)),
       "the manifest hash is the configuration file's SHA-256")
report(identical(unname(tools::md5sum(file.path(manifest_dir, "config.json"))),
                 unname(tools::md5sum(other_path))),
       "the manifest directory holds a byte copy of the configuration")
report(identical(manifest$seed, 7L) && identical(manifest$days, 3L) &&
         identical(manifest$iterations, 2L) && identical(manifest$scenario, "high_intensity"),
       "the seed, horizon, replication count and scenario are recorded")
report(identical(manifest$command, "Rscript run.R --config other.json"),
       "the command is recorded")
report(is.character(manifest$r_version) && !is.null(manifest$packages$simmer),
       "the R and package versions are recorded")

resolved <- fromJSON(file.path(manifest_dir, "resolved_config.json"), simplifyVector = FALSE)
default_resolved <- resolve_scenario(read_config(other_path), "default")
report(!identical(resolved$vars, default_resolved$vars),
       "the resolved copy carries the scenario overlay, not the base parameters")

# ── 4. A console-saved file runs through run.R --config ─────────────────────

cat("\n-- run.R --config (live) --\n")

# Written exactly as app.R's Save Configuration writes it.
saved <- fromJSON(SHIPPED_CONFIG, simplifyVector = FALSE)
saved$pops[[1]]$count <- as.integer(saved$pops[[1]]$count / 5)
saved_path <- file.path(work_dir, "env_data_saved.json")
write_json(saved, saved_path, pretty = TRUE, auto_unbox = TRUE)

shipped_before <- unname(tools::md5sum(SHIPPED_CONFIG))

#' Run run.R for a few days and return the run's directory and exit status
#'
#' @param name Subdirectory of the work directory to write to.
#' @param extra Further arguments to pass.
#' @return List of `dir` and `status`.
run_model <- function(name, extra = character(0)) {
  out <- file.path(work_dir, name)
  status <- system2("Rscript",
                    c("run.R", "--seed", 42, "--days", LIVE_DAYS, "--iterations", 1,
                      "--output-dir", shQuote(out), extra),
                    stdout = FALSE, stderr = FALSE)
  list(dir = out, status = as.integer(status))
}

default_run <- run_model("run_default")
saved_run <- run_model("run_saved", c("--config", shQuote(saved_path)))

report(identical(default_run$status, 0L) && identical(saved_run$status, 0L),
       "run.R completes with and without --config")

#' Number of arrivals a run recorded
#'
#' @param run A run as returned by `run_model()`.
#' @return The row count of its arrival monitor.
n_arrivals <- function(run) nrow(read.csv(file.path(run$dir, "mon_arrivals.csv")))
report(n_arrivals(saved_run) != n_arrivals(default_run),
       "the saved configuration changes the run (%d arrivals against %d)",
       n_arrivals(saved_run), n_arrivals(default_run))

saved_manifest <- fromJSON(file.path(saved_run$dir, "run_manifest.json"),
                           simplifyVector = FALSE)
report(identical(saved_manifest$config_sha256, sha_independent(saved_path)),
       "the run's manifest records the saved configuration's hash")
report(identical(unname(tools::md5sum(SHIPPED_CONFIG)), shipped_before),
       "the shipped env_data.json is untouched")

bad_run <- run_model("run_bad", c("--config", shQuote(bad_path)))
report(!identical(bad_run$status, 0L) &&
         !file.exists(file.path(bad_run$dir, "mon_arrivals.csv")),
       "a malformed configuration stops run.R before any output is written")

refresh_run <- run_model("run_refresh", c("--config", shQuote(saved_path),
                                          "--refresh-baseline"))
report(!identical(refresh_run$status, 0L),
       "run.R --refresh-baseline refuses a configuration other than the shipped file")

# ── Result ──────────────────────────────────────────────────────────────────

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All configuration option checks passed.\n")
quit(status = 0)
