##############################################
## R/cli.R                                  ##
## Command-line argument validation         ##
##############################################
#
# Dependency-free (base R only) so a regression check can source this file
# alone and exercise every rule without loading simmer, without running the
# model, and without triggering an entry point's own CLI block.
#
# Why this lives in its own file. Command-line arguments are input from
# outside the program exactly as env_data.json is, and the style guide (E1,
# E2) requires them validated before use, failing with a message that names
# the field and the value found. Left unchecked, `--days 0` would be
# accepted, a full simulation attempted, and the failure raised inside
# the analysis pipeline naming a monitoring data frame the user never
# referred to. Keeping the rules here rather than inside an entry point's
# `if (!interactive())` block is what lets them be tested in milliseconds
# instead of by spawning a process per case.

# ── Value validators ─────────────────────────────────────────────────────────

#' Reject a switch whose value is not a single number within a range
#'
#' @param value Value parsed for the switch.
#' @param switch_name Switch as the user typed it, used in the message.
#' @param min_value Smallest permitted value.
#' @param max_value Largest permitted value (default Inf).
#' @return Invisibly `value`, having passed; stops otherwise.
#'
#' @details Length and finiteness are checked alongside the range so that NA,
#'   NULL and a vector of values are rejected by the same rule rather than
#'   reaching a comparison that would return NA and be treated as FALSE.
require_number_in_range <- function(value, switch_name, min_value, max_value = Inf) {
  if (length(value) != 1L || !is.numeric(value) || !is.finite(value) ||
        value < min_value || value > max_value) {
    stop(sprintf("%s must be a single number between %s and %s, and was %s",
                 switch_name, format(min_value), format(max_value),
                 paste(format(value), collapse = ", ")), call. = FALSE)
  }
  invisible(value)
}

#' Reject a directory switch that names nothing
#'
#' @param value Value parsed for the switch, or NULL where it is optional.
#' @param switch_name Switch as the user typed it, used in the message.
#' @return Invisibly `value`, having passed; stops otherwise.
#'
#' @details NULL passes, an optional directory switch being absent rather
#'   than empty; an empty or multi-element string does not.
require_directory <- function(value, switch_name) {
  if (is.null(value)) return(invisible(value))
  if (length(value) != 1L || !is.character(value) || !nzchar(value)) {
    stop(sprintf("%s must name a directory, and was empty", switch_name),
         call. = FALSE)
  }
  invisible(value)
}

# ── Rules spanning more than one switch ──────────────────────────────────────

#' Validate a warm-up period against the run length it is taken from
#'
#' @param warm_up Warm-up days to exclude from the analysis window.
#' @param days Run length in days.
#' @return Invisibly `warm_up`, having passed; stops otherwise.
#'
#' @details A warm-up at or beyond the run length leaves nothing to analyse,
#'   which surfaces downstream as summary statistics taken over no rows rather
#'   than as a rejected argument.
validate_warm_up <- function(warm_up, days) {
  require_number_in_range(warm_up, "--warm-up", 0)
  if (warm_up >= days) {
    stop(sprintf(paste("--warm-up must be less than the run length, and was %s",
                       "against %s days: a warm-up at or beyond the run length",
                       "leaves the analysis window empty"),
                 format(warm_up), format(days)), call. = FALSE)
  }
  invisible(warm_up)
}

#' Resolve the execution mode, checking it against the replication count
#'
#' @param mode "single", "multi", or NULL to infer from `iterations`.
#' @param iterations Replication count.
#' @return The resolved mode, "single" or "multi"; stops on a contradiction.
#'
#' @details The two modes produce different artifact sets: only single-run
#'   mode writes logs.txt and the arrival diagnostics, those being records of
#'   one event stream. Inferring the mode when it is omitted is what keeps
#'   every invocation documented before this switch existed behaving as it did.
resolve_run_mode <- function(mode, iterations) {
  implied <- if (iterations == 1L) "single" else "multi"
  if (is.null(mode)) return(implied)

  if (length(mode) != 1L || !mode %in% c("single", "multi")) {
    stop(sprintf("--mode must be 'single' or 'multi', and was %s",
                 sQuote(paste(mode, collapse = ", "))), call. = FALSE)
  }
  if (mode != implied) {
    stop(sprintf(paste("--mode %s contradicts --iterations %s, which implies %s:",
                       "pass one or the other, or make them agree"),
                 mode, format(iterations), implied), call. = FALSE)
  }
  mode
}

#' Validate a baseline refresh against the run it would be written from
#'
#' @param refresh_baseline TRUE where the tracked evidence set is to be written.
#' @param iterations Replication count.
#' @param scenario Name of the scenario profile in force.
#' @return Invisibly TRUE, having passed; stops otherwise.
#'
#' @details Two conditions protect the tracked set, and each names its own
#'   reason rather than surfacing as arithmetic about the iteration count. A
#'   multi-run refresh could only ever write part of the set; a
#'   refresh under a profile would leave the set describing a configuration
#'   the repository does not ship.
validate_baseline_refresh <- function(refresh_baseline, iterations, scenario) {
  if (!isTRUE(refresh_baseline)) return(invisible(TRUE))

  if (iterations != 1L) {
    stop(sprintf(paste("--refresh-baseline requires single-run mode, and --iterations",
                       "was %s: logs.txt and the arrival diagnostics are records of one",
                       "run's event stream and have no multi-replication equivalent, so",
                       "a multi-run refresh could only write part of the set"),
                 format(iterations)), call. = FALSE)
  }
  if (!identical(scenario, "default")) {
    stop(sprintf(paste("--refresh-baseline requires --scenario default, and was %s:",
                       "the tracked baseline evidence set describes the shipped",
                       "configuration, so a profile must not write it"),
                 sQuote(scenario)), call. = FALSE)
  }
  invisible(TRUE)
}

# ── Configuration file switch ────────────────────────────────────────────────

#' Path of the shipped configuration, the default of every `--config` switch
SHIPPED_CONFIG <- "env_data.json"

#' Resolve the configuration file a command-line entry point is to run against
#'
#' @param config Value of `--config`, or NULL where it was not given.
#' @param refresh_baseline TRUE where the tracked evidence set is to be written.
#' @param shipped Path of the shipped configuration.
#' @return The path of the configuration file to load.
#'
#' @details The shipped `env_data.json` is the default, so an invocation that
#'   names no file behaves as it did before the switch existed. The file is
#'   checked to exist here, before any simulation, and its content is validated
#'   where it is read (`read_config()` in R/environment.R). A baseline refresh
#'   rejects any other file: the tracked evidence describes the shipped
#'   configuration only, and a configuration whose bytes equal the shipped
#'   file's is that configuration whatever it is called.
resolve_config_path <- function(config = NULL, refresh_baseline = FALSE,
                                shipped = SHIPPED_CONFIG) {
  path <- if (!is.null(config)) config else shipped

  if (length(path) != 1L || !is.character(path) || is.na(path) || !nzchar(path)) {
    stop("--config must name a configuration file, and was empty", call. = FALSE)
  }
  if (!file.exists(path) || dir.exists(path)) {
    stop(sprintf("--config must name an existing configuration file, and %s was not found",
                 sQuote(path)), call. = FALSE)
  }
  if (isTRUE(refresh_baseline) && file.exists(shipped)) {
    same_file <- identical(normalizePath(path), normalizePath(shipped))
    same_bytes <- identical(unname(tools::md5sum(path)), unname(tools::md5sum(shipped)))
    if (!same_file && !same_bytes) {
      stop(sprintf(paste("--refresh-baseline rejects --config %s: the tracked evidence",
                         "set describes the shipped configuration (%s) only, so a",
                         "different file must not write it"), sQuote(path), shipped),
           call. = FALSE)
    }
  }
  path
}

# ── Resume checkpoints ───────────────────────────────────────────────────────

#' Path of the entry point Rscript is running
#'
#' @param args The session's full command line (default
#'   `commandArgs(trailingOnly = FALSE)`).
#' @return The script's path, or a zero-length character vector when the session
#'   was not started by `Rscript <file>` or the file cannot be found.
entry_script_path <- function(args = commandArgs(trailingOnly = FALSE)) {
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) return(character(0))
  path <- sub("^--file=", "", file_arg[1L])
  if (file.exists(path)) path else character(0)
}

#' Files whose content decides what an experiment's checkpoint holds
#'
#' @param entry Path of the entry point running the experiment (default the one
#'   Rscript is running).
#' @return The model and analysis sources under `R/`, the entry point and the
#'   package lockfile, those that exist, in a fixed order.
#'
#' @details Every file under `R/` is included rather than a chosen subset, since
#'   a checkpoint holds the result of the model and of the reduction the entry
#'   point applies to it, and a subset chosen by hand would go stale the first
#'   time a module gained a dependency. A console-only edit therefore starts a
#'   sweep afresh, the cheaper of the two ways to be wrong.
checkpoint_sources <- function(entry = entry_script_path()) {
  files <- c(sort(list.files("R", pattern = "\\.R$", full.names = TRUE)), entry,
             "renv.lock")
  files[file.exists(files)]
}

#' Fingerprint of the configuration and code an experiment's checkpoints came from
#'
#' @param config_path Configuration file the experiment runs against.
#' @param sources Files whose content the result also depends on (default
#'   `checkpoint_sources()`).
#' @return Twelve hexadecimal characters, changing whenever the content of the
#'   configuration or of any source changes.
#'
#' @details Content rather than names is hashed, so a configuration whose bytes
#'   equal the shipped file's fingerprints as the shipped file does, the rule
#'   resolve_config_path() applies to a baseline refresh. MD5 (`tools::md5sum`)
#'   keeps this file dependency-free; it detects a change rather than guarding
#'   against a forged collision, which is all a resume needs.
checkpoint_fingerprint <- function(config_path, sources = checkpoint_sources()) {
  files <- c(config_path, sources)
  absent <- files[!file.exists(files)]
  if (length(absent) > 0L) {
    stop(sprintf("checkpoint fingerprint: %s not found",
                 paste(sQuote(absent), collapse = ", ")), call. = FALSE)
  }
  digest_file <- tempfile("fingerprint")
  on.exit(unlink(digest_file), add = TRUE)
  writeLines(c(unname(tools::md5sum(config_path)), basename(sources),
               unname(tools::md5sum(sources))), digest_file)
  substr(unname(tools::md5sum(digest_file)), 1L, 12L)
}

#' Directory a long sweep writes its resume checkpoints to
#'
#' @param experiment Name of the experiment, one subdirectory per sweep.
#' @param refresh_baseline TRUE where the run is writing the tracked evidence set.
#' @param config_path Configuration file the experiment runs against, as
#'   resolve_config_path() returned it.
#' @return The directory path, created if absent.
#'
#' @details Checkpoints sit under the gitignored `outputs/` whatever the run
#'   writes, so a tracked evidence set holds measurements alone. A saved
#'   replication is named by its index, seed and horizon, which say nothing of
#'   the configuration or the model that produced it, so the directory carries
#'   checkpoint_fingerprint() instead: a run against another configuration, or
#'   after any change to the code, starts in an empty directory rather than
#'   resuming a measurement of something else, and a run returning to an earlier
#'   configuration resumes it. Nothing is deleted. A baseline refresh and an
#'   ordinary run keep separate directories, so a refresh never resumes an
#'   exploratory arm.
checkpoint_dir <- function(experiment, refresh_baseline = FALSE, config_path) {
  path <- file.path("outputs", "checkpoints",
                    if (isTRUE(refresh_baseline)) "baseline" else "run", experiment,
                    checkpoint_fingerprint(config_path))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

#' Directory one arm of an experiment saves its finished replications to
#'
#' @param experiment Name of the experiment, as checkpoint_dir() takes it.
#' @param arm Label of the arm, unique within the experiment; it names the
#'   arm's scenario and setting, so two arms never share a replication's file.
#' @param refresh_baseline TRUE when the run is writing the tracked copy.
#' @param config_path Configuration file the experiment runs against.
#' @return The directory, created if absent.
#'
#' @details The per-replication files checkpoint_replication() writes
#'   (R/replication.R) are named by index, seed and horizon alone, so each arm
#'   keeps its own directory under the fingerprinted one checkpoint_dir() gives.
arm_checkpoint_path <- function(experiment, arm, refresh_baseline = FALSE, config_path) {
  path <- file.path(checkpoint_dir(experiment, refresh_baseline, config_path), arm)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

# ── Command-line help ────────────────────────────────────────────────────────

#' The worked invocations an entry point's banner gives
#'
#' @param lines The entry point's source lines.
#' @return The banner's command examples with their comment marker removed: the
#'   first run of indented comment lines that begins with `Rscript`, continuation
#'   lines included.
banner_examples <- function(lines) {
  first <- grep("^#   +Rscript ", lines)[1L]
  if (is.na(first)) return(character(0))
  run <- first
  while (run < length(lines) && grepl("^#   +\\S", lines[run + 1L])) run <- run + 1L
  sub("^# ", "", lines[first:run])
}

#' The title an entry point's banner box gives
#'
#' @param lines The entry point's source lines.
#' @param script Path of the entry point, a box line naming it being left out.
#' @return The box's remaining lines joined into one, a title wrapped across
#'   lines being read as the one sentence it is, or "" where there is no box.
banner_title <- function(lines, script) {
  box <- grep("^## .*##$", lines, value = TRUE)
  box <- trimws(sub("##$", "", sub("^## ", "", box)))
  box <- box[box != script]
  paste(box, collapse = " ")
}

#' Usage, description and examples for an entry point's --help
#'
#' @param script Path of the entry point relative to the repository root, as the
#'   user types it after `Rscript`.
#' @return A list of `usage`, `description` and `epilogue`, the arguments
#'   optparse::OptionParser() takes for them.
#'
#' @details Each entry point's banner is the one home of its worked
#'   invocations, so the help is read from it rather than restated beside the
#'   options, and an example added to the banner reaches `--help` with no
#'   second edit. tests/checks/check_cli_help.R asserts every entry point builds
#'   its parser from this and that every banner yields examples.
cli_help <- function(script) {
  lines <- readLines(script, warn = FALSE)
  examples <- banner_examples(lines)
  epilogue <- c(
    if (length(examples) > 0L) c("Examples:", examples, ""),
    "Run from the repository root. Every run outside the tracked evidence set writes",
    "run_manifest.json beside its outputs. See README.md, Running the Experiments.",
    ""
  )
  list(usage = sprintf("usage: Rscript %s [options]", script),
       description = banner_title(lines, script),
       epilogue = paste(epilogue, collapse = "\n"))
}
