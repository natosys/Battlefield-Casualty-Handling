##############################################
## R/run_manifest.R                         ##
## The record a run leaves of what it ran   ##
##############################################
#
# A result that does not carry its own configuration, seed and commit cannot be
# cited or repeated from its own artifacts. Every command-line entry point
# writes a manifest beside its outputs: the configuration file's SHA-256 hash, a
# byte copy of that file and of its resolved form under the scenario in force,
# the seed, the replication count, the horizon, the command that was run, the git
# commit, and the R and package versions.
#
# The tracked evidence under `data/` and `images/` is the exception. A
# `--refresh-baseline` run is held to the shipped configuration and the
# documented protocol (R/cli.R), and a manifest carrying the commit would change
# on every commit and so break the byte-for-byte reproduction
# tests/checks/check_baseline_reproduction.R asserts; those sets are described by
# docs/Methods.md instead.
#
# Depends on R/scenario.R (resolve_scenario) and R/environment.R (read_config),
# and on the digest and jsonlite packages; source those before this file.

#' Packages whose versions the manifest records
MANIFEST_PACKAGES <- c("simmer", "jsonlite", "dplyr", "digest", "optparse")

#' SHA-256 hash of a file's bytes
#'
#' @param path File to hash.
#' @return The hash as a lower-case hex string.
config_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

#' Git commit the working tree is at
#'
#' @return The 40-character commit hash, or NA where git or the repository is
#'   unavailable.
git_commit_hash <- function() {
  out <- tryCatch(suppressWarnings(system2("git", c("rev-parse", "HEAD"), stdout = TRUE,
                                           stderr = FALSE)), error = function(e) NA_character_)
  if (length(out) != 1L || !grepl("^[0-9a-f]{40}$", out)) return(NA_character_)
  out
}

#' Whether the working tree differs from the commit it is at
#'
#' @return TRUE, FALSE, or NA where git is unavailable.
git_tree_dirty <- function() {
  out <- tryCatch(suppressWarnings(system2("git", c("status", "--porcelain",
                                                    "--untracked-files=no"),
                                           stdout = TRUE, stderr = FALSE)),
                  error = function(e) NULL)
  if (is.null(out)) return(NA)
  length(out) > 0L
}

#' The command that started the running script
#'
#' @return One string, `Rscript <script> <arguments>`, or NA when the session is
#'   interactive.
invoked_command <- function() {
  all_args <- commandArgs(trailingOnly = FALSE)
  script <- sub("^--file=", "", grep("^--file=", all_args, value = TRUE))
  if (length(script) == 0L) return(NA_character_)
  paste(c("Rscript", script[1], commandArgs(trailingOnly = TRUE)), collapse = " ")
}

#' Assemble the manifest of one run
#'
#' @param config_path Configuration file the run read.
#' @param options Parsed command-line options of the run, a named list.
#' @param command The command that was run, or NA.
#' @return A named list ready for `jsonlite::toJSON()`.
#'
#' @details The seed, scenario, replication count and horizon are lifted to the
#'   top level where the entry point has them, under the names the entry points
#'   use: `seed`, `scenario`, `iterations` and `days`. The full option list is
#'   kept as well, so an option no entry point shares is not lost.
build_run_manifest <- function(config_path, options = list(), command = NA_character_) {
  #' An option's value, or NA where the entry point has no such option
  #'
  #' @param name Option name.
  #' @return The value or NA.
  pick <- function(name) if (!is.null(options[[name]])) options[[name]] else NA
  pkgs <- vapply(MANIFEST_PACKAGES, function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  }, character(1))
  list(
    created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    command = command,
    config_file = config_path,
    config_sha256 = config_sha256(config_path),
    scenario = if (is.null(options$scenario)) "default" else options$scenario,
    seed = pick("seed"),
    iterations = pick("iterations"),
    days = pick("days"),
    git_commit = git_commit_hash(),
    git_tree_modified = git_tree_dirty(),
    r_version = R.version.string,
    packages = as.list(pkgs),
    options = options[!vapply(options, is.null, logical(1))]
  )
}

#' Write a run's manifest and the configuration it ran against
#'
#' @param output_dir Directory the run writes its outputs to.
#' @param config_path Configuration file the run read.
#' @param options Parsed command-line options of the run, a named list.
#' @param command The command that was run (default `invoked_command()`).
#' @return The path of the manifest, invisibly.
#'
#' @details Writes three files: `run_manifest.json`, `config.json`, a byte copy of
#'   the file that was read, and `resolved_config.json`, that file with the
#'   scenario in force applied. The copy is what lets the run be repeated once
#'   the original path no longer exists, and the hash lets a reader confirm the
#'   copy is the file that was read.
write_run_manifest <- function(output_dir, config_path, options = list(),
                               command = invoked_command()) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- build_run_manifest(config_path, options, command)

  file.copy(config_path, file.path(output_dir, "config.json"), overwrite = TRUE)
  resolved <- resolve_scenario(read_config(config_path), manifest$scenario)
  jsonlite::write_json(resolved, file.path(output_dir, "resolved_config.json"),
                       pretty = TRUE, auto_unbox = TRUE, null = "null")

  path <- file.path(output_dir, "run_manifest.json")
  jsonlite::write_json(manifest, path, pretty = TRUE, auto_unbox = TRUE, na = "null",
                       null = "null")
  invisible(path)
}
