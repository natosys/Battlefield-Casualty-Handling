#!/usr/bin/env Rscript
##############################################################################
## scripts/render_check_table.R                                             ##
## Regenerate the per-check table of docs/Continuous_Integration.md         ##
##############################################################################
#
# Usage:
#   Rscript scripts/render_check_table.R                     # write outputs/Continuous_Integration.md
#   Rscript scripts/render_check_table.R --refresh-baseline  # rewrite docs/Continuous_Integration.md
#
# Why this exists. The per-check record lived in scripts/README.md, which listed
# twenty-six of the fifty-odd checks that exist because nothing tied it to the
# suite and every check added since went unrecorded. The table is now generated
# from the checks themselves: each row's name comes from the file glob the runner
# uses, its tier from the runner's `SLOW_CHECKS`, its measured runtime from
# `scripts/check_runtimes.csv` and its summary from the check's own banner, so a
# check cannot be added without appearing in the guide, and
# `scripts/check_ci_check_table.R` fails when the table is stale.
#
# --refresh-baseline is the only way to write the tracked guide, matching every
# other render script in the project: an ordinary invocation writes under
# outputs/ and cannot disturb it.

#' Marker opening the generated table in the guide
TABLE_START <- "<!-- CHECK TABLE START -->"

#' Marker closing the generated table in the guide
TABLE_END <- "<!-- CHECK TABLE END -->"

#' Directory the checks live in
CHECK_DIR <- "scripts"

#' Pattern the runner uses to discover a check
CHECK_PATTERN <- "^check_.*[.]R$"

#' The runner, whose `SLOW_CHECKS` constant classifies the slow tier
RUNNER_PATH <- file.path("scripts", "run_all_checks.R")

#' Measured runtime per check, in seconds
RUNTIME_PATH <- file.path("scripts", "check_runtimes.csv")

#' The tracked guide the table is written into
GUIDE_PATH <- file.path("docs", "Continuous_Integration.md")

#' Where an ordinary invocation writes its copy
OUTPUT_PATH <- file.path("outputs", "Continuous_Integration.md")

#' The checks the runner classifies as slow
#'
#' @return Character vector of file names named in the runner's `SLOW_CHECKS`.
#'
#' @details Read out of the runner's source rather than sourced from it, the runner
#'   running the suite as soon as it is loaded.
slow_checks <- function() {
  text <- paste(readLines(RUNNER_PATH, warn = FALSE), collapse = " ")
  block <- regmatches(text, regexpr("SLOW_CHECKS <- c\\([^)]*\\)", text))
  if (length(block) == 0L) stop("no SLOW_CHECKS constant in ", RUNNER_PATH, call. = FALSE)
  regmatches(block, gregexpr("check_[A-Za-z0-9_]+[.]R", block))[[1]]
}

#' The summary a check states about itself in its banner
#'
#' @param path Path to the check.
#' @return The banner's title as one line, without the leading "Regression check" label.
#'
#' @details The banner is the run of leading comment lines between rules of hashes. The
#'   line naming the file is dropped and the remaining title lines are joined, so a title
#'   wrapped over several lines reads as one sentence.
check_summary <- function(path) {
  head_lines <- utils::head(readLines(path, warn = FALSE), 14L)
  head_lines <- head_lines[-1L]
  stop_at <- which(!grepl("^#", head_lines))[1]
  if (!is.na(stop_at)) head_lines <- head_lines[seq_len(stop_at - 1L)]
  head_lines <- head_lines[!grepl("^#+$", head_lines)]
  head_lines <- trimws(sub("#+$", "", sub("^#+", "", head_lines)))
  head_lines <- head_lines[nzchar(head_lines) & !grepl("^scripts/check_", head_lines)]
  # Stop at the first line that opens the usage block or a paragraph of prose.
  cut <- which(grepl("^(Usage|Why|Exits|Rscript)", head_lines))[1]
  if (!is.na(cut)) head_lines <- head_lines[seq_len(cut - 1L)]
  title <- paste(head_lines, collapse = " ")
  title <- sub("^Regression check *[:—-] *", "", title)
  gsub("\\|", "/", title)
}

#' Build the per-check table
#'
#' @return The table as a character vector of markdown lines, one row per check.
build_check_table <- function() {
  checks <- sort(list.files(CHECK_DIR, pattern = CHECK_PATTERN))
  runtimes <- utils::read.csv(RUNTIME_PATH, stringsAsFactors = FALSE)
  slow <- slow_checks()
  rows <- vapply(checks, function(f) {
    secs <- runtimes$seconds[runtimes$check == f]
    cost <- if (length(secs) == 1L) sprintf("%d s", as.integer(secs)) else "not yet measured"
    sprintf("| `%s` | %s | %s | %s |", f, if (f %in% slow) "slow" else "fast", cost,
            check_summary(file.path(CHECK_DIR, f)))
  }, character(1))
  c("| Check | Tier | Measured runtime | What it asserts |", "|---|---|---|---|", unname(rows))
}

#' Replace the generated table in the guide's text
#'
#' @param lines The guide as a character vector of lines.
#' @param table The table lines to place between the markers.
#' @return The guide's lines with the table between the markers replaced.
splice_table <- function(lines, table) {
  start <- which(lines == TABLE_START)
  end <- which(lines == TABLE_END)
  if (length(start) != 1L || length(end) != 1L || end < start) {
    stop("the guide must carry exactly one ", TABLE_START, " and one ", TABLE_END,
         call. = FALSE)
  }
  c(lines[seq_len(start)], table, lines[end:length(lines)])
}

if (sys.nframe() == 0L) {
  invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
  refresh <- "--refresh-baseline" %in% commandArgs(trailingOnly = TRUE)
  guide <- readLines(GUIDE_PATH, encoding = "UTF-8", warn = FALSE)
  rendered <- splice_table(guide, build_check_table())
  target <- if (refresh) GUIDE_PATH else OUTPUT_PATH
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  con <- file(target, open = "wb")
  writeBin(charToRaw(enc2utf8(paste0(paste(rendered, collapse = "\n"), "\n"))), con)
  close(con)
  message(sprintf("%s written (%d checks)", target, length(list.files(CHECK_DIR,
                                                                      pattern = CHECK_PATTERN))))
}
