#!/usr/bin/env Rscript
##############################################################################
## scripts/check_figure_provenance.R                                        ##
## Regression check — every figure of the results paper has a render-only   ##
## producer that runs from tracked data alone                               ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_figure_provenance.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. Every table of docs/Results.md is a generated span
# rebuilt from the tracked evidence under data/, and a check fails when the
# paper differs. The figures held no such standard: several were written only
# by the script that ran their experiment, so a change to a title or a layout
# cost the whole measurement, and the sweeps the paper reports in a table
# alone had no figure. A figure that cannot be regenerated from tracked data
# can drift from the table beside it and nobody can say which is right.
#
# Each render script declares the images it writes with `# RENDERS: <name>`
# marker lines (a name may be a glob), and the marker is what this check reads.
#
# What it asserts:
#
#   1. Every image docs/Results.md references is declared by a render script.
#   2. Every declared image of scripts/render_sweep_figures.R exists in the
#      tracked images/ directory and is referenced by the paper, so a figure
#      cannot be rendered and then left out of it.
#   3. scripts/render_sweep_figures.R runs from tracked data alone: it writes
#      every image it declares into a scratch directory, with simmer never
#      loaded, and leaves the tracked images/ untouched.
#   4. Each rendered image has the dimensions of its tracked copy, so a layout
#      change reaches the tracked image rather than only the script.

#' The results paper whose figures are checked
RESULTS <- "docs/Results.md"
#' The render script that draws the sweep figures from tracked data
SWEEP_SCRIPT <- "scripts/render_sweep_figures.R"
#' Pattern of a line declaring an image a script renders
MARKER <- "^\\s*# RENDERS: "

state <- new.env(parent = emptyenv())
state$failures <- character(0)

#' Record a failure, deferring the non-zero exit to the end of the run
#'
#' @param ... sprintf() format string and its arguments.
#' @return Invisibly, the accumulated failure vector.
fail <- function(...) state$failures <- c(state$failures, sprintf(...))

#' Print one PASS or FAIL line, recording a failure
#'
#' @param ok TRUE when the assertion held.
#' @param fmt sprintf() format string describing the assertion.
#' @param ... Arguments to `fmt`.
#' @return Invisible NULL.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
  invisible(NULL)
}

#' Image file names a markdown document references
#'
#' @param path Markdown file.
#' @return Sorted unique base names of every `images/*.png` target.
referenced_images <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  hits <- regmatches(text, gregexpr("\\]\\([^)]*images/[^)]*\\.png\\)", text))[[1]]
  sort(unique(basename(sub("\\)$", "", sub("^\\]\\(", "", hits)))))
}

#' Image names a script declares it renders
#'
#' @param path R script.
#' @return Character vector of the names after each `# RENDERS:` marker.
declared_images <- function(path) {
  lines <- readLines(path, warn = FALSE)
  trimws(sub(MARKER, "", grep(MARKER, lines, value = TRUE)))
}

#' Whether a name is covered by any declaration, treating a declaration as a glob
#'
#' @param name Image base name.
#' @param declared Declared names or globs.
#' @return TRUE when some declaration matches.
is_declared <- function(name, declared) {
  any(vapply(declared, function(d) grepl(glob2rx(d), name), logical(1)))
}

#' Pixel dimensions of a PNG, read from its header
#'
#' @param path PNG file.
#' @return Integer vector of width and height.
png_size <- function(path) {
  header <- readBin(path, "raw", 24L)
  place <- 256^(3:0)
  as.integer(c(sum(as.integer(header[17:20]) * place), sum(as.integer(header[21:24]) * place)))
}

cat("Figure provenance check\n\n")

producers <- Sys.glob("scripts/render_*.R")
declared <- unlist(lapply(producers, declared_images))
paper <- referenced_images(RESULTS)

cat("-- every figure of the paper has a render script --\n")
report(length(producers) > 0 && length(declared) > 0,
       "render scripts declare the images they write (%d declarations)", length(declared))
for (img in paper) {
  report(is_declared(img, declared), "%s is declared by a render script", img)
}

cat("\n-- every rendered sweep figure is tracked and in the paper --\n")
sweep_images <- declared_images(SWEEP_SCRIPT)
for (img in sweep_images) {
  report(file.exists(file.path("images", img)), "images/%s is tracked", img)
  report(img %in% paper, "%s is referenced by %s", img, RESULTS)
}

cat("\n-- the sweep renderer runs from tracked data alone --\n")
scratch <- file.path(tempdir(), "figure_provenance")
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
tracked_before <- unname(tools::md5sum(Sys.glob("images/*.png")))
runner_body <- "source('%s'); quit(status = if ('simmer' %%in%% loadedNamespaces()) 3L else 0L)"
runner <- sprintf(runner_body, SWEEP_SCRIPT)
status <- system2(file.path(R.home("bin"), "Rscript"),
                  c("--no-init-file", "-e", shQuote(runner), "--images-dir", shQuote(scratch)),
                  stdout = FALSE, stderr = FALSE)
report(status == 0L, "the renderer exits cleanly with simmer unloaded (status %d)", status)
for (img in sweep_images) {
  out <- file.path(scratch, img)
  report(file.exists(out) && file.size(out) > 0, "%s is rendered into the scratch directory", img)
  tracked <- file.path("images", img)
  if (file.exists(out) && file.exists(tracked)) {
    report(identical(png_size(out), png_size(tracked)),
           "%s has the dimensions of its tracked copy (%s)", img,
           paste(png_size(tracked), collapse = " x "))
  }
}
report(identical(unname(tools::md5sum(Sys.glob("images/*.png"))), tracked_before),
       "the tracked images/ are untouched by an ordinary render")

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All figure provenance checks passed.\n")
quit(status = 0)
