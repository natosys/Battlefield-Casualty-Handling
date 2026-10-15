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
#   5. No library function defaults its image directory to the tracked
#      images/: every `images_dir` default under R/ resolves under outputs/, so
#      calling one from a console, as the README's examples do, cannot replace a
#      published figure with an exploratory one. Only the entry points'
#      --refresh-baseline paths name images/, and they do so explicitly.
#   6. Every tracked file in images/ has a producer: a `# RENDERS:` marker, the
#      analysis pipeline that writes a seed-42 baseline plot (its file name is
#      a literal in R/analysis.R), the warm-up diagnostic, or a hand-authored
#      source the README documents. Every one of them also has a home: a
#      reference from one of the four academic documents, or a place in the
#      README's table of seed-42 baseline plots. A figure no document cites and
#      no producer names is a figure that can silently describe a superseded
#      model. The test is fault-injected with an image nothing accounts for.

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

cat("\n-- no library function defaults to the tracked images/ --\n")

#' Default `images_dir` of every function in a source file that has one
#'
#' @param path Source file to parse.
#' @return Named character vector of each such function's default, evaluated
#'   with `output_dir` bound to "outputs" as it is at the default call.
images_dir_defaults <- function(path) {
  exprs <- parse(path, keep.source = FALSE)
  found <- list()
  for (e in exprs) {
    is_def <- is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) &&
      identical(e[[3]][[1]], as.name("function"))
    if (!is_def) next
    fmls <- e[[3]][[2]]
    if (!"images_dir" %in% names(fmls)) next
    default_text <- paste(deparse(fmls[["images_dir"]]), collapse = "")
    if (!nzchar(default_text)) next
    resolved <- eval(parse(text = default_text), list(output_dir = "outputs"))
    found[[as.character(e[[2]])]] <- resolved
  }
  unlist(found)
}

library_files <- c("R/analysis.R", "R/scenario_runner.R", "R/warmup.R", "R/sensitivity.R")
defaults <- unlist(lapply(library_files, images_dir_defaults))
tracked_dir <- normalizePath("images", mustWork = FALSE)
resolves_to_tracked <- vapply(defaults, function(d) {
  identical(normalizePath(d, mustWork = FALSE), tracked_dir)
}, logical(1))
expected <- c("plot_scenario_comparison", "compare_scenarios", "plot_welch",
              "run_welch_analysis", "plot_long_horizon_welch",
              "plot_transport_capacity_margin_by_fleet_size",
              "plot_r2b_forward_hold_frontier", "plot_r2b_hold_threshold_sweep")
report(all(expected %in% names(defaults)),
       "the %d functions the README calls from a console are among those inspected",
       length(expected))
report(!any(resolves_to_tracked),
       "no image directory default resolves to the tracked images/ (%d inspected)",
       length(defaults))
for (nm in names(defaults)[resolves_to_tracked]) {
  report(FALSE, "%s defaults its image directory to the tracked images/", nm)
}

cat("\n-- every tracked image has a producer and a home --\n")

#' The four academic documents whose references give an image a home
DOCUMENTS <- c("README.md", RESULTS, "docs/Planning_Implications.md", "docs/Methods.md")
#' Seed-42 single-run plots written by `run.R --refresh-baseline`, each named as a literal in the
#' analysis pipeline and listed by name in the README's table of baseline plots
BASELINE_PLOTS <- c("casualty_summary.png", "r1_queues.png", "r2b_bed_queues.png",
                    "r2b_gantt.png", "r2b_handling.png", "r2b_hold_occupancy.png",
                    "r2b_ot_bypass_reason.png", "r2eheavy_gantt.png",
                    "r2eheavy_surgeries.png", "r2eheavy_bed_queue_3_teams.png",
                    "r2e_icu_gating_impact.png", "ame_backlog.png",
                    "ame_sortie_timeline.png", "force_regeneration.png",
                    "waiting_time.png", "role4_census.png", "transport_capacity_margin.png")
#' Hand-authored images with no code, each described in the README beside its caption
HAND_AUTHORED <- c("tactical_diagram.png", "tactical_diagram.kmz", "tactical_diagram.milxlyz")
#' Images the warm-up entry point writes, named as literals in the module it calls
WARMUP_IMAGES <- "welch_plot_icu_queue.png"

#' Tracked images that lack a producer or a home
#'
#' @param tracked Base names of every tracked file in images/.
#' @param referenced Base names the four documents reference.
#' @param declared Names and globs the render scripts declare.
#' @param named_in Named list of source texts keyed `analysis`, `warmup` and `readme`,
#'   each a single string.
#' @return A list of `no_producer` and `no_home`, each a character vector.
provenance_gaps <- function(tracked, referenced, declared, named_in) {
  #' Whether a file name appears literally in a source text
  #'
  #' @param img Image base name.
  #' @param text Source text to search.
  #' @return TRUE when the name occurs in the text.
  has_name <- function(img, text) grepl(img, text, fixed = TRUE)
  producer <- vapply(tracked, function(img) {
    is_declared(img, declared) ||
      (img %in% BASELINE_PLOTS && has_name(img, named_in$analysis)) ||
      (img %in% WARMUP_IMAGES && has_name(img, named_in$warmup)) ||
      (img %in% HAND_AUTHORED && has_name(img, named_in$readme))
  }, logical(1))
  home <- vapply(tracked, function(img) {
    listed <- img %in% c(BASELINE_PLOTS, HAND_AUTHORED) && has_name(img, named_in$readme)
    img %in% referenced || listed
  }, logical(1))
  list(no_producer = tracked[!producer], no_home = tracked[!home])
}

#' Whole text of a file as one string
#'
#' @param path File to read.
#' @return The file's lines joined by newlines.
read_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
named_in <- list(analysis = read_text("R/analysis.R"), warmup = read_text("R/warmup.R"),
                 readme = read_text("README.md"))
tracked_images <- list.files("images", pattern = "\\.(png|kmz|milxlyz)$")
cited <- sort(unique(unlist(lapply(DOCUMENTS, referenced_images))))
gaps <- provenance_gaps(tracked_images, cited, declared, named_in)
report(length(tracked_images) > 0, "%d tracked images are inspected", length(tracked_images))
for (img in gaps$no_producer) report(FALSE, "images/%s has no producer", img)
for (img in gaps$no_home) {
  report(FALSE, "images/%s is neither cited nor listed as baseline evidence", img)
}
report(!length(gaps$no_producer) && !length(gaps$no_home),
       "every tracked image has a producer and a home")
report(all(cited %in% tracked_images), "every image the four documents reference is tracked")

cat("\n-- the test fails on an image nothing accounts for (fault injection) --\n")
injected <- provenance_gaps(c(tracked_images, "orphan_figure.png"), cited, declared, named_in)
report(identical(injected$no_producer, "orphan_figure.png") &&
         identical(injected$no_home, "orphan_figure.png"),
       "an undeclared, uncited image is reported as both producer-less and homeless")
undeclared <- provenance_gaps(tracked_images, cited, setdiff(declared, "policy_sweep.png"),
                              named_in)
report("policy_sweep.png" %in% undeclared$no_producer,
       "removing a render declaration leaves its image without a producer")
unlisted <- provenance_gaps(tracked_images, setdiff(cited, "icu_gate.png"), declared, named_in)
report("icu_gate.png" %in% unlisted$no_home,
       "an image no document cites and the baseline table does not list has no home")

cat("\n")
if (length(state$failures)) {
  cat(sprintf("%d check(s) failed:\n", length(state$failures)))
  for (f in state$failures) cat(" - ", f, "\n", sep = "")
  quit(status = 1)
}

cat("All figure provenance checks passed.\n")
quit(status = 0)
