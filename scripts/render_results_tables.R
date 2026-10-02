#!/usr/bin/env Rscript
##############################################################################
## scripts/render_results_tables.R                                          ##
## Regenerate the tables and quoted figures of docs/Results.md              ##
##############################################################################
#
# Usage:
#   Rscript scripts/render_results_tables.R                     # render to outputs/Results.md
#   Rscript scripts/render_results_tables.R --refresh-baseline  # rewrite docs/Results.md
#
# Why this exists. docs/Results.md reports measurements and nothing else, so
# every number in it has to come from a tracked evidence set rather than from
# a sentence somebody typed, and a sentence typed once drifts the first time
# an evidence set is re-run. Each table, and each figure quoted in prose, is
# therefore a generated span between `<!-- GEN name -->` markers, and this
# script replaces the content of every span with what R/results.R builds from
# the tracked data. Nothing outside a span is touched.
#
# --refresh-baseline is the only way to write the tracked document, matching
# every other render script in the project: an ordinary invocation writes under
# outputs/ and cannot disturb it.

# The document carries non-ASCII punctuation, and under a C locale R would
# write it as an escape sequence; the locale is set before anything is read.
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

source("R/results.R")

args <- commandArgs(trailingOnly = TRUE)

#' Whether this invocation may write the tracked document
REFRESH <- "--refresh-baseline" %in% args

#' The tracked document whose spans are regenerated
RESULTS_PATH <- file.path("docs", "Results.md")

#' Where an ordinary invocation writes its copy
OUTPUT_PATH <- file.path("outputs", "Results.md")

text <- paste(readLines(RESULTS_PATH, encoding = "UTF-8", warn = FALSE), collapse = "\n")
rendered <- render_results(text)

target <- if (REFRESH) RESULTS_PATH else OUTPUT_PATH
dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
con <- file(target, open = "wb")
writeBin(charToRaw(enc2utf8(paste0(rendered, "\n"))), con)
close(con)
message(sprintf("%s written (%d bytes)", target, file.size(target)))
