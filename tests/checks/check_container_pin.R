#!/usr/bin/env Rscript
##############################################################################
## tests/checks/check_container_pin.R                                       ##
## Regression check — the Dev Container and CI run the same pinned image    ##
##############################################################################
#
# Usage:
#   Rscript tests/checks/check_container_pin.R
#
# Exits 0 when every check passes, 1 otherwise.
#
# Why this check exists. The seed-42 reproduction and the figure provenance
# check compare their outputs with the tracked copies byte for byte, and a
# rendered figure's bytes depend on the fonts and system libraries of the image
# it is drawn in. CI runs in `rocker/rstudio:4.4.2` pinned by digest; a Dev
# Container built from the tag alone would follow the tag when it moves, and a
# contributor's figure check could then fail where CI's passes, or pass where
# CI's fails. Both definitions name the same digest, and this check holds them
# together, so a change to one is a change to both or a failure here.
#
# What it asserts:
#
#   1. The Dockerfile's base image is pinned by a sha256 digest.
#   2. The workflow's CONTAINER_IMAGE is pinned by a sha256 digest.
#   3. Every job image in the workflow is that CONTAINER_IMAGE.
#   4. The Dockerfile's base image is that CONTAINER_IMAGE.

#' The Dev Container definition
DOCKERFILE <- file.path(".devcontainer", "Dockerfile")

#' The workflow CI runs
WORKFLOW <- file.path(".github", "workflows", "checks.yml")

#' Pattern of an image reference pinned by digest
PINNED <- "^[^ ]+@sha256:[0-9a-f]{64}$"

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
#'
#' @details Anything other than TRUE is a failure, NA included.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  passed <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (passed) "PASS" else "FAIL", msg))
  if (!passed) fail("%s", msg)
  invisible(NULL)
}

#' The values a set of lines assigns to one key, trimmed
#'
#' @param lines Lines of a file.
#' @param pattern Regular expression matching the key and its separator.
#' @return Character vector of the values found, in file order.
values_of <- function(lines, pattern) {
  hits <- lines[grepl(pattern, lines)]
  trimws(sub(pattern, "", hits))
}

dockerfile <- readLines(DOCKERFILE, warn = FALSE)
workflow <- readLines(WORKFLOW, warn = FALSE)

base <- values_of(dockerfile, "^FROM[[:space:]]+")
report(length(base) == 1L && grepl(PINNED, base),
       "the Dockerfile has one base image, pinned by digest (%s)",
       paste(base, collapse = ", "))

pinned <- values_of(workflow, "^[[:space:]]*CONTAINER_IMAGE:[[:space:]]*")
report(length(pinned) == 1L && grepl(PINNED, pinned),
       "the workflow sets one CONTAINER_IMAGE, pinned by digest (%s)",
       paste(pinned, collapse = ", "))

images <- values_of(workflow, "^[[:space:]]*image:[[:space:]]*")
report(length(images) > 0L && length(pinned) == 1L && all(images == pinned),
       "every job image in the workflow is CONTAINER_IMAGE (%d jobs)", length(images))

report(length(base) == 1L && length(pinned) == 1L && identical(base, pinned),
       "the Dev Container builds from the image CI runs in")

if (length(state$failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(state$failures)))
  for (f in state$failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nThe Dev Container and CI run the same pinned image.\n")
quit(status = 0L)
