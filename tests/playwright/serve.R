#!/usr/bin/env Rscript
##############################################################################
## tests/playwright/serve.R                                                 ##
## Launcher for the console under the browser test suite                    ##
##############################################################################
#
# Usage:
#   Rscript tests/playwright/serve.R [port]
#
# Started by playwright.config.js's webServer, not by hand. It exists as a
# script rather than as an inline `-e` expression because the console's Quick
# Run dispatches its work to a `future` multisession worker, and a worker
# started from a session whose own standard input is the expression pipe
# `R -e` leaves behind fails on the first write back to its parent, surfacing
# in the app as "Run failed: ignoring SIGPIPE signal". Run as a script under
# Rscript, the same run completes.
#
# The port defaults to the one playwright.config.js uses, so running this by
# hand and pointing the suite at it with BCH_APP_PORT is a working way to
# debug a spec against an app that is already warm.

DEFAULT_PORT <- 7654

args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1L) suppressWarnings(as.integer(args[[1]])) else DEFAULT_PORT
if (is.na(port) || port < 1L || port > 65535L) {
  stop(sprintf("serve.R: port must be an integer between 1 and 65535, found %s",
               paste(args, collapse = " ")), call. = FALSE)
}

shiny::runApp("app.R", port = port, host = "127.0.0.1", launch.browser = FALSE)
