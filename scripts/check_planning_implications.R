#!/usr/bin/env Rscript
##############################################################################
## scripts/check_planning_implications.R                                    ##
## Regression check: the planning paper quotes the results paper faithfully ##
##############################################################################
#
# Usage:
#   Rscript scripts/check_planning_implications.R
#
# Exits 0 when every assertion passes and 1 otherwise, so it can gate a pull
# request.
#
# Why this check exists. The multi-run paper the planning paper replaces
# interleaved measurement with argument, so an argument could outlive the
# number beneath it: advice written at 30 days stood beside a 360-day cliff,
# and a conclusion called an effect demonstrated that the body called
# unresolved. docs/Planning_Implications.md therefore states no measurement of
# its own. Every figure in it is a generated span quoting a cell of a table of
# docs/Results.md, rebuilt from the tracked evidence by
# scripts/render_results_tables.R, and this check asserts four things about
# that arrangement: that re-rendering reproduces the document, so a quoted
# figure cannot differ from the evidence it cites; that no figure is typed
# outside a span and that every paragraph or table row carrying one links the
# section of the results paper it comes from; that the lever table covers
# every lever the results paper reports and labels each; and that no design or
# provenance content has crept back in. It also asserts that the half-widths
# behind the resolution table are the ones the experiments' own scripts sized
# their replication counts against.

invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

source("R/results.R")

#' The document under test
PLANNING_PATH <- file.path("docs", "Planning_Implications.md")

#' The results paper every quoted figure comes from
RESULTS_PATH <- file.path("docs", "Results.md")

#' Heading of the section that carries the lever table
LEVER_HEADING <- "## Every Lever, and What the Evidence Supports"

#' Evidence labels a lever row must carry at least one of
EVIDENCE_LABELS <- c("Measured", "Direction only", "Unresolved", "Untested")

#' Results sections whose levers the lever table must cover
#'
#' @details Each subsection of the forward holding and evacuation levers section,
#'   the national support base section, the mass casualty section and the
#'   comparative scenario section, which carries the surgical team diagnosis.
COVERED_SECTIONS <- c("## Forward Holding and Evacuation Levers",
                      "## National Support Base and Strategic Airlift",
                      "## Mass Casualty Events", "## Comparative Scenario Analysis")

#' Percentages a sentence may state without being a measured figure
#'
#' @details Design labels such as the swept sortie loss and the confidence level,
#'   which describe a setting rather than report a result.
ALLOWED_PERCENTAGES <- c("0%", "5%", "10%", "15%", "20%", "25%", "40%", "50%", "75%", "95%",
                         "100%")

#' Text that marks design or provenance content, which belongs in the methods paper
FORBIDDEN_PATTERNS <- c("Issue #", "Rscript", "refresh-baseline", "scripts/", "data/",
                        "\\.csv", "seed vector", "migrated", "Design\\.\\*\\*")

#' The scripts that sized each resolution row's replication count
HALF_WIDTH_SCRIPTS <- c("hold_window/hold_window_paired.csv" = "scripts/run_hold_window.R",
                        "icu_gate/icu_gate_paired.csv" = "scripts/run_icu_gate.R",
                        "policy/policy_sweep_paired.csv" = "scripts/run_policy_sweep.R",
                        "policy/saturation_sweep_paired.csv" = "scripts/run_saturation_sweep.R")

failures <- character(0)

#' Record a failure
#'
#' @param ... Arguments passed to `sprintf()` to build the message.
#' @return The accumulated failures, invisibly; called for its side effect.
fail <- function(...) {
  assign("failures", c(failures, sprintf(...)), envir = globalenv())
}

#' Print one PASS or FAIL line and record a failure
#'
#' @param ok Whether the assertion held.
#' @param fmt `sprintf()` format string describing the assertion.
#' @param ... Values interpolated into `fmt`.
#' @return The printed line, invisibly; called for its side effect.
report <- function(ok, fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg))
  if (!ok) fail("%s", msg)
}

#' The GitHub anchor of a heading
#'
#' @param heading The heading text without its leading hashes.
#' @return The anchor GitHub generates: lower case, punctuation dropped, spaces hyphenated.
slug <- function(heading) {
  gsub(" ", "-", gsub("[^a-z0-9 -]", "", tolower(heading)), fixed = TRUE)
}

#' The half-width a script sized one response against
#'
#' @param script_path Path to the experiment's script.
#' @param response The response key.
#' @return The half-width as a numeric, or NA where the script does not state one.
script_half_width <- function(script_path, response) {
  text <- paste(readLines(script_path, warn = FALSE), collapse = " ")
  block <- regmatches(text, regexpr("PAIRED_HALF_WIDTHS <- c\\([^)]*\\)", text))
  if (length(block) == 0L) return(NA_real_)
  hit <- regmatches(block, regexpr(sprintf("\\b%s *= *[0-9.]+", response), block))
  if (length(hit) == 0L) return(NA_real_)
  as.numeric(sub("^.*= *", "", hit))
}

# ── 1. The document reproduces from the tracked evidence ────────────────────

cat("-- docs/Planning_Implications.md equals its own re-rendering --\n")

text <- paste(readLines(PLANNING_PATH, encoding = "UTF-8", warn = FALSE), collapse = "\n")
results_text <- paste(readLines(RESULTS_PATH, encoding = "UTF-8", warn = FALSE),
                      collapse = "\n")
report(identical(text, render_results(text)),
       "re-rendering %s from data/ reproduces it exactly", PLANNING_PATH)

spans <- regmatches(text, gregexpr(RESULTS_SPAN_PATTERN, text, perl = TRUE))[[1]]
report(length(spans) >= 30L, "the document quotes its figures as generated spans (found %d)",
       length(spans))
report(!any(grepl("<!-- GEN cell:[^>]*-->(x|\\?)?<!-- /GEN", spans)),
       "no quoted figure is left holding its placeholder")

# ── 2. No figure outside a span, and every span's section is linked ─────────

cat("\n-- every figure is quoted and its section of the results paper is linked --\n")

lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
fence <- cumsum(grepl("^```", lines))
body <- lines[fence %% 2L == 0L & !grepl("^```", lines)]
refs_at <- grep("^## References", body)
if (length(refs_at) == 1L) body <- body[seq_len(refs_at - 1L)]
prose <- gsub(RESULTS_SPAN_PATTERN, "", paste(body, collapse = "\n"), perl = TRUE)
prose <- gsub("`[^`]*`", "", prose)
prose <- gsub("\\([^)]*\\)", "", prose)
prose <- gsub("\\$[^$]*\\$", "", prose)

decimals <- regmatches(prose, gregexpr("[0-9][0-9,]*\\.[0-9]+", prose))[[1]]
report(length(decimals) == 0L, "no decimal figure is typed outside a span (found: %s)",
       paste(utils::head(decimals, 5), collapse = ", "))
pcts <- regmatches(prose, gregexpr("[0-9]+(\\.[0-9]+)?%", prose))[[1]]
stray <- setdiff(unique(pcts), ALLOWED_PERCENTAGES)
report(length(stray) == 0L, "no measured percentage is typed outside a span (found: %s)",
       paste(stray, collapse = ", "))
report(!grepl("—", text), "the document uses no em dash")

blocks <- body
carries_span <- grepl("<!-- GEN cell:", blocks, fixed = TRUE)
# A paragraph is a run of non-blank lines; a table row is its own block.
block_id <- cumsum(!nzchar(trimws(blocks)) | grepl("^\\|", blocks) |
                     c(FALSE, grepl("^\\|", blocks[-length(blocks)])))
unlinked <- character(0)
for (id in unique(block_id[carries_span])) {
  members <- blocks[block_id == id]
  if (!any(grepl("\\(Results\\.md#[a-z0-9-]+\\)", members))) {
    unlinked <- c(unlinked, substr(paste(members, collapse = " "), 1L, 60L))
  }
}
report(length(unlinked) == 0L,
       "every paragraph or row carrying a quoted figure links its results section (%d without: %s)",
       length(unlinked), paste(utils::head(unlinked, 2), collapse = " / "))

linked <- unique(sub("^.*\\(Results\\.md#([a-z0-9-]+)\\).*$", "\\1",
                     regmatches(text, gregexpr("\\(Results\\.md#[a-z0-9-]+\\)", text))[[1]]))
results_lines <- strsplit(results_text, "\n", fixed = TRUE)[[1]]
heads <- sub("^#+ ", "", results_lines[grepl("^#{2,3} ", results_lines)])
missing_anchor <- setdiff(linked, vapply(heads, slug, character(1)))
report(length(missing_anchor) == 0L, "every linked results anchor is a heading (missing: %s)",
       paste(missing_anchor, collapse = ", "))

# ── 3. The lever table covers every lever and labels each ───────────────────

cat("\n-- the lever table covers every lever the results paper reports --\n")

at <- which(lines == LEVER_HEADING)
report(length(at) == 1L, "the document carries one '%s' heading", LEVER_HEADING)
if (length(at) == 1L) {
  after <- lines[(at + 1L):length(lines)]
  end <- which(grepl("^## ", after))[1]
  section <- after[seq_len(end - 1L)]
  rows <- section[grepl("^\\| ", section)]
  rows <- rows[-1]
  report(length(rows) >= 10L, "the lever table has a row per lever (%d rows)", length(rows))
  unlabelled <- rows[!vapply(rows, function(r) {
    last <- trimws(utils::tail(strsplit(sub("^\\|", "", sub("\\|$", "", r)), "\\|")[[1]], 1))
    any(vapply(EVIDENCE_LABELS, function(l) {
      grepl(sprintf("\\*\\*%s\\*\\*", l), last, ignore.case = TRUE)
    }, logical(1)))
  }, logical(1))]
  report(length(unlabelled) == 0L, "every lever row carries an evidence label (%d without)",
         length(unlabelled))

  needed <- character(0)
  for (heading in COVERED_SECTIONS) {
    start <- which(results_lines == heading)
    if (length(start) != 1L) next
    rest <- results_lines[(start + 1L):length(results_lines)]
    stop_at <- which(grepl("^## ", rest))[1]
    inside <- rest[seq_len(if (is.na(stop_at)) length(rest) else stop_at - 1L)]
    subs <- sub("^### ", "", inside[grepl("^### ", inside)])
    needed <- c(needed, slug(if (length(subs)) subs else sub("^## ", "", heading)))
  }
  table_text <- paste(rows, collapse = " ")
  #' Whether the lever table links one results anchor
  #'
  #' @param a The anchor without its hash.
  #' @return TRUE where a row of the table links it.
  linked_in_table <- function(a) grepl(sprintf("(Results.md#%s)", a), table_text, fixed = TRUE)
  absent <- needed[!vapply(needed, linked_in_table, logical(1))]
  report(length(absent) == 0L && length(needed) > 0L,
         "the lever table links every lever section of the results paper (absent: %s)",
         paste(absent, collapse = ", "))
}

# ── 4. The resolution table's half-widths are the experiments' own ──────────

cat("\n-- the resolution table's half-widths are those the experiments sized against --\n")

for (spec in RESOLUTION_ROWS) {
  stated <- script_half_width(HALF_WIDTH_SCRIPTS[[spec[[1]]]], spec[[2]])
  report(!is.na(stated) && abs(stated - spec[[6]]) < 1e-9,
         "'%s' is sized against %s, the script's %s", spec[[7]], format(spec[[6]]),
         format(stated))
}

# ── 5. No design or provenance content remains ──────────────────────────────

cat("\n-- the document holds no design or provenance content --\n")

for (pattern in FORBIDDEN_PATTERNS) {
  report(!any(grepl(pattern, body)), "the document does not contain '%s'", pattern)
}

if (length(failures) > 0L) {
  cat(sprintf("\n%d check(s) FAILED:\n", length(failures)))
  for (f in failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("\nEvery figure in the planning paper agrees with the results paper.\n")
quit(status = 0L)
