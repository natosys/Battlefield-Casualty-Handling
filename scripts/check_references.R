#!/usr/bin/env Rscript
##############################################
## scripts/check_references.R               ##
## Reference list structural checks         ##
##############################################
#
# Regression check for the reference lists of the three academic documents.
#
# The checks below are structural, not bibliographic: they assert the
# properties a reader relies on when following a citation, and they are the
# properties a hand renumber silently breaks. Whether a URL is open access is
# a judgement no script can make, so it stays a manual step at the point a
# reference is added; what the script can guarantee is that the same source is
# not listed twice, that every entry is reachable from the text and every
# citation from the list, that the numbering runs in order of first appearance,
# and that each entry carries the URL and retrieval date the project's
# Reference List Rules require.

#' Reference entries of a document, in list order
#'
#' @param lines Character vector of the document's lines
#' @return Data frame of number, text and line, one row per entry, empty when
#'   the document carries no reference block
reference_entries <- function(lines) {
  start <- which(lines == "<!-- REFERENCES START -->")
  end <- which(lines == "<!-- REFERENCES END -->")
  if (length(start) != 1 || length(end) != 1) {
    return(data.frame(number = integer(0), text = character(0), line = integer(0)))
  }

  block <- seq(start + 1, end - 1)
  is_entry <- grepl("^\\[[0-9]+\\] ", lines[block])
  entries <- block[is_entry]

  data.frame(
    number = as.integer(sub("^\\[([0-9]+)\\].*$", "\\1", lines[entries])),
    text = sub("^\\[[0-9]+\\] ", "", lines[entries]),
    line = entries,
    stringsAsFactors = FALSE
  )
}

#' Citation numbers appearing in a document's body, in order of appearance
#'
#' @param lines Character vector of the document's lines
#' @return Integer vector of the numbers cited before the reference block,
#'   with repeats retained so the caller can take first appearances
body_citations <- function(lines) {
  start <- which(lines == "<!-- REFERENCES START -->")
  body <- if (length(start) == 1) lines[seq_len(start - 1)] else lines
  matches <- regmatches(body, gregexpr("\\[\\[[0-9]+\\]\\]", body))
  as.integer(gsub("[^0-9]", "", unlist(matches)))
}

#' First URL an entry carries
#'
#' @param text Character vector of entry text
#' @return Character vector of URLs, NA where the entry carries none. A URL is
#'   taken to end at the first whitespace, with trailing sentence punctuation
#'   and a closing bracket removed, so a URL wrapped in markdown link syntax
#'   compares equal to the same URL written bare.
entry_url <- function(text) {
  # perl = TRUE is load bearing. R's default TRE engine reads the backslash
  # inside a bracket expression as a literal, so "[^ )\\]]" closes the class at
  # the first "]" and the trailing "]" becomes a literal the URL would have to
  # be followed by, which no entry is: the pattern then matches nothing and
  # every entry reads as carrying no URL.
  pos <- regexpr("https?://[^ )\\]]+", text, perl = TRUE)
  urls <- rep(NA_character_, length(text))
  urls[pos > 0] <- regmatches(text, pos)
  sub("[.,;]+$", "", urls)
}

#' Check one document's reference list
#'
#' @param file_path Path to the markdown document
#' @return Number of violations found, zero when the list is sound
check_references <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  entries <- reference_entries(lines)
  if (nrow(entries) == 0) {
    cat(sprintf("  %s: no reference block, skipped\n", file_path))
    return(0)
  }

  violations <- 0
  cited <- body_citations(lines)
  first_appearance <- unique(cited)

  # Every [[n]] in the text resolves to an entry.
  dangling <- setdiff(first_appearance, entries$number)
  if (length(dangling) > 0) {
    violations <- violations + length(dangling)
    cat(sprintf("  %s: citation [[%d]] matches no reference entry\n",
                file_path, dangling))
  }

  # Every entry is reached from the text.
  uncited <- setdiff(entries$number, first_appearance)
  if (length(uncited) > 0) {
    violations <- violations + length(uncited)
    cat(sprintf("  %s: entry [%d] is never cited\n", file_path, uncited))
  }

  # The list is numbered 1..N without gaps or repeats.
  expected <- seq_len(nrow(entries))
  if (!identical(entries$number, expected)) {
    violations <- violations + 1
    cat(sprintf("  %s: entries are not numbered 1 to %d in order\n",
                file_path, nrow(entries)))
  }

  # Numbering is monotonic in order of first appearance in the text.
  resolvable <- first_appearance[first_appearance %in% entries$number]
  if (!identical(resolvable, sort(resolvable))) {
    out_of_order <- resolvable[c(FALSE, diff(resolvable) < 0)]
    violations <- violations + length(out_of_order)
    cat(sprintf("  %s: entry [%d] first appears after a higher-numbered one\n",
                file_path, out_of_order))
  }

  urls <- entry_url(entries$text)

  # Every entry carries a URL.
  missing_url <- entries$number[is.na(urls)]
  if (length(missing_url) > 0) {
    violations <- violations + length(missing_url)
    cat(sprintf("  %s: entry [%d] carries no URL\n", file_path, missing_url))
  }

  # No two entries name the same source.
  duplicated_url <- unique(urls[!is.na(urls) & duplicated(urls)])
  if (length(duplicated_url) > 0) {
    violations <- violations + length(duplicated_url)
    for (u in duplicated_url) {
      shared <- entries$number[!is.na(urls) & urls == u]
      cat(sprintf("  %s: entries [%s] share the URL %s\n",
                  file_path, paste(shared, collapse = "], ["), u))
    }
  }

  # Every entry carries a retrieval date.
  retrieved <- grepl("[Rr]etrieved [0-9]{1,2} [A-Za-z]+,? [0-9]{2,4}", entries$text) |
    grepl("[Rr]etrieved [A-Za-z]+ [0-9]{1,2},? [0-9]{4}", entries$text)
  missing_date <- entries$number[!retrieved]
  if (length(missing_date) > 0) {
    violations <- violations + length(missing_date)
    cat(sprintf("  %s: entry [%d] carries no retrieval date\n",
                file_path, missing_date))
  }

  if (violations == 0) {
    cat(sprintf("  %s: %d entries, all cited, numbered and sourced\n",
                file_path, nrow(entries)))
  }

  violations
}

reference_docs <- c("README.md",
                    "docs/Single_Run_Analysis.md",
                    "docs/Multi_Run_Analysis.md")

violations <- sum(vapply(reference_docs, check_references, numeric(1)))

if (violations > 0) {
  cat(sprintf("⚠️ %d reference list violation(s) — repair them and re-run.\n", violations))
  quit(status = 1)
} else {
  cat("✓ Every reference list is complete, uniquely sourced and correctly numbered.\n")
}
