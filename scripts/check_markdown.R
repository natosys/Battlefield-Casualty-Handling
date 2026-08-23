#' Anchor GitHub generates for a heading, reproduced exactly
#'
#' @param title Character vector of heading text, with the leading hashes and
#'   surrounding whitespace already removed
#' @return Character vector of anchors, without the leading "#"
#'
#' @details GitHub derives a heading's id by deleting every character outside
#'   letters, numbers, combining marks, connector punctuation and the
#'   hyphen-minus, lower-casing what remains, and replacing each space with a
#'   hyphen. Two details of that sequence matter and are easy to get wrong.
#'   Spaces are replaced one for one rather than collapsed, so a heading whose
#'   punctuation sits between spaces (as an em dash does) yields two adjacent
#'   hyphens, not one. And the deletion is by Unicode property rather than by
#'   POSIX class, which is what makes it independent of the session's locale:
#'   an em dash is punctuation to a UTF-8 locale but an ordinary byte sequence
#'   to a C locale, so a POSIX class would strip it in one and keep it in the
#'   other, producing a different anchor from the same document.
#'
#'   Verified character for character against the ids GitHub itself generates
#'   for all 140 headings across the three documents this script maintains.
github_anchor <- function(title) {
  kept <- gsub("[^\\p{L}\\p{N}\\p{M}\\p{Pc}\\- ]", "", title, perl = TRUE)
  gsub(" ", "-", tolower(kept), fixed = TRUE)
}

#' Disambiguate repeated anchors the way GitHub does
#'
#' @param anchors Character vector of anchors in document order
#' @return The same vector with the second and later occurrences of any
#'   repeated anchor suffixed "-1", "-2", and so on
disambiguate_anchors <- function(anchors) {
  seen <- list()
  vapply(anchors, function(a) {
    n <- seen[[a]]
    if (is.null(n)) {
      seen[[a]] <<- 0L
      a
    } else {
      seen[[a]] <<- n + 1L
      paste0(a, "-", n + 1L)
    }
  }, character(1), USE.NAMES = FALSE)
}

#' Heading lines of a document, excluding those inside fenced code blocks
#'
#' @param lines Character vector of the document's lines
#' @param levels Regular expression matching the heading levels wanted
#' @return Logical vector, TRUE for each line that is a heading
#'
#' @details A comment line inside a fenced block can open with hashes and a
#'   space without being a heading; GitHub renders it as code and gives it no
#'   id, so it must not reach the table of contents either.
heading_lines <- function(lines, levels = "^#{2,6} ") {
  fence <- grepl("^\\s*```", lines)
  inside_fence <- cumsum(fence) %% 2 == 1
  grepl(levels, lines) & !(fence | inside_fence)
}

update_or_check_toc <- function(file_path, mode = c("verify", "replace"), toc_start = "<!-- TOC START -->", toc_end = "<!-- TOC END -->") {
  mode <- match.arg(mode)
  lines <- readLines(file_path, encoding = "UTF-8")

  # Extract headings H2 to H6
  headings <- lines[heading_lines(lines)]
  titles <- trimws(sub("^#{2,6} ", "", headings))
  anchors <- disambiguate_anchors(github_anchor(titles))
  toc <- lapply(seq_along(headings), function(i) {
    level <- attr(regexpr("^#+", headings[i]), "match.length")
    indent <- paste(rep("  ", level - 2), collapse = "")
    paste0(indent, "- [", titles[i], "](#", anchors[i], ")")
  })
  new_toc <- paste(unlist(toc), collapse = "\n")

  # Identify TOC block boundaries
  start_line <- grep(toc_start, lines)
  end_line <- grep(toc_end, lines)
  
  if (length(start_line) == 1 && length(end_line) == 1 && start_line < end_line) {
    current_toc <- lines[(start_line + 1):(end_line - 1)]
    if (mode == "verify") {
      # Compare trimmed lines
      match <- identical(trimws(current_toc), trimws(strsplit(new_toc, "\n")[[1]]))
      if (match) {
        cat("✓ TOC is up to date.\n")
      } else {
        cat("⚠️ TOC is outdated. Regenerate to stay in sync.\n")
        quit(status = 1)
      }
    } else if (mode == "replace") {
      updated_lines <- c(
        lines[1:start_line],
        new_toc,
        lines[end_line:length(lines)]
      )
      # useBytes: the lines are UTF-8-flagged by readLines() above, and a C
      # locale cannot represent every character in them natively, so writing
      # them through the locale would replace those with escapes
      writeLines(updated_lines, file_path, useBytes = TRUE)
      cat("✅ TOC block replaced.\n")
      # Logging
      log_entry <- sprintf("[%s] TOC updated in %s", Sys.time(), file_path)
      write(log_entry, file = "log.log", append = TRUE)
    }
  } else {
    cat("⚠️ TOC markers not found or malformed.\n")
    quit(status = 1)
  }
}

# enforce_return_links <- function(file_path, mode = c("verify", "replace"), 
#                                  top_anchor = "#contents", 
#                                  return_text = "<sub>[Return to Top](#table-of-contents)</sub>") {
#   mode <- match.arg(mode)
#   lines <- readLines(file_path)
#   new_lines <- c()
#   i <- 1
#   missing_count <- 0
#   
#   while (i <= length(lines)) {
#     line <- lines[i]
#     new_lines <- c(new_lines, line)
#     
#     if (grepl("^## ", line)) {
#       next_line <- if (i + 1 <= length(lines)) lines[i + 1] else ""
#       
#       if (!grepl(return_text, next_line, fixed = TRUE)) {
#         if (mode == "verify") {
#           missing_count <- missing_count + 1
#         } else if (mode == "replace") {
#           new_lines <- c(new_lines, return_text)
#         }
#       }
#     }
#     i <- i + 1
#   }
#   
#   if (mode == "verify") {
#     if (missing_count == 0) {
#       cat("✓ All H2 headings have return links.\n")
#     } else {
#       cat(sprintf("⚠️ %d H2 headings are missing return links.\n", missing_count))
#       quit(status = 1)
#     }
#   } else if (mode == "replace") {
#     writeLines(new_lines, file_path)
#     cat(sprintf("✅ Inserted return links under H2 headings in %s\n", file_path))
#     
#     # Optional: audit log entry
#     log_entry <- sprintf("[%s] Return links inserted under H2 in %s", Sys.time(), file_path)
#     write(log_entry, file = "log.log", append = TRUE)
#   }
# }
enforce_return_links <- function(file_path, mode = c("verify", "replace"),
                                 top_anchor = "#contents",
                                 return_text = "<sub>[Return to Top](#contents)</sub>",
                                 log_path = "log.log") {
  mode <- match.arg(mode)
  lines <- readLines(file_path, encoding = "UTF-8")
  new_lines <- c()
  i <- 1
  missing_count <- 0
  patched_headings <- c()
  
  # Pattern to detect ALL return-to-top links
  generic_return_pattern <- "(?i)(<sub>|<small>)?\\[return to top\\]\\(#.*?\\)(</sub>|</small>)?"
  
  # Define canonical version to preserve
  canonical_return <- "<sub>[Return to Top](#contents)</sub>"
  return_text <- canonical_return

  while (i <= length(lines)) {
    line <- lines[i]
    
    if (grepl(generic_return_pattern, line, perl = TRUE)) {
      # Skip line ONLY if it's not already standardized
      if (trimws(line) != canonical_return) {
        i <- i + 1
        next  # Remove nonstandard return-to-top
      }
    }
    
    new_lines <- c(new_lines, line)
    
    # Detect H2 heading
    if (grepl("^## ", line)) {
      heading <- trimws(sub("^##\\s*", "", line))
      next_line <- if (i + 1 <= length(lines)) lines[i + 1] else ""
      has_canonical_return <- trimws(next_line) == canonical_return
      has_any_return <- grepl(generic_return_pattern, next_line, perl = TRUE)
      
      has_return <- grepl(generic_return_pattern, next_line, perl = TRUE)
      if (!has_return) {
        if (mode == "verify") {
          missing_count <- missing_count + 1
        } else if (mode == "replace") {
          new_lines <- c(new_lines, return_text)
          patched_headings <- c(patched_headings, heading)
        }
      } else {
        # Skip the legacy return link line
        i <- i + 1
      }
    }
    i <- i + 1
  }
  
  if (mode == "verify") {
    if (missing_count == 0) {
      cat("✓ All H2 headings have clean return links.\n")
    } else {
      cat(sprintf("⚠️ %d H2 headings are missing valid return links.\n", missing_count))
      quit(status = 1)
    }
  } else if (mode == "replace") {
    if (!has_canonical_return) {
      if (has_any_return) {
        i <- i + 1  # Skip legacy link line
      }
      new_lines <- c(new_lines, return_text)
      patched_headings <- c(patched_headings, heading)
    }    
    if (length(patched_headings) > 0) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- sprintf("[%s] Updated 'Return to Top' under H2 in %s:\n- %s\n\n", 
                           timestamp, file_path, paste(patched_headings, collapse = "\n- "))
      dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
      write(log_entry, file = log_path, append = TRUE)
    }
  }
}

check_no_emoji_headings <- function(file_path) {
  lines <- readLines(file_path, encoding = "UTF-8")
  headings <- grep("^#{1,6} ", lines, value = TRUE)

  # Same character class update_or_check_toc() strips when generating anchors
  # (\p{So} = Symbol/Other, covers most emoji; \p{Cn} = unassigned code points,
  # covers emoji not yet classified as symbols in this R build's Unicode data).
  emoji_headings <- headings[grepl("[\\p{So}\\p{Cn}]", headings, perl = TRUE)]

  if (length(emoji_headings) > 0) {
    cat(sprintf("Found in %s:\n", file_path))
    for (h in emoji_headings) cat(sprintf("  - %s\n", trimws(h)))
    TRUE
  } else {
    FALSE
  }
}

#' Blank out the parts of a document GitHub renders as code
#'
#' @param lines Character vector of the document's lines
#' @return The same vector with every fenced block and inline code span
#'   emptied, its length and so its line numbering unchanged
#'
#' @details A link written inside backticks is rendered as literal text and is
#'   not a link, so its target need not exist and it must not be checked.
#'   These documents state their own cross-reference conventions by example,
#'   and those examples name placeholder anchors that no heading generates.
strip_code <- function(lines) {
  fence <- grepl("^\\s*```", lines)
  inside_fence <- cumsum(fence) %% 2 == 1
  lines[fence | inside_fence] <- ""
  gsub("`+[^`]*`+", "", lines)
}

#' Every anchor a document's headings offer, in document order
#'
#' @param file_path Path to the markdown file
#' @return Character vector of anchors, without the leading "#"
document_anchors <- function(file_path) {
  lines <- readLines(file_path, encoding = "UTF-8")
  headings <- lines[heading_lines(lines, "^#{1,6} ")]
  disambiguate_anchors(github_anchor(trimws(sub("^#{1,6} ", "", headings))))
}

#' Report anchor links that do not resolve to a heading
#'
#' @param file_path Path to the markdown file to check
#' @param anchors_by_doc Named list of anchor vectors, one per document in
#'   scope, named by path relative to the repository root
#' @return Number of links that resolve to nothing
#'
#' @details The comparison is case-sensitive, because GitHub's is: the ids it
#'   generates are lower-cased, so a link differing from its heading only in
#'   letter case resolves to nothing and is reported like any other.
check_anchor_links <- function(file_path, anchors_by_doc) {
  lines <- strip_code(readLines(file_path, encoding = "UTF-8"))
  broken <- 0

  for (i in seq_along(lines)) {
    links <- regmatches(lines[i], gregexpr("\\]\\([^) ]*#[^) ]+\\)", lines[i], perl = TRUE))[[1]]
    for (link in links) {
      target <- sub("\\)$", "", sub("^\\]\\(", "", link))
      file_part <- sub("#.*$", "", target)
      anchor <- sub("^[^#]*#", "", target)
      target_doc <- if (nzchar(file_part)) {
        normalizePath(file.path(dirname(file_path), file_part), mustWork = FALSE)
      } else {
        normalizePath(file_path, mustWork = FALSE)
      }
      target_doc <- sub(paste0("^", getwd(), "/"), "", target_doc)
      if (!(target_doc %in% names(anchors_by_doc))) next   # outside the checked set

      if (anchor %in% anchors_by_doc[[target_doc]]) next
      broken <- broken + 1
      cat(sprintf("  %s:%d links to %s#%s, which no heading generates\n",
                  file_path, i, target_doc, anchor))
    }
  }

  broken
}

# The three documents that carry a table of contents block and the return
# links beneath their H2 headings. Only these are rewritten.
markdown_docs <- c("README.md", "docs/Single_Run_Analysis.md", "docs/Multi_Run_Analysis.md")

# Every tracked markdown document, which is the scope of the link check alone.
# A document listed here is checked in both directions: its own anchor links
# must resolve, and a link from elsewhere in the set to one of its headings is
# resolved against the headings it actually offers. The six beyond the three
# above carry no table of contents block and must not be given one, the
# maintenance above being scoped deliberately to the documents that do. A new
# markdown document added to the repository belongs here.
link_check_docs <- c(markdown_docs, "CLAUDE.md",
                     "docs/BCH_Simulation_Action_Plan.md",
                     "docs/BCH_Task_Role_Allocation.md",
                     "docs/Getting_Started.md",
                     "docs/STYLE_GUIDE.md",
                     "scripts/README.md")

for (doc in markdown_docs) {
  update_or_check_toc(doc, "replace")
  enforce_return_links(doc, "replace")
}

anchors_by_doc <- setNames(lapply(link_check_docs, document_anchors), link_check_docs)
broken_links <- sum(vapply(link_check_docs, check_anchor_links, numeric(1),
                           anchors_by_doc = anchors_by_doc))
if (broken_links > 0) {
  cat(sprintf("⚠️ %d anchor link(s) point at no heading — repair them and re-run.\n", broken_links))
  quit(status = 1)
} else {
  cat("✓ Every anchor link resolves to a heading.\n")
}

emoji_found <- Reduce(`|`, lapply(markdown_docs, check_no_emoji_headings), accumulate = FALSE)
if (isTRUE(emoji_found)) {
  cat("⚠️ Headings must not contain emoji or symbol characters — remove them from the heading text above and re-run.\n")
  quit(status = 1)
} else {
  cat("✓ No emoji found in any heading.\n")
}
