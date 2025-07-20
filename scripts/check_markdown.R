Sys.setlocale("LC_CTYPE", "")

update_or_check_toc <- function(file_path, mode = c("verify", "replace"), toc_start = "<!-- TOC START -->", toc_end = "<!-- TOC END -->") {
  mode <- match.arg(mode)
  lines <- readLines(file_path)
  
  # Extract headings H2 to H6
  headings <- grep("^#{2,6} ", lines, value = TRUE)
  toc <- lapply(headings, function(h) {
    level <- attr(regexpr("^#+", h), "match.length")
    title <- trimws(sub("^#{2,6} ", "", h))
    
    # Remove emojis and punctuation for anchor
    title_clean <- gsub("[\\p{So}\\p{Cn}]", "", title, perl = TRUE)
    anchor <- tolower(title_clean)
    anchor <- gsub("[[:punct:]]", "", anchor)
    anchor <- gsub("\\s+", "-", anchor)
    
    indent <- paste(rep("  ", level - 2), collapse = "")
    paste0(indent, "- [", title, "](#", anchor, ")")
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
      writeLines(updated_lines, file_path)
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
                                 return_text = "<small>[Return to Top](#contents)</small>",
                                 log_path = "log.log") {
  mode <- match.arg(mode)
  lines <- readLines(file_path)
  new_lines <- c()
  i <- 1
  missing_count <- 0
  patched_headings <- c()
  
  # Pattern to detect ALL return-to-top links
  generic_return_pattern <- "(?i)(<sub>|<small>)?\\[return to top\\]\\(#.*?\\)(</sub>|</small>)?"
  
  # Define canonical version to preserve
  canonical_return <- "<small>[Return to Top](#contents)</small>"

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
    writeLines(new_lines, file_path)
    cat(sprintf("✅ Standardized return links under %d H2 headings in %s\n", length(patched_headings), file_path))
    
    if (length(patched_headings) > 0) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- sprintf("[%s] Updated 'Return to Top' under H2 in %s:\n- %s\n\n", 
                           timestamp, file_path, paste(patched_headings, collapse = "\n- "))
      dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
      write(log_entry, file = log_path, append = TRUE)
    }
  }
}

update_or_check_toc("README.md", "replace")
enforce_return_links("README.md", "replace")
