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

update_or_check_toc("README.md", "replace")
