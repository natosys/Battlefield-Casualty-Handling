generate_toc <- function(file_path) {
  # Read the markdown file
  lines <- readLines(file_path)
  
  # Extract heading lines and their levels
  headings <- grep("^#{1,6} ", lines, value = TRUE)
  
  # Create anchor links and TOC entries
  toc <- lapply(headings, function(h) {
    level <- attr(regexpr("^#+", h), "match.length")  # Count # characters
    title <- trimws(sub("^#{1,6} ", "", h))  # Remove # and leading spaces
    anchor <- tolower(gsub("[^a-zA-Z0-9\\s]", "", title))  # Remove punctuation
    anchor <- gsub("\\s+", "-", anchor)  # Replace spaces with hyphens
    
    indent <- paste(rep("  ", level - 1), collapse = "")  # Indent by level
    paste0(indent, "- [", title, "](#", anchor, ")")
  })
  
  # Write TOC to console or file
  toc_output <- paste(unlist(toc), collapse = "\n")
  cat(toc_output)
}

# Example usage
generate_toc("README.md")