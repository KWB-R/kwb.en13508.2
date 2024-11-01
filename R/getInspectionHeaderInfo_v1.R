# getInspectionHeaderInfo_v1 ---------------------------------------------------
getInspectionHeaderInfo_v1 <- function(text)
{
  # Get list of matching sub expressions
  matches <- kwb.utils::subExpressionMatches("^#B(\\d\\d)=(.*)$", text)
  
  # Indices of header lines
  header_indices <- which(! sapply(matches, is.null))
  
  # Keep only the sub expressions of matching rows
  matches <- matches[header_indices]
  
  # Number of header (#B01 = 1, #B02 = 2)
  header_numbers <- as.integer(sapply(matches, "[[", 1L))
  
  # Only the header (right of equal sign)
  header_lines <- sapply(matches, "[[", 2L)
  
  unique_headers <- unique(header_lines)
  
  # For each different type of header, determine the line numbers in which it
  # occurs
  header_rows <- lapply(unique_headers, function(header) {
    indices <- which(header_lines == header)
    header_number <- unique(header_numbers[indices])
    stopifnot(length(header_number) == 1L)
    list(line = header_number, rows = header_indices[indices])
  })
  
  stats::setNames(header_rows, unique_headers)
}
