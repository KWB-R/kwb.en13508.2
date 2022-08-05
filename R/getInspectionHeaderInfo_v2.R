# getInspectionHeaderInfo_v2 ---------------------------------------------------
getInspectionHeaderInfo_v2 <- function(eu_lines)
{
  pattern <- "^#B(\\d\\d)=(.*)$"
  
  # Get list of matching sub expressions
  matches <- kwb.utils::extractSubstring(
    pattern = pattern, 
    x = eu_lines, 
    index = c(number = 1L, fields = 2L)
  )
  
  matches$row <- seq_along(eu_lines)
  
  # Indices of header lines
  header_indices <- which(!kwb.utils::isNaOrEmpty(matches$fields))
  
  # Number of header (#B01 = 1, #B02 = 2)
  header_numbers <- as.integer(matches$number[header_indices])
  
  # Only the header (right of equal sign)
  header_lines <- matches$fields[header_indices]
  
  unique_headers <- unique(header_lines)
  
  # For each different type of header, determine the line numbers in which it
  # occurs
  header_rows <- lapply(unique_headers, function(header) {
    
    indices <- which(header_lines == header)
    
    stopifnot(kwb.utils::allAreEqual(header_numbers[indices]))
    
    list(line = header_numbers[indices[1L]], rows = header_indices[indices])
  })
  
  stats::setNames(header_rows, unique_headers)
}
