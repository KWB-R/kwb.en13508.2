# getHeaderInfo ----------------------------------------------------------------
getHeaderInfo <- function(text)
{
  # Indices of header lines (starting with #A, #B, #C, or #Z
  indices <- grep("^#[ABCZ]", text)

  # Corresponding header lines
  headers <- text[indices]
  
  key_value = strsplit(headers, "=")
  
  keys <- sapply(key_value, "[", 1L)
  
  values <- character(length(keys))
  has_value <- lengths(key_value) > 1L
  values[has_value] <- sapply(key_value[has_value], "[", 2L)
  
  header_info <- data.frame(
    row = indices,
    type = substr(headers, 2L, 2L),
    key = keys,
    uniqueKey = rep("", length(indices)),
    value = values,
    stringsAsFactors = FALSE
  )
  
  set_unique_key <- function(data, type) {
    isType <- data$type == type
    uniqueValues <- unique(data$value[isType])
    key <- paste0(tolower(type), match(data$value[isType], uniqueValues))
    data$uniqueKey[isType] <- key
    data
  }
  
  header_info <- set_unique_key(header_info, "B")  
  header_info <- set_unique_key(header_info, "C")

  # Set inspection number in column "inspno"
  changes <- kwb.utils::findChanges(header_info$type)
  b_starts <- changes$starts_at[changes$value == "B"]
    
  inspection_numbers <- rep(NA_integer_, nrow(header_info))
  inspection_numbers[b_starts] <- seq_along(b_starts)
  inspection_numbers <- kwb.utils::naToLastNonNa(inspection_numbers)
  
  header_info[["inspno"]] <- inspection_numbers
  
  header_info
}
