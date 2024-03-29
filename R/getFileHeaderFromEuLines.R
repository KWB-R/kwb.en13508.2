# getFileHeaderFromEuLines -----------------------------------------------------
getFileHeaderFromEuLines <- function(eu_lines, warn = TRUE)
{
  a.lines <- grep("^#A", eu_lines, value = TRUE)
  
  # original_fields <- do.call(kwb.utils::toLookupList, kwb.utils::toKeysAndValues(
  #   kwb.utils::collapsed(gsub("^#", "", a.lines), "@"),
  #   separators = c("@", "=")
  # ))
  # 
  # renamed_fields <- kwb.utils::renameColumns(original_fields, list(
  #   A1 = "encoding", A2 = "language", A3 = "separator", A4 = "decimal",
  #   A5 = "quote", A6 = "year"
  # ))
  
  # Set quote to "" instead of NA because read.table will give strange results
  quote <- findKeyAndExtractValue(a.lines, "A5", warn = warn)
  
  list(
    encoding = findKeyAndExtractValue(a.lines, "A1", warn = warn),
    language = findKeyAndExtractValue(a.lines, "A2", warn = warn),
    separator = findKeyAndExtractValue(a.lines, "A3", warn = warn),
    decimal = findKeyAndExtractValue(a.lines, "A4", warn = warn),
    quote = kwb.utils::defaultIfNA(quote, ""),
    year = findKeyAndExtractValue(a.lines, "A6", warn = warn)
  )
}

# findKeyAndExtractValue -------------------------------------------------------
findKeyAndExtractValue <- function(keyvalues, key, default = NA, warn = TRUE)
{
  pattern <- paste0("^#", key)
  
  index <- grep(pattern, keyvalues)
  
  if (length(index) == 0L) {
    
    warnMessage <- sprintf(
      "Key '#%s' not found in the #A-header of the file.", key
    )
    
    if (! is.na(default)) {
      warnMessage <- paste(warnMessage, "I will use the default:", default)
    }
    
    if (warn) {
      message(warnMessage)
      warning(warnMessage)
    }
    
    default
    
  } else {
    
    strsplit(keyvalues[index], "=")[[1L]][2L]
  }
}
