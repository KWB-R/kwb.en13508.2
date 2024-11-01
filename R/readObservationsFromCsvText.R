# readObservationsFromCsvText --------------------------------------------------
readObservationsFromCsvText <- function(text, sep, dec, quote, colClasses, ...)
{
  # If colClasses is specified, reduce it to the columns that actually occur
  if (!identical(colClasses, NA)) {
    
    # Get the column names from the first line
    colNames <- strsplit(text[1L], sep)[[1L]]
    
    # Check that the column names are unique
    stopifnot(anyDuplicated(colNames) == 0L)
    
    # Check that we know the column class for each column name
    stopifnot(all(colNames %in% names(colClasses)))
    
    colClasses <- colClasses[colNames]
  }
  
  dot.args <- list(...)
  #dot.args <- list(header = TRUE) # for debugging!
  
  result <- try(kwb.utils::callWith(
    utils::read.table,
    text = text, 
    sep = sep, 
    dec = dec, 
    quote = quote, 
    comment.char = "", 
    blank.lines.skip = FALSE, 
    stringsAsFactors = FALSE, 
    colClasses = colClasses,
    dot.args
  ))
  
  if (!kwb.utils::isTryError(result)) {
    return(result)
  }
  
  result <- kwb.utils::callWith(
    utils::read.table,
    text = text, 
    sep = sep, 
    dec = dec, 
    quote = quote, 
    comment.char = "", 
    blank.lines.skip = FALSE, 
    stringsAsFactors = FALSE, 
    colClasses = NA,
    dot.args
  )
  
  convertTypes(result, codes = inspectionDataFieldCodes())
}
