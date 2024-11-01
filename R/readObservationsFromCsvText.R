# readObservationsFromCsvText --------------------------------------------------
readObservationsFromCsvText <- function(
    text, sep, dec, quote, colClasses, dbg = TRUE, ...
)
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
  
  common_args <- c(dot.args, list(
    text = text, 
    sep = sep, 
    dec = dec, 
    quote = quote, 
    comment.char = "", 
    blank.lines.skip = FALSE, 
    stringsAsFactors = FALSE
  ))
  
  tryCatch(
    expr = do.call(
      utils::read.table, c(common_args, list(colClasses = colClasses))
    ), 
    silent = TRUE, 
    error = {
      data <- do.call(utils::read.table, c(common_args, list(colClasses = NA)))
      convertTypes(data, dbg = dbg, classes = sapply(
        X = get_elements(inspectionDataFieldCodes(), names(data)), 
        FUN = get_elements, 
        elements = "class"
      ))
    }
  )
}

# convertTypes -----------------------------------------------------------------
convertTypes <- function(data, classes, dbg = TRUE)
{
  verbose_converter <- function(FUN, what) {
    function(x) {
      suppressWarnings(y <- FUN(x))
      failed <- !is.na(x) & is.na(y)
      if (any(failed)) {
        message(sprintf(
          "Could not convert the following values to %s: %s",
          what, kwb.utils::stringList(unique(x[failed]))
        ))
      }
      y
    }
  }
  
  converters <- list(
    character = as.character,
    numeric = verbose_converter(as.numeric, "numeric"),
    integer = verbose_converter(as.integer, "integer")
  )
  
  for (column in names(which(sapply(data, "class") != classes))) {
    class <- classes[column]
    data[[column]] <- kwb.utils::catAndRun(
      messageText = sprintf("Converting column '%s' to %s", column, class), 
      expr = (get_elements(converters, class))(data[[column]]),
      dbg = dbg
    )
  }
  
  data
}
