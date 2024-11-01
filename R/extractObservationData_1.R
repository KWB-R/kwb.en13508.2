# extractObservationData_1 -----------------------------------------------------
extractObservationData_1 <- function(
    text, header.info, old.version = FALSE, dbg = TRUE
)
{
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  
  # Create accessor function to header info fields
  fromHeader <- kwb.utils::createAccessor(header.info)
  
  # Get information on the row numbers where the different blocks start
  indices <- getBlockIndices(text, dbg = dbg)
  
  # Column separator
  sep <- fromHeader("separator")
  
  # Try to get the C-Block captions (if they are unique, otherwise rais error!)
  captions <- tryToGetUniqueCaptions(text[indices$C], sep)
  
  tableHeader <- paste(captions, collapse = sep)
  rowsToRemove <- c(indices$A, indices$B, indices$B + 1L, indices$C, indices$Z)
  tableBody <- text[-rowsToRemove]
  
  # Try to find the column types for the given captions
  colClasses <- getColClasses(codes = inspectionDataFieldCodes(), captions)
  
  observations <- readObservationsFromCsvText(
    text = c(tableHeader, tableBody), 
    sep = sep, 
    dec = fromHeader("decimal"), 
    quote = fromHeader("quote"), 
    colClasses = colClasses,
    header = TRUE
  )
  
  indices$B01 <- indices$B[grep("^#B01=", text[indices$B])]
  
  # Try to generate a vector of inspection numbers assigning to each observation
  # the number of inspection that it belongs to 
  result <- addInspectionNumbers(
    observations = observations, 
    indices = indices, 
    maxline = length(text),
    old.version = old.version,
    dbg = dbg
  )
  
  kwb.utils::moveColumnsToFront(result[, order(names(result))], "inspno")
}

# getBlockIndices --------------------------------------------------------------
getBlockIndices <- function(text, dbg = TRUE)
{
  block_letters <- c("A", "B", "C", "Z")
  
  patterns <- paste0("^#", block_letters)
  
  indices <- lapply(patterns, grep, text)
  
  names(indices) <- block_letters
  
  # If the file does not end with #Z add "number of lines + 1" to the vector of
  # #Z-indices
  last_z_index <- if (kwb.utils::isNullOrEmpty(indices$Z)) {
    -1L
  } else {
    kwb.utils::lastElement(indices$Z)
  }
  
  n_lines <- length(text)
  
  if (last_z_index != n_lines) {
    
    indices$Z <- c(indices$Z, n_lines + 1)
    
    kwb.utils::catIf(
      dbg, "A 'virtual' inspection block end '#Z' has been added.\n"
    )
  }
  
  indices
}

# tryToGetUniqueCaptions -------------------------------------------------------
tryToGetUniqueCaptions <- function(header_lines, sep)
{
  headers <- getValueFromKeyValueString(header_lines)
  
  stopifnot(kwb.utils::allAreEqual(headers))
  
  # Split the caption line into column captions using the separator  
  strsplit(headers[1L], sep)[[1L]]
}

# getColClasses ----------------------------------------------------------------
getColClasses <- function(codes, captions)
{
  colClasses <- sapply(captions, function(x) codes[[x]]$class)
  
  # Which columns are unknown (the type is NULL)
  is_null <- sapply(colClasses, is.null)
  
  # If all column names are unknown, read all columns by setting colClasses to NA
  if (all(is_null)) {
    warning(
      "None of the column names of the C-blocks is known.\n", 
      "Expected column names are:\n  ", paste0(
        names(codes), " (", sapply(codes, "[[", "meaning"), ")",
        collapse = "\n  "
      ), call. = FALSE
    )
    return(NA)
  } 
  
  # read.table() will skip the unknown columns
  if (any(is_null)) {
    warning(
      "The following columns of the 'C-blocks' are skipped since their ", 
      "meaning is unknown:\n", kwb.utils::stringList(names(which(is_null))), 
      call. = FALSE
    )
  }
  
  colClasses
}

# getColClasses2 ---------------------------------------------------------------
getColClasses2 <- function(codes, as.text)
{
  colClasses <- sapply(codes, get_elements, "class")
  
  if (as.text) {
    colClasses[] <- "character"
  }
  
  colClasses  
}

# convertTypes -----------------------------------------------------------------
convertTypes <- function(data, codes)
{
  target_classes <- sapply(
    get_elements(codes, names(data)), get_elements, "class"
  )
  
  given_classes <- sapply(data, "class")
  
  columns_convert <- names(which(given_classes != target_classes))
  
  for (column in columns_convert) {
    target_class <- target_classes[column]
    data[[column]] <- kwb.utils::catAndRun(
      sprintf("Converting column '%s' to %s", column, target_class),
      do.call(paste0("as.", target_class), list(data[[column]]))
    )
  }
  
  data
}
