# getObservationsFromEuLines ---------------------------------------------------
getObservationsFromEuLines <- function(
    eu_lines, header.info, old.version = FALSE, dbg = TRUE
)
{
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  #header.info <- kwb.en13508.2::euCodedFileHeader()
  
  # Create accessor function to header info fields
  header_field <- kwb.utils::createAccessor(header.info)
  
  # Get information on the row numbers where the different blocks start
  indices <- getBlockIndices(eu_lines, dbg = dbg)
  
  # Column separator
  sep <- header_field("separator")
  
  # Try to get the C-Block captions (if they are unique, otherwise rais error!)
  captions <- tryToGetUniqueCaptions(eu_lines[indices$C], sep)
  
  # Try to find the column types for the given captions
  colClasses <- getColClasses(codes = inspectionDataFieldCodes(), captions)
  
  observations <- readObservationsFromCsvText(
    text = eu_lines[-c(indices$A, indices$B, indices$B + 1L, indices$C, indices$Z)], 
    sep = sep, 
    dec = header_field("decimal"), 
    quote = header_field("quote"), 
    colClasses = unname(colClasses)
  )
  
  # Set the column names to the captions
  names(observations) <- if (identical(colClasses, NA)) {
    captions
  } else {
    captions[!sapply(colClasses, is.null)]
  }
  
  indices$B01 <- indices$B[grep("^#B01=", eu_lines[indices$B])]
  
  # Try to generate a vector of inspection numbers assigning to each observation
  # the number of inspection that it belongs to 
  result <- addInspectionNumbers(
    observations = observations, 
    indices = indices, 
    maxline = length(eu_lines),
    old.version = old.version,
    dbg = dbg
  )
  
  kwb.utils::moveColumnsToFront(result[, order(names(result))], "inspno")
}

# getBlockIndices --------------------------------------------------------------
getBlockIndices <- function(eu_lines, dbg = TRUE)
{
  block_letters <- c("A", "B", "C", "Z")
  
  patterns <- paste0("^#", block_letters)
  
  indices <- lapply(patterns, grep, eu_lines)
  
  names(indices) <- block_letters
  
  # If the file does not end with #Z add "number of lines + 1" to the vector of
  # #Z-indices
  last_z_index <- if (kwb.utils::isNullOrEmpty(indices$Z)) {
    -1L
  } else {
    kwb.utils::lastElement(indices$Z)
  }
  
  n_lines <- length(eu_lines)
  
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

# readObservationsFromCsvText --------------------------------------------------
readObservationsFromCsvText <- function(text, sep, dec, quote, colClasses, ...)
{
  # If colClasses is specified, reduce it to the columns that actually occur
  if (! identical(colClasses, NA)) {
    
    # Get the column names from the first line
    colNames <- strsplit(text[1L], sep)[[1L]]
    
    # Check that the column names are unique
    stopifnot(anyDuplicated(colNames) == 0L)
    
    # Check that we know the column class for each column name
    stopifnot(all(colNames %in% names(colClasses)))
    
    colClasses <- colClasses[colNames]
  }
  
  dot.args <- list(...)
  #dot.args <- list() # for debugging!
  
  kwb.utils::callWith(
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
  )
}
