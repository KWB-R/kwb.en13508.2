# getObservationsFromEuLines ---------------------------------------------------
getObservationsFromEuLines <- function(
  eu_lines, header.info, old.version = FALSE, dbg = TRUE
)
{
  block_letters <- c("A", "B", "C", "Z")
  
  patterns <- paste0("^#", block_letters)
  
  indices <- stats::setNames(lapply(patterns, grep, eu_lines), block_letters)
  
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
  
  #kwb.utils::assignPackageObjects("kwb.en13508.2")  
  c_headers <- getValueFromKeyValueString(eu_lines[indices$C])
  
  stopifnot(kwb.utils::allAreEqual(c_headers))
  
  indices_remove <- c(indices$A, indices$B, indices$B + 1, indices$C, indices$Z)
  
  #header.info <- kwb.en13508.2::euCodedFileHeader()
  observations <- get_observations(
    caption_line = c_headers[1], 
    c_body = eu_lines[-indices_remove], 
    header_info = header.info
  )
  
  indices$B01 <- indices$B[grep("^#B01=", eu_lines[indices$B])]
  
  n_inspections <- length(indices$B01)
  
  # Try to generate a vector of inspection numbers assigning to each observation
  # the number of inspection that it belongs to 
  inspection_numbers <- try(if (old.version) {
    
    getInspectionNumbers.old(
      indices$C, indices$Z, n_inspections, maxline = length(eu_lines)
    )
    
  } else {
    
    getInspectionNumbers(indices$C, indices$B01, indices$B, indices$Z)
  })
  
  stopifnot(length(inspection_numbers) == nrow(observations))
  
  if (is.null(inspection_numbers)) {
    
    message(
      "I could not determine the inspection numbers so I put NA into ",
      "column 'inspno'!"
    )
    
    inspection_numbers <- NA
  }
  
  result <- kwb.utils::setColumns(observations, inspno = inspection_numbers)
  
  kwb.utils::moveColumnsToFront(result[, order(names(result))], "inspno")
}

# get_observations -------------------------------------------------------------
get_observations <- function(caption_line, c_body, header_info)
{
  # Select and rename elements from "header_info" into list "arguments"
  elements <- c(sep = "separator", dec = "decimal", quote = "quote")

  arguments <- get_elements(header_info, elements)

  # Split the caption line into column captions using the separator  
  captions <- strsplit(caption_line, arguments$sep)[[1]]

  # Get codes of inspection data columns
  codes <- inspectionDataFieldCodes()
  
  # Try to find the column types for the given captions
  colClasses <- sapply(captions, FUN = function(x) codes[[x]]$class)

  # Which columns are unknown (the type is NULL)
  is_null <- sapply(colClasses, is.null)

  # Are all columns unknown?
  all_null <- all(is_null)
  
  if (all_null) {
    
    warning(
      "None of the column names of the C-blocks is known.\n", 
      "Expected column names are:\n  ", paste0(
        names(codes), " (", sapply(codes, "[[", "meaning"), ")",
        collapse = "\n  "
      ), call. = FALSE
    )
    
  } else if (sum(is_null)) {
    
    warning(
      "The following columns of the 'C-blocks' are skipped since their ", 
      "meaning is unknown:\n", kwb.utils::stringList(names(which(is_null))), 
      call. = FALSE
    )
  }
  
  # If all column names are unknown, read all columns by setting colClasses
  # to NA, otherwise let read.table skip the unknown columns
  colClasses <- if (all(is_null)) NA else unname(colClasses)

  observations <- readObservationsFromCsvText(
    text = paste(c_body, collapse = "\n"), 
    sep = arguments$sep, 
    dec = arguments$dec, 
    quote = arguments$quote, 
    colClasses = colClasses
  )
  
  # Set the column names to the captions
  names(observations) <- if (all(is_null)) captions else captions[! is_null]
  
  observations
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
    if (interactive()) list() else list(...)
  )
}

# getInspectionNumbers ---------------------------------------------------------
getInspectionNumbers <- function(indices.C, indices.B01, indices.B, indices.Z)
{
  # To find the number of the inspection corresponding to the observation block 
  # look for the "nearest" #B01-index "up" of the #C-indices and return the
  # position of this matching index within indices.B01
  inspectionID <- sapply(indices.C, FUN = function(index.C) {
    
    which.max(indices.B01[indices.B01 < index.C])
  })
  
  # Find the "nearest" #B-index (either #B01 or #B02 or ...) "up" of the
  # #C-indices
  nearest.index.B <- sapply(indices.C, FUN = function(index.C) {
    
    max(indices.B[indices.B < index.C])
  })
  
  # Find the "nearest" #Z-index "down" of the #C indices
  nearest.index.Z <- sapply(indices.C, FUN = function(index.C) {
    
    min(indices.Z[indices.Z > index.C])
  })
  
  # For each #C-index we need to have an inspection ID
  stopifnot(length(indices.C) == length(inspectionID))
  
  # For each #C-index we need to have a corresponding #Z-index
  stopifnot(length(indices.C) == length(nearest.index.Z))
  
  # How often does each inspection ID need to be repeated in order to fill the
  # column "inspno" (inspection number = inspection ID)
  times <- nearest.index.Z - indices.C - 1
  
  # Repeat the inspection IDs, each as often as there are lines in the #C-block
  rep(inspectionID, times = times)
}

# getInspectionNumbers.old -----------------------------------------------------
getInspectionNumbers.old <- function(
  indices.C, indices.Z, numberOfInspections, maxline
)
{
  block.begs <- indices.C + 1
  block.ends <- indices.Z - 1
  
  missingBlockEnds <- numberOfInspections - length(block.ends)
  
  if (missingBlockEnds == 1) {
    
    block.ends <- c(block.ends, maxline)
    
  } else if (missingBlockEnds > 0) {
    
    stop(
      "I found ", numberOfInspections, " inspections (starting with '#B01') ",
      "but not the corresponding number of end indicators '#Z' (",
      missingBlockEnds, ")"
    )
  }
  
  if (length(block.ends) == length(block.begs)) {
    
    obs.lengths <- block.ends - block.begs + 1
    
    rep(seq_len(length(obs.lengths)), times = obs.lengths)
    
  } # else NULL implicitly
}
