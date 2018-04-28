# getObservationsFromEuLines ---------------------------------------------------

getObservationsFromEuLines <- function(
  eu.lines, header.info, old.version = FALSE, dbg = TRUE
)
{
  indices.A <- grep("^#A", eu.lines)
  
  indices.B <- grep("^#B", eu.lines)
  
  indices.C <- grep("^#C", eu.lines)
  
  indices.Z <- grep("^#Z", eu.lines)
  
  # If the file does not end with #Z add "number of lines + 1" to the vector of
  # #Z-indices
  lastIndex.Z <- if (kwb.utils::isNullOrEmpty(indices.Z)) {
    
    -1
    
  } else {
    
    indices.Z[length(indices.Z)]
  }

  if (lastIndex.Z != length(eu.lines)) {
    
    indices.Z <- c(indices.Z, length(eu.lines) + 1)
    
    kwb.utils::catIf(
      dbg, "A 'virtual' inspection block end '#Z' has been added.\n"
    )
  }
    
  c.caption.lines <- getValueFromKeyValueString(eu.lines[indices.C])
  
  stopifnot(kwb.utils::allAreEqual(c.caption.lines))
  
  indices.remove <- c(
    indices.A, indices.B, indices.B + 1, indices.C, indices.Z
  )
  
  observations <- getObservationsAsDataFrame(
    captionLine = c.caption.lines[1], c.value.lines = eu.lines[-indices.remove], 
    header.info = header.info
  )
  
  indices.B01 <- indices.B[grep("^#B01=", eu.lines[indices.B])]
  
  numberOfInspections <- length(indices.B01)
  
  # Try to generate a vector of inspection numbers assigning to each observation
  # the number of inspection that it belongs to 
  inspectionNumbers <- try(if (old.version) {
    getInspectionNumbers.old(
      indices.C, indices.Z, numberOfInspections, maxline = length(eu.lines)
    )      
  } else {
    getInspectionNumbers(indices.C, indices.B01, indices.B, indices.Z)
  })
  
  stopifnot(length(inspectionNumbers) == nrow(observations))
  
  if (is.null(inspectionNumbers)) {
    
    message(
      "I could not determine the inspection numbers so I put NA into ",
      "column 'inspno'!"
    )
    
    inspectionNumbers <- NA
  }
  
  kwb.utils::setColumns(observations, inspno = inspectionNumbers)
}

# getObservationsAsDataFrame ---------------------------------------------------

getObservationsAsDataFrame <- function(captionLine, c.value.lines, header.info)
{  
  c.captions <- strsplit(captionLine, header.info$separator)[[1]]
  
  colClasses <- sapply(c.captions, FUN = inspectionDataFieldCodeClass)
  
  nullClasses <- colClasses[sapply(colClasses, is.null)]
  
  if (length(nullClasses) > 0) {
    
    skippedColumns <- names(nullClasses)
    
    warning(
      "The following columns of the 'C-blocks' are skipped since their ", 
      "meaning is unknown:\n", kwb.utils::stringList(skippedColumns)
    )
    
    c.captions <- setdiff(c.captions, skippedColumns)
  }
  
  observations <- kwb.utils::csvTextToDataFrame(
    text = paste(c.value.lines, collapse = "\n"), sep = header.info$separator, 
    dec = header.info$decimal, quote = header.info$quote, comment.char = "", 
    blank.lines.skip = FALSE, stringsAsFactors = FALSE, colClasses = colClasses
  )
  
  stats::setNames(observations, c.captions)
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
