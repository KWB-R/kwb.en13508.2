# addInspectionNumbers ---------------------------------------------------------
addInspectionNumbers <- function(
    observations, indices, maxline, old.version = FALSE, dbg = TRUE
)
{
  inspnos <- try(if (old.version) {
    getInspectionNumbers.old(
      indices.C = indices$C, 
      indices.Z = indices$Z,
      numberOfInspections = length(indices$B01), 
      maxline = maxline
    )
  } else {
    getInspectionNumbers(
      indices.C = indices$C, 
      indices.B01 = indices$B01, 
      indices.B = indices$B, 
      indices.Z = indices$Z
    )
  })
  
  stopifnot(length(inspnos) == nrow(observations))
  
  if (is.null(inspnos)) {
    message(
      "I could not determine the inspection numbers so I put NA into ",
      "column 'inspno'!"
    )
    inspnos <- NA
  }
  
  kwb.utils::setColumns(observations, inspno = inspnos, dbg = dbg)
}

# getInspectionNumbers ---------------------------------------------------------
getInspectionNumbers <- function(indices.C, indices.B01, indices.B, indices.Z)
{
  # To find the number of the inspection corresponding to the observation block 
  # look for the "nearest" #B01-index "up" of the #C-indices and return the
  # position of this matching index within indices.B01
  if (length(indices.B01) == 0L) {
    indices.B01 <- indices.C - 1L
  }
  
  inspectionID <- sapply(indices.C, FUN = function(index.C) {
    which.max(indices.B01[indices.B01 < index.C])
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
  times <- nearest.index.Z - indices.C - 1L
  
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
