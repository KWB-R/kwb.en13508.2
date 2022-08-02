# readAndMergeEuCodedFiles -----------------------------------------------------

#' Read and Merge Files in EN13508.2-Format
#' 
#' Read files in EN13508.2-format using \code{\link{readEuCodedFiles}} and merge
#' them by means of \code{\link{mergeInspectionData}}
#' 
#' @param input.files full path to text file containing CCTV inspection results
#'   in the format described in DIN EN 13508-2
#' @param dbg if \code{TRUE} debug messages are shown
#' @param \dots further arguments passed to \code{\link{readEuCodedFiles}}
#' @export
#' 
readAndMergeEuCodedFiles <- function(input.files, dbg = FALSE, ...)
{
  # by setting simple.algorithm = FALSE we get unique column names, e.g. "ADE"
  # and "ADE.1"    
  mergeInspectionData(
    readEuCodedFiles(
      input.files = input.files, 
      simple.algorithm = FALSE, 
      dbg = dbg, 
      ...
    )
  )
}

# mergeInspectionData ----------------------------------------------------------

#' Merge Inspection Data
#' 
#' Merge inspections and observations provided in a list
#' 
#' @param x list of elements each of which represents inspection data read from
#'   an EN13508.2-encoded file by means of \code{\link{readEuCodedFile}}.
#'   
#' @return list with elements \code{header.info}, \code{inspections}, 
#'   \code{observations}.
#' 
#' @export
#' 
mergeInspectionData <- function(x)
{
  if (length(x) == 1L) {
    return (x[[1L]])
  }
  
  # Check if there are differences in the file headers
  warnOnDifferingHeaders(x)
  
  # Prepare vector of offsets to be added to the inspection number (= row number
  # in list element "inspections")
  offsets <- cumsum(numberOfInspections(x))
  
  # Add offsets to observation table's column "inspno"
  observations <- lapply(seq_along(x), function(i) {
    
    obs <- get_elements(x[[i]], "observations")

    # Add inspection number offset to column "inspno" if this is not the very 
    # first data frame of observations
    if (i > 1L) {
      obs[["inspno"]] <- get_columns(obs, "inspno") + offsets[i - 1L]
    }
    
    obs
  })

  list(
    header.info = get_elements(x[[1L]], "header.info"), 
    inspections = kwb.utils::safeRowBindOfListElements(x, "inspections"),
    observations = kwb.utils::safeRowBindAll(observations)
  )
}

# warnOnDifferingHeaders -------------------------------------------------------
warnOnDifferingHeaders <- function(x)
{
  # list to data frame
  header_infos <- do.call(rbind, lapply(x, function(x) {
    as.data.frame(get_elements(x, "header.info"))
  }))
  
  # In which columns all the values are equal?
  equal_in_column <- sapply(header_infos, kwb.utils::allAreEqual)
    
  if (! all(equal_in_column)) {
  
    text <- paste(collapse = "\n", utils::capture.output(print(
      unique(header_infos[, ! equal_in_column, drop = FALSE])
    )))
    
    warning(
      "The file headers are differing in the folowing fields:\n\n",
      text,
      "\n\nI will use the first header.",
      call. = FALSE
    )
  }
}
