# readAndMergeEuCodedFiles -----------------------------------------------------

#' Read and Merge Files in EN13508.2-Format
#' 
#' Read files in EN13508.2-format using \code{\link{readEuCodedFiles}} and merge
#' them by means of \code{\link{mergeInspectionData}}
#' 
#' @param input.files full path to text file containing CCTV inspection results
#'   in the format described in DIN EN 13508-2
#' @param dbg if \code{TRUE} debug messages are shown
#' 
#' @export
#' 
readAndMergeEuCodedFiles <- function(input.files, dbg = FALSE)
{
  # by setting simple.algorithm = FALSE we get unique column names, e.g. "ADE"
  # and "ADE.1"    
  mergeInspectionData(readEuCodedFiles(
    input.files = input.files, simple.algorithm = FALSE, dbg = dbg
  ))  
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
  if (length(x) == 1) {
    
    return (x[[1]])
  }
  
  # Check if there are differences in the file headers
  warnOnDifferingHeaders(x)
  
  # In any case, use the first header
  header_info <- kwb.utils::selectElements(x[[1]], "header.info")
  
  # Join the inspections
  inspections_all <- kwb.utils::safeRowBindOfListElements(x, "inspections")
  
  # Join the observations
  observations_all <- NULL
  
  # Prepare vector of offsets to be added to the inspection number (= row number
  # in list element "inspections")
  offsets <- cumsum(numberOfInspections(x))
  
  for (i in seq_along(x)) {
    
    observations <- kwb.utils::selectElements(x[[i]], "observations")
          
    # Add inspection number offset (maximum value so far) to column "inspno"
    if (i > 1) {
      
      inspno <- kwb.utils::selectColumns(observations, "inspno")
      
      observations$inspno <- inspno + offsets[i - 1]
    }
    
    observations_all <- kwb.utils::safeRowBind(observations_all, observations)
  }  
    
  list(
    header.info = header_info, 
    inspections = inspections_all,
    observations = observations_all
  )
}

# warnOnDifferingHeaders -------------------------------------------------------

warnOnDifferingHeaders <- function(x)
{
  # list to data frame
  header_infos <- do.call(rbind, lapply(x, function(x) {
    
    as.data.frame(kwb.utils::selectElements(x, "header.info"))
  }))
  
  # In which columns all the values are equal?
  equal_in_column <- sapply(header_infos, kwb.utils::allAreEqual)
    
  if (! all(equal_in_column)) {
    
    text <- paste0(
      "The file headers are differing in the folowing fields:\n\n",
      paste(
        utils::capture.output(print(unique(header_infos[, ! equal_in_column]))), 
        collapse = "\n"
      ),
      "\n\nI will use the first header."
    )
    
    warning(text)
  }
}
