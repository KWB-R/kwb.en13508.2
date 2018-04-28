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
  mergeInspectionData(inspectionDataList = readEuCodedFiles(
    input.files = input.files, simple.algorithm = FALSE, dbg = dbg
  ))  
}

# mergeInspectionData ----------------------------------------------------------

#' Merge Inspection Data
#' 
#' Merge inspections and observations provided in a list
#' 
#' @param inspectionDataList list of elements each of which represents
#'   inspection data read from an EN13508.2-encoded file by means of
#'   \code{\link{readEuCodedFile}}.
#'   
#' @return list with elements \code{header.info}, \code{inspections}, 
#'   \code{observations}.
#' 
#' @export
#' 
mergeInspectionData <- function(inspectionDataList)
{
  if (length(inspectionDataList) == 1) {
    
    return (inspectionDataList[[1]])
  }
  
  # Check if there are differences in the file headers
  warnOnDifferingHeaders(inspectionDataList)
  
  # In any case, use the first header
  header.info <- inspectionDataList[[1]]$header.info
  
  # Join the inspections
  inspections.all <- kwb.utils::safeRowBindOfListElements(
    inspectionDataList, "inspections"
  )
  
  # Join the observations
  observations.all <- NULL
  
  # Prepare vector of offsets to be added to the inspection number (= row number
  # in list element "inspections")
  offsets <- cumsum(numberOfInspections(inspectionDataList))
  
  for (i in seq_along(inspectionDataList)) {
    
    observations <- inspectionDataList[[i]]$observations
          
    # Add inspection number offset (maximum value so far) to column "inspno"
    if (i > 1) {
      
      observations$inspno <- observations$inspno + offsets[i - 1]
    }
    
    observations.all <- kwb.utils::safeRowBind(observations.all, observations)    
  }  
    
  list(
    header.info = header.info, 
    inspections = inspections.all,
    observations = observations.all
  )
}

# warnOnDifferingHeaders -------------------------------------------------------

warnOnDifferingHeaders <- function(inspectionDataList)
{
  header.infos.list <- lapply(inspectionDataList, function(x) {
    
    as.data.frame(x$header.info)
  })
  
  # list to data frame
  header.infos <- do.call(rbind, header.infos.list)
  
  # In which columns are all the values equal?
  equalInColumn <- sapply(header.infos, kwb.utils::allAreEqual)
    
  if (! all(equalInColumn)) {
    
    warningText <- paste0(
      "The file headers are differing in the folowing fields:\n\n",
      paste(
        utils::capture.output(print(unique(header.infos[, !equalInColumn]))), 
        collapse = "\n"
      ),
      "\n\nI will use the first header."
    )
    
    #message(warningText)
    warning(warningText)
  }
}
