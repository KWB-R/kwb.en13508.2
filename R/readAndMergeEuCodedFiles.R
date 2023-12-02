# readAndMergeEuCodedFiles -----------------------------------------------------

#' Read and Merge Files in EN13508.2-Format
#' 
#' Read files in EN13508.2-format using \code{\link{readEuCodedFiles}} and merge
#' them by means of \code{\link{mergeInspectionData}}
#' 
#' @param input.files full path to text file containing CCTV inspection results
#'   in the format described in DIN EN 13508-2
#' @param dbg if \code{TRUE} debug messages are shown
#' @param name.convention passed to \code{\link{readEuCodedFiles}}
#' @param \dots further arguments passed to \code{\link{readEuCodedFiles}}
#' @param add.inspid if \code{TRUE} (the default is \code{FALSE}) a globally 
#'   unique inspection ID (inspid) is added to the data frames in elements 
#'   "inspections" and "observations" of the returned list.
#' @param project name of project to which the data are related, such as:
#'   "Lausanne"
#' @param default.time passed to \code{\link{setGlobalInspectionID}}
#' @export
#' 
readAndMergeEuCodedFiles <- function(
  input.files, 
  dbg = FALSE, 
  name.convention = "norm",
  ..., 
  add.inspid = FALSE,
  project = NULL,
  default.time = "22:22"
)
{
  # by setting simple.algorithm = FALSE we get unique column names, e.g. "ADE"
  # and "ADE.1"
  inspection.data.list <- readEuCodedFiles(
    input.files = input.files, 
    dbg = dbg, 
    name.convention = name.convention,
    simple.algorithm = FALSE, 
    ...
  )
  
  inspection.data <- mergeInspectionData(inspection.data.list)
  
  if (!add.inspid) {
    return(inspection.data)
  }

  setGlobalInspectionID(
    inspection.data, 
    project, 
    default.time = default.time,
    name.convention = name.convention
  )
}

# mergeInspectionData ----------------------------------------------------------

#' Merge Inspection Data
#' 
#' Merge inspections and observations provided in a list
#' 
#' @param x list of elements each of which represents inspection data read from
#'   an EN13508.2-encoded file by means of \code{\link{readEuCodedFile}}.
#' @param warn logical indicating whether to warn about different header
#'  information. By default, warnings are not shown.  
#' @return list with elements \code{header.info}, \code{inspections}, 
#'   \code{observations}.
#' 
#' @export
#' 
mergeInspectionData <- function(x, warn = FALSE)
{
  if (length(x) == 1L) {
    return (x[[1L]])
  }
  
  # Check if there are differences in the file headers
  if (warn) {
    warnOnDifferingHeaders(x)
  }
  
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

  header.info <- get_elements(x[[1L]], "header.info")
  inspections <- kwb.utils::safeRowBindOfListElements(x, "inspections")
  observations <- kwb.utils::safeRowBindAll(observations)
  
  # Replace NA with "" in columns of type character
  inspections <- replaceNaWithEmptyStringInCharColumns(inspections)
  observations <- replaceNaWithEmptyStringInCharColumns(observations)
  
  list(
    header.info = header.info, 
    inspections = inspections,
    observations = observations
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
      "The file headers are differing in the following fields:\n\n",
      text,
      "\n\nI will use the first header.",
      call. = FALSE
    )
  }
}
