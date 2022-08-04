# readEuCodedFiles -------------------------------------------------------------

#' Read Multiple CCTV Inspection Files
#' 
#' Read CCTV inspection data from multiple files coded according to EN13508-2
#' 
#' @param input.files vector of character paths to input files
#' @param dbg if \code{TRUE} debug messages are shown
#' @param append.file.names if TRUE (default) the filename will be provided in
#'   column \code{file} in the \code{inspections} element of the result list
#' @param \dots arguments passed to \code{\link{readEuCodedFile}}, such as 
#'   \code{read.inspections}, \code{simple.algorithm}, \code{warn}, see there.
#'   
#' @return list of sublists each of which is the result of a call to 
#'   \code{\link{readEuCodedFile}}. The names of the list elements are 
#'   constructed from the file names of the input files. Special characters 
#'   in the file names are replaced with underscore. Names will get a preceding 
#'   letter "x" if they start with a digit or with underscore. If files could
#'   not be read correctly, their indices are returnded in attribute
#'   \code{which_failed}.
#' @importFrom kwb.utils catIf isTryError stringList substSpecialChars
#' @export
#' 
readEuCodedFiles <- function(
  input.files, dbg = TRUE, append.file.names = TRUE, ...
)
{
  result <- lapply(seq_along(input.files), function(i) {
    
    input.file <- input.files[i]
    
    kwb.utils::catIf(dbg, sprintf(
      "input file %d/%d: %s\n", i, length(input.files), input.file
    ))
    
    inspectionData <- try(readEuCodedFile(input.file, dbg = dbg, ...))
    
    # Return NULL if an error occurred
    if (kwb.utils::isTryError(inspectionData)) {
      return(NULL)
    }
    
    if (append.file.names) {
      inspectionData$inspections <- setFilename(
        inspectionData$inspections, 
        basename(input.file)
      )
    }
    
    inspectionData
  })

  failed <- sapply(result, is.null)
  
  # Give a warning about occurred errors
  if (any(failed)) {
    
    warning(call. = FALSE, sprintf(
      "readEuCodedFile() returned with error for the following %d files:\n%s", 
      sum(failed), kwb.utils::stringList(basename(input.files[failed]))
    ))
  }

  # Create valid list element names
  elements <- kwb.utils::substSpecialChars(basename(input.files))
  
  # Prepend an "x" to element names that start with a digit
  starts_with_digit <- grepl("^[0-9_]", elements)
  elements[starts_with_digit] <- paste0("x", elements[starts_with_digit])

  # Set list element names
  result <- stats::setNames(result[! failed], elements[! failed])
  
  # Return the indices of the files that could not be read correctly
  structure(result, which_failed = which(failed))
}

# setFilename ------------------------------------------------------------------
setFilename <- function(data, name)
{
  data[["file"]] <- name
  kwb.utils::moveColumnsToFront(data, "file")
}
