# getCodes ---------------------------------------------------------------------

#' Get EU Codes and Their Meaning
#' 
#' Get a data frame containing EU codes and their meaning in different languages
#' 
#' @param table name or vector of names of tables in the EU norm for which to 
#'   get field information. Use \code{unique(getCodes()$Table)} to get the 
#'   possible table names.
#' @param fields set to a vector of field (column) names to restrict the columns
#'   returned
#'   
#' @return data frame
#'
#' @export
#' 
getCodes <- function(table = NULL, fields = NULL)
{
  codes <- readPackageFile("eucodes.csv")
  
  # Check if all codes are unique
  stopifnot(! any(duplicated(get_columns(codes, "Code"))))

  if (! is.null(table)) {
    
    subtables <- split(codes, get_columns(codes, "Table"))
    
    codes <- kwb.utils::selectElements(subtables, table)
    
    if (length(table) > 1L) {
      codes <- kwb.utils::safeRowBindAll(codes)
    }
  }

  # Reset row names
  row.names(codes) <- NULL
  
  if (is.null(fields)) {
    return(codes)
  }
  
  get_columns(codes, fields)
}

# numberOfInspections ----------------------------------------------------------

#' Number of Inspections
#' 
#' Get number of inspections from list of inspection data
#' 
#' @param x list of inspection data elements each of which was read from an EN
#'   13508-2-coded file by means of \code{\link{readEuCodedFile}}
#'   
#' @return vector of integer representing the number of inspections in each
#'   element of \code{inspectionDataList}
#' 
#' @export
#' 
numberOfInspections <- function(x)
{
  sapply(x, function(xx) nrow(get_elements(xx, "inspections")))
}

# inspectionDataFieldCodes -----------------------------------------------------

inspectionDataFieldCodes <- function()
{
  codeInfo <- readPackageFile("eucodes_de.csv")
  
  codes <- get_columns(codeInfo, "Code")
  
  columns <- c("class", "meaning")
  
  lapply(kwb.utils::toNamedList(codes), function(code) {
    
    as.list(get_columns(codeInfo[codes == code, ], columns))
  })
}

