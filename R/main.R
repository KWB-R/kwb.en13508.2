# get_columns ------------------------------------------------------------------
get_columns <- kwb.utils::selectColumns

# get_elements -----------------------------------------------------------------
get_elements <- kwb.utils::selectElements

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

# readPackageFile --------------------------------------------------------------

#' Read CSV File from Package's extdata Folder
#' 
#' @param file file name (without path)
#' @param \dots additional arguments passed to \code{read.csv}
#' 
#' @return data frame representing the content of \code{\link{file}}
#' 
readPackageFile <- function(file, ...)
{
  kwb.utils::readPackageFile(file, package = "kwb.en13508.2", ...)
}

# dataFrameContentToTextLines --------------------------------------------------

dataFrameContentToTextLines <- function(dframe, mycsv = FALSE, ...)
{
  if (mycsv) {
    
    dataFrameToCsvLines_v1(dframe, ...)
    
  } else {
    
    dataFrameToCsvLines_v2(dframe, ...)
  }
}

# dataFrameToCsvLines_v1 -------------------------------------------------------

dataFrameToCsvLines_v1 <- function(dframe, sep, ...) 
{
  #cat(sprintf("\nsep in dataFrameToCsvLines_v1(): '%s'\n", sep))
  
  n_columns <- ncol(dframe)
  
  output <- matrix("", nrow = nrow(dframe), ncol = n_columns)
  
  for (column_num in seq_len(n_columns)) {
    
    output[, column_num] <- valuesToCsv(dframe[[column_num]], sep = sep, ...)
  }
  
  apply(output, 1, paste, collapse = sep)
}

# dataFrameToCsvLines_v2 -------------------------------------------------------

dataFrameToCsvLines_v2 <- function(dframe, qchar = NULL, ...)
{
  # Do not pass qchar to write.table
  
  con <- textConnection("buffer", "w")
  
  utils::write.table(dframe, con, row.names = FALSE, col.names = FALSE, ...)      
  
  close(con)  
  
  buffer  
}

# valuesToCsv ------------------------------------------------------------------

#' Values to CSV
#' 
#' @param x vector of values representing a row in a CSV file
#' @param dec decimal character
#' @param sep field separating character
#' @param na text to be used in case of NA values
#' @param qchar quoting character to be used to surround text fields containing
#'   the field separator
#' @param qmethod method used to indicate that a quoting character within a
#'   quoted text field is not the ending quote. Either "double" (double the
#'   quote character) or "escape" (backslash in front of the quoting character).
#' 
valuesToCsv <- function(
  x, dec = ".", sep = ",", na = "", qchar = '"', 
  qmethod = c("double", "escape")[1]
)  
{
  #cat(sprintf("\nsep in valuesToCsv(): '%s'\n", sep))
  
  na.indices <- is.na(x)
  
  if (is.factor(x)) {
    
    x <- as.character(x)
  }
  
  x <- if (mode(x) == "numeric") {
    
    # Replace decimal character if required
    if (dec != ".") sub("\\.", dec, x) else x

  } else {
    
    quoteTextIfNeeded(x, sep, qchar, qmethod)
  }
  
  x[na.indices] <- na
  
  x
}

# quoteTextIfNeeded ------------------------------------------------------------

quoteTextIfNeeded <- function(x, sep, qchar, qmethod)
{
  pattern <- sprintf("\\%s|\\%s", sep, qchar)
  
  indices <- grep(pattern, x)
  
  if (length(indices) > 0) {
    
    surrogate <- paste0(if (qmethod == "double") qchar else '\\\\', qchar)
    
    quoted <- gsub(qchar, surrogate, x[indices])
    
    x[indices] <- paste0(qchar, quoted, qchar)    
  }
  
  x
}
