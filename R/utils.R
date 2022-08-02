# asNumericIfRequired ----------------------------------------------------------
asNumericIfRequired <- function(x, name = deparse(substitute(x)), dbg = TRUE)
{
  if (is.numeric(x)) {
    return(x)
  }  

  kwb.utils::catAndRun(
    messageText = sprintf("Converting '%s' to numeric", name),
    expr = as.numeric(x),
    dbg = dbg
  )
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

# get_columns ------------------------------------------------------------------
get_columns <- kwb.utils::selectColumns

# get_elements -----------------------------------------------------------------
get_elements <- kwb.utils::selectElements

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

