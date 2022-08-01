# getLineDamageInfo ------------------------------------------------------------

#' Get Information on Line Damages
#' 
#' @param observations data frame with observations. Required columns: \code{I}
#'   (= horizontal or vertical position), \code{J} (= code for line damage),
#'   \code{inspno} (number of inspection to which the observation belongs)
#' @param dbg if \code{TRUE}, debug messages are shown
#' @return data frame with columns \code{ino} (inspection number), \code{ldno}
#'   (line damage number), \code{beg.at}, \code{end.at}, \code{beg.x} (position
#'   of line damage begin), \code{end.x} (position of line damage end),
#'   \code{length} (length of line damage)
#' @importFrom kwb.utils selectColumns setColumns
#' @export
getLineDamageInfo <- function(observations, dbg = TRUE)
{
  getcol <- kwb.utils::selectColumns
  
  if (! "J" %in% names(observations)) {
    message("No column 'J' (line damages) found in table of observations.")
    return(NULL)
  }
  
  I <- asNumericIfRequired(getcol(observations, "I"), dbg = dbg)
  
  J <- getcol(observations, "J")
  
  # Check if the values in J match the expected pattern
  stopOnInvalidLineDamageCodes(J)
  
  # Split line damage identifier in J into "A" or "B" (ld) and number (ldno)
  x <- kwb.utils::noFactorDataFrame(
    ino = getcol(observations, "inspno"),
    ld = substr(J, 1, 1),
    ldno = substr(J, 2, nchar(J))
  )
  
  info <- merge(
    x = aggregateAndFilter(x, FUN = min, name = "beg.at"), 
    y = aggregateAndFilter(x, FUN = max, name = "end.at")
  )
  
  # Order by inspection number and line damage number
  info <- info[do.call(order, info[, c("ino", "ldno")]), ]
  
  kwb.utils::setColumns(
    info, 
    beg.x = I[getcol(info, "beg.at")], 
    end.x = I[getcol(info, "end.at")],
    length = I[getcol(info, "end.at")] - I[getcol(info, "beg.at")], 
    dbg = FALSE
  )
}

# stopOnInvalidLineDamageCodes -------------------------------------------------

#' @importFrom kwb.utils stopFormatted stringList
stopOnInvalidLineDamageCodes <- function(J, pattern = "^$|^[AB]\\d+$")
{
  unique_values <- unique(J)
  
  is_invalid <- ! grepl(pattern, unique_values)
  
  if (any(is_invalid)) {
    
    kwb.utils::stopFormatted(
      "There are line damage codes in column 'J' that do not match '%s':\n%s",
      pattern, kwb.utils::stringList(unique_values[is_invalid])
    )
  }
}

# aggregateAndFilter -----------------------------------------------------------
aggregateAndFilter <- function(x, FUN, name) 
{
  result <- stats::aggregate(seq_len(nrow(x)), by = x[, c("ino", "ldno")], FUN)
  
  names(result)[ncol(result)] <- name
  
  result[result$ldno != "", ]
}
