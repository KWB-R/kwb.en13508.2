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
#' @importFrom kwb.utils orderBy selectColumns setColumns
#' @export
getLineDamageInfo <- function(observations, dbg = TRUE)
{
  if (! "J" %in% names(observations)) {
    message("No column 'J' (line damages) found in table of observations.")
    return(NULL)
  }

  fetch <- kwb.utils::createAccessor(observations)
  
  I <- asNumericIfRequired(fetch("I"), dbg = dbg)
  J <- fetch("J")
  
  # Check if the values in J match the expected pattern
  stopOnInvalidLineDamageCodes(J)
  
  # Split line damage identifier in J into "A" or "B" (ld) and number (ldno)
  x <- kwb.utils::noFactorDataFrame(
    ino = fetch("inspno"),
    ld = substr(J, 1L, 1L),
    ldno = substr(J, 2L, nchar(J))
  )
  
  info <- merge(
    x = aggregateAndFilter(x, FUN = min, name = "beg.at"), 
    y = aggregateAndFilter(x, FUN = max, name = "end.at")
  )
  
  # Order by inspection number and line damage number
  info <- kwb.utils::orderBy(info, c("ino", "ldno"))
  
  fetch <- kwb.utils::createAccessor(info)
  
  beg_x <- I[fetch("beg.at")]
  end_x <- I[fetch("end.at")]
  
  kwb.utils::setColumns(
    info, 
    beg.x = beg_x, 
    end.x = end_x,
    length = end_x - beg_x, 
    dbg = FALSE
  )
}

# stopOnInvalidLineDamageCodes -------------------------------------------------

#' @importFrom kwb.utils stopFormatted stringList
stopOnInvalidLineDamageCodes <- function(J, pattern = "^$|^[ABC]\\d+$")
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
