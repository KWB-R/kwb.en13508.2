# getLineDamageInfo ------------------------------------------------------------

#' Get Information on Line Damages
#' 
#' @param observations data frame with observations. Required columns: \code{I}
#'   (= horizontal or vertical position), \code{J} (= code for line damage),
#'   \code{inspno} (number of inspection to which the observation belongs)
#' @param I Numeric vector of positions of observations
#' @param J Vector of character of same length as \code{I} containing the codes 
#'   used to indicate continuous defects
#' @param inspno vector of inspection identifiers of same length as \code{I}
#' @param dbg if \code{TRUE}, debug messages are shown
#' @return data frame with columns \code{ino} (inspection number), \code{ldno}
#'   (line damage number), \code{beg.at}, \code{end.at}, \code{beg.x} (position
#'   of line damage begin), \code{end.x} (position of line damage end),
#'   \code{length} (length of line damage)
#' @importFrom kwb.utils orderBy selectColumns
#' @export
getLineDamageInfo <- function(
    observations, 
    I = observations$I, 
    J = observations$J, 
    inspno = observations$inspno,
    dbg = TRUE
)
{
  if (is.null(I)) {
    stop("No positions of observations given in 'I'.", call. = FALSE)
  }

  if (is.null(inspno)) {
    stop("No inspection identifiers given in 'inspno'.", call. = FALSE)
  }
  
  if (is.null(J)) {
    message("No line damage codes given in 'J'.")
    return(NULL)
  }

  I <- asNumericIfRequired(I, dbg = dbg)

  # Check if the values in J match the expected pattern
  stopOnInvalidLineDamageCodes(J)
  
  # Split line damage identifier in J into "A" or "B" (ld) and number (ldno)
  x <- data.frame(
    ino = inspno,
    ld = substr(J, 1L, 1L),
    ldno = substr(J, 2L, nchar(J)),
    stringsAsFactors = FALSE
  )

  aggregateAndFilter <- function(x, FUN, name) {
    result <- stats::aggregate(
      seq_len(nrow(x)), 
      by = x[, c("ino", "ldno")], 
      FUN
    )
    names(result)[ncol(result)] <- name
    result[result$ldno != "", ]
  }
  
  info <- merge(
    x = aggregateAndFilter(x, FUN = min, name = "beg.at"), 
    y = aggregateAndFilter(x, FUN = max, name = "end.at")
  )
  
  info$beg.x <- I[kwb.utils::selectColumns(info, "beg.at")] 
  info$end.x <- I[kwb.utils::selectColumns(info, "end.at")]
  info$length <- info$end.x - info$beg.x
  
  # Order by inspection number and line damage number
  kwb.utils::orderBy(info, c("ino", "ldno"))
}

# stopOnInvalidLineDamageCodes -------------------------------------------------

#' @importFrom kwb.utils stopFormatted stringList
stopOnInvalidLineDamageCodes <- function(J, pattern = "^$|^[ABC]\\d+$")
{
  if (any(is_invalid <- !grepl(pattern, (unique_values <- unique(J))))) {
    kwb.utils::stopFormatted(
      "There are line damage codes in column 'J' that do not match '%s':\n%s",
      pattern, kwb.utils::stringList(unique_values[is_invalid])
    )
  }
}
