# extractObservationData -------------------------------------------------------

#' Extract Observations from EN13508.2-coded file
#'  
#' @param euLines text lines read from EN13508.2-coded file
#' @param headerInfo data frame with information about header lines
#' @param header.info list as returned by
#'   \code{kwb.en13508.2:::getFileHeaderFromEuLines}
#' @return data frame with columns \code{A}, \code{B}, \code{C}, ... as defined 
#'   in EN13508.2 and a column \code{inspno} referring to the inspection number.
extractObservationData <- function(euLines, headerInfo, header.info)
{
  # Create accessor to data frame headerInfo
  fetch <- kwb.utils::createAccessor(headerInfo)
  
  keys <- unique(fetch("uniqueKey")[fetch("type") == "C"])
  
  colClasses <- sapply(
    X = inspectionDataFieldCodes(), 
    FUN = kwb.utils::selectElements, 
    elements = "class"
  )
  
  dataBlocks <- lapply(keys, function(key) {
    
    #key <- keys[1L]
    blocks <- extractObservationBlocks(euLines, headerInfo, key)
    
    rowsWithKey <- which(fetch("uniqueKey") == key)
    
    captionLine <- fetch("value")[rowsWithKey][1L]
    
    text <- c(captionLine, do.call(c, blocks))
    
    blockLengths <- lengths(blocks)
    
    stopifnot(length(text) == sum(blockLengths) + 1L)
    
    result <- readObservationsFromCsvText(
      text = text, 
      sep = header.info$separator, 
      dec = header.info$decimal, 
      quote = header.info$quote, 
      colClasses = colClasses, 
      header = TRUE
    )
    
    result[["inspno"]] <- rep(fetch("inspno")[rowsWithKey], blockLengths)
    
    removeEmptyRecords(result, context = key)
  })
  
  inspectionData <- kwb.utils::safeRowBindAll(dataBlocks)
  inspectionData <- inspectionData[, order(names(inspectionData))]
  inspectionData <- kwb.utils::orderBy(inspectionData, c("inspno", "I"))
  
  kwb.utils::moveColumnsToFront(inspectionData, "inspno")
}

# extractObservationBlocks -----------------------------------------------------

#' Extract Lines Between #C-Header and #Z End Tag
#' 
#' @param euLines text lines read from EN13508.2-coded file
#' @param headerInfo data frame with information about header lines
#' @param uniqueKey identifier of C-header row, as given in 
#'   \code{headerInfo$uniqueKey}
#' @return list of vectors of character representing the "body" lines
#'   below the #C-headers of type specified in \code{uniqueKey}
extractObservationBlocks <- function(euLines, headerInfo, uniqueKey)
{
  kwb.utils::checkForMissingColumns(headerInfo, c("uniqueKey", "type"))
  
  keyMatches <- headerInfo[["uniqueKey"]] == uniqueKey
  x <- headerInfo[keyMatches | headerInfo[["type"]] == "Z", ]
  
  changes <- kwb.utils::findChanges(x$type)
  
  if (changes$value[1L] == "Z") {
    changes <- changes[-1L, ]
  }

  from <- x$row[changes$starts_at[changes$value == "C"]] + 1L
  to <- x$row[changes$starts_at[changes$value == "Z"]] - 1L

  # Add a last "to" value if the last EU-line is not "#Z"
  if (length(to) != length(from)) {
    stopifnot(length(to) == length(from) - 1L)
    to <- c(to, length(euLines))
  }
  
  mapply(
    from = from,
    to = to,
    FUN = function(from, to) euLines[from:to], 
    SIMPLIFY = FALSE
  )
}

# removeEmptyRecords -----------------------------------------------------------
removeEmptyRecords <- function(data, context)
{
  # Convert data frame to a matrix of text values (excluding "inspno")
  x <- as.matrix(kwb.utils::removeColumns(data, "inspno"))
  mode(x) <- "character"

  # Which rows are full of empty text values?  
  isEmpty <- rowSums(kwb.utils::defaultIfNA(nchar(x), 0L)) == 0L
  
  if (any(isEmpty)) {
    message(sprintf(
      paste(
        "Removing %d empty records from observations table (inspection ", 
        "number(s): %s, context: %s)"
      ), 
      sum(isEmpty),
      paste(unique(data[["inspno"]][isEmpty]), collapse = ", "),
      context
    ))
  }
  
  data[!isEmpty, ]
}
