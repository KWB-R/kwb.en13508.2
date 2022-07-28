#' Extract Observations from EN13508.2-coded file
#'  
#' @param euLines text lines read from EN13508.2-coded file
#' @param headerInfo data frame with information about header lines
#' @param header.info list as returned by
#'   \code{kwb.en13508.2:::getHeaderInfoFromHeaderLines}
#' @return data frame with columns \code{A}, \code{B}, \code{C}, ... as defined 
#'   in EN13508.2 and a column \code{inspno} referring to the inspection number.
extractObservationData <- function(euLines, headerInfo, header.info)
{
  kwb.utils::checkForMissingColumns(headerInfo, c("uniqueKey", "type", "value"))
  
  uniqueKeys <- unique(headerInfo[["uniqueKey"]][headerInfo[["type"]] == "C"])
  
  colClasses <- sapply(
    inspectionDataFieldCodes(), kwb.utils::selectElements, "class"
  )
  
  dataBlocks <- lapply(uniqueKeys, function(uniqueKey) {
    
    blocks <- extractObservationBlocks(euLines, headerInfo, uniqueKey)
    
    rowsWithKey <- which(headerInfo[["uniqueKey"]] == uniqueKey)
    
    captionLine <- headerInfo[["value"]][rowsWithKey][1L]
    
    text <- c(captionLine, do.call(c, blocks))
    
    blockLengths <- lengths(blocks)
    
    stopifnot(length(text) == sum(blockLengths) + 1L)
    
    #result <- read.table(text = text, sep = ";", header = TRUE)
    
    result <- readObservationsFromCsvText(
      text = text, 
      sep = header.info$separator, 
      dec = header.info$decimal, 
      quote = header.info$quote, 
      colClasses = colClasses, 
      header = TRUE
    )
    
    result$inspno <- rep(headerInfo$inspno[rowsWithKey], blockLengths)
    
    result
  })
  
  inspectionData <- kwb.utils::safeRowBindAll(dataBlocks)
  
  inspectionData <- inspectionData[, order(names(inspectionData))]
  
  inspectionData <- kwb.utils::orderBy(inspectionData, c("inspno", "I"))
  
  kwb.utils::moveColumnsToFront(inspectionData, "inspno")
}

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
  
  mapply(
    from = x$row[changes$starts_at[changes$value == "C"]] + 1L,
    to = x$row[changes$starts_at[changes$value == "Z"]] - 1L,
    FUN = function(from, to) euLines[from:to], 
    SIMPLIFY = FALSE
  )
}
