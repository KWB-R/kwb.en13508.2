# getHeaderInfo ----------------------------------------------------------------
getHeaderInfo <- function(euLines)
{
  pattern <- paste0("^#", c("A", "B", "C", "Z"), collapse = "|")
  
  headerIndices <- grep(pattern, euLines)
  
  headerLines <- euLines[headerIndices]
  
  keyValue = strsplit(headerLines, "=")
  
  keys <- sapply(keyValue, "[", 1L)
  
  values <- character(length(keys))
  hasValue <- lengths(keyValue) > 1L
  values[hasValue] <- sapply(keyValue[hasValue], "[", 2L)
  
  headerInfo <- kwb.utils::noFactorDataFrame(
    row = headerIndices,
    type = substr(headerLines, 2L, 2L),
    key = keys,
    uniqueKey = "",
    value = values
  )
  
  setUniqueKey <- function(data, type) {
    isType <- data$type == type
    uniqueValues <- unique(data$value[isType])
    key <- paste0(tolower(type), match(data$value[isType], uniqueValues))
    data$uniqueKey[isType] <- key
    data
  }
  
  headerInfo <- setUniqueKey(headerInfo, "B")  
  headerInfo <- setUniqueKey(headerInfo, "C")  

  # Set inspection number in column "inspno"
  changes <- kwb.utils::findChanges(headerInfo$type)
  bStarts <- changes$starts_at[changes$value == "B"]
    
  inspectionNumbers <- rep(NA_integer_, nrow(headerInfo))
  inspectionNumbers[bStarts] <- seq_along(bStarts)
  inspectionNumbers <- kwb.utils::naToLastNonNa(inspectionNumbers)
  
  headerInfo[["inspno"]] <- inspectionNumbers
  
  headerInfo
}
