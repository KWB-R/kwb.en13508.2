# getInspectionsFromEuLines.new ------------------------------------------------

getInspectionsFromEuLines.new <- function(eu.lines, header.info, dbg = TRUE)
{
  headerInfos <- getInspectionHeaderInfo(eu.lines)
  
  inspectionBlocks <- extractInspectionBlocks(
    eu.lines = eu.lines, 
    headerInfos = headerInfos, 
    sep = header.info$separator, 
    dec = header.info$decimal, 
    quoteCharacter = header.info$quote, 
    dbg = dbg
  )
  
  x <- mergeInspectionBlocks(inspectionBlocks)
  
  inspectionNumbers <- seq_len(nrow(x))

  B.rows <- data.frame(inspno = inspectionNumbers, rows = x$row)
  
  structure(kwb.utils::removeColumns(x,  "row"), B.rows = B.rows)
}

# getInspectionHeaderInfo ------------------------------------------------------

getInspectionHeaderInfo <- function(eu.lines)
{
  pattern <- "^#B(\\d\\d)=(.*)$"
  
  matchInfo <- regexec(pattern, eu.lines)
  
  # indices of header lines
  header.indices <- which(sapply(matchInfo, "[", 1) != -1)
  
  # Number of header (#B01 = 1, #B02 = 2)
  headerNumbers <- as.numeric(
    getMatchingElements(eu.lines, matchInfo, header.indices, position = 2)
  )
  
  # Only the header (right of equal sign)
  headerLines <- getMatchingElements(
    eu.lines, matchInfo, header.indices, position = 3
  )
  
  uniqueHeaders <- unique(headerLines)
  
  # For each different type of header, determine the line numbers in which it
  # occurs
  headerRows <- lapply(uniqueHeaders, function(x) {
    
    indices <- which(headerLines == x)
    
    headerNumber <- unique(headerNumbers[indices])
    
    stopifnot(length(headerNumber) == 1)
    
    list(line = headerNumber, rows = header.indices[indices])
  })
  
  stats::setNames(headerRows, uniqueHeaders)
}

# getMatchingElements ----------------------------------------------------------

getMatchingElements <- function(x, matchInfo, indices, position)
{
  matches <- matchInfo[indices]  
  
  startpos <- sapply(matches, "[", position)
  
  stringlength <- sapply(matches, function(x) attr(x, "match.length")[position])
  
  substr(x[indices], start = startpos, stop = startpos + stringlength - 1)
}

# extractInspectionBlocks ------------------------------------------------------

extractInspectionBlocks <- function(
  eu.lines, headerInfos, sep, dec, quoteCharacter, dbg = TRUE
)
{
  inspectionBlocks <- list()
  
  uniqueHeaders <- names(headerInfos)
  
  for (i in seq_len(length(headerInfos))) {
    
    rowNumbers <- headerInfos[[i]]$rows + 1
    
    textlines <- eu.lines[rowNumbers]
    
    x <- textblockToDataframe(
      textblock = paste(textlines, collapse = "\n"), sep = sep, dec = dec,
      quoteCharacter = quoteCharacter, captionLine = uniqueHeaders[i],
      rowNumbers = rowNumbers, dbg = dbg
    )
    
    lineNumber <- headerInfos[[i]]$line
    
    if (length(inspectionBlocks) < lineNumber) {
      
      inspectionBlocks[[lineNumber]] <- list(
        line = lineNumber, dataFrames = list()
      )
    }
    
    lastIndex <- length(inspectionBlocks[[lineNumber]]$dataFrames)
    
    inspectionBlocks[[lineNumber]]$dataFrames[[lastIndex + 1]] <- x
  }
  
  inspectionBlocks
}

# textblockToDataframe ---------------------------------------------------------

textblockToDataframe <- function(
  textblock, sep, dec, quoteCharacter, captionLine, rowNumbers, dbg = TRUE
)
{
  x <- kwb.utils::csvTextToDataFrame(
    textblock, sep = sep, dec = dec, quote = quoteCharacter, comment.char = "",
    stringsAsFactors = FALSE
  )
  
  captions <- strsplit(captionLine, sep)[[1]]
  
  # the number of captions must be equal to the number of columns in x
  if (length(captions) != ncol(x)) {
    
    textmessage <- sprintf(
      paste0(
        "The number of captions (%d) is not equal to the number of columns ",
        "in the data block (%d). \nCaptions: %s\nFirst data row: %s\n"
      ), 
      length(captions), ncol(x), captions, x[1, ]
    )
    
    stop(textmessage)
    
  } else {
    
    names(x) <- captions
  }
  
  # Check for duplicated columns and remove duplicated columns if all values 
  # within the columns are identical to the corresponding values in the original
  # column
  x <- removeDuplicatedColumns(x, dbg = dbg)
  
  # There may still be columns with duplicated names (with differing values)
  # give unique names by appending ".1", ".2"
  names(x) <- kwb.utils::makeUnique(names(x), warn = FALSE)
  
  x$row <- rowNumbers
  
  x
}

# determineColumnsToRemove -----------------------------------------------------

determineColumnsToRemove <- function(x, captions, duplicates, dbg = TRUE)
{
  columnsToRemove <- numeric()
  
  for (duplicate in duplicates) {
    
    message("Column '", duplicate, "' exists multiple times!")

    columns <- which(captions == duplicate)    
    
    allEqualInRow <- apply(x[, columns], MARGIN = 1, kwb.utils::allAreEqual)
    
    if (all(allEqualInRow)) {
      
      columnsToRemove <- c(columnsToRemove, columns[-1])
      
      message(
        "For each row, the values in the duplicated rows are equal ",
        "-> I removed the duplicated columns!"
      )
      
      if (dbg) {
        
        cat("The values in the duplicated columns are:\n")
        
        x.output <- x[, columns]
        
        print(x.output[! duplicated(x.output), ])
      }
    }    

    kwb.utils::catIf(
      dbg && length(columnsToRemove) > 0, 
      "columnsToRemove:", kwb.utils::stringList(columnsToRemove), "\n"
    )
  }    
  
  # vector of numbers of columns to be removed
  columnsToRemove
}

# mergeInspectionBlocks --------------------------------------------------------

mergeInspectionBlocks <- function(inspectionBlocks)
{
  indices <- seq_along(inspectionBlocks)
  
  # Loop through the inspection data blocks
  for (i in indices) {
    
    result <- kwb.utils::safeRowBindAll(inspectionBlocks[[i]]$dataFrames)
    
    # Order by row numbers of header lines
    result <- result[order(result$row), ]
    
    if (i == 1) {
      
      results <- result
      
    } else {
      
      # Prepare column "row" of the current result data frame for merging with 
      # the current header block (#B02 if i == 2 or #B03 if i == 3 or ...)
      results$row <- results$row + 2
      
      # Before merging, check for columns that are in both, result and results
      commonColumns <- setdiff(intersect(names(results), names(result)), "row")
      
      if (length(commonColumns) > 0) {
        
        names(result) <- sapply(
          names(result), 
          kwb.utils::hsSafeName, 
          myNames = setdiff(names(results), "row")
        )
        
        message(sprintf(
          "Column(s) %s were given unique names.", 
          kwb.utils::stringList(commonColumns)
        ))
      }
      
      results <- merge(results, result, by = "row", all.x = TRUE)
      
      # If columns of the same name exist in both tables merged, they exist in
      # results with suffixes ".x" and ".y". Check if such columns exist and
      # remove duplicated columns if they contain duplicated values
      #results <- cleanDuplicatedColumns(results)
    }
  }
  
  # Order columns by name but put "row" column first
  results[, kwb.utils::moveToFront(sort(names(results)), "row")]  
}

# removeDuplicatedColumns ------------------------------------------------------

removeDuplicatedColumns <- function(x, dbg = TRUE)
{
  captions <- names(x)
  
  duplicates <- unique(captions[duplicated(captions)])
  
  if (length(duplicates) > 0) {
    
    columnsToRemove <- determineColumnsToRemove(
      x, captions, duplicates, dbg = dbg
    )
    
    # if there is any column to remove, remove it
    if (length(columnsToRemove) > 0) {
      
      message(
        "Removing columns: ", kwb.utils::stringList(captions[columnsToRemove])
      )
      
      x <- x[, -columnsToRemove]
      
    } else {
      
      message("There are different values in columns with the same name: ")
      
      x.output <- x[, names(x) %in% duplicates, drop = FALSE]
      
      print(x.output[!duplicated(x.output), ])
    }
  }  

  x
}

# cleanDuplicatedColumns -------------------------------------------------------

cleanDuplicatedColumns <- function(x)
{
  captions <- names(x)
  
  # indices of captions ending with ".x" or ".y"
  indices <- lapply(paste0("\\.", c("x", "y"), "$"), grep, captions)
  
  if (length(all_indices <- unlist(indices))) {
    
    #message("There are columns with suffixes '.x' or '.y'")

    if (length(indices[[1]]) != length(indices[[2]])) stop(
      "Missing columns with suffix '.x' or '.y':\n",
      kwb.utils::stringList(captions[all_indices])
    )
    
    # Remove suffixes ".x" or ".y" -> produce duplicated column names
    captions[all_indices] <- kwb.utils::removeExtension(captions[all_indices])
    
    x <- removeDuplicatedColumns(stats::setNames(x, captions))
    
  }
  
  x
}
