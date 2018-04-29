# getInspectionsFromEuLines.new ------------------------------------------------

getInspectionsFromEuLines.new <- function(eu_lines, header.info, dbg = TRUE)
{
  x <- mergeInspectionBlocks(extractInspectionBlocks(
    eu_lines = eu_lines, 
    headerInfos = getInspectionHeaderInfo(eu_lines), 
    sep = get_elements(header.info, "separator"), 
    dec = get_elements(header.info, "decimal"), 
    quoteCharacter = get_elements(header.info, "quote"), 
    dbg = dbg
  ))

  structure(
    kwb.utils::removeColumns(x, "row"), 
    B.rows = data.frame(inspno = seq_len(nrow(x)), rows = x$row)
  )
}

# getInspectionHeaderInfo ------------------------------------------------------

getInspectionHeaderInfo <- function(eu_lines)
{
  # Get list of matching sub expressions
  matches <- kwb.utils::subExpressionMatches("^#B(\\d\\d)=(.*)$", eu_lines)
  
  # Indices of header lines
  header_indices <- which(! sapply(matches, is.null))
  
  # Keep only the sub expressions of matching rows
  matches <- matches[header_indices]
  
  # Number of header (#B01 = 1, #B02 = 2)
  header_numbers <- as.numeric(sapply(matches, "[[", 1))

  # Only the header (right of equal sign)
  header_lines <- sapply(matches, "[[", 2)

  unique_headers <- unique(header_lines)
  
  # For each different type of header, determine the line numbers in which it
  # occurs
  header_rows <- lapply(unique_headers, function(header) {
    
    indices <- which(header_lines == header)
    
    header_number <- unique(header_numbers[indices])
    
    stopifnot(length(header_number) == 1)
    
    list(line = header_number, rows = header_indices[indices])
  })
  
  stats::setNames(header_rows, unique_headers)
}

# extractInspectionBlocks ------------------------------------------------------

extractInspectionBlocks <- function(
  eu_lines, headerInfos, sep, dec, quoteCharacter, dbg = TRUE
)
{
  blocks <- list()
  
  unique_headers <- names(headerInfos)
  
  for (i in seq_along(headerInfos)) {
    
    row_numbers <- headerInfos[[i]]$rows + 1
    
    x <- textblockToDataframe(
      textblock = paste(eu_lines[row_numbers], collapse = "\n"), 
      sep = sep, dec = dec, quoteCharacter = quoteCharacter, 
      captionLine = unique_headers[i], rowNumbers = row_numbers, dbg = dbg
    )
    
    line_number <- headerInfos[[i]]$line
    
    if (length(blocks) < line_number) {
      
      blocks[[line_number]] <- list(line = line_number, dataFrames = list())
    }
    
    last_index <- length(blocks[[line_number]]$dataFrames)
    
    blocks[[line_number]]$dataFrames[[last_index + 1]] <- x
  }
  
  blocks
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
  
  kwb.utils::setColumns(x, row = rowNumbers, dbg = FALSE)
}

# getColumnsToRemove -----------------------------------------------------------

getColumnsToRemove <- function(x, captions, duplicates, dbg = TRUE)
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
    
    columns <- getColumnsToRemove(x, captions, duplicates, dbg = dbg)
    
    # if there is any column to remove, remove it
    if (length(columns)) {
      
      message("Removing columns: ", kwb.utils::stringList(captions[columns]))
      
      x <- x[, -columns]
      
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
