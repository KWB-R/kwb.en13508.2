# getInspectionRecords_v1 ------------------------------------------------------
getInspectionRecords_v1 <- function(eu_lines, header.info, dbg = TRUE)
{
  inspections.complete <- NULL
  
  header.line.number <- 1L
  
  continue <- TRUE
  
  indices.B <- grep("^#B01", eu_lines)
  
  aborted <- FALSE
  
  while (! aborted && length(indices.B) > 0L) {
    
    b.caption.lines <- getValueFromKeyValueString(eu_lines[indices.B])
    
    b.captions <- strsplit(b.caption.lines, header.info$separator)
    
    if (kwb.utils::allAreEqual(b.captions)) {
      
      inspections <- extractInspectionData(
        b.lines = eu_lines[indices.B + 1L],
        header.info = header.info,
        captions = b.captions[[1L]]
      )
      
      inspections.complete <- kwb.utils::safeColumnBind(
        inspections.complete, inspections
      )
      
    } else {
      
      if (dbg) {
        message(
          "The #B-header lines differ within the file -> I will change the ",
          "algorithm..."
        )
      }
      
      aborted <- TRUE
    }    
    
    header.line.number <- header.line.number + 1L
    
    indices.B <- grep(sprintf("^#B%02d", header.line.number), eu_lines)
  }  
  
  if (aborted) {
    return(NULL)
  }
  
  inspections.complete
}

# extractInspectionData --------------------------------------------------------
extractInspectionData <- function(b.lines, header.info, captions)
{
  inspections <- kwb.utils::csvTextToDataFrame(
    text = paste(b.lines, collapse = "\n"),
    sep = header.info$separator, 
    dec = header.info$decimal, 
    quote = header.info$quote,
    comment.char = "",
    stringsAsFactors = FALSE
  )
  
  stats::setNames(inspections, captions)
}
