# extractInspectionData_v1 -----------------------------------------------------
extractInspectionData_v1 <- function(text, header.info, dbg = TRUE)
{
  b_indices <- function(b_number) {
    grep(sprintf("^#B%02d", b_number), text)
  }
  
  inspections <- NULL
  b_number <- 1L
  sep <- kwb.utils::selectElements(header.info, "separator")
  
  while (length(indices <- b_indices(b_number))) {
    
    captions <- strsplit(getValueFromKeyValueString(text[indices]), sep)
    
    if (!kwb.utils::allAreEqual(captions)) {
      if (dbg) {
        message(
          "The #B-header lines differ within the file -> I will change the ",
          "algorithm..."
        )
      }
      return(NULL)
    }

    if (any(indices == length(text))) {
      stop("No value line available after #B-header line (end of text block)!")
    }
    
    partial_inspections <- utils::read.table(
      text = paste(text[indices + 1L], collapse = "\n"),
      sep = sep, 
      dec = kwb.utils::selectElements(header.info, "decimal"), 
      quote = kwb.utils::selectElements(header.info, "quote"),
      comment.char = "",
      stringsAsFactors = FALSE
    )
    
    names(partial_inspections) <- captions[[1L]]
    inspections <- kwb.utils::safeColumnBind(inspections, partial_inspections)
    b_number <- b_number + 1L
  }  
  
  inspections
}
