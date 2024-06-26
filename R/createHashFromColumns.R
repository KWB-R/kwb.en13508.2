# createHashFromColumns --------------------------------------------------------
createHashFromColumns <- function(data, columns, nchars = 8L, silent = FALSE)
{
  duplicates <- kwb.utils::findPartialDuplicates(data, columns)
  
  if (!is.null(duplicates)) {
    
    if (!silent) {
      message(
        "Cannot create unique hashes due to duplicates in the key columns (",
        kwb.utils::stringList(columns), 
        ")! Returning -1L. Check attribute 'duplicates'."
      )
    }
    
    return(structure(-1L, duplicates = duplicates))
  }
  
  keys <- kwb.utils::pasteColumns(data, columns, "|")
  
  stopifnot(!anyDuplicated(keys))
  
  hashes <- kwb.utils::left(unlist(lapply(keys, digest::digest)), nchars)
  
  stopifnot(!anyDuplicated(hashes))
  
  hashes
}
