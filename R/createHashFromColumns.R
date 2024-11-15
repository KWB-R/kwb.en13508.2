# createHashFromColumns --------------------------------------------------------
createHashFromColumns <- function(
    data, columns, nchars = 8L, silent = FALSE, makeUnique = FALSE
)
{
  duplicates <- kwb.utils::findPartialDuplicates(data, columns)
  
  if (!is.null(duplicates)) {
    if (!silent) {
      message(
        "Cannot create unique hashes due to duplicates in the key columns (",
        kwb.utils::stringList(columns), 
        ")! "
      )
      if (makeUnique) {
        message("I will make the hashes unique.")
      } else {
        message("Returning -1L. Check attribute 'duplicates'.")
      }
    }
    if (!makeUnique) {
      return(structure(-1L, duplicates = duplicates))
    }
  }
  
  keys <- kwb.utils::pasteColumns(data, columns, "|")

  if (!makeUnique) {
    stopifnot(!anyDuplicated(keys))
  }
  
  hashes <- kwb.utils::left(unlist(lapply(keys, digest::digest)), nchars)
  
  if (makeUnique) {
    hashes <- kwb.utils::makeUnique(hashes, warn = FALSE)
  }
  
  stopifnot(!anyDuplicated(hashes))

  hashes
}
