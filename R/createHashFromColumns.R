# createHashFromColumns --------------------------------------------------------
createHashFromColumns <- function(data, columns, n.chars = 8L)
{
  duplicates <- kwb.utils::findPartialDuplicates(data, columns)
  
  if (!is.null(duplicates)) {
    print(duplicates)
    stop("There are duplicates in the key columns (see above)!")
  }
  
  keys <- kwb.utils::pasteColumns(data, columns, "|")
  
  stopifnot(!anyDuplicated(keys))
  
  hashes <- kwb.utils::left(unlist(lapply(keys, digest::digest)), n.chars)
  
  stopifnot(!anyDuplicated(hashes))
  
  hashes
}
