# createInspectionId -----------------------------------------------------------
createInspectionId <- function(
  inspections, 
  id.columns = c(
    "project", 
    "inspection_date", 
    "inspection_time", 
    "node_1_ref", 
    "node_2_ref"
  ),
  n.chars = 8L
)
{
  kwb.utils::checkForMissingColumns(inspections, id.columns)
  
  duplicateInfo <- kwb.utils::findPartialDuplicates(inspections, id.columns)
  
  if (! is.null(duplicateInfo)) {
    print(duplicateInfo)
    stop("There are duplicates in the key columns (see above)!")
  }
  
  keyStrings <- kwb.utils::pasteColumns(inspections, id.columns, "|")
  
  stopifnot(!anyDuplicated(keyStrings))
  
  ids <- kwb.utils::left(unlist(lapply(keyStrings, digest::digest)), n.chars)
  
  stopifnot(!anyDuplicated(ids))
  
  ids
}
