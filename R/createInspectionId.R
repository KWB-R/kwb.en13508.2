# createInspectionId -----------------------------------------------------------
createInspectionId <- function(
  inspections, 
  id.columns = c("project", "InspDate", "InspTime", "Node1Ref", "Node2Ref"),
  n.chars = 8L
)
{
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
