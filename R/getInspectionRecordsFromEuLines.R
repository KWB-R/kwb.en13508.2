# getInspectionRecordsFromEuLines ----------------------------------------------
getInspectionRecordsFromEuLines <- function(
    eu_lines, header.info, read.inspections, simple.algorithm, dbg
)
{
  if (!read.inspections) {
    warning(
      "I (yet) cannot read the inspection data (#B-blocks). ",
      "So I just returned the number of inspections instead of a ",
      "data frame with all information on the inspection!"
    )
    return(length(grep("^#B01", eu_lines)))
  }
  
  inspections <- if (simple.algorithm) {
    extractInspectionData_v1(eu_lines, header.info, dbg = dbg > 1L)
  } # else NULL
  
  # If the inspections could not be read with the simple algorithm (due to
  # changing header rows) or if the user requests it, try it again with
  # another algorithm
  if (is.null(inspections)) {
    inspections <- extractInspectionData_v2(
      eu_lines, header.info, dbg = dbg
    )
  }
  
  kwb.utils::catIf(dbg, paste(nrow(inspections), "inspections extracted. "))
  
  inspections
}
