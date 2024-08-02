# getObservationRecordsFromEuLines ---------------------------------------------
getObservationRecordsFromEuLines <- function(
    eu_lines, 
    header.info, 
    dbg,
    file = "",
    ...
)
{
  dot.args <- list(...)
  #dot.args <- list() # for debugging!
  
  observations <- try(silent = TRUE, {
    extractObservationData_1(
      eu_lines, 
      header.info, 
      dbg = dbg
    )
  })
  
  if (kwb.utils::isTryError(observations)) {
    observations <- kwb.utils::callWith(
      extractObservationData_2,
      euLines = eu_lines, 
      headerInfo = getHeaderInfo(eu_lines), 
      header.info = header.info,
      file = file,
      dot.args
    )
  }
  
  kwb.utils::catIf(dbg, paste(nrow(observations), "observations extracted. "))
  
  observations
}
