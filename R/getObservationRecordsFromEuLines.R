# getObservationRecordsFromEuLines ---------------------------------------------
getObservationRecordsFromEuLines <- function(
    eu_lines, 
    header.info, 
    dbg,
    file = "",
    ...
)
{
  observations <- try(
    getObservationsFromEuLines(eu_lines, header.info, dbg = dbg), 
    silent = TRUE
  )
  
  dot.args <- list(...)
  #dot.args <- list() # for debugging!
  
  if (kwb.utils::isTryError(observations)) {
    observations <- kwb.utils::callWith(
      extractObservationData,
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
