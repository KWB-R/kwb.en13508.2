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
  
  if (kwb.utils::isTryError(observations)) {
    observations <- kwb.utils::callWith(
      extractObservationData,
      euLines = eu_lines, 
      headerInfo = getHeaderInfo(eu_lines), 
      header.info = header.info,
      file = file,
      if (interactive()) list() else list(...)
    )
  }
  
  kwb.utils::catIf(dbg, paste(nrow(observations), "observations extracted. "))
  
  observations
}
