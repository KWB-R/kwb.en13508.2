# getObservationRecordsFromEuLines ---------------------------------------------
getObservationRecordsFromEuLines <- function(eu_lines, header.info, dbg)
{
  observations <- try(
    getObservationsFromEuLines(eu_lines, header.info, dbg = dbg), 
    silent = TRUE
  )
  
  if (kwb.utils::isTryError(observations)) {
    headerInfo <- getHeaderInfo(eu_lines)
    observations <- extractObservationData(eu_lines, headerInfo, header.info)
  }
  
  kwb.utils::catIf(dbg, paste(nrow(observations), "observations extracted. "))
  
  observations
}
