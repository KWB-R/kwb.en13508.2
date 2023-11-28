# getObservationRecordsFromEuLines ---------------------------------------------
getObservationRecordsFromEuLines <- function(
    eu_lines, 
    header.info, 
    dbg,
    file = ""
)
{
  observations <- try(
    getObservationsFromEuLines(eu_lines, header.info, dbg = dbg), 
    silent = TRUE
  )
  
  if (kwb.utils::isTryError(observations)) {
    headerInfo <- getHeaderInfo(eu_lines)
    observations <- extractObservationData(
      eu_lines, 
      headerInfo, 
      header.info,
      file = file
    )
  }
  
  kwb.utils::catIf(dbg, paste(nrow(observations), "observations extracted. "))
  
  observations
}
