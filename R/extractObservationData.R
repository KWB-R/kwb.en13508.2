# extractObservationData -------------------------------------------------------
extractObservationData <- function(text, header.info, dbg, file = "", ...)
{
  dot.args <- list(...)
  #dot.args <- list() # for debugging!
  
  observations <- tryCatch(
    silent = TRUE, 
    expr = {
      extractObservationData_1(
        text = text, 
        header.info = header.info, 
        dbg = dbg
      )
    }, 
    error = {
      kwb.utils::callWith(
        extractObservationData_2,
        text = text, 
        headerInfo = getHeaderInfo(text), 
        header.info = header.info,
        file = file,
        dot.args
      )      
    }
  )
  
  kwb.utils::catIf(dbg, paste(nrow(observations), "observations extracted. "))
  
  observations
}
