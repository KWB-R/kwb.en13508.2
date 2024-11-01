# extractObservationData -------------------------------------------------------
extractObservationData <- function(
    text, header.info, dbg = TRUE, file = "", ...
)
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
      do.call(extractObservationData_2, c(dot.args, list(
        text = text, 
        headerInfo = getHeaderInfo(text), 
        header.info = header.info,
        file = file,
        dbg = dbg
      )))
    }
  )
  
  kwb.utils::catIf(dbg, paste(nrow(observations), "observations extracted. "))
  
  observations
}
