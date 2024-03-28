inspectionDataFieldCodes <- function()
{
  codeInfo <- readPackageFile("eucodes_de.csv")
  
  codes <- get_columns(codeInfo, "Code")
  
  columns <- c("class", "meaning")
  
  lapply(kwb.utils::toNamedList(codes), function(code) {
    as.list(get_columns(codeInfo[codes == code, ], columns))
  })
}
