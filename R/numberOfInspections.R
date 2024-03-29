#' Number of Inspections
#' 
#' Get number of inspections from list of inspection data
#' 
#' @param x list of inspection data elements each of which was read from an EN
#'   13508-2-coded file by means of \code{\link{readEuCodedFile}}
#'   
#' @return vector of integer representing the number of inspections in each
#'   element of \code{inspectionDataList}
#' 
#' @export
#' 
numberOfInspections <- function(x)
{
  sapply(x, function(xx) nrow(get_elements(xx, "inspections")))
}
