#' Set Global Inspection ID
#'
#' Convert inspections numbers (inspno) 1,2,3,... to globally unique identifiers
#' (inspid), such as "e4b48d86"
#'
#' @param inspection.data list with elements \code{header.info},
#'   \code{inspections}, \code{observations}
#' @param project name of project to which the data are related, such as:
#'   "Lausanne"
#' @return list with the same elements as in \code{inspection.data} but with
#'   columns \code{inspid} being added to the data frames "inspections" and
#'   "observations"
setGlobalInspectionID <- function(inspection.data, project = NULL)
{
  if (is.null(project)) {
    stop(
      "Please specify the 'project' name (e.g. name of city or data provider)", 
      call. = FALSE
    )
  }
  
  header.info <- kwb.utils::selectElements(inspection.data, "header.info")
  inspections <- kwb.utils::selectElements(inspection.data, "inspections")
  observations <- kwb.utils::selectElements(inspection.data, "observations")

  inspections <- kwb.utils::removeEmptyColumns(inspections)
  inspections[["Project"]] <- project
  inspections[["inspid"]] <- createInspectionId(inspections)

  observations <- kwb.utils::removeEmptyColumns(observations)
  i <- kwb.utils::selectColumns(observations, "inspno")
  observations[["inspid"]] <- kwb.utils::selectColumns(inspections, "inspid")[i]
  observations <- kwb.utils::removeColumns(observations, "inspno")

  list(
    header.info = header.info,
    inspections = kwb.utils::moveColumnsToFront(inspections, "inspid"),
    observations = kwb.utils::moveColumnsToFront(observations, "inspid")
  )
}
