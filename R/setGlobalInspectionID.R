#' Set Global Inspection ID
#'
#' Convert inspections numbers (inspno) 1,2,3,... to globally unique identifiers
#' (inspid), such as "e4b48d86"
#'
#' @param inspection.data list with elements \code{header.info},
#'   \code{inspections}, \code{observations}
#' @param project name of project to which the data are related, such as:
#'   "Lausanne"
#' @param default.time default time string to use if column InspTime is not
#'   available. Default: "12:00". A random number will be generated for the 
#'   seconds, just to increase the chance that setting the time is enough to
#'   generate a unique key.
#' @return list with the same elements as in \code{inspection.data} but with
#'   columns \code{inspid} being added to the data frames "inspections" and
#'   "observations"
setGlobalInspectionID <- function(
    inspection.data, 
    project = NULL, 
    default.time = "12:00"
)
{
  if (is.null(project)) {
    stop(
      "Please specify the 'project' name (e.g. name of city or data provider)", 
      call. = FALSE
    )
  }
  
  fetch <- kwb.utils::createAccessor(inspection.data)
  
  # Just a shortcut
  removeEmpty <- function(df) kwb.utils::removeEmptyColumns(df, dbg = FALSE)

  inspections <- removeEmpty(fetch("inspections"))
  observations <- removeEmpty(fetch("observations"))
  
  inspections[["project"]] <- project
  
  # The following function requires the column "InspTime". If this column does
  # not exist, create it with a default value
  if (is.null(inspections[["InspTime"]])) {
    
    message(
      "There is no column 'InspTime' (inspection time). ", 
      "I will create this column\n", 
      "and set it to '", default.time, "' (plus random seconds) ", 
      "for each inspection.\n", 
      "You may change this time value by setting the argument 'default.time'."
    )

    # We have to fix the random number generator otherwise the times are not
    # reproducible!
    set.seed(123L)
    
    # Generate a random number for the seconds
    inspections[["InspTime"]] <- sprintf(
      "%s:%02d", 
      default.time, 
      sample(0:59, size = nrow(inspections), replace = TRUE)
    )
  }
  
  # Create the inspection IDs and store them in column "inspid"
  inspections[["inspid"]] <- createInspectionId(inspections)

  i <- kwb.utils::selectColumns(observations, "inspno")
  observations[["inspid"]] <- kwb.utils::selectColumns(inspections, "inspid")[i]
  observations <- kwb.utils::removeColumns(observations, "inspno")

  # Just a shortcut
  inspidFirst <- function(df) kwb.utils::moveColumnsToFront(df, "inspid")

  list(
    header.info = fetch("header.info"),
    inspections = inspidFirst(inspections),
    observations = inspidFirst(observations)
  )
}
