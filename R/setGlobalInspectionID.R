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
#'   available. Default: "22:33". A random number will be generated for the 
#'   seconds, just to increase the chance that setting the time is enough to
#'   generate a unique key.
#' @return list with the same elements as in \code{inspection.data} but with
#'   columns \code{inspid} being added to the data frames "inspections" and
#'   "observations"
setGlobalInspectionID <- function(
    inspection.data, 
    project = NULL, 
    default.time = "22:33"
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
  
  # The following function requires the column "inspection_time". If this 
  # column does not exist, create it with a default value
  timeColumn <- "inspection_time"
  inspections <- kwb.utils::hsAddMissingCols(inspections, timeColumn)
  
  hasNoTime <- kwb.utils::isNaOrEmpty(inspections[[timeColumn]])
  
  if (any(hasNoTime)) {
    n_missing <- sum(hasNoTime)
    message(
      "Setting ", n_missing, " missing inspection times to '", default.time, 
      "' (plus random seconds). You may change this time value by setting the ",
      "argument 'default.time'."
    )
    
    # We have to fix the random number generator otherwise the times are not
    # reproducible!
    set.seed(123L)
    
    # Generate a random number for the seconds
    inspections[[timeColumn]][hasNoTime] <- sprintf(
      "%s:%02d", 
      default.time, 
      sample(0:59, size = n_missing, replace = TRUE)
    )
  }
  
  # Create the inspection IDs and store them in column "inspection_id"
  inspections[["inspection_id"]] <- createInspectionId(inspections)

  i <- kwb.utils::selectColumns(observations, "inspno")
  
  observations[["inspection_id"]] <- kwb.utils::selectColumns(
    inspections, "inspection_id"
  )[i]
  
  observations <- kwb.utils::removeColumns(observations, "inspno")

  # Just a shortcut
  idFirst <- function(df) kwb.utils::moveColumnsToFront(df, "inspection_id")

  list(
    header.info = fetch("header.info"),
    inspections = idFirst(inspections),
    observations = idFirst(observations)
  )
}
