#' Set Global Inspection ID
#'
#' Convert inspections numbers (inspno) 1,2,3,... to globally unique identifiers
#' (inspid), such as "e4b48d86"
#'
#' @param inspection.data list with elements \code{header.info},
#'   \code{inspections}, \code{observations}
#' @param project name of project to which the data are related, such as:
#'   "Lausanne"
#' @param default.time default time string to use if column <inspection-time> is
#'   not available. Default: "22:22". A random number will be generated for the
#'   seconds, just to increase the chance that setting the time is enough to
#'   generate a unique key.
#' @param name.convention one of \code{c("norm", "camel", "snake")}
#' @return list with the same elements as in \code{inspection.data} but with
#'   columns \code{inspid} being added to the data frames "inspections" and
#'   "observations"
setGlobalInspectionID <- function(
    inspection.data, 
    project = NULL, 
    default.time = "22:22",
    name.convention = "norm"
)
{
  if (is.null(project)) {
    stop(
      "Please specify the 'project' name (e.g. name of city or data provider)", 
      call. = FALSE
    )
  }
  
  # Just a shortcut
  removeEmpty <- function(df) kwb.utils::removeEmptyColumns(df, dbg = FALSE)

  inspections <- removeEmpty(get_elements(inspection.data, "inspections"))
  observations <- removeEmpty(get_elements(inspection.data, "observations"))
  
  inspections[["project"]] <- project
  
  # The following function requires the column "inspection_time". If this 
  # column does not exist, create it with a default value
  timeColumn <- get_elements(elements = name.convention, list(
    norm = "ABG",
    camel = "InspTime",
    snake = "inspection_time"
  ))
  
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

  # Columns from which to generate the hash code
  columns <- get_elements(elements = name.convention, list(
    norm = c(
      "project", 
      "ABF", 
      "ABG", 
      "AAD", 
      "AAF"
    ),
    camel = c(
      "project", 
      "InspDate", 
      "InspTime", 
      "Node1Ref", 
      "Node2Ref"
    ),
    snake = c(
      "project", 
      "inspection_date", 
      "inspection_time", 
      "node_1_ref", 
      "node_2_ref"
    )
  ))
  
  # Create the inspection IDs and store them in column "inspection_id"
  inspections[["inspection_id"]] <- createHashFromColumns(
    data = inspections,
    columns = columns
  )

  i <- kwb.utils::selectColumns(observations, "inspno")
  
  observations[["inspection_id"]] <- kwb.utils::selectColumns(
    inspections, "inspection_id"
  )[i]
  
  observations <- kwb.utils::removeColumns(observations, "inspno")

  # Just a shortcut
  idFirst <- function(df) kwb.utils::moveColumnsToFront(df, "inspection_id")

  list(
    header.info = get_elements(inspection.data, "header.info"),
    inspections = idFirst(inspections),
    observations = idFirst(observations)
  )
}
