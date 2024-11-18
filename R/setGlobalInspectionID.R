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
#' @param error.file optional. Path to file to which duplicates are are written 
#'   (if any). Default: \code{"setGlobalInspectionID_duplicates.txt"}
#' @return list with the same elements as in \code{inspection.data} but with
#'   columns \code{inspid} being added to the data frames "inspections" and
#'   "observations"
setGlobalInspectionID <- function(
    inspection.data, 
    project = NULL, 
    default.time = "22:22",
    name.convention = "norm",
    error.file = NULL
)
{
  if (is.null(project)) {
    stop(
      "Please specify the 'project' name (e.g. name of city or data provider)", 
      call. = FALSE
    )
  }
  
  removeEmpty <- function(x) kwb.utils::removeEmptyColumns(x, dbg = FALSE)
  inspections <- removeEmpty(get_elements(inspection.data, "inspections"))
  observations <- removeEmpty(get_elements(inspection.data, "observations"))
  
  # Columns from which to generate the hash code
  columns <- get_elements(elements = name.convention, list(
    norm = c("project", "ABF", "ABG", "AAD", "AAF"),
    camel = c("project", "InspDate", "InspTime", "Node1Ref", "Node2Ref"),
    snake = c("project", "inspection_date", "inspection_time", "node_1_ref", "node_2_ref")
  ))
  
  inspections[["project"]] <- project
  
  # The following function requires the column "inspection_time". If this 
  # column does not exist, create it with a default value
  inspections <- checkOrAddInspectionTime(
    data = inspections, column = columns[3L], hhmm = default.time, seed = 123L
  )
  
  # Create inspection_ids and check if they have duplicated values
  inspection_ids <- createHashFromColumns(inspections, columns, silent = TRUE)
  stop_on_hash_duplicates(inspection_ids, error.file = error.file)
  
  updated <- setInspectionId(inspections, observations, inspection_ids)
  
  list(
    header.info = get_elements(inspection.data, "header.info"),
    inspections = updated$inspections,
    observations = updated$observations
  )
}

# checkOrAddInspectionTime -----------------------------------------------------
checkOrAddInspectionTime <- function(data, column, ...)
{
  x <- if (column %in% names(data)) {
    data[[column]]
  } else {
    character(nrow(data))
  }  
  data[[column]] <- fillTimeVector(x, ...)
  data
}

# fillTimeVector ---------------------------------------------------------------
fillTimeVector <- function(x, hhmm = "22:22", seed = NULL, silent = FALSE)
{
  if (any(isEmpty <- kwb.utils::isNaOrEmpty(x))) {
    
    if (!silent) {
      message(sprintf(
        "Setting %d missing inspection times to '%s' (plus random seconds).",
        sum(isEmpty), hhmm
      ))
    }
    
    # Fix the random number generator to be reproducible
    if (!is.null(seed)) {
      set.seed(seed)
    }
    
    # Generate a random number for the seconds. 
    seconds <- sample(0:59, size = sum(isEmpty), replace = TRUE)
    x[isEmpty] <- sprintf("%s:%02d", hhmm, seconds)
  }
  
  x
}

# stop_on_hash_duplicates ------------------------------------------------------

#'@importFrom utils capture.output
stop_on_hash_duplicates <- function(hashes, error.file = NULL)
{
  if (identical(kwb.utils::removeAttributes(hashes), -1L)) {
    
    duplicates <- kwb.utils::getAttribute(hashes, "duplicates")
    
    if (is.null(error.file)) {
      print(duplicates)
    } else {
      writeLines(utils::capture.output(print(duplicates)), error.file)
    }
    
    stop(
      "There were duplicates in the key columns (see ", 
      ifelse(is.null(error.file), "above", dQuote(error.file, '"')),
      ").",
      call. = FALSE
    )
  }
}

# setInspectionId --------------------------------------------------------------

#' Add column inspection_id to table of inspections and observations
#' 
#' @param inspections data frame where each row represents an inspection
#' @param observations data frame where each row represents an observation. The 
#'   data frame must have a column "inspno" that refers to a row in the data 
#'   frame \code{inspections}.
#' @param inspection_ids vector of as many inspection ids as there are rows in 
#'   \code{inspections}
#' @return list with elements \code{inspections} and \code{observations} each of
#'   which has a new column "inspection_id" as its first column.
#' @importFrom kwb.utils moveColumnsToFront removeColumns selectColumns
#' @export
#' @examples
#' inspections <- data.frame(pipe_id = 1:3)
#' observations <- data.frame(
#'   inspno = c(1, 1, 2, 2, 3, 3),
#'    observation = c("start", "end", "start", "end", "start", "end")
#' )
#' setInspectionId(inspections, observations, paste0("id_", 1:3))
#' 
setInspectionId <- function(inspections, observations, inspection_ids)
{
  stopifnot(length(inspection_ids) == nrow(inspections))
  stopifnot(!anyDuplicated(inspection_ids))
  
  INSPID <- "inspection_id"
  INSPNO <- "inspno"
  
  inspection_numbers <- kwb.utils::selectColumns(observations, INSPNO)
  inspections[[INSPID]] <- inspection_ids
  observations[[INSPID]] <- inspection_ids[inspection_numbers]
  
  id_first <- function(x) kwb.utils::moveColumnsToFront(x, INSPID)
  
  list(
    inspections = id_first(inspections),
    observations = id_first(kwb.utils::removeColumns(observations, INSPNO))
  )
}
