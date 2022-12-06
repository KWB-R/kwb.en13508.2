# readEuCodedFile --------------------------------------------------------------

#' Read CCTV Inspection Data in EN13508-2 Format
#' 
#' Read CCTV inspection data from file coded according to EN13508-2
#' 
#' @param input.file full path to text file containing CCTV inspection results
#'   in the format described in DIN EN 13508-2
#' @param encoding default: "latin1"
#' @param read.inspections if \code{TRUE}, general inspection data (in
#'   #B-blocks) are read, otherwise skipped (use if function fails)
#' @param meaningful.names if \code{FALSE} (default), the short names (codes) as
#'   defined in EN13508.2 are used as column names, otherwise more meaningful
#'   names are used. See columns\code{Code} and \code{Name}, respectively, in
#'   the data frame returned by \code{getCodes()}.
#' @param simple.algorithm if \code{TRUE} (default), a simple (and faster)
#'   algorithm is used to extract the general information about the inspections
#'   from the #B-headers. It requires that all #B-headers have the same number
#'   and order of fields. If \code{FALSE}, another algorithm being able to treat
#'   differing #B-header rows is used.
#' @param warn if \code{TRUE}, warnings are shown (e.g. if not all #A-header
#'   fields were found)
#' @param dbg if \code{TRUE}, debug messages are shown, else not
#'
#' @return list with elements \code{header.info}, \code{inspections},
#'   \code{observations}
#' @importFrom kwb.utils catAndRun catIf isTryError .logstart .logok
#' @export
#' 
readEuCodedFile <- function(
  input.file, 
  encoding = "latin1", 
  read.inspections = TRUE, 
  meaningful.names = FALSE,
  simple.algorithm = TRUE, 
  warn = TRUE, 
  dbg = TRUE
)
{
  #kwb.utils::assignArgumentDefaults(kwb.en13508.2::readEuCodedFile)
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  
  run <- function(...) kwb.utils::catAndRun(dbg = dbg, ...)
  
  eu_lines <- run(
    paste("Reading input file", input.file),
    readLines(input.file, encoding = encoding)
  )
  
  eu_lines <- run(
    "Removing empty lines (if any)",
    removeEmptyLines(eu_lines, dbg = dbg)
  )
  
  header.info <- run(
    "Extracting file header", 
    getFileHeaderFromEuLines(eu_lines, warn)
  )
  
  inspections <- run(
    "Extracting inspection records",
    getInspectionRecordsFromEuLines(
      eu_lines, header.info, read.inspections, simple.algorithm, dbg
    )
  )
  
  observations <- run(
    "Extracting observation records",
    getObservationRecordsFromEuLines(eu_lines, header.info, dbg)
  )

  if (meaningful.names) {
    inspections <- renameColumnsToMeaningful(inspections)
    observations <- renameColumnsToMeaningful(observations)
  }
  
  list(
    header.info = header.info, 
    inspections = inspections, 
    observations = observations
  )
}

# renameColumnsToMeaningful ----------------------------------------------------
renameColumnsToMeaningful <- function(x)
{
  codeInfo <- kwb.utils::selectColumns(getCodes(), c("Code", "Name"))
  renamings <- kwb.utils::toLookupList(data = codeInfo)
  kwb.utils::renameColumns(x, renamings)
}

