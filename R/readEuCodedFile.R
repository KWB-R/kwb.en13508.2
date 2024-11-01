# readEuCodedFile --------------------------------------------------------------

#' Read CCTV Inspection Data in EN13508-2 Format
#' 
#' Read CCTV inspection data from file coded according to EN13508-2
#' 
#' @param input.file full path to text file containing CCTV inspection results
#'   in the format described in DIN EN 13508-2
#' @param encoding default: "unknown", passed to \code{\link{readLines}}, see 
#'   there.
#' @param file.encoding Encoding to be assumed for the \code{input.file}.
#'   The default is \code{NULL} in which case the name of the encoding is read
#'   from the \code{#A1} field of the \code{input.file}. 
#' @param read.inspections if \code{TRUE}, general inspection data (in
#'   #B-blocks) are read, otherwise skipped (use if function fails)
#' @param name.convention one of \code{c("norm", "camel", "snake")} specifying
#'   the set of names used in the returned tables. \code{"norm"}: as specified 
#'   in the norm EN13508.2, \code{"camel"}: \code{CamelCase}, \code{"snake"}: 
#'   \code{snake_case}
#' @param simple.algorithm if \code{TRUE} (default), a simple (and faster)
#'   algorithm is used to extract the general information about the inspections
#'   from the #B-headers. It requires that all #B-headers have the same number
#'   and order of fields. If \code{FALSE}, another algorithm being able to treat
#'   differing #B-header rows is used.
#' @param warn if \code{TRUE}, warnings are shown (e.g. if not all #A-header
#'   fields were found)
#' @param dbg if \code{TRUE}, debug messages are shown, else not
#' @param check.encoding logical indicating whether or not to check if the 
#'   encoding string that is given in the \code{#A1} header of the file is
#'   "known". The default is \code{TRUE}, i.e. the check is performed and an
#'   error is thrown if the encoding is not in the list of known encodings.
#' @param \dots further arguments to be passed to 
#'   \code{kwb.en13508.2:::extractObservationData}
#' @return list with elements \code{header.info}, \code{inspections},
#'   \code{observations}
#' @importFrom kwb.utils catAndRun readLinesWithEncoding
#' @export
#' 
readEuCodedFile <- function(
    input.file, 
    encoding = "unknown", 
    file.encoding = NULL,
    read.inspections = TRUE, 
    name.convention = c("norm", "camel", "snake")[1L],
    simple.algorithm = TRUE, 
    warn = TRUE, 
    dbg = TRUE,
    check.encoding = TRUE,
    ...
)
{
  #kwb.utils::assignArgumentDefaults(kwb.en13508.2::readEuCodedFile)
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  
  name.convention <- match.arg(name.convention, c("norm", "camel", "snake"))
  
  # If not explicitly given, use the encoding as given in the #A1 header
  if (is.null(file.encoding)) {
    file.encoding <- readFileEncodingFromHeader(input.file)
    
    # Replace "iso-8859-1:1998" with "latin1"
    # (see https://de.wikipedia.org/wiki/ISO_8859-1: 
    # "ISO 8859-1, genauer ISO/IEC 8859-1, auch bekannt als Latin-1 [...]")
    file.encoding <- gsub(
      "^iso-8859-1(:1998)?$", 
      "latin1", 
      file.encoding, 
      ignore.case = TRUE
    )
  }
  
  if (check.encoding) {
    stopOnInvalidEncoding(file.encoding)
  }
  
  eu_lines <- kwb.utils::catAndRun(
    dbg = dbg,
    sprintf("Reading %s assuming %s encoding", input.file, file.encoding), 
    kwb.utils::readLinesWithEncoding(
      file = input.file, 
      fileEncoding = file.encoding,
      encoding = encoding, 
      warn = FALSE
    )
  )
  
  eu_lines <- kwb.utils::catAndRun(
    dbg = dbg,
    "Removing empty lines (if any)",
    removeEmptyLines(eu_lines, dbg = dbg)
  )
  
  header.info <- kwb.utils::catAndRun(
    dbg = dbg,
    "Extracting file header", 
    getFileHeaderFromEuLines(eu_lines, warn)
  )
  
  inspections <- kwb.utils::catAndRun(
    dbg = dbg,
    "Extracting inspection records",
    extractInspectionData(
      text = eu_lines, 
      header.info = header.info, 
      read.inspections = read.inspections, 
      simple.algorithm = simple.algorithm, 
      dbg = dbg
    )
  )
  
  dot.args <- list(...)
  #dot.args <- list() # for debugging!
  #dot.args <- list(as.text = TRUE)
  
  observations <- kwb.utils::catAndRun(
    dbg = dbg,
    "Extracting observation records",
    do.call(extractObservationData, c(dot.args, list(
      text = eu_lines, 
      header.info = header.info, 
      dbg = dbg,
      file = input.file
    )))
  )
  
  if (name.convention != "norm") {
    snake.case <- name.convention == "snake"
    inspections <- applyNameConvention(inspections, snake.case)
    observations <- applyNameConvention(observations, snake.case)
  }
  
  list(
    header.info = header.info, 
    inspections = inspections, 
    observations = observations
  )
}

# readFileEncodingFromHeader ---------------------------------------------------
readFileEncodingFromHeader <- function(file)
{
  get_elements(
    x = getFileHeaderFromEuLines(readLines(kwb.utils::safePath(file), n = 6L)), 
    elements = "encoding"
  )
}

# stopOnInvalidEncoding --------------------------------------------------------
stopOnInvalidEncoding <- function(encoding)
{
  if (!encoding %in% (available <- c("latin1", iconvlist()))) {
    stop(kwb.utils::noSuchElements(encoding, available, "encoding string"))
  }
}

# applyNameConvention ----------------------------------------------------------
applyNameConvention <- function(x, snake.case = FALSE)
{
  result <- kwb.utils::renameColumns(x, renamings = readRenamings(
    file.name = "eucodes.csv",
    column.from = "Code",
    column.to = "Name"
  ))
  
  if (snake.case) {
    result <- kwb.utils::renameColumns(result, renamings = readRenamings(
      file.name = "column-names.csv", 
      column.from = "name_1", 
      column.to = "name_2"
    ))
  }
  
  result
}

# readRenamings ----------------------------------------------------------------
readRenamings <- function(file.name, column.from, column.to)
{
  data <- readPackageFile(file.name)
  data <- kwb.utils::selectColumns(data, c(column.from, column.to))
  is.complete <- rowSums(nchar(as.matrix(data)) > 0L) == 2L
  kwb.utils::toLookupList(data = data[is.complete, ])
}
