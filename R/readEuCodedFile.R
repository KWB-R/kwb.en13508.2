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
#' @param short.names if \code{TRUE} (default), the short names (codes) as defined
#'   in EN13508.2 are used as column names, otherwise more meaningful names are used.
#'   See columns\code{Code} and \code{Name}, respectively, in the data frame returned by 
#'   \code{getCodes()}.
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
    input.file, encoding = "latin1", read.inspections = TRUE, short.names = TRUE,
    simple.algorithm = TRUE, warn = TRUE, dbg = TRUE
)
{
  #kwb.utils::assignArgumentDefaults(kwb.en13508.2::readEuCodedFile)
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  
  eu_lines <- kwb.utils::catAndRun(
    dbg = dbg, paste("Reading input file", input.file),
    readLines(input.file, encoding = encoding)
  )
  
  eu_lines <- kwb.utils::catAndRun(
    dbg = dbg, "Removing empty lines (if any)",
    removeEmptyLines(eu_lines)
  )
  
  header.info <- kwb.utils::catAndRun(
    dbg = dbg, "Extracting file header", 
    getFileHeaderFromEuLines(eu_lines)
  )
  
  kwb.utils::.logstart(dbg, "Extracting inspection records")
  
  if (read.inspections) {
    
    inspections <- if (simple.algorithm) {
      getInspectionsFromEuLines(eu_lines, header.info, dbg = dbg > 1L)
    } # else NULL
    
    # If the inspections could not be read with the simple algorithm (due to
    # changing header rows) or if the user requests it, try it again with
    # another algorithm
    if (is.null(inspections)) {
      
      inspections <- getInspectionsFromEuLines.new(
        eu_lines, header.info, dbg = dbg
      )
    }
    
    kwb.utils::catIf(dbg, sprintf(
      "%d inspections extracted. ", nrow(inspections)
    ))
    
  } else {
    
    warning(
      "I (yet) cannot read the inspection data (#B-blocks). ",
      "So I just returned the number of inspections instead of a ",
      "data frame with all information on the inspection!"
    )
    
    inspections <- length(grep("^#B01", eu_lines))
  }
  
  kwb.utils::.logok(dbg)
  
  kwb.utils::.logstart(dbg, "Extracting observation records")
  
  observations <- try(
    getObservationsFromEuLines(eu_lines, header.info, dbg = dbg), 
    silent = TRUE
  )
  
  if (kwb.utils::isTryError(observations)) {
    headerInfo <- getHeaderInfo(eu_lines)
    #View(headerInfo)
    observations <- extractObservationData(eu_lines, headerInfo, header.info)
  }
  
  kwb.utils::catIf(
    dbg, sprintf("%d observations extracted. ", nrow(observations))
  )
  
  kwb.utils::.logok(dbg)
  
  if (!short.names) {
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

# getFileHeaderFromEuLines -----------------------------------------------------
getFileHeaderFromEuLines <- function(eu_lines, warn = TRUE)
{
  header.lines <- grep("^#A", eu_lines, value = TRUE)
  
  # original_fields <- do.call(kwb.utils::toLookupList, kwb.utils::toKeysAndValues(
  #   kwb.utils::collapsed(gsub("^#", "", header.lines), "@"),
  #   separators = c("@", "=")
  # ))
  # 
  # renamed_fields <- kwb.utils::renameColumns(original_fields, list(
  #   A1 = "encoding", A2 = "language", A3 = "separator", A4 = "decimal",
  #   A5 = "quote", A6 = "year"
  # ))
  
  # Set quote to "" instead of NA because read.table will give strange results
  quote <- findKeyAndExtractValue(header.lines, "A5", warn = warn)
  quote <- kwb.utils::defaultIfNA(quote, "")
  
  list(
    encoding = findKeyAndExtractValue(header.lines, "A1", warn = warn),
    language = findKeyAndExtractValue(header.lines, "A2", warn = warn),
    separator = findKeyAndExtractValue(header.lines, "A3", warn = warn),
    decimal = findKeyAndExtractValue(header.lines, "A4", warn = warn),
    quote = quote,
    year = findKeyAndExtractValue(header.lines, "A6", warn = warn)
  )
}

# findKeyAndExtractValue -------------------------------------------------------
findKeyAndExtractValue <- function(keyvalues, key, default = NA, warn = TRUE)
{
  pattern <- paste0("^#", key)
  
  index <- grep(pattern, keyvalues)
  
  if (length(index) == 0L) {
    
    warnMessage <- sprintf(
      "Key '#%s' not found in the #A-header of the file.", key
    )
    
    if (! is.na(default)) {
      warnMessage <- paste(warnMessage, "I will use the default:", default)
    }
    
    if (warn) {
      message(warnMessage)
      warning(warnMessage)
    }
    
    default
    
  } else {
    
    strsplit(keyvalues[index], "=")[[1L]][2L]
  }
}

# getInspectionsFromEuLines ----------------------------------------------------
getInspectionsFromEuLines <- function(eu_lines, header.info, dbg = TRUE)
{
  inspections.complete <- NULL
  
  header.line.number <- 1L
  
  continue <- TRUE
  
  indices.B <- grep("^#B01", eu_lines)
  
  aborted <- FALSE
  
  while (! aborted && length(indices.B) > 0L) {
    
    b.caption.lines <- getValueFromKeyValueString(eu_lines[indices.B])
    
    b.captions <- strsplit(b.caption.lines, header.info$separator)
    
    if (kwb.utils::allAreEqual(b.captions)) {
      
      inspections <- extractInspectionData(
        b.lines = eu_lines[indices.B + 1L],
        header.info = header.info,
        captions = b.captions[[1L]]
      )
      
      inspections.complete <- kwb.utils::safeColumnBind(
        inspections.complete, inspections
      )
      
    } else {
      
      if (dbg) {
        message(
          "The #B-header lines differ within the file -> I will change the ",
          "algorithm..."
        )
      }
      
      aborted <- TRUE
    }    
    
    header.line.number <- header.line.number + 1L
    
    indices.B <- grep(sprintf("^#B%02d", header.line.number), eu_lines)
  }  
  
  if (aborted) {
    return(NULL)
  }
  
  inspections.complete
}

# extractInspectionData --------------------------------------------------------
extractInspectionData <- function(b.lines, header.info, captions)
{
  inspections <- kwb.utils::csvTextToDataFrame(
    text = paste(b.lines, collapse = "\n"),
    sep = header.info$separator, 
    dec = header.info$decimal, 
    quote = header.info$quote,
    comment.char = "",
    stringsAsFactors = FALSE
  )
  
  stats::setNames(inspections, captions)
}

# getValueFromKeyValueString ---------------------------------------------------
getValueFromKeyValueString <- function(keyvalue)
{
  sapply(strsplit(keyvalue, "="), "[", 2L)
}

# setFilename ------------------------------------------------------------------
setFilename <- function(data, name)
{
  data[["file"]] <- name
  kwb.utils::moveColumnsToFront(data, "file")
}
