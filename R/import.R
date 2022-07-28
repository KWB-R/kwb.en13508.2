# readEuCodedFiles -------------------------------------------------------------

#' Read Multiple CCTV Inspection Files
#' 
#' Read CCTV inspection data from multiple files coded according to EN13508-2
#' 
#' @param input.files vector of character paths to input files
#' @param dbg if \code{TRUE} debug messages are shown
#' @param append.file.names if TRUE (default) the filename will be provided in
#'   column \code{file} in the \code{inspections} element of the result list
#' @param \dots arguments passed to \code{\link{readEuCodedFile}}, such as 
#'   \code{read.inspections}, \code{simple.algorithm}, \code{warn}, see there.
#'   
#' @return list of sublists each of which is the result of a call to 
#'   \code{\link{readEuCodedFile}}. The names of the list elements are 
#'   constructed from the file names of the input files. Special characters 
#'   in the file names are replaced with underscore. Names will get a preceding 
#'   letter "x" if they start with a digit or with underscore. If files could
#'   not be read correctly, their indices are returnded in attribute
#'   \code{which_failed}.
#' @importFrom kwb.utils catIf isTryError stringList substSpecialChars
#' @export
#' 
readEuCodedFiles <- function(
  input.files, dbg = TRUE, append.file.names = TRUE, ...
)
{
  result <- lapply(seq_along(input.files), function(i) {
    
    input.file <- input.files[i]
    
    kwb.utils::catIf(
      dbg, sprintf("input file %d/%d: %s\n", i, length(input.files), input.file)
    )

    inspectionData <- try(readEuCodedFile(input.file, dbg = dbg, ...))
    
    # Skip the following if an error occurred
    if (! kwb.utils::isTryError(inspectionData)) {

      # Append inspection data to result list
      filename <- basename(input.file)
      
      if (append.file.names) {
        inspectionData$inspections$file <- filename
      }
      
      inspectionData
    }
  })

  failed <- sapply(result, is.null)
  
  # Give a warning about occurred errors
  if (any(failed)) {
    
    warning(call. = FALSE, sprintf(
      "readEuCodedFile() returned with error for the following %d files:\n%s", 
      sum(failed), kwb.utils::stringList(basename(input.files[failed]))
    ))
  }

  # Create valid list element names
  elements <- kwb.utils::substSpecialChars(basename(input.files))
  
  # Prepend an "x" to element names that start with a digit
  starts_with_digit <- grepl("^[0-9_]", elements)
  elements[starts_with_digit] <- paste0("x", elements[starts_with_digit])

  # Set list element names
  result <- stats::setNames(result[! failed], elements[! failed])
  
  # Return the indices of the files that could not be read correctly
  structure(result, which_failed = which(failed))
}

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
  input.file, encoding = "latin1", read.inspections = TRUE, 
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
    dbg = dbg, 
    "Extracting file header", {
    getHeaderInfoFromHeaderLines(
      header.lines = getHeaderLinesFromEuCodedLines(eu_lines),
      warn = warn
    )
  })
    
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
    
    kwb.utils::catIf(
      dbg, sprintf("%d inspections extracted. ", nrow(inspections))
    )
    
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
  
  list(
    header.info = header.info, 
    inspections = inspections, 
    observations = observations
  )
}

# removeEmptyLines -------------------------------------------------------------

removeEmptyLines <- function(x, dbg = TRUE)
{
  empty.line.indices <- grep("^$", x)
  
  numberOfEmptyLines <- length(empty.line.indices)
  
  if (numberOfEmptyLines > 0) {
    
    kwb.utils::.logstart(dbg, "Removing",  numberOfEmptyLines, "empty lines")
    
    x <- x[-empty.line.indices]
    
    kwb.utils::.logok(dbg)
  }
  
  x
}

# getHeaderLinesFromEuCodedLines -----------------------------------------------

getHeaderLinesFromEuCodedLines <- function(lines)
{
  grep("^#A", lines, value = TRUE)
}

# getHeaderInfoFromHeaderLines -------------------------------------------------

getHeaderInfoFromHeaderLines <- function(header.lines, warn = TRUE)
{
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
  
  if (length(index) == 0) {
    
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
    
    strsplit(keyvalues[index], "=")[[1]][2]
  }
}

# getInspectionsFromEuLines ----------------------------------------------------

getInspectionsFromEuLines <- function(eu_lines, header.info, dbg = TRUE)
{
  inspections.complete <- NULL
  
  header.line.number <- 1
  
  continue <- TRUE
  
  indices.B <- grep("^#B01", eu_lines)
  
  aborted <- FALSE
  
  while (! aborted && length(indices.B) > 0) {
    
    b.caption.lines <- getValueFromKeyValueString(eu_lines[indices.B])
    
    b.captions <- strsplit(b.caption.lines, header.info$separator)
    
    if (kwb.utils::allAreEqual(b.captions)) {
      
      inspections <- extractInspectionData(
        b.lines = eu_lines[indices.B + 1],
        header.info = header.info,
        captions = b.captions[[1]]
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
    
    header.line.number <- header.line.number + 1
    
    indices.B <- grep(sprintf("^#B%02d", header.line.number), eu_lines)    
  }  
  
  if (! aborted) {
    
    inspections.complete
  } 
  # else NULL implicitly
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
  sapply(strsplit(keyvalue, "="), "[", 2)
}
