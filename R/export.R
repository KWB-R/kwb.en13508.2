utils::globalVariables(c("buffer")) # see toEuFormattedLines_v1

# euCodedFileHeader ------------------------------------------------------------

#' Generate List With EU Header Information
#' 
#' @param separator default: ";"
#' @param decimal default: "."
#' @param quote default: '"'
#' @param encoding default: "ISO-8859-1"
#' @param language default: "en"
#' @param year default: 2010
#' 
#' @return list with elements \code{separator}, \code{decimal}, \code{quote},
#'   \code{encoding}, \code{language}, \code{year}
#'   
#' @export
#' 
euCodedFileHeader <- function(
  separator = ";", decimal = ".", quote = '"', encoding = "ISO-8859-1",
  language = "en", year = 2010
)
{
  list(
    separator = separator, decimal = decimal, quote = quote, 
    encoding = encoding, language = language, year = year
  )
}

# writeEuCodedFiles ------------------------------------------------------------

#' Write Inspection Data to Files in EU Format
#' 
#' Write inspection data to files in EU format with each file containing data of
#' a fix number (default: 100) of inspections
#' 
#' @param survey list with elements \code{header.info}, \code{inspections}, 
#'   \code{observations}, just as required for parameter "inspection.data" in 
#'   \code{\link{writeEuCodedFile}}.
#' @param file Full path to output file. The file name must end with ".txt"
#'   which will be replaced with "_<i>_<j>.txt" with i and j being the number of
#'   the first and last inspection, respectively, contained in the file.
#' @param blocksize number of inspections to be written to one file. Default:
#'   100
#' @param dbg if TRUE (default) debug messages are shown.
#' 
#' @export
#'   
#' @seealso \code{\link{writeEuCodedFile}}
#' 
writeEuCodedFiles <- function(survey, file, blocksize = 100, dbg = TRUE)
{
  stopifnot(kwb.utils::stringEndsWith(file, ".txt"))
  
  N <- nrow(survey$inspections)
  
  for (blocknumber in seq_len(ceiling(N / blocksize))) {
    
    i <- (blocknumber - 1) * blocksize + 1
    j <- min(blocknumber * blocksize, N)
    
    pattern <- paste0("%0", nchar(N), "d")
    
    postfix <- sprintf(paste0("_", pattern, "_", pattern, ".txt"), i, j)
    output.file <- gsub("\\.txt$", postfix, file)
    
    # Select inspections with numbers between i and j and select the 
    # corresponding observations
    
    selected <- kwb.utils::inRange(survey$observations$inspno, i, j)
    
    block <- list(
      header.info = survey$header.info,
      inspections = survey$inspections[i:j, ],
      observations = survey$observations[selected, ]
    )
    
    kwb.utils::.logstart(dbg, "Writing", output.file)
    
    writeEuCodedFile(block, output.file, dbg = dbg)
    
    kwb.utils::.logok(dbg)
  }
}

# writeEuCodedFile -------------------------------------------------------------

#' Write Inspection Data to File in EU Format
#' 
#' @param inspection.data inspection data as retrieved by e.g.
#'   \code{\link{readEuCodedFile}}: list with elements \code{header.info},
#'   \code{inspections}, \code{observations}. List element \code{header.info} is
#'   a list with at least the following elements: \code{separator} (column
#'   separator), \code{decimal} (decimal character "." or ","), \code{quote}
#'   (character used to quote texts in order to allow the separator sign to be
#'   used within the text). List element \code{observations} is a data.frame
#'   with required columns \code{inspno} (inspection number)
#' @param output.file full path to output file
#' @param version version of implementation. One of \code{c(1, 2, 3)}
#' @param dbg if \code{TRUE} debug messages are shown
#' @param \dots passed to \code{toEuFormattedLines_v2_3}
#' 
#' @export
#' 
writeEuCodedFile <- function(
  inspection.data, output.file, version = 3, dbg = TRUE, ...
)
{
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  
  columns <- c("header.info", "inspections", "observations")
  
  kwb.utils::checkForMissingColumns(inspection.data, columns)
  
  kwb.utils::.logstart(dbg, "Formatting lines")
  
  output.lines <- if (version == 1) {
    
    toEuFormattedLines_v1(
      header.info = inspection.data$header.info, 
      inspections = inspection.data$inspections, 
      observations = inspection.data$observations
    )
    
  } else if (version == 2) {
    
    toEuFormattedLines_v2_3(inspection.data, mycsv = FALSE, ...)
    
  } else {
    
    toEuFormattedLines_v2_3(inspection.data, mycsv = TRUE, ...)
  }  
  
  kwb.utils::.logok(dbg)
  
  kwb.utils::.logstart(dbg, "Writing lines to", output.file)
  
  writeLines(output.lines, output.file)  
  
  kwb.utils::.logok(dbg)
}

# toEuFormattedLines_v1 --------------------------------------------------------

#' Generate Lines in EU Export Format (v1)
#' 
#' Generate lines in EU export format (version 1: slow)
#' 
#' @param header.info according to list element "header.info" of list returned
#'   by \code{\link{readEuCodedFile}}
#' @param inspections according to list element "inspections" of list returned
#'   by \code{\link{readEuCodedFile}}
#' @param observations according to list element "observations" of list returned
#'   by \code{\link{readEuCodedFile}}
#' 
toEuFormattedLines_v1 <- function(header.info, inspections, observations)
{
  sep <- header.info$separator
  
  # Save the inspection numbers
  inspno <- kwb.utils::selectColumns(observations, "inspno")
  
  # Remove the column containing the inspection numbers
  observations <- kwb.utils::removeColumns(observations, "inspno")
  
  tc <- textConnection("buffer", "w")
  
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  writeLines(getHeaderLinesFromHeaderInfo(header.info), tc)
  
  insp.header.line <- inspectionHeaderLine(
    header.fields = names(inspections), separator = sep
  )
  
  obs.header.line <- observationHeaderLine(
    header.fields = names(observations), separator = sep
  )
  
  insp.numbers <- rownames(inspections)
  
  n <- nrow(inspections)
  
  # Define helper function
  write_table <- function(x, tc, sep) utils::write.table(
    x, tc, sep = sep, col.names = FALSE, row.names = FALSE, append = TRUE, 
    na = ""
  )
  
  # Get index ranges of inspections (see kwb.event::hsEventsOnChange()
  n <- length(inspno)
  change_index <- which(inspno[1:(n - 1)] != inspno[2:n]) + 1 
  begin_index <- c(1, change_index)
  end_index = c(change_index - 1, n)
  
  # Loop through the inspections
  for (i in seq_len(n)) {
    
    kwb.utils::catIf(i %% 100 == 0, "i =", i, "\n")

    writeLines(insp.header.line, tc)
    
    write_table(inspections[i, ], tc, sep)
    
    writeLines(obs.header.line, tc)
    
    indices <- begin_index[i]:end_index[i]
    
    write_table(observations[indices, ], tc, sep)
    
    if (i < n) {
      
      writeLines("#Z", tc)
    }
  }
  
  close(tc)
  
  buffer
}

# getHeaderLinesFromHeaderInfo -------------------------------------------------

#' Get Header Lines From Header Info
#' 
#' @param header.info list with elements \code{encoding}, \code{language},
#'   \code{separator}, \code{decimal}, \code{quote}, \code{year}
#' 
getHeaderLinesFromHeaderInfo <- function(header.info)
{
  columns <- c("encoding", "language", "separator", "decimal", "quote", "year")
  
  kwb.utils::checkForMissingColumns(header.info, columns)
  
  values <- c(
    header.info$encoding, 
    header.info$language, 
    header.info$separator, 
    header.info$decimal, 
    header.info$quote, 
    header.info$year
  )
  
  paste(sprintf("#A%d", seq_along(values)), values, sep = "=")
}

# inspectionHeaderLine ---------------------------------------------------------

inspectionHeaderLine <- function(header.fields, separator) 
{  
  sprintf("#B01=%s", paste(header.fields, collapse = separator))
}

# observationHeaderLine --------------------------------------------------------

observationHeaderLine <- function(header.fields, separator)
{  
  sprintf("#C=%s", paste(
    header.fields[header.fields != "inspno"], collapse = separator
  ))
}

# toEuFormattedLines_v2_3 ------------------------------------------------------

#' Generate Lines in EU Export Format (v2)
#' 
#' Generate lines in EU export format (version 2: faster than version 1)
#' 
#' @param inspection.data inspection data as retrieved by e.g.
#'   \code{\link{readEuCodedFile}}
#' @param mycsv logical. If TRUE "my" version of writing CSV is used (fast),
#'   otherwise CSV is written by means of write.table (slow)
#' @param \dots further arguments passed to dataFrameContentToTextLines
#'   
toEuFormattedLines_v2_3 <- function(inspection.data, mycsv, ...)
{
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  columns <- c("header.info", "inspections", "observations")
  
  kwb.utils::checkForMissingColumns(inspection.data, columns)
  
  header.info <- inspection.data$header.info
  
  inspections <- inspection.data$inspections
  
  observations <- inspection.data$observations
  
  kwb.utils::checkForMissingColumns(observations, "inspno")
  
  cumlen <- cumsum(numberOfObservations(observations$inspno))
  
  observations <- kwb.utils::removeColumns(observations, "inspno")
  
  columns <- c("separator", "decimal", "quote")
  
  kwb.utils::checkForMissingColumns(header.info, columns)
  
  sep <- header.info$separator
  dec <- header.info$decimal
  qchar <- header.info$quote
  
  cat("\n  Writing inspections to buffer...")
  
  inspections.buffer <- dataFrameContentToTextLines(
    dframe = inspections, sep = sep, dec = dec, qchar = qchar, na = "", 
    mycsv = mycsv, ...
  )
  
  cat("ok.\n")
  
  cat("  Writing observations to buffer...")
  
  observations.buffer <- dataFrameContentToTextLines(
    observations, sep = sep, dec = dec, qchar = qchar, na = "", mycsv = mycsv, 
    ...
  )
  
  cat("ok.\n")
  
  inspections.header <- inspectionHeaderLine(names(inspections), sep)    
  observations.header <- observationHeaderLine(names(observations), sep)
  
  end.of.inspection.identifier <- "#Z"  
  
  a.lines <- getHeaderLinesFromHeaderInfo(header.info)
  
  offset <- length(a.lines)
  a.header.indices <- 1:offset
  b.header.indices <- get_B_Positions(cumlen) + offset
  b.values.indices <- b.header.indices + 1
  c.header.indices <- b.header.indices + 2
  z.indices <- get_Z_Positions(cumlen) + offset
  
  skip.indices <- c(
    a.header.indices, 
    b.header.indices, b.values.indices, 
    c.header.indices, 
    z.indices
  )
  
  num.rows <- numberOfNeededRows(cumlen) + offset
  c.values.indices <- setdiff(1:num.rows, skip.indices)
  
  out.buffer <- character()
  out.buffer[a.header.indices] <- a.lines
  out.buffer[b.header.indices] <- inspections.header
  out.buffer[b.values.indices] <- inspections.buffer
  out.buffer[c.header.indices] <- observations.header
  out.buffer[c.values.indices] <- observations.buffer
  out.buffer[z.indices] <- end.of.inspection.identifier
  out.buffer
}

# numberOfObservations ---------------------------------------------------------

numberOfObservations <- function(inspection.numbers)
{
  counts <- stats::aggregate(
    inspection.numbers, by = list(inspection.numbers), FUN = length
  )
  
  counts$x
}

# numberOfNeededRows -----------------------------------------------------------

numberOfNeededRows <- function(cumlen)
{
  n <- length(cumlen)
  
  cumlen[n] + 4 * n - 1
}

# get_B_Positions --------------------------------------------------------------

get_B_Positions <- function(cumlen) 
{
  n <- length(cumlen)
  
  1 + 4 * seq(0, n - 1) + c(0, cumlen[-n])
}

# get_Z_Positions --------------------------------------------------------------

get_Z_Positions <- function(cumlen)
{
  (get_B_Positions(cumlen) - 1)[-1]
}
