utils::globalVariables(c("buffer")) # see toEuFormat_v1

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
  separator = ";", 
  decimal = ".", 
  quote = '"', 
  encoding = "ISO-8859-1",
  language = "en", 
  year = 2010L
)
{
  list(
    separator = separator, 
    decimal = decimal, 
    quote = quote, 
    encoding = encoding, 
    language = language, 
    year = year
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
  stopifnot(endsWith(file, ".txt"))
  
  header.info <- get_elements(survey, "header.info")
  inspections <- get_elements(survey, "inspections")
  observations <- get_elements(survey, "observations")
  
  inspnos <- get_columns(observations, "inspno")
  
  N <- nrow(inspections)
  
  # Function to create output file name
  fileToOutputFile <- function(i, j) {
    pattern <- paste0("%0", nchar(N), "d")
    postfix <- sprintf(paste0("_", pattern, "_", pattern, ".txt"), i, j)
    gsub("\\.txt$", postfix, file)
  }
  
  for (block_number in seq_len(ceiling(N / blocksize))) {
    
    i <- (block_number - 1L) * blocksize + 1L
    j <- min(block_number * blocksize, N)
    
    output_file <- fileToOutputFile(file, i, j)

    kwb.utils::catAndRun(dbg = dbg, paste("Writing", output_file), {
      
      writeEuCodedFile(
        inspection.data = list(
          header.info = header.info,
          # Select inspections with numbers between i and j
          inspections = inspections[i:j, ],
          # Select the corresponding observations
          observations = observations[kwb.utils::inRange(inspnos, i, j), ]
        ), 
        output.file = output_file, 
        dbg = dbg
      )
    })
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
#' @param \dots passed to \code{toEuFormat_v2}
#' 
#' @return if \code{output.file} is given, the path to the output file is 
#'   returned, otherwise (\code{output.file = NULL}) the file content is
#'   returned as a vector of character representing the lines of the file.
#' 
#' @export
#' 
writeEuCodedFile <- function(
  inspection.data, output.file = NULL, version = 3L, dbg = TRUE, ...
)
{
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  
  output.lines <- kwb.utils::catAndRun(dbg = dbg, "Formatting lines", {
    
    if (version == 1L) {
      
      toEuFormat_v1(
        header.info = get_elements(inspection.data, "header.info"), 
        inspections = get_elements(inspection.data, "inspections"), 
        observations = get_elements(inspection.data, "observations")
      )
      
    } else if (version == 2L) {
      
      toEuFormat_v2(inspection.data, mycsv = FALSE, ...)
      
    } else {
      
      toEuFormat_v2(inspection.data, mycsv = TRUE, ...)
    }  
    
  })
  
  if (is.null(output.file)) {
    return(output.lines)
  }
    
  kwb.utils::catAndRun(dbg = dbg, paste("Writing lines to", output.file), {
    writeLines(output.lines, output.file)  
  })
}

# toEuFormat_v1 ----------------------------------------------------------------

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
toEuFormat_v1 <- function(header.info, inspections, observations)
{
  sep <- header.info$separator
  
  # Save the inspection numbers
  inspnos <- get_columns(observations, "inspno")
  
  # Remove the column containing the inspection numbers
  observations <- kwb.utils::removeColumns(observations, "inspno")
  
  tc <- textConnection("buffer", "w")
  on.exit(close(tc))
  
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  writeLines(getHeaderLinesFromHeaderInfo(header.info), tc)
  
  insp.header.line <- inspectionHeaderLine(names(inspections), sep)
  obs.header.line <- observationHeaderLine(names(observations), sep)
  
  insp.numbers <- rownames(inspections)
  
  n <- nrow(inspections)
  
  # Define helper function
  write_table <- function(x, tc, sep) utils::write.table(
    x, tc, sep = sep, col.names = FALSE, row.names = FALSE, append = TRUE, 
    na = ""
  )
  
  # Get index ranges of inspections (see kwb.event::hsEventsOnChange())
  n_obs <- length(inspnos)
  change_index <- which(inspnos[1:(n_obs - 1L)] != inspnos[2:n_obs]) + 1L
  begin_index <- c(1L, change_index)
  end_index = c(change_index - 1L, n_obs)
  
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
  elements <- c("encoding", "language", "separator", "decimal", "quote", "year")
  
  values <- unlist(kwb.utils::selectElements(header.info, elements))
  
  paste(sprintf("#A%d", seq_along(values)), values, sep = "=")
}

# inspectionHeaderLine ---------------------------------------------------------
inspectionHeaderLine <- function(header.fields, sep) 
{  
  sprintf("#B01=%s", paste(header.fields, collapse = sep))
}

# observationHeaderLine --------------------------------------------------------
observationHeaderLine <- function(header.fields, sep)
{  
  sprintf("#C=%s", paste(setdiff(header.fields, "inspno"), collapse = sep))
}

# toEuFormat_v2 ----------------------------------------------------------------

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
toEuFormat_v2 <- function(inspection.data, mycsv, ...)
{
  #kwb.utils::assignPackageObjects("kwb.en13508.2")

  # Provide list elements in variables header_info, inspections, observations
  header_info <- get_elements(inspection.data, "header.info")
  inspections <- get_elements(inspection.data, "inspections")
  observations <- get_elements(inspection.data, "observations")
  
  # Save the inspection numbers in inspno
  inspection_numbers <- get_columns(observations, "inspno")
  
  # Remove the inspection numbers
  observations <- kwb.utils::removeColumns(observations, "inspno")

  # Build argument list for calling dataFrameContentToTextLines
  elements <- c(sep = "separator", dec = "decimal", qchar = "quote")
  arguments <- get_elements(header_info, elements)
  
  # Save the separator in its own variable for reusage
  sep <- arguments$sep

  # Helper function to create CSV lines
  to_csv <- function(x) do.call(dataFrameContentToTextLines, c(
    arguments, list(dframe = x, na = "", mycsv = mycsv, ...)
  ))

  # Start the output lines with the A-block
  out_lines <- getHeaderLinesFromHeaderInfo(header_info)

  # Offset of further rows
  offset <- length(out_lines)

  # Cumulative sizes (number of lines) of the C-blocks
  c_sizes <- cumsum(unname(table(inspection_numbers)))

  # Number of C-blocks (= number of inspections)
  n <- length(c_sizes)

  cat("ok.\n")
  
  kwb.utils::catAndRun("  Writing B-blocks (inspection data) ... ", {
    
    b_indices <- offset + 1L + 4 * (seq_len(n) - 1L) + c(0L, c_sizes[-n])
    out_lines[b_indices] <- inspectionHeaderLine(names(inspections), sep)
    out_lines[b_indices + 1L] <- to_csv(inspections)
  })
  
  kwb.utils::catAndRun("  Writing C-blocks (observation data) ... ", {
    
    out_lines[b_indices + 2L] <- observationHeaderLine(names(observations), sep)
    z_indices <- b_indices[-1L] - 1L
    skip_indices <- c(b_indices, b_indices + 1L, b_indices + 2L, z_indices)
    n_rows <- c_sizes[n] + 4 * n - 1L + offset
    c_body_indices <- setdiff(seq(offset + 1L, n_rows), skip_indices)
    out_lines[c_body_indices] <- to_csv(observations)
  })
  
  out_lines[z_indices] <- "#Z"

  out_lines
}
