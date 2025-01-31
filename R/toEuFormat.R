utils::globalVariables(c("buffer")) # see toEuFormat_v1

#' Generate Lines in EU Export Format
#' 
#' @param inspection.data inspection data as retrieved by e.g.
#'   \code{\link{readEuCodedFile}}
#' @param version version of implementation. One of \code{c(1, 2, 3)}
#' @param \dots passed to \code{toEuFormat_v2}
#' @param dbg whether or not to show debug messages
toEuFormat <- function(inspection.data, version = 3L, ..., dbg = TRUE)
{
  if (version == 1L) {
    
    toEuFormat_v1(
      header.info = get_elements(inspection.data, "header.info"), 
      inspections = get_elements(inspection.data, "inspections"), 
      observations = get_elements(inspection.data, "observations"),
      dbg = dbg
    )
    
  } else if (version == 2L) {
    
    toEuFormat_v2(inspection.data, mycsv = FALSE, ..., dbg = dbg)
    
  } else {
    
    toEuFormat_v2(inspection.data, mycsv = TRUE, ..., dbg = dbg)
  }
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
#' @param dbg whether or not to show debug messages
toEuFormat_v1 <- function(header.info, inspections, observations, dbg = TRUE)
{
  #kwb.utils::assignPackageObjects("kwb.en13508.2")

  # Get row ranges of observations per inspection
  changes <- kwb.utils::findChanges(get_columns(observations, "inspno"))
  
  # Remove the column containing the inspection numbers
  observations <- kwb.utils::removeColumns(observations, "inspno")
  
  tc <- textConnection("buffer", "w")
  on.exit(close(tc))
  
  sep <- header.info$separator

  a.header <- getHeaderLinesFromHeaderInfo(header.info)
  b.header <- inspectionHeaderLine(names(inspections), sep)
  c.header <- observationHeaderLine(names(observations), sep)
  
  # Define helper function
  writeTable <- function(header, data, add.z = TRUE) {
    writeLines(header, tc)
    utils::write.table(
      data, 
      file = tc, 
      sep = sep, 
      col.names = FALSE, 
      row.names = FALSE, 
      append = TRUE, 
      na = ""
    )
    if (add.z) {
      writeLines("#Z", tc)
    }
  }
  
  # Write #A section
  writeLines(a.header, tc)

  # Number of inspections
  n_inspections <- nrow(inspections)
  
  # Loop through the inspections
  for (inspno in seq_len(n_inspections)) {
    
    kwb.utils::catIf(inspno %% 100 == 0L, "i =", i, "\n")
    
    # Write #B section
    writeTable(header = b.header, data = inspections[inspno, , drop = FALSE])
    
    # Get observations for the current inspection
    i <- which(changes[["value"]] == inspno)

    if (len <- length(i)) {
      
      stopifnot(len == 1L)
      
      # Extract rows of observations
      data <- observations[changes[["starts_at"]][i]:changes[["ends_at"]][i], ]
      
      # Write #C section
      writeTable(header = c.header, data = data, add.z = inspno < n_inspections)
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
  
  values <- unlist(get_elements(header.info, elements))
  
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
#' @param dbg whether or not to show debug messages
toEuFormat_v2 <- function(inspection.data, mycsv, ..., dbg = TRUE)
{
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  
  # Provide list elements in variables header_info, inspections, observations
  header_info <- get_elements(inspection.data, "header.info")
  inspections <- get_elements(inspection.data, "inspections")
  observations <- get_elements(inspection.data, "observations")
  
  # Save the inspection numbers in inspnos
  inspnos <- get_columns(observations, "inspno")
  
  # Remove the inspection numbers
  observations <- kwb.utils::removeColumns(observations, "inspno")
  
  # Build argument list for calling dataFrameContentToTextLines
  elements <- c(sep = "separator", dec = "decimal", qchar = "quote")
  arguments <- get_elements(header_info, elements)
  
  # Save the separator in its own variable
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
  c_sizes <- cumsum(unname(table(inspnos)))
  
  # Number of C-blocks (= number of inspections)
  n_inspections <- length(c_sizes)
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  kwb.utils::catAndRun(
    "  Writing B-blocks (inspection data) ... ", 
    dbg = dbg, 
    expr = {
      b_at <- offset + 1L + 
        4L * (seq_len(n_inspections) - 1L) + 
        c(0L, c_sizes[-n_inspections])
      
      out_lines[b_at] <- inspectionHeaderLine(names(inspections), sep)
      out_lines[b_at + 1L] <- to_csv(inspections)
    }
  )
  
  kwb.utils::catAndRun(
    "  Writing C-blocks (observation data) ... ", 
    dbg = dbg, 
    expr = {
      out_lines[b_at + 2L] <- observationHeaderLine(names(observations), sep)
      z_at <- b_at[-1L] - 1L
      skip_indices <- c(b_at, b_at + 1L, b_at + 2L, z_at)
      n_rows <- c_sizes[n_inspections] + 4 * n_inspections - 1L + offset
      c_body_at <- setdiff(seq(offset + 1L, n_rows), skip_indices)
      out_lines[c_body_at] <- to_csv(observations)
    }
  )
  
  out_lines[z_at] <- "#Z"
  
  out_lines
}
