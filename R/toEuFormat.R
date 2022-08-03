utils::globalVariables(c("buffer")) # see toEuFormat_v1

#' Generate Lines in EU Export Format
#' 
#' @param inspection.data inspection data as retrieved by e.g.
#'   \code{\link{readEuCodedFile}}
#' @param version version of implementation. One of \code{c(1, 2, 3)}
#' @param \dots passed to \code{toEuFormat_v2}
toEuFormat <- function(inspection.data, version = 3L)
{
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
  # Save the inspection numbers
  inspnos <- get_columns(observations, "inspno")
  
  # Remove the column containing the inspection numbers
  observations <- kwb.utils::removeColumns(observations, "inspno")
  
  tc <- textConnection("buffer", "w")
  on.exit(close(tc))
  
  #kwb.utils::assignPackageObjects("kwb.en13508.2")
  writeLines(getHeaderLinesFromHeaderInfo(header.info), tc)
  
  sep <- header.info$separator
  
  insp.header.line <- inspectionHeaderLine(names(inspections), sep)
  obs.header.line <- observationHeaderLine(names(observations), sep)
  
  insp.numbers <- rownames(inspections)
  
  n_inspections <- nrow(inspections)
  
  # Define helper function
  write_table <- function(x) utils::write.table(
    x, 
    file = tc, 
    sep = sep, 
    col.names = FALSE, 
    row.names = FALSE, 
    append = TRUE, 
    na = ""
  )
  
  # Get index ranges of inspections (see kwb.event::hsEventsOnChange())
  n_obs <- length(inspnos)
  change_index <- which(inspnos[1:(n_obs - 1L)] != inspnos[2:n_obs]) + 1L
  begin_index <- c(1L, change_index)
  end_index = c(change_index - 1L, n_obs)
  
  # Loop through the inspections
  for (i in seq_len(n_inspections)) {
    
    kwb.utils::catIf(i %% 100 == 0L, "i =", i, "\n")
    
    writeLines(insp.header.line, tc)
    write_table(inspections[i, ])
    writeLines(obs.header.line, tc)
    
    indices <- begin_index[i]:end_index[i]
    
    write_table(observations[indices, ])
    
    if (i < n_inspections) {
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
  
  cat("ok.\n")
  
  kwb.utils::catAndRun("  Writing B-blocks (inspection data) ... ", {
    
    b_at <- offset + 1L + 
      4L * (seq_len(n_inspections) - 1L) + 
      c(0L, c_sizes[-n_inspections])
    
    out_lines[b_at] <- inspectionHeaderLine(names(inspections), sep)
    out_lines[b_at + 1L] <- to_csv(inspections)
  })
  
  kwb.utils::catAndRun("  Writing C-blocks (observation data) ... ", {
    
    out_lines[b_at + 2L] <- observationHeaderLine(names(observations), sep)
    z_at <- b_at[-1L] - 1L
    skip_indices <- c(b_at, b_at + 1L, b_at + 2L, z_at)
    n_rows <- c_sizes[n_inspections] + 4 * n_inspections - 1L + offset
    c_body_at <- setdiff(seq(offset + 1L, n_rows), skip_indices)
    out_lines[c_body_at] <- to_csv(observations)
  })
  
  out_lines[z_at] <- "#Z"
  
  out_lines
}
