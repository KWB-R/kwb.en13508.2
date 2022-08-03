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
  
  n_inspections <- nrow(inspections)
  
  # Function to create output file name indicating first and last inspection
  fileToOutputFile <- function(i, j) {
    pattern <- paste0("%0", nchar(n_inspections), "d")
    postfix <- sprintf(paste0("_", pattern, "_", pattern, ".txt"), i, j)
    gsub("\\.txt$", postfix, file)
  }
  
  for (block_number in seq_len(ceiling(n_inspections / blocksize))) {
    
    i <- (block_number - 1L) * blocksize + 1L
    j <- min(block_number * blocksize, n_inspections)
    
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
