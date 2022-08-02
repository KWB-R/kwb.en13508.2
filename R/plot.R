# plotObservations -------------------------------------------------------------

#' Plot Observations per Pipe
#' 
#' @param survey list with elements \code{inspections} and \code{observations}
#'   as e.g. returned by \code{kwb.en13508.2:::readEuCodedFile}
#' @param to_pdf if \code{TRUE} (default) the output goes into a temporary PDF
#'   file
#' @param matrix_dim vector of two specifying the number of rows and columns,
#'   respectivly of the matrix in which to arrange the single plots.
#' 
#' @export
#' 
#' @examples 
#' # Install packages if not yet installed
#' \dontrun{
#' install.packages("ggplot2")
#' devtools::install_github("guiastrennec/ggplus")
#' }
#' 
#' # Load example data
#' file <- system.file("extdata/example_13508_2.txt", package = "kwb.en13508.2")
#' survey <- kwb.en13508.2::readEuCodedFile(file)
#' 
#' # Create one plot per inspection in "survey"
#' kwb.en13508.2::plotObservations(survey, to_pdf = FALSE)
#' 
plotObservations <- function(survey, to_pdf = TRUE, matrix_dim = c(7, 1))
{
  # Prepare data frame "x" for plotting
  x <- get_extended_observations(survey)
  
  OBS_PIPE <- "_PIPE_"
  
  x$Observation <- sprintf("%s (%s)", x$CodeMeaning, x$A)
  x$Observation[grepl("^(Start|Finish) node", x$CodeMeaning)] <- OBS_PIPE
  x$Observation[x$A == "PIP"] <- OBS_PIPE
  
  x$Inspection <- sprintf("Pipe \"%s\", %s %s", x$AAA, x$ABF, x$ABG)

  # Define plot function  
  plot_x <- function(x) {
    ggplot2::ggplot(x, ggplot2::aes_string("I", "Observation")) + 
      ggplot2::geom_point() + 
      ggplot2::geom_line(
        data = remove_point_damages, ggplot2::aes_string(
          group = "ldid", col = "factor(ldidno)"
        )
      ) + 
      ggplot2::xlab("Position (m)") +
      ggplot2::ylab("") +
      ggplot2::facet_wrap("Inspection", ncol = matrix_dim[2], scales = "free_y") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")
  }

  inspection_numbers <- sort(unique(x$inspno))
  
  n <- length(inspection_numbers)
  
  groups <- rep(seq_len(n), each = prod(matrix_dim))[1:n]
  
  plots <- lapply(
    X = split(inspection_numbers, groups),
    FUN = function(inspnos) {
      plot_x(x[x$inspno %in% inspnos, ])
    }
  )

  # Plot gg_objects into PDF-file
  pdf_file <- kwb.utils::preparePdfIf(to_pdf, landscape = FALSE)
  
  print(plots)
  
  kwb.utils::finishAndShowPdfIf(to_pdf, pdf_file)
}

# get_extended_observations ----------------------------------------------------
get_extended_observations <- function(survey)
{
  inspections <- kwb.utils::selectElements(survey, "inspections")
  observations <- kwb.utils::selectElements(survey, "observations")

  # Provide column "inspno" if not existing
  if (is.null(inspections$inspno)) {
    
    inspections$inspno <- seq_len(nrow(inspections))
  }
  
  # Make sure that numeric columns are numeric
  inspections$ABQ <- as.numeric(inspections$ABQ)
  observations$I <- as.numeric(observations$I)
  
  ins_columns <- c("inspno", "AAA", "ABF", "ABG")
  ins <- kwb.utils::selectColumns(inspections, ins_columns)
  
  # Get observations with extreme positions
  extremes <- get_extreme_positions(inspections)
  observations <- kwb.utils::safeRowBind(observations, extremes)
  
  # Define names of code columns
  code_columns <- c("A", "B", "C")
  
  obs_columns <- c("inspno", "I", code_columns, "J")
  obs <- kwb.utils::selectColumns(observations, obs_columns)

  # Add start and end positions and code meanings to observations
  codes <- getCodeMeanings()
  obs <- merge(obs, codes, by.x = "A", by.y = "Code", all.x = TRUE)
  obs <- merge(obs, ins, by = "inspno")
  
  #obs <- kwb.rerau::prepareForScoring(obs)

  # Replace NA or empty string in columns B and C with dash "-"
  for (column in c("B", "C")) {
    
    obs[[column]][kwb.utils::isNaOrEmpty(obs[[column]])] <- "-"
  }

  obs$ldidno <- as.integer(gsub("^[AB]", "", obs$J))
  obs$ldid <- paste0(obs$A, obs$B, obs$C, sprintf("%02d", obs$ldidno))
  obs$ldid[kwb.utils::isNaOrEmpty(obs$J)] <- ""

  key_columns <- c("inspno", "I", code_columns)
  
  obs <- kwb.utils::orderBy(obs, key_columns)
  
  kwb.utils::moveColumnsToFront(obs, columns = c(key_columns, "ldid", "ldidno"))
}

# get_extreme_positions --------------------------------------------------------
get_extreme_positions <- function(inspections)
{
  inspnos <- seq_len(nrow(inspections))
  
  n <- length(inspnos)
  
  extremes <- kwb.utils::noFactorDataFrame(
    inspno = rep(inspnos, 2),
    A = "PIP",
    I = c(rep(0, n), kwb.utils::selectColumns(inspections, "ABQ")),
    J = c(rep("A0", n), rep("B0", n))
  )
  
  kwb.utils::resetRowNames(extremes[order(extremes$inspno, extremes$J), ])
}


# getCodeMeanings --------------------------------------------------------------
getCodeMeanings <- function()
{
  renamings <- list(
    Table = "CodeTable", 
    Code = "Code",
    Text_EN = "CodeMeaning"
  )
  
  # Provide table that maps codes to their meanings
  code_meanings <- getCodes(
    table = sprintf("T%d", 4:7),
    fields = names(renamings)
  )
  
  kwb.utils::renameColumns(code_meanings, renamings)
}

# remove_point_damages ---------------------------------------------------------
remove_point_damages <- function(x)
{
  x[kwb.utils::selectColumns(x, "ldid") != "", ]
}
