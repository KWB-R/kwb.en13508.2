# plot_observations ------------------------------------------------------------
plot_observations <- function(survey, to_pdf = TRUE)
{
  # Prepare data frame "x" for plotting
  x <- get_extended_observations(survey)
  
  OBS_PIPE <- "_PIPE_"
  
  x$Observation <- sprintf("%s (%s)", x$CodeMeaning, x$A)
  x$Observation[grepl("^(Start|Finish) node", x$CodeMeaning)] <- OBS_PIPE
  x$Observation[x$A == "PIP"] <- OBS_PIPE
  
  x$Inspection <- sprintf("Pipe \"%s\", %s %s", x$AAA, x$ABF, x$ABG)
  
  # Create gg_object from data
  gg_object <- ggplot2::ggplot(x, ggplot2::aes_string("I", "Observation")) + 
    ggplot2::geom_point() + 
    ggplot2::geom_line(
    data = remove_point_damages, ggplot2::aes_string(
      group = "ldid", col = "factor(ldidno)"
    )
  ) + ggplot2::theme(legend.position = "none")
  
  # Plot gg_objects into PDF-file
  pdf_file <- kwb.utils::preparePdfIf(to_pdf, landscape = FALSE)
  
  ggplus::facet_multiple(gg_object, facets = "Inspection", ncol = 1, nrow = 6)
  
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
  
  ins_columns <- c("inspno", "AAA", "ABF", "ABG")
  ins <- kwb.utils::selectColumns(inspections, ins_columns)
  
  # Get observations with extreme positions
  extremes <- get_extreme_positions(inspections)
  observations <- kwb.utils::safeRowBind(observations, extremes)
  
  obs_columns <- c("inspno", "I", "A", "B", "C", "J")
  obs <- kwb.utils::selectColumns(observations, obs_columns)

  # Add start and end positions and code meanings to observations
  codes <- get_code_meanings()
  obs <- merge(obs, codes, by.x = "A", by.y = "Code", all.x = TRUE)
  obs <- merge(obs, ins, by = "inspno")
  obs <- kwb.rerau::prepareForScoring(obs)
  obs$ldidno <- as.integer(gsub("^[AB]", "", obs$J))
  obs$ldid <- paste0(obs$A, obs$B, obs$C, sprintf("%02d", obs$ldidno))
  obs$ldid[kwb.utils::isNaOrEmpty(obs$J)] <- ""

  key_columns <- c("inspno", "I", "A", "B", "C")
  
  obs <- order_by(obs, key_columns)
  
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


# get_code_meanings ------------------------------------------------------------
get_code_meanings <- function()
{
  # Provide table that maps codes to their meanings
  code_meanings <- kwb.utils::resetRowNames(do.call(rbind, lapply(
    X = sprintf("T%d", 4:7), 
    FUN = kwb.en13508.2::getCodes, 
    fields = c("Table", "Code", "Text_EN")
  )))
  
  stats::setNames(code_meanings, c("CodeTable", "Code", "CodeMeaning"))  
}

# order_by ---------------------------------------------------------------------
order_by <- function(df, columns)
{
  keys <- kwb.utils::selectColumns(df, columns)
  
  kwb.utils::resetRowNames(df[do.call(order, keys), ])
}

# remove_point_damages ---------------------------------------------------------
remove_point_damages <- function(x)
{
  x[kwb.utils::selectColumns(x, "ldid") != "", ]
}
