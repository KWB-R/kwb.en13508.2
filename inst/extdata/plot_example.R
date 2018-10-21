# Install and load packages ----------------------------------------------------
#devtools::install_github("guiastrennec/ggplus")
library(ggplot2)
library(ggplus)
library(kwb.utils)

# Load example data ------------------------------------------------------------
file_path <- "extdata/example_13508_2.txt"
input_file <- system.file(file_path, package = "kwb.en13508.2")
survey <- kwb.en13508.2::readEuCodedFile(input_file)

plot_observations(survey)