% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numberOfInspections.R
\name{numberOfInspections}
\alias{numberOfInspections}
\title{Number of Inspections}
\usage{
numberOfInspections(x)
}
\arguments{
\item{x}{list of inspection data elements each of which was read from an EN
13508-2-coded file by means of \code{\link{readEuCodedFile}}}
}
\value{
vector of integer representing the number of inspections in each
  element of \code{inspectionDataList}
}
\description{
Get number of inspections from list of inspection data
}
