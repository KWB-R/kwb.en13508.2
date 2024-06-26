% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractObservationData.R
\name{extractObservationBlocks}
\alias{extractObservationBlocks}
\title{Extract Lines Between #C-Header and #Z End Tag}
\usage{
extractObservationBlocks(euLines, headerInfo, uniqueKey)
}
\arguments{
\item{euLines}{text lines read from EN13508.2-coded file}

\item{headerInfo}{data frame with information about header lines}

\item{uniqueKey}{identifier of C-header row, as given in 
\code{headerInfo$uniqueKey}}
}
\value{
list of vectors of character representing the "body" lines
  below the #C-headers of type specified in \code{uniqueKey}
}
\description{
Extract Lines Between #C-Header and #Z End Tag
}
