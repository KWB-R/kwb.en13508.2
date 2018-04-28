% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge.R
\name{readAndMergeEuCodedFiles}
\alias{readAndMergeEuCodedFiles}
\title{Read and Merge Files in EN13508.2-Format}
\usage{
readAndMergeEuCodedFiles(input.files, dbg = FALSE)
}
\arguments{
\item{input.files}{full path to text file containing CCTV inspection results
in the format described in DIN EN 13508-2}

\item{dbg}{if \code{TRUE} debug messages are shown}
}
\description{
Read files in EN13508.2-format using \code{\link{readEuCodedFiles}} and merge
them by means of \code{\link{mergeInspectionData}}
}