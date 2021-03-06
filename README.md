# kwb.en13508.2

[![Appveyor build status](https://ci.appveyor.com/api/projects/status/i5xx4npr86rg783h/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-en13508-2/branch/master)
[![Build Status](https://travis-ci.org/KWB-R/kwb.en13508.2.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.en13508.2)
[![codecov](https://codecov.io/github/KWB-R/kwb.en13508.2/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.en13508.2)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/kwb.en13508.2)](http://cran.r-project.org/package=kwb.en13508.2)

This R package provides functions to read and write data on CCTV sewerage 
inspections. The format expected when reading and generated when writing is the 
text format that is described in the European Norm 
[EN 13508-2](http://www.dwa.de/dwa/shop/shop.nsf/Produktanzeige?openform&produktid=P-DWAA-8KTG5R).

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.en13508.2' from GitHub

remotes::install_github("kwb-r/kwb.en13508.2")
```

## Load the Package

```r
library(kwb.en13508.2)
```

## Read a File Encoded in EN 13508.2 Format

You can read a file formatted in EN 13508.2 text format using 
`readEuCodedFile()`. 

For the purpose of demonstration, we load an example file that is contained 
in the package:

```r
# Set the relative path to the example file
file_path <- "extdata/example_13508_2.txt"

# Set the absolute path to the example file in the package folder
input_file <- system.file(file_path, package = "kwb.en13508.2")

# Read the example file
survey <- readEuCodedFile(input_file)
```

The result is a list with the three components representing the A-, B- and C-
parts, respectively, of the file:

* `header.info`: A-part containing information on the file format
* `inspections`: B-parts containing information on the inspections as a whole
* `observations`: C-parts containing information on the detailed observations 
that were made during the inspections

### Part A: Information on the file format

```r
survey$header.info
```

### Part B: Inspections

```r
head(survey$inspections)
```

The codes used as column names correspond to the codes defined in the norm. 
We provide a function `getCodes()` in the package that returns a table mapping 
these codes to their meanings:

```r
# Get all codes and meanings defined in the European Norm
code_info <- getCodes(fields = c("Code", "Text_EN"))

# Show the meanings for the codes that are used in the table of inspections
code_info[code_info$Code %in% names(survey$inspections), ]
```


### Part C: Observations

```r
head(survey$observations)
```

Again, let's have a look at what the columns mean:

```r
# Show the meanings for the codes that are used in the table of observations
code_info[code_info$Code %in% names(survey$observations), ]
```

The column `inspno` groups together observations that belong to the same 
inspection. The number in the column refers to the row number in the table of 
inspections (`survey$inspections`). For example, to get all the observations 
that belong to the third inspection, you may filter `survey$observations` like 
in the following. For a more compact output we exclude the tenth column ("F")
containing the remarks:

```r
survey$observations[survey$observations$inspno == 3, -10]
```

The information on the inspection is found in the `inspno`-th row 
(here: third row) of `config$inspections`:

```r
survey$inspections[3, ]
```

## Write a File Encoded in EN 13508.2 Format

Once you have prepared a list with the three components `header.info`, 
`inspections` and `observations` as described above, you can use the function
`writeEuCodedFile()` to write a file formatted in EN 13508.2-format:

```r
# Define the path to an output file
output_file <- file.path(tempdir(), "example_en13508.2.txt")

# Write the CCTV survey data to the file
writeEuCodedFile(survey, output_file)
```
The first 20 lines of the file produced read:

```r
kwb.utils::catLines(readLines(output_file, 20))
```

# Documentation

Release: [https://kwb-r.github.io/kwb.en13508.2](https://kwb-r.github.io/kwb.en13508.2)

Development: [https://kwb-r.github.io/kwb.en13508.2/dev](https://kwb-r.github.io/kwb.en13508.2/dev)
