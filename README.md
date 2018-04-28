
<!-- README.md is generated from README.Rmd. Please edit that file -->
kwb.en13508.2
=============

[![Build Status](https://travis-ci.org/KWB-R/kwb.en13508.2.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.en13508.2)

This R package allows you to read and write data on sewerage inspections in the text format described in the European Norm EN 13508-2.

Installation
------------

You can install kwb.en13508.2 from github with:

``` r
# install.packages("devtools")
devtools::install_github("kwb-r/kwb.en13508.2", build_vignettes = TRUE)
```

Load the Package
----------------

``` r
library(kwb.en13508.2)
```

Main Functionality
------------------

You can read a file formatted in EN-format using `readEuCodedFile()`:

``` r
inspection_data <- readEuCodedFile(input_file)
```

You can write a file formatted in EN-format using `readEuCodedFile()`:

``` r
writeEuCodedFile(inspection_data, output_file)
```
