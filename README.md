
<!-- README.md is generated from README.Rmd. Please edit that file -->
kwb.en13508.2
=============

[![Build Status](https://travis-ci.org/KWB-R/kwb.en13508.2.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.en13508.2)

This R package allows you to read and write data on sewerage inspections in the text format described in the European Norm [EN 13508-2](http://www.dwa.de/dwa/shop/shop.nsf/Produktanzeige?openform&produktid=P-DWAA-8KTG5R).

Install the Package
-------------------

You can install the latest development version of the package from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("KWB-R/kwb.en13508.2", build_vignettes = TRUE)
```

Load the Package
----------------

``` r
library(kwb.en13508.2)
```

Read a File Encoded in EN 13508.2 Format
----------------------------------------

You can read a file formatted in EN 13508.2 text format using `readEuCodedFile()`.

For the purpose of demonstration, we load an example file that is contained in the package:

``` r
# Set the relative path to the example file
file_path <- "extdata/example_13508_2.txt"

# Set the absolute path to the example file in the package folder
input_file <- system.file(file_path, package = "kwb.en13508.2")

# Read the example file
survey <- readEuCodedFile(input_file)
#> *** Reading input file /home/hsonne/R/packages/kwb.en13508.2/extdata/example_13508_2.txt ... *** ok.
#> *** Removing empty lines (if any) ... *** ok.
#> *** Extracting file header ... *** ok.
#> *** Extracting inspection records ... 19 inspections extracted. *** ok.
#> *** Extracting observation records ... A 'virtual' inspection block end '#Z' has been added.
#> Provide column 'inspno' to data frame 'observations'... ok.
#> 335 observations extracted. *** ok.
```

The result is a list with the three components representing the A-, B- and C- parts, respectively, of the file:

-   `header.info`: A-part containing information on the file format
-   `inspections`: B-parts containing information on the inspections as a whole
-   `observations`: C-parts containing information on the detailed observations that were made during the inspections

### Part A: Information on the file format

``` r
survey$header.info
#> $encoding
#> [1] "ISO-8859-1"
#> 
#> $language
#> [1] "de"
#> 
#> $separator
#> [1] ","
#> 
#> $decimal
#> [1] "."
#> 
#> $quote
#> [1] "\""
#> 
#> $year
#> [1] "2010"
```

### Part B: Inspections

``` r
head(survey$inspections)
#>   ACC ACB ABC                     ABA  ABQ AAD  AAB AAK  AAF        ABF
#> 1 400 400   A EN 13508-2:2003+A1:2011 23.5   6 2925   A 2925 2007-07-19
#> 2 300 300   A EN 13508-2:2003+A1:2011 12.3   6 2942   A 2942 1999-02-25
#> 3 200 200   A EN 13508-2:2003+A1:2011 49.1   8    8   B    7 2001-01-02
#> 4 200 200   A EN 13508-2:2003+A1:2011 49.5   8    7   A    7 2009-08-14
#> 5 400 400   A EN 13508-2:2003+A1:2011 32.7  11   11   B   10 2001-01-03
#> 6 400 400   A EN 13508-2:2003+A1:2011 13.1  14   14   B   13 2001-02-14
#>     ABG                    ABH AAA
#> 1 09:05 Mr. Clean/Mrs. Perfect   1
#> 2 12:51         Mr. Incredible   2
#> 3 14:51           Mrs. Perfect   3
#> 4 11:19              Mr. Clean   4
#> 5 21:19           Mrs. Perfect   9
#> 6 22:37         Mrs. Pragmatic  15
```

The codes used as column names correspond to the codes defined in the norm. We provide a function `getCodes()` in the package that returns a table mapping these codes to their meanings:

``` r
# Get all codes and meanings defined in the European Norm
code_info <- getCodes(fields = c("Code", "Text_EN"))

# Show the meanings for the codes that are used in the table of inspections
code_info[code_info$Code %in% names(survey$inspections), ]
#>    Code                          Text_EN
#> 15  AAA        Pipeline length reference
#> 16  AAB             Start node reference
#> 18  AAD                 Node 1 reference
#> 20  AAF                 Node 2 reference
#> 25  AAK          Direction of inspection
#> 37  ABA                         Standard
#> 39  ABC     Longitudinal reference point
#> 42  ABF               Date of inspection
#> 43  ABG               Time of inspection
#> 44  ABH                Name of inspector
#> 53  ABQ Anticipated length of inspection
#> 58  ACB                           Height
#> 59  ACC                            Width
```

### Part C: Observations

``` r
head(survey$observations)
#>     I N   A B C    D  G  H K                       F M  J inspno
#> 1 0.0   BCD X P 2925 NA NA                                     1
#> 2 0.5   BAC B     59 12 12 A                         4         1
#> 3 3.4   BAB A A    1 12 NA A [Chr1 Y->A] [Chr2 Y->A]           1
#> 4 3.4   BAB B A    2  6 NA A [Chr1 Y->B] [Chr2 Y->A] 5         1
#> 5 4.4   BAB B A    2  6 NA               [Chr1 Y->B]   A2      1
#> 6 4.4   BAB B A    2 12 NA               [Chr1 Y->B] 6 A1      1
```

Again, let's have a look at what the columns mean:

``` r
# Show the meanings for the codes that are used in the table of inspections
code_info[code_info$Code %in% names(survey$observations), ]
#>    Code                           Text_EN
#> 1     A                         Main code
#> 2     B                Characterisation 1
#> 3     C                Characterisation 2
#> 4     D                  Quantification 1
#> 6     F                           Remarks
#> 7     G        Circumferential location 1
#> 8     H        Circumferential location 2
#> 9     I Longitudinal or vertical location
#> 10    J            Continuous defect code
#> 11    K                             Joint
#> 13    M              Photograph reference
#> 14    N                   Video reference
```

The column `inspno` groups together observations that belong to the same inspection. The number in the column refers to the row number in the table of inspections (`survey$inspections`). For example, to get all the observations that belong to the third inspection, you may filter `survey$observations` like in the following. For a more compact output we exclude the tenth column ("F") containing the remarks:

``` r
survey$observations[survey$observations$inspno == 3, -10]
#>       I    N   A B C  D  G  H K M  J inspno
#> 50  0.0 4617 BCD X P  8 NA NA             3
#> 51  1.6 4645 BAB A A  1 12 NA A           3
#> 52  1.6 4651 BAC B   39 12 NA A           3
#> 53  1.6 4657 BAJ B   15  6 NA A           3
#> 54  3.6 4720 BAJ B   15  6 NA A           3
#> 55  4.6 4732 BAJ B   20  6 NA A           3
#> 56 14.0 4820 BBC C    6 12 12             3
#> 57 15.5 4855 BBA A    3 12 12     A1      3
#> 58 18.6 4923 BBA A    3 12 12     B1      3
#> 59 30.8 5028 BBA A    2 12 12             3
#> 60 48.4 5211 BAC B   45 12 12 A           3
#> 61 49.1 5225 BCE X P  7 NA NA             3
```

The information on the inspection is found in the `inspno`-th row (here: third row) of `config$inspections`:

``` r
survey$inspections[3, ]
#>   ACC ACB ABC                     ABA  ABQ AAD AAB AAK AAF        ABF
#> 3 200 200   A EN 13508-2:2003+A1:2011 49.1   8   8   B   7 2001-01-02
#>     ABG          ABH AAA
#> 3 14:51 Mrs. Perfect   3
```

Write a File Encoded in EN 13508.2 Format
-----------------------------------------

Once you have prepared a list with the three components `header.info`, `inspections` and `observations` as described above, you can use the function `writeEuCodedFile()` to write a file formatted in EN-format:

``` r
# Define the path to an output file
output_file <- file.path(tempdir(), "example_en13508.2.txt")

# Write the CCTV survey data to the file
writeEuCodedFile(survey, output_file)
#> *** Formatting lines ... ok.
#>   Writing B-blocks (inspection data) ... ok.
#>   Writing C-blocks (observation data) ... ok.
#> *** ok.
#> *** Writing lines to /tmp/RtmpWbedKx/example_en13508.2.txt ... *** ok.
```

The first 20 lines of the file produced read like that:

``` r
kwb.utils::catLines(readLines(output_file, 20))
#> #A1=ISO-8859-1
#> #A2=de
#> #A3=,
#> #A4=.
#> #A5="
#> #A6=2010
#> #B01=ACC,ACB,ABC,ABA,ABQ,AAD,AAB,AAK,AAF,ABF,ABG,ABH,AAA
#> 400,400,A,EN 13508-2:2003+A1:2011,23.5,6,2925,A,2925,2007-07-19,09:05,Mr. Clean/Mrs. Perfect,1
#> #C=I,N,A,B,C,D,G,H,K,F,M,J
#> 0,,BCD,X,P,2925,,,,,,
#> 0.5,,BAC,B,,59,12,12,A,,4,
#> 3.4,,BAB,A,A,1,12,,A,[Chr1 Y->A] [Chr2 Y->A],,
#> 3.4,,BAB,B,A,2,6,,A,[Chr1 Y->B] [Chr2 Y->A],5,
#> 4.4,,BAB,B,A,2,6,,,[Chr1 Y->B],,A2
#> 4.4,,BAB,B,A,2,12,,,[Chr1 Y->B],6,A1
#> 5.1,,BAB,B,A,2,6,,,[Chr1 Y->B],,B2
#> 5.4,,BAB,B,A,2,12,,,[Chr1 Y->B],,B1
#> 6.4,,BAB,A,A,1,6,,A,[Chr1 Y->A] [Chr2 Y->A],,
#> 9.4,,BAB,A,A,1,6,,,[Chr1 Y->A],,A3
#> 9.4,,BAB,A,A,1,12,,,[Chr1 Y->A],,A4
```
