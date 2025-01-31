#library(testthat)
test_that("extractObservationData_2() works", {

  f <- kwb.en13508.2:::extractObservationData_2
  
  expect_error(f())

  euLines <- c(
    "#C=I,A,B", #1
    "1.0,a,b",  #2
    "#Z",       #3
    "#C=I,A",   #4
    "1.1,a",    #5
    "#Z"        #6
  )
  
  headerInfo <- data.frame(
    row = as.integer(c(1, 3, 4, 6)),
    type = c("C", "Z", "C", "Z"),
    value = c("I,A,B", "", "I,A", ""),
    uniqueKey = c("1", "", "2", ""),
    inspno = 1L
  )

  header.info <- list(
    separator = ",",
    quote = '"',
    decimal = "."
  )
  
  result <- f(euLines, headerInfo, header.info)
  result_text <- f(euLines, headerInfo, header.info, as.text = TRUE)
  
  expect_s3_class(result, "data.frame")
  expect_s3_class(result_text, "data.frame")
  
  classes <- function(x) sapply(x, class)
  
  expect_identical(classes(result), c(
    inspno = "integer", 
    A = "character", 
    B = "character", 
    I = "numeric"
  ))
  
  expect_identical(classes(result_text), c(
    inspno = "integer", 
    A = "character", 
    B = "character", 
    I = "character"
  ))
})
