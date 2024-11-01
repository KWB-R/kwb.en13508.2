#library(testthat)
test_that("extractObservationData_1() works", {

  header.info <- kwb.en13508.2::euCodedFileHeader()
  
  f <- function(text) {
    kwb.en13508.2:::extractObservationData_1(text, header.info, dbg = FALSE)
  }
  
  expect_error(f())
  
  result <- f(text = c(
    "#C=A;B", 
    "1;2"
  ))
  expect_identical(result, data.frame(inspno = 1L, A = "1", B = "2"))

  expect_error(f(text = c(
    "#C=A;B", 
    "1;2", 
    "#C=A;C", 
    "2;4"
  )))
  
  result <- f(text = c(
    "#B01=", 
    "#C=A;B", 
    "1;3", 
    "#Z", 
    "#B01=", 
    "#C=A;B", 
    "2;4"
  ))
  
  expect_identical(result, data.frame(
    inspno = 1:2, 
    A = as.character(1:2),
    B = as.character(3:4)
  ))
  
})
