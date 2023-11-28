#library(testthat)
test_that("extractObservationBlocks() works", {

  f <- kwb.en13508.2:::extractObservationBlocks
  
  expect_error(f())

})
