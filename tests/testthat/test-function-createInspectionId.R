#library(testthat)
test_that("createInspectionId() works", {

  f <- kwb.en13508.2:::createInspectionId
  
  expect_error(f())

})
