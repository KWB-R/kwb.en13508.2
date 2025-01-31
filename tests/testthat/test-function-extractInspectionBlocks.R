#library(testthat)

test_that("extractInspectionBlocks() works", {

  f <- kwb.en13508.2:::extractInspectionBlocks
  expect_error(f(text = 1, headerInfos = 1))

})
