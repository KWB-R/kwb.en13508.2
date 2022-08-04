#library(testthat)

test_that("getCodes() works", {

  f <- kwb.en13508.2:::getCodes
  
  result <- f()

  expect_true(is.data.frame(result))
  expect_true(all(c("Table", "Code", "Name") %in% names(result)))
})
