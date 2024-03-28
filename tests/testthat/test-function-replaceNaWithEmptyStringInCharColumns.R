#library(testthat)
test_that("replaceNaWithEmptyStringInCharColumns() works", {

  f <- kwb.en13508.2:::replaceNaWithEmptyStringInCharColumns

  expect_error(f())

  x <- data.frame(
    a = 1:2, 
    b = c("x", NA)
  )
  
  expect_identical(f(x)[["b"]], c("x", ""))
  
})
