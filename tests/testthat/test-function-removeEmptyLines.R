test_that("removeEmptyLines() works", {

  f <- kwb.en13508.2:::removeEmptyLines
  
  expect_error(f())
  
  expect_output(result <- f(c("a", "", "b")))
  
  expect_identical(result, c("a", "b"))
})
