test_that("getHeaderLinesFromEuCodedLines() works", {

  f <- kwb.en13508.2:::getHeaderLinesFromEuCodedLines
  
  expect_error(f())

  expect_identical(f(c("#A123", "#B", "#C")), "#A123")
})
