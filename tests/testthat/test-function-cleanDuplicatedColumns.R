test_that("cleanDuplicatedColumns() works", {

  f <- kwb.en13508.2:::cleanDuplicatedColumns
  
  expect_error(f())

  x <- data.frame(a.x = 1:2, a.y = 1:2, id = 1:2)
  
  expect_output(result <- f(x))
  
  expect_identical(result, stats::setNames(x[, -2L, ], c("a", "id")))
})
