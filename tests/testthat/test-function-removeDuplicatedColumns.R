test_that("removeDuplicatedColumns() works", {

  f <- kwb.en13508.2:::removeDuplicatedColumns
  
  expect_error(f())
})
