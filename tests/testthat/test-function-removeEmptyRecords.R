test_that("removeEmptyRecords() works", {

  f <- kwb.en13508.2:::removeEmptyRecords
  
  expect_error(f())

})
