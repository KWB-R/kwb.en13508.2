test_that("renameColumnsToMeaningful() works", {

  f <- kwb.en13508.2:::renameColumnsToMeaningful

  expect_error(f())

})
