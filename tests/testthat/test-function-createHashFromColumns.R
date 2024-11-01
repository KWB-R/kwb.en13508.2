test_that("createHashFromColumns() works", {
  f <- kwb.en13508.2:::createHashFromColumns
  expect_error(f())
})
