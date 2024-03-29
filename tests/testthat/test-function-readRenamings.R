test_that("readRenamings() works", {

  f <- kwb.en13508.2:::readRenamings

  expect_error(f())

})
