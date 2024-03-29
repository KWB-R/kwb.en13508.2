test_that("getExampleFile() works", {

  f <- kwb.en13508.2:::getExampleFile

  expect_true(file.exists(f()))

})
