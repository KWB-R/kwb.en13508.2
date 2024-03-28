test_that("readPackageFile() works", {

  f <- kwb.en13508.2:::readPackageFile
  
  expect_error(f())
  expect_error(f("no-such-file.txt"))
  
  result <- f("eucodes.csv")
  expect_true(is.data.frame(result))
})
