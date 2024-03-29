test_that("getFileHeaderFromEuLines() works", {

  f <- kwb.en13508.2:::getFileHeaderFromEuLines

  expect_error(f())

})
