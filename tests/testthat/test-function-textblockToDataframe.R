test_that("textblockToDataframe() works", {

  f <- kwb.en13508.2:::textblockToDataframe
  
  expect_error(f())
})
