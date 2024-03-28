test_that("warnOnDifferingHeaders() works", {

  f <- kwb.en13508.2:::warnOnDifferingHeaders
  
  expect_error(f(x = 1))

  expect_error(f(list(list(), list())))

  expect_warning(f(list(
    list(header.info = list(a = 1)), 
    list(header.info = list(a = 2))
  )))

  expect_silent(f(list(
    list(header.info = list(a = 1)), 
    list(header.info = list(a = 1))
  )))
  
})
