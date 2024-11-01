test_that("convertTypes() works", {

  f <- kwb.en13508.2:::convertTypes

  expect_error(f())

  expect_message(result <- f(
    data.frame(a = c(1, 1.9), b = c("x", "1.0")), 
    classes = c(a = "integer", b = "numeric"), 
    dbg = FALSE
  ))
  
  expect_identical(result, data.frame(a = c(1L, 1L), b = c(NA, 1.0)))
})
