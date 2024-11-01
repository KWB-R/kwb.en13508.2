#library(testthat)
test_that("extractInspectionData_v1() works", {

  f <- kwb.en13508.2:::extractInspectionData_v1
  
  header <- list(separator = ",", decimal = ".", quote = "")
  
  expect_error(f())
  expect_error(f(text))
  expect_error(f(text, list()))
  
  expect_error(f(header.info = header, text = c(
    "#B01=a,b"
  )))

  text <- c("#B01=a,b", "a,b")
  expected <- data.frame(a = "a", b = "b")
  expect_identical(f(text, header), expected)

  text <- c("#B01=a,b", "a,b", "#B02=c,d", "c,d")
  expected <- data.frame(a = "a", b = "b", c = "c", d = "d")
  expect_identical(f(text, header), expected)

  text <- c("#B01=a,b", "a,b", "#B02=c,d", "c,d", "#B01=a", "x")
  expect_message(result <- f(text, header))
  expect_null(result)
  
})
