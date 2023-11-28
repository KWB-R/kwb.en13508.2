#library(testthat)
test_that("readAndMergeEuCodedFiles() works", {

  f <- kwb.en13508.2:::readAndMergeEuCodedFiles
  
  expect_error(f())
  
  file <- kwb.en13508.2:::getExampleFile()
  files <- c(file, file)
    
  result_camel <- f(files, meaningful.names = TRUE)
  result_snake <- f(files, meaningful.names = TRUE, snake.case = TRUE)
  
  elements_ok <- function(x) expect_identical(names(x), c(
    "header.info", "inspections", "observations"
  ))
  
  elements_ok(result_camel)
  elements_ok(result_snake)
  
  expect_true(all(
    c("MainCode", "Char1", "Char2") %in% names(result_camel$observations)
  ))
  
  expect_true(all(
    c("main_code", "char_1", "char_2") %in% names(result_snake$observations)
  ))

  expect_true(!anyNA(result_camel$observations$Char1))
  expect_true(!anyNA(result_camel$observations$Char2))
  
  expect_true(!anyNA(result_snake$observations$char_1))
  expect_true(!anyNA(result_snake$observations$char_2))
})
