#library(testthat)
test_that("readAndMergeEuCodedFiles() works", {

  f <- kwb.en13508.2:::readAndMergeEuCodedFiles
  
  expect_error(f())
  
  file <- kwb.en13508.2:::getExampleFile()
  files <- c(file, file)
    
  result_camel <- f(files, name.convention = "camel", dbg = FALSE)
  result_snake <- f(files, name.convention = "snake", dbg = FALSE)
  
  check_top_level <- function(x) {
    expect_identical(names(x), c("header.info", "inspections", "observations"))
  }
  
  check_top_level(result_camel)
  check_top_level(result_snake)
  
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
