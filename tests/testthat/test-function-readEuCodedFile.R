#kwb.utils::assignPackageObjects("kwb.en13508.2")
#library(testthat)
test_that("readEuCodedFile() works", {

  f <- kwb.en13508.2:::readEuCodedFile
  
  expect_error(f())

  file <- kwb.en13508.2:::getExampleFile()
  
  result_1 <- f(file, dbg = FALSE)
  result_2 <- f(file, dbg = FALSE, name.convention = "camel")
  result_3 <- f(file, dbg = FALSE, name.convention = "snake")

  check_top_level <- function(x) {
    expect_type(x, "list")
    expect_identical(names(x), c("header.info", "inspections", "observations"))
  }
  
  check_top_level(result_1)
  check_top_level(result_2)
  check_top_level(result_3)
  
  expect_true(all(c("AAA", "AAB", "AAD") %in% names(result_1$inspections)))
  expect_true(all(c("A", "B", "C") %in% names(result_1$observations)))

  expect_true(all(c("Node1Ref", "Node2Ref") %in% names(result_2$inspections)))
  expect_true(all(c("MainCode", "Char1", "Char2") %in% names(result_2$observations)))
  
  expect_true(all(c("node_1_ref", "node_2_ref") %in% names(result_3$inspections)))
  expect_true(all(c("main_code", "char_1", "char_2") %in% names(result_3$observations)))
})
