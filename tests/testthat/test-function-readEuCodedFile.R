#kwb.utils::assignPackageObjects("kwb.en13508.2")
#library(testthat)
test_that("readEuCodedFile() works", {

  f <- kwb.en13508.2:::readEuCodedFile
  
  expect_error(f())

  result.1 <- f(getExampleFile(), dbg = FALSE)
  result.2 <- f(getExampleFile(), dbg = FALSE, name.convention = "camel")
  result.3 <- f(getExampleFile(), dbg = FALSE, name.convention = "snake")

  check_top_level <- function(x) {
    expect_type(x, "list")
    expect_identical(names(x), c("header.info", "inspections", "observations"))
  }
  
  check_top_level(result.1)
  check_top_level(result.2)
  check_top_level(result.3)
  
  expect_true(all(c("AAA", "AAB", "AAD") %in% names(result.1$inspections)))
  expect_true(all(c("A", "B", "C") %in% names(result.1$observations)))

  expect_true(all(c("Node1Ref", "Node2Ref") %in% names(result.2$inspections)))
  expect_true(all(c("MainCode", "Char1", "Char2") %in% names(result.2$observations)))
  
  expect_true(all(c("node_1_ref", "node_2_ref") %in% names(result.3$inspections)))
  expect_true(all(c("main_code", "char_1", "char_2") %in% names(result.3$observations)))
})
