#kwb.utils::assignPackageObjects("kwb.en13508.2")

test_that("readEuCodedFile() works", {

  f <- kwb.en13508.2:::readEuCodedFile
  
  expect_error(f())

  result.1 <- f(getExampleFile(), dbg = FALSE)
  result.2 <- f(getExampleFile(), dbg = FALSE, short.names = FALSE)
  
  expect_type(result.1, "list")
  expect_identical(names(result.1), c("header.info", "inspections", "observations"))
  
  expect_true(all(c("AAA", "AAB", "AAD") %in% names(result.1$inspections)))
  expect_true(all(c("A", "B", "C") %in% names(result.1$observations)))

  expect_true(all(c("Node1Ref", "Node2Ref") %in% names(result.2$inspections)))
  expect_true(all(c("MainCode", "Char1", "Char2") %in% names(result.2$observations)))
})
