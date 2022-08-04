#kwb.utils::assignPackageObjects("kwb.en13508.2")

test_that("readEuCodedFile() works", {

  f <- kwb.en13508.2:::readEuCodedFile
  
  expect_error(f())

  result <- f(getExampleFile(), dbg = FALSE)
  
  expect_type(result, "list")
  expect_identical(names(result), c("header.info", "inspections", "observations"))
})
