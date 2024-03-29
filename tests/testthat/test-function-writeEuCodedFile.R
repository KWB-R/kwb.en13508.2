#library(testthat)
#kwb.utils::assignPackageObjects("kwb.en13508.2")
test_that("writeEuCodedFile() works", {
  
  f <- kwb.en13508.2:::writeEuCodedFile
  
  expect_error(f(dbg = FALSE))
  expect_error(f(list(), dbg = FALSE), "No such element")

  data.1 <- getExampleData()
  
  # Write example data to temporary file
  output.file <- tempfile()
  
  f(data.1, output.file, dbg = FALSE)
  
  # Read tempoary file
  data.2 <- readEuCodedFile(output.file, dbg = FALSE)
  
  expect_identical(data.1, data.2)
})
