#library(testthat)
#kwb.utils::assignPackageObjects("kwb.en13508.2")
test_that("writeEuCodedFile() works", {
  
  f <- kwb.en13508.2:::writeEuCodedFile
  
  expect_error(f(dbg = FALSE))
  expect_error(f(list(), dbg = FALSE), "No such element")

  file <- system.file("extdata/example_13508_2.txt", package = "kwb.en13508.2")

  # Read example file  
  inspection.data <- readEuCodedFile(file, dbg = FALSE)
  
  # Write to temporary file
  output.file <- tempfile()
  f(inspection.data, output.file, dbg = FALSE)
  
  # Read tempoary file
  inspection.data.2 <- readEuCodedFile(output.file, dbg = FALSE)
  
  expect_identical(inspection.data, inspection.data.2)
})
