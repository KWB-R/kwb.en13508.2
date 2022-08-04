test_that("readAndMergeEuCodedFiles() works", {

  f <- kwb.en13508.2:::readAndMergeEuCodedFiles
  
  expect_error(f())
  
  file <- getExampleFile()
  
  result <- f(c(file, file))
})
