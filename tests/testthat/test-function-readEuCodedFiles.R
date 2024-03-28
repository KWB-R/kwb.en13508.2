test_that("readEuCodedFiles() works", {
  
  f <- kwb.en13508.2:::readEuCodedFiles
  
  expect_error(f(dbg = FALSE))
  
  
})
