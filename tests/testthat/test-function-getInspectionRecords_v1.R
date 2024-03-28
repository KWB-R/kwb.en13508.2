test_that("getInspectionRecords_v1() works", {

  f <- kwb.en13508.2:::getInspectionRecords_v1
  
  expect_error(f())

  eu_lines <- c(
    "#A1=x", 
    "#A2=y"
  )
  
  expect_null(f(eu_lines))
})
