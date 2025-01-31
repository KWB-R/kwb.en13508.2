test_that("extractInspectionData_v2() works", {

  f <- kwb.en13508.2:::extractInspectionData_v2
  
  expect_error(f())
})
