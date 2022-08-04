test_that("getInspectionRecords_v2() works", {

  f <- kwb.en13508.2:::getInspectionRecords_v2
  
  expect_error(f())
})
