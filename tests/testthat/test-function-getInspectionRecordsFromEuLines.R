test_that("getInspectionRecordsFromEuLines() works", {

  f <- kwb.en13508.2:::getInspectionRecordsFromEuLines

  expect_error(f())

})
