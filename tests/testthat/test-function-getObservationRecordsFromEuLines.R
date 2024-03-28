test_that("getObservationRecordsFromEuLines() works", {

  f <- kwb.en13508.2:::getObservationRecordsFromEuLines

  expect_error(f())

})
