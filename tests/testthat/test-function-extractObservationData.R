test_that("extractObservationData() works", {

  f <- kwb.en13508.2:::extractObservationData

  expect_error(f())
  expect_error(f("#C", list()))
})
