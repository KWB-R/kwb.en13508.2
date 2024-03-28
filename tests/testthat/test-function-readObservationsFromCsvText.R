test_that("readObservationsFromCsvText() works", {

  f <- kwb.en13508.2:::readObservationsFromCsvText

  expect_error(f())

})
