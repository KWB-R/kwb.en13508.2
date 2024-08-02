#kwb.utils::assignPackageObjects("kwb.en13508.2")
test_that("extractObservationData_1() works", {

  f <- kwb.en13508.2:::extractObservationData_1
  
  expect_error(f())
  
  eu_lines <- c(
    "#C=A;B",
    "1;2"
  )
  
  header.info <- euCodedFileHeader()
  
  expect_error(capture.output(f(eu_lines, header.info)))
})
