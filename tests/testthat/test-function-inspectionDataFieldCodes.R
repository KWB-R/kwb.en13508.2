test_that("inspectionDataFieldCodes() works", {

  f <- kwb.en13508.2:::inspectionDataFieldCodes

  result <- f()
  
  expect_type(result, "list")
  expect_true(kwb.utils::allAreIdentical(lapply(result, names)))
  expect_identical(names(result[[1L]]), c("class", "meaning"))
})
