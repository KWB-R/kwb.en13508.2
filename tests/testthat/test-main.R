test_that("getCodes() works", {

  kwb.en13508.2:::getCodes()
})

test_that("numberOfInspections() works", {

  expect_error(kwb.en13508.2:::numberOfInspections())
})

test_that("inspectionDataFieldCodes() works", {

  kwb.en13508.2:::inspectionDataFieldCodes()
})

test_that("readPackageFile() works", {

  expect_error(kwb.en13508.2:::readPackageFile())
})

test_that("dataFrameContentToTextLines() works", {

  expect_error(kwb.en13508.2:::dataFrameContentToTextLines())
})

test_that("dataFrameToCsvLines_v1() works", {

  expect_error(kwb.en13508.2:::dataFrameToCsvLines_v1())
})

test_that("dataFrameToCsvLines_v2() works", {

  expect_error(kwb.en13508.2:::dataFrameToCsvLines_v2())
})

test_that("valuesToCsv() works", {

  expect_error(kwb.en13508.2:::valuesToCsv())
})

test_that("quoteTextIfNeeded() works", {

  expect_error(kwb.en13508.2:::quoteTextIfNeeded())
})

