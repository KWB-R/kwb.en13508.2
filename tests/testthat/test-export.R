test_that("euCodedFileHeader() works", {

  kwb.en13508.2:::euCodedFileHeader()
})

test_that("writeEuCodedFiles() works", {

  expect_error(kwb.en13508.2:::writeEuCodedFiles())
})

test_that("writeEuCodedFile() works", {

  expect_error(kwb.en13508.2:::writeEuCodedFile())
})

test_that("toEuFormat_v1() works", {

  expect_error(kwb.en13508.2:::toEuFormat_v1())
})

test_that("getHeaderLinesFromHeaderInfo() works", {

  expect_error(kwb.en13508.2:::getHeaderLinesFromHeaderInfo())
})

test_that("inspectionHeaderLine() works", {

  expect_error(kwb.en13508.2:::inspectionHeaderLine())
})

test_that("observationHeaderLine() works", {

  expect_error(kwb.en13508.2:::observationHeaderLine())
})

test_that("toEuFormat_v2() works", {

  expect_error(kwb.en13508.2:::toEuFormat_v2())
})

