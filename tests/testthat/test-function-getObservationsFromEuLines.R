#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("getObservationsFromEuLines() works", {

  expect_error(
    kwb.en13508.2:::getObservationsFromEuLines(eu_lines = 1, header.info = 1)
    # non-character argument
  )
   expect_error(
    kwb.en13508.2:::getObservationsFromEuLines(eu_lines = "a", header.info = 1)
    # kwb.utils::allAreEqual(c_headers) is not TRUE
  )

})
