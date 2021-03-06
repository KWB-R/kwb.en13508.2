#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("getInspectionNumbers() works", {

  expect_error(
    kwb.en13508.2:::getInspectionNumbers(indices.C = 1, indices.B01 = 1)
    # argument "indices.B" is missing, with no default
  )
   expect_error(
    kwb.en13508.2:::getInspectionNumbers(indices.C = 1, indices.B01 = list(key = c("a", "b"), value = 1:2))
    # (list) object cannot be coerced to type 'double'
  )
   expect_error(
    kwb.en13508.2:::getInspectionNumbers(indices.C = 1:2, indices.B01 = list(key = c("a", "b"), value = 1:2))
    # (list) object cannot be coerced to type 'integer'
  )
   expect_error(
    kwb.en13508.2:::getInspectionNumbers(indices.C = "a", indices.B01 = as.POSIXct("2018-06-03 23:50:00"))
    # character string is not in a standard unambiguous format
  )
   expect_error(
    kwb.en13508.2:::getInspectionNumbers(indices.C = TRUE, indices.B01 = list(key = c("a", "b"), value = 1:2))
    # (list) object cannot be coerced to type 'logical'
  )

})

