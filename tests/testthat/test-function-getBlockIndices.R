#
# This file was generated by kwb.test::create_test_files(), 
# launched by hsonne on 2024-11-01 18:19:55.697403.
# Please modify the dummy functions so that real cases are
# tested. Then, delete this comment.
#

test_that("getBlockIndices() works", {

  f <- kwb.en13508.2:::getBlockIndices

  expect_error(
    f()
    # Argument "text" fehlt (ohne Standardwert)
  )

})
