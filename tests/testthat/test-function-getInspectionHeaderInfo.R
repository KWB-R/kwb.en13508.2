test_that("getInspectionHeaderInfo() works", {

  f <- kwb.en13508.2:::getInspectionHeaderInfo
  
  expect_error(f())
  
  lines <- c("#X", "#B01=a", "#B02=b", "#Y", "#Z")

  expect_equal(f(lines), list(
    a = list(line = 1L, rows = 2L),
    b = list(line = 2L, rows = 3L)
  ))
})
