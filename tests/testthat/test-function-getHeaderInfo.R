test_that("getHeaderInfo() works", {

  f <- kwb.en13508.2:::getHeaderInfo

  expect_error(f())
  expect_no_error(f(""))
  
  expect_identical(
    f("#B01=a,b"), 
    data.frame(
      row = 1L, 
      type = "B", 
      key = "#B01", 
      uniqueKey = "b1", 
      value = "a,b", 
      inspno = 1L
    )
  )
})
