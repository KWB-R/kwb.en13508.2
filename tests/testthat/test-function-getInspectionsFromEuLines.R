test_that("getInspectionsFromEuLines() works", {

  f <- kwb.en13508.2:::getInspectionsFromEuLines
  
  expect_error(f())

  eu_lines <- c(
    "#A1=x", 
    "#A2=y"
  )
  
  expect_null(f(eu_lines))
})
