test_that("getExampleData() works", {

  f <- kwb.en13508.2:::getExampleData
  
  result <- f()

  expect_identical(
    names(result), 
    c("header.info", "inspections", "observations")
  )
})
