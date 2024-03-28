test_that("observationHeaderLine() works", {

  f <- kwb.en13508.2:::observationHeaderLine
  
  expect_error(f())
  
  expect_identical(f(c("a", "b", "c"), sep = ","), c("#C=a,b,c"))
})
