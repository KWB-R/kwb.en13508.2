#library(testthat)

test_that("toEuFormat_v1() and toEuFormat_v2() work", {

  f1 <- kwb.en13508.2:::toEuFormat_v1
  f2 <- kwb.en13508.2:::toEuFormat_v1
  
  expect_error(f())

  header <- kwb.en13508.2::euCodedFileHeader()
  
  inspections <- data.frame(AAA = 1:3)
  observations <- data.frame(inspno = 1:3, A = 1:3, B = 2:4)

  r11 <- f1(header, inspections, observations)
  r12 <- f2(header, inspections, observations)
  
  r21 <- f1(header, inspections, observations[c(1L, 3L), ])
  r22 <- f2(header, inspections, observations[c(1L, 3L), ])
  
  expect_identical(r11, r12)
  expect_identical(r21, r22)
})
