#library(testthat)
test_that("extractObservationBlocks() works", {

  f <- kwb.en13508.2:::extractObservationBlocks
  
  expect_error(f())

  euLines <- c(
    "#C=A,B,C", #1
    "1,2,3",    #2
    "#Z",       #3
    "#C=A,B",   #4
    "11,22",    #5
    "#Z"        #6
  )
  
  headerInfo <- data.frame(
    row = as.integer(c(1, 3, 4, 6)),
    type      = c("C", "Z", "C", "Z"),
    uniqueKey = c("1", "", "2", "")
  )
  
  expect_identical(f(euLines, headerInfo, uniqueKey = "no-such-key"), list())
  
  expect_identical(
    f(euLines, headerInfo, uniqueKey = "1"),
    list("1,2,3")
  )
  
  expect_identical(
    f(euLines, headerInfo, uniqueKey = "2"),
    list("11,22")
  )
  
})
