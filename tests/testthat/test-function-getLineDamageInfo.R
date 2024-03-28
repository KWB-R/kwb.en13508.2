#library(testthat)

test_that("getLineDamageInfo() works", {

  f <- kwb.en13508.2::getLineDamageInfo
  
  expect_error(f())

  expect_message(result <- f(data.frame()))
  expect_null(result)

  observations <- read.csv(text = "
    inspno,I,J
    1,1.0,A1
    1,2.0,
    1,3.0,B1"
  )
  
  expected <- read.csv(colClasses = c(ldno = "character"), text = "
    ino,ldno,beg.at,end.at,beg.x,end.x,length
    1,1,1,3,1.0,3.0,2.0"
  )

  expect_identical(f(observations), expected)
  
  observations <- read.csv(text = "
    inspno,I,J
    1,0.0,
    1,1.0,A2
    1,2.0,
    1,3.0,C2
    1,4.0,C2
    1,5.0,
    1,6.0,B2
    1,7.0,"
  )
  
  expected <- read.csv(colClasses = c(ldno = "character"), text = "
    ino,ldno,beg.at,end.at,beg.x,end.x,length
    1,2,2,7,1.0,6.0,5.0"
  )
  
  expect_identical(f(observations), expected)
})
