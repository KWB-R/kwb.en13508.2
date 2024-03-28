test_that("mergeInspectionData() works", {

  f <- kwb.en13508.2:::mergeInspectionData
  
  f(x = 1)
  
  expect_error(f(x = 1:2))

  x <- list(
    list(
      header.info = "",
      inspections = data.frame(file = "a", id = 1:2),
      observations = data.frame(inspno = c(1L, 1L, 2L, 2L, 2L))
    ),
    list(
      header.info = "",
      inspections = data.frame(file = "b", id = 1:3),
      observations = data.frame(inspno = c(1L, 2L, 3L))
    )
  )
  
  result <- f(x)
  
  expect_identical(
    result$inspections, 
    rbind(x[[1L]]$inspections, x[[2L]]$inspections)
  )
  
  expect_identical(
    result$observations$inspno, 
    c(1L, 1L, 2L, 2L, 2L, 3L, 4L, 5L)
  )
})
