library(testthat)

test_that("setGlobalInspectionID() works", {

  f <- kwb.en13508.2:::setGlobalInspectionID

  expect_error(f())

  file <- file.path(tempdir(), "duplicates.txt")
  
  expect_error(f(
    inspection.data = list(
      inspections = data.frame(
        inspection_date = "2024-01-03",
        inspection_time = "22:22",
        node_1_ref = "A",
        node_2_ref = "B"
      )[c(1, 1), ],
      observations = data.frame(
        inspno = 1L
      ),
      header.info = list()
    ),
    project = "Lausanne", 
    name.convention = "snake",
    file = file
  ))

  expect_true(file.exists(file))  
})
