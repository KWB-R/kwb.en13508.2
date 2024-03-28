test_that("getCodeMeanings() works", {

  f <- kwb.en13508.2:::getCodeMeanings

  result <- f()

  expect_identical(names(result), c("CodeTable", "Code", "CodeMeaning"))
})
