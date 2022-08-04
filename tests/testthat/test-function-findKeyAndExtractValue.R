test_that("findKeyAndExtractValue() works", {

  f <- kwb.en13508.2:::findKeyAndExtractValue
  
  expect_error(f())
  
  expect_warning(suppressMessages(
    result <- f(c("A1", "B", "C2"), "A")
  ))
  
  expect_identical(result, NA)
  
  expect_identical(f(c("#ABC=1", "B", "C2"), "A"), "1")
  expect_identical(f(c("#ABC=1", "#B=hallo", "C2"), "B"), "hallo")
})
