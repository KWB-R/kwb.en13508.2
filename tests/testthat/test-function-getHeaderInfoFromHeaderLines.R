test_that("getHeaderInfoFromHeaderLines() works", {

  f <- kwb.en13508.2:::getHeaderInfoFromHeaderLines

  expect_error(f())
  
  expect_warning(suppressMessages(f(c("a", "b"))))
  
  header.lines <- c("#A1=a", "#A2=b", "#A3=c", "#A4=d", "#A5=e", "#A6=f")
  
  result <- f(header.lines)
  
  expect_identical(unname(unlist(result)), letters[1:6])
})
