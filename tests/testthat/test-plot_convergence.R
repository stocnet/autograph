test_that("monan diagnostics works", {
  skip_on_os("linux") # CRAN R CMD check issue
  p <- plot(monan_conv)
  expect_s3_class(p, "ggplot")  
})

test_that("ergm diagnostics works", {
  skip_on_os("linux") # CRAN R CMD check issue
  p <- plot(ergm_res)
  expect_s3_class(p, "ggplot")
})
