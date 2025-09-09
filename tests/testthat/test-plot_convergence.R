test_that("monan diagnostics works", {
  p <- plot(monan_conv)
  expect_s3_class(p, "ggplot")  
})

test_that("ergm diagnostics works", {
  p <- plot(ergm_res$sample)
  expect_s3_class(p, "ggplot")  
})
