test_that("monan diagnostics works", {
  p <- plot(res_monan_traces)
  expect_s3_class(p, "ggplot")  
})

test_that("ergm diagnostics works", {
  p <- plot(ergm_res$sample)
  expect_s3_class(p, "ggplot")  
})
