test_that("migraph network test plotting works", {
  p <- plot(res_migraph_test)
  expect_s3_class(p, "ggplot")  
})

test_that("migraph network lm plotting works", {
  p <- plot(res_migraph_reg)
  expect_s3_class(p, "ggplot")  
})
