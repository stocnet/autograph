# Test that the gof plotting functions work correctly
test_that("gof plotting siena gof objects works", {
  p <- plot(siena_gof)
  expect_s3_class(p, "ggplot")  
})

test_that("gof plotting monan gof objects works", {
  p <- plot(monan_gof)
  expect_s3_class(p, "ggplot")  
})

test_that("gof plotting ergm gof objects works", {
  p <- plot(ergm_gof)
  expect_s3_class(p, "ggplot")  
})
