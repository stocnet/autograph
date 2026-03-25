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

test_that("ergm gof plot obs and sims factor levels are aligned", {
  p <- plot(ergm_gof)
  sims_levels <- levels(p$data$name)
  # levels should be pure numeric strings (no 'X' prefix from R's check.names)
  numeric_vals <- suppressWarnings(as.numeric(sims_levels))
  expect_true(!any(is.na(numeric_vals)),
              info = "Factor levels should be numeric, not R-modified names (e.g. 'X0')")
  # levels should be in sorted numeric order
  expect_equal(numeric_vals, sort(numeric_vals))
})
