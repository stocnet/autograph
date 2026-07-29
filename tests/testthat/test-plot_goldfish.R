# The goldfish diagnostic classes. These objects arrive plot-ready: each is a
# tibble carrying the metadata contract, so every method here reads its series
# and its labels off the object rather than reshaping it.

test_that("outliers plotting works", {
  p <- plot(goldfish_outliers)
  expect_s3_class(p, "ggplot")
  # The series is the one the diagnostic analysed, and the flag is logical.
  expect_type(goldfish_outliers$outlier, "logical")
  expect_true(".series" %in% names(goldfish_outliers))
  expect_true(any(goldfish_outliers$outlier))
})

test_that("an object with nothing flagged says so instead of plotting", {
  quiet <- goldfish_outliers
  quiet$outlier <- FALSE
  expect_output(p <- plot(quiet), "No outliers found")
  expect_null(p)
})

test_that("changepoints plotting works", {
  p <- plot(goldfish_changepoints)
  expect_s3_class(p, "ggplot")
  expect_type(goldfish_changepoints$cpt, "logical")
  expect_true(any(goldfish_changepoints$cpt))
})

test_that("margin table plotting works", {
  p <- plot(goldfish_margins)
  expect_s3_class(p, "ggplot")
})

test_that("the methods read the metadata contract, not the columns", {
  for (object in list(
    goldfish_outliers,
    goldfish_changepoints,
    goldfish_margins
  )) {
    expect_s3_class(object, "tbl_df")
    expect_type(attr(object, "diagnostic"), "character")
    expect_type(attr(object, "context"), "list")
    expect_type(attr(object, "params"), "list")
    # The producing goldfish version, so a precooked fixture can be seen to
    # have aged.
    expect_type(attr(object, "version"), "character")
  }
  expect_identical(attr(goldfish_outliers, "diagnostic"), "diagnose_outliers")
  expect_identical(
    attr(goldfish_changepoints, "diagnostic"),
    "diagnose_changepoints"
  )
  expect_identical(attr(goldfish_margins, "diagnostic"), "margin_table")
})

test_that("the plots render without goldfish attached", {
  # Dispatch is on class alone, and nothing here calls back into goldfish.
  expect_false("package:goldfish" %in% search())
  expect_s3_class(plot(goldfish_outliers), "ggplot")
  expect_s3_class(plot(goldfish_changepoints), "ggplot")
  expect_s3_class(plot(goldfish_margins), "ggplot")
})

test_that("the margin plot caps the actors it draws, and says how many", {
  full <- plot(goldfish_margins, top = Inf)
  capped <- plot(goldfish_margins)
  expect_gt(nrow(full$data), nrow(capped$data))
  # Nothing is dropped silently.
  expect_match(capped$labels$subtitle, "further actors not shown")
  expect_null(full$labels$subtitle)
})
