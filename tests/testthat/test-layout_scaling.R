# The scaling layout and the fit it reports. See R/layout_scaling.R.

test_that("scaling layout graphs correctly", {
  skip_on_cran()
  p <- graphr(manynet::ison_southern_women, layout = "scaling")
  expect_equal(p$plot_env$layout, "scaling")
  expect_buildable(p)
  expect_equal(nrow(p$data),
               as.integer(manynet::net_nodes(manynet::ison_southern_women)))
})

test_that("scaling layout places every node of an awkward network", {
  skip_on_cran()
  # A disconnected network, which "pmds" refuses outright, and a signed one,
  # whose weights make the shortest paths uncomputable.
  for (net in list(manynet::ison_adolescents, manynet::ison_southern_women,
                   manynet::fict_thrones, manynet::fict_marvel)) {
    lo <- layout_scaling(net)
    expect_named(lo, c("x", "y"))
    expect_equal(nrow(lo), as.integer(manynet::net_nodes(net)))
    expect_true(all(is.finite(as.matrix(lo))))
  }
})

test_that("scaling layout scales in full where it can and by pivots otherwise", {
  skip_on_cran()
  full <- attr(layout_scaling(manynet::ison_southern_women), "fit")
  expect_true(is.na(full$pivots))
  expect_false(is.na(full$variance))
  pivoted <- attr(layout_scaling(manynet::ison_southern_women, pivots = 5),
                  "fit")
  expect_equal(pivoted$pivots, 5L)
  # The share of variance comes from the decomposition the pivots avoid.
  expect_true(is.na(pivoted$variance))
  expect_error(layout_scaling(manynet::ison_adolescents, pivots = 1),
               "at least 2")
})

test_that("scaling layout draws axes and reports its fit", {
  skip_on_cran()
  p <- graphr(manynet::ison_southern_women, layout = "scaling")
  expect_equal(p$labels$x, "Dimension 1")
  expect_equal(p$labels$y, "Dimension 2")
  expect_match(p$labels$caption, "Stress: [0-9]+%")
  expect_match(p$labels$caption, "distance variance")
  # One scale for both axes, or the distances drawn cannot be compared.
  expect_equal(p$coordinates$ratio, 1)
  # Axes a void theme would have blanked.
  expect_false(inherits(p$theme$axis.text, "element_blank"))
})

test_that("a poor fit is reported at the console", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(graphr(manynet::ison_networkers, layout = "scaling"),
                 "read the clusters")
  expect_no_message(graphr(manynet::ison_adolescents, layout = "scaling"))
})

test_that("scaling coordinates are kept when snapping is asked for", {
  skip_on_cran()
  snapped <- suppressMessages(
    graphr(manynet::ison_southern_women, layout = "scaling", snap = TRUE))
  plain <- graphr(manynet::ison_southern_women, layout = "scaling")
  expect_equal(snapped$data[, c("x", "y")], plain$data[, c("x", "y")])
})

test_that("the fit is captioned alongside the isolates", {
  skip_on_cran()
  iso <- manynet::add_nodes(manynet::ison_adolescents, 1,
                            list(name = "Zoe"))
  p <- suppressMessages(
    graphr(iso, layout = "scaling", isolates = "caption"))
  expect_match(p$labels$caption, "Isolates: Zoe")
  expect_match(p$labels$caption, "Stress")
})

test_that("check_stress() scores a layout that draws distances better", {
  skip_on_cran()
  scaled <- check_stress(graphr(manynet::ison_southern_women,
                                layout = "scaling"))
  circle <- check_stress(graphr(manynet::ison_southern_women,
                                layout = "circle"))
  expect_length(scaled, 1)
  expect_true(is.finite(scaled) && scaled >= 0)
  expect_lt(scaled, circle)
  expect_equal(attr(scaled, "pairs"), 32 * 31)
  # The layout reports the same score it is measured by.
  expect_equal(unname(scaled),
               attr(layout_scaling(manynet::ison_southern_women),
                    "fit")$stress, tolerance = 1e-6)
  expect_error(check_stress(ggplot2::ggplot()), "coordinates")
})
