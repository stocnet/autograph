test_that("a spatial network is drawn at its nodes' positions", {
  net <- manynet::create_ring(4) |>
    manynet::mutate_nodes(x = c(0, 10, 10, 0), y = c(0, 0, 5, 5))
  expect_equal(.infer_layout(net, NULL), "spatial")
  p <- graphr(net)
  expect_equal(p$data$x, c(0, 10, 10, 0))
  expect_equal(p$data$y, c(0, 0, 5, 5))
  # ggplot2 4 holds a fixed ratio on a cartesian coordinate system
  expect_equal(p$coordinates$ratio, 1)
})

test_that("longitudes are scaled by the cosine of the mean latitude", {
  net <- manynet::create_ring(3) |>
    manynet::mutate_nodes(lon = c(0, 1, 2), lat = c(60, 60, 60))
  lo <- layout_spatial(net)
  expect_equal(lo$x, c(0, 1, 2) * cos(60 * pi / 180))
  expect_equal(lo$y, c(60, 60, 60))
})

test_that("the spatial layout is fixed and needs positions", {
  expect_true(.is_fixed_layout("spatial"))
  expect_false(.layout_applies(manynet::create_ring(4), "spatial"))
  expect_error(layout_spatial(manynet::create_ring(4)), "positions")
})

test_that("tie lengths are passed to the layouts that read them", {
  net <- manynet::create_ring(4) |>
    manynet::mutate_ties(length = c(1, 2, 1, 2))
  expect_equal(.length_layout_weights(net, "kk"), c(1, 2, 1, 2))
  expect_equal(.length_layout_weights(net, "stress"), 1 / c(1, 2, 1, 2))
  expect_null(.length_layout_weights(net, "fr"))
  expect_null(.length_layout_weights(manynet::create_ring(4), "kk"))
})

test_that("a backbone of a network with only lengths reads them", {
  net <- manynet::create_ring(4) |>
    manynet::mutate_ties(length = c(1, 2, 1, 2))
  expect_equal(.backbone_filter(net, NULL), "lans")
  expect_equal(.backbone_filter(manynet::create_ring(4), NULL), "simmelian")
})
