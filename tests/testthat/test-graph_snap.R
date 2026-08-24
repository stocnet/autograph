# The grid-snapping step behind `graphr(snap = TRUE)`, and the layouts it
# deliberately leaves alone. See R/graph_snap.R and `.layered_layouts()`.

test_that("snapping a layout to the grid yields integer-ish unique positions", {
  skip_on_cran()
  p <- graphr(manynet::ison_adolescents, snap = TRUE)
  expect_buildable(p)
  # depth_first_recursive_search() assigns each node its own grid point
  expect_false(any(duplicated(p$data[, c("x", "y")])))
})

test_that("lattice networks snap by rotation to align edges to the grid", {
  skip_on_cran()
  p <- suppressMessages(graphr(manynet::create_lattice(9), snap = TRUE))
  expect_buildable(p)
  expect_true(all(p$data$x == round(p$data$x)))
})

test_that("snapping a two-mode (layered) layout falls back gracefully", {
  skip_on_cran()
  # The default two-mode layout is "layered", whose layered coordinates
  # would be collapsed by square-grid snapping, so snapping is skipped and
  # the original coordinates are retained (see graph_layout()).
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(
    graphr(manynet::ison_southern_women, snap = TRUE),
    "layered")
  snapped <- suppressMessages(graphr(manynet::ison_southern_women, snap = TRUE))
  plain   <- graphr(manynet::ison_southern_women)
  expect_buildable(snapped)
  expect_equal(snapped$data[, c("x", "y")], plain$data[, c("x", "y")])
})

test_that("snapping still works on a two-mode network with a force layout", {
  skip_on_cran()
  p <- suppressMessages(
    graphr(manynet::ison_southern_women, layout = "stress", snap = TRUE))
  expect_buildable(p)
  # every node lands on its own grid point
  expect_false(any(duplicated(p$data[, c("x", "y")])))
})

test_that("snapping returns coordinates in the original node order (>= 10 nodes)", {
  skip_on_cran()
  # Regression: depth_first_recursive_search() sorts nodes by centroid distance
  # internally, then must restore the input node order before returning, because
  # graph_layout() assigns the result positionally. Ordering the row names
  # lexicographically ("1","10","11",...,"2") scrambled coordinates across nodes
  # for any network with 10+ nodes; they must be ordered numerically.
  lo <- ggraph::create_layout(manynet::as_tidygraph(manynet::fict_lotr),
                              "stress")
  expect_true(nrow(lo) >= 10)
  out <- depth_first_recursive_search(lo)
  # returned rows line up with the input nodes, not a lexicographic shuffle
  expect_identical(rownames(out), as.character(seq_len(nrow(out))))
  # snapped positions track the pre-snap layout rather than being permuted
  expect_gt(stats::cor(lo$x, out$x), 0.5)
  expect_gt(stats::cor(lo$y, out$y), 0.5)
})
