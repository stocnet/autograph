# The grid-snapping step behind `graphr(snap = TRUE)`, and the layouts it
# deliberately leaves alone. See R/graph_snap.R and `.fixed_layouts()`.

test_that("snapping a layout to the grid yields integer-ish unique positions", {
  skip_on_cran()
  p <- graphr(manynet::ison_adolescents, snap = TRUE)
  expect_buildable(p)
  # depth_first_recursive_search() assigns each node its own grid point
  expect_false(any(duplicated(p$data[, c("x", "y")])))
})

test_that("lattice networks snap onto a full rectangular grid", {
  skip_on_cran()
  # A lattice repeats two steps, which .snap_basis() maps onto the axes, so
  # every node lands on its own point of a rectangle of rows and columns.
  # create_lattice(12) is triangular (interior degree 6): its third family of
  # ties is drawn as diagonals of that same square grid.
  for (g in list(manynet::create_lattice(9),
                 manynet::create_lattice(12),
                 manynet::create_lattice(16),
                 manynet::create_lattice(12, width = 4),
                 manynet::create_lattice(20, width = 4))) {
    p <- suppressMessages(graphr(g, snap = TRUE))
    expect_buildable(p)
    d <- p$data[, c("x", "y")]
    expect_equal(d$x, round(d$x))
    expect_equal(d$y, round(d$y))
    expect_false(any(duplicated(d)))
    # no gap in any row or column: the grid is filled exactly
    expect_equal(length(unique(d$x)) * length(unique(d$y)), nrow(d))
  }
})

test_that("networks without a repeating structure take the fallback", {
  skip_on_cran()
  # A ring and a tree have about as many ties as nodes and no repeating steps,
  # so .snap_basis() declines them and depth_first_recursive_search() snaps
  # them instead.
  for (g in list(manynet::ison_adolescents, manynet::create_ring(12),
                 manynet::create_tree(15))) {
    lo <- ggraph::create_layout(manynet::as_tidygraph(g), "stress")
    expect_null(.snap_basis(lo, manynet::as_igraph(g)))
    p <- suppressMessages(graphr(g, snap = TRUE))
    expect_buildable(p)
    expect_false(any(duplicated(p$data[, c("x", "y")])))
  }
})

test_that("snapping a named network reads its ties by position", {
  skip_on_cran()
  # Regression: .edge_angle_deviation() read the tie ends with
  # igraph::as_edgelist(graph), which returns node names for a named network.
  # Indexing the coordinates by name gave NA, and the rotation score with it.
  p <- suppressMessages(graphr(manynet::fict_lotr, snap = TRUE))
  expect_buildable(p)
  expect_false(anyNA(p$data[, c("x", "y")]))
})

test_that("a cardinal rotation scores better than a diagonal one", {
  skip_on_cran()
  # Regression: the deviation was measured from 45 degrees, so which.min()
  # picked the angle at which the fewest ties ran cardinally.
  g <- manynet::as_igraph(manynet::create_lattice(12, width = 4))
  lo <- ggraph::create_layout(manynet::as_tidygraph(g), "stress")
  cardinal <- .snap_rotate(lo, g)
  expect_lt(.edge_angle_deviation(cardinal, g),
            .edge_angle_deviation(.rotate_layout(cardinal, pi/4), g))
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
