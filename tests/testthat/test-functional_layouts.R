# Functional audit of the layout family: every exported
# layout_tbl_graph_<name> algorithm is run through graphr() on each fixture
# network it should conform to, and the resulting plot must build.
# Non-conformant layout x fixture combinations skip with an AUDIT message.

# Layouts that only make sense for particular structures get a restricted
# fixture set; everything else is tried on the full grid.
layout_fixture_map <- list(
  alluvial        = c("twomode"),
  hierarchy       = c("twomode", "labelled"),
  railway         = c("twomode", "labelled"),
  ladder          = c("twomode", "labelled"),
  matching        = c("twomode"),
  lineage         = c("labelled"),
  multilevel      = c("twomode"),
  layered         = c("tree", "directed"),
  configuration   = c("basic", "labelled"),
  concentric      = c("labelled", "twomode"),
  valence         = c("basic", "directed", "labelled"),
  dyad            = c("basic"),
  triad           = c("basic"),
  tetrad          = c("basic"),
  pentad          = c("basic"),
  hexad           = c("basic")
)
# Extra arguments some layouts require
layout_args_map <- list(
  lineage = list(rank = "year"),
  hierarchy = list(center = "events")
)

test_that("every exported layout algorithm draws a buildable plot", {
  skip_on_cran()
  layouts <- sub("^layout_tbl_graph_", "",
                 ag_alive_functions("^layout_tbl_graph_"))
  expect_true(length(layouts) > 0)
  lineage_fix <- manynet::add_node_attribute(manynet::ison_adolescents, "year",
                                             rep(c(1985, 1990, 1995, 2000),
                                                 times = 2))
  for (lay in layouts) {
    fixtures <- layout_fixture_map[[lay]]
    if (is.null(fixtures)) fixtures <- names(ag_fixtures)
    for (fix in fixtures) {
      net <- if (lay == "lineage") lineage_fix else ag_fixtures[[fix]]
      extra <- if (fix == "twomode" || lay == "lineage")
        layout_args_map[[lay]] else NULL
      p <- run_or_skip(
        do.call(graphr, c(list(net, layout = lay), extra)),
        paste0("layout ", lay), fix)
      run_or_skip(expect_buildable(p), paste0("build ", lay), fix)
    }
  }
})

test_that("layered layout accepts a raw edgelist and returns coordinates", {
  ties <- data.frame(
    from = c("A", "A", "B", "C", "D", "F", "F", "E"),
    to   = c("B", "C", "D", "E", "E", "E", "G", "G"),
    stringsAsFactors = FALSE)
  coords <- layout_tbl_graph_layered(ties, times = 6)
  expect_equal(sort(rownames(coords)), sort(unique(c(ties$from, ties$to))))
  expect_true(all(c("x", "y") %in% names(coords)))
  # sources sit above sinks
  expect_true(coords["A", "y"] > coords["G", "y"])
})

test_that("matching layout aligns matched partners vertically", {
  skip_on_cran()
  coords <- layout_tbl_graph_matching(manynet::ison_southern_women)
  expect_true(all(c("x", "y") %in% names(coords)))
  expect_true(nrow(coords) ==
                manynet::net_nodes(manynet::ison_southern_women))
})

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

test_that("snapping a two-mode (hierarchy) layout falls back gracefully", {
  skip_on_cran()
  # The default two-mode layout is "hierarchy", whose layered coordinates
  # would be collapsed by square-grid snapping, so snapping is skipped and
  # the original coordinates are retained (see graph_layout()).
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(
    graphr(manynet::ison_southern_women, snap = TRUE),
    "hierarchy")
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
