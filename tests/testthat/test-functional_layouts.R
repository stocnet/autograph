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
  ladder          = c("twomode"),
  matching        = c("twomode"),
  lineage         = c("labelled"),
  multilevel      = c("twomode"),
  layered         = c("tree", "directed"),
  configuration   = c("basic", "labelled"),
  concentric      = c("labelled", "twomode"),
  valence         = c("basic", "directed", "signed"),
  dyad            = c("basic"),
  triad           = c("basic"),
  tetrad          = c("basic"),
  pentad          = c("basic"),
  hexad           = c("basic")
)

# Some layouts only accept a network of a particular size or shape, so the
# shared ag_fixtures grid cannot supply them. Previously these combinations all
# errored into an AUDIT skip -- and because skip() aborts the whole test_that
# block, the very first one silently prevented every later layout from being
# audited at all. Give them a network they can actually lay out.
layout_net_map <- list(
  # The configurational layouts place exactly n nodes at fixed positions
  dyad          = list(basic = manynet::create_ring(2)),
  triad         = list(basic = manynet::create_ring(3)),
  tetrad        = list(basic = manynet::create_ring(4)),
  pentad        = list(basic = manynet::create_ring(5)),
  hexad         = list(basic = manynet::create_ring(6)),
  configuration = list(basic    = manynet::create_ring(4),
                       labelled = manynet::to_named(manynet::create_ring(4))),
  # A ladder pairs the two modes off, so they must be equally sized
  ladder        = list(twomode = manynet::create_ring(6, 6)),
  # layered ranks nodes by path depth, so it needs an acyclic network;
  # ag_fixtures$directed is a random digraph and may contain cycles
  layered       = list(directed = manynet::create_tree(8, directed = TRUE))
)
# Extra arguments some layouts require, keyed layout -> fixture. Keying by
# fixture as well as layout matters because the right argument depends on the
# network: hierarchy's `center` only names a mode on a two-mode network, and
# concentric needs a membership for whichever fixture it is given.
layout_args_map <- list(
  lineage    = list(labelled = list(rank = "year")),
  hierarchy  = list(twomode = list(center = "events")),
  concentric = list(labelled = list(membership = rep(c("a", "b"), 4)),
                    twomode  = list(membership = "type")),
  multilevel = list(twomode = list(level = "type"))
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
      net <- if (lay == "lineage") lineage_fix
        else if (!is.null(layout_net_map[[lay]][[fix]]))
          layout_net_map[[lay]][[fix]]
        else ag_fixtures[[fix]]
      extra <- layout_args_map[[lay]][[fix]]
      p <- run_or_skip(
        do.call(graphr, c(list(net, layout = lay), extra)),
        paste0("layout ", lay), fix)
      run_or_skip(expect_buildable(p), paste0("build ", lay), fix)
    }
  }
})

test_that("every exported layout_* alias returns usable coordinates", {
  skip_on_cran()
  # The audit above enumerates the layout_tbl_graph_* functions, which the
  # user-facing layout_* aliases delegate to. The aliases were previously
  # covered by nothing at all -- they do not match that pattern -- so enumerate
  # them here too, and a new alias is picked up automatically. One fixture
  # each is enough, since the underlying algorithm is already exercised above.
  aliases <- grep("^layout_tbl_graph_", ag_alive_functions("^layout_"),
                  value = TRUE, invert = TRUE)
  expect_true(length(aliases) > 0)
  for (fn in aliases) {
    lay <- sub("^layout_", "", fn)
    fixtures <- layout_fixture_map[[lay]]
    fix <- if (is.null(fixtures)) "basic" else fixtures[[1]]
    net <- if (lay == "lineage")
      manynet::add_node_attribute(manynet::ison_adolescents, "year",
                                  rep(c(1985, 1990, 1995, 2000), times = 2))
      else if (!is.null(layout_net_map[[lay]][[fix]]))
        layout_net_map[[lay]][[fix]]
      else ag_fixtures[[fix]]
    extra <- layout_args_map[[lay]][[fix]]
    coords <- run_or_skip(
      do.call(get(fn, envir = asNamespace("autograph")), c(list(net), extra)),
      paste0("alias ", fn), fix)
    run_or_skip({
      coords <- as.data.frame(coords)
      testthat::expect_true(all(c("x", "y") %in% names(coords)))
      testthat::expect_equal(nrow(coords),
                             as.integer(manynet::net_nodes(net)))
      testthat::expect_true(all(is.finite(coords$x)) && all(is.finite(coords$y)))
    }, paste0("alias coords ", fn), fix)
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
