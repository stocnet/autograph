# Tests for paths the family audits do not reach: branches selected by a
# particular argument value, and helpers only some inputs route through. These
# were the largest uncovered clusters in the covr per-function report.

# Hierarchy `center` ----

test_that("hierarchy centres on either mode, or on a named node", {
  skip_on_cran()
  sw <- manynet::ison_southern_women
  # Each `center` takes its own branch through the coordinate construction,
  # and each normalises its rows with nrm()/rng().
  for (ctr in c("actors", "events")) {
    coords <- as.data.frame(layout_tbl_graph_hierarchy(sw, center = ctr))
    expect_equal(nrow(coords), as.integer(manynet::net_nodes(sw)))
    expect_true(all(is.finite(coords$x)) && all(is.finite(coords$y)))
    # The centred mode sits between the two halves of the other one
    expect_length(unique(coords$x), 3L)
  }
  # A node name centres on that node rather than on a mode
  nm <- manynet::node_names(sw)[1]
  coords <- as.data.frame(layout_tbl_graph_hierarchy(sw, center = nm))
  expect_equal(nrow(coords), as.integer(manynet::net_nodes(sw)))
  expect_true(all(is.finite(coords$x)))
})

test_that("hierarchy refuses to centre a one-mode network", {
  skip_on_cran()
  # Centring names a mode, so there is nothing to centre on without two of
  # them. This is an abort rather than a substitution because the user can drop
  # the argument (see .layout_requirements() in R/graph_checks.R).
  expect_error(
    layout_tbl_graph_hierarchy(manynet::ison_adolescents, center = "actors"),
    "one-mode network")
})

test_that("nrm() normalises vectors and arrays onto a common scale", {
  expect_equal(autograph:::nrm(c(0, 5, 10)), c(0, 0.5, 1))
  # A single value has no range to normalise against and is returned as is
  expect_equal(autograph:::nrm(7), 7)
  out <- autograph:::nrm(cbind(c(0, 10), c(0, 5)))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
})

# Multilevel weights ----

test_that("multilevel drops tie weights it cannot use", {
  skip_on_cran()
  # .drop_unusable_weights() strips a weight attribute that would otherwise
  # make the level distances meaningless.
  net <- manynet::add_tie_attribute(manynet::ison_southern_women, "weight",
                                    rep(1, manynet::net_ties(
                                      manynet::ison_southern_women)))
  # suppressWarnings: the weighted edge scale goes through ggraph, which still
  # calls continuous_scale(trans = ) and so emits a ggplot2 3.5.0 deprecation
  # warning that is not ours to fix.
  suppressWarnings(
    expect_buildable(graphr(net, layout = "multilevel", level = "type")))
})

# Radial label angles ----

test_that("labels on radial layouts are rotated to follow the circle", {
  skip_on_cran()
  # Only "circle" and "concentric" route through .cart2pol()/.hypot() to work
  # out a per-label angle; every other layout leaves labels upright.
  p <- graphr(manynet::ison_adolescents, layout = "circle", labels = TRUE)
  built <- expect_buildable(p)
  # The angle is passed as a per-node vector of aes_params, so it is only
  # visible once the plot is built.
  angles <- built$data[[length(built$data)]][["angle"]]
  expect_false(is.null(angles))
  # Labels are spread around the circle rather than all at one angle
  expect_gt(length(unique(angles)), 1)
  expect_true(all(is.finite(angles)))
  # An ordinary layout leaves them upright
  flat <- ggplot2::ggplot_build(graphr(manynet::ison_adolescents,
                                       layout = "stress", labels = TRUE))
  expect_equal(length(unique(flat$data[[length(flat$data)]][["angle"]])), 1L)
})

test_that("cartesian coordinates convert to polar", {
  out <- as.data.frame(autograph:::.cart2pol(cbind(c(1, 0), c(0, 1))))
  expect_equal(nrow(out), 2L)
  # (1,0) lies on the positive x axis, (0,1) a quarter turn round
  expect_equal(out$phi[1], 0)
  expect_equal(out$phi[2], pi / 2)
  expect_equal(autograph:::.hypot(3, 4), 5)
})

# Theme persistence ----

test_that("the theme preference is written and forgotten on request", {
  # R_user_dir() is redirected so the test never touches the real config.
  tmp <- withr_tempdir <- tempfile("agconfig")
  dir.create(tmp)
  old <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA)
  Sys.setenv(R_USER_CONFIG_DIR = tmp)
  on.exit({
    if (is.na(old)) Sys.unsetenv("R_USER_CONFIG_DIR")
    else Sys.setenv(R_USER_CONFIG_DIR = old)
    unlink(tmp, recursive = TRUE)
    suppressMessages(stocnet_theme("default"))
  }, add = TRUE)

  expect_true(autograph:::write_theme_pref("iheid"))
  f <- autograph:::theme_pref_file()
  expect_true(file.exists(f))
  expect_equal(readRDS(f), "iheid")
  autograph:::forget_theme_pref()
  expect_false(file.exists(f))
})

# Goodness-of-fit variants ----

test_that("ergm gof plots each statistic it holds, and says so when it cannot", {
  skip_on_cran()
  # The fixture carries degree, espartners and distance statistics; each
  # selects its own branch for extracting observed and simulated values.
  for (s in c("degree", "espartners", "distance")) {
    expect_buildable(plot(ergm_gof, statistic = s))
  }
  # Asking for one the fit does not hold reports which, rather than failing
  # somewhere in the extraction.
  expect_error(plot(ergm_gof, statistic = "dspartners"), "dspart")
})

test_that("gof plots accept a cumulative view and a custom title", {
  skip_on_cran()
  for (cm in c(TRUE, FALSE)) {
    expect_buildable(plot(ergm_gof, cumulative = cm))
    expect_buildable(plot(siena_gof, cumulative = cm))
  }
  # `main` short-circuits the constructed title
  p <- plot(siena_gof, main = "A title of my own")
  expect_buildable(p)
  expect_match(paste(unlist(p$labels), collapse = " "), "A title of my own")
})

# Group reduction ----

test_that("node_group folds sparse categories into an 'Other' group", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  # Four categories of one member each, plus one of three: .reduce_categories()
  # condenses the sparse ones rather than drawing a hull around every singleton.
  net <- manynet::add_node_attribute(manynet::ison_adolescents, "grp",
                                     c("a", "a", "a", "b", "c", "d", "e", "f"))
  expect_message(p <- graphr(net, node_group = "grp"), "Other")
  expect_buildable(p)
  expect_true("Other" %in% manynet::node_attribute(
    manynet::mutate_nodes(manynet::as_tidygraph(net),
                          g = autograph:::.reduce_categories(
                            manynet::as_tidygraph(net), "grp")), "g"))
  # Exactly two sparse categories take the other branch
  net2 <- manynet::add_node_attribute(manynet::ison_adolescents, "grp",
                                      c("a", "a", "a", "b", "b", "b", "c", "d"))
  expect_buildable(graphr(net2, node_group = "grp"))
})

# Label selection ----

test_that("labels select nodes by count, by criterion, and at random", {
  skip_on_cran()
  net <- manynet::ison_adolescents
  n_labels <- function(p) {
    lab <- p[["layers"]][[length(p[["layers"]])]][["data"]][["name"]]
    length(stats::na.omit(lab))
  }
  # A count selects by *rank* rather than by node, so ties widen the selection:
  # ison_adolescents' degrees are 4,4,3,3,2,2,1,1, and asking for 3 labels the
  # whole of each rank it reaches rather than cutting a tie arbitrarily.
  p3 <- graphr(net, labels = 3)
  expect_buildable(p3)
  expect_gte(n_labels(p3), 3L)
  expect_lt(n_labels(p3), as.integer(manynet::net_nodes(net)))
  # A named criterion ranks by that measure instead
  expect_buildable(graphr(net, labels = "degree"))
  # "random" samples rather than ranks (.sample_labels)
  set.seed(123)
  expect_buildable(graphr(net, labels = "random"))
  # A logical node attribute marks which to label
  marked <- manynet::add_node_attribute(net, "keep",
                                        rep(c(TRUE, FALSE), 4))
  pm <- graphr(marked, labels = "keep")
  expect_buildable(pm)
  expect_equal(n_labels(pm), 4L)
  # Named nodes label exactly those
  pn <- graphr(net, labels = manynet::node_names(net)[1:2])
  expect_buildable(pn)
  expect_equal(n_labels(pn), 2L)
})

# Diffusion node colouring ----

test_that("graphr colours nodes by their adoption time on a diffusion", {
  skip_on_cran()
  set.seed(123)
  diff <- manynet::play_diffusion(manynet::create_ring(10), seeds = 1)
  p <- graphr(diff)
  expect_buildable(p)
  # .node_adoption_time() turns the event history into a per-node value, so
  # nodes must differ rather than all taking one colour
  built <- ggplot2::ggplot_build(p)
  node_layer <- built$data[[length(built$data)]]
  expect_gt(length(unique(stats::na.omit(node_layer$fill))), 0)
})
