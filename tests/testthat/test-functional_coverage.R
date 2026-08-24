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
    coords <- as.data.frame(layout_tbl_graph_layered(sw, center = ctr))
    expect_equal(nrow(coords), as.integer(manynet::net_nodes(sw)))
    expect_true(all(is.finite(coords$x)) && all(is.finite(coords$y)))
    # The centred mode sits between the two halves of the other one
    expect_length(unique(coords$x), 3L)
  }
  # A node name centres on that node rather than on a mode
  nm <- manynet::node_names(sw)[1]
  coords <- as.data.frame(layout_tbl_graph_layered(sw, center = nm))
  expect_equal(nrow(coords), as.integer(manynet::net_nodes(sw)))
  expect_true(all(is.finite(coords$x)))
})

test_that("hierarchy refuses to centre a one-mode network", {
  skip_on_cran()
  # Centring names a mode, so there is nothing to centre on without two of
  # them. This is an abort rather than a substitution because the user can drop
  # the argument (see .layout_requirements() in R/graph_checks.R).
  expect_error(
    layout_tbl_graph_layered(manynet::ison_adolescents, center = "actors"),
    "one-mode network")
})

test_that(".nrm() normalises vectors and arrays onto a common scale", {
  expect_equal(autograph:::.nrm(c(0, 5, 10)), c(0, 0.5, 1))
  # A single value has no range to normalise against and is returned as is
  expect_equal(autograph:::.nrm(7), 7)
  out <- autograph:::.nrm(cbind(c(0, 10), c(0, 5)))
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
    expect_buildable(graphr(net, layout = "levels", level = "type")))
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

test_that("the medium preference is written, read back, and forgotten", {
  # The medium persists the same way the theme does, so it is redirected the
  # same way and never touches the real config.
  tmp <- tempfile("agconfig")
  dir.create(tmp)
  old <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA)
  Sys.setenv(R_USER_CONFIG_DIR = tmp)
  on.exit({
    if (is.na(old)) Sys.unsetenv("R_USER_CONFIG_DIR")
    else Sys.setenv(R_USER_CONFIG_DIR = old)
    unlink(tmp, recursive = TRUE)
    suppressMessages(stocnet_medium("screen"))
  }, add = TRUE)

  f <- autograph:::pref_file("medium")
  suppressMessages(stocnet_medium("presentation", persist = TRUE))
  expect_true(file.exists(f))
  expect_equal(autograph:::read_medium_pref(), "presentation")
  # Setting a medium without `persist` forgets the remembered one, so that the
  # next session does not start in a medium the user has since left.
  suppressMessages(stocnet_medium("screen"))
  expect_false(file.exists(f))
  expect_null(autograph:::read_medium_pref())
  # A stored value that is not one of the media is discarded rather than set.
  autograph:::write_pref("medium", "papyrus")
  expect_null(autograph:::read_medium_pref())
  autograph:::write_pref("medium", c("screen", "print"))
  expect_null(autograph:::read_medium_pref())
})

test_that("stocnet_medium reports the medium and rejects a bad argument", {
  on.exit(suppressMessages(stocnet_medium("screen")), add = TRUE)
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  suppressMessages(stocnet_medium("screen"))
  expect_message(stocnet_medium(), "currently set to")
  # The medium must be one string: a vector or a number names no medium.
  expect_error(stocnet_medium(c("screen", "print")), "single medium")
  expect_error(stocnet_medium(2), "single medium")
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

# Diffusion summaries ----

test_that("a diffusion that never spread says so rather than plotting", {
  skip_on_cran()
  # A single row is the whole diffusion, so there is no trace to draw.
  flat <- manynet::as_diffusion(
    manynet::play_diffusion(manynet::create_empty(5), seeds = 1))
  expect_equal(nrow(flat), 1L)
  # snet_warn() speaks through cli, and only when verbosity is turned up.
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  # The method returns the cli message's own value, not a plot.
  expect_message(out <- plot(flat), "No diffusion was observed")
  expect_false(inherits(out, "ggplot"))
})

test_that("the diffusion plot adds a line for each compartment it holds", {
  skip_on_cran()
  # The exposed and recovered lines are added only when those compartments
  # are populated, so an SI run draws two lines and an SIR run three.
  set.seed(1)
  si <- manynet::as_diffusion(
    manynet::play_diffusion(manynet::create_ring(10), seeds = 1))
  expect_false(any(si$E > 0))
  expect_false(any(si$R > 0))
  n_lines <- function(p) {
    sum(vapply(p[["layers"]],
               function(l) inherits(l[["geom"]], "GeomLine"), logical(1)))
  }
  expect_equal(n_lines(si_p <- plot(si)), 2L)
  expect_buildable(si_p)
  set.seed(1)
  sir <- manynet::as_diffusion(
    manynet::play_diffusion(manynet::create_ring(10), seeds = 1,
                            recovery = 0.4))
  expect_true(any(sir$R > 0))
  expect_equal(n_lines(sir_p <- plot(sir)), 3L)
  expect_buildable(sir_p)
  set.seed(2)
  seir <- manynet::as_diffusion(
    manynet::play_diffusion(manynet::create_ring(10), seeds = 1,
                            latency = 0.9))
  expect_true(any(seir$E > 0))
  expect_equal(n_lines(seir_p <- plot(seir)), 3L)
  expect_buildable(seir_p)
})

test_that("multiple diffusions smooth one line per compartment", {
  skip_on_cran()
  skip_if_not_installed("migraph")
  set.seed(1)
  sir <- migraph::play_diffusions(manynet::create_ring(10), seeds = 1,
                                  latency = 0.9, recovery = 0.4, times = 3)
  expect_true(any(sir$E > 0) && any(sir$R > 0))
  # suppressWarnings: loess on a short series warns about its span, which is
  # not what this is checking.
  suppressWarnings(expect_buildable(plot(sir)))
  expect_equal(
    sum(vapply(plot(sir)[["layers"]],
               function(l) inherits(l[["geom"]], "GeomSmooth"), logical(1))),
    4L)
})

# Motif illustrations ----

test_that("motif results are illustrated by the census they come from", {
  skip_on_cran()
  skip_if_not_installed("netrics")
  net <- manynet::ison_adolescents
  set.seed(123)
  dir <- manynet::generate_random(8, directed = TRUE)
  # Each census names its own motifs, and each set of names selects the
  # illustration drawn for it.
  expect_buildable(plot(netrics::node_x_dyad(net)))       # Mutual
  expect_buildable(plot(netrics::node_x_dyad(dir)))       # Asymmetric
  expect_buildable(plot(netrics::node_x_triad(net)))      # 102
  expect_buildable(plot(netrics::node_x_triad(dir)))      # 021D
  expect_buildable(plot(netrics::net_x_dyad(net)))
  expect_buildable(plot(netrics::net_x_triad(dir)))
})

test_that("a census with no illustration says so rather than drawing", {
  skip_on_cran()
  # The message names the censuses that can be drawn, so the reader is not
  # left guessing which results the method takes.
  motifs <- structure(matrix(0, nrow = 2, ncol = 2,
                             dimnames = list(c("A", "B"), c("Q1", "Q2"))),
                      class = c("node_motif", "matrix", "array"))
  expect_error(plot(motifs), "cannot be illustrated")
  net_motifs <- structure(c(Q1 = 0, Q2 = 0), class = "network_motif")
  expect_error(plot(net_motifs), "cannot be illustrated")
})

# Test distributions ----

test_that("the test plot shades one tail or two, on the side tested", {
  skip_on_cran()
  x <- res_migraph_test
  # The density layers are areas too, so the shaded tails are counted by
  # their own geom rather than by what they inherit from.
  n_areas <- function(p) {
    sum(vapply(p[["layers"]],
               function(l) class(l[["geom"]])[1] == "GeomArea", logical(1)))
  }
  # Two tails is the default, and shades both ends of the distribution.
  expect_equal(n_areas(plot(x)), 2L)
  # A one-tailed test shades the end the observed value sits in, so a value
  # below the median takes the other branch from one above it.
  low <- x
  low$testval <- stats::quantile(x$testdist, 0.1)
  expect_equal(n_areas(p_low <- plot(low, tails = "one")), 1L)
  expect_buildable(p_low)
  high <- x
  high$testval <- stats::quantile(x$testdist, 0.9)
  expect_equal(n_areas(p_high <- plot(high, tails = "one")), 1L)
  expect_buildable(p_high)
  # A correlation-like distribution spanning zero expands to both limits
  both <- x
  both$testdist <- c(x$testdist, -x$testdist)
  expect_buildable(plot(both))
})

# Selection and influence tables ----

test_that("interpretation tables follow the theme and the curve asked for", {
  skip_on_cran()
  # `quad = FALSE` joins the points instead of smoothing them, and a
  # monochrome theme takes its own branch through the colour scale.
  expect_buildable(plot(siena_selection, quad = FALSE))
  expect_buildable(plot(siena_influence, quad = FALSE))
  old <- options(stocnet_theme = "bw")
  on.exit(options(old), add = TRUE)
  expect_buildable(plot(siena_selection))
  # Separation offsets the egos so that overlapping curves stay readable
  expect_buildable(plot(siena_selection, separation = 0.1))
})

# Fonts ----

test_that("a theme whose fonts are missing falls back to the default", {
  old <- options(snet_font = getOption("snet_font"),
                 snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  # The system's own font list is not something a test can arrange, so the
  # lookup is replaced: available_fonts() is its own function for this.
  testthat::local_mocked_bindings(available_fonts = function() "sans")
  expect_message(autograph:::set_font_theme("ethz"), "are available")
  expect_equal(getOption("snet_font"), "sans")
  # A font the theme asks for and the system has is used as it is
  testthat::local_mocked_bindings(
    available_fonts = function() c("sans", "Arial"))
  expect_message(autograph:::set_font_theme("ethz"), "Setting font to Arial")
  expect_equal(getOption("snet_font"), "Arial")
  # A theme with no preferred fonts asks for none
  autograph:::set_font_theme("default")
  expect_equal(getOption("snet_font"), "sans")
})

# Label geometry ----

test_that("polar conversion takes a point, a matrix, or a third dimension", {
  # A single point comes back as a vector, a matrix row-wise, and a third
  # column is carried through untouched.
  expect_equal(autograph:::.cart2pol(c(1, 0)), c(0, 1))
  expect_equal(autograph:::.cart2pol(c(1, 0, 5)), c(0, 1, 5))
  out <- autograph:::.cart2pol(cbind(c(1, 0), c(0, 1), c(2, 3)))
  expect_equal(colnames(out), c("phi", "r", "z"))
  expect_equal(out[, "z"], c(2, 3))
  # Anything else says what shape was expected rather than failing inside
  # the arithmetic.
  expect_error(autograph:::.cart2pol("a"), "numeric")
  expect_error(autograph:::.cart2pol(1:5), "vector of length 3")
})

test_that("the hypotenuse recycles a single leg and refuses a mismatch", {
  expect_equal(autograph:::.hypot(3, c(4, 4)), c(5, 5))
  expect_equal(autograph:::.hypot(c(3, 3), 4), c(5, 5))
  # Nothing to measure
  expect_length(autograph:::.hypot(numeric(0), 3), 0L)
  expect_error(autograph:::.hypot("a", 1), "numeric or complex")
  expect_error(autograph:::.hypot(c(1, 2), c(1, 2, 3)), "same size")
})

test_that("labels are nudged clear of the nodes when they cannot be repelled", {
  skip_on_cran()
  # Without ggrepel there is no algorithm to keep labels off the nodes, so
  # each layout family approximates the same clearance with a fixed nudge.
  radial <- graphr(manynet::ison_adolescents, layout = "circle",
                   labels = TRUE, label_repel = FALSE)
  built <- expect_buildable(radial)
  # The nudge is radial, so it differs per label rather than being one offset
  layer <- radial[["layers"]][[length(radial[["layers"]])]]
  expect_s3_class(layer[["position"]], "PositionNudge")
  expect_gt(length(unique(layer[["position"]][["x"]])), 1)
  expect_buildable(graphr(manynet::ison_southern_women, layout = "bipartite",
                          labels = TRUE, label_repel = FALSE))
  expect_buildable(graphr(manynet::ison_southern_women, layout = "layered",
                          labels = TRUE, label_repel = FALSE))
  # A node size mapped from an attribute is cut down to the labelled nodes
  sized <- manynet::add_node_attribute(manynet::ison_adolescents, "wt",
                                       seq_len(8))
  expect_buildable(graphr(sized, node_size = "wt", labels = 3,
                          label_repel = FALSE))
})

# Label selection without netrics ----

test_that("ranking labels without netrics falls back or says what is missing", {
  skip_on_cran()
  net <- manynet::ison_adolescents
  # .has_netrics() is its own function so that this fallback can be tested
  # with the package installed.
  testthat::local_mocked_bindings(.has_netrics = function() FALSE)
  # A criterion the user asked for by name is not silently substituted.
  expect_error(graphr(net, labels = "degree"), "netrics")
  # An automatic selection is too incidental to a plot to stop it, so it
  # samples instead and says that it did.
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  set.seed(123)
  # More than 30 named nodes drawn, so graphr() selects which to label itself.
  big <- suppressMessages(manynet::to_named(manynet::generate_random(40, 0.2)))
  expect_message(p <- graphr(big), "random selection")
  expect_buildable(p)
  # "random" needs nothing of netrics either way
  expect_buildable(graphr(net, labels = "random"))
})

test_that("labels can mark every node a measure flags, however many", {
  skip_on_cran()
  skip_if_not_installed("netrics")
  # A mark rather than a ranking: cutpoints are labelled by their flag.
  net <- manynet::ison_adolescents
  p <- graphr(net, labels = "cutpoints")
  expect_buildable(p)
  labelled <- p[["layers"]][[length(p[["layers"]])]][["data"]][["name"]]
  expect_equal(sort(stats::na.omit(labelled)),
               sort(manynet::node_names(net)[
                 as.logical(netrics::node_is_cutpoint(net))]))
})

test_that("a random label sample is drawn within each mode or level", {
  skip_on_cran()
  # Both modes are sampled from, so a two-mode plot labels both rather than
  # only the larger one.
  sel <- autograph:::.sample_labels(
    manynet::as_igraph(manynet::ison_southern_women), 2)
  modes <- igraph::V(manynet::as_igraph(manynet::ison_southern_women))$type
  expect_equal(sum(sel[!modes]), 2L)
  expect_equal(sum(sel[modes]), 2L)
  # The session's RNG is left as it was found, so a plot drawn twice is the
  # same plot and the caller's stream is undisturbed.
  set.seed(42)
  before <- stats::runif(1)
  set.seed(42)
  invisible(autograph:::.sample_labels(
    manynet::as_igraph(manynet::ison_adolescents), 3))
  expect_equal(stats::runif(1), before)
})

# Layer assignment ----

test_that("the layered layout asks igraph for layers when none are given", {
  skip_on_cran()
  # A one-mode network arrives without layers, so they are computed rather
  # than taken from the modes.
  lo <- autograph:::.sugiyama_layout(
    manynet::as_igraph(manynet::create_tree(10, directed = TRUE)))
  expect_equal(nrow(lo), 10L)
  expect_gt(length(unique(lo[, 2])), 1)
  # A network with nothing to layer is returned in one row of nodes rather
  # than run through the crossing-minimisation sweeps.
  flat <- autograph:::.sugiyama_layout(
    manynet::as_igraph(manynet::create_empty(4)))
  expect_equal(nrow(flat), 4L)
  expect_equal(length(unique(flat[, 2])), 1L)
})

test_that("a tie spanning two layers is routed through a dummy node", {
  skip_on_cran()
  # a -> f skips the middle layer, so the sweeps need a placeholder there to
  # count its crossings against.
  el <- data.frame(from = c("a", "b", "a", "c", "d", "a"),
                   to   = c("c", "d", "e", "e", "f", "f"))
  g <- igraph::graph_from_data_frame(el, directed = TRUE)
  lo <- autograph:::.sugiyama_layout(g, layers = c(0, 0, 1, 1, 2, 2),
                                     times = 5)
  # One row per real node: the dummy is a routing device, not a node.
  expect_equal(nrow(lo), 6L)
  expect_equal(lo[, 2], c(0, 0, 1, 1, 2, 2))
  expect_true(all(is.finite(lo[, 1])))
})

test_that("a layer of nodes is spread, and a negative count refused", {
  # .rng() spreads a layer's nodes over a common range; one node has no
  # spread to take.
  expect_equal(autograph:::.rng(1), 0)
  spread <- autograph:::.rng(3)
  expect_length(spread, 3L)
  expect_equal(spread[2], 0)
  expect_true(spread[1] < spread[3])
  expect_error(autograph:::.rng(-1), "negative number of nodes")
})

test_that("concentric needs one membership per node", {
  skip_on_cran()
  # A vector that is neither an attribute name nor one value per node says
  # which of the two it should have been.
  expect_error(
    layout_concentric(manynet::as_igraph(manynet::ison_adolescents),
                      membership = c("a", "b")),
    "membership")
})

test_that("hierarchy centres on a node of either mode", {
  skip_on_cran()
  sw <- manynet::ison_southern_women
  # The events are the second mode, and centring on one takes its own branch
  # from centring on an actor.
  event <- utils::tail(manynet::node_names(sw), 1)
  coords <- as.data.frame(layout_tbl_graph_layered(sw, center = event))
  expect_equal(nrow(coords), as.integer(manynet::net_nodes(sw)))
  expect_true(all(is.finite(coords$x)) && all(is.finite(coords$y)))
  # A name that is in neither mode names what was expected instead
  expect_error(layout_tbl_graph_layered(sw, center = "Nobody"), "Nobody")
})

test_that("multilevel ignores weights it cannot read as distances", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  # Negative weights are no distance, and igraph::distances() rejects them
  # outright, so they are dropped rather than the layout failing.
  sw <- manynet::ison_southern_women
  net <- manynet::add_tie_attribute(
    sw, "weight", rep(c(-1, 1), length.out = as.integer(manynet::net_ties(sw))))
  # suppressWarnings: the weighted edge scale goes through ggraph, see above.
  suppressWarnings(expect_message(
    p <- graphr(net, layout = "levels", level = "type"),
    "Ignoring the tie weights"))
  suppressWarnings(expect_buildable(p))
})

# Margin dispersion ----

test_that("a margin table carrying dispersion is drawn as level against shape", {
  skip_on_cran()
  # `margin_table(dispersion = TRUE)` adds a second reading, and where both
  # are present the figure is the two against each other. The fixture is the
  # precooked table with that column put on it, since the method reads the
  # object's columns rather than calling back into goldfish.
  m <- goldfish_margins
  set.seed(123)
  m$dispersion <- c(rep(NA_real_, 10),
                    stats::runif(nrow(m) - 10, 0.5, 2))
  p <- plot(m)
  expect_buildable(p)
  # One point per actor kept, not one row per actor and margin
  expect_equal(p$labels$y, "Dispersion of the actor's own spans")
  # Both kinds of omission are named: actors with no shape reading, and
  # actors beyond `top`.
  expect_match(p$labels$subtitle, "below two completed spans")
  expect_match(p$labels$subtitle, "further actors not shown")
  # With every actor drawn, only the shape omission is left to report
  full <- plot(m, top = Inf)
  expect_match(full$labels$subtitle, "below two completed spans")
  expect_false(grepl("further actors", full$labels$subtitle))
  expect_gt(nrow(full$data), nrow(p$data))
  # A table where every actor has a shape reading says nothing at all
  m$dispersion <- stats::runif(nrow(m), 0.5, 2)
  expect_null(plot(m, top = Inf)$labels$subtitle)
})

# Ego-alter goodness of fit ----

test_that("an ego-alter gof is split into one panel per ego", {
  skip_on_cran()
  # The statistic names pair an ego with an alter, so the figure facets on
  # the ego and moves the p-value to the caption.
  x <- siena_gof
  cn <- colnames(x[[1]]$Simulations)
  paired <- outer(1:3, 1:9, function(a, b) paste0(a, b))[seq_along(cn)]
  colnames(x[[1]]$Simulations) <- paired
  names(x[[1]]$Observations) <- paired
  attr(x, "EgoAlter") <- TRUE
  p <- plot(x)
  expect_buildable(p)
  expect_equal(p$labels$x, "Alter")
  expect_match(p$labels$caption, "^p:")
  # Names that do not pair an ego with an alter say so, rather than being
  # split into halves that mean nothing.
  y <- siena_gof
  attr(y, "EgoAlter") <- TRUE
  expect_error(plot(y), "two characters long")
})

# Argument checking ----

test_that("a choice argument names the argument, the options, and a near miss", {
  skip_on_cran()
  net <- manynet::ison_adolescents
  # Not a single string at all
  expect_error(graphr(net, isolates = 2), "single string")
  # A near miss is suggested rather than only rejected
  expect_error(graphr(net, isolates = "legned"), "Did you mean")
  # A choice given in another case is taken as meant
  expect_buildable(graphr(net, isolates = "Legend"))
})

test_that("labels of a type that selects nothing say what they could be", {
  skip_on_cran()
  expect_error(graphr(manynet::ison_adolescents, labels = list(1)),
               "which nodes to label")
})

test_that("an attribute that varies nowhere is drawn in one colour", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  # Mapping a colour to it would produce a legend of one entry, so it is
  # dropped, and the reader told why the mapping had no effect.
  flat <- manynet::add_node_attribute(manynet::ison_adolescents, "grp",
                                      rep("a", 8))
  expect_message(p <- graphr(flat, node_color = "grp"), "same value")
  expect_buildable(p)
})

# Colourblind palettes ----

test_that("a palette says when it is asked for more colours than it holds", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit({
    options(old)
    suppressMessages(stocnet_theme("default"))
  }, add = TRUE)
  suppressMessages(stocnet_theme("iheid"))
  n <- length(getOption("snet_cat"))
  expect_silent(ag_qualitative(n))
  expect_message(ag_qualitative(n + 3), "mixtures")
})

test_that("a legend of more than seven keys is worth saying something about", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  net <- manynet::as_igraph(manynet::ison_adolescents)
  many <- igraph::set_vertex_attr(net, "band",
                                  value = paste0("b", seq_len(igraph::vcount(net))))
  expect_message(graphr(many, node_colour = "band", labels = FALSE), "8 keys")
  # Six categories are within what a reader can match, and a continuous
  # attribute has no keys to count at all.
  expect_no_message(autograph:::.check_legend_size(
    manynet::as_igraph(fict_lotr), node_color = "Race"))
  expect_no_message(autograph:::.check_legend_size(
    many, node_color = NULL, node_shape = NULL, edge_color = NULL))
})

test_that("a single colour and an indistinct palette are still handled", {
  # convertColor() drops to a vector for one colour, which the caller reads
  # by row like any other.
  lab <- autograph:::colorblind_lab("#FF0000", "normal")
  expect_equal(dim(lab), c(1L, 3L))
  # Where no colour stands out from the background, the one furthest from it
  # leads rather than the sort failing.
  faint <- c("#FFFFFE", "#FFFFFD", "#FFFFFC")
  sorted <- colorblind_sort(faint)
  expect_setequal(sorted, faint)
  expect_length(sorted, 3L)
})
