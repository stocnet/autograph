# grapht() builds a frame-stacked ggplot; none of these tests render a gif.
skip_if_not_installed("gganimate")
skip_if_not_installed("gifski")

# Fixtures ----

.wave_fixture <- function() {
  set.seed(123)
  manynet::to_waves(
    manynet::mutate_ties(manynet::ison_adolescents,
                         wave = sample(1995:1998, 10, replace = TRUE)),
    cumulative = TRUE)
}

.comp_fixture <- function() {
  # Changing node composition: B exits after t1, D/E enter later
  list(t1 = manynet::as_tidygraph(igraph::graph_from_data_frame(
         data.frame(from = c("A", "B"), to = c("B", "C")), directed = FALSE)),
       t2 = manynet::as_tidygraph(igraph::graph_from_data_frame(
         data.frame(from = c("A", "D"), to = c("D", "E")), directed = FALSE)),
       t3 = manynet::as_tidygraph(igraph::graph_from_data_frame(
         data.frame(from = c("C", "E"), to = c("E", "A")), directed = FALSE)))
}

.node_layer <- function(p) {
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  p$layers[[which(geoms == "GeomPoint")[1]]]
}

.edge_layer <- function(p) {
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  p$layers[[which(geoms == "GeomSegment")[1]]]
}

# Structure ----

test_that("grapht() returns a grapht object with full frame data", {
  p <- grapht(.wave_fixture())
  expect_s3_class(p, "grapht")
  expect_s3_class(p, "gganim")
  expect_equal(attr(p, "nwaves"), 3)
  nd <- .node_layer(p)$data
  n_union <- length(unique(nd$name))
  n_frames <- length(unique(nd$frame))
  expect_equal(nrow(nd), n_union * n_frames)
  # every node appears in every frame
  expect_true(all(table(nd$name) == n_frames))
})

test_that("grapht() accepts an unsplit longitudinal network", {
  set.seed(123)
  net <- manynet::mutate_ties(manynet::ison_adolescents,
                              wave = sample(1995:1998, 10, replace = TRUE))
  p <- grapht(net)
  expect_s3_class(p, "grapht")
  expect_gt(attr(p, "nwaves"), 1)
})

test_that("grapht() accepts an unsplit dynamic network", {
  # Dynamic (time-stamped, event-based) networks are split into cumulative
  # time slices via manynet::to_slices(), which needs an increment/weight.
  el <- data.frame(from = c("A", "B", "C", "A", "D"),
                   to = c("B", "C", "D", "D", "B"),
                   time = c(1, 1, 2, 2, 3), increment = 1)
  net <- manynet::as_tidygraph(igraph::graph_from_data_frame(el,
                                                             directed = FALSE))
  skip_if_not(manynet::is_dynamic(net))
  p <- grapht(net)
  expect_s3_class(p, "grapht")
  expect_equal(attr(p, "nwaves"), 3)
})

# Layout and label defaults ----

test_that("two-mode networks default to the dynamic stress layout, not hierarchy", {
  skip_if_not_installed("graphlayouts")
  w1 <- manynet::as_tidygraph(igraph::make_bipartite_graph(
    c(TRUE, TRUE, FALSE, FALSE, FALSE),
    c(1, 3, 1, 4, 2, 5)))
  w1 <- manynet::add_node_attribute(w1, "name", c("a", "b", "X", "Y", "Z"))
  w2 <- manynet::as_tidygraph(igraph::make_bipartite_graph(
    c(TRUE, TRUE, FALSE, FALSE, FALSE),
    c(2, 3, 1, 5, 2, 4)))
  w2 <- manynet::add_node_attribute(w2, "name", c("a", "b", "X", "Y", "Z"))
  skip_if_not(manynet::is_twomode(w1))
  p <- grapht(list(t1 = w1, t2 = w2))
  nd <- .node_layer(p)$data
  # a stress layout spreads nodes in 2D: y takes many distinct values, unlike
  # a hierarchy layout that places each mode on a single horizontal line
  expect_gt(length(unique(round(nd$y[nd$frame == unique(nd$frame)[1]], 6))), 2)
})

test_that("labels are suppressed by default for large networks", {
  set.seed(1)
  # 40-node network across two waves
  make_wave <- function() {
    el <- data.frame(from = sample(letters[1:20], 30, replace = TRUE),
                     to = paste0("n", sample(1:20, 30, replace = TRUE)))
    el <- el[el$from != el$to, ]
    manynet::as_tidygraph(igraph::graph_from_data_frame(el, directed = FALSE))
  }
  p <- grapht(list(t1 = make_wave(), t2 = make_wave()))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomText" %in% geoms)
  # explicit labels = TRUE overrides the suppression
  p2 <- grapht(list(t1 = make_wave(), t2 = make_wave()), labels = TRUE)
  geoms2 <- vapply(p2$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomText" %in% geoms2)
})

test_that("dense frames fade present edges below the sparse-network default", {
  set.seed(2)
  make_dense <- function() {
    el <- data.frame(from = sample(paste0("n", 1:40), 300, replace = TRUE),
                     to = sample(paste0("n", 1:40), 300, replace = TRUE))
    el <- el[el$from != el$to, ]
    manynet::as_tidygraph(igraph::graph_from_data_frame(el, directed = FALSE))
  }
  p <- grapht(list(t1 = make_dense(), t2 = make_dense()))
  ed <- .edge_layer(p)$data
  present_alpha <- unique(ed$ealpha[ed$ealpha > 0])
  expect_length(present_alpha, 1)
  expect_lt(present_alpha, 0.4)
  # sparse network keeps the standard 0.4
  sg <- manynet::as_tidygraph(igraph::graph_from_data_frame(
    data.frame(from = "A", to = "B"), directed = FALSE))
  ps <- grapht(list(t1 = sg, t2 = sg))
  eds <- .edge_layer(ps)$data
  expect_equal(unique(eds$ealpha[eds$ealpha > 0]), 0.4)
})

test_that("alpha = 1 anchors every node across frames", {
  skip_if_not_installed("graphlayouts")
  p <- grapht(.wave_fixture(), alpha = 1)
  nd <- .node_layer(p)$data
  stable <- vapply(split(nd[, c("x", "y")], nd$name),
                   function(d) nrow(unique(round(d, 8))) == 1, logical(1))
  expect_true(all(stable))
})

test_that("non-stress layouts fall back to a static aggregate layout", {
  p <- grapht(.wave_fixture(), layout = "circle")
  nd <- .node_layer(p)$data
  stable <- vapply(split(nd[, c("x", "y")], nd$name),
                   function(d) nrow(unique(d)) == 1, logical(1))
  expect_true(all(stable))
})

# Composition change and isolates ----

test_that("changing composition fades nodes in and out in place", {
  p <- grapht(.comp_fixture())
  nd <- .node_layer(p)$data
  expect_setequal(unique(nd$name), c("A", "B", "C", "D", "E"))
  # B is only present in t1
  expect_equal(nd$nalpha[nd$name == "B"][order(nd$frame[nd$name == "B"])],
               c(1, 0, 0))
  # absent nodes still have defined coordinates so tweening can pair them
  expect_false(any(is.na(nd$x)) || any(is.na(nd$y)))
})

test_that("zero-tie waves are retained", {
  empty <- manynet::as_tidygraph(igraph::graph_from_data_frame(
    data.frame(from = character(0), to = character(0)),
    directed = FALSE, vertices = data.frame(name = c("A", "B", "C"))))
  tied <- manynet::as_tidygraph(igraph::graph_from_data_frame(
    data.frame(from = "A", to = "B"),
    directed = FALSE, vertices = data.frame(name = c("A", "B", "C"))))
  p <- grapht(list(t1 = tied, t2 = empty, t3 = tied))
  expect_equal(attr(p, "nwaves"), 3)
  ed <- .edge_layer(p)$data
  expect_false(any(ed$status[ed$frame == "t2"]))
  expect_true(all(ed$status[ed$frame == "t1"]))
})

test_that("isolates argument controls fading of isolated nodes", {
  empty <- manynet::as_tidygraph(igraph::graph_from_data_frame(
    data.frame(from = character(0), to = character(0)),
    directed = FALSE, vertices = data.frame(name = c("A", "B", "C"))))
  tied <- manynet::as_tidygraph(igraph::graph_from_data_frame(
    data.frame(from = "A", to = "B"),
    directed = FALSE, vertices = data.frame(name = c("A", "B", "C"))))
  p_keep <- grapht(list(t1 = tied, t2 = empty), isolates = "keep")
  nd_keep <- .node_layer(p_keep)$data
  expect_true(all(nd_keep$nalpha == 1))
  p_fade <- grapht(list(t1 = tied, t2 = empty), isolates = "fade")
  nd_fade <- .node_layer(p_fade)$data
  # C is always an isolate; everyone is isolated in t2
  expect_true(all(nd_fade$nalpha[nd_fade$name == "C"] == 0))
  expect_true(all(nd_fade$nalpha[nd_fade$frame == "t2"] == 0))
  expect_true(all(nd_fade$nalpha[nd_fade$frame == "t1" &
                                   nd_fade$name %in% c("A", "B")] == 1))
})

test_that("keep_isolates is deprecated but still honoured", {
  empty <- manynet::as_tidygraph(igraph::graph_from_data_frame(
    data.frame(from = character(0), to = character(0)),
    directed = FALSE, vertices = data.frame(name = c("A", "B", "C"))))
  tied <- manynet::as_tidygraph(igraph::graph_from_data_frame(
    data.frame(from = "A", to = "B"),
    directed = FALSE, vertices = data.frame(name = c("A", "B", "C"))))
  expect_warning(p <- grapht(list(t1 = tied, t2 = tied), keep_isolates = FALSE),
                 "deprecated")
  nd <- .node_layer(p)$data
  expect_true(all(nd$nalpha[nd$name == "C"] == 0))
})

# Aesthetics ----

test_that("node aesthetics map with globally consistent levels", {
  set.seed(123)
  tl <- manynet::mutate_ties(
    manynet::mutate(manynet::ison_adolescents,
                    gender = rep(c("male", "female"), times = 4),
                    age = sample(11:16, 8, replace = TRUE)),
    wave = sample(1995:1998, 10, replace = TRUE))
  p <- grapht(tl, node_color = "gender", node_size = "age")
  nl <- .node_layer(p)
  expect_true(all(c("fill", "size") %in% names(nl$mapping)))
  expect_setequal(unique(nl$data$ncolor), c("male", "female"))
  # same levels present in every frame
  by_frame <- tapply(nl$data$ncolor, nl$data$frame,
                     function(x) length(unique(x)))
  expect_true(all(by_frame == 2))
})

test_that("directed networks get trimmed segments and an arrow", {
  dl <- list(
    t1 = manynet::as_tidygraph(igraph::graph_from_data_frame(
      data.frame(from = c("A", "B", "C"), to = c("B", "C", "A")))),
    t2 = manynet::as_tidygraph(igraph::graph_from_data_frame(
      data.frame(from = c("A", "C"), to = c("C", "B")))))
  p <- grapht(dl)
  el <- .edge_layer(p)
  expect_false(is.null(el$geom_params$arrow))
  nd <- .node_layer(p)$data
  raw_xend <- nd$x[match(paste(el$data$to, el$data$frame),
                         paste(nd$name, nd$frame))]
  expect_true(any(abs(el$data$xend - raw_xend) > 1e-10))
})

test_that("signed networks map sign to colour and linetype", {
  sg <- manynet::as_tidygraph(igraph::graph_from_data_frame(
    data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"),
               sign = c(1, -1, 1)), directed = FALSE))
  p <- grapht(list(t1 = sg, t2 = sg))
  el <- .edge_layer(p)
  expect_true(all(c("colour", "linetype") %in% names(el$mapping)))
  expect_setequal(unique(el$data$ecolor), c("Positive", "Negative"))
  expect_setequal(unique(el$data$linetype), c("solid", "dashed"))
})

test_that("diffusion models colour nodes by adoption state per wave", {
  set.seed(123)
  p <- grapht(manynet::play_diffusion(manynet::ison_adolescents, seeds = 5))
  nd <- .node_layer(p)$data
  expect_true(all(unique(nd$ncolor) %in%
                    c("Susceptible", "Exposed", "Infected", "Recovered")))
  # adoption states change across frames
  per_frame <- tapply(nd$ncolor, nd$frame, paste, collapse = "")
  expect_gt(length(unique(per_frame)), 1)
})

test_that("unlabelled networks work with labels suppressed", {
  ul <- lapply(1:2, function(i) manynet::as_tidygraph(manynet::create_ring(6)))
  p <- grapht(ul)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomText" %in% geoms)
})

# Internals ----

test_that(".grapht_waves harmonises waves over the union of nodes", {
  wl <- autograph:::.grapht_waves(.comp_fixture())
  expect_equal(length(wl$waves), 3)
  vnames <- lapply(wl$waves, function(w) igraph::V(w)$name)
  expect_true(all(vapply(vnames, identical, logical(1), vnames[[1]])))
  expect_equal(dim(wl$present), c(5, 3))
  expect_false(wl$present["B", "t2"])
  expect_true(wl$present["B", "t1"])
})

test_that(".fill_series carries values forward then backward", {
  expect_equal(autograph:::.fill_series(c(NA, "a", NA, "b", NA)),
               c("a", "a", "a", "b", "b"))
  expect_equal(autograph:::.fill_series(c(NA, NA)), c(NA, NA))
  expect_equal(autograph:::.fill_series(c(1, 2, 3)), c(1, 2, 3))
})

test_that(".shorten_segments never produces negative-length segments", {
  nodes_out <- data.frame(name = rep(c("A", "B"), 2),
                          frame = rep(c("t1", "t2"), each = 2),
                          x = c(0, 0.01, 0, 10), y = c(0, 0, 0, 0),
                          nsize = 20)
  edges_out <- data.frame(id = "A->B", from = "A", to = "B",
                          frame = c("t1", "t2"),
                          x = c(0, 0), y = c(0, 0),
                          xend = c(0.01, 10), yend = c(0, 0))
  out <- autograph:::.shorten_segments(edges_out, nodes_out)
  newlen <- sqrt((out$xend - out$x)^2 + (out$yend - out$y)^2)
  expect_true(all(newlen > 0))
  oldlen <- sqrt((edges_out$xend - edges_out$x)^2 +
                   (edges_out$yend - edges_out$y)^2)
  expect_true(all(newlen <= oldlen))
})

# Issue #40: the tutorial pipeline with a custom time attribute ----

test_that("grapht() works on to_waves() output split by a custom attribute (#40)", {
  skip_if_not_installed("manynet", minimum_version = "2.2.2")
  set.seed(123)
  p <- manynet::fict_lotr |>
    manynet::mutate_ties(year = sample(1:12, manynet::net_ties(manynet::fict_lotr),
                                       replace = TRUE)) |>
    manynet::to_waves(attribute = "year", cumulative = TRUE) |>
    grapht()
  expect_s3_class(p, "grapht")
  expect_equal(attr(p, "nwaves"), 12)
  # Frames animate in natural year order, not lexicographic order
  frames <- unique(as.character(.node_layer(p)$data$frame))
  expect_length(frames, 12)
  expect_equal(frames, as.character(sort(as.numeric(frames))))
})

test_that("grapht() aborts clearly when the input cannot be split into waves", {
  expect_error(grapht(manynet::ison_adolescents), "waves, or slices")
})

# Interval/spell networks (begin/end ties, e.g. irps_wwi) ----

test_that("grapht() splits a begin/end spell network into active-spell slices", {
  # irps_wwi records tie lifespans as begin/end spells; it is_dynamic() but has
  # no `time` attribute, so it must not be routed to to_slices().
  expect_true(autograph:::.grapht_is_spell(manynet::irps_wwi))
  expect_false(autograph:::.grapht_is_spell(manynet::irps_nuclear))

  slices <- autograph:::.grapht_spell_slices(manynet::irps_wwi)
  expect_type(slices, "list")
  # One slice per change point (each moment a tie begins or ends), named by year
  # and in order; this mirrors manynet::to_time() on manynet >= 2.2.2.
  changes <- sort(unique(c(manynet::tie_attribute(manynet::irps_wwi, "begin"),
                           manynet::tie_attribute(manynet::irps_wwi, "end"))))
  expect_equal(names(slices), as.character(changes))
  # Ties active at the first change point (begin <= t < end) are non-empty.
  expect_gt(manynet::net_ties(slices[[1]]), 0)

  p <- grapht(manynet::irps_wwi)
  expect_s3_class(p, "grapht")
  expect_equal(attr(p, "nwaves"), length(slices))
})
