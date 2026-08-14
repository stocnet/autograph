# A representative sample of real {manynet} datasets, one per network format.
# This used to sweep every ison_/fict_/irps_/mpn_ object in the package, which
# dominated check time for little extra signal: format coverage is the job of
# the ag_fixtures grid (helper-functional.R), and the messier datasets each have
# a dedicated regression test further down this file. What remains here is a
# fast smoke test that graphr() still handles real, un-synthetic data. It is
# deliberately *not* skip_on_cran(), so CRAN retains a check of the core entry
# point; keep this list short so that stays true.
graphr_smoke_data <- c(
  "ison_adolescents",   # labelled, one-mode
  "ison_southern_women",# two-mode
  "ison_networkers",    # directed, weighted
  "ison_algebra",       # multiplex
  "fict_marvel",        # signed
  "ison_monks"          # longitudinal
)
for (nm in graphr_smoke_data) {
  test_that(paste("graphr() works on", nm), {
    expect_buildable(graphr(get(nm, envir = asNamespace("manynet"))))
  })
}

fmrg <- to_giant(to_uniplex(fict_marvel, "relationship"))

test_that("unweighted, unsigned, undirected networks graph correctly", {
  skip_on_cran()
  # Unweighted, unsigned, undirected network
  test_brandes <- graphr(ison_brandes)
  # Node position
  expect_equal(round(test_brandes[["data"]][["x"]][[1]]), 3)
  expect_equal(round(test_brandes[["data"]][["y"]][[1]]), -1)
  # Edge parameters
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_brandes[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  # Node parameters
  expect_equal(round(test_brandes[["layers"]][[2]][["aes_params"]][["size"]]), 11)
  expect_equal(test_brandes[["layers"]][[2]][["aes_params"]][["shape"]], 21)  # fillable circle
})

test_that("unweighted, signed, undirected networks graph correctly", {
  skip_on_cran()
  # Unweighted, signed, undirected network
  test_marvel <- graphr(fmrg)
  # Node position
  expect_equal(round(test_marvel[["data"]][["x"]][[1]]), -1)
  expect_equal(round(test_marvel[["data"]][["y"]][[1]]), 1)
  # Edge parameters
  expect_equal(test_marvel[["layers"]][[2]][["aes_params"]][["edge_alpha"]], 0.4)
  # Node parameters
  expect_equal(test_marvel[["layers"]][[4]][["aes_params"]][["size"]], 3)
  #expect_equal(test_marvel[["layers"]][[4]][["aes_params"]][["shape"]], "circle")
})

test_that("unweighted, unsigned, directed networks graph correctly", {
  skip_on_cran()
  # Unweighted, unsigned, directed network
  test_algebra <- graphr(ison_algebra)
  # Node position
  expect_equal(round(test_algebra[["data"]][["x"]][[1]]), 0)
  expect_equal(round(test_algebra[["data"]][["y"]][[1]]), 0)
  # Edge parameters
  expect_equal(test_algebra[["layers"]][[1]][["aes_params"]][["edge_alpha"]], 0.4)
  expect_equal(test_algebra[["layers"]][[1]][["aes_params"]][["edge_linetype"]], "solid")
  #expect_equal(test_algebra[["layers"]][[1]][["mapping"]][["edge_colour"]], "black")
  # Node parameters
  expect_equal(round(test_algebra[["layers"]][[2]][["aes_params"]][["size"]]), 8)
  expect_equal(test_algebra[["layers"]][[2]][["aes_params"]][["shape"]], 21)  # fillable circle
})

test_that("weighted, unsigned, directed networks graph correctly", {
  skip_on_cran()
  # Weighted, unsigned, directed network
  test_networkers <- graphr(ison_networkers)
  # Node position (exact coordinates vary across layout-engine versions,
  # so only check that a finite, non-degenerate layout was produced)
  expect_true(all(is.finite(test_networkers[["data"]][["x"]])))
  expect_true(all(is.finite(test_networkers[["data"]][["y"]])))
  expect_gt(stats::sd(test_networkers[["data"]][["x"]]), 0)
  expect_gt(stats::sd(test_networkers[["data"]][["y"]]), 0)
  # Edge parameters
  #expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_alpha"]], 0.4)
  #expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_linetype"]], "solid")
  #expect_equal(test_networkers[["layers"]][[2]][["aes_params"]][["edge_colour"]], "black")
  # Node parameters
  expect_equal(round(test_networkers[["layers"]][[3]][["aes_params"]][["size"]]), 3)
  #expect_equal(test_networkers[["layers"]][[3]][["aes_params"]][["shape"]], "circle")
})

# Testing the node_color, node_size, and node_shape args by specifying a node attribute
test_that("fancy node mods graph correctly", {
  skip_on_cran()
  # one-mode network
  fmrg <- dplyr::mutate(fmrg, nodesize = Appearances/1000)
  testcolnodes <- graphr(fmrg, node_color = "Gender",
                         node_size = "Appearances", 
                         node_shape = "Attractive")
  expect_s3_class(testcolnodes, c("ggraph","gg","ggplot"))
  expect_equal(nrow(testcolnodes[["plot_env"]][["lo"]]),
               c(net_nodes(fmrg)))
  # two-mode network
  ison_southern_women <- add_node_attribute(ison_southern_women, "group",
                                            c(sample(c("a", "b"),
                                                     length(ison_southern_women),
                                                     replace = TRUE)))
  test2 <- graphr(ison_southern_women, node_color = "type")
  expect_s3_class(test2, c("ggraph","gg","ggplot"))
  expect_equal(round(test2$data$x[1]), 0)
  expect_equal(round(test2$data$y[1]), 0)
  expect_equal(nrow(test2[["plot_env"]][["lo"]]),
               c(net_nodes(ison_southern_women)))
})

test_that("edge colours and edge size graph correctly", {
  skip_on_cran()
  ison_brandes2 <- ison_brandes %>%
    add_tie_attribute("tiecolour",
                      c("A", "B", "A", "B", "B", "B", "B", "B", "B", "B", "B", "B")) %>%
    add_tie_attribute("weight", c(rep(1:6, 2)))
  test_brandes2 <- graphr(ison_brandes2, edge_color = "tiecolour", edge_size = "weight")
  expect_false(is.null(test_brandes2$layers[[1]]$mapping$edge_colour))
  expect_false(is.null(test_brandes2$layers[[1]]$mapping$edge_width))
})

# Named networks
test_that("named networks plot correctly", {
  skip_on_cran()
  onemode <- graphr(ison_adolescents)
  twomode <- graphr(ison_southern_women)
  expect_equal(onemode[["data"]][["name"]], node_names(ison_adolescents))
  expect_equal(twomode[["data"]][["name"]], node_names(ison_southern_women))
})

# Test that autographr() works with arguments without quotes
test_that("node_group works correctly", {
  skip_on_cran()
  expect_equal(graphr(ison_lawfirm, node_group = gender),
               graphr(ison_lawfirm, node_group = "gender"))
})

test_that("unquoted arguments plot correctly", {
  skip_on_cran()
  expect_equal(graphr(ison_lawfirm, node_color = "gender"),
               graphr(ison_lawfirm, node_color = gender))
})

# Tests for fill aesthetic (color-to-fill change)
test_that("nodes use fill aesthetic instead of colour", {
  skip_on_cran()
  # Default node uses fill parameter
  p <- graphr(ison_brandes)
  expect_equal(p[["layers"]][[2]][["aes_params"]][["fill"]], "black")
  # Mapped node_color uses fill in aes
  p2 <- ison_brandes %>%
    dplyr::mutate(color = c(rep(c(1, 2), 5), 1)) %>%
    graphr(node_color = color)
  expect_false(is.null(p2[["layers"]][[2]][["mapping"]][["fill"]]))
})

test_that("node_color with multiple values uses fill scale", {
  skip_on_cran()
  # More than 2 colors triggers scale_fill_manual with qualitative palette
  p <- ison_brandes %>%
    dplyr::mutate(grp = c(rep(c("a", "b", "c"), 3), "a", "b")) %>%
    graphr(node_color = grp)
  expect_s3_class(p, c("ggraph", "gg", "ggplot"))
  # Check that fill scale is used (not colour)
  scale_names <- vapply(p[["scales"]][["scales"]], function(s) {
    paste(s[["aesthetics"]], collapse = ",")
  }, character(1))
  expect_true(any(grepl("fill", scale_names)))
})

test_that("two-mode networks get correct node shapes", {
  skip_on_cran()
  p <- graphr(ison_southern_women)
  expect_s3_class(p, c("ggraph", "gg", "ggplot"))
  # Two-mode shape mapping should use "One"/"Two" labels
  node_layer <- p[["layers"]][[2]]
  expect_false(is.null(node_layer[["mapping"]][["shape"]]))
})

test_that("two-mode shape legends name the modes where the network does", {
  skip_on_cran()
  # "One" and "Two" say nothing that the shapes do not already say.
  expect_equal(levels(.infer_nshape(fict_marvel, NULL)),
               c("characters", "teams"))
  expect_equal(levels(.infer_nshape(ison_southern_women, NULL)),
               c("women", "social events"))
  # A factor rather than a character vector, so that the first mode keeps the
  # first shape: "social events" would otherwise sort before "women" and the
  # two modes would swap symbols.
  shapes <- .infer_nshape(ison_southern_women, NULL)
  expect_s3_class(shapes, "factor")
  expect_equal(as.character(shapes[!manynet::node_is_mode(ison_southern_women)][1]),
               "women")
  # Networks that do not record their modes keep the old labels.
  expect_null(manynet::mode_names(irps_revere))
  expect_equal(levels(.infer_nshape(irps_revere, NULL)), c("One", "Two"))
  # One-mode networks are unaffected: mode_names() gives them a single name.
  expect_equal(.infer_nshape(ison_adolescents, NULL), 21)
})

test_that("multiplex networks are coloured by layer rather than by sign", {
  skip_on_cran()
  # fict_marvel's affiliation ties carry no sign, and were coloured "Negative"
  # while being drawn solid, so colour and linetype disagreed about which ties
  # were negative. Layer is what every tie has, and sign is left to linetype.
  colours <- .infer_ecolor(fict_marvel, NULL)
  expect_setequal(levels(colours), c("affiliation", "relationship"))
  expect_equal(.infer_ecolor_title(fict_marvel, NULL), "Layer")
  # Whether there are layers to draw is how many the ties are divided between,
  # which is 1 for a network whose ties are not divided at all.
  expect_equal(manynet::net_layers(ison_adolescents), 1L)
  expect_false(.has_layers(ison_adolescents))
  expect_gt(manynet::net_layers(fict_marvel), 1)
  expect_true(.has_layers(ison_algebra))
  # Not `is_multiplex()`, which is TRUE for a network with no layers to draw.
  expect_true(manynet::is_multiplex(ison_monks))
  expect_true(.has_layers(ison_monks))
  # Signed networks without layers still show their signs, and now treat an
  # absent sign as positive, as the linetype always did.
  signed <- manynet::to_uniplex(fict_marvel, "relationship")
  expect_equal(manynet::net_layers(signed), 1L)
  expect_false(.has_layers(signed))
  expect_setequal(levels(.infer_ecolor(signed, NULL)), c("Positive", "Negative"))
  expect_equal(.infer_ecolor_title(signed, NULL), "Sign")
  unsigned_ties <- is.na(igraph::E(fict_marvel)$sign)
  expect_equal(as.character(.infer_line_type(fict_marvel)[unsigned_ties][1]),
               "solid")
  # An explicitly named attribute still titles the legend after itself.
  expect_equal(.infer_ecolor_title(fict_marvel, "type"), "type")
})

test_that("signs are given a legend wherever the colours no longer carry them", {
  skip_on_cran()
  # The linetype is drawn through an identity scale, which shows no legend of
  # its own. That was right while the colours said "Sign" too, but leaves the
  # dashes unexplained now that the colours of a multiplex network say "Layer".
  guide_titles <- function(p) {
    vapply(ggplot2::ggplot_build(p)$plot$scales$scales, function(s) {
      nm <- s$name
      if (is.null(nm) || !is.character(nm)) NA_character_ else nm
    }, character(1))
  }
  expect_true("Sign" %in% guide_titles(graphr(fict_marvel)))
  # A signed network without layers is coloured by sign, so a second legend
  # saying the same thing is not drawn.
  signed <- manynet::to_giant(manynet::to_uniplex(fict_marvel, "relationship"))
  expect_false("Sign" %in% guide_titles(signed %>% graphr()))
  expect_buildable(graphr(fict_marvel))
})

test_that("color legend uses fillable shape when node_shape is also mapped", {
  skip_on_cran()
  p <- ison_brandes %>%
    dplyr::mutate(grp = c(rep(c("a", "b", "c"), 3), "a", "b"),
                  cat = c(rep(c("x", "y"), 5), "x")) %>%
    graphr(node_color = grp, node_shape = cat)
  # The fill guide should override shape to 21 so colors render in legend
  fill_guide <- p[["guides"]][["guides"]][["fill"]]
  expect_equal(fill_guide[["params"]][["override.aes"]][["shape"]], 21)
})

test_that("node_color with 2 values uses highlight palette", {
  skip_on_cran()
  p <- ison_brandes %>%
    dplyr::mutate(grp = c(rep(c("x", "y"), 5), "x")) %>%
    graphr(node_color = grp)
  expect_s3_class(p, c("ggraph", "gg", "ggplot"))
  # Should use scale_fill_manual with highlight defaults
  scale_names <- vapply(p[["scales"]][["scales"]], function(s) {
    paste(s[["aesthetics"]], collapse = ",")
  }, character(1))
  expect_true(any(grepl("fill", scale_names)))
})

test_that("edge_size = 0 fully suppresses arrowheads on directed networks (#50)", {
  skip_on_cran()
  net <- to_directed(ison_adolescents)
  p_zero <- graphr(net, edge_size = 0, labels = FALSE)
  expect_null(p_zero$layers[[1]]$geom_params$arrow)

  p_default <- graphr(net, labels = FALSE)
  expect_false(is.null(p_default$layers[[1]]$geom_params$arrow))

  p_thick <- graphr(net, edge_size = 3, labels = FALSE)
  expect_gt(grid::convertUnit(p_thick$layers[[1]]$geom_params$arrow$length, "mm", valueOnly = TRUE),
            grid::convertUnit(p_default$layers[[1]]$geom_params$arrow$length, "mm", valueOnly = TRUE))
})

test_that("label_dist and label_repel are respected (#52)", {
  skip_on_cran()
  net <- ison_adolescents
  p_default <- graphr(net)
  p_dist <- graphr(net, label_dist = 25)
  label_layer <- function(p) p$layers[[length(p$layers)]]
  expect_equal(grid::convertUnit(label_layer(p_dist)$geom_params$point.padding, "pt", valueOnly = TRUE), 25)
  expect_equal(grid::convertUnit(label_layer(p_default)$geom_params$point.padding, "pt", valueOnly = TRUE), 5)

  p_norepel <- graphr(net, label_repel = FALSE)
  expect_s3_class(label_layer(p_norepel)$geom, "GeomLabel")
  expect_s3_class(label_layer(p_default)$geom, "GeomLabelRepel")
})

test_that("labels stay clear of larger nodes (#13)", {
  skip_on_cran()
  small <- graphr(ison_adolescents, node_size = 3)
  big <- graphr(ison_adolescents, node_size = 20)
  built_small <- ggplot2::ggplot_build(small)
  built_big <- ggplot2::ggplot_build(big)
  n <- length(small$layers)
  expect_gt(mean(built_big$data[[n]]$point.size), mean(built_small$data[[n]]$point.size))
})

test_that("graphr() works on a stocnet-class object", {
  skip_on_cran()
  sn <- manynet::as_stocnet(ison_adolescents)
  expect_buildable(graphr(sn))
})

test_that("edge_bundle swaps in a bundling geom (#19)", {
  skip_on_cran()
  set.seed(123)
  net <- manynet::generate_random(40, 0.1)
  # Off by default: unchanged straight-edge geom
  p_off <- graphr(net)
  expect_s3_class(p_off$layers[[1]]$geom, "GeomEdgeSegment")
  expect_false(inherits(p_off$layers[[1]]$geom, "GeomEdgePath"))
  # TRUE / "force" both use force-directed bundling
  p_force <- graphr(net, edge_bundle = TRUE)
  expect_s3_class(p_force$layers[[1]]$geom, "GeomEdgePath")
  p_force2 <- graphr(net, edge_bundle = "force")
  expect_s3_class(p_force2$layers[[1]]$geom, "GeomEdgePath")
  # Alternative algorithms selectable by name
  p_path <- graphr(net, edge_bundle = "path")
  expect_s3_class(p_path$layers[[1]]$geom, "GeomEdgePath")
  # Bundling renders without error
  expect_buildable(p_force)
  # Directed networks bundle and retain an arrow
  dnet <- manynet::to_directed(manynet::generate_random(30, 0.12))
  p_dir <- graphr(dnet, edge_bundle = TRUE)
  expect_s3_class(p_dir$layers[[1]]$geom, "GeomEdgePath")
  expect_buildable(p_dir)
})

test_that("edge_bundle rejects unknown algorithms", {
  skip_on_cran()
  # Assert the message, not merely that *something* failed -- otherwise a
  # typo in the test itself would also pass.
  expect_error(graphr(ison_adolescents, edge_bundle = "banana"),
               "banana|edge_bundle|should be one of|arg")
})


test_that("graphr() on a changing network without diffusion events emits no max() warning (#57)", {
  skip_on_cran()
  # fict_potter is a dynamic/changing network but has no adoption ("I") events,
  # so the diffusion node-colour path must not warn on an all-infinite vector.
  expect_true(manynet::is_changing(manynet::fict_potter))
  expect_no_warning(p <- graphr(manynet::fict_potter))
  expect_buildable(p)
})


test_that("graphr() on a complex network (self-loops) emits no recycling warnings", {
  skip_on_cran()
  # geom_edge_arc()'s stat drops self-loops before drawing, so its length-
  # preserving `strength` parameter must exclude loop edges. Otherwise a full-
  # length vector recycles against the loop-free edge set, emitting ~14 "longer
  # object length is not a multiple" / "items to replace" warnings at build time.
  expect_true(manynet::is_complex(manynet::ison_emotions))
  p <- graphr(manynet::ison_emotions)
  expect_s3_class(p, "ggplot")
  expect_no_warning(ggplot2::ggplot_build(p))
})


test_that("graphr() renders a signed multiplex two-mode network without error", {
  skip_on_cran()
  # fict_marvel is signed, complex, multiplex, and two-mode. Because only its
  # signed layer carries a `sign`, the other layers' ties come back with NA
  # signs. Those NAs used to flow into the edge linetype/colour vectors and grid
  # rejected them at draw time ("invalid hex digit in 'color' or 'lty'"). They
  # must instead be drawn solid/positive, and the plot must build cleanly.
  expect_true(manynet::is_signed(manynet::fict_marvel))
  expect_true(manynet::is_multiplex(manynet::fict_marvel))
  expect_true(manynet::is_twomode(manynet::fict_marvel))
  p <- graphr(manynet::fict_marvel)
  expect_buildable(p)
  # Signed styling is preserved: positives solid, negatives dashed.
  lts <- unique(ggplot2::ggplot_build(p)$data[[1]]$edge_linetype)
  expect_setequal(lts, c("solid", "dashed"))
})

test_that("graphs()/graphr() render signed longitudinal snapshots without error", {
  skip_on_cran()
  # to_waves(ison_monks) yields signed, directed, weighted snapshots. Their
  # per-tie linetype must be mapped through aes() (not passed as a constant
  # parameter), otherwise geom_edge_arc's point expansion length-checks the
  # linetype vector against the expanded data and fails ("Aesthetics must be
  # either length 1 or the same as the data").
  waves <- manynet::to_waves(manynet::ison_monks)
  expect_true(manynet::is_signed(waves[[1]]))
  p1 <- graphr(waves[[1]])
  expect_buildable(p1)
  ps <- graphs(waves)
  expect_buildable(ps)
})
