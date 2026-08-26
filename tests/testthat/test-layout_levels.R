# Levels layout
test_that("levels is the default layout for multilevel networks", {
  skip_on_cran()
  # fict_marvel interlocks a one-mode layer among its characters with a
  # two-mode layer of their affiliations. A layered layout would put each
  # mode on a row of its own, collapsing the one-mode layer entirely.
  expect_true(.ag_is_multilevel(fict_marvel))
  expect_equal(graphr(fict_marvel)$plot_env$layout, "levels")
  expect_equal(graphr(fict_actually)$plot_env$layout, "levels")
  # Two-mode networks whose ties all run between the modes are unaffected,
  # as are one-mode networks.
  expect_false(.ag_is_multilevel(ison_southern_women))
  expect_equal(graphr(ison_southern_women)$plot_env$layout, "layered")
  expect_equal(graphr(ison_adolescents)$plot_env$layout, "stress")
  # The one-mode layer of fict_marvel on its own is not multilevel.
  expect_equal(graphr(to_giant(to_uniplex(fict_marvel,
                                          "relationship")))$plot_env$layout,
               "stress")
})

test_that("levels layout infers its levels when none are given", {
  skip_on_cran()
  # Both of these used to fail with "argument 'level' is missing, with no
  # default": the levels were never derived, only reported as found.
  expect_equal(nrow(layout_levels(to_multilevel(fict_marvel))),
               as.integer(net_nodes(fict_marvel)))
  p <- graphr(fict_marvel, layout = "levels")
  expect_equal(p$plot_env$layout, "levels")
  expect_buildable(p)
  # Naming the levels explicitly still works, and agrees with the inferred
  # levels, since fict_marvel holds its within-mode ties in the first mode.
  expect_equal(layout_levels(fict_marvel, level = "type"),
               layout_levels(fict_marvel))
})

test_that("levels layout keeps the ordering of numeric levels", {
  skip_on_cran()
  # as.factor() would re-code these in sorted order, silently reversing the
  # levels of any attribute whose ordering is not already alphabetical.
  expect_equal(.as_level(c(3, 1, 2)), c(3L, 1L, 2L))
  expect_equal(.as_level(c("c", "a", "b")), c(3L, 1L, 2L))
  expect_equal(.as_level(c(FALSE, TRUE)), c(1L, 2L))
})

test_that("levels layout reports what it cannot lay out", {
  skip_on_cran()
  # graphlayouts lays each level out separately for these methods, and levels
  # with no ties within them leave it an empty subgraph to lay out, which it
  # reports as "attempt to select less than one element in integerOneIndex".
  expect_error(graphr(fict_marvel, layout = "levels",
                      method = "separate"), "no ties within")
  expect_error(graphr(fict_marvel, layout = "levels",
                      method = "fix2"), "no ties within")
  expect_no_error(suppressMessages(layout_levels(fict_marvel,
                                                     method = "fix1")))
  expect_error(graphr(fict_marvel, layout = "levels", method = "bloop"),
               "method")
  # Distances are infinite between components, which graphlayouts reports as
  # "missing value where TRUE/FALSE needed". Two disjoint multilevel triads:
  # in each, two first-mode nodes are tied to each other and to one of the
  # second mode.
  disconnected <- igraph::make_undirected_graph(
    c(1,2, 1,3, 2,3, 4,5, 4,6, 5,6))
  igraph::V(disconnected)$type <- c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
  expect_true(.ag_is_multilevel(disconnected))
  expect_false(manynet::is_connected(disconnected))
  expect_error(layout_levels(disconnected), "connected")
  # A one-mode network has no levels to derive, and says so.
  expect_error(layout_levels(manynet::as_igraph(ison_adolescents)),
               "level")
})

test_that("levels layout draws each level at a size of its own", {
  skip_on_cran()
  # The default size shrinks with how crowded the plot is, but each level of a
  # levels layout is only as crowded as itself: sizing fict_marvel's 53
  # characters as if there were 194 of them draws them as specks.
  marvel <- ag_net(fict_marvel)
  sizes <- .infer_nsize(marvel, NULL, "levels")
  expect_length(unique(sizes), 2)
  expect_gt(min(sizes), .infer_nsize(marvel, NULL))
  # Other layouts, and an explicit size, are unaffected.
  expect_length(unique(.infer_nsize(marvel, NULL, "layered")), 1)
  expect_equal(unique(.infer_nsize(marvel, 5, "levels")), 5)
  # A default size is not mapped through aes(), so it is neither rescaled nor
  # given a legend of its own.
  p <- graphr(fict_marvel, labels = FALSE)
  expect_buildable(p)
  expect_false("size" %in% names(ggplot2::ggplot_build(p)$plot$guides$guides))
})

test_that("levels layout draws the ties between levels more faintly", {
  skip_on_cran()
  # Cross-level ties outnumber within-level ties in fict_marvel, and at equal
  # strength they curtain over both levels.
  marvel <- ag_net(fict_marvel)
  alphas <- .infer_ealpha(marvel, "levels")
  expect_length(unique(alphas), 2)
  expect_lt(max(alphas[manynet::tie_is_twomode(marvel)]),
            min(alphas[!manynet::tie_is_twomode(marvel)]))
  # Every other layout keeps the single constant it always used.
  expect_equal(.infer_ealpha(marvel, "layered"), 0.4)
  expect_equal(.infer_ealpha(ag_net(ison_adolescents), "levels"), 0.4)
  # The varying alpha reaches the drawn edges rather than being rescaled.
  built <- ggplot2::ggplot_build(graphr(fict_marvel, labels = FALSE))
  expect_setequal(round(unique(built$data[[1]]$edge_alpha), 2), c(0.08, 0.5))
})

test_that("levels labels are tied to the nodes they belong to", {
  skip_on_cran()
  p <- graphr(fict_actually)
  lab <- p[["layers"]][[length(p[["layers"]])]]
  expect_s3_class(lab[["geom"]], "GeomTextRepel")
  # A leader line however short the displacement, rather than only past
  # ggrepel's default half a line of text.
  expect_equal(lab[["geom_params"]][["min.segment.length"]], 0)
  # Pulled back towards its own node, so that most labels need no line at all.
  expect_equal(lab[["geom_params"]][["force_pull"]], 4)
  expect_equal(lab[["geom_params"]][["box.padding"]], 0.1)
  expect_buildable(p)
})

