# Layouts
test_that("concentric and circular layouts graph correctly", {
  skip_on_cran()
  fmrg <- to_giant(to_uniplex(fict_marvel, "relationship"))
  test_circle <- graphr(fmrg, layout = "circle")
  test_conc <- graphr(fmrg, layout = "concentric", membership = "Gender")
  expect_equal(test_circle$plot_env$layout, "circle")
  expect_equal(test_conc$plot_env$layout, "concentric")
  expect_equal(eval(quote(pairlist(...)),
                    envir = test_conc$plot_env)$membership,
               "Gender")
})

test_that("concentric layout works when node names are missing", {
  skip_on_cran()
  llabel <- ison_southern_women |>
    mutate(name = ifelse(type == TRUE, "", name)) |>
    graphr(layout = "concentric")
  expect_true(any(llabel$data$name == ""))
})

test_that("hierarchy and lineage layouts graph correctly", {
  skip_on_cran()
  test_lin <- ison_adolescents |> 
    mutate(year = rep(c(1985, 1990, 1995, 2000), times = 2)) |>
    graphr(layout = "lineage", rank = "year")
  test_hie <- graphr(ison_southern_women,
                     layout = "hierarchy", center = "events")
  expect_equal(test_lin$plot_env$layout, "lineage")
  expect_equal((eval(quote(pairlist(...)),
                     envir = test_lin[["plot_env"]])[["rank"]]),
               "year")
  expect_equal(test_hie$plot_env$layout, "hierarchy")
  expect_equal((eval(quote(pairlist(...)),
                     envir = test_hie[["plot_env"]])[["center"]]),
               "events")
})

# test_that("graphr works for diff_model objects", {
#   skip_on_cran()
#   skip_on_ci()
#   test_diff <- graphr(play_diffusion(ison_brandes, old_version = TRUE))
#   if (inherits(test_diff$guides, "Guides")) {
#     expect_s3_class(test_diff[["guides"]][["guides"]][["shape"]], "GuideLegend")
#     expect_s3_class(test_diff[["guides"]][["guides"]][["colour"]], "GuideColourbar")
#   } else {
#     expect_equal(test_diff[["guides"]][["shape"]][["name"]], "legend")
#     expect_equal(test_diff[["guides"]][["colour"]][["name"]], "colorbar")
#   }
# })

test_that("hierarchy layout works for two mode networks", {
  skip_on_cran()
  tm <- ison_brandes |>
    mutate(type = twomode_type, name = LETTERS[1:11]) |>
    graphr()
  expect_length(unique(tm$data[tm$data$type == TRUE, "y"]), 1)
  expect_length(unique(tm$data[tm$data$type == FALSE, "y"]), 1)
})

test_that("default hierarchy layout uses sugiyama for two-mode networks", {
  skip_on_cran()
  p <- graphr(ison_southern_women, layout = "hierarchy")
  expect_s3_class(p, c("ggraph", "gg", "ggplot"))
  expect_equal(p$plot_env$layout, "hierarchy")
  # Two-mode should have exactly 2 unique y values (layers)
  expect_equal(length(unique(round(p$data$y, 6))), 2)
})

test_that("multilevel is the default layout for multilevel networks", {
  skip_on_cran()
  # fict_marvel interlocks a one-mode layer among its characters with a
  # two-mode layer of their affiliations. A hierarchy layout would put each
  # mode on a row of its own, collapsing the one-mode layer entirely.
  expect_true(.ag_is_multilevel(fict_marvel))
  expect_equal(graphr(fict_marvel)$plot_env$layout, "multilevel")
  expect_equal(graphr(fict_actually)$plot_env$layout, "multilevel")
  # Two-mode networks whose ties all run between the modes are unaffected,
  # as are one-mode networks.
  expect_false(.ag_is_multilevel(ison_southern_women))
  expect_equal(graphr(ison_southern_women)$plot_env$layout, "hierarchy")
  expect_equal(graphr(ison_adolescents)$plot_env$layout, "stress")
  # The one-mode layer of fict_marvel on its own is not multilevel.
  expect_equal(graphr(to_giant(to_uniplex(fict_marvel,
                                          "relationship")))$plot_env$layout,
               "stress")
})

test_that("multilevel layout infers its levels when none are given", {
  skip_on_cran()
  # Both of these used to fail with "argument 'level' is missing, with no
  # default": the levels were never derived, only reported as found.
  expect_equal(nrow(layout_multilevel(to_multilevel(fict_marvel))),
               as.integer(net_nodes(fict_marvel)))
  p <- graphr(fict_marvel, layout = "multilevel")
  expect_equal(p$plot_env$layout, "multilevel")
  expect_buildable(p)
  # Naming the levels explicitly still works, and agrees with the inferred
  # levels, since fict_marvel holds its within-mode ties in the first mode.
  expect_equal(layout_multilevel(fict_marvel, level = "type"),
               layout_multilevel(fict_marvel))
})

test_that("multilevel layout keeps the ordering of numeric levels", {
  skip_on_cran()
  # as.factor() would re-code these in sorted order, silently reversing the
  # levels of any attribute whose ordering is not already alphabetical.
  expect_equal(.as_level(c(3, 1, 2)), c(3L, 1L, 2L))
  expect_equal(.as_level(c("c", "a", "b")), c(3L, 1L, 2L))
  expect_equal(.as_level(c(FALSE, TRUE)), c(1L, 2L))
})

test_that("multilevel layout reports what it cannot lay out", {
  skip_on_cran()
  # graphlayouts lays each level out separately for these methods, and levels
  # with no ties within them leave it an empty subgraph to lay out, which it
  # reports as "attempt to select less than one element in integerOneIndex".
  expect_error(graphr(fict_marvel, layout = "multilevel",
                      method = "separate"), "no ties within")
  expect_error(graphr(fict_marvel, layout = "multilevel",
                      method = "fix2"), "no ties within")
  expect_no_error(suppressMessages(layout_multilevel(fict_marvel,
                                                     method = "fix1")))
  expect_error(graphr(fict_marvel, layout = "multilevel", method = "bloop"),
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
  expect_error(layout_multilevel(disconnected), "connected")
  # A one-mode network has no levels to derive, and says so.
  expect_error(layout_multilevel(manynet::as_igraph(ison_adolescents)),
               "level")
})

test_that("multilevel layout draws each level at a size of its own", {
  skip_on_cran()
  # The default size shrinks with how crowded the plot is, but each level of a
  # multilevel layout is only as crowded as itself: sizing fict_marvel's 53
  # characters as if there were 194 of them draws them as specks.
  sizes <- .infer_nsize(fict_marvel, NULL, "multilevel")
  expect_length(unique(sizes), 2)
  expect_gt(min(sizes), .infer_nsize(fict_marvel, NULL))
  # Other layouts, and an explicit size, are unaffected.
  expect_length(unique(.infer_nsize(fict_marvel, NULL, "hierarchy")), 1)
  expect_equal(unique(.infer_nsize(fict_marvel, 5, "multilevel")), 5)
  # A default size is not mapped through aes(), so it is neither rescaled nor
  # given a legend of its own.
  p <- graphr(fict_marvel, labels = FALSE)
  expect_buildable(p)
  expect_false("size" %in% names(ggplot2::ggplot_build(p)$plot$guides$guides))
})

test_that("multilevel layout draws the ties between levels more faintly", {
  skip_on_cran()
  # Cross-level ties outnumber within-level ties in fict_marvel, and at equal
  # strength they curtain over both levels.
  alphas <- .infer_ealpha(fict_marvel, "multilevel")
  expect_length(unique(alphas), 2)
  expect_lt(max(alphas[manynet::tie_is_twomode(fict_marvel)]),
            min(alphas[!manynet::tie_is_twomode(fict_marvel)]))
  # Every other layout keeps the single constant it always used.
  expect_equal(.infer_ealpha(fict_marvel, "hierarchy"), 0.4)
  expect_equal(.infer_ealpha(ison_adolescents, "multilevel"), 0.4)
  # The varying alpha reaches the drawn edges rather than being rescaled.
  built <- ggplot2::ggplot_build(graphr(fict_marvel, labels = FALSE))
  expect_setequal(round(unique(built$data[[1]]$edge_alpha), 2), c(0.08, 0.5))
})

test_that("multilevel labels are tied to the nodes they belong to", {
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

test_that("self-loops are sized to the layout rather than stretching it", {
  skip_on_cran()
  # A loop's `strength` is its diameter in the layout's own coordinates, and
  # geom_edge_loop0() defaults it to 1. fict_marvel's multilevel layout spans
  # about one unit each way, so the single loop was drawn wider than the whole
  # network, stretching the panel to twice the width the nodes needed and
  # leaving a gap between the plot and its legend.
  expect_true(manynet::is_complex(fict_marvel))
  p <- graphr(fict_marvel, labels = FALSE)
  panel <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  # Only the usual 5% expansion either side, rather than room for the loop.
  expect_lt(diff(panel), diff(range(p$data$x)) * 1.25)
  # The loop is still drawn, at a fraction of the layout rather than all of it.
  loop <- ggplot2::ggplot_build(p)$data[[2]]
  expect_gt(diff(range(loop$x)), 0)
  expect_lt(diff(range(loop$x)), diff(range(p$data$x)) / 4)
})

test_that("alluvial layout works", {
  skip_on_cran()
  p <- graphr(ison_southern_women, layout = "alluvial")
  expect_s3_class(p, c("ggraph", "gg", "ggplot"))
  expect_equal(p$plot_env$layout, "alluvial")
})

test_that("hierarchy layout minimises edge crossings", {
  skip_on_cran()
  # Helper: count bipartite edge crossings given x positions
  count_crossings <- function(el, x_pos) {
    crossings <- 0
    if (nrow(el) < 2) return(0)
    for (i in 1:(nrow(el) - 1)) {
      for (j in (i + 1):nrow(el)) {
        a1 <- x_pos[el[i, 1]]; b1 <- x_pos[el[i, 2]]
        a2 <- x_pos[el[j, 1]]; b2 <- x_pos[el[j, 2]]
        if ((a1 - a2) * (b1 - b2) < 0) crossings <- crossings + 1
      }
    }
    crossings
  }
  # Test with ison_southern_women (18 women, 14 events, 89 ties)
  g <- manynet::as_igraph(ison_southern_women)
  n <- igraph::vcount(g)
  el <- igraph::as_edgelist(g, names = FALSE)
  layers <- ifelse(igraph::V(g)$type, 2, 1)
  lo <- autograph:::.sugiyama_layout(g, layers = layers, times = 100)
  x_pos <- lo[, 1]
  # Naive layout: sequential ordering within each layer
  naive_x <- rep(0, n)
  naive_x[layers == 1] <- seq_len(sum(layers == 1))
  naive_x[layers == 2] <- seq_len(sum(layers == 2))
  optimised_crossings <- count_crossings(el, x_pos)
  naive_crossings <- count_crossings(el, naive_x)
  # The optimised layout should have fewer crossings than naive
  expect_lt(optimised_crossings, naive_crossings)
  # Verify all nodes got valid positions
  expect_true(all(is.finite(x_pos)))
  expect_equal(length(unique(lo[layers == 1, 2])), 1)
  expect_equal(length(unique(lo[layers == 2, 2])), 1)
})
