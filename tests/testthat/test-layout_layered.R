# Layered layouts
test_that("layered and lineage layouts graph correctly", {
  skip_on_cran()
  test_lin <- ison_adolescents |> 
    mutate(year = rep(c(1985, 1990, 1995, 2000), times = 2)) |>
    graphr(layout = "lineage", ranks = "year")
  test_hie <- graphr(ison_southern_women,
                     layout = "layered", center = "events")
  expect_equal(test_lin$plot_env$layout, "lineage")
  expect_equal((eval(quote(pairlist(...)),
                     envir = test_lin[["plot_env"]])[["ranks"]]),
               "year")
  expect_equal(test_hie$plot_env$layout, "layered")
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

test_that("layered layout works for two mode networks", {
  skip_on_cran()
  tm <- ison_brandes |>
    mutate(type = twomode_type, name = LETTERS[1:11]) |>
    graphr()
  expect_length(unique(tm$data[tm$data$type == TRUE, "y"]), 1)
  expect_length(unique(tm$data[tm$data$type == FALSE, "y"]), 1)
})

test_that("default layered layout uses sugiyama for two-mode networks", {
  skip_on_cran()
  p <- graphr(ison_southern_women, layout = "layered")
  expect_s3_class(p, c("ggraph", "gg", "ggplot"))
  expect_equal(p$plot_env$layout, "layered")
  # Two-mode should have exactly 2 unique y values (layers)
  expect_equal(length(unique(round(p$data$y, 6))), 2)
})

test_that("layered is the default layout for directed acyclic networks", {
  skip_on_cran()
  thrones <- to_layer(fict_thrones, "parent")
  expect_true(is_directed(thrones) && is_acyclic(thrones))
  expect_equal(graphr(thrones)$plot_env$layout, "layered")
  # ison_adolescents is acyclic but undirected, so it has no roots to hang
  # from and keeps the force-directed default.
  expect_false(is_directed(ison_adolescents))
  expect_equal(graphr(ison_adolescents)$plot_env$layout, "stress")
})

test_that("layered draws parents above their children", {
  skip_on_cran()
  thrones <- to_layer(fict_thrones, "parent")
  lo <- layout_layered(thrones)
  ties <- igraph::as_edgelist(as_igraph(thrones), names = FALSE)
  expect_true(all(lo$y[ties[, 1]] > lo$y[ties[, 2]]))
})

test_that("layered places every node, isolates included", {
  skip_on_cran()
  thrones <- to_layer(fict_thrones, "parent")
  expect_equal(nrow(layout_layered(thrones)),
               as.integer(net_nodes(thrones)))
  # The retired "layered" layout dropped tie-less nodes, which failed here.
  expect_s3_class(graphr(thrones, isolates = "keep"), "ggraph")
})

test_that("layered packs the components apart", {
  skip_on_cran()
  thrones <- delete_isolates(to_layer(fict_thrones, "parent"))
  lo <- layout_layered(thrones)
  memb <- igraph::components(as_igraph(thrones), mode = "weak")$membership
  spans <- lapply(sort(unique(memb)), function(cc) range(lo$x[memb == cc]))
  spans <- spans[order(vapply(spans, `[`, numeric(1), 1))]
  # No component starts before the one to its left has finished.
  for (i in seq_along(spans)[-1])
    expect_gt(spans[[i]][1], spans[[i - 1]][2])
})

test_that("self-loops are sized to the layout rather than stretching it", {
  skip_on_cran()
  # A loop's `strength` is its diameter in the layout's own coordinates, and
  # geom_edge_loop0() defaults it to 1. fict_marvel's levels layout spans
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

test_that("lineage layout works", {
  skip_on_cran()
  p <- graphr(ison_southern_women, layout = "lineage")
  expect_s3_class(p, c("ggraph", "gg", "ggplot"))
  expect_equal(p$plot_env$layout, "lineage")
})

test_that("layered layout minimises edge crossings", {
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

# The engine behind the layered layouts, and the two costs it minimises.

test_that("tight ranks shorten the ties", {
  skip_on_cran()
  thrones <- to_layer(fict_thrones, "parent")
  spans <- vapply(c("tight", "generation", "compact"), function(r)
    attr(check_span(graphr(thrones, ranks = r)), "total"), numeric(1))
  # Ranking by distance from a root pins a parent whose only child is several
  # generations down to the top row, which manufactures a long tie. Choosing
  # the ranks that shorten the ties instead is worth about a third.
  expect_lt(spans[["tight"]], spans[["generation"]])
  expect_lt(spans[["tight"]], spans[["compact"]])
  expect_lte(spans[["tight"]], 300)
  # The longest tie does not move, so it is the manufactured ties that go and
  # not the real ones.
  expect_equal(max(check_span(graphr(thrones))),
               max(check_span(graphr(thrones, ranks = "generation"))))
})

test_that("all three rank rules layer the same network alike but rank it differently", {
  skip_on_cran()
  thrones <- to_layer(fict_thrones, "parent")
  rows <- lapply(c("tight", "generation", "compact"), function(r)
    layout_layered(thrones, ranks = r)$y)
  expect_equal(length(unique(rows[[1]])), length(unique(rows[[2]])))
  expect_equal(length(unique(rows[[1]])), length(unique(rows[[3]])))
  expect_false(identical(rows[[1]], rows[[2]]))
})

test_that("straight alignment straightens the ties, rungs does not", {
  skip_on_cran()
  thrones <- to_layer(fict_thrones, "parent")
  straight <- attr(check_offset(graphr(thrones)), "mean")
  rungs <- attr(check_offset(graphr(thrones, alignment = "rungs")), "mean")
  expect_lt(straight, rungs)
  expect_lt(straight, 0.04)
})

test_that("the rank rules fall back where the network is not acyclic", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  cyclic <- igraph::graph_from_data_frame(
    data.frame(from = c("A", "B", "C", "C"), to = c("B", "C", "A", "D")),
    directed = TRUE)
  expect_message(lo <- layout_layered(cyclic), "acyclic")
  expect_equal(nrow(lo), igraph::vcount(cyclic))
  expect_false(anyNA(lo))
})

test_that("lineage is layered with the axes exchanged", {
  skip_on_cran()
  thrones <- to_layer(fict_thrones, "parent")
  h <- layout_layered(thrones)
  a <- layout_lineage(thrones)
  expect_equal(a$x, -h$y)
  expect_equal(a$y, h$x)
})

test_that("railway gives every layer the same spacing", {
  skip_on_cran()
  lo <- layout_railway(ison_southern_women)
  for (row in unique(lo$y)) {
    spaced <- sort(lo$x[lo$y == row])
    expect_equal(length(unique(round(diff(spaced), 8))), 1L)
  }
})

# The pieces of the engine ----

test_that(".tighten_layers keeps every tie pointing down and shortens them", {
  skip_on_cran()
  g <- as_igraph(delete_isolates(to_layer(fict_thrones, "parent")))
  ties <- igraph::as_edgelist(g, names = FALSE)
  loose <- autograph:::.rank_layers(g)
  tight <- autograph:::.tighten_layers(g)
  feasible <- function(r) all(r[ties[, 2]] > r[ties[, 1]])
  expect_true(feasible(loose))
  expect_true(feasible(tight))
  total <- function(r) sum(r[ties[, 2]] - r[ties[, 1]])
  expect_lte(total(tight), total(loose))
  # Running it again on its own output changes nothing.
  expect_equal(autograph:::.tighten_layers(g, tight), tight)
})

test_that(".place_layer respects the order and the separation", {
  want <- c(1, 1, 1, 8, 2)
  got <- autograph:::.place_layer(want, sep = 1)
  expect_length(got, length(want))
  expect_true(all(diff(got) >= 1 - 1e-9))
  # An input that already satisfies both is returned as it is.
  fine <- c(1, 2, 3, 4)
  expect_equal(autograph:::.place_layer(fine, sep = 1), fine)
  expect_equal(autograph:::.place_layer(5), 5)
})

# The exported checks ----

test_that("check_span and check_offset read a graphr plot", {
  skip_on_cran()
  thrones <- to_layer(fict_thrones, "parent")
  p <- graphr(thrones)
  ties <- net_ties(delete_isolates(thrones))
  span <- check_span(p)
  offset <- check_offset(p)
  expect_length(span, ties)
  expect_length(offset, ties)
  expect_equal(attr(span, "total"), sum(span))
  expect_equal(attr(span, "mean"), mean(span))
  expect_equal(attr(offset, "mean"), mean(offset))
  expect_true(all(span >= 1))
  expect_true(all(offset >= 0 & offset <= 1))
  # Read from the axis holding the rows, so a flipped layout scores the same.
  expect_equal(as.vector(check_span(graphr(thrones, layout = "lineage"))),
               as.vector(span))
})

test_that("the checks say what they need", {
  expect_error(check_span(list(data = data.frame(a = 1))), "coordinates")
  expect_error(check_offset(list(data = data.frame(x = 1, y = 1))), "network")
})
