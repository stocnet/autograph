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
  skip_on_ci()
  llabel <- ison_southern_women %>%
    mutate(name = ifelse(type == TRUE, "", name)) %>%
    graphr(layout = "concentric")
  expect_true(any(llabel$data$name == ""))
})

test_that("hierarchy and lineage layouts graph correctly", {
  skip_on_cran()
  test_lin <- ison_adolescents %>% 
    mutate(year = rep(c(1985, 1990, 1995, 2000), times = 2)) %>%
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
  tm <- ison_brandes %>%
    mutate(type = twomode_type, name = LETTERS[1:11]) %>%
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
