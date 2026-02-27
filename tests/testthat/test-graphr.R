data_objs <- mget(ls("package:manynet"), inherits = TRUE)
# Filter to relevant objects 
data_objs <- data_objs[grepl("ison_|fict_|irps_|mpn_", names(data_objs))]
# data_objs <- data_objs[!grepl("starwars|physicians|potter", names(data_objs))]
for (nm in names(data_objs)) { 
  test_that(paste("graphr() works on", nm), {
    skip_if(grepl("starwars|physicians|potter|marvel", nm))
    expect_error(graphr(data_objs[[nm]]), NA) }) 
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
  skip_on_ci()
  # Weighted, unsigned, directed network
  test_networkers <- graphr(ison_networkers)
  # Node position
  expect_equal(round(test_networkers[["data"]][["x"]][[1]]), 9)
  expect_equal(round(test_networkers[["data"]][["y"]][[1]]), -1)
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
  skip_on_ci()
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
  skip_on_ci()
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

