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


test_that("concentric refuses to draw a node in more than one circle", {
  skip_on_cran()
  # The circles are read from the node names, so two nodes of the same name
  # are one node in two circles, which the layout cannot draw.
  dupe <- manynet::as_igraph(ison_southern_women)
  igraph::V(dupe)$name[2] <- igraph::V(dupe)$name[1]
  expect_error(layout_concentric(dupe, membership = "type"), "one circle only")
})

test_that("concentric draws an unlabelled network", {
  skip_on_cran()
  # An unlabelled network used to put every node on a circle of its own,
  # because the groups were named while `is_labelled()` said they were not.
  un <- to_unnamed(ison_southern_women)
  lo <- layout_concentric(un)
  expect_equal(nrow(lo), as.integer(net_nodes(un)))
  expect_false(anyNA(lo))
  # The two modes are the two circles, so the nodes sit at two radii.
  expect_length(unique(round(sqrt(lo$x^2 + lo$y^2), 6)), 2)
  expect_s3_class(graphr(un, layout = "concentric"), "ggraph")
})

test_that("concentric gathers the nodes no group claims onto their own circle", {
  skip_on_cran()
  # A membership of NA names no group, so those nodes belong to none of them.
  net <- manynet::add_node_attribute(ison_adolescents, "grp",
                                     c("a", "a", "a", "b", "b", "b", NA, NA))
  lo <- layout_concentric(net, membership = "grp")
  expect_equal(nrow(lo), as.integer(net_nodes(net)))
  expect_false(anyNA(lo))
  # Three circles: the two groups, and the two nodes left over.
  radii <- round(sqrt(lo$x^2 + lo$y^2), 6)
  expect_length(unique(radii), 3)
  # Each group is drawn on one circle of its own.
  expect_length(unique(radii[1:3]), 1)
  expect_length(unique(radii[4:6]), 1)
  expect_length(unique(radii[7:8]), 1)
})

test_that("order.by orders the nodes around each circle", {
  skip_on_cran()
  net <- manynet::add_node_attribute(ison_adolescents, "grp",
                                     c("a", "a", "a", "a", "b", "b", "b", "b"))
  net <- manynet::add_node_attribute(net, "val", 8:1)
  lo <- layout_concentric(net, membership = "grp", order.by = "val")
  expect_equal(nrow(lo), as.integer(net_nodes(net)))
  expect_false(anyNA(lo))
  # The highest value in each group starts the circle, at angle zero.
  expect_equal(lo$x[1], 0.5)
  expect_equal(lo$y[1], 0)
  expect_equal(lo$x[5], 1)
  expect_equal(lo$y[5], 0)
  # Reversing the values reverses the order the nodes are drawn in.
  rev <- manynet::add_node_attribute(net, "rev", 1:8)
  lo2 <- layout_concentric(rev, membership = "grp", order.by = "rev")
  expect_equal(lo2$x[4], 0.5)
  expect_equal(lo2$y[4], 0)
  # A name no node attribute carries is reported rather than drawn.
  expect_error(layout_concentric(net, membership = "grp", order.by = "vale"),
               "Could not find")
})
