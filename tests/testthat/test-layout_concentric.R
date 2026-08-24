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
