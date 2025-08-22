# Layouts
test_that("concentric and circular layouts graph correctly", {
  skip_on_cran()
  test_circle <- graphr(to_giant(ison_marvel_relationships),
                        layout = "circle")
  test_conc <- graphr(to_giant(ison_marvel_relationships),
                      layout = "concentric", membership = "Gender")
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
  skip_on_ci()
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
  skip_on_ci()
  tm <- ison_brandes %>%
    mutate(type = twomode_type, name = LETTERS[1:11]) %>%
    graphr()
  expect_length(unique(tm$data[tm$data$type == TRUE, "y"]), 1)
  expect_length(unique(tm$data[tm$data$type == FALSE, "y"]), 1)
})
