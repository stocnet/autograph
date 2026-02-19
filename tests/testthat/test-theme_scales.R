##### test scales
test_that("scales graph correctly", {
  test_sdg <- ison_brandes %>%
    mutate(color = c(rep(c(1,2,3,4,5), 2), 1)) %>%
    graphr(node_color = color) +
    scale_fill_sdgs()
  test_iheid <- ison_brandes %>%
    mutate(color = c(rep(c(1,2), 5), 3)) %>%
    graphr(node_color = color) +
    scale_fill_iheid()
  test_ethz <- ison_brandes %>%
    mutate(color = c(rep(c(1,2,3), 3), 4, 5)) %>%
    graphr(node_color = color) +
    scale_fill_ethz()
  test_uzh <- ison_brandes %>%
    mutate(color = c(rep(c(1,2,3), 3), 1, 2)) %>%
    graphr(node_color = color) +
    scale_fill_uzh()
  test_rug <- ison_brandes %>%
    mutate(color = c(rep(c(1,2), 4), 1, 2, 1)) %>%
    graphr(node_color = color) +
    scale_fill_rug()
  expect_equal(as.character(test_sdg[["scales"]][["scales"]][[2]][["call"]]), "scale_fill_sdgs")
  expect_equal(as.character(test_iheid[["scales"]][["scales"]][[2]][["call"]]), "scale_fill_iheid")
  expect_equal(as.character(test_ethz[["scales"]][["scales"]][[2]][["call"]]), "scale_fill_ethz")
  expect_equal(as.character(test_uzh[["scales"]][["scales"]][[2]][["call"]]), "scale_fill_uzh")
  expect_equal(as.character(test_rug[["scales"]][["scales"]][[2]][["call"]]), "scale_fill_rug")
})
