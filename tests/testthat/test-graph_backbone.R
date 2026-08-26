# The backbone behind `graphr(backbone = )`: which ties a filter keeps, how
# they are drawn, and the layouts they move. See R/graph_backbone.R.

# A modular network dense enough to draw as a hairball: four groups of thirty,
# tied often within a group and seldom between.
bb_fixture <- local({
  set.seed(42)
  pm <- matrix(0.03, 4, 4)
  diag(pm) <- 0.35
  ag_net(igraph::sample_sbm(120, pm, rep(30, 4)))
})

bb_alphas <- function(p) {
  sort(unique(ggplot2::ggplot_build(p)$data[[1]][["edge_alpha"]]))
}

test_that("the backbone argument resolves each of the forms it takes", {
  expect_identical(.check_backbone(NULL), "auto")
  expect_null(.check_backbone(FALSE))
  expect_equal(.check_backbone(TRUE), list(filter = NULL, threshold = NULL))
  expect_equal(.check_backbone("disparity"),
               list(filter = "disparity", threshold = NULL))
  expect_equal(.check_backbone(0.01), list(filter = NULL, threshold = 0.01))
  # A misspelling is named, with the nearest filter suggested.
  expect_error(.check_backbone("simmelain"), "simmelian")
  expect_error(.check_backbone(5), "threshold between 0 and 1")
  expect_error(.check_backbone(c(TRUE, FALSE)), "threshold between 0 and 1")
})

test_that("only a large, dense network counts as a hairball", {
  expect_true(.is_hairball(bb_fixture))
  expect_false(.is_hairball(ag_net(manynet::ison_adolescents)))
  # Fifty nodes are enough only where the ties are, at four for each node.
  expect_false(.is_hairball(ag_net(manynet::create_ring(60))))
})

test_that("a backbone fades the ties the filter does not keep", {
  skip_if_not(manynet_has("tie_is_backbone"))
  p <- suppressMessages(graphr(bb_fixture, backbone = TRUE, labels = FALSE))
  expect_buildable(p)
  expect_equal(bb_alphas(p), c(0.08, 0.4))
  plain <- suppressMessages(graphr(bb_fixture, backbone = FALSE,
                                   labels = FALSE))
  expect_buildable(plain)
  expect_equal(bb_alphas(plain), 0.4)
})

test_that("a backbone is drawn without being asked for, and can be refused", {
  skip_if_not(manynet_has("tie_is_backbone"))
  auto <- suppressMessages(graphr(bb_fixture, labels = FALSE))
  expect_equal(bb_alphas(auto), c(0.08, 0.4))
  # A network the reader can already follow is left alone.
  small <- graphr(manynet::ison_adolescents)
  expect_equal(bb_alphas(small), 0.4)
})

test_that("a backbone moves the layouts that read tie lengths", {
  skip_if_not(manynet_has("tie_is_backbone"))
  moved <- suppressMessages(graphr(bb_fixture, layout = "stress",
                                   backbone = TRUE, labels = FALSE))
  plain <- suppressMessages(graphr(bb_fixture, layout = "stress",
                                   backbone = FALSE, labels = FALSE))
  expect_false(isTRUE(all.equal(moved$data[, c("x", "y")],
                                plain$data[, c("x", "y")])))
  # A layout whose coordinates carry meaning keeps them, and fades only.
  fixed <- suppressMessages(graphr(manynet::ison_networkers,
                                   layout = "scaling", backbone = TRUE,
                                   labels = FALSE))
  unfixed <- suppressMessages(graphr(manynet::ison_networkers,
                                     layout = "scaling", backbone = FALSE,
                                     labels = FALSE))
  expect_equal(fixed$data[, c("x", "y")], unfixed$data[, c("x", "y")])
  # Building this network on this layout warns whether or not a backbone is
  # drawn, which is not this test's business.
  expect_equal(suppressWarnings(bb_alphas(fixed)), c(0.08, 0.4))
})

test_that("a tie length points the way each layout reads it", {
  # A line of four nodes, of which the middle tie is the one kept. Every node
  # holds a kept tie except the last, whose only tie is anchored below.
  line <- igraph::make_graph(~ A - B, B - C, C - D)
  mark <- c(FALSE, TRUE, FALSE)
  # ggraph inverts the weights it hands to "stress", so a larger weight there
  # draws two nodes together, as it does in "fr" and "drl".
  # The first and last ties are drawn short as well, since they are all that
  # holds nodes A and D beside the rest. See `.backbone_anchored()`.
  expect_equal(.backbone_layout_weights(line, "stress", mark), c(4, 4, 4))
  # A node that the filter left a tie of is not anchored again.
  star <- igraph::make_graph(~ A - B, B - C, A - C, C - D)
  expect_equal(.backbone_anchored(star, c(TRUE, TRUE, TRUE, FALSE)),
               c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(.backbone_anchored(star, c(TRUE, TRUE, TRUE, TRUE)),
               c(TRUE, TRUE, TRUE, TRUE))
  short <- .backbone_anchored(line, mark)
  expect_equal(.backbone_layout_weights(line, "fr", mark),
               ifelse(short, 4, 1))
  # "kk" reads a weight as a distance, so the ties drawn short take the
  # smaller one.
  expect_equal(.backbone_layout_weights(line, "kk", mark),
               ifelse(short, 1, 4))
  # A layout that reads no tie lengths is left as it is.
  expect_null(.backbone_layout_weights(line, "circle", mark))
  expect_null(.backbone_layout_weights(line, "layered", mark))
  expect_false(.backbone_moves_layout("scaling"))
})

test_that("a network without a backbone to draw is drawn as it was", {
  skip_if_not(manynet_has("tie_is_backbone"))
  # A signed network has no backbone, since these null models have no place
  # for a negative weight.
  signed <- suppressMessages(graphr(manynet::fict_marvel, backbone = TRUE,
                                    labels = FALSE))
  expect_buildable(signed)
  expect_null(.infer_backbone(ag_net(manynet::fict_marvel),
                              list(filter = NULL, threshold = NULL)))
  # A two-mode network holds no triangle, so a Simmelian filter keeps no tie.
  expect_null(.infer_backbone(ag_net(manynet::ison_southern_women),
                              list(filter = NULL, threshold = NULL)))
  # An empty network has no tie to mark.
  expect_null(.infer_backbone(ag_net(manynet::create_empty(10)),
                              list(filter = NULL, threshold = NULL)))
})

test_that("a bundled network is drawn without a fading", {
  skip_if_not(manynet_has("tie_is_backbone"))
  p <- suppressMessages(graphr(bb_fixture, backbone = TRUE, labels = FALSE,
                               edge_bundle = TRUE))
  expect_buildable(p)
})

test_that("the filters are offered as completions", {
  vals <- .completion_values("backbone", bb_fixture)
  expect_setequal(vals[["value"]], .backbone_filters())
})
