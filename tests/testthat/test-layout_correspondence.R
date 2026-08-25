# The correspondence layout and the fit it reports. See
# R/layout_correspondence.R.

test_that("correspondence layout graphs correctly", {
  skip_on_cran()
  p <- graphr(manynet::ison_southern_women, layout = "correspondence")
  expect_equal(p$plot_env$layout, "correspondence")
  expect_buildable(p)
  expect_equal(nrow(p$data),
               as.integer(manynet::net_nodes(manynet::ison_southern_women)))
})

test_that("correspondence layout places both modes in one space", {
  skip_on_cran()
  lo <- layout_correspondence(manynet::ison_southern_women)
  expect_named(lo, c("x", "y"))
  expect_equal(nrow(lo),
               as.integer(manynet::net_nodes(manynet::ison_southern_women)))
  expect_true(all(is.finite(as.matrix(lo))))
  # Neither mode is placed off on its own: both are drawn against the same
  # axes, which is the point of drawing a two-mode network this way.
  mode <- manynet::node_is_mode(manynet::ison_southern_women)
  expect_true(min(lo$x[mode]) < max(lo$x[!mode]))
  expect_true(min(lo$x[!mode]) < max(lo$x[mode]))
})

test_that("correspondence layout reports inertia and cos2", {
  skip_on_cran()
  fit <- attr(layout_correspondence(manynet::ison_southern_women), "fit")
  expect_equal(fit$type, "correspondence")
  # The published correspondence analysis of this network. Both dimensions
  # are shares of the total inertia, so both fall between 0 and 1.
  expect_equal(fit$inertia, c(0.380, 0.193), tolerance = 1e-2)
  expect_equal(fit$total, 1.65, tolerance = 1e-2)
  expect_length(fit$cos2,
                as.integer(manynet::net_nodes(manynet::ison_southern_women)))
  expect_true(all(fit$cos2 >= 0 & fit$cos2 <= 1))
  expect_named(fit$cos2, manynet::node_names(manynet::ison_southern_women))
})

test_that("correspondence layout reports every dimension, not only the two drawn", {
  skip_on_cran()
  fit <- attr(layout_correspondence(manynet::ison_southern_women), "fit")
  # A table of 18 rows and 14 columns supports 13 dimensions at most, and the
  # zeroes for those it does not support are dropped, so that the count is
  # what the inertia was actually spread over.
  expect_lte(length(fit$scree), 13)
  expect_equal(sum(fit$scree), 1)
  expect_true(!is.unsorted(rev(fit$scree)))
  # The two drawn are the first two of them.
  expect_equal(unname(fit$scree[1:2]), fit$inertia)
  # The percentages are exact rather than corrected: no Benzecri or adjusted
  # rescaling applies to a single two-way table, so the shares of the raw
  # eigenvalues are what is reported.
  m <- as.matrix(manynet::as_matrix(manynet::ison_southern_women))
  P <- m / sum(m)
  r <- rowSums(P)
  cm <- colSums(P)
  eig <- svd((P - outer(r, cm)) / outer(sqrt(r), sqrt(cm)))$d^2
  expect_equal(fit$inertia, eig[1:2] / sum(eig), tolerance = 1e-8)
  expect_equal(fit$total, sum(eig), tolerance = 1e-8)
})

test_that("correspondence layout is well defined for a one-mode network", {
  skip_on_cran()
  # A symmetric table places its rows and its columns identically, so a
  # one-mode network has one position for each node whichever side it is read
  # from, and the layout is not left to choose between them.
  lo <- layout_correspondence(manynet::ison_adolescents)
  expect_equal(nrow(lo),
               as.integer(manynet::net_nodes(manynet::ison_adolescents)))
  expect_true(all(is.finite(as.matrix(lo))))
  # Deterministic: the decomposition fixes the axes, and the layout fixes
  # their direction, so two calls draw the same picture.
  expect_equal(lo, layout_correspondence(manynet::ison_adolescents))
})

test_that("correspondence layout reads a direction where there is one", {
  skip_on_cran()
  net <- manynet::ison_networkers
  both <- layout_correspondence(net)
  out <- layout_correspondence(net, direction = "out")
  ins <- layout_correspondence(net, direction = "in")
  for (lo in list(both, out, ins)) {
    expect_equal(nrow(lo), as.integer(manynet::net_nodes(net)))
    expect_true(all(is.finite(as.matrix(lo))))
  }
  # Who a node sends to and who it receives from are different profiles.
  expect_false(isTRUE(all.equal(out$x, ins$x)))
  expect_false(isTRUE(all.equal(both$x, out$x)))
  expect_error(layout_correspondence(net, direction = "sideways"),
               "direction")
})

test_that("a signed network needs its signs split", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(
    p <- graphr(manynet::ison_monks, layout = "correspondence"),
    "unsigned network")
  expect_equal(p$plot_env$layout, "stress")
  lo <- layout_correspondence(manynet::ison_monks, double = TRUE)
  expect_equal(nrow(lo),
               as.integer(manynet::net_nodes(manynet::ison_monks)))
  expect_true(all(is.finite(as.matrix(lo))))
  p2 <- suppressMessages(
    graphr(manynet::ison_monks, layout = "correspondence", double = TRUE))
  expect_equal(p2$plot_env$layout, "correspondence")
  expect_buildable(p2)
})

test_that("correspondence layout draws axes naming their inertia", {
  skip_on_cran()
  p <- graphr(manynet::ison_southern_women, layout = "correspondence")
  expect_match(p$labels$x, "^Dimension 1 \\([0-9]+% of inertia\\)$")
  expect_match(p$labels$y, "^Dimension 2 \\([0-9]+% of inertia\\)$")
  # One scale for both axes, or the distances drawn cannot be compared.
  expect_equal(p$coordinates$ratio, 1)
  # Axes a void theme would have blanked.
  expect_false(inherits(p$theme$axis.text, "element_blank"))
  # The inertia is named on the axes, so the caption has nothing to add.
  expect_null(p$labels$caption)
})

test_that("nodes the plane holds poorly are named at the console", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(graphr(manynet::ison_southern_women,
                        layout = "correspondence"),
                 "far off the plane")
})

test_that("two dimensions no better than chance are reported", {
  skip_on_cran()
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  # The warning does not follow the raw share, which is the point of it.
  # ison_adolescents draws the larger share of the two (60% against 36%) but
  # has only seven dimensions to win it from, so two of them hold less than
  # breaking the inertia at random would have given them.
  expect_message(graphr(manynet::ison_adolescents, layout = "correspondence"),
                 "dividing it at random")
  expect_no_message(
    .note_corresp_inertia(
      attr(layout_correspondence(manynet::ison_networkers), "fit")))
})

test_that("the broken stick baseline is the share of a random division", {
  skip_on_cran()
  # The first two of k pieces of a stick broken at k - 1 random points, which
  # is a good deal more than the even share of 2/k.
  expect_equal(.broken_stick(7), mean(1/(1:7)) + mean(1/(2:7)))
  expect_gt(.broken_stick(7), 2 / 7)
  expect_gt(.broken_stick(31), 2 / 31)
  # Fewer dimensions leaves more for each of the two drawn.
  expect_gt(.broken_stick(5), .broken_stick(50))
})

test_that("a node with no ties is placed at the origin", {
  skip_on_cran()
  iso <- manynet::add_nodes(manynet::ison_adolescents, 1, list(name = "Zoe"))
  # graphr() sets isolates aside itself, so the layout only meets one where it
  # is called directly or where the isolates are kept.
  lo <- layout_correspondence(iso)
  expect_equal(unname(unlist(lo[9, ])), c(0, 0))
  expect_true(all(is.finite(as.matrix(lo))))
  expect_true(is.na(attr(lo, "fit")$cos2[["Zoe"]]))
  expect_buildable(suppressMessages(
    graphr(iso, layout = "correspondence", isolates = "keep")))
})

test_that("correspondence coordinates are kept when snapping is asked for", {
  skip_on_cran()
  snapped <- suppressMessages(
    graphr(manynet::ison_southern_women, layout = "correspondence",
           snap = TRUE))
  plain <- suppressMessages(
    graphr(manynet::ison_southern_women, layout = "correspondence"))
  expect_equal(snapped$data[, c("x", "y")], plain$data[, c("x", "y")])
})

test_that("correspondence layout answers a network too small to analyse", {
  skip_on_cran()
  empty <- manynet::create_empty(5)
  lo <- layout_correspondence(empty)
  expect_equal(nrow(lo), 5)
  expect_true(all(is.finite(as.matrix(lo))))
})
