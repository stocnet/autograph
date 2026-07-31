# Edge-case and error-path audit for graphr(). Deliberately lean: these run
# against one or two small fixtures rather than the full ag_fixtures grid,
# because the point is to pin *behaviour on bad input*, not to re-cover formats.
#
# The contract these assert is that a plausible beginner mistake fails here,
# with a message naming the argument and suggesting what was probably meant,
# rather than falling through to ggplot2, grid or igraph. The validation lives
# in R/graph_checks.R.
#
# Where autograph still does not handle a case gracefully, the current
# behaviour is pinned with a KNOWN GAP comment saying what it should do
# instead, so the gap is visible and the test will fail loudly when someone
# fixes it (which is the moment to tighten it).

test_that("graphr() rejects input it cannot coerce to a network", {
  # The message names the argument and the class given, rather than the
  # internal generic that failed to dispatch.
  expect_error(graphr(NULL), "should be a network")
  expect_error(graphr("not a network"), "\\.data")
  expect_error(graphr(42), "numeric")
})

test_that("graphr() handles degenerate node counts", {
  # Empty and two-node networks lay out and build fine.
  expect_buildable(graphr(manynet::create_empty(0)))
  expect_buildable(graphr(manynet::create_empty(2)))

  # KNOWN GAP: a single-node network errors with "invalid indexing" from inside
  # the layout code -- note that both 0 and 2 nodes work, so this is an
  # off-by-one in the layout path rather than an unsupported case. It should
  # draw one isolate. Pinned here so the crash is documented and regression-
  # tested; tighten to expect_buildable() once fixed.
  expect_error(graphr(manynet::create_empty(1)), "invalid indexing")
})

test_that("graphr() rejects a nonexistent node attribute name", {
  net <- manynet::ison_adolescents
  # Previously the typo was passed through and surfaced as "Unknown colour
  # name: nosuchattribute" from grid, at draw time.
  expect_error(graphr(net, node_color = "nosuchattribute"), "node_color")
  expect_error(graphr(net, node_color = "nosuchattribute"), "node attribute")
  # A literal colour, by name or hex code, is still accepted.
  expect_buildable(graphr(net, node_color = "red"))
  expect_buildable(graphr(net, node_color = "#4575b4"))
})

test_that("graphr() suggests the attribute the user probably meant", {
  net <- manynet::add_node_attribute(manynet::ison_adolescents,
                                     "wealth", seq_len(8))
  expect_error(graphr(net, node_color = "wealthh"), "Did you mean")
  expect_error(graphr(net, node_color = "wealthh"), "wealth")
})

test_that("graphr() reads through a capitalisation slip", {
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  net <- manynet::add_node_attribute(manynet::ison_adolescents,
                                     "wealth", seq_len(8))
  expect_message(p <- graphr(net, node_color = "Wealth"), "Interpreting")
  expect_buildable(p)
})

test_that("graphr() rejects unusable node_size and edge_size values", {
  net <- manynet::ison_adolescents
  # Previously these leaked base-R warnings ("no non-missing arguments to max";
  # "NAs introduced by coercion") from deep inside the sizing code.
  expect_error(graphr(net, node_size = "nosuchattribute"), "node_size")
  expect_error(graphr(net, edge_size = "nosuchattribute"), "edge_size")
  # A vector of the wrong length names both lengths rather than reporting
  # ggplot2's internal data frame size.
  expect_error(graphr(net, node_size = c(1, 2, 3)), "8 nodes")
})

test_that("graphr() rejects an unusable node_shape", {
  net <- manynet::ison_adolescents
  expect_error(graphr(net, node_shape = "shappe"), "node_shape")
  expect_buildable(graphr(net, node_shape = "square"))
  expect_buildable(graphr(net, node_shape = 21))
})

test_that("graphr() takes a single node_size at face value", {
  net <- manynet::ison_adolescents
  # `length(node_size > 1)` used to be truthy for any value, so a size in
  # [0, 1] was silently multiplied by ten.
  expect_equal(.infer_nsize(manynet::as_tidygraph(net), 0.5), 0.5)
  # A vector of proportions is still rescaled to be visible.
  expect_equal(.infer_nsize(manynet::as_tidygraph(net), rep(0.5, 8)),
               rep(5, 8))
})

test_that("graphr() accepts out-of-range numeric aesthetics without erroring", {
  net <- manynet::ison_adolescents
  # Negative sizes are nonsensical but must not crash the layout: ggplot2
  # clamps them at draw time. Asserted so a future validation change is a
  # deliberate decision rather than an accident.
  expect_buildable(graphr(net, node_size = -1))
  expect_buildable(graphr(net, edge_size = -1))
})

test_that("graphr() rejects an unknown layout by name", {
  net <- manynet::ison_adolescents
  expect_error(graphr(net, layout = "notalayout"), "layout")
  # The message suggests the intended layout where there is a near match, and
  # names autograph's own layouts.
  expect_error(graphr(net, layout = "stresss"), "Did you mean")
  expect_error(graphr(net, layout = "notalayout"), "concentric")
})

test_that("graphr() explains that a layout is named, not passed as a function", {
  expect_error(graphr(manynet::ison_adolescents, layout = igraph::layout_with_fr),
               "rather than a layout function")
})

test_that("graphr() validates isolates whether or not there are isolates", {
  # `match.arg()` used to sit inside .infer_isolates(), which does not always
  # force its argument, so this was caught or ignored depending on the network.
  with_isolates <- manynet::create_empty(4) %>%
    manynet::add_ties(c(1, 2))
  expect_error(graphr(manynet::ison_adolescents, isolates = "drop"), "isolates")
  expect_error(graphr(with_isolates, isolates = "drop"), "isolates")
})

test_that("graphr() passes a list of networks on to graphs()", {
  # The aesthetic arguments have no defaults, so naming them when forwarding
  # forced promises that were still missing, giving
  # "argument \"node_color\" is missing, with no default".
  expect_no_error(graphr(manynet::to_egos(manynet::ison_adolescents)))
  expect_s3_class(graphr(manynet::to_egos(manynet::ison_adolescents)),
                  "patchwork")
})

test_that("graphs() rejects waves outside the range available", {
  egos <- manynet::to_egos(manynet::ison_adolescents)
  expect_error(graphs(egos, waves = 99), "between 1 and 8")
  expect_error(graphs(egos, waves = "first"), "waves")
})

test_that("layouts that need an extra argument say how to give it", {
  net <- manynet::ison_adolescents
  # Previously "argument \"rank\" is missing, with no default".
  expect_error(graphr(net, layout = "lineage"), "rank")
  expect_error(graphr(net, layout = "lineage"), "for each node")
  expect_error(graphr(net, layout = "concentric"), "membership")
})
