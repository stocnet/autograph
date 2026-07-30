# Edge-case and error-path audit for graphr(). Deliberately lean: these run
# against one or two small fixtures rather than the full ag_fixtures grid,
# because the point is to pin *behaviour on bad input*, not to re-cover formats.
#
# Where autograph currently handles a case gracefully, that is asserted as the
# contract. Where it does not, the current behaviour is pinned with a KNOWN GAP
# comment saying what it should do instead, so the gap is visible and the test
# will fail loudly when someone fixes it (which is the moment to tighten it).

test_that("graphr() rejects input it cannot coerce to a network", {
  # KNOWN GAP: the error comes from manynet's as_tidygraph() dispatch rather
  # than from a validation check in graphr(), so the message names an internal
  # generic instead of the offending argument. A friendlier check on .data
  # would be an improvement.
  expect_error(graphr(NULL), "as_tidygraph")
  expect_error(graphr("not a network"), "as_tidygraph")
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

test_that("graphr() tolerates a nonexistent node attribute name", {
  net <- manynet::ison_adolescents
  # A colour attribute that does not exist is ignored rather than fatal, so a
  # typo degrades to the default styling instead of breaking a script.
  expect_buildable(graphr(net, node_color = "nosuchattribute"))
})

test_that("graphr() warns rather than errors on unusable attribute types", {
  net <- manynet::ison_adolescents
  # KNOWN GAP: both of these leak a base-R warning ("no non-missing arguments
  # to max"; "NAs introduced by coercion") from deep inside the sizing code.
  # They should instead either warn naming the offending argument, or error
  # cleanly. Pinned so the leak is visible.
  # capture_warnings() rather than expect_warning(): these paths emit more than
  # one warning, and the surplus would otherwise bubble up as test noise.
  expect_match(capture_warnings(graphr(net, node_size = "nosuchattribute")),
               "no non-missing arguments to max", all = FALSE)
  chr <- manynet::add_node_attribute(net, "grp", rep(c("a", "b"), 4))
  expect_match(capture_warnings(graphr(chr, node_size = "grp")),
               "NAs introduced by coercion", all = FALSE)
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
  # KNOWN GAP: the message is the raw "object 'layout_tbl_graph_notalayout'
  # not found" rather than something naming the `layout` argument and listing
  # the valid options, as edge_bundle does.
  expect_error(graphr(manynet::ison_adolescents, layout = "notalayout"),
               "layout_tbl_graph_notalayout")
})
