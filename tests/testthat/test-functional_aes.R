# Functional audit of graphr()'s aesthetic arguments: every formal argument
# is enumerated and must have at least one registered value to exercise, so
# a newly added argument fails the audit until it is registered here. Each
# registered value is run on a fixture carrying node/tie attributes and the
# resulting plot must build.

# Fixture with numeric and categorical node attributes plus tie weights
aes_fixture <- local({
  set.seed(1234)
  net <- manynet::ison_adolescents
  net <- manynet::add_node_attribute(net, "grp", rep(c("A", "B"), times = 4))
  net <- manynet::add_node_attribute(net, "num", stats::runif(8, 1, 10))
  manynet::add_tie_attribute(net, "weight",
                             seq_len(manynet::net_ties(net)))
})

# Values to try for each graphr() argument; NULL means the argument is
# exercised elsewhere (layouts, snapping) or is just an alias.
graphr_arg_values <- list(
  .data       = NULL, # the fixture itself
  layout      = NULL, # audited exhaustively in test-functional_layouts.R
  labels      = list(TRUE, FALSE),
  node_color  = list("grp", "num", "darkred"),
  node_colour = NULL, # alias of node_color
  node_shape  = list("grp", "square"),
  node_size   = list("num", 3),
  node_group  = list("grp"),
  edge_color  = list("weight", "darkblue"),
  edge_colour = NULL, # alias of edge_color
  edge_size   = list("weight", 0.5),
  edge_bundle = list("force"),
  isolates    = list("legend", "caption", "keep"),
  snap        = NULL, # exercised in test-functional_layouts.R
  label_dist  = list(0.5),
  label_repel = list(TRUE, FALSE),
  ...         = NULL
)

test_that("every graphr() argument is audited with working values", {
  skip_on_cran()
  for (arg in names(formals(graphr))) {
    if (!arg %in% names(graphr_arg_values)) {
      fail(paste0("AUDIT [graphr(", arg, ")]: no values registered"))
      next
    }
    vals <- graphr_arg_values[[arg]]
    if (is.null(vals)) next
    for (val in vals) {
      args <- stats::setNames(list(aes_fixture, val), c(".data", arg))
      p <- run_or_skip(do.call(graphr, args), paste0("graphr ", arg),
                       deparse(val))
      run_or_skip(expect_buildable(p), paste0("build graphr ", arg),
                  deparse(val))
    }
  }
})

test_that("aesthetics also resolve on signed, weighted and two-mode data", {
  skip_on_cran()
  for (fix in c("signed", "weighted", "twomode", "multiplex", "directed")) {
    p <- run_or_skip(graphr(ag_fixtures[[fix]]), "graphr defaults", fix)
    run_or_skip(expect_buildable(p), "build defaults", fix)
  }
})

test_that("isolates are removed but noted by default", {
  skip_on_cran()
  net <- manynet::add_nodes(manynet::ison_adolescents, 2)
  p <- graphr(net, isolates = "legend")
  expect_buildable(p)
  expect_lt(nrow(p$data), as.numeric(manynet::net_nodes(net)))
  p2 <- graphr(net, isolates = "keep")
  expect_equal(nrow(p2$data), as.numeric(manynet::net_nodes(net)))
  # a network of only isolates keeps its nodes rather than emptying the plot
  empty <- manynet::create_empty(3)
  p3 <- graphr(empty)
  expect_equal(nrow(p3$data), 3L)
})

test_that("diffusion results colour nodes by state", {
  skip_on_cran()
  set.seed(123)
  diff <- manynet::play_diffusion(manynet::create_ring(8), seeds = 1)
  p <- graphr(diff)
  expect_buildable(p)
})
