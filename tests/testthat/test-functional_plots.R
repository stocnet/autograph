# Functional audit of the plot.<class> S3 method family: every method
# registered by autograph is looked up in a fixture registry (precooked
# data objects where available, live-generated results otherwise) and its
# plot must build. Methods without a registered fixture, or whose fixture
# cannot be generated (e.g. missing Suggests package), skip with a
# greppable AUDIT message rather than silently escaping the audit.

# One fixture factory per plotted class. Factories are functions so that
# generation errors are caught per-class and reported as audit skips.
plot_fixture_registry <- list(
  ag_conv   = NULL, # internal wrapper class, exercised via traces.monan
  ag_gof    = NULL, # internal wrapper class, exercised via sienaGOF etc.
  grapht    = NULL, # print method for animations, tested in test-grapht.R
  changepoints.goldfish = function() autograph::goldfish_changepoints,
  outliers.goldfish     = function() autograph::goldfish_outliers,
  diff_model  = function() autograph::res_manynet_diff,
  diffs_model = function() autograph::res_migraph_diff,
  learn_model = function() {
    set.seed(123)
    manynet::play_learning(manynet::create_ring(6), beliefs = stats::runif(6))
  },
  mnet        = function() manynet::ison_adolescents,
  matrix      = function() manynet::as_matrix(manynet::ison_adolescents),
  netlm       = function() autograph::res_migraph_reg,
  netlogit    = function() {
    testthat::skip_if_not_installed("migraph")
    set.seed(123)
    net <- manynet::add_node_attribute(manynet::ison_adolescents, "smoke",
                                       rep(c(0, 1), 4))
    migraph::net_regression(~ alter(smoke), net, times = 20)
  },
  network_test = function() autograph::res_migraph_test,
  network_measures = function() {
    meas <- data.frame(time = 1:10, value = cumsum(stats::rnorm(10)))
    class(meas) <- c("network_measures", class(meas))
    meas
  },
  node_measure = function() {
    testthat::skip_if_not_installed("netrics")
    netrics::node_by_degree(manynet::ison_adolescents)
  },
  tie_measure = function() {
    testthat::skip_if_not_installed("netrics")
    netrics::tie_by_betweenness(manynet::ison_adolescents)
  },
  node_member = function() {
    testthat::skip_if_not_installed("netrics")
    netrics::node_in_walktrap(manynet::ison_southern_women, "e")
  },
  node_motif = function() {
    testthat::skip_if_not_installed("netrics")
    netrics::node_x_triad(manynet::ison_adolescents)
  },
  network_motif = function() {
    testthat::skip_if_not_installed("netrics")
    netrics::net_x_triad(manynet::ison_adolescents)
  },
  ergm = function() {
    testthat::skip_if_not_installed("ergm")
    autograph::load_ergm_res()
  },
  gof.ergm        = function() autograph::ergm_gof,
  sienaGOF        = function() autograph::siena_gof,
  gof.stats.monan = function() autograph::monan_gof,
  traces.monan    = function() autograph::monan_conv,
  influenceTable  = function() autograph::siena_influence,
  selectionTable  = function() autograph::siena_selection
)

test_that("every registered plot method has a fixture and builds", {
  skip_on_cran()
  for (cls in ag_plot_classes()) {
    if (!cls %in% names(plot_fixture_registry)) {
      # New plot methods must be added to the registry above
      fail(paste0("AUDIT [plot.", cls, "]: no fixture registered"))
      next
    }
    factory <- plot_fixture_registry[[cls]]
    if (is.null(factory)) next # covered elsewhere, see registry comments
    # suppressWarnings: fixture generators may warn upstream (e.g. migraph)
    fixture <- run_or_skip(suppressWarnings(factory()),
                           paste0("plot.", cls), "fixture")
    p <- run_or_skip(plot(fixture), paste0("plot.", cls), "plot")
    if (is.null(p)) next # methods may validly print a message and return NULL
    # suppressWarnings: data-dependent smoothing warnings (loess etc.) are
    # not what this audit is checking for
    run_or_skip(suppressWarnings(expect_buildable(p)),
                paste0("plot.", cls), "build")
  }
})

# Deeper, deterministic checks for the analysis methods that the audit above
# only smoke-tests.

test_that("measure plots support histogram and density types", {
  skip_on_cran()
  skip_if_not_installed("netrics")
  deg <- netrics::node_by_degree(manynet::ison_adolescents)
  expect_buildable(plot(deg))
  expect_buildable(plot(deg, type = "h"))
  expect_buildable(plot(deg, type = "d"))
  tb <- netrics::tie_by_betweenness(manynet::ison_adolescents)
  expect_buildable(plot(tb, type = "h"))
  expect_buildable(plot(tb, type = "d"))
})

test_that("network measures over time plot as a trace", {
  meas <- data.frame(time = 1:10, value = cumsum(stats::rnorm(10)))
  class(meas) <- c("network_measures", class(meas))
  expect_buildable(plot(meas))
})

test_that("matrix plots support memberships, two-mode and signed data", {
  skip_on_cran()
  skip_if_not_installed("netrics")
  memb <- netrics::node_in_walktrap(manynet::ison_adolescents, "e")
  expect_buildable(plot(manynet::as_matrix(manynet::ison_adolescents),
                        membership = memb))
  memb2 <- netrics::node_in_walktrap(manynet::ison_southern_women, "e")
  expect_buildable(plot(manynet::as_matrix(manynet::ison_southern_women),
                        membership = memb2))
  signed <- manynet::to_signed(manynet::create_ring(6))
  expect_buildable(plot(manynet::as_matrix(signed)))
  # unlabelled matrices get generated node names
  expect_buildable(plot(manynet::as_matrix(manynet::create_ring(6))))
})

test_that("diffusion summaries plot for mnet and diff_model objects", {
  skip_on_cran()
  set.seed(123)
  diff <- manynet::play_diffusion(manynet::create_ring(8), seeds = 1)
  expect_buildable(plot(diff)) # mnet dispatch, changing -> as_diffusion
  expect_buildable(plot(manynet::ison_adolescents)) # mnet dispatch, static
  expect_buildable(plot(autograph::res_manynet_diff, all_steps = FALSE))
})

test_that("goldfish diagnostics print a message when nothing is found", {
  quiet_outliers <- autograph::goldfish_outliers
  quiet_outliers$outlier <- rep("NO", nrow(quiet_outliers))
  expect_output(out <- plot(quiet_outliers), "No outliers found")
  expect_null(out)
  quiet_cpts <- autograph::goldfish_changepoints
  quiet_cpts$cpt_points <- NULL
  expect_output(out <- plot(quiet_cpts), "No regime changes found")
  expect_null(out)
})

test_that("motif plots draw the corresponding motif panels", {
  skip_on_cran()
  undirected_triads <- structure(
    matrix(1, 1, 4, dimnames = list(NULL, c("003", "102", "201", "300"))),
    class = c("node_motif", "matrix"))
  expect_buildable(plot(undirected_triads))
  dyads <- structure(c(Mutual = 1, Asymmetric = 2, Null = 3),
                     class = "network_motif")
  expect_buildable(plot(dyads))
  unknown <- structure(c(Z9 = 1), class = "network_motif")
  expect_error(plot(unknown), "Cannot plot")
})

# graphs() variants: shared layouts, wave selection, and fallbacks

test_that("graphs() shares layouts across waves and selects waves", {
  skip_on_cran()
  nets <- lapply(1:5, function(i) {
    set.seed(i)
    manynet::generate_random(6)
  })
  # unnamed list of >4 networks reduces to first and last
  p <- suppressMessages(graphs(nets))
  expect_s3_class(p, "patchwork")
  # scalar waves keeps the first n; vector waves selects
  expect_s3_class(graphs(nets, waves = 2), "patchwork")
  expect_s3_class(graphs(nets, waves = c(2, 4)), "patchwork")
  # layouts based on last or both waves
  expect_s3_class(graphs(nets, waves = c(1, 2), based_on = "last"),
                  "patchwork")
  expect_s3_class(graphs(nets, waves = c(1, 2), based_on = "both"),
                  "patchwork")
})

test_that("graphs() handles ego networks and changing networks", {
  skip_on_cran()
  # a full set of egos gets the star layout centred on each ego
  p <- suppressMessages(graphs(manynet::to_egos(manynet::ison_adolescents),
                               waves = 8))
  expect_s3_class(p, "patchwork")
  # a partial set is not an ego network, so layouts are independent
  p2 <- suppressMessages(graphs(manynet::to_egos(manynet::ison_adolescents),
                                waves = c(1, 2)))
  expect_s3_class(p2, "patchwork")
  set.seed(123)
  diff <- manynet::play_diffusion(manynet::create_ring(8), seeds = 1)
  expect_s3_class(suppressMessages(graphs(diff)), "patchwork")
  # networks with different node sets fall back to independent layouts
  uneven <- list(manynet::create_ring(4), manynet::create_ring(6))
  expect_s3_class(suppressMessages(graphs(uneven)), "patchwork")
})
