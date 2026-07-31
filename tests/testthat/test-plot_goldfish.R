# The goldfish diagnostic classes. These objects arrive plot-ready: each is a
# tibble carrying the metadata contract, so every method here reads its series
# and its labels off the object rather than reshaping it.

test_that("outliers plotting works", {
  p <- plot(goldfish_outliers)
  expect_s3_class(p, "ggplot")
  # The series is the one the diagnostic analysed, and the flag is logical.
  expect_type(goldfish_outliers$outlier, "logical")
  expect_true(".series" %in% names(goldfish_outliers))
  expect_true(any(goldfish_outliers$outlier))
})

test_that("an object with nothing flagged says so instead of plotting", {
  quiet <- goldfish_outliers
  quiet$outlier <- FALSE
  expect_output(p <- plot(quiet), "No outliers found")
  expect_null(p)
})

test_that("changepoints plotting works", {
  p <- plot(goldfish_changepoints)
  expect_s3_class(p, "ggplot")
  expect_type(goldfish_changepoints$cpt, "logical")
  expect_true(any(goldfish_changepoints$cpt))
})

test_that("margin table plotting works", {
  p <- plot(goldfish_margins)
  expect_s3_class(p, "ggplot")
})

test_that("the methods read the metadata contract, not the columns", {
  for (object in list(
    goldfish_outliers,
    goldfish_changepoints,
    goldfish_margins
  )) {
    expect_s3_class(object, "tbl_df")
    expect_type(attr(object, "diagnostic"), "character")
    expect_type(attr(object, "context"), "list")
    expect_type(attr(object, "params"), "list")
    # The producing goldfish version, so a precooked fixture can be seen to
    # have aged.
    expect_type(attr(object, "version"), "character")
  }
  expect_identical(attr(goldfish_outliers, "diagnostic"), "diagnose_outliers")
  expect_identical(
    attr(goldfish_changepoints, "diagnostic"),
    "diagnose_changepoints"
  )
  expect_identical(attr(goldfish_margins, "diagnostic"), "margin_table")
})

test_that("the plots render without goldfish attached", {
  # Dispatch is on class alone, and nothing here calls back into goldfish.
  expect_false("package:goldfish" %in% search())
  expect_s3_class(plot(goldfish_outliers), "ggplot")
  expect_s3_class(plot(goldfish_changepoints), "ggplot")
  expect_s3_class(plot(goldfish_margins), "ggplot")
})

test_that("the margin plot caps the actors it draws, and says how many", {
  full <- plot(goldfish_margins, top = Inf)
  capped <- plot(goldfish_margins)
  expect_gt(nrow(full$data), nrow(capped$data))
  # Nothing is dropped silently.
  expect_match(capped$labels$subtitle, "further actors not shown")
  expect_null(full$labels$subtitle)
})

# The two test classes. Both are classed LISTS of tibbles rather than single
# tibbles -- one rectangle does not hold a per-effect table and a per-interval
# series -- so the methods take their series from a named component.

test_that("gof process plotting works", {
  p <- plot(goldfish_gof)
  expect_s3_class(p, "ggplot")
  # One panel per tested effect, from the object's own process table.
  expect_identical(
    length(unique(goldfish_gof$process$term)),
    nrow(goldfish_gof$effects)
  )
})

test_that("the gof x axis is the object's clock, not a re-derived one", {
  p <- plot(goldfish_gof)
  # The bands are valid on whichever clock produced the process, so the axis
  # has to be the `u` column the object carries. Reading an event index here
  # would draw the path on one clock and the reference on another.
  expect_identical(rlang::quo_get_expr(p$mapping$x), quote(.data$u))
  expect_identical(p$labels$x, "Share of events")

  # And the label follows the clock rather than being fixed.
  information <- goldfish_gof
  attr(information, "params")$clock <- "information"
  expect_identical(
    plot(information)$labels$x,
    "Cumulative share of information"
  )
})

test_that("the gof reference band inverts the same distribution as the test", {
  # The band is the two-sided Kolmogorov quantile, which is what the
  # event-clock p-value comes from -- so a path touching the band must sit at
  # the plotted level, or band and p-value disagree.
  q <- gf_bridge_quantile(0.95)
  j <- seq_len(100)
  cdf <- 1 - 2 * sum((-1)^(j - 1) * exp(-2 * j^2 * q^2))
  expect_equal(cdf, 0.95, tolerance = 1e-8)
  expect_gt(gf_bridge_quantile(0.99), q)
})

test_that("time residual plotting works", {
  p <- plot(goldfish_time)
  expect_s3_class(p, "ggplot")
  expect_identical(
    length(unique(goldfish_time$residuals$term)),
    nrow(goldfish_time$effects)
  )
  expect_match(p$labels$subtitle, "time trend")
})

test_that("the trend method draws no period legend", {
  # `period` is all-NA under the trend method; colouring by a constant would
  # put a one-level legend on every trend plot.
  expect_true(all(is.na(goldfish_time$residuals$period)))
  p <- plot(goldfish_time)
  expect_false(any(vapply(
    p$layers,
    function(l) "colour" %in% names(l$mapping),
    logical(1)
  )))

  # With periods present the scatter is coloured by them instead.
  periods <- goldfish_time
  periods$residuals$period <- rep(
    c("early", "late"),
    length.out = nrow(periods$residuals)
  )
  attr(periods, "params")$method <- "periods"
  q <- plot(periods)
  expect_true(any(vapply(
    q$layers,
    function(l) "colour" %in% names(l$mapping),
    logical(1)
  )))
  expect_match(q$labels$subtitle, "across periods")
})

test_that("the test objects carry the metadata contract", {
  for (object in list(goldfish_gof, goldfish_time)) {
    expect_type(object, "list")
    expect_type(attr(object, "diagnostic"), "character")
    expect_type(attr(object, "context"), "list")
    expect_type(attr(object, "params"), "list")
    expect_type(attr(object, "version"), "character")
  }
  expect_identical(attr(goldfish_gof, "diagnostic"), "test_gof")
  expect_identical(attr(goldfish_time, "diagnostic"), "test_time")
})

test_that("the test plots render without goldfish attached", {
  expect_false("package:goldfish" %in% search())
  expect_s3_class(plot(goldfish_gof), "ggplot")
  expect_s3_class(plot(goldfish_time), "ggplot")
})

# The onset class. Two panels composed with patchwork, both windowed on the
# excursion rather than the sequence -- the geometry is the substance here, so
# it is what the tests pin.

test_that("onset plotting composes two panels", {
  p <- plot(goldfish_onset)
  expect_s3_class(p, "patchwork")
  # And each panel is available alone, the escape hatch for a model with too
  # many coefficients for a composed figure.
  expect_s3_class(plot(goldfish_onset, view = "path"), "ggplot")
  expect_s3_class(plot(goldfish_onset, view = "accrual"), "ggplot")
  expect_error(plot(goldfish_onset, view = "nonesuch"))
})

test_that("the path panel is windowed on each coefficient's own excursion", {
  drawn <- plot(goldfish_onset, view = "path")$data
  summary <- as.data.frame(goldfish_onset$summary)
  n_events <- attr(goldfish_onset, "context")$n_events

  # Full range is mostly bridge tail: the path returns to the estimate by
  # construction, so drawing all of it squashes what is being read.
  expect_lt(max(drawn$dropped_events), n_events)

  for (i in seq_len(nrow(summary))) {
    at <- summary$stabilized_at[i]
    window <- max(drawn$dropped_events[drawn$term == summary$term[i]])
    expected <- if (at == 0) {
      n_events
    } else {
      min(n_events, max(ceiling(1.15 * at), 10))
    }
    # `expect_equal`, not identical: the window comes off an integer column
    # and the formula returns a double from `ceiling()`.
    expect_equal(window, expected)
    # The window has to reach past the point it marks, or the marker falls
    # outside the panel it belongs to.
    if (at > 0) expect_gte(window, at)
  }
})

test_that("the path facets carry free scales, not one shared window", {
  # A window shared across facets re-creates the squashing the per-coefficient
  # window exists to prevent.
  p <- plot(goldfish_onset, view = "path")
  expect_true(p$facet$params$free$x)
  expect_true(p$facet$params$free$y)
})

test_that("the accrual panel is full range with the diagonal drawn", {
  accrual <- plot(goldfish_onset, view = "accrual")
  n_events <- attr(goldfish_onset, "context")$n_events
  # Full range, unlike the path panel: the window is shaded, not cut to.
  expect_identical(max(accrual$data$dropped_events), n_events)
  # Without the proportional diagonal a monotone 0-to-1 curve says nothing --
  # the departure from it is the finding.
  slopes <- vapply(
    accrual$layers,
    function(l) {
      if (is.null(l$data$slope)) NA_real_ else l$data$slope[[1]]
    },
    numeric(1)
  )
  expect_true(any(!is.na(slopes) & abs(slopes - 1 / n_events) < 1e-12))
})

test_that("a fixed coefficient is not drawn", {
  # An offset is a flat line at its imposed value by construction.
  held <- goldfish_onset
  held$summary$fixed[1] <- TRUE
  drawn <- plot(held, view = "path")$data
  expect_false(goldfish_onset$summary$term[1] %in% drawn$term)

  # And with every coefficient held there is nothing to trace.
  all_held <- goldfish_onset
  all_held$summary$fixed <- TRUE
  expect_output(p <- plot(all_held), "No estimated coefficient")
  expect_null(p)
})

test_that("the onset plot renders without goldfish attached", {
  expect_false("package:goldfish" %in% search())
  expect_s3_class(plot(goldfish_onset), "patchwork")
})
