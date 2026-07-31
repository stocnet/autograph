# Functional audit of the theming family: every registered theme is set and
# every ag_* palette accessor must then return usable values, so that a new
# theme or accessor is audited automatically.

test_that("every theme yields valid colours from every palette accessor", {
  on.exit(suppressMessages(stocnet_theme("default")), add = TRUE)
  is_colour <- function(x) {
    vapply(x, function(cl) {
      tryCatch(is.matrix(grDevices::col2rgb(cl)), error = function(e) FALSE)
    }, logical(1))
  }
  for (thm in autograph:::theme_opts) {
    expect_no_error(suppressMessages(stocnet_theme(thm)))
    expect_true(all(is_colour(ag_base())), info = thm)
    expect_true(all(is_colour(ag_highlight())), info = thm)
    expect_true(all(is_colour(ag_positive())), info = thm)
    expect_true(all(is_colour(ag_negative())), info = thm)
    for (n in c(1, 3, 7)) {
      expect_length(ag_qualitative(n), n)
      expect_true(all(is_colour(ag_qualitative(n))), info = thm)
      expect_true(all(is_colour(ag_sequential(n))), info = thm)
      expect_true(all(is_colour(ag_divergent(n))), info = thm)
    }
    expect_type(ag_font(), "character")
  }
})

test_that("theme setting is case-insensitive and rejects unknown themes", {
  on.exit(suppressMessages(stocnet_theme("default")), add = TRUE)
  expect_no_error(suppressMessages(stocnet_theme("UZH")))
  expect_equal(getOption("stocnet_theme"), "uzh")
  # an unknown theme errors, suggesting the nearest available theme, and
  # leaves the current theme in place
  expect_error(stocnet_theme("notatheme"), "themes available")
  expect_error(stocnet_theme("uzhh"), "Did you mean")
  expect_error(stocnet_theme(c("uzh", "ethz")), "a single theme")
  expect_equal(getOption("stocnet_theme"), "uzh")
  # querying without arguments reports the current theme
  expect_no_error(suppressMessages(stocnet_theme()))
})

test_that("palette accessors cope with n beyond the palette length", {
  on.exit(suppressMessages(stocnet_theme("default")), add = TRUE)
  is_colour <- function(x) {
    vapply(x, function(cl) {
      tryCatch(is.matrix(grDevices::col2rgb(cl)), error = function(e) FALSE)
    }, logical(1))
  }
  for (thm in autograph:::theme_opts) {
    suppressMessages(stocnet_theme(thm))
    # More categories than the palette has colours must still return n usable
    # colours (by recycling or interpolating), not NA or a short vector.
    big <- suppressWarnings(ag_qualitative(40))
    expect_length(big, 40)
    expect_true(all(is_colour(big)), info = thm)
    # Degenerate n
    expect_length(suppressWarnings(ag_sequential(1)), 1)
    expect_length(suppressWarnings(ag_divergent(2)), 2)
  }
})

test_that("set_stocnet_theme is an alias for stocnet_theme", {
  on.exit(suppressMessages(stocnet_theme("default")), add = TRUE)
  suppressMessages(set_stocnet_theme("ethz"))
  expect_equal(getOption("stocnet_theme"), "ethz")
  expect_identical(ag_base(), {
    suppressMessages(stocnet_theme("ethz")); ag_base()
  })
})

test_that("palette accessors work before any theme has been set", {
  # tests/testthat.R calls stocnet_theme("default") before the suite runs, so
  # every other test here sees the snet_* options already populated. A user
  # who just calls library(autograph) does not. Clear the options to reproduce
  # that fresh session: ag_divergent() used to error with "invalid color name
  # 'default'" because its fallback was the literal string "default".
  snet_opts <- grep("^snet_", names(options()), value = TRUE)
  old <- options()[snet_opts]
  on.exit({
    options(old)
    suppressMessages(stocnet_theme("default"))
  }, add = TRUE)
  options(stats::setNames(vector("list", length(snet_opts)), snet_opts))

  is_colour <- function(x) {
    vapply(x, function(cl) {
      tryCatch(is.matrix(grDevices::col2rgb(cl)), error = function(e) FALSE)
    }, logical(1))
  }
  expect_true(all(is_colour(ag_base())))
  expect_true(all(is_colour(ag_highlight())))
  expect_true(all(is_colour(ag_positive())))
  expect_true(all(is_colour(ag_negative())))
  expect_type(ag_font(), "character")
  for (n in c(1, 3, 5)) {
    expect_true(all(is_colour(ag_qualitative(n))), info = paste("qualitative", n))
    expect_true(all(is_colour(ag_sequential(n))), info = paste("sequential", n))
    expect_true(all(is_colour(ag_divergent(n))), info = paste("divergent", n))
  }
})

test_that("colour utilities behave", {
  expect_true(is_dark("#000000"))
  expect_false(is_dark("#FFFFFF"))
  expect_type(match_color(c("#d73027", "#4575b4")), "character")
})

test_that("two-colour divergent palettes are interpolated through white", {
  on.exit({
    options(snet_div = NULL)
    suppressMessages(stocnet_theme("default"))
  }, add = TRUE)
  options(snet_div = c("#00204D", "#FFEA46"))
  cols <- ag_divergent(3)
  expect_length(cols, 3)
  expect_equal(toupper(cols[2]), "#FFFFFF")
})

test_that("dark backgrounds are applied to plots under the neon theme", {
  on.exit(suppressMessages(stocnet_theme("default")), add = TRUE)
  suppressMessages(stocnet_theme("neon"))
  p <- graphr(manynet::ison_adolescents)
  expect_s3_class(p, "ggplot")
  bg <- p$theme$panel.background$fill
  expect_equal(bg, "#070f23")
})
