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
  # an unknown theme warns and leaves the current theme in place
  suppressMessages(suppressWarnings(stocnet_theme("notatheme")))
  expect_equal(getOption("stocnet_theme"), "uzh")
  # querying without arguments reports the current theme
  expect_no_error(suppressMessages(stocnet_theme()))
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
