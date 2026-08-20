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

test_that("font detection sees system fonts and falls back cleanly", {
  on.exit(suppressMessages(stocnet_theme("default")), add = TRUE)
  # list_fonts() must report more than the handful of device aliases that
  # grDevices lists, otherwise a font a user installs for a theme can never
  # be matched.
  fonts <- list_fonts()
  expect_type(fonts, "character")
  expect_true(length(fonts) > 0)
  expect_false(anyDuplicated(fonts) > 0)
  expect_true(all(list_fonts("sans") %in% fonts))
  # Themes that name no font get the sans-serif fallback without complaint.
  suppressMessages(stocnet_theme("default"))
  expect_equal(ag_font(), "sans")
  # A theme that names fonts either matches one of them or falls back.
  suppressMessages(stocnet_theme("clay"))
  expect_true(ag_font() %in% c(autograph:::theme_fonts("clay"), "sans"))
})

test_that("palettes separate colours for colour-blind viewers", {
  on.exit(suppressMessages(stocnet_theme("default")), add = TRUE)
  # Simulation is anchored on a pair that normal vision separates easily and
  # red-green colour blindness does not.
  expect_gt(contrast_colors(c("#B7352D", "#4575b4"))[1, 2], 40)
  expect_lt(contrast_colors(c("#B7352D", "#627313"))[1, 2], 10)
  expect_length(simulate_colorblind(c("#d73027", "#4575b4"), "deutan"), 2)
  expect_error(simulate_colorblind("#d73027", "quadran"))

  for (thm in autograph:::theme_opts) {
    suppressMessages(stocnet_theme(thm))
    # Each stored palette is already in the order colorblind_sort() would choose, so
    # that a palette added later cannot ship in an order that hides colours
    # from one another. The exception is a palette whose own order is the
    # point, which is sampled across its length instead.
    pal <- getOption("snet_cat")
    if (thm %in% autograph:::colorblind_unsorted) {
      expect_true(getOption("snet_cat_spread"), info = thm)
    } else {
      expect_false(getOption("snet_cat_spread"), info = thm)
      expect_identical(autograph:::colorblind_sort(pal, getOption("snet_background")),
                       pal, info = thm)
    }
    # The colours a plot of two to four categories gets must be separable by
    # every viewer, not only by those with unaffected colour vision.
    for (k in 2:4) {
      if (k > length(pal)) next
      cols <- ag_qualitative(k)
      expect_gt(min(contrast_colors(cols)[upper.tri(diag(k))]), 10)
    }
    # Divergent poles must not be a red-green pair, and the two highlights
    # must not be a pair only some viewers can tell apart.
    dv <- getOption("snet_div")
    expect_gt(contrast_colors(dv[c(1, length(dv))])[1, 2], 40, label = thm)
    hl <- getOption("snet_highlight")
    expect_gt(contrast_colors(hl)[1, 2], 20, label = thm)
    # The ink must stay legible on the theme's own ground, whether that
    # ground is white, ivory, or near-black.
    expect_gt(contrast_colors(c(ag_ink(), getOption("snet_background")))[1, 2],
              50, label = thm)
  }
})

test_that("a theme's ground reaches every plot, not only the graphs", {
  on.exit(suppressMessages(stocnet_theme("default")), add = TRUE)
  suppressMessages(stocnet_theme("neon"))
  # A dark theme used to ground graphr() alone, so every other plot drew the
  # theme's bright colours on white.
  p <- plot(netrics::node_by_degree(manynet::ison_adolescents))
  expect_equal(p$theme$plot.background$fill, "#070f23")
  expect_equal(p$theme$text$colour, ag_ink())
  g <- graphr(manynet::ison_adolescents)
  expect_equal(g$theme$panel.background$fill, "#070f23")
  # Grounding a graph must not put back what theme_void() blanked: colouring
  # the axis text drew coordinates and ticks onto graphs that have no use for
  # them, and that the white-backed themes never showed.
  expect_s3_class(g$theme$axis.text, "element_blank")
  # Ties take the colour the theme writes with, so that a dark ground does
  # not swallow them.
  expect_equal(autograph:::.infer_ecolor(manynet::as_igraph(manynet::ison_adolescents),
                                         NULL), ag_ink())
  # A white-backed theme is left exactly as ggplot2 draws it.
  suppressMessages(stocnet_theme("default"))
  p <- plot(netrics::node_by_degree(manynet::ison_adolescents))
  expect_null(p$theme$plot.background$fill)
})
