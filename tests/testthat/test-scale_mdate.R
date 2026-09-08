# The mdate scales resolve each messy date to one date and draw a date axis,
# instead of the discrete character axis that {ggplot2} draws by default.
# {messydates} is only suggested, so every test skips without it.

make_mdates <- function() {
  df <- data.frame(y = 1:3)
  df$date <- messydates::as_messydate(c("2012-01-01", "2012-06", "2013~"))
  df
}

test_that("a layer mapping an mdate gets a date scale without asking", {
  skip_if_not_installed("messydates")
  df <- make_mdates()
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = y)) +
    ggplot2::geom_point()
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$data[[1]]$x,
               as.numeric(as.Date(df$date, FUN = messydates::vmin)))
  expect_match(built$layout$panel_params[[1]]$x$get_labels(),
               "[0-9]{4}", all = TRUE)
})

test_that("FUN chooses how each messy date is resolved onto the axis", {
  skip_if_not_installed("messydates")
  df <- make_mdates()
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = y)) +
    ggplot2::geom_point() + scale_x_mdate(FUN = messydates::vmax)
  expect_equal(ggplot2::ggplot_build(p)$data[[1]]$x,
               as.numeric(as.Date(df$date, FUN = messydates::vmax)))
})

test_that("scale_y_mdate() places an mdate on the y axis", {
  skip_if_not_installed("messydates")
  df <- make_mdates()
  p <- ggplot2::ggplot(df, ggplot2::aes(x = y, y = date)) +
    ggplot2::geom_point() + scale_y_mdate()
  expect_equal(ggplot2::ggplot_build(p)$data[[1]]$y,
               as.numeric(as.Date(df$date, FUN = messydates::vmin)))
})

test_that("the span a messy date covers can be drawn with both ends", {
  skip_if_not_installed("messydates")
  df <- make_mdates()
  p <- ggplot2::ggplot(df, ggplot2::aes(x = messydates::vmin(date),
                                        xend = messydates::vmax(date),
                                        y = y, yend = y)) +
    ggplot2::geom_segment()
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$data[[1]]$x,
               as.numeric(as.Date(df$date, FUN = messydates::vmin)))
  expect_equal(built$data[[1]]$xend,
               as.numeric(as.Date(df$date, FUN = messydates::vmax)))
})

test_that("a plot with an mdate scale renders", {
  skip_if_not_installed("messydates")
  df <- make_mdates()
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = y)) +
    ggplot2::geom_point() + scale_x_mdate(date_labels = "%Y-%m")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(suppressWarnings(print(p)))
})
