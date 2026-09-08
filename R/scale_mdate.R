# Scales for 'messy' dates, i.e. the "mdate" class of the {messydates} package.
# {messydates} is only suggested, and holds no {ggplot2} code of its own, so
# these scales live here, where {ggplot2} is already imported.

#' Scales for messy dates
#'
#' @description
#'   These scales place a 'messy' date, of the `mdate` class that
#'   `{messydates}` defines, on the x or y axis of a `{ggplot2}` plot.
#'   The dates are then spaced by how far apart they are,
#'   and the axis is marked with date breaks.
#'   Without them, `{ggplot2}` reads an `mdate` column as a character vector
#'   and draws a discrete axis, which orders the dates as text
#'   and spaces them evenly.
#' @details
#'   `{ggplot2}` chooses a scale from the class of the column,
#'   so a plot that maps an `mdate` column to `x` or `y`
#'   picks these scales up without naming them.
#'   Add the scale to the plot to pass `FUN`, `date_breaks`, or `date_labels`.
#'
#'   A position on an axis is a single point,
#'   but a messy date may be a range, a set, or an unspecified component,
#'   and so covers a span of dates.
#'   These scales therefore resolve each date to one date with `FUN`,
#'   as `messydates::as.Date()` does.
#'   To draw the span itself, resolve both of its ends in the mapping,
#'   as in `aes(x = vmin(date), xend = vmax(date))` with `geom_segment()`;
#'   both ends are `mdate` vectors, so they share this scale.
#' @param ... Arguments passed on to [ggplot2::scale_x_date()] or
#'   [ggplot2::scale_y_date()],
#'   such as `name`, `breaks`, `date_breaks`, `date_labels`, or `limits`.
#' @param FUN The function that resolves each messy date to the one date
#'   at which it is drawn.
#'   `messydates::vmin` by default, as for `messydates::as.Date()`,
#'   which also takes `vmax`, `vmean`, `vmedian`, `vmodal`, or `vrandom`.
#' @returns A `{ggplot2}` scale to add to a plot.
#' @name mdate_scales
#' @examplesIf requireNamespace("messydates", quietly = TRUE)
#' library(ggplot2)
#' dates <- messydates::as_messydate(c("2012-01-01", "2012-06", "2013~"))
#' df <- data.frame(date = dates, y = 1:3)
#' ggplot(df, aes(x = date, y = y)) + geom_point()
#' ggplot(df, aes(x = date, y = y)) + geom_point() +
#'   scale_x_mdate(FUN = messydates::vmax, date_labels = "%Y-%m")
NULL

#' @rdname mdate_scales
#' @export
scale_x_mdate <- function(..., FUN = messydates::vmin) {
  .mdate_scale("x", ..., FUN = FUN)
}

#' @rdname mdate_scales
#' @export
scale_y_mdate <- function(..., FUN = messydates::vmin) {
  .mdate_scale("y", ..., FUN = FUN)
}

# Builds a {ggplot2} date scale, then swaps in a transformation that resolves
# an mdate to a Date first. {ggplot2}'s own date transformation rejects
# anything that is not already a Date. The scale is otherwise untouched, so
# its breaks, labels and guides stay those of a date axis.
.mdate_scale <- function(aesthetic, ..., FUN) {
  thisRequires("messydates")
  scale <- if (aesthetic == "x") ggplot2::scale_x_date(...) else
    ggplot2::scale_y_date(...)
  transformation <- if (is.null(scale$get_transformation)) scale$trans else
    scale$get_transformation()
  transformation$transform <- function(x) {
    if (inherits(x, "mdate")) x <- as.Date(x, FUN = FUN)
    if (inherits(x, "POSIXct")) x <- as.Date(x)
    structure(as.numeric(x), names = names(x))
  }
  # the field is `trans` up to ggplot2 3.5 and `transformation` thereafter
  if (exists("trans", envir = scale, inherits = FALSE))
    scale$trans <- transformation
  if (exists("transformation", envir = scale, inherits = FALSE))
    scale$transformation <- transformation
  scale
}

# Tells {ggplot2} which scale to look for when a layer maps an mdate column.
# {ggplot2} then finds scale_x_mdate()/scale_y_mdate() above.
# Registered in .onLoad(), since {ggplot2} owns the generic.
scale_type.mdate <- function(x) "mdate"
