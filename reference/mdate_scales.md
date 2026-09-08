# Scales for messy dates

These scales place a 'messy' date, of the `mdate` class that
`{messydates}` defines, on the x or y axis of a `{ggplot2}` plot. The
dates are then spaced by how far apart they are, and the axis is marked
with date breaks. Without them, `{ggplot2}` reads an `mdate` column as a
character vector and draws a discrete axis, which orders the dates as
text and spaces them evenly.

## Usage

``` r
scale_x_mdate(..., FUN = messydates::vmin)

scale_y_mdate(..., FUN = messydates::vmin)
```

## Arguments

- ...:

  Arguments passed on to
  [`ggplot2::scale_x_date()`](https://ggplot2.tidyverse.org/reference/scale_date.html)
  or
  [`ggplot2::scale_y_date()`](https://ggplot2.tidyverse.org/reference/scale_date.html),
  such as `name`, `breaks`, `date_breaks`, `date_labels`, or `limits`.

- FUN:

  The function that resolves each messy date to the one date at which it
  is drawn.
  [`messydates::vmin`](https://globalgov.github.io/messydates/reference/resolve_extrema.html)
  by default, as for `messydates::as.Date()`, which also takes `vmax`,
  `vmean`, `vmedian`, `vmodal`, or `vrandom`.

## Value

A `{ggplot2}` scale to add to a plot.

## Details

`{ggplot2}` chooses a scale from the class of the column, so a plot that
maps an `mdate` column to `x` or `y` picks these scales up without
naming them. Add the scale to the plot to pass `FUN`, `date_breaks`, or
`date_labels`.

A position on an axis is a single point, but a messy date may be a
range, a set, or an unspecified component, and so covers a span of
dates. These scales therefore resolve each date to one date with `FUN`,
as `messydates::as.Date()` does. To draw the span itself, resolve both
of its ends in the mapping, as in
`aes(x = vmin(date), xend = vmax(date))` with
[`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html);
both ends are `mdate` vectors, so they share this scale.

## Examples

``` r
library(ggplot2)
dates <- messydates::as_messydate(c("2012-01-01", "2012-06", "2013~"))
df <- data.frame(date = dates, y = 1:3)
ggplot(df, aes(x = date, y = y)) + geom_point()

ggplot(df, aes(x = date, y = y)) + geom_point() +
  scale_x_mdate(FUN = messydates::vmax, date_labels = "%Y-%m")
```
