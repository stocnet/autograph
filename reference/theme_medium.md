# Setting the medium a plot is made for

A theme says how a plot should look. A medium says where it will be
seen, which is a separate question: the same institutional theme serves
a figure worked on at a desk, projected in a lecture theatre, printed in
an article, and read on a phone, but each of those wants a different
size of text and, in one case, a different ground. `stocnet_medium()`
sets the medium for all subsequent plots, as
[`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
sets the theme, and leaves the theme alone.

If no medium is specified (i.e. the function is called without
argument), the current medium is reported. The default medium is
"screen".

## Usage

``` r
stocnet_medium(medium = NULL, persist = FALSE)

set_stocnet_medium(medium = NULL, persist = FALSE)

ag_size()
```

## Arguments

- medium:

  String naming a medium. By default "screen". The following media are
  currently available: screen, presentation, mobile, print. This string
  can be capitalised or not.

- persist:

  Logical, by default FALSE. If TRUE, the medium is remembered across
  sessions, by writing it to the user's configuration directory (see
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).
  Nothing is written to disk unless this is set explicitly. Use
  `stocnet_medium(persist = FALSE)` when setting a medium to forget a
  previously persisted choice.

## Value

`stocnet_medium()` sets the medium to be used across all stocnet
packages. The medium is written to an option and held there. `ag_size()`
returns the multiplier the current medium applies to text sizes, which
is 1 unless the medium says otherwise.

## Details

The media available are:

- "screen", the default, which draws as `{autograph}` always has.

- "presentation", which enlarges text by half, for a figure read from
  the back of a room.

- "mobile", which enlarges text further, for a figure read in a narrow
  column on a handheld screen. Keep such a figure to one point, with few
  categories: a legend of more than about seven keys, or more than about
  three panels from
  [`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md),
  will not survive the width.

- "print", which leaves text at its usual size but draws on white,
  whatever ground the theme prefers. A dark or tinted ground costs ink
  and is often not reproduced.

The medium scales text, not marks. Node sizes are relative to the layout
they sit in, so enlarging them without enlarging the layout would crowd
it. Where a figure needs larger nodes as well, set `node_size` in
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md).

The medium does not set the size of the file written. Give
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
the width, height and resolution the medium calls for as well.

## See also

Other themes:
[`list_fonts()`](https://stocnet.github.io/autograph/reference/list_fonts.md),
[`theme_colorblind`](https://stocnet.github.io/autograph/reference/theme_colorblind.md),
[`theme_set`](https://stocnet.github.io/autograph/reference/theme_set.md)

## Examples

``` r
stocnet_medium("presentation")
ag_size()
#> [1] 1.5
stocnet_medium("screen")
```
