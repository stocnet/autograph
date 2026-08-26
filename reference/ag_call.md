# Consistent palette calls

These functions assist in calling particular parts of a theme's palette.
For example, `ag_base()` will return the current theme's base or
background color, and `ag_highlight()` will return the color used in
that theme to highlight one or more nodes, lines, or such. `ag_ink()`
returns the darker colour that theme writes with: axis text, reference
lines, and other chrome. `ag_missing()` returns the neutral that theme
sets aside for data that should recede: missing values, isolates counted
out of a drawing, and any "other" remainder left when small categories
are grouped down. Keeping one colour for all three means a reader learns
it once. Keeping the two apart lets the base be light enough to stand
away from the highlight while the ink stays dark enough to read. Where
the ground changes under a theme – the "print" medium forces white,
whatever the theme prefers – `ag_ink()` falls back to black or white
rather than return an ink that cannot be read on it. See
[`check_contrast()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
and
[`stocnet_medium()`](https://stocnet.github.io/autograph/reference/theme_medium.md).

Using palettes that are high contrast, aesthetically pleasing, and
institutionally or thematically consistent is not without its
challenges.

## Usage

``` r
ag_base()

ag_ink()

ag_missing()

ag_highlight()

ag_positive()

ag_negative()

ag_qualitative(number)

ag_sequential(number)

ag_divergent(number)

ag_font()
```

## Arguments

- number:

  Integer of how many category colours to return.

## Value

One or more hexcodes as strings.

## Colour blindness

The default palettes are designed to be colour-blind friendly. There are
different types of colour-blindness. The most common type, red-green
colour-blindness, finds it difficult to distinguish between the red and
green hues used in the [rainbow
palette](https://colorspace.r-forge.r-project.org/articles/endrainbow.html),
for instance. Fortunately there are a range of palettes that function
fairly well for those who are color-blind. These include the
[viridis](https://CRAN.R-project.org/package=viridis) palette, and the
ColorBrewer palettes (included in the RColorBrewer package).

An institutional palette is not ours to change, but its order is. Each
theme's categorical palette is therefore reordered when the theme is
set, so that the first colours a plot draws on are those that stay
distinct under each type of colour blindness, and `ag_qualitative()`
takes those colours in order rather than interpolating between them.
Divergent palettes pair a warm pole with a cool one for the same reason.
Use
[`check_separation()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
to check how your own colours fare, and
[`simulate_colorblind()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
to see them as a colour-blind viewer would.

Two further questions are worth asking of a palette. Whether its text
can be read on what it sits on is a matter of contrast rather than of
hue, and
[`check_contrast()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
scores it against the thresholds of WCAG 2.1. Whether it survives print
is a matter of lightness alone, since a greyscale device keeps the
luminance of a colour and discards the rest;
`simulate_colorblind(type = "grey")` shows that view, and
[`check_separation()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
reports the greyscale distances beside its own score. Most institutional
palettes separate by hue and so collapse in greyscale. Where a figure
has to print in black and white, use the "bw" theme, or add a second
channel such as `node_shape`.

The "rainbow" theme is the exception, and is left in its own order. Its
point is fidelity to the spectrum of an observed rainbow, which
reordering would destroy, so `ag_qualitative()` samples across its whole
length instead. A spectrum is not a colour-blind safe scheme: its reds
and greens are exactly the pair that red-green colour blindness cannot
separate. Choose it where the order of the categories is itself
meaningful, and check the result with
[`check_separation()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md);
for categories with no order, another theme serves more readers.

## Examples

``` r
# Single colours from the currently active theme
ag_base()
#> [1] "black"
ag_ink()
#> [1] "#121212"
ag_highlight()
#> [1] "red"
ag_missing()
#> [1] "#8C8C8C"
ag_positive()
#> [1] "#4575b4"
ag_negative()
#> [1] "#d73027"
# Palettes of a requested length
ag_qualitative(3)
#> [1] "#1B9E77" "#E6AB02" "#7570B3"
ag_sequential(5)
#> [1] "#000000" "#350C09" "#6B1813" "#A1241D" "#D73027"
ag_divergent(5)
#> [1] "#D73027" "#EB9793" "#FFFFFF" "#A2BAD9" "#4575B4"
# The accessors follow whichever theme is set
ag_font()
#> [1] "sans"
```
