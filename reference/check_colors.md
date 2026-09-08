# Checking colours for colour blindness and legibility

These functions score a set of colours rather than the plot that uses
them, so that one palette can be compared with another.

`check_separation()` reports how far apart the colours are, taking the
worst case over normal vision and each type of colour blindness, so that
a palette is only credited for a difference that every viewer can see.

`check_contrast()` reports whether text can be read on a ground.

## Usage

``` r
check_separation(colors, background = NULL)

check_contrast(colors, background = NULL)
```

## Arguments

- colors:

  One or more colours, given as hexcodes or as names R knows.

- background:

  Optionally, a colour to include in the comparison, so that a colour
  too pale or too dark to be seen against it is not counted as distinct.
  By default the current theme's background is used.

## Value

`check_separation()` returns a square matrix of worst-case distances,
with the colours as its dimnames and a missing diagonal, so that
`min(x, na.rm = TRUE)` gives the closest pair. A "grey" attribute holds
the same matrix as seen in greyscale.

`check_contrast()` returns a square matrix of WCAG contrast ratios,
shaped the same way.

## Details

The two functions answer different questions, and a palette needs both
answered. `check_separation()` asks whether two marks can be told apart,
and `check_contrast()` asks whether text can be read on what it sits on.
[`simulate_colorblind()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
asks a third question, which is whether either survives a photocopier.

Distances are Euclidean distances in CIELAB space, the same measure
[`match_color()`](https://stocnet.github.io/autograph/reference/theme_match.md)
uses. As a rule of thumb, a distance below 10 means two colours are
easily confused, 10 to 25 means they are separable but close, and above
25 means they are comfortably distinct. Ratios are those of WCAG 2.1,
which asks for at least 4.5 for body text and at least 3 for large text
and for graphical objects.

Colour blindness affects about 8% of men and 0.5% of women, and the
worst case is taken over deuteranopia, protanopia and tritanopia as well
as normal vision.

## References

World Wide Web Consortium. 2018. *Web Content Accessibility Guidelines
(WCAG) 2.1*. <https://www.w3.org/TR/WCAG21/>

## See also

[`check_layout()`](https://stocnet.github.io/autograph/reference/check_layout.md),
which scores a drawing rather than a palette.

Other themes:
[`list_fonts()`](https://stocnet.github.io/autograph/reference/list_fonts.md),
[`theme_colorblind`](https://stocnet.github.io/autograph/reference/theme_colorblind.md),
[`theme_medium`](https://stocnet.github.io/autograph/reference/theme_medium.md),
[`theme_set`](https://stocnet.github.io/autograph/reference/theme_set.md)

## Examples

``` r
# How well does the current theme's palette separate five categories?
check_separation(ag_qualitative(5))
#>          #1B9E77  #E6AB02  #7570B3  #d73027  #666666
#> #1B9E77       NA 57.28188 33.30590 27.24970 15.15340
#> #E6AB02 57.28188       NA 55.49031 32.50189 50.76671
#> #7570B3 33.30590 55.49031       NA 69.48994 13.49946
#> #d73027 27.24970 32.50189 69.48994       NA 32.60059
#> #666666 15.15340 50.76671 13.49946 32.60059       NA
#> 
#> Closest pair in greyscale: 2.4
# The closest pair in it
min(check_separation(ag_qualitative(5)), na.rm = TRUE)
#> [1] 13.49946
# And the closest pair once it is printed in greyscale
min(attr(check_separation(ag_qualitative(5)), "grey"), na.rm = TRUE)
#> [1] 2.39003
# A red and a green that only look different to some viewers
check_separation(c("#B7352D", "#627313"))[1, 2]
#> [1] 3.755487
# Can the current theme's ink be read on its ground?
check_contrast(ag_ink())[1, 2]
#> [1] 18.73366
```
