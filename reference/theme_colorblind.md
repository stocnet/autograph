# Checking colours for colour blindness, print, and legibility

These functions report how a set of colours holds up for viewers with
colour vision deficiency (CVD), which affects about 8% of men and 0.5%
of women, and for readers who see the plot in greyscale or at a
distance.

`simulate_colorblind()` returns what a set of colours looks like to a
viewer with a given type of colour blindness, or in greyscale.
`check_separation()` scores how far apart colours are, taking the worst
case over normal vision and each type of colour blindness, so that a
palette is only credited for a difference that every viewer can see.
`check_contrast()` scores whether text can be read on a ground.

## Usage

``` r
simulate_colorblind(
  colors,
  type = c("deutan", "protan", "tritan", "grey", "normal"),
  severity = 1
)

check_separation(colors, background = NULL)

check_contrast(colors, background = NULL)
```

## Arguments

- colors:

  One or more colours, given as hexcodes or as names R knows.

- type:

  The type of colour blindness to simulate: "deutan" (green-blind, the
  most common), "protan" (red-blind), "tritan" (blue-blind), "grey" for
  greyscale, as a photocopier renders it, or "normal" for unaffected
  vision.

- severity:

  How severe the colour blindness is, between 0 and 1. By default 1,
  which is dichromacy. A value between 0 and 1 is anomalous trichromacy.
  Ignored for the "grey" and "normal" types.

- background:

  Optionally, a colour to include in the comparison, so that a colour
  too pale or too dark to be seen against it is not counted as distinct.
  By default the current theme's background is used.

## Value

`simulate_colorblind()` returns a vector of hexcodes as long as
`colors`.

`check_separation()` returns a square matrix of worst-case distances,
with the colours as its dimnames and a missing diagonal, so that
`min(x, na.rm = TRUE)` gives the closest pair. A "grey" attribute holds
the same matrix as seen in greyscale.

`check_contrast()` returns a square matrix of WCAG contrast ratios,
shaped the same way.

## Details

The three functions answer three different questions, and a palette
needs all three answered. `check_separation()` asks whether two marks
can be told apart, `check_contrast()` asks whether text can be read on
what it sits on, and the "grey" simulation asks whether either survives
a photocopier.

Simulation uses the matrices of Machado, Oliveira and Fernandes (2009),
applied in linear RGB. Those matrices are published for each severity of
colour blindness; `severity` interpolates between the identity and the
full-severity matrix, which approximates the published steps closely
enough for a check. Full severity is dichromacy (deuteranopia,
protanopia, tritanopia); a lower severity is anomalous trichromacy
(deuteranomaly, protanomaly), which is the more common condition.
Greyscale conversion takes the relative luminance of the colour, the
same quantity `check_contrast()` scores with.

Distances are Euclidean distances in CIELAB space, the same measure
[`match_color()`](https://stocnet.github.io/autograph/reference/theme_match.md)
uses. As a rule of thumb, a distance below 10 means two colours are
easily confused, 10 to 25 means they are separable but close, and above
25 means they are comfortably distinct. Ratios are those of WCAG 2.1,
which asks for at least 4.5 for body text and at least 3 for large text
and for graphical objects.

## References

Machado, Gustavo M., Manuel M. Oliveira, and Leandro A. F. Fernandes.
2009. "A Physiologically-Based Model for Simulation of Color Vision
Deficiency". *IEEE Transactions on Visualization and Computer Graphics*
15(6): 1291-98.
[doi:10.1109/TVCG.2009.113](https://doi.org/10.1109/TVCG.2009.113)

World Wide Web Consortium. 2018. *Web Content Accessibility Guidelines
(WCAG) 2.1*. <https://www.w3.org/TR/WCAG21/>

## See also

Other themes:
[`list_fonts()`](https://stocnet.github.io/autograph/reference/list_fonts.md),
[`theme_medium`](https://stocnet.github.io/autograph/reference/theme_medium.md),
[`theme_set`](https://stocnet.github.io/autograph/reference/theme_set.md)

## Examples

``` r
simulate_colorblind(c("#d73027", "#4575b4"), "deutan")
#> [1] "#8E7F1E" "#4B6FB3"
# A milder deuteranomaly, and the same colours in greyscale
simulate_colorblind(c("#d73027", "#4575b4"), "deutan", severity = 0.5)
#> [1] "#B76123" "#4872B3"
simulate_colorblind(c("#d73027", "#4575b4"), "grey")
#> [1] "#727272" "#737373"
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
