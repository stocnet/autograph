# Simulating how colours look to a colour-blind viewer

`simulate_colorblind()` returns what a set of colours looks like to a
viewer with a given type of colour vision deficiency (CVD), which
affects about 8% of men and 0.5% of women, or in greyscale, as a reader
sees the plot in print.

## Usage

``` r
simulate_colorblind(
  colors,
  type = c("deutan", "protan", "tritan", "grey", "normal"),
  severity = 1
)
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

## Value

`simulate_colorblind()` returns a vector of hexcodes as long as
`colors`.

## Details

Simulation uses the matrices of Machado, Oliveira and Fernandes (2009),
applied in linear RGB. Those matrices are published for each severity of
colour blindness; `severity` interpolates between the identity and the
full-severity matrix, which approximates the published steps closely
enough for a check. Full severity is dichromacy (deuteranopia,
protanopia, tritanopia); a lower severity is anomalous trichromacy
(deuteranomaly, protanomaly), which is the more common condition.
Greyscale conversion takes the relative luminance of the colour, the
same quantity
[`check_contrast()`](https://stocnet.github.io/autograph/reference/check_colors.md)
scores with.

## References

Machado, Gustavo M., Manuel M. Oliveira, and Leandro A. F. Fernandes.
2009. "A Physiologically-Based Model for Simulation of Color Vision
Deficiency". *IEEE Transactions on Visualization and Computer Graphics*
15(6): 1291-98.
[doi:10.1109/TVCG.2009.113](https://doi.org/10.1109/TVCG.2009.113)

## See also

[`check_colors()`](https://stocnet.github.io/autograph/reference/check_colors.md),
which scores a palette rather than simulating it.

Other themes:
[`check_colors`](https://stocnet.github.io/autograph/reference/check_colors.md),
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
```
