# Listing the fonts available to R

`list_fonts()` reports the font families that R can currently see, which
is what a theme's preferred fonts are matched against. A font that is
installed on the system but missing from this list is not available to R
yet; see the Fonts section of
[theme_set](https://stocnet.github.io/autograph/reference/theme_set.md)
for how to make it so.

## Usage

``` r
list_fonts(pattern = NULL)
```

## Arguments

- pattern:

  Optionally, a string with which to filter the font families returned,
  matched without regard to case. For example, `list_fonts("sans")`
  returns every family whose name includes "sans".

## Value

A vector of font family names.

## See also

Other themes:
[`theme_colorblind`](https://stocnet.github.io/autograph/reference/theme_colorblind.md),
[`theme_medium`](https://stocnet.github.io/autograph/reference/theme_medium.md),
[`theme_set`](https://stocnet.github.io/autograph/reference/theme_set.md)

## Examples

``` r
head(list_fonts())
#> [1] "Academy Engraved LET" "Al Bayan"             "Al Nile"             
#> [4] "Al Tarikh"            "American Typewriter"  "Andale Mono"         
```
