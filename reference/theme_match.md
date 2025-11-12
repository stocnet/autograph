# Matching colors across palettes

Sometimes a palette or particular colours are chosen to symbolise or
represent a particular idea, such as red for "stop" or green for "go",
or to convey some other interpretation. Yet institutional palettes do
not necessarily include all colours, which can constrain how
interpretable visualisations are under institutional branding
requirements. `match_color()` helps to find the closest matching colours
in a given palette to one or more input colours.

There is also a helper function, `is_dark()`, to determine whether a
color is dark or light, which can be useful when deciding whether to use
white or black text on top of a colored background.

## Usage

``` r
match_color(colors, pal)

is_dark(colors)
```

## Arguments

  - colors:
    
    One or more hexcodes to match with colors from the palette.

  - pal:
    
    Optionally, a vector of hexcodes representing a palette in which to
    find matches. By default, the current theme's qualitative palette is
    used.

## Value

A vector of hexcodes the length of the first argument.

## Details

This function uses the Euclidean distance of colours in CIELAB space to
those of a target palette to find the closes corresponding colours. It
also ensures that each input color is matched to a unique color in the
palette. If there are more input colors than unique colors in the
palette, an error is returned.

By default, the current theme's qualitative palette is used, but any
vector of hexcodes can be passed to the `pal` argument.

## Examples

``` r
match_color("#4575b4")
#> [1] "#4575B4"
is_dark(c("#000","#FFF"))
#>  #000  #FFF 
#>  TRUE FALSE 
```
