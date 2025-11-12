# Themed scales for further customization

These functions enable to add color scales to be graphs.

## Usage

``` r
scale_fill_iheid(direction = 1, ...)

scale_colour_iheid(direction = 1, ...)

scale_color_iheid(direction = 1, ...)

scale_edge_colour_iheid(direction = 1, ...)

scale_edge_color_iheid(direction = 1, ...)

scale_fill_centres(direction = 1, ...)

scale_colour_centres(direction = 1, ...)

scale_color_centres(direction = 1, ...)

scale_edge_colour_centres(direction = 1, ...)

scale_edge_color_centres(direction = 1, ...)

scale_fill_sdgs(direction = 1, ...)

scale_colour_sdgs(direction = 1, ...)

scale_color_sdgs(direction = 1, ...)

scale_edge_colour_sdgs(direction = 1, ...)

scale_edge_color_sdgs(direction = 1, ...)

scale_fill_ethz(direction = 1, ...)

scale_colour_ethz(direction = 1, ...)

scale_color_ethz(direction = 1, ...)

scale_edge_colour_ethz(direction = 1, ...)

scale_edge_color_ethz(direction = 1, ...)

scale_fill_uzh(direction = 1, ...)

scale_colour_uzh(direction = 1, ...)

scale_color_uzh(direction = 1, ...)

scale_edge_colour_uzh(direction = 1, ...)

scale_edge_color_uzh(direction = 1, ...)

scale_fill_rug(direction = 1, ...)

scale_colour_rug(direction = 1, ...)

scale_color_rug(direction = 1, ...)

scale_edge_colour_rug(direction = 1, ...)

scale_edge_color_rug(direction = 1, ...)
```

## Arguments

  - direction:
    
    Direction for using palette colors.

  - ...:
    
    Extra arguments passed to `ggplot2::discrete_scale()`.

## Examples

``` r
#ison_brandes %>%
#mutate(core = migraph::node_is_core(ison_brandes)) %>%
#graphr(node_color = "core") +
#scale_color_iheid()
#graphr(ison_physicians[[1]], edge_color = "type") +
#scale_edge_color_ethz()
```
