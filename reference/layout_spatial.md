# Spatial layout

The "spatial" layout places each node at its own position, as a spatial
network records it (see
[`manynet::is_spatial()`](https://stocnet.github.io/manynet/reference/mark_format_node.html)):
in 'x' and 'y' node attributes, or in 'lon' and 'lat' (or 'longitude'
and 'latitude').
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
uses it by default for a spatial network, such as one read by
[`manynet::read_geojson()`](https://stocnet.github.io/manynet/reference/make_read.html).
The two axes share one scale, so that a distance drawn is the same
whichever way it runs.

Longitudes and latitudes are drawn on an equirectangular projection
centred on the network's mean latitude: each longitude is multiplied by
the cosine of that latitude, so that a degree of longitude is drawn as
long as it is on the ground there. This keeps the shape of a city or a
region, though not of a continent.

A node without a position is placed at the centre of those with one.

## Usage

``` r
layout_spatial(.data, circular = FALSE, times = 1)

layout_tbl_graph_spatial(.data, circular = FALSE, times = 1)
```

## Arguments

- .data:

  Some `{manynet}` compatible network data.

- circular:

  Should the layout be transformed into a radial representation. Only
  possible for some layouts. Defaults to FALSE. Required for `{ggraph}`
  compatibility.

- times:

  Maximum number of iterations, where appropriate. Required for
  `{ggraph}` compatibility, and ignored by the layouts that do not
  iterate.

## Value

Returns a table of nodes' x and y coordinates.

## See also

Other mapping:
[`check_layout`](https://stocnet.github.io/autograph/reference/check_layout.md),
[`completion`](https://stocnet.github.io/autograph/reference/completion.md),
[`layout_concentric()`](https://stocnet.github.io/autograph/reference/layout_concentric.md),
[`layout_configuration()`](https://stocnet.github.io/autograph/reference/layout_configuration.md),
[`layout_correspondence()`](https://stocnet.github.io/autograph/reference/layout_correspondence.md),
[`layout_layered()`](https://stocnet.github.io/autograph/reference/layout_layered.md),
[`layout_levels()`](https://stocnet.github.io/autograph/reference/layout_levels.md),
[`layout_matching()`](https://stocnet.github.io/autograph/reference/layout_matching.md),
[`layout_scaling()`](https://stocnet.github.io/autograph/reference/layout_scaling.md),
[`layout_valence()`](https://stocnet.github.io/autograph/reference/layout_valence.md),
[`plot_graphr`](https://stocnet.github.io/autograph/reference/plot_graphr.md),
[`plot_graphs`](https://stocnet.github.io/autograph/reference/plot_graphs.md),
[`plot_grapht`](https://stocnet.github.io/autograph/reference/plot_grapht.md)

## Examples

``` r
net <- manynet::create_ring(4) |>
  manynet::mutate_nodes(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
graphr(net)
```
