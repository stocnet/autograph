# Layout algorithms based on configurational positions

Configurational layouts locate nodes at symmetric coordinates to help
illustrate particular configurations. Currently configurational layouts
are available for 2-6 nodes. The "configuration" layout will choose the
appropriate configurational layout automatically.

## Usage

``` r
layout_configuration(.data, circular = TRUE, times = 1)

layout_tbl_graph_configuration(.data, circular = TRUE, times = 1)

layout_dyad(.data, circular = TRUE, times = 1)

layout_triad(.data, circular = TRUE, times = 1)

layout_tetrad(.data, circular = TRUE, times = 1)

layout_pentad(.data, circular = TRUE, times = 1)

layout_hexad(.data, circular = TRUE, times = 1)
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
# "configuration" picks the layout matching the number of nodes
graphr(manynet::create_ring(4), layout = "configuration")

# the specific configurations are also available as functions
layout_tetrad(manynet::create_ring(4))
#>   x y
#> 1 0 0
#> 2 0 1
#> 3 1 0
#> 4 1 1
```
