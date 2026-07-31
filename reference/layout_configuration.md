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

layout_tbl_graph_dyad(.data, circular = TRUE, times = 1)

layout_triad(.data, circular = TRUE, times = 1)

layout_tbl_graph_triad(.data, circular = TRUE, times = 1)

layout_tetrad(.data, circular = TRUE, times = 1)

layout_tbl_graph_tetrad(.data, circular = TRUE, times = 1)

layout_pentad(.data, circular = TRUE, times = 1)

layout_tbl_graph_pentad(.data, circular = TRUE, times = 1)

layout_hexad(.data, circular = TRUE, times = 1)

layout_tbl_graph_hexad(.data, circular = TRUE, times = 1)
```

## Arguments

- .data:

  Some `{manynet}` compatible network data.

- circular:

  Logical, required for `{ggraph}` compatibility, default TRUE.

- times:

  Integer, how many times to run the algorithm. Required by for
  `{ggraph}` compatibility, but not used here, so default = 1.

## See also

Other mapping:
[`layout_partition`](https://stocnet.github.io/autograph/reference/layout_partition.md),
[`plot_graphr`](https://stocnet.github.io/autograph/reference/plot_graphr.md),
[`plot_graphs`](https://stocnet.github.io/autograph/reference/plot_graphs.md),
[`plot_grapht`](https://stocnet.github.io/autograph/reference/plot_grapht.md)

## Examples

``` r
# "configuration" picks the layout matching the number of nodes
graphr(manynet::create_ring(4), layout = "configuration")

# or a specific configuration can be named
graphr(manynet::create_ring(3), layout = "triad")

# the layout functions can also be called directly for their coordinates
layout_tetrad(manynet::create_ring(4))
#>   x y
#> 1 0 0
#> 2 0 1
#> 3 1 0
#> 4 1 1
```
