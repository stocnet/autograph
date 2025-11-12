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

Other mapping: `layout_partition`, `plot_graphr`, `plot_graphs`,
`plot_grapht`
