# Valence layout

The "valence" layout places the nodes of a signed network so that
positively tied nodes are drawn together and negatively tied nodes
apart.

## Usage

``` r
layout_valence(
  .data,
  times = 500,
  center = NULL,
  circular = FALSE,
  repulsion_coef = 1,
  attraction_coef = 0.05
)

layout_tbl_graph_valence(
  .data,
  times = 500,
  center = NULL,
  circular = FALSE,
  repulsion_coef = 1,
  attraction_coef = 0.05
)
```

## Arguments

- .data:

  Some `{manynet}` compatible network data.

- times:

  Maximum number of iterations, where appropriate. Required for
  `{ggraph}` compatibility, and ignored by the layouts that do not
  iterate.

- center:

  Required for `{ggraph}` compatibility, and not used here.

- circular:

  Should the layout be transformed into a radial representation. Only
  possible for some layouts. Defaults to FALSE. Required for `{ggraph}`
  compatibility.

- repulsion_coef:

  Coefficient for global repulsion force. Default is 1.

- attraction_coef:

  Coefficient for edge-based attraction/repulsion force. Default is
  0.05.

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
[`plot_graphr`](https://stocnet.github.io/autograph/reference/plot_graphr.md),
[`plot_graphs`](https://stocnet.github.io/autograph/reference/plot_graphs.md),
[`plot_grapht`](https://stocnet.github.io/autograph/reference/plot_grapht.md)

## Examples

``` r
edges <- data.frame(
  from = c("A", "B", "C", "D"),
  to   = c("B", "C", "D", "A"),
  weight = c(2, 3, 1, 4),
  sign = c(1, -1, 1, -1)  # 1 = positive, -1 = negative
  )
graphr(as_igraph(edges), layout="valence")
```
