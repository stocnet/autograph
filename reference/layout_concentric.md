# Concentric layout

The "concentric" layout places the nodes on one or more circles, with
each group of nodes on a circle of its own, and the groups ordered
around those circles so that adjacent nodes are drawn close together.
Where one group holds a single node, that node occupies the centre.

## Usage

``` r
layout_concentric(
  .data,
  membership,
  radius = NULL,
  order.by = NULL,
  circular = FALSE,
  times = 1000
)

layout_tbl_graph_concentric(
  .data,
  membership,
  radius = NULL,
  order.by = NULL,
  circular = FALSE,
  times = 1000
)
```

## Source

Diego Diez, Andrew P. Hutchins and Diego Miranda-Saavedra. 2014.
"Systematic identification of transcriptional regulatory modules from
protein-protein interaction networks". *Nucleic Acids Research*, 42 (1)
e6.

## Arguments

- .data:

  Some `{manynet}` compatible network data.

- membership:

  A node attribute or a vector to draw concentric circles. By default
  this is the two modes of a two-mode network.

- radius:

  A vector of radii at which the concentric circles should be located.
  By default this is equal placement around an empty centre, unless one
  (the core) is a single node, in which case this node occupies the
  centre of the graph.

- order.by:

  An attribute label indicating the (decreasing) order for the nodes
  around the circles. By default ordering is given by a bipartite
  placement that reduces the number of edge crossings.

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
#graphr(ison_southern_women, layout = "concentric", membership = "type",
#           node_color = "type", node_size = 3)
```
