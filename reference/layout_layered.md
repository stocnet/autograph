# Layered layouts

These algorithms assign each node to a layer, which becomes one axis,
and a position within that layer, which becomes the other. They are
recommended for use with
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
or `{ggraph}`, and suit two-mode networks and directed acyclic networks.

The four layouts are one engine drawn four ways, and differ only in
which axis carries the layers and in how each layer is spread out:

|                          |                     |                    |
|--------------------------|---------------------|--------------------|
|                          | Layers stacked flat | Layers standing up |
| `alignment = "straight"` | "layered"           | "lineage"          |
| `alignment = "rungs"`    | "railway"           | "ladder"           |

That is, the "layered" layout places the first node set along the bottom
and the second node set along the top, sequenced and spaced as necessary
to minimise tie overlap. The "lineage" layout is the same layout with
the axes exchanged, so that successive layers run left to right rather
than bottom to top. The "railway" and "ladder" layouts are "layered" and
"lineage" with every layer given the same spacing, so that the nodes
line up across the layers like the rails and rungs the names describe.

## Usage

``` r
layout_layered(
  .data,
  center = NULL,
  ranks = c("tight", "generation", "compact"),
  alignment = c("straight", "rungs"),
  circular = FALSE,
  times = 1000
)

layout_tbl_graph_layered(
  .data,
  center = NULL,
  ranks = c("tight", "generation", "compact"),
  alignment = c("straight", "rungs"),
  circular = FALSE,
  times = 1000
)

layout_lineage(
  .data,
  ranks = c("tight", "generation", "compact"),
  alignment = c("straight", "rungs"),
  circular = FALSE,
  times = 1000,
  rank = NULL
)

layout_tbl_graph_lineage(
  .data,
  ranks = c("tight", "generation", "compact"),
  alignment = c("straight", "rungs"),
  circular = FALSE,
  times = 1000,
  rank = NULL
)

layout_railway(
  .data,
  ranks = c("tight", "generation", "compact"),
  circular = FALSE,
  times = 1000
)

layout_tbl_graph_railway(
  .data,
  ranks = c("tight", "generation", "compact"),
  circular = FALSE,
  times = 1000
)

layout_ladder(
  .data,
  ranks = c("tight", "generation", "compact"),
  circular = FALSE,
  times = 1000
)

layout_tbl_graph_ladder(
  .data,
  ranks = c("tight", "generation", "compact"),
  circular = FALSE,
  times = 1000
)
```

## Arguments

- .data:

  Some `{manynet}` compatible network data.

- center:

  Further split a "layered" layout by declaring the "center" argument as
  the "events", "actors", or by declaring a node name. Defaults to NULL.

- ranks:

  How the layers are assigned: "tight" (the default) chooses the layers
  that make the total tie length as short as possible, while still
  pointing every tie down at least one layer; "generation" ranks each
  node by its distance from a root, so that a layer is a generation, at
  the cost of some longer ties; "compact" asks
  [`igraph::layout_with_sugiyama()`](https://r.igraph.org/reference/layout_with_sugiyama.html)
  for the layers. The first two need an acyclic network, and fall back
  to "compact" where the network is not. Ignored for a two-mode network,
  whose layers are its modes.

  A node attribute can be given here instead, either as the name of a
  numeric node attribute or as a numeric vector as long as the network
  has nodes. Then the layers are those values, and nodes are placed
  along that axis in proportion to them rather than at even steps, so
  that a network of dated nodes is drawn as a timeline. The values run
  in the same direction as the layers the engine works out: down the
  page in a "layered" or "railway" layout, and left to right in a
  "lineage" or "ladder" layout, so that the smallest value comes first.

- alignment:

  How each layer is spread out: "straight" (the default) draws the ties
  as close to straight as the ordering allows, which groups the nodes
  that belong together; "rungs" gives every layer the same integer
  spacing, so that the nodes line up across the layers.

- circular:

  Should the layout be transformed into a radial representation. Only
  possible for some layouts. Defaults to FALSE. Required for `{ggraph}`
  compatibility.

- times:

  Maximum number of iterations, where appropriate. Required for
  `{ggraph}` compatibility, and ignored by the layouts that do not
  iterate.

- rank:

  Deprecated. Use `ranks` instead, which now takes a node attribute as
  well as a method.

## Value

Returns a table of nodes' x and y coordinates.

## See also

Other mapping:
[`check_layout`](https://stocnet.github.io/autograph/reference/check_layout.md),
[`completion`](https://stocnet.github.io/autograph/reference/completion.md),
[`layout_concentric()`](https://stocnet.github.io/autograph/reference/layout_concentric.md),
[`layout_configuration()`](https://stocnet.github.io/autograph/reference/layout_configuration.md),
[`layout_correspondence()`](https://stocnet.github.io/autograph/reference/layout_correspondence.md),
[`layout_levels()`](https://stocnet.github.io/autograph/reference/layout_levels.md),
[`layout_matching()`](https://stocnet.github.io/autograph/reference/layout_matching.md),
[`layout_scaling()`](https://stocnet.github.io/autograph/reference/layout_scaling.md),
[`layout_valence()`](https://stocnet.github.io/autograph/reference/layout_valence.md),
[`plot_graphr`](https://stocnet.github.io/autograph/reference/plot_graphr.md),
[`plot_graphs`](https://stocnet.github.io/autograph/reference/plot_graphs.md),
[`plot_grapht`](https://stocnet.github.io/autograph/reference/plot_grapht.md)

## Examples

``` r
#graphr(ison_southern_women, layout = "layered", center = "events",
#           node_color = "type", node_size = 3)
#graphr(ison_southern_women, layout = "lineage")
# ison_adolescents |>
#   mutate(year = rep(c(1985, 1990, 1995, 2000), times = 2)) |>
#   graphr(layout = "lineage", ranks = "year")
```
