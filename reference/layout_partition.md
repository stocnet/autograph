# Layout algorithms based on bi- or other partitions

These algorithms layout networks based on two or more partitions, and
are recommended for use with `graphr()` or `{ggraph}`.

The "hierarchy" layout layers the first node set along the bottom, and
the second node set along the top, sequenced and spaced as necessary to
minimise edge overlap. The "alluvial" layout is similar to "hierarchy",
but places successive layers horizontally rather than vertically. The
"railway" layout is similar to "hierarchy", but nodes are aligned across
the layers. The "ladder" layout is similar to "railway", but places
successive layers horizontally rather than vertically. The "concentric"
layout places a "hierarchy" layout around a circle, with successive
layers appearing as concentric circles. The "multilevel" layout places
successive layers as multiple levels. The "lineage" layout ranks nodes
in Y axis according to values.

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

layout_multilevel(.data, level, circular = FALSE)

layout_tbl_graph_multilevel(.data, level, circular = FALSE)

layout_lineage(.data, rank, circular = FALSE)

layout_tbl_graph_lineage(.data, rank, circular = FALSE)

layout_hierarchy(.data, center = NULL, circular = FALSE, times = 1000)

layout_tbl_graph_hierarchy(
  .data,
  center = NULL,
  circular = FALSE,
  times = 1000
)

layout_alluvial(.data, circular = FALSE, times = 1000)

layout_tbl_graph_alluvial(.data, circular = FALSE, times = 1000)

layout_railway(.data, circular = FALSE, times = 1000)

layout_tbl_graph_railway(.data, circular = FALSE, times = 1000)

layout_ladder(.data, circular = FALSE, times = 1000)

layout_tbl_graph_ladder(.data, circular = FALSE, times = 1000)
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
    
    A node attribute or a vector to draw concentric circles for
    "concentric" layout.

  - radius:
    
    A vector of radii at which the concentric circles should be located
    for "concentric" layout. By default this is equal placement around
    an empty centre, unless one (the core) is a single node, in which
    case this node occupies the centre of the graph.

  - order.by:
    
    An attribute label indicating the (decreasing) order for the nodes
    around the circles for "concentric" layout. By default ordering is
    given by a bipartite placement that reduces the number of edge
    crossings.

  - circular:
    
    Should the layout be transformed into a radial representation. Only
    possible for some layouts. Defaults to FALSE.

  - times:
    
    Maximum number of iterations, where appropriate

  - level:
    
    A node attribute or a vector to hierarchically order levels for
    "multilevel" layout.

  - rank:
    
    A numerical node attribute to place nodes in Y axis according to
    values for "lineage" layout.

  - center:
    
    Further split "hierarchical" layouts by declaring the "center"
    argument as the "events", "actors", or by declaring a node name in
    hierarchy layout. Defaults to NULL.

## See also

Other mapping: `layout_configuration()`, `plot_graphr`, `plot_graphs`,
`plot_grapht`

## Examples

``` r
#graphr(ison_southern_women, layout = "concentric", membership = "type",
#           node_color = "type", node_size = 3)
#graphr(ison_lotr, layout = "multilevel",
#           node_color = "Race", level = "Race", node_size = 3)
# ison_adolescents %>%
#   mutate(year = rep(c(1985, 1990, 1995, 2000), times = 2),
#          cut = node_is_cutpoint(ison_adolescents)) %>%
#   graphr(layout = "lineage", rank = "year", node_color = "cut",
#              node_size = migraph::node_degree(ison_adolescents)*10)
#graphr(ison_southern_women, layout = "hierarchy", center = "events",
#           node_color = "type", node_size = 3)
#graphr(ison_southern_women, layout = "alluvial")
```
