# Easily graph networks with sensible defaults

This function provides users with an easy way to graph (m)any network
data for exploration, investigation, inspiration, and communication.

`graphr()` builds upon `{ggplot2}` and `{ggraph}` to offer pretty, easy,
and extensible graphing solutions. Just passing the function some
network data will often be sufficient to return a reasonable-looking
graph.

The function also makes it easy to modify many of the most commonly
adapted aspects of a graph, including node and edge size, colour, and
shape, as arguments rather than additional functions that you need to
remember. These can be defined outright, e.g. `node_size = 8`, or in
reference to an attribute of the network, e.g. `node_size = "wealth"`.

Lastly, `graphr()` uses `{ggplot2}`-related theme information, so it is
easy to make colour palette and fonts institution-specific and
consistent. See e.g. `theme_iheid()` for more.

To learn more about what can be done visually, try
`run_tute("Visualisation")`.

## Usage

``` r
graphr(
  .data,
  layout = NULL,
  labels = TRUE,
  node_color,
  node_shape,
  node_size,
  node_group,
  edge_color,
  edge_size,
  isolates = c("legend", "caption", "keep"),
  snap = FALSE,
  label_dist = NULL,
  label_repel = TRUE,
  edge_bundle = FALSE,
  ...,
  node_colour,
  edge_colour
)
```

## Arguments

  - .data:
    
    A manynet-consistent object.

  - layout:
    
    An igraph, ggraph, or manynet layout algorithm. If not declared,
    defaults to "triad" for networks with 3 nodes, "quad" for networks
    with 4 nodes, "stress" for all other one mode networks, or
    "hierarchy" for two mode networks. For "hierarchy" layout, one can
    further split graph by declaring the "center" argument as the
    "events", "actors", or by declaring a node name. For "concentric"
    layout algorithm please declare the "membership" as an extra
    argument. The "membership" argument expects either a quoted node
    attribute present in data or vector with the same length as nodes to
    draw concentric circles. For "multilevel" layout algorithm please
    declare the "level" as extra argument. The "level" argument expects
    either a quoted node attribute present in data or vector with the
    same length as nodes to hierarchically order categories. If "level"
    is missing, function will look for 'lvl' node attribute in data. The
    "lineage" layout ranks nodes in Y axis according to values. For
    "lineage" layout algorithm please declare the "rank" as extra
    argument. The "rank" argument expects either a quoted node attribute
    present in data or vector with the same length as nodes.

  - labels:
    
    Logical, whether to print node names as labels if present.

  - node\_color, node\_colour:
    
    Node variable to be used for coloring the nodes. It is easiest if
    this is added as a node attribute to the graph before plotting.
    Nodes can also be colored by declaring a color instead.

  - node\_shape:
    
    Node variable to be used for shaping the nodes. It is easiest if
    this is added as a node attribute to the graph before plotting.
    Nodes can also be shaped by declaring a shape instead.

  - node\_size:
    
    Node variable to be used for sizing the nodes. This can be any
    continuous variable on the nodes of the network. Since this function
    expects this to be an existing variable, it is recommended to
    calculate all node-related statistics prior to using this function.
    Nodes can also be sized by declaring a numeric size or vector
    instead.

  - node\_group:
    
    Node variable to be used for grouping the nodes. It is easiest if
    this is added as a hull over groups before plotting. Group variables
    should have a minimum of 3 nodes, if less, number groups will be
    reduced by merging categories with lower counts into one called
    "other".

  - edge\_color, edge\_colour:
    
    Tie variable to be used for coloring the nodes. It is easiest if
    this is added as an edge or tie attribute to the graph before
    plotting. Edges can also be colored by declaring a color instead.

  - edge\_size:
    
    Tie variable to be used for sizing the edges. This can be any
    continuous variable on the nodes of the network. Since this function
    expects this to be an existing variable, it is recommended to
    calculate all edge-related statistics prior to using this function.
    Edges can also be sized by declaring a numeric size or vector
    instead.

  - isolates:
    
    Character scalar, how to treat isolates. "keep" will keep isolates
    in the graph as they are. "legend" (default) will remove isolates
    from the graph but note them in the legend. "caption" will remove
    isolates from the graph but note them in the caption. If there are
    no isolates, this argument will be ignored. If the default layout
    ("stress") is used, we recommend that the "legend" option is used to
    avoid isolates crowding out the giant component.

  - snap:
    
    Logical scalar, whether the layout should be snapped to a grid.

  - label\_dist:
    
    Numeric scalar, in points (pt), controlling the extra gap left
    between labels and node borders – similar to `igraph`'s
    `vertex.label.dist`. Node size is always accounted for automatically
    (larger nodes push labels further away without any extra
    configuration); `label_dist` adds further spacing on top of that,
    and defaults to a small gap (5pt). Set to `0` for labels right at
    the node border, or to a larger value (e.g. `15`) for more spacing.
    Only used when `labels = TRUE` and `label_repel = TRUE` (as the
    padding passed to the repel algorithm) or `label_repel = FALSE` (as
    a fixed nudge away from the node, in the layouts where this makes
    sense, e.g. "circle"/"concentric", "bipartite"/"railway",
    "alluvial").

  - label\_repel:
    
    Logical scalar, whether labels should be repelled away from each
    other and from nodes using `ggrepel` (via `ggraph`'s `repel`
    argument). Defaults to `TRUE`. Set to `FALSE` to place labels at a
    fixed offset (see `label_dist`) without the (sometimes slow, and
    non-deterministic between runs for some layouts) repelling
    algorithm.

  - edge\_bundle:
    
    Edge bundling, off by default (`FALSE`). When `TRUE` (or
    equivalently `"force"`), edges are bundled together using ggraph's
    force-directed edge bundling (`geom_edge_bundle_force()`), which
    pulls nearby edges into shared paths to reduce visual clutter in
    dense networks. Alternative non-hierarchical algorithms can be
    selected by name: `"path"` (`geom_edge_bundle_path()`) or
    `"minimal"` (`geom_edge_bundle_minimal()`). Bundling only makes a
    visible difference when a network has enough edges; for directed
    networks arrowheads are retained, but the slight reciprocal-tie
    curvature used for unbundled edges does not apply.

  - ...:
    
    Extra arguments to pass on to the layout algorithm, if necessary.

## Value

A `ggplot2::ggplot()` object. The last plot can be saved to the file
system using `ggplot2::ggsave()`.

## See also

Other mapping: `layout_configuration()`, `layout_partition`,
`plot_graphs`, `plot_grapht`

## Examples

``` r
graphr(ison_adolescents)

ison_adolescents %>%
  mutate(color = rep(c("introvert","extrovert"), times = 4),
         size = ifelse(netrics::node_is_cutpoint(ison_adolescents), 6, 3)) %>%
  mutate_ties(ecolor = rep(c("friends", "acquaintances"), times = 5)) %>%
  graphr(node_color = "color", node_size = "size",
         edge_size = 1.5, edge_color = "ecolor")

graphr(ison_southern_women, labels = TRUE, label_dist = 10)

graphr(ison_southern_women, labels = TRUE, label_repel = FALSE)

graphr(manynet::generate_random(40, 0.1), edge_bundle = TRUE)
```
