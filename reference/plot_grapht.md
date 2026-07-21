# Easily animate dynamic networks with sensible defaults

This function provides users with an easy way to graph dynamic network
data for exploration and presentation.

It builds upon this package's `graphr()` function, and inherits all the
same features and arguments. See `graphr()` for more. However, it uses
the `{gganimate}` package to animate the changes between successive
iterations of a network. This is useful for networks in which the ties
and/or the node or tie attributes are changing, including networks whose
node composition changes over time: every node that ever appears is
assigned a stable position, and nodes fade in and out in place as they
enter and exit the network.

By default node positions transition smoothly between waves using the
dynamic stress layout from `{graphlayouts}`
(`graphlayouts::layout_as_dynamic()`), which anchors each wave's layout
to a reference layout of the aggregate network. The `alpha` argument
controls this trade-off: 0 lets each wave's layout follow that wave's
structure freely, while 1 freezes every node at its aggregate position.
When another `layout` is requested, a single static layout is computed
on the aggregate (union of waves) network instead, so that positions
remain constant. Unlike `graphr()`, `grapht()` uses this dynamic stress
layout by default even for two-mode networks (rather than a hierarchy
layout, which would collapse many nodes onto a line); the two modes
remain distinguishable by node shape. For networks with more than 30
nodes, node labels are suppressed by default to keep frames legible;
pass `labels = TRUE` to force them.

`grapht()` returns a `{ggplot2}`-compatible object that can be extended
with additional layers such as `ggplot2::labs()`, `ggplot2::theme()`,
scale functions, and others, just like plots produced by `graphr()` and
`graphs()`. The animation is rendered when the object is printed or
displayed. Users who want more control over animation parameters can
call `gganimate::animate()` directly on the returned object.

The visual appearance is consistent with `graphr()`: nodes use fillable
shapes with the fill aesthetic, the same colour palettes are applied,
directed networks receive arrowheads, signed networks distinguish
positive from negative ties by linetype, and labels use the current
theme font. Legends transition along with the mapped aesthetics.

A progress bar is shown if it takes some time to encode all the .png
files into a .gif.

## Usage

``` r
grapht(
  tlist,
  layout = NULL,
  labels = TRUE,
  node_color,
  node_shape,
  node_size,
  edge_color,
  edge_size,
  isolates = c("keep", "fade"),
  alpha = 0.5,
  label_dist = NULL,
  label_repel = TRUE,
  keep_isolates = NULL,
  ...,
  node_colour,
  edge_colour
)

# S3 method for class 'grapht'
print(x, ...)
```

## Source

https://blog.schochastics.net/posts/2021-09-15\_animating-network-evolutions-with-gganimate/

## Arguments

  - tlist:
    
    A manynet-compatible network listed according to a time attribute,
    waves, or slices. This can also be a single manynet network object
    that encodes time, which will be split automatically: longitudinal
    or changing networks are split into waves via `manynet::to_waves()`;
    dynamic (time-stamped, event-based) networks such as
    `manynet::irps_nuclear` into cumulative time slices via
    `manynet::to_slices()`; and interval (spell) networks that record
    tie `begin`/`end` lifespans, such as `manynet::irps_wwi`, into one
    snapshot per change point showing the ties active in that spell. It
    can also be a diffusion model result from e.g.
    `manynet::play_diffusion()`.

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
    
    One of `"keep"` (the default) or `"fade"`. `"keep"` retains isolated
    nodes at their layout positions in every wave in which they are
    present. `"fade"` fades nodes out during waves in which they are
    isolates, and fades them back in when they regain ties. Nodes that
    are absent from a wave altogether (composition change) always fade
    out.

  - alpha:
    
    A number between 0 and 1 controlling the stability of node positions
    across waves when the default dynamic (stress) layout is used. 0
    computes each wave's layout freely, 1 fixes all nodes at their
    aggregate-network positions. By default 0.5. Passed to
    `graphlayouts::layout_as_dynamic()`.

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

  - keep\_isolates:
    
    Deprecated. Use `isolates = "keep"` or `isolates = "fade"` instead.

  - ...:
    
    Extra arguments to pass on to the layout algorithm, if necessary.

  - x:
    
    A grapht object to print.

## Value

A `{ggplot2}`-compatible object with `{gganimate}` animation layers.
This object can be extended with additional `{ggplot2}` layers (e.g. `+
labs(subtitle = "My subtitle")`). When printed or displayed, the
animation is rendered as a .gif. For more control over animation
parameters, pass the result to `gganimate::animate()` directly.

## Details

Unlike `graphr()`, `grapht()` does not use `ggrepel`-based label
repelling (there is no straightforward way to repel labels consistently
across animation frames), so `label_repel` here instead toggles a fixed
offset nudging labels away from their nodes, and `label_dist` scales the
size of that nudge rather than being used as repel padding.

Some further `graphr()` features are not available in animations:
`node_group` hulls, edge bundling, curved arcs for reciprocated ties,
and self-loops (loops are not drawn; a note is printed if present).

## See also

Other mapping: `layout_configuration()`, `layout_partition`,
`plot_graphr`, `plot_graphs`

## Examples

``` r
# A dynamic signed network of shifting European alliances 1872-1918,
# split automatically into snapshots of the ties active in each spell:
grapht(irps_wwi)
```
