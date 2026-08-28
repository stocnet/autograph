# Easily animate dynamic networks with sensible defaults

This function provides users with an easy way to graph dynamic network
data for exploration and presentation.

It builds upon this package's
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
function, and inherits all the same features and arguments. See
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
for more. However, it uses the `{gganimate}` package to animate the
changes between successive iterations of a network. This is useful for
networks in which the ties and/or the node or tie attributes are
changing, including networks whose node composition changes over time:
every node that ever appears is assigned a stable position, and nodes
fade in and out in place as they enter and exit the network.

By default node positions transition smoothly between waves using the
dynamic stress layout from `{graphlayouts}`
([`graphlayouts::layout_as_dynamic()`](https://schochastics.github.io/graphlayouts/reference/layout_dynamic.html)),
which anchors each wave's layout to a reference layout of the aggregate
network. The `alpha` argument controls this trade-off: 0 lets each
wave's layout follow that wave's structure freely, while 1 freezes every
node at its aggregate position. When another `layout` is requested, a
single static layout is computed on the aggregate (union of waves)
network instead, so that positions remain constant. Unlike
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md),
`grapht()` uses this dynamic stress layout by default even for two-mode
networks (rather than a layered layout, which would collapse many nodes
onto a line); the two modes remain distinguishable by node shape. For
networks with more than 30 nodes, node labels are suppressed by default
to keep frames legible; pass `labels = TRUE` to force them.

`grapht()` returns a `{ggplot2}`-compatible object that can be extended
with additional layers such as
[`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html),
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html),
scale functions, and others, just like plots produced by
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
and
[`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md).
The animation is rendered when the object is printed or displayed. Users
who want more control over animation parameters can call
[`gganimate::animate()`](https://gganimate.com/reference/animate.html)
directly on the returned object.

The visual appearance is consistent with
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md):
nodes use fillable shapes with the fill aesthetic, the same colour
palettes are applied, directed networks receive arrowheads, signed
networks distinguish positive from negative ties by linetype, and labels
use the current theme font. Legends transition along with the mapped
aesthetics.

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

https://blog.schochastics.net/posts/2021-09-15_animating-network-evolutions-with-gganimate/

## Arguments

- tlist:

  A manynet-compatible network listed according to a time attribute,
  waves, or slices. This can also be a single manynet network object
  that encodes time, which will be split automatically: longitudinal or
  changing networks are split into waves via
  [`manynet::to_waves()`](https://stocnet.github.io/manynet/reference/modif_split.html);
  dynamic (time-stamped, event-based) networks such as
  [`manynet::irps_nuclear`](https://stocnet.github.io/manynet/reference/irps_nuclear.html)
  into cumulative time slices via
  [`manynet::to_slices()`](https://stocnet.github.io/manynet/reference/modif_split.html);
  and interval (spell) networks that record tie `begin`/`end` lifespans,
  such as
  [`manynet::irps_wwi`](https://stocnet.github.io/manynet/reference/irps_wwi.html),
  into one snapshot per change point showing the ties active in that
  spell. It can also be a diffusion model result from e.g.
  [`manynet::play_diffusion()`](https://stocnet.github.io/manynet/reference/make_play.html).

- layout:

  An igraph, ggraph, or manynet layout algorithm. If not declared,
  defaults to "configuration" for networks of up to six nodes, "levels"
  for connected multilevel networks, "layered" for other two mode
  networks, and "stress" for all other networks. For "layered" layout,
  one can further split graph by declaring the "center" argument as the
  "events", "actors", or by declaring a node name. For "concentric"
  layout algorithm please declare the "membership" as an extra argument.
  The "membership" argument expects either a quoted node attribute
  present in data or vector with the same length as nodes to draw
  concentric circles. For "levels" layout algorithm one may declare the
  "level" as extra argument. The "level" argument expects either a
  quoted node attribute present in data or vector with the same length
  as nodes to hierarchically order categories. If "level" is missing,
  the levels are taken from a 'lvl' node attribute where there is one,
  or else from the two modes of a two mode network. The layered layouts
  ("layered", "lineage", "railway" and "ladder") accept a "ranks"
  argument, which takes either one of the methods named at
  [`?layout_layered`](https://stocnet.github.io/autograph/reference/layout_layered.md)
  or a numeric node attribute to lay the layers out by, as a quoted
  attribute name or a vector with one value for each node. The "scaling"
  layout places the nodes by multidimensional scaling, so that the
  distance between two nodes approximates the number of steps between
  them. Since those coordinates can be read, this layout is drawn with
  labelled axes on one scale, and captioned with how well two dimensions
  hold the distances; see
  [`?layout_scaling`](https://stocnet.github.io/autograph/reference/layout_scaling.md)
  and
  [`check_stress()`](https://stocnet.github.io/autograph/reference/check_layout.md).
  Note that those axes carry distances rather than named dimensions: the
  drawing can be turned or mirrored without fitting the network any
  better or any worse. The "correspondence" layout places the nodes by
  correspondence analysis, so that two nodes with similar ties are drawn
  together, whether or not they are tied to each other. It is the usual
  way to draw a two mode network, since it places both modes against the
  same pair of axes, and it accepts a "direction" argument for a
  directed network and a "double" argument for a signed one; see
  [`?layout_correspondence`](https://stocnet.github.io/autograph/reference/layout_correspondence.md).
  Each axis names the share of the network's inertia that it holds.

- labels:

  Which nodes to label, if the network is labelled. `TRUE` (the default)
  labels every node and `FALSE` none of them, but a label for every node
  of a large network hides the network behind them, so a *selection* of
  the nodes can be given instead:

  - a number, e.g. `labels = 5`, labels the nodes within the top five
    ranks by degree. Note that this is a depth of ranks rather than a
    count of nodes: nodes tied at the cut are labelled together, so more
    than five labels may appear.

  - a measure to rank by, e.g. `labels = "betweenness"`, labels just the
    node or nodes that measure singles out. `"degree"`, `"betweenness"`,
    `"cutpoints"` (every node the mark flags) and `"random"` (a small
    random sample) are available. The two can be combined by naming the
    number, as in `labels = c(betweenness = 5)`.

  - the name of a logical node attribute, e.g. `labels = "is_broker"`,
    labels the nodes it marks.

  - a logical vector, one value per node, e.g.
    `labels = netrics::node_is_cutpoint(net)`; or the names or positions
    of the nodes to label, e.g. `labels = c("Alice", "Betty")`.

  Where a length-one string could mean more than one of these, a node
  attribute is preferred to a measure, and a measure to a node name. A
  single number is always read as a depth of ranks rather than as one
  node's position, so a lone node is best named, as in
  `labels = "Alice"`. For networks of more than 30 nodes, `labels`
  defaults to a selection rather than to every node; pass
  `labels = TRUE` for all of them. Ranking nodes uses the `{netrics}`
  package, which is suggested rather than required: without it
  installed, an automatic selection falls back to a random sample.
  Two-mode and multilevel networks are ranked within each mode or level,
  so that every level is labelled and not just the densest.

- node_color, node_colour:

  Node variable to be used for coloring the nodes. It is easiest if this
  is added as a node attribute to the graph before plotting. A
  categorical variable gives one colour to each category. A measure,
  such as a centrality or coreness score, is drawn instead as a gradient
  from the theme's base colour to its highlight colour, with a colourbar
  in place of the legend. Nodes can also be colored by declaring a color
  instead.

- node_shape:

  Node variable to be used for shaping the nodes. It is easiest if this
  is added as a node attribute to the graph before plotting. Nodes can
  also be shaped by declaring a shape instead.

- node_size:

  Node variable to be used for sizing the nodes. This can be any
  continuous variable on the nodes of the network. Since this function
  expects this to be an existing variable, it is recommended to
  calculate all node-related statistics prior to using this function.
  Nodes can also be sized by declaring a numeric size or vector instead.

- edge_color, edge_colour:

  Tie variable to be used for coloring the nodes. It is easiest if this
  is added as an edge or tie attribute to the graph before plotting.
  Edges can also be colored by declaring a color instead.

- edge_size:

  Tie variable to be used for sizing the edges. This can be any
  continuous variable on the nodes of the network. Since this function
  expects this to be an existing variable, it is recommended to
  calculate all edge-related statistics prior to using this function.
  Edges can also be sized by declaring a numeric size or vector instead.

- isolates:

  One of `"keep"` (the default) or `"fade"`. `"keep"` retains isolated
  nodes at their layout positions in every wave in which they are
  present. `"fade"` fades nodes out during waves in which they are
  isolates, and fades them back in when they regain ties. Nodes that are
  absent from a wave altogether (composition change) always fade out.

- alpha:

  A number between 0 and 1 controlling the stability of node positions
  across waves when the default dynamic (stress) layout is used. 0
  computes each wave's layout freely, 1 fixes all nodes at their
  aggregate-network positions. By default 0.5. Passed to
  [`graphlayouts::layout_as_dynamic()`](https://schochastics.github.io/graphlayouts/reference/layout_dynamic.html).

- label_dist:

  Numeric scalar, in points (pt), controlling the extra gap left between
  labels and node borders – similar to `igraph`'s `vertex.label.dist`.
  Node size is always accounted for automatically (larger nodes push
  labels further away without any extra configuration); `label_dist`
  adds further spacing on top of that, and defaults to a small gap
  (5pt). Set to `0` for labels right at the node border, or to a larger
  value (e.g. `15`) for more spacing. Only used when `labels = TRUE` and
  `label_repel = TRUE` (as the padding passed to the repel algorithm) or
  `label_repel = FALSE` (as a fixed nudge away from the node, in the
  layouts where this makes sense, e.g. "circle"/"concentric", "railway",
  "lineage").

- label_repel:

  Logical scalar, whether labels should be repelled away from each other
  and from nodes using `ggrepel` (via `ggraph`'s `repel` argument).
  Defaults to `TRUE`. Set to `FALSE` to place labels at a fixed offset
  (see `label_dist`) without the (sometimes slow, and non-deterministic
  between runs for some layouts) repelling algorithm. The layered
  layouts ("layered", "lineage", "railway" and "ladder") place each node
  in a layer, which is where the reader looks for it, so a repelled
  label there would say less about which node it labels than a fixed
  offset does. They ignore this argument and always offset.

- keep_isolates:

  Deprecated. Use `isolates = "keep"` or `isolates = "fade"` instead.

- ...:

  Extra arguments to pass on to the layout algorithm, if necessary.

- x:

  A grapht object to print.

## Value

A `{ggplot2}`-compatible object with `{gganimate}` animation layers.
This object can be extended with additional `{ggplot2}` layers (e.g.
`+ labs(subtitle = "My subtitle")`). When printed or displayed, the
animation is rendered as a .gif. For more control over animation
parameters, pass the result to
[`gganimate::animate()`](https://gganimate.com/reference/animate.html)
directly.

## Details

Unlike
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md),
`grapht()` does not use `ggrepel`-based label repelling (there is no
straightforward way to repel labels consistently across animation
frames), so `label_repel` here instead toggles a fixed offset nudging
labels away from their nodes, and `label_dist` scales the size of that
nudge rather than being used as repel padding.

`labels` can select which nodes to label here too, and the selection is
resolved once over all the waves so that the same nodes stay labelled
from frame to frame. Unlike
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md),
though, animations of more than 30 nodes default to no labels at all
rather than to a selection of them.

Some further
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
features are not available in animations: `node_group` hulls, edge
bundling, curved arcs for reciprocated ties, and self-loops (loops are
not drawn; a note is printed if present). Note too that, where no
`layout` is named, `grapht()` defaults to the "stress" layout for every
network rather than choosing one by the network's shape as
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
does, so that nodes move smoothly from one wave to the next. A layout
named explicitly is still used, computed on the aggregate network.

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
[`plot_graphs`](https://stocnet.github.io/autograph/reference/plot_graphs.md)

## Examples

``` r
# A dynamic signed network of shifting European alliances 1872-1918,
# split automatically into snapshots of the ties active in each spell.
# Wrapped in \donttest{} because rendering the animation to a .gif is
# slow, not because the code is unsafe to run.
# \donttest{
grapht(irps_wwi)
# }
```
