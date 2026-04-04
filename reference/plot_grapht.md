# Easily animate dynamic networks with sensible defaults

This function provides users with an easy way to graph dynamic network
data for exploration and presentation.

It builds upon this package's `graphr()` function, and inherits all the
same features and arguments. See `graphr()` for more. However, it uses
the `{gganimate}` package to animate the changes between successive
iterations of a network. This is useful for networks in which the ties
and/or the node or tie attributes are changing.

`grapht()` returns a `{ggplot2}`-compatible object that can be extended
with additional layers such as `ggplot2::labs()`, `ggplot2::theme()`,
scale functions, and others, just like plots produced by `graphr()` and
`graphs()`. The animation is rendered when the object is printed or
displayed. Users who want more control over animation parameters can
call `gganimate::animate()` directly on the returned object.

The visual appearance is consistent with `graphr()`: nodes use fillable
shapes with the fill aesthetic, the same colour palettes are applied,
and labels use the current theme font.

A progress bar is shown if it takes some time to encode all the .png
files into a .gif.

## Usage

``` r
grapht(
  tlist,
  keep_isolates = TRUE,
  layout = NULL,
  labels = TRUE,
  node_color,
  node_shape,
  node_size,
  edge_color,
  edge_size,
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
    waves, or slices. This can also be a diffusion model result from
    e.g. `manynet::play_diffusion()`.

  - keep\_isolates:
    
    Logical, whether to keep isolate nodes in the graph. TRUE by
    default. If FALSE, removes nodes from each frame they are isolated
    in.

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

## See also

Other mapping: `layout_configuration()`, `layout_partition`,
`plot_graphr`, `plot_graphs`

## Examples

``` r
#ison_adolescents %>%
#  mutate_ties(year = sample(1995:1998, 10, replace = TRUE)) %>%
#  to_waves(attribute = "year", cumulative = TRUE) %>%
#  grapht()
#ison_adolescents %>% 
#  mutate(gender = rep(c("male", "female"), times = 4),
#         hair = rep(c("black", "brown"), times = 4),
#         age = sample(11:16, 8, replace = TRUE)) %>%
#  mutate_ties(year = sample(1995:1998, 10, replace = TRUE),
#              links = sample(c("friends", "not_friends"), 10, replace = TRUE),
#              weekly_meetings = sample(c(3, 5, 7), 10, replace = TRUE)) %>%
#  to_waves(attribute = "year") %>%
#  grapht(layout = "concentric", membership = "gender",
#             node_shape = "gender", node_color = "hair",
#             node_size =  "age", edge_color = "links",
#             edge_size = "weekly_meetings")
#grapht(play_diffusion(ison_adolescents, seeds = 5))
```
