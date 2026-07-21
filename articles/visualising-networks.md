# Visualising Networks

**Static preview**: This is a static, read-only preview of the
“Visualising Networks” tutorial, so the plots that the code produces
and the exercise solutions are not shown here, and the quizzes are
replaced with notes like this one. Install the package and run
`run_tute()` at the R console to work through the tutorial interactively
— running the code, seeing the graphs, and getting hints, solutions, and
quizzes.

## Why graph

Network visualisation is non-trivial; indeed it is very important, for
at least two reasons.

First, visualisation is a crucial part of the process of data analysis.
As a first step, network visualisation – or *graphing* – offers us a way
to vet our data for anything strange that might be going on, both
revealing and informing our assumptions and intuitions. The following
image relates to the famous [Anscombe’s
quartet](https://en.wikipedia.org/wiki/Anscombe%27s_quartet), which
shows how different datasets can have identical statistical properties
that are only revealed to be very different when graphed.

![animation of the datasaurus dozen: very different scatterplots with
identical summary
statistics](https://info5940.infosci.cornell.edu/notes/dataviz/why-visualize-data/index_files/figure-html/datasaurus-graph-1.gif)

As Tufte (1983: 9) said:

> “At their best, graphics are instruments for reasoning about
> quantitative information. Often the most effective way to describe,
> explore, and summarize a set of numbers – even a very large set – is
> to look at pictures of those numbers”

All of this is crucial with networks. Drawing network graphs is key to
exploring and understanding both the global structure of a network as
well as smaller-scale structures such as nodal positions or communities
within it.

Second, visualisation is a crucial part of communicating to others the
lessons that we have learned through investigation. As Brandes et al
([1999](https://doi.org/10.1177/0951692899011001004)) argue,
visualisation involves thinking about the *substance* of what you are
trying to communicate, how to *design* it so that it is ergonomic and
(ideally) aesthetic, and which *algorithm* is most appropriate to lay
out the graph informatively. The aim is to offer a concise and precise
delivery of insights.

There may be some dead-ends and time-sinks involved in visualising your
data, but it is worth taking the time to explore your data and
experiment with ways to make what you have learned over a longer period
of time evident to others in a shorter period of time.

**Catching up**: This tutorial assumes you know what a network is, made
up of nodes and ties , and that you can load or make network data in R.
If any of that is hazy, work through the
[manynet](https://stocnet.github.io/manynet/) tutorials first: run
`run_tute("Making")` and `run_tute("Manipulating")` at the R console, or
read their static versions on the [manynet
website](https://stocnet.github.io/manynet/).

**New to network vocabulary?**: Throughout this tutorial, key terms are
italicised: hover over them for a definition, and a full glossary of the
terms used appears at the end of the tutorial.

### Aims

By the end of this tutorial, you should be able to:

  -   Graph any compatible network with `graphr()` and read what the
    defaults show you
  -   Map node and tie attributes to colour, shape, size, and groups
  -   Set a consistent theme across all your plots, and tailor palettes
    for accessibility or print
  -   Add titles, labels, and legends that help others read your graph
  -   Choose an appropriate layout, and know when the distances between
    nodes can be interpreted
  -   Arrange multiple graphs together with `graphs()` and animate
    change over time with `grapht()`
  -   Plot centrality measures and other results with consistent
    `plot()` methods
  -   Export publication-ready figures with `ggsave()`

**Choose your own data**: The worked examples below mostly use
`fict_lotr`, a fictional network of affinities among Lord of the Rings
characters bundled with [manynet](https://stocnet.github.io/manynet/),
plus a couple of classical datasets. But wherever there is an exercise
box, you are encouraged to swap in a network that interests *you*.
Remember the three flavours of bundled data as a rough difficulty ladder
— **Classic** (`ison_*`, small & tidy), **Fiction** (`fict_*`, mid-sized
& fun), **Real-world** (`irps_*`, larger & realistic) — and that you can
browse the full list with `table_data()`.

![gif of Bob Ross painting a happy little
landscape](https://media1.tenor.com/m/gHo3jnYbDYwAAAAC/bob-ross-painting.gif)

## Getting started

On this page: Plotting approaches · Graphing approaches · Your first
graph

Before we start, let’s load the packages used in this tutorial.
[autograph](https://stocnet.github.io/autograph/) provides the graphing
and plotting functions (and loads
[manynet](https://stocnet.github.io/manynet/), which provides the
network data and manipulation verbs),
[netrics](https://stocnet.github.io/netrics/) provides the network
measures we will occasionally map onto graphs, and
[patchwork](https://patchwork.data-imaginist.com) lets us arrange
multiple plots together.

``` r
library(autograph)
library(netrics)
library(patchwork)
```

### Plotting approaches

To understand graph and network visualisation with
[autograph](https://stocnet.github.io/autograph/), it is useful to
review the different approaches already taken in R. Plotting in R is
typically based around two main approaches:

  - the ‘base’ approach in R by default, and
  - the ‘grid’ approach made popular by the famous and very flexible
    [ggplot2](https://ggplot2.tidyverse.org) package.[¹](#fn1)

In the case of base R graphics, plots are essentially written straight
to the plotting device. This means that they are not easily modified
after the fact: you would need to replot the whole thing to change
something. Moreover, while there is an admirably clean aesthetic to base
R graphics, it can be difficult to modify or extend them to your needs.

In the case of grid graphics, plots are built up in layers, and thus can
be modified after the fact. That is, you can initialise a plot using
`ggplot2::ggplot()`, specifying the data and mapping variables to
various aesthetic features, and then add layers to it using `+` to add
further points and lines, but also titles, legends, etc.

The following figure illustrates the difference between these two
approaches.[²](#fn2) **Run the code to compare the two plots.** (There
are buttons to run the code you have entered, to start over, and — where
available — to receive hints and solutions. You will use these
throughout the tutorial.)

``` r
plot(mtcars$hp, mtcars$mpg,
     main = "Base R: MPG vs Horsepower",
     xlab = "Horsepower",
     ylab = "Miles per Gallon",
     pch = 19,
     col = "blue")
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "blue") +
  labs(title = "ggplot2: MPG vs Horsepower",
       x = "Horsepower",
       y = "Miles per Gallon")
```

### Graphing approaches

Approaches to plotting *graphs* or *networks* in R can be similarly
divided:

  - two classic packages, [igraph](https://r.igraph.org/) and
    [sna](https://statnet.org), both build upon the ‘base’ R graphics
    engine,
  - newer packages
    [`{ggnetwork}`](https://www.r-bloggers.com/2016/03/ggnetwork-network-geometries-for-ggplot2/)
    and [`{ggraph}`](https://ggraph.data-imaginist.com/index.html) build
    upon a ‘grid’ approach.[³](#fn3)

Let’s see how the `fict_lotr` network would be plotted using
[igraph](https://r.igraph.org/) and
[ggraph](https://ggraph.data-imaginist.com), adding a title to each to
facilitate comparison, but otherwise relying on default behaviour.

``` r
plot(as_igraph(fict_lotr),
     main = "igraph: fict_lotr")
ggraph::ggraph(as_tidygraph(fict_lotr)) +
  ggraph::geom_edge_link() +
  ggraph::geom_node_point() +
  ggtitle("ggraph: fict_lotr")
```

We can see here that [igraph](https://r.igraph.org/) plots the network
in a fairly basic way, straight to the plotting device (window). By
default, it uses a force-directed layout (see the Layouts section
below),[⁴](#fn4) colors the nodes orange, and prints node labels if they
have them. However, the layout is not optimised for the size of the
plotting window, the node labels are regularly overlapping, and the
orange color with black borders is not particularly appealing or helpful
for label legibility. It only works with ‘igraph’ objects.

In contrast, [ggraph](https://ggraph.data-imaginist.com) offers the
trademark flexibility of the grammar of graphics approach. However, it
requires the user to build up a plot from the ground up, which can be
daunting for new users and fiddly even for experienced ones. Four lines
are required to get even a basic plot, with an additional line required
if a grey background is not desired. No labels or other information are
added by default, and would also require additional lines. It works with
‘tidygraph’ objects, which are an additional layer on top of ‘igraph’
objects.

### Your first graph

[autograph](https://stocnet.github.io/autograph/) builds upon these
packages, but takes a somewhat different approach. It builds upon the
‘grid’ approach of [ggplot2](https://ggplot2.tidyverse.org) and
[ggraph](https://ggraph.data-imaginist.com), lending itself to the
additional layering and flexibility of those packages. Because it
depends on the coercion routines available in
[manynet](https://stocnet.github.io/manynet/), it can be used with
network-related objects from most common network analysis packages —
igraph, network, tidygraph, matrices, edgelists, and more — without you
needing to convert anything first. Unlike those packages, though, it
offers concise and easy-to-use functions with sensible defaults for most
common use cases, using the information that is available in the network
object.

The first thing you will want to do when you import or create a new
network dataset is draw it. Compared to the
[igraph](https://r.igraph.org/) and
[ggraph](https://ggraph.data-imaginist.com) examples above,
`autograph::graphr()` offers a much more concise way to draw the
network. **Try it now.**

``` r
graphr(fict_lotr)
```

Note everything that happened without being asked: `graphr()` recognised
that the network is labelled and printed the node labels, chose a
deterministic layout (so you get the same picture every time), sized and
spaced the labels to minimise overlap, and dropped the axes and grey
background that mean nothing for networks. Because the network is
undirected , there are no arrowheads; for a directed network, `graphr()`
would draw them automatically.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

The package also offers methods for plotting statistics related to
networks (e.g. degree distributions ) and models of them
(e.g. goodness-of-fit plots). We will get to some of these later in
this tutorial, and others are demonstrated in the tutorials of other
`{stocnet}` packages. [autograph](https://stocnet.github.io/autograph/)
also offers consistent theming across graphs and plots, so that you do
not need to keep specifying the same options over and over again.

In the following pages, we’re going to go through a number of different
ways of taking control of the graphing process. Click ‘Next Topic’ to
continue.

**In brief**: `graphr()` graphs any manynet-compatible network object
with sensible defaults inferred from the data: labels where the network
is labelled, arrowheads where it is directed, a deterministic layout,
and no chart junk. It returns a [ggplot2](https://ggplot2.tidyverse.org)
object, so anything you can do to a ggplot — adding layers, titles,
scales with `+` — you can do to a graph.

## Illustrating graphs

On this page: Shaping · Colouring · Sizing · Ties · Arrows · Free play

Once we have an initial graph of our network, we can start to explore
features of the network and its structure in more detail. There are a
number of different dimensions network researchers can play with to
illustrate different aspects of the network. On [her excellent and
helpful website](https://kateto.net/network-visualization), Katya
Ognyanova outlines some of these dimensions:

| Nodes    |                               | Ties   |                               |
| :------- | :---------------------------- | :----- | :---------------------------- |
| Position | `layout=`                     | Arrows | (e.g. capped, head shape)     |
| Labels   | `labels=`, `node_group=`      | Type   | (e.g. solid, dashed)          |
| Shape    | `node_shape=`                 | Shape  | (e.g. straight, bent)         |
| Size     | `node_size=`                  | Size   | `edge_size=`                  |
| Color    | `node_color=`, `node_colour=` | Color  | `edge_color=`, `edge_colour=` |

Currently only those options with named parameters in the table above
are available to be customised in
[autograph](https://stocnet.github.io/autograph/). Tie arrows and shapes
are used to indicate directionality and reciprocity , where present in
the data.

Each of these arguments can be given either a literal value
(e.g. `node_size = 6`) or, more interestingly, the name of a node or
tie attribute in the data (e.g. `node_color = "Race"`), in which case
`graphr()` maps the attribute to that aesthetic and adds a legend where
appropriate. Let’s go through some of these options in more detail.

### Shaping nodes

One of the first things we might be interested in doing is understanding
better the distribution of some categorical variable. Our `fict_lotr`
dataset contains a variable called `Race`, so let’s try and change the
shape of the nodes by this variable. Following the syntax shown in the
table above, we just need to reference the variable name in the
`node_shape` argument. **Print the network first to check the
attribute’s (case-sensitive) name, then graph it.**

``` r
fict_lotr
graphr(fict_lotr, node_shape = "Race")
```

We can see here that there are six different races present.[⁵](#fn5)
Unfortunately, this is a few too many different categories to be
effectively distinguished by shape: at a glance, can you quickly find
the triangles among the squares? Shape works best for two or three
categories at most.

One place where shape excels, though, is distinguishing the node sets of
a multimodal network — and there `graphr()` does it for you. For a
two-mode network, nodes in the first mode are drawn as circles and nodes
in the second mode as squares, with a “Mode” legend added automatically;
were a third node set mapped to shape, it would be drawn as triangles.
**Graph the `ison_southern_women` network, where the women (first mode)
appear as circles and the events they attended (second mode) as
squares.**

``` r
graphr(ison_southern_women)
```

### Colouring nodes

![gif of an artist swirling paint colours together on a
palette](https://media1.tenor.com/m/AYxBhVmi0p4AAAAC/mixing-paint-peter-draws.gif)

Let’s try instead colouring the nodes by this “Race” variable. It is
very similar to the shape example above. **Can you complete the code
yourself?**

That’s much easier to read. Note how a legend has been added
automatically, using the colours of whatever theme is currently set
(more on themes soon).

How should we interpret this graph? Since the same colours seem to be
clustered together, with the humans and hobbits each clustered together
in the centre of the graph, and the elves clustered towards the left, we
might infer that there is some homophily going on here — that characters
tend to be connected to others of the same race — a hypothesis to test
properly in another tutorial. Interpreting an attribute-coloured graph
like this is often the first, informal step toward a more formal
analysis.

An alternative to colouring the nodes is to use the `node_group`
argument to highlight groups in a network. This puts a shaded area
around nodes of the same group. For rather spatially clustered
distributions, this can be a very effective way to show groupings, but
it is sensitive to the layout used: if nodes of the same group are not
close together, the shaded areas can overlap and make the graph harder
to read.

``` r
graphr(fict_lotr, node_group = "Race")
```

Note that `node_color` and `node_group` can be used together, either to
highlight different groupings, or to emphasise group assignment where
the groups interpenetrate, as described above.

### Sizing nodes

What about if we’re interested in a continuous variable instead of a
categorical variable? While the `fict_lotr` dataset does not contain any
continuous nodal variables, we can create one rather easily from the
network itself. Let’s use each node’s degree , which is the number of
ties incident/connecting to the node.

**Beginner note**: The `|>` symbol below is called a ‘pipe’. It passes
the result of the expression on its left on to the function on its
right, so the code below means “take `fict_lotr`, *then* add a Degree
attribute to its nodes, *then* graph it with node size mapped to that
attribute”. Piping or ‘chaining’ functions like this is very common in
modern R, and we use it throughout these tutorials. `mutate()` and the
other [dplyr](https://dplyr.tidyverse.org)-style verbs for networks are
covered in [manynet](https://stocnet.github.io/manynet/)’s “Manipulating
Network Data” tutorial.

``` r
fict_lotr |>
  mutate(Degree = node_by_deg(fict_lotr)) |>
  graphr(node_size = "Degree")
```

Larger nodes are now the better-connected characters, and a size legend
has been added. Who turns out to be the most connected character in the
fellowship?

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

### Tying up loose ends

All this works similarly with ties/edges. Just replace `node_` with
`edge_` in the arguments above, and you can control edges’ size and
color. In the following example, we add two tie attributes: a continuous
variable measuring how ‘close’ each tie is to others, and a binary
variable indicating whether the tie is part of a triangle or not, and
then colour the ties by the latter. **Run the code, then try colouring
or sizing the ties by `"weight"` instead.**

``` r
fict_lotr |>
  mutate_ties(weight = tie_by_closeness(fict_lotr),
              is_tri = tie_is_triangular(fict_lotr)) |>
  graphr(edge_color = "is_tri")
```

Note also that some tie attributes are recognised automatically: if a
network contains a tie attribute called `weight`, ties will be sized by
weight without you asking, and a tie attribute called `type` will be
used to distinguish tie types. Naming your attributes accordingly can
save you some typing.

### Pointing arrows

So far our example network has been undirected. For directed networks,
`graphr()` adds arrowheads automatically, pointing from the sender to
the receiver of each tie, and trims them back so they are not swallowed
by the receiving node. Arrowheads are also scaled automatically with the
width of the ties: thin ties get small arrowheads, thick ties get larger
(but capped) ones, and ties of width zero lose their arrowheads
entirely. This means arrowheads stay proportionate even when tie width
is mapped from a weight attribute, as in the `ison_networkers` network
of messages exchanged among early network researchers. You can also
scale them manually: because the arrowheads follow the tie width,
setting `edge_size` yourself resizes both together. **Compare the
automatic sizing with a manually thickened version.**

``` r
(graphr(ison_networkers) + ggtitle("Automatic") |
   graphr(ison_networkers, edge_size = 1) + ggtitle("Manual (edge_size = 1)"))
```

### Free play

**Your turn**: choose another network and illustrate something about it.
Pick a dataset with interesting node attributes — here is one suggestion
per flavour:

| Classic (small, easy)                       | Fiction (moderate)                             | Real-world (larger)                        |
| ------------------------------------------- | ---------------------------------------------- | ------------------------------------------ |
| `ison_lawfirm` (various partner attributes) | `fict_greys` (Grey’s Anatomy: sex, race, sign) | `irps_blogs` (US political blogs: leaning) |

Print the network first to see which attributes are available, then map
one or two of them to colour, shape, size, or groups.

**Going further**: Larger, denser networks like `irps_blogs` can turn
into a ‘hairball’ where the ties obscure everything. Two recent
additions to `graphr()` help: `edge_bundle = TRUE` bundles edges that
travel in similar directions (like cables tied together) so the main
‘highways’ of the network stand out, and the `isolates` argument
controls whether unconnected nodes are kept in the graph, moved to a
legend, or noted in a caption. See `?graphr` for the details.

**In brief**: `graphr()` maps node and tie attributes to visual
aesthetics by name: `node_color`, `node_shape`, `node_size`, and
`node_group` for nodes, `edge_color` and `edge_size` for ties. Use
colour or shape for categorical attributes (colour scales better), size
for continuous ones, and `node_group` to shade spatially clustered
memberships.

## Theming

On this page: Setting a theme · Hues · Grayscale · Manual override

### Setting a theme

Perhaps you are preparing a presentation, representing your institution,
department, or research centre at home or abroad. In this case, you may
wish to theme the whole network with institutional colors and fonts.
Indeed, you may even want to set a theme that is then reused across all
your graphs and plots. [autograph](https://stocnet.github.io/autograph/)
offers a number of themes that can be set using the `stocnet_theme()`
function. Once set, the theme applies to *every* subsequent graph and
plot in your session — no need to repeat yourself.

``` r
stocnet_theme("default")
graphr(fict_lotr, node_color = "Race")
stocnet_theme("iheid")
graphr(fict_lotr, node_color = "Race")
stocnet_theme("default")
```

Currently available themes include a number of institutional themes
(`"iheid"`, `"ethz"`, `"uzh"`, `"rug"`, `"unibe"`, `"oxf"`, `"unige"`,
`"cmu"`, `"iast"`, `"hwu"`) as well as stylistic ones (`"default"`,
`"bw"`, `"crisp"`, `"neon"`, `"rainbow"`). Run `stocnet_theme()` without
arguments to see which theme is currently set. More institutional scales
and themes can be implemented upon pull request.

### Who’s hue?

![gif from The Devil Wears Prada: that is not just blue, that is
cerulean](https://media1.tenor.com/m/wI7dn3jz6p8AAAAC/prada-cerulean.gif)

By default, `graphr()` will use a color palette that offers fairly good
contrast and better accessibility. However, a different hue might offer
a better aesthetic or identifiability for some nodes. Because the
`graphr()` function is based on the grammar of graphics, it’s easy to
extend or alter aesthetic aspects. Here let’s try and change the colors
assigned to the different races in the `fict_lotr` dataset. Note that
despite the argument being `node_color`, when overwriting the colors
please use functions of the type `ggplot2::scale_fill_*()`, as it is the
“fill” aesthetic that is being mapped to the variable in this case.

``` r
graphr(fict_lotr,
           node_color = "Race")

graphr(fict_lotr,
           node_color = "Race") +
  ggplot2::scale_fill_hue()
```

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

At this stage, it is worth noting that not everyone experiences colours
in the same way. Some people are colour-blind, whether by deuteranomaly,
deuteranopia, protanomaly, or protanopia, and so it is worth checking
that your visualisations are accessible to them.[⁶](#fn6) Others are
less sensitive to colour distinctions. The old trope is that males are
less sensitive to colour distinctions:[⁷](#fn7)

![comic strip about perceived colour vocabulary
differences](http://thedoghousediaries.com/dhdcomics/2010-03-01-12bf011.png)

### Grayscale

Other times color may not be desired. Some publications require
grayscale images. To use a grayscale color palette, replace `_hue` from
above with `_grey` (note the ‘e’ spelling):

``` r
graphr(fict_lotr,
           node_color = "Race") +
  ggplot2::scale_fill_grey()
```

As you can see, grayscale is more effective for continuous variables or
for very few discrete categories than for the six categories used here.
If you need to distinguish several categories in print, consider
combining grayscale with `node_shape`, or use the `"bw"` theme, which is
designed for this purpose.

### Manual override

Or we may want to choose particular colors for each category. This is
pretty straightforward to do with `ggplot2::scale_fill_manual()`. Some
common color names are available, but otherwise hex color codes can be
used for more specific colors. Unspecified categories are coloured
(dark) grey.

``` r
graphr(fict_lotr,
           node_color = "Race") +
  ggplot2::scale_fill_manual(
    values = c("Dwarf" = "red",
               "Hobbit" = "orange",
               "Maiar" = "#DEC20B",
               "Human" = "lightblue",
               "Elf" = "lightgreen",
               "Ent" = "darkgreen")) +
  labs(fill = "Color")
```

**In brief**: `stocnet_theme()` sets a theme once for all subsequent
graphs and plots, with institutional and stylistic palettes included.
Individual graphs can still be adjusted by appending
`ggplot2::scale_fill_*()` functions — `_hue()` for a different palette,
`_grey()` for print, `_manual()` for hand-picked colours — and it is
worth checking your palette is colour-blind accessible.

## Titles, labels, and legends

On this page: Labels · Titles · Legends

When it comes to communicating insights from network graphs to others,
it is important to add in the contextual information that will help them
understand what they are looking at. In this section, we will learn how
to add titles, labels, and legends to graphs.

### Labels

With our `fict_lotr` example above, because the network is itself
labelled, `graphr()` automatically adds the node labels. If you do not
want these labels, you can remove them from the network before passing
it on to `graphr()`, or more simply use the argument `labels = FALSE`.

``` r
graphr(fict_lotr, labels = FALSE)
```

Without the labels, the structure of the network is clearer and easier
to interpret, though we lose the information about which node is which
character. Which you prefer depends on what the graph is *for*:
exploring who-is-who, or communicating overall structure.

**Going further**: By default `graphr()` repels labels away from each
other and from nodes so that they do not overlap. Two further arguments
offer finer control: `label_repel = FALSE` places labels at a fixed
offset instead, and `label_dist` controls how far labels sit from their
nodes (in points). For crowded graphs, also consider labelling only some
nodes, e.g. `mutate(name = ifelse(node_is_max(node_by_deg(.)), name,
""))`.

### Titles

[autograph](https://stocnet.github.io/autograph/) works well with both
[ggplot2](https://ggplot2.tidyverse.org) and
[ggraph](https://ggraph.data-imaginist.com) functions that can be
appended to create more tailored visualisations. Let’s try this by
adding a title to a plot. **Append (with a `+`) `labs(title = )` to add
a title to a plot, say “My graph”, and then add also a subtitle (an
argument to that function), say “I did this”.**

Note that you can also use `ggtitle()` to do the same thing, but if you
just remember `labs()` you can also use it to add labels for *x* and *y*
axes, and legends (see below).

### Legends

While [autograph](https://stocnet.github.io/autograph/) attempts to
provide legends where necessary, in some cases the legends offer
insufficient detail, or are absent, such as in the following figure,
where we highlight the node with the highest betweenness centrality.

``` r
fict_lotr |>
  mutate(maxbet = node_is_max(node_by_betweenness(fict_lotr))) |>
  graphr(node_color = "maxbet")
```

Which node is highlighted here, and why might that be? Without a legend
title, a reader cannot know what the colour signifies.
[autograph](https://stocnet.github.io/autograph/) supports the
[ggplot2](https://ggplot2.tidyverse.org) way of adding legends after the
main plot has been constructed, using `guides()` to add in the legends,
and `labs()` for giving those legends particular titles. Note that we
can use `"\n"` within the legend title to make the title span multiple
lines.

``` r
fict_lotr |>
  mutate(maxbet = node_is_max(node_by_betweenness(fict_lotr))) |>
  graphr(node_color = "maxbet") +
  guides(color = "legend") +
  labs(color = "Maximum\nBetweenness")
```

To change the position of the legend, add the `theme()` function from
[ggplot2](https://ggplot2.tidyverse.org). The legend can be positioned
at the top, bottom, left, or right, or removed using “none”.

**In brief**: `labs()` adds titles, subtitles, and legend titles;
`guides()` forces or removes legends; `labels = FALSE` hides node
labels, and `label_repel`/`label_dist` fine-tune their placement. A
graph that leaves your hands should be readable without you standing
next to it explaining.

## Layouts

On this page: Force-directed · Layered · Circular · Spectral · Grid ·
Manual

The aim of graph layouts is to position nodes in a (usually)
two-dimensional space to maximise some analytic and aesthetically
pleasing function. Unlike the maps and scatterplots you may be used to,
*where* a node is drawn on a network graph is usually not data: it is
chosen by an algorithm to make the structure readable. Knowing which
algorithm — and what can and cannot be read off the result — is the
point of this section. Quality measures a layout algorithm might attend
to include:

  - minimising the *crossing number* of edges/ties in the graph ([planar
    graphs](https://www.jasondavies.com/planarity/) require no
    crossings)
  - minimising the *slope number* of distinct edge slopes in the graph
    (where vertices are represented as points on a Euclidean plane)
  - minimising the *bend number* in all edges in the graph (every graph
    has a right angle crossing (RAC) drawing with three bends per edge)
  - minimising the *total edge length*
  - minimising the *maximum edge length*
  - minimising the *edge length variance*
  - maximising the *angular resolution* or sharpest angle of edges
    meeting at a common vertex
  - minimising the *bounding box* of the plot
  - evening the *aspect ratio* of the plot
  - displaying *symmetry groups* (subgraph automorphisms)

Graph layouts available in the [igraph](https://r.igraph.org/),
[ggraph](https://ggraph.data-imaginist.com),
[graphlayouts](https://github.com/schochastics/graphlayouts), and
[autograph](https://stocnet.github.io/autograph/) packages can be used
in `graphr()`. These can be specified using the `layout` argument. For
these examples we will use `ison_southern_women`, a classical two-mode
network of women attending events, because two-mode networks make the
differences between layouts especially visible. In the following
sections, we review some of the most common types of layouts.

### Force-directed layouts

![gif of yoda moving things with the
force](https://giffiles.alphacoders.com/131/13131.gif)

Force-directed layouts update some initial placement of vertices through
the operation of some system of metaphorically-physical forces. These
might include attractive and repulsive forces.

``` r
(graphr(ison_southern_women, layout = "kk") + ggtitle("Kamada-Kawai") |
   graphr(ison_southern_women, layout = "fr") + ggtitle("Fruchterman-Reingold") |
   graphr(ison_southern_women, layout = "stress") + ggtitle("Stress Minimisation"))
```

The *Kamada-Kawai* (KK) method inserts a spring between all pairs of
vertices that is the length of the graph distance between them. This
means that edges with a large weight will be longer. KK offers a good
layout for lattice -like networks, because it will try to space the
network out evenly.

The *Fruchterman-Reingold* (FR) method uses an attractive force between
directly connected vertices, and a repulsive force between all vertex
pairs. The attractive force is proportional to the edge’s weight, thus
edges with a large weight will be shorter. FR offers a good baseline for
most types of networks.

The *Stress Minimisation* (stress) method is related to the KK
algorithm, but offers better runtime, quality, and stability and so is
generally preferred. Indeed,
[autograph](https://stocnet.github.io/autograph/) uses it as the default
for most networks. It has the advantage of returning the same layout
each time it is run on the same network.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

Other force-directed layouts available include:

  - Simulated annealing (Davidson and Harel 1993): `"dh"`
  - Graph embedder (Frick et al. 1995): `"gem"`
  - Graphopt (Schmuhl): `"graphopt"`
  - Distributed recursive graph layout (Martin et al. 2008): `"drl"`

### Layered layouts

Layered layouts arrange nodes into horizontal (or vertical) layers,
positioning them so that they reduce crossings. These layouts are best
suited for directed acyclic graphs, two-mode networks, or other data
with a natural hierarchy or ordering.

``` r
graphr(ison_southern_women, layout = "bipartite") + ggtitle("Bipartite")
graphr(ison_southern_women, layout = "hierarchy") + ggtitle("Hierarchy")
graphr(ison_southern_women, layout = "railway") + ggtitle("Railway")
```

Note that `"hierarchy"` and `"railway"` use a different algorithm to
[igraph](https://r.igraph.org/)’s `"bipartite"`, and generally perform
better, especially where there are multiple layers. Whereas
`"hierarchy"` tries to position nodes to minimise overlaps, `"railway"`
sequences the nodes in each layer to a grid so that nodes are matched as
far as possible. If you want to flip the horizontal and vertical, you
could flip the coordinates, or use something like the following layout.

``` r
graphr(ison_southern_women, layout = "alluvial") + ggtitle("Alluvial")
```

Other layered layouts include:

  - Tree: `"tree"`
  - Dominance layouts

### Circular layouts

Circular layouts arrange nodes around (potentially concentric) circles,
such that crossings are minimised and adjacent nodes are located close
together. In some cases, location or layer can be specified by attribute
or mode.

``` r
graphr(ison_southern_women, layout = "concentric") + ggtitle("Concentric")
```

Other such layouts include:

  - circular: `"circle"`
  - sphere: `"sphere"`
  - star: `"star"`
  - arc or linear layouts: `"linear"`

### Spectral layouts

Spectral layouts arrange nodes according to the eigenvalues of the
Laplacian matrix of a graph. These layouts tend to exaggerate the
clustering of like-nodes and the separation of less similar nodes in
two-dimensional space.

``` r
graphr(ison_southern_women, layout = "eigen") + ggtitle("Eigenvector")
```

Somewhat similar are multidimensional scaling (MDS) techniques, which
visualise the similarity between nodes in terms of their proximity in a
two-dimensional (or more) space.

``` r
graphr(ison_southern_women, layout = "mds") + ggtitle("Multidimensional Scaling")
```

Other such layouts include:

  - Pivot multidimensional scaling: `"pmds"`

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

### Grid layouts

![gif of a cartoon character energetically rearranging the living room
furniture](https://media1.tenor.com/m/06gR6YNAA6IAAAAd/family-guy-cartermiroquai.gif)

Grid layouts arrange nodes based on some Cartesian coordinates. These
can be useful for making sure all nodes’ labels are visible, but
horizontal and vertical lines can overlap, making it difficult to
distinguish whether some nodes are tied or not.

``` r
graphr(ison_southern_women, layout = "grid") + ggtitle("Grid")
```

Other grid layouts include:

  - orthogonal layouts for e.g. printed circuit boards
  - grid snapping for other layouts

### Manual layouts

Whatever their differences, all these layout algorithms do the same job:
they return a table of node coordinates. Nothing stops you computing
that table yourself, inspecting it, adjusting a coordinate or two, and
handing the result back to `graphr()` via its `x` and `y` arguments.
This is handy when a layout is *almost* right — say one label sits
awkwardly, or you want a particular node set apart — or when you need
the same hand-tuned positions across several figures. **Compute a stress
layout for `fict_lotr`, inspect the coordinate table, banish Gollum to
the top-right corner, and re-graph.**

``` r
lo <- ggraph::create_layout(as_tidygraph(fict_lotr), layout = "stress")
head(lo[, c("name", "x", "y")])
lo$x[lo$name == "Gollum"] <- max(lo$x) + 1
lo$y[lo$name == "Gollum"] <- max(lo$y) + 1
graphr(fict_lotr, x = lo$x, y = lo$y)
```

The same trick lets you reuse a layout across plots (compute once, pass
the same `x`/`y` to each call), which keeps node positions identical
between figures — useful when readers need to compare them.

**Going further**: `graphr()`’s `snap = TRUE` argument snaps any
layout’s coordinates to a grid, combining a familiar layout with the
label legibility of a grid.
[autograph](https://stocnet.github.io/autograph/) also provides its own
special-purpose layouts — `"configuration"`, `"lineage"`,
`"multilevel"`, `"triad"`/`"quad"`, and layouts that align nodes by
partition — documented at `?layout_partition` and friends.

**In brief**: Pass `layout =` to `graphr()` to choose among
force-directed (`"stress"`, `"fr"`, `"kk"`), layered (`"hierarchy"`,
`"railway"`, `"alluvial"`), circular (`"concentric"`, `"circle"`),
spectral (`"eigen"`, `"mds"`), and grid layouts. Force-directed layouts
are illustrative — do not over-interpret distances; spectral/MDS layouts
place nodes by measured similarity; layered layouts suit two-mode or
hierarchical data. And since every layout is just a table of
coordinates, you can always compute one with `ggraph::create_layout()`,
adjust it, and pass it back via `graphr()`’s `x` and `y` arguments.

## Multiple graphs

On this page: Arrangements · Sets · Dynamics

Sometimes one graph is not enough: we want to compare two networks,
several subgraphs, or the same network at different points in time.

### Arrangements

[autograph](https://stocnet.github.io/autograph/) uses the
[patchwork](https://patchwork.data-imaginist.com) package for arranging
graphs together, e.g. side-by-side or above one another. The syntax is
quite straightforward and is used throughout these vignettes/tutorials.
Basically, you just use `+` or `|` to put graphs side-by-side, and `/`
to put them above one another. Parentheses can be used to group graphs
together. **Try graphing `fict_lotr` and `ison_algebra` side-by-side,
and then one above the other.**

### Sets

`graphr()` is not the only graphing function included in
[autograph](https://stocnet.github.io/autograph/). To graph *sets* of
networks together, `graphs()` makes sure that two or more networks are
plotted together, using a consistent layout and theme across the panels
so that they can be compared. This might be a set of ego networks,
subgraphs , or waves of a longitudinal network.

``` r
graphs(to_subgraphs(fict_lotr, "Race"),
       waves = c(1,2,3,4))
```

What is happening here is that `to_subgraphs()` is creating a list of
subgraphs — one per race — and then `graphs()` is plotting them together
at once with the same set of aesthetic parameters. The `waves` argument
selects which networks in the list to plot — here the first four of the
six race subgraphs. Left to its own devices, `graphs()` plots just the
first and last networks of longer lists, which suits before-and-after
comparisons of longitudinal networks.

### Dynamics

![gif of a hand flipping through a flipbook of animated stick
figures](https://media1.tenor.com/m/eJEUysVdTxkAAAAd/calvin-and-hobbes-stick-figures-tiger-eating-man-this-was-my-book-stick-figures.gif)

`grapht()` is another alternative to `graphr()`, this time rendering
network changes over time as an animated gif. Longitudinal networks
(with discrete waves) and dynamic networks (with dated changes) are both
supported. Nodes appear, move, and fade as they enter and exit the
network, and node positions transition smoothly between waves. **Run the
following to animate a randomly-evolving version of our Lord of the
Rings network.** (Be patient — rendering an animation takes considerably
longer than drawing a static graph, and requires the suggested
[gganimate](https://gganimate.com) and
[gifski](https://r-rust.r-universe.dev/gifski) packages.)

``` r
fict_lotr |>
  mutate_ties(wave = sample(2001:2012, manynet::net_ties(fict_lotr), replace = TRUE)) |>
  to_waves(cumulative = TRUE) |>
  grapht()
```

Note that here, as with `weight` and `type` in the previous section,
attribute naming matters a little: a time attribute called `wave` marks
the network as longitudinal for
[manynet](https://stocnet.github.io/manynet/), so `to_waves()` (and
`grapht()` itself, passed such a network directly) will split it without
being told which attribute to use. From
[manynet](https://stocnet.github.io/manynet/) 2.2.2, any other name
(say, `year`) works just as well — it only needs declaring via
`to_waves()`’s `attribute` argument.

**In brief**: Combine individual graphs with
[patchwork](https://patchwork.data-imaginist.com) operators (`+`/`|`
beside, `/` above), graph lists of related networks with `graphs()` for
comparable panels, and animate longitudinal or dynamic networks with
`grapht()`.

## Going further with ggraph

![gif of Mr Bean taking the restoration of a painting into his own
hands](https://media1.tenor.com/m/MFmfCpzr4L0AAAAd/mr-bean-whistlers-mother.gif)

For more flexibility with visualisations,
[autograph](https://stocnet.github.io/autograph/) users are encouraged
to use the excellent [ggraph](https://ggraph.data-imaginist.com)
package. [ggraph](https://ggraph.data-imaginist.com) is built upon the
venerable [ggplot2](https://ggplot2.tidyverse.org) package and works
with `tbl_graph` and `igraph` objects. As with
[ggplot2](https://ggplot2.tidyverse.org),
[ggraph](https://ggraph.data-imaginist.com) users are expected to build
a particular plot from the ground up, adding explicit layers to
visualise the nodes and edges. This means more typing, but near-total
control.

``` r
library(ggraph)
ggraph(fict_greys, layout = "fr") +
  geom_edge_link(edge_colour = "dark grey",
                  arrow = arrow(angle = 45,
                                length = unit(2, "mm"),
                                type = "closed"),
                  end_cap = circle(3, "mm")) +
  geom_node_point(size = 2.5, shape = 19, colour = "blue") +
  geom_node_text(aes(label=name), family = "serif", size = 2.5) +
  scale_edge_width(range = c(0.3,1.5)) +
  theme_graph() +
  theme(legend.position = "none")
```

As we can see in the code above, we can specify various aspects of the
plot to tailor it to our network.

First, we can alter the **layout** of the network using the `layout =`
argument to create a clearer visualisation of the ties between nodes.
This is especially important for larger networks, where nodes and ties
are more easily obscured or misrepresented. In
[ggraph](https://ggraph.data-imaginist.com), the default layout is the
“stress” layout. The “stress” layout is a safe choice because it is
deterministic and fits well with almost any graph, but it is also a good
idea to explore and try out other layouts on your data. More layouts can
be found in the
[graphlayouts](https://github.com/schochastics/graphlayouts) and
[igraph](https://r.igraph.org/) R packages. To use a layout from the
[igraph](https://r.igraph.org/) package, enter only the last part of the
layout algorithm name (eg. `layout = "mds"` for “layout\_with\_mds”).

Second, using `geom_node_point()` which draws the nodes as geometric
shapes (circles, squares, or triangles), we can specify the presentation
of **nodes** in the network in terms of their *shape* (`shape=`, choose
from 1 to 21), *size* (`size=`), or *colour* (`colour=`). We can also
use `aes()` to match to node attributes. To add labels, use
`geom_node_text()` or `geom_node_label()` (draws labels within a box).
The font (`family=`), font size (`size=`), and colour (`colour=`) of the
labels can be specified.

Third, we can also specify the presentation of **edges** in the network.
To draw edges, we use `geom_edge_link0()` or `geom_edge_link()`. Using
the latter function makes it possible to draw a straight line with a
gradient. The following features can be tailored either globally or
matched to specific edge attributes using `aes()`:

  - *colour*: `edge_colour=`

  - *width*: `edge_width=`

  - *linetype*: `edge_linetype=`

  - *opacity*: `edge_alpha=`

For directed graphs, arrows can be drawn using the `arrow=` argument and
the `arrow()` function from [ggplot2](https://ggplot2.tidyverse.org).
The angle, length, arrowhead type, and padding between the arrowhead and
the node can also be specified.

For more see David Schoch’s [excellent resources on
this](http://mr.schochastics.net/netVizR.md).

**In brief**: Because `graphr()` returns a ggplot object, you can go a
long way just appending
[ggplot2](https://ggplot2.tidyverse.org)/[ggraph](https://ggraph.data-imaginist.com)
layers to it. When you need full control over every geom, build the plot
directly in [ggraph](https://ggraph.data-imaginist.com) — the skills
transfer directly, since
[autograph](https://stocnet.github.io/autograph/) uses
[ggraph](https://ggraph.data-imaginist.com) underneath.

## Plotting results

While researchers will probably want to start with using `graphr()` to
visualise the network itself,
[autograph](https://stocnet.github.io/autograph/) also offers `plot()`
methods for a number of different network-related objects, so that
`plot(result)` “just works” without you needing to remember a special
function for each object. These include measures of centrality,
cohesion, and clustering, as well as goodness-of-fit plots for network
models from packages such as
[RSiena](https://www.stats.ox.ac.uk/~snijders/siena/),
[ergm](https://statnet.org), and `{MoNAn}`. Usefully, all these plots
use the same theming system as `graphr()`, so that you can set a theme
once and have it apply to all your graphs and plots. **Let’s try this
now with a few examples, plotting the distributions of two centrality
measures under two themes.**

``` r
stocnet_theme("default")
plot(node_by_degree(fict_lotr)) +
plot(node_by_closeness(fict_lotr))
stocnet_theme("oxf")
plot(node_by_degree(fict_lotr)) +
plot(node_by_closeness(fict_lotr))
stocnet_theme("default")
```

Each plot shows the distribution of a node measure across the network —
here how unequal the characters’ degree and closeness centralities are.
This is a very simple example, but the same principle applies to all
plots in [autograph](https://stocnet.github.io/autograph/): one can set
a theme once and have it apply to all plots, and one can always add
additional [ggplot2](https://ggplot2.tidyverse.org) layers to any plot
to further customise it — titles and labels, but also trend lines,
confidence intervals, and so on. The plot methods for model results are
demonstrated in the tutorials of the packages that produce those
results.

## Exporting plots

![gif of a maker declaring that the masterpiece is done and it is time
to show the
world](https://media1.tenor.com/m/NitBp-Ag5dAAAAAd/alright-the-masterpiece-is-done-show-the-world.gif)

We can save the plots we have made by point-and-click by selecting ‘Save
as PDF…’ from under the ‘Export’ dropdown menu in the plots panel tab of
RStudio.

If you want to do this programmatically, say because you want to record
how you have saved it so that you can e.g. make some changes to the
parameters at some point, this is also not too difficult. After running
the (gg-based) plot you want to save, use `ggsave()` to save it to disk:

``` r
graphr(fict_lotr, node_color = "Race")
ggsave("lotr_race.pdf")
ggsave("lotr_race.png", width = 9, height = 6, dpi = 300)
```

`ggsave()` infers the file type from the extension (`.pdf`, `.png`,
`.jpeg`, `.svg`, …), saves to your working directory unless you specify
a path, and lets you fix the exact `width`, `height`, and resolution
(`dpi`) your publisher requires. For print, prefer vector formats
(`.pdf`, `.svg`), which stay sharp at any size; see `?ggsave` for more.

Animations made with `grapht()` are saved slightly differently: use
`gganimate::anim_save("my_animation.gif")`, which works just like
`ggsave()` but for the last animation rendered.

## Summary

![gif of an enthusiastic standing ovation and cries of
bravo](https://media1.tenor.com/m/gSsbNTouixUAAAAC/bravo-applause.gif)

Well done — you have completed the tutorial on visualising networks\!
Along the way, you have learned to use these functions:

| Function                                                  | What it does                                                 |
| --------------------------------------------------------- | ------------------------------------------------------------ |
| `graphr()`                                                | graphs any manynet-compatible network with sensible defaults |
| `graphr(..., node_color/node_shape/node_size/node_group)` | maps node attributes to aesthetics                           |
| `graphr(..., edge_color/edge_size)`                       | maps tie attributes to aesthetics                            |
| `graphr(..., labels, label_repel, label_dist)`            | controls node labelling                                      |
| `graphr(..., layout, snap)`                               | chooses and adjusts the layout algorithm                     |
| `graphr(..., x, y)`                                       | places nodes at manually supplied coordinates                |
| `ggraph::create_layout()`                                 | returns a layout’s table of node coordinates for tweaking    |
| `graphr(..., edge_bundle, isolates)`                      | tames large, dense, or disconnected networks                 |
| `stocnet_theme()`                                         | sets a consistent theme for all graphs and plots             |
| `ggplot2::scale_fill_hue()`, `_grey()`, `_manual()`       | overrides node colour palettes                               |
| `labs()`, `ggtitle()`, `guides()`                         | adds titles, axis and legend labels                          |
| `graphs()`                                                | graphs a list of networks as comparable panels               |
| `grapht()`                                                | animates a longitudinal or dynamic network as a gif          |
| `plot()`                                                  | plots measures, motifs, and model results consistently       |
| `ggsave()`                                                | exports the last plot at publication quality                 |

When you are ready, continue with the tutorials in the other `{stocnet}`
packages — on network structure and centrality in
[netrics](https://stocnet.github.io/netrics/), and on diffusion and
regression in [migraph](https://stocnet.github.io/migraph/) — where the
measures you can now visualise are properly introduced. Run `run_tute()`
at the console to see all available tutorials.

### Glossary

Here are some of the terms that we have covered in this tutorial:

  - Betweenness : The betweenness centrality of a node is the proportion
    of shortest paths between all pairs of nodes that pass through that
    node.
  - Closeness : The closeness centrality of a node is the reciprocal of
    the sum of its distances to all other nodes.
  - Community : A community is a set of nodes more densely connected to
    one another than to other nodes in the network.
  - Degree : The degree of a node is the number of connections it has.
  - Directed : A directed network is a network where the ties have a
    direction, from a sender to a receiver.
  - Distribution : A degree distribution is the frequency distribution
    of the degrees of the nodes in a network.
  - Homophily : A tendency for nodes to connect to similar nodes.
  - Label : A labelled network includes unique labels for each node (or
    ties) in the network.
  - Lattice : A network that can be drawn as a regular tiling.
  - Longitudinal : A longitudinal network is one observed in two or more
    discrete waves or panels over time.
  - Network : A network comprises one or more sets of nodes, one or more
    sets of ties among them, and potentially some node, tie, or
    network-level attributes.
  - Node : A node or vertex is an entity or actor within a network.
  - Reciprocity : A measure of how often nodes in a directed network are
    mutually linked.
  - Subgraph : A subgraph comprises a subset of the nodes and ties in a
    network.
  - Tie : A tie, edge, or link is a connection or relationship between
    two nodes.
  - Triangle : A cycle of length three in a network.
  - Twomode : A two-mode (or bipartite) network is a network with two
    different sets of nodes, where ties connect only nodes from
    different sets, such as people and the events they attend.
  - Undirected : An undirected or line network is one in which tie
    direction is undefined.
  - Weighted : A weighted network is where the ties have been assigned
    weights.

-----

1.  Perhaps of interest, `gg` stands for the Grammar of Graphics
    (<https://doi.org/10.1007/0-387-28695-0>).

2.  For more on the differences between base and grid graphics, see
    <https://flowingdata.com/2016/03/22/comparing-ggplot2-and-r-base-graphics/>.

3.  Others include: ‘Networkly’ for creating 2-D and 3-D interactive
    networks that can be rendered with plotly and can be easily
    integrated into shiny apps or markdown documents; ‘visNetwork’
    interacts with javascript (vis.js) to make interactive networks
    (<http://datastorm-open.github.io/visNetwork/>); and ‘networkD3’
    interacts with javascript (D3) to make interactive networks
    (<https://www.r-bloggers.com/2016/10/network-visualization-part-6-d3-and-r-networkd3/>).

4.  Which incidentally returns a different layout each time it is run.

5.  Though the keen-eyed and well-read among you will have noticed that
    there are some racial assignments that are debatable.

6.  The
    [viridis](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html)
    and
    [colorspace](http://colorspace.r-forge.r-project.org/articles/endrainbow.md)
    packages have excellent vignettes on this.

7.  Though see <https://blog.xkcd.com/2010/05/03/color-survey-results/>
    for a more nuanced take.
