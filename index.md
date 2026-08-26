# autograph

## About the package

This package aims to make network visualisation *easier*, *succinct*,
and *consistent*. Visualisation is a key part of the research process,
from the initial exploration of data to the analysis of results and the
presentation of findings in publications. However, it is often a tedious
and time-consuming task. Trying to wrangle these into a consistent style
for publication or presentation can be frustrating and requires a lot of
code. While there are a number of excellent packages for network
analysis in R, they each face several of the following challenges when
it comes to visualisation:

- defaults are often not sensible for different types of networks
- customisation can sometimes be difficult
- some require multiple lines of code to even produce a graph or plot
- most require multiple lines of code to produce a graph or plot that is
  styled suitable for publication or presentation
- such style code needs to be repeated every time a graph or plot is
  produced if a consistent style is to be maintained
- defaults and syntax are different for different packages, so a
  workflow using multiple packages must adapt to multiple syntaxes
- different visual defaults can frustrate interpretation, and
  potentially invites errors when comparing plots from different
  packages
- some plotting methods are available for some networks or
  network-related results and not others

[autograph](https://stocnet.github.io/autograph/) aims to solve these
problems by providing automatic graph drawing for networks in any of the
[manynet](https://stocnet.github.io/manynet/) formats, and automatic
plotting for results from `{stocnet}` packages, including
[migraph](https://stocnet.github.io/migraph/),
[RSiena](https://www.stats.ox.ac.uk/~snijders/siena/), and `{MoNAn}`,
and more.

All you need to do is install the package (loading it last will make
sure its plotting methods are the default), use
[`set_stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
(once) to set your preferred theme, and then use
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
to graph your networks, or
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) to plot your
results. That’s it!

## Drawing graphs

[autograph](https://stocnet.github.io/autograph/) includes three
one-line graphing functions with sensible defaults based on the
network’s properties.

First,
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
is used to graph networks in any of the
[manynet](https://stocnet.github.io/manynet/) formats. Because it builds
upon [manynet](https://stocnet.github.io/manynet/), it can graph
networks in any of the [manynet](https://stocnet.github.io/manynet/)
formats, including `network`, `igraph`, `sna`, `tidygraph`, and more.

Second, it includes sensible defaults so that researchers can view their
network’s structure or distribution quickly with a minimum of fuss.
Compare the output from
[autograph](https://stocnet.github.io/autograph/) with a similar default
from [igraph](https://r.igraph.org/):

![Example illustrating differences in default igraph and autograph
graphs](https://www.jameshollway.com/post/autograph/README-layout-comparison-1.png)

[igraph](https://r.igraph.org/) requires the bipartite layout to be
specified, has cumbersome node size defaults for all but the smallest
graphs, and labels also very often need resizing and adjustment to avoid
overlap. Getting this default plot to look good can take a lot of trial
and error, and time. By contrast,
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
recognises the network as two-mode and uses a bipartite layout by
default. It also recognises that the network contains names for the
nodes and prints them vertically so that they are legible in this
layout. Other ‘clever’ features include automatic node sizing and more.

This inference matters for more than tidiness. Where a default does not
recognise a property of the network, that property is usually dropped
silently. Compare the same signed network drawn by each package:

![Example illustrating that igraph's default draws positive and negative
ties
identically](https://www.jameshollway.com/post/autograph/README-signed-comparison-1.png)

`irps_tribes` records both alliance and antagonism between sixteen
tribes, in equal number. [igraph](https://r.igraph.org/) draws all of
these ties identically, so the distinction that motivates the data is
not visible.
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
recognises the network as signed and maps the sign to both colour and
linetype, with a legend. The same applies to weights, to self-ties, and
to direction:
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
reads these from the network rather than requiring you to know to ask
for them.

### More options

All of
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)’s
adjustments can be overridden, however… Changing the size and colors of
nodes and ties is as easy as specifying the function’s relevant argument
with a replacement, e.g. `node_color = "darkblue"` or `node_size = 6`,
or indicating from which attribute it should inherit this information,
e.g. `node_color = "Office"` or `node_size = "Seniority"`.

![Graph illustrating automatic and manual use of node color and
size](https://www.jameshollway.com/post/autograph/README-more-options-1.png)

Legends are added by default when node or tie aesthetics are mapped to
attributes, but can be removed with `show_legend = FALSE`. Since the
[autograph](https://stocnet.github.io/autograph/) builds upon
[ggplot2](https://ggplot2.tidyverse.org), titles, subtitles and, for
plotting, axis labels can all be added on easily, or other elements
(e.g. font size) can be tweaked for a particular output.

### More layouts

[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
can use all the layout algorithms offered by packages such as
[igraph](https://r.igraph.org/),
[ggraph](https://ggraph.data-imaginist.com), and
[graphlayouts](https://github.com/schochastics/graphlayouts).
[autograph](https://stocnet.github.io/autograph/) also offers some
additional layout algorithms for visualising layers horizontally,
vertically, or concentrically, conforming to configurational
coordinates, or for snapping these layouts to a grid.

![Graphs illustrating different
layouts](https://www.jameshollway.com/post/autograph/README-more-layouts-1.png)

### More networks

The second graph drawing function included,
[`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md),
is used to graph multiple networks together. This can be useful for ego
networks or network panels.
[patchwork](https://patchwork.data-imaginist.com) is used to help
arrange individual plots together, and is used throughout the package to
help arrange plots together informatively.

[`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
computes one layout and holds it across every panel. Plotting each
network separately gives each panel its own layout, so a node can appear
in a different position in each panel even where nothing about that node
has changed. Holding the layout constant makes the panels comparable, so
that what moves on the page is what changed in the data.
[`graphs()`](https://stocnet.github.io/autograph/reference/plot_graphs.md)
also collects a single legend for the whole set.

![Example of graphs() used on longitudinal
data](https://www.jameshollway.com/post/autograph/README-autographs-1.png)

### More time

The third graph drawing function,
[`grapht()`](https://stocnet.github.io/autograph/reference/plot_grapht.md),
is used to visualise dynamic networks. It uses
[gganimate](https://gganimate.com) and
[gifski](https://r-rust.r-universe.dev/gifski) to create a gif that
visualises network changes over time, with node positions transitioning
smoothly between waves and nodes fading in and out as they enter and
exit the network. It really couldn’t be easier.

![Example of grapht() on longitudinal
data](https://www.jameshollway.com/post/autograph/README-autographd-1.gif)

## Generating plots

Since network analysis involves not just drawing graphs,
[autograph](https://stocnet.github.io/autograph/) also provides a
function for plotting results from the analysis or modelling of those
networks. To keep things simple, all users need to remember is a single,
generic function:
[`plot()`](https://rdrr.io/r/graphics/plot.default.html). Method
dispatching takes care of the rest, so you can concentrate on exploring
and interpreting your results.

Dispatching works because the results carry a class.
[`igraph::degree()`](https://r.igraph.org/reference/degree.html) and
`sna::degree()` each return a bare numeric vector, so
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) falls back to a
scatterplot of the values against their index, and that index is not
meaningful.
[`netrics::node_by_degree()`](https://stocnet.github.io/netrics/reference/measure_central_degree.html)
returns a `node_measure`, which
[autograph](https://stocnet.github.io/autograph/) plots as a themed
distribution:

![Example illustrating that plotting a bare vector of degree scores
gives an index scatterplot, where plotting a node_measure gives a
distribution](https://www.jameshollway.com/post/autograph/README-result-comparison-1.png)

The same holds for the other result classes. Here are some further
examples, using goodness-of-fit results from fitting a SAOM in
[RSiena](https://www.stats.ox.ac.uk/~snijders/siena/) and an ERGM in
[ergm](https://statnet.org). (Note that neither the data nor the model
are similar; this is just for illustrative purposes.)

![Goodness-of-fit plots for a SAOM fitted in RSiena and an ERGM fitted
in
ergm](https://www.jameshollway.com/post/autograph/README-siena-ergm-gof-1.png)![Goodness-of-fit
plots for a SAOM fitted in RSiena and an ERGM fitted in
ergm](https://www.jameshollway.com/post/autograph/README-siena-ergm-gof-2.png)

### Setting a theme

Note that in the above plots, the same colour scheme and fonts were
used. They can be easily changed though.
[autograph](https://stocnet.github.io/autograph/) includes a number of
themes that can be used to style all graphs and plots consistently. And
it is very easy to set a theme. Just type
[`stocnet_theme()`](https://stocnet.github.io/autograph/reference/theme_set.md)
to see which is the theme currently set, and to get a list of available
themes. Then enter the chosen theme name in the function to set it. All
plots created using [autograph](https://stocnet.github.io/autograph/)
functions will then use this theme, until you change it again.

``` r

stocnet_theme()
(plot(netrics::node_by_degree(ison_karateka)) + 
plot(netrics::tie_by_betweenness(ison_karateka)))/
(plot(netrics::node_in_regular(ison_southern_women, "e")) + 
plot(as_matrix(ison_southern_women),
     membership = netrics::node_in_regular(ison_southern_women, "e")))
stocnet_theme("ethz")
(plot(netrics::node_by_degree(ison_karateka)) + 
plot(netrics::tie_by_betweenness(ison_karateka)))/
(plot(netrics::node_in_regular(ison_southern_women, "e")) + 
plot(as_matrix(ison_southern_women),
     membership = netrics::node_in_regular(ison_southern_women, "e")))
```

![Themed
figures](https://www.jameshollway.com/post/autograph/README-themeset-1.png)![Themed
figures](https://www.jameshollway.com/post/autograph/README-themeset-2.png)

There are a range of institutional and topical themes available,
including default, bw, crisp, neon, clay, iheid, ethz, uzh, rug, unibe,
oxf, unige, cmu, iast, hwu, rainbow, with more on the way.

![Institutional
themes](https://www.jameshollway.com/post/autograph/README-theme-opts-1.png)![Institutional
themes](https://www.jameshollway.com/post/autograph/README-theme-opts-2.png)

### Colours everyone can read

About one man in twelve, and one woman in two hundred, sees colour
differently. A palette that separates its categories for most readers
can collapse for them, and the classic offender is the red-green pair
that so many palettes hold.

[autograph](https://stocnet.github.io/autograph/) does something about
this without asking you to give up a palette. Every theme’s categorical
palette is reordered when the theme is set, so that the colours a graph
reaches for first are those that stay distinct under each type of colour
blindness, and each divergent palette pairs a warm pole with a cool one.

[`simulate_colorblind()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
shows a set of colours as another viewer sees them, so mapping the
simulated colours back onto a graph shows you their view of it. Here is
the same network four times: in
[autograph](https://stocnet.github.io/autograph/)’s default palette as
most readers see it, then as a reader with deuteranopia does, then as a
photocopier renders it, and then in the palette
[ggraph](https://ggraph.data-imaginist.com) falls back on when
[autograph](https://stocnet.github.io/autograph/) is not setting the
colours, as that same reader with deuteranopia sees it.

``` r

set_stocnet_theme("default")
as_seen <- function(colours, type, title){
  graphr(fict_lotr, node_colour = "Race", node_size = 3, labels = FALSE) +
    ggplot2::scale_fill_manual(values = simulate_colorblind(colours, type)) +
    ggtitle(title)
}
as_seen(ag_qualitative(6), "normal", "autograph") |
  as_seen(ag_qualitative(6), "deutan", "autograph, deuteranopia") |
  as_seen(ag_qualitative(6), "grey", "autograph, greyscale") |
  as_seen(scales::hue_pal()(6), "deutan", "ggraph default, deuteranopia")
```

![The same network seen with normal vision, with deuteranopia, and in
greyscale, in autograph's palette, and with deuteranopia in
ggraph's](https://www.jameshollway.com/post/autograph/README-cvd-1.png)

The six races remain tellable apart in the second panel, its closest
pair being Hobbits and Maiar. In the right-hand one, Elves and Ents have
become the same olive. The third panel is the harder case, and it is not
one reordering can fix: a greyscale device keeps only the luminance of a
colour, so two colours of the same lightness merge however different
their hues.
[`check_separation()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
reports that view beside its own score; where a figure has to print in
black and white, use the `"bw"` theme or add a second channel such as
`node_shape`.
[`check_separation()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
puts a number on it, scoring how far apart colours are at their worst
across normal vision and each type of colour blindness:

``` r

round(min(check_separation(ag_qualitative(6)), na.rm = TRUE), 1)             # autograph
#> [1] 13.5
round(min(check_separation(scales::hue_pal()(6)), na.rm = TRUE), 1)          # ggraph
#> [1] 5.5
round(min(check_separation(igraph::categorical_pal(6)), na.rm = TRUE), 1)    # igraph
#> [1] 16.2
```

Below 10 two colours are easily confused, above 25 they are comfortably
distinct. [igraph](https://r.igraph.org/)’s categorical palette is the
Okabe-Ito scheme, which was designed for this and scores accordingly:
where you are free to choose any colours at all, such a scheme is hard
to beat, and
[`graphr()`](https://stocnet.github.io/autograph/reference/plot_graphr.md)
will happily take it. The harder case is the one
[autograph](https://stocnet.github.io/autograph/) is built for — colours
chosen by somebody else, for reasons that were not legibility — and
there the ordering is what stands between a brand palette and an
unreadable graph. A palette with more colours to draw on has more room
to gain: six categories score 29 under the `"hwu"` theme and 26 under
`"oxf"`.

Marks are only half of it. Text has to be read rather than told apart,
which is a matter of contrast rather than of hue, and
[`check_contrast()`](https://stocnet.github.io/autograph/reference/theme_colorblind.md)
scores it against the thresholds of WCAG 2.1: 4.5 for body text, 3 for
large text and for graphical objects. Every theme’s ink clears 4.5 on
that theme’s own ground, and the test suite holds it there.

![Each theme's name written in that theme's ink on that theme's ground,
annotated with its WCAG contrast
ratio](https://www.jameshollway.com/post/autograph/README-wcag-1.png)

The medium is a separate question again.
[`stocnet_medium()`](https://stocnet.github.io/autograph/reference/theme_medium.md)
sizes the text for where the figure will be seen — `"screen"`,
`"presentation"`, `"mobile"` — and `"print"` draws on white whatever
ground the theme prefers, since a tinted ground costs ink and is often
not reproduced. The theme is untouched by it, so one institutional
palette carries from the desk to the slide to the page.

If your institution or organisation is not included and you would like
it to be, please just raise an issue on Github, along with a link to
your corporate branding or style guide if available, and we will attempt
to add it at the next opportunity.

In sum, while there is a lot of clever defaults and customisation
available, all it takes is three simple functions for your

## Installation

### Stable

The easiest way to install the latest stable version of
[autograph](https://stocnet.github.io/autograph/) is via CRAN. Simply
open the R console and enter:

`install.packages('autograph')`

[`library(autograph)`](https://stocnet.github.io/autograph/) will then
load the package and make the data and tutorials (see below) contained
within the package available.

### Development

For the latest development version, for slightly earlier access to new
features or for testing, you may wish to download and install the
binaries from Github or install from source locally. The latest binary
releases for all major OSes – Windows, Mac, and Linux – can be found
[here](https://github.com/stocnet/autograph/releases/latest). Download
the appropriate binary for your operating system, and install using an
adapted version of the following commands:

- For Windows:
  `install.packages("~/Downloads/autograph_winOS.zip", repos = NULL)`
- For Mac:
  `install.packages("~/Downloads/autograph_macOS.tgz", repos = NULL)`
- For Unix:
  `install.packages("~/Downloads/autograph_linuxOS.tar.gz", repos = NULL)`

To install from source the latest main version of
[autograph](https://stocnet.github.io/autograph/) from Github, please
install the [remotes](https://remotes.r-lib.org) package from CRAN and
then:

- For latest stable version:
  `remotes::install_github("stocnet/autograph")`
- For latest development version:
  `remotes::install_github("stocnet/autograph@develop")`

### Other sources

Those using Mac computers may also install using Macports:

`sudo port install R-autograph`

## Funding details

Development on this package has been funded by the Swiss National
Science Foundation (SNSF) [Grant Number
188976](https://data.snf.ch/grants/grant/188976): “Power and Networks
and the Rate of Change in Institutional Complexes” (PANARCHIC).
