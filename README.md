
<!-- README.md is generated from README.Rmd. Please edit that file -->

# autograph

<img src="man/figures/logo.png" align="right" alt="autograph logo" width="150"/>

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
![CRAN/METACRAN](https://img.shields.io/cran/v/autograph) ![GitHub
release (latest by
date)](https://img.shields.io/github/v/release/stocnet/autograph)
![GitHub Release
Date](https://img.shields.io/github/release-date/stocnet/autograph)
[![Codecov test
coverage](https://codecov.io/gh/stocnet/autograph/branch/main/graph/badge.svg)](https://app.codecov.io/gh/stocnet/autograph?branch=main)
<!-- [![CodeFactor](https://www.codefactor.io/repository/github/stocnet/manynet/badge)](https://www.codefactor.io/repository/github/stocnet/manynet) -->
<!-- [![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/4559/badge)](https://bestpractices.coreinfrastructure.org/projects/4559) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7076396.svg)](https://doi.org/10.5281/zenodo.7076396) -->
<!-- see https://zenodo.org/record/7076396 -->
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/stocnet/migraph/total) -->
<!-- badges: end -->

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

`{autograph}` aims to solve these problems by providing automatic graph
drawing for networks in any of the `{manynet}` formats, and automatic
plotting for results from `{stocnet}` packages, including `{migraph}`,
`{RSiena}`, and `{MoNAn}`, and more.

All you need to do is install the package (loading it last will make
sure its plotting methods are the default), use `set_stocnet_theme()`
(once) to set your preferred theme, and then use `graphr()` to graph
your networks, or `plot()` to plot your results. That’s it!

## Drawing graphs

`{autograph}` includes three one-line graphing functions with sensible
defaults based on the network’s properties.

First, `graphr()` is used to graph networks in any of the `{manynet}`
formats. Because it builds upon `{manynet}`, it can graph networks in
any of the `{manynet}` formats, including `network`, `igraph`, `sna`,
`tidygraph`, and more.

Second, it includes sensible defaults so that researchers can view their
network’s structure or distribution quickly with a minimum of fuss.
Compare the output from `{autograph}` with a similar default from
`{igraph}`:

<img src="https://www.jameshollway.com/post/manynet/README-layout-comparison-1.png" alt="Example illustrating differences in default igraph and autograph graphs"/>

`{igraph}` requires the bipartite layout to be specified, has cumbersome
node size defaults for all but the smallest graphs, and labels also very
often need resizing and adjustment to avoid overlap. Getting this
default plot to look good can take a lot of trial and error, and time.
By contrast, `graphr()` recognises the network as two-mode and uses a
bipartite layout by default. It also recognises that the network
contains names for the nodes and prints them vertically so that they are
legible in this layout. Other ‘clever’ features include automatic node
sizing and more.

### More options

All of `graphr()`’s adjustments can be overridden, however… Changing the
size and colors of nodes and ties is as easy as specifying the
function’s relevant argument with a replacement,
e.g. `node_color = "darkblue"` or `node_size = 6`, or indicating from
which attribute it should inherit this information,
e.g. `node_color = "Office"` or `node_size = "Seniority"`.

<img src="https://www.jameshollway.com/post/manynet/README-more-options-1.png" alt="Graph illustrating automatic and manual use of node color and size"/>

Legends are added by default when node or tie aesthetics are mapped to
attributes, but can be removed with `show_legend = FALSE`. Since the
`{autograph}` builds upon `{ggplot2}`, titles, subtitles and, for
plotting, axis labels can all be added on easily, or other elements
(e.g. font size) can be tweaked for a particular output.

### More layouts

`graphr()` can use all the layout algorithms offered by packages such as
`{igraph}`, `{ggraph}`, and `{graphlayouts}`. `{autograph}` also offers
some additional layout algorithms for visualising partitions
horizontally, vertically, or concentrically, conforming to
configurational coordinates, or for snapping these layouts to a grid.

<img src="https://www.jameshollway.com/post/manynet/README-more-layouts-1.png" alt="Graphs illustrating different layouts"/>

### More networks

The second graph drawing function included, `graphs()`, is used to graph
multiple networks together. This can be useful for ego networks or
network panels. `{patchwork}` is used to help arrange individual plots
together, and is used throughout the package to help arrange plots
together informatively.

<img src="https://www.jameshollway.com/post/manynet/README-autographs-1.png" alt="Example of graphs() used on longitudinal data"/>

### More time

The third graph drawing function, `grapht()`, is used to visualise
dynamic networks. It uses `{gganimate}` and `{gifski}` to create a gif
that visualises network changes over time. It really couldn’t be easier.

<img src="https://www.jameshollway.com/post/manynet/README-autographd-1.gif" alt="Example of grapht() on longitudinal data"/>

<!-- provide a common set of tools that can be used to import, export, create, and manipulate network data in a wide variety of formats, -->

<!-- and obtain a good first visualisation quickly. -->

<!-- This can be useful for pedagogical purposes, initial description, or checking something part way through the modelling process. -->

<!-- Through the most comprehensive network class-coercion available, -->

<!-- users can access routines not available in their chosen package or even in `{manynet}`. -->

<!-- `{manynet}` provides a common set of tools and a standard syntax for analysing many different types of networks. -->

<!-- It offers a broad range of functions to make, manipulate, map, measure, and model: -->

<!-- - one-, two-, and sometimes three-mode networks -->

<!-- - undirected, directed, and sometimes complex networks -->

<!-- - unweighted, weighted, and sometimes signed networks -->

## Generating plots

Since network analysis involves not just drawing graphs, `{autograph}`
also provides a function for plotting results from the analysis or
modelling of those networks. To keep things simple, all users need to
remember is a single, generic function: `plot()`. Method dispatching
takes care of the rest, so you can concentrate on exploring and
interpreting your results. Here are some examples, using goodness-of-fit
results from fitting a SAOM in `{RSiena}` and an ERGM in `{ergm}`. (Note
that neither the data nor the model are similar; this is just for
illustrative purposes.)

<img src="man/figures/README-siena-ergm-gof-1.png" width="100%" /><img src="man/figures/README-siena-ergm-gof-2.png" width="100%" />

### Setting a theme

Note that in the above plots, the same colour scheme and fonts were
used. They can be easily changed though. `{autograph}` includes a number
of themes that can be used to style all graphs and plots consistently.
And it is very easy to set a theme. Just type `stocnet_theme()` to see
which is the theme currently set, and to get a list of available themes.
Then enter the chosen theme name in the function to set it. All plots
created using `{autograph}` functions will then use this theme, until
you change it again.

``` r
stocnet_theme()
(plot(node_degree(ison_karateka)) + 
plot(tie_betweenness(ison_karateka)))/
(plot(node_in_regular(ison_southern_women, "e")) + 
plot(as_matrix(ison_southern_women),
     membership = node_in_regular(ison_southern_women, "e")))
```

<img src="man/figures/README-themeset-1.png" alt="Themed figures" width="100%" />

``` r
stocnet_theme("ethz")
(plot(node_degree(ison_karateka)) + 
plot(tie_betweenness(ison_karateka)))/
(plot(node_in_regular(ison_southern_women, "e")) + 
plot(as_matrix(ison_southern_women),
     membership = node_in_regular(ison_southern_women, "e")))
```

<img src="man/figures/README-themeset-2.png" alt="Themed figures" width="100%" />

There are a range of institutional and topical themes available,
including default, bw, crisp, neon, iheid, ethz, uzh, rug, unibe, oxf,
unige, cmu, rainbow, with more on the way. If your institution or
organisation is not included and you would like it to be, please just
raise an issue on Github, along with a link to your corporate branding
or style guide if available, and we will attempt to add it at the next
opportunity.

In sum, while there is a lot of clever defaults and customisation
available, all it takes is three simple functions for your

## Installation

### Stable

The easiest way to install the latest stable version of `{autograph}` is
via CRAN. Simply open the R console and enter:

`install.packages('autograph')`

`library(autograph)` will then load the package and make the data and
tutorials (see below) contained within the package available.

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

To install from source the latest main version of `{autograph}` from
Github, please install the `{remotes}` package from CRAN and then:

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
